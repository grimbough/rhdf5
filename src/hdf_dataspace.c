#include "common.h"

SEXP Rf_DropDims(SEXP);

SEXP HDF_space_print(SEXP space)
{
    hssize_t	dims;

    if(!isSPACE(space)) {
	Rprintf("not a dataspace");
	return R_NilValue;
    }

    Rprintf("HDF5 dataspace: ");
    dims = H5Sget_select_npoints(HID(space));
    Rprintf("%.0f points\n",(double)dims);
    return R_NilValue;
}

SEXP HDF_space_store(SEXP in, SEXP space, SEXP vec)
{
    hid_t	memspace;
    hsize_t	dims[1], cnt[1];
    hssize_t	off[1];
    herr_t      val;
    SEXP v, w;
    int lv, i;
    
    dims[0] = H5Sget_select_npoints(HID(space));
    off[0]  = 0;
    cnt[0]  = dims[0];

    if( !isVector(vec) )
	error("wrong argument for replacement");
    lv = length(vec);
    
    if( dims[0] > 0 && dims[0] % lv )
	warning("number of items to replace is not a multiple of replacement length");

    PROTECT(v=coerceVector(vec, REALSXP));
    if( lv != dims[0] ) { /* need to expand the vector */
	PROTECT(w = allocVector(REALSXP,(int)dims[0]));
	for(i=0; i<dims[0]; i++)
	    REAL(w)[i] = REAL(v)[i % lv];
    }
    else
	PROTECT(w = v);


    memspace = H5Screate_simple(1, dims, NULL);
    if( memspace < 0 )
	error("couldn't allocate dataspace");

    val = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, off, NULL, cnt, NULL);
    if( val < 0 ) {
	H5Sclose(memspace);
	error("select failed");
    }

    val = H5Dwrite(HID(in), H5T_NATIVE_DOUBLE, memspace, HID(space),
	     H5P_DEFAULT, REAL(w)); 
    H5Sclose(memspace);
    if( val < 0 )
	error("write failed");

    UNPROTECT(2);
    return(R_NilValue);
}

/*
  a subset of the HDF5 object is extracted.
   Until we have automatic translators we proceed as follows:
   1) determine the type of the HDF5 array and the corresponding R 
      storage type
   2) allocate appropriate R memory of that size and do the extraction
   3) determine whether the HDF5 object has an R type;
      if so, and if it's different from what we extracted
      call coerceVector
*/
SEXP HDF_space_load(SEXP in, SEXP space, SEXP drop)
{
    SEXP  ans, dimlist, spaced;
    SEXPTYPE Rtype;
    int i;
    hid_t memspace, d, t, htype, select;
    hssize_t dims[1], off[1], cnt[1];

    dims[0] = H5Sget_select_npoints(HID(space));
    off[0]  = 0;
    cnt[0]  = dims[0];
    
    if( !isDATASET(in)) {
	Rprintf("not a dataset \n");
	return R_NilValue;
    }
 
    d = HID(in);
    t = H5Dget_type(d);

    Rtype = HDFclass2Rtype(H5Tget_class(t));

    PROTECT(ans = allocVector(Rtype, (int) dims[0]));

    htype = Rtype2HDFtype(Rtype);

    memspace = H5Screate_simple(1, dims, NULL);
    H5Sselect_hyperslab(memspace, H5S_SELECT_SET, off, NULL, cnt,
                        NULL);

    /* note REAL( ) is ok here, we just getting the pointer */
    select = H5Dread(HID(in), htype, memspace, HID(space),
		     H5P_DEFAULT, REAL(ans));

    H5Sclose(memspace);

    if( select < 0 ) {
	H5Tclose(t);
	error("failed to obtain the subset");
    }


    /* check to see if the data set has an Rtype */
    if(  (i = HDF_getRtype(d)) > 0 )
	if( Rtype != (SEXPTYPE) i ) {
	    ans = coerceVector(ans, (SEXPTYPE) i);
	    UNPROTECT(1);
	    PROTECT(ans);
	}

    H5Tclose(t);
    /* Get the space dimensions */
    spaced = getAttrib(space, HDF_SpaceDimsSymbol);
    if(spaced != R_NilValue) {
	PROTECT(dimlist = duplicate(spaced));
	setAttrib(ans, R_DimSymbol, dimlist);
	UNPROTECT(1);
    }

    if( !isMissingArg(drop) ) {
	i = asLogical(drop);
	if( i && i != NA_LOGICAL )
	    ans = Rf_DropDims(ans);
    }
    UNPROTECT(1);
    return(ans);
}




/* This function is called for its side effect on the supplied
   dataspace */

SEXP HDF_select_hyperslab(SEXP space, SEXP selop, SEXP Rstart, SEXP
			  Rstride, SEXP Rcount, SEXP Rblock)
{
    hid_t s;
    H5S_seloper_t op=H5S_SELECT_SET;
    hssize_t *start;
    hsize_t *stride, *count, *block, *dims;
    int ndims, i;

    if( !isSPACE(space) )
	error("select_hyperslab requires a dataspace as its first argument");

    if( !isString(selop) )
	error("wrong type of selection operator, must be a string");

    if( !strcmp(STR(selop), "SET") )
	op = H5S_SELECT_SET;
    else if (!strcmp(STR(selop), "OR") )
	op = H5S_SELECT_OR;
    else
	error("unknown select operation: %s", STR(selop));

    ndims = H5Sget_simple_extent_ndims(HID(space));
    dims = (hsize_t*) R_alloc(ndims, sizeof(hsize_t));
    count = (hsize_t*) R_alloc(ndims, sizeof(hsize_t));
    block = (hsize_t*) R_alloc(ndims, sizeof(hsize_t));
    stride = (hsize_t*) R_alloc(ndims, sizeof(hsize_t));
    start = (hssize_t*) R_alloc(ndims, sizeof(hssize_t));
    
    if( !isVector(Rstart) || length(Rstart) != ndims )
	error("start must be a vector");
    PROTECT(Rstart = coerceVector(Rstart, INTSXP));

    if( !isVector(Rcount) || length(Rcount) != ndims )
	error("start must be a vector");
    PROTECT(Rcount = coerceVector(Rcount, INTSXP));   

    if( !isVector(Rblock) || length(Rcount) != ndims )
	error("start must be a vector");
    PROTECT(Rblock = coerceVector(Rblock, INTSXP));

    if( !isVector(Rstride) || length(Rstride) != ndims )
	error("start must be a vector");
    PROTECT(Rstride = coerceVector(Rstride, INTSXP));
   
    for(i=0; i<ndims; i++) {
	count[i] = (hsize_t) INTEGER(Rcount)[i];
	stride[i] = (hsize_t) INTEGER(Rstride)[i];
	block[i] = (hsize_t) INTEGER(Rblock)[i];
	start[i] = (hssize_t) INTEGER(Rstart)[i];
    }

    s = H5Sselect_hyperslab(HID(space), op, start, stride, count, block);
    if( s < 0 )
	error("select_hyperslab failed");

    UNPROTECT(4);

    return R_NilValue;
}
	










