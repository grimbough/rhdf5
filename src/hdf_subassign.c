/* copyright R. Gentleman, 2001 */
/* an implementation of subset assignment for hdf5-R objects */

#include "common.h"

static SEXP HDF_VectorSubassign(SEXP, SEXP, SEXP);
static SEXP HDF_ArraySubassign(SEXP, SEXP, SEXP);


/* FIXME:
     - a big assumption here is that the lhs is an R_object,
       we need to handle hdf5 objects as well at some time
*/

/* HDF_typefix:
    coerce y to the same type as the x argument.
    Signal an error if this is not possible

    any brave adventerous soul can implement x coercion, it should
    just involve hdf_duplicate
*/
static SEXP HDF_typefix(SEXP x, SEXP y)
{
    SEXP rval;
    SEXPTYPE Rtype;
    int which;
    hid_t ds, t;

    ds = H5Dget_space(HID(x));
    if( ds<0 || H5Iget_type(ds) != H5I_DATASPACE )
	error("failed to obtain dataspace");
    
    t =  H5Dget_type(HID(x));
    Rtype = HDFclass2Rtype(H5Tget_class(t));

    which = 100 * Rtype + TYPEOF(y);

    switch (which) {
    case 1010:	/* logical    <- logical    */
    case 1310:	/* integer    <- logical    */
    case 1410:	/* real	      <- logical    */
    case 1510:	/* complex    <- logical    */
    case 1313:	/* integer    <- integer    */
    case 1413:	/* real	      <- integer    */
    case 1414:	/* real	      <- real	    */
	rval = coerceVector(y, Rtype);
	break;

    case 1013:	/* logical    <- integer    */
        /* this shouldn't be a problem though, x should be an integer hdf*/
	error("hdf: unable to perform type coercion");
	/* *x = coerceVector(*x, INTSXP); */
	break;

    case 1014:	/* logical    <- real	    */
    case 1314:	/* integer    <- real	    */
        error("hdf: unable to perform type coercion");
	break;

    case 1015:	/* logical    <- complex    */
    case 1315:	/* integer    <- complex    */
    case 1415:	/* real	      <- complex    */
	error("unable to perform type coercion");
	/* *x = coerceVector(*x, CPLXSXP); */
	break;

    case 1610:	/* character  <- logical    */
    case 1613:	/* character  <- integer    */
    case 1614:	/* character  <- real	    */
    case 1615:	/* character  <- complex    */

	rval = coerceVector(y, STRSXP);
	break;

    case 1016:	/* logical    <- character  */
    case 1316:	/* integer    <- character  */
    case 1416:	/* real	      <- character  */
    case 1516:	/* complex    <- character  */
        error("hdf: unable to perform type coercion");
	/* *x = coerceVector(*x, STRSXP); */
	break;

    default:
	error("hdf: incompatible types");

    }
    return(rval);
}

/* replace the portion of x described by subargs by
   the elements in value; either x or value may be type-coerced 

   we will accept either a vector (1-d) subscript or a subscript that
   is the same dimension as x, everything else is an error */


SEXP HDF_subassign(SEXP x, SEXP subargs, SEXP value)
{
    SEXP ans, newValue;
    int i;

    if( !isDATASET(x) )
	error("argument is not an HDF5 dataset");

    if( TYPEOF(subargs) != VECSXP )
	error("subset argument is the wrong type");

    PROTECT( newValue = HDF_typefix(x, value) );

    /* replace "Missing_Arg" with R_MissingArg */
    if( TYPEOF(subargs) == VECSXP ) {
	for(i=0; i<length(subargs); i++)
	    if( isMissingArg(VECTOR_ELT(subargs, i)) )
		SET_VECTOR_ELT(subargs, i, R_MissingArg);
    }
    else
	error("unknown type of args, cannot proceed");

    if( length(subargs) == 1 ) 
	PROTECT( ans = HDF_VectorSubassign(x, subargs, newValue));
    else
	PROTECT( ans = HDF_ArraySubassign(x, subargs, newValue));

    UNPROTECT(2);
    return(ans);
}

/* we have one subscript and we will treat x as if it is a vector,
   regardless of its dims */

static SEXP HDF_VectorSubassign(SEXP x, SEXP s, SEXP y)
{
    SEXP dim, indx, out;
    SEXPTYPE Rtype;
    int nx, stretch, i, j, rank, val, dimx, n;
    int ny, iy, which;
    double z;
    hid_t ds, status, htype, t, memspace;
    hsize_t *dims, *cnt, *tmp, sdim[1];
    hssize_t *off, *t2;
    void *buf;

    ds = H5Dget_space(HID(x));
    if( ds<0 || H5Iget_type(ds) != H5I_DATASPACE )
	error("failed to obtain dataspace");
    
    t =  H5Dget_type(HID(x));
    Rtype = HDFclass2Rtype(H5Tget_class(t));

    rank = H5Sget_simple_extent_ndims(ds);
    if( rank < 0 )
	error("failed to get dims of dataspace");

    ny = LENGTH(y);

    off = (hssize_t*) R_alloc(rank, sizeof(hssize_t));
    cnt = (hsize_t*) R_alloc(rank, sizeof(hsize_t));
    dims = (hsize_t*) R_alloc(rank, sizeof(hsize_t));
    tmp = (hsize_t*) R_alloc(rank+1, sizeof(hsize_t));

    status = H5Sget_simple_extent_dims(ds, dims, NULL);

    if( status < 0 ) {
	H5Sclose(ds);
	error("unable to obtain dims");
    }

    /* make sure nothing is selected */
    /* this might be a problem if we use selections for something */
    status = H5Sselect_none(ds);
    if( status < 0 ) {
	H5Sclose(ds);
	error("select none failed");
    }

    nx = 1;
    for(i=0; i<rank; i++)
	nx *= (int) dims[i];

    PROTECT(indx= vectorSubscript(nx, VECTOR_ELT(s, 0),
					    &stretch, NULL, R_NilValue));

    n = length(indx);

    PROTECT(out = allocVector(Rtype, 1));
    htype = Rtype2HDFtype(Rtype);

    tmp[0] = 1;
    for(i = 0; i<rank; i++) {
	cnt[i] = 1;
	tmp[i+1] = tmp[i]*dims[i];
    }

    sdim[0]=1;
    memspace = H5Screate_simple(1, sdim, NULL);

    /* Rtype is the type of the hdf5 array we are writing in to */
    switch(Rtype) {
    case INTSXP:
    case LGLSXP:
	for( i=0; i<n; i++ ) {
	    val = INTEGER(indx)[i];
	    if (val == NA_INTEGER) continue;
	    val = val-1;
	    INTEGER(out)[0] = INTEGER(y)[i %ny];
	    for(j = rank-1; j>=0; j--) {
		off[j] = val/tmp[j];
		val = val%tmp[j];
	    }
	    H5Sselect_hyperslab(ds, H5S_SELECT_SET, off, NULL, cnt, NULL);
	    H5Dwrite(HID(x), htype, memspace, ds, H5P_DEFAULT, INTEGER(out));
	}
	break;

    case REALSXP:
	for (i = 0; i < n; i++) {
	    val = INTEGER(indx)[i];
	    if (val == NA_INTEGER) continue;
	    val = val - 1;
	    z = REAL(y)[i % ny];
	    if (z == NA_REAL)
		REAL(out)[0] = NA_REAL;
	    else
		REAL(out)[0] = z;
	    for(j = rank-1; j>=0; j--) {
		off[j] = val/tmp[j];
		val = val%tmp[j];
	    }
	    H5Sselect_hyperslab(ds, H5S_SELECT_SET, off, NULL, cnt, NULL);
	    H5Dwrite(HID(x), htype, memspace, ds, H5P_DEFAULT, REAL(out));
	}	    
	break;

    default:
	error("hdf:sub assignment not done; bug");
    }
    UNPROTECT(2);
}

/* here I think we might just want to translate to vector subscripts
   and then use the code above */

static SEXP HDF_ArraySubassign(SEXP x, SEXP s, SEXP y)
{
    SEXP dim, out;
    SEXPTYPE Rtype;
    int nx, stretch, i, j, rank, val, dimx, n;
    int ii, ij, jj, ny, iy, which;
    double z;
    hid_t ds, status, htype, t, memspace;
    hsize_t *dims, *cnt, *tmp;
    hssize_t *off, *t2;
    void *buf;
    int **subs, *bound, *indx, *offset;
    double ry;

 
    ds = H5Dget_space(HID(x));
    if( ds<0 || H5Iget_type(ds) != H5I_DATASPACE )
	error("failed to obtain dataspace");
    
    t =  H5Dget_type(HID(x));
    Rtype = HDFclass2Rtype(H5Tget_class(t));

    rank = H5Sget_simple_extent_ndims(ds);
    if( rank < 0 )
	error("failed to get dims of dataspace");

    ny = LENGTH(y);

    off = (hssize_t*) R_alloc(rank, sizeof(hssize_t));
    cnt = (hsize_t*) R_alloc(rank, sizeof(hsize_t));
    dims = (hsize_t*) R_alloc(rank, sizeof(hsize_t));
    tmp = (hsize_t*) R_alloc(rank+1, sizeof(hsize_t));

    status = H5Sget_simple_extent_dims(ds, dims, NULL);

    if( status < 0 ) {
	H5Sclose(ds);
	error("unable to obtain dims");
    }

    /* make sure nothing is selected */
    /* this might be a problem if we use selections for something */
    status = H5Sselect_none(ds);
    if( status < 0 ) {
	H5Sclose(ds);
	error("select none failed");
    }


    subs = (int**)R_alloc(rank, sizeof(int*));
    indx = (int*)R_alloc(rank, sizeof(int));
    bound = (int*)R_alloc(rank, sizeof(int));
    offset = (int*)R_alloc(rank, sizeof(int));

    if (rank != length(s))
	error("incorrect number of subscripts");

    PROTECT(dim = allocVector(INTSXP, rank));
    offset[0] = 1;
    for (i = 1; i < rank; i++) {
	offset[i] = offset[i - 1] * dims[i - 1];
	INTEGER(dim)[i-1] = dims[i-1];
    }
    INTEGER(dim)[rank-1] = dims[rank-1];

    /* Expand the list of subscripts. */
    /* s is protected, so no GC problems here */

    for (i = 0; i < rank; i++) 
	SET_VECTOR_ELT(s, i, arraySubscript(i, VECTOR_ELT(s, i), dim,
					    NULL, R_NilValue)); 
    n = 1;
    tmp[0] = 1;
    for (i = 0; i < rank; i++) {
	indx[i] = 0;
	subs[i] = INTEGER(VECTOR_ELT(s, i));
	bound[i] = LENGTH(VECTOR_ELT(s, i));
	n *= bound[i];
	cnt[i] = 1;
	tmp[i+1] = tmp[i]*dims[i];
    }

    if (n > 0 && ny == 0)
	error("nothing to replace with");
    if (n > 0 && n % ny)
	error("number of items to replace is not a multiple of replacement length");

    if (ny == 0) {
	return(x);
    }

    memspace = H5Screate_simple(1, cnt, NULL);
    PROTECT(out = allocVector(Rtype, 1));
    htype = Rtype2HDFtype(Rtype);

    /* Note that we are now committed.  Since we are mutating */
    /* existing objects any changes we make now are permanent. */

    for (i = 0; i < n; i++) {
	for (j = 0; j < rank; j++) {
	    off[j] = subs[j][indx[j]];
	    if (off[j] == NA_INTEGER) 
		goto next_i;
	    else
		off[j] = off[j] - 1;
	}
	switch(Rtype) {
	case INTSXP:
	case LGLSXP:
	    INTEGER(out)[0] = INTEGER(y)[i %ny];

	    H5Sselect_hyperslab(ds, H5S_SELECT_SET, off, NULL, cnt, NULL);
	    H5Dwrite(HID(x), htype, memspace, ds, H5P_DEFAULT, INTEGER(out));
	    break;
	    
	case REALSXP:
	    z = REAL(y)[i % ny];
	    if (z == NA_REAL)
		REAL(out)[0] = NA_REAL;
	    else
		REAL(out)[0] = z;

	    H5Sselect_hyperslab(ds, H5S_SELECT_SET, off, NULL, cnt, NULL);
	    H5Dwrite(HID(x), htype, memspace, ds, H5P_DEFAULT, REAL(out));
	    break;

	default:
	    error("hdf:sub assignment not done; bug");
	}
	if (n > 1) {
	    j = 0;
	    while (++indx[j] >= bound[j]) {
		indx[j] = 0;
		j = (j + 1) % rank;
	    }
	}
    next_i:
	;
    }
    UNPROTECT(2);
}





