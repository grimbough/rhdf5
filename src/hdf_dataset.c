#include "common.h"

hid_t hdf_datatype_resolve(SEXP); /* in hdf_datatype.c */
int   HDF_has_subgroup(SEXP group,SEXP name); /* in hdf_group.c */

SEXP HDF_dataset_print(SEXP dataset)
{
    hid_t d, s, t;
    int i, rank;
    SEXPTYPE Rtype;

    if(!isDATASET(dataset)) {
	Rprintf("not a dataset\n");
	return R_NilValue;
    }
    
    d = HID(dataset);
    if( (s = H5Dget_space(d)) < 0 )
	error("could not obtain the dataspace");
    if( (t = H5Dget_type(d)) < 0 ) {
	H5Sclose(s);
	error("could not obtain the datatype");
    }
    
    if(H5Sis_simple(s)) {
	hsize_t*  dims;

	rank = H5Sget_simple_extent_ndims(s);
	if( rank < 0 )
	    error("could not obtain ndims for the data set");
	if( rank == 0 )
	    Rprintf("an uninitalized dataset");
	else {
	    dims = (hsize_t*) R_alloc(sizeof(hsize_t), rank);
	    H5Sget_simple_extent_dims(s, dims, NULL);
	    Rprintf("%d", (int) dims[0]);
	    for(i=1;i<rank;i++)
		Rprintf("x%d", (int) dims[i]);
	    Rprintf(" matrix");
	}
    } else 
	Rprintf("complex dataset");
    if( (i= HDF_getRtype(d)) > 0 ) {
	Rtype = (SEXPTYPE) i;
	switch( Rtype ) {
	case INTSXP: Rprintf(" of integers\n");break;
	case REALSXP: Rprintf(" of reals\n");break;
	case STRSXP: Rprintf(" of strings\n");break;
	case LGLSXP: Rprintf(" of logicals\n"); break;
	default: Rprintf(" of unknown type, %d \n", Rtype);
	}
    }
    else {
	switch(H5Tget_class(t))	{
	case H5T_INTEGER: Rprintf(" of integers\n");break;
	case H5T_FLOAT: Rprintf(" of numerics\n");break;
	case H5T_TIME: Rprintf(" of times\n");break;
	case H5T_STRING: Rprintf(" of strings\n");break;
	default: Rprintf(" of unsupported\n");break;
	}
    }

  H5Tclose(t);
  H5Sclose(s);

  return R_NilValue;
}

/* select the dataspace and return it */
SEXP HDF_dataset_getspace(SEXP in)
{

    hid_t    s, status;

    if(!isDATASET(in) )
	error("the argument must be an HDF5 dataset");
    
    s = H5Dget_space(HID(in));
    return H5Ssexp(s);
}

/* 
   return an SEXP that contains the dimensions of the
   HDF5 dataset, in
*/
SEXP HDF_dims(SEXP in)
{
    SEXP     ans;
    int      rank, i;
    hsize_t* dims;
    hid_t    s, status;

    if(!isDATASET(in) && !isSPACE(in) )
	return R_NilValue;
    
    if( isDATASET(in) )
	s = H5Dget_space(HID(in));
    else
	s = HID(in);

    if( s < 0 )
	error("unable to retrieve a dataspace");
    
    rank = H5Sget_simple_extent_ndims(s);
    PROTECT(ans = allocVector(INTSXP, rank));
    dims = (hsize_t*)R_alloc(rank, sizeof(hsize_t));
    status = H5Sget_simple_extent_dims(s, dims, NULL);
    H5Sclose(s);
    if( status < 0 )
	error("couldn't obtain the number of points");
   
    for(i=0;i<rank;i++)
	INTEGER(ans)[i] = (int)dims[i];

    UNPROTECT(1);
    return(ans);
}

SEXP HDF_dataset_length(SEXP in)
{
    SEXP ans;
    int i;
    hid_t s;

    if(!isDATASET(in))
	return R_NilValue;
    
    s = H5Dget_space(HID(in));
    if( s < 0 )
	error("unable to retrieve a dataspace");
    
    i = H5Sget_simple_extent_npoints(s);
    ans = allocVector(INTSXP, 1);
    INTEGER(ans)[0] = i;

    return ans;
}
    
SEXP HDF_dataset_create_simple(SEXP group, SEXP name, SEXP dim, SEXP
			       create, SEXP type)
{
    hid_t space, data, dtype;
    int		ndims, i, Rtype;
    hsize_t	*dims;
    
    /*Make sure we're not being stupid*/
    if(!isFILE(group) && !isGROUP(group)) 
      error("not a file or group");

    if(!isPLIST(create))
      error("create: not a property list!");
    
    if(HDF_has_subgroup(group, name))
	error("cannot create data set: it already exists"); 

    /* not sure we should be this general! */
    if(type == R_NilValue)
	dtype = H5T_NATIVE_DOUBLE;
    else
	dtype = hdf_datatype_resolve(type);
    
    /*Convert the dimension array*/
    ndims = length(dim);
    dims = (hsize_t*) R_alloc(ndims, sizeof(hsize_t));

    if( ndims == 0 )
	warning("creating an unitialized dataset");
    
    switch(TYPEOF(dim)) {
    case INTSXP:
      for(i=0;i<ndims;i++) dims[i] = (hsize_t)INTEGER(dim)[i];
      break;
    case REALSXP:
      for(i=0;i<ndims;i++) dims[i] = (size_t)REAL(dim)[i];
      break;
    case VECSXP:
      for(i=0; i<ndims; i++) {
	switch( TYPEOF(VECTOR_ELT(dim, i)) ) {
	case INTSXP:
	  dims[i] = (hsize_t)INTEGER(VECTOR_ELT(dim, i))[0]; 
	  break;
	case REALSXP:
	  dims[i] = (hsize_t)REAL(VECTOR_ELT(dim,i))[0];
	  break;
	default:
	  error("unsupported datatype in conversion");
	}
      }
      break;
    }
    /*Create the temporary simple dataspace*/
    space = H5Screate_simple(ndims, dims, NULL);

    if(space < 0)
	error("unable to create simple dataspace");
    
    data = H5Dcreate(HID(group), STR(name), dtype, space,
		     HID(create));
    H5Sclose(space);

    if(data <= 0) 
	error("unable to create dataset");

    /* set the Rtype to something */
    Rtype = 0;
    if( dtype == H5T_NATIVE_DOUBLE )
	Rtype = REALSXP;
    if( dtype == H5T_NATIVE_INT )
	Rtype = INTSXP;

    HDF_setRtype(data, Rtype);

    return H5Dsexp(data);
}

/* Decide whether or not this vector is contiguous */
int is_contig_i(SEXP vec)
{
    int i,l;

    l = length(vec);
    for(i=1;i<l;i++)
	if(INTEGER(vec)[i-1]+1 != INTEGER(vec)[i]) return 0;
    return 1;
}

int is_contig_r(SEXP vec)
{
    int i,l;

    l = length(vec);
    for(i=1;i<l;i++)
	if(REAL(vec)[i-1]+1 != REAL(vec)[i]) return 0;
    return 1;
}

/* Do all the dirty work */
/* FIXME: I think we can now only get INTSXP subscripts here and the
   other code can disappear, RG */
/* do_subslab is called recursively, count times.
   Each time we grab another part of the object */

#define SELECT (d == 0) ? H5S_SELECT_SET : H5S_SELECT_OR

void do_subslab(hid_t space, hssize_t* off, hsize_t* cnt, int ldim, int
		count, hsize_t* dims, SEXP vec)
{
    int	      i;
    hsize_t   d;
    SEXP      arg;
    herr_t    success;

    ldim++;
    arg = VECTOR_ELT(vec, ldim); 

    d	= H5Sget_select_npoints(space);
    switch(TYPEOF(arg)) {
    case SYMSXP:
	off[ldim] = 0;
	cnt[ldim]= dims[ldim];
	if(ldim == count-1)
	    success = H5Sselect_hyperslab(space,SELECT,off,NULL,cnt,NULL);
	else
	    do_subslab(space, off, cnt, ldim, count, dims, vec);
	break;
    case INTSXP:
	if(length(arg) > 1) {
	    if(is_contig_i(arg)) {
		off[ldim] = (hssize_t)INTEGER(arg)[0]-1;
		cnt[ldim] = (hsize_t)( INTEGER(arg)[length(arg)-1] - (INTEGER(arg)[0]-1) );
		if(ldim == count-1)
		    success = H5Sselect_hyperslab(space, SELECT, off,
						  NULL, cnt, NULL);
		else 		    
		    do_subslab(space, off, cnt, ldim, count, dims, vec);
	    } else {
		for(i=0;i<length(arg);i++) {
		    off[ldim] = (hssize_t)INTEGER(arg)[i]-1;
		    cnt[ldim] = 1;
		    if(ldim == count-1)
			success = H5Sselect_hyperslab(space, SELECT,
						      off, NULL, cnt, NULL);
		    else
			do_subslab(space, off, cnt, ldim, count, dims, vec);
		} /* for */
	    } /* if */
	} else {
	    off[ldim] = (hssize_t)INTEGER(arg)[0]-1;
	    cnt[ldim] = 1;
	    if(ldim == count-1)
		H5Sselect_hyperslab(space,SELECT,off,NULL,cnt,NULL);
	    else
		do_subslab(space,off,cnt,ldim,count,dims, vec);
	} /* if */
	break;
    case REALSXP:
	/* moved this here to overcome a compiler bug in Suse 6.4 */
	off[ldim] = (hssize_t)REAL(arg)[0]-1;
	if(length(arg) > 1) {
	    if(is_contig_r(arg)) {
		cnt[ldim] = (hsize_t)( REAL(arg)[length(arg)-1] - (REAL(arg)[0]-1) );
		if(ldim == count)
		    H5Sselect_hyperslab(space,SELECT,off,NULL,cnt,NULL);
		else
		    do_subslab(space,off,cnt,ldim,count,dims, vec);
	    } else {
		for(i=0;i<length(arg);i++) {
		    off[ldim] = (hssize_t)REAL(arg)[i]-1;
		    cnt[ldim] = 1;
		    if(ldim == count-1)
			H5Sselect_hyperslab(space,SELECT,off,NULL,cnt,NULL);
		    else
			do_subslab(space,off,cnt,ldim,count,dims, vec);
		} /* for */
	    } /* if */
    } else {
	cnt[ldim] = 1;
	if(ldim == count-1)
	    H5Sselect_hyperslab(space,SELECT,off,NULL,cnt,NULL);
	else
	    do_subslab(space,off,cnt,ldim,count,dims, vec);
    } /* if */
	break;
    }
}

/* the args are:
     HDF_dataset_select_space
     x - the dataset
     i - the first dimension
     j - the second dimension

  This sets up the dataspace that corresponds to the
  component of x selected by the subset indices i and j.


*/
SEXP HDF_dataset_select(SEXP x, SEXP args, SEXP drop)
{
    SEXP  ans;
    int   i;

    if( !isDATASET(x) )
	error("argument is not an HDF5 dataset");

    if( TYPEOF(args) != VECSXP )
	error("subset argument is the wrong type");

    for(i=0; i<length(args); i++)
	if( isMissingArg(VECTOR_ELT(args, i)) )
	    SET_VECTOR_ELT(args, i, R_MissingArg);
    
    if( length(args) == 1 ) 
	PROTECT( ans = HDF_VectorSubset(x, args));
    else
	PROTECT( ans = HDF_ArraySubset(x, args));

    if( !isMissingArg(drop) ) {
	i = asLogical(drop);
	if( i && i != NA_LOGICAL )
	    ans = Rf_DropDims(ans);
    }
    UNPROTECT(1);
    return ans;
}

SEXP HDF_ArraySubset(SEXP x, SEXP subargs)
{
    hssize_t  *off; 
    hsize_t   *dims, *cnt, sdim[1];
    SEXP      ans, dimlist, sublist, s, out;
    SEXPTYPE Rtype;
    hid_t     space, t, memspace, htype;
    int       req, rank, i, j, ii, jj, nreq;
    int       **subs, *indx, *bound;

    req = length(subargs);

    t =  H5Dget_type(HID(x));

    space = H5Dget_space(HID(x));

    if( (space < 0 ) || H5Iget_type(space) != H5I_DATASPACE )
	error("problem getting data space");
    rank  = H5Sget_simple_extent_ndims(space);

    if(rank != req) {
	H5Sclose(space);
	error("invalid number of dimensions specified");
    }

    sdim[0]=1;
    memspace = H5Screate_simple(1, sdim, NULL);

    dims = (hsize_t*) R_alloc(rank, sizeof(hsize_t));
    off  = (hssize_t*) R_alloc(rank, sizeof(hssize_t));
    cnt  = (hsize_t*) R_alloc(rank, sizeof(hsize_t));
    
    subs = (int**)R_alloc(rank, sizeof(int*));
    indx = (int*)R_alloc(rank, sizeof(int));
    bound = (int*)R_alloc(rank, sizeof(int));

    H5Sget_simple_extent_dims(space, dims, NULL);
    H5Sselect_none(space);

    PROTECT(dimlist = allocVector(INTSXP, rank));
    for(i=0;i<rank;i++) 
	INTEGER(dimlist)[i] = (int) dims[i];

    /* get R to compute the requested subscripts */
    /* and other initializations */
    nreq = 1;
    PROTECT(sublist = allocVector(VECSXP, req));
    for( i=0; i<req; i++ ) {
	s = VECTOR_ELT(subargs, i);
	if( isMissingArg(s) )
	    s = R_MissingArg;
	SET_VECTOR_ELT(sublist, i, arraySubscript(i, s, dimlist,
						  NULL, R_NilValue));
	bound[i] = length(VECTOR_ELT(sublist, i));
	nreq *= bound[i];
	INTEGER(dimlist)[i] = bound[i];
	indx[i] = 0;
	subs[i] = INTEGER(VECTOR_ELT(sublist, i));
	cnt[i] = 1;
    }

    Rtype = HDFclass2Rtype(H5Tget_class(t));
    htype = Rtype2HDFtype(Rtype);
    
    PROTECT(out = allocVector(Rtype, 1));
    PROTECT(ans = allocVector(Rtype, nreq));

    /* now we do the selection, slowly... */
    /* code is from R's arraysubscript    */
    for (i = 0; i < nreq; i++) {
	ii = 0;
	for (j = 0; j < rank; j++) {
	    jj = subs[j][indx[j]];
	    if (jj == NA_INTEGER) {
		ii = NA_INTEGER;
		goto assignLoop;
	    }
	    if (jj < 1 || jj > (int) dims[j])
		error("bad subscript");
	    off[j] = jj-1;
	}

      assignLoop:
	H5Sselect_hyperslab(space, H5S_SELECT_SET, off, NULL, cnt,
			    NULL);
	H5Dread(HID(x), htype, memspace, space, H5P_DEFAULT,
			REAL(out));
	switch (Rtype) {
	case LGLSXP:
	    if (ii != NA_LOGICAL)
		LOGICAL(ans)[i] = LOGICAL(out)[0]; 
	    else
		LOGICAL(ans)[i] = NA_LOGICAL;
	    break;
	case INTSXP:
	    if (ii != NA_INTEGER)
		INTEGER(ans)[i] = INTEGER(out)[0]; 
	    else
		INTEGER(ans)[i] = NA_INTEGER;
	    break;
	case REALSXP:
	    if (ii != NA_INTEGER)
		REAL(ans)[i] = REAL(out)[0]; 
	    else
		REAL(ans)[i] = NA_REAL;
	    break;
	default:
	    error("unsupported type");

	}
	if (nreq > 1) {
	    j = 0;
	    while (++indx[j] >= bound[j]) {
		indx[j] = 0;
		j = (j + 1) % rank;
	    }
	}
    }
	
    setAttrib(ans, R_DimSymbol, dimlist);
    UNPROTECT(4);
    return(ans);
}

/* select a subset when the subset indices are offsets
   returns a dataspace with the appropriate elements selected */

SEXP HDF_VectorSubset(SEXP x, SEXP s)
{
    SEXP indx, ans, out;
    SEXPTYPE Rtype;
    int nx, stretch, i, j, rank, lindx, val, dimx;
    hid_t ds, status, htype, t, memspace;
    hsize_t *dims, *cnt, *tmp, sdim[1];
    hssize_t *off, *t2;
    void *buf;

    if( s == R_MissingArg )
	return HDF_duplicate(x);

    PROTECT(s);


    ds = H5Dget_space(HID(x));
    if( ds<0 || H5Iget_type(ds) != H5I_DATASPACE )
	error("failed to obtain dataspace");
    
    t =  H5Dget_type(HID(x));

    rank = H5Sget_simple_extent_ndims(ds);
    if( rank < 0 )
	error("failed to get dims of dataspace");

    off = (hssize_t*) R_alloc(rank, sizeof(hssize_t));
    cnt = (hsize_t*) R_alloc(rank, sizeof(hsize_t));
    dims = (hsize_t*) R_alloc(rank, sizeof(hsize_t));
    tmp = (hsize_t*) R_alloc(rank+1, sizeof(hsize_t));

    status = H5Sget_simple_extent_dims(ds, dims, NULL);
    if( status < 0 ) {
	H5Sclose(ds);
	error("unable to obtain dims");
    }
    status = H5Sselect_none(ds);
    if( status < 0 ) {
	H5Sclose(ds);
	error("select none failed");
    }
    nx = 1;
    for(i=0; i<rank; i++)
	nx *= (int) dims[i];

    stretch=1; /* can stretch the subscript vector */
    PROTECT(indx= vectorSubscript(nx, VECTOR_ELT(s, 0),
					    &stretch, NULL, R_NilValue));

    lindx = length(indx);

    /* set up the return value */
    Rtype = HDFclass2Rtype(H5Tget_class(t));

    PROTECT(out = allocVector(Rtype, 1));
    PROTECT(ans = allocVector(Rtype, lindx));

    htype = Rtype2HDFtype(Rtype);

    tmp[0] = 1;
    for(i = 0; i<rank; i++) {
	cnt[i] = 1;
	tmp[i+1] = tmp[i]*dims[i];
    }

    sdim[0]=1;
    memspace = H5Screate_simple(1, sdim, NULL);

    /* here we presume C-style indexing;
       this will need to be fixed for other
       types of indexing */
    for( i=0; i<lindx; i++ ) {
	val = INTEGER(indx)[i]-1;
	for(j = rank-1; j>=0; j--) {
	    off[j] = val/tmp[j];
	    val = val%tmp[j];
	}
	H5Sselect_hyperslab(ds, H5S_SELECT_SET, off, NULL, cnt, NULL);
	H5Dread(HID(x), htype, memspace, ds, H5P_DEFAULT, REAL(out));
	REAL(ans)[i] = REAL(out)[0];
    }

    UNPROTECT(4);
    return(ans);
}

/* 
  copies the R data in mat into the HDF5 format indicated by "in"
*/
SEXP HDF_dataset_store(SEXP in, SEXP mat, SEXP name)
{
    SEXP dimVal;
    hssize_t *datadim, matdim[1];
    hid_t    dataspace, memspace, dset, dtype;
    int ndims, i;

    if( !isFILE(in) && !isGROUP(in) ) {
	error("not an HDF5 File or Group");
	return R_NilValue;
    }

    if(HDF_has_subgroup(in, name))
	error("cannot create data set: it already exists"); 

    if( !isVector(mat) )
	error("second argument must be an array or vector");

    dimVal = getAttrib(mat, R_DimSymbol);
    ndims = length(dimVal);
    if( ndims <= 1 ) {/* we have a vector */
	ndims = 1;
	datadim = (hssize_t *) R_alloc(1, sizeof(hssize_t));
	datadim[0] = length(mat);
    }
    else {
	datadim = (hssize_t *) R_alloc(ndims, sizeof(hssize_t));
	for(i=0; i<ndims; i++)
	    datadim[i] = INTEGER(dimVal)[i];
    }

    memspace = H5Screate_simple(ndims, datadim, NULL);
    if( memspace < 0 ) 
	error("could not allocate a dataspace");

    dtype = Rtype2HDFtype(TYPEOF(mat));
    dset = H5Dcreate(HID(in), STR(name), dtype, memspace,
		     H5P_DEFAULT);

    if( dset < 0 ) {
	H5Sclose(memspace);
	error("could not allocate a dataset");
    }

    H5Dwrite(dset, dtype, memspace, memspace,
	     H5P_DEFAULT, REAL(mat));

    H5Sclose(memspace);
    HDF_setRtype(dset, TYPEOF(mat));
    return H5Dsexp(dset);
}

/* copy the data in hdfdata into the memory allocated in rdata
   all dimension checks have been done, space has been selected
*/
void HDF_hdf2Rdata(hid_t hdfdata, hid_t memspace, SEXP rdata)
{
    hssize_t matdim[1];
    herr_t status;
 
    /* FIXME: when rdata can be a character vector this needs
       to change a bit */
    matdim[0] = (hssize_t) length(rdata);
    if( matdim[0] != H5Sget_simple_extent_npoints(memspace) )
	error("datasets are not the same size");

    switch( TYPEOF(rdata) ) {
    case INTSXP:
    case LGLSXP:
	status = H5Dread(hdfdata, H5T_NATIVE_INT, memspace, H5S_ALL,
		 H5P_DEFAULT, INTEGER(rdata)); 
	if(status < 0 )
	    error("read failed in HDF_hdf2Rdata \n");
	break;
    case REALSXP:
	status = H5Dread(hdfdata, H5T_NATIVE_DOUBLE, memspace, H5S_ALL,
			 H5P_DEFAULT, REAL(rdata)); 
	if(status < 0 )
	    error("read failed (2) in HDF_hdf2Rdata \n");
	break;
    default:
	error("unsupported conversion \n");
    }
}

/* take an hdf5 file on disk and make an R matrix/array */
SEXP HDF_dataset_mat_load(SEXP in)
{
    hssize_t matdim[2], space[1];
    hid_t  dataspace, memspace;
    SEXP  ans;
    
    if(!isDATASET(in)) {
	error("not an HDF5 Dataset");
	return R_NilValue;
    }
    
    dataspace = H5Dget_space(HID(in));
    H5Sget_simple_extent_dims(dataspace, matdim, NULL);
    
    space[0] = matdim[0]*matdim[1];
    memspace = H5Screate_simple(1, space, NULL);
    
    PROTECT(ans = allocVector(REALSXP, (int)(matdim[0]*matdim[1])));
    H5Dread(HID(in), H5T_NATIVE_DOUBLE, memspace, dataspace,
	    H5P_DEFAULT, REAL(ans));
    /* setClass(ans,"mat"); */
    setMatrixDims(ans, 2, matdim);
    UNPROTECT(1);
    
    return ans;
}


/* find out which elements of the dataset are finite */
/* since the return value is as large as the original it had better
   be a HDF object -- where do we put it?
   FIXME: temp name for the dataset seems to be a problem! 
*/
SEXP HDF_dataset_finite(SEXP in)
{
    double *ansd;
    int  n, i, *wbuf, *ansi;
    hid_t s, dnew, d, status;
    
    d = HID(in);
    s = H5Dget_space(d);
    if( s<0 )
	error("unable to obtain the dataspace");

    dnew = HDF_copydataset(d, H5T_NATIVE_INT, s, H5P_DEFAULT);

    n = H5Sget_simple_extent_npoints(s);

    if(n<0) 
	error("unable to obtain number of points");

    wbuf = (int *) R_alloc(n, sizeof(int));

    /* first extract the data */
    switch(HDF_getRtype(HID(in))) {
    case INTSXP:
    case LGLSXP:
	ansi = (int *) R_alloc(n, sizeof(int));
	status = H5Dread(d, H5T_NATIVE_INT, s, H5S_ALL,
		 H5P_DEFAULT, ansi); 
	if(status < 0 )
	    error("read failed in HDF_dataset_finite");
	for (i = 0; i < n; i++)
	    wbuf[i] = (ansi[i] != NA_INTEGER);
	break;
    case REALSXP:
	ansd = (double *) R_alloc(n, sizeof(double));
	status = H5Dread(d, H5T_NATIVE_DOUBLE, s, H5S_ALL,
			 H5P_DEFAULT, ansd); 
	if(status < 0 )
	    error("read failed in HDF_dataset_finite");
	for (i = 0; i < n; i++)
	    wbuf[i] = R_FINITE(ansd[i]);
	break;
    default:
	Rprintf("type of HID(in) is: %d\n", HDF_getRtype(HID(in)));  
	error("unsupported conversion");
    }
    H5Dwrite(dnew, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
	     wbuf);
    HDF_setRtype(dnew, LGLSXP);

    H5Sclose(s);

    return(H5Dsexp(dnew));
}


/* functions to add C-memory to HDF5 datasets */

SEXP HDF_dataset_addmemory(SEXP dataset) 
{
    hid_t s;
    int i;

    if(!isDATASET(dataset))
	error("not a dataset\n");
    
    if( hasMEMORY(dataset) ) {
	warning("the dataset already has memory allocated");
	return R_NilValue;
    }

    s = H5Dget_space(HID(dataset));
    if( s < 0 )
	error("unable to retrieve a dataspace");

    i = H5Sget_simple_extent_npoints(s);
    H5Sclose(s);

    switch(HDF_getRtype(HID(dataset))) {
    case INTSXP:
    case LGLSXP:
	setMEMORY(dataset, allocVector(INTSXP, i) );
	break;
    case REALSXP:
	setMEMORY(dataset, allocVector(REALSXP, i) );
	break;
    default:
	error("can't allocate memory for that storage type");
    }
    return R_NilValue;
}

/* return TRUE if the dataset has memory attached to it */

SEXP HDF_dataset_hasmemory(SEXP dataset)
{
    SEXP ans;

    if(!isDATASET(dataset))
	error("the argument is not a dataset");

    ans = allocVector(LGLSXP, 1);

    if( hasMEMORY(dataset) ) 
	INTEGER(ans)[0] = 1;
    else
	INTEGER(ans)[0] = 0;
    return ans;
}

/* free the R memory */
SEXP HDF_dataset_freememory(SEXP dataset)
{
    if(!isDATASET(dataset)) 
	error("the argument is not a dataset");
    
    setMEMORY(dataset, R_NilValue);
    return R_NilValue;
}

/* swap R memory from in to out */
 
SEXP HDF_dataset_swapmemory(SEXP in, SEXP out)
{
    SEXP mem;
    hid_t s;
    int i;
    
    if(!isDATASET(in))
        error("first argument is not a dataset");
	
    if(!isDATASET(out))
        error("second argument is not a dataset\n");
	
    mem = getMEMORY(in);
    if( mem == R_NilValue )
	error("in doesn't have any memory to swap");

    s = H5Dget_space(HID(in));
    if( s < 0 )
	error("unable to retrieve a dataspace");

    i = H5Sget_simple_extent_npoints(s);
    H5Sclose(s);
    if( !isVector(mem) || i != length(mem) )
        error("memory is not conformable");

    setMEMORY(out, mem);
    setMEMORY(in, R_NilValue);
    return R_NilValue;
}

/* return the vector containing the R memory */
SEXP HDF_dataset_getmemory(SEXP dataset)
{
    if(!isDATASET(dataset)) 
	error("dataset has no memory associated with it");

    return( getMEMORY(dataset) );
}

/* min/max range support */
/* imin/imax/rmin/rmax - borrowed from R 26/8/2001 */


#define R_INT_MIN	(1+INT_MIN)

static void imin(int *x, int n, int *value, int narm, int updated)
{
    int i, s;
    s = INT_MAX;
    for (i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    if (s > x[i]) {
		s = x[i];
		if(!updated) updated = 1;
	    }
	}
	else if (!narm) {
	    if(!updated) updated = 1;
	    *value = NA_INTEGER;
	    return;
	}
    }
    *value = s;
}

static void rmin(double *x, int n, double *value, int narm, int updated)
{
    double s;
    int i;
#ifdef IEEE_754
    s = R_PosInf;
    for (i = 0; i < n; i++) {
	if (ISNAN(x[i])) {/* Na(N) */
	    if (!narm) {
		if(s != NA_REAL) s = x[i];/* was s += x[i];*/
		if(!updated) updated = 1;
	    }
	}
	else if (x[i] < s) {
	    s = x[i];
	    if(!updated) updated = 1;
	}
    }
    *value = /* (!updated) ? NA_REAL : */ s;
#else
    s = NA_REAL;
    for (i = 0; i < n; i++) {
	if (!ISNAN(x[i])) {
	    if (ISNAN(s) || s > x[i]) {
		s = x[i];
		if(!updated) updated = 1;
	    }
	}
	else if (!narm) {
	    if(!updated) updated = 1;
	    *value = NA_REAL;
	    return;
	}
    }
    *value = s;
#endif
}

static void imax(int *x, int n, int *value, int narm, int updated)
{
    int i, s;
    s = R_INT_MIN;
    for (i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    if (s < x[i]) {
		s = x[i];
		if(!updated) updated = 1;
	    }
	} else if (!narm) {
	    if(!updated) updated = 1;
	    *value = NA_INTEGER;
	    return;
	}
    }
    *value = s;
}

static void rmax(double *x, int n, double *value, int narm, int updated)
{
    double s;
    int i;
#ifdef IEEE_754
    s = R_NegInf;
    for (i = 0; i < n; i++) {
	if (ISNAN(x[i])) {/* Na(N) */
	    if (!narm) {
		if(s != NA_REAL) s = x[i];/* was s += x[i];*/
		if(!updated) updated = 1;
	    }
	}
	else if (x[i] > s) {
	    s = x[i];
	    if(!updated) updated = 1;
	}
    }
    *value = /* (!updated) ? NA_REAL : */ s;
#else
    s = NA_REAL;
    for (i = 0; i < n; i++) {
	if (!ISNAN(x[i])) {
	    if (ISNAN(s) || s < x[i])
		s = x[i];
	    if(!updated) updated = 1;
	}
	else if (!narm) {
	    if(!updated) updated = 1;
	    *value = NA_REAL;
	    return;
	}
    }
    *value = s;
#endif
}


SEXP HDF_dataset_min(SEXP args)
{
    SEXP dataset, ans;
    int n, narm, *wbuf, minint;
    double *rbuf, mindoub;
    hid_t d, s, status;

    args=CDR(args); /* strip the call */
    dataset = CAR(args);
    args=CDR(args);
    if( TYPEOF(CAR(args)) != LGLSXP )
	error("wrong type of argument for na.rm");
    narm = LOGICAL(CAR(args))[0];

    if(!isDATASET(dataset))
	error("not a hdf5 dataset\n");
    
    d = HID(dataset);
    s = H5Dget_space(d);
    if( s<0 ) 
	error("unable to obtain the dataspace");


    n = H5Sget_simple_extent_npoints(s);
    if(n<0)
	error("unable to obtain the number of points");

    switch(HDF_getRtype(HID(dataset))) {
    case INTSXP:
    case LGLSXP:
	wbuf = (int *) R_alloc(n, sizeof(int));
	status = H5Dread(d, H5T_NATIVE_INT, s, H5S_ALL,
		 H5P_DEFAULT, wbuf);
	if(status < 0 )
	    error("read failed in HDF_dataset_min");
	imin(wbuf, n, &minint, narm, 0);
	ans = allocVector(INTSXP, 1);
	INTEGER(ans)[0] = minint;
	break;
    case REALSXP:
	rbuf = (double *) R_alloc(n, sizeof(double));
	status = H5Dread(d, H5T_NATIVE_DOUBLE, s, H5S_ALL,
			 H5P_DEFAULT, rbuf); 
	if(status < 0 )
	    error("read failed in HDF_dataset_min");
	rmin(rbuf, n, &mindoub, narm, 0);
	ans = allocVector(REALSXP, 1);
	REAL(ans)[0] = mindoub;
	break;
    default:
	error("wrong type for min");
    }
    H5Sclose(s);
    return ans;
}

SEXP HDF_dataset_max(SEXP args)
{
    SEXP dataset, ans;
    int n, narm, *wbuf, maxint;
    double *rbuf, maxdoub;
    hid_t d, s, status;

    args=CDR(args); /* strip the call */
    dataset = CAR(args);
    args=CDR(args);
    if( TYPEOF(CAR(args)) != LGLSXP )
	error("wrong type of argument for na.rm");
    narm = LOGICAL(CAR(args))[0];

    if(!isDATASET(dataset))
	error("not a hdf5 dataset\n");
    
    d = HID(dataset);
    s = H5Dget_space(d);
    if( s<0 ) 
	error("unable to obtain the dataspace");


    n = H5Sget_simple_extent_npoints(s);
    if(n<0)
	error("unable to obtain the number of points");

    switch(HDF_getRtype(HID(dataset))) {
    case INTSXP:
    case LGLSXP:
	wbuf = (int *) R_alloc(n, sizeof(int));
	status = H5Dread(d, H5T_NATIVE_INT, s, H5S_ALL,
		 H5P_DEFAULT, wbuf);
	if(status < 0 )
	    error("read failed in HDF_dataset_max");
	imax(wbuf, n, &maxint, narm, 0);
	ans = allocVector(INTSXP, 1);
	INTEGER(ans)[0] = maxint;
	break;
    case REALSXP:
	rbuf = (double *) R_alloc(n, sizeof(double));
	status = H5Dread(d, H5T_NATIVE_DOUBLE, s, H5S_ALL,
			 H5P_DEFAULT, rbuf); 
	if(status < 0 )
	    error("read failed in HDF_dataset_max");
	rmax(rbuf, n, &maxdoub, narm, 0);
	ans = allocVector(REALSXP, 1);
	REAL(ans)[0] = maxdoub;
	break;
    default:
	error("wrong type for max");
    }
    H5Sclose(s);
    return ans;
}


SEXP HDF_dataset_range(SEXP args)
{
    SEXP dataset, ans;
    int n, narm, *wbuf, minint, maxint;
    double *rbuf, mindoub, maxdoub;
    hid_t d, s, status;

    args=CDR(args); /* strip the call */
    dataset = CAR(args);
    args=CDR(args);
    if( TYPEOF(CAR(args)) != LGLSXP )
	error("wrong type of argument for na.rm");
    narm = LOGICAL(CAR(args))[0];

    if(!isDATASET(dataset))
	error("not a hdf5 dataset\n");
    
    d = HID(dataset);
    s = H5Dget_space(d);
    if( s<0 ) 
	error("unable to obtain the dataspace");


    n = H5Sget_simple_extent_npoints(s);
    if(n<0)
	error("unable to obtain the number of points");

    switch(HDF_getRtype(HID(dataset))) {
    case INTSXP:
    case LGLSXP:
	wbuf = (int *) R_alloc(n, sizeof(int));
	status = H5Dread(d, H5T_NATIVE_INT, s, H5S_ALL,
		 H5P_DEFAULT, wbuf);
	if(status < 0 )
	    error("read failed in HDF_dataset_min");
	imin(wbuf, n, &minint, narm, 0);
	imax(wbuf, n, &maxint, narm, 0);
	ans = allocVector(INTSXP, 2);
	INTEGER(ans)[0] = minint;
	INTEGER(ans)[1] = maxint;
	break;
    case REALSXP:
	rbuf = (double *) R_alloc(n, sizeof(double));
	status = H5Dread(d, H5T_NATIVE_DOUBLE, s, H5S_ALL,
			 H5P_DEFAULT, rbuf); 
	if(status < 0 )
	    error("read failed in HDF_dataset_min");
	rmin(rbuf, n, &mindoub, narm, 0);
	rmax(rbuf, n, &maxdoub, narm, 0);
	ans = allocVector(REALSXP, 2);
	REAL(ans)[0] = mindoub;
	REAL(ans)[1] = maxdoub;
	break;
    default:
	error("wrong type for min");
    }
    H5Sclose(s);
    return ans;
}
