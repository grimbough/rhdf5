#include "common.h"

/* 
  given an hdf5 data set, see if it has dims set, if so,
  copy these to a dim attribute and attach that to rdata,
  if check is positive, check to see that rdata is the right
  size
  return -1 if check fails, otherwise return 1
*/
int HDF_hdfdims2Rdims(hid_t hdfdata, SEXP rdata, int check)
{
    hid_t d, s;
    int i, rank, count=1;
    SEXP Rdims;
    hsize_t *dims;

    d = hdfdata;
    s = H5Dget_space(d);
    
    rank = H5Sget_simple_extent_ndims(s);
    dims = (hsize_t*) R_alloc(sizeof(hsize_t), rank);
    H5Sget_simple_extent_dims(s, dims, NULL);

    PROTECT(Rdims = allocVector(INTSXP, rank));

    for(i = 0; i<rank; i++) {
	INTEGER(Rdims)[i] = dims[i];
	count *= dims[i];
    }
    H5Sclose(s); H5Dclose(d);

    if(check && ( count != length(rdata) ))
	    return -1;
	    
    setAttrib(rdata, R_DimSymbol, Rdims);
    UNPROTECT(1);
    return 1;
}

/* copy the dimensions from rdat to the hdf5 data */
/* FIXME: not finished yet */
/* not clear what to do: hdf5 datasets have a dim set 
   when they are created and it doesn't seem easy to 
   reset them;
*/
int HDF_rdims2hdf5dims(hid_t hdfdata, SEXP rdata, int check)
{
    hid_t d, s;
    int i, rank, count=1;
    SEXP Rdims;
    hsize_t *dims;

    d = hdfdata;
    s = H5Dget_space(d);
    
    rank = H5Sget_simple_extent_ndims(s);
    dims = (hsize_t*) R_alloc(sizeof(hsize_t), rank);
    H5Sget_simple_extent_dims(s, dims, NULL);
    return 1;
}

#ifdef OLD
/* note that old class attributes are lost */
void setClass(SEXP obj, const char* name)
{
    SEXP class;
    PROTECT(class = mkString(name));
    classgets(obj, class);
    UNPROTECT(1);
}
#endif

void addClass(SEXP obj, const char* name)
{
    SEXP class, oclass;
    int i, leno;
    
    oclass = getAttrib(obj, R_ClassSymbol);
    leno = length(oclass);

    if( oclass == R_NilValue )
        PROTECT(class = mkString(name));
    else {
	PROTECT(class = allocVector(STRSXP, leno+1));
	for (i = 0; i < leno; i++)
	    SET_VECTOR_ELT(class, i, VECTOR_ELT(oclass, i));
	SET_VECTOR_ELT(class, leno, mkChar(name));
    }
    classgets(obj, class);
    UNPROTECT(1);
}

/* we seem to have a lot of these */
void setMatrixDims(SEXP obj, int ndims, hsize_t* dims)
{
    SEXP d;
    int  i;

    PROTECT(d = allocVector(INTSXP,ndims));
    for(i=0;i<ndims;i++)
	INTEGER(d)[i] = (int)dims[i];
    setAttrib(obj,R_DimSymbol,d);
    UNPROTECT(1);
}


/* copy hdf5 data to a C pointer that can be passed to R */
/* we return a pointer to whatever the type of obj is */
/* FIXME: need some design, HDF_hdf2Rdata does about the same thing */
 
void *HDF_convertoR(SEXP obj, R_CConvertInfo *inf, R_toCConverter *el)
{
    hid_t d, s;
    int i, n;
    herr_t status;
    SEXP ans;

    if( !isDATASET(obj) )
	error("only datasets can be converted now");

    d = HID(obj);
    s = H5Dget_space(d);


    if( hasMEMORY(obj) )
	ans = getMEMORY(obj);
    else {
        Rprintf("no memory\n");
	ans = R_NilValue;
	n = H5Sget_simple_extent_npoints(s);
    }

    switch(HDF_getRtype(d)) {
    case INTSXP:
    case LGLSXP:
	if( ans == R_NilValue ) {
	    ans = allocVector(INTSXP, n);
	    setMEMORY(obj, ans);
	}
	status = H5Dread(d, H5T_NATIVE_INT, s, H5S_ALL,
		 H5P_DEFAULT, INTEGER(ans)); 
	if(status < 0 )
	    error("read failed in HDF_convertoR");
    case REALSXP:
	if( ans == R_NilValue) {
	    ans = allocVector(REALSXP, n);
	    setMEMORY(obj, ans);
	}
	status = H5Dread(d, H5T_NATIVE_DOUBLE, s, H5S_ALL,
			 H5P_DEFAULT, REAL(ans)); 
	if(status < 0 )
	    error("read failed (2) in HDF_hdfconvertoR");
	break;
    default:
	error("unsupported converstion \n");
    }
    return (void*) REAL(ans);
}

/* duplicate an HDF object */
/* For now only datasets and dataspaces:
   - for a dataset we make a duplicate copy in 
     the working directory
*/

SEXP HDF_duplicate(SEXP x) 
{
    SEXP dup;
    hid_t s;

    if( !isSPACE(x) && !isDATASET(x) )
	error("don't know how to duplicate object");

    if( isSPACE(x) ) {
	s = H5Scopy(HID(x));
	return H5Ssexp(s);
    }

    if( isDATASET(x) ) {
	s = HDF_copydataset(HID(x), -1, -1, -1);
	return H5Dsexp(s);
    }
    error("didn't copy properly");
    return R_NilValue; /* not reached */
}


/* create a copy of the dataset in */

hid_t HDF_copydataset(hid_t in, hid_t type, hid_t space, hid_t plist)
{
    char *foo, *bar;
    hid_t dnew;
    int newspace=0, newtype=0, newplist=0;

    /* some fiddling to get a valid tmpname */
    foo = tmpnam(NULL);
    bar = strrchr(foo, '/');
    bar++;
    
    if( space < 0 ) {
	space = H5Dget_space(in);
	newspace = 1;
    }
    if( plist < 0 ) {
	plist = H5Dget_create_plist(in);
	newplist = 1;
    }
    if( type < 0 ) {
	type = H5Dget_type(type);
	newtype = 1;
    }

    dnew = H5Dcreate(HID(HDF_workgroup), bar, type, space,
		  plist); 
    if( dnew < 0 )
	error("failed to create temporary dataset ");

    if( newspace )
	H5Sclose(space);
    if( newplist )
	H5Pclose(plist);
    if( newtype )
	H5Tclose(type);

    return dnew;
}







