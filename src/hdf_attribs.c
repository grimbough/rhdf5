#include "common.h"

#define MAXATTRLEN 30

static SEXP getattrbyindex(hid_t, int);

struct attrib_data {
  int   index;
  SEXP  data;
};

static int attrmatchname(hid_t attr, char *name)
{
    char buf[MAXATTRLEN];
    
    H5Aget_name(attr, MAXATTRLEN, buf);
    if( !strcmp(name, buf) )
	return 1;
    return 0;
}

/* set a single attribute on an HDF5 object */
SEXP HDF_attr_set(SEXP in, SEXP name, SEXP value)
{
    hid_t   attr, atype, aid;
    SEXP    dims;
    int     count, i, stringlen, tmp;
    char    *strval, *tstr;
    hsize_t *dlist;

  if( !isGROUP(in) && !isDATASET(in) )
      error("object is not an HDF5 group or dataset");
  if( !isString(name) )
      error("the attribute name must be a string");
  if( length(name) != 1 )
      error("only one attribute can be accessed at a time");

  /* Remove the old attribute if it exists */
  count = H5Aget_num_attrs(HID(in));
  for(i=0; i<count; i++) {
      attr = H5Aopen_idx(HID(in), i);
      if( attrmatchname(attr, STR(name)) ) {
	  H5Aclose(attr);
	  H5Adelete(HID(in), STR(name));
	  break;
      }
     H5Aclose(attr); 
  }

  dims = getAttrib(value, R_DimSymbol);
  if(dims == R_NilValue) { 
      count = 1;
      dlist = (hsize_t*)R_alloc(1, sizeof(hsize_t));
      dlist[0] = length(value);
      aid  = H5Screate_simple(count, dlist, NULL);  
      } 
  else { /* We are dealing with a matrix or larger */
      count = length(dims);
      dlist = (hsize_t*)R_alloc(sizeof(hsize_t), count);
      if(TYPEOF(dims) == INTSXP) {
	  for(i=0;i<count;i++)
	      dlist[i] = (hsize_t)INTEGER(dims)[i];
      } else {
	  for(i=0;i<count;i++)
	      dlist[i] = (hsize_t)REAL(dims)[i];
      }
      aid  = H5Screate_simple(count, dlist, NULL);
  }


  switch(TYPEOF(value)) {
  case STRSXP: 
      /* if it's a matrix, turn it in to a vector */
      if( count > 1 ) {
         warning("only vectors of strings can be added, attribute converted");
	 H5Sclose(aid);
	 count = 1;
	 dlist = (hsize_t*)R_alloc(1, sizeof(hsize_t));
	 dlist[0] = length(value);
	 aid  = H5Screate_simple(count, dlist, NULL);  
      } 
      /* get the length of the elements of the value */
      stringlen = 0;
      for(i=0; i<length(value); i++) {
	  tmp = length(STRING_ELT(value,i));
	  if( tmp > stringlen ) stringlen=tmp;
      }
      strval = (char *) R_alloc(length(value)*stringlen, sizeof(char));
      /* use strncpy to pad the string with nulls */
      tstr = strval;
      for(i=0; i<length(value); i++) {
	  strncpy(tstr, CHAR(STRING_ELT(value, i)), stringlen);
	  tstr+=stringlen;
      }
      atype = H5Tcopy(H5T_C_S1);
      H5Tset_size(atype, stringlen);
      H5Tset_strpad(atype, H5T_STR_NULLPAD);
      
      attr = H5Acreate(HID(in), STR(name), atype, aid, H5P_DEFAULT);
      if(attr >= 0)
	  i = H5Awrite(attr, atype, strval);
      else {
	  H5Sclose(aid);  H5Tclose(atype);
	  error("could not obtain attribute handle");
      }
      H5Tclose(atype);
      break;
  case INTSXP:
      attr = H5Acreate(HID(in), STR(name),H5T_NATIVE_INT, aid,
		       H5P_DEFAULT); 
      H5Awrite(attr, H5T_NATIVE_INT, INTEGER(value));
      break;
  case REALSXP:
      attr = H5Acreate(HID(in), STR(name), H5T_NATIVE_DOUBLE, aid,
		       H5P_DEFAULT); 
      H5Awrite(attr, H5T_NATIVE_DOUBLE, REAL(value));
      break;
  default:
      /* H5Aclose(attr); */ /* not needed - SDR */
      H5Sclose(aid);
      error("unsupported attribute type");
      attr = aid; /* -Wall */
  }
  H5Aclose(attr);  H5Sclose(aid);

  return(in);
}

/* get a single attribute */
SEXP HDF_attr_get(SEXP in, SEXP name)
{
    hid_t  attr;
    int i, count, found;
    
    if( !isGROUP(in) && !isDATASET(in) )
	error("object is not an HDF5 group or dataset");
    if( !isString(name) )
	error("argument is not an attribute name");
    if( length(name) > 1 )
	error("exactly one attribute name must be given");
    
    count = H5Aget_num_attrs(HID(in));
    if( count < 0 )
	error("failed to obtain the attributes");
    
    found = 0;
    for(i=0; i<count; i++) {
	attr = H5Aopen_idx(HID(in), i);
	if( attrmatchname(attr, STR(name)) ) {
	    found = 1;
	    H5Aclose(attr);
	    break;
	}
	H5Aclose(attr);
    }

    if( !found )
	return R_NilValue;

    return getattrbyindex(HID(in), i);
}



static SEXP getattrbyindex(hid_t data, int i)
{
    SEXP ans, dims;
    hid_t attr, atype, aspace;
    hsize_t *sdim;
    int rank, nelt, strsize;
    herr_t success;
    char *outstr;

    attr = H5Aopen_idx(data, i);
    atype = H5Aget_type(attr);
    aspace = H5Aget_space(attr);
    rank = H5Sget_simple_extent_ndims(aspace);
    sdim = (hsize_t*) R_alloc(rank, sizeof(hsize_t));
    H5Sget_simple_extent_dims(aspace, sdim, NULL);

    nelt = 1;
    for(i=0; i<rank; i++) 
	nelt *= (int) sdim[i];

    H5Sclose(aspace);

    switch(H5Tget_class(atype)) {
    case H5T_STRING:
	PROTECT(ans = allocVector(STRSXP, nelt));
	strsize = H5Tget_size(atype);
	outstr = (char*) R_alloc(nelt*strsize, sizeof(char));
#ifdef OLD
	for(i=0; i<nelt; i++)
	    outstr[i] = (char*) R_alloc(strsize, sizeof(char));
#endif
	success = H5Aread(attr, atype, outstr);
	for(i=0; i<nelt; i++) 
	    SET_STRING_ELT(ans, i, mkChar(outstr));
	break;
    case H5T_FLOAT:
	PROTECT(ans = allocVector(REALSXP, nelt));
	H5Aread(attr, H5T_NATIVE_DOUBLE, REAL(ans));
	break;
    case H5T_INTEGER:
	PROTECT(ans = allocVector(INTSXP, nelt));
	H5Aread(attr, H5T_NATIVE_INT, INTEGER(ans));
	break;
    default:
	H5Aclose(attr);  H5Tclose(atype);
	error("unknown type of attribute");
    ans = R_NilValue; /* -Wall */
    }

    H5Aclose(attr);  H5Tclose(atype);
  
    if( rank > 1 ) { /* put dims onto the return value */
	PROTECT(dims = allocVector(INTSXP, rank));
	for(i=0; i<rank; i++)
	    INTEGER(dims)[i] = (int) sdim[i];
	setAttrib(ans, R_DimSymbol, dims);
	UNPROTECT(1);
    }
    UNPROTECT(1);   
    return(ans);
}

herr_t attrib_names(hid_t loc, const char* name, void* data)
{
    struct attrib_data *d = (struct attrib_data *) data;
    SET_STRING_ELT(d->data, d->index++, mkChar(name));
    return 0;
}

/* obtain all attributes for an HDF5 group or dataset */
SEXP HDF_attribute_get(SEXP in)
{
    struct attrib_data d;
    int i, nattrs;
    SEXP names, values;
    
    if( !isGROUP(in) && !isDATASET(in) )
	error("wrong type of argument: only groups and datasets allowed"); 
    
    nattrs = H5Aget_num_attrs(HID(in));
    if( nattrs < 0 )
	error("unable to obtain number of attributes");
    if(nattrs == 0 )
	return R_NilValue;
    
    PROTECT(names = allocVector(STRSXP, nattrs));
    d.index = 0;
    d.data  = names;
    H5Aiterate(HID(in), NULL, attrib_names, (void*)&d);

    PROTECT(values = allocVector(VECSXP, nattrs));
    for(i=0; i<nattrs; i++)
	SET_VECTOR_ELT(values, i, getattrbyindex(HID(in), i));

    setAttrib(values, R_NamesSymbol, names);
    UNPROTECT(2);
    return values;
}

/* set the data type for data to Rtype */
int HDF_setRtype(hid_t data, int Rtype)
{
    int count, i;
    hid_t attr, aid;
    hsize_t dim[1];

    if( H5Iget_type(data) != H5I_DATASET )
	return -1;

    /* find and delete the attribute if it exists */
    count = H5Aget_num_attrs(data);
    for(i=0; i<count; i++) {
	attr = H5Aopen_idx(data, i);
	if( attrmatchname(attr, "*Rtype*") ) {
	  H5Aclose(attr);
	  H5Adelete(data, "*Rtype*");
	  break;
      }
     H5Aclose(attr); 
    }

    dim[0] = 1;
    aid  = H5Screate_simple(1, dim, NULL); 
    if( aid < 0 )
	error("couldn't create dataspace");

    attr = H5Acreate(data, "*Rtype*" , H5T_NATIVE_INT, aid,
		       H5P_DEFAULT);
    if( attr < 0 )
        error("attribute problem");

    H5Awrite(attr, H5T_NATIVE_INT, &Rtype);
    H5Sclose(aid);

    H5Aclose(attr);
    return 1;
}

/* find the Rtype of an HDF5 dataset,
   return -1 if no type is found */

int HDF_getRtype(hid_t data)
{
    int count, i, ans, found=0;
    int npt;
    hid_t attr, atype, aspace;

    if( H5Iget_type(data) != H5I_DATASET )
	error("argument is not a dataset");

    count = H5Aget_num_attrs(data);
    for(i=0; i<count; i++) {
	attr = H5Aopen_idx(data, i);
	if( attrmatchname(attr, "*Rtype*") ) {
	    found = 1;
	    break;
         }
         H5Aclose(attr); 
    }
    
    if( !found )
	return -1;

    atype = H5Aget_type(attr);
    aspace = H5Aget_space(attr);
    npt = (int) H5Sget_simple_extent_npoints(aspace);
    H5Sclose(aspace);
    
    if( npt!= 1 ) {
	H5Aclose(attr);  H5Tclose(atype);
	error("wrong number of values for Rtype");
    }
    if( !(H5Tget_class(atype) == H5T_INTEGER) ) {
	H5Aclose(attr);  H5Tclose(atype);
	error("wrong type of value for Rtype");
    }
    H5Aread(attr, H5T_NATIVE_INT, &ans);
    H5Aclose(attr);  H5Tclose(atype);
    return ans;
}


