#include <stdlib.h>
#include "H5A.h"

/*################################*/
/* functions */
/*################################*/

/* hid_t H5Acreate( hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id ) */
SEXP _H5Acreate( SEXP _obj_id, SEXP _attr_name, SEXP _type_id, SEXP _space_id ) {
  
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *attr_name = CHAR(STRING_ELT(_attr_name, 0));
  hid_t type_id = STRSXP_2_HID( _type_id );
  hid_t space_id = STRSXP_2_HID( _space_id );

  hid_t hid = H5Acreate( obj_id, attr_name, type_id, space_id, 
			 H5P_DEFAULT, H5P_DEFAULT );
  addHandle(hid);

  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Aopen( hid_t obj_id, const char *attr_name, hid_t aapl_id ) */
SEXP _H5Aopen( SEXP _obj_id, SEXP _attr_name ) {
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *attr_name = CHAR(STRING_ELT(_attr_name, 0));
  hid_t hid = H5Aopen( obj_id, attr_name, H5P_DEFAULT );
  addHandle( hid );

  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Aopen_by_name( hid_t loc_id, const char *obj_name, const char *attr_name, hid_t aapl_id, hid_t lapl_id ) */
SEXP _H5Aopen_by_name( SEXP _obj_id, SEXP _obj_name, SEXP _attr_name ) {
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *obj_name = CHAR(STRING_ELT(_obj_name, 0));
  const char *attr_name = CHAR(STRING_ELT(_attr_name, 0));
  hid_t hid = H5Aopen_by_name( obj_id, obj_name, attr_name, H5P_DEFAULT, H5P_DEFAULT );
  addHandle( hid );

  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Aopen_by_idx( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t aapl_id, hid_t lapl_id ) */
SEXP _H5Aopen_by_idx( SEXP _obj_id, SEXP _obj_name, SEXP _idx_type, SEXP _order, SEXP _n ) {
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *obj_name = CHAR(STRING_ELT(_obj_name, 0));
  H5_index_t idx_type = (H5_index_t) INTEGER(_idx_type)[0];
  H5_iter_order_t order = (H5_iter_order_t) INTEGER(_order)[0];
  hsize_t n = INTEGER(_n)[0];
  hid_t hid = H5Aopen_by_idx( obj_id, obj_name, idx_type, order, n, H5P_DEFAULT, H5P_DEFAULT );
  addHandle( hid );

  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* htri_t H5Aexists( hid_t obj_id, const char *attr_name ) */
SEXP _H5Aexists( SEXP _obj_id, SEXP _attr_name ) {
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *attr_name = CHAR(STRING_ELT(_attr_name, 0));
  htri_t htri = H5Aexists( obj_id, attr_name );
  SEXP Rval = ScalarInteger(htri);
  return Rval;
}

/* herr_t H5Aclose(hid_t attr_id) */
SEXP _H5Aclose( SEXP _attr_id ) {
  hid_t attr_id = STRSXP_2_HID( _attr_id );
  herr_t herr = H5Aclose( attr_id );
  if (herr == 0) {
    removeHandle(attr_id);
  }

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Adelete( hid_t loc_id, const char *attr_name ) */
SEXP _H5Adelete( SEXP _obj_id, SEXP _attr_name ) {
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *attr_name = CHAR(STRING_ELT(_attr_name, 0));
  herr_t herr = H5Adelete( obj_id, attr_name );
  SEXP Rval = ScalarInteger(herr);
  return Rval;
}


SEXP H5Aread_helper_INTEGER(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, hid_t dtype_id,
                            int bit64conversion) {
    
  hid_t mem_type_id = -1;

  SEXP Rval;
  int b = H5Tget_size(dtype_id);
  H5T_sign_t sgn = H5Tget_sign(dtype_id);
  herr_t herr;
  
  if((b < 4) | ((b == 4) & (sgn == H5T_SGN_2))) {
      mem_type_id = H5T_NATIVE_INT;
    
      void * buf;
      if (length(_buf) == 0) {
        Rval = PROTECT(allocVector(INTSXP, n));
        buf = INTEGER(Rval);
      } else {
        buf = INTEGER(_buf);
        Rval = _buf;
      }
      herr = H5Aread(attr_id, mem_type_id, buf );
      if (length(_buf) == 0) {
        setAttrib(Rval, R_DimSymbol, Rdim);
        UNPROTECT(1);
      }
  } else if ( ((b == 4) & (sgn == H5T_SGN_NONE)) | (b == 8) ) { 
      // unsigned32-bit or 64-bit integer
      void* intbuf;
      void* buf;
      
      if(b == 4) {
          mem_type_id = H5T_STD_U32LE;
          intbuf = R_alloc(n, sizeof(unsigned int));
      } else if((b == 8) & (sgn == H5T_SGN_NONE)) {
          mem_type_id = H5T_NATIVE_UINT64;
          intbuf = R_alloc(n, sizeof(unsigned long long));
      } else {
          mem_type_id = H5T_NATIVE_INT64;
          intbuf = R_alloc(n, sizeof(long long));
      }
      
      if (intbuf == 0) {
          error("Not enough memory to read the attribute.");
      }
      
      herr = H5Aread(attr_id, mem_type_id, intbuf );
      if(herr< 0) {
        error("Error reading attribute");
      }
      
      if (bit64conversion == 0) {  // Convert data to R-integer and replace overflow values with NA_integer

          if (length(_buf) == 0) {
              Rval = PROTECT(allocVector(INTSXP, n));
              buf = (int *) INTEGER(Rval);
          } else {
              buf = INTEGER(_buf);
              Rval = _buf;
          }
          if ((b == 4) & (sgn == H5T_SGN_NONE)) {
              uint32_to_int32(intbuf, n, buf);
          } else if (b == 8) { 
              int64_to_int32(intbuf, n, buf, sgn);
          }
      } else {

          if (length(_buf) == 0) {
              Rval = PROTECT(allocVector(REALSXP, n));
              buf = (long long *) REAL(Rval);
          } else {
              buf = REAL(_buf);
              Rval = _buf;
          }
          if (bit64conversion == 1) {  //convert to double
              if ((b == 4) & (sgn == H5T_SGN_NONE)) {
                  uint32_to_double(intbuf, n, buf);
              } else if (b == 8) {
                  int64_to_double(intbuf, n, buf, sgn);
              }
          } else { // convert to integer64
              if((b == 4) & (sgn == H5T_SGN_NONE)) {
                  uint32_to_integer64(intbuf, n, buf);
              } else if (b == 8) {
                  int64_to_integer64(intbuf, n, buf, sgn);
              }
              SEXP la = PROTECT(mkString("integer64"));
              setAttrib(Rval, R_ClassSymbol, la);
              UNPROTECT(1);
          }
      }
      
      if (length(_buf) == 0) {
          setAttrib(Rval, R_DimSymbol, Rdim);
          UNPROTECT(1);
      }
  }

  return(Rval);
}


SEXP H5Aread_helper_FLOAT(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, hid_t dtype_id) {
  hid_t mem_type_id = -1;

  SEXP Rval;
  mem_type_id = H5T_NATIVE_DOUBLE;
  void * buf;
  if (length(_buf) == 0) {
    Rval = PROTECT(allocVector(REALSXP, n));
    buf = REAL(Rval);
  } else {
    buf = REAL(_buf);
    Rval = _buf;
  }
  
  herr_t herr = H5Aread(attr_id, mem_type_id, buf );
  if(herr < 0) {
    error("Error reading attribute");
  }
  
  if (length(_buf) == 0) {
    setAttrib(Rval, R_DimSymbol, Rdim);
    UNPROTECT(1);
  }
  return(Rval);
}

SEXP H5Aread_helper_STRING(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, hid_t dtype_id) {
  hid_t mem_type_id = -1;

  SEXP Rval;
  size_t size = H5Tget_size(dtype_id);
  mem_type_id = dtype_id;
  Rval = PROTECT(allocVector(STRSXP, n));
  if (H5Tis_variable_str(dtype_id)) {
    char *bufSTR[n];
    herr_t herr = H5Aread(attr_id, mem_type_id, bufSTR );
    if(herr < 0) { error("Error reading attribute"); }
    for (int i=0; i<n; i++) {
      SET_STRING_ELT(Rval, i, mkChar(bufSTR[i]));
      free(bufSTR[i]);
    }
  } else {
    char bufSTR[n][size];
    
    herr_t herr = H5Aread(attr_id, mem_type_id, bufSTR );
    if(herr < 0) { error("Error reading attribute"); }
    
    char bufSTR2[n][size+1];
    for (int i=0; i<n; i++) {
      for (int j=0; j<size; j++) {
        bufSTR2[i][j] = bufSTR[i][j];
      }
      bufSTR2[i][size] = '\0';
    }

    for (int i=0; i<n; i++) {
      SET_STRING_ELT(Rval, i, mkChar(bufSTR2[i]));
    }
  }
  setAttrib(Rval, R_DimSymbol, Rdim);
  UNPROTECT(1);
  return(Rval);
}

SEXP H5Aread_helper_REFERENCE(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, hid_t dtype_id) {
  
  void *references;
  SEXP Rrefs, Rtype, Rval; 
  
  if(H5Tequal(dtype_id, H5T_STD_REF_OBJ)) {
    Rrefs = PROTECT(allocVector(RAWSXP, sizeof(hobj_ref_t) * n ));
    Rtype = PROTECT(ScalarInteger(H5R_OBJECT));
  } else if (H5Tequal(dtype_id, H5T_STD_REF_DSETREG)) {
    Rrefs = PROTECT(allocVector(RAWSXP, sizeof(hdset_reg_ref_t) * n ));
    Rtype = PROTECT(ScalarInteger(H5R_DATASET_REGION));
  } else {
    error("Unkown reference type");
    return R_NilValue;
  }
  
  references = RAW(Rrefs);
  herr_t err = H5Aread(attr_id, dtype_id, references);
  if (err < 0) {
    error("could not read attribute");
    return R_NilValue;
  }
  
  Rval = PROTECT(R_do_new_object(R_getClassDef("H5Ref")));
  R_do_slot_assign(Rval, mkString("val"), Rrefs);
  R_do_slot_assign(Rval, mkString("type"), Rtype);
  UNPROTECT(3);
  return Rval;
}

SEXP H5Aread_helper(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, int bit64conversion ) {
    
    hid_t dtype_id;
    dtype_id = H5Aget_type(attr_id);
    hid_t dtypeclass_id = H5Tget_class(dtype_id);

  SEXP Rval;
  switch(dtypeclass_id) {
  case H5T_INTEGER: {
    Rval = H5Aread_helper_INTEGER(attr_id, n, Rdim, _buf, dtype_id, bit64conversion);
  } break;
  case H5T_FLOAT: {
    Rval = H5Aread_helper_FLOAT(attr_id, n, Rdim, _buf, dtype_id);
  } break;
  case H5T_STRING: {
    Rval = H5Aread_helper_STRING(attr_id, n, Rdim, _buf, dtype_id);
  } break;
  case H5T_REFERENCE: {
    Rval = H5Aread_helper_REFERENCE(attr_id, n, Rdim, _buf, dtype_id);
  } break;
  case H5T_COMPOUND:
 /* { */
 /*    Rval = H5Aread_helper_COMPOUND(attr_id, n, Rdim, _buf, dtype_id); */
 /*  } break; */
  case H5T_TIME:
  case H5T_BITFIELD:
  case H5T_OPAQUE:
  case H5T_ENUM:
  case H5T_VLEN:
  case H5T_ARRAY:
  default: {
    double na = R_NaReal;
    Rval = PROTECT(allocVector(REALSXP, n));
    for (int i=0; i<n; i++) { REAL(Rval)[i] = na; }
    setAttrib(Rval, R_DimSymbol, Rdim);
    UNPROTECT(1);
    char str[256];
    sprintf(str, "Reading attribute data of type '%s' not yet implemented. Values replaced by NA's.", getDatatypeClass(dtype_id));
    warning(str);
  } break;
  }

  return(Rval);
}

/* herr_t H5Aread(hid_t attr_id, hid_t mem_type_id, void *buf ) */
SEXP _H5Aread( SEXP _attr_id, SEXP _buf, SEXP _bit64conversion ) {
    
  int bit64conversion = INTEGER(_bit64conversion)[0];

  /***********************************************************************/
  /* attr_id                                                          */
  /***********************************************************************/
  hid_t attr_id = STRSXP_2_HID( _attr_id );
  /***********************************************************************/
  /* file_space_id and get dimensionality of output file_space and buf   */
  /***********************************************************************/
  hid_t file_space_id = H5Aget_space( attr_id );
  int rank = H5Sget_simple_extent_ndims( file_space_id );
  hsize_t size[rank];
  hsize_t maxsize[rank];
  H5Sget_simple_extent_dims(file_space_id, size, maxsize);

  /***********************************************************************/
  /* create mem_space_id                                                 */
  /***********************************************************************/
  hsize_t n = 1;
  hid_t mem_space_id;
  SEXP Rdim;
  for (int i=0; i < rank; i++) {
      n = n * size[i];
  }
  hsize_t dims[rank];
  for (int i=0; i<rank; i++) {
    dims[i] = size[rank-i-1];
  }
  mem_space_id = H5Screate_simple( rank, dims, dims);
  if (rank > 0) {
    Rdim = PROTECT(allocVector(INTSXP, rank));
    for (int i=0; i<rank; i++) {
      INTEGER(Rdim)[i] = dims[i];
    }
  } else {
    Rdim = NULL_USER_OBJECT;
  }

  /***********************************************************************/
  /* read file space data type                                           */
  /***********************************************************************/

  SEXP Rval = H5Aread_helper(attr_id, n, Rdim, _buf, bit64conversion);

  // close mem space
  H5Sclose(mem_space_id);
  if (rank > 0) {
    UNPROTECT(1);
  }

  // close file space
  H5Sclose(file_space_id);
  return Rval;
}

/* herr_t H5Awrite(hid_t attr_id, hid_t mem_type_id, const void *buf ) */
SEXP _H5Awrite( SEXP _attr_id, SEXP _buf) {
    hid_t attr_id = STRSXP_2_HID( _attr_id );
    hid_t mem_type_id;

    const void * buf;
    static const char* H5Ref[] = {"H5Ref", ""};
    
    switch(TYPEOF(_buf)) {
    case INTSXP :
        mem_type_id = H5T_NATIVE_INT;
        buf = INTEGER(_buf);
        break;
    case REALSXP :
        mem_type_id = H5T_NATIVE_DOUBLE;
        buf = REAL(_buf);
        break;
    case STRSXP :
        mem_type_id = H5Aget_type(attr_id);
        size_t stsize;
        if (H5Tis_variable_str(mem_type_id)) {
            const char ** strbuf = (const char **)R_alloc(LENGTH(_buf),sizeof(const char*));
            for (int i=0; i < LENGTH(_buf); i++) {
            strbuf[i] = CHAR(STRING_ELT(_buf,i));
            }
            buf = strbuf;
        } else {
            stsize = H5Tget_size( mem_type_id );
            char * strbuf = (char *)R_alloc(LENGTH(_buf),stsize);
            int z=0;
            int j;
            for (int i=0; i < LENGTH(_buf); i++) {
            for (j=0; (j < LENGTH(STRING_ELT(_buf,i))) & (j < (stsize-1)); j++) {
                strbuf[z++] = CHAR(STRING_ELT(_buf,i))[j];
            }
            for (; j < stsize; j++) {
                strbuf[z++] = '\0';
            }
            }
            buf = strbuf;
        }
        break;
    case S4SXP : 
      if(R_check_class_etc(_buf, H5Ref) >= 0) {
        if(INTEGER(R_do_slot(_buf, mkString("type")))[0] == H5R_OBJECT) {
            mem_type_id = H5T_STD_REF_OBJ;
        } else if (INTEGER(R_do_slot(_buf, mkString("type")))[0] == H5R_DATASET_REGION) {
            mem_type_id = H5T_STD_REF_DSETREG;
        } else {
            mem_type_id = -1;
            Rf_error("Error writing references");
        }
        }
        buf = RAW(R_do_slot(_buf, mkString("val")));
        break;
    default :
        mem_type_id = -1;
        error("Writing of this type of attribute data not supported.");
        SEXP Rval = R_NilValue;
        return Rval;
    }

    herr_t herr = H5Awrite(attr_id, mem_type_id, buf );
    if(herr < 0) { error("Error writing attribute"); }
    SEXP Rval;
    PROTECT(Rval = allocVector(INTSXP, 1));
    INTEGER(Rval)[0] = herr;
    UNPROTECT(1);
    return Rval;
}

/* ssize_t H5Aget_name(hid_t attr_id, size_t buf_size, char *buf ) */
SEXP _H5Aget_name(SEXP _attr_id ) {
  hid_t attr_id = STRSXP_2_HID( _attr_id );
  ssize_t s = H5Aget_name(attr_id, 0, NULL );
  char buf[s+1];
  H5Aget_name(attr_id, s+1, buf );
  SEXP name = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(name, 0, mkChar(buf));
  UNPROTECT(1);
  return name;
}

/* hid_t H5Aget_space(hid_t attr_id) */
SEXP _H5Aget_space(SEXP _attr_id ) {
  hid_t attr_id = STRSXP_2_HID( _attr_id );
  hid_t sid = H5Aget_space( attr_id );
  addHandle(sid);
  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(sid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Aget_type(hid_t attr_id) */
SEXP _H5Aget_type( SEXP _attr_id ) {
  hid_t attr_id = STRSXP_2_HID( _attr_id );
  hid_t hid = H5Aget_type( attr_id );
  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

