#include "H5T.h"

/* hid_t H5Tcopy( hid_t dtype_id ) */
SEXP _H5Tcopy( SEXP _dtype_id ) {

  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  hid_t tid = H5Tcopy(dtype_id);

  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(tid));
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Tset_size( hid_t dtype_id, size_t size )  */
SEXP _H5Tset_size( SEXP _dtype_id, SEXP _size ) {

  hid_t dtype_id = STRSXP_2_HID( _dtype_id );

  herr_t herr;
  if (LENGTH(_size) > 0) {
    size_t size = INTEGER(_size)[0];
    herr = H5Tset_size(dtype_id, size);
  } else {
    herr = H5Tset_size(dtype_id, H5T_VARIABLE);
  }

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* size_t H5Tget_size(hid_t type_id); */
SEXP _H5Tget_size( SEXP _dtype_id ) {
  
  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  SEXP Rval = R_NilValue;

  if (!H5Tis_variable_str(dtype_id)) {
    size_t size = H5Tget_size( dtype_id );
    PROTECT(Rval = allocVector(INTSXP, 1));
    INTEGER(Rval)[0] = size;
    UNPROTECT(1);
  }

  return Rval;
}

/* herr_t H5Tset_strpad( hid_t dtype_id, H5T_str_t strpad ) */
SEXP _H5Tset_strpad( SEXP _dtype_id, SEXP _strpad ) {
  
  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  int strpad_int = INTEGER(_strpad)[0];
  H5T_str_t strpad;
  
  switch(strpad_int) {
  case 0:
    strpad = H5T_STR_NULLTERM;
    break;
  case 1:
    strpad = H5T_STR_NULLPAD;
    break;
  case 2:
    strpad = H5T_STR_SPACEPAD;
    break;
  default: 
    error("Unknown string padding argument\n");
  }
  
  herr_t herr = H5Tset_strpad(dtype_id, strpad);
  
  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* size_t H5Tget_strpad(hid_t type_id); */
SEXP _H5Tget_strpad( SEXP _dtype_id ) {
  
  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  H5T_str_t strpad = H5Tget_strpad( dtype_id );
  
  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = strpad;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Tset_cset( hid_t dtype_id, H5T_cset_t csetpad ) */
SEXP _H5Tset_cset( SEXP _dtype_id, SEXP _cset ) {

  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  int cset_int = INTEGER(_cset)[0];
  H5T_cset_t cset;

  switch(cset_int) {
  case 0:
    cset = H5T_CSET_ASCII;
    break;
  case 1:
    cset = H5T_CSET_UTF8;
    break;
  default:
    error("Unknown cset argument\n");
  }

  herr_t herr = H5Tset_cset(dtype_id, cset);

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* size_t H5Tget_cset(hid_t type_id); */
SEXP _H5Tget_cset( SEXP _dtype_id ) {

  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  H5T_cset_t cset = H5Tget_cset( dtype_id );

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = cset;
  UNPROTECT(1);
  return Rval;
}

/* htri_t H5Tis_variable_str( hid_t dtype_id ) */
SEXP _H5Tis_variable_str( SEXP _dtype_id ) {
  
  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  htri_t res = H5Tis_variable_str( dtype_id );
  
  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = res;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Tset_precision( hid_t dtype_id, size_t precision ) */
SEXP _H5Tset_precision( SEXP _dtype_id, SEXP _precision ) {
  
  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  size_t precision = INTEGER(_precision)[0];
  
  herr_t herr = H5Tset_precision(dtype_id, precision);
  
  SEXP Rval;
  PROTECT(Rval = ScalarInteger(herr));
  UNPROTECT(1);
  return Rval;
}

/* size_t H5Tget_precision( hid_t dtype_id ) */
SEXP _H5Tget_precision( SEXP _dtype_id ) {
  
  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  
  size_t precision = H5Tget_precision(dtype_id);
  
  SEXP Rval;
  PROTECT(Rval = ScalarInteger(precision));
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Tset_offset( hid_t dtype_id, size_t offset ) */
SEXP _H5Tset_offset( SEXP _dtype_id, SEXP _offset ) {
  
  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  size_t offset = INTEGER(_offset)[0];
  
  herr_t herr = H5Tset_precision(dtype_id, offset);
  
  SEXP Rval;
  PROTECT(Rval = ScalarInteger(herr));
  UNPROTECT(1);
  return Rval;
}

/* int H5Tget_offset( hid_t dtype_id ) */
SEXP _H5Tget_offset( SEXP _dtype_id ) {
  
  hid_t dtype_id = STRSXP_2_HID( _dtype_id );

  int offset = H5Tget_precision(dtype_id);
  
  SEXP Rval;
  PROTECT(Rval = ScalarInteger(offset));
  UNPROTECT(1);
  return Rval;
}

SEXP _H5Tenum_create( SEXP _base_id ) {
  
  hid_t base_id = STRSXP_2_HID( _base_id );
  
  hid_t tid = H5Tenum_create(base_id);
  
  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(tid));
  UNPROTECT(1);
  return Rval;
}

SEXP _H5Tenum_insert( SEXP _type, SEXP _name, SEXP _value ) {
  
  hid_t type = STRSXP_2_HID( _type );
  const char *name = CHAR(STRING_ELT(_name, 0));
  void * value = INTEGER(_value);

  herr_t herr = H5Tenum_insert(type, name, value);
  
  SEXP Rval;
  PROTECT(Rval = ScalarInteger(herr));
  UNPROTECT(1);
  return Rval;
}
  
SEXP _H5Tget_class( SEXP _dtype_id ) {

  hid_t type = STRSXP_2_HID( _dtype_id );
  H5T_class_t tid_class = H5Tget_class(type);
  SEXP Rval = PROTECT(allocVector(STRSXP, 1));

  switch(tid_class) {
  case H5T_INTEGER: {
    SET_STRING_ELT(Rval, 0, mkChar("H5T_INTEGER"));
  } break;
  case H5T_FLOAT: {
    SET_STRING_ELT(Rval, 0, mkChar("H5T_FLOAT"));
  } break;
  case H5T_TIME: {
    SET_STRING_ELT(Rval, 0, mkChar("H5T_TIME"));
  } break;
  case H5T_STRING: {
    SET_STRING_ELT(Rval, 0, mkChar("H5T_STRING"));
  } break;
  case H5T_BITFIELD: {
    SET_STRING_ELT(Rval, 0, mkChar("H5T_BITFIELD"));
  } break;
  case H5T_OPAQUE: {
    SET_STRING_ELT(Rval, 0, mkChar("H5T_OPAQUE"));
  } break;
  case H5T_COMPOUND: {
    SET_STRING_ELT(Rval, 0, mkChar("H5T_COMPOUND"));
  } break;
  case H5T_REFERENCE: {
    SET_STRING_ELT(Rval, 0, mkChar("H5T_REFERENCE"));
  } break;
  case H5T_ENUM: {
    SET_STRING_ELT(Rval, 0, mkChar("H5T_ENUM"));
  } break;
  case H5T_VLEN: {
    SET_STRING_ELT(Rval, 0, mkChar("H5T_VLEN"));
  } break;
  case H5T_ARRAY: {
    SET_STRING_ELT(Rval, 0, mkChar("H5T_ARRAY"));
  } break;
  default: {
    UNPROTECT(1);
    error("Unknown class");
  } break;
  }
  
  UNPROTECT(1);
  return Rval;
}