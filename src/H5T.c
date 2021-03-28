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
