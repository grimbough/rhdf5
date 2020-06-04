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
  size_t size = INTEGER(_size)[0];
  herr_t herr = H5Tset_size(dtype_id, size);

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* size_t H5Tget_size(hid_t type_id); */
SEXP _H5Tget_size( SEXP _dtype_id ) {
  
  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  size_t size = H5Tget_size( dtype_id );
  
  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = size;
  UNPROTECT(1);
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