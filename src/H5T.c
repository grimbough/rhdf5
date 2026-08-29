#include "H5T.h"

SEXP _H5Tcreate(SEXP _type, SEXP _size) {

  H5T_class_t type = (H5T_class_t)INTEGER(_type)[0];
  size_t size = (size_t)INTEGER(_size)[0];

  hid_t tid = H5Tcreate(type, size);

  SEXP Rval = HID_2_STRSXP(tid);
  return Rval;
}

/* hid_t H5Tcopy( hid_t dtype_id ) */
SEXP _H5Tcopy(SEXP _dtype_id) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);
  hid_t tid = H5Tcopy(dtype_id);
  addHandle(tid);

  SEXP Rval = HID_2_STRSXP(tid);
  return Rval;
}

/* herr_t H5Tset_size( hid_t dtype_id, size_t size )  */
SEXP _H5Tset_size(SEXP _dtype_id, SEXP _size) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);

  herr_t herr;
  if (LENGTH(_size) > 0) {
    size_t size = INTEGER(_size)[0];
    herr = H5Tset_size(dtype_id, size);
  } else {
    herr = H5Tset_size(dtype_id, H5T_VARIABLE);
  }

  return ScalarInteger(herr);
}

/* size_t H5Tget_size(hid_t type_id); */
SEXP _H5Tget_size(SEXP _dtype_id) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);
  SEXP Rval = R_NilValue;

  if (!H5Tis_variable_str(dtype_id)) {
    size_t size = H5Tget_size(dtype_id);
    Rval = ScalarInteger(size);
  }

  return Rval;
}

/* herr_t H5Tset_strpad( hid_t dtype_id, H5T_str_t strpad ) */
SEXP _H5Tset_strpad(SEXP _dtype_id, SEXP _strpad) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);
  int strpad_int = INTEGER(_strpad)[0];
  H5T_str_t strpad;

  switch (strpad_int) {
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

  return ScalarInteger(herr);
}

/* size_t H5Tget_strpad(hid_t type_id); */
SEXP _H5Tget_strpad(SEXP _dtype_id) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);
  H5T_str_t strpad = H5Tget_strpad(dtype_id);

  return ScalarInteger(strpad);
}

/* herr_t H5Tset_cset( hid_t dtype_id, H5T_cset_t csetpad ) */
SEXP _H5Tset_cset(SEXP _dtype_id, SEXP _cset) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);
  int cset_int = INTEGER(_cset)[0];
  H5T_cset_t cset;

  switch (cset_int) {
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

  return ScalarInteger(herr);
}

/* size_t H5Tget_cset(hid_t type_id); */
SEXP _H5Tget_cset(SEXP _dtype_id) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);
  H5T_cset_t cset = H5Tget_cset(dtype_id);

  return ScalarInteger(cset);
}

/* htri_t H5Tis_variable_str( hid_t dtype_id ) */
SEXP _H5Tis_variable_str(SEXP _dtype_id) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);
  htri_t res = H5Tis_variable_str(dtype_id);

  return ScalarInteger(res);
}

/* herr_t H5Tset_precision( hid_t dtype_id, size_t precision ) */
SEXP _H5Tset_precision(SEXP _dtype_id, SEXP _precision) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);
  size_t precision = INTEGER(_precision)[0];

  herr_t herr = H5Tset_precision(dtype_id, precision);

  SEXP Rval = ScalarInteger(herr);
  return Rval;
}

/* size_t H5Tget_precision( hid_t dtype_id ) */
SEXP _H5Tget_precision(SEXP _dtype_id) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);

  size_t precision = H5Tget_precision(dtype_id);

  SEXP Rval = ScalarInteger(precision);
  return Rval;
}

/* herr_t H5Tset_offset( hid_t dtype_id, size_t offset ) */
SEXP _H5Tset_offset(SEXP _dtype_id, SEXP _offset) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);
  size_t offset = INTEGER(_offset)[0];

  herr_t herr = H5Tset_precision(dtype_id, offset);

  SEXP Rval = ScalarInteger(herr);
  return Rval;
}

/* int H5Tget_offset( hid_t dtype_id ) */
SEXP _H5Tget_offset(SEXP _dtype_id) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);

  int offset = H5Tget_precision(dtype_id);

  SEXP Rval = ScalarInteger(offset);
  return Rval;
}

SEXP _H5Tenum_create(SEXP _base_id) {

  hid_t base_id = STRSXP_2_HID(_base_id);

  hid_t tid = H5Tenum_create(base_id);

  SEXP Rval = HID_2_STRSXP(tid);
  return Rval;
}

SEXP _H5Tenum_insert(SEXP _type, SEXP _name, SEXP _value) {

  hid_t type = STRSXP_2_HID(_type);
  const char *name = CHAR(STRING_ELT(_name, 0));
  void *value = INTEGER(_value);

  herr_t herr = H5Tenum_insert(type, name, value);

  SEXP Rval = ScalarInteger(herr);
  return Rval;
}

/* int H5Tget_nmembers	(	hid_t 	type_id	)	 */
SEXP _H5Tget_nmembers(SEXP _dtype_id) {

  hid_t dtype_id = STRSXP_2_HID(_dtype_id);

  int nmembers = H5Tget_nmembers(dtype_id);

  SEXP Rval = ScalarInteger(nmembers);
  return Rval;
}

SEXP _H5Tget_class(SEXP _dtype_id) {

  hid_t type = STRSXP_2_HID(_dtype_id);
  H5T_class_t tid_class = H5Tget_class(type);
  SEXP Rval;

  switch (tid_class) {
  case H5T_INTEGER: {
    Rval = mkString("H5T_INTEGER");
  } break;
  case H5T_FLOAT: {
    Rval = mkString("H5T_FLOAT");
  } break;
  case H5T_TIME: {
    Rval = mkString("H5T_TIME");
  } break;
  case H5T_STRING: {
    Rval = mkString("H5T_STRING");
  } break;
  case H5T_BITFIELD: {
    Rval = mkString("H5T_BITFIELD");
  } break;
  case H5T_OPAQUE: {
    Rval = mkString("H5T_OPAQUE");
  } break;
  case H5T_COMPOUND: {
    Rval = mkString("H5T_COMPOUND");
  } break;
  case H5T_REFERENCE: {
    Rval = mkString("H5T_REFERENCE");
  } break;
  case H5T_ENUM: {
    Rval = mkString("H5T_ENUM");
  } break;
  case H5T_VLEN: {
    Rval = mkString("H5T_VLEN");
  } break;
  case H5T_ARRAY: {
    Rval = mkString("H5T_ARRAY");
  } break;
  default: {
    error("Unknown class");
  } break;
  }

  return Rval;
}

/* herr_t H5Tclose( hid_t dtype_id ) */
SEXP _H5Tclose( SEXP _dtype_id ) {
  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  herr_t herr = H5Tclose( dtype_id );
  if (herr == 0) {
    removeHandle(dtype_id);
  }
  
  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}