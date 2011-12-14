#include "H5O.h"

SEXP H5O_info_t2SEXP (H5O_info_t *object_info) {
  SEXP Rval = PROTECT(allocVector(VECSXP, 9));
  SET_VECTOR_ELT(Rval,0,ScalarInteger(object_info->fileno));
  SET_VECTOR_ELT(Rval,1,ScalarLogical(object_info->addr));
  SET_VECTOR_ELT(Rval,2,ScalarInteger(object_info->type));
  SET_VECTOR_ELT(Rval,3,ScalarInteger(object_info->rc));
  SET_VECTOR_ELT(Rval,4,ScalarReal(object_info->atime));
  SET_VECTOR_ELT(Rval,5,ScalarReal(object_info->mtime));
  SET_VECTOR_ELT(Rval,6,ScalarReal(object_info->ctime));
  SET_VECTOR_ELT(Rval,7,ScalarReal(object_info->btime));
  SET_VECTOR_ELT(Rval,8,ScalarInteger(object_info->num_attrs));
  SEXP names = PROTECT(allocVector(STRSXP, 9));
  SET_STRING_ELT(names, 0, mkChar("fileno"));
  SET_STRING_ELT(names, 1, mkChar("addr"));
  SET_STRING_ELT(names, 2, mkChar("type"));
  SET_STRING_ELT(names, 3, mkChar("rc"));
  SET_STRING_ELT(names, 4, mkChar("atime"));
  SET_STRING_ELT(names, 5, mkChar("mtime"));
  SET_STRING_ELT(names, 6, mkChar("ctime"));
  SET_STRING_ELT(names, 7, mkChar("btime"));
  SET_STRING_ELT(names, 8, mkChar("num_attrs"));
  SET_NAMES(Rval, names);
  UNPROTECT(2);
  return(Rval);
}

/* herr_t H5Oget_info( hid_t object_id, H5O_info_t *object_info ) */
SEXP _H5Oget_info( SEXP _object_id ) {
  hid_t object_id = INTEGER(_object_id)[0];
  H5O_info_t object_info;
  herr_t herr = H5Oget_info( object_id, &object_info);
  SEXP Rval;
  if (herr < 0) {
    Rval = R_NilValue;
  } else {
    Rval = H5O_info_t2SEXP(&object_info);
  }
  return Rval;
}

/* herr_t H5Oget_info_by_name( hid_t loc_id, const char *object_name, H5O_info_t *object_info, hid_t lapl_id ) */
SEXP _H5Oget_info_by_name( SEXP _loc_id, SEXP _object_name ) {
  hid_t loc_id = INTEGER(_loc_id)[0];
  const char *object_name = CHAR(STRING_ELT(_object_name, 0));
  H5O_info_t object_info;
  herr_t herr = H5Oget_info_by_name( loc_id, object_name, &object_info, H5P_DEFAULT);
  SEXP Rval;
  if (herr < 0) {
    Rval = R_NilValue;
  } else {
    Rval = H5O_info_t2SEXP(&object_info);
  }
  return Rval;
}

