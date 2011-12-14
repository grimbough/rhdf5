#include "H5L.h"

/* htri_t H5Lexists( hid_t loc_id, const char *name, hid_t lapl_id ); */
SEXP _H5Lexists( SEXP _loc_id, SEXP _name ) {
  hid_t loc_id = INTEGER(_loc_id)[0];
  const char *name = CHAR(STRING_ELT(_name, 0));
  htri_t htri = H5Lexists( loc_id, name, H5P_DEFAULT);
  SEXP Rval = ScalarInteger(htri);
  /* PROTECT(Rval = allocVector(INTSXP, 1)); */
  /* INTEGER(Rval)[0] = herr; */
  /* UNPROTECT(1); */
  return Rval;
}

SEXP H5L_info_t2SEXP (H5L_info_t *link_buff) {
  SEXP Rval = PROTECT(allocVector(VECSXP, 4));
  SET_VECTOR_ELT(Rval,0,ScalarInteger(link_buff->type));
  SET_VECTOR_ELT(Rval,1,ScalarLogical(link_buff->corder_valid));
  SET_VECTOR_ELT(Rval,2,ScalarInteger(link_buff->corder));
  SET_VECTOR_ELT(Rval,3,ScalarInteger(link_buff->cset));
  SEXP names = PROTECT(allocVector(STRSXP, 4));
  SET_STRING_ELT(names, 0, mkChar("type"));
  SET_STRING_ELT(names, 1, mkChar("corder_valid"));
  SET_STRING_ELT(names, 2, mkChar("corder"));
  SET_STRING_ELT(names, 3, mkChar("cset"));
  SET_NAMES(Rval, names);
  UNPROTECT(2);
  return(Rval);
}

/* herr_t H5Lget_info( hid_t link_loc_id, const char *link_name, H5L_info_t *link_buff, hid_t lapl_id ) */
SEXP _H5Lget_info( SEXP _loc_id, SEXP _name ) {
  hid_t loc_id = INTEGER(_loc_id)[0];
  const char *name = CHAR(STRING_ELT(_name, 0));
  H5L_info_t link_buff;
  herr_t herr = H5Lget_info( loc_id, name, &link_buff, H5P_DEFAULT);
  SEXP Rval;
  if (herr < 0) {
    Rval = R_NilValue;
  } else {
    Rval = H5L_info_t2SEXP(&link_buff);
  }
  return Rval;
}

