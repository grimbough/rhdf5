#include "H5F.h"

/* hid_t H5Fcreate( const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id ) */
/* TODO more parameters: hid_t fcpl_id, hid_t fapl_id */
SEXP _H5Fcreate( SEXP _name, SEXP _flags ) {
  const char *name = CHAR(STRING_ELT(_name, 0));
  unsigned flags = INTEGER(_flags)[0];
  hid_t hid = H5Fcreate( name, flags, H5P_DEFAULT, H5P_DEFAULT );
  if (hid > 0) {
    addFileHandle(hid, name);
  }
  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = hid;
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Fopen( const char *name, unsigned flags, hid_t fapl_id ) */
/* TODO more parameters: , unsigned flags, hid_t fapl_id */
SEXP _H5Fopen( SEXP _name, SEXP _flags ) {
  const char *name = CHAR(STRING_ELT(_name, 0));
  unsigned flags = INTEGER(_flags)[0];
  hid_t hid = H5Fopen( name, flags, H5P_DEFAULT );
  if (hid > 0) {
    addFileHandle(hid, name);
  }

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = hid;
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Freopen(hid_t file_id ) */
SEXP _H5Freopen( SEXP _file_id ) {
  hid_t file_id =  INTEGER(_file_id)[0];
  hid_t hid = H5Freopen( file_id );
  if (hid > 0) {
    H5Handle h = handleInfo(file, file_id);
    addFileHandle(hid, h.name);
  }

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = hid;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Fclose( hid_t file_id ) */
SEXP _H5Fclose( SEXP _file_id ) {
  hid_t file_id =  INTEGER(_file_id)[0];
  herr_t herr = H5Fclose( file_id );
  if (herr == 0) {
    removeHandle(file, file_id);
  }

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Fflush(hid_t object_id, H5F_scope_t scope ) */
SEXP _H5Fflush(SEXP _object_id, SEXP _scope ) {
  hid_t object_id =  INTEGER(_object_id)[0];
  H5F_scope_t scope = INTEGER(_scope)[0];

  herr_t herr = H5Fflush(object_id, scope );
  SEXP Rval = ScalarInteger(herr);
  return(Rval);
}




