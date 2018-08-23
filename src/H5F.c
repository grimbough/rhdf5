#include "H5F.h"

/* hid_t H5Fcreate( const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id ) */
/* TODO more parameters: hid_t fcpl_id, hid_t fapl_id */
SEXP _H5Fcreate( SEXP _name, SEXP _flags, SEXP _fcpl_id, SEXP _fapl_id ) {
  const char *name = CHAR(STRING_ELT(_name, 0));
  unsigned flags = INTEGER(_flags)[0];

  hid_t fcpl_id = H5P_DEFAULT;
  hid_t fapl_id = H5P_DEFAULT;
  if (length(_fcpl_id) > 0) { fcpl_id = STRSXP_2_HID( _fcpl_id ); }
  if (length(_fapl_id) > 0) { fapl_id = STRSXP_2_HID( _fapl_id ); }

  hid_t hid = H5Fcreate( name, flags, fcpl_id, fapl_id );
  addHandle(hid);

  SEXP Rval;
  //PROTECT(Rval = allocVector(INTSXP, 1));
  //INTEGER(Rval)[0] = hid;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Fopen( const char *name, unsigned flags, hid_t fapl_id ) */
/* TODO more parameters: , unsigned flags, hid_t fapl_id */
SEXP _H5Fopen( SEXP _name, SEXP _flags ) {
  const char *name = CHAR(STRING_ELT(_name, 0));
  unsigned flags = INTEGER(_flags)[0];
  hid_t hid = H5Fopen( name, flags, H5P_DEFAULT );
  addHandle(hid);

  SEXP Rval;
  //PROTECT(Rval = allocVector(INTSXP, 1));
  //INTEGER(Rval)[0] = hid;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Freopen(hid_t file_id ) */
SEXP _H5Freopen( SEXP _file_id ) {
  //hid_t file_id =  INTEGER(_file_id)[0];
  hid_t file_id = STRSXP_2_HID( _file_id );    
  hid_t hid = H5Freopen( file_id );
  addHandle(file_id);

  SEXP Rval;
  //PROTECT(Rval = allocVector(INTSXP, 1));
  //INTEGER(Rval)[0] = hid;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Fclose( hid_t file_id ) */
SEXP _H5Fclose( SEXP _file_id ) {
  //hid_t file_id =  INTEGER(_file_id)[0];
  hid_t file_id = STRSXP_2_HID( _file_id );
  herr_t herr = H5Fclose( file_id );
  if (herr == 0) {
    removeHandle(file_id);
  }

  //SEXP Rval;
  //PROTECT(Rval = allocVector(INTSXP, 1));
  //INTEGER(Rval)[0] = herr;
  SEXP Rval = ScalarInteger(herr);
  //UNPROTECT(1);
  return Rval;
}

/* herr_t H5Fflush(hid_t object_id, H5F_scope_t scope ) */
SEXP _H5Fflush(SEXP _object_id, SEXP _scope ) {
  //hid_t object_id =  INTEGER(_object_id)[0];
  hid_t object_id = STRSXP_2_HID( _object_id );    
  H5F_scope_t scope = INTEGER(_scope)[0];

  herr_t herr = H5Fflush(object_id, scope );
  SEXP Rval = ScalarInteger(herr);
  return(Rval);
}

/* htri_t H5Fis_hdf5(const char *name ) */
SEXP _H5Fis_hdf5( SEXP _name ) {
  const char *name = CHAR(STRING_ELT(_name, 0));
  htri_t htri = H5Fis_hdf5( name );
  SEXP Rval;
  PROTECT(Rval = allocVector(LGLSXP, 1));
  if (htri >= 0) {
    LOGICAL(Rval)[0] = htri;
  } else {
    LOGICAL(Rval)[0] = NA_LOGICAL;
  }
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Fget_filesize( hid_t file_id, hsize_t *size ) */
SEXP _H5Fget_filesize( SEXP _file_id ) {
  //hid_t file_id =  INTEGER(_file_id)[0];
  hid_t file_id = STRSXP_2_HID( _file_id );    
  hsize_t size;
  herr_t herr = H5Fget_filesize( file_id, &size );
  SEXP Rval;
  PROTECT(Rval = allocVector(REALSXP, 1));
  if (herr >= 0) {
    REAL(Rval)[0] = size;
  } else {
    REAL(Rval)[0] = NA_REAL;
  }
  UNPROTECT(1);
  return Rval;  
}

/* ssize_t H5Fget_name(hid_t obj_id, char *name, size_t size ) */
SEXP _H5Fget_name( SEXP _obj_id ) {
  //hid_t obj_id =  INTEGER(_obj_id)[0];
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  ssize_t size = H5Fget_name( obj_id, NULL, 0);
  SEXP Rval;
  PROTECT(Rval = allocVector(STRSXP, 1));
  if (size >= 0) {
    //char name[size+1];
    char *name = R_alloc(size+1, sizeof(char));
    size = H5Fget_name( obj_id, name, size+1);
    if (size >= 0) {
      SET_STRING_ELT(Rval, 0, mkChar(name));
    } else {
      SET_STRING_ELT(Rval, 0, NA_STRING);
    }
  } else {
    SET_STRING_ELT(Rval, 0, NA_STRING);
  }
  UNPROTECT(1);
  return Rval;  
}

/* ssize_t H5Fget_obj_count( hid_t file_id, unsigned int types ) */

/* hid_t H5Fget_create_plist(hid_t file_id ) { */
SEXP _H5Fget_create_plist( SEXP _file_id ) {
  //hid_t file_id =  INTEGER(_file_id)[0];
  hid_t file_id = STRSXP_2_HID( _file_id );
  hid_t plist_id = H5Fget_create_plist( file_id );
  //SEXP Rval = ScalarInteger(plist_id);
  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(plist_id));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Fget_access_plist(hid_t file_id ) { */
SEXP _H5Fget_access_plist( SEXP _file_id ) {
  //hid_t file_id =  INTEGER(_file_id)[0];
  hid_t file_id = STRSXP_2_HID( _file_id );
  hid_t plist_id = H5Fget_access_plist( file_id );
  //SEXP Rval = ScalarInteger(plist_id);
  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(plist_id));
  UNPROTECT(1);
  return Rval;
}

