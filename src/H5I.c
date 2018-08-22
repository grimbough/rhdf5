#include "H5I.h"

// H5I_type_t H5Iget_type( hid_t obj_id )
SEXP _H5Iis_valid( SEXP _obj_id ) {
  hid_t obj_id = INTEGER(_obj_id)[0];
  int v = H5Iis_valid(obj_id);
  SEXP Rval = ScalarLogical(v);
  return(Rval);
}

// H5I_type_t H5Iget_type( hid_t obj_id )
SEXP _H5Iget_type( SEXP _obj_id ) {
  hid_t obj_id = INTEGER(_obj_id)[0];
  H5I_type_t t = H5Iget_type(obj_id);
  SEXP Rval = ScalarInteger(t);
  return(Rval);
}

/* ssize_t H5Iget_name( hid_t obj_id, char *name, size_t size ) */
SEXP _H5Iget_name( SEXP _obj_id ) {
  hid_t obj_id = INTEGER(_obj_id)[0];
  ssize_t st = H5Iget_name( obj_id, NULL, 0 );
  
  //char name[st+1];
  char *name = R_alloc(st+1, sizeof(char));
  
  H5Iget_name( obj_id, name, st+1 );
  SEXP Rval;
  Rval = mkString(name);
  return(Rval);
}

