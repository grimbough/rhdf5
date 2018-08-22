#include "H5O.h"

herr_t opAcount( hid_t g_id, const char *name, const H5A_info_t *info, void *op_data) {
  int *n = op_data;
  (*n)++;
  herr_t herr = 0;
  return(herr);
}

hsize_t H5Oget_num_attrs ( hid_t obj_id ) {
  hsize_t n=0;
  hsize_t idx=0;
  herr_t herr = H5Aiterate( obj_id, H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, &idx, &opAcount, &n);
  return(n);
}

SEXP _H5Oget_num_attrs ( SEXP _obj_id ) {
  //hid_t obj_id = INTEGER(_obj_id)[0];
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  hsize_t n = H5Oget_num_attrs ( obj_id );
  SEXP Rval = ScalarInteger(n);
  return(Rval);
}

/*
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
*/

SEXP _H5Oopen( SEXP _loc_id, SEXP _name) {
  //hid_t loc_id = INTEGER(_loc_id)[0];
  hid_t loc_id = STRSXP_2_HID( _loc_id );
  const char *name = CHAR(STRING_ELT(_name, 0));
  hid_t hid = H5Oopen( loc_id, name, H5P_DEFAULT );
  addHandle(hid);

  SEXP Rval;
  //PROTECT(Rval = allocVector(INTSXP, 1));
  //INTEGER(Rval)[0] = hid;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

SEXP _H5Oclose( SEXP _object_id ) {
  //hid_t object_id =  INTEGER(_object_id)[0];
  hid_t object_id = STRSXP_2_HID( _object_id );
  herr_t herr = H5Oclose( object_id );
  if (herr == 0) {
    removeHandle(object_id);
  }

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}


/* herr_t H5Oget_info( hid_t object_id, H5O_info_t *object_info ) */
/*
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
*/

/* herr_t H5Oget_info_by_name( hid_t loc_id, const char *object_name, H5O_info_t *object_info, hid_t lapl_id ) */
/*
SEXP _H5Oget_info_by_name( SEXP _loc_id, SEXP _object_name ) {
  //  hid_t loc_id = INTEGER(_loc_id)[0];
  const char *object_name = CHAR(STRING_ELT(_object_name, 0));
  H5O_info_t object_info;
  printf("%ud\n",sizeof(struct H5O_info_t));
  //  object_info = (H5O_info_t *)R_alloc(sizeof(struct H5O_info_t),1 );
  // object_info = (H5O_info_t *)malloc(sizeof(struct H5O_info_t) );
  hid_t loc_id = H5Fopen("myhdf5file.h5", H5F_ACC_RDWR, H5P_DEFAULT);
  if (H5Iis_valid(loc_id)) {
    printf("file id valid.\n");
    printf("name = %s\n", object_name);
    //    oid = H5Oopen(loc_id, name=object_name);
    //if (oid >= 0) {
      
    //  H5Oclose(oid);
    //} else {
    //  printf("Cannot open object.");
   // }
    
    herr_t herr = H5Oget_info_by_name( loc_id, object_name, &object_info, H5P_DEFAULT);
    printf("herr=%d\n",herr);
    H5Fclose(loc_id);
    printf("name = %s\n", object_name);
  } else {
    printf("file id not valid.\n");
    //    Rval = R_NilValue;
  }
  SEXP Rval;
  printf("return.\n");
  // if (herr < 0) {
    Rval = R_NilValue;
    //  } else {
    //  Rval = H5O_info_t2SEXP(object_info);
    // }
  //  Free(object_info);
  return Rval;
}

*/
