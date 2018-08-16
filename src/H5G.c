#include "H5G.h"

/* hid_t H5Gcreate( hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id ) */
/* TODO more parameters:  hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id */
SEXP _H5Gcreate( SEXP _loc_id, SEXP _name ) {
  //hid_t loc_id = INTEGER(_loc_id)[0];
  hid_t loc_id = STRSXP_2_HID( _loc_id );
  const char *name = CHAR(STRING_ELT(_name, 0));
  hid_t hid = H5Gcreate( loc_id, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
  addHandle(hid);

  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Gcreate_anon( hid_t loc_id, hid_t gcpl_id, hid_t gapl_id ) */
/* TODO more parameters:  hid_t gcpl_id, hid_t gapl_id */
SEXP _H5Gcreate_anon( SEXP _loc_id ) {
  //hid_t loc_id = INTEGER(_loc_id)[0];
  hid_t loc_id = STRSXP_2_HID( _loc_id );
  hid_t hid = H5Gcreate_anon( loc_id, H5P_DEFAULT, H5P_DEFAULT );
  addHandle(hid);

  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Gopen( hid_t loc_id, const char * name, hid_t gapl_id ) */
/* TODO more parameters: hid_t gapl_id */
SEXP _H5Gopen( SEXP _loc_id, SEXP _name ) {
  //hid_t loc_id = INTEGER(_loc_id)[0];
  hid_t loc_id = STRSXP_2_HID( _loc_id );
  const char *name = CHAR(STRING_ELT(_name, 0));
  hid_t hid = H5Gopen( loc_id, name, H5P_DEFAULT );
  addHandle(hid);

  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Gclose(hid_t group_id) */
SEXP _H5Gclose( SEXP _group_id ) {
  //hid_t group_id =  INTEGER(_group_id)[0];
  hid_t group_id = STRSXP_2_HID( _group_id );
  herr_t herr = H5Gclose( group_id );
  if (herr == 0) {
    removeHandle(group_id);
  }

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Gget_info( hid_t group_id, H5G_info_t *group_info ) */
SEXP _H5Gget_info( SEXP _group_id ) {
 // hid_t group_id = INTEGER(_group_id)[0];
  hid_t group_id = STRSXP_2_HID( _group_id );
  H5G_info_t group_info;
  herr_t herr = H5Gget_info( group_id, &group_info );

  SEXP rv;
  PROTECT(rv= allocVector(VECSXP, 4));

  SEXP storage_type;
  PROTECT(storage_type=allocVector(INTSXP,1));
  INTEGER(storage_type)[0] = group_info.storage_type;
  SET_VECTOR_ELT(rv,0,storage_type);
  UNPROTECT(1);

  SEXP nlinks;
  PROTECT(nlinks=allocVector(INTSXP,1));
  INTEGER(nlinks)[0] = group_info.nlinks;
  SET_VECTOR_ELT(rv,1,nlinks);
  UNPROTECT(1);

  SEXP max_corder;
  PROTECT(max_corder=allocVector(INTSXP,1));
  INTEGER(max_corder)[0] = group_info.max_corder;
  SET_VECTOR_ELT(rv,2,max_corder);
  UNPROTECT(1);

  SEXP mounted;
  PROTECT(mounted=allocVector(LGLSXP,1));
  INTEGER(mounted)[0] = group_info.mounted;
  SET_VECTOR_ELT(rv,3,mounted);
  UNPROTECT(1);
  
  SEXP names = PROTECT(allocVector(STRSXP, 4));
  SET_STRING_ELT(names, 0, mkChar("storage_type"));
  SET_STRING_ELT(names, 1, mkChar("nlink"));
  SET_STRING_ELT(names, 2, mkChar("max_corder"));
  SET_STRING_ELT(names, 3, mkChar("mounted"));
  SET_NAMES(rv, names);
  UNPROTECT(1);

  UNPROTECT(1);
  return(rv);
}

/* herr_t H5Gget_info_by_name( hid_t loc_id, const char *group_name, H5G_info_t *group_info, hid_t lapl_id ) */
/* TODO more parameters:  hid_t lapl_id */
SEXP _H5Gget_info_by_name( SEXP _loc_id, SEXP _group_name ) {
  //hid_t loc_id = INTEGER(_loc_id)[0];
  hid_t loc_id = STRSXP_2_HID( _loc_id );    
  const char *group_name = CHAR(STRING_ELT(_group_name, 0));
  H5G_info_t group_info;
  herr_t herr = H5Gget_info_by_name( loc_id, group_name, &group_info, H5P_DEFAULT );

  SEXP rv;
  PROTECT(rv= allocVector(VECSXP, 4));

  SEXP storage_type;
  PROTECT(storage_type=allocVector(INTSXP,1));
  INTEGER(storage_type)[0] = group_info.storage_type;
  SET_VECTOR_ELT(rv,0,storage_type);
  UNPROTECT(1);

  SEXP nlinks;
  PROTECT(nlinks=allocVector(INTSXP,1));
  INTEGER(nlinks)[0] = group_info.nlinks;
  SET_VECTOR_ELT(rv,1,nlinks);
  UNPROTECT(1);

  SEXP max_corder;
  PROTECT(max_corder=allocVector(INTSXP,1));
  INTEGER(max_corder)[0] = group_info.max_corder;
  SET_VECTOR_ELT(rv,2,max_corder);
  UNPROTECT(1);

  SEXP mounted;
  PROTECT(mounted=allocVector(LGLSXP,1));
  INTEGER(mounted)[0] = group_info.mounted;
  SET_VECTOR_ELT(rv,3,mounted);
  UNPROTECT(1);
  
  SEXP names = PROTECT(allocVector(STRSXP, 4));
  SET_STRING_ELT(names, 0, mkChar("storage_type"));
  SET_STRING_ELT(names, 1, mkChar("nlink"));
  SET_STRING_ELT(names, 2, mkChar("max_corder"));
  SET_STRING_ELT(names, 3, mkChar("mounted"));
  SET_NAMES(rv, names);
  UNPROTECT(1);

  UNPROTECT(1);
  return(rv);
}

/* herr_t H5Gget_info_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, hsize_t n, H5G_info_t *group_info, hid_t lapl_id ) */
/* TODO more parameters:  hid_t lapl_id */
SEXP _H5Gget_info_by_idx( SEXP _loc_id, SEXP _group_name,  SEXP _index_type, SEXP _order, SEXP _n) {
  //hid_t loc_id = INTEGER(_loc_id)[0];
  hid_t loc_id = STRSXP_2_HID( _loc_id );    
  const char *group_name = CHAR(STRING_ELT(_group_name, 0));
  H5_index_t index_type = INTEGER(_index_type)[0];
  H5_iter_order_t order = INTEGER(_order)[0];
  hsize_t n = INTEGER(_n)[0];
  H5G_info_t group_info;
  herr_t herr = H5Gget_info_by_idx( loc_id, group_name, index_type, order, n, &group_info, H5P_DEFAULT );

  SEXP rv;
  PROTECT(rv= allocVector(VECSXP, 4));

  SEXP storage_type;
  PROTECT(storage_type=allocVector(INTSXP,1));
  INTEGER(storage_type)[0] = group_info.storage_type;
  SET_VECTOR_ELT(rv,0,storage_type);
  UNPROTECT(1);

  SEXP nlinks;
  PROTECT(nlinks=allocVector(INTSXP,1));
  INTEGER(nlinks)[0] = group_info.nlinks;
  SET_VECTOR_ELT(rv,1,nlinks);
  UNPROTECT(1);

  SEXP max_corder;
  PROTECT(max_corder=allocVector(INTSXP,1));
  INTEGER(max_corder)[0] = group_info.max_corder;
  SET_VECTOR_ELT(rv,2,max_corder);
  UNPROTECT(1);

  SEXP mounted;
  PROTECT(mounted=allocVector(LGLSXP,1));
  INTEGER(mounted)[0] = group_info.mounted;
  SET_VECTOR_ELT(rv,3,mounted);
  UNPROTECT(1);
  
  SEXP names = PROTECT(allocVector(STRSXP, 4));
  SET_STRING_ELT(names, 0, mkChar("storage_type"));
  SET_STRING_ELT(names, 1, mkChar("nlink"));
  SET_STRING_ELT(names, 2, mkChar("max_corder"));
  SET_STRING_ELT(names, 3, mkChar("mounted"));
  SET_NAMES(rv, names);
  UNPROTECT(1);

  UNPROTECT(1);
  return(rv);
}

