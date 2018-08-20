#include <stdlib.h>
#include "H5A.h"

/*################################*/
/* functions */
/*################################*/

/* hid_t H5Acreate( hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id ) */
SEXP _H5Acreate( SEXP _obj_id, SEXP _attr_name, SEXP _type_id, SEXP _space_id ) {
  //hid_t obj_id = INTEGER(_obj_id)[0];
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *attr_name = CHAR(STRING_ELT(_attr_name, 0));
  //hid_t type_id = INTEGER(_type_id)[0];
  //hid_t space_id =  INTEGER(_space_id)[0];
  hid_t type_id = STRSXP_2_HID( _type_id );
  hid_t space_id = STRSXP_2_HID( _space_id );

  hid_t hid = H5Acreate( obj_id, attr_name, type_id, space_id, 
			 H5P_DEFAULT, H5P_DEFAULT );
  addHandle(hid);

  SEXP Rval;
  //PROTECT(Rval = allocVector(INTSXP, 1));
  //INTEGER(Rval)[0] = hid;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Aopen( hid_t obj_id, const char *attr_name, hid_t aapl_id ) */
SEXP _H5Aopen( SEXP _obj_id, SEXP _attr_name ) {
  //hid_t obj_id = INTEGER(_obj_id)[0];
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *attr_name = CHAR(STRING_ELT(_attr_name, 0));
  hid_t hid = H5Aopen( obj_id, attr_name, H5P_DEFAULT );
  addHandle( hid );

  SEXP Rval;
  //PROTECT(Rval = allocVector(INTSXP, 1));
  //INTEGER(Rval)[0] = hid;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Aopen_by_name( hid_t loc_id, const char *obj_name, const char *attr_name, hid_t aapl_id, hid_t lapl_id ) */
SEXP _H5Aopen_by_name( SEXP _obj_id, SEXP _obj_name, SEXP _attr_name ) {
  //hid_t obj_id = INTEGER(_obj_id)[0];
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *obj_name = CHAR(STRING_ELT(_obj_name, 0));
  const char *attr_name = CHAR(STRING_ELT(_attr_name, 0));
  hid_t hid = H5Aopen_by_name( obj_id, obj_name, attr_name, H5P_DEFAULT, H5P_DEFAULT );
  addHandle( hid );

  SEXP Rval;
  //PROTECT(Rval = allocVector(INTSXP, 1));
  //INTEGER(Rval)[0] = hid;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Aopen_by_idx( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t aapl_id, hid_t lapl_id ) */
SEXP _H5Aopen_by_idx( SEXP _obj_id, SEXP _obj_name, SEXP _idx_type, SEXP _order, SEXP _n ) {
  //hid_t obj_id = INTEGER(_obj_id)[0];
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *obj_name = CHAR(STRING_ELT(_obj_name, 0));
  H5_index_t idx_type = INTEGER(_idx_type)[0];
  H5_iter_order_t order = INTEGER(_order)[0];
  hsize_t n = INTEGER(_n)[0];
  hid_t hid = H5Aopen_by_idx( obj_id, obj_name, idx_type, order, n, H5P_DEFAULT, H5P_DEFAULT );
  addHandle( hid );

  SEXP Rval;
  //PROTECT(Rval = allocVector(INTSXP, 1));
  //INTEGER(Rval)[0] = hid;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

/* htri_t H5Aexists( hid_t obj_id, const char *attr_name ) */
SEXP _H5Aexists( SEXP _obj_id, SEXP _attr_name ) {
  //hid_t obj_id = INTEGER(_obj_id)[0];
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *attr_name = CHAR(STRING_ELT(_attr_name, 0));
  htri_t htri = H5Aexists( obj_id, attr_name );
  SEXP Rval = ScalarInteger(htri);
  return Rval;
}

/* herr_t H5Aclose(hid_t attr_id) */
SEXP _H5Aclose( SEXP _attr_id ) {
  //hid_t attr_id = INTEGER(_attr_id)[0];
  hid_t attr_id = STRSXP_2_HID( _attr_id );
  herr_t herr = H5Aclose( attr_id );
  if (herr == 0) {
    removeHandle(attr_id);
  }

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Adelete( hid_t loc_id, const char *attr_name ) */
SEXP _H5Adelete( SEXP _obj_id, SEXP _attr_name ) {
  //hid_t obj_id = INTEGER(_obj_id)[0];
  hid_t obj_id = STRSXP_2_HID( _obj_id );
  const char *attr_name = CHAR(STRING_ELT(_attr_name, 0));
  herr_t herr = H5Adelete( obj_id, attr_name );
  SEXP Rval = ScalarInteger(herr);
  return Rval;
}


/* void getMemSpaceDim( hid_t file_space_id, hsize_t *size) { */
/*   hssize_t sel_hyper_nblocks = H5Sget_select_hyper_nblocks( file_space_id ); */
/*   int rank = H5Sget_simple_extent_ndims( file_space_id ); */
/*   hsize_t sizebuf[2*rank*sel_hyper_nblocks]; */
/*   H5Sget_select_hyper_blocklist(file_space_id, 0, sel_hyper_nblocks, sizebuf ); */
  
/*   int isnew; */
/*   for (int i=0; i < rank; i++) { */
/*     size[i] = 0; */
/*     for (int j=0; j < sel_hyper_nblocks; j++) { */
/*       isnew = 1; */
/*       for (int k=0; k < j; k++) { */
/* 	if ((sizebuf[j*2*rank+i] == sizebuf[k*2*rank+i])  */
/* 	    & (sizebuf[j*2*rank+i+rank] == sizebuf[k*2*rank+i+rank])) { */
/* 	  isnew=0; */
/* 	} */
/*       } */
/*       if (isnew != 0) { */
/* 	size[i] += (sizebuf[j*2*rank+i+rank] - sizebuf[j*2*rank+i] + 1); */
/*       } */
/*     } */
/*   } */
/* } */

SEXP H5Aread_helper_INTEGER(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, hid_t dtype_id) {
  hid_t mem_type_id = -1;

  SEXP Rval;
  /* if (cpdType < 0) { */
    mem_type_id = H5T_NATIVE_INT;
  /* } else { */
  /*   mem_type_id = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_INT)); */
  /*   herr_t status = H5Tinsert(mem_type_id, cpdField[0], 0, H5T_NATIVE_INT); */
  /*   for (int i=1; i<cpdNField; i++) { */
  /*     hid_t mem_type_id2 = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_INT)); */
  /*     herr_t status = H5Tinsert(mem_type_id2, cpdField[i], 0, mem_type_id); */
  /*     mem_type_id = mem_type_id2; */
  /*   } */
  /* } */
  void * buf;
  if (length(_buf) == 0) {
    Rval = PROTECT(allocVector(INTSXP, n));
    buf = INTEGER(Rval);
  } else {
    buf = INTEGER(_buf);
    Rval = _buf;
  }
  herr_t herr = H5Aread(attr_id, mem_type_id, buf );
  if (length(_buf) == 0) {
    setAttrib(Rval, R_DimSymbol, Rdim);
    UNPROTECT(1);
  }

  return(Rval);
}


SEXP H5Aread_helper_FLOAT(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, hid_t dtype_id) {
  hid_t mem_type_id = -1;

  SEXP Rval;
  /* if (cpdType < 0) { */
    mem_type_id = H5T_NATIVE_DOUBLE;
  /* } else { */
  /*   mem_type_id = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_DOUBLE)); */
  /*   herr_t status = H5Tinsert(mem_type_id, cpdField[0], 0, H5T_NATIVE_DOUBLE); */
  /*   for (int i=1; i<cpdNField; i++) { */
  /*     hid_t mem_type_id2 = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_DOUBLE)); */
  /*     herr_t status = H5Tinsert(mem_type_id2, cpdField[i], 0, mem_type_id); */
  /*     mem_type_id = mem_type_id2; */
  /*   } */
  /* } */
  void * buf;
  if (length(_buf) == 0) {
    Rval = PROTECT(allocVector(REALSXP, n));
    buf = REAL(Rval);
  } else {
    buf = REAL(_buf);
    Rval = _buf;
  }
  herr_t herr = H5Aread(attr_id, mem_type_id, buf );
  if (length(_buf) == 0) {
    setAttrib(Rval, R_DimSymbol, Rdim);
    UNPROTECT(1);
  }
  return(Rval);
}

SEXP H5Aread_helper_STRING(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, hid_t dtype_id) {
  hid_t mem_type_id = -1;

  SEXP Rval;
  size_t size = H5Tget_size(dtype_id);
  /* if (cpdType < 0) { */
    mem_type_id = dtype_id;
  /* } else { */
  /*   mem_type_id = H5Tcreate(H5T_COMPOUND, size); */
  /*   herr_t status = H5Tinsert(mem_type_id, cpdField[0], 0, dtype_id); */
  /*   for (int i=1; i<cpdNField; i++) { */
  /* 	hid_t mem_type_id2 = H5Tcreate(H5T_COMPOUND, size); */
  /* 	herr_t status = H5Tinsert(mem_type_id2, cpdField[i], 0, mem_type_id); */
  /* 	mem_type_id = mem_type_id2; */
  /*   } */
  /* } */
  Rval = PROTECT(allocVector(STRSXP, n));
  if (H5Tis_variable_str(dtype_id)) {
    char *bufSTR[n];
    herr_t herr = H5Aread(attr_id, mem_type_id, bufSTR );
    for (int i=0; i<n; i++) {
      SET_STRING_ELT(Rval, i, mkChar(bufSTR[i]));
      free(bufSTR[i]);
    }
  } else {
    char bufSTR[n][size];
    herr_t herr = H5Aread(attr_id, mem_type_id, bufSTR );
    char bufSTR2[n][size+1];
    for (int i=0; i<n; i++) {
      for (int j=0; j<size; j++) {
        bufSTR2[i][j] = bufSTR[i][j];
      }
      bufSTR2[i][size] = '\0';
    }

    for (int i=0; i<n; i++) {
      SET_STRING_ELT(Rval, i, mkChar(bufSTR2[i]));
    }
  }
  setAttrib(Rval, R_DimSymbol, Rdim);
  UNPROTECT(1);
  return(Rval);
}

/* SEXP H5Dread_helper_COMPOUND(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf,  */
/* 			    hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame ) { */
/*   hid_t mem_type_id = -1; */

/*   if ((LENGTH(Rdim) > 1) && compoundAsDataFrame) { */
/*     compoundAsDataFrame = 0; */
/*     printf("Warning: Cannot coerce multi-dimensional data to data.frame. Data returned it as list.\n"); */
/*   } */

/*   SEXP Rval; */
/*   if (cpdType < 0) { */
/*     int N = H5Tget_nmembers(dtype_id); */
/*     PROTECT(Rval = allocVector(VECSXP, N)); */
/*     SEXP names = PROTECT(allocVector(STRSXP, N)); */
/*     for (int i=0; i<N; i++) { */
/*       SET_STRING_ELT(names, i, mkChar(H5Tget_member_name(dtype_id,i))); */
/*       char* name[1]; */
/*       name[0] = H5Tget_member_name(dtype_id,i); */
/*       SEXP col;  */
/*       if (compoundAsDataFrame && (H5Tget_member_class(dtype_id,i) == H5T_COMPOUND)) { */
/* 	printf("Warning: Cannot read hierarchical compound data types as data.frame. Use 'compoundAsDataFrame=FALSE' instead. Values replaced by NA's.\n"); */
/* 	double na = R_NaReal; */
/* 	col = PROTECT(allocVector(REALSXP, n)); */
/* 	for (int i=0; i<n; i++) { REAL(col)[i] = na; } */
/* 	setAttrib(col, R_DimSymbol, Rdim); */
/* 	UNPROTECT(1); */
/*       } else { */
/* 	col = H5Dread_helper(dataset_id, file_space_id, mem_space_id, n, Rdim, _buf, */
/* 			     H5Tget_member_type(dtype_id,i), 1, name, compoundAsDataFrame); */
/*       } */
/*       SET_VECTOR_ELT(Rval, i, col); */
/*     } */
/*     SET_NAMES(Rval, names); */
/*     if (compoundAsDataFrame) { */
/*       SEXP rn = PROTECT(allocVector(INTSXP, INTEGER(Rdim)[0])); */
/*       for (int i=0; i<INTEGER(Rdim)[0]; i++) { INTEGER(rn)[i] = i+1; } */
/*       UNPROTECT(1); */
/*       setAttrib(Rval, mkString("row.names"), rn); */
/*       setAttrib(Rval, R_ClassSymbol, mkString("data.frame")); */
/*     } */
/*     UNPROTECT(2); */
/*   } else { */
/*     int N = H5Tget_nmembers(dtype_id); */
/*     PROTECT(Rval = allocVector(VECSXP, N)); */
/*     SEXP names = PROTECT(allocVector(STRSXP, N)); */
/*     for (int i=0; i<N; i++) { */
/*       SET_STRING_ELT(names, i, mkChar(H5Tget_member_name(dtype_id,i))); */
/*       char* name[cpdNField+1]; */
/*       name[0] = H5Tget_member_name(dtype_id,i); */
/*       for (int j=0; j<cpdNField; j++) { */
/* 	name[j+1] = cpdField[j]; */
/*       } */
/*       SEXP col = H5Dread_helper(dataset_id, file_space_id, mem_space_id, n, Rdim, _buf, */
/* 				H5Tget_member_type(dtype_id,i), cpdNField+1, name, compoundAsDataFrame); */
      
/*       SET_VECTOR_ELT(Rval, i, col); */
/*     } */
/*     SET_NAMES(Rval, names); */
/*     UNPROTECT(2); */
/*   } */
/*   return(Rval); */
/* } */

SEXP H5Aread_helper(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf ) {

  hid_t dtype_id;
  dtype_id = H5Aget_type(attr_id);
  hid_t dtypeclass_id = H5Tget_class(dtype_id);

  SEXP Rval;
  switch(dtypeclass_id) {
  case H5T_INTEGER: {
    Rval = H5Aread_helper_INTEGER(attr_id, n, Rdim, _buf, dtype_id);
  } break;
  case H5T_FLOAT: {
    Rval = H5Aread_helper_FLOAT(attr_id, n, Rdim, _buf, dtype_id);
  } break;
  case H5T_STRING: {
    Rval = H5Aread_helper_STRING(attr_id, n, Rdim, _buf, dtype_id);
  } break;
  case H5T_COMPOUND:
 /* { */
 /*    Rval = H5Aread_helper_COMPOUND(attr_id, n, Rdim, _buf, dtype_id); */
 /*  } break; */
  case H5T_TIME:
  case H5T_BITFIELD:
  case H5T_OPAQUE:
  case H5T_REFERENCE:
  case H5T_ENUM:
  case H5T_VLEN:
  case H5T_ARRAY:
  default: {
    double na = R_NaReal;
    Rval = PROTECT(allocVector(REALSXP, n));
    for (int i=0; i<n; i++) { REAL(Rval)[i] = na; }
    setAttrib(Rval, R_DimSymbol, Rdim);
    UNPROTECT(1);
    char str[256];
    sprintf(str, "Reading attribute data of type '%s' not yet implemented. Values replaced by NA's.", getDatatypeClass(dtype_id));
    warning(str);
  } break;
  }

  return(Rval);
}

/* herr_t H5Aread(hid_t attr_id, hid_t mem_type_id, void *buf ) */
SEXP _H5Aread( SEXP _attr_id, SEXP _buf ) {

  /***********************************************************************/
  /* attr_id                                                          */
  /***********************************************************************/
  //hid_t attr_id = INTEGER(_attr_id)[0];
  hid_t attr_id = STRSXP_2_HID( _attr_id );
  /***********************************************************************/
  /* file_space_id and get dimensionality of output file_space and buf   */
  /***********************************************************************/
  hid_t file_space_id = H5Aget_space( attr_id );
  int rank = H5Sget_simple_extent_ndims( file_space_id );
  hsize_t size[rank];
  hsize_t maxsize[rank];
  H5Sget_simple_extent_dims(file_space_id, size, maxsize);

  /***********************************************************************/
  /* create mem_space_id                                                 */
  /***********************************************************************/
  hsize_t n = 1;
  hid_t mem_space_id;
  SEXP Rdim;
  for (int i=0; i < rank; i++) {
      n = n * size[i];
  }
  hsize_t dims[rank];
  for (int i=0; i<rank; i++) {
    dims[i] = size[rank-i-1];
  }
  mem_space_id = H5Screate_simple( rank, dims, dims);
  if (rank > 0) {
    Rdim = PROTECT(allocVector(INTSXP, rank));
    for (int i=0; i<rank; i++) {
      INTEGER(Rdim)[i] = dims[i];
    }
  } else {
    Rdim = NULL_USER_OBJECT;
  }

  /***********************************************************************/
  /* read file space data type                                           */
  /***********************************************************************/

  SEXP Rval = H5Aread_helper(attr_id, n, Rdim, _buf);

  // close mem space
  H5Sclose(mem_space_id);
  if (rank > 0) {
    UNPROTECT(1);
  }

  // close file space
  H5Sclose(file_space_id);
  return Rval;
}

/* herr_t H5Awrite(hid_t attr_id, hid_t mem_type_id, const void *buf ) */
SEXP _H5Awrite( SEXP _attr_id, SEXP _buf) {
  //hid_t attr_id = INTEGER(_attr_id)[0];
  hid_t attr_id = STRSXP_2_HID( _attr_id );
  hid_t mem_type_id;

  const void * buf;
  if (TYPEOF(_buf) == INTSXP) {
    mem_type_id = H5T_NATIVE_INT;
    buf = INTEGER(_buf);
  } else {
    if (TYPEOF(_buf) == REALSXP) {
      mem_type_id = H5T_NATIVE_DOUBLE;
      buf = REAL(_buf);
    } else {
      if (TYPEOF(_buf) == STRSXP) {
	mem_type_id = H5Aget_type(attr_id);
	size_t stsize = H5Tget_size( mem_type_id );
	char * strbuf = (char *)R_alloc(LENGTH(_buf),stsize);
	int z=0;
	int j;
	for (int i=0; i < LENGTH(_buf); i++) {
	  for (j=0; (j < LENGTH(STRING_ELT(_buf,i))) & (j < (stsize-1)); j++) {
	    strbuf[z++] = CHAR(STRING_ELT(_buf,i))[j];
	  }
	  for (; j < stsize; j++) {
	    strbuf[z++] = '\0';
	  }
	}
	buf = strbuf;
      } else {
	mem_type_id = -1;
	warning("Writing of this type of attribute data not supported.");
	SEXP Rval = R_NilValue;
	return Rval;
      }
    }
  }
  herr_t herr = 3;
  herr = H5Awrite(attr_id, mem_type_id, buf );
  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* ssize_t H5Aget_name(hid_t attr_id, size_t buf_size, char *buf ) */
SEXP _H5Aget_name(SEXP _attr_id ) {
  //hid_t attr_id = INTEGER(_attr_id)[0];
  hid_t attr_id = STRSXP_2_HID( _attr_id );
  ssize_t s = H5Aget_name(attr_id, 0, NULL );
  char buf[s+1];
  H5Aget_name(attr_id, s+1, buf );
  SEXP name = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(name, 0, mkChar(buf));
  UNPROTECT(1);
  return name;
}

/* hid_t H5Aget_space(hid_t attr_id) */
SEXP _H5Aget_space(SEXP _attr_id ) {
  //hid_t attr_id = INTEGER(_attr_id)[0];
  hid_t attr_id = STRSXP_2_HID( _attr_id );
  hid_t sid = H5Aget_space( attr_id );
  addHandle(sid);
  //SEXP Rval = ScalarInteger( sid );
  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(sid));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Aget_type(hid_t attr_id) */
SEXP _H5Aget_type( SEXP _attr_id ) {
  //hid_t attr_id = INTEGER(_attr_id)[0];
  hid_t attr_id = STRSXP_2_HID( _attr_id );
  hid_t hid = H5Aget_type( attr_id );
  //SEXP Rval = ScalarInteger(hid);
  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(hid));
  UNPROTECT(1);
  return Rval;
}

