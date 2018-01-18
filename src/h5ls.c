#include "H5G.h"
#include "H5O.h"
#include <stdio.h>
#include "printdatatype.h"

typedef struct opLinfoListElement {
  long idx;
  char *name;
  char *group;
  char *datatype;
  char *class;
  char *spacetype;
  int rank;
  char *dim;
  char *maxdim;
  H5L_info_t info;
  H5I_type_t type;
  hsize_t num_attrs;
  //  H5O_info_t object_info;
  struct opLinfoListElement *next;
} opLinfoListElement;

typedef struct {
  long n;
  long depth;
  char *group;
  long maxdepth;
  int showdatasetinfo;
  H5_index_t index_type;
  H5_iter_order_t order;
  opLinfoListElement *first;
  opLinfoListElement *last;
} opLinfoList;

herr_t opAddToLinfoList( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) {
  opLinfoList *data = op_data;

  herr_t herr = 0;
  opLinfoListElement *newElement = (opLinfoListElement *)R_alloc(1,sizeof(struct opLinfoListElement) );
  newElement->idx = data->n;
  //  printf("sizeof = %ld cset=%ld group=>%s< name=>%s<\n", strlen(name), info->cset, data->group, name);
  newElement->name = (char *)R_alloc(1,(strlen(name)+1)*sizeof(char));
  strcpy(newElement->name, name);
  newElement->group = (char *)R_alloc(1,(strlen(data->group)+1)*sizeof(char));
  strcpy(newElement->group, data->group);
  newElement->info = (*info);

  if ((info->type == H5L_TYPE_SOFT) | (info->type == H5L_TYPE_ERROR)) {
    newElement->type = INT_MIN;
    newElement->num_attrs = 0;
    newElement->datatype = "";
    newElement->class = "";
    newElement->rank = 0;
    newElement->spacetype = "";
    newElement->dim = "";
    newElement->maxdim = "";
    newElement->next = NULL;

    data->n = data->n + 1;
    if (data->first == NULL) {
      data->first = newElement;
    } else {
      data->last->next = newElement;
    }
    data->last = newElement;
  } else {
    hid_t oid = H5Oopen( g_id, name, H5P_DEFAULT );
    // herr_t herr = H5Oget_info( oid, &(newElement->object_info) );
    newElement->type = H5Iget_type(oid);
    newElement->num_attrs = H5Oget_num_attrs(oid);
    if ((data->showdatasetinfo) & (newElement->type == H5I_DATASET)) {
      hid_t did = H5Dopen( g_id, name, H5P_DEFAULT );
      hid_t type = H5Dget_type(did);
      newElement->datatype = getDatatypeName(type);
      newElement->class = getDatatypeClass(type);
      /* H5Tclose(type); */
      hid_t sid = H5Dget_space( did );
      hsize_t   size[H5S_MAX_RANK];
      hsize_t   maxsize[H5S_MAX_RANK];
      newElement->rank = H5Sget_simple_extent_dims(sid, size, maxsize);

      H5S_class_t space_type = H5Sget_simple_extent_type(sid);
      switch(space_type) {
      case H5S_SCALAR:   newElement->spacetype = "SCALAR"; break;
      case H5S_SIMPLE:   newElement->spacetype = "SIMPLE"; break;
      case H5S_NULL:     newElement->spacetype = "NULL"; break;
      case H5S_NO_CLASS: newElement->spacetype = "NO_CLASS"; break;
      default:           newElement->spacetype = "unknown dataspace"; break;
      } /* end switch */
      newElement->dim = "";
      newElement->maxdim = "";
      switch(space_type) {
      case H5S_SCALAR: {
	newElement->dim = "( 0 )";
	newElement->maxdim = "( 0 )";
      } break;
      case H5S_SIMPLE: {
	char* tmp = (char *)R_alloc(100*newElement->rank,sizeof(char));
    #ifdef H5_HAVE_WINDOWS
	    sprintf(tmp, "%I64u", size[newElement->rank-1]);
    #else
	    sprintf(tmp, "%llu", size[newElement->rank-1]);
    #endif
	for(int i = newElement->rank-2; i >= 0; i--) {
      #ifdef H5_HAVE_WINDOWS      
        sprintf(tmp, "%s x %I64u", tmp, size[i]);
      #else
        sprintf(tmp, "%s x %llu", tmp, size[i]);
      #endif 
	}
	sprintf(tmp, "%s", tmp);
	newElement->dim = (char *)R_alloc((strlen(tmp)+1),sizeof(char));
	strcpy(newElement->dim, tmp);
	if(maxsize[0] == H5S_UNLIMITED) {
	  sprintf(tmp, "UNLIMITED");
	} else {
      #ifdef H5_HAVE_WINDOWS      
	    sprintf(tmp, "%I64u", maxsize[newElement->rank-1]);
      #else
	    sprintf(tmp, "%llu", maxsize[newElement->rank-1]);
      #endif 
	  for(int i = newElement->rank-2; i >= 0 ; i--) {
        #ifdef H5_HAVE_WINDOWS      
	      sprintf(tmp, "%s x %I64u", tmp, maxsize[i]);
        #else
	      sprintf(tmp, "%s x %llu", tmp, maxsize[i]);
        #endif
	  }
	  sprintf(tmp, "%s", tmp);
	}
	newElement->maxdim = (char *)R_alloc((strlen(tmp)+1),sizeof(char));
	strcpy(newElement->maxdim, tmp);
      } break;
      case H5S_NULL: {
	newElement->dim = ""; 
	newElement->maxdim = ""; 
      } break;
      case H5S_NO_CLASS:
      default:  {
	newElement->dim = "unknown dataspace"; 
	newElement->maxdim = "unknown dataspace"; 
      } break;
      } /* end switch */
      H5Sclose(sid);
      
      /* printf("type=%ld\n",H5T_STD_I32LE); */
      /* printf("type=%ld\n",H5T_IEEE_F32LE); */
      /* const char *typename = getDatatypeName(type, 1); */
      /* printf("type=%ld\n",hid); */
      /* char *typename; */
      /* typename = malloc(1001*sizeof(char)); */
      /* ssize_t s = H5Iget_name( hid, typename, 1000 ); */
      /* printf("size=%ld\n",s); */
      /* printf("name=%s\n\n",typename); */
      H5Dclose(did);
    } else {
      newElement->datatype = "";
      newElement->class = "";
      newElement->rank = 0;
      newElement->spacetype = "";
      newElement->dim = "";
      newElement->maxdim = "";
    }
    
    newElement->next = NULL;

    data->n = data->n + 1;
    if (data->first == NULL) {
      data->first = newElement;
    } else {
      data->last->next = newElement;
    }
    data->last = newElement;
    
    if (newElement->type == H5I_GROUP) {
      if ((data->maxdepth < 0) | (data->depth < data->maxdepth)) {
	hsize_t idx=0;
	char* group = data->group;
	data->group = (char *)R_alloc((strlen(name)+strlen(group)+2),sizeof(char));
	strcpy(data->group, group);
	if (data->depth > 1) {
	  strcat(data->group, "/");
	}
	strcat(data->group, name);
	data->depth = data->depth + 1;
	herr = H5Literate( oid, data->index_type, data->order, &idx, &opAddToLinfoList, op_data );
	data->depth = data->depth - 1;
	data->group = group;
      }
    }
    H5Oclose(oid);
    
  }

  return(herr);
}

SEXP _h5ls( SEXP _loc_id, SEXP _depth, SEXP _datasetinfo, SEXP _index_type, SEXP _order ) {
  hid_t loc_id = INTEGER(_loc_id)[0];
  opLinfoList data;
  data.n = 0;
  data.maxdepth = INTEGER(_depth)[0];
  data.depth = 1;
  data.group = (char *)R_alloc(2,sizeof(char));
  strcpy(data.group, "/");
  data.showdatasetinfo = INTEGER(_datasetinfo)[0];
  data.first = NULL;
  data.last = NULL;
  data.index_type = INTEGER(_index_type)[0];
  data.order = INTEGER(_order)[0];
  // H5_index_t index_type = H5_INDEX_NAME; 
  // H5_iter_order_t order = H5_ITER_INC; 
  hsize_t idx=0;
  //   printf("Start visit.\n"); 

  herr_t herr = H5Literate( loc_id, data.index_type, data.order, &idx, &opAddToLinfoList, &data );

  SEXP Rval;

  if (herr < 0) {
    PROTECT(Rval = allocVector(INTSXP, 1));
    INTEGER(Rval)[0] = herr;
    UNPROTECT(1);
  } else {
    PROTECT(Rval= allocVector(VECSXP, 14));
    // SEXP elementnames = PROTECT(allocVector(STRSXP, 0)); 
    SEXP group = PROTECT(allocVector(STRSXP, data.n));
    SEXP elementnames = PROTECT(allocVector(STRSXP, data.n));
    SEXP ltype = PROTECT(allocVector(INTSXP, data.n));
    SEXP corder_valid = PROTECT(allocVector(LGLSXP, data.n));
    SEXP corder = PROTECT(allocVector(INTSXP, data.n));
    SEXP cset = PROTECT(allocVector(INTSXP, data.n));
    //   SEXP fileno = PROTECT(allocVector(INTSXP, data.n));
    //    SEXP addr = PROTECT(allocVector(INTSXP, data.n));
    SEXP otype = PROTECT(allocVector(INTSXP, data.n));
    //    SEXP rc = PROTECT(allocVector(INTSXP, data.n));
    //    SEXP atime = PROTECT(allocVector(REALSXP, data.n));
    //    SEXP mtime = PROTECT(allocVector(REALSXP, data.n));
    //    SEXP ctime = PROTECT(allocVector(REALSXP, data.n));
    //    SEXP btime = PROTECT(allocVector(REALSXP, data.n));
    SEXP num_attrs = PROTECT(allocVector(INTSXP, data.n));
    SEXP dclass = PROTECT(allocVector(STRSXP, data.n));
    SEXP dtype = PROTECT(allocVector(STRSXP, data.n));
    SEXP stype = PROTECT(allocVector(STRSXP, data.n));
    SEXP rank = PROTECT(allocVector(INTSXP, data.n));
    SEXP dim = PROTECT(allocVector(STRSXP, data.n));
    SEXP maxdim = PROTECT(allocVector(STRSXP, data.n));
    SEXP rowNames = PROTECT(allocVector(INTSXP, data.n));

    opLinfoListElement *el = data.first;
    opLinfoListElement *elnext;
    while (el != NULL) {
      // printf("element %d\n",el->idx); 
      SET_STRING_ELT(group, el->idx, mkChar(el->group));
      SET_STRING_ELT(elementnames, el->idx, mkChar(el->name));
      INTEGER(ltype)[el->idx] = el->info.type;
      LOGICAL(corder_valid)[el->idx] = el->info.corder_valid;
      INTEGER(corder)[el->idx] = el->info.corder;
      INTEGER(cset)[el->idx] = el->info.cset;
      //      INTEGER(fileno)[el->idx] = el->object_info.fileno;
      //      INTEGER(addr)[el->idx] = el->object_info.addr;
      INTEGER(otype)[el->idx] = el->type;
      //      INTEGER(rc)[el->idx] = el->object_info.rc;
      //      REAL(atime)[el->idx] = el->object_info.atime;
      //      REAL(mtime)[el->idx] = el->object_info.mtime;
      //      REAL(ctime)[el->idx] = el->object_info.ctime;
      //      REAL(btime)[el->idx] = el->object_info.btime;
      INTEGER(num_attrs)[el->idx] = el->num_attrs;
      SET_STRING_ELT(dclass, el->idx, mkChar(el->class));
      SET_STRING_ELT(dtype, el->idx, mkChar(el->datatype));
      SET_STRING_ELT(stype, el->idx, mkChar(el->spacetype));
      INTEGER(rank)[el->idx] = el->rank;
      SET_STRING_ELT(dim, el->idx, mkChar(el->dim));
      SET_STRING_ELT(maxdim, el->idx, mkChar(el->maxdim));
      INTEGER(rowNames)[el->idx] = el->idx;
      elnext = el->next;
      el = elnext;
    }

    SET_VECTOR_ELT(Rval,0,group);
    SET_VECTOR_ELT(Rval,1,elementnames);
    SET_VECTOR_ELT(Rval,2,ltype);
    SET_VECTOR_ELT(Rval,3,corder_valid);
    SET_VECTOR_ELT(Rval,4,corder);
    SET_VECTOR_ELT(Rval,5,cset);
    //    SET_VECTOR_ELT(Rval,6,fileno);
    //    SET_VECTOR_ELT(Rval,7,addr);
    SET_VECTOR_ELT(Rval,6,otype);
    //    SET_VECTOR_ELT(Rval,9,rc);
    //    SET_VECTOR_ELT(Rval,10,atime);
    //    SET_VECTOR_ELT(Rval,11,mtime);
    //    SET_VECTOR_ELT(Rval,12,ctime);
    //    SET_VECTOR_ELT(Rval,13,btime);
    SET_VECTOR_ELT(Rval,7,num_attrs);
    SET_VECTOR_ELT(Rval,8,dclass);
    SET_VECTOR_ELT(Rval,9,dtype);
    SET_VECTOR_ELT(Rval,10,stype);
    SET_VECTOR_ELT(Rval,11,rank);
    SET_VECTOR_ELT(Rval,12,dim);
    SET_VECTOR_ELT(Rval,13,maxdim);

    SEXP names = PROTECT(allocVector(STRSXP, 14));
    SET_STRING_ELT(names, 0, mkChar("group"));
    SET_STRING_ELT(names, 1, mkChar("name"));
    SET_STRING_ELT(names, 2, mkChar("ltype"));
    SET_STRING_ELT(names, 3, mkChar("corder_valid"));
    SET_STRING_ELT(names, 4, mkChar("corder"));
    SET_STRING_ELT(names, 5, mkChar("cset"));
    //    SET_STRING_ELT(names, 6, mkChar("fileno"));
    //    SET_STRING_ELT(names, 7, mkChar("addr"));
    SET_STRING_ELT(names, 6, mkChar("otype"));
    //    SET_STRING_ELT(names, 9, mkChar("rc"));
    //    SET_STRING_ELT(names, 10, mkChar("atime"));
    //    SET_STRING_ELT(names, 11, mkChar("mtime"));
    //    SET_STRING_ELT(names, 12, mkChar("ctime"));
    //    SET_STRING_ELT(names, 13, mkChar("btime"));
    SET_STRING_ELT(names, 7, mkChar("num_attrs"));
    SET_STRING_ELT(names, 8, mkChar("dclass"));
    SET_STRING_ELT(names, 9, mkChar("dtype"));
    SET_STRING_ELT(names, 10, mkChar("stype"));
    SET_STRING_ELT(names, 11, mkChar("rank"));
    SET_STRING_ELT(names, 12, mkChar("dim"));
    SET_STRING_ELT(names, 13, mkChar("maxdim"));
    SET_NAMES(Rval, names);
    UNPROTECT(1);

    setAttrib(Rval, R_ClassSymbol, mkString("data.frame"));
    setAttrib(Rval, mkString("row.names"), rowNames);

    UNPROTECT(15);
    UNPROTECT(1);
  }


  return Rval; 
}

