#include "h5ls.h"

herr_t opAddToObjList( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data ) {
    
    H5O_info_t infobuf;
    opObjList *data = (struct opObjList *) op_data;
    herr_t herr = 0;
    
    herr = H5Oget_info_by_name (g_id, name, &infobuf, H5P_DEFAULT);
    
    struct opObjListElement *newElement = (opObjListElement *) R_alloc(1, sizeof(struct opObjListElement) );
    newElement->idx = data->n;
    newElement->name = (char *) R_alloc(1, (strlen(name)+1) * sizeof(char));
    strcpy(newElement->name, name);
    newElement->group = (char *) R_alloc(1, (strlen(data->group)+1) * sizeof(char));
    strcpy(newElement->group, data->group);
    newElement->info = (*info);
    newElement->addr = infobuf.addr;

    hid_t oid = H5Oopen( g_id, name, H5P_DEFAULT );
    newElement->type = H5Iget_type(oid);
    newElement->num_attrs = H5Oget_num_attrs(oid);
    H5Oclose(oid);
    
    switch (infobuf.type) {
        case H5O_TYPE_GROUP: {
            /* check if we have an recursive loop in the hierarchy */
            if ( data->n > 0 && group_check (data->last, infobuf.addr) ) {
                warning ("Identical objects found\n");
            } else { /* otherwise create a new element in our linked list */
            
            /* for a group these features don't really apply so we make them empty */
            newElement->datatype = (char *) R_alloc(1, sizeof(char));
            strncpy(newElement->datatype, "", 1);
            newElement->dataclass = (char *) R_alloc(1, sizeof(char));
            strncpy(newElement->dataclass, "", 1);
            newElement->rank = 0;
            strncpy(newElement->spacetype, "\0", 20);
            strncpy(newElement->dim, "", 100);
            strncpy(newElement->maxdim, "", 100);
            
            newElement->prev = data->last;
                data->n = data->n + 1;
                data->last = newElement;
                if (data->first == NULL) {
                    data->first = newElement;
                } 
                
                if ((data->maxdepth < 0) | (data->depth < data->maxdepth)) {
                    hsize_t idx=0;
                    char* group = data->group;
                    data->group = (char *)R_alloc((strlen(name)+strlen(group)+2), sizeof(char));
                    strcpy(data->group, group);
                    if (data->depth > 1) {
                        strcat(data->group, "/");
                    }
                    strcat(data->group, name);
                    data->depth = data->depth + 1;
                
                    herr = H5Literate_by_name (g_id, name, H5_INDEX_NAME,
                                           data->order, NULL, opAddToObjList, (void *) data,
                                           H5P_DEFAULT);
                    data->depth = data->depth - 1;
                    data->group = group;
                }
                
            }
            break;
        }
        case H5O_TYPE_DATASET: { ;
            hid_t did = H5Dopen( g_id, name, H5P_DEFAULT );
            hid_t type = H5Dget_type(did);
            hid_t sid = H5Dget_space( did );
            H5Dclose(did);
            
            newElement->datatype = getDatatypeName(type);
            newElement->dataclass = getDatatypeClass(type);
            
            hsize_t size[H5S_MAX_RANK];
            hsize_t maxsize[H5S_MAX_RANK];
            newElement->rank = H5Sget_simple_extent_dims(sid, size, maxsize);
            
            H5S_class_t space_type = H5Sget_simple_extent_type(sid);
            H5Sclose(sid);
            
            switch(space_type) {
            case H5S_SCALAR:   
                strncpy(newElement->spacetype, "SCALAR", 20); 
                break;
            case H5S_SIMPLE:   
                strncpy(newElement->spacetype, "SIMPLE", 20); 
                break;
            case H5S_NULL:     
                strncpy(newElement->spacetype, "NULL", 20); 
                break;
            case H5S_NO_CLASS: 
                strncpy(newElement->spacetype, "NO_CLASS", 20); 
                break;
            default:           
                strncpy(newElement->spacetype, "unknown dataspace", 20);
                break;
            } /* end switch */
    
            format_dimensions(space_type, newElement, size, maxsize, data->native);
            
            newElement->prev = data->last;
            data->n = data->n + 1;
            data->last = newElement;
            if (data->first == NULL) {
                data->first = newElement;
            } 
            break;
        }
        case H5O_TYPE_NAMED_DATATYPE: {
            Rprintf ("Datatype: %s\n", name);
            break;
        }
        default: {
            Rprintf ( "Unknown: %s\n", name);
        }
    }
    
    return(herr);
}

SEXP _h5ls( SEXP _loc_id, SEXP _depth, SEXP _datasetinfo, SEXP _index_type, SEXP _order, SEXP _native ) {
    
    hid_t loc_id = STRSXP_2_HID( _loc_id ); /* an open file handle */
        
        /* initialize linked list */                    
        opObjList data;
        data.n = 0;
        data.maxdepth = INTEGER(_depth)[0];
        data.depth = 1;
        data.group = (char *) R_alloc(2, sizeof(char));
        strcpy(data.group, "/");
        data.showdatasetinfo = INTEGER(_datasetinfo)[0];
        data.native = INTEGER(_native)[0];
        data.first = NULL;
        data.last = NULL;
        data.index_type = (H5_index_t) INTEGER(_index_type)[0];
        data.order = (H5_iter_order_t) INTEGER(_order)[0];
        hsize_t idx=0;
        
        herr_t herr = H5Literate( loc_id, data.index_type, data.order, &idx, &opAddToObjList, &data );
        
        SEXP Rval;
        
        if (herr < 0) {
            PROTECT(Rval = allocVector(INTSXP, 1));
            INTEGER(Rval)[0] = herr;
            UNPROTECT(1);
        } else {
            PROTECT(Rval= allocVector(VECSXP, 12));
            SEXP group = PROTECT(allocVector(STRSXP, data.n));
            SEXP elementnames = PROTECT(allocVector(STRSXP, data.n));
            SEXP ltype = PROTECT(allocVector(INTSXP, data.n));
            SEXP cset = PROTECT(allocVector(INTSXP, data.n));
            SEXP otype = PROTECT(allocVector(INTSXP, data.n));
            SEXP num_attrs = PROTECT(allocVector(INTSXP, data.n));
            SEXP dclass = PROTECT(allocVector(STRSXP, data.n));
            SEXP dtype = PROTECT(allocVector(STRSXP, data.n));
            SEXP stype = PROTECT(allocVector(STRSXP, data.n));
            SEXP rank = PROTECT(allocVector(INTSXP, data.n));
            SEXP dim = PROTECT(allocVector(STRSXP, data.n));
            SEXP maxdim = PROTECT(allocVector(STRSXP, data.n));
            SEXP rowNames = PROTECT(allocVector(INTSXP, data.n));
            
            opObjListElement *el = data.last;
            opObjListElement *elnext;
            while (el != NULL) {
                SET_STRING_ELT(group, el->idx, mkChar(el->group));
                SET_STRING_ELT(elementnames, el->idx, mkChar(el->name));
                INTEGER(ltype)[el->idx] = el->info.type;
                INTEGER(cset)[el->idx] = el->info.cset;
                INTEGER(otype)[el->idx] = el->type;
                INTEGER(num_attrs)[el->idx] = el->num_attrs;
                SET_STRING_ELT(dclass, el->idx, mkChar(el->dataclass));
                SET_STRING_ELT(dtype, el->idx, mkChar(el->datatype));
                SET_STRING_ELT(stype, el->idx, mkChar(el->spacetype));
                INTEGER(rank)[el->idx] = el->rank;
                SET_STRING_ELT(dim, el->idx, mkChar(el->dim));
                SET_STRING_ELT(maxdim, el->idx, mkChar(el->maxdim));
                INTEGER(rowNames)[el->idx] = el->idx;
                elnext = el->prev;
                el = elnext;
            }
            
            SET_VECTOR_ELT(Rval,0,group);
            SET_VECTOR_ELT(Rval,1,elementnames);
            SET_VECTOR_ELT(Rval,2,ltype);
            SET_VECTOR_ELT(Rval,3,cset);
            SET_VECTOR_ELT(Rval,4,otype);
            SET_VECTOR_ELT(Rval,5,num_attrs);
            SET_VECTOR_ELT(Rval,6,dclass);
            SET_VECTOR_ELT(Rval,7,dtype);
            SET_VECTOR_ELT(Rval,8,stype);
            SET_VECTOR_ELT(Rval,9,rank);
            SET_VECTOR_ELT(Rval,10,dim);
            SET_VECTOR_ELT(Rval,11,maxdim);
            
            SEXP names = PROTECT(allocVector(STRSXP, 12));
            SET_STRING_ELT(names, 0, mkChar("group"));
            SET_STRING_ELT(names, 1, mkChar("name"));
            SET_STRING_ELT(names, 2, mkChar("ltype"));
            SET_STRING_ELT(names, 3, mkChar("cset"));
            SET_STRING_ELT(names, 4, mkChar("otype"));
            SET_STRING_ELT(names, 5, mkChar("num_attrs"));
            SET_STRING_ELT(names, 6, mkChar("dclass"));
            SET_STRING_ELT(names, 7, mkChar("dtype"));
            SET_STRING_ELT(names, 8, mkChar("stype"));
            SET_STRING_ELT(names, 9, mkChar("rank"));
            SET_STRING_ELT(names, 10, mkChar("dim"));
            SET_STRING_ELT(names, 11, mkChar("maxdim"));
            SET_NAMES(Rval, names);
            UNPROTECT(1);
            
            setAttrib(Rval, R_ClassSymbol, mkString("data.frame"));
            setAttrib(Rval, mkString("row.names"), rowNames);
            
            UNPROTECT(13);
            UNPROTECT(1);
        }
        
        
        return Rval; 
}

