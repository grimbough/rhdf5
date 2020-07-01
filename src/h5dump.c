#include "h5dump.h"

herr_t opAddToDumpTree( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) {
    
    H5O_info_t infobuf;
    opDumpTree *data = (opDumpTree *) op_data;
    herr_t herr = 0;

    herr = H5Oget_info_by_name (g_id, name, &infobuf, H5P_DEFAULT);

    opObjListElement *newElement = (opObjListElement *) R_alloc(1, sizeof(struct opObjListElement) );
    newElement->idx = data->n;
    newElement->name = (char *) R_alloc((strlen(name)+1), sizeof(char));
    strcpy(newElement->name, name);
    newElement->group = (char *) R_alloc((strlen(data->group)+1), sizeof(char));
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
                newElement->child = NULL;
                newElement->next = NULL;

                data->n = data->n + 1;
                if (data->first == NULL) {
                    data->first = newElement;
                } else {
                    if (data->insertAsChild) {
                        data->last->child = newElement;
                        data->insertAsChild = 0;
                    } else {
                        data->last->next = newElement;
                    }
                }
                data->last = newElement;

                if ((data->maxdepth < 0) | (data->depth < data->maxdepth)) {
                    hsize_t idx=0;
                    char* group = data->group;
                    data->group = (char *) R_alloc((strlen(name)+strlen(group)+2), sizeof(char));
                    strcpy(data->group, group);
                    if (data->depth > 1) {
                        strcat(data->group, "/");
                    }
                    strcat(data->group, name);
                    data->insertAsChild = 1;
                    opObjListElement *last = data->last;
                    data->depth = data->depth + 1;
                    herr = H5Literate_by_name (g_id, name, H5_INDEX_NAME,
                                           data->order, NULL, opAddToDumpTree, (void *) data,
                                           H5P_DEFAULT);
                    data->depth = data->depth - 1;
                    data->insertAsChild = 0;
                    data->last = last;
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
            newElement->child = NULL;
            newElement->next = NULL;
            data->n = data->n + 1;
            if (data->first == NULL) {
                data->first = newElement;
            } else {
                if (data->insertAsChild) {
                    data->last->child = newElement;
                    data->insertAsChild = 0;
                } else {
                    data->last->next = newElement;
                }
            }
            data->last = newElement;
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

SEXP getTree(opObjListElement* elstart, opDumpTree* data, hid_t loc_id, int depth) {
    
        int n=0;
        opObjListElement *el = elstart;
        while (el != NULL) {
            n = n + 1;
            el = el->next;
        }

        SEXP Rval;
        PROTECT(Rval= allocVector(VECSXP, n));
        SEXP names = PROTECT(allocVector(STRSXP, n));
        
        n=0;
        el = elstart;
        while (el != NULL) {
            
            SET_STRING_ELT(names, n, mkChar(el->name));
            
            if (el->child != NULL) {
                SEXP childtree = getTree(el->child, data, loc_id, depth+1);
                SET_VECTOR_ELT(Rval,n,childtree);
            } else {
                if (el->type == H5I_GROUP) {
                    SET_VECTOR_ELT(Rval,n,R_NilValue);
                } else {
                    SEXP info = PROTECT(allocVector(VECSXP, 12));
                    SET_VECTOR_ELT(info,0,mkString("/"));  
                    SET_VECTOR_ELT(info,1,mkString(el->name));  
                    SET_VECTOR_ELT(info,2,ScalarInteger(el->info.type));  
                    SET_VECTOR_ELT(info,3,ScalarInteger(el->info.cset));
                    SET_VECTOR_ELT(info,4,ScalarInteger(el->type));
                    SET_VECTOR_ELT(info,5,ScalarInteger(el->num_attrs));
                    SET_VECTOR_ELT(info,6,mkString(el->dataclass));  
                    SET_VECTOR_ELT(info,7,mkString(el->datatype)); 
                    SET_VECTOR_ELT(info,8,mkString(el->spacetype)); 
                    SET_VECTOR_ELT(info,9,ScalarInteger(el->rank));
                    SET_VECTOR_ELT(info,10,mkString(el->dim));
                    SET_VECTOR_ELT(info,11,mkString(el->maxdim));
                    
                    SEXP infonames = PROTECT(allocVector(STRSXP, 12));
                    SET_STRING_ELT(infonames, 0, mkChar("group"));
                    SET_STRING_ELT(infonames, 1, mkChar("name"));
                    SET_STRING_ELT(infonames, 2, mkChar("ltype"));
                    SET_STRING_ELT(infonames, 3, mkChar("cset"));
                    SET_STRING_ELT(infonames, 4, mkChar("otype"));
                    SET_STRING_ELT(infonames, 5, mkChar("num_attrs"));
                    SET_STRING_ELT(infonames, 6, mkChar("dclass"));
                    SET_STRING_ELT(infonames, 7, mkChar("dtype"));
                    SET_STRING_ELT(infonames, 8, mkChar("stype"));
                    SET_STRING_ELT(infonames, 9, mkChar("rank"));
                    SET_STRING_ELT(infonames, 10, mkChar("dim"));
                    SET_STRING_ELT(infonames, 11, mkChar("maxdim"));
                    SET_NAMES(info, infonames);
                    setAttrib(info, R_ClassSymbol, mkString("data.frame"));
                    setAttrib(info, mkString("row.names"), ScalarInteger(1));
                    UNPROTECT(1);
                    
                    SET_VECTOR_ELT(Rval,n,info);
                    UNPROTECT(1);
                }
            }
            el = el->next;
            n = n + 1;
        }
        
        SET_NAMES(Rval, names);
        UNPROTECT(1);
        UNPROTECT(1);
        
        return(Rval);
    }

SEXP _h5dump( SEXP _loc_id, SEXP _depth, SEXP _index_type, SEXP _order ) {

    hid_t loc_id = STRSXP_2_HID( _loc_id );
    opDumpTree data;
    data.n = 0;
    data.maxdepth = INTEGER(_depth)[0];
    data.depth = 1;
    data.group = (char *)R_alloc(2,sizeof(char));
    strcpy(data.group, "/");
    data.showdatasetinfo = 2;
    data.insertAsChild = 0;
    data.first = NULL;
    data.last = NULL;
    data.index_type = (H5_index_t) INTEGER(_index_type)[0];
    data.order = (H5_iter_order_t) INTEGER(_order)[0];
    hsize_t idx=0;

    herr_t herr = H5Literate( loc_id, data.index_type, data.order, &idx, &opAddToDumpTree, &data );
    
    SEXP Rval;
    Rval = getTree(data.first, &data, loc_id, 0);
    
    
    return Rval;  
}

