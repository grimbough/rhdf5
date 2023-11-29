/* General utility functions used by multiple other functions */
#include "utils.h"

void concatdim(char *s1, hsize_t next_dim, int index)
{
    
    char tmp[1000];
    memset(tmp, '\0',1000);
    strncpy(tmp, s1, 999);

    snprintf(s1, 1000, "%.977s%llu%.3s", tmp, next_dim, index ? " x " : "");
}

void concatdim_native(char *s1, hsize_t next_dim, int index)
{
    char tmp[1000];
    memset(tmp, '\0',1000);
    strncpy(tmp, s1, 999);
    
    snprintf(s1, 1000, "%.977s%.3s%llu", tmp, index ? " x " : "", next_dim);
}


void format_dimensions (H5S_class_t space_type, opObjListElement *newElement, hsize_t *size, hsize_t *maxsize, int native) {
    
    switch(space_type) {
    case H5S_SCALAR: {
        strncpy(newElement->dim, "( 0 )", 1000);
        strncpy(newElement->maxdim, "( 0 )", 1000);
    } break;
    case H5S_SIMPLE: {
        char* tmp = (char *) R_alloc(1000 * newElement->rank, sizeof(char));
        memset(tmp, '\0', 1000 * sizeof(char));
        if (native) {
            for(int i = 0; i < newElement->rank; i++) {
                concatdim_native(tmp, size[i], i);
            }
        } else {
            for(int i = newElement->rank-1; i >= 0; i--) {
                concatdim(tmp, size[i], i);
            }
        }
        strcpy(newElement->dim, tmp);
        
        if(maxsize[0] == H5S_UNLIMITED) {
            snprintf(tmp, 10, "UNLIMITED");
        } else {
            memset(tmp, '\0', 1000 * sizeof(char));
            if (native) {
                for(int i = 0; i < newElement->rank; i++) {
                    concatdim_native(tmp, maxsize[i], i);
                }
            } else {
                for(int i = newElement->rank-1; i >= 0; i--) {
                    concatdim(tmp, maxsize[i], i);
                }
            }
        }
        strcpy(newElement->maxdim, tmp);
    } break;
    case H5S_NULL: {;
        memset(newElement->dim, '\0', sizeof(char)); 
        memset(newElement->maxdim, '\0', sizeof(char)); 
    } break;
    case H5S_NO_CLASS:
    default:  {
        strncpy(newElement->dim, "unknown dataspace", 1000); 
        strncpy(newElement->maxdim, "unknown dataspace", 1000); 
    } break;
    } 
}

/************************************************************
 
 This function recursively searches the linked list of
 opdata structures for one whose address matches
 target_addr.  Returns 1 if a match is found, and 0
 otherwise.
 
 ************************************************************/
int group_check (struct opObjListElement *od, haddr_t target_addr, unsigned long target_fileno)
{
    if (od->addr == target_addr && od->fileno == target_fileno) {  /* Addresses match */
      return 1;
    } else if (!od->prev) {            /* Root group reached with no matches */
      return 0;       
    } else {                          /* Recursively examine the next node */
      return group_check (od->prev, target_addr, target_fileno);
    }
}

/* used in H5Dread and H5Aread when reading a string datatype */
void * read_string_datatype(hid_t mem_type_id, SEXP _buf) {
    if (!H5Tis_variable_str(mem_type_id)) {
        size_t stsize = H5Tget_size( mem_type_id );
        char * strbuf = (char *)R_alloc(LENGTH(_buf),stsize);
        int i, j, z=0;

        for (i=0; i < LENGTH(_buf); i++) {
            for (j=0; (j < LENGTH(STRING_ELT(_buf,i))) & (j < stsize); j++) {
                strbuf[z++] = CHAR(STRING_ELT(_buf,i))[j];
            }
            for (; j < stsize; j++) {
                strbuf[z++] = '\0';
            }
        }
        return(strbuf);
    } else {
        const char ** strbuf = (const char **)R_alloc(LENGTH(_buf), sizeof(char*));
        for (int i=0; i < LENGTH(_buf); i++) {
            strbuf[i] = CHAR(STRING_ELT(_buf, i));
        }
        return(strbuf);
    }
}
