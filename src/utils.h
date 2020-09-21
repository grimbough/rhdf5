#ifndef _utils_H
#define _utils_H

#include <stdlib.h>
#include <string.h>
#include "myhdf5.h"

/* used in h5ls and h5dump */
typedef struct opObjListElement {
    long idx;
    char *name;
    char *group;
    char *datatype;
    char *dataclass;
    char spacetype[20];
    int rank;
    char dim[1000];
    char maxdim[1000];
    H5L_info_t info;
    H5I_type_t type;
    hsize_t num_attrs;
    struct opObjListElement *prev; /* Pointer to previous element */
    struct opObjListElement *next; /* Pointer to previous element */
    struct opObjListElement *child;
    haddr_t addr;                  /* Group address */
} opObjListElement;

/* used in h5ls */
typedef struct opObjList {
    long n;
    long depth;
    char *group;
    long maxdepth;
    int showdatasetinfo;
    int native;
    H5_index_t index_type;
    H5_iter_order_t order;
    opObjListElement *first;
    opObjListElement *last;
} opObjList;

/* used in h5dump */
typedef struct {
    long n;
    long depth;
    char *group;
    long maxdepth;
    int showdatasetinfo;
    int native;
    H5_index_t index_type;
    H5_iter_order_t order;
    opObjListElement *first;
    opObjListElement *last;
    int insertAsChild;
} opDumpTree;

void concatdim(char *s1, hsize_t next_dim, int index);
void concatdim_native(char *s1, hsize_t next_dim, int index);
void format_dimensions(H5S_class_t space_type, opObjListElement *newElement, hsize_t *size, hsize_t *maxsize, int native);
int  group_check(struct opObjListElement *od, haddr_t target_addr);

#endif