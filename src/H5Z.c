#include "H5Z.h"

/* htri_t H5Zfilter_avail(H5Z_filter_t filter) */
SEXP _H5Zfilter_avail( SEXP _filter ) {
    H5Z_filter_t filter =  INTEGER( _filter )[0];
    
    htri_t avail = H5Zfilter_avail( filter );
    
    if(avail < 0) {
        error("Unable to check filter availability\n");
    }
    
    SEXP Rval = ScalarLogical(avail);
    return Rval; 
}
