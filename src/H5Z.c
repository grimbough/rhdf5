#include "H5Z.h"

/* htri_t H5Zfilter_avail(H5Z_filter_t filter) */
SEXP _H5Zfilter_avail( SEXP _filter ) {
    H5Z_filter_t filter =  INTEGER( _filter )[0];
    
    herr_t avail = H5Zfilter_avail( filter );
    
    SEXP Rval = ScalarInteger(avail);
    return Rval; 
}