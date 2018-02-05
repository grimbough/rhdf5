#include "external_filters.h"

#ifdef _H5P_filters

SEXP _H5Pset_lzf( SEXP _plist_id ) {
    
    herr_t herr;
    
    //int r = register_lzf();
    hid_t plist_id = INTEGER(_plist_id)[0];
    herr = H5Pset_shuffle(plist_id);
    herr = H5Pset_filter(plist_id, H5PY_FILTER_LZF, H5Z_FLAG_OPTIONAL, 0, NULL);
    SEXP Rval = ScalarInteger(herr);
    
    htri_t avail = H5Zfilter_avail(H5PY_FILTER_LZF);
    unsigned filter_config;
    if (avail) {
        herr = H5Zget_filter_info (H5PY_FILTER_LZF, &filter_config);
        if ( (filter_config & H5Z_FILTER_CONFIG_ENCODE_ENABLED) && 
             (filter_config & H5Z_FILTER_CONFIG_DECODE_ENABLED) ) 
            Rprintf ("lzf filter is available for encoding and decoding.\n");
    }
    
    return Rval;
}
  
     
 /* LZ4 filter */
SEXP _H5Pset_lz4( SEXP _plist_id ) {
    
    herr_t herr;
    const unsigned int cd_values[1] = {9};
    
    hid_t plist_id = INTEGER(_plist_id)[0];
    herr = H5Pset_shuffle(plist_id);
    herr = H5Pset_filter(plist_id, H5Z_FILTER_LZ4, H5Z_FLAG_OPTIONAL, (size_t)1, cd_values);
    SEXP Rval = ScalarInteger(herr);
    
    htri_t avail = H5Zfilter_avail(H5Z_FILTER_LZ4);
    herr_t status;
    unsigned filter_config;
    if (avail) {
        status = H5Zget_filter_info (H5Z_FILTER_LZ4, &filter_config);
        if ( (filter_config & H5Z_FILTER_CONFIG_ENCODE_ENABLED) && 
             (filter_config & H5Z_FILTER_CONFIG_DECODE_ENABLED) ) 
            Rprintf ("lz4 filter is available for encoding and decoding.\n");
    }    
    
    return Rval;
} 

SEXP _H5Pset_blosc( SEXP _plist_id ) {
    
    herr_t herr;
    unsigned int cd_values[6];
    
    hid_t plist_id = INTEGER(_plist_id)[0];
    
    cd_values[4] = 5;  
    cd_values[5] = 1;  
    herr = H5Pset_filter(plist_id, H5Z_FILTER_BLOSC, H5Z_FLAG_OPTIONAL, (size_t)6, cd_values);
    SEXP Rval = ScalarInteger(herr);
    
    htri_t avail = H5Zfilter_avail(H5Z_FILTER_BLOSC);
    unsigned filter_config;
    if (avail) {
        herr = H5Zget_filter_info (H5Z_FILTER_BLOSC, &filter_config);
        if ( (filter_config & H5Z_FILTER_CONFIG_ENCODE_ENABLED) && 
             (filter_config & H5Z_FILTER_CONFIG_DECODE_ENABLED) ) 
            Rprintf ("blosc filter is available for encoding and decoding.\n");
    }
    
    return Rval;
}

#endif
