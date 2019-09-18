#include "external_filters.h"

#ifdef _H5P_filters

SEXP _H5Pset_lzf( SEXP _plist_id ) {
    
    herr_t herr;
    
    //int r = register_lzf();
    hid_t plist_id = INTEGER(_plist_id)[0];
    herr = H5Pset_shuffle(plist_id);
    herr = H5Pset_filter(plist_id, H5PY_FILTER_LZF, H5Z_FLAG_OPTIONAL, 0, NULL);
    SEXP Rval = ScalarInteger(herr);
    
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
    
    return Rval;
}

SEXP _H5Pset_blosc( SEXP _plist_id, SEXP _method, SEXP _level ) {
    
    herr_t herr;
    unsigned int cd_values[7];
    
    hid_t plist_id = STRSXP_2_HID( _plist_id );
    cd_values[4] = INTEGER(_level)[0]; // compression level 
    cd_values[5] = 1; // shuffle yes/no 
    cd_values[6] = INTEGER(_method)[0]; // compression algorithm
    herr = H5Pset_filter(plist_id, H5Z_FILTER_BLOSC, H5Z_FLAG_OPTIONAL, (size_t)7, cd_values);
    SEXP Rval = ScalarInteger(herr);
    
    return Rval;
}

#endif
