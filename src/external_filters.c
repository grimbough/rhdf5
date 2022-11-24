#include "external_filters.h"

#ifdef _H5P_filters

/* Bzip2 filter */
SEXP  _H5Pset_bzip2( SEXP _plist_id, SEXP _level ) {
  
  herr_t herr;
  unsigned int cd_values[1];
  
  hid_t plist_id = STRSXP_2_HID( _plist_id );
  cd_values[0] = INTEGER(_level)[0];
  herr = H5Pset_filter (plist_id, H5Z_FILTER_BZIP2, H5Z_FLAG_OPTIONAL, (size_t)1, cd_values);
  SEXP Rval = ScalarInteger(herr);
  
  return Rval;
}


SEXP _H5Pset_blosc( SEXP _plist_id, SEXP _method, SEXP _level, SEXP _shuffle, SEXP _typesize, SEXP _buffersize ) {
    
    herr_t herr;
    unsigned int cd_values[7];
    
    hid_t plist_id = STRSXP_2_HID( _plist_id );
    cd_values[0] = 2; // H5Z_FILTER_BLOSC_VERSION;
    cd_values[1] = 2; // BLOSC_VERSION_FORMAT
    cd_values[2] = INTEGER(_typesize)[0];
    cd_values[3] = INTEGER(_buffersize)[0];
    cd_values[4] = INTEGER(_level)[0]; // compression level 
    cd_values[5] = INTEGER(_shuffle)[0]; // shuffle yes/no 
    cd_values[6] = INTEGER(_method)[0]; // compression algorithm
    herr = H5Pset_filter(plist_id, H5Z_FILTER_BLOSC, H5Z_FLAG_OPTIONAL, (size_t)7, cd_values);
    SEXP Rval = ScalarInteger(herr);
    
    return Rval;
}

SEXP _H5Pset_lzf( SEXP _plist_id, SEXP _buffersize ) {
  
  herr_t herr;
  unsigned int cd_values[3];
  
  hid_t plist_id = STRSXP_2_HID( _plist_id );
  cd_values[0] = 32000; 
  cd_values[1] = 4; 
  cd_values[2] = INTEGER(_buffersize)[0];

  herr = H5Pset_filter(plist_id, H5Z_FILTER_LZF, H5Z_FLAG_OPTIONAL, (size_t)3, cd_values);
  SEXP Rval = ScalarInteger(herr);
  
  return Rval;
}

SEXP _H5Pset_rle( SEXP _plist_id, SEXP _bit32) {
  
  herr_t herr;
  unsigned int cd_values[1];
  
  hid_t plist_id = STRSXP_2_HID( _plist_id );
  cd_values[0] = INTEGER(_bit32)[0];

  herr = H5Pset_filter (plist_id, H5Z_FILTER_RLE, H5Z_FLAG_OPTIONAL, (size_t)1, cd_values);
  SEXP Rval = ScalarInteger(herr);
  
  return Rval;
}

SEXP _H5Pset_rle8( SEXP _plist_id ) {
  
  herr_t herr;
  unsigned int cd_values[1];
  
  hid_t plist_id = STRSXP_2_HID( _plist_id );

  herr = H5Pset_filter (plist_id, H5Z_FILTER_RLE8, H5Z_FLAG_OPTIONAL, (size_t)0, cd_values);
  SEXP Rval = ScalarInteger(herr);
  
  return Rval;
}

SEXP _H5Pset_turborle( SEXP _plist_id ) {
  
  herr_t herr;
  unsigned int cd_values[1];
  
  hid_t plist_id = STRSXP_2_HID( _plist_id );

  herr = H5Pset_filter (plist_id, H5Z_FILTER_TURBORLE, H5Z_FLAG_OPTIONAL, (size_t)1, cd_values);
  SEXP Rval = ScalarInteger(herr);
  
  return Rval;
}


SEXP _H5Pset_lip( SEXP _plist_id, SEXP _nbits) {
  
  herr_t herr;
  unsigned int cd_values[1];
  
  hid_t plist_id = STRSXP_2_HID( _plist_id );
  cd_values[0] = INTEGER(_nbits)[0];
  
  herr = H5Pset_filter(plist_id, H5Z_FILTER_LIP, H5Z_FLAG_OPTIONAL, (size_t)1, cd_values);
  SEXP Rval = ScalarInteger(herr);
  
  return Rval;
}

SEXP _H5Pset_simdcomp( SEXP _plist_id, SEXP _nbits ) {
  
  herr_t herr;
  unsigned int cd_values[1];
  
  hid_t plist_id = STRSXP_2_HID( _plist_id );
  cd_values[0] = INTEGER(_nbits)[0];
  
  herr = H5Pset_filter(plist_id, H5Z_FILTER_SIMDCOMP, H5Z_FLAG_OPTIONAL, (size_t)1, cd_values);
  SEXP Rval = ScalarInteger(herr);
  
  return Rval;
}




#endif
