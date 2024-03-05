#include "H5S.h"

/* hid_t H5Screate( H5S_class_t type ) */
SEXP _H5Screate( SEXP _type ) {
    H5S_class_t type = INTEGER(_type)[0];
    hid_t hid = H5Screate( type );
    addHandle(hid);
    
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(hid));
    UNPROTECT(1);
    return Rval;
}

/* hid_t H5Scopy( hid_t space_id ) */
SEXP _H5Scopy( SEXP _space_id ) {
    hid_t space_id = STRSXP_2_HID( _space_id );
    hid_t hid = H5Scopy( space_id );
    addHandle(hid);
    
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(hid));
    UNPROTECT(1);
    return Rval;
}

/* herr_t H5Sclose( hid_t space_id ) */
SEXP _H5Sclose( SEXP _space_id ) {
    hid_t space_id = STRSXP_2_HID( _space_id );
    herr_t herr = H5Sclose( space_id );
    if (herr == 0) {
        removeHandle(space_id);
    }
    
    SEXP Rval;
    PROTECT(Rval = allocVector(INTSXP, 1));
    INTEGER(Rval)[0] = herr;
    UNPROTECT(1);
    return Rval;
}

/* hid_t H5Screate_simple( int rank, const hsize_t * dims, const hsize_t * maxdims ) */
SEXP _H5Screate_simple( SEXP _dims, SEXP _maxdims ) {
    hid_t hid;
    int rank = length(_dims);
    hsize_t dims[rank];
    for (int i=0; i<rank; i++) {
        dims[i] = (hsize_t) REAL(_dims)[i];
    }
    if (length(_maxdims) == 0) {
        hid = H5Screate_simple( rank, dims, NULL);
        addHandle(hid);
    } else {
        if (length(_maxdims) != length(_dims)) {
            warning("dims and maxdims must have the same length.\n");
            hid = -1;
        } else {
            hsize_t maxdims[rank];
            double maxdim;
            for (int i=0; i<rank; i++) {
                // We explicitly use -1 to represent unlimited dimensions
                maxdim = REAL(_maxdims)[i];
                maxdims[i] = maxdim < 0 ? H5S_UNLIMITED : (hsize_t) maxdim;
            }
            hid = H5Screate_simple( rank, dims, maxdims);
            addHandle(hid);
        }
    }
    
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(hid));
    UNPROTECT(1);
    return Rval;
}

/* htri_t H5Sis_simple( hid_t space_id ) */
SEXP _H5Sis_simple( SEXP _space_id ) {
    hid_t space_id = STRSXP_2_HID( _space_id );
    htri_t htri = H5Sis_simple( space_id );
    
    SEXP Rval;
    PROTECT(Rval = allocVector(INTSXP, 1));
    INTEGER(Rval)[0] = htri;
    UNPROTECT(1);
    return Rval;
}

/* int H5Sget_simple_extent_dims(hid_t space_id, hsize_t *dims, hsize_t *maxdims ) */
SEXP _H5Sget_simple_extent_dims( SEXP _space_id ) {
    hid_t space_id = STRSXP_2_HID( _space_id );
    hsize_t   size[H5S_MAX_RANK];
    hsize_t   maxsize[H5S_MAX_RANK];
    int rank = H5Sget_simple_extent_dims(space_id, size, maxsize);
    
    SEXP Rval = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(Rval,0,ScalarInteger(rank));
    
    SEXP Rsize;
    SEXP Rmaxsize;
    int size_is_numeric = 0, maxsize_is_numeric = 0;
    if (rank < 0) {
        Rsize = PROTECT(allocVector(INTSXP, 0));
        Rmaxsize = PROTECT(allocVector(INTSXP, 0));
        SET_VECTOR_ELT(Rval,1,Rsize);
        SET_VECTOR_ELT(Rval,2,Rmaxsize);
        UNPROTECT(2);
    } else {
        for (int i=0; i < rank; i++) {
            size_is_numeric += size[i] > R_LEN_T_MAX;
            maxsize_is_numeric += (maxsize[i] > R_LEN_T_MAX) & (maxsize[i] != H5S_UNLIMITED);
        }
        Rsize = PROTECT(allocVector(REALSXP, rank));
        Rmaxsize = PROTECT(allocVector(REALSXP, rank));
        for (int i=0; i < rank; i++) {
            REAL(Rsize)[i] = (double) size[i];
            REAL(Rmaxsize)[i] = (maxsize[i] == H5S_UNLIMITED) ? -1 : (double) maxsize[i];
        }
        SET_VECTOR_ELT(Rval,1, Rsize);
        SET_VECTOR_ELT(Rval,2, Rmaxsize);
        UNPROTECT(2);
    }
    
    if (!size_is_numeric)
        SET_VECTOR_ELT(Rval, 1, AS_INTEGER(VECTOR_ELT(Rval, 1)));
    if (!maxsize_is_numeric)
        SET_VECTOR_ELT(Rval, 2, AS_INTEGER(VECTOR_ELT(Rval, 2)));
    
    SEXP names = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, mkChar("rank"));
    SET_STRING_ELT(names, 1, mkChar("size"));
    SET_STRING_ELT(names, 2, mkChar("maxsize"));
    SET_NAMES(Rval, names);
    UNPROTECT(1);
    UNPROTECT(1);
    return(Rval);
}

/* herr_t H5Sset_extent_simple( hid_t space_id, int rank, const hsize_t *current_size, const hsize_t *maximum_size ) */
SEXP _H5Sset_extent_simple( SEXP _space_id, SEXP _current_size, SEXP _maximum_size ) {
    hid_t space_id = STRSXP_2_HID( _space_id );
    hid_t herr;
    int rank = length(_current_size);
    hsize_t current_size[rank];
    for (int i=0; i<rank; i++) {
        current_size[i] = (hsize_t) REAL(_current_size)[i];
    }
    if (length(_maximum_size) == 0) {
        herr = H5Sset_extent_simple( space_id, rank, current_size, NULL);
    } else {
        if (length(_maximum_size) != length(_current_size)) {
            warning("dims and maxdims must have the same length.\n");
            herr = -1;
        } else {
            hsize_t maximum_size[rank];
            double maxdim;
            for (int i=0; i<rank; i++) {
              // We explicitly use -1 to represent unlimited dimensions
              maxdim = REAL(_maximum_size)[i];
              maximum_size[i] = maxdim < 0 ? H5S_UNLIMITED : (hsize_t) maxdim;
            }
            herr = H5Sset_extent_simple( space_id, rank, current_size, maximum_size);
        }
    }
    
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}


/* hssize_t H5S_GET_SELECT_NPOINTS(hid_t spaceid) */
SEXP _H5Sget_select_npoints( SEXP _space_id ) {
  hid_t space_id = STRSXP_2_HID( _space_id );
  
  hssize_t npoints = H5Sget_select_npoints(space_id);
  SEXP Rval = ScalarReal( (double) npoints);
  return Rval;
}

/* hssize_t H5Sget_select_hyper_nblocks( hid_t space_id ) */
SEXP _H5Sget_select_hyper_nblocks( SEXP _space_id ) {
  hid_t space_id = STRSXP_2_HID( _space_id );
  
  hssize_t nblocks = H5Sget_select_hyper_nblocks(space_id);
  SEXP Rval = ScalarInteger( (int) nblocks);
  return Rval;
}

/* herr_t H5Sget_select_hyper_blocklist(hid_t spaceid, hsize_t startblock, hsize_t numblocks, hsize_t *buf) */
SEXP _H5Sget_select_hyper_blocklist( SEXP _space_id, SEXP _startblock, SEXP _numblocks, SEXP _bufferlength) {
  
  hid_t space_id = STRSXP_2_HID( _space_id );
  hsize_t startblock = (hsize_t) asInteger(_startblock);
  hsize_t numblocks = (hsize_t) asInteger(_numblocks);
  int bufferlength = asInteger(_bufferlength);
  hsize_t *buf = (hsize_t *) R_alloc(bufferlength, sizeof(hsize_t));
  
  herr_t herr = H5Sget_select_hyper_blocklist(space_id, startblock, numblocks, buf);
  if( herr < 0) {
      error("Error selecting blocklist");
  }
  
  SEXP Rval = PROTECT(allocVector(INTSXP, bufferlength));
  for(int i=0; i < bufferlength; i++) {
    /* C to R coordinate conversion applied here */
    INTEGER(Rval)[i] = (int) buf[i] + 1;
  }
  UNPROTECT(1);
  
  return Rval;
}

/* herr_t H5Sselect_none(hid_t spaceid) */
SEXP _H5Sselect_all( SEXP _space_id ) {
  
  hid_t space_id = STRSXP_2_HID( _space_id );
  herr_t herr = H5Sselect_all(space_id);
  
  SEXP Rval = ScalarInteger(herr);
  return Rval;
}

/* herr_t H5Sselect_none(hid_t spaceid) */
SEXP _H5Sselect_none( SEXP _space_id ) {
  
  hid_t space_id = STRSXP_2_HID( _space_id );
  herr_t herr = H5Sselect_none(space_id);
  
  SEXP Rval = ScalarInteger(herr);
  return Rval;
}

/* htri_t H5Sselect_valid(hid_t spaceid) */
SEXP _H5Sselect_valid( SEXP _space_id ) {
  
  hid_t space_id = STRSXP_2_HID( _space_id );
  htri_t htri = H5Sselect_valid(space_id);
  
  SEXP Rval = ScalarInteger(htri);
  return Rval;
}

/* herr_t H5Sselect_hyperslab(hid_t space_id, H5S_seloper_t op, const hsize_t *start, const hsize_t *stride, const hsize_t *count, const hsize_t *block ) */
SEXP _H5Sselect_hyperslab( SEXP _space_id, SEXP _op, SEXP _start, SEXP _stride, SEXP _count, SEXP _block ) {
    hid_t space_id = STRSXP_2_HID( _space_id );
    H5S_seloper_t op =  (H5S_seloper_t) INTEGER(_op)[0];
    hsize_t start[LENGTH(_start)];
    hsize_t stride[LENGTH(_stride)];
    hsize_t count[LENGTH(_count)];
    hsize_t block[LENGTH(_block)];
    int i;
    for (i=0; i < LENGTH(_start); i++) {
        start[i] = REAL(_start)[i];
    }
    for (i=0; i < LENGTH(_stride); i++) {
        stride[i] = REAL(_stride)[i];
    }
    for (i=0; i < LENGTH(_count); i++) {
        count[i] = REAL(_count)[i];
    }
    for (i=0; i < LENGTH(_block); i++) {
        block[i] = REAL(_block)[i];
    }
    
    herr_t herr = H5Sselect_hyperslab( space_id, op, start, stride, count, block );
    if(herr < 0) {
      error("Unable to select hyperslab\n");
    }

    SEXP Rval = ScalarInteger(0);
    return Rval;
    
}

/* hid_t  H5Scombine_hyperslab ( hid_t space_id, H5S_seloper_t op, const hsize_t start[],
 const hsize_t stride[], const hsize_t count[], const hsize_t block[] ) */
SEXP _H5Scombine_hyperslab( SEXP _space_id, SEXP _op, SEXP _start, SEXP _stride, SEXP _count, SEXP _block ) {
  hid_t space_id = STRSXP_2_HID( _space_id );
  H5S_seloper_t op = (H5S_seloper_t) INTEGER(_op)[0];
  
  hsize_t start[LENGTH(_start)];
  hsize_t stride[LENGTH(_stride)];
  hsize_t count[LENGTH(_count)];
  hsize_t block[LENGTH(_block)];
  int i;
  for (i=0; i < LENGTH(_start); i++) {
    start[i] = REAL(_start)[i];
  }
  for (i=0; i < LENGTH(_stride); i++) {
    stride[i] = REAL(_stride)[i];
  }
  for (i=0; i < LENGTH(_count); i++) {
    count[i] = REAL(_count)[i];
  }
  for (i=0; i < LENGTH(_block); i++) {
    block[i] = REAL(_block)[i];
  }
  
  hid_t new_space_id = H5Scombine_hyperslab( space_id, op, start, stride, count, block );
  addHandle(new_space_id);

  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(new_space_id));
  UNPROTECT(1);
  return Rval;
  
}

/* hid_t H5Scombine_select(hid_t space1_id, H5S_seloper_t op, hid_t space2_id); */
SEXP _H5Scombine_select( SEXP _space1_id, SEXP _op, SEXP _space2_id) {
  
  hid_t space1_id = STRSXP_2_HID( _space1_id );
  hid_t space2_id = STRSXP_2_HID( _space2_id );
  H5S_seloper_t op =  INTEGER(_op)[0];
  
  hid_t space_id = H5Scombine_select(space1_id, op, space2_id);
  addHandle( space_id );
  
  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(space_id));
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Sselect_elements(hid_t space_id, H5S_seloper_t op, size_t num_elements, const hsize_t *coord); */
SEXP _H5Sselect_elements( SEXP _space_id, SEXP _op, SEXP _num_elements, SEXP _coord ) {
  
  hid_t space_id = STRSXP_2_HID( _space_id );
  H5S_seloper_t op =  (H5S_seloper_t) asInteger(_op);
  size_t num_elements = asInteger(_num_elements);
  hsize_t *coord = (hsize_t *) R_alloc(LENGTH(_coord), sizeof(hsize_t));
  
  int *_coordp = INTEGER(_coord);
  for(int i = 0; i < LENGTH(_coord); i++) {
    // do the conversion from R to C indices here
    coord[i] = _coordp[i] - 1;
  }
  
  herr_t herr = H5Sselect_elements(space_id, op, num_elements, coord);
  
  SEXP Rval = ScalarInteger(herr);
  return Rval;
}


