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
        dims[i] = REAL(_dims)[i];
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
            for (int i=0; i<rank; i++) {
                maxdims[i] = REAL(_maxdims)[i];
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
            maxsize_is_numeric += maxsize[i] > R_LEN_T_MAX;
        }
        Rsize = PROTECT(allocVector(REALSXP, rank));
        Rmaxsize = PROTECT(allocVector(REALSXP, rank));
        for (int i=0; i < rank; i++) {
            REAL(Rsize)[i] = size[i];
            REAL(Rmaxsize)[i] = maxsize[i];
        }
        SET_VECTOR_ELT(Rval,1,Rsize);
        SET_VECTOR_ELT(Rval,2,Rmaxsize);
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
        current_size[i] = REAL(_current_size)[i];
    }
    if (length(_maximum_size) == 0) {
        herr = H5Sset_extent_simple( space_id, rank, current_size, NULL);
    } else {
        if (length(_maximum_size) != length(_current_size)) {
            warning("dims and maxdims must have the same length.\n");
            herr = -1;
        } else {
            hsize_t maximum_size[rank];
            for (int i=0; i<rank; i++) {
                maximum_size[i] = REAL(_maximum_size)[i];
            }
            herr = H5Sset_extent_simple( space_id, rank, current_size, maximum_size);
        }
    }
    
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

/* herr_t H5Sselect_hyperslab(hid_t space_id, H5S_seloper_t op, const hsize_t *start, const hsize_t *stride, const hsize_t *count, const hsize_t *block ) */
SEXP _H5Sselect_hyperslab( SEXP _space_id, SEXP _op, SEXP _start, SEXP _stride, SEXP _count, SEXP _block ) {
    hid_t space_id = STRSXP_2_HID( _space_id );
    H5S_seloper_t op =  INTEGER(_op)[0];
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
    
    SEXP Rval = ScalarInteger(herr);
    return Rval;
    
}

SEXP _H5Sselect_cols( SEXP _space_id, SEXP _start, SEXP _stride, SEXP _count, SEXP _block ) {
  
  hid_t space_id = STRSXP_2_HID( _space_id );
  herr_t herr = H5Sselect_none(space_id);

  hsize_t start[2];
  hsize_t stride[2];
  hsize_t count[2];
  hsize_t block[2];
  
  int i, j;
  for(i = 0; i < LENGTH(_start); i++) {
    
    start[1] = 0;
    stride[1] = 1;
    count[1] = 1;
    block[1] = REAL(_block)[i];
    
    start[0] = REAL(_start)[i];
    stride[0] = REAL(_stride)[i];
    count[0] = REAL(_count)[i];
    block[0] = 1;
    
    herr = H5Sselect_hyperslab(space_id, H5S_SELECT_OR, start, stride, count, block);
  }
  
  SEXP Rval = ScalarInteger(herr);
  return Rval;
}

/* H5Sselect_index is not part of the standart H5S interfaces. It is a iteratie call to H5Sselect_point. */
SEXP _H5Sselect_index( SEXP _space_id, SEXP _start, SEXP _count) {
    hid_t space_id = STRSXP_2_HID( _space_id );
    
    int l = LENGTH(_start);
    
    herr_t herr = H5Sselect_none(space_id);
    hsize_t start[l];
    hsize_t stride[l];
    hsize_t count[l];
    hsize_t block[l];
    int index[l];
    for (int i=0; i<l; i++) {
        stride[i] = 1;
        index[i] = 0;
        block[i] = 1;
    }
    int cont = 1;
    if (herr < 0) {
        cont = 0;
    }
    int k = l-1;
    while(cont > 0) {
        for (int i=0; i<l; i++) {
            start[i] = REAL(VECTOR_ELT(_start,i))[index[i]];
            count[i] = REAL(VECTOR_ELT(_count,i))[index[i]];
        }
        herr = H5Sselect_hyperslab(space_id, H5S_SELECT_OR, start, stride, count, block);
        if (herr < 0) {
            cont = 0;
        } else {
            k = l-1;
            index[k]++;
            int carry = 0;
            if (index[k] >= LENGTH(VECTOR_ELT(_count,k))) {
                carry = 1;
            }
            while ((k >= 0) & (carry > 0)) {
                index[k] = 0;
                k--;
                if (k >= 0) {
                    index[k]++;
                    if (index[k] >= LENGTH(VECTOR_ELT(_count,k))) {
                        carry = 1;
                    } else {
                        carry = 0;
                    }
                }
            }
            if (k < 0) {
                cont = 0;
            }
        }
    }
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

