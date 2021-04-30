/* This file contains functions relating to selecting hyperslabs using
more familiar R-style indexing.  This can be considered as additions to 
to standard HDF5 API */
#ifndef _H5S_H
#include "H5S.h"
#endif

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
