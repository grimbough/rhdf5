#include "h5writeDataFrame.h"

/* creating the compund dataset for a data.frame */
SEXP _h5createDataFrame(SEXP _obj, SEXP _loc_id, SEXP _name, SEXP _level, SEXP _chunk) {
    hid_t loc_id = STRSXP_2_HID( _loc_id );
    const char *name = CHAR(STRING_ELT(_name, 0));
    unsigned int level = INTEGER(_level)[0];
    hsize_t chunk = INTEGER(_chunk)[0];
    
    size_t size = 0;
    size_t strsize[LENGTH(_obj)];
    for (int i=0; i< LENGTH(_obj); i++) {
        if (TYPEOF(VECTOR_ELT(_obj,i)) == INTSXP) {
            size = size + H5Tget_size(H5T_NATIVE_INT32);
        } else if (TYPEOF(VECTOR_ELT(_obj,i)) == REALSXP) {
            size = size + H5Tget_size(H5T_NATIVE_DOUBLE);
        } else if (TYPEOF(VECTOR_ELT(_obj,i)) == STRSXP) {
            strsize[i] = 0;
            size_t s2 = 0;
            for (int j=0; j < LENGTH(VECTOR_ELT(_obj,i)); j++) {
                s2 = LENGTH(STRING_ELT(VECTOR_ELT(_obj,i),j));
                if (s2 > strsize[i]) { strsize[i] = s2; }
            }
            if (strsize[i] == 0) { strsize[i] = 1; }
            size = size + strsize[i];
        }
    }
    hid_t tid = H5Tcreate (H5T_COMPOUND, size);
    hsize_t offset = 0;
    SEXP aa = getAttrib(_obj, mkString("names"));
    for (int i=0; i< LENGTH(_obj); i++) {
        const char *nn = CHAR(STRING_ELT(aa, i));
        if (TYPEOF(VECTOR_ELT(_obj,i)) == INTSXP) {
            H5Tinsert (tid, nn, offset, H5T_NATIVE_INT32);
            offset = offset + H5Tget_size(H5T_NATIVE_INT32);
        } else if (TYPEOF(VECTOR_ELT(_obj,i)) == REALSXP) {
            H5Tinsert (tid, nn, offset, H5T_NATIVE_DOUBLE);
            offset = offset + H5Tget_size(H5T_NATIVE_DOUBLE);
        } else if (TYPEOF(VECTOR_ELT(_obj,i)) == STRSXP) {
            hid_t tid2 = H5Tcopy(H5T_C_S1);
            H5Tset_size(tid2, strsize[i]);
            H5Tinsert (tid, nn, offset, tid2);
            offset = offset + strsize[i];
        }
    }
    hsize_t n = LENGTH(VECTOR_ELT(_obj,0));
    hid_t space = H5Screate_simple (1, &n, &n);
    
    hid_t plist = H5P_DEFAULT;
    if (level > 0) {
        plist = H5Pcreate(H5P_DATASET_CREATE);
        H5Pset_fill_time( plist, H5D_FILL_TIME_ALLOC );
        int rank = 1L;
        hsize_t cdim[rank];
        
        if(chunk > n) {
            chunk = n;
        }
        hsize_t max_chunk_length = floor( UINT_MAX  / offset);
        cdim[0] = max_chunk_length;
        if(chunk < max_chunk_length) {
            cdim[0] = chunk;
        }
        H5Pset_chunk(plist, rank, cdim);
        H5Pset_deflate( plist, level );
    }

    hid_t dset_id = H5Dcreate (loc_id, name,tid, space, H5P_DEFAULT, plist, H5P_DEFAULT);
    
    H5Sclose(space);
    
    addHandle(dset_id);
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(dset_id));
    UNPROTECT(1);
    return Rval;
}


SEXP _h5writeDataFrame(SEXP _obj, SEXP _dset_id) {

    hid_t dset_id = STRSXP_2_HID( _dset_id );
    
    hsize_t n = LENGTH(VECTOR_ELT(_obj,0));
    hid_t space = H5Screate_simple (1, &n, &n);
    
    SEXP aa = getAttrib(_obj, mkString("names"));
    
    size_t strsize[LENGTH(_obj)];
    for (int i=0; i< LENGTH(_obj); i++) {
        if (TYPEOF(VECTOR_ELT(_obj,i)) == STRSXP) {
            strsize[i] = 0;
            size_t s2 = 0;
            for (int j=0; j < LENGTH(VECTOR_ELT(_obj,i)); j++) {
                s2 = LENGTH(STRING_ELT(VECTOR_ELT(_obj,i),j));
                if (s2 > strsize[i]) { strsize[i] = s2; }
            }
            if (strsize[i] == 0) { strsize[i] = 1; }
        }
    }

    for (int i=0; i< LENGTH(_obj); i++) {
        const char *nn = CHAR(STRING_ELT(aa, i));
        if (TYPEOF(VECTOR_ELT(_obj,i)) == INTSXP) {
            hid_t tidn = H5Tcreate (H5T_COMPOUND, H5Tget_size(H5T_NATIVE_INT32));
            H5Tinsert (tidn, nn, 0, H5T_NATIVE_INT32);
            H5Dwrite(dset_id, tidn, space, space, H5P_DEFAULT, INTEGER(VECTOR_ELT(_obj,i)));
            H5Tclose(tidn);
        } else if (TYPEOF(VECTOR_ELT(_obj,i)) == REALSXP) {
            hid_t tidn = H5Tcreate (H5T_COMPOUND, H5Tget_size(H5T_NATIVE_DOUBLE));
            H5Tinsert (tidn, nn, 0, H5T_NATIVE_DOUBLE);
            H5Dwrite(dset_id, tidn, space, space, H5P_DEFAULT, REAL(VECTOR_ELT(_obj,i)));
            H5Tclose(tidn);
        } else if (TYPEOF(VECTOR_ELT(_obj,i)) == STRSXP) {
            hid_t tidn = H5Tcreate(H5T_COMPOUND, strsize[i]);
            hid_t tid2 = H5Tcopy(H5T_C_S1);
            H5Tset_size(tid2, strsize[i]);
            H5Tinsert (tidn, nn, 0, tid2);
            
            char * strbuf = (char *)R_alloc(n,strsize[i]);
            int z=0;
            int j;
            for (int k=0; k < LENGTH(VECTOR_ELT(_obj,i)); k++) {
                for (j=0; (j < LENGTH(STRING_ELT(VECTOR_ELT(_obj,i),k))) & (j < (strsize[i])); j++) {
                    strbuf[z++] = CHAR(STRING_ELT(VECTOR_ELT(_obj,i),k))[j];
                }
                for (; j < strsize[i]; j++) {
                    strbuf[z++] = '\0';
                }
            }
            H5Dwrite(dset_id, tidn, space, space, H5P_DEFAULT, strbuf);
            H5Tclose(tidn);
        }
    }
    
    H5Sclose(space);
    
    SEXP Rval = R_NilValue;
    return Rval;
}

