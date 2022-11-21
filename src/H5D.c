#include "H5D.h"
#include <stdlib.h>
#include <time.h>

void permute_setup(hid_t dim_space_id, int *rank_p, hsize_t **dims_p,
                   int **iip_p, int **stride_p) {
    int rank = H5Sget_simple_extent_ndims(dim_space_id);
    hsize_t *dims = (hsize_t *) R_alloc(rank, sizeof(hsize_t));
    int *iip = (int *) R_alloc(rank, sizeof(int));
    int *stride = (int *) R_alloc(rank, sizeof(int));
    H5Sget_simple_extent_dims(dim_space_id, dims, NULL);
    
    for (int i = 0; i < rank; i++) {
        if (i == 0)
            iip[i] = 1;
        else
            iip[i] = iip[i-1] * dims[rank-i];
    }
    
    for (int i = 0; i < rank; i++)
        stride[i] = iip[rank-i-1];
    
    for (int i = 0; i < rank; iip[i++] = 0);
    
    *rank_p = rank;
    *dims_p = dims;
    *iip_p = iip;
    *stride_p = stride;
}

#define CLICKJ                                              \
int itmp;                                                   \
for (itmp = 0; itmp < rank; itmp++) {                       \
    if (iip[itmp] == dims[itmp] - 1)                        \
        iip[itmp] = 0;                                      \
    else {                                                  \
        iip[itmp]++;                                        \
        break;                                              \
    }                                                       \
}                                                           \
for (lj = 0, itmp = 0; itmp < rank; itmp++)                 \
    lj += iip[itmp] * stride[itmp];

#define PERMUTE(FROM, ACCESSOR, DIM_SPACE_ID) do {                \
SEXP to = PROTECT(allocVector(TYPEOF(FROM), LENGTH(FROM)));       \
int rank, *iip, *stride;                                          \
hsize_t *dims;                                                    \
permute_setup(DIM_SPACE_ID, &rank, &dims, &iip, &stride);         \
for (int li = 0, lj = 0; li < LENGTH(FROM); li++) {               \
    ACCESSOR(to)[li] = ACCESSOR(FROM)[lj];                        \
    CLICKJ;                                                       \
}                                                                 \
FROM = to;                                                        \
} while(0)

/* hid_t H5Dcreate( hid_t loc_id, const char *name, hid_t dtype_id, hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id ) */
SEXP _H5Dcreate( SEXP _loc_id, SEXP _name, SEXP _dtype_id, SEXP _space_id, SEXP _lcpl_id, SEXP _dcpl_id, SEXP _dapl_id ) {

    hid_t loc_id = STRSXP_2_HID( _loc_id );
    const char *name = CHAR(STRING_ELT(_name, 0));
    hid_t dtype_id = STRSXP_2_HID( _dtype_id );
    hid_t space_id = STRSXP_2_HID( _space_id );
    
    hid_t lcpl_id = H5P_DEFAULT;
    hid_t dcpl_id = H5P_DEFAULT;
    hid_t dapl_id = H5P_DEFAULT;
    if (length(_lcpl_id) > 0) { lcpl_id = STRSXP_2_HID(_lcpl_id); }
    if (length(_dcpl_id) > 0) { dcpl_id = STRSXP_2_HID(_dcpl_id); }
    if (length(_dapl_id) > 0) { dapl_id = STRSXP_2_HID(_dapl_id); }
    
    hid_t hid = H5Dcreate( loc_id, name, dtype_id, space_id, 
                           lcpl_id, dcpl_id, dapl_id );
    
    addHandle(hid);
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(hid));
    UNPROTECT(1);
    return Rval;
}

/* hid_t H5Dopen( hid_t loc_id, const char *name, hid_t dapl_id ) */
SEXP _H5Dopen( SEXP _loc_id, SEXP _name, SEXP _dapl_id ) {
    hid_t loc_id = STRSXP_2_HID( _loc_id );
    const char *name = CHAR(STRING_ELT(_name, 0));
    hid_t dapl_id = H5P_DEFAULT;
    if (length(_dapl_id) > 0) { dapl_id = STRSXP_2_HID(_dapl_id); }
    hid_t hid = H5Dopen( loc_id, name, dapl_id );
    addHandle(hid);
    
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(hid));
    UNPROTECT(1);
    return Rval;
}

/* herr_t H5Dclose(hid_t dataset_id ) */
SEXP _H5Dclose( SEXP _dataset_id ) {
    //hid_t dataset_id = INTEGER(_dataset_id)[0];
    hid_t dataset_id = STRSXP_2_HID( _dataset_id );
    herr_t herr = H5Dclose( dataset_id );
    if (herr == 0) {
        removeHandle(dataset_id);
    }
    
    SEXP Rval;
    PROTECT(Rval = allocVector(INTSXP, 1));
    INTEGER(Rval)[0] = herr;
    UNPROTECT(1);
    return Rval;
}

/* hsize_t H5Dget_storage_size( hid_t dataset_id ) */
SEXP _H5Dget_storage_size( SEXP _dataset_id ) {

    hid_t dataset_id = STRSXP_2_HID( _dataset_id );
    hsize_t size = H5Dget_storage_size( dataset_id );

    SEXP Rval = ScalarInteger(0);
    if (size <= INT_MAX) {
        Rval = ScalarInteger(size);
    } else {
        double dsize = size;
        Rval = ScalarReal(dsize);
    }
    return Rval;
}

SEXP H5Dread_helper_INTEGER(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf, 
                            hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame,
                            int bit64conversion, int native ) {
    hid_t mem_type_id = -1;
    herr_t herr = 0;
    SEXP Rval;
    
    int b = H5Tget_size(dtype_id);
    H5T_sign_t sgn = H5Tget_sign(dtype_id);

    int warn = 0;
    int warn_overflow_64bit = 0;
    int warn_double = 0;
    
    /* 1-byte integers. Reading strategy is dependent on whether these 
     * are signed or unsigned (RAWSXP vs INTSXP) */
    if(b == 1) {
      void * buf;

      if(sgn == H5T_SGN_NONE) {
        if (cpdType < 0) {
          mem_type_id = H5T_NATIVE_UCHAR;
        } else {
          mem_type_id = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_UCHAR));
          herr = H5Tinsert(mem_type_id, cpdField[0], 0, H5T_NATIVE_UCHAR);
          for (int i=1; i<cpdNField; i++) {
            hid_t mem_type_id2 = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_UCHAR));
            herr = H5Tinsert(mem_type_id2, cpdField[i], 0, mem_type_id);
            mem_type_id = mem_type_id2;
          }
        }
        
        if (length(_buf) == 0) {
          Rval = PROTECT(allocVector(RAWSXP, n));
          buf = RAW(Rval);
        } else {
          buf = RAW(_buf);
          Rval = _buf;
        }
        herr = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, H5P_DEFAULT, buf );
        if(herr < 0) {
          error("Error reading dataset");
        }
        
        if (native)
          PERMUTE(Rval, RAW, mem_space_id);
        
      } else {
        
        if (cpdType < 0) {
          mem_type_id = H5T_NATIVE_INT32;
        } else {
          mem_type_id = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_INT32));
          herr = H5Tinsert(mem_type_id, cpdField[0], 0, H5T_NATIVE_INT32);
          for (int i=1; i<cpdNField; i++) {
            hid_t mem_type_id2 = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_INT32));
            herr = H5Tinsert(mem_type_id2, cpdField[i], 0, mem_type_id);
            mem_type_id = mem_type_id2;
          }
        }
        if (length(_buf) == 0) {
          Rval = PROTECT(allocVector(INTSXP, n));
          buf = INTEGER(Rval);
        } else {
          buf = INTEGER(_buf);
          Rval = _buf;
        }
        herr = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, H5P_DEFAULT, buf );
        if(herr < 0) {
          error("Error reading dataset");
        }
        
        if (native)
          PERMUTE(Rval, INTEGER, mem_space_id);
        
      }
      if (length(_buf) == 0) {
        setAttrib(Rval, R_DimSymbol, Rdim);
      }
      
    } else {
    if ( ((b >= 2) & (b < 4)) | ((b == 4) & (sgn == H5T_SGN_2))) {   // Read directly to R-integer without loss of data (short or signed int)
        if (cpdType < 0) {
            mem_type_id = H5T_NATIVE_INT32;
        } else {
            mem_type_id = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_INT32));
            herr = H5Tinsert(mem_type_id, cpdField[0], 0, H5T_NATIVE_INT32);
            for (int i=1; i<cpdNField; i++) {
                hid_t mem_type_id2 = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_INT32));
                herr = H5Tinsert(mem_type_id2, cpdField[i], 0, mem_type_id);
                mem_type_id = mem_type_id2;
            }
        }
        void * buf;
        if (length(_buf) == 0) {
            Rval = PROTECT(allocVector(INTSXP, n));
            buf = INTEGER(Rval);
        } else {
            buf = INTEGER(_buf);
            Rval = _buf;
        }
        herr_t herr = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, H5P_DEFAULT, buf );
        if(herr < 0) {
          error("Error reading dataset");
        }
        
        if (native)
            PERMUTE(Rval, INTEGER, mem_space_id);
        
        if (length(_buf) == 0) {
            setAttrib(Rval, R_DimSymbol, Rdim);
        }
    } else { 
        hid_t dtypeNative;
        void* intbuf;
        if ((b < 4) | ((b == 4) & (sgn == H5T_SGN_2))) {
            dtypeNative = H5T_NATIVE_INT;
            intbuf = R_alloc(n, sizeof(int));
        } else if ((b == 4) & (sgn == H5T_SGN_NONE)) {
            dtypeNative = H5T_NATIVE_UINT;
            intbuf = R_alloc(n, sizeof(unsigned int));
        } else if ((b == 8) & (sgn == H5T_SGN_2)) {
            dtypeNative = H5T_NATIVE_INT64;
            intbuf = R_alloc(n, sizeof(long long));
        } else if ((b == 8) & (sgn == H5T_SGN_NONE)) {
            dtypeNative = H5T_NATIVE_UINT64;
            intbuf = R_alloc(n, sizeof(unsigned long long));
        } else {
            error("Unkown data type.  Aborting.\n");
        }
        if (intbuf == 0) {
            error("Not enough memory to read data! Try to read a subset of data by specifying the index or count parameter.");
        }
        if (cpdType < 0) {
            mem_type_id = dtypeNative;
        } else {
            mem_type_id = H5Tcreate(H5T_COMPOUND, H5Tget_size(dtypeNative));
            herr = H5Tinsert(mem_type_id, cpdField[0], 0, dtypeNative);
            for (int i=1; i<cpdNField; i++) {
                hid_t mem_type_id2 = H5Tcreate(H5T_COMPOUND, H5Tget_size(dtypeNative));
                herr = H5Tinsert(mem_type_id2, cpdField[i], 0, mem_type_id);
                mem_type_id = mem_type_id2;
            }
        }
        
        herr_t herr = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, H5P_DEFAULT, intbuf );
        if(herr < 0) {
          error("Error reading dataset");
        }
        
        if (bit64conversion == 0) {  // Convert data to R-integer and replace overflow values with NA_integer
            void * buf;
            if (length(_buf) == 0) {
                Rval = PROTECT(allocVector(INTSXP, n));
                buf = (int *) INTEGER(Rval);
            } else {
                buf = INTEGER(_buf);
                Rval = _buf;
            }
            if ((b == 4) & (sgn == H5T_SGN_NONE)) {
                uint32_to_int32(intbuf, n, buf);
            } else if (b == 8) { 
                int64_to_int32(intbuf, n, buf, sgn);
            }
        } else {
            void * buf;
            if (length(_buf) == 0) {
                Rval = PROTECT(allocVector(REALSXP, n));
                buf = (long long *) REAL(Rval);
            } else {
                buf = REAL(_buf);
                Rval = _buf;
            }
            if (bit64conversion == 1) {  //convert to double
                long long i;
                if ((b < 4) | ((b == 4) & (sgn == H5T_SGN_2))) {
                    for (i=0; i<n; i++){
                        ((double *)buf)[i] = ((int *)intbuf)[i];
                    }
                } else if ((b == 4) & (sgn == H5T_SGN_NONE)) {
                    uint32_to_double(intbuf, n, buf);
                } else if (b == 8) {
                    int64_to_double(intbuf, n, buf, sgn);
                }
            } else { // convert to integer64 class
                long long i;
                if ((b < 4) | ((b == 4) & (sgn == H5T_SGN_2))) {
                    for (i=0; i<n; i++){
                        ((long long *)buf)[i] = ((int *)intbuf)[i];
                    }
                } else if ((b == 4) & (sgn == H5T_SGN_NONE)) {
                    uint32_to_integer64(intbuf, n, buf);
                } else if (b == 8) {
                    int64_to_integer64(intbuf, n, buf, sgn);
                }
                if (native)
                    PERMUTE(Rval, INTEGER, mem_space_id);
                SEXP la = PROTECT(mkString("integer64"));
                setAttrib(Rval, R_ClassSymbol, la);
                UNPROTECT(1);
            }
        }
        if (length(_buf) == 0) {
            setAttrib(Rval, R_DimSymbol, Rdim);
        }
    }
    }
    
    if (warn > 0) {
        warning("NAs produced by integer overflow while converting 64-bit integer or unsigned 32-bit integer from HDF5 to a 32-bit integer in R.\nChoose bit64conversion='bit64' or bit64conversion='double' to avoid data loss and see the vignette 'rhdf5' for more details about 64-bit integers.");
    } else if (warn_overflow_64bit > 0) {
        warning("NAs produced by integer overflow while converting unsigned 64-bit integer from HDF5 to signed 64-bit integer in R.");
    } else if (warn_double > 0) {
        warning("integer precision lost while converting 64-bit integer or unsigned 32-bit integer from HDF5 to double in R.\nChoose bit64conversion='bit64' to avoid data loss and see the vignette 'rhdf5' for more details about 64-bit integers.");
    }
    
    UNPROTECT( (length(_buf) == 0) + native );
    return(Rval);
}


SEXP H5Dread_helper_FLOAT(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf, 
                          hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame, int native ) {
    hid_t mem_type_id = -1;
    herr_t herr = 0;
    
    SEXP Rval;
    if (cpdType < 0) {
        mem_type_id = H5T_NATIVE_DOUBLE;
    } else {
        mem_type_id = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_DOUBLE));
        herr = H5Tinsert(mem_type_id, cpdField[0], 0, H5T_NATIVE_DOUBLE);
        for (int i=1; i<cpdNField; i++) {
            hid_t mem_type_id2 = H5Tcreate(H5T_COMPOUND, H5Tget_size(H5T_NATIVE_DOUBLE));
            herr = H5Tinsert(mem_type_id2, cpdField[i], 0, mem_type_id);
            mem_type_id = mem_type_id2;
        }
    }
    void * buf;
    if (length(_buf) == 0) {
        Rval = PROTECT(allocVector(REALSXP, n));
        buf = REAL(Rval);
    } else {
        buf = REAL(_buf);
        Rval = _buf;
    }
    
    herr = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, H5P_DEFAULT, buf );
    if(herr < 0) {
      error("Unable to read dataset");
    }
    
    if (native)
      PERMUTE(Rval, REAL, mem_space_id);
    
    if (length(_buf) == 0) {
      setAttrib(Rval, R_DimSymbol, Rdim);
    }
    
    UNPROTECT( (length(_buf) == 0) + native );
    return(Rval);
}

SEXP H5Dread_helper_STRING(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf, 
                           hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame, int native ) {
    hid_t mem_type_id = -1;
    herr_t herr;
    SEXP Rval;
    
    size_t size = H5Tget_size(dtype_id);
    if (cpdType < 0) {
        mem_type_id = dtype_id;
    } else {
        mem_type_id = H5Tcreate(H5T_COMPOUND, size);
        herr = H5Tinsert(mem_type_id, cpdField[0], 0, dtype_id);
        for (int i=1; i<cpdNField; i++) {
            hid_t mem_type_id2 = H5Tcreate(H5T_COMPOUND, size);
            herr = H5Tinsert(mem_type_id2, cpdField[i], 0, mem_type_id);
            mem_type_id = mem_type_id2;
        }
    }
    Rval = PROTECT(allocVector(STRSXP, n));

    if(n > 0) { /* return empty vector if length == 0 */
      if (H5Tis_variable_str(dtype_id)) {
          char **bufSTR = (char **) R_alloc(n, sizeof(char *));
          herr = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, H5P_DEFAULT, bufSTR );
          if(herr < 0) {
            error("Unable to read dataset");
          }
          for (int i=0; i<n; i++) {
              SET_STRING_ELT(Rval, i, mkChar(bufSTR[i]));
          }
          herr = H5Dvlen_reclaim(mem_type_id, file_space_id, H5P_DEFAULT, bufSTR);
          if(herr < 0) {
              error("Unable to reclaim variable length buffer\n");
          }
      } else {
          void* bufSTR = R_alloc(n * size, sizeof(char));
          if (bufSTR == 0) {
              error("Not enough memory to read data! Try to read a subset of data by specifying the index or count parameter.");
          }
          herr = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, H5P_DEFAULT, bufSTR );
          if(herr < 0) { 
            error("Unable to read dataset");
          }
          char* bufSTR2 = R_alloc(size + 1, sizeof(char));
          if (bufSTR2 == 0) {
              error("Not enough memory to read data! Try to read a subset of data by specifying the index or count parameter.");
          }
          bufSTR2[size] = '\0';
          char* bufSTR3 = ((char* )bufSTR);
          for (int i=0; i<n; i++) {
              for (int j=0; j<size; j++) {
                  bufSTR2[j] = bufSTR3[i*sizeof(char)*size+j];
              }
              SET_STRING_ELT(Rval, i, mkChar(bufSTR2));
          }
      }
      if (native)
          PERMUTE(Rval, STRING_PTR, mem_space_id);
      setAttrib(Rval, R_DimSymbol, Rdim);
    }
    UNPROTECT( 1 + native );
    return(Rval);
}

SEXP H5Dread_helper_ENUM(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf, 
                         hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame, int native ) {
    hid_t mem_type_id = -1;
    herr_t herr = 0;
    SEXP Rval;
    
    hid_t superclass =  H5Tget_class(H5Tget_super( dtype_id ));
    if (superclass == H5T_INTEGER) {
        hid_t enumtype = H5Tenum_create(H5T_NATIVE_INT);
        int nmembers = H5Tget_nmembers( dtype_id );
        SEXP levels = PROTECT(allocVector(STRSXP, nmembers));
        for (int i=0; i<nmembers; i++) {
            char * st = H5Tget_member_name( dtype_id, i );
            SET_STRING_ELT(levels, i, mkChar(st));
            herr = H5Tenum_insert (enumtype, st, &i);
        }
        
        if (cpdType < 0) {
            mem_type_id = enumtype;
        } else {
            mem_type_id = H5Tcreate(H5T_COMPOUND, H5Tget_size(enumtype));
            herr = H5Tinsert(mem_type_id, cpdField[0], 0, enumtype);
            for (int i=1; i<cpdNField; i++) {
                hid_t mem_type_id2 = H5Tcreate(H5T_COMPOUND, H5Tget_size(enumtype));
                herr = H5Tinsert(mem_type_id2, cpdField[i], 0, mem_type_id);
                mem_type_id = mem_type_id2;
            }
        }
        
        
        void * buf;
        if (length(_buf) == 0) {
            Rval = PROTECT(allocVector(INTSXP, n));
            buf = INTEGER(Rval);
        } else {
            buf = INTEGER(_buf);
            Rval = _buf;
        }
        
        herr = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, H5P_DEFAULT, buf );
        if(herr < 0) {
          error("Unable to read dataset");
        }
        
        if (native)
            PERMUTE(Rval, INTEGER, mem_space_id);
        if (length(_buf) == 0) {
            if (native) {
                for (int i=0; i < n; i++)
                    INTEGER(Rval)[i] += 1;
            } else {
                for (int i=0; i < n; i++)
                    ((int *)buf)[i] += 1;
            }
            setAttrib(Rval, R_DimSymbol, Rdim);
            setAttrib(Rval, mkString("levels"), levels);
            setAttrib(Rval, R_ClassSymbol, mkString("factor"));
        }
        UNPROTECT(native + (length(_buf) == 0) + 1);
    } else {
        double na = R_NaReal;
        Rval = PROTECT(allocVector(REALSXP, n));
        for (int i=0; i<n; i++) { REAL(Rval)[i] = na; }
        setAttrib(Rval, R_DimSymbol, Rdim);
        UNPROTECT(1);
        char str[256];
        sprintf(str, "h5read for type ENUM [%s] not yet implemented. Values replaced by NA's.", getDatatypeClass(H5Tget_super( dtype_id )));
        warning(str);
    }
    
    return(Rval);
    
}

SEXP H5Dread_helper_ARRAY(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf,
                          hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame, int native ) {
    hid_t mem_type_id = -1;
    herr_t herr = 0;
    SEXP Rval;
    
    hid_t superclass =  H5Tget_class(H5Tget_super( dtype_id ));
    if (((superclass == H5T_INTEGER) | (superclass == H5T_FLOAT)) & (!((cpdNField > 0) & (compoundAsDataFrame > 0)))) {
        int ndims = H5Tget_array_ndims (dtype_id);
        hsize_t na = 1;
        hsize_t adims[ndims];
        H5Tget_array_dims( dtype_id, adims );
        for (int i=0; i < ndims; i++) {
            na = na * adims[i];
        }
        hid_t arraytype;
        if (superclass == H5T_INTEGER) {
            arraytype = H5Tarray_create (H5T_NATIVE_INT, ndims, adims);
        } else if (superclass == H5T_FLOAT) {
            arraytype = H5Tarray_create (H5T_NATIVE_DOUBLE, ndims, adims);
        } else {
          error("Unable to create array type\n");
        }
        if (cpdType < 0) {
            mem_type_id =  arraytype;
        } else {
            mem_type_id = H5Tcreate(H5T_COMPOUND, H5Tget_size(arraytype));
            herr = H5Tinsert(mem_type_id, cpdField[0], 0, arraytype);
            for (int i=1; i<cpdNField; i++) {
                hid_t mem_type_id2 = H5Tcreate(H5T_COMPOUND, H5Tget_size(arraytype));
                herr = H5Tinsert(mem_type_id2, cpdField[i], 0, mem_type_id);
                mem_type_id = mem_type_id2;
            }
        }
        void * buf;
        if (length(_buf) == 0) {
            if (superclass == H5T_INTEGER) {
                Rval = PROTECT(allocVector(INTSXP, n*na));
                buf = INTEGER(Rval);
            } else if (superclass == H5T_FLOAT) {
                Rval = PROTECT(allocVector(REALSXP, n*na));
                buf = REAL(Rval);
            } else {
              error("Unknown superclass\n");
            }
        } else {
            if (superclass == H5T_INTEGER) {
                buf = INTEGER(_buf);
            } else if (superclass == H5T_FLOAT) {
                buf = REAL(_buf);
            } else {
              error("Unknown superclass\n");
            }
            Rval = _buf;
        }
        
        herr = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, H5P_DEFAULT, buf );
        if(herr < 0) {
          error("Unable to read dataset");
        }
        
        if (native) {
            int rank0 = H5Sget_simple_extent_ndims(mem_space_id);
            hsize_t* dims0 = (hsize_t *) R_alloc(rank0, sizeof(hsize_t));
            H5Sget_simple_extent_dims(mem_space_id, dims0, NULL);
            
            int rank = rank0 + ndims;
            hsize_t *dims = (hsize_t *) R_alloc(rank, sizeof(hsize_t));
            int *iip = (int *) R_alloc(rank, sizeof(int));
            int *stride = (int *) R_alloc(rank, sizeof(int));
            
            for (int i = 0; i < rank0; i++)
                dims[i] = dims0[rank0-i-1];
            for (int i = rank0; i < rank; i++)
                dims[i] = adims[i-rank0];
            
            for (int i = 0; i < rank; i++) {
                if (i == 0)
                    iip[0] = 1;
                else
                    iip[i] = iip[i-1] * dims[rank-i];
            }
            
            for (int i = 0; i < rank; i++)
                stride[i] = iip[rank-i-1];
            
            for (int i = 0; i < rank; iip[i++] = 0);
            
            SEXP buffer = PROTECT(allocVector(TYPEOF(Rval), LENGTH(Rval)));
            for (int li = 0, lj = 0; li < LENGTH(Rval); li++) {
                INTEGER(buffer)[li] = INTEGER(Rval)[lj];
                CLICKJ;
            }
            Rval = buffer;
        }
        
        if (length(_buf) == 0) {
            SEXP Rdima = PROTECT(allocVector(INTSXP, LENGTH(Rdim)+ndims));
            int i=0,j=0;
            if (native) {
                for (j=LENGTH(Rdim)-1; j>=0; j--,i++) {
                    INTEGER(Rdima)[i] = INTEGER(Rdim)[j];
                }
                for (j=0; j<ndims; j++,i++) {
                    INTEGER(Rdima)[i] = adims[j];
                }
            }
            else {
                for (j=ndims-1; j>=0; j--,i++) {
                    INTEGER(Rdima)[i] = adims[j];
                }
                for (j=0; j<LENGTH(Rdim); j++,i++) {
                    INTEGER(Rdima)[i] = INTEGER(Rdim)[j];
                }
            }
            setAttrib(Rval, R_DimSymbol, Rdima);
            UNPROTECT(2 + (native > 0));
        }
    } else {
        double na = R_NaReal;
        Rval = PROTECT(allocVector(REALSXP, n));
        for (int i=0; i<n; i++) { REAL(Rval)[i] = na; }
        setAttrib(Rval, R_DimSymbol, Rdim);
        UNPROTECT(1);
        if ((cpdNField > 0) & (compoundAsDataFrame > 0)) {
            warning("h5read cannot coerce COMPOUND dataset with element of type ARRAY to data.frame. Values replaced by NA's. Try h5read with argument compoundAsDataFrame=FALSE to read element of type ARRAY.");
        } else {
            char str[256];
            sprintf(str, "h5read for type ARRAY [%s] not yet implemented. Values replaced by NA's.", getDatatypeClass(H5Tget_super( dtype_id )));
            warning(str);
        }
    }
    
    return(Rval);
    
}

SEXP H5Dread_helper_COMPOUND(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf, 
                             hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame,
                             int bit64conversion, int native ) {

    if ((LENGTH(Rdim) > 1) && compoundAsDataFrame) {
        compoundAsDataFrame = 0;
        warning("Cannot coerce multi-dimensional data to data.frame. Data returned as a list.");
    }
    
    SEXP Rval;
    if (cpdType < 0) {
        int N = H5Tget_nmembers(dtype_id);
        PROTECT(Rval = allocVector(VECSXP, N));
        SEXP names = PROTECT(allocVector(STRSXP, N));
        for (int i=0; i<N; i++) {
            SET_STRING_ELT(names, i, mkChar(H5Tget_member_name(dtype_id,i)));
            char* name[1];
            name[0] = H5Tget_member_name(dtype_id,i);
            SEXP col;
            if (compoundAsDataFrame && (H5Tget_member_class(dtype_id,i) == H5T_COMPOUND)) {
                warning("Cannot read hierarchical compound data types as data.frame. Use 'compoundAsDataFrame=FALSE' instead. Values replaced by NA's.");
                double na = R_NaReal;
                col = PROTECT(allocVector(REALSXP, n));
                for (int i=0; i<n; i++) { REAL(col)[i] = na; }
                setAttrib(col, R_DimSymbol, Rdim);
                UNPROTECT(1);
            } else {
                col = H5Dread_helper(dataset_id, file_space_id, mem_space_id, n, Rdim, _buf,
                                     H5Tget_member_type(dtype_id,i), 1, name, compoundAsDataFrame,
                                     bit64conversion, 0);
            }
            SET_VECTOR_ELT(Rval, i, col);
        }
        SET_NAMES(Rval, names);
        if (compoundAsDataFrame) {
            SEXP rn = PROTECT(allocVector(INTSXP, INTEGER(Rdim)[0]));
            for (int i=0; i<INTEGER(Rdim)[0]; i++) { INTEGER(rn)[i] = i+1; }
            UNPROTECT(1);
            setAttrib(Rval, mkString("row.names"), rn);
            setAttrib(Rval, R_ClassSymbol, mkString("data.frame"));
        }
        UNPROTECT(2);
    } else {
        int N = H5Tget_nmembers(dtype_id);
        PROTECT(Rval = allocVector(VECSXP, N));
        SEXP names = PROTECT(allocVector(STRSXP, N));
        for (int i=0; i<N; i++) {
            SET_STRING_ELT(names, i, mkChar(H5Tget_member_name(dtype_id,i)));
            char* name[cpdNField+1];
            name[0] = H5Tget_member_name(dtype_id,i);
            for (int j=0; j<cpdNField; j++) {
                name[j+1] = cpdField[j];
            }
            SEXP col = H5Dread_helper(dataset_id, file_space_id, mem_space_id, n, Rdim, _buf,
                                      H5Tget_member_type(dtype_id,i), cpdNField+1, name, compoundAsDataFrame,
                                      bit64conversion, 0);
            
            SET_VECTOR_ELT(Rval, i, col);
        }
        SET_NAMES(Rval, names);
        UNPROTECT(2);
    }
    return(Rval);
}


//SEXP H5Dread_helper_REFERENCE(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, hid_t dtype_id) {
SEXP H5Dread_helper_REFERENCE(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf,
                          hid_t dtype_id, int native) {
  
  void *references;
  SEXP Rrefs, Rtype, Rval; 
  
  if(H5Tequal(dtype_id, H5T_STD_REF_OBJ)) {
    Rrefs = PROTECT(allocVector(RAWSXP, sizeof(hobj_ref_t) * n ));
    Rtype = PROTECT(ScalarInteger(H5R_OBJECT));
  } else if (H5Tequal(dtype_id, H5T_STD_REF_DSETREG)) {
    Rrefs = PROTECT(allocVector(RAWSXP, sizeof(hdset_reg_ref_t) * n ));
    Rtype = PROTECT(ScalarInteger(H5R_DATASET_REGION));
  } else {
    error("Unkown reference type");
    return R_NilValue;
  }
  
  references = RAW(Rrefs);
  
  herr_t err = H5Dread(dataset_id, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, references);
  if (err < 0) {
    error("could not read dataset");
    return R_NilValue;
  }

  Rval = PROTECT(R_do_new_object(R_getClassDef("H5Ref")));
  R_do_slot_assign(Rval, mkString("val"), Rrefs);
  R_do_slot_assign(Rval, mkString("type"), Rtype);
  UNPROTECT(3);
  return Rval;
}


SEXP H5Dread_helper(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim,
                    SEXP _buf, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame,
                    int bit64conversion, int native ) {
    
    herr_t herr = 0;
    hid_t dtype_id;
    if (cpdType >= 0) {
        dtype_id = cpdType;
    } else {
        dtype_id = H5Dget_type(dataset_id);
    }
    hid_t dtypeclass_id = H5Tget_class(dtype_id);
    
    SEXP Rval;
    switch(dtypeclass_id) {
    case H5T_INTEGER: {
        Rval = H5Dread_helper_INTEGER(dataset_id, file_space_id, mem_space_id, n, Rdim, _buf, 
                                      dtype_id, cpdType, cpdNField, cpdField, compoundAsDataFrame,
                                      bit64conversion, native );
    } break;
    case H5T_FLOAT: {
        Rval = H5Dread_helper_FLOAT(dataset_id, file_space_id, mem_space_id, n, Rdim, _buf, 
                                    dtype_id, cpdType, cpdNField, cpdField, compoundAsDataFrame, native );
    } break;
    case H5T_STRING: {
        Rval = H5Dread_helper_STRING(dataset_id, file_space_id, mem_space_id, n, Rdim, _buf, 
                                     dtype_id, cpdType, cpdNField, cpdField, compoundAsDataFrame, native );
    } break;
    case H5T_COMPOUND: {
        Rval = H5Dread_helper_COMPOUND(dataset_id, file_space_id, mem_space_id, n, Rdim, _buf, 
                                       dtype_id, cpdType, cpdNField, cpdField, compoundAsDataFrame,
                                       bit64conversion, native );
    } break;
    case H5T_ENUM: {
        Rval = H5Dread_helper_ENUM(dataset_id, file_space_id, mem_space_id, n, Rdim, _buf,
                                   dtype_id, cpdType, cpdNField, cpdField, compoundAsDataFrame, native );
    } break;
    case H5T_ARRAY: {
        Rval = H5Dread_helper_ARRAY(dataset_id, file_space_id, mem_space_id, n, Rdim, _buf,
                                    dtype_id, cpdType, cpdNField, cpdField, compoundAsDataFrame, native );
    } break;
    case H5T_TIME:
    case H5T_BITFIELD:
    case H5T_OPAQUE:
    case H5T_REFERENCE: {
        Rval = H5Dread_helper_REFERENCE(dataset_id, file_space_id, mem_space_id, n, Rdim, _buf,
                                        dtype_id, native );
    } break;
    case H5T_VLEN:
    default: {
        double na = R_NaReal;
        Rval = PROTECT(allocVector(REALSXP, n));
        for (int i=0; i<n; i++) { REAL(Rval)[i] = na; }
        setAttrib(Rval, R_DimSymbol, Rdim);
        UNPROTECT(1);
        char str[256];
        sprintf(str, "h5read for type '%s' not yet implemented. Values replaced by NA's.", getDatatypeClass(dtype_id));
        warning(str);
    } break;
    }
    
    herr = H5Tclose(dtype_id);
    if(herr < 0) {
      error("Error closing data type\n");
    }
    
    return(Rval);
}

/* herr_t H5Dread(hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t xfer_plist_id, void * buf ) */
/* TODO: accept mem_type_id as parameter */
SEXP _H5Dread( SEXP _dataset_id, SEXP _file_space_id, SEXP _mem_space_id, SEXP _buf, SEXP _compoundAsDataFrame,
               SEXP _bit64conversion, SEXP _drop, SEXP _native ) {
    int compoundAsDataFrame = LOGICAL(_compoundAsDataFrame)[0];
    int drop = LOGICAL(_drop)[0];
    int native = LOGICAL(_native)[0];
    int bit64conversion = INTEGER(_bit64conversion)[0];
    
    /***********************************************************************/
    /* dataset_id                                                          */
    /***********************************************************************/
    hid_t dataset_id = STRSXP_2_HID( _dataset_id );
    
    /***********************************************************************/
    /* file_space_id and get dimensionality of output file_space and buf   */
    /***********************************************************************/
    hid_t file_space_id;
    
    if (length(_file_space_id) == 0) {
        file_space_id = H5Dget_space( dataset_id );
    } else {
        file_space_id = STRSXP_2_HID( _file_space_id );
    }
    
    /***********************************************************************/
    /* create mem_space_id                                                 */
    /***********************************************************************/
    
    hid_t mem_space_id;
    if (length(_mem_space_id) == 0) {
        H5S_sel_type sel_type = H5Sget_select_type(file_space_id);
        if (sel_type != H5S_SEL_ALL) {
            warning("file dataspace is set up for selective reading (e.g. hyperslabs). You have to provide a memory space for selective reading.");
            SEXP Rval = R_NilValue;
            return Rval;
        }
        int rank = H5Sget_simple_extent_ndims( file_space_id );
        hsize_t size[rank];
        hsize_t maxsize[rank];
        H5Sget_simple_extent_dims(file_space_id, size, maxsize);
        mem_space_id = H5Screate_simple( rank, size, size );
    } else {
        mem_space_id = STRSXP_2_HID( _mem_space_id );
    }
    
    /***********************************************************************/
    /* calculate buffer size and set dim-attribute                         */
    /***********************************************************************/
    
    int rank = H5Sget_simple_extent_ndims( mem_space_id );
    hsize_t size[rank];
    hsize_t maxsize[rank];
    H5Sget_simple_extent_dims(mem_space_id, size, maxsize);
    hsize_t n = 1;
    int too_large = 0;
    for (int i=0; i < rank; i++) {
        n = n * size[i];
        if(size[i] > 2147483647) {
            too_large = 1;
        }
    }

    hid_t dtype_id = H5Dget_type(dataset_id);
    hid_t dtypeclass_id = H5Tget_class(dtype_id);
    SEXP Rdim;
    int protect_bool = 0;
    
    if( rank == 0 ) {
    /* scalar with no dimensions */
        Rdim = NULL_USER_OBJECT;
    } else if( dtypeclass_id == H5T_ENUM && rank == 0 ) {
    /* do all ENUM have rank 0? */ 
        Rdim = NULL_USER_OBJECT;
    } else if( (dtypeclass_id == H5T_INTEGER || dtypeclass_id == H5T_FLOAT || dtypeclass_id == H5T_STRING) &&
        (drop || too_large) ) {
        Rdim = NULL_USER_OBJECT;
    } else {
        protect_bool = 1;
        Rdim = PROTECT(allocVector(INTSXP, rank));
        for (int i=0; i<rank; i++) {
            INTEGER(Rdim)[rank-i-1] = native ? size[rank-i-1] : size[i];
        }
    }
    
    if(!drop && rank > 1 && too_large) {
        warning("Dataset dimensions exceed R's maximum.  Coerced to a vector.");
    } 
     
    
    /***********************************************************************/
    /* read file space data type                                           */
    /***********************************************************************/
    
    SEXP Rval = H5Dread_helper(dataset_id, file_space_id, mem_space_id, n, 
                               Rdim, _buf, 
                               -1, -1, NULL, compoundAsDataFrame, bit64conversion, native);
    
    // close data type
    H5Tclose(dtype_id);
    
    // close mem space
    if (length(_mem_space_id) == 0) {
        H5Sclose(mem_space_id);
    }
    if(protect_bool) {
        UNPROTECT(1);
    }
    
    // close file space 
    if (length(_file_space_id) == 0) {
        H5Sclose(file_space_id);
    }
    return Rval;
}

/* herr_t H5Dwrite(hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t xfer_plist_id, const void * buf ) */
/* TODO more parameters: hid_t xfer_plist_id */
SEXP _H5Dwrite( SEXP _dataset_id, SEXP _buf, SEXP _file_space_id, SEXP _mem_space_id, SEXP _native) {
    hid_t dataset_id = STRSXP_2_HID( _dataset_id );
    int native = LOGICAL(_native)[0];
    hid_t mem_type_id;
    hid_t mem_space_id;
    if (length(_mem_space_id) == 0) {
        mem_space_id = H5S_ALL;
    } else {
        mem_space_id = STRSXP_2_HID( _mem_space_id );
    }
    hid_t file_space_id;
    if (length(_file_space_id) == 0) {
        file_space_id = H5S_ALL;
    } else {
        file_space_id = STRSXP_2_HID( _file_space_id );
    }
    
    const void * buf;
    static const char* H5Ref[] = {"H5Ref", ""};
    
    hid_t dim_space_id = mem_space_id == H5S_ALL ? dataset_id : mem_space_id;
    
    switch(TYPEOF(_buf)) {
    case RAWSXP :
        mem_type_id = H5T_NATIVE_UCHAR;
        if (native)
            PERMUTE(_buf, RAW, dim_space_id);
        buf = RAW(_buf);
        break;
    case INTSXP :
        mem_type_id = H5T_NATIVE_INT;
        if (native)
            PERMUTE(_buf, INTEGER, dim_space_id);
        buf = INTEGER(_buf);
        break;
    case REALSXP :
        mem_type_id = H5T_NATIVE_DOUBLE;
        if (native)
            PERMUTE(_buf, REAL, dim_space_id);
        buf = REAL(_buf);
        break;
    case LGLSXP :
        /* R logical is 32-bit, so this must stay as NATIVE_INT */
        mem_type_id = H5T_NATIVE_INT;
        if (native)
            PERMUTE(_buf, LOGICAL, dim_space_id);
        buf = LOGICAL(_buf);
        break;
    case STRSXP :
        mem_type_id = H5Dget_type(dataset_id);
        if (native)
            PERMUTE(_buf, STRING_PTR, dim_space_id);

        /* prepare for hdf5 */
        if (!H5Tis_variable_str(mem_type_id)) {
            size_t stsize = H5Tget_size( mem_type_id );
            char * strbuf = (char *)R_alloc(LENGTH(_buf),stsize);
            int z=0;
            int j;
            for (int i=0; i < LENGTH(_buf); i++) {
                for (j=0; (j < LENGTH(STRING_ELT(_buf,i))) & (j < stsize); j++) {
                    strbuf[z++] = CHAR(STRING_ELT(_buf,i))[j];
                }
                for (; j < stsize; j++) {
                    strbuf[z++] = '\0';
                }
            }
            buf = strbuf;
        } else {
            const char ** strbuf = (const char **)R_alloc(LENGTH(_buf), sizeof(char*));
            for (int i=0; i < LENGTH(_buf); i++) {
                strbuf[i] = CHAR(STRING_ELT(_buf, i));
            }
            buf = strbuf;
        }

        break;
    case S4SXP :
        if(R_check_class_etc(_buf, H5Ref) >= 0) {
          if(INTEGER(R_do_slot(_buf, mkString("type")))[0] == H5R_OBJECT) {
            mem_type_id = H5T_STD_REF_OBJ;
          } else if (INTEGER(R_do_slot(_buf, mkString("type")))[0] == H5R_DATASET_REGION) {
            mem_type_id = H5T_STD_REF_DSETREG;
          } else {
            mem_type_id = -1;
            UNPROTECT(native);
            Rf_error("Error writing references");
          }
          buf = RAW(R_do_slot(_buf, mkString("val")));
        } else {
          Rf_error("Class check failed\n");
        }
        break;
    default :
        mem_type_id = -1;
        UNPROTECT(native);
        error("Writing '%s' not supported.", Rf_type2char(TYPEOF(_buf)));
        break;
    }
    
    herr_t herr = H5Dwrite(dataset_id, mem_type_id, mem_space_id, file_space_id, H5P_DEFAULT, buf );
    SEXP Rval;
    PROTECT(Rval = allocVector(INTSXP, 1));
    INTEGER(Rval)[0] = herr;
    UNPROTECT( 1 + native );
    return Rval;
}

/* hid_t H5Dget_space(hid_t dataset_id ) */
SEXP _H5Dget_space(SEXP _dataset_id ) {
    hid_t dataset_id = STRSXP_2_HID( _dataset_id );
    hid_t sid = H5Dget_space( dataset_id );
    addHandle(sid);
    
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(sid));
    UNPROTECT(1);
    return Rval;
}

/* hid_t H5Dget_type(hid_t dataset_id) */
SEXP _H5Dget_type( SEXP _dataset_id ) {
    hid_t dataset_id = STRSXP_2_HID( _dataset_id );
    hid_t hid = H5Dget_type( dataset_id );
    
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(hid));
    UNPROTECT(1);
    return Rval;
}

/* hid_t H5Dget_create_plist(hid_t dataset_id ) */
SEXP _H5Dget_create_plist( SEXP _dataset_id ) {
    hid_t dataset_id = STRSXP_2_HID( _dataset_id );
    hid_t hid = H5Dget_create_plist( dataset_id );
    
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(hid));
    UNPROTECT(1);
    return Rval;
}

/* herr_t H5Dset_extent( hid_t dset_id, const hsize_t size[] ) */
SEXP _H5Dset_extent( SEXP _dataset_id, SEXP _size ) {
    //hid_t dataset_id = INTEGER(_dataset_id)[0];
    hid_t dataset_id = STRSXP_2_HID( _dataset_id );
    int rank = length(_size);
    herr_t herr = 3;
    if (rank > 0) {
        hsize_t size[rank];
        for (int i=0; i < rank; i++) {
            size[i] = (hsize_t) REAL(_size)[i];
        }
        herr = H5Dset_extent( dataset_id, size );
    } else {
        error("size parameter in H5Dset_extend has to be a vector of length > 0.");
    }
    SEXP Rval;
    PROTECT(Rval = allocVector(INTSXP, 1));
    INTEGER(Rval)[0] = herr;
    UNPROTECT(1);
    return Rval;
}
