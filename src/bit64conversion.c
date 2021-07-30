#include "bit64conversion.h"

void uint32_to_int32(void* intbuf, hsize_t n, void* buf) {
    
    long long i;
    int warn = 0;
    
    for (i=0; i<n; i++) {
        ((int *)buf)[i] = ((unsigned int *)intbuf)[i];
    }
    for (i=0; i<n; i++) {
        if (((unsigned int *)intbuf)[i] > INT_MAX) {
            ((int *)buf)[i] = INT_MIN;
            warn = 1;
        }
    }
    
    if(warn > 0) {
        warning("NAs produced by integer overflow while converting unisigned 32-bit integer from HDF5 to a signed 32-bit integer in R.\nChoose bit64conversion='bit64' or bit64conversion='double' to avoid data loss");
    }
    
}

void int64_to_int32(void* intbuf, hsize_t n, void* buf, H5T_sign_t sign) {
    
    long long i;
    int warn = 0;

    if (sign == H5T_SGN_2) {
        for (i=0; i<n; i++) {
            ((int *)buf)[i] = ((long long *)intbuf)[i];
        }
        for (i=0; i<n; i++) {
            if (((long long *)intbuf)[i] > INT_MAX) {
                ((int *)buf)[i] = INT_MIN;
                warn = 1;
            }
            if (((long long *)intbuf)[i] < INT_MIN) {
                ((int *)buf)[i] = INT_MIN;
                warn = 1;
            }
        }
    } else if (sign == H5T_SGN_NONE) {
        for (i=0; i<n; i++) {
            ((int *)buf)[i] = ((unsigned long long *)intbuf)[i];
        }
        for (i=0; i<n; i++) {
            if (((unsigned long long *)intbuf)[i] > INT_MAX) {
                ((int *)buf)[i] = INT_MIN;
                warn = 1;
            }
        }
    }
    
    if(warn > 0) {
        warning("NAs produced by integer overflow while converting 64-bit integer from HDF5 to a 32-bit integer in R.\nChoose bit64conversion='bit64' or bit64conversion='double' to avoid data loss");
    }

}

void uint32_to_double(void* intbuf, hsize_t n, void* buf) {
    long long i;
    for (i=0; i<n; i++){
        ((double *)buf)[i] = ((unsigned int *)intbuf)[i];
    }
}

void int64_to_double(void* intbuf, hsize_t n, void* buf, H5T_sign_t sign) {
    
    long long i;
    int warn_double = 0;
    
    if (sign == H5T_SGN_2) {
        for (i=0; i<n; i++){
            ((double *)buf)[i] = ((long long *)intbuf)[i];
        }
        for (i=0; i<n; i++) {
            if (((long long *)intbuf)[i] > 0x001fffffffffffffL) {
                warn_double = 1;
            }
            if (((long long *)intbuf)[i] < (long long) 0xffe0000000000000L) {
                warn_double = 1;
            }
        }
    } else if (sign == H5T_SGN_NONE) {
        for (i=0; i<n; i++){
            ((double *)buf)[i] = ((unsigned long long *)intbuf)[i];
        }
        for (i=0; i<n; i++) {
            if (((unsigned long long *)intbuf)[i] > 0x001fffffffffffffUL) {
                warn_double = 1;
            }
        } 
    }
    
    if (warn_double > 0) {
        warning("integer precision lost while converting 64-bit integer from HDF5 to double in R.\nChoose bit64conversion='bit64' to avoid data loss and see the vignette 'rhdf5' for more details about 64-bit integers.");
    }
    
}

void uint32_to_integer64(void* intbuf, hsize_t n, void* buf) {
    long long i;
    
    for (i=0; i<n; i++){
        ((long long *)buf)[i] = ((unsigned int *)intbuf)[i];
    }
}

void int64_to_integer64(void* intbuf, hsize_t n, void* buf, H5T_sign_t sign) {
    
    long long i;
    int warn_overflow_64bit = 0;
    
    if (sign == H5T_SGN_2) {
        for (i=0; i<n; i++){
            ((long long *)buf)[i] = ((long long *)intbuf)[i];
        }
    } else if (sign == H5T_SGN_NONE) {
        for (i=0; i<n; i++){
            ((long long *)buf)[i] = ((unsigned long long *)intbuf)[i];
        }
        for (i=0; i<n; i++) {
            if (((unsigned long long *)intbuf)[i] > LLONG_MAX) {
                ((long long *)buf)[i] = LLONG_MIN;
                warn_overflow_64bit = 1;
            }
        } 
    }
    
    if(warn_overflow_64bit > 0) {
        warning("NAs produced by integer overflow while converting unsigned 64-bit integer from HDF5 to signed 64-bit integer in R.");
    }
}

