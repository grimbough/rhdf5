#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"

void uint32_to_int32(void* intbuf, hsize_t n, void* buf);
void int64_to_int32(void* intbuf, hsize_t n, void* buf, H5T_sign_t sign);
void uint32_to_double(void* intbuf, hsize_t n, void* buf);
void int64_to_double(void* intbuf, hsize_t n, void* buf, H5T_sign_t sign);
void uint32_to_integer64(void* intbuf, hsize_t n, void* buf);
void int64_to_integer64(void* intbuf, hsize_t n, void* buf, H5T_sign_t sign);
