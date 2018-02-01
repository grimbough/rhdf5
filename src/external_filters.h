#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "hdf5.h"

// from lzf_filter.h
#define H5PY_FILTER_LZF_VERSION 4
#define H5PY_FILTER_LZF 32000
int register_lzf(void);

SEXP _H5Pset_lzf( SEXP _plist_id );