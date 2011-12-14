#ifndef _H5printdatatype_H
#define _H5printdatatype_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include <stdio.h>

char* getDatatypeName(hid_t type);
char* getDatatypeClass(hid_t type);

SEXP _getDatatypeName(SEXP _type);
SEXP _getDatatypeClass(SEXP _type);


#endif
