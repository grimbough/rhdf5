#ifndef _HandleList_H
#define _HandleList_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include <stdlib.h>
#include "myhdf5.h"

void addHandle(hid_t ID);

void removeHandle( hid_t fid );

SEXP _h5listIdentifier( );
SEXP _h5validObjects( );

SEXP _handleInfo(SEXP _ID);

SEXP _h5listOpenObjects(SEXP _file_id);

#endif

