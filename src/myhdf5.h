#ifndef _MYHDF5_H
#define _MYHDF5_H

#ifndef SEXP
#include <Rdefines.h>
#endif

#ifdef WINDOWS
#include "includeWin/hdf5.h"
#else
#include "hdf5.h"
#endif

#define STRSXP_2_HID(x) (atoll(CHAR(asChar(x))));

SEXP HID_2_CHARSXP(hid_t hid);
SEXP HID_2_STRSXP(hid_t hid);

#endif

