#ifndef _MYHDF5_H
#define _MYHDF5_H

#ifdef WINDOWS
#include "includeWin/hdf5.h"
#else
#include "hdf5.h"
#endif

#endif


#define STRSEXP_2_HID(x) (atoll(CHAR(asChar(x))));