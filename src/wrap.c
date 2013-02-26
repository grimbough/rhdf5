#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "H5constants.h"
#include "H5A.h"
#include "H5D.h"
#include "H5F.h"
#include "H5G.h"
#include "H5I.h"
#include "H5L.h"
#include "H5O.h"
#include "H5S.h"
#include "H5T.h"
#include "h5ls.h"
#include "HandleList.h"
#include "h5dump.h"
#include "H5constants.h"
#include "h5writeDataFrame.h"
#include "printdatatype.h"

static R_CallMethodDef libraryRCalls[] = {
  {"_H5constants", (DL_FUNC) &_H5constants, 0},
  {"_H5Acreate", (DL_FUNC) &_H5Acreate, 4},
  {"_H5Aopen", (DL_FUNC) &_H5Aopen, 2},
  {"_H5Aopen_by_name", (DL_FUNC) &_H5Aopen_by_name, 3},
  {"_H5Aopen_by_idx", (DL_FUNC) &_H5Aopen_by_idx, 5},
  {"_H5Aexists", (DL_FUNC) &_H5Aexists, 2},
  {"_H5Aread", (DL_FUNC) &_H5Aread, 2},
  {"_H5Awrite", (DL_FUNC) &_H5Awrite, 2},
  {"_H5Aclose", (DL_FUNC) &_H5Aclose, 1},
  {"_H5Adelete", (DL_FUNC) &_H5Adelete, 2},
  {"_H5Aget_name", (DL_FUNC) &_H5Aget_name, 1},
  {"_H5Aget_space", (DL_FUNC) &_H5Aget_space, 1},
  {"_H5Aget_type", (DL_FUNC) &_H5Aget_type, 1},
  {"_H5Dcreate", (DL_FUNC) &_H5Dcreate, 6},
  {"_H5Dopen", (DL_FUNC) &_H5Dopen, 2},
  {"_H5Dclose", (DL_FUNC) &_H5Dclose, 1},
  {"_H5Dget_type", (DL_FUNC) &_H5Dget_type, 1},
  {"_H5Dget_space", (DL_FUNC) &_H5Dget_space, 1},
  {"_H5Dread", (DL_FUNC) &_H5Dread, 6},
  {"_H5Dwrite", (DL_FUNC) &_H5Dwrite, 4},
  {"_H5Fcreate", (DL_FUNC) &_H5Fcreate, 2},
  {"_H5Fopen", (DL_FUNC) &_H5Fopen, 2},
  {"_H5Freopen", (DL_FUNC) &_H5Freopen, 1},
  {"_H5Fclose", (DL_FUNC) &_H5Fclose, 1},
  {"_H5Fflush", (DL_FUNC) &_H5Fflush, 2},
  {"_H5Gcreate", (DL_FUNC) &_H5Gcreate, 2},
  {"_H5Gcreate_anon", (DL_FUNC) &_H5Gcreate_anon, 1},
  {"_H5Gopen", (DL_FUNC) &_H5Gopen, 2},
  {"_H5Gclose", (DL_FUNC) &_H5Gclose, 1},
  {"_H5Gget_info", (DL_FUNC) &_H5Gget_info, 1},
  {"_H5Gget_info_by_name", (DL_FUNC) &_H5Gget_info_by_name, 2},
  {"_H5Gget_info_by_idx", (DL_FUNC) &_H5Gget_info_by_idx, 5},
  /* {"_H5Lget_info", (DL_FUNC) &_H5Lget_info, 2}, */
  /* {"_H5Ldump", (DL_FUNC) &_H5Ldump, 1}, */
  {"_H5Iget_name", (DL_FUNC) &_H5Iget_name, 1},
  {"_H5Iget_type", (DL_FUNC) &_H5Iget_type, 1},
  {"_H5Iis_valid", (DL_FUNC) &_H5Iis_valid, 1},
  {"_H5Oopen", (DL_FUNC) &_H5Oopen, 2},
  {"_H5Oclose", (DL_FUNC) &_H5Oclose, 1},
  {"_H5Oget_num_attrs", (DL_FUNC) &_H5Oget_num_attrs, 1},
  //  {"_H5Oget_info", (DL_FUNC) &_H5Oget_info, 1},
  //  {"_H5Oget_info_by_name", (DL_FUNC) &_H5Oget_info_by_name, 2},
  {"_H5Lexists", (DL_FUNC) &_H5Lexists, 2},
  {"_H5Lget_info", (DL_FUNC) &_H5Lget_info, 2},
  {"_H5Screate", (DL_FUNC) &_H5Screate, 1},
  {"_H5Scopy", (DL_FUNC) &_H5Scopy, 1},
  {"_H5Sclose", (DL_FUNC) &_H5Sclose, 1},
  {"_H5Screate_simple", (DL_FUNC) &_H5Screate_simple, 2},
  {"_H5Sis_simple", (DL_FUNC) &_H5Sis_simple, 1},
  {"_H5Sget_simple_extent_dims", (DL_FUNC) &_H5Sget_simple_extent_dims, 1},
  {"_H5Sselect_hyperslab", (DL_FUNC) &_H5Sselect_hyperslab, 6},
  {"_H5Sselect_index", (DL_FUNC) &_H5Sselect_index, 3},
  {"_H5Tcopy", (DL_FUNC) &_H5Tcopy, 1},
  {"_H5Tset_size", (DL_FUNC) &_H5Tset_size, 2},
  {"_h5ls", (DL_FUNC) &_h5ls, 5},
  {"_h5dump", (DL_FUNC) &_h5dump, 4},
  {"_h5listIdentifier", (DL_FUNC) &_h5listIdentifier, 0},
  {"_h5validObjects", (DL_FUNC) &_h5validObjects, 0},
  /* {"_listHandles", (DL_FUNC) &_listHandles, 0}, */
  {"_handleInfo", (DL_FUNC) &_handleInfo, 1},
  {"_getDatatypeName", (DL_FUNC) &_getDatatypeName, 1},
  {"_getDatatypeClass", (DL_FUNC) &_getDatatypeClass, 1},
  {"_h5writeDataFrame", (DL_FUNC) &_h5writeDataFrame, 4},
  {NULL, NULL, 0}
};

void R_init_rhdf5 (DllInfo * winDll) {
  R_registerRoutines (winDll, NULL, libraryRCalls, NULL, NULL);
  R_useDynamicSymbols (winDll, FALSE);
}


