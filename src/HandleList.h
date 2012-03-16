#ifndef _HandleList_H
#define _HandleList_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"

/* enum HandleType {file = 0, group = 1, space = 2, dataset = 3, attribute = 4}; */

/* typedef struct { */
/*   enum HandleType type; */
/*   int ID; */
/*   const char *name; */
/*   const char *group; */
/*   enum HandleType parentType; */
/*   int parentID; */
/*   int parentFileID; */
/*   hid_t dtypeID; */
/*   hid_t spaceID; */
/*   int isvalid; */
/* } H5Handle; */

void addHandle(hid_t ID);
/* void addFileHandle(hid_t ID, const char *name); */
/* void addGroupHandle(hid_t ID, const char *name, hid_t loc_id); */
/* void addSpaceHandle(hid_t hid, H5S_class_t type); */
/* void addDatasetHandle(hid_t hid, hid_t loc_id, const char *name, hid_t dtype_id, hid_t space_id); */
/* void addAttributeHandle(hid_t hid, hid_t obj_id, const char *name, hid_t dtype_id, hid_t space_id); */

int removeHandle( hid_t fid );
/* int removeHandle( enum HandleType type, hid_t fid ); */

/* void listHandles( int addInfo ); */
/* SEXP _h5validHandles(  ); */
SEXP _h5listIdentifier( );
SEXP _h5validObjects( );
/* SEXP _h5lsIdentifier( ); */

/* H5Handle handleInfo(enum HandleType type, hid_t ID); */
SEXP _handleInfo(SEXP _ID);

#endif

