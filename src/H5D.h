#ifndef _H5D_H
#define _H5D_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "HandleList.h"
#include "printdatatype.h"

/*################################*/
/* constants */
/*################################*/

/*################################*/
/* functions */
/*################################*/

SEXP _H5Dcreate( SEXP _loc_id, SEXP _name, SEXP _dtype_id, SEXP _space_id, SEXP _lcpl_id, SEXP _dcpl_id, SEXP _dapl_id );
/* H5Dcreate_anon */
SEXP _H5Dopen( SEXP _loc_id, SEXP _name, SEXP _dapl_id );
SEXP _H5Dclose( SEXP _dataset_id );

SEXP _H5Dget_space(SEXP _dataset_id );
/* H5Dget_space_status */
SEXP _H5Dget_type( SEXP _dataset_id );
/* H5Dget_create_plist */
/* H5Dget_access_plist */
/* H5Dget_offset */
/* H5Dget_storage_size */

/* H5Dvlen_get_buf_size */
/* H5Dvlen_reclaim */

SEXP H5Dread_helper_INTEGER(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf, 
			    hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame,
                            int bit64conversion );
SEXP H5Dread_helper_FLOAT(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf, 
			  hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame);
SEXP H5Dread_helper_STRING(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf, 
			   hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame);
SEXP H5Dread_helper_ENUM(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf, 
			   hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame);
SEXP H5Dread_helper_ARRAY(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf,
			   hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame);
SEXP H5Dread_helper_COMPOUND(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf, 
			     hid_t dtype_id, hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame,
			     int bit64conversion);
SEXP H5Dread_helper(hid_t dataset_id, hid_t file_space_id, hid_t mem_space_id, hsize_t n, SEXP Rdim, SEXP _buf, 
		    hid_t cpdType, int cpdNField, char ** cpdField, int compoundAsDataFrame,
                    int bit64conversion );
SEXP _H5Dread( SEXP _dataset_id, SEXP _file_space_id, SEXP _mem_space_id, SEXP _buf, SEXP _compoundAsDataFrame,
               SEXP _bit64conversion );
SEXP _H5Dwrite( SEXP _dataset_id, SEXP _buf, SEXP _file_space_id, SEXP _mem_space_id );
/* H5Diterate */

SEXP _H5Dset_extent( SEXP _dataset_id, SEXP _size );

/* H5Dfill */

#endif
