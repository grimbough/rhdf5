#ifndef _H5I_H
#define _H5I_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "H5constants.h"


/*     H5Iget_file_id */
/*     H5Iget_name */
SEXP _H5Iget_type( SEXP _obj_id );
/*     H5Iobject_verify */
/*     H5Iremove_verify */
/*     H5Isearch */
/*     H5Iis_valid  */

/*     H5Iget_ref */
/*     H5Iinc_ref */
/*     H5Idec_ref */
/*     H5Iregister */
/*     H5Iregister_type */
/*     H5Idestroy_type  */

/*     H5Itype_exists */
/*     H5Iget_type_ref */
/*     H5Idec_type_ref */
/*     H5Iinc_type_ref */
/*     H5Iclear_type */
/*     H5Inmembers  */

#endif
