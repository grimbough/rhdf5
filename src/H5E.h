#ifndef _H5E_H
#define _H5E_H

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

SEXP _h5errorHandling( SEXP _type );

herr_t _rhdf5PrintErrorR( hid_t estack_id, void * client_data);

herr_t _rhdf5PrintErrorRcompact( hid_t estack_id, void * client_data);

#endif
