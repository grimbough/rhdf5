#include "common.h"
#include "Rversion.h"

/* Only compile this part if we are version 1.3.0+ */
#if R_VERSION >= 66304

#include "R_ext/Rdynload.h"

#define CALL(NAME,ARGS) {#NAME,(DL_FUNC)&##NAME,ARGS},

/* Automate using sed or something. */
#if _MSC_VER >= 1000
__declspec(dllexport)
#endif
void R_init_hdf5(DllInfo *info)
{   
  static const R_CallMethodDef _CallFun[] = {
    CALL(HDF_init,0)
    CALL(HDF_is_valid,1)

    CALL(HDF_attr_set,3)
    CALL(HDF_attr_get,2)
    CALL(HDF_attribute_get,1)
    
    CALL(HDF_dataset_addmemory, 1)
    CALL(HDF_dataset_create_simple,5)
    CALL(HDF_dims,1)
    CALL(HDF_dataset_freememory, 1)
    CALL(HDF_dataset_getmemory, 1)
    CALL(HDF_dataset_getspace, 1)
    CALL(HDF_dataset_hasmemory, 1)
    CALL(HDF_dataset_length,1)
    CALL(HDF_dataset_mat_load,1)
    CALL(HDF_dataset_store,3)
    CALL(HDF_dataset_max, 1)
    CALL(HDF_dataset_min, 1)
    CALL(HDF_dataset_print,1)
    CALL(HDF_dataset_range,1)
    CALL(HDF_dataset_select,3)
    CALL(HDF_dataset_swapmemory, 2)
 

    CALL(HDF_space_print,1)
    CALL(HDF_space_store,3)
    CALL(HDF_space_load,3)
    CALL(HDF_select_hyperslab, 6)
    
    CALL(HDF_file_open,2)
    CALL(HDF_file_create,4)

    CALL(HDF_group_print,1)
    CALL(HDF_group_members,1)
    CALL(HDF_group_set_comment,3)
    CALL(HDF_group_get_comment,2)
    CALL(HDF_group_get_info,2)
    CALL(HDF_group_get_group,2)
    CALL(HDF_group_get_dataset,2)
    CALL(HDF_group_mkgroup,2)
    CALL(HDF_group_delete,2)
    CALL(HDF_group_apply,4)
    
    CALL(HDF_plist_print,1)
    CALL(HDF_plist_default_plist, 0)
    CALL(HDF_plist_set_sizes,2)
    CALL(HDF_plist_get_sizes,1)
    CALL(HDF_plist_file_create,2)
    CALL(HDF_plist_file_access,0)
    CALL(HDF_plist_dataset_create,0)
    CALL(HDF_plist_dataset_xfer,0)
    CALL(HDF_plist_mount,0)
    
    CALL(HDF_readCEL, 3)

    {NULL}
  };

  /* FIXME: should have a call to get_libversion here as well */

  R_registerRoutines(info,NULL,_CallFun,NULL,NULL);

  R_addToCConverter(R_converterMatchClass, HDF_convertoR, NULL,
		    "hdf5.dataset", "Convert HDF5 data to R data");
 
}


#endif
