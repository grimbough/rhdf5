#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "H5constants.h"
#include "H5.h"
#include "H5A.h"
#include "H5D.h"
#include "H5E.h"
#include "H5F.h"
#include "H5G.h"
#include "H5I.h"
#include "H5L.h"
#include "H5O.h"
#include "H5S.h"
#include "H5T.h"
#include "H5P.h"
#include "H5Z.h"
#include "h5ls.h"
#include "HandleList.h"
#include "h5dump.h"
#include "H5constants.h"
#include "h5writeDataFrame.h"
#include "printdatatype.h"
#include "h5testLock.h"

SEXP _H5open(void);
SEXP _H5close(void);
SEXP _H5garbage_collect(void);
SEXP _H5get_libversion(void);

static R_CallMethodDef libraryRCalls[] = {
  {"_H5open", (DL_FUNC) &_H5open, 0},
  {"_H5close", (DL_FUNC) &_H5close, 0},
  {"_H5garbage_collect", (DL_FUNC) &_H5garbage_collect, 0},
  {"_H5get_libversion", (DL_FUNC) &_H5get_libversion, 0},
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
  {"_H5Dcreate", (DL_FUNC) &_H5Dcreate, 7},
  {"_H5Dopen", (DL_FUNC) &_H5Dopen, 3},
  {"_H5Dclose", (DL_FUNC) &_H5Dclose, 1},
  {"_H5Dget_type", (DL_FUNC) &_H5Dget_type, 1},
  {"_H5Dget_create_plist", (DL_FUNC) &_H5Dget_create_plist, 1},
  {"_H5Dget_space", (DL_FUNC) &_H5Dget_space, 1},
  {"_H5Dget_storage_size", (DL_FUNC) &_H5Dget_space, 1},
  {"_H5Dread", (DL_FUNC) &_H5Dread, 8},
  {"_H5Dwrite", (DL_FUNC) &_H5Dwrite, 5},
  {"_H5Dset_extent", (DL_FUNC) &_H5Dset_extent, 2},
  {"_H5Fcreate", (DL_FUNC) &_H5Fcreate, 4},
  {"_H5Fopen", (DL_FUNC) &_H5Fopen, 2},
  {"_H5Freopen", (DL_FUNC) &_H5Freopen, 1},
  {"_H5Fclose", (DL_FUNC) &_H5Fclose, 1},
  {"_H5Fflush", (DL_FUNC) &_H5Fflush, 2},
  {"_H5Fis_hdf5", (DL_FUNC) &_H5Fis_hdf5, 1},
  {"_H5Fget_filesize", (DL_FUNC) &_H5Fget_filesize, 1},
  {"_H5Fget_create_plist", (DL_FUNC) &_H5Fget_create_plist, 1},
  {"_H5Fget_access_plist", (DL_FUNC) &_H5Fget_access_plist, 1},
  {"_H5Fget_name", (DL_FUNC) &_H5Fget_name, 1},
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
  {"_H5Lcreate_external", (DL_FUNC) &_H5Lcreate_external, 4},
  {"_H5Lexists", (DL_FUNC) &_H5Lexists, 2},
  {"_H5Ldelete", (DL_FUNC) &_H5Ldelete, 2},
  {"_H5Lmove", (DL_FUNC) &_H5Lmove, 6},
  {"_H5Lcopy", (DL_FUNC) &_H5Lcopy, 6},
  {"_H5Lget_info", (DL_FUNC) &_H5Lget_info, 2},
  {"_H5Screate", (DL_FUNC) &_H5Screate, 1},
  {"_H5Scopy", (DL_FUNC) &_H5Scopy, 1},
  {"_H5Sclose", (DL_FUNC) &_H5Sclose, 1},
  {"_H5Screate_simple", (DL_FUNC) &_H5Screate_simple, 2},
  {"_H5Sis_simple", (DL_FUNC) &_H5Sis_simple, 1},
  {"_H5Sget_simple_extent_dims", (DL_FUNC) &_H5Sget_simple_extent_dims, 1},
  {"_H5Sset_extent_simple", (DL_FUNC) &_H5Sset_extent_simple, 3},
  {"_H5Sget_select_npoints", (DL_FUNC) &_H5Sget_select_npoints, 1},
  {"_H5Sselect_none", (DL_FUNC) &_H5Sselect_none, 1},
  {"_H5Sselect_hyperslab", (DL_FUNC) &_H5Sselect_hyperslab, 6},
  {"_H5Sselect_index", (DL_FUNC) &_H5Sselect_index, 3},
  {"_H5Tcopy", (DL_FUNC) &_H5Tcopy, 1},
  {"_H5Tset_size", (DL_FUNC) &_H5Tset_size, 2},
  {"_H5Tget_size", (DL_FUNC) &_H5Tget_size, 1},
  {"_H5Tset_strpad", (DL_FUNC) &_H5Tset_strpad, 2},
  {"_H5Tget_strpad", (DL_FUNC) &_H5Tget_strpad, 1},
  {"_H5Pcreate", (DL_FUNC) &_H5Pcreate, 1},
  {"_H5Pget_class", (DL_FUNC) &_H5Pget_class, 1},
  {"_H5Pcopy", (DL_FUNC) &_H5Pcopy, 1},
  {"_H5Pclose", (DL_FUNC) &_H5Pclose, 1},
  {"_H5Pclose_class", (DL_FUNC) &_H5Pclose_class, 1},
  /* {"_H5Pget_version", (DL_FUNC) &_H5Pget_version, 5}, */
  /* {"_H5Pset_userblock", (DL_FUNC) &_H5Pset_userblock, 2}, */
  /* {"_H5Pget_userblock", (DL_FUNC) &_H5Pget_userblock, 2}, */
  /* {"_H5Pset_sizes", (DL_FUNC) &_H5Pset_sizes, 3}, */
  /* {"_H5Pget_sizes", (DL_FUNC) &_H5Pget_sizes, 3}, */
  /* {"_H5Pset_sym_k", (DL_FUNC) &_H5Pset_sym_k, 3}, */
  /* {"_H5Pget_sym_k", (DL_FUNC) &_H5Pget_sym_k, 3}, */
  /* {"_H5Pset_istore_k", (DL_FUNC) &_H5Pset_istore_k, 2}, */
  /* {"_H5Pget_istore_k", (DL_FUNC) &_H5Pget_istore_k, 2}, */
  /* {"_H5Pset_shared_mesg_nindexes", (DL_FUNC) &_H5Pset_shared_mesg_nindexes, 2}, */
  /* {"_H5Pget_shared_mesg_nindexes", (DL_FUNC) &_H5Pget_shared_mesg_nindexes, 2}, */
  /* {"_H5Pset_shared_mesg_index", (DL_FUNC) &_H5Pset_shared_mesg_index, 4}, */
  /* {"_H5Pget_shared_mesg_index", (DL_FUNC) &_H5Pget_shared_mesg_index, 4}, */
  /* {"_H5Pset_shared_mesg_phase_change", (DL_FUNC) &_H5Pset_shared_mesg_phase_change, 3}, */
  /* {"_H5Pget_shared_mesg_phase_change", (DL_FUNC) &_H5Pget_shared_mesg_phase_change, 3}, */
  /* {"_H5Pset_driver", (DL_FUNC) &_H5Pset_driver, 3}, */
  /* {"_H5Pget_driver", (DL_FUNC) &_H5Pget_driver, 1}, */
  /* {"_H5Pget_driver_info", (DL_FUNC) &_H5Pget_driver_info, 1}, */
  /* {"_H5Pset_fclose_degree", (DL_FUNC) &_H5Pset_fclose_degree, 2}, */
  /* {"_H5Pget_fclose_degree", (DL_FUNC) &_H5Pget_fclose_degree, 2}, */
  /* {"_H5Pset_fapl_core", (DL_FUNC) &_H5Pset_fapl_core, 3}, */
  /* {"_H5Pget_fapl_core", (DL_FUNC) &_H5Pget_fapl_core, 3}, */
  /* {"_H5Pset_core_write_tracking", (DL_FUNC) &_H5Pset_core_write_tracking, 3}, */
  /* {"_H5Pget_core_write_tracking", (DL_FUNC) &_H5Pget_core_write_tracking, 3}, */
  /* {"_H5Pset_fapl_direct", (DL_FUNC) &_H5Pset_fapl_direct, 4}, */
  /* {"_H5Pget_fapl_direct", (DL_FUNC) &_H5Pget_fapl_direct, 4}, */
  /* {"_H5Pset_fapl_family", (DL_FUNC) &_H5Pset_fapl_family, 3}, */
  /* {"_H5Pget_fapl_family", (DL_FUNC) &_H5Pget_fapl_family, 3}, */
  /* {"_H5Pset_family_offset", (DL_FUNC) &_H5Pset_family_offset, 2}, */
  /* {"_H5Pget_family_offset", (DL_FUNC) &_H5Pget_family_offset, 2}, */
  /* {"_H5Pset_fapl_log", (DL_FUNC) &_H5Pset_fapl_log, 4}, */
  /* {"_H5Pset_fapl_mpio", (DL_FUNC) &_H5Pset_fapl_mpio, 3}, */
  /* {"_H5Pget_fapl_mpio", (DL_FUNC) &_H5Pget_fapl_mpio, 3}, */
  /* {"_H5Pset_fapl_mpiposix", (DL_FUNC) &_H5Pset_fapl_mpiposix, 3}, */
  /* {"_H5Pget_fapl_mpiposix", (DL_FUNC) &_H5Pget_fapl_mpiposix, 3}, */
  /* {"_H5Pset_fapl_multi", (DL_FUNC) &_H5Pset_fapl_multi, 6}, */
  /* {"_H5Pget_fapl_multi", (DL_FUNC) &_H5Pget_fapl_multi, 6}, */
  /* {"_H5Pset_multi_type", (DL_FUNC) &_H5Pset_multi_type, 2}, */
  /* {"_H5Pget_multi_type", (DL_FUNC) &_H5Pget_multi_type, 2}, */
  /* {"_H5Pset_fapl_split", (DL_FUNC) &_H5Pset_fapl_split, 5}, */
  /* {"_H5Pset_fapl_sec2", (DL_FUNC) &_H5Pset_fapl_sec2, 1}, */
  /* {"_H5Pset_fapl_stdio", (DL_FUNC) &_H5Pset_fapl_stdio, 1}, */
  /* {"_H5Pset_fapl_windows", (DL_FUNC) &_H5Pset_fapl_windows, 1}, */
  /* {"_H5Pset_file_image", (DL_FUNC) &_H5Pset_file_image, 3}, */
  /* {"_H5Pget_file_image", (DL_FUNC) &_H5Pget_file_image, 3}, */
  /* {"_H5Pset_file_image_callbacks", (DL_FUNC) &_H5Pset_file_image_callbacks, 2}, */
  /* {"_H5Pget_file_image_callbacks", (DL_FUNC) &_H5Pget_file_image_callbacks, 2}, */
  /* {"_H5Pset_meta_block_size", (DL_FUNC) &_H5Pset_meta_block_size, 2}, */
  /* {"_H5Pget_meta_block_size", (DL_FUNC) &_H5Pget_meta_block_size, 2}, */
  /* {"_H5Pset_sieve_buf_size", (DL_FUNC) &_H5Pset_sieve_buf_size, 2}, */
  /* {"_H5Pget_sieve_buf_size", (DL_FUNC) &_H5Pget_sieve_buf_size, 2}, */
  /* {"_H5Pset_alignment", (DL_FUNC) &_H5Pset_alignment, 3}, */
  /* {"_H5Pget_alignment", (DL_FUNC) &_H5Pget_alignment, 3}, */
  /* {"_H5Pset_cache", (DL_FUNC) &_H5Pset_cache, 5}, */
  /* {"_H5Pget_cache", (DL_FUNC) &_H5Pget_cache, 5}, */
  /* {"_H5Pset_elink_file_cache_size", (DL_FUNC) &_H5Pset_elink_file_cache_size, 2}, */
  /* {"_H5Pget_elink_file_cache_size", (DL_FUNC) &_H5Pget_elink_file_cache_size, 2}, */
  /* {"_H5Pset_mdc_config", (DL_FUNC) &_H5Pset_mdc_config, 2}, */
  /* {"_H5Pget_mdc_config", (DL_FUNC) &_H5Pget_mdc_config, 2}, */
  /* {"_H5Pset_gc_reference", (DL_FUNC) &_H5Pset_gc_reference, 2}, */
  /* {"_H5Pget_gc_references", (DL_FUNC) &_H5Pget_gc_references, 2}, */
  /* {"_H5Pset_small_data_block_size", (DL_FUNC) &_H5Pset_small_data_block_size, 2}, */
  /* {"_H5Pget_small_data_block_size", (DL_FUNC) &_H5Pget_small_data_block_size, 2}, */
  {"_H5Pset_libver_bounds", (DL_FUNC) &_H5Pset_libver_bounds, 3},
  {"_H5Pget_libver_bounds", (DL_FUNC) &_H5Pget_libver_bounds, 1},
  /* {"_H5Pset_local_heap_size_hint", (DL_FUNC) &_H5Pset_local_heap_size_hint, 2}, */
  /* {"_H5Pget_local_heap_size_hint", (DL_FUNC) &_H5Pget_local_heap_size_hint, 2}, */
  /* {"_H5Pset_link_creation_order", (DL_FUNC) &_H5Pset_link_creation_order, 2}, */
  /* {"_H5Pget_link_creation_order", (DL_FUNC) &_H5Pget_link_creation_order, 2}, */
  /* {"_H5Pset_est_link_info", (DL_FUNC) &_H5Pset_est_link_info, 3}, */
  /* {"_H5Pget_est_link_info", (DL_FUNC) &_H5Pget_est_link_info, 3}, */
  /* {"_H5Pset_link_phase_change", (DL_FUNC) &_H5Pset_link_phase_change, 3}, */
  /* {"_H5Pget_link_phase_change", (DL_FUNC) &_H5Pget_link_phase_change, 3}, */
  {"_H5Pset_char_encoding", (DL_FUNC) &_H5Pset_char_encoding, 2},
  {"_H5Pget_char_encoding", (DL_FUNC) &_H5Pget_char_encoding, 1},
  {"_H5Pset_create_intermediate_group", (DL_FUNC) &_H5Pset_create_intermediate_group, 2},
  {"_H5Pget_create_intermediate_group", (DL_FUNC) &_H5Pget_create_intermediate_group, 1},
  /* {"_H5Pset_nlinks", (DL_FUNC) &_H5Pset_nlinks, 2}, */
  /* {"_H5Pget_nlinks", (DL_FUNC) &_H5Pget_nlinks, 2}, */
  /* {"_H5Pset_elink_cb", (DL_FUNC) &_H5Pset_elink_cb, 3}, */
  /* {"_H5Pget_elink_cb", (DL_FUNC) &_H5Pget_elink_cb, 3}, */
  /* {"_H5Pset_elink_prefix", (DL_FUNC) &_H5Pset_elink_prefix, 2}, */
  /* {"_H5Pget_elink_prefix", (DL_FUNC) &_H5Pget_elink_prefix, 3}, */
  /* {"_H5Pset_elink_fapl", (DL_FUNC) &_H5Pset_elink_fapl, 2}, */
  /* {"_H5Pget_elink_fapl", (DL_FUNC) &_H5Pget_elink_fapl, 1}, */
  /* {"_H5Pset_elink_acc_flags", (DL_FUNC) &_H5Pset_elink_acc_flags, 2}, */
  /* {"_H5Pget_elink_acc_flags", (DL_FUNC) &_H5Pget_elink_acc_flags, 2}, */
  {"_H5Pset_layout", (DL_FUNC) &_H5Pset_layout, 2},
  {"_H5Pget_layout", (DL_FUNC) &_H5Pget_layout, 1},
  {"_H5Pset_chunk", (DL_FUNC) &_H5Pset_chunk, 2},
  {"_H5Pget_chunk", (DL_FUNC) &_H5Pget_chunk, 1},
  {"_H5Pset_deflate", (DL_FUNC) &_H5Pset_deflate, 2},
  {"_H5Pset_fill_value", (DL_FUNC) &_H5Pset_fill_value, 3},
  /* {"_H5Pget_fill_value", (DL_FUNC) &_H5Pget_fill_value, 3}, */
  {"_H5Pfill_value_defined", (DL_FUNC) &_H5Pfill_value_defined, 1},
  {"_H5Pset_fill_time", (DL_FUNC) &_H5Pset_fill_time, 2},
  {"_H5Pget_fill_time", (DL_FUNC) &_H5Pget_fill_time, 1},
  {"_H5Pset_alloc_time", (DL_FUNC) &_H5Pset_alloc_time, 2},
  {"_H5Pget_alloc_time", (DL_FUNC) &_H5Pget_alloc_time, 1},
  /* {"_H5Pset_filter", (DL_FUNC) &_H5Pset_filter, 5}, */
  {"_H5Pall_filters_avail", (DL_FUNC) &_H5Pall_filters_avail, 1},
  {"_H5Pget_nfilters", (DL_FUNC) &_H5Pget_nfilters, 1},
  /* {"_H5Pget_filter1", (DL_FUNC) &_H5Pget_filter1, 7}, */
  {"_H5Pget_filter", (DL_FUNC) &_H5Pget_filter, 2},
  /* {"_H5Pget_filter_by_id1", (DL_FUNC) &_H5Pget_filter_by_id1, 7}, */
  /* {"_H5Pget_filter_by_id2", (DL_FUNC) &_H5Pget_filter_by_id2, 8}, */
  /* {"_H5Pmodify_filter", (DL_FUNC) &_H5Pmodify_filter, 5}, */
  /* {"_H5Premove_filter", (DL_FUNC) &_H5Premove_filter, 2}, */
  /* {"_H5Pset_fletcher32", (DL_FUNC) &_H5Pset_fletcher32, 1}, */
  /* {"_H5Pset_nbit", (DL_FUNC) &_H5Pset_nbit, 1}, */
  /* {"_H5Pset_scaleoffset", (DL_FUNC) &_H5Pset_scaleoffset, 3}, */
  /* {"_H5Pset_shuffle", (DL_FUNC) &_H5Pset_shuffle, 1}, */
  /* {"_H5Pset_szip", (DL_FUNC) &_H5Pset_szip, 3}, */
  /* {"_H5Pset_external", (DL_FUNC) &_H5Pset_external, 4}, */
  /* {"_H5Pget_external_count", (DL_FUNC) &_H5Pget_external_count, 1}, */
  /* {"_H5Pget_external", (DL_FUNC) &_H5Pget_external, 6}, */
  {"_H5Pset_chunk_cache", (DL_FUNC) &_H5Pset_chunk_cache, 4},
  /* {"_H5Pget_chunk_cache", (DL_FUNC) &_H5Pget_chunk_cache, 1}, */
  /* {"_H5Pset_buffer", (DL_FUNC) &_H5Pset_buffer, 4}, */
  /* {"_H5Pget_buffer", (DL_FUNC) &_H5Pget_buffer, 3}, */
  /* {"_H5Pset_preserve", (DL_FUNC) &_H5Pset_preserve, 2}, */
  /* {"_H5Pget_preserve", (DL_FUNC) &_H5Pget_preserve, 1}, */
  /* {"_H5Pset_edc_check", (DL_FUNC) &_H5Pset_edc_check, 2}, */
  /* {"_H5Pget_edc_check", (DL_FUNC) &_H5Pget_edc_check, 1}, */
  /* {"_H5Pset_filter_callback", (DL_FUNC) &_H5Pset_filter_callback, 3}, */
  /* {"_H5Pset_data_transform", (DL_FUNC) &_H5Pset_data_transform, 2}, */
  /* {"_H5Pget_data_transform", (DL_FUNC) &_H5Pget_data_transform, 3}, */
  /* {"_H5Pset_type_conv_cb", (DL_FUNC) &_H5Pset_type_conv_cb, 3}, */
  /* {"_H5Pget_type_conv_cb", (DL_FUNC) &_H5Pget_type_conv_cb, 3}, */
  /* {"_H5Pset_hyper_vector_size", (DL_FUNC) &_H5Pset_hyper_vector_size, 2}, */
  /* {"_H5Pget_hyper_vector_size", (DL_FUNC) &_H5Pget_hyper_vector_size, 2}, */
  /* {"_H5Pset_btree_ratios", (DL_FUNC) &_H5Pset_btree_ratios, 4}, */
  /* {"_H5Pget_btree_ratios", (DL_FUNC) &_H5Pget_btree_ratios, 4}, */
  /* {"_H5Pset_vlen_mem_manager", (DL_FUNC) &_H5Pset_vlen_mem_manager, 5}, */
  /* {"_H5Pget_vlen_mem_manager", (DL_FUNC) &_H5Pget_vlen_mem_manager, 5}, */
  /* {"_H5Pset_dxpl_mpio", (DL_FUNC) &_H5Pset_dxpl_mpio, 2}, */
  /* {"_H5Pset_dxpl_mpio_chunk_opt", (DL_FUNC) &_H5Pset_dxpl_mpio_chunk_opt, 2}, */
  /* {"_H5Pset_dxpl_mpio_chunk_opt_num", (DL_FUNC) &_H5Pset_dxpl_mpio_chunk_opt_num, 2}, */
  /* {"_H5Pset_dxpl_mpio_chunk_opt_ratio", (DL_FUNC) &_H5Pset_dxpl_mpio_chunk_opt_ratio, 2}, */
  /* {"_H5Pset_dxpl_mpio_collective_opt", (DL_FUNC) &_H5Pset_dxpl_mpio_collective_opt, 2}, */
  /* {"_H5Pget_dxpl_mpio", (DL_FUNC) &_H5Pget_dxpl_mpio, 2}, */
  /* {"_H5Pget_mpio_actual_chunk_opt_mode", (DL_FUNC) &_H5Pget_mpio_actual_chunk_opt_mode, 2}, */
  /* {"_H5Pget_mpio_actual_io_mode", (DL_FUNC) &_H5Pget_mpio_actual_io_mode, 2}, */
  /* {"_H5Pget_mpio_no_collective_cause", (DL_FUNC) &_H5Pget_mpio_no_collective_cause, 3}, */
  /* {"_H5Pset_create_intermediate_group", (DL_FUNC) &_H5Pset_create_intermediate_group, 2}, */
  /* {"_H5Pget_create_intermediate_group", (DL_FUNC) &_H5Pget_create_intermediate_group, 2}, */
  /* {"_H5Pset_obj_track_times", (DL_FUNC) &_H5Pset_obj_track_times, 2}, */
  /* {"_H5Pget_obj_track_times", (DL_FUNC) &_H5Pget_obj_track_times, 2}, */
  /* {"_H5Pset_attr_phase_change", (DL_FUNC) &_H5Pset_attr_phase_change, 3}, */
  /* {"_H5Pget_attr_phase_change", (DL_FUNC) &_H5Pget_attr_phase_change, 3}, */
  /* {"_H5Pset_attr_creation_order", (DL_FUNC) &_H5Pset_attr_creation_order, 2}, */
  /* {"_H5Pget_attr_creation_order", (DL_FUNC) &_H5Pget_attr_creation_order, 2}, */
  /* {"_H5Pset_copy_object", (DL_FUNC) &_H5Pset_copy_object, 2}, */
  /* {"_H5Pget_copy_object", (DL_FUNC) &_H5Pget_copy_object, 2}, */
  /* {"_H5Padd_merge_committed_dtype_path", (DL_FUNC) &_H5Padd_merge_committed_dtype_path, 2}, */
  /* {"_H5Pfree_merge_committed_dtype_paths", (DL_FUNC) &_H5Pfree_merge_committed_dtype_paths, 1}, */
  /* {"_H5Pset_mcdt_search_cb", (DL_FUNC) &_H5Pset_mcdt_search_cb, 3}, */
  /* {"_H5Pget_mcdt_search_cb", (DL_FUNC) &_H5Pget_mcdt_search_cb, 3}, */
  /* {"_H5Pset_char_encoding", (DL_FUNC) &_H5Pset_char_encoding, 2}, */
  /* {"_H5Pget_char_encoding", (DL_FUNC) &_H5Pget_char_encoding, 2}, */
  /* {"_H5Pcreate_class", (DL_FUNC) &_H5Pcreate_class, 8}, */
  /* {"_H5Pregister1", (DL_FUNC) &_H5Pregister1, 10}, */
  /* {"_H5Pregister2", (DL_FUNC) &_H5Pregister2, 11}, */
  /* {"_H5Pinsert1", (DL_FUNC) &_H5Pinsert1, 9}, */
  /* {"_H5Pinsert2", (DL_FUNC) &_H5Pinsert2, 10}, */
  /* {"_H5Pset", (DL_FUNC) &_H5Pset, 3}, */
  /* {"_H5Pexist", (DL_FUNC) &_H5Pexist, 2}, */
  /* {"_H5Pget_size", (DL_FUNC) &_H5Pget_size, 3}, */
  /* {"_H5Pget_nprops", (DL_FUNC) &_H5Pget_nprops, 2}, */
  /* {"_H5Pget_class_name", (DL_FUNC) &_H5Pget_class_name, 1}, */
  /* {"_H5Pget_class_parent", (DL_FUNC) &_H5Pget_class_parent, 1}, */
  /* {"_H5Pisa_class", (DL_FUNC) &_H5Pisa_class, 2}, */
  /* {"_H5Pget", (DL_FUNC) &_H5Pget, 3}, */
  {"_H5Pequal", (DL_FUNC) &_H5Pequal, 2},
  /* {"_H5Piterate", (DL_FUNC) &_H5Piterate, 4}, */
  /* {"_H5Pcopy_prop", (DL_FUNC) &_H5Pcopy_prop, 3}, */
  /* {"_H5Premove", (DL_FUNC) &_H5Premove, 2}, */
  /* {"_H5Punregister", (DL_FUNC) &_H5Punregister, 2}, */
  {"_H5Zfilter_avail", (DL_FUNC) &_H5Zfilter_avail, 1},
  {"_h5ls", (DL_FUNC) &_h5ls, 6},
  {"_h5dump", (DL_FUNC) &_h5dump, 4},
  {"_h5listIdentifier", (DL_FUNC) &_h5listIdentifier, 0},
  {"_h5validObjects", (DL_FUNC) &_h5validObjects, 0},
  /* {"_listHandles", (DL_FUNC) &_listHandles, 0}, */
  {"_handleInfo", (DL_FUNC) &_handleInfo, 1},
  {"_getDatatypeName", (DL_FUNC) &_getDatatypeName, 1},
  {"_getDatatypeClass", (DL_FUNC) &_getDatatypeClass, 1},
  {"_h5writeDataFrame", (DL_FUNC) &_h5writeDataFrame, 2},
  {"_h5createDataFrame", (DL_FUNC) &_h5createDataFrame, 5},
  {"_h5errorHandling", (DL_FUNC) &_h5errorHandling, 1},
  {"_h5fileLock", (DL_FUNC) &_h5fileLock, 1},
  {"_H5Sselect_cols", (DL_FUNC) &_H5Sselect_cols, 5},
  {"_h5listOpenObjects", (DL_FUNC) &_h5listOpenObjects, 1},
  {NULL, NULL, 0}
};

void R_init_rhdf5 (DllInfo * winDll) {
  R_registerRoutines (winDll, NULL, libraryRCalls, NULL, NULL);
  R_useDynamicSymbols (winDll, FALSE);
}

SEXP HID_2_CHARSXP(hid_t hid) {
    char tmp_string[21]; 
    sprintf(tmp_string, "%lld", (long long) hid);
    return(mkChar(tmp_string));
}

SEXP HID_2_STRSXP(hid_t hid) {
    char tmp_string[21]; 
    sprintf(tmp_string, "%lld", (long long) hid);
    return(mkString(tmp_string));
}