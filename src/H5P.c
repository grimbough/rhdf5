#include "H5P.h"
#include "H5pubconf.h"

////////////////////////////////////////////////////
// General Property List Operations
////////////////////////////////////////////////////

/* hid_t H5Pcreate( hid_t cls_id ) */
SEXP _H5Pcreate( SEXP _cls_id ) {
    hid_t cls_id =  STRSXP_2_HID( _cls_id );
    hid_t hid = H5Pcreate( cls_id );
    addHandle(hid);
    
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(hid));
    UNPROTECT(1);
    return Rval;
}

/* hid_t H5Pget_class(hid_t plist ) */
SEXP _H5Pget_class( SEXP _plist ) {
    hid_t plist = STRSXP_2_HID( _plist);
    hid_t hid = H5Pget_class( plist );
    addHandle(hid);
    
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(hid));
    UNPROTECT(1);
    return Rval;
}

/* hid_t H5Pcopy(hid_t plist ) */
SEXP _H5Pcopy( SEXP _plist ) {
    hid_t plist = STRSXP_2_HID( _plist);
    hid_t hid = H5Pcopy( plist );
    addHandle(hid);
    
    SEXP Rval;
    PROTECT(Rval = HID_2_STRSXP(hid));
    UNPROTECT(1);
    return Rval;
}

/* herr_t H5Pclose(hid_t plist ) */
SEXP _H5Pclose( SEXP _plist ) {
    hid_t plist = STRSXP_2_HID( _plist);
    herr_t herr = H5Pclose( plist );
    if (herr == 0) {
        removeHandle(plist);
    }
    
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

////////////////////////////////////////////////////
// File Creation Properties
////////////////////////////////////////////////////


/* /\* herr_t H5Pget_version(hid_t plist, unsigned * super, unsigned * freelist, unsigned * stab, unsigned * shhdr) *\/ */
SEXP _H5Pget_version(SEXP _plist) {
    hid_t plist = STRSXP_2_HID(_plist);
    unsigned super, freelist, stab, shhdr;
    herr_t herr = H5Pget_version(plist, &super, &freelist, &stab, &shhdr);
    SEXP Rval;
    if (herr >= 0) {
        static const char *names[] = {"superblock", "freelist", "symboltable", "shobjheader", ""};
        Rval = PROTECT(Rf_mkNamed(INTSXP, names));
        int *rval = INTEGER(Rval);
        rval[0] = super;
        rval[1] = freelist;
        rval[2] = stab;
        rval[3] = shhdr;
        UNPROTECT(1);
    } else
        Rval = R_NilValue;
    return Rval;
}

/* /\* herr_t H5Pset_userblock(hid_t plist, hsize_t size) *\/ */
SEXP _H5Pset_userblock(SEXP _plist, SEXP _size) {
    hid_t plist = STRSXP_2_HID(_plist);
    hsize_t size = INTEGER_VALUE(_size);
    herr_t herr = H5Pset_userblock(plist, size);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pget_userblock(hid_t plist, hsize_t * size) *\/ */
SEXP _H5Pget_userblock(SEXP _plist) {
    hid_t plist = STRSXP_2_HID(_plist);
    hsize_t size;
    herr_t herr = H5Pget_userblock(plist, &size);
    SEXP Rval = ScalarInteger(herr >= 0 ? size : herr);
    return Rval;
}

/* /\* herr_t H5Pset_sizes(hid_t plist, size_t sizeof_addr, size_t sizeof_size) *\/ */
SEXP _H5Pset_sizes(SEXP _plist, SEXP _sizeof_addr, SEXP _sizeof_size) {
    hid_t plist = STRSXP_2_HID(_plist);
    size_t sizeof_addr = INTEGER_VALUE(_sizeof_addr);
    size_t sizeof_size = INTEGER_VALUE(_sizeof_size);
    herr_t herr = H5Pset_sizes(plist, sizeof_addr, sizeof_size);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pget_sizes(hid_t plist, size_t * sizeof_addr, size_t * sizeof_size) *\/ */
SEXP _H5Pget_sizes(SEXP _plist) {
    hid_t plist = STRSXP_2_HID(_plist);
    size_t sizeof_addr, sizeof_size;
    herr_t herr = H5Pget_sizes(plist, &sizeof_addr, &sizeof_size);
    SEXP Rval;
    if (herr >= 0) {
        static const char *names[] = {"offset", "length", ""};
        Rval = PROTECT(Rf_mkNamed(INTSXP, names));
        int *rval = INTEGER(Rval);
        rval[0] = sizeof_addr;
        rval[1] = sizeof_size;
        UNPROTECT(1);
    } else
        Rval = ScalarInteger(herr);
  return Rval;
}

/* /\* herr_t H5Pset_sym_k(hid_t fcpl_id, unsigned ik, unsigned lk) *\/ */
SEXP _H5Pset_sym_k(SEXP _plist, SEXP _ik, SEXP _lk) {
    hid_t plist = STRSXP_2_HID(_plist);
    unsigned ik = INTEGER_VALUE(_ik);
    unsigned lk = INTEGER_VALUE(_lk);
    herr_t herr = H5Pset_sym_k(plist, ik, lk);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pget_sym_k(hid_t fcpl_id, unsigned * ik, unsigned * lk) *\/ */
SEXP _H5Pget_sym_k(SEXP _plist) {
    hid_t plist = STRSXP_2_HID(_plist);
    unsigned ik, lk;
    herr_t herr = H5Pget_sym_k(plist, &ik, &lk);
    SEXP Rval;
    if (herr >= 0) {
        static const char *names[] = {"ik", "lk", ""};
        Rval = PROTECT(Rf_mkNamed(INTSXP, names));
        int *rval = INTEGER(Rval);
        rval[0] = ik;
        rval[1] = lk;
        UNPROTECT(1);
    } else
        Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pset_istore_k(hid_t fcpl_id, unsigned ik) *\/ */
SEXP _H5Pset_istore_k(SEXP _plist, SEXP _ik) {
    hid_t plist = STRSXP_2_HID(_plist);
    unsigned ik = INTEGER_VALUE(_ik);
    herr_t herr = H5Pset_istore_k(plist, ik);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pget_istore_k(hid_t fcpl_id, unsigned * ik) *\/ */
SEXP _H5Pget_istore_k(SEXP _plist) {
    hid_t plist = STRSXP_2_HID(_plist);
    unsigned ik;
    herr_t herr = H5Pget_istore_k(plist, &ik);
    SEXP Rval = ScalarInteger(herr >= 0 ? ik : herr);
    return Rval;
}

/* /\* herr_t H5Pset_shared_mesg_nindexes(hid_t plist_id, unsigned nindexes) *\/ */
SEXP _H5Pset_shared_mesg_nindexes(SEXP _plist, SEXP _nindexes) {
    hid_t plist = STRSXP_2_HID(_plist);
    unsigned nindexes = INTEGER_VALUE(_nindexes);
    herr_t herr = H5Pset_shared_mesg_nindexes(plist, nindexes);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pget_shared_mesg_nindexes(hid_t fcpl_id, unsigned nindexes) *\/ */
SEXP _H5Pget_shared_mesg_nindexes(SEXP _plist) {
    hid_t plist = STRSXP_2_HID(_plist);
    unsigned nindexes;
    herr_t herr = H5Pget_shared_mesg_nindexes(plist, &nindexes);
    SEXP Rval = ScalarInteger(herr >= 0 ? nindexes : herr);
    return Rval;
}

/* /\* herr_t H5Pset_shared_mesg_index(hid_t fcpl_id, unsigned index_num, unsigned mesg_type_flags, unsigned min_mesg_size) *\/ */
SEXP _H5Pset_shared_mesg_index(SEXP _plist, SEXP _index_num, SEXP _mesg_type_flags, SEXP _min_mesg_size ) {
    hid_t plist = STRSXP_2_HID(_plist);
    unsigned index_num = INTEGER_VALUE(_index_num);
    unsigned mesg_type_flags = INTEGER_VALUE(_mesg_type_flags);
    unsigned min_mesg_size = INTEGER_VALUE(_min_mesg_size);
    herr_t herr = H5Pset_shared_mesg_index(plist, index_num, mesg_type_flags, min_mesg_size);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pget_shared_mesg_index(hid_t fcpl_id, unsigned index_num, unsigned mesg_type_flags, unsigned min_mesg_size) *\/ */
SEXP _H5Pget_shared_mesg_index(SEXP _plist, SEXP _index_num) {
    hid_t plist = STRSXP_2_HID(_plist);
    unsigned index_num = INTEGER_VALUE(_index_num), mesg_type_flags, min_mesg_size;
    herr_t herr = H5Pget_shared_mesg_index(plist, index_num, &mesg_type_flags, &min_mesg_size);
    SEXP Rval;
    if (herr >= 0) {
        static const char *names[] = {"type_flags", "size", ""};
        Rval = PROTECT(Rf_mkNamed(INTSXP, names));
        int *rval = INTEGER(Rval);
        rval[0] = mesg_type_flags;
        rval[1] = min_mesg_size;
        UNPROTECT(1);
    } else
        Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pset_shared_mesg_phase_change(hid_t fcpl_id, unsigned max_list, unsigned min_btree) *\/ */
SEXP _H5Pset_shared_mesg_phase_change(SEXP _plist, SEXP _max_list, SEXP _min_btree) {
    hid_t plist = STRSXP_2_HID(_plist);
    unsigned max_list = INTEGER_VALUE(_max_list);
    unsigned min_btree = INTEGER_VALUE(_min_btree);
    herr_t herr = H5Pset_shared_mesg_phase_change(plist, max_list, min_btree);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pget_shared_mesg_phase_change(hid_t fcpl_id, unsigned max_list, unsigned min_btree) *\/ */
SEXP _H5Pget_shared_mesg_phase_change(SEXP _plist) {
    hid_t plist = STRSXP_2_HID(_plist);
    unsigned max_list, min_btree;
    herr_t herr = H5Pget_shared_mesg_phase_change(plist, &max_list, &min_btree);
    SEXP Rval;
    if (herr >= 0) {
        static const char *names[] = {"max_list", "min_btree", ""};
        Rval = PROTECT(Rf_mkNamed(INTSXP, names));
        int *rval = INTEGER(Rval);
        rval[0] = max_list;
        rval[1] = min_btree;
        UNPROTECT(1);
    } else
        Rval = ScalarInteger(herr);
    return Rval;
}


////////////////////////////////////////////////////
// File Access Properties
////////////////////////////////////////////////////

/* herr_t H5Pset_driver(hid_t plist_id, hid_t new_driver_id, const void * new_driver_info) */
/* SEXP _H5Pset_driver( SEXP _plist_id, SEXP _new_driver_id, SEXP _new_driver_info ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   hid_t new_driver_id = INTEGER(_new_driver_id)[0]; */
/*   TODO: const void * new_driver_info = _new_driver_info */
/*   herr_t herr = H5Pset_driver(hid_tplist_id, hid_tnew_driver_id, const void *new_driver_info); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* hid_t H5Pget_driver(hid_t plist_id) *\/ */
/* SEXP _H5Pget_driver( SEXP _plist_id ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   hid_t hid = H5Pget_driver(hid_tplist_id); */
/*   addHandle(hid); */
/*   SEXP Rval = ScalarInteger(hid); */
/*   return Rval; */
/* } */

/* /\* void * H5Pget_driver_info(hid_t plist_id) *\/ */
/* SEXP _H5Pget_driver_info( SEXP _plist_id ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   H5Pget_driver_info(hid_tplist_id); */
/*   SEXP Rval = /\* R_NilValue; *\/ */
/* /\*   return Rval; *\/ */
/* /\* } *\/ */

/* /\* /\\* herr_t H5Pset_fclose_degree(hid_t fapl_id, H5F_close_degree_t fc_degree) *\\/ *\/ */
/* /\* SEXP _H5Pset_fclose_degree( SEXP _fapl_id, SEXP _fc_degree ) { *\/ */
/* /\*   hid_t fapl_id = INTEGER(_fapl_id)[0]; *\/ */
/* /\*   TODO: H5F_close_degree_t fc_degree = _fc_degree *\/ */
/* /\*   herr_t herr = H5Pset_fclose_degree(hid_tfapl_id, H5F_close_degree_tfc_degree); *\/ */
/* /\*   SEXP Rval = ScalarInteger(herr); *\/ */
/* /\*   return Rval; *\/ */
/* } */

/* /\* herr_t H5Pget_fclose_degree(hid_t fapl_id, H5F_close_degree_t * fc_degree) *\/ */
/* SEXP _H5Pget_fclose_degree( SEXP _fapl_id, SEXP _fc_degree ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: H5F_close_degree_t * fc_degree = _fc_degree */
/*   herr_t herr = H5Pget_fclose_degree(hid_tfapl_id, H5F_close_degree_t *fc_degree); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fapl_core(hid_t fapl_id, size_t increment, hbool_t backing_store) *\/ */
/* SEXP _H5Pset_fapl_core( SEXP _fapl_id, SEXP _increment, SEXP _backing_store ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   size_t increment = INTEGER(_increment)[0]; */
/*   TODO: hbool_t backing_store = _backing_store */
/*   herr_t herr = H5Pset_fapl_core(hid_tfapl_id, size_tincrement, hbool_tbacking_store); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_fapl_core(hid_t fapl_id, size_t increment, hbool_t backing_store) *\/ */
/* SEXP _H5Pget_fapl_core( SEXP _fapl_id, SEXP _increment, SEXP _backing_store ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   size_t increment = INTEGER(_increment)[0]; */
/*   TODO: hbool_t backing_store = _backing_store */
/*   herr_t herr = H5Pget_fapl_core(hid_tfapl_id, size_tincrement, hbool_tbacking_store); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_core_write_tracking(hid_t fapl_id, hbool_t is_enabled, size_t page_size) *\/ */
/* SEXP _H5Pset_core_write_tracking( SEXP _fapl_id, SEXP _is_enabled, SEXP _page_size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: hbool_t is_enabled = _is_enabled */
/*   size_t page_size = INTEGER(_page_size)[0]; */
/*   herr_t herr = H5Pset_core_write_tracking(hid_tfapl_id, hbool_tis_enabled, size_tpage_size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_core_write_tracking(hid_t fapl_id, hbool_t is_enabled, size_t page_size) *\/ */
/* SEXP _H5Pget_core_write_tracking( SEXP _fapl_id, SEXP _is_enabled, SEXP _page_size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: hbool_t is_enabled = _is_enabled */
/*   size_t page_size = INTEGER(_page_size)[0]; */
/*   herr_t herr = H5Pget_core_write_tracking(hid_tfapl_id, hbool_tis_enabled, size_tpage_size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fapl_direct(hid_t fapl_id, size_t alignment, size_t block_size, size_t cbuf_size) *\/ */
/* SEXP _H5Pset_fapl_direct( SEXP _fapl_id, SEXP _alignment, SEXP _block_size, SEXP _cbuf_size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   size_t alignment = INTEGER(_alignment)[0]; */
/*   size_t block_size = INTEGER(_block_size)[0]; */
/*   size_t cbuf_size = INTEGER(_cbuf_size)[0]; */
/*   herr_t herr = H5Pset_fapl_direct(hid_tfapl_id, size_talignment, size_tblock_size, size_tcbuf_size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_fapl_direct(hid_t fapl_id, size_t alignment, size_t block_size, size_t cbuf_size) *\/ */
/* SEXP _H5Pget_fapl_direct( SEXP _fapl_id, SEXP _alignment, SEXP _block_size, SEXP _cbuf_size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   size_t alignment = INTEGER(_alignment)[0]; */
/*   size_t block_size = INTEGER(_block_size)[0]; */
/*   size_t cbuf_size = INTEGER(_cbuf_size)[0]; */
/*   herr_t herr = H5Pget_fapl_direct(hid_tfapl_id, size_talignment, size_tblock_size, size_tcbuf_size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fapl_family(hid_t fapl_id, hsize_t memb_size, hid_t memb_fapl_id) *\/ */
/* SEXP _H5Pset_fapl_family( SEXP _fapl_id, SEXP _memb_size, SEXP _memb_fapl_id ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   hsize_t memb_size = INTEGER(_memb_size)[0]; */
/*   hid_t memb_fapl_id = INTEGER(_memb_fapl_id)[0]; */
/*   herr_t herr = H5Pset_fapl_family(hid_tfapl_id, hsize_tmemb_size, hid_tmemb_fapl_id); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_fapl_family(hid_t fapl_id, hsize_t * memb_size, hid_t * memb_fapl_id) *\/ */
/* SEXP _H5Pget_fapl_family( SEXP _fapl_id, SEXP _memb_size, SEXP _memb_fapl_id ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: hsize_t * memb_size = _memb_size */
/*   TODO: hid_t * memb_fapl_id = _memb_fapl_id */
/*   herr_t herr = H5Pget_fapl_family(hid_tfapl_id, hsize_t *memb_size, hid_t *memb_fapl_id); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_family_offset(hid_t fapl_id, hsize_t offset) *\/ */
/* SEXP _H5Pset_family_offset( SEXP _fapl_id, SEXP _offset ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   hsize_t offset = INTEGER(_offset)[0]; */
/*   herr_t herr = H5Pset_family_offset(hid_tfapl_id, hsize_toffset); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_family_offset(hid_t fapl_id, hsize_t * offset) *\/ */
/* SEXP _H5Pget_family_offset( SEXP _fapl_id, SEXP _offset ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: hsize_t * offset = _offset */
/*   herr_t herr = H5Pget_family_offset(hid_tfapl_id, hsize_t *offset); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fapl_log(hid_t fapl_id, const char * logfile, unsigned long long flags, size_t buf_size) *\/ */
/* SEXP _H5Pset_fapl_log( SEXP _fapl_id, SEXP _logfile, SEXP _flags, SEXP _buf_size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   const char * logfile = CHAR(STRING_ELT(_logfile, 0)); */
/*   unsigned long long flags = INTEGER(_flags)[0]; */
/*   size_t buf_size = INTEGER(_buf_size)[0]; */
/*   herr_t herr = H5Pset_fapl_log(hid_tfapl_id, const char *logfile, unsigned long longflags, size_tbuf_size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fapl_mpio(hid_t fapl_id, MPI_Comm comm, MPI_Info info) *\/ */
/* SEXP _H5Pset_fapl_mpio( SEXP _fapl_id, SEXP _comm, SEXP _info ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: MPI_Comm comm = _comm */
/*   TODO: MPI_Info info = _info */
/*   herr_t herr = H5Pset_fapl_mpio(hid_tfapl_id, MPI_Commcomm, MPI_Infoinfo); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_fapl_mpio(hid_t fapl_id, MPI_Comm * comm, MPI_Info * info) *\/ */
/* SEXP _H5Pget_fapl_mpio( SEXP _fapl_id, SEXP _comm, SEXP _info ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: MPI_Comm * comm = _comm */
/*   TODO: MPI_Info * info = _info */
/*   herr_t herr = H5Pget_fapl_mpio(hid_tfapl_id, MPI_Comm *comm, MPI_Info *info); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fapl_mpiposix(hid_t fapl_id, MPI_Comm comm, hbool_t use_gpfs_hints) *\/ */
/* SEXP _H5Pset_fapl_mpiposix( SEXP _fapl_id, SEXP _comm, SEXP _use_gpfs_hints ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: MPI_Comm comm = _comm */
/*   TODO: hbool_t use_gpfs_hints = _use_gpfs_hints */
/*   herr_t herr = H5Pset_fapl_mpiposix(hid_tfapl_id, MPI_Commcomm, hbool_tuse_gpfs_hints); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_fapl_mpiposix(hid_t fapl_id, MPI_Comm * comm, hbool_t * use_gpfs_hints) *\/ */
/* SEXP _H5Pget_fapl_mpiposix( SEXP _fapl_id, SEXP _comm, SEXP _use_gpfs_hints ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: MPI_Comm * comm = _comm */
/*   TODO: hbool_t * use_gpfs_hints = _use_gpfs_hints */
/*   herr_t herr = H5Pget_fapl_mpiposix(hid_tfapl_id, MPI_Comm *comm, hbool_t *use_gpfs_hints); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fapl_multi(hid_t fapl_id, const H5FD_mem_t * memb_map, const hid_t * memb_fapl, const char * const * memb_name, const haddr_t * memb_addr, hbool_t relax) *\/ */
/* SEXP _H5Pset_fapl_multi( SEXP _fapl_id, SEXP _memb_map, SEXP _memb_fapl, SEXP _memb_name, SEXP _memb_addr, SEXP _relax ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: const H5FD_mem_t * memb_map = _memb_map */
/*   TODO: const hid_t * memb_fapl = _memb_fapl */
/*   TODO: const char * const * memb_name = _memb_name */
/*   TODO: const haddr_t * memb_addr = _memb_addr */
/*   TODO: hbool_t relax = _relax */
/*   herr_t herr = H5Pset_fapl_multi(hid_tfapl_id, const H5FD_mem_t *memb_map, const hid_t *memb_fapl, const char * const *memb_name, const haddr_t *memb_addr, hbool_trelax); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_fapl_multi(hid_t fapl_id, const H5FD_mem_t * memb_map, const hid_t * memb_fapl, const char ** memb_name, const haddr_t * memb_addr, hbool_t * relax) *\/ */
/* SEXP _H5Pget_fapl_multi( SEXP _fapl_id, SEXP _memb_map, SEXP _memb_fapl, SEXP _memb_name, SEXP _memb_addr, SEXP _relax ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: const H5FD_mem_t * memb_map = _memb_map */
/*   TODO: const hid_t * memb_fapl = _memb_fapl */
/*   TODO: const char ** memb_name = _memb_name */
/*   TODO: const haddr_t * memb_addr = _memb_addr */
/*   TODO: hbool_t * relax = _relax */
/*   herr_t herr = H5Pget_fapl_multi(hid_tfapl_id, const H5FD_mem_t *memb_map, const hid_t *memb_fapl, const char **memb_name, const haddr_t *memb_addr, hbool_t *relax); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_multi_type(hid_t fapl_id, H5FD_mem_t type) *\/ */
/* SEXP _H5Pset_multi_type( SEXP _fapl_id, SEXP _type ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: H5FD_mem_t type = _type */
/*   herr_t herr = H5Pset_multi_type(hid_tfapl_id, H5FD_mem_ttype); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_multi_type(hid_t fapl_id, H5FD_mem_t * type) *\/ */
/* SEXP _H5Pget_multi_type( SEXP _fapl_id, SEXP _type ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: H5FD_mem_t * type = _type */
/*   herr_t herr = H5Pget_multi_type(hid_tfapl_id, H5FD_mem_t *type); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fapl_split(hid_t fapl_id, const char * meta_ext, hid_t meta_plist_id, const char * raw_ext, hid_t raw_plist_id) *\/ */
/* SEXP _H5Pset_fapl_split( SEXP _fapl_id, SEXP _meta_ext, SEXP _meta_plist_id, SEXP _raw_ext, SEXP _raw_plist_id ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   const char * meta_ext = CHAR(STRING_ELT(_meta_ext, 0)); */
/*   hid_t meta_plist_id = INTEGER(_meta_plist_id)[0]; */
/*   const char * raw_ext = CHAR(STRING_ELT(_raw_ext, 0)); */
/*   hid_t raw_plist_id = INTEGER(_raw_plist_id)[0]; */
/*   herr_t herr = H5Pset_fapl_split(hid_tfapl_id, const char *meta_ext, hid_tmeta_plist_id, const char *raw_ext, hid_traw_plist_id); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fapl_sec2(hid_t fapl_id) *\/ */
/* SEXP _H5Pset_fapl_sec2( SEXP _fapl_id ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   herr_t herr = H5Pset_fapl_sec2(hid_tfapl_id); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fapl_stdio(hid_t fapl_id) *\/ */
/* SEXP _H5Pset_fapl_stdio( SEXP _fapl_id ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   herr_t herr = H5Pset_fapl_stdio(hid_tfapl_id); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fapl_windows(hid_t fapl_id) *\/ */
/* SEXP _H5Pset_fapl_windows( SEXP _fapl_id ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   herr_t herr = H5Pset_fapl_windows(hid_tfapl_id); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_file_image(hid_t fapl_id, void * buf_ptr, size_t buf_len) *\/ */
/* SEXP _H5Pset_file_image( SEXP _fapl_id, SEXP _buf_ptr, SEXP _buf_len ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: void * buf_ptr = _buf_ptr */
/*   size_t buf_len = INTEGER(_buf_len)[0]; */
/*   herr_t herr = H5Pset_file_image(hid_tfapl_id, void *buf_ptr, size_tbuf_len); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_file_image(hid_t fapl_id, void ** buf_ptr_ptr, size_t * buf_len_ptr) *\/ */
/* SEXP _H5Pget_file_image( SEXP _fapl_id, SEXP _buf_ptr_ptr, SEXP _buf_len_ptr ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: void ** buf_ptr_ptr = _buf_ptr_ptr */
/*   TODO: size_t * buf_len_ptr = _buf_len_ptr */
/*   herr_t herr = H5Pget_file_image(hid_tfapl_id, void **buf_ptr_ptr, size_t *buf_len_ptr); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_file_image_callbacks(hid_t fapl_id, H5_file_image_callbacks_t * callbacks_ptr) *\/ */
/* SEXP _H5Pset_file_image_callbacks( SEXP _fapl_id, SEXP _callbacks_ptr ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: H5_file_image_callbacks_t * callbacks_ptr = _callbacks_ptr */
/*   herr_t herr = H5Pset_file_image_callbacks(hid_tfapl_id, H5_file_image_callbacks_t *callbacks_ptr); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_file_image_callbacks(hid_t fapl_id, H5_file_image_callbacks_t * callbacks_ptr) *\/ */
/* SEXP _H5Pget_file_image_callbacks( SEXP _fapl_id, SEXP _callbacks_ptr ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: H5_file_image_callbacks_t * callbacks_ptr = _callbacks_ptr */
/*   herr_t herr = H5Pget_file_image_callbacks(hid_tfapl_id, H5_file_image_callbacks_t *callbacks_ptr); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_meta_block_size(hid_t fapl_id, hsize_t size) *\/ */
/* SEXP _H5Pset_meta_block_size( SEXP _fapl_id, SEXP _size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   hsize_t size = INTEGER(_size)[0]; */
/*   herr_t herr = H5Pset_meta_block_size(hid_tfapl_id, hsize_tsize); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_meta_block_size(hid_t fapl_id, hsize_t * size) *\/ */
/* SEXP _H5Pget_meta_block_size( SEXP _fapl_id, SEXP _size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: hsize_t * size = _size */
/*   herr_t herr = H5Pget_meta_block_size(hid_tfapl_id, hsize_t *size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_sieve_buf_size(hid_t fapl_id, size_t size) *\/ */
/* SEXP _H5Pset_sieve_buf_size( SEXP _fapl_id, SEXP _size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   size_t size = INTEGER(_size)[0]; */
/*   herr_t herr = H5Pset_sieve_buf_size(hid_tfapl_id, size_tsize); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_sieve_buf_size(hid_t fapl_id, size_t * size) *\/ */
/* SEXP _H5Pget_sieve_buf_size( SEXP _fapl_id, SEXP _size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: size_t * size = _size */
/*   herr_t herr = H5Pget_sieve_buf_size(hid_tfapl_id, size_t *size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_alignment(hid_t plist, hsize_t threshold, hsize_t alignment) *\/ */
/* SEXP _H5Pset_alignment( SEXP _plist, SEXP _threshold, SEXP _alignment ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   hsize_t threshold = INTEGER(_threshold)[0]; */
/*   hsize_t alignment = INTEGER(_alignment)[0]; */
/*   herr_t herr = H5Pset_alignment(hid_tplist, hsize_tthreshold, hsize_talignment); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_alignment(hid_t plist, hsize_t *threshold, hsize_t *alignment) *\/ */
/* SEXP _H5Pget_alignment( SEXP _plist, SEXP _*threshold, SEXP _*alignment ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   hsize_t *threshold = INTEGER(_*threshold)[0]; */
/*   hsize_t *alignment = INTEGER(_*alignment)[0]; */
/*   herr_t herr = H5Pget_alignment(hid_tplist, hsize_t*threshold, hsize_t*alignment); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_cache(hid_t plist_id, int mdc_nelmts, size_t rdcc_nslots, size_t rdcc_nbytes, double rdcc_w0) *\/ */
/* SEXP _H5Pset_cache( SEXP _plist_id, SEXP _mdc_nelmts, SEXP _rdcc_nslots, SEXP _rdcc_nbytes, SEXP _rdcc_w0 ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   int mdc_nelmts = INTEGER(_mdc_nelmts)[0]; */
/*   size_t rdcc_nslots = INTEGER(_rdcc_nslots)[0]; */
/*   size_t rdcc_nbytes = INTEGER(_rdcc_nbytes)[0]; */
/*   double rdcc_w0 = REAL(_rdcc_w0)[0]; */
/*   herr_t herr = H5Pset_cache(hid_tplist_id, intmdc_nelmts, size_trdcc_nslots, size_trdcc_nbytes, doublerdcc_w0); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_cache(hid_t plist_id, int *mdc_nelmts, size_t *rdcc_nelmts, size_t *rdcc_nbytes, double *rdcc_w0) *\/ */
/* SEXP _H5Pget_cache( SEXP _plist_id, SEXP _*mdc_nelmts, SEXP _*rdcc_nelmts, SEXP _*rdcc_nbytes, SEXP _*rdcc_w0 ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   int *mdc_nelmts = INTEGER(_*mdc_nelmts)[0]; */
/*   size_t *rdcc_nelmts = INTEGER(_*rdcc_nelmts)[0]; */
/*   size_t *rdcc_nbytes = INTEGER(_*rdcc_nbytes)[0]; */
/*   double *rdcc_w0 = REAL(_*rdcc_w0)[0]; */
/*   herr_t herr = H5Pget_cache(hid_tplist_id, int*mdc_nelmts, size_t*rdcc_nelmts, size_t*rdcc_nbytes, double*rdcc_w0); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_elink_file_cache_size(hid_t fapl_id, unsigned efc_size) *\/ */
/* SEXP _H5Pset_elink_file_cache_size( SEXP _fapl_id, SEXP _efc_size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   unsigned efc_size = INTEGER(_efc_size)[0]; */
/*   herr_t herr = H5Pset_elink_file_cache_size(hid_tfapl_id, unsignedefc_size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_elink_file_cache_size(hid_t fapl_id, unsigned * efc_size) *\/ */
/* SEXP _H5Pget_elink_file_cache_size( SEXP _fapl_id, SEXP _efc_size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: unsigned * efc_size = _efc_size */
/*   herr_t herr = H5Pget_elink_file_cache_size(hid_tfapl_id, unsigned *efc_size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_mdc_config(hid_t plist_id, H5AC_cache_config_t * config_ptr) *\/ */
/* SEXP _H5Pset_mdc_config( SEXP _plist_id, SEXP _config_ptr ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: H5AC_cache_config_t * config_ptr = _config_ptr */
/*   herr_t herr = H5Pset_mdc_config(hid_tplist_id, H5AC_cache_config_t *config_ptr); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_mdc_config(hid_t plist_id, H5AC_cache_config_t * config_ptr) *\/ */
/* SEXP _H5Pget_mdc_config( SEXP _plist_id, SEXP _config_ptr ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: H5AC_cache_config_t * config_ptr = _config_ptr */
/*   herr_t herr = H5Pget_mdc_config(hid_tplist_id, H5AC_cache_config_t *config_ptr); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_gc_reference(hid_t plist, unsigned gc_ref) *\/ */
/* SEXP _H5Pset_gc_reference( SEXP _plist, SEXP _gc_ref ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   unsigned gc_ref = INTEGER(_gc_ref)[0]; */
/*   herr_t herr = H5Pset_gc_reference(hid_tplist, unsignedgc_ref); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_gc_references(hid_t plist, unsigned gc_ref) *\/ */
/* SEXP _H5Pget_gc_references( SEXP _plist, SEXP _gc_ref ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   unsigned gc_ref = INTEGER(_gc_ref)[0]; */
/*   herr_t herr = H5Pget_gc_references(hid_tplist, unsignedgc_ref); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_small_data_block_size(hid_t fapl_id, hsize_t size) *\/ */
/* SEXP _H5Pset_small_data_block_size( SEXP _fapl_id, SEXP _size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   hsize_t size = INTEGER(_size)[0]; */
/*   herr_t herr = H5Pset_small_data_block_size(hid_tfapl_id, hsize_tsize); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_small_data_block_size(hid_t fapl_id, hsize_t * size) *\/ */
/* SEXP _H5Pget_small_data_block_size( SEXP _fapl_id, SEXP _size ) { */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   TODO: hsize_t * size = _size */
/*   herr_t herr = H5Pget_small_data_block_size(hid_tfapl_id, hsize_t *size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_libver_bounds(hid_t fapl_id, H5F_libver_t libver_low, H5F_libver_t libver_high) *\/ */
SEXP _H5Pset_libver_bounds( SEXP _fapl_id, SEXP _libver_low, SEXP _libver_high ) {
    hid_t fapl_id = STRSXP_2_HID( _fapl_id );
    H5F_libver_t libver_low = INTEGER(_libver_low)[0];
    H5F_libver_t libver_high = INTEGER(_libver_high)[0];
    herr_t herr = H5Pset_libver_bounds(fapl_id, libver_low, libver_high);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pget_libver_bounds(hid_t fapl_id, H5F_libver_t * libver_low, H5F_libver_t * libver_high) *\/ */
SEXP _H5Pget_libver_bounds( SEXP _fapl_id ) {
    hid_t fapl_id = STRSXP_2_HID( _fapl_id );
    H5F_libver_t libver_low;
    H5F_libver_t libver_high;
    herr_t herr = H5Pget_libver_bounds(fapl_id, &libver_low, &libver_high);
    if (herr != 0) {
        error("Error while calling H5Pget_libver_bounds");
    }
    Rprintf("low: %d high: %d\n", libver_low, libver_high);
    SEXP Rval;
    PROTECT(Rval = allocVector(INTSXP, 2));
    INTEGER(Rval)[0] = libver_low;
    INTEGER(Rval)[1] = libver_high;
    UNPROTECT(1);
    return Rval;
}


#ifdef H5_HAVE_ROS3_VFD
/* herr_t H5Pset_fapl_ros3(hid_t fapl_id, H5FD_ros3_fapl_t *fa) */
/* We pass the components of the H5FD_ros3_fapl_t separately */
SEXP _H5Pset_fapl_ros3( SEXP _fapl_id, SEXP _authenticate, SEXP _aws_region, SEXP _access_key_id, SEXP _secret_access_key ) {
    
    hid_t fapl_id = STRSXP_2_HID( _fapl_id );
    
    // initialise in an non-authenticating fapl configuration 
    H5FD_ros3_fapl_t fa = { 1, 0, "", "", "" };
    int should_authenticate = INTEGER(_authenticate)[0];
    const char *the_region = CHAR(STRING_ELT(_aws_region, 0));
    const char *the_access_key_id = CHAR(STRING_ELT(_access_key_id, 0));
    const char *the_secret_access_key = CHAR(STRING_ELT(_secret_access_key, 0));
    
    fa.authenticate = should_authenticate; /* 0 (FALSE) or 1 (TRUE) */
    strncpy(fa.aws_region, the_region, H5FD_ROS3_MAX_REGION_LEN);
    strncpy(fa.secret_id, the_access_key_id, H5FD_ROS3_MAX_SECRET_ID_LEN);
    strncpy(fa.secret_key, the_secret_access_key, H5FD_ROS3_MAX_SECRET_KEY_LEN);
    
    herr_t herr = H5Pset_fapl_ros3(fapl_id, &fa);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}
#else
SEXP _H5Pset_fapl_ros3( SEXP _fapl_id, SEXP _authenticate, SEXP _aws_region, SEXP _access_key_id, SEXP _secret_access_key ) {
    
    error("Rhdf5lib was not compiled with support for the S3 VFD\n");

    herr_t herr = 0;
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}
#endif




////////////////////////////////////////////////////
// Group Creation Properties
////////////////////////////////////////////////////

/* /\* herr_t H5Pset_local_heap_size_hint(hid_t gcpl_id, size_t size_hint) *\/ */
/* SEXP _H5Pset_local_heap_size_hint( SEXP _gcpl_id, SEXP _size_hint ) { */
/*   hid_t gcpl_id = INTEGER(_gcpl_id)[0]; */
/*   size_t size_hint = INTEGER(_size_hint)[0]; */
/*   herr_t herr = H5Pset_local_heap_size_hint(hid_tgcpl_id, size_tsize_hint); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_local_heap_size_hint(hid_t gcpl_id, size_t * size_hint) *\/ */
/* SEXP _H5Pget_local_heap_size_hint( SEXP _gcpl_id, SEXP _size_hint ) { */
/*   hid_t gcpl_id = INTEGER(_gcpl_id)[0]; */
/*   TODO: size_t * size_hint = _size_hint */
/*   herr_t herr = H5Pget_local_heap_size_hint(hid_tgcpl_id, size_t *size_hint); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_link_creation_order(hid_t gcpl_id, unsigned crt_order_flags) *\/ */
/* SEXP _H5Pset_link_creation_order( SEXP _gcpl_id, SEXP _crt_order_flags ) { */
/*   hid_t gcpl_id = INTEGER(_gcpl_id)[0]; */
/*   unsigned crt_order_flags = INTEGER(_crt_order_flags)[0]; */
/*   herr_t herr = H5Pset_link_creation_order(hid_tgcpl_id, unsignedcrt_order_flags); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_link_creation_order(hid_t gcpl_id, unsigned crt_order_flags) *\/ */
/* SEXP _H5Pget_link_creation_order( SEXP _gcpl_id, SEXP _crt_order_flags ) { */
/*   hid_t gcpl_id = INTEGER(_gcpl_id)[0]; */
/*   unsigned crt_order_flags = INTEGER(_crt_order_flags)[0]; */
/*   herr_t herr = H5Pget_link_creation_order(hid_tgcpl_id, unsignedcrt_order_flags); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_est_link_info(hid_t gcpl_id, unsigned est_num_entries, unsigned est_name_len) *\/ */
/* SEXP _H5Pset_est_link_info( SEXP _gcpl_id, SEXP _est_num_entries, SEXP _est_name_len ) { */
/*   hid_t gcpl_id = INTEGER(_gcpl_id)[0]; */
/*   unsigned est_num_entries = INTEGER(_est_num_entries)[0]; */
/*   unsigned est_name_len = INTEGER(_est_name_len)[0]; */
/*   herr_t herr = H5Pset_est_link_info(hid_tgcpl_id, unsignedest_num_entries, unsignedest_name_len); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_est_link_info(hid_t gcpl_id, unsigned est_num_entries, unsigned est_name_len) *\/ */
/* SEXP _H5Pget_est_link_info( SEXP _gcpl_id, SEXP _est_num_entries, SEXP _est_name_len ) { */
/*   hid_t gcpl_id = INTEGER(_gcpl_id)[0]; */
/*   unsigned est_num_entries = INTEGER(_est_num_entries)[0]; */
/*   unsigned est_name_len = INTEGER(_est_name_len)[0]; */
/*   herr_t herr = H5Pget_est_link_info(hid_tgcpl_id, unsignedest_num_entries, unsignedest_name_len); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_link_phase_change(hid_t gcpl_id, unsigned max_compact, unsigned min_dense) *\/ */
/* SEXP _H5Pset_link_phase_change( SEXP _gcpl_id, SEXP _max_compact, SEXP _min_dense ) { */
/*   hid_t gcpl_id = INTEGER(_gcpl_id)[0]; */
/*   unsigned max_compact = INTEGER(_max_compact)[0]; */
/*   unsigned min_dense = INTEGER(_min_dense)[0]; */
/*   herr_t herr = H5Pset_link_phase_change(hid_tgcpl_id, unsignedmax_compact, unsignedmin_dense); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_link_phase_change(hid_t gcpl_id, unsigned max_compact, unsigned min_dense) *\/ */
/* SEXP _H5Pget_link_phase_change( SEXP _gcpl_id, SEXP _max_compact, SEXP _min_dense ) { */
/*   hid_t gcpl_id = INTEGER(_gcpl_id)[0]; */
/*   unsigned max_compact = INTEGER(_max_compact)[0]; */
/*   unsigned min_dense = INTEGER(_min_dense)[0]; */
/*   herr_t herr = H5Pget_link_phase_change(hid_tgcpl_id, unsignedmax_compact, unsignedmin_dense); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

////////////////////////////////////////////////////
// Link Creation Properties
////////////////////////////////////////////////////


/* herr_t H5Pset_char_encoding(hid_t plist_id, H5T_cset_t encoding) */
SEXP _H5Pset_char_encoding( SEXP _plist_id, SEXP _encoding ) {
    //hid_t plist_id = INTEGER(_plist_id)[0];
    hid_t plist_id = STRSXP_2_HID( _plist_id );
    H5T_cset_t encoding = INTEGER(_encoding)[0];
    herr_t herr = H5Pset_char_encoding(plist_id, encoding);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* herr_t H5Pget_char_encoding(hid_t plist_id, H5T_cset_t encoding) */
SEXP _H5Pget_char_encoding( SEXP _plist_id ) {

    hid_t plist_id = STRSXP_2_HID( _plist_id );
    H5T_cset_t encoding;
    herr_t herr = H5Pget_char_encoding(plist_id, &encoding);
    SEXP Rval = R_NilValue;
    if (herr < 0) {
        Rval = R_NilValue;
    } else {
        Rval = ScalarInteger(encoding);
    }
    return Rval;
}

/* herr_t H5Pset_create_intermediate_group(hid_t lcpl_id, unsigned crt_intermed_group) */
SEXP _H5Pset_create_intermediate_group( SEXP _lcpl_id, SEXP _crt_intermed_group ) {
    hid_t lcpl_id = STRSXP_2_HID( _lcpl_id );
    unsigned crt_intermed_group = (unsigned int)INTEGER(_crt_intermed_group)[0];
    herr_t herr = H5Pset_create_intermediate_group(lcpl_id, crt_intermed_group);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* herr_t H5Pget_create_intermediate_group(hid_t lcpl_id, unsigned * crt_intermed_group) */
SEXP _H5Pget_create_intermediate_group( SEXP _lcpl_id ) {
    hid_t lcpl_id = STRSXP_2_HID( _lcpl_id );
    unsigned crt_intermed_group[1];
    herr_t herr = H5Pget_create_intermediate_group(lcpl_id, crt_intermed_group);
    SEXP Rval;
    if (herr < 0) {
        Rval = R_NilValue;
    } else {
        Rval = ScalarLogical(crt_intermed_group[0]);
    }
    return Rval;
}

////////////////////////////////////////////////////
// Link Access Properties
////////////////////////////////////////////////////


/* /\* herr_t H5Pset_nlinks(hid_t lapl_id, size_t nlinks) *\/ */
/* SEXP _H5Pset_nlinks( SEXP _lapl_id, SEXP _nlinks ) { */
/*   hid_t lapl_id = INTEGER(_lapl_id)[0]; */
/*   size_t nlinks = INTEGER(_nlinks)[0]; */
/*   herr_t herr = H5Pset_nlinks(hid_tlapl_id, size_tnlinks); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_nlinks(hid_t lapl_id, size_t&nbsp;* nlinks) *\/ */
/* SEXP _H5Pget_nlinks( SEXP _lapl_id, SEXP _nlinks ) { */
/*   hid_t lapl_id = INTEGER(_lapl_id)[0]; */
/*   TODO: size_t&nbsp;* nlinks = _nlinks */
/*   herr_t herr = H5Pget_nlinks(hid_tlapl_id, size_t&nbsp;*nlinks); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_elink_cb(hid_t lapl_id, H5L_elink_traverse_t func, void * op_data) *\/ */
/* SEXP _H5Pset_elink_cb( SEXP _lapl_id, SEXP _func, SEXP _op_data ) { */
/*   hid_t lapl_id = INTEGER(_lapl_id)[0]; */
/*   TODO: H5L_elink_traverse_t func = _func */
/*   TODO: void * op_data = _op_data */
/*   herr_t herr = H5Pset_elink_cb(hid_tlapl_id, H5L_elink_traverse_tfunc, void *op_data); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_elink_cb(hid_t lapl_id, H5L_elink_traverse_t * func, void ** op_data) *\/ */
/* SEXP _H5Pget_elink_cb( SEXP _lapl_id, SEXP _func, SEXP _op_data ) { */
/*   hid_t lapl_id = INTEGER(_lapl_id)[0]; */
/*   TODO: H5L_elink_traverse_t * func = _func */
/*   TODO: void ** op_data = _op_data */
/*   herr_t herr = H5Pget_elink_cb(hid_tlapl_id, H5L_elink_traverse_t *func, void **op_data); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_elink_prefix(hid_t lapl_id, const char * prefix) *\/ */
/* SEXP _H5Pset_elink_prefix( SEXP _lapl_id, SEXP _prefix ) { */
/*   hid_t lapl_id = INTEGER(_lapl_id)[0]; */
/*   const char * prefix = CHAR(STRING_ELT(_prefix, 0)); */
/*   herr_t herr = H5Pset_elink_prefix(hid_tlapl_id, const char *prefix); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* ssize_t H5Pget_elink_prefix(hid_t lapl_id, char * prefix, size_t size) *\/ */
/* SEXP _H5Pget_elink_prefix( SEXP _lapl_id, SEXP _prefix, SEXP _size ) { */
/*   hid_t lapl_id = INTEGER(_lapl_id)[0]; */
/*   char * prefix = CHAR(STRING_ELT(_prefix, 0)); */
/*   size_t size = INTEGER(_size)[0]; */
/*   ssize_t s = H5Pget_elink_prefix(hid_tlapl_id, char *prefix, size_tsize); */
/*   SEXP Rval = ScalarInteger(s); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_elink_fapl(hid_t lapl_id, hid_t fapl_id) *\/ */
/* SEXP _H5Pset_elink_fapl( SEXP _lapl_id, SEXP _fapl_id ) { */
/*   hid_t lapl_id = INTEGER(_lapl_id)[0]; */
/*   hid_t fapl_id = INTEGER(_fapl_id)[0]; */
/*   herr_t herr = H5Pset_elink_fapl(hid_tlapl_id, hid_tfapl_id); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* hid_t H5Pget_elink_fapl(hid_t lapl_id) *\/ */
/* SEXP _H5Pget_elink_fapl( SEXP _lapl_id ) { */
/*   hid_t lapl_id = INTEGER(_lapl_id)[0]; */
/*   hid_t hid = H5Pget_elink_fapl(hid_tlapl_id); */
/*   addHandle(hid); */
/*   SEXP Rval = ScalarInteger(hid); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_elink_acc_flags(hid_t lapl_id, unsigned flags) *\/ */
/* SEXP _H5Pset_elink_acc_flags( SEXP _lapl_id, SEXP _flags ) { */
/*   hid_t lapl_id = INTEGER(_lapl_id)[0]; */
/*   unsigned flags = INTEGER(_flags)[0]; */
/*   herr_t herr = H5Pset_elink_acc_flags(hid_tlapl_id, unsignedflags); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_elink_acc_flags(hid_t lapl_id, unsigned * flags) *\/ */
/* SEXP _H5Pget_elink_acc_flags( SEXP _lapl_id, SEXP _flags ) { */
/*   hid_t lapl_id = INTEGER(_lapl_id)[0]; */
/*   TODO: unsigned * flags = _flags */
/*   herr_t herr = H5Pget_elink_acc_flags(hid_tlapl_id, unsigned *flags); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

////////////////////////////////////////////////////
// Dataset Creation Properties
////////////////////////////////////////////////////


/* herr_t H5Pset_layout(hid_t plist, H5D_layout_t layout) */
SEXP _H5Pset_layout( SEXP _plist, SEXP _layout ) {
    //hid_t plist = INTEGER(_plist)[0];
    hid_t plist = STRSXP_2_HID( _plist );
    H5D_layout_t layout = INTEGER(_layout)[0];
    herr_t herr = H5Pset_layout(plist, layout);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* H5D_layout_t H5Pget_layout(hid_t plist) */
SEXP _H5Pget_layout( SEXP _plist ) {
    //hid_t plist = INTEGER(_plist)[0];
    hid_t plist = STRSXP_2_HID( _plist );
    H5D_layout_t layout = H5Pget_layout(plist);
    SEXP Rval = ScalarInteger(layout);
    return Rval;
}

/* herr_t H5Pset_chunk(hid_t plist, int ndims, const hsize_t * dim) */
SEXP _H5Pset_chunk( SEXP _plist, SEXP _dim ) {
    //hid_t plist = INTEGER(_plist)[0];
    hid_t plist = STRSXP_2_HID( _plist );
    int ndims = length(_dim);
    hsize_t dim[ndims];
    for (int i=0; i < ndims; i++) {
        dim[i] = INTEGER(_dim)[i];
    }
    herr_t herr = H5Pset_chunk(plist, ndims, dim);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* int H5Pget_chunk(hid_t plist, int max_ndims, hsize_t * dims) */
SEXP _H5Pget_chunk( SEXP _plist ) {
    //hid_t plist = INTEGER(_plist)[0];
    hid_t plist = STRSXP_2_HID( _plist );
    hsize_t   dims[H5S_MAX_RANK];
    int rank = H5Pget_chunk(plist, H5S_MAX_RANK, dims);
    SEXP Rval = R_NilValue;
    if (rank > 0) {
        Rval = PROTECT(allocVector(INTSXP, rank));
        for (int i=0; i < rank; i++) {
            INTEGER(Rval)[i] = dims[i];
        }
        UNPROTECT(1);
    }
    return Rval;
}

/* herr_t H5Pset_deflate(hid_t plist_id, uint level) */
SEXP _H5Pset_deflate( SEXP _plist_id, SEXP _level ) {
    //hid_t plist_id = INTEGER(_plist_id)[0];
    hid_t plist_id = STRSXP_2_HID( _plist_id );
    unsigned int level = (unsigned int)INTEGER(_level)[0];
    herr_t herr = H5Pset_deflate(plist_id, level);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* herr_t H5Pset_fill_value(hid_t plist_id, hid_t type_id, const void * value) */
SEXP _H5Pset_fill_value( SEXP _plist_id, SEXP _type_id, SEXP _value ) {
    hid_t plist_id = STRSXP_2_HID( _plist_id );
    hid_t type_id = STRSXP_2_HID( _type_id );
    void * value;
    if (type_id == H5T_IEEE_F64LE) {
        value = REAL(_value);
    } else if (type_id == H5T_STD_I32LE) {
        value = INTEGER(_value);
    } else if (type_id == H5T_STD_I8LE) {
        value = LOGICAL(_value);
    } else {
        value = (void *)CHAR(STRING_ELT(_value, 0));
    }
    herr_t herr = H5Pset_fill_value(plist_id, type_id, value);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pget_fill_value(hid_t plist_id, hid_t type_id, void * value) *\/ */
/* SEXP _H5Pget_fill_value( SEXP _plist_id, SEXP _type_id ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   hid_t type_id = INTEGER(_type_id)[0]; */
/*   int value; */
/* CHECK FOR TYPE before calling */
/*   herr_t herr = H5Pget_fill_value(plist_id, type_id, &value); */
/*   SEXP Rval = R_NilValue; */
/*   if (herr >= 0) { */
/*     Rval = PROTECT(allocVector(INTSXP, rank)); */
/*     for (int i=0; i < rank; i++) { */
/*       INTEGER(Rval)[i] = dims[i]; */
/*     } */
/*     UNPROTECT(1); */
/*   } */
/*   return Rval; */

/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* herr_t H5Pfill_value_defined(hid_t plist_id, H5D_fill_value_t * status) */
SEXP _H5Pfill_value_defined( SEXP _plist_id ) {
    hid_t plist_id = STRSXP_2_HID( _plist_id );
    H5D_fill_value_t status;
    herr_t herr = H5Pfill_value_defined(plist_id, &status);
    SEXP Rval = R_NilValue;
    if (herr < 0) {
        Rval = R_NilValue;
    } else {
        Rval = ScalarLogical(status);
    }
    return Rval;
}

/* herr_t H5Pset_fill_time(hid_t plist_id, H5D_fill_time_t fill_time) */
SEXP _H5Pset_fill_time( SEXP _plist_id, SEXP _fill_time ) {
    hid_t plist_id = STRSXP_2_HID( _plist_id );
    H5D_fill_time_t fill_time = INTEGER(_fill_time)[0];
    herr_t herr = H5Pset_fill_time(plist_id, fill_time);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* herr_t H5Pget_fill_time(hid_t plist_id, H5D_fill_time_t * fill_time) */
SEXP _H5Pget_fill_time( SEXP _plist_id ) {
    hid_t plist_id = STRSXP_2_HID( _plist_id );
    H5D_fill_time_t fill_time;
    herr_t herr = H5Pget_fill_time(plist_id, &fill_time);
    SEXP Rval = R_NilValue;
    if (herr < 0) {
        Rval = R_NilValue;
    } else {
        Rval = ScalarInteger(fill_time);
    }
    return Rval;
}

/* herr_t H5Pset_alloc_time(hid_t plist_id, H5D_alloc_time_t alloc_time) */
SEXP _H5Pset_alloc_time( SEXP _plist_id, SEXP _alloc_time ) {
    hid_t plist_id = STRSXP_2_HID( _plist_id );
    H5D_alloc_time_t alloc_time = INTEGER(_alloc_time)[0];
    herr_t herr = H5Pset_alloc_time(plist_id, alloc_time);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* herr_t H5Pget_alloc_time(hid_t plist_id, H5D_alloc_time_t * alloc_time) */
SEXP _H5Pget_alloc_time( SEXP _plist_id ) {
    hid_t plist_id = STRSXP_2_HID( _plist_id );
    H5D_alloc_time_t alloc_time;
    herr_t herr = H5Pget_alloc_time(plist_id, &alloc_time);
    SEXP Rval = R_NilValue;
    if (herr < 0) {
        Rval = R_NilValue;
    } else {
        Rval = ScalarInteger(alloc_time);
    }
    return Rval;
}

/* /\* herr_t H5Pset_filter(hid_t plist_id, H5Z_filter_t filter_id, unsigned int flags, size_t cd_nelmts, const unsigned int cd_values[]) *\/ */
/* SEXP _H5Pset_filter( SEXP _plist_id, SEXP _filter_id, SEXP _flags, SEXP _cd_nelmts, SEXP _cd_values[] ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: H5Z_filter_t filter_id = _filter_id */
/*   unsigned int flags = INTEGER(_flags)[0]; */
/*   size_t cd_nelmts = INTEGER(_cd_nelmts)[0]; */
/*   const unsigned int cd_values[] = INTEGER(_cd_values[])[0]; */
/*   herr_t herr = H5Pset_filter(hid_tplist_id, H5Z_filter_tfilter_id, unsigned intflags, size_tcd_nelmts, const unsigned intcd_values[]); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

SEXP _H5Pset_filter( SEXP _plist_id, SEXP _filter_id, SEXP _mandatory, SEXP _cd_values ) {
  hid_t plist_id  = STRSXP_2_HID( _plist_id );
  H5Z_filter_t filter_id = (H5Z_filter_t) INTEGER(_filter_id)[0];
  unsigned int flags = (asLogical(_mandatory) == 0) ? H5Z_FLAG_OPTIONAL : H5Z_FLAG_MANDATORY;
  size_t cd_nelmts = (size_t) length(_cd_values);
  unsigned int *cd_values = (unsigned int *) R_alloc(sizeof(unsigned int), cd_nelmts);
  for(int i = 0; i < cd_nelmts; i++)  { cd_values[i] = INTEGER(_cd_values)[i]; }
  
  herr_t herr = H5Pset_filter(plist_id, filter_id, flags, cd_nelmts, cd_values);
  SEXP Rval = ScalarInteger(herr);
  return Rval; 
}

/* htri_t H5Pall_filters_avail(hid_t plist_id) */
SEXP _H5Pall_filters_avail( SEXP _plist_id ) {
    hid_t plist_id  = STRSXP_2_HID( _plist_id );
    htri_t htri = H5Pall_filters_avail(plist_id);
    SEXP Rval = ScalarInteger(htri);
    return Rval; 
} 

/* int H5Pget_nfilters(hid_t plist) */
SEXP _H5Pget_nfilters( SEXP _plist) {
    hid_t plist  = STRSXP_2_HID( _plist );
    int n = H5Pget_nfilters(plist);
    SEXP Rval = ScalarInteger(n);
    return Rval;
}

/* /\* H5Z_filter_t H5Pget_filter1(hid_t plist_id, unsigned int idx, unsigned int * flags, size_t * cd_nelmts, unsigned int * cd_values, size_t namelen, char name[]) *\/ */
/* SEXP _H5Pget_filter1( SEXP _plist_id, SEXP _idx, SEXP _flags, SEXP _cd_nelmts, SEXP _cd_values, SEXP _namelen, SEXP _name[] ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   unsigned int idx = INTEGER(_idx)[0]; */
/*   TODO: unsigned int * flags = _flags */
/*   TODO: size_t * cd_nelmts = _cd_nelmts */
/*   TODO: unsigned int * cd_values = _cd_values */
/*   size_t namelen = INTEGER(_namelen)[0]; */
/*   TODO: char name[] = INTEGER(_name[])[0]; */
/* H5Pget_filter1(hid_tplist_id, unsigned intidx, unsigned int *flags, size_t *cd_nelmts, unsigned int *cd_values, size_tnamelen, charname[]); */
/*   SEXP Rval = R_NilValue; */
/*   return Rval; */
/* TODO: UNKOWN RETURN VALUE */
/* } */

/* H5Z_filter_t H5Pget_filter2(hid_t plist_id, unsigned idx, unsigned int * flags, size_t * cd_nelmts, unsigned cd_values[], size_t namelen, char name[], unsigned * filter_config) */
SEXP _H5Pget_filter( SEXP _plist_id, SEXP _idx ) { 
    hid_t plist_id  = STRSXP_2_HID( _plist_id );
    unsigned idx = INTEGER(_idx)[0]; 
  
    unsigned int flags;
    /* this is an abritry value of 10 for now - MLS 25.03.19 */
    size_t cd_nelmts = 10; 
    unsigned int cd_values[10];
    size_t namelen = 50; 
    char *name = R_alloc(namelen, sizeof(char)); 
    unsigned int filter_config;

    H5Z_filter_t filt_id = H5Pget_filter2(plist_id, idx, &flags, &cd_nelmts, cd_values, namelen, name, &filter_config); 

    SEXP Rval = PROTECT(allocVector(VECSXP, 2)); 
    SET_VECTOR_ELT(Rval, 0, ScalarInteger(filt_id));
    SET_VECTOR_ELT(Rval, 1, mkString(name));
    UNPROTECT(1);
    return Rval; 
}

/* /\* herr_t H5Pget_filter_by_id1(hid_t plist_id, H5Z_filter_t filter_id, unsigned int * flags, size_t * cd_nelmts, unsigned int cd_values[], size_t namelen, char name[]) *\/ */
/* SEXP _H5Pget_filter_by_id1( SEXP _plist_id, SEXP _filter_id, SEXP _flags, SEXP _cd_nelmts, SEXP _cd_values[], SEXP _namelen, SEXP _name[] ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: H5Z_filter_t filter_id = _filter_id */
/*   TODO: unsigned int * flags = _flags */
/*   TODO: size_t * cd_nelmts = _cd_nelmts */
/*   unsigned int cd_values[] = INTEGER(_cd_values[])[0]; */
/*   size_t namelen = INTEGER(_namelen)[0]; */
/*   TODO: char name[] = INTEGER(_name[])[0]; */
/*   herr_t herr = H5Pget_filter_by_id1(hid_tplist_id, H5Z_filter_tfilter_id, unsigned int *flags, size_t *cd_nelmts, unsigned intcd_values[], size_tnamelen, charname[]); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_filter_by_id2(hid_t plist_id, H5Z_filter_t filter_id, unsigned int * flags, size_t * cd_nelmts, unsigned int cd_values[], size_t namelen, char name[], unsigned int * filter_config) *\/ */
/* SEXP _H5Pget_filter_by_id2( SEXP _plist_id, SEXP _filter_id, SEXP _flags, SEXP _cd_nelmts, SEXP _cd_values[], SEXP _namelen, SEXP _name[], SEXP _filter_config ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: H5Z_filter_t filter_id = _filter_id */
/*   TODO: unsigned int * flags = _flags */
/*   TODO: size_t * cd_nelmts = _cd_nelmts */
/*   unsigned int cd_values[] = INTEGER(_cd_values[])[0]; */
/*   size_t namelen = INTEGER(_namelen)[0]; */
/*   TODO: char name[] = INTEGER(_name[])[0]; */
/*   TODO: unsigned int * filter_config = _filter_config */
/*   herr_t herr = H5Pget_filter_by_id2(hid_tplist_id, H5Z_filter_tfilter_id, unsigned int *flags, size_t *cd_nelmts, unsigned intcd_values[], size_tnamelen, charname[], unsigned int *filter_config); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pmodify_filter(hid_t plist_id, H5Z_filter_t filter_id, unsigned int flags, size_t cd_nelmts, const unsigned int cd_values[]) *\/ */
/* SEXP _H5Pmodify_filter( SEXP _plist_id, SEXP _filter_id, SEXP _flags, SEXP _cd_nelmts, SEXP _cd_values[] ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: H5Z_filter_t filter_id = _filter_id */
/*   unsigned int flags = INTEGER(_flags)[0]; */
/*   size_t cd_nelmts = INTEGER(_cd_nelmts)[0]; */
/*   const unsigned int cd_values[] = INTEGER(_cd_values[])[0]; */
/*   herr_t herr = H5Pmodify_filter(hid_tplist_id, H5Z_filter_tfilter_id, unsigned intflags, size_tcd_nelmts, const unsigned intcd_values[]); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Premove_filter(hid_t plist_id, H5Z_filter_t filter) *\/ */
/* SEXP _H5Premove_filter( SEXP _plist_id, SEXP _filter ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: H5Z_filter_t filter = _filter */
/*   herr_t herr = H5Premove_filter(hid_tplist_id, H5Z_filter_tfilter); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_fletcher32(hid_t plist_id) *\/ */
/* SEXP _H5Pset_fletcher32( SEXP _plist_id ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   herr_t herr = H5Pset_fletcher32(hid_tplist_id); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* herr_t H5Pset_nbit(hid_t plist_id) *\/ */
SEXP _H5Pset_nbit( SEXP _plist_id ) {
  hid_t plist_id = STRSXP_2_HID( _plist_id );
  herr_t herr = H5Pset_nbit(plist_id);
  SEXP Rval = ScalarInteger(herr);
  return Rval;
} 

/* /\* herr_t H5Pset_scaleoffset(hid_t plist_id, H5Z_SO_scale_type_t scale_type, int scale_factor) *\/ */
/* SEXP _H5Pset_scaleoffset( SEXP _plist_id, SEXP _scale_type, SEXP _scale_factor ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: H5Z_SO_scale_type_t scale_type = _scale_type */
/*   int scale_factor = INTEGER(_scale_factor)[0]; */
/*   herr_t herr = H5Pset_scaleoffset(hid_tplist_id, H5Z_SO_scale_type_tscale_type, intscale_factor); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* herr_t H5Pset_shuffle(hid_t plist_id) */
SEXP _H5Pset_shuffle( SEXP _plist_id ) { 
  hid_t plist_id = STRSXP_2_HID( _plist_id );
  herr_t herr = H5Pset_shuffle(plist_id);
  SEXP Rval = ScalarInteger(herr);
  return Rval;
}

/* herr_t H5Pset_szip(hid_t plist, unsigned int options_mask, unsigned int pixels_per_block) *\/ */
SEXP _H5Pset_szip( SEXP _plist_id, SEXP _options_mask, SEXP _pixels_per_block ) {
  hid_t plist_id = STRSXP_2_HID( _plist_id );
  unsigned int options_mask = INTEGER(_options_mask)[0];
  unsigned int pixels_per_block = INTEGER(_pixels_per_block)[0];
  herr_t herr = H5Pset_szip(plist_id, options_mask, pixels_per_block);
  SEXP Rval = ScalarInteger(herr);
  return Rval;
} 

/* /\* herr_t H5Pset_external(hid_t plist, const char *name, off_t offset, hsize_t size) *\/ */
/* SEXP _H5Pset_external( SEXP _plist, SEXP _*name, SEXP _offset, SEXP _size ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   TODO: const char *name = INTEGER(_*name)[0]; */
/*   TODO: off_t offset = _offset */
/*   hsize_t size = INTEGER(_size)[0]; */
/*   herr_t herr = H5Pset_external(hid_tplist, const char*name, off_toffset, hsize_tsize); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* int H5Pget_external_count(hid_t plist) *\/ */
/* SEXP _H5Pget_external_count( SEXP _plist ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   int n = H5Pget_external_count(hid_tplist); */
/*   SEXP Rval = ScalarInteger(n); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_external(hid_t plist, unsigned idx, size_t name_size, char *name, off_t *offset, hsize_t *size) *\/ */
/* SEXP _H5Pget_external( SEXP _plist, SEXP _idx, SEXP _name_size, SEXP _*name, SEXP _*offset, SEXP _*size ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   unsigned idx = INTEGER(_idx)[0]; */
/*   size_t name_size = INTEGER(_name_size)[0]; */
/*   TODO: char *name = INTEGER(_*name)[0]; */
/*   TODO: off_t *offset = _*offset */
/*   hsize_t *size = INTEGER(_*size)[0]; */
/*   herr_t herr = H5Pget_external(hid_tplist, unsignedidx, size_tname_size, char*name, off_t*offset, hsize_t*size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

////////////////////////////////////////////////////
// Dataset Access Properties
////////////////////////////////////////////////////

/* herr_t H5Pset_chunk_cache(hid_t dapl_id, size_t rdcc_nslots, size_t rdcc_nbytes, double rdcc_w0) */
SEXP _H5Pset_chunk_cache( SEXP _dapl_id, SEXP _rdcc_nslots, SEXP _rdcc_nbytes, SEXP _rdcc_w0 ) {

    hid_t dapl_id = STRSXP_2_HID( _dapl_id );
    size_t rdcc_nslots = INTEGER(_rdcc_nslots)[0];
    size_t rdcc_nbytes = INTEGER(_rdcc_nbytes)[0];
    double rdcc_w0 = REAL(_rdcc_w0)[0];
    herr_t herr = H5Pset_chunk_cache(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0);
    SEXP Rval = ScalarInteger(herr);
    return Rval;
}

/* /\* herr_t H5Pget_chunk_cache(hid_t dapl_id, size_t * rdcc_nslots, size_t * rdcc_nbytes, double * rdcc_w0) *\/ */
/* SEXP _H5Pget_chunk_cache( SEXP _dapl_id ) { */
/*   hid_t dapl_id = INTEGER(_dapl_id)[0]; */
/*   size_t nslots, nbytes; */
/*   double w0; */
/*   herr_t herr = H5Pget_chunk_cache(dapl_id, &nslots, &nbytes, &w0); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

////////////////////////////////////////////////////
// Dataset Transfer Properties
////////////////////////////////////////////////////



/* /\* herr_t H5Pset_buffer(hid_t plist, hsize_t size, void *tconv, void *bkg) *\/ */
/* SEXP _H5Pset_buffer( SEXP _plist, SEXP _size, SEXP _*tconv, SEXP _*bkg ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   hsize_t size = INTEGER(_size)[0]; */
/*   TODO: void *tconv = _*tconv */
/*   TODO: void *bkg = _*bkg */
/*   herr_t herr = H5Pset_buffer(hid_tplist, hsize_tsize, void*tconv, void*bkg); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* hsize_t H5Pget_buffer(hid_t plist, void **tconv, void **bkg) *\/ */
/* SEXP _H5Pget_buffer( SEXP _plist, SEXP _**tconv, SEXP _**bkg ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   TODO: void **tconv = _**tconv */
/*   TODO: void **bkg = _**bkg */
/*   hsize_t n = H5Pget_buffer(hid_tplist, void**tconv, void**bkg); */
/*   SEXP Rval = ScalarInteger(n); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_preserve(hid_t plist, hbool_t status) *\/ */
/* SEXP _H5Pset_preserve( SEXP _plist, SEXP _status ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   TODO: hbool_t status = _status */
/*   herr_t herr = H5Pset_preserve(hid_tplist, hbool_tstatus); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* int H5Pget_preserve(hid_t plist) *\/ */
/* SEXP _H5Pget_preserve( SEXP _plist ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   int n = H5Pget_preserve(hid_tplist); */
/*   SEXP Rval = ScalarInteger(n); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_edc_check(hid_t plist, H5Z_EDC_t check) *\/ */
/* SEXP _H5Pset_edc_check( SEXP _plist, SEXP _check ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   TODO: H5Z_EDC_t check = _check */
/*   herr_t herr = H5Pset_edc_check(hid_tplist, H5Z_EDC_tcheck); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* H5Z_EDC_t H5Pget_edc_check(hid_t plist) *\/ */
/* SEXP _H5Pget_edc_check( SEXP _plist ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/* H5Pget_edc_check(hid_tplist); */
/*   SEXP Rval = R_NilValue; */
/*   return Rval; */
/* TODO: UNKOWN RETURN VALUE */
/* } */

/* /\* herr_t H5Pset_filter_callback(hid_t plist, H5Z_filter_func_t func, void * op_data) *\/ */
/* SEXP _H5Pset_filter_callback( SEXP _plist, SEXP _func, SEXP _op_data ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   TODO: H5Z_filter_func_t func = _func */
/*   TODO: void * op_data = _op_data */
/*   herr_t herr = H5Pset_filter_callback(hid_tplist, H5Z_filter_func_tfunc, void *op_data); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_data_transform(hid_t plist_id, const char expression) *\/ */
/* SEXP _H5Pset_data_transform( SEXP _plist_id, SEXP _expression ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: const char expression = INTEGER(_expression)[0]; */
/*   herr_t herr = H5Pset_data_transform(hid_tplist_id, const charexpression); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* ssize_t H5Pget_data_transform(hid_t plist_id, char expression, size_t size) *\/ */
/* SEXP _H5Pget_data_transform( SEXP _plist_id, SEXP _expression, SEXP _size ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: char expression = INTEGER(_expression)[0]; */
/*   size_t size = INTEGER(_size)[0]; */
/*   ssize_t s = H5Pget_data_transform(hid_tplist_id, charexpression, size_tsize); */
/*   SEXP Rval = ScalarInteger(s); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_type_conv_cb(hid_t plist, H5T_conv_except_func_t func, void * op_data) *\/ */
/* SEXP _H5Pset_type_conv_cb( SEXP _plist, SEXP _func, SEXP _op_data ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   TODO: H5T_conv_except_func_t func = _func */
/*   TODO: void * op_data = _op_data */
/*   herr_t herr = H5Pset_type_conv_cb(hid_tplist, H5T_conv_except_func_tfunc, void *op_data); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_type_conv_cb(hid_t plist, H5T_conv_except_func_t * func, void ** op_data) *\/ */
/* SEXP _H5Pget_type_conv_cb( SEXP _plist, SEXP _func, SEXP _op_data ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   TODO: H5T_conv_except_func_t * func = _func */
/*   TODO: void ** op_data = _op_data */
/*   herr_t herr = H5Pget_type_conv_cb(hid_tplist, H5T_conv_except_func_t *func, void **op_data); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_hyper_vector_size(hid_t dxpl_id, size_t vector_size) *\/ */
/* SEXP _H5Pset_hyper_vector_size( SEXP _dxpl_id, SEXP _vector_size ) { */
/*   hid_t dxpl_id = INTEGER(_dxpl_id)[0]; */
/*   size_t vector_size = INTEGER(_vector_size)[0]; */
/*   herr_t herr = H5Pset_hyper_vector_size(hid_tdxpl_id, size_tvector_size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_hyper_vector_size(hid_t dxpl_id, size_t * vector_size) *\/ */
/* SEXP _H5Pget_hyper_vector_size( SEXP _dxpl_id, SEXP _vector_size ) { */
/*   hid_t dxpl_id = INTEGER(_dxpl_id)[0]; */
/*   TODO: size_t * vector_size = _vector_size */
/*   herr_t herr = H5Pget_hyper_vector_size(hid_tdxpl_id, size_t *vector_size); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_btree_ratios(hid_t plist, double left, double middle, double right) *\/ */
/* SEXP _H5Pset_btree_ratios( SEXP _plist, SEXP _left, SEXP _middle, SEXP _right ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   double left = REAL(_left)[0]; */
/*   double middle = REAL(_middle)[0]; */
/*   double right = REAL(_right)[0]; */
/*   herr_t herr = H5Pset_btree_ratios(hid_tplist, doubleleft, doublemiddle, doubleright); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_btree_ratios(hid_t plist, double left, double middle, double right) *\/ */
/* SEXP _H5Pget_btree_ratios( SEXP _plist, SEXP _left, SEXP _middle, SEXP _right ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   double left = REAL(_left)[0]; */
/*   double middle = REAL(_middle)[0]; */
/*   double right = REAL(_right)[0]; */
/*   herr_t herr = H5Pget_btree_ratios(hid_tplist, doubleleft, doublemiddle, doubleright); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_vlen_mem_manager(hid_t plist, H5MM_allocate_t alloc, void alloc_info, H5MM_free_t free, void free_info) *\/ */
/* SEXP _H5Pset_vlen_mem_manager( SEXP _plist, SEXP _alloc, SEXP _alloc_info, SEXP _free, SEXP _free_info ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   TODO: H5MM_allocate_t alloc = _alloc */
/*   TODO: void alloc_info = _alloc_info */
/*   TODO: H5MM_free_t free = _free */
/*   TODO: void free_info = _free_info */
/*   herr_t herr = H5Pset_vlen_mem_manager(hid_tplist, H5MM_allocate_talloc, voidalloc_info, H5MM_free_tfree, voidfree_info); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_vlen_mem_manager(hid_t plist, H5MM_allocate_t alloc, void alloc_info, H5MM_free_t free, void free_info) *\/ */
/* SEXP _H5Pget_vlen_mem_manager( SEXP _plist, SEXP _alloc, SEXP _alloc_info, SEXP _free, SEXP _free_info ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   TODO: H5MM_allocate_t alloc = _alloc */
/*   TODO: void alloc_info = _alloc_info */
/*   TODO: H5MM_free_t free = _free */
/*   TODO: void free_info = _free_info */
/*   herr_t herr = H5Pget_vlen_mem_manager(hid_tplist, H5MM_allocate_talloc, voidalloc_info, H5MM_free_tfree, voidfree_info); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_dxpl_mpio(hid_t dxpl_id, H5FD_mpio_xfer_t xfer_mode) *\/ */
/* SEXP _H5Pset_dxpl_mpio( SEXP _dxpl_id, SEXP _xfer_mode ) { */
/*   hid_t dxpl_id = INTEGER(_dxpl_id)[0]; */
/*   TODO: H5FD_mpio_xfer_t xfer_mode = _xfer_mode */
/*   herr_t herr = H5Pset_dxpl_mpio(hid_tdxpl_id, H5FD_mpio_xfer_txfer_mode); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_dxpl_mpio_chunk_opt(hid_t dxpl_id, H5FD_mpio_chunk_opt_t opt_mode) *\/ */
/* SEXP _H5Pset_dxpl_mpio_chunk_opt( SEXP _dxpl_id, SEXP _opt_mode ) { */
/*   hid_t dxpl_id = INTEGER(_dxpl_id)[0]; */
/*   TODO: H5FD_mpio_chunk_opt_t opt_mode = _opt_mode */
/*   herr_t herr = H5Pset_dxpl_mpio_chunk_opt(hid_tdxpl_id, H5FD_mpio_chunk_opt_topt_mode); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_dxpl_mpio_chunk_opt_num(hid_t dxpl_id, unsigned num_chunk_per_proc) *\/ */
/* SEXP _H5Pset_dxpl_mpio_chunk_opt_num( SEXP _dxpl_id, SEXP _num_chunk_per_proc ) { */
/*   hid_t dxpl_id = INTEGER(_dxpl_id)[0]; */
/*   unsigned num_chunk_per_proc = INTEGER(_num_chunk_per_proc)[0]; */
/*   herr_t herr = H5Pset_dxpl_mpio_chunk_opt_num(hid_tdxpl_id, unsignednum_chunk_per_proc); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_dxpl_mpio_chunk_opt_ratio(hid_t dxpl_id, unsigned percent_proc_per_chunk) *\/ */
/* SEXP _H5Pset_dxpl_mpio_chunk_opt_ratio( SEXP _dxpl_id, SEXP _percent_proc_per_chunk ) { */
/*   hid_t dxpl_id = INTEGER(_dxpl_id)[0]; */
/*   unsigned percent_proc_per_chunk = INTEGER(_percent_proc_per_chunk)[0]; */
/*   herr_t herr = H5Pset_dxpl_mpio_chunk_opt_ratio(hid_tdxpl_id, unsignedpercent_proc_per_chunk); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_dxpl_mpio_collective_opt(hid_t dxpl_id, H5FD_mpio_collective_opt_t opt_mode) *\/ */
/* SEXP _H5Pset_dxpl_mpio_collective_opt( SEXP _dxpl_id, SEXP _opt_mode ) { */
/*   hid_t dxpl_id = INTEGER(_dxpl_id)[0]; */
/*   TODO: H5FD_mpio_collective_opt_t opt_mode = _opt_mode */
/*   herr_t herr = H5Pset_dxpl_mpio_collective_opt(hid_tdxpl_id, H5FD_mpio_collective_opt_topt_mode); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_dxpl_mpio(hid_t dxpl_id, H5FD_mpio_xfer_t * xfer_mode) *\/ */
/* SEXP _H5Pget_dxpl_mpio( SEXP _dxpl_id, SEXP _xfer_mode ) { */
/*   hid_t dxpl_id = INTEGER(_dxpl_id)[0]; */
/*   TODO: H5FD_mpio_xfer_t * xfer_mode = _xfer_mode */
/*   herr_t herr = H5Pget_dxpl_mpio(hid_tdxpl_id, H5FD_mpio_xfer_t *xfer_mode); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_mpio_actual_chunk_opt_mode(hid_t dxpl_id, H5D_mpio_actual_chunk_opt_mode_t * actual_chunk_opt_mode) *\/ */
/* SEXP _H5Pget_mpio_actual_chunk_opt_mode( SEXP _dxpl_id, SEXP _actual_chunk_opt_mode ) { */
/*   hid_t dxpl_id = INTEGER(_dxpl_id)[0]; */
/*   TODO: H5D_mpio_actual_chunk_opt_mode_t * actual_chunk_opt_mode = _actual_chunk_opt_mode */
/*   herr_t herr = H5Pget_mpio_actual_chunk_opt_mode(hid_tdxpl_id, H5D_mpio_actual_chunk_opt_mode_t *actual_chunk_opt_mode); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_mpio_actual_io_mode(hid_t dxpl_id, H5D_mpio_actual_io_mode_t * actual_io_mode) *\/ */
/* SEXP _H5Pget_mpio_actual_io_mode( SEXP _dxpl_id, SEXP _actual_io_mode ) { */
/*   hid_t dxpl_id = INTEGER(_dxpl_id)[0]; */
/*   TODO: H5D_mpio_actual_io_mode_t * actual_io_mode = _actual_io_mode */
/*   herr_t herr = H5Pget_mpio_actual_io_mode(hid_tdxpl_id, H5D_mpio_actual_io_mode_t *actual_io_mode); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_mpio_no_collective_cause(hid_t dxpl_id, uint32_t * local_no_collective_cause, uint32_t * global_no_collective_cause) *\/ */
/* SEXP _H5Pget_mpio_no_collective_cause( SEXP _dxpl_id, SEXP _local_no_collective_cause, SEXP _global_no_collective_cause ) { */
/*   hid_t dxpl_id = INTEGER(_dxpl_id)[0]; */
/*   TODO: uint32_t * local_no_collective_cause = _local_no_collective_cause */
/*   TODO: uint32_t * global_no_collective_cause = _global_no_collective_cause */
/*   herr_t herr = H5Pget_mpio_no_collective_cause(hid_tdxpl_id, uint32_t *local_no_collective_cause, uint32_t *global_no_collective_cause); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */


////////////////////////////////////////////////////
// Object Creation Properties
////////////////////////////////////////////////////

/* /\* herr_t H5Pset_create_intermediate_group(hid_t lcpl_id, unsigned crt_intermed_group) *\/ */
/* SEXP _H5Pset_create_intermediate_group( SEXP _lcpl_id, SEXP _crt_intermed_group ) { */
/*   hid_t lcpl_id = INTEGER(_lcpl_id)[0]; */
/*   unsigned crt_intermed_group = INTEGER(_crt_intermed_group)[0]; */
/*   herr_t herr = H5Pset_create_intermediate_group(hid_tlcpl_id, unsignedcrt_intermed_group); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_create_intermediate_group(hid_t lcpl_id, unsigned * crt_intermed_group) *\/ */
/* SEXP _H5Pget_create_intermediate_group( SEXP _lcpl_id, SEXP _crt_intermed_group ) { */
/*   hid_t lcpl_id = INTEGER(_lcpl_id)[0]; */
/*   TODO: unsigned * crt_intermed_group = _crt_intermed_group */
/*   herr_t herr = H5Pget_create_intermediate_group(hid_tlcpl_id, unsigned *crt_intermed_group); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* herr_t H5Pset_obj_track_times(hid_t ocpl_id, hbool_t track_times) *\/ */
SEXP _H5Pset_obj_track_times( SEXP _ocpl_id, SEXP _track_times ) {
  hid_t ocpl_id = STRSXP_2_HID( _ocpl_id );
  hbool_t track_times = INTEGER(_track_times)[0];
  herr_t herr = H5Pset_obj_track_times(ocpl_id, track_times);
  SEXP Rval = ScalarInteger(herr);
  return Rval;
}

/* herr_t H5Pget_obj_track_times(hid_t ocpl_id, hbool_t track_times) */
SEXP _H5Pget_obj_track_times( SEXP _ocpl_id ) { 
    
    hid_t ocpl_id = STRSXP_2_HID( _ocpl_id );
    hbool_t track_times;
    SEXP Rval;
    
    herr_t herr = H5Pget_obj_track_times(ocpl_id, &track_times);
    
    if (herr < 0) {
        Rval = R_NilValue;
    } else {
        Rval = ScalarLogical(track_times);
    }
    return Rval;
} 

/* /\* herr_t H5Pset_attr_phase_change(hid_t ocpl_id, unsigned max_compact, unsigned min_dense) *\/ */
/* SEXP _H5Pset_attr_phase_change( SEXP _ocpl_id, SEXP _max_compact, SEXP _min_dense ) { */
/*   hid_t ocpl_id = INTEGER(_ocpl_id)[0]; */
/*   unsigned max_compact = INTEGER(_max_compact)[0]; */
/*   unsigned min_dense = INTEGER(_min_dense)[0]; */
/*   herr_t herr = H5Pset_attr_phase_change(hid_tocpl_id, unsignedmax_compact, unsignedmin_dense); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_attr_phase_change(hid_t ocpl_id, unsigned max_compact, unsigned min_dense) *\/ */
/* SEXP _H5Pget_attr_phase_change( SEXP _ocpl_id, SEXP _max_compact, SEXP _min_dense ) { */
/*   hid_t ocpl_id = INTEGER(_ocpl_id)[0]; */
/*   unsigned max_compact = INTEGER(_max_compact)[0]; */
/*   unsigned min_dense = INTEGER(_min_dense)[0]; */
/*   herr_t herr = H5Pget_attr_phase_change(hid_tocpl_id, unsignedmax_compact, unsignedmin_dense); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_attr_creation_order(hid_t ocpl_id, unsigned crt_order_flags) *\/ */
/* SEXP _H5Pset_attr_creation_order( SEXP _ocpl_id, SEXP _crt_order_flags ) { */
/*   hid_t ocpl_id = INTEGER(_ocpl_id)[0]; */
/*   unsigned crt_order_flags = INTEGER(_crt_order_flags)[0]; */
/*   herr_t herr = H5Pset_attr_creation_order(hid_tocpl_id, unsignedcrt_order_flags); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_attr_creation_order(hid_t ocpl_id, unsigned crt_order_flags) *\/ */
/* SEXP _H5Pget_attr_creation_order( SEXP _ocpl_id, SEXP _crt_order_flags ) { */
/*   hid_t ocpl_id = INTEGER(_ocpl_id)[0]; */
/*   unsigned crt_order_flags = INTEGER(_crt_order_flags)[0]; */
/*   herr_t herr = H5Pget_attr_creation_order(hid_tocpl_id, unsignedcrt_order_flags); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

////////////////////////////////////////////////////
// Object Copy Properties
////////////////////////////////////////////////////


/* /\* herr_t H5Pset_copy_object(hid_t ocpypl_id, unsigned copy_options) *\/ */
/* SEXP _H5Pset_copy_object( SEXP _ocpypl_id, SEXP _copy_options ) { */
/*   hid_t ocpypl_id = INTEGER(_ocpypl_id)[0]; */
/*   unsigned copy_options = INTEGER(_copy_options)[0]; */
/*   herr_t herr = H5Pset_copy_object(hid_tocpypl_id, unsignedcopy_options); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_copy_object(hid_t ocp_plist_id, unsigned * copy_options) *\/ */
/* SEXP _H5Pget_copy_object( SEXP _ocp_plist_id, SEXP _copy_options ) { */
/*   hid_t ocp_plist_id = INTEGER(_ocp_plist_id)[0]; */
/*   TODO: unsigned * copy_options = _copy_options */
/*   herr_t herr = H5Pget_copy_object(hid_tocp_plist_id, unsigned *copy_options); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Padd_merge_committed_dtype_path(hid_t ocpypl_id, char * path) *\/ */
/* SEXP _H5Padd_merge_committed_dtype_path( SEXP _ocpypl_id, SEXP _path ) { */
/*   hid_t ocpypl_id = INTEGER(_ocpypl_id)[0]; */
/*   char * path = CHAR(STRING_ELT(_path, 0)); */
/*   herr_t herr = H5Padd_merge_committed_dtype_path(hid_tocpypl_id, char *path); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pfree_merge_committed_dtype_paths(hid_t ocpypl_id) *\/ */
/* SEXP _H5Pfree_merge_committed_dtype_paths( SEXP _ocpypl_id ) { */
/*   hid_t ocpypl_id = INTEGER(_ocpypl_id)[0]; */
/*   herr_t herr = H5Pfree_merge_committed_dtype_paths(hid_tocpypl_id); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset_mcdt_search_cb(hid_t ocpypl_id, H5O_mcdt_search_cb_t func, void * op_data) *\/ */
/* SEXP _H5Pset_mcdt_search_cb( SEXP _ocpypl_id, SEXP _func, SEXP _op_data ) { */
/*   hid_t ocpypl_id = INTEGER(_ocpypl_id)[0]; */
/*   TODO: H5O_mcdt_search_cb_t func = _func */
/*   TODO: void * op_data = _op_data */
/*   herr_t herr = H5Pset_mcdt_search_cb(hid_tocpypl_id, H5O_mcdt_search_cb_tfunc, void *op_data); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_mcdt_search_cb(hid_t ocpypl_id, H5O_mcdt_search_cb_t * func, void ** op_data) *\/ */
/* SEXP _H5Pget_mcdt_search_cb( SEXP _ocpypl_id, SEXP _func, SEXP _op_data ) { */
/*   hid_t ocpypl_id = INTEGER(_ocpypl_id)[0]; */
/*   TODO: H5O_mcdt_search_cb_t * func = _func */
/*   TODO: void ** op_data = _op_data */
/*   herr_t herr = H5Pget_mcdt_search_cb(hid_tocpypl_id, H5O_mcdt_search_cb_t *func, void **op_data); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */


////////////////////////////////////////////////////
// Attribute Creation Properties
////////////////////////////////////////////////////

/* /\* herr_t H5Pset_char_encoding(hid_t plist_id, H5T_cset_t encoding) *\/ */
/* SEXP _H5Pset_char_encoding( SEXP _plist_id, SEXP _encoding ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: H5T_cset_t encoding = _encoding */
/*   herr_t herr = H5Pset_char_encoding(hid_tplist_id, H5T_cset_tencoding); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_char_encoding(hid_t plist_id, H5T_cset_t encoding) *\/ */
/* SEXP _H5Pget_char_encoding( SEXP _plist_id, SEXP _encoding ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: H5T_cset_t encoding = _encoding */
/*   herr_t herr = H5Pget_char_encoding(hid_tplist_id, H5T_cset_tencoding); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */


////////////////////////////////////////////////////
// Generic Property Operations (Advanced)
////////////////////////////////////////////////////

/* /\* hid_t H5Pcreate_class(hid_t parent_class, const char * name, H5P_cls_create_func_t create, void * create_data, H5P_cls_copy_func_t copy, void * copy_data, H5P_cls_close_func_t close, void * close_data) *\/ */
/* SEXP _H5Pcreate_class( SEXP _parent_class, SEXP _name, SEXP _create, SEXP _create_data, SEXP _copy, SEXP _copy_data, SEXP _close, SEXP _close_data ) { */
/*   hid_t parent_class = INTEGER(_parent_class)[0]; */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   TODO: H5P_cls_create_func_t create = _create */
/*   TODO: void * create_data = _create_data */
/*   TODO: H5P_cls_copy_func_t copy = _copy */
/*   TODO: void * copy_data = _copy_data */
/*   TODO: H5P_cls_close_func_t close = _close */
/*   TODO: void * close_data = _close_data */
/*   hid_t hid = H5Pcreate_class(hid_tparent_class, const char *name, H5P_cls_create_func_tcreate, void *create_data, H5P_cls_copy_func_tcopy, void *copy_data, H5P_cls_close_func_tclose, void *close_data); */
/*   addHandle(hid); */
/*   SEXP Rval = ScalarInteger(hid); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pregister1(hid_t class, const char * name, size_t size, void * default, H5P_prp_create_func_t create, H5P_prp_set_func_t set, H5P_prp_get_func_t get, H5P_prp_delete_func_t delete, H5P_prp_copy_func_t copy, H5P_prp_close_func_t close) *\/ */
/* SEXP _H5Pregister1( SEXP _class, SEXP _name, SEXP _size, SEXP _default, SEXP _create, SEXP _set, SEXP _get, SEXP _delete, SEXP _copy, SEXP _close ) { */
/*   hid_t class = INTEGER(_class)[0]; */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   size_t size = INTEGER(_size)[0]; */
/*   TODO: void * default = _default */
/*   TODO: H5P_prp_create_func_t create = _create */
/*   TODO: H5P_prp_set_func_t set = _set */
/*   TODO: H5P_prp_get_func_t get = _get */
/*   TODO: H5P_prp_delete_func_t delete = _delete */
/*   TODO: H5P_prp_copy_func_t copy = _copy */
/*   TODO: H5P_prp_close_func_t close = _close */
/*   herr_t herr = H5Pregister1(hid_tclass, const char *name, size_tsize, void *default, H5P_prp_create_func_tcreate, H5P_prp_set_func_tset, H5P_prp_get_func_tget, H5P_prp_delete_func_tdelete, H5P_prp_copy_func_tcopy, H5P_prp_close_func_tclose); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pregister2(hid_t class, const char * name, size_t size, void * default, H5P_prp_create_func_t create, H5P_prp_set_func_t set, H5P_prp_get_func_t get, H5P_prp_delete_func_t delete, H5P_prp_copy_func_t copy, H5P_prp_compare_func_t compare, H5P_prp_close_func_t close) *\/ */
/* SEXP _H5Pregister2( SEXP _class, SEXP _name, SEXP _size, SEXP _default, SEXP _create, SEXP _set, SEXP _get, SEXP _delete, SEXP _copy, SEXP _compare, SEXP _close ) { */
/*   hid_t class = INTEGER(_class)[0]; */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   size_t size = INTEGER(_size)[0]; */
/*   TODO: void * default = _default */
/*   TODO: H5P_prp_create_func_t create = _create */
/*   TODO: H5P_prp_set_func_t set = _set */
/*   TODO: H5P_prp_get_func_t get = _get */
/*   TODO: H5P_prp_delete_func_t delete = _delete */
/*   TODO: H5P_prp_copy_func_t copy = _copy */
/*   TODO: H5P_prp_compare_func_t compare = _compare */
/*   TODO: H5P_prp_close_func_t close = _close */
/*   herr_t herr = H5Pregister2(hid_tclass, const char *name, size_tsize, void *default, H5P_prp_create_func_tcreate, H5P_prp_set_func_tset, H5P_prp_get_func_tget, H5P_prp_delete_func_tdelete, H5P_prp_copy_func_tcopy, H5P_prp_compare_func_tcompare, H5P_prp_close_func_tclose); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pinsert1(hid_t plid, const char * name, size_t size, void * value, H5P_prp_set_func_t set, H5P_prp_get_func_t get, H5P_prp_delete_func_t delete, H5P_prp_copy_func_t copy, H5P_prp_close_func_t close) *\/ */
/* SEXP _H5Pinsert1( SEXP _plid, SEXP _name, SEXP _size, SEXP _value, SEXP _set, SEXP _get, SEXP _delete, SEXP _copy, SEXP _close ) { */
/*   hid_t plid = INTEGER(_plid)[0]; */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   size_t size = INTEGER(_size)[0]; */
/*   TODO: void * value = _value */
/*   TODO: H5P_prp_set_func_t set = _set */
/*   TODO: H5P_prp_get_func_t get = _get */
/*   TODO: H5P_prp_delete_func_t delete = _delete */
/*   TODO: H5P_prp_copy_func_t copy = _copy */
/*   TODO: H5P_prp_close_func_t close = _close */
/*   herr_t herr = H5Pinsert1(hid_tplid, const char *name, size_tsize, void *value, H5P_prp_set_func_tset, H5P_prp_get_func_tget, H5P_prp_delete_func_tdelete, H5P_prp_copy_func_tcopy, H5P_prp_close_func_tclose); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pinsert2(hid_t plid, const char * name, size_t size, void * value, H5P_prp_set_func_t set, H5P_prp_get_func_t get, H5P_prp_delete_func_t delete, H5P_prp_copy_func_t copy, H5P_prp_compare_func_t compare, H5P_prp_close_func_t close) *\/ */
/* SEXP _H5Pinsert2( SEXP _plid, SEXP _name, SEXP _size, SEXP _value, SEXP _set, SEXP _get, SEXP _delete, SEXP _copy, SEXP _compare, SEXP _close ) { */
/*   hid_t plid = INTEGER(_plid)[0]; */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   size_t size = INTEGER(_size)[0]; */
/*   TODO: void * value = _value */
/*   TODO: H5P_prp_set_func_t set = _set */
/*   TODO: H5P_prp_get_func_t get = _get */
/*   TODO: H5P_prp_delete_func_t delete = _delete */
/*   TODO: H5P_prp_copy_func_t copy = _copy */
/*   TODO: H5P_prp_compare_func_t compare = _compare */
/*   TODO: H5P_prp_close_func_t close = _close */
/*   herr_t herr = H5Pinsert2(hid_tplid, const char *name, size_tsize, void *value, H5P_prp_set_func_tset, H5P_prp_get_func_tget, H5P_prp_delete_func_tdelete, H5P_prp_copy_func_tcopy, H5P_prp_compare_func_tcompare, H5P_prp_close_func_tclose); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pset(hid_t plid, const char * name, void * value) *\/ */
/* SEXP _H5Pset( SEXP _plid, SEXP _name, SEXP _value ) { */
/*   hid_t plid = INTEGER(_plid)[0]; */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   TODO: void * value = _value */
/*   herr_t herr = H5Pset(hid_tplid, const char *name, void *value); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* htri_t H5Pexist(hid_t id, const char * name) *\/ */
/* SEXP _H5Pexist( SEXP _id, SEXP _name ) { */
/*   hid_t id = INTEGER(_id)[0]; */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   htri_t htri = H5Pexist(hid_tid, const char *name); */
/*   SEXP Rval = ScalarInteger(htri); */
/*   return Rval; */
/* } */

/* /\* int H5Pget_size(hid_t id, const char * name, size_t * size) *\/ */
/* SEXP _H5Pget_size( SEXP _id, SEXP _name, SEXP _size ) { */
/*   hid_t id = INTEGER(_id)[0]; */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   TODO: size_t * size = _size */
/*   int n = H5Pget_size(hid_tid, const char *name, size_t *size); */
/*   SEXP Rval = ScalarInteger(n); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget_nprops(hid_t plist_id, size_t * nprops) *\/ */
/* SEXP _H5Pget_nprops( SEXP _plist_id, SEXP _nprops ) { */
/*   hid_t plist_id = INTEGER(_plist_id)[0]; */
/*   TODO: size_t * nprops = _nprops */
/*   herr_t herr = H5Pget_nprops(hid_tplist_id, size_t *nprops); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* char * H5Pget_class_name(hid_t pcid) *\/ */
/* SEXP _H5Pget_class_name( SEXP _pcid ) { */
/*   hid_t pcid = INTEGER(_pcid)[0]; */
/* H5Pget_class_name(hid_tpcid); */
/*   SEXP Rval = R_NilValue; */
/*   return Rval; */
/* TODO: UNKOWN RETURN VALUE */
/* } */

/* /\* hid_t H5Pget_class_parent(hid_t pcid) *\/ */
/* SEXP _H5Pget_class_parent( SEXP _pcid ) { */
/*   hid_t pcid = INTEGER(_pcid)[0]; */
/*   hid_t hid = H5Pget_class_parent(hid_tpcid); */
/*   addHandle(hid); */
/*   SEXP Rval = ScalarInteger(hid); */
/*   return Rval; */
/* } */

/* /\* htri_t H5Pisa_class(hid_t plist, hid_t pclass) *\/ */
/* SEXP _H5Pisa_class( SEXP _plist, SEXP _pclass ) { */
/*   hid_t plist = INTEGER(_plist)[0]; */
/*   hid_t pclass = INTEGER(_pclass)[0]; */
/*   htri_t htri = H5Pisa_class(hid_tplist, hid_tpclass); */
/*   SEXP Rval = ScalarInteger(htri); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pget(hid_t plid, const char * name, void * value) *\/ */
/* SEXP _H5Pget( SEXP _plid, SEXP _name, SEXP _value ) { */
/*   hid_t plid = INTEGER(_plid)[0]; */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   TODO: void * value = _value */
/*   herr_t herr = H5Pget(hid_tplid, const char *name, void *value); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* htri_t H5Pequal(hid_t id1, hid_t id2) */
SEXP _H5Pequal( SEXP _id1, SEXP _id2 ) {
    hid_t id1 = STRSXP_2_HID( _id1 );
    hid_t id2 = STRSXP_2_HID( _id2 );
    htri_t htri = H5Pequal(id1, id2);
    SEXP Rval = ScalarInteger(htri);
    return Rval;
}

/* /\* int H5Piterate(hid_t id, int * idx, H5P_iterate_t iter_func, void * iter_data) *\/ */
/* SEXP _H5Piterate( SEXP _id, SEXP _idx, SEXP _iter_func, SEXP _iter_data ) { */
/*   hid_t id = INTEGER(_id)[0]; */
/*   TODO: int * idx = _idx */
/*   TODO: H5P_iterate_t iter_func = _iter_func */
/*   TODO: void * iter_data = _iter_data */
/*   int n = H5Piterate(hid_tid, int *idx, H5P_iterate_titer_func, void *iter_data); */
/*   SEXP Rval = ScalarInteger(n); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Pcopy_prop(hid_t dst_id, hid_t src_id, const char * name) *\/ */
/* SEXP _H5Pcopy_prop( SEXP _dst_id, SEXP _src_id, SEXP _name ) { */
/*   hid_t dst_id = INTEGER(_dst_id)[0]; */
/*   hid_t src_id = INTEGER(_src_id)[0]; */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   herr_t herr = H5Pcopy_prop(hid_tdst_id, hid_tsrc_id, const char *name); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Premove(hid_t plid, const char * name) *\/ */
/* SEXP _H5Premove( SEXP _plid, SEXP _name ) { */
/*   hid_t plid = INTEGER(_plid)[0]; */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   herr_t herr = H5Premove(hid_tplid, const char *name); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* /\* herr_t H5Punregister(H5P_class_t class, const char * name) *\/ */
/* SEXP _H5Punregister( SEXP _class, SEXP _name ) { */
/*   TODO: H5P_class_t class = _class */
/*   const char * name = CHAR(STRING_ELT(_name, 0)); */
/*   herr_t herr = H5Punregister(H5P_class_tclass, const char *name); */
/*   SEXP Rval = ScalarInteger(herr); */
/*   return Rval; */
/* } */

/* herr_t H5Pclose_class( hid_t class ) */
SEXP _H5Pclose_class( SEXP _class ) {
    hid_t class = STRSXP_2_HID( _class );
    herr_t herr = H5Pclose_class( class );
    if (herr == 0) {
        removeHandle(class);
    }
    
    SEXP Rval;
    PROTECT(Rval = allocVector(INTSXP, 1));
    INTEGER(Rval)[0] = herr;
    UNPROTECT(1);
    return Rval;
}


