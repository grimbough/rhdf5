#ifndef hdf5_common_h__
#define hdf5_common_h__


#include <Rinternals.h>
#include <R_ext/RConverters.h>
#include <hdf5.h>
#ifndef WIN32
#include <unistd.h>
#define  stricmp strcasecmp
#endif

extern SEXP HDF_hid_tag;
extern SEXP HDF_SpaceDimsSymbol;
extern SEXP HDF_workfile;
extern SEXP HDF_workgroup;

#define HDF_ERROR_CONTROL herr_t (*old_func)(void*);void* old_client_data;H5Eget_auto(&old_func,&old_client_data)
#define HDF_ERRORS_OFF() H5Eset_auto(NULL,NULL)
#define HDF_ERRORS_ON()  H5Eset_auto(old_func,old_client_data);


#define STR(SE) CHAR(STRING_ELT(SE,0))


#define EPTR(SE) VECTOR_ELT((SE),0)
#define HID(SE)	(hid_t)R_ExternalPtrAddr(EPTR((SE)))
#define HREF(H)	R_MakeExternalPtr((H),HDF_hid_tag,R_NilValue)
#define IS_HID(SE) (TYPEOF(SE) == VECSXP && TYPEOF(EPTR((SE))) == EXTPTRSXP && R_ExternalPtrTag(EPTR((SE))) == HDF_hid_tag)

#define isPLIST(SE) (IS_HID((SE)) && (HID(SE) == H5P_DEFAULT || H5Pget_class(HID((SE))) != H5P_NO_CLASS))
#define isDPLIST(SE) (IS_HID((SE)) && R_ExternalPtrAddr((SE)) == NULL)
#define isFILE(SE) (IS_HID((SE)) && H5Iget_type(HID((SE))) == H5I_FILE)
#define isGROUP(SE) (IS_HID((SE)) && H5Iget_type(HID((SE))) == H5I_GROUP)
#define isSPACE(SE) (IS_HID((SE)) && H5Iget_type(HID((SE))) == H5I_DATASPACE)
#define isDATASET(SE) (IS_HID((SE)) && H5Iget_type(HID((SE))) == H5I_DATASET)
#define isMissingArg(SE) (isString((SE)) && !strcmp(STR((SE)), "R_MissingArg"))

#define hasMEMORY(SE) ( isDATASET((SE)) && (R_ExternalPtrProtected(EPTR((SE))) != R_NilValue))
#define getMEMORY(SE)   (R_ExternalPtrProtected(EPTR((SE))) )
#define setMEMORY(SE,M) (R_SetExternalPtrProtected(EPTR((SE)), (M)))

/* Useful Functions */

void addClass(SEXP obj, const char* name);
SEXP H5sexp(hid_t,const char*,void (*)(SEXP));
void setMatrixDims(SEXP obj, int ndims, hsize_t* dims);


#ifdef OLD
/* Finalizer References */
void H5Ffinalize(SEXP);
void H5Gfinalize(SEXP);
void H5Dfinalize(SEXP);
void H5Pfinalize(SEXP);
void H5Sfinalize(SEXP);
#endif
void H5finalize(SEXP);


#define H5Fsexp(H) H5sexp(H, "hdf5.file", H5finalize)
#define H5Gsexp(H) H5sexp(H, "hdf5.group", H5finalize)
#define H5Dsexp(H) H5sexp(H, "hdf5.dataset", H5finalize)
#define H5Ssexp(H) H5sexp(H, "hdf5.dataspace", H5finalize)
#define H5Psexp(H) H5sexp(H, "hdf5.proplist", H5finalize)

/* Function Prototypes */
#define R_FUNC(FN) SEXP FN

/* Function Prototypes */
R_FUNC(HDF_init)(void);
R_FUNC(HDF_is_valid)(SEXP);

R_FUNC(HDF_attr_set)(SEXP,SEXP,SEXP);
R_FUNC(HDF_attr_get)(SEXP,SEXP);
R_FUNC(HDF_attribute_get)(SEXP);

R_FUNC(HDF_dataset_addmemory(SEXP));
R_FUNC(HDF_dataset_create_simple)(SEXP,SEXP,SEXP,SEXP,SEXP);
R_FUNC(HDF_dims)(SEXP);
R_FUNC(HDF_dataset_freememory(SEXP));
R_FUNC(HDF_dataset_getmemory(SEXP));
R_FUNC(HDF_dataset_getspace)(SEXP);
R_FUNC(HDF_dataset_hasmemory(SEXP));
R_FUNC(HDF_dataset_length)(SEXP);
R_FUNC(HDF_dataset_store)(SEXP,SEXP,SEXP);
R_FUNC(HDF_dataset_mat_load)(SEXP);
R_FUNC(HDF_dataset_max)(SEXP);
R_FUNC(HDF_dataset_min)(SEXP);
R_FUNC(HDF_dataset_print)(SEXP);
R_FUNC(HDF_dataset_range)(SEXP);
R_FUNC(HDF_dataset_select)(SEXP, SEXP, SEXP);
R_FUNC(HDF_dataset_swapmemory(SEXP, SEXP));

R_FUNC(HDF_duplicate)(SEXP);

R_FUNC(HDF_space_print)(SEXP);
R_FUNC(HDF_space_store)(SEXP,SEXP,SEXP);
R_FUNC(HDF_space_load)(SEXP,SEXP,SEXP);
R_FUNC(HDF_select_hyperslab)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

R_FUNC(HDF_file_open)(SEXP,SEXP);
R_FUNC(HDF_file_create)(SEXP,SEXP,SEXP,SEXP);

R_FUNC(HDF_group_print)(SEXP);
R_FUNC(HDF_group_members)(SEXP);
R_FUNC(HDF_group_set_comment)(SEXP,SEXP,SEXP);
R_FUNC(HDF_group_get_comment)(SEXP,SEXP);
R_FUNC(HDF_group_get_info)(SEXP,SEXP);
R_FUNC(HDF_group_get_group)(SEXP,SEXP);
R_FUNC(HDF_group_get_dataset)(SEXP,SEXP);
R_FUNC(HDF_group_mkgroup)(SEXP,SEXP);
R_FUNC(HDF_group_delete)(SEXP,SEXP);
R_FUNC(HDF_group_apply)(SEXP,SEXP,SEXP,SEXP);

R_FUNC(HDF_plist_print)(SEXP);
R_FUNC(HDF_plist_default_plist)(void);
R_FUNC(HDF_plist_set_sizes)(SEXP,SEXP);
R_FUNC(HDF_plist_get_sizes)(SEXP);
R_FUNC(HDF_plist_file_create)(SEXP,SEXP);
R_FUNC(HDF_plist_file_access)(void);
R_FUNC(HDF_plist_dataset_create)(void);
R_FUNC(HDF_plist_dataset_xfer)(void);
R_FUNC(HDF_plist_mount)(void);

R_FUNC(HDF_readCEL)(SEXP, SEXP, SEXP);

R_FUNC(HDF_VectorSubset)(SEXP, SEXP);
R_FUNC(HDF_ArraySubset)(SEXP, SEXP);

/* other functions */
hid_t HDF_fileopen(char*, hid_t);
hid_t HDF_groupmkgroup(hid_t, char*);
herr_t double2int (hid_t, hid_t, H5T_cdata_t*, size_t, size_t, size_t,
		   void*, void*, hid_t);
void * HDF_convertoR(SEXP, R_CConvertInfo*, R_toCConverter*);
hid_t HDF_copydataset(hid_t, hid_t, hid_t, hid_t);

int HDF_getRtype(hid_t);
int HDF_setRtype(hid_t, int);

SEXPTYPE HDFclass2Rtype(hid_t);
hid_t Rtype2HDFtype(SEXPTYPE);


/* functions imported from R */
SEXP Rf_DropDims(SEXP);
SEXP Rf_mat2indsub(SEXP, SEXP);

#endif




