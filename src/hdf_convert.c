#include "common.h"

herr_t double2int (hid_t src, hid_t dst, H5T_cdata_t *cdata,
                 size_t nelmts, size_t buf_stride, size_t bkg_stride,
    void *buf, void *background, hid_t dset_xfer_plist)
{
    unsigned char *lsrc = (unsigned char *)buf;
    unsigned char *ldst = lsrc;
    int i;
 
    switch (cdata->command) {
    case H5T_CONV_INIT:
	if (H5Tequal (src, H5T_NATIVE_DOUBLE) &&
	    H5Tequal( dst, H5T_NATIVE_INT) )
	    i = 0;
	else
	    i = -1;
	/*cdata->need_bkg = H5T_BKG_YES; */
	return i;
    case H5T_CONV_FREE:
	break;
    case H5T_CONV_CONV:
	for (i=0; i<nelmts; i++)
	    ldst[i] = (int) lsrc[i];
	break;
    default:
	return -1;
    }
    return 0;
}
