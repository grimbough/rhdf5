#include "H5E.h"
#include <stdlib.h>
#include <time.h>

SEXP _h5errorHandling( SEXP _type ) {
  H5E_auto_t err_func;
  void *err_func_data;
  int type = INTEGER(_type)[0];
  if (type == 0) {
    H5Eset_auto (H5E_DEFAULT, 0, 0);
  } else {
    if (type == 1) {
      H5Eget_auto (H5E_DEFAULT, &err_func, &err_func_data);
      H5E_auto_t myfct = &_rhdf5PrintErrorRcompact;
      H5Eset_auto (H5E_DEFAULT, myfct, &err_func_data);
    } else {
      if (type == 2) {
	H5Eget_auto (H5E_DEFAULT, &err_func, &err_func_data);
	H5E_auto_t myfct = &_rhdf5PrintErrorR;
	H5Eset_auto (H5E_DEFAULT, myfct, &err_func_data);
      } else {
	if (type == 3) {
	  error("unknown type");
	} else {
	  error("unknown type");
	}
      }
    }
  }
  SEXP Rval = ScalarInteger(0);
  return Rval;
}

#define MSG_SIZE       64

struct DataCollector {
  int n;
  char txt[401][1024];
};

herr_t
custom_print_cb(unsigned n, const H5E_error2_t *err_desc, void* client_data)
{
    /* FILE		*stream  = (FILE *)client_data; */
    char                maj[MSG_SIZE];
    char                min[MSG_SIZE];
    char                cls[MSG_SIZE];
    const int		indent = 4;

    /* Get descriptions for the major and minor error numbers */
    if(H5Eget_class_name(err_desc->cls_id, cls, MSG_SIZE)<0)
      error("test error 1");

    if(H5Eget_msg(err_desc->maj_num, NULL, maj, MSG_SIZE)<0)
      error("test error 2");

    if(H5Eget_msg(err_desc->min_num, NULL, min, MSG_SIZE)<0)
      error("test error 3");

    struct DataCollector * cd = client_data;
    if (cd->n >= 400) {
      strcpy(cd->txt[cd->n]," ... [truncated]\n");
      cd->n = cd->n + 1;
    } else {
      sprintf (cd->txt[cd->n],"%*serror #%03d: %s in %s(): line %u",
	       indent, "", n, err_desc->file_name,
	       err_desc->func_name, err_desc->line);
      sprintf (cd->txt[cd->n+1],"%*sclass: %s", indent*2, "", cls);
      sprintf (cd->txt[cd->n+2],"%*smajor: %s", indent*2, "", maj);
      sprintf (cd->txt[cd->n+3],"%*sminor: %s", indent*2, "", min);
      cd->n = cd->n + 4;
    }

    return 0;

  error:
    return -1;
}

herr_t
custom_print_cb_compact(unsigned n, const H5E_error2_t *err_desc, void* client_data)
{
    /* FILE		*stream  = (FILE *)client_data; */
    char                maj[MSG_SIZE];
    char                min[MSG_SIZE];
    char                cls[MSG_SIZE];
    const int		indent = 4;

    /* Get descriptions for the major and minor error numbers */
    if(H5Eget_class_name(err_desc->cls_id, cls, MSG_SIZE)<0)
      error("test error 1");

    if(H5Eget_msg(err_desc->maj_num, NULL, maj, MSG_SIZE)<0)
      error("test error 2");

    if(H5Eget_msg(err_desc->min_num, NULL, min, MSG_SIZE)<0)
      error("test error 3");

    struct DataCollector * cd = client_data;
    if (cd->n >= 400) {
      strcpy(cd->txt[cd->n]," ... [truncated]\n");
      cd->n = cd->n + 1;
    } else {
      if (n==0) {
	sprintf (cd->txt[cd->n],"%s. %s. %s.", cls, maj, min);
	cd->n = cd->n + 1;
      }
    }

    return 0;

  error:
    return -1;
}

herr_t _rhdf5PrintErrorR( hid_t estack_id, void * stream) {
  ssize_t s = H5Eget_num(estack_id);
  if ( s > 0) {
    struct DataCollector client_data;
    client_data.n = 0;
    herr_t eee = H5Ewalk(estack_id, H5E_WALK_DOWNWARD, &custom_print_cb, &client_data);

    int L = 0;
    for (int i=0; i<client_data.n; i++) {
      L = L + strlen(client_data.txt[i]) + 2;
    }
    if (client_data.n > 1) {
      char str[L];
      strcpy(str, "libhdf5");
      for (int i=0; i<client_data.n; i++) {
	strcat(str, "\n");
	strcat(str, client_data.txt[i]);
      }
      error(str);
    } else {
      if (client_data.n == 1) {
	error(client_data.txt[0]);
      } else {
	error("libhdf5 (no error message captured).");
      }
    }
  } else {
    error("libhdf5 (no error message captured).");
  }
  return 0;
}

herr_t _rhdf5PrintErrorRcompact( hid_t estack_id, void * stream) {
  ssize_t s = H5Eget_num(estack_id);
  if ( s > 0) {
    struct DataCollector client_data;
    client_data.n = 0;
    herr_t eee = H5Ewalk(estack_id, H5E_WALK_DOWNWARD, &custom_print_cb_compact, &client_data);

    int L = 0;
    for (int i=0; i<client_data.n; i++) {
      L = L + strlen(client_data.txt[i]) + 2;
    }
    if (client_data.n > 1) {
      char str[L];
      strcpy(str, "libhdf5");
      for (int i=0; i<client_data.n; i++) {
	strcat(str, "\n");
	strcat(str, client_data.txt[i]);
      }
      error(str);
    } else {
      if (client_data.n == 1) {
	error(client_data.txt[0]);
      } else {
	error("libhdf5 (no error message captured).");
      }
    }
  } else {
    error("libhdf5 (no error message captured).");
  }
  return 0;
}
