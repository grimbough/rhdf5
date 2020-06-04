/* General utility functions used by multiple other functions */
#include "utils.h"

void concatdim(char *s1, hsize_t next_dim, int index)
{
    char tmp[100];
    strncpy(tmp, s1, 100);
    
#ifdef H5_HAVE_WINDOWS
    snprintf(s1, 100, "%s%I64u%s", tmp, next_dim, index ? " x ": "");
#else
    snprintf(s1, 100, "%s%llu%s", tmp, next_dim, index ? " x " : "");
#endif
}

void concatdim_native(char *s1, hsize_t next_dim, int index)
{
    char tmp[100];
    strncpy(tmp, s1, 100);
    
#ifdef H5_HAVE_WINDOWS
    snprintf(s1, 100, "%s%s%I64u", tmp, index ? " x ": "", next_dim);
#else
    snprintf(s1, 100, "%s%s%llu", tmp, index ? " x " : "", next_dim);
#endif
}