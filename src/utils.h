#ifndef _utils_H
#define _utils_H

#include <stdlib.h>
#include <string.h>
#include "myhdf5.h"

void concatdim(char *s1, hsize_t next_dim, int index);
void concatdim_native(char *s1, hsize_t next_dim, int index);

#endif