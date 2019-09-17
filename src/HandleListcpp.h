#ifndef _HandleListCPP_H
#define _HandleListCPP_H

#include <hdf5.h>

void addHandleCPP ( hid_t id );

hsize_t idListSizeCPP ( );

hsize_t validIdentifierCPP ( hid_t *validIDs, hsize_t size );

void removeHandleCPP ( hid_t id );

#endif

