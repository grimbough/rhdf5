
#include "HandleList.hpp"

extern "C" {
#include "HandleListcpp.h"

void
addHandleCPP( hid_t id ) {
  HandleList::Instance().addHandle( id );
}

hsize_t
idListSizeCPP ( ) {
  return HandleList::Instance().size( );
}

hsize_t
validIdentifierCPP ( hid_t *validIDs, hsize_t size ) {
  return HandleList::Instance().validIdentifier( validIDs, size );
}

void
removeHandleCPP( hid_t id ) {
  HandleList::Instance().removeHandle( id );
}

}   // extern "C"
