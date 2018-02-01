#ifndef HandleList_HPP
#define HandleList_HPP

#include <map>
#include <set>
#include <vector>
#include <string>
#include <hdf5.h>

//extern "C" {
//#include "HandleList.h"
//}

// extern "C" {
//   void addHandle(hid_t ID);

//   void removeHandle( hid_t fid );

// }

using namespace std;

class HandleList {
private :
  std::set<hid_t> handleList_;

public :
  inline void addHandle( hid_t id ) {
    if (id >= 0) {
      handleList_.insert(id);
    }
  };

  inline void removeHandle( hid_t id ) {
    handleList_.erase(id);
  };

  inline long size ( ) {
    return handleList_.size();
  };

  inline hsize_t validIdentifier( hid_t *validIDs, hsize_t size ) {
    hid_t id;
    std::vector<hid_t> invalidIDs;
    std::set<hid_t>::iterator it;
    hsize_t i = 0;
    for (it = handleList_.begin(); it != handleList_.end(); it++) {
      id = (*it);
      if (H5Iis_valid(id)) {
	if (i < size) {
	  validIDs[i] = id;
	  i++;
	}
      } else {
	invalidIDs.push_back(id);
      }
    }
    std::vector<hid_t>::iterator it2;
    if (invalidIDs.size() > 0) {
      for (it2 = invalidIDs.begin(); it2 != invalidIDs.end(); it2++) {
	handleList_.erase(*it2);
      }
    }

    return i;
  };

  inline static HandleList& Instance() {
    static HandleList instance;
    return instance;
  }

private : 
  inline explicit HandleList() {}
  inline ~HandleList() {}
};

#endif // ifndef HandleList_HPP
