#ifndef HandleList_HPP
#define HandleList_HPP

#include <map>
#include <set>
#include <vector>
#include <string>
#include <hdf5.h>

extern "C" {
#include "HandleList.h"
}

using namespace std;

class HandleList {
private :
  std::set<hid_t> handleList_;
  // int ntype;
  // int lastKey_;

public :
  inline void addHandle( hid_t id ) {
    
    // H5Handle h;
    // h.isvalid = 1;
    // h.type = type;
    // h.ID = ID;
    // h.name = name;
    // h.group = group;
    // h.parentType = parentType;
    // h.parentID = parentID;
    // h.parentFileID = parentFileID;
    // h.dtypeID = dtypeID;
    // h.spaceID = spaceID;
    // std::map<int,H5Handle>::iterator it = handleList_[type].find( ID );
    // if (it != handleList_[type].end()) {
    //   handleList_[type].erase(it);
    // }
    if (id >= 0) {
      handleList_.insert(id);
    }
  };

  // inline void addHandle( enum HandleType type, hid_t ID, const char *name, const char *group, 
  // 			 enum HandleType parentType, hid_t parentID, int parentFileID,
  // 			 hid_t dtypeID = -1, hid_t spaceID = -1 ) {
    /*
    H5Handle h;
    h.isvalid = 1;
    h.type = type;
    h.ID = ID;
    h.name = name;
    h.group = group;
    h.parentType = parentType;
    h.parentID = parentID;
    h.parentFileID = parentFileID;
    h.dtypeID = dtypeID;
    h.spaceID = spaceID;
    std::map<int,H5Handle>::iterator it = handleList_[type].find( ID );
    if (it != handleList_[type].end()) {
      handleList_[type].erase(it);
    }
    handleList_[type].insert(pair<int, H5Handle>(ID, h ));
    */
  // };

  inline int removeHandle( hid_t id ) {
    return( handleList_.erase(id) );
    // std::set<hid_t>::iterator it = handleList_.find( id );
    // if (it == handleList_.end()) {
    // 	// printf("Not a valid file.\n");
    //   return(0);
    // } else {
    //   handleList_.erase(it);
    // }
    return(1);
  };

  // inline int removeHandle( enum HandleType type, hid_t fid ) {
  //   /*
  //   std::map<int,H5Handle>::iterator it = handleList_[type].find( fid );
  //   if (it == handleList_[type].end()) {
  // 	// printf("Not a valid file.\n");
  //     return(0);
  //   } else {
  //     handleList_[type].erase(it);
  //   }
  //   */
  //   return(1);
  // };

  // inline H5Handle info( enum HandleType type, hid_t fid ) {
  //   // printf("type = %d\n",type);
  //   // printf("size = %d\n",handleList_.size());
  //   H5Handle h;
  //   //    std::map<int,H5Handle>::iterator it = handleList_[type].find( fid );
  //   //    if (it == handleList_[type].end()) {
  //     // printf("ID not found!\n");
  //     h.isvalid = 0;
  //     //} else {
  //     // printf("ID found!\n");
  //     //  h = it->second;
  //     //}
  //   return(h);
  // };

  // inline void ls() {
  //   /*
  //   std::map<int,H5Handle>::iterator it;
  //   int i=0;
  //   for (int type=0; type < handleList_.size(); type++) {
  //     for (it = handleList_[type].begin(),i=0; it != handleList_[type].end(); it++, i++) {
  // 	printf("type %d; file %d; filename = %s; group = %s; parentType %d; parentID %d\n",
  // 	       type, it->first,
  // 	       it->second.name, it->second.group, it->second.parentType, it->second.parentID);
  //     }
  //   }
  //   */
  // };

  inline void validIdentifier( std::vector<hid_t> &validIDs ) {
    hid_t id;
    // vector<hid_t> validIDs;
    std::vector<hid_t> invalidIDs;
    std::set<hid_t>::iterator it;
    for (it = handleList_.begin(); it != handleList_.end(); it++) {
      id = (*it);
      if (H5Iis_valid(id)) {
	validIDs.push_back(id);
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
  };

  inline static HandleList& Instance() {
    static HandleList instance;
    // if (instance.handleList_.size() < instance.ntype) {
    //   instance.handleList_.resize(instance.ntype);
    // }
    return instance;
  }

private : 
  inline explicit HandleList() {}
  inline ~HandleList() {}
  /*
  inline explicit HandleList(HandleList const&) {}
  inline HandleList& operator=(HandleList const&) { return *this; }
  */
};

#endif // ifndef HandleList_HPP
