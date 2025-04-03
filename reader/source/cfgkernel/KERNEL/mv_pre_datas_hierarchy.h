/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/
#ifndef MV_PRE_DATAS_HIERARCHY_H
#define MV_PRE_DATAS_HIERARCHY_H


#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>


#include "mv_domain.h"         
#include "mv_full_type.h"
#include "mv_pre_descriptor.h"

#include "mv_utils.h"
#include <unordered_map>
class MvPreDatasHierarchy_t;

typedef vector<MvPreDatasHierarchy_t *> MvPreDatasHierarchyList_t;


class MvPreDatasHierarchy_t {
public:  // Constructor and destructor
  MvPreDatasHierarchy_t(MvDomain_e domain, void *cfgkernel, const string &keyword,const string &title,
			object_type_e type,const MvSubtype_t *subtype_p,const string &filename="",
			const PseudoStringList_t *user_name_list_p=NULL);
  ~MvPreDatasHierarchy_t();
  MvPreDatasHierarchy_t(const MvPreDatasHierarchy_t &hier);
public:  // Accessors
  
  inline MvDomain_e          getDomain()        const { return myDomain; }
  
  inline const string       &getKeyword()       const { return myKeyword; }
  inline const string       &getTitle()         const { return myTitle; }
  inline object_type_e       getType()          const { return myFullType.getType(); }
  inline const MvSubtype_t  *getSubtypePtr()    const { return myFullType.getSubtypePtr(); }
  inline const MvFullType_t &getFullType()      const { return myFullType; }
  const MvDescriptor_t      *getDescriptorPtr(void* cfgkernel) const;
  inline bool                getExpand()        const { return myDoExpand;}
  inline void                setExpand(bool do_expand) {myDoExpand=do_expand;} 
  void getObjTypeList(MvObjectTypeSet_t *set) const;

  const string& getHtype() const { return myHtype; }
  void setHtype(string type) { myHtype = type; }

  int getFlags() const { return myFlags; }
  void setFlags(int value) { myFlags = value; }
  bool getFlagStatus(int flag);

  MvPreDescriptor_t *getPreDescriptorPtr() const {return myPreDescriptorPtr;}
public:  // Hierarchy management
  inline const MvPreDatasHierarchyList_t &getChildList() const { return myChildList; }
  inline int getNbChildren() const { return (int)(myChildList.size()); }
  void addChild(MvPreDatasHierarchy_t *child_p);
  void copyChild(MvPreDatasHierarchy_t *child);
  void removeChild(const MvPreDatasHierarchy_t* child);
  const MvPreDatasHierarchy_t *search(const MvFullType_t &fulltype) const;
  bool findObjectType(obj_type_e type);
private: // Datas
  MvDomain_e                 myDomain;           
  string                     myKeyword;
  string                     myTitle;
  string                     myHtype;
  MvFullType_t               myFullType;
  mutable MvPreDescriptor_t *myPreDescriptorPtr; 
  MvPreDatasHierarchyList_t  myChildList;
  bool                       myDoExpand;  
  MvObjectTypeSet_t          myObjTypeList;
  int                        myFlags;
};


#endif //MV_PRE_DATAS_HIERARCHY_H




