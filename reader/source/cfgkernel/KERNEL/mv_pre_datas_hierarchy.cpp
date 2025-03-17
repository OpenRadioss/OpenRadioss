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

#include <UTILS/win32_utils.h>  


#include "mv_pre_datas_hierarchy.h"
#include <HCDI/hcdi_multicfgkernelmgr.h>

MvPreDatasHierarchy_t::MvPreDatasHierarchy_t(MvDomain_e domain, void *cfgkernel, const string &keyword,const string &title,
					     object_type_e type,const MvSubtype_t *subtype_p,
					     const string &filename,
					     const PseudoStringList_t *user_name_list_p) :
  myDomain(domain),        
  myKeyword(keyword),
  myTitle(title),
  myFullType(type,subtype_p),
  myPreDescriptorPtr(NULL),
  myChildList(),
  myDoExpand(false), 
  myFlags(0),
  myHtype("")
{
    if (filename != "") {
        int    a_user_id = -1;
        int    a_hm_config_type = -1;
        int    a_hm_type = -1;
        short int a_id_pool = -1;
        char* a_card_image = NULL;
        if (subtype_p != NULL) {
            a_user_id = subtype_p->getUserId();
            a_hm_config_type = subtype_p->getHMConfigType();
            a_hm_type = subtype_p->getHMType();
            a_id_pool = subtype_p->getIdPool();
            a_card_image = subtype_p->getCardImage();
        }

        myPreDescriptorPtr = new MvPreDescriptor_t(filename, type, keyword, a_user_id, a_hm_config_type, a_hm_type, a_id_pool, a_card_image);

        if (filename != "") myPreDescriptorPtr->getDescriptorPtr(cfgkernel);

        CFGKernel* a_cfgkernel = (CFGKernel*)cfgkernel;

        bool a_is_user = (a_user_id >= 0);
        const MvStringList_t* a_user_name_list_p = (const MvStringList_t*)user_name_list_p;
        if (a_is_user || a_hm_config_type > 0 || a_hm_type > 0 || a_id_pool > 0 || (user_name_list_p && !a_user_name_list_p->empty())) {
            a_cfgkernel->add_user_pre_descriptor(type, myPreDescriptorPtr, user_name_list_p);
            myPreDescriptorPtr = NULL;
        }
    }
}

MvPreDatasHierarchy_t::~MvPreDatasHierarchy_t() {
  if(myPreDescriptorPtr!=NULL) delete myPreDescriptorPtr;
  for(MvPreDatasHierarchyList_t::const_iterator it=myChildList.begin();it!=myChildList.end();++it) delete (*it);
}

const MvDescriptor_t *MvPreDatasHierarchy_t::getDescriptorPtr(void *cfgkernel) const {
  if(myPreDescriptorPtr==NULL || cfgkernel == NULL) return NULL;
  return myPreDescriptorPtr->getDescriptorPtr(cfgkernel);
}

MvPreDatasHierarchy_t::MvPreDatasHierarchy_t(const MvPreDatasHierarchy_t &hier):
    myDomain(hier.getDomain()),
    myKeyword(hier.getKeyword()),
    myTitle(hier.getTitle()),
    myFullType(hier.getFullType()),
    myPreDescriptorPtr(hier.getPreDescriptorPtr()),
    myChildList(),
    myDoExpand(hier.getExpand()),
    myFlags(hier.getFlags()),
    myHtype(hier.getHtype())
{
    hier.getObjTypeList(&myObjTypeList);
}

void MvPreDatasHierarchy_t::addChild(MvPreDatasHierarchy_t *child_p) {
  myChildList.push_back(child_p);

  obj_type_e type = child_p->getType();
  myObjTypeList.insert(type);

  child_p->getObjTypeList(&myObjTypeList);
}

void MvPreDatasHierarchy_t::copyChild(MvPreDatasHierarchy_t *child) {
    int nb_child = child->getNbChildren();
    if (nb_child)
    {
        const MvPreDatasHierarchyList_t &a_child_list = child->getChildList();
        MvPreDatasHierarchyList_t::const_iterator it_begin = a_child_list.begin();
        MvPreDatasHierarchyList_t::const_iterator it_end = a_child_list.end();
        MvPreDatasHierarchyList_t::const_iterator it;
        for (it = it_begin; it != it_end; ++it)
        {
            MvPreDatasHierarchy_t *sub_child = (*it);
            MvPreDatasHierarchy_t *copy_hier = new MvPreDatasHierarchy_t(*sub_child);
            copy_hier->copyChild(sub_child);
            myChildList.push_back(copy_hier);
        }
    }
}

void MvPreDatasHierarchy_t::removeChild(const MvPreDatasHierarchy_t* child)
{
    MvPreDatasHierarchyList_t::const_iterator iter_b = myChildList.begin();
    MvPreDatasHierarchyList_t::const_iterator iter_e = myChildList.end();
    MvPreDatasHierarchyList_t::const_iterator iter;
    for (iter = iter_b; iter != iter_e; ++iter)
    {
        MvPreDatasHierarchy_t* a_val = *iter;
        if (a_val == child)
        {
            myChildList.erase(iter);
            delete a_val;
            break;
        }
    }
}

const MvPreDatasHierarchy_t *MvPreDatasHierarchy_t::search(const MvFullType_t &fulltype) const {
  const MvPreDatasHierarchy_t *a_result_p=NULL;
  //
  if(getNbChildren()==0) {
    if(getType()!= HCDI_OBJ_TYPE_NULL && fulltype<=myFullType) a_result_p=this;
  } else if(getType()== HCDI_OBJ_TYPE_NULL || fulltype<=myFullType) {
    MvPreDatasHierarchyList_t::const_iterator it_begin = myChildList.begin();
    MvPreDatasHierarchyList_t::const_iterator it_end   = myChildList.end();
    MvPreDatasHierarchyList_t::const_iterator it;
    for(it=it_begin;a_result_p==NULL && it!=it_end;++it) a_result_p=(*it)->search(fulltype);
  }
  //
  return a_result_p;
}

bool MvPreDatasHierarchy_t::findObjectType(obj_type_e type)
{
    MvObjectTypeSet_t::iterator iter = myObjTypeList.find(type);
    if(iter != myObjTypeList.end())
        return true;
    return false;
}

void MvPreDatasHierarchy_t::getObjTypeList(MvObjectTypeSet_t *set) const
{
    if(set == NULL)
        return;
    MvObjectTypeSet_t::iterator iter_b = myObjTypeList.begin();
    MvObjectTypeSet_t::iterator iter_e = myObjTypeList.end();
    MvObjectTypeSet_t::iterator iter;
    for(iter = iter_b; iter!=iter_e; ++iter)
    {
        set->insert(*iter);
    }
}

bool MvPreDatasHierarchy_t::getFlagStatus(int flag)
{
    if (myFlags & flag)
        return true;
    return false;
}

