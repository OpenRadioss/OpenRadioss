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



#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <UTILS/error.h>
#include <UTILS/system_utils.h>


#include "mv_descriptor_parser.h"
#include "mv_pre_descriptor.h"
#include "mv_descriptor.h"
#include "HCDI/hcdi_multicfgkernelmgr.h"

typedef deque<string> MvStringList_t;


MvPreDescriptor_t::MvPreDescriptor_t(const string &filename,
                     object_type_e obj_type,const string &keyword, int user_id,int hm_config_type,int hm_type, short int idpool, char* card_image) :
  myObjectType(obj_type),
  myKeyword(keyword),
  myUserId(user_id),
  myHmConfigType(hm_config_type),
  myHmType(hm_type),
  myFileName(filename),
  myDescriptorPtr(NULL),
  myIdPool(idpool)
{
    myCardImage = card_image;
}

MvPreDescriptor_t::MvPreDescriptor_t() :
  myObjectType(HCDI_OBJ_TYPE_NULL),
  myUserId(-1),
  myHmConfigType(0),
  myHmType(0),
  myFileName(""),
  myDescriptorPtr(NULL),
  myIdPool(0)
{
    myCardImage = NULL;
}

MvPreDescriptor_t::~MvPreDescriptor_t() {
  if(myDescriptorPtr!=NULL) delete myDescriptorPtr;
}

const MvDescriptor_t *MvPreDescriptor_t::getDescriptorPtr(void *cfgkernel) const {
  if(myDescriptorPtr==NULL) {
    static string cfg_base=/*MV_get_config_version_dir()+*/"/";
    string a_file_path ;
    try {
      const CFGKernel* a_cfgkernel = (const CFGKernel*)cfgkernel;
      if (!a_cfgkernel) return nullptr;

      bool is_user=(myObjectType!= HCDI_OBJ_TYPE_NULL && (a_cfgkernel->is_user(myObjectType) || myUserId>=0 || myHmConfigType >0 ));
      a_file_path = a_cfgkernel->mv_get_cfg_file(myFileName);;// MV_get_cfg_file(myFileName);
      if(a_file_path=="") throw MvError_t(getMsg(0),myFileName.c_str());
      myDescriptorPtr=MvDescriptorParser_t(cfgkernel, a_file_path,myObjectType,is_user).getDescriptorPtr();
      myDescriptorPtr->setUserId(myUserId);
      myDescriptorPtr->setConfigType((unsigned int)myHmConfigType);
      myDescriptorPtr->setHmType((unsigned int)myHmType);
      myDescriptorPtr->setKeyword(myKeyword);
      myDescriptorPtr->setCardImage(myCardImage);
      myDescriptorPtr->setIdPool(myIdPool);
    } catch(MvError_t &err) {
      myDescriptorPtr=NULL;
#ifndef DEBUG
      cerr << err << endl;
#endif 
      throw err;
    }
  } 
  return myDescriptorPtr;
}

void MvPreDescriptor_t::deleteDescriptor()
{ 
    if (myDescriptorPtr != NULL) 
    { delete myDescriptorPtr; myDescriptorPtr = NULL; } 
}

/* --------- Maps (declaration) (hidden) --------- */

//static LocUserPreDescriptorMapArray_t *pdma_p[FF_LAST]= { NULL };

/* --------- Maps (implementation) (hidden) --------- */

LocUserPreDescriptorMapArray_t::LocUserPreDescriptorMapArray_t() {
}


LocUserPreDescriptorMapArray_t::~LocUserPreDescriptorMapArray_t() {
  for(int i=0;i<MV_NB_MCDS_TYPES;++i) {
    LocUserPreDescriptorIdMap_t::iterator a_it_begin = myUserPreDescriptorIdMapArray[i].begin();
    LocUserPreDescriptorIdMap_t::iterator a_it_end   = myUserPreDescriptorIdMapArray[i].end();
    LocUserPreDescriptorIdMap_t::iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
      MvPreDescriptor_t *a_pre_descr_pf=(*a_it).second;
      deleteDescrData(a_pre_descr_pf, i);
      }

    LocUserPreDescriptorIdMap_t::iterator a_it_begin_config = myUserPreDescriptorHMConfigTypeMapArray[i].begin();
    LocUserPreDescriptorIdMap_t::iterator a_it_end_config   = myUserPreDescriptorHMConfigTypeMapArray[i].end();
    LocUserPreDescriptorIdMap_t::iterator a_it_config;
    for(a_it_config=a_it_begin_config;a_it_config!=a_it_end_config;++a_it_config) {
        MvPreDescriptor_t *a_pre_descr_pf=(*a_it_config).second;
        deleteDescrData(a_pre_descr_pf, i);
    }

    LocUserPreDescriptorIdMap_t::iterator a_it_begin_hm = myUserPreDescriptorHMTypeMapArray[i].begin();
    LocUserPreDescriptorIdMap_t::iterator a_it_end_hm   = myUserPreDescriptorHMTypeMapArray[i].end();
    LocUserPreDescriptorIdMap_t::iterator a_it_hm;
    for(a_it_hm=a_it_begin_hm;a_it_hm!=a_it_end_hm;++a_it_hm) {
        MvPreDescriptor_t *a_pre_descr_pf=(*a_it_hm).second;
        deleteDescrData(a_pre_descr_pf, i);
    }

    LocUserVecPreDescriptorIdMap_t::iterator a_it_begin_hmconfig = myUserPreDescriptorHMConfigHMTypeMapArray[i].begin();
    LocUserVecPreDescriptorIdMap_t::iterator a_it_end_hmconfig   = myUserPreDescriptorHMConfigHMTypeMapArray[i].end();
    LocUserVecPreDescriptorIdMap_t::iterator a_it_hmconfig;
    for(a_it_hmconfig=a_it_begin_hmconfig;a_it_hmconfig!=a_it_end_hmconfig;++a_it_hmconfig) {

        vector<MvPreDescriptor_t *> a_vecpredesp=(*a_it_hmconfig).second;
        vector<MvPreDescriptor_t *>::iterator a_it_begin_vhmconfig = a_vecpredesp.begin();
        vector<MvPreDescriptor_t *>::iterator a_it_end_vhmconfig   = a_vecpredesp.end();
        vector<MvPreDescriptor_t *>::iterator a_it_vhmconfig;
        for(a_it_vhmconfig=a_it_begin_vhmconfig;a_it_vhmconfig!=a_it_end_vhmconfig;++a_it_vhmconfig) {
            MvPreDescriptor_t *a_pre_descr_pf=(*a_it_vhmconfig);
            deleteDescrData(a_pre_descr_pf, i);
            }
        }
    /*
    LocUserVecPreDescriptorShortIdMap_t::iterator a_it_begin2 = myUserPreDescriptorIdPoolMapArray[i].begin();
    LocUserVecPreDescriptorShortIdMap_t::iterator a_it_end2 = myUserPreDescriptorIdPoolMapArray[i].end();
    LocUserVecPreDescriptorShortIdMap_t::iterator a_it2;
    for (a_it2 = a_it_begin2; a_it2 != a_it_end2; ++a_it2) {
        vector<MvPreDescriptor_t *> a_vecpredesp = (*a_it2).second;
        vector<MvPreDescriptor_t *>::iterator a_itr_str_begin = a_vecpredesp.begin();
        vector<MvPreDescriptor_t *>::iterator a_itr_str_end = a_vecpredesp.end();
        vector<MvPreDescriptor_t *>::iterator a_itr_str;
        for (a_itr_str = a_itr_str_begin; a_itr_str != a_itr_str_end; ++a_itr_str)
        {
            MvPreDescriptor_t *a_pre_descr_pf = (*a_itr_str);
            deleteDescrData(a_pre_descr_pf, i);
        }
    }
     IDPOOL_PREDESCRIPTOR*/
    LocUserPreDescriptorStrMap_t::iterator a_it_str_begin = myUserPreDescriptorStrMapArray[i].begin();
    LocUserPreDescriptorStrMap_t::iterator a_it_str_end = myUserPreDescriptorStrMapArray[i].end();
    LocUserPreDescriptorStrMap_t::iterator a_it_str;
    for (a_it_str = a_it_str_begin; a_it_str != a_it_str_end; ++a_it_str)
    {
        MvPreDescriptor_t *a_pre_descr_pf = (*a_it_str).second;
        deleteDescrData(a_pre_descr_pf, i);
    }
  }
}


void LocUserPreDescriptorMapArray_t::addPreDescriptor(object_type_e type,MvPreDescriptor_t *pre_descr_p,
						     const PseudoStringList_t *user_name_list_p) 
{
  /*if (pre_descr_p->getIdPool() > 0)
      myUserPreDescriptorIdPoolMapArray[type][pre_descr_p->getIdPool()].push_back(pre_descr_p); IDPOOL_PREDESCRIPTOR*/
  if(pre_descr_p->getUserId() > 0)
      myUserPreDescriptorIdMapArray[type][pre_descr_p->getUserId()]=pre_descr_p;
  if(pre_descr_p->getHmConfigType() < 0 && pre_descr_p->getHmType() > 0)
      myUserPreDescriptorHMTypeMapArray[type][pre_descr_p->getHmType()]=pre_descr_p;
  else if(pre_descr_p->getHmConfigType() > 0 && pre_descr_p->getHmType() < 0)
      myUserPreDescriptorHMConfigTypeMapArray[type][pre_descr_p->getHmConfigType()]=pre_descr_p;
  else if(pre_descr_p->getHmConfigType() > 0 && pre_descr_p->getHmType() > 0)
  {
      /*
      LocUserVecPreDescriptorIdMap_t::const_iterator it=myUserPreDescriptorHMConfigHMTypeMapArray[type].find(pre_descr_p->getHmConfigType());
      if(it!=myUserPreDescriptorHMConfigHMTypeMapArray[type].end()) 
      {
          vector<MvPreDescriptor_t *> vec_pre_obj = ((*it).second);
          vec_pre_obj.push_back(pre_descr_p);
      }
      else
      {
          myUserPreDescriptorHMConfigHMTypeMapArray[type][pre_descr_p->getHmConfigType()].push_back(pre_descr_p);
      }
      */
      myUserPreDescriptorHMConfigHMTypeMapArray[type][pre_descr_p->getHmConfigType()].push_back(pre_descr_p);
  }
  const MvStringList_t *a_user_name_list_p=(const MvStringList_t *)user_name_list_p;
  if(a_user_name_list_p==NULL || a_user_name_list_p->empty()) {
    myUserPreDescriptorStrMapArray[type][pre_descr_p->getKeyword()]=pre_descr_p;
  } else {
    MvStringList_t::const_iterator a_it_begin = a_user_name_list_p->begin();
    MvStringList_t::const_iterator a_it_end   = a_user_name_list_p->end();
    MvStringList_t::const_iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) myUserPreDescriptorStrMapArray[type][*a_it]=pre_descr_p;
  }
}

const MvPreDescriptor_t *LocUserPreDescriptorMapArray_t::getPreDescriptorPtr(object_type_e type,int id) const {
  LocUserPreDescriptorIdMap_t::const_iterator it=myUserPreDescriptorIdMapArray[type].find(id);
  if(it==myUserPreDescriptorIdMapArray[type].end()) return NULL;
  return (*it).second;
}


const MvPreDescriptor_t *LocUserPreDescriptorMapArray_t::getPreDescriptorPtr(object_type_e type,int hm_config_type, int hm_type) const {

    LocUserPreDescriptorIdMap_t::const_iterator it;
    if(hm_config_type > 0 && hm_type < 0)
    {
        it=myUserPreDescriptorHMConfigTypeMapArray[type].find(hm_config_type);
        if(it==myUserPreDescriptorHMConfigTypeMapArray[type].end()) return NULL;
		return (*it).second;
    }
    else if(hm_config_type < 0 && hm_type > 0)
    {
        it=myUserPreDescriptorHMTypeMapArray[type].find(hm_type);
        if(it==myUserPreDescriptorHMTypeMapArray[type].end()) return NULL;
		return (*it).second;
    }
    else if(hm_config_type > 0 && hm_type > 0)
    {
        LocUserVecPreDescriptorIdMap_t::const_iterator a_it=myUserPreDescriptorHMConfigHMTypeMapArray[type].find(hm_config_type);
        if(a_it ==myUserPreDescriptorHMConfigHMTypeMapArray[type].end()) return NULL;

        vector<MvPreDescriptor_t *> vec_pre_obj = (*a_it).second;

        vector<MvPreDescriptor_t *>::iterator itr;
        vector<MvPreDescriptor_t *>::iterator itr_beg =  vec_pre_obj.begin();
        vector<MvPreDescriptor_t *>::iterator itr_end =  vec_pre_obj.end();

        for(itr=itr_beg;itr!=itr_end;++itr)
        {
            if((*itr)->getHmType() == hm_type)
            {
                return (*itr);
            }
        }
        return NULL;
    }
    return NULL;
}
const MvPreDescriptor_t *LocUserPreDescriptorMapArray_t::getPreDescriptorPtr(object_type_e type, int hm_config_type, int hm_type, const string &keyword) const 
{
  if (hm_config_type > 0 || hm_type > 0)
      return getPreDescriptorPtr(type, hm_config_type, hm_type);
  LocUserPreDescriptorStrMap_t::const_iterator it=myUserPreDescriptorStrMapArray[type].find(keyword);
  if(it==myUserPreDescriptorStrMapArray[type].end()) return NULL;
  return (*it).second;
}

void LocUserPreDescriptorMapArray_t::display(object_type_e type) const {
  LocUserPreDescriptorIdMap_t::const_iterator a_updim_begin = myUserPreDescriptorIdMapArray[type].begin();
  LocUserPreDescriptorIdMap_t::const_iterator a_updim_end   = myUserPreDescriptorIdMapArray[type].end();
  LocUserPreDescriptorIdMap_t::const_iterator a_updim_it;
  for(a_updim_it=a_updim_begin;a_updim_it!=a_updim_end;++a_updim_it) {
    cout << (*a_updim_it).first << " -> " << (*a_updim_it).second << endl;
  }
  LocUserPreDescriptorStrMap_t::const_iterator a_updsm_begin = myUserPreDescriptorStrMapArray[type].begin();
  LocUserPreDescriptorStrMap_t::const_iterator a_updsm_end   = myUserPreDescriptorStrMapArray[type].end();
  LocUserPreDescriptorStrMap_t::const_iterator a_updsm_it;
  for(a_updsm_it=a_updsm_begin;a_updsm_it!=a_updsm_end;++a_updsm_it) {
    cout << (*a_updsm_it).first << " -> " << (*a_updsm_it).second << endl;
  }
}

void LocUserPreDescriptorMapArray_t::deleteDescrData(MvPreDescriptor_t *pre_descr_p, int i)
{
    if (pre_descr_p == NULL)
        return;
    int hm_type = pre_descr_p->getHmType();
    int config_type = pre_descr_p->getHmConfigType();
    short int id_pool = pre_descr_p->getIdPool();
    if (hm_type > 0 && config_type > 0)
    {
        vector<MvPreDescriptor_t *> &a_loc_list = myUserPreDescriptorHMConfigHMTypeMapArray[i][config_type];
        vector<MvPreDescriptor_t *>::iterator it;
        it = find(a_loc_list.begin(), a_loc_list.end(), pre_descr_p);
        if (it != a_loc_list.end())
        {
            (*it) = NULL;
        }
    }
    else if (hm_type > 0)
    {
        myUserPreDescriptorHMTypeMapArray[i][hm_type] = NULL;
    }
    else if (config_type > 0)
    {
        myUserPreDescriptorHMConfigTypeMapArray[i][config_type] = NULL;
    }
    if (id_pool > 0)
    {
        /*vector<MvPreDescriptor_t *> &a_loc_list = myUserPreDescriptorIdPoolMapArray[i][id_pool];
        vector<MvPreDescriptor_t *>::iterator it;
        it = find(a_loc_list.begin(), a_loc_list.end(), pre_descr_p);
        if (it != a_loc_list.end())
        {
            (*it) = NULL;
        } IDPOOL_PREDESCRIPTOR*/
    }

    LocUserPreDescriptorStrMap_t::iterator a_iter_b = myUserPreDescriptorStrMapArray[i].begin();
    LocUserPreDescriptorStrMap_t::iterator a_iter_e = myUserPreDescriptorStrMapArray[i].end();
    LocUserPreDescriptorStrMap_t::iterator a_iter;
    for (a_iter = a_iter_b; a_iter != a_iter_e; ++a_iter)
    {
        MvPreDescriptor_t *a_pre_descr_p = (*a_iter).second;
        if (a_pre_descr_p == pre_descr_p)
        {
            (*a_iter).second = NULL;
        }
    }
    pre_descr_p->deleteDescriptor();
    delete pre_descr_p;
}
/* --------- Non member functions --------- */

#include "mv_type.h"


void CFGKernel::init_pre_descriptors() {
  loc_get_pdma_ptr(1);
}



void CFGKernel::close_pre_descriptors() {
  loc_get_pdma_ptr(-1);
}


void CFGKernel::add_user_pre_descriptor(object_type_e obj_type,MvPreDescriptor_t *pre_descr_p,
				const PseudoStringList_t *user_name_list_p) 
{
  LocUserPreDescriptorMapArray_t *a_pdma_p=loc_get_pdma_ptr();
  a_pdma_p->addPreDescriptor(obj_type,pre_descr_p,user_name_list_p);
}


void CFGKernel::add_user_pre_descriptor(object_type_e obj_type,MvPreDescriptor_t *pre_descr_p,
				const string &user_name)
{
  MvStringList_t a_user_names;
  a_user_names.push_back(user_name);
  //
  LocUserPreDescriptorMapArray_t *a_pdma_p=loc_get_pdma_ptr();
  a_pdma_p->addPreDescriptor(obj_type,pre_descr_p,(const PseudoStringList_t *)(&a_user_names));
}


const MvPreDescriptor_t * CFGKernel::get_user_pre_descriptor(object_type_e obj_type,int id) const {
  LocUserPreDescriptorMapArray_t *a_pdma_p=loc_get_pdma_ptr();
  return a_pdma_p->getPreDescriptorPtr(obj_type,id);
}

const MvPreDescriptor_t * CFGKernel::get_user_pre_descriptor(object_type_e obj_type,int hm_config_type,int hm_type) const
{
  LocUserPreDescriptorMapArray_t *a_pdma_p=loc_get_pdma_ptr();
  return a_pdma_p->getPreDescriptorPtr(obj_type,hm_config_type,hm_type);
}
const MvPreDescriptor_t * CFGKernel::get_user_pre_descriptor(object_type_e obj_type, int hm_config_type, int hm_type, const string &keyword) const {
  LocUserPreDescriptorMapArray_t *a_pdma_p=loc_get_pdma_ptr();
  return a_pdma_p->getPreDescriptorPtr(obj_type, hm_config_type, hm_type, keyword);
}

/* --------- Static functions --------- */


LocUserPreDescriptorMapArray_t * CFGKernel::loc_get_pdma_ptr(int option) const {
  //LocUserPreDescriptorMapArray_t *a_pdma_p= p_pdma_p;
  //
  switch(option) {
  case 1:  // Creation
    if(p_pdma_p !=NULL) delete p_pdma_p;
    p_pdma_p =new LocUserPreDescriptorMapArray_t();
    break;
  case 0:  // Access
    if(p_pdma_p ==NULL) p_pdma_p =new LocUserPreDescriptorMapArray_t();
    break;
  case -1: // Destruction
    if(p_pdma_p !=NULL) {
      delete p_pdma_p;
      p_pdma_p =NULL;
    }
    break;
  default:
    break;
  }
  //
  return p_pdma_p;
}

