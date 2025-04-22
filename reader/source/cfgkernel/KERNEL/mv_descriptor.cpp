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



#include <UTILS/mv_cstring.h>
#include <UTILS/mv_stl_various.h>
#include <UTILS/error.h>
#include <UTILS/set_utils.h>
#include <UTILS/memory_utils.h>


#include "mv_descriptor.h"
#include "mv_default_map.h"
#include "mv_full_type.h"
#include "mv_data_if_feature.h"
#include "mv_data_conditional_feature.h" 
#include "mv_descriptor_parser.h"
#include "mv_iparam_descr_translation.h" 
#include "mv_iparam_descr_rotation.h"    
#include "mv_iparam_descr_scaling.h"     
#include "mv_oparam_descr_th_variable.h" 
//#include "mv_th_variables.h"             
//#include "mv_thnms_var.h"                
#include "mv_data_radio_feature.h"
#include "mv_data_uncond_function_feature.h"
#include "mv_compare_test.h"
#include "mv_compare_attributes_test.h"
#include <assert.h>

#include "mv_data_cfg.h"
#include "mv_pre_datas_hierarchy.h"
#include "mv_compare_test.h"
#include "mv_compare_attributes_test.h"
#include "mv_attribute_expression.h"
#include "mv_kernel.h"
#include <KERNEL_BASE/utils.h>
#include <KERNEL_BASE/fileformat_API.h>
#include "mv_pre_descriptor.h"
#include "HCDI/hcdi_multicfgkernelmgr.h"


#include<assert.h>

typedef map<MvDomain_e,MvIKeywordList_t>        LocIKeywordListMap_t;
typedef map<MvDomain_e,MvDataFeatureList_t>     LocDataFeatureListMap_t;

typedef map<MvDomain_e,MvTestList_t>            LocTestListMap_t;
typedef map<MvDomain_e,MvDependenceList_t>      LocDependenceListMap_t;

typedef map<MvDomain_e,MvCondIParamDescrList_t> LocCondIParamDescrListMap_t; 
typedef map<MvDomain_e,MvCondOParamDescrList_t> LocCondOParamDescrListMap_t; 

typedef map<MvDomain_e,MvDrawablePtrSet_t>      LocDrawableMap_t;
typedef map<string,const MvDrawable_t *>        LocDrawableNameMap_t;


typedef map<MvDomain_e,MvIParamDescrPtrList_t>  LocIParamDescrMap_t;     
typedef map<string,const MvIParamDescr_t *>     LocIParamDescrNameMap_t; 


typedef map<MvDomain_e,MvOParamDescrPtrList_t>  LocOParamDescrMap_t;
typedef map<string,const MvOParamDescr_t *>     LocOParamDescrNameMap_t;


typedef map<string,MvIKeywordSet_t>             LocDefinition_t;        
typedef map<MvDomain_e,LocDefinition_t>         LocDefinitionMap_t;     

typedef map<string,MvDataSelectionFeature_t *>    LocDataSelectionFeatureMap_t;

typedef map<string,string>                      LocKeywordKeywordMap_t;


#define MvPseudoDefaultMap_t void
class MvDefaultMapArray_t {
public:
  MvDefaultMapArray_t() :myBoolDefaultMap(), myIntDefaultMap(), myUIntDefaultMap(), myFloatDefaultMap(), myStringDefaultMap() {}
public:
  const MvPseudoDefaultMap_t *operator[](value_type_e vtype) const {
    switch(vtype) {
    case VTYPE_INT: 
    {
         return &myIntDefaultMap;    //break;
    }   
    case VTYPE_BOOL:
    {
         return &myBoolDefaultMap;    //break;
    }
    case VTYPE_UINT:   return &myUIntDefaultMap; 
    case VTYPE_FLOAT:  return &myFloatDefaultMap;  //break;
    case VTYPE_STRING: return &myStringDefaultMap; //break;
    case VTYPE_OBJECT: return &myObjectDefaultMap;
    default:           return NULL;                //break;
    }
  }
  MvPseudoDefaultMap_t *operator[](value_type_e vtype) {
    switch(vtype) {
    case VTYPE_INT:   return &myIntDefaultMap;    //break;
    case VTYPE_BOOL:  return &myBoolDefaultMap;    //break;
    case VTYPE_UINT:    return &myUIntDefaultMap;    //break;
    case VTYPE_FLOAT:  return &myFloatDefaultMap;  //break;
    case VTYPE_STRING: return &myStringDefaultMap; //break;
    case VTYPE_OBJECT: return &myObjectDefaultMap;
    default:           return NULL;                //break;
    }
  }
private:
  MvIntDefaultMap_t    myBoolDefaultMap;
  MvIntDefaultMap_t    myIntDefaultMap;
  MvUIntDefaultMap_t   myUIntDefaultMap;
  MvFloatDefaultMap_t  myFloatDefaultMap;
  MvStringDefaultMap_t myStringDefaultMap;
  MvObjectDefaultMap_t myObjectDefaultMap;
};
typedef map<MvDomain_e,MvDefaultMapArray_t> MvDefaults_t; 


#define MvPseudoIdentifierMap_t void
class MvIdentifierMapArray_t {
public:
  MvIdentifierMapArray_t() : myIntIdentifierMap() {}
public:
  const MvPseudoIdentifierMap_t *operator[](value_type_e vtype) const {
    return &myIntIdentifierMap;    //break;
  }
  MvPseudoIdentifierMap_t *operator[](value_type_e vtype) {
      return &myIntIdentifierMap;    //break;
  }
  MvIntIdentifierMap_t    myIntIdentifierMap;
};

typedef map<MvDomain_e,MvIdentifierMapArray_t> MvIdentifiers_t;


static void loc_add_special_parameters(); 
static char *loc_string_fit_all(char *name);
static int loc_get_cell_length(ff_cell_t *cell_p);

/* --------- Constructors and destructor --------- */

MvDescriptor_t::MvDescriptor_t(int user_id) : 
  myDescriptorPtr(NULL), 
  myIKeywordListMapPtr((MvPseudoIKeywordListMap_t *)(new LocIKeywordListMap_t())),
  myDefaultsPtr((MvPseudoDefaults_t *)(new MvDefaults_t())), 
  myIdentifiersPtr((MvPseudoIdentifierMap_t *)(new MvIdentifiers_t())),
  myDataFeatureListMapPtr((MvPseudoDataFeatureListMap_t *)(new LocDataFeatureListMap_t())),
  myDataFeatureReducedListMapPtr(NULL),
  myTestListMapPtr((MvPseudoTestListMap_t *)(new LocTestListMap_t())),
  myDependenceListMapPtr((MvPseudoDependenceListMap_t *)(new LocDependenceListMap_t())),
  
  myCondIParamDescrListMapPtr((MvPseudoCondIParamDescrListMap_t *)(new LocCondIParamDescrListMap_t())),
  myCondOParamDescrListMapPtr((MvPseudoCondOParamDescrListMap_t *)(new LocCondOParamDescrListMap_t())),
  
  myDrawableListMapPtr((MvPseudoDrawableMap_t *)(new LocDrawableMap_t())),
  myDrawableListNameMapPtr((MvPseudoDrawableNameMap_t *)(new LocDrawableNameMap_t())),
  myIParamDescrListMapPtr((MvPseudoIParamDescrMap_t *)(new LocIParamDescrMap_t())),
  myIParamDescrListNameMapPtr((MvPseudoIParamDescrNameMap_t *)(new LocIParamDescrNameMap_t())),
  
  myOParamDescrListMapPtr((MvPseudoOParamDescrMap_t *)(new LocOParamDescrMap_t())),
  myOParamDescrListNameMapPtr((MvPseudoOParamDescrNameMap_t *)(new LocOParamDescrNameMap_t())),
  
  myDefinitionMapPtr((MvPseudoDefinitionMap_t *)(new LocDefinitionMap_t())), 
  mySolverNameSKeywordMapPtr((MvPseudoKeywordKeywordMap_t *)(new LocKeywordKeywordMap_t())),
  mySelectionMapPtr(NULL),
  myIdPool(0)
{
  // MCDS_descriptor
  MCDS_new_descriptor(&myDescriptorPtr);
  setUserId(user_id);
  myConfigType = 0;
  myHmType = 0;
  myKeyword = "";
  // Default values
  
  //myDefaultMapArray[VTYPE_INT]    = (MvPseudoDefaultMap_t *)(new MvIntDefaultMap_t());
  //myDefaultMapArray[VTYPE_FLOAT]  = (MvPseudoDefaultMap_t *)(new MvFloatDefaultMap_t());
  //myDefaultMapArray[VTYPE_STRING] = (MvPseudoDefaultMap_t *)(new MvStringDefaultMap_t());
  
}

MvDescriptor_t::~MvDescriptor_t() {
  // MCDS descriptor
  MCDS_delete_descriptor(myDescriptorPtr);
  myfree(myDescriptorPtr);
  
  // Domains & i-keywords
  LocIKeywordListMap_t *a_ikwlm_pf=(LocIKeywordListMap_t *)myIKeywordListMapPtr;
  if(a_ikwlm_pf!=NULL) delete a_ikwlm_pf;
  // Defaults
  MvDefaults_t *a_defaults_pf=(MvDefaults_t *)myDefaultsPtr;
  if(a_defaults_pf!=NULL) delete a_defaults_pf;
  // IKeyword Identifier
  MvIdentifiers_t *a_identifier_pf=(MvIdentifiers_t *)myIdentifiersPtr;
  if(a_identifier_pf!=NULL) delete a_identifier_pf;

  
  // Default maps
  
  //if(myDefaultMapArray[VTYPE_INT]!=NULL)    delete (MvIntDefaultMap_t *)(myDefaultMapArray[VTYPE_INT]);
  //if(myDefaultMapArray[VTYPE_FLOAT]!=NULL)  delete (MvFloatDefaultMap_t *)(myDefaultMapArray[VTYPE_FLOAT]);
  //if(myDefaultMapArray[VTYPE_STRING]!=NULL) delete (MvStringDefaultMap_t *)(myDefaultMapArray[VTYPE_STRING]);
  
  // Tests
  LocTestListMap_t *a_tlm_pf=(LocTestListMap_t *)myTestListMapPtr;
  if(a_tlm_pf!=NULL) {
    for(LocTestListMap_t::iterator a_tlm_it=a_tlm_pf->begin();a_tlm_it!=a_tlm_pf->end();++a_tlm_it) {
      MvTestList_t &a_tl=(*a_tlm_it).second;
      for(MvTestList_t::iterator a_tl_it=a_tl.begin();a_tl_it!=a_tl.end();++a_tl_it) {
	delete (*a_tl_it);
      }
    }
    delete a_tlm_pf;
  }
  // Data features
  LocDataFeatureListMap_t *a_dflm_pf=(LocDataFeatureListMap_t *)myDataFeatureListMapPtr;
  if(a_dflm_pf!=NULL) {
    for(LocDataFeatureListMap_t::iterator a_dflm_it=a_dflm_pf->begin();a_dflm_it!=a_dflm_pf->end();++a_dflm_it) {
      MvDataFeatureList_t &a_dfl=(*a_dflm_it).second;
      for(MvDataFeatureList_t::iterator a_dfl_it=a_dfl.begin();a_dfl_it!=a_dfl.end();++a_dfl_it) {
        delete (*a_dfl_it);
      }
    }
    delete a_dflm_pf;
  }
  // delete for reduced list, no need to delete inside contents as they are already deleted above
  if (myDataFeatureReducedListMapPtr)
  {
      LocDataFeatureListMap_t* aa_dflm_pf = (LocDataFeatureListMap_t*)myDataFeatureReducedListMapPtr;
      delete aa_dflm_pf;
  }
  // Conditional ikeywords (depedences)
  LocDependenceListMap_t *a_dlm_pf=(LocDependenceListMap_t *)myDependenceListMapPtr;
  if(a_dflm_pf!=NULL) {
    LocDependenceListMap_t::iterator a_dlm_it_begin = a_dlm_pf->begin();
    LocDependenceListMap_t::iterator a_dlm_it_end   = a_dlm_pf->end();
    LocDependenceListMap_t::iterator a_dlm_it;
    for(a_dlm_it=a_dlm_it_begin;a_dlm_it!=a_dlm_it_end;++a_dlm_it) {
      MvDependenceList_t &a_dl=(*a_dlm_it).second;
      MvDependenceList_t::iterator a_dl_it_begin = a_dl.begin();
      MvDependenceList_t::iterator a_dl_it_end   = a_dl.end();
      MvDependenceList_t::iterator a_dl_it;
      for(a_dl_it=a_dl_it_begin;a_dl_it!=a_dl_it_end;++a_dl_it) delete (*a_dl_it);
    }
    a_dlm_pf->clear();
    delete a_dlm_pf;
  }
  
  // Conditional i-param descriptors
  LocCondIParamDescrListMap_t *a_cipdlm_pf=(LocCondIParamDescrListMap_t *)myCondIParamDescrListMapPtr;
  if(a_cipdlm_pf!=NULL) {
    LocCondIParamDescrListMap_t::iterator a_cipdlm_it_begin = a_cipdlm_pf->begin();
    LocCondIParamDescrListMap_t::iterator a_cipdlm_it_end   = a_cipdlm_pf->end();
    LocCondIParamDescrListMap_t::iterator a_cipdlm_it;
    for(a_cipdlm_it=a_cipdlm_it_begin;a_cipdlm_it!=a_cipdlm_it_end;++a_cipdlm_it) {
      MvCondIParamDescrList_t &a_dl=(*a_cipdlm_it).second;
      MvCondIParamDescrList_t::iterator a_cipdl_it_begin = a_dl.begin();
      MvCondIParamDescrList_t::iterator a_cipdl_it_end   = a_dl.end();
      MvCondIParamDescrList_t::iterator a_cipdl_it;
      for(a_cipdl_it=a_cipdl_it_begin;a_cipdl_it!=a_cipdl_it_end;++a_cipdl_it) delete (*a_cipdl_it);
    }
    a_cipdlm_pf->clear();
    delete a_cipdlm_pf;
  }
  // Conditional o-param descriptors
  LocCondOParamDescrListMap_t *a_copdlm_pf=(LocCondOParamDescrListMap_t *)myCondOParamDescrListMapPtr;
  if(a_copdlm_pf!=NULL) {
    LocCondOParamDescrListMap_t::iterator a_copdlm_it_begin = a_copdlm_pf->begin();
    LocCondOParamDescrListMap_t::iterator a_copdlm_it_end   = a_copdlm_pf->end();
    LocCondOParamDescrListMap_t::iterator a_copdlm_it;
    for(a_copdlm_it=a_copdlm_it_begin;a_copdlm_it!=a_copdlm_it_end;++a_copdlm_it) {
      MvCondOParamDescrList_t &a_dl=(*a_copdlm_it).second;
      MvCondOParamDescrList_t::iterator a_copdl_it_begin = a_dl.begin();
      MvCondOParamDescrList_t::iterator a_copdl_it_end   = a_dl.end();
      MvCondOParamDescrList_t::iterator a_copdl_it;
      for(a_copdl_it=a_copdl_it_begin;a_copdl_it!=a_copdl_it_end;++a_copdl_it) delete (*a_copdl_it);
    }
    a_copdlm_pf->clear();
    delete a_copdlm_pf;
  }
  
  // Drawables
  if(myDrawableListNameMapPtr!=NULL) delete (LocDrawableNameMap_t*)myDrawableListNameMapPtr;
  LocDrawableMap_t *a_drlm_pf=(LocDrawableMap_t *)myDrawableListMapPtr;
  if(a_drlm_pf!=NULL) {
    LocDrawableMap_t::iterator a_drlm_it_begin = a_drlm_pf->begin();
    LocDrawableMap_t::iterator a_drlm_it_end   = a_drlm_pf->end();
    LocDrawableMap_t::iterator a_drlm_it;
    for(a_drlm_it=a_drlm_it_begin;a_drlm_it!=a_drlm_it_end;++a_drlm_it) {
      MvDrawablePtrSet_t &a_drawables=(*a_drlm_it).second;
      MvDrawablePtrSet_t::iterator a_ds_it_begin = a_drawables.begin();
      MvDrawablePtrSet_t::iterator a_ds_it_end   = a_drawables.end();
      MvDrawablePtrSet_t::iterator a_ds_it;
      for(a_ds_it=a_ds_it_begin;a_ds_it!=a_ds_it_end;++a_ds_it)  {
	const MvDrawable_t *a_drawable_pf=(*a_ds_it);
	delete a_drawable_pf;
      }
    }
    a_drlm_pf->clear();
    delete a_drlm_pf;
  }
  // Input parameters
  if(myIParamDescrListMapPtr!=NULL) delete (LocIParamDescrMap_t *)myIParamDescrListMapPtr;
  LocIParamDescrNameMap_t *a_ipnm_pf=(LocIParamDescrNameMap_t *)myIParamDescrListNameMapPtr;
  if(a_ipnm_pf!=NULL) {
    LocIParamDescrNameMap_t::iterator a_ipnm_it_begin = a_ipnm_pf->begin();
    LocIParamDescrNameMap_t::iterator a_ipnm_it_end   = a_ipnm_pf->end();
    LocIParamDescrNameMap_t::iterator a_ipnm_it;
    for(a_ipnm_it=a_ipnm_it_begin;a_ipnm_it!=a_ipnm_it_end;++a_ipnm_it) {
      const MvIParamDescr_t *a_iparam_pf=(*a_ipnm_it).second;
      delete a_iparam_pf;
    }
    a_ipnm_pf->clear();
    delete a_ipnm_pf;
  }
  
  // Output parameters
  if(myOParamDescrListMapPtr!=NULL) delete (LocOParamDescrMap_t *)myOParamDescrListMapPtr;
  LocOParamDescrNameMap_t *a_opnm_pf=(LocOParamDescrNameMap_t *)myOParamDescrListNameMapPtr;
  if(a_opnm_pf!=NULL) {
    LocOParamDescrNameMap_t::iterator a_opnm_it_begin = a_opnm_pf->begin();
    LocOParamDescrNameMap_t::iterator a_opnm_it_end   = a_opnm_pf->end();
    LocOParamDescrNameMap_t::iterator a_opnm_it;
    for(a_opnm_it=a_opnm_it_begin;a_opnm_it!=a_opnm_it_end;++a_opnm_it) {
      const MvOParamDescr_t *a_oparam_pf=(*a_opnm_it).second;
      delete a_oparam_pf;
    }
    a_opnm_pf->clear();
    delete a_opnm_pf;
  }
  
  // Definitions
  delete ((LocDefinitionMap_t *)myDefinitionMapPtr); 
  // Keyword mapping
  delete (LocKeywordKeywordMap_t *)mySolverNameSKeywordMapPtr;

  //selection pointers
   LocDataSelectionFeatureMap_t *a_sdsm_pf = (LocDataSelectionFeatureMap_t *)mySelectionMapPtr;
   if(a_sdsm_pf != NULL)
   {
       LocDataSelectionFeatureMap_t::iterator a_sdsm_it_begin = a_sdsm_pf->begin();
       LocDataSelectionFeatureMap_t::iterator a_sdsm_it_end   = a_sdsm_pf->end();
       LocDataSelectionFeatureMap_t::iterator a_sdsm_it;
       for(a_sdsm_it=a_sdsm_it_begin;a_sdsm_it!=a_sdsm_it_end;++a_sdsm_it) {
           MvDataSelectionFeature_t *a_sdatas_pf =(*a_sdsm_it).second;
           delete a_sdatas_pf;
       }
       a_sdsm_pf->clear();
       delete a_sdsm_pf;
   }

}


/* --------- MCDS description --------- */

/* --------- Creation --------- */

void MvDescriptor_t::addValue(MvDomain_e domain,value_type_e vtype,
			      int ikeyword,const string &skeyword,const string &comment) 
{
  if(MCDS_add_descriptor_value(myDescriptorPtr,vtype,ikeyword,skeyword.c_str(),comment.c_str())) {
    throw MvError_t(string("MvDescriptor_t::addValue -> Error when adding ")+skeyword);
  }
  addDomainIKeyword(domain,ikeyword);
}

void MvDescriptor_t::addObject(MvDomain_e domain,object_type_e otype,
			       int ikeyword,const string &skeyword,const string &comment)
{
  if(MCDS_add_descriptor_object(myDescriptorPtr,otype,ikeyword,skeyword.c_str(),comment.c_str())) {
    throw MvError_t(string("MvDescriptor_t::addObject -> Error when adding ")+skeyword);
  }
  addDomainIKeyword(domain,ikeyword);
}

void MvDescriptor_t::addSize(MvDomain_e domain,int ikeyword,
			     const string &skeyword,const string &comment) 
{
  if(MCDS_add_descriptor_size(myDescriptorPtr,ikeyword,skeyword.c_str(),comment.c_str())) {
    throw MvError_t(string("MvDescriptor_t::addSize -> Error when adding ")+skeyword);
  }
  addDomainIKeyword(domain,ikeyword);
}

void MvDescriptor_t::addValueArray(MvDomain_e domain,value_type_e vtype,
				   int ikeyword,const string &skeyword,const string &comment,
				   attribute_type_e array_type,
                                   const MvSizeVector & sizeArrayVector)
{
  dimension_size_t *size_array = (dimension_size_t *)my_malloc((int)(sizeArrayVector.size()),sizeof(dimension_size_t));

  for(unsigned int i = 0; i<sizeArrayVector.size();i++){
     size_array[i] = sizeArrayVector[i];
  }

  if(MCDS_add_descriptor_value_array(myDescriptorPtr,
				     vtype,ikeyword,skeyword.c_str(),comment.c_str(),array_type,
                                      (int)(sizeArrayVector.size()),size_array)) {
    throw MvError_t(string("MvDescriptor_t::addValueArray -> Error when adding ")+skeyword);
  }
//  if(size_array)
//    my_free(size_array);
  addDomainIKeyword(domain,ikeyword);
}

void MvDescriptor_t::addObjectArray(MvDomain_e domain,object_type_e otype,
				    int ikeyword,const string &skeyword,const string &comment,
				    attribute_type_e array_type,
                                    const MvSizeVector & sizeArrayVector)
{
  dimension_size_t *size_array = (dimension_size_t *)my_malloc((int)(sizeArrayVector.size()),sizeof(dimension_size_t));

  for(unsigned int i = 0; i<sizeArrayVector.size();i++){
     size_array[i] = sizeArrayVector[i];
  }

  if(MCDS_add_descriptor_object_array(myDescriptorPtr,
				      otype,ikeyword,skeyword.c_str(),comment.c_str(),array_type,
                                      (int)(sizeArrayVector.size()),size_array)) {
    throw MvError_t(string("MvDescriptor_t::addObjectArray -> Error when adding ")+skeyword);
  }
//  if(size_array)
//    my_free(size_array);
  addDomainIKeyword(domain,ikeyword);
}


/* --------- Acces to descriptor --------- */

bool MvDescriptor_t::isUser() const {
  int a_is_user;
  MCDS_is_descriptor_user(myDescriptorPtr,&a_is_user);
  return a_is_user!=0;
}

int MvDescriptor_t::getUserId() const {
  int a_id;
  MCDS_get_descriptor_user_id(myDescriptorPtr,&a_id);
  return a_id;
}

void MvDescriptor_t::setUserId(int id) {
  MCDS_set_descriptor_user_id(myDescriptorPtr,id);
}


/* --------- Keywords (private methods) --------- */

int MvDescriptor_t::getFirstIKeyword() const {
  int an_ikeyword=END_ARGS;
  MCDS_get_descriptor_first_ikeyword(myDescriptorPtr,&an_ikeyword);
  return an_ikeyword;
}

int MvDescriptor_t::getNextIKeyword(int ikeyword) const {
  int an_ikeyword=END_ARGS;
  MCDS_get_descriptor_next_ikeyword(myDescriptorPtr,ikeyword,&an_ikeyword);
  return an_ikeyword;
}

void MvDescriptor_t::addDomainIKeyword(MvDomain_e domain,int ikeyword) {
  (*((LocIKeywordListMap_t *)myIKeywordListMapPtr))[domain].push_back(ikeyword);
}


/* --------- Keywords --------- */

int MvDescriptor_t::getIKeyword(const string &skeyword) const {
  int ikeyword = 0;
  MCDS_get_descriptor_ikeyword(myDescriptorPtr,skeyword.c_str(),&ikeyword);
  return ikeyword;
}

string MvDescriptor_t::getSKeyword(int ikeyword) const {
  char *skeyword=const_cast<char*>("UNKNOWN");
  int err=MCDS_get_descriptor_skeyword(myDescriptorPtr,ikeyword,&skeyword);
  if(err) return "UNKNOWN";
  return skeyword;
}
int MvDescriptor_t::getSKeyword(int ikeyword, const char** skeyword) const {
    static char* skeyword_unknown = const_cast<char*>("UNKNOWN");
    int err = MCDS_get_descriptor_skeyword(myDescriptorPtr, ikeyword, (char **)skeyword);
    if (err) *skeyword = skeyword_unknown;
    return err;
}
const string & MvDescriptor_t::getSKeywordFromSolverName(const string &solvername) const
{
    static string a_empty_string;
    const LocKeywordKeywordMap_t &a_SolverNameSKeywordMap = (*((const LocKeywordKeywordMap_t *)mySolverNameSKeywordMapPtr));
    LocKeywordKeywordMap_t::const_iterator a_it = a_SolverNameSKeywordMap.find(solvername);
    if(a_it != a_SolverNameSKeywordMap.end()) return a_it->second;
    else                                      return a_empty_string;
}

const MvCondKeywordList_t& MvDescriptor_t::getSKeywordsFromSolverName(const string &solvername) const
{
    static MvCondKeywordList_t empty_list;
    map<string, MvCondKeywordList_t>::const_iterator a_it = mySolverNameSKeywordsMap.find(solvername);
    if(a_it != mySolverNameSKeywordsMap.end()) return a_it->second;
    else                                       return empty_list;
}


MvIKeywordSet_t *MvDescriptor_t::getIKeywords(const MvSKeywordSet_t &skw_set,MvIKeywordSet_t *ikw_set_p) const {
  if(ikw_set_p==NULL) ikw_set_p=new MvIKeywordSet_t();
  //
  MvSKeywordSet_t::const_iterator a_skw_it_begin = skw_set.begin();
  MvSKeywordSet_t::const_iterator a_skw_it_end   = skw_set.end();
  MvSKeywordSet_t::const_iterator a_skw_it;
  for(a_skw_it=a_skw_it_begin;a_skw_it!=a_skw_it_end;++a_skw_it) {
      ikw_set_p->insert(getIKeyword(*a_skw_it));
  }
  //
  return ikw_set_p;
}

MvSKeywordSet_t *MvDescriptor_t::getSKeywords(const MvIKeywordSet_t &ikw_set,MvSKeywordSet_t *skw_set_p) const {
  if(skw_set_p==NULL) skw_set_p=new MvSKeywordSet_t();
  //
  MvIKeywordSet_t::const_iterator a_ikw_it_begin = ikw_set.begin();
  MvIKeywordSet_t::const_iterator a_ikw_it_end   = ikw_set.end();
  MvIKeywordSet_t::const_iterator a_ikw_it;
  for(a_ikw_it=a_ikw_it_begin;a_ikw_it!=a_ikw_it_end;++a_ikw_it) {
      skw_set_p->insert(getSKeyword(*a_ikw_it));
  }
  //
  return skw_set_p;
}



MvIKeywordSet_t *MvDescriptor_t::getSupportIKeywords(int domain,MvIKeywordSet_t *ikw_set_p) const {
  if(ikw_set_p==NULL) ikw_set_p=new MvIKeywordSet_t();
  //
  MvDataFeatureList_t *a_dfl_pf=getDataFeatures(domain);
  MvDataFeatureList_t::const_iterator a_dfl_begin=a_dfl_pf->begin();
  MvDataFeatureList_t::const_iterator a_dfl_end  =a_dfl_pf->end();
  MvDataFeatureList_t::const_iterator a_dfl_it;
  for(a_dfl_it=a_dfl_begin;a_dfl_it!=a_dfl_end;++a_dfl_it) {
    if((*a_dfl_it)->getType()==DFT_SUPPORT) {
      const MvDataSupportFeature_t *a_dsf_p=(MvDataSupportFeature_t *)(*a_dfl_it);
      int a_nb_attributes=a_dsf_p->getNbAttributes();
      for(int i=0;i<a_nb_attributes;++i) {
	int a_ikeyword=a_dsf_p->getAttributeIKeyword(i);
	if(a_ikeyword!=END_ARGS) ikw_set_p->insert(a_ikeyword);
      }
    }
  }
  delete a_dfl_pf;
  //
  return ikw_set_p;
}

const MvIKeywordList_t &MvDescriptor_t::getDomainIKeywords(MvDomain_e domain) const {
  static const MvIKeywordList_t a_ikw_void_list; /*multimodel not required*/
  const LocIKeywordListMap_t &a_ikw_list_map=*((LocIKeywordListMap_t *)myIKeywordListMapPtr);
  //
  LocIKeywordListMap_t::const_iterator a_it=a_ikw_list_map.find(domain);
  if(a_it==a_ikw_list_map.end()) return a_ikw_void_list;
  //
  return (*a_it).second;
}

MvIKeywordList_t *MvDescriptor_t::getIKeywords(int domain,MvIKeywordList_t *ikw_list_p) const {
  if(ikw_list_p==NULL) ikw_list_p=new MvIKeywordList_t();
  //
  const LocIKeywordListMap_t &a_ikw_list_map=*((LocIKeywordListMap_t *)myIKeywordListMapPtr);
  LocIKeywordListMap_t::const_iterator a_ikwlm_it_begin = a_ikw_list_map.begin();
  LocIKeywordListMap_t::const_iterator a_ikwlm_it_end   = a_ikw_list_map.end();
  LocIKeywordListMap_t::const_iterator a_ikwlm_it;
  for(a_ikwlm_it=a_ikwlm_it_begin;a_ikwlm_it!=a_ikwlm_it_end;++a_ikwlm_it) if((*a_ikwlm_it).first & domain) {
    const MvIKeywordList_t &a_ikw_list=(*a_ikwlm_it).second;
    MvIKeywordList_t::const_iterator a_ikwl_it_begin = a_ikw_list.begin();
    MvIKeywordList_t::const_iterator a_ikwl_it_end   = a_ikw_list.end();
    MvIKeywordList_t::const_iterator a_ikwl_it;
    for(a_ikwl_it=a_ikwl_it_begin;a_ikwl_it!=a_ikwl_it_end;++a_ikwl_it) ikw_list_p->push_back(*a_ikwl_it);
  }
  //
  return ikw_list_p;
}

MvIKeywordList_t *MvDescriptor_t::getIKeywords(int domain,value_type_e vtype,MvIKeywordList_t *ikw_list_p) const
{
  if(ikw_list_p==NULL) ikw_list_p=new MvIKeywordList_t();
  //
  const LocIKeywordListMap_t &a_ikw_list_map=*((LocIKeywordListMap_t *)myIKeywordListMapPtr);
  LocIKeywordListMap_t::const_iterator a_ikwlm_it_begin = a_ikw_list_map.begin();
  LocIKeywordListMap_t::const_iterator a_ikwlm_it_end   = a_ikw_list_map.end();
  LocIKeywordListMap_t::const_iterator a_ikwlm_it;
  for(a_ikwlm_it=a_ikwlm_it_begin;a_ikwlm_it!=a_ikwlm_it_end;++a_ikwlm_it) if((*a_ikwlm_it).first & domain) {
    const MvIKeywordList_t &a_ikw_list=(*a_ikwlm_it).second;
    MvIKeywordList_t::const_iterator a_ikwl_it_begin = a_ikw_list.begin();
    MvIKeywordList_t::const_iterator a_ikwl_it_end   = a_ikw_list.end();
    MvIKeywordList_t::const_iterator a_ikwl_it;
    for(a_ikwl_it=a_ikwl_it_begin;a_ikwl_it!=a_ikwl_it_end;++a_ikwl_it) {
      int a_ikeyword=(*a_ikwl_it);
      if(getValueType(a_ikeyword)==vtype) ikw_list_p->push_back(a_ikeyword);
    }
  }
  //
  return ikw_list_p;  
}

MvIKeywordList_t *MvDescriptor_t::getIKeywords(int domain,attribute_type_e atype,
					       MvIKeywordList_t *ikw_list_p) const
{
   if(ikw_list_p==NULL) ikw_list_p=new MvIKeywordList_t();
  //
  const LocIKeywordListMap_t &a_ikw_list_map=*((LocIKeywordListMap_t *)myIKeywordListMapPtr);
  LocIKeywordListMap_t::const_iterator a_ikwlm_it_begin = a_ikw_list_map.begin();
  LocIKeywordListMap_t::const_iterator a_ikwlm_it_end   = a_ikw_list_map.end();
  LocIKeywordListMap_t::const_iterator a_ikwlm_it;
  for(a_ikwlm_it=a_ikwlm_it_begin;a_ikwlm_it!=a_ikwlm_it_end;++a_ikwlm_it) if((*a_ikwlm_it).first & domain) {
    const MvIKeywordList_t &a_ikw_list=(*a_ikwlm_it).second;
    MvIKeywordList_t::const_iterator a_ikwl_it_begin = a_ikw_list.begin();
    MvIKeywordList_t::const_iterator a_ikwl_it_end   = a_ikw_list.end();
    MvIKeywordList_t::const_iterator a_ikwl_it;
    for(a_ikwl_it=a_ikwl_it_begin;a_ikwl_it!=a_ikwl_it_end;++a_ikwl_it) {
      int a_ikeyword=(*a_ikwl_it);
      if(getAttributeType(a_ikeyword)==atype) ikw_list_p->push_back(a_ikeyword);
    }
  }
  //
  return ikw_list_p;   
}

MvIKeywordList_t *MvDescriptor_t::getIKeywords(int domain,attribute_type_e atype,value_type_e vtype,
					       MvIKeywordList_t *ikw_list_p) const
{
  if(ikw_list_p==NULL) ikw_list_p=new MvIKeywordList_t();
  //
  const LocIKeywordListMap_t &a_ikw_list_map=*((LocIKeywordListMap_t *)myIKeywordListMapPtr);
  LocIKeywordListMap_t::const_iterator a_ikwlm_it_begin = a_ikw_list_map.begin();
  LocIKeywordListMap_t::const_iterator a_ikwlm_it_end   = a_ikw_list_map.end();
  LocIKeywordListMap_t::const_iterator a_ikwlm_it;
  for(a_ikwlm_it=a_ikwlm_it_begin;a_ikwlm_it!=a_ikwlm_it_end;++a_ikwlm_it) if((*a_ikwlm_it).first & domain) {
    const MvIKeywordList_t &a_ikw_list=(*a_ikwlm_it).second;
    MvIKeywordList_t::const_iterator a_ikwl_it_begin = a_ikw_list.begin();
    MvIKeywordList_t::const_iterator a_ikwl_it_end   = a_ikw_list.end();
    MvIKeywordList_t::const_iterator a_ikwl_it;
    for(a_ikwl_it=a_ikwl_it_begin;a_ikwl_it!=a_ikwl_it_end;++a_ikwl_it) {
      int a_ikeyword=(*a_ikwl_it);
      if(getAttributeType(a_ikeyword)==atype && getValueType(a_ikeyword)==vtype) {
	ikw_list_p->push_back(a_ikeyword);
      }
    }
  }
  //
  return ikw_list_p;  
}
int MvDescriptor_t::getAllIKeywords(MvDomain_e domain, MvAtypeVtypeList_t  &atype_vtypeikeywordlst) const
{
    int size = 0, loc_size = 0;
    vector<MvIKeywordList_t>  a_vec_single_lst(VTYPE_LAST);
    vector<MvIKeywordList_t>  a_vec_array_lst(VTYPE_LAST);
    MvIKeywordList_t a_ikeywords;

    //single
    getIKeywords(domain,ATYPE_VALUE,VTYPE_BOOL,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if(loc_size)
        a_vec_single_lst[VTYPE_BOOL]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_VALUE,VTYPE_UINT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if(loc_size)
       a_vec_single_lst[VTYPE_UINT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_VALUE,VTYPE_INT,&a_ikeywords);
    getIKeywords(domain,ATYPE_SIZE,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
      a_vec_single_lst[VTYPE_INT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_VALUE,VTYPE_FLOAT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
       a_vec_single_lst[VTYPE_FLOAT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_VALUE,VTYPE_STRING,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_single_lst[VTYPE_STRING]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_VALUE,VTYPE_OBJECT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
       a_vec_single_lst[VTYPE_OBJECT]=a_ikeywords;

    //array
    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_BOOL,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_BOOL,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_array_lst[VTYPE_BOOL]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_UINT,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_UINT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_array_lst[VTYPE_UINT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_INT,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_INT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_array_lst[VTYPE_INT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_FLOAT,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_FLOAT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_array_lst[VTYPE_FLOAT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_STRING,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_STRING,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_array_lst[VTYPE_STRING]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_OBJECT,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_OBJECT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_array_lst[VTYPE_OBJECT]=a_ikeywords;

    atype_vtypeikeywordlst.push_back(a_vec_single_lst);
    atype_vtypeikeywordlst.push_back(a_vec_array_lst);
    return size;
}

int MvDescriptor_t::getAllIKeywords(MvDomain_e domain, map< int, vector<MvIKeywordList_t> >  &map_type_keywordlst) const
{
    int size = 0, loc_size = 0;
    vector<MvIKeywordList_t>  a_vec_single_lst(VTYPE_LAST);
    vector<MvIKeywordList_t>  a_vec_array_lst(VTYPE_LAST);
    MvIKeywordList_t a_ikeywords;

    //single
    getIKeywords(domain,ATYPE_VALUE,VTYPE_BOOL,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_single_lst[VTYPE_BOOL]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_VALUE,VTYPE_UINT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
       a_vec_single_lst[VTYPE_UINT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_VALUE,VTYPE_INT,&a_ikeywords);
    getIKeywords(domain,ATYPE_SIZE,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
      a_vec_single_lst[VTYPE_INT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_VALUE,VTYPE_FLOAT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_single_lst[VTYPE_FLOAT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_VALUE,VTYPE_STRING,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_single_lst[VTYPE_STRING]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_VALUE,VTYPE_OBJECT,&a_ikeywords);

    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
       a_vec_single_lst[VTYPE_OBJECT]=a_ikeywords;

    //array
    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_BOOL,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_BOOL,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_array_lst[VTYPE_BOOL]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_UINT,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_UINT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
       a_vec_array_lst[VTYPE_UINT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_INT,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_INT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
        a_vec_array_lst[VTYPE_INT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_FLOAT,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_FLOAT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
       a_vec_array_lst[VTYPE_FLOAT]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_STRING,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_STRING,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
       a_vec_array_lst[VTYPE_STRING]=a_ikeywords;

    a_ikeywords.clear();
    getIKeywords(domain,ATYPE_STATIC_ARRAY,VTYPE_OBJECT,&a_ikeywords);
    getIKeywords(domain,ATYPE_DYNAMIC_ARRAY,VTYPE_OBJECT,&a_ikeywords);
    loc_size = (int)a_ikeywords.size();
    size += loc_size;
    if (loc_size)
       a_vec_array_lst[VTYPE_OBJECT]=a_ikeywords;
    map_type_keywordlst[0] = a_vec_single_lst;
    map_type_keywordlst[1] = a_vec_array_lst;
    return size;
}

MvIKeywordList_t* MvDescriptor_t::getAllIKeywordsHavingDefaults(int domains, MvIKeywordList_t* ikw_list_p) const
{
    if (ikw_list_p == NULL) ikw_list_p = new MvIKeywordList_t();
    bool a_found = false;
    const MvDefaults_t& a_defaults = (*((const MvDefaults_t*)myDefaultsPtr));
    for (int i = (int)(DOM_UNKNOWN + 1); (!a_found) && i < (int)DOM_LAST; i <<= 1) if (domains & i) {
        MvDomain_e a_domain = (MvDomain_e)i;
        MvDefaults_t::const_iterator a_def_it = a_defaults.find(a_domain);
        //
        if (a_def_it != a_defaults.end()) {
            const MvDefaultMapArray_t& a_dma = (*a_def_it).second;
            const MvIntDefaultMap_t& a_intdefmap = *((MvIntDefaultMap_t*)(a_dma[VTYPE_INT]));
            // add all ikeywords
            for (auto &a_it : a_intdefmap)
            {
                int a_ikeyword = a_it.first;
                ikw_list_p->push_back(a_ikeyword);
            }
            const MvUIntDefaultMap_t& a_uintdefmap = *((MvUIntDefaultMap_t*)(a_dma[VTYPE_UINT]));
            for (auto& a_it : a_uintdefmap)
            {
                int a_ikeyword = a_it.first;
                ikw_list_p->push_back(a_ikeyword);
            }
            const MvFloatDefaultMap_t& a_floatdefmap = *((MvFloatDefaultMap_t*)(a_dma[VTYPE_FLOAT]));
            for (auto& a_it : a_floatdefmap)
            {
                int a_ikeyword = a_it.first;
                ikw_list_p->push_back(a_ikeyword);
            }
            const MvStringDefaultMap_t& a_stringdefmap = *((MvStringDefaultMap_t*)(a_dma[VTYPE_STRING]));
            for (auto& a_it : a_stringdefmap)
            {
                int a_ikeyword = a_it.first;
                ikw_list_p->push_back(a_ikeyword);
            }
            const MvIntDefaultMap_t& a_booldefmap = *((MvIntDefaultMap_t*)(a_dma[VTYPE_BOOL]));
            for (auto& a_it : a_booldefmap)
            {
                int a_ikeyword = a_it.first;
                ikw_list_p->push_back(a_ikeyword);
            }
            const MvObjectDefaultMap_t& a_objectdefmap = *((MvObjectDefaultMap_t*)(a_dma[VTYPE_OBJECT]));
            for (auto& a_it : a_objectdefmap)
            {
                int a_ikeyword = a_it.first;
                ikw_list_p->push_back(a_ikeyword);
            }
        }
    }
    return ikw_list_p;
}

void MvDescriptor_t::getIdentifiers(MvIKeywordList_t *pikeyidentifierlst) const
{
    if(!pikeyidentifierlst)
        return;
    int ikw_max = getMaxIKeyword();
    int k = 0;
    pikeyidentifierlst->reserve(ikw_max+1);
    for(k=0; k <ikw_max+1; ++k)
        pikeyidentifierlst->push_back(0);

    MvIntIdentifierMap_t  iden_lst;
    getIdentifierValue(DOM_COMMON, iden_lst);
    MvIntIdentifierMap_t::iterator it = iden_lst.begin();
    for (it = iden_lst.begin(); it != iden_lst.end(); ++it) {
        int  value = it->second;
        int  ikey = it->first;
        (*pikeyidentifierlst)[ikey] = value;
    }
    for(k=0; k <ikw_max+1; ++k)
        if((*pikeyidentifierlst)[k] > 0)
            break;
    if(k == ikw_max)
        pikeyidentifierlst = NULL;
}

int MvDescriptor_t::getMaxIKeyword() const
{
    int ikw_max = 0;
    MCDS_get_descriptor_max_ikeyword(myDescriptorPtr,&ikw_max);
    return ikw_max;
}

//void MvDescriptor_t::getMultiObjectTypes(int ikeyword, MvFullTypeSet_t& set) const
//{
//    const descriptor_t* cdescr_p = getDescriptorPtr();
//    if (cdescr_p == NULL)
//        return;
//    object_descriptor_t* objdescr_p = (object_descriptor_t*)(cdescr_p->attdescr_array[ikeyword]);
//    if (objdescr_p == NULL)
//        return;
//    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
//    CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
//    if (!a_cfgkernel)
//        return;
//    for (int i = 0; i < objdescr_p->num; i++)
//    {
//        if (objdescr_p->subtypes[i])
//        {
//            const MvSubtype_t* subtype = a_cfgkernel->get_subtype(objdescr_p->allowed_types[i], objdescr_p->subtypes[i]);
//            if (subtype == NULL)
//            {
//                vector<string> key_vect;
//                string string_val = objdescr_p->subtypes[i];
//                a_cfgkernel->getChildKeywordListForGivenKeyword(objdescr_p->allowed_types[i], string_val, key_vect);
//                vector<string>::iterator iter_b = key_vect.begin();
//                vector<string>::iterator iter_e = key_vect.end();
//                vector<string>::iterator iter;
//                for (iter = iter_b; iter != iter_e; iter++)
//                {
//                    string keyw = *iter;
//                    MvFullType_t ftype(objdescr_p->allowed_types[i], keyw);
//                    set.insert(ftype);
//                }
//            }
//            else
//            {
//                // insert subtype if found
//                MvFullType_t ftype(objdescr_p->allowed_types[i], objdescr_p->subtypes[i]);
//                set.insert(ftype);
//            }
//        }
//        else
//        {
//            MvFullType_t ftype(objdescr_p->allowed_types[i]);
//            set.insert(ftype);
//        }
//    }
//}

/* --------- Iterators --------- */

MvDescriptor_t::iterator::iterator(const MvDescriptor_t *descr_p,int ikeyword) : 
  myDescriptorPtr(descr_p), 
  myIKeyword(ikeyword) 
{}

const MvDescriptor_t::iterator &MvDescriptor_t::iterator::operator++() { 
  iterator::myIKeyword=myDescriptorPtr->getNextIKeyword(myIKeyword); 
  return *this; 
}

bool MvDescriptor_t::iterator::operator==(const MvDescriptor_t::iterator &it) const {
  return myIKeyword==it.myIKeyword && myDescriptorPtr==it.myDescriptorPtr;
}

bool MvDescriptor_t::iterator::operator!=(const MvDescriptor_t::iterator &it) const {
  return myIKeyword!=it.myIKeyword || myDescriptorPtr!=it.myDescriptorPtr;
}


/* --------- Common data --------- */

attribute_type_e MvDescriptor_t::getAttributeType(int ikeyword) const {
  attribute_type_e a_result = ATYPE_UNKNOWN;
  MCDS_get_descriptor_attributes(myDescriptorPtr,ikeyword,DESCR_ATTRIB_TYPE,&a_result,END_ARGS);
  return a_result;
}

value_type_e MvDescriptor_t::getValueType(int ikeyword) const {
  value_type_e a_result = VTYPE_UNKNOWN;
  MCDS_get_descriptor_attributes(myDescriptorPtr,ikeyword,DESCR_VALUE_TYPE,&a_result,END_ARGS);
  return a_result;
}

string MvDescriptor_t::getComment(int ikeyword) const {
  char *a_result=NULL;
  MCDS_get_descriptor_attributes(myDescriptorPtr,ikeyword,DESCR_COMMENT,&a_result,END_ARGS);
  return a_result;
}

string MvDescriptor_t::getSolverName(int ikeyword) const {
    char *a_result = NULL;
    MCDS_get_descriptor_attributes(myDescriptorPtr,ikeyword,DESCR_SOLVER_NAME,&a_result,END_ARGS);
    return a_result;
}

int MvDescriptor_t::getAttributeLength(int ikeyword) const {
    int a_result = 0;
    MCDS_get_descriptor_attributes(myDescriptorPtr,ikeyword,DESCR_LENGTH,&a_result,END_ARGS);
    return a_result;
}

/* --------- Object data --------- */

object_type_e MvDescriptor_t::getObjectType(int ikeyword) const {
  object_type_e a_result= HCDI_OBJ_TYPE_NULL;
  MCDS_get_descriptor_attributes(myDescriptorPtr,ikeyword,DESCR_OBJECT_TYPE,&a_result,END_ARGS);
  return a_result;
}

bool MvDescriptor_t::isMultiType(int ikeyword) const {
  object_type_e a_result= HCDI_OBJ_TYPE_NULL;
  MCDS_get_descriptor_attributes(myDescriptorPtr,ikeyword,DESCR_OBJECT_TYPE,&a_result,END_ARGS);
  if(a_result == HCDI_OBJ_TYPE_MULTIOBJECT || a_result == HCDI_OBJ_TYPE_SETS || a_result == HCDI_OBJ_TYPE_ELEMS)
      return true;
  return false;
}
bool MvDescriptor_t::hasObjectAttribSubtype(int ikeyword, int *nb_subtype) const {
    object_descriptor_t *objdescr_p = (object_descriptor_t *)(myDescriptorPtr->attdescr_array[ikeyword]);
    *nb_subtype = objdescr_p->num;
    return (objdescr_p->num == 0) ? false : true;
}
/* --------- Array data --------- */

void MvDescriptor_t::getDimensionSize(int ikeyword,MvSizeVector & sizeArrayVector) const 
{
  int dim = 0;
  dimension_size_t * tab = NULL;
  MCDS_get_descriptor_attributes(myDescriptorPtr,ikeyword,DESCR_DIMENSION,&dim,DESCR_MULTI_SIZE,&tab,END_ARGS);
  for(int i=0; i< dim;i++){
     sizeArrayVector.push_back(tab[i]);//[i] = tab[i];
  }
}

bool MvDescriptor_t::isMultiDimensionalArray(int ikeyword) const
{
  int dim = 0;
  MCDS_get_descriptor_attributes(myDescriptorPtr,ikeyword,DESCR_DIMENSION,&dim,END_ARGS);
  if(dim>1)
    return true;
  else
    return false;
}


int MvDescriptor_t::getSize(int ikeyword) const {
  int a_result=0;
  MCDS_get_descriptor_attributes(myDescriptorPtr,ikeyword,DESCR_SIZE,&a_result,END_ARGS);
  return a_result;
}

int MvDescriptor_t::getSizeIKeyword(int ikeyword) const {
  int a_result=0;
  MCDS_get_descriptor_attributes(myDescriptorPtr,ikeyword,DESCR_SIZE_IKEYWORD,&a_result,END_ARGS);
  return a_result;
}

string MvDescriptor_t::getSizeSKeyword(int ikeyword) const {
  return getSKeyword(getSizeIKeyword(ikeyword));
}

MvIKeywordSet_t *MvDescriptor_t::getSizeConnectedIKeywords(int size_ikw,MvIKeywordSet_t *ikw_set_p) const {
  if(ikw_set_p==NULL) ikw_set_p=new MvIKeywordSet_t();
  //
  int a_nb_conn_ikws=0;
  MCDS_get_descriptor_attributes(myDescriptorPtr,size_ikw,DESCR_SIZE_NB_CONN_IKEYWORDS,&a_nb_conn_ikws,END_ARGS);
  for(int i=0;i<a_nb_conn_ikws;++i) {
    int a_conn_ikw=END_ARGS;
    MCDS_get_descriptor_tab(myDescriptorPtr,size_ikw,DESCR_SIZE_CONN_IKEYWORD,i,(void *)(&a_conn_ikw));
    if(a_conn_ikw!=END_ARGS) ikw_set_p->insert(a_conn_ikw);
  }
  //
  return ikw_set_p;
}



bool MvDescriptor_t::isMainArray(int ikeyword) const {
  const MvIKeywordSet_t& a_main_ikeywords = getDefinition(DOM_COMMON, "MAINS");
  if(a_main_ikeywords.find(ikeyword) != a_main_ikeywords.end())
    {
      return true;
    }
  else
    {
      return false;
    }
}





MvIKeywordSet_t* MvDescriptor_t::getMainArrayIKeywords(MvIKeywordSet_t* ikw_set_p) const
{
  if(ikw_set_p == NULL)
    {
      ikw_set_p = (MvIKeywordSet_t*)mymalloc(sizeof(MvIKeywordSet_t));
    }

  const MvIKeywordSet_t& a_main_ikeywords = getDefinition(DOM_COMMON, "MAINS");

  for(MvIKeywordSet_t::const_iterator a_it = a_main_ikeywords.begin();
      a_it != a_main_ikeywords.end();
      ++a_it)
    {
      ikw_set_p->insert(*a_it);
    }

  return ikw_set_p;
}



MvIKeywordSet_t* MvDescriptor_t::getSecondaryArrayIKeywords(int ikeyword, 
							MvIKeywordSet_t* ikw_set_p) const
{
  if(ikw_set_p == NULL)
    {
      ikw_set_p = (MvIKeywordSet_t*)malloc(sizeof(MvIKeywordSet_t));
    }

  
  int a_size_ikeyword = getSizeIKeyword(ikeyword);
  MvIKeywordSet_t a_array_ikws;
  getSizeConnectedIKeywords(a_size_ikeyword,&a_array_ikws);
  a_array_ikws-=ikeyword;
  (*ikw_set_p)+=a_array_ikws;
  

  return ikw_set_p;
}



bool MvDescriptor_t::isSecondaryArray(int ikeyword) const
{
  MvIKeywordSet_t a_ikeyword_set;
  int a_size_ikeyword = getSizeIKeyword(ikeyword);

  getSizeConnectedIKeywords(a_size_ikeyword, &a_ikeyword_set);
  for(MvIKeywordSet_t::iterator a_it = a_ikeyword_set.begin();
      a_it != a_ikeyword_set.end();
      ++a_it)
  {
      int a_ikywd = *a_it;
      if(a_ikywd != ikeyword && isMainArray(a_ikywd))
      {
          return true;
      }
  }
  return false;
}



MvIKeywordSet_t* MvDescriptor_t::getAlternateSupportIKeywords(MvIKeywordSet_t* ikw_set_p) const
{
  if(ikw_set_p == NULL)
    {
      ikw_set_p = (MvIKeywordSet_t*)malloc(sizeof(MvIKeywordSet_t));
    }

  const MvIKeywordSet_t& a_alternate_support_ikeywords = getDefinition(DOM_COMMON, "SUPPORTING");

  for(MvIKeywordSet_t::const_iterator a_it = a_alternate_support_ikeywords.begin();
      a_it != a_alternate_support_ikeywords.end();
      ++a_it)
  {
    ikw_set_p->insert(*a_it);
  }

  return ikw_set_p;
}


/* --------- Dependences --------- */

void MvDescriptor_t::addDependence(MvDomain_e domain,const MvDependence_t *depend_p) {
  LocDependenceListMap_t *a_dlm_p=(LocDependenceListMap_t *)myDependenceListMapPtr;
  (*a_dlm_p)[domain].push_back(depend_p);
}

MvDependenceList_t *MvDescriptor_t::getDependences(int domain,MvDependenceList_t *dl_p) const {
  if(dl_p==NULL) dl_p=new MvDependenceList_t();
  //
  const LocDependenceListMap_t *a_dlm_p=(const LocDependenceListMap_t *)myDependenceListMapPtr;
  //
  MvDomain_e a_dom_begin = MV_get_domain_begin();
  MvDomain_e a_dom_end   = MV_get_domain_end();
  MvDomain_e a_dom;
  for(a_dom=a_dom_begin;a_dom!=a_dom_end;a_dom=MV_get_domain_next(a_dom)) if(domain&a_dom) {
    LocDependenceListMap_t::const_iterator a_dlm_it=a_dlm_p->find(a_dom);
    if(a_dlm_it!=a_dlm_p->end()) {
      const MvDependenceList_t &a_dom_dl=(*a_dlm_it).second;
      MvDependenceList_t::const_iterator a_it_begin = a_dom_dl.begin();
      MvDependenceList_t::const_iterator a_it_end   = a_dom_dl.end();
      MvDependenceList_t::const_iterator a_it;
      for(a_it=a_it_begin;a_it!=a_it_end;++a_it) dl_p->push_back(*a_it);
    }
  }
  //
  return dl_p;
}


/* --------- Default values --------- */

bool MvDescriptor_t::hasDefault(int ikeyword,int domains) const {
  const MvDefaults_t &a_defaults = (*((const MvDefaults_t *)myDefaultsPtr));
  value_type_e        a_vtype    = getValueType(ikeyword);
  bool                a_found    = false;
  //
  for(int i=(int)(DOM_UNKNOWN+1);(!a_found) && i<(int)DOM_LAST;i<<=1) if(domains & i) {
    MvDomain_e                   a_domain = (MvDomain_e)i;
    MvDefaults_t::const_iterator a_def_it = a_defaults.find(a_domain);
    //
    if(a_def_it!=a_defaults.end()) {
      const MvDefaultMapArray_t &a_dma=(*a_def_it).second;
      //
      switch(a_vtype) {
      case VTYPE_INT:
      {
	const MvIntDefaultMap_t &a_defmap=*((MvIntDefaultMap_t *)(a_dma[VTYPE_INT]));
	MvIntDefaultMap_t::const_iterator it=a_defmap.find(ikeyword);
	a_found=(it!=a_defmap.end());      
      }
      break;
      case VTYPE_BOOL:
      {
	const MvIntDefaultMap_t &a_defmap=*((MvIntDefaultMap_t *)(a_dma[VTYPE_BOOL]));
	MvIntDefaultMap_t::const_iterator it=a_defmap.find(ikeyword);
	a_found=(it!=a_defmap.end());      
      }
      break;
      case VTYPE_UINT:
      {
	const MvUIntDefaultMap_t &a_defmap=*((MvUIntDefaultMap_t *)(a_dma[VTYPE_UINT]));
	MvUIntDefaultMap_t::const_iterator it=a_defmap.find(ikeyword);
	a_found=(it!=a_defmap.end());      
      }
      break;
      case VTYPE_FLOAT:
	{
	  const MvFloatDefaultMap_t &a_defmap=*((MvFloatDefaultMap_t *)(a_dma[VTYPE_FLOAT]));
	  MvFloatDefaultMap_t::const_iterator it=a_defmap.find(ikeyword);
	  a_found=(it!=a_defmap.end());      
	}
	break;
    case VTYPE_STRING:
      {
	const MvStringDefaultMap_t &a_defmap=*((MvStringDefaultMap_t *)(a_dma[VTYPE_STRING]));
	MvStringDefaultMap_t::const_iterator it=a_defmap.find(ikeyword);
	a_found=(it!=a_defmap.end());      
      }
      break;
    case VTYPE_OBJECT:
    {
        const MvObjectDefaultMap_t &a_defmap=*((MvObjectDefaultMap_t *)(a_dma[VTYPE_OBJECT]));
        MvObjectDefaultMap_t::const_iterator it=a_defmap.find(ikeyword);
        a_found=(it!=a_defmap.end());
    }
    break;
    default:
	a_found=false;
	break;
      }
    }
  }
  //
  return a_found;
}

bool MvDescriptor_t::getBoolDefaultValue(int ikeyword,int domains,bool *is_default_p) const { 
  // Checking value type
  if(getValueType(ikeyword)!=VTYPE_BOOL) {
    throw MvError_t("MvDescriptor_t::getBoolDefaultValue -> \"%s\" is not of bool type",
		    getSKeyword(ikeyword).c_str());
  }
  // Getting the value
  int                 a_value    = 0;
  bool                a_found    = false;
  const MvDefaults_t &a_defaults = (*((const MvDefaults_t *)myDefaultsPtr));
  for(int i=(int)(DOM_UNKNOWN+1);(!a_found) && i<(int)DOM_LAST;i<<=1) if(domains & i) {
    MvDomain_e                   a_domain = (MvDomain_e)i;
    MvDefaults_t::const_iterator a_def_it = a_defaults.find(a_domain);
    //
    if(a_def_it!=a_defaults.end()) {
      const MvDefaultMapArray_t &a_dma    = (*a_def_it).second;
      const MvIntDefaultMap_t   &a_defmap = *((MvIntDefaultMap_t *)(a_dma[getValueType(ikeyword)]));
      //
      MvIntDefaultMap_t::const_iterator a_it=a_defmap.find(ikeyword);
      a_found=(a_it!=a_defmap.end());
      if(a_found) a_value=(*a_it).second;
    }
  }
  //
  if(is_default_p!=NULL) *is_default_p=a_found;
  return a_value > 0 ? true : false;
}

int MvDescriptor_t::getIntDefaultValue(int ikeyword,int domains,bool *is_default_p) const {
  // Checking value type
  if(getValueType(ikeyword)!=VTYPE_INT) {
    throw MvError_t("MvDescriptor_t::getIntDefaultValue -> \"%s\" is not of integer type",
		    getSKeyword(ikeyword).c_str());
  }
  // Getting the value
  int                 a_value    = 0;
  bool                a_found    = false;
  const MvDefaults_t &a_defaults = (*((const MvDefaults_t *)myDefaultsPtr));
  for(int i=(int)(DOM_UNKNOWN+1);(!a_found) && i<(int)DOM_LAST;i<<=1) if(domains & i) {
    MvDomain_e                   a_domain = (MvDomain_e)i;
    MvDefaults_t::const_iterator a_def_it = a_defaults.find(a_domain);
    //
    if(a_def_it!=a_defaults.end()) {
      const MvDefaultMapArray_t &a_dma    = (*a_def_it).second;
      const MvIntDefaultMap_t   &a_defmap = *((MvIntDefaultMap_t *)(a_dma[getValueType(ikeyword)]));
      //
      MvIntDefaultMap_t::const_iterator a_it=a_defmap.find(ikeyword);
      a_found=(a_it!=a_defmap.end());
      if(a_found) a_value=(*a_it).second;
    }
  }
  //
  if(is_default_p!=NULL) *is_default_p=a_found;
  return a_value;
}

unsigned int MvDescriptor_t::getUIntDefaultValue(int ikeyword,int domains,bool *is_default_p) const {
  // Checking value type
  if(getValueType(ikeyword)!=VTYPE_UINT) {
    throw MvError_t("MvDescriptor_t::getUIntDefaultValue -> \"%s\" is not of unsigned integer type",
		    getSKeyword(ikeyword).c_str());
  }
  // Getting the value
  unsigned int                 a_value    = 0;
  bool                a_found    = false;
  const MvDefaults_t &a_defaults = (*((const MvDefaults_t *)myDefaultsPtr));
  for(int i=(int)(DOM_UNKNOWN+1);(!a_found) && i<(int)DOM_LAST;i<<=1) if(domains & i) {
    MvDomain_e                   a_domain = (MvDomain_e)i;
    MvDefaults_t::const_iterator a_def_it = a_defaults.find(a_domain);
    //
    if(a_def_it!=a_defaults.end()) {
      const MvDefaultMapArray_t &a_dma    = (*a_def_it).second;
      const MvUIntDefaultMap_t   &a_defmap = *((MvUIntDefaultMap_t *)(a_dma[VTYPE_UINT]));
      //
      MvUIntDefaultMap_t::const_iterator a_it=a_defmap.find(ikeyword);
      a_found=(a_it!=a_defmap.end());
      if(a_found) a_value=(*a_it).second;
    }
  }
  //
  if(is_default_p!=NULL) *is_default_p=a_found;
  return a_value;
}

void MvDescriptor_t::getIdentifierValue(int domains, MvIntIdentifierMap_t  &identifierlst) const {
    const MvIdentifiers_t &a_identifiers = (*((const MvIdentifiers_t *)myIdentifiersPtr));
    for(int i=(int)(DOM_UNKNOWN+1);i<(int)DOM_LAST;i<<=1) if(domains & i) {
        MvDomain_e                   a_domain = (MvDomain_e)i;
        MvIdentifiers_t::const_iterator a_def_it = a_identifiers.find(a_domain);
        //
        if(a_def_it!=a_identifiers.end()) {
            const MvIdentifierMapArray_t &a_dma    = (*a_def_it).second;
            const MvIntIdentifierMap_t   &a_defmap = a_dma.myIntIdentifierMap;
            identifierlst = a_defmap;
            //
        }
    }
    //
}


int MvDescriptor_t::getIdentifierValue(int domains, int ikeyword) const {
  // Getting the value
  int                 a_value    = 0;
  bool                a_found    = false;
  const MvIdentifiers_t &a_identifiers = (*((const MvIdentifiers_t *)myIdentifiersPtr));
  for(int i=(int)(DOM_UNKNOWN+1);(!a_found) && i<(int)DOM_LAST;i<<=1) if(domains & i) {
    MvDomain_e                   a_domain = (MvDomain_e)i;
    MvIdentifiers_t::const_iterator a_def_it = a_identifiers.find(a_domain);
    //
    if(a_def_it!=a_identifiers.end()) {
      const MvIdentifierMapArray_t &a_dma    = (*a_def_it).second;
      const MvIntIdentifierMap_t   &a_defmap = a_dma.myIntIdentifierMap;
      //
      MvIntDefaultMap_t::const_iterator a_it=a_defmap.find(ikeyword);
      a_found=(a_it!=a_defmap.end());
      if(a_found) a_value=(*a_it).second;
    }
  }
  //
  return a_value;
}

int MvDescriptor_t::getIdentifierValue(int domains, const string &skeyword) const {
  // Getting the value
  int                 a_value    = 0;
  bool                a_found    = false;
  int ikeyword = getIKeyword(skeyword);
  const MvIdentifiers_t &a_identifiers = (*((const MvIdentifiers_t *)myIdentifiersPtr));
  for(int i=(int)(DOM_UNKNOWN+1);(!a_found) && i<(int)DOM_LAST;i<<=1) if(domains & i) {
    MvDomain_e                   a_domain = (MvDomain_e)i;
    MvIdentifiers_t::const_iterator a_def_it = a_identifiers.find(a_domain);
    //
    if(a_def_it!=a_identifiers.end()) {
      const MvIdentifierMapArray_t &a_dma    = (*a_def_it).second;
      const MvIntIdentifierMap_t   &a_defmap = a_dma.myIntIdentifierMap;
      //
      MvIntDefaultMap_t::const_iterator a_it=a_defmap.find(ikeyword);
      a_found=(a_it!=a_defmap.end());
      if(a_found) a_value=(*a_it).second;
    }
  }
  //
  return a_value;
}
int MvDescriptor_t::getIKeywordFromIdentifierValue(int domains, int identifier) const {
  // Getting the value
  const MvIdentifiers_t &a_identifiers = (*((const MvIdentifiers_t *)myIdentifiersPtr));
  for(int i=(int)(DOM_UNKNOWN+1); i<(int)DOM_LAST;i<<=1) if(domains & i) {
    MvDomain_e                   a_domain = (MvDomain_e)i;
    MvIdentifiers_t::const_iterator a_def_it = a_identifiers.find(a_domain);
    //
    if(a_def_it!=a_identifiers.end()) {
      const MvIdentifierMapArray_t &a_dma    = (*a_def_it).second;
      const MvIntIdentifierMap_t   &a_defmap = a_dma.myIntIdentifierMap;
      //
	  MvIntIdentifierMap_t::const_iterator itr_beg = a_defmap.begin();
	  MvIntIdentifierMap_t::const_iterator itr_end = a_defmap.end();
	  MvIntIdentifierMap_t::const_iterator itr;

	  for(itr=itr_beg; itr!=itr_end; ++itr)
	  {
		  if((*itr).second == identifier)
			  return (*itr).first;
	  }
    }
  }
  //
  return 0;
}
double MvDescriptor_t::getFloatDefaultValue(int ikeyword,int domains,bool *is_default_p) const {
  // Checking value type
  if(getValueType(ikeyword)!=VTYPE_FLOAT) {
    throw MvError_t("MvDescriptor_t::getFloatDefaultValue -> \"%s\" is not of Float type",
		    getSKeyword(ikeyword).c_str());
  }
  double a_value=0.;
  // Getting the value
  bool                a_found    = false;
  const MvDefaults_t &a_defaults = (*((const MvDefaults_t *)myDefaultsPtr));
  for(int i=(int)(DOM_UNKNOWN+1);(!a_found) && i<(int)DOM_LAST;i<<=1) if(domains & i) {
    MvDomain_e                   a_domain = (MvDomain_e)i;
    MvDefaults_t::const_iterator a_def_it = a_defaults.find(a_domain);
    //
    if(a_def_it!=a_defaults.end()) {
      const MvDefaultMapArray_t  &a_dma   = (*a_def_it).second;
      const MvFloatDefaultMap_t &a_defmap = *((MvFloatDefaultMap_t *)(a_dma[VTYPE_FLOAT]));
      //
      MvFloatDefaultMap_t::const_iterator a_it=a_defmap.find(ikeyword);
      a_found=(a_it!=a_defmap.end());
      if(a_found) a_value=(*a_it).second;
    }
  }
  //
  if(is_default_p!=NULL) *is_default_p=a_found;
  return a_value;
}

string MvDescriptor_t::getStringDefaultValue(int ikeyword,int domains,bool *is_default_p) const {
  // Checking value type
  if(getValueType(ikeyword)!=VTYPE_STRING) {
    throw MvError_t("MvDescriptor_t::getStringDefaultValue -> \"%s\" is not of string type",
		    getSKeyword(ikeyword).c_str());
  }
  // Getting the value
  string              a_value    = "";
  bool                a_found    = false;
  const MvDefaults_t &a_defaults = (*((const MvDefaults_t *)myDefaultsPtr));
  for(int i=(int)(DOM_UNKNOWN+1);(!a_found) && i<(int)DOM_LAST;i<<=1) if(domains & i) {
    MvDomain_e                   a_domain = (MvDomain_e)i;
    MvDefaults_t::const_iterator a_def_it = a_defaults.find(a_domain);
    //
    if(a_def_it!=a_defaults.end()) {
      const MvDefaultMapArray_t  &a_dma    = (*a_def_it).second;
      const MvStringDefaultMap_t &a_defmap = *((MvStringDefaultMap_t *)(a_dma[VTYPE_STRING]));
      //
      MvStringDefaultMap_t::const_iterator a_it=a_defmap.find(ikeyword);
      a_found=(a_it!=a_defmap.end());
      if(a_found) a_value=(*a_it).second;
    }
  }
  //
  if(is_default_p!=NULL) *is_default_p=a_found;
  return a_value;
}
void MvDescriptor_t::setBoolDefaultValue(int ikeyword,MvDomain_e domain,bool value) {
  // Checking value type
  if(getValueType(ikeyword) != VTYPE_BOOL) {
    throw MvError_t("MvDescriptor_t::setBoolDefaultValue -> \"%s\" is not of bool type",
		    getSKeyword(ikeyword).c_str());
  }
  // Setting the value
  MvDefaults_t      &a_defaults = (*((MvDefaults_t *)myDefaultsPtr));
  MvIntDefaultMap_t &a_defmap   = *((MvIntDefaultMap_t *)(a_defaults[domain][getValueType(ikeyword)]));
  int a_val = 0;
  if(value == true)
     a_val = 1;
  a_defmap[ikeyword]= a_val;
}
void MvDescriptor_t::setIntDefaultValue(int ikeyword,MvDomain_e domain,int value) {

  // Checking value type
  if(getValueType(ikeyword)!=VTYPE_INT) {
    throw MvError_t("MvDescriptor_t::setIntDefaultValue -> \"%s\" is not of integer type",
		    getSKeyword(ikeyword).c_str());
  }
  // Setting the value
  MvDefaults_t      &a_defaults = (*((MvDefaults_t *)myDefaultsPtr));
  MvIntDefaultMap_t &a_defmap   = *((MvIntDefaultMap_t *)(a_defaults[domain][getValueType(ikeyword)]));
  a_defmap[ikeyword]=value;
}

void MvDescriptor_t::setUIntDefaultValue(int ikeyword,MvDomain_e domain,unsigned int value) {
  // Checking value type
  if(getValueType(ikeyword)!=VTYPE_UINT) {
    throw MvError_t("MvDescriptor_t::getUIntDefaultValue -> \"%s\" is not of unsigned integer type",
		    getSKeyword(ikeyword).c_str());
  }
  // Setting the value
  MvDefaults_t      &a_defaults = (*((MvDefaults_t *)myDefaultsPtr));
  MvUIntDefaultMap_t &a_defmap   = *((MvUIntDefaultMap_t *)(a_defaults[domain][VTYPE_UINT]));
  a_defmap[ikeyword]=value;
}

void MvDescriptor_t::setIntIdentifierValue(int ikeyword,MvDomain_e domain,int value) {
    // Setting the value
  MvIdentifiers_t      &a_identifiers = (*((MvIdentifiers_t *)myIdentifiersPtr));
  MvIntIdentifierMap_t &a_defmap   = *((MvIntIdentifierMap_t *)(a_identifiers[domain][VTYPE_INT]));
  a_defmap[ikeyword]=value;
}
void MvDescriptor_t::setFloatDefaultValue(int ikeyword,MvDomain_e domain,double value) {
  // Checking value type
  if(getValueType(ikeyword)!=VTYPE_FLOAT) {
    throw MvError_t("MvDescriptor_t::setFloatDefaultValue -> \"%s\" is not of float type",
		    getSKeyword(ikeyword).c_str());
  }
  // Setting the value
  MvDefaults_t        &a_defaults = (*((MvDefaults_t *)myDefaultsPtr));
  MvFloatDefaultMap_t &a_defmap   = *((MvFloatDefaultMap_t *)(a_defaults[domain][VTYPE_FLOAT]));
  a_defmap[ikeyword]=value;
}

void MvDescriptor_t::setStringDefaultValue(int ikeyword,MvDomain_e domain,const string &value) {
  // Checking value type
  if(getValueType(ikeyword)!=VTYPE_STRING) {
    throw MvError_t("MvDescriptor_t::setStingDefaultValue -> \"%s\" is not of string type",
		    getSKeyword(ikeyword).c_str());
  }
  // Setting the value
  MvDefaults_t         &a_defaults = (*((MvDefaults_t *)myDefaultsPtr));
  MvStringDefaultMap_t &a_defmap   = *((MvStringDefaultMap_t *)(a_defaults[domain][VTYPE_STRING]));
  a_defmap[ikeyword]=value;
}

void MvDescriptor_t::setObjectDefaultValue(int ikeyword,MvDomain_e domain) {

    // Checking value type
    if(getValueType(ikeyword)!=VTYPE_OBJECT) {
        throw MvError_t("MvDescriptor_t::setObjectDefaultValue -> \"%s\" is not of object type",
                        getSKeyword(ikeyword).c_str());
    }
    // Setting the value
    MvDefaults_t      &a_defaults = (*((MvDefaults_t *)myDefaultsPtr));
    MvIntDefaultMap_t &a_defmap   = *((MvIntDefaultMap_t *)(a_defaults[domain][VTYPE_OBJECT]));
    a_defmap[ikeyword]=0;
}


/* --------- Default values (protected) --------- */



/* --------- Domains --------- */

int MvDescriptor_t::getNbDomains() const {
  return (int)(((LocDataFeatureListMap_t *)myDataFeatureListMapPtr)->size());
}


/* --------- Data features --------- */

void MvDescriptor_t::setDataFeature(MvDomain_e domain,const MvDataFeature_t *df_p, bool skip_reduced) {
  LocDataFeatureListMap_t *a_dflm_p=(LocDataFeatureListMap_t *)myDataFeatureListMapPtr;
  //
  (*a_dflm_p)[domain].push_back(df_p);

  if (!skip_reduced)
  {
      if (myDataFeatureReducedListMapPtr == NULL)
      {
          myDataFeatureReducedListMapPtr = (MvPseudoDataFeatureListMap_t*)(new LocDataFeatureListMap_t());
      }
      LocDataFeatureListMap_t* a_dflmreduced_p = (LocDataFeatureListMap_t*)myDataFeatureReducedListMapPtr;
      (*a_dflmreduced_p)[domain].push_back(df_p);
  }
}

MvDomain_e MvDescriptor_t::getDomain(const MvDataFeature_t *df_p) const {
  MvDomain_e                    a_result = DOM_UNKNOWN;
  const LocDataFeatureListMap_t *a_dflm_p = (const LocDataFeatureListMap_t *)myDataFeatureListMapPtr;
  //
  LocDataFeatureListMap_t::const_iterator a_it_begin = a_dflm_p->begin();
  LocDataFeatureListMap_t::const_iterator a_it_end   = a_dflm_p->end();
  LocDataFeatureListMap_t::const_iterator a_it;
  for(a_it=a_it_begin;a_result==DOM_UNKNOWN && a_it!=a_it_end;++a_it) {
    MvDomain_e                 a_domain = (*a_it).first;
    const MvDataFeatureList_t &a_dfl    = (*a_it).second;
    //
    MvDataFeatureList_t::const_iterator a_dfl_it_begin = a_dfl.begin();
    MvDataFeatureList_t::const_iterator a_dfl_it_end   = a_dfl.end();
    MvDataFeatureList_t::const_iterator a_dfl_it;
    for(a_dfl_it=a_dfl_it_begin;a_result==DOM_UNKNOWN && a_dfl_it!=a_dfl_it_end;++a_dfl_it) {
      const MvDataFeature_t *a_feature_p=(*a_dfl_it);
      if(a_feature_p==df_p) a_result=a_domain;
    }
  }
  //
  return a_result;
}

MvDataFeatureList_t *MvDescriptor_t::getDataFeatures(int domain,MvDataFeatureList_t *dfl_p, bool get_reduced) const {
  if(dfl_p==NULL) dfl_p=new MvDataFeatureList_t();
  //
  MvPseudoDataFeatureListMap_t* ptr = (get_reduced) ? myDataFeatureReducedListMapPtr : myDataFeatureListMapPtr;
  LocDataFeatureListMap_t *a_dflm_p=(LocDataFeatureListMap_t *)ptr;
  if(a_dflm_p!=NULL) {
    for(LocDataFeatureListMap_t::const_iterator a_dflm_it=a_dflm_p->begin();
	a_dflm_it!=a_dflm_p->end();
	++a_dflm_it) {
      if(domain & (*a_dflm_it).first) {
	const MvDataFeatureList_t &a_dfl=(*a_dflm_it).second;
	for(MvDataFeatureList_t::const_iterator a_dfl_it=a_dfl.begin();a_dfl_it!=a_dfl.end();++a_dfl_it) {
	  dfl_p->push_back(*a_dfl_it);
	}
      }
    }
  }
  return dfl_p;
}

static void LocAddDataFeatures(const MvDataFeature_t *df_p, MvDataFeatureType_e dft,
                               MvDataFeatureList_t *dfl_p, bool add_containing_array_feat)
{
    MvDataFeatureType_e a_dft = df_p->getType();
    if(a_dft == dft) dfl_p->push_back(df_p);
    if((DFT_ARRAY_TO_SINGLE == a_dft ||
        DFT_DYNAMIC_ARRAY == a_dft ||
        DFT_DYNAMIC_MATRIX == a_dft ||
        DFT_STATIC_ARRAY == a_dft) && add_containing_array_feat)
    { // recursive calls for the features of the array
        const MvDataArrayFeature_t *a_adf_p = static_cast<const MvDataArrayFeature_t *>(df_p);
        for(int i = 0; i < a_adf_p->getNumber(); ++i)
        {
            LocAddDataFeatures(a_adf_p->getDataFeature(i), dft, dfl_p, add_containing_array_feat);
        }
    }
}

MvDataFeatureList_t *MvDescriptor_t::getDataFeatures(int domain,MvDataFeatureType_e a_dft,
						     MvDataFeatureList_t *dfl_p, bool add_containing_array_feat) const 
{
  if(dfl_p==NULL) dfl_p=new MvDataFeatureList_t();
  //
  // Special treatment of selection features, they are only built on demand and stored in a separate map
  if(DFT_SELECTION == a_dft) {
      LocDataSelectionFeatureMap_t *a_sel_map_p = (LocDataSelectionFeatureMap_t *)mySelectionMapPtr;
      if(NULL == a_sel_map_p)
      {
          addSelectionFeatures(DOM_COMMON);
          a_sel_map_p = (LocDataSelectionFeatureMap_t *)mySelectionMapPtr;
      }

      LocDataSelectionFeatureMap_t::iterator it;
      LocDataSelectionFeatureMap_t::iterator it_begin = a_sel_map_p->begin();
      LocDataSelectionFeatureMap_t::iterator it_end = a_sel_map_p->end();
      for(it = it_begin; it != it_end; ++it)
      {
          dfl_p->push_back(it->second);
      }
      return dfl_p;
  }
  //
  LocDataFeatureListMap_t *a_dflm_p=(LocDataFeatureListMap_t *)myDataFeatureListMapPtr;
  if(a_dflm_p!=NULL) {
    for(LocDataFeatureListMap_t::const_iterator a_dflm_it=a_dflm_p->begin();
        a_dflm_it!=a_dflm_p->end();
        ++a_dflm_it)
    {
        if(domain & (*a_dflm_it).first) {
            const MvDataFeatureList_t &a_dfl=(*a_dflm_it).second;
            for(MvDataFeatureList_t::const_iterator a_dfl_it=a_dfl.begin();a_dfl_it!=a_dfl.end();++a_dfl_it) {
                LocAddDataFeatures(*a_dfl_it, a_dft, dfl_p, add_containing_array_feat);
            }
        }
    }
  }
  return dfl_p;
}

MvDataFeatureSet_t *MvDescriptor_t::getConditionalDataFeatures(int domain,MvDataFeatureSet_t *dfs_p) const {
  if(dfs_p==NULL) dfs_p=new MvDataFeatureSet_t();
  //
  MvDataFeatureList_t a_if_dfl;
  getDataFeatures(domain,DFT_IF,&a_if_dfl);
  MvDataFeatureList_t::const_iterator a_it_begin = a_if_dfl.begin();
  MvDataFeatureList_t::const_iterator a_it_end   = a_if_dfl.end();
  MvDataFeatureList_t::const_iterator a_it;
  MvDataFeatureList_t::const_iterator a_dfl_it_begin,a_dfl_it_end,a_dfl_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    const MvDataIfFeature_t *a_dif_p=(const MvDataIfFeature_t *)(*a_it);
    //
    const MvDataFeatureList_t &a_r_dfl=a_dif_p->getDefaultFeaturePtrList();
    a_dfl_it_begin = a_r_dfl.begin();
    a_dfl_it_end   = a_r_dfl.end();
    for(a_dfl_it=a_dfl_it_begin;a_dfl_it!=a_dfl_it_end;++a_dfl_it) dfs_p->insert(*a_dfl_it);
    //
    const MvDataFeatureList_t &a_w_dfl=a_dif_p->getDefaultWrongFeaturePtrList();
    a_dfl_it_begin = a_w_dfl.begin();
    a_dfl_it_end   = a_w_dfl.end();
    for(a_dfl_it=a_dfl_it_begin;a_dfl_it!=a_dfl_it_end;++a_dfl_it) dfs_p->insert(*a_dfl_it);
  }
  //
  return dfs_p;
}

MvDataFeatureSet_t *MvDescriptor_t::getConditionDataFeatures(int domain,MvDataFeatureSet_t *dfs_p) const {
  if(dfs_p==NULL) dfs_p=new MvDataFeatureSet_t();
  //
  MvDataFeatureList_t a_if_dfl;
  getDataFeatures(domain,DFT_IF,&a_if_dfl);
  MvDataFeatureList_t::const_iterator a_it_begin = a_if_dfl.begin();
  MvDataFeatureList_t::const_iterator a_it_end   = a_if_dfl.end();
  MvDataFeatureList_t::const_iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    const MvDataIfFeature_t *a_dif_p    = (const MvDataIfFeature_t *)(*a_it);
    (*dfs_p)+=a_dif_p->getTestFeatures();
  }
  //
  return dfs_p;
}


MvDataFeatureSet_t *MvDescriptor_t::getConditionalDimensionFeatures(MvDataFeatureType_e feature_type,
								    bool                array_subfeatures,
								    MvDataFeatureSet_t *dfs_p) const
{
  if(dfs_p==NULL) dfs_p=new MvDataFeatureSet_t();
  //
  MvDataFeatureList_t a_features;
  getDataFeatures(MV_get_all_domains(),feature_type,&a_features);
  //
  MvDataFeatureList_t::const_iterator a_it_begin = a_features.begin();
  MvDataFeatureList_t::const_iterator a_it_end   = a_features.end();
  MvDataFeatureList_t::const_iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) dfs_p->insert(*a_it);
  //
  if(array_subfeatures) {
    a_features.clear();
    getDataFeatures(MV_get_all_domains(),DFT_DYNAMIC_ARRAY,&a_features);
    getDataFeatures(MV_get_all_domains(),DFT_STATIC_ARRAY,&a_features);
    getDataFeatures(MV_get_all_domains(),DFT_ARRAY_TO_SINGLE,&a_features);
    //
    a_it_begin = a_features.begin();
    a_it_end   = a_features.end();
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
      const MvDataArrayFeature_t *a_daf_p       = (const MvDataArrayFeature_t *)(*a_it);
      int                         a_nb_features = a_daf_p->getNumber();
      //
      for(int i=0;i<a_nb_features;++i) {
	const MvDataFeature_t *a_feature_p    = a_daf_p->getDataFeature(i);
	MvDataFeatureType_e    a_feature_type = a_feature_p->getType();
	//
	if(a_feature_type==feature_type) dfs_p->insert(a_feature_p);
      }      
    }
  }
  //
  return dfs_p;
}



MvDataFeatureSet_t *MvDescriptor_t::getConditionalDimensionFeatures(MvDataFeatureSet_t *dfs_p) const
{
  
  MvDataFeatureSet_t *a_dfs_p=getConditionalDimensionFeatures(DFT_COND_SCALAR,true,dfs_p);
  return getConditionalDimensionFeatures(DFT_COND_FUNCTION,true,a_dfs_p);
  
}



MvDataFeatureSet_t *MvDescriptor_t::getConditionDimensionFeatures(MvDataFeatureSet_t *dfs_p) const
{
  if(dfs_p==NULL) dfs_p=new MvDataFeatureSet_t();
  //
  MvDataFeatureSet_t a_features;
  getConditionalDimensionFeatures(&a_features);
  //
  MvDataFeatureSet_t::const_iterator a_it_begin = a_features.begin();
  MvDataFeatureSet_t::const_iterator a_it_end   = a_features.end();
  MvDataFeatureSet_t::const_iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    
    const MvDataConditionalFeature_t *a_feature_p=dynamic_cast<const MvDataConditionalFeature_t *>(*a_it);
    a_feature_p->getConditionFeatures(dfs_p);
    
  }
  //
  return dfs_p;
}



MvDataFeatureSet_t *MvDescriptor_t::getConditionalDimensionFeatures(const MvDataFeature_t *cond_p,
								    MvDataFeatureType_e    feature_type,
								    bool                   array_subfeatures,
								    MvDataFeatureSet_t    *dfs_p) const
{
  if(dfs_p==NULL) dfs_p=new MvDataFeatureSet_t();
  //
  MvDataFeatureSet_t a_features;
  getConditionalDimensionFeatures(feature_type,array_subfeatures,&a_features);
  //
  MvDataFeatureSet_t::const_iterator a_it_begin = a_features.begin();
  MvDataFeatureSet_t::const_iterator a_it_end   = a_features.end();
  MvDataFeatureSet_t::const_iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    const MvDataFeature_t            *a_feature_p = (*a_it);
    const MvDataConditionalFeature_t *a_cf_p      = dynamic_cast<const MvDataConditionalFeature_t *>(a_feature_p);
    //
    MvDataFeatureSet_t a_conditions;
    a_cf_p->getConditionFeatures(&a_conditions);
    //
    if(cond_p<a_conditions) dfs_p->insert(a_feature_p);
  }
  //
  return dfs_p;
}



MvDataFeatureSet_t *MvDescriptor_t::getConditionalDimensionFeatures(const MvDataFeature_t *cond_p,
								    MvDataFeatureSet_t    *dfs_p) const
{
  
  MvDataFeatureSet_t *a_dfs_p=getConditionalDimensionFeatures(cond_p,DFT_COND_SCALAR,true,dfs_p);
  return getConditionalDimensionFeatures(cond_p,DFT_COND_FUNCTION,true,a_dfs_p);  
  
}


const MvDataFeature_t * loc_getIkeywordDataFeature(const MvDataFeature_t *df_p, int ikeyword)
{
    MvDataFeatureType_e feature_type = df_p->getType();
    switch (feature_type) {
    case DFT_DYNAMIC_ARRAY:
    case DFT_STATIC_ARRAY:
    case DFT_ARRAY_TO_SINGLE:
    case DFT_RADIO_ARRAY:
    case DFT_DYNAMIC_MATRIX:
    {
        // recursive calls for the children of array features
        assert(dynamic_cast<const MvDataArrayFeature_t*>(df_p) != NULL);
        const MvDataArrayFeature_t* arrayf_p = static_cast<const MvDataArrayFeature_t*>(df_p);
        int nb_features = arrayf_p->getNumber();
        for (int i = 0; i < nb_features; ++i)
        {
            const MvDataFeature_t* childf_p = arrayf_p->getDataFeature(i);
            childf_p = loc_getIkeywordDataFeature(childf_p, ikeyword);
            if (NULL != childf_p) return childf_p;
        }
    }
    break;
    case DFT_SCALAR:
    case DFT_COND_SCALAR:
    case DFT_SIZE:
    case DFT_SIZE_RADIO:
    case DFT_FILE:
    case DFT_DIR:
    case DFT_SKEW:
    case DFT_FLAG:
    case DFT_RADIO:
    case DFT_DATA:
    case DFT_SUBOBJECT:
    case DFT_UNIT:
    case DFT_SENSOR:
    case DFT_ACCELEROMETER:
    case DFT_APPEND:
    case DFT_ASSIGN:
    {
        assert(dynamic_cast<const MvDataSingleFeature_t*>(df_p) != NULL);
        int ikw = (static_cast<const MvDataSingleFeature_t*>(df_p))->getIKeyword();
        if (ikw == ikeyword) return df_p;
    }
    break;
    case DFT_TRIPLE:
    case DFT_POINT:
    {
        assert(dynamic_cast<const MvDataTripleFeature_t*>(df_p) != NULL);
        const MvDataTripleFeature_t* vf_p = static_cast<const MvDataTripleFeature_t*>(df_p);
        if (vf_p->getIKeyword() == ikeyword) return df_p;
    }
    break;
    case DFT_FUNCTION:
    case DFT_COND_FUNCTION:
    {
        assert(dynamic_cast<const MvDataFunctionFeature_t*>(df_p) != NULL);
        const MvDataFunctionFeature_t* ff_p =
            static_cast<const MvDataFunctionFeature_t*>(df_p);
        if (ff_p->getFunctionIKeyword() == ikeyword) return df_p;
        else if (ff_p->isXScaled() && ff_p->getXScalingIKeyword() == ikeyword) return df_p;
        else if (ff_p->isYScaled() && ff_p->getYScalingIKeyword() == ikeyword) return df_p;
    }
    break;
    default:
        break;
    }
    return NULL;
}

const MvDataFeature_t* MvDescriptor_t::getIkeywordDataFeature(int domains, int ikeyword) const
{
    MvDataFeatureList_t a_dfl;
    getDataFeatures(domains,&a_dfl);

    MvDataFeatureList_t::const_iterator a_it_begin = a_dfl.begin();
    MvDataFeatureList_t::const_iterator a_it_end   = a_dfl.end();
    MvDataFeatureList_t::const_iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it)
    {
        const MvDataFeature_t *df_p = (*a_it);
        df_p = loc_getIkeywordDataFeature(df_p, ikeyword);
        if(NULL != df_p) return df_p;
    }
    return NULL;
}


/* --------- Tests --------- */

void MvDescriptor_t::addTest(MvDomain_e domain,const MvTest_t *test_p) {
  LocTestListMap_t *a_tlm_p=(LocTestListMap_t *)myTestListMapPtr;
  //
  (*a_tlm_p)[domain].push_back(test_p);
}

MvTestList_t *MvDescriptor_t::getTests(int domains,MvTestList_t *tl_p) const {
  if(tl_p==NULL) tl_p=new MvTestList_t();
  //
  LocTestListMap_t *a_tlm_p=(LocTestListMap_t *)myTestListMapPtr;
  if(a_tlm_p!=NULL) {
    for(LocTestListMap_t::const_iterator a_tlm_it=a_tlm_p->begin();a_tlm_it!=a_tlm_p->end();++a_tlm_it) {
      if(domains & (*a_tlm_it).first) {
	const MvTestList_t &a_tl=(*a_tlm_it).second;
	for(MvTestList_t::const_iterator a_tl_it=a_tl.begin();a_tl_it!=a_tl.end();++a_tl_it) {
	  tl_p->push_back(*a_tl_it);
	}
      }
    }
  }
  //
  return tl_p;
}

/* --------- File formats --------- */

void MvDescriptor_t::addFileFormat(MvFileFormat_e ff_id,fileformat_t *ff_p) {  
  MCDS_add_descriptor_fileformat(myDescriptorPtr,ff_id,ff_p);
}

const fileformat_t *MvDescriptor_t::getFileFormatPtr(MvFileFormat_e ff_id) const {
  const fileformat_t *a_ff_p=NULL;
  MCDS_get_descriptor_fileformat(myDescriptorPtr,ff_id,&a_ff_p);
  return a_ff_p;
}

const fileformat_t *MvDescriptor_t::getLowerFileFormatPtr(MvFileFormat_e ff_id) const {
  const fileformat_t *a_ff_p=NULL;
  MCDS_get_descriptor_lower_fileformat(myDescriptorPtr,ff_id,&a_ff_p);
  return a_ff_p;
}

const fileformat_t* MvDescriptor_t::getDynaFileFormatPtr(MvFileFormat_e* ff_id_p) const {
    if (!ff_id_p)
        return NULL;
    const fileformat_t* a_format_p = getFileFormatPtr(*ff_id_p);
    if (a_format_p == NULL)
    {
        if (*ff_id_p >= FF_971R16)
            a_format_p = getFileFormatPtr(FF_971R16);
        if (a_format_p == NULL)
        {
            if (*ff_id_p >= FF_971R15)
                a_format_p = getFileFormatPtr(FF_971R15);
            if (a_format_p == NULL)
            {
                if (*ff_id_p >= FF_971R141)
                    a_format_p = getFileFormatPtr(FF_971R141);
                if (a_format_p == NULL)
                {
                    if (*ff_id_p >= FF_971R14)
                        a_format_p = getFileFormatPtr(FF_971R14);
                    if (a_format_p == NULL)
                    {
                        if (*ff_id_p >= FF_971R131)
                            a_format_p = getFileFormatPtr(FF_971R131);
                        if (a_format_p == NULL)
                        {
                            if (*ff_id_p >= FF_971R13)
                                a_format_p = getFileFormatPtr(FF_971R13);
                            if (a_format_p == NULL)
                            {
                                if (*ff_id_p >= FF_971R12)
                                    a_format_p = getFileFormatPtr(FF_971R12);
                                if (a_format_p == NULL)
                                {
                                    if (*ff_id_p >= FF_971R112)
                                        a_format_p = getFileFormatPtr(FF_971R112);
                                    if (a_format_p == NULL)
                                    {
                                        if (*ff_id_p >= FF_971R111)
                                            a_format_p = getFileFormatPtr(FF_971R111);
                                        if (a_format_p == NULL)
                                        {
                                            if (*ff_id_p >= FF_971R11)
                                                a_format_p = getFileFormatPtr(FF_971R11);
                                            if (a_format_p == NULL)
                                            {
                                                if (*ff_id_p >= FF_971R101)
                                                    a_format_p = getFileFormatPtr(FF_971R101);
                                                if (a_format_p == NULL)
                                                {
                                                    if (*ff_id_p >= FF_971R93)
                                                        a_format_p = getFileFormatPtr(FF_971R93);
                                                    if (a_format_p == NULL)
                                                    {
                                                        if (*ff_id_p >= FF_971R9)
                                                            a_format_p = getFileFormatPtr(FF_971R9);
                                                        if (a_format_p == NULL)
                                                        {
                                                            if (*ff_id_p >= FF_971R8)
                                                                a_format_p = getFileFormatPtr(FF_971R8);
                                                            if (a_format_p == NULL)
                                                            {
                                                                if (*ff_id_p >= FF_971R7)
                                                                    a_format_p = getFileFormatPtr(FF_971R7);
                                                                if (a_format_p == NULL)
                                                                {
                                                                    if (*ff_id_p >= FF_971R6)
                                                                        a_format_p = getFileFormatPtr(FF_971R6);
                                                                    if (a_format_p == NULL)
                                                                    {
                                                                        if (*ff_id_p >= FF_971R5)
                                                                            a_format_p = getFileFormatPtr(FF_971R5);
                                                                        if (a_format_p == NULL)
                                                                        {
                                                                            a_format_p = getFileFormatPtr(FF_971R4);
                                                                            if (a_format_p != NULL)
                                                                                *ff_id_p = FF_971R4;
                                                                        }
                                                                        else
                                                                            *ff_id_p = FF_971R5;
                                                                    }
                                                                    else
                                                                        *ff_id_p = FF_971R6;
                                                                }
                                                                else
                                                                    *ff_id_p = FF_971R7;
                                                            }
                                                            else
                                                                *ff_id_p = FF_971R8;
                                                        }
                                                        else
                                                            *ff_id_p = FF_971R9;
                                                    }
                                                    else
                                                        *ff_id_p = FF_971R93;
                                                }
                                                else
                                                    *ff_id_p = FF_971R101;
                                            }
                                            else
                                                *ff_id_p = FF_971R11;
                                        }
                                        else
                                            *ff_id_p = FF_971R111;
                                    }
                                    else
                                        *ff_id_p = FF_971R112;
                                }
                                else
                                    *ff_id_p = FF_971R12;
                            }
                            else
                                *ff_id_p = FF_971R13;
                        }
                        else
                            *ff_id_p = FF_971R131;
                    }
                    else
                        *ff_id_p = FF_971R14;
                }
                else
                    *ff_id_p = FF_971R141;
            }
            else
                *ff_id_p = FF_971R15;
        }
        else
            *ff_id_p = FF_971R16;
    }
    return a_format_p;
}



const fileformat_t *MvDescriptor_t::getRadiossFileFormatPtr(MvFileFormat_e ff_id) const
{
    const fileformat_t *a_ff_p=NULL;

    if (ff_id >= FF_D00_2026)                 a_ff_p = getFileFormatPtr(FF_D00_2026);
    if (a_ff_p == NULL && ff_id >= FF_D00_2025) a_ff_p = getFileFormatPtr(FF_D00_2025);
    if (a_ff_p == NULL && ff_id >= FF_D00_2024) a_ff_p = getFileFormatPtr(FF_D00_2024);
    if (a_ff_p == NULL && ff_id >= FF_D00_2023) a_ff_p = getFileFormatPtr(FF_D00_2023);
    if (a_ff_p == NULL && ff_id >= FF_D00_2022) a_ff_p = getFileFormatPtr(FF_D00_2022);
    if (a_ff_p == NULL && ff_id >= FF_D00_2021) a_ff_p = getFileFormatPtr(FF_D00_2021);
    if (a_ff_p == NULL && ff_id >= FF_D00_2020X) a_ff_p = getFileFormatPtr(FF_D00_2020X);
    if (a_ff_p==NULL && ff_id >= FF_D00_20200) a_ff_p=getFileFormatPtr(FF_D00_20200);
    if (a_ff_p==NULL && ff_id >= FF_D00_2019X) a_ff_p=getFileFormatPtr(FF_D00_2019X);
    if (a_ff_p==NULL && ff_id >= FF_D00_20190) a_ff_p=getFileFormatPtr(FF_D00_20190);
    if (a_ff_p==NULL && ff_id >= FF_D00_2018X) a_ff_p=getFileFormatPtr(FF_D00_2018X);
    if (a_ff_p==NULL && ff_id >= FF_D00_20180) a_ff_p=getFileFormatPtr(FF_D00_20180);
    if (a_ff_p==NULL && ff_id >= FF_D00_2017X) a_ff_p=getFileFormatPtr(FF_D00_2017X);
    if (a_ff_p==NULL && ff_id >= FF_D00_20170) a_ff_p=getFileFormatPtr(FF_D00_20170);
    if (a_ff_p==NULL && ff_id >= FF_D00_19X) a_ff_p=getFileFormatPtr(FF_D00_19X);
    if (a_ff_p==NULL && ff_id >= FF_D00_190) a_ff_p=getFileFormatPtr(FF_D00_190);
    if (a_ff_p==NULL && ff_id >= FF_D00_18X) a_ff_p=getFileFormatPtr(FF_D00_18X);
    if (a_ff_p==NULL && ff_id >= FF_D00_180) a_ff_p=getFileFormatPtr(FF_D00_180);
    if (a_ff_p==NULL && ff_id >= FF_D00_17X) a_ff_p=getFileFormatPtr(FF_D00_17X);
    if (a_ff_p==NULL && ff_id >= FF_D00_170) a_ff_p=getFileFormatPtr(FF_D00_170);
    if (a_ff_p==NULL && ff_id >= FF_D00_16X) a_ff_p=getFileFormatPtr(FF_D00_16X);
    if (a_ff_p==NULL && ff_id >= FF_D00_160) a_ff_p=getFileFormatPtr(FF_D00_160);
    if (a_ff_p==NULL && ff_id >= FF_D00_15X) a_ff_p=getFileFormatPtr(FF_D00_15X);
    if (a_ff_p==NULL && ff_id >= FF_D00_150) a_ff_p=getFileFormatPtr(FF_D00_150);
    if (a_ff_p==NULL && ff_id >= FF_D00_14X) a_ff_p=getFileFormatPtr(FF_D00_14X);
    if (a_ff_p==NULL && ff_id >= FF_D00_140) a_ff_p=getFileFormatPtr(FF_D00_140);
    if (a_ff_p==NULL && ff_id >= FF_D00_13X) a_ff_p=getFileFormatPtr(FF_D00_13X);
    if (a_ff_p==NULL && ff_id >= FF_D00_130) a_ff_p=getFileFormatPtr(FF_D00_130);
    if (a_ff_p==NULL && ff_id >= FF_D00_12X) a_ff_p=getFileFormatPtr(FF_D00_12X);
    if (a_ff_p==NULL && ff_id >= FF_D00_120) a_ff_p=getFileFormatPtr(FF_D00_120);
    if (a_ff_p==NULL && ff_id >= FF_D00_11X) a_ff_p=getFileFormatPtr(FF_D00_11X);
    if (a_ff_p==NULL && ff_id >= FF_D00_110) a_ff_p=getFileFormatPtr(FF_D00_110);
    if (a_ff_p==NULL && ff_id >= FF_D00_10X) a_ff_p=getFileFormatPtr(FF_D00_10X);
    if (a_ff_p==NULL && ff_id >= FF_D00_100) a_ff_p=getFileFormatPtr(FF_D00_100);
    if (a_ff_p==NULL && ff_id >= FF_D00_9X)  a_ff_p=getFileFormatPtr(FF_D00_9X);
    if (a_ff_p==NULL && ff_id >= FF_D00_90)  a_ff_p=getFileFormatPtr(FF_D00_90);
    if (a_ff_p==NULL && ff_id >= FF_D00_5X)  a_ff_p=getFileFormatPtr(FF_D00_5X);
    if (a_ff_p==NULL && ff_id >= FF_D00_52)  a_ff_p=getFileFormatPtr(FF_D00_52);
    if (a_ff_p==NULL && ff_id >= FF_D00_51)  a_ff_p=getFileFormatPtr(FF_D00_51);
    if (a_ff_p==NULL && ff_id >= FF_D00_4X)  a_ff_p=getFileFormatPtr(FF_D00_4X);
    if (a_ff_p==NULL && ff_id >= FF_D00_44)  a_ff_p=getFileFormatPtr(FF_D00_44);
    if (a_ff_p==NULL && ff_id >= FF_D00_43)  a_ff_p=getFileFormatPtr(FF_D00_43);
    if (a_ff_p==NULL && ff_id >= FF_D00_42)  a_ff_p=getFileFormatPtr(FF_D00_42);
    if (a_ff_p==NULL && ff_id >= FF_D00_41B) a_ff_p=getFileFormatPtr(FF_D00_41B);
    if (a_ff_p==NULL && ff_id >= FF_D00_41F) a_ff_p=getFileFormatPtr(FF_D00_41F);
    if (a_ff_p==NULL && ff_id >= FF_D00_41)  a_ff_p=getFileFormatPtr(FF_D00_41);

    return a_ff_p;
}

const fileformat_t *MvDescriptor_t::getDynaFileFormatPtr(MvFileFormat_e ff_id) const
{
    const fileformat_t *a_ff_p=NULL;

    if (ff_id >= FF_971R14)                a_ff_p=getFileFormatPtr(FF_971R14);
    if (a_ff_p==NULL && ff_id >= FF_971R131) a_ff_p=getFileFormatPtr(FF_971R131);
    if (a_ff_p==NULL && ff_id >= FF_971R13) a_ff_p=getFileFormatPtr(FF_971R13);
    if (a_ff_p==NULL && ff_id >= FF_971R12) a_ff_p=getFileFormatPtr(FF_971R12);
    if (a_ff_p==NULL && ff_id >= FF_971R112) a_ff_p=getFileFormatPtr(FF_971R112);
    if (a_ff_p==NULL && ff_id >= FF_971R111) a_ff_p=getFileFormatPtr(FF_971R111);
    if (a_ff_p==NULL && ff_id >= FF_971R11) a_ff_p=getFileFormatPtr(FF_971R11);
    if (a_ff_p==NULL && ff_id >= FF_971R101) a_ff_p=getFileFormatPtr(FF_971R101);
    if (a_ff_p==NULL && ff_id >= FF_971R93) a_ff_p=getFileFormatPtr(FF_971R93);
    if (a_ff_p==NULL && ff_id >= FF_971R9) a_ff_p=getFileFormatPtr(FF_971R9);
    if (a_ff_p==NULL && ff_id >= FF_971R8) a_ff_p=getFileFormatPtr(FF_971R8);
    if (a_ff_p==NULL && ff_id >= FF_971R7) a_ff_p=getFileFormatPtr(FF_971R7);
    if (a_ff_p==NULL && ff_id >= FF_971R6) a_ff_p=getFileFormatPtr(FF_971R6);
    if (a_ff_p==NULL && ff_id >= FF_971R5) a_ff_p=getFileFormatPtr(FF_971R5);
    if (a_ff_p==NULL && ff_id >= FF_971R4) a_ff_p=getFileFormatPtr(FF_971R4);
    if (a_ff_p==NULL && ff_id >= FF_DYNA) a_ff_p=getFileFormatPtr(FF_DYNA);

    return a_ff_p;
}

void MvDescriptor_t::postTreatFileFormat(MvFileFormat_e ff_id)
{
  const fileformat_t *ff_p = getFileFormatPtr(ff_id);
  /*NB: We have similar mapping exists in meci_read_d00_base.cpp and 
    meci_write_d00_base.cpp as local functions (loc_get_file_format_ptr).
   */
  if (NULL == ff_p)
  {
      ff_p = getLowerFileFormatPtr(ff_id);
  }

  if (NULL == ff_p) return;

  int nb_cards = 0;
  MCDS_get_fileformat_nb_cards(ff_p, &nb_cards);
  if (nb_cards == 0) return;

  string comment("");
  MvExpressionList_t expressionList;
  for (int i = 0; i < nb_cards; i++) {
    ff_card_t *a_card_p = NULL;
    MCDS_get_fileformat_card(ff_p, i, &a_card_p);

    postTreatFileFormat(a_card_p,comment,expressionList);
  }

  initDefinitionsAttributesSolverNames();
  initOtherAttributesSolverNames();
}

void MvDescriptor_t::postTreatFileFormat(ff_card_t *card_p, string& comment,
    MvExpressionList_t& expressionList)
{
  if (NULL == card_p) return;

  switch (card_p->type) {
  case CARD_COMMENT:
 {
      int nb_cells = 0;
      MCDS_get_ff_card_attributes(card_p, CARD_NB_CELLS, &nb_cells, END_ARGS);
      if (nb_cells != 1 ) return;

      ff_cell_t* cell_p = NULL;
      MCDS_get_ff_card_tab(card_p, CARD_CELL, 0, (void*)(&cell_p));
      char* comment_str = NULL;
      MCDS_get_ff_cell_attributes(cell_p, CELL_STRING,&comment_str,END_ARGS);
      comment = comment_str;
      //comment.replace(0,1," "); /*Replacing first char($/#) with space char*/
      std::replace(comment.begin(), comment.end(), comment_str[0], ' ');
      // replace "%%" with "%"
      size_t pos = comment.find("%%");
      while(comment.npos != pos)
      {
          comment.replace(pos, 2, "%");
          pos = comment.find("%%", pos+1);
      }
 }
 break;
  case CARD_SINGLE:
  case CARD_CELL_LIST:
  case CARD_LIST: /*NB: Need to support static array*/
    {
      int nb_cells = 0;
      MCDS_get_ff_card_attributes(card_p,CARD_NB_CELLS,&nb_cells,END_ARGS);
      if (nb_cells == 0) return;

      int position = 0; /*Initial position*/

      for ( int i = 0; i < nb_cells; i++) {
          ff_cell_t *cell_p=NULL;
          MCDS_get_ff_card_tab(card_p,CARD_CELL,i,(void *)(&cell_p));
          int length = postTreatFileFormat(cell_p, comment.c_str(), position, expressionList);
          position += length;
      }
    }
    break;
  case CARD_CARD_LIST:
      {
          int nb_sub_cards  = 0;
          MCDS_get_ff_card_attributes(card_p,CARD_NB_CARDS,&nb_sub_cards,END_ARGS);

          for(int j=0;j<nb_sub_cards;++j) {
              ff_card_t *sub_card_p=NULL;
              MCDS_get_ff_card_tab(card_p,CARD_CARD,j,(void *)(&sub_card_p));
              postTreatFileFormat(sub_card_p,comment, expressionList);
          }
      }
      break;
  case CARD_IF:
    {
        int nb_ccls = 0;
        MCDS_get_ff_card_attributes(card_p,CARD_NB_COND_CARD_LISTS,&nb_ccls,END_ARGS);

        for (int i = 0;i<nb_ccls;i++) {
            /*In case there is a comment before if which is valid to all else cards.*/
            string current_string = comment;

            ff_condcardlist_t *ccl_p  = NULL;
            MCDS_get_ff_card_tab(card_p,CARD_COND_CARD_LIST,i,&ccl_p);

            // add condition to expressionList for sub_cards
            expression_t *a_expr_p = NULL;
            MCDS_get_ff_condcardlist_expression(ccl_p, &a_expr_p);
            if(a_expr_p != NULL) expressionList.push_back(MvExpression_t(a_expr_p,false));

            int nb_cards=0;
            MCDS_get_ff_condcardlist_nb_cards(ccl_p,&nb_cards);
            for (int j=0;j<nb_cards;j++) {
                ff_card_t *sub_card_p=NULL;
                MCDS_get_ff_condcardlist_card(ccl_p,j,&sub_card_p);
                postTreatFileFormat(sub_card_p,current_string, expressionList);
                if (sub_card_p != NULL && sub_card_p->type == CARD_COMMENT && nb_cards == 1)
                {
                    comment = current_string;
                }
            }

            // remove condition from expressionList
            if(a_expr_p != NULL) expressionList.pop_back();
        }
    }
    break;
  case CARD_SUBOBJECTS:
  {
      mySubobjectsCardInfoList.resize(mySubobjectsCardInfoList.size() + 1);
      MvSubobjectsCardInfo_t& cardInfo = mySubobjectsCardInfoList.back();
      MCDS_get_ff_card_attributes(card_p,
          CARD_OBJECTS_IKW, &cardInfo.ikeyword,
          CARD_KFULLTYPE,   &cardInfo.full_type, 
          CARD_SUBOBJ_PARENT_LNK_ATT, &cardInfo.plnkatt,
          CARD_SUBOBJ_CHILD_LNK_ATT, &cardInfo.clnkatt,
          END_ARGS);
      cardInfo.conditions = expressionList;
  }
  break;
  default:
      break;
  }
}

int MvDescriptor_t::postTreatFileFormat(ff_cell_t *cell_p, const char *comment, int position,
    MvExpressionList_t& expressionList)
{
  if (NULL == cell_p) return 0;

  char solver_name[200] = "";
  int length = 0, length_solver_name = 0;

  ff_cell_type_e cell_type = CELL_UNKNOWN;
  MCDS_get_ff_cell_attributes(cell_p,CELL_TYPE,&cell_type,END_ARGS);

  switch(cell_type) {
  case CELL_VALUE:
  case CELL_ID:
  case CELL_DIR_RADIO:
  case CELL_BLANK:
  case CELL_APPEND_OPTIONS:
    {
      length_solver_name = length = loc_get_cell_length(cell_p);
      if (strlen(comment) > position)
      {
        if(strlen(comment) < position + length) length_solver_name = (int) strlen(comment) - position;
        strncpy(solver_name, comment+position, length_solver_name);
      }
      loc_string_fit_all(solver_name);
      int ikeyword = END_ARGS;
      MCDS_get_ff_cell_attributes(cell_p,CELL_IKEYWORD,&ikeyword,END_ARGS);

      if (ikeyword != END_ARGS)
      {
          initAttributeLength(ikeyword, length);
          initAttributeSolverNames(solver_name, ikeyword, true, expressionList);
      }
    }
    break;
  case CELL_SCALAR_OR_OBJECT:
  case CELL_SCALAR_OR_STRING:
  case CELL_FLAGGED_OBJECT:
    {
      length_solver_name = length = loc_get_cell_length(cell_p);
      if (strlen(comment) > position)
      {
          if(strlen(comment) < position + length) length_solver_name = (int) strlen(comment) - position;
          strncpy(solver_name, comment+position, length_solver_name);
      }
      loc_string_fit_all(solver_name);
      int scalar_ikw = END_ARGS;
      int object_ikw = END_ARGS;
      int flag_ikw = END_ARGS;
      int string_ikw = END_ARGS;

      MCDS_get_ff_cell_attributes(cell_p,CELL_FLAG_IKW,&flag_ikw,CELL_SCALAR_IKW,&scalar_ikw,END_ARGS);
      if (cell_type == CELL_SCALAR_OR_OBJECT || cell_type == CELL_FLAGGED_OBJECT)
        MCDS_get_ff_cell_attributes(cell_p,CELL_OBJECT_IKW,&object_ikw,END_ARGS);
      else
        MCDS_get_ff_cell_attributes(cell_p,CELL_STRING_IKW,&string_ikw,END_ARGS);

      // if (flag_ikw != END_ARGS) initAttributeSolverNames(solver_name, flag_ikw); better not!
      if (scalar_ikw != END_ARGS)
      {
          initAttributeLength(scalar_ikw, length);
          // temporarily pushing the condition to the list.
          // NB: the memory of the MCDS expression probably gets leaked, but for now we tolerate this
          MvAttributeExpression_t expr(this, flag_ikw, "==", "FALSE", false);
          expressionList.push_back(expr);
          initAttributeSolverNames(solver_name, scalar_ikw, true, expressionList);
          expressionList.pop_back();
      }
      if (object_ikw != END_ARGS)
      {
          // temporarily pushing the condition to the list.
          // NB: the memory of the MCDS expression probably gets leaked, but for now we tolerate this
          if(CELL_SCALAR_OR_OBJECT == cell_type)
          {
              MvAttributeExpression_t expr(this, flag_ikw, "!=", "FALSE", false);
              expressionList.push_back(expr);
          }
          initAttributeSolverNames(solver_name, object_ikw, true, expressionList);
          if(CELL_SCALAR_OR_OBJECT == cell_type)
          {
              expressionList.pop_back();
          }
      }
      if (string_ikw != END_ARGS)
      {
          initAttributeLength(string_ikw, length);
          // temporarily pushing the condition to the list.
          // NB: the memory of the MCDS expression probably gets leaked, but for now we tolerate this
          MvAttributeExpression_t expr(this, flag_ikw, "!=", "FALSE", false);
          expressionList.push_back(expr);
          initAttributeSolverNames(solver_name, string_ikw, true, expressionList);
          expressionList.pop_back();
      }
    }
      break;
  case CELL_COMMENT:
      {
          MCDS_get_ff_cell_attributes(cell_p,CELL_SIZE,&length,END_ARGS);
      }
      break;
 case CELL_DIGITS:
     {
       length_solver_name = length = loc_get_cell_length(cell_p);
       if(strlen(comment) > position)
       {
           if(strlen(comment) < position + length) length_solver_name = (int) strlen(comment) - position;
           strncpy(solver_name, comment+position, length_solver_name);
       }
       loc_string_fit_all(solver_name);
       int nb_ikws = 0;
       MCDS_get_ff_cell_attributes(cell_p,CELL_NB_IKEYWORDS,&nb_ikws,END_ARGS);
       for(int i=0;i<nb_ikws;++i) {
         int ikeyword=END_ARGS;
         MCDS_get_ff_cell_tab(cell_p,CELL_IKEYWORD,i,(void *)(&ikeyword));
         if (ikeyword != END_ARGS) initAttributeSolverNames(solver_name, ikeyword, true, expressionList);
       }
     }
     break;
 case CELL_DIR_FLAGS:
     {
       length_solver_name = length = loc_get_cell_length(cell_p);
       if(strlen(comment) > position)
       {
           if(strlen(comment) < position + length) length_solver_name = (int) strlen(comment) - position;
           strncpy(solver_name, comment+position, length_solver_name);
       }
       loc_string_fit_all(solver_name);
       int xdir_ikw=END_ARGS,ydir_ikw=END_ARGS,zdir_ikw=END_ARGS;
       MCDS_get_ff_cell_attributes(cell_p,CELL_DIRX_IKW,&xdir_ikw,CELL_DIRY_IKW,&ydir_ikw,
            CELL_DIRZ_IKW,&zdir_ikw,END_ARGS);

       /*For all 3 attributes, if solver name is not found then set skeyword*/
       if (xdir_ikw != END_ARGS) initAttributeSolverNames(solver_name, xdir_ikw, true, expressionList);
       if (ydir_ikw != END_ARGS) initAttributeSolverNames(solver_name, ydir_ikw, true, expressionList);
       if (zdir_ikw != END_ARGS) initAttributeSolverNames(solver_name, zdir_ikw, true, expressionList);
     }
     break;
  case CELL_COND:
      {
         int nb_ccls = 0;
         MCDS_get_ff_cell_attributes(cell_p,CELL_NB_COND_CELL,&nb_ccls,END_ARGS);
         for(int i=0;i<nb_ccls;++i) {
             ff_condcell_t *ccl_p = NULL;
             MCDS_get_ff_cell_tab(cell_p,CELL_COND_CELL,i,&ccl_p);
             ff_cell_t *sub_cell_p = NULL;
             MCDS_get_ff_condcell_cell(ccl_p, &sub_cell_p);
             length = postTreatFileFormat(sub_cell_p,comment,position, expressionList);
         }
      }
      break;
   case CELL_PAIR:
    {
      length_solver_name = length = loc_get_cell_length(cell_p);
      if(strlen(comment) > position)
      {
          if(strlen(comment) < position + length) length_solver_name = (int) strlen(comment) - position;
          strncpy(solver_name, comment+position, length_solver_name);
      }
      loc_string_fit_all(solver_name);
      int ikeyword = END_ARGS;
      MCDS_get_ff_cell_attributes(cell_p,CELL_IKEYWORD,&ikeyword,END_ARGS);
      if (ikeyword != END_ARGS) initAttributeSolverNames(solver_name, ikeyword, true, expressionList);
    }
      break;
  default:
      break;
  }

  return length;
}

void MvDescriptor_t::initDefinitionsAttributesSolverNames()
{
  MvIKeywordSet_t def_ikeywords;
  def_ikeywords += getDefinition(DOM_COMMON, "SET_PART");
  def_ikeywords += getDefinition(DOM_COMMON, "SET_SHELL");
  def_ikeywords += getDefinition(DOM_COMMON, "SET_PART_SCALAR_OR_OBJECT");
  def_ikeywords += getDefinition(DOM_COMMON, "SET_PART_FLAGGED_OBJECT");
  if (def_ikeywords.empty()) return;

  MvIKeywordSet_t::const_iterator it;
  for (it = def_ikeywords.begin(); it != def_ikeywords.end(); ++it) {
    int ikeyword_set = *it;
    string skeyword = getSKeyword(ikeyword_set);
    skeyword += "_ID";
    int ikeyword = getIKeyword(skeyword);
    if (ikeyword != END_ARGS) {
      string solver_name = getSolverName(ikeyword);
      initAttributeSolverNames(solver_name, ikeyword_set);
    }
  }
}

void MvDescriptor_t::initOtherAttributesSolverNames()
{
  MvIKeywordList_t ikeyword_set;
  getIKeywords(MV_get_all_domains(), &ikeyword_set);

  MvIKeywordList_t::const_iterator it;
  MvIKeywordList_t::const_iterator it_beg = ikeyword_set.begin();
  MvIKeywordList_t::const_iterator it_end = ikeyword_set.end();

  for (it = it_beg; it != it_end; ++it) {
    int ikeyword = *it;
    string solver_name = getSolverName(ikeyword);
    if (solver_name.length() == 0) {
      initAttributeSolverNames(solver_name, ikeyword);
    }
  }
}

void MvDescriptor_t::initAttributeSolverNames(const string &solver_name, int ikeyword, bool set_skeyword,
    const MvExpressionList_t& expressionList)
{
  //If we don't find solver name and "skeyword flag" on, then we set the skeyword as solver name.
  size_t len = solver_name.length();
  string skeyword = getSKeyword(ikeyword);
  if(len > 0)
  {
      MCDS_set_descriptor_attributes(getDescriptorPtr(), ikeyword, DESCR_SOLVER_NAME, solver_name.c_str(), END_ARGS);
  }
  else
  {
      MCDS_set_descriptor_attributes(getDescriptorPtr(), ikeyword, DESCR_SOLVER_NAME, skeyword.c_str(), END_ARGS);
  }

  LocKeywordKeywordMap_t &a_SolverNameSKeywordMap = (*((LocKeywordKeywordMap_t *)mySolverNameSKeywordMapPtr));
  //If we don't find solver name and "skeyword flag" on, then we set the skeyword as solver name.
  if (solver_name.length()== 0 && set_skeyword) {
    a_SolverNameSKeywordMap[skeyword] = skeyword;
  }
  else if (solver_name.length() > 0) {
    a_SolverNameSKeywordMap[solver_name] = skeyword;
    MvCondKeywordList_t& condKeywordList = mySolverNameSKeywordsMap[solver_name];
    condKeywordList.push_back(make_pair(skeyword, expressionList));
  }
}

void MvDescriptor_t::initAttributeLength(int ikeyword, int length)
{
    MCDS_set_descriptor_attributes(getDescriptorPtr(),ikeyword,DESCR_LENGTH,length,END_ARGS);
}

/* --------- Drawable --------- */

void MvDescriptor_t::addDrawable(MvDomain_e domain,const MvDrawable_t *drawable_p) {
  LocDrawableMap_t     *a_dm_p  = (LocDrawableMap_t*)myDrawableListMapPtr; 
  LocDrawableNameMap_t *a_dnm_p = (LocDrawableNameMap_t*)myDrawableListNameMapPtr;
  //
  (*a_dm_p)[domain].insert(drawable_p);
  (*a_dnm_p)[drawable_p->getName()]=drawable_p;  
}

const MvDrawable_t *MvDescriptor_t::getDrawablePtr( const string &name) const {
  LocDrawableNameMap_t *a_dnm_p=(LocDrawableNameMap_t*)myDrawableListNameMapPtr;
  LocDrawableNameMap_t::const_iterator a_it=a_dnm_p->find(name);
  if(a_it==a_dnm_p->end()) return NULL;
  return (*a_it).second;
}

MvDrawablePtrSet_t *MvDescriptor_t::getDrawables(int domain,MvDrawablePtrSet_t* drawable_set_p) const {
  if(drawable_set_p==NULL) drawable_set_p=new MvDrawablePtrSet_t();
  //
  LocDrawableMap_t *a_dm_p = (LocDrawableMap_t*)myDrawableListMapPtr;
  MvDomainSet_t    a_domain_set;
  MV_get_domains(domain,&a_domain_set);
  //  
  MvDomainSet_t::const_iterator a_dom_it_begin = a_domain_set.begin();
  MvDomainSet_t::const_iterator a_dom_it_end   = a_domain_set.end();
  MvDomainSet_t::const_iterator a_dom_it;
  for(a_dom_it=a_dom_it_begin;a_dom_it!=a_dom_it_end;++a_dom_it) {
    MvDomain_e                      a_domain = (*a_dom_it);
    LocDrawableMap_t::const_iterator a_it     = a_dm_p->find(a_domain);
    if(a_it!=a_dm_p->end()) (*drawable_set_p)+=(*a_it).second;
  }
  //
  return drawable_set_p; 
}

MvDrawablePtrSet_t *MvDescriptor_t::getDrawables(int domain,MvDrawableAccess_e access,
						 MvDrawablePtrSet_t* drawable_set_p) const 
{
  if(drawable_set_p==NULL) drawable_set_p=new MvDrawablePtrSet_t();
  //
  LocDrawableMap_t *a_dm_p = (LocDrawableMap_t*)myDrawableListMapPtr;
  MvDomainSet_t    a_domain_set;
  MV_get_domains(domain,&a_domain_set);
  //  
  MvDomainSet_t::const_iterator a_dom_it_begin = a_domain_set.begin();
  MvDomainSet_t::const_iterator a_dom_it_end   = a_domain_set.end();
  MvDomainSet_t::const_iterator a_dom_it;
  for(a_dom_it=a_dom_it_begin;a_dom_it!=a_dom_it_end;++a_dom_it) {
    MvDomain_e                      a_domain = (*a_dom_it);
    LocDrawableMap_t::const_iterator a_it     = a_dm_p->find(a_domain);
    if(a_it!=a_dm_p->end()) {
      const MvDrawablePtrSet_t &a_drawable_set=(*a_it).second;
      //
      MvDrawablePtrSet_t::const_iterator a_dps_it_begin = a_drawable_set.begin();
      MvDrawablePtrSet_t::const_iterator a_dps_it_end   = a_drawable_set.end();
      MvDrawablePtrSet_t::const_iterator a_dps_it;
      for(a_dps_it=a_dps_it_begin;a_dps_it!=a_dps_it_end;++a_dps_it) {
	const MvDrawable_t *a_drawable_p=(*a_dps_it);
	if(a_drawable_p->getAccess()==access) drawable_set_p->insert(a_drawable_p);
      }
    }
  }
  //
  return drawable_set_p; 
}

MvDrawableNameSet_t *MvDescriptor_t::getDrawableNames(int domain,MvDrawableNameSet_t* dname_set_p) const {
  if(dname_set_p==NULL) dname_set_p=new MvDrawableNameSet_t();
  //
  LocDrawableMap_t *a_dm_p = (LocDrawableMap_t*)myDrawableListMapPtr;
  MvDomainSet_t    a_domain_set;
  MV_get_domains(domain,&a_domain_set);
  //  
  MvDomainSet_t::const_iterator a_dom_it_begin = a_domain_set.begin();
  MvDomainSet_t::const_iterator a_dom_it_end   = a_domain_set.end();
  MvDomainSet_t::const_iterator a_dom_it;
  for(a_dom_it=a_dom_it_begin;a_dom_it!=a_dom_it_end;++a_dom_it) {
    MvDomain_e                      a_domain = (*a_dom_it);
    LocDrawableMap_t::const_iterator a_it     = a_dm_p->find(a_domain);
    if(a_it!=a_dm_p->end()) {
      const MvDrawablePtrSet_t &a_drawable_set=(*a_it).second;
      //
      MvDrawablePtrSet_t::const_iterator a_dps_it_begin = a_drawable_set.begin();
      MvDrawablePtrSet_t::const_iterator a_dps_it_end   = a_drawable_set.end();
      MvDrawablePtrSet_t::const_iterator a_dps_it;
      for(a_dps_it=a_dps_it_begin;a_dps_it!=a_dps_it_end;++a_dps_it) {
	const MvDrawable_t *a_drawable_p=(*a_dps_it);
	dname_set_p->insert(a_drawable_p->getName());
      }
    }
  }
  //
  return dname_set_p; 
}

MvDrawableNameSet_t *MvDescriptor_t::getDrawableNames(int domain,MvDrawableAccess_e access,
						      MvDrawableNameSet_t* dname_set_p) const 
{
  if(dname_set_p==NULL) dname_set_p=new MvDrawableNameSet_t();
  //
  LocDrawableMap_t *a_dm_p = (LocDrawableMap_t*)myDrawableListMapPtr;
  MvDomainSet_t    a_domain_set;
  MV_get_domains(domain,&a_domain_set);
  //  
  MvDomainSet_t::const_iterator a_dom_it_begin = a_domain_set.begin();
  MvDomainSet_t::const_iterator a_dom_it_end   = a_domain_set.end();
  MvDomainSet_t::const_iterator a_dom_it;
  for(a_dom_it=a_dom_it_begin;a_dom_it!=a_dom_it_end;++a_dom_it) {
    MvDomain_e                      a_domain = (*a_dom_it);
    LocDrawableMap_t::const_iterator a_it     = a_dm_p->find(a_domain);
    if(a_it!=a_dm_p->end()) {
      const MvDrawablePtrSet_t &a_drawable_set=(*a_it).second;
      //
      MvDrawablePtrSet_t::const_iterator a_dps_it_begin = a_drawable_set.begin();
      MvDrawablePtrSet_t::const_iterator a_dps_it_end   = a_drawable_set.end();
      MvDrawablePtrSet_t::const_iterator a_dps_it;
      for(a_dps_it=a_dps_it_begin;a_dps_it!=a_dps_it_end;++a_dps_it) {
	const MvDrawable_t *a_drawable_p=(*a_dps_it);
	if(a_drawable_p->getAccess()==access) dname_set_p->insert(a_drawable_p->getName());
      }
    }
  }
  //
  return dname_set_p; 
}


/* --------- M-Xplore's parameters --------- */


void MvDescriptor_t::addIParamDescr(MvDomain_e domain,const MvIParamDescr_t *iparam_p) {
  LocIParamDescrMap_t     *a_ipm_p  = (LocIParamDescrMap_t*)myIParamDescrListMapPtr; 
  LocIParamDescrNameMap_t *a_ipnm_p = (LocIParamDescrNameMap_t*)myIParamDescrListNameMapPtr;
  //
  (*a_ipm_p)[domain].push_back(iparam_p); 
  (*a_ipnm_p)[iparam_p->getName()]=iparam_p;  
}




const MvIParamDescr_t *MvDescriptor_t::getIParamDescrPtr(const string &name) const {
  LocIParamDescrNameMap_t *a_ipnm_p = (LocIParamDescrNameMap_t*)myIParamDescrListNameMapPtr;
  LocIParamDescrNameMap_t::const_iterator a_it=a_ipnm_p->find(name);
  if(a_it==a_ipnm_p->end()) return NULL;
  return (*a_it).second;
}





MvIParamDescrPtrList_t *MvDescriptor_t::getIParamDescrs(int domains,
							MvIParamDescrPtrList_t *iparams_p) const
{
  if(iparams_p==NULL) iparams_p=new MvIParamDescrPtrList_t();
  //
  LocIParamDescrMap_t *a_ipm_p=(LocIParamDescrMap_t *)myIParamDescrListMapPtr;
  MvDomainSet_t  a_domain_set;
  MV_get_domains(domains,&a_domain_set);
  //  
  MvDomainSet_t::const_iterator a_dom_it_begin = a_domain_set.begin();
  MvDomainSet_t::const_iterator a_dom_it_end   = a_domain_set.end();
  MvDomainSet_t::const_iterator a_dom_it;
  for(a_dom_it=a_dom_it_begin;a_dom_it!=a_dom_it_end;++a_dom_it) {
    MvDomain_e                    a_domain = (*a_dom_it);
    LocIParamDescrMap_t::const_iterator a_it     = a_ipm_p->find(a_domain);
    if(a_it!=a_ipm_p->end()) {
      const MvIParamDescrPtrList_t &a_iparams=(*a_it).second;
      //
      iparams_p->reserve(iparams_p->size()+a_iparams.size());
      MvIParamDescrPtrList_t::const_iterator a_ip_it_begin = a_iparams.begin();
      MvIParamDescrPtrList_t::const_iterator a_ip_it_end   = a_iparams.end();
      MvIParamDescrPtrList_t::const_iterator a_ip_it;
      for(a_ip_it=a_ip_it_begin;a_ip_it!=a_ip_it_end;++a_ip_it)	(*iparams_p).push_back(*a_ip_it);
    }
  }
  //
  return iparams_p; 
}






MvIParamDescrPtrList_t *MvDescriptor_t::getIParamDescrs(int domains,MvIParamAccess_e access,
							MvIParamDescrPtrList_t* iparams_p) const
{
  if(iparams_p==NULL) iparams_p=new MvIParamDescrPtrList_t();
  //
  LocIParamDescrMap_t *a_ipm_p=(LocIParamDescrMap_t *)myIParamDescrListMapPtr;
  MvDomainSet_t  a_domain_set;
  MV_get_domains(domains,&a_domain_set);
  //  
  MvDomainSet_t::const_iterator a_dom_it_begin = a_domain_set.begin();
  MvDomainSet_t::const_iterator a_dom_it_end   = a_domain_set.end();
  MvDomainSet_t::const_iterator a_dom_it;
  for(a_dom_it=a_dom_it_begin;a_dom_it!=a_dom_it_end;++a_dom_it) {
    MvDomain_e                         a_domain = (*a_dom_it);
    LocIParamDescrMap_t::const_iterator a_it     = a_ipm_p->find(a_domain);
    if(a_it!=a_ipm_p->end()) {
      const MvIParamDescrPtrList_t &a_iparams=(*a_it).second;
      //
      MvIParamDescrPtrList_t::const_iterator a_ips_it_begin = a_iparams.begin();
      MvIParamDescrPtrList_t::const_iterator a_ips_it_end   = a_iparams.end();
      MvIParamDescrPtrList_t::const_iterator a_ips_it;
      for(a_ips_it=a_ips_it_begin;a_ips_it!=a_ips_it_end;++a_ips_it) {
	const MvIParamDescr_t *a_iparam_p=(*a_ips_it);
	if(a_iparam_p->getAccess()==access) iparams_p->push_back(a_iparam_p);
      }
    }
  }
  //
  return iparams_p; 
}




void MvDescriptor_t::addOParamDescr(MvDomain_e domain,const MvOParamDescr_t *oparam_p) {
  LocOParamDescrMap_t     *a_opm_p  = (LocOParamDescrMap_t*)myOParamDescrListMapPtr; 
  LocOParamDescrNameMap_t *a_opnm_p = (LocOParamDescrNameMap_t*)myOParamDescrListNameMapPtr;
  //
  (*a_opm_p)[domain].push_back(oparam_p); 
  (*a_opnm_p)[oparam_p->getName()]=oparam_p;  
}



const MvOParamDescr_t *MvDescriptor_t::getOParamDescrPtr(const string &name) const {
  LocOParamDescrNameMap_t *a_opnm_p = (LocOParamDescrNameMap_t*)myOParamDescrListNameMapPtr;
  LocOParamDescrNameMap_t::const_iterator a_it=a_opnm_p->find(name);
  if(a_it==a_opnm_p->end()) return NULL;
  return (*a_it).second;
}



MvOParamDescrPtrList_t *MvDescriptor_t::getOParamDescrs(int                     domains,
							MvOParamDescrPtrList_t *oparams_p) const
{
  if(oparams_p==NULL) oparams_p=new MvOParamDescrPtrList_t();
  //
  LocOParamDescrMap_t *a_opm_p=(LocOParamDescrMap_t *)myOParamDescrListMapPtr;
  MvDomainSet_t  a_domain_set;
  MV_get_domains(domains,&a_domain_set);
  //  
  MvDomainSet_t::const_iterator a_dom_it_begin = a_domain_set.begin();
  MvDomainSet_t::const_iterator a_dom_it_end   = a_domain_set.end();
  MvDomainSet_t::const_iterator a_dom_it;
  for(a_dom_it=a_dom_it_begin;a_dom_it!=a_dom_it_end;++a_dom_it) {
    MvDomain_e                    a_domain = (*a_dom_it);
    LocOParamDescrMap_t::const_iterator a_it     = a_opm_p->find(a_domain);
    if(a_it!=a_opm_p->end()) {
      const MvOParamDescrPtrList_t &a_oparams=(*a_it).second;
      //
      oparams_p->reserve(oparams_p->size()+a_oparams.size());
      MvOParamDescrPtrList_t::const_iterator a_op_it_begin = a_oparams.begin();
      MvOParamDescrPtrList_t::const_iterator a_op_it_end   = a_oparams.end();
      MvOParamDescrPtrList_t::const_iterator a_op_it;
      for(a_op_it=a_op_it_begin;a_op_it!=a_op_it_end;++a_op_it)	(*oparams_p).push_back(*a_op_it);
    }
  }
  //
  return oparams_p; 
}



MvOParamDescrPtrList_t *MvDescriptor_t::getOParamDescrs(int                     domains,
							MvOParamAccess_e        access,
							MvOParamDescrPtrList_t *oparams_p) const
{
  if(oparams_p==NULL) oparams_p=new MvOParamDescrPtrList_t();
  //
  LocOParamDescrMap_t *a_opm_p=(LocOParamDescrMap_t *)myOParamDescrListMapPtr;
  MvDomainSet_t  a_domain_set;
  MV_get_domains(domains,&a_domain_set);
  //  
  MvDomainSet_t::const_iterator a_dom_it_begin = a_domain_set.begin();
  MvDomainSet_t::const_iterator a_dom_it_end   = a_domain_set.end();
  MvDomainSet_t::const_iterator a_dom_it;
  for(a_dom_it=a_dom_it_begin;a_dom_it!=a_dom_it_end;++a_dom_it) {
    MvDomain_e                         a_domain = (*a_dom_it);
    LocOParamDescrMap_t::const_iterator a_it     = a_opm_p->find(a_domain);
    if(a_it!=a_opm_p->end()) {
      const MvOParamDescrPtrList_t &a_oparams=(*a_it).second;
      //
      MvOParamDescrPtrList_t::const_iterator a_ops_it_begin = a_oparams.begin();
      MvOParamDescrPtrList_t::const_iterator a_ops_it_end   = a_oparams.end();
      MvOParamDescrPtrList_t::const_iterator a_ops_it;
      for(a_ops_it=a_ops_it_begin;a_ops_it!=a_ops_it_end;++a_ops_it) {
	const MvOParamDescr_t *a_oparam_p=(*a_ops_it);
	if(a_oparam_p->getAccess()==access) oparams_p->push_back(a_oparam_p);
      }
    }
  }
  //
  return oparams_p; 
} 



/* --------- Conditional I/O parameter descriptors --------- */


void MvDescriptor_t::addCondIParamDescriptors(MvDomain_e                       domain,
					      const MvCondIParamDescriptors_t *cipd_p)
{
  LocCondIParamDescrListMap_t *a_cipdlm_p=(LocCondIParamDescrListMap_t *)myCondIParamDescrListMapPtr;
  (*a_cipdlm_p)[domain].push_back(cipd_p);
}



void MvDescriptor_t::addCondOParamDescriptors(MvDomain_e                       domain,
					      const MvCondOParamDescriptors_t *copd_p)
{
  LocCondOParamDescrListMap_t *a_copdlm_p=(LocCondOParamDescrListMap_t *)myCondOParamDescrListMapPtr;
  (*a_copdlm_p)[domain].push_back(copd_p);
}



MvCondIParamDescrList_t *MvDescriptor_t::getCondIParamDescriptors(int                      domains,
								  MvCondIParamDescrList_t *cipdl_p) const
{
  if(cipdl_p==NULL) cipdl_p=new MvCondIParamDescrList_t();
  //
  const LocCondIParamDescrListMap_t *a_cipdlm_p=(const LocCondIParamDescrListMap_t *)myCondIParamDescrListMapPtr;
  //
  MvDomain_e a_dom_begin = MV_get_domain_begin();
  MvDomain_e a_dom_end   = MV_get_domain_end();
  MvDomain_e a_dom;
  for(a_dom=a_dom_begin;a_dom!=a_dom_end;a_dom=MV_get_domain_next(a_dom)) if(domains & a_dom) {
    LocCondIParamDescrListMap_t::const_iterator a_cipdlm_it=a_cipdlm_p->find(a_dom);
    //
    if(a_cipdlm_it!=a_cipdlm_p->end()) {
      const MvCondIParamDescrList_t &a_dom_dl=(*a_cipdlm_it).second;
      MvCondIParamDescrList_t::const_iterator a_it_begin = a_dom_dl.begin();
      MvCondIParamDescrList_t::const_iterator a_it_end   = a_dom_dl.end();
      MvCondIParamDescrList_t::const_iterator a_it;
      for(a_it=a_it_begin;a_it!=a_it_end;++a_it) cipdl_p->push_back(*a_it);
    }
  }
  //
  return cipdl_p;
}



MvCondOParamDescrList_t *MvDescriptor_t::getCondOParamDescriptors(int                      domains,
								  MvCondOParamDescrList_t *copdl_p) const
{
  if(copdl_p==NULL) copdl_p=new MvCondOParamDescrList_t();
  //
  const LocCondOParamDescrListMap_t *a_copdlm_p=(const LocCondOParamDescrListMap_t *)myCondOParamDescrListMapPtr;
  //
  MvDomain_e a_dom_begin = MV_get_domain_begin();
  MvDomain_e a_dom_end   = MV_get_domain_end();
  MvDomain_e a_dom;
  for(a_dom=a_dom_begin;a_dom!=a_dom_end;a_dom=MV_get_domain_next(a_dom)) if(domains & a_dom) {
    LocCondOParamDescrListMap_t::const_iterator a_copdlm_it=a_copdlm_p->find(a_dom);
    //
    if(a_copdlm_it!=a_copdlm_p->end()) {
      const MvCondOParamDescrList_t &a_dom_dl=(*a_copdlm_it).second;
      MvCondOParamDescrList_t::const_iterator a_it_begin = a_dom_dl.begin();
      MvCondOParamDescrList_t::const_iterator a_it_end   = a_dom_dl.end();
      MvCondOParamDescrList_t::const_iterator a_it;
      for(a_it=a_it_begin;a_it!=a_it_end;++a_it) copdl_p->push_back(*a_it);
    }
  }
  //
  return copdl_p;
}



MvDataFeatureSet_t *MvDescriptor_t::getIfFeatures(int domains, const MvDataFeature_t *test_p,MvDataFeatureSet_t *dfs_p) const
{
    if(dfs_p==NULL) dfs_p = new MvDataFeatureSet_t();

    MvDataFeatureList_t *iffeatures_p = getDataFeatures(domains,DFT_IF);
    
    MvDataFeatureList_t::iterator it = iffeatures_p->begin();
    MvDataFeatureList_t::iterator it_end = iffeatures_p->end();

    while(it != it_end)
    {
        MvDataIfFeature_t *iffeature_p = (MvDataIfFeature_t *)(*it);
        const MvDataFeatureSet_t testfeatures = iffeature_p->getTestFeatures();
        MvDataFeatureSet_t::const_iterator it_test = testfeatures.find(test_p);

        if(it_test != testfeatures.end())
        {
            dfs_p->insert(*it);
        }
        ++it;
    }

    return dfs_p;
}


/* --------- Definitions --------- */


void MvDescriptor_t::addDefinition(MvDomain_e domain,const string &name,const MvIKeywordSet_t &ikeywords) {
  LocDefinition_t &a_definitions=(*((LocDefinitionMap_t *)myDefinitionMapPtr))[domain];
  a_definitions[name]=ikeywords;
}



const MvIKeywordSet_t &MvDescriptor_t::getDefinition(int domain,const string &name) const {
  static MvIKeywordSet_t a_empty_ikeywords; /*multimodel not required*/
  //
  for (int i = (int)(DOM_UNKNOWN + 1); i < (int)DOM_LAST; i <<= 1) if (domain & i) {
      MvDomain_e a_domain = (MvDomain_e)i;
      const LocDefinitionMap_t& a_definition_map = (*((LocDefinitionMap_t*)myDefinitionMapPtr));
      LocDefinitionMap_t::const_iterator  a_dm_it = a_definition_map.find(a_domain);
      //
      if (a_dm_it != a_definition_map.end()) {
          const LocDefinition_t& a_definitions = (*a_dm_it).second;
          LocDefinition_t::const_iterator  a_it = a_definitions.find(name);
          //
          if (a_it != a_definitions.end()) { return (*a_it).second; }
      }
  }
  //
  return a_empty_ikeywords;
}

bool MvDescriptor_t::hasIKeyInDefinition(int domain,const string &name, int ikey) const {
  static MvIKeywordSet_t a_empty_ikeywords;/*multimodel not required*/
  //
  for (int i = (int)(DOM_UNKNOWN + 1); i < (int)DOM_LAST; i <<= 1) if (domain & i) {
      MvDomain_e a_domain = (MvDomain_e)i;
      const LocDefinitionMap_t& a_definition_map = (*((LocDefinitionMap_t*)myDefinitionMapPtr));
      LocDefinitionMap_t::const_iterator  a_dm_it = a_definition_map.find(a_domain);
      //
      if (a_dm_it != a_definition_map.end()) {
          const LocDefinition_t& a_definitions = (*a_dm_it).second;
          LocDefinition_t::const_iterator  a_it = a_definitions.find(name);
          //
          if (a_it != a_definitions.end())
          {
              MvIKeywordSet_t::const_iterator  uint_it = ((*a_it).second).find(ikey);
              if (uint_it != ((*a_it).second).end()) return true;
          }
      }
  }
  //
  return false;
}

/* --------- Output --------- */

ostream &MvDescriptor_t::display(ostream &os) const {
  MvDomain_e a_domain;
  MvAtypeVtypeList_t tikw_list;
  getAllIKeywords((MvDomain_e)MV_get_all_domains(), tikw_list);
  MvAtypeVtypeList_t::iterator iter_b = tikw_list.begin();
  MvAtypeVtypeList_t::iterator iter_e = tikw_list.end();
  MvAtypeVtypeList_t::iterator iter;
  int max_size_skw = 0;
  for (iter = iter_b; iter != iter_e; ++iter)
  {
      vector<MvIKeywordList_t> a_ikw_vect = *iter;
      vector<MvIKeywordList_t>::iterator mylist_iter_b = a_ikw_vect.begin();
      vector<MvIKeywordList_t>::iterator mylist_iter_e = a_ikw_vect.end();
      vector<MvIKeywordList_t>::iterator mylist_iter;
      for (mylist_iter = mylist_iter_b; mylist_iter != mylist_iter_e; ++mylist_iter)
      {
          MvIKeywordList_t my_list = *mylist_iter;
          MvIKeywordList_t::iterator my_it_b = my_list.begin();
          MvIKeywordList_t::iterator my_it_e = my_list.end();
          MvIKeywordList_t::iterator my_it;

          for (my_it = my_it_b; my_it != my_it_e; ++my_it)
          {
              int ikw = *my_it;
              string skeyword = getSKeyword(ikw);
              int size = (int)skeyword.size();
              if (size > max_size_skw)
                  max_size_skw = size;
          }
      }
  }

  for (a_domain = MV_get_domain_begin(); a_domain != MV_get_domain_end(); a_domain = MV_get_domain_next(a_domain)) {
      int number = 0;
      MvAtypeVtypeList_t ikw_list;
      MvIKeywordList_t local_attrib_list;
      number = getAllIKeywords(a_domain, ikw_list);
      if (number == 0)
          continue;
      os << "ATTRIBUTE(" << MV_get_domain(a_domain) << ") {" << endl;

      iter_b = ikw_list.begin();
      iter_e = ikw_list.end();

      for (iter = iter_b; iter != iter_e; ++iter)
      {
          vector<MvIKeywordList_t> a_ikw_vect = *iter;
          vector<MvIKeywordList_t>::iterator mylist_iter_b = a_ikw_vect.begin();
          vector<MvIKeywordList_t>::iterator mylist_iter_e = a_ikw_vect.end();
          vector<MvIKeywordList_t>::iterator mylist_iter;
          for (mylist_iter = mylist_iter_b; mylist_iter != mylist_iter_e; ++mylist_iter)
          {
              MvIKeywordList_t my_list = *mylist_iter;
              displayAttribData(os, a_domain, my_list, &local_attrib_list, max_size_skw);
          }
      }
      // local attrib data
      os << "//local attributes" << endl;
      displayAttribData(os, a_domain, local_attrib_list, NULL, max_size_skw);
      os << "}" << endl;
  }

  for (a_domain = MV_get_domain_begin(); a_domain != MV_get_domain_end(); a_domain = MV_get_domain_next(a_domain)) {
      int number = 0;
      MvAtypeVtypeList_t ikw_list;
      number = getAllIKeywords(a_domain, ikw_list);

      if (number == 0)
          continue;
      os << "SKEYWORDS_IDENTIFIER(" << MV_get_domain(a_domain) << ") {" << endl;

      iter_b = ikw_list.begin();
      iter_e = ikw_list.end();

      for (iter = iter_b; iter != iter_e; ++iter)
      {
          vector<MvIKeywordList_t> a_ikw_vect = *iter;
          vector<MvIKeywordList_t>::iterator mylist_iter_b = a_ikw_vect.begin();
          vector<MvIKeywordList_t>::iterator mylist_iter_e = a_ikw_vect.end();
          vector<MvIKeywordList_t>::iterator mylist_iter;
          for (mylist_iter = mylist_iter_b; mylist_iter != mylist_iter_e; ++mylist_iter)
          {
              MvIKeywordList_t my_list = *mylist_iter;

              MvIKeywordList_t::iterator my_it_b = my_list.begin();
              MvIKeywordList_t::iterator my_it_e = my_list.end();
              MvIKeywordList_t::iterator my_it;

              for (my_it = my_it_b; my_it != my_it_e; ++my_it)
              {
                  int ikw = *my_it;
                  string skeyword = getSKeyword(ikw);
                  int value = getIdentifierValue(a_domain, skeyword);
                  if (value == 0)
                      continue;
                  os << "    " << skeyword;
                  displaySpaces(os, (int)skeyword.size(), max_size_skw);
                  os << " = " << value << ";" << endl;
              }
          }
      }
      os << "}" << endl;
  }

  for (a_domain = MV_get_domain_begin(); a_domain != MV_get_domain_end(); a_domain = MV_get_domain_next(a_domain)) {
      int number = 0;
      MvAtypeVtypeList_t ikw_list;
      number = getAllIKeywords(a_domain, ikw_list);
      if (number == 0)
          continue;
      os << "DEFAULTS(" << MV_get_domain(a_domain) << ") {" << endl;

      iter_b = ikw_list.begin();
      iter_e = ikw_list.end();

      for (iter = iter_b; iter != iter_e; ++iter)
      {
          vector<MvIKeywordList_t> a_ikw_vect = *iter;
          vector<MvIKeywordList_t>::iterator mylist_iter_b = a_ikw_vect.begin();
          vector<MvIKeywordList_t>::iterator mylist_iter_e = a_ikw_vect.end();
          vector<MvIKeywordList_t>::iterator mylist_iter;
          for (mylist_iter = mylist_iter_b; mylist_iter != mylist_iter_e; ++mylist_iter)
          {
              MvIKeywordList_t my_list = *mylist_iter;

              MvIKeywordList_t::iterator my_it_b = my_list.begin();
              MvIKeywordList_t::iterator my_it_e = my_list.end();
              MvIKeywordList_t::iterator my_it;

              for (my_it = my_it_b; my_it != my_it_e; ++my_it)
              {
                  int ikw = *my_it;
                  string skeyword = getSKeyword(ikw);
                  bool has_default = false;
                  value_type_e vtype = getValueType(ikw);
                  if (vtype == VTYPE_INT)
                  {
                      int value = getIntDefaultValue(skeyword, a_domain, &has_default);
                      if (has_default)
                      {
                          os << "    " << skeyword;
                          displaySpaces(os, (int)skeyword.size(), max_size_skw);
                          os << " = " << value << ";" << endl;
                      }
                  }
                  else if (vtype == VTYPE_FLOAT)
                  {
                      double value = getFloatDefaultValue(skeyword, a_domain, &has_default);
                      if (has_default)
                      {
                          os << "    " << skeyword;
                          displaySpaces(os, (int)skeyword.size(), max_size_skw);
                          os << " = " << value << ";" << endl;
                      }
                  }
                  else if (vtype == VTYPE_BOOL)
                  {
                      bool value = getBoolDefaultValue(skeyword, a_domain, &has_default);
                      if (has_default)
                      {
                          os << "    " << skeyword;
                          displaySpaces(os, (int)skeyword.size(), max_size_skw);
                          os << " = " << (int)value << ";" << endl;
                      }
                  }
                  else if (vtype == VTYPE_UINT)
                  {
                      unsigned int value = getUIntDefaultValue(skeyword, a_domain, &has_default);
                      if (has_default)
                      {
                          os << "    " << skeyword;
                          displaySpaces(os, (int)skeyword.size(), max_size_skw);
                          os << " = " << value << ";" << endl;
                      }
                  }
                  else if (vtype == VTYPE_STRING)
                  {
                      string value = getStringDefaultValue(skeyword, a_domain, &has_default);
                      if (has_default)
                      {
                          if (strstr(skeyword.c_str(), "_type"))
                          {
                              os << "    " << skeyword;
                              displaySpaces(os, (int)skeyword.size(), max_size_skw);
                              os << " = " << "\"" << value << "\";" << endl;
                          }
                          else
                          {
                              os << "    " << skeyword;
                              displaySpaces(os, (int)skeyword.size(), max_size_skw);
                              if (value == "")
                              {
                                  os << " = " << "\"\";" << endl;
                              }
                              else
                              {
                                  os << " = " << value << ";" << endl;
                              }
                          }
                      }
                  }
              }
          }
      }
      os << "}" << endl;
  }

  for (a_domain = MV_get_domain_begin(); a_domain != MV_get_domain_end(); a_domain = MV_get_domain_next(a_domain)) {
      MvTestList_t *test_list = getTests(a_domain);
      if (test_list->empty())
      {
          delete test_list;
          continue;
      }
      os << "CHECK(" << MV_get_domain(a_domain) << ") {" << endl;
      MvTestList_t::iterator test_iter_b = test_list->begin();
      MvTestList_t::iterator test_iter_e = test_list->end();
      MvTestList_t::iterator test_iter;
      for (test_iter = test_iter_b; test_iter != test_iter_e; ++test_iter)
      {
          const MvTest_t *test_p = *test_iter;
          MvTestType_e test_type = test_p->getType();
          os << "    ";
          if (test_type == TST_COMPARE)
          {
              const MvCompareTest_t *comp_test_p = dynamic_cast<const MvCompareTest_t *>(test_p);
              if (comp_test_p)
              {
                  int ikw = comp_test_p->getIKeyword();
                  string skeyword = getSKeyword(ikw);
                  os << skeyword;
                  displaySpaces(os, (int)skeyword.size(), max_size_skw);
                  string comparator = comp_test_p->getComparator();
                  os << " " << comparator << " " << comp_test_p->getValue() << ";" << endl;
              }
          }
          else if (test_type == TST_COMPARE_ATTRIBUTES)
          {
              const MvCompareAttributesTest_t *comp_test_p = dynamic_cast<const MvCompareAttributesTest_t *>(test_p);
              if (comp_test_p)
              {
                  int ikw = comp_test_p->getLeftIKeyword();
                  string skeyword = getSKeyword(ikw);
                  os << skeyword;
                  displaySpaces(os, (int)skeyword.size(), max_size_skw);
                  string comparator = comp_test_p->getComparator();
                  os << " " << comparator;
                  ikw = comp_test_p->getRightIKeyword();
                  os << " " << getSKeyword(ikw) << ";" << endl;
              }
          }
      }
      os << "}" << endl;
      delete test_list;
  }
  for(a_domain=MV_get_domain_begin();a_domain!=MV_get_domain_end();a_domain=MV_get_domain_next(a_domain)) {
    // Data features of the domain
    MvDataFeatureList_t a_dfl;
    getDataFeatures(a_domain,&a_dfl);
    // Removing conditional data features
    MvDataFeatureSet_t a_cond_dfs;
    getConditionalDataFeatures(a_domain,&a_cond_dfs);
    MvDataFeatureSet_t::const_iterator a_cond_it_begin = a_cond_dfs.begin();
    MvDataFeatureSet_t::const_iterator a_cond_it_end   = a_cond_dfs.end();
    MvDataFeatureSet_t::const_iterator a_cond_it;
    for(a_cond_it=a_cond_it_begin;a_cond_it!=a_cond_it_end;++a_cond_it) a_dfl.remove(*a_cond_it);
    // Loop on data features
    if(a_dfl.size()>0) {
      os << "GUI(" << MV_get_domain(a_domain) << ") {" << endl;
      //
      bool a_is_optional=false;
      MvDataFeatureList_t::const_iterator a_it_begin = a_dfl.begin();
      MvDataFeatureList_t::const_iterator a_it_end   = a_dfl.end();
      for(MvDataFeatureList_t::const_iterator a_it=a_it_begin;a_it!=a_it_end;++a_it) {
	if(a_it==a_it_begin || (*a_it)->isOptional()!=a_is_optional) {
	  a_is_optional=(*a_it)->isOptional();
	  if(a_is_optional) os << "optional:"; else os << "mandatory:";
	  os << endl;
	}
	//
	(*a_it)->display(os,*this,1);
	os << endl;
      }
      os << "}" << endl << endl;
    }
  }
  return os;
}

ostream &MvDescriptor_t::displayAttribData(ostream &os, int ikw) const {
    string a_comment = getComment(ikw);
    obj_type_e an_obj_type = getObjectType(ikw);
    value_type_e vtype = getValueType(ikw);
    string solvername = getSolverName(ikw);
    if (vtype == VTYPE_INT)
    {
        os << "INT, ";
    }
    else if (vtype == VTYPE_FLOAT)
    {
        os << "FLOAT, ";
    }
    else if (vtype == VTYPE_BOOL)
    {
        os << "BOOL, ";
    }
    else if (vtype == VTYPE_UINT)
    {
        os << "UINT, ";
    }
    else if (vtype == VTYPE_STRING)
    {
        os << "STRING, ";
    }
    else if (vtype == VTYPE_OBJECT)
    {
        os << MV_get_type(an_obj_type) << ", ";
    }

    os << "\"" << a_comment << "\"";
    if (solvername != "")
    {
        os << ",";
        os << "\"" << solvername << "\"";
        os << ");" << endl;
    }
    else
    {
        if (vtype == VTYPE_OBJECT)
        {
            const descriptor_t *a_cdescr_p = getDescriptorPtr();
            object_descriptor_t *arrdescr_p = (object_descriptor_t *)(a_cdescr_p->attdescr_array[ikw]);
            if (arrdescr_p->num)
            {
                os << ") { SUBTYPES = (";
                for (int i = 0; i < arrdescr_p->num; i++)
                {
                    os << " /" << MV_get_type(arrdescr_p->allowed_types[i]);

                    if (arrdescr_p->subtypes[i])
                    {
                        os << "/" << arrdescr_p->subtypes[i];
                    }
                    if (i != arrdescr_p->num - 1)
                        os << ",";
                }
                os << ") ; }" << endl;
            }
            else
            {
                os << ");" << endl;
            }
        }
        else
        {
            os << ");" << endl;
        }
    }
    return os;
}

ostream &MvDescriptor_t::displayAttribData(ostream &os, MvDomain_e domain, MvIKeywordList_t &list, MvIKeywordList_t *local_attrib_list, int max_size_skw) const {
    MvIKeywordList_t::iterator my_it_b = list.begin();
    MvIKeywordList_t::iterator my_it_e = list.end();
    MvIKeywordList_t::iterator my_it;

    for (my_it = my_it_b; my_it != my_it_e; ++my_it)
    {
        int ikw = *my_it;
        string skeyword = getSKeyword(ikw);

        if (local_attrib_list)
        {
            int value = getIdentifierValue(domain, skeyword);
            if (value == -1)
            {
                local_attrib_list->push_back(ikw);
                continue;
            }
        }

        os << "  ";
        os << skeyword;
        displaySpaces(os, (int)skeyword.size(), max_size_skw);
        os << " = ";
        attribute_type_e atype = getAttributeType(ikw);
        string a_comment = getComment(ikw);
        string solvername = getSolverName(ikw);
        switch (atype)
        {
        case ATYPE_VALUE:
        {
            os << "VALUE(";
            displayAttribData(os, ikw);
            break;
        }
        case ATYPE_SIZE:
        {
            os << "SIZE(\"" << a_comment << "\"";
            if (solvername != "")
            {
                os << ",";
                os << "\"" << solvername << "\"";
                os << ");";
            }
            else
            {
                os << ");";
            }
            os << endl;
            break;
        }
        case ATYPE_DYNAMIC_ARRAY:
        {
            os << "ARRAY[" << getSizeSKeyword(ikw) << "](";
            displayAttribData(os, ikw);
            break;
        }
        case ATYPE_STATIC_ARRAY:
        {
            MvSizeVector sizeArrayVector;
            getDimensionSize(ikw, sizeArrayVector);
            os << "ARRAY[" << sizeArrayVector[0].size << "](";
            displayAttribData(os, ikw);
            break;
        }
        default:
            break;
        }
    }
    return os;
}

ostream &MvDescriptor_t::displaySpaces(ostream &os, int skeyw_size, int max_skeyw_size) const {
    if (skeyw_size < max_skeyw_size)
    {
        for (int i = 0; i < (max_skeyw_size - skeyw_size); i++)
        {
            os << " ";
        }
    }
    return os;
}
/* --------- Output of an object in an output stream (non member function) --------- */

ostream &operator<<(ostream &os,const MvDescriptor_t &descr) { 
  return descr.display(os); 
}

const MvDataSelectionFeature_t* MvDescriptor_t::getSelectionFeature(const char *rootname) const
{
    LocDataSelectionFeatureMap_t *a_sel_map_p = (LocDataSelectionFeatureMap_t *)mySelectionMapPtr;
    if(NULL == a_sel_map_p)
    {
        addSelectionFeatures(DOM_COMMON);
        a_sel_map_p = (LocDataSelectionFeatureMap_t *)mySelectionMapPtr;
    }

    LocDataSelectionFeatureMap_t::iterator itr = a_sel_map_p->find(rootname);
    MvDataSelectionFeature_t *selectionfeatureptr=NULL;
    if(itr != a_sel_map_p->end())
         selectionfeatureptr = itr->second;

    return selectionfeatureptr;
}

MvDataSelectionFeature_t* MvDescriptor_t::addSelectionFeature(MvDomain_e domain,int ikeyword) const
{
    MvDataSelectionFeature_t *dataselection_p=new MvDataSelectionFeature_t(this,ikeyword);
    LocDataSelectionFeatureMap_t *a_definition_map  = (LocDataSelectionFeatureMap_t *)mySelectionMapPtr;
    string skeyword = getSKeyword(ikeyword);
    size_t pos = skeyword.find("_",0);
    skeyword.erase(pos);
    (*a_definition_map)[skeyword] = dataselection_p;
    return dataselection_p;
}

void MvDescriptor_t::addSelectionFeatures(MvDomain_e domain) const
{
    mySelectionMapPtr = (MvPseudoSelectionListMap_t *) new LocDataSelectionFeatureMap_t();

    /* The convention is that all attributes in the "SELECTION_ROOT" definition
    * are the default ikeywords of selection features. */
    const MvIKeywordSet_t a_iks = getDefinition((int)domain,"SELECTION_ROOT");
    MvIKeywordSet_t::const_iterator it;
    MvIKeywordSet_t::const_iterator it_begin = a_iks.begin();
    MvIKeywordSet_t::const_iterator it_end = a_iks.end();

    for(it = it_begin; it != it_end; ++it)
    {
        //int default_ikeyword = (*it);
        //MvDataSelectionFeature_t *a_dsf = addSelectionFeature(domain,default_ikeyword);
    }
}
void MvDescriptor_t::setConfigType(unsigned int configtype) {
  myConfigType = configtype;
}
void MvDescriptor_t::setHmType(unsigned int hmtype) {
  myHmType = hmtype;
}
void MvDescriptor_t::setKeyword(string keyword) {
    myKeyword = keyword;
}
void MvDescriptor_t::setCardImage(char* card_image) {
    myCardImage = card_image;
}
void MvDescriptor_t::setIdPool(short int idpool) {
    myIdPool = idpool;
}
unsigned int MvDescriptor_t::getConfigType() const {
  return myConfigType;
}
unsigned int MvDescriptor_t::getHmType() const {
  return myHmType;
}
const string& MvDescriptor_t::getKeyword() const {
    return myKeyword;
}
char*  MvDescriptor_t::getCardImage() const {
    return myCardImage;
}
short int MvDescriptor_t::getIdPool() const {
    return myIdPool;
}

void loc_update_feature_attribute(const MvDescriptor_t *descr_p, MvDataFeature_t* feature, feature_attribute_type_e attrib, int val, set< value_type_e>* vset)
{
    if (feature == NULL)
        return;
    int ikw = feature->getIKeyword();
    if (ikw > 0)
    {
        value_type_e vtype = descr_p->getValueType(ikw);
        if (vset && (vtype < *vset))
        {
            switch (attrib)
            {
                case FEATURE_ATTRIBUTE_EDITABLE:
                {
                    feature->setEditable((bool)val);
                    break;
                }
                case FEATURE_ATTRIBUTE_VISIBLE:
                {
                    feature->setVisible((bool)val);
                    break;
                }
                case FEATURE_ATTRIBUTE_DISPLAY_STATUS:
                {
                    feature->setDisplayStatus(val);
                    break;
                }
                case FEATURE_ATTRIBUTE_PARAMETERIZED:
                {
                    feature->setParameterized((bool)val);
                    break;
                }
                default:
                    break;
            }
        }
    }
}

void MvDescriptor_t::UpdateFeatureAttributes(MvDataFeature_t *feature_p, feature_attribute_type_e attrib, int val, set< value_type_e> *vset) const
{
    MvDataFeatureType_e ftype = feature_p->getType();

    if ((ftype == DFT_DYNAMIC_ARRAY) || (ftype == DFT_STATIC_ARRAY) || (ftype == DFT_ARRAY_TO_SINGLE) || (ftype == DFT_RADIO_ARRAY) ||
        (ftype == DFT_DYNAMIC_MATRIX))
    {
        const MvDataArrayFeature_t* array_feature = dynamic_cast<const MvDataArrayFeature_t*>(feature_p);
        int nb_features = array_feature->getNumber();
        for (int i = 0; i < nb_features; ++i)
        {
            const MvDataFeature_t* a_feature = array_feature->getDataFeature(i);
            loc_update_feature_attribute(this, (MvDataFeature_t *)a_feature, attrib, val, vset);
        }
    }
    else
    {
        loc_update_feature_attribute(this, feature_p, attrib, val, vset);
    }
}
/* --------- Descriptor stocking tools (declaration) --------- */


typedef map<const MvSubtype_t *,MvPreDescriptor_t *> MvPreDescriptorMap_t;





/* --------- Getting a descriptor (public non member) --------- */


const MvDescriptorMapArray_t * CFGKernel::loc_manage_map_array(int option) const {
 // MvDescriptorMapArray_t *loc_descr_map_array_p=p_descr_map_array_p;
  //
  switch(option) {
  case 0:
    if(p_descr_map_array_p ==NULL) p_descr_map_array_p =new MvDescriptorMapArray_t();
    break;
  case 1:
    if(p_descr_map_array_p !=NULL) { delete p_descr_map_array_p; p_descr_map_array_p =NULL; }
    break;
  }
  //
  return p_descr_map_array_p;
}


const MvDescriptor_t * CFGKernel::get_descriptor(const MvFullType_t &fulltype) const {
  return get_descriptor(fulltype.getType(),fulltype.getSubtypePtr());
}

const MvDescriptor_t * CFGKernel::get_descriptor(object_type_e type,const MvSubtype_t *subtype_p) const {
  
  const MvDescriptor_t *a_descr_p=NULL;
  //
  if(type != HCDI_OBJ_TYPE_SUBOBJECT && (is_user(type)) ) {
      const MvPreDatasHierarchy_t* a_mv_data_cfg_p = get_tree_cfg(DTT_ASSIGNED, "Assigned data");// MV_get_data_cfg();
    if(NULL==a_mv_data_cfg_p)
        return NULL;
    const MvPreDatasHierarchy_t *a_data_cfg_p=a_mv_data_cfg_p->search(MvFullType_t(type,subtype_p));
    if(a_data_cfg_p!=NULL) a_descr_p=a_data_cfg_p->getDescriptorPtr((void *)this);
  }
  //
  if(type != HCDI_OBJ_TYPE_SUBOBJECT && a_descr_p==NULL) {
    
    const MvDescriptorMapArray_t &a_dma=(*loc_manage_map_array(0));
    a_descr_p=a_dma.getDescriptorPtr((void *)this, type,subtype_p);
    /* need_to_be_checked */
    /*if((a_descr_p==NULL)&&(type==CONNECTION))
    {
        const MvSubtype_t *local_subtype_p = get_subtype_connection(type,(connection_type_e) subtype_p->getSubtype());
        a_descr_p=a_dma.getDescriptorPtr(type,local_subtype_p);
    }
    */
    
  }
  //
  if(a_descr_p==NULL && subtype_p!=NULL) { // Crypted material/property
    a_descr_p=get_user_descriptor(type,subtype_p->getKeyword());
  }  
  //
  return a_descr_p;
  
}


const MvDescriptor_t * CFGKernel::get_descriptor(object_type_e type,const string &keyword) const {
  const MvSubtype_t *a_subtype_p=get_subtype(type,keyword);
  return get_descriptor(type,a_subtype_p);
}
const MvDescriptor_t * CFGKernel::get_descriptor(const string &fulltype) const {
  MvFullType_t a_fulltype(*this, fulltype);
  return get_descriptor(a_fulltype.getType(),a_fulltype.getSubtypePtr());
}

const MvDescriptor_t * CFGKernel::get_user_descriptor(object_type_e type,int user_id) const {
  const MvPreDescriptor_t *a_pre_descr_p=get_user_pre_descriptor(type,user_id);
  if(a_pre_descr_p==NULL) return NULL;
  return a_pre_descr_p->getDescriptorPtr((void *)this);
}

const MvDescriptor_t * CFGKernel::get_user_descriptor(object_type_e type,int hm_config_type, int hm_type) const {
  const MvPreDescriptor_t *a_pre_descr_p=get_user_pre_descriptor(type,hm_config_type, hm_type);
  if(a_pre_descr_p==NULL) return NULL;
  return a_pre_descr_p->getDescriptorPtr((void*)this);
}
const MvDescriptor_t * CFGKernel::get_user_descriptor(object_type_e type,const string &keyword) const {
  const MvPreDescriptor_t *a_pre_descr_p=get_user_pre_descriptor(type,-1,-1,keyword);
  if(a_pre_descr_p==NULL) return NULL;
  return a_pre_descr_p->getDescriptorPtr((void*)this);
}
const MvDescriptor_t * CFGKernel::get_user_descriptor_userkeyword(object_type_e type, int hm_config_type, int hm_type, const string &keyword, string &first_user_keyword) const {
    const MvPreDescriptor_t *a_pre_descr_p=get_user_pre_descriptor(type, hm_config_type, hm_type, keyword);
    if(a_pre_descr_p==NULL) return NULL;
    first_user_keyword.clear();
    first_user_keyword += a_pre_descr_p->getKeyword();

    return a_pre_descr_p->getDescriptorPtr((void*)this);
}

void CFGKernel::get_user_descriptor_userkeyword(object_type_e type,int config_type, int hm_type, string &first_user_keyword) const {
    const MvPreDescriptor_t *a_pre_descr_p=get_user_pre_descriptor(type,config_type, hm_type);
    if(a_pre_descr_p==NULL) return;
    first_user_keyword.clear();
    first_user_keyword += a_pre_descr_p->getKeyword();
}



void CFGKernel::init_descriptors() {
  loc_manage_map_array(0);
  loc_add_special_parameters(); 
}



void CFGKernel::close_descriptors() {
  loc_manage_map_array(1);
}



/* --------- Descriptor stocking tools (implementation) --------- */

MvDescriptorMapArray_t::MvDescriptorMapArray_t() {
  
}

MvDescriptorMapArray_t::~MvDescriptorMapArray_t() {
  for(int i=0;i<MV_NB_MCDS_TYPES;++i) {
    MvPreDescriptorMap_t &a_map=myPreDescriptorMapArray[i];
    //
    MvPreDescriptorMap_t::iterator a_it_begin = a_map.begin();
    MvPreDescriptorMap_t::iterator a_it_end   = a_map.end();
    MvPreDescriptorMap_t::iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
      MvPreDescriptor_t *a_pre_descriptor_p=(*a_it).second;
      a_pre_descriptor_p->deleteDescriptor(); 
      delete a_pre_descriptor_p;
    }
  }
}

const MvDescriptor_t *MvDescriptorMapArray_t::getDescriptorPtr(void* cfgkernel, object_type_e type,
							       const MvSubtype_t *subtype_p) const 
{
  if(((int)type)<0) return NULL;
  //
  MvPreDescriptorMap_t::const_iterator it=myPreDescriptorMapArray[type].find(subtype_p);
  if(it==myPreDescriptorMapArray[type].end()) return NULL;
  return (*it).second->getDescriptorPtr(cfgkernel); 
}



MvPreDescriptor_t *MvDescriptorMapArray_t::addPreDescriptor(void *cfgkernel, const string &filename,object_type_e type,const string &keyword,int user_id,int hm_config_type,int hm_type, short int idpool, string cardImage, const string& optional_header_string) {
  if (!cfgkernel)
     return nullptr;

  MvPreDescriptorMap_t &a_pdm=myPreDescriptorMapArray[type];
  const MvSubtype_t    *a_subtype_p=NULL;

  CFGKernel* a_cfgkernel = (CFGKernel*)cfgkernel;

  if(keyword!="") {
    if(user_id==-1) a_subtype_p= a_cfgkernel->get_subtype(type,keyword);
    else            a_subtype_p= a_cfgkernel->add_user_subtype(type,user_id,hm_config_type,hm_type, idpool, cardImage, keyword, optional_header_string);
  }
  //
  
  char* card_image = NULL;
  if(a_subtype_p)
     card_image = a_subtype_p->getCardImage();
  MvPreDescriptor_t *a_pre_descr_p=new MvPreDescriptor_t(filename,type,keyword,user_id, hm_config_type, hm_type, idpool, card_image);
  if(user_id==-1) a_pdm[a_subtype_p]=a_pre_descr_p;
  else            a_cfgkernel->add_user_pre_descriptor(type,a_pre_descr_p,keyword);
  a_pre_descr_p->getDescriptorPtr(cfgkernel);
  
  //
  return a_pre_descr_p; 
}



/* --------- Static functions --------- */

static int loc_get_cell_length(ff_cell_t *cell_p)
{
    const char *cell_format_p = NULL;
    MCDS_get_ff_cell_attributes(cell_p,CELL_FORMAT,&cell_format_p,END_ARGS);

    int length = 0;
    if (NULL != cell_format_p)
    {
        sscanf(cell_format_p+1, "%d", &length);
        length = abs(length);
    }

    return length;
}

static char *loc_string_fit_all(char *name)
{
    int i= 0,length = 0, i_new = 0; 

    if (name == NULL) 
        return NULL;

    length = (int)strlen (name);
    i_new = 0;
    for (i = 0; i < length; i++) 
    {
        if (name[i] != ' ') 
        {
            name[i_new] = name[i];
            i_new++;
        }
    }
    name[i_new] = '\0';
    
    return name;
}


static void loc_add_special_parameters() {
  
  MvSubtypePtrSet_t a_subtypes;
  /* need_to_be_checked */
 
  // TH variables (output parameters)
}

