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



#include <UTILS/mv_cstdarg.h> 
#include <UTILS/mv_string.h>
#include <UTILS/mv_cstring.h>
#include <UTILS/mv_stl_various.h>
#include <UTILS/error.h>
#include <MESSAGE/mv_messages.h>


#include "mv_type.h"
#include "HCDI/hcdi_multicfgkernelmgr.h"
#include "cfg_kernel.h"


/* --------- MvKeywordSubtypeMap_t (declaration) --------- */




//static MvTypeKeywordSubtypeMap_t &get_subtype_map();
//static MvTypeUserIdSubtypeMap_t &get_user_subtype_map();
//static MvHMConfigTypeSubtypeMap_t &get_hm_config_type_subtype_map();
//static MvHMTypeSubtypeMap_t &get_hm_type_subtype_map();



//static MvTypeKeywordSubtypeMap_t MV_SUBTYPE_MAP[FF_LAST];
//static MvTypeUserIdSubtypeMap_t p_user_subtype_map[FF_LAST];
//static MvHMConfigTypeSubtypeMap_t MV_HM_CONFIG_TYPE_SUBTYPE_MAP[FF_LAST];
//static MvHMTypeSubtypeMap_t MV_HM_TYPE_SUBTYPE_MAP[FF_LAST];
/* --------- MvSubtype_t --------- */

// Constructor for simple subtype
MvSubtype_t::MvSubtype_t(const string &keyword, int subtype, string cardImage, MvStringList_t *user_name_list, int user_id, int hm_config_type, int hm_type, short int idpool, MvStringList_t* optional_header_string_list) :
  mySubtype(subtype), 
  myKeyword(keyword), 
  myUserId(user_id),
  myConfigType(hm_config_type),
  myHmType(hm_type),
  myFlagValueList(),
  myIdPool(idpool),
  myUserNames()
{
    if (cardImage != "")
        myCardImage = strdup(cardImage.c_str());
    else
        myCardImage = NULL;
    if (user_name_list)
    {
        int size = (int)user_name_list->size();
        for (int i = 0; i < size; i++)
        {
            string elem = user_name_list->at(i);
            myUserNames.push_back(elem);
        }
    }

    if (optional_header_string_list)
    {
        int size = (int)optional_header_string_list->size();
        for (int i = 0; i < size; i++)
        {
            string elem = optional_header_string_list->at(i);
            myOptionalHeaderStrings.insert(elem);
        }
    }


}

// Constructor for subtype with a list of flag values
MvSubtype_t::MvSubtype_t(const string &keyword,int subtype,int user_id,int attrib_kw,...) :
  mySubtype(subtype), 
  myKeyword(keyword), 
  myUserId(user_id),
  myCardImage(nullptr),
  myFlagValueList(),
  myUserNames()
{
  va_list arglist;
  int     attrib_keyword,attrib_value;
  myConfigType=0;
  myHmType=0;
  myIdPool = 0;
  //
  va_start(arglist, attrib_kw);
  attrib_keyword=attrib_kw;
  //
  while(attrib_keyword!=END_ARGS) {
    attrib_value=va_arg(arglist,int);
    myFlagValueList.push_back(MvFlagValue_t(attrib_keyword,attrib_value));
    attrib_keyword=va_arg(arglist,int);
  }
  va_end(arglist);
  myUserNames.clear();
  myOptionalHeaderStrings.clear();
}

MvSubtype_t::~MvSubtype_t() {
    if (myCardImage)
    {
        free(myCardImage);
        myCardImage = NULL;
    }
    myUserNames.clear();
    myOptionalHeaderStrings.clear();
  //cout << "MvSubtype_t::~MvSubtype_t -> " << myKeyword << endl;
}

// output in an output stream
ostream &operator<<(ostream &os,const MvSubtype_t *subtype_p) {
  os << subtype_p->getKeyword() 
     << " (" << subtype_p->getSubtype() << ")";
  const MvFlagValueList_t &a_fvl=subtype_p->getFlagValueList();
  if(a_fvl.size()>0) {
    os << " ->";
    for(MvFlagValueList_t::const_iterator it=a_fvl.begin();it!=a_fvl.end();++it) {
      os << " (" << (*it).getKeyword() << "," << (*it).getValue() << ")";
    }
  }
  return os;
}


MvKeywordSet_t *MvSubtype_t::getKeywords(object_type_e otype,MvKeywordSet_t *keywords_p) const {
  MvKeywordSet_t *a_keywords_p=(keywords_p==NULL ? new MvKeywordSet_t() : keywords_p);
  //
  CFGKernel* a_kernel_model = const_cast<CFGKernel *>(MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel());
  if (!a_kernel_model)
      return nullptr;

  const MvTypeKeywordSubtypeMap_t& a_tksm = a_kernel_model->get_subtype_map();
  // get_subtype_map(); /*mutimodel check for alternative*/

  MvTypeKeywordSubtypeMap_t::const_iterator  a_tksm_it = a_tksm.find(otype);
  if(a_tksm_it==a_tksm.end()) return NULL;
  //
  const MvKeywordSubtypeMap_t &a_ksm=(*a_tksm_it).second;
  MvKeywordSubtypeMap_t::const_iterator a_ksm_it_begin = a_ksm.begin();
  MvKeywordSubtypeMap_t::const_iterator a_ksm_it_end   = a_ksm.end();
  MvKeywordSubtypeMap_t::const_iterator a_ksm_it;
  for(a_ksm_it=a_ksm_it_begin;a_ksm_it!=a_ksm_it_end;++a_ksm_it) {
    const string      &a_keyword   = (*a_ksm_it).first;
    const MvSubtype_t *a_subtype_p = (*a_ksm_it).second;
    //
    if(a_subtype_p==this) a_keywords_p->insert(a_keyword);
  }
  //
  return a_keywords_p;
}



/* --------- Static functions --------- */

MvTypeKeywordSubtypeMap_t& CFGKernel::get_subtype_map() {
  //static MvTypeKeywordSubtypeMap_t MV_SUBTYPE_MAP;
  return p_subtype_map;
}





MvTypeUserIdSubtypeMap_t& CFGKernel::get_user_subtype_map()  {
  return p_user_subtype_map;
}
MvHMConfigTypeSubtypeMap_t& CFGKernel::get_hm_config_type_subtype_map()  {
  return p_hm_config_type_subtype_map;
}
MvHMTypeSubtypeMap_t & CFGKernel::get_hm_type_subtype_map()  {
  return p_hm_type_subtype_map;
}

/* --------- Public functions --------- */

const MvSubtype_t *CFGKernel::get_subtype(object_type_e obj_type,const string &keyword,connection_type_e econtype) const {
  const MvTypeKeywordSubtypeMap_t::const_iterator tksm_it= p_subtype_map.find(obj_type);
  if(tksm_it== p_subtype_map.end()) return NULL;
  //
  const MvKeywordSubtypeMap_t &a_ksm=(*tksm_it).second;
  MvKeywordSubtypeMap_t::const_iterator ksm_it=a_ksm.find(keyword);

  // For now, we are not allowing partial keyword name support.
  // We are trying to find the exact name as mentioned in the user names.
  // If keyword not found then returning null from here only rather than
  // trying to read the partial keyword. 
  // Some exception are added here as needed, but we do not want to take risk by doing generically.
  // We will see in future if partial reading is needed.
  if (ksm_it == a_ksm.end())
  {
      switch(obj_type)
      {
      case HCDI_OBJ_TYPE_COMPS:
          break; // try partial reading below
      default:
          return NULL;
      }
  }

  // If not found, here is another chance
  // The keyword might be the string in the map with some options appended
  if(ksm_it==a_ksm.end()) {
    ksm_it=a_ksm.begin();
    const MvSubtype_t *a_subtype_p=NULL;
    while(ksm_it!=a_ksm.end() && a_subtype_p==NULL) {
      const char *a_map_str=(*ksm_it).first.c_str();
      const char *a_kwd_str=keyword.c_str();
      int         a_map_str_length=(int)(strlen(a_map_str));
      if(!strncmp(a_map_str,a_kwd_str,a_map_str_length)) a_subtype_p=(*ksm_it).second;
      ++ksm_it;
    }
    return a_subtype_p;
  }
  // If found, no problemo
  return (*ksm_it).second;
}

const MvSubtype_t* MV_get_subtype(object_type_e obj_type, const string& keyword, connection_type_e econtype) {

    const CFGKernel* a_kernel_model = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_kernel_model)
        return nullptr;

    return a_kernel_model->get_subtype(obj_type, keyword, econtype);
}

static const string &loc_get_connection_type_keyword(connection_type_e con_type);
static const string &loc_get_connection_type_keyword(connection_type_e con_type) {
  static string CON_KEYWORDS[CONNECTION_LAST];
  static bool first=true;
  static string sempty;
  if(con_type>=CONNECTION_LAST) {
      return sempty;
  }
  //
  if(first) {
    first=false;
    //
    CON_KEYWORDS[CONNECTION_SPOTWELD]     = "SPOTWELD";
    CON_KEYWORDS[CONNECTION_BOLT]         = "BOLT";
    CON_KEYWORDS[CONNECTION_WELDING_LINE] = "WELDING_LINE";
    CON_KEYWORDS[CONNECTION_GLUE]         = "GLUE";
    CON_KEYWORDS[CONNECTION_HEMMING]      = "HEMMING";
  }
  //
  return CON_KEYWORDS[con_type];
}

const MvSubtype_t * CFGKernel::get_subtype_connection(object_type_e obj_type,connection_type_e econtype) const
{
  string        a_skeyword;
  a_skeyword=loc_get_connection_type_keyword(econtype);
  return get_subtype(obj_type,a_skeyword,econtype);
}

const MvSubtype_t * CFGKernel::get_user_subtype(object_type_e obj_type,int user_id) const {
  return get_user_subtype(MV_get_type(obj_type),user_id);
}

const MvSubtype_t * CFGKernel::get_user_subtype(const string &obj_type,int user_id) const {
  MvTypeUserIdSubtypeMap_t::const_iterator a_tuism_it= p_user_subtype_map.find(obj_type);
  //  if(a_tuism_it==get_subtype_map().end()) return NULL;
  if(a_tuism_it== p_user_subtype_map.end()) return NULL;
  //
  const MvUserIdSubtypeMap_t           &a_uism    = (*a_tuism_it).second;
  MvUserIdSubtypeMap_t::const_iterator  a_uism_it = a_uism.find(user_id);
  if(a_uism_it==a_uism.end()) return NULL;
  //
  return (*a_uism_it).second;
}
const MvSubtype_t * CFGKernel::get_config_type_subtype(object_type_e obj_type, int hm_config_type, int hm_type) const {
  return get_config_type_subtype(MV_get_type(obj_type),hm_config_type,hm_type);
}

const MvSubtype_t * CFGKernel::get_config_type_subtype(const string &obj_type, int hm_config_type, int hm_type) const {

    if(hm_config_type < 0 && hm_type < 0)
        return NULL;
    MvTypeUserIdSubtypeMap_t::const_iterator a_tuism_it;

    if(hm_config_type > 0 && hm_type < 0)
        a_tuism_it= p_hm_config_type_subtype_map.find(obj_type);
    else if(hm_config_type < 0 && hm_type > 0)
        a_tuism_it= p_hm_type_subtype_map.find(obj_type);
    else if(hm_config_type > 0 && hm_type > 0)
        a_tuism_it= p_hm_config_type_subtype_map.find(obj_type);

    if(a_tuism_it== p_hm_config_type_subtype_map.end()) return NULL;
    //
    const MvUserIdSubtypeMap_t           &a_uism    = (*a_tuism_it).second;
    MvUserIdSubtypeMap_t::const_iterator  a_uism_it;

    if(hm_config_type > 0 && hm_type < 0)
        a_uism_it = a_uism.find(hm_config_type);
    else if(hm_config_type < 0 && hm_type > 0)
       a_uism_it = a_uism.find(hm_type);
    else if(hm_config_type > 0 && hm_type > 0)
    {
        //a_uism_it = a_uism.find(hm_config_type);
        //need  to iterate
        MvUserIdSubtypeMap_t::const_iterator  a_it;

        for(a_it=a_uism.begin();a_it!=a_uism.end();++a_it)
        {
            MvSubtype_t *a_sub_type = (*a_it).second;
            if(a_sub_type->getHMConfigType() == hm_config_type && a_sub_type->getHMType() == hm_type)
            {
               return a_sub_type;
            }
        }

    }

    if(a_uism_it==a_uism.end()) return NULL;
    //
    return (*a_uism_it).second;
}
int MV_get_MCDS_user_subtype(object_type_e obj_type) {
  static int  MV_MCDS_USER_SUBTYPES[MV_NB_MCDS_TYPES];/*multimodel.. not required all are subtypes...*/
  static bool first=true;
  //
  if(first) {
    for(int i=0;i<MV_NB_MCDS_TYPES;++i) MV_MCDS_USER_SUBTYPES[i]=0;
  }
  //
  if(obj_type<0 || obj_type>=MV_NB_MCDS_TYPES) return -1;
  return MV_MCDS_USER_SUBTYPES[obj_type];
}

const MvSubtype_t * CFGKernel::add_user_subtype(object_type_e obj_type,int user_id,int hm_config_type,int hm_type, short int idpool, string &cardImage,
                       const PseudoStringList_t *user_name_list_p, const PseudoStringList_t* optional_header_string_list_p,
                       bool add_to_keyword_map)
{
  if(!MV_is_user_subtyped(obj_type)) {
  #ifdef _DEBUG
  printf("%s -- %d: ", __FILE__,__LINE__);
  #endif  
    throw MvError_t(MV_get_msg_array(MSGT_KERNEL)[39],user_id,MV_get_type(obj_type).c_str());
  }
  //
  const MvStringList_t *a_user_name_list_p=(const MvStringList_t *)user_name_list_p;
  const string &a_keyword=*(a_user_name_list_p->begin());

  const MvStringList_t* a_optional_header_string_list_p = (const MvStringList_t*)optional_header_string_list_p;
  

  MvSubtype_t *a_subtype_p=new MvSubtype_t(a_keyword,MV_get_MCDS_user_subtype(obj_type), cardImage, (MvStringList_t *)a_user_name_list_p, user_id, hm_config_type, hm_type, idpool, (MvStringList_t*)a_optional_header_string_list_p);
  //
  if(add_to_keyword_map)
  {
      MvKeywordSubtypeMap_t &a_ksm= p_subtype_map[obj_type];
      MvStringList_t::const_iterator a_it_begin = a_user_name_list_p->begin();
      MvStringList_t::const_iterator a_it_end   = a_user_name_list_p->end();
      MvStringList_t::const_iterator a_it;
      for(a_it=a_it_begin; a_it!=a_it_end; ++a_it) a_ksm[*a_it]=a_subtype_p;
  }
  //
  if(user_id > 0)
      p_user_subtype_map[MV_get_type(obj_type)][user_id]=a_subtype_p;

  if(hm_config_type <= 0 && hm_type > 0)
      p_hm_type_subtype_map[MV_get_type(obj_type)][hm_type]=a_subtype_p;
  else if(hm_config_type > 0)
      p_hm_config_type_subtype_map[MV_get_type(obj_type)][hm_config_type]=a_subtype_p;

  //
  return a_subtype_p;
}


const MvSubtype_t * CFGKernel::add_user_subtype(object_type_e obj_type,int user_id,int hm_config_type,int hm_type, short int idpool, string &cardImage, const string &user_name, const string& optional_header_string)  {
  MvStringList_t a_user_name_list;
  a_user_name_list.push_back(user_name);
  MvStringList_t a_optional_header_string_list;
  a_optional_header_string_list.push_back(optional_header_string);
  //
  const MvSubtype_t *a_subtype_p=add_user_subtype(obj_type,user_id, hm_config_type, hm_type, idpool, cardImage,
                             (const PseudoStringList_t *)(&a_user_name_list), (const PseudoStringList_t*)(&a_optional_header_string_list));
  return a_subtype_p;
}


bool MV_is_user_subtyped(object_type_e obj_type) {
  return (MV_get_MCDS_user_subtype(obj_type)>=0);
}

bool CFGKernel::MV_is_subtyped(object_type_e obj_type) const {
  const MvTypeKeywordSubtypeMap_t &a_subtype_map= p_subtype_map;
  return a_subtype_map.find(obj_type)!=a_subtype_map.end();
}
bool MV_is_subtyped(object_type_e obj_type) {

    CFGKernel* a_kernel_model = const_cast<CFGKernel *>(MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel());
    if (!a_kernel_model)
        return false;

    const MvTypeKeywordSubtypeMap_t& a_subtype_map = a_kernel_model->get_subtype_map();
   // const MvTypeKeywordSubtypeMap_t& a_subtype_map = get_subtype_map();
    return a_subtype_map.find(obj_type) != a_subtype_map.end();
}

MvSubtypePtrSet_t * CFGKernel::get_subtypes(object_type_e otype,MvSubtypePtrSet_t *subtypes_p) const {
  MvSubtypePtrSet_t *a_subtypes_p=(subtypes_p==NULL ? new MvSubtypePtrSet_t() : subtypes_p);
  //
  const MvTypeKeywordSubtypeMap_t           &a_subtype_map = p_subtype_map;
  MvTypeKeywordSubtypeMap_t::const_iterator  a_sm_it       = a_subtype_map.find(otype);
  if(a_sm_it!=a_subtype_map.end()) {
    const MvKeywordSubtypeMap_t &a_ksm=(*a_sm_it).second;
    MvKeywordSubtypeMap_t::const_iterator a_it_begin = a_ksm.begin();
    MvKeywordSubtypeMap_t::const_iterator a_it_end   = a_ksm.end();
    MvKeywordSubtypeMap_t::const_iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
      const MvSubtype_t *a_subtype_p=(*a_it).second;
      a_subtypes_p->insert(a_subtype_p);
    }
  }
  //
  return a_subtypes_p;
}

//MvSubtypePtrSet_t* MV_get_subtypes(object_type_e otype, MvSubtypePtrSet_t* subtypes_p) {
//    MvSubtypePtrSet_t* a_subtypes_p = (subtypes_p == NULL ? new MvSubtypePtrSet_t() : subtypes_p);
//    //
//    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
//
//    CFGKernel* a_kernel_model = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
//    if (!a_kernel_model)
//        return nullptr;
//
//
//    const MvTypeKeywordSubtypeMap_t& a_subtype_map = a_kernel_model->get_subtype_map(); // get_subtype_map();
//    MvTypeKeywordSubtypeMap_t::const_iterator  a_sm_it = a_subtype_map.find(otype);
//    if (a_sm_it != a_subtype_map.end()) {



//        MvKeywordSubtypeMap_t::const_iterator a_it;
//        for (a_it = a_it_begin; a_it != a_it_end; ++a_it) {
//            const MvSubtype_t* a_subtype_p = (*a_it).second;
//            a_subtypes_p->insert(a_subtype_p);
//        }
//    }
//    //
//    return a_subtypes_p;
//}

void CFGKernel::delete_all_subtypes()
{

    set<MvSubtype_t *> all_subtype_set;
    MvTypeKeywordSubtypeMap_t          &a_subtype_map = p_subtype_map;
    MvTypeUserIdSubtypeMap_t           &a_user_subtype_map = get_user_subtype_map();
    MvHMConfigTypeSubtypeMap_t         &a_config_user_subtype_map = get_hm_config_type_subtype_map();
    MvHMTypeSubtypeMap_t               &a_hmtype_user_subtype_map = get_hm_type_subtype_map();
    //
    {
        MvTypeKeywordSubtypeMap_t::iterator  a_sm_beg_it       = a_subtype_map.begin();
        MvTypeKeywordSubtypeMap_t::iterator  a_sm_end_it       = a_subtype_map.end();
        MvTypeKeywordSubtypeMap_t::iterator  a_sm_it;

        for(a_sm_it=a_sm_beg_it;a_sm_it!=a_sm_end_it;++a_sm_it) 
        {

            MvKeywordSubtypeMap_t &a_ksm=(*a_sm_it).second;


            MvKeywordSubtypeMap_t::iterator a_it_begin = a_ksm.begin();
            MvKeywordSubtypeMap_t::iterator a_it_end   = a_ksm.end();
            MvKeywordSubtypeMap_t::iterator a_it;

            for(a_it=a_it_begin;a_it!=a_it_end;++a_it) 
            {
                MvSubtype_t *a_subtype_p=(*a_it).second;
                all_subtype_set.insert(a_subtype_p);
                (*a_it).second = NULL;
            }

        }
    }
    //
    {
        MvTypeUserIdSubtypeMap_t::iterator  a_sm_beg_it       = a_user_subtype_map.begin();
        MvTypeUserIdSubtypeMap_t::iterator  a_sm_end_it       = a_user_subtype_map.end();
        MvTypeUserIdSubtypeMap_t::iterator  a_sm_it;
        for(a_sm_it=a_sm_beg_it;a_sm_it!=a_sm_end_it;++a_sm_it) 
        {

            MvUserIdSubtypeMap_t &a_ksm=(*a_sm_it).second;


            MvUserIdSubtypeMap_t::iterator a_it_begin = a_ksm.begin();
            MvUserIdSubtypeMap_t::iterator a_it_end   = a_ksm.end();
            MvUserIdSubtypeMap_t::iterator a_it;
            set<MvSubtype_t *> subtype_set;
            for(a_it=a_it_begin;a_it!=a_it_end;++a_it) 
            {
                MvSubtype_t *a_subtype_p=(*a_it).second;
                all_subtype_set.insert(a_subtype_p);
                (*a_it).second = NULL;
            }
        }
    }
    //
    {
        MvHMConfigTypeSubtypeMap_t::iterator  a_sm_beg_it       = a_config_user_subtype_map.begin();
        MvHMConfigTypeSubtypeMap_t::iterator  a_sm_end_it       = a_config_user_subtype_map.end();
        MvHMConfigTypeSubtypeMap_t::iterator  a_sm_it;

        for(a_sm_it=a_sm_beg_it;a_sm_it!=a_sm_end_it;++a_sm_it) 
        {

            MvUserIdSubtypeMap_t &a_ksm=(*a_sm_it).second;

            MvUserIdSubtypeMap_t::iterator a_it_begin = a_ksm.begin();
            MvUserIdSubtypeMap_t::iterator a_it_end   = a_ksm.end();
            MvUserIdSubtypeMap_t::iterator a_it;
            set<MvSubtype_t *> subtype_set;
            for(a_it=a_it_begin;a_it!=a_it_end;++a_it) 
            {
                MvSubtype_t *a_subtype_p=(*a_it).second;
                all_subtype_set.insert(a_subtype_p);
                (*a_it).second = NULL;
            }
        }
    }
    //
    {
        MvHMTypeSubtypeMap_t::iterator  a_sm_beg_it       = a_hmtype_user_subtype_map.begin();
        MvHMTypeSubtypeMap_t::iterator  a_sm_end_it       = a_hmtype_user_subtype_map.end();
        MvHMTypeSubtypeMap_t::iterator  a_sm_it;

        for(a_sm_it=a_sm_beg_it;a_sm_it!=a_sm_end_it;++a_sm_it) 
        {

            MvUserIdSubtypeMap_t &a_ksm=(*a_sm_it).second;

            MvUserIdSubtypeMap_t::iterator a_it_begin = a_ksm.begin();
            MvUserIdSubtypeMap_t::iterator a_it_end   = a_ksm.end();
            MvUserIdSubtypeMap_t::iterator a_it;
            set<MvSubtype_t *> subtype_set;
            for(a_it=a_it_begin;a_it!=a_it_end;++a_it) 
            {
                MvSubtype_t *a_subtype_p=(*a_it).second;
                all_subtype_set.insert(a_subtype_p);
                (*a_it).second = NULL;
            }
        }
    }
    //free all sutypes
    {
        set<MvSubtype_t *>::iterator a_it_begin = all_subtype_set.begin();
        set<MvSubtype_t *>::iterator a_it_end   = all_subtype_set.end();
        set<MvSubtype_t *>::iterator a_it;
        for(a_it=a_it_begin;a_it!=a_it_end;++a_it) 
        {
            MvSubtype_t *a_subtype_p=(*a_it);
            delete a_subtype_p;
        }
    }
    a_subtype_map.clear();
    a_user_subtype_map.clear();
    a_config_user_subtype_map.clear();
    a_hmtype_user_subtype_map.clear();

}





/* --------- MvKeywordSubtypeMap_t (implementation) --------- */

MvKeywordSubtypeMap_t::~MvKeywordSubtypeMap_t() {
  set<MvSubtype_t *> subtype_set;
  //
  for(MvKeywordSubtypeMap_t::iterator it=begin();it!=end();++it) {
    MvSubtype_t *a_subtype_p=(*it).second;
    if(a_subtype_p && subtype_set.insert(a_subtype_p).second) delete a_subtype_p;
  }
}

MvTypeKeywordSubtypeMap_t::MvTypeKeywordSubtypeMap_t() : map<object_type_e,MvKeywordSubtypeMap_t>() {

}
