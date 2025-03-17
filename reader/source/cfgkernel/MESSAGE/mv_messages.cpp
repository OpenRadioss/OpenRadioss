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
  #include <UTILS/str_utils.h>         
  #include <UTILS/memory_utils.h>  
  #include <UTILS/system_utils.h>     
  #include <UTILS/error.h>         
  #include <PARSER/mv_parser_base.h> 
  #include "msg_utils.h"
  #include "msg_manager.h"
  #include "mv_messages.h"


class MvMsgArray_t;
static MvMsgType_e         loc_get_msg_type(const string &type);
static const MvMsgArray_t &loc_get_msg_arrays(MvMsgType_e type);


/* --------- Classes declaration --------- */

class MvMsgArray_t {
public:
  MvMsgArray_t(int increm=50);
  ~MvMsgArray_t();
public:
  inline int          getNbMsg()    const { return myNbMsg; }
  inline const char **getStrArray() const { return (const char **)myStrArray; }
  void setMsg(int ind,const string &msg);
private:
  int    myNbMsg,myRealSize,myIncrement;
  char **myStrArray;
};

class MvMsgArrayFile_t : public MvParserBase_t {
public:
  MvMsgArrayFile_t(const string &fullname, MvMsgArray_t *msg_array_tab);
private:
  void readMessages(MvMsgArray_t *msg_array_p);
};

typedef vector<string> MvNameTab_t;

class MvMsgArrayTable_t {
public:
  MvMsgArrayTable_t(const MvNameTab_t &fullname_tab);
public:
  inline const MvMsgArray_t &operator[](MvMsgType_e type) const { return myMsgArrayTable[type]; }
private:
  MvMsgArray_t myMsgArrayTable[MSGT_LAST];
};


/* --------- static functions --------- */

static const MvMsgArray_t &loc_get_msg_arrays(MvMsgType_e type) {
    static bool        a_first=true;
    static MvNameTab_t a_name_tab; 
    //
    if(a_first) {
      a_first=false;
      //
      string a_config_dir=getenv("HM_MSG_DIR");
#if defined WIN32 || _WIN32 
      a_config_dir+="\\CONFIG\\";
#else
      a_config_dir+="/CONFIG/";
#endif
      //
      a_name_tab.push_back(a_config_dir+"msg_arrays.cfg");
      a_name_tab.push_back(a_config_dir+"msg_table.cfg");
    }
    //
    static MvMsgArrayTable_t MV_MSG_ARRAYS(a_name_tab); 
    return MV_MSG_ARRAYS[type];
}


/* --------- public functions implementation --------- */

extern "C" const char **MV_get_msg_array(MvMsgType_e type) {
  return loc_get_msg_arrays(type).getStrArray();
}

extern "C" int MV_get_nb_msg(MvMsgType_e type) {
  return loc_get_msg_arrays(type).getNbMsg();
}


/* --------- MvMsgArray_t implementation --------- */

MvMsgArray_t::MvMsgArray_t(int increm) :
  myNbMsg(0),
  myRealSize(increm),
  myIncrement(increm),
  myStrArray((char **)mycalloc(increm,sizeof(char *)))
{}

MvMsgArray_t::~MvMsgArray_t() {
  for(int i=0;i<myNbMsg;i++) myfree(myStrArray[i]);
  myfree(myStrArray);
}

void MvMsgArray_t::setMsg(int ind,const string &msg) {
  if(ind>=myRealSize) {
    int a_nb_increm=1+(ind-myRealSize)/myIncrement;
    int a_old_size=myRealSize;
    myRealSize+=a_nb_increm*myIncrement;
    myStrArray=(char **)myrealloc(myStrArray,myRealSize*sizeof(char *));
    for(int i=a_old_size;i<myRealSize;++i) myStrArray[i]=NULL;
  }
  myStrArray[ind]=mystrcpy(myStrArray[ind],msg.c_str());
  if(ind>=myNbMsg) myNbMsg=ind+1;
}


/* --------- MvMsgArrayFile_t implementation --------- */
MvMsgArrayFile_t::MvMsgArrayFile_t(const string &fullname, MvMsgArray_t *msg_array_tab) : MvParserBase_t(fullname,false,true) {
  string a_keyword;
  
  //try { getNextChar(); unreadChar(); } catch(MvError_t &) {}
  
  while(!seof(false)) { 
    a_keyword=getNextString();
    MvMsgType_e a_msg_type=loc_get_msg_type(a_keyword);
    if(a_msg_type==MSGT_UNKNOWN) {
      MvMsgManager_t::DisplayMess(CFG_WARNING,"WARNING: \"%s\" is not a valid message keyword\n",a_keyword.c_str());
    }
    MvMsgArray_t *a_msg_array_p=(a_msg_type==MSGT_UNKNOWN ? NULL : msg_array_tab+a_msg_type);
    readMessages(a_msg_array_p);

/*    try { getNextChar(); unreadChar(); } catch(MvError_t &) {}*/

  }  
}

void MvMsgArrayFile_t::readMessages(MvMsgArray_t *msg_array_p) {
  if(getNextChar()!='{') throwError("missing \'{\'");
  while(getNextChar()!='}') {
    unreadChar();
    int    a_index  = getNextInt();
    string a_string = getNextQuotedString();
    if(msg_array_p!=NULL) msg_array_p->setMsg(a_index,a_string);
  }    
}

typedef map<string,MvMsgType_e> LocMapStringMsg_t; // BR 16/01/2003
static MvMsgType_e loc_get_msg_type(const string &type) {
  static bool                    a_first=true;
  static map<string,MvMsgType_e> a_type_map;
  //
  if(a_first) {
	a_first = false;
	a_type_map["UTILS"]             = MSGT_UTILS;  
	a_type_map["KERNEL"]            = MSGT_KERNEL;
	a_type_map["MODEL"]             = MSGT_MODEL;
	a_type_map["MODEL_IO"]          = MSGT_MODEL_IO;         
	a_type_map["READ_D00_5X"]       = MSGT_READ_D00_5X;      
	a_type_map["READ_D00_4X"]       = MSGT_READ_D00_4X;
	a_type_map["READ_NASTRAN"]      = MSGT_READ_NASTRAN;
	a_type_map["READ_UNV"]          = MSGT_READ_UNV;
	a_type_map["WRITE_D00_5X"]      = MSGT_WRITE_D00_5X;     
	a_type_map["WRITE_D00_4XB"]     = MSGT_WRITE_D00_4XB;
	a_type_map["WRITE_D00_41F"]     = MSGT_WRITE_D00_41F;
	a_type_map["WRITE_NASTRAN"]     = MSGT_WRITE_NASTRAN;
	a_type_map["WRITE_UNV"]         = MSGT_WRITE_UNV;
	a_type_map["ACTION"]            = MSGT_ACTION;
	a_type_map["BATCH"]             = MSGT_BATCH;
	a_type_map["DATA_BASE"]         = MSGT_DATA_BASE;
	a_type_map["CHECK"]             = MSGT_CHECK;
	a_type_map["REMOVE"]            = MSGT_REMOVE;
	 
	a_type_map["SUBMODEL2MODEL"]    = MSGT_SUBMODEL2MODEL;
	 
	a_type_map["APP"]               = MSGT_APP;
	a_type_map["MPOST"]             = MSGT_MPOST;             
	a_type_map["READ_D00_INFO"]     = MSGT_READ_D00_INFO;
	a_type_map["READ_D00_ERROR"]    = MSGT_READ_D00_ERROR;
	a_type_map["READ_D00_WARNING"]  = MSGT_READ_D00_WARNING;
	a_type_map["WRITE_D00_INFO"]    = MSGT_WRITE_D00_INFO;
	a_type_map["WRITE_D00_ERROR"]   = MSGT_WRITE_D00_ERROR;
	a_type_map["BOLTMAKE_INFO"]     = MSGT_BOLTMAKE_INFO;
	a_type_map["BOLTMAKE_ERROR"]    = MSGT_BOLTMAKE_ERROR;
	a_type_map["BOLTMAKE_WARNING"]  = MSGT_BOLTMAKE_WARNING;
	a_type_map["BOTTOM_ERROR"]      = MSGT_BOTTOM_ERROR;
	a_type_map["BOUND_INFO"]        = MSGT_BOUND_INFO;
	a_type_map["BOUND_ERROR"]       = MSGT_BOUND_ERROR;
	a_type_map["BOUND_WARNING"]     = MSGT_BOUND_WARNING;
	a_type_map["CALLSAFETY_INFO"]   = MSGT_CALLSAFETY_INFO;
	a_type_map["CLOAD_INFO"]        = MSGT_CLOAD_INFO;
	a_type_map["CLOAD_ERROR"]       = MSGT_CLOAD_ERROR;
	a_type_map["CLOAD_WARNING"]     = MSGT_CLOAD_WARNING;
	a_type_map["CONNEC_INFO"]       = MSGT_CONNEC_INFO;
	a_type_map["CONNEC_ERROR"]      = MSGT_CONNEC_ERROR;
	a_type_map["CONNEC_WARNING"]    = MSGT_CONNEC_WARNING;
	a_type_map["CONTACT_INFO"]      = MSGT_CONTACT_INFO;
	a_type_map["CONTACT_ERROR"]     = MSGT_CONTACT_ERROR;
/* PM:0032:11/03/2004 */
	a_type_map["MONIVOL_INFO"]      = MSGT_MONIVOL_INFO;
	a_type_map["DISCONNE_INFO"]     = MSGT_DISCONNE_INFO;
	a_type_map["DISPIMP_INFO"]      = MSGT_DISPIMP_INFO;
	a_type_map["DISPIMP_ERROR"]     = MSGT_DISPIMP_ERROR;
	a_type_map["DISPIMP_WARNING"]   = MSGT_DISPIMP_WARNING;
	a_type_map["DISPLAY_INFO"]      = MSGT_DISPLAY_INFO;
	a_type_map["DISPLAY_ERROR"]     = MSGT_DISPLAY_ERROR;
	a_type_map["MECASPOT_ERROR"]    = MSGT_MECASPOT_ERROR;
        a_type_map["ELEMENTARY_TIMESTEP"]= MSGT_ELEMENTARY_TIMESTEP; /* PM:0134:28/02/2005 */
	a_type_map["EXTRACT_INFO"]      = MSGT_EXTRACT_INFO;
	a_type_map["EXTRACT_ERROR"]     = MSGT_EXTRACT_ERROR;
	a_type_map["EXTRACT_WARNING"]   = MSGT_EXTRACT_WARNING;
	a_type_map["GUI_OGL_ERROR"]     = MSGT_GUI_OGL_ERROR;
	a_type_map["GENERAL_INFO"]      = MSGT_GENERAL_INFO;
	a_type_map["GENERAL_ERROR"]     = MSGT_GENERAL_ERROR;
	a_type_map["GENERAL_WARNING"]   = MSGT_GENERAL_WARNING;
	a_type_map["GLUEMAKE_INFO"]     = MSGT_GLUEMAKE_INFO;
	a_type_map["GLUEMAKE_ERROR"]    = MSGT_GLUEMAKE_ERROR;
	a_type_map["GLUEMAKE_WARNING"]  = MSGT_GLUEMAKE_WARNING;
	a_type_map["GRAVMAKE_INFO"]     = MSGT_GRAVMAKE_INFO;
	a_type_map["GRAVMAKE_ERROR"]    = MSGT_GRAVMAKE_ERROR;
	a_type_map["GRAVMAKE_WARNING"]  = MSGT_GRAVMAKE_WARNING;
	a_type_map["GTKGLAREA_ERROR"]   = MSGT_GTKGLAREA_ERROR;
	a_type_map["HCMASS_LABEL"]      = MSGT_HCMASS_LABEL;   
	a_type_map["HCMASS_WARNING"]    = MSGT_HCMASS_WARNING;
	a_type_map["HCMASS_ERROR"]      = MSGT_HCMASS_ERROR;
    a_type_map["HCTRANSFORMMANAGER_LABEL"]   = MSGT_HCTRANSFORMMANAGER_LABEL;   
    a_type_map["HCTRANSFORMMANAGER_MESSAGE"] = MSGT_HCTRANSFORMMANAGER_MESSAGE; 
    a_type_map["HCTRANSFORMMANAGER_ERROR"]   = MSGT_HCTRANSFORMMANAGER_ERROR;   
	a_type_map["HELP_INFO"]         = MSGT_HELP_INFO;
	a_type_map["HEMMAKE_INFO"]      = MSGT_HEMMAKE_INFO;
	a_type_map["HEMMAKE_ERROR"]     = MSGT_HEMMAKE_ERROR;
	a_type_map["HEMMAKE_WARNING"]   = MSGT_HEMMAKE_WARNING;
    a_type_map["HMSCRIPT_LABEL"]   = MSGT_HMSCRIPT_LABEL;
    a_type_map["HMSCRIPT_MESSAGE"] = MSGT_HMSCRIPT_MESSAGE;
    a_type_map["HMSCRIPT_ERROR"]   = MSGT_HMSCRIPT_ERROR;
	a_type_map["MAINWINDOW_ERROR"]  = MSGT_MAINWINDOW_ERROR;
	a_type_map["MASSDIST_INFO"]     = MSGT_MASSDIST_INFO;
	a_type_map["MASSDIST_ERROR"]    = MSGT_MASSDIST_ERROR;
	a_type_map["MASSDIST_WARNING"]  = MSGT_MASSDIST_WARNING;
	a_type_map["MATGEN_WARNING"]    = MSGT_MATGEN_WARNING;
	a_type_map["MERGENAS_INFO"]     = MSGT_MERGENAS_INFO;
	a_type_map["MERGENAS_ERROR"]    = MSGT_MERGENAS_ERROR;
	a_type_map["MESH2D_ERROR"]      = MSGT_MESH2D_ERROR;
	a_type_map["MESHGEN_INFO"]      = MSGT_MESHGEN_INFO;
	a_type_map["MESHGEN_ERROR"]     = MSGT_MESHGEN_ERROR;
	a_type_map["MESHGEN_WARNING"]   = MSGT_MESHGEN_WARNING;
	a_type_map["MESSAGEWIN_INFO"]   = MSGT_MESSAGEWIN_INFO;
	a_type_map["MODIFYELEM_INFO"]   = MSGT_MODIFYELEM_INFO;
	a_type_map["MODIFYELEM_ERROR"]  = MSGT_MODIFYELEM_ERROR;
	a_type_map["MODULE_INFO"]       = MSGT_MODULE_INFO;
	a_type_map["MOVE_INFO"]         = MSGT_MOVE_INFO;
	a_type_map["MOVE_ERROR"]        = MSGT_MOVE_ERROR;
	a_type_map["MOVE_WARNING"]      = MSGT_MOVE_WARNING;

	a_type_map["NEWAUTOGEN_ERROR"]  = MSGT_NEWAUTOGEN_ERROR;

	a_type_map["NEWBARGEN_INFO"]    = MSGT_NEWBARGEN_INFO;
	a_type_map["NEWBARGEN_ERROR"]   = MSGT_NEWBARGEN_ERROR;
	a_type_map["NEWMASSGEN_INFO"]   = MSGT_NEWMASSGEN_INFO;
	a_type_map["NEWMASSGEN_ERROR"]  = MSGT_NEWMASSGEN_ERROR;
	a_type_map["NEWMATGEN_INFO"]    = MSGT_NEWMATGEN_INFO;
	a_type_map["NEWMATGEN_ERROR"]   = MSGT_NEWMATGEN_ERROR;
	a_type_map["NEWMATGEN_WARNING"] = MSGT_NEWMATGEN_WARNING;
    /* PM:0067:22/07/2004 */
	a_type_map["MATPROP_INFO"]      = MSGT_MATPROP_INFO;
	a_type_map["MATPROP_ERROR"]     = MSGT_MATPROP_ERROR;
	a_type_map["MATPROP_WARNING"]   = MSGT_MATPROP_WARNING;
    /* END PM:0067 */
	a_type_map["NEWPROPGEN_INFO"]    = MSGT_NEWPROPGEN_INFO;
	a_type_map["NEWPROPGEN_ERROR"]   = MSGT_NEWPROPGEN_ERROR;
	a_type_map["NEWPROPGEN_WARNING"] = MSGT_NEWPROPGEN_WARNING;
	a_type_map["NEWMERGE_INFO"]     = MSGT_NEWMERGE_INFO;
	a_type_map["NEWMERGE_ERROR"]    = MSGT_NEWMERGE_ERROR;
	a_type_map["NEWPOSTPRO_INFO"]   = MSGT_NEWPOSTPRO_INFO;
	a_type_map["NEWPOSTPRO_ERROR"]  = MSGT_NEWPOSTPRO_ERROR;
	a_type_map["NEWPOSTPRO_WARNING"]= MSGT_NEWPOSTPRO_WARNING;
	a_type_map["NEWSPOT_INFO"]      = MSGT_NEWSPOT_INFO;
	a_type_map["NEWSPOT_ERROR"]     = MSGT_NEWSPOT_ERROR;
  a_type_map["OBJECTCFG_INFO"]     = MSGT_OBJECTCFG_INFO; /* PM:0224:09/12/2005 */
	a_type_map["PURGE_INFO"]        = MSGT_PURGE_INFO;
	a_type_map["RADCHECK_INFO"]     = MSGT_RADCHECK_INFO;
	a_type_map["RADCHECK_ERROR"]    = MSGT_RADCHECK_ERROR;
	a_type_map["RADCHECK_WARNING"]  = MSGT_RADCHECK_WARNING;
	a_type_map["RBODYRIVET_INFO"]   = MSGT_RBODYRIVET_INFO;
	a_type_map["RBODYRIVET_ERROR"]  = MSGT_RBODYRIVET_ERROR;
	a_type_map["RBODYRIVET_WARNING"]= MSGT_RBODYRIVET_WARNING;
	a_type_map["READHONDA_INFO"]    = MSGT_READHONDA_INFO;
	a_type_map["READHONDA_ERROR"]   = MSGT_READHONDA_ERROR;
	a_type_map["READHONDA_WARNING"] = MSGT_READHONDA_WARNING;
	a_type_map["READ_M00_ERROR"]    = MSGT_READ_M00_ERROR;
	a_type_map["READNASM00_INFO"]   = MSGT_READNASM00_INFO;
	a_type_map["READNASM00_WARNING"]= MSGT_READNASM00_WARNING;
	a_type_map["READUNV_INFO"]      = MSGT_READUNV_INFO;
	a_type_map["READUNV_ERROR"]     = MSGT_READUNV_ERROR;
	a_type_map["READUNV_WARNING"]   = MSGT_READUNV_WARNING;
	a_type_map["READD2M_INFO"]      = MSGT_READD2M_INFO;
	a_type_map["READD2M_ERROR"]     = MSGT_READD2M_ERROR;
	a_type_map["READD2M_WARNING"]   = MSGT_READD2M_WARNING;
	a_type_map["READP2M_INFO"]      = MSGT_READP2M_INFO;
	a_type_map["REN2MECA_ERROR"]    = MSGT_REN2MECA_ERROR;
	a_type_map["REN2MECA_WARNING"]  = MSGT_REN2MECA_WARNING;
	a_type_map["RENUM_WARNING"]     = MSGT_RENUM_WARNING;
	a_type_map["REPLACE_INFO"]      = MSGT_REPLACE_INFO;
	a_type_map["REPLACE_ERROR"]     = MSGT_REPLACE_ERROR;
	a_type_map["SIMPLE_INFO"]       = MSGT_SIMPLE_INFO; 
	a_type_map["SIMPLE_ERROR"]      = MSGT_SIMPLE_ERROR;
	a_type_map["SIMPLE_WARNING"]    = MSGT_SIMPLE_WARNING;
	a_type_map["SPOTMAKE_INFO"]     = MSGT_SPOTMAKE_INFO;
	a_type_map["SPOTMAKE_ERROR"]    = MSGT_SPOTMAKE_ERROR;
	a_type_map["SPOTMAKE_WARNING"]  = MSGT_SPOTMAKE_WARNING;
	a_type_map["SUBMODELLING_INFO"] = MSGT_SUBMODELLING_INFO; 
	a_type_map["SYSGEN_INFO"]       = MSGT_SYSGEN_INFO;
	a_type_map["SYSGEN_ERROR"]      = MSGT_SYSGEN_ERROR;
	a_type_map["SYSGEN_WARNING"]    = MSGT_SYSGEN_WARNING;
	a_type_map["TREE_INFO"]         = MSGT_TREE_INFO;
	a_type_map["TREE_ERROR"]        = MSGT_TREE_ERROR;
	a_type_map["TREEMODEL_LABEL"]   = MSGT_TREEMODEL_LABEL;  
	a_type_map["TREEMODEL_POPUP"]   = MSGT_TREEMODEL_POPUP;   
    a_type_map["TREEMODEL_MESSAGE"] = MSGT_TREEMODEL_MESSAGE;  
	a_type_map["UTILITY_ERROR"]     = MSGT_UTILITY_ERROR;
	a_type_map["UTILITY_WARNING"]   = MSGT_UTILITY_WARNING;
	a_type_map["UTILITYGUI_INFO"]   = MSGT_UTILITYGUI_INFO;
	a_type_map["UTILITYGUI_ERROR"]  = MSGT_UTILITYGUI_ERROR;
	a_type_map["UTILITYGUI_WARNING"]= MSGT_UTILITYGUI_WARNING;
	a_type_map["VECTOR_ERROR"]      = MSGT_VECTOR_ERROR;
	a_type_map["VELIMP_INFO"]       = MSGT_VELIMP_INFO;
	a_type_map["VELIMP_ERROR"]      = MSGT_VELIMP_ERROR;
	a_type_map["VELIMP_WARNING"]    = MSGT_VELIMP_WARNING;
	a_type_map["VELOCITY_INFO"]     = MSGT_VELOCITY_INFO;
	a_type_map["VELOCITY_ERROR"]    = MSGT_VELOCITY_ERROR;
	a_type_map["WELDMAKE_INFO"]     = MSGT_WELDMAKE_INFO;
	a_type_map["WELDMAKE_ERROR"]    = MSGT_WELDMAKE_ERROR;
	a_type_map["WELDMAKE_WARNING"]  = MSGT_WELDMAKE_WARNING;
	a_type_map["WRITE_D01_ERROR"]   = MSGT_WRITE_D01_ERROR;
	a_type_map["WRITE_D01_WARNING"] = MSGT_WRITE_D01_WARNING;
	a_type_map["WRITENAS_INFO"]     = MSGT_WRITENAS_INFO;
	a_type_map["WRITENAS_WARNING"]  = MSGT_WRITENAS_WARNING;
	a_type_map["WRITEPAM_INFO"]     = MSGT_WRITEPAM_INFO;
	a_type_map["WRITEPAM_ERROR"]    = MSGT_WRITEPAM_ERROR;
	a_type_map["WRITEPAM_WARNING"]  = MSGT_WRITEPAM_WARNING;
	a_type_map["WRITE41F_INFO"]     = MSGT_WRITE41F_INFO;
	a_type_map["WRITE41F_ERROR"]    = MSGT_WRITE41F_ERROR;
	a_type_map["WRITE41F_WARNING"]  = MSGT_WRITE41F_WARNING;
	a_type_map["WRITEUNV_INFO"]     = MSGT_WRITEUNV_INFO;
	a_type_map["WRITEUNV_WARNING"]  = MSGT_WRITEUNV_WARNING;
	a_type_map["WRITEM2D_INFO"]     = MSGT_WRITEM2D_INFO;
	a_type_map["WRITEM2D_ERROR"]    = MSGT_WRITEM2D_ERROR;
	a_type_map["WRITEM2D_WARNING"]  = MSGT_WRITEM2D_WARNING;
	
	a_type_map["READ_M00"]          = MSGT_READ_M00;
	
	
	a_type_map["WRITE_M00_ERROR"]   = MSGT_WRITE_M00_ERROR;
	
	
	a_type_map["PARSER"]            = MSGT_PARSER;
	a_type_map["MUNITS"]            = MSGT_MUNITS;
	
    a_type_map["GENEDIT_INFO"]      = MSGT_GENEDIT_INFO;
    a_type_map["VERTICALPANEL_INFO"] = MSGT_VERTICALPANEL_INFO;
    a_type_map["GLOBAL"]             = MSGT_GLOBAL;
   }
  //
  LocMapStringMsg_t::iterator a_it=a_type_map.find(type);
  if(a_it==a_type_map.end()) return MSGT_UNKNOWN;
  return (*a_it).second;
}

  
/* --------- MvMsgArrayTable_t imlementation --------- */

MvMsgArrayTable_t::MvMsgArrayTable_t(const MvNameTab_t &fullname_tab) {
    MvNameTab_t::const_iterator a_it_begin = fullname_tab.begin();
    MvNameTab_t::const_iterator a_it_end   = fullname_tab.end();
    MvNameTab_t::const_iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
	MvMsgArrayFile_t a_array_file(*a_it, myMsgArrayTable);
    }
}




