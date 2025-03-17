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
#include <UTILS/set_utils.h>
#include <UTILS/mv_cstring.h>
#include <KERNEL/mv_type.h>
#include <KERNEL/mv_utils.h>
#include <KERNEL/mv_model_descriptors.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include "mv_solver_input_infos.h"
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <HCDI/hcdi_utils.h>


bool stringContainsSubstring(const string& str, const string& substr) {
    return str.find(substr) != string::npos;
}
// Function to get a substring and trim trailing spaces or stop at first space when n == 0
std::string get_trimmed_substr(const std::string& str, size_t n) {
    if (n == 0) {
        // Find last space or newline
        size_t end = str.find_last_of(" \t\n\r"); 
        // Trim at the first blank
        return (end == std::string::npos) ? str : str.substr(0, end); 
    }
    else {
        size_t len = (n > str.size()) ? str.size() : n; 
        size_t end = str.find_last_not_of(" \t\n\r", len - 1);
        return (end == std::string::npos) ? "" : str.substr(0, end + 1);
    }
}

static bool isDigitString(const char* test_str)
{
    // test whole string is a number or not, not like SH3N, TETRA4..
    // e.g.return true for 1,2,...
    int length = (int)strlen(test_str);
    int count = 0;
    for (int i = 0; i < length; i++)
    {
        if (isdigit(test_str[i]))
        {
            count++;
        }
        else
            break;
    }
    if (count == length)
        return true;
    return false;
}

static void trimTrailingSpaces(char* str) {
    int len = (int)std::strlen(str);
    while (len > 0 && (str[len - 1] == ' ' || str[len - 1] == '\t' ||
        str[len - 1] == '\n' || str[len - 1] == '\r' ||
        str[len - 1] == '\f' || str[len - 1] == '\v')) {
        len--;
    }
    str[len] = '\0';
}



struct compare {
    inline bool operator()(const StringTypeInfoPair& first,
        const StringTypeInfoPair& second) const
    {
        return first.first < second.first;
    }
};

bool eraseLastNumericParts(std::string& header, char del) 
{
    bool found = false;
    size_t pos = header.rfind(del);

    while (pos != std::string::npos)
    {
        // Check if the substring after the delimiter is numeric
        std::string lastPart = header.substr(pos + 1);
        if (std::all_of(lastPart.begin(), lastPart.end(), ::isdigit))
        {
            // Remove the last numeric part and update position
            header.erase(pos, lastPart.length() + 1);
            pos = header.rfind(del, pos - 1); // Search from the previous delimiter position
            found = true;
        }
        else
        {
            // Non-numeric character found, stop searching
            break;
        }
    }
    // Return the modified string
    return found;
}

static int GetHMNAMEEntityNameSize(obj_type_e& etype, const char* line, int* id_format)
{
    if (!line)
        return 0;
    if (!strncmp(line, "COMPS", 5))
    {
        etype = HCDI_OBJ_TYPE_COMPS;
        return 5;
    }
    else if (!strncmp(line, "LOADCOLS", 8))
    {
        etype = HCDI_OBJ_TYPE_LOADCOLS;
        return 8;
    }
    else if (!strncmp(line, "SYSCOLS", 7))
    {
        etype = HCDI_OBJ_TYPE_SYSTCOLS;
        return 7;
    }
    else if (!strncmp(line, "SETS", 4))
    {
        etype = HCDI_OBJ_TYPE_SETS;
        return 4;
    }
    else if (!strncmp(line, "PROPS", 5))
    {
        etype = HCDI_OBJ_TYPE_PROPS;
        return 5;
    }
    else if (!strncmp(line, "GROUPS", 6))
    {
        etype = HCDI_OBJ_TYPE_GROUPS;
        return 6;
    }
    else if (!strncmp(line, "CURVES", 6))
    {
        etype = HCDI_OBJ_TYPE_CURVES;
        return 6;
    }
    else if (!strncmp(line, "MATS", 4))
    {
        etype = HCDI_OBJ_TYPE_MATS;
        return 4;
    }
    else if (!strncmp(line, "PROPERTIES", 10))
    {
        *id_format = 10;
        etype = HCDI_OBJ_TYPE_PROPS;
        return 10;
    }
    else if (!strncmp(line, "PROPERTY", 8))
    {
        *id_format = 10;
        etype = HCDI_OBJ_TYPE_PROPS;
        return 8;
    }
    else if (!strncmp(line, "MATERIALS", 9))
    {
        *id_format = 10;
        etype = HCDI_OBJ_TYPE_MATS;
        return 9;
    }
    else if (!strncmp(line, "MATERIALBEHAVIOR", 16))
    {
        *id_format = 10;
        etype = HCDI_OBJ_TYPE_MATERIALBEHAVIOR;
        return 16;
    }
    else if (!strncmp(line, "MATERIAL", 8))
    {
        *id_format = 10;
        etype = HCDI_OBJ_TYPE_MATS;
        return 8;
    }
    else if (!strncmp(line, "ASSEMS", 6))
    {
        etype = HCDI_OBJ_TYPE_ASSEMS;
        return 6;
    }
    else if (!strncmp(line, "VECTORCOL", 9))
    {
        etype = HCDI_OBJ_TYPE_VECTORCOLS;
        return 9;
    }
    else if (!strncmp(line, "OUTPUTBLOCKS", 12))
    {
        etype = HCDI_OBJ_TYPE_OUTPUTBLOCKS;
        return 12;
    }
    else if (!strncmp(line, "SENSOR", 6))
    {
        etype = HCDI_OBJ_TYPE_SENSORS;
        return 6;
    }
    else if (!strncmp(line, "CONTROLVOLS", 11))
    {
        etype = HCDI_OBJ_TYPE_CONTROLVOLS;
        return 11;
    }
    else if (!strncmp(line, "TAG", 3))
    {
        etype = HCDI_OBJ_TYPE_TAG;
        return 3;
    }
    else if (!strncmp(line, "HMASSEM", 7))
    {
        etype = HCDI_OBJ_TYPE_ASSEMS;
        return 7;
    }
    else if (!strncmp(line, "HMREFSYSTEM", 11))
    {
        return 11;
    }
    else if (!strncmp(line, "CONTROLVOL", 10))
    {
        etype = HCDI_OBJ_TYPE_CONTROLVOLS;
        return 10;
    }
    else if (!strncmp(line, "BLOCKS", 6))
    {
        etype = HCDI_OBJ_TYPE_BLOCKS;
        return 6;
    }
    else if (!strncmp(line, "CSURFS", 6))
    {
        etype = HCDI_OBJ_TYPE_CONTACTSURFS;
        return 6;
    }
    else if (!strncmp(line, "BEAMSECTCOLS", 12))
    {
        etype = HCDI_OBJ_TYPE_BEAMSECTCOLS;
        return 12;
    }
    else if (!strncmp(line, "BEAMSECTS", 9))
    {
        etype = HCDI_OBJ_TYPE_BEAMSECTS;
        return 9;
    }
    else if (!strncmp(line, "PARAMETER", 9))
    {
        etype = HCDI_OBJ_TYPE_PARAMETERS;
        return 9;
    }
    else if (!strncmp(line, "CROSSSECTIONS", 13))
    {
        etype = HCDI_OBJ_TYPE_CROSSSECTIONS;
        return 13;
    }
    else if (!strncmp(line, "CONSTRAINEDEXTRANODE", 20))
    {
        etype = HCDI_OBJ_TYPE_CONSTRAINEDEXTRANODES;
        return 20;
    }
    else if (!strncmp(line, "CONSTRAINEDRIGIDBODY", 20))
    {
        etype = HCDI_OBJ_TYPE_CONSTRAINEDRIGIDBODIES;
        return 20;
    }
    else if (!strncmp(line, "ACCELEROMETER", 13))
    {
        etype = HCDI_OBJ_TYPE_ACCELEROMETERS;
        return 13;
    }
    else if (!strncmp(line, "RIGIDWALLS", 10))
    {
        etype = HCDI_OBJ_TYPE_RIGIDWALLS;
        return 10;
    }
    else if (!strncmp(line, "HOURGLASS", 9))
    {
        etype = HCDI_OBJ_TYPE_HOURGLASS;
        return 9;
    }
    else if (!strncmp(line, "PRETENSIONER", 12))
    {
        etype = HCDI_OBJ_TYPE_PRETENSIONERS;
        return 12;
    }
    else if (!strncmp(line, "RETRACTOR", 9))
    {
        etype = HCDI_OBJ_TYPE_RETRACTORS;
        return 9;
    }
    else if (!strncmp(line, "SLIPRING", 8))
    {
        etype = HCDI_OBJ_TYPE_SLIPRINGS;
        return 8;
    }
    else if (!strncmp(line, "ALESMOOTHINGS", 13))
    {
        etype = HCDI_OBJ_TYPE_ALESMOOTHINGS;
        return 13;
    }
    else if (!strncmp(line, "INTERFACECOMPONENT", 18))
    {
        etype = HCDI_OBJ_TYPE_INTERFACECOMPONENTS;
        return 18;
    }
    else if (!strncmp(line, "ALETANKTEST", 11))
    {
        etype = HCDI_OBJ_TYPE_ALETANKTESTS;
        return 11;
    }
    else if (!strncmp(line, "ALEFSIPROJECTION", 16))
    {
        etype = HCDI_OBJ_TYPE_ALEFSIPROJECTIONS;
        return 16;
    }
    else if (!strncmp(line, "ALEREFERENCESYSTEMNODE", 22))
    {
        etype = HCDI_OBJ_TYPE_ALEREFERENCESYSTEMNODES;
        return 22;
    }
    else if (!strncmp(line, "ALEREFERENCESYSTEMCURVE", 23))
    {
        etype = HCDI_OBJ_TYPE_ALEREFERENCESYSTEMCURVES;
        return 23;
    }
    else if (!strncmp(line, "ALEREFERENCESYSTEMSWITCH", 24))
    {
        etype = HCDI_OBJ_TYPE_ALEREFERENCESYSTEMSWITCHES;
        return 24;
    }
    else if (!strncmp(line, "ALEREFERENCESYSTEMGROUP", 23))
    {
        etype = HCDI_OBJ_TYPE_ALEREFERENCESYSTEMGROUPS;
        return 23;
    }

    vector< std::pair<obj_type_e, string> > vec_type_title;
    HCDIGetAllTypesTitle(vec_type_title);

    vector< std::pair<obj_type_e, string> >::const_iterator it_begin = vec_type_title.begin();
    vector< std::pair<obj_type_e, string> >::const_iterator it_end = vec_type_title.end();
    vector< std::pair<obj_type_e, string> >::const_iterator it;

    *id_format = 10;
    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e obj_type = (*it).first;
        string a_title = (*it).second;

        if (!strncmp(line, a_title.c_str(), a_title.length()))
        {
            etype = obj_type;
            return (int)(a_title.length());
        }

    }
    return 0;
}


SolverInputInfo::SolverInputInfo() :InputInfos()
{
    const CFGKernel* a_cfg_kernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if(nullptr != a_cfg_kernel) myApplicationMode = a_cfg_kernel->getUserProfile();
    else                        myApplicationMode = HCDI_SOLVER_NONE; // shouldn't happen
    myVersion = MultiCFGKernelMgr::getInstance().GetActiveUserProfile();

    initSolverSpecialKeywords();

    //int max_bit = 0;
    //myCAN_HAVE_TITLE_CARD = HCDI_get_data_hierarchy_bitmask(string("CAN_HAVE_TITLE_CARD"));
    //myHAS_TITLE_CARD = HCDI_get_data_hierarchy_bitmask(string("HAS_TITLE_CARD"));
    //myHAS_ID_CARD = HCDI_get_data_hierarchy_bitmask(string("HAS_ID_CARD"));
    //myHAS_MPP_CARD = HCDI_get_data_hierarchy_bitmask(string("HAS_MPP_CARD"));
    //myHAS_ID_IN_FIRST_DATA_CARD = HCDI_get_data_hierarchy_bitmask(string("HAS_ID_IN_FIRST_DATA_CARD"));
    //myKEYWORD_HAS_ID = HCDI_get_data_hierarchy_bitmask(string("KEYWORD_HAS_ID"));
    //mySECTION_KWD_HAS_SET = HCDI_get_data_hierarchy_bitmask(string("SECTION_KWD_HAS_SET"));
    //mySECTION_KWD_HAS_PLANE = HCDI_get_data_hierarchy_bitmask(string("SECTION_KWD_HAS_PLANE"));
    //myHAS_SPC_CARD = HCDI_get_data_hierarchy_bitmask(string("HAS_SPC_CARD"));
    //myHAS_INERTIA_CARD = HCDI_get_data_hierarchy_bitmask(string("HAS_INERTIA_CARD"));
    //myCAN_HAVE_ID_CARD = HCDI_get_data_hierarchy_bitmask(string("CAN_HAVE_ID_CARD"));
    //int myI10_FORMAT = HCDI_get_data_hierarchy_bitmask(string("I10_FORMAT"));
    //max_bit = HCDIGetMaxProfileBit();
    //if (myCAN_HAVE_TITLE_CARD == 0)
    //{
    //    myCAN_HAVE_TITLE_CARD = max_bit << 1;
    //    max_bit = myCAN_HAVE_TITLE_CARD;
    //}
    //if (myHAS_TITLE_CARD == 0)
    //{
    //    myHAS_TITLE_CARD = max_bit << 1;
    //    max_bit = myHAS_TITLE_CARD;
    //}
    //if (myHAS_ID_CARD == 0)
    //{
    //    myHAS_ID_CARD = max_bit << 1;
    //    max_bit = myHAS_ID_CARD;
    //}
    //if (myHAS_MPP_CARD == 0)
    //{
    //    myHAS_MPP_CARD = max_bit << 1;
    //    max_bit = myHAS_MPP_CARD;
    //}
    //if (myHAS_ID_IN_FIRST_DATA_CARD == 0)
    //{
    //    myHAS_ID_IN_FIRST_DATA_CARD = max_bit << 1;
    //    max_bit = myHAS_ID_IN_FIRST_DATA_CARD;
    //}
    //if (myKEYWORD_HAS_ID == 0)
    //{
    //    myKEYWORD_HAS_ID = max_bit << 1;
    //    max_bit = myKEYWORD_HAS_ID;
    //}
    //if (mySECTION_KWD_HAS_SET == 0)
    //{
    //    mySECTION_KWD_HAS_SET = max_bit << 1;
    //    max_bit = mySECTION_KWD_HAS_SET;
    //}
    //if (mySECTION_KWD_HAS_PLANE == 0)
    //{
    //    mySECTION_KWD_HAS_PLANE = max_bit << 1;
    //    max_bit = mySECTION_KWD_HAS_PLANE;
    //}
    //if (myHAS_SPC_CARD == 0)
    //{
    //    myHAS_SPC_CARD = max_bit << 1;
    //    max_bit = myHAS_SPC_CARD;
    //}
    //if (myHAS_INERTIA_CARD == 0)
    //{
    //    myHAS_INERTIA_CARD = max_bit << 1;
    //    max_bit = myHAS_INERTIA_CARD;
    //}
    //if (myCAN_HAVE_ID_CARD == 0)
    //{
    //    myCAN_HAVE_ID_CARD = max_bit << 1;
    //    max_bit = myCAN_HAVE_ID_CARD;
    //}

    if (nullptr != a_cfg_kernel) a_cfg_kernel->getlUserNamesSolverInfo(myobjectsolverinfo, true);//need new api to directly populate in string vs typeinfo
    ApplicationMode_e appmode = (ApplicationMode_e)GetAppMode();
    // Modify myMap in place and merge into a single map
    //   In case we have the same user_name for a "real" type and for a subobject, we want to keep the "real" one.
    //   Therefore we add subobjects first, the "real" one will overwrite if there is one.
    for (auto it = myobjectsolverinfo[HCDI_OBJ_TYPE_SUBOBJECT].begin();
         it != myobjectsolverinfo[HCDI_OBJ_TYPE_SUBOBJECT].end(); ++it) {
        mymapkeywordsolverinfo[it->first] = it->second; // Insert into merged map
    }
    for (unsigned int type = HCDI_OBJ_TYPE_NODES; type < HCDI_OBJ_TYPE_HC_MAX; ++type) {
        auto& singleMap = myobjectsolverinfo[type];
        //if (appmode == HCDI_SOLVER_RADIOSS)
        //{
        //    std::map<std::string, CUserNameTypeInfo> updatedMap;
        //    for (auto it = singleMap.begin(); it != singleMap.end(); ++it) {
        //        std::string newKey = it->first; // Copy the key because map keys are immutable
        //        std::replace(newKey.begin(), newKey.end(), '_', '/');
        //        updatedMap[newKey] = it->second; // Insert into the updated map
        //    }
        //    singleMap = std::move(updatedMap); // Replace the original map with the updated map
        //}
        if(HCDI_OBJ_TYPE_SUBOBJECT == type) continue;
        // Merge into the final merged map
        for (auto it = singleMap.begin(); it != singleMap.end(); ++it) {
            mymapkeywordsolverinfo[it->first] = it->second; // Insert into merged map
        }
    }

    myContinueReadWithoutHeader=false;
    if (appmode == HCDI_SOLVER_LSDYNA)
    {
        myContinueReadWithoutHeader = true;
        myUserNameExactMatch = true;
    }

   // std::map<std::string, std::vector<CUserNameTypeInfo>>  descrete_lst;
   // a_cfg_kernel->getDiscreteKeywordInfoMap(descrete_lst);
}

int SolverInputInfo::getKeywordProcessCommentFlag() const
{ 
    ApplicationMode_e app_mode = (ApplicationMode_e)GetAppMode();
    switch (app_mode)
    {
    case HCDI_SOLVER_RADIOSS:
        return SOLVER_PORCESS_KEYWORD_COMMMENT_NONE;
    case HCDI_SOLVER_LSDYNA:
        return SOLVER_PROCESS_KEYWORD_COMMMENT_AFTER;
    default:
        return SOLVER_PROCESS_KEYWORD_COMMENT_BEFORE_AFTER;
        break;
    }

    return SOLVER_PORCESS_KEYWORD_COMMMENT_NONE;
}

bool SolverInputInfo::IsIncludeFilePathOnSameLine() const /*can come from include cfg file as well*/
{
    ApplicationMode_e app_mode = (ApplicationMode_e)GetAppMode();
    switch (app_mode)
    {
    case HCDI_SOLVER_RADIOSS:
        return true;
    case HCDI_SOLVER_LSDYNA:
        return false;
    default:
        return false;
        break;
    }
    return true;
}


const char* SolverInputInfo::GetSolverSubdeckKeyword() const
{
    return mysubmodelkeyword.c_str();
}
const char* SolverInputInfo::GetSolverSubdeckKeywordEnd() const
{
    return mysubmodelendkeyword.c_str();
}
const char* SolverInputInfo::GetEncryptedKeyword() const
{
    return mykeykeyword.c_str();
}
const char* SolverInputInfo::GetIncludeKeyword() const
{
    return myincludekeyword.c_str();
}
const char* SolverInputInfo::GetIncludeEndKeyword() const
{
    return myincludeendkeyword.c_str();
}
const char* SolverInputInfo::GetBeginKeyword() const
{
    return mybeginkeyword.c_str();
}
/* --------- Header Keywords --------- */
bool SolverInputInfo::IsIncludeHeader(const char* header) const
{
    ApplicationMode_e appmode = (ApplicationMode_e)GetAppMode();
    int len_incl = (int)strlen(GetIncludeKeyword());
    if (len_incl <= 0)
        return false;

    if (appmode == HCDI_SOLVER_LSDYNA)
    {
        int len = (int)strlen(header);
        if (len == len_incl) //should have exact match
            return mystrncasecmp(header, GetIncludeKeyword(), len_incl) == 0;
        else
            return false;
    }
    return (mystrncasecmp(header, GetIncludeKeyword(), len_incl) == 0);
}


void SolverInputInfo::initSolverSpecialKeywords() {
  // Commented as now keyword vs type is fetched from HCDIgetKeywordTypeMap
    ApplicationMode_e appmode = (ApplicationMode_e)GetAppMode();


    /*Need to map from solver_info.cfg file: To Do*/
    switch (appmode)
    {
    case HCDI_SOLVER_LSDYNA:
    {
        mybeginkeyword = "*KEYWORD";
        myendkeyword = "*END";
        mysubmodelkeyword = "*INCLUDE_TRANSFORM";
        mysubmodelendkeyword = "";
        mykeykeyword = "";
        myincludekeyword = "*INCLUDE";
        myincludeendkeyword = "";
    }
    break;
    case HCDI_SOLVER_RADIOSS:
    {
        mybeginkeyword = "/BEGIN";
        myendkeyword = "/END";
        mysubmodelkeyword = "//SUBMODEL";
        mysubmodelendkeyword = "//ENDSUB";
        mykeykeyword = "/KEY";
        myincludekeyword = "#include";
        myincludeendkeyword = "#enddata";
    }
    break;
    default:
        break;
    }
    return;
/*
    int ikey_begin = pdescrp->getIKeyword("_beginkeyword");
    if (ikey_begin > 0)
        mybeginkeyword = pdescrp->getStringDefaultValue(ikey_begin, DOM_COMMON);

    int ikey_end = pdescrp->getIKeyword("_endkeyword");
    if (ikey_end > 0)
        myendkeyword = pdescrp->getStringDefaultValue(ikey_end, DOM_COMMON);

    int ikey_submodel = pdescrp->getIKeyword("_submodelkeyword");
    if (ikey_submodel > 0)
        mysubmodelkeyword = pdescrp->getStringDefaultValue(ikey_submodel, DOM_COMMON);

    int ikey_submodelend = pdescrp->getIKeyword("_submodelendkeyword");
    if (ikey_submodelend > 0)
        mysubmodelendkeyword = pdescrp->getStringDefaultValue(ikey_submodelend, DOM_COMMON);

    int ikey_key = pdescrp->getIKeyword("_keykeyword");
    if(ikey_key)
        mykeykeyword = pdescrp->getStringDefaultValue(ikey_key, DOM_COMMON);

    int ikey_include = pdescrp->getIKeyword("_includekeyword");
    if (ikey_include)
        myincludekeyword = pdescrp->getStringDefaultValue(ikey_include, DOM_COMMON);

    int ikey_includeend = pdescrp->getIKeyword("_includeendkeyword");
    if (ikey_includeend)
        myincludeendkeyword = pdescrp->getStringDefaultValue(ikey_includeend, DOM_COMMON);
*/
  } 

const char* SolverInputInfo::GetNormalizedKeyword(const char* keyword) const {
    MyKeywordSet_t::const_iterator a_it = myAllKeywords.find(keyword);
    //
    if (a_it != myAllKeywords.end()) {
        const string& a_keyword = (*a_it);
        return a_keyword.c_str();
    }

    int         a_length = (int)(strlen(keyword));
    const char* a_normalized_keyword = NULL;
    //
    MyKeywordSet_t::const_iterator a_it_begin = myAllKeywords.begin();
    MyKeywordSet_t::const_iterator a_it_end = myAllKeywords.end();
    for (a_it = a_it_begin; a_it != a_it_end; ++a_it) {
        const string& a_ref_keyword = (*a_it);
        int           a_ref_length = (int)(a_ref_keyword.length());
        //
        if (a_ref_length > a_length) a_ref_length = a_length;
        int a_cmp = strncmp(keyword, a_ref_keyword.c_str(), a_ref_length);
        //
        if (a_cmp == 0) {
            if (a_normalized_keyword != NULL) return NULL;
            a_normalized_keyword = a_ref_keyword.c_str();
        }
        else if (a_cmp < 0) {
            return a_normalized_keyword;
        }
    }
    //
    return NULL;
}

bool SolverInputInfo::IsEofKeyword(const char *keyword) const {

    size_t sz = myendkeyword.size();
    int len = strlen(keyword);
    if (sz <= 0 || sz < len)
        return false;

  return (strncmp(keyword, myendkeyword.c_str(), myendkeyword.size())==0);
}

PseudoObjectKeywords_t *SolverInputInfo::GetElementKeywords(PseudoObjectKeywords_t *elt_keywords_p) const {
  MyObjectKeywords_t *a_elts_keywords_p=(elt_keywords_p==NULL ? new MyObjectKeywords_t() : (MyObjectKeywords_t *)elt_keywords_p);
  //
  MyKeywordSet_t a_element_keywords=myElementKeywords;
  //
  MyKeywordSet_t::iterator a_it_begin = a_element_keywords.begin();
  MyKeywordSet_t::iterator a_it_end   = a_element_keywords.end();
  MyKeywordSet_t::iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    const string &a_keyword=(*a_it);
    a_elts_keywords_p->push_back(MyKeywordOType_t(a_keyword,a_keyword));
  }
  //
  return (PseudoObjectKeywords_t *)a_elts_keywords_p;
}


const PseudoKeywordSet_t *SolverInputInfo::GetElements() const {
  return (PseudoKeywordSet_t *)(&myElementKeywords);
}



const CUserNameTypeInfo* SolverInputInfo::GetKeywordSolverInfoFromHeaderLine(
    const string &header_line, const ISyntaxInfos* pSyntaxInfo,
    set<string>* stripheader, obj_type_e etype, bool isexactmatch) const
{
    ApplicationMode_e appmode = (ApplicationMode_e)GetAppMode();
    std::vector<std::string> a_fields;
    string  a_header_line(header_line);

    bool fromStart = pSyntaxInfo->GetHeaderFromStartFlag();
    char ch = pSyntaxInfo->getHeaderSeparator();
    if(isexactmatch)
        eraseLastNumericParts(a_header_line, ch);

    //vector<string> erased_strings;
    if (stripheader && stripheader->size())
    {
        std::set<string>::iterator itr;
        for (itr = stripheader->begin(); itr != stripheader->end(); itr++)
        {
            size_t pos = a_header_line.find(*itr);
            if (pos != string::npos) {
                a_header_line.erase(a_header_line.begin() + pos, a_header_line.begin() + pos + (*itr).length());
                //erased_strings.push_back(*itr);
            }
        }
    }
    if (etype > HCDI_OBJ_TYPE_NULL && etype < HCDI_OBJ_TYPE_HC_MAX)
    {
        const map< std::string, CUserNameTypeInfo >& a_mapkeysolinfo = myobjectsolverinfo[etype];

        // Check for exact match first
        auto it = a_mapkeysolinfo.lower_bound(a_header_line); // Find the first key >= a_header_line
        if (it != a_mapkeysolinfo.end() && it->first == a_header_line) {
            // Exact match found
            return &(it->second);
        }

        int first_size = 0;
        std::string firstPart;
        if (!myUserNameExactMatch && pSyntaxInfo && pSyntaxInfo->getHeader())
        {
            size_t headerPos = a_header_line.find(pSyntaxInfo->getHeader()[0]);
            if (headerPos != std::string::npos)
            {
                firstPart = a_header_line.substr(0, headerPos);
                first_size = (int)firstPart.length();
            }
        }
        if (first_size > 0)
        {
            // Search for the best prefix using reverse iterator from current position
            for (auto revIt = std::make_reverse_iterator(it); revIt != a_mapkeysolinfo.rend(); ++revIt)
            {
                int sz = (int)revIt->first.size();
                string str = revIt->first;

                if (str.compare(0, firstPart.size(), firstPart) != 0)
                    break;

                if (str.compare(0, first_size, firstPart) == 0)
                {
                    if (a_header_line.compare(0, sz, revIt->first) == 0)
                    {
                        // Longest prefix match found
                        return &(revIt->second);
                    }
                }
            }
        }
    }
    else
    {
        if (pSyntaxInfo->GetHeaderFromStartFlag())
        {
            // Check for exact match first
            //auto it = mymapkeywordsolverinfo.lower_bound(a_header_line); // Find the first key >= a_header_line
            string str_trim = get_trimmed_substr(a_header_line, myheader_size);
            auto it = mymapkeywordsolverinfo.lower_bound(str_trim);
            if (it != mymapkeywordsolverinfo.end() && it->first == str_trim) {
                // Exact match found
                return &(it->second);
            }

            int first_size = 0;
            std::string firstPart;
            if (!myUserNameExactMatch && pSyntaxInfo && pSyntaxInfo->getHeader())
            {
                size_t headerPos = a_header_line.find(pSyntaxInfo->getHeader()[0]);
                if (headerPos != std::string::npos)
                {
                    firstPart = a_header_line.substr(0, headerPos);
                    first_size = (int)firstPart.length();
                }
            }
            if (first_size > 0)
            {
                // Search for the best prefix using reverse iterator from current position
                for (auto revIt = std::make_reverse_iterator(it); revIt != mymapkeywordsolverinfo.rend(); ++revIt)
                {
                    int sz = (int)revIt->first.size();
                    string str = revIt->first;

                    if (str.compare(0, firstPart.size(), firstPart) != 0)
                        break;

                    if (str.compare(0, first_size, firstPart) == 0)
                    {
                        if (a_header_line.compare(0, sz, revIt->first) == 0)
                        {
                            // Longest prefix match found
                            return &(revIt->second);
                        }
                    }
                }
            }
        }
        else
        {
            for (const auto& pair : mymapkeywordsolverinfo) {
                if (stringContainsSubstring(a_header_line, pair.first)) {
                    return &(pair.second);
                }
            }
            return nullptr;
        }
    }
    return nullptr;
}


bool SolverInputInfo::IsSupportedForContinueReadWithoutHeader() const
{
    return myContinueReadWithoutHeader;
}


void SolverInputInfo::ProcessKeywordComments(std::vector<std::vector<string>>& comments,
                                             obj_type_e& etype, IdentifierValuePairList& vallst) const
{
    ApplicationMode_e appmode = (ApplicationMode_e)GetAppMode();
    bool id_read = false;
    int key_comment_process_flag = getKeywordProcessCommentFlag();
    if (key_comment_process_flag > InputInfos::SOLVER_PROCESS_KEYWORD_COMMENT_BEFORE_AFTER ||
        key_comment_process_flag == SOLVER_PORCESS_KEYWORD_COMMMENT_NONE)
        return;

    std::vector<string>& a_commments = comments[key_comment_process_flag];

    int a_size = (int)a_commments.size();
    for (int i = 0; i < a_size; i++)
    {
        const char* line = NULL;
        line = a_commments[i].c_str();

        if (a_commments[i].find("$HC_ID") != string::npos)
        {
            vector <string > result;
            HCDI_splitString(a_commments[i], ";", result);
            int sz = (int)result.size();
            if (sz > 2)
            {
                result[2].erase(result[2].begin(), std::find_if(result[2].begin(), result[2].end(), std::bind1st(std::not_equal_to<char>(), ' ')));
                result[2].erase(std::find_if(result[2].rbegin(), result[2].rend(), std::bind1st(std::not_equal_to<char>(), ' ')).base(), result[2].end());
                //skwy_val.push_back(string("name=") + result[2]);
                cfgkernel::Variant val(result[2]);
                vallst.push_back(std::make_pair("name", val));

            }
            if (sz > 1)
            {
                result[1].erase(result[1].begin(), std::find_if(result[1].begin(), result[1].end(), std::bind1st(std::not_equal_to<char>(), ' ')));
                result[1].erase(std::find_if(result[1].rbegin(), result[1].rend(), std::bind1st(std::not_equal_to<char>(), ' ')).base(), result[1].end());
                unsigned int a_id = strtoul(result[1].c_str(), NULL, 0);
                if (a_id > 0)
                {
                    //skwy_val.push_back(string("id=") + to_string(a_id));
                    cfgkernel::Variant val(a_id);
                    vallst.push_back(std::make_pair("id", val));
                    id_read = true;
                }
            }
        }
        else if (a_commments[i].find("$HMNAME") != string::npos)
        {
            // a_commments[i].erase(a_commments[i].begin(), std::find_if(a_commments[i].begin(), a_commments[i].end(), [](unsigned char ch) {
               //     return !std::isspace(ch);
            a_commments[i].erase(0, a_commments[i].find_first_not_of(" \t\n\r\f"));

            string rem_str;
            // Remove spaces after 7 characters:
            if (a_commments[i].length() > 7) {
                rem_str = a_commments[i].substr(7);
                rem_str.erase(0, rem_str.find_first_not_of(" \t\n\r\f"));
            }
            const char* a_hm_line = rem_str.c_str();
            int id_format = 8;
            unsigned int a_id = 0;
            int hm_entity_sz = GetHMNAMEEntityNameSize(etype, a_hm_line, &id_format);
            char frt[20];
            strncpy(frt, a_hm_line + hm_entity_sz, id_format);
            string id_param_str = "";
            bool is_param_cell = false;// dataReader.isParameterCell(frt, id_format, id_param_str);
            if (is_param_cell)
            {
                a_id = 0;
                //a_pre_object_p->SetId(a_id);
                //a_pre_object_p->SetParameterIdName(id_param_str.c_str());
            }
            else
            {
                frt[10] = '\0';
                a_id = strtoul(frt, NULL, 0); // Revisit if MYOBJ_INT type is changed.
                if (a_id > 0)
                {
                    //skwy_val.push_back(string("id=") + to_string(a_id)); //a_pre_object_p->SetId(a_id);
                    cfgkernel::Variant val(a_id);
                    vallst.push_back(std::make_pair("id", val));
                    id_read = true;
                }
            }


            int len = (int)(strlen(a_hm_line));
            if (len > (hm_entity_sz + id_format)) {
                char title[256] = { 0 };
                std::string stringVal = a_hm_line + hm_entity_sz + id_format;
                stringVal.erase(stringVal.find_last_not_of(" \t\n\r\f\v") + 1);
                cfgkernel::Variant val(stringVal);
                vallst.push_back(std::make_pair("name", val));
            }
        }
        else if (a_commments[i].find("$HWCOLOR") != string::npos)
        {

            a_commments[i].erase(a_commments[i].find_last_not_of(" \t\n\r\f\v") + 1);
            a_commments[i].erase(0, 8);

            int id_format = 8;
            unsigned int a_id = 0;
            int hm_entity_sz = GetHMNAMEEntityNameSize(etype, a_commments[i].c_str(), &id_format);

            string frt = a_commments[i].substr(hm_entity_sz + id_format);

            string id_param_str = "";
            bool is_param_cell = false;// dataReader.isParameterCell(frt.c_str(), id_format, id_param_str);
            if (is_param_cell)
            {
                a_id = 0;
                //a_pre_object_p->SetId(a_id);
                //a_pre_object_p->SetParameterIdName(id_param_str.c_str());
            }
            else
            {
                //a_id = strtoul(frt.c_str(), NULL, 0); // Revisit if MYOBJ_INT type is changed.
                if (a_id > 0 && !id_read)
                {
                    //skwy_val.push_back(string("id=") + to_string(a_id)); //a_pre_object_p->SetId(a_id);
                    cfgkernel::Variant val(a_id);
                    vallst.push_back(std::make_pair("id", val));

                }
            }

            int len = (int)a_commments[i].size();
            if (len > (hm_entity_sz + id_format))
            {
                string frt = a_commments[i].substr(hm_entity_sz + id_format);
                int a_col = atoi(frt.c_str());
                //skwy_val.push_back(string("color=") + to_string(a_col));//a_pre_object_p->AddIntValue("_HWCOLOR", a_col);
                cfgkernel::Variant val(a_col);
                vallst.push_back(std::make_pair("color", val));
            }
        }
        else if (line[0] == '$')
        {
            //skwy_val.push_back(string("comments=") + a_commments[i]);//a_pre_object_p->AddUserComment(string(line));
        }
    }
}


/* --------- Element infos --------- */
void SolverInputInfo::initElementInfos() {
  // QUAD

}


bool SolverInputInfo::IsEntityCryptable(object_type_e obj_type, MvFileFormat_e version) const {

    ApplicationMode_e appmode = (ApplicationMode_e)GetAppMode();
    bool id_read = false;
    switch (appmode)
    {
        case HCDI_SOLVER_RADIOSS:
        {
            if (version < FF_D00_51 && obj_type == HCDI_OBJ_TYPE_CURVES)
                return false;
            switch (obj_type)
            {
            case HCDI_OBJ_TYPE_MATS:
            case HCDI_OBJ_TYPE_PROPS:
            case HCDI_OBJ_TYPE_FAILURES:
            case HCDI_OBJ_TYPE_CURVES:
                return true;
            default:
                break;
            }
            return false;
            break;
        }
        case HCDI_SOLVER_LSDYNA:
        {
            switch (obj_type)
            {
            case HCDI_OBJ_TYPE_MATS:
            case HCDI_OBJ_TYPE_PROPS:
            case HCDI_OBJ_TYPE_FAILURES:
            case HCDI_OBJ_TYPE_CURVES:
                return true;
            default:
                break;
            }
            break;
        }
        default:
            break;
    }
    return false;
}
