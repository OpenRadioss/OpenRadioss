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



#include <UTILS/mv_stl_various.h>
#include <UTILS/error.h>
#include <UTILS/str_utils.h>

#include <HCDI/hcdi_utils.h>

#include "mv_data_cfg_parser.h"
#include "mv_type.h"
#include "mv_data_data_feature.h"
typedef deque<string> MvStringList_t;

static map<void *, vector< pair<string, comparator_e> > >  mapdata_dft_fulltypes; /*multimodel should be OK temp variable*/
//will work when num_1 is non-zero
int loc_substract(int num_1, int num_2);

std::string loc_trim(const std::string& str) {
    size_t first = str.find_first_not_of(' ');
    if (std::string::npos == first) {
        return str;
    }
    size_t last = str.find_last_not_of(' ');
    return str.substr(first, (last - first + 1));
}
// Function to convert string to lower case
std::string loc_toLower(const std::string& str) {
    std::string result;
    for (char c : str) {
        result += std::tolower(c);
    }
    return result;
}

MvDataCfgParser_t::MvDataCfgParser_t(const string &fullname) : MvParserBase_t(fullname) {
}
void MvDataCfgParser_t::StoreFullTypesForPostTreat(void *dft, vector< pair<string, comparator_e> > &fulltype)
{
    mapdata_dft_fulltypes.insert(pair<void *, vector< pair<string, comparator_e> > &>(dft, fulltype));
}
MvPreDatasHierarchy_t *MvDataCfgParser_t::getDatasHierarchyPtr(CFGKernel& cfgkernel, const string &title) {

    mapdata_dft_fulltypes.clear();

  
  MvPreDatasHierarchy_t *a_pdh_p=new MvPreDatasHierarchy_t(DOM_COMMON, (void *)(&cfgkernel),"NO_KEYWORD",title, HCDI_OBJ_TYPE_NULL,NULL);
  
  //
  string keyword;
  int allowed_flags = cfgkernel.get_default_bit();
  
  //try { getNextChar(); unreadChar(); } catch(MvError_t &) {}

  char rep_with = '\0';
  if (cfgkernel.getUserProfile() == HCDI_SOLVER_RADIOSS)
  {
      //SyntaxInfo* pinfo = cfgkernel.getSyntaxInfo();
      //string begin_str, end_str, submodel_str, submodel_end_str;
      //int linelength = 0;
      //if (pinfo)
      //{
      //    std::vector<SyntaxInfo::Variant> h_lst = pinfo->getArrayAttribute("HEADER");
      //    for (int i = 0; i < h_lst.size(); i++)
      //    {
      //        string  hh = h_lst[i].getStringValue();
      //    }
      //}
      rep_with = '/';
  }

  while(!seof(false)) {
    keyword=getNextString();
    if(keyword!="HIERARCHY" && keyword != "FLAGS") {
      delete a_pdh_p;
      throwError(getMsg(4),keyword.c_str());
    }
    
  if (keyword == "HIERARCHY")
  {
    a_pdh_p->addChild(getDatasHierarchyPtr(cfgkernel, HCDI_OBJ_TYPE_NULL,a_pdh_p->getDomain(),allowed_flags, rep_with));
  }
  else if (keyword == "FLAGS")
  {
      readFlagInformation(cfgkernel , &allowed_flags);
  }
    
    //try { getNextChar(); unreadChar(); } catch(MvError_t &) {}
  }
  map<void *, vector< pair<string, comparator_e> >>::iterator itr_beg = mapdata_dft_fulltypes.begin();
  map<void *, vector< pair<string, comparator_e> >>::iterator itr_end = mapdata_dft_fulltypes.end();
  map<void *, vector< pair<string, comparator_e> >>::iterator itr;
  for (itr = itr_beg; itr != itr_end; ++itr)
  {
      void *dft = (*itr).first;
      vector< pair<string, comparator_e> >  &vectlst = (*itr).second;
      vector< pair<string, comparator_e> >::iterator itr_vec_beg = vectlst.begin();
      vector< pair<string, comparator_e> >::iterator itr_vec_end = vectlst.end();
      vector< pair<string, comparator_e> >::iterator itr_vec;
      for (itr_vec = itr_vec_beg; itr_vec != itr_vec_end; ++itr_vec)
      {
          pair<string, comparator_e> a_pair = *itr_vec;
          string &fulltype = (*itr_vec).first;

          comparator_e  a_comp = (*itr_vec).second;
          if (a_comp == CMPT_EQ)
          {
              MvFullType_t a_fulltype((const CFGKernel & )cfgkernel, fulltype);
              MvDataDataFeature_t* a_ddf_p = (MvDataDataFeature_t*)dft;
              a_ddf_p->addObjectFullType(a_fulltype);
          }
          else if (a_comp == CMPT_NE)
          {
              MvFullType_t a_fulltype(cfgkernel, fulltype);
              obj_type_e type = a_fulltype.getType();
              const MvSubtype_t* subtype_main = a_fulltype.getSubtypePtr();
              MvSubtypePtrSet_t a_subtypep_set;
              cfgkernel.get_subtypes(type, &a_subtypep_set);
              MvSubtypePtrSet_t::iterator a_iter_b = a_subtypep_set.begin();
              MvSubtypePtrSet_t::iterator a_iter_e = a_subtypep_set.end();
              MvSubtypePtrSet_t::iterator a_iter;
              for (a_iter = a_iter_b; a_iter != a_iter_e; ++a_iter)
              {
                  const MvSubtype_t* subtype = *a_iter;
                  if(subtype && subtype_main && subtype != subtype_main)
                  {
                      MvFullType_t a_ftype(type, subtype);
                      MvDataDataFeature_t* a_ddf_p = (MvDataDataFeature_t*)dft;
                      a_ddf_p->addObjectFullType(a_ftype);
                  }
              }
          }
      }
  }
  
  
  return a_pdh_p;
}

SyntaxInfo* MvDataCfgParser_t::getSyntaxInfoPtr() 
{
    SyntaxInfo* a_syninfo = new SyntaxInfo();
    std::string a_line;
    std::string keyword;
    bool capture = false;
    while (!seof(false)) {
        keyword = getNextString();
        if (!capture)
        {
            if (keyword.find("SYNTAX_INFO") != std::string::npos)
            {
                capture = true;
                if (getNextChar() != '{') throwError(getMsg(6));
                   continue;
            }
        }
        else
        {
            vector<string> str_arr;
            char ch = getNextChar();
            if (ch == '}')
                break;

            if (ch != '=') throwError(getMsg(7));
            std::string value;
            char comma = '\"';
            char c = getNextChar();
            if (c == comma)
            {
                unreadChar();
                value = getNextQuotedString();
            }
            else
            {
                if (c == '(')
                {
                    value = string(1, c);
                    char a_char;
                    do 
                    {
                        string a_name;
                        a_char = getNextChar();
                        unreadChar();
                        if (a_char == '"')
                            a_name = getNextQuotedString();
                        else
                            a_name = getNextString();

                        if (a_name.empty()) throwError(getMsg(2));
                        //a_user_name_list.push_back(a_name);
                        value += a_name;

                        str_arr.push_back(a_name);

                        a_char = getNextChar();
                        value += string(1, a_char);
                    } while (a_char == ',');
                    if (a_char != ')') throwError(getMsg(13), ")");
                }
                else
                {
                    unreadChar();
                    value = getNextString();
                }
            }
            if (getNextChar() != ';') throwError(getMsg(8));

            if(keyword.empty() || value.empty())
                throwError(getMsg(4), keyword.c_str());

            // Check if value is an array
            if(str_arr.size())
            {
                SyntaxInfo::ArrayValue arrayValue;
                for (int i = 0; i < str_arr.size(); i++)
                {
                    std::istringstream isst(str_arr[i]);
                    int intValue;
                    if (isst >> intValue)
                        arrayValue.push_back(cfgkernel::Variant(intValue));
                    else
                        arrayValue.push_back(cfgkernel::Variant(str_arr[i]));
                }
                a_syninfo->addArrayAttribute(keyword, arrayValue);
            }
            else
            {
                std::string lowerValue = loc_toLower(value);
                if (lowerValue == "true" || lowerValue == "false")
                {
                    bool boolValue = lowerValue == "true";
                    a_syninfo->addAttribute(keyword, boolValue);
                }
                else
                {
                    // Try converting to integer
                    std::istringstream isst(value);
                    int intValue;
                    if (isst >> intValue)
                    {
                        a_syninfo->addAttribute(keyword, intValue);
                    }
                    else
                    {
                        // If stoi() fails, treat as string
                        a_syninfo->addAttribute(keyword, value);
                    }
                }
            }
        }
    }
    return a_syninfo;
}
MvPreDatasHierarchy_t *MvDataCfgParser_t::getDatasHierarchyPtr(CFGKernel& cfgkernel, object_type_e current_type,
							       MvDomain_e    current_domain, int parent_flag, char un_rep_char)
{
  
  MvDomain_e a_domain=current_domain;
  char a_char=getNextChar();
  if(a_char=='(') {
    a_domain=getNextDomain();
    if(getNextChar()!=')') throwError(getMsg(13),")");
    a_char=getNextChar();
  }
  
  if(a_char!='{') throwError(getMsg(6));
  //
  MvPreDatasHierarchyList_t a_child_list;
  string                    a_keyword="NO_KEYWORD";
  string                    a_title="NO_TITLE";
  string                    a_file_name="";
  string                    a_cardimage="";
  object_type_e             a_type=current_type;
  int                       a_user_id=-1, a_hm_config_type=-1, a_hm_type=-1, a_idpool=-1;
  bool                      a_subtype_bool=false;
  string                    a_subtype_string;
  const MvSubtype_t        *a_subtype_p=NULL;
  MvStringList_t            a_user_name_list;
  MvStringList_t            a_optional_header_name_list;

  bool                      a_do_expand=false;
  //
  string keyword;
  string a_htype = "";

  int flag = parent_flag;
  while(getNextChar()!='}') {
    unreadChar();
    keyword=getNextString();
    // Title
    if(keyword=="TITLE") {
      if(getNextChar()!='=') throwError(getMsg(7));
      a_title=getNextQuotedString();      
      if(!isSeparator(getNextChar())) throwError(getMsg(8));
    } 
    // Keyword
    else if(keyword=="KEYWORD") {
      if(getNextChar()!='=') throwError(getMsg(7));
      
      char comma = '\"';
      char c = getNextChar();
      if (c == comma)
      {
          unreadChar();
          a_keyword = getNextQuotedString();
      }
      else
      {
          unreadChar();
          a_keyword = getNextString();
      }
      if(!isSeparator(getNextChar())) throwError(getMsg(8));
    } 
    // Type
    else if(keyword=="TYPE") {
      if(getNextChar()!='=') throwError(getMsg(7));
      string a_type_string=getNextString();
      a_type=MV_get_type(a_type_string);
      if(a_type== HCDI_OBJ_TYPE_NULL) throwError(getMsg(9),a_type_string.c_str());
      if(!isSeparator(getNextChar())) throwError(getMsg(8));
    } 
    // Subtype
    else if(keyword=="SUBTYPE") {
      if(a_type== HCDI_OBJ_TYPE_NULL) throwError(getMsg(11));
      if(getNextChar()!='=') throwError(getMsg(7));
      a_subtype_string=getNextString();
      a_subtype_p=(a_subtype_string!="USER") ? cfgkernel.get_subtype(a_type,a_subtype_string) : NULL;
      if(!isSeparator(getNextChar())) throwError(getMsg(8));
      a_subtype_bool=true;
    }
	 
	 // Expand
	 else if(keyword=="EXPAND") {
		  if(getNextChar()!='=') throwError(getMsg(7));
		  string a_status=getNextString();
		  if(a_status=="TRUE") a_do_expand=true;
		  if(!isSeparator(getNextChar())) throwError(getMsg(8));
	 }
	 
    // File
    else if(keyword=="FILE") {
      if(a_type== HCDI_OBJ_TYPE_NULL) throwError(getMsg(35),a_title.c_str());
      if(getNextChar()!='=') throwError(getMsg(7));
      a_file_name=getNextQuotedString();
      if(!isSeparator(getNextChar())) throwError(getMsg(8));
    }
    // User ID
    else if(keyword=="USER_ID") {
      if(getNextChar()!='=') throwError(getMsg(7));
      a_user_id=getNextInt();
      if(!isSeparator(getNextChar())) throwError(getMsg(8));
    }
    else if(keyword=="HM_CONFIG_TYPE") {
      if(getNextChar()!='=') throwError(getMsg(7));
      a_hm_config_type=getNextInt();
      if(!isSeparator(getNextChar())) throwError(getMsg(8));
    }
    else if(keyword=="HM_TYPE") {
      if(getNextChar()!='=') throwError(getMsg(7));
      a_hm_type=getNextInt();
      if(!isSeparator(getNextChar())) throwError(getMsg(8));
    }
    else if (keyword == "ID_POOL") {
      if (getNextChar() != '=') throwError(getMsg(7));
      a_idpool = getNextInt();
      if (!isSeparator(getNextChar())) throwError(getMsg(8));
    }
    // Card Image
    else if(keyword=="CARD_IMAGE") {
      if(getNextChar()!='=') throwError(getMsg(7));
      a_cardimage=getNextString();
      if(!isSeparator(getNextChar())) throwError(getMsg(8));
    } 
    // Other names
    else if(keyword=="USER_NAMES") {
      if(getNextChar()!='=') throwError(getMsg(7));
      if(getNextChar()!='(') throwError(getMsg(13),"(");

      do {
          string a_user_name;
          a_char = getNextChar();
          unreadChar();
          if (a_char == '"')
              a_user_name = getNextQuotedString();
          else
              a_user_name = getNextString();

          if(a_user_name.empty()) throwError(getMsg(2));
          a_user_name_list.push_back(a_user_name);
          if (un_rep_char != '\0')
          {
              auto pos = a_user_name.find('_');
              if (pos != std::string::npos) {
                  std::replace(a_user_name.begin(), a_user_name.end(), '_', un_rep_char);
                  // Add the modified string to the vector
                  a_user_name_list.push_back(a_user_name);
              }
          }
          a_char = getNextChar();
      } while (a_char == ',');
      if (a_char != ')') throwError(getMsg(13), ")");
      if (!isSeparator(getNextChar())) throwError(getMsg(8));
    }
    else if (keyword == "OPTIONAL_HEADER_NAMES") {
        if (getNextChar() != '=') throwError(getMsg(7));
        if (getNextChar() != '(') throwError(getMsg(13), "(");

        do {
            string a_optional_name = getNextString();
            a_optional_header_name_list.push_back(a_optional_name);
            a_char = getNextChar();
        } while (a_char == ',');
        if (a_char != ')') throwError(getMsg(13), ")");
        if (!isSeparator(getNextChar())) throwError(getMsg(8));
    }
    else if (keyword == "HTYPE")
    {
        if (getNextChar() != '=') throwError(getMsg(7));
        a_htype = getNextString();
        if (getNextChar() != ';') throwError(getMsg(8));
    }
    else if (keyword == "FLAGS")
    {
        readFlagInformation(cfgkernel, &flag);
    }
    // Children
    else if(keyword=="HIERARCHY") {
      a_child_list.push_back(getDatasHierarchyPtr(cfgkernel, a_type,a_domain,flag, un_rep_char));
    } 
    // Error
    else {
      throwError(getMsg(4),keyword.c_str());
    }
  }

  bool add_to_keyword_map = a_child_list.size() == 0; // only USER_NAMES of last level to be added to map
  // everything is user by default, to make it as intrinsic non-user type, need to implement non-user
  if(a_subtype_bool && a_subtype_p==NULL) {
    if((a_user_id==-1 && (a_hm_config_type <= 0 && a_hm_type <= 0) && a_user_name_list.empty())|| a_subtype_string!="USER" || a_user_name_list.empty()) {
      throwError(getMsg(10),a_subtype_string.c_str());
    } else {
        if(a_cardimage != "")
            a_user_name_list.push_back(a_cardimage);
        a_subtype_p= cfgkernel.add_user_subtype(
            a_type, a_user_id, a_hm_config_type, a_hm_type, (short)a_idpool, a_cardimage,
            (PseudoStringList_t *)(&a_user_name_list), (PseudoStringList_t*)(&a_optional_header_name_list),
            add_to_keyword_map);
    }
  }
  else
  {
      if(a_user_name_list.empty() && (a_keyword == "NO_KEYWORD") && (a_title == "NO_TITLE"))
      {
          throwError(getMsg(10), a_subtype_string.c_str());
      }

      if (a_user_name_list.empty() && (a_user_id > 0 ||a_hm_config_type > 0 || a_hm_type > 0))
      {
          // fill with keyword if user name is not defined.
          a_user_name_list.push_back(a_keyword);
      }
      if (a_cardimage != "")
          a_user_name_list.push_back(a_cardimage);
      if(!a_user_name_list.empty())
          a_subtype_p = cfgkernel.add_user_subtype(
              a_type, a_user_id, a_hm_config_type, a_hm_type, (short)a_idpool, a_cardimage,
              (PseudoStringList_t *)(&a_user_name_list), (PseudoStringList_t*)(&a_optional_header_name_list),
              add_to_keyword_map);
  }
  
  //cout << a_keyword << " -> " << MV_get_domain(a_domain) << endl;
  MvPreDatasHierarchy_t *a_pdh_p=new MvPreDatasHierarchy_t(a_domain, (void *)(&cfgkernel), a_keyword,a_title,
							   a_type,a_subtype_p,a_file_name,
							   (PseudoStringList_t *)(&a_user_name_list));
	a_pdh_p->setExpand(a_do_expand);  
    a_pdh_p->setHtype(a_htype);
    a_pdh_p->setFlags(flag);
  
  MvPreDatasHierarchyList_t::const_iterator it,it_begin=a_child_list.begin(),it_end=a_child_list.end();
  for(it=it_begin;it!=it_end;++it) a_pdh_p->addChild(*it);
  return a_pdh_p;
}


MvDomain_e MvDataCfgParser_t::getNextDomain() {
  string     a_domain_str = getNextString();
  MvDomain_e a_domain     = MV_get_domain(a_domain_str);
  if(a_domain==DOM_UNKNOWN) throwError(getMsg(21),a_domain_str.c_str());
  //
  return a_domain;
}

void MvDataCfgParser_t::readFlagInformation(CFGKernel& cfgkernel, int *flag)
{
    string flag_list = "";
    if (getNextChar() != '=') throwError(getMsg(7));
    if (getNextChar() != '(') throwError(getMsg(13), "(");

    char c = getNextChar();
    while (c != ')')
    {
        flag_list += c;
        c = getNextChar();
        if (c == ';')
            break;
    }
    if (c != ')') throwError(getMsg(13), ")");
    c = getNextChar();
    if (c != ';') throwError(getMsg(13), ";");

    vector<string> key_vect;
    StringTokenize(flag_list, key_vect, "|");

    int nb_tokens = (int)key_vect.size();
    for (int i = 0; i < nb_tokens; i++)
    {
        string token = key_vect[i];
        int value = 0;
        string value_str = "";
        if (token.at(0) == '!')
        {
            int size = (int)token.size();
            if (size > 0)
            {
                string sub_token = token.substr(1, (size_t)size - 1);
                value = cfgkernel.set_default_data_hierarchy_flag(sub_token);
                *flag = loc_substract(*flag, value);
            }
        }
        else
        {
            value = cfgkernel.set_default_data_hierarchy_flag(token);
            *flag = *flag | value;
        }
    }
}

int loc_substract(int num_1, int num_2)
{
    while (num_2 != 0)
    {
		if(num_1 == 0)
        { 
            return 0;
        }
        int borrow = (~num_1) & num_2;

        num_1 = num_1 ^ num_2;

        num_2 = borrow << 1;
    }
    return num_1;
}


