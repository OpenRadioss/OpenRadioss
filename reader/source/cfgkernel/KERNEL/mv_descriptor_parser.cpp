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
#include <UTILS/str_utils.h>

#include <cstring>
#include "mv_type.h"
#include "mv_keywords.h"
#include "mv_file_format.h"
#include "mv_compare_test.h"
#include "mv_compare_attributes_test.h"      
#include "mv_logical_operator.h"
#include "mv_data_cond_scalar_feature.h"     
#include "mv_data_uncond_scalar_feature.h"   
#include "mv_data_file_feature.h"
#include "mv_data_dir_feature.h"
#include "mv_data_triple_feature.h"
#include "mv_data_point_feature.h"
#include "mv_data_radio_feature.h"
#include "mv_data_flag_feature.h"
#include "mv_data_flag_list_feature.h"
#include "mv_data_dynamic_array_feature.h"
#include "mv_data_static_array_feature.h"
#include "mv_data_data_feature.h"
#include "mv_data_subobject_feature.h"
#include "mv_data_skew_feature.h"
#include "mv_data_sensor_feature.h"
#include "mv_data_uncond_function_feature.h" 
#include "mv_data_cond_function_feature.h"   
#include "mv_data_accelero_feature.h"
#include "mv_data_radio_array_feature.h"
#include "mv_data_support_feature.h"
#include "mv_data_append_feature.h"
#include "mv_data_if_feature.h"
#include "mv_data_separator_feature.h"
#include "mv_data_unit_feature.h"
#include "mv_data_array_to_single_feature.h" 
#include "mv_data_dynamic_matrix_feature.h"  
#include "mv_data_assign_feature.h"
#include "mv_attribute_expression.h"
#include "mv_logical_expression.h"
#include "mv_drawable_scalar.h"
#include "mv_drawable_subdrawable.h"
#include "mv_drawable_while_zero.h"
#include "mv_drawable_eval.h"
#include "mv_drawable_time_step.h"
#include "mv_drawable_max.h"
#include "mv_drawable_min.h"
#include "mv_drawable_volume.h"              
#include "mv_drawable_area.h"                
#include "mv_iparam_descr_scalar.h"          
#include "mv_iparam_descr_subparam.h"        
#include "mv_iparam_descr_translation.h"     
#include "mv_iparam_descr_rotation.h"        
#include "mv_iparam_descr_scaling.h"         
#include "mv_model_descriptors.h"            
#include "Structure_fileformat_others.h"
#include "mv_descriptor_parser.h"
#include "mv_data_size_radio_feature.h"
#include "mvc_utils.h"

#include <UTILS/memory_utils.h>
#include <UTILS/set_utils.h>
#include <UTILS/error.h>
#include <KERNEL_BASE/fileformat_API.h> 
#include <KERNEL_BASE/utils.h>
#include <UTILS/str_utils.h>
#include "mv_data_cfg_parser.h"

#include <HCDI/hcdi_multicfgkernelmgr.h>
#include "cfg_kernel.h"



//#include "mv_memory_report.h"       


#define LOC_IKEYWORD_OFFSET CFG_IKEYWORD_LAST-1



typedef vector<string>      LocFormatList_t;
typedef vector<ff_cell_t *> LocCellList_t;
typedef vector<ff_card_t *> LocCardList_t;


typedef vector<LocCardList_t>    LocCardListList_t;
typedef vector<MvExpression_t *> LocExprList_t;



static value_type_e  loc_get_vtype(const string &vtype);
static int           loc_string2int(const string &s);
static double        loc_string2double(const string &s); 
static int           loc_get_fmt(const string &fmt_str,LocFormatList_t *fmt_p);
static ff_card_t    *loc_new_card(ff_card_type_e card_type,LocCellList_t &cell_list);




//static MvFullTypeSet_t &operator+=(MvFullTypeSet_t &fts,const MvObjectTypeSet_t &otypes); 
void operatorplusegal(MvFullTypeSet_t &fts,const MvObjectTypeSet_t &otypes); 





typedef struct LocRadioButton_s {
  int        myIntValue;
  unsigned int myUIntValue;
  double     myFloatValue;
  string     myStringValue;
  string     myComment;
  MvDomain_e myDomain;
  bool       retain_comments;
} LocRadioButton_t;
typedef vector<LocRadioButton_t> LocRadioButtonList_t;


/* --------- Constructor and destructor --------- */

MvDescriptorParser_t::MvDescriptorParser_t(void* cfgkernel, const string &fullname,object_type_e obj_type,bool is_user) :
  MvParserBase_t(fullname),
  myIsUser(is_user),
  myCurrentIKeyword(LOC_IKEYWORD_OFFSET),
  myObjectType(obj_type),
  mycfgkernel(cfgkernel),
  myIsOptional(true) /*S_176280:RC_15_10_2009*/
{
  //MV_add_memory_report("  End of MvDescriptorParser_t::MvDescriptorParser_t");
}


/* --------- Parsing the file and getting the descriptor --------- */

MvDescriptor_t* MvDescriptorParser_t::getDescriptorPtr() {
    //MV_add_memory_report("  Before MvDescriptorParser_t::getDescriptorPtr()");
    MvDescriptor_t* a_descr_p = new MvDescriptor_t();
    //
    try {
        string a_keyword;
        
        //try { getNextChar(); unreadChar(); } catch(MvError_t &) {}
        while (!seof(false)) {
            a_keyword = getNextString();
            if (a_keyword == "ATTRIBUTES") {
                readAttributes(a_descr_p);
            }
            else if (a_keyword == "DEFAULTS") {
                readDefaults(a_descr_p);
            }
            else if (a_keyword == "SKEYWORDS_IDENTIFIER") {//identifier?????
                readSKeywordsIdentifier(a_descr_p);
            }
            else if (a_keyword == "CHECK") {
                readTests(a_descr_p);
            }
            else if (a_keyword == "GUI") {
                readFeatures(a_descr_p);
            }
            else if (a_keyword == "FORMAT") {
                readFormat(a_descr_p);
            }
            else if (a_keyword == "DRAWABLES") {
                readDrawables(a_descr_p);
            }
            else if (a_keyword == "PARAMETERS") {  
                readParameters(a_descr_p);          
            }
            else if (a_keyword == "DEFINITIONS") { 
                readDefinitions(a_descr_p);         
            }
            else {
                throwError(getMsg(4), a_keyword.c_str());
            }
            //try { getNextChar(); unreadChar(); } catch(MvError_t &) {}
        }
        
    }
    catch (MvError_t& err) {
        delete a_descr_p;
        throw MvError_t(err.GetMessage() + "\n" + myBuffer + getMsg(5));
    }
    //
    //MV_add_memory_report("  Before MvDescriptorParser_t::postRead(a_descr_p)");
    postRead(a_descr_p); 
    //MV_add_memory_report("  After  MvDescriptorParser_t::postRead(a_descr_p)");
    //
    //MV_add_memory_report("  After  MvDescriptorParser_t::getDescriptorPtr()");
    return a_descr_p;
}


/* --------- Attributes --------- */

void MvDescriptorParser_t::readAttributes(MvDescriptor_t* descr_p) {
    // Domain
    MvDomain_e a_domain = DOM_COMMON;
    char a_char = getNextChar();
    if (a_char == '(') {
        a_domain = getNextDomain();
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        a_char = getNextChar();
    }
    if (a_char != '{') throwError(getMsg(13), "{");
    string a_skeyword;
    while (getNextChar() != '}') {
        unreadChar();
        a_skeyword = getNextString();
        while (a_skeyword == "if") a_skeyword = readDependence(a_domain, descr_p, a_skeyword);
        if (a_skeyword != "") readAttribute(a_domain, descr_p, a_skeyword);
    }
}

int MvDescriptorParser_t::readAttribute(MvDomain_e domain, MvDescriptor_t* descr_p, const string& skeyword) {
    int a_ikeyword = descr_p->getIKeyword(skeyword);
    if (a_ikeyword != END_ARGS) {
        if (getNextChar() != ';') throwError(getMsg(49), skeyword.c_str(), ";");
        return a_ikeyword;
    }
    //
    if (getNextChar() != '=') throwError(getMsg(13), "=");
    string an_attrib_type = getNextString();
    if (an_attrib_type == "VALUE")      readAttributeValue(domain, descr_p, skeyword);
    else if (an_attrib_type == "SIZE")  readAttributeSize(domain, descr_p, skeyword);
    else if (an_attrib_type == "ARRAY") readAttributeArray(domain, descr_p, skeyword);
    else                             throwError(getMsg(20), an_attrib_type.c_str());
    //
    a_ikeyword = descr_p->getIKeyword(skeyword);
    return a_ikeyword;
}

string MvDescriptorParser_t::readDependence(MvDomain_e domain, MvDescriptor_t* descr_p, const string& skeyword) {
    string          a_skeyword = skeyword;
    string          a_cond_type = a_skeyword;
    MvDependence_t* a_depend_p = NULL;
    while (a_cond_type == "if") {
        // Expression
        if (getNextChar() != '(') throwError(getMsg(13), "(");
        MvExpression_t* a_expr_p = getNextExpressionPtr(descr_p);
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        // Ikeywords
        MvIKeywordSet_t a_ikw_set;
        if (getNextChar() != '{') {
            unreadChar();
            a_ikw_set.insert(readAttribute(domain, descr_p, getNextString()));
        }
        else while (getNextChar() != '}') {
            unreadChar();
            a_ikw_set.insert(readAttribute(domain, descr_p, getNextString()));
        }
        // Adding the condition
        if (a_depend_p == NULL) a_depend_p = new MvDependence_t();
        a_depend_p->addCondition(a_expr_p, a_ikw_set);
        // Next condition
        char a_char = getNextChar();
        unreadChar();
        if (a_char == '}') {
            a_cond_type = a_skeyword = "";
        }
        else {
            a_cond_type = a_skeyword = getNextString();
            if (a_skeyword == "else") {
                a_ikw_set.clear();
                a_skeyword = "";
                if (getNextChar() == '{') while (getNextChar() != '}') {
                    unreadChar();
                    a_ikw_set.insert(readAttribute(domain, descr_p, getNextString()));
                }
                else {
                    unreadChar();
                    a_skeyword = getNextString();
                    if (a_skeyword != "if") {
                        a_cond_type = a_skeyword;
                        a_ikw_set.insert(readAttribute(domain, descr_p, a_skeyword));
                        a_skeyword = "";
                    }
                }
                if (a_skeyword != "if") a_depend_p->addDefault(a_ikw_set);
            }
            else {
                a_cond_type = "";
            }
        }
    }
    // Adding the dependence
    if (a_depend_p)
    {
        a_depend_p->posttreat();
        descr_p->addDependence(domain, a_depend_p);
    }
    return a_skeyword;
}


void MvDescriptorParser_t::getSubtypesListForMultiObject(vector<string>& vect_subtype)
{

    char c = getNextChar();
    if (c != '{')
        throwError(getMsg(13), "{");

    string text = getNextString();
    if (text != "SUBTYPES")
        throwError(getMsg(13), "SUBTYPES");
    c = getNextChar();
    if (c != '=')
        throwError(getMsg(13), "=");

    c = getNextChar();
    if (c != '(')
        throwError(getMsg(13), "(");

    c = getNextChar();

    while (c != ')')
    {
        if (c != '/')
            throwError(getMsg(13), "/");

        text = getNextString();
        if (text != "")
            vect_subtype.push_back(text);

        c = getNextChar();

        // subtypes are finished
        if (c == ')')
            break;
        if (c != ',')
            throwError(getMsg(13), ",");

        c = getNextChar();
    }

    c = getNextChar();
    while (c != ';')
    {
        c = getNextChar();
    }
    c = getNextChar();
    if (c != '}')
        throwError(getMsg(13), "}");
}

void MvDescriptorParser_t::readPossibleSubtypeList(MvDescriptor_t* descr_p, int a_ikeyword)
{
    // reading of SUBTYPES
    //  { SUBTYPES = ( /NODE , /GRNOD ) ;  }
    vector<string> vect_subtype;
    getSubtypesListForMultiObject(vect_subtype);

    // fill the data
    const descriptor_t* cdescr_p = descr_p->getDescriptorPtr();
    object_descriptor_t* objdescr_p = (object_descriptor_t*)(cdescr_p->attdescr_array[a_ikeyword]);

    int nb_subtypes = (int)vect_subtype.size();
    objdescr_p->allowed_types = (obj_type_e*)my_malloc(nb_subtypes, sizeof(obj_type_e));
    objdescr_p->comments = (char**)my_malloc(nb_subtypes, sizeof(char*));
    objdescr_p->subtypes = (char**)my_malloc(nb_subtypes, sizeof(char*));
    objdescr_p->num = nb_subtypes;

    for (int i = 0; i < nb_subtypes; i++)
    {
        string subtype_str = vect_subtype.at(i);
        vector<string> key_vect;
        StringTokenize(subtype_str, key_vect, "/");
        int size = (int)key_vect.size();
        if (!key_vect.empty())
        {
            object_type_e a_type = MV_get_type(key_vect[0]);
            if (a_type == HCDI_OBJ_TYPE_NULL)
                throwError(getMsg(10), key_vect[0].c_str());
            objdescr_p->comments[i] = (char*)my_malloc((int)(strlen("")), sizeof(char));
            objdescr_p->allowed_types[i] = a_type;
#ifdef _DEBUG
            if (objdescr_p->allowed_types[i] == HCDI_OBJ_TYPE_NULL)
                throwError(getMsg(10), key_vect[0].c_str());
#endif
            if (size == 2)
            {
                objdescr_p->subtypes[i] = (char*)my_malloc((int)(key_vect[1].length()) + 1, sizeof(char));
                strcat(objdescr_p->subtypes[i], key_vect[1].c_str());
            }
            else
            {
                objdescr_p->subtypes[i] = (char*)my_malloc((int)(strlen("")), sizeof(char));
            }
        }
    }
}
void MvDescriptorParser_t::readAttributeValue(MvDomain_e domain, MvDescriptor_t* descr_p, const string& skeyword)
{
    // Parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    string a_type_str = getNextString();
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    string a_comment = getNextQuotedString(), a_solver_comment = "";// skeyword;
    //std::transform(a_solver_comment.begin(), a_solver_comment.end(), a_solver_comment.begin(), ::toupper);
    char a_char = getNextChar();
    if (a_char == ',')
    {
        a_solver_comment = getNextQuotedString();
        if (getNextChar() != ')') throwError(getMsg(13), ")");
    }
    else if (a_char != ')') throwError(getMsg(13), ")");

    // Getting ikeyword
    int an_ikeyword = getIKeyword(skeyword);
    if (an_ikeyword == END_ARGS) throwError(getMsg(15), skeyword.c_str());
    // Getting value type (value_type_e) and object type (object_type_e, if necessary)
    value_type_e  a_vtype = loc_get_vtype(a_type_str);
    object_type_e an_obj_type = (a_vtype == VTYPE_UNKNOWN) ? MV_get_type(a_type_str) : HCDI_OBJ_TYPE_NULL;
    // Inserting the attribute (checking: value or object)
    if (a_vtype != VTYPE_UNKNOWN) {
        descr_p->addValue(domain, a_vtype, an_ikeyword, skeyword, a_comment);
    }
    else if (an_obj_type != HCDI_OBJ_TYPE_NULL) {
        descr_p->addObject(domain, an_obj_type, an_ikeyword, skeyword, a_comment);
    }
    else {
        throwError(getMsg(14), a_type_str.c_str());
    }
    if (a_solver_comment != "")
    {
        descr_p->initAttributeSolverNames(a_solver_comment, an_ikeyword, false);
    }


    if (a_type_str != "MULTIOBJECT")
    {
        if (getNextChar() != ';')
        {
            unreadChar();
            readPossibleSubtypeList(descr_p, an_ikeyword);
        }
        if (getNextChar() == '{')
        {
            
            throwError(getMsg(85), ";");
        }
        unreadChar();
    }
    else
    {
        readPossibleSubtypeList(descr_p, an_ikeyword);
    }
}

void MvDescriptorParser_t::readAttributeSize(MvDomain_e domain, MvDescriptor_t* descr_p, const string& skeyword)
{
    // Parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    string a_comment = getNextQuotedString();
    string a_solver_comment = "";
    char a_char = getNextChar();
    if (a_char == ',')
    {
        a_solver_comment = getNextQuotedString();
        if (getNextChar() != ')') throwError(getMsg(13), ")");
    }
    else if (a_char != ')') throwError(getMsg(13), ")");
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // Getting ikeyword
    int an_ikeyword = getIKeyword(skeyword);
    if (an_ikeyword == END_ARGS) throwError(getMsg(15), skeyword.c_str());
    // All is checked => inserting the attribute
    descr_p->addSize(domain, an_ikeyword, skeyword, a_comment);
    if (a_solver_comment != "")
    {
        descr_p->initAttributeSolverNames(a_solver_comment, an_ikeyword, false);
    }
}

void MvDescriptorParser_t::readAttributeArray(MvDomain_e domain, MvDescriptor_t* descr_p, const string& skeyword)
{
    // Parsing
    if (getNextChar() != '[') throwError(getMsg(13), "[");
    
    //get multidimensional array's sizes, 
    //e.g. From ARRAY[DIM1][DIM2][DIM3]...[DIMX], you'll get DIM1,DIM2,DIM3,...DIMX
    vector<string> size_strings;
    string char_tmp;
    do {
        size_strings.push_back(getNextString());
        char_tmp = getNextChar();
        if (char_tmp != "]") throwError(getMsg(13), "]");
        char_tmp = getNextChar();
        if (char_tmp == "(") break;
        else if (char_tmp != "[") throwError(getMsg(13), "[ or (");
    } while (1);
    //if(char_tmp!="]") throwError(getMsg(13),"]");
    

    //if(getNextChar()!='(') throwError(getMsg(13),"(");
    string a_type_str = getNextString();
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    string a_comment = getNextQuotedString(), a_solver_comment = "";
    char a_char = getNextChar();
    if (a_char == ',')
    {
        a_solver_comment = getNextQuotedString();
        if (getNextChar() != ')') throwError(getMsg(13), ")");
    }
    else if (a_char != ')') throwError(getMsg(13), ")");

    // Getting ikeyword
    int an_ikeyword = getIKeyword(skeyword);
    if (an_ikeyword == END_ARGS) throwError(getMsg(15), skeyword.c_str());

    
    //get size or size's ikeyword
    dimension_size_t size_tmp;
    MvSizeVector sizeArrayVector;
    attribute_type_e an_array_type = ATYPE_UNKNOWN;
    int a_size = END_ARGS;
    bool isDynamicArray = false;
    for (unsigned int i = 0; i < size_strings.size(); i++) {
        // Getting array type (attribute_type_e) and size
        a_size = descr_p->getIKeyword(size_strings[i]);//(a_size_str);
        an_array_type = (a_size == END_ARGS) ? ATYPE_STATIC_ARRAY : ATYPE_DYNAMIC_ARRAY;
        switch (an_array_type) {
        case ATYPE_STATIC_ARRAY:
            try { a_size = loc_string2int(size_strings[i]); }
            catch (MvError_t& err) { throwError(err.GetMessage()); }
            size_tmp.size = a_size;
            size_tmp.isRealSize = 1;//it's real size
            sizeArrayVector.push_back(size_tmp);
            break;
        case ATYPE_DYNAMIC_ARRAY:
            if (descr_p->getAttributeType(a_size) != ATYPE_SIZE) throwError(getMsg(16), size_strings[i].c_str());
            size_tmp.size = a_size;
            size_tmp.isRealSize = 0;//it's size's ikeyword
            sizeArrayVector.push_back(size_tmp);
            isDynamicArray = true;
            break;
        default:
            break;
        }
    }
    an_array_type = isDynamicArray ? ATYPE_DYNAMIC_ARRAY : ATYPE_STATIC_ARRAY;
    

    // Getting value type (value_type_e) and object type (object_type_e, if necessary)
    value_type_e  a_vtype = loc_get_vtype(a_type_str);
    object_type_e an_obj_type = (a_vtype == VTYPE_UNKNOWN) ? MV_get_type(a_type_str) : HCDI_OBJ_TYPE_NULL;
    // Inserting the attribute (checking: value or object)
    if (a_vtype != VTYPE_UNKNOWN) {
        descr_p->addValueArray(domain, a_vtype, an_ikeyword, skeyword, a_comment, an_array_type, sizeArrayVector); 
    }
    else if (an_obj_type != HCDI_OBJ_TYPE_NULL) {
        descr_p->addObjectArray(domain, an_obj_type, an_ikeyword, skeyword, a_comment, an_array_type, sizeArrayVector); 
    }
    else {
        throwError(getMsg(14), a_type_str.c_str());
    }

    if (a_solver_comment != "")
    {
        descr_p->initAttributeSolverNames(a_solver_comment, an_ikeyword, false);
    }
    char c = getNextChar();
    if (c != ';')
    {
        // reading of SUBTYPES
        //  { SUBTYPES = ( /NODE , /GRNOD ) ;  }
        unreadChar();
        int nb_subtypes = 0;
        c = getNextChar();
        if (c != '{')
            throwError(getMsg(13), "{");
        vector<string> vect_subtype;
        string text = getNextString();
        if (text != "SUBTYPES")
            throwError(getMsg(13), "SUBTYPES");
        c = getNextChar();
        if (c != '=')
            throwError(getMsg(13), "=");

        c = getNextChar();
        if (c != '(')
            throwError(getMsg(13), "(");

        c = getNextChar();

        while (c != ')')
        {
            if (c != '/')
                throwError(getMsg(13), "/");

            text = getNextString();
            if (text != "")
                vect_subtype.push_back(text);

            c = getNextChar();

            // subtypes are finished
            if (c == ')')
                break;
            if (c != ',')
                throwError(getMsg(13), ",");

            c = getNextChar();
        }

        c = getNextChar();
        while (c != ';')
        {
            c = getNextChar();
        }
        c = getNextChar();
        if (c != '}')
            throwError(getMsg(13), "}");

        // fill the data
        const descriptor_t* cdescr_p = descr_p->getDescriptorPtr();
        object_descriptor_t* objdescr_p = (object_descriptor_t*)(cdescr_p->attdescr_array[an_ikeyword]);

        nb_subtypes = (int)vect_subtype.size();
        objdescr_p->allowed_types = (obj_type_e*)my_malloc(nb_subtypes, sizeof(obj_type_e));
        objdescr_p->comments = (char**)my_malloc(nb_subtypes, sizeof(char*));
        objdescr_p->subtypes = (char**)my_malloc(nb_subtypes, sizeof(char*));
        objdescr_p->num = nb_subtypes;

        for (int i = 0; i < nb_subtypes; i++)
        {
            string subtype_str = vect_subtype.at(i);
            vector<string> key_vect;
            StringTokenize(subtype_str, key_vect, "/");
            int size = (int)key_vect.size();
            if (!key_vect.empty())
            {
                object_type_e a_type = MV_get_type(key_vect[0]);
                if (a_type == HCDI_OBJ_TYPE_NULL)
                    throwError(getMsg(10), key_vect[0].c_str());

                objdescr_p->comments[i] = (char*)my_malloc((int)(strlen("")), sizeof(char));
                objdescr_p->allowed_types[i] = a_type;

                if (size == 2)
                {
                    objdescr_p->subtypes[i] = (char*)my_malloc((int)(key_vect[1].length()) + 1, sizeof(char));
                    strcat(objdescr_p->subtypes[i], key_vect[1].c_str());
                }
                else
                {
                    objdescr_p->subtypes[i] = (char*)my_malloc((int)(strlen("")), sizeof(char));
                }
            }
        }
    }
}


/* --------- Defaults --------- */


void MvDescriptorParser_t::readDefaults(MvDescriptor_t* descr_p) {
    string       a_skeyword;
    int          a_ikeyword;
    value_type_e a_vtype;
    // Domain
    char       a_char = getNextChar();
    MvDomain_e a_domain = DOM_COMMON;
    if (a_char == '(') {
        a_domain = getNextDomain();
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        a_char = getNextChar();
    }
    // Default values
    if (a_char != '{') throwError(getMsg(13), "{");
    while (getNextChar() != '}') {
        bool a_is_locked = false;
        unreadChar();
        // Getting skeyword
        a_skeyword = getNextString();
        
        if (a_skeyword == "locked") {
            a_is_locked = true;
            a_skeyword = getNextString();
        }
        
        a_ikeyword = descr_p->getIKeyword(a_skeyword);
        if (a_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
        // Getting value type
        a_vtype = descr_p->getValueType(a_ikeyword);
        //
        if (getNextChar() != '=') throwError(getMsg(13), "=");
        // Getting default value
        switch (a_vtype) {
        case VTYPE_BOOL:   descr_p->setBoolDefaultValue(a_ikeyword, a_domain, getNextBool());                 break;
        case VTYPE_INT:    descr_p->setIntDefaultValue(a_ikeyword, a_domain, getNextInt());                   break;
        case VTYPE_UINT:   descr_p->setUIntDefaultValue(a_ikeyword, a_domain, (unsigned int)getNextInt());    break;
        case VTYPE_FLOAT:  descr_p->setFloatDefaultValue(a_ikeyword, a_domain, getNextFloat());               break;
        case VTYPE_STRING: descr_p->setStringDefaultValue(a_ikeyword, a_domain, getNextQuotedString());       break;
        case VTYPE_OBJECT: 
            if(getNextString() != "NONE") throwError(getMsg(19), a_skeyword.c_str());
            descr_p->setObjectDefaultValue(a_ikeyword, a_domain);
            break;
        default:
            throwError("MvDescriptorParser_t::readDefaults -> Unknown value type for \"%s\" ", a_skeyword.c_str());
            break;
        }
        //
        if (a_is_locked) myLockedIKeywords[a_domain].insert(a_ikeyword); 
        if (getNextChar() != ';') throwError(getMsg(13), ";");
    }
}


void MvDescriptorParser_t::readSKeywordsIdentifier(MvDescriptor_t* descr_p) {
    string       a_skeyword;
    int          a_ikeyword;
    //value_type_e a_vtype;
    // Domain
    char       a_char = getNextChar();
    MvDomain_e a_domain = DOM_COMMON;
    if (a_char == '(') {
        a_domain = getNextDomain();
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        a_char = getNextChar();
    }
    // Default values
    if (a_char != '{') throwError(getMsg(13), "{");
    while (getNextChar() != '}') {

        unreadChar();
        // Getting skeyword
        a_skeyword = getNextString();
        

        
        a_ikeyword = descr_p->getIKeyword(a_skeyword);
        if (a_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
        // Getting value type
        //a_vtype=descr_p->getValueType(a_ikeyword);
        //
        if (getNextChar() != '=') throwError(getMsg(13), "=");

        // Getting default value

        int val = getNextInt();
        descr_p->setIntIdentifierValue(a_ikeyword, a_domain, val);


        /*
        switch(a_vtype) {
        case VTYPE_INT:    descr_p->setIntDefaultValue(a_ikeyword,a_domain,getNextInt());             break;
        case VTYPE_FLOAT:  descr_p->setFloatDefaultValue(a_ikeyword,a_domain,getNextFloat());         break;
        case VTYPE_STRING: descr_p->setStringDefaultValue(a_ikeyword,a_domain,getNextQuotedString()); break;
        case VTYPE_OBJECT: throwError(getMsg(19),a_skeyword.c_str());                                  break;
        default:
          throwError("MvDescriptorParser_t::readDefaults -> Unknown value type for \"%s\" ",a_skeyword.c_str());
          break;
        }
        */

        //
        if (getNextChar() != ';') throwError(getMsg(13), ";");
    }
}



/* --------- Tests --------- */

void MvDescriptorParser_t::readTests(MvDescriptor_t* descr_p) {
    char       a_char = getNextChar();
    MvDomain_e a_domain = DOM_COMMON;
    if (a_char == '(') {
        a_domain = getNextDomain();
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        a_char = getNextChar();
    }
    //
    if (a_char != '{') throwError(getMsg(13), "{");
    while (getNextChar() != '}') {
        unreadChar();
        // Getting skeyword
        string a_skeyword = getNextString();
        int a_ikeyword = descr_p->getIKeyword(a_skeyword);
        if (a_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
        // Getting value type
        value_type_e a_vtype = descr_p->getValueType(a_ikeyword);
        // Getting comparator
        string a_comparator = getNextComparator();
        // Getting the value
        bool   a_is_attrib = true; 
        string a_value;          
        switch (a_vtype) {
        case VTYPE_INT:
        case VTYPE_UINT:
        case VTYPE_FLOAT:
        case VTYPE_OBJECT:
            a_value = getNextString();
            break;
        case VTYPE_STRING:
            
        {
            char a_loc_char = getNextChar();
            unreadChar();
            //
            if (a_loc_char == '\"') {
                a_is_attrib = false;
                a_value = getNextQuotedString();
            }
            else {
                a_value = getNextString();
            }
        }
        
        break;
        default:
            throwError("MvDescriptorParser_t::readTests -> Unknown value type for \"%s\" ", a_skeyword.c_str());
            //break;
        }
        if (getNextChar() != ';') throwError(getMsg(13), ";");
        // Adding the test in the descriptor's test list
        
        int a_right_ikw = END_ARGS;
        if (a_is_attrib) a_right_ikw = descr_p->getIKeyword(a_value);
        if (a_right_ikw == END_ARGS) {
            // test (valid value?)
            descr_p->addTest(a_domain, new MvCompareTest_t(a_ikeyword, a_comparator, a_value));
        }
        else {
            value_type_e a_left_vtype = descr_p->getValueType(a_ikeyword);
            value_type_e a_right_vtype = descr_p->getValueType(a_right_ikw);
            if (a_left_vtype != a_right_vtype) throwError(getMsg(78), a_skeyword.c_str(), a_value.c_str());
            descr_p->addTest(a_domain, new MvCompareAttributesTest_t(a_ikeyword, a_comparator, a_right_ikw));
        }
        
    }
}


/* --------- GUI (data features) --------- */

void MvDescriptorParser_t::readFeatures(MvDescriptor_t* descr_p) {
    char       a_char = getNextChar();
    MvDomain_e a_domain = DOM_COMMON;
    if (a_char == '(') {
        a_domain = getNextDomain();
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        a_char = getNextChar();
    }
    //
    bool a_is_graphical = false;
    if (a_char != '{') throwError(getMsg(13), "{");
    while (getNextChar() != '}') {
        MvDataFeature_t* a_df_p = NULL;
        unreadChar();
        string a_feature_type = getNextString();
        if (a_feature_type == "SCALAR")               a_df_p = readScalarFeature(descr_p);
        else if (a_feature_type == "SIZE")            a_df_p = readSizeFeature(descr_p);
        else if (a_feature_type == "SIZE_RADIO")      a_df_p = readSizeRadioFeature(descr_p, a_domain);
        else if (a_feature_type == "FILE")            a_df_p = readFileFeature(descr_p);
        else if (a_feature_type == "DIR")            a_df_p = readDirFeature(descr_p);
        else if (a_feature_type == "TRIPLE")          a_df_p = readTripleFeature(descr_p);
        else if (a_feature_type == "POINT")           a_df_p = readTripleFeature(descr_p, true);
        else if (a_feature_type == "RADIO")           a_df_p = readRadioFeature(descr_p, a_domain);         
        else if (a_feature_type == "RADIO_ARRAY")     a_df_p = readRadioArrayFeature(descr_p);
        else if (a_feature_type == "FLAG")            a_df_p = readFlagFeature(descr_p);
        else if (a_feature_type == "FLAG_LIST")       a_df_p = readFlagListFeature(descr_p);
        else if (a_feature_type == "ARRAY")           a_df_p = readArrayFeature(descr_p, a_domain);         
        
        else if (a_feature_type == "ARRAY_TO_SINGLE") a_df_p = readArrayToSingleFeature(descr_p, a_domain); 
        
        
        else if (a_feature_type == "MATRIX")          a_df_p = readMatrixFeature(descr_p);
        
        else if (a_feature_type == "DATA")            a_df_p = readDataFeature(descr_p, DFT_DATA);
        else if (a_feature_type == "SUBOBJECT")       a_df_p = readDataFeature(descr_p, DFT_SUBOBJECT);
        else if (a_feature_type == "TOOL")            a_df_p = readToolFeature(descr_p);
        else if (a_feature_type == "FUNCTION")        a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION);           
        else if (a_feature_type == "TABLE")           a_df_p = readFunctionFeature(descr_p, FFTY_TABLE);              
        else if (a_feature_type == "DEFINE_FUNCTION") a_df_p = readFunctionFeature(descr_p, FFTY_DEFINE_FUNCTION);
        else if (a_feature_type == "FUNCTION_OR_TABLE") a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION_OR_TABLE); 
        else if (a_feature_type == "FUNCTION_OR_DEFINE_FUNCTION") a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION_OR_DEFINE_FUNCTION);
        else if (a_feature_type == "SUPPORT")         a_df_p = readSupportFeature(descr_p);
        else if (a_feature_type == "APPEND")          a_df_p = readAppendFeature(descr_p);
        else if (a_feature_type == "if")              a_df_p = readIfFeature(descr_p, a_domain);            
        else if (a_feature_type == "SEPARATOR")       a_df_p = readSeparatorFeature();
        else if (a_feature_type == "UNIT")            a_df_p = readUnitFeature();
        else if (a_feature_type == "ASSIGN")          a_df_p = readAssignFeature(descr_p, DFT_ASSIGN);
        else if (a_feature_type == "graphical")       a_is_graphical = true;
        else if (a_feature_type == "optional" || a_feature_type == "mandatory") {
            setOptional(a_feature_type == "optional");
            if (getNextChar() != ':') throwError(getMsg(13), ":");
        }
        else {
            throwError(getMsg(22), a_feature_type.c_str());
        }
        //
        if (a_df_p != NULL) {

            // Adding the feature into the feature list(this is done to store if feature, before default, wrong feature in feature list
            a_df_p->setOptional(isOptional());
            a_df_p->setGraphical(a_is_graphical);
            a_is_graphical = false;
            descr_p->setDataFeature(a_domain, a_df_p);

            // Adding "if" depending features into the descriptor's feature list
            if (a_df_p->getType() == DFT_IF) {
                const MvDataIfFeature_t* a_dif_p = (const MvDataIfFeature_t*)a_df_p;
                MvDataFeatureList_t::const_iterator a_it_begin, a_it_end, a_it;
                // Default right features
                const MvDataFeatureList_t& a_r_dftl = a_dif_p->getDefaultFeaturePtrList();
                a_it_begin = a_r_dftl.begin();
                a_it_end = a_r_dftl.end();
                for (a_it = a_it_begin; a_it != a_it_end; ++a_it) descr_p->setDataFeature(a_domain, *a_it, true);
                // Default wrong features
                const MvDataFeatureList_t& a_w_dftl = a_dif_p->getDefaultWrongFeaturePtrList();
                a_it_begin = a_w_dftl.begin();
                a_it_end = a_w_dftl.end();
                for (a_it = a_it_begin; a_it != a_it_end; ++a_it) descr_p->setDataFeature(a_domain, *a_it, true);
            }

            // Graphical array -> every contained feature must be set graphical

            MvDataFeatureType_e a_df_type = a_df_p->getType();
            if (((a_df_type == DFT_DYNAMIC_ARRAY) || (a_df_type == DFT_STATIC_ARRAY)) && a_df_p->isGraphical()) {
                MvDataArrayFeature_t* a_daf_p = (MvDataArrayFeature_t*)a_df_p;
                int a_nb_features = a_daf_p->getNumber();
                for (int i = 0; i < a_nb_features; ++i) {
                    MvDataFeature_t* a_loc_df_p = const_cast<MvDataFeature_t*>(a_daf_p->getDataFeature(i));
                    a_loc_df_p->setGraphical(true);
                }
            }
        }
    }
    //
    if (a_domain != DOM_COMMON) clearFeatureMap();
}

// SCALAR(tt) {VISIBLE=TRUE;}
// calling readFeaturesAttributes with do_step =false, the '=', ';' must be handled outside!!!
// calling readFeaturesAttributes with do_step =true, the ';' must be handled outside!!!
//readFeaturesAttributes(false, feature_str, map);
void MvDescriptorParser_t::readFeaturesAttributes(bool do_step, string &feature_str, map<string, string> &value_map)
{
    string a_att_type = "";
    if (do_step)
    {
        a_att_type = getNextString();
        if (getNextChar() != '=') throwError(getMsg(13), "=");
    }
    else
    {
        a_att_type = feature_str;
    }
    string value = getNextString();
    value_map[a_att_type] = value;
}

void MvDescriptorParser_t::setFeaturesAttributes(MvDataFeature_t *feature_p, map<string, string> &value_map)
{
    map<string, string>::iterator iter_b = value_map.begin();
    map<string, string>::iterator iter_e = value_map.end();
    map<string, string>::iterator iter;
    for (iter = iter_b; iter != iter_e; ++iter)
    {
        string attrib = iter->first;
        string value = iter->second;
        if (attrib == "EDITABLE")
        {
            feature_p->setEditable((value == "TRUE") ? true: false);
        }
        else if (attrib == "VISIBLE")
        {
            feature_p->setVisible((value == "TRUE") ? true : false);
        }
        else if (attrib == "PARAMETERIZED")
        {
            feature_p->setParameterized((value == "TRUE") ? true : false);
        }
        else if (attrib == "DISPLAY_STATUS")
        {
            if (value == "TRUE")
                feature_p->setDisplayStatus(1);
            else if (value == "ALWAYS_ON")
                feature_p->setDisplayStatus(2);
            else
                feature_p->setDisplayStatus(0);
        }
    }
}


typedef struct LocScalarCondItems_s {
    MvExpression_t* myExprPtr;
    MvDataFeatureSet_t   myTestFeaturePtrSet;
    string               myTitle;
    MvDimension_e        myDimension;
    vector<string>       myArgVect;
} LocScalarCondItems_t;
typedef vector<LocScalarCondItems_t> LocScalarCondList_t;



MvDataFeature_t* MvDescriptorParser_t::readScalarFeature(const MvDescriptor_t* descr_p, bool do_check_ikw) {
    int                  a_ikeyword = END_ARGS;
    string               a_title = "";
    MvDimension_e        a_dimension = UDI_DIMENSIONLESS;
    LocScalarCondList_t  a_cond_list;     
    LocScalarCondItems_t a_default_items; 
    //
    a_default_items.myExprPtr = NULL;              
    a_default_items.myTitle = "";                
    a_default_items.myDimension = UDI_DIMENSIONLESS; 
    vector<string> argVect;
    //
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Checking ikeyword
    a_ikeyword = getNextIKeyword(descr_p);
    string a_skeyword = "";
    a_skeyword = descr_p->getSKeyword(a_ikeyword);
    if (do_check_ikw && isUsed(a_ikeyword)) {
        throwError(getMsg(43), a_skeyword.c_str());
    }
    // Continuing parsing
    char a_char = getNextChar();
    if (a_char == ',') {
        a_title = getNextQuotedString();
        a_char = getNextChar();
        if (a_char == ',') {
            a_dimension = getNextDimension(&argVect);
            a_char = getNextChar();
        }
    }
    else {
        a_title = descr_p->getComment(a_ikeyword);
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // Block checking
    a_char = getNextChar();
    map<string, string> map;
    if (a_char == '{') {
        
        LocScalarCondItems_t a_cond_items;
        string               a_cond_type = "";
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = a_cond_type;
            if (a_feature_keyword == "else if") {
                a_feature_keyword = "if";
                a_cond_type = "";
            }
            else {
                a_feature_keyword = getNextString();
            }
            //
            if (a_cond_type == "" && a_feature_keyword == "if") {
                a_cond_type = "if";
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                a_cond_items.myExprPtr = getNextExpressionPtr(descr_p, true, &(a_cond_items.myTestFeaturePtrSet));
                a_cond_items.myTitle = a_title;
                a_cond_items.myDimension = a_dimension;
                if (getNextChar() != ')') throwError(getMsg(13), ")");
                if (getNextChar() != '{') throwError(getMsg(13), "{");
            }
            else {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                if (a_feature_keyword == "TITLE") {
                    if (a_cond_type == "") a_title = getNextQuotedString();
                    else                a_cond_items.myTitle = getNextQuotedString();
                }
                else if (a_feature_keyword == "DIMENSION") {
                    if (a_cond_type == "") a_dimension = getNextDimension(&argVect);
                    else                a_cond_items.myDimension = getNextDimension(&a_cond_items.myArgVect);
                }
                else
                {
                    readFeaturesAttributes(false, a_feature_keyword, map);
                }
                if (getNextChar() != ';') throwError(getMsg(13), ";");
                //
                if (a_cond_type != "") {
                    if (getNextChar() != '}') {
                        unreadChar();
                    }
                    else {
                        a_cond_list.push_back(a_cond_items);
                        a_cond_items = a_default_items; 
                        //
                        if (a_cond_type == "else") {
                            a_cond_type = "";
                        }
                        else {
                            a_char = getNextChar();
                            unreadChar();
                            if (a_char != '}') {
                                a_cond_type = getNextString();
                                if (a_cond_type != "else") {
                                    int a_size = (int)(a_cond_type.size());
                                    while (a_size--) unreadChar();
                                    a_cond_type = "";
                                }
                                else {
                                    if (getNextChar() != '{') {
                                        unreadChar();
                                        a_cond_type = getNextString();
                                        if (a_cond_type != "if") throwError(getMsg(44));
                                        a_cond_type = "else if";
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    // Creating the feature
    
    MvDataFeature_t* a_feature_p = NULL;
    if (a_cond_list.empty()) {
        a_feature_p = new MvDataUncondScalarFeature_t(a_title, a_ikeyword, a_dimension, &argVect);
    }
    else {
        int a_nb_tests = (int)(a_cond_list.size());
        if (a_cond_list[a_nb_tests - 1].myExprPtr == NULL) --a_nb_tests;
        MvDataCondScalarFeature_t* a_dcsf_p = new MvDataCondScalarFeature_t(a_nb_tests, a_title, a_ikeyword);
        for (int i = 0; i < a_nb_tests; ++i) {
            
            MvDimension_e a_loc_dimension = (a_cond_list[i].myDimension == a_default_items.myDimension ? a_dimension : a_cond_list[i].myDimension);
            string        a_loc_title = (a_cond_list[i].myTitle == a_default_items.myTitle ? a_title : a_cond_list[i].myTitle);
            vector<string> a_loc_arg_vect = (a_cond_list[i].myArgVect == a_default_items.myArgVect ? argVect : a_cond_list[i].myArgVect);
            a_dcsf_p->setConditionalData(i,
                a_cond_list[i].myTestFeaturePtrSet,
                a_cond_list[i].myExprPtr,
                a_loc_dimension,
                a_loc_title,
                a_loc_arg_vect);
            
        }
        int a_nb_data = (int)(a_cond_list.size());
        if (a_nb_tests < a_nb_data) {
            
            MvDimension_e a_loc_dimension = (a_cond_list[a_nb_tests].myDimension == a_default_items.myDimension ? a_dimension : a_cond_list[a_nb_tests].myDimension);
            string        a_loc_title = (a_cond_list[a_nb_tests].myTitle == a_default_items.myTitle ? a_title : a_cond_list[a_nb_tests].myTitle);
            vector<string> a_loc_arg_vect = (a_cond_list[a_nb_tests].myArgVect == a_default_items.myArgVect ? argVect : a_cond_list[a_nb_tests].myArgVect);
            a_dcsf_p->setDefaultData(a_loc_dimension,
                a_loc_title, a_loc_arg_vect);
            
        }
        else {
            a_dcsf_p->setDefaultData(a_dimension, a_title, argVect);
        }
        a_feature_p = a_dcsf_p;
    }
    
    setFeaturesAttributes(a_feature_p, map);
    addFeature(a_ikeyword, a_feature_p);
    return a_feature_p;
}


MvDataFeature_t* MvDescriptorParser_t::readSizeFeature(const MvDescriptor_t* descr_p, bool do_check_ikw) {
    int           an_ikeyword = END_ARGS;
    string        a_title;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Checking ikeyword
    an_ikeyword = getNextIKeyword(descr_p);
    if (do_check_ikw && isUsed(an_ikeyword)) {

        const MvDataFeature_t* feature = getFeaturePtr(an_ikeyword);
        if (feature)
        {
            MvDataFeatureType_e ftype = feature->getType();
            if (ftype != DFT_ASSIGN)
            {
                string a_skeyword = descr_p->getSKeyword(an_ikeyword);
                throwError(getMsg(43), a_skeyword.c_str());
            }
        }

    }
    // Continuing parsing
    char a_char = getNextChar();
    if (a_char == ',') {
        a_title = getNextQuotedString();
        a_char = getNextChar();
    }
    else {
        a_title = descr_p->getComment(an_ikeyword);
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // Block checking
    map<string, string> map;
    a_char = getNextChar();
    if (a_char == '{') {
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            if (getNextChar() != '=') throwError(getMsg(13), "=");
            //
            if (a_feature_keyword == "TITLE") {
                a_title = getNextQuotedString();
            }
            else {
                readFeaturesAttributes(false, a_feature_keyword, map);
            }
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    // Creating the feature
    MvDataSizeFeature_t* a_feature_p = new MvDataSizeFeature_t(a_title, an_ikeyword);
    addFeature(an_ikeyword, (MvDataFeature_t*)a_feature_p);
    addSizeFeature(an_ikeyword, a_feature_p);
    setFeaturesAttributes(a_feature_p, map);
    return (MvDataFeature_t*)a_feature_p;
}

MvDataFeature_t* MvDescriptorParser_t::readSizeRadioFeature(const MvDescriptor_t* descr_p, MvDomain_e domain, bool do_check_ikw) {
    int           an_ikeyword = END_ARGS;
    string        a_title;
    bool is_size_radio_feature = false;
    value_type_e a_vtype = VTYPE_UNKNOWN;
    int nb_radios = 0;
    MvOrientation_e      an_orientation = ORI_COLUMN;
    int                  a_nbr = 0, a_nbc = 0;
    bool                a_do_prefix_value = true;
    bool                a_enum_value_flag = false;
    LocRadioButtonList_t a_rbl;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Checking ikeyword
    string a_skeyword = "";
    an_ikeyword = getNextIKeyword(descr_p, &a_skeyword);
    if (do_check_ikw && isUsed(an_ikeyword)) {
        string a_lskeyword = descr_p->getSKeyword(an_ikeyword);
        throwError(getMsg(43), a_lskeyword.c_str());
    }
    a_vtype = descr_p->getValueType(an_ikeyword);
    // to be seen ???
    if (a_vtype == VTYPE_OBJECT)
        throwError(getMsg(30), a_skeyword.c_str());
    // Continuing parsing
    char a_char = getNextChar();
    if (a_char == ',') {
        a_title = getNextQuotedString();
        a_char = getNextChar();
    }
    else {
        a_title = descr_p->getComment(an_ikeyword);
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // Block checking
    map<string, string> map;
    a_char = getNextChar();
    if (a_char == '{') {
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            string               doprefixval, enumvalueflag;

            if (a_feature_keyword == "TITLE") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                a_title = getNextQuotedString();
            }
            else if (a_feature_keyword == "ORIENTATION") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                an_orientation = getNextOrientation(&a_nbr, &a_nbc);
            }
            else if (a_feature_keyword == "DO_PREFIX_VALUE") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                doprefixval = getNextString();
                if (doprefixval == "FALSE") a_do_prefix_value = false;
            }
            else if (a_feature_keyword == "ENUM_VALUE_FLAG") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                enumvalueflag = getNextString();
                if (enumvalueflag == "TRUE") a_enum_value_flag = true;
            }
            else if (a_feature_keyword == "ADD")
            {
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                // Getting Value
                LocRadioButton_t a_button;
                string a_value_str = "";
                is_size_radio_feature = true;
                if (a_vtype == VTYPE_STRING) a_value_str = getNextQuotedString(); else a_value_str = getNextString();
                try {
                    switch (a_vtype) {
                    case VTYPE_INT:    a_button.myIntValue = loc_string2int(a_value_str);    break;
                    case VTYPE_FLOAT:  a_button.myFloatValue = loc_string2double(a_value_str); break;
                    case VTYPE_STRING: a_button.myStringValue = a_value_str;                    break;
                    default:           break;
                    }
                }
                catch (MvError_t& err) { throwError(err.GetMessage()); }
                if (getNextChar() != ',') throwError(getMsg(13), ",");
                // Getting comment
                a_button.myComment = getNextQuotedString();
                // Getting domain
                a_button.myDomain = domain;
                a_char = getNextChar();
                if (a_char == ',') {
                    a_button.myDomain = getNextDomain();
                    a_char = getNextChar();
                }
                if (a_char != ')') throwError(getMsg(13), ")");
                // Adding the radio button
                a_rbl.push_back(a_button);
                
            }
            else {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                readFeaturesAttributes(false, a_feature_keyword, map);
            }
            if (getNextChar() != ';') throwError(getMsg(13), ";");

            //if(getNextChar()!='=') throwError(getMsg(13),"=");
            //
            //if(a_feature_keyword=="TITLE") {
            //    a_title=getNextQuotedString();
            //} else {
           //     throwError(getMsg(20),a_feature_keyword.c_str());
          //  }
          //  if(getNextChar()!=';') throwError(getMsg(13),";");
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    nb_radios = (int)(a_rbl.size());
#if 0
    if (a_char == '{') {
        long left_bracket_pos = getCurrentPos();
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();

            if (getNextChar() == '(')
            {
                string               a_title, doprefixval;

                is_size_radio_feature = true;
                //
                a_vtype = descr_p->getValueType(an_ikeyword);
                if (a_vtype == VTYPE_OBJECT) throwError(getMsg(30), a_skeyword.c_str());
                if (do_check_ikw && isUsed(an_ikeyword)) throwError(getMsg(43), a_skeyword.c_str());
                // Continuing parsing
                char a_char = getNextChar();
                if (a_char == ',') {
                    a_title = getNextQuotedString();
                    a_char = getNextChar();
                    if (a_char == ',') {
                        an_orientation = getNextOrientation(&a_nbr, &a_nbc);
                        a_char = getNextChar();
                    }
                }
                else {
                    a_title = descr_p->getComment(an_ikeyword);
                }

                FILE* file = getFile();
                fseek(file, left_bracket_pos, SEEK_SET);
                //long line = getCurrentLine();
                //line--;

                //unreadChar();
                //unreadChar();
                //unreadChar();
                //unreadChar();
                //unreadChar();
                //unreadChar();
                //unreadChar();
                // Parsing block
                a_char = getNextChar();
                if (a_char == '{') {
                    while (getNextChar() != '}') {
                        unreadChar();
                        string a_feature_keyword = getNextString();
                        if (a_feature_keyword == "TITLE") {
                            if (getNextChar() != '=') throwError(getMsg(13), "=");
                            a_title = getNextQuotedString();
                        }
                        else if (a_feature_keyword == "ORIENTATION") {
                            if (getNextChar() != '=') throwError(getMsg(13), "=");
                            an_orientation = getNextOrientation(&a_nbr, &a_nbc);
                        }
                        else if (a_feature_keyword == "DO_PREFIX_VALUE") {
                            if (getNextChar() != '=') throwError(getMsg(13), "=");
                            doprefixval = getNextString();
                            if (doprefixval == "FALSE") a_do_prefix_value = false;
                        }
                        else if (a_feature_keyword == "ENUM_VALUE_FLAG") {
                            if (getNextChar() != '=') throwError(getMsg(13), "=");
                            enumvalueflag = getNextString();
                            if (enumvalueflag == "TRUE") a_enum_value_flag = true;
                        }
                        else if (a_feature_keyword == "ADD")
                        {
                            if (getNextChar() != '(') throwError(getMsg(13), "(");
                            // Getting Value
                            LocRadioButton_t a_button;
                            string a_value_str = "";
                            if (a_vtype == VTYPE_STRING) a_value_str = getNextQuotedString(); else a_value_str = getNextString();
                            try {
                                switch (a_vtype) {
                                case VTYPE_INT:    a_button.myIntValue = loc_string2int(a_value_str);    break;
                                case VTYPE_FLOAT:  a_button.myFloatValue = loc_string2double(a_value_str); break;
                                case VTYPE_STRING: a_button.myStringValue = a_value_str;                    break;
                                default:           break;
                                }
                            }
                            catch (MvError_t& err) { throwError(err.GetMessage()); }
                            if (getNextChar() != ',') throwError(getMsg(13), ",");
                            // Getting comment
                            a_button.myComment = getNextQuotedString();
                            // Getting domain
                            a_button.myDomain = domain;
                            char a_char = getNextChar();
                            if (a_char == ',') {
                                a_button.myDomain = getNextDomain();
                                a_char = getNextChar();
                            }
                            if (a_char != ')') throwError(getMsg(13), ")");
                            // Adding the radio button
                            a_rbl.push_back(a_button);
                            
                        }
                        else {
                            throwError(getMsg(20), a_feature_keyword.c_str());
                        }
                        if (getNextChar() != ';') throwError(getMsg(13), ";");
                    }
                }
                else if (a_char != ';') {
                    throwError(getMsg(13), ";");
                }
                nb_radios = (int)(a_rbl.size());
            }
            else if (getNextChar() != '=')
            {
                throwError(getMsg(13), "=");
            }
            //
            if (a_feature_keyword == "TITLE") {
                a_title = getNextQuotedString();
            }
            else {
                throwError(getMsg(20), a_feature_keyword.c_str());
            }
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
#endif
    // Creating the feature
    MvDataFeature_t* a_feature_p = NULL;
    if (is_size_radio_feature)
    {
        a_feature_p = new MvDataSizeRadioFeature_t(a_title, an_ikeyword, a_vtype, nb_radios, an_orientation, a_nbr, a_nbc);
        ((MvDataSizeRadioFeature_t*)a_feature_p)->setDoPrefixValue(a_do_prefix_value);
        ((MvDataSizeRadioFeature_t*)a_feature_p)->setIsSizeRadioFeature(is_size_radio_feature);
        ((MvDataSizeRadioFeature_t*)a_feature_p)->setEnumValueFlag(a_enum_value_flag);
        for (int i = 0; i < nb_radios; ++i)
        {
            switch (a_vtype) {
            case VTYPE_INT:
                ((MvDataSizeRadioFeature_t*)a_feature_p)->setRadio(i, a_rbl[i].myComment, a_rbl[i].myIntValue, a_rbl[i].myDomain);
                break;
            case VTYPE_FLOAT:
                ((MvDataSizeRadioFeature_t*)a_feature_p)->setRadio(i, a_rbl[i].myComment, a_rbl[i].myFloatValue, a_rbl[i].myDomain);
                break;
            case VTYPE_STRING:
                ((MvDataSizeRadioFeature_t*)a_feature_p)->setRadio(i, a_rbl[i].myComment, a_rbl[i].myStringValue, a_rbl[i].myDomain);
                break;
            default:
                break;
            }
        }
    }
    else
    {
        a_feature_p = new MvDataSizeFeature_t(a_title, an_ikeyword);
    }
    addFeature(an_ikeyword, (MvDataFeature_t*)a_feature_p);
    addSizeFeature(an_ikeyword, (MvDataSizeFeature_t*)a_feature_p);
    setFeaturesAttributes(a_feature_p, map);
    return (MvDataFeature_t*)a_feature_p;
}

MvDataFeature_t* MvDescriptorParser_t::readFileFeature(const MvDescriptor_t* descr_p, bool do_check_ikw) {
    int    a_ikeyword = END_ARGS;
    string a_title;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Checking ikeyword
    a_ikeyword = getNextIKeyword(descr_p);
    string a_skeyword = descr_p->getSKeyword(a_ikeyword);
    if (do_check_ikw && isUsed(a_ikeyword))              throwError(getMsg(43), a_skeyword.c_str());
    if (descr_p->getValueType(a_ikeyword) != VTYPE_STRING) throwError(getMsg(58), a_skeyword.c_str());
    // Continuing parsing
    char a_char = getNextChar();
    if (a_char == ',') {
        a_title = getNextQuotedString();
        a_char = getNextChar();
    }
    else {
        a_title = descr_p->getComment(a_ikeyword);
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // block reading
    map<string, string> map;
    a_char = getNextChar();
    if (a_char == '{')
    {
        while (getNextChar() != '}') {
            unreadChar();
            string feature_str = "";
            readFeaturesAttributes(true, feature_str, map);
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') throwError(getMsg(13), ";");
    // Creating the feature
    MvDataFeature_t* a_feature_p = (MvDataFeature_t*)(new MvDataFileFeature_t(a_title, a_ikeyword));
    setFeaturesAttributes(a_feature_p, map);
    addFeature(a_ikeyword, a_feature_p);
    return a_feature_p;
}

MvDataFeature_t* MvDescriptorParser_t::readDirFeature(const MvDescriptor_t* descr_p, bool do_check_ikw) {
    int    a_ikeyword = END_ARGS;
    string a_title;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Checking ikeyword
    a_ikeyword = getNextIKeyword(descr_p);
    string a_skeyword = descr_p->getSKeyword(a_ikeyword);
    if (do_check_ikw && isUsed(a_ikeyword))              throwError(getMsg(43), a_skeyword.c_str());
    if (descr_p->getValueType(a_ikeyword) != VTYPE_STRING) throwError(getMsg(58), a_skeyword.c_str());
    // Continuing parsing
    char a_char = getNextChar();
    if (a_char == ',') {
        a_title = getNextQuotedString();
        a_char = getNextChar();
    }
    else {
        a_title = descr_p->getComment(a_ikeyword);
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // block reading
    map<string, string> map;
    a_char = getNextChar();
    if (a_char == '{')
    {
        while (getNextChar() != '}') {
            unreadChar();
            string feature_str = "";
            readFeaturesAttributes(true, feature_str, map);
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') throwError(getMsg(13), ";");
    // Creating the feature
    MvDataFeature_t* a_feature_p = (MvDataFeature_t*)(new MvDataDirFeature_t(a_title, a_ikeyword));
    setFeaturesAttributes(a_feature_p, map);
    addFeature(a_ikeyword, a_feature_p);
    return a_feature_p;
}

MvDataFeature_t* MvDescriptorParser_t::readTripleFeature(const MvDescriptor_t* descr_p, bool is_point) {
    int           an_ikw;
    string        a_ikw_title;
    string        a_title;
    MvDimension_e a_dimension = UDI_DIMENSIONLESS;
    MvDataTripleFeatureType_e a_type = DTP_TYPE_BASE;
    vector<string> argVect;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Getting ikeyword
    an_ikw = getNextIKeyword(descr_p);
    bool a_multi = descr_p->isMultiDimensionalArray(an_ikw);
    if (!a_multi)
    {
        int size_ikw = descr_p->getSizeIKeyword(an_ikw);
        if (size_ikw > 0)
            throwError(getMsg(90));
        else
        {
            int a_size = descr_p->getSize(an_ikw);
            if (a_size != 3)
                throwError(getMsg(89));
        }
    }
    else {
        MvSizeVector sizeArrayVector;
        descr_p->getDimensionSize(an_ikw, sizeArrayVector);
        size_t array_dimension = sizeArrayVector.size();
        if (array_dimension > 2)
            throwError(getMsg(91));
        bool is_real_size = sizeArrayVector[array_dimension - 1].isRealSize;
        if (!is_real_size)
            throwError(getMsg(90));
        else
        {
            int size = sizeArrayVector[array_dimension - 1].size;
            if (size != 3)
                throwError(getMsg(89));
        }
    }

    a_ikw_title = descr_p->getComment(an_ikw);
    char a_char = getNextChar();
    // Getting title
    if (a_char == ',') {
        a_title = getNextQuotedString();
        // Getting dimension
        a_char = getNextChar();
        if (a_char == ',') {
            a_dimension = getNextDimension(&argVect);
            a_char = getNextChar();
            if (a_char == ',') {
                a_type = getNextDifferentiatorType();
                a_char = getNextChar();
            }
        }
    }
    // End of parenthesis
    if (a_char != ')') throwError(getMsg(13), ")");
    // Block checking
    map<string, string> map;
    a_char = getNextChar();
    if (a_char == '{') {
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            string a_vector_field = "NO_FIELD";
            a_char = getNextChar();
            if (a_feature_keyword == "TITLE" && a_char == '(') {
                a_vector_field = getNextString();
                if (getNextChar() != ')') throwError(getMsg(13), ")");
                a_char = getNextChar();
            }
            if (a_char != '=') throwError(getMsg(13), "=");
            //
            if (a_feature_keyword == "TITLE") {
                if (a_vector_field == "NO_FIELD") {
                    a_title = getNextQuotedString();
                }
                else {
                    int an_ikeyword = descr_p->getIKeyword(a_vector_field);
                    int a_field =
                        (an_ikeyword == an_ikw) ? 0 : -1;
                    if (a_field == -1) throwError(getMsg(20), a_vector_field.c_str());
                    a_ikw_title = getNextQuotedString();
                }
            }
            else if (a_feature_keyword == "DIMENSION") {
                a_dimension = getNextDimension(&argVect);
            }
            else if (a_feature_keyword == "TYPE") {
                a_type = getNextDifferentiatorType();
            }
            else {
                readFeaturesAttributes(false, a_feature_keyword, map);
            }
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    // Creating the feature
    MvDataFeature_t* df_p = NULL;
    if (is_point)
    {
        df_p = new MvDataPointFeature_t(a_title, an_ikw, a_ikw_title, a_dimension, &argVect, a_type);
    }
    else
    {
        df_p = new MvDataTripleFeature_t(a_title, an_ikw, a_ikw_title, a_dimension, &argVect, a_type);
    }
    setFeaturesAttributes(df_p, map);
    return df_p;
}




MvDataFeature_t* MvDescriptorParser_t::readRadioFeature(const MvDescriptor_t* descr_p, MvDomain_e domain, bool do_check_ikw) {
    
    int                  an_ikeyword = END_ARGS;
    string               a_title, doprefixval, enumvalueflag;
    MvOrientation_e      an_orientation = ORI_COLUMN;
    int                  a_nbr = 0, a_nbc = 0;
    bool                a_do_prefix_value = true;
    bool                a_enum_value_flag = false;
    LocRadioButtonList_t a_rbl; 
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Checking ikeyword
    string a_skeyword;
    an_ikeyword = getNextIKeyword(descr_p, &a_skeyword);
    if (an_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
    //
    value_type_e a_vtype = descr_p->getValueType(an_ikeyword);
    if (a_vtype == VTYPE_OBJECT) throwError(getMsg(30), a_skeyword.c_str());
    if (do_check_ikw && isUsed(an_ikeyword)) throwError(getMsg(43), a_skeyword.c_str());
    // Continuing parsing
    char a_char = getNextChar();
    if (a_char == ',') {
        a_title = getNextQuotedString();
        a_char = getNextChar();
        if (a_char == ',') {
            an_orientation = getNextOrientation(&a_nbr, &a_nbc);
            a_char = getNextChar();
        }
    }
    else {
        a_title = descr_p->getComment(an_ikeyword);
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // Parsing block
    a_char = getNextChar();
    if (a_char == '{') {
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            if (a_feature_keyword == "TITLE") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                a_title = getNextQuotedString();
            }
            else if (a_feature_keyword == "ORIENTATION") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                an_orientation = getNextOrientation(&a_nbr, &a_nbc);
            }
            else if (a_feature_keyword == "DO_PREFIX_VALUE") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                doprefixval = getNextString();
                if (doprefixval == "FALSE") a_do_prefix_value = false;
            }
            else if (a_feature_keyword == "ENUM_VALUE_FLAG") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                enumvalueflag = getNextString();
                if (enumvalueflag == "TRUE") a_enum_value_flag = true;
            }
            else if (a_feature_keyword == "ADD") {
                
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                // Getting Value
                LocRadioButton_t a_button;
                string a_value_str = "";
                if (a_vtype == VTYPE_STRING) a_value_str = getNextQuotedString(); else a_value_str = getNextString();
                try {
                    switch (a_vtype) {
                    case VTYPE_INT:    a_button.myIntValue = loc_string2int(a_value_str);    break;
                    case VTYPE_UINT:   a_button.myUIntValue = (unsigned int)loc_string2int(a_value_str);    break;
                    case VTYPE_FLOAT:  a_button.myFloatValue = loc_string2double(a_value_str); break;
                    case VTYPE_STRING: a_button.myStringValue = a_value_str;                    break;
                    default:           break;
                    }
                }
                catch (MvError_t& err) { throwError(err.GetMessage()); }
                if (getNextChar() != ',') throwError(getMsg(13), ",");
                // Getting comment
                a_button.myComment = getNextQuotedString();
                // Getting domain
                a_button.myDomain = domain;
                a_char = getNextChar();
                if (a_char == ',') {
                    a_button.myDomain = getNextDomain();
                    a_char = getNextChar();
                }
                if (a_char != ')') throwError(getMsg(13), ")");
                // Adding the radio button
                a_rbl.push_back(a_button);
                
            }
            else {
                throwError(getMsg(20), a_feature_keyword.c_str());
            }
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    /* Creating */
    // Creating the feature
    int nb_radios = (int)(a_rbl.size());
    MvDataRadioFeature_t* a_feature_p = new MvDataRadioFeature_t(a_title, an_ikeyword, a_vtype, nb_radios,
        an_orientation, a_nbr, a_nbc);
    a_feature_p->setDoPrefixValue(a_do_prefix_value);
    a_feature_p->setEnumValueFlag(a_enum_value_flag);

    
    for (int i = 0; i < nb_radios; ++i) switch (a_vtype) {
    case VTYPE_INT:    a_feature_p->setRadio(i, a_rbl[i].myComment, a_rbl[i].myIntValue, a_rbl[i].myDomain);    break;
    case VTYPE_UINT:   a_feature_p->setRadio(i, a_rbl[i].myComment, a_rbl[i].myUIntValue, a_rbl[i].myDomain);    break;
    case VTYPE_FLOAT:  a_feature_p->setRadio(i, a_rbl[i].myComment, a_rbl[i].myFloatValue, a_rbl[i].myDomain);  break;
    case VTYPE_STRING: a_feature_p->setRadio(i, a_rbl[i].myComment, a_rbl[i].myStringValue, a_rbl[i].myDomain); break;
    default:           break;
    }
    
    // Block checking
    if (getNextChar() == '{')
    {
        map<string, string> map;
        while (getNextChar() != '}') {
            unreadChar();
            string feature_str = "";
            readFeaturesAttributes(true, feature_str, map);
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }

        setFeaturesAttributes(a_feature_p, map);
    }
    else
    {
        unreadChar();
    }
    addFeature(an_ikeyword, a_feature_p);
    //
    return (MvDataFeature_t*)a_feature_p;
}


MvDataFeature_t* MvDescriptorParser_t::readRadioArrayFeature(const MvDescriptor_t* descr_p, bool do_check_ikw) {
    int                  a_size, an_ikeyword = END_ARGS;
    string               a_title, * a_title_array = NULL; /* PM:0324:01/06/2006 */
    LocRadioButtonList_t a_rbl; 
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Checking ikeyword
    string a_skeyword;
    an_ikeyword = getNextIKeyword(descr_p, &a_skeyword);
    if (an_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
    if (descr_p->getValueType(an_ikeyword) != VTYPE_INT) throwError(getMsg(30), a_skeyword.c_str());
    if (descr_p->getAttributeType(an_ikeyword) != ATYPE_STATIC_ARRAY) throwError(getMsg(32), a_skeyword.c_str());
    if (do_check_ikw && isUsed(an_ikeyword)) throwError(getMsg(43), a_skeyword.c_str());
    // Continuing parsing
    char a_char = getNextChar();
    if (a_char == ',') {
        a_size = getNextInt();
        if (a_size > descr_p->getSize(an_ikeyword)) throwError(getMsg(33), a_skeyword.c_str(), a_size);
        a_title_array = new string[a_size];
        for (int i = 0; i < a_size; ++i) a_title_array[i] = "";
        a_char = getNextChar();
        //
        if (a_char == ',') {
            a_title = getNextQuotedString();
            a_char = getNextChar();
        }
        else {
            a_title = descr_p->getComment(an_ikeyword);
        }
    }
    else {
        a_size = descr_p->getSize(an_ikeyword);
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // Parsing block
    map<string, string> map;
    a_char = getNextChar();
    if (a_char == '{') {
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            if (a_feature_keyword == "TITLE") {
                a_char = getNextChar();
                if (a_char == '[') {
                    int ind = getNextInt();
                    if (ind < 0 || ind >= a_size) throwError(getMsg(26), ind);
                    if (getNextChar() != ']') throwError(getMsg(13), "]");
                    if (getNextChar() != '=') throwError(getMsg(13), "=");
                    a_title_array[ind] = getNextQuotedString();
                }
                else if (a_char == '=') {
                    string a_last_title = a_title;
                    a_title = getNextQuotedString();
                    for (int i = 0; i < a_size; ++i) {
                        if (a_title_array[i] == "" || a_title_array[i] == a_last_title) a_title_array[i] = a_title;
                    }
                }
                else {
                    throwError(getMsg(13), "=");
                }
            }
            else if (a_feature_keyword == "ADD") {
                
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                // Getting Value
                LocRadioButton_t a_button;
                string a_value_str = getNextString();
                try { a_button.myIntValue = loc_string2int(a_value_str); }
                catch (MvError_t& err) { throwError(err.GetMessage()); } 
                if (getNextChar() != ',') throwError(getMsg(13), ",");
                // Getting comment
                a_button.myComment = getNextQuotedString();
                if (getNextChar() != ')') throwError(getMsg(13), ")");
                // Adding the pair
                a_rbl.push_back(a_button);
                
            }
            else {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                readFeaturesAttributes(false, a_feature_keyword, map);
            }
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    /* Creating */
    int i, a_nbr = (int)(a_rbl.size());
    MvDataRadioArrayFeature_t* a_feature_p = new MvDataRadioArrayFeature_t(a_title, an_ikeyword, a_nbr, a_size);
    for (i = 0; i < a_nbr; ++i) a_feature_p->setRadio(i, a_rbl[i].myComment, a_rbl[i].myIntValue); 
    for (i = 0; i < a_size; ++i) a_feature_p->setTitle(i, a_title_array[i]);

    setFeaturesAttributes(a_feature_p, map);
    addFeature(an_ikeyword, a_feature_p);
    delete[] a_title_array;
    //
    return (MvDataFeature_t*)a_feature_p;
}

MvDataFeature_t* MvDescriptorParser_t::readFlagFeature(const MvDescriptor_t* descr_p, bool do_check_ikw) {
    int    an_ikeyword = END_ARGS;
    string a_title;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Checking ikeyword
    string a_skeyword;
    an_ikeyword = getNextIKeyword(descr_p, &a_skeyword);
    if (an_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
    if (descr_p->getValueType(an_ikeyword) != VTYPE_INT && descr_p->getValueType(an_ikeyword) != VTYPE_BOOL) throwError(getMsg(31), a_skeyword.c_str());
    if (do_check_ikw && isUsed(an_ikeyword)) throwError(getMsg(43), a_skeyword.c_str());
    // Continuing parsing
    char a_char = getNextChar();
    if (a_char == ',') {
        a_title = getNextQuotedString();
        a_char = getNextChar();
    }
    else {
        a_title = descr_p->getComment(an_ikeyword);
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // Parsing block
    map<string, string> map;
    a_char = getNextChar();
    if (a_char == '{') {
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            if (getNextChar() != '=') throwError(getMsg(13), "=");
            if (a_feature_keyword == "TITLE") {
                a_title = getNextQuotedString();
            }
            else {
                readFeaturesAttributes(false, a_feature_keyword, map);
            }
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    /* Creating */
    MvDataFeature_t* a_feature_p = (MvDataFeature_t*)(new MvDataFlagFeature_t(a_title, an_ikeyword));
    setFeaturesAttributes(a_feature_p, map);
    addFeature(an_ikeyword, a_feature_p);
    //
    return a_feature_p;
}

typedef pair<int, string>     MvFlagPair_t;
typedef vector<MvFlagPair_t> MvFlagPairList_t;

MvDataFeature_t* MvDescriptorParser_t::readFlagListFeature(const MvDescriptor_t* descr_p, bool do_check_ikw) {
    string           a_title = "";
    MvOrientation_e  an_orientation = ORI_COLUMN;
    int              a_nbr = 0, a_nbc = 0;
    MvFlagPairList_t a_fpl;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    char a_char = getNextChar();
    if (a_char != ')') {
        unreadChar();
        a_title = getNextQuotedString();
        a_char = getNextChar();
        if (a_char == ',') {
            an_orientation = getNextOrientation(&a_nbr, &a_nbc);
            a_char = getNextChar();
        }
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // Parsing block
    map<string, string> map;
    a_char = getNextChar();
    if (a_char == '{') {
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            if (a_feature_keyword == "TITLE") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                a_title = getNextQuotedString();
            }
            else if (a_feature_keyword == "ORIENTATION") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                an_orientation = getNextOrientation(&a_nbr, &a_nbc);
            }
            else if (a_feature_keyword == "ADD") {
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                // Getting Value
                string a_skeyword;
                int    an_ikeyword = getNextIKeyword(descr_p, &a_skeyword);
                if (an_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
                if (descr_p->getValueType(an_ikeyword) != VTYPE_INT) throwError(getMsg(31), a_skeyword.c_str());
                if (do_check_ikw && isUsed(an_ikeyword)) throwError(getMsg(43), a_skeyword.c_str());
                // Getting comment
                string a_comment;
                a_char = getNextChar();
                if (a_char != ',') {
                    a_comment = descr_p->getComment(an_ikeyword);
                }
                else {
                    a_comment = getNextQuotedString();
                    a_char = getNextChar();
                }
                if (a_char != ')') throwError(getMsg(13), ")");
                // Adding the pair
                a_fpl.push_back(MvFlagPair_t(an_ikeyword, a_comment));
            }
            else {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                readFeaturesAttributes(false, a_feature_keyword, map);
            }
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    /* Creating */
    // Creating the feature
    int a_nbf = (int)(a_fpl.size());
    MvDataFlagListFeature_t* a_feature_p = new MvDataFlagListFeature_t(a_title, a_nbf, an_orientation, a_nbr, a_nbc);
    for (int i = 0; i < a_nbf; ++i) {
        a_feature_p->setFlag(i, a_fpl[i].first, a_fpl[i].second);
        addFeature(a_fpl[i].first, a_feature_p);
    }
    setFeaturesAttributes(a_feature_p, map);
    return (MvDataFeature_t*)a_feature_p;
}


MvDataFeature_t* MvDescriptorParser_t::readArrayFeature(const MvDescriptor_t* descr_p, MvDomain_e domain) {
    
    int                  a_size = 0;
    MvDataSizeFeature_t* a_size_p = NULL;
    string               a_title, a_feature_type_str = "", * a_title_array = NULL; /* PM:0324:01/06/2006 */
    bool                 a_is_dynamic = true;
    MvDataFeatureList_t  a_dfl;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Getting size or size keyword
    string a_skeyword;
    a_size = getNextIKeyword(descr_p, &a_skeyword);
    if (a_size == END_ARGS) {
        try { a_size = loc_string2int(a_skeyword); }
        catch (MvError_t&) { throwError(getMsg(20), a_skeyword.c_str()); }
        a_is_dynamic = false;
        a_title_array = new string[a_size];
        for (int i = 0; i < a_size; ++i) a_title_array[i] = "";
    }
    // Checking
    if (a_is_dynamic) {
        if (descr_p->getAttributeType(a_size) != ATYPE_SIZE) {
            throwError(getMsg(16), (descr_p->getSKeyword(a_size)).c_str());
        }
        a_size_p = getSizeFeaturePtr(a_size);
        if (a_size_p == NULL) throwError(getMsg(46), (descr_p->getSKeyword(a_size)).c_str());
    }
    // Continuing parsing
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    a_title = getNextQuotedString();
    if (!a_is_dynamic) for (int i = 0; i < a_size; ++i) a_title_array[i] = a_title;
    if (getNextChar() != ')') throwError(getMsg(13), ")");
    // Block checking
    bool a_is_optional = isOptional();
    char a_char = getNextChar();
    if (a_char == '{') {
        bool a_is_graphical = false;
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            if (a_feature_keyword == "TITLE") {
                a_char = getNextChar();
                if (!a_is_dynamic && a_char == '[') {
                    int ind = getNextInt();
                    if (ind < 0 || ind >= a_size) throwError(getMsg(26), ind);
                    if (getNextChar() != ']') throwError(getMsg(13), "]");
                    if (getNextChar() != '=') throwError(getMsg(13), "=");
                    a_title_array[ind] = getNextQuotedString();
                }
                else if (a_char == '=') {
                    string a_last_title = a_title;
                    a_title = getNextQuotedString();
                    for (int i = 0; i < a_size; ++i) {
                        if (a_title_array[i] == "" || a_title_array[i] == a_last_title) a_title_array[i] = a_title;
                    }
                }
                else {
                    throwError(getMsg(13), "=");
                }
                if (getNextChar() != ';') throwError(getMsg(13), ";");
            }
            else {
                /*
                if(a_feature_type_str!="" && a_feature_keyword!=a_feature_type_str) {
                  if(a_feature_keyword!="optional" && a_feature_keyword!="mandatory" && a_feature_keyword!="graphical") {
                    throwError(getMsg(41),a_feature_keyword.c_str(),a_feature_type_str.c_str());
                  }
                }
                */
                //
                MvDataFeature_t* a_df_p = NULL;
                //
                if (a_feature_keyword == "SCALAR")         a_df_p = readScalarFeature(descr_p);
                else if (a_feature_keyword == "FILE")      a_df_p = readFileFeature(descr_p);
                else if (a_feature_keyword == "DIR")       a_df_p = readDirFeature(descr_p);
                else if (a_feature_keyword == "TRIPLE")    a_df_p = readTripleFeature(descr_p);
                else if (a_feature_keyword == "POINT")     a_df_p = readTripleFeature(descr_p,true);
                else if (a_feature_keyword == "RADIO")     a_df_p = readRadioFeature(descr_p, domain); 
                else if (a_feature_keyword == "FLAG")      a_df_p = readFlagFeature(descr_p);
                else if (a_feature_keyword == "FLAG_LIST") a_df_p = readFlagListFeature(descr_p);
                else if (a_feature_keyword == "DATA")      a_df_p = readDataFeature(descr_p, DFT_DATA);
                else if (a_feature_keyword == "SUBOBJECT") a_df_p = readDataFeature(descr_p, DFT_SUBOBJECT);
                else if (a_feature_keyword == "TOOL")      a_df_p = readToolFeature(descr_p);
                else if (a_feature_keyword == "FUNCTION")          a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION);         
                else if (a_feature_keyword == "TABLE")             a_df_p = readFunctionFeature(descr_p, FFTY_TABLE);            
                else if (a_feature_keyword == "DEFINE_FUNCTION")   a_df_p = readFunctionFeature(descr_p, FFTY_DEFINE_FUNCTION);
                else if (a_feature_keyword == "FUNCTION_OR_TABLE") a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION_OR_TABLE); 
                else if (a_feature_keyword == "FUNCTION_OR_DEFINE_FUNCTION") a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION_OR_DEFINE_FUNCTION);
                else if (a_feature_keyword == "SUPPORT")   a_df_p = readSupportFeature(descr_p);
                else if (a_feature_keyword == "ASSIGN")   a_df_p = readAssignFeature(descr_p, DFT_ASSIGN);
                else if (a_feature_keyword == "ARRAY") a_df_p = readArrayFeature(descr_p, domain);
                else if (a_feature_keyword == "graphical") a_is_graphical = true;
                else if (a_feature_keyword == "optional" || a_feature_keyword == "mandatory") {
                    setOptional(a_feature_keyword == "optional");
                    if (getNextChar() != ':') throwError(getMsg(13), ":");
                }
                else if (a_feature_keyword == "if") {
                    a_df_p = readIfFeature(descr_p, domain);
                }
                else {
                    throwError(getMsg(22), a_feature_keyword.c_str());
                }
                //
                if (a_df_p != NULL) {
                    a_df_p->setOptional(isOptional());
                    a_df_p->setGraphical(a_is_graphical);
                    a_is_graphical = false;
                    a_dfl.push_back(a_df_p);
                }
                //
                if (a_feature_type_str == "" &&
                    a_feature_keyword != "optional" && a_feature_keyword != "mandatory" &&
                    a_feature_keyword != "graphical") {
                    a_feature_type_str = a_feature_keyword;
                }
            }
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    setOptional(a_is_optional);
    /* Creating */
    // Creating the feature
    int nb_features = (int)(a_dfl.size());
    MvDataDynamicArrayFeature_t* a_daf_p = NULL;
    if (a_is_dynamic) {
        bool value = a_size_p->getIsSizeRadioFeature();
        if (value)
        {
            MvDataDynamicArraySizeRadioFeature_t *a_dafs_p = new MvDataDynamicArraySizeRadioFeature_t(a_title, nb_features, a_size);
            a_daf_p = (MvDataDynamicArrayFeature_t *)a_dafs_p;
            ((MvDataSizeRadioFeature_t *)a_size_p)->addArray(a_dafs_p);
        }
        else
        {
            a_daf_p = new MvDataDynamicArrayFeature_t(a_title, nb_features, a_size);
            a_size_p->addArray(a_daf_p);
        }
    }
    else {
        MvDataStaticArrayFeature_t* a_saf_p = new MvDataStaticArrayFeature_t(a_title, nb_features, a_size);
        a_daf_p = (MvDataDynamicArrayFeature_t*)a_saf_p;
        for (int i = 0; i < a_size; ++i) a_saf_p->setTitle(i, a_title_array[i]);
        delete[] a_title_array;
    }
    int i;
    MvDataFeatureList_t::const_iterator it;
    MvDataFeatureList_t::const_iterator it_begin = a_dfl.begin();
    MvDataFeatureList_t::const_iterator it_end = a_dfl.end();
    for (it = it_begin, i = 0; it != it_end; ++it, ++i) a_daf_p->setDataFeature(i, *it);

    set<value_type_e> vtype_set;
    vtype_set.insert(VTYPE_STRING);
    vtype_set.insert(VTYPE_OBJECT);
    descr_p->UpdateFeatureAttributes(a_daf_p, FEATURE_ATTRIBUTE_PARAMETERIZED, 0, &vtype_set);
    //
    return (MvDataFeature_t*)a_daf_p;
}



MvDataFeature_t* MvDescriptorParser_t::readArrayToSingleFeature(const MvDescriptor_t* descr_p, MvDomain_e domain) {
    
    string a_title, a_feature_type_str = "";
    int    a_index = 0;
    MvDataFeatureList_t  a_dfl;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    a_index = getNextInt();
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    a_title = getNextQuotedString();
    if (getNextChar() != ')') throwError(getMsg(13), ")");
    // Block checking
    bool a_is_optional = isOptional();
    char a_char = getNextChar();
    if (a_char == '{') {
        bool a_is_graphical = false;
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            //
            MvDataFeature_t* a_df_p = NULL;
            if (a_feature_keyword == "SCALAR")         a_df_p = readScalarFeature(descr_p);
            else if (a_feature_keyword == "FILE")      a_df_p = readFileFeature(descr_p);
            else if (a_feature_keyword == "DIR")      a_df_p = readDirFeature(descr_p);
            else if (a_feature_keyword == "TRIPLE")    a_df_p = readTripleFeature(descr_p);
            else if (a_feature_keyword == "POINT")     a_df_p = readTripleFeature(descr_p, true);
            else if (a_feature_keyword == "RADIO")     a_df_p = readRadioFeature(descr_p, domain); 
            else if (a_feature_keyword == "FLAG")      a_df_p = readFlagFeature(descr_p);
            else if (a_feature_keyword == "FLAG_LIST") a_df_p = readFlagListFeature(descr_p);
            else if (a_feature_keyword == "DATA")      a_df_p = readDataFeature(descr_p, DFT_DATA);
            else if (a_feature_keyword == "SUBOBJECT") a_df_p = readDataFeature(descr_p, DFT_SUBOBJECT);
            else if (a_feature_keyword == "TOOL")      a_df_p = readToolFeature(descr_p);
            else if (a_feature_keyword == "FUNCTION")          a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION);         
            else if (a_feature_keyword == "TABLE")             a_df_p = readFunctionFeature(descr_p, FFTY_TABLE);            
            else if (a_feature_keyword == "DEFINE_FUNCTION")   a_df_p = readFunctionFeature(descr_p, FFTY_DEFINE_FUNCTION);
            else if (a_feature_keyword == "FUNCTION_OR_TABLE") a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION_OR_TABLE); 
            else if (a_feature_keyword == "FUNCTION_OR_DEFINE_FUNCTION") a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION_OR_DEFINE_FUNCTION);
            else if (a_feature_keyword == "SUPPORT")   a_df_p = readSupportFeature(descr_p);
            else if (a_feature_keyword == "ASSIGN")      a_df_p = readAssignFeature(descr_p, DFT_ASSIGN);
            else if (a_feature_keyword == "graphical") a_is_graphical = true;
            else if (a_feature_keyword == "optional" || a_feature_keyword == "mandatory") {
                setOptional(a_feature_keyword == "optional");
                if (getNextChar() != ':') throwError(getMsg(13), ":");
            }
            else {
                throwError(getMsg(22), a_feature_keyword.c_str());
            }
            //
            if (a_df_p != NULL) {
                a_df_p->setOptional(isOptional());
                a_df_p->setGraphical(a_is_graphical);
                a_is_graphical = false;
                a_dfl.push_back(a_df_p);
            }
            //
            if (a_feature_type_str == "" &&
                a_feature_keyword != "optional" && a_feature_keyword != "mandatory" &&
                a_feature_keyword != "graphical") {
                a_feature_type_str = a_feature_keyword;
            }
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    setOptional(a_is_optional);
    /* Creating */
    // Creating the feature
    int nb_features = (int)(a_dfl.size());
    MvDataArrayToSingleFeature_t* a_da2sf_p = new MvDataArrayToSingleFeature_t(a_title, nb_features, a_index);
    int i;
    MvDataFeatureList_t::const_iterator it_begin = a_dfl.begin();
    MvDataFeatureList_t::const_iterator it_end = a_dfl.end();
    MvDataFeatureList_t::const_iterator it;
    for (it = it_begin, i = 0; it != it_end; ++it, ++i) a_da2sf_p->setDataFeature(i, *it);

    set<value_type_e> vtype_set;
    vtype_set.insert(VTYPE_STRING);
    vtype_set.insert(VTYPE_OBJECT);
    descr_p->UpdateFeatureAttributes(a_da2sf_p, FEATURE_ATTRIBUTE_PARAMETERIZED, 0, &vtype_set);
    //
    return (MvDataFeature_t*)a_da2sf_p;
}



MvDataFeature_t* MvDescriptorParser_t::readMatrixFeature(const MvDescriptor_t* descr_p) {
    typedef vector<int>                     LocIKeywordList_t;
    typedef vector<const MvDataFeature_t*> LocDataFeatureList_t;
    LocIKeywordList_t    a_dim_ikws; a_dim_ikws.reserve(5);
    LocDataFeatureList_t a_features; a_features.reserve(5);
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Getting size or size keyword
    int a_size_ikw = getNextIKeyword(descr_p);
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    string a_title = getNextQuotedString();
    //
    char a_char = getNextChar();
    while (a_char == ',') {
        int a_dim_ikw = getNextIKeyword(descr_p);
        a_dim_ikws.push_back(a_dim_ikw);
        a_char = getNextChar();
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    if (getNextChar() != '{') throwError(getMsg(13), "{");
    a_char = getNextChar();
    // block reading
    map<string, string> map;
    while (a_char != '}') {
        unreadChar();
        //
        string a_feature_keyword = getNextString();
        //
        MvDataFeature_t* a_df_p = NULL;
        if (a_feature_keyword == "SCALAR") a_df_p = readScalarFeature(descr_p);
        else
        {
            if (getNextChar() != '=') throwError(getMsg(13), "=");
            readFeaturesAttributes(false, a_feature_keyword, map);
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
        a_features.push_back(a_df_p);
        //
        a_char = getNextChar();
    }
    /* Creating */
    // Creating the feature
    int a_nb_features = (int)(a_features.size());
    int a_nb_dims = (int)(a_dim_ikws.size());
    MvDataDynamicMatrixFeature_t* a_dmf_p = NULL;
    a_dmf_p = new MvDataDynamicMatrixFeature_t(a_title, a_nb_features, a_size_ikw, a_nb_dims);
    //
    int i;
    for (i = 0; i < a_nb_features; ++i) a_dmf_p->setDataFeature(i, a_features[i]);
    for (i = 0; i < a_nb_dims; ++i)     a_dmf_p->setDimensionIKeyword(i, a_dim_ikws[i]);
    //
    setFeaturesAttributes((MvDataFeature_t*)a_dmf_p, map);
    return (MvDataFeature_t*)a_dmf_p;
}


MvDataFeature_t* MvDescriptorParser_t::readDataFeature(const MvDescriptor_t* descr_p, MvDataFeatureType_e dft_type, bool do_check_ikw, bool inside_if) {
    int             an_ikeyword = END_ARGS;
    string          a_title;
    MvFullType_t    a_full_type;
    MvFullTypeSet_t a_ofts;
    vector< pair<string, comparator_e> > fulltypelst;
    int nb_filters = 0;
    vector<string> attrib_vect, value_vect, criteria_vect, unit_vect, message_vect;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Checking ikeyword
    string a_skeyword;
    an_ikeyword = getNextIKeyword(descr_p, &a_skeyword);
    if (an_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
    if (descr_p->getValueType(an_ikeyword) != VTYPE_OBJECT) throwError(getMsg(24), a_skeyword.c_str());

    if (inside_if == false)
        if (do_check_ikw && isUsed(an_ikeyword)) throwError(getMsg(43), a_skeyword.c_str());
    // Getting object type
    object_type_e obj_type = descr_p->getObjectType(an_ikeyword);
    a_full_type = MvFullType_t(obj_type);
    // Continuing parsing
    char a_char = getNextChar();
    if (a_char == ',') {
        a_title = getNextQuotedString();
        a_char = getNextChar();
    }
    else {
        a_title = descr_p->getComment(an_ikeyword);
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // Block checking
    a_char = getNextChar();
    map<string, string> map;
    if (a_char == '{') {
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            //
            if (a_feature_keyword == "TITLE") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                a_title = getNextQuotedString();
            }
            else if (a_feature_keyword == "SUBTYPES")
            {
                bool nt_eq = false;
                a_char = getNextChar();
                if (a_char == '!')
                {
                    nt_eq = true;
                    if (getNextChar() != '=')
                    {
                        throwError(getMsg(13), "=");
                    }
                }
                else if (a_char != '=')
                {
                    throwError(getMsg(13), "=");
                }
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                a_char = getNextChar();
                unreadChar();

                while (a_char != ')') {
                    string a_strtype(""), a_subtype("");
                    string        a_full_type_str = getNextString();
                    vector<string> pTokens;
                    StringTokenize(a_full_type_str, pTokens, "/");
                    size_t a_size = pTokens.size();
                    if (a_size == 1)
                        a_strtype = pTokens[0];
                    else if (a_size == 2)
                    {
                        a_strtype = pTokens[0];
                        a_subtype = pTokens[1];
                    }
                    const CFGKernel* p_cfgkernel = (const CFGKernel*)mycfgkernel;
                    MvFullType_t  a_nfull_type(*p_cfgkernel, a_strtype, a_subtype);
                    object_type_e a_type = a_nfull_type.getType();
                    //
                    if (a_type == HCDI_OBJ_TYPE_NULL) throwError(getMsg(9), a_full_type_str.c_str());

                    if (nt_eq)
                    {
                        obj_type_e type = a_nfull_type.getType();
                        MvSubtypePtrSet_t a_subtypep_set;
                        ((CFGKernel*)(mycfgkernel))->get_subtypes(type, &a_subtypep_set); /*may not get all subtypes, may be defined later. can store with some rule in string /TYPE/_NE_*/
                        MvSubtypePtrSet_t::iterator a_iter_b = a_subtypep_set.begin();
                        MvSubtypePtrSet_t::iterator a_iter_e = a_subtypep_set.end();
                        MvSubtypePtrSet_t::iterator a_iter;
                        for (a_iter = a_iter_b; a_iter != a_iter_e; ++a_iter)
                        {
                            const MvSubtype_t* subtype = *a_iter;
                            MvFullType_t a_ftype(type, subtype);
                            if (a_nfull_type != a_ftype)
                            {
                                a_ofts.insert(a_ftype);
                            }
                        }
                        if (!a_subtypep_set.size() && a_nfull_type.getSubtypePtr() == NULL && a_subtype != "")
                        {
                            pair<string, comparator_e>  a_pair;
                            a_pair.first = a_full_type_str;
                            a_pair.second = CMPT_NE;
                            fulltypelst.push_back(a_pair);
                        }
                    }
                    else if (a_nfull_type.getSubtypePtr() == NULL && a_subtype != "")
                    {
                        pair<string, comparator_e>  a_pair;
                        a_pair.first = a_full_type_str;
                        a_pair.second = CMPT_EQ;
                        fulltypelst.push_back(a_pair);
                    }
                    else
                        a_ofts.insert(a_nfull_type);

                    
                    a_char = getNextChar();
                    if (a_char != ',' && a_char != ')') throwError(getMsg(34), ",", ")");
                }
            }
            else if (!strncmp(a_feature_keyword.c_str(), "/FILTER", 7))
            {
                fillFilterDetails(a_feature_keyword, attrib_vect, value_vect, criteria_vect, unit_vect, message_vect, &nb_filters);
            }
            else {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                readFeaturesAttributes(false, a_feature_keyword, map);
            }
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    /* Creating */
    
    MvDataDataFeature_t* a_ddf_p = NULL;
    if (DFT_DATA == dft_type)
        a_ddf_p = new MvDataDataFeature_t(a_title, an_ikeyword, obj_type);
    else if (DFT_SUBOBJECT == dft_type)
        a_ddf_p = new MvDataSubobjectFeature_t(a_title, an_ikeyword, obj_type);
    else
        return NULL;
    if (inside_if == true)
    {
        MvDataFeature_t* df = myIKeywordFeatureMap[an_ikeyword];
        if (!df)
        {
            addFeature(an_ikeyword, a_ddf_p);
        }
        else
        {
            df->setNextDataFeature(a_ddf_p);
        }
    }
    else
        addFeature(an_ikeyword, a_ddf_p);

    // Adding full types
    if (fulltypelst.size())
        MvDataCfgParser_t::StoreFullTypesForPostTreat((void*)a_ddf_p, fulltypelst);

    if (a_ofts.empty() && !fulltypelst.size()) {
        a_ddf_p->addObjectFullType(a_full_type);
    }
    else {
        MvFullTypeSet_t::const_iterator a_ofts_begin = a_ofts.begin();
        MvFullTypeSet_t::const_iterator a_ofts_end = a_ofts.end();
        MvFullTypeSet_t::const_iterator a_ofts_it;
        for (a_ofts_it = a_ofts_begin; a_ofts_it != a_ofts_end; ++a_ofts_it) a_ddf_p->addObjectFullType(*a_ofts_it);
    }

    if (nb_filters)
    {
        a_ddf_p->createFilter(nb_filters, attrib_vect, value_vect, criteria_vect, unit_vect, message_vect);
    }
    setFeaturesAttributes(a_ddf_p, map);
    //
    return (MvDataFeature_t*)a_ddf_p;
}

MvDataFeature_t* MvDescriptorParser_t::readAssignFeature(const MvDescriptor_t* descr_p, MvDataFeatureType_e dft_type, bool do_check_ikw) {
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    //
    int a_ikeyword = 0;
    string str_op = "";
    string a_formula = "";
    string          a_title("");
    map<string, string> map;
    try
    {
        //For first field
        a_ikeyword = getNextIKeyword(descr_p);
        if (getNextChar() != ',') throwError(getMsg(13), ",");

        int    a_nb_parenthesis = 1;
        bool chk_comma = true;
        while (a_nb_parenthesis > 0)
        {
            char a_char = getNextChar();
            // skip the addition of [ ] and continue reading commas
            if (a_char == '[')
            {
                chk_comma = false;
                continue;
            }
            else if (a_char == ']')
            {
                chk_comma = true;
                continue;
            }
            if (a_char == '(')
                ++a_nb_parenthesis;
            else if (a_char == ')')
                --a_nb_parenthesis;

            if ((a_char == ',') && (a_nb_parenthesis <= 1) && chk_comma)
            {
                str_op = getNextString();
                if (str_op != "TRUE" && str_op != "FALSE") throwError(getMsg(13), ",");
            }
            else
            {
                if (a_nb_parenthesis > 0)
                    a_formula += a_char;
                else if (a_nb_parenthesis < 1)
                {
                    if (a_char != ')') throwError(getMsg(13), ")");
                    a_char = getNextChar();
                    if (a_char == '{')
                    {
                        while (getNextChar() != '}')
                        {
                            unreadChar();
                            string a_feature_keyword = "";
                            readFeaturesAttributes(true, a_feature_keyword, map);
                            if (getNextChar() != ';') throwError(getMsg(13), ";");
                        }
                    }
                    else if (a_char != ';') throwError(getMsg(13), ";");
                }
            }
        }
    }
    catch (MvError_t& a_error) {
        throw a_error;
    }
    a_title = descr_p->getComment(a_ikeyword);
    value_type_e assigned_valtype = VTYPE_UNKNOWN;
    assigned_valtype = descr_p->getValueType(a_ikeyword);
    ff_card_t* a_card_p = NULL;
    readCardAssignData(descr_p, &a_card_p, a_ikeyword, a_formula, assigned_valtype, str_op);
    // Creating the feature

    MvDataAssignFeature_t* a_feature_p = new MvDataAssignFeature_t(a_title, a_ikeyword);
    string test_string = "";
    int size = (int)a_formula.size();
    bool quotes_present = false;
    for (int i = 0; i < size; i++)
    {
        if (a_formula[i] == '\"')
        {
            quotes_present = true;
            continue;
        }
        test_string += a_formula[i];
    }
    a_feature_p->SetExpression(test_string);
    if (str_op == "TRUE")
        a_feature_p->SetIsEditable(true);

    a_feature_p->SetCardData(a_card_p);
    addFeature(a_ikeyword, a_feature_p);
    setFeaturesAttributes(a_feature_p, map);
    //
    return (MvDataFeature_t*)a_feature_p;
}














MvDataFeature_t* MvDescriptorParser_t::readToolFeature(const MvDescriptor_t* descr_p, bool do_check_ikw) {
    string                    a_title;
    int                       a_ikeyword = END_ARGS;
    MvDataSkewFeatureOrigin_e a_origin = DSF_ORIGIN_UNALLOWED; 
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Checking ikeyword
    string a_skeyword;
    a_ikeyword = getNextIKeyword(descr_p, &a_skeyword);
    if (a_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
    if (descr_p->getValueType(a_ikeyword) != VTYPE_OBJECT) throwError(getMsg(24), a_skeyword.c_str());
    object_type_e a_otype = descr_p->getObjectType(a_ikeyword); 
    if (do_check_ikw && isUsed(a_ikeyword)) throwError(getMsg(43), a_skeyword.c_str());
    // Continuing parsing
    char a_char = getNextChar();
    if (a_char == ',') {
        a_title = getNextQuotedString();
        a_char = getNextChar();
    }
    else {
        a_title = descr_p->getComment(a_ikeyword);
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // Block checking
    map<string, string> map;
    a_char = getNextChar();
    if (a_char == '{') {
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            if (getNextChar() != '=') throwError(getMsg(13), "=");
            //
            
            if (a_feature_keyword == "TITLE") {
                a_title = getNextQuotedString();
            }
            else if (a_feature_keyword == "ORIGIN") {
                string a_feature_value = getNextString();
                if (a_otype != HCDI_OBJ_TYPE_SYSTS) throwError(getMsg(20), a_feature_keyword.c_str());
                if (a_feature_value == "UNALLOWED")      a_origin = DSF_ORIGIN_UNALLOWED;
                else if (a_feature_value == "OPTIONAL")  a_origin = DSF_ORIGIN_OPTIONAL;
                else if (a_feature_value == "MANDATORY") a_origin = DSF_ORIGIN_MANDATORY;
                else                                  throwError(getMsg(73), a_feature_value.c_str());
            }
            else {
                readFeaturesAttributes(false, a_feature_keyword, map);
            }
            //
            if (getNextChar() != ';') throwError(getMsg(13), ";");
            
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    /* Creating */
    
    // Creating the feature
    MvDataFeature_t* a_feature_p = NULL;
    switch (a_otype) {
    case HCDI_OBJ_TYPE_SYSTS:     a_feature_p = (MvDataFeature_t*)(new MvDataSkewFeature_t(a_title, a_ikeyword, a_origin)); break;
    case HCDI_OBJ_TYPE_SENSORS:   a_feature_p = (MvDataFeature_t*)(new MvDataSensorFeature_t(a_title, a_ikeyword));        break;
    case HCDI_OBJ_TYPE_ACCELEROMETERS: a_feature_p = (MvDataFeature_t*)(new MvDataAcceleroFeature_t(a_title, a_ikeyword));      break;
    default:       throwError(getMsg(25), MV_get_type(a_otype).c_str());                                    //break;
    }
    
    setFeaturesAttributes(a_feature_p, map);
    addFeature(a_ikeyword, a_feature_p);
    //
    return a_feature_p;
}


typedef struct LocFunctionCondItems_s {
    MvExpression_t* myExprPtr;
    MvDataFeatureSet_t  myTestFeaturePtrSet;
    string              myName;                        
    string              myXScalingName, myYScalingName, myZScalingName; 
    string              myXTitle, myYTitle, myZTitle;
    MvDimension_e       myXDimension, myYDimension, myZDimension;
    vector<string>      myXDimArgVect, myYDimArgVect, myZDimArgVect;
    string              mySubXTitle, mySubYTitle, mySubZTitle;         
    MvDimension_e       mySubXDimension, mySubYDimension, mySubZDimension; 
    vector<string>      mySubXDimArgVect, mySubYDimArgVect, mySubZDimArgVect;
} LocFunctionCondItems_t;
typedef vector<LocFunctionCondItems_t> LocFunctionCondList_t;



MvDataFeature_t* MvDescriptorParser_t::readFunctionFeature(const MvDescriptor_t* descr_p, MvFunctionType_e func_type, bool do_check_ikw) {
    
    int                    an_ikeyword = END_ARGS, a_xscale_ikw = END_ARGS, a_yscale_ikw = END_ARGS, a_zscale_ikw = END_ARGS;
    string                 a_xtitle = "X-values", a_ytitle = "Y-values", a_ztitle = "Z-values", a_sub_xtitle = "X-values", a_sub_ytitle = "Y-values", a_sub_ztitle = "Z-values", a_title = "";
    string                 a_xscale_title = "", a_yscale_title = "", a_zscale_title = "", str_x_can_be_negative = "";
    MvDimension_e          a_xdim = UDI_DIMENSIONLESS, a_ydim = UDI_DIMENSIONLESS, a_zdim = UDI_DIMENSIONLESS, a_sub_xdim = UDI_DIMENSIONLESS, a_sub_ydim = UDI_DIMENSIONLESS, a_sub_zdim = UDI_DIMENSIONLESS;
    LocFunctionCondList_t  a_cond_list;
    LocFunctionCondItems_t a_default_items;
    bool is_x_can_be_negative = true;
    int nb_filters = 0;
    vector<string> attrib_vect, value_vect, criteria_vect, unit_vect, message_vect, xDimArgVect, yDimArgVect, zDimArgVect, subXDimArgVect, subYDimArgVect, subZDimArgVect;
    
    //
    
    a_default_items.myExprPtr = NULL;
    a_default_items.myName = "";
    a_default_items.myXScalingName = a_default_items.myYScalingName = "", a_default_items.myZScalingName = "";
    a_default_items.myXTitle = a_default_items.myYTitle = a_default_items.myZTitle = "";
    a_default_items.myXDimension = a_default_items.myYDimension = UDI_DIMENSIONLESS, a_default_items.myZDimension = UDI_DIMENSIONLESS;
    a_default_items.mySubXTitle = a_default_items.mySubYTitle = a_default_items.mySubZTitle = "";
    a_default_items.mySubXDimension = a_default_items.mySubYDimension = a_default_items.mySubZDimension = UDI_DIMENSIONLESS;
    
    //
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Checking ikeyword
    string a_skeyword;
    an_ikeyword = getNextIKeyword(descr_p, &a_skeyword);
    if (an_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
    if (descr_p->getValueType(an_ikeyword) != VTYPE_OBJECT || descr_p->getObjectType(an_ikeyword) != HCDI_OBJ_TYPE_CURVES) {
        throwError(getMsg(29), a_skeyword.c_str());
    }
    if (do_check_ikw && isUsed(an_ikeyword)) throwError(getMsg(43), a_skeyword.c_str());
    // Continuing parsing
    char a_char = getNextChar();
    if (a_char == ',') {
        a_title = getNextQuotedString();
        a_char = getNextChar();
    }
    else {
        a_title = descr_p->getComment(an_ikeyword);
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // Block checking
    a_char = getNextChar();
    map<string, string> map;
    if (a_char == '{') {
        LocFunctionCondItems_t a_cond_items;
        string               a_cond_type = "";
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = a_cond_type;
            if (a_feature_keyword == "else if") {
                a_feature_keyword = "if";
                a_cond_type = "";
            }
            else {
                a_feature_keyword = getNextString();
            }
            //
            if (a_cond_type == "" && a_feature_keyword == "if") {
                a_cond_type = "if";
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                a_cond_items = a_default_items;
                a_cond_items.myExprPtr = getNextExpressionPtr(descr_p, true, &(a_cond_items.myTestFeaturePtrSet));
                if (getNextChar() != ')') throwError(getMsg(13), ")");
                if (getNextChar() != '{') throwError(getMsg(13), "{");
            }
            else {
                if (a_feature_keyword == "X_SCALING" || a_feature_keyword == "Y_SCALING" || a_feature_keyword == "Z_SCALING" || a_feature_keyword == "SCALING") { 
                  
                    if (getNextChar() != '(') throwError(getMsg(13), ")");
                    string a_scale_skw = getNextString();
                    int& a_scale_ikw = (a_feature_keyword == "X_SCALING" ? a_xscale_ikw : a_yscale_ikw);
                    int a_old_scale_ikw = a_scale_ikw;
                    a_scale_ikw = descr_p->getIKeyword(a_scale_skw);
                    if (a_scale_ikw == END_ARGS) throwError(getMsg(20), a_scale_skw.c_str());
                    if (a_scale_ikw != a_old_scale_ikw && a_old_scale_ikw != END_ARGS) throwError(getMsg(79), a_scale_skw.c_str());
                    if (descr_p->getValueType(a_scale_ikw) != VTYPE_FLOAT) throwError(getMsg(28), a_scale_skw.c_str());
                    a_char = getNextChar();
                    if (a_char == ',') {
                        if (a_feature_keyword == "X_SCALING") {
                            if (a_cond_type == "") a_xscale_title = getNextQuotedString();
                            else                a_cond_items.myXScalingName = getNextQuotedString();
                        }
                        else if(a_feature_keyword == "Y_SCALING") {
                            if (a_cond_type == "") a_yscale_title = getNextQuotedString();
                            else                a_cond_items.myYScalingName = getNextQuotedString();
                        }
                        else if (a_feature_keyword == "Z_SCALING") {
                            if (a_cond_type == "") a_zscale_title = getNextQuotedString();
                            else                a_cond_items.myZScalingName = getNextQuotedString();
                        }
                        a_char = getNextChar();
                    }
                    else {
                        if (a_feature_keyword == "X_SCALING") {
                            if (a_cond_type == "") a_xscale_title = descr_p->getComment(a_scale_ikw);
                            else                a_cond_items.myXScalingName = descr_p->getComment(a_scale_ikw);
                        }
                        else if (a_feature_keyword == "Y_SCALING") {
                            if (a_cond_type == "") a_yscale_title = descr_p->getComment(a_scale_ikw);
                            else                a_cond_items.myYScalingName = descr_p->getComment(a_scale_ikw);
                        }
                        else if (a_feature_keyword == "Z_SCALING") {
                            if (a_cond_type == "") a_zscale_title = descr_p->getComment(a_scale_ikw);
                            else                a_cond_items.myZScalingName = descr_p->getComment(a_scale_ikw);
                        }
                    }
                    if (a_char != ')') throwError(getMsg(13), ")");
                    
                }
                else if (!strncmp(a_feature_keyword.c_str(), "/FILTER", 7))
                {
                    fillFilterDetails(a_feature_keyword, attrib_vect, value_vect, criteria_vect, unit_vect, message_vect, &nb_filters);
                }
                else {
                    if (getNextChar() != '=') throwError(getMsg(13), "=");
                    if (a_feature_keyword == "TITLE") {
                        if (a_cond_type == "") a_title = getNextQuotedString();
                        else                a_cond_items.myName = getNextQuotedString();
                    }
                    else if (a_feature_keyword == "X_DIMENSION") {
                        if (a_cond_type == "") a_xdim = getNextDimension(&xDimArgVect);
                        else                a_cond_items.myXDimension = getNextDimension(&a_cond_items.myXDimArgVect);
                    }
                    else if (a_feature_keyword == "Y_DIMENSION") {
                        if (a_cond_type == "") a_ydim = getNextDimension(&yDimArgVect);
                        else                a_cond_items.myYDimension = getNextDimension(&a_cond_items.myYDimArgVect);
                    }
                    else if (a_feature_keyword == "Z_DIMENSION") {
                        if (a_cond_type == "") a_zdim = getNextDimension(&zDimArgVect);
                        else                a_cond_items.myZDimension = getNextDimension(&a_cond_items.myZDimArgVect);
                    }
                    else if (a_feature_keyword == "X_TITLE") {
                        if (a_cond_type == "") a_xtitle = getNextQuotedString();
                        else                a_cond_items.myXTitle = getNextQuotedString();
                    }
                    else if (a_feature_keyword == "Y_TITLE") {
                        if (a_cond_type == "") a_ytitle = getNextQuotedString();
                        else                a_cond_items.myYTitle = getNextQuotedString();
                    }
                    else if (a_feature_keyword == "Z_TITLE") {
                        if (a_cond_type == "") a_ztitle = getNextQuotedString();
                        else                a_cond_items.myZTitle = getNextQuotedString();
                    }
                    else if (a_feature_keyword == "SUB_X_DIMENSION") {
                        if (a_cond_type == "") a_sub_xdim = getNextDimension(&subXDimArgVect);
                        else                a_cond_items.mySubXDimension = getNextDimension(&a_cond_items.mySubXDimArgVect);
                    }
                    else if (a_feature_keyword == "SUB_Y_DIMENSION") {
                        if (a_cond_type == "") a_sub_ydim = getNextDimension(&subYDimArgVect);
                        else                a_cond_items.mySubYDimension = getNextDimension(&a_cond_items.mySubYDimArgVect);
                    }
                    else if (a_feature_keyword == "SUB_Z_DIMENSION") {
                        if (a_cond_type == "") a_sub_zdim = getNextDimension(&subZDimArgVect);
                        else                a_cond_items.mySubZDimension = getNextDimension(&a_cond_items.mySubZDimArgVect);
                    }
                    else if (a_feature_keyword == "SUB_X_TITLE") {
                        if (a_cond_type == "") a_sub_xtitle = getNextQuotedString();
                        else                a_cond_items.mySubXTitle = getNextQuotedString();
                    }
                    else if (a_feature_keyword == "SUB_Y_TITLE") {
                        if (a_cond_type == "") a_sub_ytitle = getNextQuotedString();
                        else                a_cond_items.mySubYTitle = getNextQuotedString();
                    }
                    else if (a_feature_keyword == "SUB_Z_TITLE") {
                        if (a_cond_type == "") a_sub_ztitle = getNextQuotedString();
                        else                a_cond_items.mySubZTitle = getNextQuotedString();
                    }
                    else if (a_feature_keyword == "X_CAN_BE_NEGATIVE") {
                        str_x_can_be_negative = getNextString();
                        if (str_x_can_be_negative == "FALSE") is_x_can_be_negative = false;
                    }
                    else {
                        readFeaturesAttributes(false, a_feature_keyword, map);
                    }
                }
                if (getNextChar() != ';') throwError(getMsg(13), ";");
                //
                if (a_cond_type != "") {
                    if (getNextChar() != '}') {
                        unreadChar();
                    }
                    else {
                        a_cond_list.push_back(a_cond_items);
                        a_cond_items = a_default_items;
                        //
                        if (a_cond_type == "else") {
                            a_cond_type = "";
                        }
                        else {
                            a_char = getNextChar();
                            unreadChar();
                            if (a_char != '}') {
                                a_cond_type = getNextString();
                                if (a_cond_type != "else") {
                                    int a_size = (int)(a_cond_type.size());
                                    while (a_size--) unreadChar();
                                    a_cond_type = "";
                                }
                                else {
                                    if (getNextChar() != '{') {
                                        unreadChar();
                                        a_cond_type = getNextString();
                                        if (a_cond_type != "if") throwError(getMsg(44));
                                        a_cond_type = "else if";
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    /* Creating */
    // Creating the feature
    MvDataFeature_t* a_feature_p = NULL;
    if (a_cond_list.empty()) {
        
        
        if (a_xscale_ikw == END_ARGS && a_yscale_ikw == END_ARGS) {
            
            a_feature_p = new MvDataUncondFunctionFeature_t(a_title, an_ikeyword, a_title,
                a_xtitle, a_xdim, xDimArgVect, a_ytitle, a_ydim, yDimArgVect, a_ztitle, a_zdim, zDimArgVect,
                a_sub_xtitle, a_sub_xdim, &subXDimArgVect, a_sub_ytitle, a_sub_ydim, &subYDimArgVect, a_sub_ztitle, a_sub_zdim, &subZDimArgVect);
            
        }
        else {
            
            a_feature_p = new MvDataUncondFunctionFeature_t(a_title, an_ikeyword, a_title,
                a_xtitle, a_xdim, xDimArgVect, a_ytitle, a_ydim, yDimArgVect, a_ztitle, a_zdim, zDimArgVect,
                a_xscale_ikw, a_xscale_title,
                a_yscale_ikw, a_yscale_title,
                a_zscale_ikw, a_zscale_title);
            
        }
        
        
    }
    else {
        
        int a_nb_tests = (int)(a_cond_list.size());
        if (a_cond_list[a_nb_tests - 1].myExprPtr == NULL) --a_nb_tests;
        //
        
        if (a_xscale_ikw == END_ARGS && a_yscale_ikw == END_ARGS) {
            a_feature_p = new MvDataCondFunctionFeature_t(a_title, a_nb_tests, an_ikeyword);
        }
        else {
            a_feature_p = new MvDataCondFunctionFeature_t(a_title, a_nb_tests, an_ikeyword, a_xscale_ikw, a_yscale_ikw, a_zscale_ikw);
        }
        
        MvDataCondFunctionFeature_t* a_dcff_p = (MvDataCondFunctionFeature_t*)a_feature_p;
        //
        for (int i = 0; i < a_nb_tests; ++i) {
            string        a_loc_title = (a_cond_list[i].myName == a_default_items.myName ? a_title : a_cond_list[i].myName);
            string        a_loc_xscale_title = (a_cond_list[i].myXScalingName == a_default_items.myXScalingName ? a_xscale_title : a_cond_list[i].myXScalingName);
            string        a_loc_yscale_title = (a_cond_list[i].myYScalingName == a_default_items.myYScalingName ? a_yscale_title : a_cond_list[i].myYScalingName);
            string        a_loc_zscale_title = (a_cond_list[i].myZScalingName == a_default_items.myZScalingName ? a_zscale_title : a_cond_list[i].myZScalingName);
            string        a_loc_xtitle = (a_cond_list[i].myXTitle == a_default_items.myXTitle ? a_xtitle : a_cond_list[i].myXTitle);
            string        a_loc_ytitle = (a_cond_list[i].myYTitle == a_default_items.myYTitle ? a_ytitle : a_cond_list[i].myYTitle);
            string        a_loc_ztitle = (a_cond_list[i].myZTitle == a_default_items.myZTitle ? a_ztitle : a_cond_list[i].myZTitle);
            MvDimension_e a_loc_xdim = (a_cond_list[i].myXDimension == a_default_items.myXDimension ? a_xdim : a_cond_list[i].myXDimension);
            MvDimension_e a_loc_ydim = (a_cond_list[i].myYDimension == a_default_items.myYDimension ? a_ydim : a_cond_list[i].myYDimension);
            MvDimension_e a_loc_zdim = (a_cond_list[i].myZDimension == a_default_items.myZDimension ? a_zdim : a_cond_list[i].myZDimension);
            string        a_loc_sub_xtitle = (a_cond_list[i].mySubXTitle == a_default_items.mySubXTitle ? a_sub_xtitle : a_cond_list[i].mySubXTitle);
            string        a_loc_sub_ytitle = (a_cond_list[i].mySubYTitle == a_default_items.mySubYTitle ? a_sub_ytitle : a_cond_list[i].mySubYTitle);
            string        a_loc_sub_ztitle = (a_cond_list[i].mySubZTitle == a_default_items.mySubZTitle ? a_sub_ztitle : a_cond_list[i].mySubZTitle);
            MvDimension_e a_loc_sub_xdim = (a_cond_list[i].mySubXDimension == a_default_items.mySubXDimension ? a_sub_xdim : a_cond_list[i].mySubXDimension);
            MvDimension_e a_loc_sub_ydim = (a_cond_list[i].mySubYDimension == a_default_items.mySubYDimension ? a_sub_ydim : a_cond_list[i].mySubYDimension);
            MvDimension_e a_loc_sub_zdim = (a_cond_list[i].mySubZDimension == a_default_items.mySubZDimension ? a_sub_zdim : a_cond_list[i].mySubZDimension);
            vector<string> a_loc_xdim_arg_vect = (a_cond_list[i].myXDimArgVect == a_default_items.myXDimArgVect ? xDimArgVect : a_cond_list[i].myXDimArgVect);
            vector<string> a_loc_ydim_arg_vect = (a_cond_list[i].myYDimArgVect == a_default_items.myYDimArgVect ? yDimArgVect : a_cond_list[i].myYDimArgVect);
            vector<string> a_loc_zdim_arg_vect = (a_cond_list[i].myZDimArgVect == a_default_items.myZDimArgVect ? zDimArgVect : a_cond_list[i].myZDimArgVect);
            vector<string> a_loc_sub_xdim_arg_vect = (a_cond_list[i].mySubXDimArgVect == a_default_items.mySubXDimArgVect ? subXDimArgVect : a_cond_list[i].mySubXDimArgVect);
            vector<string> a_loc_sub_ydim_arg_vect = (a_cond_list[i].mySubYDimArgVect == a_default_items.mySubYDimArgVect ? subYDimArgVect : a_cond_list[i].mySubYDimArgVect);
            vector<string> a_loc_sub_zdim_arg_vect = (a_cond_list[i].mySubZDimArgVect == a_default_items.mySubZDimArgVect ? subZDimArgVect : a_cond_list[i].mySubZDimArgVect);
            //
            a_dcff_p->setConditionalData(i,
                a_cond_list[i].myTestFeaturePtrSet,
                a_cond_list[i].myExprPtr,
                a_loc_title,
                a_loc_xtitle, a_loc_ytitle, a_loc_ztitle,
                a_loc_xdim_arg_vect, a_loc_ydim_arg_vect, a_loc_zdim_arg_vect,
                a_loc_xdim, a_loc_ydim, a_loc_zdim,
                a_loc_xscale_title, a_loc_yscale_title, a_loc_zscale_title,
                a_loc_sub_xtitle, a_loc_sub_ytitle, a_loc_sub_ztitle,
                a_loc_sub_xdim, a_loc_sub_ydim, a_loc_sub_zdim,
                &a_loc_sub_xdim_arg_vect, &a_loc_sub_ydim_arg_vect, &a_loc_sub_zdim_arg_vect);
        }
        int a_nb_data = (int)(a_cond_list.size());
        if (a_nb_tests < a_nb_data) {
            string        a_loc_title = (a_cond_list[a_nb_tests].myName == a_default_items.myName ? a_title : a_cond_list[a_nb_tests].myName);
            string        a_loc_xscale_title = (a_cond_list[a_nb_tests].myXScalingName == a_default_items.myXScalingName ? a_xscale_title : a_cond_list[a_nb_tests].myXScalingName);
            string        a_loc_yscale_title = (a_cond_list[a_nb_tests].myYScalingName == a_default_items.myYScalingName ? a_yscale_title : a_cond_list[a_nb_tests].myYScalingName);
            string        a_loc_zscale_title = (a_cond_list[a_nb_tests].myZScalingName == a_default_items.myZScalingName ? a_zscale_title : a_cond_list[a_nb_tests].myZScalingName);
            string        a_loc_xtitle = (a_cond_list[a_nb_tests].myXTitle == a_default_items.myXTitle ? a_xtitle : a_cond_list[a_nb_tests].myXTitle);
            string        a_loc_ytitle = (a_cond_list[a_nb_tests].myYTitle == a_default_items.myYTitle ? a_ytitle : a_cond_list[a_nb_tests].myYTitle);
            string        a_loc_ztitle = (a_cond_list[a_nb_tests].myZTitle == a_default_items.myZTitle ? a_ztitle : a_cond_list[a_nb_tests].myZTitle);
            MvDimension_e a_loc_xdim = (a_cond_list[a_nb_tests].myXDimension == a_default_items.myXDimension ? a_xdim : a_cond_list[a_nb_tests].myXDimension);
            MvDimension_e a_loc_ydim = (a_cond_list[a_nb_tests].myYDimension == a_default_items.myYDimension ? a_ydim : a_cond_list[a_nb_tests].myYDimension);
            MvDimension_e a_loc_zdim = (a_cond_list[a_nb_tests].myZDimension == a_default_items.myZDimension ? a_zdim : a_cond_list[a_nb_tests].myZDimension);
            string        a_loc_sub_xtitle = (a_cond_list[a_nb_tests].mySubXTitle == a_default_items.mySubXTitle ? a_sub_xtitle : a_cond_list[a_nb_tests].mySubXTitle);
            string        a_loc_sub_ytitle = (a_cond_list[a_nb_tests].mySubYTitle == a_default_items.mySubYTitle ? a_sub_ytitle : a_cond_list[a_nb_tests].mySubYTitle);
            string        a_loc_sub_ztitle = (a_cond_list[a_nb_tests].mySubZTitle == a_default_items.mySubZTitle ? a_sub_ztitle : a_cond_list[a_nb_tests].mySubZTitle);
            MvDimension_e a_loc_sub_xdim = (a_cond_list[a_nb_tests].mySubXDimension == a_default_items.mySubXDimension ? a_sub_xdim : a_cond_list[a_nb_tests].mySubXDimension);
            MvDimension_e a_loc_sub_ydim = (a_cond_list[a_nb_tests].mySubYDimension == a_default_items.mySubYDimension ? a_sub_ydim : a_cond_list[a_nb_tests].mySubYDimension);
            MvDimension_e a_loc_sub_zdim = (a_cond_list[a_nb_tests].mySubZDimension == a_default_items.mySubZDimension ? a_sub_zdim : a_cond_list[a_nb_tests].mySubZDimension);
            vector<string> a_loc_xdim_arg_vect = (a_cond_list[a_nb_tests].myXDimArgVect == a_default_items.myXDimArgVect ? xDimArgVect : a_cond_list[a_nb_tests].myXDimArgVect);
            vector<string> a_loc_ydim_arg_vect = (a_cond_list[a_nb_tests].myYDimArgVect == a_default_items.myYDimArgVect ? yDimArgVect : a_cond_list[a_nb_tests].myYDimArgVect);
            vector<string> a_loc_zdim_arg_vect = (a_cond_list[a_nb_tests].myZDimArgVect == a_default_items.myZDimArgVect ? zDimArgVect : a_cond_list[a_nb_tests].myZDimArgVect);
            vector<string> a_loc_sub_xdim_arg_vect = (a_cond_list[a_nb_tests].mySubXDimArgVect == a_default_items.mySubXDimArgVect ? subXDimArgVect : a_cond_list[a_nb_tests].mySubXDimArgVect);
            vector<string> a_loc_sub_ydim_arg_vect = (a_cond_list[a_nb_tests].mySubYDimArgVect == a_default_items.mySubYDimArgVect ? subYDimArgVect : a_cond_list[a_nb_tests].mySubYDimArgVect);
            vector<string> a_loc_sub_zdim_arg_vect = (a_cond_list[a_nb_tests].mySubZDimArgVect == a_default_items.mySubZDimArgVect ? subZDimArgVect : a_cond_list[a_nb_tests].mySubZDimArgVect);

            a_dcff_p->setDefaultData(a_loc_title,
                a_loc_xtitle, a_loc_ytitle, a_loc_ztitle,
                a_loc_xdim_arg_vect, a_loc_ydim_arg_vect, a_loc_zdim_arg_vect,
                a_loc_xdim, a_loc_ydim, a_loc_zdim,
                a_loc_xscale_title, a_loc_yscale_title, a_loc_zscale_title,
                a_loc_sub_xtitle, a_loc_sub_ytitle, a_loc_sub_ztitle,
                a_loc_sub_xdim, a_loc_sub_ydim, a_loc_sub_zdim,
                &a_loc_sub_xdim_arg_vect, &a_loc_sub_ydim_arg_vect, &a_loc_sub_zdim_arg_vect);
        }
        else {
            a_dcff_p->setDefaultData(a_title,
                a_xtitle, a_ytitle, a_ztitle,
                xDimArgVect, yDimArgVect, zDimArgVect,
                a_xdim, a_ydim, a_zdim,
                a_xscale_title, a_yscale_title, a_zscale_title,
                a_sub_xtitle, a_sub_ytitle, a_sub_ztitle,
                a_sub_xdim, a_sub_ydim, a_sub_zdim,
                &subXDimArgVect, &subYDimArgVect, &subZDimArgVect);
        }
        
    }
    //
    ((MvDataFunctionFeature_t*)a_feature_p)->setFunctionType(func_type);
    ((MvDataFunctionFeature_t*)a_feature_p)->setXCanBeNegative(is_x_can_be_negative);
    if (nb_filters)
    {
        ((MvDataFunctionFeature_t*)a_feature_p)->createFilter(nb_filters, attrib_vect, value_vect, criteria_vect, unit_vect, message_vect);
    }
    setFeaturesAttributes(a_feature_p, map);
    addFeature(an_ikeyword, a_feature_p);
    //
    return a_feature_p;
}



typedef MvFlagPair_t     MvSupportPair_t;
typedef MvFlagPairList_t MvSupportPairList_t;

MvDataFeature_t* MvDescriptorParser_t::readSupportFeature(const MvDescriptor_t* descr_p) {
    string              a_title;
    MvSupportPairList_t a_spl;
    MvFullTypeSet_t     a_ofts;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Getting title
    a_title = getNextQuotedString();
    if (getNextChar() != ')') throwError(getMsg(13), ")");
    // Block checking
    map<string, string> map;
    char a_char = getNextChar();
    if (a_char == '{') {
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            if (a_feature_keyword == "ADD") {
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                // Getting attribute
                string a_skeyword, a_comment;
                int an_ikeyword = getNextIKeyword(descr_p, &a_skeyword);
                if (an_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
                if (descr_p->getValueType(an_ikeyword) != VTYPE_OBJECT) throwError(getMsg(24), a_skeyword.c_str());
                // Getting comment
                a_char = getNextChar();
                if (a_char == ',') {
                    a_comment = getNextQuotedString();
                    a_char = getNextChar();
                }
                else {
                    a_comment = descr_p->getComment(an_ikeyword);
                }
                if (a_char != ')') throwError(getMsg(13), ")");
                // Adding the pair
                a_spl.push_back(MvSupportPair_t(an_ikeyword, a_comment));
            }
            else if (a_feature_keyword == "OBJECTS") {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                a_char = getNextChar();
                if (a_char != ')') unreadChar();
                while (a_char != ')') {
                    getNextObjectFullTypes(&a_ofts); 
                    a_char = getNextChar();
                    if (a_char != ',' && a_char != ')') throwError(getMsg(34), ",", ")");
                }
            }
            else {
                if (getNextChar() != '=') throwError(getMsg(13), "=");
                readFeaturesAttributes(false, a_feature_keyword, map);
            }
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    /* Creating */
    int i, nb_attributes = (int)(a_spl.size());
    MvDataSupportFeature_t* a_dsf_p = new MvDataSupportFeature_t(a_title, nb_attributes);
    MvSupportPairList_t::const_iterator a_spl_begin = a_spl.begin();
    MvSupportPairList_t::const_iterator a_spl_end = a_spl.end();
    MvSupportPairList_t::const_iterator a_spl_it;
    for (i = 0, a_spl_it = a_spl_begin; a_spl_it != a_spl_end; ++a_spl_it, ++i) {
        a_dsf_p->setAttribute(*descr_p, i, (*a_spl_it).first, (*a_spl_it).second);
    }
    MvFullTypeSet_t::const_iterator a_ofts_begin = a_ofts.begin();
    MvFullTypeSet_t::const_iterator a_ofts_end = a_ofts.end();
    MvFullTypeSet_t::const_iterator a_ofts_it;
    for (a_ofts_it = a_ofts_begin; a_ofts_it != a_ofts_end; ++a_ofts_it) a_dsf_p->addObjectFullType(*a_ofts_it);
    //
    setFeaturesAttributes((MvDataFeature_t*)a_dsf_p, map);
    return (MvDataFeature_t*)a_dsf_p;
}

MvDataFeature_t* MvDescriptorParser_t::readAppendFeature(const MvDescriptor_t* descr_p) {
    int          a_loc_ikeyword = END_ARGS;
    MvFullType_t a_ext_fulltype;
    string       a_ext_skeyword;
    string       a_title;
    bool         a_is_default = false;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Getting the local ikeyword
    string a_loc_skeyword;
    a_loc_ikeyword = getNextIKeyword(descr_p, &a_loc_skeyword);
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    // Getting the external full type
    a_ext_fulltype = getNextObjectFullType();
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    // Getting the external skeyword
    a_ext_skeyword = getNextString();
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    // Getting the title
    a_title = getNextQuotedString();
    //
    char a_char = getNextChar();
    if (a_char == ',') {
        string a_default = getNextString();
        a_is_default = (a_default == "DEFAULT");
        a_char = getNextChar();
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    // block reading
    map<string, string> map;
    a_char = getNextChar();
    if (a_char == '{')
    {
        while (getNextChar() != '}') {
            unreadChar();
            string feature_str = "";
            readFeaturesAttributes(true, feature_str, map);
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
    }
    else if (a_char != ';') throwError(getMsg(13), ";");
    /* Creating */
    MvDataAppendFeature_t* a_daf_p = new MvDataAppendFeature_t(a_title, a_loc_ikeyword,
        a_ext_fulltype, a_ext_skeyword, a_is_default);
    setFeaturesAttributes((MvDataFeature_t*)a_daf_p, map);
    return (MvDataFeature_t*)a_daf_p;
}

typedef struct MvConditionalList_s {
    /*
    int                    myIKeyword;
    const MvDataFeature_t *myFeaturePtr;
    int                    myIndex;
    string                 myComparator;
    string                 myRValue;
    */
    MvExpression_t* myExprPtr;
    MvDataFeatureSet_t   myTestFeaturePtrSet;
    MvDataFeatureList_t  myFeaturePtrList;
    MvDataFeatureList_t  myFeaturePtrReducedList;
} MvConditionalList_t;


MvDataFeature_t* MvDescriptorParser_t::readIfFeature(const MvDescriptor_t* descr_p, MvDomain_e domain) {
    
    deque<MvConditionalList_t> a_cld;
    bool parallel_cards_to_if = false;
    /*
      if() 1
      {
          if() 2
          {
          }
          card; // parallel card to if() 2
      }
    */
    //
    bool   a_continue = true;
    string a_cond_type = "if";
    /* Parsing */
    while (a_continue || parallel_cards_to_if) {
        MvConditionalList_t a_cl;
        if (a_cond_type == "if") {
            if (getNextChar() != '(') throwError(getMsg(13), "(");
            // Getting expression
            /*
            a_cl.myIKeyword   = getNextIKeyword(descr_p);
            if(getNextChar()!='[') {
          unreadChar();
          a_cl.myIndex=-1;
            } else {
          a_cl.myIndex=getNextInt();
          if(getNextChar()!=']') throwError(getMsg(13),"]");
            }
            a_cl.myFeaturePtr = getFeaturePtr(a_cl.myIKeyword);
            if(a_cl.myFeaturePtr==NULL) throwError(getMsg(45),descr_p->getSKeyword(a_cl.myIKeyword).c_str());
            a_cl.myComparator = getNextComparator();
            a_cl.myRValue     = getNextString();
            */
            a_cl.myExprPtr = getNextExpressionPtr(descr_p, true, &(a_cl.myTestFeaturePtrSet));
            if (getNextChar() != ')') throwError(getMsg(13), ")");
        }
        else {
            if (!parallel_cards_to_if)
            {
                a_cl.myExprPtr = NULL;
            }
        }
        // Reading features
        if (!parallel_cards_to_if)
        {
            if (getNextChar() != '{') throwError(getMsg(13), "{");
        }
        bool a_is_optional = isOptional();
        bool a_is_graphical = false;
        char c = getNextChar();
        while (c != '}') {
            unreadChar();
            string               a_feature_keyword = getNextString();
            MvDataFeatureList_t& a_dfl = a_cl.myFeaturePtrList;
            MvDataFeatureList_t& a_reduced_dfl = a_cl.myFeaturePtrReducedList;
            MvDataFeature_t* a_df_p = NULL;
            //
            if (a_feature_keyword == "SCALAR")           a_df_p = readScalarFeature(descr_p, false);
            else if (a_feature_keyword == "SIZE")        a_df_p = readSizeFeature(descr_p, false);
            else if (a_feature_keyword == "SIZE_RADIO")  a_df_p = readSizeRadioFeature(descr_p, domain, false);
            else if (a_feature_keyword == "FILE")        a_df_p = readFileFeature(descr_p, false);
            else if (a_feature_keyword == "DIR")         a_df_p = readDirFeature(descr_p, false);
            else if (a_feature_keyword == "TRIPLE")      a_df_p = readTripleFeature(descr_p);
            else if (a_feature_keyword == "POINT")       a_df_p = readTripleFeature(descr_p, true);
            
            else if (a_feature_keyword == "RADIO")       a_df_p = readRadioFeature(descr_p, domain, false);
            
            else if (a_feature_keyword == "RADIO_ARRAY") a_df_p = readRadioArrayFeature(descr_p, false);
            else if (a_feature_keyword == "FLAG")        a_df_p = readFlagFeature(descr_p, false);
            else if (a_feature_keyword == "FLAG_LIST")   a_df_p = readFlagListFeature(descr_p, false);
            
            else if (a_feature_keyword == "ARRAY")       a_df_p = readArrayFeature(descr_p, domain);
            
            
            else if (a_feature_keyword == "ARRAY_TO_SINGLE") a_df_p = readArrayToSingleFeature(descr_p, domain);
            else if (a_feature_keyword == "MATRIX")          a_df_p = readMatrixFeature(descr_p);
            
            else if (a_feature_keyword == "DATA")        a_df_p = readDataFeature(descr_p, DFT_DATA, false, true);
            else if (a_feature_keyword == "SUBOBJECT")        a_df_p = readDataFeature(descr_p, DFT_SUBOBJECT, false);
            else if (a_feature_keyword == "TOOL")        a_df_p = readToolFeature(descr_p, false);
            else if (a_feature_keyword == "FUNCTION")          a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION, false);         
            else if (a_feature_keyword == "TABLE")             a_df_p = readFunctionFeature(descr_p, FFTY_TABLE, false);            
            else if (a_feature_keyword == "DEFINE_FUNCTION")   a_df_p = readFunctionFeature(descr_p, FFTY_DEFINE_FUNCTION, false);
            else if (a_feature_keyword == "FUNCTION_OR_TABLE") a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION_OR_TABLE, false); 
            else if (a_feature_keyword == "FUNCTION_OR_DEFINE_FUNCTION")   a_df_p = readFunctionFeature(descr_p, FFTY_FUNCTION_OR_DEFINE_FUNCTION, false);
            else if (a_feature_keyword == "SUPPORT")     a_df_p = readSupportFeature(descr_p);
            else if (a_feature_keyword == "APPEND")      a_df_p = readAppendFeature(descr_p);
            else if (a_feature_keyword == "SEPARATOR")   a_df_p = readSeparatorFeature();
            else if (a_feature_keyword == "ASSIGN")      a_df_p = readAssignFeature(descr_p, DFT_ASSIGN);
            else if (a_feature_keyword == "graphical")   a_is_graphical = true;
            else if (a_feature_keyword == "if")       a_df_p = readIfFeature(descr_p, domain);
            else if (a_feature_keyword == "optional" || a_feature_keyword == "mandatory") {
                setOptional(a_feature_keyword == "optional");
                if (getNextChar() != ':') throwError(getMsg(13), ":");
            }
            else {
                throwError(getMsg(22), a_feature_keyword.c_str());
            }
            //
            if (a_df_p != NULL)
            {
                a_df_p->setOptional(isOptional());
                a_df_p->setGraphical(a_is_graphical);
                a_is_graphical = false;
                a_dfl.push_back(a_df_p);
                a_reduced_dfl.push_back(a_df_p);
                // Adding "if" depending features into the descriptor's feature list
                if (a_df_p->getType() == DFT_IF) {
                    const MvDataIfFeature_t* a_dif_p = (const MvDataIfFeature_t*)a_df_p;
                    MvDataFeatureList_t::const_iterator a_it_begin, a_it_end, a_it;
                    // Default right features
                    const MvDataFeatureList_t& a_r_dftl = a_dif_p->getDefaultFeaturePtrList();
                    a_it_begin = a_r_dftl.begin();
                    a_it_end = a_r_dftl.end();
                    for (a_it = a_it_begin; a_it != a_it_end; ++a_it) a_dfl.push_back(*a_it);
                    // Default wrong features
                    const MvDataFeatureList_t& a_w_dftl = a_dif_p->getDefaultWrongFeaturePtrList();
                    a_it_begin = a_w_dftl.begin();
                    a_it_end = a_w_dftl.end();
                    for (a_it = a_it_begin; a_it != a_it_end; ++a_it) a_dfl.push_back(*a_it);
                }
                // Adding the feature into the feature list
                MvDataFeatureType_e a_df_type = a_df_p->getType();
                if (((a_df_type == DFT_DYNAMIC_ARRAY) || (a_df_type == DFT_STATIC_ARRAY)) && a_df_p->isGraphical()) {
                    MvDataArrayFeature_t* a_daf_p = (MvDataArrayFeature_t*)a_df_p;
                    int a_nb_features = a_daf_p->getNumber();
                    for (int i = 0; i < a_nb_features; ++i) {
                        MvDataFeature_t* a_loc_df_p = const_cast<MvDataFeature_t*>(a_daf_p->getDataFeature(i));
                        a_loc_df_p->setGraphical(true);
                    }
                }
            }
            c = getNextChar();
        }
        parallel_cards_to_if = false;
        setOptional(a_is_optional);
        /*if(a_cl.myFeaturePtrList.size()>0)*/
        a_cld.push_back(a_cl); 
        // Next test?
        a_continue = (a_cond_type == "if");
        if (a_continue) {
            a_cond_type = getNextString();
            if (a_cond_type != "else") {
                // to break the parallel cards, else the following cards after } will be read
                if (c != '}')
                {
                    parallel_cards_to_if = true;
                }
                a_continue = false;
                int a_size = (int)(a_cond_type.size());
                while (a_size--) unreadChar();
            }
            else {
                if (getNextChar() != '{') {
                    unreadChar();
                    a_cond_type = getNextString();
                    if (a_cond_type != "if") throwError(getMsg(44));
                }
                else {
                    unreadChar();
                }
            }
        }
    }
    /* Creating */
    bool a_default = true;
    int  a_nb_tests = (int)(a_cld.size());
    if (a_cld[--a_nb_tests].myExprPtr != NULL) { ++a_nb_tests; a_default = false; }
    MvDataIfFeature_t* a_dif_p = new MvDataIfFeature_t(a_nb_tests);
    for (int i = 0; i < a_nb_tests; ++i) {
        MvConditionalList_t& a_cl = a_cld[i];
        a_dif_p->setTest(descr_p, i,
            a_cl.myTestFeaturePtrSet, a_cl.myExprPtr,
            a_cl.myFeaturePtrList, a_cl.myFeaturePtrReducedList);
    }
    if (a_default) a_dif_p->setDefault(a_cld[a_nb_tests].myFeaturePtrList);
    if (a_default) a_dif_p->setDefaultReduced(a_cld[a_nb_tests].myFeaturePtrReducedList);
    a_dif_p->updateWrongFeaturePtrLists();
    //
    return (MvDataFeature_t*)a_dif_p;
}

MvDataFeature_t* MvDescriptorParser_t::readSeparatorFeature() {
    string a_title = "";
    bool is_open = false;
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    if (getNextChar() != ')') {
        unreadChar();
        a_title = getNextQuotedString();
        char a_char = getNextChar();
        /*RC_19_11_2010-Added OPEN option*/
        if (a_char == ',')
        {
            is_open = (getNextString() == "OPEN");
            a_char = getNextChar();
        }
        if (a_char != ')') throwError(getMsg(13), ")");
    }
    // Block checking
    char a_char = getNextChar();
    if (a_char == '{') {
        while (getNextChar() != '}') {
            unreadChar();
            string a_feature_keyword = getNextString();
            if (getNextChar() != '=') throwError(getMsg(13), "=");
            string a_feature_value = getNextQuotedString();
            if (getNextChar() != ';') throwError(getMsg(13), ";");
            //
            if (a_feature_keyword == "TITLE") {
                a_title = a_feature_value;
            }
            else {
                throwError(getMsg(20), a_feature_keyword.c_str());
            }
        }
    }
    else if (a_char != ';') {
        throwError(getMsg(13), ";");
    }
    //
    return (MvDataFeature_t*)(new MvDataSeparatorFeature_t(a_title, is_open));
}

MvDataFeature_t* MvDescriptorParser_t::readUnitFeature()
{
    string a_title = "";
    /* Parsing */
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    if (getNextChar() != ')') {
        unreadChar();
        a_title = getNextQuotedString();
        if (getNextChar() != ')') throwError(getMsg(13), ")");
    }

    char a_char = getNextChar();
    if (a_char != ';') throwError(getMsg(13), ";");
    int ikeyword = ((CFGKernel*)(mycfgkernel))->get_ikeyword(myObjectType, "UNIT");
    return (MvDataFeature_t*)(new MvDataUnitFeature_t(a_title, ikeyword));
}

/* --------- FORMAT (I/O) --------- */

void MvDescriptorParser_t::readFormat(MvDescriptor_t* descr_p) {
    // Which kind of file?
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    string         a_ff_str = getNextString();
    MvFileFormat_e a_ff_id = MV_get_file_format(a_ff_str);
    if (a_ff_id == FF_UNKNOWN) throwError(getMsg(36), a_ff_str.c_str());
    if (getNextChar() != ')') throwError(getMsg(13), ")");
    // Read cards
    LocCardList_t a_card_list;
    if (getNextChar() != '{') throwError(getMsg(13), "{");
    string a_card_type = ""; 
    while (getNextChar() != '}') {
        unreadChar();
        if (a_card_type == "") a_card_type = getNextString();
        a_card_type = readNextCard(descr_p, a_card_type, (PseudoCardList_t*)(&a_card_list));
        
    }
    // Creating a MCDS file format
    int a_nb_cards = (int)(a_card_list.size());
    fileformat_t* a_ff_p;
    MCDS_new_fileformat(&a_ff_p, a_nb_cards);
    for (int i = 0; i < a_nb_cards; ++i) MCDS_set_fileformat_card(a_ff_p, i, a_card_list[i]);
    // Adding the in MCDS file format the descriptor
    descr_p->addFileFormat(a_ff_id, a_ff_p);
}


string MvDescriptorParser_t::readNextCard(const MvDescriptor_t* descr_p, const string& card_type, PseudoCardList_t* card_list_p) {
    string a_card_type = "";                                                              
    //
    if (card_type == "CARD" || card_type == "HEADER" || card_type == "FF_CARD" || card_type == "CARD_PREREAD")  
        readCard(descr_p, card_list_p, false, card_type);  
    else if (card_type == "FREE_CARD")        readCard(descr_p, card_list_p, true, card_type);          
    else if (card_type == "BLANK")            readBlankCard(descr_p, card_list_p);
    else if (card_type == "COMMENT")          readCommentCard(descr_p, card_list_p);
    else if (card_type == "LIST")             readList(descr_p, card_list_p);               
    else if (card_type == "CELL_LIST")        readCellList(descr_p, card_list_p, false);     
    else if (card_type == "FREE_CELL_LIST")   readCellList(descr_p, card_list_p, true);      
    else if (card_type == "OBJECT_LIST")      readObjectList(descr_p, card_list_p, false);   
    else if (card_type == "FREE_OBJECT_LIST") readObjectList(descr_p, card_list_p, true);    
    else if (card_type == "CARD_LIST")        readCardList(descr_p, card_list_p, false);
    else if (card_type == "FREE_CARD_LIST") readCardList(descr_p, card_list_p, true);
    else if (card_type == "SUBOBJECTS")       readSubobjectsCard(descr_p, card_list_p);     
    else if (card_type == "if")               a_card_type = readIfCard(descr_p, card_list_p); 
    else if (card_type == "ASSIGN")           readCardAssign((MvDescriptor_t*)(descr_p), card_list_p);
    else                                   throwError(getMsg(37), card_type.c_str());
    return a_card_type;
}



ff_cell_t* MvDescriptorParser_t::readCell(const MvDescriptor_t* descr_p, const string& format) {
    ff_cell_t* a_cell_p = NULL;
    //
    switch (format[0]) {
    case '%':
    {
        string a_func_name = "";
        int a_ikeyword = getNextIKeyword(descr_p, &a_func_name);
        if (a_ikeyword == END_ARGS) {
            
            if (a_func_name == "DIRECTION") {
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                string a_dir_type = getNextString();
                if (a_dir_type == "RADIO") {
                    // Parsing radio direction
                    if (getNextChar() != ',') throwError(getMsg(13), ",");
                    int  a_radio_ikw = getNextIKeyword(descr_p);
                    char a_char = getNextChar();
                    int  a_is_extended = 1;
                    if (a_char == ')') {
                        unreadChar();
                    }
                    else {
                        if (a_char != ',') throwError(getMsg(13), ",");
                        a_is_extended = (getNextString() == "TRUE");
                    }
                    // Creating a radio direction cell
                    MCDS_new_ff_cell(&a_cell_p, CELL_DIR_RADIO);
                    MCDS_set_ff_cell_attributes(a_cell_p,
                        CELL_FORMAT, format.c_str(),
                        CELL_IKEYWORD, a_radio_ikw,
                        CELL_IS_EXTENDED, a_is_extended,
                        END_ARGS);
                }
                else if (a_dir_type == "FLAGS") {
                    // Parsing flags direction
                    if (getNextChar() != ',') throwError(getMsg(13), ",");
                    int a_xdir_ikw = getNextIKeyword(descr_p);
                    if (getNextChar() != ',') throwError(getMsg(13), ",");
                    int a_ydir_ikw = getNextIKeyword(descr_p);
                    if (getNextChar() != ',') throwError(getMsg(13), ",");
                    int a_zdir_ikw = getNextIKeyword(descr_p);
                    // Creating a flags direction cell
                    MCDS_new_ff_cell(&a_cell_p, CELL_DIR_FLAGS);
                    MCDS_set_ff_cell_attributes(a_cell_p,
                        CELL_FORMAT, format.c_str(),
                        CELL_DIRX_IKW, a_xdir_ikw,
                        CELL_DIRY_IKW, a_ydir_ikw,
                        CELL_DIRZ_IKW, a_zdir_ikw,
                        END_ARGS);
                }
                else {
                    throwError(getMsg(67), a_dir_type.c_str());
                }
                if (getNextChar() != ')') throwError(getMsg(13), ")");
            }
            else if (a_func_name == "DIGITS") {
                MvIKeywordList_t a_ikeywords;
                a_ikeywords.reserve(10);
                char a_char = getNextChar();
                if (a_char != '(') throwError(getMsg(13), "(");
                do {
                    int a_likeyword = getNextIKeyword(descr_p);
                    a_ikeywords.push_back(a_likeyword);
                    a_char = getNextChar();
                    if (a_char != ',' && a_char != ')') throwError(getMsg(13), ",");
                } while (a_char != ')');
                //
                int a_nb_ikeywords = (int)(a_ikeywords.size());
                MCDS_new_ff_cell(&a_cell_p, CELL_DIGITS);
                MCDS_set_ff_cell_attributes(a_cell_p,
                    CELL_FORMAT, format.c_str(),
                    CELL_NB_IKEYWORDS, a_nb_ikeywords,
                    END_ARGS);
                for (int i = 0; i < a_nb_ikeywords; ++i) {
                    int a_likeyword = a_ikeywords[i];
                    MCDS_set_ff_cell_tab(a_cell_p, CELL_IKEYWORD, i, (const void*)(&a_likeyword));
                }
                
            }
            else if ((a_func_name == "SCALAR_OR_FUNCTION") || (a_func_name == "SCALAR_OR_OBJECT")) {
                // Parsing keywords
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                int a_flag_ikw = getNextIKeyword(descr_p);
                if (getNextChar() != ',') throwError(getMsg(13), ",");
                int a_scalar_ikw = getNextIKeyword(descr_p);
                if (getNextChar() != ',') throwError(getMsg(13), ",");
                int a_object_ikw = getNextIKeyword(descr_p);
                if (getNextChar() != ')') throwError(getMsg(13), ")");
                // Creating a SCALAR_OR_FUNCTION cell
                MCDS_new_ff_cell(&a_cell_p, CELL_SCALAR_OR_OBJECT);
                MCDS_set_ff_cell_attributes(a_cell_p,
                    CELL_FORMAT, format.c_str(),
                    CELL_FLAG_IKW, a_flag_ikw,
                    CELL_SCALAR_IKW, a_scalar_ikw,
                    CELL_OBJECT_IKW, a_object_ikw,
                    END_ARGS);
                
                
            }
            else if (a_func_name == "FLAGGED_OBJECT") {
                // Parsing keywords
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                int a_object_ikw = getNextIKeyword(descr_p);
                if (getNextChar() != ',') throwError(getMsg(13), ",");
                int a_flag_ikw = getNextIKeyword(descr_p);
                if (getNextChar() != ')') throwError(getMsg(13), ")");
                // Creating a FLAGGED_FUNCTION cell
                MCDS_new_ff_cell(&a_cell_p, CELL_FLAGGED_OBJECT);
                MCDS_set_ff_cell_attributes(a_cell_p,
                    CELL_FORMAT, format.c_str(),
                    CELL_OBJECT_IKW, a_object_ikw,
                    CELL_FLAG_IKW, a_flag_ikw,
                    END_ARGS);
                
            
            }
            else if ((a_func_name == "SCALAR_OR_STRING")) {
                // Parsing keywords
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                int a_flag_ikw = getNextIKeyword(descr_p);
                if (getNextChar() != ',') throwError(getMsg(13), ",");
                int a_scalar_ikw = getNextIKeyword(descr_p);
                if (getNextChar() != ',') throwError(getMsg(13), ",");
                int a_string_ikw = getNextIKeyword(descr_p);
                if (getNextChar() != ')') throwError(getMsg(13), ")");
                // Creating a SCALAR_OR_FUNCTION cell
                MCDS_new_ff_cell(&a_cell_p, CELL_SCALAR_OR_STRING);
                MCDS_set_ff_cell_attributes(a_cell_p,
                    CELL_FORMAT, format.c_str(),
                    CELL_FLAG_IKW, a_flag_ikw,
                    CELL_SCALAR_IKW, a_scalar_ikw,
                    CELL_STRING_IKW, a_string_ikw,
                    END_ARGS);
                
            }
            else if ((a_func_name == "CELL_COND")) {
                a_cell_p = readIfCell(descr_p, format);
                MCDS_set_ff_cell_attributes(a_cell_p, CELL_FORMAT, format.c_str(), END_ARGS);
            }
            else if ((a_func_name == "CELL_PAIR")) {
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                int a_object_ikw = getNextIKeyword(descr_p);
                if (getNextChar() != ')') throwError(getMsg(13), ")");
                MCDS_new_ff_cell(&a_cell_p, CELL_PAIR);
                MCDS_set_ff_cell_attributes(a_cell_p,
                    CELL_FORMAT, format.c_str(),
                    CELL_IKEYWORD, a_object_ikw,
                    END_ARGS);
            }
            else if (a_func_name == "NAME_VALUE")
            {
                MCDS_new_ff_cell(&a_cell_p, CELL_NAME_VALUE);
                if (getNextChar() != '(') throwError(getMsg(13), "(");
                int nb_cells = 0;
                LocCellList_t a_cell_list;
                int i = 0;
                vector< pair <int, string> > myIkwmap;

                char c = getNextChar();
                if (c != '[')
                {
                    throwError(getMsg(13), "[");
                }
                while (c == '[')
                {
                    int an_ikeyword = END_ARGS;
                    string a_skeyword = getNextString();
                    an_ikeyword = descr_p->getIKeyword(a_skeyword);
                    if (an_ikeyword == END_ARGS)
                    {
                        if (a_skeyword == "")
                        {
                            throwError(getMsg(20), a_skeyword.c_str());
                        }
                    }
                    if (getNextChar() == ',')
                    {
                        a_skeyword = getNextQuotedString();
                        if (getNextChar() != ']')
                        {
                            throwError(getMsg(13), "]");
                        }
                        if (getNextChar() != ',')
                        {
                            throwError(getMsg(13), ",");
                        }
                    }
                    else
                    {
                        a_skeyword = "";
                        if (getNextChar() != ',')
                        {
                            throwError(getMsg(13), ",");
                        }
                    }

                    nb_cells++;
                    myIkwmap.push_back(make_pair(an_ikeyword, a_skeyword));
                    c = getNextChar();
                }

                MCDS_set_ff_cell_attributes(a_cell_p, CELL_NAME_VALUE_NUMBER, nb_cells, END_ARGS);
                vector< pair <int, string> >::iterator iter_b = myIkwmap.begin();
                vector< pair <int, string> >::iterator iter_e = myIkwmap.end();
                vector< pair <int, string> >::iterator iter;
                for (iter = iter_b; iter != iter_e; ++iter)
                {
                    int ikw = iter->first;
                    string a_skeyword = iter->second;
                    MCDS_set_ff_cell_tab(a_cell_p, CELL_NAME_VALUE_IKEYWORD, i, &ikw);
                    MCDS_set_ff_cell_tab(a_cell_p, CELL_NAME_VALUE_STRING, i, a_skeyword.c_str());
                    i++;
                }

                unreadChar();
                string qstr = getNextQuotedString();
                char pair_char = qstr[0];
                if (getNextChar() != ',')
                {
                    throwError(getMsg(13), ",");
                }
                qstr = getNextQuotedString();
                char separator = qstr[0];
                if (getNextChar() != ')')
                {
                    throwError(getMsg(13), ")");
                }

                MCDS_set_ff_cell_attributes(a_cell_p, CARD_NAME_VALUE_PAIR_CHAR, pair_char, CARD_NAME_VALUE_PAIR_SEPARATOR, separator, END_ARGS);
                MCDS_set_ff_cell_attributes(a_cell_p, CELL_FORMAT, format.c_str(), END_ARGS);
            }
      		  else if ((a_func_name == "CELL_LIST")) {
       			 if (getNextChar() != '(') throwError(getMsg(13), "(");
       			 int a_ikw = getNextIKeyword(descr_p);
        		if(a_ikw == END_ARGS)
            		throwError(getMsg(13), "invalid attribute");
        		else {
            		attribute_type_e a_atype = descr_p->getAttributeType(a_ikw);
            		if(a_atype == ATYPE_VALUE || a_atype == ATYPE_SIZE)
                		throwError(getMsg(13), "invalid attribute type");
        	    }
        		int ind = -1;
        		char a_char = getNextChar();
        		if (a_char == ',')
        		{
            		string str = getNextString();
            		bool isValue = (mvc_isNumber(str.c_str()) == 1) ? true : false;
            		if (isValue == false)
                		throwError(getMsg(13), "invalid index");
            		else {
                		ind = atoi(str.c_str());
            		}
            		a_char = getNextChar();
        		}
        		if (a_char != ')') throwError(getMsg(13), ")");
        		MCDS_new_ff_cell(&a_cell_p, CELL_LIST);
        		MCDS_set_ff_cell_attributes(a_cell_p,
            	CELL_FORMAT, format.c_str(),
            	CELL_IKEYWORD, a_ikw,
            	CELL_LIST_INDEX, ind,
            	END_ARGS);
        	}
            else if ((a_func_name == MV_ID_KEYWORD)) {
                
                MCDS_new_ff_cell(&a_cell_p, CELL_ID);
                MCDS_set_ff_cell_attributes(a_cell_p,
                    CELL_FORMAT, format.c_str(),
                    END_ARGS);

            }
            else if (a_func_name == MV_BLANK_KEYWORD)
            {
                MCDS_new_ff_cell(&a_cell_p, CELL_BLANK);
                MCDS_set_ff_cell_attributes(a_cell_p, CELL_FORMAT, format.c_str(), END_ARGS);
            }
            else if (a_func_name == "APPEND_OPTIONS")
            {
                MCDS_new_ff_cell(&a_cell_p, CELL_APPEND_OPTIONS);
                MCDS_set_ff_cell_attributes(a_cell_p, CELL_FORMAT, format.c_str(), END_ARGS);

                if (getNextChar() != '(') throwError(getMsg(13), "(");
                char c = getNextChar();
                bool end_bracket_found = false;
                while (c != ')' && !end_bracket_found)
                {
                    if (c != '[') throwError(getMsg(13), "[");
                    string a_ikeyname = "";
                    string a_skw = getNextString();
                    vector<int> intvalues;
                    vector<bool> boolvalues;
                    vector<string> stringvalues;
                    vector<unsigned int> uintvalues;
                    vector<double> doublevalues;

                    vector<string> options;
                    if (a_skw != "")
                    {
                        int ikw = descr_p->getIKeyword(a_skw);
                        value_type_e a_vtype = descr_p->getValueType(ikw);

                        if (getNextChar() != '(') throwError(getMsg(13), "(");
                        c = getNextChar();

                        if (c == '{')
                        {
                            c = getNextChar();
                            string value_txt = "";
                            while (c != '}')
                            {
                                value_txt += c;
                                c = getNextChar();
                            }
                            if (c != '}') throwError(getMsg(13), "}");
                            if (getNextChar() != ')') throwError(getMsg(13), ")");
                            vector<string> key_vect;
                            StringTokenize(value_txt, key_vect, ",");
                            for (int i = 0; i < (int)key_vect.size(); i++)
                            {
                                if (a_vtype == VTYPE_INT)
                                {
                                    int value = atoi(key_vect[i].c_str());
                                    intvalues.push_back(value);
                                }
                                else if (a_vtype == VTYPE_FLOAT)
                                {
                                    double value = atof(key_vect[i].c_str());
                                    doublevalues.push_back(value);
                                }
                                else if (a_vtype == VTYPE_BOOL)
                                {
                                    bool value = false;
                                    if (key_vect[i] == "TRUE")
                                    {
                                        value = true;
                                    }
                                    boolvalues.push_back(value);
                                }
                                else if (a_vtype == VTYPE_UINT)
                                {
                                    unsigned int value = atoi(key_vect[i].c_str());
                                    uintvalues.push_back(value);
                                }
                                else if (a_vtype == VTYPE_STRING)
                                {
                                    stringvalues.push_back(key_vect[i]);
                                }
                            }
                            if (getNextChar() != ',') throwError(getMsg(13), ",");
                            c = getNextChar();
                            if (c != '{') throwError(getMsg(13), "{");
                            while (c != '}')
                            {
                                string option_name = getNextQuotedString();
                                options.push_back(option_name);
                                c = getNextChar();
                            }
                            if (c != '}') throwError(getMsg(13), "}");
                        }
                        else
                        {
                            string value_txt;
                            while (c != ')')
                            {
                                value_txt += c;
                                c = getNextChar();
                            }
                            if (c != ')') throwError(getMsg(13), ")");

                            if (a_vtype == VTYPE_INT)
                            {
                                int value = atoi(value_txt.c_str());
                                intvalues.push_back(value);
                            }
                            else if (a_vtype == VTYPE_FLOAT)
                            {
                                double value = atof(value_txt.c_str());
                                doublevalues.push_back(value);
                            }
                            else if (a_vtype == VTYPE_BOOL)
                            {
                                bool value = false;
                                if (value_txt == "TRUE")
                                {
                                    value = true;
                                }
                                boolvalues.push_back(value);
                            }
                            else if (a_vtype == VTYPE_UINT)
                            {
                                unsigned int value = atoi(value_txt.c_str());
                                uintvalues.push_back(value);
                            }
                            else if (a_vtype == VTYPE_STRING)
                            {
                                stringvalues.push_back(value_txt);
                            }

                            if (getNextChar() != ',') throwError(getMsg(13), ",");
                            string option_name = getNextQuotedString();
                            options.push_back(option_name);
                        }
                        if (getNextChar() != ']') throwError(getMsg(13), "]");

                        end_bracket_found = (getNextChar() == ')') ? true : false;
                        c = getNextChar();
                    }
                    else
                    {
                        throwError(getMsg(20), a_skw.c_str());
                    }
                    if (!intvalues.empty())
                    {
                        MCDS_add_ff_cell_attributes(a_cell_p, a_skw, intvalues, options, VTYPE_INT);
                    }
                    if (!boolvalues.empty())
                    {
                        MCDS_add_ff_cell_attributes(a_cell_p, a_skw, boolvalues, options, VTYPE_BOOL);
                    }
                    if (!stringvalues.empty())
                    {
                        MCDS_add_ff_cell_attributes(a_cell_p, a_skw, stringvalues, options, VTYPE_STRING);
                    }
                    if (!uintvalues.empty())
                    {
                        MCDS_add_ff_cell_attributes(a_cell_p, a_skw, uintvalues, options, VTYPE_UINT);
                    }
                    if (!doublevalues.empty())
                    {
                        MCDS_add_ff_cell_attributes(a_cell_p, a_skw, doublevalues, options, VTYPE_FLOAT);
                    }
                }
                unreadChar();
            }
            else {
                throwError(getMsg(20), a_func_name.c_str());
            }
            
        }
        else {
            MCDS_new_ff_cell(&a_cell_p, CELL_VALUE);
            MCDS_set_ff_cell_attributes(a_cell_p,
                CELL_FORMAT, format.c_str(),
                CELL_IKEYWORD, a_ikeyword,
                END_ARGS);
        }
    }
    break;
    
    default:
    {
        MCDS_new_ff_cell(&a_cell_p, CELL_COMMENT);
        MCDS_set_ff_cell_attributes(a_cell_p, CELL_STRING, format.c_str(), END_ARGS);
    }
    break;
    
    }
    //
    return a_cell_p;
}



void MvDescriptorParser_t::readCard(const MvDescriptor_t* descr_p, PseudoCardList_t* card_list_p, bool is_free, const string& card_type) {
    // Parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    //
    int a_free_ikw = END_ARGS;
    if (is_free) {
        a_free_ikw = getNextIKeyword(descr_p);
        if (getNextChar() != ',') throwError(getMsg(13), ",");
    }
    //
    string a_fmt_str = getNextQuotedString();
    
    LocFormatList_t a_fmt_list;
    LocCellList_t   a_cell_list;
    
    int err = loc_get_fmt(a_fmt_str, &a_fmt_list);
    if (err) throwError(getMsg(38), a_fmt_str.c_str());
    int a_nb_cells = (int)(a_fmt_list.size());
    //
    
    try {
        for (int i = 0; i < a_nb_cells; ++i) {
            if (a_fmt_list[i][0] == '%' && getNextChar() != ',') throwError(getMsg(13), ",");
            ff_cell_t* a_cell_p = readCell(descr_p, a_fmt_list[i]);
            a_cell_list.push_back(a_cell_p);
            loc_multi_array_not_acceptable(descr_p, a_cell_p);
        }
    }
    catch (MvError_t& a_error) {
        int a_nb_read_cells = (int)(a_cell_list.size());
        for (int i = 0; i < a_nb_read_cells; ++i) {
            MCDS_delete_ff_cell(a_cell_list[i]);
            myfree(a_cell_list[i]);
        }
        throw a_error;
    }
    
    //
    if (getNextChar() != ')') throwError(getMsg(13), ")");
    int flag = 0;
    char a_char = getNextChar();
    if (a_char == '{')
    {
        while (a_char != '}')
        {
            std::string a_feature = getNextString();
            if (a_feature == "NO_COMMENT")
                flag = flag | CARD_FLAG_NO_COMMENT;
            else if (a_feature == "FIXED_FORMAT")
                flag = flag | CARD_FLAG_NO_FREE_FORMAT;
            else if (a_feature == "NO_END")
                flag = flag | CARD_FLAG_NO_END;
            else throwError(getMsg(22), a_feature.c_str());
            a_char = getNextChar();
            if (!(a_char == ',' || a_char == ';')) throwError(getMsg(13), "Expected , or ;");
            if (a_char == ';')
            {
                a_char = getNextChar();
                if (a_char != '}') throwError(getMsg(13), "Expected }");
            }
        }
    }
    else if (a_char != ';') throwError(getMsg(13), ";");

    // Creating card
     //Added for the new card types  	
    ff_card_type_e type = CARD_SINGLE;
    if (card_type == "HEADER")
        type = CARD_HEADER;
    else if (card_type == "FF_CARD")
        type = CARD_FREE_FORMAT;
    else if (card_type == "CARD_PREREAD") 
        type = CARD_PREREAD;
    ff_card_t* a_card_p = loc_new_card(type, a_cell_list); 

    MCDS_set_ff_card_attributes(a_card_p, CARD_IS_FREE, a_free_ikw, END_ARGS);
    MCDS_set_ff_card_attributes(a_card_p, CARD_FLAGS, flag, END_ARGS);
    // Adding the card
    ((LocCardList_t*)card_list_p)->push_back(a_card_p);

}



void MvDescriptorParser_t::readCardAssign(const MvDescriptor_t* descr_p, PseudoCardList_t* card_list_p) {

    if (getNextChar() != '(') throwError(getMsg(13), "(");
    //
    LocCellList_t   a_cell_list;
    int a_ikeyword = 0;
    string str_io = "";
    string a_formula = "";
    value_type_e    assigned_valtype = VTYPE_UNKNOWN;
    try
    {
        //For first field
        // Append _IOMODE_ in the descriptor attribute list, when it is not already appended, when ASSIGN feature is defined using it
        string skeyword = "";
        a_ikeyword = getNextIKeyword(descr_p, &skeyword);
        if ((a_ikeyword == END_ARGS) && (skeyword == "_IOMODE_"))
        {
            const descriptor_t* cdescr_p = descr_p->getDescriptorPtr();
            if (cdescr_p)
            {
                MCDS_add_descriptor_value((descriptor_t *)cdescr_p, VTYPE_INT, CFG_IKEYWORD_IO_MODE, "_IOMODE_", "_IOMODE_");
                a_ikeyword = CFG_IKEYWORD_IO_MODE;
            }
        }
        assigned_valtype = descr_p->getValueType(a_ikeyword);
        if (getNextChar() != ',') throwError(getMsg(13), ",");

        //if(getNextChar()!='(') throwError(getMsg(13),"(");

        int    a_nb_parenthesis = 1;
        bool   b_square_parenthesis = false;
        while (a_nb_parenthesis > 0)
        {
            char a_char = getNextChar();
            // only for literals string NOT for use case of _erase, _find, _combine with quotes
            // ASSIGN(A, "abc");
            if (assigned_valtype == VTYPE_STRING && a_char == '"'  && a_formula.empty()) 
            {
                unreadChar();
                a_formula = getNextQuotedString();
                a_char = getNextChar();
            }
            if (a_char == ']' && b_square_parenthesis == false)
            {
                throwError(getMsg(13), "[");
            }
            if (a_char == '[')
                b_square_parenthesis = true;
            else if (a_char == ']')
                b_square_parenthesis = false;
            if (a_char == '(')
                ++a_nb_parenthesis;
            else if (a_char == ')')
                --a_nb_parenthesis;

            if ((a_char == ',') && (a_nb_parenthesis <= 1) && (b_square_parenthesis == false))
            {
                str_io = getNextString();
                if (str_io != "IMPORT" && str_io != "EXPORT") throwError(getMsg(13), ",");
            }
            else
            {
                if (a_nb_parenthesis > 0)
                {
                    if (a_char == ';')
                        throwError(getMsg(38), "ASSIGN CARD");
                    a_formula += a_char;
                }
                else if (a_nb_parenthesis < 1)
                {
                    if (a_char != ')') throwError(getMsg(13), ")");
                    if (getNextChar() != ';') throwError(getMsg(13), ";");
                }
            }
        }

    }
    catch (MvError_t& a_error) {
        throw a_error;
    }

    ff_card_t* a_card_p = NULL;
    readCardAssignData(descr_p, &a_card_p, a_ikeyword, a_formula, assigned_valtype, str_io);
    // Adding the card
    if (a_card_p != NULL)
        ((LocCardList_t*)card_list_p)->push_back(a_card_p);
}

void MvDescriptorParser_t::readCardAssignData(const MvDescriptor_t* descr_p, ff_card_t** card_p, int ikeyword, string &formula, value_type_e &assigned_valtype, string &str_io)
{
    string firstWord;
    value_type_e first_key_valtype = VTYPE_UNKNOWN, second_key_valtype = VTYPE_UNKNOWN;
    firstWord = formula.substr(0, formula.find("("));

    assign_operator_e  assign_card_mode = ASSIGN_UNKNOWN;
    if (firstWord == "_ADD" || firstWord == "_SUB" || firstWord == "_DIV" || firstWord == "_MUL")
    {
        string firstAtt, secondAtt;
        bool isFValue = false, isSValue = false;
        int fikeyword = 0, sikeyword = 0;
        // assign "assign_card_type"
        if (firstWord == "_ADD")
        {
            assign_card_mode = ASSIGN_ADD;
        }
        else if (firstWord == "_SUB")
        {
            assign_card_mode = ASSIGN_SUB;
        }
        else if (firstWord == "_MUL")
        {
            assign_card_mode = ASSIGN_MUL;
        }
        else if (firstWord == "_DIV")
        {
            assign_card_mode = ASSIGN_DIV;
        }
        int rtype = MCDS_new_ff_assign_card(card_p, assign_card_mode);
        if (rtype)
            return;
        ff_card_assign_basic_operations_t* a_basic_opera_assign_card_p = (ff_card_assign_basic_operations_t*)(*card_p);

        int comma = 0;
        size_t len_Atype = firstWord.length(), len_a_formula = formula.length();
        for (size_t i = len_Atype + 1; i < len_a_formula - 1; ++i)
        {
            if (formula[i] == ',')
            {
                comma += 1;
                i++;
                if (comma > 1)
                    throwError(getMsg(48), ",");
            }
            if (formula[i] == ')')
                break;
            if (comma == 0)
            {
                firstAtt += formula[i];
            }
            else
            {
                secondAtt += formula[i];
            }
        }

        if (firstAtt == "")
            throwError(getMsg(13), "First argument");
        if (secondAtt == "")
            throwError(getMsg(13), "Second argument");
        fikeyword = descr_p->getIKeyword(firstAtt);
        if (fikeyword <= 0)
            isFValue = (mvc_isNumber(firstAtt.c_str()) == 1) ? true : false;
        if (fikeyword <= 0 && isFValue == false)
            throwError(getMsg(20), firstAtt.c_str());
        if (fikeyword > 0)
        {
            first_key_valtype = descr_p->getValueType(fikeyword);
            if (first_key_valtype == VTYPE_STRING)
                throwError(getMsg(84), firstAtt.c_str(), "STRING");
            else if (first_key_valtype == VTYPE_OBJECT)
                throwError(getMsg(84), firstAtt.c_str(), "OBJECT");
            else
                a_basic_opera_assign_card_p->first_ikey = fikeyword;
        }
        else if (fikeyword <= 0 && isFValue == true)
            a_basic_opera_assign_card_p->firstVal = atof(firstAtt.c_str());

        sikeyword = descr_p->getIKeyword(secondAtt);
        if (sikeyword <= 0)
            isSValue = (mvc_isNumber(secondAtt.c_str()) == 1) ? true : false;
        if (sikeyword <= 0 && isSValue == false)
            throwError(getMsg(20), secondAtt.c_str());
        if (sikeyword > 0)
        {
            second_key_valtype = descr_p->getValueType(sikeyword);
            if (second_key_valtype == VTYPE_STRING)
                throwError(getMsg(84), secondAtt.c_str(), "STRING");
            else if (second_key_valtype == VTYPE_OBJECT)
                throwError(getMsg(84), secondAtt.c_str(), "OBJECT");
            else
                a_basic_opera_assign_card_p->second_ikey = sikeyword;
        }
        else if (sikeyword <= 0 && isSValue == true)
            a_basic_opera_assign_card_p->secondVal = atof(secondAtt.c_str());
    }
    else if (firstWord == "_ATTRIB")
    {
        assign_card_mode = ASSIGN_ATTRIB;
        string firstAtt = "", secondAtt = "", thirdAtt = "";
        int fikeyword = 0, sikeyword = 0, tikeyword = 0;
        int comma = 0;
        size_t len_Atype = firstWord.length(), len_a_formula = formula.length();
        for (size_t i = len_Atype + 1; i < len_a_formula - 1; ++i)
        {
            if (formula[i] == ',')
            {
                comma += 1;
                i++;
                if (comma > 2)
                    throwError(getMsg(48), ",");
            }
            if (formula[i] == ')')
                break;
            if (comma == 0)
                firstAtt += formula[i];
            else if (comma == 1)
                secondAtt += formula[i];
            else
                thirdAtt += formula[i];
        }
        MCDS_new_ff_assign_card(card_p, assign_card_mode);
        ff_card_assign_Copy_t* a_copy_assign_card_p = (ff_card_assign_Copy_t *)(*card_p);
        if (firstAtt == "")
            throwError(getMsg(13), "First argument");
        fikeyword = descr_p->getIKeyword(firstAtt);
        attribute_type_e assigned_attrib_atype = descr_p->getAttributeType(ikeyword);
        if (fikeyword > 0)
        {
            value_type_e f_attrib_vtype = descr_p->getValueType(fikeyword);
            // allowing UINT assign to a size attribute(INT type), throw error, when FLOAT, STRING assign to a INT, UINT, size attribute
            if (((f_attrib_vtype != assigned_valtype) && (assigned_attrib_atype != ATYPE_SIZE)) ||
                ((assigned_attrib_atype == ATYPE_SIZE) && ((f_attrib_vtype == VTYPE_FLOAT) || (f_attrib_vtype == VTYPE_STRING)) && ((assigned_valtype == VTYPE_INT) || (assigned_valtype == VTYPE_UINT))))
                throwError(getMsg(13), "Value types are different");
            attribute_type_e f_attrib_atype = descr_p->getAttributeType(fikeyword);
            if (f_attrib_atype == ATYPE_SIZE || f_attrib_atype == ATYPE_VALUE)
            {
                if (assigned_attrib_atype == ATYPE_STATIC_ARRAY || assigned_attrib_atype == ATYPE_DYNAMIC_ARRAY)
                    throwError(getMsg(13), "Cannot Assign Single Attribute to Array Attribute");
                else if (secondAtt != "")
                    throwError(getMsg(13), "No Index required for Attribute of Type Single");
            }
            a_copy_assign_card_p->ikeyword = fikeyword;
        }
        else
            throwError(getMsg(13), "Undefined First attribute");

        if (secondAtt == "")
            sikeyword = -1;
        else
        {
            sikeyword = descr_p->getIKeyword(secondAtt);
            if (sikeyword > 0)
            {
                value_type_e s_attrib_vtype = descr_p->getValueType(sikeyword);
                if (!(s_attrib_vtype == VTYPE_INT || s_attrib_vtype == VTYPE_UINT))
                    throwError(getMsg(13), "Expected Integer or Unsigned Integer");
            }
            else
                throwError(getMsg(13), "Undefined Second attribute");
        }
        a_copy_assign_card_p->index_ikey = sikeyword;
        if (thirdAtt == "")
            tikeyword = -1;
        else
        {
            if (assigned_attrib_atype == ATYPE_SIZE || assigned_attrib_atype == ATYPE_VALUE)
                throwError(getMsg(13), "Values are expected to be copied to Array Attribute");
            tikeyword = descr_p->getIKeyword(thirdAtt);
            if (tikeyword > 0)
            {
                if (sikeyword > 0)
                {
                    value_type_e t_attrib_vtype = descr_p->getValueType(tikeyword);
                    if (!(t_attrib_vtype == VTYPE_INT || t_attrib_vtype == VTYPE_UINT))
                        throwError(getMsg(13), "Expected Integer or Unsigned Integer");
                }
                else
                    throwError(getMsg(13), "Third argument cannot be defined with out second argument");
            }
            else
                throwError(getMsg(13), "Undefined third argument");
        }
        a_copy_assign_card_p->last_index_ikey = tikeyword;
    }
    else if (firstWord == "_GET_ENTITY_VALUE")
    {
        string firstAtt = "", secondAtt = "", thirdAtt = "", forthAtt = "";
        int fikeyword = 0, tikeyword = 0, forth_ikeyword = 0;
        int comma = 0;
        size_t len_Atype = firstWord.length(), len_a_formula = formula.length();
        for (size_t i = len_Atype + 1; i < len_a_formula - 1; ++i)
        {
            if (formula[i] == ',')
            {
                comma += 1;
                i++;
                if (comma > 3)
                    throwError(getMsg(48), ",");
            }
            if (formula[i] == ')')
                break;
            if (comma == 0)
            {
                firstAtt += formula[i];
            }
            else if (comma == 1)
            {
                secondAtt += formula[i];
            }
            else if (comma == 2)
            {
                thirdAtt += formula[i];
            }
            else
            {
                forthAtt += formula[i];
            }
        }
        if (firstAtt[0] == '\"')
            assign_card_mode = ASSIGN_GET_CURRENT_ENTITY;
        else
            assign_card_mode = ASSIGN_GET_ENTITY_VALUE;
        MCDS_new_ff_assign_card(card_p, assign_card_mode);
        ff_card_assign_entity_value_t* a_ent_val_assign_card_p = (ff_card_assign_entity_value_t *)(*card_p);
        if (firstAtt[0] == '\"')
        {
            char obj_type_str[100];
            int size = (int)firstAtt.size();
            int j = 0;
            for (int i = 0; i < size; i++)
            {
                // skip quotes
                if (i == 0 || i == (size - 1))
                {
                    continue;
                }
                obj_type_str[j] = firstAtt[i];
                j++;
            }
            obj_type_str[j] = '\0';
            a_ent_val_assign_card_p->objTypeStr = my_strcpy(a_ent_val_assign_card_p->objTypeStr, obj_type_str);
        }
        else
        {
            fikeyword = descr_p->getIKeyword(firstAtt);
            value_type_e obj_type = descr_p->getValueType(fikeyword);
            if (obj_type != VTYPE_OBJECT)
                throwError(getMsg(24), firstAtt.c_str());
            else
            {
                a_ent_val_assign_card_p->entity_ikey = fikeyword;
            }
            if (thirdAtt == "")
                a_ent_val_assign_card_p->row_ikey = 0;
            else
            {
                tikeyword = descr_p->getIKeyword(thirdAtt);
                value_type_e t_val_type = descr_p->getValueType(tikeyword);
                attribute_type_e t_attrib_atype = descr_p->getAttributeType(tikeyword);
                if (!(t_val_type == VTYPE_INT || t_val_type == VTYPE_UINT))
                    throwError(getMsg(59), thirdAtt.c_str(), "is expected to be an", "Integer or Unsigned Integer");
                if (t_attrib_atype != ATYPE_VALUE)
                    throwError(getMsg(84), thirdAtt.c_str(), "Expected single attribute");
                a_ent_val_assign_card_p->row_ikey = tikeyword;
            }
            if (forthAtt == "")
                a_ent_val_assign_card_p->col_ikey = 0;
            else
            {
                forth_ikeyword = descr_p->getIKeyword(forthAtt);
                value_type_e forth_val_type = descr_p->getValueType(forth_ikeyword);
                attribute_type_e t_attrib_atype = descr_p->getAttributeType(forth_ikeyword);
                if (!(forth_val_type == VTYPE_INT || forth_val_type == VTYPE_UINT))
                    throwError(getMsg(59), forthAtt.c_str(), "is expected to be an", "Integer or Unsigned Integer");
                if (t_attrib_atype != ATYPE_VALUE)
                    throwError(getMsg(84), forthAtt.c_str(), "Expected single attribute");
                a_ent_val_assign_card_p->col_ikey = forth_ikeyword;
            }
        }
        a_ent_val_assign_card_p->value_Skey = my_strcpy(a_ent_val_assign_card_p->value_Skey, secondAtt.c_str());
    }
    else if (firstWord == "_GET_NLOOKUP_VALUE")
    {
        string firstAtt, secondAtt, thirdAtt;
        bool isFValue = false, isSValue = false;
        int t_att_ikeyword = 0;
        value_type_e   t_att_valtype = VTYPE_UNKNOWN;
        assign_card_mode = ASSIGN_GET_NLOOKUP_VALUE;

        int rtype = MCDS_new_ff_assign_card(card_p, assign_card_mode);
        if (rtype)
            return;
        ff_card_assign_nlookup_t* a_nlookup_assign_card_p = (ff_card_assign_nlookup_t *)(*card_p);

        int comma = 0;
        size_t len_Atype = firstWord.length(), len_a_formula = formula.length();
        for (size_t i = len_Atype + 1; i < len_a_formula - 1; ++i)
        {
            if (formula[i] == ',')
            {
                comma += 1;
                i++;
                if (comma > 2)
                    throwError(getMsg(48), ",");
            }
            if (formula[i] == ')')
                break;
            if (comma == 0)
                firstAtt += formula[i];
            else if (comma == 1)
                secondAtt += formula[i];
            else
                thirdAtt += formula[i];
        }

        if (firstAtt == "")
            throwError(getMsg(13), "First argument");
        if (secondAtt == "")
            throwError(getMsg(13), "Second argument");
        if (thirdAtt == "")
            throwError(getMsg(13), "Third argument");

        isFValue = (mvc_isNumber(firstAtt.c_str()) == 1) ? true : false;
        if (isFValue == false)
            throwError(getMsg(20), firstAtt.c_str());
        else
            a_nlookup_assign_card_p->table_num1_val = atoi(firstAtt.c_str());
        isSValue = (mvc_isNumber(secondAtt.c_str()) == 1) ? true : false;
        if (isSValue == false)
            throwError(getMsg(20), secondAtt.c_str());
        else
            a_nlookup_assign_card_p->table_num2_val = atoi(secondAtt.c_str());

        t_att_ikeyword = descr_p->getIKeyword(thirdAtt);

        if (t_att_ikeyword > 0)
        {
            t_att_valtype = descr_p->getValueType(t_att_ikeyword);
            if (t_att_valtype != VTYPE_OBJECT)
                throwError(getMsg(24), thirdAtt.c_str());
            else
                a_nlookup_assign_card_p->id_ikey = t_att_ikeyword;
        }
        else
            throwError(getMsg(13), "Third argument");
    }
    else if (firstWord == "_GET_NEXT_MAX_AVAILABLE_ID")
    {
        assign_card_mode = ASSIGN_GET_NEXT_MAX_AVAILABLE_ID;
        int rtype = MCDS_new_ff_assign_card(card_p, assign_card_mode);
        if (rtype)
            return;
        size_t len_Atype = firstWord.length();
        if (formula[len_Atype + 1] != ')')
            throwError(getMsg(38), "Assign Get_next_max_avail_id expected No Argument");
    }
    else if (firstWord == "_GET_DISPLAY_STATUS" || firstWord == "_PUSH")
    {
        string firstAtt = "";
        int fikeyword = 0;

        size_t len_Atype = firstWord.length(), len_a_formula = formula.length();
        for (size_t i = len_Atype + 1; i < len_a_formula - 1; ++i)
        {
            if (formula[i] == ',')
                throwError(getMsg(48), ",");
            if (formula[i] == ')')
                break;
            firstAtt += formula[i];
        }
        fikeyword = descr_p->getIKeyword(firstAtt);
        if (fikeyword <= 0)
            throwError(getMsg(20), firstAtt.c_str());
        if (firstWord == "_GET_DISPLAY_STATUS")
        {
            assign_card_mode = ASSIGN_GET_DISPLAY_STATUS;
            MCDS_new_ff_assign_card(card_p, assign_card_mode);
            ff_card_assign_displaystatus_t* a_assign_card_displaystatus_p = (ff_card_assign_displaystatus_t *)(*card_p);
            a_assign_card_displaystatus_p->att_ikey = fikeyword;
        }
        else if (firstWord == "_PUSH")
        {
            assign_card_mode = ASSIGN_PUSH;
            MCDS_new_ff_assign_card(card_p, assign_card_mode);
            attribute_type_e assigned_attr_type = descr_p->getAttributeType(ikeyword);
            if (assigned_attr_type != ATYPE_DYNAMIC_ARRAY)
                throwError(getMsg(13), "Assign_card attribute has to be of type Dynamic Array");
            value_type_e   f_att_valtype = descr_p->getValueType(fikeyword);

            if (assigned_valtype != VTYPE_STRING)
            {
                if (assigned_valtype == VTYPE_FLOAT)
                {
                    if (f_att_valtype == VTYPE_STRING)
                        throwError(getMsg(13), "String attributes cannot be pushed to Float array");
                }
                else if (assigned_valtype == VTYPE_UINT)
                {
                    if (!(f_att_valtype == VTYPE_UINT || f_att_valtype == VTYPE_OBJECT || f_att_valtype == VTYPE_BOOL))
                        throwError(getMsg(13), "Unsigned Integer attributes has to be pushed to Unsigned integer array");
                }
                else if (assigned_valtype == VTYPE_INT)
                {
                    if (!(f_att_valtype == VTYPE_INT || f_att_valtype == VTYPE_BOOL))
                        throwError(getMsg(13), "Integer attributes has to be pushed to Integer array");
                }
                else
                    throwError(getMsg(13), "Array has to be of type STRING or INTEGER or UNSIGNED INT or FLOAT");
            }
            ff_card_assign_push_t* a_assign_card_push_p = (ff_card_assign_push_t *)(*card_p);
            a_assign_card_push_p->att_ikey = fikeyword;
        }
    }
    else if (firstWord == "_GET_NB_FREE_CARDS")
    {
        assign_card_mode = ASSIGN_GET_NB_FREE_CARDS;
        int rtype = MCDS_new_ff_assign_card(card_p, assign_card_mode);
        if (rtype)
            return;
        size_t len_Atype = firstWord.length();
        if (formula[len_Atype + 1] != ')')
            throwError(getMsg(38), "Assign _GET_NB_FREE_CARDS expected No Argument");
    }
    else if (firstWord == "_GET_FORMAT_TYPE")
    {
        assign_card_mode = ASSIGN_GET_FORMAT_TYPE;
        int rtype = MCDS_new_ff_assign_card(card_p, assign_card_mode);
        if (rtype)
            return;
        size_t len_Atype = firstWord.length();
        if (formula[len_Atype + 1] != ')')
            throwError(getMsg(38), "Assign _GET_FORMAT_TYPE expected No Argument");
    }
    else if (firstWord == "_COMBINE" || firstWord == "_ERASE" || firstWord == "_FIND")
    {
        attribute_type_e assigned_attrib_atype = descr_p->getAttributeType(ikeyword);
        if (assigned_attrib_atype != ATYPE_VALUE)
            throwError(getMsg(38), "Only non array attributes are allowed");
        if (firstWord == "_COMBINE" || firstWord == "_ERASE")
        {
            if (assigned_valtype != VTYPE_STRING)
                throwError(getMsg(38), "Only attribute of type string is allowed");
        }
        else if (assigned_valtype != VTYPE_INT)
            throwError(getMsg(38), "Only attribute of type integer is allowed");

        string firstAtt, secondAtt;
        int fikeyword = 0, sikeyword = 0;
        // assign "assign_card_type"
        if (firstWord == "_COMBINE")
        {
            assign_card_mode = ASSIGN_COMBINE;
        }
        else if (firstWord == "_ERASE")
        {
            assign_card_mode = ASSIGN_ERASE;
        }
        else if (firstWord == "_FIND")
        {
            assign_card_mode = ASSIGN_FIND;
        }
        int rtype = MCDS_new_ff_assign_card(card_p, assign_card_mode);
        if (rtype)
            return;
        ff_card_assign_string_t* a_assign_string_card_p = (ff_card_assign_string_t *)(*card_p);

        int comma = 0;
        size_t len_Atype = firstWord.length(), len_a_formula = formula.length();
        for (size_t i = len_Atype + 1; i < len_a_formula - 1; ++i)
        {
            if (formula[i] == ',')
            {
                comma += 1;
                i++;
                if (comma > 1)
                    throwError(getMsg(48), ",");
            }
            if (formula[i] == ')')
                break;
            if (comma == 0)
            {
                firstAtt += formula[i];
            }
            else
            {
                secondAtt += formula[i];
            }
        }
        if (firstAtt == "")
            throwError(getMsg(13), "First argument");
        if (secondAtt == "")
            throwError(getMsg(13), "Second argument");
        bool is_string_filled = false;
        fikeyword = descr_p->getIKeyword(firstAtt);
        if (fikeyword > 0)
        {
            int valtype = descr_p->getValueType(fikeyword);
            if (valtype != VTYPE_STRING)
                throwError(getMsg(84), firstAtt.c_str(), "STRING");
            else
                a_assign_string_card_p->first_ikey = fikeyword;
        }
        else
        {
            if (firstAtt[0] != '"')
                throwError(getMsg(13), "First argument has to be an attribute of type string or it should be in quotes");
            string att_to_be_copied;
            size_t len = firstAtt.length();
            if (firstAtt[len - 1] != '"')
                throwError(getMsg(13), "First argument has to be an attribute of type string or it should be in quotes");
            for (size_t j = 1; j < len - 1; ++j)
                att_to_be_copied += firstAtt[j];
            a_assign_string_card_p->value_str = my_strcpy(a_assign_string_card_p->value_str, att_to_be_copied.c_str());
            is_string_filled = true;
        }

        sikeyword = descr_p->getIKeyword(secondAtt);
        if (sikeyword > 0)
        {
            int valtype = descr_p->getValueType(sikeyword);
            if (valtype != VTYPE_STRING)
                throwError(getMsg(84), secondAtt.c_str(), "STRING");
            else
                a_assign_string_card_p->second_ikey = sikeyword;
        }
        else
        {
            if (secondAtt[0] != '"')
                throwError(getMsg(13), "Second argument has to be an attribute of type string or it should be in quotes");
            else if (is_string_filled)
                throwError(getMsg(13), "Both are defined as string literals and it is not allowed");
            string att_to_be_copied;
            size_t len = secondAtt.length();
            if (secondAtt[len - 1] != '"')
                throwError(getMsg(13), "Second argument has to be an attribute of type string or it should be in quotes");
            for (size_t j = 1; j < len - 1; ++j)
                att_to_be_copied += secondAtt[j];
            a_assign_string_card_p->value_str = my_strcpy(a_assign_string_card_p->value_str, att_to_be_copied.c_str());
        }
    }
    else
    {
        assign_card_mode = ASSIGN_EXPRESSION;
        int rtype = MCDS_new_ff_assign_card(card_p, assign_card_mode);
        if (rtype)
            return;
        // ff_card_assign_header_t* a_header_assign_card_p = (ff_card_assign_header_t *)(*card_p);
    }

    ((ff_card_assign_header_t *)(*card_p))->assign_card_type = assign_card_mode;
    ((ff_card_assign_header_t *)(*card_p))->attribute_ikw = ikeyword;
    ((ff_card_assign_header_t *)(*card_p))->exp_str = my_strcpy(((ff_card_assign_header_t *)(*card_p))->exp_str, formula.c_str());
    if (str_io == "IMPORT" || str_io == "")
    {
        ((ff_card_assign_header_t *)(*card_p))->mode = ASSIGN_MODE_IMPORT;
    }
    else if (str_io == "EXPORT")
    {
        ((ff_card_assign_header_t *)(*card_p))->mode = ASSIGN_MODE_EXPORT;
    }
}

void MvDescriptorParser_t::readBlankCard(const MvDescriptor_t*, PseudoCardList_t* card_list_p) {
    // Parsing
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // Creating card
    ff_card_t* a_card_p;
    MCDS_new_ff_card(&a_card_p, CARD_BLANK);
    // Adding the card
    ((LocCardList_t*)card_list_p)->push_back(a_card_p);
}


void MvDescriptorParser_t::readCommentCard(const MvDescriptor_t* descr_p, PseudoCardList_t* card_list_p) {
    // Parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    string a_fmt_str = getNextQuotedString();
    LocFormatList_t a_fmt_list;
    LocCellList_t   a_cell_list;
    int err = loc_get_fmt(a_fmt_str, &a_fmt_list);
    if (err) throwError(getMsg(38), a_fmt_str.c_str());
    int a_nb_cells = (int)(a_fmt_list.size());
    if (a_nb_cells > 0)
        if (a_fmt_list[0][0] == '%') throwError(getMsg(38), "Comment card should not start with %");
    char c = getNextChar();
    unreadChar();
    if (c == ',')
    {
        try {
            for (int i = 0; i < a_nb_cells; ++i) {
                if (a_fmt_list[i][0] == '%' && getNextChar() != ',') throwError(getMsg(13), ",");
                ff_cell_t* a_cell_p = readCell(descr_p, a_fmt_list[i]);
                a_cell_list.push_back(a_cell_p);
                loc_multi_array_not_acceptable(descr_p, a_cell_p);
            }
        }
        catch (MvError_t& a_error) {
            int a_nb_read_cells = (int)(a_cell_list.size());
            for (int i = 0; i < a_nb_read_cells; ++i) {
                MCDS_delete_ff_cell(a_cell_list[i]);
                myfree(a_cell_list[i]);
            }
            throw a_error;
        }
    }
    else
    {
        string a_format = "";
        for (int i = 0; i < a_nb_cells; ++i) {
            a_format.append(a_fmt_list[i]);
        }
        ff_cell_t* a_cell_p = NULL;
        MCDS_new_ff_cell(&a_cell_p, CELL_COMMENT);
        MCDS_set_ff_cell_attributes(a_cell_p, CELL_STRING, a_format.c_str(), END_ARGS);
        a_cell_list.push_back(a_cell_p);
    }
    if (getNextChar() != ')') throwError(getMsg(13), ")");
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // Creating card
    ff_card_t* a_card_p = loc_new_card(CARD_COMMENT, a_cell_list);
    // Adding the card
    ((LocCardList_t*)card_list_p)->push_back(a_card_p);
}



void MvDescriptorParser_t::readList(const MvDescriptor_t* descr_p, PseudoCardList_t* card_list_p) {
    // Parsing 
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Format
    string          a_fmt_str = getNextQuotedString();
    LocFormatList_t a_fmt_list;
    LocCellList_t   a_cell_list;
    int a_err = loc_get_fmt(a_fmt_str, &a_fmt_list);
    if (a_err) throwError(getMsg(38), a_fmt_str.c_str());
    //
    int a_nb_cells = (int)(a_fmt_list.size());
    try {
        for (int i = 0; i < a_nb_cells; ++i) {
            if (a_fmt_list[i][0] == '%' && getNextChar() != ',') throwError(getMsg(13), ",");
            ff_cell_t* a_cell_p = readCell(descr_p, a_fmt_list[i]);
            a_cell_list.push_back(a_cell_p);
        }
    }
    catch (MvError_t& a_error) {
        int a_nb_read_cells = (int)(a_cell_list.size());
        for (int i = 0; i < a_nb_read_cells; ++i) {
            MCDS_delete_ff_cell(a_cell_list[i]);
            myfree(a_cell_list[i]);
        }
        throw a_error;
    }
    // Size max
    int  a_size_max = -1, ikey = -1;
    char a_char = getNextChar();
    if (a_char == ',') {
       // a_size_max = getNextInt();
        std::string skey;
        ikey = getNextIKeyword(descr_p, &skey);
        if (ikey <= 0)
        {
            try {
                a_size_max = std::stoi(skey);
            }
            catch (...) {
                if (useLocalMsg()) {
                    throwError("\"%s\" is not an integer", skey.c_str());
                }
                else {
                    throwError(getMsg(4), skey.c_str());
                }
            }
        }

        a_char = getNextChar();
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // Creating card
    ff_card_t* a_card_p = loc_new_card(CARD_LIST, a_cell_list);
    MCDS_set_ff_card_attributes(a_card_p, CARD_LENGTH_MAX, a_size_max, END_ARGS);
    MCDS_set_ff_card_attributes(a_card_p, CARD_IKEYWORD_NB_BLOCKS, ikey, END_ARGS);

    // Adding the card
    ((LocCardList_t*)card_list_p)->push_back(a_card_p);
}



void MvDescriptorParser_t::readCellList(const MvDescriptor_t* descr_p, PseudoCardList_t* card_list_p, bool is_free) {
    // Parsing 
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    
    // Size
    string a_size_skw;
    int  a_size = getNextIKeyword(descr_p, &a_size_skw);
    bool a_is_static = false;
    if (a_size == END_ARGS) {
        if (is_free) throwError(getMsg(20), a_size_skw.c_str());
        try { a_size = loc_string2int(a_size_skw); }
        catch (MvError_t&) { throwError(getMsg(20), a_size_skw.c_str()); }
        a_is_static = true;
    }
    if (a_is_static)                                        a_size = (-a_size);
    else if (descr_p->getAttributeType(a_size) != ATYPE_SIZE) throwError(getMsg(13), a_size_skw.c_str());
    
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    // Cells
    string a_fmt_str = getNextQuotedString();
    
    LocFormatList_t a_fmt_list;
    LocCellList_t   a_cell_list;
    
    int err = loc_get_fmt(a_fmt_str, &a_fmt_list);
    if (err) throwError(getMsg(38), a_fmt_str.c_str());
    int a_nb_cells = (int)(a_fmt_list.size());
    
    try {
        for (int i = 0; i < a_nb_cells; ++i) {
            if (a_fmt_list[i][0] == '%' && getNextChar() != ',') throwError(getMsg(13), ",");
            ff_cell_t* a_cell_p = readCell(descr_p, a_fmt_list[i]);
            a_cell_list.push_back(a_cell_p);
        }
    }
    catch (MvError_t& a_error) {
        int a_nb_read_cells = (int)(a_cell_list.size());
        for (int i = 0; i < a_nb_read_cells; ++i) {
            MCDS_delete_ff_cell(a_cell_list[i]);
            myfree(a_cell_list[i]);
        }
        throw a_error;
    }
    
    // Size max
    int  a_size_max = -1;
    char a_char = getNextChar();
    if (a_char == ',') {
        a_size_max = getNextInt();
        a_char = getNextChar();
    }
    if (a_char != ')') throwError(getMsg(13), ")");
    int flag = 0;
    a_char = getNextChar();
    string a_offset_fmt_str = "";
    string a_offset_val_str = "";
    if (a_char == '{')
    {
        while (a_char != '}')
        {
            std::string a_feature = getNextString();
            if (a_feature == "NO_COMMENT")
                flag = flag | CARD_FLAG_NO_COMMENT;
            else if (a_feature == "FIXED_FORMAT")
                flag = flag | CARD_FLAG_NO_FREE_FORMAT;
            else if (a_feature == "BLOCK_TOGETHER")
                flag = flag | CARD_FLAG_BLOCK_TOGETHER;
            else if (a_feature == "OFFSET")
            {
                flag = flag | CARD_FLAG_OFFSET;
                a_char = getNextChar();
                if (a_char != '(') throwError(getMsg(13), "(");
                a_offset_fmt_str = getNextQuotedString();
                int a_nb_chars = (int)(a_offset_fmt_str.size());
                if (a_nb_chars <= 0) throwError(getMsg(13), "offset_format");
                int i = 0, a_nb_format_specifiers = 0;
                while (i < a_nb_chars)
                {
                    a_nb_format_specifiers++;
                    if (a_nb_format_specifiers > 1)
                        throwError(getMsg(38), "Cell_list offset format More than one format specifier mentioned");
                    char c = a_offset_fmt_str[i];
                    if (c == '%')
                    {
                        while (c != 'f' && c != 'g' && c != 'd' && c != 'i' && c != 's' && c != 'u' && c != 'e' && c != 'E' && i < a_nb_chars) c = a_offset_fmt_str[++i];
                        if (c != 's') throwError(getMsg(58), "Offset Format specifier");
                        if (i >= a_nb_chars) throwError(getMsg(38), "Cell_list offset format");
                    }
                    else throwError(getMsg(38), "Cell_list offset format");
                    i++;
                }
                a_char = getNextChar();
                if (a_char != ',') throwError(getMsg(13), ",");
                a_offset_val_str = getNextQuotedString();
                a_char = getNextChar();
                if (a_char != ')') throwError(getMsg(13), ")");
            }
            else throwError(getMsg(22), a_feature.c_str());
            a_char = getNextChar();
            if (!(a_char == ',' || a_char == ';')) throwError(getMsg(13), "Expected , or ;");
            if (a_char == ';')
            {
                a_char = getNextChar();
                if (a_char != '}') throwError(getMsg(13), "Expected }");
            }
        }
    }
    else if (a_char != ';') throwError(getMsg(13), ";");
    // Creating card
    ff_card_t* a_card_p = loc_new_card(CARD_CELL_LIST, a_cell_list); 
    int a_is_free = (is_free ? 1 : 0);
    MCDS_set_ff_card_attributes(a_card_p, CARD_SIZE, a_size, CARD_LENGTH_MAX, a_size_max,
        CARD_IS_FREE, a_is_free, CARD_FLAGS, flag,
        CARD_CELL_LIST_OFFSET_FORMAT, a_offset_fmt_str.c_str(),
        CARD_CELL_LIST_OFFSET_VALUE, a_offset_val_str.c_str(), END_ARGS);
    // Adding the card
    ((LocCardList_t*)card_list_p)->push_back(a_card_p);
}


void MvDescriptorParser_t::readObjectList(const MvDescriptor_t* descr_p, PseudoCardList_t* card_list_p, bool is_free) {
    // Parsing 
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Cell format
    string a_cell_format = getNextQuotedString();
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    // Positive array 
    string a_pos_array_skw;
    int    a_pos_array_ikw = getNextIKeyword(descr_p, &a_pos_array_skw);
    if (descr_p->getAttributeType(a_pos_array_ikw) != ATYPE_DYNAMIC_ARRAY) {
        throwError(getMsg(80), a_pos_array_skw.c_str());
    }
    if (descr_p->getValueType(a_pos_array_ikw) != VTYPE_OBJECT) {
        throwError(getMsg(81), a_pos_array_skw.c_str());
    }
    int a_pos_size_ikw = descr_p->getSizeIKeyword(a_pos_array_ikw);
    loc_multi_array_not_acceptable(descr_p, a_pos_array_ikw);
    // Negative array
    int a_neg_size_ikw = END_ARGS;
    int a_neg_array_ikw = END_ARGS;
    char a_char = getNextChar();
    if (a_char == ',') {
        string a_neg_array_skw;
        a_neg_array_ikw = getNextIKeyword(descr_p, &a_neg_array_skw);
        if (descr_p->getAttributeType(a_neg_array_ikw) != ATYPE_DYNAMIC_ARRAY) {
            throwError(getMsg(80), a_neg_array_skw.c_str());
        }
        if (descr_p->getValueType(a_neg_array_ikw) != VTYPE_OBJECT) {
            throwError(getMsg(81), a_neg_array_skw.c_str());
        }
        a_neg_size_ikw = descr_p->getSizeIKeyword(a_neg_array_ikw);
        loc_multi_array_not_acceptable(descr_p, a_neg_array_ikw);
        //
        a_char = getNextChar();
    }
    //
    string            a_comment = "";
    MvObjectTypeSet_t a_otypes;
    a_otypes.insert(descr_p->getObjectType(a_pos_array_ikw));
    if (a_neg_array_ikw != END_ARGS) a_otypes.insert(descr_p->getObjectType(a_neg_array_ikw));
    //
    // Size max
    int  a_size_max = -1;
    if (a_char == ',') {
        a_size_max = getNextInt();
        a_char = getNextChar();
    }

    if (a_char != ')') throwError(getMsg(13), ")");
    a_char = getNextChar();
    if (a_char != ';' && a_char != '{') throwError(getMsg(13), "{");
    if (a_char == '{') while (getNextChar() != '}')
    {
        unreadChar();
        //
        string a_keyword = getNextString();
        if (a_keyword == "COMMENT")
        {
            if (getNextChar() != '=')
                throwError(getMsg(13), "=");
            a_comment = getNextQuotedString();
            if (getNextChar() != ';') throwError(getMsg(13), ";");
        }
        else if (a_keyword == "OBJECT_TYPES")
        {
            if (getNextChar() != '=')
                throwError(getMsg(13), "=");
            if (getNextChar() != '(')
                throwError(getMsg(13), "(");
            a_otypes.clear();
            do
            {
                getNextObjectTypes(&a_otypes);
                a_char = getNextChar();
                if (a_char != ')' && a_char != ',')
                    throwError(getMsg(13), ",");
            } while (a_char != ')');
            if (getNextChar() != ';')
                throwError(getMsg(13), ";");
        }
        else
        {
            throwError(getMsg(20), a_keyword.c_str());
        }
    }
    // Creating card
    ff_card_t* a_card_p = NULL;
    MCDS_new_ff_card(&a_card_p, CARD_OBJECT_LIST);
    int a_is_free = (is_free ? 1 : 0);
    MCDS_set_ff_card_attributes(a_card_p,
        CARD_STRING, a_comment.c_str(),
        CARD_CELL_FORMAT, a_cell_format.c_str(),
        CARD_IS_FREE, a_is_free,
        CARD_POS_SIZE, a_pos_size_ikw,
        CARD_POS_ARRAY, a_pos_array_ikw,
        CARD_NEG_SIZE, a_neg_size_ikw,
        CARD_NEG_ARRAY, a_neg_array_ikw,
        CARD_NB_OTYPES, a_otypes.size(),
        CARD_LENGTH_MAX, a_size_max,
        END_ARGS);
    int a_ind = 0;
    MvObjectTypeSet_t::iterator a_it_begin = a_otypes.begin();
    MvObjectTypeSet_t::iterator a_it_end = a_otypes.end();
    MvObjectTypeSet_t::iterator a_it;
    for (a_ind = 0, a_it = a_it_begin; a_it != a_it_end; ++a_ind, ++a_it) {
        object_type_e a_otype = (*a_it);
        MCDS_set_ff_card_tab(a_card_p, CARD_OTYPE, a_ind, (const void*)(&a_otype));
    }
    // Adding the card
    ((LocCardList_t*)card_list_p)->push_back(a_card_p);
}



void MvDescriptorParser_t::readCardList(const MvDescriptor_t* descr_p, PseudoCardList_t* card_list_p, bool is_free) {
    // Parsing 
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Size
    string a_size_skw;
    int  a_size = getNextIKeyword(descr_p, &a_size_skw);
    bool a_is_static = false;
    if (a_size == END_ARGS) {
        if (is_free) throwError(getMsg(20), a_size_skw.c_str());
        try { a_size = loc_string2int(a_size_skw); }
        catch (MvError_t&) { throwError(getMsg(20), a_size_skw.c_str()); }
        a_is_static = true;
    }
    if (a_is_static)                                        a_size = (-a_size);
    else if (descr_p->getAttributeType(a_size) != ATYPE_SIZE) throwError(getMsg(13), a_size_skw.c_str());
    if (getNextChar() != ')') throwError(getMsg(13), ")");
    // Cards
    if (getNextChar() != '{') throwError(getMsg(13), "{");
    vector<string> token_list;
    LocCardList_t a_card_list;
    while (getNextChar() != '}') {
        unreadChar();
        string a_card_type = getNextString();
        if (a_card_type == "CARD" || a_card_type == "CARD_PREREAD")         readCard(descr_p, (PseudoCardList_t*)(&a_card_list), false, a_card_type); 
        else if (a_card_type == "BLANK")   readBlankCard(descr_p, (PseudoCardList_t*)(&a_card_list));
        
        else if (a_card_type == "COMMENT") readCommentCard(descr_p, (PseudoCardList_t*)(&a_card_list));
        
        else if (a_card_type == "if")      readIfCard(descr_p, (PseudoCardList_t*)(&a_card_list));     
        else if (a_card_type == "ASSIGN")     readCardAssign(descr_p, (PseudoCardList_t*)(&a_card_list));
        else if (a_card_type == "CELL_LIST")     readCellList(descr_p, (PseudoCardList_t*)(&a_card_list), false);
        else if (a_card_type == "LIST")          readList(descr_p, (PseudoCardList_t*)(&a_card_list));
        else   throwError(getMsg(37), a_card_type.c_str());                 
    }
    /*reading TOKEN_END if present*/
    int flag = 0;
    char c = getNextChar();
    if (c == '{')
    {
        while (c != '}')
        {
            bool skip_scolun = false;
            string a_card_type = getNextString();
            if (a_card_type == "TOKEN_END")
            {
                if (getNextChar() != '=')
                    throwError(getMsg(13), "=");
                c = getNextChar();
                if (c != '(')
                    throwError(getMsg(13), "(");
                while (c != ')')
                {
                    string token_str = getNextQuotedString();
                    token_list.push_back(token_str);
                    c = getNextChar();
                }
            }
            else if (a_card_type == "NO_COMMENT")
                flag = flag | CARD_FLAG_NO_COMMENT;
            else if (a_card_type == "FIXED_FORMAT")
                flag = flag | CARD_FLAG_NO_FREE_FORMAT;
            else if (a_card_type == "")
            {
                skip_scolun = true;
            }
            c = getNextChar();
            if (c != ';' && !skip_scolun) throwError(getMsg(13), ";");
        }
    }
    else
    {
        unreadChar();
    }
    // Creating the card
    ff_card_t* a_card_p;
    MCDS_new_ff_card(&a_card_p, CARD_CARD_LIST);
    // Adding subcards
    int a_nb_cards = (int)(a_card_list.size());
    MCDS_set_ff_card_attributes(a_card_p, CARD_NB_CARDS, a_nb_cards, CARD_SIZE, a_size, END_ARGS);
    for (int i = 0; i < a_nb_cards; ++i) MCDS_set_ff_card_tab(a_card_p, CARD_CARD, i, (const void*)(&(a_card_list[i])));
    int a_is_free = (is_free ? 1 : 0);
    MCDS_set_ff_card_attributes(a_card_p, CARD_IS_FREE, a_is_free, END_ARGS);
    int nb_token = (int)token_list.size();
    if (nb_token)
    {
        MCDS_set_ff_card_attributes(a_card_p, CARD_FREE_CARD_LIST_TOKEN_NB, nb_token, END_ARGS);
        for (int i = 0; i < nb_token; i++)
        {
            string token = token_list[i];
            const char* token_c = token.c_str();
            MCDS_set_ff_card_tab(a_card_p, CARD_FREE_CARD_LIST_TOKEN_STR, i, &token_c);
        }
    }
    MCDS_set_ff_card_attributes(a_card_p, CARD_FLAGS, flag, END_ARGS);
    // Adding the card
    ((LocCardList_t*)card_list_p)->push_back(a_card_p);
}



void MvDescriptorParser_t::readSubobjectsCard(const MvDescriptor_t* descr_p, PseudoCardList_t* card_list_p) {
    // Parsing 
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Object(s) ikeyword
    string a_objects_skw;
    int iskeyword = 0;
    char lnkby = 'i';
    int    a_objects_ikw = getNextIKeyword(descr_p, &a_objects_skw);
    loc_multi_array_not_acceptable(descr_p, a_objects_ikw);
    attribute_type_e a_atype = descr_p->getAttributeType(a_objects_ikw);
    if (a_atype != ATYPE_VALUE && a_atype != ATYPE_STATIC_ARRAY && a_atype != ATYPE_DYNAMIC_ARRAY) {
        throwError(getMsg(24), a_objects_skw.c_str());
    }
    if (descr_p->getValueType(a_objects_ikw) != VTYPE_OBJECT) {
        throwError(getMsg(24), a_objects_skw.c_str());
    }
    // Kernel fulltype
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    string a_kfulltype = getNextString();
    // Parent-child link
    string att_lnk_parent, att_lnk_child;
    if (getNextChar() == ',')
    {
        if (getNextChar() == '(') { // input by skeyword pair
            att_lnk_parent = getNextString();
            char nc = getNextChar();
            if (nc != ')' && nc != ',') //nc is neither ')' nor ','
                throwError(getMsg(13), nc);
            else
            {
                if (nc == ')')
                {
                    att_lnk_child = att_lnk_parent;
                }
                else
                {
                    att_lnk_child = getNextString();
                    if (getNextChar() != ')') throwError(getMsg(13), ")");
                }
            }
        }
        else { // input by single common skeyword
            unreadChar();
            att_lnk_parent = getNextString();
            att_lnk_child = att_lnk_parent;
        }
    }
    else
        unreadChar();

    if (getNextChar() != ')') throwError(getMsg(13), ")");
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // Creating card
    ff_card_t* a_card_p = NULL;
    MCDS_new_ff_card(&a_card_p, CARD_SUBOBJECTS);
    const char* a_kfulltype_c_str = a_kfulltype.c_str();
    const char* a_p_lnk_att = att_lnk_parent.c_str();
    const char* a_c_lnk_att = att_lnk_child.c_str();
    MCDS_set_ff_card_attributes(a_card_p,
        CARD_KFULLTYPE, a_kfulltype_c_str,
        CARD_OBJECTS_IKW, a_objects_ikw,
        CARD_SUBOBJ_PARENT_LNK_ATT, a_p_lnk_att,
        CARD_SUBOBJ_CHILD_LNK_ATT, a_c_lnk_att,
        END_ARGS);
    // Adding the card
    ((LocCardList_t*)card_list_p)->push_back(a_card_p);
}



string MvDescriptorParser_t::readIfCard(const MvDescriptor_t* descr_p, PseudoCardList_t* card_list_p) {
    LocExprList_t     a_expr_list;
    LocCardListList_t a_cl_list;
    string            a_cond_type = "if";
    string            a_skeyword = "";
    // Parsing
    while (a_cond_type == "if") {
        // Reading condition
        if (getNextChar() != '(') throwError(getMsg(13), "(");
        MvExpression_t* a_expr_p = getNextExpressionPtr(descr_p, false);
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        // Reading cards
        LocCardList_t a_if_cl;
        if (getNextChar() != '{') {
            unreadChar();
            readNextCard(descr_p, getNextString(), &a_if_cl);
        }
        else while (getNextChar() != '}') {
            unreadChar();
            readNextCard(descr_p, getNextString(), &a_if_cl);
        }
        a_expr_list.push_back(a_expr_p);
        a_cl_list.push_back(a_if_cl);
        // Next condition (is there a "else"?)
        char a_char = getNextChar();
        unreadChar();
        if (a_char == '}') {
            a_cond_type = "";
        }
        else {
            a_cond_type = getNextString();
            //

            if (a_cond_type == "else") {
                LocCardList_t a_else_cl;
                //
                if (getNextChar() == '{') while (getNextChar() != '}') {
                    unreadChar();
                    readNextCard(descr_p, getNextString(), &a_else_cl);
                }
                else {
                    unreadChar();
                    a_cond_type = getNextString();
                    if (a_cond_type != "if") readNextCard(descr_p, a_cond_type, &a_else_cl);
                }
                //
                if (a_cond_type != "if") {
                    a_expr_list.push_back(NULL);
                    a_cl_list.push_back(a_else_cl);
                }
            }
            else if (a_cond_type == "if" || a_cond_type == "CARD" || a_cond_type == "ASSIGN" ||
                a_cond_type == "CARD_PREREAD" || a_cond_type == "CARD_LIST" || a_cond_type == "COMMENT" || a_cond_type == "FREE_CARD" || a_cond_type == "HEADER" ||
                a_cond_type == "BLANK" || a_cond_type == "LIST" || a_cond_type == "CELL_LIST" || a_cond_type == "FREE_OBJECT_LIST" ||
                a_cond_type == "SUBOBJECTS" || a_cond_type == "FREE_CELL_LIST" || a_cond_type == "OBJECT_LIST" || a_cond_type == "FREE_CARD_LIST")
            {
                int size = (int)a_cond_type.size();
                for (int i = 0; i < size; i++)
                {
                    unreadChar();
                }
                break;
            }
            else {
                a_skeyword = a_cond_type;
                a_cond_type = "";
            }
        }
    }
    // Creating the card
    ff_card_t* a_card_p = NULL;
    MCDS_new_ff_card(&a_card_p, CARD_IF);
    int a_nb_ccl = (int)(a_cl_list.size());
    MCDS_set_ff_card_attributes(a_card_p, CARD_NB_COND_CARD_LISTS, a_nb_ccl, END_ARGS);
    for (int i = 0; i < a_nb_ccl; ++i) {
        MvExpression_t* a_expr_pf = a_expr_list[i];
        expression_t* a_mcds_expr_p = (a_expr_pf == NULL ? NULL : a_expr_pf->getExpressionPtr());
        LocCardList_t& a_card_list = a_cl_list[i];
        int                a_nb_cards = (int)(a_card_list.size());
        ff_condcardlist_t* a_ccl_p = NULL;
        //
        MCDS_new_ff_condcardlist(&a_ccl_p, a_mcds_expr_p, a_nb_cards);
        if (a_expr_pf != NULL) delete a_expr_pf;
        //
        for (int j = 0; j < a_nb_cards; ++j) {
            ff_card_t* a_loc_card_p = a_card_list[j];
            MCDS_set_ff_condcardlist_card(a_ccl_p, j, a_loc_card_p);
        }
        //
        MCDS_set_ff_card_tab(a_card_p, CARD_COND_CARD_LIST, i, &a_ccl_p);
    }
    // Adding the card
    ((LocCardList_t*)card_list_p)->push_back(a_card_p);
    //
    return a_skeyword;
}




ff_cell_t* MvDescriptorParser_t::readIfCell(const MvDescriptor_t* descr_p, const string& format) {
    LocExprList_t     a_expr_list;
    LocCellList_t     a_cl_list;
    string            a_cond_type = "if";
    string            a_skeyword = "";
    // Parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    a_cond_type = getNextString();

    while (a_cond_type == "if")
    {
        // Reading condition
        if (getNextChar() != '(') throwError(getMsg(13), "(");
        MvExpression_t* a_expr_p = getNextExpressionPtr(descr_p, false);
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        // Reading cell
        ff_cell_t* a_if_cl = NULL;
        if (getNextChar() != '{')
        {
            unreadChar();
            a_if_cl = readCell(descr_p, format);
            if (getNextChar() != ';')   throwError(getMsg(13), ";");
        }
        else
        {
            a_if_cl = readCell(descr_p, format);
            if (getNextChar() != ';')   throwError(getMsg(13), ";");
            if (getNextChar() != '}')  throwError(getMsg(13), "}");
        }
        a_expr_list.push_back(a_expr_p);
        a_cl_list.push_back(a_if_cl);

        // Next condition (is there a "else"?)
        char a_char = getNextChar();
        unreadChar();
        if (a_char == ')')
        {
            a_cond_type = "";
        }
        else
        {
            a_cond_type = getNextString();
            //
            if (a_cond_type == "else")
            {
                ff_cell_t* a_else_cl = NULL;
                //
                if (getNextChar() == '{')
                {
                    a_else_cl = readCell(descr_p, format);
                    a_char = getNextChar();
                    if (a_char != ';')   throwError(getMsg(13), ";");
                    if (getNextChar() != '}')  throwError(getMsg(13), "}");
                }
                else
                {
                    unreadChar();
                    char a_char1 = getNextChar();
                    char a_char2 = MvReadBase_t::readChar();

                    if (a_char1 == 'i' && a_char2 == 'f')
                    {
                        a_cond_type = "if";
                    }
                    else
                    {
                        unreadChar();
                        unreadChar();
                        a_else_cl = readCell(descr_p, format);
                        a_char = getNextChar();
                        if (a_char != ';')   throwError(getMsg(13), ";");
                    }
                }
                //
                if (a_cond_type != "if")
                {
                    a_expr_list.push_back(NULL);
                    a_cl_list.push_back(a_else_cl);
                }
            }
            else
            {
                a_skeyword = a_cond_type;
                a_cond_type = "";
            }
        }
    }

    char a_char1 = getNextChar();
    if (a_char1 != ')')  throwError(getMsg(13), ")");

    // Creating the cell
    ff_cell_t* a_cell_p;
    MCDS_new_ff_cell(&a_cell_p, CELL_COND);

    int a_nb_ccl = (int)(a_cl_list.size());

    MCDS_set_ff_cell_attributes(a_cell_p, CELL_NB_COND_CELL, a_nb_ccl, END_ARGS);

    for (int i = 0; i < a_nb_ccl; ++i)
    {
        MvExpression_t* a_expr_pf = a_expr_list[i];
        expression_t* a_mcds_expr_p = (a_expr_pf == NULL ? NULL : a_expr_pf->getExpressionPtr());
        ff_condcell_t* a_ccl_p = NULL;
        //
        MCDS_new_ff_condcell(&a_ccl_p, a_mcds_expr_p, a_cl_list[i]);
        if (a_expr_pf != NULL) delete a_expr_pf;

        //
        MCDS_set_ff_cell_tab(a_cell_p, CELL_COND_CELL, i, &a_ccl_p);
    }
    return a_cell_p;
}



/* --------- Drawables --------- */

void MvDescriptorParser_t::readDrawables(MvDescriptor_t* descr_p) {
    // Domain
    MvDomain_e         a_domain = DOM_COMMON;
    MvDrawableAccess_e a_access = DRA_PUBLIC;
    //
    char a_char = getNextChar();
    if (a_char == '(') {
        if (getNextChar() != ')') {
            unreadChar();
            a_domain = getNextDomain();
            if (getNextChar() != ')') throwError(getMsg(13), ")");
        }
        a_char = getNextChar();
    }
    // Loop on drawables
    if (a_char != '{') throwError(getMsg(13), "{");
    while (getNextChar() != '}') {
        unreadChar();
        // Drawable's name
        string a_drawable_name = getNextString();
        // Drawable's access
        while (a_drawable_name == "public" || a_drawable_name == "private") {
            a_access = (a_drawable_name == "public" ? DRA_PUBLIC : DRA_PRIVATE);
            if (getNextChar() != ':') throwError(getMsg(13), ":");
            a_drawable_name = getNextString();
        }
        // Drawable's type
        if (getNextChar() != '=') throwError(getMsg(13), "=");
        string a_dtype = getNextString();
        // Test on the drawable's type
        if (a_dtype == "SCALAR")               readScalarDrawable(a_domain, a_drawable_name, a_access, descr_p);
        else if (a_dtype == "SUBDRAWABLE")     readSubDrawableDrawable(a_domain, a_drawable_name, a_access, descr_p);
        else if (a_dtype == "WHILE_ZERO")      readWhileZeroDrawable(a_domain, a_drawable_name, a_access, descr_p);
        else if (a_dtype == "EVAL")            readEvalDrawable(a_domain, a_drawable_name, a_access, descr_p);
        else if (a_dtype == "TIME_STEP")       readTimeStepDrawable(a_domain, a_drawable_name, a_access, descr_p);
        else if (a_dtype == "MAX")             readOptDrawable(DRT_MAX, a_domain, a_drawable_name, a_access, descr_p);
        else if (a_dtype == "MIN")             readOptDrawable(DRT_MIN, a_domain, a_drawable_name, a_access, descr_p);
        
        else if (a_dtype == "VOLUME")          readVolumeDrawable(a_domain, a_drawable_name, a_access, descr_p);
        
        
        
        else if (a_dtype == "AREA")            readAreaDrawable(a_domain, a_drawable_name, a_access, descr_p);
        
        
        else                                throwError(getMsg(52), a_dtype.c_str());
    }
}

void MvDescriptorParser_t::readScalarDrawable(MvDomain_e domain, const string& name, MvDrawableAccess_e access,
    MvDescriptor_t* descr_p)
{
    // Begin parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Name, SKeyword
    string a_drawable_name = name;
    string a_skeyword = getNextString();
    // End of parsing
    if (getNextChar() != ')') throwError(getMsg(13), ")");
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // IKeyword
    int a_ikeyword = descr_p->getIKeyword(a_skeyword);
    if (a_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
    //  
    value_type_e     a_vtype = descr_p->getValueType(a_ikeyword);
    attribute_type_e a_atype = descr_p->getAttributeType(a_ikeyword);
    // Testing the validity of the keyword
    if (a_atype != ATYPE_VALUE || a_vtype != VTYPE_FLOAT) {
        //DisplayMess(ERROR,getMsg(53),a_skeyword.c_str(),a_drawable_name.c_str());
    }
    // Creating and adding the drawable
    try
    {
        descr_p->addDrawable(domain, new MvDrawableScalar_t(a_drawable_name, a_ikeyword, access));
    }
    catch (MvError_t& error)
    {
        throwError(error.c_str());
    }
}

void MvDescriptorParser_t::readSubDrawableDrawable(MvDomain_e domain, const string& name,
    MvDrawableAccess_e access, MvDescriptor_t* descr_p)
{
    // Begin parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // SKeyword
    string a_skeyword = getNextString();
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    // Sub-drawable name
    string a_subname = getNextString();
    // End of parsing
    if (getNextChar() != ')') throwError(getMsg(13), ")");
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // IKeyword
    int a_ikeyword = descr_p->getIKeyword(a_skeyword);
    if (a_ikeyword == END_ARGS) throwError(getMsg(17), a_skeyword.c_str());
    // Testing the validity of the keyword
    value_type_e     a_vtype = descr_p->getValueType(a_ikeyword);
    attribute_type_e a_atype = descr_p->getAttributeType(a_ikeyword);
    if (a_atype != ATYPE_VALUE || a_vtype != VTYPE_OBJECT) {
        //DisplayMess(ERROR,getMsg(54),a_skeyword.c_str(),name.c_str());
    }
    // Creating and adding the drawable
    try
    {
        descr_p->addDrawable(domain, new MvDrawableSubdrawable_t(name, a_ikeyword, a_subname, access));
    }
    catch (MvError_t& error)
    {
        throwError(error.c_str());
    }
}

void MvDescriptorParser_t::readWhileZeroDrawable(MvDomain_e domain, const string& name,
    MvDrawableAccess_e access, MvDescriptor_t* descr_p)
{
    // Parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Drawables
    vector<const MvDrawable_t*> a_drawable_array;
    char a_char = getNextChar();
    if (a_char != ')') unreadChar();
    while (a_char != ')') {
        // Drawable
        string              a_drawable_name = getNextString();
        const MvDrawable_t* a_drawable_p = descr_p->getDrawablePtr(a_drawable_name);
        if (a_drawable_p == NULL) throwError(getMsg(55), a_drawable_name.c_str());
        else                   a_drawable_array.push_back(a_drawable_p);
        a_char = getNextChar();
        if ((a_char != ',') && (a_char != ')')) throwError(getMsg(13), ")");
    }
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // Creating and adding the drawable
    MvDrawableWhileZero_t* a_wz_drawable_p = new MvDrawableWhileZero_t(name, access);
    int a_nb_drawables = (int)(a_drawable_array.size());
    for (int i = 0; i < a_nb_drawables; ++i) a_wz_drawable_p->addDrawable(a_drawable_array[i]);
    try
    {
        descr_p->addDrawable(domain, a_wz_drawable_p);
    }
    catch (MvError_t& error)
    {
        throwError(error.c_str());
    }
}

void MvDescriptorParser_t::readEvalDrawable(MvDomain_e domain, const string& name, MvDrawableAccess_e access,
    MvDescriptor_t* descr_p)
{
    // Parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    string a_formula = "";
    int    a_nb_parenthesis = 1;
    while (a_nb_parenthesis > 0) {
        char a_char = getNextChar();
        if (a_char == '(') ++a_nb_parenthesis; else if (a_char == ')') --a_nb_parenthesis;
        if (a_nb_parenthesis > 0) a_formula += a_char;
    }
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // Creating and adding the drawable
    try
    {
        descr_p->addDrawable(domain, new MvDrawableEval_t(name, access, a_formula, descr_p));
    }
    catch (MvError_t& error)
    {
        throwError(error.c_str());
    }
}

void MvDescriptorParser_t::readTimeStepDrawable(MvDomain_e domain, const string& name, MvDrawableAccess_e access,
    MvDescriptor_t* descr_p)
{
    // Parsing
    char a_char = getNextChar();
    if (a_char == '(') {
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        a_char = getNextChar();
    }
    if (a_char != ';') throwError(getMsg(13), ";");
    // Creating and adding the drawable
    try
    {
        descr_p->addDrawable(domain, new MvDrawableTimeStep_t(name, access));
    }
    catch (MvError_t& error)
    {
        throwError(error.c_str());
    }
}

void MvDescriptorParser_t::readOptDrawable(MvDrawableType_e drawable_type,
    MvDomain_e domain, const string& name, MvDrawableAccess_e access,
    MvDescriptor_t* descr_p)
{
    // Parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    // Drawables
    vector<const MvDrawable_t*> a_drawable_array;
    char a_char = getNextChar();
    if (a_char != ')') unreadChar();
    // FLAG to understand whether first variable is read or not 
    //This is to distinguish between Array and Single attribute use cases.
    bool is_first_parameter_read = false;
    int a_ikeyword = 0;
    while (a_char != ')') {
        // Drawable
        string              a_drawable_name = getNextString();
        const MvDrawable_t* a_drawable_p = descr_p->getDrawablePtr(a_drawable_name);
        
        if (a_drawable_p == NULL)
        {
            if (is_first_parameter_read) throwError(getMsg(55), a_drawable_name.c_str());// If the first parameter is already read, it means a_drawable_p should be there for the remaining parameters.
            a_ikeyword = descr_p->getIKeyword(a_drawable_name);
            if (a_ikeyword == END_ARGS) throwError(getMsg(55), a_drawable_name.c_str());
            attribute_type_e an_array_type = descr_p->getAttributeType(a_ikeyword);
            if (!(an_array_type == ATYPE_STATIC_ARRAY || an_array_type == ATYPE_DYNAMIC_ARRAY))  throwError(getMsg(55), a_drawable_name.c_str());
            a_char = getNextChar();
            if (a_char != ')') throwError(getMsg(13), ")");
            unreadChar();
        }
        else  a_drawable_array.push_back(a_drawable_p);
        is_first_parameter_read = true;// The first parameter is read and so flag will be set to true.
        a_char = getNextChar();
        if ((a_char != ',') && (a_char != ')')) throwError(getMsg(13), ")");
    }
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    bool isArrayAttribute = a_ikeyword > 0 ? true : false;
    // Creating and adding the drawable
    MvDrawableOpt_t* a_drawable_p = NULL;
    if (drawable_type == DRT_MAX) a_drawable_p = new MvDrawableMax_t(name, access, isArrayAttribute, a_ikeyword);
    else                       a_drawable_p = new MvDrawableMin_t(name, access, isArrayAttribute, a_ikeyword);
    int a_nb_drawables = (int)(a_drawable_array.size());
    for (int i = 0; i < a_nb_drawables; ++i) a_drawable_p->addDrawable(a_drawable_array[i]);
    try
    {
        descr_p->addDrawable(domain, a_drawable_p);
    }
    catch (MvError_t& error)
    {
        throwError(error.c_str());
    }
}


void MvDescriptorParser_t::readVolumeDrawable(MvDomain_e domain, const string& name, MvDrawableAccess_e access,
    MvDescriptor_t* descr_p)
{
    // Parsing
    char a_char = getNextChar();
    if (a_char == '(') {
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        a_char = getNextChar();
    }
    if (a_char != ';') throwError(getMsg(13), ";");
    // Creating and adding the drawable
    try
    {
        descr_p->addDrawable(domain, new MvDrawableVolume_t(name, access));
    }
    catch (MvError_t& error)
    {
        throwError(error.c_str());
    }
}



void MvDescriptorParser_t::readAreaDrawable(MvDomain_e domain, const string& name, MvDrawableAccess_e access,
    MvDescriptor_t* descr_p)
{
    // Parsing
    char a_char = getNextChar();
    if (a_char == '(') {
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        a_char = getNextChar();
    }
    if (a_char != ';') throwError(getMsg(13), ";");
    // Creating and adding the drawable
    try
    {
        descr_p->addDrawable(domain, new MvDrawableArea_t(name, access));
    }
    catch (MvError_t& error)
    {
        throwError(error.c_str());
    }
}



/* --------- Parameters --------- */


void MvDescriptorParser_t::readParameters(MvDescriptor_t* descr_p) {
    // Domain
    MvDomain_e       a_domain = DOM_COMMON;
    MvIParamAccess_e a_iaccess = IPA_PUBLIC;
    MvOParamAccess_e a_oaccess = OPA_PUBLIC;
    //
    char a_char = getNextChar();
    if (a_char == '(') {
        if (getNextChar() != ')') {
            unreadChar();
            a_domain = getNextDomain();
            if (getNextChar() != ')') throwError(getMsg(13), ")");
        }
        a_char = getNextChar();
    }
    // Loop on parameters
    if (a_char != '{') throwError(getMsg(13), "{");
    while (getNextChar() != '}') {
        unreadChar();
        // Parameter's name
        string a_param_name = getNextString();
        
        while (a_param_name == "public" || a_param_name == "private" || a_param_name == "if") {
            if (a_param_name == "if") {
                a_param_name = readConditionalParameters(a_domain, a_iaccess, a_oaccess, descr_p);
            }
            else {
                // Parameter's access
                if (a_param_name == "public") { a_iaccess = IPA_PUBLIC;  a_oaccess = OPA_PUBLIC; }
                else { a_iaccess = IPA_PRIVATE; a_oaccess = OPA_PRIVATE; }
                if (getNextChar() != ':') throwError(getMsg(13), ":");
                a_param_name = getNextString();
            }
        }
        // Parameter
        if (a_param_name != "if" && a_param_name != "") {
            if (getNextChar() != '=') throwError(getMsg(13), "=");
            readParameter(a_domain, a_param_name, a_iaccess, a_oaccess, descr_p);
        }
        
    }
}



string MvDescriptorParser_t::readConditionalParameters(MvDomain_e       domain,
    MvIParamAccess_e  iaccess,
    MvOParamAccess_e  oaccess,
    MvDescriptor_t* descr_p)
{

    bool   a_is_iparam = false;
    bool   a_is_oparam = false;
    string a_skeyword = "if";
    string a_cond_type = a_skeyword;
    MvCondIParamDescriptors_t* a_cond_iparams_p = NULL;
    MvCondOParamDescriptors_t* a_cond_oparams_p = NULL;
    //
    while (a_cond_type == "if") {
        // Expression
        if (getNextChar() != '(') throwError(getMsg(13), "(");
        MvExpression_t* a_iexpr_p = getNextExpressionPtr(descr_p);
        MvExpression_t* a_oexpr_p = new MvExpression_t(a_iexpr_p->getExpressionPtr());
        if (getNextChar() != ')') throwError(getMsg(13), ")");
        // Parameters
        MvIParamDescrPtrSet_t a_iparams;
        MvOParamDescrPtrSet_t a_oparams;
        if (getNextChar() != '{') {
            unreadChar();
            string a_param_name = getNextString();
            if (getNextChar() != '=') throwError(getMsg(13), "=");
            //
            MvIParamDescr_t* a_iparam_p = NULL;
            MvOParamDescr_t* a_oparam_p = NULL;
            readParameter(domain, a_param_name, iaccess, oaccess, descr_p, &a_iparam_p, &a_oparam_p);
            //
            if (a_iparam_p != NULL) a_iparams.insert(a_iparam_p);
            if (a_oparam_p != NULL) a_oparams.insert(a_oparam_p);
        }
        else while (getNextChar() != '}') {
            unreadChar();
            string a_param_name = getNextString();
            // Parameter's access
            MvIParamAccess_e a_loc_iaccess = iaccess;
            MvOParamAccess_e a_loc_oaccess = oaccess;
            while (a_param_name == "public" || a_param_name == "private") {
                if (a_param_name == "public") { a_loc_iaccess = IPA_PUBLIC;  a_loc_oaccess = OPA_PUBLIC; }
                else { a_loc_iaccess = IPA_PRIVATE; a_loc_oaccess = OPA_PRIVATE; }
                if (getNextChar() != ':') throwError(getMsg(13), ":");
                a_param_name = getNextString();
            }
            if (getNextChar() != '=') throwError(getMsg(13), "=");
            //
            MvIParamDescr_t* a_iparam_p = NULL;
            MvOParamDescr_t* a_oparam_p = NULL;
            readParameter(domain, a_param_name, a_loc_iaccess, a_loc_oaccess, descr_p, &a_iparam_p, &a_oparam_p);
            //
            if (a_iparam_p != NULL) a_iparams.insert(a_iparam_p);
            if (a_oparam_p != NULL) a_oparams.insert(a_oparam_p);
        }
        // Adding the condition
        if (a_cond_iparams_p == NULL) a_cond_iparams_p = new MvCondIParamDescriptors_t();
        if (a_cond_oparams_p == NULL) a_cond_oparams_p = new MvCondOParamDescriptors_t();
        a_cond_iparams_p->addCondition(a_iexpr_p, a_iparams);
        a_cond_oparams_p->addCondition(a_oexpr_p, a_oparams);
        if (!a_iparams.empty()) a_is_iparam = true;
        if (!a_oparams.empty()) a_is_oparam = true;
        // Next condition
        char a_char = getNextChar();
        unreadChar();
        if (a_char == '}') {
            a_cond_type = a_skeyword = "";
        }
        else if (a_cond_type != "") {
            a_cond_type = a_skeyword = getNextString();
            if (a_skeyword == "else") {
                a_iparams.clear();
                a_oparams.clear();
                a_skeyword = "";
                if (getNextChar() == '{') {
                    while (getNextChar() != '}') {
                        unreadChar();
                        string a_param_name = getNextString();
                        // Parameter's access
                        MvIParamAccess_e a_loc_iaccess = iaccess;
                        MvOParamAccess_e a_loc_oaccess = oaccess;
                        while (a_param_name == "public" || a_param_name == "private") {
                            if (a_param_name == "public") { a_loc_iaccess = IPA_PUBLIC;  a_loc_oaccess = OPA_PUBLIC; }
                            else { a_loc_iaccess = IPA_PRIVATE; a_loc_oaccess = OPA_PRIVATE; }
                            if (getNextChar() != ':') throwError(getMsg(13), ":");
                            a_param_name = getNextString();
                        }
                        if (getNextChar() != '=') throwError(getMsg(13), "=");
                        //
                        MvIParamDescr_t* a_iparam_p = NULL;
                        MvOParamDescr_t* a_oparam_p = NULL;
                        readParameter(domain, a_param_name, a_loc_iaccess, a_loc_oaccess, descr_p, &a_iparam_p, &a_oparam_p);
                        //
                        if (a_iparam_p != NULL) a_iparams.insert(a_iparam_p);
                        if (a_oparam_p != NULL) a_oparams.insert(a_oparam_p);
                    }
                    a_cond_type = "";
                }
                else {
                    unreadChar();
                    string a_param_name = a_cond_type = a_skeyword = getNextString();
                    if (a_skeyword != "if") {
                        a_cond_type = a_skeyword;
                        //
                        if (getNextChar() != '=') throwError(getMsg(13), "=");
                        //
                        MvIParamDescr_t* a_iparam_p = NULL;
                        MvOParamDescr_t* a_oparam_p = NULL;
                        readParameter(domain, a_param_name, iaccess, oaccess, descr_p, &a_iparam_p, &a_oparam_p);
                        //
                        if (a_iparam_p != NULL) a_iparams.insert(a_iparam_p);
                        if (a_oparam_p != NULL) a_oparams.insert(a_oparam_p);
                        //
                        a_cond_type = a_skeyword = "";
                    }
                }
                //
                if (a_skeyword != "if") {
                    a_cond_iparams_p->addDefault(a_iparams);
                    a_cond_oparams_p->addDefault(a_oparams);
                    if (!a_iparams.empty()) a_is_iparam = true;
                    if (!a_oparams.empty()) a_is_oparam = true;
                }
            }
        }
    }
    // Adding the conditional parameters
    if (!a_is_iparam && a_is_oparam) {
        int a_nb_conditions = a_cond_iparams_p->getNbConditions();
        for (int i = 0; i < a_nb_conditions; ++i) {
            MvExpression_t* a_expr_p = a_cond_iparams_p->getExpressionPtr(i);
            a_expr_p->setDelete(false);
        }
    }
    else {
        if (a_cond_oparams_p)
        {
            int a_nb_conditions = a_cond_oparams_p->getNbConditions();
            for (int i = 0; i < a_nb_conditions; ++i) {
                MvExpression_t* a_expr_p = a_cond_oparams_p->getExpressionPtr(i);
                a_expr_p->setDelete(false);
            }
        }
    }
    //
    if (a_is_iparam) {
        a_cond_iparams_p->posttreat();
        descr_p->addCondIParamDescriptors(domain, a_cond_iparams_p);
    }
    else {
        delete a_cond_iparams_p;
    }
    if (a_is_oparam) {
        a_cond_oparams_p->posttreat();
        descr_p->addCondOParamDescriptors(domain, a_cond_oparams_p);
    }
    else {
        delete a_cond_oparams_p;
    }
    //
    return a_cond_type;
}



void MvDescriptorParser_t::readParameter(MvDomain_e         domain,
    const string& param_name,
    MvIParamAccess_e   iaccess,
    MvOParamAccess_e   /*oaccess*/,
    MvDescriptor_t* descr_p,
    MvIParamDescr_t** iparam_descr_pp,
    MvOParamDescr_t** oparam_descr_pp)
{
    MvIParamDescr_t* a_ipd_p = NULL;
    MvOParamDescr_t* a_opd_p = NULL;
    //
    string a_ptype = getNextString();
    // Test on the parameter's type
    if (a_ptype == "INPUT_SCALAR") {
        a_ipd_p = readScalarIParam(domain, param_name, iaccess, descr_p);
    }
    else if (a_ptype == "INPUT_SUBPARAMETER") {
        a_ipd_p = readSubparamIParam(domain, param_name, iaccess, descr_p);
    }
    else if (a_ptype == "INPUT_TRANSLATION") {
        a_ipd_p = readTransformationIParam(IPT_TRANSLATION, domain, param_name, iaccess, descr_p);
    }
    else if (a_ptype == "INPUT_ROTATION") {
        a_ipd_p = readTransformationIParam(IPT_ROTATION, domain, param_name, iaccess, descr_p);
    }
    else if (a_ptype == "INPUT_SCALING") {
        a_ipd_p = readTransformationIParam(IPT_SCALING, domain, param_name, iaccess, descr_p);
    }
    else {
        throwError(getMsg(68), a_ptype.c_str());
    }
    //
    if (iparam_descr_pp != NULL)* iparam_descr_pp = a_ipd_p;
    if (oparam_descr_pp != NULL)* oparam_descr_pp = a_opd_p;
}




MvIParamDescr_t* MvDescriptorParser_t::readScalarIParam(MvDomain_e        domain,
    const string& name,
    MvIParamAccess_e  access,
    MvDescriptor_t* descr_p)
{
    string a_comment = "";
    // Parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    int a_ikeyword = getNextIKeyword(descr_p);
    char a_char = getNextChar();
    if (a_char == ',') {
        a_comment = getNextQuotedString();
        a_char = getNextChar();
    }
    if (a_char != ')')        throwError(getMsg(13), ")");
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // Creating the parameter
    MvIParamDescrScalar_t* a_param_p = new MvIParamDescrScalar_t(a_ikeyword, access, name, a_comment);
    descr_p->addIParamDescr(domain, a_param_p);
    //
    return a_param_p;
}





MvIParamDescr_t* MvDescriptorParser_t::readSubparamIParam(MvDomain_e        domain,
    const string& name,
    MvIParamAccess_e  access,
    MvDescriptor_t* descr_p)
{
    string a_comment = "";
    // Parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    int    a_ikeyword = getNextIKeyword(descr_p);
    if (getNextChar() != ',') throwError(getMsg(13), ",");
    string a_param = getNextString();
    char a_char = getNextChar();
    if (a_char == ',') {
        a_comment = getNextQuotedString();
        a_char = getNextChar();
    }
    if (a_char != ')')        throwError(getMsg(13), ")");
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // Creating the parameter
    MvIParamDescrSubparam_t* a_param_p = new MvIParamDescrSubparam_t(a_ikeyword, a_param, access, name, a_comment);
    descr_p->addIParamDescr(domain, a_param_p);
    //
    return a_param_p;
}





MvIParamDescr_t* MvDescriptorParser_t::readTransformationIParam(MvIParamType_e    ipt,
    MvDomain_e        domain,
    const string& name,
    MvIParamAccess_e  access,
    MvDescriptor_t* descr_p)
{
    string a_comment = "";
    // Parsing
    if (getNextChar() != '(') throwError(getMsg(13), "(");
    if (getNextChar() != ')') {
        unreadChar();
        a_comment = getNextQuotedString();
        if (getNextChar() != ')') throwError(getMsg(13), ")");
    }
    if (getNextChar() != ';') throwError(getMsg(13), ";");
    // Creating the parameter
    MvIParamDescr_t* a_param_p = NULL;
    switch (ipt) {
    case IPT_TRANSLATION: a_param_p = new MvIParamDescrTranslation_t(access, name, a_comment); break;
    case IPT_ROTATION:    a_param_p = new MvIParamDescrRotation_t(access, name, a_comment);    break;
    case IPT_SCALING:     a_param_p = new MvIParamDescrScaling_t(access, name, a_comment);     break;
    default:              break;
    }
    descr_p->addIParamDescr(domain, a_param_p);
    //
    return a_param_p;
}




/* --------- Definitions --------- */

void MvDescriptorParser_t::readDefinitions(MvDescriptor_t* descr_p) {
    typedef map<string, MvIKeywordSet_t> LocDefinitions_t;
    LocDefinitions_t a_definitions;
    // Domain
    MvDomain_e a_domain = DOM_COMMON;
    char a_char = getNextChar();
    if (a_char == '(') {
        if (getNextChar() != ')') {
            unreadChar();
            a_domain = getNextDomain();
            if (getNextChar() != ')') throwError(getMsg(13), ")");
        }
        a_char = getNextChar();
    }
    // Loop on definitions
    if (a_char != '{') throwError(getMsg(13), "{");
    while (getNextChar() != '}') {
        unreadChar();
        string a_name = getNextString();
        if (getNextChar() != '=') throwError(getMsg(13), "=");
        //
        MvIKeywordSet_t& a_ikeywords = a_definitions[a_name];
        a_char = getNextChar();
        if (a_char != '(') throwError(getMsg(13), "(");
        while (a_char != ')') {
            int a_ikeyword = getNextIKeyword(descr_p);
            a_ikeywords.insert(a_ikeyword);
            a_char = getNextChar();
            if ((a_char != ',') && (a_char != ')')) throwError(getMsg(13), ")");
        }
        if (getNextChar() != ';') throwError(getMsg(13), ";");
    }
    // Creating the parsed definitions
    LocDefinitions_t::iterator a_it_begin = a_definitions.begin();
    LocDefinitions_t::iterator a_it_end = a_definitions.end();
    LocDefinitions_t::iterator a_it;
    for (a_it = a_it_begin; a_it != a_it_end; ++a_it) descr_p->addDefinition(a_domain, (*a_it).first, (*a_it).second);
}



/* --------- Parsing --------- */

int MvDescriptorParser_t::getNextIKeyword(const MvDescriptor_t* descr_p, string* skeyword_p) {
    int an_ikeyword = END_ARGS;
    string a_skeyword = getNextString();
    an_ikeyword = descr_p->getIKeyword(a_skeyword);
    if (an_ikeyword == END_ARGS) {
        if (skeyword_p == NULL) throwError(getMsg(20), a_skeyword.c_str());
    }
    if (skeyword_p != NULL)
        *skeyword_p = a_skeyword;
    return an_ikeyword;
}

MvDimension_e   MvDescriptorParser_t::getNextDimension(vector<string> *argVect) {
    string        a_dim_str = getNextQuotedString();
    MvDimension_e a_dimension = UDI_UNKNOWN;

    int pos = (int)a_dim_str.find("<");
    int pos_close = (int)a_dim_str.find(">");
    if (pos != string::npos)
    {
        if (pos_close == string::npos)
            throwError(getMsg(13), ">");
        int length = (int)a_dim_str.size();
        string arg_list = a_dim_str.substr(pos + 1, length - pos - 2);
        vector<string> key_vect;
        StringTokenize(arg_list, key_vect, ",");

        vector<string>::iterator iter_b = key_vect.begin();
        vector<string>::iterator iter_e = key_vect.end();
        vector<string>::iterator iter;
        for (iter = iter_b; iter != iter_e; ++iter)
        {
            string a_arg = *iter;
            string pend_arg = "";
            int size = (int)a_arg.size();
            for (int i = 0; i < size; i++)
            {
                if (a_arg[i] == ' ')
                    continue;
                pend_arg += a_arg[i];
            }
            argVect->push_back(pend_arg);
        }

        a_dim_str = a_dim_str.substr(0, pos);
        a_dimension = MV_get_dimension(a_dim_str);
        if (a_dimension != UDI_UNKNOWN)
        {
            int result = MU_check_custom_dimension_info(a_dimension, *argVect);
            if (result == -1)
            {
                throwError(getMsg(88), a_dim_str.c_str(), (int)argVect->size());
            }
        }
    }
    else
    {
        a_dimension = MV_get_dimension(a_dim_str);
    }

    //
    if (a_dimension == UDI_UNKNOWN) throwError(getMsg(23), a_dim_str.c_str());
    //
    return a_dimension;
}

MvDataTripleFeatureType_e MvDescriptorParser_t::getNextDifferentiatorType() {
    string a_type_str = getNextString();
    MvDataTripleFeatureType_e type = DTP_TYPE_UNKNOWN;
    if (a_type_str=="RELATIVE")  type= DTP_TYPE_RELATIVE;
    else if (a_type_str=="BASE") type= DTP_TYPE_BASE;
	else if (a_type_str == "VALUE_DIFFERENTIATOR_POSITION")  type = DTP_TYPE_POSITION;
	else if (a_type_str == "VALUE_DIFFERENTIATOR_DIRECTION") type = DTP_TYPE_DIRECTION;
	else if (a_type_str == "VALUE_DIFFERENTIATOR_VECTOR")    type = DTP_TYPE_VECTOR;
	else if (a_type_str == "VALUE_DIFFERENTIATOR_RGB")       type = DTP_TYPE_RGB;

    if(type== DTP_TYPE_UNKNOWN) throwError(getMsg(83),a_type_str.c_str());

    return type;
}

MvOrientation_e MvDescriptorParser_t::getNextOrientation(int* nbr_p, int* nbc_p) {
    string          an_ori_str = getNextString();
    MvOrientation_e an_orientation = MV_get_orientation(an_ori_str);
    *nbr_p = *nbc_p = 0;
    //
    if (an_orientation == ORI_UNKNOWN) throwError(getMsg(27), an_ori_str.c_str());
    //
    if (an_orientation == ORI_MATRIX) {
        if (getNextChar() != '(') throwError(getMsg(13), "(");
        *nbr_p = getNextInt();
        if (getNextChar() != ',') throwError(getMsg(13), ",");
        *nbc_p = getNextInt();
        if (getNextChar() != ')') throwError(getMsg(13), ")");
    }
    //
    return an_orientation;
}

object_type_e MvDescriptorParser_t::getNextObjectType() {
    string        a_type_str = getNextString();
    object_type_e a_type = MV_get_type(a_type_str);
    //
    if (a_type == HCDI_OBJ_TYPE_NULL) throwError(getMsg(9), a_type_str.c_str());
    //
    return a_type;
}


MvObjectTypeSet_t* MvDescriptorParser_t::getNextObjectTypes(MvObjectTypeSet_t* otypes_p) {
    //
    if (otypes_p == NULL) otypes_p = new MvObjectTypeSet_t();
    //
    string a_otype_str = getNextString();
    if (a_otype_str == "ELEMENTS" || a_otype_str == "1D_ELEMENTS" || a_otype_str == "2D_ELEMENTS" || a_otype_str == "3D_ELEMENTS")
    {
        MvObjectTypeSet_t a_set;
        a_set.insert(HCDI_OBJ_TYPE_ELEMS);
        *otypes_p += a_set;
    }
    else {
        object_type_e a_otype = MV_get_type(a_otype_str);
        if (a_otype == HCDI_OBJ_TYPE_NULL) throwError(getMsg(9), a_otype_str.c_str());
        *otypes_p += a_otype;
    }
    //
    return otypes_p;
}


MvFullType_t MvDescriptorParser_t::getNextObjectFullType() {
    string        a_full_type_str = getNextString();
    const CFGKernel* p_cfgkernel = (const CFGKernel*)mycfgkernel;
    MvFullType_t  a_full_type(*p_cfgkernel, a_full_type_str);
    object_type_e a_type = a_full_type.getType();
    //
    if (a_type == HCDI_OBJ_TYPE_NULL) throwError(getMsg(9), a_full_type_str.c_str());
    //
    return a_full_type;
}


MvFullTypeSet_t* MvDescriptorParser_t::getNextObjectFullTypes(MvFullTypeSet_t* fts_p) {
    //
    MvFullTypeSet_t* a_fts_p = (fts_p == NULL ? new MvFullTypeSet_t() : fts_p);
    //
    string a_full_type_str = getNextString();
    
      // need to get the various element type by element config like:HM_ELEMENT_CONFIG_SPRING
    if (a_full_type_str == "ELEMENTS" || a_full_type_str == "1D_ELEMENTS" || a_full_type_str == "2D_ELEMENTS" || a_full_type_str == "3D_ELEMENTS")
    {
        MvObjectTypeSet_t a_set;
        a_set.insert(HCDI_OBJ_TYPE_ELEMS);
        if (fts_p)
        {
            operatorplusegal(*fts_p, a_set);
        }
    }
    
    else {
        const CFGKernel* p_cfgkernel = (const CFGKernel*)mycfgkernel;
        MvFullType_t  a_full_type(*p_cfgkernel, a_full_type_str);
        //
        object_type_e a_type = a_full_type.getType();
        if (a_type == HCDI_OBJ_TYPE_NULL) throwError(getMsg(9), a_full_type_str.c_str());
        //
        a_fts_p->insert(a_full_type);
    }
    //

    //
    return a_fts_p;
}


MvDomain_e MvDescriptorParser_t::getNextDomain() {
    string a_domain_str = getNextString();
    MvDomain_e a_domain = MV_get_domain(a_domain_str);
    if (a_domain == DOM_UNKNOWN) throwError(getMsg(21), a_domain_str.c_str());
    //
    return a_domain;
}



MvExpression_t* MvDescriptorParser_t::getNextExpressionPtr(const MvDescriptor_t* descr_p,
    bool do_delete, MvDataFeatureSet_t* features_p)
{
    MvExpression_t* a_expr_p = NULL;
    MvLogicalOperator_e  a_operator = LGOP_UNKNOWN;
    //
    try {
        do {
            MvExpression_t* a_loc_expr_p = NULL;
            char            a_char = getNextChar();
            //
            if (a_char == '(') {
                a_loc_expr_p = getNextExpressionPtr(descr_p, false, features_p);
                if (getNextChar() != ')') throwError(getMsg(13), ")");
            }
            else if (a_char == '!') {
                a_loc_expr_p = getNextExpressionPtr(descr_p, false, features_p);
                MvExpression_t* a_old_expr_pf = a_loc_expr_p; 
                a_loc_expr_p = new MvLogicalExpression_t(LGOP_NOT, a_old_expr_pf, false);
                delete a_old_expr_pf;                       
            }
            else {
                unreadChar();
                const MvDataFeature_t* a_ft_p = NULL;
                a_loc_expr_p = getNextSingleExpressionPtr(descr_p, false, features_p == NULL ? NULL : &a_ft_p);
                if (features_p != NULL && a_ft_p != NULL) features_p->insert(a_ft_p);
            }
            //
            if (a_operator == LGOP_UNKNOWN) {
                if (a_expr_p != NULL) throwError(getMsg(66));
                a_expr_p = a_loc_expr_p;
            }
            else {
                if (a_expr_p == NULL) throwError(getMsg(66));
                MvExpression_t* a_old_expr_pf = a_expr_p; 
                a_expr_p = new MvLogicalExpression_t(a_old_expr_pf, a_operator, a_loc_expr_p, false);
                delete a_loc_expr_p;                    
                delete a_old_expr_pf;                   
            }
            //
            a_char = getNextChar();
            if (a_char == ')') {
                unreadChar();
                a_operator = LGOP_UNKNOWN;
            }
            else {
                unreadChar();
                string a_operator_str = getNextLogicalOperator();
                a_operator = MV_get_logical_operator(a_operator_str);
            }
        } while (a_operator != LGOP_UNKNOWN);
    }
    catch (MvError_t& a_error) {
        throwError(getMsg(48), a_error.c_str());
    }
    //
    a_expr_p->setDelete(do_delete);
    return a_expr_p;
}





MvExpression_t* MvDescriptorParser_t::getNextSingleExpressionPtr(const MvDescriptor_t* descr_p,
    bool do_delete, const MvDataFeature_t** ft_pp)
{
    MvAttributeExpression_t* a_expr_p = NULL;
    //
    try {
        // Ikeyword
        int a_ikw = getNextIKeyword(descr_p);
        // Index
        int a_index = -1;
        if (getNextChar() != '[') {
            unreadChar();
        }
        else {
            a_index = getNextInt();
            if (getNextChar() != ']') throwError(getMsg(13), "]");
        }
        // Comparator
        string a_cmp = getNextComparator();
        // R-value
        string a_rvalue = "";
        switch (descr_p->getValueType(a_ikw)) {
        case VTYPE_BOOL:
        case VTYPE_INT:
        case VTYPE_UINT:
        case VTYPE_FLOAT:
        case VTYPE_OBJECT:
            a_rvalue = getNextString();
            break;
        case VTYPE_STRING:
            a_rvalue = getNextQuotedString();
            break;
        default:
            throw MvError_t("MvDescriptorParser_t::getNextExpressionPtr -> wrong value type");
            //break;
        }
        //
        if (a_index < 0) a_expr_p = new MvAttributeExpression_t(descr_p, a_ikw, a_cmp, a_rvalue, do_delete);
        //else          a_expr_p=new MvAttributeExpression_t(descr_p,a_ikw,a_index,a_cmp,a_rvalue);
        //
        if (ft_pp != NULL)* ft_pp = getFeaturePtr(a_ikw); 
    }
    catch (MvError_t& a_error) {
        throwError(getMsg(48), a_error.c_str());
    }
    //
    return a_expr_p;
}




/* --------- String keyword -> integer keyword --------- */

int MvDescriptorParser_t::getIKeyword(const string& skeyword) {
    
    typedef set<string> LocSKeywordSet_t;
    
    // User objects
    if (myIsUser) {
        int a_ikeyword = (++myCurrentIKeyword);
        return a_ikeyword;
    }
  // Standard objects
  
    if (myObjectType == HCDI_OBJ_TYPE_NULL) return MV_get_model_ikeyword(skeyword);
    
    return ((CFGKernel *)(mycfgkernel))->get_ikeyword(myObjectType,skeyword);
    
}


/* --------- Integer keyword -> feature --------- */

bool MvDescriptorParser_t::isUsed(int ikeyword) const {
    return myIKeywordFeatureMap.find(ikeyword) != myIKeywordFeatureMap.end();
}

void MvDescriptorParser_t::addFeature(int ikeyword, MvDataFeature_t* dft_p) {
    myIKeywordFeatureMap[ikeyword] = dft_p;
}

const MvDataFeature_t* MvDescriptorParser_t::getFeaturePtr(int ikeyword) const {
    MvIKeywordFeatureMap_t::const_iterator a_it = myIKeywordFeatureMap.find(ikeyword);
    if (a_it == myIKeywordFeatureMap.end()) return NULL;
    //
    return (*a_it).second;
}

void MvDescriptorParser_t::clearFeatureMap() {
    myIKeywordFeatureMap.clear();
}


/* --------- Size map --------- */

MvDataSizeFeature_t* MvDescriptorParser_t::getSizeFeaturePtr(int ikeyword) const {
    MvIKeywordFeatureMap_t::const_iterator a_it = mySizeMap.find(ikeyword);
    if (a_it == mySizeMap.end()) return NULL;
    //
    return (MvDataSizeFeature_t*)((*a_it).second);
}

void MvDescriptorParser_t::addSizeFeature(int ikeyword, MvDataSizeFeature_t* dsf_p) {
    mySizeMap[ikeyword] = dsf_p;
}


/* --------- Post-treatements --------- */


void MvDescriptorParser_t::postRead(const MvDescriptor_t* descr_p) const {
    // Loop on domains
    MvLockedIKeywordMap_t::const_iterator a_li_it_begin = myLockedIKeywords.begin();
    MvLockedIKeywordMap_t::const_iterator a_li_it_end = myLockedIKeywords.end();
    MvLockedIKeywordMap_t::const_iterator a_li_it;
    for (a_li_it = a_li_it_begin; a_li_it != a_li_it_end; ++a_li_it) {
        MvDomain_e             a_domain = (*a_li_it).first;
        const MvIKeywordSet_t& a_ikeywords = (*a_li_it).second;
        //
        MvIKeywordSet_t::const_iterator a_ikw_it_begin = a_ikeywords.begin();
        MvIKeywordSet_t::const_iterator a_ikw_it_end = a_ikeywords.end();
        MvIKeywordSet_t::const_iterator a_ikw_it;
        for (a_ikw_it = a_ikw_it_begin; a_ikw_it != a_ikw_it_end; ++a_ikw_it) {
            int a_ikeyword = (*a_ikw_it);
            MvIKeywordFeatureMap_t::const_iterator a_it = myIKeywordFeatureMap.find(a_ikeyword);
            //
            if (a_it == myIKeywordFeatureMap.end()) {
                string a_skeyword = descr_p->getSKeyword(a_ikeyword);
                throwError(getMsg(72), a_skeyword.c_str());
            }
            //
            MvDataFeature_t* a_feature_p = (*a_it).second;
            a_feature_p->addLockedDomain(a_domain);
        }
    }
}



void MvDescriptorParser_t::loc_multi_array_not_acceptable(const MvDescriptor_t* descr_p, const ff_cell_t* a_cell_p)
{
    int a_cell_ikw = END_ARGS;
   ff_cell_type_e a_cell_type = CELL_UNKNOWN;
   MCDS_get_ff_cell_attributes(a_cell_p, CELL_TYPE, &a_cell_type, CELL_IKEYWORD,&a_cell_ikw,END_ARGS);
   bool a_ok = false;
   if(a_cell_type != CELL_LIST) a_ok = descr_p->isMultiDimensionalArray(a_cell_ikw);
    if (a_ok) {
        string a_msg = descr_p->getSKeyword(a_cell_ikw) + ", multidimensional array isn't acceptable in this card.";
        throwError(a_msg.c_str());
    }
}
void MvDescriptorParser_t::loc_multi_array_not_acceptable(const MvDescriptor_t* descr_p, int a_cell_ikw)
{
    bool a_ok = descr_p->isMultiDimensionalArray(a_cell_ikw);
    if (a_ok) {
        string a_msg = descr_p->getSKeyword(a_cell_ikw) + ", multidimensional array isn't acceptable in this card.";
        throwError(a_msg.c_str());
    }
}

void MvDescriptorParser_t::fillFilterDetails(string& feature_keyword, StringVect_t& attrib_vect, StringVect_t& value_vect, StringVect_t& criteria_vect, StringVect_t& unit_vect,
    StringVect_t& message_vect, int* nb_filters)
{
    string copy_str(feature_keyword);
    StringVect_t pTokens;
    StringTokenize(copy_str, pTokens, "/");
    int size = (int)pTokens.size();
    int i = 0;
    for (i = 1; i < size; i++)
    {
        string token = pTokens[i];
        if (i == 1)
        {
            attrib_vect.push_back(token);
        }
        else if (i == 2)
        {
            value_vect.push_back(token);
        }
        else if (i == 3)
        {
            criteria_vect.push_back(token);
        }
        else if (i == 4)
        {
            unit_vect.push_back(token);
        }
        else if (i == 5)
        {
            message_vect.push_back(token);
        }
    }
    if (size == 1)
    {
        throwError(getMsg(86));
    }
    else if (size == 2)
    {
        throwError(getMsg(87));
    }

    char c = getNextChar();
    if (c != ';')
    {
        if (message_vect.empty())
            throwError(getMsg(92));
        string messg_str = message_vect[0];
        int size = (int)messg_str.size();
        size += 2; // because one space, and one char has been read
        // reach before first quote, ", by unreadchar size times
        while (size)
        {
            unreadChar();
            size--;
        }
        string total_str = getNextQuotedString();
        message_vect.clear();
        message_vect.push_back(total_str);
    }
    else
    {
        // to pass upcoming check for ';'
        unreadChar();
    }
    // for such filters /FILTER/COMBINE/OR,AND, need to push "" in not needed criteria_vect, unit_vect, message_vect
    // so that all the filters informations are in sync
    if (i == 3)
    {
        criteria_vect.push_back("");
        unit_vect.push_back("");
        message_vect.push_back("");
    }
    else if (i == 4)
    {
        unit_vect.push_back("");
        message_vect.push_back("");
    }

    (*nb_filters)++;
}


/* --------- Static functions --------- */

static value_type_e loc_get_vtype(const string& vtype) {
    if (vtype == "BOOL")    return VTYPE_BOOL;
    if (vtype == "INT")    return VTYPE_INT;
    if (vtype == "UINT")    return VTYPE_UINT;
    if ((vtype == "FLOAT") || (vtype == "DOUBLE"))  return VTYPE_FLOAT;
    if (vtype == "STRING") return VTYPE_STRING;
    if (vtype == "OBJECT") return VTYPE_OBJECT;
    return VTYPE_UNKNOWN;
}

static int loc_string2int(const string& s) {
    char* a_res = NULL;
    int an_int = (int)strtol(s.c_str(), &a_res, 10);
    if (a_res == s.c_str()) throw MvError_t(MV_get_msg_array(MSGT_KERNEL)[12], s.c_str());
    return an_int;
}


static double loc_string2double(const string& s) {
    char* a_res = NULL;
    double  a_result = strtod(s.c_str(), &a_res);
    if (a_res == s.c_str()) throw MvError_t(MV_get_msg_array(MSGT_KERNEL)[82], s.c_str());
    return a_result;
}


static int loc_get_fmt(const string& fmt_str, LocFormatList_t* fmt_p) {
    int a_nb_char = (int)(fmt_str.size());
    if (a_nb_char <= 0) return 1;
    //
    //cout << "---------" << endl << "\"" << fmt_str << "\"" << endl;
    
    int i_ini = 0, i = i_ini;
    while (i < a_nb_char) {
        char c = fmt_str[i];
        if (c == '%') {
            if (i > i_ini) {
                fmt_p->push_back(fmt_str.substr(i_ini, i - i_ini));
                //cout << "\"" << fmt_str.substr(i_ini,i-i_ini) << "\" pushed" << endl;
                i_ini = i;
            }
            //
            while (c != 'f' && c != 'g' && c != 'd' && c != 'i' && c != 's' && c != 'u' && c != 'e' && c != 'E' && i < a_nb_char) c = fmt_str[++i];
            if (i < a_nb_char) c = fmt_str[++i];
            fmt_p->push_back(fmt_str.substr(i_ini, i - i_ini));
            //cout << "\"" << fmt_str.substr(i_ini,i-i_ini) << "\" pushed" << endl;
            i_ini = i;
        }
        else {
            ++i;
        }
    }
    if (i > i_ini) {
        fmt_p->push_back(fmt_str.substr(i_ini, i - i_ini));
        //cout << "\"" << fmt_str.substr(i_ini,i-i_ini) << "\" pushed (last)" << endl;
    }
    /*
    int i_ini=0,i=i_ini;
    char c_ini=fmt_str[i++],c;
    while(i<a_nb_char) {
      c=fmt_str[i];
      switch(c_ini) {
      case '%':
        switch(c) {
        case ' ': //no break
        case '%': fmt_p->push_back(fmt_str.substr(i_ini,i-i_ini)); i_ini=i++; c_ini=c; break;
        default:  ++i; break;
        }
        break;
      default:
        switch(c) {
        case '%': fmt_p->push_back(fmt_str.substr(i_ini,i-i_ini)); i_ini=i++; c_ini=c; break;
        default:  ++i; break;
        }
        break;
      }
    }
    if(i>i_ini) fmt_p->push_back(fmt_str.substr(i_ini,i-i_ini));
    */
    
    //
    return 0;
}


static ff_card_t* loc_new_card(ff_card_type_e card_type, LocCellList_t& cell_list) {
    ff_card_t* a_card_p;
    MCDS_new_ff_card(&a_card_p, card_type);
    //
    int a_nb_cells = (int)(cell_list.size());
    MCDS_set_ff_card_attributes(a_card_p, CARD_NB_CELLS, a_nb_cells, END_ARGS);
    //
    for (int i = 0; i < a_nb_cells; ++i) {
        ff_cell_t* a_cell_p = cell_list[i];
        MCDS_set_ff_card_tab(a_card_p, CARD_CELL, i, (const void*)(&a_cell_p));
    }
    //
    return a_card_p;
}




void operatorplusegal(MvFullTypeSet_t& fts, const MvObjectTypeSet_t& otypes) {
    MvObjectTypeSet_t::const_iterator a_it_begin = otypes.begin();
    MvObjectTypeSet_t::const_iterator a_it_end = otypes.end();
    MvObjectTypeSet_t::const_iterator a_it;
    for (a_it = a_it_begin; a_it != a_it_end; ++a_it) {
        MvFullType_t a_full_type(*a_it);
        fts.insert(a_full_type);
    }
}







