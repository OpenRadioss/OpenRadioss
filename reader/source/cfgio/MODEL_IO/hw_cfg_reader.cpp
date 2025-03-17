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
/***********************************************************************************
HWCFGReader: HWCFGReader is a versatile data reader or Common Data Reader that can 
read decks from solvers like Radioss, LSDyna, and others. Its functionality is 
based on identifying sequential data patterns such as Start/End,Header Specifier,
multiple headers, header in between, comments, Max Line length, Min cell length,
free format, include file, Submodel, version [lower/higher], Format Types, Encryption,
Unit, Parameter etc. By identifying these data patterns, the reader can convert data 
into neutral objects.
************************************************************************************/

#include <UTILS/win32_utils.h>
#include <UTILS/mv_cstring.h>
#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <UTILS/memory_utils.h>
#include <UTILS/set_utils.h>
#include <UTILS/str_utils.h>
#include <UTILS/file_utils.h>
#include <UTILS/direction_utils.h>
#include <MESSAGE/mv_messages.h>
#include <KERNEL/mv_type.h>
#include <KERNEL/mv_utils.h>
#include <KERNEL/mv_model_descriptors.h>
#include <UTILS/mv_climits.h> 
#include <KERNEL_BASE/fileformat_API.h> 
#include <KERNEL/mv_file_format.h>
#include <cassert>
extern "C"
{
#include <KERNEL_BASE/utils.h>
}
#include <MODEL_IO/cdr_reserveattribs.h>
#include <HCDI/hcdi_multicfgkernelmgr.h>
#include <MODEL_IO/mv_file_stack_track.h>
#include <MODEL_IO/mv_header_positions.h>
#include <MODEL_IO/mv_solver_input_infos.h>
#include <MODEL_IO/mv_element_infos.h> 

#include <MODEL_IO/mec_component.h>
#include <HCDI/hcdi_mec_pre_object.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include <HCDI/hcdi_utils.h>
#include <MODEL_IO/meci_read_model_base.h>
#include <MODEL_IO/meci_model_factory.h>
#include "hw_cfg_reader.h"
#include <boost/algorithm/string/classification.hpp> // Include boost::for is_any_of
#include <boost/algorithm/string/split.hpp> // Include for boost::split
#include <boost/algorithm/string.hpp>
#include <mec_data_writer.h>
typedef InputInfos::MyKeywordSet_t     LocKeywordSet_t;
typedef InputInfos::MyObjectKeywords_t LocObjectKeywords_t;

typedef map<string, int>         LocCounters_t;
typedef pair<string, int>        LocIntFlag_t;
typedef vector<LocIntFlag_t>    LocIntFlagList_t;
typedef pair<string, string>     LocStringFlag_t;
typedef vector<LocStringFlag_t> LocStringFlagList_t;
typedef vector<const char*>    LocOTypeList_t;
typedef vector<MECComponent*>   LocComponentVect_t;
typedef vector<MECReadFile*>    LocIncludeFileVect_t;
typedef map<int, int>             LocIdOrderMap_t;
typedef map<int, LocIdOrderMap_t> LocTransformOrderMap_t;
typedef map<string, int>          LocIndexMap_t;

typedef vector<MECReadFile*>     LocFileVect_t;       
typedef vector<MECComponent*>     LocComponentVect_t;  
typedef pair<string, string>       LocKeywordOType_t;
typedef vector<LocKeywordOType_t> LocObjectKeywords_t;
typedef vector<string>            LocControlCardList_t;
typedef string                    LocRecordBuffer_t;   
typedef set<string>               LocFileList_t;       
typedef stack<int>                LocIntStack_t;      
typedef set<int>                  LocIntSet_t;        


static bool                loc_is_alpha(char c);
static int                 loc_get_fmt_size(const string& fmt);

static int                 loc_get_cell_size(const ff_cell_t* cell_format_p);
static const fileformat_t* loc_get_file_format_ptr(const IDescriptor* descr_p, MvFileFormat_e version);

static bool isDigitString(const char* test_str);


static string loc_manage_slash_name(char** header_tab_p, int start_index, int stop_index)
{
    string concat_string = "";
    int i = start_index;
    while (i < stop_index)
    {
        concat_string = str_printf("%s %s", concat_string.c_str(), header_tab_p[i]);
        ++i;
    }
    return concat_string;
}
void eraseSubstringFromEnd(const char* str, const char* sub) 
{
    std::string modifiedString(str);
    std::size_t pos = modifiedString.rfind(sub);

    if (pos != std::string::npos) {
        
        modifiedString.erase(pos, strlen(sub));
    }

    std::strcpy(const_cast<char*>(str), modifiedString.c_str()); // Modifying const data (caution)
}


/* --------- Constructors & destructor --------- */
#define BUFFER_NB_CHAR 203
#define CELL_LENGTH 10
HWCFGReader::HWCFGReader(const char* full_name, MvFileFormat_e version, ISyntaxInfos& syntaxSolverInfos, InputInfos& solverInf,
                         const ReadFileFactorySP& factory_p) :
    MECIReadModelBase(full_name, BUFFER_NB_CHAR, syntaxSolverInfos.getLineLength(), factory_p),
    myCellLength(syntaxSolverInfos.getCellLength()),
    myLineNbCells(CELL_LENGTH),
    myVersion(version),
    myCommentState(true),
    myModelFactoryPtr(NULL),
    myIsCrypted(false),
    myInHMFlag(true)
{
    myInputInfosPtr = &solverInf;
    mySyntaxInfo = &syntaxSolverInfos;
    myDataReader = this->newDataReader(); //new MECIDataReader(this, mySyntaxInfo, mySyntaxInfo->getLineLength());
    myDataReader->setFormatId(version);
    myCommentsHeader.resize(InputInfos::SOLVER_PROCESS_KEYWORD_COMMENT_BEFORE_AFTER);
    myEncryptionPtr = newEncryption();
}

HWCFGReader::HWCFGReader(MECReadFile* file, MvFileFormat_e version, ISyntaxInfos& syntaxSolverInfos, InputInfos& solverInf,
                         const ReadFileFactorySP& factory_p) :
    MECIReadModelBase(file, BUFFER_NB_CHAR, syntaxSolverInfos.getLineLength(), factory_p),
    myCellLength(syntaxSolverInfos.getCellLength()),
    myLineNbCells(CELL_LENGTH),
    myVersion(version),
    myCommentState(true),
    myModelFactoryPtr(NULL),
    myIsCrypted(false),
    myInHMFlag(true)
{
    myInputInfosPtr = &solverInf;
    mySyntaxInfo = &syntaxSolverInfos;
    myDataReader = this->newDataReader(); //new MECIDataReader(this, mySyntaxInfo, myCellLength * myLineNbCells);
    myDataReader->setFormatId(version);
    myCommentsHeader.resize(InputInfos::SOLVER_PROCESS_KEYWORD_COMMENT_BEFORE_AFTER);
    myEncryptionPtr = newEncryption();
}

HWCFGReader::~HWCFGReader() {
    //
    delete myDataReader;
    if(myEncryptionPtr)
       delete myEncryptionPtr;
}

MECIDataReader* HWCFGReader::newDataReader() 
{
    if (myDataReader)
        return myDataReader;
    MECIDataReader* a_reader = new MECIDataReader(this, mySyntaxInfo, mySyntaxInfo->getLineLength());
    return a_reader; 
}
IEncryption* HWCFGReader::newEncryption()
{
    IEncryption* encryp = nullptr;
    encryp = new IEncryption();
    return encryp;
}


void HWCFGReader::SetVersion(MvFileFormat_e version)
{
    if (myVersion == version || !mySyntaxInfo) return;
    myVersion = version;
    myCellLength = mySyntaxInfo->getCellLength();
    if (myDataReader)
    {
        myDataReader->setFormatId(version);
        myDataReader->setLineLength(mySyntaxInfo->getLineLength());
    }
    SetLineNbChars(mySyntaxInfo->getLineLength());

    if (NULL != myInputInfosPtr)    delete myInputInfosPtr;

    myInputInfosPtr = new SolverInputInfo();
}


void HWCFGReader::readHeaderPositions(MECIModelFactory* model_p) {
    bool        a_continue = true;
 
    int         a_curr_comp_index = -1; 
    string      a_cur_buffer = "", a_kfulltype="";
    int         a_cur_nb_lines = 0;

    //before start reading the starter block clear all the engine header positions
    ClearAllPrevoiusPositionTypes();
    // 
    const CFGKernel* a_cfg_kernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if(nullptr == a_cfg_kernel) return; // shouldn't happen
    SyntaxInfo* pinfo = a_cfg_kernel->getSyntaxInfo();
    set<string> sh_set;
    string begin_str = "";
    if (pinfo)
    {
        std::vector<cfgkernel::Variant> sh_lst = pinfo->getArrayAttribute("STRIP_HEADER");

        if (sh_lst.size())
        {
            for (int j = 0; j < sh_lst.size(); j++)
            {
                string sval = sh_lst[j].getStringValue();
                sh_set.insert(sval);
            }
        }
        cfgkernel::Variant syn_begin = pinfo->getAttribute("BEGIN");
        begin_str = syn_begin.getStringValue();
    }
    bool isexactmatch = myInputInfosPtr->IsExactUsername();
    string a_cur_keyword_str("");
    vector<pair<string, int>>  vec_key_count;
    while (a_continue)
    {
        const char* a_buffer = ReadBuffer(true); 

        if(a_buffer != NULL && mySyntaxInfo->getHeaderSize())
            a_buffer = killBlanksBegin(a_buffer);  // remove leading spaces incase header (*,/,... ) are defined
        //
        if (a_buffer != NULL && !isHeader(a_buffer))
        {
            a_cur_nb_lines++;
            continue;
        }

        if (a_buffer != NULL)
        {
            int a_current_include_file_index_before_header = GetCurrentFileIndex();
            int a_cursubindx1 = MECSubdeck::curSubdeckIdx;
            bool  a_is_header = isHeader(a_buffer);  //isHeader(a_buffer, &a_keyword);
            //
            if (a_is_header)
            {
                bool is_endofkey = myInputInfosPtr->IsEofKeyword(a_buffer);

                obj_type_e etype = HCDI_OBJ_TYPE_NULL;

                const char* a_buffer_d = a_buffer;
                
                if(mySyntaxInfo->GetHeaderFromStartFlag())
                    a_buffer_d = mySyntaxInfo->getNormalisedHeader(a_buffer);// +mySyntaxInfo->getHeaderSize();
                a_buffer_d = killBlanksBegin(a_buffer_d);
                string a_header(a_buffer_d);

                //store the current position after reading the header as these position may change after reading comments after headers..
                int a_current_include_file_index = GetCurrentFileIndex();
                int a_current_component_index = GetCurrentComponentIndex();
                _HC_LONG a_before_read_line = GetPreviousLine();
                _HC_LONG a_before_read_loc = GetPreviousLocation();

                const CUserNameTypeInfo* p_type_info = myInputInfosPtr->GetKeywordSolverInfoFromHeaderLine(a_header, mySyntaxInfo, &sh_set, etype, isexactmatch);
                if (p_type_info)
                {
                    etype = p_type_info->obj_type;

                    if (etype == HCDI_OBJ_TYPE_PARAMETERS)
                    {
                        UnreadBuffer();
                        int r_status = GetReadingStatus();
                        SetReadingStatus(READ_STATUS_SINGLE_KEYWORD);
                        ManageReadKeyWord(model_p, HCDI_get_entitystringtype(HCDI_OBJ_TYPE_PARAMETERS).c_str(), p_type_info);
                        SetReadingStatus(r_status);
                        //readObjects(model_p, HCDI_get_entitystringtype(HCDI_OBJ_TYPE_PARAMETERS).c_str(), HCDI_get_entitystringtype(HCDI_OBJ_TYPE_PARAMETERS).c_str()); 
                        clearHeaderComments();
                        continue;
                    }
                    if (etype == HCDI_OBJ_TYPE_CARDS)
                    {
                        bool begin_key_found = false;
                        const char* begin_proc_str = NULL;
                        begin_proc_str = mySyntaxInfo->getNormalisedHeader(begin_str.c_str());
                        if (strncmp(a_buffer_d, begin_proc_str, (int)strlen(begin_proc_str)) == 0)
                            begin_key_found = true;
                        if (begin_key_found)
                        {
                            UnreadBuffer();
                            int r_status = GetReadingStatus();
                            SetReadingStatus(READ_STATUS_SINGLE_KEYWORD);
                            ManageReadKeyWord(model_p, HCDI_get_entitystringtype(HCDI_OBJ_TYPE_CARDS).c_str(), p_type_info);
                            SetReadingStatus(r_status);
                            clearHeaderComments();
                            continue;
                        }
                    }
                }



                //can get this information fro solverinfo whether to store or not..
                int key_comment_process_flag = myInputInfosPtr->getKeywordProcessCommentFlag();
                if (key_comment_process_flag == InputInfos::SOLVER_PROCESS_KEYWORD_COMMMENT_AFTER)
                {
                    bool iscomment = true;
                    while (iscomment)
                    {
                        char* buffer = ReadBuffer(false, -1, false);
                        if (!buffer)
                        {
                            UnreadBuffer();
                            break;
                        }
                        else
                        {
                            iscomment = isComment(buffer);
                            if (iscomment)
                            {
                                myCommentsHeader[InputInfos::SOLVER_PROCESS_KEYWORD_COMMMENT_AFTER].push_back(buffer);
                            }
                            else
                                UnreadBuffer();
                        }
                    }
                } 
                
                InputInfos::IdentifierValuePairList  vallst;
                myInputInfosPtr->ProcessKeywordComments(myCommentsHeader, etype, vallst); // process the comments to name value pair.
                size_t headerlen = mySyntaxInfo->getHeaderKeywordCellLength();
                if (headerlen > 0)
                {
                    char free_c = mySyntaxInfo->getFreeFormatSpecifier();
                    size_t a_header_size = a_header.length();
                    if(a_header_size > headerlen) 
                        a_header.erase(headerlen); // Erase everything after headerlen character if defined ex: "MAT1    1  2"  

                    for (int i = 0; i < headerlen; i++)
                    {
                        if (a_header_size <= i)
                            break;

                        if (a_header[i] == ' ' || a_header[i] == free_c)
                        {
                            a_header.erase(i); // Find the last non-space and free format character index ex: "MAT1 ,1"
                            break;
                        }
                    }
                }



                clearHeaderComments();
                a_curr_comp_index = GetCurrentComponentIndex();
                a_cur_nb_lines = 0;
                //
                if (!p_type_info)
                {
                    displayCurrentLocation(MSG_ERROR);
                    displayMessage(MSG_ERROR, getMsg(1), a_header.c_str());
                }


                //Store Current Position Type 
                MvHeaderPositions_t& a_header_positions = (*((MvHeaderPositions_t*)myHeaderPositionsPtr));

                a_header_positions.setTypeString(MV_get_type(etype)); //(a_kfulltype);
                a_header_positions.setComponentIndex(a_current_component_index);
                a_header_positions.setFileIndex(a_current_include_file_index);
                int a_line_counts = 0;
                int a_cursubindx = MECSubdeck::curSubdeckIdx;
                
                //

                MECReadFile* a_cur_file_p = GetCurrentFilePtr();
                if (a_cur_file_p)
                {
                    int file_index = GetCurrentFileIndex();
                    int subdeck_index = getCurrentSubdeckIndex();
                    _HC_LONG aloc = a_cur_file_p->GetCurrentLocation();
                    _HC_LONG acurline = a_cur_file_p->GetCurrentLine();
                    char buffer_p[512];
                    char* kw_p = NULL;
                    bool kw_found = false;
                    bool do_continue = true;

                    while (do_continue)
                    {
                        aloc = a_cur_file_p->GetCurrentLocation();
                        acurline = a_cur_file_p->GetCurrentLine();

                        bool a_error = a_cur_file_p->readBuffer(myBufferNbChars, buffer_p);
                        if (!a_error)
                            do_continue = false;

                        bool a_is_eof = (a_cur_file_p == NULL || a_cur_file_p->eof());
                        if (a_is_eof)
                            do_continue = false;

                        bool  is_header = isHeader(buffer_p);
                        bool a_is_include = myInputInfosPtr->IsIncludeHeader(buffer_p);
                        bool a_is_comment = mySyntaxInfo->isComment(buffer_p);

                        if (!is_header && !a_is_include)
                        {
                            if (a_is_comment)
                                continue;

                            a_line_counts++;
                            continue;
                        }
                        do_continue = false;
                    }
                    a_cur_file_p->SetCurrentLocation(aloc);
                    a_cur_file_p->SetCurrentLine(acurline);
                    SetCurrentFileIndex(file_index);
                    setCurrentSubdeckIndex(subdeck_index);
                }
                a_header_positions.addKeywordData(a_before_read_line, a_before_read_loc, a_cursubindx, a_header, vallst, p_type_info, a_line_counts);
                
                //
                if (is_endofkey) 
                {
                    if(a_current_include_file_index == 0)//main file
                       a_continue = false;
                    else
                    {
                        MECReadFile* a_cur_file_p = GetCurrentFilePtr();
                        if(a_cur_file_p)
                            a_cur_file_p->seek(0, SEEK_END);
                    }
                }
            }
            else
                a_cur_nb_lines++;
        }
        else
        {
            a_continue = false;
            if (!eof())
            {
                displayCurrentLocation(MSG_ERROR);
                displayMessage(MSG_ERROR, getMsg(2));
            }
        }
    }

    model_p->SortPreObjectsByName("PARAMETER");


    //resolved submodel parameter if any




    //MvHeaderPositions_t& a_header_positions = (*((MvHeaderPositions_t*)myHeaderPositionsPtr));
    //string str = a_header_positions.displayInfo();

    // check 	each file is closed 
    LocFileVect_t* a_cur_file_vect_p = ((LocFileVect_t*)GetFilePtrLst());
    for (int ifile = 0; ifile < a_cur_file_vect_p->size(); ifile++)
    {
        MECReadFile* a_file = (*a_cur_file_vect_p)[ifile];
        a_file->close();
    }
    // reset all the stack
    LocIntStack_t* a_cur_comp_stack_p = ((LocIntStack_t*)GetComponentIndexStackPtr());
    while (!a_cur_comp_stack_p->empty())
        a_cur_comp_stack_p->pop();
    LocIntStack_t* a_cur_file_stack_p = ((LocIntStack_t*)GetFileIndexStackPtr());
    while (!a_cur_file_stack_p->empty())
        a_cur_file_stack_p->pop();
}

/* --------- Reading model (public) --------- */

void HWCFGReader::readModel(MECIModelFactory* model_p, bool do_transform) {
    SetModelFactoryPtr(model_p);
    MECIReadModelBase::readModel(model_p, do_transform);


    std::map<string, CUserNameTypeInfo>  objectsolverinfo[HCDI_OBJ_TYPE_HC_MAX];
    const CFGKernel* a_cfg_kernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if(nullptr == a_cfg_kernel) return; // shouldn't happen
    a_cfg_kernel->getlUserNamesSolverInfo(objectsolverinfo, true);

    //set order of reading 
    //readObjects(model_p, HCDI_get_entitystringtype(HCDI_OBJ_TYPE_PARAMETERS).c_str(), HCDI_get_entitystringtype(HCDI_OBJ_TYPE_PARAMETERS).c_str()); 
    //readObjects(model_p, /*myInputInfosPtr->GetEncryptedKeyword()*/HCDI_get_entitystringtype(HCDI_OBJ_TYPE_ENCRYPTIONS).c_str(), HCDI_get_entitystringtype(HCDI_OBJ_TYPE_ENCRYPTIONS).c_str());

    // some types read first (maybe get from Factory or SolverInfo?)
    std::vector<int> first_types = {
        HCDI_OBJ_TYPE_ENCRYPTIONS, HCDI_OBJ_TYPE_NODES,
        HCDI_OBJ_TYPE_MATS, HCDI_OBJ_TYPE_PROPS, HCDI_OBJ_TYPE_COMPS, HCDI_OBJ_TYPE_ELEMS, HCDI_OBJ_TYPE_SETS};
    mySkipIncludeReading = true;
    for(int i : first_types)
    {
        readObjects(model_p, HCDI_get_entitystringtype(i).c_str(), HCDI_get_entitystringtype(i).c_str());
    }
    // now read other types
    for (int i = 0; i < HCDI_OBJ_TYPE_HC_MAX; i++)
    {
        size_t a_size = objectsolverinfo[i].size();
        if (!a_size || HCDI_OBJ_TYPE_PARAMETERS == i ||
            std::find(first_types.begin(), first_types.end(), i) != first_types.end())
        {
            continue;
        }

        readObjects(model_p, HCDI_get_entitystringtype(i).c_str(), HCDI_get_entitystringtype(i).c_str()); 
    }
    mySkipIncludeReading = false;
}

bool HWCFGReader::readHeader(const CUserNameTypeInfo* headerdata,      string&                    input_ftype_str, 
                             string&              kernel_ftype_str,      const PseudoDescriptor_t**  descr_pp,
                             const PseudoFileFormat_t**  format_pp,      int*                            id_p,
                             char**                        title_p,      int* unit_id_p,    char** id_param_p,
                             char**                id_unit_param_p,      obj_type_e                    &etype,
                             char**                    header_card,      int                         curHeaderPos)
{
    string        a_norm_otype = "", a_otype_str = "", a_oftype = "";
    bool          a_is_titled = false;
    string        a_subtypes = "";
    int           a_nb_fields = 0, a_otype_ind = 0;
    string        a_title = "";
    string        modif_title = "";
    bool          is_title_modified = false;

    char* a_header = NULL;
    char* a_header_fields[50];    // read line
    if (curHeaderPos > 0)
    {
        MECReadFile* a_cur_file_p = GetCurrentFilePtr();
        a_cur_file_p->SetCurrentLocation((_HC_LONG)curHeaderPos);
        a_header = my_strdup(ReadBuffer());
    }
    else if ( *header_card == NULL || (*header_card && !isHeader(*header_card)) )
        a_header = my_strdup(ReadBuffer());
    else
        a_header = my_strdup(*header_card);

    if (!a_header)
        return false;

    killBlanksNLEnd(a_header);

    //if (isHeader(header_card))
    //    a_header = a_header + 1;

    string header_str(mySyntaxInfo->getNormalisedHeader(a_header));
    //obj_type_e etype = HCDI_OBJ_TYPE_NULL;
    if(!headerdata)
        headerdata = myInputInfosPtr->GetKeywordSolverInfoFromHeaderLine(header_str, mySyntaxInfo, nullptr, etype);

    if (!headerdata)
        return false;

    etype = headerdata->obj_type;
    kernel_ftype_str =  string("/") + MV_get_type(headerdata->obj_type) + string("/") + headerdata->myusername;
    if(input_ftype_str.empty())
        input_ftype_str = a_header;
    const IDescriptor *a_descr_p = headerdata->pdescrp;
    //
    if (!a_descr_p)
         return false;

    *descr_pp = (const MvDescriptor_t*)a_descr_p;

    const fileformat_t* a_format_p = loc_get_file_format_ptr(a_descr_p, getFormatVersion());
    if (NULL != format_pp) *format_pp = (const PseudoFileFormat_t*)a_format_p;


    ApplicationMode_e a_mode = (ApplicationMode_e)myInputInfosPtr->GetAppMode();

    /** Added this to respect the cfg files written without _ID_, title and unitid.
    *   Can also remove this block but need to correct cfg files with _ID_, TITLE, and unitid.
    **/
    if (a_mode == HCDI_SOLVER_LSDYNA)
    {
        if (headerdata->myAllPossibleflags)
        {
            static string can_have_title_card_str = "CAN_HAVE_TITLE_CARD";
            static string can_have_id_card_str    = "CAN_HAVE_ID_CARD";
            static string has_title_card_str      = "HAS_TITLE_CARD";
            static string has_id_card_str         = "HAS_ID_CARD";
            static string title_str               = "_TITLE";
            static string id_str                  = "_ID";

            /*shoud be a part of member variable later*/
            int bit_can_have_title  = HCDI_get_data_hierarchy_bitmask(can_have_title_card_str);
            int bit_hastitle        = HCDI_get_data_hierarchy_bitmask(has_title_card_str);

            bool has_title          = headerdata->myAllPossibleflags & bit_hastitle;
            bool can_have_title = false;
            if(!has_title)
                can_have_title = headerdata->myAllPossibleflags & bit_can_have_title;

            if (has_title || can_have_title)
            {
                bool title_flag = true;
                if (!has_title)
                {
                    size_t pos = input_ftype_str.find(title_str);
                    if (pos == string::npos)
                        title_flag = false;
                }
                if (title_flag)
                {
                    char* a_buffer = ReadBuffer();
                    if (a_buffer == NULL)
                    {
                        my_free(a_header);
                        return false;
                    }
                    killBlanksNLEnd(a_buffer);
                    const char* a_title = killBlanksBegin(a_buffer);
                    if (title_p != NULL)
                    {
                        *title_p = strdup(a_title);
                        if (!*header_card)
                            *header_card = my_strdup(a_header);
                    }
                }
            }
            else
            {
                int bit_has_idcard = HCDI_get_data_hierarchy_bitmask(has_id_card_str);
                int bit_can_have_idcard = HCDI_get_data_hierarchy_bitmask(can_have_id_card_str);

                bool has_idcard = headerdata->myAllPossibleflags & bit_has_idcard;
                bool can_have_idcard = false;
                if (!has_idcard && bit_can_have_idcard)
                    can_have_idcard = headerdata->myAllPossibleflags & bit_can_have_idcard;

                if (has_idcard || can_have_idcard)
                {
                    bool idcard_flag = true;
                    if(can_have_idcard)
                    {
                        size_t pos = input_ftype_str.find(id_str);
                        if (pos == string::npos)
                            idcard_flag = false;
                    }

                    if(idcard_flag)
                    {
                        char* a_buffer = ReadBuffer();
                        if (a_buffer == NULL)
                        {
                            my_free(a_header);
                            return false;
                        }
                        killBlanksNLEnd(a_buffer);
                        const char* line = a_buffer; //dont kill begin end

                        MYOBJ_INT id = 0;
                        char id_field[11];
                        string param_str = "";

                        strncpy(id_field, line, 10);
                        id_field[10] = '\0';
                        if (!*header_card)
                            *header_card = my_strdup(a_header);
                        int cell_length = 10;
                        bool is_param_cell = myDataReader->isParameterCell(id_field, cell_length, param_str);

                        if (is_param_cell) // need to return parameter id str
                        {
                            id = 0;
                            *id_param_p = strdup(param_str.c_str());
                            //a_pre_object_p->SetId(id);
                            //a_pre_object_p->SetParameterIdName(param_str.c_str());
                        }
                        else
                        {
                            sscanf(id_field, "%u", &id);
                            if (id_p != NULL)
                                *id_p = id;
                        }

                        if (10 < strlen(line))
                        {
                            bool IsTitleCardEmpty = true;
                            int i = 0;
                            while ((line[i + 10] != '\0') && (line[i + 10] != '\n') && (line[i + 10] != '\r'))
                            {
                                if (line[i + 10] != ' ')
                                {
                                    IsTitleCardEmpty = false;
                                    break;
                                }
                                i++;
                            }
                            if (!IsTitleCardEmpty)
                            {
                                if (title_p != NULL)
                                {
                                    *title_p = strdup(line + 10);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    else if (a_mode == HCDI_SOLVER_RADIOSS)
    {
        if (!a_format_p)
        {
            
            // in the given format (i.e. format not found)
            MvFileFormat_e first_format = FF_D00_LAST;
            MvFileFormat_e current_format = getFormatVersion();
            for (int i = current_format + 1; i < FF_D00_LAST; ++i)
            {
                a_format_p = a_descr_p->getFileFormatPtr((MvFileFormat_e)i);
                if (NULL != a_format_p)
                {
                    first_format = (MvFileFormat_e)i;
                    break;
                }
            }
            if (FF_D00_LAST != first_format)
            {
                const char* format_name = MV_get_file_format(first_format).c_str();
                if (format_name && !strncmp(format_name, "radioss", 7)) format_name = format_name + 7;
                strcpy(myLineBuffer, header_str.c_str()); // for error message
                displayMessage(MSG_WARNING, getMsg(51), input_ftype_str.c_str(), format_name);
                if (NULL != format_pp) *format_pp = (const PseudoFileFormat_t*)a_format_p;
            }
            else
            {
                return false;
            }
        }

        // Title
        MvFileFormat_e format_version = getFormatVersion();
        a_is_titled = MV_is_titled(etype, "RADIOSS");
        if (title_p != NULL && format_version >= FF_D00_51)
        {
            if (a_is_titled) {
                char* a_buffer = ReadBuffer();
                if (a_buffer == NULL)
                {
                    my_free(a_header);
                    return false;
                }
                killBlanksNLEnd(a_buffer);
                a_title = killBlanksBegin(a_buffer);
                if (!*header_card)
                    *header_card = strdup(a_header);
            }
            *title_p = strdup(a_title.c_str());
        }

        int           a_id_ind = 0;
        int a_nb_fields = splitHeader(a_header, a_header_fields);
        // Type
        LocKeywordSet_t a_prefix_keywords; /*= (*((const LocKeywordSet_t *)(myInputInfosPtr->GetPrefixKeywords())));*/

        a_otype_str = a_header_fields[a_otype_ind];
        //if (a_otype_str < a_prefix_keywords) a_otype_str = a_header_fields[++a_otype_ind];
        //

        static const char* cUNSUPPORTEDCARD = "UNSUPPORTEDCARD";
        a_norm_otype = a_otype_str.c_str();

        if (1 < a_nb_fields && !strcmp(a_header_fields[0], "TABLE"))
        {
            a_id_ind = 2;
        }
        else if (1 < a_nb_fields && !strcmp(a_header_fields[0], "ADMAS"))
        {
            a_id_ind = 2;
        }
        else
        {
            // first try to find a composed subtype (e.g. /IMPVEL/FGEO is /LOAD/IMPVEL_FGEO, not /LOAD/IMPVEL)
            for (int i = 0; i < a_nb_fields; i++)
            {
                if (isDigitString(a_header_fields[i]))
                    break;
                ++a_id_ind;
                // correction for a weird case: if the last field is empty, we suppose that it is
                // the id field, but the id is missing (this is the case in some BVT)
                if (i == a_nb_fields - 1 && strlen(a_header_fields[i]) == 0) --a_id_ind;
            }
        }


        // Manage the additional "/" in name of header (for MAT ,PROP and PART only) in 44
        // those "\" create an offset that make some errors.
        // Also search for position of the id.
        if (format_version < FF_D00_51)
        {
            // MAT and PROP options are always "/MAT/LAW/id/title" so in header:
            // id is at pos a_header_fiels[2], and nb_fields is 4;
            if (etype == HCDI_OBJ_TYPE_MATS || etype == HCDI_OBJ_TYPE_PROPS)
            {
                a_id_ind = 2;
                if (a_nb_fields > 4)
                {
                    // conactenate every name string like in case /MAT/LAW/id/a/material/name into "a material name"
                    modif_title = loc_manage_slash_name(a_header_fields, 3, a_nb_fields);
                    a_nb_fields = 4;
                    is_title_modified = true;
                }
            }
            // for PART and FUNCTION, it is "/TYPE/id/title" so in header:
            // id is at a_header_fiels[1], and nb_fields is 3
            else if ((etype == HCDI_OBJ_TYPE_COMPS || etype == HCDI_OBJ_TYPE_CURVES) &&
                a_nb_fields != 3)
            {
                a_id_ind = 1;
                // conactenate every name string like in case /PART/id/a/part/name into "a part name"
                modif_title = loc_manage_slash_name(a_header_fields, 2, a_nb_fields);
                a_nb_fields = 3;
                is_title_modified = true;
            }
            else if ((etype == HCDI_OBJ_TYPE_DAMPINGS) && (a_nb_fields == 3))
            {
                a_title = a_header_fields[2];
                a_id_ind = 1;
            }
            else
            {
                // by default, we consider the first numeric field to be the id
                a_id_ind = 1;
                while (a_id_ind < a_nb_fields - 1 && !isDigitString(a_header_fields[a_id_ind])) ++a_id_ind;
            }
            // Title
            if (title_p != NULL) {
                if (a_is_titled) {
                    if (is_title_modified)
                        a_title = killBlanksBegin(modif_title.c_str());
                    else
                        a_title = killBlanksBegin(a_header_fields[a_nb_fields - 1]);
                }
                *title_p = strdup(a_title.c_str());
            }
        }

        // Id
        if (id_p != NULL)
        {
            if (format_version > FF_D00_51)
            {
                if (etype == HCDI_OBJ_TYPE_SOLVERMASSES)
                {
                    a_id_ind = 2;
                }
                else if ((etype == HCDI_OBJ_TYPE_CURVES) && (!(strncmp(a_norm_otype.c_str(), "TABLE", 5))))
                {
                    a_id_ind = 2;
                }
                else
                {
                    if (a_nb_fields > 2)
                    {
                        //HC100_2 DEFECT62668 --venkat k 01-04-09 (BEG)
                        //Here the strtol is same as atoi plus its copies trailing character to
                        //the chk. it is useful when string composed of alphanumeric and starts with digit
                        //eg; /MAT/3D_COMP/2
                        char* chk = NULL;
                        long int chk_val = strtol(a_header_fields[a_id_ind - 1], &chk, 10);
                        if (chk_val > 0 && chk_val < LONG_MAX && strcmp(chk, "") == 0)
                        {
                            a_id_ind--;
                        }
                        //HC100_2 DEFECT62668 --venkat k 01-04-09 (END)
                    }
                }
            }
            if (a_id_ind > -1 && (a_id_ind < a_nb_fields))
            {
                killBlanksNLEnd(a_header_fields[a_id_ind]);
                if (a_header_fields[a_id_ind][0] == '&')
                {
                    //Get name of the parameter
                    char* param_p = a_header_fields[a_id_ind];
                    //Earlier we use to remove the "&" in "GetParameterObject", but now that code is removed and hence need to do this modification here
                    //To remove the "&" from the referred place for parameter so that we send back only the name without &
                    //Change related to issue #287368, 287485, 287728, 287515
                    if (param_p[0] == '&') param_p = param_p + 1; //Remove '&' if present
                    if (nullptr != id_param_p)  *id_param_p = strdup(param_p);
                    if (myModelFactoryPtr)
                    {
                        //Get parameter value
                        *id_p = myModelFactoryPtr->GetIntParameter(param_p, GetCurrentFileIndex());
                    }
                }
                else
                {
                    *id_p = atoi(a_header_fields[a_id_ind]);
                }
            }
            if (*id_p == 0)
                *id_p = 1;
        }

        // populate input_ftype_str / keyword
        if(a_id_ind > -1 && (a_id_ind < a_nb_fields)) // should always be true
        {
            input_ftype_str.clear();
            for(int i = 0; i < a_id_ind; ++i) 
            {
                input_ftype_str += string("/") + a_header_fields[i];
            }
        }

        // Unit ID 
        if (unit_id_p != NULL)
        {
            if (format_version > FF_D00_51)
            {
                if (a_id_ind < a_nb_fields - 1)
                {
                    killBlanksNLEnd(a_header_fields[a_id_ind + 1]);
                    if (a_header_fields[a_id_ind + 1][0] == '&')
                    {
                        //Get name of the parameter
                        char* param_p = a_header_fields[a_id_ind + 1];
                        if (param_p[0] == '&') param_p = param_p + 1; //Remove '&' if present
                        if (nullptr != id_unit_param_p) *id_unit_param_p = strdup(param_p);
                        if (myModelFactoryPtr)
                        {
                            //Get parameter value
                            *unit_id_p = myModelFactoryPtr->GetIntParameter(param_p, GetCurrentFileIndex());
                        }
                    }
                    else
                    {
                        *unit_id_p = atoi(a_header_fields[a_id_ind + 1]);
                    }
                }
            }
        }
    }
    if (format_pp == NULL) return false;

    my_free(a_header);
    return true;
}


//++////////////////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//
//      This function is used to allocate the memory for the objects to be created.
//      In the function, we firstly get the object number whose type is given by otype.
//      Then check validity of keywords and allocate the memory.
//Parameters:
//      MECIModelFactory *model_p: a pointer to the model object
//      const char * keyword: keyword
//      const char *otype: object type
//Return value:
//      None
//--//////////////////////////////////////////////////////////////////////////////////////////
void HWCFGReader::initReadObjects(MECIModelFactory* model_p, const char* keyword, const char* otype) {  
    //need to implement later
    //int a_nb_objects = 0;
    //if (a_nb_objects > 0)
    //{
    //    model_p->Alloc(otype,a_nb_objects);
    //}
}

//++///////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//	This function is used to update the container corresponding to the otype in the model.
//Parameters:
//	MECIModelFactory *model_p: a pointer to the model object
//      const char *:
//      const char *otype: object type
//Return value:
//None
//--///////////////////////////////////////////////////////////////////////////////
void HWCFGReader::closeReadObjects(MECIModelFactory* model_p,
    const char* keyword,
    const char* otype)
{
  //model_p->PostTreat(otype,keyword, IfCleanButnotSave);
}

bool HWCFGReader::checkObject(const IMECPreObject& object, const MECIModelFactory* model_p) const {
    bool a_ok = true;
    return a_ok;
}

void HWCFGReader::readCurrentObjects(vector<IMECPreObject*>& preobj_lst, char* header_card, int linecount) {
    // Header
    int   a_id = 0;
    char* a_title_str = NULL;
    string a_iftype_str, a_kftype_str;
    int   a_unit_id = 0;
    char* id_param_p = NULL;
    const IDescriptor* a_descr_p = NULL;
    const fileformat_t* a_format_p = NULL;
    obj_type_e etype;
    char* id_unit_param_p = NULL;
    /* Needs to initialize myIsCrypted, myCryptType, myCommentState before calling readHeader: any new header*/
    myIsCrypted = false;
    myEncryptionPtr->SetCryptingMethod(IEncryption::CRYPT_UNKNOWN);
    myCommentState = true;


    bool  a_is_readable = readHeader(nullptr, a_iftype_str, a_kftype_str, (const void**)&a_descr_p, (const void**)&a_format_p, &a_id, &a_title_str, &a_unit_id, &id_param_p, &id_unit_param_p, etype, &header_card);
    // Reading cards
    if (a_is_readable)
    {
        // Getting descriptor and format
        if (a_is_readable && a_format_p) {
            object_type_e a_kernel_otype = HCDI_GetHCObjectType(a_kftype_str);

            int subdeck_index = getCurrentSubdeckIndex();
            // Creating pre-object
            IMECPreObject* a_pre_object = HCDI_GetPreObjectHandle(a_kftype_str.c_str(), a_iftype_str != "" ? a_iftype_str.c_str() : a_kftype_str.c_str(), a_title_str, a_id, a_unit_id);
            if (a_pre_object == NULL)
                return;
            a_pre_object->Init((const PseudoDescriptor_t*)a_descr_p);
            a_pre_object->SetEntityType(etype);

            a_pre_object->SetSubdeckIndex(subdeck_index);

            bool          a_is_cryptable = myInputInfosPtr->IsEntityCryptable(a_kernel_otype, getFormatVersion());
            if (a_is_cryptable) {

                if (!myEncryptionPtr->IsKeySupported())
                {
                    bool a_continue = true;
                    myDoRecord = true;
                    StartRecording();
                    pushPosition();
                    bool a_isencrypted = false;
                    while (a_continue)
                    {
                        const char* a_buffer = ReadBuffer(true);
                        if (a_buffer == NULL)
                            break;

                        if (mySyntaxInfo->getHeaderSize())
                            a_buffer = killBlanksBegin(a_buffer);
                        if (isHeader(a_buffer))
                        {
                            const char* a_crypted_data = NULL;
                            a_crypted_data = StopRecording();
                            UnrecordLastComments();
                            if (a_isencrypted)
                            {
                                const char* a_copy_cryp_data = strdup(a_crypted_data);
                                if (a_crypted_data && *a_crypted_data != '\0')
                                {
                                    eraseSubstringFromEnd(a_copy_cryp_data, a_buffer);
                                    a_pre_object->SetCryptedData(a_copy_cryp_data);
                                    const char* a_crypt_ref = myEncryptionPtr->GetCurCryptingReference() + (myEncryptionPtr->GetCurCryptingReference()[0] == myEncryptionPtr->GetCrypChar() ? 1 : 0);
                                    a_pre_object->SetCryptingReference(a_crypt_ref);
                                }
                            }
                            UnreadBuffer();
                            myIsCrypted = false;
                            break;
                        }
                        //
                        if (myEncryptionPtr->IsStringEncrypted(a_buffer))
                        {
                            myEncryptionPtr->SetCryptingMethod(a_kernel_otype);
                            a_isencrypted = true;
                        }
                    }
                    if (!a_isencrypted)
                    {
                        popPosition();
                        StopRecording();
                        UnrecordLastComments();
                        ClearRecordBuffer();
                    }
                    else
                    {
                        ClearRecordBuffer();
                        // Freeing strings
                        if (a_title_str)
                            myfree(a_title_str);
                        if (id_param_p)
                            myfree(id_param_p);

                        preobj_lst.push_back(a_pre_object);
                        return ;
                    }
                }
                else
                    myEncryptionPtr->SetCryptingMethod(a_kernel_otype);

                StartRecording();
            }
            //
            bool a_ok = false;



            a_ok = readObjectData((const PseudoFileFormat_t*)a_format_p, a_pre_object, NULL, (const PseudoDescriptor_t*)a_descr_p);
            if (a_is_cryptable)
            {
                const char* a_crypted_data = NULL;
                if (a_ok && isCrypted())
                {
                    a_crypted_data = StopRecording();
                    a_pre_object->SetCryptedData(a_crypted_data); 
                    UnrecordLastComments();
                    if (myEncryptionPtr->IsKeySupported())
                    {
                        const char* a_crypt_ref = myEncryptionPtr->GetCurCryptingReference() + (myEncryptionPtr->GetCurCryptingReference()[0] == myEncryptionPtr->GetCrypChar() ? 1 : 0);
                        a_pre_object->SetCryptingReference(a_crypt_ref);
                    }
                }
                if (a_crypted_data == NULL) StopRecording();
                ClearRecordBuffer();
            }
            if (!a_ok) displayMessage(MSG_ERROR, getMsg(11), a_iftype_str.c_str(), a_id);
            if (a_ok) preobj_lst.push_back(a_pre_object);
            if (a_pre_object)
                HCDI_ReleasePreObjectHandle(a_pre_object);
        }
        if (!a_descr_p)
        {
            displayCurrentLocation(MSG_ERROR);
            displayMessage(MSG_ERROR, getMsg(2), a_iftype_str.c_str());
        }
        else if (a_descr_p && !a_format_p)
        {
            MvFileFormat_e format_version = getFormatVersion();
            double a_version = (int)(format_version) * .1;
            displayCurrentLocation(MSG_ERROR);
            displayMessage(MSG_ERROR, getMsg(3), a_iftype_str.c_str(), a_version);
        }
    }
    else
    {
        displayMessage(MSG_ERROR, getMsg(11), a_iftype_str.c_str(), a_id);
    }
    //
// Freeing strings
    myfree(a_title_str);
    my_free(id_param_p);
}

IMECPreObject* HWCFGReader::readCurrentObjects(MECIModelFactory* model_p, char* header_card, const CUserNameTypeInfo* headerdata, int linecount) {
    // Header
    int   a_id = 0;
    char* a_title_str = NULL;
    string a_iftype_str, a_kftype_str;
    int   a_unit_id = 0;
    char* id_param_p = NULL;
    const IDescriptor* a_descr_p = NULL;
    const fileformat_t* a_format_p = NULL;
    char* id_unit_param_p = NULL;

    /* Needs to initialize myIsCrypted, myCryptType, myCommentState before calling readHeader: any new header*/
    myIsCrypted = false;
    myEncryptionPtr->SetCryptingMethod(IEncryption::CRYPT_UNKNOWN);
    myCommentState = true;
    obj_type_e etype = HCDI_OBJ_TYPE_NULL;
    bool  a_is_readable = readHeader(headerdata, a_iftype_str, a_kftype_str, (const void**)&a_descr_p, (const void**)&a_format_p, &a_id, &a_title_str,
                                     &a_unit_id, &id_param_p, &id_unit_param_p, etype, &header_card);
    IMECPreObject* preobj = NULL;
    // Reading cards
    if (a_is_readable && a_format_p) {
        //

        /* it is important to store the component and file index before */
        /* to read the card because those values can change (ex #enddata changes the index file)*/
        int subdeck_index = getCurrentSubdeckIndex();
        int a_current_include_file_index = GetCurrentFileIndex();
        int a_current_component_index = GetCurrentComponentIndex();

        // Creating pre-object
        preobj = HCDI_GetPreObjectHandle(a_kftype_str.c_str(), a_iftype_str != "" ? a_iftype_str.c_str() : a_kftype_str.c_str(), a_title_str, a_id, a_unit_id);
        preobj->Init((const PseudoDescriptor_t*)a_descr_p);
        preobj->SetEntityType(etype);
        if (header_card)
        {
            string str(header_card);
            preobj->SetHeaderLine(str);
        }

        preobj->SetSubdeckIndex(subdeck_index);
        preobj->SetFileIndex(subdeck_index < 0 ? MECSubdeck::curSubdeckIdx : subdeck_index); // during first parse depends on MECSubdeck::curSubdeckIdx
        preobj->SetComponentIndex(a_current_component_index);

        object_type_e a_kernel_otype = HCDI_GetHCObjectType(a_kftype_str);
        bool          a_is_cryptable = myInputInfosPtr->IsEntityCryptable(a_kernel_otype, getFormatVersion());
        if (a_is_cryptable) {

            if (!myEncryptionPtr->IsKeySupported())
            {
                bool a_continue = true;
                myDoRecord = true;
                StartRecording();
                pushPosition();
                bool a_isencrypted = false;
                while (a_continue)
                {
                    const char* a_buffer = ReadBuffer(true);
                    if (a_buffer == NULL)
                        break;

                    if (mySyntaxInfo->getHeaderSize())
                        a_buffer = killBlanksBegin(a_buffer);
                    if (isHeader(a_buffer))
                    {
                        const char* a_crypted_data = NULL;
                        a_crypted_data = StopRecording();
                        UnrecordLastComments();
                        if (a_isencrypted)
                        {
                            const char* a_copy_cryp_data = strdup(a_crypted_data);
                            if (a_crypted_data && *a_crypted_data != '\0')
                            {
                                eraseSubstringFromEnd(a_copy_cryp_data, a_buffer);
                                preobj->SetCryptedData(a_copy_cryp_data);
                                const char* a_crypt_ref = myEncryptionPtr->GetCurCryptingReference() + (myEncryptionPtr->GetCurCryptingReference()[0] == myEncryptionPtr->GetCrypChar() ? 1 : 0);
                                preobj->SetCryptingReference(a_crypt_ref);
                            }
                        }
                        UnreadBuffer();
                        myIsCrypted = false;
                        break;
                    }
                    //
                    if (myEncryptionPtr->IsStringEncrypted(a_buffer))
                    {
                        myEncryptionPtr->SetCryptingMethod(a_kernel_otype);
                        a_isencrypted = true;
                    }
                }
                if (!a_isencrypted)
                {
                    popPosition();
                    StopRecording();
                    UnrecordLastComments();
                    ClearRecordBuffer();
                }
                else
                {
                    ClearRecordBuffer();
                    // Freeing strings
                    if (a_title_str)
                        myfree(a_title_str);
                    if (id_param_p)
                        myfree(id_param_p);

                    return preobj;
                }
            }
            else
                myEncryptionPtr->SetCryptingMethod(a_kernel_otype);

            StartRecording();
        }
        // 
        bool a_ok = false;

        int a_card_ind0 = 0;
        a_ok = readObjectData((const PseudoFileFormat_t*)a_format_p, (IMECPreObject*)preobj, model_p, (const PseudoDescriptor_t*)a_descr_p, a_card_ind0);
        if (a_is_cryptable)
        {
            const char* a_crypted_data = NULL;
            if (a_ok && isCrypted())
            {
                a_crypted_data = StopRecording();
                preobj->SetCryptedData(a_crypted_data);
                UnrecordLastComments();

                const char* a_crypt_ref = myEncryptionPtr->GetCurCryptingReference() + (myEncryptionPtr->GetCurCryptingReference()[0] == myEncryptionPtr->GetCrypChar() ? 1 : 0);
                preobj->SetCryptingReference(a_crypt_ref);
            }
            if (a_crypted_data == NULL) StopRecording();
            ClearRecordBuffer();
        }
        if (!a_ok) displayMessage(MSG_ERROR, getMsg(11), a_iftype_str.c_str(), a_id);
    }
    if (!a_descr_p && a_iftype_str.size() != 0)
    {
        
        displayCurrentLocation(MSG_ERROR);
        displayMessage(MSG_ERROR, getMsg(2), a_iftype_str.c_str());
    }
    else if (a_descr_p && !a_format_p)
    {
        MvFileFormat_e format_version = getFormatVersion();
        double a_version = (int)(format_version) * .1;
        displayCurrentLocation(MSG_ERROR);
        displayMessage(MSG_ERROR, getMsg(3), a_iftype_str.c_str(), a_version);
    }
    // Freeing strings
    if (a_title_str)
        myfree(a_title_str);
    if (id_param_p)
        myfree(id_param_p);
    return preobj;
}


//++///////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//	This function is used to read object's data from the data file.
//Parameters:
//	MECIModelFactory *model_p: a pointer to the model object
//      const PseudoFileFormat_t *format_p: option's data format
//      MECPreObject             *object_p: Object used to read/write data
//      in the model.
//      const PseudoDescriptor_t *descr_p: descriptor pointer
//      int card_ind0: index indicate from where it begins to read.
//Return value:
//	bool: returns the reading status
//--///////////////////////////////////////////////////////////////////////////////
bool HWCFGReader::readObjectData(const PseudoFileFormat_t* format_p,  IMECPreObject* object_p,
                                 MECIModelFactory* model_p,           const PseudoDescriptor_t* descr_p,
                                 int            card_ind0,            int cur_index)
{//Begin -- HWCFGReader::readObjectData

    bool                a_ok = true;
    //ApplicationMode_e a_mode = (ApplicationMode_e)myInputInfosPtr->GetAppMode();
    //HAS_TITLE_CARD
    //CARD_MPP_EXISTS -> a_pre_object_p->AddIntValue("CARD_MPP_EXISTS", 1);
    //a_pre_object_p->AddIntValue("HAS_OPTION_PLANE", 1);
    //a_pre_object_p->AddIntValue("SPC", 1);
    //a_pre_object_p->AddIntValue("HAS_OPTION_INERTIA", 1);

    char* line = NULL;
    if (cur_index > 0)
    {
        line = readBuffer();
        if (!line || (cur_index > 0 && line && isHeader(line)))
        {
            unreadBuffer();
            HCDI_ReleasePreObjectHandle(object_p);
            return false;
        }
        unreadBuffer();
        if (myEncryptionPtr->IsStringEncrypted(line))
        {
            HCDI_ReleasePreObjectHandle(object_p);
            return false;
        }
        myDataReader->setSkipHeaderCardReadingState(true);
    }

    a_ok = myDataReader->readObjectData(format_p, object_p, model_p, descr_p, card_ind0);
    myDataReader->setSkipHeaderCardReadingState(false);
    return a_ok;
}//End -- HWCFGReader::readObjectData




/* --------- Parsing --------- */

/*default implementation is to read the include file in the same line.. incase of dyna it is in next line...*/
bool HWCFGReader::IsIncludedFile(const char* buffer, char** full_name_p, char** relative_name_p)  {
    bool a_is_include = myInputInfosPtr->IsIncludeHeader(buffer); 
  //
    if (a_is_include) {

        //get descriptor
        //create preobject based on format defined wiht common datanames + some solver 

        //store preobject in mv_modelfactor_t
        bool isfilepathonesameline = myInputInfosPtr->IsIncludeFilePathOnSameLine();
        if (isfilepathonesameline)
        {
            char* a_buffer = strdup(buffer + strlen(myInputInfosPtr->GetIncludeKeyword()));
            killBlanksNLEnd(a_buffer);
            string a_name = killBlanksBegin(a_buffer);
            boost::erase_all(a_name, "'");
            myfree(a_buffer);
            //

            string a_full_name;
            string a_relative_name;

            string a_main_name_file = myMainFile;
            string last_current_path = my_dir_get_current_path();
            //my_dir_set_current_path(my_get_tmp_dir());
            bool is_include_relative = my_is_file_path_relative(a_name);
            // case include in absolute path
            if (!is_include_relative)
            {
                a_full_name = a_name;
                a_relative_name = "";
            }
            else
            {
                a_full_name = my_file_full_name_modify_with_relative(a_main_name_file, a_name);
                a_relative_name = a_name;
            }

            *relative_name_p = strdup(a_relative_name.c_str());
            *full_name_p = strdup(a_full_name.c_str());
            my_dir_restore_current_path(last_current_path);
        }
        else
        {
            char* a_buffer = ReadBuffer();
            killBlanksNLEnd(a_buffer);
            string a_name = killBlanksBegin(a_buffer);

            //

            string a_full_name;
            string a_relative_name;

            string a_main_name_file = myMainFile;
            string last_current_path = my_dir_get_current_path();
            //my_dir_set_current_path(my_get_tmp_dir());
            bool is_include_relative = my_is_file_path_relative(a_name);
            // case include in absolute path
            if (!is_include_relative)
            {
                a_full_name = a_name;
                a_relative_name = "";
            }
            else
            {
                a_full_name = my_file_full_name_modify_with_relative(a_main_name_file, a_name);
                a_relative_name = a_name;
            }

            *relative_name_p = strdup(a_relative_name.c_str());
            *full_name_p = strdup(a_full_name.c_str());
            my_dir_restore_current_path(last_current_path);

        }
    }
    //
    return a_is_include;
}

// Pretreat the closing of a file
bool HWCFGReader::pretreatCloseFile()
{
    MECReadFile* a_cur_file_p = GetCurrentFilePtr();
    // in "READ HEADER MODE", find for "DUMMY_START" 
    if (a_cur_file_p && GetReadingStatus() == READ_STATUS_HEADER_POS)
    {

    }
    return false;
}


bool HWCFGReader::isComment(const char* buffer) const {
    return (mySyntaxInfo->isComment(buffer)) && getCommentState();
}

void HWCFGReader::manageComment(const char* buffer) {
    bool a_is_eof = false;
    int len = (int)strlen(myInputInfosPtr->GetIncludeEndKeyword());
    if(len > 0)
        a_is_eof = (mystrncasecmp(buffer, myInputInfosPtr->GetIncludeEndKeyword(), len) == 0); 
    //
    int key_comment_process_flag = myInputInfosPtr->getKeywordProcessCommentFlag();
    if (key_comment_process_flag == InputInfos::SOLVER_PROCESS_KEYWORD_COMMMENT_BEFORE)
          myCommentsHeader[InputInfos::SOLVER_PROCESS_KEYWORD_COMMMENT_BEFORE].push_back(buffer);

    if (a_is_eof) {
        // if #enddata is in a main file it must not be taken into account
        int a_nb_files = getNbCurrentFiles();
        if (pretreatCloseFile() == true)
            return;
        if (a_nb_files > 1)
            closeFile();
    }
}
bool HWCFGReader::IsComponent(const char* buffer) const
{
    //bool is_a_component = (strncmp(buffer, "*SUBMODEL", 9) == 0);
    const char *subdeck_key = myInputInfosPtr->GetSolverSubdeckKeyword();
    size_t len1 = strlen(subdeck_key);

    if (!len1)
        return false;

    bool is_a_component = (strncmp(buffer, subdeck_key, len1) == 0);

    if (!is_a_component)
    {
        const char* subdeck_keyend = myInputInfosPtr->GetSolverSubdeckKeywordEnd();
        size_t len2 = strlen(subdeck_keyend);
        if (len2 > 0)
        {
            is_a_component = (strncmp(buffer, subdeck_keyend, len2) == 0);
        }
    }
    return is_a_component;
}

MECComponent* HWCFGReader::ManageComponent(const char* buffer, bool& continue_flag, MECReadFile** new_file_p)
{
    static bool component_first = false;

    if (component_first)
    {
        continue_flag = false;
        return NULL;
    }
    char* a_buffer = ((char*)(buffer)); 
    MECComponent* a_new_component = NULL;
    const char* subdeck_key = myInputInfosPtr->GetSolverSubdeckKeyword();
    size_t len1 = strlen(subdeck_key);

    if (strncmp(buffer, subdeck_key, len1) == 0)
    {
        killBlanksNLEnd(a_buffer);
        // Splitting header
        string subdeck_str(subdeck_key);
        //subdeck_str.erase(std::remove(subdeck_str.begin(), subdeck_str.end(), mySyntaxInfo->getHeader()[0]), subdeck_str.end());;
        const char* hd = mySyntaxInfo->getHeader();
        if (hd)
        {
            size_t sz = strlen(hd);
            for (int i = 0; i < sz; i++)
                subdeck_str.erase(std::remove(subdeck_str.begin(), subdeck_str.end(), hd[i]), subdeck_str.end());
        }
        IDescriptor* a_descr_p = HCDI_GetDescriptorHandleFromKeyword(HCDI_OBJ_TYPE_INCLUDEFILES, subdeck_str.c_str());

        IMECPreObject* a_pre_object_p = NULL;
        const char* component_name = "";
        const char* a_title = "";
        if (a_descr_p)
        {
            UnreadBuffer();
            
            const fileformat_t* a_format_p = loc_get_file_format_ptr(a_descr_p, getFormatVersion());
            if (a_format_p)
            {
                component_first = true;
                string fulltype = string("/INCLUDEFILE/") + subdeck_str;
                a_pre_object_p = HCDI_GetPreObjectHandle(fulltype.c_str(), buffer, "", 0, 0);
                readObjectData(a_format_p, a_pre_object_p, myModelFactoryPtr, a_descr_p);
                a_pre_object_p->SetEntityType(HCDI_OBJ_TYPE_INCLUDEFILES);
                int subdeck_index = getCurrentSubdeckIndex();
                int a_current_include_file_index = GetCurrentFileIndex();
                int a_current_component_index = GetCurrentComponentIndex();
                a_pre_object_p->SetSubdeckIndex(subdeck_index);
                a_pre_object_p->SetFileIndex(subdeck_index);
                a_pre_object_p->SetComponentIndex(a_current_component_index);


                component_first = false;
                if (a_pre_object_p)
                {
                    //continue_flag = false;
                    string filemname_skey = GetAttribNameFromDrawable(a_descr_p, cdr::g_AttribFileName);
                    const char* component_name1 = a_pre_object_p->GetStringValue(filemname_skey.c_str()); 
                    if (component_name1) component_name = component_name1;
                    a_title = a_pre_object_p->GetTitle();
                }
            }
            else
            {
                bool testbool = false;
                assert((testbool && "Check for format. Higher version might have defined in SolverSubmodel/Include Transform"));
            }

        }
        else
        {
            //a_buffer = ReadBuffer();
            killBlanksNLEnd(a_buffer);
            const char* component_name = strdup(a_buffer);
        }


        if (a_title[0] == '\0' && component_name[0] == '\0')
        {
            int a_id = 1;
            if (a_pre_object_p)
                a_id = a_pre_object_p->GetId();
            a_title = "submodel";
        }
        string c_name = a_title[0] != '\0' ? a_title : component_name;
        bool found_comp = false;
        //for (int j = 0; j < MECSubdeck::mySubdeckVector.size(); j++)
        //{
        //    if (MECSubdeck::mySubdeckVector[j]->GetName() == c_name)
        //    {
        //        found_comp = true;
        //        break;
        //    }
        //}

        if (!found_comp)
        {
            MECSubdeck* subdeck = new MECSubdeck(a_title[0] != '\0' ? a_title : component_name, HCDI_OBJ_TYPE_SOLVERSUBMODELS, 0, 0, 0, a_pre_object_p, subdeck_str, mySyntaxInfo->getHeaderLst());
            a_new_component = new MECComponent(a_title[0] != '\0' ? a_title : component_name, 0, 0);

            PushComponent(a_new_component);
            int parentIndex = GetCurrentComponentIndex();
            a_new_component->SetParentIndex(parentIndex);
            int a_file_index = GetCurrentFileIndex();
            a_new_component->SetFileIndex(a_file_index);
            //
            string a_full_name;
            string a_relative_name;

            string a_main_name_file = myMainFile;
            string last_current_path = my_dir_get_current_path();
            //my_dir_set_current_path(my_get_tmp_dir());
            if (component_name && component_name[0] != '\0')
            {
                bool is_include_relative = my_is_file_path_relative(component_name);
                // case include in absolute path
                if (!is_include_relative)
                {
                    a_full_name = component_name;
                    a_relative_name = "";
                }
                else
                {
                    a_full_name = my_file_full_name_modify_with_relative(a_main_name_file, component_name);
                    a_relative_name = component_name;
                }
                my_dir_restore_current_path(last_current_path);

                int a_find_index = SearchFile(a_full_name.c_str());
                if (a_find_index == -1)
                {
                    MECReadFile* a_new_file_p = openFile(a_full_name.c_str());

                    if (a_new_file_p != NULL)
                    {
                        a_new_file_p->SetRelativeName(a_relative_name.c_str());
                        // MECSubdeck::mySubdeckVector.back()->SetFileRelativeName(a_relative_name);
                        *new_file_p = a_new_file_p;
                    }
                    else
                        *new_file_p = GetCurrentFilePtr();

                    (*new_file_p)->open();
                    a_file_index = GetCurrentFileIndex();
                    PushFileIndex(a_file_index);
                }
                else
                {
                    *new_file_p = GetFile(a_find_index);
                    if (!(*new_file_p)->isOpen())
                        (*new_file_p)->open();
                    PushFileIndex(a_find_index);
                }
                (*new_file_p)->SetVersion(myVersion);
                //PopComponent();
                //MECSubdeck::PopSubDeck();
                mycomponentstate = true;
                //
                // pop it ....????
                //a_new_component = PopComponent();
                //MECSubdeck::PopSubDeck();
            }
        }
    }
    else // //ENDSUB
    {
        a_new_component = PopComponent();
        MECSubdeck::PopSubDeck();
    }

    return a_new_component;
}


bool HWCFGReader::isHeader(const char* buffer, char** keyword_p) const
//++///////////////////////////////////////////////////////////////////////////
//
//Function Descriptions:
//
//	This function is used to check if it's a (valid) header.
//Parameters:
//
//	const char *buffer: one line read from the file [in]
//
//      char **keyword_p: keyword [out]
//
//Return value:
//
//      Bool: if it's a header
//
//--/////////////////////////////////////////////////////////////////////////
{


    return mySyntaxInfo->isHeader(buffer, keyword_p);

}

int HWCFGReader::splitHeader(char* header, char** header_fields) const {
    char* a_char_p = header;
    int   a_nb_fields = 0;
    //
    while (*a_char_p != '\0') {
        if (isHeader(a_char_p)) {
            *a_char_p = '\0';
            header_fields[a_nb_fields++] = (++a_char_p);
        }
        else {
            ++a_char_p;
        }
    }
    //
    return a_nb_fields;
}

int HWCFGReader::getNbFreeLines() {
    int a_nb_free_lines = 0;
    pushPosition();
    //

    bool a_do_record = myDoRecord;
    if (a_do_record) StopRecording();
    bool a_continue = true;
    while (a_continue) {
        char* a_card = ReadBuffer(false);
        //
        a_continue = (a_card != NULL && !isHeader(a_card));
        if (a_continue) ++a_nb_free_lines;
    }
    if (a_do_record) StartRecording();

    //
    popPosition();
    //
    return a_nb_free_lines;
}


bool HWCFGReader::isCrypted() {
    return myIsCrypted;
}

bool HWCFGReader::isEntityCrypted() {


    return myIsCrypted;
}

/* --------- Crypting --------- */
char* HWCFGReader::DecryptLine(char* iline) {
    const int* a_ikey = NULL;
    int aaikey = 0;
    bool newCryptingVersion = false;
    //
    if (!myEncryptionPtr->CheckCouldBeEncryptedLine(iline)) return MECIReadModelBase::DecryptLine(iline);
    //
    IEncryption::MyCryptingType_e a_crypt_type = myEncryptionPtr->GetCryptingMethod();
    if (a_crypt_type == IEncryption::CRYPT_UNKNOWN) return MECIReadModelBase::DecryptLine(iline);
    //
    if (!myEncryptionPtr->IsKeySupported())
    {  
        static char* emptystr = "";
        return MECIReadModelBase::DecryptLine(emptystr); 
    }
    
    const MECIModelFactory* a_model_p = GetModelFactoryPtr();

    char* aa_ref = myEncryptionPtr->GetKeyRefFromLine(iline);
    const char* a_cryp_key = a_model_p->GetCryptingKey(aa_ref);
    myIsCrypted = false;
    return myEncryptionPtr->GetDecryptLine(iline, a_cryp_key, myIsCrypted);
}



/* --------- Recording --------- */
void HWCFGReader::ClearRecordBuffer() {
    MECIReadModelBase::ClearRecordBuffer();
    myIsCrypted = false;
}


/* --------- Messages --------- */
const char* HWCFGReader::getMsg(int ind) const {
    return MV_get_msg_array(MSGT_READ_D00_5X)[ind];
}


/* --------- Static functions --------- */

static bool loc_is_alpha(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

static int loc_get_fmt_size(const string& fmt) {
    int a_size = atoi(fmt.substr(1, fmt.find_first_of(".sdilfeg") - 1).c_str());
    return a_size < 0 ? (-a_size) : a_size;
}


static int loc_get_cell_size(const ff_cell_t* cell_format_p) {
    int a_nb_chars = 0;
    //
    ff_cell_type_e  a_cell_type = CELL_UNKNOWN;
    MCDS_get_ff_cell_attributes(cell_format_p, CELL_TYPE, &a_cell_type, END_ARGS);
    //
    switch (a_cell_type) {
    case CELL_COMMENT:
    {
        MCDS_get_ff_cell_attributes(cell_format_p, CELL_SIZE, &a_nb_chars, END_ARGS);
    }
    break;
    case CELL_VALUE:
    case CELL_DIR_RADIO:
    case CELL_DIR_FLAGS:
    {
        const char* a_fmt = NULL;
        MCDS_get_ff_cell_attributes(cell_format_p, CELL_FORMAT, &a_fmt, END_ARGS);
        a_nb_chars = loc_get_fmt_size(a_fmt);
    }
    break;
    default:
        break;
    }
    //
    return a_nb_chars;
}

static const fileformat_t* loc_get_file_format_ptr(const IDescriptor* descr_p, MvFileFormat_e version) {

    const fileformat_t* a_format_p = descr_p->getFileFormatPtr(version);

    if (a_format_p == NULL)
    {
        a_format_p = descr_p->getLowerFileFormatPtr(version);
    }
    return a_format_p;
}

int HWCFGReader::scanInt(const char* cell, const char* format, int nb_chars, bool* a_ok_p) const
{
    return myDataReader->scanInt(cell, format, nb_chars, a_ok_p);
}

double HWCFGReader::scanDouble(const char* cell, const char* format, int nb_chars, bool* a_ok_p) const
{
    return myDataReader->scanDouble(cell, format, nb_chars, a_ok_p);
}

char* HWCFGReader::scanString(const char* cell, const char* format, int nb_chars, char* value, bool* a_ok_p) const
{
    myDataReader->scanString(cell, format, nb_chars, value, a_ok_p);
    return value;
}

void HWCFGReader::pushPosition()
{
    
    myUnReadLine        = myUnreadPositionPtr->GetLine();
    myUnReadLoc         = myUnreadPositionPtr->GetLocation();
    myUnReadFileIndex   = GetCurrentFileIndex();

    myIndCurrentFile = GetCurrentFileIndex();
    myCurrentLoc = GetCurrentLocation();
    myCurrentLine = getCurrentLine();
}

void HWCFGReader::popPosition()
{
    SetHierarchyFile(myIndCurrentFile);
    GetCurrentFilePtr()->SetCurrentLocation(myCurrentLoc);
    GetCurrentFilePtr()->SetCurrentLine(myCurrentLine);

    myUnreadPositionPtr->SetLine(myUnReadLine);
    myUnreadPositionPtr->SetLocation(myUnReadLoc);
    SetCurrentFileIndex(myUnReadFileIndex);

    myUnReadLine = 0;
    myUnReadLoc = 0;
    myUnReadFileIndex = 0;
}



int HWCFGReader::scanInt(const char* cell, const char* format, int nb_chars, string& param_name, bool* a_ok_p) const
{
    //Check if the cell is parameterized
    bool is_parameter_negated = false;
    bool isParameterCell = myDataReader->isParameterCell(cell, nb_chars, param_name, &is_parameter_negated);
    if (isParameterCell)
    {
        //Get the value from parameter
        int value = myModelFactoryPtr->GetIntParameter(param_name.c_str(), GetCurrentFileIndex());
        if (is_parameter_negated)
        {
            value = -1 * value;
        }
        return value;
    }
    else
    {
        return myDataReader->scanInt(cell, format, nb_chars, a_ok_p);
    }
}

double HWCFGReader::scanDouble(const char* cell, const char* format, int nb_chars, string& param_name, bool* a_ok_p) const
{
    //Check if the cell is parameterized
    bool is_parameter_negated = false;
    bool isParameterCell = myDataReader->isParameterCell(cell, nb_chars, param_name, &is_parameter_negated);
    if (isParameterCell)
    {
        //Get the value from parameter
        double value = myModelFactoryPtr->GetFloatParameter(param_name.c_str(), GetCurrentFileIndex());
        if (is_parameter_negated)
        {
            value = -1 * value;
        }
        return value;
    }
    else
    {
        return myDataReader->scanDouble(cell, format, nb_chars, a_ok_p);
    }
}
MvFileFormat_e HWCFGReader::getFileFormatVersion(int subdeck_index)
{
    MECReadFile* CurFile = GetCurrentFilePtr();
    if (!CurFile)
        return FF_UNKNOWN;
    return CurFile->GetVersion();
}

MvFileFormat_e HWCFGReader::getFormatVersion()
{
    int subdeck_index = getCurrentSubdeckIndex();
    MvFileFormat_e local_version = getFileFormatVersion(subdeck_index);
    if (local_version > 0)
        return local_version;

    return myVersion;
}

void HWCFGReader::readCurrentObjects(MECIModelFactory* model_p, vector<IMECPreObject*>& preobj_lst, char* header_card, const CUserNameTypeInfo* headerdata, int linecount)
{
    // Header
    string a_iftype_str, a_kftype_str;
    int id = 0, a_format_id = 0;
    const IDescriptor* a_descr_p = NULL;
    const fileformat_t* a_format_p = NULL;
    int   a_id = 0;
    char* a_title_str = NULL;
    int   a_unit_id = 0;
    char* id_param_p = NULL;
    char* id_unit_param_p = NULL;
    obj_type_e etype = HCDI_OBJ_TYPE_NULL;
    /* Needs to initialize myIsCrypted, myCryptType, myCommentState before calling readHeader: any new header*/
    myIsCrypted = false;
    myEncryptionPtr->SetCryptingMethod(IEncryption::CRYPT_UNKNOWN);
    myCommentState = true;
    char* a_header_card=NULL;
    
    bool  a_is_readable = readHeader(headerdata, a_iftype_str, a_kftype_str, (const void**)&a_descr_p, (const void**)&a_format_p, &a_id, &a_title_str, &a_unit_id, &id_param_p, &id_unit_param_p, etype, header_card==NULL? &a_header_card : &header_card);
    if (!a_is_readable)
    {
        if (!a_descr_p && a_iftype_str.size() != 0)
        {
            
            displayCurrentLocation(MSG_ERROR);
            displayMessage(MSG_ERROR, getMsg(2), a_iftype_str.c_str());
        }
        else if (a_descr_p && !a_format_p)
        {
            MvFileFormat_e format_version = getFormatVersion();
            double a_version = (int)(format_version) * .1;
            displayCurrentLocation(MSG_ERROR);
            displayMessage(MSG_ERROR, getMsg(3), a_iftype_str.c_str(), a_version);
        }

        return;
    }

    // Loop: read as many objects as there are before the next header card
    bool do_stop = false;
    int a_ent_count_indx = 0;
    do
    {
        IMECPreObject* preobj = NULL;
        object_type_e a_kernel_otype = HCDI_GetHCObjectType(a_kftype_str);

        bool a_ok = false;
        /* it is important to store the component and file index before */
        /* to read the card because those values can change (ex #enddata changes the index file)*/
        int subdeck_index = getCurrentSubdeckIndex();
        int a_current_include_file_index = GetCurrentFileIndex();
        int a_current_component_index = GetCurrentComponentIndex();

        // Creating pre-object
        preobj = HCDI_GetPreObjectHandle(a_kftype_str.c_str(), a_iftype_str != "" ?  a_iftype_str.c_str() : a_kftype_str.c_str(), a_title_str, a_id, a_unit_id);
        preobj->Init((const PseudoDescriptor_t*)a_descr_p);
        preobj->SetEntityType(etype);
        if (header_card || a_header_card)
        {
            string str(header_card==NULL? a_header_card : header_card);
            preobj->SetHeaderLine(str);
        }
        preobj->SetSubdeckIndex(subdeck_index);
        preobj->SetFileIndex(subdeck_index);
        preobj->SetComponentIndex(a_current_component_index);

        bool          a_is_cryptable = myInputInfosPtr->IsEntityCryptable(a_kernel_otype, getFormatVersion());
        if (a_is_cryptable) {

            if (!myEncryptionPtr->IsKeySupported())
            {
                bool a_continue = true;
                myDoRecord = true;
                StartRecording();
                pushPosition();
                bool a_isencrypted = false;
                while (a_continue)
                {
                    const char* a_buffer = ReadBuffer(true);
                    if (a_buffer == NULL)
                        break;

                    if (mySyntaxInfo->getHeaderSize())
                        a_buffer = killBlanksBegin(a_buffer);
                    if (isHeader(a_buffer))
                    {
                        const char* a_crypted_data = NULL;
                        a_crypted_data = StopRecording();
                        UnrecordLastComments();

                        if (a_isencrypted)
                        {
                            const char* a_copy_cryp_data = strdup(a_crypted_data);
                            if (a_crypted_data && *a_crypted_data != '\0')
                            {
                                eraseSubstringFromEnd(a_copy_cryp_data, a_buffer);
                                preobj->SetCryptedData(a_copy_cryp_data);
                                const char* a_crypt_ref = myEncryptionPtr->GetCurCryptingReference() + (myEncryptionPtr->GetCurCryptingReference()[0] == myEncryptionPtr->GetCrypChar() ? 1 : 0);
                                preobj->SetCryptingReference(a_crypt_ref);
                            }
                        }
                        UnreadBuffer();
                        myIsCrypted = false;
                        break;
                    }
                    //
                    if (myEncryptionPtr->IsStringEncrypted(a_buffer))
                    {
                        myEncryptionPtr->SetCryptingMethod(a_kernel_otype);
                        a_isencrypted = true;
                    }
                }
                if (!a_isencrypted)
                {
                    popPosition();
                    StopRecording();
                    UnrecordLastComments();
                    ClearRecordBuffer();
                }
                else
                {
                    ClearRecordBuffer();
                    // Freeing strings
                    if (a_title_str)
                        myfree(a_title_str);
                    if (id_param_p)
                        myfree(id_param_p);

                    preobj_lst.push_back(preobj);
                    return;
                }
            }
            else
                myEncryptionPtr->SetCryptingMethod(a_kernel_otype);

            StartRecording();
        }
        // 

        int a_card_ind0 = 0;

        a_ok = readObjectData((const PseudoFileFormat_t*)a_format_p, (IMECPreObject*)preobj, model_p, (const PseudoDescriptor_t*)a_descr_p, a_card_ind0, a_ent_count_indx++);
        
        if (a_is_cryptable)
        {
            const char* a_crypted_data = NULL;
            if (a_ok && isCrypted())
            {
                a_crypted_data = StopRecording();
                preobj->SetCryptedData(a_crypted_data);
                UnrecordLastComments();

                const char* a_crypt_ref = myEncryptionPtr->GetCurCryptingReference() + (myEncryptionPtr->GetCurCryptingReference()[0] == myEncryptionPtr->GetCrypChar() ? 1 : 0);
                preobj->SetCryptingReference(a_crypt_ref);
            }
            if (a_crypted_data == NULL) StopRecording();
            ClearRecordBuffer();
        }
        if (!a_ok)
        {
            displayMessage(MSG_ERROR, getMsg(11), a_iftype_str.c_str(), a_id);
            do_stop = true;
        }
        if (a_ok && NULL != preobj) {
            preobj->SetEntityType(etype);
            preobj_lst.push_back(preobj);
        }
        else
        {
            do_stop = true;
        }

    } while (!do_stop);
    // Freeing strings
    if (a_title_str)
        myfree(a_title_str);
    if (id_param_p)
        myfree(id_param_p);
    if (a_header_card)
        myfree(a_header_card);

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

/************************************************/
//Solver specific classes to override any specific behavior

HWCFGReaderLSDyna::HWCFGReaderLSDyna(const char* full_name, MvFileFormat_e version, ISyntaxInfos& syntaxSolverInfos, InputInfos& solverInf,
                                     const ReadFileFactorySP& factory_p) :
    HWCFGReader(full_name, version, syntaxSolverInfos, solverInf, factory_p)
{
}

HWCFGReaderLSDyna::HWCFGReaderLSDyna(MECReadFile* file, MvFileFormat_e version, ISyntaxInfos& syntaxSolverInfos, InputInfos& solverInf,
                                     const ReadFileFactorySP& factory_p) :
    HWCFGReader(file, version, syntaxSolverInfos, solverInf, factory_p)
{
}

void HWCFGReaderLSDyna::readCurrentObjects(MECIModelFactory* model_p, vector<IMECPreObject*>& preobj_lst,
                                           char* header_card, const CUserNameTypeInfo* headerdata, int linecount)
{
    // Header
    string a_iftype_str, a_kftype_str;
    int id = 0, a_format_id = 0;
    const IDescriptor* a_descr_p = NULL;
    const fileformat_t* a_format_p = NULL;
    int   a_id = 0;
    char* a_title_str = NULL;
    int   a_unit_id = 0;
    char* id_param_p = NULL;
    char* id_unit_param_p = NULL;
    obj_type_e etype = HCDI_OBJ_TYPE_NULL;
    /* Needs to initialize myIsCrypted, myCryptType, myCommentState before calling readHeader: any new header*/
    myIsCrypted = false;
    myEncryptionPtr->SetCryptingMethod(IEncryption::CRYPT_UNKNOWN);
    myCommentState = true;
    char* a_header_card = NULL;

    bool  a_is_readable = readHeader(
        headerdata, a_iftype_str, a_kftype_str, (const void**)&a_descr_p, (const void**)&a_format_p,
        &a_id, &a_title_str, &a_unit_id, &id_param_p, &id_unit_param_p, etype,
        header_card == NULL ? &a_header_card : &header_card);
    if (!a_is_readable)
    {
        if (!a_descr_p && a_iftype_str.size() != 0)
        {
            
            displayCurrentLocation(MSG_ERROR);
            displayMessage(MSG_ERROR, getMsg(2), a_iftype_str.c_str());
        }
        else if (a_descr_p && !a_format_p)
        {
            MvFileFormat_e format_version = getFormatVersion();
            double a_version = (int)(format_version) * .1;
            displayCurrentLocation(MSG_ERROR);
            displayMessage(MSG_ERROR, getMsg(3), a_iftype_str.c_str(), a_version);
        }

        return;
    }

    // Loop: read as many objects as there are before the next header card
    bool do_stop = false;
    int a_ent_count_indx = 0;
    do
    {
        IMECPreObject* preobj = NULL;
        object_type_e a_kernel_otype = HCDI_GetHCObjectType(a_kftype_str);

        bool a_ok = false;
        /* it is important to store the component and file index before */
        /* to read the card because those values can change (ex #enddata changes the index file)*/
        int subdeck_index = getCurrentSubdeckIndex();
        int a_current_include_file_index = GetCurrentFileIndex();
        int a_current_component_index = GetCurrentComponentIndex();

        // Creating pre-object
        preobj = HCDI_GetPreObjectHandle(a_kftype_str.c_str(), a_iftype_str != "" ? a_iftype_str.c_str() : a_kftype_str.c_str(), a_title_str, a_id, a_unit_id);
        preobj->Init((const PseudoDescriptor_t*)a_descr_p);
        preobj->SetEntityType(etype);


        if (header_card || a_header_card)
        {
            string str(header_card == NULL ? a_header_card : header_card);
            preobj->SetHeaderLine(str);
        }
        preobj->SetSubdeckIndex(subdeck_index);
        preobj->SetFileIndex(subdeck_index < 0 ? MECSubdeck::curSubdeckIdx : subdeck_index);
        preobj->SetComponentIndex(a_current_component_index);

        bool          a_is_cryptable = myInputInfosPtr->IsEntityCryptable(a_kernel_otype, getFormatVersion());
        if (a_is_cryptable) {

            if (!myEncryptionPtr->IsKeySupported())
            {
                bool a_continue = true;
                myDoRecord = true;
                StartRecording();
                pushPosition();
                bool a_isencrypted = false;
                while (a_continue)
                {
                    const char* a_buffer = ReadBuffer(true);
                    if (a_buffer == NULL)
                        break;

                    if (mySyntaxInfo->getHeaderSize())
                        a_buffer = killBlanksBegin(a_buffer);
                    if (isHeader(a_buffer))
                    {
                        const char* a_crypted_data = NULL;
                        a_crypted_data = StopRecording();
                        UnrecordLastComments();

                        if (a_isencrypted)
                        {
                            const char* a_copy_cryp_data = strdup(a_crypted_data);
                            if (a_crypted_data && *a_crypted_data != '\0')
                            {
                                eraseSubstringFromEnd(a_copy_cryp_data, a_buffer);
                                preobj->SetCryptedData(a_copy_cryp_data);
                                const char* a_crypt_ref = myEncryptionPtr->GetCurCryptingReference() + (myEncryptionPtr->GetCurCryptingReference()[0] == myEncryptionPtr->GetCrypChar() ? 1 : 0);
                                preobj->SetCryptingReference(a_crypt_ref);
                            }
                        }
                        UnreadBuffer();
                        myIsCrypted = false;
                        break;
                    }
                    //
                    if (myEncryptionPtr->IsStringEncrypted(a_buffer))
                    {
                        myEncryptionPtr->SetCryptingMethod(a_kernel_otype);
                        a_isencrypted = true;
                    }
                }
                if (!a_isencrypted)
                {
                    popPosition();
                    StopRecording();
                    UnrecordLastComments();
                    ClearRecordBuffer();
                }
                else
                {
                    ClearRecordBuffer();
                    // Freeing strings
                    if (a_title_str)
                        myfree(a_title_str);
                    if (id_param_p)
                        myfree(id_param_p);

                    preobj_lst.push_back(preobj);
                    return;
                }
            }
            else
                myEncryptionPtr->SetCryptingMethod(a_kernel_otype);

            StartRecording();
        }
        // 

        int a_card_ind0 = 0;

        char* line = NULL;
        if (a_ent_count_indx > 0)
        {
            line = readBuffer();
            if (!line || (a_ent_count_indx > 0 && line && isHeader(line)))
            {
                unreadBuffer();
                HCDI_ReleasePreObjectHandle(preobj);
                if (a_title_str)
                    myfree(a_title_str);
                if (id_param_p)
                    myfree(id_param_p);
                if (a_header_card)
                    myfree(a_header_card);

                StopRecording();
                ClearRecordBuffer();
                return;
            }
            unreadBuffer();
            if (myEncryptionPtr->IsStringEncrypted(line))
            {
                HCDI_ReleasePreObjectHandle(preobj);

                if (a_title_str)
                    myfree(a_title_str);
                if (id_param_p)
                    myfree(id_param_p);
                if (a_header_card)
                    myfree(a_header_card);

                StopRecording();
                ClearRecordBuffer();
                return;
            }
            myDataReader->setSkipHeaderCardReadingState(true);
            if (a_title_str)
            {
                myfree(a_title_str);
                a_title_str = nullptr;
            }
            if (id_param_p)
            {
                myfree(id_param_p);
                id_param_p = nullptr;
            }
            char *header_p = const_cast<char*>(a_iftype_str.c_str());

            a_is_readable = readHeader(headerdata, a_iftype_str, a_kftype_str, (const void**)&a_descr_p, (const void**)&a_format_p, &a_id, &a_title_str, &a_unit_id, &id_param_p, &id_unit_param_p, etype, &header_p);
            preobj->SetId(a_id);
            preobj->SetTitle(a_title_str);
            preobj->SetUnitId(a_unit_id);

            if (id_param_p && !preobj->IsParameterId())
            {
                preobj->SetParameterIdName(id_param_p);
            }
            if (id_unit_param_p)
                preobj->SetParameterName(id_unit_param_p, "UNIT"); // need to defined as DRAWABLES 

        }

        a_ent_count_indx++;
        myDataReader->setCurKeyTotalLineCount(linecount);
        a_ok = readObjectData((const PseudoFileFormat_t*)a_format_p, (IMECPreObject*)preobj, model_p, (const PseudoDescriptor_t*)a_descr_p, a_card_ind0);
        myDataReader->setSkipHeaderCardReadingState(false);
        myDataReader->setCurKeyTotalLineCount(0);
        if (a_is_cryptable)
        {
            const char* a_crypted_data = NULL;
            if (a_ok && isCrypted())
            {
                a_crypted_data = StopRecording();
                preobj->SetCryptedData(a_crypted_data);
                UnrecordLastComments();

                const char* a_crypt_ref = myEncryptionPtr->GetCurCryptingReference() + (myEncryptionPtr->GetCurCryptingReference()[0] == myEncryptionPtr->GetCrypChar() ? 1 : 0);
                preobj->SetCryptingReference(a_crypt_ref);
            }
            if (a_crypted_data == NULL) StopRecording();
            ClearRecordBuffer();
        }
        if (!a_ok)
        {
            displayMessage(MSG_ERROR, getMsg(11), a_iftype_str.c_str(), a_id);
            do_stop = true;
        }
        if (a_ok && NULL != preobj) {
            static string a_title_str = "_TITLE";
            static string a_LSD_TitleOpt_str = "LSD_TitleOpt";
            if (a_header_card != NULL)
            {
                if (strstr(a_header_card, a_title_str.c_str()))
                    preobj->AddIntValue(a_LSD_TitleOpt_str.c_str(), 1);
            }
            else
            {
                /*incase it is already filled ex: for some cases for  _ID LSD_TitleOpt is used */
                int attrib_ind = preobj->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_LSD_TitleOpt_str);
                if (attrib_ind < 0)
                    preobj->AddIntValue(a_LSD_TitleOpt_str.c_str(), 0);
            }
            preobj->SetEntityType(etype);
            preobj_lst.push_back(preobj);
        }
        else
        {
            do_stop = true;
        }

    } while (!do_stop);
    // Freeing strings
    if (a_title_str)
        myfree(a_title_str);
    if (id_param_p)
        myfree(id_param_p);
    if (a_header_card)
        myfree(a_header_card);

}


/************************************************/
