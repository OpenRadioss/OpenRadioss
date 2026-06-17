//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
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
#include <regex>
#include <string_view>
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
#include "hw_cfg_reader_message.h"
#include "hcioi_utils.h"
#include <boost/algorithm/string/classification.hpp> // Include boost::for is_any_of
#include <boost/algorithm/string/split.hpp> // Include for boost::split
#include <boost/algorithm/string.hpp>
#include <fstream>

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

#define ALLOC_SIZE  100000

// Initialize static members
bool PerformanceConfig::timing_enabled_ = false;
std::set<std::string> PerformanceConfig::enabled_operations_;

static bool                loc_is_alpha(char c);
static int                 loc_get_fmt_size(const string& fmt);

static int                 loc_get_cell_size(const ff_cell_t* cell_format_p);
static const fileformat_t* loc_get_file_format_ptr(const IDescriptor* descr_p, MvFileFormat_e version);

static bool isDigitString(const char* test_str);
static bool file_exists(const std::string& path);

enum class CharPresence {
    NONE,
    HAS_FREEFORMATONLY,
    HAS_PARAMETERONLY,
    HAS_FREEFORMAT_PARAMETER_BOTH
};


inline void fastTrim(char* s) {
    int len = (int)strlen(s);
    while (len > 0 && s[len - 1] == ' ') s[--len] = '\0';
}


static int readAndGetValidCardListCard( MECIDataReader*             dataReader,
                                        ff_card_t*                  card_p,
                                        IMECPreObject*              pre_object,
                                        const PseudoDescriptor_t*   descr_p,
                                        MECIModelFactory*           model_p,
                                        string                      header,
                                        vector< ff_card_t*>&        cardlst);


bool file_exists(const std::string& path) 
{
    std::ifstream f(path.c_str());
    return f.good();
}
 
inline CharPresence checkForFreeAndParameterChars(const char* str) // need to pass freeformat and parameter externally
{  
    if (!str) return CharPresence::NONE;

    bool hasComma = false;
    bool hasAmp = false;

    while (*str) {
        if (*str == ',') hasComma = true;
        else if (*str == '&') hasAmp = true;

        if (hasComma && hasAmp) return CharPresence::HAS_FREEFORMAT_PARAMETER_BOTH;

        ++str;
    }

    if (hasComma) return CharPresence::HAS_FREEFORMATONLY;
    if (hasAmp) return CharPresence::HAS_PARAMETERONLY;
    return CharPresence::NONE;
}
// Trim trailing spaces
static std::string trimRight(const std::string& str) {
    size_t end = str.find_last_not_of(" \t\r\n");
    return (end == std::string::npos) ? "" : str.substr(0, end + 1);
}

// Compatible endsWith
static bool endsWith(const std::string& str, const std::string& suffix) {
    if (suffix.size() > str.size()) return false;
    return std::equal(suffix.rbegin(), suffix.rend(), str.rbegin());
}

static bool hasContinuation(const std::string& currentLine, const std::string& nextLine) {
    std::string trimmed = trimRight(currentLine);

    if (endsWith(trimmed, " +") || endsWith(trimmed, "...") || endsWith(trimmed, "&")) {
        return true;
    }

    if (!nextLine.empty() && nextLine[0] == '+') {
        return true;
    }

    return false;
}

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


static std::string clean_format_string(const std::string& input) {
    // Match all format specifiers starting with % and strip flags/width/precision
    // Capture length modifiers (like l, ll) and type specifiers (like d, f, lg, etc.)
    std::regex format_regex(R"(%[-+0# ]*\d*(\.\d+)?(ll|l|L)?([a-zA-Z]+))");
    std::string cleaned = std::regex_replace(input, format_regex, "%$2$3");

    // Insert space before each '%', except the first one
    std::string result;
    bool first = true;
    for (size_t i = 0; i < cleaned.size(); ++i) {
        if (cleaned[i] == '%') {
            if (!first) result += ' ';
            first = false;
        }
        result += cleaned[i];
    }

    return result;
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
    myCommentsHeader.resize(InputInfos::SOLVER_PROCESS_KEYWORD_COMMENT_BEFORE_AFTER+1);
    myEncryptionPtr = newEncryption();


    // Initialize performance timing from environment
    initializePerformanceTimingFromEnvironment();
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
    myCommentsHeader.resize(InputInfos::SOLVER_PROCESS_KEYWORD_COMMENT_BEFORE_AFTER+1);
    myEncryptionPtr = newEncryption();


    // Initialize performance timing from environment
    initializePerformanceTimingFromEnvironment();
}

HWCFGReader::~HWCFGReader() {
    //
    delete myDataReader;
    if(myEncryptionPtr)
       delete myEncryptionPtr;
    if(m_owningMessageList && m_pMessageList) delete m_pMessageList;
}



// Performance timing methods
void HWCFGReader::enablePerformanceTiming(bool enable) {
    PerformanceConfig::EnableTiming(enable);
}

void HWCFGReader::enableTimingForOperation(const std::string& operation) {
    PerformanceConfig::EnableOperation(operation);
}

void HWCFGReader::initializePerformanceTimingFromEnvironment() {
    // Enable timing based on environment variable - check the value, not just existence
    const char* timing_env = getenv("HWCFG_ENABLE_PERFORMANCE_TIMING");
    if (timing_env) {
        // Only enable if the value is not "0" or "false" (case insensitive)
        std::string timing_value(timing_env);
        std::transform(timing_value.begin(), timing_value.end(), timing_value.begin(), ::tolower);

        if (timing_value != "0" && timing_value != "false" && timing_value != "") {
            PerformanceConfig::EnableTiming(true);
            printf("[DEBUG] Performance timing enabled globally (value: %s)\n", timing_env);
        }
        else
            return;
    }
    else {
        return;
    }

    // Enable specific operations
    const char* operations = getenv("HWCFG_TIMING_OPERATIONS");
    if (operations && strlen(operations) > 0) {  // Check for valid string
        printf("[DEBUG] Found HWCFG_TIMING_OPERATIONS: %s\n", operations);
        std::string ops(operations);
        std::istringstream ss(ops);
        std::string operation;
        while (std::getline(ss, operation, ',')) {
            // Trim whitespace
            operation.erase(0, operation.find_first_not_of(" \t"));
            operation.erase(operation.find_last_not_of(" \t") + 1);

            if (!operation.empty()) {  // Only add non-empty operations
                printf("[DEBUG] Enabling operation: '%s'\n", operation.c_str());
                PerformanceConfig::EnableOperation(operation);
            }
        }
    }
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
    ApplicationMode_e a_mode = (ApplicationMode_e)myInputInfosPtr->GetAppMode(); 
    switch (a_mode)
    {
    case HCDI_SOLVER_RADIOSS:
        encryp = new EncryptionRadioss();
        encryp->InitializeCrypting();
        break;
    case HCDI_SOLVER_LSDYNA:
        encryp = new EncryptionLSDyna();
        encryp->InitializeCrypting();
        break;
    default:
    encryp = new IEncryption();
        break;
    }
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
    HWCFG_PERF_TIMER("HWCFGReader::readHeaderPositions");

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
    bool first_begin_found = false;

    //no begin block to start reading.
    if (begin_str == "") 
        first_begin_found = true;

    while (a_continue)
    {
        const char* a_buffer = ReadBuffer(true); 

        if (!a_buffer)
            break;

        if(a_buffer != NULL && mySyntaxInfo->getHeaderSize())
            a_buffer = killBlanksBegin(a_buffer);  // remove leading spaces incase header (*,/,... ) are defined
        //
        if (a_buffer != NULL && !isHeader(a_buffer) || a_buffer[0]=='\0')
        {
            a_cur_nb_lines++;

            if (!first_begin_found && strstr(begin_str.c_str(), a_buffer))
                first_begin_found = true;

            continue;
        }
        if (!first_begin_found && strstr(begin_str.c_str(), a_buffer))
            first_begin_found = true;
        char* header_keyword = strdup(a_buffer);
        if (a_buffer != NULL)
        {
            int a_current_include_file_index_before_header = GetCurrentFileIndex();
            int a_cursubindx1 = MECSubdeck::curSubdeckIdx;
            bool  a_is_header = isHeader(a_buffer);  //isHeader(a_buffer, &a_keyword);
            //
            if (a_is_header)
            {
                myCurrentHeader = a_buffer;

                bool is_endofkey = myInputInfosPtr->IsEofKeyword(a_buffer);

                obj_type_e etype = HCDI_OBJ_TYPE_NULL;

                const char* a_buffer_d = a_buffer;
                
                if (mySyntaxInfo->GetHeaderFromStartFlag())
                {
                    a_buffer_d = mySyntaxInfo->getNormalisedHeader(a_buffer);// +mySyntaxInfo->getHeaderSize();
                }
                a_buffer_d = killBlanksBegin(a_buffer_d);
                string a_header(a_buffer_d);

                if (mySyntaxInfo->GetHeaderFromStartFlag())
                {
                    std::size_t pos;
                    //remove all after ',' separater 
                    a_header.erase((pos = a_header.find(',')) != std::string::npos ? pos : a_header.size());
                }

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
                    if (etype == HCDI_OBJ_TYPE_INCLUDES)
                    {
                        UnreadBuffer();
                        int r_status = GetReadingStatus();
                        SetReadingStatus(READ_STATUS_SINGLE_KEYWORD);
                        ManageReadKeyWord(model_p, HCDI_get_entitystringtype(HCDI_OBJ_TYPE_INCLUDES).c_str(), p_type_info);
                        SetReadingStatus(r_status);
                        //readObjects(model_p, HCDI_get_entitystringtype(HCDI_OBJ_TYPE_PARAMETERS).c_str(), HCDI_get_entitystringtype(HCDI_OBJ_TYPE_PARAMETERS).c_str()); 
                        clearHeaderComments();
                        model_p->PreTreatObject("INCLUDE");
                        //string folderpath_skey = GetAttribNameFromDrawable(p_type_info->pdescrp, "_FOLDERPATH_RELATIVE");
                        //const char* component_name1 = a_pre_object_p->GetStringValue(folderpath_skey.c_str());

                        //myFolderPathRelative.push_back(folderpath_skey);
                        continue;
                    }
                    if (etype == HCDI_OBJ_TYPE_CARDS)
                    {
                        bool begin_key_found = false;
                        const char* begin_proc_str = NULL;
                        begin_proc_str = mySyntaxInfo->getNormalisedHeader(begin_str.c_str());
                        if (strncmp(a_buffer_d, begin_proc_str, (int)strlen(begin_proc_str)) == 0)
                        {
                            begin_key_found = true;
                            first_begin_found = true;
                        }
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
                else if (is_endofkey)
                {
                    if (a_current_include_file_index == 0)//main file
                        a_continue = false;
                    else
                    {
                        MECReadFile* a_cur_file_p = GetCurrentFilePtr();
                        if (a_cur_file_p)
                            a_cur_file_p->seek(0, SEEK_END);
                    }
                    continue;
                }


                if (!first_begin_found)
                    continue;


                //can get this information fro solverinfo whether to store or not..
                int key_comment_process_flag = myInputInfosPtr->getKeywordProcessCommentFlag();
                if (key_comment_process_flag == InputInfos::SOLVER_PROCESS_KEYWORD_COMMMENT_AFTER)
                {
                    pushPosition(); // reason is that HEADER card is not set correctly if next line is called (with comments) then unreadbuffer will lose prev position
                    bool iscomment = true;
                    while (iscomment)
                    {
                        char* buffer = ReadBuffer(false, -1, false);
                        if (!buffer)
                        {
                            break;
                        }
                        else
                        {
                            iscomment = isComment(buffer);
                            if (iscomment)
                            {
                                myCommentsHeader[InputInfos::SOLVER_PROCESS_KEYWORD_COMMMENT_AFTER].push_back(buffer);
                            }
                        }
                    }
                    popPosition();
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
                if (!p_type_info && !is_endofkey)
                {
                    displayCurrentLocation(MSG_ERROR);
                    displayMessage(MSG_ERROR, getMsg(52), a_header.c_str());
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


                    if (!readKeyword(p_type_info, model_p, header_keyword))
                        continue;

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
                    //
                    postTreatLineCount(p_type_info, a_header, &a_line_counts);
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
                displayMessage(MSG_ERROR, getMsg(53));
            }
        }
    }

    model_p->SortPreObjectsByName("PARAMETER");
    model_p->EvaluateExpressionParameters(this);

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

int readAndGetValidCardListCardIfCard(MECIDataReader*                       dataReader,
                                      ff_card_t*                            card_p,
                                      IMECPreObject                        *pre_object,
                                      const PseudoDescriptor_t*             descr_p,
                                      MECIModelFactory*                     model_p,
                                      string                                header,
                                      vector< ff_card_t*>                  &cardlst)
{
    const ff_card_t* a_card_p = (const ff_card_t*)card_p;
    //
    int  a_nb_ccls = 0;
    bool a_checked = false;
    MCDS_get_ff_card_attributes(a_card_p, CARD_NB_COND_CARD_LISTS, &a_nb_ccls, END_ARGS);
    for (int i = 0; !a_checked && i < a_nb_ccls; ++i) {
        ff_condcardlist_t* a_ccl_p = NULL;
        expression_t* a_expr_p = NULL;
        MCDS_get_ff_card_tab(a_card_p, CARD_COND_CARD_LIST, i, &a_ccl_p);
        MCDS_get_ff_condcardlist_expression(a_ccl_p, &a_expr_p);
        a_checked = (a_expr_p == NULL);
        //
        if (!a_checked) {
            MvExpression_t a_expr(a_expr_p, false);
            a_checked = pre_object->EvaluateExpression((PseudoExpression_t*)(&a_expr), descr_p);
        }
        //
        if (a_checked) {
            int a_nb_sub_cards = 0;
            MCDS_get_ff_condcardlist_nb_cards(a_ccl_p, &a_nb_sub_cards);
            for (int j = 0; j < a_nb_sub_cards; ++j) {
                ff_card_t* a_sub_card_p = NULL;
                MCDS_get_ff_condcardlist_card(a_ccl_p, j, &a_sub_card_p);
                int ret = readAndGetValidCardListCard(dataReader,  a_sub_card_p, pre_object, descr_p, model_p, header, cardlst);
                if (ret == -1)
                    return ret;
            }
        }
    }
    return 0;
}

int readAndGetValidCardListCard(MECIDataReader*             dataReader,
                                ff_card_t*                  card_p,
                                IMECPreObject*              pre_object,
                                const PseudoDescriptor_t*   descr_p,
                                MECIModelFactory*           model_p,
                                string                      header,
                                vector< ff_card_t*>&        cardlst)
{
    ff_card_type_e a_card_type = CARD_UNKNOWN;
    MCDS_get_ff_card_attributes(card_p, CARD_TYPE, &a_card_type, END_ARGS);

    if (a_card_type == CARD_CARD_LIST)
    {
        int  a_loc_nb_cards = 0;
        MCDS_get_ff_card_attributes(card_p, CARD_NB_CARDS, &a_loc_nb_cards, END_ARGS);
        //
        for (int k = 0; k < a_loc_nb_cards; k++)
        {
            ff_card_t* a_loc_card_format_p;
            MCDS_get_ff_card_tab(card_p, CARD_CARD, k, (void*)(&a_loc_card_format_p));
            MCDS_get_ff_card_attributes(a_loc_card_format_p, CARD_TYPE, &a_card_type, END_ARGS);

            if (a_card_type == CARD_IF)
            {
                return -1;
            }
            else if (a_card_type == CARD_SINGLE)
            {
                cardlst.push_back(a_loc_card_format_p);
            }
            else
                return -1;
        }
        return 0;
    }
    else if (a_card_type == CARD_IF)
    {
        int ret = readAndGetValidCardListCardIfCard(dataReader, card_p, pre_object, descr_p, model_p, header, cardlst);
        if (ret == -1)
            return ret;
    }
    else
    {
        //if (a_card_type == CARD_HEADER)
        //    pre_object->SetHeaderLine(header);

        bool a_do_continue = true;
        dataReader->readNextCard(card_p, pre_object, (void*)model_p, descr_p, -1, &a_do_continue);
        if (!a_do_continue)
            return -1;
    }

    return 0;
}


int HWCFGReader::readKeyword(const CUserNameTypeInfo* p_type_info, MECIModelFactory* model_p, const char* header)
{
    HWCFG_PERF_TIMER("HWCFGReader::readKeyword");
    
    if (!p_type_info || !header || !(p_type_info->obj_type == HCDI_OBJ_TYPE_NODES || p_type_info->obj_type == HCDI_OBJ_TYPE_ELEMS
       /* || p_type_info->obj_type == HCDI_OBJ_TYPE_CURVES*/ || p_type_info->obj_type == HCDI_OBJ_TYPE_SOLVERMASSES))
        return -1;

    // ===== PHASE 1: SETUP AND METADATA EXTRACTION =====
    // Direct destination pointers eliminate unordered_map hash lookups in the hot loop.
    struct FieldMeta {
        int              ikey;  
        string           skey; 
        value_type_e     type;
        int              width_base;   // base width based on short format
        int              width_cur;    // current width based on line format (short or long)
        const ff_cell_t* cell_p;
        // Direct destination pointers bound used per-value in  loop
        std::vector<int>* dst_int = nullptr;
        std::vector<double>* dst_double = nullptr;
        std::vector<int>* dst_obj_id = nullptr;
        std::vector<std::string>* dst_str = nullptr;
    };


    ApplicationMode_e a_mode = (ApplicationMode_e)myInputInfosPtr->GetAppMode();
    std::vector < std::vector<FieldMeta> >  metaList; 
    const IDescriptor* a_descr_p = p_type_info->pdescrp;
    if (!a_descr_p)
        return -1;

    string otype_st = HCDI_get_entitystringtype(p_type_info->obj_type);
    string fulltype = "/" + otype_st + "/" + p_type_info->myusername;

    int header_size = mySyntaxInfo->getHeaderKeywordCellLength();
    string input_ftype_str;
    if (header_size > 0)
    {
        const string str = string(header);
        input_ftype_str = (str.size() <= header_size) ? str : str.substr(0, header_size);
    }
    else
    {
        input_ftype_str = header;
    }

    // Find the position of the first '/' followed by only digits until the end or next '/'
    size_t last = input_ftype_str.size();
    while (last > 0) {
        size_t slash_pos = input_ftype_str.rfind('/', last - 1);
        if (slash_pos == std::string::npos)
            break;
        // Check if everything after slash_pos+1 is digits (until next '/')
        size_t start = slash_pos + 1;
        size_t end = input_ftype_str.find('/', start);
        if (end == std::string::npos) end = input_ftype_str.size();
        std::string segment = input_ftype_str.substr(start, end - start);
        if (!segment.empty() && std::all_of(segment.begin(), segment.end(), ::isdigit)) {
            // Remove from slash_pos to end
            input_ftype_str.erase(slash_pos);
            last = slash_pos;
        }
        else {
            break;
        }
    }


    IMECPreObject* a_pre_object = HCDI_GetPreObjectHandle(fulltype.c_str(), input_ftype_str.c_str(), "", 0, 0);
    a_pre_object->SetFileIndex(MECSubdeck::curSubdeckIdx);

    const fileformat_t* a_format_p = loc_get_file_format_ptr(a_descr_p, getFormatVersion());

    if(!a_format_p)
        a_format_p = loc_get_file_format_ptr(a_descr_p, myVersion);

    if (!a_format_p)
        return -1;

    // ===== PHASE 2: STORAGE INITIALIZATION =====
    std::unordered_map<int, vector<double>>        map_ikey_vec_d;
    std::unordered_map<int, vector<int>>           map_ikey_vec_i;
    std::unordered_map<int, vector<unsigned int>>  map_ikey_vec_ui;
    std::unordered_map<int, vector<int>>           map_ikey_vec_obj_id;
    std::unordered_map<int, vector<string>>        map_ikey_vec_s;

    int a_nb_cards = 0;
    ff_card_type_e a_card_type = CARD_UNKNOWN;
    bool                a_ok = true;

    MCDS_get_fileformat_nb_cards(a_format_p, &a_nb_cards);


    int file_index = GetCurrentFileIndex();
    int subdeck_index = getCurrentSubdeckIndex();
    MECReadFile* a_cur_file_p = GetCurrentFilePtr();
    _HC_LONG aloc = a_cur_file_p->GetCurrentLocation();
    _HC_LONG acurline = a_cur_file_p->GetCurrentLine();

    vector<ff_card_t*> cardlst;
    if (a_nb_cards <= 0)
        MCDS_get_fileformat_nb_cards(a_format_p, &a_nb_cards);

    UnreadBuffer(); 

    // ===== PHASE 3: VALIDATE CARD LIST (CHECK FOR CARD_IF) =====
    for (int i = 0; i < a_nb_cards; ++i)
    {
        ff_card_t* a_card_format_p;
        ff_card_t* a_next_card_format_p = NULL;
        MCDS_get_fileformat_card(a_format_p, i, &a_card_format_p);
        if (readAndGetValidCardListCard(myDataReader, a_card_format_p, a_pre_object, a_descr_p,
            model_p, string(header), cardlst))
        {
            HCDI_ReleasePreObjectHandle(a_pre_object);
            a_cur_file_p->SetCurrentLocation(aloc);
            a_cur_file_p->SetCurrentLine(acurline);
            SetCurrentFileIndex(file_index);
            setCurrentSubdeckIndex(subdeck_index);
            return -1;
        }
    }

    if (!cardlst.size())
        return -1;

    mySyntaxInfo->resetLineFormat();
    io_types::format_type_e  fmt_type_base = mySyntaxInfo->getFormatType();

    //bool is_longformat = false;

    int ikey_att_arr = 0;
    metaList.resize(cardlst.size());
    for (int k = 0; k < cardlst.size(); k++)
    {
        ff_card_t* a_loc_card_format_p = cardlst[k];
        MCDS_get_ff_card_attributes(a_loc_card_format_p, CARD_TYPE, &a_card_type, END_ARGS);
        if (a_card_type == CARD_IF)
        {
            HCDI_ReleasePreObjectHandle(a_pre_object);
            return -1;
        }
        
        if (a_card_type == CARD_SINGLE)
        {
            int   a_nb_cells = 0;
            MCDS_get_ff_card_attributes(a_loc_card_format_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);

            for (int j = 0; j < a_nb_cells; ++j) {
                ff_cell_t* a_cell_format_p = NULL;
                MCDS_get_ff_card_tab(a_loc_card_format_p, CARD_CELL, j, (void*)(&a_cell_format_p));
                //
                const char* a_cell_fmt = NULL;
                int   a_cell_ikw = END_ARGS;
                MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_FORMAT, &a_cell_fmt, CELL_IKEYWORD, &a_cell_ikw, END_ARGS);
                int a_cell_size = 0;
                value_type_e a_vtype = a_descr_p->getValueType(a_cell_ikw);
                attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
                

                if (a_atype == ATYPE_DYNAMIC_ARRAY || a_atype == ATYPE_STATIC_ARRAY)
                {
                    ikey_att_arr = a_cell_ikw;
                }
                //else
                //    continue; /* need to check for usecase of single value type */

                //const char* skey = a_descr_p->getSKeyword(a_cell_ikw);
                ff_cell_type_e  a_cell_type = CELL_UNKNOWN;
                MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_TYPE, &a_cell_type, END_ARGS);

                if (a_cell_type == CELL_COMMENT)
                {
                    const char* a_cell_string = NULL;
                    MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_STRING, &a_cell_string, END_ARGS);
                    a_cell_size = (int)strlen(a_cell_string);
                }
                else
                    GetSyntaxInfo()->GetFormatSize(a_cell_fmt, false, a_cell_size);

                FieldMeta fd;
                fd.ikey = a_cell_ikw;
                fd.width_base = a_cell_size;
                fd.type = a_vtype;
                fd.cell_p = a_cell_format_p;
                fd.width_cur = a_cell_size;
                
                if(a_cell_ikw>0)
                    fd.skey = a_descr_p->getSKeyword(a_cell_ikw);
                
                metaList[k].push_back(fd);

                // Create map entries (initial page-wise reserve)
                switch (fd.type) {
                case VTYPE_INT:
                    map_ikey_vec_i[fd.ikey].reserve(ALLOC_SIZE); break;
                case VTYPE_FLOAT:
                    map_ikey_vec_d[fd.ikey].reserve(ALLOC_SIZE); break;
                case VTYPE_STRING:
                    map_ikey_vec_s[fd.ikey].reserve(ALLOC_SIZE); break; // fast, caller can free if needed
                case VTYPE_OBJECT:
                    map_ikey_vec_obj_id[fd.ikey].reserve(ALLOC_SIZE); break;
                }
            }
        }
    }
    if (!metaList.size())
    {
        HCDI_ReleasePreObjectHandle(a_pre_object);
        a_cur_file_p->SetCurrentLocation(aloc);
        a_cur_file_p->SetCurrentLine(acurline);
        SetCurrentFileIndex(file_index);
        setCurrentSubdeckIndex(subdeck_index);
        return -1;
    }

    // ===== OPTIMIZATION: BIND DIRECT DESTINATION POINTERS =====
    // Maps are now fully populated with all ikey entries. No new keys will be    
    //char buffer_p[512];
    // inserted, so pointers into the map values are stable.
    // reserve() on existing entries does NOT invalidate pointers (only grows capacity).
    for (auto& card_meta : metaList) {
        for (auto& meta : card_meta) {
            switch (meta.type) {
            case VTYPE_INT:
                meta.dst_int = &map_ikey_vec_i[meta.ikey]; break;
            case VTYPE_FLOAT:
                meta.dst_double = &map_ikey_vec_d[meta.ikey]; break;
            case VTYPE_STRING:
                meta.dst_str = &map_ikey_vec_s[meta.ikey]; break;
            case VTYPE_OBJECT:
                meta.dst_obj_id = &map_ikey_vec_obj_id[meta.ikey]; break;
            }
        }
    }

    // ===== PHASE 5: PARSING SETUP =====
    bool do_continue = true;
    int line_count = 0;


    int ml_sz = (int)metaList.size();

    const char free_c = mySyntaxInfo->getFreeFormatSpecifier();
    const int line_len = mySyntaxInfo->getLineLength();
    const char* paramsym = mySyntaxInfo->getParameterSymbol();
    const char paramsym_char = (paramsym && paramsym[0]) ? paramsym[0] : '\0';

    while (do_continue)
    {
        int cur_fmt_size = 0;
        for (int i = 0; i < ml_sz; i++) // loop over number of CARDs
        {
            //bool a_error = a_cur_file_p->readBuffer(myBufferNbChars, buffer_p);
            //char* buffer_p = ReadBuffer(true); // Is this faster or above
            char* buffer_p =  nullptr;

            bool a_is_eof = (a_cur_file_p == NULL || a_cur_file_p->eof());
            if (a_is_eof)
            {
                do_continue = false;
                UnreadBuffer();
                break;
            }

            buffer_p = myDataReader->readBuffer();
            if (!buffer_p) {
                do_continue = false;
                break;
            }
			// check for empty line
            if (*buffer_p == '\0') {
                while ((buffer_p = myDataReader->readBuffer()) != nullptr && *buffer_p == '\0');
                if (!buffer_p) {
                    do_continue = false;
                    break;
                }
            }
            

            bool  is_header = isHeader(buffer_p);
            bool a_is_include = myInputInfosPtr->IsIncludeHeader(buffer_p);
            bool a_is_comment = mySyntaxInfo->isComment(buffer_p);
            int h_len = mySyntaxInfo->getHeaderKeywordCellLength();

            bool has_header_line_card_cont = false;
            if (h_len && is_header) 
            {
                if (h_len && mySyntaxInfo->IsSpaceORContinueChars(buffer_p[0]) || !strncmp(buffer_p, header, h_len))
                    has_header_line_card_cont = true;
                
            }

            if ((!has_header_line_card_cont && is_header) || a_is_include)
            {
                UnreadBuffer();
                do_continue = false;
                break;
            }
            if (a_is_comment)
                continue;

            //update width based on line format type
            io_types::format_type_e  fmt_type = mySyntaxInfo->getFormatType();
            if (fmt_type == io_types::format_type_e::FORMAT_LONG && fmt_type  != fmt_type_base)
            {
                for (auto& meta : metaList[i])
                {
                    if (meta.cell_p->type != CELL_COMMENT)
                        meta.width_cur = meta.width_base * 2;
                    else
                        meta.width_cur = meta.width_cur;
                }
            }
            else
            {
                for (auto& meta : metaList[i])
                {
                    meta.width_cur = meta.width_base;
                }
            }

            
            if (has_header_line_card_cont || !is_header && !a_is_include)
            {
                line_count++;

                // ===== PAGE-WISE REALLOCATION (PRESERVED) =====
                // reserve() on existing vectors does NOT invalidate dst_* pointers
                // because the pointer is to the vector object in the map, not to its data.
                int rem = line_count % ALLOC_SIZE;
                if (!rem)
                {
                    int next_capacity = ALLOC_SIZE * ((line_count / ALLOC_SIZE) + 1);
                    for (auto& pair : map_ikey_vec_d) {
                        pair.second.reserve(next_capacity);
                    }
                    for (auto& pair : map_ikey_vec_i) {
                        pair.second.reserve(next_capacity);
                    }
                }

                const char* ptr = buffer_p;
                if (!memchr(ptr, free_c /*','*/, line_len))
                {
                    // ===== FIXED FORMAT PARSING =====
                    cur_fmt_size = 0;
                    for (const auto& meta : metaList[i])
                    {
                        const char* fieldStart = ptr;  // Remember start for fixed width slicing

                        if (ptr && (ptr[0] == '\0' ||
                            (mySyntaxInfo->IsSpaceORContinueChars(ptr[0]) && ptr[1] == '\0'))) 
                        {
                            switch (meta.type) {
                            case VTYPE_INT:
                                meta.dst_int->emplace_back(0); break;
                            case VTYPE_FLOAT:
                                meta.dst_double->emplace_back(0.0); break;
                            case VTYPE_STRING:
                                meta.dst_str->emplace_back(""); break;
                            case VTYPE_OBJECT:
                                meta.dst_obj_id->emplace_back(0); break;
                            }

                            continue;
                        }
                        if (meta.cell_p->type != CELL_COMMENT && meta.cell_p->type != CELL_BLANK)
                        {
                            int p_count = 1;
                            if (meta.cell_p->type == CELL_PAIR)
                            {
                                p_count = 2;
                            }
                            for (int k = 0; k < p_count; k++)
                            {
                                int l_s = 0;
                                while (*ptr == ' ' && meta.width_cur > l_s)
                                {
                                    ++ptr; l_s++;
                                }
                                int leadingSpaces = static_cast<int>(ptr - fieldStart);
                                if (paramsym_char && (*ptr == paramsym_char))
                                {
                                    ++ptr;
                                    char alias[32] = {};
                                    int len = 0;
                                    int maxAliasChars = meta.width_cur - leadingSpaces - 1;
                                    while (*ptr && len < maxAliasChars && len < 31)
                                        alias[len++] = *ptr++;

                                    alias[len] = '\0';
                                    bool is_parameter_negated = (alias[0] == '-');

                                    MECIModelFactory* nc_model_p = (MECIModelFactory*)model_p;
                                    switch (meta.type) {
                                    case VTYPE_INT:
                                    {
                                        int a_value = nc_model_p->GetIntParameter(alias, a_pre_object->GetFileIndex());
                                        if (is_parameter_negated) a_value = -a_value;
                                        meta.dst_int->push_back(a_value);
                                        a_pre_object->SetParameterName(alias, meta.skey.c_str(), line_count - 1, is_parameter_negated);
                                        break;
                                    }
                                    case VTYPE_FLOAT:
                                    {
                                        double a_value = nc_model_p->GetFloatParameter(alias, a_pre_object->GetFileIndex());
                                        if (is_parameter_negated) a_value = -a_value;
                                        meta.dst_double->push_back(a_value);
                                        a_pre_object->SetParameterName(alias, meta.skey.c_str(), line_count - 1, is_parameter_negated);
                                        break;
                                    }
                                    case VTYPE_STRING:
                                    {
                                        //map_ikey_vec_s[meta.ikey].push_back(strdup(temp)); 
                                        break;
                                    }
                                    case VTYPE_OBJECT:
                                    {
                                        int a_id = nc_model_p->GetIntParameter(alias, a_pre_object->GetFileIndex());
                                        if (is_parameter_negated) a_id = -a_id;
                                        meta.dst_obj_id->push_back(a_id);
                                        a_pre_object->SetParameterName(alias, meta.skey.c_str(), line_count - 1, is_parameter_negated);
                                        break;
                                    }
                                    }
                                    /**/
                                }
                                else {
                                    char temp[64] = {};
                                    // Adjust width to exclude leading spaces
                                    int fieldWidth = meta.width_cur - leadingSpaces;
                                    memcpy(temp, ptr, fieldWidth);
                                    temp[fieldWidth] = '\0';

                                    switch (meta.type) {
                                    case VTYPE_INT:
                                        meta.dst_int->emplace_back(cfgio_atoi_fast(temp));
                                        break;
                                    case VTYPE_FLOAT:
                                        meta.dst_double->emplace_back(cfgio_atof_fast(temp));
                                        break;
                                    case VTYPE_STRING:
                                        meta.dst_str->emplace_back(temp);
                                        break;
                                    case VTYPE_OBJECT:
                                        meta.dst_obj_id->emplace_back(cfgio_atoi_fast(temp));
                                        break;
                                    }
                                }
                                ptr += meta.width_cur - leadingSpaces;
                                fieldStart = ptr;
                            }
                        }
                        else
                        {
                            ptr += meta.width_cur;
                        }

                        cur_fmt_size += meta.width_base;
                        if (fmt_type == io_types::format_type_e::FORMAT_LONG && mySyntaxInfo->HasLengthReachedForNextLine(cur_fmt_size))
                        {
                            //const char* a_card = readBuffer();
                            const char* a_card = myDataReader->readBuffer();
                            if (a_card == NULL)
                                return false;

                            //myReadContext_p->killNLEnd(a_card);
                            
                            //a_cur_cell = a_card;
                            int a_is_header = mySyntaxInfo->isHeader(a_card);
                            if (a_is_header)
                            {
                                a_ok = false;
                                unreadBuffer();
                                break;
                            }
                            //
                            bool is_free_size_format = mySyntaxInfo->isFreeSizeCard(a_card);
                            int offset = mySyntaxInfo->getInitialOffset(); // Need to get based of solver... should take care of free format as well with long format
                            if (offset && is_free_size_format)
                            {
                                offset = 1;
                                for (const char* p = a_card; *p; ++p) {
                                    if (*p == mySyntaxInfo->getFreeFormatSpecifier()) {
                                        offset++;
                                        break;
                                    }
                                }
                            }
                            if (strlen(a_card) < offset)
                                return false;
                            a_card = a_card + offset;
                            cur_fmt_size = 0;

                            ptr = a_card;
                        }
                    }
                }
                else
                {
                    // ===== FREE FORMAT PARSING =====
                    size_t fieldIndex = 0;
                    //while (ptr && *ptr && fieldIndex < metaList.size()) {
                    for (const auto& meta : metaList[i])
                    {
                        //const char* fieldStart = ptr;
                        //const auto& meta = metaList[fieldIndex];
                        const char* start = ptr;
                        const char* end = nullptr;
                        size_t len = 0;
                        if (start)
                        {
                            end = strchr(ptr, free_c /*','*/);
                            len = end ? static_cast<size_t>(end - ptr) : strlen(ptr);

                            int l_s = 0;
                            while (*ptr == ' ' && len >= l_s)
                            {
                                ++ptr; l_s++;
                            }
                            len = len-l_s;
                        }
                        if (len == 0) {
                            // Empty field
                            switch (meta.type) {
                            case VTYPE_INT:
                                meta.dst_int->emplace_back(0); break;
                            case VTYPE_FLOAT:
                                meta.dst_double->emplace_back(0); break;
                            case VTYPE_STRING:
                                meta.dst_str->emplace_back(""); break;
                            case VTYPE_OBJECT:
                                meta.dst_obj_id->emplace_back(0); break;
                            }
                        }
                        else if (paramsym_char && (*ptr == paramsym_char)) {
                            if (len > 1) {
                                char alias[32] = {};
                                size_t copyLen = std::min(len - 1, sizeof(alias) - 1);
                                memcpy(alias, ptr + 1, copyLen);
                                alias[copyLen] = '\0';

                                bool is_parameter_negated = (alias[0] == '-');

                                MECIModelFactory* nc_model_p = (MECIModelFactory*)model_p;
                                switch (meta.type) {
                                case VTYPE_INT:
                                {
                                    int a_value = nc_model_p->GetIntParameter(alias, a_pre_object->GetFileIndex());
                                    if (is_parameter_negated) a_value = -a_value;
                                    meta.dst_int->push_back(a_value);
                                    a_pre_object->SetParameterName(alias, meta.skey.c_str(), line_count - 1, is_parameter_negated);
                                    break;
                                }
                                case VTYPE_FLOAT:
                                {
                                    double a_value = nc_model_p->GetFloatParameter(alias, a_pre_object->GetFileIndex());
                                    if (is_parameter_negated) a_value = -a_value;
                                    meta.dst_double->push_back(a_value);
                                    a_pre_object->SetParameterName(alias, meta.skey.c_str(), line_count - 1, is_parameter_negated);
                                    break;
                                }
                                case VTYPE_STRING:
                                {
                                    //map_ikey_vec_s[meta.ikey].push_back(strdup(temp)); 
                                    break;
                                }
                                case VTYPE_OBJECT:
                                {
                                    int a_id = nc_model_p->GetIntParameter(alias, a_pre_object->GetFileIndex());
                                    if (is_parameter_negated) a_id = -a_id;
                                    meta.dst_obj_id->push_back(a_id);
                                    a_pre_object->SetParameterName(alias, meta.skey.c_str(), line_count - 1, is_parameter_negated);
                                    break;
                                }
                                }
                            }
                        }
                        else {
                            char temp[64] = {};
                            size_t copyLen = std::min(len, sizeof(temp) - 1);
                            memcpy(temp, ptr, copyLen);
                            temp[copyLen] = '\0';

                            switch (meta.type) {
                            case VTYPE_INT:
                                meta.dst_int->emplace_back(cfgio_atoi_fast(temp)); break;
                            case VTYPE_FLOAT:
                                meta.dst_double->emplace_back(cfgio_atof_fast(temp)); break;
                            case VTYPE_STRING:
                                meta.dst_str->emplace_back(temp); break;
                            case VTYPE_OBJECT:
                                meta.dst_obj_id->emplace_back(cfgio_atoi_fast(temp)); break;
                            }
                        }
                        ptr = end ? end + 1 : nullptr;
                        ++fieldIndex;
                    }
                }
            }
        }
    }

    //int ikey = p_type_info->pdescrp->getIKeyword("id"); //get it from any array skeyword

    int size_ikey = p_type_info->pdescrp->getSizeIKeyword(ikey_att_arr);
    MvIKeywordSet_t       a_array_ikws;
    p_type_info->pdescrp->getSizeConnectedIKeywords(size_ikey, &a_array_ikws);

    int t_size_d = 0;
    int t_size_i = 0;
    int t_size_obj = 0;
    int t_size = 0;
    if (map_ikey_vec_d.size())
        t_size_d = (int)map_ikey_vec_d[(*map_ikey_vec_d.begin()).first].size();
    if (map_ikey_vec_i.size())
        t_size_i = (int)map_ikey_vec_i[(*map_ikey_vec_i.begin()).first].size();
    if (map_ikey_vec_obj_id.size())
        t_size_obj = (int)map_ikey_vec_obj_id[(*map_ikey_vec_obj_id.begin()).first].size();

    t_size = std::max({ t_size_d, t_size_i, t_size_obj });

    if (!t_size)
        assert(0);

    string skey_size = a_descr_p->getSKeyword(size_ikey);

    a_pre_object->AddIntValue(skey_size.c_str(), t_size);
    if (a_array_ikws.size())
    {
        for (auto a_aikw_it = a_array_ikws.begin(); a_aikw_it != a_array_ikws.end(); ++a_aikw_it) {
            int a_arr_ikw = (*a_aikw_it);
            ResizeArrayAttributesToPreObject(*a_pre_object, p_type_info->pdescrp, a_arr_ikw, t_size);
        }
    }

    //a_pre_object->SetFileIndex(file_index);
    a_pre_object->SetEntityType(p_type_info->obj_type);

    model_p->StorePreObject(p_type_info->obj_type, a_pre_object);

    for (auto& pair : map_ikey_vec_d)  {
        string skey = a_descr_p->getSKeyword(pair.first);
        pair.second.resize(t_size);
        pair.second.shrink_to_fit();
        a_pre_object->AddFloatValues(skey.c_str(), std::move(pair.second));
    }
    for (auto& pair : map_ikey_vec_i) {
        string skey = a_descr_p->getSKeyword(pair.first);
        pair.second.resize(t_size);
        pair.second.shrink_to_fit();
        a_pre_object->AddIntValues(skey.c_str(), std::move(pair.second));
    }
    for (auto& pair : map_ikey_vec_obj_id) {
        string skey = a_descr_p->getSKeyword(pair.first);
        object_type_e  a_cell_otype = a_descr_p->getObjectType(pair.first);
        const string  &a_cell_otype_str = MV_get_type(a_cell_otype);
        pair.second.resize(t_size);
        pair.second.shrink_to_fit();
        a_pre_object->AddObjectValues(skey.c_str(), a_cell_otype_str.c_str(), std::move(pair.second));
    }

    //for (const auto& [key, vec] : map_ikey_vec_ui) {
    //    string skey = a_descr_p->getSKeyword(key);
    //    a_pre_object->AddUIntValues(skey, std::move(vec));
    //}
    //for (const auto& [key, vec] : map_ikey_vec_s) {
    //    string skey = a_descr_p->getSKeyword(key);
    //    a_pre_object->AddStringValues(skey, std::move(vec));
    //}

    return 0;
}








/* --------- Reading model (public) --------- */

void HWCFGReader::readModel(MECIModelFactory* model_p, bool do_transform) {

    HWCFG_PERF_TIMER("HWCFGReader::readModel");

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

    myCurrentHeader = a_header;

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
    if (input_ftype_str.empty())
    {
        int header_size = mySyntaxInfo->getHeaderKeywordCellLength();
        if (header_size > 0)
        {
            const string str = string(a_header);
            input_ftype_str = (str.size() <= header_size) ? str : str.substr(0, header_size);
        }
        else
            input_ftype_str = a_header;
    }
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
        if (!*header_card)
            *header_card = strdup(a_header);
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
        preobj = model_p->CreateObject(a_kftype_str.c_str(), a_iftype_str != "" ? a_iftype_str.c_str() : a_kftype_str.c_str(), a_title_str, a_id, a_unit_id);
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
bool HWCFGReader::IsIncludeBlockActive(int fileIndex) const 
{
    if (fileIndex < 0 || fileIndex >= (int)myIncludeBlock.size()) {
        return false; // Safe default for invalid indices
    }
    return myIncludeBlock[fileIndex];
}

void HWCFGReader::SetIncludeBlockState(int fileIndex, bool state) 
{
    EnsureIncludeBlockSize(fileIndex);
    if (fileIndex >= 0 && fileIndex < (int)myIncludeBlock.size()) {
        myIncludeBlock[fileIndex] = state;
#ifdef DEBUG_INCLUDE_BLOCKS
        printf("[DEBUG] Set include block state for file %d to %s\n",
            fileIndex, state ? "true" : "false");
#endif
    }
}

void HWCFGReader::UpdateIncludeBlockState(int fileIndex, bool isHeader) 
{
    EnsureIncludeBlockSize(fileIndex);
    if (isHeader) {
        SetIncludeBlockState(fileIndex, false);
    }
    // For non-header cases, state is managed in IsIncludedFile
}

void HWCFGReader::EnsureIncludeBlockSize(int fileIndex) 
{
    if ((int)myIncludeBlock.size() <= fileIndex) {
        size_t oldSize = myIncludeBlock.size();
        myIncludeBlock.resize(fileIndex + 1, false);
#ifdef DEBUG_INCLUDE_BLOCKS
        printf("[DEBUG] Resized myIncludeBlock from %zu to %d for file %d\n",
            oldSize, fileIndex + 1, fileIndex);
#endif
    }
}

void HWCFGReader::OptimizeIncludeBlockStorage() 
{
    // Optional: shrink vector when files are closed
    // Remove trailing false values to save memory
    while (!myIncludeBlock.empty() && !myIncludeBlock.back()) {
        myIncludeBlock.pop_back();
    }
}
/*default implementation is to read the include file in the same line.. incase of dyna it is in next line...*/
        // Enhanced IsIncludedFile method with better error handling and state management
bool HWCFGReader::IsIncludedFile(const char* buffer, char** full_name_p, char** relative_name_p, char** include_path)
{
    if (!buffer || !full_name_p || !relative_name_p) {
        return false;
    }

    bool a_is_include = myInputInfosPtr->IsIncludeHeader(buffer);
    int fileIndex = GetCurrentFileIndex();

    // Ensure we have valid file index
    if (fileIndex < 0) {
        fileIndex = 0;
    }

    EnsureIncludeBlockSize(fileIndex);
    bool inIncludeBlock = IsIncludeBlockActive(fileIndex);

    if (a_is_include || inIncludeBlock) {
        bool cont = true;
        std::string result;
        string a_name, a_name_prev = "";

        //get descriptor
        //create preobject based on format defined wiht common datanames + some solver 

        //store preobject in mv_modelfactor_t
        bool isfilepathonesameline = myInputInfosPtr->IsIncludeFilePathOnSameLine();
        if (isfilepathonesameline)
        {
            char* a_buffer = strdup(buffer + strlen(myInputInfosPtr->GetIncludeKeyword()));
            if (!a_buffer) {
                SetIncludeBlockState(fileIndex, false);
                return false;
            }
            killBlanksNLEnd(a_buffer);
            a_name_prev = killBlanksBegin(a_buffer);
            boost::erase_all(a_name_prev, "'");
            myfree(a_buffer);
        }

        // Remove "NAME", optional spaces, "=", optional spaces, keep only filename
        // Replace the regex and extraction logic for "NAME" to also support "INPUT" and allow spaces before/after '=' and value
        static const std::string name_key = "NAME";
        static const std::string input_key = "INPUT";
        // Find "NAME" or "INPUT" as a whole word, possibly after commas/spaces/tabs, allow spaces before/after '=' and value
        std::regex name_input_regex(R"((^|[,\s])(NAME|INPUT)\s*=\s*([^\s,]+))", std::regex_constants::icase);
        std::smatch match;
        if (std::regex_search(a_name_prev, match, name_input_regex)) {
            // match[3] is the filename (non-space, non-comma sequence after '=')
            std::string after_name = match[3].str();
            // Trim trailing spaces/tabs
            size_t last_non_space = after_name.find_last_not_of(" \t");
            if (last_non_space != std::string::npos) {
                after_name = after_name.substr(0, last_non_space + 1);
            }
            a_name_prev = after_name;
        }

        /*******************************************************************************/
        // In future this will be handled from cfg file of include
        int curmode = GetReadingStatus();  // need to set this to avoid again going to isinclude/iscomponent
        SetReadingStatus(READ_STATUS_SINGLE_KEYWORD);
        char* cLine = nullptr;
        std::string mutableBuffer; // Create a mutable copy for cases where we need to modify

        if (a_name_prev != "") {
            mutableBuffer = a_name_prev;
            cLine = &mutableBuffer[0];
        }
        else if (!a_is_include) {
            mutableBuffer = std::string(buffer);
            cLine = &mutableBuffer[0];
            a_is_include = true;
        }
        else {
            cLine = ReadBuffer();
        }

        std::string nextLine = "";
        while (cont) {
            if (!cLine) break;
            killBlanksNLEnd(cLine);
            string line = killBlanksBegin(cLine);
            std::string trimmed = trimRight(line);

            if (endsWith(trimmed, " +")) {
                trimmed = trimRight(trimmed.substr(0, trimmed.size() - 2));
            }
            else if (endsWith(trimmed, "...")) {
                trimmed = trimRight(trimmed.substr(0, trimmed.size() - 3));
            }
            else if (endsWith(trimmed, "&")) {
                trimmed = trimRight(trimmed.substr(0, trimmed.size() - 1));
            }
            else if (!result.empty() && !line.empty() && line[0] == '+') {
                // Remove '+' and leading spaces
                size_t start = 1;
                while (start < line.size() && isspace(line[start])) ++start;
                trimmed = line.substr(start);
                trimmed = trimRight(trimmed);
            }
            else
            {
                result += trimmed;
                break;
            }
            result += trimmed;
            char* peeked = ReadBuffer();
            if (peeked)
            {
                bool  a_is_header = isHeader(peeked);
                if (a_is_header)
                {
                    UnreadBuffer();
                    SetIncludeBlockState(fileIndex, false);
                    break;
                }
                killBlanksNLEnd(peeked);
            }
            else
            {
                UnreadBuffer();
                SetIncludeBlockState(fileIndex, false);
                break;
            }
            nextLine = peeked ? peeked : "";
            if (!hasContinuation(line, nextLine)) {
                UnreadBuffer();
                break;
            }

            // For subsequent iterations, create a mutable copy of nextLine
            mutableBuffer = nextLine;
            cLine = &mutableBuffer[0];
        }
        SetReadingStatus(curmode);

        int rstatus = GetReadingStatus();
        SetReadingStatus(READ_STATUS_SINGLE_KEYWORD);

        // Lookahead scan to determine if we're inside an include block
        // This determines whether following lines should be treated as include data
        pushPosition(); // Save current position before lookahead
        while (true) {
            // Read next line including comments (important for Radioss format)
            char* peeked = ReadBuffer(false, -1, false);

            // Handle end of file or read error
            if (!peeked) {
                SetIncludeBlockState(fileIndex, false);
                break;
            }

            // Check if we've encountered a header line
            if (isHeader(peeked)) {
                // Header found: include block ends here
                SetIncludeBlockState(fileIndex, false);
                break;
            }

            // Check if this line is another include header
            if (myInputInfosPtr->IsIncludeHeader(peeked)) {
                // Another include header: current include block ends
                SetIncludeBlockState(fileIndex, false);
                break;
            }

            // Line is not an include header - check if it's a comment
            if (!isComment(peeked)) {
                // Non-comment, non-header line: we are inside an include block
                SetIncludeBlockState(fileIndex, true);
                break;
            }

            // If we reach here, it's a comment line - continue scanning
        }

        // Restore file position to where we started the lookahead
        popPosition();

        SetReadingStatus(rstatus);
        /*******************************************************************************/
        a_name = result;

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
            MECReadFile* a_cur_file_p = GetCurrentFilePtr();
            if (a_cur_file_p)
            {
                a_main_name_file = a_cur_file_p->GetFullName();
            }

                a_full_name = my_file_full_name_modify_with_relative(a_main_name_file, a_name);

            if (!file_exists(a_full_name) && myMainFile != a_main_name_file)
            {
                a_full_name = my_file_full_name_modify_with_relative(myMainFile, a_name);
            }

            // If file still not found, search in all paths from myFolderPathToPreObjectMap
            // These paths are relative to the directory containing a_main_name_file

            if (!file_exists(a_full_name)) {
                string main_dir = my_get_dir_path(a_main_name_file);
                string fullfile_name = findIncludeFileInSearchPaths(main_dir, a_name, include_path);
                if (!fullfile_name.empty()) {
                    a_full_name = fullfile_name;
                }
            }
            a_relative_name = a_name;
        }

        *relative_name_p = strdup(a_relative_name.c_str());
        *full_name_p = strdup(a_full_name.c_str());

        // Check for allocation failures
        if (!*relative_name_p || !*full_name_p) {
            if (*relative_name_p) {
                free(*relative_name_p);
                *relative_name_p = nullptr;
            }
            if (*full_name_p) {
                free(*full_name_p);
                *full_name_p = nullptr;
            }
            SetIncludeBlockState(fileIndex, false);
            return false;
        }

        my_dir_restore_current_path(last_current_path);
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
    //const char *subdeck_key = myInputInfosPtr->GetSolverSubdeckKeyword();
    //size_t len1 = strlen(subdeck_key);

    //if (!len1)
    //    return false;

    //bool is_a_component = (strncmp(buffer, subdeck_key, len1) == 0);
    bool is_a_component = myInputInfosPtr->IsBlockKeyword(buffer, true);
    if (!is_a_component)
    {
        //const char* subdeck_keyend =   myInputInfosPtr->GetSolverSubdeckKeywordEnd();
        //size_t len2 = strlen(subdeck_keyend);
        //if (len2 > 0)
        //{
        //    is_a_component = (strncmp(buffer, subdeck_keyend, len2) == 0);
        //}
        is_a_component = myInputInfosPtr->IsBlockKeyword(buffer, false);
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
    //const char* subdeck_key = myInputInfosPtr->GetSolverSubdeckKeyword();
    //size_t len1 = strlen(subdeck_key);

    const std::string* subdeck_str1 = nullptr;

    bool is_a_component = myInputInfosPtr->IsBlockKeyword(buffer, true, true, &subdeck_str1);
    if (is_a_component && subdeck_str1)
    {
        killBlanksNLEnd(a_buffer);
        // Splitting header
        string subdeck_str(*subdeck_str1);
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

                if (!file_exists(a_full_name)) {
                    string main_dir = my_get_dir_path(a_main_name_file);
                    string fullfile_name = findIncludeFileInSearchPaths(main_dir, component_name, nullptr);
                    if (!fullfile_name.empty()) {
                        a_full_name = fullfile_name;
                    }
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
                        

                        
                        IMECPreObject* last_include_po = MECSubdeck::mySubdeckVector.back()->GetPreObject();
                        if (last_include_po)
                        {
                            IDescriptor* pdescrp = HCDI_GetDescriptorHandle(last_include_po->GetKernelFullType());
                            if (pdescrp)
                            {
                                string filemname_skey = GetAttribNameFromDrawable(pdescrp, cdr::g_AttribFileName);
                                last_include_po->AddStringValue(filemname_skey.c_str(), a_relative_name.c_str());
                            }
                        }
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

void HWCFGReader::displayMessage(MyMsgType_e msg_type,const char *format,...) const
{
    if (format == NULL) return;

    if(!m_pMessageList)
    {
        m_pMessageList = new HWCFGReaderMessageList();
        m_owningMessageList = true;
    }

    int type;
    switch(msg_type)
    {
        case MSG_MESSAGE: type = 0; break;
        case MSG_WARNING: type = 1; break;
        default         : type = 2;
    }

    va_list args;
    va_start(args, format);
    m_pMessageList->Add(format, args, type,
                        myCurrentHeader, myLineBuffer,
                        getCurrentFullName(), (unsigned int) getCurrentLine());
    va_end(args);
}

void HWCFGReader::SetMessageList(HWCFGReaderMessageList* pMessageList, bool owningMessageList)
{
    if(m_owningMessageList && m_pMessageList) delete m_pMessageList;
    m_pMessageList = pMessageList;
    m_owningMessageList = owningMessageList;
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
    if (myCurrentLoc < 0) // may have reached end of file
        return;
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
        preobj = model_p->CreateObject(a_kftype_str.c_str(), a_iftype_str != "" ?  a_iftype_str.c_str() : a_kftype_str.c_str(), a_title_str, a_id, a_unit_id);
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
        preobj = model_p->CreateObject(a_kftype_str.c_str(), a_iftype_str != "" ? a_iftype_str.c_str() : a_kftype_str.c_str(), a_title_str, a_id, a_unit_id);
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

void HWCFGReaderLSDyna::postTreatLineCount(const CUserNameTypeInfo* p_type_info, string& header, int *line_count)
{
    if (p_type_info == NULL)
        return;

    if (p_type_info->myAllPossibleflags)
    {
        int sub_count_id_or_title = 0;
        static string can_have_title_card_str = "CAN_HAVE_TITLE_CARD";
        static string can_have_id_card_str = "CAN_HAVE_ID_CARD";
        static string has_title_card_str = "HAS_TITLE_CARD";
        static string has_id_card_str = "HAS_ID_CARD";
        static string title_str = "_TITLE";
        static string id_str = "_ID";

        /*shoud be a part of member variable later*/
        int bit_can_have_title = HCDI_get_data_hierarchy_bitmask(can_have_title_card_str);
        int bit_hastitle = HCDI_get_data_hierarchy_bitmask(has_title_card_str);

        bool has_title = p_type_info->myAllPossibleflags & bit_hastitle;
        bool can_have_title = false;
        if (!has_title)
            can_have_title = p_type_info->myAllPossibleflags & bit_can_have_title;

        if (has_title || can_have_title)
        {
            bool title_flag = true;
            if (!has_title)
            {
                size_t pos = header.find(title_str);
                if (pos == string::npos)
                    title_flag = false;
            }
            if (title_flag)
            {
                sub_count_id_or_title = 1;
            }
        }
        else
        {
            int bit_has_idcard = HCDI_get_data_hierarchy_bitmask(has_id_card_str);
            int bit_can_have_idcard = HCDI_get_data_hierarchy_bitmask(can_have_id_card_str);

            bool has_idcard = p_type_info->myAllPossibleflags & bit_has_idcard;
            bool can_have_idcard = false;
            if (!has_idcard && bit_can_have_idcard)
                can_have_idcard = p_type_info->myAllPossibleflags & bit_can_have_idcard;

            if (has_idcard || can_have_idcard)
            {
                bool idcard_flag = true;
                if (can_have_idcard)
                {
                    size_t pos = header.find(id_str);
                    if (pos == string::npos)
                        idcard_flag = false;
                }

                if (idcard_flag)
                {
                    sub_count_id_or_title = 1;
                }
            }
        }
        if (line_count)
        {
            *line_count -= sub_count_id_or_title;
        }
    }
}

string HWCFGReader::findIncludeFileInSearchPaths(const string& main_dir, const string& filename, char** include_path) 
{
    for (const auto& pathPreObjPair : myFolderPathToPreObjectMap) {
        const string& relPath = pathPreObjPair.first;
        string res_fullname;

        // Check if the relative path contains ../ or ..\ which requires special handling
        if (relPath.find("../") != std::string::npos || relPath.find("..\\") != std::string::npos) {
            // For parent directory navigation, resolve from main_dir + relPath, then add filename
            string resolved_dir = my_file_full_name_modify_with_relative(main_dir + "/", relPath);
            res_fullname = my_file_full_name_modify_with_relative(resolved_dir + "/", filename);
        }
        else {
            // For simple paths, remove leading "./" if present and combine main_dir + relPath + filename
            string cleanPath = relPath;
            if (cleanPath.length() >= 2 && cleanPath.substr(0, 2) == "./") {
                cleanPath = cleanPath.substr(2);  // Remove "./"
            }

            // Check if relPath is absolute (contains drive letter on Windows or starts with / on Unix)
            bool isAbsolute = false;

#ifdef _WIN32
            // Windows: Check for drive letter (C:) or UNC path (\\) or forward slash UNC (//)
            if ((cleanPath.length() >= 3 && cleanPath[1] == ':') ||
                (cleanPath.length() >= 2 && cleanPath.substr(0, 2) == "\\\\") ||
                (cleanPath.length() >= 2 && cleanPath.substr(0, 2) == "//")) {
                isAbsolute = true;
            }
#else
            // Unix/Linux: Check if starts with /
            if (!cleanPath.empty() && cleanPath[0] == '/') {
                isAbsolute = true;
            }
#endif
            if (isAbsolute) {
                // relPath is absolute, combine directly with filename
                // Ensure proper path separator
                char sep = '/';
#ifdef _WIN32
                if (cleanPath.find('\\') != std::string::npos) {
                    sep = '\\';
                }
#endif
                res_fullname = cleanPath + sep + filename;
            }
            else {
                // relPath is relative, use existing logic with main_dir
                string combined_path = main_dir + "/" + cleanPath + "/" + filename;
                res_fullname = combined_path;
            }
        }

        if (file_exists(res_fullname)) {
            if (include_path) {
                // Store the relative path corresponding to the found full name
                *include_path = strdup(relPath.c_str());
            }
            return res_fullname;
        }
    }
    return ""; // Empty string if not found
}


/************************************************/
