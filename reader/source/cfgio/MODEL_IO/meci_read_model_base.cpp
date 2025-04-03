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
#include <UTILS/memory_utils.h>
#include <MESSAGE/mv_messages.h>
#include <UTILS/mv_iostream.h>
#include <UTILS/set_utils.h>
#include "meci_read_context.h"

#include <MODEL_IO/cdr_reserveattribs.h>
#include <MODEL_IO/hcioi_utils.h>

#include <HCDI/hcdi_mv_descriptor.h>
#include <HCDI/mec_pre_object.h>
#include "mv_model_factory.h"
#include <UTILS/mv_cstring.h>
#include <UTILS/mv_string.h>
#include "mv_header_positions.h"
#include "meci_read_model_base.h"
#include "mec_component.h"
#include "mec_subdeck.h"
#define MAIN_COMPONENT_TITLE "Main" 

typedef vector<MECReadFile *>     LocFileVect_t;       
typedef vector<MECComponent*>     LocComponentVect_t;  
typedef pair<string,string>       LocKeywordOType_t;
typedef vector<LocKeywordOType_t> LocObjectKeywords_t;
typedef vector<string>            LocControlCardList_t;
typedef string                    LocRecordBuffer_t;   
typedef set<string>               LocFileList_t;     
typedef stack<int>                LocIntStack_t;    
typedef set<int>                  LocIntSet_t;      

typedef enum ReadingBlockStatus_s
{
   READ_STATER_BLOCK = 0,
   READ_ENGINE_BLOCK = 1
} ReadingBlockStatus_e;

/* --------- Constructors & destructor --------- */

MECIReadModelBase::MECIReadModelBase(const char *full_name,int buffer_nb_chars,int line_nb_chars,
                                     const ReadFileFactorySP& factory_p) :
myFileVectPtr((PseudoLocFileVect_t*)(new LocFileVect_t())),					
myComponentVectPtr((PseudoLocComponentVect_t*)(new LocComponentVect_t())),					
myComponentIndexStackPtr((PseudoIntStack_t*)(new LocIntStack_t())),          
myFileIndexStackPtr((PseudoIntStack_t*)(new LocIntStack_t())),              
myUnreadPositionPtr(new CKeywordData(0L, 0L)),                              
myHeaderPositionsPtr((PseudoHeaderPositions_t *)(new MvHeaderPositions_t())),
myInputInfosPtr(NULL),
myCryptingMethod(0),                                                
myDoRecord(false),                                                 
myRecordBufferPtr((PseudoRecordBuffer_t *)(new LocRecordBuffer_t())), 
myMainFile(strdup(full_name)),                                      
myFileListPtr((PseudoFileList_t *)(new LocFileList_t())),           
MECIReadContext(line_nb_chars, buffer_nb_chars),
myCurSubdeckIndex(-1),
myUnReadFileIndex(-1),
myFileFactoryPtr(factory_p)
{
    // file is now open in readModel() 
    // add the main component
    myIndexNewFile = 0;  
    myIndexNewComponent = 0;
    MECComponent* aComponent_p = new MECComponent(MAIN_COMPONENT_TITLE,0,0);
    aComponent_p->SetParentIndex(-1);
    aComponent_p->SetFileIndex(0);
    PushComponent(aComponent_p);
    myReadingStatus = READ_STATUS_UNKNOWN;
    if(!myFileFactoryPtr) myFileFactoryPtr = make_shared<ReadFILEFactory>();
}
MECIReadModelBase::MECIReadModelBase(MECReadFile *file,int buffer_nb_chars,int line_nb_chars,
                                     const ReadFileFactorySP& factory_p) :
myFileVectPtr((PseudoLocFileVect_t*)(new LocFileVect_t())),					
myComponentVectPtr((PseudoLocComponentVect_t*)(new LocComponentVect_t())),		
myComponentIndexStackPtr((PseudoIntStack_t*)(new LocIntStack_t())),     
myFileIndexStackPtr((PseudoIntStack_t*)(new LocIntStack_t())),     
myUnreadPositionPtr(new CKeywordData(0L, 0L)),                  
myHeaderPositionsPtr((PseudoHeaderPositions_t *)(new MvHeaderPositions_t())),
myInputInfosPtr(NULL),
myCryptingMethod(0),                                        
myDoRecord(false),                                            
myRecordBufferPtr((PseudoRecordBuffer_t *)(new LocRecordBuffer_t())),
myMainFile(const_cast<char*>("")),                                     
myFileListPtr((PseudoFileList_t *)(new LocFileList_t())),
MECIReadContext(line_nb_chars, buffer_nb_chars),
myCurSubdeckIndex(-1),
myUnReadFileIndex(-1),
myFileFactoryPtr(factory_p)
{
    // file is now open in readModel() 
    // add the main component
    myIndexNewFile = 0;  
    myIndexNewComponent = 0;
//    MECComponent* aComponent_p = new MECComponent(MAIN_COMPONENT_TITLE,0,0);
//    aComponent_p->SetParentIndex(-1);
//    aComponent_p->SetFileIndex(0);
//    PushComponent(aComponent_p);
    myReadingStatus = READ_STATUS_UNKNOWN;
    if(!myFileFactoryPtr) myFileFactoryPtr = make_shared<ReadFILEFactory>();

    updateFile(file);  
}
MECIReadModelBase::~MECIReadModelBase() {
    //
    LocFileVect_t *a_cur_file_vect_pf=((LocFileVect_t*)myFileVectPtr);
    LocFileVect_t::iterator ite_beg=a_cur_file_vect_pf->begin();
    LocFileVect_t::iterator ite_end=a_cur_file_vect_pf->end();
    for(LocFileVect_t::iterator ite=ite_beg; ite!=ite_end;++ite)
    {
        MECReadFile* a_cur_file=*ite;
       // a_cur_file->close(); 
        delete a_cur_file;
    }
    delete a_cur_file_vect_pf;

    //

    LocIntStack_t *a_cur_comp_stack_pf=((LocIntStack_t*)myComponentIndexStackPtr);
    delete a_cur_comp_stack_pf;
    //
    LocIntStack_t *a_cur_file_stack_pf=((LocIntStack_t*)myFileIndexStackPtr);
    delete a_cur_file_stack_pf;

    if(myUnreadPositionPtr) delete myUnreadPositionPtr;


    //
    MvHeaderPositions_t *a_header_positions_pf=(MvHeaderPositions_t *)myHeaderPositionsPtr;
    delete a_header_positions_pf;
    //
    
    LocRecordBuffer_t *a_record_buffer_pf=(LocRecordBuffer_t *)myRecordBufferPtr;
    delete a_record_buffer_pf;
    
    //
    
    if(((int)strlen(myMainFile))!=0)
       myfree(myMainFile);
    //
    LocFileList_t *a_file_list_pf=(LocFileList_t *)myFileListPtr;
    delete a_file_list_pf;
    
    delete ((LocComponentVect_t *)myComponentVectPtr);
    myComponentVectPtr = NULL;

    myIndexNewFile = 0; 
    myIndexNewComponent = 0;
}

void MECIReadModelBase::SetLineNbChars(int line_nb_chars)
{
    if(myLineNbChars == line_nb_chars) return;
    myLineNbChars = line_nb_chars;
    if(myBufferNbChars <= line_nb_chars)
    {
        myBufferNbChars = line_nb_chars + 1;
        myLineBuffer = (char *) myrealloc(myLineBuffer, myBufferNbChars*sizeof(char));
    }
}


/* --------- Reading model (public) --------- */
//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//	This function is used to read all data from file to the model.
//Parameters:
//      MECIModelFactory *model_p [IN]: pointer to the model
//      bool do_transform [IN]:
//Return value:
//      None
//--/////////////////////////////////////////////////////////////////////////
void MECIReadModelBase::readModel(MECIModelFactory *model_p, bool do_transform) 
{
    MECSubdeck::clearSubdeckVector();
    openFile(myMainFile);   
    //MECSubdeck::mySubdeckVector.back()->SetFileRelativeName(myMainFile);
    //displayMessage(MSG_MESSAGE, getMsg(8), GetCurrentFullName()); 

    readHeaderPositions(model_p);
    //displayHeaderPositions();
    AddSubDecks(model_p);
    if (isMessageDisplayed(MSG_ERROR))        displayMessage(MSG_ERROR, getMsg(11));
    else if (isMessageDisplayed(MSG_WARNING)) displayMessage(MSG_WARNING, getMsg(10));
    else                                     displayMessage(MSG_MESSAGE, getMsg(9));
}//End -- MECIReadModelBase::readModel



/* --------- Reading model (protected) --------- */

// It is important to note that a_cur_keyword is the keyword that was read
// at last step. This is a very bad idea!! and it must be changed asap!!!
// instead of a "while do" we must use a "do while"

#ifdef WIN32
#pragma  message ("Change  <while.... do> to a <do while> algorithm")
#endif



void MECIReadModelBase::displayHeaderPositions() const {
    
    const MvHeaderPositions_t &a_header_positions=(*((const MvHeaderPositions_t *)myHeaderPositionsPtr));
    string aString = a_header_positions.displayInfo();
    displayMessage(MSG_MESSAGE,"%s:\n",aString.c_str());
}

string MECIReadModelBase::getHeaderDetails() const
{
    const MvHeaderPositions_t& a_header_positions = (*((const MvHeaderPositions_t*)myHeaderPositionsPtr));
    return a_header_positions.displayInfo();
}

void MECIReadModelBase::updateSubKeywordRead(const char* keyword) const
{
    MvHeaderPositions_t& a_header_positions = (*((MvHeaderPositions_t*)myHeaderPositionsPtr));
    MvComponentFilePositions_t* a_comp_file_pos = NULL;

    int cur_file_indx = GetCurrentFileIndex();
    _HC_LONG cur_loc = GetCurrentLocation();
    a_comp_file_pos = a_header_positions.findType(keyword);
    int index_component = -1;
    int index_file = -1;

    if (a_comp_file_pos == NULL)
        return;

    if (a_comp_file_pos)
    {
        MvComponentFilePositions_t::iterator a_ite_beg = a_comp_file_pos->begin();
        MvComponentFilePositions_t::iterator a_ite_end = a_comp_file_pos->end();
        bool do_continue = true;
        for (MvComponentFilePositions_t::iterator a_ite = a_ite_beg; a_ite != a_ite_end; ++a_ite)
        {
            // set the component index
            index_component = (*a_ite).first;
            MvFilePositions_t& a_file_pos = (*a_ite).second;
            MvFilePositions_t::iterator a_ite2_beg = a_file_pos.begin();
            MvFilePositions_t::iterator a_ite2_end = a_file_pos.end();
            for (MvFilePositions_t::iterator a_ite2 = a_ite2_beg; a_ite2 != a_ite2_end; ++a_ite2)
            {
                index_file = (*a_ite2).first;
                if (cur_file_indx != index_file)
                    continue;

                MvPositions_t& a_positions = (*a_ite2).second;
                MvPositions_t::iterator a_ite3_beg = a_positions.begin();
                MvPositions_t::iterator a_ite3_end = a_positions.end();
                for (MvPositions_t::iterator a_ite3 = a_ite3_beg; a_ite3 != a_ite3_end; ++a_ite3)
                {
                    CKeywordData& a_pos = (*a_ite3);
                    if (a_pos.GetLocation() == cur_loc)
                    {
                        a_pos.SetLoadedFlag(true);
                        do_continue = false;
                        break;
                    }
                }
                if (!do_continue)
                    break;
            }
        }
    }
}



bool MECIReadModelBase::ReadKeyword(MECIModelFactory *model_p,const char *keyword) {

    MvHeaderPositions_t           &a_header_positions = (*((MvHeaderPositions_t *)myHeaderPositionsPtr));
    MvComponentFilePositions_t* a_comp_file_pos = NULL;

    a_comp_file_pos = a_header_positions.findType(keyword);
    int index_component=-1;
    int index_file=-1;
    MECReadFile* currentFile= NULL;
    if(a_comp_file_pos==NULL)
        return false;

    if(a_comp_file_pos)
    {
        MvComponentFilePositions_t::const_iterator a_ite_beg = a_comp_file_pos->begin();
        MvComponentFilePositions_t::const_iterator a_ite_end = a_comp_file_pos->end();
        for(MvComponentFilePositions_t::const_iterator a_ite = a_ite_beg; a_ite!=a_ite_end; ++a_ite)
        {
            // set the component index
            index_component = (*a_ite).first;
            PushComponentIndex(index_component);
            // set the offset
            MECComponent* a_comp_p = NULL;
            MECOffset* a_offset_p = NULL;
            a_comp_p = GetComponent(index_component);
            if(a_comp_p)
                a_offset_p=(MECOffset*)(a_comp_p->GetGlobalOffset());
            if(a_offset_p)
                model_p->SetCurrentOffset(a_offset_p);

            const MvFilePositions_t& a_file_pos=(*a_ite).second;
            MvFilePositions_t::const_iterator a_ite2_beg = a_file_pos.begin();
            MvFilePositions_t::const_iterator a_ite2_end = a_file_pos.end();
            for(MvFilePositions_t::const_iterator a_ite2 = a_ite2_beg; a_ite2!=a_ite2_end; ++a_ite2)
            {
                index_file  = (*a_ite2).first;		

                const MvPositions_t& a_positions=(*a_ite2).second;
                MvPositions_t::const_iterator a_ite3_beg = a_positions.begin();
                MvPositions_t::const_iterator a_ite3_end = a_positions.end();
                for(MvPositions_t::const_iterator a_ite3 = a_ite3_beg; a_ite3!=a_ite3_end; ++a_ite3)
                {
                    const CKeywordData&  a_pos =(*a_ite3);

                    if (a_pos.GetLoadedFlag())
                        continue;

                    SetHierarchyFile(index_file);
                    currentFile=GetFile(index_file);
                    currentFile->SetCurrentLine(a_pos.GetLine());
                    currentFile->SetCurrentLocation(a_pos.GetLocation());
                    
                    //MECSubdeck::curTmpIdx = a_pos.GetSubdeckIdx();
                    setCurrentSubdeckIndex(a_pos.GetSubdeckIdx());
                    const InputInfos::IdentifierValuePairList* keywordcomments=NULL;
                    a_pos.GetCommentData(&keywordcomments);
                    ManageReadKeyWord( model_p, keyword, a_pos.GetHeaderInfo(), a_pos.GetLineCount(), keywordcomments);
                }
            }
        }
        model_p->PreTreatObject(keyword);

        if (model_p->isPushInterface())
        {
            model_p->AddObjects(keyword);
            model_p->PostTreatObject(keyword);
        }
    }
    return true;
}




void MECIReadModelBase::ManageReadKeyWord( MECIModelFactory* model_p, const char* keyword, const CUserNameTypeInfo* headerdata, int linecount,
                                            const InputInfos::IdentifierValuePairList* keywordData)
{

    
    IMECPreObject* a_preobj = nullptr;
    vector<IMECPreObject*> preobj_lst;
    if (myInputInfosPtr->IsSupportedForContinueReadWithoutHeader())
    {
        readCurrentObjects(model_p, preobj_lst, nullptr, headerdata, linecount);
    }
    else
    {
        a_preobj = readCurrentObjects(model_p, nullptr, headerdata, linecount);
        if (a_preobj)
        {
            preobj_lst.push_back(a_preobj);
            //cout << a_preobj->GetReport();
        }
    }
    const char* begin_key = getBeginKeyword();
    ApplicationMode_e mode = (ApplicationMode_e)getAppMode();
    ISyntaxInfos* psyninfo = GetSyntaxInfo();
    const char* begin_proc_key = psyninfo->getNormalisedHeader(begin_key);
    char subtype[100];
    if (begin_proc_key)
    {
        sprintf(subtype, "%s/%s", "/CARD", begin_proc_key);
    }
    for (int i = 0; i < preobj_lst.size(); i++)
    {
        if (i == 0)
            model_p->StorePreObject(headerdata->obj_type, preobj_lst[i], keywordData);//only for first 
        else
            model_p->StorePreObject(headerdata->obj_type, preobj_lst[i]);

        //handling of default title if not read
        const char* title_p = preobj_lst[i]->GetTitle();
        if (headerdata->pdescrp && (!title_p || (title_p && title_p[0] == '\0')))
        {
            string title = headerdata->pdescrp->getKeyword();
            MYOBJ_INT id = preobj_lst[i]->GetId();
            if (0 < id && title != "")
            {
                title = title + string("_") + std::to_string(id);
            }
            if (title != "")
            {
                preobj_lst[i]->SetTitle(title.c_str());
            }
        }

        if (headerdata->obj_type == HCDI_OBJ_TYPE_CARDS)
        {
            if (strcmp(preobj_lst[i]->GetKernelFullType(), subtype) == 0)
            {
                string type_skey = GetAttribNameFromDrawable(headerdata->pdescrp, cdr::g_AttribFormatType);
                int ikw = headerdata->pdescrp->getIKeyword(type_skey);
                if (ikw != END_ARGS)
                {
                    // for /CARD/KEYWORD, store format type information
                    int file_index = preobj_lst[i]->GetFileIndex();
                    int type = preobj_lst[i]->GetIntValue(type_skey.c_str());
                    io_types::format_type_e fmt_type = (io_types::format_type_e)type;
                    psyninfo->updateFormatType(file_index, fmt_type);
                }

                string version_skey = GetAttribNameFromDrawable(headerdata->pdescrp, cdr::g_AttribFileVersion);
                ikw = headerdata->pdescrp->getIKeyword(version_skey);
                if (ikw != END_ARGS)
                {
                    // for /CARD/BEGIN, store version information
                    int version = preobj_lst[i]->GetIntValue(version_skey.c_str());
                    MECReadFile* file = GetCurrentFilePtr();
                    if (version)
                    {
                        string version_str = std::to_string(version);

                        MvFileFormat_e aformat_enum = CFGKernelGetFileFormatFromString(mode, version_str);
                        file->SetVersion(aformat_enum);
                    }
                }
            }
        }
    }

    if (a_preobj && headerdata && headerdata->obj_type == HCDI_OBJ_TYPE_ENCRYPTIONS)
    {
        //need to get keyid, ndata, data from DRAWABLES

        const char *key = a_preobj->GetStringValue("keyid");
        if (key)
        {
            std::string keystrid(key);
            std::string str_list;
            int ndata = a_preobj->GetIntValue("datamax");
            int attrib_ind = a_preobj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, "data");
            if (attrib_ind >= 0)
            {
                for (int i = 0; i < ndata; i++)
                {
                    str_list.append(a_preobj->GetStringValue(attrib_ind, i));
                }
            }
            model_p->AddCryptingKey(keystrid.c_str(), str_list.c_str(), "", "", "", "", headerdata->myusername.c_str());
        }
    }


    //vector<IMECPreObject*> preobj_lst;
    //readCurrentObjects(model_p, "", preobj_lst, nullptr, headerdata);

    //for (int i = 0; i < preobj_lst.size(); i++)
    //{
    //    model_p->AddObject(*(preobj_lst[i]), keywordData);
    //}
}

void MECIReadModelBase::readObjects(MECIModelFactory *model_p,const char *keyword,const char *otype) 
{
    //initReadObjects(model_p, keyword, otype);
    if (ReadKeyword(model_p, keyword))
        displayMessage(MSG_MESSAGE, getMsg(4), keyword);

    closeReadObjects(model_p, keyword, otype);
}



/* --------- File management (stack of opened files) --------- */

MECReadFile *MECIReadModelBase::openFile(const char *full_name, obj_type_e file_type) {

    MECReadFile* a_cur_file_p = NULL;
    if(myFileFactoryPtr) a_cur_file_p = myFileFactoryPtr->open(full_name);
    //
    if (a_cur_file_p != NULL && a_cur_file_p->isOpen())
    {
        int a_current_file_index = GetCurrentFileIndex();
        a_cur_file_p->SetParentIndex(a_current_file_index);
        int a_current_component_index = GetCurrentComponentIndex();
        a_cur_file_p->SetComponentIndex(a_current_component_index);

        _HC_LONG a_current_loc = GetCurrentLocation();
        _HC_LONG a_current_line = getCurrentLine();
        a_cur_file_p->SetParentLocation(a_current_loc);
        a_cur_file_p->SetParentLine(a_current_line);

        this->PushFile(a_cur_file_p);
        //if(file_type != HCDI_OBJ_TYPE_INCLUDES)

        ISyntaxInfos* psyninfo = GetSyntaxInfo();

        io_types::format_type_e fmt_type = psyninfo->getFormatType(a_cur_file_p);

        newDataReader()->setLineLength(psyninfo->getLineLength());

        new MECSubdeck(full_name, file_type, a_current_component_index, 0, 0, nullptr, myInputInfosPtr->GetIncludeKeyword(), psyninfo == nullptr ? nullptr :psyninfo->getHeaderLst(), fmt_type);
    }
    else
    {
        displayCurrentLocation(MSG_ERROR);
        displayMessage(MSG_ERROR,getMsg(16),full_name);
    }
    //
    return a_cur_file_p;
}
MECReadFile *MECIReadModelBase::updateFile(MECReadFile* a_cur_file_p) {

    if(a_cur_file_p!=NULL && a_cur_file_p->isOpen())
    {
        
        int a_current_file_index=GetCurrentFileIndex(); 
        a_cur_file_p->SetParentIndex(a_current_file_index);
        int a_current_component_index = GetCurrentComponentIndex();
        a_cur_file_p->SetComponentIndex(a_current_component_index);

	  _HC_LONG a_current_loc = GetCurrentLocation();
	  _HC_LONG a_current_line = getCurrentLine();
        a_cur_file_p->SetParentLocation(a_current_loc);
        a_cur_file_p->SetParentLine(a_current_line);

        this->PushFile(a_cur_file_p);
    }
    else
    {
        //displayCurrentLocation(MSG_ERROR);
        //displayMessage(MSG_ERROR,getMsg(16),full_name);	  
    }
    //
    return a_cur_file_p;
}

void MECIReadModelBase::closeFile() 
{
    // close current file
    MECReadFile* a_cur_file=GetCurrentFilePtr();
    if(a_cur_file!=NULL)
        a_cur_file->close();
    // pop file in stack
    PopFile();

    
    MECSubdeck::PopSubDeck();
}

bool MECIReadModelBase::eof() const {
    
    MECReadFile* a_cur_file=GetCurrentFilePtr();
    if(a_cur_file!=NULL)
        return	a_cur_file->eof();
    return true;
    
}

int MECIReadModelBase::getNbCurrentFiles() const {
    const LocFileVect_t &a_cur_files=(*((LocFileVect_t *)myFileVectPtr));
    //
    return (int)(a_cur_files.size());
}

MECReadFile *MECIReadModelBase::GetCurrentFilePtr() const { 
    
    int current_index = GetCurrentFileIndex();
    LocFileVect_t &a_cur_file_vect=(*((LocFileVect_t*)myFileVectPtr));
    if(current_index<a_cur_file_vect.size())
        return a_cur_file_vect[current_index];
    return NULL;
    
}

const char *MECIReadModelBase::getCurrentFullName() const {
    
    MECReadFile          *a_cur_file_p = GetCurrentFilePtr();
    if(a_cur_file_p!=NULL)
        return a_cur_file_p->GetFullName();
    return("");
    
}


_HC_LONG MECIReadModelBase::GetCurrentLocation() const {

    MECReadFile          *a_cur_file_p = GetCurrentFilePtr();
    if(a_cur_file_p!=NULL)
        return a_cur_file_p->GetCurrentLocation();
    return -1;
}


_HC_LONG MECIReadModelBase::getCurrentLine() const {
    
    MECReadFile          *a_cur_file_p = GetCurrentFilePtr();
    if(a_cur_file_p!=NULL)
        return a_cur_file_p->GetCurrentLine();
    return (unsigned long)(-1); //to avoid to redeclare MAXINT
    
}


_HC_LONG MECIReadModelBase::GetPreviousLocation() const {

    MECReadFile          *a_cur_file_p = GetCurrentFilePtr();
    if(a_cur_file_p!=NULL)
        return a_cur_file_p->GetPreviousLocation();
    return -1;
}



_HC_LONG MECIReadModelBase::GetPreviousLine() const {

    MECReadFile          *a_cur_file_p = GetCurrentFilePtr();
     if(a_cur_file_p!=NULL)
        return a_cur_file_p->GetPreviousLine();
    return (unsigned long)(-1); //to avoid to redeclare MAXINT
}



void MECIReadModelBase::StoreCurrentPositionType(const char* a_type, InputInfos::IdentifierValuePairList& keywrodcommentdata)
{
    MvHeaderPositions_t &a_header_positions = (*((MvHeaderPositions_t *)myHeaderPositionsPtr));
    string a_type_keyword(a_type);
    int a_current_include_file_index = GetCurrentFileIndex();
    int a_current_component_index = GetCurrentComponentIndex();
	_HC_LONG a_before_read_line = GetPreviousLine();
	_HC_LONG a_before_read_loc = GetPreviousLocation();
    MECReadFile* current_file_p = GetFile(a_current_include_file_index);
    a_header_positions.setTypeString(a_type_keyword);
    a_header_positions.setComponentIndex(a_current_component_index);
    a_header_positions.setFileIndex(a_current_include_file_index);
    a_header_positions.addKeywordData(a_before_read_line, a_before_read_loc,MECSubdeck::curSubdeckIdx, "",keywrodcommentdata, nullptr, 0 );

}
void MECIReadModelBase::ClearAllPrevoiusPositionTypes()
{
	MvHeaderPositions_t &a_header_positions = (*((MvHeaderPositions_t *)myHeaderPositionsPtr));
    a_header_positions.ClearAllComponentFilePosition();
}



MECReadFile* MECIReadModelBase::GetFile(int an_index)
{
    MECReadFile* a_read_file=NULL;
    LocFileVect_t& a_cur_file_vect=*((LocFileVect_t*)myFileVectPtr);
    if(an_index<a_cur_file_vect.size())
        a_read_file=a_cur_file_vect[an_index];
    return a_read_file;
}



MECComponent* MECIReadModelBase::GetComponent(int an_index)
{
    MECComponent* a_component=NULL;
    LocComponentVect_t &a_cur_comp_vect=*((LocComponentVect_t*)myComponentVectPtr);
    if(an_index<a_cur_comp_vect.size())
        a_component=a_cur_comp_vect[an_index];
    return a_component;
}



int   MECIReadModelBase::SearchFile(const char* full_name) const
{
    LocFileVect_t& a_cur_file_vect=*((LocFileVect_t*)myFileVectPtr);
    int nb_files = (int)a_cur_file_vect.size();
    if(nb_files<1)
        return -1;
    int a_curr_index = GetCurrentFileIndex();
	_HC_LONG a_curr_loc = GetCurrentLocation();
    for(int ifile=0; ifile<nb_files; ifile++)
    {
        MECReadFile* a_loop_file_p = a_cur_file_vect[ifile];
        if(strcmp(a_loop_file_p->GetFullName(),full_name)==0)
        {
            int indexParent = a_loop_file_p->GetParentIndex();
            _HC_LONG posParent = a_loop_file_p->GetParentLocation();
            if(a_curr_index == indexParent && a_curr_loc==posParent)
                return ifile;
        }
    }
    return -1;
}


// This functions open all the file parent of target file
// And close all the other files
void MECIReadModelBase::SetHierarchyFile(int target_index)
{	

    int curr_index = GetCurrentFileIndex();

    if(curr_index == target_index) return;

    MECReadFile* a_target_file_p = GetFile(target_index);

    // get all the parent of current file
    LocIntSet_t parents_current;
    parents_current.insert(curr_index);
    MECReadFile* a_cur_file_p = GetCurrentFilePtr();

    int a_parent_index=-1;
    MECReadFile* a_file_loop_p = a_cur_file_p;
    bool do_continue=true;
    while(do_continue)
    {
        if(a_cur_file_p)
            a_parent_index=a_file_loop_p->GetParentIndex();
        if(a_parent_index!=-1)
        {
            parents_current.insert(a_parent_index);
            a_file_loop_p=GetFile(a_parent_index);
        }
        else
            do_continue=false;
    }

    LocIntSet_t a_commun_parents;


    // get all the parent of target file
    LocIntSet_t parents_target;
    parents_target.insert(target_index);

    a_parent_index=-1;
    do_continue=true;
    a_file_loop_p = a_target_file_p;
    while(do_continue)
    {
        if(a_target_file_p)
            a_parent_index=a_file_loop_p->GetParentIndex();
        if(a_parent_index!=-1)
        {
            parents_target.insert(a_parent_index);
            a_file_loop_p=GetFile(a_parent_index);
        }
        else
            do_continue=false;
    }

    // get the intersection of the set = all the commun parent	
    inter(parents_current, parents_target, &a_commun_parents);

    // close all the files until first parent in a_commun_parents
    // from currentFile

    a_file_loop_p = GetCurrentFilePtr();
    int index_first_commun_parent =curr_index;
    if(!(curr_index<a_commun_parents))
    {
        if(a_file_loop_p)
            do_continue=true;
        while(do_continue)
        {
            a_parent_index=a_file_loop_p->GetParentIndex();
            if(a_parent_index!=-1)
            {
                closeFile();
                if(a_parent_index<a_commun_parents)
                {
                    index_first_commun_parent=a_parent_index;
                    do_continue = false;
                }
                a_file_loop_p=GetCurrentFilePtr();
            }
            else
                do_continue=false;
        }
    }

    // until parent is index_first_commun_parent
    // push the file index from target to index_first_commun_parent in a stack
    do_continue=true;
    int loop_file_index = target_index;
    LocIntStack_t a_stack;

    if(index_first_commun_parent==target_index)
        do_continue=false;
    while(do_continue)
    {
        a_stack.push(loop_file_index);
        a_file_loop_p = GetFile(loop_file_index);
        loop_file_index=a_file_loop_p->GetParentIndex();
        if(loop_file_index==index_first_commun_parent)
            do_continue=false;
    }

    // push all the value of the stack
	_HC_LONG last_parent_loc = -1;
    while(a_stack.size()>0)
    {
        loop_file_index = a_stack.top();
        a_file_loop_p = GetFile(loop_file_index);
        a_file_loop_p->open();
        PushFileIndex(loop_file_index);
		_HC_LONG last_parent_line = a_file_loop_p->GetParentLine();
        last_parent_loc = a_file_loop_p->GetParentLocation();
        int ind_parent_loop = a_file_loop_p->GetParentIndex();
        if(last_parent_loc!=-1 && ind_parent_loop!=-1)
        {
            MECReadFile* a_parent_loop_file_p = GetFile(ind_parent_loop);
            a_parent_loop_file_p->SetCurrentLocation(last_parent_loc);
            a_parent_loop_file_p->SetCurrentLine(last_parent_line);
        }
        a_stack.pop();
    }
}


//void MECIReadModelBase::AddIncludeObjects(MECIModelFactory *model_p)
//{
//    model_p->AddIncludeObjects();
//}

//void MECIReadModelBase::AddSubmodelObjects(MECIModelFactory *model_p)
//{
//    model_p->AddSubmodelObjects();
//}

void MECIReadModelBase::AddSubDecks(MECIModelFactory *model_p)
{
    model_p->AddSubDeckObjects();
}


int MECIReadModelBase::GetCurrentFileIndex() const
{
    LocIntStack_t &a_cur_file_stack=(*((LocIntStack_t*)myFileIndexStackPtr));
    int a_value = -1;
    int a_size = (int)a_cur_file_stack.size();
    if(a_size==1)
        a_value =  0 ;
    else if(a_size>1)
        a_value =  a_cur_file_stack.top();
    return a_value;
}



void MECIReadModelBase::SetCurrentFileIndex(int an_index) 
{
    LocIntStack_t &a_cur_file_stack=(*((LocIntStack_t*)myFileIndexStackPtr));
    if(a_cur_file_stack.size()>0)
        a_cur_file_stack.pop();
    a_cur_file_stack.push(an_index);
}




int MECIReadModelBase::GetCurrentComponentIndex() const
{
    LocIntStack_t &a_cur_comp_stack=(*((LocIntStack_t*)myComponentIndexStackPtr));
    int a_value = -1;
    int a_size = (int)a_cur_comp_stack.size();
    if(a_size==1)
        a_value =  0 ;
    else if(a_size>1)
        a_value =  a_cur_comp_stack.top();
    return a_value;
}



void MECIReadModelBase::SetCurrentComponentIndex(int an_index) 
{
    LocIntStack_t &a_cur_component_stack=(*((LocIntStack_t*)myComponentIndexStackPtr));
    if(a_cur_component_stack.size()>0)
        a_cur_component_stack.pop();
    a_cur_component_stack.push(an_index);
}



void MECIReadModelBase::setCurrentSubdeckIndex(int an_index) 
{
    myCurSubdeckIndex = an_index;
}

int MECIReadModelBase::getCurrentSubdeckIndex() 
{
    return myCurSubdeckIndex;
}



void MECIReadModelBase::PushFile(MECReadFile* a_read_file)
{
    // insert the file in myFileVectPtr
    LocFileVect_t &a_cur_file_vect=(*((LocFileVect_t*)myFileVectPtr));
    a_cur_file_vect.push_back(a_read_file);

    // increment and push index in the index stack
    PushFileIndex(myIndexNewFile);
    myIndexNewFile++;
}



MECReadFile* MECIReadModelBase::PopFile()
{
    // get the last index
    LocIntStack_t &a_cur_file_stack=(*((LocIntStack_t*)myFileIndexStackPtr));
    int stack_size = (int)a_cur_file_stack.size();
    if(stack_size>0)
    {
        a_cur_file_stack.pop();
        stack_size--;
        if(stack_size>0)
        {
            int last_index = a_cur_file_stack.top();
            // get the file of such index
            LocFileVect_t &a_cur_file_vect=(*((LocFileVect_t*)myFileVectPtr));
            if(last_index<a_cur_file_vect.size())
                return a_cur_file_vect[last_index];
        }
    }
    //
    return NULL;
}




void MECIReadModelBase::PushFileIndex(int an_index)
{
    LocIntStack_t &a_cur_file_stack=(*((LocIntStack_t*)myFileIndexStackPtr));
    a_cur_file_stack.push(an_index);
}



void MECIReadModelBase::PushComponent(MECComponent* a_comp_p)
{
    // add Offset;
    int a_parent_index=a_comp_p->GetParentIndex();
    if(a_parent_index!=-1)
        a_comp_p->InitGlobalOffset(GetComponent(a_parent_index));

    a_comp_p->ComputeGlobalOffset();

    // insert the component in myComponentVectPtr
    LocComponentVect_t &a_cur_component_vect=(*((LocComponentVect_t*)myComponentVectPtr));
    a_cur_component_vect.push_back(a_comp_p);

    // increment and push index in the index stack
    PushComponentIndex(myIndexNewComponent);
    myIndexNewComponent++;
}




MECComponent* MECIReadModelBase::PopComponent()
{
    // get the last index
LocIntStack_t& a_cur_component_stack = (*((LocIntStack_t*)myComponentIndexStackPtr));
int stack_size = (int)a_cur_component_stack.size();
if (stack_size > 0)
{
    a_cur_component_stack.pop();
    stack_size--;
    if (stack_size > 0)
    {
        int last_index = a_cur_component_stack.top();
        // get the file of such index
        LocComponentVect_t& a_cur_component_vect = (*((LocComponentVect_t*)myComponentVectPtr));
        if (last_index < a_cur_component_stack.size())
            return a_cur_component_vect[last_index];
    }
}
//
return NULL;
}



void MECIReadModelBase::PushComponentIndex(int an_index)
{
    LocIntStack_t& a_cur_component_stack = (*((LocIntStack_t*)myComponentIndexStackPtr));
    a_cur_component_stack.push(an_index);
}


/* --------- Parsing --------- */

char* MECIReadModelBase::ReadBuffer(bool do_check_eof, int nb_chars, bool skip_comment)
//++///////////////////////////////////////////////////////////////////////////
//
//Function Descriptions:
//
//	This function is used to read one line from the file to the buffer.
//Parameters:
//
//	bool do_check_eof: [in]
//
//      int nb_chars: keyword [out]
//
//Return value:
//
//      Char *: one line in the file
//
//Modification History:
//

//	
//--/////////////////////////////////////////////////////////////////////////
{//Begin -- MECIReadModelBase::ReadBuffer

    
    myUnreadPositionPtr->SetLine(getCurrentLine());
    myUnreadPositionPtr->SetLocation(GetCurrentLocation());
    myUnReadFileIndex = GetCurrentFileIndex();


    MECReadFile* a_cur_file_p = GetCurrentFilePtr(); //RAR#MGEN_DEV_2006_171#24_09_2006


    //
    // Reading the buffer
    //
    int  a_nb_chars = (nb_chars < 0 ? myBufferNbChars : nb_chars + 2);
    bool a_continue = true;
    while (a_continue)
    {
        // Managing end-of-file
        bool a_is_eof = (a_cur_file_p == NULL || a_cur_file_p->eof());
        if (a_is_eof)
        {
            
            if ((GetReadingStatus() == READ_STATUS_SINGLE_KEYWORD))
            {
                if (do_check_eof)
                {
                    displayCurrentLocation(MSG_ERROR);
                    displayMessage(MSG_ERROR, getMsg(7));
                }
                return NULL;
            }
            else
            {
                if (!do_check_eof)
                    return NULL;


                if (GetComponentState())
                {
                    SetComponentState(false);
                    PopComponent();
                }

                a_cur_file_p = PopFile();
                MECSubdeck::PopSubDeck(); 
                if (a_cur_file_p != NULL)
                {
                    myUnreadPositionPtr->SetLine(getCurrentLine());
                    myUnreadPositionPtr->SetLocation(GetCurrentLocation());
                    myUnReadFileIndex = GetCurrentFileIndex();
                    a_continue = true;
                }
                else
                {
                    if (do_check_eof)
                    {
                        displayCurrentLocation(MSG_ERROR);
                        displayMessage(MSG_ERROR, getMsg(7));
                    }
                    return NULL;
                }
            }
        }

        // Reading line
        
        bool a_error = (!a_cur_file_p->readBuffer(a_nb_chars, myLineBuffer));
        if (a_error)
        {
            a_continue = a_cur_file_p->eof();
            if (!a_continue)
            {
                if (GetComponentState() &&  GetReadingStatus() != READ_STATUS_SINGLE_KEYWORD) 
                {
                    SetComponentState(false);
                    PopComponent();
                    MECSubdeck::PopSubDeck();
                }
                return NULL;
            }
        }
        else
        {
            if (skip_comment == false)
                break;
            // Managing comments and include files
            char *a_full_name=NULL;
            char *a_relative_name=NULL;  //RAR#MGEN_DEV_2006_171#24_09_2006
            killBlanksNLEnd(myLineBuffer);
            bool  a_is_include = false;
            bool is_component = false;

            if (GetReadingStatus() != READ_STATUS_SINGLE_KEYWORD)
            {
                if (!mySkipIncludeReading)
                {
                    a_is_include = IsIncludedFile(myLineBuffer, &a_full_name, &a_relative_name);  //RAR#MGEN_DEV_2006_171#24_09_2006
                    is_component = IsComponent(myLineBuffer);
                }
            }
            //bool is_parameter = IsParameter(myLineBuffer);
            if(a_is_include && a_full_name && (GetReadingStatus() != READ_STATUS_SINGLE_KEYWORD))
            {
                //RAR#MGEN_DEV_2006_171#24_09_2006 (BEG)
                int a_find_index= SearchFile(a_full_name);
                if(a_find_index==-1)
                {
                    MECReadFile *a_new_file_p=openFile(a_full_name);
                    if (MECSubdeck::curSubdeckIdx < MECSubdeck::mySubdeckVector.size())
                    {
                        IMECPreObject* preobj = MECSubdeck::mySubdeckVector[MECSubdeck::curSubdeckIdx]->GetPreObject();
                        if (preobj)
                        {
                            IDescriptor* pdescrp = HCDI_GetDescriptorHandle(preobj->GetKernelFullType());
                            if (pdescrp)
                            {
                                string filemname_skey = GetAttribNameFromDrawable(pdescrp, cdr::g_AttribFileName);
                                preobj->AddStringValue(filemname_skey.c_str(), a_relative_name);
                            }
                        }
                    }

                    if(a_new_file_p!=NULL)
                    {
                        a_new_file_p->SetRelativeName(a_relative_name);
                       // MECSubdeck::mySubdeckVector.back()->SetFileRelativeName(a_relative_name);

                        a_cur_file_p=a_new_file_p;
                    }
                    else 
                        a_cur_file_p=GetCurrentFilePtr();

                    myUnreadPositionPtr->SetLine(getCurrentLine());
                    myUnreadPositionPtr->SetLocation(GetCurrentLocation());
                    myUnReadFileIndex = GetCurrentFileIndex();
                }
                else
                {
                    a_cur_file_p=GetFile(a_find_index);
                    a_cur_file_p->open();
                    PushFileIndex(a_find_index);
                }
                //set include file version if not present then set the original version
                a_cur_file_p->SetVersion(getVersion());//
                myfree(a_full_name);
                myfree(a_relative_name);
                //RAR#MGEN_DEV_2006_171#24_09_2006 (END)
                a_continue=true;
            }
            
            else if(is_component && (GetReadingStatus() != READ_STATUS_SINGLE_KEYWORD))
            {
                MECReadFile* a_new_file_p = NULL;
                bool a_component_flag = true;
                ManageComponent(myLineBuffer, a_component_flag , &a_new_file_p);
                if(a_new_file_p)
                    a_cur_file_p = a_new_file_p;
                a_continue = a_component_flag;
                myUnreadPositionPtr->SetLine(getCurrentLine());
                myUnreadPositionPtr->SetLocation(GetCurrentLocation());
                myUnReadFileIndex = GetCurrentFileIndex();
            }
            
            else
            {
                a_continue =  isComment(myLineBuffer);//if it's comment, skip to the next line
                if(a_continue) 
                {
                    
                    if(myDoRecord) RecordLine(myLineBuffer);
                    
                    manageComment(myLineBuffer);
                    a_cur_file_p   = GetCurrentFilePtr(); 
                    //	  a_cur_nb_files = getNbCurrentFiles();
                }
            }
        }
    }
    //
    
    if(myDoRecord && myLineBuffer!=NULL) RecordLine(myLineBuffer);
    return DecryptLine(myLineBuffer);
    

}//End -- MECIReadModelBase::ReadBuffer

void MECIReadModelBase::UnreadBuffer() { 
    
    if (GetCurrentFileIndex()!=myUnReadFileIndex)
        SetHierarchyFile(myUnReadFileIndex);
    MECReadFile* a_cur_file_p=GetCurrentFilePtr();
    if(a_cur_file_p)
    {
        a_cur_file_p->SetCurrentLine(myUnreadPositionPtr->GetLine());
        a_cur_file_p->SetCurrentLocation(myUnreadPositionPtr->GetLocation());
    }
    

    
    if(myDoRecord) {
        UnrecordLastLine();
        UnrecordLastComments(); 
    }
    
}

 /// Jump to a kw and return true if found
bool MECIReadModelBase::JumpToKw(const char* kw)
{
    MECReadFile *a_cur_file_p   = GetCurrentFilePtr();
    //
    char* buffer_p = ((char *)mymalloc(myBufferNbChars*sizeof(char)));
    char* kw_p = NULL;
    bool kw_found = false;
    bool do_continue = true;
    while(!kw_found && do_continue) 
    {
        // Managing end-of-file
        bool a_is_eof= (a_cur_file_p==NULL || a_cur_file_p->eof());
        if(a_is_eof)
            do_continue=false;
        // Reading line
        bool a_error=(!a_cur_file_p->readBuffer(myBufferNbChars,buffer_p));
        killBlanksNLEnd(buffer_p);
        bool  is_header = isHeader(buffer_p,&kw_p);
        if(is_header)
        {
                killBlanksNLEnd(kw_p);
                if ( strcmp(kw_p, kw)==0)
                    kw_found=true;
        }
    }
    if(buffer_p)
        myfree(buffer_p);
    //
    return kw_found;
}


/* --------- Crypting --------- */


char *MECIReadModelBase::DecryptLine(char *iline) {
    return iline;
}



/* --------- Recording --------- */


void MECIReadModelBase::StartRecording(bool do_clear_buffer) {
    myDoRecord=true;
    if(do_clear_buffer) ClearRecordBuffer();
}



const char *MECIReadModelBase::StopRecording() {
    myDoRecord=false;
    return GetRecordBuffer();
}



void MECIReadModelBase::ClearRecordBuffer() {
    LocRecordBuffer_t &a_record_buffer=(*((LocRecordBuffer_t *)myRecordBufferPtr));
    a_record_buffer.resize(0);
}



const char *MECIReadModelBase::GetRecordBuffer() const {
    const LocRecordBuffer_t &a_record_buffer=(*((const LocRecordBuffer_t *)myRecordBufferPtr));
    return a_record_buffer.c_str();
}



void MECIReadModelBase::RecordLine(const char *iline) {
    LocRecordBuffer_t &a_record_buffer=(*((LocRecordBuffer_t *)myRecordBufferPtr));
    a_record_buffer+=iline;
}



void MECIReadModelBase::UnrecordLastLine() {
    LocRecordBuffer_t &a_record_buffer = (*((LocRecordBuffer_t *)myRecordBufferPtr));
    int                a_pos           = GetLastRecordedLinePos();
    //
    a_record_buffer=a_record_buffer.substr(0,a_pos);
}



const char *MECIReadModelBase::GetLastRecordedLine() const {
    LocRecordBuffer_t &a_record_buffer = (*((LocRecordBuffer_t *)myRecordBufferPtr));
    int                a_pos           = GetLastRecordedLinePos();
    //
    return a_record_buffer.c_str()+a_pos;
}



int MECIReadModelBase::GetLastRecordedLinePos() const {
    LocRecordBuffer_t &a_record_buffer = (*((LocRecordBuffer_t *)myRecordBufferPtr));
    int                a_pos           = (int)(a_record_buffer.length()-2);
    //
    if(a_pos<0) a_pos=0; // in case a_record_buffer.length()<2
    while(a_pos>0 && a_record_buffer[a_pos]!='\n') --a_pos;
    if(a_record_buffer.length()>0 && a_record_buffer[a_pos]=='\n') ++a_pos;
    //
    return a_pos;
}



void MECIReadModelBase::UnrecordLastComments() {
    bool a_is_comment=true;
    while(a_is_comment) {
        const char *a_last_line=GetLastRecordedLine();
        a_is_comment=isComment(a_last_line);
        if(a_is_comment) UnrecordLastLine();
    };
}



/* --------- Messages --------- */

const char *MECIReadModelBase::getMsg(int ind) const {
    return MV_get_msg_array(MSGT_MODEL_IO)[ind];
}

void MECIReadModelBase::displayCurrentLocation(MyMsgType_e msg_type) const {
    const char    *a_cur_full_name = getCurrentFullName(); 
  _HC_LONG  a_cur_line      = getCurrentLine();     
    //
    displayMessage(msg_type,getMsg(0),a_cur_line,a_cur_full_name);
}
