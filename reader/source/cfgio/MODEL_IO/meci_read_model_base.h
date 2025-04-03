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


#ifndef MECI_READ_MODEL_BASE_H
#define MECI_READ_MODEL_BASE_H

#include "mec_read_file.h"
#include "meci_model_factory.h"

#include "meci_input_infos.h"
#include "mec_component.h"
#include "meci_read_context.h"
#include "mec_position.h"
#include "meci_syntax_infos.h"
#include "meci_data_reader.h"
#include "hcio.h"
#include <MESSAGE/msg_types.h>
#define PseudoLocFileVect_t         void 

#define PseudoLocComponentVect_t    void 

#define PseudoIntStack_t            void 

#define PseudoLocFileVect_t         void 
#define PseudoHeaderPositions_t     void
#define PseudoRecordBuffer_t        void 
#define PseudoFileList_t            void 



/// Base class for reading models
class HCIO_DATA_DLL_API MECIReadModelBase :public MECIReadContext {


public: /** @name Constructors, destructor, re-configuring */
  //@{
  /// Constructor
  MECIReadModelBase(const char *full_name,int buffer_nb_chars,int line_nb_chars,
                    const ReadFileFactorySP& factory_p=nullptr);
  MECIReadModelBase(MECReadFile *file,int buffer_nb_chars,int line_nb_chars,
                    const ReadFileFactorySP& factory_p=nullptr);

  /// Destructor
  virtual ~MECIReadModelBase();

  /// Sets Line Number of Chars (if reading driven from outside)
  void SetLineNbChars(int line_nb_chars);
  //@}

public: /** @name Reading model (public) */
  //@{
  /// Reads a model
    virtual void readModel(MECIModelFactory *model_p, bool do_transform=true);

    virtual void readCurrentObjects(vector<IMECPreObject *> &preobj_lst, char *header_card, int linecount=0) { return ; }
    virtual IMECPreObject* readCurrentObjects(MECIModelFactory* model_p, char* header_card = NULL, const CUserNameTypeInfo* headerdata = nullptr, int linecount=0) { return nullptr; }
  //@}
    string getHeaderDetails() const;

    virtual void updateSubKeywordRead(const char* keyword) const;

protected: /** @name Reading model (protected) */
  //@{
  /// Reads the positions of headers
    virtual void readHeaderPositions(MECIModelFactory* model_p) { }
  /// Displays the positions of headers
  void displayHeaderPositions() const;
  /// Reading all the control cards
  //virtual void readControlCards(MECIModelFactory *model_p, bool isengine = false); 
    /// Reading a keyword
    bool ReadKeyword(MECIModelFactory *model_p,const char *keyword);
  /// Reading the current control card
  //virtual void readCurrentControlCard(MECIModelFactory *model_p,const char *keyword)=0;
  /// Initializations before reading objects
  virtual void initReadObjects(MECIModelFactory *model_p,const char *keyword,const char *otype)=0;
  /// Post-treatments after reading objects
  virtual void closeReadObjects(MECIModelFactory* model_p, const char* keyword, const char* otype) { }
  /// Reading the current objects (current header)
  virtual void readCurrentObjects(MECIModelFactory* model_p, std::vector<IMECPreObject*>& preobj_lst,
      char* header_card = NULL, const CUserNameTypeInfo* headerdata = nullptr, int linecount=0) {  }
  /// Reading all the objects of given type
  void readObjects(MECIModelFactory *model_p, const char *keyword,const char *otype);
    /// Sets reading status (internal)

    inline void SetReadingStatus(int a_stat) 
    {
        //if(myReadingStatus != READ_STATUS_SINGLE_KEYWORD)
        myReadingStatus = a_stat;
    } 

    /// Gets reading status (internal)

    inline int  GetReadingStatus() {return myReadingStatus;}            
    
    /// Gets Line Number of Chars (internal) 
    inline int  GetLineNbChars() {return myLineNbChars;}  
    
    

    void ManageReadKeyWord(MECIModelFactory* model_p, const char * keyword, const CUserNameTypeInfo* headerdata=nullptr, int linecount=0, const InputInfos::IdentifierValuePairList* keywordData = NULL); 

    virtual MvFileFormat_e getVersion() { return FF_UNKNOWN; }

  //@}

protected: /** @name File management (stack of opened files) */
  //@{
  /// Opening a new file (and pushing it)
  MECReadFile *openFile(const char *full_name, obj_type_e file_type = HCDI_OBJ_TYPE_INCLUDEFILES);
  /// Closing a new file (and poping it)
  void closeFile();
  /// Pretreat the closing of a file
  virtual bool pretreatCloseFile()=0;  
  
  bool eof() const;
  /// Gets the number of opened files (size of the stack)
  int getNbCurrentFiles() const;
protected:
  /// Gets the current full name
    const char *getCurrentFullName() const; 
    /// Gets the current location
    _HC_LONG GetCurrentLocation() const;             
  /// Gets the current line
    _HC_LONG getCurrentLine() const;   
    /// Gets the previous location (before last read)
    _HC_LONG GetPreviousLocation() const;             

    /// Gets the previous line (before last read)
    _HC_LONG GetPreviousLine() const;   


    /// Gets the current file index

    int GetCurrentFileIndex() const;		     

    /// Sets the current file index

    void SetCurrentFileIndex(int an_index);    

    /// Gets the current component index

    int GetCurrentComponentIndex() const;      

    /// Sets the current component index

    void SetCurrentComponentIndex(int an_index);

    /// Pushes the file

    void PushFile(MECReadFile* a_file);        

    /// Pushes the component

    void PushComponent(MECComponent* a_comp); 

    /// Pop last file

    MECReadFile* PopFile();                   

    /// Pop last component

    MECComponent* PopComponent();                   

    /// Pushes the file index

    void PushFileIndex(int an_index);         

    /// Pushes the file index

    void PushComponentIndex(int an_index);    

    /// Stores position (line and position) for a given type, a component index and a file index

    void StoreCurrentPositionType(const char* a_type, InputInfos::IdentifierValuePairList& keywrodcommentdata);   
    
    void ClearAllPrevoiusPositionTypes();
    /// Gets the file by his index

    MECReadFile* GetFile(int index);          

    /// Gets the component by his index

    MECComponent* GetComponent(int index) ;    

    /// Gets component state

    virtual bool GetComponentState() { return false; }
    virtual void SetComponentState(bool state) { }
    /// Search a file with his full_name 

    int  SearchFile(const char* full_name) const;  

    /// Sets a hierarchy of file : open all parent of index_file, and close the others

    void SetHierarchyFile(int index_file);     

    virtual void AddIncludeObjects(MECIModelFactory* model_p) { }
    virtual void AddSubmodelObjects(MECIModelFactory* model_p) { }
    
    

    virtual void AddSubDecks(MECIModelFactory* model_p);
    
    
    void setCurrentSubdeckIndex(int an_index);
    int getCurrentSubdeckIndex();
    
  //@}

protected: /** @name Parsing */
  //@{

  /// Reads a line
    char *ReadBuffer(bool do_check_eof=true,int nb_chars=-1, bool skip_comment = true); 
  /// Goes back to the previous line
   void UnreadBuffer();
   /// Jump to a kw and return true if found
   bool JumpToKw(const char* kw);  
   virtual char *readBuffer(bool do_check_eof=true,int nb_chars=-1, bool skip_comment = true){ return ReadBuffer(do_check_eof,nb_chars, skip_comment);}

   virtual void unreadBuffer(){ UnreadBuffer(); }



    /// Save a position by pushing it on a stack of positions, in order to get back to it with popPosition().
  virtual void pushPosition()=0;
    /// Get back to a saved position by popping it from the stack of positions filled with pushPosition().
  virtual void popPosition()=0;


  
  virtual bool IsIncludedFile(const char* buffer, char** full_name_p, char** relative_name_p) { return false; }

  /// Manages a comment
  virtual void manageComment(const char *buffer)=0;

  /// Returns true, if the line is commented
  virtual bool isComment(const char *buffer) const=0;
  /// Returns true if the line is a header
  virtual bool isHeader(const char *buffer,char **keyword_p=NULL) const=0;

    /// Returns true if a component must be read

    virtual bool IsComponent(const char *buffer) const=0; 

    /// Read a component

    virtual MECComponent* ManageComponent(const char* buffer, bool& continue_flag, MECReadFile** new_file_p = NULL) { return nullptr; }

  //@}

protected: /** @name Header Keywords */
  //@{
  
  inline const char *getNormalizedKeyword(const char *keyword) const {
    return myInputInfosPtr->GetNormalizedKeyword(keyword);
  }
  /// Returns true if the keyword of the header means end-of-file
  inline bool isEofKeyword(const char *keyword) const {
    return myInputInfosPtr->IsEofKeyword(keyword);
  }
  /// Gets the keywords and object types of the elements
  inline PseudoObjectKeywords_t *getElementKeywords(PseudoObjectKeywords_t *elt_keywords_p=NULL) const {
    return myInputInfosPtr->GetElementKeywords(elt_keywords_p);
  }
  inline const char* getBeginKeyword() const {
    return myInputInfosPtr->GetBeginKeyword();
  }
  inline unsigned int getAppMode() const {
    return myInputInfosPtr->GetAppMode();
  }
  //@}  

protected:/** @name Object counters */
  //@{
    virtual void updateCounters(const char* keyword, int nb_lines, int comp_index, const char* buffer = NULL) { }
  //@}

  
protected: /** @name Crypting */
  //@{
  /// Sets the crypting method
  inline void SetCryptingMethod(int crypting_method) { myCryptingMethod=crypting_method; }
  /// Sets the crypting method
  inline int  GetCryptingMethod() const { return myCryptingMethod; }
  /// Crypts the input line
  virtual char *DecryptLine(char *iline);
  //@}
  

  
protected: /** @name Recording */
  //@{
  /// Starts recording
  void  StartRecording(bool do_clear_buffer=false);
  /// Stops recording
  const char *StopRecording();
  /// Clears the record buffer
  virtual void  ClearRecordBuffer();
  /// Clears the record buffer
  const char *GetRecordBuffer() const;
  /// Recording a line
  void RecordLine(const char *iline);
  
  /// Deleting last recorded line
  void UnrecordLastLine();
  /// Getting the last recorded line
  const char *GetLastRecordedLine() const;
  /// Getting the last recorded line
  int GetLastRecordedLinePos() const;
  /// Removing last rerorded comments
  void UnrecordLastComments();

  PseudoLocFileVect_t* GetFilePtrLst() { return myFileVectPtr; }
  PseudoIntStack_t* GetComponentIndexStackPtr() { return myComponentIndexStackPtr; }
  PseudoIntStack_t* GetFileIndexStackPtr() { return  myFileIndexStackPtr; }
protected:
  /// Gets the message of the given index
  const char *getMsg(int ind) const;

public: /** @name Messages */
  //@{
  /// Displaying current line and file (as message)
  virtual void displayCurrentLocation(MyMsgType_e msg_type) const;
  //@}

    /// Reading status 
    typedef enum MyReadingStatus_s 
    {
        READ_STATUS_UNKNOWN, 
        READ_STATUS_SINGLE_KEYWORD,
        READ_STATUS_HEADER_POS,
        READ_STATUS_OBJECT,
        READ_STATUS_IS_ENGINE_BLOCK_RADIOSS,
        READ_STATUS_LAST
    } MyReadingStatus_e;

    virtual ISyntaxInfos* GetSyntaxInfo() { return NULL; }

    virtual void clearHeaderComments() 
    { 
        for(int i =0; i < myCommentsHeader.size(); i++)
            myCommentsHeader[i].clear(); 
    }
    virtual MECIDataReader* newDataReader()  { return nullptr; }

private: // Data
    PseudoLocFileVect_t     *myFileVectPtr;		     

    PseudoLocComponentVect_t*myComponentVectPtr;		 

    PseudoIntStack_t        *myComponentIndexStackPtr; 

    PseudoIntStack_t        *myFileIndexStackPtr;      

    int                     myIndexNewFile;            

    int                     myIndexNewComponent;            

    int                     myCurSubdeckIndex;        



    int                     myReadingStatus;           // 0-> CCARD  1->UNIT

    ReadFileFactorySP       myFileFactoryPtr = nullptr;

protected:
    CKeywordData             *myUnreadPositionPtr;

    int                     myUnReadFileIndex;


protected: // Data
  PseudoHeaderPositions_t *myHeaderPositionsPtr;
  InputInfos          *myInputInfosPtr;         




protected: // Crypting
  int                      myCryptingMethod; 

protected: // Recording
  bool                     myDoRecord;        
  PseudoRecordBuffer_t    *myRecordBufferPtr; 

protected: // Include files
    char                     *myMainFile;       
    PseudoFileList_t         *myFileListPtr;    
    std::vector< vector<std::string> >      myCommentsHeader;
    bool                      mySkipIncludeReading = false;
public:
    MECReadFile *updateFile(MECReadFile *file);
  /// Gets the current file
    MECReadFile *GetCurrentFilePtr() const; 
};


#endif //MECI_READ_MODEL_BASE_H
