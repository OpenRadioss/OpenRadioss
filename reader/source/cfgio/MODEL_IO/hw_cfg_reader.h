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
#ifndef HW_CFG_READER_H
#define HW_CFG_READER_H

#include <MODEL_IO/meci_read_model_base.h>
#include <MODEL_IO/mv_solver_input_infos.h>
#include <MODEL_IO/mv_model_factory.h>
#include <MODEL_IO/meci_data_reader.h>
#include <vector>

#define PseudoFileFormat_t      void
#define PseudoFileFormatCard_t  void
#define PseudoFileFormatCell_t  void
#define PseudoDescriptor_t      void
#define PseudoObjectTypeSet_t   void
#define PseudoIntFlagList_t     void
#define PseudoStringFlagList_t  void
#define PseudoTransformOrderMap_t void 
#define PseudoIndexMap_t void /*MultidimensionalArray*/
#include <string>
#include <queue>
#include <chrono>

typedef map<string, int> map_str_int;

class HWCFGReaderMessageList;
// Performance timing classes
class PerformanceTimer {
private:
    std::chrono::high_resolution_clock::time_point start_time_;
    const char* operation_name_;
    bool enabled_;

public:
    explicit PerformanceTimer(const char* operation, bool enabled = true)
        : operation_name_(operation), enabled_(enabled) {
        if (enabled_) {
            start_time_ = std::chrono::high_resolution_clock::now();
        }
    }

    ~PerformanceTimer() {
        if (!enabled_) return;

        auto end_time = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time_);

        // Use HWCFGReader's message system
        printf("[PERF] %s: %lld ms\n", operation_name_, duration.count());
    }

    // Disable copy constructor and assignment
    PerformanceTimer(const PerformanceTimer&) = delete;
    PerformanceTimer& operator=(const PerformanceTimer&) = delete;
};

// Performance configuration
class PerformanceConfig {
private:
    static bool timing_enabled_;
    static std::set<std::string> enabled_operations_;

public:
    static bool IsTimingEnabled() { return timing_enabled_; }

    static bool IsOperationEnabled(const char* operation) {
        
        if (!timing_enabled_) {
            return false;
        }

        
        printf("[DEBUG] Checking operation: '%s', timing_enabled_=true\n", operation);

        bool result = (enabled_operations_.empty() ||
            enabled_operations_.find(operation) != enabled_operations_.end());

        printf("[DEBUG] Operation '%s' result: %s (enabled_operations_.size()=%zu)\n",
            operation, result ? "ENABLED" : "DISABLED", enabled_operations_.size());

        return result;
    }

    static void EnableTiming(bool enable = true) { timing_enabled_ = enable; }
    static void EnableOperation(const std::string& operation) { enabled_operations_.insert(operation); }
    static void ClearOperations() { enabled_operations_.clear(); }
};


/*
# Enable all timing
export HWCFG_ENABLE_PERFORMANCE_TIMING=1

# Enable specific operations only
export HWCFG_TIMING_OPERATIONS=HWCFGReader::readModel,HWCFGReader::readHeaderPositions,HWCFGReader::readKeyword

# Then run your application
./your_application
*/
// Macros for easy usage
#define HWCFG_PERF_TIMER(name) PerformanceTimer timer(name, PerformanceConfig::IsOperationEnabled(name))
#define HWCFG_PERF_TIMER_SCOPED(name) PerformanceTimer timer_##__LINE__(name, PerformanceConfig::IsOperationEnabled(name))

class IEncryption
{
public:
    IEncryption() { }

    virtual ~IEncryption()  { }
    /// Crypting types
    typedef enum MyCryptingType_s {
        CRYPT_UNKNOWN,
        CRYPT_GENE,
        CRYPT_FUNC,
        CRYPT_BEGIN_PGP_MESSAGE,
        CRYPT_LAST
    } MyCryptingType_e;

    virtual void InitializeCrypting() { }

    virtual bool IsEncryptionSupported() { return false; }
    virtual bool CheckCouldBeEncryptedLine(const char *line) { return false; }
    virtual bool IsStringEncrypted(const char* line) { return false; }
    virtual bool IsKeySupported() { return false; }

    /// Sets/Gets the crypting method
    virtual inline void SetCryptingMethod(MyCryptingType_e type)  {  }
    virtual inline void SetCryptingMethod(object_type_e type) {  }
    virtual inline MyCryptingType_e GetCryptingMethod() const { return IEncryption::CRYPT_UNKNOWN; }

    virtual inline char *GetKeyRefFromLine(char *iline) { return 0; }

    virtual const char* GetCurCryptingReference() { return "";  }

    virtual char GetCrypChar() { return ' '; }

    virtual const int* GetCryptingKey(const std::string& ref) { return nullptr; }

    virtual int* GetCryptingIkey(const char* skey, bool newCryptingVersion = false) { return nullptr; }


    virtual  void  EncryptLine(const char* iline, char* oline, const char* ref, const int* ikey, bool newCryptingVersion = false) { }
    virtual  char* GetDecryptLine(const char* iline, const char* ref, bool& isdecrypted) { return const_cast<char *>(iline); }
};

class EncryptionLSDyna : public IEncryption
{
public:
    bool IsKeySupported() override { return false; }
};

class EncryptionRadioss : public IEncryption
{
public:
    bool IsKeySupported() override  { return true; }
};


/// Base class for reading files
class HCIO_DATA_DLL_API HWCFGReader : public MECIReadModelBase {

public: /** @name Constructors, destructor, re-configuring */
    //@{
    /// Constructor
    HWCFGReader(const char* full_name, MvFileFormat_e version, ISyntaxInfos &syntaxSolverInfos, InputInfos &solverInf,
                const ReadFileFactorySP& factory_p=nullptr);
    HWCFGReader(MECReadFile* file, MvFileFormat_e version, ISyntaxInfos& syntaxSolverInfos, InputInfos& solverInf,
                const ReadFileFactorySP& factory_p=nullptr);
    /// Destructor
    virtual ~HWCFGReader();

    /// Setting a version (if reading driven from outside)
    void SetVersion(MvFileFormat_e version);
    MvFileFormat_e getVersion() { return myVersion; }
    //@}

public: /** @name Reading model (public) */
    /// Reading model
    void readModel(MECIModelFactory* model_p, bool do_transform = true);
    IMECPreObject* readCurrentObjects(MECIModelFactory* model_p, char* header_card = NULL, const CUserNameTypeInfo* headerdata=nullptr, int linecount=0);
    virtual void readCurrentObjects(MECIModelFactory* model_p, vector<IMECPreObject*>& preobj_lst, char* header_card, const CUserNameTypeInfo* headerdata=nullptr, int linecount=0);
    virtual int readKeyword(const CUserNameTypeInfo* p_type_info, MECIModelFactory* model_p, const char* header);
protected: /** @name Reading model (protected) */
    //@{

    /// Gets the header's informations
    bool readHeader(const CUserNameTypeInfo* headerdata, string& input_ftype_str, string& kernel_ftype_str,
        const PseudoDescriptor_t** descr_pp, const PseudoFileFormat_t** format_pp, int* id_p, char** title_p, int* unit_id_p,
        char** id_param_p, char** id_unit_param_p, obj_type_e &etype,
        char** header_card = nullptr, int     curHeaderPos = -1);

    /// Initializations before reading objects
    virtual void initReadObjects(MECIModelFactory* model_p, const char* keyword, const char* otype);
    /// Post-treatments after reading objects
    virtual void closeReadObjects(MECIModelFactory* model_p, const char* keyword, const char* otype);
    /// Reading the current objects (current header)
    //virtual void readCurrentObjects(MECIModelFactory* model_p, const char* otype);
    /// Returns true if the object can be added to the model
    bool checkObject(const IMECPreObject& object, const MECIModelFactory* model_p) const;

    /// Reading object data
    bool readObjectData(const PseudoFileFormat_t* format_p,  IMECPreObject* object_p,
                        MECIModelFactory* model_p,   const PseudoDescriptor_t* descr_p,
                        int  card_ind0 = 0, int cur_index=0);

    /// Save a position by pushing it on a stack of positions, in order to get back to it with popPosition().
    virtual void pushPosition();
    /// Get back to a saved position by popping it from the stack of positions filled with pushPosition().
    virtual void popPosition();
    //@}

protected: /** @name Parsing */
    //@{
    
    virtual bool IsIncludedFile(const char* buffer, char** full_name_p, char** relative_name_p, char** include_path=nullptr) ;
    /// Pretreat the closing of a file
    virtual bool pretreatCloseFile();
    /// Returns true, if the line is commented
    virtual bool isComment(const char* buffer) const;
    /// Sets the state of comments
    inline void setCommentState(bool state) { myCommentState = state; } 
    /// Gets the state of comments
    inline bool getCommentState() const { return myCommentState; } 
    /// Manages a comment
    virtual void manageComment(const char* buffer);

    bool IsComponent(const char* buffer) const;

    /// Splits the header
    int splitHeader(char* header, char** header_fields) const;
    /// Gets the number of lines before next header
    int getNbFreeLines();
    /// Returns true if the current line is crypted
    bool isCrypted(); 
    bool isEntityCrypted();
    /// Read a component
    virtual MECComponent* ManageComponent(const char* buffer, bool& continue_flag, MECReadFile** new_file_p=NULL);

    virtual bool GetComponentState() { return mycomponentstate; }
    virtual void SetComponentState(bool state) { mycomponentstate = state; }
    /// Reading an integer
    int scanInt(const char* cell, const char* format, int nb_chars, bool* a_ok_p = NULL) const;
    /// Reading a double
    double scanDouble(const char* cell, const char* format, int nb_chars, bool* a_ok_p = NULL) const;
    /// Reading a string
    char* scanString(const char* cell, const char* format, int nb_chars, char* value, bool* a_ok_p = NULL) const;


    /// Reading an integer having possibility of the field being parameterized
    int scanInt(const char* cell, const char* format, int nb_chars, string& param_name, bool* a_ok_p = NULL) const;
    /// Reading a double having possibility of the field being parameterized
    double scanDouble(const char* cell, const char* format, int nb_chars, string& param_name, bool* a_ok_p = NULL) const;
    /// Reading an integer having possibility of the field being parameterized

   //@}
    MvFileFormat_e getFileFormatVersion(int subdeck_index);
    MvFileFormat_e getFormatVersion(void);

    /// Sets the current model factory
public:
    virtual void readHeaderPositions(MECIModelFactory* model_p);
    inline void SetModelFactoryPtr(MECIModelFactory* mf_p) { myModelFactoryPtr = mf_p; }
    /// Returns true if the line is a header
    virtual bool isHeader(const char* buffer, char** keyword_p = NULL) const;
    virtual void postTreatLineCount(const CUserNameTypeInfo* p_type_info, string &header, int* line_count) {}
public:
    // Helper methods for include block management
    void EnsureIncludeBlockSize(int fileIndex);
    bool IsIncludeBlockActive(int fileIndex) const;
    void SetIncludeBlockState(int fileIndex, bool state);
    void UpdateIncludeBlockState(int fileIndex, bool isHeader);
    void OptimizeIncludeBlockStorage();
    string findIncludeFileInSearchPaths(const string& main_dir, const string& filename, char** include_path);
protected:
    /// Gets the current model factory
    inline const MECIModelFactory* GetModelFactoryPtr() const { return myModelFactoryPtr; }
    /// Gets the current model factory
    inline MECIModelFactory* GetModelFactoryPtr() { return myModelFactoryPtr; }
    /// Decrypting a line
    virtual char* DecryptLine(char* iline);
    //@}
protected: /** @name Recording */
    //@{
    /// Clears the record buffer
    virtual void  ClearRecordBuffer();
    //@}
protected:/** @name Object counters */


protected: /** @name Messages */
    //@{
    /// Gets the message of the given index
    const char* getMsg(int ind) const;
    // Implementation of MECIReadContext.
    // Location is stored in displayMessage(), so we don't do anything here
    virtual void displayCurrentLocation(MyMsgType_e msg_type) const {}
    /// Displays a message of the given type (implemented from MECMsgManager)
    /// The reader in fact cannot display the messages itself, it stores them and they can be
    /// queried by the application with GetMessageList().
    virtual void displayMessage(MyMsgType_e msg_type,const char *format,...) const;
    //@}

public: /** @name MessageList */
    //@{
    /// Sets a message list which will be populated when reading
    void SetMessageList(HWCFGReaderMessageList* pMessageList, bool owningMessageList);
    /// Gets the message list (might be nullptr)
    const HWCFGReaderMessageList* GetMessageList() const { return m_pMessageList; }
    //@}

public:
    MECIDataReader* getDataReader() { return  myDataReader; }
    void SetInHMFlag(bool status) { myInHMFlag = status; }
    bool GetInHMFlag() { return myInHMFlag; }
    virtual ISyntaxInfos* GetSyntaxInfo() { return mySyntaxInfo; }
    virtual MECIDataReader* newDataReader() ;

public:
    IEncryption* newEncryption();


public:
    // Performance timing methods
    void enablePerformanceTiming(bool enable = true);
    void enableTimingForOperation(const std::string& operation);
    void initializePerformanceTimingFromEnvironment();


protected: // Data
    int                         myCellLength;
    int                         myLineNbCells;
    MvFileFormat_e              myVersion;
    bool                        myCommentState;
    int                         myIndCurrentFile;
    _HC_LONG                    myCurrentLoc;
    _HC_LONG                    myCurrentLine;
    _HC_LONG                    myUnReadLine;
    _HC_LONG                    myUnReadLoc;
    int                         myUnReadFileIndex;
    std::string                 myCurrentHeader;

protected: 
    MECIModelFactory*           myModelFactoryPtr;
    bool                        myIsCrypted;
    MECIDataReader*             myDataReader=nullptr;
    ISyntaxInfos*               mySyntaxInfo;
    bool                        myInHMFlag;
    bool                        mycomponentstate=false;
    IEncryption*                myEncryptionPtr=nullptr;
    mutable HWCFGReaderMessageList* m_pMessageList = nullptr;
    mutable bool                    m_owningMessageList = false;
    // Replace single include block flag with per-file vector
    std::vector<bool>           myIncludeBlock;
};


class HCIO_DATA_DLL_API HWCFGReaderLSDyna : public HWCFGReader {

public: /** @name Constructors, destructor, re-configuring */
    //@{
    /// Constructor
    HWCFGReaderLSDyna(const char* full_name, MvFileFormat_e version, ISyntaxInfos& syntaxSolverInfos, InputInfos& solverInf,
                      const ReadFileFactorySP& factory_p=nullptr);
    HWCFGReaderLSDyna(MECReadFile* file, MvFileFormat_e version, ISyntaxInfos& syntaxSolverInfos, InputInfos& solverInf,
                      const ReadFileFactorySP& factory_p=nullptr);
    /// Destructor
    virtual ~HWCFGReaderLSDyna() { }

public:

    void readCurrentObjects(MECIModelFactory* model_p, vector<IMECPreObject*>& preobj_lst, char* header_card, const CUserNameTypeInfo* headerdata = nullptr, int linecount=0);

    virtual void postTreatLineCount(const CUserNameTypeInfo* p_type_info, string& header, int* line_count);
};




#endif //HW_CFG_READER_H
