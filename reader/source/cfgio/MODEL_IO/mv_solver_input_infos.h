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
#ifndef MV_RADIOSS_INFOS_H
#define MV_RADIOSS_INFOS_H

#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <KERNEL/mv_type.h>
#include "mv_element_infos.h"

#include "hcio.h"
#include "meci_input_infos.h"
#include "meci_syntax_infos.h"
#include <HCDI/hcdi_multicfgkernelmgr.h>

/// Class for informations about Radioss input file
class HCIO_DATA_DLL_API SolverInputInfo : public InputInfos {

public: /** @name Constructors & destructor */
 //@{
 /// Constructor
    SolverInputInfo();
    /// Destructor
    virtual ~SolverInputInfo() {}
    //@}


public:

    virtual const char* GetSolverSubdeckKeyword() const;
    virtual const char* GetSolverSubdeckKeywordEnd() const;
    virtual const char* GetEncryptedKeyword() const;
    virtual const char* GetIncludeKeyword() const;
    virtual const char* GetIncludeEndKeyword() const;
    virtual bool IsEntityCryptable(object_type_e obj_type, MvFileFormat_e version) const;
    virtual unsigned int GetAppMode() const { return myApplicationMode; }

    virtual int getHeaderSize() const { return myheader_size; }
    virtual const string& getHeaderSkipStr() const { return myheader_skip_str; }
    virtual int getKeywordProcessCommentFlag() const;
    virtual bool IsIncludeHeader(const char* header) const;
    virtual bool IsIncludeFilePathOnSameLine() const;
    virtual const char* GetBeginKeyword() const;
public: /** @name Header Keywords */
 //@{
 /// Init header keywords
    void initSolverSpecialKeywords();
    
    virtual const char* GetNormalizedKeyword(const char* keyword) const;
    /// Returns true if the keyword of the header means end-of-file
    virtual bool IsEofKeyword(const char* keyword) const;


    /// Gets the keywords and object types of the elements
    virtual PseudoObjectKeywords_t* GetElementKeywords(PseudoObjectKeywords_t* elt_keywords_p = NULL) const;

    /// Gets the keywords of the elements
    virtual const PseudoKeywordSet_t* GetElements() const;

    //@}  

public: /** @name Element infos */
 //@{
 /// Initializing infos about element types
    void initElementInfos();

    //@} 
    virtual void Release() { delete this; }
private: // Types

    typedef map<object_type_e, MvElementInfos_t> MyElementInfosByType_t;
public:

    string& GetIncludeHeader() { return myIncludeHeader; }
    const string& GetIncludeHeader() const { return myIncludeHeader; }
    virtual const CUserNameTypeInfo* GetKeywordSolverInfoFromHeaderLine(
        const string &header_line, const ISyntaxInfos* pSyntaxInfo,
        set<string>* stripheader = nullptr, obj_type_e etype = HCDI_OBJ_TYPE_NULL, bool isexactmatch = false) const;

    bool IsSupportedForContinueReadWithoutHeader() const;
    void ProcessKeywordComments(std::vector<std::vector<string>>& comments, obj_type_e& etype,
                                IdentifierValuePairList& vallst) const;
    virtual bool IsExactUsername() const { return myUserNameExactMatch; }

    virtual std::map<string, CUserNameTypeInfo>& getlUserNamesSolverInfo() {
        return mymapkeywordsolverinfo;
    }


protected: // Data
    int                    myVersion;
    MyKeywordSet_t         myElementKeywords;
    MyKeywordSet_t         myAllKeywords;

    string                 myIncludeHeader;

    string                 mybeginkeyword;
    string                 myendkeyword;
    string                 mysubmodelkeyword;
    string                 mysubmodelendkeyword;
    string                 mykeykeyword;
    string                 myincludekeyword;
    string                 myincludeendkeyword;
    ApplicationMode_e      myApplicationMode;

    int                    myheader_size=0;
    string                 myheader_skip_str="";
    bool                   myContinueReadWithoutHeader;
    bool                   myUserNameExactMatch = false;
    //std::vector<StringTypeInfoPair>  myobjectsolverinfo[HCDI_OBJ_TYPE_HC_MAX];
    std::map<string, CUserNameTypeInfo>  myobjectsolverinfo[HCDI_OBJ_TYPE_HC_MAX];
    std::map<string, CUserNameTypeInfo>  mymapkeywordsolverinfo;
};

//HCIO_DATA_DLL_API bool IsEntityCryptable(object_type_e obj_type, int version);

#endif //MV_RADIOSS_INFOS_H
