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
#ifndef MECI_HCDI_MECI_INPUT_INFOS_H
#define MECI_HCDI_MECI_INPUT_INFOS_H

#define PseudoKeywordSet_t          void
#define PseudoObjectKeywords_t      void
#define PseudoElementInfos_t        void
#define PseudoControlCardKeywords_t void
#define PseudoControlCardList_t     void

#include "hcio.h"
#include <UTILS/mv_string.h>
#include <cstring>
#include <UTILS/mv_stl_various.h>
#include <KERNEL/mv_utils.h>
#include <UTILS/str_utils.h>
#include <KERNEL/cfg_kernel.h>


class ISyntaxInfos;
/// Base class for informations about input file (Radioss, LSDyna, Nastran, ...)
class HCIO_DATA_DLL_API InputInfos {

public: /** @name Constructors & destructor */
    //@{
    /// Constructor
  inline InputInfos() {}
  /// Destructor
  virtual ~InputInfos() {}
  //@}

public:
    enum {
        SOLVER_PORCESS_KEYWORD_COMMMENT_NONE = 0,
        SOLVER_PROCESS_KEYWORD_COMMMENT_BEFORE,
        SOLVER_PROCESS_KEYWORD_COMMMENT_AFTER,
        SOLVER_PROCESS_KEYWORD_COMMENT_BEFORE_AFTER
    };
    typedef set<string>               MyKeywordSet_t;
    typedef pair<string, string>      MyKeywordOType_t;
    typedef vector<MyKeywordOType_t>  MyObjectKeywords_t;
    typedef std::vector< std::pair<std::string, cfgkernel::Variant> > IdentifierValuePairList;
public:
    virtual bool IsIncludeHeader(const char* header) const
    { 
        size_t len = strlen(GetIncludeKeyword());
        if (!len) return false;
        return (mystrncasecmp(header, GetIncludeKeyword(), strlen(GetIncludeKeyword())) == 0); 
    }

    virtual const char* GetSolverSubdeckKeyword() const = 0;
    virtual const char* GetSolverSubdeckKeywordEnd() const = 0;
    virtual const char* GetEncryptedKeyword() const = 0;
    virtual const char* GetIncludeKeyword() const = 0;
    virtual const char* GetIncludeEndKeyword() const = 0;
    virtual bool IsEntityCryptable(object_type_e obj_type, MvFileFormat_e version) const { return false; }
    virtual unsigned int GetAppMode() const { return 0;  }
    virtual int getKeywordProcessCommentFlag() const { return SOLVER_PORCESS_KEYWORD_COMMMENT_NONE; }
    virtual bool IsIncludeFilePathOnSameLine() const { return true; }
    virtual bool IsExactUsername() const { return false; }

    virtual std::map<string, CUserNameTypeInfo>& getlUserNamesSolverInfo()
    {  
        static map<string, CUserNameTypeInfo> empty;
        return  empty;
    }
    /// <summary>
    /// Gets begin keyword
    /// </summary>
    virtual const char* GetBeginKeyword() const { return ""; }
 public: /** @name Header Keywords */
  //@{
  
     virtual const char* GetNormalizedKeyword(const char* keyword) const { return ""; }
  /// Returns true if the keyword of the header means end-of-file
  virtual bool IsEofKeyword(const char *keyword) const=0; 
  ///Returns UserName and its type info best match for a given header line
  virtual const CUserNameTypeInfo* GetKeywordSolverInfoFromHeaderLine(
      const string& header_line, const ISyntaxInfos* pSyntaxInfo,
      set<string> *stripheader = nullptr, obj_type_e etype = HCDI_OBJ_TYPE_NULL, bool isexactmatch=false) const
  { return nullptr; }
  ///
  virtual bool IsSupportedForContinueReadWithoutHeader() const { return false; }
  ///
  virtual void ProcessKeywordComments(std::vector<std::vector<string>>& comments,
                                      obj_type_e& etype, IdentifierValuePairList& vallst) const
  { return; }
  /// Gets the keywords and object types of the elements
  virtual PseudoObjectKeywords_t *GetElementKeywords(PseudoObjectKeywords_t *elt_keywords_p=NULL) const { return nullptr; }

  /// Gets the keywords of the elements
  virtual const PseudoKeywordSet_t *GetElements() const { return nullptr; }
  //@}  
  virtual void Release() = 0;
  //@} 
};

EXTERNC HCIO_DATA_DLL_API InputInfos *HCDIGetMECRadiossInfosPtr(int version);
EXTERNC HCIO_DATA_DLL_API std::vector<std::string>& HCDIGetSubojectkeywordList();
#endif //MECI_HCDI_MECI_INPUT_INFOS_H
