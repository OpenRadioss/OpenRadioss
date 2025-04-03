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
#ifndef MV_SUBTYPE_H
#define MV_SUBTYPE_H


#include <UTILS/mv_iostream.h>
#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <UTILS/mv_list.h>
#include "mv_utils.h"

#include "mv_flag_value.h"
#include <KERNEL_BASE/Structure_hierarchical.h>
#include <HCDI/hcdi.h>
#include <KERNEL_BASE/Structure_types.h>

#define PseudoStringList_t void

typedef list<MvFlagValue_t> MvFlagValueList_t;
typedef set<string>         MvKeywordSet_t;    
typedef deque<string> MvStringList_t;

typedef map<int, MvSubtype_t*>           MvUserIdSubtypeMap_t;
typedef map<string, MvUserIdSubtypeMap_t> MvTypeUserIdSubtypeMap_t;
typedef map<string, MvUserIdSubtypeMap_t> MvHMConfigTypeSubtypeMap_t;
typedef map<string, MvUserIdSubtypeMap_t> MvHMTypeSubtypeMap_t;

/** @name Subtypes */
//@{

/** Subtype class */
class MvSubtype_t {

public:   /**@name Constructors and destructor*/
  //@{
  /// Constructor without flags
  MvSubtype_t(const string &keyword,int subtype, string cardImage = "", MvStringList_t *user_name_list = NULL, int user_id=-1, int hm_config_type = 0, int hm_type = 0, short int idpool = 0, MvStringList_t* optional_header_string_list=NULL);
  /** Constructor with flags<br>
      After the subtype:
      <ul>
      <li>pairs of (attribute,value) of flags composing this subtype
      <li>ended by END_ARGS (even if no pair needed)
      </ul>
  */
  MvSubtype_t(const string &keyword,int subtype,int user_id,int attrib_keyword,...);
  /// Destructor
  ~MvSubtype_t();
  //@}  

public:  /**@name Accessors*/
  //@{
  /// Gets the Card Image
  inline char* getCardImage()      const { return myCardImage; }
  /// Gets the keyword
  inline const string &getKeyword()      const { return myKeyword; }
  /// Gets the MCDS subtype
  inline int           getSubtype()      const { return mySubtype; }
  /// Gets the MCDS subtype
  inline int           getUserId()       const { return myUserId; }
  /// Gets the MCDS subtype
  inline int           getHMConfigType()       const { return myConfigType; }
  /// Gets the MCDS subtype
  inline int           getHMType()       const { return myHmType; }
  /// Gets the id pool
  inline short int     getIdPool()       const { return myIdPool; }
  /// Gets the number of flags
  inline int           getNbFlagValues() const { return (int)myFlagValueList.size(); } 
  /// Gets the list of flags
  inline const MvFlagValueList_t &getFlagValueList() const { return myFlagValueList; }
  
  /// Gets the keywords
  MvKeywordSet_t *getKeywords(object_type_e otype,MvKeywordSet_t *keywords_p=NULL) const;
  
  inline const MvStringList_t &getUserNameList() const { return myUserNames; }
  inline const MvKeywordSet_t& getOptionalHeaderStringList() const { return myOptionalHeaderStrings; }
  //@}

private: // Datas
  int               mySubtype;
  char*             myCardImage;
  string            myKeyword;
  int               myUserId;
  int               myConfigType;
  int               myHmType;
  MvFlagValueList_t myFlagValueList;
  short int         myIdPool;
  MvStringList_t    myUserNames;
  MvKeywordSet_t    myOptionalHeaderStrings;
};


class MvKeywordSubtypeMap_t : public map<string, MvSubtype_t*> {
public: // Constructor and destructor
    inline MvKeywordSubtypeMap_t() : map<string, MvSubtype_t*>() {}
    ~MvKeywordSubtypeMap_t();
};



/* --------- MvTypeKeywordSubtypeMap_t (declaration) --------- */


class MvTypeKeywordSubtypeMap_t : public map<object_type_e, MvKeywordSubtypeMap_t> {
public:  // Constructor
    MvTypeKeywordSubtypeMap_t();
};



/// Set of tubtypes
typedef set<const MvSubtype_t *> MvSubtypePtrSet_t;


/// output of a (MvSubtype_t *) in an output stream
ostream &operator<<(ostream &os,const MvSubtype_t *subtype_p);

/// Getting subtype from keyword
const MvSubtype_t *MV_get_subtype(object_type_e obj_type,const string &keyword,connection_type_e econtype=CONNECTION_UNKNOWN);
//
/// Getting subtype of connection from connection_type_e
//const MvSubtype_t *MV_get_subtype_connection(object_type_e obj_type,connection_type_e econtype);
//
//
/// Getting user subtype from user id
//const MvSubtype_t *MV_get_user_subtype(object_type_e obj_type,int user_id);
//
/// Getting user subtype from user id
//const MvSubtype_t *MV_get_user_subtype(const string &obj_type,int user_id);
//
/// Getting user subtype from user id

//
/// Getting user subtype from user id

//
/// Gettting MCDS user subtype of a MCDS type
int MV_get_MCDS_user_subtype(object_type_e obj_type);
//
/// Adding user subtype

//                                       const PseudoStringList_t *user_name_list_p);

/// Adding user subtype

//                                      const string &user_name);

//				       const string &user_name);
//
//
/// Does a given type admit user subtypes?
HC_DATA_DLL_API bool MV_is_user_subtyped(object_type_e obj_type);
//
/// Does a given type admit subtypes?
HC_DATA_DLL_API bool MV_is_subtyped(object_type_e obj_type);
//
//
/// Returns the subtypes of a given type
//MvSubtypePtrSet_t *MV_get_subtypes(object_type_e otype,MvSubtypePtrSet_t *subtypes_p=NULL);
//
//void MV_delete_all_subtypes() ;
//@}


#endif /* MV_SUBTYPE_H */




