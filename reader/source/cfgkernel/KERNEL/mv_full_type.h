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
#ifndef MV_FULL_TYPE_H
#define MV_FULL_TYPE_H


#include <UTILS/mv_stl_various.h>

#include "mv_subtype.h"
#include <HCDI/hcdi.h>
#include <KERNEL/cfg_kernel.h>
#if defined _WIN32 || defined WIN32 
#pragma warning(disable:4251)    
#endif 
/** @name Full types */
//@{

/// Full type class
class HC_DATA_DLL_API MvFullType_t {

public: /** @name Constructors & Destructor */
  //@{
  /// Constructor -> ex: MvFullType_t(MATERIAL,MV_get_subtype(MATERIAL,"PLAS_ZERIL"))
    /* need_to_be_checked . Temporarily changing MV_EMPTY to */
  MvFullType_t(object_type_e type= HCDI_OBJ_TYPE_NULL,const MvSubtype_t *subtype_p=NULL);
  /// Constructor -> ex: MvFullType_t(MATERIAL,"PLAS_ZERIL")
  MvFullType_t(const CFGKernel& cfgkernel, object_type_e type,const string &subtype, const string &idpoolstr="");/*kernelmodel needed*/
  /// Constructor -> ex: MvFullType_t("MAT","PLAS_ZERIL")
  MvFullType_t(const CFGKernel& cfgkernel, const string &type,const string &subtype, const string &idpoolstr = "");/*kernelmodel needed*/
  /// Constructor -> ex: MvFullType_t("/MAT/PLAS_ZERIL")
  MvFullType_t(const CFGKernel & cfgkernel, const string &fulltype);  /*kernelmodel needed*/
  //@}

public: /** @name Accessors */
  //@{
  /// Gets the type
  inline object_type_e      getType()       const { return myType; }
  /// Gets the subtype
  inline const MvSubtype_t *getSubtypePtr() const { return mySubtypePtr; }
  /// Gets the idpoolstr
  inline const string &getIdPoolStr() const { return myIdPoolStr; }
  //@}

public: /** @name Operators */
  //@{
  ///
  bool operator==(const MvFullType_t &fulltype) const;
  //
  bool operator!=(const MvFullType_t &fulltype) const;
  /// 
  bool operator<(const MvFullType_t &fulltype)  const;
  ///
  bool operator<=(const MvFullType_t &fulltype) const;
  ///
  bool operator>(const MvFullType_t &fulltype)  const;
  ///
  bool operator>=(const MvFullType_t &fulltype) const;
  //@}


public: /** @name Getting strings */
  //@{
  /// Gets the type's string
  const string &getTypeStr()    const;
  /// Gets the subtype's string
  const string &getSubtypeStr() const;
  /// String cast
  operator string() const;
  //@}

private: // Member datas
  object_type_e      myType;
  const MvSubtype_t *mySubtypePtr;
  string             myIdPoolStr;
};

/// Comparison class (helpfull for maps ot sets of full types)
class HC_DATA_DLL_API MvFullTypeLess_t {
public: 
  inline MvFullTypeLess_t() {}
public: 
  /// Function operator
  bool operator()(const MvFullType_t &ft0,const MvFullType_t &ft1) const;
};



/** @name Set of full types */
//@{
/// Class MvFullTypeSet_t
class MvFullTypeSet_t : public set<MvFullType_t,MvFullTypeLess_t> {
 public:
    inline MvFullTypeSet_t() : set<MvFullType_t,MvFullTypeLess_t>() {}
    inline MvFullTypeSet_t(const set<MvFullType_t,MvFullTypeLess_t> &ftypes) :
      set<MvFullType_t,MvFullTypeLess_t>(ftypes) {}
 public:
    /// Returns true if the left set is included in the right set
    bool operator<=(const MvFullTypeSet_t &fts1) const;
    /// Returns true if the left set is strictly included in the right set
    bool operator<(const MvFullTypeSet_t &fts1) const;
    /// Returns true if the left set is equal to the right set
    bool operator==(const MvFullTypeSet_t &fts1) const;
    /// Returns true if the left set is equal to the right set
    inline bool operator!=(const MvFullTypeSet_t &fts1) const {
      return !(*this==fts1);
    }
};



/// Returns true if the full type is included in the set
bool operator<=(const MvFullType_t &full_type,const MvFullTypeSet_t &full_type_set);
//@}


//@}


/// Comparison class 
class MvFullTypeOrderLess_t:public MvFullTypeLess_t
{
public: 
  inline MvFullTypeOrderLess_t() {}
public: 
  /// Function operator
  bool operator()(const MvFullType_t &ft0,const MvFullType_t &ft1) const;
};
typedef set<MvFullType_t, MvFullTypeOrderLess_t> MvFullTypeOrderSet_t;

#endif //MV_FULL_TYPE_H




