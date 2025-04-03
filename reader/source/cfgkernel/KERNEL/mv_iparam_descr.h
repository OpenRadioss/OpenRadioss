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
#ifndef MV_IPARAM_DESCR_H
#define MV_IPARAM_DESCR_H

#include <UTILS/mv_string.h>
#include <UTILS/mv_iostream.h>
#include <UTILS/mv_stl_various.h>

class MvDescriptor_t;

/** @name Input paramater */
//@{


/// Input paramater types
enum MvIParamType_s {
  /** */ IPT_UNKNOWN,
  /** */ IPT_SCALAR,
  /** */ IPT_SUBPARAM,
  /** */ IPT_TRANSLATION, 
  /** */ IPT_ROTATION,    
  /** */ IPT_SCALING,     
  /** */ IPT_LAST
}; 
/// Input paramater types
typedef enum MvIParamType_s MvIParamType_e;

/// Input paramater access
enum MvIParamAccess_s {
  /** */ IPA_UNKNOWN,
  /** */ IPA_PUBLIC,
  /** */ IPA_PRIVATE,
  /** */ IPA_LAST
}; 
/// Input paramater access
typedef enum MvIParamAccess_s MvIParamAccess_e;

/// Input paramater descriptor class
class MvIParamDescr_t {

public:    /** @name Constructors and destructor */
  //@{
  /// Constructor
  inline MvIParamDescr_t(MvIParamAccess_e access,const string &name,const string &comment) : 
    myAccess(access), myName(name), myComment(comment) {}
  /// Destructor
  virtual inline ~MvIParamDescr_t() {}
  //@}

public:    /** @name Accessors */
  //@{
  /// Gets the type
  virtual inline MvIParamType_e  getType()        const { return IPT_UNKNOWN; }
  /// Gets the access (IPA_PUBLIC or IPA_PRIVATE)
  inline MvIParamAccess_e        getAccess()      const { return myAccess; }
  /// Gets the name
  inline const string           &getName()        const { return myName; }
  /// Gets the comment
  inline const string           &getComment()     const { return myComment; }
  
  /// Gets the number of float data
  virtual inline int             getNbFloatData() const { return 0; }  
  
  //@}
  
public: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const=0;

protected: // Data
  MvIParamAccess_e myAccess;
  string           myName;
  string           myComment;

};


/// List of input paramaters
typedef vector<const MvIParamDescr_t *> MvIParamDescrPtrList_t; 
typedef set<const MvIParamDescr_t *>    MvIParamDescrPtrSet_t;  



//@}


#endif //MV_IPARAM_DESCR_H




