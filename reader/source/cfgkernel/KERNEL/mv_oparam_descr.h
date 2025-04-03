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

//

#ifndef MV_OPARAM_DESCR_H
#define MV_OPARAM_DESCR_H

#include <UTILS/mv_string.h>
#include <UTILS/mv_iostream.h>
#include <UTILS/mv_stl_various.h>

class MvDescriptor_t;

/** @name Output paramater */
//@{


/// Output paramater types
enum MvOParamType_s {
  /** */ OPT_UNKNOWN,
  /** */ OPT_TH_VARIABLE,
  /** */ OPT_LAST
}; 
/// Output paramater types
typedef enum MvOParamType_s MvOParamType_e;

/// Output paramater access
enum MvOParamAccess_s {
  /** */ OPA_UNKNOWN,
  /** */ OPA_PUBLIC,
  /** */ OPA_PRIVATE,
  /** */ OPA_LAST
}; 
/// Output paramater access
typedef enum MvOParamAccess_s MvOParamAccess_e;

/// Output paramater descriptor class
class MvOParamDescr_t {

public:    /** @name Constructors and destructor */
  //@{
  /// Constructor
  inline MvOParamDescr_t(MvOParamAccess_e access,const string &name) : 
    myAccess(access), myName(name) {}
  /// Destructor
  virtual inline ~MvOParamDescr_t() {}
  //@}

public:    /** @name Accessors */
  //@{
  /// Gets the type
  virtual inline MvOParamType_e  getType()        const { return OPT_UNKNOWN; }
  /// Gets the access (OPA_PUBLIC or OPA_PRIVATE)
  inline MvOParamAccess_e        getAccess()      const { return myAccess; }
  /// Gets the name
  inline const string           &getName()        const { return myName; }
  //@}
  
public:    // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const=0;

protected: // Data
  MvOParamAccess_e myAccess;
  string           myName;

};


/// List of input paramaters
typedef vector<const MvOParamDescr_t *> MvOParamDescrPtrList_t;
typedef set<const MvOParamDescr_t *>    MvOParamDescrPtrSet_t;  



//@}


#endif //MV_OPARAM_DESCR_H
