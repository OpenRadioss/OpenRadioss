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

#ifndef MV_IPARAM_DESCR_ROTATION_H
#define MV_IPARAM_DESCR_ROTATION_H

#include "mv_iparam_descr.h"


/** @name Rotation input paramater descriptor */
//@{


/// Rotation input paramater descriptor class
class MvIParamDescrRotation_t : public MvIParamDescr_t {

public: /** @name Constructors and destructor */
  //@{
  /// Constructor
  MvIParamDescrRotation_t(MvIParamAccess_e access,const string &name,const string &comment);
  /// Destructor
  virtual inline ~MvIParamDescrRotation_t() {}
  //@}

public: /** @name MvIParam_t redefined methods */
  //@{
  /// Gets the type
  virtual inline MvIParamType_e getType()        const { return IPT_ROTATION; }
  /// Gets the number of float data
  virtual inline int            getNbFloatData() const { return 6; }  
  //@}
  
public: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

};


//@}


#endif //MV_IPARAM_DESCR_ROTATION_H
