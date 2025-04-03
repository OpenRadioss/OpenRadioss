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
#ifndef MV_DATA_APPEND_FEATURE_H
#define MV_DATA_APPEND_FEATURE_H

#include "mv_full_type.h"
#include "mv_data_single_feature.h"


/// Data append feature
class MvDataAppendFeature_t : public MvDataSingleFeature_t {

public: /** @name Constructors & destructor */
//@{
  /// Constructor
  MvDataAppendFeature_t(const string &name,int loc_ikeyword,
			const MvFullType_t &full_type,const string &ext_skeyword,bool is_default);
  /// Destructor
  virtual ~MvDataAppendFeature_t();
//@}

public: /** @name Access methods */
//@{
  /** Returns true if the value to append (export) is the currently described object.<br>
      If this method returns false, use getIKeyword() to get the ikeyword of the concerned subvalue
  */
  inline bool                isThisKeyword()       const { return getIKeyword()==END_ARGS; }
  /// Returns true if the "append" must be done by default
  inline bool                isDefault()           const { return myIsDefault; }
  /// Gets the allowed fulltype for the external object
  inline const MvFullType_t &getExternalFullType() const { return myExternalFullType; }
  /// Gets the skeyword where to put the value into the external object
  inline const string       &getExternalSKeyword() const { return myExternalSKeyword; }
//@}

protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

protected:
  MvFullType_t myExternalFullType;
  string       myExternalSKeyword;
  bool         myIsDefault;

};


#endif //MV_DATA_APPEND_FEATURE_H




