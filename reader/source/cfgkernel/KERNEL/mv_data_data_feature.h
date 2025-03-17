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
#ifndef MV_DATA_DATA_FEATURE_H
#define MV_DATA_DATA_FEATURE_H

#include "mv_full_type.h"
#include "mv_data_object_feature.h"
#include "mv_filter.h"

/// Data data feature
class MvDataDataFeature_t : public MvDataObjectFeature_t {

public: /** @name Constructors & destructor */
//@{
  /// Constructor
  MvDataDataFeature_t(const string &name,int ikeyword,object_type_e obj_type);
  /// Destructor
  virtual ~MvDataDataFeature_t();
//@}

public: /** @name Accessors */
//@{
  /// Gets allowed object types
  inline const MvFullTypeSet_t &getAllowedObjectFullTypes() const { return myFullTypeSet; }
  /// Adds an object full type in the object full type set
  void addObjectFullType(const MvFullType_t &full_type) { myFullTypeSet.insert(full_type); }
  /** Set filter information*/
  void createFilter(int nb, StringVect_t &attribs, StringVect_t &values, StringVect_t &criterias, StringVect_t &units, StringVect_t &messgs);
  /** Get filter information*/
  EntityFilter *getFilter() {return myFilter;}

//@}

protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

private:
  MvFullTypeSet_t myFullTypeSet;
  EntityFilter   *myFilter;

};


#endif //MV_DATA_DATA_FEATURE_H




