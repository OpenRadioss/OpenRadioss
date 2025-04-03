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
#ifndef MV_DATA_FLAG_LIST_FEATURE_H
#define MV_DATA_FLAG_LIST_FEATURE_H

#include "mv_data_orientation_feature.h"
#include "mv_data_multi_feature.h"


/// Data flag feature
class MvDataFlagListFeature_t : public MvDataMultiFeature_t, public MvDataOrientationFeature_t {

public: /** @name Constructors & destructor */
  //@{
  /// Constructor
  MvDataFlagListFeature_t(const string &name,int nb_flags,MvOrientation_e orient,int nbr=0,int nbc=0);
  /// Destructor
  virtual ~MvDataFlagListFeature_t();
  //@}
  
public: /** @name Accessors */
  //@{
  /// Gets the number of feature
  inline int getNumber()                    const { return getNbFeatures(); }
  /// Gets an integer keyword
  inline int getFlagIKeyword(int ind)       const { return getIKeyword(ind); }
  /// Gets a name
  inline const string &getFlagName(int ind) const { return getName(ind); }
  /// Sets a feature
  inline void setFlag(int ind,int ikeyword,const string &name) { setFeature(ind,ikeyword,name); }
  //@}

protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

};


#endif //MV_DATA_FLAG_LIST_FEATURE_H




