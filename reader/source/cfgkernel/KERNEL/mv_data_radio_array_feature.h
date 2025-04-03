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
#ifndef MV_DATA_RADIO_ARRAY_FEATURE_H
#define MV_DATA_RADIO_ARRAY_FEATURE_H

#include "mv_data_static_array_feature.h"


/// Data radio array feature
class MvDataRadioArrayFeature_t : public MvDataStaticArrayFeature_t {

public: /** @name Constructors & destructor */
  //@{
  /// Constructor
  MvDataRadioArrayFeature_t(const string &name,int ikeyword,int nb_values,int size);
  /// Destructor
  virtual ~MvDataRadioArrayFeature_t();
  //@}

public: /** @name Accessors */
  //@{
  /// Gets the integer keyword
  virtual inline int getIKeyword() const { 
    return ((MvDataRadioFeature_t *)(getDataFeature(0)))->getIKeyword(); 
  }
  /// Gets the number of values
  inline int getNbRadios() const { return ((MvDataRadioFeature_t *)(getDataFeature(0)))->getNumber(); }
  /// Gets a radio's name
  inline const string  &getRadioName(int ind)  const { 
    return ((MvDataRadioFeature_t *)(getDataFeature(0)))->getRadioName(ind); 
  }
  /// Gets a radio's value
  inline int getRadioValue(int ind) const { 
    return ((MvDataRadioFeature_t *)(getDataFeature(0)))->getRadioValue(ind);
  }
  /// Sets a radio button
  void setRadio(int ind,const string &name,int value) {
    
    ((MvDataRadioFeature_t *)(getDataFeature(0)))->setRadio(ind,name,value,DOM_UNKNOWN);
    
  }
  //@}

protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

};


#endif //MV_DATA_RADIO_ARRAY_FEATURE_H




