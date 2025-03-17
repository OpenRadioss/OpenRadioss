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
#ifndef _MV_DATA_SIZE_RADIO_FEATURE_H_
#define _MV_DATA_SIZE_RADIO_FEATURE_H_

#include <KERNEL_BASE/Structure_descriptor.h>
#include "mv_data_size_feature.h"
#include "mv_data_radio_feature.h"
#include "mv_data_dynamic_array_size_radio_feature.h"

class MvDataSizeRadioFeature_t : public MvDataSingleFeature_t, public MvDataOrientationFeature_t
{
public: /** @name Constructors & destructor */
//@{
  /// Constructor
  MvDataSizeRadioFeature_t(const string &name,int ikeyword, value_type_e vtype,int nb_radios,
		                   MvOrientation_e orient=ORI_COLUMN,int nbr=0,int nbc=0);
  /// Destructor
  virtual ~MvDataSizeRadioFeature_t();
public: /** @name Associated dynamic arrays */
//@{
  /// Adds a dynamic array
  void addArray(MvDataDynamicArraySizeRadioFeature_t *daf_p);
  /// Gets the number of dynamic arrays
  inline int getNbArrays() const { return myNbArrays; }
  /// Gets the number of dynamic arrays
  inline MvDataDynamicArraySizeRadioFeature_t *getArrayPtr(int ind) const { return myArrayTab[ind]; }
//@}

public:
//@{
  /// Sets the number of dynamic arrays
  void setNbArrays(int value);
  /// Initializes the array ptr
  void setArrayPtr(MvDataDynamicArraySizeRadioFeature_t **value) {myArrayTab = value;}
//@}
protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;
public:
  /// Gets the type of values
  inline int           getValueType()               const { return myValueType; }
  /// Gets the number of radio buttons
  inline int           getNumber()                  const { return myNumber; }
  /// Gets a radio's name
  inline const string &getRadioName(int ind)        const { return myNameArray[ind]; }
  /// Gets a radio's value
  inline int           getRadioIntValue(int ind)    const { return myIntValueArray[ind]; }
  /// Gets a radio's value
  inline double        getRadioFloatValue(int ind)  const { return myFloatValueArray[ind]; }
  /// Gets a radio's value
  inline const string &getRadioStringValue(int ind) const { return myStringValueArray[ind]; }
  /// Gets a radio's value
  inline int           getRadioValue(int ind)       const { return getRadioIntValue(ind); }
  
  /// Gets a radio's domain
  inline MvDomain_e    getRadioDomain(int ind) const { return myDomainArray[ind]; }

  /// Gets retain comments flag
  bool  getDoPrefixValue(){ return myDoPrefixValue;}
  /// Sets flag whether to retain the comments or not (RETAIN_LIST_COMMENTS)
  void setDoPrefixValue(bool  doprefixvalue){ myDoPrefixValue = doprefixvalue; }
  /// Sets whether a radio option is enum type or not
  void setEnumValueFlag(bool enumvalueflag) { myEnumValueFlag = enumvalueflag;}
  /// Gets the enum flag
  bool getEnumValueFlag() {return myEnumValueFlag;}
  /// Sets a radio button
  void setRadio(int ind,const string &name,int value,MvDomain_e domain);
  /// Sets a radio button
  void setRadio(int ind,const string &name,double value,MvDomain_e domain);
  /// Sets a radio button
  void setRadio(int ind,const string &name,const string &value,MvDomain_e domain);
  /// Sets the size radio feature flag
  void setIsSizeRadioFeature(bool value) {mySizeRadioFeature=value;}
  /// Gets the size radio feature flag
  virtual bool getIsSizeRadioFeature() {return mySizeRadioFeature;}

//@}
private:
  bool          myDoPrefixValue;
  bool          mySizeRadioFeature;
  bool          myEnumValueFlag;
  value_type_e  myValueType;
  int           myNumber;
  string       *myNameArray;
  MvDomain_e   *myDomainArray;
  union {
    int        *myIntValueArray;
    double     *myFloatValueArray;
    string     *myStringValueArray;
  };
  int                           myNbArrays;
  MvDataDynamicArraySizeRadioFeature_t **myArrayTab;
};

#endif /* _MV_DATA_SIZE_RADIO_FEATURE_H_*/
