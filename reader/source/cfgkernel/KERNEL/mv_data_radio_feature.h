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
#ifndef MV_DATA_RADIO_FEATURE_H
#define MV_DATA_RADIO_FEATURE_H

#include "mv_domain.h"                   
#include "mv_data_orientation_feature.h"
#include "mv_data_single_feature.h"
#include <KERNEL_BASE/Structure_descriptor.h>


/// Data radio feature
class MvDataRadioFeature_t : public MvDataSingleFeature_t, public MvDataOrientationFeature_t {

public: /** @name Constructors & destructor */
//@{
  /// Constructor
  MvDataRadioFeature_t(const string &name,int ikeyword,value_type_e vtype,int nb_radios,
		       MvOrientation_e orient=ORI_COLUMN,int nbr=0,int nbc=0);
  /// Destructor
  virtual ~MvDataRadioFeature_t();
//@}

public: /** @name Accessors */
//@{
  /// Gets the type of values
  inline int           getValueType()               const { return myValueType; }
  /// Gets the number of radio buttons
  inline int           getNumber()                  const { return myNumber; }
  /// Gets a radio's name
  inline const string &getRadioName(int ind)        const { return myNameArray[ind]; }
  /// Gets a radio's value
  inline int           getRadioIntValue(int ind)    const { return myIntValueArray[ind]; }
  inline unsigned int           getRadioUIntValue(int ind)    const { return myUIntValueArray[ind]; }
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
  /// Sets a radio button
  void setRadio(int ind,const string &name,int value,MvDomain_e domain);
  void setRadio(int ind,const string &name,unsigned int value,MvDomain_e domain);
  /// Sets a radio button
  void setRadio(int ind,const string &name,double value,MvDomain_e domain);
  /// Sets a radio button
  void setRadio(int ind,const string &name,const string &value,MvDomain_e domain);
  /// Sets flag whether to retain the comments or not (RETAIN_LIST_COMMENTS)
  void setDoPrefixValue(bool  doprefixvalue){ myDoPrefixValue = doprefixvalue; }
  /// Sets whether a radio option is enum type or not
  void setEnumValueFlag(bool enumvalueflag) { myEnumValueFlag = enumvalueflag;}
  /// Gets the enum flag
  bool getEnumValueFlag() const {return myEnumValueFlag;}
  
  /// this is handled only for int, float, uint types, for other datatypes, any change done here for these datatypes,
  /// should be reflected outside (for string, bool) also
  template <typename TYPE>
  bool checkValuePresent(TYPE value) const
  {
      bool found = false;
      for (int i = 0; i < myNumber; i++)
      {
          if (myValueType == VTYPE_INT)
          {
              int a_value = getRadioIntValue(i);
              if (value == a_value)
              {
                  found = true;
                  break;
              }
          }
          else if (myValueType == VTYPE_UINT)
          {
              unsigned int a_value = getRadioUIntValue(i);
              if (value == a_value)
              {
                  found = true;
                  break;
              }
          }
          else if (myValueType == VTYPE_FLOAT)
          {
              double a_value = getRadioFloatValue(i);
              if (value == a_value)
              {
                  found = true;
                  break;
              }
          }
      }
      return found;
  }

  
//@}

public:
//@{
 /// Sets the type of values
  void           setValueType(int value)                {  myValueType = (value_type_e )value; }
  /// Sets the number of radio buttons
  void           setNumber(int value)                   {  myNumber = value; }

/// Sets a radio's name
  void setRadioName(int nb)
  {
      if(nb>0)
        myNameArray = new string[myNumber];
      else
        myNameArray = NULL;
  }
/// Gets a radio's domain
  void    setRadioDomain(int nb)
  {
      if(nb>0)
          myDomainArray = new MvDomain_e[myNumber];
      else
          myDomainArray = NULL;
  }
//@}

protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

protected: // Datas
  value_type_e  myValueType;
  int           myNumber;
  string       *myNameArray;
  union {
    int        *myIntValueArray;
    unsigned int        *myUIntValueArray;
    double     *myFloatValueArray;
    string     *myStringValueArray;
  };
  MvDomain_e   *myDomainArray; 
  bool          myDoPrefixValue;
  bool          myEnumValueFlag;
};



#endif //MV_DATA_RADIO_FEATURE_H




