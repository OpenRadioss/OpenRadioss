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
#ifndef MV_DATA_FEATURE_H
#define MV_DATA_FEATURE_H



#include <UTILS/mv_list.h>
#include <UTILS/mv_iostream.h>
#include <UTILS/mv_stl_various.h>
#include <UTILS/mv_string.h>


#include "mv_domain.h" 

class MvDescriptor_t;
/** @name Data features */

//@{

/// Data feature types
enum MvDataFeatureType_s {
  /** */ DFT_UNKNOWN,
  /** */ DFT_SUPPORT,
  /** */ DFT_SCALAR,
  /** */ DFT_COND_SCALAR,     
  /** */ DFT_SIZE,
  /** */ DFT_SIZE_RADIO,
  /** */ DFT_FILE,
  /** */ DFT_DIR,
  /** */ DFT_SKEW,
  /** */ DFT_FUNCTION,
  /** */ DFT_COND_FUNCTION,   
  /** */ DFT_TRIPLE,
  /** */ DFT_POINT,
  /** */ DFT_FLAG,
  /** */ DFT_FLAG_LIST,
  /** */ DFT_RADIO,
  /** */ DFT_RADIO_ARRAY,
  /** */ DFT_DATA,
  /** */ DFT_UNIT,
   DFT_SUBOBJECT,  
  /** */ DFT_SENSOR,
  /** */ DFT_ACCELEROMETER,
  /** */ DFT_SEPARATOR,
  /** */ DFT_DEACTIVATION_BCS,
  /** */ DFT_DYNAMIC_ARRAY,
  /** */ DFT_STATIC_ARRAY,
  /** */ DFT_ARRAY_TO_SINGLE, 
  /** */ DFT_DYNAMIC_MATRIX,  
  /** */ DFT_IF,
  /** */ DFT_APPEND,
  /** */ DFT_SELECTION,
  /** */ DFT_ASSIGN,
  /** */ DFT_LAST
};

/// Data feature types
typedef enum MvDataFeatureType_s MvDataFeatureType_e;

enum feature_attribute_type_s
{
    FEATURE_ATTRIBUTE_UNKNOWN = 0,
    FEATURE_ATTRIBUTE_EDITABLE,
    FEATURE_ATTRIBUTE_VISIBLE,
    FEATURE_ATTRIBUTE_DISPLAY_STATUS,
    FEATURE_ATTRIBUTE_PARAMETERIZED,
    FEATURE_ATTRIBUTE_NB
};
typedef enum feature_attribute_type_s feature_attribute_type_e;


/// Data feature base class
class MvDataFeature_t {

  friend class MvDescriptor_t;
  friend class MvDataDynamicArrayFeature_t;
  friend class MvDataStaticArrayFeature_t;
  friend class MvDataRadioArrayFeature_t;
  friend class MvDataIfFeature_t;
  friend class MvDataArrayToSingleFeature_t; 
  friend class MvDataDynamicMatrixFeature_t; 

public: /** @name Constructors & destructor */
//@{
  /// Constructor
  MvDataFeature_t(MvDataFeatureType_e type,const string &title);
  /// Destructor
  virtual ~MvDataFeature_t();
//@}
  
public: /** @name Accessors */
//@{
  /// Gets the type
  inline MvDataFeatureType_e  getType()    const { return myType; }
  /// Gets the name
  inline const string        &getTitle()   const { return myTitle; }
  /// Returns true if the feature is optional
  inline bool                 isOptional() const { return myIsOptional; }
  /// Sets the option flag
  inline void                 setOptional(bool is_optional) { myIsOptional=is_optional; }
  /// Returns true if the feature is graphical (the graphic depends on the feature)
  inline bool                 isGraphical() const { return myIsGraphical; }
  /// Sets the graphic flag
  inline void                 setGraphical(bool is_graphical) { myIsGraphical=is_graphical; }
  /// Gets the reference integer keyword
  virtual int                 getIKeyword() const { return 0; }

  inline int getLevel()    const { return myLevel; }

  /// Sets the visible flag
  inline void                 setVisible(bool value) { myIsVisible = value; myHasVisibleDef = true; }
  /// Gets the visible flag
  inline bool                 getVisible(bool* is_default = NULL) const
  {
      if (is_default != NULL)
      {
          *is_default = myHasVisibleDef;
      }
      return myIsVisible;
  }
  /// Sets the editable flag
  inline void                 setEditable(bool value) { myIsEditable = value; myHasEditableDef = true; }
  /// Gets the editable flag
  inline bool                 getEditable(bool* is_default = NULL) const
  {
      if (is_default != NULL)
      {
          *is_default = myHasEditableDef;
      }
      return myIsEditable;
  }
  /// Sets the parameterized flag
  inline void                 setParameterized(bool value) { myIsParameterized = value; myHasParameterizedDef = true; }
  /// Gets the parameterized flag
  inline bool                 getParameterized(bool* is_default = NULL) const
  {
      if (is_default != NULL)
      {
          *is_default = myHasParameterizedDef;
      }
      return myIsParameterized;
  }
  /// Sets the option flag
  inline void                 setDisplayStatus(int value) { myDisplayStatus = value; myHasDisplayStatusDef = true; }
  /// Gets the option flag
  inline int                 getDisplayStatus(bool* is_default = NULL) const
  {
      if (is_default != NULL)
      {
          *is_default = myHasDisplayStatusDef;
      }
      return myDisplayStatus;
  }

//@}


public: /** @name Locked domains */
//@{
  /// Adding a locked domain
  inline void addLockedDomain(MvDomain_e domain) { myLockedDomains|=domain; }
  /// Returns true if the feature is locked for one of the given domains
  bool isLocked(int domains) const { return (domains & myLockedDomains) ? true : false; }
//@}


public:
//@{
  void incrLevel() {myLevel++;}
  void setLevel(int value) {myLevel = value;}
  void setNextDataFeature(MvDataFeature_t *data_fp) { data_feature_next =  data_fp;  }
//@}
protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;
  ostream         &display_props(ostream &os) const; 

protected: // Datas
  MvDataFeatureType_e myType;
  string              myTitle;
  bool                myIsOptional;
  bool                myIsGraphical;
  int                 myLockedDomains; 
  int                 myLevel;
  MvDataFeature_t     *data_feature_next;
  bool                myIsVisible;
  bool                myIsEditable;
  bool                myIsParameterized;
  int                 myDisplayStatus;//0,1,2 for  DISPLAY_STATUS_OFF:DISPLAY_STATUS_ON:DISPLAY_STATUS_ALWAYS_ON
  bool                myHasVisibleDef;
  bool                myHasEditableDef;
  bool                myHasParameterizedDef;
  bool                myHasDisplayStatusDef;
};


/// List of data feature
typedef list<const MvDataFeature_t *> MvDataFeatureList_t;

/// Set of data feature
typedef set<const MvDataFeature_t *> MvDataFeatureSet_t;


//@}


#endif //MV_DATA_FEATURE_H




