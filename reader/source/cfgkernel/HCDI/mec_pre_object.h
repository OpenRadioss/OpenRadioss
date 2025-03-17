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

#ifndef MEC_PRE_OBJECT_H
#define MEC_PRE_OBJECT_H

#define PseudoAttribute_t  void
#define PseudoKeywordMap_t void
#define PseudoDescriptor_t void
#define PseudoExpression_t void

#include <UTILS/mv_stl_various.h>
#include <UTILS/mv_string.h>
#include "HCDI/hcdi_mec_pre_object.h"

/// Class for stoking the attributes of an object
class HC_DATA_DLL_API MECPreObject: public IMECPreObject {

 public: /** @name Constructors & destructor */
  //@{
  /// Constructor
  MECPreObject(const char *kernel_full_type="",const char *input_full_type="",const char *title="", MYOBJ_INT id=0,int unit_id=0);         
  /// Constructor
  MECPreObject(const IMECPreObject &pre_object);
  /// Destructor
  ~MECPreObject();
  //@}

  
 private: // Initializations
  void InitAttributes();
  void InitAttributes(const IMECPreObject &pre_object);
  
 
  
 public:  /** @name Copying data */
  //@{
  /// Reserving memory before copying
  void Reserve(const IMECPreObject &pre_object);
  /// Copying
  void Copy(const IMECPreObject &pre_object);
  //@}
  

 public:


 public: /** @name Input access */
  //@{
  /// Sets the file index
  void SetFileIndex(int ind);
  /// Sets the component index
  void SetComponentIndex(int ind); 
  /// Sets the subdeck index
  void SetSubdeckIndex(int ind); 
  /// Sets the kernel full type
  void SetKernelFullType(const char *full_type);
  /// Sets the input full type
  void SetInputFullType(const char *full_type);
  /// Sets the input title
  void SetTitle(const char *title);
  /// Sets the id
  void SetId(MYOBJ_INT id);
  void SetOrderId(int orderId) { myOrderId = orderId; }

  void SetIdOffset(int id_offset);
  /// Sets the unit id
  void SetUnitId(int unit_id);      
  
  /// Sets the crypting method
  void SetCryptingMethod(int crypt_method);
  /// Sets the crypting reference
  void SetCryptingReference(const char *crypt_ref);
  
  /// Sets the crypted data
  void SetCryptedData(const char *crypt_data);
  
  
  /// Setting the Radioss format version
  void SetRadiossFormat(int format) ;
  /// Reserving memory for attributes of given type
  void Reserve(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype,int a_nb_attributes);
  /** Adding an integer single value.
   * @return 0 normal case, an attribute with the given keyword did not yet exist.
   * @return 1 attribute with the given keyword did already exist, it is set to the new value with SetIntValue
   */
  int AddBoolValue(const char *skeyword,bool value);
  int AddIntValue(const char *skeyword,int value);
  int AddUIntValue(const char *skeyword,unsigned int value);
  /** Adding a float single value.
   * @return 0 normal case, an attribute with the given keyword did not yet exist.
   * @return 1 attribute with the given keyword did already exist, it is set to the new value with SetFloatValue
   */
  int AddFloatValue(const char *skeyword,double value);
  /** Adding a string single value.
   * @return 0 normal case, an attribute with the given keyword did not yet exist.
   * @return 1 attribute with the given keyword did already exist, it is set to the new value with SetStringValue
   */
  int AddStringValue(const char *skeyword,const char *value);
  /** Adding an object single value.
   * @return 0 normal case, an attribute with the given keyword did not yet exist.
   * @return 1 attribute with the given keyword did already exist, it is set to the new value with SetObjectValue
   */
  int AddObjectValue(const char *skeyword,const char *otype,MYOBJ_INT id,int a_ind=-1);
  int AddObjectValue(const char *skeyword, const char *otype,const char *name, int a_ind = -1);
  void SetBoolValue(int attrib_index,bool value);
  /// Setting an integer single value
  void SetIntValue(int attrib_index,int value);
  void SetUIntValue(int attrib_index,unsigned int value);
  /// Setting an float single value
  void SetFloatValue(int attrib_index,double value);
  /// Setting an string single value
  void SetStringValue(int attrib_index,const char *value);
  /// Setting an object single value
  void SetObjectValue(int attrib_index,const char *otype,MYOBJ_INT id,int ind=-1);
  void SetObjectValue(int attrib_index, const char *otype,const char *name,int ind = -1);
  int AddBoolArray(const char *skeyword,int nb_values);
  /// Adding an array of integers
  int AddIntArray(const char *skeyword,int nb_values);
  int AddUIntArray(const char *skeyword,unsigned int nb_values);
  /// Adding an array of floats
  int AddFloatArray(const char *skeyword,int nb_values);
  /// Adding an array of strings
  int AddStringArray(const char *skeyword,int nb_values);
  /// Adding an array of objects
  int AddObjectArray(const char *skeyword,int nb_values);
  void AddBoolValue(const char *skeyword,int i,bool value);
  /// Adding an integer value into an array
  void AddIntValue(const char *skeyword,int i,int value);
  void AddUIntValue(const char *skeyword,int i, unsigned int value);
  /// Adding a float value into an array
  void AddFloatValue(const char *skeyword,int i,double value);
  /// Adding a string value into an array
  void AddStringValue(const char *skeyword,int i,const char *value);
  /// Adding an object value into an array
  void AddObjectValue(const char *skeyword,int i,const char *otype,MYOBJ_INT id,int ind=-1);
  void AddObjectValue(const char *skeyword, int i,const char *otype,const char *name, int ind=-1);
  /// Adding an integer value into an array
  void AddIntValues(const char *skeyword,int i0,int nb_values,const int *value_tab);
  void AddUIntValues(const char *skeyword,int i0,int nb_values,const unsigned int *value_tab);
  /// Adding a float value into an array
  void AddFloatValues(const char *skeyword,int i0,int nb_values,const double *value_tab);
  /// Adding a string value into an array
  void AddStringValues(const char *skeyword,int i0,int nb_values,char * const *value_tab);
  /// Adding an object value into an array
  void AddObjectValues(const char *skeyword,int i0,const char *otype,int nb_values,const MYOBJ_INT *id_tab,const int *ind_tab=NULL);
  void AddObjectValues(const char *skeyword, int i0, const char *otype, int nb_values, const char **name_tab, const int *ind_tab = NULL);
  /// Adding an object value into an array
  void AddObjectValues(const char *skeyword,int i0,int nb_values,const char **otype_tab,const MYOBJ_INT *id_tab,const int *ind_tab=NULL);
  void AddObjectValues(const char *skeyword,int i0,int nb_values,const char **otype_tab,const char **name_tab,const int *ind_tab=NULL);
  
  void SetBoolValue(int attrib_index,int i,bool value);
  /// Setting an integer value into an array
  void SetIntValue(int attrib_index,int i,int value);
  void SetUIntValue(int attrib_index,int i,unsigned int value);
  /// Setting an float value into an array
  void SetFloatValue(int attrib_index,int i,double value);
  /// Setting an string value into an array
  void SetStringValue(int attrib_index,int i,const char *value);
  /// Setting an object value into an array
  void SetObjectValue(int attrib_index,int i,const char *otype,MYOBJ_INT id,int ind=-1);
  void SetObjectValue(int attrib_index,int i,const char *otype,const char *name,int ind=-1);
  /// Resizing an array
  void resizeArray(IMECPreObject::MyValueType_e vtype,const char *skeyword,int nb_values);
  /// Resizing an array
  void resizeArray(IMECPreObject::MyValueType_e vtype,int attrib_index,int nb_values);
  /** Compressing a main array<br>
      '...' are for secondary arrays (NULL for finishing)
  */
  void CompressMain(const char *main_skw,const char *size_skw,int nb_secondarys,char **secondary_skw_tab);
  void SetEntitySubtypeTypeIdentifier(int s_identifier);
  IMECPreObject* GetSubPreObject(int id);
  //@}

 public: /** @name Output access */
  //@{
  /// Gets the file index
  int         GetFileIndex()         const { return myFileIndex; }
   /// Gets the component index
  int         GetComponentIndex()    const { return myComponentIndex; }
  /// Gets the subdeck index
  int         GetSubdeckIndex()    const { return mySubdeckIndex; }

  /// Gets the kernel full type
  const char *GetKernelFullType()    const { return myKernelFullType; }
  /// Gets the input full type
  const char *GetInputFullType()     const { return myInputFullType; }
  /// Gets the input title
  const char *GetTitle()             const { return myTitle; }
  /// Gets the id
  MYOBJ_INT         GetId()                const { return myId; }
  int         GetOrderId()           const { return myOrderId; }

  /// Gets the unit id
  int         GetUnitId()            const { return myUnitId; }  
  
  /// Gets the crypting method
  int GetCryptingMethod()            const { return myCryptingMethod; }
  /// Sets the crypting reference
  const char *GetCryptingReference() const { return myCryptingRef; }
  
  /// Sets the crypted data
  const char *GetCryptedData() const { return myCryptedData; }
  
  
  /// Gets  Radioss Format
  int GetRadiossFormat() const { return myRadiossFormat; }
  int GetIdOffset() const { return myIdOffset; }
  /// Gets the value type of an attribute of given keyword
  IMECPreObject::MyValueType_e GetValueType(IMECPreObject::MyAttributeType_e atype,const char *skeyword) const;
  /// Gets the number of attributes of the given types
  int GetNbAttributes(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype) const;
  /// Gets the keyword of the attribute of given types and index
  const char *GetKeyword(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype,int ind) const;
  /// Gets the index of the attribute of given types and keyword
  int GetIndex(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype,const char *skeyword) const;
  int GetIndex(IMECPreObject::MyAttributeType_e atype, IMECPreObject::MyValueType_e vtype, const string &keyword) const;
  bool GetBoolValue(const char *skeyword) const;
  /// Gets an integer single value (attribute of given keyword)
  int GetIntValue(const char *skeyword) const;
  unsigned int GetUIntValue(const char *skeyword) const;
  /// Gets a float single value (attribute of given keyword)
  double GetFloatValue(const char *skeyword) const;
  /// Gets a string single value (attribute of given keyword)
  const char *GetStringValue(const char *skeyword) const;
  bool GetBoolValue(int attrib_ind) const;
  /// Gets an integer single value (attribute of given index)
  int GetIntValue(int attrib_ind) const;
  unsigned int GetUIntValue(int attrib_ind) const;
  /// Gets a float single value (attribute of given index)
  double GetFloatValue(int attrib_ind) const;
  /// Gets a string single value (attribute of given index)
  const char *GetStringValue(int attrib_ind) const;
  /// Gets the type of an object single value (attribute of given index)
  const char *GetObjectType(int attrib_ind) const;
  /// Gets the id of an object single value (attribute of given index)
  MYOBJ_INT GetObjectId(int attrib_ind) const;
  /// Gets the index of an object single value (attribute of given index)
  int GetObjectIndex(int attrib_ind) const;
  /// Gets the name of an object single value (attribute of given index)
  const char* GetObjectName(int attrib_ind) const;
  /// Gets the size of an array (of given type and index)
  int GetNbValues(IMECPreObject::MyValueType_e vtype,int attrib_ind) const;
  bool GetBoolValue(int attrib_ind,int i) const;
  
  int GetIntValue(int attrib_ind,int i) const;
  unsigned int GetUIntValue(int attrib_ind,int i) const;

  
  double GetFloatValue(int attrib_ind,int i) const;
  
  const char *GetStringValue(int attrib_ind,int i) const;
  
  const char *GetObjectType(int attrib_ind,int i) const;
  
  MYOBJ_INT GetObjectId(int attrib_ind,int i) const;
  
  int GetObjectIndex(int attrib_ind,int i) const;
  
  const char* GetObjectName(int attrib_ind, int i) const;

  int GetEntitySubtypeTypeIdentifier() const;
  //@}

 public: /* @name Kernel relative methods */
  //@{
  /// Initialization
  void Init(const PseudoDescriptor_t *descr_p);
  
  /// Evaluation of expression
  bool EvaluateExpression(const PseudoExpression_t *expr_p,const PseudoDescriptor_t *descr_p,int ind=-1) const;
  bool EvaluateExpression(const PseudoExpression_t *expr_p,const PseudoDescriptor_t *descr_p, set<int> *ikeywordchecklst, set<int>  *ikeywordlst, int ind=-1) const;
  bool EvaluateMCDSExpression(const PseudoExpression_t* expr_p,
                              const PseudoDescriptor_t* descr_p,
                              int                       ind=-1) const;
  double GetExpressionValue(const PseudoDescriptor_t  *descr_p, const int &ikeyword, const int &loc_ind) const;
  bool GetExpressionValue(const PseudoDescriptor_t* descr_p, const int& ikeyword, const int& loc_ind, double& val, string& val_str) const;
  
  //@}

 public: /* @name Report */
  //@{
  /// Gets a report (including solver names, if a descriptor is passed)
  char *GetReport(const PseudoDescriptor_t *descr_p = NULL) const;
  //@}

 public: /* @name Parameter object relative methods */
  //@{
  /// Get the name of the parameter
  string GetParameterName(const char* skeyword, int index=-1, bool *is_negated_p = NULL) const;
  /// set the name of the parameter
  void SetParameterName(const char* param_name, const char* skeyword,int index=-1,  bool is_negated = false, void* ent_ptr = nullptr);
  ///check if the parameter is used for an attribute
  bool IsParameter(const char *skeyword,  int index=-1) const;
  /// Set the parameter usage as negated
  void SetParameterNegated(const char* skeyword,int index=-1, bool is_param_negated = false);
  ///check if the parameter is used with negative sign
  bool IsParameterNegated(const char *skeyword,  int index=-1) const;
  /// Get the name of the parameter used for id
  string GetParameterIdName() const;
  /// set name of the parameter used for id
  void SetParameterIdName(const char* param_name);
  ///check if the parameter is used for the id field
  bool IsParameterId() const;
  ///returns the myParameters
  IMECPreObject::MyParamMap_t   GetParameterMap();
//@}

 private: //Data
  int                 myFileIndex;
  int                 myComponentIndex; 
  int                 mySubdeckIndex; 
  char               *myKernelFullType;
  char               *myInputFullType;
  char               *myTitle;
  MYOBJ_INT           myId;
  int                 myOrderId;
  int                 myUnitId;         
  PseudoAttribute_t  *myAttributes[2][6];
  PseudoKeywordMap_t *myKeywordMaps[2][6];
  int                 myCryptingMethod;    
  char               *myCryptingRef;       
  char               *myCryptedData;       
  char               *myCryptedKeyData;
  int                 myIdOffset; //for adhesive object
  int                 myRadiossFormat ; // for radioss case only and used only in few cases (GRxxx/BOX)
 public:
  void  SetSubobject(IMECPreObject *subobj) {  sub_preobj.push_back(subobj) ; }
  void  GetSubobject(vector<IMECPreObject *> &subobj) const {  subobj = sub_preobj ; }
  virtual const vector<IMECPreObject *>& GetSubobject() const { return sub_preobj ; }
  string    header_line;
 private:  //Parameter
  string    myParamIdName;
  MyParamMap_t                                         myParameters;
  MyNegateParamMap_t                                   myIsNegatedParameters;
  vector<IMECPreObject *>  sub_preobj;
 private: //HM entity type and identifier
  int                                                  entity_type;
  int                                                  identifier;   //subtype identifier
  unsigned int                                         myConfigType;
  unsigned int                                         myHmType;
  short int                                            myIdPool;
 private: 
  vector <string>                                      myUserComments;
    
 private:
  bool myHmCommentState;
 public:
  const vector <string>& GetUserComment();
  void  AddUserComment(string  const &comment);
  void  ClearUserComment();
  int  GetEntityType() const;
  void SetEntityType(int entity_type);
  void EraseFloatArray(const char *skeyword);
  //Method to remove attribute from preobject
  void EraseAttributeKeyword(IMECPreObject::MyAttributeType_e a_atype, IMECPreObject::MyValueType_e a_vtype, const string &skeyword);
  void ClearAllAttribValues(bool is_subobj_arr = false);
  void SetCryptedKeyData(const char *keydata);
  const char* GetCryptedKeyData() const;
public: //Export options
  void SetHmCommentState(bool flag) { myHmCommentState = flag;}
  int GetHmCommnetState() const {return myHmCommentState; }
  string &GetHeaderLine();
  void SetHeaderLine(string &str);
  void setConfigType(unsigned int configtype) ;
  void setHmType(const PseudoDescriptor_t *descr_p) ;
  unsigned int  getConfigType() const;
  unsigned int  getHmType() const;
  void setIdPool(short int id_pool) { myIdPool = id_pool; }
  short int getIdPool() { return myIdPool; }
 public:
  void Release() { delete this; }
};


#endif //MEC_PRE_OBJECT_H
