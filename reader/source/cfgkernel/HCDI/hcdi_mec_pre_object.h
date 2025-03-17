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

#ifndef HCDI_MEC_PRE_OBJECT_H
#define HCDI_MEC_PRE_OBJECT_H

#define PseudoAttribute_t  void
#define PseudoKeywordMap_t void
#define PseudoDescriptor_t void
#define PseudoExpression_t void

#include <UTILS/mv_stl_various.h>
#include <UTILS/mv_string.h>
#include <KERNEL/cfg_kernel.h>
#include <limits.h>
#include <float.h>
#include "hcdi.h"
#include "KERNEL_BASE/Structure_descriptor.h"

typedef unsigned int MYOBJ_INT;

/// Class for stoking the attributes of an object
class HC_DATA_DLL_API IMECPreObject {

 public: /** @name Constructors & destructor */
  //@{
  /// Constructor 
  IMECPreObject() {}        
  /// Destructor
  virtual ~IMECPreObject() {}
  //@}

  
 private: // Initializations
  virtual void InitAttributes()=0;
  virtual void InitAttributes(const IMECPreObject &pre_object) = 0;
  
  
 public:  /** @name Copying data */
  //@{
  /// Reserving memory before copying
  virtual void Reserve(const IMECPreObject &pre_object) = 0;
  /// Copying
  virtual void Copy(const IMECPreObject &pre_object) = 0;
  //@}
   
  
 public:
     typedef pair<string, int>                             MyParamKey_t;
     typedef pair< string, void * >                          MyParamNameEntPtr_t;
     typedef map<MyParamKey_t, MyParamNameEntPtr_t>       MyParamMap_t;
     typedef map<MyParamKey_t, bool>                       MyNegateParamMap_t;

  /// Attribute type
  enum MyAttributeType_s {
    ATY_UNKNOWN,
    ATY_SINGLE,
    ATY_ARRAY,
    ATY_LAST
  };
  /// Attribute type
  typedef enum MyAttributeType_s  MyAttributeType_e;

  /// Value type
  enum MyValueType_s {
    VTY_UNKNOWN,
    VTY_BOOL,
    VTY_INT,
    VTY_UINT,
    VTY_FLOAT,
    VTY_STRING,
    VTY_OBJECT,
    VTY_LAST
  };
  /// Value type
  typedef enum MyValueType_s  MyValueType_e;
 public: /** @name Input access */
  //@{
  /// Sets the file index
  virtual void SetFileIndex(int ind) = 0;
  /// Sets the component index
  virtual void SetComponentIndex(int ind) = 0; 
  /// Sets the subdeck index
  virtual void SetSubdeckIndex(int ind) = 0; 
  /// Sets the kernel full type
  virtual void SetKernelFullType(const char *full_type) = 0;
  /// Sets the input full type
  virtual void SetInputFullType(const char *full_type) = 0;
  /// Sets the input title
  virtual void SetTitle(const char *title) = 0;
  /// Sets the id
  virtual void SetId(MYOBJ_INT id) = 0;
  virtual void SetOrderId(int orderId) = 0;
  virtual void SetIdOffset(int id_offset)  = 0;
  /// Sets the unit id
  virtual void SetUnitId(int unit_id) = 0;      
  
  /// Sets the crypting method
  virtual void SetCryptingMethod(int crypt_method) = 0;
  /// Sets the crypting reference
  virtual void SetCryptingReference(const char *crypt_ref) = 0;
  
  /// Sets the crypted data
  virtual void SetCryptedData(const char *crypt_data) = 0;
  
  
  /// Setting the Radioss format version
  virtual void SetRadiossFormat(int format)  = 0;
  /// Reserving memory for attributes of given type
  virtual void Reserve(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype,int a_nb_attributes) = 0;
  /** Adding an integer single value.
   * @return 0 normal case, an attribute with the given keyword did not yet exist.
   * @return 1 attribute with the given keyword did already exist, it is set to the new value with SetIntValue
   */
  virtual int AddBoolValue(const char *skeyword,bool value) = 0;
  virtual int AddIntValue(const char *skeyword,int value) = 0;
  virtual int AddUIntValue(const char *skeyword,unsigned int value) = 0;
  /** Adding a float single value.
   * @return 0 normal case, an attribute with the given keyword did not yet exist.
   * @return 1 attribute with the given keyword did already exist, it is set to the new value with SetFloatValue
   */
  virtual int AddFloatValue(const char *skeyword,double value) = 0;
  /** Adding a string single value.
   * @return 0 normal case, an attribute with the given keyword did not yet exist.
   * @return 1 attribute with the given keyword did already exist, it is set to the new value with SetStringValue
   */
  virtual int AddStringValue(const char *skeyword,const char *value) = 0;
  /** Adding an object single value.
   * @return 0 normal case, an attribute with the given keyword did not yet exist.
   * @return 1 attribute with the given keyword did already exist, it is set to the new value with SetObjectValue
   */
  virtual int AddObjectValue(const char *skeyword,const char *otype,MYOBJ_INT id,int a_ind=-1) = 0;
  virtual int AddObjectValue(const char *skeyword,const char *otype,const char *name,int a_ind=-1) = 0;
  virtual void SetBoolValue(int attrib_index,bool value) = 0;
  /// Setting an integer single value
  virtual void SetIntValue(int attrib_index,int value) = 0;
  virtual void SetUIntValue(int attrib_index,unsigned int value) = 0;

  /// Setting an float single value
  virtual void SetFloatValue(int attrib_index,double value) = 0;
  /// Setting an string single value
  virtual void SetStringValue(int attrib_index,const char *value) = 0;
  /// Setting an object single value
  virtual void SetObjectValue(int attrib_index,const char *otype, MYOBJ_INT id,int ind=-1) = 0;
  virtual void SetObjectValue(int attrib_index,const char *otype,const char *name, int ind=-1) = 0;
  virtual int AddBoolArray(const char *skeyword,int nb_values) = 0;
  /// Adding an array of integers
  virtual int AddIntArray(const char *skeyword,int nb_values) = 0;
  virtual int AddUIntArray(const char *skeyword,unsigned int nb_values) = 0;
  /// Adding an array of floats
  virtual int AddFloatArray(const char *skeyword,int nb_values) = 0;
  /// Adding an array of strings
  virtual int AddStringArray(const char *skeyword,int nb_values) = 0;
  /// Adding an array of objects
  virtual int AddObjectArray(const char *skeyword,int nb_values) = 0;
  virtual void AddBoolValue(const char *skeyword,int i,bool value) = 0;
  /// Adding an integer value into an array
  virtual void AddIntValue(const char *skeyword,int i,int value) = 0;
  virtual void AddUIntValue(const char *skeyword,int i,unsigned int value) = 0;
  /// Adding a float value into an array
  virtual void AddFloatValue(const char *skeyword,int i,double value) = 0;
  /// Adding a string value into an array
  virtual void AddStringValue(const char *skeyword,int i,const char *value) = 0;
  /// Adding an object value into an array
  virtual void AddObjectValue(const char *skeyword,int i,const char *otype,MYOBJ_INT id,int ind=-1) = 0;
  virtual void AddObjectValue(const char *skeyword, int i, const char *otype,const char *name, int ind = -1) = 0;
  /// Adding an integer value into an array
  virtual void AddIntValues(const char *skeyword,int i0,int nb_values,const int *value_tab) = 0;
  virtual void AddUIntValues(const char *skeyword,int i0,int nb_values,const unsigned int *value_tab) = 0;
  /// Adding a float value into an array
  virtual void AddFloatValues(const char *skeyword,int i0,int nb_values,const double *value_tab) = 0;
  /// Adding a string value into an array
  virtual void AddStringValues(const char *skeyword,int i0,int nb_values,char * const *value_tab) = 0;
  /// Adding an object value into an array
  virtual void AddObjectValues(const char *skeyword,int i0,const char *otype,int nb_values,const MYOBJ_INT *id_tab,const int *ind_tab=NULL) = 0;
  virtual void AddObjectValues(const char *skeyword,int i0,const char *otype,int nb_values,const char **name,const int *ind_tab=NULL) = 0;
  /// Adding an object value into an array
  virtual void AddObjectValues(const char *skeyword,int i0,int nb_values,const char **otype_tab,const MYOBJ_INT *id_tab,const int *ind_tab=NULL) = 0;
  virtual void AddObjectValues(const char *skeyword,int i0,int nb_values,const char **otype_tab,const char **name, const int *ind_tab=NULL) = 0;
  virtual void SetBoolValue(int attrib_index,int i,bool value) = 0;
  /// Setting an integer value into an array
  virtual void SetIntValue(int attrib_index,int i,int value) = 0;
  virtual void SetUIntValue(int attrib_index,int i,unsigned int value) = 0;

  /// Setting an float value into an array
  virtual void SetFloatValue(int attrib_index,int i,double value) = 0;
  /// Setting an string value into an array
  virtual void SetStringValue(int attrib_index,int i,const char *value) = 0;
  /// Setting an object value into an array
  virtual void SetObjectValue(int attrib_index,int i,const char *otype,MYOBJ_INT id,int ind=-1) = 0;
  virtual void SetObjectValue(int attrib_index,int i,const char *otype,const char *name=NULL,int ind=-1) = 0;
  /// Resizing an array
  virtual void resizeArray(IMECPreObject::MyValueType_e vtype,const char *skeyword,int nb_values) = 0;
  /// Resizing an array
  virtual void resizeArray(IMECPreObject::MyValueType_e vtype,int attrib_index,int nb_values) = 0;
  /** Compressing a main array<br>
      '...' are for secondary arrays (NULL for finishing)
  */
  virtual void CompressMain(const char *main_skw,const char *size_skw,int nb_secondarys,char **secondary_skw_tab) = 0;
  virtual void SetEntitySubtypeTypeIdentifier(int s_identifier) = 0;
  virtual IMECPreObject* GetSubPreObject(int id) = 0;
  //@}

 public: /** @name Output access */
  //@{
  /// Gets the file index
  virtual  int         GetFileIndex()         const  = 0;
   /// Gets the component index
  virtual  int         GetComponentIndex()    const  = 0;
  /// Gets the subdeck index
  virtual  int         GetSubdeckIndex()    const  = 0;

  /// Gets the kernel full type
  virtual  const char *GetKernelFullType()    const  = 0;
  /// Gets the input full type
  virtual  const char *GetInputFullType()     const  = 0;
  /// Gets the input title
  virtual const char *GetTitle()             const  = 0;
  /// Gets the id
  virtual MYOBJ_INT         GetId()          const  = 0;
  virtual int         GetOrderId()           const = 0;
  /// Gets the unit id
  virtual int         GetUnitId()            const  = 0;
  
  /// Gets the crypting method
  virtual int GetCryptingMethod()            const  = 0;
  /// Sets the crypting reference
  virtual const char *GetCryptingReference() const  = 0;
  
  /// Sets the crypted data
  virtual const char *GetCryptedData() const  = 0;
  
  
  /// Gets  Radioss Format
  virtual int GetRadiossFormat() const  = 0;
  virtual int GetIdOffset() const  = 0;
  /// Gets the value type of an attribute of given keyword
  virtual MyValueType_e GetValueType(IMECPreObject::MyAttributeType_e atype,const char *skeyword) const = 0;
  /// Gets the number of attributes of the given types
  virtual int GetNbAttributes(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype) const = 0;
  /// Gets the keyword of the attribute of given types and index
  virtual const char *GetKeyword(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype,int ind) const = 0;
  /// Gets the index of the attribute of given types and keyword
  virtual int GetIndex(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype,const char *skeyword) const = 0;
  virtual int GetIndex(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype,const string &skeyword) const = 0;
  virtual bool GetBoolValue(const char *skeyword) const = 0;
  /// Gets an integer single value (attribute of given keyword)
  virtual int GetIntValue(const char *skeyword) const = 0;
  virtual unsigned int GetUIntValue(const char *skeyword) const = 0;
  /// Gets a float single value (attribute of given keyword)
  virtual double GetFloatValue(const char *skeyword) const = 0;
  /// Gets a string single value (attribute of given keyword)
  virtual const char *GetStringValue(const char *skeyword) const = 0;
  /// Gets an bool single value (attribute of given index)
  virtual bool GetBoolValue(int attrib_ind) const = 0;
  /// Gets an integer single value (attribute of given index)
  virtual int GetIntValue(int attrib_ind) const = 0;
  /// Gets an unsigned integer single value (attribute of given index)
  virtual unsigned int GetUIntValue(int attrib_ind) const = 0;
  /// Gets a float single value (attribute of given index)
  virtual double GetFloatValue(int attrib_ind) const = 0;
  /// Gets a string single value (attribute of given index)
  virtual const char *GetStringValue(int attrib_ind) const = 0;
  /// Gets the type of an object single value (attribute of given index)
  virtual const char *GetObjectType(int attrib_ind) const = 0;
  /// Gets the id of an object single value (attribute of given index)
  virtual MYOBJ_INT GetObjectId(int attrib_ind) const = 0;
  /// Gets the index of an object single value (attribute of given index)
  virtual int GetObjectIndex(int attrib_ind) const = 0;
  /// Gets the name of an object single value (attribute of given index)
  virtual const char* GetObjectName(int attrib_ind) const = 0;
  /// Gets the size of an array (of given type and index)
  virtual int GetNbValues(IMECPreObject::MyValueType_e vtype,int attrib_ind) const = 0;
  virtual bool GetBoolValue(int attrib_ind,int i) const = 0;
  
  virtual int GetIntValue(int attrib_ind,int i) const = 0;
  virtual unsigned int GetUIntValue(int attrib_ind,int i) const = 0;
  
  virtual double GetFloatValue(int attrib_ind,int i) const = 0;
  
  virtual const char *GetStringValue(int attrib_ind,int i) const = 0;
  
  virtual const char *GetObjectType(int attrib_ind,int i) const = 0;
  
  virtual MYOBJ_INT GetObjectId(int attrib_ind,int i) const = 0;
  
  virtual int GetObjectIndex(int attrib_ind,int i) const = 0;
  
  virtual const char* GetObjectName(int attrib_ind, int i) const = 0;

  virtual int GetEntitySubtypeTypeIdentifier() const = 0;
 
  //@}

 public: /* @name Kernel relative methods */
  //@{
  /// Initialization
  virtual void Init(const PseudoDescriptor_t *descr_p) = 0;
  
  /// Evaluation of expression
  virtual bool EvaluateExpression(const PseudoExpression_t *expr_p,const PseudoDescriptor_t *descr_p,int ind=-1) const = 0;
  virtual bool EvaluateExpression(const PseudoExpression_t *expr_p,const PseudoDescriptor_t *descr_p, set<int> *ikeywordchecklst, set<int>  *ikeywordlst, int ind=-1) const = 0;
  virtual bool EvaluateMCDSExpression(const PseudoExpression_t* expr_p,
                                      const PseudoDescriptor_t* descr_p,
                                      int                       ind=-1) const = 0;
  virtual double GetExpressionValue(const PseudoDescriptor_t  *descr_p, const int &ikeyword, const int &loc_ind) const = 0;
  virtual bool GetExpressionValue(const PseudoDescriptor_t* descr_p, const int& ikeyword, const int& loc_ind, double& val, string& val_str) const = 0;
  
  //@}

 public: /* @name Report */
  //@{
  /// Gets a report (including solver names, if a descriptor is passed)
  virtual char *GetReport(const PseudoDescriptor_t *descr_p = NULL) const = 0;
  //@}

 public: /* @name Parameter object relative methods */
  //@{
  /// Get the name of the parameter
  virtual string GetParameterName(const char* skeyword, int index=-1, bool *is_negated_p = NULL) const = 0;
  /// set the name of the parameter
  virtual void SetParameterName(const char* param_name, const char* skeyword,int index=-1,  bool is_negated = false, void *ent_ptr=nullptr) = 0;
  ///check if the parameter is used for an attribute
  virtual bool IsParameter(const char *skeyword,  int index=-1) const = 0;
  /// Set the parameter usage as negated
  virtual void SetParameterNegated(const char* skeyword,int index=-1, bool is_param_negated = false) = 0;
  ///check if the parameter is used with negative sign
  virtual bool IsParameterNegated(const char *skeyword,  int index=-1) const = 0;
  /// Get the name of the parameter used for id
  virtual string GetParameterIdName() const = 0;
  /// set name of the parameter used for id
  virtual void SetParameterIdName(const char* param_name) = 0;
  ///check if the parameter is used for the id field
  virtual bool IsParameterId() const = 0;
  virtual   MyParamMap_t GetParameterMap() = 0;
//@}
 public:
  virtual void  SetSubobject(IMECPreObject *subobj)  = 0;
  virtual void  GetSubobject(vector<IMECPreObject *> &subobj)  const = 0; // deprecated
  virtual const vector<IMECPreObject *>& GetSubobject() const = 0; // preferred
  virtual const vector <string>& GetUserComment() = 0;
  virtual void  AddUserComment(string const& comment) = 0;
  virtual void  ClearUserComment() = 0;

  virtual int GetEntityType() const = 0;
  virtual void SetEntityType(int entity_type) = 0;
  virtual void EraseFloatArray(const char *skeyword)=0;

  //method to erase attribute keywords
  virtual void EraseAttributeKeyword(MyAttributeType_e a_atype, MyValueType_e a_vtype, const string &skeyword) = 0;

  virtual void ClearAllAttribValues(bool is_subobj_arr = false) = 0;

  //
  virtual void SetCryptedKeyData(const char *keydata)=0;
  virtual const char* GetCryptedKeyData() const = 0;
public: //Export options
  virtual void SetHmCommentState(bool flag) = 0;
  virtual int GetHmCommnetState() const = 0;
  virtual string &GetHeaderLine() = 0;
  virtual void SetHeaderLine(string &str) = 0;
  virtual void setConfigType(unsigned int configtype)  = 0;
  virtual void setHmType(const PseudoDescriptor_t *descr_p)  = 0;
  virtual unsigned int  getConfigType() const = 0;
  virtual unsigned int  getHmType() const = 0;
  virtual void setIdPool(short int id_pool) = 0;
  virtual short int getIdPool() = 0;
 public:
  virtual void Release() = 0;

    //static PureList *table;     
    //void *operator new(size_t);
    //static void PopulatePureList(PureList *matTable) { if (table) matTable->Add(table); }
};
typedef IMECPreObject* IMECPREOBJECT;
EXTERNC HC_DATA_DLL_API IMECPREOBJECT HCDI_GetPreObjectHandle(const char *kernel_full_type, const char *input_full_type,const char *title,int id,int unit_id);
EXTERNC HC_DATA_DLL_API IMECPREOBJECT HCDI_GetPreObjectHandleFromOtherPreObject(IMECPreObject *pre_obj);
HC_DATA_DLL_API void HCDI_ReleasePreObjectHandle(IMECPreObject *p_pre_obj);
EXTERNC HC_DATA_DLL_API obj_type_e HCDI_GetHCObjectType(string fulltype);
//extern "C" HC_DATA_DLL_API int HCDIWriteDataToHM(IMECPreObject *ipreobj);
EXTERNC HC_DATA_DLL_API void HCDI_AddArrayAttributesToPreObject(IMECPreObject& pre_object, const PseudoDescriptor_t* descr_p, int a_arr_ikw, int size, bool update_coon_size_ikey = true);
EXTERNC HC_DATA_DLL_API bool HCDI_UpdatePreObjectConnectedSizeIkeywords(IMECPreObject& pre_object, const PseudoDescriptor_t* descr_p, int size_ikey);
// To update preobject INT, UINT, FLOAT attributes
EXTERNC HC_DATA_DLL_API bool HCDI_UpdatePreObjectValue(IMECPreObject& pre_object, const PseudoDescriptor_t* descr_p, int attrib_ikey, double value, std::string& str_val, int ind);
EXTERNC HC_DATA_DLL_API void HCDI_DescTypeToPreobjType(attribute_type_e desc_a_type, value_type_e desc_v_type, IMECPreObject::MyAttributeType_e& atype, IMECPreObject::MyValueType_e& vtype);
//To update preobject STRING attributes
EXTERNC HC_DATA_DLL_API bool HCDI_UpdatePreObjectStringValue(IMECPreObject& pre_object, const PseudoDescriptor_t* descr_p, int attrib_ikey, const std::string& str_val, int ind);
// Get the value of any attribute(int,uint,float,double,objectid) as a string
EXTERNC HC_DATA_DLL_API void HCDI_GetPreObjectValueAsString(IMECPreObject& pre_object, int att_index, std::string& str_val, IMECPreObject::MyValueType_e& vtype, int index = -1);
// Get the value of any attribute as double(used for attributes other than value type string)
EXTERNC HC_DATA_DLL_API void HCDI_GetPreObjectValue(IMECPreObject& pre_object, int att_index, double& val, std::string& str_val, IMECPreObject::MyValueType_e& vtype, int index = -1);

#endif //HCDI_MEC_PRE_OBJECT_H
