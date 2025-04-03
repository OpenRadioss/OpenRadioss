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

#include <UTILS/win32_utils.h>

#include <UTILS/mv_cstring.h>
#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <UTILS/memory_utils.h>
#include <UTILS/str_utils.h>
#include <KERNEL/mv_descriptor.h>
#include <KERNEL/mv_expression.h>
#include <KERNEL_BASE/expression_API.h>

#include <MESSAGE/msg_manager.h>
#include <KERNEL/mv_descriptor.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include <HCDI/hcdi_mec_pre_object.h>
#include "mec_pre_object.h"

struct find_id : std::unary_function<IMECPreObject*, bool> {
    int id;
    find_id(int id):id(id) { }
    bool operator()(const IMECPreObject *obj) const {
        return obj->GetId() == id;
    }
};

static int getHMEntityTypeFromHM(obj_type_e type_hc);
static void HMIN_entity_write(int hm_type, const IMECPreObject &pre_object);
template <class value_t> class LocAttribute_t {
public:
  LocAttribute_t() {}
  LocAttribute_t(const string &keyword,const value_t &value) :  myKeyword(keyword), myValue(value) {}
public:
  string  myKeyword;
  value_t myValue;
};

class LocObjectTrack_t {
public:
  LocObjectTrack_t(const char *otype=NULL,MYOBJ_INT id=0,int ind=-1) : myOType(otype), myId(id), myIndex(ind), myName("") {}
  LocObjectTrack_t(const char *otype, const char *name, int ind = -1) : myOType(otype), myId(0), myIndex(ind), myName(name) { }
public:
  const char *myOType;
  MYOBJ_INT   myId;
  int         myIndex;
  string      myName;
};


typedef LocAttribute_t<int>              LocIntSingleAttribute_t;
typedef LocAttribute_t<unsigned int>     LocUIntSingleAttribute_t;
typedef LocAttribute_t<double>           LocFloatSingleAttribute_t;
typedef LocAttribute_t<string>           LocStringSingleAttribute_t;
typedef LocAttribute_t<LocObjectTrack_t> LocObjectSingleAttribute_t;

typedef vector<LocIntSingleAttribute_t>    LocIntSingleAttributes_t;
typedef vector<LocUIntSingleAttribute_t>   LocUIntSingleAttributes_t;
typedef vector<LocFloatSingleAttribute_t>  LocFloatSingleAttributes_t;
typedef vector<LocStringSingleAttribute_t> LocStringSingleAttributes_t;
typedef vector<LocObjectSingleAttribute_t> LocObjectSingleAttributes_t;

typedef vector<int>              LocIntArray_t;
typedef vector<unsigned int>     LocUIntArray_t;
typedef vector<double>           LocFloatArray_t;
typedef vector<string>           LocStringArray_t;
typedef vector<LocObjectTrack_t> LocObjectArray_t;

typedef LocAttribute_t<LocIntArray_t>    LocIntArrayAttribute_t;
typedef LocAttribute_t<LocUIntArray_t>    LocUIntArrayAttribute_t;
typedef LocAttribute_t<LocFloatArray_t>  LocFloatArrayAttribute_t;
typedef LocAttribute_t<LocStringArray_t> LocStringArrayAttribute_t;
typedef LocAttribute_t<LocObjectArray_t> LocObjectArrayAttribute_t;

typedef vector<LocIntArrayAttribute_t>    LocIntArrayAttributes_t;
typedef vector<LocUIntArrayAttribute_t>    LocUIntArrayAttributes_t;
typedef vector<LocFloatArrayAttribute_t>  LocFloatArrayAttributes_t;
typedef vector<LocStringArrayAttribute_t> LocStringArrayAttributes_t;
typedef vector<LocObjectArrayAttribute_t> LocObjectArrayAttributes_t;

typedef map<string,int> LocKeywordMap_t;


static bool loc_evaluate_attribute_expression(const IMECPreObject &pre_object,
					      const expression_t      *expr_p,
					      const IDescriptor       *descr_p,
                          set<int>                *ikeywordchecklst =NULL,
                           set<int>                *ikeywordlst=NULL,
					      int                      ind=-1);
static bool loc_evaluate_logical_expression(const IMECPreObject &pre_object,
					    const expression_t      *expr_p,
					    const IDescriptor       *descr_p,
                        set<int>                *ikeywordchecklst =NULL,
                         set<int>                *ikeywordlst=NULL,
					    int                      ind=-1);

static bool loc_eval_int(int lvalue,comparator_e op,int rvalue);
static bool loc_eval_uint(unsigned int lvalue,comparator_e op,unsigned int rvalue);
static bool loc_eval_float(double lvalue,comparator_e op,double rvalue);
static bool loc_eval_string(const char *lvalue,comparator_e op,const char *rvalue);


/* --------- Constructors & destructor --------- */
//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      Constructor
//Parameters:
//      const char *kernel_full_type: object's full type in the kernel
//      const char *input_full_type: object's full type in the input file
//      const char *title: object's title
//      int id:  object's id
//      In the input file, the format is typically:
//      type/subtype/id/title (in the version 4x)
//      type/subtype/id
//      title                 (after version 4x)
//Note:
//      Usually, full type is composed of type + subtype, the format is like
//      "type/subtype". The difference between kernel_full_type and input_full_type
//      is , the former is used internally and the latter is used in the
//      solver. In the most of the time, this two full type is 
//      same thing but it exists some cases that they are different. That's why we 
//      need both two of them. For example, kernel_full_type and input_full_type of
//      the skew,frame are different.   
//Return value:
//      None
//Design:
//Modification History:

//--///////////////////////////////////////////////////////////////////////////  
MECPreObject::MECPreObject(const char *kernel_full_type,const char *input_full_type,const char *title, MYOBJ_INT id,int unit_id) :
  myFileIndex(0),
  myComponentIndex(0),
  mySubdeckIndex(0),
  myKernelFullType(kernel_full_type!=NULL ? strdup(kernel_full_type) : NULL),
  myInputFullType(input_full_type!=NULL ? strdup(input_full_type) : NULL),
  myTitle(title!=NULL ? strdup(title) : strdup("")),
  myId(id),
  myUnitId(unit_id),   
  myCryptingMethod(0),
  myCryptingRef(NULL),
  myCryptedData(NULL),  
  myCryptedKeyData(NULL),
  myIdOffset(0),
  myParamIdName(""),
  myHmCommentState(false),
  header_line(""),
  myOrderId(0),
  myIdPool(0),
  myRadiossFormat(0),
  entity_type(0),
  identifier(0)
{//Begin MECPreObject::MECPreObject
  InitAttributes();
  myConfigType = 0;
  myHmType = 0;
}//End MECPreObject::MECPreObject

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      Constructor
//Parameters:
//      const MECPreObject &pre_object: pre_object used to write or read data
//      into/from the model.  
//Return value:
//      None
//Design:
//Modification History:

//--///////////////////////////////////////////////////////////////////////////  
MECPreObject::MECPreObject(const IMECPreObject &pre_object) :
  myFileIndex(pre_object.GetFileIndex()),
  myComponentIndex(pre_object.GetComponentIndex()),
  mySubdeckIndex(pre_object.GetSubdeckIndex()),
  myKernelFullType(pre_object.GetKernelFullType()!=NULL ? strdup(pre_object.GetKernelFullType()) : NULL),
  myInputFullType(pre_object.GetInputFullType()!=NULL ? strdup(pre_object.GetInputFullType()) : NULL),
  myTitle(pre_object.GetTitle()!=NULL ? strdup(pre_object.GetTitle()) : NULL),
  myId(pre_object.GetId()),
  myUnitId(pre_object.GetUnitId()),   
  myCryptingMethod(pre_object.GetCryptingMethod()),
  myCryptingRef(pre_object.GetCryptingReference()!=NULL ? strdup(pre_object.GetCryptingReference()) : NULL),
  myCryptedData(pre_object.GetCryptedData()!=NULL ? strdup(pre_object.GetCryptedData()) : NULL),
  myCryptedKeyData(pre_object.GetCryptedKeyData()!=NULL ? strdup(pre_object.GetCryptedKeyData()) : NULL),
  myIdOffset(pre_object.GetIdOffset()),
  myParamIdName(""),
  myHmCommentState(false),
  header_line(""),
  myIdPool(0)
{//Begin MECPreObject::MECPreObject

  InitAttributes(pre_object);
  Copy(pre_object);
  myConfigType = 0;
  myHmType = 0;
}//End MECPreObject::MECPreObject

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      Destructor
//Parameters:  
//Return value:
//      None
//Design:
//Modification History:

//--///////////////////////////////////////////////////////////////////////////  
MECPreObject::~MECPreObject() 
{//Begin MECPreObject::~MECPreObject

  // Headers
  myfree(myKernelFullType);
  myfree(myInputFullType);
  myfree(myTitle);
  // Single values
  delete ((LocIntSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1]));
  delete ((LocIntSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1]));
  delete ((LocUIntSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1]));
  delete ((LocFloatSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1]));
  delete ((LocStringSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1]));
  delete ((LocObjectSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1]));
  // Array values
  delete ((LocIntArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1]));
  delete ((LocIntArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1]));
  delete ((LocUIntArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1]));
  delete ((LocFloatArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1]));
  delete ((LocStringArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1]));
  delete ((LocObjectArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1]));
  // Keyword maps
  int a_nb_atypes=ATY_LAST-ATY_UNKNOWN-1;
  int a_nb_vtypes=VTY_LAST-VTY_UNKNOWN-1;
  for(int i=0;i<a_nb_atypes;++i) for(int j=0;j<a_nb_vtypes;++j) {
    delete ((LocKeywordMap_t *)(myKeywordMaps[i][j]));
  }
  // Crypting
  myfree(myCryptingRef); 
  myfree(myCryptedData); 
  myfree(myCryptedKeyData);
}//End MECPreObject::~MECPreObject


/* --------- Initializations --------- */

void MECPreObject::InitAttributes() {
  // Single values
  myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1]    = (PseudoAttribute_t *)(new LocIntSingleAttributes_t());
  myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1]    = (PseudoAttribute_t *)(new LocIntSingleAttributes_t());
  myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1]    = (PseudoAttribute_t *)(new LocUIntSingleAttributes_t());

  myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1]  = (PseudoAttribute_t *)(new LocFloatSingleAttributes_t());
  myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1] = (PseudoAttribute_t *)(new LocStringSingleAttributes_t());
  myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1] = (PseudoAttribute_t *)(new LocObjectSingleAttributes_t());
  // Array values
  myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1]    = (PseudoAttribute_t *)(new LocIntArrayAttributes_t());
  myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1]    = (PseudoAttribute_t *)(new LocIntArrayAttributes_t());
  myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1]    = (PseudoAttribute_t *)(new LocUIntArrayAttributes_t());

  myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1]  = (PseudoAttribute_t *)(new LocFloatArrayAttributes_t());
  myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1] = (PseudoAttribute_t *)(new LocStringArrayAttributes_t());
  myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1] = (PseudoAttribute_t *)(new LocObjectArrayAttributes_t());
  // Single keyword maps
  int a_nb_atypes=ATY_LAST-ATY_UNKNOWN-1;
  int a_nb_vtypes=VTY_LAST-VTY_UNKNOWN-1;
  for(int i=0;i<a_nb_atypes;++i) for(int j=0;j<a_nb_vtypes;++j) {
    myKeywordMaps[i][j]=(PseudoKeywordMap_t *)(new LocKeywordMap_t());
  }
}

void MECPreObject::InitAttributes(const IMECPreObject &pre_object) {
  InitAttributes();
  Reserve(pre_object);
}


void MECPreObject::Reserve(const IMECPreObject &pre_object) {
  // Single values
  const MECPreObject &a_pre_object = (const MECPreObject &)pre_object;
  size_t a_nb_attributes=((LocIntSingleAttributes_t *)(a_pre_object.myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_BOOL,(int)a_nb_attributes);

  a_nb_attributes=((LocIntSingleAttributes_t *)(a_pre_object.myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_INT,(int)a_nb_attributes);

  a_nb_attributes=((LocUIntSingleAttributes_t *)(a_pre_object.myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_UINT,(int)a_nb_attributes);

  a_nb_attributes=((LocFloatSingleAttributes_t *)(a_pre_object.myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_FLOAT,(int)a_nb_attributes);
  a_nb_attributes=((LocStringSingleAttributes_t *)(a_pre_object.myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_STRING,(int)a_nb_attributes);
  a_nb_attributes=((LocObjectSingleAttributes_t *)(a_pre_object.myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_OBJECT,(int)a_nb_attributes);
  // Array values
  a_nb_attributes=((LocIntArrayAttributes_t *)(a_pre_object.myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_BOOL,(int)a_nb_attributes);

  a_nb_attributes=((LocIntArrayAttributes_t *)(a_pre_object.myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,(int)a_nb_attributes);

  a_nb_attributes=((LocUIntArrayAttributes_t *)(a_pre_object.myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_UINT,(int)a_nb_attributes);

  a_nb_attributes=((LocFloatArrayAttributes_t *)(a_pre_object.myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_FLOAT,(int)a_nb_attributes);
  a_nb_attributes=((LocStringArrayAttributes_t *)(a_pre_object.myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_STRING,(int)a_nb_attributes);
  a_nb_attributes=((LocObjectArrayAttributes_t *)(a_pre_object.myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1]))->size();
  Reserve(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_OBJECT,(int)a_nb_attributes);
}

/* --------- Copying data --------- */
void MECPreObject::Copy(const IMECPreObject &pre_object) {
  int i,j,k,l;
  int a_nb_atypes  = ATY_LAST-ATY_UNKNOWN-1;
  int a_nb_vtypes  = VTY_LAST-VTY_UNKNOWN-1;
  //
  SetId(pre_object.GetId());
  SetTitle(pre_object.GetTitle());
  //
  for(i=0;i<a_nb_atypes;++i) {
    IMECPreObject::MyAttributeType_e a_atype=(IMECPreObject::MyAttributeType_e)(ATY_UNKNOWN+i+1);
    //
    for(j=0;j<a_nb_vtypes;++j) {
      IMECPreObject::MyValueType_e a_vtype         = (IMECPreObject::MyValueType_e)(VTY_UNKNOWN+j+1);
      int           a_nb_attributes = pre_object.GetNbAttributes(a_atype,a_vtype);
      //
      for(k=0;k<a_nb_attributes;++k) {
	const char *a_keyword=pre_object.GetKeyword(a_atype,a_vtype,k);
	//
	switch(a_atype) {
	case ATY_SINGLE:
	  {
	    switch(a_vtype) {
	    case VTY_BOOL:
	      AddBoolValue(a_keyword,pre_object.GetBoolValue(k));
	      break;
	    case VTY_INT:
	      AddIntValue(a_keyword,pre_object.GetIntValue(k));
	      break;
	    case VTY_UINT:
	      AddUIntValue(a_keyword,pre_object.GetIntValue(k));
	      break;
	    case VTY_FLOAT:
	      AddFloatValue(a_keyword,pre_object.GetFloatValue(k));
	      break;
	    case VTY_STRING:
	      AddStringValue(a_keyword,pre_object.GetStringValue(k));
	      break;
	    case VTY_OBJECT:
            if(pre_object.GetObjectName(k)[0] != '\0')
	            AddObjectValue(a_keyword,pre_object.GetObjectType(k),pre_object.GetObjectName(k),pre_object.GetObjectIndex(k));
            else
	      AddObjectValue(a_keyword,pre_object.GetObjectType(k),pre_object.GetObjectId(k),pre_object.GetObjectIndex(k));

	      break;
	    default:
	      // Wrong type of value
	      break;
	    }
	  }
	  break;
	case ATY_ARRAY:
	  {
	    int a_nb_values=pre_object.GetNbValues(a_vtype,k);
	    //
	    switch(a_vtype) {
	    case VTY_BOOL:
	      AddBoolArray(a_keyword,a_nb_values);
	      for(l=0;l<a_nb_values;++l) SetBoolValue(k,l,pre_object.GetBoolValue(k,l));
	      break;
	    case VTY_INT:
	      AddIntArray(a_keyword,a_nb_values);
	      for(l=0;l<a_nb_values;++l) SetIntValue(k,l,pre_object.GetIntValue(k,l));
	      break;
	    case VTY_UINT:
	      AddUIntArray(a_keyword,(unsigned int)a_nb_values);
	      for(l=0;l<a_nb_values;++l) SetUIntValue(k,l,pre_object.GetUIntValue(k,l));
	      break;
	    case VTY_FLOAT:
	      AddFloatArray(a_keyword,a_nb_values);
	      for(l=0;l<a_nb_values;++l) SetFloatValue(k,l,pre_object.GetFloatValue(k,l));
	      break;
	    case VTY_STRING:
	      AddStringArray(a_keyword,a_nb_values);
	      for(l=0;l<a_nb_values;++l) SetStringValue(k,l,pre_object.GetStringValue(k,l));
	      break;
	    case VTY_OBJECT:
	      AddObjectArray(a_keyword,a_nb_values);
	      for(l=0;l<a_nb_values;++l) {
		const char *a_otype = pre_object.GetObjectType(k,l);
        const char *a_name  = pre_object.GetObjectName(k, l);
		int         a_id    = pre_object.GetObjectId(k,l);
		int         a_index = pre_object.GetObjectIndex(k,l);
        if(a_name[0] != '\0')
		   SetObjectValue(k,l,a_otype,a_name,a_index);
        else
		SetObjectValue(k,l,a_otype,a_id,a_index);
	      }
	      break;
	    default:
	      // Wrong type of value
	      break;
	    }
	  }
	  break;
	default:
	  // Wrong type of attribute
	  break;
	}
      }
    }
  }  
  const MECPreObject &a_pre_object = (const MECPreObject &) pre_object;
  myParameters = a_pre_object.myParameters;   
  myParamIdName = a_pre_object.myParamIdName;
  myIsNegatedParameters = a_pre_object.myIsNegatedParameters;

  const vector<IMECPreObject *>& a_subobj = pre_object.GetSubobject();

  size_t a_size = a_subobj.size();

  for (size_t ii = 0; ii < a_size; ii++)
  {
      SetSubobject(a_subobj[ii]);
  }
}


/* --------- Input access --------- */
void MECPreObject::SetFileIndex(int ind) {
  myFileIndex=ind;
  vector<IMECPreObject*>::iterator iter_sub;
  vector<IMECPreObject*>::iterator iter_sub_beg = sub_preobj.begin();
  vector<IMECPreObject*>::iterator iter_sub_end = sub_preobj.end();
  for (iter_sub = iter_sub_beg; iter_sub != iter_sub_end; ++iter_sub)
  {
      MECPreObject* sub_obj = (MECPreObject*)(*iter_sub);
      if(sub_obj != NULL)
          sub_obj->SetFileIndex(ind);
}
}
void MECPreObject::SetRadiossFormat(int format) {
  myRadiossFormat=format;
}


void MECPreObject::SetComponentIndex(int ind) {
  myComponentIndex=ind;
}


void MECPreObject::SetSubdeckIndex(int ind) 
{
    mySubdeckIndex=ind;
    vector<IMECPreObject*>::iterator iter_sub;
    vector<IMECPreObject*>::iterator iter_sub_beg = sub_preobj.begin();
    vector<IMECPreObject*>::iterator iter_sub_end = sub_preobj.end();
    for (iter_sub = iter_sub_beg; iter_sub != iter_sub_end; ++iter_sub)
    {
        MECPreObject* sub_obj = (MECPreObject*)(*iter_sub);
        if (sub_obj != NULL)
            sub_obj->SetSubdeckIndex(ind);
    }
}

void MECPreObject::SetIdOffset(int id_offset)
{ 
    myIdOffset = id_offset; 
    vector<IMECPreObject*>::iterator iter_sub;
    vector<IMECPreObject*>::iterator iter_sub_beg = sub_preobj.begin();
    vector<IMECPreObject*>::iterator iter_sub_end = sub_preobj.end();
    for (iter_sub = iter_sub_beg; iter_sub != iter_sub_end; ++iter_sub)
    {
        MECPreObject* sub_obj = (MECPreObject*)(*iter_sub);
        if (sub_obj != NULL)
            sub_obj->SetIdOffset(id_offset);
    }
}
//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      Set Kernel Full Type
//Parameters:  
//      const char *full_type: the Kernel Full Type need to be set
//Return value:
//      None
//Design:
//Modification History:

//--///////////////////////////////////////////////////////////////////////////
void MECPreObject::SetKernelFullType(const char *full_type) 
{//Begin MECPreObject::SetKernelFullType

  myfree(myKernelFullType);
  myKernelFullType=strdup(full_type);
  
}//End MECPreObject::SetKernelFullType

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      Set Input Full Type
//Parameters:  
//      const char *full_type: the Input Full Type need to be set
//Return value:
//      None
//Design:
//Modification History:

//--/////////////////////////////////////////////////////////////////////////// 
void MECPreObject::SetInputFullType(const char *full_type)
{//Begin MECPreObject::SetInputFullType

  myfree(myInputFullType);
  myInputFullType=strdup(full_type);
  
}//End MECPreObject::SetInputFullType

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      Set Title
//Parameters:  
//      const char *title: the title need to be set
//Return value:
//      None
//Design:
//Modification History:

//--/////////////////////////////////////////////////////////////////////////// 
void MECPreObject::SetTitle(const char *title) 
{//Begin MECPreObject::SetTitle
  myfree(myTitle);
  if(NULL != title) myTitle=strdup(title);
  else              myTitle=strdup("");
}//End MECPreObject::SetTitle

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      Set ID
//Parameters:  
//      int id: the Id need to be set
//Return value:
//      None
//Design:
//Modification History:

//--/////////////////////////////////////////////////////////////////////////// 
void MECPreObject::SetId(MYOBJ_INT id)
{//Begin MECPreObject::SetId

  myId=id;
  
}//End MECPreObject::SetId


void MECPreObject::SetUnitId(int unit_id)
{
  myUnitId=unit_id;
}



void MECPreObject::SetCryptingMethod(int crypt_method) {
  myCryptingMethod=crypt_method;
}



void MECPreObject::SetCryptingReference(const char *crypt_ref) {
  myfree(myCryptingRef);
  myCryptingRef=(crypt_ref==NULL ? NULL : strdup(crypt_ref));
}



void MECPreObject::SetCryptedData(const char *crypt_data) {
  myfree(myCryptedData);
  myCryptedData=(crypt_data==NULL ? NULL : strdup(crypt_data));
}

void MECPreObject::SetCryptedKeyData(const char *keydata)
{
    myfree(myCryptedKeyData);
    myCryptedKeyData=(keydata==NULL ? NULL : strdup(keydata));
}
const char* MECPreObject::GetCryptedKeyData() const
{
    return myCryptedKeyData;
}
void MECPreObject::Reserve(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype,int a_nb_attributes) {
  void *a_attributes_p=myAttributes[atype-ATY_UNKNOWN-1][vtype-VTY_UNKNOWN-1];
  //
  switch(atype) {
  case ATY_SINGLE:
    switch(vtype) {
    case VTY_BOOL:    ((LocIntSingleAttributes_t *)a_attributes_p)->reserve(a_nb_attributes);    break;
    case VTY_INT:    ((LocIntSingleAttributes_t *)a_attributes_p)->reserve(a_nb_attributes);    break;
    case VTY_UINT:    ((LocUIntSingleAttributes_t *)a_attributes_p)->reserve(a_nb_attributes);    break;
    case VTY_FLOAT:  ((LocFloatSingleAttributes_t *)a_attributes_p)->reserve(a_nb_attributes);  break;
    case VTY_STRING: ((LocStringSingleAttributes_t *)a_attributes_p)->reserve(a_nb_attributes); break;
    case VTY_OBJECT: ((LocObjectSingleAttributes_t *)a_attributes_p)->reserve(a_nb_attributes); break;
    default:
      // Wrong type of value
      break;
    }
    break;
  case ATY_ARRAY:
    switch(vtype) {
    case VTY_BOOL:    ((LocIntArrayAttributes_t *)a_attributes_p)->reserve(a_nb_attributes);    break;
    case VTY_INT:    ((LocIntArrayAttributes_t *)a_attributes_p)->reserve(a_nb_attributes);    break;
    case VTY_UINT:    ((LocUIntArrayAttributes_t *)a_attributes_p)->reserve(a_nb_attributes);    break;
    case VTY_FLOAT:  ((LocFloatArrayAttributes_t *)a_attributes_p)->reserve(a_nb_attributes);  break;
    case VTY_STRING: ((LocStringArrayAttributes_t *)a_attributes_p)->reserve(a_nb_attributes); break;
    case VTY_OBJECT: ((LocObjectArrayAttributes_t *)a_attributes_p)->reserve(a_nb_attributes); break;
    default:
      // Wrong type of value
      break;
    }
    break;
  default:
    // Wrong type of attribute
    break;
  }
}

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      Add Integer Value in the pre_object Object
//Parameters:  
//      const char *skeyword:string keyword indicates where the value will be added
//      int value : value to be added
//Return value:
//      int: if it can find the place matched string keyword
//Design:
//Modification History:

//--/////////////////////////////////////////////////////////////////////////// 
int MECPreObject::AddIntValue(const char *skeyword,int value) 
{ //Begin MECPreObject::AddIntValue

  
  
  int a_index=GetIndex(ATY_SINGLE,VTY_INT,skeyword);
  if (0 <= a_index) {
    SetIntValue(a_index,value);
    return 1;
  }
  
    
  void                     *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1];
  LocIntSingleAttributes_t &a_attributes   = (*((LocIntSingleAttributes_t *)a_attributes_p));
  LocKeywordMap_t          &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_SINGLE-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1])));
  string                    a_skeyword     = skeyword;
  //
  a_keyword_map[a_skeyword]=(int)(a_attributes.size());
  a_attributes.push_back(LocIntSingleAttribute_t(a_skeyword,value));
  return 0; 
}//End MECPreObject::AddIntValue

int MECPreObject::AddBoolValue(const char *skeyword,bool value) 
{ 
  int a_index=GetIndex(ATY_SINGLE,VTY_BOOL,skeyword);
  if (0 <= a_index) {
    SetBoolValue(a_index,value);
    return 1;
  }
    
  void                     *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1];
  LocIntSingleAttributes_t &a_attributes   = (*((LocIntSingleAttributes_t *)a_attributes_p));
  LocKeywordMap_t          &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_SINGLE-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1])));
  string                    a_skeyword     = skeyword;
  //
  a_keyword_map[a_skeyword]=(int)(a_attributes.size());
  if(value==true)
      a_attributes.push_back(LocIntSingleAttribute_t(a_skeyword,1));
  else
      a_attributes.push_back(LocIntSingleAttribute_t(a_skeyword,0));
  return 0;
}

int MECPreObject::AddUIntValue(const char *skeyword,unsigned int value) 
{ //Begin MECPreObject::AddUIntValue

  
  
  int a_index=GetIndex(ATY_SINGLE,VTY_UINT,skeyword);
  if (0 <= a_index) {
    SetUIntValue(a_index,value);
    return 1;
  }
  
    
  void                     *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1];
  LocUIntSingleAttributes_t &a_attributes   = (*((LocUIntSingleAttributes_t *)a_attributes_p));
  LocKeywordMap_t          &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_SINGLE-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1])));
  string                    a_skeyword     = skeyword;
  //
  a_keyword_map[a_skeyword]=(int)(a_attributes.size());
  a_attributes.push_back(LocUIntSingleAttribute_t(a_skeyword,value));
  return 0; 
}//End MECPreObject::AddIntValue
//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      Add Float Value in the pre_object Object
//Parameters:  
//      const char *skeyword:string keyword indicates where the value will be added
//      int value : value to be added
//Return value:
//      int: if it can find the place matched string keyword
//Design:
//Modification History:

//--/////////////////////////////////////////////////////////////////////////// 
int MECPreObject::AddFloatValue(const char *skeyword,double value) 
{//Begin MECPreObject::AddFloatValue

   
  
  int a_index=GetIndex(ATY_SINGLE,VTY_FLOAT,skeyword);
  if (0 <= a_index) {
    SetFloatValue(a_index,value);
    return 1;
  }
  
    
  void                       *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1];
  LocFloatSingleAttributes_t &a_attributes   = (*((LocFloatSingleAttributes_t *)a_attributes_p));
  LocKeywordMap_t            &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_SINGLE-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1])));
  string                      a_skeyword     = skeyword;
  //
  a_keyword_map[a_skeyword]=(int)(a_attributes.size());
  a_attributes.push_back(LocFloatSingleAttribute_t(a_skeyword,value));
  return 0; 
}//End MECPreObject::AddFloatValue

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      Add String Value in the pre_object Object
//Parameters:  
//      const char *skeyword:string keyword indicates where the value will be added
//      int value : value to be added
//Return value:
//      int: if it can find the place matched string keyword
//Design:
//Modification History:

//--/////////////////////////////////////////////////////////////////////////// 
int MECPreObject::AddStringValue(const char *skeyword,const char *value) 
{//Begin MECPreObject::AddStringValue

  
  
  int a_index=GetIndex(ATY_SINGLE,VTY_STRING,skeyword);
  if (0 <= a_index) {
    SetStringValue(a_index,value);
    return 1;
  }
  
    
  void                        *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1];
  LocStringSingleAttributes_t &a_attributes   = (*((LocStringSingleAttributes_t *)a_attributes_p));
  LocKeywordMap_t             &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_SINGLE-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1])));
  string                       a_skeyword     = skeyword;
  //
  a_keyword_map[a_skeyword]=(int)(a_attributes.size());
  a_attributes.push_back(LocStringSingleAttribute_t(a_skeyword,value));
  return 0; 
  
}//End MECPreObject::AddStringValue

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      Add String Value in the pre_object Object
//Parameters:  
//      const char *skeyword: string keyword 
//      const char *otype: object type
//      int id:  the value to be added
//      int ind: internal index
//Return value:
//      int: if it can find the place matched string keyword
//Design:
//Modification History:

//--/////////////////////////////////////////////////////////////////////////// 
int MECPreObject::AddObjectValue(const char *skeyword,
                                    const char *otype,
                                        MYOBJ_INT  id,
                                              int ind) 
{//Begin MECPreObject::AddObjectValue

  
  
  int a_index=GetIndex(ATY_SINGLE,VTY_OBJECT,skeyword);
  if (0 <= a_index) {
    SetObjectValue(a_index,otype,id,ind);
    return 1;
  }
  
    
  void                        *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1];
  LocObjectSingleAttributes_t &a_attributes   = (*((LocObjectSingleAttributes_t *)a_attributes_p));
  LocKeywordMap_t             &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_SINGLE-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1])));
  string                       a_skeyword     = skeyword;
  //
  a_keyword_map[a_skeyword]=(int)(a_attributes.size());
  a_attributes.push_back(LocObjectSingleAttribute_t(a_skeyword,LocObjectTrack_t(otype,id,ind)));
  return 0; 
  
}
//End MECPreObject::AddObjectValue
int MECPreObject::AddObjectValue(const char *skeyword,
    const char *otype,
    const char *name,
    int ind)
{
    int a_index = GetIndex(ATY_SINGLE, VTY_OBJECT, skeyword);
    if (0 <= a_index) {
        SetObjectValue(a_index, otype, name, ind);
        return 1;
    }
    void                        *a_attributes_p = myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1];
    LocObjectSingleAttributes_t &a_attributes = (*((LocObjectSingleAttributes_t *)a_attributes_p));
    LocKeywordMap_t             &a_keyword_map = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1])));
    string                       a_skeyword = skeyword;
    //
    a_keyword_map[a_skeyword] = (int)(a_attributes.size());
    a_attributes.push_back(LocObjectSingleAttribute_t(a_skeyword, LocObjectTrack_t(otype, name, ind)));
    return 0; 

}


void MECPreObject::SetBoolValue(int attrib_index,bool value) {
  void                     *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1];
  LocIntSingleAttributes_t &a_attributes   = (*((LocIntSingleAttributes_t *)a_attributes_p));
  LocIntSingleAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  if(value == true)
    a_attribute.myValue=1;
  else
    a_attribute.myValue=0;
}
void MECPreObject::SetIntValue(int attrib_index,int value) {
  void                     *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1];
  LocIntSingleAttributes_t &a_attributes   = (*((LocIntSingleAttributes_t *)a_attributes_p));
  LocIntSingleAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  a_attribute.myValue=value;
}
void MECPreObject::SetUIntValue(int attrib_index,unsigned int value) {
  void                     *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1];
  LocUIntSingleAttributes_t &a_attributes   = (*((LocUIntSingleAttributes_t *)a_attributes_p));
  LocUIntSingleAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  a_attribute.myValue=value;
}

void MECPreObject::SetFloatValue(int attrib_index,double value) {
  void                       *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1];
  LocFloatSingleAttributes_t &a_attributes   = (*((LocFloatSingleAttributes_t *)a_attributes_p));
  LocFloatSingleAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  a_attribute.myValue=value;
}

void MECPreObject::SetStringValue(int attrib_index,const char *value) {
  void                        *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1];
  LocStringSingleAttributes_t &a_attributes   = (*((LocStringSingleAttributes_t *)a_attributes_p));
  LocStringSingleAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  a_attribute.myValue=value;
}

void MECPreObject::SetObjectValue(int attrib_index,const char *otype, MYOBJ_INT id,int ind) {
  void                        *a_attributes_p = myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1];
  LocObjectSingleAttributes_t &a_attributes   = (*((LocObjectSingleAttributes_t *)a_attributes_p));
  LocObjectSingleAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  a_attribute.myValue=LocObjectTrack_t(otype,id,ind);
}

void MECPreObject::SetObjectValue(int attrib_index, const char *otype, const char *name, int ind) {
    void                        *a_attributes_p = myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1];
    LocObjectSingleAttributes_t &a_attributes = (*((LocObjectSingleAttributes_t *)a_attributes_p));
    LocObjectSingleAttribute_t  &a_attribute = a_attributes[attrib_index];
    //
    a_attribute.myValue = LocObjectTrack_t(otype, name, ind);
}

int MECPreObject::AddBoolArray(const char *skeyword,int nb_values) {
  int a_index=GetIndex(ATY_ARRAY,VTY_BOOL,skeyword);
  if (0 <= a_index) {
    resizeArray(VTY_BOOL, a_index, nb_values);
    return a_index;
  }
  void                    *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1];
  LocIntArrayAttributes_t &a_attributes   = (*((LocIntArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t         &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1])));
  string                   a_skeyword     = skeyword;
  //
  a_index=(int)(a_attributes.size());
  a_keyword_map[a_skeyword]=a_index;
  a_attributes.push_back(LocIntArrayAttribute_t(a_skeyword,LocIntArray_t()));
  a_attributes.back().myValue.resize(nb_values);
  return a_index;
}
int MECPreObject::AddIntArray(const char *skeyword,int nb_values) {
  int a_index=GetIndex(ATY_ARRAY,VTY_INT,skeyword);
  if (0 <= a_index) {
    resizeArray(VTY_INT, a_index, nb_values);
    return a_index;
  }
  void                    *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1];
  LocIntArrayAttributes_t &a_attributes   = (*((LocIntArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t         &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1])));
  string                   a_skeyword     = skeyword;
  //
  a_index=(int)(a_attributes.size());
  a_keyword_map[a_skeyword]=a_index;
  a_attributes.push_back(LocIntArrayAttribute_t(a_skeyword,LocIntArray_t()));
  a_attributes.back().myValue.resize(nb_values);
  return a_index;
}
int MECPreObject::AddUIntArray(const char *skeyword,unsigned int nb_values) {
  int a_index=GetIndex(ATY_ARRAY,VTY_UINT,skeyword);
  if (0 <= a_index) {
    resizeArray(VTY_UINT, a_index, nb_values);
    return a_index;
  }
  void                    *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1];
  LocUIntArrayAttributes_t &a_attributes   = (*((LocUIntArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t         &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1])));
  string                   a_skeyword     = skeyword;
  //
  a_index=(int)(a_attributes.size());
  a_keyword_map[a_skeyword]=a_index;
  a_attributes.push_back(LocUIntArrayAttribute_t(a_skeyword,LocUIntArray_t()));
  a_attributes.back().myValue.resize(nb_values);
  return a_index;
}

int MECPreObject::AddFloatArray(const char *skeyword,int nb_values) {
  int a_index=GetIndex(ATY_ARRAY,VTY_FLOAT,skeyword);
  if (0 <= a_index) {
    resizeArray(VTY_FLOAT, a_index, nb_values);
    return a_index;
  }
  void                      *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1];
  LocFloatArrayAttributes_t &a_attributes   = (*((LocFloatArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t           &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1])));
  string                     a_skeyword     = skeyword;
  //
  a_index=(int)(a_attributes.size());
  a_keyword_map[a_skeyword]=a_index;
  a_attributes.push_back(LocFloatArrayAttribute_t(a_skeyword,LocFloatArray_t()));
  a_attributes.back().myValue.resize(nb_values);
  return a_index;
}

int MECPreObject::AddStringArray(const char *skeyword,int nb_values) {
  int a_index=GetIndex(ATY_ARRAY,VTY_STRING,skeyword);
  if (0 <= a_index) {
    resizeArray(VTY_STRING, a_index, nb_values);
    return a_index;
  }
  void                       *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1];
  LocStringArrayAttributes_t &a_attributes   = (*((LocStringArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t            &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1])));
  string                      a_skeyword     = skeyword;
  //
  a_index=(int)(a_attributes.size());
  a_keyword_map[a_skeyword]=a_index;
  a_attributes.push_back(LocStringArrayAttribute_t(a_skeyword,LocStringArray_t()));
  a_attributes.back().myValue.resize(nb_values);
  return a_index;
}

int MECPreObject::AddObjectArray(const char *skeyword,int nb_values) {
  int a_index=GetIndex(ATY_ARRAY,VTY_OBJECT,skeyword);
  if (0 <= a_index) {
    resizeArray(VTY_OBJECT, a_index, nb_values);
    return a_index;
  }
  void                       *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1];
  LocObjectArrayAttributes_t &a_attributes   = (*((LocObjectArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t            &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1])));
  string                      a_skeyword     = skeyword;
  //
  a_index=(int)(a_attributes.size());
  a_keyword_map[a_skeyword]=a_index;
  a_attributes.push_back(LocObjectArrayAttribute_t(a_skeyword,LocObjectArray_t()));
  a_attributes.back().myValue.resize(nb_values);
  return a_index;
}
void MECPreObject::AddBoolValue(const char *skeyword,int i,bool value) {
  void                      *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1];
  LocIntArrayAttributes_t   &a_attributes   = (*((LocIntArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t           &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1])));
  string                     a_skeyword     = skeyword;
  int                        a_ind          = a_keyword_map[a_skeyword];
  LocIntArrayAttribute_t    &a_attribute    = a_attributes[a_ind];
  //
  a_attribute.myValue[i]=value;
}
void MECPreObject::AddIntValue(const char *skeyword,int i,int value) {
  void                      *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1];
  LocIntArrayAttributes_t   &a_attributes   = (*((LocIntArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t           &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1])));
  string                     a_skeyword     = skeyword;
  int                        a_ind          = a_keyword_map[a_skeyword];
  LocIntArrayAttribute_t    &a_attribute    = a_attributes[a_ind];
  //
  a_attribute.myValue[i]=value;
}
void MECPreObject::AddUIntValue(const char *skeyword,int i, unsigned int value) {
  void                      *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1];
  LocUIntArrayAttributes_t   &a_attributes   = (*((LocUIntArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t            &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1])));
  string                     a_skeyword     = skeyword;
  int                        a_ind          = a_keyword_map[a_skeyword];
  LocUIntArrayAttribute_t    &a_attribute    = a_attributes[a_ind];
  //
  a_attribute.myValue[i]=value;
}
void MECPreObject::AddFloatValue(const char *skeyword,int i,double value) {
  void                      *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1];
  LocFloatArrayAttributes_t &a_attributes   = (*((LocFloatArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t           &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1])));
  string                     a_skeyword     = skeyword;
  int                        a_ind          = a_keyword_map[a_skeyword];
  LocFloatArrayAttribute_t  &a_attribute    = a_attributes[a_ind];
  //
  a_attribute.myValue[i]=value;
}
void MECPreObject::AddStringValue(const char *skeyword,int i,const char *value) {
  void                       *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1];
  LocStringArrayAttributes_t &a_attributes   = (*((LocStringArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t            &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1])));
  string                      a_skeyword     = skeyword;
  int                         a_ind          = a_keyword_map[a_skeyword];
  LocStringArrayAttribute_t  &a_attribute    = a_attributes[a_ind];
  //
  a_attribute.myValue[i]=value;
}

void MECPreObject::AddObjectValue(const char *skeyword,int i,const char *otype, MYOBJ_INT id,int ind) {
  void                       *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1];
  LocObjectArrayAttributes_t &a_attributes   = (*((LocObjectArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t            &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1])));
  string                      a_skeyword     = skeyword;
  int                         a_ind          = a_keyword_map[a_skeyword];
  LocObjectArrayAttribute_t  &a_attribute    = a_attributes[a_ind];
  LocObjectTrack_t           &a_object       = a_attribute.myValue[i];
  //
  a_object.myOType = otype;
  a_object.myId    = id;
  a_object.myIndex = ind;
}  

void MECPreObject::AddObjectValue(const char *skeyword, int i, const char *otype,const char *name, int ind) {
    void                       *a_attributes_p = myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1];
    LocObjectArrayAttributes_t &a_attributes = (*((LocObjectArrayAttributes_t *)a_attributes_p));
    LocKeywordMap_t            &a_keyword_map = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1])));
    string                      a_skeyword = skeyword;
    int                         a_ind = a_keyword_map[a_skeyword];
    LocObjectArrayAttribute_t  &a_attribute = a_attributes[a_ind];
    LocObjectTrack_t           &a_object = a_attribute.myValue[i];
    //
    a_object.myOType = otype;
    a_object.myIndex = ind;
    a_object.myName = name;
}


void MECPreObject::AddIntValues(const char *skeyword,int i0,int nb_values,const int *value_tab) {
  void                     *a_attributes_p  = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1];
  LocIntArrayAttributes_t  &a_attributes    = (*((LocIntArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t          &a_keyword_map   = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1])));
  string                    a_skeyword      = skeyword;
  int                       a_ind           = a_keyword_map[a_skeyword];
  LocIntArrayAttribute_t   &a_attribute     = a_attributes[a_ind];
  //
  int i,j;
  for(i=i0,j=0;j<nb_values;++i,++j) a_attribute.myValue[i]=value_tab[j];
}
void MECPreObject::AddUIntValues(const char *skeyword,int i0,int nb_values,const unsigned int *value_tab) {
  void                     *a_attributes_p  = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1];
  LocUIntArrayAttributes_t  &a_attributes    = (*((LocUIntArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t          &a_keyword_map   = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1])));
  string                    a_skeyword      = skeyword;
  int                       a_ind           = a_keyword_map[a_skeyword];
  LocUIntArrayAttribute_t   &a_attribute     = a_attributes[a_ind];
  //
  int i,j;
  for(i=i0,j=0;j<nb_values;++i,++j) a_attribute.myValue[i]=value_tab[j];
}
void MECPreObject::AddFloatValues(const char *skeyword,int i0,int nb_values,const double *value_tab) {
  void                      *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1];
  LocFloatArrayAttributes_t &a_attributes   = (*((LocFloatArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t           &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1])));
  string                     a_skeyword     = skeyword;
  int                        a_ind          = a_keyword_map[a_skeyword];
  LocFloatArrayAttribute_t  &a_attribute    = a_attributes[a_ind];
  //
  int i,j;
  for(i=i0,j=0;j<nb_values;++i,++j) a_attribute.myValue[i]=value_tab[j];
}

void MECPreObject::AddStringValues(const char *skeyword,int i0,int nb_values,char * const *value_tab) {
  void                       *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1];
  LocStringArrayAttributes_t &a_attributes   = (*((LocStringArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t            &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1])));
  string                      a_skeyword     = skeyword;
  int                         a_ind          = a_keyword_map[a_skeyword];
  LocStringArrayAttribute_t  &a_attribute    = a_attributes[a_ind];
  //
  int i,j;
  for(i=i0,j=0;j<nb_values;++i,++j) a_attribute.myValue[i]=value_tab[j];
}

void MECPreObject::AddObjectValues(const char *skeyword,int i0,const char *otype,int nb_values,const MYOBJ_INT *id_tab,const int *ind_tab) {
  void                       *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1];
  LocObjectArrayAttributes_t &a_attributes   = (*((LocObjectArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t            &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1])));
  string                      a_skeyword     = skeyword;
  int                         a_ind          = a_keyword_map[a_skeyword];
  LocObjectArrayAttribute_t  &a_attribute    = a_attributes[a_ind];
  //
  int i,j;
  for(i=i0,j=0;j<nb_values;++i,++j) {
    LocObjectTrack_t &a_object=a_attribute.myValue[i];
    //
    a_object.myOType = otype;
    if (id_tab)
    a_object.myId    = id_tab[j];
    else
        a_object.myId = 0;

    a_object.myIndex = (ind_tab==NULL ? -1 : ind_tab[j]);
  }
}

void MECPreObject::AddObjectValues(const char *skeyword,int i0,const char *otype,int nb_values,const char **name_tab,const int *ind_tab) {
    void                       *a_attributes_p = myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1];
    LocObjectArrayAttributes_t &a_attributes = (*((LocObjectArrayAttributes_t *)a_attributes_p));
    LocKeywordMap_t            &a_keyword_map = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1])));
    string                      a_skeyword = skeyword;
    int                         a_ind = a_keyword_map[a_skeyword];
    LocObjectArrayAttribute_t  &a_attribute = a_attributes[a_ind];
    //
    int i, j;
    for (i = i0, j = 0; j<nb_values; ++i, ++j) {
        LocObjectTrack_t &a_object = a_attribute.myValue[i];
        //
        a_object.myOType = otype;

        if (name_tab[j])
            a_object.myName = name_tab[j];
        a_object.myIndex = (ind_tab == NULL ? -1 : ind_tab[j]);
    }
}



void MECPreObject::AddObjectValues(const char *skeyword,int i0,int nb_values,const char **otype_tab,const MYOBJ_INT *id_tab,const int *ind_tab) {
  void                       *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1];
  LocObjectArrayAttributes_t &a_attributes   = (*((LocObjectArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t            &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1])));
  string                      a_skeyword     = skeyword;
  int                         a_ind          = a_keyword_map[a_skeyword];
  LocObjectArrayAttribute_t  &a_attribute    = a_attributes[a_ind];
  //
  int i,j;
  for(i=i0,j=0;j<nb_values;++i,++j) {
    LocObjectTrack_t &a_object=a_attribute.myValue[i];
    //
    a_object.myOType = otype_tab[j];
    if (id_tab)
    a_object.myId    = id_tab[j];
    else
        a_object.myId = 0;
    a_object.myIndex = (ind_tab==NULL ? -1 : ind_tab[j]);
  }
}

void MECPreObject::AddObjectValues(const char *skeyword, int i0, int nb_values, const char **otype_tab, const char **name_tab, const int *ind_tab) {
    void                       *a_attributes_p = myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1];
    LocObjectArrayAttributes_t &a_attributes = (*((LocObjectArrayAttributes_t *)a_attributes_p));
    LocKeywordMap_t            &a_keyword_map = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1])));
    string                      a_skeyword = skeyword;
    int                         a_ind = a_keyword_map[a_skeyword];
    LocObjectArrayAttribute_t  &a_attribute = a_attributes[a_ind];
    //
    int i, j;
    for (i = i0, j = 0; j<nb_values; ++i, ++j) {
        LocObjectTrack_t &a_object = a_attribute.myValue[i];
        //
        a_object.myOType = otype_tab[j];
        if(name_tab[j])
           a_object.myName = name_tab[j];
        a_object.myIndex = (ind_tab == NULL ? -1 : ind_tab[j]);
    }
}

void MECPreObject::SetBoolValue(int attrib_index,int i,bool value) {
  void                    *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1];
  LocIntArrayAttributes_t &a_attributes   = (*((LocIntArrayAttributes_t *)a_attributes_p));
  LocIntArrayAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  if(value==true)
    a_attribute.myValue[i]=1;
  else
    a_attribute.myValue[i]=0;
}
void MECPreObject::SetIntValue(int attrib_index,int i,int value) {
  void                    *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1];
  LocIntArrayAttributes_t &a_attributes   = (*((LocIntArrayAttributes_t *)a_attributes_p));
  LocIntArrayAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  a_attribute.myValue[i]=value;
}
void MECPreObject::SetUIntValue(int attrib_index,int i,unsigned int value) {
  void                    *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1];
  LocUIntArrayAttributes_t &a_attributes   = (*((LocUIntArrayAttributes_t *)a_attributes_p));
  LocUIntArrayAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  a_attribute.myValue[i]=value;
}

void MECPreObject::SetFloatValue(int attrib_index,int i,double value) {
  void                      *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1];
  LocFloatArrayAttributes_t &a_attributes   = (*((LocFloatArrayAttributes_t *)a_attributes_p));
  LocFloatArrayAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  a_attribute.myValue[i]=value;
}

void MECPreObject::SetStringValue(int attrib_index,int i,const char *value) {
  void                       *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1];
  LocStringArrayAttributes_t &a_attributes   = (*((LocStringArrayAttributes_t *)a_attributes_p));
  LocStringArrayAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  a_attribute.myValue[i]=value;
}

void MECPreObject::SetObjectValue(int attrib_index,int i,const char *otype, MYOBJ_INT id,int ind) {
  void                       *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1];
  LocObjectArrayAttributes_t &a_attributes   = (*((LocObjectArrayAttributes_t *)a_attributes_p));
  LocObjectArrayAttribute_t  &a_attribute    = a_attributes[attrib_index];
  //
  LocObjectTrack_t &a_object=a_attribute.myValue[i];
  //
  a_object.myOType = otype;
  a_object.myId    = id;
  a_object.myIndex = ind;
}

void MECPreObject::SetObjectValue(int attrib_index, int i, const char *otype,const char *name,int ind) {
    void                       *a_attributes_p = myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1];
    LocObjectArrayAttributes_t &a_attributes = (*((LocObjectArrayAttributes_t *)a_attributes_p));
    LocObjectArrayAttribute_t  &a_attribute = a_attributes[attrib_index];
    //
    LocObjectTrack_t &a_object = a_attribute.myValue[i];
    //
    a_object.myOType = otype;
    a_object.myName = name;
    a_object.myIndex = ind;
}



void MECPreObject::resizeArray(IMECPreObject::MyValueType_e vtype,const char *skeyword,int nb_values) {
  int a_attrib_index=GetIndex(ATY_ARRAY,vtype,skeyword);
  resizeArray(vtype,a_attrib_index,nb_values);
}

void MECPreObject::resizeArray(IMECPreObject::MyValueType_e vtype,int attrib_index,int nb_values) {
  void *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][vtype-VTY_UNKNOWN-1];
  //
  switch(vtype) {
  case VTY_INT:
  case VTY_BOOL:
    {
      LocIntArrayAttributes_t &a_attributes = (*((LocIntArrayAttributes_t *)a_attributes_p));
      LocIntArrayAttribute_t  &a_attribute  = a_attributes[attrib_index];
      //
      a_attribute.myValue.resize(nb_values);
    }
    break;
  case VTY_UINT:
    {
      LocUIntArrayAttributes_t &a_attributes = (*((LocUIntArrayAttributes_t *)a_attributes_p));
      LocUIntArrayAttribute_t  &a_attribute  = a_attributes[attrib_index];
      //
      a_attribute.myValue.resize(nb_values);
    }
    break;

  case VTY_FLOAT:
    {
      LocFloatArrayAttributes_t &a_attributes = (*((LocFloatArrayAttributes_t *)a_attributes_p));
      LocFloatArrayAttribute_t  &a_attribute  = a_attributes[attrib_index];
      //
      a_attribute.myValue.resize(nb_values);
    }
    break;
  case VTY_STRING:
    {
      LocStringArrayAttributes_t &a_attributes = (*((LocStringArrayAttributes_t *)a_attributes_p));
      LocStringArrayAttribute_t  &a_attribute  = a_attributes[attrib_index];
      //
      a_attribute.myValue.resize(nb_values);
    }
    break;
  case VTY_OBJECT:
    {
      LocObjectArrayAttributes_t &a_attributes = (*((LocObjectArrayAttributes_t *)a_attributes_p));
      LocObjectArrayAttribute_t  &a_attribute  = a_attributes[attrib_index];
      //
      a_attribute.myValue.resize(nb_values);
    }
    break;
  default:
    break;
  }
}

void MECPreObject::CompressMain(const char *main_skw,const char *size_skw,int nb_secondarys,char **secondary_skw_tab) {
  typedef vector<int>           LocIndexes_t;
  typedef vector<IMECPreObject::MyValueType_e> LocVTypes_t;
  LocIndexes_t a_secondary_indexes; a_secondary_indexes.reserve(10);
  LocVTypes_t  a_secondary_vtypes;  a_secondary_vtypes.reserve(10);
  // Getting secondary arrays
  for(int ii=0;ii<nb_secondarys;++ii) {
    const char    *a_secondary_skw   = secondary_skw_tab[ii];
    IMECPreObject::MyValueType_e  a_secondary_vtype = GetValueType(ATY_ARRAY,a_secondary_skw);
    int            a_secondary_index = GetIndex(ATY_ARRAY,a_secondary_vtype,a_secondary_skw);
    a_secondary_vtypes.push_back(a_secondary_vtype);
    a_secondary_indexes.push_back(a_secondary_index);
  }
  // Shifting
  int a_main_index    = GetIndex(ATY_ARRAY,VTY_OBJECT,main_skw);
  int a_nb_values_index = GetIndex(ATY_SINGLE,VTY_INT,size_skw);
  //
  if(a_main_index>=0 && a_nb_values_index>=0) {
    int a_old_nb_values   = GetIntValue(a_nb_values_index);
    int a_new_nb_values   = 0;
    //
    for(int i=0;i<a_old_nb_values;++i) {
      const char *a_otype   = GetObjectType(a_main_index,i);
      const char *a_name    = GetObjectName(a_main_index, i);
      int         a_id      = GetObjectId(a_main_index,i);
      int         a_index   = GetObjectIndex(a_main_index,i);
      bool        a_is_none = (a_otype==NULL ||a_id==0);
      //
      if(!a_is_none) {
	if(a_new_nb_values<i) {
	  // Shifting main
        if (a_name[0] != '\0')
            SetObjectValue(a_main_index, a_new_nb_values, a_otype,a_name, a_index);
        else
	  SetObjectValue(a_main_index,a_new_nb_values,a_otype,a_id,a_index);
	  // Shifting secondaries
	  for(int j=0;j<nb_secondarys;++j) switch(a_secondary_vtypes[j]) {
      case VTY_BOOL:
	    {
	      bool a_secondary_value=(bool)GetBoolValue(a_secondary_indexes[j],i);
	      SetBoolValue(a_secondary_indexes[j],a_new_nb_values,a_secondary_value);
	    }
        break;
	  case VTY_INT:
	    {
	      int a_secondary_value=GetIntValue(a_secondary_indexes[j],i);
	      SetIntValue(a_secondary_indexes[j],a_new_nb_values,a_secondary_value);
	    }
	    break;
	  case VTY_UINT:
	    {
	      unsigned int a_secondary_value=GetUIntValue(a_secondary_indexes[j],i);
	      SetUIntValue(a_secondary_indexes[j],a_new_nb_values,a_secondary_value);
	    }
	    break;
	  case VTY_FLOAT:
	    {
	      double a_secondary_value=GetFloatValue(a_secondary_indexes[j],i);
	      SetFloatValue(a_secondary_indexes[j],a_new_nb_values,a_secondary_value);
	    }
	    break;
	  case VTY_STRING:
	    {
	      const char *a_secondary_value=GetStringValue(a_secondary_indexes[j],i);
	      SetStringValue(a_secondary_indexes[j],a_new_nb_values,a_secondary_value);
	    }
	    break;
	  case VTY_OBJECT:
	    {
	      const char *a_secondary_otype = GetObjectType(a_secondary_indexes[j],i);
          const char *a_name_2        = GetObjectName(a_secondary_indexes[j], i);

	      int         a_secondary_id    = GetObjectId(a_secondary_indexes[j],i);
	      int         a_secondary_index = GetObjectIndex(a_secondary_indexes[j],i);
          if (a_name_2[0] != '\0')
              SetObjectValue(a_secondary_indexes[j], a_new_nb_values, a_secondary_otype, a_name_2, a_secondary_index);
          else
	      SetObjectValue(a_secondary_indexes[j],a_new_nb_values,a_secondary_otype,a_secondary_id,a_secondary_index);
	    }
	    break;
	  default:
	    break;
	  }
	}
	//
	++a_new_nb_values;
      }
    }
    // Resizing
    resizeArray(VTY_OBJECT,a_main_index,a_new_nb_values);
    for(int k=0;k<nb_secondarys;++k) resizeArray(a_secondary_vtypes[k],a_secondary_indexes[k],a_new_nb_values);
    SetIntValue(a_nb_values_index,a_new_nb_values);  
  }
}


/* --------- Output access --------- */

MECPreObject::IMECPreObject::MyValueType_e MECPreObject::GetValueType(IMECPreObject::MyAttributeType_e atype,const char *skeyword) const {
  for(int i=(VTY_UNKNOWN+1);i<VTY_LAST;++i) {
    IMECPreObject::MyValueType_e a_vtype      = (IMECPreObject::MyValueType_e)i;
    int           a_nb_attribs = GetNbAttributes(atype,a_vtype);
    //
    for(int j=0;j<a_nb_attribs;++j) {
      const char *a_skeyword=GetKeyword(atype,a_vtype,j);
      if(!strcmp(a_skeyword,skeyword)) return a_vtype;
    }
  }
  //
  return VTY_UNKNOWN;
}

int MECPreObject::GetNbAttributes(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype) const {
  int   a_nb_attributes = 0;
  void *a_attributes_p  = myAttributes[atype-ATY_UNKNOWN-1][vtype-VTY_UNKNOWN-1];
  //
  switch(atype) {
  case ATY_SINGLE:
    switch(vtype) {
    case VTY_BOOL:    a_nb_attributes=(int)(((LocIntSingleAttributes_t *)a_attributes_p)->size());    break;
    case VTY_INT:    a_nb_attributes=(int)(((LocIntSingleAttributes_t *)a_attributes_p)->size());    break;
    case VTY_UINT:    a_nb_attributes=(int)(((LocUIntSingleAttributes_t *)a_attributes_p)->size());    break;
    case VTY_FLOAT:  a_nb_attributes=(int)(((LocFloatSingleAttributes_t *)a_attributes_p)->size());  break;
    case VTY_STRING: a_nb_attributes=(int)(((LocStringSingleAttributes_t *)a_attributes_p)->size()); break;
    case VTY_OBJECT: a_nb_attributes=(int)(((LocObjectSingleAttributes_t *)a_attributes_p)->size()); break;
    default:
      // Wrong type of value
      break;
    }
    break;
  case ATY_ARRAY:
    switch(vtype) {
    case VTY_BOOL:    a_nb_attributes=(int)(((LocIntArrayAttributes_t *)a_attributes_p)->size());    break;
    case VTY_INT:     a_nb_attributes=(int)(((LocIntArrayAttributes_t *)a_attributes_p)->size());    break;
    case VTY_UINT:    a_nb_attributes=(int)(((LocUIntArrayAttributes_t *)a_attributes_p)->size());    break;
    case VTY_FLOAT:  a_nb_attributes=(int)(((LocFloatArrayAttributes_t *)a_attributes_p)->size());  break;
    case VTY_STRING: a_nb_attributes=(int)(((LocStringArrayAttributes_t *)a_attributes_p)->size()); break;
    case VTY_OBJECT: a_nb_attributes=(int)(((LocObjectArrayAttributes_t *)a_attributes_p)->size()); break;
    default:
      // Wrong type of value
      break;
    }
    break;
  default:
    // Wrong type of attribute
    break;
  }
  //
  return a_nb_attributes;
}

const char *MECPreObject::GetKeyword(IMECPreObject::MyAttributeType_e atype,IMECPreObject::MyValueType_e vtype,int ind) const {
  const char *a_keyword      = NULL;
  void       *a_attributes_p = myAttributes[atype-ATY_UNKNOWN-1][vtype-VTY_UNKNOWN-1];
  //
  switch(atype) {
  case ATY_SINGLE:
    switch(vtype) {
    case VTY_BOOL:    a_keyword=(*((LocIntSingleAttributes_t *)a_attributes_p))[ind].myKeyword.c_str();    break;
    case VTY_INT:    a_keyword=(*((LocIntSingleAttributes_t *)a_attributes_p))[ind].myKeyword.c_str();    break;
    case VTY_UINT:    a_keyword=(*((LocUIntSingleAttributes_t *)a_attributes_p))[ind].myKeyword.c_str();    break;
    case VTY_FLOAT:  a_keyword=(*((LocFloatSingleAttributes_t *)a_attributes_p))[ind].myKeyword.c_str();  break;
    case VTY_STRING: a_keyword=(*((LocStringSingleAttributes_t *)a_attributes_p))[ind].myKeyword.c_str(); break;
    case VTY_OBJECT: a_keyword=(*((LocObjectSingleAttributes_t *)a_attributes_p))[ind].myKeyword.c_str(); break;
    default:
      // Wrong type of value
      break;
    }
    break;
  case ATY_ARRAY:
    switch(vtype) {
    case VTY_BOOL:    a_keyword=(*((LocIntArrayAttributes_t *)a_attributes_p))[ind].myKeyword.c_str();    break;
    case VTY_INT:    a_keyword=(*((LocIntArrayAttributes_t *)a_attributes_p))[ind].myKeyword.c_str();    break;
    case VTY_UINT:    a_keyword=(*((LocUIntArrayAttributes_t *)a_attributes_p))[ind].myKeyword.c_str();    break;
    case VTY_FLOAT:  a_keyword=(*((LocFloatArrayAttributes_t *)a_attributes_p))[ind].myKeyword.c_str();  break;
    case VTY_STRING: a_keyword=(*((LocStringArrayAttributes_t *)a_attributes_p))[ind].myKeyword.c_str(); break;
    case VTY_OBJECT: a_keyword=(*((LocObjectArrayAttributes_t *)a_attributes_p))[ind].myKeyword.c_str(); break;
    default:
      // Wrong type of value
      break;
    }
    break;
  default:
    // Wrong type of attribute
    break;
  }
  //
  return a_keyword; 
}

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      This function is used to get the internal index in the
//      container in the model.
//Parameters:
//      IMECPreObject::MyAttributeType_e atype: attribute type, four possible values
//      MECPreObject::ATY_UNKNOWN,
//      MECPreObject::ATY_SINGLE,
//      MECPreObject::ATY_ARRAY,
//      MECPreObject::ATY_LAST,
//      IMECPreObject::MyValueType_e vtype: value type, six possible values
//      MECPreObject::VTY_UNKNOWN,
//      MECPreObject::VTY_INT,
//      MECPreObject::VTY_FLOAT,
//      MECPreObject::VTY_STRING,
//      MECPreObject::VTY_OBJECT,
//      MECPreObject::VTY_LAST
//      const char *keyword: it's skeyword, see in the xxx.cfg file
//Return value:
//      int: internal index.
//Design:
//Modification History:

//--///////////////////////////////////////////////////////////////////////////
int MECPreObject::GetIndex(IMECPreObject::MyAttributeType_e atype,
                               IMECPreObject::MyValueType_e vtype,
                               const char *keyword) const 
{//Begin -- MECPreObject::GetIndex
  string                 a_keyword     = keyword;
  const LocKeywordMap_t &a_keyword_map = (*((LocKeywordMap_t *)(myKeywordMaps[atype-ATY_UNKNOWN-1][vtype-VTY_UNKNOWN-1])));
  //
  LocKeywordMap_t::const_iterator a_it=a_keyword_map.find(a_keyword);
  if(a_it!=a_keyword_map.end()) return (*a_it).second;
  //
  return -1;
}//End -- MECPreObject::GetIndex
int MECPreObject::GetIndex(IMECPreObject::MyAttributeType_e atype,
                               IMECPreObject::MyValueType_e vtype,
                               const string &keyword) const 
{//Begin -- MECPreObject::GetIndex
  const LocKeywordMap_t &a_keyword_map = (*((LocKeywordMap_t *)(myKeywordMaps[atype-ATY_UNKNOWN-1][vtype-VTY_UNKNOWN-1])));
  //
  LocKeywordMap_t::const_iterator a_it=a_keyword_map.find(keyword);
  if(a_it!=a_keyword_map.end()) return (*a_it).second;
  //
  return -1;
}
bool MECPreObject::GetBoolValue(const char *skeyword) const 
{//Begin -- MECPreObject::GetIntValue
  int a_index=GetIndex(ATY_SINGLE,VTY_BOOL,skeyword);
  if(a_index<0) return false;
  return GetBoolValue(a_index);
}//End -- MECPreObject::GetIntValue

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      This function is used to get the integer value by using internal index 
//      in the container in the model.
//Parameters:
//      const char *keyword: it's string keyword, see in the .cfg file
//Return value:
//      int: its value.
//Design:
//Modification History:

//--///////////////////////////////////////////////////////////////////////////
int MECPreObject::GetIntValue(const char *skeyword) const 
{//Begin -- MECPreObject::GetIntValue
  int a_index=GetIndex(ATY_SINGLE,VTY_INT,skeyword);
  if(a_index<0) return 0;
  return GetIntValue(a_index);
}//End -- MECPreObject::GetIntValue

unsigned int MECPreObject::GetUIntValue(const char *skeyword) const 
{//Begin -- MECPreObject::GetUIntValue
  int a_index=GetIndex(ATY_SINGLE,VTY_UINT,skeyword);
  if(a_index<0) return 0;
  return GetUIntValue(a_index);
}//End -- MECPreObject::GetUIntValue

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      This function is used to get the float value by using internal index 
//      in the container in the model.
//Parameters:
//      const char *keyword: it's string keyword, see in the .cfg file
//Return value:
//      int: its value.
//Design:
//Modification History:

//--///////////////////////////////////////////////////////////////////////////
double MECPreObject::GetFloatValue(const char *skeyword) const 
{//Begin -- MECPreObject::GetFloatValue

  int a_index=GetIndex(ATY_SINGLE,VTY_FLOAT,skeyword);
  if(a_index<0) return 0.;
  return GetFloatValue(a_index);
  
}//End -- MECPreObject::GetFloatValue

//++///////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      This function is used to get the string value by using internal index 
//      in the container in the model.
//Parameters:
//      const char *keyword: it's string keyword, see in the .cfg file
//Return value:
//      int: its value.
//Design:
//Modification History:

//--///////////////////////////////////////////////////////////////////////////
const char *MECPreObject::GetStringValue(const char *skeyword) const 
{//Begin -- MECPreObject::GetStringValue

  int a_index=GetIndex(ATY_SINGLE,VTY_STRING,skeyword);
  if(a_index<0) return NULL;
  return GetStringValue(a_index);
  
}//End -- MECPreObject::GetStringValue 
bool MECPreObject::GetBoolValue(int attrib_ind) const {
  if(attrib_ind<0)
      return false;
  const LocIntSingleAttributes_t &a_attributes=(*((LocIntSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue > 0 ? true : false;
}
int MECPreObject::GetIntValue(int attrib_ind) const {
  if(attrib_ind<0)
      return 0;
  const LocIntSingleAttributes_t &a_attributes=(*((LocIntSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue;
}
unsigned int MECPreObject::GetUIntValue(int attrib_ind) const {
  if(attrib_ind<0)
      return 0;
  const LocUIntSingleAttributes_t &a_attributes=(*((LocUIntSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue;
}
double MECPreObject::GetFloatValue(int attrib_ind) const {
  if(attrib_ind<0)
      return 0.;
  const LocFloatSingleAttributes_t &a_attributes=(*((LocFloatSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue;
}

const char *MECPreObject::GetStringValue(int attrib_ind) const {
  if(attrib_ind<0)
      return "";
  const LocStringSingleAttributes_t &a_attributes=(*((LocStringSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue.c_str();
}

const char *MECPreObject::GetObjectType(int attrib_ind) const {
  if(attrib_ind<0)
      return "";
  const LocObjectSingleAttributes_t &a_attributes=(*((LocObjectSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue.myOType;
}

//++/////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      This function is used to get the object ID.
//Parameters:
//      int attrib_ind: internal index, 
//      this index can be obtained by using MECPreObject::GetIndex function.
//Return value:
//      int: object ID.
//Design:
//Modification History:

//--//////////////////////////////////////////////////////////////////////////////
MYOBJ_INT MECPreObject::GetObjectId(int attrib_ind) const
{//Begin -- MECPreObject::GetObjectId
  if(attrib_ind<0)
      return 0;
  const LocObjectSingleAttributes_t &a_attributes=(*((LocObjectSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue.myId;
  
}//Begin -- MECPreObject::GetObjectId

int MECPreObject::GetObjectIndex(int attrib_ind) const {
  if(attrib_ind<0)
      return 0;
  const LocObjectSingleAttributes_t &a_attributes=(*((LocObjectSingleAttributes_t *)(myAttributes[ATY_SINGLE-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue.myIndex;
}

const char* MECPreObject::GetObjectName(int attrib_ind) const {
    if (attrib_ind<0)
        return "";
    const LocObjectSingleAttributes_t &a_attributes = (*((LocObjectSingleAttributes_t *)(myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1])));
    return a_attributes[attrib_ind].myValue.myName.c_str();
}

int MECPreObject::GetNbValues(IMECPreObject::MyValueType_e vtype,int attrib_ind) const {
  if(attrib_ind<0)
      return 0;
  void *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][vtype-VTY_UNKNOWN-1];
  int   a_nb_values    = 0;
  //
  switch(vtype) {
  case VTY_BOOL:    a_nb_values=(int)((*((LocIntArrayAttributes_t *)a_attributes_p))[attrib_ind].myValue.size());    break;
  case VTY_INT:    a_nb_values=(int)((*((LocIntArrayAttributes_t *)a_attributes_p))[attrib_ind].myValue.size());    break;
  case VTY_UINT:    a_nb_values=(int)((*((LocUIntArrayAttributes_t *)a_attributes_p))[attrib_ind].myValue.size());    break;
  case VTY_FLOAT:  a_nb_values=(int)((*((LocFloatArrayAttributes_t *)a_attributes_p))[attrib_ind].myValue.size());  break;
  case VTY_STRING: a_nb_values=(int)((*((LocStringArrayAttributes_t *)a_attributes_p))[attrib_ind].myValue.size()); break;
  case VTY_OBJECT: a_nb_values=(int)((*((LocObjectArrayAttributes_t *)a_attributes_p))[attrib_ind].myValue.size()); break;
  default:
    // Wrong type of value
    break;
  }
  //
  return a_nb_values;
}

bool MECPreObject::GetBoolValue(int attrib_ind,int i) const {
  if(attrib_ind<0)
      return 0;
  if (i<0) return GetBoolValue(attrib_ind); 
  const LocIntArrayAttributes_t &a_attributes=(*((LocIntArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_BOOL-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue[i] > 0 ? true : false;
}

int MECPreObject::GetIntValue(int attrib_ind,int i) const {
  if(attrib_ind<0)
      return 0;
  if (i<0) return GetIntValue(attrib_ind); 
  const LocIntArrayAttributes_t &a_attributes=(*((LocIntArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_INT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue[i];
}

unsigned int MECPreObject::GetUIntValue(int attrib_ind,int i) const {
  if(attrib_ind<0)
      return 0;
  if (i<0) return GetUIntValue(attrib_ind); 
  const LocUIntArrayAttributes_t &a_attributes=(*((LocUIntArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_UINT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue[i];
}

double MECPreObject::GetFloatValue(int attrib_ind,int i) const {
  if(attrib_ind<0)
      return 0.;
  if (i<0) return GetFloatValue(attrib_ind); 
  const LocFloatArrayAttributes_t &a_attributes=(*((LocFloatArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue[i];
}

const char *MECPreObject::GetStringValue(int attrib_ind,int i) const {
    if(attrib_ind<0)
      return "";
  if (i<0) return GetStringValue(attrib_ind); 
  const LocStringArrayAttributes_t &a_attributes=(*((LocStringArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_STRING-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue[i].c_str();
}

const char *MECPreObject::GetObjectType(int attrib_ind,int i) const {
      if(attrib_ind<0)
      return "";
  if (i<0) return GetObjectType(attrib_ind); 
  const LocObjectArrayAttributes_t &a_attributes=(*((LocObjectArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue[i].myOType;
}

MYOBJ_INT MECPreObject::GetObjectId(int attrib_ind,int i) const {
  if(attrib_ind<0)
      return 0;
  if (i<0) return GetObjectId(attrib_ind); 
  const LocObjectArrayAttributes_t &a_attributes=(*((LocObjectArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue[i].myId;  
}

int MECPreObject::GetObjectIndex(int attrib_ind,int i) const {
  if(attrib_ind<0)
      return 0;
  if (i<0) return GetObjectIndex(attrib_ind); 
  const LocObjectArrayAttributes_t &a_attributes=(*((LocObjectArrayAttributes_t *)(myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_OBJECT-VTY_UNKNOWN-1])));
  return a_attributes[attrib_ind].myValue[i].myIndex;  
}

const char* MECPreObject::GetObjectName(int attrib_ind, int i) const {
    if (attrib_ind<0)
        return "";
    if (i<0) return GetObjectName(attrib_ind); 
    const LocObjectArrayAttributes_t &a_attributes = (*((LocObjectArrayAttributes_t *)(myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1])));
    return a_attributes[attrib_ind].myValue[i].myName.c_str();
}

/* --------- Initialization --------- */

void MECPreObject::Init(const PseudoDescriptor_t *descr_p) {
  const IDescriptor *a_descr_p=(const MvDescriptor_t *)descr_p;
  //
  int              a_domains = MV_get_all_domains();
  int              a_nb_ikws = 0;
  MvIKeywordList_t a_ikw_list;

  // Single bool
  a_descr_p->getIKeywords(a_domains,ATYPE_VALUE,VTYPE_BOOL,&a_ikw_list);
  a_nb_ikws+=(int)(a_ikw_list.size());
  Reserve(ATY_SINGLE,VTY_BOOL,a_nb_ikws);

  // Single integers
  a_descr_p->getIKeywords(a_domains,ATYPE_VALUE,VTYPE_INT,&a_ikw_list);
  a_nb_ikws=(int)(a_ikw_list.size());
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_SIZE,&a_ikw_list);
  a_nb_ikws+=(int)(a_ikw_list.size());
  Reserve(ATY_SINGLE,VTY_INT,a_nb_ikws);

  // Single unsigned integers
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_VALUE,VTYPE_UINT,&a_ikw_list);
  a_nb_ikws=(int)(a_ikw_list.size());
  Reserve(ATY_SINGLE,VTY_UINT,a_nb_ikws);

  // Single floats
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_VALUE,VTYPE_FLOAT,&a_ikw_list);
  a_nb_ikws=(int)(a_ikw_list.size());
  Reserve(ATY_SINGLE, VTY_FLOAT, a_nb_ikws);
  // Single strings
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_VALUE,VTYPE_STRING,&a_ikw_list);
  a_nb_ikws=(int)(a_ikw_list.size());
  Reserve(ATY_SINGLE, VTY_STRING, a_nb_ikws);
  // Single objects
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_VALUE,VTYPE_OBJECT,&a_ikw_list);
  a_nb_ikws=(int)(a_ikw_list.size());
  Reserve(ATY_SINGLE, VTY_OBJECT, a_nb_ikws);

  // Array bool
  a_descr_p->getIKeywords(a_domains,ATYPE_STATIC_ARRAY,VTYPE_BOOL,&a_ikw_list);
  a_nb_ikws=(int)(a_ikw_list.size());
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_DYNAMIC_ARRAY,VTYPE_BOOL,&a_ikw_list);
  a_nb_ikws+=(int)(a_ikw_list.size());
  Reserve(ATY_ARRAY,VTY_BOOL,a_nb_ikws);

  // Array integers
  a_descr_p->getIKeywords(a_domains,ATYPE_STATIC_ARRAY,VTYPE_INT,&a_ikw_list);
  a_nb_ikws=(int)(a_ikw_list.size());
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_DYNAMIC_ARRAY,VTYPE_INT,&a_ikw_list);
  a_nb_ikws+=(int)(a_ikw_list.size());
  Reserve(ATY_ARRAY,VTY_INT,a_nb_ikws);

  // Array unsigned integers
  a_descr_p->getIKeywords(a_domains,ATYPE_STATIC_ARRAY,VTYPE_UINT,&a_ikw_list);
  a_nb_ikws=(int)(a_ikw_list.size());
  a_ikw_list.clear();

  a_descr_p->getIKeywords(a_domains,ATYPE_DYNAMIC_ARRAY,VTYPE_UINT,&a_ikw_list);
  a_nb_ikws+=(int)(a_ikw_list.size());
  Reserve(ATY_ARRAY,VTY_UINT,a_nb_ikws);

  // Array floats
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_STATIC_ARRAY,VTYPE_FLOAT,&a_ikw_list);
  a_nb_ikws=(int)(a_ikw_list.size());
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_DYNAMIC_ARRAY,VTYPE_FLOAT,&a_ikw_list);
  a_nb_ikws+=(int)(a_ikw_list.size());
  Reserve(ATY_ARRAY,VTY_FLOAT,a_nb_ikws);
  // Array strings
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_STATIC_ARRAY,VTYPE_STRING,&a_ikw_list);
  a_nb_ikws=(int)(a_ikw_list.size());
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_DYNAMIC_ARRAY,VTYPE_STRING,&a_ikw_list);
  a_nb_ikws+=(int)(a_ikw_list.size());
  Reserve(ATY_ARRAY,VTY_STRING,a_nb_ikws);
  // Array objects
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_STATIC_ARRAY,VTYPE_OBJECT,&a_ikw_list);
  a_nb_ikws=(int)(a_ikw_list.size());
  a_ikw_list.clear();
  a_descr_p->getIKeywords(a_domains,ATYPE_DYNAMIC_ARRAY,VTYPE_OBJECT,&a_ikw_list);
  a_nb_ikws+=(int)(a_ikw_list.size());
  Reserve(ATY_ARRAY,VTY_OBJECT,a_nb_ikws);
  SetEntitySubtypeTypeIdentifier(a_descr_p->getUserId());
  setConfigType(a_descr_p->getConfigType());
  setIdPool(a_descr_p->getIdPool());
}


bool MECPreObject::EvaluateExpression(const PseudoExpression_t *expr_p,
                                      const PseudoDescriptor_t *descr_p,
                                      int                       ind) const
{
    return EvaluateExpression(expr_p,descr_p,NULL,NULL,ind) ;
}
bool MECPreObject::EvaluateExpression(const PseudoExpression_t *expr_p,
                                      const PseudoDescriptor_t *descr_p,
                                      set<int>                 *ikeywordchecklst,
                                      set<int>                 *ikeywordlst,
                                      int                       ind) const
{
    const MvExpression_t *a_expr_p  = (const MvExpression_t *)expr_p;
    const IDescriptor    *a_descr_p = (const MvDescriptor_t *)descr_p;
    //
    const expression_t *a_mcds_expr_p = a_expr_p->getExpressionPtr();
    expression_type_e   a_expr_type   = EXPRT_UNKNOWN;
    MCDS_get_expression_attributes(a_mcds_expr_p,EXPR_TYPE,&a_expr_type,END_ARGS);
    //
    bool a_result=false;
    switch(a_expr_type) {
    case EXPRT_ATTRIBUTE:
        a_result=loc_evaluate_attribute_expression(*this,a_mcds_expr_p,a_descr_p,ikeywordchecklst, ikeywordlst, ind);
        break;
    case EXPRT_LOGICAL:
        a_result=loc_evaluate_logical_expression(*this,a_mcds_expr_p,a_descr_p,ikeywordchecklst, ikeywordlst, ind);
        break;    
    default:
        break;
    }
    //
    return a_result;
}
bool MECPreObject::EvaluateMCDSExpression(const PseudoExpression_t* expr_p,
                                          const PseudoDescriptor_t* descr_p,
                                          int                       ind) const
{
    const expression_t* a_mcds_expr_p = (const expression_t*)expr_p;
    const IDescriptor* a_descr_p = (const MvDescriptor_t*)descr_p;
    //
    expression_type_e   a_expr_type = EXPRT_UNKNOWN;
    MCDS_get_expression_attributes(a_mcds_expr_p, EXPR_TYPE, &a_expr_type, END_ARGS);
    //
    bool a_result = false;
    switch (a_expr_type) {
    case EXPRT_ATTRIBUTE:
        a_result = loc_evaluate_attribute_expression(*this, a_mcds_expr_p, a_descr_p, NULL, NULL, ind);
        break;
    case EXPRT_LOGICAL:
        a_result = loc_evaluate_logical_expression(*this, a_mcds_expr_p, a_descr_p, NULL, NULL, ind);
        break;
    default:
        break;
    }
    //
    return a_result;
}

double MECPreObject::GetExpressionValue(const PseudoDescriptor_t  *descr_p, const int &ikeyword, const int &loc_ind) const
{
    double  val = 0.0;
    static string val_str = "";
    GetExpressionValue( descr_p, ikeyword, loc_ind, val, val_str);
    return val;
}

bool MECPreObject::GetExpressionValue(const PseudoDescriptor_t  *descr_p, const int &ikeyword, const int &loc_ind, double& val, string &val_str) const
{
    if (descr_p == NULL)
        return false;

    const IDescriptor* a_descr_p = (const IDescriptor*)descr_p;
    string          skeyword = a_descr_p->getSKeyword(ikeyword);
    value_type_e    a_vtype  = a_descr_p->getValueType(ikeyword);
    attribute_type_e a_atype = a_descr_p->getAttributeType(ikeyword);
    MyAttributeType_e aty_type = IMECPreObject::ATY_ARRAY;
    if (a_atype == ATYPE_VALUE || a_atype == ATYPE_SIZE)
        aty_type = IMECPreObject::ATY_SINGLE;

    switch (a_vtype)
    {
    case VTYPE_INT:
    {       
        int a_attrib_index = GetIndex(aty_type, IMECPreObject::VTY_INT, skeyword.c_str());
        if (a_attrib_index >= 0)
        {
            if (aty_type == IMECPreObject::ATY_SINGLE)
                val = GetIntValue(a_attrib_index);
            else
                val = GetIntValue(a_attrib_index, loc_ind);
        }
        else
        {
            val = a_descr_p->getIntDefaultValue(ikeyword, DOM_COMMON);
        }
    }
    break;
    case VTYPE_UINT:
    {
        int a_attrib_index = GetIndex(aty_type, IMECPreObject::VTY_UINT, skeyword.c_str());

        if (a_attrib_index >= 0)
        {
            if (aty_type == IMECPreObject::ATY_SINGLE)
            {
                val = GetUIntValue(a_attrib_index);
            }
            else
                val = GetUIntValue(a_attrib_index, loc_ind);
        }
        else
        {
            val = a_descr_p->getUIntDefaultValue(ikeyword, DOM_COMMON);
        }
    }
    break;
    case VTYPE_FLOAT:
    {
        int a_attrib_index = GetIndex(aty_type, IMECPreObject::VTY_FLOAT, skeyword.c_str());
        if (a_attrib_index >= 0)
        {
            if (aty_type == IMECPreObject::ATY_SINGLE)
            {
                val = GetFloatValue(a_attrib_index);
            }
            else
                val = GetFloatValue(a_attrib_index, loc_ind);
        }
        else
        {
            val = a_descr_p->getFloatDefaultValue(ikeyword, DOM_COMMON);
        }
    }
    break;
    case VTYPE_OBJECT:
    {
        int a_attrib_index =   GetIndex(aty_type, IMECPreObject::VTY_OBJECT, skeyword.c_str()) ;
        if (a_attrib_index >= 0)
        {
            if (aty_type == IMECPreObject::ATY_SINGLE)
            {
                val = GetObjectId(a_attrib_index);
                val_str = GetObjectName(a_attrib_index);
            }
            else
            {
                val = GetObjectId(a_attrib_index, loc_ind);
                val_str = GetObjectName(a_attrib_index, loc_ind);
            }
        }
        else
            val = 0;
    }
    break;
    case VTYPE_STRING:
    {
        int a_attrib_index = GetIndex(aty_type, IMECPreObject::VTY_STRING, skeyword.c_str());
        if (a_attrib_index >= 0)
        {
            if (aty_type == IMECPreObject::ATY_SINGLE)
            {
                val_str = GetStringValue(a_attrib_index);
            }
            else
            {
                val_str = GetStringValue(a_attrib_index, loc_ind);
            }
        }
        else
            val_str = "";
    }
    break;
    default:
        break;
    }
    return true;
}


/* --------- Report --------- */

char *MECPreObject::GetReport(const PseudoDescriptor_t *descr_p) const {
  const IDescriptor *a_descr_p=(const IDescriptor *)descr_p;
  string a_report="";
  a_report+=str_printf("%s/%d/%s {\n",GetKernelFullType(),GetId(),GetTitle());
  //
  int i,j,k,l;
  int a_nb_atypes  = ATY_LAST-ATY_UNKNOWN-1;
  int a_nb_vtypes  = VTY_LAST-VTY_UNKNOWN-1;
  int a_length_max = 0, a_solvname_length_max = 0;
  for(i=0;i<a_nb_atypes;++i) {
    IMECPreObject::MyAttributeType_e a_atype=(IMECPreObject::MyAttributeType_e)(ATY_UNKNOWN+i+1);
    //
    for(j=0;j<a_nb_vtypes;++j) {
      IMECPreObject::MyValueType_e a_vtype         = (IMECPreObject::MyValueType_e)(VTY_UNKNOWN+j+1);
      int           a_nb_attributes = GetNbAttributes(a_atype,a_vtype);
      //
      for(k=0;k<a_nb_attributes;++k) {
        int a_length=(int)(strlen(GetKeyword(a_atype,a_vtype,k)));
        //
        if(a_length>a_length_max) a_length_max=a_length;
        if(a_descr_p) {
            int ikwd = a_descr_p->getIKeyword(GetKeyword(a_atype,a_vtype,k));
            int a_solvname_length=0;
            if(END_ARGS!=ikwd) a_solvname_length=(int)(a_descr_p->getSolverName(ikwd)).size();
            else a_solvname_length=(int)(a_descr_p->getSKeywordFromSolverName(GetKeyword(a_atype,a_vtype,k))).size();
            if(a_solvname_length>a_solvname_length_max) a_solvname_length_max=a_solvname_length;
        }
      }
    }
  }
  int a_iftype_length=(int)(strlen("KERNEL_FTYPE"));
  if(a_length_max<a_iftype_length) a_length_max=a_iftype_length;
  string a_keyword_format=str_printf("  %%-%ds ",a_length_max);
  string a_solvname_format, a_skeyword_format;
  if(0<a_solvname_length_max) {
      a_solvname_format=str_printf("(%%-%ds) = ",a_solvname_length_max);
      a_skeyword_format=str_printf("[%%-%ds] = ",a_solvname_length_max);
  }
  // Input type
  if (GetInputFullType() && GetInputFullType() != "")
  {
      a_report += str_printf(a_keyword_format.c_str(), "INPUT_FTYPE");
      for (i = 0; i < a_solvname_length_max; ++i) a_report += " ";
      if (0 < a_solvname_length_max) a_report += "   = ";
      else                        a_report += "= ";
      a_report += str_printf("%s\n", GetInputFullType());
  }
  // Kernel type
  a_report+=str_printf(a_keyword_format.c_str(),"KERNEL_FTYPE");
  for(i=0;i<a_solvname_length_max;++i) a_report+=" ";
  if(0<a_solvname_length_max) a_report+="   = ";
  else                        a_report+="= ";
  a_report+=str_printf("%s\n",GetKernelFullType());

  //include id and submodel id
  a_report += str_printf(a_keyword_format.c_str(), "Include ID: ");
  a_report += str_printf("%d\n", GetFileIndex());
  a_report += str_printf(a_keyword_format.c_str(), "Submodel ID: ");
  a_report += str_printf("%d\n", GetSubdeckIndex());


  // Integer single values
  for(i=0;i<a_nb_atypes;++i) {
    IMECPreObject::MyAttributeType_e a_atype=(IMECPreObject::MyAttributeType_e)(ATY_UNKNOWN+i+1);
    //
    for(j=0;j<a_nb_vtypes;++j) {
      IMECPreObject::MyValueType_e a_vtype         = (IMECPreObject::MyValueType_e)(VTY_UNKNOWN+j+1);
      int           a_nb_attributes = GetNbAttributes(a_atype,a_vtype);
      //
      for (k = 0; k < a_nb_attributes; ++k) {
          a_report+=str_printf(a_keyword_format.c_str(),GetKeyword(a_atype,a_vtype,k));
          if(0<a_solvname_length_max){
              int ikwd = a_descr_p->getIKeyword(GetKeyword(a_atype,a_vtype,k));
              if(END_ARGS!=ikwd) a_report+=str_printf(a_solvname_format.c_str(),a_descr_p->getSolverName(ikwd).c_str());
              else a_report+=str_printf(a_skeyword_format.c_str(),a_descr_p->getSKeywordFromSolverName(GetKeyword(a_atype,a_vtype,k)).c_str());
          }
          else{
              a_report+="= ";
          }
          //
          switch (a_atype) {
          case ATY_SINGLE:
          {
              switch (a_vtype) {
              case VTY_BOOL:
                  a_report += str_printf("%d", (int)GetBoolValue(k));
                  break;
              case VTY_INT:
                  a_report += str_printf("%d", GetIntValue(k));
                  break;
              case VTY_UINT:
                  a_report += str_printf("%u", GetUIntValue(k));
                  break;
              case VTY_FLOAT:
                  a_report += str_printf("%lg", GetFloatValue(k));
                  break;
              case VTY_STRING:
                  a_report += str_printf("\"%s\"", GetStringValue(k));
                  break;
              case VTY_OBJECT:
                  a_report += str_printf("/%s/%d/%s/%d", GetObjectType(k), GetObjectId(k), GetObjectName(k), GetObjectIndex(k));
                  break;
              default:
                  // Wrong type of value
                  break;
              }
          }
          break;
          case ATY_ARRAY:
          {
              int a_nb_values = GetNbValues(a_vtype, k);
              //
              a_report += "(";
              switch (a_vtype) {
              case VTY_BOOL:
                  for (l = 0; l < a_nb_values; ++l) {
                      int a_value = GetBoolValue(k, l);
                      if (l > 0) a_report += ",";
                      a_report += str_printf("%d", a_value);
                  }
                  break;
              case VTY_INT:
                  for (l = 0; l < a_nb_values; ++l) {
                      int a_value = GetIntValue(k, l);
                      if (l > 0) a_report += ",";
                      a_report += str_printf("%d", a_value);
                  }
                  break;
              case VTY_UINT:
                  for (l = 0; l < a_nb_values; ++l) {
                      unsigned int a_value = GetUIntValue(k, l);
                      if (l > 0) a_report += ",";
                      a_report += str_printf("%u", a_value);
                  }
                  break;
              case VTY_FLOAT:
                  for (l = 0; l < a_nb_values; ++l) {
                      double a_value = GetFloatValue(k, l);
                      if (l > 0) a_report += ",";
                      a_report += str_printf("%lg", a_value);
                  }
                  break;
              case VTY_STRING:
                  for (l = 0; l < a_nb_values; ++l) {
                      const char* a_value = GetStringValue(k, l);
                      if (l > 0) a_report += ",";
                      a_report += str_printf("\"%s\"", a_value);
                  }
                  break;
              case VTY_OBJECT:
                  for (l = 0; l < a_nb_values; ++l) {
                      const char* a_otype = GetObjectType(k, l);
                      const char* a_name = GetObjectName(k, l);
                      int         a_id = GetObjectId(k, l);
                      int         a_index = GetObjectIndex(k, l);
                      if (l > 0) a_report += ",";
                      a_report += str_printf("/%s/%d/%s/%d", a_otype, a_id, a_name, a_index);
                  }
                  break;
              default:
                  // Wrong type of value
                  break;
              }
              a_report += ")";
          }
          break;
          default:
              // Wrong type of attribute
              break;
          }
          //
          a_report += ";\n";
      }
    }
  }
  //
  a_report+="}";

  vector<IMECPreObject *>::const_iterator It;
  for(It=sub_preobj.begin();It!=sub_preobj.end();++It)
  {
      const IDescriptor *a_subdescr_p=NULL;
      // if parent is written with solver names, also do this for the subobjects
      if(a_descr_p) a_subdescr_p=HCDI_GetDescriptorHandle((*It)->GetKernelFullType());
      a_report += (*It)->GetReport(a_subdescr_p);
  }

  return strdup(a_report.c_str());
}


/* --------- Static functions --------- */
static bool loc_evaluate_attribute_expression(const IMECPreObject     &pre_object,
                                              const expression_t      *expr_p,
                                              const IDescriptor       *descr_p,
                                              set<int>                *ikeywordchecklst,
                                              set<int>                *ikeywordlst,
                                              int                      ind)
{
    bool a_result = false;
    //
    int          a_ikeyword = END_ARGS;
    comparator_e a_comparator = CMPT_UNKNOWN;
    int          a_rikeyword = 0;
    MCDS_get_expression_attributes(expr_p,
        EXPR_IKEYWORD, &a_ikeyword,
        EXPR_COMPARATOR, &a_comparator,
        EXPR_RHS_IKEYWORD, &a_rikeyword,
        END_ARGS);
    //
    string       a_skeyword = descr_p->getSKeyword(a_ikeyword);
    string       a_rskeyword = descr_p->getSKeyword(a_rikeyword);
    value_type_e a_vtype = descr_p->getValueType(a_ikeyword);
    int ind_l = ind;
    int ind_r = ind;

    if (ind >= 0)
    {
        attribute_type_e a_atype = descr_p->getAttributeType(a_ikeyword);
        attribute_type_e a_atype_r = descr_p->getAttributeType(a_rikeyword);
        if (a_atype == ATYPE_VALUE || a_atype == ATYPE_SIZE)
            ind_l = -1;
        if (a_atype_r == ATYPE_VALUE || a_atype_r == ATYPE_SIZE)
            ind_r = -1;
    }
    if (ikeywordchecklst)
    {
        const bool is_in = ikeywordchecklst->find(a_ikeyword) != ikeywordchecklst->end();
        bool r_is_in = true;
        if (a_rikeyword > 0)
            r_is_in = ikeywordchecklst->find(a_rikeyword) != ikeywordchecklst->end();
        if (ikeywordlst)
        {
            ikeywordlst->insert(a_ikeyword);
            if (a_rikeyword > 0)
                ikeywordlst->insert(a_rikeyword);
        }
        if (!is_in || a_rikeyword > 0)
            return true;
    }
    switch (a_vtype) {
    case VTYPE_BOOL:
    {
        int a_attrib_index = -1;

        a_attrib_index = ind_l < 0 ? pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_BOOL, a_skeyword) :
            pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_skeyword);

        int a_lvalue = 0;
        if (a_attrib_index >= 0) {
            a_lvalue = ind_l < 0 ?
                pre_object.GetBoolValue(a_attrib_index) :
                pre_object.GetBoolValue(a_attrib_index, ind_l);
        }
        else {
            a_lvalue = descr_p->getBoolDefaultValue(a_ikeyword, DOM_COMMON);

        }
        int a_rvalue = 0;
        if (a_rikeyword > 0)
        {
            a_attrib_index = ind_r < 0 ? pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_BOOL, a_rskeyword) :
                pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_rskeyword);
            if (a_attrib_index >= 0)
            {
                a_rvalue = ind_r < 0 ?
                    pre_object.GetBoolValue(a_attrib_index) :
                    pre_object.GetBoolValue(a_attrib_index, ind_r);
            }
            else
            {
                a_rvalue = descr_p->getBoolDefaultValue(a_rikeyword, DOM_COMMON);
            }
        }
        else
        {
            MCDS_get_expression_attributes(expr_p, EXPR_RVALUE, &a_rvalue, END_ARGS);
        }
        a_result = loc_eval_int(a_lvalue, a_comparator, a_rvalue);
    }
    break;
    case VTYPE_INT:
    {
        int a_attrib_index = -1;

        a_attrib_index = ind_l < 0 ? pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_skeyword) :
            pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_skeyword);


        int a_lvalue = 0;
        if (a_attrib_index >= 0) {
            a_lvalue = ind_l < 0 ?
                pre_object.GetIntValue(a_attrib_index) :
                pre_object.GetIntValue(a_attrib_index, ind_l);
        }
        else {
            a_lvalue = descr_p->getIntDefaultValue(a_ikeyword, DOM_COMMON);
        }
        int a_rvalue = 0;
        if (a_rikeyword > 0)
        {
            a_attrib_index = ind_r < 0 ? pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_rskeyword) :
                pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_rskeyword);
            if (a_attrib_index >= 0)
            {
                a_rvalue = ind_r < 0 ?
                    pre_object.GetIntValue(a_attrib_index) :
                    pre_object.GetIntValue(a_attrib_index, ind_r);
            }
            else
            {
                a_rvalue = descr_p->getIntDefaultValue(a_rikeyword, DOM_COMMON);
            }
        }
        else
        {
            MCDS_get_expression_attributes(expr_p, EXPR_RVALUE, &a_rvalue, END_ARGS);
        }
        a_result = loc_eval_int(a_lvalue, a_comparator, a_rvalue);
    }
    break;
    case VTYPE_UINT:
    {
        int a_attrib_index = ind_l < 0 ?
            pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_UINT, a_skeyword) :
            pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_skeyword);
        unsigned int a_lvalue = 0;
        if (a_attrib_index >= 0) {
            a_lvalue = ind_l < 0 ?
                pre_object.GetUIntValue(a_attrib_index) :
                pre_object.GetUIntValue(a_attrib_index, ind_l);
        }
        else {
            a_lvalue = descr_p->getUIntDefaultValue(a_ikeyword, DOM_COMMON);
        }
        unsigned int a_rvalue = 0;
        if (a_rikeyword > 0)
        {
            a_attrib_index = ind_r < 0 ?
                pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_UINT, a_rskeyword) :
                pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_rskeyword);
            if (a_attrib_index >= 0)
            {
                a_rvalue = ind_r < 0 ?
                    pre_object.GetUIntValue(a_attrib_index) :
                    pre_object.GetUIntValue(a_attrib_index, ind_r);
            }
            else
            {
                a_rvalue = descr_p->getUIntDefaultValue(a_rikeyword, DOM_COMMON);
            }
        }
        else
        {
            MCDS_get_expression_attributes(expr_p, EXPR_RVALUE, &a_rvalue, END_ARGS);
        }
        a_result = loc_eval_uint(a_lvalue, a_comparator, a_rvalue);
    }
    break;
    case VTYPE_FLOAT:
    {
        int a_attrib_index = ind_l < 0 ?
            pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_FLOAT, a_skeyword) :
            pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_skeyword);
        double a_lvalue = 0.;
        if (a_attrib_index >= 0) {
            a_lvalue = ind_l < 0 ?
                pre_object.GetFloatValue(a_attrib_index) :
                pre_object.GetFloatValue(a_attrib_index, ind_l);
        }
        else {
            a_lvalue = descr_p->getFloatDefaultValue(a_ikeyword, DOM_COMMON);
        }
        double a_rvalue = 0.;
        if (a_rikeyword > 0)
        {
            a_attrib_index = ind_r < 0 ?
                pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_FLOAT, a_rskeyword) :
                pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_rskeyword);
            if (a_attrib_index >= 0)
            {
                a_rvalue = ind_r < 0 ?
                    pre_object.GetFloatValue(a_attrib_index) :
                    pre_object.GetFloatValue(a_attrib_index, ind_r);
            }
            else
            {
                a_rvalue = descr_p->getFloatDefaultValue(a_rikeyword, DOM_COMMON);
            }
        }
        else
        {
            MCDS_get_expression_attributes(expr_p, EXPR_RVALUE, &a_rvalue, END_ARGS);
        }
        a_result = loc_eval_float(a_lvalue, a_comparator, a_rvalue);
    }
    break;
    case VTYPE_STRING:
    {
        int a_attrib_index = ind_l < 0 ?
            pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, a_skeyword) :
            pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_skeyword);
        string      a_empty = "";
        string      a_default; 
        const char* a_lvalue = a_empty.c_str();
        if (a_attrib_index >= 0) {
            a_lvalue = ind_l < 0 ?
                pre_object.GetStringValue(a_attrib_index) :
                pre_object.GetStringValue(a_attrib_index, ind_l);
        }
        else {
            a_default = descr_p->getStringDefaultValue(a_ikeyword, DOM_COMMON);
            a_lvalue = a_default.c_str();
        }
        const char* a_rvalue = NULL;
        if (a_rikeyword > 0)
        {
            a_attrib_index = ind_r < 0 ?
                pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, a_rskeyword) :
                pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_rskeyword);

            if (a_attrib_index >= 0)
            {
                a_rvalue = ind_r < 0 ?
                    pre_object.GetStringValue(a_attrib_index) :
                    pre_object.GetStringValue(a_attrib_index, ind_r);
            }
            else
            {
                a_default = descr_p->getStringDefaultValue(a_rikeyword, DOM_COMMON);
                a_rvalue = a_default.c_str();
            }
        }
        else
        {
            MCDS_get_expression_attributes(expr_p, EXPR_RVALUE, &a_rvalue, END_ARGS);
        }
        a_result = loc_eval_string(a_lvalue, a_comparator, a_rvalue);
    }
    break;
    case VTYPE_OBJECT:
    {
        int a_attrib_index = ind_l < 0 ?
            pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, a_skeyword) :
            pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_skeyword);
        int a_lvalue = 0;
        const char* a_val_str = "";
        if (a_attrib_index >= 0) {
            if (ind_l < 0)
            {
                a_val_str = pre_object.GetObjectName(a_attrib_index);
                if (a_val_str[0] == '\0')
                    a_lvalue = pre_object.GetObjectId(a_attrib_index);
                else
                    a_lvalue = 1; //assign non-zero
            }
            else
            {
                a_val_str = pre_object.GetObjectName(a_attrib_index, ind_l);
                if (a_val_str[0] == '\0')
                    a_lvalue = pre_object.GetObjectId(a_attrib_index, ind_l);
                else
                    a_lvalue = 1; //assign non-zero
            }

        }
        int a_rvalue = 0;
        a_result = loc_eval_int(a_lvalue, a_comparator, a_rvalue);
    }
    break;
    default:
        break;
    }
    //
    return a_result;
}



static bool loc_evaluate_logical_expression(const IMECPreObject      &pre_object,
					    const expression_t      *expr_p,
					    const IDescriptor       *descr_p,
                        set<int>                *ikeywordchecklst,
                        set<int>                 *ikeywordlst,
					    int                      ind)
{
  expression_t       *a_first_expr_p = NULL;
  logical_operator_e  a_operator     = LGOP_UNKNOWN;
  MCDS_get_expression_attributes(expr_p,
				 EXPR_FIRST_EXPR,&a_first_expr_p,
				 EXPR_OPERATOR,  &a_operator,
				 END_ARGS);
  //
  MvExpression_t a_first_expr(const_cast<expression_t *>(a_first_expr_p),false);
  bool           a_result=pre_object.EvaluateExpression((const PseudoExpression_t *)(&a_first_expr),
							descr_p, ikeywordchecklst, ikeywordlst, ind);
  //
  switch(a_operator) {
  case LGOP_AND:
    if(a_result) {
      expression_t *a_second_expr_p=NULL;
      MCDS_get_expression_attributes(expr_p,EXPR_SECOND_EXPR,&a_second_expr_p,END_ARGS);
      //
      MvExpression_t a_second_expr(a_second_expr_p,false);
      a_result=pre_object.EvaluateExpression((const PseudoExpression_t *)(&a_second_expr),
					     descr_p, ikeywordchecklst, ikeywordlst, ind);
    }
    break;
  case LGOP_OR:
    if(!a_result) {
      expression_t *a_second_expr_p=NULL;
      MCDS_get_expression_attributes(expr_p,EXPR_SECOND_EXPR,&a_second_expr_p,END_ARGS);
      //
      MvExpression_t a_second_expr(a_second_expr_p,false);
      a_result=pre_object.EvaluateExpression((const PseudoExpression_t *)(&a_second_expr),
					     descr_p, ikeywordchecklst, ikeywordlst, ind);
    }
    break;
  case LGOP_NOT:
    a_result=(!a_result);
    break;
  default:
    break;
  }
  //
  return a_result;
}


static bool loc_eval_int(int lvalue,comparator_e op,int rvalue) {
  switch(op) {
  case CMPT_GT: return lvalue>rvalue;  break;
  case CMPT_GE: return lvalue>=rvalue; break;
  case CMPT_LT: return lvalue<rvalue;  break;
  case CMPT_LE: return lvalue<=rvalue; break;
  case CMPT_EQ: return lvalue==rvalue; break;
  case CMPT_NE: return lvalue!=rvalue; break;
  default:      break;
  }
  return false;
}

static bool loc_eval_uint(unsigned int lvalue,comparator_e op,unsigned int rvalue) {
  switch(op) {
  case CMPT_GT: return lvalue>rvalue;  break;
  case CMPT_GE: return lvalue>=rvalue; break;
  case CMPT_LT: return lvalue<rvalue;  break;
  case CMPT_LE: return lvalue<=rvalue; break;
  case CMPT_EQ: return lvalue==rvalue; break;
  case CMPT_NE: return lvalue!=rvalue; break;
  default:      break;
  }
  return false;
}

static bool loc_eval_float(double lvalue,comparator_e op,double rvalue) {
  switch(op) {
  case CMPT_GT: return lvalue>rvalue;  break;
  case CMPT_GE: return lvalue>=rvalue; break;
  case CMPT_LT: return lvalue<rvalue;  break;
  case CMPT_LE: return lvalue<=rvalue; break;
  case CMPT_EQ: return lvalue==rvalue; break;
  case CMPT_NE: return lvalue!=rvalue; break;
  default:      break;
  }
  return false;
}

static bool loc_eval_string(const char *lvalue,comparator_e op,const char *rvalue) {
  switch(op) {
  case CMPT_GT: return strcmp(lvalue,rvalue)>0;  break;
  case CMPT_GE: return strcmp(lvalue,rvalue)>=0; break;
  case CMPT_LT: return strcmp(lvalue,rvalue)<0;  break;
  case CMPT_LE: return strcmp(lvalue,rvalue)<=0; break;
  case CMPT_EQ: return strcmp(lvalue,rvalue)==0; break;
  case CMPT_NE: return strcmp(lvalue,rvalue)!=0; break;
  default:      break;
  }
  return false;
}

/* --------- PARAMETER --------- */
/// Get the name of the parameter used for id
string MECPreObject::GetParameterIdName() const
{
    return myParamIdName;
}

// Get the name of the parameter
string MECPreObject::GetParameterName(const char *skeyword, int index,  bool *is_negated_p) const
{
    if(is_negated_p)
    {
        //! Check if the parameter is a negated one, if yes fill the value for is_negated_p
        *is_negated_p = IsParameterNegated(skeyword, index);
    }
    //! Now get the parameter name
    string a_skeyword = skeyword;
    MyParamMap_t::const_iterator iter = myParameters.find(MyParamKey_t(a_skeyword,index));
    if(iter != myParameters.end())
    {
        return((*iter).second.first);        
    }
    return "";
}

//set the name of the parameter by skeyword and array index
void MECPreObject::SetParameterName(const char* param_name, const char* skeyword, int index, bool is_negated, void* ent_ptr)
{
    //! Set the parameter if negated or not. This we always do as it has to be negated or not negated.
    SetParameterNegated(skeyword, index, is_negated);

    if(param_name && strlen(param_name)!= 0)
    {
        string param_str = param_name;   
        string skeyword_str = skeyword;
        if(myParameters.find(MyParamKey_t(skeyword_str,index)) == myParameters.end())
        {
            myParameters.insert(std::pair<MyParamKey_t, MyParamNameEntPtr_t>(MyParamKey_t(skeyword_str,index), MyParamNameEntPtr_t(param_str, ent_ptr) ) );
        }
    }
}

// Set the parameter usage as negated by skeyword and array index
void MECPreObject::SetParameterNegated(const char* skeyword, int index, bool is_param_negated)
{
    string skeyword_str = skeyword;
    if(myIsNegatedParameters.find(MyParamKey_t(skeyword_str,index)) == myIsNegatedParameters.end())
    {
        myIsNegatedParameters.insert(std::pair<MyParamKey_t,bool>(MyParamKey_t(skeyword_str,index),is_param_negated));
    }
}
// set name of the parameter used for id by skeyword and array index
void MECPreObject::SetParameterIdName(const char* param_name)
{
    if (param_name && strlen(param_name) != 0)
    {
        string param_str = param_name;
        param_str.erase(param_str.find_last_not_of(' ') + 1);        //Trailing
        param_str.erase(0, param_str.find_first_not_of(' '));        //Leading

        myParamIdName = param_str;
    }
}

//check if the parameter is used for an attribute
bool MECPreObject::IsParameter(const char *skeyword,  int index) const
{
    if(myParameters.size() == 0) 
        return false;

    string a_skeyword = skeyword;
    if(myParameters.find(MyParamKey_t(a_skeyword,index)) == myParameters.end())
    {
        return false;
    }
    else
    {
        return true;   
    }    
}

//check if the parameter is used as a negated one
bool MECPreObject::IsParameterNegated(const char *skeyword,  int index) const
{
    string a_skeyword = skeyword;
    return (myIsNegatedParameters.find(MyParamKey_t(a_skeyword,index))->second);   
}



//check if the parameter is used for the id field
bool MECPreObject::IsParameterId() const
{
    if(myParamIdName.length())
        return true;
    else
        return false;   
}
int MECPreObject::GetEntitySubtypeTypeIdentifier() const
{
    return identifier;   
}
void MECPreObject::SetEntitySubtypeTypeIdentifier(int s_identifier)
{
    identifier = s_identifier;   
}
IMECPreObject* MECPreObject::GetSubPreObject(int id)
{
    const vector<IMECPreObject *>& suboject_lst = GetSubobject();

    if (suboject_lst.empty())
        return NULL;

    vector<IMECPreObject *>::const_iterator it = find_if(suboject_lst.begin(), suboject_lst.end(), find_id(id));
    if (it != suboject_lst.end())
    {
        return *it;
    }
     
    return NULL;
}
int MECPreObject::GetEntityType() const
{
    return entity_type;   
}
void MECPreObject::SetEntityType(int hm_entity_type)
{
    entity_type = hm_entity_type;   

    vector<IMECPreObject *>::iterator iter_sub;
    vector<IMECPreObject *>::iterator iter_sub_beg = sub_preobj.begin();
    vector<IMECPreObject *>::iterator iter_sub_end = sub_preobj.end();
    for(iter_sub=iter_sub_beg; iter_sub!=iter_sub_end;++iter_sub)
    {
        MECPreObject *sub_obj = (MECPreObject*)(*iter_sub);
        const char *kfulltype = sub_obj->GetKernelFullType();
        obj_type_e type = HCDI_GetHCObjectType(kfulltype);
        if (type == HCDI_OBJ_TYPE_SUBOBJECT)
            sub_obj->SetEntityType(entity_type);
    }
}


IMECPreObject::MyParamMap_t MECPreObject::GetParameterMap()
{
  return  myParameters;
}

const vector <string>  &MECPreObject::GetUserComment()
{
   return myUserComments;
}
void  MECPreObject::AddUserComment(string const &comment)
{
    myUserComments.push_back(comment);
}
void  MECPreObject::ClearUserComment()
{
    myUserComments.clear();
}


void MECPreObject::EraseFloatArray(const char *skeyword) {
  void                      *a_attributes_p = myAttributes[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1];
  LocFloatArrayAttributes_t &a_attributes   = (*((LocFloatArrayAttributes_t *)a_attributes_p));
  LocKeywordMap_t           &a_keyword_map  = (*((LocKeywordMap_t *)(myKeywordMaps[ATY_ARRAY-ATY_UNKNOWN-1][VTY_FLOAT-VTY_UNKNOWN-1])));
  string                     a_skeyword     = skeyword;
  int                        a_ind          = a_keyword_map[a_skeyword];
  LocFloatArrayAttribute_t  &a_attribute    = a_attributes[a_ind];

  a_attribute.myValue.erase(a_attribute.myValue.begin(),a_attribute.myValue.end());
}

void MECPreObject::EraseAttributeKeyword(IMECPreObject::MyAttributeType_e a_atype, IMECPreObject::MyValueType_e a_vtype, const string &skeyword)
{
    if (a_atype == ATY_UNKNOWN || a_vtype == VTY_UNKNOWN)
        return;

    int a_ind = GetIndex(a_atype, a_vtype, skeyword.c_str());
    if (a_ind < 0)
        return;

    void                *a_attributes_p = myAttributes[a_atype - ATY_UNKNOWN - 1][a_vtype - VTY_UNKNOWN - 1];
    LocKeywordMap_t     &a_keyword_map = (*((LocKeywordMap_t *)(myKeywordMaps[a_atype - ATY_UNKNOWN - 1][a_vtype - VTY_UNKNOWN - 1])));

    switch (a_atype)
    {
    case ATY_SINGLE:
    {
        switch (a_vtype)
        {
        case VTY_BOOL:
        {
            LocIntSingleAttributes_t  &a_attributes = (*((LocIntSingleAttributes_t *)a_attributes_p));
            a_attributes.erase(a_attributes.begin() + a_ind);
            a_keyword_map.erase(skeyword);
            for (int i = 0; i < a_attributes.size(); i++)
            {
                LocKeywordMap_t::iterator it = a_keyword_map.find(a_attributes[i].myKeyword);
                if (it != a_keyword_map.end())
                    it->second = i;
            }
            break;
        }
        case VTY_INT:
        {
            LocIntSingleAttributes_t  &a_attributes = (*((LocIntSingleAttributes_t *)a_attributes_p));
            a_attributes.erase(a_attributes.begin() + a_ind);
            a_keyword_map.erase(skeyword);
            for (int i = 0; i < a_attributes.size(); i++)
            {
                LocKeywordMap_t::iterator it = a_keyword_map.find(a_attributes[i].myKeyword);
                if (it != a_keyword_map.end())
                    it->second = i;
            }
            break;
        }
        case VTY_UINT:
        {
            LocUIntSingleAttributes_t  &a_attributes = (*((LocUIntSingleAttributes_t *)a_attributes_p));
            a_attributes.erase(a_attributes.begin() + a_ind);
            a_keyword_map.erase(skeyword);
            for (int i = 0; i < a_attributes.size(); i++)
            {
                LocKeywordMap_t::iterator it = a_keyword_map.find(a_attributes[i].myKeyword);
                if (it != a_keyword_map.end())
                    it->second = i;
            }
            break;
        }
        case VTY_FLOAT:
        {
            LocFloatSingleAttributes_t  &a_attributes = (*((LocFloatSingleAttributes_t *)a_attributes_p));
            a_attributes.erase(a_attributes.begin() + a_ind);
            a_keyword_map.erase(skeyword);
            for (int i = 0; i < a_attributes.size(); i++)
            {
                LocKeywordMap_t::iterator it = a_keyword_map.find(a_attributes[i].myKeyword);
                if (it != a_keyword_map.end())
                    it->second = i;
            }
            break;
        }
        case VTY_STRING:
        {
            LocStringSingleAttributes_t  &a_attributes = (*((LocStringSingleAttributes_t *)a_attributes_p));
            a_attributes.erase(a_attributes.begin() + a_ind);
            a_keyword_map.erase(skeyword);
            for (int i = 0; i < a_attributes.size(); i++)
            {
                LocKeywordMap_t::iterator it = a_keyword_map.find(a_attributes[i].myKeyword);
                if (it != a_keyword_map.end())
                    it->second = i;
            }
            break;
        }
        case VTY_OBJECT:
        {
            LocObjectSingleAttributes_t  &a_attributes = (*((LocObjectSingleAttributes_t *)a_attributes_p));
            a_attributes.erase(a_attributes.begin() + a_ind);
            a_keyword_map.erase(skeyword);
            for (int i = 0; i < a_attributes.size(); i++)
            {
                LocKeywordMap_t::iterator it = a_keyword_map.find(a_attributes[i].myKeyword);
                if (it != a_keyword_map.end())
                    it->second = i;
            }
            break;
        }
        default:
            break;
        }
    }
    break;
    case ATY_ARRAY:
    {
        switch (a_vtype)
        {
        case VTY_INT:
        {
            LocIntArrayAttributes_t    &a_attributes = (*((LocIntArrayAttributes_t *)a_attributes_p));
            a_attributes.erase(a_attributes.begin() + a_ind);
            a_keyword_map.erase(skeyword);
            for (int i = 0; i < a_attributes.size(); i++)
            {
                LocKeywordMap_t::iterator it = a_keyword_map.find(a_attributes[i].myKeyword);
                if (it != a_keyword_map.end())
                    it->second = i;
                
            }
            break;
        }
        case VTY_UINT:
        {
            LocUIntArrayAttributes_t    &a_attributes = (*((LocUIntArrayAttributes_t *)a_attributes_p));
            a_attributes.erase(a_attributes.begin() + a_ind);
            a_keyword_map.erase(skeyword);
            for (int i = 0; i < a_attributes.size(); i++)
            {
                LocKeywordMap_t::iterator it = a_keyword_map.find(a_attributes[i].myKeyword);
                if (it != a_keyword_map.end())
                    it->second = i;
            }
            break;
        }
        case VTY_FLOAT:
        {
            LocFloatArrayAttributes_t   &a_attributes = (*((LocFloatArrayAttributes_t *)a_attributes_p));
            a_attributes.erase(a_attributes.begin() + a_ind);
            a_keyword_map.erase(skeyword);
            for (int i = 0; i < a_attributes.size(); i++)
            {
                LocKeywordMap_t::iterator it = a_keyword_map.find(a_attributes[i].myKeyword);
                if (it != a_keyword_map.end())
                    it->second = i;
            }
            break;
        }
        case VTY_STRING:
        {
            LocStringArrayAttributes_t  &a_attributes = (*((LocStringArrayAttributes_t *)a_attributes_p));
            a_attributes.erase(a_attributes.begin() + a_ind);
            a_keyword_map.erase(skeyword);
            for (int i = 0; i < a_attributes.size(); i++)
            {
                LocKeywordMap_t::iterator it = a_keyword_map.find(a_attributes[i].myKeyword);
                if (it != a_keyword_map.end())
                    it->second = i;
            }
            break;
        }
        case VTY_OBJECT:
        {
            LocObjectArrayAttributes_t  &a_attributes = (*((LocObjectArrayAttributes_t *)a_attributes_p));
            a_attributes.erase(a_attributes.begin() + a_ind);
            a_keyword_map.erase(skeyword);
            for (int i = 0; i < a_attributes.size(); i++)
            {
                LocKeywordMap_t::iterator it = a_keyword_map.find(a_attributes[i].myKeyword);
                if (it != a_keyword_map.end())
                    it->second = i;
            }
            break;
        }
        default:
            break;
        }
    }
    break;
    default:
        break;
    }
}


void MECPreObject::ClearAllAttribValues(bool is_subobj_arr) {

    if (!is_subobj_arr)
    {
        ((LocIntSingleAttributes_t*)(myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_BOOL - VTY_UNKNOWN - 1]))->clear();
        ((LocIntSingleAttributes_t*)(myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_INT - VTY_UNKNOWN - 1]))->clear();
        ((LocUIntSingleAttributes_t*)(myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_UINT - VTY_UNKNOWN - 1]))->clear();
        ((LocFloatSingleAttributes_t*)(myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_FLOAT - VTY_UNKNOWN - 1]))->clear();
        ((LocStringSingleAttributes_t*)(myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_STRING - VTY_UNKNOWN - 1]))->clear();
        ((LocObjectSingleAttributes_t*)(myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1]))->clear();

        // Array values
        ((LocIntArrayAttributes_t*)(myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_BOOL - VTY_UNKNOWN - 1]))->clear();
        ((LocIntArrayAttributes_t*)(myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_INT - VTY_UNKNOWN - 1]))->clear();
        ((LocUIntArrayAttributes_t*)(myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_UINT - VTY_UNKNOWN - 1]))->clear();
        ((LocFloatArrayAttributes_t*)(myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_FLOAT - VTY_UNKNOWN - 1]))->clear();
        ((LocStringArrayAttributes_t*)(myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_STRING - VTY_UNKNOWN - 1]))->clear();
        ((LocObjectArrayAttributes_t*)(myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1]))->clear();


        int a_nb_atypes = ATY_LAST - ATY_UNKNOWN - 1;
        int a_nb_vtypes = VTY_LAST - VTY_UNKNOWN - 1;
        for (int i = 0; i < a_nb_atypes; ++i) for (int j = 0; j < a_nb_vtypes; ++j) {
            ((LocKeywordMap_t*)myKeywordMaps[i][j])->clear();
        }
    }
    else
    {
        LocIntSingleAttributes_t& a_attributesbool = (*((LocIntSingleAttributes_t*)myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_BOOL - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributesbool)
            element.myValue = 0;

        LocIntSingleAttributes_t& a_attributesint = (*((LocIntSingleAttributes_t*)myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_INT - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributesint)
            element.myValue = 0;

        LocUIntSingleAttributes_t& a_attributesuint = (*((LocUIntSingleAttributes_t*)myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_UINT - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributesuint)
            element.myValue = 0;

        LocFloatSingleAttributes_t& a_attributesfloat = (*((LocFloatSingleAttributes_t*)myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_FLOAT - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributesfloat)
            element.myValue = 0;

        LocStringSingleAttributes_t& a_attributesstring = (*((LocStringSingleAttributes_t*)myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_STRING - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributesstring)
            element.myValue = "";

        LocObjectSingleAttributes_t& a_attributesobject = (*((LocObjectSingleAttributes_t*)myAttributes[ATY_SINGLE - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributesobject)
        {
            LocObjectTrack_t& obj = element.myValue;
            obj.myId = 0;
        }
          
        LocIntArrayAttributes_t& a_attributesarrbool = (*((LocIntArrayAttributes_t*)myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_BOOL - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributesarrbool)
            element.myValue.clear();

        LocIntArrayAttributes_t& a_attributesarrint = (*((LocIntArrayAttributes_t*)myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_INT - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributesarrint)
            element.myValue.clear();

        LocUIntArrayAttributes_t& a_attributesarruint = (*((LocUIntArrayAttributes_t*)myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_UINT - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributesarruint)
            element.myValue.clear();

        LocFloatArrayAttributes_t& a_attributesarrfloat = (*((LocFloatArrayAttributes_t*)myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_FLOAT - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributesarrfloat)
            element.myValue.clear();

        LocStringArrayAttributes_t& a_attributearrsstring = (*((LocStringArrayAttributes_t*)myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_STRING - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributearrsstring)
            element.myValue.clear();

        LocObjectArrayAttributes_t& a_attributesarrobject = (*((LocObjectArrayAttributes_t*)myAttributes[ATY_ARRAY - ATY_UNKNOWN - 1][VTY_OBJECT - VTY_UNKNOWN - 1]));
        for (auto& element : a_attributesarrobject)
        {
            LocObjectArrayAttribute_t &obj = element;
            obj.myValue.clear();
        }
    }

    myParameters.clear();
    myIsNegatedParameters.clear();
    sub_preobj.clear();
}

string &MECPreObject::GetHeaderLine()
{
    return header_line;
}

void MECPreObject::SetHeaderLine(string &str)
{
    header_line = str;
}

void MECPreObject::setConfigType(unsigned int configtype) {
  myConfigType = configtype;
}
void MECPreObject::setHmType(const PseudoDescriptor_t *descr_p) {
  const IDescriptor *a_descr_p=(const MvDescriptor_t *)descr_p;

  const MvIKeywordSet_t &a_ikeywords = a_descr_p->getDefinition(DOM_COMMON,"_HMTYPE");
  MvIKeywordSet_t::const_iterator itr;
  for( itr = a_ikeywords.begin(); itr != a_ikeywords.end() ; ++itr)
  {
      int a_ikey = *itr;
      value_type_e vtype = a_descr_p->getValueType(a_ikey);
      if(vtype != VTYPE_UINT)
          return;
      string a_skey   = a_descr_p->getSKeyword(a_ikey);
      //int indx = GetIndex(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_INT,a_skey.c_str());
      myHmType = GetUIntValue(a_skey.c_str());
      return;
  }
}
unsigned int MECPreObject::getConfigType() const {
  return myConfigType;
}
unsigned int MECPreObject::getHmType() const {
  return myHmType;
}
