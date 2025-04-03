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
#ifndef MV_MODEL_FACTORY_H
#define MV_MODEL_FACTORY_H

#include <UTILS/mv_stl_various.h>
#include <MUNITS/mu_unit_map.h>   
#include <MESSAGE/msg_manager.h>
#include "meci_model_factory.h"
#include "meci_parameter.h"
#if defined _WIN32 || defined WIN32 
#pragma warning(disable:4251)    
#endif 
#if defined _WIN32 || defined WIN32 
#pragma warning(disable: 4290)  
#endif 
class HCIO_DATA_DLL_API MvModelFactory_t : public MECIModelFactory, public MvMsgManager_t {
public:
    MvModelFactory_t(vector<IMECPreObject*>* PreobjLst = nullptr) : myParameterData(NULL), myPreobjLst(PreobjLst) { }
    /// Destructor
    virtual ~MvModelFactory_t() {}
public: /** @name Crypting keys */
 //@{
 /// Adds a crypting key (returns false if the reference already exists)
    virtual bool AddCryptingKey(const char* ref, const char* key, const char* cost, const char* feature, const char* module, const char* timeout, const char* interfaceid);
    virtual bool UpdateCryptingKeyCost(const char* ref, const char* key, const char* cost);
    virtual bool UpdateCryptingKeyFeature(const char* ref, const char* key, const char* feature);
    virtual bool UpdateCryptingKeyModule(const char* ref, const char* key, const char* module);
    virtual bool UpdateCryptingKeyTimeOut(const char* ref, const char* key, const char* timeout);
    virtual bool UpdateCryptingKeyInterfaceID(const char* ref, const char* key, const char* interfaceid);
    /// Post-treats the crypting keys
    virtual void PostTreatCryptingKeys();
    
    /// Gets a crypting key
    virtual const char* GetCryptingKey(const char* ref) const;
    
    virtual const char* GetCryptingKeyCost(const char* ref) const;
    
    virtual const char* GetCryptingKeyFeature(const char* ref) const;
    
    virtual const char* GetCryptingKeyModule(const char* ref) const;
    
    virtual const char* GetCryptingKeyTimeOut(const char* ref) const;
    
    virtual const char* GetCryptingKeyInterfaceID(const char* ref) const;
public: /** @name Managing parameter*/
   //@{
    virtual IParameter* GetParameterObject(const char* param_str, const int file_index, void** ent_ptr=nullptr);
    /// Get integer parameter value
    virtual bool SetParameterData(vector<IParameter*>* param_data_sorted);
    /// Get integer parameter value
    virtual int GetIntParameter(const char* param_str, const int file_index);
    /// Get double parameter value
    virtual double GetFloatParameter(const char* param_str, const int file_index, void** ent_ptr = nullptr);
    /// Get text parameter value
    virtual string GetTextParameter(const char* param_str, const int file_index);

    /// Get parameter value type
    virtual IParameter::Type GetParameterValueType(const char* param_str, const int file_index);
    typedef map<string, string>                          MyCryptingKeys_t;
    typedef map<string, string>                          MyCryptingKeysCost_t; 
    typedef map<string, string>                          MyCryptingKeysFeature_t; 
    typedef map<string, string>                          MyCryptingKeysModule_t; 
    typedef map<string, string>                          MyCryptingKeysTimeOut_t; 
    typedef map<string, string>                          MyCryptingKeysInterfaceID_t; 

public:
    /// Reserving memory for objects of given type
    virtual void Alloc(const char* otype, int nb_objects) { return; }
    virtual int AddObject(const IMECPreObject& pre_object, const InputInfos::IdentifierValuePairList  *metaarg=NULL) { return 0; }
    virtual int AddObjects(const char* otype);
    virtual void AddSubDeckObjects() { }
    virtual void AddSubset(int id, const char* title, int nb_children, const int* child_tab, int comp_index, int file_index) { return; }
public:
    IMECPreObject* FindByFullType(const string& fulltype);
    IMECPreObject* FindByObjectId(int entity_type, int id);

    void SortPreObjectsByName(const char* otype);

    void SetPreObjectLst(vector<IMECPreObject*>* PreobjLst) { myPreobjLst = PreobjLst; }
    vector<IMECPreObject *> *GetPreObjectLst() { return myPreobjLst; }
    vector<IMECPreObject *>& GetPreObjectLst(int otype) { return myPreobjLst[otype]; }

    //void SetPreObjectCommentLst(int otype, vector<MyPairPreobjString> &PreobjLst) { myPreobjCommentLst[otype] = PreobjLst; }
    //vector<MyPairPreobjString> &GetPreObjectCommentLst(int otype) { return myPreobjCommentLst[otype]; }

    virtual void PreTreatObject(const char* otype) {   }
    virtual void PostTreatObject(const char* otype) {   }
    void StorePreObject(const int otype, IMECPreObject* pre_object, const InputInfos::IdentifierValuePairList* keywordData = nullptr);
    bool isPushInterface() { return myIsPushInterface; }
    void setPushInterface(bool flag) { myIsPushInterface = flag; }
private:
    MyCryptingKeys_t             myCryptingKeys;
    MyCryptingKeysCost_t         myCryptingKeysCost;
    MyCryptingKeysFeature_t      myCryptingKeysFeature;
    MyCryptingKeysModule_t       myCryptingKeysModule;
    MyCryptingKeysTimeOut_t      myCryptingKeysTimeOut;
    MyCryptingKeysInterfaceID_t      myCryptingKeysInterfaceID;
    vector<IParameter*>* myParameterData;
    vector<IMECPreObject*>* myPreobjLst;

protected:
    //for generic reader
    //vector<MyPairPreobjString>  myPreobjCommentLst[HCDI_OBJ_TYPE_HC_MAX];
    bool                        myIsPushInterface=false;
};

#endif //MV_MODEL_FACTORY_H

