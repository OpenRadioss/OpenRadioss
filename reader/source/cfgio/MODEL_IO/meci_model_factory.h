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

#ifndef MECI_MODEL_FACTORY_H
#define MECI_MODEL_FACTORY_H

#include <HCDI/hcdi_mec_pre_object.h>

#include "mec_read_file.h"  
#include "mec_component.h" 
#include "mec_offset.h"     
#include "mec_subdeck.h"

#include <UTILS/mv_stl_various.h>
#include "meci_parameter.h"
#if defined _WIN32 || defined WIN32 
#pragma warning(disable:4275)    
#endif 

class MvSubset_t;

typedef const vector<string> MyVecString;
typedef pair< IMECPreObject*, InputInfos::IdentifierValuePairList *>  MyPairPreobjString;


class MECIModelFactory {

public: /** @name Constructors & destructor */
    //@{
    /// Constructor
    inline MECIModelFactory() :my_is_to_load(false), myCurrentOffsetPtr(NULL) 
    {} 
    /// Destructor
    virtual ~MECIModelFactory()
    {
        if (myCurrentOffsetPtr)     
            delete myCurrentOffsetPtr;
    } 
    //@}

public: /** @name Crypting keys */
    //@{
    /// Adds a crypting key (returns false if the reference already exists)
    virtual bool AddCryptingKey(const char* ref, const char* key, const char* cost, const char* feature, const char* module, const char* timeout, const char* interfaceid) = 0;   
    virtual bool UpdateCryptingKeyCost(const char* ref, const char* key, const char* cost) = 0; 
    virtual bool UpdateCryptingKeyFeature(const char* ref, const char* key, const char* feature) = 0; 
    virtual bool UpdateCryptingKeyModule(const char* ref, const char* key, const char* module) = 0; 
    virtual bool UpdateCryptingKeyTimeOut(const char* ref, const char* key, const char* timeout) = 0; 
    virtual bool UpdateCryptingKeyInterfaceID(const char* ref, const char* key, const char* interfaceid) = 0; 
    /// Post-treats the crypting keys
    virtual void PostTreatCryptingKeys() = 0;
    
    /// Gets a crypting key
    virtual const char* GetCryptingKey(const char* ref) const = 0;
    
    virtual const char* GetCryptingKeyCost(const char* ref) const = 0;
    
    virtual const char* GetCryptingKeyFeature(const char* ref) const = 0;
    
    virtual const char* GetCryptingKeyModule(const char* ref) const = 0;
    
    virtual const char* GetCryptingKeyTimeOut(const char* ref) const = 0;
    virtual const char* GetCryptingKeyInterfaceID(const char* ref) const = 0;

public: /** @name Managing current Offset)*/
    //@{
    /// Gets the current offset for a given type
    int virtual GetCurrentOffset(const char* otype) const { return 0; }
    /// Sets the current offset for a given type
    void inline SetCurrentOffset(MECOffset* a_offset) { myCurrentOffsetPtr = a_offset; }
public: /** @name Managing parameter*/
   //@{
    /// Get integer parameter value
    virtual bool SetParameterData(vector<IParameter*>* param_data) = 0;
    /// Get integer parameter value
    virtual int GetIntParameter(const char* param_str, const int file_index) = 0;
    /// Get double parameter value
    virtual double GetFloatParameter(const char* param_str, const int file_index, void** ent_ptr = nullptr) = 0;
    /// Get text parameter value
    virtual string GetTextParameter(const char* param_str, const int file_index) = 0;
    /// Get parameter type
    virtual IParameter::Type GetParameterValueType(const char* param_str, const int file_index) = 0;

    virtual IMECPreObject* FindByFullType(const string& fulltype) = 0;
    virtual IMECPreObject* FindByObjectId(int entity_type, int id) = 0;
    virtual void SortPreObjectsByName(const char* otype) = 0;
public:
    /// Reserving memory for objects of given type
    virtual void Alloc(const char* otype, int nb_objects) = 0;
    virtual int AddObject(const IMECPreObject& pre_object, const InputInfos::IdentifierValuePairList *metaarg=NULL) = 0;
    virtual int AddObjects(const char* otype)=0;
    virtual void PreTreatObject(const char* otype) = 0;
    virtual void PostTreatObject(const char* otype) = 0;
    virtual void AddSubset(int id, const char* title, int nb_children, const int* child_tab, int comp_index, int file_index) = 0;
    virtual void AddSubDeckObjects() = 0;
    virtual void StorePreObject(const int otype, IMECPreObject *preobj, const InputInfos::IdentifierValuePairList* keywordData = nullptr) =0;
    virtual bool isPushInterface() = 0;
    virtual void setPushInterface(bool flag) = 0;
protected:
    MECOffset* myCurrentOffsetPtr;

    bool my_is_to_load;
};



#endif //MECI_MODEL_FACTORY_H
