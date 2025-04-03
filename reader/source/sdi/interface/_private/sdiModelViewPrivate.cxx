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




////////////////////////////////////////////////////////////////////////////////////

#include <_private/sdiModelViewPrivate.h>

#include <_private/sdiEntityData.h>
#include <_private/sdiElementData.h>
#include <_private/sdiNodeData.h>


using namespace sdi;


// to be moved to sdiModelView.cxx?

// P_FindBy... base implementations construct selection and loop through it
bool ModelViewRead::P_FindById(const unsigned int id, const sdiString& keyword, HandleRead& handle) const
{
    SelectionRead sel(this, keyword);
    while(sel.Next())
    {
        if(sel->GetId() == id)
        {
            handle.SetPointer(sel->GetHandle().GetPointer());
            return true;
        }
    }
    // if we get here nothing found
    handle = HandleRead(GetEntityType(keyword));
    return false;
}

bool ModelViewRead::P_FindByName(const sdiString& name, EntityType type, HandleRead& handle, bool ignoreCase) const
{
    SelectionRead sel(this, GetKeyword(type));
    while(sel.Next())
    {
        // NB: ignoreCase == true TBD!
        if(name.compare(sel->GetName()) == 0)
        {
            handle.SetPointer(sel->GetHandle().GetPointer());
            return true;
        }
    }
    // if we get here nothing found
    return false;
}

/// Creation of a new entity. One of the two methods has to be reimplemented,
// the base implementations mutually call the other one.

Status ModelViewEdit::CreateEntity(HandleEdit&               handle,
                                   const sdiString&          keyword,
                                   const sdiString&          name,
                                   const unsigned int        id)
{
    EntityEdit ent;
    Status err = CreateEntity(ent, keyword, name, id);
    if(ent.IsValid())
    {
        handle = ent.GetHandle();
        return err;
    }
    else
    {
        return err;
    }
}

Status ModelViewEdit::CreateEntity(EntityEdit&               entity,
                                   const sdiString&          keyword,
                                   const sdiString&          name,
                                   const unsigned int        id)
{
    HandleEdit handle;
    CreateEntity(handle, keyword, name, id);
    entity = EntityEdit(this, handle);
    return entity.IsValid();
}



SDIModelViewPrivate::~SDIModelViewPrivate()
{
}

EntityEdit SDIModelViewPrivate::GetEntityEdit(SDIEntityData* pData)
{
    return EntityEdit(pData);
}

Status SDIModelViewPrivate::GetEntityHandle(const HandleRead&    handle,
                                            const sdiIdentifier& identifier,
                                            HandleEdit&          valuehandle) const
{
    // get generic sdiValue
    sdiValue value;
    bool isOk = GetValue(handle, identifier, value);
    if(!isOk) { valuehandle.SetPointer(0); return false; }

    // optional: test whether value has the right type
    if(value.GetCompoundType() != sdiCompoundType::COMPOUND_TYPE_ENTITY ||
       value.GetArrayDimension() != 0)
    {
        valuehandle.SetPointer(0); return false;
    }

    // get sdiValueEntity
    sdiValueEntity descriptorentity;
    isOk = value.GetValue(descriptorentity);
    if(!isOk) { valuehandle.SetPointer(0); return false; }

    // find in model by id and type
    unsigned int valueid = descriptorentity.GetId();
    unsigned int valuetype = 0;
    sdiValueEntityType descriptortype = descriptorentity.GetEntityFullType();
    if(descriptortype.IsTypeNumeric())
    {
        valuetype = descriptortype.GetTypeNumeric();
    }
    else
    {
        sdiString typenamed = descriptortype.GetTypeNamed();
        valuetype = GetEntityType(typenamed);
    }
    return FindById(valuetype, valueid, valuehandle);
}

Status SDIModelViewPrivate::SetEntityHandle(const HandleEdit&    handle,
                                            const sdiIdentifier& identifier,
                                            const HandleRead&    valuehandle) const
{
    return SetValue(handle, identifier,
                    sdiValue(sdiValueEntity(sdiValueEntityType(valuehandle.GetType()),
                                            P_GetId(valuehandle))));
}

Status SDIModelViewPrivate::GetEntityHandle(const EntityType  type,
                                            const void*       entityptr,
                                            const sdiIdentifier& identifier,
                                            HandleEdit&       handle) const
{
    // get generic sdiValue
    sdiValue value;
    bool isOk = GetValue(type, entityptr, identifier, value);
    if(!isOk) { handle.SetPointer(0); return false; }

    // optional: test whether value has the right type
    if(value.GetCompoundType() != sdiCompoundType::COMPOUND_TYPE_ENTITY ||
        value.GetArrayDimension() != 0)
    {
        handle.SetPointer(0); return false;
    }

    // get sdiValueEntity
    sdiValueEntity descriptorentity;
    isOk = value.GetValue(descriptorentity);
    if(!isOk) { handle.SetPointer(0); return false; }

    // find in model by id and type
    unsigned int valueid = descriptorentity.GetId();
    unsigned int valuetype = 0;
    sdiValueEntityType descriptortype = descriptorentity.GetEntityFullType();
    if(descriptortype.IsTypeNumeric())
    {
        valuetype = descriptortype.GetTypeNumeric();
    }
    else
    {
        sdiString typenamed = descriptortype.GetTypeNamed();
        valuetype = GetEntityType(typenamed);
    }
    return FindById(valuetype, valueid, handle);
}

Status SDIModelViewPrivate::SetEntityHandle(const EntityType  type,
                                            void*             entityptr,
                                            const sdiIdentifier& identifier,
                                            const HandleRead& handle) const
{
    return SetValue(type, entityptr, identifier,
        sdiValue(sdiValueEntity(sdiValueEntityType(handle.GetType()),
                     P_GetId(handle.GetType(), handle.GetPointer()))));
}

bool SDIModelViewPrivate::IsIdAvailable(EntityType entities, unsigned int id) const
{
    SelectionRead sel(this, GetKeyword(entities));
    while(sel.Next())
    {
        if(sel->GetId() == id) return false;
    }
    return true;
}

unsigned int SDIModelViewPrivate::GetNextAvailableId(EntityType entities) const
{
    unsigned int idmax = 0;
    SelectionRead sel(this, GetKeyword(entities));
    while(sel.Next())
    {
        unsigned int id = sel->GetId();
        if(idmax < id) idmax = id;
    }
    return idmax + 1;
}

