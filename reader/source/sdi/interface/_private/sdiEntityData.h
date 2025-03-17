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

#if !defined(SDIENTITYDATA__INCLUDED_)
#define SDIENTITYDATA__INCLUDED_

#include <sdiDefs.h>
#include <sdiEntityBase.h>

#include <_private/sdiModelViewPrivate.h>

namespace sdi
{

// *********************************************************************************
// SDIEntityData
// *********************************************************************************

/// Entity interface to be implemented by the application for its DB
class SDI_DECLS SDIEntityData
{
public:
    virtual bool IsValid() const { return false; }

    //! Retrieve its type
    virtual EntityType GetType() const { return 0; }

    virtual const sdiString& GetKeyword() const { return EMPTYSTRING; }


    //! Creates clone of data
    virtual SDIEntityData* Clone() const
    {
        return GetModelView()->Objectify(GetHandle());
    }

    //! Destroy is only means to delete
    virtual void Destroy() { return; }


    //! Get handle.
    virtual HandleEdit GetHandle() const { return HandleEdit(); }


    ////////////////////////////////////////////////////////////////////////////////
    //! Access to all entity's data
    ////////////////////////////////////////////////////////////////////////////////

    virtual unsigned int GetId() const                      { return GetModelView()->P_GetId(GetHandle()); }
    virtual Status       SetId(const unsigned int id) const { return GetModelView()->P_SetId(GetHandle(), id); }

    virtual sdiString    GetName() const { return GetModelView()->P_GetName(GetHandle()); }
    virtual Status       SetName(const sdiString& name) const { return GetModelView()->P_SetName(GetHandle(), name); }

    virtual HandleEdit GetInclude() const { return HandleEdit(); }
    virtual Status     SetInclude(const HandleRead& hinc) const { return false; }


    //! Underlying implementations need to know what model view entity was created
    //! in when cloning or commiting to DB.
    SDIModelViewPrivate* GetModelView()
    {
        assert(p_pModelView);
        return p_pModelView;
    }
    const SDIModelViewPrivate* GetModelView() const
    {
        assert(p_pModelView);
        return p_pModelView;
    }


    //! This tool helps implement operator== and operator!= at the entity level - maybe should not be virtual
    virtual bool IsEqualTo(const SDIEntityData* pother) const
    {
        return GetHandle() == pother->GetHandle() ? true : false;
    }


    ////////////////////////////////////////////////////////////////////////////////
    //! Descriptior Values and Hierachy by identifier
    //!   * Same functionality is also available through ModelViewPrivate
    ////////////////////////////////////////////////////////////////////////////////

    //! Return false if unable to identify.
    virtual Status GetValue(const sdiIdentifier& identifier,
                            sdiValue&            value) const
    { return GetModelView()->GetValue(GetHandle(), identifier, value); }

    //! Specialized "GetValue" for entity references.
    //! This is at least a convenience API.
    //! Providing a base implementation here that uses p_pModelView->FindById().
    //! Implementer may redefine if desired, e.g. in order to have a performance gain.
    virtual Status GetEntityHandle(const sdiIdentifier& identifier,
                                   HandleEdit&                     handle) const;

    virtual Status SetValue(const sdiIdentifier& identifier,
                            const sdiValue&      value) const
    { return GetModelView()->SetValue(GetHandle(), identifier, value); }

    virtual Status SetEntityHandle(const sdiIdentifier& identifier,
                                   const HandleRead&               handle);

    virtual Status GetAttributes(sdiVector<sdiIdentifier>& aIdentifier) const { return false; }

    //! Return whether particular identifier is parameterized for this entity
    virtual bool IsParameterized(const sdiIdentifier& identifier) const
    { return GetModelView()->IsParameterized(GetHandle(), identifier); }

    //! Return the parameter name if identifier is parameterized for this entity.
    //! If pIsNegated is passed as a valid pointer, *pIsNegated is populated with the info
    //!   whether the parameter is used as is (false) or negated (true).
    virtual sdiString GetParameterName(const sdiIdentifier& identifier,
                                       bool*                pIsNegated) const
    { return GetModelView()->GetParameterName(GetHandle(), identifier, pIsNegated); }

    //! Set a parameterized value.
    //! NB: The value of the parameter replaces the current value in the entity.
    virtual Status SetParameter(const sdiIdentifier& identifier,
                                const sdiString&     parameterName,
                                bool                 isNegated) const
    { return GetModelView()->SetParameter(GetHandle(), identifier, parameterName, isNegated); }

    virtual Status TransformEntity(double matrix[4][4],bool scale) const { return false; }


protected:

    virtual ~SDIEntityData() {}

    explicit SDIEntityData(SDIModelViewPrivate* pModelView) :
        p_pModelView(pModelView) {}


private:

    SDIEntityData() : p_pModelView(0) {}

    SDIModelViewPrivate* p_pModelView;
};

}

#endif //! !defined(SDIENTITYDATA__INCLUDED_)
