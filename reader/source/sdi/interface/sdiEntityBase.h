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

#if !defined(SDIENTITYBASE__INCLUDED_)
#define SDIENTITYBASE__INCLUDED_

#include <sdiDefs.h>
#include <sdiHandles.h>
#include <sdiSelection.h>


namespace sdi
{


// *********************************************************************************
// Foward defines
// *********************************************************************************
class SDIEntityData;
class SelectionRead;
class SelectionEdit;



// *********************************************************************************
// EntityBaseRead
// *********************************************************************************
class SDI_DECLS EntityBaseRead
{

public:

    //!
    //! Read only functionality
    //!

    unsigned int GetId() const;
    sdiString     GetName() const;
    HandleRead   GetInclude() const;
        
    EntityType GetType() const;
    const sdiString& GetKeyword() const; // Lifetime of returned string guaranteed as long as this.
    
    const ModelViewRead* GetModelViewRead() const;

    ////////////////////////////////////////////////////////////////////////////////
    //! Descriptior Values and Hierachy by identifier
    ////////////////////////////////////////////////////////////////////////////////

    //! Return false if unable to identify.
    Status GetValue(const sdiIdentifier& identifier,
                    sdiValue&            value) const;

    //! Specialized "GetValue" for entity references.
    //! This is at least a convenience API.
    //! Providing a base implementation here that uses p_pModelView->FindById().
    //! Implementer may redefine if desired, e.g. in order to have a performance gain.
    Status GetEntityHandle(const sdiIdentifier& identifier,
                           HandleRead&                     handle) const;

    Status GetAttributes(sdiVector<sdiIdentifier>& aIdentifier) const;

    //! Return whether particular identifier is parameterized for this entity
    bool IsParameterized(const sdiIdentifier& identifier) const;

    //! Return the parameter name if identifier is parameterized for this entity.
    //! If pIsNegated is passed as a valid pointer, *pIsNegated is populated with the info
    //!   whether the parameter is used as is (false) or negated (true).
    sdiString GetParameterName(const sdiIdentifier& identifier,
                               bool*                pIsNegated = nullptr) const;


    //! Operators
    bool operator==(const EntityBaseRead& other) const;
    bool operator!=(const EntityBaseRead& other) const
    {
        return !operator==(other);
    }

    
    bool IsValid() const
    {
        return GetData() ? true : false;
    }

protected:

    EntityBaseRead() {};
    virtual ~EntityBaseRead() {}

    //! Access to private data
    virtual SDIEntityData*       GetData() { return 0; }
    virtual const SDIEntityData* GetData() const { return 0; }
};



// *********************************************************************************
// EntityBaseEdit
// *********************************************************************************
class SDI_DECLS EntityBaseEdit
{

public:

    //!
    //! Edit functionality
    //!

    Status SetId(const unsigned int id) const;        //! returns false if unable to assign id
    Status SetName(const sdiString& name) const;       //! returns false if unable
    HandleEdit GetInclude() const;
    Status SetInclude(const HandleRead& includefile) const;

    const ModelViewEdit* GetModelViewEdit() const;


    ////////////////////////////////////////////////////////////////////////////////
    //! Descriptior Values by identifier
    //!     Return false if unable to identify.
    ////////////////////////////////////////////////////////////////////////////////

    Status GetEntityHandle(const sdiIdentifier& identifier,
                           HandleRead&                     handle) const;

    Status GetEntityHandle(const sdiIdentifier& identifier,
                           HandleEdit&                     handle) const;

    Status SetValue(const sdiIdentifier& identifier,
                    const sdiValue&      value) const;

    //! Set a parameterized value.
    //! NB: The value of the parameter replaces the current value in the entity.
    Status SetParameter(const sdiIdentifier& identifier,
        const sdiString&                  parameterName,
        bool                             isNegated = false) const;

    Status SetEntityHandle(const sdiIdentifier& identifier,
                           const HandleRead&               handle);


    //Transform an entity
    Status TransformEntity(double matrix[4][4],bool scale) const;

protected:

    EntityBaseEdit() {}
    virtual ~EntityBaseEdit() {}


    //! Used to access data of derived classes
    virtual SDIEntityData* GetData() { return 0; }
    virtual const SDIEntityData* GetData() const { return 0; }
};


}

#endif //! !defined(SDIENTITYBASE__INCLUDED_)
