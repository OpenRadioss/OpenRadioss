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

#if !defined(SDISELECTION__INCLUDED_)
#define SDISELECTION__INCLUDED_

#include <sdiFilter.h>
#include <sdiHandles.h>


namespace sdi
{


// *********************************************************************************
// Foward defines
// *********************************************************************************
class ModelViewRead;
class ModelViewEdit;
class IncludeFileRead;
class IncludeFileEdit;
class SelectionIncludeFileRead;
class SelectionIncludeFileEdit;

class SDISelectionData;
class SDIEntityData;



// *********************************************************************************
// SelectionBase.  Note that only subclasses of this will ever be created.
// *********************************************************************************

class SDI_DECLS SelectionBase
{
public:

    // Base selection member functions

    // Navigation.  They are not const, because they will change the "current" pointer.
    /// Move to next selectable entity. Returns false if no more entities can be selected.
    bool Next();
    /// Returns to just prior to first selectable entity.
    void Restart();


    /// Count number of selectable entities.
    unsigned int Count() const;
    /// Check whether selection is empty.
    bool         IsEmpty() const;


protected:   // All construction/destructor access is through entity specific selections

    // Create selection to entities in model view

    SelectionBase(const sdiString&      keyword,
                  const ModelViewRead* pModelView,
                  const Filter& selectionFilter = nullFilter);

    SelectionBase(SpecializationType      engtype,
                  const ModelViewRead* pModelView);


    // Create from data - used by copy constructors and by private classes of SDI
    explicit SelectionBase(const SDISelectionData* pSelectionData);

    // Create from data - take ownership of supplied SelectionData object
    explicit SelectionBase(SDISelectionData* pSelectionData);

    // Destructor
    virtual ~SelectionBase();


    // Used to clone it when applying =operator
    void P_CopyFrom(const SelectionBase& other);


    // Retrieve entity data pointer used maintain reference of current value
    SDIEntityData* P_GetReferenceToCurrentValue() const;


    // Function referring to what is accessible by selection
    bool P_IsSelected(const void* pHandle) const;


    // Hidden data
    SDISelectionData* p_pSelectionData;


private:

    // Default constructor should never be called
    SelectionBase();


public:

    SDISelectionData*       GetData()
    {
        return p_pSelectionData;
    }
    const SDISelectionData* GetData() const
    {
        return p_pSelectionData;
    }
};



// *********************************************************************************
// SelectionRead
// *********************************************************************************

#define COMMON_SDISELECTIONREAD_MEMBERS(ETYPE,SELECTIONREAD)                           \
                                                                                                \
public:                                                                                         \
                                                                                                \
    explicit SELECTIONREAD(const ModelViewRead* pModelView, \
                           const sdiString&      keyword,                                        \
                           const Filter&        selectionFilter=nullFilter)                     \
            : SelectionBase(keyword, pModelView, selectionFilter),                              \
              p_currentEntity( (SDITypeGuide<ETYPE>::data*)P_GetReferenceToCurrentValue() )     \
              {}                                                                                \
                                                                                                \
    SELECTIONREAD(const SDITypeGuide<ETYPE>::selectionread& other)         \
           : SelectionBase(other.p_pSelectionData),                            \
             p_currentEntity( (SDITypeGuide<ETYPE>::data*)P_GetReferenceToCurrentValue() )      \
             {}                                                                                 \
                                                                                                \
    virtual ~SELECTIONREAD() { p_currentEntity.p_pData = 0; }                                   \
                                                                                                \
                                                                                                \
    inline SELECTIONREAD& operator=(const SDITypeGuide<ETYPE>::selectionread& other);           \
    inline SELECTIONREAD& operator=(const SDITypeGuide<ETYPE>::selectionedit& other);           \
                                                                                                \
    const SDITypeGuide<ETYPE>::entityread &operator*()  const                                   \
    { return *(SDITypeGuide<ETYPE>::entityread*)(&p_currentEntity); }                           \
                                                                                                \
    const SDITypeGuide<ETYPE>::entityread *operator->() const                                   \
    { return  (SDITypeGuide<ETYPE>::entityread*)(&p_currentEntity); }                           \
                                                                                                \
    const ModelViewRead* GetModelViewRead() const                                               \
    { return p_currentEntity.GetModelViewRead(); }                                              \
                                                                                                \
    bool IsSelected(const SDITypeGuide<ETYPE>::handleread& handle) const                        \
            { return P_IsSelected(handle.GetPointer()); }                                       \
                                                                                                \
                                                                                                \
protected:                                                                                      \
    /* entity wrapping an EntityData owned by p_pSelectionData                                  \
       (retrieved by GetReferenceToCurrentValue() */                                            \
    SDITypeGuide<ETYPE>::entityedit  p_currentEntity;                                           \
                                                                                                \
public:                                                                                         \
                                                                                                \
    explicit SELECTIONREAD(const SDISelectionData* pSelectionData)                              \
            : SelectionBase(pSelectionData),                                                    \
              p_currentEntity( (SDITypeGuide<ETYPE>::data*)P_GetReferenceToCurrentValue() )     \
              {}


// *********************************************************************************
// SelectionEdit
// *********************************************************************************

#define COMMON_SDISELECTIONEDIT_MEMBERS(ETYPE,SELECTIONEDIT)                          \
                                                                                               \
public:                                                                                        \
                                                                                               \
    explicit SELECTIONEDIT(const ModelViewEdit* pModelView, \
                           const sdiString&      keyword)                  \
           : SDITypeGuide<ETYPE>::selectionread((const ModelViewRead*)pModelView,              \
                                                 keyword) {}                           \
                                                                                               \
    SELECTIONEDIT(const SDITypeGuide<ETYPE>::selectionedit& other)      \
           : SDITypeGuide<ETYPE>::selectionread(other) {}                     \
                                                                                               \
    inline SDITypeGuide<ETYPE>::selectionedit& operator=(                                      \
                                         const SDITypeGuide<ETYPE>::selectionedit& other);     \
                                                                                               \
    virtual ~SELECTIONEDIT() {}                                                                \
                                                                                               \
                                                                                               \
    const SDITypeGuide<ETYPE>::entityedit &operator*()  const { return  p_currentEntity; }     \
    const SDITypeGuide<ETYPE>::entityedit *operator->() const { return &p_currentEntity; }     \
                                                                                               \
    const ModelViewRead* GetModelViewRead() const                                              \
    { return p_currentEntity.GetModelViewRead(); }                                             \
                                                                                               \
                                                                                               \
public:                                                                                        \
                                                                                               \
    explicit SELECTIONEDIT(const SDISelectionData* pSelectionData)                             \
           : SDITypeGuide<ETYPE>::selectionread(pSelectionData) {}



#define COMMON_SDISELECTION_BODIES(ETYPE)                                                         \
                                                                                               \
    inline SDITypeGuide<ETYPE>::selectionread &                                                \
           SDITypeGuide<ETYPE>::selectionread::operator=(                                      \
                         const SDITypeGuide<ETYPE>::selectionread& other)                      \
    { P_CopyFrom(other);                                                                       \
      p_currentEntity.p_pData = (SDITypeGuide<ETYPE>::data*)P_GetReferenceToCurrentValue();    \
      return *this; }                                                                          \
                                                                                               \
    inline SDITypeGuide<ETYPE>::selectionread &                                                \
           SDITypeGuide<ETYPE>::selectionread::operator=(                                      \
                         const SDITypeGuide<ETYPE>::selectionedit& other)                      \
    { P_CopyFrom(other);                                                                       \
      p_currentEntity.p_pData = (SDITypeGuide<ETYPE>::data*)P_GetReferenceToCurrentValue();    \
      return *this; }                                                                          \
                                                                                               \
    inline SDITypeGuide<ETYPE>::selectionedit &                                                \
           SDITypeGuide<ETYPE>::selectionedit::operator=(                                      \
                         const SDITypeGuide<ETYPE>::selectionedit& other)                      \
    {  SDITypeGuide<ETYPE>::selectionread::operator=(other); return *this; }


}

#endif //! !defined(SDISELECTION__INCLUDED_)
