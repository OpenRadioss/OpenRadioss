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

#if !defined(SDISELECTIONOBSERVER__INCLUDED_)
#define SDISELECTIONOBSERVER__INCLUDED_

#include <_private/sdiModelViewPrivate.h>
#include <_private/sdiEntityData.h>
#include <_private/sdiSelectionData.h>

#include <stack>

namespace sdi
{


class SDISelectionObserver
{
public:
    virtual void SelectionConstructed(const SDISelectionData& selection) = 0;
    virtual void SelectionNext(const SDISelectionData& selection) = 0;
    virtual void SelectionDestructed(const SDISelectionData& selection) = 0;
    
    virtual ~SDISelectionObserver() {}

}; // SelectionObserver class

/// ModelView whose selection is observable by a SelectionObserver
class SDIModelViewSelectionObservable : public SDIModelViewPrivate
{

public:

    void SetSelectionObserver(SDISelectionObserver *pObserver, bool ownsObserver = false)
    {
        if(p_ownsObserver && nullptr != p_pObserver && pObserver != p_pObserver)
        {
            delete p_pObserver;
            p_pObserver = nullptr;
        }
        p_pObserver = pObserver;
        p_ownsObserver = ownsObserver;
    }
    SDISelectionObserver *GetSelectionObserver() const
    { return p_pObserver; }

protected:

    SDIModelViewSelectionObservable()
    {}

    SDIModelViewSelectionObservable(SDISelectionObserver *pObserver, bool ownsObserver = false) :
        p_pObserver(pObserver), p_ownsObserver(ownsObserver)
    {}

    virtual ~SDIModelViewSelectionObservable()
    {
        if(p_ownsObserver) delete p_pObserver;
    }

private:
    SDISelectionObserver *p_pObserver = nullptr;
    bool p_ownsObserver = false;
};


class SDICurrentIncludeFromSelectionSetter : public SDISelectionObserver
{
public:
    SDICurrentIncludeFromSelectionSetter(ModelViewEdit& targetModel, EntityType targetIncludeType) :
        p_targetModel(targetModel), p_targetIncludeType(targetIncludeType)
    {
        p_currentIncludeIds.push_back(0); // lowest level is always root
    }

    virtual ~SDICurrentIncludeFromSelectionSetter() {}

    virtual void SelectionConstructed(const SDISelectionData& selection)
    {
        unsigned int currentIncludeId = p_currentIncludeIds.back();
        p_currentIncludeIds.push_back(currentIncludeId);
    }
    virtual void SelectionNext(const SDISelectionData& selection)
    {
        unsigned int currentIncludeId = 0;
        if(selection.GetReferenceToCurrentValue() != nullptr &&
           selection.GetReferenceToCurrentValue()->GetHandle().IsValid()) // selection is ok
        {
            HandleRead currentInclude = selection.GetReferenceToCurrentValue()->GetInclude();
            currentIncludeId = currentInclude.GetId(selection.GetModelViewRead());
            assert(p_currentIncludeIds.size() > 0);
            p_currentIncludeIds.back() = currentIncludeId;
        }
        else // advanced past the end, reset the previous 
        {
            assert(p_currentIncludeIds.size() > 1);
            currentIncludeId = p_currentIncludeIds[p_currentIncludeIds.size() - 2];
        }
        P_SetInclude(currentIncludeId);
    }
    virtual void SelectionDestructed(const SDISelectionData& selection)
    {
        assert(p_currentIncludeIds.size() > 1);
        unsigned int currentIncludeId = p_currentIncludeIds[p_currentIncludeIds.size() - 2];
        P_SetInclude(currentIncludeId);
        p_currentIncludeIds.pop_back();
    }

private:
    HandleRead P_SetInclude(unsigned int currentIncludeId)
    {
        HandleRead currentInclude;
        p_targetModel.FindById(p_targetIncludeType, currentIncludeId, currentInclude);
        return p_targetModel.SetCurrentCollector(currentInclude);
    }

private:
    ModelViewEdit& p_targetModel;
    EntityType p_targetIncludeType;
    std::vector<unsigned int> p_currentIncludeIds;
};

} // namespace sdi

#endif //! !defined(SDISELECTIONOBSERVER__INCLUDED_)
