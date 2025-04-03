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

#include <sdiFilter.h>
#include <_private/sdiEntityData.h>


using namespace sdi;


// The singleton sentinal object indicating the end of a filter linked list.
sdi::FilterNull sdi::nullFilter;


Filter::Filter(const Filter& that)
{
    p_filter = that.Clone();
}

Filter::~Filter()
{
    if (p_filter) delete p_filter;
}

Filter* Filter::Clone() const
{
    if(p_filter) return p_filter->Clone();
    return NULL;
}

Filter& Filter::operator=(const Filter& that)
{
    if (this != &that)
    {
        if (p_filter) delete p_filter;
        p_filter = that.Clone();
    }
    return *this;
}



// *********************************************************************************
//   Filter Booleans
// *********************************************************************************
/* EntityPassesFilter() to be implemented?
FilterAnd::FilterAnd(const Filter& filter1, const Filter& filter2)
    : Filter(filter1), next2(filter2.P_Clone())
{
}

FilterAnd::FilterAnd(const FilterAnd& that)
    : Filter(that.next),
      next2(that.next2 ? that.next2->P_Clone() : 0)
{
}

FilterAnd& FilterAnd::operator=(const FilterAnd& that)
{
    if (this != &that)
    {
        P_AssignNextPointer(that.next);
        if (next2)
        {
            delete next2;
        }
        next2 = that.next2 ? that.next2->P_Clone() : 0;
    }
    return *this;
}

FilterAnd::~FilterAnd()
{
    if (next2)
    {
        delete next2;
    }
}

FilterAnd* FilterAnd::P_Clone() const
{
    return new FilterAnd(next ? *next : nullFilter, next2 ? *next2 : nullFilter);
}


FilterOr::FilterOr(const Filter& filter1, const Filter& filter2)
    : Filter(filter1),
      next2(filter2.P_Clone())
{
}

FilterOr::FilterOr(const FilterOr& that)
    : Filter(that.next),
      next2(that.next2 ? that.next2->P_Clone() : 0)
{
}

FilterOr& FilterOr::operator=(const FilterOr& that)
{
    if (this != &that)
    {
        P_AssignNextPointer(that.next);
        if (next2)
        {
            delete next2;
        }
        next2 = that.next2 ? that.next2->P_Clone() : 0;
    }
    return *this;
}

FilterOr::~FilterOr()
{
    if (next2)
    {
        delete next2;
    }
}

FilterOr* FilterOr::P_Clone() const
{
    return new FilterOr(next ? *next : nullFilter, next2 ? *next2 : nullFilter);
}


FilterMinus::FilterMinus(const Filter& filter1, const Filter& filter2)
    : Filter(filter1),
      next2(filter2.P_Clone())
{
}

FilterMinus::FilterMinus(const FilterMinus& that)
    : Filter(that.next),
      next2(that.next2 ? that.next2->P_Clone() : 0)
{
}

FilterMinus& FilterMinus::operator=(const FilterMinus& that)
{
    if (this != &that)
    {
        P_AssignNextPointer(that.next);
        if (next2)
        {
            delete next2;
        }
        next2 = that.next2 ? that.next2->P_Clone() : 0;
    }
    return *this;
}

FilterMinus::~FilterMinus()
{
    if (next2)
    {
        delete next2;
    }
}

FilterMinus* FilterMinus::P_Clone() const
{
    return new FilterMinus(next ? *next : nullFilter, next2 ? *next2 : nullFilter);
}
*/

FilterNot::FilterNot(const Filter& filter) :
    Filter()
{
    p_filter = filter.Clone();
}

FilterNot::FilterNot(const FilterNot& that) :
    Filter()
{
    if(that.p_filter) p_filter = that.p_filter->Clone();
}

FilterNot& FilterNot::operator=(const FilterNot& that)
{
    if (this != &that)
    {
        if(p_filter) delete p_filter;
        p_filter = that.p_filter ? that.p_filter->Clone() : NULL;
    }
    return *this;
}

FilterNot* FilterNot::Clone() const
{
    return new FilterNot(p_filter ? *p_filter : nullFilter);
}

bool FilterNot::EntityPassesFilter(const SDIEntityData *pData) const
{
    if(!p_filter) return false;
    return !p_filter->EntityPassesFilter(pData);
}

/*
// FilterAll ###########################################################################


FilterAll::FilterAll(const Filter& filter) : Filter(filter)
{
}


FilterAll::FilterAll(const FilterAll& that) : Filter(that.next)
{
}


FilterAll& FilterAll::operator=(const FilterAll& that)
{
    if (this != &that)
    {
        P_AssignNextPointer(that.next);
    }
    return *this;
}


FilterAll::~FilterAll()
{
}


FilterAll* FilterAll::P_Clone() const
{
    return new FilterAll(next ? *next : nullFilter);
}


// *********************************************************************************
//   FilterParent
// *********************************************************************************

FilterParent::FilterParent(
    EntityType     _parententitytype,
    const void*    _parent,
    const Filter& selectionFilter)
    : Filter(selectionFilter),
      parententitytype(_parententitytype),
      parent(_parent)
{
}

FilterParent::FilterParent(const FilterParent& that)
    : Filter(that.next),
      parententitytype(that.parententitytype),
      parent(that.parent)
{
}

FilterParent& FilterParent::operator=(const FilterParent& that)
{
    if (this != &that)
    {
        P_AssignNextPointer(that.next);
        parententitytype = that.parententitytype;
        parent           = that.parent;
    }
    return *this;
}

FilterParent::~FilterParent()
{
}

FilterParent* FilterParent::P_Clone() const
{
    return new FilterParent(parententitytype, parent, next ? *next : nullFilter);
}


// *********************************************************************************
//   FilterCustom
// *********************************************************************************
FilterCustom::FilterCustom(boost::function1<bool, const void* const> func, const Filter& selectionFilter) : 
        Filter(selectionFilter),
        p_func(func)
{
} // FilterCustom ctor


FilterCustom::FilterCustom(const FilterCustom& that) : 
        Filter(that.next),
        p_func(that.p_func)
{
} // FilterCustom copy ctor


FilterCustom::~FilterCustom()
{
} // FilterCustom dtor


FilterCustom& FilterCustom::operator=(const FilterCustom& that)
{
    if(this != &that) {
        P_AssignNextPointer(that.next);
        p_func = that.p_func;
        }

    return(*this);
} // operator=


FilterCustom* FilterCustom::P_Clone() const
{
    return( new FilterCustom(p_func, next ? *next : nullFilter) );
} // P_Clone


// *********************************************************************************
//   FilterBBox
// *********************************************************************************

FilterBBox::FilterBBox(const sdiTriple& _corner0,
                       const sdiTriple& _corner1,
                       const Filter& selectionFilter)
    : Filter(selectionFilter),
      corner0(_corner0),
      corner1(_corner1)
{
}

FilterBBox::FilterBBox(const FilterBBox& that)
    : Filter(that.next),
      corner0(that.corner0),
      corner1(that.corner1)
{
}

FilterBBox& FilterBBox::operator=(const FilterBBox& that)
{
    if (this != &that)
    {
        P_AssignNextPointer(that.next);
        corner0 = that.corner0;
        corner1 = that.corner1;
    }
    return *this;
}

FilterBBox::~FilterBBox()
{
}

FilterBBox* FilterBBox::P_Clone() const
{
    return new FilterBBox(corner0, corner1, next ? *next : nullFilter);
}

// FilterSkipUndefined ###########################################################################


FilterSkipUndefined::FilterSkipUndefined(const Filter& filter) : Filter(filter)
{
}


FilterSkipUndefined::FilterSkipUndefined(const FilterSkipUndefined& that) : Filter(that.next)
{
}


FilterSkipUndefined& FilterSkipUndefined::operator=(const FilterSkipUndefined& that)
{
    if (this != &that)
    {
        P_AssignNextPointer(that.next);
    }
    return *this;
}


FilterSkipUndefined::~FilterSkipUndefined()
{
}


FilterSkipUndefined* FilterSkipUndefined::P_Clone() const
{
    return new FilterSkipUndefined(next ? *next : nullFilter);
}

// FilterSpecializationType ###########################################################################

FilterSpecializationType::FilterSpecializationType(const unsigned int specializationTypeAccepted,
                                             const Filter& selectionFilter)
    : Filter(selectionFilter), p_SpecializationType(specializationTypeAccepted)
{ }

FilterSpecializationType::FilterSpecializationType(const FilterSpecializationType& that)
    : Filter(that.next), p_SpecializationType(that.p_SpecializationType)
{ }

FilterSpecializationType& FilterSpecializationType::operator=(const FilterSpecializationType& that)
{
    if (this != &that)
    {
        P_AssignNextPointer(that.next);
        p_SpecializationType = that.p_SpecializationType;
    }
    return *this;
}

FilterSpecializationType::~FilterSpecializationType()
{ }

FilterSpecializationType* FilterSpecializationType::P_Clone() const
{
    return new FilterSpecializationType(p_SpecializationType, next ? *next : nullFilter);
}

*/

bool FilterValue::EntityPassesFilter(const SDIEntityData *pData) const
{
    if(!pData) return false;
    sdiValue value;
    pData->GetValue(p_identifier, value);
    if(p_value.GetCompoundType() == COMPOUND_TYPE_ENTITY)
    {
        sdiValueEntity filterentity, entityentity;
        sdiValueEntityType entitytype;
        if(value.GetCompoundType() == COMPOUND_TYPE_ENTITY)
        {
            value.GetValue(entityentity);
            entitytype = entityentity.GetEntityFullType();
        }
        else if(value.GetCompoundType() != COMPOUND_TYPE_UNDEFINED)
        {
            return false; // mismatch for some odd reason
        } // else entity remains 0, but the check may be against 0
        // more permissive check for entities here:
        // p_value has no entitytype specified: only check id
        p_value.GetValue(filterentity);
        sdiValueEntityType filtertype = filterentity.GetEntityFullType();
        if(!filtertype.IsValid()) // filter type not set: just compare id
        {
            return filterentity.GetId() == entityentity.GetId();
        }
        else if(filtertype.IsTypeNumeric())
        {
            if(entitytype.IsTypeNumeric())
            {
                if(filtertype.GetTypeNumeric() != entitytype.GetTypeNumeric()) return false;
                return filterentity.GetId() == entityentity.GetId();
            }
            else
            {
                unsigned int entitytypenumeric = pData->GetModelView()->GetEntityType(entitytype.GetTypeNamed());
                if(filtertype.GetTypeNumeric() != entitytypenumeric) return false;
                return filterentity.GetId() == entityentity.GetId();
            }
        }
        else // filtertype is named
        {
            if(!entitytype.IsTypeNumeric())
            {
                if(filtertype.GetTypeNamed() != entitytype.GetTypeNamed()) return false;
                return filterentity.GetId() == entityentity.GetId();
            }
            else
            {
                unsigned int filtertypenumeric = pData->GetModelView()->GetEntityType(filtertype.GetTypeNamed());
                if(filtertypenumeric != entitytype.GetTypeNumeric()) return false;
                return filterentity.GetId() == entityentity.GetId();
            }
        }
    }
    return p_value == value;
}
