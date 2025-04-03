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

#if !defined(SDIFILTER__INCLUDED_)
#define SDIFILTER__INCLUDED_

#include <sdiDefs.h>


namespace sdi
{


// *********************************************************************************
// Foward defines
// *********************************************************************************
class SelectionBase;
class SDIEntityData;


// *********************************************************************************
// First few Filter classes.  Many more at the end of this file
// *********************************************************************************

enum FilterType
{
    SDI_FILTER_NULL,
    SDI_FILTER_AND,
    SDI_FILTER_OR,
    SDI_FILTER_MINUS,
    SDI_FILTER_NOT,
    SDI_FILTER_USERBIT,
    SDI_FILTER_PARENT,
    SDI_FILTER_PARENTSELECTION,
    SDI_FILTER_CHILDSELECTION,
    SDI_FILTER_BYSET,
    SDI_FILTER_BYSETSELECTION,
    SDI_FILTER_CARDIMAGE,
    SDI_FILTER_INCLUDEFILE,
    SDI_FILTER_INCLUDEFILESELECTION,
    SDI_FILTER_BBOX,
    SDI_FILTER_ELEMENTCONFIG,
    SDI_FILTER_ELEMENTCONFIGANDTYPE,
    SDI_FILTER_ELEMENTCONFIGLIST,
    SDI_FILTER_IDPOOLNUMBERS,
    SDI_FILTER_BYASSEMBLY,
    SDI_FILTER_BYASSEMBLYSELECTION,
    SDI_FILTER_SHOWINGUI,
    SDI_FILTER_OUTPUTTOFILE,
    SDI_FILTER_ALL,
    SDI_FILTER_FREEGEOMETRY,
    SDI_FILTER_MODULE,
    SDI_FILTER_CUSTOM,
    SDI_FILTER_MODULECOMBO,
    SDI_FILTER_SKIPUNDEFINED,
    SDI_FILTER_MODULESTRUCTURALTYPEPARTS,
    SDI_FILTER_MODULESTRUCTURALTYPEASSEMBLIES,
    SDI_FILTER_ENGINEERINGTYPE
};


class FilterOr;
class FilterAnd;
class FilterMinus;
class FilterNot;
class FilterNull;


// *********************************************************************************
// The base Filter class, and associated infrastructure
// *********************************************************************************

SDI_DECLS extern sdi::FilterNull nullFilter;


// Copied from EDI and changed to make it work here. Not sure this is the best design...

class SDI_DECLS Filter
{
protected:
    Filter() : p_filter(0) {}

public:
    Filter(const Filter& that);
    Filter& operator=(const Filter& that);

public:
    virtual ~Filter();

    virtual Filter* Clone() const;
    virtual bool IsNull() const { return 0 == p_filter; }

    virtual bool EntityPassesFilter(const SDIEntityData *pData) const = 0;

protected:
    Filter* p_filter;
};


// This is our sentinal object, that is equivalent to a NULL pointer.
// Its "next" pointer is always NULL.

class FilterNull : public Filter
{
public:
    FilterNull() : Filter() {}

    FilterNull(const FilterNull& that) : Filter() {}
    FilterNull& operator=(const FilterNull& that)
    {
        return *this;
    }
    ~FilterNull() {}
    virtual bool IsNull() const
    {
        return true;
    }

    virtual bool EntityPassesFilter(const SDIEntityData *pData) const { return true; }

    virtual Filter* Clone() const
    {
        return 0;
    }
};



// *********************************************************************************
// A bunch more Filter classes. First, all of the boolean operators
// *********************************************************************************
/* EntityPassesFilter() to be implemented?
class SDI_DECLS FilterAnd : public Filter
{
public:
    FilterAnd(const Filter& filter1, const Filter& filter2);

public:
    virtual FilterType GetType() const
    {
        return SDI_FILTER_AND;
    }
    FilterAnd(const FilterAnd& that);
    FilterAnd& operator=(const FilterAnd& that);
    virtual ~FilterAnd();
    const Filter* GetOtherNextFilter() const
    {
        return next2;
    }

private:
    virtual FilterAnd* P_Clone() const;
    Filter* next2;
};


class SDI_DECLS FilterOr : public Filter
{
public:
    FilterOr(const Filter& filter1, const Filter& filter2);

public:
    virtual FilterType GetType() const
    {
        return SDI_FILTER_OR;
    }
    FilterOr(const FilterOr& that);
    FilterOr& operator=(const FilterOr& that);
    virtual ~FilterOr();
    const Filter* GetOtherNextFilter() const
    {
        return next2;
    }

private:
    virtual FilterOr* P_Clone() const;
    Filter* next2;
};


class SDI_DECLS FilterMinus: public Filter
{
public:
    FilterMinus(const Filter& filter1, const Filter& filter2);

public:
    virtual FilterType GetType() const
    {
        return SDI_FILTER_MINUS;
    }
    FilterMinus(const FilterMinus& that);
    FilterMinus& operator=(const FilterMinus& that);
    virtual ~FilterMinus();
    const Filter* GetOtherNextFilter() const
    {
        return next2;
    }

private:
    virtual FilterMinus* P_Clone() const;
    Filter* next2;
};
*/

class SDI_DECLS FilterNot : public Filter
{
public:
    FilterNot(const Filter& filter);
    FilterNot(const FilterNot& that);
    FilterNot& operator=(const FilterNot& that);

public:
    virtual bool EntityPassesFilter(const SDIEntityData *pData) const;

    virtual FilterNot* Clone() const;

    // Filter* p_filter; in base class
};

/*
inline FilterOr operator||(const Filter& filter1, const Filter& filter2)
{
    return FilterOr(filter1, filter2);
}


inline FilterAnd operator&&(const Filter& filter1, const Filter& filter2)
{
    return FilterAnd(filter1, filter2);
}


inline FilterMinus operator-(const Filter& filter1, const Filter& filter2)
{
    return FilterMinus(filter1, filter2);
}


inline FilterNot operator!(const Filter& filter)
{
    return FilterNot(filter);
}


// *********************************************************************************
// A bunch more common Filter classes.
// *********************************************************************************

class SDI_DECLS FilterAll : public Filter
{
public:
    FilterAll(const Filter& filter);

public:
    virtual FilterType GetType() const
    {
        return SDI_FILTER_ALL;
    }
    FilterAll(const FilterAll& that);
    FilterAll& operator=(const FilterAll& that);
    virtual ~FilterAll();

private:
    virtual FilterAll* P_Clone() const;
};


class SDI_DECLS FilterParent : public Filter
{
public:
    FilterParent(EntityType    _parententitytype,
                 const void*   _parent,
                 const Filter& selectionFilter = nullFilter);

public:
    virtual FilterType GetType() const
    {
        return SDI_FILTER_PARENT;
    }
    FilterParent(const FilterParent& that);
    FilterParent& operator=(const FilterParent& that);
    virtual ~FilterParent();
    const EntityType GetEntityType() const
    {
        return parententitytype;
    }
    const void*      GetParent()     const
    {
        return parent;
    }

private:
    virtual FilterParent* P_Clone() const;
    EntityType   parententitytype;
    const void*  parent;
};


class SDI_DECLS FilterCustom : public Filter
{
public:
    FilterCustom(boost::function1<bool, const void* const> func, 
                 const Filter& selectionFilter = nullFilter);

public:
    virtual FilterType GetType() const { 
        return(SDI_FILTER_CUSTOM);
    }

    FilterCustom(const FilterCustom& that);
    FilterCustom& operator=(const FilterCustom& that);
    virtual ~FilterCustom();

    const boost::function1<bool, const void* const> GetCustomFunc() const {
        return(p_func);
        }

private:
    virtual FilterCustom* P_Clone() const;

    boost::function1<bool, const void* const> p_func;
}; // FilterCustom class


class SDI_DECLS FilterBBox : public Filter
{
public:
    FilterBBox(const sdiTriple& _corner0,
               const sdiTriple& _corner1,
               const Filter& selectionFilter = nullFilter);

public:
    virtual FilterType GetType() const
    {
        return SDI_FILTER_BBOX;
    }
    FilterBBox(const FilterBBox& that);
    FilterBBox& operator=(const FilterBBox& that);
    virtual ~FilterBBox();

private:
    virtual FilterBBox* P_Clone() const;
    sdiTriple corner0;
    sdiTriple corner1;
};


class SDI_DECLS FilterSkipUndefined : public Filter
{
public:
    FilterSkipUndefined(const Filter& filter = nullFilter);

public:
    virtual FilterType GetType() const
    {
        return SDI_FILTER_SKIPUNDEFINED;
    }
    FilterSkipUndefined(const FilterSkipUndefined& that);
    FilterSkipUndefined& operator=(const FilterSkipUndefined& that);
    virtual ~FilterSkipUndefined();

private:
    virtual FilterSkipUndefined* P_Clone() const;
};


class SDI_DECLS FilterSpecializationType : public Filter
{
public:
    FilterSpecializationType(const unsigned int specializationTypeAccepted,
                          const Filter& selectionFilter = nullFilter);

public:
    virtual FilterType GetType() const override
    {
        return SDI_FILTER_ENGINEERINGTYPE;
    } 
    FilterSpecializationType(const FilterSpecializationType& that);
    FilterSpecializationType& operator=(const FilterSpecializationType& that);
    virtual ~FilterSpecializationType();
    unsigned int GetSpecializationType() const
    {
        return p_SpecializationType;
    }

protected:
    virtual FilterSpecializationType* P_Clone() const;
    unsigned int p_SpecializationType;
};

*/

class SDI_DECLS FilterValue : public Filter
{
public:
    FilterValue(const sdiIdentifier &identifier, const sdiValue &value) :
        Filter(), p_identifier(identifier), p_value(value)
    {}

public:
    virtual bool EntityPassesFilter(const SDIEntityData *pData) const;

    virtual Filter* Clone() const { return new FilterValue(p_identifier, p_value); }

private:
    sdiIdentifier p_identifier;
    sdiValue p_value;
}; // FilterValue class


}


#endif //! !defined(SDIFILTER__INCLUDED_)
