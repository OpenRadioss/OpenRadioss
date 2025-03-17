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

#include <sdiValue.h>



// *********************************************************************************
// sdiValue - Implementation.
// *********************************************************************************

template<class T>
sdiValuePtr* _CreateSubValuePtr(const sdiValue& other, const unsigned int  i, const unsigned int  j)
{
    T val;
    bool isOk = other.GetValue(val, i, j);
    if(!isOk) return nullptr;
    return sdiValuePtr::Create(val);
}

sdiValue::sdiValue(const sdiValue&        other,
             const unsigned int  i,
             const unsigned int  j)
{
    switch(other.GetCompoundType())
    {
    case COMPOUND_TYPE_ENTITY:
        p_pValuePtr = _CreateSubValuePtr<sdiValueEntity>(other, i, j);
        break;
    default:
        switch(other.GetBasicType())
        {
        case BASIC_TYPE_BOOL: p_pValuePtr   = _CreateSubValuePtr<bool>        (other, i, j); break;
        case BASIC_TYPE_INT: p_pValuePtr    = _CreateSubValuePtr<int>         (other, i, j); break;
        case BASIC_TYPE_UINT: p_pValuePtr   = _CreateSubValuePtr<unsigned int>(other, i, j); break;
        case BASIC_TYPE_DOUBLE: p_pValuePtr = _CreateSubValuePtr<double>      (other, i, j); break;
        }
    }
}


#define DESCRIPTOR_ZERO       0.000000000001

//
// Are two value float/double are same within tolerance or toleranceratio
//
template <class TYPE>
inline bool CompareValues(const TYPE& value1, const TYPE& value2, const TYPE tolerance, const bool useToleranceRatio)
{
    if (! useToleranceRatio)
    {
        if (fabs(value1 - value2) > tolerance)
        {
            return false;
        }
    }
    else
    {
        const TYPE average = (fabs(value1) + fabs(value2)) / (TYPE)2.0;

        if (average < (TYPE)DESCRIPTOR_ZERO)
        {
            return true;
        }

        if ((fabs(value1 - value2) / average) > tolerance)
        {
            return false;
        }
    }

    return true;
}
inline bool _EqualValues(const double&  value1, const double&  value2, const double doubleTolerance, const bool useToleranceRatio = false)
{
    const double tolerance = doubleTolerance > 0.0 ? doubleTolerance : DESCRIPTOR_ZERO;

    return CompareValues<double>(value1, value2, tolerance, useToleranceRatio);
}

inline bool _EqualValues(const sdiTriple&  value1, const sdiTriple&  value2, const double doubleTolerance, const bool useToleranceRatio = false)
{
    const double tolerance = doubleTolerance > 0.0 ? doubleTolerance : DESCRIPTOR_ZERO;

    for (unsigned int i = 0; i < 3; i++)
    {
        if (! CompareValues<double>(value1[i], value2[i], tolerance, useToleranceRatio))
        {
            return false;
        }
    }

    return true;
}

//
// Are float/double lists the same within a tolerance
//
static bool _EqualValues(const sdiDoubleList&  aValue1, const sdiDoubleList&   aValue2,  const double doubleTolerance, const bool useToleranceRatio)
{
    const double tolerance = doubleTolerance > 0.0 ? doubleTolerance : DESCRIPTOR_ZERO;

    if (aValue1.size() != aValue2.size())
    {
        return false;
    }
    for (size_t i = 0; i < aValue1.size(); i++)
    {
        if (! CompareValues<double>(aValue1[i], aValue2[i], tolerance, useToleranceRatio))
        {
            return false;
        }
    }

    return true;
}

static bool _EqualValues(const sdiTripleList&  aValue1, const sdiTripleList&   aValue2,  const double doubleTolerance, const bool useToleranceRatio)
{
    const double tolerance = doubleTolerance > 0.0 ? doubleTolerance : DESCRIPTOR_ZERO;

    if (aValue1.size() != aValue2.size())
    {
        return false;
    }
    for (size_t i = 0; i < aValue1.size(); i++)
    {
        if (! CompareValues<double>(aValue1[i].GetX(), aValue2[i].GetX(), tolerance, useToleranceRatio)
            || ! CompareValues<double>(aValue1[i].GetY(), aValue2[i].GetY(), tolerance, useToleranceRatio)
            || ! CompareValues<double>(aValue1[i].GetZ(), aValue2[i].GetZ(), tolerance, useToleranceRatio))
        {
            return false;
        }
    }

    return true;
}

static bool _EqualValues(const sdiDoubleList2& aaValue1, const sdiDoubleList2& aaValue2, const double doubleTolerance, const bool useToleranceRatio)
{
    const double tolerance = doubleTolerance > 0.0 ? doubleTolerance : DESCRIPTOR_ZERO;

    if (aaValue1.size() != aaValue2.size())
    {
        return false;
    }
    for (size_t i = 0; i < aaValue1.size(); i++)
    {
        if (aaValue1[i].size() != aaValue2[i].size())
        {
            return false;
        }
        for (size_t j = 0; j < aaValue1[i].size(); j++)
        {
            if (! CompareValues<double>(aaValue1[i][j], aaValue2[i][j], tolerance, useToleranceRatio))
            {
                return false;
            }
        }
    }

    return true;
}

template <class VALUE>
static bool _AreEqualValuesWithinTolerance(const sdiValue& value1, const sdiValue& value2, const double doubleTolerance, const bool useToleranceRatio)
{
    VALUE vA, vB;

    if (!value1.GetValue(vA))
    {
        return false;
    }
    if (!value2.GetValue(vB))
    {
        return false;
    }

    return _EqualValues(vA, vB, doubleTolerance, useToleranceRatio);
}



//! Comparison with other - return false if sdiBasicType, sdiCompoundType, etc. do not match.
bool sdiValue::AreEqualWithinTolerance(const sdiValue& value1, const sdiValue& value2, const double doubleTolerance, const bool useToleranceRatio)
{
    if (value1.GetBasicType()      != value2.GetBasicType()
        || value1.GetCompoundType()   != value2.GetCompoundType()
        || value1.GetArrayDimension() != value2.GetArrayDimension())
    {
        return false;
    }

    const sdiBasicType basicType = value1.GetBasicType();
    const sdiCompoundType compoundType = value1.GetCompoundType();
    const unsigned int arrayDimension = value1.GetArrayDimension();


    // Consider cases dealing with floating point values
    switch (arrayDimension)
    {
    case 0:
        switch (compoundType)
        {
        case COMPOUND_TYPE_SINGLE:
            switch (basicType)
            {
            case BASIC_TYPE_DOUBLE:
            {
                return _AreEqualValuesWithinTolerance<double>(value1, value2, doubleTolerance, useToleranceRatio);
            }
            }
            break;

        case COMPOUND_TYPE_TRIPLE:
            switch (basicType)
            {
            case BASIC_TYPE_DOUBLE:
            {
                return _AreEqualValuesWithinTolerance<sdiTriple>(value1, value2, doubleTolerance, useToleranceRatio);
            }
            }
            break;

        }
        break;

    case 1:
        switch (compoundType)
        {
        case COMPOUND_TYPE_SINGLE:
            switch (basicType)
            {
            case BASIC_TYPE_DOUBLE:
            {
                return _AreEqualValuesWithinTolerance<sdiDoubleList>(value1, value2, doubleTolerance, useToleranceRatio);
            }
            }
            break;

        case COMPOUND_TYPE_TRIPLE:
        {
            return _AreEqualValuesWithinTolerance<sdiTripleList>(value1, value2, doubleTolerance, useToleranceRatio);
        }
        }
        break;

    case 2:
        switch (compoundType)
        {
        case COMPOUND_TYPE_SINGLE:
            switch (basicType)
            {
            case BASIC_TYPE_DOUBLE:
            {
                return _AreEqualValuesWithinTolerance<sdiDoubleList2>(value1, value2, doubleTolerance, useToleranceRatio);
            }
            }
            break;
        }
        break;
    }

    return (value1 == value2);
}
