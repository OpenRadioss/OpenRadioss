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
#ifndef MECI_PARAMETER_H
#define MECI_PARAMETER_H
#include "hcio.h"

class IParameter
{
public:
    //!Three types of parameters are supported: Integer, Double and String
    enum Type
    {
        TYPE_DOUBLE,
        TYPE_INTEGER,
        TYPE_STRING,
        TYPE_DOUBLE_EXPRESSION = 10,
        TYPE_INTEGER_EXPRESSION = 11,
        TYPE_UNKNOWN
    };

    //!enum to know the type of the parameter keyword
    enum Keywordtype
    {
        REGULAR,
        EXPRESSION,
        DUPLICATION,
        LOCAL
    };

    IParameter() {}
    virtual ~IParameter() { }
    virtual int GetFileIndex() const = 0;
    virtual int GetIntValue() const = 0;
    virtual double GetDoubleValue() const = 0;
    virtual std::string GetStringValue() const = 0;
    virtual std::string GetName() const = 0;
    virtual Type GetType() const = 0;
    virtual Keywordtype GetKeywordType() const = 0;
};


#endif //MECI_PARAMETER_H
