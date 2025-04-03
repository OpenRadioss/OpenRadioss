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




////////////////////////////////////////////////////////////////////

#include <convertutilsbase.h>
#include <sstream>

using namespace std;
using namespace sdi;

namespace sdiConvert
{

// should go to convert.cxx
bool Convert::p_doConvertParameters = false;

std::map<sdiString, sdiString> ConvertUtilsBase::p_expressionParameters = {};

void ConvertUtilsBase::GetElemKeywords(const EntityRead& enityRead, sdiStringList& keyWordList) const
{

    SelectionElementRead elemSelect(enityRead);
    keyWordList.reserve(elemSelect.Count());
    while (elemSelect.Next())
    {
        sdiString keyword = elemSelect->GetKeyword();
        keyWordList.push_back(keyword);
    }
    std::sort(keyWordList.begin(), keyWordList.end());
    auto eraseItr = std::unique(keyWordList.begin(), keyWordList.end());
    keyWordList.erase(eraseItr, keyWordList.end());
}

void ConvertUtilsBase::GetEntityHandles(const EntityRead& enityRead, const vector<sdiString>& attribNames, vector<reference_wrapper<HandleRead>>& handleReadList)
{
    std::size_t size = attribNames.size();
    if (attribNames.size() != handleReadList.size())
        return;
    for (std::size_t i = 0; i < size; ++i)
        enityRead.GetEntityHandle(sdiIdentifier(attribNames[i]), handleReadList[i]);
}

sdiStringList ConvertUtilsBase::SplitString(const sdiString& strToSplit, const sdiString& delim)
{
    sdiStringList tokens;
    size_t prev = 0, pos = 0;
    do
    {
        pos = strToSplit.find(delim, prev);
        if (pos == string::npos) pos = strToSplit.length();
        string token = strToSplit.substr(prev, pos - prev);
        if (!token.empty()) tokens.push_back(token);
        prev = pos + delim.length();
    } while (pos < strToSplit.length() && prev < strToSplit.length());
    return tokens;
}

// Removes ids in idList1 which are also present in idList2.
// NB: Both lists have to be sorted!
// First while loop copied from
// www.cplusplus.com/reference/algorithm/set_difference
void ConvertUtilsBase::IdListRemove(sdiUIntList& idList1,const sdiUIntList& idList2)
{
    sdiUIntList::iterator first1 = idList1.begin(), last1 = idList1.end(), result = idList1.begin();
    sdiUIntList::const_iterator first2 = idList2.begin(), last2 = idList2.end();
    while(first1!=last1 && first2!=last2)
    {
        if(*first1<*first2) { *result = *first1; ++result; ++first1; }
        else if(*first2<*first1) ++first2;
        else { ++first1; ++first2; }
    }
    while(first1!=last1) { *result = *first1; ++result; ++first1; }
    idList1.resize(result - idList1.begin());
}

bool ConvertUtilsBase::CopyValue(
    const sdi::EntityRead& srcEntity, const sdi::EntityEdit& destEntity,
    const sdiString& srcDataname,        const sdiString& destDataname,
    unsigned int srcRow,                unsigned int destRow,
    unsigned int srcColumn,             unsigned int destColumn)
{
    bool isOk = true;
    sdiIdentifier srcIdentifier(srcDataname, 0, srcRow, srcColumn);
    bool isParameterized = false;

    if(Convert::p_doConvertParameters)
    {
        isParameterized = srcEntity.IsParameterized(srcIdentifier);
    }
    if(isParameterized)
    {
        bool isNegated = false;
        sdiString paramName = srcEntity.GetParameterName(srcIdentifier, &isNegated);
        // Dyna seems to be case-insensitive for referenced parameters, but Radioss is
        // case sensitive, so we better replace them by their actual names
        HandleEdit paramHEdit;
        p_radiossModel->FindByName(p_radiossParameterType, paramName, paramHEdit, true);
        if(paramHEdit.IsValid()) paramName = paramHEdit.GetName(p_radiossModel);
        isOk = destEntity.SetParameter(
            sdiIdentifier(destDataname, 0, destRow, destColumn),
            paramName, isNegated);
    }
    else
    {
        sdiValue value;
        srcEntity.GetValue(srcIdentifier, value);
        isOk = destEntity.SetValue(
            sdiIdentifier(destDataname, 0, destRow, destColumn),
            sdiValue(value));
    }

    return isOk;
}

bool ConvertUtilsBase::SetExpressionValue(
    const sdi::EntityRead& srcEntity, const sdi::EntityEdit& destEntity,
    const sdiString& srcExpression,      const sdiString& destDataname,
    bool isReal,
    unsigned int srcRow,                unsigned int destRow,
    unsigned int srcColumn,             unsigned int destColumn)
{
    // from srcExpression, we build
    sdiString numExpression; // an expression with the numerical values and
    sdiString paramExpression; // an expression with the parameter names
    size_t posTokenStart = 0, posTokenEnd = 0;
    bool isParameterized = false;
    while(posTokenStart < srcExpression.size())
    {
        posTokenEnd = srcExpression.find_first_of("+-*/ ()^|&!=<>%", posTokenStart);

        if(posTokenEnd == posTokenStart)
        { // it's a "separator", so just copy
            numExpression += srcExpression[posTokenStart];
            paramExpression += srcExpression[posTokenStart];
            ++posTokenStart;
            continue;
        }

        sdiString token = srcExpression.substr(posTokenStart, posTokenEnd - posTokenStart);

        if(posTokenEnd != srcExpression.npos && srcExpression[posTokenEnd] == '(')
        { 
            numExpression += token;
            paramExpression += token;
            posTokenStart = posTokenEnd;
            continue;
        }

        double tokenValue = 0;
        int nbRead = sscanf(token.c_str(), "%lg", &tokenValue);
        if(nbRead == 1)
        { // the token is a number, so just copy
            numExpression += token;
            paramExpression += token;
            posTokenStart = posTokenEnd;
            continue;
        }

        // if we get here, the token should be an attribute name
        sdiValue value;
        sdiIdentifier tokenIdentifier(token);
        srcEntity.GetValue(tokenIdentifier, value);
        sdiString valueStr;
        value.GetValueAsString(valueStr);
        if(valueStr.size() == 0)
        {
            valueStr = "0";
        }
        /* not necessary, unary - precedes other operators
        else if(valueStr[0] == '-')
        { // enclose negative value in parantheses
            valueStr.insert(0, "(");
            valueStr += ")";
        }
        */
        numExpression += valueStr;
        if(srcEntity.IsParameterized(tokenIdentifier))
        {
            isParameterized = true;
            // Dyna seems to be case-insensitive for referenced parameters, but Radioss is
            // case sensitive, so we better replace them by their actual names
            sdiString paramName = srcEntity.GetParameterName(tokenIdentifier);
            HandleEdit paramHEdit;
            p_radiossModel->FindByName(p_radiossParameterType, paramName, paramHEdit, true);
            if(paramHEdit.IsValid()) paramName = paramHEdit.GetName(p_radiossModel);
            paramExpression += paramName;
        }
        else
        {
            paramExpression += valueStr;
        }
        posTokenStart = posTokenEnd;
    }

    if(p_expressionParameters.count(paramExpression) == 0)
    {
        // Evaluate expression TBD
        double expressionValue = 0;

        // if no parameter needed, set value and return
        if( !isParameterized || // none of the values is parameterized
            !Convert::p_doConvertParameters) // legacy: no parameters desired
        {
            sdiValue value(expressionValue);
            if(!isReal) value = sdiValue((int) expressionValue);
            return destEntity.SetValue(
                sdiIdentifier(destDataname, 0, destRow, destColumn), value);
        }

        // Find an unused name: destDataname with a number suffix if needed
        sdiString paramName = destDataname;
        if(paramName.size() > 9) paramName.resize(9);
        HandleEdit paramHEdit;
        if(p_radiossModel->FindByName(p_radiossParameterType, paramName, paramHEdit))
        {
            int i = 0;
            do
            {
                ++i;
                char buffer[11];
                sprintf(buffer, "%d", i);
                if(destDataname.size() + strlen(buffer) <= 9)
                {
                    paramName = destDataname + buffer;
                }
                else
                {
                    paramName = destDataname.substr(0, 9 - strlen(buffer)) + buffer;
                }
            } while(p_radiossModel->FindByName(p_radiossParameterType, paramName, paramHEdit));
        }

        // Create and populate parameter
        sdiString keyword("/PARAMETER/GLOBAL/REAL_EXPR");
        if(!isReal) keyword = "/PARAMETER/GLOBAL/INT_EXPR";
        p_radiossModel->CreateEntity(paramHEdit, keyword, paramName);
        EntityEdit paramEntEdit(p_radiossModel, paramHEdit);
        paramEntEdit.SetValue(sdiIdentifier("ParName"), sdiValue(paramName));
        paramEntEdit.SetValue(sdiIdentifier("Expression"), sdiValue(paramExpression));
        if(isReal) paramEntEdit.SetValue(sdiIdentifier("Rvalue"), sdiValue(expressionValue));
        else       paramEntEdit.SetValue(sdiIdentifier("Ivalue"), sdiValue((int) expressionValue));

        // Add new parameter to look-up map (for usage right now and potentially later)
        p_expressionParameters[paramExpression] = paramName;
    }

    destEntity.SetParameter(
        sdiIdentifier(destDataname, 0, destRow, destColumn),
        p_expressionParameters[paramExpression]);

    return true;
}

} // namespace sdiConvert
