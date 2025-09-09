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

#include <dyna2rad/dyna2rad.h>
#include <dyna2rad/convertparameters.h>
#include <dyna2rad/convertutils.h>

using namespace sdiD2R;
using namespace std;
using namespace sdi;
using namespace sdiConvert;

void ConvertParameter::ConvertParameters()
{
    ConvertEntities();
}

void ConvertParameter::ConvertEntities()
{
    SelectionRead parametersSelect(p_lsdynaModel,srcCard);
    while (parametersSelect.Next())
    {
        const sdiString& keyword = parametersSelect->GetKeyword();
        bool isLocal = keyword.find("LOCAL") != keyword.npos;
        bool isExpression = keyword.find("EXPRESSION") != keyword.npos;

        // using HW data names here as we do not yet have the cfg file(s)
        unsigned int type = GetValue<unsigned int>(*parametersSelect, "type");

        sdiString radKeyword = "/PARAMETER";
        if(isLocal) radKeyword += "/LOCAL";
        else        radKeyword += "/GLOBAL";
        if     ( 0 == type) radKeyword += "/REAL";
        else if(10 == type) radKeyword += "/REAL_EXPR";
        else if( 1 == type) radKeyword += "/INTEGER";
        else if(11 == type) radKeyword += "/INT_EXPR";
        else if( 2 == type) radKeyword += "/TEXT";
        else continue; // shouldn't happen

        sdiString paramName = parametersSelect->GetName();

        HandleEdit paramHEdit;
        p_radiossModel->CreateEntity(paramHEdit, radKeyword, paramName);
        if (paramHEdit.IsValid())
        {
            EntityEdit paramEntEdit(p_radiossModel, paramHEdit);

            paramEntEdit.SetValue(sdiIdentifier("ParName"), sdiValue(paramName));

            if(isExpression)
            {
                sdiString dynExpr = GetValue<sdiString>(*parametersSelect, "EXPRESSION");

                // Dyna seems to be case-insensitive for referenced parameters, but Radioss is
                // case sensitive, so we better replace them by their actual names
                sdiString radExpr;
                size_t posTokenStart = 0, posTokenEnd = 0;
                bool isParameterized = false;
                while(posTokenStart < dynExpr.size())
                {
                    if(dynExpr[posTokenStart] == ' ')
                    { // don't copy blancs
                        ++posTokenStart;
                        continue;
                    }

                    posTokenEnd = dynExpr.find_first_of("+-*/ ()^|&!=<>%", posTokenStart);

                    if(posTokenEnd == posTokenStart)
                    { // it's a "separator", so just copy
                        radExpr += dynExpr[posTokenStart];
                        ++posTokenStart;
                        continue;
                    }

                    sdiString token = dynExpr.substr(posTokenStart, posTokenEnd - posTokenStart);

                    if(posTokenEnd != dynExpr.npos && dynExpr[posTokenEnd] == '(')
                    { 
                        radExpr += token;
                        posTokenStart = posTokenEnd;
                        continue;
                    }

                    double tokenValue = 0;
                    int nbRead = sscanf(token.c_str(), "%lg", &tokenValue);
                    if(nbRead == 1)
                    { // the token is a number, so just copy
                        radExpr += token;
                        posTokenStart = posTokenEnd;
                        continue;
                    }

                    // if we get here, the token should be a parameter
                    HandleEdit refedParamHEdit;
                    bool found = p_radiossModel->FindByName(destEntityType, token, refedParamHEdit, true);
                    if(found)
                    {
                        radExpr += refedParamHEdit.GetName(p_radiossModel);
                    }
                    else
                    {
                        // not sure what happened, so let's just copy the token
                        radExpr += token;
                    }
                    posTokenStart = posTokenEnd;
                }

                paramEntEdit.SetValue(sdiIdentifier("Expression"), sdiValue(radExpr));
            }

            sdiValue value;
            parametersSelect->GetValue(sdiIdentifier("VAL"), value);
            if(0 == type || 10 == type)
            {
                paramEntEdit.SetValue(sdiIdentifier("Rvalue"), value);
            }
            else if(1 == type || 11 == type)
            {
                paramEntEdit.SetValue(sdiIdentifier("Ivalue"), value);
            }
            else if(2 == type)
            {
                paramEntEdit.SetValue(sdiIdentifier("Text"), value);
            }

            SDIHandlReadList sourceParameters = { {parametersSelect->GetHandle()} };
            Convert::PushToConversionLog(std::make_pair(paramHEdit, sourceParameters));
        }
    }
}

