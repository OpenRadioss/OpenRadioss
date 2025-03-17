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

#include <convertRuleParser.h>

using namespace std;
using namespace sdi;


ConvertRuleParser::ConvertRuleParser(const string &fullname) :
    MvParserBase_t(fullname, false, false)
{
}

void ConvertRuleParser::fillMap(convertRuleMap &m)
{
    sdiString readString;
    sdiString previousString;
    char readChar = '\0';
    char previousChar = '\0';
    bool is_option = false;
    sdiString sourceOption;
    sdiString targetOption;
    convertRule currentRule;
    
    while (!seof(false)) {
        if(readString.size() != 0) previousString = readString;
        previousChar = readChar;
        readString = getNextString();
        readChar = getNextChar();

        sdiString string1;
        sdiString string2;

        if(readChar == '{') {
            is_option = true;
            sourceOption = readString;
            targetOption = previousString;
            convertRule newrule;
            newrule.setTargetKeyword(previousString);
            currentRule = newrule;
        }

        else if(readChar == '}') {
            m.setRule(sourceOption, currentRule);
            is_option = false;
        }
        
        else if(readChar == ';' && !is_option) {
            sourceOption = readString;
            targetOption = previousString;
            convertRule newrule;
            newrule.setTargetKeyword(previousString);
            currentRule = newrule;
            m.setRule(sourceOption, currentRule);
        }

        else if(readChar == ';' && previousChar == '=' && is_option){   
            currentRule.setTargetAttribute(readString,previousString);
        }   
    }
    return;
}

