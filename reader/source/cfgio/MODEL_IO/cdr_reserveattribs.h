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
#ifndef CDR_RESERVEATTRIBS_H
#define CDR_RESERVEATTRIBS_H

#include <stdio.h>
#include <string.h>

namespace cdr
{
    const std::string g_AttribParamName =            "_PARAM_NAME";
    const std::string g_AttribParamValueInteger =    "_PARAM_VALUE_INT";
    const std::string g_AttribParamValueDouble =     "_PARAM_VALUE_DOUBLE";
    const std::string g_AttribParamValueString =     "_PARAM_VALUE_STRING";
    const std::string g_AttribParamScope =           "_PARAM_SCOPE";
    const std::string g_AttribParamType =            "_PARAM_TYPE";
    const std::string g_AttribFileName =             "_FILENAME";
    const std::string g_AttribFullFileName =         "_FULLFILENAME";
    const std::string g_AttribFormatType =           "_FORMATTYPE";
    const std::string g_AttribFileVersion =          "_FILEVERSION";
    const std::string g_AttribSplitArrayToSingle =   "_SPLIT_ARRAY_TO_SINGLE";
};


#endif 
