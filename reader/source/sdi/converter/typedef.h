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

#ifndef SDICONVERT_TYPEDEF_H
#define SDICONVERT_TYPEDEF_H


#include <sdiHandles.h>
#include <sdiModelView.h>
#include <sdiSelection.h>
#include <sdiHandles.h>

#include <map>

namespace sdiConvert
{
    typedef unsigned int EntityId;

    typedef std::map <double, double> ContainDoubleVsDouble;

    typedef std::map <sdiString, int> ContainStrVsInt;

    typedef std::map <int, sdiString> ContainIntVsStr;

    typedef std::map <unsigned int, sdiString> ContainUIntVsStr;

    typedef std::map <sdiString, double> ContainStrVsDouble;

    typedef std::map <sdiString, sdiString> ContainStrVsStr;

    typedef std::map <EntityId, sdi::HandleRead> ContainUintVsHandleRead;

    typedef sdiVector<sdi::ElementRead> SDIElemReadList;

    typedef sdiVector<sdi::EntityRead> SDIEntityReadList;

    typedef sdiVector<sdi::HandleRead> SDIHandlReadList;

    typedef std::map<sdiString, sdiUIntList> ContainStrVsUIntList;

    typedef std::map<sdi::HandleRead, SDIHandlReadList> LogQueryHandle;

    typedef std::map <EntityId, SDIEntityReadList> MapEntityIdToEntityReadList;

    typedef std::map <sdiString, std::map<sdiString, sdiDoubleList>> ContainStrVsDoubleList;

    typedef std::map <sdiString, std::map<sdiString, sdiIntList>> ContainStrVsIntList;
    
    typedef std::map <sdiTriple, sdiUIntList> ContainTripleVsEntityIdList;

    typedef std::map <sdiString, std::map<sdiString, int>> ContainStrVsMapStrVsInt;

    typedef std::map <unsigned int, sdiUIntList> ContainUIntVsUIntList;

    typedef std::map <unsigned int, unsigned int> ContainUIntVsUInt;

    typedef std::map <sdiString, unsigned int> ContainStrVsUInt;

}

#endif // !SDICONVERT_TYPEDEF_H
