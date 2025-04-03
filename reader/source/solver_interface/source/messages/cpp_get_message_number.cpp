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

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <radiossblk.h>
#include <map>
#include <dll_settings.h>

using namespace std;

extern "C" 
{

void cpp_get_message_number_(int *NBMESSAGES)
{
    const hwReaderMessageList& messages = RadiossblkGetMessages();
    for(hwReaderMessageList::const_iterator it=messages.begin(); it!=messages.end(); it++)
    {
        const hwReaderMessage &message = *it;
        *NBMESSAGES = *NBMESSAGES + 1;
    }
}


CDECL void CPP_GET_MESSAGE_NUMBER(int *NBMESSAGES)
{cpp_get_message_number_ (NBMESSAGES);}

CDECL void cpp_get_message_number__ (int *NBMESSAGES)
{cpp_get_message_number_ (NBMESSAGES);}

CDECL void cpp_get_message_number (int *NBMESSAGES)
{cpp_get_message_number_ (NBMESSAGES);}



}
