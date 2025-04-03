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
#include <dynamain.h>
#include <map>
#include <dll_settings.h>
#include <algorithm>

using namespace sdi;
using namespace std;

extern "C" 
{

CDECL void cpp_get_message_dyna_(int *I, int *MSG_ID, int *MSG_TYPE, char *MSG_DESCR, char *FILE_NAME, int *LINE_NB, char *MSG_BLOCK, char *MSG_LINE)
{
    
    // Get reader message
    const hwReaderMessageList& messages = DynakeyGetMessages();
    hwReaderMessageList::const_iterator it=messages.begin() ;
    std::advance(it, *I-1);
    const hwReaderMessage &message = *it;
    *MSG_ID=message.GetId();
    if(message.GetId() != 0)
    {
        *MSG_TYPE = message.GetType();
        std::string strMsgDescr(message.GetDescription().c_str());
        std::replace( strMsgDescr.begin(), strMsgDescr.end(), '\n', ' ');
        strcpy(MSG_DESCR,strMsgDescr.c_str());
        strcpy(FILE_NAME,message.GetFilename().c_str());
        *LINE_NB = message.GetLinenumber();
        strcpy(MSG_BLOCK,message.GetBlock().c_str());
        strcpy(MSG_LINE,message.GetLine().c_str());
/*
        printf("%s\n", message.GetTitle().c_str());
        printf("line %u in file %s\n",
                   message.GetLinenumber(), message.GetFilename().c_str());
        printf("DESCRIPTION :\n");
        printf("%s\n", message.GetDescription().c_str());
        if(!message.GetSolution().empty())
        {
            printf("%s\n", message.GetSolution().c_str());
        }
*/
    }


    // ... messages without id
/*
    if(message.GetId() == 0)
    {
        printf("Message type %d: ", message.GetType());
        printf("%s\n", message.GetDescription().c_str());
        if(!message.GetSolution().empty())
        {
            printf("%s\n", message.GetSolution().c_str());
        }
    }
*/
}

CDECL void CPP_GET_MESSAGE_DYNA(int *I, int *MSG_ID, int *MSG_TYPE, char *MSG_DESCR, char *FILE_NAME, int *LINE_NB, char *MSG_BLOCK, char *MSG_LINE)
{cpp_get_message_dyna_ (I,MSG_ID,MSG_TYPE,MSG_DESCR,FILE_NAME,LINE_NB,MSG_BLOCK,MSG_LINE);}

CDECL void cpp_get_message_dyna__ (int *I, int *MSG_ID, int *MSG_TYPE, char *MSG_DESCR, char *FILE_NAME, int *LINE_NB, char *MSG_BLOCK, char *MSG_LINE)
{cpp_get_message_dyna_ (I,MSG_ID,MSG_TYPE,MSG_DESCR,FILE_NAME,LINE_NB,MSG_BLOCK,MSG_LINE);}

CDECL void cpp_get_message_dyna (int *I, int *MSG_ID, int *MSG_TYPE, char *MSG_DESCR, char *FILE_NAME, int *LINE_NB, char *MSG_BLOCK, char *MSG_LINE)
{cpp_get_message_dyna_ (I,MSG_ID,MSG_TYPE,MSG_DESCR,FILE_NAME,LINE_NB,MSG_BLOCK,MSG_LINE);}



}
