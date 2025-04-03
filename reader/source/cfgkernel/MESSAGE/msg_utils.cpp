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

#include <UTILS/win32_utils.h>



#include <UTILS/mv_iostream.h> 


#include "msg_utils.h"
#include "msg_manager.h"

#ifdef _WIN32
#define mysnprintf _snprintf
#define myvsnprintf _vsnprintf
#else /* ! _WIN32 */
#define mysnprintf snprintf
#define myvsnprintf vsnprintf
#endif /* _WIN32 */

extern "C" void Print_Msg(type_message_e type, const char* format, ...) {
  va_list args;
  va_start(args,format);
  MvMsgManager_t::VDisplayMess(type,format,&args);
  va_end(args);
}


extern "C" void Print_Message(const char *msg) {
  MvMsgManager_t::DisplayMessage(msg);
}

extern "C" void Print_Error(const char *msg) {
  MvMsgManager_t::DisplayError(msg);
}

extern "C" void Print_Error_level(const char *msg,int level) {

  if(level<=0) {
    MvMsgManager_t::DisplayMessage(msg);
  } else if(level==1) {
    MvMsgManager_t::DisplayWarning(msg);
  } else if(level>=2) {
    MvMsgManager_t::DisplayError(msg);
  }

}

extern "C" void Print_Debug(const char *msg) {
  cout << msg;
}

extern "C" int Get_Nb_Msg(type_message_e type) {
  return MvMsgManager_t::GetNbMsg(type);
}

extern "C" void Reset_Msg_Type(type_message_e type) {
  MvMsgManager_t::ResetMsgCpt(type);
}

extern "C" void Reset_Msg(void) {
  MvMsgManager_t::ResetMsgCpt();
}




