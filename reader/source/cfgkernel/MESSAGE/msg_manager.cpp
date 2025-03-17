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


#include <UTILS/mv_cstdio.h>
#include <UTILS/mv_cstdarg.h>
#include <UTILS/mv_string.h>
#include <UTILS/mv_iostream.h>
#include <MESSAGE/msg_manager.h>
#include <MESSAGE/msg_utils.h>

extern "C" {
#include <General/general_external_functions.h>
}

#define MSG_SIZE 500

static bool MV_MSG_INIT=false;
static int  MV_MSG_CPT_ARRAY[CFG_INFO_IN_COLOR + 1]; /* PM:0319:24/05/2006 */


/* --------- MvMsgManager_t class --------- */

MvMsgManager_t::MvMsgManager_t() {
  if(!MV_MSG_INIT) {
    MV_MSG_INIT=true;
    ResetMsgCpt();
  }
}

void MvMsgManager_t::DisplayMessage(const string &msg) {
  DisplayMess(CFG_MESSAGE,msg);
}

void MvMsgManager_t::DisplayWarning(const string &msg) {
}

void MvMsgManager_t::DisplayError(const string &msg) {
}

void MvMsgManager_t::DisplayMess(type_message_e type,const string &msg) {
  MV_MSG_CPT_ARRAY[type]++;
  //
#ifdef TXT_MSG
  switch (type) {
  case CFG_MESSAGE: cout << msg << flush; break;
  case CFG_WARNING: cout << msg << flush; break;
  case CFG_ERROR:   cout << msg << flush; break;
  default:      break; 
  }
#endif //TXT_MSG
  //
  switch (type) {
  case CFG_MESSAGE: Print_Message(const_cast<char *>(msg.c_str()));       break;
  
  case CFG_WARNING: Print_Error_level(const_cast<char *>(msg.c_str()),1); break;
  case CFG_ERROR:   Print_Error_level(const_cast<char *>(msg.c_str()),2);         break;
  
  }
}

void MvMsgManager_t::DisplayMess(type_message_e type, const char *format, ...) {

  if (format == NULL) return ;
  va_list args;
  va_start(args,format);
  VDisplayMess(type,format,&args);
  va_end(args);
}

void MvMsgManager_t::VDisplayMess(type_message_e type, const char *format, va_list *arglist_p) {
  char buffer[MSG_SIZE];
  vsprintf(buffer,format,*arglist_p);
  DisplayMess(type,string(buffer));
}

int  MvMsgManager_t::GetNbMsg(type_message_e type) {
  return MV_MSG_CPT_ARRAY[type];
}

void MvMsgManager_t::ResetMsgCpt(type_message_e type) { 
  MV_MSG_CPT_ARRAY[type]=0; 
}

void MvMsgManager_t::ResetMsgCpt() { 
  ResetMsgCpt(CFG_MESSAGE);
  ResetMsgCpt(CFG_WARNING);
  ResetMsgCpt(CFG_ERROR);
}

MsgState_t MvMsgManager_t::GetMsgState() {
  return MsgState_t(GetNbMsg(CFG_ERROR),GetNbMsg(CFG_WARNING),GetNbMsg(CFG_MESSAGE));
}



