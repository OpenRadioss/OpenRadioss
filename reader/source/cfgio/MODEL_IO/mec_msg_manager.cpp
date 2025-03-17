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

#include <UTILS/mv_stl_various.h>
#include <UTILS/mv_cstdarg.h>
#include <UTILS/memory_utils.h>
#include <MESSAGE/mv_messages.h>
#include <MESSAGE/msg_manager.h>

#include "mec_msg_manager.h"


/* --------- Constructors & destructor --------- */

MECMsgManager::MECMsgManager() :
  myNbErrors(MvMsgManager_t::GetNbMsg(CFG_ERROR)),
  myNbWarnings(MvMsgManager_t::GetNbMsg(CFG_WARNING)),
  myNbMessages(MvMsgManager_t::GetNbMsg(CFG_MESSAGE))
{
}

MECMsgManager::~MECMsgManager()
{
}

/* --------- Messages --------- */

void MECMsgManager::displayMessage(MyMsgType_e msg_type,const char *format,...) const {
  va_list a_args;
  va_start(a_args,format);
/*
  switch(msg_type) {
  case MSG_MESSAGE: MvMsgManager_t::storeMessage(MESSAGE, format, &a_args); break;
  case MSG_WARNING: MvMsgManager_t::storeMessage(WARNING,format,&a_args); break;
  case MSG_ERROR:   MvMsgManager_t::storeMessage(ERROR,format,&a_args);   break;
  default:         break;
  }
 */ 
  va_end(a_args);
}

void MECMsgManager::displayMessage(MyMsgType_e msg_type,unsigned int id,...) const
{
    const char *format = MV_get_msg_array(MSGT_GLOBAL)[id];

    va_list a_args;
    va_start(a_args,id);
    char buffer[512];
    vsprintf(buffer,format,a_args);
    va_end(a_args);

    displayMessage(msg_type, buffer);
}

bool MECMsgManager::isMessageDisplayed(MyMsgType_e msg_type) const {
  int a_nb_msgs_ini=0,a_nb_msgs=0;
  //
  switch(msg_type) {
  case MSG_MESSAGE: a_nb_msgs_ini=myNbMessages; a_nb_msgs=MvMsgManager_t::GetNbMsg(CFG_MESSAGE); break;
  case MSG_WARNING: a_nb_msgs_ini=myNbWarnings; a_nb_msgs=MvMsgManager_t::GetNbMsg(CFG_WARNING); break;
  case MSG_ERROR:   a_nb_msgs_ini=myNbErrors;   a_nb_msgs=MvMsgManager_t::GetNbMsg(CFG_ERROR); break;
  default:         break;
  }
  //
  return a_nb_msgs>a_nb_msgs_ini;
}
