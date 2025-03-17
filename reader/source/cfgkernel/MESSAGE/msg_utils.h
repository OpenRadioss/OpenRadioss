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
#ifndef MSG_UTILS_H
#define MSG_UTILS_H

#include "msg_types.h"

#ifdef __cplusplus
extern "C" {
#endif

  void Print_Msg(type_message_e type, const char* format, ...);


  void Print_Message(const char *msg);
  void Print_Error(const char *msg);
  void Print_Error_level(const char *msg,int level);
  void Print_Debug(const char *msg);
  int  Get_Nb_Msg(type_message_e type);
  void Reset_Msg_Type(type_message_e type);
  void Reset_Msg(void);

#ifdef __cplusplus
}
#endif

#endif /* MSG_UTILS_H */




