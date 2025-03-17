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
#ifndef MSG_MANAGER_H
#define MSG_MANAGER_H

#include <stdarg.h>


#include <UTILS/mv_string.h> 


#include "msg_types.h"
#include "msg_state.h"
#include <UTILS/mv_stl_various.h>
#include <UTILS/mv_string.h>
#include <HCDI/hcdi.h>

#if defined _WIN32 || defined WIN32 
#pragma warning(disable:4251)    
#endif 

/// Message manager class
class HC_DATA_DLL_API MvMsgManager_t {
 public: /** @name Constructors & destructor */
  //@{
  /// Constructor
  MvMsgManager_t();
  //@}
  
 public: /** @name Messages, errors and warnings */
  //@{
  /// Messages, errors and warnings
  static void DisplayMessage(const string &msg);
  /// Warnings

  static void DisplayWarning(const string &msg);
  /// Errors
  static void DisplayError(const string &msg);
  /** Messages, errors and warnings (with argument list).<br>
      @see type_message_e
  */
  static void DisplayMess(type_message_e type,const string &msg);
  /** Messages, errors and warnings (with argument list).<br>
      @see type_message_e
  */
  static void DisplayMess(type_message_e type, const char* format, ...);
  /** Messages, errors and warnings (with argument list).<br>
      @see type_message_e
  */
  static void VDisplayMess(type_message_e type, const char *format, va_list *arglist_p);
  //@}

 public: /* @name Message counters */
  //@{
  /// Gets the number of messages since the last reset
  static int  GetNbMsg(type_message_e type);
  /// Resets a message counter
  static void ResetMsgCpt(type_message_e type);
  /// Resets all message counters
  static void ResetMsgCpt();
  /// Gets the number of messages of each type
  static MsgState_t GetMsgState();
  //@}
};


#endif /* MSG_MANAGER_H */




