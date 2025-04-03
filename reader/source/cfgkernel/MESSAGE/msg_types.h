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
#ifndef MSG_TYPES_H
#define MSG_TYPES_H


#if defined WIN32 || defined _WIN32 
#undef ERROR 
#endif



    enum type_message_s { 
      /** */ CFG_MESSAGE  = 0, 
      /** */ CFG_WARNING  = 1,
      /** */ CFG_ZL_ERROR = 2,
       CFG_ERROR    = 3, 
      /** */ CFG_FATAL    = 4,
      /** */ CFG_RIP      = 5,
      /** */ CFG_INFO_IN_COLOR      = 6
    };


/** Message types */
typedef enum type_message_s type_message_e;

#endif /* MSG_TYPES_H */




