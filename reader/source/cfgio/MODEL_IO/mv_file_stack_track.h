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


#ifndef MV_FILE_STACK_TRACK_H
#define MV_FILE_STACK_TRACK_H
#include <UTILS/file_utils.h>
#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include "hcio.h"

#if defined _WIN32 || defined WIN32 
#pragma warning(disable:4251)    
#endif

class HCIO_DATA_DLL_API MvFilePosTrack_t {
 public:
	 MvFilePosTrack_t(){}
  inline MvFilePosTrack_t(const string &full_name,_HC_LONG cur_line, _HC_LONG cur_pos) : 
    myFullName(full_name), myLine(cur_line), myPos(cur_pos) {}

 public:
  inline const string  &getFullName() const { return myFullName; }
  inline _HC_LONG  getLine()      const { return myLine; }
  inline _HC_LONG           getPos()       const { return myPos; }

 public:
  inline void setLine(_HC_LONG cur_line) { myLine=cur_line; }
  inline void setPos(_HC_LONG cur_pos)            { myPos=cur_pos; }

 private:
  string        myFullName;
  _HC_LONG myLine;
  _HC_LONG          myPos;
};

typedef deque<MvFilePosTrack_t> MvFileStackTrack_t;    // Thought to be a track of a stack of files
typedef deque<MvFilePosTrack_t> MvFilePosTrackStack_t; // Thought to be a stack of MvFilePosTrack_t (CS#805#25_08_06)


#endif //MV_FILE_STACK_TRACK_H
