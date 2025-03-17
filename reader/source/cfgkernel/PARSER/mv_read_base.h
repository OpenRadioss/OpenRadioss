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


#ifndef MV_READ_BASE_H
#define MV_READ_BASE_H

#include <stdio.h>

#include <UTILS/mv_string.h>
#include <MESSAGE/msg_manager.h> 
#include <MESSAGE/mv_messages.h> 


#ifndef NO_GZ_LIB
#include <zlib.h>
#endif
#define BUFFER_SIZE 200


class MvReadBase_t : public MvMsgManager_t {
public:    // Constructor and destructor
  MvReadBase_t(const string &fullname,bool is_gzipped=false, bool use_local_msg=false);
  virtual ~MvReadBase_t();
protected: // Accessors
  inline FILE         *getFile()        const { return myFile; }
  inline const string &getFileName()    const { return myFileName; }
  inline long          getCurrentLine() const { return myCurrentLine; }
  inline long          getCurrentPos()  const { return ftell(myFile); }
protected: // Low level functions
  inline bool eob() const { return (myBufferPos>=myBufferSize); }
  inline bool eol() const { return (eob() && myBuffer[myBufferPos-1]=='\n'); }
 // @@ MPOST R.A 18/10/2004 MP_DEV_2004_83: Zipp Param File
  bool eof() const;

  void readBuffer();
  virtual char readChar();
  inline void unreadChar() { --myBufferPos; }
  void gotoNextLine();
protected: // Messages
  inline bool        useLocalMsg() const { return myUseLocalMsg; }
  inline const char *getMsg(int i) const { return MV_get_msg_array(MSGT_PARSER)[i]; } 
protected:
  FILE   *myFile;
  // @@ MPOST R.A 18/10/2004 MP_DEV_2004_83: Zipp Param File
#ifndef NO_GZ_LIB
 gzFile myGzFile; 
#endif
  string  myFileName;
  long    myCurrentLine;
  int     myBufferSize,myBufferPos;
  char    myBuffer[BUFFER_SIZE];
  bool    myUseLocalMsg;
};

#endif //MV_READ_BASE_H




