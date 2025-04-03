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



#include <UTILS/error.h>
#include <UTILS/file_utils.h>
#include <UTILS/mv_cstring.h>


#include "mv_read_base.h"
 
// @@ MPOST R.A 18/10/2004 MP_DEV_2004_83: Zipp Param File
MvReadBase_t::MvReadBase_t(const string &fullname,bool is_gzipped, bool use_local_msg) :
  myFile(is_gzipped? NULL: myfopen(fullname.c_str(),"rt")),
#ifndef NO_GZ_LIB
  myGzFile(is_gzipped? gzopen(fullname.c_str(),"rb"): NULL), 
#endif
  myFileName(fullname),
  myCurrentLine(1),
  myBufferSize(0),
  myBufferPos(0),
  myUseLocalMsg(use_local_msg)
{
  //cerr << "MvReadBase_t -> OPENING " << myFileName << " (" << myFile << ")" << endl;
  //
	  // @@ MPOST R.A 18/10/2004 MP_DEV_2004_83: Zipp Param File
#ifndef NO_GZ_LIB
	  if((myGzFile==NULL)&&(myFile==NULL)) {
#else
		  if(myFile==NULL) {
#endif
			  if(useLocalMsg()) {
				  MvError_t a_error("ERROR: Not possible to open \"%s\"",getFileName().c_str());
				  throw a_error;
			  } else {
				  throw MvError_t(getMsg(0),getFileName().c_str());
			  }
		  }
	    // @@ MPOST R.A 18/10/2004 MP_DEV_2004_83: Zipp Param File (END)
  myBuffer[0]='\0';
}

MvReadBase_t::~MvReadBase_t() {
  if(myFile!=NULL) {
    myfclose(myFile);
    //
    //cerr << "MvReadBase_t -> CLOSING " << myFileName << " (" << myFile << ")" << endl;
  }
// @@ MPOST R.A 18/10/2004 MP_DEV_2004_83: Zipp Param File
#ifndef NO_GZ_LIB
  if(myGzFile!=NULL)
	  gzclose(myGzFile);
#endif
}

bool MvReadBase_t::eof() const
{
	if(myFile!=NULL)
		return (feof(myFile) && eob());
#ifndef NO_GZ_LIB
	else  return (gzeof(myGzFile) && eob());
#endif
	return false;
}


void MvReadBase_t::readBuffer() {
  
  bool a_is_new_line=(myBufferSize<=0 ? false : myBuffer[myBufferSize-1]=='\n');
  
  myBuffer[0]='\0'; // Don't touch this
// @@ MPOST R.A 18/10/2004 MP_DEV_2004_83: Zipp Param File
  if(myFile!=NULL) {
	  if(!feof(myFile)) {
		  if(a_is_new_line) ++myCurrentLine;
		  fgets(myBuffer,BUFFER_SIZE,myFile);
		  myBufferSize=(int)(strlen(myBuffer));
		  myBufferPos=0;
	  } else {
		  if(useLocalMsg()) {
			  throw MvError_t("In file \"%s\", at line %d:\nERROR: End of file",
				  getFileName().c_str(),getCurrentLine());
		  } else {
			  throw MvError_t(getMsg(1),getFileName().c_str(),getCurrentLine());
		  }
	  }
  }
#ifndef NO_GZ_LIB 
  else {
	  if(!gzeof(myGzFile)) {
		  if(a_is_new_line) ++myCurrentLine;
 
		gzgets(myGzFile, myBuffer,BUFFER_SIZE);
 
		myBufferSize=(int)(strlen(myBuffer));
		  myBufferPos=0;
	  } else {
		  if(useLocalMsg()) {
			  throw MvError_t("In file \"%s\", at line %d:\nERROR: End of file",
				  getFileName().c_str(),getCurrentLine());
		  } else {
			  throw MvError_t(getMsg(1),getFileName().c_str(),getCurrentLine());
		  }
	  }
  }
#endif 
  // @@ MPOST R.A 18/10/2004 MP_DEV_2004_83: Zipp Param File (END)
}

char MvReadBase_t::readChar() {
  if(eob()) readBuffer();
  return myBuffer[myBufferPos++];
}

void MvReadBase_t::gotoNextLine() {
  long a_line=getCurrentLine();
  try {
    do readBuffer(); while(a_line==getCurrentLine());
  } catch(MvError_t &a_error) {
    if(!feof(myFile)) throw a_error;
  }
}




