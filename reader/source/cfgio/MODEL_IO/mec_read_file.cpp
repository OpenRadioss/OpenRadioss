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

#include <UTILS/mv_cstring.h>
#include <UTILS/mv_cstdio.h>
#include <UTILS/file_utils.h> /* PM:0340:28/06/2006 */
#include <UTILS/memory_utils.h> /* PM:0340:28/06/2006 */
#include "mv_file_stack_track.h"

#include "mec_read_file.h"
int MECReadFile::fileNum=0;
/* --------- Constructors & destructor --------- */


MECReadFile::MECReadFile() :
  myFullName(NULL),
  myVersion(FF_UNKNOWN),
  myRelativeName(NULL),
  myCurrentPositionPtr(new CKeywordData(0L, 0L)),
  myPreviousPositionPtr(new CKeywordData(0L, 0L)),
  myParentPositionPtr(new CKeywordData(0L, 0L)),
  
  myDoCloseOnDestruction(false),
  myStatus(0),
  myIncudeParentIndex(-1),
  myCompnentIndex(-1),
  myIsFirstLineRead(false),
  myComponentIndex(-1)
{
  fileNum++;
}


MECReadFile::MECReadFile(const char *full_name) :
  myFullName(strdup(full_name)),
  myVersion(FF_UNKNOWN),

  

  myRelativeName(strdup(full_name)),

  myCurrentPositionPtr(new CKeywordData(0L, 0L)),

  myPreviousPositionPtr(new CKeywordData(0L, 0L)),

  myParentPositionPtr(new CKeywordData(0L, 0L)),

  

  myDoCloseOnDestruction(false), 
  myIncudeParentIndex(-1),
  myCompnentIndex(-1),
  myIsFirstLineRead(false)
{

    int an_acces = my_get_file_access("",full_name);
    if(an_acces == W_OK)
        myStatus = 1; // KEEP
    else
        myStatus = 2; // DATA_BASE

    myComponentIndex = -1;

    fileNum++;
}


MECReadFile::MECReadFile(const char *full_name, _HC_LONG currentLine) :
  myFullName(strdup(full_name)),
  myVersion(FF_UNKNOWN),

   

  myRelativeName(strdup(full_name)),

  myCurrentPositionPtr(new CKeywordData(currentLine, 0L)),

  myPreviousPositionPtr(new CKeywordData(0L, 0L)),

  myParentPositionPtr(new CKeywordData(0L, 0L)),

  

  myDoCloseOnDestruction(false),
  myIncudeParentIndex(-1),
  myCompnentIndex(-1),
  myIsFirstLineRead(false)
{

	   

	  
       int an_acces = my_get_file_access("",full_name); 
       if(an_acces == W_OK )
           myStatus = 1; // KEEP
       else
       myStatus = 2;    // DATA_BASE
        

	  myComponentIndex = -1;

	  
          
       fileNum++;   
}

MECReadFile::~MECReadFile() {
  myfree(myFullName);

  

  myfree(myRelativeName);

  if(myCurrentPositionPtr) delete myCurrentPositionPtr;

  if(myPreviousPositionPtr) delete myPreviousPositionPtr;

  if(myParentPositionPtr) delete myParentPositionPtr;

  fileNum--;

  
}


/* --------- File management --------- */

void  MECReadFile::SetRelativeName(const char* relative_name) {
	myfree(myRelativeName); 
	myRelativeName=strdup(relative_name);
} 


void MECReadFile::SetCurrentLine(_HC_LONG cur_line) {

   myCurrentPositionPtr->SetLine(cur_line);

}

/* --------- Parsing --------- */
bool MECReadFile::readBuffer(int nb_chars,char *buffer)

{
  myPreviousPositionPtr->SetLocation(GetCurrentLocation());
  myPreviousPositionPtr->SetLine(GetCurrentLine());

  bool a_ok=(gets(buffer, nb_chars)!=NULL);
  /*
   * When the line is too long :
   * the line is cut in 2
   * then the fgets will at the next operation get 
   * the second piece of line
   * that's why we do now a loop for continuing the fgets
   * as long as we didn't have the "\n" string (ie the end of line)
   * in the buffer
   */
  if ((a_ok == true)&& (strstr(buffer, "\n") == NULL) && !eof()  )
  {
     char buffer_tmp[103] ;
     do
     {
       a_ok = (gets(buffer_tmp, 100)!=NULL) ;
     } while ( (a_ok == true) && (strstr(buffer_tmp, "\n") == NULL));
  }
  if(a_ok)
  {
      myCurrentPositionPtr->IncreaseLine(1);
  }
  return a_ok;
}
