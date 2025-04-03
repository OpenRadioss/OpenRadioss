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

#include <UTILS/mv_cstdarg.h>
#include <UTILS/mv_cstring.h>
#include <UTILS/mv_stl_various.h>
#include <UTILS/memory_utils.h>
#include <UTILS/file_utils.h>
#include <MESSAGE/mv_messages.h>

#include "mec_single_file_writer.h"
#include "hcioi_utils.h"
/* --------- Constructors & destructor --------- */

MECSingleFileWriter::MECSingleFileWriter (FILE *filePtr, int line_nb_chars, int buffer_nb_chars) :
    MECIWriteContext (),
    myFilePtr (filePtr),
    myDoCloseOnDestruction(false),
    myLineNbChars(line_nb_chars)
{
    if (buffer_nb_chars < myLineNbChars + 2)
        myBufferNbChars = myLineNbChars + 2;
    else
        myBufferNbChars = buffer_nb_chars;
    myLineBuffer = (char *)mymalloc(myBufferNbChars*sizeof(char));
    myCurBufferPos = myLineBuffer;
}

MECSingleFileWriter::~MECSingleFileWriter ()
{
    if (myDoCloseOnDestruction && (NULL != myFilePtr))
    {
        fclose(myFilePtr);
        myFilePtr = NULL; 
    }
    if (NULL != myLineBuffer)
    {
        myfree(myLineBuffer);
        myLineBuffer = NULL; 
    }
}

/* --------- Formatted writing methods --------- */

void MECSingleFileWriter::WriteFile(const char *format,...) {
  FILE *a_file_p=myFilePtr;
  //
  va_list a_args;
  va_start(a_args,format);
  //
  size_t remainder = myBufferNbChars - (myCurBufferPos - myLineBuffer) / sizeof(char);
  int necessary = vsnprintf(myCurBufferPos, remainder,format,a_args);
  // if a trailing '\n' doesn't fit, we force it
  if(necessary >= remainder && strlen(format) > 0 && format[strlen(format) - 1] == '\n')
  {
      myLineBuffer[myBufferNbChars - 2] = '\n';
      myCurBufferPos = myLineBuffer + myBufferNbChars - 2;
  }
  char *a_char_p=myCurBufferPos;
  while(*a_char_p!='\0') {
    if(*a_char_p=='\n') {
      *a_char_p='\0';
      //
      fprintf(a_file_p,"%s\n",myLineBuffer);
      //
      ++a_char_p;
      memmove(myLineBuffer,a_char_p,strlen(a_char_p)+1);
      a_char_p=myCurBufferPos=myLineBuffer;      
    } else {
      ++a_char_p;
    }
  }
  myCurBufferPos=a_char_p;
  //
  va_end(a_args);
}
void MECSingleFileWriter::RemoveBlankEnd()
{
    killBlanksEnd(myLineBuffer);
}

