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

#include <string.h>

#include <UTILS/memory_utils.h>
#include <UTILS/str_utils.h>

#include "meci_read_context.h"

/* --------- Constructors & destructor --------- */

MECIReadContext::MECIReadContext (int line_nb_chars, int buffer_nb_chars) :
    MECMsgManager ()
{
    myLineNbChars = line_nb_chars;
    if (buffer_nb_chars > line_nb_chars + 2) myBufferNbChars = buffer_nb_chars;
    else                                     myBufferNbChars = myLineNbChars + 2; // +2 for "\r\n" ???
    myLineBuffer = (char*) mymalloc ((myBufferNbChars + 1) * sizeof(char));
}

MECIReadContext::~MECIReadContext ()
{
    myfree (myLineBuffer);
}


//Beware char are between -127 and +128, ASCII code
// between 0 and 256, then if we want to compare the char to ascii
// vale we need to use unsignbed char
void MECIReadContext::killNLEnd(char *buffer) const 
{
  unsigned char *a_char_p=(unsigned char *)strrchr(buffer,'\n');
  
  if(a_char_p!=NULL) 
  {
     while(a_char_p>=(unsigned char *)buffer && *a_char_p<0x20) 
     {
         *(a_char_p--)='\0'; 
     }
  }
}

// Beware char are between -127 and +128, ASCII code
// between 0 and 256, then if we want to compare the char to ascii
// vale we need to use unsignbed char
void MECIReadContext::killBlanksNLEnd(char *buffer) const 
{
  unsigned char *a_char_p=(unsigned char *)strrchr(buffer,'\n');
  if(a_char_p==NULL) a_char_p=(unsigned char *)strrchr(buffer,'\r');
  if(a_char_p!=NULL) 
  {
     while(a_char_p>=(unsigned char *)buffer && *a_char_p<=0x20) 
     {
         *(a_char_p--)='\0';
     }
  }
}

const char *MECIReadContext::killBlanksBegin(const char *buffer) const {
  const char *a_char_p=buffer;
  while(*a_char_p==' ') ++a_char_p;
  return a_char_p;
}

void MECIReadContext::killBlanksEnd(char *buffer) const {
  char *a_char_p=buffer+strlen(buffer)-1;
  while(a_char_p>=buffer && *a_char_p==' ') --a_char_p;
  *(++a_char_p)='\0';
}

void MECIReadContext::completeWithBlanks(char *buffer,int nb_chars) const {
  int a_nb_chars=(int)strlen(buffer);
  for(int i=a_nb_chars;i<nb_chars;++i) buffer[i]=' ';
  buffer[nb_chars]='\0';
}

void MECIReadContext::upcase(char *buffer) const {
  int a_nb_chars=(int)strlen(buffer);
  for(int i=0;i<a_nb_chars;++i) buffer[i]=myupcase(buffer[i]);
}


void MECIReadContext::displayCurrentLocation(MyMsgType_e msg_type) const {
}



void MECIReadContext::displayMessage(MyMsgType_e msg_type,const char *format,...) const {
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

const char* MECIReadContext::getCurrentFullName() const{
    return NULL;
}

_HC_LONG MECIReadContext::getCurrentLine() const{
    return 0;
}

void MECIReadContext::SetLineNbChars(int line_nb_chars)
{
    if (line_nb_chars != myLineNbChars)
    {
        myLineNbChars = line_nb_chars;
        if (myBufferNbChars < myLineNbChars + 2)
        {
            myBufferNbChars = myLineNbChars + 2; // +2 for "\r\n" ???
            myLineBuffer = (char*)myrealloc(myLineBuffer, (myBufferNbChars + 1) * sizeof(char));
        }
    }
}
