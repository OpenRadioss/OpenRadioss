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


#include "win32_utils.h"   

#include <sys/types.h>
#include <sys/stat.h>
#include <cstdlib>

#if defined _WIN32 || defined WIN32
#include <windows.h>
#include <stdio.h>
#include <process.h>
#else  //_WIN32
#include <unistd.h>
#endif //_WIN32

#include "mv_cstdlib.h"
#include "mv_cstdio.h"
#include "error.h"
#include "system_utils.h"

unsigned long mygetpid() {
// RA 09/11/01
#if defined _WIN32 || defined WIN32
  return (unsigned long)_getpid();
#else  //_WIN32
  return (unsigned long)getpid();
#endif //_WIN32
}

string mygetenv(const string &var,const string &default_value) {
  const char *a_value=getenv(var.c_str());
  return a_value==NULL ? default_value.c_str() : a_value;
}

#if defined _WIN32 || defined WIN32
extern "C" HFILE ProtectFile(char *path)
#else
extern "C" FILE *ProtectFile(char *path)
#endif
{
//	FILE *file_tmp = NULL ;
 #if defined _WIN32 || defined WIN32
    HFILE hFile;
    
    
    OFSTRUCT lpReOpenBuff  ;
#endif
    if (path == NULL) return NULL ;
#if defined _WIN32 || defined WIN32

    hFile = OpenFile(path, // open One.txt
                     &lpReOpenBuff,
                     OF_SHARE_DENY_WRITE);                    // no attr. template
    /*
    dwPos = SetFilePointer(hFile, 0, NULL, FILE_END);
    LockFile(hFile, dwPos, 0, 0, 0);
    */
    return hFile ;
	/*return fopen(path, "r") ;*/
 #else
	chmod(path, S_IRUSR) ;
	return fopen(path, "r") ;
#endif
}
#if defined _WIN32 || defined WIN32
extern "C" void UnProtectFile( HANDLE hFile)
#else
extern "C" void UnProtectFile(FILE *path)
#endif
{
#if defined _WIN32 || defined WIN32
    DWORD  dwPos;
    dwPos = SetFilePointer(hFile, 0, NULL, FILE_END);

    if (hFile == NULL) return ;
#else
    if (path == NULL) return ;
#endif
    #if defined _WIN32 || defined WIN32
    CloseHandle(hFile);
	/*  fclose(path) ;*/
    #else
	    fchmod(fileno(path), S_IRWXU) ;
    #endif
    return ;
}




