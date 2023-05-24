//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2023 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.


#include "hardware.inc"
#include "pipes_c.inc"
#include <stdio.h>

#ifdef _WIN64 

#include <sys\types.h>
#include <errno.h>
#include <signal.h>
#include <windows.h> 
#include <winbase.h> 
#include <process.h>
#include <string.h>
#include <ctype.h>
#include <io.h>

#define _FCALL 

#elif 1
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <signal.h>
#include <errno.h>
#include <time.h>




#define _FCALL

#endif

/***************************************************************************/

void syserr(char *msg)
{
    fprintf(stderr,"SYSTEM ERROR>> ");
    perror(msg);
}

void syserr_fatal(char *msg)
{
    fprintf(stderr,"SYSTEM ERROR>> ");
    perror(msg);
    exit(1); 
}

void fatal(char *msg)
{
    fprintf(stderr,"%s\n", msg);
    exit(1); 
}


#ifdef _WIN64
int readr(HANDLE pipe, char *buf, int nbytes)
#else
int readr(int pipe, char *buf, int nbytes)
#endif
{
int error;

#ifdef _WIN64
BOOL fSuccess;
unsigned long ncount,done;
#else
int ncount,done;
#endif
    ncount = nbytes;
    while (ncount > 0)
    {
#ifdef _WIN64
        fSuccess = ReadFile(pipe,buf,ncount*sizeof(TCHAR),&done,NULL);
        if ( ! fSuccess) 
        {error = GetLastError(); done = -1;
         if ((error == 109)||(error == 232)) printf("\nERROR Broken pipe\n\n");}        
#elif 1 
        done = read(pipe, buf, ncount);
#endif
        if (done < 0)
            fatal("Failed reading fifo");    
        else if (done == 0)
            break;
        ncount -= done;
        buf += done;
    }
  return 1;
}

/***************************************************************************/
#ifdef _WIN64
int writer(HANDLE pipe, char*buf, int nbytes)
#else
int writer(int pipe   , char *buf, int nbytes)
#endif
{
int  error;
#ifdef _WIN64
BOOL fSuccess;
unsigned long ncount,done;
#else
int ncount,done;
#endif

    ncount = nbytes;
    while (ncount > 0)
    {
#ifdef _WIN64
        fSuccess = WriteFile(pipe,buf,ncount*sizeof(TCHAR),&done,NULL);    
        if ( ! fSuccess) 
        {error = GetLastError(); done = -1;
         if ((error == 109)||(error == 232)) printf("\nERROR Broken pipe\n\n");}        
#elif 1 
        done = write(pipe, buf, ncount);
#endif
        if (done < 0)
            fatal("Failed writing fifo");
        ncount -= done;
        buf += done;
    }
  return 1;
}





