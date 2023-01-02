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
#if CPP_mach==CPP_sun || CPP_mach==CPP_sun25 || CPP_mach==CPP_ppw_spmd || CPP_mach==CPP_ppw
#include <utmpx.h>
#endif
#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
#include <sys\types.h>
#include <errno.h>
#include <signal.h>
#include <windows.h> 
#include <winbase.h> 
#include <process.h>
#include <string.h>
#include <ctype.h>
#include <io.h>
/* def signaux bidons */
#define SIGHUP   1
#define SIGQUIT  3
#define SIGBUS  10
#define SIGPIPE 13
#define _FCALL 
#elif 1
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include <stdlib.h>
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


static void cleanup(int sig, int ppid)
{
    if (signal(sig,SIG_IGN) == SIG_ERR)
        syserr("signal");
    printf("\n ** ERROR: Catched system signal: ");
    switch (sig)
    {
    case SIGHUP:
        printf("Hangup\n");
        break;
    case SIGINT:
        printf("Process interrupted\n");
        break;
    case SIGILL:
        printf("Illegal instruction\n");
        break;
    case SIGABRT:
        printf("Process aborted\n");
        break;
    case SIGQUIT:
        printf("Process Quit\n");
        break;
    case SIGFPE:
        printf("Floating point exception\n");
        break;
    case SIGBUS:
        printf("Bus error\n");
        break;
    case SIGSEGV:
        printf("Segmentation fault\n");
        break;
    case SIGPIPE:
        printf("Broken pipe, lost contact with master process\n");
        break;
    default:
        break;
    }
#if CPP_mach==CPP_sgi5 || CPP_mach==CPP_sgi6
    printf("\tSending termination signal to master...\n");
    if(sigsend(P_PID, ppid, SIGUSR1))
            syserr("Signal");
#endif
    exit(0);
}



void catch_sig_c(int *pid)
{

#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
#elif 1
    sigset(SIGHUP,  cleanup);
    sigset(SIGINT,  cleanup);
    sigset(SIGILL,  cleanup);
    sigset(SIGABRT, cleanup);
    sigset(SIGQUIT, cleanup);
    sigset(SIGFPE,  cleanup);
    sigset(SIGBUS,  cleanup);
    sigset(SIGSEGV, cleanup);
    sigset(SIGPIPE, cleanup);
#endif
}
void _FCALL CATCH_SIG_C(pid)
int *pid;
{
    catch_sig_c(pid);
}
void catch_sig_c_(pid)
int *pid;
{
    catch_sig_c(pid);
}
void catch_sig_c__(pid)
int *pid;
{catch_sig_c(pid);}



