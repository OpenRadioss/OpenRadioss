//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2024 Altair Engineering Inc.
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
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <signal.h>

#define _FCALL

void    ARRET(int * n);
void    arret_(int * n);
void    arret_c(int n);


#ifdef _WIN64
#define trace_cf TRACE_CF
#else
#define trace_cf trace_cf_
#endif


void    setignorecore (int *on);
void    ignoreCore (int s);
void    trace_cf(int *s,int *iw);

#ifdef _WIN64
void    ignoreCore (int s)
{
  int iw = 1, is;
  if(s == SIGFPE ) is=2;
  else if(s == SIGSEGV) is=3;
	
  trace_cf(&is,&iw);

}

#else

void    ignoreCore (int s)
{
  int iw = 1, is;
  if     (s == SIGBUS ) is=1;
  else if(s == SIGFPE ) is=2;
  else if(s == SIGSEGV) is=3;
	
  trace_cf(&is,&iw);
/*  setIgnoreCore (1);*/
}

#endif

void user_abrt(int sig)
{
 int val=6;         /* arret_c(6) initiates STOP with 3 as error code (system error) */
 printf("\n\nUser or system abort detected (CTRL-C) !\n\n");
 arret_c(val);
}


/************************/
/* get signals for core */
void _FCALL SETIGNORECORE (int *on)
{setignorecore (on);}
void setignorecore__ (int *on)
{setignorecore (on);}
void setignorecore_ (int *on)
{setignorecore (on);}

void    setignorecore (int *on)
{
  if (*on) {			/*ignoring core */
#ifdef SIGBUS
    signal (SIGBUS, ignoreCore);  /* 7 bus error*/
#endif
#ifdef SIGFPE
    signal (SIGFPE, ignoreCore);  /* 8 i/0 */
#endif
#ifdef SIGSEGV
    signal (SIGSEGV, ignoreCore);  /* 11 segmentation violation*/
#endif
  }
  else {			/* acknowledging core */
#ifdef SIGBUS
    signal (SIGBUS, SIG_DFL);  /* 7 bus error*/
#endif
#ifdef SIGFPE
    signal (SIGFPE, SIG_DFL);  /* 8 i/0 */
#endif
#ifdef SIGSEGV
    signal (SIGSEGV, SIG_DFL);  /* 11 segmentation violation*/
#endif
  }

#ifdef SIGINT
signal (SIGINT,user_abrt);  /* 2 Interrupt from keyboard*/
#endif

#ifdef SIGABRT
signal (SIGABRT,user_abrt);  /* 6 Abnormal termination*/
#endif

#ifdef SIGBREAK
signal (SIGBREAK,user_abrt);  /* Ctrl-Break signal*/
#endif

#ifdef SIGTERM
signal (SIGTERM,user_abrt);  /* 15 Termination signal*/
#endif
}
