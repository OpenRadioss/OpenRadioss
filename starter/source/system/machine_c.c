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

#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64  || CPP_mach == CPP_p4win32

#define _FCALL 

#elif 1
#define _FCALL
#endif



#if CPP_mach==CPP_linux || CPP_mach == CPP_linux64_spmd || CPP_mach == CPP_p4linux964_spmd || CPP_mach == CPP_il_spmd  || CPP_mach == CPP_il || CPP_mach == CPP_linux_spmd || CPP_mach == CPP_linux  || CPP_mach == CPP_linux964 || CPP_mach == CPP_p4linux964

#include <stdio.h>
#include <sys/resource.h>

void set_stack_c_(int * stack){

  const rlim_t kStackSize = -1L;   // -1L = Unlimited
    struct rlimit rl;
    long int StMB;
    int result;

    result = getrlimit(RLIMIT_STACK, &rl);

    if (rl.rlim_cur != -1){
    /* Current Stacksize not unlimited */

      /* Set the Stacksize to the Max possible value (best is unlimited : -1) */
      if (rl.rlim_max != -1L){
	rl.rlim_cur = rl.rlim_max;
       }else{ 
	rl.rlim_cur = kStackSize;
            }
  
        result = setrlimit(RLIMIT_STACK, &rl);

    }
    StMB = rl.rlim_cur / (1024*1024);
    *stack = StMB;
}

#elif 1
void set_stack_c_(int * stack){
  *stack = 0;
  }
#endif


void set_stack_c__(int * stack){
  set_stack_c_(stack);

  }

void _FCALL SET_STACK_C (int * stack){
  set_stack_c_(stack);

  }


#if CPP_mach == CPP_p4win64  || CPP_mach == CPP_p4win32
#include <windows.h>
#include <stdio.h>
#include <tchar.h>


void _FCALL  RAD2RAD_CREATEPROCESS(char* command_line, int * len,int * ID,int * err ) 
{
  char* line;
  int i,size;
  STARTUPINFO si;
  PROCESS_INFORMATION pi;

  size= * len;
  line = malloc(sizeof(char)*size+1);
  for (i=0;i<size;i++) line[i] = command_line[i];
  line[size] ='\0';

  ZeroMemory(&si,sizeof(si) );
  si.cb = sizeof(si);
  ZeroMemory (&pi, sizeof(pi) );
  
  * err = 0;
  * ID = 0;
  if ( ! CreateProcess (NULL,
                        line,
                        NULL,
                        NULL,
                        FALSE,
                        0,
                        NULL,
                        NULL,
                        &si,
                        &pi )  ){
          printf("ERROR CREATING CHILD PROCESS : %i \n",GetLastError() );
          *err = 1;
      } else {
          * err = 0;
          * ID = (int) pi.hProcess;
      } 

}
#endif
