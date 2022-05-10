//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2022 Altair Engineering Inc.
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
#include <stdlib.h>
#include <string.h>

#define _FCALL 


/* Define 
     ARCH_TYPE HW target 
              ARCH_TYPE=1  X86-64
              ARCH_TYPE=2  ARM64

     OS_TYPE Linux or Windows
              OS_TYPE=1  Linux
              OS_TYPE=2  Windows

         
 */


/* ---------------- */
/* Define ARCH_TYPE */
/* ---------------- */
#ifdef unix

#ifdef __x86_64
#define ARCH_TYPE 1
#endif

#ifdef __ARM_ARCH
#define ARCH_TYPE 2
#endif

#endif

#ifdef _WIN64
#define ARCH_TYPE 1
#endif

/* ---------------- */
/* Define ARCH_TYPE */
/* ---------------- */

#ifdef unix
#define OS_TYPE 1
#endif

#ifdef _WIN64
#define OS_TYPE 2
#endif

int parse_cpuinfo();


/* --------------------------------------------------
   get_ibuiltin_arch() 
   routine to find the underlying CPU/OS architecture 
   to apply propper Domain Decomposition optimization

   0 - X86-64 Linux AVX-2
   1 - X86-64 Linux AVX-512
   2 - X86-64 Linux SSE3
   3 - ARM64 Linux
   4 - X86-64 Windows AVX-2
   -------------------------------------------------- */

void get_ibuiltin_arch(int *iarch)
{
  int os_type,arch_type;

  os_type=OS_TYPE;
  arch_type=ARCH_TYPE;

  *iarch=0;
  switch (arch_type){
     case 1 : { if (os_type==1){
                      *iarch= parse_cpuinfo();
                }else {
                      *iarch= 4;
                      }
                } 
                break;

     case 2 : *iarch=3;break;
  }

  
}

void get_ibuiltin_arch_(int *iarch){
    get_ibuiltin_arch(iarch);
}

void _FCALL GET_IBUILTIN_ARCH(int *iarch){
    get_ibuiltin_arch(iarch);
}

int parse_cpuinfo(){

  FILE * stream = NULL;
  char ligne[2049];
  char * compstr="flags		:";
  char * strptr;
  int unknown;
  int cmplen=strlen(compstr);

  int instruction_set=0;

  stream=fopen("/proc/cpuinfo","r");
  unknown = 1;
  while (fgets(ligne,2048,stream) && unknown){
    if (strncmp(compstr,ligne,cmplen)==0){
      unknown = 0;
      break;
    }
  }
  strptr = strstr(ligne,"avx512");

  if (strptr !=NULL){
      instruction_set=1;
  }else{ 
      strptr = strstr(ligne,"avx2");
      if (strptr !=NULL){
          instruction_set=0;
      }else{ 
          instruction_set=2;
      }
  }
 

return instruction_set;
}

#ifdef MAIN

int main()
{
  
  int iflag;
  get_ibuiltin_arch(&iflag);

  printf("\nArchitecture Found= %i\n\n",iflag);

  return 1;

}
#endif
