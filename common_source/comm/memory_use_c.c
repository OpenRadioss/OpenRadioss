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
#include "hardware.inc"

#define _FCALL 


#if CPP_mach==CPP_linux || CPP_mach == CPP_linux64_spmd || CPP_mach == CPP_p4linux964_spmd || CPP_mach == CPP_il_spmd  || CPP_mach == CPP_il || CPP_mach == CPP_linux_spmd || CPP_mach == CPP_linux  || CPP_mach == CPP_linux964 || CPP_mach == CPP_p4linux964 || CPP_mach == CPP_p4linux932

/* ----- */
/* LINUX */
/* ----- */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <strings.h>

void map_memory(int * VmPeak, int *VmSize,int *VmRSS,int *VmHWM,int *VmStk )
{
  int pid;
  FILE * stream;
  char fil[512],cmd_line[512],line[512],str1[128],str2[128],size[128],size2[128],size3[128],size4[128],size5[128];
  char * rd;
  int i,slen,non_convertible,nreadi,VmP,VmS;

  pid = getpid();

  sprintf(fil,"/proc/%i/status",pid);
  stream = fopen(fil,"r");

  *VmPeak=-1;
  *VmSize=-1;
  *VmRSS=-1;
  *VmHWM=-1;
  *VmStk=-1;

  while (fgets(line,512,stream)){
     if (strncmp(line,"VmPeak",6)==0){
       sscanf(line,"%s %s %s",str1,size,str2);
       *VmPeak=atoi(size)/1024;
     }

     if (strncmp(line,"VmSize",6)==0){
       sscanf(line,"%s %s %s",str1,size2,str2);
      *VmSize=atoi(size2)/1024;
     }

     if (strncmp(line,"VmRSS:",6)==0){
       sscanf(line,"%s %s %s",str1,size3,str2);
       *VmRSS=atoi(size3)/1024;
     }

     if (strncmp(line,"VmHWM:",6)==0){
       sscanf(line,"%s %s %s",str1,size4,str2);
       *VmHWM=atoi(size4)/1024;
     }

     if (strncmp(line,"VmStk:",6)==0){
       sscanf(line,"%s %s %s",str1,size5,str2);
       *VmStk=atoi(size5)/1024;
     } 
  }
  fflush(stdout);
  fclose(stream);
}


void memory_stats_c_()
{
  int pid;
  FILE * stream;
  char fil[512],cmd_line[512],line[512],str1[128],str2[128],size[128],size2[128],size3[128],size4[128];
  char * rd;
  int i,slen,non_convertible,nreadi,VmP,VmS;

  pid = getpid();

  sprintf(fil,"/proc/%i/status",pid);
  stream = fopen(fil,"r");
  VmP=0;
  VmS=0;
  while (fgets(line,512,stream)){
     if (strncmp(line,"VmPeak",6)==0){
       sscanf(line,"%s %s %s",str1,size,str2);
       printf(" %s %s %s \n",str1,size,str2);

     }
     if (strncmp(line,"VmSize",6)==0){
       sscanf(line,"%s %s %s",str1,size2,str2);
       printf(" %s %s %s \n",str1,size2,str2);
     }
     if (strncmp(line,"VmRSS:",6)==0){
       sscanf(line,"%s %s %s",str1,size3,str2);
       printf(" %s %s %s \n",str1,size3,str2);
     }
     if (strncmp(line,"VmHWM:",6)==0){
       sscanf(line,"%s %s %s",str1,size4,str2);
       printf(" %s %s %s \n",str1,size4,str2);
     }
     if (strncmp(line,"VmStk:",6)==0){
       sscanf(line,"%s %s %s",str1,size4,str2);
       printf(" %s %s %s \n",str1,size4,str2);
     }
    
  }
  fflush(stdout);
  fclose(stream);
}

void memory_use_c(int *memsize)
{
  int pid;
  FILE * stream;
  char fil[512],cmd_line[512],line[512],str1[128],str2[128],size[128],size2[128];
  char * rd;
  int i,slen,non_convertible,nreadi,VmP,VmS;
  *memsize=-1;

  pid = getpid();

  sprintf(fil,"/proc/%i/status",pid);
  stream = fopen(fil,"r");
  VmP=0;
  VmS=0;
  while (fgets(line,512,stream)){
     if (strncmp(line,"VmPeak",6)==0){
       VmP=1;
       sscanf(line,"%s %s %s",str1,size,str2);
       break;
     }
     if (strncmp(line,"VmSize",6)==0){
       VmS=1;
       sscanf(line,"%s %s %s",str1,size2,str2);
     }
  }

  non_convertible=0;
  if (VmP==1){
    slen = strlen(size);
    for (i=0;i<slen;i++){if (size[i] < '0' && size[i] > '9') non_convertible=1;}
    if (non_convertible==0){*memsize = atoi(size)/1024;}else{*memsize = -1;}
  }
   else{
    if (VmS==1){
      slen = strlen(size2);
      for (i=0;i<slen;i++){
         if (size2[i] < '0' && size2[i] > '9') non_convertible=1;
      }
    if (non_convertible==0){*memsize = atoi(size2)/1024;}else{*memsize = -1;}
   }else {*memsize = -1;}
 }
  fclose(stream);
}


#elif CPP_mach==CPP_p4win64_spmd ||  CPP_mach==CPP_win64_spmd || CPP_mach==CPP_p4win64 || CPP_mach==CPP_p4win32

/* ------- */
/* WINDOWS */
/* ------- */
#include <stdio.h>
#include <Windows.h>
#include <Psapi.h>

void memory_use_c(int *memsize)
{
  PROCESS_MEMORY_COUNTERS MemInfo; 
  *memsize=-1;
  GetProcessMemoryInfo(GetCurrentProcess(), &MemInfo, sizeof(MemInfo));
  *memsize = MemInfo.PeakWorkingSetSize / 1048576;
}


void map_memory(int * VmPeak, int *VmSize,int *VmRSS,int *VmHWM,int *VmStk )
{
  int pid;
  FILE * stream;
  char fil[512],cmd_line[512],line[512],str1[128],str2[128],size[128],size2[128],size3[128],size4[128],size5[128];
  char * rd;
  int i,slen,non_convertible,nreadi,VmP,VmS;
  PROCESS_MEMORY_COUNTERS MemInfo; 


  *VmPeak=-1;
  *VmSize=-1;
  *VmRSS=-1;
  *VmHWM=-1;
  *VmStk=-1;
  GetProcessMemoryInfo(GetCurrentProcess(), &MemInfo, sizeof(MemInfo));

  *VmPeak = MemInfo.PeakWorkingSetSize / 1048576;
  *VmSize = MemInfo.WorkingSetSize / 1048576;
  
  }




#elif 1
/* --------------- */
/* Default Routine */
/* --------------- */
void memory_use_c(int *memsize)
{
  *memsize=-1;
}

void map_memory(int * VmPeak, int *VmSize,int *VmRSS,int *VmHWM,int *VmStk )
{
  *VmPeak=-1;
  *VmSize=-1;
  *VmRSS=-1;
  *VmHWM=-1;
  *VmStk=-1;
}

#endif



void memory_use_c_(int *memsize)
{
  memory_use_c(memsize);
}

void memory_use_c__(int *memsize)
{
  memory_use_c(memsize);
}


void _FCALL MEMORY_USE_C (int *memsize)
{
  memory_use_c(memsize);
}

void _FCALL MEMORY_STATS_C ()
{
  printf("Not Available under Windows\n");
}


/* ---------------------- */
void map_memory_(int * VmPeak, int *VmSize,int *VmRSS,int *VmHWM,int *VmStk )
{
  map_memory(VmPeak,VmSize,VmRSS,VmHWM,VmStk );
}

void map_memory__(int * VmPeak, int *VmSize,int *VmRSS,int *VmHWM,int *VmStk )
{
  map_memory(VmPeak,VmSize,VmRSS,VmHWM,VmStk );
}

void _FCALL MAP_MEMORY(int * VmPeak, int *VmSize,int *VmRSS,int *VmHWM,int *VmStk )
{
  map_memory(VmPeak,VmSize,VmRSS,VmHWM,VmStk );
}

