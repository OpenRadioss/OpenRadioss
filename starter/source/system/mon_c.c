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
#include "btag.inc"

#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64  || CPP_mach == CPP_p4win32

#define _FCALL 

#elif 1
#define _FCALL
#endif


#if CPP_mach==CPP_linux || CPP_mach == CPP_linux64_spmd || CPP_mach == CPP_p4linux964_spmd || CPP_mach == CPP_il_spmd  || CPP_mach == CPP_il || CPP_mach == CPP_linux_spmd || CPP_mach == CPP_linux  || CPP_mach == CPP_linux964 || CPP_mach == CPP_p4linux964 || CPP_mach == CPP_p4linux932

/* Linux : X86, X86-64, IA64*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/sysinfo.h>
#include <unistd.h>
#include <sys/utsname.h>


void cpuinfo_c(char *hostname,int *lenhost, char *cputype, int *lencputype, int *frequence,int *memory, int *swap )
{

  FILE * stream = NULL;
  char ligne[256];
  char freq[256];
  int size,i,j;
  int flag = 0;
  int debut;
  float fre;
  struct sysinfo info;
  struct utsname unam;
#if CPP_mach==CPP_il || CPP_mach==CPP_il_spmd
  char * compstr="family     :";
#elif 1
  char * compstr="model name";
#endif
  char * compMHZ="cpu MHz";
  int cpMHZ=strlen(compMHZ);
  int cmplen=strlen(compstr);
  int unknown;
  int lenmach;

  unknown = 1;
  j= 0;

  stream=fopen("/proc/cpuinfo","r");

  /* ------------ */
  /* find cputype */
  /* ------------ */
  uname(&unam);
  lenmach=strlen(unam.machine);

  while (fgets(ligne,255,stream) && unknown){
    if (strncmp(compstr,ligne,cmplen)==0){
      unknown = 0;
      break;
    }
  }

  if (unknown == 0){
    size = strlen(ligne);
    debut = 12;
    /*Elimination de blanc au debut*/
    do
      {
       debut=debut+1;
      }
    while (ligne[debut]==' '  );

    for (i=debut; i<size-1;i++) {
       if (ligne[i]>31){
       cputype[j] = ligne[i]; 
       j=j+1 ; 
      }
    }
    cputype[j] = ' ' ;
    j=j+1 ; 
    cputype[j] = '(' ;
    j=j+1 ; 
    for (i=0; i<lenmach;i++) { 
       cputype[j] = unam.machine[i]; 
       j=j+1 ; 
     }
    cputype[j] = ')' ;
    j=j+1 ; 
    cputype[j]='\0';  

  }else{
    sprintf(cputype,"Unknown");
  }
  *lencputype =  strlen(cputype);

  /* -------------- */
  /* find Frequence */
  /* -------------- */

  unknown = 1;
  while (fgets(ligne,255,stream) && unknown){
    if (strncmp(compMHZ,ligne,cpMHZ)==0){
      unknown = 0;
      break;
    }
  }
  if ( unknown == 0){
#if CPP_mach==CPP_il || CPP_mach==CPP_il_spmd
      debut = 13;
#elif 1
      debut = 10;
#endif
      j=0;
      size = strlen(ligne);
      for (i=debut; i<size-1;i++) { 
       freq[j] = ligne[i]; 
       j=j+1 ; 
     }
     freq[j]='\0';  
#if CPP_mach==CPP_il || CPP_mach==CPP_il_spmd
     fre = atof(freq);
     *frequence = fre;
#elif 1
     *frequence = atoi(freq);
#endif
  }
  else{
     *frequence = -1;
  }

  fclose(stream); 

  gethostname(hostname, 256);
  *lenhost = strlen(hostname);
  
  /*memory - swap */
    sysinfo ( &info );
    *memory =  info.totalram*info.mem_unit/(1024*1024);
    *swap = info.freeswap*info.mem_unit/(1024*1024);
  

}

#elif CPP_mach == CPP_wmr || CPP_mach==CPP_wnt ||  CPP_mach==CPP_p4win64_spmd ||  CPP_mach==CPP_win64_spmd || CPP_mach==CPP_p4win64 || CPP_mach==CPP_p4win32
/* WIN XP - WIN SERVER 2003 - WIN SERVER 2003 CCE */

#include <windows.h>
#include <winbase.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <process.h>
#include <io.h>
#include <sys\types.h>

#if CPP_mach == CPP_p4win64_spmd ||  CPP_mach==CPP_p4win64 ||  CPP_mach==CPP_p4win32 || CPP_mach==CPP_wnt && CPP_rel==10
#define popen _popen
#define pclose _pclose
#endif

void cpuinfo_c(char *hostname,int *lenhost, char *cputype, int *lencputype, int *frequence,int *memory, int *swap )
{
  FILE * stream = NULL;
  char * hname2;
  int len,j,error;
  char hname1[512];
  int nRet;
  WSADATA wsaData;
  WORD version = MAKEWORD(1,1);

  HKEY handle;
  char RegKey_Entry[]="HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0";
  char query1[] = "ProcessorNameString";
  SYSTEM_INFO info;
  int mxlen1=512;
  DWORD mhz; 
  int l = sizeof(mhz);
#if CPP_mach==CPP_wnt
  MEMORYSTATUS statex;
#elif 1
  MEMORYSTATUSEX statex;
#endif

/*hostname*/
  nRet = WSAStartup(version, &wsaData);
  len=gethostname(hostname,256);
  len=WSAGetLastError();
  *lenhost = strlen(hostname);

/* CPU type*/
    /* registry based access */
    if ( RegOpenKeyEx ( HKEY_LOCAL_MACHINE,RegKey_Entry,0, KEY_QUERY_VALUE, &handle ) != ERROR_SUCCESS ) goto FAIL1;
    if ( RegQueryValueEx (handle,query1, NULL, NULL,(LPBYTE) hname1,(LPDWORD) &mxlen1 ) != ERROR_SUCCESS ) goto FAIL1;  
    RegCloseKey ( handle );

    GetSystemInfo(&info);
    if (info.dwProcessorType < 8664) {
      hname2="x86";
    }
    else { hname2="x86_64"; }

    sprintf(cputype,"%s (%s)",hname1,hname2);
    goto CONTINUE;

FAIL1:
    sprintf(cputype,"UNKOWN");

CONTINUE:

    *lencputype = strlen(cputype);

/*Frequence*/

    if ( RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0",
			0, KEY_QUERY_VALUE, &handle ) != ERROR_SUCCESS )
      return 0;
    if ( RegQueryValueEx ( handle, "~MHz", NULL, NULL,
			   (LPBYTE )(&mhz) ,(LPDWORD) &l ) != ERROR_SUCCESS )
      return 0;
    RegCloseKey ( handle );
    *frequence = (int)(mhz/50.0 +0.5)*50;

/*memory,swap */
  *memory=-1;
  *swap=-1;
#if CPP_mach==CPP_wnt     
    statex.dwLength = sizeof (statex);
    
    GlobalMemoryStatus (&statex);
    *memory = statex.dwAvailPhys/(1024*1024);
  
    *swap=statex.dwTotalPageFile/(1024*1024);
#elif 1
   statex.dwLength = sizeof (statex);
    GlobalMemoryStatusEx (&statex);

    *memory = statex.ullAvailPhys/(1024*1024);
    *swap=statex.ullTotalPageFile/(1024*1024);

#endif

}


#elif CPP_mach ==CPP_hp11 || CPP_mach == CPP_hp11_spmd

/* HP-UX */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/pstat.h>
#include <sys/param.h>
#include <unistd.h>


void cpuinfo_c(char *hostname,int *lenhost, char *cputype, int *lencputype, int *frequence,int *memory, int *swap )
{
  char ligne[256];
  char freq[256];
  char cpt[256];
  char cmdline[512];
  char *ret;
  int size,i,j,sz;
  int debut;

/*utilise pour frequence*/
  struct pst_processor psp;
  struct pst_dynamic psd;
  struct pst_static pst;

  unsigned long int clock_speed, scclktick;

   FILE * stream = NULL;

   stream = popen ("hostname","r"); 
   fgets(ligne,256, stream);

/* hostname */
   fgets(ligne,256, stream);
   size = strlen(ligne);
   for ( i=0 ; i< size-1; i++) {
     hostname[i] = ligne[i] ; 
   }
   hostname[size-1] = '\0';
   *lenhost = strlen(hostname);
   pclose(stream);

/* cputype */
   stream = popen ("model","r"); 
   fgets(ligne,256, stream);
   size = strlen(ligne);
   for ( i=0 ; i< size-1; i++) {
     cputype[i] = ligne[i] ; 
   }

   cputype[size-1] = '\0';
   pclose(stream);

/* a faire sur HP PA ONLY */
#if CPP_mach==CPP_hp11_spmd  && CPP_rel != 600|| CPP_mach==CPP_hp11 && CPP_rel != 600
   debut=0;
   j = 0;
   do {
       debut ++;
      } while (ligne[debut] != '/');

   do {
        debut++;
        cpt[j] = ligne[debut];
        j++;
      } while (ligne[debut] != '/');
   cpt[j-1] = '\0';

   sprintf(cmdline," grep -i %s  /usr/sam/lib/mo/sched.models | awk '{print $3}'",cpt);
   stream = popen (cmdline,"r"); 
   fgets(ligne,256, stream);
   sz = strlen(ligne);

   cputype[size-1] = ' ';
   cputype[size] = '-';
   size++;
   cputype[size] = ' ';
   size++;
   
   for (i=0;i<sz-1;i++) {
     cputype[size]=ligne[i];
     size++;
   }
   cputype[size]='\0';
   pclose(stream);
#endif
   *lencputype = strlen(cputype);


/* frequence */
   pstat_getprocessor(&psp, sizeof(psp), 1, 0);
   scclktick=sysconf(_SC_CLK_TCK);
   clock_speed = psp.psp_iticksperclktick * scclktick;
   clock_speed = (clock_speed/1000000);
   *frequence = clock_speed;

/*memory */
   pstat_getstatic(&pst, sizeof(pst), (size_t)1, 0);
  *memory=pst.physical_memory*pst.page_size/(1024*1024);

/*swap*/  
  pstat_getdynamic(&psd, sizeof(psd), (size_t)1, 0);
 *swap = ((psd.psd_vm*pst.page_size - pst.physical_memory*pst.page_size)>0)?(psd.psd_vm*pst.page_size - pst.physical_memory*pst.page_size)/(1024*1024):0;

}


/* IBM */

#elif CPP_mach == CPP_pwr4_spmd || CPP_mach ==CPP_pwr4

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>


void cpuinfo_c(char *hostname,int *lenhost, char *cputype, int *lencputype, int *frequence,int *memory, int *swap )

{
  char ligne[256],swaps[256];
  char freq[256];
  char cpt[256];
  char cmdline[512];
  char *ret;
  int size,i,j,sz;
  int debut, lswaps;
  FILE * stream = NULL;

   stream = popen ("hostname","r");
   fgets(ligne,256, stream);

/* hostname */
   fgets(ligne,256, stream);
   size = strlen(ligne);
   for ( i=0 ; i< size-1; i++) {
     hostname[i] = ligne[i] ;
   }
   hostname[size-1] = '\0';
   pclose(stream);

   *lenhost = strlen(hostname);

   stream = popen ("lscfg | grep proc | awk '{print $2}'","r");

   fgets(ligne,256, stream);
   size = strlen(ligne);
   for (i=0;i<size-1;i++) cpt[i]=ligne[i];
   cpt[size-1]='\0';
   pclose(stream);

   sprintf(cmdline,"lsattr -El %s | grep frequency | awk '{print $2}'",cpt);

   stream = popen (cmdline,"r");
   fgets(ligne,256, stream);
   pclose(stream);
/* frequence */
   *frequence = atoi(ligne) / 1000000;


/* cputype */
   sprintf(cmdline,"lsattr -El %s | grep type",cpt);

   stream = popen (cmdline,"r");
   fgets(ligne,256, stream);
   pclose(stream);

   debut = 0;
   do {
      debut++;
     } while (ligne[debut]!=' ');
    debut++;
   do {
      debut++;
     } while (ligne[debut]==' ');

   i=0;
   do {
      cputype[i] = ligne[debut] ;
      i++;
      debut++;
     } while (ligne[debut]!=' ');
   pclose(stream);
   cputype[i] = '\0';

   *lencputype=strlen(cputype);

/*memory */
  *memory=-1;
   sprintf(cmdline,"lsattr -El mem0 | awk '{print $2}' ");
   stream = popen (cmdline,"r");
   fgets(ligne,256, stream);
   pclose(stream);

   *memory = atoi(ligne);
/*swap*/
  *swap=-1;
   sprintf(cmdline,"lsps -s | awk '{print $1}' ");
   stream = popen (cmdline,"r");
   fgets(ligne,256, stream);
   fgets(ligne,256, stream);
   pclose(stream);
   lswaps = 0;
   for (i=0;i<strlen(ligne);i++){
    if (ligne[i]>='0' && ligne[i]<='9'){
      swaps[lswaps]=ligne[i];
      lswaps++;
    }
   }
   swaps[lswaps]='\0';
  *swap = atoi(swaps);
  /*lsps -a*/
}


#elif CPP_mach==CPP_sun25 || CPP_mach==CPP_sol10x64_spmd
#include <stdlib.h>
#include <unistd.h>
#include <strings.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/processor.h>
#include <sys/systeminfo.h>

#if CPP_rel == 1000  || CPP_mach==CPP_sol10x64_spmd
#include <kstat.h>
#include <inttypes.h>
#endif


void cpuinfo_c(char *hostname,int *lenhost, char *cputype, int *lencputype, int *frequence,int *memory, int *swap )
{
  processor_info_t infop;
  char ligne[256],swaps[256];
  int i,resus,size;
  int proc_id=0;
  int  me,debut;
  int lswap=0;
  int count=0;
   FILE * stream = NULL;
#if CPP_rel == 1000 || CPP_mach==CPP_sol10x64_spmd
     kstat_ctl_t   *kc;
     kstat_t       *ksp;
     kstat_named_t *knp;
#endif

   stream = popen ("hostname","r");
   fgets(ligne,256, stream);

/* hostname */
   fgets(ligne,256, stream);
   size = strlen(ligne);
   for ( i=0 ; i< size-1; i++) {
     hostname[i] = ligne[i] ;
   }
   hostname[size-1] = '\0';
   *lenhost = strlen(hostname);
   pclose(stream);

/* cputype, frequency */
    processor_info(proc_id, &infop);
#if CPP_rel == 1000 || CPP_mach==CPP_sol10x64_spmd
     kc = kstat_open();
     ksp = kstat_lookup(kc, "cpu_info", -1, NULL);
     kstat_read(kc, ksp, NULL);
     knp = kstat_data_lookup(ksp, "brand");
     sprintf(cputype,"%s",knp->value);
#elif 1
    strcpy(cputype,infop.pi_processor_type);
#endif
    *lencputype = strlen(cputype);

    *frequence = infop.pi_clock;

/*memory */
  *memory=-1;

*memory  = (int)sysconf(_SC_PHYS_PAGES)/1024 * sysconf(_SC_PAGESIZE)/1024;

/*swap*/
  stream = popen("swap -s","r");
  fgets(ligne,256, stream);
  pclose(stream);
  *swap=-1;

 swaps[0]='\0';
 debut=0;
 for (i=0;i<strlen(ligne);i++)
  {
   if (ligne[i]>='0' && ligne[i]<='9')
    {
      swaps[lswap]=ligne[i];
      lswap++;
      debut=1;
     }
    else {
     if (debut==1){
           if (count<3){
                        lswap=0;
                        debut=0;
                        count++;
                       }
           else
           { break;}
         }
    }
  }
  *swap = atoi(swaps)/1024;

}

#elif CPP_mach==CPP_macosx64

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/sysctl.h>
#include <unistd.h>
#include <strings.h>


void cpuinfo_c(char *hostname,int *lenhost, char *cputype, int *lencputype, int *frequence,int *memory, int *swap )
{
  FILE * stream = NULL;
  long freq,frq,mem;
  size_t  ilen,lenh;
  char  swapstring[256],s1[256],s2[256],s3[256],s4[256];
  ilen=8;
  lenh=256;
  mem=0;
  /*----------*/
  /* Hostname */
  /*----------*/

  gethostname(hostname,lenh);
  *lenhost = strlen(hostname);

  /*-----------*/
  /* CPU brand */
  /*-----------*/

  sysctlbyname("machdep.cpu.brand_string", cputype, &lenh, NULL, 0);
  *lencputype = strlen(cputype);

  /*---------------*/
  /* CPU Frequency */
  /*---------------*/

  sysctlbyname("hw.cpufrequency",&freq,&ilen, NULL, 0);
  frq = freq/1000000;
  *frequence = (int)frq;

  /*---------------*/
  /* System Memory */
  /*---------------*/

   sysctlbyname("hw.physmem",&mem,&ilen, NULL, 0);
   mem = mem / 1048576 ;
   *memory = (int) mem;

  /*---------------*/
  /* Swap          */
  /*---------------*/
  stream = popen ("df -k /","r");
  fgets(swapstring,256, stream);
  fgets(swapstring,256, stream);

  sscanf(swapstring,"%s %s %s %s",s1,s2,s3,s4);
  *swap = atoi(s4)/1024;

}


#elif 1

#include <stdio.h>
#include <string.h>
#include <stdlib.h>


void cpuinfo_c(char *hostname,int *lenhost, char *cputype, int *lencputype, int *frequence,int *memory, int *swap )
{
 sprintf(hostname,"UNKNOWN");
 *lenhost = strlen(hostname);
 sprintf(cputype,"UNKNOWN");
 *lencputype = strlen(cputype);
 *frequence = 0;

/*memory */
  *memory=-1;
/*swap*/  
  *swap=-1;
}

#endif


void cpuinfo(char *hostname,int *lenhost, char *cputype, int *lencputype,int *frequence,int *memory, int *swap  )
{
  cpuinfo_c(hostname,lenhost, cputype,lencputype, frequence,memory, swap );
}

void cpuinfo_(char *hostname,int *lenhost, char *cputype, int *lencputype, int *frequence,int *memory, int *swap  )
{
  cpuinfo_c(hostname,lenhost, cputype,lencputype, frequence,memory, swap );
}

void cpuinfo__(char *hostname,int *lenhost, char *cputype, int *lencputype, int *frequence,int *memory, int *swap  )
{
  cpuinfo_c(hostname,lenhost, cputype,lencputype, frequence,memory, swap );
}

void _FCALL CPUINFO (char *hostname,int *lenhost, char *cputype, int *lencputype, int *frequence,int *memory, int *swap  )
{
  cpuinfo_c(hostname,lenhost, cputype,lencputype, frequence,memory, swap  );
}


