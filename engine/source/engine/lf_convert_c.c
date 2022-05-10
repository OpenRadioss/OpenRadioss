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
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#if CPP_mach==CPP_sun || CPP_mach==CPP_sun25 || CPP_mach==CPP_ppw_spmd || CPP_mach==CPP_ppw  || CPP_mach==CPP_CPP_sol10x64_spmd
#include <utmpx.h>
#endif
#if CPP_mach != CPP_macosx64
#include <malloc.h>
#endif
#include <fcntl.h>
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
#include <sys/stat.h>
#define _FCALL 
#define TODOS   1 
#elif 1
#include <sys/resource.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/param.h>
#define _FCALL
#define TODOS   0 
#endif
#ifndef BUFSIZE
#define BUFSIZE 1024
#endif

#define CR      (int) 0x0D   
#define LF      (int) 0x0A
#define MAXINCLVL 15
#define MAXINCNAMELEN 128


/***************************************************************************/

static void syserr(char *msg)
{
	fprintf(stderr,"SYSTEM ERROR>> ");
	perror(msg);
}

static void syserr_fatal(char *msg)
{
	fprintf(stderr,"SYSTEM ERROR>> ");
	perror(msg);
	exit(1); 
}

static void fatal(char *msg)
{
	fprintf(stderr,"%s\n", msg);
	exit(1); 
}

/***************************************************************************/
static int readbuf(fid, buf, nbytes)
register int fid;
register char *buf;
register int nbytes;
{
int ncount, done, nread;
#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
BOOL fSuccess;
#endif
	  ncount = nbytes;
   nread  = 0;
	  while (ncount > 0) {
#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
      fSuccess = ReadFile(fid, buf, ncount*sizeof(TCHAR), &done, NULL);
#elif 1 
		    done = read(fid, (void *) buf, (size_t) ncount);
#endif
      nread += done;
		    if (done < 0)                       
		       	fatal("Failed reading buffer");	   
		    else if (done == 0)                 
		       	break;                             
		    ncount -= done;              
		    buf += done;                        
	   }
 return nread;
}

static int writebuf(fid, buf, nbytes)
register int fid;
register char *buf;
register int nbytes;
{
int ncount, done, nwrite;
#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
BOOL fSuccess;
#endif

	  ncount = nbytes;
   nwrite = 0;
	  while (ncount > 0){
#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
        fSuccess = WriteFile(fid, buf,ncount*sizeof(TCHAR),&done,NULL);
#elif 1 
	      	done = write(fid, (void *) buf, (size_t) ncount);
#endif
        nwrite += done;
  		    if (done < 0)
  		    	  fatal("Failed writing buffer");	
  		    ncount -= done;
		      buf += done;                        
  	}
 return nwrite;
}


#if CPP_mach==CPP_p4win64_spmd ||  CPP_mach==CPP_win64_spmd || CPP_mach==CPP_p4win64 || CPP_mach==CPP_p4win32  || CPP_mach==CPP_wnt
#include <direct.h>

static char* tmpenv_c(){
  char * tmpdir;
  tmpdir =  getenv("TMPDIR");

  /* second trial get current working directory */
  if (tmpdir==NULL){
    tmpdir = _getcwd( NULL, 0 );
  }
  return tmpdir;
}
static char* cwd_c(){
  char * tmpdir;

  tmpdir = (char *)calloc(2048,sizeof(char));
  tmpdir = _getcwd( NULL, 0 );
  return tmpdir;
}
#elif 1

static char* tmpenv_c(){
  char * tmpdir;

  tmpdir =  getenv("TMPDIR");
  /* second trial get current working directory */
  if (tmpdir==NULL){
    tmpdir = (char *)calloc(2048,sizeof(char));
    getcwd(tmpdir,2048);
  }
  return tmpdir;
}

static char* cwd_c(){
  char * tmpdir;

  tmpdir = (char *)calloc(2048,sizeof(char));
  getcwd(tmpdir,2048);
  return tmpdir;
}
#endif
/***************************************************************************/

static void do_lf_convert (fdi, fdo)
int fdi, fdo;
{
  int  nread, nwrite, i, ch, last, len;
  char   inbuf[BUFSIZE], outbuf[BUFSIZE*2];
  
      if (TODOS)   /* convert to DOS */
          do {                                                    
              nwrite = 0;                                         
              nread  = readbuf(fdi, inbuf, (int )sizeof(inbuf));        
              for (i=0, len=0;  i <  nread; i++) {        
                 ch = inbuf[i];                                   
                 if (ch == LF) {                                   
                     outbuf[len++] = CR;                          
                     outbuf[len++] = LF;  
                    }                        
                 else      
                     outbuf[len++] = ch;                                           
                 last = ch;                                       
              }                                                   
               nwrite = writebuf(fdo, outbuf, len);
           }   while (nread != 0);
      else      /* convert to UNIX */
          do {                                                    
              nwrite = 0;                                         
              nread  = readbuf(fdi, inbuf, (int )sizeof(inbuf));        

              for (i=0, len=0;  i <  nread; i++) {                
                 ch = inbuf[i];                                   
                 if (ch == CR)                                    
                     outbuf[len++] = LF;                          
                 else if (ch != LF || last != CR)	                                 
                     outbuf[len++] = ch;                          
                 last = ch;                                       
               }                                                      
               nwrite = writebuf(fdo, outbuf, len);
           }   while (nread != 0);
}



void lf_convert_c(got_input, rootname, rootlen, filename, namelen, outname, ierr)
     int    *got_input, *rootlen, *namelen, *ierr;
     char   *rootname, *filename, *outname;
{
  char  *inname;
  char   tmpstr[20];
  int    fdi, fdo, pid;

    *ierr = 0;
    if (*got_input == 1) {
    
       inname = (char *) calloc(*namelen+1, sizeof (char)); 
       strncpy(inname,filename,*namelen);
       if ((fdi = open (inname, O_RDONLY,"r")) == -1) {
          fprintf (stderr, "*** ERROR INPUT FILE:   %s NOT FOUND!\n", inname);
          *ierr = 1;
        }
    }
    else {
       fdi = fileno(stdin);
    }
    
    if( *ierr==0 ) {   
    pid = getpid();                                                           
    sprintf(tmpstr,"%d",pid);                                                 
    strcpy(outname, cwd_c());                                              
    if (TODOS)
      strcat(outname, "\\");                                                     
    else
    strcat(outname, "/");  
                                                   
    strncat(outname, rootname, *rootlen);                                     
    strcat(outname, "_");                                                     
    strcat(outname, tmpstr);                                                  
    *namelen =  (int) strlen(outname);                                     

#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
    if ((fdo = _open (outname, _O_CREAT | _O_RDWR, _S_IWRITE)) == -1) {             
      fprintf (stderr, "*** ERROR INPUT FILE: CANNOT CREATE TEMP FILE\n");    
      *ierr = 1;
    }                                                                         
#elif 1
    if ((fdo = open (outname, O_CREAT | O_RDWR,S_IRWXU)) == -1) {             
      fprintf (stderr, "*** ERROR INPUT FILE: CANNOT CREATE TEMP FILE\n");    
      *ierr = 1;
    }                                                                         
#endif
                                                                           
    do_lf_convert (fdi, fdo);
    
    
    if (close(fdi) == -1 || close (fdo) == -1)
       syserr("Error: close");
    }
 }
 
 
void convertfile(FILE *stream,  int level,FILE *stream_out,char *filename,int ncharline,int *ierr){
 char *line,*line1,*tag;
 char *idchr;int id;
 char *newinc;
 int k,val,nlevel,snewinc,linelen;
 FILE *newstream;
 int firstline=1,i,j;
 int iend2=0;
 char *pch,*pch1,*pch2;
 const char *cs = "  ";
   line=(char *)malloc(sizeof(char)* ncharline);
   line1=(char *)malloc(sizeof(char)* ncharline);
   tag=(char *)malloc(sizeof(char)* ncharline);
   idchr=(char *)malloc(sizeof(char)* ncharline);
   newinc=(char *)malloc(sizeof(char)* ncharline);
   if (level > MAXINCLVL) {
       printf("Max level=%d reached\n",level);
       printf("Include file %s skipped.\n",filename);
       return;}
   nlevel = level +1;
/*               
                  123456789012345678901234567890123456789012345678901234567890       */
/*   sprintf(line1,"### include header for parameters # include level=%5d  ### %s\n",nlevel,filename);
*/
//   fputs(line1,stream_out);
   while (fgets(line,ncharline,stream)!=NULL){
     linelen=strlen(line);
     /* delete CR, assume \n at eof, skip blank lines */
     if ( linelen > 1 ) {
       if ( line[linelen-2] == '\r' ) {
         linelen --;
         line[linelen-1] = line[linelen];
         line[linelen] = '\0';
       }
     }
     if ( line[linelen-1] != '\n' ) {
       line[linelen] = '\n';
       linelen++;
       line[linelen] = '\0';
     }
     if (firstline == 1 && strncmp(line,"#",1)==0 && level == 0) {
         fputs(line,stream_out);
     }
     /* remove comment lines */
     else if ( ( strncmp(line,"#",1)==0 && strncmp(line,"#include ",9)==0 && strncmp(line,"#INCLUDE ",9)==0 ) || 
            strncmp(line,"$",1)==0 ) {
         continue ;
     }
     /* add submodel tag     1234567890*/
     else if ( strncmp(line,"//SUBMODEL",10)==0 ){
      nlevel++;
//      sprintf(line1,"### include header for parameters # include level=%5d  ### %s\n",nlevel,filename);
//      fputs(line1,stream_out);
      fputs(line,stream_out);
     }
     else if ( strncmp(line,"//ENDSUB",8)==0 ){
      fputs(line,stream_out);
//      sprintf(line1,"### include footer for parameters # include level=%5d  ### %s\n",nlevel,filename);
//      fputs(line1,stream_out);
      nlevel--;
     }
     else if ( ( (strncmp(line,"/END",4)==0 && strncmp(line,"/END/ENGINE",11)!=0) ||
               strncmp(line,"#enddata",8)==0 ||
               strncmp(line,"#ENDDATA",8)==0 ) &&
               nlevel > 1 ){
       if (strncmp(line,"/END",4)==0) {
	 printf("Warning in include file %s : /END detected !\n",filename);
       }
       return;
     }
     else if ( strncmp(line,"/END",4)==0 &&
               strncmp(line,"/END/ENGINE",11)!=0 ){
      fputs(line,stream_out);
//      if (nlevel > 1) {
//      sprintf(line1,"### include footer for parameters # include level=%5d  ### %s\n",nlevel,filename);
//        fputs(line1,stream_out);
//      }
//      nlevel--;
      if (nlevel == 1) {
//    ALTDOCTAG
	while (fgets(line,ncharline,stream)!=NULL && iend2!=1){
	  if ( strncmp(line,"/ALTDOCTAG",10)==0 ){
	    fputs(line,stream_out);
	    fgets(line,ncharline,stream);
	    fputs(line,stream_out);
	    iend2=1;
	  }
	}
	return;
      } 
     }
     else if( strncmp(line,"/END",4)==0 &&
               strncmp(line,"/END/ENGINE",11)==0 ){
      fputs(line,stream_out);
      free(line);
      free(line1);
      free(tag);
      free(idchr);
      free(newinc);
      return ;
     }
     else {
       fputs(line,stream_out);
     }
     firstline=0;
   }
   if (strncmp(line,"/END",4)==0 ) {
       printf("myline %s \n",line);
   }

//   sprintf(line1,"### include footer for parameters # include level=%5d  ### %s\n",nlevel,filename);
//   fputs(line1,stream_out);
   free(line);
   free(line1);
   free(tag);
   free(idchr);
   free(newinc);
}


void lf_convert_c_flat(got_input, rootname, rootlen, filename, namelen, outname, ierr, ncharline,len_path,path,len_path2,path2)
     int    *got_input, *rootlen, *namelen, *ierr, *ncharline;
     int    *len_path,*len_path2;
     char   *rootname, *filename, *outname, *path, *path2;
{
  char  *inname, *outname_local ;
  char   tmpstr[20];
  int    fdi, fdo, pid, ifclose_in, ifclose_out;
  FILE * stream;
  FILE * stream_out;
#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
  char tmpstr_host[MAX_COMPUTERNAME_LENGTH+1];
  int size_tmpstr_host;
  size_tmpstr_host=MAX_COMPUTERNAME_LENGTH+1;
  WSADATA wsadata;
  WORD version= MAKEWORD(1,1);
  int nRet,le;
#elif 1
  char tmpstr_host[MAXHOSTNAMELEN];
  int size_tmpstr_host;
  size_tmpstr_host=MAXHOSTNAMELEN;
#endif
    ifclose_in=0;
    *ierr = 0;
    if (*got_input == 1) {
      if(*len_path==0)
      {
       inname = (char *) calloc(*namelen+1, sizeof (char)); 
       strncpy(inname,filename,*namelen);
      }
      else
      {
       inname = (char *) calloc(*namelen+1+ *len_path, sizeof (char));
       strncpy(inname,path,*len_path);  
       strncat(inname, filename, *namelen);
      }
//       if ((fdi = open (inname, O_RDONLY,"r")) == -1) {
         stream = fopen(inname,"r");
//       }
       if (stream == NULL) {
          fprintf (stderr, "*** ERROR IN OPENING INPUT FILE : %s !\n", inname);
          *ierr = 1;
       }
    }
    if( *ierr==0 ) {   
    pid = getpid();


#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
  nRet = WSAStartup(version,&wsadata);
  gethostname(tmpstr_host,size_tmpstr_host);
  le=WSAGetLastError();
#elif 1
  gethostname(tmpstr_host,size_tmpstr_host);
#endif
    outname[0] = '\0' ;
    sprintf(tmpstr,"%d",pid); 
    if(*len_path2==0)
    {
    // current working directory   
        outname_local = (char *) calloc(2148, sizeof (char));
        strcpy(outname_local, cwd_c());                                              
        if (TODOS)
          strcat(outname_local, "\\");                                                     
        else
        strcat(outname_local, "/");  
    }
    else
    {
        outname_local = (char *) calloc(*len_path2 +1 + 2148, sizeof (char));
     memset(outname_local, '\0', sizeof(outname_local));
     strncpy(outname_local, path2, *len_path2);    
    // user working directory
    }  
    strncat(outname_local, rootname, *rootlen);                                     
    strcat(outname_local, "_");                                                     
    strcat(outname_local, tmpstr); 
    strcat(outname_local, "_"); 
    strcat(outname_local, tmpstr_host);   
 
    strcat(outname, outname_local);            
    *namelen =  (int) strlen(outname);                                     

    stream_out = fopen(outname,"w");
    if (stream_out == NULL) {
      fprintf (stderr, " *** ERROR INPUT FILE: CANNOT CREATE TEMP FILE : %s !\n",outname);    
      *ierr = 1;
    }
                                                                           
    if (stream != NULL && stream_out != NULL) {
      convertfile(stream,0,stream_out,inname,*ncharline,ierr);

      ifclose_in=fclose(stream);
      ifclose_out=fclose(stream_out);
    
      if (ifclose_in != 0) {
        syserr("Error: close input (flat deck)");
      }
      if (ifclose_out != 0) {
        syserr("Error: close output (flat deck)");
      }
    }
    }
 }

/*-------------------------------------------------------------------------------*/

void _FCALL LF_CONVERT_C_FLAT(got_input, rootname, rootlen, filename, namelen, outname, ierr, ncharline,len_path,path,len_path2,path2)
     int    *got_input, *rootlen, *namelen, *ierr, *ncharline;
     int    *len_path, *len_path2 ;
     char   *rootname, *filename, *outname;
     char   *path, *path2 ;
{	lf_convert_c_flat(got_input, rootname, rootlen, filename, namelen, outname, ierr, ncharline,len_path,path,len_path2,path2);}

void lf_convert_c_flat_(got_input, rootname, rootlen, filename, namelen, outname, ierr, ncharline,len_path,path,len_path2,path2)
     int    *got_input, *rootlen, *namelen, *ierr, *ncharline;
     int    *len_path, *len_path2 ;
     char   *rootname, *filename, *outname;
     char   *path, *path2 ;
{	lf_convert_c_flat(got_input, rootname, rootlen, filename, namelen, outname, ierr, ncharline,len_path,path,len_path2,path2);}

void lf_convert_c_flat__(got_input, rootname, rootlen, filename, namelen, outname, ierr, ncharline,len_path,path,len_path2,path2)
     int    *got_input, *rootlen, *namelen, *ierr, *ncharline;
     int    *len_path, *len_path2 ;
     char   *rootname, *filename, *outname;
     char   *path, *path2 ;
{ lf_convert_c_flat(got_input, rootname, rootlen, filename, namelen, outname, ierr, ncharline,len_path,path,len_path2,path2);}

/*----*/

void _FCALL LF_CONVERT_C(got_input, rootname, rootlen, filename, namelen, outname, ierr)
     int    *got_input, *rootlen, *namelen, *ierr;
     char   *rootname, *filename, *outname;
{	lf_convert_c(got_input, rootname, rootlen, filename, namelen, outname, ierr);}

void lf_convert_c_(got_input, rootname, rootlen, filename, namelen, outname, ierr)
     int    *got_input, *rootlen, *namelen, *ierr;
     char   *rootname, *filename, *outname;
{	lf_convert_c(got_input, rootname, rootlen, filename, namelen, outname, ierr);}

void lf_convert_c__(got_input, rootname, rootlen, filename, namelen, outname, ierr)
     int    *got_input, *rootlen, *namelen, *ierr;
     char   *rootname, *filename, *outname;
{ lf_convert_c(got_input, rootname, rootlen, filename, namelen, outname, ierr);}
/*-------------------------------------------------------------------------------*/
