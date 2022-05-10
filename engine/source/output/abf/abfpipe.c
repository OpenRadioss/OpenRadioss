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
#include <abfpipe.h>
#include <abfname.inc>
/*------------------------------------------------------*/
/*   WINDOWS Flexpipe   */
/*------------------------------------------------------*/
#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt  || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32

void open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files)
int *parent_rd,*parent_wr,*code_abf,*radiossv,*abfv,*nb_of_files;
{
int  pipe1[2], pipe2[2];
int  child_rd, child_wr;
char s1[20],s2[20],s3[20],s4[20],s5[20];
char filename[256], errormessage[256],*alt_home,*arch,*abf_path;
int  status,ppid,my_time;
int  errorcode;

      *code_abf = 0;    /* default return code = Error */
	  ppid = getpid(); 
 
      if(_pipe(pipe1, PIPE_BUF, O_BINARY ) == -1) {
	/*perror("Pipe creation failed");*/
	 *code_abf = -3;
         return;
        }
      if(_pipe(pipe2, PIPE_BUF, O_BINARY ) == -1) {
         /*perror("Pipe creation failed");*/
 	 *code_abf = -3;
         return;
        }

       child_rd  = pipe1[0];               
      *parent_wr = pipe1[1];
      *parent_rd = pipe2[0];
      child_wr  = pipe2[1];
                                           
      sprintf(s1,"%d", child_rd);          
      sprintf(s2,"%d", child_wr);          
      sprintf(s3,"%d", *parent_rd);         
      sprintf(s4,"%d", *parent_wr);        
      sprintf(s5,"%d", ppid);              
         
	  /* Spawn abf_converter process */

       sprintf(filename,"abf_converter_%s",ABFPLATTTF);
       hProcess = spawnl(P_NOWAIT, filename, s1, s2, s3, s4, s5, NULL);
       
       if (hProcess == -1) {
           if ((alt_home = getenv("ALTAIR_HOME"))!= NULL && (arch = getenv("arch"))!= NULL)
           {
              sprintf(filename,"%s/hwsolvers/bin/%s/abf_converter_%s",
                      alt_home,arch,ABFPLATTTF);
              hProcess = spawnl(P_NOWAIT, filename, s1, s2, s3, s4, s5, NULL);
           }
       }

       if (hProcess == -1) {
	 if ((abf_path = getenv("ABF_PATH"))!= NULL )
           {
              sprintf(filename,"%s/abf_converter_%s",
                     abf_path,ABFPLATTTF);
              hProcess = spawnl(P_NOWAIT, filename, s1, s2, s3, s4, s5, NULL);
           }
       }

       if (hProcess == -1) {           
 			if (errno == ENOENT)  errorcode = 1;
			if (errno == ENOEXEC) errorcode = 2;
 			if (errno == ENOMEM)  errorcode = 3;
			if (errno == E2BIG)   errorcode = 4;
			*code_abf = -1;
        }
         
        else {
                        abf_pid=GetProcessId(hProcess);

         
			if (close(child_rd)) { /*syserr("Error closing pipe");*/
                          *code_abf=-3; 
                          return;
			}
			if (close(child_wr)) { /*syserr("Error closing pipe");*/
                          *code_abf=-3; 
                          return;
			}

/*			read code retour du fils  */

			readpuncrypt (*parent_rd, (char *)code_abf ,sizeof(int));

			if (*code_abf != 1) {
/*			  abf_converter error, send termination code to child process */
                          *code_abf=-2;
			  writepuncrypt(*parent_wr, (char *)code_abf ,sizeof(int));
                          *abfv=-1;
			}
			else {
                          *code_abf = 1;
/*			  no error, exchange version info with abf_converter */
			  writepuncrypt(*parent_wr, (char *)radiossv ,sizeof(int));
			  readpuncrypt (*parent_rd, (char *)abfv ,sizeof(int));
                          writepuncrypt(*parent_wr, (char *)nb_of_files ,sizeof(int));
                          readpuncrypt (*parent_rd, (char *)code_abf ,sizeof(int));
			}
			return;
		}
}

/* C/FORTRAN interfacing */
void open_abfpipe(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }

void open_abfpipe_(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }

void open_abfpipe__(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }

void OPEN_ABFPIPE_(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }
void _OPEN_ABFPIPE(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }
void OPEN_ABFPIPE(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }

#elif 1
/*------------------------------------------------------*/
/*   Normal System Flexpipe   */
/*------------------------------------------------------*/

void open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files)
int *parent_rd,*parent_wr,*code_abf,*radiossv,*abfv,*nb_of_files;
{
int  pipe1[2], pipe2[2];
int  child_rd, child_wr;
int my_time;
char s1[20],s2[20],filename[256], errormessage[256],*alt_home,*arch,*abf_path;
 int  status;
/* open pipes  */

        if (pipe(pipe1) == -1 || pipe(pipe2) == -1){
          *code_abf = -3;
          return ;
          /*syserr("abf converter connection: Error creating pipes");*/
          }

         child_rd  = pipe1[0];     
        *parent_wr = pipe1[1];
        *parent_rd = pipe2[0];
         child_wr  = pipe2[1];     

        switch (abf_pid=fork())
        {           
           case (-1):
	    /* syserr("abf converter connection: failed creating process"); */
               *code_abf = -1;
               return;
           case (0):   
/* Radioss junior */
               if (close(*parent_rd) == -1) { /*syserr("Error closing pipe");*/
                          *code_abf=-3; 
                          return;
			}
               if (close(*parent_wr) == -1) { /*syserr("Error closing pipe");*/
                          *code_abf=-3; 
                          return;
			} 
               sprintf(s1,"%d", child_rd);
               sprintf(s2,"%d", child_wr);

               sprintf(filename,"abf_converter_%s",ABFPLATTTF);
               execl(filename,filename,s1,s2,NULL);

               if ( (alt_home = getenv("ALTAIR_HOME"))!= NULL &&(arch = getenv("arch"))!= NULL)
               {
                  sprintf(filename,"%s/hwsolvers/bin/%s/abf_converter_%s",
                          alt_home,arch,ABFPLATTTF);
                  execl(filename,filename,s1,s2,NULL);
               }

               if ( (abf_path = getenv("ABF_PATH"))!= NULL)
               {
                  sprintf(filename,"%s/abf_converter_%s",
                          abf_path,ABFPLATTTF);
                  execl(filename,filename,s1,s2,NULL);
               }

             /*  sprintf(errormessage,
                       "ERROR: failed starting program %s",filename); */
	       /* syserr2(errormessage); */
               *code_abf = -1;
               writepuncrypt(child_wr, (char *)code_abf, sizeof(int));
               readpuncrypt (child_rd, (char *)code_abf, sizeof(int));
               close(child_wr);
	       close(child_rd);
               exit(1);
        }


/*** Radioss parent ***/
        
        if (close(child_rd) == -1) { /* syserr("Error closing pipe");*/
	  *code_abf = -3 ;
          return;
	}
        if (close(child_wr) == -1){ /*  syserr("Error closing pipe");*/
	  *code_abf = -3 ;
          return;
	}

/***      read code retour du fils  ***/
        readpuncrypt   (*parent_rd,  (char *)code_abf ,sizeof(int));

 	 /* pas de abf10 */
         if (*code_abf == -1) {
           *abfv=-1;
           writepuncrypt(*parent_wr, (char *)code_abf ,sizeof(int));
           waitpid(abf_pid,&status,0); 
           return;
	 }

  	 /*wrong code sent by abf10*/
       if (*code_abf != 1) {
/***	  abf10 error, send termination code to child process ***/
           *code_abf=-1;
           writepuncrypt(*parent_wr, (char *)code_abf ,sizeof(int));
           readpuncrypt (*parent_rd, (char *)abfv ,sizeof(int));

           writepuncrypt(*parent_wr, (char *)code_abf ,sizeof(int));
           *abfv=-1;
           *code_abf=-2;
           /*waitpid(abf_pid,&status,0);  */
           return;
           }
        else {
/***       no error, exchange version info with abf ***/
           writepuncrypt(*parent_wr, (char *)radiossv ,sizeof(int));
           readpuncrypt (*parent_rd, (char *)abfv ,sizeof(int));
           writepuncrypt(*parent_wr, (char *)nb_of_files ,sizeof(int));
           readpuncrypt (*parent_rd, (char *)code_abf ,sizeof(int));
        }
}

/* C/FORTRAN interfacing */
void open_abfpipe(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }

void open_abfpipe_(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }

void open_abfpipe__(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }

void OPEN_ABFPIPE_(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }
void _OPEN_ABFPIPE(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }
void OPEN_ABFPIPE(int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files) 
  {
   open_abfpipe_std(parent_rd,parent_wr,code_abf,radiossv,abfv,nb_of_files);
  }

#endif

/***************************************************************************/

void build_abffile_std(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,nb_index_abf)
int *finp,*fout,*code_ret,*ifil,*len,*ifiltmp,*lentmp,*nb_index_abf;
{
char filnam[2148];
char filnam1[2148];
int i;
/***       exchange abf filename info with abf_writer ***/
	   for(i=0;i<*len;filnam[i++]=(char)*ifil++);
	   filnam[i]='\0';
           writepuncrypt(*fout, (char *)filnam ,sizeof(char[100]));
           readpuncrypt (*finp, (char *)code_ret ,sizeof(int));
/***       exchange tmp filename info with abf_writer ***/
	   for(i=0;i<*lentmp;filnam1[i++]=(char)*ifiltmp++);
	   filnam1[i]='\0';
           writepuncrypt(*fout, (char *)filnam1 ,sizeof(char[100]));
           readpuncrypt (*finp, (char *)code_ret ,sizeof(int));
/***       exchange number of index of abf file info with abf_writer ***/
           writepuncrypt(*fout, (char *)nb_index_abf, sizeof(int));
           readpuncrypt (*finp, (char *)code_ret ,sizeof(int));
}




/* C/FORTRAN interfacing */
void build_abffile(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *nb_index_abf) 
  {
   build_abffile_std(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,nb_index_abf);
  }

void build_abffile_(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *nb_index_abf) 
  {
   build_abffile_std(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,nb_index_abf);
  }

void build_abffile__(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *nb_index_abf) 
  {
   build_abffile_std(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,nb_index_abf);
  }

void BUILD_ABFFILE_(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *nb_index_abf) 
  {
   build_abffile_std(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,nb_index_abf);
  }

void BUILD_ABFFILE(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *nb_index_abf) 
  {
   build_abffile_std(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,nb_index_abf);
  }
void _BUILD_ABFFILE(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *nb_index_abf) 
  {
   build_abffile_std(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,nb_index_abf);
  }


/***************************************************************************/
void check_abf_std(finp,fout,code_ret)
int *finp,*fout,*code_ret;
{
           writepuncrypt(*fout, (char *)code_ret ,sizeof(int));
           if (*code_ret == -1){return;} 
           readpuncrypt(*finp, (char *)code_ret ,sizeof(int));
}

/* C/FORTRAN interfacing */
void check_abf(int *finp,int *fout,int *code_ret) 
  {
   check_abf_std(finp,fout,code_ret);
  }

void check_abf_(int *finp,int *fout,int *code_ret) 
  {
   check_abf_std(finp,fout,code_ret);
  }

void check_abf__(int *finp,int *fout,int *code_ret) 
  {
   check_abf_std(finp,fout,code_ret);
  }

void CHECK_ABF_(int *finp,int *fout,int *code_ret) 
  {
   check_abf_std(finp,fout,code_ret);
  }
void _CHECK_ABF(int *finp,int *fout,int *code_ret) 
  {
   check_abf_std(finp,fout,code_ret);
  }
void CHECK_ABF(int *finp,int *fout,int *code_ret) 
  {
   check_abf_std(finp,fout,code_ret);
  }


/***************************************************************************/
void abffile_updatestd(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,CPTFILE)
int *finp,*fout,*code_ret,*ifil,*len,*ifiltmp,*lentmp,*CPTFILE;
{
       int done;
       char filnam[100];
       char filnam1[100];
       int i;

           done=writepuncrypt(*fout, (char *)code_ret ,sizeof(int));


/***       exchange abf filename info with abf_writer ***/
	   for(i=0;i<*len;filnam[i++]=(char)*ifil++);
	   filnam[i]='\0';
           writepuncrypt(*fout, (char *)filnam ,sizeof(char[100]));
           readpuncrypt (*finp, (char *)code_ret ,sizeof(int));
/***       exchange tmp filename info with abf_writer ***/
	   for(i=0;i<*lentmp;filnam1[i++]=(char)*ifiltmp++);
	   filnam1[i]='\0';
           writepuncrypt(*fout, (char *)filnam1 ,sizeof(char[100]));
           readpuncrypt (*finp, (char *)code_ret ,sizeof(int));
/***       exchange  number of the file (for multiple abf output) ***/
           writepuncrypt(*fout, (char *)CPTFILE ,sizeof(int));
           readpuncrypt (*finp, (char *)code_ret ,sizeof(int));



           if(done == -1){printf("\n** ABF_CONVERTER PROGRAM HAD STOPPED\n");}
           if(done == -1){printf("** USE STANDALONE ABF_CONVERTER PROGRAM\n");}
           if(done == -1){printf("** IN ORDER TO CONVERT .tmp to .abf FILE\n");}
           if(done == -1){printf("** USE HELP FOR MORE INFORMATIONS\n");}
           fflush(stdout);
           readpuncrypt(*finp, (char *)code_ret ,sizeof(int));
} 

/* C/FORTRAN interfacing */
void abffile_update(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE) 
  {
   abffile_updatestd(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,CPTFILE);
  }

void abffile_update_(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE) 
  {
   abffile_updatestd(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,CPTFILE);
  }

void abffile_update__(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE) 
  {
   abffile_updatestd(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,CPTFILE);
  }

void ABFFILE_UPDATE_(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE) 
  {
   abffile_updatestd(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,CPTFILE);
  }
void _ABFFILE_UPDATE(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE) 
  {
   abffile_updatestd(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,CPTFILE);
  }
void ABFFILE_UPDATE(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE) 
  {
   abffile_updatestd(finp,fout,code_ret,ifil,len,ifiltmp,lentmp,CPTFILE);
  }


/***************************************************************************/

void release_abfpipe(finp,fout)
int *finp,*fout;
{
int  status,code_ret = -1;
        writep(*fout, (char *)&code_ret ,sizeof(int));
#if CPP_mach != CPP_w95 && CPP_mach != CPP_win64_spmd && CPP_mach != CPP_p4win64_spmd && CPP_mach != CPP_wnt && CPP_mach != CPP_wmr && CPP_mach != CPP_p4win64 && CPP_mach != CPP_p4win32
        waitpid(abf_pid,&status,0); 
#elif 1
        WaitForSingleObject(hProcess,INFINITE);
#endif
        
/*        close(*finp);
          close(*fout); */
}


/* C/FORTRAN interfacing */
void release_abfpipe_(int *finp,int *fout)
{
  release_abfpipe(finp,fout);
}

void _FCALL RELEASE_ABFPIPE(int *finp,int *fout)
{
  release_abfpipe(finp,fout);
}
/***************************************************************************/

static void syserr2 (char *message) 
{ 
   fprintf(stderr, "\n\n%s\n\n", message); 
}

static void syserr (char *message) 
{ 
   fprintf(stderr, "%s\n", message); 
   exit(-1); 
}

/***************************************************************************/

 int readpuncrypt(int pipe, char* buf, int nbytes)
{
 int ncount;
 int done,i,verif;
 char *bufdecrypt, *bufpointer;

 bufpointer=buf;
 ncount = nbytes;

 while (ncount > 0)
	{
		done = read(pipe, (void *)bufpointer, ncount);
		if (done < 0)
                break;
		else if (done == 0)
			break;
		ncount -= done;
		bufpointer += done;
	}

 if (done < 0){
   return -1;
 }

 return(nbytes-ncount);
}

/***************************************************************************/

 int writepuncrypt(int pipe, char* buf, int nbytes)
{
  int ncount, done;
  char * bufcrypt, *bufpointer;

  bufpointer = buf;

  ncount = nbytes;

  while (ncount > 0)
  {
    done = write(pipe, (void *)bufpointer,ncount);
    if (done <= 0)
       break;
       ncount -= done;
       bufpointer += done;
  }
    if (done < 0) return -1;
    return(nbytes-ncount);
}

/***************************************************************************/

void c_get_abfname(name, len)
  char * name;
  int * len;
{
  sprintf(name,"abfconvert_%s",ABFPLATTTF);
  *len=strlen(name);
}


void get_abfname(name, len)
   char* name;
   int* len;
{
  c_get_abfname(name, len);
}

void _FCALL GET_ABFNAME(name, len)
     char * name;
     int * len;
{
  c_get_abfname(name, len);
}


void get_abfname_(name, len)
     char * name;
     int * len;
{
  c_get_abfname(name, len);
}


void get_abfname__(name, len)
     char * name;
     int * len;
{
  c_get_abfname(name,len);
}

