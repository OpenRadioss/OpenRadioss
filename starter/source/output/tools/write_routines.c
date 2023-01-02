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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>
#include <malloc.h>

#ifdef _WIN64 

#include <process.h>
#include <io.h>
#include <sys\types.h>
#include <sys/stat.h>
#define _FCALL 

#else

#include <sys/resource.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>
#define _FCALL

#endif


#define BUFLEN 256


#ifndef HYPERMESH_LIB
void integer_to_IEEE_ASCII(int entier,unsigned char octet[4]);
void IEEE_ASCII_to_integer(int *entier,unsigned char octet[4]);

void real_to_IEEE_ASCII(float reel,unsigned char octet[4]);
void IEEE_ASCII_to_real(float *reel,unsigned char octet[4]);

void double_to_IEEE_ASCII(double *reel,unsigned char octet[1000][8],int len);
void IEEE_ASCII_to_double(double *reel,unsigned char octet[1000][8],int len);



/**************************************************************************/
/**************************************************************************/
/*******            ecriture lecture .Z .gz            ***********/
/**************************************************************************/
/**************************************************************************/
#define ZSUFFIX  0
#define GZSUFFIX 1
/*********************************************/


#ifdef _WIN64
static FILE *UCompressedGZRead (fileName)
     char   *fileName;
{
  int     p[2], fdc;
  int     hProcess, fdStdIn,fdStdOut;
  FILE   *unCompressedFile;

  if ((fdc = open (fileName, O_RDONLY)) == -1) {
    fprintf (stderr, "ERROR: Cannot open file %s to read!\n", fileName);
    return NULL;
  }
  if (_pipe (p, 512, O_NOINHERIT ) == -1) {
    fprintf (stderr, "ERROR: Cannot open pipe for gunzip... !\n");
    return NULL;
  }

  
  /* duplique stdin */
  fdStdIn = _dup(_fileno(stdin));
  close(_fileno(stdin));
  
  /* duplique fdc en tant que stdin */
  _dup2(fdc, _fileno(stdin));
  
  /* duplique stdout */
  fdStdOut = _dup(_fileno(stdout));
  close(_fileno(stdout));
  
  /* duplique piperead en tant que stdout */
  _dup2(p[1], _fileno(stdout));


  
  hProcess = _spawnlp(P_NOWAIT,"gzip","-c","-d",NULL);
  
  /* reset stdout */
  _dup2(fdStdOut, _fileno(stdout));
   close(fdStdOut);
  /* reset stdin */
  _dup2(fdStdIn, _fileno(stdin));
   close(fdStdIn);

  if(hProcess == -1) {
      fprintf (stderr, "ERROR: Cannot spawn to execute uncompress...\n");
      exit (1);
      }
  else{			/* main process */
      if (!(unCompressedFile = fdopen (p[0], "r"))) {
	fprintf (stderr, "ERROR: Cannot fdopen pipe!\n");
	return NULL;
      }
      close (p[1]);
      close (fdc);
      return unCompressedFile;
  }
  return NULL;
}

#else 

static FILE *UCompressedGZRead (fileName)
     char   *fileName;
{
  int     p[2], fdc;
  int     pid;
  FILE   *unCompressedFile;

  if ((fdc = open (fileName, O_RDONLY)) == -1) {
    fprintf (stderr, "ERROR: Cannot open file %s to read!\n", fileName);
    return NULL;
  }
  if (pipe (p)) {
    fprintf (stderr, "ERROR: Cannot open pipe for gzip... !\n");
    return NULL;
  }
  switch (pid = vfork ()) {
  case -1:{			/* failed fork() */
      fprintf (stderr, "ERROR: Cannot vfork to execute gzip...\n");
      return NULL;
    }
  case 0:{			/* child process */
      close (0);
      dup (fdc);
      close (1);
      dup (p[1]);
      close (p[0]);
      close (fdc);
      close (p[1]);
      if ((execlp ("gzip", "gzip", "-c", "-d", (char *) 0)) == -1) {
	fprintf (stderr, "ERROR: Cannot find gzip in your PATH...\n");
        exit (1);
      }
      break;
    }
  default:{			/* main process */
      if (!(unCompressedFile = fdopen (p[0], "r"))) {
	fprintf (stderr, "ERROR: Cannot fdopen pipe for compression!\n");
	return NULL;
      }
      close (p[1]);
      close (fdc);
      return unCompressedFile;
    }
  }
  return NULL;
}
#endif

/*
  ----------------------------------------------------------------
*/

#ifdef _WIN64
static FILE *UCompressedZRead (fileName)
     char   *fileName;
{
  int     p[2], fdc;
  int     hProcess, fdStdIn,fdStdOut;
  FILE   *unCompressedFile;

  if ((fdc = open (fileName, O_RDONLY)) == -1) {
    fprintf (stderr, "ERROR: Cannot open file %s to read!\n", fileName);
    return NULL;
  }
  if (_pipe (p, 512, O_NOINHERIT ) == -1) {
    fprintf (stderr, "ERROR: Cannot open pipe for uncompress... !\n");
    return NULL;
  }
/* duplique stdout */
  fdStdOut = _dup(_fileno(stdout));
/* duplique piperead en tant que stdout */
  _dup2(p[0], _fileno(stdout));

/* duplique stdin */
  fdStdIn = _dup(_fileno(stdin));
/* duplique fdc en tant que stdin */
  _dup2(fdc, _fileno(stdin));

  hProcess = spawnlp(P_NOWAIT, "gzip.exe -dc", NULL);
/* reset stdout */
  _dup2(fdStdOut, _fileno(stdout));
   close(fdStdOut);
/* reset stdin */
  _dup2(fdStdIn, _fileno(stdin));
   close(fdStdIn);

  if(hProcess == -1) {
      fprintf (stderr, "ERROR: Cannot spawn to execute uncompress...\n");
      exit (1);
      }
  else{
      if (!(unCompressedFile = fdopen (p[0], "r"))) {
	fprintf (stderr, "ERROR: Cannot fdopen pipe!\n");
	return NULL;
      }
      close (p[1]);
      close (fdc);
      return unCompressedFile;
  }
  return NULL;
}

#else

static FILE *UCompressedZRead (fileName)
     char   *fileName;
{
  int     p[2], fdc;
  int     pid;
  FILE   *unCompressedFile;

  if ((fdc = open (fileName, O_RDONLY)) == -1) {
    fprintf (stderr, "ERROR: Cannot open file %s to read!\n", fileName);
    return NULL;
  }
  if (pipe (p)) {
    fprintf (stderr, "ERROR: Cannot open pipe for uncompress... !\n");
    return NULL;
  }
  switch (pid = vfork ()) {
  case -1:{			/* failed fork() */
      fprintf (stderr, "ERROR: Cannot vfork to execute uncompress...\n");
      return NULL;
    }
  case 0:{			/* child process */
      close (0);
      dup (fdc);
      close (1);
      dup (p[1]);
      close (p[0]);
      close (fdc);
      close (p[1]);
      if ((execlp ("uncompress", "uncompress", "-c", (char *) 0)) == -1) {
	fprintf (stderr, "ERROR: Cannot find uncompress in your PATH...\n");
	exit (1);
      }
      break;
    }
  default:{			/* main process */
      if (!(unCompressedFile = fdopen (p[0], "r"))) {
	fprintf (stderr, "ERROR: Cannot fdopen pipe for compression!\n");
	return NULL;
      }
      close (p[1]);
      close (fdc);
      return unCompressedFile;
    }
  }
  return NULL;
}

#endif


/*
  ----------------------------------------------------------------------------------------------------------------
*/
#ifdef _WIN64 
/* fork et pipe non disponible sur pc */
int fork()
{
  return -1;
}

int pipe(p)
int p;
{
  return -1;
}

int vfork()
{
  return -1;
}

#endif



/*
  ----------------------------------------------------------------------------------------------------------------
*/
#ifdef _WIN64
static FILE *UCompressedGZCreate (char   *fileName)
{
  int     p[2], fdc;
  int     hProcess, fdStdIn,fdStdOut;
  FILE   *compressedFile, *nonCompressedFile;

  if ((fdc = open (fileName, O_WRONLY | O_CREAT, 0666)) == -1) {
    fprintf (stderr,"UCompressedFCreate: cannot open file %s for creation!\n",fileName);
    return NULL;
  }

  if (!(nonCompressedFile = fdopen (fdc, "w"))) {
    fprintf (stderr,"UCompressedFCreate: cannot fdopen file %s for creation!\n", fileName);
    return NULL;
  }

  if (_pipe (p, 512, O_NOINHERIT ) == -1) {
    fprintf (stderr,"UCompressedFCreate: cannot open pipe for compression... sorry file won't be compressed!\n");
    return nonCompressedFile;
  }

  /* duplique stdout */
  fdStdOut = _dup(_fileno(stdout));
  /* duplique fdc en tant que stdout */
  _dup2(fdc, _fileno(stdout));

  /* duplique stdin */
  fdStdIn = _dup(_fileno(stdin));
  /* duplique piperead en tant que stdin */
  _dup2(p[0], _fileno(stdin));

  hProcess = spawnlp(P_NOWAIT, "gzip","-c",NULL);
  /* reset stdout */
  _dup2(fdStdOut, _fileno(stdout));
  /* rajout fermeture fdStdOut */
   close(fdStdOut);
  /* reset stdin */
  _dup2(fdStdIn, _fileno(stdin));
  /* rajout fermeture fdStdIn */
   close(fdStdIn);

  if(hProcess == -1) {
      fprintf (stderr, "UCompressedFCreate: cannot spawn to execute gzip... sorry file won't be compressed!\n");
      return nonCompressedFile;
    }
  else{
      if (!(compressedFile = fdopen (p[1], "w"))) {
	fprintf (stderr, "UCompressedFCreate: cannot fdopen pipe!\n");
	return NULL;
      }
      close (p[0]);
      close (fdc);
      return compressedFile;
    }
  return NULL;
}

#else

static FILE *UCompressedGZCreate (fileName)
     char   *fileName;
{
  int     p[2], fdc;
  int     pid;
  FILE   *compressedFile, *nonCompressedFile;

  if ((fdc = open (fileName, O_WRONLY | O_CREAT, 0666)) == -1) {
    fprintf (stderr,"UCompressedFCreate: cannot open file %s for creation!\n",fileName);
    return NULL;
  }

  if (!(nonCompressedFile = fdopen (fdc, "w"))) {
    fprintf (stderr,"UCompressedFCreate: cannot fdopen file %s for creation!\n", fileName);
    return NULL;
  }

  if (pipe (p)) {
    fprintf (stderr,"UCompressedFCreate: cannot open pipe for compression... sorry file won't be compressed!\n");
    return nonCompressedFile;
  }

  switch (pid = vfork ()) {
  case -1:{			/* failed fork() */
      fprintf (stderr,"UCompressedFCreate: cannot vfork to execute gzip... sorry file won't be compressed!\n");
      return nonCompressedFile;
    }
  case 0:{			/* child process */
      close (0);
      dup (p[0]);
      close (1);
      dup (fdc);
      close (p[0]);
      close (fdc);
      close (p[1]);
      if ((execlp ("gzip", "gzip", "-c", (char *) 0)) == -1) {
	fprintf (stderr,"UCompressedFCreate: cannot find gzip in your PATH... sorry this won't work! (file %s will stay empty)\n", fileName);
	exit (1);
      }
      break;
    }
  default:{			/* main process */
      if (!(compressedFile = fdopen (p[1], "w"))) {
	fprintf (stderr, "UCompressedFCreate: cannot fdopen pipe!\n");
	return NULL;
      }
      close (p[0]);
      close (fdc);
      return compressedFile;
    }
  }
  return NULL;
}

#endif



#ifdef _WIN64
static FILE *UCompressedZCreate (fileName)
     char   *fileName;
{
  int     p[2], fdc;
  int     hProcess, fdStdIn,fdStdOut;
  FILE   *compressedFile, *nonCompressedFile;

  if ((fdc = open (fileName, O_WRONLY | O_CREAT, 0666)) == -1) {
    fprintf (stderr, "UCompressedFCreate: cannot open file %s for creation!\n",fileName);
    return NULL;
  }

  if (!(nonCompressedFile = fdopen (fdc, "w"))) {
    fprintf (stderr, "UCompressedFCreate: cannot fdopen file %s for creation!\n", fileName);
    return NULL;
  }

  if (_pipe (p, 512, O_NOINHERIT ) == -1) {
    fprintf (stderr, "UCompressedFCreate: cannot open pipe for compression... sorry file won't be compressed!\n");
    return nonCompressedFile;
  }

/* duplique stdout */
  fdStdOut = _dup(_fileno(stdout));
/* duplique fdc en tant que stdout */
  _dup2(fdc, _fileno(stdout));

/* duplique stdin */
  fdStdIn = _dup(_fileno(stdin));
/* duplique piperead en tant que stdin */
  _dup2(p[0], _fileno(stdin));

  hProcess = spawnlp(P_NOWAIT, "gzip","-c",NULL);
/* reset stdout */
  _dup2(fdStdOut, _fileno(stdout));
   close(fdStdOut);
/* reset stdin */
  _dup2(fdStdIn, _fileno(stdin));
   close(fdStdIn);

  if(hProcess == -1) {
      fprintf (stderr, "UCompressedFCreate: cannot spawn to execute compress... sorry file won't be compressed!\n");
      return nonCompressedFile;
    }
  else{
      if (!(compressedFile = fdopen (p[1], "w"))) {
	fprintf (stderr, "UCompressedFCreate: cannot fdopen pipe!\n");
	return NULL;
      }
      close (p[0]);
      close (fdc);
      return compressedFile;
    }
  return NULL;
}

#else

static FILE *UCompressedZCreate (fileName)
     char   *fileName;
{
  int     p[2], fdc;
  int     pid;
  FILE   *compressedFile, *nonCompressedFile;

  if ((fdc = open (fileName, O_WRONLY | O_CREAT, 0666)) == -1) {
    fprintf (stderr, "UCompressedFCreate: cannot open file %s for creation!\n",fileName);
    return NULL;
  }

  if (!(nonCompressedFile = fdopen (fdc, "w"))) {
    fprintf (stderr, "UCompressedFCreate: cannot fdopen file %s for creation!\n", fileName);
    return NULL;
  }

  if (pipe (p)) {
    fprintf (stderr, "UCompressedFCreate: cannot open pipe for compression... sorry file won't be compressed!\n");
    return nonCompressedFile;
  }

  switch (pid = vfork ()) { 
  case -1:{			/* failed fork() */
      fprintf (stderr, "UCompressedFCreate: cannot vfork to execute compress... sorry file won't be compressed!\n");
      return nonCompressedFile;
    }
  case 0:{			/* child process */
      close (0);
      dup (p[0]);
      close (1);
      dup (fdc);
      close (p[0]);
      close (fdc);
      close (p[1]);
      if ((execlp ("compress", "compress", "-c", (char *) 0)) == -1) {
	fprintf (stderr, "UCompressedFCreate: cannot find compress in your PATH... sorry this won't work! (file %s will stay empty)\n",fileName);
	exit (1);
      }
      break;
    }
  default:{			/* main process */
      if (!(compressedFile = fdopen (p[1], "w"))) {
	fprintf (stderr, "UCompressedFCreate: cannot fdopen pipe!\n");
	return NULL;
      }
      close (p[0]);
      close (fdc);
      return compressedFile;
    }
  }
  return NULL;
}

#endif


/* ---------------------------------------------- */
/* Compression : reading, writing .Z or .gz files */
/* ---------------------------------------------- */
/* open a file for compressed writing or uncompressed reading
   type: "w" -> write / type: "r" -> read */

FILE *UCompressedFOpen (char   *fileName, char *type, int suffix)
{
  switch (suffix) {
  case ZSUFFIX:
    if (!strcmp (type, "w"))
      return UCompressedZCreate (fileName);
    else if (!strcmp (type, "r"))
      return UCompressedZRead (fileName);
    else
      fprintf (stderr,"UCompressedFOpen: error, can be used only for \"r\" or \"w\" modes!\n");
    break;
  case GZSUFFIX:
    if (!strcmp (type, "w"))
      return UCompressedGZCreate (fileName);
    else if (!strcmp (type, "r"))
      return UCompressedGZRead (fileName);
    else
      fprintf (stderr,"UCompressedFOpen: error, can be used only for \"r\" or \"w\" modes!\n");
    break;
  }
  return NULL;
}

/* ---------------------------------------------- */
/* restart writing */
/* ---------------------------------------------- */
FILE *outfile[3],*curfile;
int cur_nf;

/*in parallel each restart on one thread writes on one specific file*/
/*If not each restart works on same file*/
#pragma omp threadprivate(outfile,curfile,cur_nf)



#ifdef _WIN64 
#define open_c_ OPEN_C
#endif
void open_c_(int *ifil,int *len,int *mod)
{
	char filnam[100];
	int i;

#ifdef _WIN64 
        /* mode binaire */
        _fmode = _O_BINARY;
#endif

	for(i=0;i<*len;filnam[i++]=(char)*ifil++);
	filnam[i]='\0';

        if (*mod==0)                    /* mod = 0 ouverture write */
            curfile=fopen(filnam,"w");
        else if (*mod==1)               /* ouverture read */
        {
          curfile=fopen(filnam,"r");
          if (!curfile) 
          {
            printf("\n ** ERROR: FILE %s NOT FOUND\n\n",filnam);
            exit(-1);
          }
        }
        else if (*mod==2)               /* ouverture read/write */
            curfile=fopen(filnam,"r+");
        else if (*mod==3)               /* ouverture write Z */
        {
	    filnam[i++]='.';
	    filnam[i++]='Z';
	    filnam[i++]='\0';
            curfile=UCompressedFOpen(filnam,"w",ZSUFFIX);
        }
        else if (*mod==4)               /* ouverture read Z */
        {
	    filnam[i++]='.';
	    filnam[i++]='Z';
	    filnam[i++]='\0';
            curfile=UCompressedFOpen(filnam,"r",ZSUFFIX);
        }
        else if (*mod==6)               /* ouverture write Z */
        {
	    filnam[i++]='.';
	    filnam[i++]='g';
	    filnam[i++]='z';
	    filnam[i++]='\0';
            curfile=UCompressedFOpen(filnam,"w",GZSUFFIX);
        }
        else if (*mod==7)               /* ouverture read Z */
        {
	    filnam[i++]='.';
	    filnam[i++]='g';
	    filnam[i++]='z';
	    filnam[i++]='\0';
            curfile=UCompressedFOpen(filnam,"r",GZSUFFIX);
        }
	outfile[cur_nf] = curfile;
}



/* --------------------- */
/* File size for Restart */
/* --------------------- */
void file_size_c_(long *size)
{
  *size= ftell(curfile) / 1024;
}

void  _FCALL FILE_SIZE_C(long *size)
{
  *size= ftell(curfile) / 1024;
}
/* --------------------- */


/* ---------- */
/* close file */
/* ---------- */
void close_c_(){
	fclose(curfile);
}

void _FCALL CLOSE_C(){
  	close_c_();
}



/* ---------------- */
/* set current file */
/* ---------------- */
void cur_fil_c_(int *nf){
  cur_nf = *nf;
  curfile = outfile[*nf];
}

void _FCALL CUR_FIL_C(int *nf)
{
  cur_fil_c_(nf);
}


void eor_c_(len)
int *len;
{
	int i;
	unsigned char octet[4];

	integer_to_IEEE_ASCII(*len,octet);
	for(i=0;i<4;putc(octet[i++],curfile));
}
void _FCALL EOR_C(len)
int *len;
{
	eor_c_(len);
}




/* ------------------ */
/*  writing routines  */
/* ------------------ */
void write_r_c(float *w,int *len)
{
	int i,j,k,block;
	unsigned char buf[BUFLEN*4];

	for(k=0;k<*len;k+=BUFLEN){
	  block = ((*len-k) < BUFLEN)?(*len-k):BUFLEN;
	  for(i=0;i<block;i++){
	    real_to_IEEE_ASCII(w[i+k],&buf[i*4]);
	  }
	  fwrite(buf,sizeof(unsigned char),block*4,curfile);
	}
}


void _FCALL WRITE_R_C(w,len)
int *len;
float *w;
{
	write_r_c(w,len);
}
void write_r_c_(w,len)
int *len;
float *w;
{
	write_r_c(w,len);
}

void write_db_c(w,len)
int *len;
double *w;
{
        int i,j;
        unsigned char octet[1000][8];

        if (*len > 1000)
        {
           printf(" ** ERROR: BAD SIZE FOR WRITING\n");
           exit(-1);
        }
        double_to_IEEE_ASCII(w,octet,*len);
        fwrite(octet,sizeof(char),(*len)*8,curfile);

}
     
void _FCALL WRITE_DB_C(w,len)
int *len;
double *w;
{
        write_db_c(w,len);
}

void write_db_c_(w,len)
int *len;
double *w;
{
        write_db_c(w,len);
}

void write_c_c(w,len)
int *len, *w;
{
	int i;

	for(i=0;i<*len;i++) putc((unsigned char)*w++,curfile);
}
void _FCALL WRITE_C_C(w,len)
int *len, *w;
{
	write_c_c(w,len);
}
void write_c_c_(w,len)
int *len, *w;
{
	write_c_c(w,len);
}


void write_i_c(w,len)
int *len, *w;
{
	int i,j,k,block;
	unsigned char buf[BUFLEN*4];

	for(k=0;k<*len;k+=BUFLEN){
	  block = ((*len-k) < BUFLEN)?(*len-k):BUFLEN;
	  for(i=0;i<block;i++){
	    integer_to_IEEE_ASCII(w[i+k],&buf[i*4]);
	  }
	  fwrite(buf,sizeof(unsigned char),block*4,curfile);
	}
}

void _FCALL WRITE_I_C(w,len)
int *len, *w;
{
	write_i_c(w,len);
}
void write_i_c_(w,len)
int *len, *w;
{
	write_i_c(w,len);
}

void read_db_c(w,len)
double *w;
int *len;
{
         int i, j, c;
         unsigned char octet[1000][8];

         if (*len > 1000)
         {
            printf(" ** ERROR: BAD SIZE FOR READING\n");
            exit(-1);
         }
         if(fread(octet,sizeof(char),(*len)*8,curfile)!=(*len)*8)
         {
            printf(" ** ERROR: END OF FILE DURING READING\n");
            exit(-1);
         }
         IEEE_ASCII_to_double(w,octet,*len);

} /* fin read_db_c */

void _FCALL READ_DB_C(w,len)
double *w;
int *len;
{
          read_db_c(w,len);
} /* fin READ_DB_C  */

void read_db_c_(w,len)
double *w;
int *len;
{
          read_db_c(w,len);
} /* fin read_db_c_ */

void read_i_c(w,len)
int *w, *len;
{
         int i, j, c;
         unsigned char octet[4];
         
         for(j=0;j<*len;j++)
         {
            for(i=0;i<4;i++)
            {
               if((c = getc(curfile)) == EOF)
               {
                 printf(" ** ERROR: END OF FILE DURING READING\n");
                 exit(-1);
               }
               octet[i] = c;
            }
            IEEE_ASCII_to_integer(&w[j],octet);
         }
} /* fin read_i_c */

void _FCALL READ_I_C(w,len)
int *w, *len;
{
         read_i_c(w,len);
} /* READ_I_C */

void read_i_c_(w,len)
int *w, *len;
{
         read_i_c(w,len);
} /* fin read_i_c_ */

void read_c_c(w,len)
int *w;
int *len;
{
         int j;

         for(j=0;j<*len;j++)
         {
            if((w[j] = (int) getc(curfile)) == EOF)
            {
               printf(" ** ERROR: END OF FILE DURING READING\n");
               exit(-1);
            }
         }
            
} /* fin read_c_c */


void _FCALL READ_C_C(w,len)
int *w;
int *len;
{
         read_c_c(w,len);
} /* fin READ_C_C */

void read_c_c_(w,len)
int *w;
int *len;
{
         read_c_c(w,len);
} /* fin read_c_c_ */
 
void read_c_c_txt(int *w,int *endfile,int *charline)
{
         int i,j;
         i = 0;
         for(j=0;j<105;j++)
         {
            if(i == 0)
            {
               w[j] = (int)getc(curfile);
               *charline = *charline + 1;
               if(w[j] == EOF)
               {
                  *endfile = 1;
                  *charline = *charline - 1;
               }
               if((char)w[j] == '\n')
               {
                  i = 1;
                  *charline = *charline - 1;
               }
            }
         }
}

void _FCALL READ_C_C_TXT(int *w,int *endfile,int *charline)
{
         read_c_c_txt(w,endfile,charline);
}

void read_c_c_txt_(int *w,int *endfile,int *charline)
{
         read_c_c_txt(w,endfile,charline);
}
void read_c_c_txt__(int *w,int *endfile,int *charline)
{ 
         read_c_c_txt(w,endfile,charline);
} 
/* fin read_c_c_txt */


void write_s_c(w,len)
int *len, *w;
{
        int i,j;
        unsigned char octet[4];

        for(j=0;j<*len;j++){
                integer_to_IEEE_ASCII(w[j],octet);
                for(i=2;i<4;putc(octet[i++],curfile));
        }
}
void _FCALL WRITE_S_C(w,len)
int *len, *w;
{
        write_s_c(w,len);
}
void write_s_c_(w,len)
int *len, *w;
{
        write_s_c(w,len);
}




#endif



#ifndef HYPERMESH_LIB


void delete_tmpfile_(char *name, int *size)
{
    char *cname;
    int cname_len;
    int i;
    cname_len = *size + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size;i++)  cname[i] = name[i]; 
    cname[*size]='\0';
    remove(cname);
    free(cname);
}
void _FCALL DELETE_TMPFILE (char *name, int *size)
{delete_tmpfile_ (name,size);}

void delete_tmpfile__ (char *name, int *size)
{delete_tmpfile_ (name,size);}

void delete_tmpfile (char *name, int *size)
{delete_tmpfile_ (name,size);}

#endif

