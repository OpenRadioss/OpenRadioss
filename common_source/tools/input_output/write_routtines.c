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
#include <zlib.h>


#ifdef _WIN64

#include <process.h>
#include <io.h>
#include <sys\types.h>
#include <sys/stat.h>
#include <time.h>
#include <zlib.h>


#elif 1
#include <sys/resource.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

#define _FCALL

void integer_to_IEEE_ASCII(int entier,unsigned char octet[4]);
void IEEE_ASCII_to_integer(int *entier,unsigned char octet[4]);

void real_to_IEEE_ASCII(float reel,unsigned char octet[4]);
void IEEE_ASCII_to_real(float *reel,unsigned char octet[4]);

void double_to_IEEE_ASCII(double *reel,unsigned char octet[1000][8],int len);
void IEEE_ASCII_to_double(double *reel,unsigned char octet[1000][8],int len);

// functions prototypes from this file
// -------------------------------------
void file_init_();
void open_c(int *ifil,int *len,int *mod);
void close_c();

void fseek_c_rd(int * lseek);
void fseek_end_c(int * lseek);
void file_size(int * filesize);
void filelen_sys(char * ffilnam,int *len,int * size);
void cur_fil_c(int *nf);
void eor_c(int *len);

void write_r_c(float *w,int *len);
void write_db_c(double *w,int *len);
void write_c_c(int *w,int *len);
void write_c_c_txt(char *w,int *len);
void write_i_c(int *w,int *len);
void write_s_c(int *w, int *len);

void read_db_c(double *w,int *len);
void read_r_c(float *w, int *len);
void read_i_c(int *w, int *len);
void read_c_c(int *w,int *len);
void flu_fil_c();

void arret_(int *n);
void ARRET(int *n);
void arret_c(int n);

// Compress gzip functions
int Compress_Buffer(unsigned char* input, int inputSize, unsigned char* output, int outputSize);
void write_buffer(unsigned char *buf,int type_size,int length);
void close_buffer();
void delete_tmpfile_(char *name, int *size);


// Some Defines / common variables
// --------------------------------
#define BUFLEN 256
#define ZSUFFIX  0
#define GZSUFFIX 1

#define COMP_BUFFER_SIZE 10240

//    ----------------------------------------    //
//    FILE pointer already used :                 //
//    0-->4                                       //
//    20 : reopen the _0001.rst in order to       //
//         add the real elapsed time              //
//    31-->41                                     //
//    42-->42+n for sectio (but I don't known     //
//    the value of n)                             //
//    ----------------------------------------    //
FILE *outfile[100];     // List of FILE descriptors.
int outfile_mod[100];   // Git the compression Mode : 0 no compression, 1 compress, 2 uncompress
unsigned char *compress_write_buffer[100];
gzFile compress_read_buffer[100];
int compress_buffer_length[100];
FILE *curfile;          // FILE pointer to current file  call to cur_fil_c function set this.
int cur_nf;             // ID to current FILE descriptor call to cur_fil_c function set this.

// Threadprivate: Starter Restarts
#pragma omp threadprivate(outfile,outfile_mod,compress_write_buffer,compress_buffer_length,curfile,cur_nf,compress_read_buffer)

// -------------------------------------------------------
void arret_c(int n)
//! --------------------------------------------------
//! Calls the Fortran Arret routine for termination
//! --------------------------------------------------
//! n is the error termination
{
/*Appel a une routine Fortran */
#ifdef _WIN64
 ARRET(&n);
#else
 arret_(&n);
#endif
}

// -------------------------------------------------------------------------------------

void file_init_(){
//! --------------------------------------------------
//! Initialize the file pointers to NULL
//! Initialize file mode to Zero
//! --------------------------------------------------
// 
  int i;
  for (i=0;i<100;){
    outfile[i]=NULL;
    outfile_mod[i]=0;
  }
}

void file_init(){
  int i;
  for (i=0;i<100;){
    outfile[i]=NULL;
    outfile_mod[i]=0;
  }
}

void _FCALL FILE_INIT()
{
  int i;
  for (i=0;i<100;){
    outfile[i]=NULL;
    outfile_mod[i]=0;
  }
}

// -------------------------------------------------------------------------------------

void open_c(int *ifil,int *len,int *mod)
//! --------------------------------------------------
//! Opens a file accoding to mod. 
//! ifil : filename stored as integer Ascii values
//! len : size of ifil
//! mod=
//!      Read  : 1 (unzipped), 4, 7 (zipped)
//!      Write : 0 (unzipped), 3, 6 (gzipped)
//!      Read/Write : 2 (unzipped) 
//! --------------------------------------------------
{
  char *filnam;
  int i;
  gzFile gzstream;

  #ifdef _WIN64 
  /* Windows in Binary format */
    _set_fmode(_O_BINARY);
  #endif

  // Create char* filnam
  filnam = (char*) malloc(sizeof(char)*(*len+10) );


  //printf("Mod:%i\n",*mod);

  for(i=0;i<*len;i++) filnam[i]=(char)ifil[i];
  filnam[*len]='\0';

  // Global buffers initialization
  outfile_mod[cur_nf]=0;                       // Saves compress mode (0 : non, 1 gzip, 2 gunzip)
  compress_write_buffer[cur_nf]=NULL;          // Intermediate buffer to store the data to compress
  compress_buffer_length[cur_nf]=0;            // Size of data to compress

  if (*mod == 1 || *mod == 4 || *mod == 7){    // Open Read : 4 & 7 are gziped

     if (*mod !=1){                            // unzip
      #ifdef _WIN64
         strcat_s(filnam,*len+10,".gz");
      #else
         strcat(filnam,".gz");
      #endif
      outfile_mod[cur_nf]=2;
      gzstream = gzopen(filnam,"r");
      if (gzstream == NULL){
          printf(" ** ERROR: FILE %s NOT FOUND\n",filnam); 
          /*force flush buffer to avoid missing error message on windows in some cases*/
          fflush(stdout);
          arret_c(2);
      }
      gzbuffer(gzstream, COMP_BUFFER_SIZE);
      curfile=(FILE *)gzstream;

     }else{                                    // Not zipped file

       #ifdef _WIN64
          fopen_s(&curfile,filnam,"r");
       #else
          curfile=fopen(filnam,"r");
       #endif

       if (!curfile){
            printf(" ** ERROR: FILE %s NOT FOUND\n",filnam); 
            /*force flush buffer to avoid missing error message on windows in some cases*/
            fflush(stdout);
            arret_c(2);
       }
     }
  }
  if (*mod==0 || *mod == 3 ||*mod==6 ){    // Open Write : 0 unzipped 3 & 6 are gziped
     if(*mod!=0){                          //  zip
     #ifdef _WIN64
         strcat_s(filnam,*len+10,".gz");
      #else
         strcat(filnam,".gz");
      #endif
       outfile_mod[cur_nf]=1;
       compress_write_buffer[cur_nf]=(unsigned char *)malloc(COMP_BUFFER_SIZE+2048);   // Create a buffer of COMP_BUFFER_SIZE + 2k (gzip header) for compression
                                                                                       // Several buffers can be opened at same time.
     }
     #ifdef _WIN64
        fopen_s(&curfile,filnam,"w");
     #else
        curfile=fopen(filnam,"w");
     #endif
  }

  if (*mod==2) {                           // Open Read/Write : uncompressed
     #ifdef _WIN64
        fopen_s(&curfile,filnam,"r+");
     #else
        curfile=fopen(filnam,"r+");
     #endif
  }
  outfile[cur_nf] = curfile;
  free(filnam);
}

// C/Fortran interface 
void _FCALL OPEN_C(int *ifil,int *len,int *mod)
{ open_c(ifil,len,mod);}

void open_c_(int *ifil,int *len,int *mod)
{ open_c(ifil,len,mod); }

void open_c__(int *ifil,int *len,int *mod)
{ open_c(ifil,len,mod); }

// -------------------------------------------------------------------------------------

void close_c()
//! Close the file
//! ---------------
{
  close_buffer();     // In case of gzip - flush the remaining values in buffer
  fclose(curfile);
}
void _FCALL CLOSE_C()
{ close_c(); }

void close_c_()
{ close_c(); }

void close_c__()
{ close_c();}

// -------------------------------------------------------------------------------------

void fseek_c_rd(int * lseek){
//! Determine file size
//! -----------------------------------
//! output : lseek - current file_size
//! -----------------------------------
  fseek(curfile,*lseek,SEEK_CUR);
}

void fseek_c_rd_(int * lseek){
  fseek_c_rd(lseek);
}
   
void fseek_c_rd__(int * lseek){
  fseek_c_rd(lseek);
}
   
void _FCALL FSEEK_C_RD(int * lseek){
  fseek_c_rd(lseek);
}
   
void fseek_end_c(int * lseek){
//! Determine file size
//! --------------------------------
//! output : lseek - at end of file
//! --------------------------------
  fseek(curfile,*lseek,SEEK_END);
}

void fseek_end_c_(int * lseek){
  fseek_end_c(lseek);
}
  
void _FCALL FSEEK_END_C(int * lseek){
  fseek_end_c(lseek);
}


void file_size_(int * filesize)
//! Determine file size with ftell
//! -------------------------------
//! output : size in kb.
//! -------------------------------
{
  long sz;
  sz=ftell(curfile);
  *filesize=sz/1024;
}

void file_size(int * filesize){
   file_size_(filesize);
}
   
void file_size__(int * filesize){
   file_size_(filesize);
}
   
void _FCALL FILE_SIZE(int * filesize){
   file_size_(filesize);
}


void file_size_c_(long *size)
//! Determine file size with ftell
//! -------------------------------
//! output : size in kb as long.
//! -------------------------------
{
  *size= ftell(curfile)/1024;
}

void  _FCALL FILE_SIZE_C(long *size)
{
  *size= ftell(curfile) / 1024;
}
/* --------------------- */


// -------------------------------------------------------------------------------------


#ifdef _WIN64

void filelen_sys_(char * ffilnam,int *len,int * size)
//! Determine file using system call / _stat in Windows
//! ----------------------------------------------------
//! char * ffilnam : filename can come from Fortran
//! int *len : length of filename
//! int size : output : size in kb as long.
//! ----------------------------------------------------
{
  char * filnam;
  char command[256],line[256];
  char var1[128],var2[128],var3[128],fnam[128],ssize[128];
  int i,found,ret;
  struct _stat statbuf;

  filnam = malloc(*len+10);
  for (i=0;i<*len;i++) { filnam[i]=ffilnam[i]; }
  filnam[*len]='\0';

  ret = _stat (filnam,&statbuf);
  if (ret != -1){
    *size = statbuf.st_size / 1024;
  }else{
    *size = 0;
  }

  free(filnam);

}

#else
void filelen_sys_(char * ffilnam,int *len,int * size)
//! Determine file using system call / _stat in Windows
//! ----------------------------------------------------
//! char * ffilnam : filename can come from Fortran
//! int *len : length of filename
//! int size : output : size in kb as long.
//! ----------------------------------------------------
{
  char * filnam;
  char command[256],line[256];
  char var1[128],var2[128],var3[128],var4[128],ssize[128];
  int i,ret;
  struct stat statbuf;
  FILE * scall;
  filnam = malloc(*len+10);
  for (i=0;i<*len;i++) { filnam[i]=ffilnam[i]; }
  filnam[*len]='\0';
  
  ret = stat (filnam,&statbuf);
  if (ret != -1){
    *size = statbuf.st_size / 1024;
  }else{
    *size = 0;
  }
  free(filnam);
  
}
#endif

void filelen_sys(char * ffilnam,int *len,int * size){ 
   filelen_sys_(ffilnam,len, size);
}

void filelen_sys__(char * ffilnam,int *len,int * size){ 
   filelen_sys_(ffilnam,len, size);
}

void _FCALL FILELEN_SYS(char * ffilnam,int *len,int * size){ 
   filelen_sys_(ffilnam,len, size);
}

// -------------------------------------------------------------------------------------
void cur_fil_c(int *nf)
//! Set the current file stored in outfile array
//! ---------------------------------------------
//! intput : Integer, file ID
//! ---------------------------------------------
{
  cur_nf = *nf;
  curfile = outfile[*nf];
}

void _FCALL CUR_FIL_C(int *nf)
{ cur_fil_c(nf);}

void cur_fil_c_(int *nf)
{ cur_fil_c(nf);}

void cur_fil_c__(int *nf)
{ cur_fil_c(nf); }

// -------------------------------------------------------------------------------------

void eor_c(int *len)
//! Writes the current size in file : Radioss IEEE Format, in TH / NOISE
//! ---------------------------------------------------------------------
//! intput : Integer, size
//! ---------------------------------------------------------------------
{
  int i;
  unsigned char octet[4];

  integer_to_IEEE_ASCII(*len,octet);
  write_buffer(octet,sizeof(unsigned char),4);
}
void _FCALL EOR_C(int *len)
{ eor_c(len);}

void eor_c_(int *len)
{ eor_c(len);}


// -------------------------------------------------------------------------------------

void write_r_c(float *w,int *len)
//! Writes a float array in file : Radioss IEEE Format.
//! ----------------------------------------------------
//! w, input : float array to write
//! len, input : Integer, size
//! ----------------------------------------------------
{
  int i,j,k,block;
  unsigned char buf[BUFLEN*4];

  for(k=0;k<*len;k+=BUFLEN){
    block = ((*len-k) < BUFLEN)?(*len-k):BUFLEN;
    for(i=0;i<block;i++){
      real_to_IEEE_ASCII(w[i+k],&buf[i*4]);
    }

    //fwrite(buf,sizeof(unsigned char),block*4,curfile);
    write_buffer(buf,sizeof(unsigned char),block*4);
  }
}


void _FCALL WRITE_R_C(float *w,int *len)
{
  write_r_c(w,len);
}

void write_r_c_(float *w,int *len)
{
  write_r_c(w,len);
}

// -------------------------------------------------------------------------------------

void write_db_c(double *w,int *len)
//! Write a double array in file : Radioss IEEE Format.
//! ----------------------------------------------------
//! w, input : double array to write
//! len, intput : Integer, size
//! ----------------------------------------------------
{
  int i,j;
  unsigned char octet[1000][8];

  if (*len > 1000){
      printf(" ** ERROR: BAD SIZE FOR WRITING\n");
       arret_c(2);
  }   
  double_to_IEEE_ASCII(w,octet,*len);
  //fwrite(octet,sizeof(char),(*len)*8,curfile); 
  write_buffer((unsigned char*)octet,sizeof(char),(*len)*8);
}
        
void _FCALL WRITE_DB_C(double *w,int *len)
{write_db_c(w,len);}

void write_db_c_(double *w,int *len)
{ write_db_c(w,len);}

void write_db_c__(double *w,int *len)
{write_db_c(w,len);}

// -------------------------------------------------------------------------------------

void write_c_c(int *w,int *len)
//! Write a character array in ASCII in file
//! -----------------------------------------
//! w, input : integer array to write
//! len, intput : Integer, size
//! -----------------------------------------
{
  int i;
  for(i=0;i<*len;i++)  write_buffer((unsigned char *)&w[i],sizeof(char),1);
}

void _FCALL WRITE_C_C(int *w,int *len)
{ write_c_c(w,len);}
void write_c_c_(int *w,int *len)
{write_c_c(w,len);}

void write_c_c__(int *w,int *len)
{write_c_c(w,len);}

// -------------------------------------------------------------------------------------

void write_c_c_txt(char *w,int *len)
//! Write a string in file 
//! --------------------------------------
//! w, input : integer array to write
//! len, intput : Integer, size
//! --------------------------------------
{
  char *buf;
  int i,mod;
  buf=(char*)malloc(sizeof(char)*(*len+2));

 // Buffer may come from Fortran : Need to recopy it & add \n + \0 in
  for(i=0;i<*len;i++)buf[i]=w[i];
  buf[*len]='\n';
  buf[*len+1]='\0';

  mod = outfile_mod[cur_nf];

  printf("%s",buf);
  write_buffer((unsigned char*)buf,sizeof(unsigned char),*len+1);
  free (buf);
}
void _FCALL WRITE_C_C_TXT(char *w,int *len)
{ write_c_c_txt(w,len);}

void write_c_c_txt_(char *w,int *len)
{ write_c_c_txt(w,len);}


void write_c_c_txt__(char *w,int *len)
{ write_c_c_txt(w,len);}

// -------------------------------------------------------------------------------------

void write_i_c(int *w,int *len)
//! Write a integer array of size len in Radioss IEEE format
//! ---------------------------------------------------------
//! w, input : integer array to write
//! len, intput : Integer, size
//! ---------------------------------------------------------
{
  int i,j,k,block;
  unsigned char buf[BUFLEN*4];

  for(k=0;k<*len;k+=BUFLEN){
    block = ((*len-k) < BUFLEN)?(*len-k):BUFLEN;
    for(i=0;i<block;i++){
      integer_to_IEEE_ASCII(w[i+k],&buf[i*4]);
    }
    write_buffer(buf,sizeof(unsigned char),block*4);
  }
}

void _FCALL WRITE_I_C(int *w,int *len)
{write_i_c(w,len);}

void write_i_c_(int *w,int *len)
{ write_i_c(w,len); }


void write_i_c__(int *w,int *len)
{write_i_c(w,len);}

// -------------------------------------------------------------------------------------
void write_s_c(int *w, int *len)
//! Write a string array as Integer in Radioss IEEE format
//! ---------------------------------------------------------
//! w, input : integer array to write
//! len, intput : Integer, size
//! ---------------------------------------------------------
{
  int i,j;
  unsigned char octet[4];

  for(j=0;j<*len;j++){
    integer_to_IEEE_ASCII(w[j],octet);
    for(i=2;i<4;i++) write_buffer(&octet[i],sizeof(unsigned char),1);
  }
}

void _FCALL WRITE_S_C(int *w, int *len)
{ write_s_c(w,len); }

void write_s_c_(int *w, int *len)
{ write_s_c(w,len); }

// -------------------------------------------------------------------------------------

void read_db_c(double *w,int *len){
//! Read a double precision array in Radioss IEEE format
//! -----------------------------------------------------
//! w, input : double array to read
//! len, intput : Integer, size
//! -----------------------------------------------------
  int i, j, c,nitems;
  unsigned char octet[1000][8];
  gzFile gzstream;

  if (*len > 1000){                                             // Read is done in blocks of 1000 floats
      printf(" ** ERROR: BAD SIZE FOR READING\n");
      arret_c(2);
  }

  if(outfile_mod[cur_nf]==0){                      // Read unzipped
    if(fread(octet,sizeof(char),(*len)*8,curfile)!=(*len)*8){
      printf(" ** ERROR: END OF FILE DURING READING\n");
      arret_c(2);
    }
  }else{                                          // Read Zipped
     gzstream=(gzFile)curfile;
     nitems=gzfread(octet,sizeof(unsigned char)*8,*len,gzstream);
     if (nitems <*len ){
      printf(" ** ERROR: END OF FILE DURING READING\n");
      arret_c(2);
     }
  }
  IEEE_ASCII_to_double(w,octet,*len);      
}

void _FCALL READ_DB_C(double *w,int *len)
{ read_db_c(w,len); } 

void read_db_c_(double *w,int *len)
{ read_db_c(w,len); }


// -------------------------------------------------------------------------------------
void read_r_c(float *w, int *len)
//! Read a real array in Radioss IEEE format
//! -----------------------------------------
//! w, input : real array to read
//! len, intput : Integer, size
//! -----------------------------------------
{
  int i, j, k, block, n,nitems;
  unsigned char buf[4*BUFLEN];
  unsigned char * gzbuf;
  gzFile gzstream;
  
  if(outfile_mod[cur_nf]==0){                      // Read unzipped
    for(k=0;k<*len;k+=BUFLEN){                     // Read is done according to blocks of BUFLEN (256 characters)
      block = ((*len-k) < BUFLEN)?(*len-k):BUFLEN;
      n = fread(buf,sizeof(unsigned char),block*4,curfile);
      if (n!=block*4){
        printf(" ** ERROR: END OF FILE DURING READING\n");
        for(i=0;i<n/4;i++){
          IEEE_ASCII_to_real(&w[i+k],&buf[4*i]);
        }
        w[k+n/4]=-1.;
      }
      for(i=0;i<block;i++){
        IEEE_ASCII_to_real(&w[i+k],&buf[4*i]);
      }
    }
  }else{                                          // Read Zipped
      gzstream=(gzFile)curfile;
      gzbuf=(unsigned char *)malloc(sizeof(unsigned char)* *len*4);  // Read & decompress is done in one chunk
      nitems=gzfread(buf,sizeof(unsigned char),*len*4,gzstream);
      if (nitems != *len*4){
        printf(" ** ERROR: END OF FILE DURING READING\n");
      }
      for(i=0;i<*len;i++){
        IEEE_ASCII_to_real(&w[i],&gzbuf[4*i]);
      }
      free(gzbuf);
  }
} /* fin read_r_c */


void _FCALL READ_R_C(float *w, int *len)
{ read_r_c(w,len);} /* fin READ_R_C  */

void read_r_c_(float *w, int *len)
{ read_r_c(w,len); } /* fin read_r_c_ */

// -------------------------------------------------------------------------------------
void read_i_c(int *w, int *len)
//! Read an integer array in Radioss IEEE format
//! ---------------------------------------------
//! w, input : integer array to read
//! len, intput : Integer, size
//! ---------------------------------------------
{
  int i, j, k, block,nitems;
  unsigned char buf[4*BUFLEN];
  unsigned char * gzbuf;
  gzFile gzstream;

  if(outfile_mod[cur_nf]==0){                        // Read unzipped
    for(k=0;k<*len;k+=BUFLEN){
      block = ((*len-k) < BUFLEN)?(*len-k):BUFLEN;   // Read is done in blocks of 256 items
      if (fread(buf,sizeof(unsigned char),block*4,curfile)!=block*4){
         printf(" ** ERROR: END OF FILE DURING READING\n");
         arret_c(2);
      }
      for(i=0;i<block;i++){
        IEEE_ASCII_to_integer(&w[i+k],&buf[4*i]);
      }
    }
  }else{
      gzstream=(gzFile)curfile;
      gzbuf=(unsigned char *)malloc(sizeof(unsigned char)* *len*4);    // Read is done in one chunk
      nitems=gzfread(gzbuf,sizeof(int),*len,gzstream);
      if (nitems != *len){                                           
        printf(" ** ERROR: END OF FILE DURING READING\n");
      }
      for(i=0;i<*len;i++){
        IEEE_ASCII_to_integer(&w[i],&gzbuf[4*i]);
      }
      free(gzbuf);
  }
}

void _FCALL READ_I_C(int *w, int *len)
{ read_i_c(w,len);} /* READ_I_C */

void read_i_c_(int *w, int *len)
{ read_i_c(w,len);} 

// -------------------------------------------------------------------------------------
void read_c_c(int *w, int *len)
//! Read an caracter array coded in ASCII/INT format
//! -------------------------------------------------
//! w, input : integer array to read
//! len, intput : Integer, size
//! -------------------------------------------------
{
  int j,nitems;
  unsigned char * gzbuf;
  gzFile gzstream;
    
  if(outfile_mod[cur_nf]==0){                      // Read unzipped  
       
    for(j=0;j<*len;j++){
      if((w[j] = (int) getc(curfile)) == EOF){
           printf(" ** ERROR: END OF FILE DURING READING\n");
           arret_c(2);
      }
    }
  }else{
      gzstream=(gzFile)curfile;
      gzbuf=(unsigned char *)malloc(sizeof(unsigned char)* *len);
      nitems=gzfread(gzbuf,1,*len,gzstream);
      if(nitems < *len){
           printf(" ** ERROR: END OF FILE DURING READING\n");
           arret_c(2);
      }
      for (j=0;j<*len;j++) w[j]=(int)gzbuf[j];
      free(gzbuf);
  }
} 

void _FCALL READ_C_C(int *w, int *len)
{ read_c_c(w,len); } 


void read_c_c_(int *w, int *len)
{ read_c_c(w,len);} 


void read_c_c__(int *w, int *len)
{ read_c_c(w,len);} 

// -------------------------------------------------------------------------------------
void flu_fil_c(){
//! flush the current file to disc
//! Care has no meaning when file is to gzip
//! -----------------------------------------
 fflush(curfile);
}

void _FCALL FLU_FIL_C(){
  flu_fil_c();
}

void flu_fil_c_(){
  flu_fil_c();
}


void flu_fil_c__()
{flu_fil_c();}

// -------------------------------------------------------------------------------------
int Compress_Buffer(unsigned char* input, int inputSize, unsigned char* output, int outputSize){
//! Compress Buffer : Takes a character array, return it as gzipped array
//! ----------------------------------------------------------------------
//! input : input character array
//! inputSize : lenght of this array
//! output : gziped array
//! outputSize: zise of outout array
//! ----------------------------------------------------------------------
    z_stream zs;
    zs.zalloc = Z_NULL;
    zs.zfree = Z_NULL;
    zs.opaque = Z_NULL;
    zs.avail_in = (uInt)inputSize;
    zs.next_in = (Bytef *)input;
    zs.avail_out = (uInt)outputSize;
    zs.next_out = (Bytef *)output;

    //printf("input: %i\n",inputSize);
    // hard to believe they don't have a macro for gzip encoding, "Add 16" is the best thing zlib can do:
    // "Add 16 to windowBits to write a simple gzip header and trailer around the compressed data instead of a zlib wrapper"
    deflateInit2(&zs, Z_DEFAULT_COMPRESSION, Z_DEFLATED, 15 | 16, 8, Z_DEFAULT_STRATEGY);
    deflate(&zs, Z_FINISH);
    deflateEnd(&zs);
    return zs.total_out;
//  *out_len = compress2(out_buffer,&lout_len,in_buffer,lin_len,level);
}


// -------------------------------------------------------------------------------------
void write_buffer(unsigned char *buf,int type_size,int length){
//! write Buffer : writes a buffer either direct or gziped.
//! When Gzip : stores it in a buffer of COMP_BUFFER_SIZE
//! When Buffer Full : gzip it & write
//! ----------------------------------------------------------------------
//! buf : unsigned char, array to write. 
//! type_size : the buf array is an array of a type : gives the size of type
//! length : number of item of type type_size un buf to write
//! ----------------------------------------------------------------------
  int to_do;
  int i,len,inputSize,compressed_buf_size,compressed_len,mod;
  unsigned char *compressed_buf;
  len=type_size*length;
  i=0;
  to_do=1;

  mod=outfile_mod[cur_nf];
  if( mod == 0){                                      // Write in non zipped format
      fwrite(buf,sizeof(unsigned char),len,curfile);
  }else{                                              // Write in gzip format

    while( to_do==1 ){                                // while buf has not been treated

      // Copy arrays in a Buffer of COM_BUFFER_SIZE 
      while(compress_buffer_length[cur_nf] < COMP_BUFFER_SIZE && i< len){
        compress_write_buffer[cur_nf][compress_buffer_length[cur_nf] ] = buf[i];
         i++;
         compress_buffer_length[cur_nf]++;
      }

      if (compress_buffer_length[cur_nf] == COMP_BUFFER_SIZE ){
        // COM_BUFFER_SIZE  Buffer is full. Compress & write on disk
        compressed_buf=(unsigned char *)malloc(sizeof(unsigned char)*(COMP_BUFFER_SIZE+2048));
        inputSize=compress_buffer_length[cur_nf];
        compressed_buf_size=COMP_BUFFER_SIZE+2048;
        compressed_len = Compress_Buffer(compress_write_buffer[cur_nf], inputSize , compressed_buf, compressed_buf_size);
        fwrite(compressed_buf,sizeof(unsigned char),compressed_len,curfile);

        free(compressed_buf);
        compress_buffer_length[cur_nf]=0;
      }

      if (i==len) to_do=0;
    }
  }
}



// -------------------------------------------------------------------------------------
void close_buffer(){
//! ----------------------------------------------------------------------
//! Close the file in uncompressed mode
//! Compress last buffers, write & close the file/
//! ----------------------------------------------------------------------
  int mod,len,compressed_len,compressed_buf_len;
  unsigned char *compressed_buf;

  mod=outfile_mod[cur_nf];
  if( mod != 0){
     len=compress_buffer_length[cur_nf];
     if (len > 0){

         compressed_buf=(unsigned char *)malloc(sizeof(unsigned char)*(len+2048));
         compressed_buf_len=len+2048;
         compressed_len = Compress_Buffer(compress_write_buffer[cur_nf], len , compressed_buf, compressed_buf_len);
         fwrite(compressed_buf,sizeof(unsigned char),compressed_len,curfile);
     }
    free(compress_write_buffer[cur_nf]);                // Compress buffer is freed 
    compress_buffer_length[cur_nf]=0;                   // it length is set to zero
  }
}

// -------------------------------------------------------------------------------------
void delete_tmpfile_(char *name, int *size)
//! ----------------------------------------------------------------------
//! Delete a temp file from Radioss
//! Routine can be used from Fortran, it copies name in an another string
//! and add '\0' to it.
//! ----------------------------------------------------------------------
//! char * name : string containing filename to delete
//! int * size  : size of string
//! ----------------------------------------------------------------------

{
  char *cname;
  int cname_len;
  int i;
  cname_len = *size + 1;
  cname=(char*) malloc(sizeof(char)*cname_len);
  for(i=0;i<*size;i++)  cname[i] = name[i]; 
  cname[*size]='\0';
  fclose(curfile);
  remove(cname);
  free(cname);
}
void _FCALL DELETE_TMPFILE (char *name, int *size)
{delete_tmpfile_ (name,size);}

void delete_tmpfile__ (char *name, int *size)
{delete_tmpfile_ (name,size);}

void delete_tmpfile (char *name, int *size)
{delete_tmpfile_ (name,size);}




