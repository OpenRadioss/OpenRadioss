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
#include <hardware.inc>


#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#ifdef _WIN64 
#include <windows.h>
#include <process.h>
#include <io.h>
#define _FCALL 

HANDLE  hProcess;

#elif 1

#include <unistd.h>
#include <sys/wait.h>
#include <sys/times.h>
#include <sys/param.h>
#define _FCALL

int hProcess;

#endif



#ifndef PIPE_BUF
#define	PIPE_BUF	1024
#endif
 

int abf_pid;

int write_abf_converter(int pipe, char* buf, int nbytes);
int read_abf_converter(int pipe, char* buf, int nbytes);

void open_abfpipe        (int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files);
void open_abfpipe_       (int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files);
void open_abfpipe__      (int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files);
void _FCALL OPEN_ABFPIPE (int *parent_rd,int *parent_wr,int *code_abf,int *radiossv,int *abfv,int *nb_of_files);

void build_abffile(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *nb_index_abf);
void build_abffile_(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *nb_index_abf);
void _FCALL BUILD_ABFFILE(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *nb_index_abf);

void check_abf(int *finp,int *fout,int *code_ret); 
void check_abf_(int *finp,int *fout,int *code_ret); 
void check_abf__(int *finp,int *fout,int *code_ret); 
void _FCALL CHECK_ABF(int *finp,int *fout,int *code_ret); 



#ifdef _WIN64


void abffile_update_std(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE); 
void abffile_update(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE); 
void abffile_update_(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE); 
void abffile_update__(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE); 
void ABFFILE_UPDATE_(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE); 
void _ABFFILE_UPDATE(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE);
void ABFFILE_UPDATE(int *finp,int *fout,int *code_ret,int *ifil,int *len,int *ifiltmp,int *lentmp,int *CPTFILE);

void init_abfpipe (int *fdinp,int *fdout ,int *req,int *ok,char *altdoctag,int *taglen,char *checksum,int *checksumlen);
void init_abfpipe_(int *fdinp,int *fdout ,int *req,int *ok,char *altdoctag,int *taglen,char *checksum,int *checksumlen);
void _FCALL INIT_ABFPIPE(int *fdinp,int *fdout ,int *req,int *ok,char *altdoctag,int *taglen,char *checksum,int *checksumlen);


void release_abfpipe(int *finp,int *fout);
void release_abfpipe_(int *finp,int *fout);
void _FCALL RELEASE_ABFPIPE(int *finp,int *fout);


static void syserr(char *message);
static void syserr2(char *message);
static int readpuncrypt(int pipe, char* buf, int nbytes);
static int writepuncrypt(int pipe, char* buf, int nbytes);


#endif


