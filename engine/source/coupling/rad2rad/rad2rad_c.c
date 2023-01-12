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

#include "pipes_c.inc"
#include "my_real_c.inc"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>

#ifdef _WIN64

#include <sys/types.h>
#include <sys/stat.h>
#include <io.h>
#include <process.h>
#include <errno.h>
#include <signal.h>
#include <winsock2.h>
#include <Winbase.h>
#include <windows.h>
#include <ws2tcpip.h>
#include <ctype.h>
#include <stdio.h> 
#include <tchar.h>
#include <strsafe.h>
#pragma comment(lib, "Ws2_32.lib")

// Dummy signals definition
#define SIGHUP   1
#define SIGQUIT  3
#define SIGBUS 10
#define SIGPIPE 13
#define _FCALL 

#define send_type char*

#elif 1

#include <semaphore.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <signal.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <time.h>
#include <sched.h>
#define _FCALL

#define send_type void*

#endif


#define FALSE   0
#define TRUE   1
#define SERV_TCP_PORT1   18347
#define SERV_TCP_PORT0   18344

/**************************************************************************/

#define PERM        0640
#define    ROOTLEN     80
#define    NAMESIZE    252
#define BUFSIZE         3*PIPE_BUF
#define buftop          "top"
#define MAX_LEN 10000
#define my_max(a,b)(a>=b?a:b)

struct region {
    my_real_c *fx_buf;
    my_real_c *fr_buf;
    my_real_c *sx_buf;
    my_real_c *sr_buf;
    my_real_c *vx_buf;
    my_real_c *vr_buf;
    my_real_c *mass_buf;
    my_real_c *iner_buf;
    my_real_c *iner_rby_buf;
    double *dx_buf;
    int *buf;
    int *itagr;
    int *itagr2;
    int *itags;
    int *itags2;
    int *tagelr;
    int *tagels;
    int *iactv;
};


struct region *rptr,*rptw,*com,*com2;

char fifo_1[NAMESIZE], fifo_2[NAMESIZE], semaphore_int[NAMESIZE];    /* private fifos */
char root[ROOTLEN],add_shmv[NAMESIZE];

static int iroddl,off_link,id_dom,nthreads,ispmd_glob,nspmd,proc_id,r2r_id;
static int *shmv,shmvr_size,shmvs_size;
static int *flagrot,**masterdb;
static int i7flag = 0, flag_siu= 0, flg_sphinout = 0, compt_resize = 0;
static int nbufvar = PIPE_BUF / sizeof(my_real_c);
static my_real_c fx_buf[BUFSIZE];
static my_real_c fr_buf[BUFSIZE];
static my_real_c sx_buf[PIPE_BUF];
static my_real_c sr_buf[PIPE_BUF];
static my_real_c vx_buf[BUFSIZE];
static my_real_c vr_buf[BUFSIZE];
static double dx_buf[4*BUFSIZE];
static my_real_c ms_buf[PIPE_BUF];
static my_real_c in_buf[PIPE_BUF];
static my_real_c in_rby_buf[3*BUFSIZE];


#ifdef _WIN64 
  HANDLE *sem_int;
  HANDLE shmidv;
  HANDLE fidw, fidr;
  HANDLE handler_array[2];
#elif 1
  sem_t sem_int,*sem_glob;
  int shmidv;
  static int fidw, fidr;
#endif  

/*---------------------------------------------------------------------*/
#ifdef _WIN64
int readr(HANDLE pipe, char *buf, int nbytes);
int writer(HANDLE pipe, char*buf, int nbytes);
#else
int readr(int pipe, char *buf, int nbytes);
int writer(int pipe   , char *buf, int nbytes);
#endif

extern void syserr(char *msg);
extern void syserr_fatal(char *msg);
extern void fatal(char *msg);
extern void catch_sig_c(int *pid);
/**************************************************************************/

void openfifo_c(iroot,len,fdw,fdr,sd,ispmd,nthr,ppid)
int *iroot, *len, *fdw, *fdr, *sd, *ispmd, *nthr, *ppid;
{
int i, naps, sock,val;
void syserr();
char messtop[512];

        nthreads = *nthr;
        ispmd_glob = *ispmd;
#ifdef _WIN64
        *ppid = _getpid();
#elif 1
        *ppid = getpid();
#endif

    for (i = 0; i < *len; i++)
        root[i] = (char) iroot[i];
    root[i] = '\0';

#ifdef _WIN64
     sprintf(fifo_1,"\\\\.\\pipe\\Fifo_%d_%s_1",*ispmd,root);
     sprintf(fifo_2,"\\\\.\\pipe\\Fifo_%d_%s_2",*ispmd,root);
#elif 1
     sprintf(fifo_1,"Fifo_%d_%s_1", *ispmd, root);
     sprintf(fifo_2,"Fifo_%d_%s_2", *ispmd, root);             
#endif

#ifdef _WIN64
      sock = *sd;
      if (recv(sock,(send_type)messtop,3*sizeof(char), 0) == -1){
                    perror("recv top fifo");
                    exit(1);}        
      fidr = CreateFile((const char*)&fifo_1,GENERIC_READ,0,NULL,OPEN_EXISTING,0,NULL);
      if (fidr == INVALID_HANDLE_VALUE) 
         fatal("Can't open fifo 1");
      fidw = CreateFile((const char*)&fifo_2,GENERIC_WRITE,0,NULL,OPEN_EXISTING,0,NULL);
      if (fidw == INVALID_HANDLE_VALUE) 
         fatal("Can't open fifo 2");
      if (send(sock,(send_type)buftop,3*sizeof(char), 0) == -1){
                    perror("send top fifo");
                    exit(1);}

     handler_array[0]=fidw;
     handler_array[1]=fidr;
     *fdw = 0;
     *fdr = 1;
#elif 1            
     for (naps = 0; naps < 15; naps++)
    {
        if ((fidr = open(fifo_1, O_RDONLY)) != -1) break;        
        else if (errno ==  ENOENT) sleep(1);
        else
            fatal("Radioss external link: can't open fifo");
    }

    if (fidr == -1)
        fatal("Can't open fifo after 15 naps");
                    
    if ((fidw = open(fifo_2, O_WRONLY)) == -1)
        fatal("Radioss external link: can't open fifo");
    *fdw = (int)fidw;
    *fdr = (int)fidr;
#endif
    if (*ispmd == 0) printf(" MULTIDOMAINS COUPLING-  \n");
      
        /********************opening of semaphore**********************/
#ifdef _WIN64
        sem_int = CreateSemaphore(NULL,0,128,NULL);
    if (sem_int == NULL) {printf("error creation semahpore %lu\n",GetLastError());}
#elif 1                  
        if (flag_siu==0) if (sem_init(&sem_int, 0, 0) == -1) perror("error creation semahpore");
#endif          
}


void _FCALL OPENFIFO_C(iroot,len,fdw,fdr,sd,ispmd,nthr,ppid)
int *iroot, *len, *fdw, *fdr, *sd, *ispmd, *nthr, *ppid;
{
    openfifo_c(iroot,len,fdw,fdr,sd,ispmd,nthr,ppid);
}
void openfifo_c_(iroot,len,fdw,fdr,sd,ispmd,nthr,ppid)
int *iroot, *len, *fdw, *fdr, *sd, *ispmd, *nthr, *ppid;
{
    openfifo_c(iroot,len,fdw,fdr,sd,ispmd,nthr,ppid);
}
void openfifo_c__(iroot,len,fdw,fdr,sd,ispmd,nthr,ppid)
int *iroot, *len, *fdw, *fdr, *sd, *ispmd, *nthr, *ppid;
{openfifo_c(iroot,len,fdw,fdr,sd,ispmd,nthr,ppid);}

/******************************************************************************/

void opensem_c(iroot,len,ispmd,nthr,ppid)
int *iroot, *len, *ispmd, *nthr, *ppid;
{
int i, naps, sock,val;
void syserr();
char messtop[512];

     flag_siu = 1;
     nthreads = *nthr;
 
     for (i = 0; i < *len; i++) root[i] = (char) iroot[i];
     root[i] = '\0';

     val = 0;
     sprintf(semaphore_int,"Sema_int_%d_%s_%d", val, root, *ppid);
     
        /********************opening of semaphore**********************/
#ifdef _WIN64
        sem_int = CreateSemaphore(NULL,0,128,semaphore_int);
    if (sem_int == NULL) {printf("error creation semahpore %lu\n",GetLastError());}        
#elif 1
        if (*ispmd == 0) sem_glob = sem_open(semaphore_int, O_CREAT | O_EXCL, 0644, 0); 
        else sem_glob = sem_open(semaphore_int, 0);        
        if (sem_glob == SEM_FAILED) {perror("error semaphore engine");exit(1);}
#endif
}


void _FCALL OPENSEM_C(iroot,len,ispmd,nthr,ppid)
int *iroot, *len, *ispmd, *nthr, *ppid;
{
    opensem_c(iroot,len,ispmd,nthr,ppid);
}
void opensem_c_(iroot,len,ispmd,nthr,ppid)
int *iroot, *len, *ispmd, *nthr, *ppid;
{
    opensem_c(iroot,len,ispmd,nthr,ppid);
}
void opensem_c__(iroot,len,ispmd,nthr,ppid)
int *iroot, *len, *ispmd, *nthr, *ppid;
{opensem_c(iroot,len,ispmd,nthr,ppid);}

/******************************************************************************/
void openshm_c()
{
int global_len,global_leni,local_len,offset,offseti,mclo,bid;
int *shmi;
my_real_c *shm;
char add_shm[NAMESIZE],add_shmi[NAMESIZE];
#ifdef _WIN64
HANDLE shmid,shmidi;
#elif 1
int shmid,shmidi;
#endif

void syserr();

         shmvr_size = 150;
         shmvs_size = 150;

         readr(fidr, (void *) &offset, sizeof(int));
         readr(fidr, (void *) &offseti, sizeof(int));
         readr(fidr, (void *) &global_len, sizeof(int));
         readr(fidr, (void *) &global_leni, sizeof(int));
         readr(fidr, (void *) &local_len, sizeof(int));
         readr(fidr, (void *) &r2r_id, sizeof(int));
         readr(fidr, (void *) &proc_id, sizeof(int));
         readr(fidr, (void *) &flg_sphinout, sizeof(int));
         sprintf(add_shm,"Adress_shm_%d_%d",ispmd_glob,r2r_id);                   
         sprintf(add_shmi,"Adress_shmi_%d_%d",ispmd_glob,r2r_id);
         sprintf(add_shmv,"Adress_shmv_%d_%d_%d",proc_id,ispmd_glob,r2r_id);
     
        /***********************shared memory**************************/
#ifdef _WIN64
         shmid = CreateFileMapping (INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, global_len*sizeof(my_real_c), add_shm);
         shm = MapViewOfFile(shmid, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0);
         shmidi = CreateFileMapping (INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, global_leni*sizeof(int), add_shmi);
         shmi = MapViewOfFile(shmidi, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0);
         shmidv = CreateFileMapping (INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, (shmvr_size+shmvs_size)*sizeof(int), add_shmv);
         shmv = MapViewOfFile(shmidv, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0);     
#elif 1
         shmid = shm_open(add_shm, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR); if (shmid == -1) {perror("shmopen");exit(1);}                  
         shm = mmap(NULL, global_len*sizeof(my_real_c),PROT_WRITE | PROT_READ, MAP_SHARED, shmid, 0); if (shm == MAP_FAILED) {perror("mmap");exit(1);}
         shmidi = shm_open(add_shmi, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR); if (shmid == -1) {perror("shmopen");exit(1);}                  
         shmi = mmap(NULL, global_leni*sizeof(int),PROT_WRITE | PROT_READ, MAP_SHARED, shmidi, 0); if (shmi == MAP_FAILED) {perror("mmap");exit(1);}
         shmidv = shm_open(add_shmv, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR); if (shmidv == -1) {perror("shmopen");exit(1);}                  
         shmv = mmap(NULL, (shmvr_size+shmvs_size)*sizeof(int),PROT_WRITE | PROT_READ, MAP_SHARED, shmidv, 0); if (shmv == MAP_FAILED) {perror("mmap");exit(1);}
#endif
         if((com = (struct region *) malloc(sizeof(struct region))) == NULL) syserr_fatal("Malloc");
         com->mass_buf = shm + offset;     
         com->iner_buf = shm + offset + local_len;
         com->iner_rby_buf = shm + offset + 2*local_len; 
         com->fx_buf = shm + offset + 11*local_len;
         com->fr_buf = shm + offset + 14*local_len;
         com->sx_buf = shm + offset + 17*local_len;
         com->sr_buf = shm + offset + 18*local_len;
         com->vx_buf = shm + offset + 19*local_len;
         com->vr_buf = shm + offset + 22*local_len;
         com->dx_buf = shm + offset + 25*local_len;
         com->buf    = shmi + offseti ;
         com->itagr  = shmi + offseti + 10;
         com->itagr2 = shmi + offseti + 10 + local_len;
         com->itags  = shmi + offseti + 10 + 2*local_len;
         com->itags2 = shmi + offseti + 10 + 3*local_len;
         if (flg_sphinout == 1) com->iactv  = shmi + offseti + 10 + 4*local_len; 
     
         com->tagelr = shmv ;
         com->tagels = shmv + shmvr_size;

         writer(fidw, (void *) &bid, sizeof(int)); 
}


void _FCALL OPENSHM_C()
{
    openshm_c();
}
void openshm_c_()
{
    openshm_c();
}
void openshm_c__()
{openshm_c();}

/******************************************************************************/

void r2r_unlock_threads_c(nthr)
int *nthr;
{ 
int i;

    for (i = 0; i < *nthr; i++)
        {
#ifdef _WIN64
     if (!ReleaseSemaphore(sem_int,1,NULL) ) {printf("release semaphore: %lu\n", GetLastError());}
#elif 1        
     if (flag_siu == 0) {if (sem_post(&sem_int)==-1) {perror("send");exit(1);}}
     else {if (sem_post(sem_glob)==-1) {perror("send");exit(1);}}
#endif         
        } 
}

void _FCALL R2R_UNLOCK_THREADS_C(nthr)
int *nthr;
{
    r2r_unlock_threads_c(nthr);
}
void r2r_unlock_threads_c_(nthr)
int *nthr;
{
    r2r_unlock_threads_c(nthr);
}
void r2r_unlock_threads__(nthr)
int *nthr;
{r2r_unlock_threads_c(nthr);}

/******************************************************************************/

void r2r_block_c()
{
#ifdef _WIN64
    DWORD dwWaitResult; 
    dwWaitResult = WaitForSingleObject(sem_int,18000);    
#elif 1
    if (flag_siu==0) {if (sem_wait(&sem_int)==-1) {perror("send");exit(1);}}
    else {if (sem_wait(sem_glob)==-1) {perror("send");exit(1);}}
#endif        
}

void _FCALL R2R_BLOCK_C()
{
    r2r_block_c();
}
void r2r_block_c_()
{
    r2r_block_c();
}
void r2r_block__()
{r2r_block_c();}

/******************************************************************************/

void r2r_sem_c()
{
int bid;

      writer(fidw, (void *) &bid,sizeof(int));    
}

void _FCALL R2R_SEM_C()
{
    r2r_sem_c();
}
void r2r_sem_c_()
{
    r2r_sem_c();
}
void r2r_sem__()
{r2r_sem_c();}

/******************************************************************************/

void init_link_c(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,info,typ,cdt,cdr,print,rddl,nlink,dx)
int *igd, *nng, *nodbuf, *itab,*addcnel,*cnel,*ixc,*ofc,*info,*typ,*cdt,*cdr,*print,*rddl,*nlink;
my_real_c *x,*dx;
{
int i,j,t,nn, lbuf, lbuf1, init_buf[6],lbufa,lbufb,lbufel,dimno,k,capt,g,m,w,nbel,id,ref,lus;
int *nodid,*nbelem,*cnelem,*cnelem2,*cnelem3,*listel,*listelnbn,*listel2,*listel2nbn,*listelno,**tabl,nbn,*bcs;
my_real_c *crd;

    lbuf = *nng*sizeof(my_real_c);
    flagrot  = (int *) malloc(*nlink*2*sizeof(int));
    iroddl = *rddl;
    crd  = (my_real_c *) malloc(3*lbuf);
    lbuf1  = *nng*sizeof(int);
    dimno = 0 ;
    nbel = 0 ;
    t=0;
    k=0;    
    nodid  = (int *) malloc(lbuf1);
    bcs  = (int *) malloc(lbuf1);

    if (*typ > 3)
    {
        /************************coupling type 4, 5 and 6*****************************/
    /*****************************************************************************/        
      for (i = 0; i < *nng; i++)
      {
        nn = nodbuf[i]-1;
        crd[3*i]   = x[3*nn]-dx[3*nn];        
        crd[3*i+1] = x[3*nn+1]-dx[3*nn+1];
        crd[3*i+2] = x[3*nn+2]-dx[3*nn+2];
        
        nodid[i]   = itab[nn];        
        bcs[i] = 10*cdt[nn];
        if (*rddl) bcs[i] += cdr[nn];
          }    

      id = *igd;                            
      init_buf[0] = *igd;
      init_buf[1] = *nng;
      init_buf[2] = dimno ;
      init_buf[3] = 0 ;
      init_buf[4] = 0 ;    
      init_buf[5] = *print;        
                    
      writer(fidw, (void *) init_buf,6*sizeof(int));                            
      writer(fidw, (void *) nodid, lbuf1);
      writer(fidw, (void *) bcs, lbuf1);
      writer(fidw, (void *) crd, 3*lbuf);
    }
    else
    {
    /************************coupling type 1 and 2********************************/
    /*****************************************************************************/
    nbelem  = (int *) malloc(lbuf1);
    cnelem  = (int *) malloc(sizeof(int));
    cnelem3  = (int *) malloc(sizeof(int));
    listel2  = (int *) malloc(sizeof(int));
    tabl = (int **) malloc(sizeof(int*));        
    listel2nbn  = (int *) malloc(sizeof(int));        
           
    /********Creation of buffers************/
        
    for (i = 0; i < *nng; i++)
    {
        nn = nodbuf[i]-1;
        crd[3*i]   = x[3*nn]-dx[3*nn];
        crd[3*i+1] = x[3*nn+1]-dx[3*nn+1];
        crd[3*i+2] = x[3*nn+2]-dx[3*nn+2];    
        nodid[i]   = itab[nn];
        bcs[i] = 10*cdt[nn];
        if (*rddl) bcs[i] += cdr[nn];
        
        nbelem[i]  = addcnel[nn+2]-addcnel[nn+1];
        cnelem  = (int *) realloc(cnelem, (dimno + nbelem[i]) * sizeof(int));
        cnelem3  = (int *) realloc(cnelem3, (dimno + nbelem[i]) * sizeof(int));

            for (j = 0; j < nbelem[i] ; j++)
         { 
           capt = 0;         
           cnelem[dimno+j]=cnel[addcnel[nn+1]+j];

           for (k = 0; k < t ; k++)
            {                    
             if (cnelem[dimno+j]==listel2[k])
              {            
               capt=1;
               tabl[k]  = (int *) realloc(tabl[k], (listel2nbn[k]+1) * sizeof(int));               
               tabl[k][listel2nbn[k]]=nodid[i];                           
               listel2nbn[k]++;
               cnelem3[dimno+j]=k;                                   
              }
            }
             if (capt == 0)
              {
               listel2  = (int *) realloc(listel2, (t+1) * sizeof(int));               
               listel2nbn  = (int *) realloc(listel2nbn, (t+1) * sizeof(int));
               tabl  = (int **) realloc(tabl, (t+1) * sizeof(int*));                   
                       tabl[t]  = (int *) malloc(sizeof(int));                                     
               listel2[t]=cnelem[dimno+j];
               listel2nbn[t]=1;
               tabl[t][0]=nodid[i];
               cnelem3[dimno+j]=t;                                              
                t++;                  
              }
          }  
        dimno = dimno + nbelem[i];                         
    }
            
    /********Sorting of element buffer************/
    g = 0;
    w = 0;    
    listel  = (int *) (malloc(sizeof(int)));
    listelnbn  = (int *) (malloc(sizeof(int)));        
    listelno  = (int *) (malloc(sizeof(int)));
        
    for (i = 0; i < t; i++)
    {
      if ((listel2nbn[i]>1) || ((listel2nbn[i] == 1)&&(*nng == 1)))
      {
       listel  = (int *) realloc(listel, (w+1) * sizeof(int));
       listelnbn  = (int *) realloc(listelnbn, (w+1) * sizeof(int));
       listel[w]=listel2[i];
       listelnbn[w]=listel2nbn[i];               
       for (j = 0; j < listel2nbn[i] ; j++)
        {
          listelno  = (int *) realloc(listelno, (g+1) * sizeof(int));        
              listelno[g]=tabl[i][j];          
          g++;                    
        }
       w++;        
      }    
    }
    
    /*********filtering of buffer of nodes**************/
    /*----(some elements are removed from buffer:
             - type 1,2 : elements with only one node on the interface----*/
    t = 0;
    k = 0;
    m = 0;
    cnelem2  = (int *) malloc(sizeof(int));
    for (i = 0; i < *nng; i++)
      {
       lus = nbelem[i];
       for (j = 0; j < lus ; j++)
        {
           m = cnelem3[k];
           
               if ((listel2nbn[m]>1) || ((listel2nbn[m] == 1)&&(*nng == 1)))           
             {cnelem2  = (int *) realloc(cnelem2, (t+1) * sizeof(int));
              cnelem2[t]=cnelem[k];
              t++;}
           else
                 {nbelem[i] = nbelem[i] -1 ;}
           k++; 
        }
          }
      
      dimno = t;
      
        /********send of buffers to Rad2rad**************/    
    
    id = *igd;
                            
    init_buf[0] = *igd;
    init_buf[1] = *nng;
    init_buf[2] = dimno ;
    init_buf[3] = w ;
    init_buf[4] = g ;
    init_buf[5] = *print;    
    lbufel = w*sizeof(int);
    lbufa = dimno*sizeof(int);
    lbufb = g*sizeof(int);

    writer(fidw, (void *) init_buf,6*sizeof(int));                            
    writer(fidw, (void *) nodid, lbuf1);
    writer(fidw, (void *) bcs, lbuf1);
    writer(fidw, (void *) crd, 3*lbuf);
        writer(fidw, (void *) nbelem, lbuf1);            
    writer(fidw, (void *) cnelem2, lbufa);
    writer(fidw, (void *) listel, lbufel);
    writer(fidw, (void *) listelnbn, lbufel);
    writer(fidw, (void *) listelno, lbufb);
    
    /*********Deallocation****************************/
    free(nbelem);    
    free(cnelem);
    free(tabl);
    free(nodid);
    free(crd);    
    free(listel2);        
    free(listel2nbn);    
    free(listel);        
    free(listelnbn);
    free(listelno);}                        
}

void _FCALL  INIT_LINK_C (igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,info,typ,cdt,cdr,print,rddl,nlink,dx)
int *igd, *nng, *nodbuf, *itab,*addcnel,*cnel,*ixc,*ofc,*info,*typ,*cdt,*cdr,*print,*rddl,*nlink;
my_real_c *x,*dx;
{
    init_link_c(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,info,typ,cdt,cdr,print,rddl,nlink,dx);
}

void _FCALL init_link_c_(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,info,typ,cdt,cdr,print,rddl,nlink,dx)
int *igd, *nng, *nodbuf, *itab,*addcnel,*cnel,*ixc,*ofc,*info,*typ,*cdt,*cdr,*print,*rddl,*nlink;
my_real_c *x,*dx;
{
    init_link_c(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,info,typ,cdt,cdr,print,rddl,nlink,dx);
}

void _FCALL init_link_c__(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,info,typ,cdt,cdr,print,rddl,nlink,dx)
int *igd, *nng, *nodbuf, *itab,*addcnel,*cnel,*ixc,*ofc,*info,*typ,*cdt,*cdr,*print,*rddl,*nlink;
my_real_c *x,*dx;
{init_link_c(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,info,typ,cdt,cdr,print,rddl,nlink,dx);}

void init_link_nl_c(igd, nng, itab, nodbuf, x,print,dx,ndof_nl,nb_tot_dof)
int *igd, *nng, *nodbuf, *itab,*print,*ndof_nl,*nb_tot_dof;
my_real_c *x,*dx;
{
int i,j,tt,nn,lbuf,lbuf1,init_buf[6],id;
int *nodid,*bcs;
my_real_c *crd;

    /************************coupling for non local dof***************************/
    /*****************************************************************************/
    lbuf = *nb_tot_dof*sizeof(my_real_c);
    crd  = (my_real_c *) malloc(3*lbuf);
    lbuf1  = *nb_tot_dof*sizeof(int);   
    nodid  = (int *) malloc(lbuf1);
    bcs  = (int *) malloc(lbuf1);
        
    tt = 0;
    for (i = 0; i < *nng; i++)
      {
        nn = nodbuf[i]-1;
        for (j = 0; j < ndof_nl[i]; j++)
          {
           crd[3*tt]   = x[3*nn]-dx[3*nn];        
           crd[3*tt+1] = x[3*nn+1]-dx[3*nn+1];
           crd[3*tt+2] = x[3*nn+2]-dx[3*nn+2];        
           nodid[tt]   = itab[nn]+10000*j;
           bcs[tt] = 0;
           tt++;
          }    
      }

    id = *igd;                            
    init_buf[0] = *igd;
    init_buf[1] = *nb_tot_dof;
    init_buf[2] = 0 ;
    init_buf[3] = 0 ;
    init_buf[4] = 0 ;    
    init_buf[5] = *print;        
                    
    writer(fidw, (void *) init_buf,6*sizeof(int));                            
    writer(fidw, (void *) nodid, lbuf1);
    writer(fidw, (void *) bcs, lbuf1);
    writer(fidw, (void *) crd, 3*lbuf);   

    /*********Deallocation****************************/
    free(crd);    
    free(nodid);
    free(bcs);                   
}

void _FCALL  INIT_LINK_NL_C (igd, nng, itab, nodbuf, x,print,dx,ndof_nl,nb_tot_dof)
int *igd, *nng, *nodbuf, *itab,*print,*ndof_nl,*nb_tot_dof;
my_real_c *x,*dx;
{
    init_link_nl_c(igd, nng, itab, nodbuf, x,print,dx,ndof_nl,nb_tot_dof);
}

void _FCALL init_link_nl_c_(igd, nng, itab, nodbuf, x,print,dx,ndof_nl,nb_tot_dof)
int *igd, *nng, *nodbuf, *itab,*print,*ndof_nl,*nb_tot_dof;
my_real_c *x,*dx;
{
    init_link_nl_c(igd, nng, itab, nodbuf, x,print,dx,ndof_nl,nb_tot_dof);
}

void _FCALL init_link_nl_c__(igd, nng, itab, nodbuf, x,print,dx,ndof_nl,nb_tot_dof)
int *igd, *nng, *nodbuf, *itab,*print,*ndof_nl,*nb_tot_dof;
my_real_c *x,*dx;
{init_link_nl_c(igd, nng, itab, nodbuf, x,print,dx,ndof_nl,nb_tot_dof);}


void init_buf_spmd_c(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,tlel,lel,lelnb,tleln,leln,nbelem,tcnel,cnelem2,wgt,tcneldb,cnelemdb,info,typ,nglob)
int *igd, *nng, *nodbuf, *itab,*addcnel,*cnel,*ixc,*ofc,*tlel,*lel,*lelnb,*tleln,*leln,*nbelem,*tcnel,*cnelem2,*wgt,*tcneldb,*cnelemdb,*info,*typ,*nglob;
my_real_c *x;
{
int i,j,t,nn, lbuf, lbuf1, init_buf[5],lbufa,lbufb,lbufel,dimno,k,capt,g,m,w,nbel,weight,zz,zz2;
int *nodid,*cnelem,*cnelem3,*listel,*listelnbn,*listel2,*listel2nbn,*listelno,**tabl,nbn,lus;
my_real_c *crd;

    lbuf = *nng*sizeof(my_real_c);
    crd  = (my_real_c *) malloc(3*lbuf);
    lbuf1  = *nng*sizeof(int);
    dimno = 0 ;
    nbel = 0 ;
    t=0;
    k=0;
    zz = 0;
    zz2 = 0;    
    nodid  = (int *) malloc(lbuf1);
    /*nbelem  = (int *) malloc(lbuf1);*/
    cnelem  = (int *) malloc(sizeof(int));
    cnelem3  = (int *) malloc(sizeof(int));
    listel2  = (int *) malloc(sizeof(int));    
    tabl = (int **) malloc(sizeof(int*));        
    listel2nbn  = (int *) malloc(sizeof(int));

    /********Creation of buffers************/
        
    for (i = 0; i < *nng; i++)
    {
        nn = nodbuf[i]-1;
        crd[3*i]   = x[3*nn];
        crd[3*i+1] = x[3*nn+1];
        crd[3*i+2] = x[3*nn+2];        
        nodid[i]   = itab[nn];
        weight = wgt[nn];
        nbelem[i]  = addcnel[nn+2]-addcnel[nn+1];        
        cnelem  = (int *) realloc(cnelem, (dimno + nbelem[i]) * sizeof(int));
                cnelem3  = (int *) realloc(cnelem3, (dimno + nbelem[i]) * sizeof(int));
        
            for (j = 0; j < nbelem[i] ; j++)
         { 
           capt = 0;         
           cnelem[dimno+j]=cnel[addcnel[nn+1]+j];

           for (k = 0; k < t ; k++)
            {                    
             if (cnelem[dimno+j]==listel2[k])
              {            
               capt=1;
               tabl[k]  = (int *) realloc(tabl[k], (listel2nbn[k]+1) * sizeof(int));           
               tabl[k][listel2nbn[k]]=nodid[i];                           
               listel2nbn[k]++;
               cnelem3[dimno+j]=k;                                   
              }
            }
            
             if (capt == 0)
              {
               listel2  = (int *) realloc(listel2, (t+1) * sizeof(int));
               listel2nbn  = (int *) realloc(listel2nbn,(t+1) * sizeof(int));
               tabl  = (int **) realloc(tabl,(t+1) * sizeof(int*));
                       tabl[t]  = (int *) malloc(sizeof(int));                                 
               listel2[t]=cnelem[dimno+j];
               listel2nbn[t]=1;
               tabl[t][0]=nodid[i];
               cnelem3[dimno+j]=t;                                              
                t++;                          
              }              
          }  
        dimno = dimno + nbelem[i];                         
    }
            
    /********sorting of element buffer************/
    
    g = 0;
    w = 0;
    listel  = (int *) (malloc(sizeof(int)));
    listelnbn  = (int *) (malloc(sizeof(int)));        
    listelno  = (int *) (malloc(sizeof(int)));    
    for (i = 0; i < t; i++)
    {      
     if ((listel2nbn[i]>1) || ((listel2nbn[i] == 1)&&(*nglob == 1)))
      {
       listel  = (int *) realloc(listel, (w+1) * sizeof(int));
       listelnbn  = (int *) realloc(listelnbn, (w+1) * sizeof(int));
       listel[w]=listel2[i];
       lel[w]=listel[w];
       listelnbn[w]=listel2nbn[i];
       lelnb[w]=listel2nbn[i];               
       for (j = 0; j < listel2nbn[i] ; j++)
        {
          listelno  = (int *) realloc(listelno, (g+1) * sizeof(int));        
              listelno[g]=tabl[i][j];
          leln[g]=listelno[g];          
          g++;                    
        }
       w++;        
      }    
    }
    
    *tlel = w;
    *tleln = g;
    
    /*********filtering of buffer of nodes**************/
    /*----(some elements are removed from buffer:
             - type 1,2 : elements with only one node on the interface----*/
    t = 0;
    k = 0;
    m = 0;

    for (i = 0; i < *nng; i++)
      {
       nn = nodbuf[i]-1;
       weight = wgt[nn];
       lus = nbelem[i];
       for (j = 0; j < lus ; j++)
        {
           m = cnelem3[k];
                          
           if ((listel2nbn[m]>1) || ((listel2nbn[m] == 1)&&(*nglob == 1)))           
             {
          if (weight == 1)
            {cnelem2[zz]=cnelem[k];
             zz++;
            }
            if (weight == 0)
            {cnelemdb[zz2]=cnelem[k];
             zz2++;
            }    
         }
           else
                 {nbelem[i] = nbelem[i] -1 ;}
           k++; 
        }
          }
      
     dimno = t;
    *tcnel = zz;
    *tcneldb =zz2;
    
    /*********Deallocation****************************/
    free(cnelem);
    free(tabl);
    free(nodid);
    free(crd);    
    free(listel2);        
    free(listel2nbn);    
    free(listel);        
    free(listelnbn);
    free(listelno);                            
}

void _FCALL  INIT_BUF_SPMD_C (igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,tlel,lel,lelnb,tleln,leln,nbelem,tcnel,cnelem2,wgt,tcneldb,cnelemdb,info,typ,nglob)
int *igd, *nng, *nodbuf, *itab,*addcnel,*cnel,*ixc,*ofc,*tlel,*lel,*lelnb,*tleln,*leln,*nbelem,*tcnel,*cnelem2,*wgt,*tcneldb,*cnelemdb,*info,*typ,*nglob;
my_real_c *x;
{
    init_buf_spmd_c(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,tlel,lel,lelnb,tleln,leln,nbelem,tcnel,cnelem2,wgt,tcneldb,cnelemdb,info,typ,nglob);
}

void _FCALL init_buf_spmd_c_(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,tlel,lel,lelnb,tleln,leln,nbelem,tcnel,cnelem2,wgt,tcneldb,cnelemdb,info,typ,nglob)
int *igd, *nng, *nodbuf, *itab,*addcnel,*cnel,*ixc,*ofc,*tlel,*lel,*lelnb,*tleln,*leln,*nbelem,*tcnel,*cnelem2,*wgt,*tcneldb,*cnelemdb,*info,*typ,*nglob;
my_real_c *x;
{
    init_buf_spmd_c(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,tlel,lel,lelnb,tleln,leln,nbelem,tcnel,cnelem2,wgt,tcneldb,cnelemdb,info,typ,nglob);
}

void _FCALL init_buf_spmd_c__(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,tlel,lel,lelnb,tleln,leln,nbelem,tcnel,cnelem2,wgt,tcneldb,cnelemdb,info,typ,nglob)
int *igd, *nng, *nodbuf, *itab,*addcnel,*cnel,*ixc,*ofc,*tlel,*lel,*lelnb,*tleln,*leln,*nbelem,*tcnel,*cnelem2,*wgt,*tcneldb,*cnelemdb,*info,*typ,*nglob;
my_real_c *x;
{init_buf_spmd_c(igd, nng, itab, nodbuf, x,addcnel,cnel,ixc,ofc,tlel,lel,lelnb,tleln,leln,nbelem,tcnel,cnelem2,wgt,tcneldb,cnelemdb,info,typ,nglob);}


void init_link_spmd_c(igd,nng,dbnod,nbproc,ibuf,dbibuf,dbnbuf,ddbuf,rbuf,dim,ibufnb,ibufcnel,nbel,dimel,ibufel,ibufelnbnod,ibufelnod,dimb,ibufcneldb,ibufnbeldb,typ,bcs,print,rddl,nl,nlnk,iex)
int *igd, *nng, *nbproc, *ibuf, *dbibuf,*dbnbuf,*ddbuf,*dbnod,*dim,*ibufnb,*ibufcnel,*nbel,*dimel,*ibufel,*ibufelnbnod,*ibufelnod,*dimb,*ibufcneldb,*ibufnbeldb,*typ,*bcs,*print,*rddl,*nl,*nlnk,*iex;
my_real_c *rbuf;
{
int i,j,lbuf, lbuf1,lbufb,lbufp,init_buf[8],lbufa,lbufel,lbufdimel,lbufc,offset,offsetb,offsett,iproc;
       
        nspmd = *nbproc;

    init_buf[0] = *igd;
    init_buf[1] = *nng;
    init_buf[2] = *dim;    
    init_buf[3] = *nbel;
    init_buf[4] = *dimel;
    init_buf[5] = *print;            
    init_buf[6] = *dbnod;
    init_buf[7] = *dimb;
    flagrot  = (int *) malloc((*nl+1)*sizeof(int));
    iroddl = *rddl;    
                
    writer(fidw, (void *) init_buf, 8*sizeof(int));        
    lbuf = *nng*sizeof(my_real_c);
    lbuf1  = *nng*sizeof(int);
    lbufb  = *dbnod*sizeof(int);
    lbufp  = *nbproc*sizeof(int);
    lbufa  = *dim*sizeof(int);
    lbufc  = *dimb*sizeof(int);
    lbufel  = *nbel*sizeof(int);
    lbufdimel  = *dimel*sizeof(int);
        
        /*************determination of the master for db nodes******************************/
        if (*iex == 1) {masterdb = malloc(*nlnk*sizeof(int*));}
            
        offset = 0;
        offsetb = 0;
        offsett = 0;                      
        masterdb[*iex-1] = malloc((*nng+(*dbnod))*sizeof(int));
        for (iproc = 0; iproc < nspmd; iproc++)                 
      {
           for (i = 0; i < ddbuf[iproc]; i++) masterdb[*iex-1][offset+i] = offsetb + i;
           offset += ddbuf[iproc];
           offsetb += ddbuf[iproc];           
           for (i = 0; i < dbnbuf[iproc]; i++)
             for (j = 0; j < *nng; j++)
           {if (ibuf[j] == dbibuf[offsett+i]) masterdb[*iex-1][offset+i] = j;}           
           offset += dbnbuf[iproc];
           offsett += dbnbuf[iproc];
          }                                                          
        /*************************************************************************************/        
                 
    writer(fidw, (void *) ibuf, lbuf1);
    writer(fidw, (void *) bcs, lbuf1);                            
    writer(fidw, (void *) rbuf, 3*lbuf);                        
    writer(fidw, (void *) dbibuf, lbufb);            
    writer(fidw, (void *) dbnbuf, lbufp);
    writer(fidw, (void *) ddbuf, lbufp);
    
    /****infos on elements (sent only for coupling type 1 and 2)*********************/
        if(*typ < 4) 
    {writer(fidw, (void *) ibufnb, lbuf1);    
     writer(fidw, (void *) ibufcnel, lbufa);
     writer(fidw, (void *) ibufel, lbufel);    
     writer(fidw, (void *) ibufelnbnod, lbufel);
     writer(fidw, (void *) ibufelnod, lbufdimel);
     writer(fidw, (void *) ibufnbeldb, lbufb);
     writer(fidw, (void *) ibufcneldb, lbufc);}
    /*********************************************************************************/                    
}

void _FCALL  INIT_LINK_SPMD_C(igd,nng,dbnod,nbproc,ibuf,dbibuf,dbnbuf,ddbuf,rbuf,dim,ibufnb,ibufcnel,nbel,dimel,ibufel,ibufelnbnod,ibufelnod,dimb,ibufcneldb,ibufnbeldb,typ,bcs,print,rddl,nl,nlnk,iex)
int *igd, *nng, *nbproc, *ibuf, *dbibuf,*dbnbuf,*ddbuf,*dbnod,*dim,*ibufnb,*ibufcnel,*nbel,*dimel,*ibufel,*ibufelnbnod,*ibufelnod,*dimb,*ibufcneldb,*ibufnbeldb,*typ,*bcs,*print,*rddl,*nl,*nlnk,*iex;
my_real_c *rbuf;
{
  init_link_spmd_c(igd,nng,dbnod,nbproc,ibuf,dbibuf,dbnbuf,ddbuf,rbuf,dim,ibufnb,ibufcnel,nbel,dimel,ibufel,ibufelnbnod,ibufelnod,dimb,ibufcneldb,ibufnbeldb,typ,bcs,print,rddl,nl,nlnk,iex);
}

void _FCALL init_link_spmd_c_(igd,nng,dbnod,nbproc,ibuf,dbibuf,dbnbuf,ddbuf,rbuf,dim,ibufnb,ibufcnel,nbel,dimel,ibufel,ibufelnbnod,ibufelnod,dimb,ibufcneldb,ibufnbeldb,typ,bcs,print,rddl,nl,nlnk,iex)
int *igd, *nng, *nbproc, *ibuf, *dbibuf,*dbnbuf,*ddbuf,*dbnod,*dim,*ibufnb,*ibufcnel,*nbel,*dimel,*ibufel,*ibufelnbnod,*ibufelnod,*dimb,*ibufcneldb,*ibufnbeldb,*typ,*bcs,*print,*rddl,*nl,*nlnk,*iex;
my_real_c *rbuf;
{
   init_link_spmd_c(igd,nng,dbnod,nbproc,ibuf,dbibuf,dbnbuf,ddbuf,rbuf,dim,ibufnb,ibufcnel,nbel,dimel,ibufel,ibufelnbnod,ibufelnod,dimb,ibufcneldb,ibufnbeldb,typ,bcs,print,rddl,nl,nlnk,iex);
}

void _FCALL init_link_spmd_c__(igd,nng,dbnod,nbproc,ibuf,dbibuf,dbnbuf,ddbuf,rbuf,dim,ibufnb,ibufcnel,nbel,dimel,ibufel,ibufelnbnod,ibufelnod,dimb,ibufcneldb,ibufnbeldb,typ,bcs,print,rddl,nl,nlnk,iex)
int *igd, *nng, *nbproc, *ibuf,*dbibuf,*dbnbuf,*ddbuf,*dbnod,*dim,*ibufnb,*ibufcnel,*nbel,*dimel,*ibufel,*ibufelnbnod,*ibufelnod,*dimb,*ibufcneldb,*ibufnbeldb,*typ,*bcs,*print,*rddl,*nl,*nlnk,*iex;
my_real_c *rbuf;
{
   init_link_spmd_c(igd,nng,dbnod,nbproc,ibuf,dbibuf,dbnbuf,ddbuf,rbuf,dim,ibufnb,ibufcnel,nbel,dimel,ibufel,ibufelnbnod,ibufelnod,dimb,ibufcneldb,ibufnbeldb,typ,bcs,print,rddl,nl,nlnk,iex);
}

void send_ibuf_c(ibuf, len)
int *ibuf, *len;
{
    writer(fidw, (void *) ibuf, *len*sizeof(int));
}
void _FCALL SEND_IBUF_C(ibuf, len)
int *ibuf, *len;
{
    send_ibuf_c(ibuf, len);
}
void _FCALL send_ibuf_c_(ibuf, len)
int *ibuf, *len;
{
    send_ibuf_c(ibuf, len);
}
void _FCALL send_ibuf_c__(ibuf, len)
int *ibuf, *len;
{send_ibuf_c(ibuf, len);}

/************************************************************************/

void send_fbuf_c(fbuf, len)
my_real_c *fbuf;
int *len;
{
    writer(fidw, (void *) fbuf, *len*sizeof(my_real_c));
}
void _FCALL SEND_FBUF_C(fbuf, len)
my_real_c *fbuf;
int *len;
{
    send_fbuf_c(fbuf, len);
}
void send_fbuf_c_(fbuf, len)
my_real_c *fbuf;
int *len;
{
    send_fbuf_c(fbuf, len);
}
void send_fbuf_c__(fbuf, len)
my_real_c *fbuf;
int *len;
{send_fbuf_c(fbuf, len);}


/************************************************************************/

void send_fbufdp_c(fbuf, len)
double *fbuf;
int *len;
{
    writer(fidw, (void *) fbuf, *len*sizeof(double));
}
void _FCALL SEND_FBUFDP_C(fbuf, len)
double *fbuf;
int *len;
{
    send_fbufdp_c(fbuf, len);
}
void send_fbufdp_c_(fbuf, len)
double *fbuf;
int *len;
{
    send_fbufdp_c(fbuf, len);
}
void send_fbufdp_c__(fbuf, len)
double *fbuf;
int *len;
{send_fbufdp_c(fbuf, len);}

/***************************************************************************/

void get_fbuf_c(fbuf, len)
my_real_c *fbuf;
int *len;
{
    readr(fidr, (void *) fbuf, *len*sizeof(my_real_c));
}
void _FCALL GET_FBUF_C(fbuf, len)
my_real_c *fbuf;
int *len;
{
    get_fbuf_c(fbuf, len);
}
void get_fbuf_c_(fbuf, len)
my_real_c *fbuf;
int *len;
{
    get_fbuf_c(fbuf, len);
}
void get_fbuf_c__(fbuf, len)
my_real_c *fbuf;
int *len;
{get_fbuf_c(fbuf, len);}

/***************************************************************************/

void get_fbufdp_c(fbuf, len)
double *fbuf;
int *len;
{
    readr(fidr, (void *) fbuf, *len*sizeof(double));
}
void _FCALL GET_FBUFDP_C(fbuf, len)
double *fbuf;
int *len;
{
    get_fbufdp_c(fbuf, len);
}
void get_fbufdp_c_(fbuf, len)
double *fbuf;
int *len;
{
    get_fbufdp_c(fbuf, len);
}
void get_fbufdp_c__(fbuf, len)
double *fbuf;
int *len;
{get_fbufdp_c(fbuf, len);}

/******************************************************************************/

void get_ibuf_c(ibuf, len)
int *ibuf, *len;
{
    readr(fidr, (void *) ibuf, *len*sizeof(int));
}
void _FCALL GET_IBUF_C(ibuf, len)
int *ibuf, *len;
{
    get_ibuf_c(ibuf, len);
}
void get_ibuf_c_(ibuf, len)
int *ibuf, *len;
{
    get_ibuf_c(ibuf, len);
}
void get_ibuf_c__(ibuf, len)
int *ibuf, *len;
{get_ibuf_c(ibuf, len);}


void send_mass_c(idp, nng, nodbuf, ms, in)
int *idp, *nng, *nodbuf;
my_real_c *ms, *in;
{
int i, nn, lbuf, flag;
my_real_c *mbuf, *ibuf;

    writer(fidw, (void *) idp, sizeof(int));    
    readr(fidr, (void *) &flag, sizeof(int));
    flagrot[*idp]=flag;
    
    lbuf = *nng*sizeof(my_real_c);
    mbuf = (my_real_c *) malloc(lbuf);
    if (flagrot[*idp]) ibuf = (my_real_c *) malloc(lbuf);

    for (i = 0; i < *nng; i++)
    {
        nn = nodbuf[i]-1;
        mbuf[i] = ms[nn];
        if (flagrot[*idp]) ibuf[i] = in[nn];
    }    
    writer(fidw, (void *) mbuf, lbuf);    
    if (flagrot[*idp]) writer(fidw, (void *) ibuf, lbuf);
    
    free(mbuf);
    if (flagrot[*idp]) free(ibuf);
}

void _FCALL SEND_MASS_C(idp, nng, nodbuf, ms, in)
int *idp, *nng, *nodbuf;
my_real_c *ms, *in;
{
    send_mass_c(idp, nng, nodbuf, ms, in);
}
void send_mass_c_(idp, nng, nodbuf, ms, in)
int *idp, *nng, *nodbuf;
my_real_c *ms, *in;
{
    send_mass_c(idp, nng, nodbuf, ms, in);
}
void send_mass_c__(idp, nng, nodbuf, ms, in)
int *idp, *nng, *nodbuf;
my_real_c *ms, *in;
{send_mass_c(idp, nng, nodbuf, ms, in);}

void send_mass_nl_c(idp, nng, iadd_nl, ms)
int *idp, *nng, *iadd_nl;
my_real_c *ms;
{
int i, j,nn, lbuf, flag;
my_real_c *mbuf;

    writer(fidw, (void *) idp, sizeof(int));    
    readr(fidr, (void *) &flag, sizeof(int));
    flagrot[*idp]=0;
    
    lbuf = *nng*sizeof(my_real_c);
    mbuf = (my_real_c *) malloc(lbuf);

    for (i = 0; i < *nng; i++)
      {
       mbuf[i] = ms[iadd_nl[i]-1];
      }
    
    writer(fidw, (void *) mbuf, lbuf);    
    free(mbuf);
}

void _FCALL SEND_MASS_NL_C(idp, nng, iadd_nl, ms)
int *idp, *nng, *iadd_nl;
my_real_c *ms;
{
    send_mass_nl_c(idp, nng, iadd_nl, ms);
}
void send_mass_nl_c_(idp, nng, iadd_nl, ms)
int *idp, *nng, *iadd_nl;
my_real_c *ms;
{
    send_mass_nl_c(idp, nng, iadd_nl, ms);
}
void send_mass_nl_c__(idp, nng, iadd_nl, ms)
int *idp, *nng, *iadd_nl;
my_real_c *ms;
{send_mass_nl_c(idp, nng, iadd_nl, ms);}

void send_mass_rby_c(idp, nng, nodbuf, ms, in, npby, nrbody, rby, tag, add_rby, nnpby, nrby)
int *idp, *nng, *nodbuf, *nrbody, *npby, *tag, *add_rby, *nnpby, *nrby;
my_real_c *ms, *in, *rby;
{
int i, k,l,nn, lbuf, flag, *cbuf;
my_real_c *mbuf, *ibuf, *m2buf, *matrix_buf;

    writer(fidw, (void *) idp, sizeof(int));    
    readr(fidr, (void *) &flag, sizeof(int));
    flagrot[*idp]=flag;
    
    lbuf = *nng*sizeof(my_real_c);
    mbuf = (my_real_c *) malloc(lbuf);
    m2buf = (my_real_c *) malloc(lbuf);    
    cbuf = (int *) malloc(lbuf);
    if (flagrot[*idp])
      {ibuf = (my_real_c *) malloc(lbuf);
       matrix_buf = (my_real_c *) malloc(9*lbuf);}
    
    for (i = 0; i < *nng; i++)
    {
        nn = nodbuf[i]-1;
        mbuf[i] = ms[nn];
        if (flagrot[*idp]) ibuf[i] = in[nn];
        for (k = 0; k < *nrbody; k++)
           if (npby[*nnpby*k]==nn+1)
             {tag[*add_rby+i] = k;             
              cbuf[i] = npby[*nnpby*k+2];
              m2buf[i] = rby[*nrby*k+14];
              for (l = 0; l < 9; l++)
                 matrix_buf[9*i+l]=rby[*nrby*k+16+l];}   
    }
        
    writer(fidw, (void *) mbuf, lbuf);    
    if (flagrot[*idp]) writer(fidw, (void *) ibuf, lbuf);    
    writer(fidw, (void *) cbuf, lbuf);     
    writer(fidw, (void *) m2buf, lbuf);
    writer(fidw, (void *) matrix_buf, 9*lbuf);
                   
    free(mbuf);
    free(m2buf);
    free(matrix_buf);    
    if (flagrot[*idp]) free(ibuf);
    free(cbuf);    
}

void _FCALL SEND_MASS_RBY_C(idp, nng, nodbuf, ms, in, npby, nrbody, rby, tag, add_rby, nnpby, nrby)
int *idp, *nng, *nodbuf, *nrbody, *npby, *tag, *add_rby, *nnpby, *nrby;
my_real_c *ms, *in, *rby;
{
    send_mass_rby_c(idp, nng, nodbuf, ms, in, npby, nrbody, rby, tag, add_rby, nnpby, nrby);
}
void send_mass_rby_c_(idp, nng, nodbuf, ms, in, npby, nrbody, rby, tag, add_rby, nnpby, nrby)
int *idp, *nng, *nodbuf, *nrbody, *npby, *tag, *add_rby, *nnpby, *nrby;
my_real_c *ms, *in, *rby;
{
    send_mass_rby_c(idp, nng, nodbuf, ms, in, npby, nrbody, rby, tag, add_rby, nnpby, nrby);
}
void send_mass_rby_c__(idp, nng, nodbuf, ms, in, npby, nrbody, rby, tag, add_rby, nnpby, nrby)
int *idp, *nng, *nodbuf, *nrbody, *npby, *tag, *add_rby, *nnpby, *nrby;
my_real_c *ms, *in, *rby;
{send_mass_rby_c(idp, nng, nodbuf, ms, in, npby, nrbody, rby, tag, add_rby, nnpby, nrby);}



void init_activ_c(activ)
int *activ;
{
    readr(fidr, (void *) activ, sizeof(int));
}


void  _FCALL INIT_ACTIV_C(activ)
int *activ;
{    init_activ_c(activ);}
void init_activ_c_(activ)
int *activ;
{    init_activ_c(activ);}
void init_activ_c__(activ)
int *activ;
{init_activ_c(activ);}

/*void check_roddl_c()
{
    readr(fidr, (void *) &flagrot[*idp], sizeof(int));
}*/

/*void  _FCALL CHECK_RODDL_C()
{
    check_roddl_c();
}
void check_roddl_c_()
{
    check_roddl_c();
}
void check_roddl_c__()
{check_roddl_c();}*/


void send_mass_spmd_c(idp, nng, buf1, buf2, iroddl)
int *idp, *nng, *iroddl;
my_real_c *buf1, *buf2;
{
int lbuf, flag;
    writer(fidw, (void *) idp, sizeof(int));;    
    readr(fidr, (void *) &flag, sizeof(int));        
    flagrot[*idp]=flag;
    *iroddl=flag;
    lbuf = *nng*sizeof(my_real_c);
    writer(fidw, (void *) buf1, lbuf);    
    if (flagrot[*idp]) writer(fidw, (void *) buf2, lbuf);
}
void _FCALL SEND_MASS_SPMD_C(idp, nng, buf1, buf2, iroddl)
int *idp, *nng, *iroddl;
my_real_c *buf1, *buf2;
{
    send_mass_spmd_c(idp, nng, buf1, buf2, iroddl);
}
void send_mass_spmd_c_(idp, nng, buf1, buf2, iroddl)
int *idp, *nng, *iroddl;
my_real_c *buf1, *buf2;
{
    send_mass_spmd_c(idp, nng, buf1, buf2, iroddl);
}
void send_mass_spmd_c__(idp, nng, buf1, buf2, iroddl)
int *idp, *nng, *iroddl;
my_real_c *buf1, *buf2;
{send_mass_spmd_c(idp, nng, buf1, buf2, iroddl);}



void send_mass_rby_spmd_c(idp,nng,buf1,buf2,buf3,buf4,buf5,iroddl)
int *idp, *nng, *iroddl, *buf3;
my_real_c *buf1,*buf2,*buf4,*buf5;
{
int lbuf, flag;
    writer(fidw, (void *) idp, sizeof(int));;    
    readr(fidr, (void *) &flag, sizeof(int));        
    flagrot[*idp]=flag;
    *iroddl=flag;
    lbuf = *nng*sizeof(my_real_c);
    writer(fidw, (void *) buf1, lbuf);    
    if (flagrot[*idp]) writer(fidw, (void *) buf2, lbuf);    
    writer(fidw, (void *) buf3, lbuf);     
    writer(fidw, (void *) buf4, lbuf);
    writer(fidw, (void *) buf5, 9*lbuf);
    
}
void _FCALL SEND_MASS_RBY_SPMD_C(idp,nng,buf1,buf2,buf3,buf4,buf5,iroddl)
int *idp, *nng, *iroddl, *buf3;
my_real_c *buf1,*buf2,*buf4,*buf5;
{
    send_mass_rby_spmd_c(idp,nng,buf1,buf2,buf3,buf4,buf5,iroddl);
}
void send_mass_rby_spmd_c_(idp,nng,buf1,buf2,buf3,buf4,buf5,iroddl)
int *idp, *nng, *iroddl, *buf3;
my_real_c *buf1,*buf2,*buf4,*buf5;
{
    send_mass_rby_spmd_c(idp,nng,buf1,buf2,buf3,buf4,buf5,iroddl);
}
void send_mass_rby_spmd_c__(idp,nng,buf1,buf2,buf3,buf4,buf5,iroddl)
int *idp, *nng, *iroddl, *buf3;
my_real_c *buf1,*buf2,*buf4,*buf5;
{send_mass_rby_spmd_c(idp,nng,buf1,buf2,buf3,buf4,buf5,iroddl);}



void get_mass_c(idp, nng, nodbuf, ms, in)
int *idp, *nng, *nodbuf;
my_real_c *ms, *in;
{
int i, nn, lbuf;
my_real_c *mbuf, *ibuf;

    writer(fidw, (void *) idp, sizeof(int));    

    lbuf = *nng*sizeof(my_real_c);
    mbuf = (my_real_c *) malloc(lbuf);
    if (flagrot[*idp]) ibuf = (my_real_c *) malloc(lbuf);
    
    readr(fidr, (void *) mbuf, lbuf);    
    if (flagrot[*idp]) readr(fidr, (void *) ibuf, lbuf);    

    for (i = 0; i < *nng; i++)
    {
        nn = nodbuf[i]-1;
        ms[nn] = mbuf[i];
        if (flagrot[*idp]) in[nn] = ibuf[i];
    }    
    free(mbuf);
    if (flagrot[*idp]) free(ibuf);
}

void _FCALL GET_MASS_C(igd, nng, nodbuf, ms, in)
int *igd, *nng, *nodbuf;
my_real_c *ms, *in;
{
    get_mass_c(igd, nng, nodbuf, ms, in);
}
void get_mass_c_(igd, nng, nodbuf, ms, in)
int *igd, *nng, *nodbuf;
my_real_c *ms, *in;
{
    get_mass_c(igd, nng, nodbuf, ms, in);
}
void get_mass_c__(igd, nng, nodbuf, ms, in)
int *igd, *nng, *nodbuf;
my_real_c *ms, *in;
{get_mass_c(igd, nng, nodbuf, ms, in);}



void get_mass_rby_c(idp, nng, nodbuf, ms, in, x, npby, nrbody, rby, nnpby, nrby)
int *idp, *nng, *nodbuf, *nrbody, *npby, *nnpby, *nrby;
my_real_c *ms, *in, *x, *rby;
{
int i,k,l,nn, lbuf, idrby;
my_real_c *mbuf, *ibuf, *xbuf, *matrix_buf;

    writer(fidw, (void *) idp, sizeof(int));    

    lbuf = *nng*sizeof(my_real_c);
    mbuf = (my_real_c *) malloc(lbuf);
    xbuf = (my_real_c *) malloc(3*lbuf);    
    if (flagrot[*idp])
      {ibuf = (my_real_c *) malloc(lbuf);
       matrix_buf = (my_real_c *) malloc(9*lbuf);}
    
    readr(fidr, (void *) mbuf, lbuf);    
    if (flagrot[*idp]) readr(fidr, (void *) ibuf, lbuf);
    readr(fidr, (void *) xbuf, 3*lbuf);    
    readr(fidr, (void *) matrix_buf, 9*lbuf);
    
    for (i = 0; i < *nng; i++)
    {
       nn = nodbuf[i]-1;
       ms[nn] = mbuf[i];         
       if (flagrot[*idp]) in[nn] = ibuf[i];
       x[3*nn] = xbuf[3*i];
       x[3*nn+1] = xbuf[3*i+1];
       x[3*nn+2] = xbuf[3*i+2];
       for (k = 0; k < *nrbody; k++)
          if (npby[*nnpby*k]==nn+1)
        for (l = 0; l < 9; l++)
            rby[*nrby*k+16+l]=matrix_buf[9*i+l];                      
    }
        
    free(mbuf);
    if (flagrot[*idp]) free(ibuf);
    free(xbuf);    
}

void _FCALL GET_MASS_RBY_C(igd, nng, nodbuf, ms, in, x, npby, nrbody, rby, nnpby, nrby)
int *igd, *nng, *nodbuf, *nrbody, *npby, *nnpby, *nrby;
my_real_c *ms, *in, *x, *rby;
{
    get_mass_rby_c(igd, nng, nodbuf, ms, in, x, npby, nrbody, rby, nnpby, nrby);
}
void get_mass_rby_c_(igd, nng, nodbuf, ms, in, x, npby, nrbody, rby, nnpby, nrby)
int *igd, *nng, *nodbuf, *nrbody, *npby, *nnpby, *nrby;
my_real_c *ms, *in, *x, *rby;
{
    get_mass_rby_c(igd, nng, nodbuf, ms, in, x, npby, nrbody, rby, nnpby, nrby);
}
void get_mass_rby_c__(igd, nng, nodbuf, ms, in, x, npby, nrbody, rby, nnpby, nrby)
int *igd, *nng, *nodbuf, *nrbody, *npby, *nnpby, *nrby;
my_real_c *ms, *in, *x, *rby;
{get_mass_rby_c(igd, nng, nodbuf, ms, in, x, npby, nrbody, rby, nnpby, nrby);}



void get_mass_spmd_c(idp, nng, buf1, buf2)
int *idp, *nng;
my_real_c *buf1, *buf2;
{
int lbuf;

    writer(fidw, (void *) idp, sizeof(int));    
    lbuf = *nng*sizeof(my_real_c);
    readr(fidr, (void *) buf1, lbuf);    
    if (flagrot[*idp]) readr(fidr, (void *) buf2, lbuf);    
}
void _FCALL GET_MASS_SPMD_C(igd, nng, buf1, buf2)
int *igd, *nng;
my_real_c *buf1, *buf2;
{
    get_mass_spmd_c(igd, nng, buf1, buf2);
}
void get_mass_spmd_c_(igd, nng, buf1, buf2)
int *igd, *nng;
my_real_c *buf1, *buf2;
{
    get_mass_spmd_c(igd, nng, buf1, buf2);
}
void get_mass_spmd_c__(igd, nng, buf1, buf2)
int *igd, *nng;
my_real_c *buf1, *buf2;
{get_mass_spmd_c(igd, nng, buf1, buf2);}



void get_mass_rby_spmd_c(idp,nng,buf1,buf2,buf3,buf4)
int *idp, *nng;
my_real_c *buf1,*buf2,*buf3,*buf4;
{
int lbuf;

    writer(fidw, (void *) idp, sizeof(int));    
    lbuf = *nng*sizeof(my_real_c);
    readr(fidr, (void *) buf1, lbuf);    
    if (flagrot[*idp]) readr(fidr, (void *) buf2, lbuf);
    readr(fidr, (void *) buf3, 3*lbuf);
    readr(fidr, (void *) buf4, 9*lbuf);            
}
void _FCALL GET_MASS_RBY_SPMD_C(idp,nng,buf1,buf2,buf3,buf4)
int *idp, *nng;
my_real_c *buf1,*buf2,*buf3,*buf4;
{
    get_mass_rby_spmd_c(idp,nng,buf1,buf2,buf3,buf4);
}
void get_mass_rby_spmd_c_(idp,nng,buf1,buf2,buf3,buf4)
int *idp, *nng;
my_real_c *buf1,*buf2,*buf3,*buf4;
{
    get_mass_rby_spmd_c(idp,nng,buf1,buf2,buf3,buf4);
}
void get_mass_rby_spmd_c__(idp,nng,buf1,buf2,buf3,buf4)
int *idp, *nng;
my_real_c *buf1,*buf2,*buf3,*buf4;
{get_mass_rby_spmd_c(idp,nng,buf1,buf2,buf3,buf4);}



void check_dtnoda_c(i7kglo)
int *i7kglo;
{
    if(i7flag == 0)
    {
        writer(fidw, (void *)i7kglo, sizeof(int));
        readr (fidr, (void *)i7kglo, sizeof(int));
        i7flag = *i7kglo;
    }
}
void check_dtnoda_c_(i7kglo)
int *i7kglo;
{
    check_dtnoda_c(i7kglo);
}
void _FCALL CHECK_DTNODA_C(i7kglo)
int *i7kglo;
{
    check_dtnoda_c(i7kglo);
}
void check_dtnoda_c__(i7kglo)
int *i7kglo;
{check_dtnoda_c(i7kglo);}
/******************************************************************/


void send_data_c(idp, nng, nodbuf, fx, fr, stx, str, vx,  vr,  ms,  in,  dx, x, typ, npas, rby, tag_rby, add_rby, rbylnk, kin, dr, dt2, iex, off_sph, numsph_glo, nrby)
int *idp, *nng, *nodbuf, *typ, *npas, *tag_rby, *add_rby, *rbylnk, *kin, *iex, *off_sph, *numsph_glo, *nrby;
my_real_c *fx, *fr, *stx, *str, *vx, *vr, *ms, *in, *rby, *dt2, *x;
double *dx,*dr;
{
int buflen, lbuf, rest, next, chunk;
int i, j, k, nn, nm, offset;

    rest = *nng;
        if (*iex == 1) off_link = 0;
        
    if(*typ != 7)    
    {
                chunk = rest / nthreads;
            #pragma omp parallel for schedule(static,chunk) if (chunk>350) private(k,i,j,nm,nn)                                
        for (next = 0; next < rest; next++)
        {
                        i = off_link + next;
            k = 3*i;
            nm = nodbuf[next]-1;
            nn = 3*nm;
            for (j = 0; j < 3; j++)
            {
                com->fx_buf[k+j] =  fx[nn+j];
                com->fr_buf[k+j] =  fr[nn+j];
                                if((*typ < 4)||(*npas == 0)) com->vx_buf[k+j] =  vx[nn+j];
                                if((*typ == 5)&&(*kin == 1))
                                  {dr[3*next+j] += *dt2*vr[nn+j];
                                   com->dx_buf[2*k+j] =  dx[nn+j];
                                   com->dx_buf[2*k+j+3] =  dr[3*next+j];}                                   
                                else
                                  {com->dx_buf[k+j] =  dx[nn+j];}                                  
            }
            com->mass_buf[i] = ms[nm];
            if(*typ == 5) com->sx_buf[i] = stx[nm];
            if(flagrot[*idp])
            {
                if(*typ == 5) com->sr_buf[i] = str[nm];
                com->iner_buf[i] = in[nm];
                if (*rbylnk==1)
                           for (j = 0; j < 9; j++) 
                      com->iner_rby_buf[9*i+j] = rby[*nrby*tag_rby[*add_rby+next]+j+16];                
                if((*typ < 4)||(*npas == 0)) 
                           for (j = 0; j < 3; j++) com->vr_buf[k+j] =  vr[nn+j];
            }
            /************ change of state - activation or deactivation of SPH - coordinates are transmitted instead of forces**************/
            if (flg_sphinout == 1)
              {com->iactv[i] = 0;
               if (*numsph_glo > 0)
                 if (off_sph[nm] != 0)
                   {/*printf("desactivation %d %d - %e %e %e\n",nm,off_sph[nm],x[nn],x[nn+1],x[nn+2]);*/
                for (j = 0; j < 3; j++) {com->fx_buf[k+j] =  x[nn+j];}
                    com->iactv[i] = off_sph[nm];}}
            
        }              
        off_link += rest;
    }    
}

/******************************************************************/
void _FCALL SEND_DATA_C(idp, nng, nodbuf, fx, fr, stx, str,vx,  vr,  ms,  in,  dx, x, typ, npas, rby, tag_rby, add_rby, rbylnk, kin, dr, dt2, iex, off_sph, numsph_glo, nrby)
int *idp, *nng, *nodbuf, *typ, *npas, *tag_rby, *add_rby, *rbylnk, *kin, *iex, *off_sph, *numsph_glo, *nrby;
my_real_c *fx, *fr, *stx, *str, *vx, *vr, *ms, *in, *rby, *dt2, *x;
double *dx, *dr;
{
    send_data_c(idp, nng, nodbuf, fx, fr, stx, str, vx,  vr,  ms,  in,  dx, x, typ, npas, rby, tag_rby, add_rby, rbylnk, kin, dr, dt2, iex, off_sph, numsph_glo, nrby);
}
void send_data_c_(idp, nng, nodbuf,fx, fr, stx, str, vx,  vr,  ms,  in, dx, x, typ, npas, rby, tag_rby, add_rby, rbylnk, kin, dr, dt2, iex, off_sph, numsph_glo, nrby)
int *idp, *nng, *nodbuf, *typ, *npas, *tag_rby, *add_rby, *rbylnk, *kin, *iex, *off_sph, *numsph_glo, *nrby;
my_real_c *fx, *fr, *stx, *str, *vx, *vr, *ms, *in, *rby, *dt2, *x;
double *dx, *dr;
{
    send_data_c(idp, nng, nodbuf, fx, fr, stx, str, vx,  vr,  ms,  in,  dx, x, typ, npas, rby, tag_rby, add_rby, rbylnk, kin, dr, dt2, iex, off_sph, numsph_glo, nrby);
}
void send_data_c__(idp, nng, nodbuf,fx, fr, stx, str, vx,  vr,  ms,  in,  dx, x, typ, npas, rby, tag_rby, add_rby, rbylnk, kin, dr, dt2, iex, off_sph, numsph_glo, nrby)
int *idp, *nng, *nodbuf, *typ, *npas, *tag_rby, *add_rby, *rbylnk, *kin, *iex, *off_sph, *numsph_glo, *nrby;
my_real_c *fx, *fr, *stx, *str, *vx, *vr, *ms, *in, *rby, *dt2, *x;
double *dx, *dr;
{send_data_c(idp, nng, nodbuf, fx, fr, stx, str, vx,  vr,  ms,  in,  dx, x, typ, npas, rby, tag_rby, add_rby, rbylnk, kin, dr, dt2, iex, off_sph, numsph_glo, nrby);}
/******************************************************************/

void send_data_nl_c(idp, nng, iadd_nl, fx, vx,  ms, npas, iex)
int *idp, *nng, *iadd_nl, *npas, *iex;
my_real_c *fx, *vx, *ms;
{
int rest, next, chunk;
int i;

    rest = *nng;
    chunk = rest / nthreads;

    #pragma omp parallel for schedule(static,chunk) if (chunk>350) private(i)                                
    for (next = 0; next < rest; next++)
      {
       i = next;
       com->mass_buf[off_link+i] = ms[iadd_nl[i]-1];
       com->fx_buf[3*(off_link+i)] = fx[iadd_nl[i]-1];    
      }

    if (*npas==0)    
      {
        #pragma omp parallel for schedule(static,chunk) if (chunk>350) private(i)                                
        for (next = 0; next < rest; next++)
          {
           i = next;
           com->vx_buf[3*(off_link+i)] = vx[iadd_nl[i]-1];
          }              
      }
             
    off_link += rest;     
}

/******************************************************************/
void _FCALL SEND_DATA_NL_C(idp, nng, iadd_nl, fx, vx,  ms, npas, iex)
int *idp, *nng, *iadd_nl, *npas, *iex;
my_real_c *fx, *vx, *ms;
{
    send_data_nl_c(idp, nng, iadd_nl, fx, vx,  ms, npas, iex);
}
void send_data_nl_c_(idp, nng, iadd_nl, fx, vx,  ms, npas, iex)
int *idp, *nng, *iadd_nl, *npas, *iex;
my_real_c *fx, *vx, *ms;
{
    send_data_nl_c(idp, nng, iadd_nl, fx, vx,  ms, npas, iex);
}
void send_data_nl_c__(idp, nng, iadd_nl, fx, vx,  ms, npas, iex)
int *idp, *nng, *iadd_nl, *npas, *iex;
my_real_c *fx, *vx, *ms;
{send_data_nl_c(idp, nng, iadd_nl, fx, vx,  ms, npas, iex);}
/******************************************************************/

/* el51g1+++ */
void send_data_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, bufr5, bufr6, bufr7, bufr8, bufr9, buf_rby, flg_rby, typ, npas, iex)
int *idp, *nng, *typ, *npas, *flg_rby, *iex;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4, *bufr5, *bufr6, *bufr8, *bufr9, *buf_rby;
double *bufr7;
{
int buflen, lbuf, rest, next, nn, nm, i, j, k, chunk;

    rest = *nng;
        if (*iex == 1) off_link = 0;
        
    if(*typ != 7)    
    {
                chunk = rest / nthreads;
            #pragma omp parallel for schedule(static,chunk) if (chunk>350) private(k,i,nn,nm)                                
        for (next = 0; next < rest; next++)
        {
                        i = off_link + next;
            k = 3*i;
                        nm = next;
                        nn = 3*next;
            for (j = 0; j < 3; j++)
            {
                com->fx_buf[k+j] =  bufr1[nn+j];
                com->fr_buf[k+j] =  bufr2[nn+j];
                                if((*typ < 4)||(*npas == 0)) com->vx_buf[k+j] =  bufr5[nn+j];
                                com->dx_buf[k+j] =  bufr7[nn+j];                                  
            }
            com->mass_buf[i] = bufr8[nm];
            if(*typ == 5) com->sx_buf[i] = bufr3[nm];
            if(flagrot[*idp])
            {
                if(*typ == 5) com->sr_buf[i] = bufr4[nm];
                com->iner_buf[i] = bufr9[nm];
                if (*flg_rby == 1)
                           for (j = 0; j < 9; j++) 
                      com->iner_rby_buf[9*i+j] = buf_rby[9*next+j];                
                if((*typ < 4)||(*npas == 0)) 
                           for (j = 0; j < 3; j++) com->vr_buf[k+j] =  bufr6[nn+j];
            }
        }              
        off_link += rest;
    }    
}

/******************************************************************/
void _FCALL SEND_DATA_SPMD_C(idp, nng, bufr1, bufr2, bufr3, bufr4, bufr5, bufr6, bufr7, bufr8, bufr9, buf_rby, flg_rby, typ, npas, iex)
int *idp, *nng, *typ, *npas, *flg_rby, *iex;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4, *bufr5, *bufr6, *bufr8, *bufr9, *buf_rby;
double *bufr7;
{
    send_data_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, bufr5, bufr6, bufr7, bufr8, bufr9, buf_rby, flg_rby, typ, npas, iex);
}
void send_data_spmd_c_(idp, nng, bufr1, bufr2, bufr3, bufr4, bufr5, bufr6, bufr7, bufr8, bufr9, buf_rby, flg_rby, typ, npas, iex)
int *idp, *nng, *typ, *npas, *flg_rby, *iex;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4, *bufr5, *bufr6, *bufr8, *bufr9, *buf_rby;
double *bufr7;
{
    send_data_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, bufr5, bufr6, bufr7, bufr8, bufr9, buf_rby, flg_rby, typ, npas, iex);
}
void send_data_spmd_c__(idp, nng, bufr1, bufr2, bufr3, bufr4, bufr5, bufr6, bufr7, bufr8, bufr9, buf_rby, flg_rby, typ, npas, iex)
int *idp, *nng, *typ, *npas, *flg_rby, *iex;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4, *bufr5, *bufr6, *bufr8, *bufr9, *buf_rby;
double *bufr7;
{
        send_data_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, bufr5, bufr6, bufr7, bufr8, bufr9, buf_rby, flg_rby, typ, npas, iex);}
    
/******************************************************************/

/* el51g1--- */

void get_stiff_c(idp, nng, nodbuf, ms, ir, stx, str, typ, npas, iex)
int *idp, *nng, *nodbuf, *typ, *npas, *iex;
my_real_c *ms, *ir, *stx, *str;
{
int buflen, lbuf, rest, next;
int i, j, k, nn, nm, offset, chunk;
my_real_c df, dm;    

    rest = *nng;
    if (*iex == 1) off_link = 0;
    
    if(*typ == 5)    
      { 
         chunk = rest / nthreads;
         #pragma omp parallel for schedule(static,chunk) if (chunk>350) private(i,k,nm,nn)             
         for (next = 0; next < rest; next++) {
            i  = off_link + next;
            k = 3*i;
            nm = (nodbuf[next]-1);
            nn = 3*nm;
            if (*npas == 0) ms[nm] = com->mass_buf[i];
            stx[nm] = com->sx_buf[i];
            if (flagrot[*idp]) {
                if (*npas == 0) ir[nm] = com->iner_buf[i];
                str[nm] = com->sr_buf[i];
                }
            }                 
       }
       off_link += rest;      
}
/******************************************************************/
void _FCALL GET_STIFF_C(idp, nng, nodbuf, ms, ir, stx, str, typ, npas, iex)
int    *idp, *nng, *nodbuf, *typ, *npas, *iex;
my_real_c *ms, *ir, *stx, *str;
{   get_stiff_c(idp, nng, nodbuf, ms, ir, stx, str, typ, npas, iex);  }

void get_stiff_c_(idp, nng, nodbuf, ms, ir, stx, str, typ, npas, iex)
int    *idp, *nng, *nodbuf, *typ, *npas, *iex;
my_real_c *ms, *ir, *stx, *str;
{   get_stiff_c(idp, nng, nodbuf, ms, ir, stx, str, typ, npas, iex);  }

void get_stiff_c__(idp, nng, nodbuf, ms, ir, stx, str, typ, npas, iex)
int    *idp, *nng, *nodbuf, *typ, *npas, *iex;
my_real_c *ms, *ir, *stx, *str;
{   get_stiff_c(idp, nng, nodbuf, ms, ir, stx, str, typ, npas, iex);  }
/******************************************************************/

void get_stiff_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, npas, iex, nglob)
int *idp, *nng, *typ, *npas, *iex, *nglob;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4;
{
int buflen, lbuf, rest, next, nm, nn, i, k, chunk;
my_real_c df, dm;


    rest = *nglob;
    if (*iex == 1) off_link = 0;
   
    if(*typ == 5)    
      { 
         chunk = rest / nthreads;
         #pragma omp parallel for schedule(static,chunk) if (chunk>350) private(i,k,nm,nn)             
         for (next = 0; next < rest; next++) {               
            i = off_link + masterdb[*iex-1][next];                
            k = 3*i;
            nm = next;
            nn = 3*nm;
            if (*npas == 0) bufr1[nm] = com->mass_buf[i];            
            bufr2[nm] = com->sx_buf[i];
            if (flagrot[*idp]) {
                if (*npas == 0) bufr3[nm] = com->iner_buf[i];
                bufr4[nm] = com->sr_buf[i];
                }
            }                 
       }      
       off_link += *nng;
            
}
/******************************************************************/
void _FCALL GET_STIFF_SPMD_C(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, npas, iex, nglob)
int    *idp, *nng, *typ, *npas, *iex, *nglob;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4;
{   get_stiff_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, npas, iex, nglob);  }

void get_stiff_spmd_c_(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, npas, iex, nglob)
int    *idp, *nng, *typ, *npas, *iex, *nglob;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4;
{   get_stiff_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, npas, iex, nglob);  }

void get_stiff_spmd_c__(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, npas, iex, nglob)
int    *idp, *nng, *typ, *npas, *iex, *nglob;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4;
{   get_stiff_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, npas, iex, nglob);  }

/******************************************************************/

void get_force_c(idp, nng, nodbuf, wf, wm, wf2, wm2, v, vr, fx, fr, ms, in, x, xdp, dx, typ, kin, wgt, iex, iresp, tfext)
int *idp, *nng, *nodbuf, *typ, *kin, *wgt, *iex, *iresp;
my_real_c *wf, *wm, *wf2, *wm2, *v, *vr, *fx, *fr, *ms, *in, *x, *dx;
double *xdp,*tfext;
{
int buflen, lbuf, rest, next, weight;
int i, j, k, nn, nm, chunk;
my_real_c df, dm, wfl,wf2l,wml,wm2l;

    rest = *nng;

    if (*iex == 1) off_link = 0;
    
    if (*typ != 7) 
    {                
        chunk = rest / nthreads;            
        #pragma omp parallel for schedule(static,chunk) if (chunk>350) private(i,k,nm,nn,weight,j,dm,df) reduction(+:wfl,wf2l,wml,wm2l)
        for (next = 0; next < rest; next++)   {                  
            i = off_link + next;
            k = 3*i;
            nm = (nodbuf[next]-1);
            nn = 3*nm;
            weight = wgt[nm];
            if ((*typ == 5)&&(*kin ==1)) ms[nm] = com->mass_buf[i];
            if ((*typ == 5)&&(*kin ==1)) in[nm] = com->iner_buf[i]; 
            for (j = 0; j < 3; j++)   {
               if (*typ < 4) v[nn+j] = com->vx_buf[k+j];
           /************ Activation/deactivation of SPH particles ***********/
            if (flg_sphinout == 1){
                if (com->iactv[i] == 1){                      /* activation of particle */
                    x[nn+j] = com->fx_buf[k+j];
                    if (*iresp==1) {xdp[nn+j] = com->fx_buf[k+j];}
                    com->fx_buf[k+j] = 0;
                    v[nn+j]  = com->vx_buf[k+j] ;
                    dx[nn+j] = com->dx_buf[k+j];
                    weight = 0;
               }else{
                    if (com->iactv[i] == -1){                      /* deactivation of particle */
                        x[nn+j] = com->fx_buf[k+j];
                        if (*iresp==1) xdp[nn+j] = com->fx_buf[k+j];
                        com->fx_buf[k+j] = 0;
                        v[nn+j] = 0;
                        dx[nn+j] = 0;
                        weight = 0;
                    }
               }
            }
            /*if (j==0) *tfext = *tfext - 0.5*weight*ms[nm]*(v[nn]*v[nn]+v[nn+1]*v[nn+1]+v[nn+2]*v[nn+2]);*/            
           /*****************************************/      
               df = weight*(com->fx_buf[k+j]*ms[nm] - fx[nn+j]);
           fx[nn+j] = com->fx_buf[k+j]*ms[nm];           
               wfl += df * v[nn+j] / 2.0;
           if (ms[nm] > 0) wf2l += df * fx[nn+j] / (2.0 * ms[nm]);           
               if (flagrot[*idp])
             {if (*typ < 4) vr[nn+j] = com->vr_buf[k+j];
          dm = weight*(com->fr_buf[k+j]*in[nm] - fr[nn+j]);
          fr[nn+j] = com->fr_buf[k+j]*in[nm];
          wml += dm * vr[nn+j] / 2.0;
          if (in[nm] > 0) wm2l += dm * fr[nn+j] / (2.0 * in[nm]);}     
               }
            }
        
        *wf += wfl;
        *wf2 += wf2l;
        *wm += wml;
        *wm2 += wm2l; 
        off_link += rest;        
    }    
}

/******************************************************************/
void _FCALL GET_FORCE_C(idp, nng, nodbuf, wf, wm, wf2, wm2, v, vr, fx, fr, ms, in, x, xdp, dx, typ, kin, wgt, iex, iresp, tfext)
int *idp, *nng, *nodbuf, *typ, *kin, *wgt, *iex, *iresp;
my_real_c *wf, *wm, *wf2, *wm2, *v, *vr, *fx, *fr, *ms, *in, *x, *dx;
double *xdp,*tfext;
{    get_force_c(idp, nng, nodbuf, wf, wm, wf2, wm2, v, vr, fx, fr, ms, in, x, xdp, dx, typ, kin, wgt, iex, iresp, tfext);}
void get_force_c_(idp, nng, nodbuf, wf, wm, wf2, wm2, v, vr, fx, fr, ms, in, x, xdp, dx, typ, kin, wgt, iex, iresp, tfext)
int *idp, *nng, *nodbuf, *typ, *kin, *wgt, *iex, *iresp;
my_real_c *wf, *wm, *wf2, *wm2, *v, *vr, *fx, *fr, *ms, *in, *x, *dx;
double *xdp,*tfext;
{    get_force_c(idp, nng, nodbuf, wf, wm, wf2, wm2, v, vr, fx, fr, ms, in, x, xdp, dx, typ, kin, wgt, iex, iresp, tfext);}
void get_force_c__(idp, nng, nodbuf, wf, wm, wf2, wm2, v, vr, fx, fr, ms, in, x, xdp, dx, typ, kin, wgt, iex, iresp, tfext)
int *idp, *nng, *nodbuf, *typ, *kin, *wgt, *iex, *iresp;
my_real_c *wf, *wm, *wf2, *wm2, *v, *vr, *fx, *fr, *ms, *in, *x, *dx;
double *xdp,*tfext;
{    get_force_c(idp, nng, nodbuf, wf, wm, wf2, wm2, v, vr, fx, fr, ms, in, x, xdp, dx, typ, kin, wgt, iex, iresp, tfext);}
/******************************************************************/

void get_force_nl_c(idp, nng, iadd_nl, fx, ms, iex)
int *idp, *nng, *iadd_nl, *iex;
my_real_c *fx, *ms;
{
int rest, next;
int i, chunk;

    rest = *nng;

    if (*iex == 1) off_link = 0;
                   
    chunk = rest / nthreads;            
    #pragma omp parallel for schedule(static,chunk) if (chunk>350) private(i)
    for (next = 0; next < rest; next++)
      {
       i = next;
       fx[iadd_nl[i]-1] = com->fx_buf[3*(off_link+i)]*ms[iadd_nl[i]-1];    
      }       

      off_link += rest;          
}

/******************************************************************/
void _FCALL GET_FORCE_NL_C(idp, nng, iadd_nl, fx, ms, iex)
int *idp, *nng, *iadd_nl, *iex;
my_real_c *fx, *ms;
{    get_force_nl_c(idp, nng, iadd_nl, fx, ms, iex);}
void get_force_nl_c_(idp, nng, iadd_nl, fx, ms, iex)
int *idp, *nng, *iadd_nl, *iex;
my_real_c *fx, *ms;
{    get_force_nl_c(idp, nng, iadd_nl, fx, ms, iex);}
void get_force_nl_c__(idp, nng, iadd_nl, fx, ms, iex)
int *idp, *nng, *iadd_nl, *iex;
my_real_c *fx, *ms;
{    get_force_nl_c(idp, nng, iadd_nl, fx, ms, iex);}
/******************************************************************/

void get_force_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, iex, nglob)
int *idp, *nng, *typ, *iex, *nglob;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4;
{
int buflen, lbuf, rest, next, chunk, i, j, k, nn, nm;

    rest = *nglob;
    if (*iex == 1) off_link = 0;
   
    if(*typ != 7)    
      { 
         chunk = rest / nthreads;
         #pragma omp parallel for schedule(static,chunk) if (chunk>350) private(i,k,nm,nn)             
         for (next = 0; next < rest; next++) {
            i = off_link + masterdb[*iex-1][next]; 
            k = 3*i;
            nm = next;
            nn = 3*nm; 
            for (j = 0; j < 3; j++)   {
               if (*typ < 4) bufr3[nn+j] = com->vx_buf[k+j]; 
           bufr1[nn+j] = com->fx_buf[k+j];                  
               if (flagrot[*idp])
             {if (*typ < 4) bufr4[nn+j] = com->vr_buf[k+j];
          bufr2[nn+j] = com->fr_buf[k+j];}
               }         
            }                 
       }
       off_link += *nng;    
}

/******************************************************************/
void _FCALL GET_FORCE_SPMD_C(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, iex, nglob)
int *idp, *nng, *typ, *iex, *nglob;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4;
{    get_force_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, iex, nglob);}
void get_force_spmd_c_(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, iex, nglob)
int *idp, *nng, *typ, *iex, *nglob;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4;
{    get_force_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, iex, nglob);}
void get_force_spmd_c__(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, iex, nglob)
int *idp, *nng, *typ, *iex, *nglob;
my_real_c *bufr1, *bufr2, *bufr3, *bufr4;
{    get_force_spmd_c(idp, nng, bufr1, bufr2, bufr3, bufr4, typ, iex, nglob);}


static void do_activ_c(iflg)
int *iflg;
{
    readr(fidr, (void *) iflg, sizeof(int));    
}

void _FCALL DO_ACTIV_C(iflg)
int *iflg;
{ do_activ_c(iflg);  }
void do_activ_c_(iflg)
int *iflg;
{  do_activ_c(iflg);}
void do_activ_c__(iflg)
int *iflg;
{  do_activ_c(iflg);}

/**************************************/

void send_mass_kine_c(idp, nng, nodbuf, ms, in, iex, offset)
int *idp, *nng, *nodbuf, *iex, *offset;
my_real_c *ms,*in;
{
int buflen, lbuf, rest, next;
int i, j, k, nn, nm, chunk;

    rest = *nng; 
        
        chunk = rest / nthreads;
        #pragma omp parallel for schedule(static,chunk) if (chunk>350) private(k,i,nm,nn)                                
        for (next = 0; next < rest; next++)
          {
                        i = *offset + next;
            k = 3*i;
            nm = nodbuf[next]-1;
            nn = 3*nm;
            com->mass_buf[i] = ms[nm];
            if(flagrot[*idp]) com->iner_buf[i] = in[nm];
       }                  
}

void _FCALL SEND_MASS_KINE_C(idp, nng, nodbuf, ms, in, iex, offset)
int *idp, *nng, *nodbuf, *iex, *offset;
my_real_c *ms,*in;
{ send_mass_kine_c(idp, nng, nodbuf, ms, in, iex, offset); }
void send_mass_kine_c_(idp, nng, nodbuf, ms, in, iex, offset)
int *idp, *nng, *nodbuf, *iex, *offset;
my_real_c *ms,*in;
{ send_mass_kine_c(idp, nng, nodbuf, ms, in, iex, offset); }
void send_mass_kine_c__(idp,nng, nodbuf, ms, in, iex, offset)
int *idp, *nng, *nodbuf, *iex, *offset;
my_real_c *ms,*in;
{ send_mass_kine_c(idp, nng, nodbuf, ms, in, iex, offset); }

/**************************************/


void get_displ_c(idp, nng, nodbuf, x)
int *idp, *nng, *nodbuf;
my_real_c *x;
{
int buflen, lbuf, rest, next;
int i, j, k, nn, nm;

    writer(fidw, (void *) idp, sizeof(int));    

    rest = *nng;
    next = 0;
    while(rest > 0)
    {
        buflen = (rest/nbufvar > 0 ? nbufvar : rest%nbufvar);
        lbuf = buflen * sizeof(my_real_c);
        readr(fidr, (void *) dx_buf, 3*lbuf);    

        for (i = 0; i < buflen; i++, next++)
        {
            k = 3*i;
            nm = (nodbuf[next]-1);
            nn = 3*nm;
            for (j = 0; j < 3; j++)
               x[nn+j]  = dx_buf[k+j];  
         }
        rest -= buflen;
    }    
}

void _FCALL GET_DISPL_C(idp, nng, nodbuf, x)
int *idp, *nng, *nodbuf;
my_real_c *x;
{ get_displ_c(idp, nng, nodbuf, x); }
void get_displ_c_(idp, nng, nodbuf, x)
int *idp, *nng, *nodbuf;
my_real_c *x;
{ get_displ_c(idp, nng, nodbuf, x); }
void get_displ_c__(idp,nng, nodbuf, x)
int *idp, *nng, *nodbuf;
my_real_c *x;
{ get_displ_c(idp, nng, nodbuf, x); }
/**************************************/


 void get_displ_spmd_c(idp, nng, bufr)
int *idp, *nng;
my_real_c *bufr;
{
int buflen, lbuf, rest, next;

    writer(fidw, (void *) idp, sizeof(int));    

    rest = *nng;
    next = 0;
    while(rest > 0)
    {
        buflen = (rest/nbufvar > 0 ? nbufvar : rest%nbufvar);
        lbuf = buflen * sizeof(my_real_c);
        readr(fidr, (void *) &bufr[3*next], 3*lbuf);    
                next += buflen;
        rest -= buflen;
    }    
}

void _FCALL GET_DISPL_SPMD_C(idp, nng, bufr)
int *idp, *nng;
my_real_c *bufr;
{ get_displ_spmd_c(idp, nng, bufr); }
void get_displ_spmd_c_(idp, nng, bufr)
int *idp, *nng;
my_real_c *bufr;
{ get_displ_spmd_c(idp, nng, bufr); }
void get_displ_spmd_c__(idp,nng, bufr)
int *idp, *nng;
my_real_c *bufr;
{ get_displ_spmd_c(idp, nng, bufr); }

/* el51g1 --- */


static void close_r2r_pipe_c()
{
#ifdef _WIN64
        CloseHandle(fidr);
        CloseHandle(fidw);
#else
        close(fidr);
        close(fidw);
#endif
}
void _FCALL CLOSE_R2R_PIPE_C()
{close_r2r_pipe_c();}
void close_r2r_pipe_c_()
{close_r2r_pipe_c();}
void close_r2r_pipe_c__()
{close_r2r_pipe_c();}

/***************Routines for socket communication***********************/

void connection_sock_init_c(sd)
int *sd;
{
  int num,i,nb,flags,compt,capt,compt2,port2,finished;
  struct  hostent *hp;
  struct linger so_linger;  
  char str[32];
  
#ifdef _WIN64 
  SOCKET sock;
  SOCKADDR_IN server, *serv;
  
  WSADATA WSAData;
  WSAStartup(MAKEWORD(2,0), &WSAData);
  
  struct addrinfo hints, *res;
  int err;
#else
  struct  hostent *gethostbyname();
  struct sockaddr_in server;
  int sock;
#endif
  char nom[512];
  char PUF[512];
  
  
#ifdef _WIN64 


    hp = gethostbyname(PUF);
    
    memcpy ( &(server.sin_addr.s_addr), hp->h_addr,  hp->h_length);
    server.sin_family = AF_INET;

#else

    gethostname(PUF,sizeof(PUF));
    hp = gethostbyname(PUF);
    
    memcpy ( &(server.sin_addr.s_addr), hp->h_addr,  hp->h_length);
    server.sin_family = AF_INET;
#endif

    sock = socket (AF_INET,SOCK_STREAM,0);
    compt = 0;
    compt2 = 0;
    capt = -1;
    finished = 0;
    
 /********************Iteration on ports to find a free one***************/
 
    while (finished == 0){

        while ((capt == -1)&&(compt < 3)){
           port2 = SERV_TCP_PORT1-compt;
           sock = socket (AF_INET,SOCK_STREAM,0);    
           so_linger.l_onoff = TRUE;
           so_linger.l_linger = 0;    


#ifdef _WIN64
           if (setsockopt(sock, SOL_SOCKET, SO_LINGER,(char *) &so_linger, sizeof(so_linger))==-1) 
                  {perror("setsockopt linger");exit(1);}

           printf("port:%i\n",port2);
           fflush(stdout);
           server.sin_port = htons(port2);
           capt = connect(sock, (struct sockaddr *) &server, sizeof(server));
#else
           if (setsockopt(sock, SOL_SOCKET, SO_LINGER, &so_linger, sizeof(so_linger))==-1) 
                  {perror("setsockopt linger");exit(1);}

           server.sin_port = htons(port2);
           capt = connect(sock, (struct sockaddr *)&server, sizeof(server));
#endif

           if ( capt != -1) finished = 1;
           
           if ( capt == -1) {
               compt++;    
#ifdef _WIN32
               closesocket(sock);
#else       
               close(sock);
#endif
           }
        }
        
        if ( compt >= 3) {
    
            if (compt2 == 0){
                  printf("\n Can't connect to rad2rad : waiting ...\n");}    
#ifdef _WIN32
            closesocket(sock);
            Sleep(700);
#else       
            close(sock);
            sleep(2);
#endif

            compt=0;
            compt2++;

            if (compt2 >= 10){
                  printf("\n ERROR : Can't connect to rad2rad after 10 naps\n\n");
                  exit(1);}
        }
    }

    *sd = sock;     
}

void _FCALL CONNECTION_SOCK_INIT_C(int *sd)
{connection_sock_init_c(sd);}

void connection_sock_init_c_(int *sd)
{connection_sock_init_c(sd);}

void connection_sock_init_c__(int *sd)
{connection_sock_init_c(sd);}

void send_sock_init_c(int *iroot,int *len,int *ispmd,int *sd,int *maxproc,int *imach)
{
  int num,i,nb,code,spmd,vers,compt2,imachine,flag_error;
#ifdef _WIN64
  SOCKET sock;
#else
  int sock;
#endif
  char nom[512];
  char PUF[512];
                    
    sock=*sd;
    num = *ispmd;
    nb = *maxproc;
    vers = 12180 + R4R8_C;
    imachine = *imach;
            
    for (i = 0; i < *len; i++)
    {
    root[i] = (char) iroot[i];
    }        
     
     if (send(sock,(send_type)root,70*sizeof(char), 0) == -1){
     perror("send name");
     exit(1);}
     
     if (send(sock,(send_type)&nb,sizeof(nb), 0) == -1){
     perror("send nb proc");
     exit(1);}
     
     if (send(sock,(send_type)&vers,sizeof(vers), 0) == -1){
     perror("send version");
     exit(1);}
     
     if (send(sock,(send_type)&imachine,sizeof(vers), 0) == -1){
     perror("send imach");
     exit(1);}                      
    
    if (recv(sock,(send_type)&flag_error,sizeof(flag_error), 0) <= 0){
    perror("recv rad2rad message");
    exit(1);}
    if (flag_error == 1) {printf("\n error message sent by rad2rad\n");exit(1);}    

    if (nb != 0){
       if (recv(sock,(send_type)nom,sizeof(nom), 0) <= 0){
           perror("recv top");
           exit(1);
       }
    }
}

void _FCALL SEND_SOCK_INIT_C(iroot,len,ispmd,sd,maxproc,imach)
int *iroot,*len,*ispmd,*sd,*maxproc,*imach;
{send_sock_init_c(iroot,len,ispmd,sd,maxproc,imach);}

void send_sock_init_c_(iroot,len,ispmd,sd,maxproc,imach)
int *iroot,*len,*ispmd,*sd,*maxproc,*imach;
{send_sock_init_c(iroot,len,ispmd,sd,maxproc,imach);}

void send_sock_init_c__(iroot,len,ispmd,sd,maxproc,imach)
int *iroot,*len,*ispmd,*sd,*maxproc,*imach;
{send_sock_init_c(iroot,len,ispmd,sd,maxproc,imach);}

void connection_sock_c(ispmd,sd,addr)
int *ispmd,*sd;
char *addr;
{
  int nb,port2,compt,capt;
  struct  hostent *hp;
  struct linger so_linger;  

#ifdef _WIN64
  SOCKET sock;
  SOCKADDR_IN server;
#else
  struct  hostent *gethostbyname();
  struct sockaddr_in server;  
  int sock;
#endif

  char nom[512];
  char PUF[512];
                 
    gethostname(PUF,sizeof(PUF));    
    hp = gethostbyname(addr);
    nb = *ispmd;

    memcpy ( &(server.sin_addr.s_addr), hp->h_addr,  hp->h_length);
    server.sin_family = AF_INET;
    
    compt = 0;
    capt = -1;
                 
/********************Iteration on ports to find a free one***************/
     
    while ((capt == -1) && compt < 3)              
    {port2 = SERV_TCP_PORT1-compt;
    sock = socket (AF_INET,SOCK_STREAM,0);    
    server.sin_port = htons(port2);
    so_linger.l_onoff = TRUE;
    so_linger.l_linger = 0;    
    if (setsockopt(sock, SOL_SOCKET, SO_LINGER,(send_type) &so_linger, sizeof(so_linger))==-1) {perror("setsockopt linger");exit(1);}     
    capt = connect(sock, (struct sockaddr *)&server, sizeof(server));     
    if ( capt == -1) 
       {compt++;
#ifdef _WIN64
        closesocket(sock);}}
#else       
        close(sock);}}
#endif           
               
    if ( compt >= 3) {
        perror("Can't connect socket...");
#ifdef _WIN64
        closesocket(sock);
#else       
        close(sock);
#endif    
        exit(1);
     }
     
/**************************************************************************************/     

     if (send(sock,(send_type)&nb,sizeof(nb), 0) == -1){
     perror("send ispmd");
     exit(1);}          
              
    *sd=sock;
    sock=*sd;
}

void _FCALL CONNECTION_SOCK_C(ispmd,sd,addr)
int *ispmd,*sd;
char *addr;
{connection_sock_c(ispmd,sd,addr);}

void connection_sock_c_(ispmd,sd,addr)
int *ispmd,*sd;
char *addr;
{connection_sock_c(ispmd,sd,addr);}

void connection_sock_c__(ispmd,sd,addr)
int *ispmd,*sd;
char *addr;
{connection_sock_c(ispmd,sd,addr);}

void mess_sock_c(sd)
int *sd;
{
#ifdef _WIN64
  SOCKET sock;
#else
  int sock;
#endif
  char nom[512];                    
    
     sock=*sd;    
    
     if (recv(sock,(send_type)nom,3*sizeof(char), 0) == -1)
        {perror("recv norm");
         exit(1);}     
}

void _FCALL MESS_SOCK_C(sd)
int *sd;
{mess_sock_c(sd);}

void mess_sock_c_(sd)
int *sd;
{mess_sock_c(sd);}

void mess_sock_c__(sd)
int *sd;
{mess_sock_c(sd);}

void send_sock_c(sd)
int *sd;
{
#ifdef _WIN64
  SOCKET sock;
#else
  int sock;
#endif
  char nom[512];                    
    
     sock=*sd;    
    
     if (send(sock,(send_type)buftop,3*sizeof(char), 0) == -1){
     perror("send norm");
     exit(1);}
}

void _FCALL SEND_SOCK_C(sd)
int *sd;
{send_sock_c(sd);}

void send_sock_c_(sd)
int *sd;
{send_sock_c(sd);}

void send_sock_c__(sd)
int *sd;
{send_sock_c(sd);}

/****************************************/

void get_sock_ibuf_c(sd, ibuf, len)
int *sd, *ibuf, *len;
{
#ifdef _WIN64
  SOCKET sock;
#else
  int sock;
#endif
     sock=*sd;
     if (recv(sock,(send_type)ibuf,*len*sizeof(int), 0) == -1){
     perror("send sock ibuf ");
     exit(1);}     
}
void _FCALL GET_SOCK_IBUF_C(sd, ibuf, len)
int *sd, *ibuf, *len;
{
    get_sock_ibuf_c(sd, ibuf, len);
}
void get_sock_ibuf_c_(sd, ibuf, len)
int *sd, *ibuf, *len;
{
    get_sock_ibuf_c(sd, ibuf, len);
}
void get_sock_ibuf_c__(sd, ibuf, len)
int *sd, *ibuf, *len;
{get_sock_ibuf_c(sd, ibuf, len);}

/****************************************/

void send_sock_ibuf_c(sd, ibuf, len)
int *sd, *ibuf, *len;
{
#ifdef _WIN64
  SOCKET sock;
#else
  int sock;
#endif
     sock=*sd;
     if (send(sock,(send_type)ibuf,*len*sizeof(int), 0) == -1){
     perror("send sock ibuf ");
     exit(1);}     
}
void _FCALL SEND_SOCK_IBUF_C(sd, ibuf, len)
int *sd, *ibuf, *len;
{
    send_sock_ibuf_c(sd, ibuf, len);
}
void send_sock_ibuf_c_(sd, ibuf, len)
int *sd, *ibuf, *len;
{
    send_sock_ibuf_c(sd, ibuf, len);
}
void send_sock_ibuf_c__(sd, ibuf, len)
int *sd, *ibuf, *len;
{send_sock_ibuf_c(sd, ibuf, len);}


/****************************************/

void send_sock_rbuf_c(sd, rbuf, len)
int *sd, *len;
my_real_c *rbuf;
{
#ifdef _WIN64
  SOCKET sock;
#else
  int sock;
#endif
     sock=*sd;
     if (send(sock,(send_type)rbuf,*len*sizeof(my_real_c), 0) == -1){
     perror("send sock rbuf ");
     exit(1);}     
}
void _FCALL SEND_SOCK_RBUF_C(sd, rbuf, len)
int *sd, *len;
my_real_c *rbuf;
{
    send_sock_rbuf_c(sd, rbuf, len);
}
void send_sock_rbuf_c_(sd, rbuf, len)
int *sd, *len;
my_real_c *rbuf;
{
    send_sock_rbuf_c(sd, rbuf, len);
}
void send_sock_rbuf_c__(sd, rbuf, len)
int *sd, *len;
my_real_c *rbuf;
{send_sock_rbuf_c(sd, rbuf, len);}

/****************************************/

void get_sock_rbuf_c(sd, rbuf, len)
int *sd, *len;
my_real_c *rbuf;
{
#ifdef _WIN64
  SOCKET sock;
#else
  int sock;
#endif
     sock=*sd;
     if (recv(sock,(send_type)rbuf,*len*sizeof(my_real_c), 0) == -1){
     perror("send sock rbuf ");
     exit(1);}     
}
void _FCALL GET_SOCK_RBUF_C(sd, rbuf, len)
int *sd, *len;
my_real_c *rbuf;
{
    get_sock_rbuf_c(sd, rbuf, len);
}
void get_sock_rbuf_c_(sd, rbuf, len)
int *sd, *len;
my_real_c *rbuf;
{
    get_sock_rbuf_c(sd, rbuf, len);
}
void get_sock_rbuf_c__(sd, rbuf, len)
int *sd, *len;
my_real_c *rbuf;
{get_sock_rbuf_c(sd, rbuf, len);}

/****************************************/

void send_sock_mess_c(sd, mess, len)
int *sd, *len;
char *mess;
{
#ifdef _WIN64
  SOCKET sock;
#else
  int sock;
#endif
     sock=*sd;
     if (send(sock,(send_type)mess,*len*sizeof(char), 0) == -1){
     perror("send sock mess ");
     exit(1);}     
}
void _FCALL SEND_SOCK_MESS_C(sd, mess, len)
int *sd, *len;
char *mess;
{
    send_sock_mess_c(sd, mess, len);
}
void send_sock_mess_c_(sd, mess, len)
int *sd, *len;
char *mess;
{
    send_sock_mess_c(sd, mess, len);
}

void send_sock_mess_c__(sd, mess, len)
int *sd, *len;
char *mess;
{send_sock_mess_c(sd, mess, len);}

/****************************************/

void get_sock_mess_c(sd, mess, len)
int *sd, *len;
char *mess;
{
#ifdef _WIN64
  SOCKET sock;
#else
  int sock;
#endif
     sock=*sd;
     if (recv(sock,(send_type)mess,*len*sizeof(char), 0) == -1){
     perror("send sock mess ");
     exit(1);}     
}
void _FCALL GET_SOCK_MESS_C(sd, mess, len)
int *sd, *len;
char *mess;
{
    get_sock_mess_c(sd, mess, len);
}
void get_sock_mess_c_(sd, mess, len)
int *sd, *len;
char *mess;
{
    get_sock_mess_c(sd, mess, len);
}

void get_sock_mess_c__(sd, mess, len)
int *sd, *len;
char *mess;
{get_sock_mess_c(sd, mess, len);}

/****************************************/

void close_sock_c(sd)
int *sd;
{
#ifdef _WIN64
  SOCKET sock;
#else
  int sock;
#endif
                  
     sock=*sd;
     
#ifdef _WIN64
     closesocket(sock);
     UnmapViewOfFile(shmv);
     CloseHandle(shmidv);
     if ((flag_siu == 0)||((flag_siu==1)&&(ispmd_glob==0))) CloseHandle(sem_int);
#else       
     close(sock);
     if (flag_siu == 0) {sem_destroy(&sem_int);}
     else if ((flag_siu==1)&&(ispmd_glob==0))  {sem_close(sem_glob);sem_unlink(semaphore_int);}
#endif    
}

void _FCALL CLOSE_SOCK_C(sd)
int *sd;
{close_sock_c(sd);}

void close_sock_c_(sd)
int *sd;
{close_sock_c(sd);}

void close_sock_c__(sd)
int *sd;
{close_sock_c(sd);}

/****************************************/

void get_name_c(name)
char *name;
{
/*memset(name,0,sizeof(name));*/    
 gethostname(name,512);   
}

void _FCALL GET_NAME_C(name)
char *name;
{get_name_c(name);}

void get_name_c_(name)
char *name;
{get_name_c(name);}

void get_name_c__(name)
char *name;
{get_name_c(name);}

/**************************************/

void exch_itag_c(idp, nng, nodbuf, itag, itag2, iex, offset, flag)
int *idp, *nng, *nodbuf, *iex, *offset, *itag, *itag2, *flag;
{
int buflen, lbuf, rest, next;
int i, j, k, nn, nm, chunk,nmods,nmod;

    rest = *nng;
    nmod = 0;
      
        /*chunk = rest / nthreads;
        #pragma omp parallel for schedule(static,chunk) if (chunk>350) private(k,i,nm,nn)*/                                
        for (next = 0; next < rest; next++)
          {
                        i = *offset + next;
            nm = nodbuf[next]-1;
            if (*flag == 0) /* *emission* */
                {com->itags[i] =itag[nm];
               com->itags2[i] =itag2[nm];}
            else if (*flag == 1) /* *reception* */
              {itag[nm] += com->itagr[i];
               itag2[nm] += com->itagr2[i];}
            
       }
}

void _FCALL EXCH_ITAG_C(idp, nng, nodbuf, itag, itag2, iex, offset, flag)
int *idp, *nng, *nodbuf, *iex, *offset, *itag, *itag2, *flag;
{ exch_itag_c(idp, nng, nodbuf, itag, itag2, iex, offset, flag); }
void exch_itag_c_(idp, nng, nodbuf, itag, itag2, iex, offset, flag)
int *idp, *nng, *nodbuf, *iex, *offset, *itag, *itag2, *flag;
{ exch_itag_c(idp, nng, nodbuf, itag, itag2, iex, offset, flag); }
void exch_itag_c__(idp,nng, nodbuf, itag, itag2, iex, offset, flag)
int *idp, *nng, *nodbuf, *iex, *offset, *itag, *itag2, *flag;
{ exch_itag_c(idp, nng, nodbuf, itag, itag2, iex, offset, flag); }

/****************************************/
void realloc_shmvs_c(newsize)
int newsize;
{
int i,*shmloc,new_tot_size,old_tot_size;

  old_tot_size = shmvr_size + shmvs_size;
  newsize = my_max(newsize,old_tot_size+150);
  new_tot_size = shmvr_size + newsize;
  
  shmloc = malloc(old_tot_size*sizeof(int));
  for (i = 0; i < old_tot_size; i++) shmloc[i] = shmv[i];
  
#ifdef _WIN64
  UnmapViewOfFile(shmv);
  CloseHandle(shmidv);
  compt_resize++;
  sprintf(add_shmv,"Adress_shmv_%d_%d_%d_%d",proc_id,ispmd_glob,r2r_id,compt_resize);
  shmidv = CreateFileMapping (INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, new_tot_size*sizeof(int), add_shmv);
  shmv = MapViewOfFile(shmidv, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0);
#elif 1
  munmap(shmv,old_tot_size*sizeof(int));
  if (ftruncate(shmidv, new_tot_size*sizeof(int)) == -1) {perror("ftruncate");exit(1);}
  shmv = mmap(NULL, new_tot_size*sizeof(int),PROT_WRITE | PROT_READ, MAP_SHARED, shmidv, 0);
  if (shmv == MAP_FAILED) {perror("mmap");exit(1);}
#endif

  for (i = 0; i < new_tot_size; i++) shmv[i] = 0;
  for (i = 0; i < old_tot_size; i++) shmv[i] = shmloc[i];
    
  shmvs_size = newsize;
  com->tagelr = shmv ;
  com->tagels = shmv + shmvr_size;
  free(shmloc);
}

void realloc_shmvr_c(newsize)
int newsize;
{
int i,new_tot_size,old_tot_size;

  old_tot_size = shmvr_size + shmvs_size;
  newsize = my_max(newsize,old_tot_size+150);
  new_tot_size = shmvs_size + newsize;

#ifdef _WIN64
  UnmapViewOfFile(shmv);
  CloseHandle(shmidv);
  compt_resize++;
  sprintf(add_shmv,"Adress_shmv_%d_%d_%d_%d",proc_id,ispmd_glob,r2r_id,compt_resize);
  shmidv = CreateFileMapping (INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, new_tot_size*sizeof(int), add_shmv);
  shmv = MapViewOfFile(shmidv, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0);
#elif 1
  munmap(shmv,old_tot_size*sizeof(int));
  if (ftruncate(shmidv, new_tot_size*sizeof(int)) == -1) {perror("ftruncate");exit(1);}
  shmv = mmap(NULL, new_tot_size*sizeof(int),PROT_WRITE | PROT_READ, MAP_SHARED, shmidv, 0);
  if (shmv == MAP_FAILED) {perror("mmap");exit(1);}
#endif

  shmvr_size = newsize;
  com->tagelr = shmv ;
  com->tagels = shmv + shmvr_size;
}

/**************************************/

void exch_tagel_c(int *ntagel,int *tagel,int *flag)
{
int i;

        if (((*ntagel)*3 > shmvr_size)&&(*flag == 1)) realloc_shmvr_c((*ntagel)*3);
        if (((*ntagel)*3 > shmvs_size)&&(*flag == 0)) realloc_shmvs_c((*ntagel)*3);
    
        for (i = 0; i < (*ntagel)*3; i++)
          {
            if (*flag == 0) /* *send* */
                {com->tagels[com->buf[2]+i] = tagel[i];}
            else if (*flag == 1) /* *receive* */
              {tagel[i] = com->tagelr[i];}
       }

         if (*flag == 0) com->buf[2] += (*ntagel)*3;       
}

void _FCALL EXCH_TAGEL_C(ntagel,tagel,flag)
int *ntagel,*tagel,*flag;
{ exch_tagel_c(ntagel,tagel,flag); }
void exch_tagel_c_(ntagel,tagel,flag)
int *ntagel,*tagel,*flag;
{ exch_tagel_c(ntagel,tagel,flag); }
void exch_tagel_c__(ntagel,tagel,flag)
int *ntagel,*tagel,*flag;
{ exch_tagel_c(ntagel,tagel,flag); }

/****************************************/
void get_shmbuf_c(val1,val2)
int *val1,*val2;
{
*val1 = com->buf[*val2-1];
}

void _FCALL GET_SHMBUF_C(val1,val2)
int *val1,*val2;
{get_shmbuf_c(val1,val2);}

void get_shmbuf_c_(val1,val2)
int *val1,*val2;
{get_shmbuf_c(val1,val2);}

void get_shmbuf_c__(val1,val2)
int *val1,*val2;
{get_shmbuf_c(val1,val2);}

/****************************************/
void send_shmbuf_c(val1,val2)
int *val1,*val2;
{
com->buf[*val2-1] = *val1;
}

void _FCALL SEND_SHMBUF_C(val1,val2)
int *val1,*val2;
{send_shmbuf_c(val1,val2);}

void send_shmbuf_c_(val1,val2)
int *val1,*val2;
{send_shmbuf_c(val1,val2);}

void send_shmbuf_c__(val1,val2)
int *val1,*val2;
{send_shmbuf_c(val1,val2);}

