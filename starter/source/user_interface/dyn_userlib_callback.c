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

#if CPP_mach != CPP_macosx64
#include <malloc.h>
#endif


#ifdef MYREAL4
#define my_real float
#endif

#ifdef MYREAL8
#define my_real  double
#endif


#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
#include <windows.h>
#include <process.h>
#include <io.h>
#include <sys\types.h>
#include <sys/stat.h>
#define _FCALL 
#elif 1
#include <sys/resource.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>
#define _FCALL
#endif

#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32


extern int GET_U_NUMTABLE(int * tid);
extern void GET_U_TABLE(int * itable,my_real *XX, my_real *YY);
extern void GET_U_VTABLE(int * itable, int * nel0, int * ipos,my_real *XX, my_real*YY, my_real *DYDX1);
extern void SET_U_SHLPLAS(int *USRNEL,my_real *SIGY,my_real *ETSE);
extern void SET_U_SOLPLAS(int *USRNEL, my_real*SIGY, my_real*PLA);
extern void USENS_SHIFT_AB(my_real * sensor);
extern void USENS_SHIFT_BA(my_real * sensor);
extern int GET_U_NUMSENS (int * idsens) ;
extern int GET_U_SENS_ID (int * idsens);
extern int SET_U_SENS_VALUE(int *nsens, int * ivar, my_real * var );
extern int GET_U_SENS_VALUE(int *nsens, int * ivar, my_real * var ) ;
extern int SET_U_SENS_MAXVALUE(int *nsens, int * ivar, my_real * var );
extern int GET_U_SENS_FPAR(int *nsens, int * ivar, my_real * var ) ;
extern int GET_U_SENS_IPAR(int *nsens, int * ivar, my_real * var );
extern int SET_U_SENS_ACTI(int *nsens);
extern int GET_U_SENS_ACTI(int *nsens) ;
extern my_real GET_U_SENS(int *usens);
extern my_real GET_U_SENS_DELAY(int *usens) ;
extern my_real GET_U_MAT(int *ivar, int * im) ;
extern my_real GET_U_GEO(int *ivar, int * ip) ;
extern int GET_U_PNU(int *ivar, int * ip,int * k);
extern int GET_U_MNU(int *ivar, int * ip,int * k);
extern int GET_U_PID(int * ip) ;
extern int GET_U_MID(int * im);
extern int GET_U_M(int * mid) ;
extern int GET_U_P(int * pid) ;
extern int GET_U_PROC() ;
extern int GET_U_TASK() ;
extern int GET_U_FUNC_N(int * ifunc) ;
extern my_real GET_U_FUNC_X(int * ifunc,int *n) ;
extern my_real GET_U_FUNC_Y(int * ifunc,int *n) ;
extern my_real GET_U_FUNC(int * ifunc,my_real *XX,my_real * DERI) ;
extern void GET_V_FUNC(int * ifunc,int * llt,my_real * xx, my_real * dydx,int * jpos) ;
extern int GET_U_NUMFUN(int * idfun) ;
extern int GET_U_FID(int * ifun) ;
extern int GET_U_CYCLE() ;
extern my_real GET_U_TIME();
extern my_real GET_U_ACCEL(int * nacc, my_real * ax, my_real * ay, my_real * az);
extern int GET_U_NUMACC(int * idacc) ;
extern int GET_U_NUMNOD(int * iu) ;
extern int GET_U_NOD_X(int * nod, my_real * x, my_real * y, my_real * z) ;
extern int GET_U_NOD_D(int * nod, my_real * dx, my_real * dy, my_real * dz);
extern int GET_U_NOD_V(int * nod, my_real * vx, my_real * vy, my_real * vz) ;
extern int GET_U_NOD_A(int * nod, my_real * ax, my_real * ay, my_real * az) ;
extern int GET_U_SKEW(int * idskw, int * n1,int* n2, int* n3, my_real * vect) ;
extern my_real GET_U_UVAR(int * iel, int * ilayer,int* ivar, int* nuvar);
extern void SET_SPRING_ELNUM(int * jft, int * jlt,int* ixr) ;
extern int GET_SPRING_ELNUM(int * iel) ;
extern int SET_U_GEO(int *ivar,my_real *a);
extern int SET_U_PNU(int *ivar, int * ip,int * k);
extern int RESET_U_GEO(int *ivar, int * ip,my_real * a);
extern my_real GET_U_FUNC_DERI(int * ifunc);
extern int SET_U_SENS_IPAR(int *ivar, int * var);
extern int SET_U_SENS_FPAR(int * ivar,my_real * var);
extern double FINTER (int *ifunc , double * XX, int* NPF, double *TF,double  *DERI);
extern void SET_DD_MAT_WEIGHT(my_real *MAT_WEIGHT1PT,my_real *MAT_WEIGHT5PT,int *elem_type);
extern void UELT_SPMD_ADDITIONAL_NODE(int *NODID);
extern void ARRET(int * mode);
extern void RAD_UMAT_INPUT_OPEN(char * filename,int * length,int * result);
extern void RAD_UMAT_INPUT_READ(char * line,int * len,int *size_read );
extern void RAD_UMAT_INPUT_REWIND();
extern void RAD_UMAT_CLOSE_INPUT();
extern void WRITE_IOUT(char* line,int* len);
extern int SET_U_SENS_SPMD_NODE_LIST(int *var, int * ivar);


void init_callback(void ** callback_array){

int i;

for (i=0; i<200; i++) callback_array[ i]=NULL;

/* initialize all pointers in an Array */
  callback_array[ 0]= FINTER;
  callback_array[ 1]= GET_U_NUMTABLE;
  callback_array[ 2]= GET_U_TABLE;
  callback_array[ 3]= GET_U_VTABLE;
/*  callback_array[ 4]= SET_U_SHLPLAS;        Engine Routine */
/*  callback_array[ 5]= SET_U_SOLPLAS;        Engine Routine */
/*  callback_array[ 6]= USENS_SHIFT_AB;       Engine Routine */
/*  callback_array[ 7]= USENS_SHIFT_BA;       Engine Routine */
/*  callback_array[ 8]= GET_U_NUMSENS;        Engine Routine */
/*  callback_array[ 9]= GET_U_SENS_ID;        Engine Routine */
/*  callback_array[10]= SET_U_SENS_VALUE;     Engine Routine */
/*  callback_array[11]= GET_U_SENS_VALUE;     Engine Routine */
/*  callback_array[12]= SET_U_SENS_MAXVALUE;  Engine Routine */
/*  callback_array[13]= GET_U_SENS_FPAR;      Engine Routine */
/*  callback_array[14]= GET_U_SENS_IPAR;      Engine Routine */
/*  callback_array[15]= SET_U_SENS_ACTI;      Engine Routine */
/*  callback_array[16]= GET_U_SENS_ACTI;      Engine Routine */
/*  callback_array[17]= GET_U_SENS;           Engine Routine */
/*  callback_array[18]= GET_U_SENS_DELAY;     Engine Routine */
  callback_array[19]= GET_U_MAT;
  callback_array[20]= GET_U_GEO;
  callback_array[21]= GET_U_PNU;
  callback_array[22]= GET_U_MNU;
  callback_array[23]= GET_U_PID;
  callback_array[24]= GET_U_MID;
  callback_array[25]= GET_U_M;
  callback_array[26]= GET_U_P;
/*  callback_array[27]= GET_U_PROC;           Engine Routine */
/*  callback_array[28]= GET_U_TASK;           Engine Routine */
  callback_array[29]= GET_U_FUNC_N;
/*  callback_array[30]= GET_U_FUNC_X;         Engine Routine */
  callback_array[31]= GET_U_FUNC_Y;
/*  callback_array[32]= GET_U_FUNC;           Engine Routine */
/*  callback_array[33]= GET_V_FUNC ;          Engine Routine */
  callback_array[34]= GET_U_NUMFUN;
/*  callback_array[35]= GET_U_FID;            Engine Routine */
/*  callback_array[36]= GET_U_CYCLE;          Engine Routine */
/*  callback_array[37]= GET_U_TIME;           Engine Routine */
/*  callback_array[38]= GET_U_ACCEL;          Engine Routine */
/*  callback_array[39]= GET_U_NUMACC;         Engine Routine */
/*  callback_array[40]= GET_U_NUMNOD;         Engine Routine */
/*  callback_array[41]= GET_U_NOD_X;          Engine Routine */
/*  callback_array[42]= GET_U_NOD_D;          Engine Routine */
/*  callback_array[43]= GET_U_NOD_V ;         Engine Routine */
/*  callback_array[44]= GET_U_NOD_A ;         Engine Routine */
  callback_array[45]= GET_U_SKEW;
/*  callback_array[46]= GET_U_UVAR;           Engine Routine */
/*  callback_array[47]= SET_SPRING_ELNUM;     Engine Routine */
/*  callback_array[48]= GET_SPRING_ELNUM;     Engine Routine */
  callback_array[49]= SET_U_GEO; 
  callback_array[50]= SET_U_PNU; 
  callback_array[51]= RESET_U_GEO; 
  callback_array[52]= GET_U_FUNC_DERI;
  callback_array[53]= SET_U_SENS_IPAR; 
  callback_array[54]= SET_U_SENS_FPAR; 
     callback_array[55]= WRITE_IOUT;  
  callback_array[56]= SET_DD_MAT_WEIGHT;
  callback_array[57]= UELT_SPMD_ADDITIONAL_NODE;
  callback_array[58]= ARRET;
//  callback_array[59] = SET_U_SENS_DEACTI;     Engine Routine 
//  callback_array[60] = GET_TABLE_VALUE;       Engine Routine 
//  callback_array[61] = GET_VTABLE_VALUE;      Engine Routine 
//  callback_array[62] = MAT_SOLID_GET_NOD_X;   Engine Routine 
//  callback_array[63] = MAT_SOLID_GET_NOD_V;   Engine Routine 
//  callback_array[64] = USERWINDOW_GET_A;      Engine Routine 
//  callback_array[65] = USERWINDOW_GET_AR;     Engine Routine 
  callback_array[66] = RAD_UMAT_INPUT_OPEN;
  callback_array[67] = RAD_UMAT_INPUT_READ;
  callback_array[68] = RAD_UMAT_INPUT_REWIND;
  callback_array[69] = RAD_UMAT_CLOSE_INPUT;
  callback_array[70] = SET_U_SENS_SPMD_NODE_LIST;


}



#elif 1
void init_callback(void * callback_array[200]){
 return ; 
}

#endif
