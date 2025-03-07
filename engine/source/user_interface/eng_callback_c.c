//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
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
#include <string.h>
#include <stdlib.h> 
#include <my_real_c.inc>

#define _FCALL


int *sav_buf[1000];

/* Set in other routines */
void    ARRET(int * n);
void    arret_(int * n);
void arret_c(int n) ;
void VINTER2DP (double    *tf,int *jad,int *jpos,int *jlen,int *llt, double    *xx,double    *dydx, double *yy);
void vinter2dp_(double    *tf,int *jad,int *jpos,int *jlen,int *llt, double    *xx,double    *dydx, double *yy);
void VINTER2   (my_real_c *tf,int *jad,int *jpos,int *jlen,int *llt, my_real_c *xx,my_real_c *dydx, my_real_c *yy);
void vinter2_  (my_real_c *tf,int *jad,int *jpos,int *jlen,int *llt, my_real_c *xx,my_real_c *dydx, my_real_c *yy);
/*
   ---------------------------------------------------------------------
*/

void sav_buf_point_(int *buf, int *i)
{
  if(*i<0||*i>=1000){
       fprintf(stderr,"ERROR in SAV_BUF: table overflow\n");
       arret_c(2);
  }
  sav_buf[*i] = buf;
}


void _FCALL SAV_BUF_POINT(int *buf,int *i)
{sav_buf_point_(buf,i);}

void sav_buf_point(int *buf,int *i)
{sav_buf_point_(buf,i);}


void sav_buf_point__(int *buf,int *i)
{sav_buf_point_(buf,i);}


void get_buf_point_(int *loca,int *ish,int *i)
{*ish = (sav_buf[*i] - loca);}

void _FCALL GET_BUF_POINT(int *loca,int *ish,int *i)
{*ish = (sav_buf[*i] - loca);}

void get_buf_point(int *loca,int *ish,int *i)
{*ish = (sav_buf[*i] - loca);}


void get_buf_point__(int *loca,int *ish,int *i)
{*ish = (sav_buf[*i] - loca);}


void get_var_user_f_ (int *buf,int *decalage,double * resultat)
{
 double *res;
   res= ((double*) (sav_buf[*buf]) )+ (*decalage -1);
   *resultat = *res;
}


void get_var_user_f(int *buf,int *decalage,double *resultat)
{get_var_user_f_(buf,decalage,resultat);}


void _FCALL GET_VAR_USER_F(int *buf,int *decalage,double *resultat)
{get_var_user_f_(buf,decalage,resultat);}


void get_var_user_f__(int *buf,int *decalage,double *resultat)
{get_var_user_f_(buf,decalage,resultat);}


void get_var_user_f_sp_ (int *buf,int *decalage,float *resultat)
{
 float *res;
   res= ((float*) (sav_buf[*buf]) )+ (*decalage -1);
   *resultat = *res;
}


void get_var_user_f_sp(int *buf,int *decalage,float *resultat)
{get_var_user_f_sp_(buf,decalage,resultat);}


void _FCALL GET_VAR_USER_F_SP(int *buf,int *decalage,float *resultat)
{get_var_user_f_sp_(buf,decalage,resultat);}


void get_var_user_f_sp__(int *buf,int *decalage,float *resultat)
{get_var_user_f_sp_(buf,decalage,resultat);}


void set_var_user_f_ (int *buf,int *decalage,double *variable)
{
 double *res;
   res= ((double*)(sav_buf[*buf])) +(*decalage -1);
   *res = *variable;
}

void set_var_user_f(int *buf,int *decalage,double *variable)
{set_var_user_f_(buf,decalage,variable);}

void _FCALL SET_VAR_USER_F(int *buf,int *decalage,double *variable)
{set_var_user_f_(buf,decalage,variable);}


void set_var_user_f__(int *buf,int *decalage,double *variable)
{set_var_user_f_(buf,decalage,variable);}


void set_var_user_f_sp_ (int *buf,int *decalage,float *variable)
{
 float *res;
   res= ((float*) (sav_buf[*buf]))+ (*decalage -1);
   *res = *variable;
}

void set_var_user_f_sp(int *buf,int *decalage,float *variable)
{set_var_user_f_sp_(buf,decalage,variable);}

void _FCALL SET_VAR_USER_F_SP(int *buf,int *decalage,float *variable)
{set_var_user_f_sp_(buf,decalage,variable);}


void set_var_user_f_sp__(int *buf,int *decalage,float *variable)
{set_var_user_f_sp_(buf,decalage,variable);}


void get_var_user_i_(int *buf,int *decalage,int *resultat)
{
 int *res;
 res= (int*) (sav_buf[*buf]+ (*decalage) -1);
 *resultat = *res;
}

void get_var_user_i(int *buf,int *decalage,int *resultat)
{get_var_user_i_(buf,decalage,resultat);}

void _FCALL GET_VAR_USER_I(int *buf,int *decalage,int *resultat)
{get_var_user_i_(buf,decalage,resultat);}

void get_var_user_i__(int *buf,int *decalage,int *resultat)
{get_var_user_i_(buf,decalage,resultat);}


void set_var_user_i_(int *buf,int *decalage,int *variable)
{
 int *res;
 res= (int*) (sav_buf[*buf]+ (*decalage) -1);
 *res = *variable;
}


void set_var_user_i(int *buf,int *decalage,int *variable)
{set_var_user_i_(buf,decalage,variable);}


void _FCALL SET_VAR_USER_I(int *buf,int *decalage,int *variable)
{set_var_user_i_(buf,decalage,variable);}


void set_var_user_i__(int *buf,int *decalage,int *variable)
{set_var_user_i_(buf,decalage,variable);}


void get_array_user_f_(int *buf,int *decalage,double *array,int *array_lenght)
{
 int i;
 double *ar;
 ar = ((double *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(array+i)= *(ar+i); }
}


void get_array_user_f(int *buf,int *decalage,double *array,int *array_lenght)
{get_array_user_f_(buf,decalage,array,array_lenght);}


void _FCALL GET_ARRAY_USER_F(int *buf,int *decalage,double *array,int *array_lenght)
{get_array_user_f_(buf,decalage,array,array_lenght);}


void get_array_user_f__(int *buf,int *decalage,double *array,int *array_lenght)
{get_array_user_f_(buf,decalage,array,array_lenght);}


void set_array_user_f_(int *buf,int *decalage,double *array,int *array_lenght)
{
 int i;
 double *ar;
 ar = ((double *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++)
  { *(ar+1) = *(array+i);}
}


void set_array_user_f(int *buf,int *decalage,double *array,int *array_lenght)
{set_array_user_f_(buf,decalage,array,array_lenght);}


void _FCALL SET_ARRAY_USER_F(int *buf,int *decalage,double *array,int *array_lenght)
{set_array_user_f_(buf,decalage,array,array_lenght);}


void set_array_user_f__(int *buf,int *decalage,double *array,int *array_lenght)
{set_array_user_f_(buf,decalage,array,array_lenght);}


void get_array_user_f_sp_(int *buf,int *decalage,float *array,int *array_lenght)
{
 int i;
 float *ar;
 ar = ((float *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(array+i)= *(ar+i); }
}


void get_array_user_f_sp(int *buf,int *decalage,float *array,int *array_lenght)
{get_array_user_f_sp_(buf,decalage,array,array_lenght);}


void _FCALL GET_ARRAY_USER_F_SP(int *buf,int *decalage,float *array,int *array_lenght)
{get_array_user_f_sp_(buf,decalage,array,array_lenght);}


void get_array_user_f_sp__(int *buf,int *decalage,float *array,int *array_lenght)
{get_array_user_f_sp_(buf,decalage,array,array_lenght);}


void set_array_user_f_sp_(int *buf,int *decalage,float *array,int *array_lenght)
{
 int i;
 float *ar;
 ar = ((float *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(ar+i) = *(array+i);}
}


void set_array_user_f_sp(int *buf,int *decalage,float *array,int *array_lenght)
{set_array_user_f_sp_(buf,decalage,array,array_lenght);}


void _FCALL SET_ARRAY_USER_F_SP(int *buf,int *decalage,float *array,int *array_lenght)
{set_array_user_f_sp_(buf,decalage,array,array_lenght);}


void set_array_user_f_sp__(int *buf,int *decalage,float *array,int *array_lenght)
{set_array_user_f_sp_(buf,decalage,array,array_lenght);}


void get_array_user_i_(int *buf,int *decalage,int *array,int *array_lenght)
{
 int i;
 int *ar;
 ar = ((int *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(array+i)= *(ar+i); }
}


void get_array_user_i(int *buf,int *decalage,int *array,int *array_lenght)
{get_array_user_i_(buf,decalage,array,array_lenght);}


void _FCALL GET_ARRAY_USER_I(int *buf,int *decalage,int *array,int *array_lenght)
{get_array_user_i_(buf,decalage,array,array_lenght);}


void get_array_user_i__(int *buf,int *decalage,int *array,int *array_lenght)
{get_array_user_i_(buf,decalage,array,array_lenght);}



void set_array_user_i_(int *buf,int *decalage,int *array,int *array_lenght)
{
 int i;
 int *ar;
 ar = ((int *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(ar+i) = *(array+i); }
}


void set_array_user_i(int *buf,int *decalage,int *array,int *array_lenght)
{set_array_user_i_(buf,decalage,array,array_lenght);}


void _FCALL SET_ARRAY_USER_I(int *buf,int *decalage,int *array,int *array_lenght)
{set_array_user_i_(buf,decalage,array,array_lenght);}


void set_array_user_i__(int *buf,int *decalage,int *array,int *array_lenght)
{set_array_user_i_(buf,decalage,array,array_lenght);}


void get_user_pointer_(int *buf,int *decalage,int **ptr)
{
 *ptr = sav_buf[*buf]+ (*decalage)-1;
}


void get_user_pointer(int *buf,int *decalage,int **ptr)
{get_user_pointer_(buf,decalage,ptr);}


void _FCALL GET_USER_POINTER(int *buf,int *decalage,int **ptr)
{get_user_pointer_(buf,decalage,ptr);}


void get_user_pointer__(int *buf,int *decalage,int **ptr)
{get_user_pointer_(buf,decalage,ptr);}



void u_vinter2_(int *jad,int *jpos,int *jlen,int *llt,my_real_c *xx, my_real_c *dydx, my_real_c *yy)
{
 my_real_c *tf;
 tf = (my_real_c *)sav_buf[6];
 
// Calling a Fortran Routine 
#ifdef _WIN64
 VINTER2(tf,jad,jpos,jlen,llt,xx,dydx,yy);
#else
 vinter2_(tf,jad,jpos,jlen,llt,xx,dydx,yy);
#endif
}

void u_vinter2(int *jad,int *jpos,int *jlen,int *llt,my_real_c *xx,my_real_c *dydx,my_real_c *yy)
{u_vinter2_(jad,jpos,jlen,llt,xx,dydx,yy);}


void _FCALL U_VINTER2(int *jad,int *jpos,int *jlen,int *llt,my_real_c *xx,my_real_c *dydx,my_real_c *yy)
{u_vinter2_(jad,jpos,jlen,llt,xx,dydx,yy);}


void u_vinter2__(int *jad,int *jpos,int *jlen,int *llt,my_real_c *xx,my_real_c *dydx,my_real_c *yy)
{u_vinter2_(jad,jpos,jlen,llt,xx,dydx,yy);}


void u_vinter2dp_(int *jad,int *jpos,int *jlen,int *llt,double *xx,double *dydx,double *yy)
{
 double *tf;
 tf= (double*)sav_buf[6];

/*Appel a une routine Fortran */
#ifdef _WIN64
 VINTER2DP(tf,jad,jpos,jlen,llt,xx,dydx,yy);
#else
 vinter2dp_(tf,jad,jpos,jlen,llt,xx,dydx,yy);
#endif
}

void u_vinter2dp(int *jad,int *jpos,int *jlen,int *llt,double *xx,double *dydx,double *yy)
{u_vinter2dp_(jad,jpos,jlen,llt,xx,dydx,yy);}


void _FCALL U_VINTER2DP(int *jad,int *jpos,int *jlen,int *llt,double *xx,double *dydx,double *yy)
{u_vinter2dp_(jad,jpos,jlen,llt,xx,dydx,yy);}


void u_vinter2dp__(int *jad,int *jpos,int *jlen,int *llt,double *xx,double *dydx,double *yy)
{u_vinter2dp_(jad,jpos,jlen,llt,xx,dydx,yy);}


void get_var_user_vfunc_(int *buf,int *llt,int *ifunc,int *jpos,int *jad,int *jlen)
{
 int i,fin;
 int *ar;
 fin = *llt;
 ar = (int *) sav_buf[*buf];

 for(i=0;i<fin;i++)
    {
     jad[i]  = 0.5* ar[ifunc[i]-1  ] +1 ;
     jlen[i] = 0.5* ar[ifunc[i]] - jad[i] - jpos[i] ;
    }
}


void _FCALL GET_VAR_USER_VFUNC(int *buf,int *llt,int *ifunc,int *jpos,int *jad,int *jlen)
{ get_var_user_vfunc_(buf,llt,ifunc,jpos,jad,jlen);}


void get_var_user_vfunc__(int *buf,int *llt,int *ifunc,int *jpos,int *jad,int *jlen)
{ get_var_user_vfunc_(buf,llt,ifunc,jpos,jad,jlen);}


void get_var_user_vfunc(int *buf,int *llt,int *ifunc,int *jpos,int *jad,int *jlen)
{  get_var_user_vfunc_(buf,llt,ifunc,jpos,jad,jlen); }



