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


void _FCALL SAV_BUF_POINT(buf,i)
int *buf ,  *i;{sav_buf_point_(buf,i);}
void sav_buf_point(buf,i)
int *buf ,  *i;{sav_buf_point_(buf,i);}


void sav_buf_point__(buf,i)
int *buf ,  *i;{sav_buf_point_(buf,i);}


void get_buf_point_(loca,ish,i)
int *loca ; int *ish ,  *i;{*ish = (sav_buf[*i] - loca);}

void _FCALL GET_BUF_POINT(loca,ish,i)
int *loca ; int *ish ,  *i;{*ish = (sav_buf[*i] - loca);}

void get_buf_point(loca,ish,i)
int *loca ; int *ish ,  *i;{*ish = (sav_buf[*i] - loca);}


void get_buf_point__(loca,ish,i)
int *loca ; int *ish ,  *i;{*ish = (sav_buf[*i] - loca);}


void get_var_user_f_ (buf,decalage,resultat)
int * buf, * decalage;
double * resultat;
{
 double *res;
   res= ((double*) (sav_buf[*buf]) )+ (*decalage -1);
   *resultat = *res;
}


void get_var_user_f(buf,decalage,resultat)
int * buf, * decalage;double * resultat;
{get_var_user_f_(buf,decalage,resultat);}


void _FCALL GET_VAR_USER_F(buf,decalage,resultat)
int * buf, * decalage;double * resultat;
{get_var_user_f_(buf,decalage,resultat);}


void get_var_user_f__(buf,decalage,resultat)
int * buf, * decalage;double * resultat;
{get_var_user_f_(buf,decalage,resultat);}


void get_var_user_f_sp_ (buf,decalage,resultat)
int * buf;
int * decalage;
float * resultat;
{
 float *res;
   res= ((float*) (sav_buf[*buf]) )+ (*decalage -1);
   *resultat = *res;
}


void get_var_user_f_sp(buf,decalage,resultat)
int * buf, * decalage;float * resultat;
{get_var_user_f_sp_(buf,decalage,resultat);}


void _FCALL GET_VAR_USER_F_SP(buf,decalage,resultat)
int * buf, * decalage;float * resultat;
{get_var_user_f_sp_(buf,decalage,resultat);}


void get_var_user_f_sp__(buf,decalage,resultat)
int * buf, * decalage;float * resultat;
{get_var_user_f_sp_(buf,decalage,resultat);}


void set_var_user_f_ (buf,decalage,variable)
int * buf, * decalage;
double * variable;
{
 double *res;
   res= ((double*)(sav_buf[*buf])) +(*decalage -1);
   *res = *variable;
}

void set_var_user_f(buf,decalage,variable)
int * buf, * decalage;double * variable;
{set_var_user_f_(buf,decalage,variable);}

void _FCALL SET_VAR_USER_F(buf,decalage,variable)
int * buf, * decalage;double * variable;
{set_var_user_f_(buf,decalage,variable);}


void set_var_user_f__(buf,decalage,variable)
int * buf, * decalage;double * variable;
{set_var_user_f_(buf,decalage,variable);}


void set_var_user_f_sp_ (buf,decalage,variable)
int * buf;
int * decalage;
float * variable;
{
 float *res;
   res= ((float*) (sav_buf[*buf]))+ (*decalage -1);
   *res = *variable;
}

void set_var_user_f_sp(buf,decalage,variable)
int * buf, * decalage;float * variable;
{set_var_user_f_sp_(buf,decalage,variable);}

void _FCALL SET_VAR_USER_F_SP(buf,decalage,variable)
int * buf, * decalage;float * variable;
{set_var_user_f_sp_(buf,decalage,variable);}


void set_var_user_f_sp__(buf,decalage,variable)
int * buf, * decalage;float * variable;
{set_var_user_f_sp_(buf,decalage,variable);}


void get_var_user_i_(buf,decalage,resultat)
int * buf;
int * decalage;
int * resultat;
{
 int *res;
 res= (int*) (sav_buf[*buf]+ (*decalage) -1);
 *resultat = *res;
}

void get_var_user_i(buf,decalage,resultat)
int * buf, * decalage,* resultat;
{get_var_user_i_(buf,decalage,resultat);}

void _FCALL GET_VAR_USER_I(buf,decalage,resultat)
int * buf, * decalage, * resultat;
{get_var_user_i_(buf,decalage,resultat);}
/*ow41n4+++*/
void get_var_user_i__(buf,decalage,resultat)
int * buf, * decalage,* resultat;
{get_var_user_i_(buf,decalage,resultat);}


void set_var_user_i_(buf,decalage,variable)
int * buf;
int * decalage;
int * variable;
{
 int *res;
 res= (int*) (sav_buf[*buf]+ (*decalage) -1);
 *res = *variable;
}


void set_var_user_i(buf,decalage,variable)
int * buf, * decalage,* variable;
{set_var_user_i_(buf,decalage,variable);}


void _FCALL SET_VAR_USER_I(buf,decalage,variable)
int * buf, * decalage, * variable;
{set_var_user_i_(buf,decalage,variable);}


void set_var_user_i__(buf,decalage,variable)
int * buf, * decalage,* variable;
{set_var_user_i_(buf,decalage,variable);}


void get_array_user_f_(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;
double * array;
{
 int i;
 double *ar;
 ar = ((double *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(array+i)= *(ar+i); }
}


void get_array_user_f(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;double * array;
{get_array_user_f_(buf,decalage,array,array_lenght);}


void _FCALL GET_ARRAY_USER_F(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;double * array;
{get_array_user_f_(buf,decalage,array,array_lenght);}


void get_array_user_f__(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;double * array;
{get_array_user_f_(buf,decalage,array,array_lenght);}


void set_array_user_f_(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;
double * array;
{
 int i;
 double *ar;
 ar = ((double *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++)
  { *(ar+1) = *(array+i);}
}


void set_array_user_f(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;double * array;
{set_array_user_f_(buf,decalage,array,array_lenght);}


void _FCALL SET_ARRAY_USER_F(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;double * array;
{set_array_user_f_(buf,decalage,array,array_lenght);}


void set_array_user_f__(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;double * array;
{set_array_user_f_(buf,decalage,array,array_lenght);}


void get_array_user_f_sp_(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;
float * array;
{
 int i;
 float *ar;
 ar = ((float *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(array+i)= *(ar+i); }
}


void get_array_user_f_sp(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;float * array;
{get_array_user_f_sp_(buf,decalage,array,array_lenght);}


void _FCALL GET_ARRAY_USER_F_SP(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;float * array;
{get_array_user_f_sp_(buf,decalage,array,array_lenght);}


void get_array_user_f_sp__(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;float * array;
{get_array_user_f_sp_(buf,decalage,array,array_lenght);}


void set_array_user_f_sp_(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;
float * array;
{
 int i;
 float *ar;
 ar = ((float *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(ar+i) = *(array+i);}
}


void set_array_user_f_sp(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;float * array;
{set_array_user_f_sp_(buf,decalage,array,array_lenght);}


void _FCALL SET_ARRAY_USER_F_SP(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;float * array;
{set_array_user_f_sp_(buf,decalage,array,array_lenght);}


void set_array_user_f_sp__(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;float * array;
{set_array_user_f_sp_(buf,decalage,array,array_lenght);}


void get_array_user_i_(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;
int * array;
{
 int i;
 int *ar;
 ar = ((int *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(array+i)= *(ar+i); }
}


void get_array_user_i(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;int * array;
{get_array_user_i_(buf,decalage,array,array_lenght);}


void _FCALL GET_ARRAY_USER_I(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;int * array;
{get_array_user_i_(buf,decalage,array,array_lenght);}


void get_array_user_i__(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;int * array;
{get_array_user_i_(buf,decalage,array,array_lenght);}



void set_array_user_i_(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;
int * array;
{
 int i;
 int *ar;
 ar = ((int *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(ar+i) = *(array+i); }
}


void set_array_user_i(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;int * array;
{set_array_user_i_(buf,decalage,array,array_lenght);}


void _FCALL SET_ARRAY_USER_I(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;int * array;
{set_array_user_i_(buf,decalage,array,array_lenght);}


void set_array_user_i__(buf,decalage,array,array_lenght)
int *buf, *decalage, *array_lenght;int * array;
{set_array_user_i_(buf,decalage,array,array_lenght);}


void get_user_pointer_(buf,decalage,ptr)
int *buf;
int *decalage;
int **ptr;
{
 *ptr = sav_buf[*buf]+ (*decalage)-1;
}


void get_user_pointer(buf,decalage,ptr)
int *buf,*decalage,**ptr;
{get_user_pointer_(buf,decalage,ptr);}


void _FCALL GET_USER_POINTER(buf,decalage,ptr)
int *buf,*decalage,**ptr;
{get_user_pointer_(buf,decalage,ptr);}


void get_user_pointer__(buf,decalage,ptr)
int *buf,*decalage,**ptr;
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

void u_vinter2(jad,jpos,jlen,llt,xx,dydx,yy)
int *jad, *jpos, *jlen,*llt;
double *xx,*dydx,*yy;
{u_vinter2_(jad,jpos,jlen,llt,xx,dydx,yy);}


void _FCALL U_VINTER2(jad,jpos,jlen,llt,xx,dydx,yy)
int *jad, *jpos, *jlen,*llt;
double *xx,*dydx,*yy;
{u_vinter2_(jad,jpos,jlen,llt,xx,dydx,yy);}


void u_vinter2__(jad,jpos,jlen,llt,xx,dydx,yy)
int *jad, *jpos, *jlen,*llt;
double *xx,*dydx,*yy;
{u_vinter2_(jad,jpos,jlen,llt,xx,dydx,yy);}


void u_vinter2dp_(jad,jpos,jlen,llt,xx,dydx,yy)
int *jad, *jpos, *jlen,*llt;
double *xx,*dydx,*yy;
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

void u_vinter2dp(jad,jpos,jlen,llt,xx,dydx,yy)
int *jad, *jpos, *jlen,*llt;
double *xx,*dydx,*yy;
{u_vinter2dp_(jad,jpos,jlen,llt,xx,dydx,yy);}


void _FCALL U_VINTER2DP(jad,jpos,jlen,llt,xx,dydx,yy)
int *jad, *jpos, *jlen,*llt;
double *xx,*dydx,*yy;
{u_vinter2dp_(jad,jpos,jlen,llt,xx,dydx,yy);}


void u_vinter2dp__(jad,jpos,jlen,llt,xx,dydx,yy)
int *jad, *jpos, *jlen,*llt;
double *xx,*dydx,*yy;
{u_vinter2dp_(jad,jpos,jlen,llt,xx,dydx,yy);}


void get_var_user_vfunc_(buf,llt,ifunc,jpos,jad,jlen)
int *buf,*llt,*ifunc,*jad,*jpos,*jlen;
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


void _FCALL GET_VAR_USER_VFUNC(buf,llt,ifunc,jpos,jad,jlen)
int *buf,*llt,*ifunc,*jad,*jpos,*jlen;
{
 get_var_user_vfunc_(buf,llt,ifunc,jpos,jad,jlen);
}


void get_var_user_vfunc__(buf,llt,ifunc,jpos,jad,jlen)
int *buf,*llt,*ifunc,*jad,*jpos,*jlen;
{
 get_var_user_vfunc_(buf,llt,ifunc,jpos,jad,jlen);
}


void get_var_user_vfunc(buf,llt,ifunc,jpos,jad,jlen)
int *buf,*llt,*ifunc,*jad,*jpos,*jlen;
{
 get_var_user_vfunc_(buf,llt,ifunc,jpos,jad,jlen);
}



