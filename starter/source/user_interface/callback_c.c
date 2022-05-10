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

#define _FCALL
/*=================================================================*/
/*        CALLBACK INTERFACE                                       */
/*=================================================================*/

int *sav_buf[1000];



#ifdef _WIN64
#define sav_buf_point_ SAV_BUF_POINT
#endif

void _FCALL sav_buf_point_(int *buf ,int *i)
{
  if(*i<0||*i>=1000){fprintf(stderr,"ERROR in SAV_BUF: table overflow\n");exit(1);}
  sav_buf[*i] = buf;
}


#ifdef _WIN64
#define get_buf_point_ GET_BUF_POINT
#endif

void _FCALL get_buf_point_(int *loca,int *ish,int *i)
{
 *ish = (sav_buf[*i] - loca);
}


#ifdef _WIN64
#define get_var_user_f_ GET_VAR_USER_F
#endif
void _FCALL get_var_user_f_ (int *buf, int *decalage, double *resultat)
{
 double *res;
 res= ((double*) (sav_buf[*buf]) )+ (*decalage -1);
 *resultat = *res;
}


#ifdef _WIN64
#define get_var_user_f_sp_ GET_VAR_USER_F_SP
#endif
void  _FCALL get_var_user_f_sp_ (int * buf,int * decalage,float * resultat)
{
 float *res;
 res= ((float*) (sav_buf[*buf]) )+ (*decalage -1);
 *resultat = *res;
}


#ifdef _WIN64
#define set_var_user_f_ SET_VAR_USER_F
#endif
void _FCALL set_var_user_f_ (int *buf, int* decalage,double *variable)
{
 double *res;
 res= ((double*)(sav_buf[*buf])) +(*decalage -1);
 *res = *variable;
}


#ifdef _WIN64
#define set_var_user_f_sp_ SET_VAR_USER_F_SP
#endif
void _FCALL set_var_user_f_sp_ (int * buf,int *decalage,float * variable)
{
 float *res;
   res= ((float*) (sav_buf[*buf]))+ (*decalage -1);
   *res = *variable;
}


#ifdef _WIN64
#define get_var_user_i_ GET_VAR_USER_I
#endif
void _FCALL get_var_user_i_(int * buf,int * decalage,int * resultat)
{
 int *res;
 res= (int*) (sav_buf[*buf]+ (*decalage) -1);
 *resultat = *res;
}


#ifdef _WIN64
#define set_var_user_i_ SET_VAR_USER_I
#endif
void _FCALL set_var_user_i_ (int *buf,int *decalage,int *variable)
{
 int *res;
 res= (int*) (sav_buf[*buf]+ (*decalage) -1);
 *res = *variable;
}


#ifdef _WIN64
#define get_array_user_f_ GET_ARRAY_USER_F
#endif
void _FCALL get_array_user_f_(int *buf, int *decalage, double *array, int *array_lenght)
{
 int i;
 double *ar;
 ar = ((double *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(array+i)= *(ar+i); }
}


#ifdef _WIN64
#define set_array_user_f_ SET_ARRAY_USER_F
#endif
void _FCALL set_array_user_f_(int *buf, int *decalage, double * array, int *array_lenght)
{
 int i;
 double *ar;
 ar = ((double *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++)
  { *(ar+1) = *(array+i);}
}


#ifdef _WIN64
#define get_array_user_f_sp_ GET_ARRAY_USER_F_SP
#endif
void _FCALL get_array_user_f_sp_(int *buf, int *decalage, float *array, int *array_lenght)
{
 int i;
 float *ar;
 ar = ((float *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(array+i)= *(ar+i); }
}


#ifdef _WIN64
#define set_array_user_f_sp_ SET_ARRAY_USER_F_SP
#endif
void _FCALL set_array_user_f_sp_(int *buf, int *decalage, float *array, int *array_lenght)
{
 int i;
 float *ar;
 ar = ((float *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(ar+i) = *(array+i);}
}


#ifdef _WIN64
#define get_array_user_i_ GET_ARRAY_USER_I
#endif
void get_array_user_i_(int *buf, int *decalage, int *array, int *array_lenght)
{
 int i;
 int *ar;
 ar = ((int *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(array+i)= *(ar+i); }
}


#ifdef _WIN64
#define set_array_user_i_ SET_ARRAY_USER_I
#endif
void set_array_user_i_(int *buf, int *decalage, int *array, int *array_lenght)
{
 int i;
 int *ar;
 ar = ((int *) (sav_buf[*buf]))+ (*decalage) -1;
 for (i=0;i<(*array_lenght);i++) 
  { *(ar+i) = *(array+i); }
}


