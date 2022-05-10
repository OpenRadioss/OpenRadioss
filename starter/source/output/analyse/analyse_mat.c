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
/* 
   Cls41l45 : Create File 
*/
/*********************************************************************
 *                        INCLUDES
 *********************************************************************/
#include <stdlib.h> /* qsort */
#include <stdio.h>

/*Cow41l6+++*/
#include "hardware.inc"

#if CPP_mach == CPP_w95 || CPP_mach == CPP_ant || CPP_mach == CPP_wnt
/* el41m2+1 #define _FCALL __stdcall */
#define _FCALL
#elif 1
#define _FCALL
#endif
/*Cow41l6---*/


#include "analyse_name.inc"

#include "analyse.h"

static int compar_mat (const void *mat_1_p, const void *mat_2_p)
{
  float *mat_1;
  float *mat_2;
  float test;

  mat_1 = (float *)mat_1_p;
  mat_2 = (float *)mat_2_p;

  if (*(mat_1+2) == 0) return -1;
  if (*(mat_2+2) == 0) return 1;

  test = (((*(mat_1+1))/(*(mat_1+2))) - ((*(mat_2+1))/(*(mat_2+2))));

  if (test < 0)
    return -1;
  else if (test == 0)
    return 0;
  else
    return 1;

}


static void analyse_mat(int *nbmat, float *tabmat)
{
  int i;
  int cnt=0;
  int nummat = *nbmat;
  int nummid = nummat/2;

  float e_rho_mid;
  float e_rho_min;
  float e_rho_max;
  float e_rho_tmp;

  qsort(tabmat,nummat, 3*sizeof(float),compar_mat);

  if (tabmat[nummid*3+2] == 0) 
    e_rho_mid = 0;
  else
    e_rho_mid = tabmat[nummid*3+1]/tabmat[nummid*3+2];

  e_rho_min = e_rho_mid/100;
  e_rho_max = e_rho_mid * 100;

/* el41m21  printf( " %d %e \n", nummid, e_rho_mid); */

  for(i=0; i<nummat; i++)
    {
      if (tabmat[i*3+2] == 0)
	e_rho_tmp = 0;
      else
	e_rho_tmp = tabmat[i*3+1]/tabmat[i*3+2];
      
/* el41m21       printf( "%d %e \n", i, e_rho_tmp); */

      if (e_rho_tmp<=e_rho_min)
	{
	  analyse_stack_int((int)(tabmat[i*3]));
	  analyse_stack_float(e_rho_tmp);
	  analyse_stack_float(e_rho_mid);
	  analyse_call_error(AN_INFO, 464, ANINFO_BLIND_2);
	  cnt++;
	}
      else if (e_rho_tmp>e_rho_max)
	{
	  analyse_stack_int((int)(tabmat[i*3]));
	  analyse_stack_float(e_rho_tmp);
	  analyse_stack_float(e_rho_mid);
	  analyse_call_error(AN_INFO, 465, ANINFO_BLIND_2);
	  cnt++;
	}
    }

  analyse_stack_int(cnt);
  analyse_call_check(5);

}

void anmat(int *nbmat, float *tabmat)
{
  analyse_mat(nbmat, tabmat);
}

void anmat_(int *nbmat, float *tabmat)
{
  analyse_mat(nbmat, tabmat);
}

void anmat__(int *nbmat, float *tabmat)
{
  analyse_mat(nbmat, tabmat);
}

/*void  ANMAT(int *nbmat, float *tabmat)*/
void _FCALL ANMAT(int *nbmat, float *tabmat)
{
  analyse_mat(nbmat, tabmat);
}
