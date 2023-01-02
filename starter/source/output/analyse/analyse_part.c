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
/* 
   Cls41l42 : Create File 
*/
/*********************************************************************
 *                        INCLUDES
 *********************************************************************/
/*Cow41l6+++*/
#include "hardware.inc"

#if CPP_mach == CPP_w95 || CPP_mach == CPP_ant || CPP_mach == CPP_wnt
/* el41m2 +1 #define _FCALL __stdcall*/
#define _FCALL
#elif 1
#define _FCALL
#endif
/*Cow41l6---*/
#include <stdio.h>
#include <stdlib.h>

#include "analyse_name.inc"

#include "analyse.h"
#include "analyse_part.h"

static int NB_PART;
static int *TAB_PART; 

void analyse_part_scan(int *tab_id)
{
  int i;
  int nbthick;
  int thick_shell_part = 0;

  for(i=0; i<NB_PART; i++)
    {
      /* Shell */
      nbthick = (TAB_PART[i] >> 0) & 0x1;

      if (nbthick == 1)
	{
	  analyse_stack_int(*(tab_id + i*50 + (4-1)));
	  analyse_call_error(AN_INFO, 461, ANINFO_BLIND_2);

	  thick_shell_part++;
	}
    }

  analyse_stack_int(thick_shell_part);
  analyse_call_check(110);
}

void apartscan(int *itab)
{
  analyse_part_scan(itab);
}

void apartscan_(int *itab)
{
  analyse_part_scan(itab);
}

void apartscan__(int *itab)
{
  analyse_part_scan(itab);
}

/*ow41l6 void APARTSCAN(int *itab)*/
void _FCALL APARTSCAN(int *itab)
{
  analyse_part_scan(itab);
}

/*********************************************************************/
void analyse_part_set(int part_id, int type )
{
  /* 
     WARNING : START OF TAB IN FORTRAN AND C IS SHIFTED BY 1 :
     part_id = 1, is the first elt in model, this means tab_part[0] 
  */
  part_id = part_id - 1;

  switch(type)
    {
    case CHECK_THICK_SHELL:
      TAB_PART[part_id] = TAB_PART[part_id] | 0x1;
      break;
    }
}

void apartset(int *id, int *type)
{
  analyse_part_set(*id, *type);
}

void apartset_(int *id, int *type)
{
  analyse_part_set(*id, *type);
}

void apartset__(int *id, int *type)
{
  analyse_part_set(*id, *type);
}

/*ow41l6 void APARTSET(int *id, int *type) */
void _FCALL APARTSET(int *id, int *type)
{
  analyse_part_set(*id, *type);
}


/*********************************************************************/
static void  analyse_part_tab_init(int nb)
{
  int i;
  
  TAB_PART = (int *) malloc(sizeof(int)*nb);
  NB_PART = nb;

  for(i=0; i<NB_PART; i++)
    {
      TAB_PART[i] = 0;
    }
}

void apartin( int *nb)
{
  analyse_part_tab_init( *nb);
}

void apartin_(int *nb)
{
  analyse_part_tab_init( *nb);
}

void apartin__( int *nb)
{
  analyse_part_tab_init( *nb);
}

/*ow41l6 void APARTIN(int *tab_part, int *nb)*/
void _FCALL APARTIN( int *nb)
{
  analyse_part_tab_init( *nb);
}
