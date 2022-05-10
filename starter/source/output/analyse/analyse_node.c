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
   Cls41l32 : Create File 
*/
/*********************************************************************
 *                        INCLUDES
 *********************************************************************/
/*Cow416+++*/
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
#include "analyse_node.h"

static int NB_NODE;
static int *TAB_NODE; 

void analyse_node_scan(int *tab_id)
{
  int i;
  int nbshell, nbvolu, nbtruss, nbbeam, nbspring, nbrbm, nbrbs, nbmass, nb2n, nbused;
  int nbstruct, nb2nds, nbrb, nbelt, nball; 
  int node_flying=0;
  int node_rbm_struct=0;
  int node_mass_elt=0;
  int node_mass_flying=0;
  int node_truss_elt=0;
  int node_beam_elt=0;
  int node_spring_elt=0;
  int node_rbs_elt=0;

  for(i=0; i<NB_NODE; i++)
    {
      /* Shell */
      nbshell = (TAB_NODE[i] >> 0) & 0x1;

      /* Volu */
      nbvolu = (TAB_NODE[i] >> 1) & 0x1;

      /* Truss */
      nbtruss = (TAB_NODE[i] >> 2) & 0x3;

      /* Beam */
      nbbeam =  (TAB_NODE[i] >> 4) & 0x3;

      /* Spring */
      nbspring = (TAB_NODE[i] >> 6) & 0x3;

      /* RigidBody_Master */
      nbrbm = (TAB_NODE[i] >> 8) & 0x1;

      /* RigidBody_Slave */
      nbrbs = (TAB_NODE[i] >> 9) & 0x1;

      /* Mass */
      nbmass = (TAB_NODE[i] >> 10) & 0x1;

      /* 2N */
      nb2n = (TAB_NODE[i] >> 11) & 0x3;

      /* Used */
      nbused = (TAB_NODE[i] >> 13) & 0x1;

      nbstruct = nbshell + nbvolu;
      nb2nds = nbtruss + nbbeam + nb2n;
      nbrb = nbrbm + nbrbs;
      nbelt = nbstruct + nb2nds + nbspring + nbrb;
      nball = nbelt + nbmass + nbused;

      if (nball == 0)
	{
	  analyse_stack_int(tab_id[i]);
	  analyse_call_error(AN_WARNING, 447, ANINFO_BLIND_2);

	  node_flying++;
	}

      if ((nbrbm == 1) && ((nbstruct + nb2nds + nbspring + nbrbs) != 0))
	{
	  analyse_stack_int(tab_id[i]);
	  analyse_call_error(AN_WARNING, 448, ANINFO_BLIND_2);

	  node_rbm_struct++;
	}

      if ((nbmass == 1) && (nbelt == 0))
	{
	  analyse_stack_int(tab_id[i]);
	  analyse_call_error(AN_INFO, 449, ANINFO_BLIND_2);

	  node_mass_elt++;
	}

      if ((nbmass == 1) && (nball == 1))
	{
	  analyse_stack_int(tab_id[i]);
	  analyse_call_error(AN_WARNING, 450, ANINFO_BLIND_2);

	  node_mass_flying++;
	}

      if ((nbtruss == 1) && (nbelt == 1))
	{
	  analyse_stack_int(tab_id[i]);
	  analyse_call_error(AN_WARNING, 451, ANINFO_BLIND_2);

	  node_truss_elt++;
	}

      if ((nbbeam == 1) && (nbelt == 1))
	{
	  analyse_stack_int(tab_id[i]);
	  analyse_call_error(AN_WARNING, 452, ANINFO_BLIND_2);

	  node_beam_elt++;
	}

      if ((nbspring == 1) && (nbelt == 1))
	{
	  analyse_stack_int(tab_id[i]);
	  analyse_call_error(AN_WARNING, 453, ANINFO_BLIND_2);

	  node_spring_elt++;
	}

      if ((nbrbs == 1) && (nbelt == 1))
	{
	  analyse_stack_int(tab_id[i]);
	  if ( nbmass == 1)
	    {
	      analyse_call_error(AN_WARNING, 455, ANINFO_BLIND_2);
	    }
	  else
	    {
	      analyse_call_error(AN_WARNING, 454, ANINFO_BLIND_2);
	    }

	  node_rbs_elt++;
	}
      
    }
  
  analyse_stack_int(node_flying);
  analyse_call_check(10);

  analyse_stack_int(node_rbm_struct);
  analyse_call_check(68);

  analyse_stack_int(node_mass_elt);
  analyse_call_check(76);

  analyse_stack_int(node_mass_flying);
  analyse_call_check(77);

  analyse_stack_int(node_truss_elt);
  analyse_call_check(22);

  analyse_stack_int(node_beam_elt);
  analyse_call_check(29);

  analyse_stack_int(node_spring_elt);
  analyse_call_check(36);

  analyse_stack_int(node_rbs_elt);
  analyse_call_check(70);

}

void anodscan(int *itab)
{
  analyse_node_scan(itab);
}

void anodscan_(int *itab)
{
  analyse_node_scan(itab);
}

void anodscan__(int *itab)
{
  analyse_node_scan(itab);
}

/* void ANODSCAN(int *itab)*/
void _FCALL ANODSCAN(int *itab)
{
  analyse_node_scan(itab);
}

/*********************************************************************/

void analyse_node_set(int nod_id, int type )
{
  /* 
     WARNING : START OF TAB IN FORTRAN AND C IS SHIFTED BY 1 :
     nod_id = 1, is the first elt in model, this means tab_node[0] 
  */
  nod_id = nod_id - 1;

  switch(type)
    {
    case CHECK_SHELL:
      TAB_NODE[nod_id] = TAB_NODE[nod_id] | 0x1;
      break;

    case CHECK_VOLU:
      TAB_NODE[nod_id] = TAB_NODE[nod_id] | 0x2;
      break;

    case CHECK_TRUSS:
      if(TAB_NODE[nod_id]&0x4)
	{
	  if(!(TAB_NODE[nod_id]&0x8))
	    {
	      TAB_NODE[nod_id] = TAB_NODE[nod_id] & ~0x4;
	      TAB_NODE[nod_id] = TAB_NODE[nod_id] |  0x8;
	    }
	}
      else
	{
	  TAB_NODE[nod_id] = TAB_NODE[nod_id] | 0x4;
	}
      break;

    case CHECK_BEAM:
      if(TAB_NODE[nod_id]&0x10)
	{
	  if(!(TAB_NODE[nod_id]&0x20))
	    {
	      TAB_NODE[nod_id] = TAB_NODE[nod_id] & ~0x10;
	      TAB_NODE[nod_id] = TAB_NODE[nod_id] |  0x20;
	    }
	}
      else
	{
	  TAB_NODE[nod_id] = TAB_NODE[nod_id] | 0x10;
	}
      break;

    case CHECK_SPRING:
      if(TAB_NODE[nod_id]&0x40)
	{
	  if(!(TAB_NODE[nod_id]&0x80))
	    {
	      TAB_NODE[nod_id] = TAB_NODE[nod_id] & ~0x40;
	      TAB_NODE[nod_id] = TAB_NODE[nod_id] |  0x80;
	    }
	}
      else
	{
	  TAB_NODE[nod_id] = TAB_NODE[nod_id] | 0x40;
	}
      break;


    case CHECK_RB_M:
      TAB_NODE[nod_id] = TAB_NODE[nod_id] | 0x100;
      break;

    case CHECK_RB_S:
      TAB_NODE[nod_id] = TAB_NODE[nod_id] | 0x200;
      break;

    case CHECK_MASS:
      TAB_NODE[nod_id] = TAB_NODE[nod_id] | 0x400;
      break;

    case CHECK_2N:
      if(TAB_NODE[nod_id]&0x800)
	{
	  if(!(TAB_NODE[nod_id]&0x1000))
	    {
	      TAB_NODE[nod_id] = TAB_NODE[nod_id] & ~0x800;
	      TAB_NODE[nod_id] = TAB_NODE[nod_id] |  0x1000;
	    }
	}
      else
	{
	  TAB_NODE[nod_id] = TAB_NODE[nod_id] | 0x800;
	}      
      break;

    case CHECK_USED:
      TAB_NODE[nod_id] = TAB_NODE[nod_id] | 0x2000;
      break;
    }
}

void anodset(int *id, int *type)
{
  analyse_node_set(*id, *type);
}

void anodset_(int *id, int *type)
{
  analyse_node_set(*id, *type);
}

void anodset__(int *id, int *type)
{
  analyse_node_set(*id, *type);
}

/*Cow41l6 void ANODSET(int *id, int *type)*/
void _FCALL ANODSET(int *id, int *type)
{
  analyse_node_set(*id, *type);
}

/*********************************************************************/

static void  analyse_node_tab_init( int nb)
{
  int i;
  
  TAB_NODE = (int *) malloc(sizeof(int)*nb);
  NB_NODE = nb;

  for(i=0; i<NB_NODE; i++)
    {
      TAB_NODE[i] = 0;
    }
}

void anodin( int *nb)
{
  analyse_node_tab_init( *nb);
}

void anodin_(int *nb)
{
  analyse_node_tab_init( *nb);
}

void anodin__( int *nb)
{
  analyse_node_tab_init( *nb);
}


void _FCALL ANODIN(int *nb)
{
  analyse_node_tab_init( *nb);
}
