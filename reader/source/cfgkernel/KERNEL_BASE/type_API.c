/*Copyright>    OpenRadioss
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include "utils.h"       
#include "keyword_map.h"

#include "type_API.h"

static int loc_cmp_otype(const obj_type_e *left_p,const obj_type_e *right_p);
static int loc_search_otype(obj_type_e otype,int nb_otypes,const obj_type_e *otype_tab);


int MCDS_get_first_type(obj_type_e *object_type_p) {
  if(object_type_p==NULL) return 1;
  *object_type_p=(obj_type_e)(HCDI_OBJ_TYPE_NULL +1);
  return 0;
}

int MCDS_get_last_type(obj_type_e *object_type_p) {
  if(object_type_p==NULL) return 1;
  *object_type_p=(obj_type_e)(NB_CONTAINER_MODEL-1);
  return 0;
}

int MCDS_get_next_type(obj_type_e obj_type,obj_type_e *next_obj_type_p) {
  if(next_obj_type_p==NULL) return 1;
  *next_obj_type_p=obj_type;
  if((*next_obj_type_p)<= HCDI_OBJ_TYPE_NULL) return 2;
  ++(*next_obj_type_p);
  if((*next_obj_type_p)>=NB_CONTAINER_MODEL) return 3;
  return 0;
}

int MCDS_get_previous_type(obj_type_e obj_type,obj_type_e *next_obj_type_p) {
  if(next_obj_type_p==NULL) return 1;
  *next_obj_type_p=obj_type;
  if((*next_obj_type_p)>=NB_CONTAINER_MODEL) return 3;
  --(*next_obj_type_p);
  if((*next_obj_type_p)<= HCDI_OBJ_TYPE_NULL) return 2;
  return 0;
}

/* --------- Static functions --------- */


static int loc_cmp_otype(const obj_type_e *left_p,const obj_type_e *right_p) {
  return (*left_p)-(*right_p);
}



static int loc_search_otype(obj_type_e otype,int nb_otypes,const obj_type_e *otype_tab) {
  int a_ind,a_middle_ind,a_cmp;
  if(nb_otypes<1)  return -1;
  if(nb_otypes==1) return otype_tab[0]==otype ? 0 : -1;
  a_middle_ind=nb_otypes/2;
  a_cmp=loc_cmp_otype(&otype,otype_tab+a_middle_ind);
  if(a_cmp<0)      a_ind=loc_search_otype(otype,a_middle_ind,otype_tab);
  else if(a_cmp>0) a_ind=loc_search_otype(otype,nb_otypes-a_middle_ind,otype_tab+a_middle_ind);
  else             a_ind=a_middle_ind;
  return a_ind;
}

