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
#include <stdarg.h>
#include "fileformat_API.h"
#include "descriptor_API.h"
#include "keyword_map.h"
#include "utils.h"


static counter_type_e get_ctype_from_vtype(value_type_e vtype);
static void           resize_descriptor(descriptor_t *descr_p,int ikeyword);
static void           set_attribute_descriptor(attribute_descriptor_t *attdescr_p,
					       attribute_type_e atype,value_type_e vtype,
					       const char *skeyword,const char *comment,
					       int index);
static void           set_object_descriptor(object_descriptor_t *objdescr_p,
					    obj_type_e atype,const char *skeyword,const char *comment,
					    int index);
static void           add_size_descriptor_ikeyword(size_descriptor_t *sizedescr_p,int ikeyword);


int MCDS_new_descriptor(descriptor_t **descr_pfp) {
  if(descr_pfp==NULL) return 1;
  
  *descr_pfp=(descriptor_t *)my_malloc(1,sizeof(descriptor_t));

  (*descr_pfp)->keyword_map=new_kwmap();
  (*descr_pfp)->user_id=-1;

  return 0;
}


int MCDS_is_descriptor_user(const descriptor_t *descr_p,int *bool_p) {
  if(descr_p==NULL) return 1;
  if(bool_p==NULL)  return 2;
  *bool_p=(descr_p->user_id>=0);  
  return 0;
}


int MCDS_get_descriptor_user_id(const descriptor_t *descr_p,int *id_p) {
  if(descr_p==NULL) return 1;
  if(id_p==NULL)  return 2;
  *id_p=descr_p->user_id;
  return 0;
}

int MCDS_set_descriptor_attributes(const descriptor_t *descr_p,int ikeyword,...)
{
  va_list arglist;
  int     attrib=END_ARGS;
  attribute_descriptor_t *att_descr_p;

  if(descr_p==NULL) return 1;
  if(ikeyword<=0 || ikeyword>descr_p->max_ikeyword) return 2;
  att_descr_p=descr_p->attdescr_array[ikeyword];
  if(att_descr_p==NULL) return 2;

  va_start(arglist,ikeyword);
  while((attrib=va_arg(arglist,int))!=END_ARGS) switch(attrib) {
  case DESCR_COMMENT:
    att_descr_p->comment = my_strcpy(att_descr_p->comment, va_arg(arglist,char *));
    break;
  case DESCR_SOLVER_NAME:
      if ((att_descr_p->solver_name != NULL) && (att_descr_p->solver_name[0] == '\0')) {
          att_descr_p->solver_name = my_strcpy(att_descr_p->solver_name, va_arg(arglist, char*));
      }
    break;
  case DESCR_LENGTH:
      att_descr_p->length = va_arg(arglist,int);
      break;
  default:
      break;
  }
  return 0;
}

int MCDS_set_descriptor_user_id(descriptor_t *descr_p,int id) {
  if(descr_p==NULL) return 1;
  descr_p->user_id=id;
  return 0;
}


int MCDS_add_descriptor_value(descriptor_t *descr_p,
			      value_type_e vtype,int ikeyword,const char *skeyword,const char *comment) 
{
  int index;

  /* Error management */
  if(descr_p==NULL) return 1;
  if(vtype==VTYPE_OBJECT) return 2;
  
  /* Memory management */
  resize_descriptor(descr_p,ikeyword);
  descr_p->attdescr_array[ikeyword]=(attribute_descriptor_t *)my_malloc(1,sizeof(attribute_descriptor_t));

  /* Attribute descriptor (config) */
  index=(descr_p->counter[get_ctype_from_vtype(vtype)])++;
  set_attribute_descriptor(descr_p->attdescr_array[ikeyword],ATYPE_VALUE,vtype,skeyword,comment,index);

  /* Keyword map (update) */
  add_kwmap_keyword(descr_p->keyword_map,skeyword,ikeyword);

  return 0;
}


int MCDS_add_descriptor_object(descriptor_t *descr_p,
			       obj_type_e otype,int ikeyword,const char *skeyword,const char *comment) 
{
  int index;

  /* Error management */
  if(descr_p==NULL) return 1;
  
  /* Memory management */
  resize_descriptor(descr_p,ikeyword);
  descr_p->attdescr_array[ikeyword]=(attribute_descriptor_t *)my_malloc(1,sizeof(object_descriptor_t));

  /* Attribute descriptor (config) */
  index=(descr_p->counter[CTYPE_OBJECT])++;
  set_object_descriptor((object_descriptor_t *)(descr_p->attdescr_array[ikeyword]),
			otype,skeyword,comment,index);

  /* Keyword map (update) */
  add_kwmap_keyword(descr_p->keyword_map,skeyword,ikeyword);

  return 0;
}


int MCDS_add_descriptor_size(descriptor_t *descr_p,
			     int ikeyword,const char *skeyword,const char *comment) 
{
  int index;

  /* Error management */
  if(descr_p==NULL) return 1;
  
  /* Memory management */
  resize_descriptor(descr_p,ikeyword);
  descr_p->attdescr_array[ikeyword]=(attribute_descriptor_t *)my_malloc(1,sizeof(size_descriptor_t));

  /* Attribute descriptor (config) */
  index=(descr_p->counter[CTYPE_INT])++;
  set_attribute_descriptor(descr_p->attdescr_array[ikeyword],ATYPE_SIZE,VTYPE_INT,skeyword,comment,index);

  /* Keyword map (update) */
  add_kwmap_keyword(descr_p->keyword_map,skeyword,ikeyword);

  return 0;
}


int MCDS_add_descriptor_value_array(descriptor_t *descr_p,
				    value_type_e vtype,int ikeyword,const char *skeyword,const char *comment,
				    attribute_type_e array_type,
                                    int dimension , dimension_size_t *size_array)
{
  int                 index;
  array_descriptor_t *arrdescr_p;

  /* Error management */
  if(descr_p==NULL) return 1;
  if(array_type!=ATYPE_STATIC_ARRAY && array_type!=ATYPE_DYNAMIC_ARRAY) return 2;
  if(vtype==VTYPE_OBJECT) return 3;
  
  /* Memory management */
  resize_descriptor(descr_p,ikeyword);
  descr_p->attdescr_array[ikeyword]=(attribute_descriptor_t *)my_malloc(1,sizeof(array_descriptor_t));

  /* Attribute descriptor (config) */
  index=(descr_p->counter[CTYPE_ARRAY])++;
  set_attribute_descriptor(descr_p->attdescr_array[ikeyword],array_type,vtype,skeyword,comment,index);
  arrdescr_p=(array_descriptor_t *)(descr_p->attdescr_array[ikeyword]);

  arrdescr_p->size=size_array[0].size;/*lei:to_be_deleted*/

  
  arrdescr_p->dimension = dimension;
  arrdescr_p->size_array = size_array;
  /*if(array_type==ATYPE_DYNAMIC_ARRAY) */
  {
    int i;
    /*stock this array's ikeyword in all size descriptors on which this array depends*/
    for(i=0;i<dimension;i++){
       if(!size_array[i].isRealSize)
          add_size_descriptor_ikeyword((size_descriptor_t *)(descr_p->attdescr_array[size_array[i].size]),ikeyword);
    }
  }
  

  /* Keyword map (update) */
  add_kwmap_keyword(descr_p->keyword_map,skeyword,ikeyword);

  return 0;
}


int MCDS_add_descriptor_object_array(descriptor_t *descr_p,
				     obj_type_e otype,int ikeyword,const char *skeyword,const char *comment,
				     attribute_type_e array_type,
                                     int dimension , dimension_size_t *size_array)
{
  int index;
  object_array_descriptor_t *arrdescr_p;


  /* Error management */
  if(descr_p==NULL) return 1;
  if(array_type!=ATYPE_DYNAMIC_ARRAY && array_type!=ATYPE_STATIC_ARRAY) return 2;
  
  /* Memory management */
  resize_descriptor(descr_p,ikeyword);
  descr_p->attdescr_array[ikeyword]=(attribute_descriptor_t *)my_malloc(1,sizeof(object_array_descriptor_t));

  /* Attribute descriptor (config) */
  index=(descr_p->counter[CTYPE_ARRAY])++;
  set_object_descriptor((object_descriptor_t *)(descr_p->attdescr_array[ikeyword]),
			otype,skeyword,comment,index);
  arrdescr_p=(object_array_descriptor_t *)(descr_p->attdescr_array[ikeyword]);

  arrdescr_p->size=size_array[0].size;/*to_be_deleted*/

  descr_p->attdescr_array[ikeyword]->atype=array_type;

  
  arrdescr_p->dimension = dimension;
  arrdescr_p->size_array = size_array;
  /*if(array_type==ATYPE_DYNAMIC_ARRAY) */
  {
    int i;
    /*stock this array ikeyword in all size descriptors on which this array depends*/
    for(i=0;i<dimension;i++){
       if(!size_array[i].isRealSize)
          add_size_descriptor_ikeyword((size_descriptor_t *)(descr_p->attdescr_array[size_array[i].size]),ikeyword);
    }
  }
  

  /* Keyword map (update) */
  add_kwmap_keyword(descr_p->keyword_map,skeyword,ikeyword);

  return 0;
}


int MCDS_get_descriptor_ikeyword(const descriptor_t *descr_p,const char *skeyword,int *ikeyword_p) {
  /* Error management */
  if(ikeyword_p==NULL) return 3;
  *ikeyword_p=0;
  if(descr_p==NULL)    return 1;
  if(skeyword==NULL)   return 2;
  if(descr_p->keyword_map==NULL) return 4; 
  
  /* Research */
  *ikeyword_p=find_kwmap_ikeyword(descr_p->keyword_map,skeyword);
  return 0;
}


int MCDS_get_descriptor_skeyword(const descriptor_t *descr_p,int ikeyword,char **skeyword_p) {
  /* Error management */
  if(skeyword_p==NULL) return 3;
  *skeyword_p=NULL;
  if(descr_p==NULL) return 1;
  if(ikeyword<=0 || ikeyword>descr_p->max_ikeyword) return 2;

  if(descr_p->attdescr_array[ikeyword]==NULL) return 4;
  *skeyword_p=descr_p->attdescr_array[ikeyword]->skeyword;

  return 0;
}


int MCDS_get_descriptor_attributes(const descriptor_t *descr_p,int ikeyword,...) {
  va_list                       arglist;
  int                           attrib;
  const attribute_descriptor_t *att_descr_p;

  if(descr_p==NULL) return 1;
  if(ikeyword<=0 || ikeyword>descr_p->max_ikeyword) return 2;
  att_descr_p=descr_p->attdescr_array[ikeyword];
  if(att_descr_p==NULL) return 2;

  va_start(arglist,ikeyword);
  while((attrib=va_arg(arglist,int))!=END_ARGS) switch (attrib) {
  case DESCR_COUNTER_TYPE:
    {
      counter_type_e   *ctype_p=va_arg(arglist,counter_type_e *);
      attribute_type_e  atype=att_descr_p->atype;
      if(atype==ATYPE_STATIC_ARRAY || atype==ATYPE_DYNAMIC_ARRAY) {
	*ctype_p=CTYPE_ARRAY;
      } else switch(att_descr_p->vtype) {
      case VTYPE_BOOL:   *ctype_p=CTYPE_BOOL;     break; 
      case VTYPE_INT:    *ctype_p=CTYPE_INT;     break;
      case VTYPE_UINT:   *ctype_p=CTYPE_UINT;     break;
      case VTYPE_FLOAT:  *ctype_p=CTYPE_FLOAT;   break;
      case VTYPE_STRING: *ctype_p=CTYPE_STRING;  break;
      case VTYPE_OBJECT: *ctype_p=CTYPE_OBJECT;  break;
      default:           *ctype_p=CTYPE_UNKNOWN; break;
      }
    }
    break;
  case DESCR_COUNTER_INDEX:
    {
      int *index_p=va_arg(arglist,int *);
      *index_p=att_descr_p->index;
    }
    break;
  case DESCR_ATTRIB_TYPE:
    {
      attribute_type_e *atype_p=va_arg(arglist,attribute_type_e *);
      *atype_p=att_descr_p->atype;
    }
    break;
  case DESCR_VALUE_TYPE:
    {
      value_type_e *vtype_p=va_arg(arglist,value_type_e *);
      *vtype_p=att_descr_p->vtype;
    }
    break;
  case DESCR_SKEYWORD:
    {
      char **str_p=va_arg(arglist,char **);
      *str_p=att_descr_p->skeyword;
    }
    break;
  case DESCR_COMMENT:
    {
      char **str_p=va_arg(arglist,char **);
      *str_p=att_descr_p->comment;
    }
    break;
  case DESCR_SOLVER_NAME:
    {
      char **str_p=va_arg(arglist,char **);
      *str_p=att_descr_p->solver_name;
    }
    break;
  case DESCR_LENGTH:
    {
      int *val=va_arg(arglist,int *);
      *val=att_descr_p->length;
    }
    break;
  case DESCR_OBJECT_TYPE:
    {
      const object_descriptor_t *obj_descr_p=(const object_descriptor_t *)att_descr_p;
      obj_type_e *type_p=va_arg(arglist,obj_type_e *);
      if(att_descr_p->vtype!=VTYPE_OBJECT) {
	*type_p= HCDI_OBJ_TYPE_NULL;
	va_end(arglist);
	return 3;
      }
      *type_p=obj_descr_p->otype;
    }
    break;
  case DESCR_SIZE:
    {
      int *size_p=va_arg(arglist,int *);
      if(att_descr_p->atype!=ATYPE_STATIC_ARRAY) {
	*size_p=0;
	va_end(arglist);
	return 3;
      }
      if(att_descr_p->vtype==VTYPE_OBJECT) {
	const object_array_descriptor_t *array_descr_p=(const object_array_descriptor_t *)att_descr_p;
	*size_p=array_descr_p->size_array[0].size;
        if(array_descr_p->size != array_descr_p->size_array[0].size)
          printf("DESCR_SIZE VTYPE_OBJECT is different");
      } else {
	const array_descriptor_t *array_descr_p=(const array_descriptor_t *)att_descr_p;
        *size_p=array_descr_p->size_array[0].size;
        if(array_descr_p->size != array_descr_p->size_array[0].size)
          printf("DESCR_SIZE VTYPE_VALUE is different");
      }
    }
    break;
  case DESCR_SIZE_IKEYWORD: 
    {
      int *size_p=va_arg(arglist,int *);
      if(att_descr_p->atype!=ATYPE_DYNAMIC_ARRAY) {
	*size_p=END_ARGS;
	return 3;
      }
      if(att_descr_p->vtype==VTYPE_OBJECT) {
	const object_array_descriptor_t *array_descr_p=(const object_array_descriptor_t *)att_descr_p;
	*size_p=array_descr_p->size_array[0].size;
        if(array_descr_p->size != array_descr_p->size_array[0].size)
          printf("Lei: DESCR_SIZE_IKEYWORD VTYPE_VALUE is different");
      } else {
	const array_descriptor_t *array_descr_p=(const array_descriptor_t *)att_descr_p;
	*size_p=array_descr_p->size_array[0].size;
        if(array_descr_p->size != array_descr_p->size_array[0].size)
          printf("Lei: DESCR_SIZE_IKEYWORD VTYPE_VALUE is different");
      }
    }
    break;
  case DESCR_SIZE_SKEYWORD: 
    {
      char **kw_p=va_arg(arglist,char **);
      if(att_descr_p->atype!=ATYPE_DYNAMIC_ARRAY) {
	*kw_p=NULL;
	va_end(arglist);
	return 3;
      }
      if(att_descr_p->vtype==VTYPE_OBJECT) {
	const object_array_descriptor_t *array_descr_p=(const object_array_descriptor_t *)att_descr_p;
	*kw_p=descr_p->attdescr_array[array_descr_p->size_array[0].size]->skeyword; 
      } else {
	const array_descriptor_t *array_descr_p=(const array_descriptor_t *)att_descr_p;
	*kw_p=descr_p->attdescr_array[array_descr_p->size_array[0].size]->skeyword; 
      }
    }
    break;
  case DESCR_SIZE_NB_CONN_IKEYWORDS:
    if(att_descr_p->atype==ATYPE_SIZE) {
      const size_descriptor_t *size_descr_p=(const size_descriptor_t *)att_descr_p;
      int *nb_conn_ikeywords_p=va_arg(arglist,int *);
      *nb_conn_ikeywords_p=size_descr_p->nb_ikeywords;
    } else {
      va_end(arglist);
      return 3;
    }
    break; 
  case DESCR_DIMENSION:
    {
      int *dimension_p=va_arg(arglist,int *);
      if(att_descr_p->atype!=ATYPE_STATIC_ARRAY && att_descr_p->atype!=ATYPE_DYNAMIC_ARRAY) {
        *dimension_p=0;
        va_end(arglist);
        return 3;
      }
      if(att_descr_p->vtype==VTYPE_OBJECT) {
        const object_array_descriptor_t *array_descr_p=(const object_array_descriptor_t *)att_descr_p;
        *dimension_p=array_descr_p->dimension;
      } else {
        const array_descriptor_t *array_descr_p=(const array_descriptor_t *)att_descr_p;
        *dimension_p=array_descr_p->dimension;
      }
    } 
    break;
  case DESCR_MULTI_SIZE:
    {
      dimension_size_t **size_array_p=va_arg(arglist,dimension_size_t**);
      if(att_descr_p->atype!=ATYPE_STATIC_ARRAY && att_descr_p->atype!=ATYPE_DYNAMIC_ARRAY) {
        *size_array_p=NULL;
        va_end(arglist);
        return 3;
      }
      if(att_descr_p->vtype==VTYPE_OBJECT) {
        const object_array_descriptor_t *array_descr_p=(const object_array_descriptor_t *)att_descr_p;
        *size_array_p=array_descr_p->size_array;
      } else {
        const array_descriptor_t *array_descr_p=(const array_descriptor_t *)att_descr_p;
        *size_array_p=array_descr_p->size_array;
      }
    } 
    break;
  default: 
    va_end(arglist);
    return 2; 
    /* break; */
  }

  va_end(arglist);
  return 0;
}


int MCDS_get_descriptor_tab(const descriptor_t *descr_p,int ikeyword,int attrib,int ind,void *val_p) {
  const size_descriptor_t *size_descr_p;

  if(descr_p==NULL) return 1;
  size_descr_p=(const size_descriptor_t *)(descr_p->attdescr_array[ikeyword]);
  if(ind<0 || ind>=size_descr_p->nb_ikeywords) return 4;
  if(val_p==NULL) return 3;

  switch(attrib) {
  case DESCR_SIZE_CONN_IKEYWORD: 
    *((int *)val_p)=size_descr_p->ikeyword_array[ind];
    break;
  case DESCR_MULTI_SIZE: 
    break;
  default: 
    return 2; 
    /* break */
  }

  return 0;
}


int MCDS_get_descriptor_first_ikeyword(const descriptor_t *descr_p,int *first_ikeyword_p) {
  if(first_ikeyword_p==NULL) return 2;
  *first_ikeyword_p=END_ARGS;
  if(descr_p==NULL) return 1;
  *first_ikeyword_p=1;
  while(*first_ikeyword_p<=descr_p->max_ikeyword && descr_p->attdescr_array[*first_ikeyword_p]==NULL) {
    ++(*first_ikeyword_p);
  }
  if(*first_ikeyword_p>descr_p->max_ikeyword) *first_ikeyword_p=END_ARGS;
  return 0;
}

int MCDS_get_descriptor_next_ikeyword(const descriptor_t *descr_p,int ikeyword,int *next_ikeyword_p) {
  if(next_ikeyword_p==NULL) return 2;
  *next_ikeyword_p=END_ARGS;
  if(descr_p==NULL) return 1;
  *next_ikeyword_p=ikeyword;
  ++(*next_ikeyword_p);
  while(*next_ikeyword_p<=descr_p->max_ikeyword && descr_p->attdescr_array[*next_ikeyword_p]==NULL) {
    ++(*next_ikeyword_p);
  }
  if(*next_ikeyword_p>descr_p->max_ikeyword) *next_ikeyword_p=END_ARGS;
  return 0;
}

int MCDS_get_descriptor_max_ikeyword(const descriptor_t *descr_p,int *max_ikeyword_p) {
  if(max_ikeyword_p==NULL) return 2;
  *max_ikeyword_p=0;
  if(descr_p==NULL) return 1;
  *max_ikeyword_p=descr_p->max_ikeyword;
  return 0;
}


int MCDS_get_descriptor_count(const descriptor_t *descr_p,counter_type_e ctype,int *nb_items_p) {
  if(descr_p==NULL)    return 1;
  if(nb_items_p==NULL) return 2;

  *nb_items_p=descr_p->counter[ctype];

  return 0;
}


int MCDS_check_descriptor_ikeyword(const descriptor_t *descr_p,int ikeyword,int *isvalid_p) {
  if(isvalid_p==NULL) return 2;
  *isvalid_p=0;
  if(descr_p==NULL) return 1;
  *isvalid_p=(ikeyword>0 && ikeyword<=descr_p->max_ikeyword && descr_p->attdescr_array[ikeyword]!=NULL);
  return 0;
}


int MCDS_add_descriptor_fileformat(descriptor_t *descr_p,int ff_id,fileformat_t *ff_p) {
  int nb_ff;
  
  if(descr_p==NULL) return 1;

  nb_ff=descr_p->nb_ff++;
  descr_p->ff_array=(ff_pair_t *)my_realloc(descr_p->ff_array,nb_ff,descr_p->nb_ff,sizeof(ff_pair_t));
  
  descr_p->ff_array[nb_ff].ff_id = ff_id;
  descr_p->ff_array[nb_ff].ff_p  = ff_p;

  return 0;
}


int MCDS_get_descriptor_fileformat(const descriptor_t *descr_p,int ff_id,const fileformat_t **ff_pp) {
  int ind=0;

  if(ff_pp==NULL)   return 2;
  *ff_pp=NULL;
  if(descr_p==NULL) return 1;  

  while(ind<descr_p->nb_ff && ff_id!=descr_p->ff_array[ind].ff_id) ++ind;
  if(ind<descr_p->nb_ff) *ff_pp=descr_p->ff_array[ind].ff_p;

  return (*ff_pp==NULL) ? 3 : 0;
}

int MCDS_get_descriptor_lower_fileformat(const descriptor_t *descr_p, int ff_id, const fileformat_t **ff_pp) {
    /*Function to get the lower format version. If the formats in the config file are not written in ascending order then the for loop is useful */
  int i =0,size=0;
  int low_ff_id = ff_id-1;
  int next_min_format =0;
  int next_min_index =-1;

  if(ff_pp==NULL)   return 2;
  *ff_pp=NULL;
  if(descr_p==NULL) return 1;  
  size = descr_p->nb_ff;

  for(i=size-1;i>=0;i--)
  {
      if(descr_p->ff_array[i].ff_id == low_ff_id)
      {
          next_min_index = i;
          break;
      }
      if( descr_p->ff_array[i].ff_id <ff_id  && next_min_format <descr_p->ff_array[i].ff_id)
      {
          next_min_format = descr_p->ff_array[i].ff_id;
          next_min_index = i;
      }
  }

  if(next_min_index>=0) 
  {
      *ff_pp = descr_p->ff_array[next_min_index].ff_p;
  }

  return (*ff_pp==NULL) ? 3 : 0;
}

int MCDS_delete_descriptor(descriptor_t *descr_p) {
  int i=0, j=0;

  /* Error management */
  if(descr_p==NULL) return 1;
  
  /* Delete keyword map */
  clear_kwmap(descr_p->keyword_map);
  my_free(descr_p->keyword_map);

  /* Attribute descriptors */
  if(descr_p->attdescr_array!=NULL) {

      for(i=0;i<=descr_p->max_ikeyword;i++)
      {
          object_descriptor_t *objdescr_p = NULL;
          object_array_descriptor_t *obj_array_descr_p = NULL;
          array_descriptor_t *array_descr_p = NULL;
          value_type_e value_type = VTYPE_UNKNOWN;
          attribute_descriptor_t *attdescr_p = descr_p->attdescr_array[i];
          if(attdescr_p == NULL)
              continue;

          if(attdescr_p->atype == ATYPE_DYNAMIC_ARRAY || attdescr_p->atype == ATYPE_STATIC_ARRAY)
          {
              array_descr_p = (array_descriptor_t *)attdescr_p;
              if (array_descr_p == NULL)
                  continue;
              my_free(array_descr_p->size_array);
              array_descr_p->size_array = NULL;
          }

          value_type = attdescr_p->vtype;
          if(value_type != VTYPE_OBJECT)
              continue;

          objdescr_p = (object_descriptor_t *)(attdescr_p);
          if(objdescr_p == NULL)
              continue;
          my_free(objdescr_p->allowed_types);
          objdescr_p->allowed_types = NULL;
          for(j=0; j<objdescr_p->num; j++)
          {
              my_free(objdescr_p->comments[j]);
              objdescr_p->comments[j] = NULL;

              my_free(objdescr_p->subtypes[j]);
              objdescr_p->subtypes[j] = NULL;
          }
          my_free(objdescr_p->comments);
          objdescr_p->comments = NULL;
          my_free(objdescr_p->subtypes);
          objdescr_p->subtypes = NULL;

          if (attdescr_p->atype == ATYPE_DYNAMIC_ARRAY || attdescr_p->atype == ATYPE_STATIC_ARRAY)
          {
              obj_array_descr_p = (object_array_descriptor_t *)(attdescr_p);
              if (obj_array_descr_p == NULL)
                  continue;
              my_free(obj_array_descr_p->size_array);
              obj_array_descr_p->size_array = NULL;
          }
      }

    for(i=0;i<=descr_p->max_ikeyword;i++) if(descr_p->attdescr_array[i]!=NULL) {
      /* Common */
      attribute_descriptor_t *attdescr_p=descr_p->attdescr_array[i];
      my_free(attdescr_p->skeyword);
      my_free(attdescr_p->comment);
      my_free(attdescr_p->solver_name); 
      /* Size attribute */
      if(attdescr_p->atype==ATYPE_SIZE) {
	size_descriptor_t *sizedescr_p=(size_descriptor_t *)attdescr_p;
	my_free(sizedescr_p->ikeyword_array);
      }
      /* The attribute descriptor itself */
      my_free(descr_p->attdescr_array[i]);
    }
    /* And finally, the attribute descriptor array */
    my_free(descr_p->attdescr_array);
  }

  for(i=0;i<descr_p->nb_ff;++i) {
    MCDS_delete_fileformat(descr_p->ff_array[i].ff_p);
    my_free(descr_p->ff_array[i].ff_p);
  }
  my_free(descr_p->ff_array);

  return 0;
}


/* --------- static functions --------- */

static counter_type_e get_ctype_from_vtype(value_type_e vtype) {
  switch(vtype) {
  case VTYPE_BOOL:    return CTYPE_BOOL;
  case VTYPE_INT:    return CTYPE_INT;
  case VTYPE_UINT:    return CTYPE_UINT;
  case VTYPE_FLOAT:  return CTYPE_FLOAT;
  case VTYPE_STRING: return CTYPE_STRING;
  case VTYPE_OBJECT: return CTYPE_OBJECT;
  default:           return CTYPE_UNKNOWN;
  }
}


static void resize_descriptor(descriptor_t *descr_p,int ikeyword) {
  if(ikeyword>descr_p->max_ikeyword) {
    int old_size=descr_p->attdescr_array==NULL ? 0 : descr_p->max_ikeyword+1;
    int new_size=(descr_p->max_ikeyword=ikeyword)+1;
    int item_size=sizeof(attribute_descriptor_t *);
    descr_p->attdescr_array=
      (attribute_descriptor_t **)my_realloc(descr_p->attdescr_array,old_size,new_size,item_size);
  }
}


static void set_attribute_descriptor(attribute_descriptor_t *attdescr_p,
				     attribute_type_e atype,value_type_e vtype,
				     const char *skeyword,const char *comment,
				     int index)
{
  attdescr_p->atype=atype;
  attdescr_p->vtype=vtype;
  attdescr_p->skeyword=my_strcpy(attdescr_p->skeyword,skeyword);
  attdescr_p->comment=my_strcpy(attdescr_p->comment,comment);
  attdescr_p->solver_name=my_strcpy(attdescr_p->solver_name,"");
  attdescr_p->index=index;
  attdescr_p->length=0;
}

static void set_object_descriptor(object_descriptor_t *objdescr_p,
				  obj_type_e otype,const char *skeyword,const char *comment,
				  int index) 
{
  set_attribute_descriptor((attribute_descriptor_t *)objdescr_p,
			   ATYPE_VALUE,VTYPE_OBJECT,skeyword,comment,index);
  objdescr_p->otype=otype;
  objdescr_p->allowed_types = NULL;
  objdescr_p->comments = NULL;
  objdescr_p->subtypes = NULL;
  objdescr_p->num = 0;
}


static void add_size_descriptor_ikeyword(size_descriptor_t *sizedescr_p,int ikeyword) {
  int i,nb_ikeywords=sizedescr_p->nb_ikeywords;
  int not_found=1;

  for(i=0;i<nb_ikeywords && not_found;i++) not_found=(sizedescr_p->ikeyword_array[i]!=ikeyword);

  if(not_found) {
    int old_nbikw = sizedescr_p->nb_ikeywords;
    int new_nbikw = sizedescr_p->nb_ikeywords+1;
    int size_ikw  = sizeof(int);
    sizedescr_p->ikeyword_array=(int *)my_realloc(sizedescr_p->ikeyword_array,old_nbikw,new_nbikw,size_ikw);
    sizedescr_p->ikeyword_array[(sizedescr_p->nb_ikeywords)++]=ikeyword;
  }
}

int MCDS_show_descriptor(const descriptor_t *descr_p) 
{
   int i = 0;
   if(descr_p ==NULL) return 1;

   printf("=================descriptor================\n");
   printf("user_id: %d\n",descr_p -> user_id);
   printf("max_ikeyword: %d\n",descr_p -> max_ikeyword);

   printf("Attribute Num: %d\n",descr_p->keyword_map->nb_pairs);
   for(i=0;i<descr_p->keyword_map->nb_pairs;i++)
   {
      printf("skeyword: %s -- ikeyword: %d\n",
             descr_p->keyword_map->pair_array[i]->skeyword,
             descr_p->keyword_map->pair_array[i]->ikeyword);

      printf("attibute type: %d | value type: %d | skeyword: %s | comment: %s | index: %d\n",
             descr_p->attdescr_array[descr_p->keyword_map->pair_array[i]->ikeyword]->atype,
             descr_p->attdescr_array[descr_p->keyword_map->pair_array[i]->ikeyword]->vtype,
             descr_p->attdescr_array[descr_p->keyword_map->pair_array[i]->ikeyword]->skeyword,
             descr_p->attdescr_array[descr_p->keyword_map->pair_array[i]->ikeyword]->comment,
             descr_p->attdescr_array[descr_p->keyword_map->pair_array[i]->ikeyword]->index);
   }

   printf("CTYPE_BOOL NUM: %d | CTYPE_UINT NUM: %d |  CTYPE_INT NUM: %d | CTYPE_FLOAT NUM: %d | CTYPE_STRING NUM: %d | CTYPE_OBJECT NUM: %d | CTYPE_ARRAY: %d \n",
          descr_p-> counter[CTYPE_BOOL],
          descr_p-> counter[CTYPE_UINT],
          descr_p-> counter[CTYPE_INT],
          descr_p-> counter[CTYPE_FLOAT],
          descr_p-> counter[CTYPE_STRING],
          descr_p-> counter[CTYPE_OBJECT],
          descr_p-> counter[CTYPE_ARRAY]);

   printf("format block card number: %d\n",descr_p -> nb_ff);
   for(i=0;i<descr_p -> nb_ff;i++)
   {
      printf("format version: %d\n",descr_p->ff_array-> ff_id);
      printf("Card Num : %d\n",descr_p->ff_array->ff_p->nb_cards); 
   }
   return 0;
}


