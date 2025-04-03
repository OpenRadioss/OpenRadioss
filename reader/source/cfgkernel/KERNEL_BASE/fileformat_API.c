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
#include <string.h> 
#include <KERNEL/Structure_fileformat_others.h>
#include "fileformat_API.h"
#include "expression_API.h"
#include <KERNEL_BASE/utils.h>

#define GET_ATTRIB_FMOR(arglist,type,data_p,attrib) { \
  type *ptr=va_arg(arglist,type*);               \
  *ptr=data_p->attrib;                           \
}

#define SET_ATTRIB_FMOR(arglist,type,data_p,attrib) { \
  data_p->attrib=va_arg(arglist,type);           \
}

void MCDS_new_ff_cell_app_opt(ff_cell_t** cell_pfp);
void MCDS_free_app_opt(ff_cell_t* cell_p);

/* --------- Cells --------- */

int MCDS_new_ff_cell(ff_cell_t **cell_pfp,ff_cell_type_e cell_type) {
  if(cell_pfp==NULL) return 1;
  *cell_pfp=NULL;

  switch(cell_type) {
    
  case CELL_COMMENT:     *cell_pfp=(ff_cell_t *)my_malloc(1,sizeof(ff_comment_cell_t));     break;
    
  case CELL_VALUE:     *cell_pfp=(ff_cell_t *)my_malloc(1,sizeof(ff_value_cell_t));     break;
    
  case CELL_DIR_RADIO: *cell_pfp=(ff_cell_t *)my_malloc(1,sizeof(ff_dir_radio_cell_t)); break;
  case CELL_DIR_FLAGS: *cell_pfp=(ff_cell_t *)my_malloc(1,sizeof(ff_dir_flags_cell_t)); break;
    
    
  case CELL_DIGITS:     *cell_pfp=(ff_cell_t *)my_malloc(1,sizeof(ff_digits_cell_t));   break;
    
    
  case CELL_SCALAR_OR_OBJECT: *cell_pfp=(ff_cell_t *)my_malloc(1,sizeof(ff_scalar_or_object_cell_t)); break;
    
    
  case CELL_FLAGGED_OBJECT: *cell_pfp=(ff_cell_t *)my_malloc(1,sizeof(ff_flagged_object_cell_t)); break;
    
    
  case CELL_SCALAR_OR_STRING: *cell_pfp=(ff_cell_t *)my_malloc(1,sizeof(ff_scalar_or_string_cell_t)); break;
    
    
  case CELL_ID:     *cell_pfp=(ff_cell_t *)my_malloc(1,sizeof(ff_value_cell_t));     break;
   
  case CELL_COND:   *cell_pfp=(ff_cell_t *)my_malloc(1,sizeof(ff_if_cell_t));     break; 
  case CELL_PAIR:   *cell_pfp=(ff_cell_t *)my_malloc(1,sizeof(ff_value_cell_t));     break;
  case CELL_LIST:   *cell_pfp = (ff_cell_t*)my_malloc(1, sizeof(ff_cell_list_t));     break;
  case CELL_BLANK:  *cell_pfp = (ff_cell_t *)my_malloc(1, sizeof(ff_formated_cell_t));     break;
  case CELL_APPEND_OPTIONS:
      MCDS_new_ff_cell_app_opt(cell_pfp);
      break;
  case CELL_NAME_VALUE:  *cell_pfp = (ff_cell_t*)my_malloc(1, sizeof(ff_name_value_cell_t));     break;
  default: return 2; /* break; */
  }
  
  (*cell_pfp)->type=cell_type;

  return 0;
}
  
int MCDS_get_ff_cell_attributes(const ff_cell_t *cell_p,...) {
  va_list   arglist;
  int       attrib;

  if(cell_p==NULL) return 1;
  
  va_start(arglist,cell_p);
  while((attrib=va_arg(arglist,int))!=END_ARGS) switch(attrib) {
  case CELL_TYPE: 
    GET_ATTRIB_FMOR(arglist,int,cell_p,type); 
    break;
  case CELL_SIZE: 
    
    if(cell_p->type!=CELL_COMMENT) { 
      va_end(arglist); 
      return 2;
      }
      else {
      const ff_comment_cell_t *a_cell_p = (const ff_comment_cell_t *)cell_p;
      int                     *a_size_p = va_arg(arglist,int *);
      *a_size_p=(a_cell_p->comment==NULL ? 0 : (int)(strlen(a_cell_p->comment)));
    }
    
    break;
    
    
    
  case CELL_STRING:
    if(cell_p->type!=CELL_COMMENT) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,char *,((ff_comment_cell_t *)cell_p),comment);
    break;
    
  case CELL_FORMAT: 
    if(cell_p->type!=CELL_VALUE && cell_p->type!=CELL_DIR_RADIO &&
       cell_p->type!=CELL_DIR_FLAGS && cell_p->type!=CELL_DIGITS &&
       cell_p->type!=CELL_SCALAR_OR_OBJECT && 
       cell_p->type!=CELL_FLAGGED_OBJECT &&   
       cell_p->type!=CELL_SCALAR_OR_STRING  &&
       cell_p->type != CELL_ID && cell_p->type != CELL_PAIR && cell_p->type != CELL_BLANK && cell_p->type != CELL_LIST &&
       cell_p->type!=CELL_APPEND_OPTIONS && cell_p->type!=CELL_NAME_VALUE) {
      va_end(arglist); 
      return 2; 
    }
    GET_ATTRIB_FMOR(arglist,char *,((ff_formated_cell_t *)cell_p),format);
    break;
  case CELL_NB_IKEYWORDS:
    if(cell_p->type!=CELL_DIGITS) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_digits_cell_t *)cell_p),nb_ikeywords);
    break;
    
  case CELL_IKEYWORD: 
      if (cell_p->type != CELL_VALUE && cell_p->type != CELL_DIR_RADIO && cell_p->type != CELL_PAIR && cell_p->type != CELL_LIST) {
      va_end(arglist);
      return 2;
    }
    GET_ATTRIB_FMOR(arglist,int,((ff_value_cell_t *)cell_p),ikeyword);
    break;
  case CELL_IS_EXTENDED: 
    if(cell_p->type!=CELL_DIR_RADIO) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_dir_radio_cell_t *)cell_p),is_extended);
    break;
  case CELL_DIRX_IKW: 
    if(cell_p->type!=CELL_DIR_FLAGS) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_dir_flags_cell_t *)cell_p),ikeyword_tab[0]);
    break;
  case CELL_DIRY_IKW: 
    if(cell_p->type!=CELL_DIR_FLAGS) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_dir_flags_cell_t *)cell_p),ikeyword_tab[1]);
    break;
  case CELL_DIRZ_IKW: 
    if(cell_p->type!=CELL_DIR_FLAGS) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_dir_flags_cell_t *)cell_p),ikeyword_tab[2]);
    break;
    
    
  case CELL_FLAG_IKW: 
    
    if(cell_p->type==CELL_SCALAR_OR_OBJECT) {
      GET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_object_cell_t *)cell_p),ikeyword_tab[0]);
      
    } else if (cell_p->type==CELL_FLAGGED_OBJECT) {
      GET_ATTRIB_FMOR(arglist,int,((ff_flagged_object_cell_t *)cell_p),ikeyword_tab[1]);
      
    } else if(cell_p->type==CELL_SCALAR_OR_STRING) {
      GET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_string_cell_t *)cell_p),ikeyword_tab[0]);
    } else {
      va_end(arglist);
      return 2;
    }
    
    break;
  case CELL_SCALAR_IKW: 
    
    if(cell_p->type==CELL_SCALAR_OR_OBJECT) {
      GET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_object_cell_t *)cell_p),ikeyword_tab[1]);
    } else if(cell_p->type==CELL_SCALAR_OR_STRING) {
      GET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_string_cell_t *)cell_p),ikeyword_tab[1]);
    } else {
      va_end(arglist);
      return 2;
    }
    
    break;
  case CELL_OBJECT_IKW: 
    
    if(cell_p->type==CELL_SCALAR_OR_OBJECT) { 
        GET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_object_cell_t *)cell_p),ikeyword_tab[2]);
        
    } else if (cell_p->type==CELL_FLAGGED_OBJECT) {
        GET_ATTRIB_FMOR(arglist,int,((ff_flagged_object_cell_t *)cell_p),ikeyword_tab[0]);
    } else {
        va_end(arglist);
        return 2;
    }
    
    break;
    
    
  case CELL_STRING_IKW: 
    if(cell_p->type!=CELL_SCALAR_OR_STRING) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_string_cell_t *)cell_p),ikeyword_tab[2]);
    break;
    
  case CELL_NB_COND_CELL: 
    if(cell_p->type!=CELL_COND) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_if_cell_t *)cell_p),nb_condcell); 
    break;

  case CELL_NAME_VALUE_NUMBER:
      if (cell_p->type != CELL_NAME_VALUE) { va_end(arglist); return 2; }
      GET_ATTRIB_FMOR(arglist, int, ((ff_name_value_cell_t *)cell_p), nb_pairs);
      break;
  case CELL_LIST_INDEX:
      if (cell_p->type != CELL_LIST) { va_end(arglist); return 2; }
      GET_ATTRIB_FMOR(arglist, int, ((ff_cell_list_t*)cell_p), index);
      break;
  case CARD_NAME_VALUE_PAIR_CHAR:
      if (cell_p->type != CELL_NAME_VALUE) { va_end(arglist); return 2; }
      GET_ATTRIB_FMOR(arglist, char, ((ff_name_value_cell_t *)cell_p), pair_char);
      break;
  case CARD_NAME_VALUE_PAIR_SEPARATOR:
      if (cell_p->type != CELL_NAME_VALUE) { va_end(arglist); return 2; }
      GET_ATTRIB_FMOR(arglist, char, ((ff_name_value_cell_t *)cell_p), separator_char);
      break;
  default:
    va_end(arglist);
    return 2;
  }

  va_end(arglist);
  return 0;
}

int MCDS_set_ff_cell_attributes(ff_cell_t *cell_p,...) {
  va_list   arglist;
  int       attrib, i = 0,old_nb = 0,new_nb = 0;

  if(cell_p==NULL) return 1;
  
  va_start(arglist,cell_p);
  while((attrib=va_arg(arglist,int))!=END_ARGS) switch(attrib) {
    
  case CELL_STRING: 
    if(cell_p->type!=CELL_COMMENT) {
      va_end(arglist);
      return 2;
    } else {
      ((ff_formated_cell_t *)cell_p)->format=my_strcpy(((ff_comment_cell_t *)cell_p)->comment,va_arg(arglist,char *));
    }
    break;
    
    
    
   case CELL_FORMAT: 
    if(cell_p->type!=CELL_VALUE && cell_p->type!=CELL_DIR_RADIO &&
       cell_p->type!=CELL_DIR_FLAGS && cell_p->type!=CELL_DIGITS &&
       cell_p->type!=CELL_SCALAR_OR_OBJECT && 
       cell_p->type!=CELL_FLAGGED_OBJECT &&   
       cell_p->type!=CELL_SCALAR_OR_STRING && 
       cell_p->type!=CELL_ID && cell_p->type != CELL_BLANK && 
       cell_p->type!=CELL_PAIR && cell_p->type != CELL_LIST &&
       cell_p->type!=CELL_APPEND_OPTIONS && cell_p->type!=CELL_NAME_VALUE) {
      va_end(arglist);
      return 2;
    }
    ((ff_formated_cell_t *)cell_p)->format=my_strcpy(((ff_formated_cell_t *)cell_p)->format,va_arg(arglist,char *));
    break;
  case CELL_NB_IKEYWORDS:
    {
      int               a_old_nb_ikeywords = 0;
      ff_digits_cell_t *a_cell_p           = NULL;
      if(cell_p->type!=CELL_DIGITS) { va_end(arglist); return 2; }
      a_cell_p=(ff_digits_cell_t *)cell_p;
      a_old_nb_ikeywords=a_cell_p->nb_ikeywords;
      SET_ATTRIB_FMOR(arglist,int,((ff_digits_cell_t *)cell_p),nb_ikeywords);
      a_cell_p->ikeyword_tab=(int *)my_realloc(a_cell_p->ikeyword_tab,
					       a_old_nb_ikeywords,
					       a_cell_p->nb_ikeywords,
					       sizeof(int));
    }
    break;
    
  case CELL_IKEYWORD: 
      if (cell_p->type != CELL_VALUE && cell_p->type != CELL_DIR_RADIO && cell_p->type != CELL_PAIR  && cell_p->type != CELL_LIST) {
      va_end(arglist);
      return 2;
    }
    SET_ATTRIB_FMOR(arglist,int,((ff_value_cell_t *)cell_p),ikeyword);
    break;
  case CELL_IS_EXTENDED: 
    if(cell_p->type!=CELL_DIR_RADIO) { va_end(arglist); return 2; }
    SET_ATTRIB_FMOR(arglist,int,((ff_dir_radio_cell_t *)cell_p),is_extended);
    break;
  case CELL_DIRX_IKW: 
    if(cell_p->type!=CELL_DIR_FLAGS) { va_end(arglist); return 2; }
    SET_ATTRIB_FMOR(arglist,int,((ff_dir_flags_cell_t *)cell_p),ikeyword_tab[0]);
    break;
  case CELL_DIRY_IKW: 
    if(cell_p->type!=CELL_DIR_FLAGS) { va_end(arglist); return 2; }
    SET_ATTRIB_FMOR(arglist,int,((ff_dir_flags_cell_t *)cell_p),ikeyword_tab[1]);
    break;
  case CELL_DIRZ_IKW: 
    if(cell_p->type!=CELL_DIR_FLAGS) { va_end(arglist); return 2; }
    SET_ATTRIB_FMOR(arglist,int,((ff_dir_flags_cell_t *)cell_p),ikeyword_tab[2]);
    break;
    
    
  case CELL_FLAG_IKW: 
    
    if(cell_p->type==CELL_SCALAR_OR_OBJECT) {
      SET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_object_cell_t *)cell_p),ikeyword_tab[0]);
      
    } else if (cell_p->type==CELL_FLAGGED_OBJECT) {
      SET_ATTRIB_FMOR(arglist,int,((ff_flagged_object_cell_t *)cell_p),ikeyword_tab[1]);
      
    } else if(cell_p->type==CELL_SCALAR_OR_STRING) {
      SET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_string_cell_t *)cell_p),ikeyword_tab[0]);
    } else {
      va_end(arglist);
      return 2;
    }
    
    break;
  case CELL_SCALAR_IKW: 
    
    
    if(cell_p->type==CELL_SCALAR_OR_OBJECT) { 
      SET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_object_cell_t *)cell_p),ikeyword_tab[1]);
    } else if(cell_p->type==CELL_SCALAR_OR_STRING) {
      SET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_string_cell_t *)cell_p),ikeyword_tab[1]);
    } else {
      va_end(arglist);
      return 2;
    }
    
    break;
  case CELL_OBJECT_IKW: 
    
    if(cell_p->type==CELL_SCALAR_OR_OBJECT) { 
        SET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_object_cell_t *)cell_p),ikeyword_tab[2]);
        
    } else if (cell_p->type==CELL_FLAGGED_OBJECT) {
        SET_ATTRIB_FMOR(arglist,int,((ff_flagged_object_cell_t *)cell_p),ikeyword_tab[0]);
    } else {
        va_end(arglist);
        return 2;
    }
    
    break;
    
    
  case CELL_STRING_IKW: 
    if(cell_p->type!=CELL_SCALAR_OR_STRING) { va_end(arglist); return 2; }
    SET_ATTRIB_FMOR(arglist,int,((ff_scalar_or_string_cell_t *)cell_p),ikeyword_tab[2]);
    break;
    

  
  case CELL_NB_COND_CELL: 

    if(cell_p->type!=CELL_COND) { va_end(arglist); return 2; }

    old_nb=((ff_if_cell_t *)cell_p)->nb_condcell;
    SET_ATTRIB_FMOR(arglist,int,((ff_if_cell_t *)cell_p),nb_condcell); 
    new_nb=((ff_if_cell_t *)cell_p)->nb_condcell;

    for(i=new_nb;i<old_nb;i++) {
      MCDS_delete_ff_condcell(((ff_if_cell_t *)cell_p)->condcell_array[i]);
      my_free(((ff_if_cell_t *)cell_p)->condcell_array[i]);
    }

    ((ff_if_cell_t *)cell_p)->condcell_array=
      (ff_condcell_t **)my_realloc(((ff_if_cell_t *)cell_p)->condcell_array,old_nb,new_nb,sizeof(ff_condcell_t *));

    break;
  case CELL_NAME_VALUE_NUMBER:
      if (cell_p->type != CELL_NAME_VALUE) { va_end(arglist); return 2; }

      old_nb = ((ff_name_value_cell_t *)cell_p)->nb_pairs;
      SET_ATTRIB_FMOR(arglist, int, ((ff_name_value_cell_t *)cell_p), nb_pairs);
      new_nb = ((ff_name_value_cell_t *)cell_p)->nb_pairs;

      if (((ff_name_value_cell_t *)cell_p)->name_value_array)
      {
          for (i = new_nb; i < old_nb; i++) {
              MCDS_delete_ff_namevaluearray(((ff_name_value_cell_t *)cell_p)->name_value_array[i]);
              my_free(((ff_name_value_cell_t *)cell_p)->name_value_array[i]);
          }
      }

      ((ff_name_value_cell_t *)cell_p)->name_value_array =
          (ff_name_info_t **)my_realloc(((ff_name_value_cell_t *)cell_p)->name_value_array, old_nb, new_nb, sizeof(ff_name_info_t *));
      break;
  case CARD_NAME_VALUE_PAIR_CHAR:
      if (cell_p->type != CELL_NAME_VALUE) { va_end(arglist); return 2; }
      /* need a cast here since va_arg only takes fully promoted types 
         If we pass as a char its Illegal instruction (SIGILL) (crash in Linux)
      */
      SET_ATTRIB_FMOR(arglist, int, ((ff_name_value_cell_t *)cell_p), pair_char);
      break;
  case CARD_NAME_VALUE_PAIR_SEPARATOR:
      if (cell_p->type != CELL_NAME_VALUE) { va_end(arglist); return 2; }
      /* need a cast here since va_arg only takes fully promoted types
         If we pass as a char its Illegal instruction (SIGILL) (crash in Linux)
      */
      SET_ATTRIB_FMOR(arglist, int, ((ff_name_value_cell_t *)cell_p), separator_char);
      break;
  case CELL_LIST_INDEX:
      if (cell_p->type != CELL_LIST) { va_end(arglist); return 2; }
      SET_ATTRIB_FMOR(arglist, int, ((ff_cell_list_t*)cell_p), index);
      break;
  default:
    va_end(arglist);
    return 2;
  }

 va_end(arglist);
 return 0;
}


int MCDS_get_ff_cell_tab(const ff_cell_t *cell_p,int attrib,int ind,void *data_p) {
  if(cell_p==NULL) return 1;
  if(data_p==NULL) return 4;
  
  switch(attrib) {
  case CELL_IKEYWORD:
    {
      const ff_digits_cell_t *a_cell_p     = (const ff_digits_cell_t *)cell_p;
      int                    *a_ikeyword_p = (int *)data_p;
      if(cell_p->type!=CELL_DIGITS)            return 2;
      if(ind<0 || ind>=a_cell_p->nb_ikeywords) return 3;
      *a_ikeyword_p=a_cell_p->ikeyword_tab[ind];
    }
    break;
  case CELL_COND_CELL:
  {
      ff_condcell_t **a_ccl_pp = (ff_condcell_t **)data_p;
      if (cell_p->type != CELL_COND) return 2;
      if (ind < 0 || ind >= ((ff_if_cell_t *)cell_p)->nb_condcell) return 3;
      *a_ccl_pp = (((ff_if_cell_t *)cell_p)->condcell_array)[ind];
      break;
  }
  case CELL_NAME_VALUE_IKEYWORD:
  {
      int *a_int_value_p = (int *)data_p;
      ff_name_info_t *a_name_value_array = NULL;
      if (cell_p->type != CELL_NAME_VALUE) return 2;
      if (ind < 0 || ind >= ((ff_name_value_cell_t *)cell_p)->nb_pairs) return 3;
      a_name_value_array = (((ff_name_value_cell_t *)cell_p)->name_value_array)[ind];
      *a_int_value_p = a_name_value_array->ikeyword;
      break;
  }
  case CELL_NAME_VALUE_STRING:
  {
      char **a_char_value_p = (char **)data_p;
      ff_name_info_t *a_name_value_array = NULL;
      if (cell_p->type != CELL_NAME_VALUE) return 2;
      if (ind < 0 || ind >= ((ff_name_value_cell_t *)cell_p)->nb_pairs) return 3;
      a_name_value_array = (((ff_name_value_cell_t *)cell_p)->name_value_array)[ind];
      *a_char_value_p = a_name_value_array->name;
      break;
  }
  default:
    return 2;
  }

  return 0;
}



int MCDS_set_ff_cell_tab(ff_cell_t *cell_p,int attrib,int ind,const void *data_p) {
  if(cell_p==NULL) return 1;
  if(data_p==NULL) return 4;
  
  switch(attrib) {
  case CELL_IKEYWORD:
    {
      ff_digits_cell_t *a_cell_p     = (ff_digits_cell_t *)cell_p;
      const int        *a_ikeyword_p = (const int *)data_p;
      if(cell_p->type!=CELL_DIGITS)            return 2;
      if(ind<0 || ind>=a_cell_p->nb_ikeywords) return 3;
      a_cell_p->ikeyword_tab[ind]=(*a_ikeyword_p);
    }
    break;
  case CELL_COND_CELL:
    {
      ff_condcell_t **a_ccl_pp  = (ff_condcell_t **)data_p;
      if(cell_p->type!=CELL_COND) return 2;

      if(ind<0 || ind>=((ff_if_cell_t *)cell_p)->nb_condcell) return 3;

      if((((ff_if_cell_t *)cell_p)->condcell_array)[ind]!=NULL) {
          MCDS_delete_ff_condcell((((ff_if_cell_t *)cell_p)->condcell_array)[ind]);
          my_free((((ff_if_cell_t *)cell_p)->condcell_array)[ind]);
      }
      (((ff_if_cell_t *)cell_p)->condcell_array)[ind] = *a_ccl_pp;
    }
    break;
    case CELL_NAME_VALUE_IKEYWORD:
    {
        int *value = (int *)data_p;
        ff_name_info_t **item = NULL;
        if (cell_p->type != CELL_NAME_VALUE) return 2;
        if (ind < 0 || ind >= ((ff_name_value_cell_t *)cell_p)->nb_pairs) return 3;

        item = ((ff_name_value_cell_t *)cell_p)->name_value_array;
        if (item[ind] == NULL)
        {
            item[ind] = (ff_name_info_t *)my_malloc(1, sizeof(ff_name_info_t));
        }
        (item[ind])->ikeyword = *value;
        break;
    }
    case CELL_NAME_VALUE_STRING:
    {
        const char *a_str = (char *)data_p;
        ff_name_info_t **item = NULL;
        if (cell_p->type != CELL_NAME_VALUE) return 2;
        if (ind < 0 || ind >= ((ff_name_value_cell_t *)cell_p)->nb_pairs) return 3;

        item = ((ff_name_value_cell_t *)cell_p)->name_value_array;
        if (item[ind] == NULL)
        {
            item[ind] = (ff_name_info_t *)my_malloc(1, sizeof(ff_name_info_t));
        }
        item[ind]->name = (char *)malloc((strlen(a_str) + 1) * sizeof(char));
        strcpy((item[ind])->name, a_str);
        break;
    }
    default:
        return 2;
    }

    return 0;
}



int MCDS_delete_ff_cell(ff_cell_t *cell_p) {
  int i = 0;
  if(cell_p==NULL) return 1;
  switch(cell_p->type) {
  case CELL_DIGITS:
    my_free(((ff_digits_cell_t *)cell_p)->ikeyword_tab);
    /* no break */
  case CELL_VALUE:
  case CELL_DIR_RADIO:
  case CELL_DIR_FLAGS:
  case CELL_SCALAR_OR_OBJECT: 
  case CELL_FLAGGED_OBJECT:   
  case CELL_SCALAR_OR_STRING: 
  case CELL_ID:
  case CELL_PAIR:
  case CELL_LIST:
  case CELL_BLANK:
    my_free(((ff_formated_cell_t *)cell_p)->format);
    break;
    
  case CELL_COMMENT:
    my_free(((ff_comment_cell_t *)cell_p)->comment);
    break;
  case CELL_COND:
    for(i=0;i<((ff_if_cell_t *)cell_p)->nb_condcell;i++) {
      MCDS_delete_ff_condcell(((ff_if_cell_t *)cell_p)->condcell_array[i]);
      my_free(((ff_if_cell_t *)cell_p)->condcell_array[i]);
    }
    my_free(((ff_if_cell_t *)cell_p)->condcell_array);    
    break;
    case CELL_NAME_VALUE:
        {
            for (i = 0; i < ((ff_name_value_cell_t *)cell_p)->nb_pairs; i++)
            {
                MCDS_delete_ff_namevaluearray(((ff_name_value_cell_t *)cell_p)->name_value_array[i]);
                my_free(((ff_name_value_cell_t *)cell_p)->name_value_array[i]);
            }
            my_free(((ff_name_value_cell_t *)cell_p)->name_value_array);
        }
      break;
  case CELL_APPEND_OPTIONS:
      MCDS_free_app_opt(cell_p);
      break;
  default:
    return 2;
  }
  return 0;
}


/* --------- Conditional list of cells --------- */


int MCDS_get_ff_condcell_expression(const ff_condcell_t *ccl_p,expression_t **expr_pp) {
  if(ccl_p==NULL)   return 1;
  if(expr_pp==NULL) return 2;

  *expr_pp=ccl_p->expression;
  
  return 0;
}

int MCDS_new_ff_condcell(ff_condcell_t **ccl_pp, expression_t *expr_p, ff_cell_t *cell_p) {
  if(ccl_pp==NULL) return 1;

  *ccl_pp               = (ff_condcell_t *)my_malloc(1,sizeof(ff_condcell_t));
  (*ccl_pp)->expression = expr_p;  
  (*ccl_pp)->cell = cell_p;

  return 0;
}


int MCDS_get_ff_condcell_cell(const ff_condcell_t *ccl_p,ff_cell_t **cell_p) {
  if(ccl_p==NULL)   return 1;
  if(cell_p==NULL) return 2;

  *cell_p=ccl_p->cell;
  
  return 0;
}

int MCDS_delete_ff_condcell(ff_condcell_t *ccl_p) {

  if(ccl_p==NULL) return 1;

  MCDS_delete_expression(ccl_p->expression, 1);
  my_free(ccl_p->expression);


  MCDS_delete_ff_cell(ccl_p->cell);
  my_free(ccl_p->cell);

  return 0;
}

int MCDS_delete_ff_namevaluearray(ff_name_info_t *ccl_p) {

    if (ccl_p == NULL) return 1;

    my_free(ccl_p->name);
    return 0;
}
/*--------------------------*/







/* --------- Cards --------- */

int MCDS_new_ff_card(ff_card_t **card_pfp,ff_card_type_e card_type) {
  if(card_pfp==NULL) return 1;
  *card_pfp=NULL;

  switch(card_type) {
  case CARD_BLANK:     *card_pfp=(ff_card_t *)my_malloc(1,sizeof(ff_blank_card_t));     break;
  
  case CARD_SINGLE:
  case CARD_COMMENT:
  
  case CARD_HEADER:  
  case CARD_FREE_FORMAT:	
  case CARD_PREREAD:  
	  *card_pfp=(ff_card_t *)my_malloc(1,sizeof(ff_single_card_t));    
	  break;

  
  case CARD_LIST:      *card_pfp=(ff_card_t *)my_malloc(1,sizeof(ff_list_card_t));      break;
  
  case CARD_CELL_LIST: 
      *card_pfp=(ff_card_t *)my_malloc(1,sizeof(ff_cell_list_card_t));
      ((ff_cell_list_card_t *)*card_pfp)->offset_fmt = my_strcpy(((ff_cell_list_card_t *)*card_pfp)->offset_fmt, "");
      ((ff_cell_list_card_t *)*card_pfp)->offset_value = my_strcpy(((ff_cell_list_card_t *)*card_pfp)->offset_value, "");
      break;
  case CARD_CARD_LIST: *card_pfp=(ff_card_t *)my_malloc(1,sizeof(ff_card_list_card_t)); break;
  
  case CARD_IF:        *card_pfp=(ff_card_t *)my_malloc(1,sizeof(ff_if_card_t));        break;
  
  
  case CARD_OBJECT_LIST:
    *card_pfp=(ff_card_t *)my_malloc(1,sizeof(ff_object_list_card_t));
    ((ff_object_list_card_t*)*card_pfp)->comment=my_strcpy(((ff_object_list_card_t*)*card_pfp)->comment,"");
    ((ff_object_list_card_t *)*card_pfp)->cell_format=my_strcpy(((ff_object_list_card_t *)*card_pfp)->cell_format,"");
    break;
    
    
  case CARD_SUBOBJECTS:
    *card_pfp=(ff_card_t *)my_malloc(1,sizeof(ff_subobjects_card_t));
    break;
    
  default:
    return 2;
    /*break;*/
  
  }
  
  (*card_pfp)->type=card_type;

  return 0;
}
  
int MCDS_new_ff_assign_card(ff_card_t **card_pfp, assign_operator_e assign_mode)
{
    if (card_pfp == NULL) return 1;
    *card_pfp = NULL;
    if (assign_mode == ASSIGN_ADD || assign_mode == ASSIGN_SUB || assign_mode == ASSIGN_MUL || assign_mode == ASSIGN_DIV)
        *card_pfp = (ff_card_t *)my_malloc(1, sizeof(ff_card_assign_basic_operations_t));
    else if (assign_mode == ASSIGN_ATTRIB)
        *card_pfp = (ff_card_t *)my_malloc(1, sizeof(ff_card_assign_Copy_t));
    else if (assign_mode == ASSIGN_GET_ENTITY_VALUE || assign_mode == ASSIGN_GET_CURRENT_ENTITY)
        *card_pfp = (ff_card_t *)my_malloc(1, sizeof(ff_card_assign_entity_value_t));
    else if (assign_mode == ASSIGN_GET_NLOOKUP_VALUE)
        *card_pfp = (ff_card_t *)my_malloc(1, sizeof(ff_card_assign_nlookup_t));
    else if (assign_mode == ASSIGN_GET_DISPLAY_STATUS)
        *card_pfp = (ff_card_t *)my_malloc(1, sizeof(ff_card_assign_displaystatus_t));
    else if (assign_mode == ASSIGN_PUSH)
        *card_pfp = (ff_card_t *)my_malloc(1, sizeof(ff_card_assign_push_t));
    else if (assign_mode == ASSIGN_EXPRESSION || assign_mode == ASSIGN_GET_NEXT_MAX_AVAILABLE_ID  || assign_mode == ASSIGN_GET_NB_FREE_CARDS || assign_mode == ASSIGN_GET_FORMAT_TYPE)
        *card_pfp = (ff_card_t *)my_malloc(1, sizeof(ff_card_assign_header_t));
    else if (assign_mode == ASSIGN_COMBINE || assign_mode == ASSIGN_ERASE || assign_mode == ASSIGN_FIND)
        * card_pfp = (ff_card_t*)my_malloc(1, sizeof(ff_card_assign_string_t));

    if (*card_pfp)
    {
        (*card_pfp)->type = CARD_ASSIGN;
    }
    return 0;
}

int MCDS_get_ff_card_attributes(const ff_card_t *card_p,...) {
  va_list   arglist;
  int       attrib;

  if(card_p==NULL) return 1;
  
  va_start(arglist,card_p);
  while((attrib=va_arg(arglist,int))!=END_ARGS) switch(attrib) {
  case CARD_TYPE: 
    GET_ATTRIB_FMOR(arglist,int,card_p,type); 
    break;
  
    
  case CARD_STRING:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,char *,((ff_object_list_card_t*)card_p),comment);
    break;
    
  
  case CARD_NB_CELLS: 
    
    switch(card_p->type) {
    case CARD_SINGLE:
    case CARD_COMMENT:
    case CARD_LIST:
    case CARD_CELL_LIST:
	case CARD_HEADER:
	case CARD_FREE_FORMAT:
    case CARD_PREREAD:  
      GET_ATTRIB_FMOR(arglist,int,((ff_single_card_t *)card_p),nb_cells);    
      break;
    default:
      va_end(arglist); return 2;
      /*break;*/
    }
    
    break;
  case CARD_SIZE:     
    switch(card_p->type) {
    case CARD_CELL_LIST: GET_ATTRIB_FMOR(arglist,int,((ff_cell_list_card_t *)card_p),size); break;
    case CARD_CARD_LIST: GET_ATTRIB_FMOR(arglist,int,((ff_card_list_card_t *)card_p),size); break;
    default:             va_end(arglist); return 2;                                 /* break; */
    }
    break;
    
  case CARD_LENGTH_MAX:     
    
    switch(card_p->type) {
    case CARD_LIST:
      GET_ATTRIB_FMOR(arglist,int,((ff_list_card_t *)card_p),length_max); 
      break;
    case CARD_CELL_LIST:
       GET_ATTRIB_FMOR(arglist,int,((ff_cell_list_card_t *)card_p),length_max); 
       break ;
    case CARD_OBJECT_LIST:
       GET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),length_max); 
       break;
    default:
      va_end(arglist); return 2;
      /*break;*/
    }
    
    break;
  case CARD_IKEYWORD_NB_BLOCKS:
      switch (card_p->type) {
      case CARD_LIST:
          GET_ATTRIB_FMOR(arglist, int, ((ff_list_card_t*)card_p), ikeyword);
          break;
      }
      break;
    
    
    
    
  case CARD_IS_FREE:     
    if(card_p->type==CARD_SINGLE || card_p->type==CARD_CELL_LIST) {
      GET_ATTRIB_FMOR(arglist,int,((ff_single_card_t *)card_p),is_free);    
    } else if(card_p->type==CARD_OBJECT_LIST) {
      GET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),is_free);
	} else if (card_p->type == CARD_CARD_LIST) {
		GET_ATTRIB_FMOR(arglist, int, ((ff_card_list_card_t *)card_p), is_free);
	} else {
      va_end(arglist); return 2;
    }
    break;
    
    
    
  case CARD_NB_CARDS: 
    if(card_p->type!=CARD_CARD_LIST) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_card_list_card_t *)card_p),nb_cards); 
    break;
    
  case CARD_NB_COND_CARD_LISTS: 
    if(card_p->type!=CARD_IF) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_if_card_t *)card_p),nb_condcardlists); 
    break;
    
    
  case CARD_CELL_FORMAT:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,char *,((ff_object_list_card_t *)card_p),cell_format); 
    break;
  case CARD_POS_SIZE:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),pos_size_ikw); 
    break;
  case CARD_NEG_SIZE:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),neg_size_ikw); 
    break;
  case CARD_POS_ARRAY:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),pos_array_ikw); 
    break;
  case CARD_NEG_ARRAY:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),neg_array_ikw); 
    break;
  case CARD_NB_OTYPES:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),nb_otypes); 
    break;
    
    
  case CARD_OBJECTS_IKW:
    if(card_p->type!=CARD_SUBOBJECTS) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,int,((ff_subobjects_card_t *)card_p),objects_ikw); 
    break;
  case CARD_KFULLTYPE:
    if(card_p->type!=CARD_SUBOBJECTS) { va_end(arglist); return 2; }
    GET_ATTRIB_FMOR(arglist,char *,((ff_subobjects_card_t *)card_p),kfulltype); 
    break;
  case CARD_FREE_CARD_LIST_TOKEN_NB:
	  if (card_p->type != CARD_CARD_LIST) { va_end(arglist); return 2; }
	  GET_ATTRIB_FMOR(arglist, int, ((ff_card_list_card_t *)card_p), nb_token);
	  break;
  case CARD_FLAGS:
	  GET_ATTRIB_FMOR(arglist, int, card_p, flag);
	  break;
  case CARD_CELL_LIST_OFFSET_FORMAT:
      if (card_p->type != CARD_CELL_LIST) { va_end(arglist); return 2; }
      GET_ATTRIB_FMOR(arglist, char *, ((ff_cell_list_card_t *)card_p), offset_fmt);
      break;
  case CARD_CELL_LIST_OFFSET_VALUE:
      if (card_p->type != CARD_CELL_LIST) { va_end(arglist); return 2; }
      GET_ATTRIB_FMOR(arglist, char *, ((ff_cell_list_card_t *)card_p), offset_value);
      break;
  case CARD_SUBOBJ_PARENT_LNK_ATT:
      if (card_p->type != CARD_SUBOBJECTS) { va_end(arglist); return 2; }
      GET_ATTRIB_FMOR(arglist, char*, ((ff_subobjects_card_t*)card_p), plnkatt);
      break;
  case CARD_SUBOBJ_CHILD_LNK_ATT:
      if (card_p->type != CARD_SUBOBJECTS) { va_end(arglist); return 2; }
      GET_ATTRIB_FMOR(arglist, char*, ((ff_subobjects_card_t*)card_p), clnkatt);
      break;
  default:
    va_end(arglist);
    return 2;
  }

  va_end(arglist);
  return 0;
}

int MCDS_set_ff_card_attributes(ff_card_t *card_p,...) {
  va_list   arglist;
  int       attrib,i,old_nb,new_nb;

  if(card_p==NULL) return 1;
  
  va_start(arglist,card_p);
  while((attrib=va_arg(arglist,int))!=END_ARGS) switch(attrib) {
    
    
  case CARD_STRING:
    {
      char *a_comment=((ff_object_list_card_t*)card_p)->comment;
      if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
      ((ff_object_list_card_t*)card_p)->comment=my_strcpy(a_comment,va_arg(arglist,char *));
    }
    break;
    
    
  case CARD_NB_CELLS: 
    
    switch(card_p->type) {
    case CARD_SINGLE:
    case CARD_COMMENT:
    case CARD_LIST:
    case CARD_CELL_LIST:
	case CARD_HEADER:
	case CARD_FREE_FORMAT:
    case CARD_PREREAD:  
      old_nb=((ff_single_card_t *)card_p)->nb_cells;
      SET_ATTRIB_FMOR(arglist,int,((ff_single_card_t *)card_p),nb_cells);    
      new_nb=((ff_single_card_t *)card_p)->nb_cells;
      for(i=new_nb;i<old_nb;i++) my_free(((ff_single_card_t *)card_p)->cell_array[i]);
      ((ff_single_card_t *)card_p)->cell_array=
	(ff_cell_t **)my_realloc(((ff_single_card_t *)card_p)->cell_array,old_nb,new_nb,sizeof(ff_cell_t *));
      break;


    default:
      va_end(arglist); return 2;
      /*break;*/
    }
    
    break;
  case CARD_SIZE:     
    switch(card_p->type) {
    case CARD_CELL_LIST: SET_ATTRIB_FMOR(arglist,int,((ff_cell_list_card_t *)card_p),size); break;
    case CARD_CARD_LIST: SET_ATTRIB_FMOR(arglist,int,((ff_card_list_card_t *)card_p),size); break;
    default:             va_end(arglist); return 2;                                 /* break; */
    }
    break;
    
  case CARD_LENGTH_MAX: 
    
    switch(card_p->type) {
    case CARD_LIST:
      SET_ATTRIB_FMOR(arglist,int,((ff_list_card_t *)card_p),length_max);
      break;
    case CARD_CELL_LIST:
      SET_ATTRIB_FMOR(arglist,int,((ff_cell_list_card_t *)card_p),length_max);    
      break;
    case CARD_OBJECT_LIST:
      SET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),length_max);    
      break;
    default:
      va_end(arglist); return 2;
      /*break;*/
    }
    

/*    if(card_p->type!=CARD_CELL_LIST) { va_end(arglist); return 2; }*/
/*    SET_ATTRIB_FMOR(arglist,int,((ff_cell_list_card_t *)card_p),length_max);    */

    break;
  case CARD_IKEYWORD_NB_BLOCKS:
      switch (card_p->type) {
      case CARD_LIST:
          SET_ATTRIB_FMOR(arglist, int, ((ff_list_card_t*)card_p), ikeyword);
          break;
      }
      break;
  case CARD_IS_FREE:     
	  
	  if(card_p->type==CARD_SINGLE || card_p->type==CARD_CELL_LIST || card_p->type==CARD_HEADER || card_p->type == CARD_FREE_FORMAT) {
      SET_ATTRIB_FMOR(arglist,int,((ff_single_card_t *)card_p),is_free);    
    } else if(card_p->type==CARD_OBJECT_LIST) {
      SET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),is_free);    
	}
	else if (card_p->type == CARD_CARD_LIST) {
		SET_ATTRIB_FMOR(arglist, int, ((ff_card_list_card_t *)card_p), is_free);
	} else {
      va_end(arglist); return 2;
    }
    break;
    
    
    
  case CARD_NB_CARDS: 
    if(card_p->type!=CARD_CARD_LIST) { va_end(arglist); return 2; }
    old_nb=((ff_card_list_card_t *)card_p)->nb_cards;
    SET_ATTRIB_FMOR(arglist,int,((ff_card_list_card_t *)card_p),nb_cards); 
    new_nb=((ff_card_list_card_t *)card_p)->nb_cards;
    for(i=new_nb;i<old_nb;i++) {
      MCDS_delete_ff_card(((ff_card_list_card_t *)card_p)->card_array[i]);
      my_free(((ff_card_list_card_t *)card_p)->card_array[i]);
    }
    ((ff_card_list_card_t *)card_p)->card_array=
      (ff_card_t **)my_realloc(((ff_card_list_card_t *)card_p)->card_array,old_nb,new_nb,sizeof(ff_card_t *));
    break;
   
  case CARD_NB_COND_CARD_LISTS: 
    if(card_p->type!=CARD_IF) { va_end(arglist); return 2; }
    old_nb=((ff_if_card_t *)card_p)->nb_condcardlists;
    SET_ATTRIB_FMOR(arglist,int,((ff_if_card_t *)card_p),nb_condcardlists); 
    new_nb=((ff_if_card_t *)card_p)->nb_condcardlists;
    for(i=new_nb;i<old_nb;i++) {
      MCDS_delete_ff_condcardlist(((ff_if_card_t *)card_p)->condcardlist_array[i]);
      my_free(((ff_if_card_t *)card_p)->condcardlist_array[i]);
    }
    ((ff_if_card_t *)card_p)->condcardlist_array=
      (ff_condcardlist_t **)my_realloc(((ff_if_card_t *)card_p)->condcardlist_array,old_nb,new_nb,sizeof(ff_condcardlist_t *));
    break;
    
    
  case CARD_CELL_FORMAT:
    {
      char *a_cell_format=((ff_object_list_card_t *)card_p)->cell_format;
      if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
      ((ff_object_list_card_t *)card_p)->cell_format=my_strcpy(a_cell_format,va_arg(arglist,char *));
    }
    break;
  case CARD_POS_SIZE:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    SET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),pos_size_ikw); 
    break;
  case CARD_NEG_SIZE:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    SET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),neg_size_ikw); 
    break;
  case CARD_POS_ARRAY:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    SET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),pos_array_ikw); 
    break;
  case CARD_NEG_ARRAY:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    SET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),neg_array_ikw); 
    break;
  case CARD_NB_OTYPES:
    if(card_p->type!=CARD_OBJECT_LIST) { va_end(arglist); return 2; }
    old_nb=((ff_object_list_card_t *)card_p)->nb_otypes;
    SET_ATTRIB_FMOR(arglist,int,((ff_object_list_card_t *)card_p),nb_otypes); 
    new_nb=((ff_object_list_card_t *)card_p)->nb_otypes;
    ((ff_object_list_card_t *)card_p)->otype_tab=
      (obj_type_e *)my_realloc(((ff_object_list_card_t *)card_p)->otype_tab,old_nb,new_nb,sizeof(obj_type_e));
    break;
    
    
  case CARD_OBJECTS_IKW:
    if(card_p->type!=CARD_SUBOBJECTS) { va_end(arglist); return 2; }
    SET_ATTRIB_FMOR(arglist,int,((ff_subobjects_card_t *)card_p),objects_ikw); 
    break;
  case CARD_KFULLTYPE:
    if(card_p->type!=CARD_SUBOBJECTS) { va_end(arglist); return 2; }
    ((ff_subobjects_card_t *)card_p)->kfulltype=my_strcpy(((ff_subobjects_card_t *)card_p)->kfulltype,va_arg(arglist,char *));
    break;
  case CARD_FREE_CARD_LIST_TOKEN_NB:
	  if (card_p->type != CARD_CARD_LIST) return 2;
	  old_nb = ((ff_card_list_card_t *)card_p)->nb_token;
	  SET_ATTRIB_FMOR(arglist, int, ((ff_card_list_card_t *)card_p), nb_token);
	  new_nb = ((ff_card_list_card_t *)card_p)->nb_token;
	  ((ff_card_list_card_t *)card_p)->token_array =
		  (char **)my_realloc(((ff_card_list_card_t *)card_p)->token_array, old_nb, new_nb, sizeof(char *));
	  break;
  case CARD_FLAGS:
	  SET_ATTRIB_FMOR(arglist, int, card_p, flag);
	  break;
  case CARD_CELL_LIST_OFFSET_FORMAT:
  {
      char *offset_format = ((ff_cell_list_card_t *)card_p)->offset_fmt;
      if (card_p->type != CARD_CELL_LIST) { va_end(arglist); return 2; }
      ((ff_cell_list_card_t *)card_p)->offset_fmt = my_strcpy(offset_format, va_arg(arglist, char *));
  }
  break;
  case CARD_CELL_LIST_OFFSET_VALUE:
  {
      char *offset_value = ((ff_cell_list_card_t *)card_p)->offset_value;
      if (card_p->type != CARD_CELL_LIST) { va_end(arglist); return 2; }
      ((ff_cell_list_card_t *)card_p)->offset_value = my_strcpy(offset_value, va_arg(arglist, char *));
  }
  break;
  case CARD_SUBOBJ_PARENT_LNK_ATT:
      if (card_p->type != CARD_SUBOBJECTS) { va_end(arglist); return 2; }
      ((ff_subobjects_card_t*)card_p)->plnkatt = my_strcpy(((ff_subobjects_card_t*)card_p)->plnkatt, va_arg(arglist, char*));
      break;
  case CARD_SUBOBJ_CHILD_LNK_ATT:
      if (card_p->type != CARD_SUBOBJECTS) { va_end(arglist); return 2; }
      ((ff_subobjects_card_t*)card_p)->clnkatt = my_strcpy(((ff_subobjects_card_t*)card_p)->clnkatt, va_arg(arglist, char*));
      break;
  default:
    va_end(arglist);
    return 2;
  }

  va_end(arglist);
  return 0;
}

int MCDS_get_ff_card_tab(const ff_card_t *card_p,int attrib,int ind,void *data_p) {
  ff_cell_t         **cell_pp  = (ff_cell_t **)data_p;
  ff_card_t         **card_pp  = (ff_card_t **)data_p;
  ff_condcardlist_t **a_ccl_pp = (ff_condcardlist_t **)data_p; 
  obj_type_e        *a_otype_p = (obj_type_e *)data_p;         
  char             **a_char_p = (char **)data_p;
  if(card_p==NULL) return 1;
  if(data_p==NULL) return 4;
  
  switch(attrib) {
  case CARD_CELL:
    
    switch(card_p->type) {
    case CARD_SINGLE:
    case CARD_COMMENT:
    case CARD_LIST:
    case CARD_CELL_LIST:
	case CARD_HEADER:
	case CARD_FREE_FORMAT:	
    case CARD_PREREAD:  
      if(ind<0 || ind>=((ff_single_card_t *)card_p)->nb_cells) return 3;
      *cell_pp=(((ff_single_card_t *)card_p)->cell_array)[ind];
    default:
      return 2;
      /*break */
    }
    
    break;
  case CARD_CARD:
    if(card_p->type!=CARD_CARD_LIST) return 2;
    if(ind<0 || ind>=((ff_card_list_card_t *)card_p)->nb_cards) return 3;
    *card_pp=(((ff_card_list_card_t *)card_p)->card_array)[ind];
    break;
    
  case CARD_COND_CARD_LIST:
    if(card_p->type!=CARD_IF) return 2;
    if(ind<0 || ind>=((ff_if_card_t *)card_p)->nb_condcardlists) return 3;
    *a_ccl_pp=(((ff_if_card_t *)card_p)->condcardlist_array)[ind];
    break;
    
    
  case CARD_OTYPE:
    if(card_p->type!=CARD_OBJECT_LIST) return 2;
    if(ind<0 || ind>=((ff_object_list_card_t *)card_p)->nb_otypes) return 3;
    *a_otype_p=((ff_object_list_card_t *)card_p)->otype_tab[ind];
    break;
  case CARD_FREE_CARD_LIST_TOKEN_STR:
	  if (card_p->type != CARD_CARD_LIST) return 2;
	  if (ind<0 || ind >= ((ff_card_list_card_t *)card_p)->nb_token) return 3;
	  *a_char_p = ((ff_card_list_card_t *)card_p)->token_array[ind];
	  break;
    
  default:
    return 2;
  }

  return 0;
}

int MCDS_set_ff_card_tab(ff_card_t *card_p,int attrib,int ind,const void *data_p) {
  ff_cell_t         **cell_pp   = (ff_cell_t **)data_p;
  ff_card_t         **card_pp   = (ff_card_t **)data_p;
  ff_condcardlist_t **a_ccl_pp  = (ff_condcardlist_t **)data_p; 
  obj_type_e         *a_otype_p = (obj_type_e *)data_p;         
  char              **a_char_p = (char **)data_p;
  if(card_p==NULL) return 1;
  if(data_p==NULL) return 4;
  
  switch(attrib) {
  case CARD_CELL:
    
    switch(card_p->type) {
    case CARD_SINGLE:
    case CARD_COMMENT:
    case CARD_LIST:
    case CARD_CELL_LIST:
	case CARD_HEADER:
	case CARD_FREE_FORMAT:
    case CARD_PREREAD:  
      if(ind<0 || ind>=((ff_single_card_t *)card_p)->nb_cells) return 3;
      if((((ff_single_card_t *)card_p)->cell_array)[ind]!=NULL) {
	my_free((((ff_single_card_t *)card_p)->cell_array)[ind]);
      }
      (((ff_single_card_t *)card_p)->cell_array)[ind]=*cell_pp;
	
    default:
      return 2;
      /*break */
    }
    
    break;
  case CARD_CARD:
    if(card_p->type!=CARD_CARD_LIST) return 2;
    if(ind<0 || ind>=((ff_card_list_card_t *)card_p)->nb_cards) return 3;
    if((((ff_card_list_card_t *)card_p)->card_array)[ind]!=NULL) {
      MCDS_delete_ff_card((((ff_card_list_card_t *)card_p)->card_array)[ind]);
      my_free((((ff_card_list_card_t *)card_p)->card_array)[ind]);
    }
    (((ff_card_list_card_t *)card_p)->card_array)[ind]=*card_pp;
    break;
    
  case CARD_COND_CARD_LIST:
    if(card_p->type!=CARD_IF) return 2;
    if(ind<0 || ind>=((ff_if_card_t *)card_p)->nb_condcardlists) return 3;
    if((((ff_if_card_t *)card_p)->condcardlist_array)[ind]!=NULL) {
      MCDS_delete_ff_condcardlist((((ff_if_card_t *)card_p)->condcardlist_array)[ind]);
      my_free((((ff_if_card_t *)card_p)->condcardlist_array)[ind]);
    }
    (((ff_if_card_t *)card_p)->condcardlist_array)[ind]=*a_ccl_pp;
    break;
    
    
  case CARD_OTYPE:
    if(card_p->type!=CARD_OBJECT_LIST) return 2;
    if(ind<0 || ind>=((ff_object_list_card_t *)card_p)->nb_otypes) return 3;
    ((ff_object_list_card_t *)card_p)->otype_tab[ind]=(*a_otype_p);
    break;
  case CARD_FREE_CARD_LIST_TOKEN_STR:
	  if (card_p->type != CARD_CARD_LIST) return 2;
	  if (ind<0 || ind >= ((ff_card_list_card_t *)card_p)->nb_token) return 3;
	  ((ff_card_list_card_t *)card_p)->token_array[ind] = my_strcpy(((ff_card_list_card_t *)card_p)->token_array[ind], (*a_char_p));
	  break;
    
  default:
    return 2;
  }

  return 0;
}

int MCDS_delete_ff_card(ff_card_t *card_p) {
  int i;

  if(card_p==NULL) return 1;
  
  switch(card_p->type) {
  case CARD_BLANK: 
    break;
    
  case CARD_OBJECT_LIST:
    my_free(((ff_object_list_card_t *)card_p)->comment);
    my_free(((ff_object_list_card_t *)card_p)->cell_format);
    my_free(((ff_object_list_card_t *)card_p)->otype_tab);
    /* no break */
  
  case CARD_LIST:       
  case CARD_SINGLE: 
  case CARD_COMMENT:
  case CARD_PREREAD:
  case CARD_HEADER:
  case CARD_FREE_FORMAT:
    for(i=0;i<((ff_single_card_t *)card_p)->nb_cells;i++) {
      
      ff_cell_t *a_cell_pf=((ff_single_card_t *)card_p)->cell_array[i];
      if (!a_cell_pf)
          continue;
      ff_cell_type_e cell_type = a_cell_pf->type;
      MCDS_delete_ff_cell(a_cell_pf);
      if (cell_type != CELL_APPEND_OPTIONS)
      {
          /*already freed in corresponding delete in MCDS_free_app_opt*/
          my_free(a_cell_pf);
      }
      
    }
    my_free(((ff_single_card_t *)card_p)->cell_array);
    break;
  case CARD_CELL_LIST:
      for (i = 0; i < ((ff_single_card_t *)card_p)->nb_cells; i++) {
          
          ff_cell_t *a_cell_pf = ((ff_single_card_t *)card_p)->cell_array[i];
          MCDS_delete_ff_cell(a_cell_pf);
          my_free(a_cell_pf);
          
      }
      my_free(((ff_single_card_t *)card_p)->cell_array);
      my_free(((ff_cell_list_card_t *)card_p)->offset_fmt);
      my_free(((ff_cell_list_card_t *)card_p)->offset_value);
      break;
  case CARD_CARD_LIST: 
    for(i=0;i<((ff_card_list_card_t *)card_p)->nb_cards;i++) {
      MCDS_delete_ff_card(((ff_card_list_card_t *)card_p)->card_array[i]);
      my_free(((ff_card_list_card_t *)card_p)->card_array[i]);
    }
    my_free(((ff_card_list_card_t *)card_p)->card_array);
	for (i = 0; i < ((ff_card_list_card_t *)card_p)->nb_token; i++)
	{
		my_free(((ff_card_list_card_t *)card_p)->token_array[i]);
		((ff_card_list_card_t *)card_p)->token_array[i] = NULL;
	}
	my_free(((ff_card_list_card_t *)card_p)->token_array);
    break;
    
  case CARD_IF:
    for(i=0;i<((ff_if_card_t *)card_p)->nb_condcardlists;i++) {
      MCDS_delete_ff_condcardlist(((ff_if_card_t *)card_p)->condcardlist_array[i]);
      my_free(((ff_if_card_t *)card_p)->condcardlist_array[i]);
    }
    my_free(((ff_if_card_t *)card_p)->condcardlist_array);    
    break;
    
    
  case CARD_SUBOBJECTS:
    my_free(((ff_subobjects_card_t *)card_p)->kfulltype);    
    my_free(((ff_subobjects_card_t*)card_p)->plnkatt);
    my_free(((ff_subobjects_card_t*)card_p)->clnkatt);
    break;
  case CARD_ASSIGN:
  {
      ff_card_assign_header_t* a_header_assign_card_p = (ff_card_assign_header_t*)card_p;
      my_free(a_header_assign_card_p->exp_str);
      assign_operator_e  assign_mode = a_header_assign_card_p->assign_card_type;
      if (assign_mode == ASSIGN_GET_ENTITY_VALUE || assign_mode == ASSIGN_GET_CURRENT_ENTITY)
      {
          my_free(((ff_card_assign_entity_value_t *)card_p)->value_Skey);
          my_free(((ff_card_assign_entity_value_t *)card_p)->objTypeStr);
      }
      else if (assign_mode == ASSIGN_COMBINE || assign_mode == ASSIGN_ERASE || assign_mode == ASSIGN_FIND)
          my_free(((ff_card_assign_string_t*)card_p)->value_str);
  }
  break;
    
  default:
    return 2;
  }

  return 0;
}


/* --------- Conditional list of cards --------- */


int MCDS_new_ff_condcardlist(ff_condcardlist_t **ccl_pp,expression_t *expr_p,int nb_cards) {
  if(ccl_pp==NULL) return 1;

  *ccl_pp               = (ff_condcardlist_t *)my_malloc(1,sizeof(ff_condcardlist_t));
  (*ccl_pp)->expression = expr_p;  
  (*ccl_pp)->nb_cards   = nb_cards;  
  (*ccl_pp)->card_array = (ff_card_t **)my_malloc(nb_cards,sizeof(ff_card_t *));

  return 0;
}



int MCDS_get_ff_condcardlist_expression(const ff_condcardlist_t *ccl_p,expression_t **expr_pp) {
  if(ccl_p==NULL)   return 1;
  if(expr_pp==NULL) return 2;

  *expr_pp=ccl_p->expression;
  
  return 0;
}



int MCDS_get_ff_condcardlist_nb_cards(const ff_condcardlist_t *ccl_p,int *nb_cards_p) {
  if(ccl_p==NULL)      return 1;
  if(nb_cards_p==NULL) return 2;

  *nb_cards_p=ccl_p->nb_cards;
  
  return 0;
}



int MCDS_get_ff_condcardlist_card(const ff_condcardlist_t *ccl_p,int ind,ff_card_t **card_pp) {
  ff_card_t * const *a_card_array=NULL;

  if(ccl_p==NULL)   return 1;
  if(card_pp==NULL) return 3;
  a_card_array=ccl_p->card_array;
  if(a_card_array==NULL || ind<0 || ind>=ccl_p->nb_cards) return 2;

  *card_pp=a_card_array[ind];

  return 0;
}



int MCDS_set_ff_condcardlist_card(ff_condcardlist_t *ccl_p,int ind,ff_card_t *card_p) {
  ff_card_t **a_card_array=NULL;

  if(ccl_p==NULL)  return 1;
  if(card_p==NULL) return 3;
  a_card_array=ccl_p->card_array;
  if(a_card_array==NULL || ind<0 || ind>=ccl_p->nb_cards) return 2;

  a_card_array[ind]=card_p;

  return 0;
}



int MCDS_delete_ff_condcardlist(ff_condcardlist_t *ccl_p) {
  int i;
  if(ccl_p==NULL) return 1;

  MCDS_delete_expression(ccl_p->expression,1); 
  my_free(ccl_p->expression);

  if(ccl_p->card_array!=NULL) {
    for(i=0;i<ccl_p->nb_cards;++i) if(ccl_p->card_array[i]!=NULL) {
      MCDS_delete_ff_card(ccl_p->card_array[i]);
      my_free(ccl_p->card_array[i]);
    }
    my_free(ccl_p->card_array);
  }

  return 0;
}



/* --------- File formats --------- */

int MCDS_new_fileformat(fileformat_t **ff_pfp,int nb_cards) {
  if(ff_pfp==NULL) return 1;
  *ff_pfp=NULL;
  if(nb_cards<0) nb_cards=0;

  *ff_pfp=(fileformat_t *)my_malloc(1,sizeof(fileformat_t));
  
  (*ff_pfp)->nb_cards=nb_cards;
  (*ff_pfp)->card_array=(ff_card_t **)my_malloc(nb_cards,sizeof(ff_card_t *));

  return 0;  
}

int MCDS_get_fileformat_nb_cards(const fileformat_t *ff_p,int *nb_cards_p) {
  if(nb_cards_p==NULL) return 2;
  *nb_cards_p=0;
  if(ff_p==NULL) return 1;

  *nb_cards_p=ff_p->nb_cards;

  return 0;
}

int MCDS_get_fileformat_card(const fileformat_t *ff_p,int ind,ff_card_t **card_pp) {
  if(card_pp==NULL)                return 3;
  *card_pp=NULL;
  if(ff_p==NULL)                   return 1;
  if(ind<0 || ind>=ff_p->nb_cards) return 2;

  *card_pp=ff_p->card_array[ind];

  return 0;
}

int MCDS_set_fileformat_card(fileformat_t *ff_p,int ind,ff_card_t *card_p) {
  if(ff_p==NULL)                   return 1;
  if(ind<0 || ind>=ff_p->nb_cards) return 2;

  if(ff_p->card_array[ind]!=NULL) {
    MCDS_delete_ff_card(ff_p->card_array[ind]);
    my_free(ff_p->card_array[ind]);
  }
  ff_p->card_array[ind]=card_p;

  return 0;
}

int MCDS_delete_fileformat(fileformat_t *ff_p) {
  int i;

  if(ff_p==NULL) return 1;

  for(i=0;i<ff_p->nb_cards;i++) {
    MCDS_delete_ff_card(ff_p->card_array[i]);
    my_free(ff_p->card_array[i]);
  }
  my_free(ff_p->card_array);

  return 0;  
}




