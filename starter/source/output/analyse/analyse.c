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
/*********************************************************************
 *                        INCLUDES
 *********************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <signal.h>


/*********************************************************************
 *                        PORTAGE
 *********************************************************************/
#include "analyse_portage.h"

/********************************************************************/
#include "analyse_name.inc"
#include "analyse_define.h"

#include "analyse_print.h"
#include "analyse_memory.h"
#include "analyse_getall.h"
#include "analyse_string.h"
#include "analyse_fill_info.h"
#include "analyse_error.h"
#include "analyse_check.h" /* analyse_check_file_read */ 
#include "analyse_GUI.h"

#include "analyse_structure.h"

#include "analyse.h"

#define SIZE_STEP 1000

static int LANGUAGE;

static int *int_stack=NULL;
static int int_stack_size=0;
static int int_stack_current=0;
static int int_stack_scan=0;

static float *float_stack=NULL;
static int float_stack_size=0;
static int float_stack_current=0;
static int float_stack_scan=0;

analyse_node_t *analyse_tree=NULL;

static analyse_node_t *current_node=NULL;
static analyse_node_t *last_node_in_list=NULL;
static analyse_node_t *first_free_in_list=NULL;

static analyse_error_info_t *analyse_error_list=NULL;

static analyse_check_group_t *analyse_check_group_list=NULL;
static analyse_check_t *analyse_check_list=NULL;


static  analyse_node_t *current_node_child_sav = NULL;
static  analyse_node_t *node_scan_sav = NULL ;

/*********************************************************************
 *
 * LOCAL PROTOTYPING
 *
 ********************************************************************/
static analyse_node_t *search_for_node_with_id(int name_id);
static void store_info_in_node_list(analyse_info_t *analyse_info);

static void remove_node_data(analyse_node_t *node_scan);
static void remove_node_structure(analyse_node_t *node_scan);

static void remove_all_in_node_recursiv(analyse_node_t *node_scan);
static void remove_data_in_node_recursiv(analyse_node_t *node_scan);

static void manage_Core (int s);

static void analyse_tree_enter(int name_id);
static void analyse_tree_close(int name_id);
static void analyse_tree_exit(int name_id);
static void analyse_init(int prgrm, int GUI_mode, int prgrm_language);



/*********************************************************************
 *********************************************************************
 *
 *                       LOCAL FUNCTIONS
 *
 *********************************************************************
 *********************************************************************/
static analyse_node_t *search_for_node_with_id(int name_id)
{
  analyse_node_t *node_scan;

  node_scan = current_node;

  while ( node_scan->info.calling_id != name_id)
    {
      if (node_scan->parent == NULL)
	return NULL;

      node_scan = node_scan->parent;
    }

  return node_scan;
}

static void store_info_in_node_list(analyse_info_t *analyse_info)
{   
  if ( first_free_in_list != NULL)
    {
      first_free_in_list->info = *analyse_info;
      
      first_free_in_list->parent = NULL;
      first_free_in_list->child = NULL;
      first_free_in_list->next = NULL;
      first_free_in_list->prev = NULL;

      last_node_in_list = first_free_in_list;
      first_free_in_list = first_free_in_list->list_next;
    }
  else
    {
      last_node_in_list->list_next = (analyse_node_t *)analyse_malloc(sizeof(analyse_node_t));
      
      analyse_node_init(last_node_in_list->list_next);

      last_node_in_list->list_next->list_prev = last_node_in_list;      
      last_node_in_list->list_next->info = *analyse_info;
      
      last_node_in_list=last_node_in_list->list_next;
    }
}


static void remove_node_data(analyse_node_t *node_scan)
{
  int nb_to_move, i;

  /* Free float datas */
  if (node_scan->info.nb_float_data != 0)
    {
      nb_to_move = float_stack_scan - (node_scan->info.float_data_offset_start + node_scan->info.nb_float_data);
      
      for(i=0; i <nb_to_move; i++)
	{
	  float_stack[node_scan->info.float_data_offset_start+i] = float_stack[node_scan->info.float_data_offset_start + node_scan->info.nb_float_data+i];
	}
      
      float_stack_current = float_stack_current - node_scan->info.nb_float_data;
      float_stack_scan = float_stack_scan - node_scan->info.nb_float_data;

      node_scan->info.nb_float_data=0;
    }
  
  /* Free int datas */
  if (node_scan->info.nb_int_data != 0)
    {
      nb_to_move = int_stack_scan - (node_scan->info.int_data_offset_start + node_scan->info.nb_int_data);
      
      for(i=0; i <nb_to_move; i++)
	{
	  int_stack[node_scan->info.int_data_offset_start+i] = int_stack[node_scan->info.int_data_offset_start + node_scan->info.nb_int_data+i];
	}
            
      int_stack_current = int_stack_current - node_scan->info.nb_int_data;
      int_stack_scan = int_stack_scan - node_scan->info.nb_int_data;

      node_scan->info.nb_int_data=0;
    }

}


static void remove_node_structure(analyse_node_t *node_scan)
{
  /* Free structure in list */
  if (node_scan == last_node_in_list)
    {
      last_node_in_list = last_node_in_list->list_prev;     
    }
  else
    {
      if (node_scan->list_prev != NULL)
	node_scan->list_prev->list_next = node_scan->list_next;
      
      if (node_scan->list_next != NULL)
	node_scan->list_next->list_prev = node_scan->list_prev;
      
      node_scan->list_prev = last_node_in_list;
      node_scan->list_next = last_node_in_list->list_next;

      last_node_in_list->list_next = node_scan;  
    }

  first_free_in_list = node_scan;
 
}


static void remove_all_in_node_recursiv(analyse_node_t *node_scan)
{
  if (node_scan == NULL)
    return;

  remove_all_in_node_recursiv(node_scan->next);
  node_scan->next=NULL;

  remove_all_in_node_recursiv(node_scan->child);
  node_scan->child = NULL;

  remove_node_data(node_scan);
  remove_node_structure(node_scan);

  if (node_scan->prev != NULL)
    node_scan->prev->next=NULL;
}

static void remove_data_in_node_recursiv(analyse_node_t *node_scan)
{
  if (node_scan == NULL)
    return;

  remove_data_in_node_recursiv(node_scan->next);
  remove_data_in_node_recursiv(node_scan->child);

  remove_node_data(node_scan);
}


/************************/
static void manage_Core (int s)
{
  int nb_int;
  int *tab_int=NULL;

  char stop_message[]="\n\n %s STOPPED due to CORE DUMPED";
  char *message;

/*  setIgnoreCore (0); */
  
  /* Create Message */
  message = (char *)analyse_malloc((strlen(analyse_tree->info.calling_name)+strlen(stop_message)+1)*sizeof(char));
  sprintf(message,stop_message,analyse_tree->info.calling_name);


  /* Print Message On SCREEN and IN L00 FILE */
  analyse_convert_string_to_int(message, &nb_int, &tab_int);

  wistdo(&nb_int, tab_int);
  wiout(&nb_int, tab_int);
  
  analyse_free(tab_int);

  /* start GUI */
  analyse_GUI_add_error_message(message);
  analyse_free(message);

  analyse_GUI_start_loop();

  /* quit Program */
  analyse_quit();

}



/*
  analyse_stack_float
*/
void analyse_stack_float(float r)
{
  if (float_stack_scan == float_stack_size)
    {
      float_stack = (float *)analyse_realloc( (void *)float_stack, (float_stack_size + SIZE_STEP)*sizeof(float));
      
      float_stack_size = float_stack_size + SIZE_STEP;
    }

  float_stack[float_stack_scan] = r;
  float_stack_scan ++;

  
}

void anstckf(float *r)
{
  analyse_stack_float(*r);
}

void anstckf_(float *r)
{
  analyse_stack_float(*r);
}

void anstckf__(float *r)
{
  analyse_stack_float(*r);
}

void _FCALL ANSTCKF(float *r)
{
  analyse_stack_float(*r);
}



/*
  analyse_stack_int
*/
void analyse_stack_int(int i)
{
  if (int_stack_scan == int_stack_size)
    {
      int_stack = (int *)analyse_realloc( (void *)int_stack, (int_stack_size + SIZE_STEP)*sizeof(int));
      
      int_stack_size = int_stack_size + SIZE_STEP;
    }
  
  int_stack[int_stack_scan] = i;
  int_stack_scan ++;

}

void anstcki(int *i)
{
  analyse_stack_int(*i);
}

void anstcki_(int *i)
{
  analyse_stack_int(*i);
}

void _FCALL ANSTCKI(int *i)
{
  analyse_stack_int(*i);
}


int analyse_get_datas(int name_id, int *nb_int, int **tab_int, int *nb_float, float **tab_float)
{

  analyse_node_t *node_scan;

  node_scan=search_for_node_with_id(name_id);

  if (node_scan == NULL)
    {
      if (nb_int!= NULL) *nb_int=0;
      if (tab_int !=NULL) *tab_int=NULL;

      if (nb_float != NULL) *nb_float=0;      
      if (tab_float != NULL) *tab_float=NULL;

      return -1;
    }

  if (nb_int != NULL) *nb_int=node_scan->info.nb_int_data;
  if (tab_int != NULL) *tab_int = int_stack + node_scan->info.int_data_offset_start;

  if (nb_float != NULL) *nb_float=node_scan->info.nb_float_data;
  if (tab_float != NULL) *tab_float = float_stack + node_scan->info.float_data_offset_start;

  return 0;
}


/*
  analyse_tree_enter
*/
static void analyse_tree_enter(int name_id)
{
  analyse_node_t *node_scan;
  analyse_info_t analyse_info;


  analyse_info_init(&analyse_info);
  analyse_info.calling_id = name_id;
  analyse_fill_info(&analyse_info);

#ifdef ANALYSE_DEBUG 
  printf(" Enter analyse_tree_enter with %s\n", analyse_info.calling_name);
#endif

  analyse_info.nb_float_data = float_stack_scan - float_stack_current;
  analyse_info.float_data_offset_start = float_stack_current;
  
  float_stack_current = float_stack_scan;

  analyse_info.nb_int_data = int_stack_scan - int_stack_current;
  analyse_info.int_data_offset_start = int_stack_current;

  int_stack_current = int_stack_scan;

  /* transfer info to a node which is necessary last_node_in_list */
  store_info_in_node_list(&analyse_info);

  last_node_in_list->parent = current_node;

  if (current_node->child == NULL)
    {
      current_node->child = last_node_in_list;
    }
  else
    {
 
      if (current_node->child == current_node_child_sav) {
        node_scan = node_scan_sav;
      } else {
        current_node_child_sav = current_node->child;
        node_scan = current_node->child;
      }
      while (node_scan->next != NULL) node_scan = node_scan->next;
      node_scan_sav = node_scan;
      
      node_scan->next = last_node_in_list;
      last_node_in_list->prev = node_scan;
    }
   	  
  current_node = last_node_in_list;

  analyse_GUI_tree_rebuild();

}

void anenter(int *name_id)
{
  analyse_tree_enter(*name_id);
}

void anenter_(int *name_id)
{
  analyse_tree_enter(*name_id);
}

void _FCALL ANENTER(int *name_id)
{
 analyse_tree_enter(*name_id);
}





/*
  analyse_tree_close
*/
static void analyse_tree_close(int name_id)
{
  analyse_node_t *node_scan;
  char tline[180];

  node_scan=search_for_node_with_id(name_id);

  if (node_scan == NULL)
    {
      sprintf(tline, "Unable to close Function id %d", name_id);
      Analyse_Print_Error_Level(tline, 2);
      return;
    }
 
  remove_data_in_node_recursiv(node_scan->child);

  remove_node_data(node_scan);

  current_node = node_scan->parent;

  analyse_GUI_tree_rebuild();
}

void anclose(int *name_id)
{
  analyse_tree_close(*name_id);
}

void anclose_(int *name_id)
{
  analyse_tree_close(*name_id);
}

void anclose__(int *name_id)
{
  analyse_tree_close(*name_id);
}
void _FCALL ANCLOSE(int *name_id)
{
 analyse_tree_close(*name_id);
}




/*
  analyse_tree_exit
*/
static void analyse_tree_exit(int name_id)
{
  analyse_node_t *node_scan;
  char tline[180];

  node_scan=search_for_node_with_id(name_id);

  if (node_scan == NULL)
    {
      sprintf(tline, "Unable to close Function id %i", name_id);
      Analyse_Print_Error_Level(tline, 2);
      return;
    }
    
  remove_all_in_node_recursiv(node_scan->child);

  remove_node_data(node_scan);
  remove_node_structure(node_scan);


  if (node_scan->prev != NULL)
    node_scan->prev->next = node_scan->next;

  if (node_scan->next != NULL)
     node_scan->next->prev = node_scan->prev;

  /* if first node, then transfer childness to next one, even if null */
  if (node_scan->prev == NULL)
    node_scan->parent->child = node_scan->next;

  current_node = node_scan->parent;
  
  analyse_GUI_tree_rebuild();

}

void anexit(int *name_id)
{
  analyse_tree_exit(*name_id);
}

void anexit_(int *name_id)
{
  analyse_tree_exit(*name_id);
}

void anexit__(int *name_id)
{
  analyse_tree_exit(*name_id);
}
void _FCALL ANEXIT(int *name_id)
{
 analyse_tree_exit(*name_id);
}



/*
  analyse_call_error
*/
void analyse_call_error(int type, int id, int mode)
{
  int nb_int;
  int *tab_int=NULL;

  int nb_int_line;
  int *tab_int_line=NULL;

  char *title;
  char *description;
  
  int nb_err, nb_warn;

  char stop_message[]="\n\n %s STOPPED due to INPUT ERROR";
  char gene_message[]="\n******************************\n%s stopped with :\n    %d Error(s)\n    %d Warning(s)\n******************************\n";
  
  char error_id_message[]="ERROR id : %d";
  char warning_id_message[]="WARNING id : %d";
  char info_id_message[]="INFO id : %d";
  char unknown_id_message[]="??? id : %d";

  char id_message_real[20];
  char *message;

  /* Get tab for a new line */
  analyse_convert_string_to_int("\n", &nb_int_line, &tab_int_line);


  /* ADD ERROR in IERR */
  if (type == AN_ERROR)
    {
      anaderr();
      analyse_tree_enter(AN_ERROR);
      analyse_error_return_message(analyse_error_list, AN_ERROR, LANGUAGE, id, &title, &description, NULL);
    }
  else if (type == AN_WARNING)
    {
      anadwar();
      analyse_tree_enter(AN_WARNING);
      analyse_error_return_message(analyse_error_list, AN_WARNING, LANGUAGE, id, &title, &description, NULL);
    }
  else
    {
      analyse_tree_enter(AN_INFO);
      analyse_error_return_message(analyse_error_list, AN_INFO, LANGUAGE, id, &title, &description, NULL);
    }

  /* Count Error */
  analyse_error_cnt(analyse_error_list, id);

  /* Print Error Id + Title */
  if (type == AN_ERROR)
    sprintf(id_message_real, error_id_message, id);
  else if (type == AN_WARNING)
    sprintf(id_message_real, warning_id_message, id);
  else if (type == AN_INFO)
    sprintf(id_message_real, info_id_message, id);
  else
    sprintf(id_message_real, unknown_id_message, id);

   analyse_GUI_add_error_message(id_message_real);
   analyse_GUI_add_error_message(title);

  switch(mode)
    {
    case ANSTOP:
    case ANINFO: /* title+description on screen, title+description in file */
    case ANINFO_BLIND_1: /* only title on screen, title + description in file */
      /* Id */
      analyse_convert_string_to_int(id_message_real, &nb_int, &tab_int);

      wiout(&nb_int_line, tab_int_line);
      /* wistdo(&nb_int_line, tab_int_line); */

      wiout(&nb_int, tab_int);
      if (mode == ANINFO ) 
      wistdo(&nb_int, tab_int);
      
      analyse_free(tab_int);

      /* Title */
      analyse_convert_string_to_int(title, &nb_int, &tab_int);
      wistdo(&nb_int, tab_int);
      wiout(&nb_int, tab_int);
 
      analyse_free(tab_int);
      break;
      
    case ANINFO_BLIND_2: /* nothing on screen, title + description in file */
    case ANINFO_BLIND_3: /* nothing on screen, title in file */

      /* Id */
      analyse_convert_string_to_int(id_message_real, &nb_int, &tab_int);

      wiout(&nb_int_line, tab_int_line);
      wiout(&nb_int, tab_int);

      analyse_free(tab_int);

      /* Title */
      analyse_convert_string_to_int(title, &nb_int, &tab_int);
      wiout(&nb_int, tab_int);
 
      analyse_free(tab_int);
      break;


    case ANINFO_BLIND_4: /* nothing on screen, nothing in file */
    default :
      break;
    }
  analyse_free(title);


  /* Print Error Description */
  analyse_GUI_add_error_message(description);  

  switch(mode)
    {
    case ANSTOP:
    case ANINFO: /* title+description on screen, title+description in file */     
      analyse_convert_string_to_int(description, &nb_int, &tab_int);

      wiout(&nb_int, tab_int);
      wistdo(&nb_int, tab_int);

      wiout(&nb_int_line, tab_int_line);
      /* wistdo(&nb_int_line, tab_int_line); */

      analyse_free(tab_int);
      break;

    case ANINFO_BLIND_1: /* only title on screen, title + description in file */
      analyse_convert_string_to_int(description, &nb_int, &tab_int);
      
      wiout(&nb_int, tab_int);

      wiout(&nb_int_line, tab_int_line);
      /* wistdo(&nb_int_line, tab_int_line); */

      analyse_free(tab_int);
      break;

    case ANINFO_BLIND_2: /* nothing on screen, title + description in file */
      analyse_convert_string_to_int(description, &nb_int, &tab_int);

      wiout(&nb_int, tab_int);

      wiout(&nb_int_line, tab_int_line);

      analyse_free(tab_int);
      break;

    case ANINFO_BLIND_3: /* nothing on screen, title in file */
      wiout(&nb_int_line, tab_int_line);
      break;

    case ANINFO_BLIND_4: /* nothing on screen, nothing in file */
    default :
      break;
    }
  analyse_free(description);
  

  /* Error Management */
  switch(mode)
    {
    case ANSTOP:
      /* Stopped due to Input Errors */
      message = (char *)analyse_malloc((strlen(analyse_tree->info.calling_name)+strlen(stop_message)+1)*sizeof(char));
      sprintf(message,stop_message,analyse_tree->info.calling_name);

      /* Print Stop Message On SCREEN, IN L00 FILE and in GUI*/
      analyse_convert_string_to_int(message, &nb_int, &tab_int);

      wistdo(&nb_int, tab_int);
      wiout(&nb_int, tab_int);
  
      analyse_free(tab_int);

      analyse_GUI_add_error_message(message);
      analyse_free(message);

      /* Prepare GUI Message and start GUI*/
      angetnb(&nb_err, &nb_warn);
      message = (char *)analyse_malloc((strlen(analyse_tree->info.calling_name)+strlen(gene_message)+ 10+10+1)*sizeof(char));
      sprintf(message,gene_message,analyse_tree->info.calling_name, nb_err, nb_warn);

      analyse_GUI_add_error_message(message);
      analyse_free(message);
         
      analyse_GUI_start_loop();
  
      /* quit Program */
      analyse_quit();
      break;
      
    case ANINFO:
    case ANINFO_BLIND_1:
    case ANINFO_BLIND_2:
    case ANINFO_BLIND_3:
    case ANINFO_BLIND_4:
      if (type == AN_ERROR)
    analyse_tree_close(AN_ERROR);
      else if (type == AN_WARNING)
    analyse_tree_close(AN_WARNING);
      else
    analyse_tree_close(AN_INFO);
      break;
      
    default :
      break;
    }
  
  analyse_free(tab_int_line);
}


/*
  Check
*/

void analyse_call_check(int id)
{
  if (id == -1)
    {
      analyse_check_file_write(analyse_check_list, analyse_check_group_list, LANGUAGE);
    }
  else
    {
      analyse_tree_enter(AN_CHECK); 
      analyse_check_store(analyse_check_list, LANGUAGE, id);
      analyse_tree_close(AN_CHECK);
    }
}


void ancheck(int *id)
{
  analyse_call_check(*id);
}

void ancheck_(int *id)
{
  analyse_call_check(*id);
}

void ancheck__(int *id)
{
  analyse_call_check(*id);
}

void _FCALL ANCHECK(int *id)
{
  analyse_call_check(*id);
}


/*
  analyse_init
*/
static void analyse_init(int prgrm, int GUI_mode, int prgrm_language)
{
  char *filename;
  char *path;
  char *tmp;
  char *env;
  char *var;
  int len_path;

  char *starter_filename="$RADIR/rad_err/starter_message_description.txt";
  char *radioss_filename="$RADIR/rad_err/radioss_message_description.txt";
  char *program_filename="$RADIR/rad_err/program_message_description.txt";

  int length;

  analyse_tree = (analyse_node_t *)analyse_malloc(sizeof(analyse_node_t));

  analyse_node_init(analyse_tree);

  LANGUAGE = prgrm_language;

  if (prgrm == AN_STARTER)
    {
      #ifdef _WIN64
        analyse_tree->info.calling_name=_strdup("Starter");
      #else
        analyse_tree->info.calling_name=strdup("Starter");
      #endif
      filename = starter_filename;
    }
  else if(prgrm == AN_RADIOSS)
    {
      #ifdef _WIN64
        analyse_tree->info.calling_name=_strdup("Radioss");
      #else
        analyse_tree->info.calling_name=strdup("Radioss");
      #endif
       filename = radioss_filename;
    }
  else 
    {
      #ifdef _WIN64
      analyse_tree->info.calling_name=_strdup("Program");
      #else
      analyse_tree->info.calling_name=strdup("Program");
      #endif
      filename = program_filename;
    }

  if (filename[0]=='$')
    {
      tmp=strchr(filename,'/');
      length = (int)(tmp-filename);
      var = (char*)analyse_malloc(length*sizeof(char));
      #ifdef _WIN64
      strncpy_s(var,length, filename+1, length-1);
      #else
      strncpy(var, filename+1, length-1);
      #endif
      var[length-1]='\0';

      env = getenv(var);
      analyse_free(var);

      if (env == NULL)
    {
    path = NULL;
    }
      else
    {
      len_path = (strlen(env)+strlen(tmp)+1)*sizeof(char);
      path = (char*)analyse_malloc(len_path);
      #ifdef _WIN64
      strcpy_s(path,len_path,"");
      strcat_s(path,len_path,env);
      strcat_s(path,len_path,tmp);
      #else
      strcpy(path,"");
      strcat(path,env);
      strcat(path,tmp);
      #endif
    }
    }
  else
    {
      #ifdef _WIN64
      path = _strdup(filename);
      #else
      path = strdup(filename);
      #endif
    }


  current_node=analyse_tree;
  last_node_in_list=analyse_tree;
  
  analyse_error_file_read(&analyse_error_list, path);
  analyse_free(path);

  analyse_GUI_window_create(GUI_mode);  

/*  setIgnoreCore (1); */
}

void aninit(int *prgrm, int *GUI_mode)
{
  analyse_init(*prgrm, *GUI_mode, ANALYSE_ENGLISH);
}

void aninit_(int *prgrm, int *GUI_mode)
{
  analyse_init(*prgrm, *GUI_mode, ANALYSE_ENGLISH);
}

void aninit__(int *prgrm, int *GUI_mode)
{
  analyse_init(*prgrm, *GUI_mode, ANALYSE_ENGLISH);
}

void _FCALL ANINIT(int *prgrm, int *GUI_mode)
{
  analyse_init(*prgrm, *GUI_mode, ANALYSE_ENGLISH);
}


/*
  analyse_quit
*/
void analyse_quit( void )
{
  anend();
}

/**************************** Cls41l04 +++ ********************************/
/* 
   count Messages 
   -> analyse_count_set
   -> analyse_count_get 
*/
void ancnts(int *id, int *value)
{
  analyse_error_set_tmp_cnt(analyse_error_list, *id, *value);
}

void ancnts_(int *id, int *value)
{
  analyse_error_set_tmp_cnt(analyse_error_list, *id, *value);
}

void ancnts__(int *id, int *value)
{
  analyse_error_set_tmp_cnt(analyse_error_list, *id, *value);
}

void _FCALL ANCNTS(int *id, int *value)
{
  analyse_error_set_tmp_cnt(analyse_error_list, *id, *value);
}


void ancntg(int *id, int *global_cnt, int *tmp_cnt)
{
  analyse_error_get_cnt(analyse_error_list, *id, global_cnt, tmp_cnt);
} 

void ancntg_(int *id, int *global_cnt, int *tmp_cnt)
{
  analyse_error_get_cnt(analyse_error_list, *id, global_cnt, tmp_cnt);
} 

void ancntg__(int *id, int *global_cnt, int *tmp_cnt)
{
  analyse_error_get_cnt(analyse_error_list, *id, global_cnt, tmp_cnt);
} 

void _FCALL ANCNTG(int *id, int *global_cnt, int *tmp_cnt)
{
  analyse_error_get_cnt(analyse_error_list, *id, global_cnt, tmp_cnt);
}
/**************************** Cls41l04 --- ********************************/
/**************************** Cls41l31 +++ ********************************/
void analyse_write_f(char *message)
{
  int nb_int;
  int *tab_int=NULL;

  analyse_convert_string_to_int(message, &nb_int, &tab_int);

//  wif(&nb_int, tab_int);

  analyse_free(tab_int);
}
/**************************** Cls41l31 --- ********************************/
/* lm50c18 +++ */

void qaclose_();

void my_exit (int *i)
{ 
  qaclose_();
  exit(*i); 
}

void my_exit_ (int *i)
{ my_exit(i); }

void my_exit__ (int *i)
{ my_exit(i); }

void _FCALL MY_EXIT (int *i)
{ my_exit(i); }

/* lm50c18 --- */
/************************************************************************/


