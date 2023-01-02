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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "analyse_define.h"/* ANALYSE_SIZE_OF_LINE
			      ANALYSE_ERROR_TITLE ANALYSE_ERROR_DESCRIPTION */

#include "analyse_getall.h" /* analyse_getkey analyse_getline */
#include "analyse_print.h" /* Analyse_Print_Error_Level  Analyse_Print_Error */
#include "analyse_memory.h" /* analyse_malloc analyse_free */
#include "analyse_comment.h" /* analyse_fill_description analyse_get_right_commment */
#include "analyse_read_tools.h" /* analyse_read_comment */

#include "analyse_error.h"

static int ANALYSE_NB_ERROR=0;

static int compar_error(const void *error_1_p, const void *error_2_p)
{
  analyse_error_info_t *error_1 = (analyse_error_info_t *)error_1_p;
  analyse_error_info_t *error_2 = (analyse_error_info_t *)error_2_p;

  if ( (error_1->id < 0) &&(error_2->id < 0)) return (error_2->id - error_1->id);
  if (error_1->id < 0) return 1;
  if (error_2->id < 0) return -1;

  return (error_1->id - error_2->id);
}

static analyse_error_info_t *error_create(int size)
{
  int i=0;
  analyse_error_info_t *error_info;

  error_info = (analyse_error_info_t *)analyse_malloc(size*sizeof(analyse_error_info_t));

  for ( i=0; i<size; i++)
    {
      (error_info+i)->id = -i-1;
      (error_info+i)->global_cnt=0;
      (error_info+i)->tmp_cnt=0;
      (error_info+i)->title = NULL;
      (error_info+i)->description = NULL;
      (error_info+i)->comment = NULL;
    }
  return error_info;

}

/**************************** Cls41l04 +++ ********************************/
void analyse_error_cnt(analyse_error_info_t *error_list, int id)
{
  analyse_error_info_t tmp_error;
  analyse_error_info_t *work_error=NULL;

  /* Search Error */
  tmp_error.id = id;
  work_error = bsearch( &tmp_error, error_list, ANALYSE_NB_ERROR, sizeof(analyse_error_info_t), compar_error);

  if (work_error != NULL)
    {
      work_error->global_cnt++;
      work_error->tmp_cnt++;
    }
}

void analyse_error_set_tmp_cnt(analyse_error_info_t *error_list, int id, int value)
{
  analyse_error_info_t tmp_error;
  analyse_error_info_t *work_error;
  
  /* Search Error */
  tmp_error.id = id;
  work_error = bsearch( &tmp_error, error_list, ANALYSE_NB_ERROR, sizeof(analyse_error_info_t), compar_error);

  if (work_error != NULL)
    {
      work_error->tmp_cnt=value;
    }
}

void analyse_error_get_cnt(analyse_error_info_t *error_list, int id, int *global_cnt, int *tmp_cnt)
{
  analyse_error_info_t tmp_error;
  analyse_error_info_t *work_error;

  /* Search Error */
  tmp_error.id = id;
  work_error = bsearch( &tmp_error, error_list, ANALYSE_NB_ERROR, sizeof(analyse_error_info_t), compar_error);

  if (work_error != NULL)
    {
      if ( global_cnt != NULL)
	{
	  *global_cnt = work_error->global_cnt;
	}

      if ( tmp_cnt != NULL)
	{
	  *tmp_cnt = work_error->tmp_cnt;
	}
    }
}
/**************************** Cls41l04 --- ********************************/
 
void analyse_error_return_message(analyse_error_info_t *error_list, int error_warning_type, 
				  int language, int id, char **title, char **description, char **comment)
{
  analyse_comment_t *scan_comment;
  analyse_error_info_t *work_error;
  analyse_error_info_t tmp_error;
  char line[ANALYSE_SIZE_OF_LINE];


  /* Search Error */
  tmp_error.id = id;
  work_error = bsearch( &tmp_error, error_list, ANALYSE_NB_ERROR, sizeof(analyse_error_info_t), compar_error);
  
  if (work_error == NULL)
    {
      sprintf(line, "Unknown Id for Error %d\nNo Message Available\n", id );
      Analyse_Print_Error_Level(line, 2);
      
      if ( title != NULL ) *title = NULL;   
      if ( description != NULL ) *description = NULL;     
      if ( comment != NULL ) *comment = NULL;

      return;
    }
  else
    {
      /* Get Title */
      if ( title != NULL )
	{
	  if (work_error->title== NULL)
	    {
	      *title = NULL;
	    }
	  else
	    {
	      scan_comment = analyse_get_right_comment(work_error->title,language,ANALYSE_ENGLISH);
          #ifdef _WIN64
          *title = _strdup(scan_comment->text);
          #else
          *title = strdup(scan_comment->text);
          #endif
	    }
	}

      /* Get Description */
      if ( description != NULL )
	{
	  if (work_error->description== NULL)
	    {
	      *description = NULL;
	    }
	  else
	    {
	      scan_comment = analyse_get_right_comment(work_error->description,language,ANALYSE_ENGLISH);
	      *description = analyse_fill_description(error_warning_type, scan_comment->text);
	    }
	}

      /* Get Comment */
      if ( comment != NULL )
	{
	  if (work_error->comment== NULL)
	    {
	      *comment = NULL;
	    }
	  else
	    {
	      scan_comment = analyse_get_right_comment(work_error->comment,language,ANALYSE_ENGLISH);
          #ifdef _WIN64
          *comment = _strdup(scan_comment->text);
          #else
          *comment = strdup(scan_comment->text);
          #endif
	    }
	}
    }
}

/*
  This Function is the equivalent of a part of the Main, in build_default_file.c
*/
void analyse_error_file_read(analyse_error_info_t **error_p, char *infilename)
{
/* ls41k10 */
  FILE *infile=NULL;

  char line[ANALYSE_SIZE_OF_LINE];
  char key[ANALYSE_SIZE_OF_LINE];
  int id;
  int linecount = 0;

  analyse_comment_t *comment;
  analyse_error_info_t *work_error;
  analyse_error_info_t tmp_error;

  int new_count = 0;
  int already_print = 0;

  int cnt, size;
  int *tab;


  /*************/
  /* Open File */
  /*************/
/* ls41k10+++ */
  /* security, in case fopen crashes with error_filename = NULL */
  if (infilename != NULL)
    {
      infile = fopen(infilename,"r");
    }
/* ls41k10 --- */

  if (infile == NULL)
    {
      /*
      Analyse_Print_Error_Level("*** Analyse Read Error : Unable to read Error Definition File :\n", 2);
      Analyse_Print_Error(infilename);
      return;
      */

      Analyse_Print_Debug(" \nNo Error File Found. Use Default\n");


      return;
    }


  /***************************/
  /* Allocate Message Memory */
  /***************************/

  /* Count message */
  analyse_count_error(line, infile, infilename, &linecount, &cnt);
  rewind(infile);
  analyse_stack_error(line, infile, infilename, &linecount, &cnt, &size, &tab);

  ANALYSE_NB_ERROR = cnt;
  analyse_free(tab);

  *error_p = error_create(ANALYSE_NB_ERROR);
  

  /****************/
  /* Read Message */
  /****************/
  rewind(infile);
  while ( ( analyse_getline(line, infile, infilename, &linecount) != -1) &&
	  ( strncmp(line, "/END", 4) != 0 ))
    {
      
      /* ls41l33     if (strstr(line, "/ANALYSE/ERROR/") != NULL)	*/
      if (strstr(line, "/ANALYSE/MESSAGE/") != NULL)
	{
	  /* Search Error */
	  analyse_getkey(3 , line, key);
	  sscanf(key, "%d", &id );  

	  tmp_error.id = id;
	  work_error = bsearch( &tmp_error, *error_p, ANALYSE_NB_ERROR, sizeof(analyse_error_info_t), compar_error);
	  
	  if (work_error == NULL)
	    {
	      if (new_count >= ANALYSE_NB_ERROR) 
		{
		  if ( already_print == 0)
		    {
		      Analyse_Print_Error_Level("\n\n*** Analyse Internal Error (ANALYSE_NB_ERROR) : Too Many Messages \n\n", 2);
		      already_print = 1;
		    }
		  continue;
		} 
	       
	      work_error =  *error_p + new_count;
	      new_count++;

	      work_error->id = id;

	      /* Note : if file order is in id increase, this qsort does nothing ... */
	      qsort(*error_p, ANALYSE_NB_ERROR, sizeof(analyse_error_info_t), compar_error);		  
	      tmp_error.id = id;
	      /* .. and this bsearch gives the same work_error */
	      work_error = bsearch( &tmp_error, *error_p, ANALYSE_NB_ERROR, sizeof(analyse_error_info_t), compar_error);
	      
	    }

	  analyse_getkey(4 , line, key);
	  if ( (strncmp(key, "TITLE", 5) == 0) ||
	       (strncmp(key, "DESCRIPTION", 11) == 0) )
	    {
	      /* Read Comment */
	      comment = analyse_read_comment(line, infile, infilename, &linecount);

	      /* Add Comment */
	      if (strncmp(key, "TITLE", 5)== 0) 
		{
		  analyse_add_comment(ANALYSE_ERROR_TITLE, work_error, comment);
		}
	      else if (strncmp(key, "DESCRIPTION", 11)== 0) 
		{
		  analyse_add_comment(ANALYSE_ERROR_DESCRIPTION, work_error, comment);	      
		}
	    }
	}
    }

  fclose(infile);
}
