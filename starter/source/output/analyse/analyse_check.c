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
#include <stdlib.h> /* bsearch qsort */

#include "analyse_define.h" /* ANALYSE_CHECK ANALYSE_CHECK_GROUP */
#include "analyse_name.inc" /* AN_CHECK */

#include "analyse.h" /* analyse*/

#include "analyse_print.h" /* Analyse_Print_Error_Level  Analyse_Print_Error */
#include "analyse_memory.h" /* analyse_malloc */
#include "analyse_getall.h" /* analyse_getlist_of_int */
#include "analyse_comment.h" /* analyse_add_comment */
#include "analyse_read_tools.h" /* analyse_read_comment */

#include "analyse_check.h"

static int ANALYSE_NB_CHECK_GROUP=0;
static int ANALYSE_NB_CHECK=0;

static int compar_check_group(const void *group_1_p, const void *group_2_p)
{
  analyse_check_group_t *group_1 = (analyse_check_group_t *)group_1_p;
  analyse_check_group_t *group_2 = (analyse_check_group_t *)group_2_p;

  if ( (group_1->id < 0) &&(group_2->id < 0)) return (group_2->id - group_1->id);
  if (group_1->id < 0) return 1;
  if (group_2->id < 0) return -1;

  return (group_1->id - group_2->id);
}

static int compar_check_group_by_order(const void *group_1_p, const void *group_2_p)
{
  analyse_check_group_t *group_1 = (analyse_check_group_t *)group_1_p;
  analyse_check_group_t *group_2 = (analyse_check_group_t *)group_2_p;

  if ( (group_1->order < 0) &&(group_2->order < 0)) return (group_2->order - group_1->order);
  if (group_1->order < 0) return 1;
  if (group_2->order < 0) return -1;

  return (group_1->order - group_2->order);
}

static int compar_check(const void *check_1_p, const void *check_2_p)
{
  analyse_check_t *check_1 = (analyse_check_t *)check_1_p;
  analyse_check_t *check_2 = (analyse_check_t *)check_2_p;

  if ( (check_1->id < 0) &&(check_2->id < 0)) return (check_2->id - check_1->id);
  if (check_1->id < 0) return 1;
  if (check_2->id < 0) return -1;

  return (check_1->id - check_2->id);
}


static analyse_check_group_t *check_group_create(int nb_check_group)
{
  analyse_check_group_t *check_group;
  int i;

  check_group = (analyse_check_group_t *)analyse_malloc(nb_check_group*sizeof(analyse_check_group_t));

  for(i=0; i<nb_check_group; i++)
    {
      (check_group+i)->order = 0;
      (check_group+i)->id = -i-1;
      (check_group+i)->title = NULL;
      (check_group+i)->nb_check = 0;
      (check_group+i)->check_list = NULL;
      (check_group+i)->check_message = NULL;
    }

  return check_group;
}

static analyse_check_t *check_create(int nb_check)
{
  analyse_check_t *check;
  int i;

  check = (analyse_check_t *)analyse_malloc(nb_check*sizeof(analyse_check_t));
  
  for(i=0; i<nb_check; i++)
    {
      (check+i)->id = -i-1;
      (check+i)->title = NULL;
      (check+i)->check_message = NULL;
    }

  return check;
}


void analyse_check_store(analyse_check_t *check_list, int language, int id)
{
  analyse_comment_t *scan_comment;
  analyse_check_t tmp_check;
  analyse_check_t *work_check;

  char line[ANALYSE_SIZE_OF_LINE];

  /* Search id */
  tmp_check.id = id;
  work_check = bsearch( &tmp_check, check_list, ANALYSE_NB_CHECK, sizeof(analyse_check_t), compar_check);
	  
  if ( work_check == NULL)
    {
      sprintf(line, "Unknown Id for Check %d\nNo Message Available\n", id );
      Analyse_Print_Error_Level(line, 2);
    }

  if ( work_check->check_message != NULL )
    {
      analyse_free(work_check->check_message);
      work_check->check_message = NULL;
    }

  scan_comment = analyse_get_right_comment( work_check->title, language, ANALYSE_ENGLISH);

  work_check->check_message = analyse_fill_description(AN_CHECK, scan_comment->text);

}


/*
  This Function is the equivalent of a part of the Main, in build_default_file.c
*/
void analyse_check_file_read(analyse_check_t **check_p, analyse_check_group_t **check_group_p, char *infilename )
{
  FILE *infile=NULL;
  char line[ANALYSE_SIZE_OF_LINE];
  char key[ANALYSE_SIZE_OF_LINE];
  int i, id;
  int linecount = 0;

  analyse_comment_t *comment;

  analyse_check_t *work_check;
  analyse_check_t tmp_check;

  analyse_check_group_t *work_check_group;
  analyse_check_group_t tmp_check_group;
  
  int count_check_group = 0;
  int check_group_already_print = 0;

  int count_check = 0;
  int check_already_print = 0;

  int nb_check, nb_check_group, size, size2;
  int *tab, *tab2;

  int nb_elt;
  int *elt;

  /*************/
  /* Open File */
  /*************/
  if (infilename != NULL)
    {
      infile = fopen(infilename,"r");
    }

  if (infile == NULL)
    {
      Analyse_Print_Debug("\nNo Check File Found. Use Default\n\n");


      return;
    }


  /***************************/
  /* Allocate Message Memory */
  /***************************/

  /* Count message */
  analyse_count_check(line, infile, infilename, &linecount, &nb_check, &nb_check_group);
  rewind(infile);
  analyse_stack_check(line, infile, infilename, &linecount, &nb_check_group, &size, &tab, &nb_check, &size2, &tab2 );

  ANALYSE_NB_CHECK_GROUP = nb_check_group;
  ANALYSE_NB_CHECK = nb_check;

  analyse_free(tab);
  analyse_free(tab2);

  *check_p = check_create(ANALYSE_NB_CHECK);
  *check_group_p = check_group_create(ANALYSE_NB_CHECK_GROUP);
  

  /****************/
  /* Read Message */
  /****************/
  rewind(infile);
  while ( ( analyse_getline(line, infile, infilename, &linecount) != -1) &&
	  ( strncmp(line, "/END", 4) != 0 ))
    {
      if (strstr(line, "/ANALYSE/CHECK/GROUP") != NULL)					     
	{
	  /* Search Group id */
	  analyse_getkey(4 , line, key);
	  sscanf(key, "%d", &id );  
	  
	  /* Search id */
	  tmp_check_group.id = id;
	  work_check_group = bsearch( &tmp_check_group, *check_group_p, ANALYSE_NB_CHECK_GROUP, sizeof(analyse_check_group_t), compar_check_group);

	  if ( work_check_group == NULL)
	    {
	      if (count_check_group >= ANALYSE_NB_CHECK_GROUP)
		{
		  if ( check_group_already_print == 0)
		    {
		      Analyse_Print_Error_Level("\n\n*** Analyse Internal Error (ANALYSE_NB_CHECK_GROUP) : Too Many Messages \n\n", 2);
		      check_group_already_print = 1;
		    }
		  continue;
		}

	      work_check_group = *check_group_p + count_check_group;

	      work_check_group->order = count_check_group;
	      work_check_group->id = id;
		  
	      /* Note : if file order is in id increase, this qsort does nothing ... */
	      qsort(*check_group_p, ANALYSE_NB_CHECK_GROUP, sizeof(analyse_check_group_t), compar_check_group);		  
	      tmp_check_group.id = id;
	      /* .. and this bsearch gives the same work_group */
	      work_check_group = bsearch( &tmp_check_group, *check_group_p, ANALYSE_NB_CHECK_GROUP, sizeof(analyse_check_group_t), compar_check_group);

	      count_check_group++;
	    }

	  analyse_getkey(5 , line, key);

	  if (strncmp(key, "TITLE", 5) == 0)
	    {
	      /* Read Comment */
	      comment = analyse_read_comment(line, infile, infilename, &linecount);
	      analyse_add_comment(ANALYSE_CHECK_GROUP, (void *)work_check_group, comment);
	    }
	  else if (strncmp(key, "DESCRIPTION", 5) == 0)
	    {
	      /* Read list of ids */
	      if ( analyse_getlist_of_int(&nb_elt, &elt, infile, infilename, &linecount) == 0)
		{
		  work_check_group->check_list = (int *)analyse_realloc(work_check_group->check_list, (work_check_group->nb_check + nb_elt)*sizeof(int));
		  work_check_group->check_message = (char **)analyse_realloc(work_check_group->check_message, (work_check_group->nb_check + nb_elt)*sizeof(char *));
		  
		  for(i=0; i<nb_elt; i++)
		    {
		      work_check_group->check_list[work_check_group->nb_check + i] = elt[i];
		      work_check_group->check_message[work_check_group->nb_check + i] = NULL;
		    }
		  analyse_free(elt);
		  work_check_group->nb_check = work_check_group->nb_check + nb_elt;
		}
	    }
	}
      else if (strstr(line, "/ANALYSE/CHECK") != NULL)					     
	{
	  analyse_getkey(3 , line, key);
	  sscanf(key, "%d", &id );
	  
	  /* Search id */
	  tmp_check.id = id;
	  work_check = bsearch( &tmp_check, *check_p, ANALYSE_NB_CHECK, sizeof(analyse_check_t), compar_check);
	  
	  if ( work_check == NULL)
	    {
	      if (count_check >= ANALYSE_NB_CHECK)
		{
		  if ( check_already_print == 0)
		    {
		      Analyse_Print_Error_Level("\n\n*** Analyse Internal Error (ANALYSE_NB_CHECK) : Too Many Messages \n\n", 2);
		      check_already_print = 1;
		    }
		  continue;
		}
	      
	      work_check = *check_p + count_check;
	      count_check++;

	      work_check->id = id;
		  
	      /* Note : if file order is in id increase, this qsort does nothing ... */
	      qsort(*check_p, ANALYSE_NB_CHECK, sizeof(analyse_check_t), compar_check);		  
	      tmp_check.id = id;
	      /* .. and this bsearch gives the same work_check */
	      work_check = bsearch( &tmp_check, *check_p, ANALYSE_NB_CHECK, sizeof(analyse_check_t), compar_check);

	    }
	  
	  /* Read Comment */
	  comment = analyse_read_comment(line, infile, infilename, &linecount);
	  analyse_add_comment(ANALYSE_CHECK, (void *)work_check, comment);
	  
	}
    }
  
  fclose(infile);

}

void analyse_check_file_write(analyse_check_t *check_list, analyse_check_group_t *check_group_list, int language)
{
  int i,j, nb_check;

  analyse_comment_t *scan_comment;

  analyse_check_group_t *work_check_group;
  
  analyse_check_t tmp_check;
  analyse_check_t *work_check;
  
  /* re order groups by order appearance in file, and no more by id */
  qsort(check_group_list, ANALYSE_NB_CHECK_GROUP, sizeof(analyse_check_group_t), compar_check_group_by_order);		  
	      
  for (i=0; i<ANALYSE_NB_CHECK_GROUP; i++)
    {
      work_check_group = check_group_list+i;
      nb_check = work_check_group->nb_check;

      scan_comment = analyse_get_right_comment( work_check_group->title, language, ANALYSE_ENGLISH);
      
      if ((scan_comment != NULL) && (scan_comment->text != NULL))
	{
	  analyse_write_f("\n\n");
	  analyse_write_f(scan_comment->text);
	  /* analyse_write_f("\n"); */
	}

      for( j=0; j<nb_check; j++)
	{
	  tmp_check.id = work_check_group->check_list[j];
	  work_check = bsearch( &tmp_check, check_list, ANALYSE_NB_CHECK, sizeof(analyse_check_t), compar_check);

	  if ((work_check != NULL) && (work_check->check_message!= NULL))
	    {
	      analyse_write_f(work_check->check_message);
	      /* analyse_write_f("\n"); */
	    }
	}
    }
}

void analyse_check_clean_memory(analyse_check_t *check_list, analyse_check_group_t *check_group_list)
{


}
