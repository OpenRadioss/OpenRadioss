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
#include <string.h>
#include <stdio.h> /* sscanf */
#include <stdlib.h> /* qsort */

#include "analyse_define.h" /* ANALYSE_SIZE_OF_LINE */ 

#include "analyse_memory.h" /* analyse_malloc */
#include "analyse_getall.h" /* analyse_getline analyse_getkey */
#include "analyse_string.h" /* analyse_string_fit_start_end */

#include "analyse_read_tools.h"

static int compar_int(const void *int_1_p, const void *int_2_p)
{
  int *int_1 = (int *)int_1_p;
  int *int_2 = (int *)int_2_p;

 return ( *int_1 - *int_2);
}

static int count_ids( int size, int *tab)
{
  int keep_id, i, cnt;

  keep_id = -1;
  cnt = 0;
  
  if (tab == NULL) return cnt;
  
  for(i=0; i<size; i++)
    {
      if (tab[i] != keep_id)
	{
	  cnt++;
	  keep_id = tab[i];
	}
    } 
  
  return cnt;
}


int analyse_find_id( int size, int *tab, int id)
{
  int keep_id, i, cnt;
  
  keep_id = -1;
  cnt = 0;
  
  for(i=0; i<size; i++)
    {
      if (tab[i] != keep_id)
	{
	  cnt++;
	  keep_id = tab[i];
	}
      
      if (tab[i] == id)
	break;
    }
  
  return cnt;
}

/* lm41n7 +++ */
char *analyse_read_text_default(char *line, FILE *infile, char *filename, int *linecount_p)
{
  int i;
  int nb_line=0;
  int nb_dataline=0;
  char *text;

  analyse_getsize_of_enum (&nb_line, &nb_dataline, infile, filename, linecount_p);
 
  text = (char *)analyse_malloc((nb_dataline*(ANALYSE_SIZE_OF_LINE+1))*sizeof(char));
  strcpy(text, "");
 
  for (i=0; i<nb_dataline; i++)
    {
      if ( analyse_getline(line, infile, filename, linecount_p) == -1)
        {
          analyse_free(text);
          return NULL;
        }

      analyse_string_fit_start_end(line);
      if (strlen(line) > 0)
        {
          strcat(text, line);
          strcat(text, "\\n");
        }
    }

  return text;
}
/* lm41n7 --- */


char *analyse_read_text(char *line, FILE *infile, char *filename, int *linecount_p)
{
  int i;
  int nb_line=0;
  int nb_dataline=0;
  char *text;

  analyse_getsize_of_enum (&nb_line, &nb_dataline, infile, filename, linecount_p);
  
  text = (char *)analyse_malloc((nb_dataline*(ANALYSE_SIZE_OF_LINE+1))*sizeof(char));
  strcpy(text, "");
  
  for (i=0; i<nb_dataline; i++)
    {
      if ( analyse_getline(line, infile, filename, linecount_p) == -1)
	{
	  analyse_free(text);
	  return NULL;
	}

      analyse_string_fit_start_end(line);
      if (strlen(line) > 0)
	{
	  strcat(text, line);
/* el41m16 +1	  strcat(text, "\\n"); */
	  strcat(text, "\n");
	}
    }

  return text;
}

analyse_comment_t *analyse_read_comment(char *line, FILE *infile, char *filename, int *linecount_p)
{
  analyse_comment_t *comment;
  
  /* Read Comment */
  comment = (analyse_comment_t *)analyse_malloc(sizeof(analyse_comment_t));
  
  if ( analyse_getline(line, infile, filename, linecount_p) == -1)
    {
      analyse_free(comment);
      return NULL;
    }
  
  if (strncmp(line, "ENGLISH",7) == 0) 
    comment->language = ANALYSE_ENGLISH;
  else
    comment->language = -1;

  comment->text = analyse_read_text(line, infile, filename, linecount_p);

  return comment;
}


void analyse_count_error(char *line,  FILE *infile, char *infilename, int *linecount_p, int *cnt_p)
{
  *cnt_p = 0;

  while ( ( analyse_getline(line, infile, infilename, linecount_p) != -1) &&
	  ( strncmp(line, "/END", 4) != 0 ))
    {
      /*ls41l33      if (strstr(line, "/ANALYSE/ERROR/") != NULL) (*cnt_p)++;*/
      if (strstr(line, "/ANALYSE/MESSAGE/") != NULL) (*cnt_p)++;
    }
}


void analyse_count_check(char *line,  FILE *infile, char *infilename, int *linecount_p, int *cnt_check_p, int *cnt_group_p)
{
  *cnt_check_p = 0;
  *cnt_group_p = 0;

  while ( ( analyse_getline(line, infile, infilename, linecount_p) != -1) &&
	  ( strncmp(line, "/END", 4) != 0 ))
    {
      if (strstr(line, "/ANALYSE/CHECK/GROUP") != NULL) 
	{
	  (*cnt_group_p)++;
	}
      else if ( strstr(line, "/ANALYSE/CHECK/") != NULL)
	{
	  (*cnt_check_p)++;
	}
    }
}


void analyse_stack_error(char *line, FILE *infile, char *infilename, int *linecount_p, 
			 int *cnt_p, int *size_p, int **tab_p)
{
  char key[ANALYSE_SIZE_OF_LINE];
  int step;

  *size_p = *cnt_p;
  *tab_p = (int*)analyse_malloc((*size_p)*sizeof(int));
  
  step = 0;

  while ( ( analyse_getline(line, infile, infilename, linecount_p) != -1) &&
	  ( strncmp(line, "/END", 4) != 0 ))
    {
      /* ls41l33 if (strstr(line, "/ANALYSE/ERROR/") != NULL) */
      if (strstr(line, "/ANALYSE/MESSAGE/") != NULL)
	{
	  analyse_getkey(3 , line, key);
	  sscanf(key, "%d", *tab_p+ step );
	  step ++;
	}
    }
  
  /* sort Ids */
  qsort( *tab_p, *size_p, sizeof(int), compar_int);
  
  /* Cnt different id */
  *cnt_p = count_ids(*size_p, *tab_p);

}


void analyse_stack_check(char *line, FILE *infile, char *infilename, int *linecount_p, 
			 int *nb_check_group_p, int *size_p, int **tab_p,  
			 int *nb_check_p, int *size2_p, int **tab2_p)
{
  char key[ANALYSE_SIZE_OF_LINE];
  int nb_check, nb_check_group; 
  
  *size_p = *nb_check_group_p;
  *size2_p = *nb_check_p;
  
  *tab_p =(int*)analyse_malloc((*size_p)*sizeof(int));
  *tab2_p =(int*)analyse_malloc((*size2_p)*sizeof(int));
  
  nb_check_group = 0;
  nb_check = 0;

  while ( ( analyse_getline(line, infile, infilename, linecount_p) != -1) &&
	  ( strncmp(line, "/END", 4) != 0 ))
    {
      if (strstr(line, "/ANALYSE/CHECK/GROUP") != NULL) 
	{
	  analyse_getkey(4 , line, key);
	  sscanf(key, "%d", *tab_p+nb_check_group );
	  nb_check_group++;
	}
      else if (strstr(line, "/ANALYSE/CHECK") != NULL) 
	{
	  analyse_getkey(3 , line, key);
	  sscanf(key, "%d", *tab2_p+nb_check);
	  nb_check ++;
	}
    }

  /* sort Ids */
  qsort(*tab_p, *size_p, sizeof(int), compar_int);
  qsort(*tab2_p, *size2_p, sizeof(int), compar_int);
  
  /* Cnt different id */
  *nb_check_group_p = count_ids(*size_p, *tab_p);
  *nb_check_p = count_ids(*size2_p, *tab2_p);

}
      
