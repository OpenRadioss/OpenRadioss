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
#include <string.h> /* strlen strcat */
#include <stdio.h> /* sprintf */

#include "analyse_define.h" /* ANALYSE_SIZE_OF_LINE 
                               ANALYSE_ERROR_TITLE ANALYSE_ERROR_DESCRIPTION ANALYSE_CHECK_GROUP ANALYSE_CHECK */

#include "analyse.h" /* analyse_get_datas */

#include "analyse_string.h" /* analyse_convert_int_to_string */
#include "analyse_memory.h" /* analyse_malloc */
#include "analyse_error.h" /* analyse_error_info_t */
#include "analyse_check.h" /* analyse_check_t analyse_check_group_t */

#include "analyse_comment.h"

void  analyse_add_comment(int object_type, void *object, analyse_comment_t *comment)
{
  analyse_comment_t *scan;
  
  switch(object_type)
    {
    case ANALYSE_ERROR_TITLE:
      scan = ((analyse_error_info_t *)object)->title;
      break;

    case ANALYSE_ERROR_DESCRIPTION:
      scan = ((analyse_error_info_t *)object)->description;
      break;

    case ANALYSE_CHECK_GROUP:
      scan = ((analyse_check_group_t *)object)->title;
      break;
      
    case ANALYSE_CHECK:
      scan = ((analyse_check_t *)object)->title;
      break;

    default:
      return;
    }

  if (scan == NULL)
    {
      scan = comment;
      scan->prev=NULL;
      scan->next = NULL;

      switch(object_type)
	{
	case ANALYSE_ERROR_TITLE:
	  ((analyse_error_info_t *)object)->title = scan;
	  break;
	  
	case ANALYSE_ERROR_DESCRIPTION:
	  ((analyse_error_info_t *)object)->description = scan ;
	  break;
	  
	case ANALYSE_CHECK_GROUP:
	  ((analyse_check_group_t *)object)->title = scan;
	  break;
	  
	case ANALYSE_CHECK:
	  ((analyse_check_t *)object)->title = scan;
	  break;
	  
	default:
	  return;
	}
    }
  else
    {
      while (scan->next != NULL)
	{
	  scan = scan->next;
	}
      
      scan->next = comment;
      comment->prev = scan;
      comment->next = NULL;
    }     
}

analyse_comment_t * analyse_get_right_comment(analyse_comment_t *start_comment, int language, int default_language)
{
  analyse_comment_t *scan_comment;
  
  scan_comment = start_comment;
  while( (scan_comment->language != language) &&
	 (scan_comment->next != NULL))
    {
      scan_comment = scan_comment->next;
    }

  if ( scan_comment->language != language)
    {
      scan_comment = start_comment;
      while( (scan_comment->language != default_language) &&
	     (scan_comment->next != NULL))
	{
	  scan_comment = scan_comment->next;
	}
      
        if ( scan_comment->language != default_language)
	  return start_comment;
	else
	  return scan_comment;
    }
  else
    {
      return scan_comment;
    }
}


char *analyse_fill_description(int object_type, char *description)
{
  int length, nb;

  int int_step=0;
  int float_step=0;

  char *scan_in_description;
  char *scan_out_description;
  char *final_description;
  
  int nb_int;
  int *tab_int=NULL;

  int nb_float;
  float *tab_float=NULL;
 
  char line[ANALYSE_SIZE_OF_LINE];

  int final_description_length;
  line[0]='\0';

  analyse_get_datas(object_type, &nb_int, &tab_int, &nb_float, &tab_float);

  length = strlen(description);

  /* Cls41k14 length = length + nb_int*6 + nb_float*10 + 1; */
  length = length + nb_int*4*((int)sizeof(int)) + nb_float*4*((int)sizeof(float)) + 1;

  final_description_length=length;
  final_description = (char *)analyse_malloc(length*sizeof(char));

  scan_in_description = description;
  scan_out_description = final_description;

  *(scan_out_description)='\0';

  while((*scan_in_description) != '\0')
    {
      if ((*scan_in_description) == '%')
	{
	  if ((*(scan_in_description+1)) == 'd')
	    {
	      if (int_step < nb_int)
		{
		  sprintf(line, "%d", *(tab_int+int_step));
		  int_step++;
		}
	    }
	  else if ((*(scan_in_description+1)) == 'f')
	    {
	      if (float_step < nb_float)
		{
		  /* Cls41k14 		  sprintf(line, "%f", *(tab_float+float_step)); */
		  sprintf(line, "%g", *(tab_float+float_step));
		  float_step++;
		}
	    }
	  else if ((*(scan_in_description+1)) == 's')
	    {
	      if (int_step < nb_int)
		{
		  length = *(tab_int+int_step);
		  int_step++;
		}
	      else
		{
		  length = 0;
		}

	      if ( ( length != 0) && ((nb_int-int_step) >= length))
		{
		  if ( length >= ANALYSE_SIZE_OF_LINE)
		    nb=ANALYSE_SIZE_OF_LINE-1;
		  else
		    nb=length;
		  
		  analyse_convert_int_to_string(nb, tab_int+int_step, line);
		  int_step = int_step+length;
		}	      
	    }

	  length = strlen(line);
      #ifdef _WIN64
      strcat_s(final_description,final_description_length, line);
      #else
      strcat(final_description, line);
      #endif
	  line[0]='\0';

	  scan_out_description = scan_out_description + length;

	  scan_in_description = scan_in_description +2;
	}
      else
	{
	  *scan_out_description=*scan_in_description;
	  *(scan_out_description+1)='\0';
	  scan_out_description=scan_out_description+1;

	  scan_in_description = scan_in_description +1;
	}
    }

  return final_description;
}

