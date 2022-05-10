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
#ifndef __ANALYSE_CHECK_H__
#define __ANALYSE_CHECK_H__

#include "analyse_comment.h"

typedef struct analyse_check_group_s
{
  int order;
  int id;
  
  analyse_comment_t *title;

  int nb_check;
  int *check_list;

  char **check_message; /* a virer. ne sert plus a rien */

}analyse_check_group_t;

typedef struct analyse_check_s
{
  int id;

  analyse_comment_t *title;

  char *check_message;

}analyse_check_t;

void analyse_check_store(analyse_check_t *check_list, int language, int check_id);
void analyse_check_file_read(analyse_check_t **check_p, analyse_check_group_t **check_group_p, char *error_filename );
void analyse_check_file_write(analyse_check_t *check_list, analyse_check_group_t *check_group_list, int language);
void analyse_check_clean_memory(analyse_check_t *check_list, analyse_check_group_t *check_group_list);

#endif
