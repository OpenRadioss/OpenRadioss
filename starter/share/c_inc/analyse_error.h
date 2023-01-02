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
#ifndef __ANALYSE_ERROR_H__
#define __ANALYSE_ERROR_H__

#include "analyse_comment.h"

typedef struct analyse_error_info_s
{
  int id;

  /* Cls41l04 +2 */
  int global_cnt;
  int tmp_cnt;

  /* Title */
  analyse_comment_t *title;

  /* Description */
  analyse_comment_t *description;

  /* Comment */
  analyse_comment_t *comment;

}analyse_error_info_t;

/* Cls41l04 +++ */
void analyse_error_cnt(analyse_error_info_t *error_list, int id);
void analyse_error_set_tmp_cnt(analyse_error_info_t *error_list, int id, int value);
void analyse_error_get_cnt(analyse_error_info_t *error_list, int id, int *global_cnt, int *tmp_cnt);
/* Cls41l04 --- */
void analyse_error_return_message(analyse_error_info_t *error_list, int error_warning_type, 
				  int language, int id, char **title, char **description, char **comment);
/*
void analyse_error_return_message(analyse_error_info_t *error_list,
				  int language, int id, char **title, char **description, char **comment);
*/
void analyse_error_file_read(analyse_error_info_t **error, char *error_description_filename);

#endif
