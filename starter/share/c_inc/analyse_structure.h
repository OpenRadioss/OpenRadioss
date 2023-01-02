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
#ifndef __ANALYSE_STRUCTURE_H__
#define __ANALYSE_STRUCTURE_H__

typedef struct analyse_info_s
{
  char *calling_name;
  int calling_id;

  int nb_float_data;
  int float_data_offset_start;

  int nb_int_data;
  int int_data_offset_start;

  void (*analyze_function);

}analyse_info_t;


typedef struct analyse_node_s
{
  struct analyse_node_s *list_prev;
  struct analyse_node_s *list_next;

  /* Tree */
  struct analyse_node_s *parent;
  struct analyse_node_s *child;

  struct analyse_node_s *next;
  struct analyse_node_s *prev;

  /* Data */
  analyse_info_t info;

}analyse_node_t;

void analyse_info_init(analyse_info_t *analyse_info);
void analyse_node_init(analyse_node_t *analyse_node);

#endif
