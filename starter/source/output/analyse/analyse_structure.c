//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2024 Altair Engineering Inc.
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

#include "analyse_structure.h"

void analyse_info_init(analyse_info_t *analyse_info)
{
  analyse_info->calling_name = NULL;
  analyse_info->calling_id = -1;

  analyse_info->nb_float_data = 0;
  analyse_info->float_data_offset_start = -1;

  analyse_info->nb_int_data = 0;
  analyse_info->int_data_offset_start = -1;

  analyse_info->analyze_function = NULL;
}

void analyse_node_init(analyse_node_t *analyse_node)
{
  analyse_node->list_prev = NULL;
  analyse_node->list_next = NULL;

  analyse_node->parent = NULL;
  analyse_node->child = NULL;
  analyse_node->next = NULL;
  analyse_node->prev = NULL;

  analyse_info_init(&analyse_node->info);
}
