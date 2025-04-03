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


#include <UTILS/win32_utils.h>

#include <UTILS/mv_cstdarg.h>

#include "mv_element_infos.h"


/* --------- Input access --------- */

void MvElementInfos_t::reserve(int nb_lines) {
  myNodeIndexes.reserve(nb_lines);
  myNodeStates.reserve(nb_lines);
}

void MvElementInfos_t::addLine(int nb_nodes,...) {
  va_list a_arglist;
  va_start(a_arglist,nb_nodes);
  //
  MyLine_t a_empty_line;
  myNodeIndexes.push_back(a_empty_line);
  myNodeStates.push_back(a_empty_line);
  MyLine_t &a_indexes = myNodeIndexes.back();
  MyLine_t &a_states  = myNodeStates.back();
  a_indexes.resize(nb_nodes);
  a_states.resize(nb_nodes);
  //
  for(int i=0;i<nb_nodes;++i) {
    int  a_node_ind    = va_arg(a_arglist,int);
    bool a_node_status = (bool)(va_arg(a_arglist,int) != 0);
    //
    a_indexes[i] = a_node_ind;
    a_states[i]  = a_node_status ? 1 : 0;
  }
  //
  va_end(a_arglist);
}
