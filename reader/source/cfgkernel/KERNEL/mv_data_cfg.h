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
#ifndef MV_DATA_CFG_H
#define MV_DATA_CFG_H

#include <UTILS/mv_string.h>





/// Data tree types
enum MvDataTreeType_s {
  /** */ DTT_UNKNOWN    = 0,
  /** */ DTT_ASSIGNED   = 1,
  /** */ DTT_POST       = 2,
  /** */ DTT_CONNECTION = 4,
  /** */ DTT_SCRIPT     = 8,
  /** */ DTT_LAST       = 16
};
/// Data tree types
typedef enum MvDataTreeType_s MvDataTreeType_e;



/// Begins iteration
inline MvDataTreeType_e MV_get_begin_data_tree_type()                    { return (MvDataTreeType_e)(DTT_UNKNOWN+1); }
/// Iterates
inline MvDataTreeType_e MV_get_next_data_tree_type(MvDataTreeType_e dtt) { return (MvDataTreeType_e)(dtt << 1); }
/// Ends iteration
inline MvDataTreeType_e MV_get_end_data_tree_type()                      { return DTT_LAST; }
/// Gets the index of the data tree type
int MV_get_data_tree_type_index(MvDataTreeType_e dtt);
/// Gets all tree types
int MV_get_all_data_tree_types();



bool MV_is_multiple_data_tree_type(MvDataTreeType_e dtt);
const string &MV_get_data_tree_type_rank_skeyword(MvDataTreeType_e dtt);



void MV_init_trees_cfg();
void close_trees_cfg();

//const MvPreDatasHierarchy_t *MV_get_tree_cfg(MvDataTreeType_e tree_type,const string &title="");



//inline const MvPreDatasHierarchy_t *MV_get_post_cfg() {
//  return MV_get_tree_cfg(DTT_POST,"Post data");
//}
//inline const MvPreDatasHierarchy_t *MV_get_connection_cfg() {
//  return MV_get_tree_cfg(DTT_CONNECTION,"Connection data");
//}
//
//inline const MvPreDatasHierarchy_t *MV_get_script_cfg() {
//  return MV_get_tree_cfg(DTT_SCRIPT,"Script data");
//}


#endif //MV_DATA_CFG_H




