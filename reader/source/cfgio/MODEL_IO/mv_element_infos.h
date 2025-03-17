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


#ifndef MV_ELEMENT_INFOS_H
#define MV_ELEMENT_INFOS_H

#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>


class MvElementInfos_t {

 public: // Constructors & destructor
  inline MvElementInfos_t() {}
  inline ~MvElementInfos_t() {}

 public: // Input access
  void reserve(int nb_lines);
  void addLine(int nb_nodes,...);
  
 public: // Output access
  inline int  getNbLines()                             const { return (int)(myNodeIndexes.size()); }
  inline int  getNbNodes(int line_ind)                 const { return (int)(myNodeIndexes[line_ind].size()); }
  inline int  getNodeIndex(int line_ind,int node_ind)  const { return myNodeIndexes[line_ind][node_ind]; }
  inline bool isNodeMandatory(int line_ind,int node_ind) const { return myNodeStates[line_ind][node_ind]!=0; }

 private: // Types
  typedef vector<int>      MyLine_t;
  typedef vector<MyLine_t> MyNodeIndexes_t;
  typedef vector<MyLine_t> MyNodeStates_t;

 private: // Data
  MyNodeIndexes_t myNodeIndexes;
  MyNodeStates_t  myNodeStates;

};


#endif //MV_ELEMENT_INFOS_H
