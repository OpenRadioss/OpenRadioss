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
#ifndef GRAPH_H_
#define GRAPH_H_
#include <iostream>
#include <vector>
#include <algorithm>
#include <iterator>

struct exp_graph {
public: 
  int m_nb_connected_components, m_total_size;
  int* m_sizes;
  int* m_paths;
};

class Graph {
public:
  // Default ctr
  Graph(){};
  // Default destructor
  ~Graph(){};
  // Ctr with a given list of connexions
  Graph(const int& npt, const int& nconnect, const std::vector<int>& connect_list);
  // Build paths
  void build_path();
  // Build cycles
  std::vector<bool> build_cycle();
  const int& getNbConnectedComponents() const {return m_nb_connected_components;}
  const std::vector<std::vector<int>>& getPath() const {return m_path;}
  const int& getTotalSize() const {return m_total_size;};
  const std::vector<std::vector<int>>& getAdjList() const {return m_adj_list;};
  void print() const;
private:
  int m_npt;
  int m_nconnect;
  int m_nb_connected_components;
  int m_total_size;
  std::vector<std::vector<int>> m_adj_list, m_path_diag, m_path;
  std::vector<int> m_degree;
  std::vector<int> m_color;
  // Depth first search
  std::vector<int> dfs(int p0, std::vector<int>&);
};
#endif
