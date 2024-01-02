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
#include "Graph.hpp"

#define _FCALL

using namespace std;

// Interface 

extern "C" 
{
  void graph_build_path_(const int* , const int* , const int* , int* , Graph**);
  void graph_get_sizes_(Graph**, int*);
  void graph_get_path_(Graph**, int*);
  void graph_free_memory_(Graph**);
  void graph_build_cycles_(Graph**, int*);
  void graph_get_nb_adj_(Graph**, int*);
  void graph_get_adj_(Graph**, int*);
  void _FCALL GRAPH_BUILD_PATH(const int* npts, const int* nconnect, const int* connection, 
			       int* nb_connected_comp, Graph** graph_ptr)
  {
    graph_build_path_(npts, nconnect, connection, nb_connected_comp, graph_ptr);
  }
  void _FCALL GRAPH_GET_SIZES(Graph** graph_ptr, int* sizes) 
  {
    graph_get_sizes_(graph_ptr, sizes);
  }
  void _FCALL GRAPH_GET_PATH(Graph** graph_ptr, int* path) 
  {
    graph_get_path_(graph_ptr, path);
  }
  void _FCALL GRAPH_GET_NB_ADJ(Graph** graph_ptr, int* nb_adj) 
  {
    graph_get_nb_adj_(graph_ptr, nb_adj);
  }
  void _FCALL GRAPH_GET_ADJ(Graph** graph_ptr, int* adj) 
  {
    graph_get_adj_(graph_ptr, adj);
  }
  void _FCALL GRAPH_FREE_MEMORY(Graph** graph_ptr) 
  {
    graph_free_memory_(graph_ptr);
  }
  void _FCALL GRAPH_BUILD_CYCLES(Graph** graph_ptr, int* cycles) 
  {
    graph_build_cycles_(graph_ptr, cycles);
  }
}

// Build Path

void graph_build_path_(const int* npts, const int* nconnect, const int* connection, 
		       int* nb_connected_comp, Graph** graph_ptr)
{
  vector<int> connect_list;
  connect_list.reserve((*nconnect) * 2);
  for (int i(0) ; i < *nconnect ; ++i) {
    connect_list.push_back(connection[2 * i]);
    connect_list.push_back(connection[2 * i + 1]);
  }
  *graph_ptr = new Graph (*npts, *nconnect, connect_list);
  (*graph_ptr)->build_path();
  *nb_connected_comp = (*graph_ptr)->getNbConnectedComponents();
  vector<vector<int>> path = (*graph_ptr)->getPath();
}

// Get sizes

void graph_get_sizes_(Graph** graph_ptr, int* sizes) {
  for (int i(0) ; i < (*graph_ptr)->getNbConnectedComponents() ; ++i) {
    sizes[i] = (*graph_ptr)->getPath()[i].size();
  }
}

// Get path

void graph_get_path_(Graph** graph_ptr, int* path) {
  int i = 0;
  for (int iconnect(0) ; iconnect < (*graph_ptr)->getNbConnectedComponents() ; ++iconnect) {
    for (int ipt(0) ; ipt < (*graph_ptr)->getPath()[iconnect].size() ; ++ipt) {
      path[i] = (*graph_ptr)->getPath()[iconnect][ipt];
      i++;
    }
  }
}

// free meomory

void graph_free_memory_(Graph** graph_ptr) {
  delete(*graph_ptr);
  *graph_ptr = nullptr;
}

// build cycles

void graph_build_cycles_(Graph** graph_ptr, int* cycles) {
  vector<bool> res((*graph_ptr)->build_cycle());
  
  const int& nb_comp = (*graph_ptr)->getNbConnectedComponents();
  
  for (int iconnect(0) ; iconnect < nb_comp ; iconnect++) {
    cycles[iconnect] = res[iconnect] ? 1 : 0;
  }
}

// get adjacence list size by elem

void graph_get_nb_adj_(Graph** graph_ptr, int* nb_adj) {
  for (int i(0) ; i < (*graph_ptr)->getAdjList().size() ; ++i) {
    nb_adj[i] = (*graph_ptr)->getAdjList().at(i).size();
  }
}

// get adjacence list 

void graph_get_adj_(Graph** graph_ptr, int* adj) {
  int cpt = 0;
  for (int i(0) ; i < (*graph_ptr)->getAdjList().size() ; ++i) {
    for (int j(0) ; j < (*graph_ptr)->getAdjList().at(i).size() ; ++j) {
      adj[cpt] = (*graph_ptr)->getAdjList().at(i).at(j);
      cpt++;
    }
  }
}

