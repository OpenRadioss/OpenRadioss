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
#include <iostream>
#include <vector>
#include <algorithm>

#define _FCALL

using namespace std;

struct tab3_t {
  int n1, n2, n3;
};

struct edge_t {
  edge_t(const int& nn1, const int& nn2, const int& eelem) {
    n1 = nn1;
    n2 = nn2;
    elem_list.push_back(eelem);
    nb_con = 0;
  }
  int n1, n2;
  vector<int> elem_list;
  int nb_con;
};

typedef vector<edge_t> edge_list_t;
typedef vector<int> tab1_t;

extern "C" 
{
  void edge_sort_(edge_list_t**, int*, int*, int*, int*);
  void edge_get_nb_connect_(edge_list_t**, int*);
  void edge_get_connect_(edge_list_t**, int*);
  void edge_free_memory_(edge_list_t**);
  void tab1_init_(tab1_t**);
  void tab1_append_(tab1_t**, int*);
  void tab1_append_tab_(tab1_t**, int*, int*);
  void tab1_get_size_(tab1_t**, int*);
  void tab1_get_(tab1_t**, int*);
  void tab1_free_memory_(tab1_t**);
  void _FCALL EDGE_SORT(edge_list_t** edge_ptr, int* n1, int* n2, int* elem, int* nedge) {
    edge_sort_(edge_ptr, n1, n2, elem, nedge);
  }
  void _FCALL EDGE_GET_NB_CONNECT(edge_list_t** edge_ptr, int* nb_conn) {
    edge_get_nb_connect_(edge_ptr, nb_conn);
  }
  void _FCALL EDGE_GET_CONNECT(edge_list_t** edge_ptr, int* elem) {
    edge_get_connect_(edge_ptr, elem);
  }
  void _FCALL EDGE_FREE_MEMORY(edge_list_t** edge_ptr) {
    edge_free_memory_(edge_ptr);
  }
  void _FCALL TAB1_INIT(tab1_t** tab1_ptr) {
    tab1_init_(tab1_ptr);
  }
  void _FCALL TAB1_APPEND(tab1_t** tab1_ptr, int* ii) {
    tab1_append_(tab1_ptr, ii);
  }
  void _FCALL TAB1_APPEND_TAB(tab1_t** tab1_ptr, int* n, int* tab) {
    tab1_append_tab_(tab1_ptr, n, tab);
  }
  void _FCALL TAB1_GET_SIZE(tab1_t** tab1_ptr, int* ii) {
    tab1_get_size_(tab1_ptr, ii);
  }
  void _FCALL TAB1_GET(tab1_t** tab1_ptr, int* ii) {
    tab1_get_(tab1_ptr, ii);
  }
  void _FCALL TAB1_FREE_MEMORY(tab1_t** tab1_ptr, int* ii) {
    tab1_free_memory_(tab1_ptr);
  }
}

// toot
void edge_sort_(edge_list_t** edge_ptr, int* n1, int* n2, int* elem, int* nedge)
{
  int nn = *nedge;
  std::vector<tab3_t> tab(nn);
  for (int i(0) ; i < nn ; ++i) {
    tab.at(i).n1 = n1[i];
    tab.at(i).n2 = n2[i];
    tab.at(i).n3 = elem[i];
  }
  std::sort(tab.begin(), tab.end(), [] (tab3_t ielem1, tab3_t ielem2) 
  	    {return (ielem1.n1 < ielem2.n1);});
  int i = 0;
  while (i < tab.size()) {
    int j = i;
    for ( ; (j < tab.size()) && (tab.at(i).n1 == tab.at(j).n1) ; j++) {}
    if (j != i) {
      std::vector<tab3_t>::iterator iter1 = tab.begin() + i;
      std::vector<tab3_t>::iterator iter2 = tab.begin() + j;
      std::sort(iter1, iter2, [] (tab3_t ielem1, tab3_t ielem2) {return (ielem1.n2 < ielem2.n2);});
      i = j;
    }
  }

  
  *edge_ptr = new edge_list_t;
  edge_list_t* edge_list = *edge_ptr;

  edge_list->push_back(edge_t(tab.at(0).n1, tab.at(0).n2, tab.at(0).n3));
  for (int i(1) ; i < nn ; ++i) {
    if (tab.at(i).n1 != (*(edge_list->end()-1)).n1 ||  tab.at(i).n2 != (*(edge_list->end()-1)).n2) {
      edge_list->push_back(edge_t(tab.at(i).n1, tab.at(i).n2, tab.at(i).n3));
    } else {
      (*(edge_list->end()-1)).elem_list.push_back(tab.at(i).n3);
    }
  }

  for (int i(0) ; i < edge_list->size() ; ++i) {
    edge_list->at(i).nb_con = edge_list->at(i).elem_list.size();
  }
  
  *nedge = edge_list->size();
  nn = *nedge;
  // Copy back
  for (int i(0) ; i < nn ; ++i) {
    n1[i] = edge_list->at(i).n1;
    n2[i] = edge_list->at(i).n2;
  }
}

void edge_get_nb_connect_(edge_list_t** edge_ptr, int* nb_conn)
{
  edge_list_t const * edge_list = *edge_ptr;
  int nn = edge_list->size();
  for (int i(0) ; i < nn ; ++i) {
    nb_conn[i] = edge_list->at(i).nb_con;
  }
}

void edge_get_connect_(edge_list_t** edge_ptr, int* elem)
{
  edge_list_t const * edge_list = *edge_ptr;
  int nn = edge_list->size();
  int cpt = 0;
  for (int i(0) ; i < nn ; ++i) {
    for (int j(0) ; j < edge_list->at(i).elem_list.size(); ++j) {
      elem[cpt] = edge_list->at(i).elem_list.at(j);
      cpt++;
    }
  }
}

void edge_free_memory_(edge_list_t** edge_ptr) 
{
  if (*edge_ptr != nullptr) {
    delete (*edge_ptr);
    *edge_ptr = nullptr;
  } 
}

void tab1_init_(tab1_t** tab_ptr) 
{
  (*tab_ptr) = new tab1_t;
}
void tab1_append_(tab1_t** tab_ptr, int* ii)
{
  (*tab_ptr)->push_back(*ii);
}
void tab1_append_tab_(tab1_t** tab_ptr, int* n, int* tab)
{
  for (int i(0) ; i < *n ; ++i) {
    (*tab_ptr)->push_back(tab[i]);
  }
}
void tab1_get_size_(tab1_t** tab_ptr, int* ss)
{
  *ss = (*tab_ptr)->size();
}
void tab1_get_(tab1_t** tab_ptr, int* out)
{
  int ss = (*tab_ptr)->size();
  for (int i(0) ; i < ss ; ++i) {
    out[i] = (*tab_ptr)->at(i);
  }
}
void tab1_free_memory_(tab1_t** tab_ptr)
{
  if ((*tab_ptr) != nullptr) {
    delete (*tab_ptr);
    (*tab_ptr) = nullptr;
  }
}
