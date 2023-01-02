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
#include "Graph.hpp"
using namespace std;
Graph::Graph(const int& npt, const int& nconnect, const vector<int>& connect_list) : 
  m_adj_list(vector<vector<int>> (npt, vector<int>())), m_degree(npt), m_npt(npt), m_nconnect(nconnect), 
  m_nb_connected_components(0), m_total_size(0)
{
  for (int i(0) ; i < m_nconnect ; ++i) {
    int p1 = connect_list.at(2 * i);
    int p2 = connect_list.at(2 * i + 1);
    m_adj_list.at(p1).push_back(p2);
    m_adj_list.at(p2).push_back(p1);
  }
  for (int i(0) ; i < npt ; ++i) {
    std::sort(m_adj_list.at(i).begin(), m_adj_list.at(i).end());
    vector<int>::iterator iter = std::unique(m_adj_list.at(i).begin(), m_adj_list.at(i).end());
    m_adj_list.at(i).erase(iter, m_adj_list.at(i).end());
    m_degree.at(i) = m_adj_list.at(i).size();
  }
  // Number of connected components and paths through each connected components
}

// DFS
vector<int> Graph::dfs(int p0, vector<int>& path) {
  vector<int> res(m_npt, -2);
  // color the points 0: white, 1: gray, 2: black
  m_color.resize(m_npt, 0);
  vector<int> p;
  p.push_back(p0);
  path.push_back(p0);
  m_color.at(p0) = 1;
  res.at(p0) = -1;
  bool ok = true;
  while (ok) {
    int si = *(p.end()-1);
    vector<int>::const_iterator iter = 
      find_if(m_adj_list.at(si).begin(), m_adj_list.at(si).end(), [this](const int& ii) {return (m_color.at(ii) == 0);});
    if (iter != m_adj_list.at(si).end()) {
      int sj = *iter;
      p.push_back(sj);
      m_color.at(sj) = 1;
      path.push_back(sj);
      res.at(sj) = si;
    } else {
      m_color.at(si) = 2;
      p.pop_back();
    }
    ok = !(p.empty());
  }
  return res;
}

void Graph::build_path() {
  m_nb_connected_components = 0;
  bool ok = true;
  int s0 = 0;
  while (ok) {
    m_nb_connected_components++;
    vector<int> path;
    path.reserve(m_npt);
    m_path_diag.push_back(dfs(s0, path));
    m_path.push_back(path);
    vector<int>::iterator iter;
    ok = (iter = find(m_color.begin(), m_color.end(), 0)) != m_color.end();
    if (ok) {
      s0 = distance(m_color.begin(), iter);
    }
  }

  m_total_size = 0;
  for (int i_connect(0) ; i_connect < m_nb_connected_components ; ++i_connect) {
    // find seed
    vector<int>::iterator iter(find(m_path_diag.at(i_connect).begin(), m_path_diag.at(i_connect).end(), -1));
    if (iter != m_path_diag.at(i_connect).end()) {
      int s0 = distance(m_path_diag.at(i_connect).begin(), iter);
      vector<vector<int>> inver(m_npt, vector<int>());
      for (int ipt(0) ; ipt < m_npt ; ipt++) {
	if (m_path_diag.at(i_connect).at(ipt) >= 0) {
	  inver.at(m_path_diag.at(i_connect).at(ipt)).push_back(ipt);
	}
      }
    }
    m_total_size += m_path.at(i_connect).size();
  }
}

vector<bool> Graph::build_cycle()
{
  vector<bool> res(m_nb_connected_components, false);
  for (int iconnect(0) ; iconnect < m_nb_connected_components ; ++iconnect) {
    vector<int> degree;
    for (int i(0) ; i < m_path.at(iconnect).size() ; ++i) {
      degree.push_back(m_degree.at(m_path.at(iconnect).at(i)));
    }
    vector<int>::iterator max_it, min_it;
    max_it = max_element(degree.begin(), degree.end());
    min_it = min_element(degree.begin(), degree.end());
    if (*max_it == 2 && *min_it == 2) {
      vector<int> path_new;
      int s0 = m_path.at(iconnect).at(0);
      int sinit = s0;
      int s1, s2;
      s1 = m_adj_list.at(s0).at(0);
      bool ok = true;
      while (ok) {
	path_new.push_back(s0);
	if ((s2 = m_adj_list.at(s1).at(0)) != s0) {
	} else {
	  s2 = m_adj_list.at(s1).at(1);
	}
	ok = (path_new.size() != m_path.at(iconnect).size());
	if (!ok && s1 == sinit) {
	  res.at(iconnect) = true;
	}
	s0 = s1;
	s1 = s2;
      }
      m_path.at(iconnect) = path_new;
    }
  }
  return res;
}

void Graph::print() const {
  cout << "Number of points: " << m_npt << endl;
  cout << "Number of edges: " << m_nconnect << endl;
  cout << "Number of connected components: " << m_nb_connected_components << endl;
  for (int i(0) ; i < m_nb_connected_components ; ++i) {
    cout << "Component " << i << endl;
    cout << "\t";
    for (int j(0) ; j < m_path.at(i).size() ; j++) {
      cout << m_path.at(i).at(j) << " " ;
    }
    cout << endl;
  }
}
