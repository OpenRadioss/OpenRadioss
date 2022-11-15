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
#include <string>
#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <set>
#include <unordered_map>
#include <algorithm>
#include <map>
#include <set>
#include <deque>
#include <list>
//#define NDEBUG
//#include <cassert>

using namespace std;

#define _FCALL

typedef vector<int> Vint;

// Elements are partitioned: one element belongs to only one domain
struct ElementPartition
{
    Vint localId;          // localId[global id] = local id on the domain
    vector<Vint> globalId; // globalId[domain p][global id] = local id on domain P
};

// Nodes are mapped to domains: one node can belong to many domains
class NodeMapping
{
    unordered_map<int, int> toCompact; // toCompact[global id] = compact id
    Vint toGlobal; // toGlobal[compact id] = global id

public:
    int size() { return toGlobal.size(); };
    void addGlobalId(const int &i)
    {
        if (toCompact.find(i) == toCompact.end())
        {
            const int nb = toCompact.size();
            toCompact[i] = nb;
            toGlobal.push_back(i);
        }
    };
    int getCompactId(const int &i) const
    {
        auto got = toCompact.find(i);
        //assert(got != toCompact.end());
        return got->second;
    };
    int getGlobalId(const int &i) const
    {
        //assert(i >= 0);
        //assert(((size_t)i) < toGlobal.size());
        return toGlobal[i];
    }
};

typedef vector<NodeMapping> Vorder;

class Adjacency
{

    vector<Vint> _Adj;
    size_t _N;

    // degree = number of neighbors
    size_t degree(const size_t &i) const
    {
        return _Adj[i].size();
    }


    //find a pseudo-peripheral root
    // [in] mask: mask[i] == -1  => i is not numbered yet
    // [out] lower_bound: starting point for looking for a root
    // [return] id of the next root: id such that mask[id] == -1 and degree(id) is minimal
    size_t pickRoot(const Vint &mask, size_t &lower_bound) const
    {
        size_t root = 0;

        // pick next avaiable root
        for (root = lower_bound; root < _N; root++)
        {
            if (mask[root] == -1)
            {
                break;
            }
        }

        int maxLevel = -1;
        bool continueLoop = true;
        size_t iter = 0;

        while(continueLoop && iter < _N)
        {
            iter++;
            size_t currentDist = 0;
            Vint level(_N, -1); // level[i] = distance to root  or -1 if not visited yet
            deque<int> currentVertices(1, root);
            level[root] = 0;
            while(currentVertices.size() > 0)
            {

                const int current = currentVertices.front();
                if (level[current] > level[root] || (level[current] == level[root] && degree(current) > degree(root)))
                {
                    //the new root is the vertex with the highest level
                    // with level > -1 (i.e. reachable from the current root)
                    root = current;
                } else if(level[current] == level[root] && degree(current) == degree(root))
                {
                    if(current < root)
                    {
                        root = current;
                    }
                }

                currentVertices.pop_front();
                currentDist = level[current] + 1;
                for(const auto &next : _Adj[current])
                {
                    if (level[next] == -1)
                    {
                        level[next] = currentDist;
                        currentVertices.push_back(next);
                    }
                }
            }

            if(maxLevel < level[root])
            { // continue if we have found a new root
                maxLevel = level[root];
            }else
            { // stop if we have not found a new root
                continueLoop = false;
            }
        }

        //assert(mask[root] == -1);
        return root;
    }

public:
    void setNbVertices(const size_t &s)
    {
        _N = s;
        _Adj.resize(_N);
    }
    void reserve(const size_t &s)
    {
        for (auto &a : _Adj)
        {
            a.reserve(s);
        }
    }

    inline void addEdge(const int &a, const int &b)
    {
        _Adj[a].push_back(b);
    }


    // [out] order, permutation of indexes to minimize the bandwidth
    Vint CuthillMckee() const
    {
        Vint dist(_N,-1);
        Vint order(_N,-1);
        size_t lowerBound = 0;
        size_t nextIndex = 0;

        while (nextIndex < _N)
        {
            const size_t root = pickRoot(order, lowerBound);
            //assert(dist[root] == -1);
            //assert(order[root] == -1);
            dist[root] = 0;
            order[root] = nextIndex++;
            size_t currentDist = 0;
            deque<int> currentVertices(1, root);
            while(currentVertices.size() > 0)
            {
                const int current = currentVertices.front();
                currentVertices.pop_front();
                currentDist = dist[current] + 1;
                multiset<pair<int, int>> nextVertices;
                for (const auto &next : _Adj[current])
                {
                    if (dist[next] == -1)
                    {
                        dist[next] = currentDist;
                        nextVertices.insert(make_pair(degree(next), next));
                    }
                }
                for(const auto &next : nextVertices)
                {
                    order[next.second] = nextIndex++;
                    currentVertices.push_back(next.second);
                }
            }
        }
        // order["old"] = "new"
        return order;
    }

    void printStats(const Vint &perm) const
    {
        int newMaxBandwidth = 0;
        long long int newSumBandwidth = 0;
        int oldMaxBandwidth = 0;
        long long int oldSumBandwidth = 0;
        Vint iperm(perm.size());
        for (size_t i = 0; i < perm.size(); ++i)
        {
            iperm[perm[i]] = i;
        }
        for (size_t i = 0; i < _N; ++i)
        {
            for (const auto &j : _Adj[i])
            {
                const int newBandwidth = abs(perm[i] - perm[j]);
                newMaxBandwidth = max(newMaxBandwidth, newBandwidth);
                newSumBandwidth += newBandwidth;
                const int oldBandwidth = abs(((int)i) - j);
                oldMaxBandwidth = max(oldMaxBandwidth, oldBandwidth);
                oldSumBandwidth += oldBandwidth;
            }
        }
        if (_N > 1)
        {
            cout << "n:" << _N << " Old/New Max: " << ((double)oldMaxBandwidth)<<"/"<<((double)newMaxBandwidth);
            cout << " Sum: " << ((double)oldSumBandwidth)<<"/"<<((double)newSumBandwidth) << endl;
        }
    }
    void printPermutation(const vector<int> &perm) const
    {
        ofstream outfile;
        outfile.open("spy_matrices.m");
        outfile << "clear all; \n";
        outfile << " A = sparse(" << _N << "," << _N << "); \n";
        outfile << " P = zeros(" << _N << ",1); \n";
        for (size_t i = 0; i < _N; ++i)
        {
            outfile << "P(" << perm[i] + 1 << ")=" << i + 1 << "; \n";
        }
        for (size_t i = 0; i < _N; ++i)
        {
            for (const auto &j : _Adj[i])
            {
                outfile << "A(" << i + 1 << "," << j + 1 << ") = 1; \n";
            }
        }
        outfile.close();
    }
};
// checks that v is a permutation of 0:v.size()-1
bool is_permutation(const Vint &v)
{
    Vint r(v.size(), 0);
    for (const auto &i : v)
    {
        if (i < 0 || ((size_t)i) > v.size())
        {
            return false;
        }
        r[i] = 1;
    }
    size_t sum = 0;
    for (auto &n : r)
    {
        sum += n;
    }
    return sum == v.size();
}
void splitPerDomain(int nel, int nodesPerElt, size_t nspmd, int *domain, int lda, int offset, int *elt2Nodes,
                    Vorder &nodes, ElementPartition &elts)
{ // Distribute the elements according to domain[i = 0:nspmd-1]
  // defines local-to-domain ids of elements and nodes
    elts.globalId = vector<Vint>(nspmd);
    elts.localId = Vint(nel, -1);
    for (int i = 0; i < nel; i++)
    {
        const int p = domain[i];
        elts.localId[i] = elts.globalId[p].size();
        elts.globalId[p].push_back(i);
        for (int j = 0; j < nodesPerElt; j++)
        {
            nodes[p].addGlobalId(elt2Nodes[i * lda + j + offset]);
        }
    }
}
extern "C"
{
    // called from Fortran
    void cpp_reorder_elements(int *NEL, int *NSPMD, int *NODES_PER_ELT, int *OFFSET, int *LDA, int *domain, int *elt2Nodes, int *permutation)
    {
        const int nel = *NEL; // number of elements
        const size_t nspmd = (size_t)*NSPMD; // number of subdomains
        const int nodesPerElt = *NODES_PER_ELT; // number of nodes per element
        const int offset = *OFFSET; // padding size of array elt2Nodes (see cgrtails.F)
        const int lda = *LDA;       // leading dimension of elt2Nodes array
        // elt2Nodes is a 2D array of size nel x nodesPerElt such as elt2Nodes[i*lda + j + offset] is the global id of the jth node of the ith element

        Vorder nodesMapping(nspmd);
        ElementPartition eltsParition;
        vector<Adjacency> adjacencies(nspmd);
        vector<vector<Vint>> node2Elts(nspmd);
        vector<Vint> elt2LocalNodes(nspmd);

        splitPerDomain(nel, nodesPerElt, nspmd, domain, lda, offset, elt2Nodes, nodesMapping, eltsParition);

        for (size_t p = 0; p < nspmd; p++)
        {
            const int nb_local_elts = eltsParition.globalId[p].size();
            const int nb_local_nodes = nodesMapping[p].size();
            node2Elts[p].resize(nb_local_nodes);
            adjacencies[p].setNbVertices(nb_local_elts);
            adjacencies[p].reserve(30); // estimation of the maximum number of neighbor per element
            elt2LocalNodes[p].resize(nb_local_elts * nodesPerElt);
        }

        // Fill node2Elts[domain][local_node_id] = set of local element id
        // Fill elt2LocalNodes[domain][position in connectivity] = local_node_id
        for (int i = 0; i < nel; i++)
        {
            const int p = domain[i];
            // local id of the element
            const int e = eltsParition.localId[i];
            for (int j = 0; j < nodesPerElt; j++)
            {
                // local id of the neighbouring node
                const int local_node_id = nodesMapping[p].getCompactId(elt2Nodes[i * lda + j + offset]);
                elt2LocalNodes[p][nodesPerElt * e + j] = local_node_id;
                node2Elts[p][local_node_id].push_back(e);
            }
        }

        // Fill Adjacency
        for(size_t p = 0; p < nspmd; p++) //for each domain
        {
            const size_t nel_local = eltsParition.globalId[p].size();
            Vint last(nel_local, -1);
            for(size_t e1 = 0 ; e1 < nel_local ; e1++) //for each element e1
            {
                for(size_t j = 0 ; j < nodesPerElt ; j++) //for each node n of e1
                {
                    const int n = elt2LocalNodes[p][nodesPerElt * e1 + j];
                    for(auto & e2 : node2Elts[p][n]) //for each element e2 connected to n
                    {
                        if(last[e2] != e1)
                        {
                            last[e2] = e1;
                            adjacencies[p].addEdge(e1, e2);
                        }
                    }
                }
            }
        }

        // for each domain, compute the local permutation (index)
        // and fill the global permutation
        int start = 0;
        for (size_t p = 0; p < nspmd; ++p)
        {
            Vint index = adjacencies[p].CuthillMckee();
            //adjacencies[p].printStats(index);
            for (size_t i = 0; i < index.size(); ++i)
            {
                // permutation["new"] = "old"
                permutation[(size_t)(start + index[i])] = 1 + eltsParition.globalId[p][i];
            }
            start += index.size();
        }
    };

    // Fortran to C++ API
    void CPP_REORDER_ELEMENTS_(int *NEL, int *NSPMD, int *NODES_PER_ELT, int *OFFSET, int *LDA, int *domain, int *elt2Nodes, int *permutation)
    {
        cpp_reorder_elements(NEL, NSPMD, NODES_PER_ELT, OFFSET, LDA, domain, elt2Nodes, permutation);
    }
    void CPP_REORDER_ELEMENTS__(int *NEL, int *NSPMD, int *NODES_PER_ELT, int *OFFSET, int *LDA, int *domain, int *elt2Nodes, int *permutation)
    {
        cpp_reorder_elements(NEL, NSPMD, NODES_PER_ELT, OFFSET, LDA, domain, elt2Nodes, permutation);
    }
    void _FCALL CPP_REORDER_ELEMENTS(int *NEL, int *NSPMD, int *NODES_PER_ELT, int *OFFSET, int *LDA, int *domain, int *elt2Nodes, int *permutation)
    {
        cpp_reorder_elements(NEL, NSPMD, NODES_PER_ELT, OFFSET, LDA, domain, elt2Nodes, permutation);
    }
    void cpp_reorder_elements_(int *NEL, int *NSPMD, int *NODES_PER_ELT, int *OFFSET, int *LDA, int *domain, int *elt2Nodes, int *permutation)
    {
        cpp_reorder_elements(NEL, NSPMD, NODES_PER_ELT, OFFSET, LDA, domain, elt2Nodes, permutation);
    }
    void cpp_reorder_elements__(int *NEL, int *NSPMD, int *NODES_PER_ELT, int *OFFSET, int *LDA, int *domain, int *elt2Nodes, int *permutation)
    {
        cpp_reorder_elements(NEL, NSPMD, NODES_PER_ELT, OFFSET, LDA, domain, elt2Nodes, permutation);
    }
}
