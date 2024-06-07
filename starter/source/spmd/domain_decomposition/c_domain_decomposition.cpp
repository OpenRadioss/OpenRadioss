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
#include <iostream>
#include <vector>
#include <map>
#include <cstdlib>
#include <set>
#include <unordered_map>
#include <algorithm>
#include <limits>
#include <cmath>
#include <chrono>
#define _FCALL

constexpr size_t MAX_CONSTRAINTS = 12;
constexpr size_t ELEM_POS = 0;
constexpr size_t INTS_POS = 1;
constexpr size_t INTM_POS = 2;
constexpr size_t INT2_POS = 3;
constexpr size_t DDL_POS = 4;
constexpr size_t SOL_POS = 5;
constexpr size_t FSI_POS = 6;
constexpr size_t DEL_POS = 7;
constexpr size_t CAND_POS = 8;
constexpr size_t KIN_POS = 9;
constexpr size_t R2R_POS = 10;
constexpr size_t NOD_SMS_POS = 11;

constexpr int OPTION_PTYPE = 0;
constexpr int OPTION_OBJTYPE = 1;
constexpr int OPTION_CTYPE = 2;
constexpr int OPTION_IPTYPE = 3;
constexpr int OPTION_RTYPE = 4;
constexpr int OPTION_DBGLVL = 5;
constexpr int OPTION_NITER = 6;
constexpr int OPTION_NCUTS = 7;
constexpr int OPTION_SEED = 8;
constexpr int OPTION_NO2HOP = 9;
constexpr int OPTION_MINCONN = 10;
constexpr int OPTION_CONTIG = 11;
constexpr int OPTION_COMPRESS = 12;
constexpr int OPTION_CCORDER = 13;
constexpr int OPTION_PFACTOR = 14;
constexpr int OPTION_NSEPS = 15;
constexpr int OPTION_UFACTOR = 16;
constexpr int OPTION_NUMBERING = 17;

extern "C"
{
        int METIS_PartGraphKway(int *, int *, int *, int *, int *, int *, int *, int *, float *, float *, int *, int *, int *);
        int METIS_PartGraphRecursive(int *, int *, int *, int *, int *, int *, int *, int *, float *, float *, int *, int *, int *);
}

// Function to permute cep2 to maximize the number of common elements with cep1
void permute(std::vector<int> &cep1, std::vector<int> &cep2, int ncolor)
{
        // Create cost matrix
        std::vector<std::vector<int>> cost_matrix(ncolor, std::vector<int>(ncolor, 0));
        const int n = cep1.size();

        for (int i = 0; i < n; i++)
        {
                cost_matrix[cep1[i]][cep2[i]]++;
        }

        // Greedy algorithm to find the best permutation
        std::vector<int> perm(ncolor, 0);
        std::vector<bool> used(ncolor, false);

        for (int i = 0; i < ncolor; i++)
        {
                int max_cost = -1;
                int max_j = -1;
                for (int j = 0; j < ncolor; j++)
                {
                        if (!used[j] && cost_matrix[i][j] > max_cost)
                        {
                                max_cost = cost_matrix[i][j];
                                max_j = j;
                        }
                }
                perm[i] = max_j;
                used[max_j] = true;
        }

        // Apply the permutation to cep2
        for (int i = 0; i < n; i++)
        {
                cep2[i] = perm[cep2[i]];
        }
}

// gives a score to the partitioninng: min_part_weight/max_part_weight
float score_partition(int *NNODE, int *NELEM, int ncond_elem, int *CEP, int *iwd_elem)
{
        // declare a vector of size *NELEM
        std::vector<int> domain_weights(*NNODE);

        // cumulate the weights of the elements in the same domain
        for (int i = 0; i < *NELEM; i++)
        {
                int domain = CEP[i] - 1;
                if (domain < 0 || domain >= *NNODE)
                {
                        CEP[i] == 1;
                        domain = 0;
                }
                int j = 0;
                domain_weights[domain] += iwd_elem[j + i * ncond_elem];
        }
        // write the domain weights:
        double tot_weight = 0;
        double max_weight = 0;
        double min_weight = std::numeric_limits<double>::max();
        for (int i = 0; i < *NNODE; i++)
        {
                // cast the weights to float
                double weight = static_cast<double>(domain_weights[i]);
                max_weight = std::max(max_weight, weight);
                tot_weight += weight;
                min_weight = std::min(min_weight, weight);
        }
        tot_weight = tot_weight / static_cast<double>(*NNODE);

        //        for(int i = 0 ; i < *NNODE ; i++)
        //        {
        //                std::cout<<"domain "<<i+1<<" weight = "<<domain_weights[i]<<std::endl;
        //        }
        //std::cout << "min=" << static_cast<long int>(min_weight) << "; max=" << static_cast<long int>(max_weight) << "; avg=" << static_cast<long int>(tot_weight) << std::endl;
        return (min_weight / max_weight) * (tot_weight / max_weight);
}

//
int get_number_of_boundary_partitions(int *NELEM, int *CEP, int *NNODE, int *XADJ, int *ADJNCY)
{
        std::vector<std::vector<bool>> connected(*NNODE, std::vector<bool>(*NNODE, false));

        for (int i = 0; i < *NELEM; i++)
        {
                int domain = CEP[i] - 1;
                for (int j = XADJ[i]; j < XADJ[i + 1]; j++)
                {
                        int neighbor = ADJNCY[j - 1] - 1;
                        int neighbor_domain = CEP[neighbor] - 1;
                        if (neighbor_domain != domain)
                        {
                                connected[domain][neighbor_domain] = true;
                        }
                }
        }
        // count the number of connected domains
        int max_count = 0;
        for (int i = 0; i < *NNODE; i++)
        {
                int count = 0;
                for (int j = 0; j < *NNODE; j++)
                {
                        if (connected[i][j])
                        {
                                count++;
                        }
                }
                max_count = std::max(max_count, count);
        }
        return max_count;
}

void METIS_Part(int which_function, int &IERR, int *NELEM, int *NCOND, int *XADJ, int *ADJNCY, int *IWD, int *NNODE, float *UBVEC, int *OPTIONS, int *NEC, int *CEP)
{
        // start chrono
        // auto start = std::chrono::high_resolution_clock::now();
        // Select the function based on the identifier passed from Fortran
        int *cep1 = (int *)malloc(*NELEM * sizeof(int));
        int *cep2 = (int *)malloc(*NELEM * sizeof(int));

        int nec1;
        int nec2;
        int *vsize = nullptr;
        int *ADJWGT2 = nullptr;
        float *tpwgts = nullptr;
        float score = 0;
        int nbound = std::numeric_limits<int>::max();
        *NEC = std::numeric_limits<int>::max();
        int iter_max = 1;

        if (*NCOND == 1)
        {
                iter_max = 1;
        }

        // OPTIONS[OPTION_DBGLVL] = 13;
        constexpr float threshold = 0.9;
        //        for (int iter = 0; iter < iter_max; iter++)
        int iter = 0;
        int ierr1 = 0;
        int ierr2 = 0;
        while (iter < iter_max)
        {
                switch (which_function)
                {
                case 1:
                        vsize = nullptr;
                        ADJWGT2 = nullptr;
                        tpwgts = nullptr;
                        OPTIONS[OPTION_SEED] = iter;
                        ierr1 = METIS_PartGraphKway(NELEM, NCOND, XADJ, ADJNCY, IWD, vsize, ADJWGT2, NNODE, tpwgts, UBVEC, OPTIONS, &nec1, cep1);
                        if (vsize != NULL)
                                free(vsize);
                        if (ADJWGT2 != NULL)
                                free(ADJWGT2);
                        if (tpwgts != NULL)
                                free(tpwgts);

                        vsize = nullptr;
                        ADJWGT2 = nullptr;
                        tpwgts = nullptr;
                        OPTIONS[OPTION_SEED] = iter_max + iter;
                        ierr2 = METIS_PartGraphKway(NELEM, NCOND, XADJ, ADJNCY, IWD, vsize, ADJWGT2, NNODE, tpwgts, UBVEC, OPTIONS, &nec2, cep2);
                        if (vsize != NULL)
                                free(vsize);
                        if (ADJWGT2 != NULL)
                                free(ADJWGT2);
                        if (tpwgts != NULL)
                                free(tpwgts);

                        break;
                case 2:
                        vsize = nullptr;
                        ADJWGT2 = nullptr;
                        tpwgts = nullptr;
                        OPTIONS[OPTION_SEED] = iter;
                        ierr1 = METIS_PartGraphRecursive(NELEM, NCOND, XADJ, ADJNCY, IWD, vsize, ADJWGT2, NNODE, tpwgts, UBVEC, OPTIONS, &nec1, cep1);
                        if (vsize != NULL)
                                free(vsize);
                        if (ADJWGT2 != NULL)
                                free(ADJWGT2);
                        if (tpwgts != NULL)
                                free(tpwgts);

                        vsize = nullptr;
                        ADJWGT2 = nullptr;
                        tpwgts = nullptr;
                        OPTIONS[OPTION_SEED] = iter_max + iter;
                        ierr2 = METIS_PartGraphRecursive(NELEM, NCOND, XADJ, ADJNCY, IWD, vsize, ADJWGT2, NNODE, tpwgts, UBVEC, OPTIONS, &nec2, cep2);
                        if (vsize != NULL)
                                free(vsize);
                        if (ADJWGT2 != NULL)
                                free(ADJWGT2);
                        if (tpwgts != NULL)
                                free(tpwgts);

                        break;
                default:
                        IERR = -1; // Invalid function identifier
                        break;
                }

                float score1 = score_partition(NNODE, NELEM, *NCOND, cep1, IWD);
                int nbound1 = get_number_of_boundary_partitions(NELEM, cep1, NNODE, XADJ, ADJNCY);
                float score2 = score_partition(NNODE, NELEM, *NCOND, cep2, IWD);
                int nbound2 = get_number_of_boundary_partitions(NELEM, cep2, NNODE, XADJ, ADJNCY);

                //  keep the best partitioning
                bool better_cut = (nec2 < *NEC) || (nec2 == *NEC && nbound2 < nbound);
                if ((score2 > threshold && better_cut) || (score2 > score && better_cut) || (iter == 0))
                {
                        for (int i = 0; i < *NELEM; i++)
                        {
                                CEP[i] = cep2[i];
                        }
                        *NEC = nec2;
                        score = score2;
                        nbound = nbound2;
                }
                better_cut = (nec1 < *NEC) || (nec1 == *NEC && nbound1 < nbound);
                if ((score1 > threshold && better_cut) || (score1 > score && better_cut))
                {
                        for (int i = 0; i < *NELEM; i++)
                        {
                                CEP[i] = cep1[i];
                        }
                        *NEC = nec1;
                        score = score1;
                        nbound = nbound1;
                }
                ++iter;
        }

        // free the memory
        free(cep1);
        free(cep2);
        // chrono end and print (in seconds)
        // std::cout<<"time = "<<std::chrono::duration<double>(std::chrono::high_resolution_clock::now() - start).count()<<std::endl;
}
void print_dd_results(int *NNODE, int *NELEM, int ncond_elem, int *CEP, int *iwd_elem)
{
        // declare a vector of size *NELEM
        std::vector<std::vector<int>> domain_weights(*NNODE);
        for (int i = 0; i < *NNODE; i++)
        {
                domain_weights[i].resize(ncond_elem);
        }
        // cumulate the weights of the elements in the same domain
        for (int i = 0; i < *NELEM; i++)
        {
                int domain = CEP[i] - 1;
                for (int j = 0; j < ncond_elem; j++)
                {
                        domain_weights[domain][j] += iwd_elem[j + i * ncond_elem];
                }
        }
        // write the domain weights:
        for (int i = 0; i < *NNODE; i++)
        {
                // std::cout << "domain " << i + 1 << " weights: ";
                for (int j = 0; j < ncond_elem; j++)
                {
                        std::cout << domain_weights[i][j] << " ";
                }
                std::cout << std::endl;
        }
}

void call_metis_function(int which_function, int *NELEM, int *NCOND, int *XADJ, int *ADJNCY, int *IWD, int *NNODE,
                         float *UBVEC, int *OPTIONS, int *NEC, int *CEP, int *constraints, int *cep_interf)
{

        int IERR;
        if (which_function > 0)
        { // Normal call to METIS_Part

                METIS_Part(which_function, IERR, NELEM, NCOND, XADJ, ADJNCY, IWD, NNODE, UBVEC, OPTIONS, NEC, CEP);
                if (cep_interf != nullptr)
                {
                        for (int i = 0; i < *NELEM; ++i)
                        {
                                cep_interf[i] = CEP[i];
                        }
                }
        }
        else
        { // split the constraints into the different arrays:  element and interface
                which_function = -which_function;
                int ncond_elem = 0;
                int ncond_interf = 0;
                if (constraints[INTS_POS] > 0)
                        ncond_interf++;
                if (constraints[INTM_POS] > 0)
                        ncond_interf++;
                if (constraints[CAND_POS] > 0)
                        ncond_interf++;
                ncond_elem = *NCOND - ncond_interf;
                // allocate iwd_elem and iwd_interf arrays of size nnode*ncond_elem and nnode*ncond_interf
                int *iwd_elem = (int *)malloc(*NELEM * ncond_elem * sizeof(int));
                int *iwd_interf = (int *)malloc(*NELEM * ncond_interf * sizeof(int));
                // allocate ubvec_elem and ubvec_interf arrays of size ncond_elem and ncond_interf
                float *ubvec_elem = (float *)malloc(ncond_elem * sizeof(float));
                float *ubvec_interf = (float *)malloc(ncond_interf * sizeof(float));

                int i_elem = 0;
                int i_interf = 0;
                for (int i = 0; i < *NCOND; ++i)
                {
                        // constraints[INT_POS] = i
                        {
                                if (i + 1 == constraints[INTS_POS] || i + 1 == constraints[INTM_POS] || i + 1 == constraints[CAND_POS])
                                { // if the constraint is an interface constraint
                                  // std::cout << "interface constraint " << i + 1 << std::endl;
                                        for (int j = 0; j < *NELEM; ++j)
                                        {
                                                iwd_interf[i_interf + j * ncond_interf] = std::max(10 * IWD[i + j * *NCOND], 1);
                                        }
                                        ubvec_interf[i_interf] = UBVEC[i];
                                        i_interf++;
                                }
                                else
                                {
                                        //   std::cout << "element constraint " << i + 1 << std::endl;
                                        for (int j = 0; j < *NELEM; ++j)
                                        {
                                                iwd_elem[i_elem + j * ncond_elem] = IWD[i + j * *NCOND];
                                        }
                                        ubvec_elem[i_elem] = UBVEC[i];
                                        i_elem++;
                                }
                        }
                }

                if (ncond_elem > 0)
                {
                        METIS_Part(which_function, IERR, NELEM, &ncond_elem, XADJ, ADJNCY, iwd_elem, NNODE, ubvec_elem, OPTIONS, NEC, CEP);
                        // print_dd_results(NNODE, NELEM, ncond_elem, CEP, iwd_elem);
                }
                else
                {
                        for (int i = 0; i < *NELEM; ++i)
                        {
                                CEP[i] = 1;
                        }
                }

                if (ncond_interf > 0)
                {
                        METIS_Part(which_function, IERR, NELEM, &ncond_interf, XADJ, ADJNCY, iwd_interf, NNODE, ubvec_interf, OPTIONS, NEC, cep_interf);
                        // print_dd_results(NNODE, NELEM, ncond_interf, cep_interf, iwd_interf);
                }
                else
                {
                        for (int i = 0; i < *NELEM; ++i)
                        {
                                cep_interf[i] = CEP[i];
                        }
                }

                if (ncond_interf > 0 && ncond_elem > 0)
                {
                        // permute the interface partitioning to maximize the number of common elements with the element partitioning
                        std::vector<int> cep1(*NELEM);
                        std::vector<int> cep2(*NELEM);
                        int count = 0;

                        // copy the partitioning to the vectors, and decrement the values by 1
                        for (size_t i = 0; i < *NELEM; i++)
                        {
                                cep1[i] = CEP[i] - 1;
                                cep2[i] = cep_interf[i] - 1;
                                if (cep1[i] != cep2[i])
                                {
                                        count++;
                                }
                        }

                        permute(cep1, cep2, *NNODE);

                        // copy the values back to the cep_interf array
                        count = 0;
                        for (size_t i = 0; i < *NELEM; i++)
                        {
                                cep_interf[i] = cep2[i] + 1;
                                if (cep_interf[i] != CEP[i])
                                {
                                        count++;
                                }
                        }
                }

                free(iwd_elem);
                free(iwd_interf);
                free(ubvec_elem);
                free(ubvec_interf);
        }
}

// Function to be called from Fortran
extern "C"
{
        void call_metis(int *which_function, int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                        int *IWD, int *NNODE, float *UBVEC, int *OPTIONS, int *NEC, int *CEP,
                        int *constraints, int *cep_interf)
        {
                call_metis_function(*which_function, NELEM, NCOND, XADJ, ADJNCY, IWD, NNODE, UBVEC, OPTIONS, NEC, CEP, constraints, cep_interf);
        }
        void _FCALL CALL_METIS(int *which_function, int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                               int *IWD, int *NNODE, float *UBVEC, int *OPTIONS, int *NEC, int *CEP,
                               int *constraints, int *cep_interf)
        {
                call_metis_function(*which_function, NELEM, NCOND, XADJ, ADJNCY, IWD, NNODE, UBVEC, OPTIONS, NEC, CEP, constraints, cep_interf);
        }
        void call_metis_(int *which_function, int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                         int *IWD, int *NNODE, float *UBVEC, int *OPTIONS, int *NEC, int *CEP,
                         int *constraints, int *cep_interf)
        {
                call_metis_function(*which_function, NELEM, NCOND, XADJ, ADJNCY, IWD, NNODE, UBVEC, OPTIONS, NEC, CEP, constraints, cep_interf);
        }

        void call_metis__(int *which_function, int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                          int *IWD, int *NNODE, float *UBVEC, int *OPTIONS, int *NEC, int *CEP,
                          int *constraints, int *cep_interf)
        {
                call_metis_function(*which_function, NELEM, NCOND, XADJ, ADJNCY, IWD, NNODE, UBVEC, OPTIONS, NEC, CEP, constraints, cep_interf);
        }
}

// =================================================================================================
// used in ddtool.F
// =================================================================================================

// convert the connectivity array into a map of nodes to elements
std::unordered_map<int, std::set<int>> create_node2elements(int *connectivity, int max_nb_nodes_per_elt, int nelem)
{
        std::unordered_map<int, std::set<int>> node2elements;
        for (int i = 0; i < nelem; ++i)
        {
                for (int j = 0; j < max_nb_nodes_per_elt; ++j)
                {
                        int node = connectivity[j + i * max_nb_nodes_per_elt];
                        if (node != 0)
                        {
                                node2elements[node].insert(i);
                        }
                }
        }
        return node2elements;
}

std::set<int> intersection_of_4_sets(const std::set<int> &s1, const std::set<int> &s2, const std::set<int> &s3, const std::set<int> &s4)
{
        std::set<int> common_elements;
        for (auto it = s1.begin(); it != s1.end(); ++it)
        {
                if (s2.find(*it) != s2.end() && s3.find(*it) != s3.end() && s4.find(*it) != s4.end())
                {
                        common_elements.insert(*it);
                }
        }
        return common_elements;
}

extern "C"
{
        void find_element_from_nodes(int *nrtm, int *n1, int *n2, int *n3, int *n4,
                                     int *connectivity, // max_nb_nodes_per_elt x nelem
                                     int *max_nb_nodes_per_elt, int *nelem, int *ielem)
        {

                std::unordered_map<int, std::set<int>> node2elements = create_node2elements(connectivity, *max_nb_nodes_per_elt, *nelem);
                // write the size of the map
                for (int i = 0; i < *nrtm; ++i)
                {
                        std::set<int> n1_elements = node2elements[n1[i]]; // n1_elements starts at 0
                        std::set<int> n2_elements = node2elements[n2[i]];
                        std::set<int> n3_elements = node2elements[n3[i]];
                        std::set<int> n4_elements = node2elements[n4[i]];
                        // find the first element that contains the 4 nodes, using the intersection of the 4 sets
                        std::set<int> common_elements = intersection_of_4_sets(n1_elements, n2_elements, n3_elements, n4_elements);
                        if (common_elements.size() > 0)
                        {
                                ielem[i] = *common_elements.begin() + 1; // +1 because Fortran indexing starts at 1
                                if (ielem[i] < 1 || ielem[i] > *nelem)
                                {
                                        std::cout << "element " << ielem[i] << " is out of range" << std::endl;
                                }
                        }
                        else
                        {
                                //if not found, set the element to -1 : (TETRA10 elements)
                                ielem[i] = -1 ;
                                //ielem[i] = n1_elements.size() > 0 ? *n1_elements.begin() : 1;
                              //  std::cout << "element not found for nodes " << n1[i] << " " << n2[i] << " " << n3[i] << " " << n4[i] << std::endl;
                              //  std::cout << "n1_elements: ";
                              //  for (auto it = n1_elements.begin(); it != n1_elements.end(); ++it)
                              //  {
                              //          std::cout << *it << " ";
                              //  }
                              //  std::cout << std::endl;
                              //  std::cout << "n2_elements: ";
                              //  for (auto it = n2_elements.begin(); it != n2_elements.end(); ++it)
                              //  {
                              //          std::cout << *it << " ";
                              //  }
                              //  std::cout << std::endl;
                              //  std::cout << "n3_elements: ";
                              //  for (auto it = n3_elements.begin(); it != n3_elements.end(); ++it)
                              //  {
                              //          std::cout << *it << " ";
                              //  }
                              //  std::cout << std::endl;
                              //  std::cout << "n4_elements: ";
                              //  for (auto it = n4_elements.begin(); it != n4_elements.end(); ++it)
                              //  {
                              //          std::cout << *it << " ";
                              //  }
                              //  std::cout << std::endl;
                              //  // exit
                              //  //std::exit(1);
                        }
                }
        }

        void find_element_from_nodes_(int *nrtm, int *n1, int *n2, int *n3, int *n4,
                                      int *connectivity, // max_nb_nodes_per_elt x nelem
                                      int *max_nb_nodes_per_elt, int *nelem, int *ielem)
        {
                find_element_from_nodes(nrtm, n1, n2, n3, n4, connectivity, max_nb_nodes_per_elt, nelem, ielem);
        }

        void find_element_from_nodes__(int *nrtm, int *n1, int *n2, int *n3, int *n4,
                                       int *connectivity, // max_nb_nodes_per_elt x nelem
                                       int *max_nb_nodes_per_elt, int *nelem, int *ielem)
        {
                find_element_from_nodes(nrtm, n1, n2, n3, n4, connectivity, max_nb_nodes_per_elt, nelem, ielem);
        }

        void _FCALL FIND_ELEMENT_FROM_NODES(int *nrtm, int *n1, int *n2, int *n3, int *n4,
                                            int *connectivity, // max_nb_nodes_per_elt x nelem
                                            int *max_nb_nodes_per_elt, int *nelem, int *ielem)
        {
                find_element_from_nodes(nrtm, n1, n2, n3, n4, connectivity, max_nb_nodes_per_elt, nelem, ielem);
        }
}
// =================================================================================================
//  Stick elements to the same domain
// =================================================================================================
// global vector of hash table
std::vector<std::vector<int>> clusters;

extern "C"
{
        void c_prevent_decomposition_(int *clusterSize, int *elements)
        {
                const int cs = (*clusterSize);
                clusters.emplace_back(elements, elements + cs);
        }

        void c_enforce_constraints_(int *cep)
        {
                for (const auto &c : clusters)
                {
                        const int domain = cep[c[0]];
                        for (const auto &v : c)
                        {
                                //                                std::cout<<"element "<<v<<" moved from "<<cep[v-1]<<" to "<<domain<<std::endl;
                                cep[v - 1] = domain;
                        }
                }
                clusters.clear();
        }

        // Fortran 2 C porting
        void _FCALL C_PREVENT_DECOMPOSITION(int *clusterSize, int *elements)
        {
                c_prevent_decomposition_(clusterSize, elements);
        }
        void c_prvent_decomposition__(int *clusterSize, int *elements)
        {
                c_prevent_decomposition_(clusterSize, elements);
        }
        void c_prevent_decomposition(int *clusterSize, int *elements)
        {
                c_prevent_decomposition_(clusterSize, elements);
        }

        void _FCALL C_ENFORCE_CONSTRAINTS(int *cep)
        {
                c_enforce_constraints_(cep);
        }
        void c_enforce_constraints__(int *cep)
        {
                c_enforce_constraints_(cep);
        }
        void c_enforce_constraints(int *cep)
        {
                c_enforce_constraints_(cep);
        }
}
