// Copyright>    OpenRadioss
// Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
// Copyright>
// Copyright>    This program is free software: you can redistribute it and/or modify
// Copyright>    it under the terms of the GNU Affero General Public License as published by
// Copyright>    the Free Software Foundation, either version 3 of the License, or
// Copyright>    (at your option) any later version.
// Copyright>
// Copyright>    This program is distributed in the hope that it will be useful,
// Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
// Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// Copyright>    GNU Affero General Public License for more details.
// Copyright>
// Copyright>    You should have received a copy of the GNU Affero General Public License
// Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
// Copyright>
// Copyright>
// Copyright>    Commercial Alternative: Altair Radioss Software
// Copyright>
// Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
// Copyright>    software under a commercial license.  Contact Altair to discuss further if the
// Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#include <stdlib.h>
#include <string.h> // for memcpy
#include <vector>
#include <algorithm>
#include <stack>
#include <cmath>
#include <limits> // for std::numeric_limits
#include <iostream>
#include <array>
#include "augmentor.h"
#include <chrono>

// Add bidirectional edges to graph and create new XADJ/ADJNCY arrays
void add_edges_to_graph(const std::vector<int> &original_xadj,
                        const std::vector<int> &original_adjncy,
                        const std::vector<std::pair<int, int>> &new_edges,
                        std::vector<int> &new_xadj,
                        std::vector<int> &new_adjncy)
{

    int nelem = original_xadj.size() - 1; // Number of vertices

    // Create adjacency lists for easier manipulation
    std::vector<std::vector<int>> adj_lists(nelem);

    // Add existing edges from original graph
    for (int v = 0; v < nelem; v++)
    {
        int start = original_xadj[v];   // Convert to 0-based
        int end = original_xadj[v + 1]; // Convert to 0-based

        for (int i = start; i < end; i++)
        {
            int neighbor = original_adjncy[i]; // Convert to 0-based vertex
            adj_lists[v].push_back(neighbor);
        }
    }

    // Add new bidirectional edges
    for (const auto &edge : new_edges)
    {
        int u = edge.first;  // Convert to 0-based
        int v = edge.second; // Convert to 0-based

        // Check bounds
        if (u >= 0 && u < nelem && v >= 0 && v < nelem && u != v)
        {
            // Add edge u -> v (if not already present)
            if (std::find(adj_lists[u].begin(), adj_lists[u].end(), v) == adj_lists[u].end())
            {
                adj_lists[u].push_back(v);
            }
            // Add edge v -> u (if not already present)
            if (std::find(adj_lists[v].begin(), adj_lists[v].end(), u) == adj_lists[v].end())
            {
                adj_lists[v].push_back(u);
            }
        }
    }

    // Sort adjacency lists for consistency
    for (int v = 0; v < nelem; v++)
    {
        std::sort(adj_lists[v].begin(), adj_lists[v].end());
    }

    // Build new XADJ and ADJNCY arrays
    new_xadj.clear();
    new_adjncy.clear();

    new_xadj.reserve(nelem + 1);

    int current_pos = 0; // Start at 1 for Fortran indexing
    new_xadj.push_back(current_pos);

    for (int v = 0; v < nelem; v++)
    {
        for (int neighbor : adj_lists[v])
        {
            new_adjncy.push_back(neighbor); // Convert back to 1-based
        }
        current_pos += adj_lists[v].size();
        new_xadj.push_back(current_pos);
    }
}
void relax_balance_constraints(float *UBVEC, int *NCOND, float relaxation_factor = 0.01f)
{
    if (UBVEC == nullptr || NCOND == nullptr)
    {
        return;
    }

    int ncond = *NCOND;

    // Skip first constraint (index 0), relax all others
    for (int c = 0; c < ncond; c++)
    {
        UBVEC[c] += relaxation_factor;
    }
}

float compute_weight_balance_ratio(int nelem, int nparts, const std::vector<int> &partition,
                                   const std::vector<int> &vertex_weights)
{
    // Calculate actual weight for each partition
    std::vector<int> partition_weights(nparts + 1, 0); // +1 because partitions are 1-based

    for (int i = 0; i < nelem; i++)
    {
        int part = partition[i];
        partition_weights[part] += vertex_weights[i];
    }

    // Find min and max partition weights
    int min_weight = std::numeric_limits<int>::max();
    int max_weight = 0;
    int avg_weight = 0;

    for (int p = 1; p <= nparts; p++)
    {
        if (partition_weights[p] > 0)
        { // Only consider non-empty partitions
            min_weight = std::min(min_weight, partition_weights[p]);
            max_weight = std::max(max_weight, partition_weights[p]);
            avg_weight += partition_weights[p];
        }
    }
    avg_weight /= nparts; // Average weight across all partitions

    // Return ratio w_min / w_max (closer to 1.0 is better balance)
    if (max_weight == 0)
    {
        return 0.0f; // Edge case: no valid partitions
    }
    std::cout << "min_weight: " << min_weight << ", max_weight: " << max_weight << ", avg_weight: " << avg_weight << std::endl;
    return static_cast<float>(min_weight) / static_cast<float>(max_weight);
}
int count_all_neighboring_partition_pairs(int nelem, const std::vector<int> &xadj,
                                          const std::vector<int> &adjncy,
                                          const std::vector<int> &partition)
{
    // Assuming max number of partitions is reasonably small (e.g. 65536)
    int max_partition = *std::max_element(partition.begin(), partition.end());
    std::vector<bool> seen_pairs(max_partition * max_partition, false);
    int count = 0;

    const int *xadj_ptr = xadj.data();
    const int *adjncy_ptr = adjncy.data();
    const int *part_ptr = partition.data();

    for (int vertex = 0; vertex < nelem; ++vertex)
    {
        int start = xadj_ptr[vertex] - 1;
        int end = xadj_ptr[vertex + 1] - 1;
        // if part_prt[vertex] is larger than max_partition, skip this vertex
        if (part_ptr[vertex] > max_partition)
        {
            continue;
        }
        unsigned short vp = static_cast<unsigned short>(part_ptr[vertex]);

        for (int i = start; i < end; ++i)
        {
            int neighbor = adjncy_ptr[i] - 1;

            if (vertex < neighbor)
            {
                if (part_ptr[neighbor] > max_partition)
                {
                    continue; // Skip neighbors with invalid partition
                }
                unsigned short np = static_cast<unsigned short>(part_ptr[neighbor]);

                if (vp != np)
                {
                    unsigned short p1 = std::min(vp, np);
                    unsigned short p2 = std::max(vp, np);

                    int index = p1 * max_partition + p2;
                    if (!seen_pairs[index])
                    {
                        seen_pairs[index] = true;
                        ++count;
                    }
                }
            }
        }
    }

    return count;
}

// Updated evaluation function with mixed quality metric - takes vectors directly
std::pair<float, float> evaluate_partition_quality(int *NELEM, const std::vector<int> &xadj, const std::vector<int> &adjncy, int *CEP,
                                                   int *IWD, int *NCOND, int *NNODE, float *UBVEC, const std::vector<float> &coords)
{
    int nelem = *NELEM;
    int ncond = *NCOND;
    int nparts = *NNODE;

    // Convert partition array to vector
    std::vector<int> partition(CEP, CEP + nelem);

    // Extract first constraint weights from IWD (first block of nelem weights)
    std::vector<int> vertex_weights(nelem);
    if (IWD != nullptr)
    {
        for (int i = 0; i < nelem; i++)
        {
            vertex_weights[i] = IWD[i * ncond]; // First constraint: indices 0 to nelem-1
        }
    }
    else
    {
        // If no weights provided, assume uniform weights
        std::fill(vertex_weights.begin(), vertex_weights.end(), 1);
    }

    // Find the maximum partition number using C++11 algorithm
    auto max_partition_iter = std::max_element(partition.begin(), partition.end());
    int max_partition = nparts;

    // Compute weight balance ratio (w_min / w_max)
    float weight_ratio = compute_weight_balance_ratio(nelem, nparts, partition, vertex_weights);

    // Count all neighboring partition pairs
    int rev_quality = count_all_neighboring_partition_pairs(nelem, xadj, adjncy, partition);
    // write time to console in seconds

    // Compute connectivity quality (inverse of component count)
    float connectivity_quality = (static_cast<float>(nparts * (nparts - 1)) - rev_quality) /
                                 static_cast<float>(nparts * (nparts - 1)); // Normalize to [0, 1]
    // cap connectivity quality to 1.0
    connectivity_quality = std::min(connectivity_quality, 1.0f);

    //    std::cout<<"weight: "<<weight_ratio<<", connectivity: "<<connectivity_quality<<"sum= "<<weight_ratio + connectivity_quality<<std::endl;
    return std::make_pair(weight_ratio, connectivity_quality);
}

extern "C"
{
    int METIS_PartGraphKway(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                            int *IWD, int *vsize, int *ADJWGT2, int *NNODE, float *tpwgts,
                            float *UBVEC, int *OPTIONS, int *NEC, int *CEP);
    int METIS_PartGraphRecursive(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                                 int *IWD, int *vsize, int *ADJWGT2, int *NNODE, float *tpwgts,
                                 float *UBVEC, int *OPTIONS, int *NEC, int *CEP);
    // IERR1 = METIS_SetDefaultOptions(options)
    int METIS_SetDefaultOptions(int *options);
}

// export to Fortran
extern "C"
{
    int cpp_wrap_metis_setdefaultoptions(int *OPTIONS)
    {
        int IERR1;
        IERR1 = METIS_SetDefaultOptions(OPTIONS);
        return IERR1;
    }

    int cpp_wrap_metis_partgraphkway(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                                     int *IWD, int *NNODE,
                                     float *UBVEC, int *OPTIONS, int *NEC, int *CEP, float *COORDS)
    {
        // start chrono
        auto start = std::chrono::high_resolution_clock::now();
        int *vsize = NULL;
        int *ADJWGT2 = NULL;
        float *tpwgts = NULL;
        int IERR1;
        int ncond = *NCOND;
        // Number of trials with different random seeds
        int num_trials = 1; // Adjust as needed
        // cap num trials by ncond
        num_trials = std::min(num_trials, ncond-1);
        num_trials = std::max(num_trials, 1);

        // Create copies of XADJ and ADJNCY as vectors
        std::vector<int> xadj(XADJ, XADJ + *NELEM + 1);             // XADJ has nelem+1 elements
        std::vector<int> adjncy(ADJNCY, ADJNCY + XADJ[*NELEM] - 1); // Size based on last XADJ entry
        std::vector<float> coords(COORDS, COORDS + *NELEM * 3);     // Assuming 3D coordinates

        if (ncond > 0)
        {
            // xadj was created with 1-based indexing, so we need to convert it to 0-based for C++
            for (int i = 0; i < xadj.size(); i++)
            {
                xadj[i]--; // Convert to 0-based indexing
            }
            for (int i = 0; i < adjncy.size(); i++)
            {
                adjncy[i]--; // Convert to 0-based indexing
            }
            std::vector<std::pair<int, int>> new_edge = augment_graph_connectivity(xadj, adjncy, coords, 4, 5.0); // Example augmentation with max edge ratio of 2.0
            // add edges

            if (!new_edge.empty())
            {
                std::vector<int> new_xadj, new_adjncy;
                add_edges_to_graph(xadj, adjncy, new_edge, new_xadj, new_adjncy);
                xadj = new_xadj;     // Update xadj with new values
                adjncy = new_adjncy; // Update adjncy with new values
            }

            new_edge = augment_graph_connectivity(xadj, adjncy, coords, 4, 5.0); // Example augmentation with max edge ratio of 2.0
            std::cout <<"Should be zero:" << new_edge.size() << " new edges." << std::endl;

            // convert back to 1-based indexing for METIS
            for (int i = 0; i < xadj.size(); i++)
            {
                xadj[i]++; // Convert back to 1-based indexing
            }
            for (int i = 0; i < adjncy.size(); i++)
            {
                adjncy[i]++; // Convert back to 1-based indexing
            }
        }
        // check time
        std::cout<<" augment time:"<<
            std::chrono::duration_cast<std::chrono::seconds>(std::chrono::high_resolution_clock::now() - start).count()
            << " seconds." << std::endl;

        // Allocate memory for temporary partition and best partition
        int *temp_cep = (int *)malloc(*NELEM * sizeof(int));
        int *best_cep = (int *)malloc(*NELEM * sizeof(int));
        int *temp_options = (int *)malloc(40 * sizeof(int));    // METIS options array size
        float *ubvec = (float *)malloc(*NCOND * sizeof(float)); // UBVEC array

        // copy UBVEC to ubvec
        for (int i = 0; i < *NCOND; i++)
        {
            ubvec[i] = UBVEC[i];
        }

        if (!temp_cep || !best_cep || !temp_options)
        {
            if (temp_cep)
                free(temp_cep);
            if (best_cep)
                free(best_cep);
            if (temp_options)
                free(temp_options);
            return -1; // Memory allocation failed
        }

        float best_quality = -1e30f; // Very low initial value
        int best_ierr = -1;

        for (int trial = 0; trial < num_trials; trial++)
        {
            // Copy original options
            memcpy(temp_options, OPTIONS, 40 * sizeof(int));

            // Set different random seed for each trial
            // METIS_OPTION_SEED is typically at index 7
            temp_options[8] = trial + 1; // Different seed for each trial
            // alternate between METIS_PartGraphKway and METIS_PartGraphRecursive

            // Run METIS with current seed using vector data
            auto start_metis = std::chrono::high_resolution_clock::now();
            IERR1 = METIS_PartGraphKway(
                NELEM, NCOND, xadj.data(), adjncy.data(),
                IWD, vsize, ADJWGT2, NNODE, tpwgts,
                ubvec, temp_options, NEC, temp_cep);
            // end chrono
            auto end_metis = std::chrono::high_resolution_clock::now();
            std::cout << "METIS_PartGraphKway took "
                      << std::chrono::duration_cast<std::chrono::seconds>(end_metis - start_metis).count()
                      << " seconds." << std::endl;

            if (IERR1 == 1)
            { // METIS_OK
                // Evaluate partition quality with load balance check

                // start new chrono
                auto start_eval = std::chrono::high_resolution_clock::now();
                std::pair<float, float> quality_pair = evaluate_partition_quality(NELEM, xadj, adjncy, temp_cep,
                                                                                  IWD, NCOND, NNODE, ubvec, coords);
                // end chrono
                auto end_eval = std::chrono::high_resolution_clock::now();
                // write time in seconds to console
                std::cout << "quality evaluation took "
                          << std::chrono::duration_cast<std::chrono::seconds>(end_eval - start_eval).count()
                          << " seconds." << std::endl;

                // Keep this partition if it's the best so far
                float quality = (4.0f * quality_pair.first + quality_pair.second) / 5.0f; // Average of weight balance and volume ratio
                std::cout << "Trial " << trial << ": quality = " << quality
                          << ", weight balance = " << quality_pair.first
                          << ", connectivity = " << quality_pair.second << std::endl;
                if (quality > best_quality && (quality_pair.first > 0.80f || trial == 0))
                {
                    best_quality = quality;
                    best_ierr = IERR1;
                    memcpy(best_cep, temp_cep, *NELEM * sizeof(int));
                }
                // if weight balance is below 0.5, then we exit early, because KWAY will not work
                if (quality_pair.first < 0.5f)
                {
                    break; // Exit early if quality is too low
                }
            }
        }

        // Copy best partition to output
        if (best_ierr == 1)
        {
            memcpy(CEP, best_cep, *NELEM * sizeof(int));
        }

        // Clean up
        free(temp_cep);
        free(best_cep);
        free(temp_options);

        if (vsize != NULL)
            free(vsize);
        if (ADJWGT2 != NULL)
            free(ADJWGT2);
        if (tpwgts != NULL)
            free(tpwgts);
        if (ubvec != NULL)
            free(ubvec);
        // end chrono
        auto end = std::chrono::high_resolution_clock::now();
        // write time in seconds to console
        std::cout << "TOTAL TIME: "
                  << std::chrono::duration_cast<std::chrono::seconds>(end - start).count()
                  << " seconds." << std::endl;
        return best_ierr;
    }

    int cpp_wrap_metis_partgraphrecursive(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                                          int *IWD, int *NNODE,
                                          float *UBVEC, int *OPTIONS, int *NEC, int *CEP, float *COORDS)
    {
        //start chrono
        auto start = std::chrono::high_resolution_clock::now();
        // vertex coordinates is made of [x1, y1, z1, x2, y2, z2, ...]
        int *vsize = NULL;
        int *ADJWGT2 = NULL;
        float *tpwgts = NULL;
        int IERR1;
        int ncond = *NCOND;
        int npart = *NNODE;
        int nelem = *NELEM;
        // Number of trials with different random seeds
        int num_trials = 1; // Adjust as needed
        num_trials = std::min(num_trials, ncond-1);
        num_trials = std::max(num_trials, 1);


        // Create copies of XADJ and ADJNCY as vectors
        std::vector<int> xadj(XADJ, XADJ + *NELEM + 1);             // XADJ has nelem+1 elements
        std::vector<int> adjncy(ADJNCY, ADJNCY + XADJ[*NELEM] - 1); // Size based on last XADJ entry
        std::vector<float> coords(COORDS, COORDS + *NELEM * 3);     // Assuming 3D coordinates

        if (ncond > 0)
        {
            // xadj was created with 1-based indexing, so we need to convert it to 0-based for C++
            for (int i = 0; i < xadj.size(); i++)
            {
                xadj[i]--; // Convert to 0-based indexing
            }
            for (int i = 0; i < adjncy.size(); i++)
            {
                adjncy[i]--; // Convert to 0-based indexing
            }
            std::vector<std::pair<int, int>> new_edge = augment_graph_connectivity(xadj, adjncy, coords, 4, 5.0); // Example augmentation with max edge ratio of 2.0
            // add edges

            if (!new_edge.empty())
            {
                std::vector<int> new_xadj, new_adjncy;
                add_edges_to_graph(xadj, adjncy, new_edge, new_xadj, new_adjncy);
                xadj = new_xadj;     // Update xadj with new values
                adjncy = new_adjncy; // Update adjncy with new values
            }
            // convert back to 1-based indexing for METIS
            for (int i = 0; i < xadj.size(); i++)
            {
                xadj[i]++; // Convert back to 1-based indexing
            }
            for (int i = 0; i < adjncy.size(); i++)
            {
                adjncy[i]++; // Convert back to 1-based indexing
            }
        }

        // Allocate memory for temporary partition and best partition
        int *temp_cep = (int *)malloc(*NELEM * sizeof(int));
        int *best_cep = (int *)malloc(*NELEM * sizeof(int));
        int *temp_options = (int *)malloc(40 * sizeof(int));    // METIS options array size
        float *ubvec = (float *)malloc(*NCOND * sizeof(float)); // UBVEC size is NCOND
        // copy UBVEC to ubvec
        memcpy(ubvec, UBVEC, *NCOND * sizeof(float));
        if (!temp_cep || !best_cep || !temp_options)
        {
            if (temp_cep)
                free(temp_cep);
            if (best_cep)
                free(best_cep);
            if (temp_options)
                free(temp_options);
            return -1; // Memory allocation failed
        }

        float best_quality = -1e30f; // Very low initial value
        int best_ierr = -1;
        {
            const int trial = 1; // For METIS_PartGraphRecursive, we only run one trial
            // Copy original options
            memcpy(temp_options, OPTIONS, 40 * sizeof(int));

            // Set different random seed for each trial
            // METIS_OPTION_SEED is typically at index 7
            temp_options[8] = trial + 1; // Different seed for each trial

            // Run METIS with current seed using vector data
            IERR1 = METIS_PartGraphRecursive(
                NELEM, NCOND, xadj.data(), adjncy.data(),
                IWD, vsize, ADJWGT2, NNODE, tpwgts,
                ubvec, temp_options, NEC, CEP);

//            if (IERR1 == 1)
//            { // METIS_OK
//                // Evaluate partition quality with load balance check
//                std::pair<float, float> quality_pair = evaluate_partition_quality(NELEM, xadj, adjncy, temp_cep,
//                                                                                  IWD, NCOND, NNODE, ubvec, coords);
//
//                float quality = (3.0f * quality_pair.first + quality_pair.second) / 5.0f; // Average of weight balance and volume ratio
//                 std::cout << "Trial " << trial << ": quality = " << quality
//                           << ", weight balance = " << quality_pair.first
//                           << ", connectivity = " << quality_pair.second << std::endl;
//                // Keep this partition if it's the best so far
//                if (quality > best_quality && (quality_pair.first > 0.80f || trial == 0))
//                {
//                    best_quality = quality;
//                    best_ierr = IERR1;
//                    memcpy(best_cep, temp_cep, *NELEM * sizeof(int));
//                }
//            }
        }

 //       // Copy best partition to output
 //       if (best_ierr == 1)
 //       {
 //           memcpy(CEP, best_cep, *NELEM * sizeof(int));
 //       }

        // Clean up
        free(temp_cep);
        free(best_cep);
        free(temp_options);

        if (vsize != NULL)
            free(vsize);
        if (ADJWGT2 != NULL)
            free(ADJWGT2);
        if (tpwgts != NULL)
            free(tpwgts);

        if (ubvec != NULL)
            free(ubvec);

        // end chrono
        auto end = std::chrono::high_resolution_clock::now();
        // write time in seconds to console
        std::cout << "METIS_PartGraphRecursive took "
                  << std::chrono::duration_cast<std::chrono::seconds>(end - start).count()
                  << " seconds." << std::endl;
        return best_ierr;
    }
}