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
#include <string.h>  // for memcpy
#include <vector>
#include <algorithm>
#include <stack>
#include <cmath>
#include <limits>  // for std::numeric_limits
#include <iostream>
#include <array>
#include "augmentor.h"

// Add bidirectional edges to graph and create new XADJ/ADJNCY arrays
void add_edges_to_graph(const std::vector<int>& original_xadj, 
                       const std::vector<int>& original_adjncy,
                       const std::vector<std::pair<int, int>>& new_edges,
                       std::vector<int>& new_xadj,
                       std::vector<int>& new_adjncy) {
    
    int nelem = original_xadj.size() - 1;  // Number of vertices
    
    // Create adjacency lists for easier manipulation
    std::vector<std::vector<int>> adj_lists(nelem);
    
    // Add existing edges from original graph
    for (int v = 0; v < nelem; v++) {
        int start = original_xadj[v] ;  // Convert to 0-based
        int end = original_xadj[v + 1]; // Convert to 0-based
        
        for (int i = start; i < end; i++) {
            int neighbor = original_adjncy[i] ;  // Convert to 0-based vertex
            adj_lists[v].push_back(neighbor);
        }
    }
    
    // Add new bidirectional edges
    for (const auto& edge : new_edges) {
        int u = edge.first;    // Convert to 0-based
        int v = edge.second;  // Convert to 0-based
        
        // Check bounds
        if (u >= 0 && u < nelem && v >= 0 && v < nelem && u != v) {
            // Add edge u -> v (if not already present)
            if (std::find(adj_lists[u].begin(), adj_lists[u].end(), v) == adj_lists[u].end()) {
                adj_lists[u].push_back(v);
            }
            // Add edge v -> u (if not already present)
            if (std::find(adj_lists[v].begin(), adj_lists[v].end(), u) == adj_lists[v].end()) {
                adj_lists[v].push_back(u);
            }
        }
    }
    
    // Sort adjacency lists for consistency
    for (int v = 0; v < nelem; v++) {
        std::sort(adj_lists[v].begin(), adj_lists[v].end());
    }
    
    // Build new XADJ and ADJNCY arrays
    new_xadj.clear();
    new_adjncy.clear();
    
    new_xadj.reserve(nelem + 1);
    
    int current_pos = 0;  // Start at 1 for Fortran indexing
    new_xadj.push_back(current_pos);
    
    for (int v = 0; v < nelem; v++) {
        for (int neighbor : adj_lists[v]) {
            new_adjncy.push_back(neighbor);  // Convert back to 1-based
        }
        current_pos += adj_lists[v].size();
        new_xadj.push_back(current_pos);
    }
}
void relax_balance_constraints(float *UBVEC, int *NCOND, float relaxation_factor = 0.01f) {
    if (UBVEC == nullptr || NCOND == nullptr) {
        return;
    }
    
    int ncond = *NCOND;
    
    // Skip first constraint (index 0), relax all others
    for (int c = 0; c < ncond; c++) {
        UBVEC[c] +=  relaxation_factor; 

    }
    //print the new UBVEC
//    std::cout << "Relaxed UBVEC: ";
//    for (int c = 0; c < ncond; c++) {
//        std::cout << UBVEC[c] << " ";
//    }
//    std::cout << std::endl; 
}
float compute_weight_balance_ratio(int nelem, int nparts, const std::vector<int>& partition, 
                                  const std::vector<int>& vertex_weights) {
    // Calculate actual weight for each partition
    std::vector<int> partition_weights(nparts + 1, 0);  // +1 because partitions are 1-based
    
    for (int i = 0; i < nelem; i++) {
        int part = partition[i];
        partition_weights[part] += vertex_weights[i];
    }
    
    // Find min and max partition weights
    int min_weight = std::numeric_limits<int>::max();
    int max_weight = 0;
    int avg_weight = 0;
    
    for (int p = 1; p <= nparts; p++) {
        if (partition_weights[p] > 0) {  // Only consider non-empty partitions
            min_weight = std::min(min_weight, partition_weights[p]);
            max_weight = std::max(max_weight, partition_weights[p]);
            avg_weight += partition_weights[p];
        }
    }
    avg_weight /= nparts;  // Average weight across all partitions
    
    // Return ratio w_min / w_max (closer to 1.0 is better balance)
    if (max_weight == 0) {
        return 0.0f;  // Edge case: no valid partitions
    }
    std::cout << "min_weight: " << min_weight << ", max_weight: " << max_weight << ", avg_weight: " << avg_weight << std::endl; 
    return static_cast<float>(avg_weight) / static_cast<float>(max_weight);
}

int count_all_neighboring_partition_pairs(int nelem, const std::vector<int>& xadj, 
                                          const std::vector<int>& adjncy, 
                                          const std::vector<int>& partition) {
    std::unordered_set<uint32_t> neighboring_pairs;
    
    // Process each edge only once
    for (int vertex = 0; vertex < nelem; vertex++) {
        int start = xadj[vertex] - 1;     
        int end = xadj[vertex + 1] - 1;   
        
        for (int i = start; i < end; i++) {
            int neighbor = adjncy[i] - 1;  
            
            // Process each edge only once
            if (vertex < neighbor) {
                unsigned short vertex_partition = static_cast<unsigned short>(partition[vertex]);
                unsigned short neighbor_partition = static_cast<unsigned short>(partition[neighbor]);
                
                if (neighbor_partition != vertex_partition) {
                    // Pack pair into single 32-bit value (smaller partition first)
                    unsigned short p1 = std::min(vertex_partition, neighbor_partition);
                    unsigned short p2 = std::max(vertex_partition, neighbor_partition);
                    uint32_t packed_pair = (static_cast<uint32_t>(p1) << 16) | p2;
                    neighboring_pairs.insert(packed_pair);
                }
            }
        }
    }
    
    return neighboring_pairs.size();
}

// Count connected components in a specific partition using iterative DFS
int count_components_in_partition(int nelem, const std::vector<int>& xadj, 
                                  const std::vector<int>& adjncy, 
                                  const std::vector<int>& partition, int target_partition) {
    std::vector<bool> visited(nelem, false);
    int component_count = 0;
    
    // Check each vertex
    for (int start_vertex = 0; start_vertex < nelem; start_vertex++) {
        // If vertex belongs to target partition and hasn't been visited
        if (partition[start_vertex] == target_partition && !visited[start_vertex]) {
            // Start iterative DFS from this vertex
            std::stack<int> dfs_stack;
            dfs_stack.push(start_vertex);
            
            while (!dfs_stack.empty()) {
                int vertex = dfs_stack.top();
                dfs_stack.pop();
                
                // Skip if already visited
                if (visited[vertex]) {
                    continue;
                }
                
                // Mark as visited
                visited[vertex] = true;
                
                // Add all unvisited neighbors in same partition to stack
                int start = xadj[vertex] - 1;     // Convert to 0-based indexing
                int end = xadj[vertex + 1] - 1;   // Convert to 0-based indexing
                
                for (int i = start; i < end; i++) {
                    int neighbor = adjncy[i] - 1;  // Convert to 0-based vertex index
                    
                    // Add to stack if unvisited and in same partition
                    if (!visited[neighbor] && partition[neighbor] == target_partition) {
                        dfs_stack.push(neighbor);
                    }
                }
            }
            
            component_count++;
        }
    }
    
    return component_count;
}

// Updated evaluation function with mixed quality metric - takes vectors directly
std::pair<float,float> evaluate_partition_quality(int *NELEM, const std::vector<int>& xadj, const std::vector<int>& adjncy, int *CEP, 
                                int *IWD, int *NCOND, int *NNODE, float *UBVEC,  const std::vector<float>& coords ) {
    int nelem = *NELEM;
    int ncond = *NCOND;
    int nparts = *NNODE;
    
    // Convert partition array to vector
    std::vector<int> partition(CEP, CEP + nelem);
    
    // Extract first constraint weights from IWD (first block of nelem weights)
    std::vector<int> vertex_weights(nelem);
    if (IWD != nullptr) {
        for (int i = 0; i < nelem; i++) {
            vertex_weights[i] = IWD[i*ncond];  // First constraint: indices 0 to nelem-1
        }
    } else {
        // If no weights provided, assume uniform weights
        std::fill(vertex_weights.begin(), vertex_weights.end(), 1);
    }
    
    // Find the maximum partition number using C++11 algorithm
    auto max_partition_iter = std::max_element(partition.begin(), partition.end());
    int max_partition = nparts;
    
    // Compute weight balance ratio (w_min / w_max)
    float weight_ratio = compute_weight_balance_ratio(nelem, nparts, partition, vertex_weights);
    
//  int rev_quality = 0;
    // Count disconnected components for each partition
//    for (int p = 1; p <= max_partition; p++) {  // Partitions are 1-based
//        int components = count_components_in_partition(nelem, xadj, adjncy, partition, p);
//        rev_quality += components;
//    }
     
    // Count all neighboring partition pairs
    int rev_quality = 26 * count_all_neighboring_partition_pairs(nelem, xadj, adjncy, partition);
    
    // Compute connectivity quality (inverse of component count)
    float connectivity_quality = (rev_quality == 0) ? 1.0f : (static_cast<float>(nparts) / static_cast<float>(rev_quality));

    // for each partition, compute the volume of the partition
    std::vector<std::array<float, 3>> bounding_box_min ; // = {std::numeric_limits<float>::max(), std::numeric_limits<float>::max(), std::numeric_limits<float>::max()};
    std::vector<std::array<float, 3>> bounding_box_max ;//= {std::numeric_limits<float>::lowest(), std::numeric_limits<float>::lowest(), std::numeric_limits<float>::lowest()};
    // Initialize bounding boxes for each partition
//    bounding_box_min.resize(nparts + 1, {std::numeric_limits<float>::max(), std::numeric_limits<float>::max(), std::numeric_limits<float>::max()});
//    bounding_box_max.resize(nparts + 1, {std::numeric_limits<float>::lowest(), std::numeric_limits<float>::lowest(), std::numeric_limits<float>::lowest()});
    for (int p = 0; p <= max_partition; p++) {
        bounding_box_min.push_back({std::numeric_limits<float>::max(), std::numeric_limits<float>::max(), std::numeric_limits<float>::max()});
        bounding_box_max.push_back({std::numeric_limits<float>::lowest(), std::numeric_limits<float>::lowest(), std::numeric_limits<float>::lowest()});
    }

    for (int i = 0; i < nelem; i++) {
        int part = partition[i] ; // Convert to 0-based index
        if (part > 0 && part <= nparts) {  // Ensure part is valid
            // check that coordinates are valid (bounded by -10e6 and 10e6)
            if (! (coords[i * 3 + 0] < -1e6 || coords[i * 3 + 0] > 1e6 ||
                coords[i * 3 + 1] < -1e6 || coords[i * 3 + 1] > 1e6 ||
                coords[i * 3 + 2] < -1e6 || coords[i * 3 + 2] > 1e6)) {
            // Update bounding box for this partition
            bounding_box_min[part][0] = std::min(bounding_box_min[part][0], coords[i * 3 + 0]);
            bounding_box_min[part][1] = std::min(bounding_box_min[part][1], coords[i * 3 + 1]);
            bounding_box_min[part][2] = std::min(bounding_box_min[part][2], coords[i * 3 + 2]);
            bounding_box_max[part][0] = std::max(bounding_box_max[part][0], coords[i * 3 + 0]);
            bounding_box_max[part][1] = std::max(bounding_box_max[part][1], coords[i * 3 + 1]);
            bounding_box_max[part][2] = std::max(bounding_box_max[part][2], coords[i * 3 + 2]);

            // Update global bounding box
            bounding_box_min[0][0] = std::min(bounding_box_min[0][0], coords[i * 3 + 0]);
            bounding_box_min[0][1] = std::min(bounding_box_min[0][1], coords[i * 3 + 1]);
            bounding_box_min[0][2] = std::min(bounding_box_min[0][2], coords[i * 3 + 2]);
            bounding_box_max[0][0] = std::max(bounding_box_max[0][0], coords[i * 3 + 0]);
            bounding_box_max[0][1] = std::max(bounding_box_max[0][1], coords[i * 3 + 1]);
            bounding_box_max[0][2] = std::max(bounding_box_max[0][2], coords[i * 3 + 2]);
            }
        }
    }

//    double total_volume = static_cast<double>(bounding_box_max[0][0] - bounding_box_min[0][0]);
//    total_volume *= static_cast<double>(bounding_box_max[0][1] - bounding_box_min[0][1]);
//    total_volume *= static_cast<double>(bounding_box_max[0][2] - bounding_box_min[0][2]);
    //std::cout<<"total volume = "<<total_volume<<std::endl;

//    double sum_partition_volume = 0.0;
//    for(int p = 1; p <= max_partition; p++) {  // Partitions are 1-based
//        if (bounding_box_min[p][0] < bounding_box_max[p][0] &&
//            bounding_box_min[p][1] < bounding_box_max[p][1] &&
//            bounding_box_min[p][2] < bounding_box_max[p][2]) {
//            double volume = static_cast<double>(bounding_box_max[p][0] - bounding_box_min[p][0]);
//            volume *= static_cast<double>(bounding_box_max[p][1] - bounding_box_min[p][1]);
//            volume *= static_cast<double>(bounding_box_max[p][2] - bounding_box_min[p][2]);
//            //std::cout<<"partition "<<p<<": volume = "<<volume<<std::endl;
//            sum_partition_volume += volume;
//        }
//    }
    

    
    // Mix weight balance ratio with connectivity quality
    // You can adjust these weights based on importance of each factor
    float weight_importance = 0.5f;  // Weight given to load balancing (0.0 to 1.0)
    float connectivity_importance = 0.0f;  // Weight given to connectivity (0.0 to 1.0)
    float volume_importance = 0.5f;  // Weight given to volume (not used here)

 //   if (sum_partition_volume < 1e-6 || total_volume < 1e-6) {
 //       // Avoid division by zero if no valid partitions
 //       sum_partition_volume = 1.0;
 //       total_volume = 1.0;  // Avoid zero volume
 //   }
    
//    float volume_ratio = static_cast<float>( total_volume / sum_partition_volume);     
    //std::cout<<"weight: "<<weight_ratio<<", connectivity: "<<connectivity_quality<<", volume: "<<volume_ratio<<", mixed: "<<mixed_quality<<std::endl;
    std::cout<<"weight: "<<weight_ratio<<", connectivity: "<<connectivity_quality<<"sum= "<<weight_ratio + connectivity_quality<<std::endl;
    return std::make_pair(weight_ratio, connectivity_quality);

}

extern "C"{
int METIS_PartGraphKway(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                        int *IWD, int *vsize, int *ADJWGT2, int *NNODE, float *tpwgts,
                        float *UBVEC, int *OPTIONS, int *NEC, int *CEP);
int METIS_PartGraphRecursive(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                             int *IWD, int *vsize, int *ADJWGT2, int *NNODE, float *tpwgts,
                             float *UBVEC, int *OPTIONS, int *NEC, int *CEP);
//IERR1 = METIS_SetDefaultOptions(options)
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
    int *vsize = NULL;
    int *ADJWGT2 = NULL;
    float *tpwgts = NULL;
    int IERR1;
    int ncond = *NCOND;
    
    // Number of trials with different random seeds
    const int num_trials = 5;  // Adjust as needed
    
    // Create copies of XADJ and ADJNCY as vectors
    std::vector<int> xadj(XADJ, XADJ + *NELEM + 1);  // XADJ has nelem+1 elements
    std::vector<int> adjncy(ADJNCY, ADJNCY + XADJ[*NELEM] - 1);  // Size based on last XADJ entry
    std::vector<float> coords(COORDS, COORDS + *NELEM * 3);  // Assuming 3D coordinates

    if(ncond > 0)
    {
    //xadj was created with 1-based indexing, so we need to convert it to 0-based for C++
    for (int i = 0; i < xadj.size(); i++) {
        xadj[i]--;  // Convert to 0-based indexing
    }
    for (int i = 0; i < adjncy.size(); i++) {
        adjncy[i]--;  // Convert to 0-based indexing
    }
    std::vector<std::pair<int,int>> new_edge =  augment_graph_connectivity(xadj, adjncy, coords,4, 5.0);  // Example augmentation with max edge ratio of 2.0
    // add edges

    if (!new_edge.empty()) {
        std::vector<int> new_xadj, new_adjncy;
        add_edges_to_graph(xadj, adjncy, new_edge, new_xadj, new_adjncy);
        xadj = new_xadj;  // Update xadj with new values
        adjncy = new_adjncy;  // Update adjncy with new values
    }
    // convert back to 1-based indexing for METIS
    for(int i = 0; i < xadj.size(); i++) {
        xadj[i]++;  // Convert back to 1-based indexing
    }
    for(int i = 0; i < adjncy.size(); i++) {
        adjncy[i]++;  // Convert back to 1-based indexing
    }
    }


    
    // Allocate memory for temporary partition and best partition
    int *temp_cep = (int*)malloc(*NELEM * sizeof(int));
    int *best_cep = (int*)malloc(*NELEM * sizeof(int));
    int *temp_options = (int*)malloc(40 * sizeof(int));  // METIS options array size
    float *ubvec = (float*)malloc(*NCOND * sizeof(float));  // UBVEC array

    //copy UBVEC to ubvec
    for(int i = 0; i < *NCOND; i++) {
        ubvec[i] = UBVEC[i];
    }
    
    if (!temp_cep || !best_cep || !temp_options) {
        if (temp_cep) free(temp_cep);
        if (best_cep) free(best_cep);
        if (temp_options) free(temp_options);
        return -1;  // Memory allocation failed
    }
    
    float best_quality = -1e30f;  // Very low initial value
    int best_ierr = -1;
    
    for (int trial = 0; trial < num_trials; trial++) {
        // Copy original options
        memcpy(temp_options, OPTIONS, 40 * sizeof(int));
        
        // Set different random seed for each trial
        // METIS_OPTION_SEED is typically at index 7
        temp_options[8] = trial + 1;  // Different seed for each trial
        // alternate between METIS_PartGraphKway and METIS_PartGraphRecursive

        if( trial % 3 == 0) {
          IERR1 = METIS_PartGraphRecursive(
            NELEM, NCOND, xadj.data(), adjncy.data(),
            IWD, vsize, ADJWGT2, NNODE, tpwgts,
            ubvec, temp_options, NEC, temp_cep);
        } else { 
        // Run METIS with current seed using vector data
        IERR1 = METIS_PartGraphKway(
            NELEM, NCOND, xadj.data(), adjncy.data(),
            IWD, vsize, ADJWGT2, NNODE, tpwgts,
            ubvec, temp_options, NEC, temp_cep);
        }
        
        if (IERR1 == 1) {  // METIS_OK
            // Evaluate partition quality with load balance check
            std::pair<float,float> quality_pair = evaluate_partition_quality(NELEM, xadj, adjncy, temp_cep, 
                                                      IWD, NCOND, NNODE, ubvec,coords);
           
            
           // Keep this partition if it's the best so far
            float quality = (quality_pair.first + quality_pair.second) / 2.0f;  // Average of weight balance and volume ratio
            if (quality > best_quality && (quality_pair.first > 0.85f||trial==0)) {
                best_quality = quality;
                best_ierr = IERR1;
                memcpy(best_cep, temp_cep, *NELEM * sizeof(int));
            }
        }
    }
    
    // Copy best partition to output
    if (best_ierr == 1) {
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
    
    return best_ierr;
  }
  
  int cpp_wrap_metis_partgraphrecursive(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                                        int *IWD, int *NNODE,
                                        float *UBVEC, int *OPTIONS, int *NEC, int *CEP, float *COORDS)
  {
    // vertex coordinates is made of [x1, y1, z1, x2, y2, z2, ...]
    int *vsize = NULL;
    int *ADJWGT2 = NULL;
    float *tpwgts = NULL;
    int IERR1;
    int ncond = *NCOND;
    int npart = *NNODE;
    int nelem = *NELEM;
//    std::cout<<"RSB"<<std::endl; 
    // Number of trials with different random seeds
    const int num_trials = 2;  // Adjust as needed
    
    // Create copies of XADJ and ADJNCY as vectors
    std::vector<int> xadj(XADJ, XADJ + *NELEM + 1);  // XADJ has nelem+1 elements
    std::vector<int> adjncy(ADJNCY, ADJNCY + XADJ[*NELEM] - 1);  // Size based on last XADJ entry
    std::vector<float> coords(COORDS, COORDS + *NELEM * 3);  // Assuming 3D coordinates

    if(ncond > 0)
    {
    //xadj was created with 1-based indexing, so we need to convert it to 0-based for C++
    for (int i = 0; i < xadj.size(); i++) {
        xadj[i]--;  // Convert to 0-based indexing
    }
    for (int i = 0; i < adjncy.size(); i++) {
        adjncy[i]--;  // Convert to 0-based indexing
    }
    std::vector<std::pair<int,int>> new_edge =  augment_graph_connectivity(xadj, adjncy, coords, 4, 5.0);  // Example augmentation with max edge ratio of 2.0
    // add edges

    if (!new_edge.empty()) {
        std::vector<int> new_xadj, new_adjncy;
        add_edges_to_graph(xadj, adjncy, new_edge, new_xadj, new_adjncy);
        xadj = new_xadj;  // Update xadj with new values
        adjncy = new_adjncy;  // Update adjncy with new values
    }
    // convert back to 1-based indexing for METIS
    for(int i = 0; i < xadj.size(); i++) {
        xadj[i]++;  // Convert back to 1-based indexing
    }
    for(int i = 0; i < adjncy.size(); i++) {
        adjncy[i]++;  // Convert back to 1-based indexing
    }
  }


    // Allocate memory for temporary partition and best partition
    int *temp_cep = (int*)malloc(*NELEM * sizeof(int));
    int *best_cep = (int*)malloc(*NELEM * sizeof(int));
    int *temp_options = (int*)malloc(40 * sizeof(int));  // METIS options array size
    float *ubvec = (float*)malloc(*NCOND * sizeof(float));  // UBVEC size is NCOND    
    // copy UBVEC to ubvec
    memcpy(ubvec, UBVEC, *NCOND * sizeof(float));
    if (!temp_cep || !best_cep || !temp_options) {
        if (temp_cep) free(temp_cep);
        if (best_cep) free(best_cep);
        if (temp_options) free(temp_options);
        return -1;  // Memory allocation failed
    }
    
    float best_quality = -1e30f;  // Very low initial value
    int best_ierr = -1;
//    if(nelem / npart < 10000) {
//        relax_balance_constraints(ubvec, NCOND);
//    }

    
    for (int trial = 0; trial < num_trials; trial++) {
        // Copy original options
        memcpy(temp_options, OPTIONS, 40 * sizeof(int));
        
        // Set different random seed for each trial
        // METIS_OPTION_SEED is typically at index 7
        temp_options[8] = trial + 1;  // Different seed for each trial
        
        // Run METIS with current seed using vector data
        IERR1 = METIS_PartGraphRecursive(
            NELEM, NCOND, xadj.data(), adjncy.data(),
            IWD, vsize, ADJWGT2, NNODE, tpwgts,
            ubvec, temp_options, NEC, temp_cep);
        
        if (IERR1 == 1) {  // METIS_OK
            // Evaluate partition quality with load balance check
            std::pair<float,float> quality_pair = evaluate_partition_quality(NELEM, xadj, adjncy, temp_cep, 
                                                      IWD, NCOND, NNODE, ubvec, coords);
            
            float quality = (quality_pair.first + quality_pair.second) / 2.0f;  // Average of weight balance and volume ratio 
 
            // Keep this partition if it's the best so far
            if (quality > best_quality && (quality_pair.first > 0.85f || trial == 0)) {
                best_quality = quality;
                best_ierr = IERR1;
                memcpy(best_cep, temp_cep, *NELEM * sizeof(int));
            }
        }
    }
    
    // Copy best partition to output
    if (best_ierr == 1) {
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

    if(ubvec != NULL)
      free(ubvec);
    
    return best_ierr;
  }
}