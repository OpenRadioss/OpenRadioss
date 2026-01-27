//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
// C++ wrapper layer for METIS calls.
// Exposes a single unmangled symbol per function so Fortran can bind via BIND(C, name="...").
#include <cstdlib>
#include <cstdint>
#include <vector>
#include <array>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include <numeric>
#include <random>
#include <cstdio>
#include <limits>
#include <cmath>

extern "C" {

int METIS_PartGraphKway(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                        int *IWD, int *vsize, int *ADJWGT2, int *NNODE, float *tpwgts,
                        float *UBVEC, int *OPTIONS, int *NEC, int *CEP);

int METIS_PartGraphRecursive(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                             int *IWD, int *vsize, int *ADJWGT2, int *NNODE, float *tpwgts,
                             float *UBVEC, int *OPTIONS, int *NEC, int *CEP);

int wrap_metis_partgraphkway(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                             int *IWD, int *NNODE,
                             float *UBVEC, int *OPTIONS, int *NEC, int *CEP)
{
  int *vsize = nullptr;
  int *ADJWGT2 = nullptr;
  float *tpwgts = nullptr;
  const int ierr = METIS_PartGraphKway(
    NELEM, NCOND, XADJ, ADJNCY,
    IWD, vsize, ADJWGT2, NNODE, tpwgts,
    UBVEC, OPTIONS, NEC, CEP);

  if (vsize != nullptr) {
    std::free(vsize);
  }
  if (ADJWGT2 != nullptr) {
    std::free(ADJWGT2);
  }
  if (tpwgts != nullptr) {
    std::free(tpwgts);
  }

  return ierr;
}

int wrap_metis_partgraphrecursive(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                                  int *IWD, int *NNODE,
                                  float *UBVEC, int *OPTIONS, int *NEC, int *CEP)
{
  int *vsize = nullptr;
  int *ADJWGT2 = nullptr;
  float *tpwgts = nullptr;

  const int ierr = METIS_PartGraphRecursive(
    NELEM, NCOND, XADJ, ADJNCY,
    IWD, vsize, ADJWGT2, NNODE, tpwgts,
    UBVEC, OPTIONS, NEC, CEP);

  if (vsize != nullptr) {
    std::free(vsize);
  }
  if (ADJWGT2 != nullptr) {
    std::free(ADJWGT2);
  }
  if (tpwgts != nullptr) {
    std::free(tpwgts);
  }

  return ierr;
}


static constexpr int VECTOR_GROUP_SIZE = 128;
static constexpr float MERGE_ALPHA = 0.5f; // Threshold factor for merging parts, merge only if part weight < (ALPHA / npart) of total weight

using metis_part_fn_t = int(*)(int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, int*, int*);

/**
 * Quality metrics for a partition
 */
struct PartitionQuality {
    // Raw metrics
    double max_imbalance_ratio;      // Max partition weight / ideal weight (1.0 = perfect)
    int num_boundary_vertices;       // Elements with cross-partition edges
    int max_neighbors_per_partition; // Max interfaces any partition has
    double avg_neighbors_per_partition; // Average interfaces per partition
    
    // Additional diagnostics
    int64_t max_partition_weight;
    int64_t min_partition_weight;
    int total_cross_edges;           // Total edges crossing partitions
    
    // Composite score (LOWER IS BETTER)
    double score;
    
    void print() const {
        std::fprintf(stdout, "[Quality]   Imbalance ratio:    %.3f (max/min weight)\n", max_imbalance_ratio);
        std::fprintf(stdout, "[Quality]   Boundary vertices:  %d\n", num_boundary_vertices);
        std::fprintf(stdout, "[Quality]   Max neighbors/part: %d\n", max_neighbors_per_partition);
        std::fprintf(stdout, "[Quality]   Avg neighbors/part: %.1f\n", avg_neighbors_per_partition);
        std::fprintf(stdout, "[Quality]   Cross-partition edges: %d\n", total_cross_edges);
        std::fprintf(stdout, "[Quality]   Weight range: [%lld, %lld]\n", 
                     (long long)min_partition_weight, (long long)max_partition_weight);
        std::fprintf(stdout, "[Quality]   COMPOSITE SCORE: %.3f (lower is better)\n", score);
    }
};

/**
 * Compute partition quality metrics
 */
static PartitionQuality compute_partition_quality(
    int nelem,
    int ncond,
    int npart,
    const int* XADJ,
    const int* ADJNCY,
    const int* IWD,
    const int* CEP,
    double weight_balance = 10.0,
    double weight_boundary = 1.0,
    double weight_neighbors = 10.0)
{
    PartitionQuality quality;
    
    // =========================================================================
    // 1. Compute partition weights and balance
    // =========================================================================
    
    std::vector<int64_t> partition_weight(npart, 0);
    int64_t total_weight = 0;
    
    for (int e = 0; e < nelem; ++e) {
        int pid = CEP[e] - 1;  // Convert to 0-indexed
        if (pid < 0 || pid >= npart) {
            std::fprintf(stdout, "[Quality] WARNING: Invalid partition ID %d for element %d\n", 
                         CEP[e], e);
            continue;
        }
        
        // Use first constraint for balance calculation
        int64_t w = IWD[e * ncond];
        partition_weight[pid] += w;
        total_weight += w;
    }
    
    double ideal_weight = static_cast<double>(total_weight) / npart;
    
    quality.max_partition_weight = *std::max_element(partition_weight.begin(), partition_weight.end());
    quality.min_partition_weight = quality.max_partition_weight;
    for (int64_t w : partition_weight) {
        if (w < quality.min_partition_weight) quality.min_partition_weight = w;
    }
    //avoid division by zero
    if(quality.min_partition_weight == 0) {quality.min_partition_weight = 1;}
    quality.max_imbalance_ratio = static_cast<double>(quality.max_partition_weight) / quality.min_partition_weight;
    
    // =========================================================================
    // 2. Count boundary vertices and cross-partition edges
    // =========================================================================
    
    std::vector<bool> is_boundary(nelem, false);
    quality.total_cross_edges = 0;
    
    for (int e = 0; e < nelem; ++e) {
        int my_part = CEP[e];
        
        int adj_start = XADJ[e] - 1;      // Convert from 1-indexed
        int adj_end = XADJ[e + 1] - 1;
        
        for (int idx = adj_start; idx < adj_end; ++idx) {
            int neighbor = ADJNCY[idx] - 1;  // Convert from 1-indexed
            
            if (neighbor < 0 || neighbor >= nelem) continue;
            
            int neighbor_part = CEP[neighbor];
            
            if (my_part != neighbor_part) {
                is_boundary[e] = true;
                quality.total_cross_edges++;
            }
        }
    }
    
    quality.num_boundary_vertices = std::count(is_boundary.begin(), is_boundary.end(), true);
    
    // Each edge is counted twice, so divide by 2
    quality.total_cross_edges /= 2;
    
    // =========================================================================
    // 3. Count neighboring partitions for each partition
    // =========================================================================
    
    std::vector<std::unordered_set<int>> partition_neighbors(npart);
    
    for (int e = 0; e < nelem; ++e) {
        int my_part = CEP[e];
        int my_pid = my_part - 1;  // 0-indexed
        
        if (my_pid < 0 || my_pid >= npart) continue;
        
        int adj_start = XADJ[e] - 1;
        int adj_end = XADJ[e + 1] - 1;
        
        for (int idx = adj_start; idx < adj_end; ++idx) {
            int neighbor = ADJNCY[idx] - 1;
            
            if (neighbor < 0 || neighbor >= nelem) continue;
            
            int neighbor_part = CEP[neighbor];
            int neighbor_pid = neighbor_part - 1;
            
            if (neighbor_pid >= 0 && neighbor_pid < npart && neighbor_part != my_part) {
                partition_neighbors[my_pid].insert(neighbor_part);
            }
        }
    }
    
    quality.max_neighbors_per_partition = 0;
    int total_neighbors = 0;
    
    for (int pid = 0; pid < npart; ++pid) {
        int num_neighbors = static_cast<int>(partition_neighbors[pid].size());
        quality.max_neighbors_per_partition = std::max(quality.max_neighbors_per_partition, num_neighbors);
        total_neighbors += num_neighbors;
    }
    
    quality.avg_neighbors_per_partition = static_cast<double>(total_neighbors) / npart;
    
    // =========================================================================
    // 4. Compute composite score (normalized and weighted)
    // =========================================================================
    
    // Normalize each metric to [0, 1] range (approximately)
    
    // Balance: 1.0 = perfect, higher is worse
    // Map to [0, +Inf) where 0 is perfect
    double balance_score = std::max(0.0, quality.max_imbalance_ratio);
    
    // Boundary: normalize by total elements
    double boundary_score = static_cast<double>(quality.num_boundary_vertices) / nelem;
    
    // Neighbors: normalize by max possible (npart - 1)
    double neighbor_score = static_cast<double>(quality.avg_neighbors_per_partition) / (npart - 1);
    
    // Weighted combination (lower is better)
    quality.score = weight_balance * balance_score +
                    weight_boundary * boundary_score +
                    weight_neighbors * neighbor_score;

    if(balance_score > 1.3) {
        quality.score += 1000.0; // Heavy penalty for highly imbalanced partitions
    }
    
    return quality;
}

/**
 * Helper function to subdivide a large part into sub-partitions of ~128 elements each.
 * Uses METIS kway partitioning (unweighted).
 * 
 * Returns a vector mapping each element in the part to its sub-partition ID (0-based).
 */
static std::vector<int> subdivide_large_part(
    const std::vector<int>& elements,
    int nelem,
    const int* XADJ,
    const int* ADJNCY,
    int* OPTIONS)
{
    const int part_size = static_cast<int>(elements.size());
    const int n_sub_parts = static_cast<int>(std::ceil(static_cast<double>(part_size) / VECTOR_GROUP_SIZE));
    
    // If we only need 1 sub-partition, no need to call METIS - just assign all to partition 1
    if (n_sub_parts <= 1) {
        std::vector<int> sub_partition(part_size, 1);  // All elements in partition 1 (1-indexed)
        return sub_partition;
    }
    
    // Build element index mapping
    std::unordered_map<int, int> elem_to_local;
    elem_to_local.reserve(part_size);
    for (int i = 0; i < part_size; ++i) {
        elem_to_local[elements[i]] = i;
    }
    
    // Build subgraph adjacency structure (1-indexed for METIS)
    std::vector<int> sub_xadj(part_size + 1);
    sub_xadj[0] = 1;  // 1-indexed
    
    std::vector<int> sub_adjncy;
    sub_adjncy.reserve(part_size * 8);  // Rough estimate
    
    for (int i = 0; i < part_size; ++i) {
        int e = elements[i];
        int adj_start = XADJ[e] - 1;      // Convert from 1-indexed
        int adj_end = XADJ[e + 1] - 1;
        
        for (int idx = adj_start; idx < adj_end; ++idx) {
            int neighbor = ADJNCY[idx] - 1;  // Convert from 1-indexed
            
            // Only include edges to elements within this part
            auto it = elem_to_local.find(neighbor);
            if (it != elem_to_local.end()) {
                int local_neighbor = it->second;
                sub_adjncy.push_back(local_neighbor + 1);  // 1-indexed
            }
        }
        
        sub_xadj[i + 1] = static_cast<int>(sub_adjncy.size()) + 1;  // 1-indexed
    }
    
    // Check if subgraph is empty (no internal edges)
    if (sub_adjncy.empty()) {
        std::fprintf(stdout, "[METIS subdivide] WARNING: Part with %d elements has no internal edges, assigning sequentially\n", part_size);
        // Assign elements sequentially to sub-partitions
        std::vector<int> sub_partition(part_size);
        for (int i = 0; i < part_size; ++i) {
            sub_partition[i] = (i / VECTOR_GROUP_SIZE) + 1;  // 1-indexed
        }
        return sub_partition;
    }
    
    // Call METIS kway (unweighted)
    std::vector<int> sub_partition(part_size);
    int sub_ncon = 1;  // No weights
    int sub_edgecut = 0;
    
    int ierr = METIS_PartGraphKway(
        const_cast<int*>(&part_size),
        &sub_ncon,
        sub_xadj.data(),
        sub_adjncy.data(),
        nullptr,      // No vertex weights
        nullptr,      // No vsize
        nullptr,      // No edge weights
        const_cast<int*>(&n_sub_parts),
        nullptr,      // No tpwgts
        nullptr,      // No ubvec needed for unweighted
        OPTIONS,
        &sub_edgecut,
        sub_partition.data()
    );
    
    if (ierr != 1) {
        std::fprintf(stdout, "[METIS subdivide] WARNING: METIS returned error %d for part with %d elements\n",
                     ierr, part_size);
    }
    
    return sub_partition;
}

/**
 * Analyze partition balance and identify problematic partitions.
 * 
 * Returns a vector of partition IDs that are underweight (below 90% of ideal weight).
 */
static std::vector<int> analyze_partition_balance(
    int n_coarse,
    int npart,
    int ncond,
    const std::vector<int>& coarse_partition,
    const std::vector<int>& coarse_vwgt,
    const std::vector<std::vector<int>>& coarse_to_elements,
    const int* part)
{
    // Calculate total weight per constraint
    std::vector<int64_t> total_weight(ncond, 0);
    for (int cv = 0; cv < n_coarse; ++cv) {
        for (int c = 0; c < ncond; ++c) {
            size_t idx = static_cast<size_t>(cv) * ncond + static_cast<size_t>(c);
            total_weight[c] += coarse_vwgt[idx];
        }
    }
    
    // Calculate weight per partition per constraint
    std::vector<std::vector<int64_t>> partition_weight(npart, std::vector<int64_t>(ncond, 0));
    std::vector<int> partition_vertex_count(npart, 0);
    
    for (int cv = 0; cv < n_coarse; ++cv) {
        int pid = coarse_partition[cv] - 1;  // Convert from 1-indexed to 0-indexed
        if (pid < 0 || pid >= npart) {
            std::fprintf(stdout, "[METIS analysis] ERROR: Invalid partition ID %d for coarse vertex %d\n",
                         coarse_partition[cv], cv);
            continue;
        }
        
        partition_vertex_count[pid]++;
        for (int c = 0; c < ncond; ++c) {
            size_t idx = static_cast<size_t>(cv) * ncond + static_cast<size_t>(c);
            partition_weight[pid][c] += coarse_vwgt[idx];
        }
    }
    
    // Calculate ideal weight per partition
    std::vector<double> ideal_weight(ncond);
    for (int c = 0; c < ncond; ++c) {
        ideal_weight[c] = static_cast<double>(total_weight[c]) / npart;
    }
    
    // Analyze balance for first constraint (most important)
    std::fprintf(stdout, "\n[METIS analysis] ========== PARTITION BALANCE ANALYSIS ==========\n");
    std::fprintf(stdout, "[METIS analysis] Total weight (constraint 0): %lld\n", 
                 static_cast<long long>(total_weight[0]));
    std::fprintf(stdout, "[METIS analysis] Ideal weight per partition: %.1f\n", ideal_weight[0]);
    std::fprintf(stdout, "[METIS analysis] Number of partitions: %d\n\n", npart);
    
    // Identify underweight and overweight partitions
    std::vector<int> underweight_partitions;
    std::vector<int> overweight_partitions;
    const double underweight_threshold = 0.90;  // 90% of ideal
    const double overweight_threshold = 1.10;   // 110% of ideal
    
    int64_t min_weight = std::numeric_limits<int64_t>::max();
    int64_t max_weight = 0;
    int min_pid = -1, max_pid = -1;
    
    for (int pid = 0; pid < npart; ++pid) {
        int64_t w = partition_weight[pid][0];
        double ratio = static_cast<double>(w) / ideal_weight[0];
        
        if (w < min_weight) {
            min_weight = w;
            min_pid = pid;
        }
        if (w > max_weight) {
            max_weight = w;
            max_pid = pid;
        }
        
        const char* status = "OK";
        if (ratio < underweight_threshold) {
            status = "UNDERWEIGHT";
            underweight_partitions.push_back(pid);
        } else if (ratio > overweight_threshold) {
            status = "OVERWEIGHT";
            overweight_partitions.push_back(pid);
        }
        
        std::fprintf(stdout, "[METIS analysis] Partition %3d: weight=%8lld (%5.1f%%) vertices=%5d [%s]\n",
                     pid + 1,  // 1-indexed for display
                     static_cast<long long>(w),
                     ratio * 100.0,
                     partition_vertex_count[pid],
                     status);
    }
    
    std::fprintf(stdout, "\n[METIS analysis] Weight range: [%lld, %lld] (%.1fx difference)\n",
                 static_cast<long long>(min_weight),
                 static_cast<long long>(max_weight),
                 static_cast<double>(max_weight) / std::max(min_weight, (int64_t)1));
    
    std::fprintf(stdout, "[METIS analysis] Imbalance: %.2f%% (max/avg - 1)\n",
                 (static_cast<double>(max_weight) / ideal_weight[0] - 1.0) * 100.0);
    
    if (!underweight_partitions.empty()) {
        std::fprintf(stdout, "[METIS analysis] %zu partitions are UNDERWEIGHT (< %.0f%% of ideal)\n",
                     underweight_partitions.size(), underweight_threshold * 100);
    }
    if (!overweight_partitions.empty()) {
        std::fprintf(stdout, "[METIS analysis] %zu partitions are OVERWEIGHT (> %.0f%% of ideal)\n",
                     overweight_partitions.size(), overweight_threshold * 100);
    }
    
    // Analyze connectivity of underweight partitions
    if (!underweight_partitions.empty()) {
        std::fprintf(stdout, "\n[METIS analysis] --- Underweight Partition Connectivity ---\n");
        
        // For underweight partitions, check which original parts they contain
        for (int pid : underweight_partitions) {
            std::unordered_map<int, int> part_count;  // original_part_id -> count of coarse vertices
            
            for (int cv = 0; cv < n_coarse; ++cv) {
                if (coarse_partition[cv] - 1 == pid) {  // This coarse vertex is in this partition
                    if (!coarse_to_elements[cv].empty()) {
                        int original_part = part[coarse_to_elements[cv][0]];
                        if (original_part >= 0) {
                            part_count[original_part]++;
                        }
                    }
                }
            }
            
            std::fprintf(stdout, "[METIS analysis] Partition %d contains parts from: ", pid + 1);
            for (const auto& kv : part_count) {
                std::fprintf(stdout, "part_%d(%d vertices) ", kv.first, kv.second);
            }
            std::fprintf(stdout, "\n");
        }
    }
    
    std::fprintf(stdout, "[METIS analysis] =============================================\n\n");
    
    return underweight_partitions;
}


static int metis_partition_with_coarsening(
    metis_part_fn_t part_fn,
    int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
    int *IWD, int *NNODE,
    float *UBVEC, int *OPTIONS, int *NEC, int *CEP, int *part)
{
    const int nelem = *NELEM;
    const int ncond = *NCOND;
    const int npart = *NNODE;

    // =========================================================================
    // Step 1: Group elements by part ID, compute per-part weights
    // =========================================================================

    std::unordered_map<int, std::vector<int>> part_to_elements;
    part_to_elements.reserve(npart);

    for (int e = 0; e < nelem; ++e) {
        int pid = part[e];
        if (pid < 0) continue;
        part_to_elements[pid].push_back(e);
    }

    std::vector<double> total_weight(ncond, 0.0);
    for (int c = 0; c < ncond; ++c) {
        for (int e = 0; e < nelem; ++e) {
            size_t idx = static_cast<size_t>(e) * ncond + static_cast<size_t>(c);
            total_weight[c] += IWD[idx];
        }
    }

    std::unordered_map<int, std::vector<double>> part_weights;
    part_weights.reserve(part_to_elements.size());

    for (const auto &kv : part_to_elements) {
        int pid = kv.first;
        const auto &elems = kv.second;
        std::vector<double> w(ncond, 0.0);
        
        for (int e : elems) {
            for (int c = 0; c < ncond; ++c) {
                size_t idx = static_cast<size_t>(e) * ncond + static_cast<size_t>(c);
                w[c] += IWD[idx];
            }
        }
        part_weights.emplace(pid, std::move(w));
    }

    // =========================================================================
    // Step 2: Classify parts as small, large, or mergeable
    // =========================================================================

    std::unordered_map<int, bool> part_merged;
    part_merged.reserve(part_to_elements.size());
    
    int n_small_parts = 0;
    int n_merged_parts = 0;
    int n_large_parts = 0;

    for (const auto &kv : part_to_elements) {
        int pid = kv.first;
        size_t size = kv.second.size();

        if (size >= static_cast<size_t>(VECTOR_GROUP_SIZE)) {
            // Large part - will be subdivided
            part_merged[pid] = false;
            n_large_parts++;
            continue;
        }

        n_small_parts++;

        // Check if small part can be merged
        bool can_merge = true;
        const auto &pw = part_weights[pid];
        for (int c = 0; c < ncond; ++c) {
            if (total_weight[c] > 0.0) {
                double ratio = pw[c] / total_weight[c];
                double threshold = MERGE_ALPHA / static_cast<double>(npart);
                if (ratio >= threshold) {
                    can_merge = false;
                    break;
                }
            }
        }
        part_merged[pid] = can_merge;
        if (can_merge) {
            n_merged_parts++;
        }
    }

    // =========================================================================
    // Fallback: if no small parts exist, call METIS directly on original graph
    // =========================================================================

    if (n_small_parts == 0 && n_large_parts == 0) {
        std::fprintf(stdout, "[METIS coarse] No parts found, using original graph\n");

        int *vsize = nullptr;
        int *ADJWGT2 = nullptr;
        float *tpwgts = nullptr;
        return part_fn(
            NELEM, NCOND, XADJ, ADJNCY,
            IWD, vsize, ADJWGT2, NNODE, tpwgts,
            UBVEC, OPTIONS, NEC, CEP);
    }

    size_t total_elements_in_parts = 0;
    for (const auto &kv : part_to_elements) {
        total_elements_in_parts += kv.second.size();
    }
    size_t elements_without_part = static_cast<size_t>(nelem) - total_elements_in_parts;
    
    std::fprintf(stdout, "[METIS coarse] Parts: %zu total (%d large >= %d elements, %d small < %d elements)\n",
                 part_to_elements.size(), n_large_parts, VECTOR_GROUP_SIZE, n_small_parts, VECTOR_GROUP_SIZE);
    std::fprintf(stdout, "[METIS coarse] Small parts: %d merged, %d rejected (weight constraint)\n",
                 n_merged_parts, n_small_parts - n_merged_parts);
    std::fprintf(stdout, "[METIS coarse] Elements: %zu in parts, %zu without part (negative ID)\n",
                 total_elements_in_parts, elements_without_part);

    // =========================================================================
    // Step 3: Build element -> coarse vertex mapping
    // =========================================================================

    std::vector<int> elem_to_coarse(nelem, -1);
    int coarse_id = 0;

    std::vector<int> coarse_to_part;
    std::vector<std::vector<int>> coarse_to_elements;
    
    // Reserve space to avoid reallocations
    size_t estimated_coarse_vertices = part_to_elements.size() * 2 + elements_without_part;
    coarse_to_part.reserve(estimated_coarse_vertices);
    coarse_to_elements.reserve(estimated_coarse_vertices);

    int total_subdivisions = 0;

    // Process parts
    for (const auto &kv : part_to_elements) {
        int pid = kv.first;
        const auto &elems = kv.second;
        size_t part_size = elems.size();

        if (part_size >= static_cast<size_t>(VECTOR_GROUP_SIZE)) {
            // Large part: subdivide into groups of ~128 elements
            std::vector<int> sub_partition = subdivide_large_part(elems, nelem, XADJ, ADJNCY, OPTIONS);
            
            // Group elements by sub-partition
            int n_sub_parts = *std::max_element(sub_partition.begin(), sub_partition.end()) + 1;
            std::vector<std::vector<int>> sub_groups(n_sub_parts);
            
            for (size_t i = 0; i < elems.size(); ++i) {
                int sub_pid = sub_partition[i] - 1;  // Convert from 1-indexed to 0-indexed
                if (sub_pid < 0 || sub_pid >= n_sub_parts) {
                    std::fprintf(stdout, "[METIS coarse] ERROR: Invalid sub-partition ID %d for element %d\n",
                                 sub_pid, elems[i]);
                    continue;
                }
                sub_groups[sub_pid].push_back(elems[i]);
            }
            
            // Create coarse vertices for each sub-group
            for (int sub_pid = 0; sub_pid < n_sub_parts; ++sub_pid) {
                const auto& sub_elems = sub_groups[sub_pid];
                if (sub_elems.empty()) continue;
                
                for (int e : sub_elems) {
                    elem_to_coarse[e] = coarse_id;
                }
                coarse_to_part.push_back(-1);  // Sub-partitions don't have a single part ID
                coarse_to_elements.push_back(sub_elems);
                coarse_id++;
            }
            
            total_subdivisions += n_sub_parts;
            
        } else if (part_merged[pid]) {
            // Small part that can be merged: all elements into one coarse vertex
            for (int e : elems) {
                elem_to_coarse[e] = coarse_id;
            }
            coarse_to_part.push_back(pid);
            coarse_to_elements.push_back(elems);
            coarse_id++;
            
        } else {
            // Small part that cannot be merged: each element becomes its own coarse vertex
            for (int e : elems) {
                elem_to_coarse[e] = coarse_id;
                coarse_to_part.push_back(-1);
                coarse_to_elements.push_back({e});
                coarse_id++;
            }
        }
    }

    // Process elements without part assignment (keep as individual coarse vertices)
    for (int e = 0; e < nelem; ++e) {
        if (part[e] < 0) {
            elem_to_coarse[e] = coarse_id;
            coarse_to_part.push_back(-1);
            coarse_to_elements.push_back({e});
            coarse_id++;
        }
    }

    int n_coarse = coarse_id;

    // Validate that all elements were assigned
    int unassigned = 0;
    for (int e = 0; e < nelem; ++e) {
        if (elem_to_coarse[e] < 0) {
            unassigned++;
        }
    }
    if (unassigned > 0) {
        for (int e = 0; e < nelem; ++e) {
            if (elem_to_coarse[e] < 0) {
                std::fprintf(stdout, "[METIS coarse] ERROR: element %d (part=%d) was not assigned a coarse vertex\n",
                             e, part[e]);
            }
        }
        std::fprintf(stdout, "[METIS coarse] ERROR: %d elements were not assigned coarse vertices\n", unassigned);
        return -1;
    }

    std::fprintf(stdout, "[METIS coarse] Large parts subdivided into %d groups\n", total_subdivisions);
    std::fprintf(stdout, "[METIS coarse] Graph: %d -> %d vertices (%.1f%% reduction)\n",
                 nelem, n_coarse, 100.0 * (1.0 - static_cast<double>(n_coarse) / nelem));

    // =========================================================================
    // Step 4: Build coarse graph adjacency with edge weights
    // =========================================================================

    // Edge structure for sorting
    struct CoarseEdge {
        int cv1;
        int cv2;
        
        bool operator<(const CoarseEdge& other) const {
            if (cv1 != other.cv1) return cv1 < other.cv1;
            return cv2 < other.cv2;
        }
    };

    // Build list of all edges (with duplicates)
    std::vector<CoarseEdge> edge_list;
    size_t estimated_edges = static_cast<size_t>(XADJ[nelem] - XADJ[0]);
    edge_list.reserve(estimated_edges);

    for (int e = 0; e < nelem; ++e) {
        int cv_e = elem_to_coarse[e];
        if (cv_e < 0 || cv_e >= n_coarse) {
            std::fprintf(stdout, "[METIS coarse] ERROR: element %d has invalid coarse vertex %d (n_coarse=%d)\n",
                         e, cv_e, n_coarse);
            continue;
        }

        int adj_start = XADJ[e] - 1;      // Convert from 1-indexed to 0-indexed
        int adj_end = XADJ[e + 1] - 1;

        for (int idx = adj_start; idx < adj_end; ++idx) {
            int neighbor = ADJNCY[idx] - 1;  // Convert from 1-indexed to 0-indexed
            if (neighbor < 0 || neighbor >= nelem) {
                std::fprintf(stdout, "[METIS coarse] ERROR: element %d has invalid neighbor %d (nelem=%d)\n",
                             e, neighbor, nelem);
                continue;
            }
            int cv_n = elem_to_coarse[neighbor];
            if (cv_n < 0 || cv_n >= n_coarse) {
                std::fprintf(stdout, "[METIS coarse] ERROR: neighbor %d has invalid coarse vertex %d (n_coarse=%d)\n",
                             neighbor, cv_n, n_coarse);
                continue;
            }
            if (cv_e < cv_n) { // Only process one direction, skip self-loops automatically
                edge_list.push_back({cv_e, cv_n});
            }
        }
    }

    // Sort edges to group duplicates together
    std::sort(edge_list.begin(), edge_list.end());

    // Count degree of each vertex (for both directions of each edge)
    std::vector<int> degree(n_coarse, 0);
    if (!edge_list.empty()) {
        int current_cv1 = edge_list[0].cv1;
        int current_cv2 = edge_list[0].cv2;
        int weight = 1;
        
        for (size_t i = 1; i <= edge_list.size(); ++i) {
            // Check if we've moved to a new edge or reached the end
            if (i == edge_list.size() || 
                edge_list[i].cv1 != current_cv1 || 
                edge_list[i].cv2 != current_cv2) {
                
                // Store edge for both vertices
                degree[current_cv1]++;
                degree[current_cv2]++;
                
                // Move to next edge if not at end
                if (i < edge_list.size()) {
                    current_cv1 = edge_list[i].cv1;
                    current_cv2 = edge_list[i].cv2;
                    weight = 1;
                }
            } else {
                weight++;
            }
        }
    }

    // Build XADJ array (CSR format)
    std::vector<int> coarse_xadj(n_coarse + 1);
    coarse_xadj[0] = 1;  // 1-indexed start for METIS
    for (int cv = 0; cv < n_coarse; ++cv) {
        int64_t next_val = static_cast<int64_t>(coarse_xadj[cv]) + static_cast<int64_t>(degree[cv]);
        if (next_val > std::numeric_limits<int>::max()) {
            std::fprintf(stdout, "[METIS coarse] ERROR: XADJ array overflow at coarse vertex %d\n", cv);
            return -1;
        }
        coarse_xadj[cv + 1] = static_cast<int>(next_val);
    }

    size_t n_coarse_edges = static_cast<size_t>(coarse_xadj[n_coarse] - 1);
    std::vector<int> coarse_adjncy(n_coarse_edges);
    std::vector<int> coarse_adjwgt(n_coarse_edges);

    // Current write position for each vertex
    std::vector<int> write_pos(n_coarse);
    for (int cv = 0; cv < n_coarse; ++cv) {
        write_pos[cv] = coarse_xadj[cv] - 1;  // Convert to 0-indexed
    }

    // Fill adjacency arrays by iterating through merged edges
    if (!edge_list.empty()) {
        int current_cv1 = edge_list[0].cv1;
        int current_cv2 = edge_list[0].cv2;
        int weight = 1;
        
        for (size_t i = 1; i <= edge_list.size(); ++i) {
            if (i == edge_list.size() || 
                edge_list[i].cv1 != current_cv1 || 
                edge_list[i].cv2 != current_cv2) {
                
                // Write edge in both directions
                coarse_adjncy[write_pos[current_cv1]] = current_cv2 + 1;  // 1-indexed
                coarse_adjwgt[write_pos[current_cv1]] = weight;
                write_pos[current_cv1]++;
                
                coarse_adjncy[write_pos[current_cv2]] = current_cv1 + 1;  // 1-indexed
                coarse_adjwgt[write_pos[current_cv2]] = weight;
                write_pos[current_cv2]++;
                
                if (i < edge_list.size()) {
                    current_cv1 = edge_list[i].cv1;
                    current_cv2 = edge_list[i].cv2;
                    weight = 1;
                }
            } else {
                weight++;
            }
        }
    }

    // =========================================================================
    // Step 5: Build coarse vertex weights
    // =========================================================================

    // Check for potential overflow in weight array size
    int64_t weight_array_size = static_cast<int64_t>(n_coarse) * static_cast<int64_t>(ncond);
    if (weight_array_size > std::numeric_limits<int>::max()) {
        std::fprintf(stdout, "[METIS coarse] ERROR: Coarse weight array size overflow\n");
        return -1;
    }

    std::vector<int> coarse_vwgt(static_cast<size_t>(weight_array_size));
    
    for (int cv = 0; cv < n_coarse; ++cv) {
        const auto &elems = coarse_to_elements[cv];
        for (int c = 0; c < ncond; ++c) {
            int64_t w = 0;
            for (int e : elems) {
                size_t idx = static_cast<size_t>(e) * ncond + static_cast<size_t>(c);
                w += IWD[idx];
            }
            if (w > std::numeric_limits<int>::max() || w < std::numeric_limits<int>::min()) {
                std::fprintf(stdout, "[METIS coarse] WARNING: Weight overflow for coarse vertex %d, constraint %d\n", cv, c);
                w = std::numeric_limits<int>::max();
            }
            size_t weight_idx = static_cast<size_t>(cv) * ncond + static_cast<size_t>(c);
            coarse_vwgt[weight_idx] = static_cast<int>(w);
        }
    }
    // =========================================================================
    // Step 5 bis : Do BFS to count identify the non-contiguous parts
    // =========================================================================

    if (n_coarse > 0) {
        std::vector<char> visited(static_cast<size_t>(n_coarse), 0);
        std::vector<int> queue;
        queue.reserve(static_cast<size_t>(n_coarse));

        int n_components = 0;

        for (int cv = 0; cv < n_coarse; ++cv) {
            if (visited[static_cast<size_t>(cv)] != 0) {
                continue;
            }

            n_components++;
            visited[static_cast<size_t>(cv)] = 1;
            queue.clear();
            queue.push_back(cv);

            for (size_t qi = 0; qi < queue.size(); ++qi) {
                int v = queue[qi];
                int adj_start = coarse_xadj[v] - 1;      // 1-indexed to 0-indexed
                int adj_end = coarse_xadj[v + 1] - 1;

                for (int idx = adj_start; idx < adj_end; ++idx) {
                    int neighbor = coarse_adjncy[static_cast<size_t>(idx)] - 1;
                    if (neighbor < 0 || neighbor >= n_coarse) {
                        continue;
                    }
                    size_t n_idx = static_cast<size_t>(neighbor);
                    if (visited[n_idx] == 0) {
                        visited[n_idx] = 1;
                        queue.push_back(neighbor);
                    }
                }
            }
        }

        std::fprintf(stdout,
                     "[METIS coarse] Coarse graph connectivity: %d connected component(s)\n",
                     n_components);
    } else {
        std::fprintf(stdout,
                     "[METIS coarse] Coarse graph connectivity: 0 connected components (empty graph)\n");
    }
 
    // =========================================================================
    // Step 6: Run METIS on coarse graph
    // =========================================================================

    std::vector<int> coarse_partition(n_coarse);
    int metis_ncon = ncond;
    int metis_nparts = npart;
    int metis_edgecut = 0;
    
    int ierr = part_fn(
        &n_coarse,
        &metis_ncon,
        coarse_xadj.data(),
        coarse_adjncy.data(),
        coarse_vwgt.data(),      // vwgt
        nullptr,                  // vsize
        coarse_adjwgt.data(),    // adjwgt
        &metis_nparts,
        nullptr,                  // tpwgts
        UBVEC,
        OPTIONS,
        &metis_edgecut,
        coarse_partition.data()
    );
    
    if (ierr != 1) {
        std::fprintf(stdout, "[METIS] WARNING: METIS returned error %d\n", ierr);
        return ierr;
    }
    
    // Analyze partition balance
    analyze_partition_balance(
        n_coarse,
        npart,
        ncond,
        coarse_partition,
        coarse_vwgt,
        coarse_to_elements,
        part
    );

    *NEC = metis_edgecut;

    // =========================================================================
    // Step 7: Map coarse partition back to original elements
    // =========================================================================

    for (int e = 0; e < nelem; ++e) {
        int cv = elem_to_coarse[e];
        CEP[e] = coarse_partition[cv];
    }

    return ierr;
}

/**
 * Build coarse graph structure (returned for multiple METIS runs)
 */
struct CoarseGraph {
    int n_coarse;
    std::vector<int> xadj;
    std::vector<int> adjncy;
    std::vector<int> adjwgt;
    std::vector<int> vwgt;
    std::vector<int> elem_to_coarse;
    std::vector<std::vector<int>> coarse_to_elements;
    // 3D coordinates of coarse vertices (if needed)
    std::vector<std::array<float,3>> coords;
    // Connected component ID per coarse vertex (before any connectivity repair)
    std::vector<int> component_id;
    bool valid;
};

/**
 * Build coarse graph ONCE (deterministic, based only on input part array)
 */
static CoarseGraph build_coarse_graph(
    int nelem, int ncond, const int* XADJ, const int* ADJNCY,
    const int* IWD, int npart, const int* part, int* OPTIONS, const float* coords)
{
    // coords are the coordinates of the center of each element (nelem x 3 floats)
    CoarseGraph cg;
    cg.valid = false;
    
    std::fprintf(stdout, "\n[METIS coarse] ========== BUILDING COARSE GRAPH (ONCE) ==========\n");
    
    // =========================================================================
    // Step 1: Group elements by part ID, compute per-part weights
    // =========================================================================

    std::unordered_map<int, std::vector<int>> part_to_elements;
    part_to_elements.reserve(npart);

    for (int e = 0; e < nelem; ++e) {
        int pid = part[e];
        if (pid < 0) continue;
        part_to_elements[pid].push_back(e);
    }

    std::vector<double> total_weight(ncond, 0.0);
    for (int c = 0; c < ncond; ++c) {
        for (int e = 0; e < nelem; ++e) {
            size_t idx = static_cast<size_t>(e) * ncond + static_cast<size_t>(c);
            total_weight[c] += IWD[idx];
        }
    }

    std::unordered_map<int, std::vector<double>> part_weights;
    part_weights.reserve(part_to_elements.size());

    for (const auto &kv : part_to_elements) {
        int pid = kv.first;
        const auto &elems = kv.second;
        std::vector<double> w(ncond, 0.0);
        
        for (int e : elems) {
            for (int c = 0; c < ncond; ++c) {
                size_t idx = static_cast<size_t>(e) * ncond + static_cast<size_t>(c);
                w[c] += IWD[idx];
            }
        }
        part_weights.emplace(pid, std::move(w));
    }

    // =========================================================================
    // Step 2: Classify parts as small, large, or mergeable
    // =========================================================================

    std::unordered_map<int, bool> part_merged;
    part_merged.reserve(part_to_elements.size());
    
    int n_small_parts = 0;
    int n_merged_parts = 0;
    int n_large_parts = 0;

    for (const auto &kv : part_to_elements) {
        int pid = kv.first;
        size_t size = kv.second.size();

        if (size >= static_cast<size_t>(VECTOR_GROUP_SIZE)) {
            part_merged[pid] = false;
            n_large_parts++;
            continue;
        }

        n_small_parts++;

        bool can_merge = true;
        const auto &pw = part_weights[pid];
        for (int c = 0; c < ncond; ++c) {
            if (total_weight[c] > 0.0) {
                double ratio = pw[c] / total_weight[c];
                double threshold = MERGE_ALPHA / static_cast<double>(npart);
                if (ratio >= threshold) {
                    can_merge = false;
                    break;
                }
            }
        }
        part_merged[pid] = can_merge;
        if (can_merge) {
            n_merged_parts++;
        }
    }

    size_t total_elements_in_parts = 0;
    for (const auto &kv : part_to_elements) {
        total_elements_in_parts += kv.second.size();
    }
    size_t elements_without_part = static_cast<size_t>(nelem) - total_elements_in_parts;
    
    std::fprintf(stdout, "[METIS coarse] Parts: %zu total (%d large >= %d elements, %d small < %d elements)\n",
                 part_to_elements.size(), n_large_parts, VECTOR_GROUP_SIZE, n_small_parts, VECTOR_GROUP_SIZE);
    std::fprintf(stdout, "[METIS coarse] Small parts: %d merged, %d rejected (weight constraint)\n",
                 n_merged_parts, n_small_parts - n_merged_parts);
    std::fprintf(stdout, "[METIS coarse] Elements: %zu in parts, %zu without part (negative ID)\n",
                 total_elements_in_parts, elements_without_part);

    // =========================================================================
    // Step 3: Build element -> coarse vertex mapping
    // =========================================================================

    cg.elem_to_coarse.resize(nelem, -1);
    int coarse_id = 0;

    std::vector<int> coarse_to_part;
    
    size_t estimated_coarse_vertices = part_to_elements.size() * 2 + elements_without_part;
    coarse_to_part.reserve(estimated_coarse_vertices);
    cg.coarse_to_elements.reserve(estimated_coarse_vertices);

    int total_subdivisions = 0;

    for (const auto &kv : part_to_elements) {
        int pid = kv.first;
        const auto &elems = kv.second;
        size_t part_size = elems.size();

        if (part_size >= static_cast<size_t>(VECTOR_GROUP_SIZE)) {
            std::vector<int> sub_partition = subdivide_large_part(elems, nelem, XADJ, ADJNCY, OPTIONS);
            
            int n_sub_parts = *std::max_element(sub_partition.begin(), sub_partition.end()) + 1;
            std::vector<std::vector<int>> sub_groups(n_sub_parts);
            
            for (size_t i = 0; i < elems.size(); ++i) {
                int sub_pid = sub_partition[i] - 1;
                if (sub_pid < 0 || sub_pid >= n_sub_parts) continue;
                sub_groups[sub_pid].push_back(elems[i]);
            }
            
            for (int sub_pid = 0; sub_pid < n_sub_parts; ++sub_pid) {
                const auto& sub_elems = sub_groups[sub_pid];
                if (sub_elems.empty()) continue;
                
                for (int e : sub_elems) {
                    cg.elem_to_coarse[e] = coarse_id;
                }
                coarse_to_part.push_back(-1);
                cg.coarse_to_elements.push_back(sub_elems);
                coarse_id++;
            }
            
            total_subdivisions += n_sub_parts;
            
        } else if (part_merged[pid]) {
            for (int e : elems) {
                cg.elem_to_coarse[e] = coarse_id;
            }
            coarse_to_part.push_back(pid);
            cg.coarse_to_elements.push_back(elems);
            coarse_id++;
            
        } else {
            for (int e : elems) {
                cg.elem_to_coarse[e] = coarse_id;
                coarse_to_part.push_back(-1);
                cg.coarse_to_elements.push_back({e});
                coarse_id++;
            }
        }
    }

    for (int e = 0; e < nelem; ++e) {
        if (part[e] < 0) {
            cg.elem_to_coarse[e] = coarse_id;
            coarse_to_part.push_back(-1);
            cg.coarse_to_elements.push_back({e});
            coarse_id++;
        }
    }

    cg.n_coarse = coarse_id;

    std::fprintf(stdout, "[METIS coarse] Large parts subdivided into %d groups\n", total_subdivisions);
    std::fprintf(stdout, "[METIS coarse] Graph: %d -> %d vertices (%.1f%% reduction)\n",
                 nelem, cg.n_coarse, 100.0 * (1.0 - static_cast<double>(cg.n_coarse) / nelem));

    // =========================================================================
    // Step 4: Build coarse graph adjacency
    // =========================================================================

    struct CoarseEdge {
        int cv1;
        int cv2;
        bool operator<(const CoarseEdge& other) const {
            if (cv1 != other.cv1) return cv1 < other.cv1;
            return cv2 < other.cv2;
        }
    };

    std::vector<CoarseEdge> edge_list;
    size_t estimated_edges = static_cast<size_t>(XADJ[nelem] - XADJ[0]);
    edge_list.reserve(estimated_edges);

    for (int e = 0; e < nelem; ++e) {
        int cv_e = cg.elem_to_coarse[e];
        if (cv_e < 0 || cv_e >= cg.n_coarse) continue;

        int adj_start = XADJ[e] - 1;
        int adj_end = XADJ[e + 1] - 1;

        for (int idx = adj_start; idx < adj_end; ++idx) {
            int neighbor = ADJNCY[idx] - 1;
            if (neighbor < 0 || neighbor >= nelem) continue;
            int cv_n = cg.elem_to_coarse[neighbor];
            if (cv_n < 0 || cv_n >= cg.n_coarse) continue;
            if (cv_e < cv_n) {
                edge_list.push_back({cv_e, cv_n});
            }
        }
    }

    std::sort(edge_list.begin(), edge_list.end());

    std::vector<int> degree(cg.n_coarse, 0);
    if (!edge_list.empty()) {
        int current_cv1 = edge_list[0].cv1;
        int current_cv2 = edge_list[0].cv2;
        
        for (size_t i = 1; i <= edge_list.size(); ++i) {
            if (i == edge_list.size() || 
                edge_list[i].cv1 != current_cv1 || 
                edge_list[i].cv2 != current_cv2) {
                
                degree[current_cv1]++;
                degree[current_cv2]++;
                
                if (i < edge_list.size()) {
                    current_cv1 = edge_list[i].cv1;
                    current_cv2 = edge_list[i].cv2;
                }
            }
        }
    }

    cg.xadj.resize(cg.n_coarse + 1);
    cg.xadj[0] = 1;
    for (int cv = 0; cv < cg.n_coarse; ++cv) {
        cg.xadj[cv + 1] = cg.xadj[cv] + degree[cv];
    }

    size_t n_coarse_edges = static_cast<size_t>(cg.xadj[cg.n_coarse] - 1);
    cg.adjncy.resize(n_coarse_edges);
    cg.adjwgt.resize(n_coarse_edges);

    std::vector<int> write_pos(cg.n_coarse);
    for (int cv = 0; cv < cg.n_coarse; ++cv) {
        write_pos[cv] = cg.xadj[cv] - 1;
    }

    if (!edge_list.empty()) {
        int current_cv1 = edge_list[0].cv1;
        int current_cv2 = edge_list[0].cv2;
        int weight = 1;
        
        for (size_t i = 1; i <= edge_list.size(); ++i) {
            if (i == edge_list.size() || 
                edge_list[i].cv1 != current_cv1 || 
                edge_list[i].cv2 != current_cv2) {
                
                cg.adjncy[write_pos[current_cv1]] = current_cv2 + 1;
                cg.adjwgt[write_pos[current_cv1]] = weight;
                write_pos[current_cv1]++;
                
                cg.adjncy[write_pos[current_cv2]] = current_cv1 + 1;
                cg.adjwgt[write_pos[current_cv2]] = weight;
                write_pos[current_cv2]++;
                
                if (i < edge_list.size()) {
                    current_cv1 = edge_list[i].cv1;
                    current_cv2 = edge_list[i].cv2;
                    weight = 1;
                }
            } else {
                weight++;
            }
        }
    }

    // =========================================================================
    // Step 5: Build coarse vertex weights
    // =========================================================================

    int64_t weight_array_size = static_cast<int64_t>(cg.n_coarse) * static_cast<int64_t>(ncond);
    cg.vwgt.resize(static_cast<size_t>(weight_array_size));
    //allocate cg.coords if coords are provided
    if (coords != nullptr) {
        cg.coords.resize(static_cast<size_t>(cg.n_coarse));
    }
    
    for (int cv = 0; cv < cg.n_coarse; ++cv) {
        const auto &elems = cg.coarse_to_elements[cv];

        if (coords != nullptr) {
            float x_sum = 0.0f;
            float y_sum = 0.0f;
            float z_sum = 0.0f;
            for (int e : elems) {
                x_sum += coords[static_cast<size_t>(e) * 3 + 0];
                y_sum += coords[static_cast<size_t>(e) * 3 + 1];
                z_sum += coords[static_cast<size_t>(e) * 3 + 2];
            }
            size_t n_elems = elems.size();
            if (n_elems > 0) {
                cg.coords[static_cast<size_t>(cv)] = {x_sum / n_elems, y_sum / n_elems, z_sum / n_elems};
            } else {
                cg.coords[static_cast<size_t>(cv)] = {0.0f, 0.0f, 0.0f};
            }
        }

        for (int c = 0; c < ncond; ++c) {
            int64_t w = 0;
            for (int e : elems) {
                size_t idx = static_cast<size_t>(e) * ncond + static_cast<size_t>(c);
                w += IWD[idx];
            }
            if (w > std::numeric_limits<int>::max()) {
                w = std::numeric_limits<int>::max();
            }
            size_t weight_idx = static_cast<size_t>(cv) * ncond + static_cast<size_t>(c);
            cg.vwgt[weight_idx] = static_cast<int>(w);
        }
    }

    // =========================================================================
    // Step 5 bis : Identify non-contiguous parts (connected components)
    //             and connect them using coordinates (weight = 5)
    // =========================================================================

    if (cg.n_coarse > 0) {
        cg.component_id.assign(static_cast<size_t>(cg.n_coarse), -1);
        std::vector<int> queue;
        queue.reserve(static_cast<size_t>(cg.n_coarse));

        int n_components = 0;
        std::vector<int> comp_rep;
        std::vector<int> comp_size;
        std::vector<std::array<float,3>> comp_sum;
        std::vector<std::vector<int>> comp_vertices;
        const bool has_coords = (coords != nullptr) && (cg.coords.size() == static_cast<size_t>(cg.n_coarse));

        for (int cv = 0; cv < cg.n_coarse; ++cv) {
            if (cg.component_id[static_cast<size_t>(cv)] >= 0) {
                continue;
            }

            int comp_id = n_components;
            n_components++;
            comp_rep.push_back(cv);
            comp_size.push_back(0);
            if (has_coords) {
                comp_sum.push_back({0.0f, 0.0f, 0.0f});
            }
            comp_vertices.push_back({});

            cg.component_id[static_cast<size_t>(cv)] = comp_id;
            queue.clear();
            queue.push_back(cv);

            for (size_t qi = 0; qi < queue.size(); ++qi) {
                int v = queue[qi];
                comp_size[static_cast<size_t>(comp_id)]++;
                comp_vertices[static_cast<size_t>(comp_id)].push_back(v);
                if (has_coords) {
                    comp_sum[static_cast<size_t>(comp_id)][0] += cg.coords[static_cast<size_t>(v)][0];
                    comp_sum[static_cast<size_t>(comp_id)][1] += cg.coords[static_cast<size_t>(v)][1];
                    comp_sum[static_cast<size_t>(comp_id)][2] += cg.coords[static_cast<size_t>(v)][2];
                }

                int adj_start = cg.xadj[v] - 1;      // 1-indexed to 0-indexed
                int adj_end = cg.xadj[v + 1] - 1;

                for (int idx = adj_start; idx < adj_end; ++idx) {
                    int neighbor = cg.adjncy[static_cast<size_t>(idx)] - 1;
                    if (neighbor < 0 || neighbor >= cg.n_coarse) {
                        continue;
                    }
                    size_t n_idx = static_cast<size_t>(neighbor);
                    if (cg.component_id[n_idx] < 0) {
                        cg.component_id[n_idx] = comp_id;
                        queue.push_back(neighbor);
                    }
                }
            }
        }

        std::fprintf(stdout,
                     "[METIS coarse] Coarse graph connectivity: %d connected component(s)\n",
                     n_components);

        if (n_components > 1) {
            struct WeightedEdge {
                int v1;
                int v2;
                int w;
            };

            std::vector<WeightedEdge> new_edges;

            if (has_coords) {
                std::vector<std::vector<int>> comp_samples(static_cast<size_t>(n_components));
                std::mt19937 rng(12345);
                for (int cid = 0; cid < n_components; ++cid) {
                    const auto &verts = comp_vertices[static_cast<size_t>(cid)];
                    const size_t sample_count = std::min<size_t>(100, verts.size());
                    if (sample_count == 0) {
                        continue;
                    }
                    if (verts.size() <= sample_count) {
                        comp_samples[static_cast<size_t>(cid)] = verts;
                    } else {
                        // Randomly select 10 vertices per component (deterministic seed)
                        std::vector<int> shuffled = verts;
                        std::shuffle(shuffled.begin(), shuffled.end(), rng);
                        comp_samples[static_cast<size_t>(cid)].assign(shuffled.begin(), shuffled.begin() + sample_count);
                    }
                }

                std::vector<char> in_tree(static_cast<size_t>(n_components), 0);
                in_tree[0] = 1;
                int connected = 1;

                while (connected < n_components) {
                    double best_dist = std::numeric_limits<double>::max();
                    int best_u = -1;
                    int best_v = -1;
                    int best_v1 = -1;
                    int best_v2 = -1;

                    for (int u = 0; u < n_components; ++u) {
                        if (in_tree[static_cast<size_t>(u)] == 0) {
                            continue;
                        }
                        for (int v = 0; v < n_components; ++v) {
                            if (in_tree[static_cast<size_t>(v)] != 0) {
                                continue;
                            }
                            const auto &samples_u = comp_samples[static_cast<size_t>(u)];
                            const auto &samples_v = comp_samples[static_cast<size_t>(v)];
                            if (samples_u.empty() || samples_v.empty()) {
                                continue;
                            }
                            for (int v1 : samples_u) {
                                const auto &c1 = cg.coords[static_cast<size_t>(v1)];
                                for (int v2 : samples_v) {
                                    const auto &c2 = cg.coords[static_cast<size_t>(v2)];
                                    double dx = static_cast<double>(c1[0] - c2[0]);
                                    double dy = static_cast<double>(c1[1] - c2[1]);
                                    double dz = static_cast<double>(c1[2] - c2[2]);
                                    double dist2 = dx * dx + dy * dy + dz * dz;
                                    if (dist2 < best_dist) {
                                        best_dist = dist2;
                                        best_u = u;
                                        best_v = v;
                                        best_v1 = v1;
                                        best_v2 = v2;
                                    }
                                }
                            }
                        }
                    }

                    if (best_u < 0 || best_v < 0 || best_v1 < 0 || best_v2 < 0) {
                        break;
                    }

                    int min_comp_size = std::min(comp_size[static_cast<size_t>(best_u)],
                                                comp_size[static_cast<size_t>(best_v)]);
                    int n_edges = static_cast<int>(std::floor(0.15 * static_cast<double>(min_comp_size)));
                    if (n_edges < 1) {
                        n_edges = 1;
                    }

                    const auto &samples_u = comp_samples[static_cast<size_t>(best_u)];
                    const auto &samples_v = comp_samples[static_cast<size_t>(best_v)];

                    struct PairCandidate {
                        int v1;
                        int v2;
                        double dist2;
                    };

                    std::vector<PairCandidate> candidates;
                    if (!samples_u.empty() && !samples_v.empty()) {
                        candidates.reserve(samples_u.size() * samples_v.size());
                        for (int v1 : samples_u) {
                            const auto &c1 = cg.coords[static_cast<size_t>(v1)];
                            for (int v2 : samples_v) {
                                const auto &c2 = cg.coords[static_cast<size_t>(v2)];
                                double dx = static_cast<double>(c1[0] - c2[0]);
                                double dy = static_cast<double>(c1[1] - c2[1]);
                                double dz = static_cast<double>(c1[2] - c2[2]);
                                double dist2 = dx * dx + dy * dy + dz * dz;
                                candidates.push_back({v1, v2, dist2});
                            }
                        }
                        std::sort(candidates.begin(), candidates.end(),
                                  [](const PairCandidate& a, const PairCandidate& b) {
                                      return a.dist2 < b.dist2;
                                  });
                    }

                    int max_edges = n_edges;
                    if (!candidates.empty()) {
                        max_edges = std::min<int>(n_edges, static_cast<int>(candidates.size()));
                        for (int i = 0; i < max_edges; ++i) {
                            int v1 = candidates[static_cast<size_t>(i)].v1;
                            int v2 = candidates[static_cast<size_t>(i)].v2;
                            if (v1 > v2) {
                                std::swap(v1, v2);
                            }
                            new_edges.push_back({v1, v2, 5});
                        }
                    } else {
                        int v1 = best_v1;
                        int v2 = best_v2;
                        if (v1 > v2) {
                            std::swap(v1, v2);
                        }
                        new_edges.push_back({v1, v2, 5});
                    }

                    in_tree[static_cast<size_t>(best_v)] = 1;
                    connected++;
                }
            } else {
                for (int cid = 1; cid < n_components; ++cid) {
                    int v1 = comp_rep[static_cast<size_t>(cid - 1)];
                    int v2 = comp_rep[static_cast<size_t>(cid)];
                    if (v1 > v2) {
                        std::swap(v1, v2);
                    }
                    new_edges.push_back({v1, v2, 5});
                }
            }

            if (!new_edges.empty()) {
                std::vector<WeightedEdge> edge_list;
                edge_list.reserve(new_edges.size() + static_cast<size_t>(cg.adjncy.size()) / 2);

                for (int v = 0; v < cg.n_coarse; ++v) {
                    int adj_start = cg.xadj[v] - 1;
                    int adj_end = cg.xadj[v + 1] - 1;
                    for (int idx = adj_start; idx < adj_end; ++idx) {
                        int neighbor = cg.adjncy[static_cast<size_t>(idx)] - 1;
                        if (neighbor < 0 || neighbor >= cg.n_coarse) {
                            continue;
                        }
                        if (v < neighbor) {
                            edge_list.push_back({v, neighbor, cg.adjwgt[static_cast<size_t>(idx)]});
                        }
                    }
                }

                edge_list.insert(edge_list.end(), new_edges.begin(), new_edges.end());
                std::sort(edge_list.begin(), edge_list.end(),
                          [](const WeightedEdge& a, const WeightedEdge& b) {
                              if (a.v1 != b.v1) return a.v1 < b.v1;
                              return a.v2 < b.v2;
                          });

                std::vector<int> degree(static_cast<size_t>(cg.n_coarse), 0);
                if (!edge_list.empty()) {
                    int current_v1 = edge_list[0].v1;
                    int current_v2 = edge_list[0].v2;
                    int weight_sum = edge_list[0].w;
                    for (size_t i = 1; i <= edge_list.size(); ++i) {
                        if (i == edge_list.size() ||
                            edge_list[i].v1 != current_v1 ||
                            edge_list[i].v2 != current_v2) {
                            degree[static_cast<size_t>(current_v1)]++;
                            degree[static_cast<size_t>(current_v2)]++;
                            if (i < edge_list.size()) {
                                current_v1 = edge_list[i].v1;
                                current_v2 = edge_list[i].v2;
                                weight_sum = edge_list[i].w;
                            }
                        } else {
                            weight_sum += edge_list[i].w;
                        }
                    }
                }

                cg.xadj.resize(static_cast<size_t>(cg.n_coarse) + 1);
                cg.xadj[0] = 1;
                for (int v = 0; v < cg.n_coarse; ++v) {
                    cg.xadj[static_cast<size_t>(v) + 1] = cg.xadj[static_cast<size_t>(v)] + degree[static_cast<size_t>(v)];
                }

                size_t n_edges = static_cast<size_t>(cg.xadj[static_cast<size_t>(cg.n_coarse)] - 1);
                cg.adjncy.assign(n_edges, 0);
                cg.adjwgt.assign(n_edges, 0);

                std::vector<int> write_pos(static_cast<size_t>(cg.n_coarse));
                for (int v = 0; v < cg.n_coarse; ++v) {
                    write_pos[static_cast<size_t>(v)] = cg.xadj[static_cast<size_t>(v)] - 1;
                }

                if (!edge_list.empty()) {
                    int current_v1 = edge_list[0].v1;
                    int current_v2 = edge_list[0].v2;
                    int weight_sum = edge_list[0].w;
                    for (size_t i = 1; i <= edge_list.size(); ++i) {
                        if (i == edge_list.size() ||
                            edge_list[i].v1 != current_v1 ||
                            edge_list[i].v2 != current_v2) {
                            int pos1 = write_pos[static_cast<size_t>(current_v1)];
                            cg.adjncy[static_cast<size_t>(pos1)] = current_v2 + 1;
                            cg.adjwgt[static_cast<size_t>(pos1)] = weight_sum;
                            write_pos[static_cast<size_t>(current_v1)]++;

                            int pos2 = write_pos[static_cast<size_t>(current_v2)];
                            cg.adjncy[static_cast<size_t>(pos2)] = current_v1 + 1;
                            cg.adjwgt[static_cast<size_t>(pos2)] = weight_sum;
                            write_pos[static_cast<size_t>(current_v2)]++;

                            if (i < edge_list.size()) {
                                current_v1 = edge_list[i].v1;
                                current_v2 = edge_list[i].v2;
                                weight_sum = edge_list[i].w;
                            }
                        } else {
                            weight_sum += edge_list[i].w;
                        }
                    }
                }

                std::fprintf(stdout,
                             "[METIS coarse] Added %zu edge(s) (weight=5) to connect components\n",
                             new_edges.size());
            }
        }
    }

    std::fprintf(stdout, "[METIS coarse] ========== COARSE GRAPH BUILD COMPLETE ==========\n\n");
    
    cg.valid = true;
    return cg;
}

/**
 * Run multiple partitioning trials on the SAME coarse graph
 */
static int run_multiple_trials(
    int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
    int *IWD, int *NNODE,
    float *UBVEC, int *OPTIONS, int *NEC, int *CEP, int *part,
    int n_kway_trials,
    int n_recursive_trials, 
    float *coords)
{
    const int nelem = *NELEM;
    const int ncond = *NCOND;
    const int npart = *NNODE;
    const int total_trials = n_kway_trials + n_recursive_trials;
    
    std::fprintf(stdout, "\n========== MULTI-TRIAL PARTITIONING ==========\n");
    std::fprintf(stdout, "[Multi-trial] Running %d total trials (%d KWAY + %d Recursive)\n",
                 total_trials, n_kway_trials, n_recursive_trials);
    std::fprintf(stdout, "==============================================\n");
    
    // Build coarse graph ONCE
    CoarseGraph cg = build_coarse_graph(nelem, ncond, XADJ, ADJNCY, IWD, npart, part, OPTIONS, coords);
    
    if (!cg.valid) {
        std::fprintf(stdout, "[Multi-trial] ERROR: Failed to build coarse graph\n");
        return -1;
    }
    
    // Save original OPTIONS values
    int original_options_0 = OPTIONS[0];
    int original_options_8 = OPTIONS[8];
    
    // Store best partition
    PartitionQuality best_quality;
    std::vector<int> best_coarse_partition;
    int best_edgecut = 0;
    int best_trial = -1;
    const char* best_method = "";
    int best_ncon_used = ncond;

    bool retry_with_reduced_constraints = false;
    int pass = 0;

    do {
        const int metis_ncon_used = retry_with_reduced_constraints ? (ncond - 1) : ncond;
        float* ubvec_used = UBVEC;

        best_quality.score = std::numeric_limits<double>::max();
        best_coarse_partition.clear();
        best_edgecut = 0;
        best_trial = -1;
        best_method = "";
        best_ncon_used = metis_ncon_used;

        std::fprintf(stdout, "\n[Multi-trial] Constraints used: %d\n", metis_ncon_used);

        // Run all METIS trials on the SAME coarse graph
        for (int trial = 0; trial < total_trials; ++trial) {
            std::fprintf(stdout, "\n========================================\n");
            std::fprintf(stdout, "[Trial %d/%d] ", trial + 1, total_trials);
            
            // Determine method
            metis_part_fn_t part_fn;
            const char* method_name;
            if (trial < n_kway_trials) {
                part_fn = METIS_PartGraphKway;
                method_name = "KWAY";
            } else {
                part_fn = METIS_PartGraphRecursive;
                method_name = "RECURSIVE";
            }
            std::fprintf(stdout, "Method: %s\n", method_name);
            
            // Set different random seed for this trial
            OPTIONS[0] = 1;
            OPTIONS[8] = OPTIONS[8] + trial + 1; // Vary the seed for each trial
            
            std::fprintf(stdout, "[Trial %d/%d] Random seed: %d\n", 
                         trial + 1, total_trials, OPTIONS[8]);
            std::fprintf(stdout, "========================================\n");
            
            // Run METIS on coarse graph
            std::vector<int> coarse_partition(cg.n_coarse);
            int metis_ncon = metis_ncon_used;
            int metis_nparts = npart;
            int edgecut_trial = 0;
            
            int ierr = part_fn(
                &cg.n_coarse,
                &metis_ncon,
                cg.xadj.data(),
                cg.adjncy.data(),
                cg.vwgt.data(),
                nullptr,
                cg.adjwgt.data(),
                &metis_nparts,
                nullptr,
                ubvec_used,
                OPTIONS,
                &edgecut_trial,
                coarse_partition.data()
            );
            
            if (ierr != 1) {
                std::fprintf(stdout, "[Trial %d/%d] FAILED with error code %d\n", 
                             trial + 1, total_trials, ierr);
                continue;
            }
            
            // Map to fine elements for quality evaluation
            std::vector<int> CEP_trial(nelem);
            for (int e = 0; e < nelem; ++e) {
                int cv = cg.elem_to_coarse[e];
                CEP_trial[e] = coarse_partition[cv];
            }
            
            // Compute quality
            PartitionQuality quality = compute_partition_quality(
                nelem, ncond, npart, XADJ, ADJNCY, IWD, CEP_trial.data(),
                10.0, 1.0, 2.0);
            
            std::fprintf(stdout, "\n[Trial %d/%d] %s Results:\n", trial + 1, total_trials, method_name);
            std::fprintf(stdout, "[Trial %d/%d]   Edge cut: %d\n", trial + 1, total_trials, edgecut_trial);
            quality.print();
            
            if (quality.score < best_quality.score) {
                best_quality = quality;
                best_coarse_partition = coarse_partition;
                best_edgecut = edgecut_trial;
                best_trial = trial + 1;
                best_method = method_name;
                std::fprintf(stdout, "[Trial %d/%d] *** NEW BEST PARTITION *** (score: %.3f)\n",
                             trial + 1, total_trials, quality.score);
            } else {
                std::fprintf(stdout, "[Trial %d/%d] Not better than current best (score: %.3f vs %.3f)\n",
                             trial + 1, total_trials, quality.score, best_quality.score);
            }
        }

        if (!retry_with_reduced_constraints && best_quality.score > 1000.0 && ncond > 1) {
            std::fprintf(stdout,
                         "\n[Multi-trial] Best score %.3f > 1000, retrying without last constraint\n",
                         best_quality.score);
            retry_with_reduced_constraints = true;
            pass++;
        } else {
            break;
        }
    } while (pass < 2);
    
    // Restore original OPTIONS
    OPTIONS[0] = original_options_0;
    OPTIONS[8] = original_options_8;
    
    // Check if any trial succeeded
    if (best_trial < 0 || best_coarse_partition.empty()) {
        std::fprintf(stdout, "[Multi-trial] WARNING: All %d trials failed, using fallback round-robin partition\n", total_trials);
        // Fallback: assign elements to partitions in round-robin fashion
        for (int e = 0; e < nelem; ++e) {
            CEP[e] = e % npart;
        }
        *NEC = 0;  // Edge cut is unknown for fallback
        return 1;  // Return success to allow computation to continue
    }
    
    // Map best coarse partition to fine elements
    for (int e = 0; e < nelem; ++e) {
        int cv = cg.elem_to_coarse[e];
        if (cv < 0 || cv >= static_cast<int>(best_coarse_partition.size())) {
            std::fprintf(stdout, "[Multi-trial] WARNING: Invalid coarse vertex %d for element %d, using fallback\n", cv, e);
            CEP[e] = e % npart;
        } else {
            CEP[e] = best_coarse_partition[cv];
        }
    }
    *NEC = best_edgecut;
    
    // Print final summary
    std::fprintf(stdout, "\n\n========================================\n");
    std::fprintf(stdout, "FINAL RESULTS - BEST PARTITION\n");
    std::fprintf(stdout, "========================================\n");
    std::fprintf(stdout, "[Final] Best trial: %d/%d (%s)\n", best_trial, total_trials, best_method);
    std::fprintf(stdout, "[Final] Constraints used: %d\n", best_ncon_used);
    std::fprintf(stdout, "[Final] Edge cut: %d\n", best_edgecut);
    std::fprintf(stdout, "[Final] Quality metrics:\n");
    best_quality.print();
    std::fprintf(stdout, "========================================\n\n");
    
    return 1;
}

/**
 * Build a coarse graph by merging small parts and subdividing large parts, then partition with METIS.
 * Runs multiple trials with both KWAY and Recursive methods, keeps the best result.
 *
 * This function assumes Fortran calling convention with 1-based indexing.
 *
 * Arguments:
 *   NELEM   - number of elements
 *   NCOND   - number of constraints (vertex weight dimensions)
 *   XADJ    - adjacency index array (size NELEM+1, 1-indexed values)
 *   ADJNCY  - adjacency list (1-indexed element IDs)
 *   IWD     - vertex weights                                                                  
 *   NNODE   - number of partitions requested
 *   UBVEC   - imbalance tolerance per constraint (size NCOND)
 *   OPTIONS - METIS options array (must exist)
 *   NEC     - output: edge cut
 *   CEP     - output: partition assignment per element (1 to npart)
 *   part    - input: part ID per element (negative values = element not in any part)
 *
 * Returns: METIS return code (or -1 for internal errors)
 */
int wrap_metis_partgraphkway_part(
    int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
    int *IWD, int *NNODE,
    float *UBVEC, int *OPTIONS, int *NEC, int *CEP, int *part)
{

    int n_recursive_trials = 5;
    int n_kway_trials = 1;
    if(*NCOND < 2) {
        n_recursive_trials = 1;
        n_kway_trials = 1;
    }
    return run_multiple_trials(
        NELEM, NCOND, XADJ, ADJNCY,
        IWD, NNODE,
        UBVEC, OPTIONS, NEC, CEP, part,
        n_kway_trials,
        n_recursive_trials,
        nullptr
    );
}

int wrap_metis_partgraphrecursive_part(
    int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
    int *IWD, int *NNODE,
    float *UBVEC, int *OPTIONS, int *NEC, int *CEP, int *part, float *coords)
{
    // For consistency, also use multi-trial approach
    return run_multiple_trials(
        NELEM, NCOND, XADJ, ADJNCY,
        IWD, NNODE,
        UBVEC, OPTIONS, NEC, CEP, part,
        0,  // n_kway_trials
        5,   // n_recursive_trials
        coords
    );
}

} // extern "C"