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
#include <unordered_map>
#include <algorithm>
#include <numeric>
#include <cstdio>
#include <limits>

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
        size_t idx;
        for (int e = 0; e < nelem; ++e) {
//!           if (!safe_array_index(e, c, nelem, idx)) {
//!               std::fprintf(stdout, "[METIS coarse] ERROR: Array index overflow for element %d, constraint %d\n", e, c);
//!               return -1;
//!           }
            idx = static_cast<size_t>(e) * ncond + static_cast<size_t>(c);
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
//!               if (!safe_array_index(e, c, nelem, idx)) {
//!                   std::fprintf(stdout, "[METIS coarse] ERROR: Array index overflow for element %d, constraint %d\n", e, c);
//!                   return -1;
//!               }
                w[c] += IWD[idx];
            }
        }
        part_weights.emplace(pid, std::move(w));
    }

    // =========================================================================
    // Step 2: Decide which small parts can be merged
    // =========================================================================

    std::unordered_map<int, bool> part_merged;
    part_merged.reserve(part_to_elements.size());
    
    int n_small_parts = 0;
    int n_merged_parts = 0;

    for (const auto &kv : part_to_elements) {
        int pid = kv.first;
        size_t size = kv.second.size();

        if (size >= static_cast<size_t>(VECTOR_GROUP_SIZE)) {
            part_merged[pid] = false;
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

    // =========================================================================
    // Fallback: if no small parts exist, call METIS directly on original graph
    // =========================================================================

    if (n_small_parts == 0) {
        std::fprintf(stdout, "[METIS coarse] No small parts (< %d elements), using original graph\n",
                     VECTOR_GROUP_SIZE);

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
    
    std::fprintf(stdout, "[METIS coarse] Parts: %zu total, %d small (< %d elements), %d merged, %d rejected (weight constraint)\n",
                 part_to_elements.size(), n_small_parts, VECTOR_GROUP_SIZE, n_merged_parts,
                 n_small_parts - n_merged_parts);
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
    size_t estimated_coarse_vertices = part_to_elements.size() + elements_without_part;
    coarse_to_part.reserve(estimated_coarse_vertices);
    coarse_to_elements.reserve(estimated_coarse_vertices);

    // Process parts (merged and unmerged)
    for (const auto &kv : part_to_elements) {
        int pid = kv.first;
        const auto &elems = kv.second;

        if (part_merged[pid]) {
            // Merge all elements in this part into one coarse vertex
            for (int e : elems) {
                elem_to_coarse[e] = coarse_id;
            }
            coarse_to_part.push_back(pid);
            coarse_to_elements.push_back(elems);
            coarse_id++;
        } else {
            // Each element becomes its own coarse vertex
            for (int e : elems) {
                elem_to_coarse[e] = coarse_id;
                coarse_to_part.push_back(-1);
                coarse_to_elements.push_back({e});
                coarse_id++;
            }
        }
    }

    // Process elements without part assignment
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
            if (cv_e < cv_n)
            { // Only process one direction, skip self-loops automatically
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
//!               if (!safe_array_index(e, c, nelem, idx)) {
//!                   std::fprintf(stdout, "[METIS coarse] ERROR: Array index overflow for element %d, constraint %d\n", e, c);
//!                   return -1;
//!               }
                w += IWD[idx];
            }
            if (w > std::numeric_limits<int>::max() || w < std::numeric_limits<int>::min()) {
                std::fprintf(stdout, "[METIS coarse] WARNING: Weight overflow for coarse vertex %d, constraint %d\n", cv, c);
                w = std::numeric_limits<int>::max();
            }
            size_t weight_idx = static_cast<size_t>(cv) * ncond + static_cast<size_t>(c);
//            if (!safe_array_index(cv, c, n_coarse, weight_idx)) {
//                std::fprintf(stdout, "[METIS coarse] ERROR: Weight array index overflow\n");
//                return -1;
//            }
            coarse_vwgt[weight_idx] = static_cast<int>(w);
        }
    }

    // =========================================================================
    // Step 6: Call METIS on coarse graph
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
        OPTIONS,                  // Use original OPTIONS (with NUMBERING=1)
        &metis_edgecut,
        coarse_partition.data()
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
 * Build a coarse graph by merging small parts, then partition with METIS.
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
    return metis_partition_with_coarsening(
        METIS_PartGraphKway,
        NELEM, NCOND, XADJ, ADJNCY,
        IWD, NNODE,
        UBVEC, OPTIONS, NEC, CEP, part);
}

int wrap_metis_partgraphrecursive_part(
    int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
    int *IWD, int *NNODE,
    float *UBVEC, int *OPTIONS, int *NEC, int *CEP, int *part)
{
    return metis_partition_with_coarsening(
        METIS_PartGraphRecursive,
        NELEM, NCOND, XADJ, ADJNCY,
        IWD, NNODE,
        UBVEC, OPTIONS, NEC, CEP, part);
}

} // extern "C"