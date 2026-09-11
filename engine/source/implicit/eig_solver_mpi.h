//Copyright>        OpenRadioss
//Copyright>        Copyright (C) 2026 Siemens
//Copyright>
//Copyright>        This program is free software: you can redistribute it and/or modify
//Copyright>        it under the terms of the GNU Affero General Public License as published by
//Copyright>        the Free Software Foundation, either version 3 of the License, or
//Copyright>        (at your option) any later version.
//Copyright>
//Copyright>        This program is distributed in the hope that it will be useful,
//Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>        GNU Affero General Public License for more details.
//Copyright>
//Copyright>        You should have received a copy of the GNU Affero General Public License
//Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>        Commercial Alternative: Simcenter Radioss Software
//Copyright>
//Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
//Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
//Copyright>        commercial version may interest you:
//Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.

/*
 * Backend-neutral distributed sparse support for the public /EIG path.
 *
 * The finite-element layer supplies rank-local symmetric contributions and
 * stable (global node, component) keys.  This layer assigns each unique DOF
 * to exactly one MPI rank, sums duplicate matrix contributions at that owner,
 * and exposes an owned-row CSR operator.  It has no knowledge of elements,
 * constraints, or OpenRadioss data structures.
 */

#ifndef OPENRADIOSS_EIG_SOLVER_MPI_H
#define OPENRADIOSS_EIG_SOLVER_MPI_H

#include <stdint.h>

#if defined(PUBLIC_EIG_MPI)
#include <mpi.h>
#endif

typedef struct {
#if defined(PUBLIC_EIG_MPI)
    MPI_Comm comm;
#endif
    int rank;
    int size;
    int64_t global_size;
    int64_t row_start;
    int local_size;
    int64_t *offsets;
} or_eig_layout;

typedef struct {
    const or_eig_layout *layout;
    int nnz;
    int *row;
    int64_t *column;
    double *value;
} or_eig_dist_matrix;

void eig_build_global_dof_map_c_(int *communicator, int *local_size,
                                  int *global_node, int *component,
                                  int64_t *global_dof, int *global_size,
                                  int *owned_size, int64_t *row_start,
                                  int *info);

int or_eig_layout_init(or_eig_layout *layout, int communicator,
                       int global_size, int owned_size, int64_t row_start);
void or_eig_layout_destroy(or_eig_layout *layout);

int or_eig_dist_matrix_build(or_eig_dist_matrix *matrix,
                             const or_eig_layout *layout, int local_size,
                             const int64_t *global_dof, const int *row,
                             const int *column, const double *diagonal,
                             const double *lower);
void or_eig_dist_matrix_destroy(or_eig_dist_matrix *matrix);
int or_eig_dist_matrix_validate_positive_diagonal(
    const or_eig_dist_matrix *matrix);
int or_eig_expand_owned_vectors(const or_eig_layout *layout, int local_size,
                                const int64_t *global_dof, int vector_count,
                                const double *owned_vectors,
                                double *local_vectors);

#if defined(PUBLIC_EIG_SLEPC)
void eig_solve_slepc_c_(
    int *local_size, int *global_size, int *owned_size, int64_t *row_start,
    int *communicator, int64_t *global_dof, int *global_node,
    int *global_component, double *global_coordinate,
    int *k_row, int *k_col, double *k_diag, double *k_lower,
    int *m_row, int *m_col, double *m_diag, double *m_lower, int *nreq,
    int *range_all, int *output_rigid_modes, int *retain_mode_shapes,
    double *shift,
    double *cut_frequency, double *tolerance, double *rigid_tolerance,
    double *upper_pivot_tolerance, double *positive_pivot_tolerance,
    int *maximum_iterations, int *subspace_factor, int64_t *solver_handle,
    double *evals, double *local_evecs, int *nfound, int *info);
void eig_slepc_result_metadata_c_(int64_t *solver_handle, int *capacity,
                                  double *evals, int *nfound, int *info);
void eig_slepc_result_batch_c_(int64_t *solver_handle, int *first_mode,
                               int *mode_count, double *local_evecs,
                               int *info);
void eig_slepc_result_release_c_(int64_t *solver_handle, int *info);
#endif

#endif
