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

/* Focused regression for a connected component with seven zero modes.
 *
 * The synthetic 12-DOF operator contains all six geometric rigid motions
 * plus one internal mechanism.  Its five positive eigenvalues are 100.  A
 * correct rigid-subspace intersection must use the complete seven-vector
 * numerical nullspace and recover all six physical rigid directions.
 */

#include <inttypes.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <mpi.h>
#include <slepceps.h>

#include "../../engine/source/implicit/eig_solver_mpi.h"

void eig_log_line_c(const char text[], int text_length);

void eig_log_line_c(const char text[], int text_length) {
    (void)text;
    (void)text_length;
}

int main(int argc, char **argv) {
    enum { dof_count = 12, lower_count = 66 };
    const double scale = 100.0;
    const double u_raw[6] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
    double u[6];
    double u_norm = 0.0;
    int64_t global_dof[dof_count];
    int global_node[dof_count];
    int global_component[dof_count];
    double global_coordinate[3 * dof_count];
    int k_row[dof_count + 1];
    int k_column[lower_count];
    double k_diagonal[dof_count];
    double k_lower[lower_count];
    int m_row[dof_count + 1];
    int m_column[1] = {1};
    double m_diagonal[dof_count];
    double m_lower[1] = {0.0};
    double eigenvalues[dof_count];
    double unused_vectors[dof_count] = {0.0};
    int local_size = dof_count;
    int global_size = dof_count;
    int owned_size = dof_count;
    int64_t row_start = 0;
    int communicator;
    int nreq = dof_count;
    int range_all = 1;
    int output_rigid_modes = 1;
    int retain_mode_shapes = 0;
    double shift = 0.0;
    double cut_frequency = 10.0;
    double tolerance = 1.0e-8;
    double rigid_tolerance = 1.0e-12;
    double upper_pivot_tolerance = 1.0e-12;
    double positive_pivot_tolerance = 1.0e-12;
    int maximum_iterations = 300;
    int subspace_factor = 2;
    int64_t solver_handle = 0;
    int nfound = 0;
    int info = 0;
    int capacity = dof_count;
    int mpi_size = 0;
    int lower = 0;
    int zero_count = 0;
    int positive_count = 0;

    if (SlepcInitialize(&argc, &argv, NULL, NULL) != PETSC_SUCCESS)
        return EXIT_FAILURE;
    if (MPI_Comm_size(PETSC_COMM_WORLD, &mpi_size) != MPI_SUCCESS ||
        mpi_size != 1) {
        fprintf(stderr, "test_full_nullspace must run on one MPI rank\n");
        SlepcFinalize();
        return EXIT_FAILURE;
    }
    communicator = (int)MPI_Comm_c2f(PETSC_COMM_WORLD);

    for (int axis = 0; axis < 6; ++axis)
        u_norm += u_raw[axis] * u_raw[axis];
    u_norm = sqrt(u_norm);
    for (int axis = 0; axis < 6; ++axis) u[axis] = u_raw[axis] / u_norm;

    k_row[0] = 1;
    for (int row = 0; row < dof_count; ++row) {
        const int row_axis = row % 6;
        const double row_sign = row < 6 ? 1.0 : -1.0;

        global_dof[row] = row;
        global_node[row] = row / 6 + 1;
        global_component[row] = row_axis + 1;
        global_coordinate[3 * row] = 0.0;
        global_coordinate[3 * row + 1] = 0.0;
        global_coordinate[3 * row + 2] = 0.0;
        k_diagonal[row] =
            0.5 * scale * (1.0 - u[row_axis] * u[row_axis]);
        m_diagonal[row] = 1.0;
        m_row[row] = 1;
        for (int column = 0; column < row; ++column) {
            const int column_axis = column % 6;
            const double column_sign = column < 6 ? 1.0 : -1.0;
            const double identity = row_axis == column_axis ? 1.0 : 0.0;

            k_column[lower] = column + 1;
            k_lower[lower] = 0.5 * scale * row_sign * column_sign *
                             (identity - u[row_axis] * u[column_axis]);
            ++lower;
        }
        k_row[row + 1] = lower + 1;
    }
    m_row[dof_count] = 1;
    if (lower != lower_count) {
        fprintf(stderr, "incorrect lower triangle size: %d\n", lower);
        SlepcFinalize();
        return EXIT_FAILURE;
    }

    eig_solve_slepc_c_(
        &local_size, &global_size, &owned_size, &row_start, &communicator,
        global_dof, global_node, global_component, global_coordinate, k_row,
        k_column, k_diagonal, k_lower, m_row, m_column, m_diagonal, m_lower,
        &nreq, &range_all, &output_rigid_modes, &retain_mode_shapes, &shift,
        &cut_frequency, &tolerance, &rigid_tolerance,
        &upper_pivot_tolerance, &positive_pivot_tolerance,
        &maximum_iterations, &subspace_factor, &solver_handle, eigenvalues,
        unused_vectors, &nfound, &info);
    if (info != 19 || solver_handle == 0) {
        fprintf(stderr, "solve failed: info=%d, handle=%" PRId64 "\n",
                info, solver_handle);
        SlepcFinalize();
        return EXIT_FAILURE;
    }
    eig_slepc_result_metadata_c_(&solver_handle, &capacity, eigenvalues,
                                 &nfound, &info);
    if (info != 0) {
        fprintf(stderr, "metadata failed: info=%d\n", info);
        eig_slepc_result_release_c_(&solver_handle, &info);
        SlepcFinalize();
        return EXIT_FAILURE;
    }
    for (int mode = 0; mode < nfound; ++mode) {
        if (fabs(eigenvalues[mode]) <= 1.0e-8)
            ++zero_count;
        else if (fabs(eigenvalues[mode] - scale) <= 1.0e-6 * scale)
            ++positive_count;
    }
    eig_slepc_result_release_c_(&solver_handle, &info);
    SlepcFinalize();
    if (info != 0 || nfound != 11 || zero_count != 6 ||
        positive_count != 5) {
        fprintf(stderr,
                "unexpected spectrum: info=%d, total=%d, rigid=%d, "
                "positive=%d\n",
                info, nfound, zero_count, positive_count);
        return EXIT_FAILURE;
    }
    printf("PASS full numerical nullspace: 7 zero vectors -> 6 rigid + "
           "1 mechanism; 5 positive modes retained\n");
    return EXIT_SUCCESS;
}
