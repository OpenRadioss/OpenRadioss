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
 * OpenRadioss public /EIG SLEPc/PETSc adapter.
 *
 * Fixed-count requests use a native target-magnitude Krylov-Schur solve with
 * bounded candidate expansion when numerical-null or guard-only Ritz values
 * occupy the initially requested slots.
 * Frequency ranges use SLEPc's native inertia-validated spectrum slicing.
 * This file only converts matrices, configures the native solver, and copies
 * the result back to OpenRadioss.
 */

#if defined(PUBLIC_EIG_SLEPC)

#include "eig_solver_mpi.h"

#include <inttypes.h>
#include <math.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <slepceps.h>
#include <petscblaslapack.h>

#define OR_EIG_LOG_MESSAGE_CAPACITY 4096
#define OR_EIG_LOG_LINE_CAPACITY 8192
#define OR_EIG_MUMPS_DIAGNOSTIC_COUNT 40

extern void eig_log_line_c(const char text[], int text_length);

/* Keep PETSc's live rank-zero output and the official Radioss output file in
   lockstep.  A few diagnostics are assembled by multiple printf calls, so
   retain fragments until a newline completes the official log record. */
static char or_eig_official_line[OR_EIG_LOG_LINE_CAPACITY];
static size_t or_eig_official_length = 0;

static PetscErrorCode or_eig_log_text(MPI_Comm communicator,
                                      const char *message) {
    PetscMPIInt rank = -1;
    size_t length;
    size_t offset;

    PetscFunctionBegin;
    PetscCheck(message != NULL, communicator, PETSC_ERR_ARG_NULL,
               "a /EIG log message is required");
    PetscCall(PetscPrintf(communicator, "%s", message));
    PetscCallMPI(MPI_Comm_rank(communicator, &rank));
    if (rank != 0) PetscFunctionReturn(PETSC_SUCCESS);

    length = strlen(message);
    for (offset = 0; offset < length; ++offset) {
        if (message[offset] == '\n') {
            if (or_eig_official_length > 0 &&
                or_eig_official_line[or_eig_official_length - 1] == '\r')
                --or_eig_official_length;
            eig_log_line_c(or_eig_official_line,
                           (int)or_eig_official_length);
            or_eig_official_length = 0;
            continue;
        }
        PetscCheck(or_eig_official_length + 1 <
                       sizeof(or_eig_official_line),
                   communicator, PETSC_ERR_ARG_SIZ,
                   "assembled /EIG log line exceeds %zu bytes",
                   sizeof(or_eig_official_line) - 1);
        or_eig_official_line[or_eig_official_length++] = message[offset];
    }
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_logf(MPI_Comm communicator,
                                  const char *format, ...) {
    char message[OR_EIG_LOG_MESSAGE_CAPACITY];
    va_list arguments;
    int formatted;

    PetscFunctionBegin;
    va_start(arguments, format);
    formatted = vsnprintf(message, sizeof(message), format, arguments);
    va_end(arguments);
    PetscCheck(formatted >= 0, communicator, PETSC_ERR_LIB,
               "unable to format /EIG log message");
    PetscCheck((size_t)formatted < sizeof(message), communicator,
               PETSC_ERR_ARG_SIZ, "/EIG log message exceeds %zu bytes",
               sizeof(message) - 1);
    PetscCall(or_eig_log_text(communicator, message));
    PetscFunctionReturn(PETSC_SUCCESS);
}

typedef struct {
    PetscInt local_size;
    PetscInt *global_rows;
    PetscInt references;
} or_eig_component_map;

typedef struct {
    PetscReal value;
    PetscInt component;
    Vec vector;
    or_eig_component_map *map;
} or_eig_component_mode;

typedef struct {
    or_eig_layout layout;
    PetscBool initialized_here;
    PetscBool mode_shapes_retained;
    int input_local_size;
    int64_t *input_global_dof;
    PetscInt rigid_count;
    or_eig_component_mode *rigid_modes;
    PetscInt component_positive_count;
    or_eig_component_mode *component_positive_modes;
    PetscLogDouble timing_started;
} or_eig_result_context;

typedef struct {
    PetscReal tolerance;
    PetscReal rigid_tolerance;
    PetscReal upper_pivot_tolerance;
    PetscReal positive_pivot_tolerance;
    PetscInt maximum_iterations;
    PetscInt subspace_factor;
} or_eig_controls;

typedef struct {
    MPI_Comm communicator;
    PetscInt component;
    PetscReal tolerance;
    PetscLogDouble solve_started;
    PetscLogDouble previous_iteration;
} or_eig_monitor_context;

static PetscBool or_eig_exhaustive_diagnostics = PETSC_FALSE;

static PetscReal or_eig_rigid_overlap_tolerance(
    const or_eig_controls *controls) {
    return PetscMax((PetscReal)100.0 * controls->tolerance,
                    (PetscReal)128.0 * PETSC_MACHINE_EPSILON);
}

typedef struct {
    PetscInt global;
    PetscInt vertex;
} or_eig_global_vertex;

typedef struct {
    int node;
    PetscInt vertex;
} or_eig_node_vertex;

static int or_eig_compare_global_vertex(const void *left,
                                        const void *right) {
    const or_eig_global_vertex *a =
        (const or_eig_global_vertex *)left;
    const or_eig_global_vertex *b =
        (const or_eig_global_vertex *)right;

    return (a->global > b->global) - (a->global < b->global);
}

static int or_eig_compare_node_vertex(const void *left,
                                      const void *right) {
    const or_eig_node_vertex *a = (const or_eig_node_vertex *)left;
    const or_eig_node_vertex *b = (const or_eig_node_vertex *)right;

    if (a->node != b->node) return (a->node > b->node) - (a->node < b->node);
    return (a->vertex > b->vertex) - (a->vertex < b->vertex);
}

static int or_eig_compare_petsc_int(const void *left,
                                    const void *right) {
    const PetscInt a = *(const PetscInt *)left;
    const PetscInt b = *(const PetscInt *)right;

    return (a > b) - (a < b);
}

static PetscInt or_eig_union_find_root(PetscInt *parent, PetscInt vertex) {
    PetscInt root = vertex;

    while (parent[root] != root) root = parent[root];
    while (parent[vertex] != vertex) {
        const PetscInt next = parent[vertex];
        parent[vertex] = root;
        vertex = next;
    }
    return root;
}

static void or_eig_union_vertices(PetscInt *parent, PetscInt left,
                                  PetscInt right) {
    PetscInt left_root = or_eig_union_find_root(parent, left);
    PetscInt right_root = or_eig_union_find_root(parent, right);

    if (left_root == right_root) return;
    if (left_root < right_root)
        parent[right_root] = left_root;
    else
        parent[left_root] = right_root;
}

static PetscInt or_eig_local_vertex(PetscInt global, PetscInt first_owned,
                                    PetscInt owned,
                                    const or_eig_global_vertex *ghost_map,
                                    PetscInt ghost_count) {
    PetscInt low = 0;
    PetscInt high = ghost_count;

    if (global >= first_owned && global < first_owned + owned)
        return global - first_owned;
    while (low < high) {
        const PetscInt middle = low + (high - low) / 2;
        if (ghost_map[middle].global < global)
            low = middle + 1;
        else
            high = middle;
    }
    return low < ghost_count && ghost_map[low].global == global
               ? ghost_map[low].vertex
               : -1;
}

static PetscInt or_eig_sorted_index(PetscInt value,
                                    const PetscInt *sorted,
                                    PetscInt count) {
    PetscInt low = 0;
    PetscInt high = count;

    while (low < high) {
        const PetscInt middle = low + (high - low) / 2;
        if (sorted[middle] < value)
            low = middle + 1;
        else
            high = middle;
    }
    return low < count && sorted[low] == value ? low : -1;
}

/* Emit explicit begin/end boundaries around collective solver phases.  A
   begin line is deliberately flushed before entering a potentially long
   PETSc/SLEPc/MUMPS call so a live log always identifies the active phase. */
static PetscErrorCode or_eig_timing_begin(MPI_Comm communicator,
                                          const char *phase,
                                          PetscLogDouble *started) {
    PetscFunctionBegin;
    PetscCall(PetscTime(started));
    PetscCall(or_eig_logf(communicator,
                          "Public /EIG timing begin: %s\n", phase));
    PetscCall(PetscFFlush(PETSC_STDOUT));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_timing_end(MPI_Comm communicator,
                                        const char *phase,
                                        PetscLogDouble started) {
    PetscLogDouble finished = 0.0;

    PetscFunctionBegin;
    PetscCall(PetscTime(&finished));
    PetscCall(or_eig_logf(
        communicator, "Public /EIG timing end: %s = %.6f s\n", phase,
        (double)(finished - started)));
    PetscCall(PetscFFlush(PETSC_STDOUT));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_initialize_diagnostics(MPI_Comm communicator) {
    const char *environment = getenv("OPENRADIOSS_EIG_DIAGNOSTICS");
    PetscBool option_value = PETSC_FALSE;
    PetscBool option_set = PETSC_FALSE;
    PetscMPIInt rank = -1;
    PetscMPIInt size = 0;
    char processor[MPI_MAX_PROCESSOR_NAME];
    char *all_processors = NULL;
    long local_pid = (long)getpid();
    long *all_pids = NULL;
    int processor_length = 0;
    char petsc_version[256];
    char slepc_version[256];
    char mpi_version[MPI_MAX_LIBRARY_VERSION_STRING];
    int mpi_version_length = 0;

    PetscFunctionBegin;
    if (environment != NULL && environment[0] != '\0' &&
        strcmp(environment, "0") != 0)
        or_eig_exhaustive_diagnostics = PETSC_TRUE;
    PetscCall(PetscOptionsGetBool(NULL, NULL,
                                  "-or_eig_exhaustive_diagnostics",
                                  &option_value, &option_set));
    if (option_set) or_eig_exhaustive_diagnostics = option_value;
    if (!or_eig_exhaustive_diagnostics) PetscFunctionReturn(PETSC_SUCCESS);

    PetscCallMPI(MPI_Comm_rank(communicator, &rank));
    PetscCallMPI(MPI_Comm_size(communicator, &size));
    memset(processor, 0, sizeof(processor));
    PetscCallMPI(MPI_Get_processor_name(processor, &processor_length));
    processor[PetscMin(processor_length, MPI_MAX_PROCESSOR_NAME - 1)] = '\0';
    PetscCall(PetscGetVersion(petsc_version, sizeof(petsc_version)));
    PetscCall(SlepcGetVersion(slepc_version, sizeof(slepc_version)));
    PetscCallMPI(MPI_Get_library_version(mpi_version, &mpi_version_length));
    mpi_version[PetscMin(mpi_version_length,
                         MPI_MAX_LIBRARY_VERSION_STRING - 1)] = '\0';
    if (rank == 0) {
        PetscCall(PetscMalloc1(size, &all_pids));
        PetscCall(PetscMalloc1((size_t)size * MPI_MAX_PROCESSOR_NAME,
                               &all_processors));
    }
    PetscCallMPI(MPI_Gather(&local_pid, 1, MPI_LONG, all_pids, 1, MPI_LONG, 0,
                            communicator));
    PetscCallMPI(MPI_Gather(processor, MPI_MAX_PROCESSOR_NAME, MPI_CHAR,
                            all_processors, MPI_MAX_PROCESSOR_NAME, MPI_CHAR,
                            0, communicator));
    if (rank == 0) {
        for (PetscMPIInt diagnostic_rank = 0; diagnostic_rank < size;
             ++diagnostic_rank) {
            PetscCall(or_eig_logf(
                communicator,
                "Public /EIG diagnostic rank identity: rank=%d/%d, pid=%ld, "
                "processor=%s\n",
                (int)diagnostic_rank, (int)size, all_pids[diagnostic_rank],
                all_processors +
                    (size_t)diagnostic_rank * MPI_MAX_PROCESSOR_NAME));
        }
    }
    PetscCall(PetscFree(all_processors));
    PetscCall(PetscFree(all_pids));
    PetscCall(or_eig_logf(
        communicator,
        "Public /EIG exhaustive diagnostics enabled: %s; %s; MPI=%s\n",
        petsc_version, slepc_version, mpi_version));
    PetscCall(PetscFFlush(PETSC_STDOUT));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_convergence_monitor(
    EPS eps, PetscInt iteration, PetscInt converged, PetscScalar eigen_real[],
    PetscScalar eigen_imaginary[], PetscReal error_estimate[],
    PetscInt estimate_count, PetscCtx raw_context) {
    const PetscReal two_pi =
        (PetscReal)6.283185307179586476925286766559;
    or_eig_monitor_context *context =
        (or_eig_monitor_context *)raw_context;
    PetscLogDouble now = 0.0;
    PetscLogDouble process_current = 0.0;
    PetscLogDouble process_peak = 0.0;
    PetscLogDouble allocator_current = 0.0;
    PetscLogDouble allocator_peak = 0.0;
    PetscLogDouble global_process_current = 0.0;
    PetscLogDouble maximum_process_current = 0.0;
    PetscLogDouble maximum_process_peak = 0.0;
    PetscLogDouble global_allocator_current = 0.0;
    PetscLogDouble global_allocator_peak = 0.0;
    PetscLogDouble maximum_allocator_peak = 0.0;
    PetscInt requested = 0;
    PetscInt subspace = 0;
    PetscInt projected = 0;
    PetscInt mode;

    PetscFunctionBegin;
    (void)eps;
    PetscCall(PetscTime(&now));
    PetscCall(PetscMemoryGetCurrentUsage(&process_current));
    PetscCall(PetscMemoryGetMaximumUsage(&process_peak));
    PetscCall(PetscMallocGetCurrentUsage(&allocator_current));
    PetscCall(PetscMallocGetMaximumUsage(&allocator_peak));
    PetscCallMPI(MPI_Allreduce(&process_current, &global_process_current, 1,
                               MPIU_PETSCLOGDOUBLE, MPI_SUM,
                               context->communicator));
    PetscCallMPI(MPI_Allreduce(&process_current, &maximum_process_current, 1,
                               MPIU_PETSCLOGDOUBLE, MPI_MAX,
                               context->communicator));
    PetscCallMPI(MPI_Allreduce(&process_peak, &maximum_process_peak, 1,
                               MPIU_PETSCLOGDOUBLE, MPI_MAX,
                               context->communicator));
    PetscCallMPI(MPI_Allreduce(&allocator_current,
                               &global_allocator_current, 1,
                               MPIU_PETSCLOGDOUBLE, MPI_SUM,
                               context->communicator));
    PetscCallMPI(MPI_Allreduce(&allocator_peak, &global_allocator_peak, 1,
                               MPIU_PETSCLOGDOUBLE, MPI_SUM,
                               context->communicator));
    PetscCallMPI(MPI_Allreduce(&allocator_peak, &maximum_allocator_peak, 1,
                               MPIU_PETSCLOGDOUBLE, MPI_MAX,
                               context->communicator));
    PetscCall(EPSGetDimensions(eps, &requested, &subspace, &projected));
    PetscCall(or_eig_logf(
        context->communicator,
        "Public /EIG convergence iteration: component=%" PetscInt_FMT
        ", iteration=%" PetscInt_FMT ", elapsed=%.6f s, delta=%.6f s, "
        "converged=%" PetscInt_FMT "/%" PetscInt_FMT
        ", estimates=%" PetscInt_FMT ", subspace=%" PetscInt_FMT
        ", projected=%" PetscInt_FMT
        ", process_rss_global=%.3f MiB, process_rss_max_rank=%.3f MiB, "
        "process_peak_max_rank=%.3f MiB, petsc_alloc_global=%.3f MiB, "
        "petsc_alloc_peak_global=%.3f MiB, "
        "petsc_alloc_peak_max_rank=%.3f MiB\n",
        context->component, iteration,
        (double)(now - context->solve_started),
        (double)(now - context->previous_iteration), converged, requested,
        estimate_count, subspace, projected,
        (double)global_process_current / (1024.0 * 1024.0),
        (double)maximum_process_current / (1024.0 * 1024.0),
        (double)maximum_process_peak / (1024.0 * 1024.0),
        (double)global_allocator_current / (1024.0 * 1024.0),
        (double)global_allocator_peak / (1024.0 * 1024.0),
        (double)maximum_allocator_peak / (1024.0 * 1024.0)));
    for (mode = 0; mode < estimate_count; ++mode) {
        const PetscReal value = PetscRealPart(eigen_real[mode]);
        const PetscReal imaginary =
            eigen_imaginary != NULL ? PetscRealPart(eigen_imaginary[mode])
                                    : (PetscReal)0.0;
        PetscReal signed_frequency = (PetscReal)0.0;

        if (value != 0.0)
            signed_frequency = PetscSqrtReal(PetscAbsReal(value)) / two_pi;
        if (value < 0.0) signed_frequency = -signed_frequency;
        PetscCall(or_eig_logf(
            context->communicator,
            "Public /EIG convergence mode: component=%" PetscInt_FMT
            ", iteration=%" PetscInt_FMT ", mode=%" PetscInt_FMT
            "/%" PetscInt_FMT ", eigenvalue_real=%.16e, "
            "eigenvalue_imaginary=%.16e, signed_frequency_hz=%.16e, "
            "ritz_error_estimate=%.16e, tolerance=%.16e, "
            "tolerance_ratio=%.16e, converged_prefix=%s\n",
            context->component, iteration, mode + 1, estimate_count,
            (double)value, (double)imaginary, (double)signed_frequency,
            (double)error_estimate[mode], (double)context->tolerance,
            (double)(error_estimate[mode] / context->tolerance),
            mode < converged ? "yes" : "no"));
    }
    context->previous_iteration = now;
    PetscCall(PetscFFlush(PETSC_STDOUT));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_create_matrix(
    const or_eig_dist_matrix *source, PetscBool positive_definite,
    Mat *matrix) {
    const or_eig_layout *layout = source->layout;
    PetscInt *diagonal_nnz = NULL;
    PetscInt *off_diagonal_nnz = NULL;
    PetscInt *columns = NULL;
    PetscScalar *values = NULL;
    PetscInt maximum_row_nnz = 0;
    MatInfo matrix_info;
    PetscLogDouble memory_before = 0.0;
    PetscLogDouble memory_after = 0.0;
    PetscLogDouble local_matrix_memory = 0.0;
    PetscLogDouble global_matrix_memory = 0.0;
    PetscLogDouble maximum_rank_matrix_memory = 0.0;
    const char *matrix_name =
        positive_definite ? "mass matrix" : "stiffness matrix";
    PetscLogDouble phase_started = 0.0;
    PetscErrorCode ierr;
    int row;

    PetscFunctionBegin;
    *matrix = NULL;
    PetscCall(PetscMallocGetCurrentUsage(&memory_before));
    ierr = or_eig_timing_begin(layout->comm,
                               positive_definite
                                   ? "mass matrix preallocation scan"
                                   : "stiffness matrix preallocation scan",
                               &phase_started);
    if (ierr != PETSC_SUCCESS) PetscFunctionReturn(ierr);
    ierr = PetscCalloc1((PetscInt)layout->local_size, &diagonal_nnz);
    if (ierr != PETSC_SUCCESS) PetscFunctionReturn(ierr);
    ierr = PetscCalloc1((PetscInt)layout->local_size, &off_diagonal_nnz);
    if (ierr != PETSC_SUCCESS) {
        PetscCall(PetscFree(diagonal_nnz));
        PetscFunctionReturn(ierr);
    }
    for (row = 0; row < layout->local_size; ++row) {
        const PetscInt count =
            (PetscInt)(source->row[row + 1] - source->row[row]);
        int entry;
        maximum_row_nnz = PetscMax(maximum_row_nnz, count);
        for (entry = source->row[row]; entry < source->row[row + 1];
             ++entry) {
            if (source->column[entry] >= layout->row_start &&
                source->column[entry] <
                    layout->row_start + layout->local_size)
                ++diagonal_nnz[row];
            else
                ++off_diagonal_nnz[row];
        }
    }
    ierr = or_eig_timing_end(layout->comm,
                             positive_definite
                                 ? "mass matrix preallocation scan"
                                 : "stiffness matrix preallocation scan",
                             phase_started);
    if (ierr != PETSC_SUCCESS) goto fail;
    ierr = or_eig_timing_begin(
        layout->comm,
        positive_definite ? "PETSc mass matrix allocation"
                          : "PETSc stiffness matrix allocation",
        &phase_started);
    if (ierr != PETSC_SUCCESS) goto fail;
    ierr = MatCreateAIJ(layout->comm, (PetscInt)layout->local_size,
                        (PetscInt)layout->local_size,
                        (PetscInt)layout->global_size,
                        (PetscInt)layout->global_size, 0, diagonal_nnz, 0,
                        off_diagonal_nnz, matrix);
    PetscCall(PetscFree(off_diagonal_nnz));
    PetscCall(PetscFree(diagonal_nnz));
    if (ierr != PETSC_SUCCESS) PetscFunctionReturn(ierr);
    ierr = MatSetOption(*matrix, MAT_NEW_NONZERO_ALLOCATION_ERR, PETSC_TRUE);
    if (ierr != PETSC_SUCCESS) goto fail;
    ierr = PetscMalloc1(PetscMax(maximum_row_nnz, 1), &columns);
    if (ierr != PETSC_SUCCESS) goto fail;
    ierr = PetscMalloc1(PetscMax(maximum_row_nnz, 1), &values);
    if (ierr != PETSC_SUCCESS) goto fail;
    ierr = or_eig_timing_end(
        layout->comm,
        positive_definite ? "PETSc mass matrix allocation"
                          : "PETSc stiffness matrix allocation",
        phase_started);
    if (ierr != PETSC_SUCCESS) goto fail;
    ierr = or_eig_timing_begin(
        layout->comm,
        positive_definite ? "mass matrix value insertion"
                          : "stiffness matrix value insertion",
        &phase_started);
    if (ierr != PETSC_SUCCESS) goto fail;
    for (row = 0; row < layout->local_size; ++row) {
        const PetscInt matrix_row = (PetscInt)(layout->row_start + row);
        const int count = source->row[row + 1] - source->row[row];
        int entry;

        if (count == 0) continue;
        for (entry = 0; entry < count; ++entry) {
            const int source_entry = source->row[row] + entry;
            columns[entry] = (PetscInt)source->column[source_entry];
            values[entry] = (PetscScalar)source->value[source_entry];
        }
        ierr = MatSetValues(*matrix, 1, &matrix_row, (PetscInt)count,
                            columns, values, INSERT_VALUES);
        if (ierr != PETSC_SUCCESS) goto fail;
    }
    ierr = or_eig_timing_end(
        layout->comm,
        positive_definite ? "mass matrix value insertion"
                          : "stiffness matrix value insertion",
        phase_started);
    if (ierr != PETSC_SUCCESS) goto fail;
    PetscCall(PetscFree(values));
    PetscCall(PetscFree(columns));
    ierr = or_eig_timing_begin(
        layout->comm,
        positive_definite ? "mass matrix collective assembly"
                          : "stiffness matrix collective assembly",
        &phase_started);
    if (ierr != PETSC_SUCCESS) goto fail;
    ierr = MatAssemblyBegin(*matrix, MAT_FINAL_ASSEMBLY);
    if (ierr != PETSC_SUCCESS) goto fail;
    ierr = MatAssemblyEnd(*matrix, MAT_FINAL_ASSEMBLY);
    if (ierr != PETSC_SUCCESS) goto fail;
    ierr = or_eig_timing_end(
        layout->comm,
        positive_definite ? "mass matrix collective assembly"
                          : "stiffness matrix collective assembly",
        phase_started);
    if (ierr != PETSC_SUCCESS) goto fail;
    ierr = MatSetOption(*matrix, MAT_SYMMETRIC, PETSC_TRUE);
    if (ierr != PETSC_SUCCESS) goto fail;
    if (positive_definite) {
        ierr = MatSetOption(*matrix, MAT_SPD, PETSC_TRUE);
        if (ierr != PETSC_SUCCESS) goto fail;
    }
    ierr = MatGetInfo(*matrix, MAT_GLOBAL_SUM, &matrix_info);
    if (ierr != PETSC_SUCCESS) goto fail;
    ierr = PetscMallocGetCurrentUsage(&memory_after);
    if (ierr != PETSC_SUCCESS) goto fail;
    local_matrix_memory = PetscMax(memory_after - memory_before, 0.0);
    ierr = MPI_Allreduce(&local_matrix_memory, &global_matrix_memory, 1,
                         MPIU_PETSCLOGDOUBLE, MPI_SUM, layout->comm);
    if (ierr != MPI_SUCCESS) {
        ierr = PETSC_ERR_MPI;
        goto fail;
    }
    ierr = MPI_Allreduce(&local_matrix_memory,
                         &maximum_rank_matrix_memory, 1,
                         MPIU_PETSCLOGDOUBLE, MPI_MAX, layout->comm);
    if (ierr != MPI_SUCCESS) {
        ierr = PETSC_ERR_MPI;
        goto fail;
    }
    ierr = or_eig_logf(layout->comm,
                       "Public /EIG %s: global rows %" PRId64
                       ", local rows %d, "
                       "used nonzeros %.0f, allocated nonzeros %.0f, "
                       "PETSc allocator growth %.3f MiB global, %.3f MiB "
                       "maximum rank, maximum local row nonzeros %"
                       PetscInt_FMT "\n",
                       matrix_name, layout->global_size, layout->local_size,
                       (double)matrix_info.nz_used,
                       (double)matrix_info.nz_allocated,
                       (double)global_matrix_memory / (1024.0 * 1024.0),
                       (double)maximum_rank_matrix_memory /
                           (1024.0 * 1024.0),
                       maximum_row_nnz);
    if (ierr != PETSC_SUCCESS) goto fail;
    PetscFunctionReturn(PETSC_SUCCESS);

fail:
    PetscCall(PetscFree(values));
    PetscCall(PetscFree(columns));
    PetscCall(MatDestroy(matrix));
    PetscFunctionReturn(ierr);
}

static PetscInt or_eig_subspace_size(const or_eig_layout *layout,
                                     PetscInt requested,
                                     const or_eig_controls *controls) {
    PetscInt subspace;

    if (requested >= (PetscInt)layout->global_size)
        return (PetscInt)layout->global_size;
    if (requested > PETSC_MAX_INT - 15 ||
        requested > PETSC_MAX_INT / controls->subspace_factor)
        subspace = (PetscInt)layout->global_size;
    else {
        subspace = requested * controls->subspace_factor;
        subspace = PetscMax(subspace, requested + 15);
    }
    subspace = PetscMax(subspace, requested + 1);
    return PetscMin(subspace, (PetscInt)layout->global_size);
}

/* Keep inclusive decimal input boundaries stable when the computed mode is a
   few solver ulps to either side.  The cap prevents a loose iteration
   tolerance from materially widening the requested interval. */
static PetscReal or_eig_boundary_margin(PetscReal boundary,
                                        const or_eig_controls *controls) {
    const PetscReal bounded_tolerance =
        PetscMin(PetscMax(controls->tolerance, (PetscReal)1.0e-10),
                 (PetscReal)1.0e-8);
    const PetscReal relative_guard =
        PetscMax((PetscReal)64.0 * PETSC_MACHINE_EPSILON,
                 (PetscReal)64.0 * bounded_tolerance);

    return relative_guard * PetscMax(PetscAbsReal(boundary), (PetscReal)1.0);
}

/* Final classification uses only the solver's stated accuracy, rather than
   the wider search guard.  This preserves an endpoint mode whose computed
   Ritz value lands barely outside the decimal input boundary without turning
   the search interval itself into the requested physical interval. */
static PetscReal or_eig_acceptance_margin(
    PetscReal boundary, const or_eig_controls *controls) {
    const PetscReal bounded_tolerance =
        PetscMin(PetscMax(controls->tolerance,
                          (PetscReal)64.0 * PETSC_MACHINE_EPSILON),
                 (PetscReal)1.0e-8);

    return bounded_tolerance *
           PetscMax(PetscAbsReal(boundary), (PetscReal)1.0);
}

static PetscErrorCode or_eig_report_solver_configuration(EPS eps) {
    ST transform;
    KSP ksp;
    PC pc;
    MPI_Comm communicator;
    const char *eps_type = NULL;
    const char *st_type = NULL;
    const char *ksp_type = NULL;
    const char *pc_type = NULL;
    PetscInt requested = 0;
    PetscInt subspace = 0;
    PetscInt projected = 0;
    PetscInt maximum_iterations = 0;
    PetscReal tolerance = 0.0;
    PetscScalar target = 0.0;
    EPSWhich which;

    PetscFunctionBegin;
    communicator = PetscObjectComm((PetscObject)eps);
    PetscCall(EPSGetType(eps, &eps_type));
    PetscCall(EPSGetDimensions(eps, &requested, &subspace, &projected));
    PetscCall(EPSGetTolerances(eps, &tolerance, &maximum_iterations));
    PetscCall(EPSGetTarget(eps, &target));
    PetscCall(EPSGetWhichEigenpairs(eps, &which));
    PetscCall(EPSGetST(eps, &transform));
    PetscCall(STGetType(transform, &st_type));
    if (which == EPS_ALL)
        PetscCall(EPSKrylovSchurGetKSP(eps, &ksp));
    else
        PetscCall(STGetKSP(transform, &ksp));
    PetscCall(KSPGetType(ksp, &ksp_type));
    PetscCall(KSPGetPC(ksp, &pc));
    PetscCall(PCGetType(pc, &pc_type));
    PetscCall(or_eig_logf(
        communicator,
        "Public /EIG solver configuration: EPS=%s, ST=%s, KSP=%s, PC=%s, "
        "requested=%" PetscInt_FMT ", subspace=%" PetscInt_FMT
        ", projected=%" PetscInt_FMT ", target=%.16e, tolerance=%.6e, "
        "maximum_iterations=%" PetscInt_FMT "\n",
        eps_type != NULL ? eps_type : "(unset)",
        st_type != NULL ? st_type : "(unset)",
        ksp_type != NULL ? ksp_type : "(unset)",
        pc_type != NULL ? pc_type : "(unset)", requested, subspace, projected,
        (double)PetscRealPart(target), (double)tolerance,
        maximum_iterations));
    PetscCall(PetscFFlush(PETSC_STDOUT));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_configure_factor(KSP ksp,
                                               PetscReal null_threshold) {
    PC pc;
    Mat factor;

    PetscFunctionBegin;
    PetscCall(KSPSetType(ksp, KSPPREONLY));
    PetscCall(KSPGetPC(ksp, &pc));
    PetscCall(PCSetType(pc, PCCHOLESKY));
    PetscCall(PCFactorSetMatSolverType(pc, MATSOLVERMUMPS));
    PetscCall(PCFactorSetUpMatSolverType(pc));
    PetscCall(PCFactorGetMatrix(pc, &factor));
    PetscCall(MatMumpsSetIcntl(factor, 13, 1));
    PetscCall(MatMumpsSetIcntl(factor, 24, 1));
    PetscCall(MatMumpsSetIcntl(factor, 25, 0));
    PetscCall(MatMumpsSetCntl(factor, 3, null_threshold));
    if (or_eig_exhaustive_diagnostics) {
        /* MUMPS output units 1--3 and verbosity ICNTL(4) are normally quiet.
           Route errors, diagnostics and global factorization information to
           stdout for the explicitly instrumented reproduction. */
        PetscCall(MatMumpsSetIcntl(factor, 1, 6));
        PetscCall(MatMumpsSetIcntl(factor, 2, 6));
        PetscCall(MatMumpsSetIcntl(factor, 3, 6));
        PetscCall(MatMumpsSetIcntl(factor, 4, 3));
    }
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_dump_mumps_diagnostics(EPS eps,
                                                     const char *stage) {
    KSP ksp;
    PC pc;
    Mat factor;
    MPI_Comm communicator;
    PetscMPIInt rank = -1;
    PetscInt integer_value = 0;
    PetscReal real_value = 0.0;
    PetscInt ksp_iterations = 0;
    PetscReal ksp_residual = 0.0;
    KSPConvergedReason ksp_reason = KSP_CONVERGED_ITERATING;
    PetscInt index;
    PetscInt local_info[OR_EIG_MUMPS_DIAGNOSTIC_COUNT];
    PetscInt *all_info = NULL;
    PetscReal local_rinfo[OR_EIG_MUMPS_DIAGNOSTIC_COUNT];
    PetscReal *all_rinfo = NULL;
    PetscMPIInt size = 0;
    char line[OR_EIG_LOG_MESSAGE_CAPACITY];
    size_t used;

    PetscFunctionBegin;
    if (!or_eig_exhaustive_diagnostics) PetscFunctionReturn(PETSC_SUCCESS);
    communicator = PetscObjectComm((PetscObject)eps);
    PetscCallMPI(MPI_Comm_rank(communicator, &rank));
    PetscCallMPI(MPI_Comm_size(communicator, &size));
    /* Spectrum slicing factors through the internal child EPS/KSP returned
       by this public API.  The parent ST KSP deliberately has no operators,
       so PCFactorGetMatrix() on it is a wrong-state error. */
    PetscCall(EPSKrylovSchurGetKSP(eps, &ksp));
    PetscCall(KSPGetPC(ksp, &pc));
    PetscCall(PCFactorGetMatrix(pc, &factor));
    PetscCall(KSPGetIterationNumber(ksp, &ksp_iterations));
    PetscCall(KSPGetResidualNorm(ksp, &ksp_residual));
    PetscCall(KSPGetConvergedReason(ksp, &ksp_reason));
    PetscCall(or_eig_logf(
        communicator,
        "Public /EIG MUMPS diagnostic snapshot begin: stage=%s, "
        "KSP_iterations=%" PetscInt_FMT ", KSP_residual=%.16e, "
        "KSP_reason=%d\n",
        stage, ksp_iterations, (double)ksp_residual, (int)ksp_reason));
    for (index = 1; index <= OR_EIG_MUMPS_DIAGNOSTIC_COUNT; ++index) {
        PetscCall(MatMumpsGetInfog(factor, index, &integer_value));
        PetscCall(or_eig_logf(
            communicator,
            "Public /EIG MUMPS INFOG: stage=%s, index=%" PetscInt_FMT
            ", value=%" PetscInt_FMT "\n",
            stage, index, integer_value));
    }
    for (index = 1; index <= OR_EIG_MUMPS_DIAGNOSTIC_COUNT; ++index) {
        PetscCall(MatMumpsGetRinfog(factor, index, &real_value));
        PetscCall(or_eig_logf(
            communicator,
            "Public /EIG MUMPS RINFOG: stage=%s, index=%" PetscInt_FMT
            ", value=%.16e\n",
            stage, index, (double)real_value));
    }
    for (index = 0; index < OR_EIG_MUMPS_DIAGNOSTIC_COUNT; ++index) {
        PetscCall(MatMumpsGetInfo(factor, index + 1, &local_info[index]));
    }
    for (index = 0; index < OR_EIG_MUMPS_DIAGNOSTIC_COUNT; ++index) {
        PetscCall(MatMumpsGetRinfo(factor, index + 1, &local_rinfo[index]));
    }
    if (rank == 0) {
        PetscCall(PetscMalloc1((size_t)size *
                                   OR_EIG_MUMPS_DIAGNOSTIC_COUNT,
                               &all_info));
        PetscCall(PetscMalloc1((size_t)size *
                                   OR_EIG_MUMPS_DIAGNOSTIC_COUNT,
                               &all_rinfo));
    }
    PetscCallMPI(MPI_Gather(local_info, OR_EIG_MUMPS_DIAGNOSTIC_COUNT,
                            MPIU_INT, all_info,
                            OR_EIG_MUMPS_DIAGNOSTIC_COUNT, MPIU_INT, 0,
                            communicator));
    PetscCallMPI(MPI_Gather(local_rinfo, OR_EIG_MUMPS_DIAGNOSTIC_COUNT,
                            MPIU_REAL, all_rinfo,
                            OR_EIG_MUMPS_DIAGNOSTIC_COUNT, MPIU_REAL, 0,
                            communicator));
    if (rank == 0) {
        for (PetscMPIInt diagnostic_rank = 0; diagnostic_rank < size;
             ++diagnostic_rank) {
            PetscCall(PetscSNPrintf(
                line, sizeof(line),
                "Public /EIG MUMPS INFO local: stage=%s, rank=%d, values=",
                stage, (int)diagnostic_rank));
            for (index = 0; index < OR_EIG_MUMPS_DIAGNOSTIC_COUNT; ++index) {
                PetscCall(PetscStrlen(line, &used));
                PetscCheck(used + 64 < sizeof(line), communicator,
                           PETSC_ERR_ARG_SIZ,
                           "MUMPS INFO diagnostic line exceeds %zu bytes",
                           sizeof(line) - 1);
                PetscCall(PetscSNPrintf(
                    line + used, sizeof(line) - used,
                    "%s%" PetscInt_FMT ":%" PetscInt_FMT,
                    index == 0 ? "[" : ",", index + 1,
                    all_info[(size_t)diagnostic_rank *
                                 OR_EIG_MUMPS_DIAGNOSTIC_COUNT +
                             index]));
            }
            PetscCall(PetscStrlen(line, &used));
            PetscCall(PetscSNPrintf(line + used, sizeof(line) - used,
                                    "]\n"));
            PetscCall(or_eig_log_text(communicator, line));

            PetscCall(PetscSNPrintf(
                line, sizeof(line),
                "Public /EIG MUMPS RINFO local: stage=%s, rank=%d, values=",
                stage, (int)diagnostic_rank));
            for (index = 0; index < OR_EIG_MUMPS_DIAGNOSTIC_COUNT; ++index) {
                PetscCall(PetscStrlen(line, &used));
                PetscCheck(used + 64 < sizeof(line), communicator,
                           PETSC_ERR_ARG_SIZ,
                           "MUMPS RINFO diagnostic line exceeds %zu bytes",
                           sizeof(line) - 1);
                PetscCall(PetscSNPrintf(
                    line + used, sizeof(line) - used,
                    "%s%" PetscInt_FMT ":%.16e", index == 0 ? "[" : ",",
                    index + 1,
                    (double)all_rinfo[(size_t)diagnostic_rank *
                                          OR_EIG_MUMPS_DIAGNOSTIC_COUNT +
                                      index]));
            }
            PetscCall(PetscStrlen(line, &used));
            PetscCall(PetscSNPrintf(line + used, sizeof(line) - used,
                                    "]\n"));
            PetscCall(or_eig_log_text(communicator, line));
        }
    }
    PetscCall(PetscFree(all_rinfo));
    PetscCall(PetscFree(all_info));
    PetscCall(or_eig_logf(
        communicator,
        "Public /EIG MUMPS diagnostic snapshot end: stage=%s\n", stage));
    PetscCall(PetscFFlush(PETSC_STDOUT));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_create_target_solver(
    Mat stiffness, Mat mass, PetscInt requested, PetscReal target,
    PetscReal region_lower, PetscReal region_upper,
    const or_eig_layout *layout, const or_eig_controls *controls, EPS *eps) {
    ST transform;
    KSP ksp;
    RG region;
    PetscLogDouble phase_started = 0.0;

    PetscFunctionBegin;
    PetscCall(or_eig_timing_begin(layout->comm,
                                  "target EPS/ST construction",
                                  &phase_started));
    PetscCall(EPSCreate(layout->comm, eps));
    PetscCall(EPSSetOperators(*eps, stiffness, mass));
    PetscCall(EPSSetProblemType(*eps, EPS_GHEP));
    PetscCall(EPSSetType(*eps, EPSKRYLOVSCHUR));
    PetscCall(EPSSetDimensions(
        *eps, requested,
        or_eig_subspace_size(layout, requested, controls), PETSC_DETERMINE));
    PetscCall(EPSSetTolerances(*eps, controls->tolerance,
                               controls->maximum_iterations));
    PetscCall(EPSSetConvergenceTest(*eps, EPS_CONV_NORM));
    PetscCall(EPSSetTrueResidual(*eps, PETSC_TRUE));
    PetscCall(EPSSetTarget(*eps, (PetscScalar)target));
    PetscCall(EPSSetWhichEigenpairs(*eps, EPS_TARGET_MAGNITUDE));
    PetscCall(EPSGetRG(*eps, &region));
    PetscCall(RGSetType(region, RGINTERVAL));
    PetscCall(RGIntervalSetEndpoints(region, region_lower, region_upper,
                                     0.0, 0.0));
    PetscCall(EPSGetST(*eps, &transform));
    PetscCall(STSetType(transform, STSINVERT));
    PetscCall(STSetShift(transform, (PetscScalar)target));
    PetscCall(STGetKSP(transform, &ksp));
    /* Materialize K-sigma*M and attach it to the KSP before asking PETSc to
       create the MUMPS factor object.  This does not factor the matrix; it
       only makes the factor object available for setting inertia controls. */
    PetscCall(STGetOperator(transform, NULL));
    PetscCall(or_eig_configure_factor(
        ksp, controls->positive_pivot_tolerance));
    PetscCall(EPSSetFromOptions(*eps));
    PetscCall(or_eig_timing_end(layout->comm,
                                "target EPS/ST construction",
                                phase_started));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_create_interval_solver(
    Mat stiffness, Mat mass, PetscReal lower, PetscReal upper,
    const or_eig_layout *layout, const or_eig_controls *controls, EPS *eps) {
    ST transform;
    KSP ksp;
    PetscLogDouble phase_started = 0.0;

    PetscFunctionBegin;
    PetscCall(or_eig_timing_begin(layout->comm,
                                  "interval EPS/ST construction",
                                  &phase_started));
    PetscCall(EPSCreate(layout->comm, eps));
    PetscCall(EPSSetOperators(*eps, stiffness, mass));
    PetscCall(EPSSetProblemType(*eps, EPS_GHEP));
    PetscCall(EPSSetType(*eps, EPSKRYLOVSCHUR));
    PetscCall(EPSSetTolerances(*eps, controls->tolerance,
                               controls->maximum_iterations));
    PetscCall(EPSSetConvergenceTest(*eps, EPS_CONV_NORM));
    PetscCall(EPSSetTrueResidual(*eps, PETSC_TRUE));
    PetscCall(EPSSetWhichEigenpairs(*eps, EPS_ALL));
    PetscCall(EPSSetInterval(*eps, lower, upper));
    PetscCall(EPSKrylovSchurSetDetectZeros(*eps, PETSC_FALSE));
    PetscCall(EPSGetST(*eps, &transform));
    PetscCall(STSetType(transform, STSINVERT));
    PetscCall(EPSKrylovSchurGetKSP(*eps, &ksp));
    PetscCall(or_eig_configure_factor(ksp,
                                      controls->upper_pivot_tolerance));
    PetscCall(EPSSetFromOptions(*eps));
    PetscCall(or_eig_timing_end(layout->comm,
                                "interval EPS/ST construction",
                                phase_started));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_factor_inertia(EPS eps, PetscInt *negative,
                                             PetscInt *zero,
                                             PetscInt *positive) {
    ST transform;
    KSP ksp;
    PC pc;
    Mat factor;

    PetscFunctionBegin;
    PetscCall(EPSGetST(eps, &transform));
    PetscCall(STGetKSP(transform, &ksp));
    PetscCall(KSPGetPC(ksp, &pc));
    PetscCall(PCFactorGetMatrix(pc, &factor));
    PetscCall(MatGetInertia(factor, negative, zero, positive));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_component_map_attach(
    PetscInt local_size, PetscInt **global_rows,
    or_eig_component_map **map) {
    PetscFunctionBegin;
    if (*map == NULL) {
        PetscCall(PetscNew(map));
        (*map)->local_size = local_size;
        (*map)->global_rows = *global_rows;
        (*map)->references = 0;
        *global_rows = NULL;
    }
    ++(*map)->references;
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_component_mode_destroy(
    or_eig_component_mode *mode) {
    PetscErrorCode ierr = PETSC_SUCCESS;
    PetscErrorCode cleanup_ierr;

    PetscFunctionBegin;
    cleanup_ierr = VecDestroy(&mode->vector);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    if (mode->map != NULL) {
        if (mode->map->references <= 0) {
            if (ierr == PETSC_SUCCESS) ierr = PETSC_ERR_PLIB;
        } else {
            --mode->map->references;
            if (mode->map->references == 0) {
                cleanup_ierr = PetscFree(mode->map->global_rows);
                if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
                cleanup_ierr = PetscFree(mode->map);
                if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
            }
        }
        mode->map = NULL;
    }
    PetscFunctionReturn(ierr);
}

static PetscErrorCode or_eig_destroy_component_modes(
    or_eig_component_mode **modes, PetscInt count) {
    PetscErrorCode ierr = PETSC_SUCCESS;
    PetscErrorCode cleanup_ierr;

    PetscFunctionBegin;
    if (*modes != NULL) {
        for (PetscInt i = 0; i < count; ++i) {
            cleanup_ierr = or_eig_component_mode_destroy(&(*modes)[i]);
            if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
        }
    }
    cleanup_ierr = PetscFree(*modes);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    *modes = NULL;
    PetscFunctionReturn(ierr);
}

static PetscErrorCode or_eig_count_interval(EPS eps, PetscInt *count) {
    PetscReal *shifts = NULL;
    PetscInt *inertias = NULL;
    PetscInt number_of_shifts = 0;

    PetscFunctionBegin;
    PetscCall(EPSKrylovSchurGetInertias(eps, &number_of_shifts, &shifts,
                                        &inertias));
    if (or_eig_exhaustive_diagnostics) {
        MPI_Comm communicator = PetscObjectComm((PetscObject)eps);
        PetscInt shift_index;

        PetscCall(or_eig_logf(
            communicator,
            "Public /EIG inertia catalogue begin: shifts=%" PetscInt_FMT
            "\n",
            number_of_shifts));
        for (shift_index = 0; shift_index < number_of_shifts;
             ++shift_index) {
            PetscCall(or_eig_logf(
                communicator,
                "Public /EIG inertia shift: index=%" PetscInt_FMT
                ", shift=%.16e, inertia=%" PetscInt_FMT "\n",
                shift_index, (double)shifts[shift_index],
                inertias[shift_index]));
        }
        PetscCall(or_eig_logf(communicator,
                              "Public /EIG inertia catalogue end\n"));
        PetscCall(PetscFFlush(PETSC_STDOUT));
    }
    *count = number_of_shifts > 1
                 ? PetscAbsInt(inertias[number_of_shifts - 1] - inertias[0])
                 : 0;
    PetscCall(PetscFree(shifts));
    PetscCall(PetscFree(inertias));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_count_above_target(EPS eps, PetscInt *count) {
    PetscInt negative;
    PetscInt zero;

    PetscFunctionBegin;
    PetscCall(or_eig_factor_inertia(eps, &negative, &zero, count));
    PetscFunctionReturn(PETSC_SUCCESS);
}

/* Find connected components of the final constrained generalized operator.
   Each rank first contracts its complete local K/M graph.  Only component
   labels on PETSc ghost DOFs are then exchanged, so convergence depends on
   the MPI partition graph rather than the diameter of the finite-element
   mesh.  DOFs belonging to one physical node are explicitly joined because
   a diagonal rotational inertia need not create an algebraic matrix edge. */
static PetscErrorCode or_eig_find_constraint_components(
    Mat stiffness, Mat mass, const or_eig_layout *layout,
    int input_local_size, const int64_t *global_dof, const int *global_node,
    PetscInt **owned_labels, PetscInt **component_ids,
    PetscInt *component_count) {
    Mat operators[2] = {stiffness, mass};
    const PetscInt *operator_ghosts[2] = {NULL, NULL};
    PetscInt operator_ghost_counts[2] = {0, 0};
    PetscInt *ghosts = NULL;
    or_eig_global_vertex *ghost_map = NULL;
    or_eig_node_vertex *node_map = NULL;
    PetscInt *parent = NULL;
    PetscInt *root_minimum = NULL;
    PetscInt *local_ids = NULL;
    PetscScalar *label_values = NULL;
    const PetscScalar *label_read_values = NULL;
    PetscScalar *local_values = NULL;
    PetscScalar *previous_values = NULL;
    Vec labels = NULL;
    Vec local_labels = NULL;
    PetscInt first_owned = 0;
    PetscInt last_owned = 0;
    PetscInt owned = 0;
    PetscInt ghost_count = 0;
    PetscInt raw_ghost_count = 0;
    PetscInt vertex_count;
    PetscInt node_map_count = 0;
    PetscInt local_component_count = 0;
    PetscInt iteration = 0;
    PetscMPIInt rank = 0;
    PetscMPIInt size = 1;
    PetscMPIInt *counts = NULL;
    PetscMPIInt *offsets = NULL;
    PetscMPIInt local_count_mpi = 0;
    PetscMPIInt global_count_mpi = 0;
    PetscBool changed = PETSC_TRUE;
    PetscLogDouble phase_started = 0.0;
    PetscInt *local_component_sizes = NULL;
    PetscInt *global_component_sizes = NULL;
    PetscInt i;

    PetscFunctionBegin;
    *owned_labels = NULL;
    *component_ids = NULL;
    *component_count = 0;
    PetscCallMPI(MPI_Comm_rank(layout->comm, &rank));
    PetscCallMPI(MPI_Comm_size(layout->comm, &size));
    PetscCall(MatGetOwnershipRange(stiffness, &first_owned, &last_owned));
    owned = last_owned - first_owned;
    PetscCheck(owned == (PetscInt)layout->local_size, layout->comm,
               PETSC_ERR_PLIB,
               "Rigid-template ownership differs from /EIG matrix layout");
    PetscCall(or_eig_timing_begin(
        layout->comm, "rigid-component local graph contraction",
        &phase_started));
    if (size > 1) {
        for (PetscInt operator_index = 0; operator_index < 2;
             ++operator_index) {
            PetscCall(MatGetGhosts(operators[operator_index],
                                   &operator_ghost_counts[operator_index],
                                   &operator_ghosts[operator_index]));
            raw_ghost_count += operator_ghost_counts[operator_index];
        }
        PetscCheck(input_local_size <= PETSC_MAX_INT - raw_ghost_count,
                   layout->comm, PETSC_ERR_MEM,
                   "Rigid-template ghost graph is too large");
        PetscCall(PetscMalloc1(
            PetscMax(raw_ghost_count + input_local_size, 1), &ghosts));
        raw_ghost_count = 0;
        for (PetscInt operator_index = 0; operator_index < 2;
             ++operator_index)
            for (i = 0; i < operator_ghost_counts[operator_index]; ++i)
                ghosts[raw_ghost_count++] =
                    operator_ghosts[operator_index][i];
        for (i = 0; i < input_local_size; ++i) {
            const PetscInt global = (PetscInt)global_dof[i];

            if (global < first_owned || global >= last_owned)
                ghosts[raw_ghost_count++] = global;
        }
        qsort(ghosts, (size_t)raw_ghost_count, sizeof(*ghosts),
              or_eig_compare_petsc_int);
        for (i = 0; i < raw_ghost_count; ++i)
            if (i == 0 || ghosts[i] != ghosts[i - 1])
                ghosts[ghost_count++] = ghosts[i];
    }
    PetscCheck(ghost_count <= PETSC_MAX_INT - owned, layout->comm,
               PETSC_ERR_MEM, "Rigid-template graph is too large");
    vertex_count = owned + ghost_count;
    PetscCall(PetscMalloc1(PetscMax(vertex_count, 1), &parent));
    PetscCall(PetscMalloc1(PetscMax(vertex_count, 1), &root_minimum));
    PetscCall(PetscMalloc1(PetscMax(ghost_count, 1), &ghost_map));
    PetscCall(PetscMalloc1(PetscMax(input_local_size, 1), &node_map));
    PetscCall(PetscMalloc1(PetscMax(owned, 1), &previous_values));
    for (i = 0; i < vertex_count; ++i) parent[i] = i;
    for (i = 0; i < ghost_count; ++i) {
        ghost_map[i].global = ghosts[i];
        ghost_map[i].vertex = owned + i;
    }
    qsort(ghost_map, (size_t)ghost_count, sizeof(*ghost_map),
          or_eig_compare_global_vertex);

    for (i = 0; i < input_local_size; ++i) {
        const PetscInt vertex = or_eig_local_vertex(
            (PetscInt)global_dof[i], first_owned, owned, ghost_map,
            ghost_count);
        if (vertex < 0) continue;
        node_map[node_map_count].node = global_node[i];
        node_map[node_map_count].vertex = vertex;
        ++node_map_count;
    }
    qsort(node_map, (size_t)node_map_count, sizeof(*node_map),
          or_eig_compare_node_vertex);
    for (i = 1; i < node_map_count; ++i) {
        if (node_map[i].node == node_map[i - 1].node)
            or_eig_union_vertices(parent, node_map[i - 1].vertex,
                                  node_map[i].vertex);
    }

    for (PetscInt operator_index = 0; operator_index < 2;
         ++operator_index) {
        for (i = first_owned; i < last_owned; ++i) {
            const PetscInt *columns = NULL;
            const PetscScalar *values = NULL;
            PetscInt entries = 0;
            PetscInt entry;

            PetscCall(MatGetRow(operators[operator_index], i, &entries,
                                &columns, &values));
            for (entry = 0; entry < entries; ++entry) {
                PetscInt column_vertex;
                if (columns[entry] == i || values[entry] == (PetscScalar)0.0)
                    continue;
                column_vertex = or_eig_local_vertex(
                    columns[entry], first_owned, owned, ghost_map,
                    ghost_count);
                PetscCheck(column_vertex >= 0, layout->comm, PETSC_ERR_PLIB,
                           "A constrained matrix column is absent from the "
                           "PETSc ghost graph");
                or_eig_union_vertices(parent, i - first_owned,
                                      column_vertex);
            }
            PetscCall(MatRestoreRow(operators[operator_index], i, &entries,
                                    &columns, &values));
        }
    }
    PetscCall(or_eig_timing_end(
        layout->comm, "rigid-component local graph contraction",
        phase_started));

    PetscCall(or_eig_timing_begin(
        layout->comm, "rigid-component distributed label propagation",
        &phase_started));
    for (i = 0; i < vertex_count; ++i) root_minimum[i] = PETSC_MAX_INT;
    for (i = 0; i < vertex_count; ++i) {
        const PetscInt root = or_eig_union_find_root(parent, i);
        const PetscInt global =
            i < owned ? first_owned + i : ghosts[i - owned];
        root_minimum[root] = PetscMin(root_minimum[root], global);
    }
    PetscCall(VecCreateGhost(layout->comm, owned,
                             (PetscInt)layout->global_size, ghost_count,
                             ghosts, &labels));
    PetscCall(VecGetArray(labels, &label_values));
    for (i = 0; i < owned; ++i)
        label_values[i] =
            (PetscScalar)root_minimum[or_eig_union_find_root(parent, i)];
    PetscCall(VecRestoreArray(labels, &label_values));

    while (changed) {
        PetscMPIInt local_changed = 0;
        PetscMPIInt global_changed = 0;

        PetscCheck(iteration <= (PetscInt)size, layout->comm,
                   PETSC_ERR_NOT_CONVERGED,
                   "Distributed rigid-component labels did not converge");
        PetscCall(VecGetArrayRead(labels, &label_read_values));
        for (i = 0; i < owned; ++i)
            previous_values[i] = label_read_values[i];
        PetscCall(VecRestoreArrayRead(labels, &label_read_values));
        PetscCall(VecGhostUpdateBegin(labels, INSERT_VALUES,
                                      SCATTER_FORWARD));
        PetscCall(VecGhostUpdateEnd(labels, INSERT_VALUES,
                                    SCATTER_FORWARD));
        PetscCall(VecGhostGetLocalForm(labels, &local_labels));
        PetscCall(VecGetArray(local_labels, &local_values));
        for (i = 0; i < vertex_count; ++i)
            root_minimum[i] = PETSC_MAX_INT;
        for (i = 0; i < vertex_count; ++i) {
            const PetscInt root = or_eig_union_find_root(parent, i);
            const PetscInt value = (PetscInt)PetscRealPart(local_values[i]);
            root_minimum[root] = PetscMin(root_minimum[root], value);
        }
        for (i = 0; i < vertex_count; ++i)
            local_values[i] = (PetscScalar)root_minimum[
                or_eig_union_find_root(parent, i)];
        PetscCall(VecRestoreArray(local_labels, &local_values));
        PetscCall(VecGhostRestoreLocalForm(labels, &local_labels));
        PetscCall(VecGhostUpdateBegin(labels, MIN_VALUES, SCATTER_REVERSE));
        PetscCall(VecGhostUpdateEnd(labels, MIN_VALUES, SCATTER_REVERSE));
        PetscCall(VecGetArrayRead(labels, &label_read_values));
        for (i = 0; i < owned; ++i) {
            if (PetscRealPart(label_read_values[i]) <
                PetscRealPart(previous_values[i])) {
                local_changed = 1;
                break;
            }
        }
        PetscCall(VecRestoreArrayRead(labels, &label_read_values));
        PetscCallMPI(MPI_Allreduce(&local_changed, &global_changed, 1,
                                   MPI_INT, MPI_MAX, layout->comm));
        changed = global_changed ? PETSC_TRUE : PETSC_FALSE;
        ++iteration;
    }
    PetscCall(VecGhostUpdateBegin(labels, INSERT_VALUES, SCATTER_FORWARD));
    PetscCall(VecGhostUpdateEnd(labels, INSERT_VALUES, SCATTER_FORWARD));
    PetscCall(VecGetArrayRead(labels, &label_read_values));
    PetscCall(PetscMalloc1(PetscMax(owned, 1), owned_labels));
    for (i = 0; i < owned; ++i)
        (*owned_labels)[i] =
            (PetscInt)PetscRealPart(label_read_values[i]);
    PetscCall(VecRestoreArrayRead(labels, &label_read_values));
    PetscCall(or_eig_timing_end(
        layout->comm, "rigid-component distributed label propagation",
        phase_started));

    PetscCall(or_eig_timing_begin(
        layout->comm, "rigid-component global catalogue construction",
        &phase_started));
    PetscCall(VecGetArrayRead(labels, &label_read_values));
    for (i = 0; i < owned; ++i) {
        if ((PetscInt)PetscRealPart(label_read_values[i]) == first_owned + i)
            ++local_component_count;
    }
    PetscCall(PetscMalloc1(PetscMax(local_component_count, 1), &local_ids));
    local_component_count = 0;
    for (i = 0; i < owned; ++i) {
        if ((PetscInt)PetscRealPart(label_read_values[i]) == first_owned + i)
            local_ids[local_component_count++] = first_owned + i;
    }
    PetscCall(VecRestoreArrayRead(labels, &label_read_values));
    PetscCall(PetscMPIIntCast(local_component_count, &local_count_mpi));
    PetscCall(PetscMalloc2(size, &counts, size, &offsets));
    PetscCallMPI(MPI_Allgather(&local_count_mpi, 1, MPI_INT, counts, 1,
                               MPI_INT, layout->comm));
    offsets[0] = 0;
    for (PetscMPIInt process = 1; process < size; ++process)
        offsets[process] = offsets[process - 1] + counts[process - 1];
    global_count_mpi = offsets[size - 1] + counts[size - 1];
    *component_count = (PetscInt)global_count_mpi;
    PetscCall(PetscMalloc1(PetscMax(*component_count, 1), component_ids));
    PetscCallMPI(MPI_Allgatherv(local_ids, local_count_mpi, MPIU_INT,
                                *component_ids, counts, offsets, MPIU_INT,
                                layout->comm));
    PetscCall(PetscCalloc2(PetscMax(*component_count, 1),
                            &local_component_sizes,
                            PetscMax(*component_count, 1),
                            &global_component_sizes));
    for (i = 0; i < owned; ++i) {
        const PetscInt index = or_eig_sorted_index(
            (*owned_labels)[i], *component_ids, *component_count);
        PetscCheck(index >= 0, layout->comm, PETSC_ERR_PLIB,
                   "A constrained row has no component catalogue entry");
        ++local_component_sizes[index];
    }
    PetscCallMPI(MPI_Allreduce(local_component_sizes,
                               global_component_sizes,
                               global_count_mpi, MPIU_INT, MPI_SUM,
                               layout->comm));
    PetscCall(or_eig_logf(
        layout->comm,
        "Public /EIG constrained graph contains %" PetscInt_FMT
        " connected components (distributed label propagation %" PetscInt_FMT
        " iterations)\n",
        *component_count, iteration));
    for (i = 0; i < *component_count; ++i)
        PetscCall(or_eig_logf(
            layout->comm,
            "Public /EIG component catalogue %" PetscInt_FMT "/%"
            PetscInt_FMT ": rows=%" PetscInt_FMT "\n",
            i + 1, *component_count, global_component_sizes[i]));
    PetscCall(or_eig_timing_end(
        layout->comm, "rigid-component global catalogue construction",
        phase_started));

    PetscCall(PetscFree2(local_component_sizes, global_component_sizes));
    PetscCall(PetscFree2(counts, offsets));
    PetscCall(PetscFree(local_ids));
    PetscCall(VecDestroy(&labels));
    PetscCall(PetscFree(previous_values));
    PetscCall(PetscFree(node_map));
    PetscCall(PetscFree(ghost_map));
    PetscCall(PetscFree(ghosts));
    PetscCall(PetscFree(root_minimum));
    PetscCall(PetscFree(parent));
    PetscFunctionReturn(PETSC_SUCCESS);
}

typedef struct {
    PetscReal value;
    PetscInt index;
} or_eig_pair;

static int or_eig_compare_pair(const void *left, const void *right) {
    const or_eig_pair *a = (const or_eig_pair *)left;
    const or_eig_pair *b = (const or_eig_pair *)right;
    if (a->value != b->value)
        return (a->value > b->value) - (a->value < b->value);
    return (a->index > b->index) - (a->index < b->index);
}

/* Classify the numerical zero cluster only after SLEPc has solved the full
   component interval.  A 100x jump in eigenvalue magnitude is a 10x jump in
   frequency, but it is meaningful as a nullspace boundary only while its
   lower member remains inside the scale-relative near-null band.  Searching
   the entire positive spectrum would incorrectly discard a genuine soft
   mode whenever two flexible modes happened to have a large frequency gap.
   Values below the roundoff scale are clamped before taking ratios so
   harmless variation inside an exact nullspace does not create a false
   split.  The independently predicted rigid count is used only to confirm an
   all-near-null result; it must not force a positive Ritz value into the
   numerical nullspace because the physical test and SLEPc are deliberately
   independent. */
static PetscInt or_eig_zero_cluster(
    const PetscReal *values, PetscInt count, PetscReal spectral_scale,
    PetscReal relative_ceiling, PetscInt physical_count,
    or_eig_pair *ordered, PetscReal *roundoff_floor,
    PetscReal *candidate_ceiling, PetscReal *separation_ratio) {
    PetscInt numerical_zero_count = 0;
    PetscInt candidate_count = 0;
    PetscInt search_start;

    /* spectral_scale is non-negative here.  PETSC_MIN_REAL is the most
       negative representable value, not a positive underflow floor, so a
       PetscMax with it is both redundant and misleading. */
    *roundoff_floor = PETSC_MACHINE_EPSILON * spectral_scale;
    *candidate_ceiling =
        PetscMax(*roundoff_floor,
                 PetscMax(relative_ceiling,
                          (PetscReal)128.0 * PETSC_MACHINE_EPSILON) *
                     spectral_scale);
    *separation_ratio = 0.0;
    for (PetscInt mode = 0; mode < count; ++mode) {
        ordered[mode].value = PetscAbsReal(values[mode]);
        ordered[mode].index = mode;
    }
    qsort(ordered, (size_t)count, sizeof(*ordered), or_eig_compare_pair);
    while (numerical_zero_count < count &&
           ordered[numerical_zero_count].value <= *roundoff_floor)
        ++numerical_zero_count;
    if (numerical_zero_count == count) return count;

    while (candidate_count < count &&
           ordered[candidate_count].value <= *candidate_ceiling)
        ++candidate_count;
    search_start = PetscMax(numerical_zero_count, (PetscInt)1) - 1;
    for (PetscInt mode = search_start;
         mode + 1 < count &&
         ordered[mode].value <= *candidate_ceiling;
         ++mode) {
        const PetscReal lower =
            PetscMax(ordered[mode].value, *roundoff_floor);
        const PetscReal upper =
            PetscMax(ordered[mode + 1].value, *roundoff_floor);
        const PetscReal ratio = upper / lower;

        if (ratio >= (PetscReal)100.0) {
            *separation_ratio = ratio;
            return mode + 1;
        }
    }
    /* If every solved value is in the near-null band and the independent
       mechanical subspace spans all of them, there is no positive anchor on
       which to observe a separating gap.  Otherwise only the roundoff-scale
       values are proven numerical zeros. */
    if (candidate_count == count && physical_count >= count) return count;
    return numerical_zero_count;
}

static PetscErrorCode or_eig_small_symmetric_eigensystem(
    MPI_Comm comm, PetscInt order, PetscScalar *matrix,
    PetscReal *eigenvalues) {
    PetscBLASInt n;
    PetscBLASInt leading_dimension;
    PetscBLASInt work_size;
    PetscBLASInt info;
    PetscScalar work[32];
#if defined(PETSC_USE_COMPLEX)
    PetscReal real_work[32];
#endif

    PetscFunctionBegin;
    if (order == 0) PetscFunctionReturn(PETSC_SUCCESS);
    PetscCheck(order <= 6, comm, PETSC_ERR_ARG_SIZ,
               "Rigid-template projection exceeds six dimensions");
    PetscCall(PetscBLASIntCast(order, &n));
    leading_dimension = n;
    work_size = (PetscBLASInt)32;
#if defined(PETSC_USE_COMPLEX)
    PetscCallBLAS("LAPACKsyev",
                  LAPACKsyev_("V", "U", &n, matrix,
                               &leading_dimension, eigenvalues, work,
                               &work_size, real_work, &info));
#else
    PetscCallBLAS("LAPACKsyev",
                  LAPACKsyev_("V", "U", &n, matrix,
                               &leading_dimension, eigenvalues, work,
                               &work_size, &info));
#endif
    PetscCheck(info == 0, comm, PETSC_ERR_LIB,
               "LAPACK failed to diagonalize a rigid-template projection");
    PetscFunctionReturn(PETSC_SUCCESS);
}

/* Consolidate row metadata at the PETSc owner.  Frontier copies contribute
   through ADD_VALUES and are averaged, so this remains deterministic when a
   finite-element DOF is present on several OpenRadioss ranks. */
static PetscErrorCode or_eig_build_owned_dof_metadata(
    const or_eig_layout *layout, int input_local_size,
    const int64_t *global_dof, const int *input_component,
    const double *input_coordinate, PetscInt **owned_component,
    PetscReal **owned_coordinate) {
    Vec fields[5] = {NULL, NULL, NULL, NULL, NULL};
    const PetscScalar *field_values[5] = {NULL, NULL, NULL, NULL, NULL};
    PetscInt row;

    PetscFunctionBegin;
    *owned_component = NULL;
    *owned_coordinate = NULL;
    PetscCall(VecCreateMPI(layout->comm, (PetscInt)layout->local_size,
                           (PetscInt)layout->global_size, &fields[0]));
    for (PetscInt field = 1; field < 5; ++field)
        PetscCall(VecDuplicate(fields[0], &fields[field]));
    for (row = 0; row < (PetscInt)input_local_size; ++row) {
        const PetscInt global = (PetscInt)global_dof[row];

        PetscCheck(input_component[row] >= 1 && input_component[row] <= 6,
                   layout->comm, PETSC_ERR_ARG_OUTOFRANGE,
                   "A /EIG row has an invalid physical DOF component");
        PetscCall(VecSetValue(fields[0], global, 1.0, ADD_VALUES));
        PetscCall(VecSetValue(fields[1], global,
                              (PetscScalar)input_component[row],
                              ADD_VALUES));
        for (PetscInt axis = 0; axis < 3; ++axis) {
            const double coordinate =
                input_coordinate[(size_t)3 * (size_t)row + (size_t)axis];
            PetscCheck(isfinite(coordinate), layout->comm,
                       PETSC_ERR_ARG_WRONG,
                       "A /EIG row has a non-finite nodal coordinate");
            PetscCall(VecSetValue(fields[axis + 2], global,
                                  (PetscScalar)coordinate, ADD_VALUES));
        }
    }
    for (PetscInt field = 0; field < 5; ++field) {
        PetscCall(VecAssemblyBegin(fields[field]));
        PetscCall(VecAssemblyEnd(fields[field]));
        PetscCall(VecGetArrayRead(fields[field], &field_values[field]));
    }
    PetscCall(PetscMalloc1(PetscMax((PetscInt)layout->local_size, 1),
                           owned_component));
    PetscCall(PetscMalloc1(
        PetscMax((PetscInt)3 * (PetscInt)layout->local_size, 1),
        owned_coordinate));
    for (row = 0; row < (PetscInt)layout->local_size; ++row) {
        const PetscReal copies = PetscRealPart(field_values[0][row]);
        PetscReal component_average;

        PetscCheck(copies > 0.0, layout->comm, PETSC_ERR_PLIB,
                   "A PETSc-owned /EIG row has no physical metadata");
        component_average = PetscRealPart(field_values[1][row]) / copies;
        (*owned_component)[row] =
            (PetscInt)PetscFloorReal(component_average + 0.5);
        PetscCheck((*owned_component)[row] >= 1 &&
                       (*owned_component)[row] <= 6 &&
                       PetscAbsReal(component_average -
                                    (PetscReal)(*owned_component)[row]) <=
                           (PetscReal)64.0 * PETSC_MACHINE_EPSILON,
                   layout->comm, PETSC_ERR_PLIB,
                   "Distributed copies disagree on a physical DOF component");
        for (PetscInt axis = 0; axis < 3; ++axis)
            (*owned_coordinate)[(size_t)3 * (size_t)row +
                                (size_t)axis] =
                PetscRealPart(field_values[axis + 2][row]) / copies;
    }
    for (PetscInt field = 0; field < 5; ++field) {
        PetscCall(VecRestoreArrayRead(fields[field], &field_values[field]));
        PetscCall(VecDestroy(&fields[field]));
    }
    PetscFunctionReturn(PETSC_SUCCESS);
}

/* Build the physical rigid-motion subspace of one constrained component.
   The six geometric candidates are projected through the final K/M rows.
   Diagonalizing the resulting 6x6 stiffness admits arbitrary free joint
   directions; the full-space K residual then rejects projected false zeros. */
static PetscErrorCode or_eig_predict_rigid_templates(
    Mat stiffness, Mat mass, const or_eig_layout *global_layout,
    const or_eig_layout *component_layout, PetscInt selected_count,
    const PetscInt *selected_global, const PetscInt *owned_component,
    const PetscReal *owned_coordinate, PetscReal stiffness_norm,
    PetscReal mass_norm, PetscReal rigid_tolerance, Vec **templates,
    PetscInt *template_count, PetscInt *candidate_rank,
    PetscReal *largest_backward_error) {
    Vec candidate = NULL;
    Vec mass_candidate = NULL;
    Vec stiffness_candidate = NULL;
    Vec raw_basis[6] = {NULL, NULL, NULL, NULL, NULL, NULL};
    PetscScalar projected[36];
    PetscReal projected_values[6];
    PetscReal local_centroid[4] = {0.0, 0.0, 0.0, 0.0};
    PetscReal centroid[4] = {0.0, 0.0, 0.0, 0.0};
    PetscReal spectral_scale = stiffness_norm / mass_norm;
    PetscReal rank_relative =
        PetscMax(rigid_tolerance,
                 (PetscReal)128.0 * PETSC_MACHINE_EPSILON);
    PetscReal projected_floor = rank_relative * spectral_scale;
    PetscInt mode;

    PetscFunctionBegin;
    *templates = NULL;
    *template_count = 0;
    *candidate_rank = 0;
    *largest_backward_error = 0.0;
    memset(projected, 0, sizeof(projected));
    memset(projected_values, 0, sizeof(projected_values));

    for (PetscInt row = 0; row < selected_count; ++row) {
        const PetscInt local =
            selected_global[row] - (PetscInt)global_layout->row_start;

        PetscCheck(local >= 0 &&
                       local < (PetscInt)global_layout->local_size,
                   global_layout->comm, PETSC_ERR_PLIB,
                   "Rigid-template metadata row is not locally owned");
        for (PetscInt axis = 0; axis < 3; ++axis)
            local_centroid[axis] +=
                owned_coordinate[(size_t)3 * (size_t)local +
                                 (size_t)axis];
        local_centroid[3] += 1.0;
    }
    PetscCallMPI(MPI_Allreduce(local_centroid, centroid, 4, MPIU_REAL,
                               MPI_SUM, component_layout->comm));
    PetscCheck(centroid[3] > 0.0, component_layout->comm, PETSC_ERR_PLIB,
               "A rigid component has no coordinate metadata");
    for (PetscInt axis = 0; axis < 3; ++axis)
        centroid[axis] /= centroid[3];

    PetscCall(MatCreateVecs(mass, &candidate, NULL));
    PetscCall(VecDuplicate(candidate, &mass_candidate));
    PetscCall(VecDuplicate(candidate, &stiffness_candidate));
    for (mode = 0; mode < 6; ++mode) {
        PetscScalar *values = NULL;
        PetscScalar mass_energy = 0.0;
        PetscReal euclidean_norm = 0.0;

        PetscCall(VecZeroEntries(candidate));
        PetscCall(VecGetArray(candidate, &values));
        for (PetscInt row = 0; row < selected_count; ++row) {
            const PetscInt local =
                selected_global[row] - (PetscInt)global_layout->row_start;
            const PetscInt component = owned_component[local];
            const PetscReal x =
                owned_coordinate[(size_t)3 * (size_t)local] - centroid[0];
            const PetscReal y =
                owned_coordinate[(size_t)3 * (size_t)local + 1] - centroid[1];
            const PetscReal z =
                owned_coordinate[(size_t)3 * (size_t)local + 2] - centroid[2];
            PetscReal value = 0.0;

            if (mode < 3) {
                if (component == mode + 1) value = 1.0;
            } else if (mode == 3) {
                if (component == 2) value = -z;
                if (component == 3) value = y;
                if (component == 4) value = 1.0;
            } else if (mode == 4) {
                if (component == 1) value = z;
                if (component == 3) value = -x;
                if (component == 5) value = 1.0;
            } else {
                if (component == 1) value = -y;
                if (component == 2) value = x;
                if (component == 6) value = 1.0;
            }
            values[row] = (PetscScalar)value;
        }
        PetscCall(VecRestoreArray(candidate, &values));

        for (PetscInt pass = 0; pass < 2; ++pass) {
            PetscCall(MatMult(mass, candidate, mass_candidate));
            for (PetscInt basis = 0; basis < *candidate_rank; ++basis) {
                PetscScalar projection = 0.0;

                PetscCall(VecDot(raw_basis[basis], mass_candidate,
                                 &projection));
                PetscCall(VecAXPY(candidate, -projection,
                                  raw_basis[basis]));
            }
        }
        PetscCall(VecNorm(candidate, NORM_2, &euclidean_norm));
        if (euclidean_norm == 0.0) continue;
        PetscCall(MatMult(mass, candidate, mass_candidate));
        PetscCall(VecDot(candidate, mass_candidate, &mass_energy));
        if (PetscRealPart(mass_energy) <=
            rank_relative * mass_norm * euclidean_norm * euclidean_norm)
            continue;
        PetscCall(VecScale(
            candidate,
            (PetscScalar)(1.0 /
                          PetscSqrtReal(PetscRealPart(mass_energy)))));
        PetscCall(VecDuplicate(candidate, &raw_basis[*candidate_rank]));
        PetscCall(VecCopy(candidate, raw_basis[*candidate_rank]));
        ++*candidate_rank;
    }

    for (PetscInt column = 0; column < *candidate_rank; ++column) {
        PetscCall(MatMult(stiffness, raw_basis[column],
                          stiffness_candidate));
        for (PetscInt row = 0; row <= column; ++row) {
            PetscScalar value = 0.0;

            PetscCall(VecDot(raw_basis[row], stiffness_candidate, &value));
            projected[row + column * 6] = value;
            projected[column + row * 6] = PetscConj(value);
        }
    }
    if (*candidate_rank > 0) {
        PetscScalar compact[36];

        memset(compact, 0, sizeof(compact));
        for (PetscInt column = 0; column < *candidate_rank; ++column)
            for (PetscInt row = 0; row < *candidate_rank; ++row)
                compact[row + column * (*candidate_rank)] =
                    projected[row + column * 6];
        PetscCall(or_eig_small_symmetric_eigensystem(
            component_layout->comm, *candidate_rank, compact,
            projected_values));
        PetscCall(PetscCalloc1(*candidate_rank, templates));
        for (mode = 0; mode < *candidate_rank; ++mode) {
            Vec physical = NULL;
            PetscReal vector_norm = 0.0;
            PetscReal residual_norm = 0.0;
            PetscReal backward_error = 0.0;

            if (stiffness_norm > 0.0 &&
                PetscAbsReal(projected_values[mode]) > projected_floor)
                continue;
            PetscCall(VecDuplicate(raw_basis[0], &physical));
            PetscCall(VecZeroEntries(physical));
            for (PetscInt basis = 0; basis < *candidate_rank; ++basis)
                PetscCall(VecAXPY(
                    physical,
                    compact[basis + mode * (*candidate_rank)],
                    raw_basis[basis]));
            if (stiffness_norm > 0.0) {
                PetscCall(MatMult(stiffness, physical,
                                  stiffness_candidate));
                PetscCall(VecNorm(physical, NORM_2, &vector_norm));
                PetscCall(VecNorm(stiffness_candidate, NORM_2,
                                  &residual_norm));
                backward_error =
                    residual_norm / (stiffness_norm * vector_norm);
            }
            if (backward_error > rank_relative) {
                PetscCall(VecDestroy(&physical));
                continue;
            }
            (*templates)[*template_count] = physical;
            ++*template_count;
            *largest_backward_error =
                PetscMax(*largest_backward_error, backward_error);
        }
    }

    for (mode = 0; mode < 6; ++mode)
        PetscCall(VecDestroy(&raw_basis[mode]));
    PetscCall(VecDestroy(&stiffness_candidate));
    PetscCall(VecDestroy(&mass_candidate));
    PetscCall(VecDestroy(&candidate));
    PetscFunctionReturn(PETSC_SUCCESS);
}

/* Intersect the mechanically predicted rigid subspace with SLEPc's solved
   numerical-zero subspace.  Principal-angle eigenvalues near one are the
   directions on which both independent tests agree. */
static PetscErrorCode or_eig_intersect_rigid_subspaces(
    EPS eps, Mat mass, const PetscBool *is_numerical_zero,
    PetscInt converged, Vec *physical_templates,
    PetscInt physical_count, const or_eig_controls *controls,
    Vec **agreed_templates, PetscInt *agreed_count,
    PetscInt *numerical_count, PetscReal *overlap_squared) {
    Vec mass_templates[6] = {NULL, NULL, NULL, NULL, NULL, NULL};
    Vec mass_vector = NULL;
    BV invariant_basis = NULL;
    BV zero_basis = NULL;
    PetscScalar overlap[6];
    PetscScalar gram[36];
    PetscInt zero_column = 0;
    PetscInt mode;

    PetscFunctionBegin;
    *agreed_templates = NULL;
    *agreed_count = 0;
    *numerical_count = 0;
    memset(overlap, 0, sizeof(overlap));
    memset(gram, 0, sizeof(gram));
    for (mode = 0; mode < 6; ++mode) overlap_squared[mode] = 0.0;
    for (mode = 0; mode < converged; ++mode)
        if (is_numerical_zero[mode]) ++*numerical_count;
    if (*numerical_count == 0 || physical_count == 0)
        PetscFunctionReturn(PETSC_SUCCESS);

    /* Select the exact solver indices classified as numerical zero; their
       positions in SLEPc's internal invariant BV are not an ordering API.
       Explicit mass orthonormalization then removes the small loss of mutual
       orthogonality that reconstructed degenerate eigenvectors can acquire,
       keeping the strict principal-angle agreement test reproducible. */
    PetscCall(MatCreateVecs(mass, &mass_vector, NULL));
    PetscCall(EPSGetBV(eps, &invariant_basis));
    PetscCall(BVDuplicateResize(invariant_basis, *numerical_count,
                                &zero_basis));
    PetscCall(BVSetActiveColumns(zero_basis, 0, *numerical_count));
    for (mode = 0; mode < converged; ++mode) {
        Vec vector = NULL;

        if (!is_numerical_zero[mode]) continue;
        PetscCall(BVGetColumn(zero_basis, zero_column, &vector));
        PetscCall(EPSGetEigenvector(eps, mode, vector, NULL));
        PetscCall(BVRestoreColumn(zero_basis, zero_column, &vector));
        ++zero_column;
    }
    PetscCheck(zero_column == *numerical_count,
               PetscObjectComm((PetscObject)mass), PETSC_ERR_PLIB,
               "Numerical zero-vector selection count changed");
    PetscCall(BVSetMatrix(zero_basis, mass, PETSC_FALSE));
    PetscCall(BVOrthogonalize(zero_basis, NULL));
    for (PetscInt physical = 0; physical < physical_count; ++physical) {
        PetscCall(VecDuplicate(mass_vector, &mass_templates[physical]));
        PetscCall(MatMult(mass, physical_templates[physical],
                          mass_templates[physical]));
    }
    /* Accumulate the physical-subspace projection one mass-orthonormal basis
       vector at a time.  Temporary distributed storage is proportional only
       to the classified numerical-zero subspace, not to every converged
       positive mode. */
    for (mode = 0; mode < *numerical_count; ++mode) {
        Vec vector = NULL;
        PetscScalar mass_energy = 0.0;
        PetscReal inverse_mass_norm;

        PetscCall(BVGetColumn(zero_basis, mode, &vector));
        PetscCall(MatMult(mass, vector, mass_vector));
        PetscCall(VecDot(vector, mass_vector, &mass_energy));
        PetscCheck(PetscRealPart(mass_energy) > 0.0,
                   PetscObjectComm((PetscObject)mass), PETSC_ERR_PLIB,
                   "SLEPc returned a mass-null numerical zero vector");
        inverse_mass_norm =
            1.0 / PetscSqrtReal(PetscRealPart(mass_energy));
        for (PetscInt physical = 0; physical < physical_count; ++physical) {
            PetscCall(VecDot(vector, mass_templates[physical],
                             &overlap[physical]));
            overlap[physical] *= (PetscScalar)inverse_mass_norm;
        }
        for (PetscInt column = 0; column < physical_count; ++column) {
            for (PetscInt row = 0; row <= column; ++row) {
                const PetscScalar contribution =
                    PetscConj(overlap[row]) * overlap[column];

                gram[row + column * physical_count] += contribution;
                if (row != column)
                    gram[column + row * physical_count] +=
                        PetscConj(contribution);
            }
        }
        PetscCall(BVRestoreColumn(zero_basis, mode, &vector));
    }
    PetscCall(or_eig_small_symmetric_eigensystem(
        PetscObjectComm((PetscObject)mass), physical_count, gram,
        overlap_squared));
    PetscCall(PetscCalloc1(physical_count, agreed_templates));
    for (mode = 0; mode < physical_count; ++mode) {
        Vec agreed = NULL;

        if (overlap_squared[mode] <
            1.0 - or_eig_rigid_overlap_tolerance(controls))
            continue;
        PetscCall(VecDuplicate(physical_templates[0], &agreed));
        PetscCall(VecZeroEntries(agreed));
        for (PetscInt basis = 0; basis < physical_count; ++basis)
            PetscCall(VecAXPY(agreed,
                              gram[basis + mode * physical_count],
                              physical_templates[basis]));
        (*agreed_templates)[*agreed_count] = agreed;
        ++*agreed_count;
    }

    for (mode = 0; mode < 6; ++mode)
        PetscCall(VecDestroy(&mass_templates[mode]));
    PetscCall(BVDestroy(&zero_basis));
    PetscCall(VecDestroy(&mass_vector));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_retain_target_results(
    EPS eps, Mat stiffness, const or_eig_layout *layout,
    PetscReal lower, PetscReal upper, PetscInt positive_keep,
    PetscBool classify_zero_cluster, PetscReal spectral_scale,
    PetscReal zero_relative_tolerance, PetscBool retain_mode_shapes,
    or_eig_component_mode **modes, PetscInt *accepted) {
    Vec retained_vector = NULL;
    PetscInt *global_rows = NULL;
    or_eig_component_map *component_map = NULL;
    or_eig_pair *pairs = NULL;
    or_eig_pair *zero_order = NULL;
    or_eig_pair *output_pairs = NULL;
    PetscBool *is_numerical_zero = NULL;
    PetscReal *values = NULL;
    PetscInt converged;
    PetscInt selected = 0;
    PetscInt retained = 0;
    PetscInt numerical_zero_count = 0;
    PetscInt i;
    PetscReal roundoff_floor = 0.0;
    PetscReal candidate_ceiling = 0.0;
    PetscReal separation_ratio = 0.0;
    PetscLogDouble phase_started = 0.0;
    PetscErrorCode ierr = PETSC_SUCCESS;
    PetscErrorCode cleanup_ierr;

    PetscFunctionBegin;
    *modes = NULL;
    *accepted = 0;
    ierr = or_eig_timing_begin(layout->comm,
                               "eigenvalue collection, sorting and filtering",
                               &phase_started);
    if (ierr != PETSC_SUCCESS) goto cleanup;
    ierr = EPSGetConverged(eps, &converged);
    if (ierr != PETSC_SUCCESS) goto cleanup;
    PetscCheck(positive_keep >= 0, layout->comm, PETSC_ERR_ARG_SIZ,
               "Negative retained SLEPc mode count");
    ierr = PetscMalloc1(PetscMax(converged, 1), &pairs);
    if (ierr != PETSC_SUCCESS) goto cleanup;
    if (classify_zero_cluster) {
        ierr = PetscCalloc1(PetscMax(converged, 1), &is_numerical_zero);
        if (ierr != PETSC_SUCCESS) goto cleanup;
        ierr = PetscMalloc1(PetscMax(converged, 1), &zero_order);
        if (ierr != PETSC_SUCCESS) goto cleanup;
        ierr = PetscMalloc1(PetscMax(converged, 1), &values);
        if (ierr != PETSC_SUCCESS) goto cleanup;
    }
    ierr = PetscMalloc1(PetscMax(positive_keep, 1), &output_pairs);
    if (ierr != PETSC_SUCCESS) goto cleanup;
    for (i = 0; i < converged; ++i) {
        PetscScalar raw;
        PetscReal lambda;

        ierr = EPSGetEigenvalue(eps, i, &raw, NULL);
        if (ierr != PETSC_SUCCESS) goto cleanup;
        lambda = PetscRealPart(raw);
        PetscCheck(!PetscIsInfOrNanReal(lambda), layout->comm,
                   PETSC_ERR_NOT_CONVERGED,
                   "SLEPc returned a non-finite /EIG eigenvalue");
        pairs[i].value = lambda;
        pairs[i].index = i;
        if (values != NULL) values[i] = lambda;
    }
    if (classify_zero_cluster && converged > 0) {
        numerical_zero_count = or_eig_zero_cluster(
            values, converged, spectral_scale, zero_relative_tolerance, 0,
            zero_order, &roundoff_floor, &candidate_ceiling,
            &separation_ratio);
        for (i = 0; i < numerical_zero_count; ++i)
            is_numerical_zero[zero_order[i].index] = PETSC_TRUE;
        ierr = or_eig_logf(
            layout->comm,
            "Public /EIG fixed-target zero-cluster classification: "
            "roundoff_floor=%.16e, candidate_ceiling=%.16e, "
            "eigenvalue_gap=%.6e, zero=%" PetscInt_FMT
            ", converged=%" PetscInt_FMT "\n",
            (double)roundoff_floor, (double)candidate_ceiling,
            (double)separation_ratio, numerical_zero_count, converged);
        if (ierr != PETSC_SUCCESS) goto cleanup;
    }
    qsort(pairs, (size_t)converged, sizeof(*pairs), or_eig_compare_pair);
    for (i = 0; i < converged && selected < positive_keep; ++i) {
        if ((!classify_zero_cluster ||
             !is_numerical_zero[pairs[i].index]) &&
            pairs[i].value > 0.0 && pairs[i].value >= lower &&
            pairs[i].value <= upper)
            output_pairs[selected++] = pairs[i];
    }
    ierr = or_eig_timing_end(layout->comm,
                             "eigenvalue collection, sorting and filtering",
                             phase_started);
    if (ierr != PETSC_SUCCESS) goto cleanup;
    ierr = PetscCalloc1(PetscMax(selected, 1), modes);
    if (ierr != PETSC_SUCCESS) goto cleanup;
    if (retain_mode_shapes) {
        ierr = PetscMalloc1(PetscMax((PetscInt)layout->local_size, 1),
                            &global_rows);
        if (ierr != PETSC_SUCCESS) goto cleanup;
        for (PetscInt row = 0; row < (PetscInt)layout->local_size; ++row)
            global_rows[row] = (PetscInt)layout->row_start + row;
    }
    ierr = or_eig_timing_begin(layout->comm,
                               "fixed-target eigenvector retention",
                               &phase_started);
    if (ierr != PETSC_SUCCESS) goto cleanup;
    for (i = 0; i < selected; ++i) {
        PetscScalar eigenvalue;

        if (retain_mode_shapes) {
            ierr = MatCreateVecs(stiffness, &retained_vector, NULL);
            if (ierr != PETSC_SUCCESS) goto cleanup;
            ierr = EPSGetEigenpair(eps, output_pairs[i].index, &eigenvalue,
                                   NULL, retained_vector, NULL);
            if (ierr != PETSC_SUCCESS) goto cleanup;
            ierr = or_eig_component_map_attach(
                (PetscInt)layout->local_size, &global_rows, &component_map);
            if (ierr != PETSC_SUCCESS) goto cleanup;
        }
        (*modes)[i].value = output_pairs[i].value;
        (*modes)[i].component = 0;
        (*modes)[i].vector = retained_vector;
        (*modes)[i].map = component_map;
        retained_vector = NULL;
        ++retained;
    }
    ierr = or_eig_timing_end(layout->comm,
                             "fixed-target eigenvector retention",
                             phase_started);
    if (ierr != PETSC_SUCCESS) goto cleanup;
    *accepted = selected;
cleanup:
    cleanup_ierr = VecDestroy(&retained_vector);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    cleanup_ierr = PetscFree(global_rows);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    cleanup_ierr = PetscFree(pairs);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    cleanup_ierr = PetscFree(zero_order);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    cleanup_ierr = PetscFree(is_numerical_zero);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    cleanup_ierr = PetscFree(values);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    cleanup_ierr = PetscFree(output_pairs);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    if (ierr != PETSC_SUCCESS) {
        (void)or_eig_destroy_component_modes(modes, retained);
        *accepted = 0;
    }
    PetscFunctionReturn(ierr);
}

static int or_eig_compare_component_mode(const void *left,
                                         const void *right) {
    const or_eig_component_mode *a =
        (const or_eig_component_mode *)left;
    const or_eig_component_mode *b =
        (const or_eig_component_mode *)right;

    if (a->value != b->value)
        return (a->value > b->value) - (a->value < b->value);
    return (a->component > b->component) -
           (a->component < b->component);
}

/* Solve every eigenpair in each independent block of the final constrained
   K/M graph.  The small negative endpoint is only a factorization guard; it
   also lets SLEPc return roundoff-negative members of the numerical nullspace.
   Rigid/positive classification happens strictly after EPSSolve and therefore
   cannot change inertia counts, requested dimensions, or convergence. */
static PetscErrorCode or_eig_solve_component_intervals(
    Mat stiffness, Mat mass, const or_eig_layout *layout,
    int input_local_size, const int64_t *global_dof,
    const int *global_node, const int *global_component,
    const double *global_coordinate, PetscReal requested_lower,
    PetscReal requested_upper, PetscInt positive_limit,
    PetscBool retain_positive_modes, PetscBool retain_rigid_modes,
    PetscBool retain_mode_shapes, PetscBool rigid_only,
    const or_eig_controls *controls,
    or_eig_component_mode **positive_modes,
    PetscInt *positive_count, PetscInt *available_count,
    or_eig_component_mode **rigid_modes, PetscInt *rigid_count) {
    const PetscReal two_pi =
        (PetscReal)6.283185307179586476925286766559;
    PetscInt *owned_labels = NULL;
    PetscInt *component_ids = NULL;
    PetscInt *owned_component = NULL;
    PetscReal *owned_coordinate = NULL;
    PetscInt component_count = 0;
    or_eig_component_mode *collected = NULL;
    or_eig_component_mode *collected_rigid = NULL;
    PetscInt collected_count = 0;
    PetscInt collected_rigid_count = 0;
    PetscInt solved_components = 0;
    PetscInt empty_components = 0;
    PetscInt zero_stiffness_components = 0;
    PetscInt invalid_mass_components = 0;
    PetscInt total_interval_count = 0;
    PetscInt total_converged = 0;
    PetscInt total_direct_zero_dimension = 0;
    PetscInt total_numerical_zero = 0;
    PetscInt total_physical_rigid = 0;
    PetscInt total_agreed_rigid = 0;
    PetscInt total_zero_mechanisms = 0;
    PetscInt total_nonpositive = 0;
    PetscInt total_below_interval = 0;
    PetscInt total_above_interval = 0;
    PetscInt component_index;
    PetscLogDouble all_started = 0.0;

    PetscFunctionBegin;
    *positive_modes = NULL;
    *positive_count = 0;
    *available_count = 0;
    *rigid_modes = NULL;
    *rigid_count = 0;
    PetscCall(or_eig_timing_begin(
        layout->comm, "sequential component interval extraction",
        &all_started));
    PetscCall(or_eig_find_constraint_components(
        stiffness, mass, layout, input_local_size, global_dof, global_node,
        &owned_labels, &component_ids, &component_count));
    PetscCall(or_eig_build_owned_dof_metadata(
        layout, input_local_size, global_dof, global_component,
        global_coordinate, &owned_component, &owned_coordinate));
    PetscCheck(component_count > 0, layout->comm, PETSC_ERR_PLIB,
               "The constrained /EIG graph has no component to solve");

    for (component_index = 0; component_index < component_count;
         ++component_index) {
        PetscInt *selected_global = NULL;
        PetscInt selected_count = 0;
        PetscInt global_component_size = 0;
        PetscInt global_component_columns = 0;
        PetscInt component_row_start = 0;
        PetscInt component_row_end = 0;
        or_eig_component_map *component_map = NULL;
        IS selection = NULL;
        Mat component_stiffness = NULL;
        Mat component_mass = NULL;
        or_eig_layout component_layout = *layout;
        EPS component_eps = NULL;
        PetscReal *values = NULL;
        PetscBool *is_numerical_zero = NULL;
        or_eig_pair *ordered = NULL;
        PetscReal stiffness_norm = 0.0;
        PetscReal mass_norm = 0.0;
        Vec *physical_templates = NULL;
        Vec *agreed_templates = NULL;
        PetscInt physical_count = 0;
        PetscInt physical_candidate_rank = 0;
        PetscInt numerical_zero_count = 0;
        PetscInt agreed_count = 0;
        PetscReal largest_template_error = 0.0;
        PetscReal overlap_squared[6] = {0.0, 0.0, 0.0,
                                        0.0, 0.0, 0.0};
        PetscReal spectral_scale;
        PetscReal factorization_scale;
        PetscReal factorization_shift;
        PetscReal roundoff_floor = 0.0;
        PetscReal zero_candidate_ceiling = 0.0;
        PetscReal separation_ratio = 0.0;
        PetscReal solve_lower;
        PetscReal solve_upper =
            requested_upper +
            or_eig_boundary_margin(requested_upper, controls);
        const PetscReal accepted_lower =
            PetscMax((PetscReal)0.0,
                     requested_lower -
                         or_eig_acceptance_margin(requested_lower,
                                                  controls));
        const PetscReal accepted_upper =
            requested_upper +
            or_eig_acceptance_margin(requested_upper, controls);
        PetscInt interval_count = 0;
        PetscInt converged = 0;
        EPSConvergedReason reason;
        PetscInt component_positive_before = collected_count;
        PetscInt component_rigid_before = collected_rigid_count;
        PetscInt component_rigid_count = 0;
        PetscInt component_zero_mechanisms = 0;
        PetscInt component_direct_zero_dimension = 0;
        PetscInt component_eligible_positive = 0;
        PetscInt component_positive_retain = 0;
        PetscInt component_nonpositive = 0;
        PetscInt component_below_interval = 0;
        PetscInt component_above_interval = 0;
        const char *component_status = "solved";
        PetscInt mode;
        PetscLogDouble component_total_started = 0.0;
        PetscLogDouble stage_started = 0.0;
        PetscLogDouble stage_finished = 0.0;
        PetscLogDouble submatrix_seconds = 0.0;
        PetscLogDouble prediction_seconds = 0.0;
        PetscLogDouble setup_seconds = 0.0;
        PetscLogDouble solve_seconds = 0.0;
        or_eig_monitor_context monitor_context;
        char phase[160];

        PetscCall(PetscTime(&component_total_started));
        memset(&monitor_context, 0, sizeof(monitor_context));

        for (PetscInt row = 0; row < (PetscInt)layout->local_size; ++row)
            if (owned_labels[row] == component_ids[component_index])
                ++selected_count;
        PetscCall(PetscMalloc1(PetscMax(selected_count, 1),
                               &selected_global));
        selected_count = 0;
        for (PetscInt row = 0; row < (PetscInt)layout->local_size; ++row)
            if (owned_labels[row] == component_ids[component_index])
                selected_global[selected_count++] =
                    (PetscInt)layout->row_start + row;

        PetscCall(PetscSNPrintf(
            phase, sizeof(phase),
            "component %" PetscInt_FMT " submatrix construction",
            component_index + 1));
        PetscCall(or_eig_timing_begin(layout->comm, phase,
                                      &stage_started));
        if (component_count == 1) {
            /* Avoid duplicating the complete production K/M pair when the
               graph is connected.  Taking an extra PETSc reference keeps
               the common cleanup path correct without allocating matrix
               storage. */
            component_stiffness = stiffness;
            component_mass = mass;
            PetscCall(PetscObjectReference(
                (PetscObject)component_stiffness));
            PetscCall(PetscObjectReference((PetscObject)component_mass));
        } else {
            PetscCall(ISCreateGeneral(layout->comm, selected_count,
                                      selected_global, PETSC_COPY_VALUES,
                                      &selection));
            PetscCall(MatCreateSubMatrix(stiffness, selection, selection,
                                         MAT_INITIAL_MATRIX,
                                         &component_stiffness));
            PetscCall(MatCreateSubMatrix(mass, selection, selection,
                                         MAT_INITIAL_MATRIX,
                                         &component_mass));
        }
        PetscCall(MatSetOption(component_stiffness, MAT_SYMMETRIC,
                               PETSC_TRUE));
        PetscCall(MatSetOption(component_mass, MAT_SYMMETRIC, PETSC_TRUE));
        PetscCall(MatSetOption(component_mass, MAT_SPD, PETSC_TRUE));
        PetscCall(MatGetSize(component_stiffness, &global_component_size,
                             &global_component_columns));
        PetscCheck(global_component_size == global_component_columns,
                   layout->comm, PETSC_ERR_PLIB,
                   "A component eigensystem is not square");
        PetscCall(MatGetOwnershipRange(component_stiffness,
                                       &component_row_start,
                                       &component_row_end));
        PetscCheck(component_row_end - component_row_start == selected_count,
                   layout->comm, PETSC_ERR_PLIB,
                   "A component submatrix changed its local row layout");
        component_layout.global_size = global_component_size;
        component_layout.local_size = selected_count;
        component_layout.row_start = component_row_start;
        PetscCall(MatNorm(component_stiffness, NORM_INFINITY,
                          &stiffness_norm));
        PetscCall(MatNorm(component_mass, NORM_INFINITY, &mass_norm));
        PetscCheck(!PetscIsInfOrNanReal(stiffness_norm) &&
                       !PetscIsInfOrNanReal(mass_norm),
                   layout->comm, PETSC_ERR_ARG_WRONG,
                   "A component has a non-finite matrix norm");
        PetscCall(PetscTime(&stage_finished));
        submatrix_seconds = stage_finished - stage_started;
        PetscCall(or_eig_timing_end(layout->comm, phase, stage_started));
        if (mass_norm <= 0.0) {
            component_status = "invalid_mass";
            ++invalid_mass_components;
            PetscCall(or_eig_logf(
                layout->comm,
                "Public /EIG component %" PetscInt_FMT "/%" PetscInt_FMT
                " skipped: rows=%" PetscInt_FMT
                ", stiffness_norm=%.16e, mass_norm=%.16e; no finite "
                "generalized eigenproblem\n",
                component_index + 1, component_count,
                global_component_size, (double)stiffness_norm,
                (double)mass_norm));
            goto component_cleanup;
        }
        if (requested_lower <= 0.0) {
            PetscCall(PetscSNPrintf(
                phase, sizeof(phase),
                "component %" PetscInt_FMT
                " physical rigid prediction",
                component_index + 1));
            PetscCall(or_eig_timing_begin(layout->comm, phase,
                                          &stage_started));
            PetscCall(or_eig_predict_rigid_templates(
                component_stiffness, component_mass, layout,
                &component_layout, selected_count, selected_global,
                owned_component, owned_coordinate, stiffness_norm, mass_norm,
                controls->rigid_tolerance, &physical_templates,
                &physical_count, &physical_candidate_rank,
                &largest_template_error));
            PetscCall(PetscTime(&stage_finished));
            prediction_seconds = stage_finished - stage_started;
            PetscCall(or_eig_timing_end(layout->comm, phase, stage_started));
            PetscCall(or_eig_logf(
                layout->comm,
                "Public /EIG component %" PetscInt_FMT
                " physical rigid prediction: geometric_rank=%" PetscInt_FMT
                ", stiffness_nullity=%" PetscInt_FMT
                ", largest_backward_error=%.6e\n",
                component_index + 1, physical_candidate_rank,
                physical_count, (double)largest_template_error));
        }
        if (stiffness_norm <= 0.0) {
            /* The assembled mass matrix is required to be SPD, so every row
               in a principal component submatrix is mass-supported.  The old
               path explicitly constructed one dense set of coordinate basis
               vectors merely to rediscover this rank, which scaled
               quadratically in storage for a large K=0 component. */
            component_status = "zero_stiffness";
            ++zero_stiffness_components;
            component_rigid_count = physical_count;
            component_direct_zero_dimension = global_component_size;
            component_zero_mechanisms =
                global_component_size - physical_count;
            PetscCall(or_eig_logf(
                layout->comm,
                "Public /EIG component %" PetscInt_FMT "/%" PetscInt_FMT
                " zero-stiffness result: rows=%" PetscInt_FMT
                ", stiffness_norm=%.16e, mass_norm=%.16e"
                ", physical_rigid=%" PetscInt_FMT
                ", zero_mechanisms=%" PetscInt_FMT "\n",
                component_index + 1, component_count,
                global_component_size, (double)stiffness_norm,
                (double)mass_norm, physical_count,
                global_component_size - physical_count));
            if (retain_rigid_modes && physical_count > 0)
                PetscCall(PetscRealloc(
                    (size_t)(collected_rigid_count + physical_count) *
                        sizeof(*collected_rigid),
                    &collected_rigid));
            for (mode = 0; mode < physical_count; ++mode) {
                PetscCall(or_eig_logf(
                    layout->comm,
                    "Public /EIG accepted component %" PetscInt_FMT
                    " exact rigid mode %" PetscInt_FMT
                    " with stiffness backward error 0.000000e+00 "
                    "(raw eigenvalue 0.0000000000000000e+00)\n",
                    component_index + 1,
                    collected_rigid_count - component_rigid_before + 1));
                if (!retain_rigid_modes) {
                    ++collected_rigid_count;
                    continue;
                }
                collected_rigid[collected_rigid_count].value = 0.0;
                collected_rigid[collected_rigid_count].component =
                    component_index;
                if (retain_mode_shapes) {
                    PetscCall(or_eig_component_map_attach(
                        selected_count, &selected_global, &component_map));
                    collected_rigid[collected_rigid_count].vector =
                        physical_templates[mode];
                    collected_rigid[collected_rigid_count].map =
                        component_map;
                    physical_templates[mode] = NULL;
                } else {
                    collected_rigid[collected_rigid_count].vector = NULL;
                    collected_rigid[collected_rigid_count].map = NULL;
                }
                ++collected_rigid_count;
            }
            goto component_cleanup;
        }
        spectral_scale = stiffness_norm / mass_norm;
        factorization_scale =
            rigid_only ? spectral_scale
                       : PetscMax(spectral_scale, requested_upper);
        factorization_shift =
            PetscMax(PetscMax((PetscReal)128.0 * PETSC_MACHINE_EPSILON,
                              controls->upper_pivot_tolerance),
                     rigid_only ? controls->rigid_tolerance : (PetscReal)0.0) *
            factorization_scale;
        if (rigid_only) {
            /* Irigid=2 needs only the algebraic near-null band.  A tiny
               relative expansion keeps an eigenvalue exactly at the
               configured acceptance threshold away from a MUMPS endpoint. */
            factorization_shift =
                factorization_shift *
                    ((PetscReal)1.0 +
                     (PetscReal)128.0 * PETSC_MACHINE_EPSILON);
            solve_lower = -factorization_shift;
            solve_upper = factorization_shift;
        } else {
            solve_lower = requested_lower <= 0.0
                              ? -factorization_shift
                              : requested_lower -
                                    or_eig_boundary_margin(requested_lower,
                                                           controls);
        }
        PetscCall(or_eig_logf(
            layout->comm,
            "Public /EIG component %" PetscInt_FMT "/%" PetscInt_FMT
            ": rows=%" PetscInt_FMT
            ", spectral_scale=%.16e, factorization_interval=[%.16e, "
            "%.16e], requested_interval=[%.16e, %.16e]\n",
            component_index + 1, component_count, global_component_size,
            (double)spectral_scale, (double)solve_lower, (double)solve_upper,
            (double)requested_lower, (double)requested_upper));
        PetscCall(or_eig_create_interval_solver(
            component_stiffness, component_mass, solve_lower, solve_upper,
            &component_layout, controls, &component_eps));
        PetscCall(PetscSNPrintf(
            phase, sizeof(phase),
            "component %" PetscInt_FMT " EPS setup and interval count",
            component_index + 1));
        PetscCall(or_eig_timing_begin(layout->comm, phase,
                                      &stage_started));
        PetscCall(EPSSetUp(component_eps));
        PetscCall(or_eig_count_interval(component_eps, &interval_count));
        PetscCall(PetscTime(&stage_finished));
        setup_seconds = stage_finished - stage_started;
        PetscCall(or_eig_timing_end(layout->comm, phase, stage_started));
        PetscCall(or_eig_dump_mumps_diagnostics(component_eps,
                                                "after EPSSetUp/count"));
        if (interval_count == 0) {
            component_status = "empty_interval";
            ++empty_components;
            goto component_cleanup;
        }

        PetscCall(or_eig_report_solver_configuration(component_eps));
        PetscCall(PetscSNPrintf(
            phase, sizeof(phase),
            "component %" PetscInt_FMT " SLEPc EPSSolve",
            component_index + 1));
        PetscCall(or_eig_timing_begin(layout->comm, phase,
                                      &stage_started));
        if (or_eig_exhaustive_diagnostics) {
            monitor_context.communicator = layout->comm;
            monitor_context.component = component_index + 1;
            monitor_context.tolerance = controls->tolerance;
            monitor_context.solve_started = stage_started;
            monitor_context.previous_iteration = stage_started;
            PetscCall(EPSMonitorSet(component_eps,
                                    or_eig_convergence_monitor,
                                    &monitor_context, NULL));
        }
        PetscCall(EPSSolve(component_eps));
        PetscCall(PetscTime(&stage_finished));
        solve_seconds = stage_finished - stage_started;
        PetscCall(or_eig_timing_end(layout->comm, phase, stage_started));
        PetscCall(or_eig_dump_mumps_diagnostics(component_eps,
                                                "after EPSSolve"));
        PetscCall(EPSGetConvergedReason(component_eps, &reason));
        PetscCall(EPSGetConverged(component_eps, &converged));
        PetscCheck(reason >= 0 && converged >= interval_count,
                   layout->comm, PETSC_ERR_NOT_CONVERGED,
                   "A component SLEPc interval solve did not converge");
        ++solved_components;

        PetscCall(PetscCalloc1(converged, &values));
        PetscCall(PetscCalloc1(converged, &is_numerical_zero));
        PetscCall(PetscMalloc1(converged, &ordered));
        for (mode = 0; mode < converged; ++mode) {
            PetscScalar raw;

            PetscCall(EPSGetEigenvalue(component_eps, mode, &raw, NULL));
            values[mode] = PetscRealPart(raw);
            PetscCheck(!PetscIsInfOrNanReal(values[mode]), layout->comm,
                       PETSC_ERR_NOT_CONVERGED,
                       "SLEPc returned a non-finite component eigenvalue");
        }
        if (rigid_only) {
            /* Every value in this deliberately tolerance-sized interval is a
               numerical near-null candidate.  The physical-template/SLEPc
               subspace intersection below remains the acceptance gate and
               rejects mechanisms or weak flexible modes. */
            numerical_zero_count = converged;
            roundoff_floor = PETSC_MACHINE_EPSILON * spectral_scale;
            zero_candidate_ceiling = factorization_shift;
            PetscCall(or_eig_logf(
                layout->comm,
                "Public /EIG component %" PetscInt_FMT
                " rigid-only candidate band: |eigenvalue| <= %.16e, "
                "candidates=%" PetscInt_FMT "\n",
                component_index + 1, (double)factorization_shift,
                converged));
        } else if (requested_lower <= 0.0) {
            numerical_zero_count = or_eig_zero_cluster(
                values, converged, spectral_scale,
                controls->rigid_tolerance, physical_count, ordered,
                &roundoff_floor, &zero_candidate_ceiling,
                &separation_ratio);
        }
        if (rigid_only) {
            for (mode = 0; mode < converged; ++mode)
                is_numerical_zero[mode] = PETSC_TRUE;
        } else {
            for (mode = 0; mode < numerical_zero_count; ++mode)
                is_numerical_zero[ordered[mode].index] = PETSC_TRUE;
        }
        PetscCall(or_eig_logf(
            layout->comm,
            "Public /EIG component %" PetscInt_FMT
            " zero-cluster classification: roundoff_floor=%.16e, "
            "candidate_ceiling=%.16e, eigenvalue_gap=%.6e, rigid=%"
            PetscInt_FMT
            ", converged=%" PetscInt_FMT "\n",
            component_index + 1, (double)roundoff_floor,
            (double)zero_candidate_ceiling, (double)separation_ratio,
            numerical_zero_count, converged));

        PetscCall(or_eig_intersect_rigid_subspaces(
            component_eps, component_mass, is_numerical_zero, converged,
            physical_templates, physical_count, controls,
            &agreed_templates, &agreed_count, &numerical_zero_count,
            overlap_squared));
        component_rigid_count = agreed_count;
        component_zero_mechanisms = numerical_zero_count - agreed_count;
        PetscCall(or_eig_logf(
            layout->comm,
            "Public /EIG component %" PetscInt_FMT
            " rigid AND gate: slepc_zero=%" PetscInt_FMT
            ", physical_rigid=%" PetscInt_FMT
            ", agreed=%" PetscInt_FMT
            ", zero_mechanisms=%" PetscInt_FMT
            ", overlap_tolerance=%.6e\n",
            component_index + 1, numerical_zero_count, physical_count,
            agreed_count, numerical_zero_count - agreed_count,
            (double)or_eig_rigid_overlap_tolerance(controls)));
        if (physical_count > 0) {
            PetscCall(or_eig_logf(layout->comm,
                                  "Public /EIG component %" PetscInt_FMT
                                  " rigid overlap squared:",
                                  component_index + 1));
            for (mode = 0; mode < physical_count; ++mode)
                PetscCall(or_eig_logf(layout->comm, " %.6e",
                                      (double)overlap_squared[mode]));
            PetscCall(or_eig_logf(layout->comm, "\n"));
        }

        for (mode = 0; mode < converged; ++mode) {
            PetscReal backward_error = 0.0;
            PetscReal relative_error = 0.0;
            PetscReal signed_frequency = 0.0;
            const char *disposition = "eligible_positive";

            PetscCall(EPSComputeError(component_eps, mode,
                                      EPS_ERROR_BACKWARD,
                                      &backward_error));
            PetscCall(EPSComputeError(component_eps, mode,
                                      EPS_ERROR_RELATIVE,
                                      &relative_error));
            if (values[mode] != 0.0)
                signed_frequency =
                    PetscSqrtReal(PetscAbsReal(values[mode])) / two_pi;
            if (values[mode] < 0.0)
                signed_frequency = -signed_frequency;
            if (is_numerical_zero[mode]) {
                disposition = "slepc_numerical_zero";
            } else if (values[mode] <= 0.0) {
                disposition = "rejected_nonpositive";
                ++component_nonpositive;
            } else if (values[mode] < accepted_lower) {
                disposition = "rejected_below_interval";
                ++component_below_interval;
            } else if (values[mode] > accepted_upper) {
                disposition = "rejected_above_interval";
                ++component_above_interval;
            } else {
                ++component_eligible_positive;
            }
            PetscCall(or_eig_logf(
                layout->comm,
                "Public /EIG component %" PetscInt_FMT
                " mode %" PetscInt_FMT "/%" PetscInt_FMT
                ": eigenvalue=%.16e, signed_frequency_hz=%.16e"
                ", backward_error=%.6e, tolerance_ratio=%.6e"
                ", relative_error=%.6e"
                ", classification=%s\n",
                component_index + 1, mode + 1, converged,
                (double)values[mode], (double)signed_frequency,
                (double)backward_error,
                (double)(backward_error / controls->tolerance),
                (double)relative_error,
                disposition));
        }

        if (retain_rigid_modes && agreed_count > 0)
            PetscCall(PetscRealloc(
                (size_t)(collected_rigid_count + agreed_count) *
                    sizeof(*collected_rigid),
                &collected_rigid));
        for (mode = 0; mode < agreed_count; ++mode) {
            PetscCall(or_eig_logf(
                layout->comm,
                "Public /EIG accepted component %" PetscInt_FMT
                " rigid mode %" PetscInt_FMT
                " by SLEPc/template subspace agreement\n",
                component_index + 1,
                collected_rigid_count - component_rigid_before + 1));
            if (!retain_rigid_modes) {
                ++collected_rigid_count;
                continue;
            }
            collected_rigid[collected_rigid_count].value = 0.0;
            collected_rigid[collected_rigid_count].component =
                component_index;
            if (retain_mode_shapes) {
                PetscCall(or_eig_component_map_attach(
                    selected_count, &selected_global, &component_map));
                collected_rigid[collected_rigid_count].vector =
                    agreed_templates[mode];
                collected_rigid[collected_rigid_count].map = component_map;
                agreed_templates[mode] = NULL;
            } else {
                collected_rigid[collected_rigid_count].vector = NULL;
                collected_rigid[collected_rigid_count].map = NULL;
            }
            ++collected_rigid_count;
        }

        *available_count += component_eligible_positive;
        component_positive_retain = component_eligible_positive;
        if (positive_limit > 0)
            component_positive_retain =
                PetscMin(component_positive_retain, positive_limit);
        if (retain_positive_modes && component_positive_retain > 0)
            PetscCall(PetscRealloc(
                (size_t)(collected_count + component_positive_retain) *
                    sizeof(*collected),
                &collected));
        component_positive_retain = 0;
        for (mode = 0; mode < converged; ++mode) {
            if (is_numerical_zero[mode]) continue;
            if (values[mode] <= 0.0 || values[mode] < accepted_lower ||
                values[mode] > accepted_upper)
                continue;
            ordered[component_positive_retain].value = values[mode];
            ordered[component_positive_retain].index = mode;
            ++component_positive_retain;
        }
        qsort(ordered, (size_t)component_positive_retain,
              sizeof(*ordered), or_eig_compare_pair);
        if (positive_limit > 0)
            component_positive_retain =
                PetscMin(component_positive_retain, positive_limit);
        if (retain_positive_modes) {
            for (mode = 0; mode < component_positive_retain; ++mode) {
                Vec retained_vector = NULL;

                if (retain_mode_shapes) {
                    PetscScalar raw;

                    PetscCall(MatCreateVecs(component_stiffness,
                                            &retained_vector, NULL));
                    PetscCall(EPSGetEigenpair(
                        component_eps, ordered[mode].index, &raw, NULL,
                        retained_vector, NULL));
                    PetscCall(or_eig_component_map_attach(
                        selected_count, &selected_global, &component_map));
                }
                collected[collected_count].value = ordered[mode].value;
                collected[collected_count].component = component_index;
                collected[collected_count].vector = retained_vector;
                collected[collected_count].map = component_map;
                ++collected_count;
            }
        }

component_cleanup:
        total_interval_count += interval_count;
        total_converged += converged;
        total_direct_zero_dimension += component_direct_zero_dimension;
        total_numerical_zero += numerical_zero_count;
        total_physical_rigid += physical_count;
        total_agreed_rigid += component_rigid_count;
        total_zero_mechanisms += component_zero_mechanisms;
        total_nonpositive += component_nonpositive;
        total_below_interval += component_below_interval;
        total_above_interval += component_above_interval;
        PetscCall(PetscTime(&stage_finished));
        PetscCall(or_eig_logf(
            layout->comm,
            "Public /EIG component summary %" PetscInt_FMT "/%"
            PetscInt_FMT ": status=%s, rows=%" PetscInt_FMT
            ", interval_count=%" PetscInt_FMT ", converged=%" PetscInt_FMT
            ", direct_zero_dimension=%" PetscInt_FMT
            ", physical_rigid=%" PetscInt_FMT ", slepc_zero=%"
            PetscInt_FMT ", agreed_rigid=%" PetscInt_FMT
            ", zero_mechanisms=%" PetscInt_FMT
            ", eligible_positive=%" PetscInt_FMT
            ", stored_positive=%" PetscInt_FMT
            ", rejected_nonpositive=%" PetscInt_FMT
            ", rejected_below=%" PetscInt_FMT
            ", rejected_above=%" PetscInt_FMT
            ", seconds={submatrix:%.6f,prediction:%.6f,setup:%.6f,"
            "solve:%.6f,total:%.6f}\n",
            component_index + 1, component_count, component_status,
            global_component_size, interval_count, converged,
            component_direct_zero_dimension, physical_count,
            numerical_zero_count, component_rigid_count,
            component_zero_mechanisms, component_eligible_positive,
            collected_count - component_positive_before,
            component_nonpositive, component_below_interval,
            component_above_interval, (double)submatrix_seconds,
            (double)prediction_seconds, (double)setup_seconds,
            (double)solve_seconds,
            (double)(stage_finished - component_total_started)));
        for (mode = 0; mode < agreed_count; ++mode)
            PetscCall(VecDestroy(&agreed_templates[mode]));
        PetscCall(PetscFree(agreed_templates));
        for (mode = 0; mode < physical_count; ++mode)
            PetscCall(VecDestroy(&physical_templates[mode]));
        PetscCall(PetscFree(physical_templates));
        PetscCall(PetscFree(ordered));
        PetscCall(PetscFree(is_numerical_zero));
        PetscCall(PetscFree(values));
        PetscCall(EPSDestroy(&component_eps));
        PetscCall(MatDestroy(&component_mass));
        PetscCall(MatDestroy(&component_stiffness));
        PetscCall(ISDestroy(&selection));
        PetscCall(PetscFree(selected_global));

        /* A bounded request needs only the globally smallest N candidates.
           Prune after every sequential component so retained vector storage
           never grows to N times the number of disconnected components. */
        if (retain_positive_modes && positive_limit > 0 &&
            collected_count > positive_limit) {
            qsort(collected, (size_t)collected_count,
                  sizeof(*collected), or_eig_compare_component_mode);
            for (mode = positive_limit; mode < collected_count; ++mode)
                PetscCall(or_eig_component_mode_destroy(&collected[mode]));
            collected_count = positive_limit;
        }
    }

    {
        const PetscInt keep =
            positive_limit > 0
                ? PetscMin(positive_limit, collected_count)
                : collected_count;

        if (collected_count > 1)
            qsort(collected, (size_t)collected_count, sizeof(*collected),
                  or_eig_compare_component_mode);
        for (PetscInt mode = 0; mode < collected_count; ++mode)
            PetscCall(or_eig_logf(
                layout->comm,
                "Public /EIG global positive mode %" PetscInt_FMT "/%"
                PetscInt_FMT ": component=%" PetscInt_FMT
                ", eigenvalue=%.16e, frequency_hz=%.16e, selected=%d\n",
                mode + 1, collected_count,
                collected[mode].component + 1,
                (double)collected[mode].value,
                (double)(PetscSqrtReal(collected[mode].value) / two_pi),
                mode < keep ? 1 : 0));
        if (keep > 0) {
            PetscCall(PetscMalloc1(keep, positive_modes));
            for (PetscInt mode = 0; mode < keep; ++mode) {
                (*positive_modes)[mode] = collected[mode];
                collected[mode].vector = NULL;
                collected[mode].map = NULL;
            }
        }
        for (PetscInt mode = keep; mode < collected_count; ++mode)
            PetscCall(or_eig_component_mode_destroy(&collected[mode]));
        *positive_count = keep;
        PetscCall(or_eig_logf(
            layout->comm,
            "Public /EIG merged %" PetscInt_FMT
            " positive component modes and retained %" PetscInt_FMT
            " after the global limit\n",
            collected_count, keep));
    }
    if (retain_rigid_modes && collected_rigid_count > 0) {
        if (collected_rigid_count > 1)
            qsort(collected_rigid, (size_t)collected_rigid_count,
                  sizeof(*collected_rigid), or_eig_compare_component_mode);
        PetscCall(PetscMalloc1(collected_rigid_count, rigid_modes));
        for (PetscInt mode = 0; mode < collected_rigid_count; ++mode) {
            PetscCall(or_eig_logf(
                layout->comm,
                "Public /EIG global rigid mode %" PetscInt_FMT "/%"
                PetscInt_FMT ": component=%" PetscInt_FMT
                ", eigenvalue=0.0000000000000000e+00"
                ", frequency_hz=0.0000000000000000e+00\n",
                mode + 1, collected_rigid_count,
                collected_rigid[mode].component + 1));
            (*rigid_modes)[mode] = collected_rigid[mode];
            collected_rigid[mode].vector = NULL;
            collected_rigid[mode].map = NULL;
        }
    }
    *rigid_count = collected_rigid_count;
    PetscCall(or_eig_logf(
        layout->comm,
        "Public /EIG classified %" PetscInt_FMT
        " rigid modes from the solved component spectra\n",
        collected_rigid_count));
    PetscCall(or_eig_logf(
        layout->comm,
        "Public /EIG global component summary: components=%" PetscInt_FMT
        ", solved=%" PetscInt_FMT ", empty_interval=%" PetscInt_FMT
        ", zero_stiffness=%" PetscInt_FMT ", invalid_mass=%" PetscInt_FMT
        ", inertia_interval_total=%" PetscInt_FMT
        ", converged_total=%" PetscInt_FMT
        ", direct_zero_dimension=%" PetscInt_FMT
        ", slepc_zero=%" PetscInt_FMT
        ", physical_rigid=%" PetscInt_FMT
        ", agreed_rigid=%" PetscInt_FMT
        ", zero_mechanisms=%" PetscInt_FMT
        ", eligible_positive=%" PetscInt_FMT
        ", stored_positive=%" PetscInt_FMT
        ", retained_positive=%" PetscInt_FMT
        ", rejected_nonpositive=%" PetscInt_FMT
        ", rejected_below=%" PetscInt_FMT
        ", rejected_above=%" PetscInt_FMT "\n",
        component_count, solved_components, empty_components,
        zero_stiffness_components, invalid_mass_components,
        total_interval_count, total_converged,
        total_direct_zero_dimension, total_numerical_zero,
        total_physical_rigid, total_agreed_rigid,
        total_zero_mechanisms, *available_count, collected_count,
        *positive_count, total_nonpositive, total_below_interval,
        total_above_interval));
    PetscCall(PetscFree(collected_rigid));
    PetscCall(PetscFree(collected));
    PetscCall(PetscFree(component_ids));
    PetscCall(PetscFree(owned_labels));
    PetscCall(PetscFree(owned_coordinate));
    PetscCall(PetscFree(owned_component));
    PetscCall(or_eig_timing_end(
        layout->comm, "sequential component interval extraction",
        all_started));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_transfer_component_results(
    const or_eig_layout *layout, int input_local_size,
    const int64_t *global_dof, PetscInt mode_count,
    const or_eig_component_mode *modes, double *evals,
    double *local_evecs) {
    double *owned = NULL;
    const PetscScalar *vector_values = NULL;
    size_t value_count = 1;
    PetscInt mode = 0;
    PetscErrorCode ierr = PETSC_SUCCESS;
    PetscErrorCode cleanup_ierr;

    PetscFunctionBegin;
    if (mode_count == 0) PetscFunctionReturn(PETSC_SUCCESS);
    if (layout->local_size > 0) {
        PetscCheck((size_t)mode_count <=
                       SIZE_MAX / (size_t)layout->local_size,
                   layout->comm, PETSC_ERR_MEM,
                   "Component result buffer extent overflow");
        value_count =
            (size_t)mode_count * (size_t)layout->local_size;
    }
    ierr = PetscCalloc1(value_count, &owned);
    if (ierr != PETSC_SUCCESS) goto cleanup;
    for (mode = 0; mode < mode_count; ++mode) {
        PetscInt component_local_size = 0;

        if (evals != NULL) evals[mode] = (double)modes[mode].value;
        if (modes[mode].map == NULL || modes[mode].vector == NULL) {
            ierr = PETSC_ERR_PLIB;
            goto cleanup;
        }
        ierr = VecGetLocalSize(modes[mode].vector,
                               &component_local_size);
        if (ierr != PETSC_SUCCESS) goto cleanup;
        if (component_local_size != modes[mode].map->local_size) {
            ierr = PETSC_ERR_PLIB;
            goto cleanup;
        }
        ierr = VecGetArrayRead(modes[mode].vector, &vector_values);
        if (ierr != PETSC_SUCCESS) goto cleanup;
        for (PetscInt row = 0; row < component_local_size; ++row) {
            const PetscInt local =
                modes[mode].map->global_rows[row] -
                (PetscInt)layout->row_start;

            if (local < 0 || local >= (PetscInt)layout->local_size) {
                ierr = PETSC_ERR_PLIB;
                goto cleanup;
            }
            owned[(size_t)mode * (size_t)layout->local_size +
                  (size_t)local] =
                (double)PetscRealPart(vector_values[row]);
        }
        ierr = VecRestoreArrayRead(modes[mode].vector, &vector_values);
        vector_values = NULL;
        if (ierr != PETSC_SUCCESS) goto cleanup;
    }
    if (or_eig_expand_owned_vectors(
            layout, input_local_size, global_dof, (int)mode_count, owned,
            local_evecs) != 0)
        ierr = PETSC_ERR_LIB;

cleanup:
    if (vector_values != NULL) {
        cleanup_ierr =
            VecRestoreArrayRead(modes[mode].vector, &vector_values);
        if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    }
    cleanup_ierr = PetscFree(owned);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    PetscFunctionReturn(ierr);
}

static PetscErrorCode or_eig_log_retained_storage(
    const or_eig_result_context *context) {
    PetscLogDouble local_bytes = 0.0;
    PetscLogDouble global_bytes = 0.0;
    PetscLogDouble maximum_bytes = 0.0;

    PetscFunctionBegin;
    for (PetscInt mode = 0;
         mode < context->component_positive_count; ++mode) {
        PetscInt local_size = 0;

        if (context->component_positive_modes[mode].vector == NULL) continue;
        PetscCall(VecGetLocalSize(
            context->component_positive_modes[mode].vector, &local_size));
        local_bytes +=
            (PetscLogDouble)local_size * (PetscLogDouble)sizeof(PetscScalar);
    }
    for (PetscInt mode = 0; mode < context->rigid_count; ++mode) {
        PetscInt local_size = 0;

        if (context->rigid_modes[mode].vector == NULL) continue;
        PetscCall(VecGetLocalSize(context->rigid_modes[mode].vector,
                                  &local_size));
        local_bytes +=
            (PetscLogDouble)local_size * (PetscLogDouble)sizeof(PetscScalar);
    }
    PetscCallMPI(MPI_Allreduce(&local_bytes, &global_bytes, 1, MPI_DOUBLE,
                               MPI_SUM, context->layout.comm));
    PetscCallMPI(MPI_Allreduce(&local_bytes, &maximum_bytes, 1, MPI_DOUBLE,
                               MPI_MAX, context->layout.comm));
    PetscCall(or_eig_logf(
        context->layout.comm,
        "Public /EIG retained distributed mode storage: "
        "metadata_positive=%" PetscInt_FMT ", metadata_rigid=%" PetscInt_FMT
        ", mode_shapes=%s, vector_global_mib=%.3f, "
        "vector_maximum_rank_mib=%.3f, "
        "full_global_equivalent_mib=%.3f\n",
        context->component_positive_count, context->rigid_count,
        context->mode_shapes_retained ? "enabled" : "disabled",
        (double)(global_bytes / (1024.0 * 1024.0)),
        (double)(maximum_bytes / (1024.0 * 1024.0)),
        (double)(((PetscLogDouble)context->layout.global_size *
                  (PetscLogDouble)(context->component_positive_count +
                                   context->rigid_count) *
                  (PetscLogDouble)sizeof(PetscScalar)) /
                 (1024.0 * 1024.0))));
    PetscFunctionReturn(PETSC_SUCCESS);
}

static PetscErrorCode or_eig_result_context_destroy(
    or_eig_result_context *context) {
    PetscErrorCode ierr = PETSC_SUCCESS;
    PetscErrorCode cleanup_ierr;

    PetscFunctionBegin;
    if (context == NULL) PetscFunctionReturn(PETSC_SUCCESS);
    cleanup_ierr = or_eig_destroy_component_modes(
        &context->component_positive_modes,
        context->component_positive_count);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    cleanup_ierr = or_eig_destroy_component_modes(
        &context->rigid_modes, context->rigid_count);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    cleanup_ierr = PetscFree(context->input_global_dof);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    or_eig_layout_destroy(&context->layout);
    if (context->initialized_here) {
        cleanup_ierr = SlepcFinalize();
        if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    }
    free(context);
    PetscFunctionReturn(ierr);
}

void eig_slepc_result_metadata_c_(
    int64_t *solver_handle, int *capacity, double *evals,
    int *nfound, int *info) {
    or_eig_result_context *context;
    PetscInt required;

    *info = 0;
    *nfound = 0;
    if (*solver_handle == 0 || *capacity < 1) {
        *info = 20;
        return;
    }
    context = (or_eig_result_context *)(intptr_t)*solver_handle;
    required = context->component_positive_count + context->rigid_count;
    if (required < 1 || required > (PetscInt)*capacity) {
        *info = 20;
        return;
    }
    for (PetscInt mode = 0;
         mode < context->component_positive_count; ++mode)
        evals[mode] =
            (double)context->component_positive_modes[mode].value;
    for (PetscInt mode = 0; mode < context->rigid_count; ++mode)
        evals[context->component_positive_count + mode] = 0.0;
    *nfound = (int)required;
}

void eig_slepc_result_batch_c_(
    int64_t *solver_handle, int *first_mode, int *mode_count,
    double *local_evecs, int *info) {
    or_eig_result_context *context;
    PetscInt first;
    PetscInt count;
    PetscInt positive_count;
    PetscInt rigid_first;
    PetscInt rigid_count;
    PetscLogDouble phase_started = 0.0;
    PetscErrorCode ierr = PETSC_SUCCESS;

    *info = 0;
    if (*solver_handle == 0 || *first_mode < 1 || *mode_count < 1) {
        *info = 20;
        return;
    }
    context = (or_eig_result_context *)(intptr_t)*solver_handle;
    if (!context->mode_shapes_retained) {
        *info = 20;
        return;
    }
    first = (PetscInt)*first_mode - 1;
    count = (PetscInt)*mode_count;
    {
        const PetscInt total = context->component_positive_count +
                               context->rigid_count;

        if (first >= total || count > total - first) {
            *info = 20;
            return;
        }
    }
    ierr = or_eig_timing_begin(context->layout.comm,
                               "distributed result batch transfer",
                               &phase_started);
    if (ierr != PETSC_SUCCESS) goto failure;
    positive_count =
        first < context->component_positive_count
            ? PetscMin(count, context->component_positive_count - first)
            : 0;
    if (positive_count > 0) {
        ierr = or_eig_transfer_component_results(
            &context->layout, context->input_local_size,
            context->input_global_dof, positive_count,
            context->component_positive_modes + first, NULL,
            local_evecs);
        if (ierr != PETSC_SUCCESS) goto failure;
    }
    rigid_count = count - positive_count;
    if (rigid_count > 0) {
        rigid_first = first + positive_count -
                      context->component_positive_count;
        ierr = or_eig_transfer_component_results(
            &context->layout, context->input_local_size,
            context->input_global_dof, rigid_count,
            context->rigid_modes + rigid_first, NULL,
            local_evecs +
                (size_t)positive_count *
                    (size_t)context->input_local_size);
        if (ierr != PETSC_SUCCESS) goto failure;
    }
    ierr = or_eig_timing_end(context->layout.comm,
                             "distributed result batch transfer",
                             phase_started);
    if (ierr != PETSC_SUCCESS) goto failure;
    ierr = or_eig_logf(
        context->layout.comm,
        "Public /EIG transferred retained mode batch %d through %d "
        "from distributed storage\n",
        *first_mode, *first_mode + *mode_count - 1);
    if (ierr == PETSC_SUCCESS) return;

failure:
    *info = 2000 + (int)ierr;
}

void eig_slepc_result_release_c_(int64_t *solver_handle, int *info) {
    or_eig_result_context *context;
    PetscErrorCode ierr = PETSC_SUCCESS;
    PetscErrorCode cleanup_ierr;

    *info = 0;
    if (*solver_handle == 0) return;
    context = (or_eig_result_context *)(intptr_t)*solver_handle;
    *solver_handle = 0;
    ierr = or_eig_timing_end(context->layout.comm,
                             "complete SLEPc eigensolver C path",
                             context->timing_started);
    cleanup_ierr = or_eig_result_context_destroy(context);
    if (ierr == PETSC_SUCCESS) ierr = cleanup_ierr;
    if (ierr != PETSC_SUCCESS) *info = 2000 + (int)ierr;
}

void eig_solve_slepc_c_(
    int *local_size, int *global_size, int *owned_size, int64_t *row_start,
    int *communicator, int64_t *global_dof, int *global_node,
    int *global_component, double *global_coordinate,
    int *k_row, int *k_col, double *k_diag,
    double *k_lower, int *m_row, int *m_col, double *m_diag, double *m_lower,
    int *nreq, int *range_all, int *output_rigid_modes,
    int *retain_mode_shapes, double *shift,
    double *cut_frequency, double *tolerance, double *rigid_tolerance,
    double *upper_pivot_tolerance, double *positive_pivot_tolerance,
    int *maximum_iterations, int *subspace_factor, int64_t *solver_handle,
    double *evals, double *local_evecs, int *nfound, int *info) {
    const PetscReal two_pi =
        (PetscReal)6.283185307179586476925286766559;
    or_eig_layout layout;
    or_eig_dist_matrix distributed_stiffness;
    or_eig_dist_matrix distributed_mass;
    or_eig_controls controls;
    or_eig_result_context *saved = NULL;
    Mat stiffness = NULL;
    Mat mass = NULL;
    EPS eps = NULL;
    or_eig_component_mode *rigid_modes = NULL;
    or_eig_component_mode *component_positive_modes = NULL;
    PetscBool initialized_here = PETSC_FALSE;
    PetscBool was_initialized = PETSC_FALSE;
    PetscInt rigid_count = 0;
    PetscInt component_positive_count = 0;
    PetscInt available = 0;
    PetscInt accepted = 0;
    PetscInt solve_required = (PetscInt)*nreq;
    PetscLogDouble phase_started = 0.0;
    PetscLogDouble total_started = 0.0;
    PetscErrorCode ierr = PETSC_SUCCESS;
    int assembly_error;

    *info = 0;
    *nfound = 0;
    /* SLEPc results are returned through the retained-context API for both
       fixed-target and interval extraction. These legacy direct-result
       arguments remain in the cross-language ABI for the dense backend. */
    (void)evals;
    (void)local_evecs;
    memset(&layout, 0, sizeof(layout));
    memset(&distributed_stiffness, 0, sizeof(distributed_stiffness));
    memset(&distributed_mass, 0, sizeof(distributed_mass));
    controls.tolerance =
        *tolerance > 0.0 && isfinite(*tolerance) ? *tolerance : 1.0e-8;
    controls.rigid_tolerance =
        *rigid_tolerance > 0.0 && isfinite(*rigid_tolerance)
            ? *rigid_tolerance
            : 1.0e-12;
    controls.upper_pivot_tolerance =
        *upper_pivot_tolerance > 0.0 && isfinite(*upper_pivot_tolerance)
            ? *upper_pivot_tolerance
            : 1.0e-12;
    controls.positive_pivot_tolerance =
        *positive_pivot_tolerance > 0.0 &&
                isfinite(*positive_pivot_tolerance)
            ? *positive_pivot_tolerance
            : 1.0e-12;
    controls.maximum_iterations =
        *maximum_iterations > 0 ? (PetscInt)*maximum_iterations : 300;
    controls.subspace_factor =
        *subspace_factor > 0 ? (PetscInt)*subspace_factor : 2;

    if (*nreq < 1 || *range_all < 0 || *range_all > 3 ||
        *output_rigid_modes < 0 || *output_rigid_modes > 2 ||
        *retain_mode_shapes < 0 || *retain_mode_shapes > 1 ||
        !isfinite(*shift) || !isfinite(*cut_frequency)) {
        *info = 21;
        return;
    }

    if (*solver_handle != 0) {
        *info = 20;
        return;
    }

    assembly_error = or_eig_layout_init(
        &layout, *communicator, *global_size, *owned_size, *row_start);
    if (assembly_error) {
        *info = 13;
        goto cleanup;
    }
    ierr = SlepcInitialized(&was_initialized);
    if (ierr != PETSC_SUCCESS) goto petsc_failure;
    if (!was_initialized) {
        ierr = SlepcInitializeNoArguments();
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        initialized_here = PETSC_TRUE;
    }
    ierr = or_eig_initialize_diagnostics(layout.comm);
    if (ierr != PETSC_SUCCESS) goto petsc_failure;
    ierr = or_eig_timing_begin(layout.comm,
                               "complete SLEPc eigensolver C path",
                               &total_started);
    if (ierr != PETSC_SUCCESS) goto petsc_failure;

    ierr = or_eig_timing_begin(layout.comm,
                               "distributed stiffness CSR construction",
                               &phase_started);
    if (ierr != PETSC_SUCCESS) goto petsc_failure;
    assembly_error = or_eig_dist_matrix_build(
        &distributed_stiffness, &layout, *local_size, global_dof, k_row,
        k_col, k_diag, k_lower);
    if (assembly_error) {
        *info = 14;
        goto cleanup;
    }
    ierr = or_eig_timing_end(layout.comm,
                             "distributed stiffness CSR construction",
                             phase_started);
    if (ierr != PETSC_SUCCESS) goto petsc_failure;
    ierr = or_eig_create_matrix(&distributed_stiffness, PETSC_FALSE,
                                &stiffness);
    if (ierr != PETSC_SUCCESS) goto petsc_failure;
    or_eig_dist_matrix_destroy(&distributed_stiffness);

    ierr = or_eig_timing_begin(layout.comm,
                               "distributed mass CSR construction",
                               &phase_started);
    if (ierr != PETSC_SUCCESS) goto petsc_failure;
    assembly_error = or_eig_dist_matrix_build(
        &distributed_mass, &layout, *local_size, global_dof, m_row, m_col,
        m_diag, m_lower);
    if (assembly_error) {
        *info = 15;
        goto cleanup;
    }
    ierr = or_eig_timing_end(layout.comm,
                             "distributed mass CSR construction",
                             phase_started);
    if (ierr != PETSC_SUCCESS) goto petsc_failure;
    assembly_error =
        or_eig_dist_matrix_validate_positive_diagonal(&distributed_mass);
    if (assembly_error) {
        *info = 16;
        goto cleanup;
    }
    ierr = or_eig_create_matrix(&distributed_mass, PETSC_TRUE, &mass);
    if (ierr != PETSC_SUCCESS) goto petsc_failure;
    or_eig_dist_matrix_destroy(&distributed_mass);

    if (*range_all == 0) {
        const PetscReal requested_target = (PetscReal)*shift;
        const PetscReal accepted_lower =
            PetscMax((PetscReal)0.0,
                     requested_target -
                         or_eig_acceptance_margin(requested_target,
                                                  &controls));
        PetscReal stiffness_norm = 0.0;
        PetscReal mass_norm = 0.0;
        PetscReal spectral_scale = 0.0;
        PetscReal solve_guard =
            or_eig_boundary_margin(requested_target, &controls);
        PetscReal solve_target;
        PetscInt converged = 0;
        EPSConvergedReason reason;

        ierr = MatNorm(stiffness, NORM_INFINITY, &stiffness_norm);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        ierr = MatNorm(mass, NORM_INFINITY, &mass_norm);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        if (PetscIsInfOrNanReal(stiffness_norm)) {
            *info = 14;
            goto cleanup;
        }
        if (PetscIsInfOrNanReal(mass_norm) || mass_norm <= 0.0) {
            *info = 16;
            goto cleanup;
        }
        spectral_scale = stiffness_norm / mass_norm;
        if (PetscIsInfOrNanReal(spectral_scale)) {
            *info = 14;
            goto cleanup;
        }
        /* A zero target needs a scale-aware negative factorization shift.
           For a positive target, retain the narrow inclusive-boundary guard
           so modes well below Freqmin are not needlessly requested. */
        if (requested_target <= 0.0)
            solve_guard = PetscMax(
                solve_guard,
                PetscMax((PetscReal)128.0 * PETSC_MACHINE_EPSILON,
                         controls.positive_pivot_tolerance) *
                    PetscMax(spectral_scale, (PetscReal)1.0));
        solve_target = requested_target - solve_guard;

        ierr = or_eig_logf(
            layout.comm,
            "Public /EIG selecting %d positive eigenvalues at or above "
            "%.16g\n", *nreq, *shift);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        ierr = or_eig_create_target_solver(
            stiffness, mass, solve_required, solve_target, solve_target,
            PETSC_MAX_REAL, &layout, &controls, &eps);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        ierr = or_eig_timing_begin(
            layout.comm, "fixed-target MUMPS setup and inertia count",
            &phase_started);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        ierr = EPSSetUp(eps);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        ierr = or_eig_count_above_target(eps, &available);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        ierr = or_eig_timing_end(
            layout.comm, "fixed-target MUMPS setup and inertia count",
            phase_started);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        if (available < 1) {
            *info = 11;
            goto cleanup;
        }
        solve_required = PetscMin(available, solve_required);
        if (requested_target <= 0.0 && solve_required < available)
            solve_required += PetscMin((PetscInt)6,
                                       available - solve_required);
        if (solve_required != (PetscInt)*nreq) {
            ierr = EPSSetDimensions(
                eps, solve_required,
                or_eig_subspace_size(&layout, solve_required, &controls),
                PETSC_DETERMINE);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
        }
        for (;;) {
            PetscInt next_required;

            ierr = or_eig_report_solver_configuration(eps);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
            ierr = or_eig_timing_begin(
                layout.comm, "SLEPc fixed-target eigensolve",
                &phase_started);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
            ierr = EPSSolve(eps);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
            ierr = or_eig_timing_end(
                layout.comm, "SLEPc fixed-target eigensolve",
                phase_started);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
            ierr = EPSGetConvergedReason(eps, &reason);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
            ierr = EPSGetConverged(eps, &converged);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
            if (reason < 0 || converged < solve_required) {
                *info = 12;
                goto cleanup;
            }
            ierr = or_eig_retain_target_results(
                eps, stiffness, &layout, accepted_lower,
                *cut_frequency > 0.0
                    ? PetscSqr(two_pi * (PetscReal)*cut_frequency) +
                          or_eig_acceptance_margin(
                              PetscSqr(two_pi *
                                       (PetscReal)*cut_frequency),
                              &controls)
                    : PETSC_MAX_REAL,
                (PetscInt)*nreq,
                requested_target <= 0.0 ? PETSC_TRUE : PETSC_FALSE,
                spectral_scale, controls.rigid_tolerance,
                *retain_mode_shapes ? PETSC_TRUE : PETSC_FALSE,
                &component_positive_modes, &accepted);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
            if (accepted >= (PetscInt)*nreq ||
                solve_required >= available)
                break;

            ierr = or_eig_destroy_component_modes(
                &component_positive_modes, accepted);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
            next_required = solve_required +
                            PetscMin(solve_required,
                                     available - solve_required);
            next_required = PetscMax(next_required,
                                     solve_required + 1);
            next_required = PetscMin(next_required, available);
            ierr = or_eig_logf(
                layout.comm,
                "Public /EIG fixed-target expansion: %" PetscInt_FMT
                " retained positives from %" PetscInt_FMT
                " solved Ritz values; expanding request to %" PetscInt_FMT
                "\n",
                accepted, solve_required, next_required);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
            solve_required = next_required;
            ierr = EPSSetDimensions(
                eps, solve_required,
                or_eig_subspace_size(&layout, solve_required, &controls),
                PETSC_DETERMINE);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
        }
        if (accepted < 1) {
            *info = 11;
            goto cleanup;
        }
        if (accepted < (PetscInt)*nreq) {
            ierr = or_eig_logf(
                layout.comm,
                "WARNING: /EIG requested %d modes, but only %" PetscInt_FMT
                " eligible positive modes are available; returning all "
                "available modes\n",
                *nreq, accepted);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
        }
        component_positive_count = accepted;
        goto retain_results;
    }

    {
        const PetscReal requested_lower = (PetscReal)*shift;
        const PetscReal requested_upper =
            PetscSqr(two_pi * (PetscReal)*cut_frequency);

        if (*range_all != 3 &&
            (*cut_frequency <= 0.0 ||
             !isfinite((double)requested_upper) ||
             requested_upper <= requested_lower)) {
            *info = 11;
            goto cleanup;
        }
        ierr = or_eig_solve_component_intervals(
            stiffness, mass, &layout, *local_size, global_dof, global_node,
            global_component, global_coordinate, requested_lower,
            requested_upper,
            *range_all == 2 ? (PetscInt)*nreq : 0,
            *range_all != 3 ? PETSC_TRUE : PETSC_FALSE,
            *output_rigid_modes ? PETSC_TRUE : PETSC_FALSE,
            *retain_mode_shapes ? PETSC_TRUE : PETSC_FALSE,
            *range_all == 3 ? PETSC_TRUE : PETSC_FALSE,
            &controls, &component_positive_modes,
            &component_positive_count,
            &available, &rigid_modes, &rigid_count);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        if (*range_all == 3) {
            if (rigid_count < 1) {
                *info = 11;
                goto cleanup;
            }
            ierr = or_eig_logf(
                layout.comm,
                "Public /EIG rigid-only request retains %" PetscInt_FMT
                " rigid component modes from the near-null spectrum\n",
                rigid_count);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
        } else {
            ierr = or_eig_logf(
                layout.comm,
                "Public /EIG interval contains %" PetscInt_FMT
                " positive modes and %" PetscInt_FMT " exact rigid modes; "
                "output retains %" PetscInt_FMT " modes (Irigid=%d)\n",
                available, rigid_count,
                component_positive_count +
                    (*output_rigid_modes ? rigid_count : 0),
                *output_rigid_modes);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
            if (*range_all == 2 && available < (PetscInt)*nreq) {
                ierr = or_eig_logf(
                    layout.comm,
                    "WARNING: /EIG requested %d modes, but the bounded "
                    "interval contains %" PetscInt_FMT
                    "; returning all available modes\n", *nreq, available);
                if (ierr != PETSC_SUCCESS) goto petsc_failure;
            }
        }
    }

retain_results:
    {
        const PetscInt required =
            component_positive_count +
            (*output_rigid_modes ? rigid_count : 0);

        if (required < 1) {
            *info = 11;
            goto cleanup;
        }
        int context_error;
        PetscErrorCode allocation_ierr = PETSC_SUCCESS;

        /* The retained result needs the communicator layout and component
           vectors only. Release the global PETSc solve objects before
           transferring ownership, and do not hide a failed destroy behind
           the successful retained-result status. The plain-C assembly CSR
           buffers were already released immediately after Mat assembly. */
        ierr = EPSDestroy(&eps);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        ierr = MatDestroy(&mass);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;
        ierr = MatDestroy(&stiffness);
        if (ierr != PETSC_SUCCESS) goto petsc_failure;

        if (!*output_rigid_modes) {
            ierr = or_eig_destroy_component_modes(&rigid_modes,
                                                   rigid_count);
            if (ierr != PETSC_SUCCESS) goto petsc_failure;
            rigid_count = 0;
        }
        saved = (or_eig_result_context *)malloc(sizeof(*saved));
        context_error = saved == NULL;
        if (saved != NULL) {
            memset(saved, 0, sizeof(*saved));
            allocation_ierr = PetscMalloc1(
                PetscMax(*local_size, 1), &saved->input_global_dof);
            if (allocation_ierr != PETSC_SUCCESS) context_error = 1;
        }
#if defined(PUBLIC_EIG_MPI)
        {
            int global_context_error = 0;
            if (MPI_Allreduce(&context_error, &global_context_error, 1,
                              MPI_INT, MPI_MAX,
                              layout.comm) != MPI_SUCCESS) {
                if (saved != NULL)
                    (void)PetscFree(saved->input_global_dof);
                free(saved);
                saved = NULL;
                ierr = PETSC_ERR_MPI;
                goto petsc_failure;
            }
            context_error = global_context_error;
        }
#endif
        if (context_error) {
            if (saved != NULL)
                (void)PetscFree(saved->input_global_dof);
            free(saved);
            saved = NULL;
            *info = 10;
            goto cleanup;
        }
        saved->layout = layout;
        saved->initialized_here = initialized_here;
        saved->mode_shapes_retained =
            *retain_mode_shapes ? PETSC_TRUE : PETSC_FALSE;
        saved->input_local_size = *local_size;
        if (*local_size > 0)
            memcpy(saved->input_global_dof, global_dof,
                   (size_t)*local_size * sizeof(*global_dof));
        saved->rigid_count = rigid_count;
        saved->rigid_modes = rigid_modes;
        saved->component_positive_count = component_positive_count;
        saved->component_positive_modes = component_positive_modes;
        saved->timing_started = total_started;
        ierr = or_eig_log_retained_storage(saved);
        if (ierr != PETSC_SUCCESS) {
            (void)PetscFree(saved->input_global_dof);
            free(saved);
            saved = NULL;
            goto petsc_failure;
        }
        rigid_modes = NULL;
        component_positive_modes = NULL;
        *solver_handle = (int64_t)(intptr_t)saved;
        *nfound = (int)required;
        *info = 19;
        return;
    }

petsc_failure:
    *info = 2000 + (int)ierr;

cleanup:
    *solver_handle = 0;
    free(saved);
    or_eig_destroy_component_modes(&component_positive_modes,
                                   component_positive_count);
    or_eig_destroy_component_modes(&rigid_modes, rigid_count);
    EPSDestroy(&eps);
    MatDestroy(&mass);
    MatDestroy(&stiffness);
    if (initialized_here) SlepcFinalize();
    or_eig_dist_matrix_destroy(&distributed_mass);
    or_eig_dist_matrix_destroy(&distributed_stiffness);
    or_eig_layout_destroy(&layout);
}
#else
typedef int or_eig_slepc_disabled_translation_unit;
#endif
