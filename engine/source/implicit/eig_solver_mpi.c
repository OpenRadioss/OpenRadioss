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
 * Distributed matrix assembly and vector exchange for public /EIG solvers.
 */

#include "eig_solver_mpi.h"

#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#if defined(PUBLIC_EIG_MPI)
typedef struct {
    int64_t key;
    int position;
} or_eig_key_position;
#endif

typedef struct {
    int64_t row;
    int64_t column;
    double value;
} or_eig_triplet;

#if defined(PUBLIC_EIG_MPI)
/* A rank-local validation or allocation failure must become collective before
 * any rank advances to the next MPI call.  Otherwise one failed rank can
 * return while its peers block forever in a collective. */
static int or_eig_sync_error(MPI_Comm comm, int local_error) {
    int global_error = local_error;
    if (MPI_Allreduce(&local_error, &global_error, 1, MPI_INT, MPI_MAX,
                      comm) != MPI_SUCCESS)
        return 3;
    /* MPI_MAX must retain every nonzero local error.  Keep that invariant
     * explicit so a failed allocation can never be followed by a null
     * dereference even if an MPI implementation violates the contract. */
    if (local_error != 0 && global_error == 0) return local_error;
    return global_error;
}
#define OR_EIG_SYNC_ERROR(comm, error) or_eig_sync_error((comm), (error))
#else
#define OR_EIG_SYNC_ERROR(comm, error) (error)
#endif

#if defined(__GNUC__) || defined(__clang__)
#define OR_EIG_ALLOCATOR __attribute__((malloc, alloc_size(1, 2)))
#else
#define OR_EIG_ALLOCATOR
#endif

static void *or_eig_alloc(size_t count, size_t size) OR_EIG_ALLOCATOR;
static void *or_eig_calloc(size_t count, size_t size) OR_EIG_ALLOCATOR;

static void *or_eig_alloc(size_t count, size_t size) {
    if (count == 0) count = 1;
    if (size != 0 && count > SIZE_MAX / size) return NULL;
    return malloc(count * size);
}

static void *or_eig_calloc(size_t count, size_t size) {
    if (count == 0) count = 1;
    if (size != 0 && count > SIZE_MAX / size) return NULL;
    return calloc(count, size);
}

#undef OR_EIG_ALLOCATOR

#if defined(PUBLIC_EIG_MPI)
static int or_eig_compare_key_position(const void *left, const void *right) {
    const or_eig_key_position *a = (const or_eig_key_position *)left;
    const or_eig_key_position *b = (const or_eig_key_position *)right;
    if (a->key < b->key) return -1;
    if (a->key > b->key) return 1;
    return a->position < b->position ? -1 : (a->position > b->position);
}
#endif

static int or_eig_compare_triplet(const void *left, const void *right) {
    const or_eig_triplet *a = (const or_eig_triplet *)left;
    const or_eig_triplet *b = (const or_eig_triplet *)right;
    if (a->row < b->row) return -1;
    if (a->row > b->row) return 1;
    if (a->column < b->column) return -1;
    if (a->column > b->column) return 1;
    return 0;
}

#if defined(PUBLIC_EIG_MPI)
static int or_eig_owner_from_key(int64_t key, int size) {
    uint64_t mixed = (uint64_t)key + UINT64_C(0x9e3779b97f4a7c15);
    mixed = (mixed ^ (mixed >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
    mixed = (mixed ^ (mixed >> 27)) * UINT64_C(0x94d049bb133111eb);
    mixed ^= mixed >> 31;
    return (int)(mixed % (uint64_t)size);
}
#endif

static int or_eig_owner_from_gid(const or_eig_layout *layout, int64_t gid) {
    int low = 0;
    int high = layout->size;
    if (gid < 0 || gid >= layout->global_size) return -1;
    while (low + 1 < high) {
        const int middle = low + (high - low) / 2;
        if (gid < layout->offsets[middle]) high = middle;
        else low = middle;
    }
    return low;
}

void eig_build_global_dof_map_c_(int *communicator, int *local_size,
                                  int *global_node, int *component,
                                  int64_t *global_dof, int *global_size,
                                  int *owned_size, int64_t *row_start,
                                  int *info) {
    int i;

    *info = 0;
    *global_size = 0;
    *owned_size = 0;
    *row_start = 0;
#if defined(PUBLIC_EIG_MPI)
    {
        MPI_Comm comm = MPI_Comm_f2c((MPI_Fint)*communicator);
        int rank = 0;
        int size = 1;
        int *send_count = NULL;
        int *send_offset = NULL;
        int *receive_count = NULL;
        int *receive_offset = NULL;
        int *cursor = NULL;
        int *origin = NULL;
        int *local_owner = NULL;
        int *source_rank = NULL;
        int64_t *send_key = NULL;
        int64_t *receive_key = NULL;
        int64_t *reply = NULL;
        int64_t *returned = NULL;
        or_eig_key_position *sorted = NULL;
        int receive_total = 0;
        int64_t owned64 = 0;
        int64_t global64 = 0;
        int mpi_error = MPI_SUCCESS;

        if (MPI_Comm_rank(comm, &rank) != MPI_SUCCESS ||
            MPI_Comm_size(comm, &size) != MPI_SUCCESS || size < 1) {
            *info = 3;
            return;
        }
        *info = OR_EIG_SYNC_ERROR(comm, *local_size < 0 ? 1 : 0);
        if (*info != 0) return;
        send_count = (int *)or_eig_calloc((size_t)size, sizeof(*send_count));
        send_offset = (int *)or_eig_calloc((size_t)size, sizeof(*send_offset));
        receive_count =
            (int *)or_eig_calloc((size_t)size, sizeof(*receive_count));
        receive_offset =
            (int *)or_eig_calloc((size_t)size, sizeof(*receive_offset));
        cursor = (int *)or_eig_calloc((size_t)size, sizeof(*cursor));
        origin = (int *)or_eig_alloc((size_t)*local_size, sizeof(*origin));
        local_owner =
            (int *)or_eig_alloc((size_t)*local_size, sizeof(*local_owner));
        send_key =
            (int64_t *)or_eig_alloc((size_t)*local_size, sizeof(*send_key));
        if (!send_count || !send_offset || !receive_count ||
            !receive_offset || !cursor || !origin || !local_owner ||
            !send_key) *info = 2;
        *info = OR_EIG_SYNC_ERROR(comm, *info);
        if (*info != 0) goto map_cleanup;
        for (i = 0; i < *local_size; ++i) {
            int64_t key;
            int owner;
            if (global_node[i] <= 0 || component[i] <= 0 ||
                component[i] > 255) {
                *info = 1;
                break;
            }
            key = ((int64_t)global_node[i] << 8) |
                  (int64_t)(component[i] - 1);
            owner = or_eig_owner_from_key(key, size);
            if (send_count[owner] == INT_MAX) {
                *info = 4;
                break;
            }
            ++send_count[owner];
        }
        *info = OR_EIG_SYNC_ERROR(comm, *info);
        if (*info != 0) goto map_cleanup;
        for (i = 1; i < size; ++i) {
            if (send_offset[i - 1] > INT_MAX - send_count[i - 1]) {
                *info = 4;
                break;
            }
            send_offset[i] = send_offset[i - 1] + send_count[i - 1];
        }
        *info = OR_EIG_SYNC_ERROR(comm, *info);
        if (*info != 0) goto map_cleanup;
        memcpy(cursor, send_offset, (size_t)size * sizeof(*cursor));
        for (i = 0; i < *local_size; ++i) {
            const int64_t key = ((int64_t)global_node[i] << 8) |
                                (int64_t)(component[i] - 1);
            const int owner = or_eig_owner_from_key(key, size);
            const int position = cursor[owner]++;
            send_key[position] = key;
            origin[position] = i;
        }
        mpi_error = MPI_Alltoall(send_count, 1, MPI_INT, receive_count, 1,
                                 MPI_INT, comm);
        if (mpi_error != MPI_SUCCESS) {
            *info = 3;
            goto map_cleanup;
        }
        for (i = 1; i < size; ++i) {
            if (receive_offset[i - 1] > INT_MAX - receive_count[i - 1]) {
                *info = 4;
                break;
            }
            receive_offset[i] = receive_offset[i - 1] + receive_count[i - 1];
        }
        if (*info == 0 &&
            receive_offset[size - 1] > INT_MAX - receive_count[size - 1])
            *info = 4;
        *info = OR_EIG_SYNC_ERROR(comm, *info);
        if (*info != 0) goto map_cleanup;
        receive_total = receive_offset[size - 1] + receive_count[size - 1];
        receive_key = (int64_t *)or_eig_alloc((size_t)receive_total,
                                               sizeof(*receive_key));
        reply =
            (int64_t *)or_eig_alloc((size_t)receive_total, sizeof(*reply));
        returned =
            (int64_t *)or_eig_alloc((size_t)*local_size, sizeof(*returned));
        sorted = (or_eig_key_position *)or_eig_alloc((size_t)receive_total,
                                                      sizeof(*sorted));
        source_rank =
            (int *)or_eig_alloc((size_t)receive_total, sizeof(*source_rank));
        if (!receive_key || !reply || !returned || !sorted || !source_rank)
            *info = 2;
        *info = OR_EIG_SYNC_ERROR(comm, *info);
        if (*info != 0) goto map_cleanup;
        mpi_error = MPI_Alltoallv(send_key, send_count, send_offset,
                                  MPI_INT64_T, receive_key, receive_count,
                                  receive_offset, MPI_INT64_T, comm);
        if (mpi_error != MPI_SUCCESS) {
            *info = 3;
            goto map_cleanup;
        }
        for (i = 0; i < size; ++i) {
            int j;
            for (j = receive_offset[i];
                 j < receive_offset[i] + receive_count[i]; ++j)
                source_rank[j] = i;
        }
        for (i = 0; i < receive_total; ++i) {
            sorted[i].key = receive_key[i];
            sorted[i].position = i;
        }
        qsort(sorted, (size_t)receive_total, sizeof(*sorted),
              or_eig_compare_key_position);
        /* The hash rank is only a sparse directory.  Assign each DOF to the
         * lowest rank that actually contains it, retaining the Engine domain
         * decomposition and minimizing solver-vector halo traffic. */
        for (i = 0; i < receive_total;) {
            int j = i + 1;
            int owner = source_rank[sorted[i].position];
            while (j < receive_total && sorted[j].key == sorted[i].key) {
                const int candidate = source_rank[sorted[j].position];
                if (candidate < owner) owner = candidate;
                ++j;
            }
            while (i < j) reply[sorted[i++].position] = (int64_t)owner;
        }
        mpi_error = MPI_Alltoallv(reply, receive_count, receive_offset,
                                  MPI_INT64_T, returned, send_count,
                                  send_offset, MPI_INT64_T, comm);
        if (mpi_error != MPI_SUCCESS) {
            *info = 3;
            goto map_cleanup;
        }
        for (i = 0; i < *local_size; ++i)
            local_owner[origin[i]] = (int)returned[i];

        free(source_rank);
        free(sorted);
        free(reply);
        free(receive_key);
        source_rank = NULL;
        sorted = NULL;
        reply = NULL;
        receive_key = NULL;
        memset(send_count, 0, (size_t)size * sizeof(*send_count));
        memset(send_offset, 0, (size_t)size * sizeof(*send_offset));
        memset(receive_count, 0, (size_t)size * sizeof(*receive_count));
        memset(receive_offset, 0, (size_t)size * sizeof(*receive_offset));
        for (i = 0; i < *local_size; ++i) ++send_count[local_owner[i]];
        for (i = 1; i < size; ++i)
            send_offset[i] = send_offset[i - 1] + send_count[i - 1];
        memcpy(cursor, send_offset, (size_t)size * sizeof(*cursor));
        for (i = 0; i < *local_size; ++i) {
            const int position = cursor[local_owner[i]]++;
            send_key[position] = ((int64_t)global_node[i] << 8) |
                                 (int64_t)(component[i] - 1);
            origin[position] = i;
        }
        mpi_error = MPI_Alltoall(send_count, 1, MPI_INT, receive_count, 1,
                                 MPI_INT, comm);
        if (mpi_error != MPI_SUCCESS) {
            *info = 3;
            goto map_cleanup;
        }
        for (i = 1; i < size; ++i) {
            if (receive_offset[i - 1] > INT_MAX - receive_count[i - 1]) {
                *info = 4;
                break;
            }
            receive_offset[i] = receive_offset[i - 1] + receive_count[i - 1];
        }
        if (*info == 0 &&
            receive_offset[size - 1] > INT_MAX - receive_count[size - 1])
            *info = 4;
        *info = OR_EIG_SYNC_ERROR(comm, *info);
        if (*info != 0) goto map_cleanup;
        receive_total = receive_offset[size - 1] + receive_count[size - 1];
        receive_key = (int64_t *)or_eig_alloc((size_t)receive_total,
                                               sizeof(*receive_key));
        reply =
            (int64_t *)or_eig_alloc((size_t)receive_total, sizeof(*reply));
        sorted = (or_eig_key_position *)or_eig_alloc((size_t)receive_total,
                                                      sizeof(*sorted));
        if (!receive_key || !reply || !sorted) *info = 2;
        *info = OR_EIG_SYNC_ERROR(comm, *info);
        if (*info != 0) goto map_cleanup;
        mpi_error = MPI_Alltoallv(send_key, send_count, send_offset,
                                  MPI_INT64_T, receive_key, receive_count,
                                  receive_offset, MPI_INT64_T, comm);
        if (mpi_error != MPI_SUCCESS) {
            *info = 3;
            goto map_cleanup;
        }
        for (i = 0; i < receive_total; ++i) {
            sorted[i].key = receive_key[i];
            sorted[i].position = i;
        }
        qsort(sorted, (size_t)receive_total, sizeof(*sorted),
              or_eig_compare_key_position);
        for (i = 0; i < receive_total; ++i) {
            if (i == 0 || sorted[i].key != sorted[i - 1].key) ++*owned_size;
        }
        owned64 = (int64_t)*owned_size;
        mpi_error = MPI_Exscan(&owned64, row_start, 1, MPI_INT64_T, MPI_SUM,
                               comm);
        if (mpi_error != MPI_SUCCESS) {
            *info = 3;
            goto map_cleanup;
        }
        if (rank == 0) *row_start = 0;
        mpi_error = MPI_Allreduce(&owned64, &global64, 1, MPI_INT64_T,
                                  MPI_SUM, comm);
        if (mpi_error != MPI_SUCCESS || global64 > INT_MAX) {
            *info = mpi_error == MPI_SUCCESS ? 4 : 3;
            goto map_cleanup;
        }
        *global_size = (int)global64;
        {
            int64_t gid = *row_start - 1;
            int64_t previous = INT64_MIN;
            for (i = 0; i < receive_total; ++i) {
                if (i == 0 || sorted[i].key != previous) ++gid;
                previous = sorted[i].key;
                reply[sorted[i].position] = gid;
            }
        }
        mpi_error = MPI_Alltoallv(reply, receive_count, receive_offset,
                                  MPI_INT64_T, returned, send_count,
                                  send_offset, MPI_INT64_T, comm);
        if (mpi_error != MPI_SUCCESS) {
            *info = 3;
            goto map_cleanup;
        }
        for (i = 0; i < *local_size; ++i) global_dof[origin[i]] = returned[i];

map_cleanup:
        free(source_rank);
        free(sorted);
        free(returned);
        free(reply);
        free(receive_key);
        free(send_key);
        free(local_owner);
        free(origin);
        free(cursor);
        free(receive_offset);
        free(receive_count);
        free(send_offset);
        free(send_count);
    }
#else
    (void)communicator;
    (void)global_node;
    (void)component;
    if (*local_size < 0) {
        *info = 1;
        return;
    }
    for (i = 0; i < *local_size; ++i) global_dof[i] = (int64_t)i;
    *global_size = *local_size;
    *owned_size = *local_size;
#endif
}

int or_eig_layout_init(or_eig_layout *layout, int communicator,
                       int global_size, int owned_size, int64_t row_start) {
    int i;
    int error;
    memset(layout, 0, sizeof(*layout));
    error = global_size < 1 || owned_size < 0 || owned_size > global_size ||
                    row_start < 0 ||
                    row_start > (int64_t)global_size - owned_size
                ? 1
                : 0;
    layout->global_size = (int64_t)global_size;
    layout->local_size = owned_size;
    layout->row_start = row_start;
#if defined(PUBLIC_EIG_MPI)
    layout->comm = MPI_Comm_f2c((MPI_Fint)communicator);
    if (MPI_Comm_rank(layout->comm, &layout->rank) != MPI_SUCCESS ||
        MPI_Comm_size(layout->comm, &layout->size) != MPI_SUCCESS) return 3;
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) return error;
#else
    (void)communicator;
    layout->rank = 0;
    layout->size = 1;
    if (error != 0) return error;
#endif
    layout->offsets =
        (int64_t *)or_eig_alloc((size_t)layout->size + 1u,
                                sizeof(*layout->offsets));
    if (!layout->offsets) error = 2;
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) {
        or_eig_layout_destroy(layout);
        return error;
    }
#if defined(PUBLIC_EIG_MPI)
    {
        int64_t local = (int64_t)owned_size;
        if (MPI_Allgather(&local, 1, MPI_INT64_T, layout->offsets, 1,
                          MPI_INT64_T, layout->comm) != MPI_SUCCESS) {
            or_eig_layout_destroy(layout);
            return 3;
        }
    }
#else
    layout->offsets[0] = (int64_t)owned_size;
#endif
    {
        int64_t offset = 0;
        for (i = 0; i < layout->size; ++i) {
            const int64_t count = layout->offsets[i];
            layout->offsets[i] = offset;
            offset += count;
        }
        layout->offsets[layout->size] = offset;
        if (offset != layout->global_size ||
            layout->offsets[layout->rank] != row_start)
            error = 1;
        error = OR_EIG_SYNC_ERROR(layout->comm, error);
        if (error != 0) {
            or_eig_layout_destroy(layout);
            return error;
        }
    }
    return 0;
}

void or_eig_layout_destroy(or_eig_layout *layout) {
    free(layout->offsets);
    memset(layout, 0, sizeof(*layout));
}

int or_eig_dist_matrix_build(or_eig_dist_matrix *matrix,
                             const or_eig_layout *layout, int local_size,
                             const int64_t *global_dof, const int *row,
                             const int *column, const double *diagonal,
                             const double *lower) {
    int lower_count;
    size_t contribution_count;
    int *send_count = NULL;
    int *send_offset = NULL;
    int *receive_count = NULL;
    int *receive_offset = NULL;
    int *cursor = NULL;
    or_eig_triplet *send = NULL;
    or_eig_triplet *receive = NULL;
    int receive_total;
    int compacted_count = 0;
    int i;
    int error = 0;

    memset(matrix, 0, sizeof(*matrix));
    matrix->layout = layout;
    lower_count = 0;
    contribution_count = 0;
    if (local_size < 0 || (local_size > 0 && row[0] != 1)) {
        error = 1;
    } else {
        for (i = 0; i < local_size; ++i) {
            if (row[i] < 1 || row[i + 1] < row[i] ||
                !isfinite(diagonal[i])) {
                error = 1;
                break;
            }
        }
        if (error == 0) {
            lower_count = local_size > 0 ? row[local_size] - 1 : 0;
            if ((size_t)lower_count > SIZE_MAX / 2u ||
                (size_t)local_size >
                    SIZE_MAX - 2u * (size_t)lower_count)
                error = 1;
            else {
                contribution_count =
                    (size_t)local_size + 2u * (size_t)lower_count;
                if (contribution_count > INT_MAX) error = 4;
            }
            for (i = 0; error == 0 && i < lower_count; ++i) {
                if (!isfinite(lower[i])) error = 1;
            }
        }
    }
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) goto cleanup;

    send_count = (int *)or_eig_calloc((size_t)layout->size, sizeof(int));
    send_offset = (int *)or_eig_calloc((size_t)layout->size, sizeof(int));
    receive_count =
        (int *)or_eig_calloc((size_t)layout->size, sizeof(int));
    receive_offset =
        (int *)or_eig_calloc((size_t)layout->size, sizeof(int));
    cursor = (int *)or_eig_calloc((size_t)layout->size, sizeof(int));
    send = (or_eig_triplet *)or_eig_alloc(contribution_count, sizeof(*send));
    if (!send_count || !send_offset || !receive_count || !receive_offset ||
        !cursor || !send) error = 2;
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) goto cleanup;
    for (i = 0; i < local_size; ++i) {
        int entry;
        const int row_owner = or_eig_owner_from_gid(layout, global_dof[i]);
        if (row_owner < 0) {
            error = 1;
            break;
        }
        ++send_count[row_owner];
        for (entry = row[i] - 1; entry < row[i + 1] - 1; ++entry) {
            const int local_column = column[entry] - 1;
            int column_owner;
            if (local_column < 0 || local_column >= local_size) {
                error = 1;
                break;
            }
            column_owner = or_eig_owner_from_gid(
                layout, global_dof[local_column]);
            if (column_owner < 0) {
                error = 1;
                break;
            }
            ++send_count[row_owner];
            ++send_count[column_owner];
        }
        if (error != 0) break;
    }
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) goto cleanup;
    for (i = 1; i < layout->size; ++i)
        send_offset[i] = send_offset[i - 1] + send_count[i - 1];
    memcpy(cursor, send_offset, (size_t)layout->size * sizeof(*cursor));
    for (i = 0; i < local_size; ++i) {
        int entry;
        int owner = or_eig_owner_from_gid(layout, global_dof[i]);
        int position = cursor[owner]++;
        send[position].row = global_dof[i];
        send[position].column = global_dof[i];
        send[position].value = diagonal[i];
        for (entry = row[i] - 1; entry < row[i + 1] - 1; ++entry) {
            const int local_column = column[entry] - 1;
            const int64_t other = global_dof[local_column];
            owner = or_eig_owner_from_gid(layout, global_dof[i]);
            position = cursor[owner]++;
            send[position].row = global_dof[i];
            send[position].column = other;
            send[position].value = lower[entry];
            owner = or_eig_owner_from_gid(layout, other);
            position = cursor[owner]++;
            send[position].row = other;
            send[position].column = global_dof[i];
            send[position].value = lower[entry];
        }
    }
#if defined(PUBLIC_EIG_MPI)
    if (MPI_Alltoall(send_count, 1, MPI_INT, receive_count, 1, MPI_INT,
                     layout->comm) != MPI_SUCCESS) {
        error = 3;
        goto cleanup;
    }
#else
    receive_count[0] = send_count[0];
#endif
    for (i = 1; i < layout->size; ++i) {
        if (receive_offset[i - 1] > INT_MAX - receive_count[i - 1]) {
            error = 4;
            break;
        }
        receive_offset[i] = receive_offset[i - 1] + receive_count[i - 1];
    }
    if (error == 0 && receive_offset[layout->size - 1] >
                          INT_MAX - receive_count[layout->size - 1])
        error = 4;
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) goto cleanup;
    receive_total = receive_offset[layout->size - 1] +
                    receive_count[layout->size - 1];
    receive =
        (or_eig_triplet *)or_eig_alloc((size_t)receive_total, sizeof(*receive));
    if (!receive) error = 2;
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) goto cleanup;
#if defined(PUBLIC_EIG_MPI)
    {
        int *send_byte_count =
            (int *)or_eig_alloc((size_t)layout->size, sizeof(int));
        int *send_byte_offset =
            (int *)or_eig_alloc((size_t)layout->size, sizeof(int));
        int *receive_byte_count =
            (int *)or_eig_alloc((size_t)layout->size, sizeof(int));
        int *receive_byte_offset =
            (int *)or_eig_alloc((size_t)layout->size, sizeof(int));
        if (!send_byte_count || !send_byte_offset || !receive_byte_count ||
            !receive_byte_offset) error = 2;
        if (!error) {
            for (i = 0; i < layout->size; ++i) {
                if (send_count[i] > INT_MAX / (int)sizeof(*send) ||
                    receive_count[i] > INT_MAX / (int)sizeof(*receive) ||
                    send_offset[i] > INT_MAX / (int)sizeof(*send) ||
                    receive_offset[i] > INT_MAX / (int)sizeof(*receive)) {
                    error = 4;
                    break;
                }
                send_byte_count[i] = send_count[i] * (int)sizeof(*send);
                send_byte_offset[i] = send_offset[i] * (int)sizeof(*send);
                receive_byte_count[i] =
                    receive_count[i] * (int)sizeof(*receive);
                receive_byte_offset[i] =
                    receive_offset[i] * (int)sizeof(*receive);
            }
        }
        error = OR_EIG_SYNC_ERROR(layout->comm, error);
        if (!error && MPI_Alltoallv(send, send_byte_count, send_byte_offset,
                                    MPI_BYTE, receive, receive_byte_count,
                                    receive_byte_offset, MPI_BYTE,
                                    layout->comm) != MPI_SUCCESS) error = 3;
        free(receive_byte_offset);
        free(receive_byte_count);
        free(send_byte_offset);
        free(send_byte_count);
        if (error) goto cleanup;
    }
#else
    if (receive_total > 0)
        memcpy(receive, send, (size_t)receive_total * sizeof(*receive));
#endif
    /* The exchange metadata and outbound triplets are no longer needed.
     * Release them before sorting and allocating the final CSR arrays so the
     * three large representations never coexist at peak memory. */
    free(send);
    send = NULL;
    free(cursor);
    cursor = NULL;
    free(receive_offset);
    receive_offset = NULL;
    free(receive_count);
    receive_count = NULL;
    free(send_offset);
    send_offset = NULL;
    free(send_count);
    send_count = NULL;
    qsort(receive, (size_t)receive_total, sizeof(*receive),
          or_eig_compare_triplet);
    for (i = 0; i < receive_total; ++i) {
        if (compacted_count > 0 &&
            receive[compacted_count - 1].row == receive[i].row &&
            receive[compacted_count - 1].column == receive[i].column) {
            receive[compacted_count - 1].value += receive[i].value;
            if (!isfinite(receive[compacted_count - 1].value)) {
                error = 1;
                goto cleanup;
            }
        } else {
            if (compacted_count != i) receive[compacted_count] = receive[i];
            ++compacted_count;
        }
    }
    matrix->row =
        (int *)or_eig_calloc((size_t)layout->local_size + 1u, sizeof(int));
    matrix->column =
        (int64_t *)or_eig_alloc((size_t)compacted_count, sizeof(int64_t));
    matrix->value =
        (double *)or_eig_alloc((size_t)compacted_count, sizeof(double));
    if (!matrix->row || !matrix->column || !matrix->value) {
        error = 2;
        goto cleanup;
    }
    for (i = 0; i < compacted_count; ++i) {
        const int64_t local_row = receive[i].row - layout->row_start;
        if (local_row < 0 || local_row >= layout->local_size) {
            error = 1;
            goto cleanup;
        }
        ++matrix->row[(int)local_row + 1];
    }
    for (i = 1; i <= layout->local_size; ++i)
        matrix->row[i] += matrix->row[i - 1];
    matrix->nnz = compacted_count;
    for (i = 0; i < compacted_count; ++i) {
        matrix->column[i] = receive[i].column;
        matrix->value[i] = receive[i].value;
    }
cleanup:
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    free(receive);
    free(send);
    free(cursor);
    free(receive_offset);
    free(receive_count);
    free(send_offset);
    free(send_count);
    if (error) or_eig_dist_matrix_destroy(matrix);
    return error;
}

void or_eig_dist_matrix_destroy(or_eig_dist_matrix *matrix) {
    free(matrix->value);
    free(matrix->column);
    free(matrix->row);
    memset(matrix, 0, sizeof(*matrix));
}

int or_eig_dist_matrix_validate_positive_diagonal(
    const or_eig_dist_matrix *matrix) {
    int local_bad = 0;
    int global_bad = 0;
    int row;
    for (row = 0; row < matrix->layout->local_size; ++row) {
        const int64_t gid = matrix->layout->row_start + row;
        int entry;
        double diagonal = 0.0;
        for (entry = matrix->row[row]; entry < matrix->row[row + 1]; ++entry) {
            if (matrix->column[entry] == gid) diagonal += matrix->value[entry];
        }
        if (!isfinite(diagonal) || !(diagonal > 0.0)) ++local_bad;
    }
#if defined(PUBLIC_EIG_MPI)
    if (MPI_Allreduce(&local_bad, &global_bad, 1, MPI_INT, MPI_SUM,
                      matrix->layout->comm) != MPI_SUCCESS) return -3;
#else
    global_bad = local_bad;
#endif
    return global_bad;
}

int or_eig_expand_owned_vectors(const or_eig_layout *layout, int local_size,
                                const int64_t *global_dof, int vector_count,
                                const double *owned_vectors,
                                double *local_vectors) {
    int *request_count = NULL;
    int *request_offset = NULL;
    int *incoming_count = NULL;
    int *incoming_offset = NULL;
    int *cursor = NULL;
    int *origin = NULL;
    int64_t *request_gid = NULL;
    int64_t *incoming_gid = NULL;
    double *send_value = NULL;
    double *receive_value = NULL;
    int incoming_total;
    int p;
    int i;
    int vector;
    int error = 0;

    if (local_size < 0 || vector_count < 0) error = 1;
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) goto cleanup;

    request_count =
        (int *)or_eig_calloc((size_t)layout->size, sizeof(int));
    request_offset =
        (int *)or_eig_calloc((size_t)layout->size, sizeof(int));
    incoming_count =
        (int *)or_eig_calloc((size_t)layout->size, sizeof(int));
    incoming_offset =
        (int *)or_eig_calloc((size_t)layout->size, sizeof(int));
    cursor = (int *)or_eig_calloc((size_t)layout->size, sizeof(int));
    origin = (int *)or_eig_alloc((size_t)local_size, sizeof(int));
    request_gid =
        (int64_t *)or_eig_alloc((size_t)local_size, sizeof(int64_t));
    if (!request_count || !request_offset || !incoming_count ||
        !incoming_offset || !cursor || !origin || !request_gid) error = 2;
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) goto cleanup;
    for (i = 0; i < local_size; ++i) {
        const int owner = or_eig_owner_from_gid(layout, global_dof[i]);
        if (owner < 0) {
            error = 1;
            break;
        }
        ++request_count[owner];
    }
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) goto cleanup;
    for (p = 1; p < layout->size; ++p)
        request_offset[p] = request_offset[p - 1] + request_count[p - 1];
    memcpy(cursor, request_offset, (size_t)layout->size * sizeof(int));
    for (i = 0; i < local_size; ++i) {
        const int owner = or_eig_owner_from_gid(layout, global_dof[i]);
        const int position = cursor[owner]++;
        request_gid[position] = global_dof[i];
        origin[position] = i;
    }
#if defined(PUBLIC_EIG_MPI)
    if (MPI_Alltoall(request_count, 1, MPI_INT, incoming_count, 1, MPI_INT,
                     layout->comm) != MPI_SUCCESS) {
        error = 3;
        goto cleanup;
    }
#else
    incoming_count[0] = request_count[0];
#endif
    for (p = 1; p < layout->size; ++p) {
        if (incoming_offset[p - 1] > INT_MAX - incoming_count[p - 1]) {
            error = 4;
            break;
        }
        incoming_offset[p] = incoming_offset[p - 1] + incoming_count[p - 1];
    }
    if (error == 0 && incoming_offset[layout->size - 1] >
                          INT_MAX - incoming_count[layout->size - 1])
        error = 4;
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) goto cleanup;
    incoming_total = incoming_offset[layout->size - 1] +
                     incoming_count[layout->size - 1];
    incoming_gid =
        (int64_t *)or_eig_alloc((size_t)incoming_total, sizeof(int64_t));
    send_value =
        (double *)or_eig_alloc((size_t)incoming_total, sizeof(double));
    receive_value =
        (double *)or_eig_alloc((size_t)local_size, sizeof(double));
    if (!incoming_gid || !send_value || !receive_value) error = 2;
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) goto cleanup;
#if defined(PUBLIC_EIG_MPI)
    if (MPI_Alltoallv(request_gid, request_count, request_offset,
                      MPI_INT64_T, incoming_gid, incoming_count,
                      incoming_offset, MPI_INT64_T,
                      layout->comm) != MPI_SUCCESS) {
        error = 3;
        goto cleanup;
    }
#else
    if (local_size > 0)
        memcpy(incoming_gid, request_gid,
               (size_t)local_size * sizeof(int64_t));
#endif
    for (i = 0; i < incoming_total; ++i) {
        if (incoming_gid[i] < layout->row_start ||
            incoming_gid[i] >= layout->row_start + layout->local_size) {
            error = 1;
            break;
        }
    }
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    if (error != 0) goto cleanup;
    for (vector = 0; vector < vector_count; ++vector) {
        for (i = 0; i < incoming_total; ++i) {
            const int local = (int)(incoming_gid[i] - layout->row_start);
            send_value[i] = owned_vectors[(size_t)vector *
                                              (size_t)layout->local_size +
                                          (size_t)local];
        }
#if defined(PUBLIC_EIG_MPI)
        if (MPI_Alltoallv(send_value, incoming_count, incoming_offset,
                          MPI_DOUBLE, receive_value, request_count,
                          request_offset, MPI_DOUBLE,
                          layout->comm) != MPI_SUCCESS) {
            error = 3;
            goto cleanup;
        }
#else
        if (local_size > 0)
            memcpy(receive_value, send_value,
                   (size_t)local_size * sizeof(double));
#endif
        for (i = 0; i < local_size; ++i) {
            local_vectors[(size_t)vector * (size_t)local_size +
                          (size_t)origin[i]] =
                receive_value[i];
        }
    }

cleanup:
    error = OR_EIG_SYNC_ERROR(layout->comm, error);
    free(receive_value);
    free(send_value);
    free(incoming_gid);
    free(request_gid);
    free(origin);
    free(cursor);
    free(incoming_offset);
    free(incoming_count);
    free(request_offset);
    free(request_count);
    return error;
}
