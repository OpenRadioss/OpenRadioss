# `-preview` Command Line Option: MPI/Computation Overlap for Broad Phase Collision Detection

## Overview

When the `-preview` flag is passed on the command line to the engine, it activates an optimized broad phase collision detection path that overlaps candidate pair search with MPI communication. The goal is to hide the latency of MPI exchanges by performing the local candidate pair search while waiting for remote secondary node data to arrive.

## Activation

The flag is parsed in [`execargcheck.F`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/engine/execargcheck.F) and stored in the module-level integer variable `GOT_PREVIEW` (defined in [`COMMAND_LINE_ARGS_MOD`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/share/modules/command_line_args.F)):

- `GOT_PREVIEW = 0` — default behavior (legacy sorting algorithm)
- `GOT_PREVIEW = 1` — overlap mode enabled

## Behavior

### Without `-preview` (legacy path)

1. The segment list (NRTM) is split across OpenMP threads (`ESHIFT` partitioning).
2. MPI exchange of secondary node data ([`SPMD_CELL_EXCHANGE`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/mpi/generic/spmd_cell_exchange.F)) completes fully before the sorting begins.
3. The broad phase uses the [`I7BUCE_VOX`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/i7buce.F) / `I7BUCE` bucket sort routine, which allocates and fills the voxel in a single pass after all data is available.

### With `-preview` (overlap path)

The key idea is to **overlap the local candidate pair search with the MPI exchange of remote secondary node data**. Since the local voxel is complete before the exchange starts, the local search can proceed immediately while remote data is in transit. Remote nodes are inserted into a **separate remote voxel** as messages arrive, and the remote candidate pair search runs after all data is received.

1. **Pre-sort phase** ([`inter_prepare_sort.F`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/generic/inter_prepare_sort.F)):
   - Voxel dimensions are computed as soon as the bounding box information is known ([`compute_voxel_dimensions`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/voxel_dimensions.F90)).
   - [`FILL_VOXEL_LOCAL`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/fill_voxel.F90) is called to insert **all** local secondary nodes into the local voxel in a single pass. This completes before the MPI exchange begins.

2. **MPI exchange + local search phase** ([`spmd_cell_exchange.F`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/mpi/generic/spmd_cell_exchange.F)):
   - Non-blocking sends (`MPI_ISEND`) and receives (`MPI_IRECV`) of secondary node data (coordinates, integer data) are posted.
   - [`INTER7_CANDIDATE_PAIRS_LOCAL`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/inter7_candidate_pairs.F90) is called immediately after posting the sends/receives. This performs the **full local candidate pair search** using the pre-built local voxel, while MPI transfers are in flight. This is the main source of overlap — the local search (which is the bulk of the computation) runs concurrently with network communication.
   - After the local search completes, a `MPI_WAITANY` loop processes receives as they complete: each time a message arrives, [`FILL_VOXEL_REMOTE_SEPARATE`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/fill_voxel.F90) immediately inserts the received remote secondary nodes into a **separate remote voxel** structure — overlapping communication of subsequent messages with voxel insertion of already-received data.

3. **Remote collision detection** ([`inter_sort_07.F`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/int07/inter_sort_07.F)):
   - The full segment list is passed to all threads (`ESHIFT = 0`, `NRTM_T = NRTM`) rather than being statically pre-partitioned by the caller. This is because the candidate pair routines use `!$OMP DO SCHEDULE(DYNAMIC)` internally to distribute the loop over segments across threads. Dynamic scheduling provides better load balancing since the work per segment varies (different number of neighboring voxels and candidate nodes).
   - [`INTER7_COLLISION_DETECTION`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/inter7_collision_detection.F90) is called to perform the **remote candidate pair search** using the separate remote voxel via [`INTER7_CANDIDATE_PAIRS_REMOTE`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/inter7_candidate_pairs.F90).

## Data Flow

1. **`inter_prepare_sort`** — voxel structure preparation
   - `compute_voxel_dimensions`
   - `FILL_VOXEL_LOCAL` (all local nodes, single pass)

2. **`spmd_cell_exchange`** — MPI exchange + local candidate search
   - `MPI_ISEND` / `MPI_IRECV` (secondary node coordinates + integer data) — posted, non-blocking
   - `INTER7_CANDIDATE_PAIRS_LOCAL` (`!$OMP DO SCHEDULE(DYNAMIC)` over segments, local voxel) — **overlaps with MPI transfers**
   - Loop: `MPI_WAITANY` (receives)
     - `FILL_VOXEL_REMOTE_SEPARATE` (insert received remote nodes into separate remote voxel) — overlaps with remaining receives

3. **`inter_sort_07`** — remote candidate pair enumeration
   - `INTER7_COLLISION_DETECTION`
     - `INTER7_CANDIDATE_PAIRS_REMOTE` (`!$OMP DO SCHEDULE(DYNAMIC)` over segments, remote voxel)

## Timeline

```
[FILL_VOXEL_LOCAL]
                   [MPI_IRECV + MPI_ISEND (non-blocking) ══════════════════]
                   [INTER7_CANDIDATE_PAIRS_LOCAL ═════════]
                                                           [WAITANY → FILL_VOXEL_REMOTE ═══]
                                                                                             [CANDIDATE_PAIRS_REMOTE]
```

## Key Subroutines

| Subroutine | File | Role |
|---|---|---|
| `FILL_VOXEL_LOCAL` | [`fill_voxel.F90`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/fill_voxel.F90) | Inserts all local secondary nodes into the local voxel (full single pass) |
| `FILL_VOXEL_REMOTE_SEPARATE` | [`fill_voxel.F90`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/fill_voxel.F90) | Inserts a batch of received remote secondary nodes into a separate remote voxel (1-based indexing) |
| `INTER7_CANDIDATE_PAIRS_LOCAL` | [`inter7_candidate_pairs.F90`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/inter7_candidate_pairs.F90) | Enumerates node–segment candidate pairs from the local voxel (called during MPI exchange) |
| `INTER7_CANDIDATE_PAIRS_REMOTE` | [`inter7_candidate_pairs.F90`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/inter7_candidate_pairs.F90) | Enumerates node–segment candidate pairs from the remote voxel |
| `INTER7_COLLISION_DETECTION` | [`inter7_collision_detection.F90`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/interfaces/intsort/inter7_collision_detection.F90) | Calls the remote candidate pair search after MPI exchange is complete |
| `SPMD_CELL_EXCHANGE` | [`spmd_cell_exchange.F`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/source/mpi/generic/spmd_cell_exchange.F) | MPI exchange of secondary node data + local candidate pair search (overlapped) |

## Data Structures

The `inter_struct_type` (defined in [`inter_struct_mod.F`](https://github.com/OpenRadioss/OpenRadioss/blob/main/engine/share/modules/inter_struct_mod.F)) holds both local and remote voxel data:

- **Local voxel**: `voxel`, `next_nod`, `last_nod`, `nb_voxel_on`, `list_nb_voxel_on`
- **Remote voxel** (separate): `voxel_remote`, `next_nod_remote`, `last_nod_remote`, `nb_voxel_on_remote`, `list_nb_voxel_on_remote`, `size_node_remote`

Remote nodes are indexed 1..NSNR in the remote voxel arrays. The search subroutine maps `jj -> nsn+jj` for downstream compatibility with the candidate pair arrays.

### Timing fields (for performance analysis)

| Field | Description |
|---|---|
| `t_fill_local` | Cumulative time in `FILL_VOXEL_LOCAL` |
| `t_mpi_wait` | Cumulative time waiting in `MPI_WAITANY` |
| `t_fill_remote` | Cumulative time in `FILL_VOXEL_REMOTE_SEPARATE` |
| `t_candidate_pairs` | Cumulative time in `INTER7_CANDIDATE_PAIRS_LOCAL` + `INTER7_CANDIDATE_PAIRS_REMOTE` |
| `n_calls` | Number of sort calls (for averaging) |

## Performance Rationale

In SPMD (distributed memory) runs, each process needs remote secondary node positions to perform collision detection on its local main segments. The traditional approach serializes the steps:

```
[MPI exchange] ──► [voxel fill] ──► [candidate search]
```

With `-preview`, the local candidate pair search runs while MPI transfers are in flight:

```
[FILL_VOXEL_LOCAL]
                   [MPI Isend/Irecv ═══════════════════════════]
                   [CANDIDATE_PAIRS_LOCAL ═══════]
                                                  [WAITANY → FILL_REMOTE]
                                                                         [CANDIDATE_PAIRS_REMOTE]
```

This reduces the wall-clock time of the broad phase sorting step by hiding the local candidate pair search (the most expensive computation) behind MPI communication latency. The separation of local and remote voxels is essential: the local voxel is complete and searchable immediately, while the remote voxel is built incrementally as messages arrive.
