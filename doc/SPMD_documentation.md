# SPMD / MPI Parallelism in OpenRadioss

This document covers the MPI layer: how the mesh is decomposed, how boundary nodes
are represented, how nodal forces are exchanged across domains each cycle, and how
the two parallelism modes (Parith/OFF and Parith/ON) differ. It also maps the
`engine/source/mpi/` directory structure.

---

## 1. Overview

OpenRadioss uses a **hybrid MPI + OpenMP** model:

- **MPI** decomposes the mesh spatially into `NSPMD` domains. Each domain is an
  independent OS process. MPI rank 0-based index of this domain is `ISPMD`.
- **OpenMP** parallelizes element loops inside each domain (up to `NTHREAD` threads).

The Starter writes one restart file (`_NNNN.rst`) per MPI domain. The Engine reads
these files and runs independently, exchanging only boundary ("frontier") node data
through MPI calls once per time cycle.

All MPI calls in OpenRadioss go through the wrapper module `SPMD_MOD`
(`engine/source/mpi/spmd_mod.F90`). Never call `MPI_*` directly; always use
`spmd_send`, `spmd_recv`, `spmd_isend`, etc. The `#ifndef MPI` guard provides
no-op stubs for single-process builds.

---

## 2. Key global variables

| Variable | Location | Meaning |
|----------|----------|---------|
| `NSPMD` | `/COM01/` common block (`com01_c.inc` line 28) | Number of MPI domains (= `np` from Starter) |
| `ISPMD` | `/SPMD/` common block (`task_c.inc` line 53) | 0-based rank of this process |
| `IT_SPMD(PARASIZ)` | `/SPMD/` common block (`task_c.inc` line 55) | Array mapping local process index → MPI rank |
| `IPARIT` | `/PARIT/` common block (`parit_c.inc` line 27) | 0 = Parith/OFF (direct exchange), 1 = Parith/ON (skyline) |
| `NTHREAD` | `/TASK/` common block (`task_c.inc` line 26) | Number of OpenMP threads |

`PARASIZ = 8192` (maximum number of MPI processes), `NTHMAX = 512` (maximum threads).

---

## 3. Domain decomposition (Starter → Engine handoff)

The Starter's `DDSPLIT` routine (called from `SGRTAILS`/`CGRTAILS` after grouping)
partitions the mesh so that:

1. Each domain owns a contiguous set of internal node IDs.
2. Elements that straddle domain boundaries are assigned to one owner; their
   non-owner nodes become **frontier nodes** in that domain.
3. Frontier nodes appear in the domain's node array (`NODES%X`, `NODES%V`, etc.)
   with full state — they are computed locally and then reconciled via MPI exchange.

The Starter writes per-domain restart files (`model_0001.rst`, `model_0002.rst`, …).
These contain the local node array, element connectivity, and the **boundary exchange
lists** (`NODES%BOUNDARY`, `NODES%BOUNDARY_ADD`).

---

## 4. Boundary node data structures

Frontier nodes (shared between domains) are tracked in the `NODES` structure
(type `nodal_arrays_`, defined in `common_source/modules/nodal_arrays.F90` lines 162–168):

| Field | Type | Meaning |
|-------|------|---------|
| `NODES%BOUNDARY_SIZE` | integer | Total number of frontier node entries |
| `NODES%BOUNDARY(BOUNDARY_SIZE)` | integer | List of internal node indices that are frontier nodes (ordered by domain) |
| `NODES%BOUNDARY_ADD(2, NSPMD+1)` | integer | CSR-style address array: the block of frontier nodes shared with domain `I` is `BOUNDARY(BOUNDARY_ADD(1,I) : BOUNDARY_ADD(1,I+1)-1)`, itself split into a **recv** sub-block `BOUNDARY_ADD(1,I)..BOUNDARY_ADD(2,I)-1` and a **send** sub-block `BOUNDARY_ADD(2,I)..BOUNDARY_ADD(1,I+1)-1` |
| `NODES%WEIGHT(NUMNOD)` | integer | Ownership flag: `1` = node owned by this domain, `0` = ghost copy; used to avoid double-counting shared nodes in global sums and output |
| `NODES%global_boundary_nb` | integer | Number of globally unique frontier nodes |
| `NODES%global_boundary(:)` | integer | Global node IDs of frontier nodes |

The analog in the force exchange routines is:
- `IAD_ELEM(2, NSPMD+1)` — CSR address array (same role as `BOUNDARY_ADD`)
- `FR_ELEM(*)` — frontier element/node list (same role as `BOUNDARY`)

These are passed as dummy arguments to `SPMD_EXCH_A` because that routine predates
the `NODES` structure; functionally they carry the same information.

---

## 5. The force exchange cycle

After all element forces have been assembled into `NODES%A(3, NUMNOD)` on each
domain, `RESOL` calls one of two force-exchange routines depending on `IPARIT`:

```
IF (IPARIT == 0) THEN
  CALL SPMD_EXCH_A(A, ADP, AR, STIFN, STIFR, MS, IAD_ELEM, FR_ELEM, …)
ELSE
  CALL SPMD_EXCH2_A_PON(INTERFACES, IAD_ELEM, FR_ELEM, ADDCNE, PROCNE, …, FSKY, FSKYV, FSKYM, …)
END IF
```

### 5.1 Parith/OFF (`IPARIT = 0`) — `SPMD_EXCH_A`

File: `engine/source/mpi/forces/spmd_exch_a.F`

**Algorithm:**
1. Post non-blocking receives (`MPI_IRECV`) from all neighbor domains into `RBUF`.
2. Pack frontier node forces (`A`, `AR`, `STIFN`, `STIFR`, `MS`, thermal terms) from
   `FR_ELEM(IAD_ELEM(1,I)..IAD_ELEM(1,I+1)-1)` into `SBUF` for domain `I`.
3. Non-blocking sends (`MPI_ISEND`) of `SBUF` to each neighbor.
4. `MPI_WAITALL` — synchronize.
5. Unpack `RBUF`: **add** received forces into local `A`, `AR`; **add** stiffnesses
   into `STIFN`, `STIFR`; **add** masses into `MS`.

This is the simple, symmetric approach: every domain sends its local accumulated
contribution and adds the neighbor's contribution, so every domain ends up with
the complete sum for its frontier nodes. `NODES%WEIGHT` (the 0/1 ownership flag)
is used elsewhere — in global reductions and output gathering — so each shared
node is counted exactly once.

### 5.2 Parith/ON (`IPARIT = 1`) — `SPMD_EXCH2_A_PON`

File: `engine/source/mpi/forces/spmd_exch2_a_pon.F`

Parith/ON provides **bit-for-bit reproducible** results regardless of the number of
MPI domains. Instead of summing floating-point numbers in arbitrary order (which
gives round-off differences), each frontier node's force contribution is assigned
to a single **skyline** entry in a fixed global ordering.

**Skyline arrays:**
- `FSKY(8, LSKY)` — skyline forces (8 components: 3 translational + 3 rotational + 2 extras)
- `FSKYV(LSKY, 8)` — transposed layout variant used for vectorization
- `FSKYM(LSKY)` — skyline mass
- `LSKY` — number of skyline rows, one per element-node contribution (defined in
  the `/PARIT/` common block, `parit_c.inc`)

**Algorithm:**
1. Element kernels write each of their nodal contributions into a private skyline
   row (instead of accumulating directly into `NODES%A`), addressed by the
   per-element back-pointers (`IADC` for shells).
2. `SPMD_EXCH2_A_PON` packs the skyline rows owned locally and exchanges them via
   non-blocking MPI (same ISend/IRecv pattern as Parith/OFF); received rows are
   **written** (not summed) into slots reserved for the remote rank.
3. The `ASSPAR4` subroutine (called inside `!$OMP PARALLEL` in `RESOL`, line ~4877)
   sums each node's skyline rows into `NODES%A` in a deterministic order.

The skyline addressing tables (`ADSKY`, `PROCNE`, `IADC`) are stored in
`ELEMENT%PON%*` arrays (type `element_pon_`, `parith_on_mod.F90`) and come from
the restart file; the send/recv tables (`ISENDP`/`IRECVP`/`FR_NBCC`) are built by
`ASSADD2` (called once from `RESOL_INIT`) and rebuilt by `REBUILD_PON_TABLES`
after a node split (see `doc/NODE_SPLITING.md`).

---

## 6. `SPMD_MOD` — the MPI wrapper module

**File:** `engine/source/mpi/spmd_mod.F90`

`SPMD_MOD` is an umbrella module that re-exports all low-level MPI primitives.
All other OpenRadioss routines import from `SPMD_MOD`, never from the underlying
modules directly. The module also defines `SPMD_REAL8` (1 if double precision,
0 if single) and MPI sentinel constants.

| Exported symbol | Source module | Wraps |
|-----------------|---------------|-------|
| `spmd_send` | `spmd_send_mod` | `MPI_SEND` (blocking) |
| `spmd_recv` | `spmd_recv_mod` | `MPI_RECV` (blocking) |
| `spmd_isend` | `spmd_isend_mod` | `MPI_ISEND` (non-blocking) |
| `spmd_irecv` | `spmd_irecv_mod` | `MPI_IRECV` (non-blocking) |
| `spmd_wait` / `spmd_waitall` / `spmd_waitany` | `spmd_wait_mod` | `MPI_WAIT*` family |
| `spmd_allreduce` / `spmd_iallreduce` | `spmd_allreduce_mod`, `spmd_iallreduce_mod` | `MPI_ALLREDUCE` |
| `spmd_allgather` / `spmd_allgatherv` | `spmd_allgather_mod`, `spmd_allgatherv_mod` | `MPI_ALLGATHER*` |
| `spmd_alltoall` | `spmd_alltoall_mod` | `MPI_ALLTOALL` |
| `spmd_pack` / `spmd_unpack` | `spmd_pack_mod`, `spmd_unpack_mod` | `MPI_PACK` / `MPI_UNPACK` |
| `spmd_comm_rank` / `spmd_comm_size` | inline in `spmd_mod.F90` | `MPI_COMM_RANK` / `MPI_COMM_SIZE` |
| `spmd_max` / `spmd_min` / `spmd_sum` / `spmd_prod` | `spmd_allreduce_mod` | Typed allreduce wrappers |
| `SPMD_COMM_WORLD` | `spmd_comm_world_mod` | Global MPI communicator handle |

---

## 7. `engine/source/mpi/` directory map

| Sub-directory / file | Contents |
|----------------------|----------|
| `forces/` | Nodal force exchange: `SPMD_EXCH_A` (Parith/OFF), `SPMD_EXCH2_A_PON` (Parith/ON), `SPMD_EXCH_A_INT2*` (contact force variants), `SPMD_EXCH_A_AMS_POFF` (AMS variant) |
| `nodes/` | Coordinate/velocity exchange: `SPMD_SD_XV` (position+velocity), `SPMD_EXCH_N` (node state), `SPMD_EXCH_THKNOD` (shell thickness), `SPMD_EXCH_WAVE` (wave propagation), `SPMD_EXCH_ICODT` / `SPMD_EXCH_ICONT` (time-step / contact flags) |
| `interfaces/` | Contact MPI exchange: `SPMD_INT` (candidate search), `SPMD_GET_PENIS*` (penetration), `SPMD_GET_STIF*` (contact stiffness), `SPMD_EXCH_NEIGHBOUR_SEGMENT`, `send_cand.F` |
| `airbags/` | FVM airbag: `SPMD_FVB_SWITCH`, `SPMD_MV_CA`, `SPMD_EXCH_FVSTATS` |
| `ale/` | ALE advection: `SPMD_EXCH_MIN_MAX`, `SPMD_EXCH_N_NEIGHBOR_2D/3D`, cell exchange helpers |
| `ams/` | AMS (Advanced Mass Scaling): `SPMD_EXCH_NODNX`, `SPMD_EXCH_SMS`, `SPMD_EXCH_SMS6`, `SPMD_FI_SMS` |
| `anim/` | Animation gather: `SPMD_AGET_SECT` (cross-section results), `SPMD_AGETMSR` (sensors), `SPMD_ANIM_PLY_*` (composite ply animation) |
| `elements/` | Element-level exchange: `SPMD_SPH` (SPH neighbor data: density, velocity, forces, gradients), `SPMD_EXCH_A_SCND*` (secondary force exchange) |
| `fluid/` | CFD/multifluid: `SPMD_CFD` (numerous routines for ALE void neighbor exchange) |
| `generic/` | Generic wrappers: `spmd_allgather.F90`, `spmd_alltoall.F90`, `spmd_alltoallv.F90` |
| `implicit/` | Implicit solver exchange routines |
| `init/` | Initialization: `inipar.F` (MPI setup), `init_global_boundary_list.F90` (builds `BOUNDARY`/`BOUNDARY_ADD`), `spmd_kill.F`, `spmd_mstop.F` |
| `kinematic_conditions/` | Rigid body force exchange: `SPMD_EXCH_A_RB6*`, `SPMD_EXCH_A_RM6` |
| `lag_multipliers/` | Lagrange multiplier exchange (implicit/constraint solver) |
| `output/` | Output gather operations |
| `r2r/` | Rank-to-rank specialized exchange |
| `seatbelts/` | Seatbelt MPI exchange: `SPMD_EXCH_A_SEATBELT` |
| `sections/` | Cross-section force gather |
| `sph/` | Additional SPH communication |
| `user_interface/` | Python element MPI sync |
| `spmd_mod.F90` | Umbrella wrapper module (see §6) |
| `python_spmd_mod.F90` | Python element state synchronization across domains |
| `spmd_exch_sub.F` | Non-local damage exchange: `SPMD_SUB_BOUNDARIES` (table construction), `SPMD_EXCH_SUB_POFF` (Parith/OFF sum), `SPMD_EXCH_SUB_PON` (Parith/ON skyline slots) |
| `get_mpi_operator.F90` | `get_mpi_operator_mod` — maps Fortran reduction type to `MPI_Op` |

---

## 8. Time-step global minimum

The per-domain time step is `DT2 = min(element DT over all local elements)`.
A global minimum across all domains is then needed. This is done by encoding the
nodal stiffness `STIFN`/`STIFR` (which sets the critical time step) into the
force exchange message and accumulating via `SPMD_GATHER_DTNODA`:

```fortran
! After SPMD_EXCH_A:
CALL SPMD_GATHER_DTNODA(DT2, DT2T, NSPMD, …)
```

Alternatively, `spmd_allreduce` with `MPI_MIN` is used directly in the AMS path.

---

## 9. Parith flag and `/PARITH/ON|OFF` input

The user controls reproducibility via:
- `/PARITH/ON` — sets `IPARIT=1`; forces use the skyline path; bit-for-bit
  reproducible across all domain counts.
- `/PARITH/OFF` (default) — sets `IPARIT=0`; direct boundary-sum path; faster but
  results may differ by round-off when changing `np`.

The flag is read from the engine input deck (`radioss2.F` → `LECINP`) and stored in
`/PARIT/` common block (`parit_c.inc`).

---

## 10. Key files

| File | Role |
|------|------|
| `engine/source/mpi/spmd_mod.F90` | Umbrella MPI wrapper module |
| `engine/source/mpi/spmd_comm_world.F90` | `SPMD_COMM_WORLD` communicator definition |
| `engine/source/mpi/forces/spmd_exch_a.F` | Parith/OFF nodal force exchange |
| `engine/source/mpi/forces/spmd_exch2_a_pon.F` | Parith/ON skyline force exchange |
| `engine/source/mpi/init/init_global_boundary_list.F90` | Builds `BOUNDARY`/`BOUNDARY_ADD` from restart data |
| `engine/share/includes/task_c.inc` | `/TASK/` and `/SPMD/` commons: `NTHREAD`, `ISPMD`, `IT_SPMD` |
| `engine/share/includes/com01_c.inc` | `/COM01/` common: `NSPMD`, `NGROUP`, `NCYCLE`, … |
| `engine/share/includes/parit_c.inc` | `/PARIT/` common: `IPARIT`, `LSKY`, `NISKY`, … |
| `common_source/modules/nodal_arrays.F90` | `BOUNDARY`, `BOUNDARY_ADD`, `WEIGHT` fields in `nodal_arrays_` |
| `common_source/modules/parith_on_mod.F90` | `element_pon_` and `interface_pon_` types for skyline arrays |

---

## 11. See also

- `doc/ENGINE_TIME_LOOP_documentation.md` — where `SPMD_EXCH_A` / `SPMD_EXCH2_A_PON`
  and `ASSPAR4` are called in the cycle loop
- `doc/GROUPS_AND_CONNECTIVITY_documentation.md` — `NODES%WEIGHT`, `NODES%ITAB`,
  domain boundary nodes
- `doc/NLOCAL_STR_documentation.md` — non-local damage uses `SPMD_EXCH_SUB_PON`
  for cross-domain regularization
- `doc/NODE_SPLITING.md` — how the Parith/ON skyline and the boundary/exchange
  tables are extended and rebuilt when nodes are split at runtime
