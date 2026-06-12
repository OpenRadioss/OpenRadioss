# NODE_SPLITING — Node splitting for crack propagation (with PARITH/ON)

## 1. Purpose and scope

This document describes the **node-splitting** technique used to propagate cracks
in the OpenRadioss engine, and — most importantly — how it keeps the
**PARITH/ON** (bitwise-reproducible) force-assembly machinery consistent when new
nodes are created at runtime.

A crack advances by **duplicating (splitting) a node**: the shells on one side of
the crack keep the original node, the shells on the other side are reconnected to
a brand-new node `N'`. The two nodes occupy the same position at the split cycle
but are then free to separate.

The hard part is doing this **without breaking `/PARITH/ON`**. With
`IPARIT > 0`, OpenRadioss guarantees that nodal forces are summed in a
fixed, domain-decomposition-independent order so that results are **bitwise
identical regardless of the number of MPI ranks**. This is implemented with the
*skyline* arrays `FSKY` / `ADSKY` / `IADC` / `PROCNE`. When a node is split, room
must be made in those arrays **and** all the per-element back-pointers and the
MPI send/receive descriptors must stay mutually consistent.

> Related docs: `doc/NLOCAL_STR_documentation.md` (non-local damage structure that
> drives the crack criterion and has its **own** independent skyline), and
> `doc/SPMD_documentation.md` (Parith/ON domain exchange).

### Source layout

| File | Role |
|------|------|
| `engine/source/engine/node_spliting/nloc_shell_detach.F90` | Entry point: builds the list of (node, shells) to split from the damage field, exchanges ghost-shell damage, calls `apply_crack`. |
| `engine/source/engine/node_spliting/apply_crack.F90` | Ownership race, owner/mirror/placeholder dispatch, global UID de-duplication. Defines `node_split_info`. |
| `engine/source/engine/node_spliting/detach_node.F90` | `detach_node` (owner), `mirror_node_split` (ghost copy), `detach_node_from_shells`, `set_new_node_values`, interface/rwall detach. |
| `engine/source/engine/node_spliting/update_pon.F90` | `update_pon_shells`: makes room in the **mechanical** PARITH/ON skyline (`FSKY`,`ADSKY`,`IADC`,`PROCNE`). |
| `engine/source/engine/node_spliting/detach_node_nloc.F90` | Same as `update_pon` but for the **non-local damage** skyline inside `NLOC_DMG`. |
| `engine/source/engine/node_spliting/ghost_shells.F90` | `init_ghost_shells`, `spmd_exchange_ghost_shells`: build/refresh the ghost-shell mirror set. |
| `engine/source/engine/node_spliting/spmd_rebuild_boundary.F90` | `spmd_rebuild_boundary`, `merge_boundary_with_split`: rebuild MPI boundary node lists after a split. |
| `engine/source/engine/node_spliting/check_pon_consistency.F90` | Diagnostic audit of the `IADC` ⊂ `ADSKY` invariant (read-only). |
| `engine/source/assembly/asspar4.F` | Assembles `FSKY` → `NODES%A` using `ADSKY` (read side). |
| `engine/source/assembly/assadd2.F` | `ASSADD2` / `REBUILD_PON_TABLES`: build `ISENDP/IRECVP/IADSDP/IADRCP/FR_NBCC` from `ADSKY`+`PROCNE`. |
| `engine/source/elements/shell/coque/cupdtn3.F` | Writes element forces into `FSKY(:,IADC(j,e))` (write side). |
| `engine/source/mpi/spmd_exch_sub.F` | **NLOC** exchange: `SPMD_SUB_BOUNDARIES` builds the non-local skyline send/recv tables; `SPMD_EXCH_SUB_PON` performs the exchange (the NLOC analog of `REBUILD_PON_TABLES` + `SPMD_EXCH2_A_PON`). |
| `common_source/modules/parith_on_mod.F90` | `element_pon_` type (the mechanical skyline container). |
| `common_source/modules/nlocal_reg_mod.F` | `NLOCAL_STR_` type (`NLOC_DMG`): the **separate** non-local damage skyline + DOF arrays. |
| `common_source/modules/connectivity.F90` | `shell_`, `ghost_shell_`, `connectivity_` types. |
| `common_source/modules/nodal_arrays.F90` | `nodal_arrays_` type + `extend_nodal_arrays`. |

---

## 2. The PARITH/ON skyline (what must stay consistent)

The mechanical skyline lives in `element%pon` (type `element_pon_`,
`common_source/modules/parith_on_mod.F90`):

```
FSKY(1:8, 1:SFSKY/8)   ! per-contribution force rows: (Fx,Fy,Fz, Mx,My,Mz, STIFN, STIFR)
ADSKY(1:SADSKY)        ! CSR row offsets per node: node N owns rows ADSKY(N) .. ADSKY(N+1)-1
IADC(1:4, 1:NUMELC)    ! per-shell-corner index into FSKY (which row each corner writes)
PROCNE(1:SFSKY/8)      ! MPI owner (1-based rank) of each FSKY row (SPMD classification)
ISENDP/IRECVP          ! lists of FSKY rows to send / receive (built from ADSKY+PROCNE)
IADSDP/IADRCP          ! per-rank displacement into ISENDP/IRECVP
FR_NBCC(2,NSPMD)       ! (send_count, recv_count) per remote rank
```
(`SADSKY = NUMNOD + 1`, `SFSKY = 8 * number_of_FSKY_rows`.)

### 2.1 Write side — `cupdtn3.F`

Each cycle, after force computation, every 4-node shell writes its four corner
force/moment contributions into `FSKY`, one row per corner, addressed by `IADC`:

```fortran
K = IADC(1,I); FSKY(1,K)=-F11(I); FSKY(2,K)=-F21(I); ... ; FSKY(8,K)=STIR(I)*FAC(I,1)
K = IADC(2,I); ...
K = IADC(3,I); ...
K = IADC(4,I); ...
```

So shell `I`, corner `j`, writes into the single FSKY row `IADC(j,I)`.

### 2.2 Read side — `asspar4.F`

Assembly sums, for each node `N`, exactly the rows in its `ADSKY` band:

```fortran
DO N = 1,NUMNOD
  NCT = ADSKY(N)-1
  NC  = ADSKY(N+1)-ADSKY(N)
  DO K = NCT+1, NCT+NC              ! rows ADSKY(N) .. ADSKY(N+1)-1
    NODES%A(1,N)   = NODES%A(1,N)   + FSKY(1,K)
    ...
    NODES%STIFR(N) = NODES%STIFR(N) + FSKY(8,K)
  END DO
END DO
```

`NODES%A` is later divided by `NODES%MS` in `ACCELE` to give the acceleration.

### 2.3 The invariant

For every local shell corner `(j, e)` attached to a **local** node `N`:

```
ADSKY(N)  ≤  IADC(j,e)  ≤  ADSKY(N+1)-1        (corner writes into its node's band)
and every FSKY row is claimed by at most one (element, corner)   (no aliasing)
```

If this is violated, a corner writes a row that its node never reads (force
lost), or a row read by the wrong node (force misattributed). This is exactly
what `check_pon_consistency` audits, and the class of bug diagnosed and fixed
during the PARITH/ON bring-up of node splitting (post-mortem in §9).

### 2.4 SPMD reproducibility (why `PROCNE` exists)

For a node shared across ranks, the FSKY rows in its `ADSKY` band are split into:

* **local rows** — contributions computed on *this* rank, `PROCNE(row) = ispmd+1`;
* **recv rows** — placeholders reserved for a *remote* rank's contribution,
  `PROCNE(row) = remote_rank+1`.

`ASSADD2` / `REBUILD_PON_TABLES` (in `assadd2.F`) walk each boundary node's band
and classify each row `CC`:

```fortran
IF     (PROCNE(CC) == LOC_PROC) THEN     ! LOC_PROC = ispmd+1
  FR_NBCC(1,I) = FR_NBCC(1,I)+1          ! SEND row CC to remote rank I
  ISENDP(LSD)  = CC
ELSEIF (PROCNE(CC) == I) THEN            ! row reserved for remote rank I
  FR_NBCC(2,I) = FR_NBCC(2,I)+1          ! RECV into row CC from remote rank I
  IRECVP(LRC)  = CC
ENDIF
```

`SPMD_EXCH2_A_PON` (called just before `ASSPAR4`, `resol.F ~4816`) then copies
each rank's local rows into the matching recv rows on the partner rank. Because
every rank ends up with the **same set of rows in the same ADSKY order**, the
final `ASSPAR4` sum is bitwise identical to the single-rank sum — provided the
row ordering (owner's local shells first, sorted by shell user-id, then recv
rows) is reproduced. `apply_crack` enforces this ordering (see §5.3, §6.3).

### 2.5 The non-local damage skyline — a *second, independent* skyline

When non-local damage regularization is active (`NLOC_DMG%IMOD > 0`, the driver
of crack propagation) there is a **completely separate** Parith/ON skyline living
inside `NLOC_DMG` (type `NLOCAL_STR_`, `common_source/modules/nlocal_reg_mod.F`).
It is **not** shared with `element%pon`; it has its own arrays and its own MPI
exchange, and it must be kept consistent on a split in exactly the same way.

| Mechanical (`element%pon`) | Non-local (`NLOC_DMG`) | Meaning |
|----------------------------|------------------------|---------|
| `ADSKY(N)` | `ADDCNE(NN)` | CSR row offsets per node (`NN` = non-local node rank) |
| `IADC(j,e)` | `IADC/IADS/IADTG(j,e)` | per-element-corner FSKY row index |
| `FSKY(8,·)` | `FSKY(·,NDDMAX)`, `STSKY` | skyline force / stiffness accumulator (per DOF) |
| `PROCNE(row)` | `PROCNE(row)` | owning rank (1-based) of each row |
| `ISENDP/IRECVP` | `ISENDSP/IRECSP` | FSKY rows to send / receive |
| `IADSDP/IADRCP` | `IADSDP/IADRCP` | per-rank displacement into the send/recv lists |
| `FR_NBCC(2,NSPMD)` | `FR_NBCC(2,NSPMD)` | (send,recv) counts per remote rank |
| `NODES%BOUNDARY`/`BOUNDARY_ADD` | `NLOC_DMG%FR_ELEM`/`IAD_ELEM` | boundary node list per rank |

Extra node-index tables unique to `NLOC_DMG` (a node has `NDDL` DOFs, not 3+3):

| Array | Meaning |
|-------|---------|
| `INDX(NN)` | non-local node rank `NN` → local node id |
| `IDXI(local_id)` | local node id → non-local rank `NN` (0 if the node is not non-local) |
| `POSI(NN)` | CSR offset of node `NN`'s first DOF; `NDDL = POSI(NN+1)-POSI(NN)` |
| `VNL/UNL/DNL/MASS/FNL/STIFNL` | per-DOF state (velocity, cumulated variable, force, …) |

Assembly mirrors the mechanical path: element routines (`cfint_reg`, `c3fint_reg`,
…) write each shell's non-local force into `NLOC_DMG%FSKY(IADC(j,e),dof)`;
`ASSPAR_SUB` gathers `FSKY` → `FNL` via `ADDCNE`; and **`SPMD_EXCH_SUB_PON`**
(the NLOC analog of `SPMD_EXCH2_A_PON`) exchanges rows across ranks using
`ISENDSP/IRECSP/IADSDP/IADRCP/FR_NBCC`, which are built by **`SPMD_SUB_BOUNDARIES`**
(the NLOC analog of `REBUILD_PON_TABLES`). Because it is a wholly separate skyline,
a node split must patch it **independently** — see §6.7 (per-node growth,
`detach_node_nloc`) and §8 (post-split table rebuild). Getting the mechanical
skyline right does **not** fix the non-local one.

---

## 3. Where node splitting runs in the time loop

Node splitting is invoked once per cycle inside `RESOL`, **after** force assembly
and **before** acceleration (`engine/source/engine/resol.F`). `RESOL_INIT`
(`~1645`) runs **once**, before the per-cycle loop entry point (label `100` at
`~2650`, inside the enclosing `!$OMP PARALLEL`); the first `ASSADD2` there is what
zeroes `FSKY` and builds the initial send/recv tables. The repeating cycle is:

```
RESOL_INIT                       ! ~1645  ONCE: ASSADD2 zeroes FSKY, builds PON tables
100 CONTINUE                     ! ~2650  <-- per-cycle loop re-entry (GO TO 100 at ~9979)
FORINTC / FORINT                 ! ~4105 / ~4190  element forces -> FSKY via IADC   (cupdtn3.F)
--- FORCE ASSEMBLY ---           ! ~4589
  SPMD_EXCH2_A_PON               ! ~4816  MPI exchange of FSKY rows (recv rows filled)
  ASSPAR4                        ! ~4877  FSKY -> NODES%A via ADSKY
--- NODE SPLITTING ---           ! ~5310  detach nodes, grow FSKY/ADSKY/IADC/PROCNE,
                                 !        rebuild boundary + PON tables (this document)
ACCELE                           ! ~6915  NODES%A / NODES%MS -> acceleration
GO TO 100                        ! ~9979  next cycle
```

`FSKY` is **not** re-zeroed every cycle: each shell corner overwrites its own row
with `=` (not `+=`) in `cupdtn3.F`, and the skyline is sized so that every row is
owned by exactly one contribution. The one-time zeroing (initial `ASSADD2`, plus
the whole-array zeroing done by `update_pon_shells` at each split, §6.4) is what
keeps any *unwritten* row at zero.

Consequence: a split created at cycle `c` uses the **old** connectivity for the
assembly at cycle `c`; the new node `N'` first receives its own element forces at
cycle `c+1`. At the split cycle, `N'` inherits the parent's `V/X` and an
**area-weighted share `w`** of the parent's mass, inertia and assembled force
(`MS/MS0`, `IN/IN0`, `A/AR`) via `set_new_node_values` (§4.2); the parent keeps
the complement `1-w`. `w` is the fraction of the attached-shell area migrating
to `N'` (§5.1.5).

The orchestration block in `resol.F` (guarded by `NLOC_DMG%IMOD>0`) is:

```fortran
call nloc_shell_detach(... , new_crack, ...)     ! detect + apply all splits this cycle
if (new_crack > 0) then
  numnodm = numnodm_old + new_crack
#ifdef MPI
  if (NSPMD > 1) then
    ... snapshot current BOUNDARY / BOUNDARY_ADD as the rolling baseline ...
    call spmd_rebuild_boundary(...)              ! rebuild BOUNDARY / BOUNDARY_ADD
    call merge_boundary_with_split(...)          ! keep baseline sections + add new nodes
    call SPMD_SUB_BOUNDARIES(NLOC_DMG,...)       ! non-local MPI boundary tables
    call init_ghost_shells(...)                  ! refresh ghost-shell mirror set
    if (IPARIT /= 0) then                        ! rebuild Parith/ON send/recv tables
      call ASSINIT(...)                          ! size ISENDP/IRECVP
      ... reallocate ISENDP / IRECVP ...
      call REBUILD_PON_TABLES(ADSKY, BOUNDARY_ADD, BOUNDARY, PROCNE, FR_NBCC,
                              IADSDP, IADRCP, ISENDP, IRECVP, NUMNOD_OLD)
    end if
  end if
#endif
  call INIT_NODAL_STATE(...)                     ! reinit nodal state for new nodes
  ... CHKINIT (ADDCNEL/CNEL), ALLOCATE_OUTPUT_DATA, skyline reallocation, rwalls ...
end if
```

---

## 4. Data structures created / grown per split

### 4.1 Nodal arrays (`nodal_arrays_`)

`extend_nodal_arrays(nodes, numnod+1)` grows every per-node array by one slot
(it does **not** increment `nodes%numnod`; the caller does that last). The new
node always takes local id `numnod+1`.

Key per-node fields relevant to splitting:

| Field | Meaning after split |
|-------|---------------------|
| `itab(N')` | user id of `N'` (assigned in the global UID phase, §5.4). Temporarily `max_uid`. |
| `MS(N') , MS0(N')` | area-weighted share `w` of the parent's mass; the parent keeps `1-w` (§5.1.5). |
| `IN(N') , IN0(N')` | rotational inertia, split with the same fraction `w`. |
| `A(N') , AR(N')` | `w` × parent's assembled force/moment (parent keeps `1-w`), so the acceleration `A/MS` stays continuous across the split cycle. |
| `X,V,D,VR,...` | copied from parent. `STIFN/STIFR` reset to `EM20`. |
| `WEIGHT(N')` | `1` if this rank owns `N'`, `0` if it is a ghost/mirror copy. |
| `MAIN_PROC(N')` | owning rank + 1 (1-based). |
| `parent_node(N')` | root of the split chain; `nchilds(root)` incremented. |
| `nodglob(N')` | global node index (same on every rank that holds `N'`). |

### 4.2 `set_new_node_values` (detach_node.F90)

Copies kinematics/topology from the parent into the new slot and splits the
mass, inertia and assembled force with the area-weighted fraction `w`
(`mass_fraction` argument, computed in §5.1.5):

```fortran
nodes%X(:,numnod+1)  = nodes%X(:,i)       ! same position as parent
nodes%V(:,numnod+1)  = nodes%V(:,i)
nodes%MS(numnod+1)   = nodes%MS(i)*w   ; nodes%MS(i) = nodes%MS(i)*(1-w)
nodes%IN(numnod+1)   = nodes%IN(i)*w   ; nodes%IN(i) = nodes%IN(i)*(1-w)   ! iroddl
nodes%A(:,numnod+1)  = nodes%A(:,i)*w  ; nodes%A(:,i) = nodes%A(:,i)*(1-w) ! keeps A/MS continuous
nodes%STIFN(numnod+1)= EM20               ! reaccumulated from elements next cycle
nodes%WEIGHT(numnod+1)= 1                 ! overridden to 0 for mirror/placeholder
nodes%parent_node(numnod+1)= root(i) ; nodes%nchilds(root)+ = 1
```

### 4.3 Shell connectivity

For each shell in the (local) split list, every corner that pointed at the parent
`node_id` is repointed to `N'` in **both** connectivity representations:

```fortran
elements%shell%nodes(j, s) = new_local_id
elements%shell%ixc(j+1,  s) = new_local_id
```

### 4.4 Ghost shells (`ghost_shell_`, connectivity.F90)

`element%ghost_shell` is a **read-only mirror of neighbouring ranks' shells that
touch a boundary node of this rank**. It is rebuilt by `init_ghost_shells`:

| Field | Meaning |
|-------|---------|
| `nodes(1:4,g)` | corner nodes of ghost shell `g` (in remote local ids / mapped). |
| `uid(g)` | user id of ghost shell `g` (used for the ownership race). |
| `offset(p)..offset(p+1)-1` | ghost shells received from rank `p-1` (0-based rank `p-1`). |
| `shells_to_send(p)%index` | this rank's local shells to send to rank `p`. |
| `addcnel/cnel` | node→ghost-shell connectivity (used by non-local damage). |
| `glob2loc` | map global shell id → ghost local index. |

Ghost shells are **never** passed to `detach_node`; they are only used to decide
ownership (§5.1) and to build the non-local neighbourhood.

---

## 5. The split algorithm (single cycle)

### 5.0 Detection — `nloc_shell_detach`

Builds a `crack_info_list(:)` of `node_split_info` records, one per node to split:

```fortran
type node_split_info
  integer :: parent_uid, parent_id      ! node being split (user id / local id)
  integer :: node_uid, node_id          ! new node (filled during UID sync)
  integer :: weight                     ! 1 = this rank owns N', 0 = remote owner
  integer :: owning_rank                ! 0-based owner rank
  integer, allocatable :: shell_uids(:) ! signed local ids of shells going to N':
                                        !   > 0 local shell,  < 0 ghost-shell index
end type
```

The current code uses a hard-coded demonstration list (nodes `10682..10722` and a
fixed shell list) gated to fire once; this is the placeholder to be replaced by a
real criterion driven by the non-local damage field. Before building the list,
`spmd_exchange_ghost_shells` exchanges per-shell damage so every rank sees the
damage of ghost shells too.

### 5.1 Phase 1 — ownership race (local, no MPI)

For each split, the owning rank is the one holding the shell with the **globally
smallest user id** among all shells (local **and** ghost) attached to the new
node:

```
minuid over shell_uids:
  local shell  s>0 : candidate user_id = element%shell%user_id(s)
  ghost shell  s<0 : candidate user_id = element%ghost_shell%uid(-s)
if the min-uid shell is LOCAL  -> weight=1, owning_rank=ispmd
else (min-uid shell is a ghost)-> weight=0, owning_rank = sender rank of that ghost
                                  (found from ghost_shell%offset ranges)
```

Because every rank sharing the parent has ghost copies of the others' shells,
**all ranks independently compute the same owner** — no communication needed.
This deterministic choice is what makes the later ADSKY ordering reproducible.

### 5.1.5 Phase 1.6 — area-weighted mass split fraction

For each record, `split_mass_fraction` computes the share `w` of the parent's
lumped nodal mass that migrates to `N'`:

```
w = Σ (corners_at_parent · area) over MIGRATING shells (local + ghost)
    ─────────────────────────────────────────────────────────────────
    Σ (corners_at_parent · area) over ALL attached shells (local + ghost)
```

clamped to `[0.01, 0.99]` so neither side of the crack is left massless
(fallback `0.5` if no attached shell is found). This replaces the former fixed
50/50 split: a node whose migrating fan carries 1/3 of the attached area hands
over 1/3 of its mass, which matches the lumped-mass pattern of 4-node shells
(each shell contributes `ρ·t·A/4` per corner; assuming uniform `ρ·t` over the
fan, the `ρ·t/4` factor cancels).

`w` must be **bitwise identical** on every rank holding the parent (the parent
and child masses of shared nodes must stay identical across ranks) and equal to
the 1-rank value. Two ingredients guarantee this:

* each shell's area is computed **once, on its home rank**, from `nodes%X`
  (identical on all ranks under `/PARITH/ON`), and shipped to the other ranks'
  ghost copies through `spmd_exchange_ghost_shells` — the same channel used for
  the damage exchange;
* the numerator/denominator sums are accumulated in **ascending shell-user-id
  order** (insertion sort), so the floating-point summation order does not
  depend on each rank's local/ghost storage layout.

The fraction is applied on the creating ranks by `set_new_node_values`
(mechanical mass/inertia/force, §4.2) and `detach_node_nloc` (non-local mass,
§6.7), and on ranks that hold the parent without creating `N'` by
`scale_parent_on_noncreating_rank` in Phase 5 (§5.4) — every holder of the
parent applies the same `1-w` factor. `w` is part of the Phase-3 allgather so
non-creating ranks receive it.

### 5.2 Phase 1.5 — communicate corner counts (Parith/ON)

The owner rank of `N'` will create one **local** FSKY row per shell corner landing
on `N'` (its `contributions_count`). Every rank that mirrors `N'` must create the
**same number of recv rows** so that `FR_NBCC` is symmetric (else
`SPMD_EXCH2_A_PON` over/under-runs its MPI buffer). Since a rank's shells are not
necessarily ghost-copied to the mirror rank, the count cannot be inferred locally;
it is gathered explicitly with `spmd_allgatherv` into
`(pon_uid_global, pon_n_global, pon_rank_global)` keyed by `parent_uid`.

### 5.3 Phase 2 — perform the local split

Only **local** (positive) shell ids are ever forwarded to the detach routines.
Local shells are first **sorted by shell user-id ascending** so the owner's rows
land in a reproducible order (owner's smallest-uid shell first). Then, per record:

| Condition | Action |
|-----------|--------|
| `weight == 1` (this rank owns `N'`) | `detach_node(...)` — full split: interface detach, new node, PON growth, non-local growth. Builds recv slots from `pon_ghost_contrib_per_rank`. |
| `weight == 0`, **has** local shells to `N'` | `mirror_node_split(...)` — create a **ghost copy** of `N'` (`MAIN_PROC=owner`, `WEIGHT=0`) with local shells repointed and `n_pon_recv` recv slots reserved for the owner. |
| `weight == 0`, **no** local shells to `N'` | ghost **placeholder**: create `N'` slot (so ghost shells referencing the new UID resolve to a positive local id), grow PON with only recv rows, no local shell repointing. |

### 5.4 Phases 3–5 — global UID de-duplication

The same physical split may be created on several ranks (owner + mirrors). To give
them one consistent user id:

1. **Phase 3**: `spmd_allgatherv` the `(parent_uid, owning_rank)` pairs from every
   creating rank.
2. **Phase 4**: sort the parent-uid list (`stlsort_int_int`); equal adjacent values
   = one physical split across ranks. Assign a single new uid deterministically
   (`old_max_uid+1`, consistent everywhere because `max_uid` was allreduce-maxed
   first). Write `itab/itabm1/nodglob/main_proc` on each rank that holds a local
   `N'`. `new_crack` counts **unique** splits; `numnodg` grows by that amount.
3. **Phase 5**: a rank that holds the parent but did **not** create `N'` scales the
   parent's `MS/MS0` (and `IN/IN0`, `A/AR`, non-local `MASS/MASS0`) by `1-w`
   via `scale_parent_on_noncreating_rank` — the same factor the creating ranks
   already applied in `set_new_node_values` / `detach_node_nloc` — keeping the
   parent bitwise identical on every rank and the total mass conserved. The
   fraction `w` of each split travels with the Phase-3 allgather.

---

## 6. Making room in the skyline — `update_pon_shells`

This is the heart of the PARITH/ON side. It is called (via
`detach_node_from_shells`) once per new node, on the owner, the mirror, and the
placeholder paths, always with the same contract.

Inputs: `old_node_id`, the shell list, `new_numnod` (= id of `N'`), `ispmd`, and
the recv descriptor `(n_recv, recv_procne)`.

### 6.1 Count local contributions

```fortran
new_id = new_numnod
contributions_count = number of (shell,corner) in shell_list whose corner == new_id
total_new_rows      = contributions_count + n_recv
```

`contributions_count` is the number of **local** FSKY rows `N'` needs;
`n_recv` is the number of **remote** placeholder rows (0 on the placeholder-less
mono case). The new node's FSKY band is `[local rows | recv rows]`.

### 6.2 Extend `ADSKY` by one node

`N'` takes id `new_numnod`, so its band is appended at the **end** of the current
FSKY:

```fortran
new_adsky(1:new_numnod)   = adsky(1:new_numnod)                 ! unchanged
new_adsky(new_numnod + 1) = new_adsky(new_numnod) + total_new_rows
elements%pon%sadsky       = new_numnod + 1
```

Invariant maintained: `ADSKY(last) == current FSKY length`, so sequential splits
in the same cycle keep stacking their bands at the tail without overlap.

### 6.3 Reassign `IADC` for the moved corners

Each corner of the split shells that now points at `N'` is given a fresh FSKY row
inside `N'`'s **local** sub-band, in shell-list order (which was pre-sorted by
user-id for reproducibility):

```fortran
cc = 0
do over shell_list, corners:
  if corner == new_id:
    elements%pon%iadc(j, shell_id) = elements%pon%adsky(new_numnod) + cc   ! band base + offset
    cc = cc + 1
```

This is precisely what guarantees the §2.3 invariant for the new node:
`IADC(j,e) ∈ [ADSKY(N'), ADSKY(N')+contributions_count-1] ⊂ [ADSKY(N'),ADSKY(N'+1)-1]`.

> Note: the parent node `N` keeps its original `ADSKY` band and its remaining
> corners keep their original `IADC`. Only the corners that migrate to `N'` are
> touched. Rows in `N`'s old band that belonged to migrated corners simply become
> stale: after the split they are no longer written by any element, and because
> `update_pon_shells` zeroed the whole `FSKY` at the split (§6.4) they stay at
> zero — so they correctly contribute nothing to `N` (the migrated shell has left
> `N`). The band is intentionally **not** compacted so that pre-existing
> `ISENDP/IRECVP` pointers to `N`'s rows stay valid.

### 6.4 Extend `FSKY` and `PROCNE`

```fortran
sfsky_old = elements%pon%sfsky / 8
i         = sfsky_old + total_new_rows
call extend_array(elements%pon%fsky, 8, sfsky_old, 8, i)     ! grow to i rows
elements%pon%sfsky        = i * 8
elements%pon%fsky(:,1:i)  = 0                    ! active rows rewritten each cycle by cupdtn3; stale rows stay 0

if (total_new_rows > 0) then
  call extend_array(elements%pon%procne, sfsky_old, i)
  ! local rows: this rank owns them
  do cc = 1, contributions_count
    procne(sfsky_old + cc) = ispmd + 1
  end do
  ! recv rows: reserved for a remote rank's contribution
  do cc = 1, n_recv
    procne(sfsky_old + contributions_count + cc) = recv_procne(cc)
  end do
end if
```

So `PROCNE` is what later tells `REBUILD_PON_TABLES` which of `N'`'s rows are
**SEND** (`== ispmd+1`) and which are **RECV** (`== remote+1`) — see §2.4.

### 6.5 Who supplies `n_recv` / `recv_procne`

| Caller | `n_recv` | `recv_procne` values |
|--------|----------|----------------------|
| `detach_node` (owner) | `sum(pon_ghost_contrib_per_rank)` | one slot per remote rank's contribution, value = `remote_rank+1` |
| `mirror_node_split` (ghost of `N'`) | `n_owner_contrib_pon` (owner's corner count, from Phase 1.5) | `owning_rank+1` for every slot |
| ghost **placeholder** (no local shells) | `n_owner_contrib_pon` | `owning_rank+1` for every slot |
| mono / no MPI | `0` | — |

The symmetry `FR_NBCC(1, ghost) == FR_NBCC(2, owner)` (and vice-versa) is what
makes `SPMD_EXCH2_A_PON` buffers match on both ends.

### 6.6 Why it can look like an off-by-one (`cupdtn3` write vs `asspar4` read)

A natural first suspicion is that `IADC` (the row `cupdtn3.F` *writes*) is shifted
by one relative to the `ADSKY` band (the rows `asspar4.F` *reads*). It is not —
here is why the appearance arises.

New nodes' FSKY rows are appended **per node** as `[local … local | recv … recv]`
blocks (§6.2/§6.4). When each shared node has 1 local + 1 recv row, the local
rows land at **stride 2** while `asspar4` reads each band contiguously:

```
FSKY row:  10088   10089   10090   10091   ...
role:      A.local A.recv  B.local B.recv
IADC       ^13326c1        ^13326c4                <- cupdtn3 writes only these (10088,10090,...)
asspar4:   A reads {10088,10089}   B reads {10090,10091}   <- reads the whole band
```

(Observed on rank 5: `IADC(1,13326)=10088` for node 20024, and shell 13326's other
detached corner `IADC(4,13326)=10090` for the next split node — so the element-side
writes skip 10089, 10091 … .)

The "skipped" odd rows (`10089`, `10091`, …) are **receive placeholders**, filled
by `SPMD_EXCH2_A_PON` from the partner rank, **not** by any local element. So a row
that `asspar4` reads but no local `cupdtn3` wrote is *by design* — it is where the
remote contribution lands.

Three checks confirm the indexing is correct and the fault is in the *data*, not
the *index*:

1. `check_pon_consistency` passes — every local `IADC` lies in its node's `ADSKY`
   band with no double-claim (`IADC(1,13326)=10088` is exactly node 20024's first
   band row).
2. The reverse exchange direction (ghost recv ← owner send) is **bitwise-correct**
   every cycle under the *same* indexing scheme.
3. In a 1-rank run there are **no** recv rows: node 20024's band is `[13326c1 |
   13413c4]`, both local, contiguous, both written by `cupdtn3` — so no "shift"
   appears and `A(20024)=0`.

The real fault (§9) is that the *value* delivered into the recv slot is a foreign
row's force (misordered send/recv pairing), not a shifted `IADC`/`ADSKY` index.

### 6.7 The non-local analog — `detach_node_nloc`

`detach_node_nloc` (`engine/source/engine/node_spliting/detach_node_nloc.F90`) does
for `NLOC_DMG` what `set_new_node_values` + `update_pon_shells` do for the nodal
arrays and the mechanical skyline. It is called from `detach_node`,
`mirror_node_split`, and the ghost-placeholder branch of `apply_crack`, right after
their mechanical counterparts. Its steps (only when `IDXI(parent) > 0`, i.e. the
parent is a non-local node):

1. **Node-index tables** — extend `IDXI` (by one local id), `INDX` (append the new
   local id), and `POSI` (append `L_NLOC + NDDL + 1`); increment `NNOD`, `L_NLOC`.
2. **DOF-space vectors** — extend `VNL/UNL/DNL/MASS/MASS0/…` by `NDDL`, copying the
   parent's DOF values into the child; split the non-local `MASS/MASS0` between
   parent (`1-w`) and child (`w`) with the same area-weighted fraction as the
   mechanical mass (§5.1.5); zero the `FNL/STIFNL` accumulators for the new DOFs.
3. **Skyline (PARITH/ON)** — extend `ADDCNE` by one node, append `PROCNE` rows
   (`ISPMD+1` for local contributions, `remote+1` for recv rows — exactly the
   mechanical convention), grow `FSKY/STSKY`, and reassign `IADC` for the migrated
   shell corners into the new node's band.

The MPI send/recv tables (`ISENDSP/IRECSP/…`) are **not** patched here; like the
mechanical `ISENDP/IRECVP` they are rebuilt wholesale after the cycle's splits by
`SPMD_SUB_BOUNDARIES` (§8). This split is where the second reproducibility bug lived
(§9.3).

---

## 7. Mirror and ghost objects — precise definitions

Three distinct "phantom" concepts appear; they are easy to confuse:

### 7.1 Ghost shells — `element%ghost_shell`

A per-rank, read-only **mirror of neighbouring ranks' shells** that touch one of
this rank's boundary nodes. Built by `init_ghost_shells`, damage exchanged by
`spmd_exchange_ghost_shells`. Used for:

* the **ownership race** (§5.1) — a rank can see the user-ids of remote shells
  attached to a shared node and thus agree on the owner without communicating;
* building the **non-local neighbourhood** (`addcnel/cnel`).

Ghost shells are indexed by negative numbers in `shell_uids` and are **never**
detached or written into `FSKY`.

### 7.2 Mirror node — `mirror_node_split`

A **ghost copy of the new node `N'`** created on a rank that has local shells
migrating to `N'` but does **not** own `N'`. Characteristics:

* `MAIN_PROC(N') = owning_rank+1`, `WEIGHT(N') = 0` (output routines skip it);
* its local shells are repointed to `N'` (so they write real local FSKY rows);
* its `ADSKY` band = `[local rows | recv rows]` where the recv rows are reserved
  for the **owner's** contributions (`recv_procne = owning_rank+1`).

Each step, boundary exchange propagates `N'`'s owner-side data into the mirror so
both ranks assemble the identical total force for `N'`.

### 7.3 Ghost placeholder — `apply_crack`, `local_n == 0` branch

A rank that **knows the parent** (holds it as a boundary node) but has **no local
shells** going to `N'`. It still creates the `N'` slot with `WEIGHT=0` and grows
the PON skyline with **recv rows only**. Rationale: after `init_ghost_shells`,
ghost shells from the owner that reference the new UID must resolve to a
**positive local id**; without the placeholder the new UID would be stored as a
negative id and dropped from the non-local neighbourhood, causing slow MPI-vs-mono
divergence.

| Concept | Owns `N'`? | Has local shells at `N'`? | `WEIGHT` | Local FSKY rows | Recv FSKY rows |
|---------|-----------|---------------------------|----------|-----------------|----------------|
| Owner (`detach_node`) | yes | yes | 1 | `contributions_count` | one per remote contribution |
| Mirror (`mirror_node_split`) | no | yes | 0 | its local corner count | `n_owner_contrib_pon` |
| Placeholder | no | no | 0 | 0 | `n_owner_contrib_pon` |

---

## 8. Post-split rebuild (MPI)

After all splits of the cycle, the MPI descriptors must be regenerated because
`ASSADD2` normally runs only once before the time loop:

1. `spmd_rebuild_boundary` + `merge_boundary_with_split` — rebuild
   `NODES%BOUNDARY` / `BOUNDARY_ADD` (boundary node lists per rank). The current
   boundary is snapshotted as a **rolling baseline** *before* the rebuild; the
   merge then keeps the baseline sections for existing nodes (local id ≤ `NUMNOD_OLD`)
   and appends the new split nodes (local id > `NUMNOD_OLD`) from the rebuild. The
   baseline must roll (re-snapshotted on *every* split, not only the first),
   otherwise nodes created in earlier splits are dropped — see §9.4.
2. `SPMD_SUB_BOUNDARIES(NLOC_DMG, BOUNDARY_ADD, BOUNDARY, NUMNOD_OLD, ITAB)` —
   rebuild the **non-local** exchange tables (`NLOC_DMG%FR_ELEM/IAD_ELEM`, then
   `ISENDSP/IRECSP/IADSDP/IADRCP/FR_NBCC`) from the merged boundary. Like the
   mechanical `REBUILD_PON_TABLES` it enumerates each rank's boundary nodes and
   classifies each skyline row as send/recv by `PROCNE`; it therefore needs the
   **same split-node ordering fix** — new non-local nodes (`INDX(NN) > NUMNOD_OLD`)
   are sorted by `ITAB(INDX(NN))` so the owner SEND order matches the partner RECV
   order (§9.3). Also drives `SPMD_EXCH_SUB_PON` next cycle.
3. `init_ghost_shells(...)` — refresh the ghost-shell mirror set for the new
   topology.
4. `ASSINIT` + reallocate `ISENDP/IRECVP` + `REBUILD_PON_TABLES(...)` — rebuild
   the Parith/ON send/receive tables (`FR_NBCC`, `IADSDP`, `IADRCP`, `ISENDP`,
   `IRECVP`) from the updated `ADSKY` + `PROCNE` + boundary. This is what makes
   `SPMD_EXCH2_A_PON` actually transfer the new node's rows next cycle.
5. `INIT_NODAL_STATE`, `CHKINIT` (rebuild `ADDCNEL/CNEL`), skyline reallocation,
   rigid-wall re-registration, output-array resize.

> Two independent skylines ⇒ **two** table rebuilds that must both use a
> globally-consistent new-node order: `REBUILD_PON_TABLES` (mechanical, step 4) and
> `SPMD_SUB_BOUNDARIES` (non-local, step 2). Fixing only one leaves the other
> non-reproducible (this is exactly what happened — see §9.1/§9.2 for the
> mechanical fix and §9.3 for the non-local one).

---

## 9. Consistency invariant and the reproducibility bugs (post-mortem)

The single most important invariant (from §2.3) is that **every local shell
corner's `IADC` entry lies inside its node's `ADSKY` band, and every FSKY row is
claimed exactly once**. `check_pon_consistency` (wired into `nloc_shell_detach`)
audits this and prints `[PON_AUDIT] MISMATCH` / `FSKY_COLLISION` if broken.

The rest of this section is a post-mortem of the two PARITH/ON reproducibility
bugs found while bringing up node splitting under MPI. Both are **fixed** in the
current sources (§9.2 in `assadd2.F`, §9.3 in `spmd_exch_sub.F`); the diagnosis
is kept here because it documents *how* to trace this class of bug.

### Worked example (from `log_mpi.txt` / `log_mono.txt`)

Parent node uid `10711` splits into a new node uid `20024`.

* **Mono (1 rank)** — `20024` is a normal owned node. `ASSPAR4` reads its band as
  `K = 79243, 79244` (2 rows): shell `13326` corner 1, then shell `13413`
  corner 4. Both local. Sum order: `13326c1 + 13413c4`.

* **8-MPI** — the split straddles ranks 5 and 7:
  * Rank **5** owns `20024` (local id `2551`, `weight=1`). `update_pon` sets
    `IADC(1,13326) = 10088`; band `= [10088 (local, PROCNE=6), 10089 (recv from
    rank 7, PROCNE=8)]`.
  * Rank **7** mirrors `20024` (local id `2577`, `weight=0`). `update_pon` sets
    `IADC(4,13413) = 10191`; band `= [10191 (local, PROCNE=8), 10192 (recv from
    rank 5, PROCNE=6)]`.
  * After `SPMD_EXCH2_A_PON`: rank 5 row `10089 ← rank7:10191` (`13413c4`), and
    rank 7 row `10192 ← rank5:10088` (`13326c1`). Both ranks then assemble
    `13326c1 + 13413c4` in the **same order** as mono → bitwise identical **iff**
    the bands, `IADC`, `PROCNE` and send/recv tables are all consistent.

The reported symptom was **wrong `A(20024)` only with 8 MPI, starting ~cycle 1805**.
Tracing the FSKY contribution rows across `log_mono.txt` / `log_mpi.txt` gave a
precise, bitwise diagnosis:

| Contribution (row) | mono vs MPI-local | verdict |
|--------------------|-------------------|---------|
| `13413c4` (rank7 local `10191`) | equal to mono `79244` for **all 1806** cycles | element force OK |
| `13326c1` (rank5 local `10088`) | equal to mono `79243` until cycle **1805** | diverges *after* the exchange breaks |
| `rank7` recv `10192` ← `rank5` send `10088` | **bitwise equal** every cycle (incl. 1805) | exchange OK |
| `rank5` recv `10089` ← `rank7` send `10191` | **wrong**: `10089` becomes non-zero at cycle **1804** while the intended source `10191` is still exactly `0` (it only turns non-zero at 1805) | **exchange misrouted** |

So the root cause is **not** the local `IADC`/`ADSKY` mapping — `check_pon_consistency`
reports zero mismatches/collisions, and the `13413c4`/`13326c1` element forces match
mono until the corruption arrives. The failure is **directional**: the *owner* rank
(5) receives the *ghost* rank (7) contribution into the wrong slot — `10089`
picks up a foreign row's value one cycle before its true source is even non-zero —
whereas the reverse direction (ghost receiving owner) is bitwise-correct. This
corrupts `A(20024)` on the owner at 1804, which feeds back into shell `13326`'s
kinematics and makes its force diverge at 1805, then cascades.

A directional, node-specific mismatch like this lives in the **post-split rebuild
of the Parith/ON exchange tables** — `spmd_rebuild_boundary` /
`merge_boundary_with_split` → `ASSINIT` → `REBUILD_PON_TABLES`
(`ISENDP`/`IRECVP`/`IADSDP`/`IADRCP`/`FR_NBCC`) — an **inconsistent
ordering/placement of the new node in the two ranks' rebuilt `BOUNDARY` lists**, so
`SPMD_EXCH2_A_PON` paired rank 5's recv slot for `20024` with the wrong rank 7 send
slot. The recv counts (`FR_NBCC`) for the node were symmetric (1 send / 1 recv on
each side), which pointed at *ordering* rather than *count* — confirmed in §9.1.

### 9.1 Confirmed root cause (`[EXCH_DBG]` table dump)

Instrumenting `REBUILD_PON_TABLES` to print the ordered `ISENDP`/`IRECVP`
construction for the (5,7) pair confirmed it exactly. All 31 pre-existing (Pass 1)
entries pair correctly; the mismatch is entirely in **Pass 2 (new split nodes)**:

```
pos  rank7 SEND->r5     rank5 RECV<-r7
31   uid20024 cc10191   uid20023 cc10086   <- mispaired
32   uid20023 cc10189   uid20023 cc10087
33   uid20023 cc10190   uid20024 cc10089   <- mispaired
```

rank 7 enumerates the new nodes as **[20024, 20023, 20023]** (local ids 2577→2576)
but rank 5 as **[20023, 20023, 20024]** (local ids 2550→2551). `SPMD_EXCH2_A_PON`
pairs entry *k* on the sender with entry *k* on the receiver, so node 20024's force
(`cc10191`) is delivered into node 20023's slot (`cc10086`) and node 20023's force
lands in 20024's slot (`cc10089`).

Why Pass 2 diverges while Pass 1 does not: a **split node is bidirectional** — the
owner *and* the mirror both assemble it, so it carries both a SEND row and a RECV
row on each rank. `REBUILD_PON_TABLES` walks the boundary block *recv-sub-block then
send-sub-block* and classifies by `PROCNE`. Because ownership is opposite for 20024
(owned by 5) and 20023 (owned by 5 but with all contributions on 7), each node sits
in a different sub-block on each rank, so the boundary-order walk visits them in a
different relative order on the two ranks. Pre-existing nodes are unidirectional
(one owner accumulates; others only send), so their single-direction ordering was
already consistent from the startup decomposition. The `r5→r7` direction happens to
have only one Pass-2 entry (20024) and therefore *accidentally* stays aligned.

### 9.2 Fix

Enumerate Pass-2 (split) nodes in a **globally consistent order** instead of
boundary order. `REBUILD_PON_TABLES` now collects the new nodes for each partner,
**insertion-sorts them by `ITAB` (global user id, identical on every rank)**, and
builds `ISENDP`/`IRECVP` in that order. Both ranks then enumerate `[20023, 20024]`,
so the owner SEND order matches the partner RECV order:

```
rank7 SEND->r5 (sorted): 20023 cc10189, 20023 cc10190, 20024 cc10191
rank5 RECV<-r7 (sorted): 20023 cc10086, 20023 cc10087, 20024 cc10089   -> aligned
```

Pass 1 is unchanged (already correct). Only the mechanical Parith/ON force tables
need this because they classify by `PROCNE` over the combined boundary block;
unidirectional exchanges (e.g. `SPMD_SD_XV` for X/V) use the send/recv sub-blocks
directly and are already consistently ordered. Implemented in
`engine/source/assembly/assadd2.F` (`REBUILD_PON_TABLES`, new `ITAB` argument);
the call in `resol.F` passes `NODES%ITAB`.

> **Debugging note:** `cupdtn3.F` prints the FSKY row `K` each shell corner
> *writes*; `asspar4.F` prints the rows `K` each node *reads*. A mismatch between
> "the `K` a shell of node 20024 writes" and "the `K` node 20024 reads" is a
> direct violation of §2.3 and pinpoints the offending `IADC`/`ADSKY`/`PROCNE`
> entry.

### 9.3 Secondary root cause — the non-local (NLOC) skyline had the *same* bug

After the mechanical fix (§9.2), the split nodes were bitwise-reproducible, but a
**second** divergence appeared later (cycle ~1984) on *pre-existing* nodes
(13789, 13790, 13881), not on split nodes. Tracing both skylines separated cause
from symptom:

* the mechanical assembled force (`[TRACEA]`, sum over the `ADSKY` band) matched
  mono until ~1984 and then diverged only by **last bits**;
* the **non-local field `VNL`** (`[NLOC_NODE]`) was exactly `0.0` in both runs
  through cycle 1800, then activated around 1975 and **immediately diverged by
  large amounts** — for split node 20024 the MPI values *oscillated wildly*
  (`+1.7e-8, −1.6e-7, −2.0e-7 …`) versus mono's smooth growth.

So the **non-local skyline diverged first (≈1975) and the mechanical force
followed (1984)**. The mechanism: non-local damage is, by definition, a
**neighbourhood average**, so a split node whose `NLOC` force is wrong pollutes its
neighbours' damage → their stress → their element forces. That is why the visible
acceleration error surfaced on 13789/13790/13881 (neighbours of the split) rather
than on the split node itself.

**Root cause:** `SPMD_SUB_BOUNDARIES` (`engine/source/mpi/spmd_exch_sub.F`) builds
the NLOC `ISENDSP/IRECSP` tables by walking `NLOC_DMG%FR_ELEM` — which is derived
from `NODES%BOUNDARY` and therefore lists new split nodes in the **same
inconsistent per-rank order** that broke the mechanical tables. It is the identical
bug as §9.1, in the second skyline.

**Fix (identical pattern):** two-pass fill in `SPMD_SUB_BOUNDARIES` — pre-existing
non-local nodes (`INDX(NN) ≤ NUMNOD_OLD`) in `FR_ELEM` order, then new split nodes
(`INDX(NN) > NUMNOD_OLD`) **insertion-sorted by `ITAB(INDX(NN))`** (global uid).
`SPMD_SUB_BOUNDARIES` gains `NUMNOD_OLD` + `ITAB` arguments; the startup call in
`resol_init.F` passes `NUMNOD` (so no node is "new" → identical to before), and the
post-split call in `resol.F` passes `NUMNOD_OLD`.

> **Lesson:** with two independent Parith/ON skylines, every ordering fix must be
> applied to **both** `REBUILD_PON_TABLES` *and* `SPMD_SUB_BOUNDARIES`. A
> non-reproducible NLOC field manifests indirectly, on the *neighbours* of the
> split, one physical layer downstream — trace `NLOC_DMG%VNL` per node, not just
> the mechanical force, to see it.

### 9.4 Merge baseline must roll across multiple splits

`merge_boundary_with_split` (`spmd_rebuild_boundary.F90`) rebuilds the boundary as
*baseline entries* (kept wholesale) *plus* the rebuilt entries whose local id is
`> NUMNOD_OLD` (the new split nodes of the current split). The baseline is held in
the `SAVE` arrays `NLOC_BND_ADD_ORIG` / `NLOC_BND_ORIG` in `resol.F`.

**Bug (multi-split):** the baseline was captured only **once**, on the first split
(guarded by `NLOC_BND_NSPMD_SAVE < 0`), i.e. it froze the *startup* boundary. But
`NUMNOD_OLD = NUMNOD` is re-read before every split and `NUMNOD` grows each time
(`apply_crack`: `nodes%numnod = nodes%numnod + 1`). So on split *N > 1* a node
created in an earlier split satisfies **neither** side of the merge:

* it is **not** in the frozen startup baseline (created after startup), and
* it is **not** "new" any more, because its id is now `≤ NUMNOD_OLD`.

It is therefore written to neither `merged_bnd` sub-block and silently **dropped**
from `BOUNDARY` / `BOUNDARY_ADD` when `move_alloc` replaces them — removing that
node from all subsequent Parith/ON and NLOC boundary exchanges.

**Fix:** make the baseline **roll**. Snapshot the *current* `BOUNDARY` /
`BOUNDARY_ADD` into `NLOC_BND_*_ORIG` on **every** split, *before* calling
`spmd_rebuild_boundary`. Before split *N* the current boundary is exactly split
*N−1*'s merged result, so it already contains all earlier split nodes; the merge
keeps them (baseline side) and appends only split *N*'s new nodes (id
`> NUMNOD_OLD`). The two sets stay disjoint (the snapshot precedes this split's
node creation), so no duplicates are introduced. Single-split runs are unaffected
(the first snapshot is still the startup boundary).

---

## 10. Quick reference — call tree

```
resol.F (per cycle, after ASSPAR4)
└─ nloc_shell_detach                         detect splits, exchange ghost damage
   ├─ spmd_exchange_ghost_shells
   ├─ apply_crack                            ownership + dispatch + UID sync
   │  ├─ (Phase 1)   ownership race (ghost-shell min-uid)
   │  ├─ (Phase 1.5) spmd_allgatherv corner counts (Parith/ON)
   │  ├─ (Phase 2)
   │  │   ├─ detach_node                     weight=1 owner
   │  │   │   ├─ detach_node_from_interfaces
   │  │   │   ├─ extend_nodal_arrays / set_new_node_values
   │  │   │   ├─ detach_node_from_shells ─ update_pon_shells   (FSKY/ADSKY/IADC/PROCNE)
   │  │   │   └─ detach_node_nloc                              (non-local skyline)
   │  │   ├─ mirror_node_split               weight=0 + local shells (ghost copy of N')
   │  │   │   ├─ set_new_node_values (WEIGHT=0, MAIN_PROC=owner)
   │  │   │   ├─ detach_node_from_shells ─ update_pon_shells   (recv rows only for owner)
   │  │   │   └─ detach_node_nloc
   │  │   └─ (placeholder)                   weight=0, no local shells
   │  │       └─ update_pon_shells + detach_node_nloc (recv rows only)
   │  ├─ (Phase 1.6) split_mass_fraction: area-weighted mass share w (§5.1.5)
   │  └─ (Phase 3-5) spmd_allgatherv (uids, ranks, w) + stlsort_int_int + de-dup UID,
   │                 scale parent mass by 1-w on non-creating ranks
   └─ check_pon_consistency                  audit IADC ⊂ ADSKY, FSKY row aliasing
   (back in resol.F, if new_crack>0 and NSPMD>1)
   ├─ spmd_rebuild_boundary / merge_boundary_with_split
   ├─ SPMD_SUB_BOUNDARIES (non-local)        rebuild NLOC ISENDSP/IRECSP/FR_NBCC
   │                                          (Pass-2 new nodes sorted by ITAB, §9.3)
   ├─ init_ghost_shells
   └─ ASSINIT + REBUILD_PON_TABLES           rebuild mechanical ISENDP/IRECVP/…
                                              (Pass-2 new nodes sorted by ITAB, §9.2)
```

> The two `Pass-2 sorted by ITAB` steps are the two independent fixes required for
> full `/PARITH/ON` reproducibility after a split: one per skyline (mechanical and
> non-local).
