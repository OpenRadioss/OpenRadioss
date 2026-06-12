# Element Groups, Connectivity Arrays, and Nodal Arrays

This document describes the element-group system (`IPARG`/`NGROUP`/`MVSIZ`),
the connectivity arrays (`IXS`, `IXC`, `IXP`, `IXT`, `IXR`, `IXTG`, `IXQ`),
and the main nodal arrays (`X`, `V`, `A`, `MS`, `ITAB`). These are the backbone
of all element computations and are present in virtually every element kernel call.

---

## 1. Element groups and IPARG

### 1.1 What is a group?

All elements in OpenRadioss are partitioned into **groups**. A group is a contiguous
block of elements of the same type (solid, shell, beam, spring, …), sharing the
same material law and property set, which are processed together in a vectorized loop
of length at most `MVSIZ` (typically 256 elements, set from common block via `MVSIZ_MOD`).

The total number of groups is `NGROUP`. The Starter assigns elements to groups during
the grouping pass (`SGRTAILS.F`, `CGRTAILS.F`, `PGRTAILS.F`, `TGRTAILS.F`,
`RGRTAILS.F`, etc.).

### 1.2 `IPARG` — the group descriptor array

`IPARG(NPARG, NGROUP)` is the integer parameter array describing every group.
`NPARG = 100` (set in `starter/source/starter/starter0.F`, line 551).
`IPARG` is stored in the `PARAM` common block (`engine/share/includes/param_c.inc`).

**Well-defined slots (verified from `sgrtails.F`, `cgrtails.F`):**

| Slot | Symbol | Meaning (for solid groups; may differ for other element types) |
|------|--------|-----------------------------------------------------------------|
| `IPARG(1, NG)` | MLN | Material law number |
| `IPARG(2, NG)` | NEL | Number of elements in the group |
| `IPARG(3, NG)` | NFT-1 | Index of first element − 1 (offset into IXS/IXC…) |
| `IPARG(4, NG)` | LBUFEL+1 | Buffer layout flag (legacy, kept for compatibility) |
| `IPARG(5, NG)` | ITY | Element type index (dispatch key in `FORINT`) |
| `IPARG(6, NG)` | NPT | Number of integration points |
| `IPARG(7, NG)` | JALE | ALE flag |
| `IPARG(9, NG)` | ISSN | Sub-mesh numbering flag |
| `IPARG(10, NG)` | ICPRE | Constant pressure formulation flag |
| `IPARG(11, NG)` | JEUL | Eulerian flag |
| `IPARG(12, NG)` | JTUR | Turbulence model flag |
| `IPARG(13, NG)` | JTHE | Thermal coupling flag |
| `IPARG(14, NG)` | JLAG | Lagrangian flag |
| `IPARG(17, NG)` | ICSTR | Strain computation flag |
| `IPARG(18, NG)` | MID | Material ID (local material index) |
| `IPARG(43, NG)` | IFAIL | Failure model flag |
| `IPARG(52, NG)` | JSMS | AMS selector |
| `IPARG(55, NG)` | IEOS | EOS flag |

Additional slots (up to 93 observed for solid groups) carry flags for hourglass
control, void elements, SPH, print control, etc. Shell groups use overlapping but
distinct semantics for some slots (e.g. `IPARG(6)` = number of integration points
through thickness for shells).

### 1.3 Element type values (`IPARG(5, NG)`)

In `FORINT` (`engine/source/elements/forint.F`), the SELECT CASE on `IPARG(5, NG)`
dispatches to the element kernel:

| Value | Element family |
|-------|---------------|
| 1 | 8-node hexahedral solid (`SFORC3` / `S4FORC3`) |
| 2 | Solid with extra EOS variables (`S8ZFORC3`) |
| 14 | 20-node hex (`S20FORC3`) |
| etc. | (many values; see `forint.F` for the complete CASE list) |

Shell groups are handled by `FORINTC` (4-node) and `FORINTS` (3-node, SPH, etc.)
which have their own dispatch tables.

### 1.4 `MVSIZ` — vectorization chunk size

`MVSIZ` is the canonical inner-loop length used throughout the element kernels.
It is defined in `MVSIZ_MOD` (`common_source/modules/`) and typically set to 256.
Groups are subdivided into chunks of `MVSIZ` elements, iterated with
`LFT = 1, LLT = MIN(LFT+MVSIZ-1, NEL)` style loops.

---

## 2. Connectivity arrays

All element types have a dedicated connectivity array. Their dimensions are defined
as Fortran parameters in `common_source/modules/elements/element_mod.F90`
(lines 1170–1180):

| Array | Fortran parameter | First dim | First dim value | Used for |
|-------|-------------------|-----------|-----------------|----------|
| `IXS(NIXS, NUMELS)` | `nixs = 11` | 11 | verified | Solid elements (8-noded hex, tetra10, etc.) |
| `IXC(NIXC, NUMELC)` | `nixc = 7` | 7 | verified | 4-node quadrilateral shells (coque4n) |
| `IXTG(NIXTG, NUMELTG)` | `nixtg = 6` | 6 | verified | 3-node triangle shells |
| `IXT(NIXT, NUMELT)` | `nixt = 5` | 5 | verified | Truss elements |
| `IXP(NIXP, NUMELP)` | `nixp = 6` | 6 | verified | Beam elements |
| `IXR(NIXR, NUMELR)` | `nixr = 6` | 6 | verified | Spring/damper elements |
| `IXQ(NIXQ, NUMELQ)` | `nixq = 7` | 7 | verified | 2D quad elements |

### 2.1 `IXS` — solid connectivity (11 × NUMELS)

From `sgrtails.F` line 69 comment: "CONECS+PID+MID+NOS SOLIDS".

| Row | Contents |
|-----|----------|
| `IXS(1, IE)` | Material ID (internal, = index into PM) |
| `IXS(2..9, IE)` | 8 node indices (local internal node IDs, 1-based) |
| `IXS(10, IE)` | Property ID (= `NIXS-1`) |
| `IXS(11, IE)` | User element ID (= `NIXS`) |

`IXS(NIXS-1, II)` is the property ID (`PID`) and `IXS(NIXS, II)` is the user
element ID, verified from `sgrtails.F` lines 460-461:
```fortran
MID = IXS(1,II)
PID = IXS(NIXS-1,II)
```

### 2.2 `IXC` — 4-node shell connectivity (7 × NUMELC)

From `cgrtails.F` line 67 comment: "MID(1)+CONECS(2-5)+PID(6)+…".

| Row | Contents |
|-----|----------|
| `IXC(1, IE)` | Material ID |
| `IXC(2..5, IE)` | 4 node indices |
| `IXC(6, IE)` | Property ID (verified in `cinit3.F` line 256) |
| `IXC(7, IE)` | User element ID |

### 2.3 `IXT` — truss connectivity (5 × NUMELT)

From `tgrtails.F` line 49 comment: "CONECS+PID+MID+NOS TRUSSES".

| Row | Contents |
|-----|----------|
| `IXT(1, IE)` | Material ID |
| `IXT(2..3, IE)` | 2 node indices |
| `IXT(4, IE)` | Property ID (verified in `tgrtails.F` line 234) |
| `IXT(5, IE)` | User element ID |

### 2.4 `IXP` — beam connectivity (6 × NUMELP)

From `pgrtails.F` line 58 comment: "CONECS+PID+MID+NOS POUTRES" (POUTRE = French for beam).

| Row | Contents |
|-----|----------|
| `IXP(1, IE)` | Material ID |
| `IXP(2..3, IE)` | 2 end-node indices |
| `IXP(4, IE)` | Orientation node (or skew ID) |
| `IXP(5, IE)` | Property ID |
| `IXP(6, IE)` | User element ID |

### 2.5 `IXR` — spring/damper connectivity (6 × NUMELR)

From `rgrtails.F` line 53 comment: "CONNECTIVITY+PID+SPRING NUMBERS TABLE".
Layout similar to beams (MID, nodes, PID, user ID).

### 2.6 Addressing within a group

All connectivity arrays are 1-indexed globally across all elements of a given type.
Group NG owns elements from index `NFT+1` to `NFT+NEL` where:
- `NFT = IPARG(3, NG)` (first element offset, minus 1)
- `NEL = IPARG(2, NG)` (number of elements in the group)

Access pattern in element kernels:
```fortran
NFT  = IPARG(3, NG)         ! first element index - 1
NEL  = IPARG(2, NG)
LFT  = 1
LLT  = NEL                  ! (or MVSIZ in vectorized chunks)
!
! node indices for solid elements, element IE (IE = NFT+1..NFT+NEL):
IX1 = IXS(2, NFT+IE)
IX2 = IXS(3, NFT+IE)
! ... etc.
```

---

## 3. Nodal arrays

All nodal arrays use **internal sequential node numbering** (1..`NUMNOD`). The user
node IDs are stored in `NODES%ITAB`; the inverse mapping in `NODES%ITABM1`.

### 3.1 Core nodal arrays in the `NODES` structure

The `NODES` variable is of type `nodal_arrays` (defined in
`common_source/modules/nodal_arrays.F90`). Key fields:

| Field | Dimensions | Description |
|-------|-----------|-------------|
| `NODES%X(3, NUMNOD)` | `my_real` | Nodal coordinates (current) |
| `NODES%D(3, NUMNOD)` | `my_real` | Nodal displacements |
| `NODES%V(3, NUMNOD)` | `my_real` | Nodal translational velocities |
| `NODES%VR(3, NUMNOD)` | `my_real` | Nodal rotational velocities |
| `NODES%A(3, NUMNOD)` | `my_real` | Nodal forces → accelerations |
| `NODES%AR(3, NUMNOD)` | `my_real` | Nodal rotational forces → accelerations |
| `NODES%MS(NUMNOD)` | `my_real` | Nodal translational mass |
| `NODES%IN(NUMNOD)` | integer | Boundary condition fixity mask (translations) |
| `NODES%STIFN(NUMNOD)` | `my_real` | Nodal stiffness (translational, for DT) |
| `NODES%STIFR(NUMNOD)` | `my_real` | Nodal stiffness (rotational, for DT) |
| `NODES%ITAB(NUMNOD)` | integer | User node ID for internal node I |
| `NODES%ITABM1(2*NUMNOD)` | integer | Inverse: user → internal node ID (sorted pair array) |
| `NODES%NODGLOB(NUMNOD)` | integer | Global (across MPI domains) internal node ID |
| `NODES%WEIGHT(NUMNOD)` | `my_real` | MPI weight (1/number of domains owning this node) |
| `NODES%TEMP(NUMNOD)` | `my_real` | Nodal temperature (thermal coupling) |
| `NODES%MCP(NUMNOD)` | `my_real` | Nodal heat capacity × mass (thermal) |

`NODES%A` is used **both** for forces (during assembly) and accelerations (after
the `ACCELE` call divides by `NODES%MS`).

### 3.2 Internal vs user node numbering

- **Internal numbering**: sequential 1..`NUMNOD`, used in all element kernels and
  connectivity arrays. Determined by the Starter mesh-reading order, then permuted
  by domain decomposition.
- **User numbering**: the IDs from the input deck (can be non-sequential, large).
  Stored in `NODES%ITAB(I)` = user ID of internal node I.
- **Inverse mapping**: `NODES%ITABM1` is a sorted lookup array of size `2*NUMNOD`
  used to find the internal index from a user ID. Defined in
  `common_source/modules/nodal_arrays.F90` line 118.

### 3.3 MPI domain boundary nodes

In an MPI run, frontier (boundary) nodes are shared between domains. The exchange
lists are in `NODES%BOUNDARY_ADD(1, NSPMD+1)` and `NODES%BOUNDARY(…)`, built
during the Starter domain-decomposition pass. `NODES%WEIGHT` is `1/N` for a node
shared by N domains; all domains accumulate forces then multiply by `WEIGHT` to
avoid double-counting.

---

## 4. Lifecycle: Starter → Engine

### Starter

1. Mesh reading (`LECGRN`, `HM_READ_*`) fills `IXS`, `IXC`, `IXP`, `IXT`, `IXR`
   with user-ID connectivity, then remaps to internal IDs.
2. Grouping (`SGRTAILS`, `CGRTAILS`, etc.) assigns elements to groups, fills `IPARG`,
   initializes `ELBUF_TAB(NG)` via `ELBUF_INI`.
3. Domain decomposition (`DDSPLIT`) permutes node/element ordering so that each MPI
   domain owns a contiguous block; `ITAB`/`ITABM1` are set.
4. Restart files (`_NNNN.rst`) written per domain — contain `IPARG`, connectivity,
   `ELBUF_TAB`, nodal arrays, and boundary exchange lists.

### Engine

1. `LECTUR`/`LECSTAT`/`RDRESA` rebuild all arrays from restart files.
2. `RESOL` uses them unchanged throughout the time loop; connectivity is read-only.
3. After element failure (`DESACTI`), `IXS(1, IE)` may be negated or `ELBUF_TAB(NG)%GBUF%off(IE)` set to mark deletion.

---

## 5. Key files

| File | Contents |
|------|----------|
| `common_source/modules/elements/element_mod.F90` | `nixs`, `nixc`, `nixt`, `nixp`, `nixr`, `nixtg`, `nixq` constants (lines 1170–1180) |
| `common_source/modules/nodal_arrays.F90` | `nodal_arrays` type: `ITAB`, `ITABM1`, `X`, `V`, `A`, `MS`, … |
| `starter/source/elements/solid/solide/sgrtails.F` | Solid group builder — fills `IPARG` (lines 1467–1535), sets `IXS` row meaning (line 69 comment) |
| `starter/source/elements/shell/coque/cgrtails.F` | Shell group builder — `IXC` row meaning (line 67 comment) |
| `starter/source/elements/truss/tgrtails.F` | Truss group builder — `IXT` row meaning (line 49 comment) |
| `starter/source/elements/beam/pgrtails.F` | Beam group builder — `IXP` row meaning (line 58 comment) |
| `starter/source/elements/spring/rgrtails.F` | Spring group builder — `IXR` row meaning (line 53 comment) |
| `starter/source/starter/starter0.F` | `NPARG = 100` (line 551) |
| `engine/source/elements/forint.F` | Main element force dispatch — `IPARG(5,NG)` SELECT CASE |

---

## 6. See also

- `doc/ELBUF_TAB_documentation.md` — element state buffer indexed by `NG`
- `doc/ENGINE_TIME_LOOP_documentation.md` — how `FORINT` is called and `ACCELE` updates nodal arrays
- `doc/SPMD_documentation.md` — frontier node exchange using `NODES%BOUNDARY*`
