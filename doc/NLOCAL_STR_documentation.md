# `NLOCAL_STR_` — Non-Local Damage Regularization Data Structure

This document describes the `NLOCAL_STR_` derived type used by OpenRadioss to implement
**non-local damage regularization**, its complete field reference, its lifecycle across the
two binaries (Starter and Engine), its interaction with the element buffer structure
`ELBUF_TAB`, and the dedicated MPI (SPMD) communication routines.

---

## 1. Purpose and theoretical background

Softening material models (damage, e.g. Gurson-type porous plasticity) suffer from
**pathological mesh dependence**: when the material softens, deformation localizes in a band
whose width is set by the element size, so the dissipated energy goes to zero on mesh
refinement. The non-local (implicit-gradient / micromorphic) approach restores
mesh-objectivity by replacing a **local** state variable (typically the cumulated plastic
strain) with a **regularized, spatially smoothed** counterpart obtained as the solution of a
screened-Poisson (Helmholtz-type) equation over the structure:

```
u_nl - LEN² ∇²u_nl = u_local
```

where `LEN` is the **non-local internal length** which controls the width of the
localization band and becomes a material parameter.

In OpenRadioss this equation is not solved implicitly. It is solved **dynamically within
the explicit time integration scheme**: the non-local variable is carried by **additional
degrees of freedom** attached to the mesh nodes, given an artificial "mass" (`DENS`) and
"damping" (`DAMP`), and integrated in time exactly like the mechanical d.o.fs
(force assembly → acceleration → velocity → increment). The artificial dynamic
parameters are chosen automatically so that the non-local problem converges to the
quasi-static regularized solution without penalizing the mechanical time step.

The `NLOCAL_STR_` structure is the container for everything this auxiliary explicit
problem needs: the non-local d.o.f numbering, the artificial material parameters, the state
vectors (mass, force, velocity, increment, cumulated value), the Parith/ON skyline
assembly tables, and the SPMD (MPI) boundary communication tables.

---

## 2. Definition and location

The type is defined in the module **`NLOCAL_REG_MOD`**:

* File: `common_source/modules/nlocal_reg_mod.F` (type definition at lines 135–183)
* The module is shared by **both binaries** (it lives in `common_source`).
* The single instance of the structure is conventionally named **`NLOC_DMG`** throughout
  the code (Starter and Engine).

The module also defines two global constants and two helper routines:

| Item | Value / formula | Role |
|---|---|---|
| `CSTA` | `40.0` | Safety coefficient between non-local stability and mechanical stability. Used to compute the artificial density so the non-local time step does not drive the global time step (`nlocal_reg_mod.F:130`). |
| `CDAMP` | `0.7` | Stability coefficient accounting for damping; multiplies the non-local critical time step (`nlocal_reg_mod.F:133`). |
| `GET_LEMAX` | `LE_MAX = √π · LEN / 3` | Converts the internal length into the maximal element-size target guaranteeing convergence (`nlocal_reg_mod.F:195`). |
| `GET_LENGTH` | `LEN = 3 · LE_MAX / √π` | Inverse conversion (`nlocal_reg_mod.F:210`). |

---

## 3. Field reference

### 3.1 Scalar control fields

| Field | Type | Description |
|---|---|---|
| `IMOD` | integer | Non-local regularization activation flag. `> 0` means at least one material in the model uses non-local regularization; tested everywhere in the Engine (`NLOC_DMG%IMOD > 0`) to enable the non-local pipeline. |
| `NNOD` | integer | Number of nodes carrying additional (non-local) d.o.fs. Only nodes belonging to elements whose material is non-local are counted. |
| `L_NLOC` | integer | Total length of the non-local state vectors = sum over the `NNOD` nodes of their number of additional d.o.fs. |
| `NUMELS_NL` | integer | Number of non-local **solid** elements. |
| `NUMELC_NL` | integer | Number of non-local **4-node shell** elements. |
| `NUMELTG_NL` | integer | Number of non-local **3-node (triangle) shell** elements. |
| `NDDMAX` | integer | Maximum number of additional d.o.fs per node (a node may carry several non-local d.o.fs, e.g. one per integration layer through the shell thickness). Sized as `MAXVAL` of the per-node d.o.f counts. |
| `LCNE_NL` | integer | Length of the `PROCNE` connectivity table (Parith/ON). |

### 3.2 Per-material artificial parameters (dimension `NUMMAT`)

These arrays are indexed by the internal material number `IMAT` and are computed
automatically by the Starter (see §5.3):

| Field | Description |
|---|---|
| `LEN` | Non-local **internal length** ℓ (the physical regularization parameter, from the `/NONLOCAL` card or derived from `LE_MAX`). |
| `LE_MAX` | **Maximal element-length target**: above this element size the regularization is not guaranteed to converge. Linked to `LEN` by `LE_MAX = √π·LEN/3`. |
| `DENS` | Artificial non-local "**density**" ζ giving inertia to the non-local d.o.fs. Must be as small as possible (fast convergence to the quasi-static solution) but large enough not to decrease the global time step. |
| `DAMP` | Artificial non-local "**damping**" coefficient (homogeneous to a time) that damps the spurious oscillations of the auxiliary dynamic problem. |
| `SSPNL` | Non-local "**sound speed**": propagation speed of the non-local wave, used for the stability condition and for the absorbing boundary applied on deleted elements. |

### 3.3 Non-local d.o.f numbering (nodal maps)

| Field | Dimension | Description |
|---|---|---|
| `INDX` | `NNOD` | Non-local node → global node number (direct index table). |
| `IDXI` | `NUMNOD` | Global node number → non-local node number (inverse table, 0 if the node carries no non-local d.o.f). |
| `POSI` | `NNOD+1` | Position of the **first d.o.f of each non-local node** inside the `L_NLOC`-long state vectors (CSR-like cumulative addressing). The number of d.o.fs of node `N` is `POSI(N+1) - POSI(N)`. |

Typical access pattern used by every element and assembly routine:

```fortran
N    = NLOC_DMG%IDXI(global_node)   ! non-local node number
NPOS = NLOC_DMG%POSI(N)             ! first d.o.f of that node
NDDL = NLOC_DMG%POSI(N+1) - NPOS    ! number of d.o.fs of that node
```

### 3.4 State vectors of the auxiliary explicit problem (dimension `L_NLOC`)

| Field | Dimension | Description |
|---|---|---|
| `MASS` | `L_NLOC` | Current non-local nodal "mass" (volume × `DENS`, with Gauss/thickness weights). Can grow during the run by added-mass if `/DT/NODA/CST` style control is active on the non-local d.o.fs. |
| `MASS0` | `L_NLOC` | Initial non-local mass (reference for the added-mass ratio `√(MASS/MASS0)`). |
| `FNL` | `L_NLOC × NTHREAD` | Non-local **forces**, then **accelerations** (divided in place by `MASS` in `NLOCAL_ACC`). One column per OpenMP thread in Parith/OFF; a single column is used in Parith/ON. |
| `VNL` | `L_NLOC` | Non-local d.o.f **velocities**. |
| `VNL_OLD` | `L_NLOC` | Velocities of the previous cycle (needed by the absorbing forces on deleted elements). |
| `DNL` | `L_NLOC` | Non-local variable **increment** over the cycle (`DNL = dt · VNL`). This is the quantity interpolated back to the integration points and passed to the material laws. |
| `UNL` | `L_NLOC` | **Cumulated** non-local variable (`UNL += DNL`), i.e. the regularized counterpart of the local cumulated plastic strain at nodes. |
| `STIFNL` | `L_NLOC × NTHREAD` | Equivalent non-local nodal **stiffness**, accumulated during element loops and used by `NLOCAL_DTNODA` to compute the nodal non-local time step (only when nodal time step `NODADT > 0`). |

### 3.5 Parith/ON skyline assembly tables

Parith/ON (`/PARITH/ON`, `IPARIT ≠ 0`) requires bitwise-reproducible sums independent of
the domain decomposition and thread count. As for the mechanical forces, the non-local
element contributions are therefore **not** accumulated directly: each element writes its
contribution into a private slot of a skyline vector, and a deterministic gather produces
the nodal sum.

| Field | Dimension | Description |
|---|---|---|
| `ADDCNE` | `0:NNOD+1` | Address of each non-local node's slice in the skyline vector `FSKY` (cumulative). |
| `CNE` | `ADDCNE(NNOD+1)-1` | Element attached to each skyline slot (node→elements connectivity). |
| `PROCNE` | `LCNE_NL` | SPMD **processor number owning the element** of each skyline slot — this is what allows boundary slots to be filled by remote contributions (see §8.4). |
| `IADS` | `8 × NUMELS_NL` | Skyline addresses of the 8 nodes of each non-local solid element. |
| `IADC` | `4 × NUMELC_NL` | Skyline addresses of the 4 nodes of each non-local shell element. |
| `IADTG` | `3 × NUMELTG_NL` | Skyline addresses of the 3 nodes of each non-local triangle element. |
| `FSKY` | `ADDCNE(NNOD+1) × NDDMAX` | Skyline vector of the non-local **forces** (one row per element-node incidence, one column per additional d.o.f). |
| `STSKY` | `ADDCNE(NNOD+1) × NDDMAX` | Skyline vector of the non-local **stiffness** (when `NODADT > 0`). |

### 3.6 SPMD boundary communication tables

These describe which non-local d.o.fs sit on inter-domain boundaries (built by the Starter
during domain decomposition and finalized by `SPMD_SUB_BOUNDARIES` at Engine startup):

**Parith/OFF exchange (nodal force reduction):**

| Field | Dimension | Description |
|---|---|---|
| `IAD_ELEM` | `NSPMD+1` | For each remote domain `P`, address in `FR_ELEM` of the first boundary non-local node shared with `P` (cumulative). |
| `IAD_SIZE` | `NSPMD+1` | Cumulative **buffer size** (in d.o.fs, ×2 when stiffness is also exchanged) of the message exchanged with each remote domain. |
| `FR_ELEM` | total boundary nodes | List of the non-local boundary nodes (local non-local numbering), ordered by remote domain. |

**Parith/ON exchange (skyline slot exchange):**

| Field | Dimension | Description |
|---|---|---|
| `ISENDSP` | send count | Skyline offsets (within a node's `ADDCNE` slice) of the slots **owned locally** that must be sent. |
| `IRECSP` | recv count | Skyline offsets of the slots that will be **received** from the remote owner of the corresponding element. |
| `IADSDP` / `IADRCP` | `NSPMD+1` | Cumulative addresses of the send / receive lists per remote domain. |
| `FR_NBCC` | `2 × NSPMD` | `FR_NBCC(1,P)` = number of values to send to domain `P`; `FR_NBCC(2,P)` = number to receive. |
| `FR_ELEM_S` / `FR_ELEM_R` | send/recv counts | Non-local node associated with each sent / received skyline slot. |

---

## 4. Element-level computation (`*FINT_REG` routines)

Each element family has a dedicated non-local internal-force routine, called from the
`*FORC3` element force routine after the material law:

| Element type | Engine routine |
|---|---|
| 8-node brick | `engine/source/elements/solid/solide/sfint_reg.F` |
| HA8/H8Z bricks | `engine/source/elements/solid/solide8z/s8zfint_reg.F` |
| 4-node tetra | `engine/source/elements/solid/solide4/s4fint_reg.F` |
| Penta 6 | `engine/source/elements/solid/solide6z/s6fint_reg.F90` |
| Thick shells | `scfint_reg.F`, `s8cfint_reg.F`, `s6cfint_reg.F` |
| 4-node shells (QBAT/QEPH/Q4) | `cbafint_reg.F`, `cfint_reg.F` |
| 3-node shells | `c3fint_reg.F`, `cdkfint_reg.F`, `cdk6fint_reg.F` |

Taking `sfint_reg.F` (8-node brick) as reference, for each element the routine computes the
elementary residual of the regularization equation discretized with the same shape
functions as the displacement field:

* `NTN_UNL = NᵀN·UNL` — consistent "reaction" term of the non-local variable
  (`sfint_reg.F:207`),
* `NTN_VNL = ξ·NᵀN·VNL` — damping term, scaled by `√(MASS/MASS0)` when added mass is
  active (`sfint_reg.F:211–222`),
* `B = LEN²·Vol·(BᵀB)·UNL` — gradient (diffusion) term (`sfint_reg.F:225–255`),
* `NTVAR = ∫ Nᵀ·VAR_REG` — **source term**, where `VAR_REG` is the *local* variable to
  regularize (the cumulated plastic strain increment returned by the material law)
  (`sfint_reg.F:262`),

and assembles the nodal force `F = NTN_UNL + NTN_VNL − NTVAR + B` (`sfint_reg.F:265–273`).

Two important behaviors:

* **Deleted elements** (`OFF = 0`) do not vanish from the non-local problem: they apply an
  **absorbing force** `F = ζ·SSPNL·(VNL+VNL_OLD)/2·(3/4)·Lc²` so the non-local wave is
  absorbed instead of reflected at the crack faces (`sfint_reg.F:303–347`).
* Assembly is dual-mode (`sfint_reg.F:350–428`): Parith/OFF accumulates into
  `FNL(:,ITASK)` / `STIFNL(:,ITASK)`; Parith/ON writes each contribution into its
  reserved skyline slot `FSKY(IADS(k,elem),1)` / `STSKY(...)`.
* When the nodal time step is off (`NODADT = 0`), the routine also computes the non-local
  **elemental critical time step** and minimizes `DT2T` with it
  (`sfint_reg.F:430–444`):

  ```
  DTNL = 2·min(Le, LE_MAX)·√(3ζ) / √(12·LEN² + min(Le, LE_MAX)²)
  DT2T = min(DT2T, DTFAC·CDAMP·DTNL)
  ```

For shells, the interpolation of the nodal increment `DNL` to the Gauss points
(producing the `VARNL` array given to the material law) is done by e.g.
`engine/source/elements/shell/coqueba/cbavarnl.F:79–101`.

---

## 5. Lifecycle in the Starter

### 5.1 Input reading — `/NONLOCAL/MAT` card

`starter/source/materials/nonlocal/hm_read_nonlocal.F`:

* Reads `LENGTH` (internal length ℓ) and `LE_MAX` per material (lines 153–154). If
  `LE_MAX` is given it wins, and ℓ is derived via `GET_LENGTH`; otherwise `LE_MAX` is
  derived from ℓ via `GET_LEMAX` (lines 158–168).
* Flags the material: `MAT_PARAM(IMAT)%NLOC = 1` (line 170).
* Requests storage of the non-local variables in the element buffers (see §7):
  `MLAW_TAG(IMAT)%G_PLANL = L_PLANL = G_EPSDNL = L_EPSDNL = 1` (lines 174–177).

Non-local regularization is currently used by the **Gurson failure model**
(`/FAIL/GURSON`, `Iloc = 2` micromorphic or `Iloc = 3` Peerlings — see
`starter/source/materials/fail/gurson/hm_read_fail_gurson.F`) and the
**Drucker–Gurson material law 104** (`IGURSON = 2/3`, see
`starter/source/materials/mat/mat104/`).

### 5.2 Structure construction — `NLOC_DMG_INIT`

`starter/source/materials/fail/nloc_dmg_init.F` (called from
`read_material_models.F`) builds the whole structure:

1. **Node tagging** (lines 283–637): loops over solids (tetra4, penta6, brick8 and
   degenerated bricks; properties 14, 6, 20, 21), 4-node shells and 3-node shells
   (properties 1, 9). Each node of a non-local element is tagged with its number of
   additional d.o.fs:
   * tetra/penta: 1 d.o.f per node,
   * bricks with several layers in one direction: one d.o.f per layer,
   * shells: one d.o.f per **thickness integration point** (`NPTT`).
2. **Numbering** (lines 695–714): builds `NNOD`, `L_NLOC`, `INDX`, `POSI`, `IDXI`;
   `NDDMAX = MAXVAL(NDDL)` (line 906).
3. **Automatic artificial parameters** (lines 745–868), per attached element and stored
   per material (the **most restrictive** value over the elements is kept):

   ```
   SSP   = √((K + 4G/3)/ρ)                 (solids,  line 758)
   SSP   = √(E/(1−ν²)/ρ)                   (shells,  line 813)
   DTMIN = max(LE_MAX/SSP, DTMINI)                   (line 847)
   DENS  = CSTA·((LEN/LE_MAX)² + 1/12)·DTMIN²        (line 849)
   DAMP  = (2η/LE_MAX)·√(DENS·(LEN²π² + LE_MAX²))    (line 858, η = 0.2)
   SSPNL = √((LEN² + LE_MAX²/π²)/DENS)               (line 864)
   ```

   If `LE_MAX` was not provided it defaults to the minimal element characteristic length.
   A warning is emitted for elements larger than `LE_MAX`.
4. **Non-local mass** (lines 917–1077): allocates the state vectors and computes
   `MASS = MASS0 = w·VOLN·DENS` per d.o.f, with Gauss weights through the thickness for
   layered solids/shells. For shells and thick shells, the through-thickness masses
   `MASSTH` are also stored in `ELBUF_TAB(NG)%NLOC(IR,IS)%MASSTH` /
   `%NLOCTS(IR,IS)%MASSTH` (lines 1005, 1057) — see §7.2.
5. **Parith/ON tables**: `ADDCNE`, `CNE`, `IADS/IADC/IADTG` (skyline addressing) and
   `PROCNE` (element→processor map filled during decomposition).

### 5.3 Initial non-local field — `NLOCAL_INIT_STA`

When initial plastic strains exist (e.g. `/INISTA`), the non-local field must start in
equilibrium with the local field. `starter/source/materials/fail/nlocal_init_sta.F`
performs a **pseudo-dynamic convergence loop** (explicit cycles of the auxiliary problem
only, calling the `*FINT_REG_INI` starter variants) until
`‖DNL‖ < 5·10⁻⁴` with stagnation `< 10⁻⁸`, then resets `FNL`, `VNL`, `DNL` to zero,
leaving a converged `UNL`.

### 5.4 Domain decomposition and restart writing

During `ddsplit`:

* `starter/source/restart/ddsplit/c_front.F` / `w_front.F` build, for each SPMD domain,
  the boundary tables: for every boundary node shared with domain `P`, its number of
  non-local d.o.fs (`POSI(N+1)−POSI(N)`) is added to the send/receive counters, producing
  the per-domain `IAD_ELEM` / `FR_ELEM` and Rad2Rad lists.
* `starter/source/restart/ddsplit/write_nloc_struct.F` renumbers the structure **per
  domain** (local `NNOD_L`, `L_NLOC_L`, `INDX_L`, `POSI`, `IDXI_L`, local skyline tables
  with `PROCNE` set from the element owner) and writes to each domain restart file:
  an 8-integer header (`IMOD, NNOD, L_NLOC, NUMELS_NL, NUMELC_NL, NUMELTG_NL, NDDMAX,
  LCNE_NL`), the material parameter arrays, the index tables, the Parith/ON tables, and
  the state vectors (`MASS`, `MASS0`, `UNL`; `FNL/VNL/VNL_OLD/DNL` written as zero).

---

## 6. Lifecycle in the Engine

### 6.1 Restart I/O

* `engine/source/output/restart/read_nloc_struct.F` reads the header, allocates every
  array (notably `FNL`/`STIFNL` with `NTHREAD` columns in Parith/OFF, and
  `FSKY`/`STSKY` sized `ADDCNE(NNOD+1) × NDDMAX` in Parith/ON) and restores the state
  vectors.
* `engine/source/output/restart/write_nloc_struct.F` writes the same data back at each
  `/RFILE`, making the non-local state fully restartable.

### 6.2 Initialization — `RESOL_INIT`

`engine/source/engine/resol_init.F:1161` calls **`SPMD_SUB_BOUNDARIES`** (see §8.2) to
derive the non-local communication tables from the generic SPMD boundary tables
(`IAD_ELEM`/`FR_ELEM` of the mechanical problem).

### 6.3 Per-cycle workflow (`RESOL`)

Within each cycle of the explicit loop in `engine/source/engine/resol.F` (line numbers
from the current sources):

```
1. Element force loops (FORINT / FORINTC / FORINTP)
   └─ *FORC3 element routines
      ├─ interpolate nodal DNL to Gauss points → VARNL (e.g. CBAVARNL for shells)
      ├─ material law (MMAIN / MULAW / MULAWC / USERMAT_*)
      │    consumes VARNL (non-local increment), updates ELBUF PLANL/EPSDNL,
      │    returns VARNL = local variable to regularize             (§7.3)
      └─ *FINT_REG: non-local forces → FNL(:,ITASK)  (Parith/OFF)
                                     → FSKY/STSKY     (Parith/ON)

2. MPI exchange (Parith/ON only)                       resol.F:4491
   SPMD_EXCH_SUB_PON: exchange of boundary skyline slots FSKY/STSKY

3. Force assembly (OpenMP parallel)
   ├─ Parith/OFF: ASSPAR_SUB_POFF reduces FNL/STIFNL over threads
   │              (resol.F:4552, 4556) + NLOCAL_DTNODA (resol.F:4558)
   └─ Parith/ON : ASSPAR_SUB gathers FSKY→FNL(:,1), STSKY→STIFNL(:,1)
                  in deterministic order (resol.F:4817, 4821)
                  + NLOCAL_DTNODA (resol.F:4823)

4. Debug output (optional, /DEBUG/ACC)                 resol.F:4893
   SPMD_COLLECT_NLOCAL: gather FNL to proc 0 → *_NLOCAL_*.adb file

5. MPI exchange (Parith/OFF only)                      resol.F:6721
   SPMD_EXCH_SUB_POFF: sum FNL/STIFNL across domain boundaries

6. NLOCAL_ACC   (resol.F:6768)  FNL := FNL / MASS        (accelerations)
7. NLOCAL_VEL   (resol.F:8725)  VNL_OLD := VNL ; VNL += DT12·FNL ; FNL := 0
8. NLOCAL_INCR  (resol.F:8822)  DNL := DT2·VNL ; UNL += DNL
```

The assembly/integration helpers are:

| Routine | File | Action |
|---|---|---|
| `NLOCAL_ACC` | `engine/source/assembly/nlocal_acc.F` | Divides assembled forces by the non-local mass (acceleration stored in place in `FNL`). |
| `NLOCAL_VEL` | `engine/source/assembly/nlocal_vel.F` | Saves `VNL_OLD`, integrates `VNL += DT12·FNL`, resets `FNL` for the next cycle. |
| `NLOCAL_INCR` | `engine/source/assembly/nlocal_incr.F` | Computes the increment `DNL = DT2·VNL` and accumulates `UNL += DNL`. |
| `NLOCAL_DTNODA` | `engine/source/time_step/nlocal_dtnoda.F` | Nodal non-local time step `dt = DTFAC·CDAMP·√(2·MASS/STIFNL)`; min-reduces into `DT2T`; applies added mass on `MASS` under `/DT/.../CST`-type control (using `CSTA`); resets `STIFNL`. |

The Rad2Rad coupling path additionally calls `R2R_GETDATA`/`R2R_EXCHANGE`, which use
`SPMD_EXCH_R2R_NL` (§8.6) to keep the non-local field consistent across coupled models.

---

## 7. Non-local data inside `ELBUF_TAB`

**Yes** — non-local data is also stored in the element buffer structure. `NLOCAL_STR_`
holds the **nodal** view of the auxiliary problem; `ELBUF_TAB`
(`common_source/modules/mat_elem/elbufdef_mod.F90`) holds the **integration-point** view
used by the material laws, the failure criteria and the output. There are three distinct
pieces:

### 7.1 Regularized plastic strain and strain rate per integration point

* Activation flags in the global (element-mean) buffer `G_BUFEL_`:
  `G_PLANL`, `G_EPSDNL` (`elbufdef_mod.F90:792–793`) and corresponding `GBUF%PLANL`
  array (element-mean value, used by output routines).
* Per-layer lengths in `BUF_LAY_`: `L_PLANL`, `L_EPSDNL`
  (`elbufdef_mod.F90:1257–1258`).
* Integration-point arrays in `L_BUFEL_` (`elbufdef_mod.F90:1068–1069`):

  | Field | Dimension | Content |
  |---|---|---|
  | `LBUF%PLANL` | `NEL·L_PLANL` | **Non-local cumulated plastic strain** at the integration point (the regularized counterpart of `LBUF%PLA`). |
  | `LBUF%EPSDNL` | `NEL·L_EPSDNL` | **Non-local plastic strain rate** at the integration point. |

* These arrays are allocated only when the `/NONLOCAL` card set the `MLAW_TAG` flags
  (§5.1): Starter `starter/source/elements/elbuf_init/allocbuf_auto.F:265–267`, Engine
  restart `engine/source/elements/elbuf/allocbuf_auto.F:755–759`.
* They are written to animation/H3D/time-history output
  (`h3d_shell_scalar_1.F`, `h3d_solid_scalar_1.F`, `dfuncc.F`, `dfunc6.F`, `thcoq.F`,
  `thsol.F`), which makes the regularized field directly post-processable.

### 7.2 Through-thickness non-local buffers for shells / thick shells

For layered elements, part of the regularization operates **through the thickness** and is
stored per element group inside `ELBUF_TAB` (`elbufdef_mod.F90:1081–1093, 1325–1326`):

```fortran
TYPE BUF_NLOC_                       ! shells     — ELBUF_TAB(NG)%NLOC(IR,IS)
  MASSTH(:,:)   ! non-local nodal masses through the thickness
  UNLTH(:,:)    ! non-local cumulated variable
  VNLTH(:,:)    ! non-local velocities
  FNLTH(:,:)    ! non-local forces
END TYPE
TYPE BUF_NLOCTS_                     ! thick shells — ELBUF_TAB(NG)%NLOCTS(IR,IS)
  ... same fields ...
END TYPE
```

They are allocated in `starter/source/elements/elbuf_init/elbuf_ini.F`
(lines 1237 for shells, 568 for thick shells), initialized with the thickness-integrated
masses by `NLOC_DMG_INIT` (`nloc_dmg_init.F:1005, 1057`), split per domain by
`starter/source/restart/ddsplit/w_elbuf_str.F:890, 916`, and used by the in-plane/
through-thickness regularization routines (`cbafint_reg*.F`, `cfint_reg*.F`,
`cdkfint_reg*.F`, `c3fint_reg*.F`, `scfint_reg.F`, ...).

### 7.3 Data flow between `ELBUF_TAB` and `NLOCAL_STR_`

The bridge is the per-group working array **`VARNL(NEL)`**, with a two-way convention
(reference: `engine/source/materials/mat_share/mulaw.F90`, group flag
`INLOC = IPARG(78,NG)`):

1. **Nodal → integration point** (before the constitutive update): the element routine
   interpolates the nodal increment `NLOC_DMG%DNL` to the Gauss points into `VARNL`
   (e.g. `cbavarnl.F:79–101` for shells). On entry of `MULAW`, `VARNL` is clamped to ≥ 0
   and stored in the element buffer (`mulaw.F90:586–591`):

   ```fortran
   lbuf%planl(i)  = lbuf%planl(i) + varnl(i)     ! cumulated non-local plastic strain
   lbuf%epsdnl(i) = varnl(i)/max(dt1,em20)       ! non-local strain rate
   ```

   The material/failure routines (e.g. `sigeps104`, Gurson) then drive **damage growth
   with the non-local values** (`PLANL`, `EPSDNL`, `DPLA_NL = VARNL`) instead of the local
   plastic strain — this is the regularization itself.
2. **Integration point → nodal** (after the constitutive update): `VARNL` is overwritten
   with the **local** variable to regularize (the plastic strain `DEFP`, zero for deleted
   elements; `mulaw.F90:2879–2890`). It becomes the source term `VAR_REG` of the
   `*FINT_REG` routine (§4), closing the loop:

```
NLOC_DMG%DNL ──interpolate──► VARNL ──► LBUF%PLANL / %EPSDNL ──► damage law
                                            (ELBUF_TAB)
local plastic strain ◄── material law ──► VARNL(out) ──► *FINT_REG ──► NLOC_DMG%FNL
```

### 7.4 Element deletion (`OFF` flag) in non-local damage

Element deletion in the non-local damage context uses the **same `OFF` flag** as standard
element erosion (`OFF = 1` alive, `0 < OFF < 1` ramping down, `OFF = 0` fully deleted),
but the failure threshold is evaluated against the **non-local** plastic strain instead of
the local one.

**Where `OFF` is set — step by step:**

1. **Pointer switch** (`mulaw.F90:2243`, `mmain.F90:2278`): when `INLOC = IPARG(78,NG) > 0`,
   the pointer `el_pla` is redirected:

   ```fortran
   if (inloc > 0) then
     el_pla => lbuf%planl(1:nel)   ! non-local cumulated plastic strain
   else
     el_pla => defp(1:nel)          ! local cumulated plastic strain
   end if
   ```

2. **Failure criterion dispatch** (`mulaw.F90` / `mmain.F90`, `do ir = 1, nfail` loop):
   the `/FAIL` routines (`fail_johnson`, `fail_tab2_s`, `fail_inievo_s`, etc.) are called
   with `el_pla` pointing to `LBUF%PLANL`. When `el_pla(I)` exceeds the failure threshold,
   the routine sets:

   ```fortran
   OFF(I) = FOUR_OVER_5   ! = 0.8, first step of the deletion sequence
   ```

   The failure routines themselves are **identical** whether non-local or local — only
   their `el_pla` input changes.

3. **Source zeroing** (`mulaw.F90:2879–2890`, `mmain.F90:2817–2827`): immediately after
   the failure loop, elements that have started failing stop contributing to the
   non-local source term:

   ```fortran
   if (off(i) == one) then
     varnl(i) = defp(i)   ! alive: local plastic strain drives the non-local field
   else
     varnl(i) = zero       ! failing/dead: no new source (absorbing BC in *FINT_REG)
   end if
   ```

4. **`GBUF%OFF` synchronization** (`smallb3.F:69–78`): at the end of each element
   kernel, `GBUF%OFF(I)` (the persistent element-buffer flag) is synchronized with the
   local `OFF(I)`. When `OFF(I) == ZERO`, `GBUF%OFF(I) = ZERO` and the element is fully
   removed from all subsequent computations.

**Summary**: `OFF` is **first set in the `/FAIL` criterion routines** (not in the material
law itself). The non-local specificity is entirely in the `el_pla` pointer swap at
`mulaw.F90:2243`. See §4 for how `*FINT_REG` handles already-deleted elements
(`OFF ≠ 1`) with an absorbing boundary force instead of a source term.

---

## 8. Dedicated MPI (SPMD) communications

All MPI traffic specific to the non-local problem goes through four routines. The
non-local d.o.fs being purely nodal, the exchanges mirror the mechanical force
communications, but on the compact non-local numbering.

### 8.1 Overview

| Routine | File | When | What |
|---|---|---|---|
| `SPMD_SUB_BOUNDARIES` | `engine/source/mpi/spmd_exch_sub.F:31` | Once, at `RESOL_INIT` | Builds the non-local boundary tables. |
| `SPMD_EXCH_SUB_POFF` | `engine/source/mpi/spmd_exch_sub.F:230` | Every cycle, Parith/OFF | Sums `FNL` (+`STIFNL`) at domain boundaries. |
| `SPMD_EXCH_SUB_PON` | `engine/source/mpi/spmd_exch_sub.F:394` | Every cycle, Parith/ON | Exchanges boundary **skyline slots** `FSKY` (+`STSKY`). |
| `SPMD_COLLECT_NLOCAL` | `engine/source/mpi/output/spmd_collect_nlocal.F` | On `/DEBUG/ACC` output cycles | Gathers the non-local field to proc 0 for `.adb` debug files. |
| `SPMD_EXCH_R2R_NL` | `engine/source/mpi/r2r/spmd_exch_r2r_nl.F` | Rad2Rad coupling | Exchanges `FNL` at the coupling interface. |

### 8.2 `SPMD_SUB_BOUNDARIES` — communication table construction

Called from `resol_init.F:1161`. From the *mechanical* boundary tables
(`IAD_ELEM`/`FR_ELEM` of the global problem) it extracts the subset of nodes carrying
non-local d.o.fs (via `IDXI`) and builds:

* the Parith/OFF tables `NLOC_DMG%IAD_ELEM`, `IAD_SIZE` (buffer sizes accounting for
  `NDDL` d.o.fs per node, doubled when stiffness is exchanged) and `FR_ELEM`
  (`spmd_exch_sub.F:63–135`);
* in Parith/ON (`spmd_exch_sub.F:139–217`), for each boundary non-local node it scans the
  attached elements through `ADDCNE`/`PROCNE` and classifies each skyline slot:
  * slot of a **locally owned** element → entry in `ISENDSP`/`FR_ELEM_S` (its value must
    be sent to the other domains sharing the node),
  * slot of an element **owned by domain P** → entry in `IRECSP`/`FR_ELEM_R` (its value
    will be received from `P`),

  filling `IADSDP`, `IADRCP` and the counters `FR_NBCC`.

### 8.3 `SPMD_EXCH_SUB_POFF` — Parith/OFF boundary reduction

Called from `resol.F:6721`, after the thread assembly and before `NLOCAL_ACC`.
Classical non-blocking neighbor exchange:

1. post `MPI_IRECV` for every domain with a non-empty buffer (sizes from `IAD_SIZE`);
2. pack, per remote domain, for each boundary node `NN` of `FR_ELEM` and each of its
   `NDDL` d.o.fs: `FNL(NPOS+L-1,1)` and, when `NODADT > 0`, `STIFNL(NPOS+L-1,1)`;
3. `MPI_ISEND`;
4. on each `MPI_WAIT`-ed receive, **accumulate**:
   `FNL += recv`, `STIFNL += recv` — the boundary d.o.f thus obtains the complete sum of
   its element contributions from all domains;
5. wait for the sends.

This is a *reduction* (sum) — the result depends on message arrival order at machine
precision, which is exactly what Parith/ON forbids, hence the second routine.

### 8.4 `SPMD_EXCH_SUB_PON` — Parith/ON skyline exchange

Called from `resol.F:4491`, **before** the deterministic gather `ASSPAR_SUB`. Instead of
exchanging sums, each domain exchanges the **individual element contributions**:

1. `MPI_IRECV` of `FR_NBCC(2,P)` values per neighbor `P`;
2. pack: for each entry `J` of `FR_ELEM_S`, the slot `FSKY(ISENDSP(J),L)` (and
   `STSKY(...)` when `NODADT > 0`) for all `NDDL` d.o.fs of the node;
3. `MPI_ISEND` of `FR_NBCC(1,P)` values;
4. unpack: received values are **written** (not summed) into the reserved remote slots
   `FSKY(IRECSP(J),L)` / `STSKY(IRECSP(J),L)`.

After the exchange, every domain owns a complete skyline for its boundary nodes, and
`ASSPAR_SUB` (`resol.F:4817`) sums the slots **in a fixed, decomposition-independent
order**, guaranteeing bitwise reproducibility of the non-local forces — same design as the
mechanical Parith/ON assembly.

### 8.5 `SPMD_COLLECT_NLOCAL` — debug/output gathering

Called from `resol.F:4893` when `/DEBUG/ACC` is active and the model is non-local. Every
processor sends to processor 0 the pairs (global node number from `NODGLOB(INDX(I))`,
first-d.o.f value of `FNL`); processor 0 merges them into a `NUMNODG`-long array and dumps
a `ROOTNAME_NLOCAL_<run>_<cycle>.adb` text file (one line per node, value printed in
hexadecimal for exact comparison between runs/decompositions).

### 8.6 `SPMD_EXCH_R2R_NL` — Rad2Rad coupling

`engine/source/mpi/r2r/spmd_exch_r2r_nl.F`, used by the multi-domain (Rad2Rad)
coupling. The exchange buffers carry, in addition to the standard kinematic quantities
(accelerations, masses/inertias), the **first non-local d.o.f of every interface node**
(`FNL(POSI(IDXI(NOD)))`), so that damage regularization stays consistent across the
boundary between coupled Radioss models. The interface node lists are sized during the
Starter decomposition (`c_front.F`: the Rad2Rad element list is enlarged from 2 to 4
blocks when `NLOC_DMG%IMOD > 0`).

### 8.7 Parith/OFF vs Parith/ON summary

| Aspect | Parith/OFF (`IPARIT = 0`) | Parith/ON (`IPARIT ≠ 0`) |
|---|---|---|
| Element assembly target | `FNL(:,ITASK)`, `STIFNL(:,ITASK)` per thread | Skyline `FSKY`, `STSKY`, one slot per element-node incidence |
| Thread reduction | `ASSPAR_SUB_POFF` (`resol.F:4552`) | deterministic gather `ASSPAR_SUB` (`resol.F:4817`) |
| MPI routine | `SPMD_EXCH_SUB_POFF` (`resol.F:6721`) | `SPMD_EXCH_SUB_PON` (`resol.F:4491`) |
| MPI position | after assembly | **before** assembly |
| MPI semantics | sum of nodal values | copy of individual element slots |
| Reproducibility | not bitwise | bitwise, independent of `NSPMD`/`NTHREAD` |

---

## 9. Time-step interaction

The auxiliary problem is integrated with the **same Δt** as the mechanics, so its
stability constrains the global time step:

* **Element time step** (`NODADT = 0`): computed in the `*FINT_REG` routines (§4) and
  min-reduced into `DT2T` with safety `CDAMP = 0.7`.
* **Nodal time step** (`NODADT > 0`): `NLOCAL_DTNODA` computes
  `dt = DTFAC·CDAMP·√(2·MASS/STIFNL)` per non-local d.o.f; under constant-time-step
  options it adds mass to the non-local d.o.fs (bounded by `CSTA = 40`) instead of
  reducing Δt.

The artificial density `DENS` is calibrated in the Starter (§5.2) precisely so that this
non-local time step stays above the mechanical one in the target mesh-size range
(`Le ≤ LE_MAX`).

---

## 10. Key file index

| Area | Files |
|---|---|
| Type definition, constants | `common_source/modules/nlocal_reg_mod.F` |
| Starter input | `starter/source/materials/nonlocal/hm_read_nonlocal.F`, `starter/source/materials/fail/gurson/hm_read_fail_gurson.F`, `starter/source/materials/mat/mat104/law104_upd.F` |
| Starter construction | `starter/source/materials/fail/nloc_dmg_init.F`, `starter/source/materials/fail/nlocal_init_sta.F` |
| Domain decomposition / restart write | `starter/source/restart/ddsplit/write_nloc_struct.F`, `c_front.F`, `w_front.F`, `w_elbuf_str.F` |
| Engine restart | `engine/source/output/restart/read_nloc_struct.F`, `write_nloc_struct.F` |
| Element regularization | `engine/source/elements/**/ *fint_reg*.F`, `engine/source/elements/shell/coqueba/cbavarnl.F` |
| Material bridge | `engine/source/materials/mat_share/mulaw.F90`, `mulawc.F90`, `mmain.F90`, `usermat_shell.F`, `usermat_solid.F` |
| Time integration of the non-local field | `engine/source/assembly/nlocal_acc.F`, `nlocal_vel.F`, `nlocal_incr.F`, `engine/source/time_step/nlocal_dtnoda.F` |
| MPI | `engine/source/mpi/spmd_exch_sub.F`, `engine/source/mpi/output/spmd_collect_nlocal.F`, `engine/source/mpi/r2r/spmd_exch_r2r_nl.F` |
| Main loop | `engine/source/engine/resol.F`, `resol_init.F` |
| ELBUF definitions | `common_source/modules/mat_elem/elbufdef_mod.F90` (`PLANL`, `EPSDNL`, `BUF_NLOC_`, `BUF_NLOCTS_`), `starter/source/elements/elbuf_init/elbuf_ini.F`, `allocbuf_auto.F` |
| Node splitting | `engine/source/engine/node_spliting/detach_node.F90`, `engine/source/engine/node_spliting/detach_node_nloc.F90`, `engine/source/engine/node_spliting/nloc_shell_detach.F90` |

---

## 11. Array Size Quick Reference

```
NNOD        : number of non-local nodes  (subset of all nodes)
L_NLOC      : sum of NDDL over all non-local nodes
NDDL(n)     : through-thickness DOF count of node n (= NPTT for shells, 1 for solids)
NDDMAX      : max(NDDL)

POSI        : size NNOD+1          (CSR offsets into L_NLOC space)
INDX        : size NNOD            (non-local rank → node local id)
IDXI        : size NUMNOD          (node local id → non-local rank, 0 if not NL)

MASS, MASS0, UNL, VNL, VNL_OLD, DNL : size L_NLOC
FNL, STIFNL : size L_NLOC × NTHREAD

ADDCNE      : size NNOD+2          (skyline CSR offsets, PARITH/ON only)
FSKY, STSKY : size ADDCNE(NNOD+1) × NDDMAX
IADC        : size 4 × NUMELC_NL   (quad-shell back-pointers)
IADS        : size 8 × NUMELS_NL   (solid back-pointers)
IADTG       : size 3 × NUMELTG_NL  (triangle-shell back-pointers)
```

---

## 12. Node Splitting Support

When a fracture propagates through a mesh via node-splitting, the `NLOCAL_STR_`
structure must be extended to accommodate the new (duplicated) node and its non-local
degrees of freedom.

### 12.1 What is already handled at the split

`update_pon_shells` (called from `detach_node_from_shells`) handles the **mechanical
PARITH/ON skyline** stored in `elements%pon` (`parith_on_mod.F90`):

| Updated field | Type |
|---|---|
| `elements%pon%adsky` | CSR row offsets for the mechanical force skyline |
| `elements%pon%iadc` | Per-shell FSKY row indices (mechanical) |
| `elements%pon%fsky` | Mechanical skyline force buffer |

### 12.2 What `detach_node_nloc` must additionally handle

The **non-local damage structure `NLOCAL_STR_`** (`NLOC_DMG`) has its own independent
skyline and DOF arrays — completely separate from `elements%pon` and not touched by the
existing mechanical node-splitting code. The following tables summarise every field that
must be patched.

**Index tables (change on split):**

| Array | Dim | Meaning | Change on split |
|-------|-----|---------|----------------|
| `INDX` | `(NNOD)` | Non-local rank → local node id | Append new local id |
| `POSI` | `(NNOD+1)` | CSR DOF offsets | Append new entry; last entry = new `L_NLOC` |
| `IDXI` | `(NUMNOD)` | Local node id → non-local rank | Extend by 1; set `IDXI(new_id) = new_NNOD` |

**DOF-space state vectors (all sized `L_NLOC` or `L_NLOC × NTHREAD`):**

| Array | Meaning | Initialise new DOFs |
|-------|---------|---------------------|
| `MASS` / `MASS0` | Non-local mass (current / reference) | Copy from parent, then split both 50/50 |
| `VNL`, `VNL_OLD`, `DNL`, `UNL` | Velocity, previous velocity, increment, cumulated variable | Copy from parent |
| `FNL`, `STIFNL` | Force / stiffness accumulator | Zero |

**Skyline connectivity (PARITH/ON only):**

| Array | Meaning | Change on split |
|-------|---------|----------------|
| `ADDCNE` | CSR row offsets into FSKY for each non-local node | Extend by 1 for new node |
| `PROCNE` | MPI rank of each element contribution | Append for new contributions |
| `FSKY` / `STSKY` | Skyline force / stiffness accumulator | Extend; new entries zero |
| `IADC` | Per-shell FSKY row indices | Update for shells in `shell_list` |

### 12.3 Integration in `resol.F` — activating node splitting (~line 5236)

The node-splitting block in `resol.F` calls, in order:

1. `test_jc_shell_detach` — detect and perform splits (calls `detach_node` → `detach_node_nloc`)
2. `INIT_NODAL_STATE` — re-initialise nodal boundary/state arrays for the new node
3. `CHKINIT` (gated on ALE/deletion flags) — rebuild element-to-node connectivity skyline
4. `ALLOCATE_OUTPUT_DATA` — extend output arrays for the new node
5. `SPMD_SUB_BOUNDARIES(NLOC_DMG, NODES%BOUNDARY_ADD, NODES%BOUNDARY)` (gated on
   `NSPMD > 1`) — rebuild the non-local MPI boundary tables after the split

The post-processing block in `resol.F` (~line 5285) now contains:

```fortran
! Rebuild non-local SPMD boundary arrays (ISENDSP, IRECSP, etc.)
! after the skyline topology changed due to node split.
IF (NSPMD > 1 .AND. NLOC_DMG%IMOD > 0) THEN
  CALL SPMD_SUB_BOUNDARIES(NLOC_DMG, NODES%BOUNDARY_ADD, NODES%BOUNDARY)
ENDIF
```

This rebuilds `NLOC_DMG%IAD_ELEM`, `NLOC_DMG%IAD_SIZE`, and `NLOC_DMG%FR_ELEM` from
the updated nodal boundary tables, so the non-local MPI state stays valid after the split.

### 12.4 `detach_node_nloc` — step-by-step implementation

File: `engine/source/engine/node_spliting/detach_node_nloc.F90`.
Called from `detach_node` immediately after `detach_node_from_shells`, before
`nodes%numnod` is incremented.

**Signature:**

```fortran
subroutine detach_node_nloc(nloc_dmg, old_local_id, new_local_id, &
                             shell_list, list_size)
  use nlocal_reg_mod
  use my_alloc_mod
  use extend_array_mod
  implicit none
  type(nlocal_str_), intent(inout) :: nloc_dmg
  integer,           intent(in)    :: old_local_id   ! local id of the split node
  integer,           intent(in)    :: new_local_id   ! local id of the new node (= old numnod + 1)
  integer,           intent(in)    :: list_size
  integer,           intent(in)    :: shell_list(list_size)
```

**Step-by-step logic:**

```
Step 1 — Early exit
  nl_idx = nloc_dmg%idxi(old_local_id)
  if (nl_idx == 0) return   ! parent is not a non-local node

Step 2 — DOF layout of the parent
  old_pos = nloc_dmg%posi(nl_idx)
  nddl    = nloc_dmg%posi(nl_idx+1) - old_pos   ! DOF count

Step 3 — Extend IDXI (size NUMNOD → NUMNOD+1)
  call extend_array(nloc_dmg%idxi, old_numnod, new_local_id)
  nloc_dmg%idxi(new_local_id) = nloc_dmg%nnod + 1

Step 4 — Extend INDX (size NNOD → NNOD+1)
  call extend_array(nloc_dmg%indx, nloc_dmg%nnod, nloc_dmg%nnod+1)
  nloc_dmg%indx(nloc_dmg%nnod+1) = new_local_id

Step 5 — Extend POSI (size NNOD+1 → NNOD+2)
  call extend_array(nloc_dmg%posi, nloc_dmg%nnod+1, nloc_dmg%nnod+2)
  nloc_dmg%posi(nloc_dmg%nnod+2) = nloc_dmg%l_nloc + nddl + 1

Step 6 — Extend DOF-space vectors by nddl
  new_pos = nloc_dmg%l_nloc + 1
  For each of MASS, MASS0, VNL, VNL_OLD, DNL, UNL:
    call extend_array(array, nloc_dmg%l_nloc, nloc_dmg%l_nloc + nddl)
    array(new_pos : new_pos+nddl-1) = array(old_pos : old_pos+nddl-1)
  ! Split mass equally (conserves total non-local mass)
  nloc_dmg%mass (new_pos:new_pos+nddl-1) = nloc_dmg%mass (old_pos:old_pos+nddl-1) * HALF
  nloc_dmg%mass0(new_pos:new_pos+nddl-1) = nloc_dmg%mass0(old_pos:old_pos+nddl-1) * HALF
  nloc_dmg%mass (old_pos:old_pos+nddl-1) = nloc_dmg%mass (old_pos:old_pos+nddl-1) * HALF
  nloc_dmg%mass0(old_pos:old_pos+nddl-1) = nloc_dmg%mass0(old_pos:old_pos+nddl-1) * HALF
  For FNL (l_nloc, nthread) and STIFNL (l_nloc, nthread):
    call extend_array_2d(array, nloc_dmg%l_nloc, nthread, nloc_dmg%l_nloc+nddl, nthread)
    array(new_pos:new_pos+nddl-1, 1:nthread) = ZERO

Step 7 — Commit scalar counters
  nloc_dmg%nnod   = nloc_dmg%nnod + 1
  nloc_dmg%l_nloc = nloc_dmg%l_nloc + nddl

Step 8 — Skyline update (PARITH/ON only, if ADDCNE is allocated)
  new_nnod = nloc_dmg%nnod   ! already incremented
  Count n_contrib = number of (shell, corner) pairs in shell_list where the
  corner was old_local_id (now replaced by new_local_id in element connectivity).

  8a — Extend ADDCNE by 1:
    call extend_array(nloc_dmg%addcne, new_nnod, new_nnod+1)
    nloc_dmg%addcne(new_nnod+1) = nloc_dmg%addcne(new_nnod) + n_contrib

  8b — Extend PROCNE by n_contrib and append entries:
    old_lcne = nloc_dmg%lcne_nl
    call extend_array(nloc_dmg%procne, old_lcne, old_lcne + n_contrib)
    k = old_lcne + 1
    do i = 1, list_size
      shell_id = shell_list(i)
      do j = 1, 4
        if elements%shell%nodes(j, shell_id) == new_local_id:
          nloc_dmg%procne(k) = ISPMD   ! local domain rank
          k = k + 1
      end do
    end do
    nloc_dmg%lcne_nl = old_lcne + n_contrib
    ! NOTE: CNE is always size 0 and unused; do NOT extend it.

  8c — Extend FSKY and STSKY rows by n_contrib, zeroing new entries:
    old_fsky_rows = nloc_dmg%addcne(new_nnod)
    new_fsky_rows = nloc_dmg%addcne(new_nnod+1)
    call extend_array_2d(nloc_dmg%fsky,  old_fsky_rows, nddmax, new_fsky_rows, nddmax)
    call extend_array_2d(nloc_dmg%stsky, old_fsky_rows, nddmax, new_fsky_rows, nddmax)
    nloc_dmg%fsky (old_fsky_rows+1:new_fsky_rows, 1:nddmax) = ZERO
    nloc_dmg%stsky(old_fsky_rows+1:new_fsky_rows, 1:nddmax) = ZERO

  8d — Update IADC back-pointers for shells in shell_list:
    For each shell_id in shell_list, for each corner j = 1..4:
      if elements%shell%nodes(j, shell_id) == new_local_id:
        nloc_dmg%iadc(j, shell_id) = <new FSKY row assigned in 8c>
    The assignment must use the same ordering as used when filling PROCNE in 8b.
```

### 12.5 SPMD boundary arrays after a split

The arrays `ISENDSP`, `IRECSP`, `IADSDP`, `IADRCP`, `FR_NBCC`, `FR_ELEM_S`,
`FR_ELEM_R` are built by `SPMD_SUB_BOUNDARIES` / `SPMD_EXCH_SUB_PON` from `ADDCNE`,
`PROCNE`, `IAD_ELEM`, `FR_ELEM`, and `POSI`. They must be **fully rebuilt** after the
split because the send/receive patterns depend on the global topology:

```fortran
if (nloc_dmg%imod > 0 .and. nspmd > 1) then
  call spmd_sub_boundaries(nloc_dmg, nodes%boundary_add, nodes%boundary)
end if
```

This call is placed in `resol.F` alongside the mechanical PON rebuild that happens after
all node splits in a cycle (see commented block near line 5238). Localised patching is
not feasible because the patterns depend on the global non-local node layout across all
MPI domains.

### 12.6 Restart compatibility after a split

`write_nloc_struct.F` and `read_nloc_struct.F` write/read `NNOD`, `L_NLOC`, `LCNE_NL`,
`NUMELS_NL`, `NUMELC_NL`, `NUMELTG_NL`, `NDDMAX` as a header, then all arrays
sequentially. After a node split the sizes change; no extra work is needed beyond ensuring
the write happens after the split update so that the checkpoint captures the new state.

---

## 13. Deriving Crack Node and Crack Direction from `UNL`

The non-local field `UNL` is exactly the information needed to drive node-splitting: the
**highest-damage node** and **the orientation of the damage gradient** (perpendicular to
the crack plane) are both directly accessible from `NLOC_DMG`.

### 13.1 Identifying the crack node

For each structural node `i` belonging to non-local elements:

```fortran
nl_idx = nloc_dmg%idxi(i)
if (nl_idx == 0) cycle          ! node not non-local
pos  = nloc_dmg%posi(nl_idx)
nddl = nloc_dmg%posi(nl_idx+1) - pos
unl_node = maxval(nloc_dmg%unl(pos : pos+nddl-1))
```

`unl_node` is the peak regularised strain at node `i` across the thickness.  The node
to split is the one maximising `unl_node` among all candidate nodes (e.g. nodes shared
by at least two heavily loaded elements), subject to the condition that the value exceeds
the failure threshold of the material.

For shells with `NDDL > 1`, the through-thickness DOF index `k` that achieves the
maximum indicates **which layer** is most damaged (useful for progressive delamination
detection).

### 13.2 Deriving the crack direction

The implicit-gradient regularisation means that `UNL` is smooth. The **crack plane
normal** is parallel to `∇UNL` evaluated at the crack node: the crack grows
perpendicular to the direction of maximum gradient.

**Practical computation** using the nodal values already stored in `UNL`:

For a quad shell, the gradient within element `e` at its centroid is:

```
∇UNL_e ≈ (1/A_e) * SUM_{corner i} [UNL_i * outward_normal_of_opposite_edge * edge_length_i]
```

or equivalently using the `BTB` geometry matrices already computed inside `cfint_reg`:

```
grad_x_e(k) = (1/V_e) * (BTB11 * (UNL_1 - UNL_3) + BTB12 * (UNL_2 - UNL_4))
grad_y_e(k) = (1/V_e) * (BTB12 * (UNL_1 - UNL_3) + BTB22 * (UNL_2 - UNL_4))
```

The **crack normal** at the target node is the weighted average of `∇UNL_e` over the
elements sharing that node:

```
n_crack = NORMALIZE( SUM_e [ vol_e * ∇UNL_e ] )
```

The **crack plane** passes through the node and is perpendicular to `n_crack`.  The
**edge to split** between two adjacent nodes `A` and `B` is the one most parallel to
`n_crack` (i.e. maximising `|dot(AB, n_crack)|`).

### 13.3 Implementation sketch

```fortran
! 1. Find the highest-damage non-local node
unl_max = 0.0_wp
split_node = 0
do i = 1, numnod
  nl_idx = nloc_dmg%idxi(i)
  if (nl_idx == 0) cycle
  pos  = nloc_dmg%posi(nl_idx)
  nddl = nloc_dmg%posi(nl_idx+1) - pos
  val  = maxval(nloc_dmg%unl(pos:pos+nddl-1))
  if (val > unl_threshold .and. val > unl_max) then
    unl_max    = val
    split_node = i
  end if
end do

! 2. Compute ∇UNL at split_node from attached element DOFs
crack_normal = 0.0_wp
total_weight = 0.0_wp
do ie = 1, n_elems_attached_to_split_node
  e = attached_elem(ie)
  do j = 1, 4
    inod  = ixc(j+1, e)
    nl_j  = nloc_dmg%idxi(inod)
    pos_j = nloc_dmg%posi(nl_j)
    unl_corner(j) = nloc_dmg%unl(pos_j)   ! DOF k=1 for simplicity
  end do
  grad(1) = (unl_corner(1) - unl_corner(3)) / dx_e
  grad(2) = (unl_corner(2) - unl_corner(4)) / dy_e
  weight   = element_area(e)
  crack_normal = crack_normal + weight * grad
  total_weight = total_weight + weight
end do
crack_normal = crack_normal / total_weight
crack_normal = crack_normal / norm2(crack_normal)   ! unit normal

! 3. Pick the element edge most aligned with the crack plane (perpendicular to n_crack)
!    to guide which elements go to the new node
```

### 13.4 Relationship with `nloc_shell_detach`

The current implementation in `nloc_shell_detach.F90` already uses the **non-local
field** `UNL` directly to build the shell damage indicator, instead of the older
`dammx` / `OFF` path:

| Criterion | Source | When available |
|---|---|---|
| `dammx` / `OFF` | Local failure flag in element buffer | After failure criterion is met (post-peak) |
| `UNL` | Non-local regularised variable | From the onset of inelastic strains |

This means the split trigger is now aligned with the regularised damage field, so node
splitting can happen **before** the element actually fails. That better captures the
damage-band geometry and avoids relying only on element deletion.

What is still not derived from `UNL` is the **crack direction / split plane normal**: the
current implementation still uses the shell-geometry eccentricity heuristic to choose
which corner to detach. Section 13.2 above describes how to use `∇UNL` instead.

---

## 14. Known Limitations / Open Points

| Scenario | Status |
|---|---|
| Single domain (`NSPMD = 1`) | ✅ Fully functional — all MPI-gated calls are skipped |
| Multi-domain, PARITH/OFF | ✅ Boundary tables are rebuilt after split |
| Multi-domain, PARITH/ON | ✅ Boundary tables are rebuilt after split |

### Individual issues

1. **Mass splitting is 50/50.**  `detach_node_nloc` splits the non-local mass equally
   between the parent and the new node.  A more physically consistent approach would
   weight the split by the volume fraction of elements remaining attached to each node
   after the split.

2. **`CNE` array is never extended.**  `CNE` (element connectivity in skyline format) is
   always kept at size 0.  If any future code path reads `CNE` for the non-local nodes
   added by splitting, it will access out-of-bounds memory.

3. **Only quad-shell elements are split.**  `detach_node_nloc` handles the `IADC`
   back-pointer update for quad shells only.  Support for triangle shells (`IADTG`) and
   solids (`IADS`) needs to be added when node splitting is extended to those element
   types.

4. **Crack direction is not yet derived from `UNL`.**  The current `nloc_shell_detach`
   implementation uses `UNL` to trigger splitting, but it still selects the corner to
   detach from shell geometry rather than from a gradient of `UNL`.  Section 13 above
   describes how to use `∇UNL` instead.
