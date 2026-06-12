# Engine Main Time Loop — Call Tree and Cycle Structure

This document describes the engine's startup sequence, the structure of the main
explicit time-integration loop inside `resol.F`, and the order of major operations
within a single time cycle. It is primarily useful for debugging element behaviour,
understanding where contact forces are computed, tracing output triggers, and
following the MPI/OpenMP parallel flow.

---

## 1. Purpose

OpenRadioss is an explicit finite-element solver (central-difference / leapfrog
scheme). The engine reads a restart file written by the Starter and then drives a
cycle loop over time cycles until termination. The loop computes:

1. Internal element forces (materials, EOS, failure)
2. External forces (loads, contacts, kinematic conditions, rigid bodies)
3. MPI force assembly across domain boundaries
4. Nodal accelerations → velocities → coordinates (leapfrog)
5. Time-step estimation
6. Output (animation, time-history, restart)

---

## 2. Binary startup chain

```
engine_linux64_gf        (main program, engine/source/engine/radioss.F, line 36)
  └─ RADIOSS0()
       └─ RADIOSS2(IDATA, MIDATA, RDATA, MRDATA)   (engine/source/engine/radioss2.F, line 146)
            ├─ INI_MSG / BUILD_MSG                 message initialisation
            ├─ INICONSTANT                         physical constants
            ├─ INICOD / COQINI                     code flags
            ├─ INIPAR(…, 1, …)                     first parameter setup
            ├─ LECINP / LECSTAT / LECTUR           read engine input (engine/source/input/)
            │     └─ reads *.rad (engine commands) + restart file (*_NNNN.rst)
            │        rebuilds ELBUF_TAB, IPARG, IXS/IXC/IXP/…, NODES
            ├─ RDRESA                              read restart state
            ├─ INIPAR(…, 3, …)                     third parameter setup (post-read)
            ├─ INIRESA                             initialise result buffers
            └─ RESOL(TIMERS, ELEMENT, NODES, …)   TIME LOOP  (engine/source/engine/resol.F, line 579)
```

`radioss.F` has only 40 lines; it calls `RADIOSS0` which simply calls `RADIOSS2`.
All real work is in `RADIOSS2` and `RESOL`.

---

## 3. `RESOL` — the main time loop

`RESOL` (`engine/source/engine/resol.F`, declared at line 579) is a single large
subroutine. After one-time initialisation it enters a cycle loop (one pass = one
time step). The OpenMP region is entered and exited **within** the cycle loop
using `!$OMP PARALLEL` / `!$OMP END PARALLEL` blocks.

### 3.1 One-time initialisation (before cycle loop)

| Step | Key calls | Lines (approx) |
|------|-----------|----------------|
| SMP thread setup | `SMP_INIT` | ~1628 |
| Per-cycle init arrays | `RESOL_INIT` | ~1633 |
| RESOL header (print) | `RESOL_HEAD` | ~2216 |
| AMS setup (if `/DT/AMS`) | `SMS_INI_JAD_*`, `SMS_INI_KIN_2` | ~2100–2247 |

### 3.2 Per-cycle sequence

Each cycle executes in this order. Line numbers are approximate (the file is ~10000 lines).

#### Step 0 — Sensor/activation/external forces (serial, line ~2700–2960)
- Gather shell thickness for interface checking (`THICKVAR`)
- ALE/Euler function table update (`TIMFUN`)
- Sensor evaluation (`STOP_SENSOR`)
- Element activation/deactivation (`DESACTI`)
- External concentrated loads (`FORCE`)

#### Step 1 — Interface sort and pre-force prep (serial, ~line 2780–3465)
- Interface TYPE 14/15 buffer init (`I14IST`)
- Shell thickness gather and MPI exchange (`THICKVAR` + `SPMD_EXCH_THKNOD`)

#### Step 2 — ALE / multifluid (serial, line ~3465)
```
CALL ALEMAIN(TIMERS, …)        ! engine/source/ale/alemain.F
```
Handles ALE advection and cell-to-node remapping (only if `/ALE` model active).

#### Step 3 — 1D element forces (line ~3545)
```
CALL FORINTS(…)                ! engine/source/elements/forints.F
```
Forces for SPH particles (and other 1D-family elements).

#### Step 4 — Contact / interface forces (lines ~3650–3960)
```
CALL INTFOP8(…)                ! TYPE 8 interface (rigid wall)
CALL INTFOP1(…)                ! TYPE 1 contact
CALL INTFOP2(…)                ! TYPE 2,5,7,9,10,11,14,15,16,17,18,21,24,25 interfaces
```
`INTFOP2` (`engine/source/interfaces/interf/intfop2.F`) is the main contact kernel;
it loops over all active interface groups and dispatches to the appropriate
contact law.

After interface forces, MPI exchanges distribute contact results across domains:
`SPMD_EXCH_PRESS`, `SPMD_EXCH_EFRIC`, `SPMD_I7FCOM_POFF/PON`, etc.

#### Step 5 — Shell internal forces (line ~4093)
```
CALL FORINTC(TIMERS, GPU, …)   ! engine/source/elements/forintc.F
```
Internal forces for all **4-node quadrilateral shells** (coque4n family).

#### Step 6 — Solid and other element forces (line ~4178)
```
CALL FORINT(TIMERS, PYTHON, …) ! engine/source/elements/forint.F
```
Internal forces for **solids, beams, truss, springs, 3-node shells, thick shells,
user elements, XFEM, IG3D**. `FORINT` loops over element groups NG=1..NGROUP,
dispatches by element type via `IPARG(5,NG)`:

```fortran
! Inside forint.F:
GBUF => ELBUF_TAB(NG)%GBUF
SELECT CASE (IPARG(5,NG))
  CASE(1) : CALL SFORC3(…, ELBUF_TAB, NG, …)   ! solide4 (8-noded hex)
  CASE(2) : CALL S8ZFORC3(…)                    ! solidez
  ! ... many more cases
END SELECT
```

#### Step 7 — SPH element forces (line ~4320)
```
CALL FORINTP(TIMERS, …)        ! engine/source/elements/forintp.F
```

#### Step 8 — Non-local MPI exchange (line ~4537)
```
CALL SPMD_EXCH_SUB_PON(NLOC_DMG)   ! non-local damage exchange (Parith/ON)
```

#### Step 9 — MPI force assembly and exchange (line ~4641–4755)

Two paths depending on `IPARIT` flag:

| `IPARIT` | Method |
|----------|--------|
| 0 (Parith/OFF) | `SPMD_EXCH_A(…)` — direct boundary-node force exchange |
| 1 (Parith/ON) | `SPMD_EXCH2_A_PON(…)` — skyline force assembly before exchange |

After MPI exchange, the `!$OMP PARALLEL` section assembles Parith/ON skyline forces
with `ASSPAR4` (line ~4877).

#### Step 9b — Node splitting (crack propagation, line ~5310)

When non-local damage regularization is active (`NLOC_DMG%IMOD > 0`), the
node-splitting block runs after force assembly and before `ACCELE`:
`NLOC_SHELL_DETACH` detects and applies crack-propagation node splits, and, when
nodes were split, the boundary lists, ghost shells, and Parith/ON send/recv
tables are rebuilt (`SPMD_REBUILD_BOUNDARY`, `SPMD_SUB_BOUNDARIES`,
`INIT_GHOST_SHELLS`, `REBUILD_PON_TABLES`). See `doc/NODE_SPLITING.md`.

#### Step 10 — Acceleration (inside `!$OMP PARALLEL`, line ~6915)
```fortran
CALL ACCELE(NODES%A, NODES%AR, NODES%V, NODES%MS, NODES%IN, …)
```
Divides total nodal force `NODES%A` by nodal mass `NODES%MS` to get acceleration.
Rotational acceleration divides the nodal moment `NODES%AR` by the nodal
rotational inertia `NODES%IN`.

Temperature update (thermal coupling):
```fortran
CALL TEMPUR(NODES%TEMP, NODES%MCP, FTHE, …)
```

#### Step 11 — Velocity and coordinate update (inside `!$OMP PARALLEL`)

**Not a named subroutine**; done inline in `RESOL`. The leapfrog (central-difference)
scheme updates velocities and coordinates directly on `NODES%V`, `NODES%VR`,
`NODES%X`, `NODES%D`. The update follows:

```
V(n+1/2) = V(n-1/2) + DT * A(n)
X(n+1)   = X(n)     + DT * V(n+1/2)
D(n+1)   = D(n)     + DT * V(n+1/2)
```

Kinematic conditions (imposed velocities/displacements) are applied here through
`NODES%IKINE`/`NODES%KINET` arrays, which were set up by `RESOL_INIT`.

#### Step 12 — Time-step computation (line ~5780–5960)

Time step starts as the minimum over all element groups (computed inside the force
routines and stored in `NODES%STIFN` / `NODES%STIFR`). Then:

- Nodal time step: `CALL DTNODA(…)` or `CALL DTNODAMS(…)` (AMS version)
  inside `!$OMP PARALLEL`
- Optional damped/Rayleigh time step: `CALL DTNODAMP`, `CALL DTNODARAYL`
- Global MPI minimum: implicit via `SPMD_EXCH_A` (STIFN/STIFR are in the exchange)
- Final global `DT2 = min(DT2T_thread)` across threads and MPI domains

#### Step 13 — Output (line ~8520)
```
CALL SORTIE_MAIN(TIMERS, PM, NODES%D, NODES%V, …, ELBUF_TAB, …)
```
`SORTIE_MAIN` (`engine/source/output/sortie_main.F`) is the main output dispatcher.
It checks time triggers and calls:

| Trigger | Output | Sub-call inside `SORTIE_MAIN` |
|---------|--------|-------------------------------|
| `/ANIM/dt` | Animation (`.anim` binary) | `GENANI` / `GENSTAT` |
| `/H3D/dt` | H3D result file | `GENH3D` |
| `/TH/dt` | Time-history ASCII | `HIST2` → `THSOL`, `THCOQ`, etc. |
| `/PRINT/dt` | Stats file (`.sta`) | `GENSTAT` |
| `/DYNAIN/dt` | Dynain restart | `GENDYNAIN` |

Restart files (`_NNNN.rst`) are written by `CALL WRRESTP(…)` (line ~9547).

#### Step 14 — Time advance and termination check
```fortran
NCYCLE = NCYCLE + 1
TT = TT + DT2            ! or via TT_DOUBLE for double-precision accumulation
```
Loop terminates when `TT >= TSTOP` or cycle limit exceeded or `MSTOP` flag set.

---

## 4. Condensed one-cycle call tree

```
RESOL (resol.F:579)
  ├── THICKVAR                   shell thickness gather
  ├── TIMFUN                     function table update
  ├── STOP_SENSOR                sensor evaluation
  ├── DESACTI                    element activation/deactivation
  ├── FORCE                      external concentrated loads
  ├── ALEMAIN                    ALE advection  (if ALE)
  ├── FORINTS                    1D / SPH forces
  ├── INTFOP8 / INTFOP1 / INTFOP2   contact/interface forces
  ├── [SPMD_EXCH_PRESS, …]       MPI contact force exchange
  ├── FORINTC                    4-node shell forces
  ├── FORINT                     solid + all other element forces
  ├── FORINTP                    SPH forces
  ├── SPMD_EXCH_SUB_PON          non-local MPI exchange
  ├── SPMD_EXCH_A / _EXCH2_A_PON  nodal force MPI assembly
  ├──[!$OMP PARALLEL]
  │   ├── ASSPAR4                Parith/ON skyline assembly (if IPARIT=1)
  │   ├── NLOC_SHELL_DETACH      node splitting + table rebuilds (if NLOC_DMG%IMOD>0,
  │   │                          see doc/NODE_SPLITING.md)
  │   ├── ACCELE                 F/M → acceleration
  │   ├── TEMPUR                 temperature integration  (if thermal)
  │   ├── [leapfrog inline]      V += DT*A; X += DT*V
  │   └── DTNODA / DTNODAMS     nodal time step (per thread)
  │  [!$OMP END PARALLEL]
  ├── DTNODAMP / DTNODARAYL      damping time step corrections
  ├── SORTIE_MAIN                output dispatcher
  │   ├── GENANI / GENSTAT       animation / stats (if triggered)
  │   ├── GENH3D                 H3D (if triggered)
  │   ├── HIST2                  time-history (if triggered)
  │   └── GENDYNAIN              dynain (if triggered)
  ├── WRRESTP                    restart write (if triggered)
  └── TT += DT2; NCYCLE++
```

---

## 5. Key variables in `RESOL`

| Variable | Type | Meaning |
|----------|------|---------|
| `TT` | `my_real` | Current simulation time |
| `DT2` | `my_real` | Current time step |
| `DT2T` | `my_real` | Thread-local candidate time step |
| `NCYCLE` | integer | Cycle counter |
| `TSTOP` | `my_real` | Stop time |
| `NGROUP` | integer | Number of element groups |
| `IPARIT` | integer | 0=Parith/OFF, 1=Parith/ON assembly |
| `NSPMD` | integer | Number of MPI domains |
| `NTHREAD` | integer | Number of OpenMP threads |
| `NODES%A` | `my_real(3,NUMNOD)` | Nodal forces → accelerations |
| `NODES%V` | `my_real(3,NUMNOD)` | Nodal velocities |
| `NODES%X` | `my_real(3,NUMNOD)` | Nodal coordinates |
| `NODES%D` | `my_real(3,NUMNOD)` | Nodal displacements |
| `NODES%MS` | `my_real(NUMNOD)` | Nodal translational mass |
| `NODES%IN` | `my_real(NUMNOD)` | Nodal rotational inertia (used with `IRODDL`) |
| `NODES%STIFN` | `my_real(NUMNOD)` | Nodal stiffness (for DT calc) |
| `ELBUF_TAB` | `TYPE(ELBUF_STRUCT_)(NGROUP)` | Element state buffers |
| `IPARG` | `integer(NPARG,NGROUP)` | Element group descriptors |

---

## 6. Files

| File | Role |
|------|------|
| `engine/source/engine/radioss.F` | Program entry (40 lines) |
| `engine/source/engine/radioss2.F` | Setup, input read, calls `RESOL` |
| `engine/source/engine/resol.F` | THE main loop (~10000 lines) |
| `engine/source/engine/resol_init.F` | Per-cycle initialization inside `!$OMP PARALLEL` |
| `engine/source/engine/resol_head.F` | Time-step print header |
| `engine/source/engine/resol_alloc.F90` | Dynamic allocation for `RESOL` arrays |
| `engine/source/elements/forint.F` | Solid/beam/spring/shell/XFEM force loop |
| `engine/source/elements/forintc.F` | 4-node shell (coque4n) force loop |
| `engine/source/elements/forints.F` | SPH / 1D force loop |
| `engine/source/elements/forintp.F` | SPH particle force loop |
| `engine/source/interfaces/interf/intfop2.F` | Contact/interface force dispatcher |
| `engine/source/ale/alemain.F` | ALE advection driver |
| `engine/source/output/sortie_main.F` | Output dispatcher (anim, H3D, TH, dynain) |
| `engine/source/output/restart/wrrestp.F` | Restart file writer |

---

## 7. See also

- `doc/ELBUF_TAB_documentation.md` — element buffer structure passed to `FORINT`
- `doc/GROUPS_AND_CONNECTIVITY_documentation.md` — `IPARG`, `IXS`/`IXC`, nodal arrays
- `doc/SPMD_documentation.md` — MPI force exchange routines
- `doc/NLOCAL_STR_documentation.md` — non-local damage loop inside the cycle
- `doc/NODE_SPLITING.md` — node splitting (crack propagation) inside the cycle
