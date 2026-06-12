# `ELBUF_STRUCT_` — Element Buffer Data Structure

This document describes the `ELBUF_STRUCT_` derived type and the `ELBUF_TAB` array
that is passed through most of the element kernel in OpenRadioss. It covers the type
hierarchy, the meaning of each sub-structure, the addressing scheme, the lifecycle
from Starter to Engine, and how element routines access the buffer.

---

## 1. Purpose

Every element group (solid, shell, beam, spring, etc.) owns a collection of
**per-element** and **per-integration-point state variables**: stress tensor, plastic
strain, internal energy, material user variables (UVAR), failure indicators, and more.
`ELBUF_STRUCT_` is the container for all of these for one element group. An array of
these structures, conventionally named `ELBUF_TAB(NGROUP)`, holds the buffers for
every group in the model.

---

## 2. Definition and location

- **File**: `common_source/modules/mat_elem/elbufdef_mod.F90`
- **Module**: `ELBUFDEF_MOD`
- **Instance name**: `ELBUF_TAB` (declared as `TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP)`
  in engine entry points such as `engine/source/elements/forint.F`, line 233)

The module is shared by both the Starter and Engine; it lives in `common_source/`.

---

## 3. Type hierarchy

```
ELBUF_STRUCT_                        ! one entry per element group NG
  ├─ igtyp, nel, nlay, nptr, npts, nptt, nintlay, ixfem, nxel, idrape  (scalars)
  ├─ gbuf       : g_bufel_           ! global (mean-element) state
  ├─ bufly(:)   : buf_lay_(nlay)     ! per-layer container
  │    ├─ lbuf(:,:,:) : l_bufel_(nptr,npts,nptt)   ! per-Gauss-point state
  │    ├─ mat(:,:,:)  : buf_mat_     ! material user vars per GP
  │    ├─ fail(:,:,:) : buf_fail_    ! failure model state per GP
  │    ├─ prop(:,:,:) : buf_prop_    ! property state per GP
  │    ├─ eos(:,:,:)  : buf_eos_     ! EOS state per GP
  │    ├─ visc(:,:,:) : buf_visc_    ! viscosity state per GP
  │    ├─ poro(:,:,:) : buf_poro_    ! porosity state per GP
  │    ├─ xfem(:)     : buf_xfem_    ! XFEM cracked elements
  │    ├─ lbuf_dir(:) : l_bufel_dir_ ! direction vectors per GP
  │    └─ (layer scalars: ilaw, imat, nlay_vars, ly_*, l_* offset integers)
  ├─ intlay(:)  : buf_intlay_(nintlay) ! inter-ply layers (composites)
  ├─ xfem(:)    : buf_xfem_           ! XFEM phantom elements (nxel)
  ├─ nloc(:,:)  : buf_nloc_           ! non-local shell thickness wires
  ├─ nlocts(:,:): buf_nlocts_         ! non-local thickshell thickness wires
  └─ damp_range : buf_damp_range_     ! damping frequency range parameters
```

---

## 4. Top-level scalars of `ELBUF_STRUCT_`

| Field | Type | Meaning |
|-------|------|---------|
| `igtyp` | integer | Element type index (same as `IPARG(5,NG)`) |
| `nel` | integer | Number of elements in the group |
| `nlay` | integer | Number of integration layers (≥1; >1 for shells, composites) |
| `nintlay` | integer | Number of inter-ply layers (composites) |
| `nptr` | integer | Integration points in r-direction (in-plane direction 1) |
| `npts` | integer | Integration points in s-direction (in-plane direction 2) |
| `nptt` | integer | Integration points in t-direction (through thickness) |
| `ixfem` | integer | XFEM flag for the group |
| `nxel` | integer | Number of XFEM phantom element slots |
| `idrape` | integer | Composite draping flag |

---

## 5. `g_bufel_` — global (mean-element) buffer

Contains **one value per element** (dimension `nel`) for variables that are means or
sums over the whole element. All fields are `real(kind=WP)` 1D pointer arrays unless
noted.

Access pattern: `ELBUF_TAB(NG)%GBUF%<fieldname>(IE)` where `IE` is the local element
index 1..NEL.

Key fields (selected; defined at lines 732–1003 of `elbufdef_mod.F90`):

| Field | Description |
|-------|-------------|
| `off` | Element off-flag (`1`=active, `0`=deleted/eroded; `0<off<1` while ramping down during erosion) |
| `eint` | Internal energy per element |
| `rho` | Density |
| `vol` | Volume |
| `pla` | Mean cumulative plastic strain |
| `temp` | Mean temperature |
| `sig` | Mean stress tensor (flat array of 6 components × nel) |
| `for` | Resultant forces (shells/beams) |
| `mom` | Resultant moments (shells/beams) |
| `thk` | Shell thickness |
| `dmg` | Mean damage variable |
| `hourg` | Hourglass energy |
| `gama` | Rotation angle / frame |
| `isms` | AMS flag per element (integer) |
| `dt` | Per-element time step |
| `tm_yield` | Max/historic yield value (output tracking) |
| `tm_sig` | Max/historic principal stress (output tracking) |
| `noff` | Number of failed Gauss points (integer) |
| `ierr` | Error flag array (integer) |

**Offset integers** in `g_bufel_` (e.g. `g_pla`, `g_sig`, `g_vol`, …) store the
**starting index** of the corresponding pointer inside a flat array used during
restart I/O and restart read/write. They are set during allocation in the Starter.

---

## 6. `buf_lay_` — layer container

One `ELBUF_TAB(NG)%BUFLY(IL)` entry per integration layer `IL=1..NLAY`.

**Scalars** (key ones):

| Field | Description |
|-------|-------------|
| `ilaw` | Material law type for this layer |
| `imat` | Material index |
| `ieos` | EOS index |
| `ivisc` | Viscosity law index |
| `nfail` | Number of failure models |
| `nvar_mat` | Number of user variables (UVAR) for material |
| `nvar_loc` | Number of local (per-GP) variables in `lbuf` |
| `nvar_lay` | Number of layer-level variables |
| `nptt` | Integration points through layer |
| `ly_dmg` | Offset of layer damage in layer array |
| `ly_gama` | Offset of layer rotation |
| `l_pla` | Offset of plastic strain in `lbuf` |
| `l_sig` | Offset of stress tensor in `lbuf` |

**Layer-level arrays** (one value per element per layer):
`dmg`, `gama`, `dira`, `dirb`, `hourg`, `uelr`, `uelr1`, `off`, `offpg`, `crkdir`

**Per-integration-point sub-arrays** (accessed as `BUFLY(IL)%LBUF(IR,IS,IT)%<field>`):

```fortran
! Access: ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%pla(IE)
!   NG = group index, IL = layer (1..NLAY)
!   IR = r-point (1..NPTR), IS = s-point (1..NPTS), IT = t-point (1..NPTT)
!   IE = element index (1..NEL)
```

---

## 7. `l_bufel_` — per-integration-point state

Contains **one value per element** at a given integration point `(IR,IS,IT)`. All
fields are `real(kind=WP)` 1D pointers of length `NEL`. Defined at lines 1006–1072.

Key fields:

| Field | Description |
|-------|-------------|
| `off` | GP off-flag (same convention as `GBUF%off`: `1`=active, `0`=failed) |
| `pla` | Cumulative plastic strain at this GP |
| `wpla` | Work-equivalent plastic strain |
| `sig` | Stress tensor (flat, 6 × NEL) |
| `eint` | Internal energy at GP |
| `rho` | Density at GP |
| `vol` | Volume at GP |
| `temp` | Temperature at GP |
| `dmg` | Damage at GP |
| `dam` | Additional damage variable |
| `epsd` | Deviatoric strain rate |
| `ssp` | Sound speed |
| `mlaw` | Material law type (integer, not a pointer) |
| `lawid` | Material law ID (integer) |

---

## 8. Ancillary buffer types

| Type | Accessed via | Contents |
|------|-------------|----------|
| `buf_mat_` | `BUFLY(IL)%MAT(IR,IS,IT)` | `var(:)` — material user variables (UVAR array) |
| `buf_fail_` | `BUFLY(IL)%FAIL(IR,IS,IT)` | `floc(:)` — array of `fail_loc_` (one per failure model) |
| `fail_loc_` | inside `buf_fail_%floc(IF)` | `dam(:)`, `var(:)`, `dammx(:)` — failure state; `ilawf` law type |
| `buf_eos_` | `BUFLY(IL)%EOS(IR,IS,IT)` | `var(:)` — EOS extra vars; `vartmp(:)` (integer temp array) |
| `buf_visc_` | `BUFLY(IL)%VISC(IR,IS,IT)` | `var(:)` — viscosity law state |
| `buf_prop_` | `BUFLY(IL)%PROP(IR,IS,IT)` | `var(:)`, `varn(:)` — property state |
| `buf_nloc_` | `ELBUF_TAB(NG)%NLOC(IR,IS)` | `massth`, `unlth`, `vnlth`, `fnlth` — through-thickness non-local buffers (shells); `%NLOCTS(IR,IS)` for thick shells |
| `buf_damp_range_` | `ELBUF_TAB(NG)%DAMP_RANGE` | `alpha(:)`, `tau(:)` — Maxwell damping parameters |

---

## 9. Lifecycle

### Starter: allocation and initialization

`ELBUF_INI` (`starter/source/elements/elbuf_init/elbuf_ini.F`) allocates
`ELBUF_TAB(NG)` for each group:

1. Sets top-level scalars (`IGTYP`, `NEL`, `NLAY`, `NPTR`, `NPTS`, `NPTT`, `NINTLAY`,
   `IXFEM`, `NXEL`, `IDRAPE`) from the group descriptor `IPARG(:,NG)` (lines 396–405).
2. `ALLOCATE(ELBUF_TAB(NG)%BUFLY(NLAY))` and `ALLOCATE(ELBUF_TAB(NG)%INTLAY(NINTLAY))`.
3. Calls `ZEROVARS_AUTO(ELBUF_TAB(NG))` to zero all pointers.
4. Calls `INI_OUTMAX_AUTO(ELBUF_TAB(NG), ITY, NG)` to set up max-tracking arrays.

Subsequently, per-group initialization routines (`SINIT3`, `CINIT3`, `PINIT3`, etc.) in
`starter/source/elements/*/` fill initial values (stresses from `SIGINI`, plastic
strains, orientations) into the buffer. The Starter serializes it to the restart
file (`_NNNN.rst`) for each MPI domain.

### Engine: read and use

During engine startup (`LECSTAT`/`LECTUR`, `engine/source/input/`), the restart file is
read back and `ELBUF_TAB(NGROUP)` is reconstructed. The array is then passed by
reference to all element computation routines.

In the element force loop (`FORINT`, `engine/source/elements/forint.F`):

```fortran
! For each group NG:
GBUF => ELBUF_TAB(NG)%GBUF                       ! line 392
CALL SFORC3(TIMERS, ELBUF_TAB, NG, PM, GEO, ...) ! typical solid call (line ~404)
! Inside SFORC3:
!   accesses ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%sig(IE)
!   updates  ELBUF_TAB(NG)%GBUF%pla(IE)
```

The buffer is also passed to output routines (animation, time-history, restart write).

---

## 10. Addressing summary

```
ELBUF_TAB(NG)                       ! group NG (1..NGROUP)
  %GBUF%<field>(IE)                 ! global var, element IE
  %BUFLY(IL)                        ! layer IL (1..NLAY)
    %<layer_field>(IE)              ! layer-level var, element IE
    %LBUF(IR,IS,IT)%<field>(IE)    ! local var, GP (IR,IS,IT), element IE
    %MAT(IR,IS,IT)%var(IUVAR,IE)   ! material UVAR slot IUVAR, element IE
    %FAIL(IR,IS,IT)%floc(IF)%dam(IE)  ! failure model IF, damage, element IE
    %EOS(IR,IS,IT)%var(IEOS_VAR,IE)  ! EOS variable slot
  %INTLAY(ILAY)                     ! inter-ply layer ILAY (composites)
  %NLOC(IR,IS)%unlth(:,:)          ! through-thickness non-local buffer (shells)
  %DAMP_RANGE%alpha(ICOMP)         ! Maxwell damping component ICOMP
```

---

## 11. Files that use `ELBUF_TAB`

`elbufdef_mod.F90` is the single most widely included module in the codebase — the
caller list at the top of the file spans over 700 entries. Key categories:

| Category | Representative files |
|----------|---------------------|
| Element force loops | `engine/source/elements/forint.F`, `forints.F`, `forintc.F`, `forintp.F` |
| Solid elements | `elements/solid/solide/sforc3.F`, `solide8/s8forc3.F`, `solide4/s4forc3.F` |
| Shell elements | `elements/shell/coque/cforc3.F`, `coqueba/cbaforc3.F`, `coquez/czforc3.F` |
| Thick shells | `elements/thickshell/solidec/scforc3.F`, `solide8c/s8cforc3.F` |
| Beam/spring | `elements/beam/main_beam3.F`, `elements/spring/rforc3.F` |
| Materials | `materials/mat_share/mmain8.F`, `mulawc.F90`, `cmain3.F` |
| Failure | `materials/fail/fail_setoff_c.F` |
| Output | `output/anim/generate/animx.F`, `output/th/thsol.F` |
| Restart | `output/restart/wrrestp.F` |
| Starter init | `starter/source/elements/elbuf_init/elbuf_ini.F`, `allocbuf_auto.F` |

---

## 12. Dependencies

- **Defines**: `ELBUFDEF_MOD` (module)
- **Uses**: `precision_mod.F90` (for `WP`)
- **Used by**: virtually all element processing, material law, failure, output, and
  restart routines in both Starter and Engine.
- **Related**: `mat_elem_mod.F90` re-exports `ELBUFDEF_MOD` together with
  `MATPARAM_DEF_MOD`, `GROUP_PARAM_MOD`, `PROP_PARAM_MOD` (one-stop import for
  material routines).
