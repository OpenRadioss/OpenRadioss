# Public `/EIG` solver and eigen-animation output

This directory contains the end-to-end tests for the public OpenRadioss
eigenmode path. The Engine solves the constrained generalized problem

```text
K q = lambda M q
```

using the matrices assembled by the existing implicit mechanics path. With
animation output enabled it writes one native Radioss `E###` animation for
each retained mode. Production runs do not write an `A000` base database or
diagnostic matrix/CSV side files.

## `/EIG` controls

The public implementation uses these fields:

```text
#     Nmod     Inorm             Cutfreq             Freqmin
#    Nbloc      Incv     Niter      Ipri                 Tol  TolRigid   TolPivU   TolPivP
```

Only one `/EIG` request is accepted per run; multiple requests fail explicitly.

- `Nmod > 0` requests at most that many positive modes. With `Cutfreq > 0`,
  the request is also bounded by `Freqmin` and `Cutfreq`.
- `Nmod = -1` requests every positive mode in the frequency interval.
- `Freqmin` and `Cutfreq` use the model's inverse-time frequency unit. A zero
  lower bound remains zero; Starter stores it as `(2*pi*Freqmin)^2`.
- `Nbloc > 0` bounds component-vector transfer, recovery, and `E###` writing
  after the eigensolve. It does not split or repeat the eigensolve.
- `Inorm = 0` gives unit generalized-mass modes. `Inorm = 1` scales the largest
  absolute displacement or rotation component to one.
- `Incv` controls the SLEPc subspace-size factor; the default is `2`.
- `Niter` is the SLEPc iteration limit; the default is `300`.
- `Tol` is the SLEPc convergence tolerance; the default is `1e-8`.
- `Irigid = 0` omits rigid modes, `1` includes them in interval results, and
  `2` writes only rigid modes. The default is `1`. A plain fixed-target
  request (`Nmod > 0` with no `Cutfreq`) returns positive modes only because
  that path does not solve the complete component nullspaces. `Irigid = 2`
  ignores `Freqmin` and `Cutfreq`: it
  solves only a scale-aware near-null interval sized by the rigid and interval
  pivot tolerances, then retains only modes accepted by both SLEPc and the
  physical rigid-template subspace.
- `TolRigid` is the relative backward-error threshold used when projecting the
  six physical rigid-motion templates through the final component stiffness.
  It does not prescribe a rigid-mode count. The default is `1e-12`.
- `TolPivU` and `TolPivP` are the MUMPS null-pivot thresholds used for interval
  endpoint and fixed-target factorizations; both default to `1e-12`.
- `Ifile` must be zero. Legacy additional-mode (`1`) and Nastran-mode (`2`)
  imports are rejected explicitly because the public backend does not
  implement their constrained validation, normalization, or recovery
  semantics. `Ipri` is retained in the input schema but has no effect on
  SLEPc logging.

The restart record keeps its extended width. Its former lower-pivot slot is
reserved and zero because the removed lower-endpoint recovery algorithm no
longer consumes it.

## Mechanics path

The eigen adapter does not reconstruct element or constraint objects:

1. `IMP_GLOB_K` and `UPD_GLOB_K` assemble the production tangent stiffness,
   physical lumped mass, and constraint condensation.
2. `EIG_MATRIX_EXPORT` selects the requested independent degrees of freedom
   and exposes the already-constrained symmetric operators.
3. `EIG_SOLVE_BACKEND` dispatches the same problem to the dense reference or
   distributed SLEPc backend.
4. `RECUDIS` and `RECUKIN` expand reduced eigenvectors through the standard
   implicit recovery path.
5. Existing element response routines populate requested animation fields and
   `GENANI` writes the native files.

The mass operator includes the final Engine `MS` and `IN` arrays, including
element-lumped mass and `/ADMAS`. RBE2, `/INTER/TYPE2`, RBE3, rigid-body, and
other supported transformations have already been applied by the implicit
path.

Kinematically constrained models currently require an all-node `/EIG`
selection without a separate `/EIG` clamp. Unsupported partial constraint
closures fail explicitly instead of approximating the matrix.

## Solver paths

`OPENRADIOSS_EIG_BACKEND=auto|dense|slepc` selects the backend.

- `dense` is a single-rank reference for at most 5,000 DOFs. It materializes
  dense `K` and `M` and calls LAPACK `DSYGV`. It returns positive modes only;
  rigid-template classification and `Irigid = 2` require the SLEPc path.
- `slepc` keeps matrices and eigenvectors distributed. Fixed-count requests
  use Krylov-Schur shift-invert. Frequency ranges find independent connected
  components of the final constrained matrix, solve each component once with
  native spectrum slicing and MUMPS inertia, then merge the positive spectra.
- `auto` selects SLEPc when compiled in, otherwise dense.

For a zero lower bound in a positive-spectrum request, the SLEPc path solves
the complete interval for every
constraint-connected component, including numerical zero modes. A scale-aware
tiny negative factorization endpoint includes roundoff-negative null modes
without changing the requested physical output interval. Only after SLEPc has
converged are the eigenvalues sorted by magnitude. The numerical-zero search is
limited to a scale-relative near-null band; within that band, a 100-fold
eigenvalue jump (a ten-fold frequency jump) may separate the null cluster.
This prevents an ordinary gap between two positive flexible modes from
discarding the softer mode. Independently, the final component matrices are
used to construct mechanically admissible rigid translations and rotations.
The physical count does not force SLEPc's numerical-zero count; it only
confirms the special case where the entire solved interval is near-null and
there is no positive mode against which to observe a gap. A mode is labelled
and animated as rigid only where the numerical-zero eigenvectors selected by
their actual SLEPc result indices, mass-orthonormalized as one block, and the
physical subspace agree, using
`1 - overlap_squared <= 100*Tol`. The classifier never changes the inertia
count, solver dimensions, or convergence conditions, and it is independent of
the requested maximum frequency. Positive spectra are then merged globally;
agreed rigid vectors are retained only when `Irigid` requests them.

The output handshake retains only completed eigenvalues, component-local
distributed vectors, and shared component row maps. It does not retain `K`,
`M`, `EPS`, or a factorization. Fortran fetches only the active `Nbloc` batch,
so the solver never expands every component vector into a full-global mode set
and the eigensolve is never repeated to size output arrays.

Production MPI builds require PETSc, SLEPc, and MUMPS built against the same
MPI ABI as the Engine. The checked-in scripts build a pinned, real-double
dependency stack and OpenRadioss into one install prefix:

```bash
tools/eigenmode_probe/build_eig_dependencies.sh
tools/eigenmode_probe/build_openradioss_eig.sh
```

The scripts require Bash, Git, Python 3, GNU Make, CMake, working C/C++/Fortran
compilers, and OpenMPI compiler wrappers plus development headers. They fetch
and compile PETSc, SLEPc, MUMPS, METIS, ScaLAPACK, and OpenBLAS; preinstalled
copies of those numerical libraries are not required.

Set `EIG_INSTALL_PREFIX`, `EIG_DEPS_BUILD_ROOT`,
`EIG_OPENRADIOSS_BUILD_ROOT`, or `EIG_BUILD_JOBS` to override their defaults.
The default is four parallel build jobs.
`MPICC`, `MPICXX`, and `MPIFORT` select one OpenMPI installation for the whole
stack. Set `EIG_RUN_DEPENDENCY_CHECKS=0` only when deliberately skipping the
PETSc and SLEPc self-tests.

For safety, the dependency script will only reuse build and install directories
that it previously marked as its own. Point both variables at empty, dedicated
directories on the first run.

The dependency build pins PETSc 3.25.5 and SLEPc 3.25.1. PETSc builds the
matching MUMPS, ScaLAPACK, METIS, and OpenBLAS releases selected by that PETSc
commit. ParMETIS is explicitly disabled because its upstream licence is not
suitable for general binary redistribution; the script rejects a dependency
closure that contains it. Third-party licence texts are installed under
`share/licenses/openradioss-eig`.

The installed Engine uses `$ORIGIN/../lib` for the numerical dependency stack
and records the OpenMPI library directory selected by `MPICC`. SLEPc, PETSc,
MUMPS, and their bundled dependencies are placed in the common install prefix;
the matching OpenMPI installation and compiler runtime remain system
dependencies. Rebuild against the destination system's MPI rather than moving
the Engine between incompatible MPI installations. For MPI runs, set
`OPENBLAS_NUM_THREADS` deliberately (normally to `1`) so every rank does not
create a full machine-sized BLAS thread pool; the regression harness sets it to
its `--threads` value.

## Animation output

Use ordinary Engine animation controls, for example:

```text
/ANIM/DT
0.0 1.0
/ANIM/VECT/DISP
/ANIM/VECT/DROT
```

A run named `model` writes `modelE001`, `modelE002`, and so on. Positive modes
are labelled `Eigenmode`; accepted analytic modes are labelled `Rigid mode`.
Coordinates contain the display-scaled modal deformation and `Displacement`
contains the requested normalized eigenvector.

The normal animation cards request modal energy and element tensors, including
`/ANIM/ELEM/EINT`, `/ANIM/ELEM/ENER`, shell stress/strain tensors, and brick
stress/strain tensors. The response pass calls the established element
dispatchers, so `GENANI` consumes the ordinary family-specific buffer layouts.

## Regression harness

Run the default suite with:

```bash
python3 tools/eigenmode_probe/run_e2e_suite.py \
  --output-dir /path/to/review-directory
```

The harness defaults to one OpenMP thread. Use `--threads N` to exercise a
specific thread count; the selected value is recorded in the review bundle's
report and provenance.

Run the component-focused distributed suite with:

```bash
python3 tools/eigenmode_probe/run_e2e_suite.py \
  --cases-file tools/eigenmode_probe/e2e_component_cases.json \
  --np 8 --backend slepc \
  --starter /path/to/starter \
  --engine /path/to/engine \
  --output-dir /path/to/component-review
```

Run the unsupported legacy-input contract test on one rank with:

```bash
python3 tools/eigenmode_probe/run_e2e_suite.py \
  --cases-file tools/eigenmode_probe/e2e_unsupported_cases.json \
  --np 1 --backend slepc \
  --starter /path/to/starter \
  --engine /path/to/engine \
  --output-dir /path/to/unsupported-input-review
```

It constructs a syntactically valid legacy `Ifile=1` payload and also checks
`Ifile=2`; Starter must reject both before writing a restart.

Dedicated fixture files cover shells, solids, beams, type-25 springs,
constraint recovery, disconnected components, and partial restraint.
Successful cases check the reference spectrum, frequency/eigenvalue
consistency, rigid-template residuals, native `E###` headers and topology,
finite requested fields, normalization, and energy/tensor identities.
`e2e_prestress_cases.json` adds an initially stressed solid whose fixed modal
energy references detect first-order prestress contamination in energy
recovery.

`test_full_nullspace.c` is a focused backend regression for the case where a
component has seven numerical zero vectors: six physical rigid motions and one
internal mechanism. Compile it with the same `PUBLIC_EIG_MPI` and
`PUBLIC_EIG_SLEPC` definitions, MPI compiler, SLEPc/PETSc include and link
settings as the Engine, and link `eig_solver_slepc.c` plus
`eig_solver_mpi.c`. It must report six rigid modes, one mechanism, and five
positive modes. Run it with PETSc allocator debugging when validating a
release.

`anim_reader.py` is an independent native-file decoder used only by the tests.
The expected spectra are fixed test data rather than values regenerated by the
production solver.
