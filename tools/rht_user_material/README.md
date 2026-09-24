# RHT solid user material

This tool implements the published three-surface Riedel-Hiermaier-Thoma
concrete model as an OpenRadioss `/MAT/USER01` solid material. It supplies
the Starter reader, Engine update, a standalone material module, and
reproducible tests. Build it against the official
[user library SDK](https://github.com/OpenRadioss/Tools/tree/main/userlib_sdk)
and load the resulting library with `-dylib` in both Starter and Engine.

The implementation includes pressure and Lode dependence, separate tensile
and compressive plastic strain-rate enhancement, a homothetic hardening
surface and cap, irreversible p-alpha compaction, an energy-dependent EOS,
tensile plastic volume, post-peak damage, residual frictional strength, and
optional strain erosion. The source is independent of commercial solver
implementations. This is a user subroutine, not a built-in LAW number or an
LS-DYNA keyword reader.

## Equations and numerical choices

The equation references are Borrvall and Riedel (2011),
[The RHT concrete model in LS-DYNA, section 5](https://lsdyna.ansys.com/wp-content/uploads/attachments/Session12_Paper1.pdf),
and the [LS-DYNA R16 Material Manual](https://lsdyna.ansys.com/wp-content/uploads/2025/04/LS-DYNA_Manual_Vol_II_R16.pdf),
`MAT_272`, printed pages 2-1817 to 2-1825. These specify the constitutive
surfaces; the numerical closure below is part of this implementation and
does not claim pointwise equivalence to a commercial solver.

- Stress is tensile-positive, pressure is `-trace(stress)/3`, and the
  six-component order is `xx, yy, zz, xy, yz, zx`. Strain increments use
  engineering shear. The host supplies the corotated stress and increments;
  the material does not apply a second rotation.
- The physical density comes directly from Engine. Porosity satisfies
  `alpha = min(alpha_old, alpha_crush(p))` with `1 <= alpha <= alpha0`.
  Pressure and alpha are solved together, rather than crushing at a trial
  pressure and retaining that inconsistent pressure. Unloading holds alpha.
- Rate enhancement uses the *plastic* strain increment divided by the
  substep duration. Each branch is continuous at its break rate and is
  bounded below by one at and below its reference rate. Fully damaged
  compressive strength is circular in the deviatoric plane and has no rate
  multiplier.
- The published tensile flow description gives the deviatoric direction
  and the `PTF=1` limit. Here its explicit interpolation is
  `d eps_p = lambda * (s - PTF*p*I)` for `p<0`; compression has deviatoric
  flow. Equivalent plastic strain uses `sqrt(2/3 * d eps_p:d eps_p)`,
  including the tensile volume contribution. Pure compaction is stored
  separately in alpha. To separate elastic volume from tensile plastic
  dilation, the mechanical EOS density is `rho*exp(eps_p_vol)`. The thermal
  energy term always uses physical `rho`.
- The return uses backward Euler with a scalar plastic multiplier and an
  inner pressure solve in tension. The hardening fraction is solved
  implicitly from the homothetic surface. Damage starts only with the
  post-peak part of plastic strain and never heals on reconfinement.
- The published tensile post-peak formula contains a finite strength drop
  at its activation even at zero damage. Pre-peak and post-peak roots are
  solved separately to avoid converging to that jump. At complete tensile
  damage, a separate limiting return evaluates finite plastic strain with
  zero pressure and deviatoric stress. Complete damage alone does not
  delete an element; `EPSF` controls erosion.
- Density is interpolated logarithmically within a host step; energy and
  strain increments are subdivided. The initial substep strain bound is
  `0.001`. A damage increment above `0.025` or a failed solve retries with
  twice as many substeps, up to 12 attempts and 65536 substeps. Stress
  residual tolerance is `2e-10*max(FC, abs(trial stress))` in double
  precision. On the last permitted subdivision attempt, an already
  post-peak tensile point whose scalar return still has status 4 can use
  a bounded zero-pressure, zero-deviatoric-stress endpoint. Its finite
  plastic strain determines damage; damage is **not** forced to one.
  Acceptance requires a finite EOS zero-pressure root with positive
  modulus, nondecreasing damage, `q <= yield strength` and
  `pressure >= tensile cutoff`. `USR16 < 0` records the number of these
  relaxed substeps while the point remains active. This interior
  over-return can locally increase dissipation and is not an exact
  consistency solution. It is reserved for exhausted tensile searches;
  invalid inputs, EOS failures, nonfinite states and unrelated failures
  are not converted to success. Any remaining failed update leaves input
  stress/history unchanged and the adapter stops Engine with an element
  ID and status code.
- Sound speed uses a fixed-porosity elastic bulk modulus, including
  adiabatic energy work under compression, plus `4G/3`. In tension the
  fixed-energy modulus bounds the smaller adiabatic modulus. Active pore
  crushing therefore does not produce an unsafe, overly large time step.
  A zero-time Engine initialization call returns sound speed without
  advancing history.

The core has no saved or shared mutable state. The adapter handles every
active point in the `NEL` block and converts Engine's total element energy
to specific energy as `EINT/(rho*volume)`.

## Build and verify

The tested configuration is Windows x64, double precision, GNU Fortran
8.3, and the OpenRadioss Windows release `latest-20260728`. Use the same
GNU compiler for the SDK modules and the user library. Python 3.8 or newer,
CMake, Ninja, GCC, gfortran, and GNU `ar` are required. The Python scripts
use only the standard library.

Run the following from the OpenRadioss repository root, with the compiler
tools on `PATH`. All output goes into `cbuild_rht`, which is ignored by the
repository. On Windows, use the `ar.exe` belonging to the GNU compiler;
an unrelated archiver on `PATH` will not work.

```text
git clone https://github.com/OpenRadioss/Tools.git cbuild_rht/Tools
git -C cbuild_rht/Tools checkout 4e52942e191d3b1ede4b320fb0f1780f4e41b59a
cmake -S cbuild_rht/Tools/userlib_sdk/source -B cbuild_rht/sdk -G Ninja -Darch=win64 -Dcompiler=gfortran -Dprecision=dp
cmake --build cbuild_rht/sdk
python tools/rht_user_material/tests/test_material.py --build-dir cbuild_rht/tests
python tools/rht_user_material/tests/test_return_robustness.py --build-dir cbuild_rht/return_tests --sdk-build cbuild_rht/sdk
python tools/rht_user_material/build_userlib.py --sdk-build cbuild_rht/sdk --build-dir cbuild_rht/library --verify-adapter
```

The return-robustness test also accepts `--baseline <old probe library>` to
compare ordinary paths bit for bit and reproduce the pre-fix return
failures. The optional `--sdk-build` check calls the real USER01 adapter
using SDK types and a local `ARRET` test stub; it does not run Engine.
It verifies that a negative diagnostic count keeps the point active,
survives reconfinement, and does not prevent later strain-based erosion.
The script writes its source hash and measured results to
`return_robustness.json` in the chosen build directory.

Extract the [official Windows release](https://github.com/OpenRadioss/OpenRadioss/releases/tag/latest-20260728)
under `cbuild_rht/runtime`, so that `cbuild_rht/runtime/OpenRadioss/exec`
contains Starter, Engine and `th_to_csv_win64.exe`. Then run:

```text
python tools/rht_user_material/tests/run_solver.py --radioss-root cbuild_rht/runtime/OpenRadioss --library cbuild_rht/library/libraduser_win64.dll --output cbuild_rht/solver_runs
```

The runner creates ten one-brick decks, invokes Starter and Engine, converts
their histories to CSV, and checks the responses. It exits nonzero on a
failed run or numerical assertion. `--case energy`, for example, runs
only the polynomial EOS case. Logs, decks, CSVs and `run_summary.json`
remain in the output directory. See [VALIDATION.md](VALIDATION.md) for
the checks, measured results, and limits of this evidence.

The build helper copies the SDK archive before replacing its two USER01
placeholder objects. It does not edit the SDK or rebuild the solver.
The GNU Linux build path is provided (`-Darch=linux64` and `.so` output),
but has not been executed here. Single precision and Intel-built user
libraries have not been validated.

## Material card

Use a native `/MAT/USER01` card with a title, the native density line,
then **all 38 constants in the order below**. The reader uses Fortran
list-directed input, so constants can span multiple lines. The native
density and constant 1 must agree. No constants may be omitted; zero
means its literal value, not an automatic default. In particular,
negative `ONEMPA` automatic calibration is unsupported. Positive `ONEMPA`
records the pressure unit; all dimensional inputs must already be in a
consistent unit system.

The `rht_defaults` routine provides the following explicit demonstration
set in m, kg, s, Pa. It does not override the input card. The solver tests
set `B0=B1=GAMMA=0` when isolating cold mechanical behavior; the `energy`
case exercises `B0=B1=1.22`, and `gruneisen` exercises `GAMMA=1.1`.

| Index | Constant | Meaning / unit | Demonstration value |
|---:|---|---|---:|
| 1 | RO | Initial bulk density, kg/m3 | 2314 |
| 2 | SHEAR | Elastic shear modulus, Pa | 1.67e10 |
| 3 | ONEMPA | Pressure units per MPa | 1e6 |
| 4 | EPSF | Erosion plastic strain; zero disables | 2 |
| 5 | B0 | Polynomial EOS energy coefficient | 1.22 |
| 6 | B1 | Polynomial EOS energy coefficient | 1.22 |
| 7 | T1 | Tensile EOS linear coefficient, Pa | 3.527e10 |
| 8 | A | Failure surface coefficient | 1.6 |
| 9 | N | Failure surface exponent | 0.61 |
| 10 | FC | Compressive strength, Pa | 3.5e7 |
| 11 | FS | Shear strength / FC | 0.18 |
| 12 | FT | Tensile strength / FC | 0.10 |
| 13 | Q0 | Lode factor at zero pressure | 0.6805 |
| 14 | B | Pressure dependence of Lode factor | 0.0105 |
| 15 | T2 | Tensile EOS quadratic coefficient, Pa | 0 |
| 16 | E0C | Compression reference rate, 1/s | 3e-5 |
| 17 | E0T | Tension reference rate, 1/s | 3e-6 |
| 18 | EC | Compression break rate, 1/s | 3e25 |
| 19 | ET | Tension break rate, 1/s | 3e25 |
| 20 | BETAC | Compression rate exponent | 0.032 |
| 21 | BETAT | Tension rate exponent | 0.036 |
| 22 | PTF | Tensile pressure contribution to flow | 0.001 |
| 23 | GC | Compression elastic strength factor | 0.53 |
| 24 | GT | Tension elastic strength factor | 0.70 |
| 25 | XI | Hardening shear modulus / SHEAR | 0.50 |
| 26 | D1 | Damage strain coefficient | 0.04 |
| 27 | D2 | Damage strain exponent | 1 |
| 28 | EPM | Minimum post-peak failure strain | 0.01 |
| 29 | AF | Residual strength coefficient | 1.6 |
| 30 | NF | Residual strength exponent | 0.61 |
| 31 | GAMMA | Gruneisen coefficient when B0=0 | 0 |
| 32 | A1 | Compressive EOS linear coefficient, Pa | 3.527e10 |
| 33 | A2 | Compressive EOS quadratic coefficient, Pa | 3.958e10 |
| 34 | A3 | Compressive EOS cubic coefficient, Pa | 9.04e9 |
| 35 | PEL | Initial pore-collapse pressure, Pa | 2.33e7 |
| 36 | PCO | Full compaction pressure, Pa | 6e9 |
| 37 | NP | Crush-curve exponent | 3 |
| 38 | ALPHA0 | Initial distention | 1.1884 |

`rht_validate` checks the supported parameter domain before simulation.
In particular, `0<PTF<=1`, `0<N<=1`, `0.5<Q0<=1`, `0<=BETAC,BETAT<=1/3`,
`NP>=1`, `ALPHA0>=1`, `PCO>PEL>=0`, and break rates cannot be below their
reference rates. `Q(p*)` is bounded above by one and below just above
one half. Coefficients yielding nonpositive acoustic bulk modulus during
an update are rejected. `B0>0` selects the compression/tension polynomial
EOS; `B0=0` selects the Gruneisen branch.

For example, this explicit SI card uses the demonstration set:

```text
/MAT/USER01/1
RHT concrete
                2314
2314 1.67e10 1e6 2 1.22
1.22 3.527e10 1.6 0.61 3.5e7
0.18 0.1 0.6805 0.0105 0
3e-5 3e-6 3e25 3e25 0.032
0.036 0.001 0.53 0.70 0.50
0.04 1 0.01 1.6 0.61
0 3.527e10 3.958e10 9.04e9 2.33e7
6e9 3 1.1884
```

## History and diagnostics

History is initialized from an all-zero state on the first positive-time
update. Preserve all 16 variables across restarts. Standard `PLAS` and
`DPLA` are the accumulated equivalent plastic strain and its increment;
`SIGY` is the current strength. Request `USR1` through `USR16` in brick
time histories to inspect the following values.

| USR | Quantity |
|---:|---|
| 1 | Irreversible distention alpha |
| 2 | Accumulated equivalent plastic strain, including tensile plastic volume |
| 3 | Damage D in [0,1] |
| 4 | Accumulated tensile plastic volumetric strain |
| 5 | Normalized hardening fraction |
| 6 | Irreversible post-peak activation flag |
| 7 | Last physical density |
| 8 | Pressure, positive in compression |
| 9 | Plastic rate in the last accepted internal substep |
| 10 | Equivalent deviatoric stress |
| 11 | Rate enhancement factor in the last substep |
| 12 | Active strength |
| 13 | Pore-collapse measure log(ALPHA0/alpha) |
| 14 | Accumulated deviatoric equivalent plastic strain |
| 15 | Specific internal energy used by the last update |
| 16 | 0: ordinary active point; negative integer: accumulated bounded tensile-return substeps while active; 1: strain erosion |

Integration status codes are 1 (invalid input/history), 2 (invalid EOS or
bulk modulus), 3 (non-finite state), 4 (return did not converge),
5 (damage increment requires subdivision), and 6 (substep limit).
An accepted bounded return has status 0 and a decreasing negative `USR16`;
this distinguishes a tracked recovery from an unreported failed solve.
The Engine adapter tests `USR16 > 0.5` for erosion. Preserve the signed
value in restart and postprocessing code; `USR16 != 0` is not an erosion
test.

The implementation targets corotational 3D solids. It does not supply a
shell/plane-stress reduction, automatic concrete calibration, nonlocal
regularization, or fracture-energy/mesh-size scaling. Material parameters,
mesh sensitivity, unloading paths and impact predictions still require
validation for the intended application. The included tests establish
equation and interface consistency, not experimental validation.
