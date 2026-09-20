# RHT verification record

Completed on 2026-09-20 using double precision on Windows x64.

| Component | Version |
|---|---|
| OpenRadioss source baseline | `15b9676622666f3dca2a65cd50a5a8e5cce11c70` |
| OpenRadioss/Tools SDK | `4e52942e191d3b1ede4b320fb0f1780f4e41b59a` |
| Starter / Engine / history converter | Official `latest-20260728` Windows release |
| User library compiler | GNU Fortran 8.3.0, GNU GCC / ar from the same toolchain |
| Parallel execution | One Engine thread |

Reproduction commands are in [README.md](README.md). No commercial solver
binary, reference output or experimental dataset is required. Test oracles
are analytical calibration points, independent EOS roots, conservation of
state invariants, and comparison of the SDK adapter against the core.

## Material-point verification: 24 tests passed

`tests/test_material.py` builds the core with `-O0`, `-g`, `-Wall`,
`-Wextra`, `-fcheck=all`, and floating-point exception flags. It checks:

- Virgin zero state, small elastic shear with engineering increments,
  compressive/tensile/shear strength calibration, initial yield factors,
  and continuity between failure-surface segments.
- Lode meridians and scale invariance, including stresses scaled by
  `1e-20` and `1e20`; cyclic permutation of coordinate axes; consistency
  of returned stress with the plastic strain increment.
- Separate rate branches, continuity at their break rates, pressure
  interpolation, and use of plastic flow rather than elastic strain rate.
- Both energy EOS branches, independent tensile coefficients, irreversible
  crushing and full compaction, and an acoustic modulus checked against
  an adiabatic finite difference.
- Tensile plastic volume and damage, rate-dependent passage through the
  tensile peak, zero traction at complete tensile damage, circular and
  rate-independent fully damaged compression, and no healing on
  reconfinement.
- Irreversible erosion, invalid-input rollback, zero-time initialization,
  SI-to-mm/ms/kg unit conversion, and step refinement on compression,
  tension and shear paths with 500, 1000 and 2000 steps. Refinement limits
  are `0.001*FC` for stress, `0.001` for damage and `2e-7` for plastic
  strain. The nonsmooth peak transition does not imply a monotone error
  reduction at every step count.

## SDK interface verification: passed

`build_userlib.py --verify-adapter` compiles against the SDK's actual
`ULAWBUF` and `ULAWINTBUF` definitions. `tests/test_adapter.F90` checks the
38-constant Starter reader and elastic `PARMAT` entries, then a three-point
Engine block with an inactive point, distinct densities, volumes and
energies, and all three shear components. It verifies stresses, histories,
sound speed, plastic strain output and inactive-point handling against
direct core calls. This catches energy-unit and array-order errors that
material-point tests alone cannot expose.

## Native OpenRadioss verification: 10 cases passed

`tests/run_solver.py` generates a 10 mm cube using one solid integration
point. Every nodal translation is prescribed; these tests isolate the
constitutive update and do not measure structural impact accuracy. Starter
and Engine returned zero and Engine reported normal termination in all ten
cases. The checked CSV histories are summarized in
[tests/verification_summary.json](tests/verification_summary.json).

| Case | Additional assertion beyond shared invariants | Result |
|---|---|---|
| elastic | Analytical constrained elastic modulus; no plasticity or damage | Pass |
| hydro | Independent pressure-space p-alpha root; negligible deviator | Pass |
| compression | Finite compressive evolution with compaction and damage | Pass |
| tension | Tensile plastic volume and damage become positive | Pass |
| hydro_tension | Hydrostatic tensile plastic volume and damage | Pass |
| fast | Plastic rate enhancement exceeds 1.1 | Pass |
| energy | Polynomial EOS including specific internal energy | Pass |
| gruneisen | Gruneisen EOS including specific internal energy | Pass |
| unload | Compacted porosity remains fixed through unloading | Pass |
| spall | D reaches 1, stress vanishes, element remains active | Pass |

Every history also checks finite values, density against the prescribed
deformation determinant, irreversible alpha and damage, standard `PLAS`
against `USR2`, and pressure/equivalent stress against the stress tensor.
The largest hydrostatic pressure discrepancy was `5.44e-7`, normalized by
`max(FC, abs(reference pressure))`. In the energy cases the adapter exercised
nonzero internal energy. The complete-tension case ended with zero normal
stress and `OFF=1`, distinguishing complete tensile damage from erosion.

These histories use the release converter's finite output precision.
The last sampled time precedes the exact stop time; normal termination is
checked separately. The fast case has ten history samples and is a rate
activation check, not a high-rate accuracy benchmark. No experimental
correlation, commercial RHT equivalence, multi-element objectivity,
restart-file round trip, MPI, Linux, single-precision, or performance claim
is made by this record.
