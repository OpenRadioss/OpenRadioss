# SPDX-License-Identifier: AGPL-3.0-or-later
"""Regression for a bounded tensile return after exhausted scalar searches.

The fallback is an explicitly tracked, dissipative interior over-return,
not an exact consistency solution or a replacement for material calibration.
It is limited to an already post-peak tensile state. State 16 is zero for
ordinary active points, negative for the cumulative number of relaxed
substeps, and positive for erosion; the existing Engine ABI is unchanged.

Run with --build-dir <scratch directory> --compiler <gfortran>. Supplying
--baseline <pre-fix probe.dll/probe.so> also verifies bitwise identity on
ordinary paths and demonstrates the original failures. No solver is run.
"""
import argparse
import ctypes as ct
import hashlib
import json
import math
import os
from pathlib import Path
import random
import subprocess

D = ct.c_double
A6, A16, A38 = D * 6, D * 16, D * 38
MIXED = [1e-4, -5e-5, 0, 1e-4, 0, 0]
# Observed with gfortran double precision, -O0, on Windows. Failure step
# can differ across compilers; the fixed implementation must remain bounded.
KNOWN = [
    (.022117799297770194, .00788249864844147, 72),
    (.022273104102200302, .0009405865545511054, 8),
    (.019294168208900047, .0008062348127212463, 7),
    (.020347905177152806, .0011866655329918073, 10),
    (.024384234216443087, .0008603565197458339, 7),
]


def refs(*values):
    return [ct.byref(D(v)) for v in values]


class Point:
    def __init__(self, lib, ft=None, epm=None):
        self.lib = lib
        self.c, self.s, self.h = A38(), A6(), A16()
        lib.defaults(self.c)
        if ft is not None:
            self.c[11] = ft
        if epm is not None:
            self.c[27] = epm
        self.rho = self.c[0]

    def step(self, de=MIXED, dt=1e-3, require_success=True):
        before_s, before_h = list(self.s), list(self.h)
        rho = self.rho * math.exp(-sum(de[:3]))
        sound, status = D(), ct.c_int()
        self.lib.update(self.c, *refs(dt, rho, 0), A6(*de), self.s, self.h,
                        ct.byref(sound), ct.byref(status))
        if status.value:
            assert list(self.s) == before_s and list(self.h) == before_h, 'failed update mutated its inputs'
            assert not require_success, f'integration failed: {status.value}, FT={self.c[11]}, EPM={self.c[27]}'
            return status.value
        self.rho = rho
        assert all(math.isfinite(v) for v in [*self.s, *self.h, sound.value])
        assert sound.value > 0
        assert before_h[2] <= self.h[2] <= 1
        assert self.h[1] >= before_h[1] and self.h[3] >= before_h[3]
        assert self.h[15] <= 0, 'robustness cases must not erode the element'
        assert self.h[15] <= before_h[15], 'fallback history must persist'
        assert self.h[15] == math.floor(self.h[15])
        mean = sum(self.s[:3]) / 3
        dev = [s - mean for s in self.s[:3]] + list(self.s[3:])
        q = math.sqrt(1.5 * (sum(s*s for s in dev[:3]) + 2*sum(s*s for s in dev[3:])))
        assert abs(q - self.h[9]) < 1e-7 * self.c[9]
        assert abs(-mean - self.h[7]) < 1e-7 * self.c[9]
        # Check the actual returned state, never simply tolerate a small
        # nonzero status or accept an outside-yield stress as a "best" root.
        if self.h[15] < before_h[15]:
            assert q <= self.h[11] + 1e-7 * self.c[9]
            out = A6()
            self.lib.surfaces(self.c, *refs(self.h[7]), A6(*dev),
                              *refs(self.h[0], self.h[1], self.h[8], self.h[1],
                                    self.h[2], self.h[5]), out)
            assert self.h[7] >= out[4] - 1e-7 * self.c[9]
            # Recover the actual EOS pressure using the updated tensile
            # plastic volume. Energy is zero in these reproduction paths.
            pressure, alpha, bulk, ec = D(), D(), D(), ct.c_int()
            self.lib.eos(self.c, *refs(rho * math.exp(self.h[3]), 0, self.h[0]),
                         ct.byref(pressure), ct.byref(alpha), ct.byref(bulk), ct.byref(ec))
            assert ec.value == 0
            assert abs(pressure.value - self.h[7]) < 1e-7 * self.c[9]
        return 0


def load(path):
    lib = ct.CDLL(str(path.resolve()))
    for name in ['defaults', 'eos', 'surfaces', 'update']:
        getattr(lib, name).restype = None
    return lib


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--build-dir', type=Path, required=True)
    ap.add_argument('--compiler', default='gfortran')
    ap.add_argument('--baseline', type=Path)
    ap.add_argument('--sdk-build', type=Path,
                    help='Optional existing GNU SDK .mod directory for a real USER01 adapter check')
    args = ap.parse_args()
    src = Path(__file__).resolve().parent.parent
    build = args.build_dir.resolve()
    build.mkdir(parents=True, exist_ok=True)
    probe = build / ('probe.dll' if os.name == 'nt' else 'probe.so')
    flags = ['-shared', '-fPIC', '-O0', '-g', '-fcheck=all',
             '-ffpe-trap=invalid,zero,overflow', '-fbacktrace', '-Wall', '-Wextra']
    subprocess.run([args.compiler, *flags, str(src/'rht_material_mod.F90'),
                    str(src/'tests/probe.F90'), '-o', str(probe)], cwd=build, check=True)
    lib = load(probe)
    result = {'source_sha256': hashlib.sha256((src/'rht_material_mod.F90').read_bytes()).hexdigest(),
              'compiler_flags': flags, 'checks': {}, 'known_paths': []}

    user = Point(lib, .0282, .002)
    for step in range(100):
        user.step()
        if step == 17:
            assert abs(user.h[2] - .96929873267615) < 2e-7
            result['user_step17_damage'] = user.h[2]
    assert user.h[2] == 1 and list(user.s) == [0]*6
    result['checks']['reported_parameter_path_100_steps'] = True

    partial_recovery = False
    for ft, epm, failure_step in KNOWN:
        p = Point(lib, ft, epm)
        snapshots = []
        for step in range(120):
            previous = p.h[15]
            p.step()
            if p.h[15] < previous:
                snapshots.append({'step': step, 'damage': p.h[2], 'relaxation_substeps': -p.h[15]})
                partial_recovery |= p.h[2] < 1
        assert p.h[2] == 1 and list(p.s) == [0]*6
        # Unload and reconfine the recovered point. Damage must not heal,
        # and negative diagnostic history must not mark it as eroded.
        for _ in range(40):
            p.step([-1e-4]*3 + [0]*3)
        assert p.h[7] > 0 and p.h[2] == 1
        result['known_paths'].append({'ft': ft, 'epm': epm, 'old_failure_step': failure_step,
                                      'fallback_events': snapshots})
    assert partial_recovery, 'need an observed recovery with D<1, not only forced full damage'
    result['checks']['five_failed_paths_recover_and_reconfine'] = True
    result['checks']['partially_damaged_recovery_observed'] = True

    randomizer = random.Random(713)
    fallback_paths = 0
    for _ in range(200):
        p = Point(lib, randomizer.uniform(.015, .1), 10**randomizer.uniform(-3.3, -1.5))
        for _ in range(100):
            p.step()
            if p.h[2] == 1:
                break
        fallback_paths += p.h[15] < 0
    result['checks']['two_hundred_seeded_parameter_paths'] = True
    result['seeded_paths_using_fallback'] = fallback_paths

    # Invalid parameters/state still fail transactionally, including at a
    # point with real fallback history. No blanket status-to-success change.
    for target, index, value in [('c', 1, -1), ('c', 11, 0), ('h', 2, 1.01)]:
        p = Point(lib, *KNOWN[-1][:2])
        for _ in range(8):
            p.step()
        getattr(p, target)[index] = value
        assert p.step(require_success=False) == 1
    result['checks']['invalid_input_and_history_roll_back'] = True

    # Invalid history must be rejected before the already-eroded shortcut
    # can zero caller stress. Check with nonzero input stress so a rejected
    # update that still commits erosion cannot pass unnoticed.
    p = Point(lib, *KNOWN[-1][:2])
    for _ in range(8):
        p.step()
    p.h[2], p.h[15] = 1.01, 1.
    p.s[:] = [1., 2., 3., 4., 5., 6.]
    assert p.step(require_success=False) == 1
    result['checks']['invalid_eroded_history_rolls_back_nonzero_stress'] = True

    if args.baseline:
        old = load(args.baseline)
        exact_steps = 0
        for de in [[-2e-5, 0, 0, 0, 0, 0], [2e-6, 0, 0, 0, 0, 0],
                   [0, 0, 0, 1e-5, 0, 0], [-1e-5, -2e-6, 3e-6, 4e-6, 5e-6, 6e-6]]:
            a, b = Point(old), Point(lib)
            for _ in range(400):
                a.step(de, dt=1e-6)
                b.step(de, dt=1e-6)
                assert list(a.s) == list(b.s) and list(a.h) == list(b.h)
                exact_steps += 1
        result['checks']['ordinary_paths_bitwise_unchanged'] = True
        result['ordinary_identical_steps'] = exact_steps
        result['baseline_failures'] = []
        for ft, epm, _ in KNOWN:
            p = Point(old, ft, epm)
            for step in range(120):
                status = p.step(require_success=False)
                if status:
                    result['baseline_failures'].append({'ft': ft, 'epm': epm, 'step': step,
                                                        'status': status, 'damage': p.h[2]})
                    break
        assert result['baseline_failures'], 'provided baseline did not reproduce any expected failure'
    if args.sdk_build:
        recovered = Point(lib, *KNOWN[-1][:2])
        for _ in range(8):
            recovered.step()
        assert recovered.h[15] < 0 and recovered.h[2] < 1
        # Seed the real adapter with an actual recovered state. Small
        # floating-point differences between a DLL call and a Fortran
        # driver need not trigger relaxation at the very same substep.
        seed_lines = ['  rho(1)=' + format(recovered.rho, '.17e') + '_wp']
        seed_lines += [f'  history(1,{i+1})={value:.17e}_wp' for i, value in enumerate(recovered.h)]
        seed_lines += [f'  buf%sigo{name}(1)={value:.17e}_wp'
                       for name, value in zip(['xx', 'yy', 'zz', 'xy', 'yz', 'zx'], recovered.s)]
        adapter_source = build/'test_relaxation_adapter.F90'
        adapter_source.write_text('''! Generated by test_return_robustness.py; no Engine required.
program test_relaxation_adapter
  use rht_material_mod
  use law_userso, only: ulawintbuf
  implicit none
  type(ulawintbuf) :: buf
  real(wp) :: c(nparam),rho(1),vol(1),energy(1),sound(1),visc(1)
  real(wp) :: history(1,nstate),off(1),sigy(1),pla(1),tf(1),de(6)
  integer :: i,ngl(1),ifunc(1),npf(1)
  call rht_defaults(c)
  c(12)=0.024384234216443087_wp
  c(28)=0.0008603565197458339_wp
  rho=c(1)
  energy=0.0_wp
  history=0.0_wp
  off=1.0_wp
  tf=0.0_wp
  ngl=91
  ifunc=0
  npf=0
  buf%rho0(1)=c(1)
  buf%sigoxx(1)=0.0_wp
  buf%sigoyy(1)=0.0_wp
  buf%sigozz(1)=0.0_wp
  buf%sigoxy(1)=0.0_wp
  buf%sigoyz(1)=0.0_wp
  buf%sigozx(1)=0.0_wp
! INITIAL_RECOVERED_STATE
  do i=1,160
    de=[1.0e-4_wp,-5.0e-5_wp,0.0_wp,1.0e-4_wp,0.0_wp,0.0_wp]
    if (i>120) de=[-1.0e-4_wp,-1.0e-4_wp,-1.0e-4_wp,0.0_wp,0.0_wp,0.0_wp]
    rho=rho*exp(-sum(de(1:3)))
    vol=1.0_wp/rho
    buf%depsxx(1)=de(1)
    buf%depsyy(1)=de(2)
    buf%depszz(1)=de(3)
    buf%depsxy(1)=de(4)
    buf%depsyz(1)=de(5)
    buf%depszx(1)=de(6)
    call luser01(1,nparam,nstate,0,ifunc,npf,tf,real(i,wp)*1.0e-3_wp,1.0e-3_wp, &
                 c,rho,vol,energy,ngl,sound,visc,history,off,sigy,pla,buf)
    if (off(1)/=1.0_wp) stop 1
    if (history(1,16)>=0.0_wp) stop 2
    if (sound(1)<=0.0_wp) stop 4
    buf%sigoxx(1)=buf%signxx(1)
    buf%sigoyy(1)=buf%signyy(1)
    buf%sigozz(1)=buf%signzz(1)
    buf%sigoxy(1)=buf%signxy(1)
    buf%sigoyz(1)=buf%signyz(1)
    buf%sigozx(1)=buf%signzx(1)
  end do
  if (history(1,3)/=1.0_wp .or. history(1,8)<=0.0_wp) stop 5
  ! Real strain erosion still changes OFF, even after negative diagnostics.
  c(4)=history(1,2)/2
  rho=rho*exp(-sum(de(1:3)))
  vol=1.0_wp/rho
  call luser01(1,nparam,nstate,0,ifunc,npf,tf,0.161_wp,1.0e-3_wp, &
               c,rho,vol,energy,ngl,sound,visc,history,off,sigy,pla,buf)
  if (off(1)/=0.0_wp .or. history(1,16)/=1.0_wp) stop 6
  print *, 'PASS: 160 active USER01 updates, fallback history, reconfinement, subsequent erosion'
end program
subroutine arret(code)
  implicit none
  integer, intent(in) :: code
  print *, 'Unexpected ARRET: ',code
  stop 99
end subroutine
subroutine write_iout(message,nchar)
  implicit none
  integer, intent(in) :: nchar
  character(len=*), intent(in) :: message
  print *,message(1:nchar)
end subroutine
'''.replace('! INITIAL_RECOVERED_STATE', '\n'.join(seed_lines)), encoding='ascii')
        adapter = build/('test_relaxation_adapter.exe' if os.name == 'nt' else 'test_relaxation_adapter')
        subprocess.run([args.compiler, '-O0', '-g', '-fcheck=all',
                        '-ffpe-trap=invalid,zero,overflow', '-I'+str(args.sdk_build.resolve()),
                        str(src/'rht_material_mod.F90'), str(src/'luser01.F90'),
                        str(adapter_source), '-o', str(adapter)], cwd=build, check=True)
        executed = subprocess.run([str(adapter)], cwd=build, check=True, capture_output=True, text=True)
        result['checks']['real_sdk_adapter_negative_counter_and_erosion'] = True
        result['adapter_seed_damage'] = recovered.h[2]
        result['adapter_seed_fallback_substeps'] = -recovered.h[15]
        result['adapter_stdout'] = executed.stdout.strip()
    result['passed'] = True
    (build/'return_robustness.json').write_text(json.dumps(result, indent=2) + '\n', encoding='utf-8')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
