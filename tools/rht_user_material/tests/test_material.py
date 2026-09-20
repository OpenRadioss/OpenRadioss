# SPDX-License-Identifier: AGPL-3.0-or-later
"""Independent calibration points, invariants and path convergence for RHT.

Run: python tests/test_material.py --build-dir <directory outside the source>
Requires Python 3 and gfortran. Uses only the Python standard library.
"""
import argparse
import ctypes as ct
import math
import os
from pathlib import Path
import subprocess
import sys
import unittest

D = ct.c_double
A6 = D * 6
A16 = D * 16
A38 = D * 38
TXC = [-2, 1, 1, 0, 0, 0]
TXE = [2, -1, -1, 0, 0, 0]
SHEAR = [0, 0, 0, 1, 0, 0]
LIB = None


def refs(*values):
    return [ct.byref(D(v)) for v in values]


def defaults():
    c = A38()
    LIB.defaults(c)
    return c


def eos(c, rho, e=0, alpha=None):
    p, a, k, status = D(), D(), D(), ct.c_int()
    LIB.eos(c, *refs(rho, e, c[37] if alpha is None else alpha),
            ct.byref(p), ct.byref(a), ct.byref(k), ct.byref(status))
    if status.value:
        raise RuntimeError(f'EOS failed: {status.value}, rho={rho}, alpha={alpha}')
    return p.value, a.value, k.value


def failure(c, p, s=TXC, fr=1):
    y, pt, r3 = D(), D(), D()
    LIB.failure(c, *refs(p, fr), A6(*s), ct.byref(y), ct.byref(pt), ct.byref(r3))
    return y.value * r3.value, pt.value, r3.value


def surfaces(c, p, s=TXC, alpha=None, ep=0, rate=0, oldep=0, damage=0, failed=0):
    out = A6()
    LIB.surfaces(c, *refs(p), A6(*s),
                 *refs(c[37] if alpha is None else alpha, ep, rate, oldep, damage, failed), out)
    return list(out)


class Point:
    def __init__(self, c=None):
        self.c = defaults() if c is None else c
        self.s = A6()
        self.h = A16()
        self.rho = self.c[0]

    def step(self, de, dt=1, e=0, rho=None):
        if rho is None:
            rho = self.rho * math.exp(-sum(de[:3]))
        sound, status = D(), ct.c_int()
        LIB.update(self.c, *refs(dt, rho, e), A6(*de), self.s, self.h,
                   ct.byref(sound), ct.byref(status))
        if status.value:
            raise RuntimeError(f'Update failed: {status.value}, de={de}, rho={rho}, state={list(self.h)}')
        self.rho = rho
        return sound.value


class RHTTest(unittest.TestCase):
    def assertClose(self, actual, expected, rtol=2e-7, atol=1e-8):
        self.assertLessEqual(abs(actual-expected), atol+rtol*abs(expected), (actual, expected))

    def test_virgin_zero(self):
        p = Point()
        sound = p.step([0]*6)
        self.assertEqual(list(p.s), [0]*6)
        self.assertEqual(p.h[2], 0)
        self.assertClose(sound, math.sqrt((p.c[31]/p.c[37]+4*p.c[1]/3)/p.rho))

    def test_engine_zero_time_initialization(self):
        p=Point()
        before=list(p.h)
        sound=p.step([0]*6,dt=0)
        self.assertGreater(sound,0)
        self.assertEqual(list(p.h),before)
        self.assertEqual(list(p.s),[0]*6)

    def test_strength_calibration_points(self):
        c = defaults()
        self.assertClose(failure(c, 1/3)[0], 1)
        self.assertClose(failure(c, -c[11]/3, TXE)[0], c[11])
        self.assertClose(failure(c, 0, SHEAR)[0], c[10])

    def test_failure_continuity(self):
        c = defaults()
        for fr in [1, 3]:
            for p in [0, fr/3]:
                yl = failure(c, p-1e-9, fr=fr)[0]
                yr = failure(c, p+1e-9, fr=fr)[0]
                self.assertClose(yl, yr, atol=1e-7)

    def test_lode_limits_and_scale_invariance(self):
        c = defaults()
        self.assertClose(failure(c, 0, TXC)[2], 1)
        self.assertClose(failure(c, 0, TXE)[2], c[12])
        for scale in [1e-20, 1, 1e20]:
            self.assertClose(failure(c, 0, [x*scale for x in TXE])[2], c[12])
        c[13] = 0.1
        self.assertClose(failure(c, 10, TXE)[2], 1)

    def test_published_rate_break_and_pressure_blending(self):
        c = defaults()
        c[17], c[18] = 30, 3
        for rate in [0, 0.1, 3, 30, 300]:
            for pressure in [-10, 0, 10]:
                out = (D*4)()
                LIB.rates(c, *refs(rate, pressure), out)
                expected_c = (max(rate,c[15])/c[15])**c[19] if rate <= 30 else (30/c[15])**c[19]*(rate/30)**(1/3)
                expected_t = (max(rate,c[16])/c[16])**c[20] if rate <= 3 else (3/c[16])**c[20]*(rate/3)**(1/3)
                self.assertClose(out[0], expected_c)
                self.assertClose(out[1], expected_t)
                self.assertGreaterEqual(out[2], min(out[0],out[1]))
                self.assertLessEqual(out[2], max(out[0],out[1]))

    def test_eos_both_energy_branches(self):
        c = defaults()
        c[37] = 1
        e, eta = 1234, 0.05
        rho = c[0]*(1+eta)
        poly = c[31]*eta+c[32]*eta**2+c[33]*eta**3
        p = eos(c,rho,e)[0]
        self.assertClose(p, poly+(c[4]+c[5]*eta)*rho*e)
        c[4], c[30] = 0, 1.1
        self.assertClose(eos(c,rho,e)[0], poly*(1-c[30]*eta/2)+c[30]*rho*e)

    def test_eos_tensile_coefficients(self):
        c = defaults()
        eta, e = -0.0001, 0.1
        c[14] = 2e9
        rho = c[0]*(1+eta)
        expected = (c[6]*eta+c[14]*eta*eta)/c[37]+c[4]*rho*e
        self.assertClose(eos(c,rho,e)[0],expected)

    def test_acoustic_modulus_includes_energy_work(self):
        c=defaults()
        c[37]=1
        rho=1.05*c[0]
        energy=1000
        p,a,k=eos(c,rho,energy)
        # Perturb along de=p/rho*d(log rho), the adiabatic work identity.
        h=1e-6
        dp=eos(c,rho*math.exp(h),energy+h*p/rho)[0]
        dm=eos(c,rho*math.exp(-h),energy-h*p/rho)[0]
        self.assertClose(k,(dp-dm)/(2*h),rtol=2e-6)

    def test_crush_curve_and_irreversible_unloading(self):
        c = defaults()
        alpha = c[37]
        for ratio in [1,1.001,1.01,1.1,1.2,1.35,1.6]:
            p, a, k = eos(c,c[0]*ratio,alpha=alpha)
            self.assertLessEqual(a,alpha+1e-12)
            self.assertGreaterEqual(a,1)
            if 1+1e-7 < a < alpha-1e-7:
                expected = 1+(c[37]-1)*((c[35]-p)/(c[35]-c[34]))**c[36]
                self.assertClose(a,expected,rtol=1e-8)
            alpha = a
        self.assertEqual(alpha,1)
        for ratio in [1.3,1.25,1.2]:
            p,a,k = eos(c,c[0]*ratio,alpha=alpha)
            self.assertEqual(a,1)

    def test_initial_yield_uniaxial_calibration(self):
        c = defaults()
        c[13] = 0  # isolate the homothetic scaling from pressure-dependent Q
        c[34],c[35] = 1e10,2e10  # remove compaction cap from this isolation test
        comp = surfaces(c,c[9]*c[22]/3)
        tens = surfaces(c,-c[9]*c[11]*c[23]/3,TXE)
        self.assertClose(comp[0],c[9]*c[22])
        self.assertClose(tens[0],c[9]*c[11]*c[23])

    def test_full_damage_is_circular_and_rate_independent(self):
        c = defaults()
        pressure = 5*c[9]
        expected = c[9]*c[28]*5**c[29]
        for s in [TXC,TXE,SHEAR]:
            for rate in [0,10000]:
                y = surfaces(c,pressure,s,ep=1,oldep=1,rate=rate,damage=1,failed=1)[0]
                self.assertClose(y,expected)
        self.assertEqual(surfaces(c,-1e6,ep=1,oldep=1,damage=1,failed=1)[0],0)

    def test_small_elastic_shear_and_engineering_convention(self):
        p = Point()
        p.step([0,0,0,1e-8,0,0])
        self.assertClose(p.s[3],p.c[1]*1e-8)
        self.assertEqual(p.h[1],0)

    def test_plastic_return_strain_consistency(self):
        p = Point()
        p.c[19],p.c[20] = 0,0
        p.c[34],p.c[35] = 1e10,2e10
        q_before = 0
        for _ in range(300):
            oldep = p.h[1]
            olddamage = p.h[2]
            de = 1e-5
            q_trial = q_before+math.sqrt(3)*p.c[1]*de
            p.step([0,0,0,de,0,0])
            q_after = math.sqrt(3)*abs(p.s[3])
            self.assertClose(q_trial-q_after,3*p.c[1]*(p.h[1]-oldep),atol=0.2)
            self.assertGreaterEqual(p.h[2],olddamage)
            self.assertTrue(all(math.isfinite(x) for x in p.h))
            q_before = q_after
        self.assertGreater(p.h[1],0)
        self.assertGreater(p.h[2],0)

    def test_rate_uses_plastic_flow(self):
        slow,fast = Point(),Point()
        for _ in range(80):
            previous = fast.h[1]
            slow.step([0,0,0,1e-5,0,0],dt=1)
            fast.step([0,0,0,1e-5,0,0],dt=1e-8)
            self.assertClose(fast.h[8],(fast.h[1]-previous)/1e-8,atol=1e-7)
        self.assertGreater(fast.s[3],slow.s[3]*1.2)
        elastic=Point()
        elastic.step([0,0,0,1e-9,0,0],dt=1e-12)
        self.assertEqual(elastic.h[8],0)
        self.assertEqual(elastic.h[10],1)

    def test_hydrostatic_tension_has_plastic_volume_and_damage(self):
        p = Point()
        p.c[19],p.c[20] = 0,0
        peak = 0
        for _ in range(250):
            p.step([1e-5,1e-5,1e-5,0,0,0])
            peak=max(peak,p.s[0])
            self.assertClose(p.s[0],p.s[1])
            self.assertClose(p.s[0],p.s[2])
        self.assertGreater(peak,0)
        self.assertGreater(p.h[3],0)
        self.assertGreater(p.h[2],0)
        self.assertLess(p.s[0],peak)

    def test_complete_tensile_failure_has_zero_traction(self):
        for de in [[2e-5]*3+[0]*3,[0,0,0,2e-5,0,0]]:
            p=Point()
            for _ in range(2000):
                p.step(de,dt=1e-5)
            self.assertEqual(p.h[2],1)
            self.assertEqual(list(p.s),[0]*6)
            p.step(de,dt=1e-5)
            self.assertEqual(list(p.s),[0]*6)
            self.assertEqual(p.h[15],0)  # full damage is not element erosion

    def test_dynamic_tension_peak_transition(self):
        p=Point()
        p.c[4]=0
        p.c[5]=0
        failed=False
        for _ in range(1000):
            previous=p.h[2]
            p.step([2e-6,0,0,0,0,0],dt=1e-6)
            self.assertGreaterEqual(p.h[2],previous)
            if failed:
                self.assertEqual(p.h[5],1)
            failed=p.h[5]>0.5
        self.assertTrue(failed)
        self.assertGreater(p.h[2],0)

    def test_step_refinement(self):
        # Compare the same physical history at successively smaller time steps.
        # Peak/cap crossings are nonsmooth, so convergence need not be
        # monotone at every refinement. Bound stress by 0.1% of fc, damage
        # by 0.001, and plastic strain by 2e-7 over a fourfold refinement.
        for strain, duration in [([-0.02,0,0,0,0,0],0.001),
                                 ([0.002,0,0,0,0,0],0.001),
                                 ([0,0,0,0.003,0,0],0.001)]:
            states=[]
            for n in [500,1000,2000]:
                p=Point()
                p.c[4]=0
                p.c[5]=0
                for _ in range(n):
                    p.step([v/n for v in strain],dt=duration/n)
                states.append([*list(p.s),p.h[1],p.h[2]])
            for coarse in states[:2]:
                self.assertLess(max(abs(a-b) for a,b in zip(coarse[:6],states[2][:6])),0.001*p.c[9])
                self.assertLess(abs(coarse[6]-states[2][6]),2e-7)
                self.assertLess(abs(coarse[7]-states[2][7]),0.001)

    def test_unit_invariance(self):
        # Change SI -> mm, ms, kg. Stress scales by 1e-9, density by 1e-9,
        # time by 1e3, specific energy by one.
        a=Point()
        b=Point()
        for i in [0,1,2,6,9,14,31,32,33,34,35]:
            b.c[i]*=1e-9
        for i in [15,16,17,18]:
            b.c[i]*=1e-3
        b.rho=b.c[0]
        for _ in range(100):
            de=[-1e-5,0,0,0,0,0]
            a.step(de,dt=1e-6,e=1)
            b.step(de,dt=1e-3,e=1)
        for x,y in zip(a.s,b.s):
            self.assertClose(x,y*1e9,rtol=2e-5,atol=1)
        self.assertClose(a.h[1],b.h[1],rtol=2e-5)

    def test_rotated_stress_update(self):
        # A cyclic coordinate permutation is an exact 120-degree rotation.
        a,b=Point(),Point()
        for _ in range(100):
            de=[-1e-5,-2e-6,3e-6,4e-6,5e-6,6e-6]
            rotated=[de[2],de[0],de[1],de[5],de[3],de[4]]
            a.step(de,dt=1e-6)
            b.step(rotated,dt=1e-6)
        self.assertClose(a.h[1],b.h[1])
        self.assertClose(a.h[2],b.h[2])
        for x,y in zip(b.s,[a.s[2],a.s[0],a.s[1],a.s[5],a.s[3],a.s[4]]):
            self.assertClose(x,y)

    def test_damage_does_not_heal_on_reconfinement(self):
        p=Point()
        for _ in range(350):
            p.step([0,0,0,1e-5,0,0])
        damage=p.h[2]
        self.assertGreater(damage,0)
        for _ in range(100):
            p.step([-0.0001]*3+[0]*3)
            self.assertGreaterEqual(p.h[2],damage)
            self.assertEqual(p.h[5],1)
            damage=p.h[2]

    def test_erosion_is_irreversible(self):
        p = Point()
        p.c[3] = 0.0001
        for _ in range(50):
            p.step([0,0,0,1e-5,0,0])
        self.assertEqual(p.h[15],1)
        self.assertEqual(list(p.s),[0]*6)
        p.step([-0.0001,0,0,0,0,0])
        self.assertEqual(list(p.s),[0]*6)

    def test_invalid_input_does_not_mutate_state(self):
        p = Point()
        before_s, before_h = list(p.s),list(p.h)
        p.c[1] = -1
        with self.assertRaises(RuntimeError):
            p.step([0]*6)
        self.assertEqual(list(p.s),before_s)
        self.assertEqual(list(p.h),before_h)


def main():
    global LIB
    ap = argparse.ArgumentParser()
    ap.add_argument('--build-dir',type=Path,required=True)
    ap.add_argument('--compiler',default='gfortran')
    args, extra = ap.parse_known_args()
    src=Path(__file__).resolve().parent.parent
    build=args.build_dir.resolve()
    build.mkdir(parents=True,exist_ok=True)
    lib=build/('probe.dll' if os.name=='nt' else 'probe.so')
    subprocess.run([args.compiler,'-shared','-fPIC','-g','-O0','-fcheck=all',
                    '-ffpe-trap=invalid,zero,overflow','-fbacktrace','-Wall','-Wextra',
                    str(src/'rht_material_mod.F90'),str(src/'tests/probe.F90'),'-o',str(lib)],
                   cwd=build,check=True)
    LIB=ct.CDLL(str(lib))
    for name in ['defaults','eos','failure','rates','surfaces','update']:
        getattr(LIB,name).restype=None
    unittest.main(argv=[sys.argv[0],*extra],verbosity=2)


if __name__=='__main__':
    main()
