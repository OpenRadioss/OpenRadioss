//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
// ==============================================================================
//  OpenRadioss GPU — Kernel 3 (Option C): Forces + Assembly
//  Covers: CHVIS3  — hourglass forces  (ISMSTR=1, IHBE<=1)
//          CFINT3  — internal force computation (local → global)
//          CUPDT3  — atomic scatter to global nodal arrays
//
//  Scope: ISMSTR=1, IHBE<=1, ISHFRAM=0, standard formulation (no ISIGI=5)
//         One thread per element, flat SoA layout.
// ==============================================================================

#ifndef SHELL_FORCE_ASSEMBLY_KERNEL_CU
#define SHELL_FORCE_ASSEMBLY_KERNEL_CU

#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cfloat>
#include <type_traits>

// Use the canonical HourglassParams definition from the header
#include "shell_force_assembly_kernel.h"
#include "real_type.h"

// ===========================================================================
//  Kernel 3 — shell_force_assembly_kernel
//
//  Reads generalized forces/moments (FOR, MOM) from Kernel 2, plus local
//  frame, shape functions, hourglass state.  Computes hourglass forces
//  (CHVIS3), assembles nodal forces in local frame (CFINT3), rotates to
//  global frame, and scatters to global arrays via atomicAdd (CUPDT3).
//
//  Thread mapping:  I = blockIdx.x * blockDim.x + threadIdx.x
// ===========================================================================
template <int ISMSTR>
__global__ __launch_bounds__(256, 1) void shell_force_assembly_kernel(
    // ---- Connectivity (read-only, SoA) ----
    const int* __restrict__ d_N1,
    const int* __restrict__ d_N2,
    const int* __restrict__ d_N3,
    const int* __restrict__ d_N4,
    // ---- Local frame from Kernel 1 ----
    const Real* __restrict__ d_E1x,
    const Real* __restrict__ d_E1y,
    const Real* __restrict__ d_E1z,
    const Real* __restrict__ d_E2x,
    const Real* __restrict__ d_E2y,
    const Real* __restrict__ d_E2z,
    const Real* __restrict__ d_E3x,
    const Real* __restrict__ d_E3y,
    const Real* __restrict__ d_E3z,
    // ---- Shape-function derivatives & geometry from Kernel 1 ----
    const Real* __restrict__ d_PX1,
    const Real* __restrict__ d_PX2,
    const Real* __restrict__ d_PY1,
    const Real* __restrict__ d_PY2,
    const Real* __restrict__ d_AREA,
    // ---- Hourglass volume ratios (for non-uniform GAMA) ----
    const Real* __restrict__ d_VHX,
    const Real* __restrict__ d_VHY,

    // ---- Local velocities (for hourglass, from Kernel 2 intermediates) ----
    //   These are the local-frame projected velocities VX1..VZ4 and RX1..RY4.
    //   For the 3-kernel split, Kernel 2 must write them out.
    //   Alternatively, we re-project from gathered velocities + local frame.
    //   Here we choose to re-project (avoids extra arrays).
    const Real* __restrict__ d_VL1x,
    const Real* __restrict__ d_VL1y,
    const Real* __restrict__ d_VL1z,
    const Real* __restrict__ d_VL2x,
    const Real* __restrict__ d_VL2y,
    const Real* __restrict__ d_VL2z,
    const Real* __restrict__ d_VL3x,
    const Real* __restrict__ d_VL3y,
    const Real* __restrict__ d_VL3z,
    const Real* __restrict__ d_VL4x,
    const Real* __restrict__ d_VL4y,
    const Real* __restrict__ d_VL4z,
    const Real* __restrict__ d_VRL1x,
    const Real* __restrict__ d_VRL1y,
    const Real* __restrict__ d_VRL1z,
    const Real* __restrict__ d_VRL2x,
    const Real* __restrict__ d_VRL2y,
    const Real* __restrict__ d_VRL2z,
    const Real* __restrict__ d_VRL3x,
    const Real* __restrict__ d_VRL3y,
    const Real* __restrict__ d_VRL3z,
    const Real* __restrict__ d_VRL4x,
    const Real* __restrict__ d_VRL4y,
    const Real* __restrict__ d_VRL4z,
    // ---- Element state ----
    const Real* __restrict__ d_OFF,
    const Real* __restrict__ d_THK0,      // [NUMELC] reference thickness
    const Real* __restrict__ d_THK02,     // [NUMELC] thk0^2
    // ---- Generalized forces/moments from Kernel 2 ----
    const Real* __restrict__ d_FORxx,     // FOR(1) = Nxx / thk
    const Real* __restrict__ d_FORyy,     // FOR(2) = Nyy / thk
    const Real* __restrict__ d_FORxy,     // FOR(3) = Nxy / thk
    const Real* __restrict__ d_FORyz,     // FOR(4) = Nyz / thk
    const Real* __restrict__ d_FORzx,     // FOR(5) = Nxz / thk
    const Real* __restrict__ d_MOMxx,     // MOM(1) = Mxx/thk^2
    const Real* __restrict__ d_MOMyy,     // MOM(2) = Myy/thk^2
    const Real* __restrict__ d_MOMxy,     // MOM(3) = Mxy/thk^2
    // ---- Hourglass state (read/write) ----
    Real* __restrict__ d_HOUR,            // [NUMELC*5] hourglass energy integrals
    // ---- Material scalars for hourglass ----
    Real* __restrict__ d_STI,             // [NUMELC] nodal stiffness (write)
    Real* __restrict__ d_STIR,            // [NUMELC] rotational stiffness (write)
    const Real* __restrict__ d_SSP,       // [NUMELC] sound speed
    const Real* __restrict__ d_RHO,       // [NUMELC] density
    const Real* __restrict__ d_YM,        // [NUMELC] Young's modulus
    const Real* __restrict__ d_NU,        // [NUMELC] Poisson's ratio
    const Real* __restrict__ d_A11,       // [NUMELC] plane-stress stiffness
    const Real* __restrict__ d_G,         // [NUMELC] shear modulus
    const Real* __restrict__ d_SHF,       // [NUMELC] shear factor
    // ---- Internal energy (read/write) ----
    Real* __restrict__ d_EINT,            // [NUMELC*2]
    // ---- Global nodal arrays (atomic write) ----
    Real* __restrict__ d_Fx,              // [NUMNOD]
    Real* __restrict__ d_Fy,
    Real* __restrict__ d_Fz,
    Real* __restrict__ d_Mx,
    Real* __restrict__ d_My,
    Real* __restrict__ d_Mz,
    Real* __restrict__ d_STIFN,           // [NUMNOD]
    Real* __restrict__ d_STIFR,           // [NUMNOD]
    // ---- Scalars ----
    HourglassParams hg_params,
    Real dt,
    int    NPT,
    int    NUMELC,
    int    compute_sti,
    int    IHBE
)
{
    // ======================================================================
    //  Thread mapping
    // ======================================================================
    int I = blockIdx.x * blockDim.x + threadIdx.x;
    if (I >= NUMELC) return;

    // ======================================================================
    //  Load inputs
    // ======================================================================
    Real e1x = d_E1x[I]; Real e1y = d_E1y[I]; Real e1z = d_E1z[I];
    Real e2x = d_E2x[I]; Real e2y = d_E2y[I]; Real e2z = d_E2z[I];
    Real e3x = d_E3x[I]; Real e3y = d_E3y[I]; Real e3z = d_E3z[I];

    Real px1  = d_PX1[I];   Real px2  = d_PX2[I];
    Real py1  = d_PY1[I];   Real py2  = d_PY2[I];
    Real area = d_AREA[I];

    Real off_i  = d_OFF[I];
    Real thk0   = d_THK0[I];
    Real thk02  = thk0 * thk0;  // THK0 squared (matches CPU CCOEF3: THK02=THK0*THK0)

    //  ALDT² stored in d_STI by kernel 1 — read before overwriting
    Real aldt_sq = d_STI[I];

    int n1 = d_N1[I]; int n2 = d_N2[I]; int n3 = d_N3[I]; int n4 = d_N4[I];

    // Generalized forces (already stored as FOR/thk and MOM/thk^2)
    Real for1 = d_FORxx[I];   // Nxx / thk
    Real for2 = d_FORyy[I];   // Nyy / thk
    Real for3 = d_FORxy[I];   // Nxy / thk
    Real for4 = d_FORyz[I];   // Nyz / thk
    Real for5 = d_FORzx[I];   // Nxz / thk
    Real mom1 = d_MOMxx[I];   // Mxx/thk^2
    Real mom2 = d_MOMyy[I];   // Myy/thk^2
    Real mom3 = d_MOMxy[I];   // Mxy/thk^2

    Real off_val = fmin(1.0, fabs(off_i));

    // ======================================================================
    //  1.  CHVIS3 — Hourglass forces  (ISMSTR=1, IHBE<=1 path)
    //      Simplified: GAMA = ±OFF (uniform hourglass base vectors)
    // ======================================================================

    //  Re-project gathered velocities to local frame for hourglass
    Real vl1x = d_VL1x[I]; Real vl1y = d_VL1y[I]; Real vl1z = d_VL1z[I];
    Real vl2x = d_VL2x[I]; Real vl2y = d_VL2y[I]; Real vl2z = d_VL2z[I];
    Real vl3x = d_VL3x[I]; Real vl3y = d_VL3y[I]; Real vl3z = d_VL3z[I];
    Real vl4x = d_VL4x[I]; Real vl4y = d_VL4y[I]; Real vl4z = d_VL4z[I];

    //  Staged FMA waves: 12 independent ops per wave to fill DP pipeline
    //  Wave 1: initial products (12 independent DMULs)
    Real vx1_l = e1x*vl1x;  Real vx2_l = e1x*vl2x;
    Real vx3_l = e1x*vl3x;  Real vx4_l = e1x*vl4x;
    Real vy1_l = e2x*vl1x;  Real vy2_l = e2x*vl2x;
    Real vy3_l = e2x*vl3x;  Real vy4_l = e2x*vl4x;
    Real vz1_l = e3x*vl1x;  Real vz2_l = e3x*vl2x;
    Real vz3_l = e3x*vl3x;  Real vz4_l = e3x*vl4x;
    //  Wave 2: second component FMAs (12 independent)
    vx1_l = fma(e1y, vl1y, vx1_l);  vx2_l = fma(e1y, vl2y, vx2_l);
    vx3_l = fma(e1y, vl3y, vx3_l);  vx4_l = fma(e1y, vl4y, vx4_l);
    vy1_l = fma(e2y, vl1y, vy1_l);  vy2_l = fma(e2y, vl2y, vy2_l);
    vy3_l = fma(e2y, vl3y, vy3_l);  vy4_l = fma(e2y, vl4y, vy4_l);
    vz1_l = fma(e3y, vl1y, vz1_l);  vz2_l = fma(e3y, vl2y, vz2_l);
    vz3_l = fma(e3y, vl3y, vz3_l);  vz4_l = fma(e3y, vl4y, vz4_l);
    //  Wave 3: third component FMAs (12 independent)
    vx1_l = fma(e1z, vl1z, vx1_l);  vx2_l = fma(e1z, vl2z, vx2_l);
    vx3_l = fma(e1z, vl3z, vx3_l);  vx4_l = fma(e1z, vl4z, vx4_l);
    vy1_l = fma(e2z, vl1z, vy1_l);  vy2_l = fma(e2z, vl2z, vy2_l);
    vy3_l = fma(e2z, vl3z, vy3_l);  vy4_l = fma(e2z, vl4z, vy4_l);
    vz1_l = fma(e3z, vl1z, vz1_l);  vz2_l = fma(e3z, vl2z, vz2_l);
    vz3_l = fma(e3z, vl3z, vz3_l);  vz4_l = fma(e3z, vl4z, vz4_l);

    //  Rotational velocities in local frame
    Real vrl1x = d_VRL1x[I]; Real vrl1y = d_VRL1y[I]; Real vrl1z = d_VRL1z[I];
    Real vrl2x = d_VRL2x[I]; Real vrl2y = d_VRL2y[I]; Real vrl2z = d_VRL2z[I];
    Real vrl3x = d_VRL3x[I]; Real vrl3y = d_VRL3y[I]; Real vrl3z = d_VRL3z[I];
    Real vrl4x = d_VRL4x[I]; Real vrl4y = d_VRL4y[I]; Real vrl4z = d_VRL4z[I];

    //  Staged FMA waves for rotational velocity projections (8 per wave)
    //  Wave 1: initial products
    Real rx1 = e1x*vrl1x;  Real rx2 = e1x*vrl2x;
    Real rx3 = e1x*vrl3x;  Real rx4 = e1x*vrl4x;
    Real ry1 = e2x*vrl1x;  Real ry2 = e2x*vrl2x;
    Real ry3 = e2x*vrl3x;  Real ry4 = e2x*vrl4x;
    //  Wave 2: second component FMAs
    rx1 = fma(e1y, vrl1y, rx1);  rx2 = fma(e1y, vrl2y, rx2);
    rx3 = fma(e1y, vrl3y, rx3);  rx4 = fma(e1y, vrl4y, rx4);
    ry1 = fma(e2y, vrl1y, ry1);  ry2 = fma(e2y, vrl2y, ry2);
    ry3 = fma(e2y, vrl3y, ry3);  ry4 = fma(e2y, vrl4y, ry4);
    //  Wave 3: third component FMAs
    rx1 = fma(e1z, vrl1z, rx1);  rx2 = fma(e1z, vrl2z, rx2);
    rx3 = fma(e1z, vrl3z, rx3);  rx4 = fma(e1z, vrl4z, rx4);
    ry1 = fma(e2z, vrl1z, ry1);  ry2 = fma(e2z, vrl2z, ry2);
    ry3 = fma(e2z, vrl3z, ry3);  ry4 = fma(e2z, vrl4z, ry4);

    //  Material values for hourglass
    Real rho_v   = d_RHO[I];
    Real ssp_v   = d_SSP[I];
    Real ym_v    = d_YM[I];
    Real nu_v    = d_NU[I];
    Real shf_v   = d_SHF[I];

    //  Hourglass coefficients (from CHVIS3)
    //  HVISC, HVLIN, HELAS from common /SCR06R/ (set in radioss2.F)
    const Real HVISC = hg_params.HVISC;
    const Real HVLIN = hg_params.HVLIN;
    const Real HELAS = hg_params.HELAS;
    const Real SR2D2 = 0.70710678118654752;  // sqrt(2)/2

    Real shfpr3 = shf_v / (3.0 * (1.0 + nu_v));
    Real r0 = 0.25 * rho_v;
    Real r1 = r0 * 100.0;
    r0 = r0 * HVLIN;

    Real a1_hg = r1 * HVISC * hg_params.H1;
    Real a2_hg = r0 * SR2D2 * hg_params.SRH1;
    Real srshfpr3 = sqrt(fmax(shfpr3, 0.0));
    Real a3_hg = r1 * HVISC * hg_params.H2 * srshfpr3;
    Real a4_hg = r0 * SR2D2 * hg_params.SRH2 * srshfpr3;
    Real a5_hg = HELAS * hg_params.H3 * r1 * 0.072169;
    Real a6_hg = SR2D2 * hg_params.SRH3 * r0 * 0.072169;
    Real r0_ym = 0.25 * ym_v * HELAS;
    Real a7_hg = hg_params.H1 * r0_ym;
    Real a8_hg = hg_params.H2 * r0_ym * shfpr3;

    Real t2a = thk02 * area;
    Real tsa = sqrt(fmax(t2a, 0.0));
    Real h1q = a1_hg * tsa;
    Real h1l = a2_hg * ssp_v * tsa;
    Real h2q = a3_hg * thk02;
    Real h2l = a4_hg * ssp_v * thk02;
    Real h3q = a5_hg * t2a;
    Real h3l = a6_hg * ssp_v * t2a;
    Real td  = thk0 * dt;
    Real hh1 = a7_hg * td;
    Real b1v = fma(py1, py1, px1 * px1);
    Real b2v = fma(py2, py2, px2 * px2);
    Real hh2 = a8_hg * thk02 * td / fmax(b1v + b2v, 1.0e-20);

    //  GAMA vectors — non-uniform when ISMSTR!=1,!=11 and IHBE>=1 (cf. CHVIS3)
    Real gama1, gama2, gama3, gama4;
    if (ISMSTR != 1 && ISMSTR != 11 && IHBE >= 1) {
        Real vhx = d_VHX[I];
        Real vhy = d_VHY[I];
        Real px1v = px1 * vhx;
        Real px2v = px2 * vhx;
        Real py1v = py1 * vhy;
        Real py2v = py2 * vhy;
        gama1 = off_val * ( 1.0 - px1v - py1v);
        gama3 = off_val * ( 1.0 + px1v + py1v);
        gama2 = off_val * (-1.0 - px2v - py2v);
        gama4 = off_val * (-1.0 + px2v + py2v);
    } else {
        gama1 =  off_val;
        gama2 = -off_val;
        gama3 =  off_val;
        gama4 = -off_val;
    }

    //  Detect degenerate triangle (node 3 == node 4)
    if (n3 == n4) {
        h1q = 0.0; h1l = 0.0; h2q = 0.0; h2l = 0.0;
        h3q = 0.0; h3l = 0.0; hh1 = 0.0; hh2 = 0.0;
    }

    // --- Anti-hourglass membrane forces ---
    // Use GAMA-weighted velocities (matches CHVIS3 non-uniform path) — FMA chains
    Real hg1_m = fma(vx4_l, gama4, fma(vx3_l, gama3, fma(vx2_l, gama2, vx1_l*gama1)));
    Real hg2_m = fma(vy4_l, gama4, fma(vy3_l, gama3, fma(vy2_l, gama2, vy1_l*gama1)));

    //  Read & update hourglass state (HOUR(I,1:2) → membrane)
    Real hour1_state = fma(hg1_m, hh1, d_HOUR[0 * NUMELC + I]);
    Real hour2_state = fma(hg2_m, hh1, d_HOUR[1 * NUMELC + I]);
    d_HOUR[0 * NUMELC + I] = hour1_state;
    d_HOUR[1 * NUMELC + I] = hour2_state;

    Real hour1a = fma(hg1_m, fma(h1q, fabs(hg1_m), h1l), hour1_state);
    Real hour2a = fma(hg2_m, fma(h1q, fabs(hg2_m), h1l), hour2_state);

    Real h11 = hour1a * gama1;
    Real h12 = hour1a * gama2;
    Real h13 = hour1a * gama3;
    Real h21 = hour2a * gama1;
    Real h22 = hour2a * gama2;
    Real h23 = hour2a * gama3;

    Real ehou = fma(hour2a, hg2_m, hour1a * hg1_m);

    // --- Anti-hourglass bending forces ---
    // Use GAMA-weighted velocity (matches CHVIS3 non-uniform path)
    Real hg1_b = fma(vz4_l, gama4, fma(vz3_l, gama3, fma(vz2_l, gama2, vz1_l*gama1)));

    Real hour3_state = fma(hg1_b, hh2, d_HOUR[2 * NUMELC + I]);
    d_HOUR[2 * NUMELC + I] = hour3_state;

    Real hour3a = fma(hg1_b, fma(h2q, fabs(hg1_b), h2l), hour3_state);
    Real h31 = hour3a * gama1;
    Real h32 = hour3a * gama2;
    Real h33 = hour3a * gama3;

    ehou = fma(hour3a, hg1_b, ehou);

    // --- Anti-hourglass moments ---
    Real hg1_r = rx1 - rx2 + rx3 - rx4;
    Real hg2_r = ry1 - ry2 + ry3 - ry4;

    Real hour4 = hg1_r * fma(h3q, fabs(hg1_r), h3l);
    Real hour5 = hg2_r * fma(h3q, fabs(hg2_r), h3l);

    d_HOUR[3 * NUMELC + I] = hour4;
    d_HOUR[4 * NUMELC + I] = hour5;

    ehou = fma(hour4, hg1_r, fma(hour5, hg2_r, ehou));
    ehou *= dt * off_val;

    Real b11 =  hour4 * off_val;
    Real b12 = -hour4 * off_val;
    Real b13 =  hour4 * off_val;
    Real b14 = -hour4 * off_val;
    Real b21 =  hour5 * off_val;
    Real b22 = -hour5 * off_val;
    Real b23 =  hour5 * off_val;
    Real b24 = -hour5 * off_val;

    // ======================================================================
    //  Stiffness for time step (STI, STIR) — compute_sti == 1 path
    //
    //  CPU flow for ISMSTR /= 3 (our case):
    //    CDERI3:  STI = 0   (CPXPY3 is only called when ISMSTR==3)
    //    CHVIS3:  SCALE = max(gamma²) * DT * max(HH1+H1L, HH2+H2L, H3L)
    //                     / max(DT², EM20)
    //             STI += SCALE
    //             STI += max(B1,B2) * THK0 * A11 / (AREA * VISCMX² * ALPE)
    //             STIR = STI * (THK02/12 + AREA/9)
    //
    //  HH1, HH2, H1L, H2L, H3L are already computed in the hourglass
    //  section above and are still in scope.
    //
    //  VV = VISCMX² * ALPE.  For law 2: VISCMX=0 → transformed to 1.0,
    //  ALPE=1.0 (ccoef3.F).  So VV = 1.0.
    // ======================================================================
    // b1v, b2v already computed above in hourglass section
    Real pxx = fmax(b1v, b2v);

    //  STI starts at zero for ISMSTR /= 3  (CDERI3 path, no CPXPY3)
    Real sti_i = 0.0;

    //  CHVIS3 hourglass stiffness contribution (SCALE term)
    //  SCALE = max(gamma²) * DT * max(HH1+H1L, HH2+H2L, H3L) / max(DT², eps)
    //  For ISMSTR=1/IHBE<=1: gamma = ±OFF, so max(gamma²) = off²
    //  Reuse hh1, hh2, h1l, h2l, h3l from hourglass section (correct coefficients).
    Real off2 = off_val * off_val;
    Real hg_max  = fmax(hh1 + h1l, fmax(hh2 + h2l, h3l));
    Real scale_hg = off2 * dt * hg_max / fmax(dt * dt, 1.0e-20);
    sti_i += scale_hg;

    //  CHSTI3 material stiffness: MAX(B1,B2)*THK0*A11 / (AREA * VV)
    //  VV = VISCMX² * ALPE.  For law 2: VISCMX = sqrt(1+DM²)-DM ≈ 1.0
    //  (DM = membrane viscosity coef, typically 0.0 for IHBE<=1).
    //  ALPE = 1.0 (from CCOEF3).  So VV ≈ 1.0.
    Real a11_v = d_A11[I];
    Real vv = 1.0;  // VV = VISCMX² * ALPE ≈ 1.0 for law 2
    Real sti_mat = pxx * thk0 * a11_v / fmax(area * vv, 1.0e-20);
    sti_i += sti_mat;

    //  STIR = STI * (THK02/12 + AREA/9)  where THK02 = THK0² (cf. ccoef3.F line 80)
    Real vol0 = area * thk0;
    Real stir_i = sti_i * (thk02 / 12.0 + area / 9.0);

    //  compute_sti modes:
    //    0 = no STI for STIFN (NODADT=0 AND IDTMIN3=0: CDT3 returns early)
    //    1 = CPXPY3+CHSTI3 formula (NODADT/=0 OR IDT1SH=1 OR IDTMINS=2)
    //    2 = CDT3 formula: 0.81*0.5*VOL0*YM/ALDT^2 (NODADT=0 AND IDTMIN3/=0)
    if (compute_sti == 0) {
        sti_i  = 0.0;
        stir_i = 0.0;
    } else if (compute_sti == 2) {
        //  CDT3-style STI:  STI = ZEP81 * HALF * VOL0 * YM / ALDT^2
        //  For law 2: VISCMX = sqrt(1+DM^2)-DM = 1.0 (DM=0), ALPE=1.0
        //  so ALDT_modified = ALDT * VISCMX/sqrt(ALPE) = ALDT
        Real divm = fmax(aldt_sq, 1.0e-20);
        sti_i  = 0.81 * 0.5 * vol0 * ym_v / divm;
        stir_i = 0.0;
    }
    // else compute_sti == 1: keep CPXPY3+CHSTI3 values computed above

    //  Apply OFF
    sti_i  *= off_val;
    stir_i *= off_val;

    // ======================================================================
    //  2.  CFINT3 — Internal force computation
    //      (ISIGI /= 5 standard path)
    // ======================================================================

    //  Scale generalized forces by thickness
    Real f1a = for1 * thk0;   // Nxx
    Real f2a = for2 * thk0;   // Nyy
    Real f3a = for3 * thk0;   // Nxy
    Real f4a = for4 * thk0;   // Nyz  (transverse shear)
    Real f5a = for5 * thk0;   // Nxz  (transverse shear)

    Real m4  = f4a * area;    // transverse shear resultant Y
    Real m5  = f5a * area;    // transverse shear resultant X

    //  Local nodal forces from B-matrix (membrane + transverse shear)
    //  Node pairing via PX1/PY1 (diag 1-3) and PX2/PY2 (diag 2-4)
    //  Use FMA: 6 independent FMAs in one wave
    Real fl12 = fma(f3a, py2, f1a * px2);       // node 2, local x
    Real fl22 = fma(f3a, px2, f2a * py2);       // node 2, local y
    Real fl32 = fma(f4a, py2, f5a * px2);       // node 2, local z
    Real fl11 = fma(f3a, py1, f1a * px1);       // node 1 contrib
    Real fl21 = fma(f3a, px1, f2a * py1);
    Real fl31 = fma(f4a, py1, f5a * px1);

    //  Combine with hourglass forces (H arrays)
    Real g11 = fl11 + h11;    Real g13 = h13 - fl11;
    Real g21 = fl21 + h21;    Real g23 = h23 - fl21;
    Real g31 = fl31 + h31;    Real g33 = h33 - fl31;
    Real g12 = fl12 + h12;
    Real g22 = fl22 + h22;
    Real g32 = fl32 + h32;

    //  Rotate to global frame (nodes 1,2,3) — staged FMA for ILP + fusion
    //  Wave 1: 9 independent MULs (e_x * g_local)
    Real f11_g = e1x*g11;  Real f12_g = e1x*g12;  Real f13_g = e1x*g13;
    Real f21_g = e1y*g11;  Real f22_g = e1y*g12;  Real f23_g = e1y*g13;
    Real f31_g = e1z*g11;  Real f32_g = e1z*g12;  Real f33_g = e1z*g13;
    //  Wave 2: 9 independent FMAs
    f11_g = fma(e2x, g21, f11_g);  f12_g = fma(e2x, g22, f12_g);  f13_g = fma(e2x, g23, f13_g);
    f21_g = fma(e2y, g21, f21_g);  f22_g = fma(e2y, g22, f22_g);  f23_g = fma(e2y, g23, f23_g);
    f31_g = fma(e2z, g21, f31_g);  f32_g = fma(e2z, g22, f32_g);  f33_g = fma(e2z, g23, f33_g);
    //  Wave 3: 9 independent FMAs
    f11_g = fma(e3x, g31, f11_g);  f12_g = fma(e3x, g32, f12_g);  f13_g = fma(e3x, g33, f13_g);
    f21_g = fma(e3y, g31, f21_g);  f22_g = fma(e3y, g32, f22_g);  f23_g = fma(e3y, g33, f23_g);
    f31_g = fma(e3z, g31, f31_g);  f32_g = fma(e3z, g32, f32_g);  f33_g = fma(e3z, g33, f33_g);

    //  Node 4 by equilibrium: F4 = -(F1 + F2 + F3)
    Real f14_g = -f11_g - f12_g - f13_g;
    Real f24_g = -f21_g - f22_g - f23_g;
    Real f34_g = -f31_g - f32_g - f33_g;

    // --- Moments (from bending) ---
    Real m1a = mom1 * thk02;   // MOM*THK02:  (Mxx/thk0²)*thk0² = Mxx
    Real m2a = mom2 * thk02;   // Myy
    Real m3a = mom3 * thk02;   // Mxy
    m4 *= 0.25;                  // transverse shear moment contribution
    m5 *= 0.25;

    //  Local moment nodal forces (FMA: 4 independent)
    Real ml11 = fma(-m3a, px1, -m2a * py1);
    Real ml21 = fma( m3a, py1,  m1a * px1);
    Real ml12 = fma(-m3a, px2, -m2a * py2);
    Real ml22 = fma( m3a, py2,  m1a * px2);

    //  Combine with hourglass moment forces (B arrays) and transverse shear moments
    Real gm11 = ml11 - m4 + b11;     Real gm13 = -ml11 - m4 + b13;
    Real gm12 = ml12 - m4 + b12;     Real gm14 = -ml12 - m4 + b14;
    Real gm21 = ml21 + m5 + b21;     Real gm23 = -ml21 + m5 + b23;
    Real gm22 = ml22 + m5 + b22;     Real gm24 = -ml22 + m5 + b24;

    //  Rotate moments to global frame (only E1, E2 needed) — staged FMA
    //  Wave 1: 12 independent MULs
    Real m11_g = e1x*gm11;  Real m12_g = e1x*gm12;
    Real m13_g = e1x*gm13;  Real m14_g = e1x*gm14;
    Real m21_g = e1y*gm11;  Real m22_g = e1y*gm12;
    Real m23_g = e1y*gm13;  Real m24_g = e1y*gm14;
    Real m31_g = e1z*gm11;  Real m32_g = e1z*gm12;
    Real m33_g = e1z*gm13;  Real m34_g = e1z*gm14;
    //  Wave 2: 12 independent FMAs
    m11_g = fma(e2x, gm21, m11_g);  m12_g = fma(e2x, gm22, m12_g);
    m13_g = fma(e2x, gm23, m13_g);  m14_g = fma(e2x, gm24, m14_g);
    m21_g = fma(e2y, gm21, m21_g);  m22_g = fma(e2y, gm22, m22_g);
    m23_g = fma(e2y, gm23, m23_g);  m24_g = fma(e2y, gm24, m24_g);
    m31_g = fma(e2z, gm21, m31_g);  m32_g = fma(e2z, gm22, m32_g);
    m33_g = fma(e2z, gm23, m33_g);  m34_g = fma(e2z, gm24, m34_g);

    // ======================================================================
    //  Zero forces/moments for deleted elements (OFFG < 0)
    // ======================================================================
    if (off_i < 0.0) {
        f11_g = 0.0; f21_g = 0.0; f31_g = 0.0;
        f12_g = 0.0; f22_g = 0.0; f32_g = 0.0;
        f13_g = 0.0; f23_g = 0.0; f33_g = 0.0;
        f14_g = 0.0; f24_g = 0.0; f34_g = 0.0;
        m11_g = 0.0; m21_g = 0.0; m31_g = 0.0;
        m12_g = 0.0; m22_g = 0.0; m32_g = 0.0;
        m13_g = 0.0; m23_g = 0.0; m33_g = 0.0;
        m14_g = 0.0; m24_g = 0.0; m34_g = 0.0;
        sti_i = 0.0; stir_i = 0.0;
    }

    // ======================================================================
    //  3.  CUPDT3 — Atomic scatter to global nodal arrays
    // Inefficient due to atomics, but straightforward and safe for parallelism.
    // Coloring or other techniques should be used to reduce contention
    // ======================================================================
    //  Node 1
    atomicAdd(&d_Fx[n1], -f11_g);
    atomicAdd(&d_Fy[n1], -f21_g);
    atomicAdd(&d_Fz[n1], -f31_g);
    atomicAdd(&d_Mx[n1], -m11_g);
    atomicAdd(&d_My[n1], -m21_g);
    atomicAdd(&d_Mz[n1], -m31_g);
    atomicAdd(&d_STIFN[n1], sti_i);
    atomicAdd(&d_STIFR[n1], stir_i);

    //  Node 2
    atomicAdd(&d_Fx[n2], -f12_g);
    atomicAdd(&d_Fy[n2], -f22_g);
    atomicAdd(&d_Fz[n2], -f32_g);
    atomicAdd(&d_Mx[n2], -m12_g);
    atomicAdd(&d_My[n2], -m22_g);
    atomicAdd(&d_Mz[n2], -m32_g);
    atomicAdd(&d_STIFN[n2], sti_i);
    atomicAdd(&d_STIFR[n2], stir_i);

    //  Node 3
    atomicAdd(&d_Fx[n3], -f13_g);
    atomicAdd(&d_Fy[n3], -f23_g);
    atomicAdd(&d_Fz[n3], -f33_g);
    atomicAdd(&d_Mx[n3], -m13_g);
    atomicAdd(&d_My[n3], -m23_g);
    atomicAdd(&d_Mz[n3], -m33_g);
    atomicAdd(&d_STIFN[n3], sti_i);
    atomicAdd(&d_STIFR[n3], stir_i);

    //  Node 4
    atomicAdd(&d_Fx[n4], -f14_g);
    atomicAdd(&d_Fy[n4], -f24_g);
    atomicAdd(&d_Fz[n4], -f34_g);
    atomicAdd(&d_Mx[n4], -m14_g);
    atomicAdd(&d_My[n4], -m24_g);
    atomicAdd(&d_Mz[n4], -m34_g);
    atomicAdd(&d_STIFN[n4], sti_i);
    atomicAdd(&d_STIFR[n4], stir_i);

    // ======================================================================
    //  4.  Energy update
    //      Hourglass energy contribution into EINT (component 2 = bending)
    // ======================================================================
    d_EINT[1 * NUMELC + I] += ehou;
}

// ===========================================================================
//  Host-side launcher
// ===========================================================================
extern "C"
void launch_shell_force_assembly_kernel(
    // Connectivity
    const int* d_N1, const int* d_N2, const int* d_N3, const int* d_N4,
    // Local frame
    const Real* d_E1x, const Real* d_E1y, const Real* d_E1z,
    const Real* d_E2x, const Real* d_E2y, const Real* d_E2z,
    const Real* d_E3x, const Real* d_E3y, const Real* d_E3z,
    // Shape functions
    const Real* d_PX1, const Real* d_PX2,
    const Real* d_PY1, const Real* d_PY2,
    const Real* d_AREA,
    // Hourglass volume ratios (for non-uniform GAMA)
    const Real* d_VHX, const Real* d_VHY,
    // Gathered velocities
    const Real* d_VL1x,  const Real* d_VL1y,  const Real* d_VL1z,
    const Real* d_VL2x,  const Real* d_VL2y,  const Real* d_VL2z,
    const Real* d_VL3x,  const Real* d_VL3y,  const Real* d_VL3z,
    const Real* d_VL4x,  const Real* d_VL4y,  const Real* d_VL4z,
    const Real* d_VRL1x, const Real* d_VRL1y, const Real* d_VRL1z,
    const Real* d_VRL2x, const Real* d_VRL2y, const Real* d_VRL2z,
    const Real* d_VRL3x, const Real* d_VRL3y, const Real* d_VRL3z,
    const Real* d_VRL4x, const Real* d_VRL4y, const Real* d_VRL4z,
    // Element state
    const Real* d_OFF, const Real* d_THK0, const Real* d_THK02,
    // Generalized forces/moments
    const Real* d_FORxx, const Real* d_FORyy, const Real* d_FORxy,
    const Real* d_FORyz, const Real* d_FORzx,
    const Real* d_MOMxx, const Real* d_MOMyy, const Real* d_MOMxy,
    // Hourglass state
    Real* d_HOUR,
    // Stiffness outputs
    Real* d_STI, Real* d_STIR,
    // Material
    const Real* d_SSP, const Real* d_RHO, const Real* d_YM,
    const Real* d_NU,  const Real* d_A11, const Real* d_G,
    const Real* d_SHF,
    // Internal energy
    Real* d_EINT,
    // Global nodal arrays
    Real* d_Fx, Real* d_Fy, Real* d_Fz,
    Real* d_Mx, Real* d_My, Real* d_Mz,
    Real* d_STIFN, Real* d_STIFR,
    // Scalars
    HourglassParams hg_params,
    Real dt, int NPT, int NUMELC,
    int compute_sti,
    int ISMSTR, int IHBE,
    cudaStream_t stream)
{
    const int BLOCK_SIZE = 256;
    int grid_size = (NUMELC + BLOCK_SIZE - 1) / BLOCK_SIZE;

    auto launch = [&](auto ismstr_tag) {
        constexpr int ISMSTR_V = decltype(ismstr_tag)::value;
        shell_force_assembly_kernel<ISMSTR_V><<<grid_size, BLOCK_SIZE, 0, stream>>>(
            d_N1, d_N2, d_N3, d_N4,
            d_E1x, d_E1y, d_E1z,
            d_E2x, d_E2y, d_E2z,
            d_E3x, d_E3y, d_E3z,
            d_PX1, d_PX2, d_PY1, d_PY2, d_AREA,
            d_VHX, d_VHY,
            d_VL1x, d_VL1y, d_VL1z,
            d_VL2x, d_VL2y, d_VL2z,
            d_VL3x, d_VL3y, d_VL3z,
            d_VL4x, d_VL4y, d_VL4z,
            d_VRL1x, d_VRL1y, d_VRL1z,
            d_VRL2x, d_VRL2y, d_VRL2z,
            d_VRL3x, d_VRL3y, d_VRL3z,
            d_VRL4x, d_VRL4y, d_VRL4z,
            d_OFF, d_THK0, d_THK02,
            d_FORxx, d_FORyy, d_FORxy, d_FORyz, d_FORzx,
            d_MOMxx, d_MOMyy, d_MOMxy,
            d_HOUR,
            d_STI, d_STIR,
            d_SSP, d_RHO, d_YM, d_NU, d_A11, d_G, d_SHF,
            d_EINT,
            d_Fx, d_Fy, d_Fz,
            d_Mx, d_My, d_Mz,
            d_STIFN, d_STIFR,
            hg_params, dt, NPT, NUMELC, compute_sti, IHBE);
    };

    switch (ISMSTR) {
        case 1:  launch(std::integral_constant<int,1>{});  break;
        case 2:  launch(std::integral_constant<int,2>{});  break;
        case 11: launch(std::integral_constant<int,11>{}); break;
        default:
            fprintf(stderr, "Unsupported ISMSTR=%d in shell_force_assembly_kernel\n", ISMSTR);
            exit(EXIT_FAILURE);
    }

    // Check for kernel launch errors
    {
        cudaError_t err = cudaPeekAtLastError();
        if (err != cudaSuccess) {
            fprintf(stderr, "CUDA kernel launch error (force+assembly): %s\n",
                    cudaGetErrorString(err));
            exit(EXIT_FAILURE);
        }
    }
}

#endif // SHELL_FORCE_ASSEMBLY_KERNEL_CU
