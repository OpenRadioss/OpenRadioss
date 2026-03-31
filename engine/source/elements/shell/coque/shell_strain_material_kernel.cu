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
//  OpenRadioss GPU — Kernel 2 (Option C): Strains + Material
//  Covers: CCOEF3  — material / geometry coefficients (simplified, uniform mat)
//          CDEFO3  — membrane strain rates         (IHBE <= 1)
//          CCURV3  — curvature rates (bending)
//          CSTRA3  — strain increments & accumulation
//          CMAIN3 / SIGEPS02C / M2CPLR — through-thickness material integration
//                   (Johnson-Cook law 02, radial-return plasticity)
//
//  Scope: ISMSTR=1, IHBE<=1, ISHFRAM=0, MTN=2 (Johnson-Cook)
//         One thread per element, flat SoA layout.
// ==============================================================================

#ifndef SHELL_STRAIN_MATERIAL_KERNEL_CU
#define SHELL_STRAIN_MATERIAL_KERNEL_CU

#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cfloat>
#include <type_traits>
#include "real_type.h"

// ===========================================================================
//  Johnson-Cook material parameters — passed via constant memory or struct
// ===========================================================================
struct JohnsonCookParams {
    Real E;           // Young's modulus
    Real nu;          // Poisson's ratio
    Real G;           // Shear modulus  E/(2(1+nu))
    Real A11;         // plane-stress stiffness  E/(1-nu^2)
    Real A12;         // coupling  nu*A11
    Real CA;          // J-C param A  (initial yield)
    Real CB;          // J-C param B  (hardening modulus)
    Real CN;          // J-C param n  (hardening exponent)
    Real CC;          // J-C param C  (strain-rate sensitivity)
    Real EPDR;        // reference strain rate  (before dt scaling)
    Real EPMX;        // max plastic strain for ductile rupture
    Real YMAX;        // yield stress cap
    Real M_EXP;       // thermal exponent m
    Real FISOKIN;     // isotropic/kinematic ratio (1=iso, 0=kin)
    Real RHOCP;       // rho * Cp  (adiabatic heating)
    Real TREF;        // reference temperature
    Real TMELT;       // melting temperature
    Real ASRATE;      // strain-rate filter coefficient
    Real RHO;         // density
    Real SSP;         // sound speed
    Real SHF_COEF;    // shear factor (from GEO, typically 5/6)
    int    IPLA;        // plasticity algorithm (0,1,2)
    int    VP;          // strain-rate mode (1=plastic, 2=total, 3=deviatoric)
    int    IFORM;       // 0=Johnson-Cook, 1=Zerilli-Armstrong
    int    ICC;         // Cowper-Symonds flag
    Real Z3, Z4;      // Zerilli-Armstrong params (zero for J-C)
};

// ===========================================================================
//  Device helper: M2CPLR-equivalent radial-return for a SINGLE integration pt
//  Implements IPLA=0 (simple radial), IPLA=1 (plane-stress Newton, 3 iter),
//  and IPLA=2 (normal projection + radial return).
//  Kinematic hardening included when FISOKIN > 0.
// ===========================================================================
template <int IPLA>
__device__ __forceinline__ void m2cplr_device(
    // Material constants
    Real A1, Real A2, Real G_mod, Real GS_val,
    Real nu, Real E_mod,
    Real CA_eff, Real CB_eff, Real CN, Real YMAX_eff,
    Real CC_val, Real EPDR_scaled, Real FISOKIN,
    // Strain increments (combined membrane + bending)
    Real depsxx, Real depsyy, Real depsxy, Real depsyz, Real depszx,
    // Old stress (input)
    Real sigoxx, Real sigoyy, Real sigoxy, Real sigoyz, Real sigozx,
    // Backstress (in/out)
    Real &sigbakxx, Real &sigbakyy, Real &sigbakxy,
    // Strain rate * dt
    Real epsdot,
    // State (in/out)
    Real &pla, Real off,
    // Outputs
    Real &signxx, Real &signyy, Real &signxy, Real &signyz, Real &signzx,
    Real &dpla, Real &ezz, Real &yld_out, Real &hardm, Real &etse)
{
    const Real SMALL = 1.0e-7;
    const int NMAX = 3;
    Real H_val = 0.0;

    // --- Initialize new stress = old stress ---
    signxx = sigoxx;
    signyy = sigoyy;
    signxy = sigoxy;
    signyz = sigoyz;
    signzx = sigozx;
    etse   = 1.0;

    // --- Kinematic hardening: subtract backstress ---
    if (FISOKIN > 0.0) {
        signxx -= sigbakxx;
        signyy -= sigbakyy;
        signxy -= sigbakxy;
    }

    // --- Elastic predictor ---
    signxx += A1 * depsxx + A2 * depsyy;
    signyy += A2 * depsxx + A1 * depsyy;
    signxy += G_mod * depsxy;
    signyz += GS_val * depsyz;
    signzx += GS_val * depszx;

    // --- Strain rate effect on yield parameters ---
    Real ca_loc = CA_eff;
    Real cb_loc = CB_eff;
    Real ymax_loc = YMAX_eff;

    // (logep computed from epsdot already scaled by dt)
    Real logep = 0.0;
    if (CC_val != 0.0) {
        Real epsp_use = fmax(epsdot, EPDR_scaled);
        logep = log(epsp_use / EPDR_scaled);
        // Johnson-Cook strain-rate + thermal is already folded into CA_eff, CB_eff externally
        // but for IFORM=0 (J-C) we apply the (1 + C*ln) factor here
        Real q = (1.0 + CC_val * logep);
        q = fmax(q, 1.0e-20);
        ca_loc *= q;
        cb_loc *= q;
    }

    // --- Yield stress ---
    dpla = 0.0;
    ezz  = 0.0;
    Real yld;
    if (pla == 0.0) {
        yld = ca_loc;
    } else {
        Real beta = cb_loc * (1.0 - FISOKIN);
        yld = ca_loc + beta * pow(pla, CN);
    }
    yld = fmin(yld, ymax_loc);
    yld_out = yld;

    // =================================================================
    //  IPLA = 0: Simple radial return
    // =================================================================
    if (IPLA == 0) {
        Real svm = sqrt(signxx * signxx + signyy * signyy - signxx * signyy + 3.0 * signxy * signxy);
        Real r = fmin(1.0, yld / (svm + 1.0e-15));
        if (r < 1.0) {
            signxx *= r;
            signyy *= r;
            signxy *= r;
            dpla = off * fmax(0.0, (svm - yld) / E_mod);
            Real s1 = 0.5 * (signxx + signyy);
            ezz = dpla * s1 / yld;
            pla += dpla;
            if (yld >= ymax_loc)
                H_val = 0.0;
            else
                H_val = CN * cb_loc * pow(pla + SMALL, CN - 1.0);
            etse = H_val / (H_val + E_mod);
        }
    }
    // =================================================================
    //  IPLA = 1: Plane-stress corrected Newton iteration (3 iterations)
    // =================================================================
    else if (IPLA == 1) {
        Real s1_val = signxx + signyy;
        Real s2_val = signxx - signyy;
        Real s3_val = signxy;
        Real Av = 0.25 * s1_val * s1_val;
        Real Bv = 0.75 * s2_val * s2_val + 3.0 * s3_val * s3_val;
        Real svm = sqrt(Av + Bv);

        if (svm > yld && off == 1.0) {
            Real nu1 = 1.0 / (1.0 - nu);
            Real nu2 = 1.0 / (1.0 + nu);
            if (yld >= ymax_loc)
                H_val = 0.0;
            else
                H_val = CN * cb_loc * pow(pla + SMALL, CN - 1.0);
            etse = H_val / (H_val + E_mod);

            Real dpla_j = (svm - yld) / (3.0 * G_mod + H_val);
            Real Anu1 = Av * nu1;
            Real Bnu2 = 3.0 * Bv * nu2;
            Real H2 = 2.0 * H_val;

            Real P_val, Q_val;

            if (FISOKIN == 0.0) {
                // Pure isotropic hardening path
                for (int n = 0; n < NMAX; n++) {
                    Real dpla_i = dpla_j;
                    Real pla_i = pla + dpla_i;
                    dpla = dpla_j;
                    Real yld_i;
                    if (pla_i == 0.0)
                        yld_i = fmin(ymax_loc, ca_loc);
                    else
                        yld_i = fmin(ymax_loc, ca_loc + cb_loc * pow(pla_i, CN));
                    Real DR = 0.5 * E_mod * dpla_i / yld_i;
                    P_val = 1.0 / (1.0 + DR * nu1);
                    Q_val = 1.0 / (1.0 + 3.0 * DR * nu2);
                    Real P2 = P_val * P_val;
                    Real Q2 = Q_val * Q_val;
                    Real F_val = Av * P2 + Bv * Q2 - yld_i * yld_i;
                    Real DF_val = -(Anu1 * P2 * P_val + Bnu2 * Q2 * Q_val)
                                    * (E_mod - DR * H2) / yld_i
                                    - H2 * yld_i;
                    if (dpla_i > 0.0)
                        dpla_j = fmax(0.0, dpla_i - F_val / DF_val);
                    else
                        dpla_j = 0.0;
                }
            } else {
                // Mixed isotropic/kinematic hardening
                Real beta_h = H_val * FISOKIN;
                Real HI = H_val - beta_h;
                Real HK = (2.0 / 3.0) * beta_h;
                Real aaa = 3.0 * HK / E_mod;
                Real nu11 = nu1 + aaa;
                Real nu21 = 3.0 * nu2 + aaa;
                Anu1 = Av * nu11;
                Bnu2 = Bv * nu21;
                H2 = 2.0 * HI;

                for (int n = 0; n < NMAX; n++) {
                    Real dpla_i = dpla_j;
                    Real pla_i = pla + dpla_i;
                    dpla = dpla_j;
                    Real beta = 1.0 - FISOKIN;
                    Real yld_i;
                    if (pla_i == 0.0)
                        yld_i = fmin(ymax_loc, ca_loc);
                    else
                        yld_i = fmin(ymax_loc, ca_loc + beta * cb_loc * pow(pla_i, CN));
                    Real DR = 0.5 * E_mod * dpla_i / yld_i;
                    P_val = 1.0 / (1.0 + DR * nu11);
                    Q_val = 1.0 / (1.0 + DR * nu21);
                    Real P2 = P_val * P_val;
                    Real Q2 = Q_val * Q_val;
                    Real F_val = Av * P2 + Bv * Q2 - yld_i * yld_i;
                    Real DF_val = -(Anu1 * P2 * P_val + Bnu2 * Q2 * Q_val)
                                    * (E_mod - DR * H2) / yld_i
                                    - H2 * yld_i;
                    if (dpla_i > 0.0)
                        dpla_j = fmax(0.0, dpla_i - F_val / DF_val);
                    else
                        dpla_j = 0.0;
                }
            }

            // --- Plastically admissible stresses ---
            pla += dpla;
            Real S1 = (signxx + signyy) * P_val;
            Real S2 = (signxx - signyy) * Q_val;
            signxx = 0.5 * (S1 + S2);
            signyy = 0.5 * (S1 - S2);
            signxy = signxy * Q_val;
            Real DR_final = 0.5 * E_mod * dpla / yld;
            ezz = DR_final * S1 / E_mod;
        }
    }
    // =================================================================
    //  IPLA = 2: Normal projection + radial return
    // =================================================================
    else if (IPLA == 2) {
        Real svm2 = signxx * signxx + signyy * signyy
                      - signxx * signyy + 3.0 * signxy * signxy;
        Real svm = sqrt(svm2);
        Real yld2 = yld * yld;

        if (svm2 > yld2 && off == 1.0) {
            if (yld >= ymax_loc)
                H_val = 0.0;
            else
                H_val = CN * cb_loc * pow(pla + SMALL, CN - 1.0);
            etse = H_val / (H_val + E_mod);

            // Normal projection
            Real aa = (svm2 - yld2)
                        / (5.0 * svm2 + 3.0 * (-signxx * signyy + signxy * signxy));
            Real s1n = (1.0 - 2.0 * aa) * signxx + aa * signyy;
            Real s2n = aa * signxx + (1.0 - 2.0 * aa) * signyy;
            Real s3n = (1.0 - 3.0 * aa) * signxy;
            signxx = s1n;
            signyy = s2n;
            signxy = s3n;

            dpla = off * (svm - yld) / (3.0 * G_mod + H_val);
            pla += dpla;
            yld += H_val * dpla;

            // Radial return
            svm = sqrt(signxx * signxx + signyy * signyy
                       - signxx * signyy + 3.0 * signxy * signxy);
            Real r = fmin(1.0, yld / fmax(1.0e-20, svm));
            signxx *= r;
            signyy *= r;
            signxy *= r;
            ezz = dpla * 0.5 * (signxx + signyy) / yld;
        }
    }

    yld_out = yld;
    hardm = H_val;

    // --- Kinematic hardening: update backstress, re-add ---
    if (FISOKIN > 0.0) {
        Real hkin = FISOKIN * H_val;
        Real alpha = (yld > 0.0) ? hkin * dpla / yld : 0.0;
        sigbakxx += alpha * signxx;
        sigbakyy += alpha * signyy;
        sigbakxy += alpha * signxy;

        signxx += sigbakxx;
        signyy += sigbakyy;
        signxy += sigbakxy;
    }
}

// ===========================================================================
//  Kernel 2 — shell_strain_material_kernel
//
//  Reads outputs of Kernel 1 (local frame, velocities, shape functions).
//  Produces generalized forces FOR(5) and moments MOM(3), updated element
//  state (GSTR, THK, OFF, PLA, SIG, EPSD, SIGBAK, EINT …).
//
//  Thread mapping:  I = blockIdx.x * blockDim.x + threadIdx.x
// ===========================================================================
template <int IPLA, int NPT, int ISMSTR>
__global__ __launch_bounds__(256, 1) void shell_strain_material_kernel(
    // ---- Inputs from Kernel 1 (per-element, read-only) ----
    const Real* __restrict__ d_E1x,
    const Real* __restrict__ d_E1y,
    const Real* __restrict__ d_E1z,
    const Real* __restrict__ d_E2x,
    const Real* __restrict__ d_E2y,
    const Real* __restrict__ d_E2z,
    const Real* __restrict__ d_E3x,
    const Real* __restrict__ d_E3y,
    const Real* __restrict__ d_E3z,
    // Gathered translational velocities  (4 nodes × 3)
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
    // Gathered rotational velocities  (4 nodes × 3)
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
    // Shape-function derivatives & geometry from Kernel 1
    const Real* __restrict__ d_PX1,
    const Real* __restrict__ d_PX2,
    const Real* __restrict__ d_PY1,
    const Real* __restrict__ d_PY2,
    const Real* __restrict__ d_AREA,
    const Real* __restrict__ d_A_I,
    // Displacement increments (ISMSTR=11 only; zero otherwise)
    const Real* __restrict__ d_UX1,
    const Real* __restrict__ d_UY1,
    const Real* __restrict__ d_UX2,
    const Real* __restrict__ d_UY2,
    const Real* __restrict__ d_UX3,
    const Real* __restrict__ d_UY3,
    const Real* __restrict__ d_UX4,
    const Real* __restrict__ d_UY4,
    // ---- Element state (read/write) ----
    Real* __restrict__ d_OFF,         // [NUMELC]
    Real* __restrict__ d_THK,         // [NUMELC] current thickness
    const Real* __restrict__ d_THK0,  // [NUMELC] reference thickness
    Real* __restrict__ d_GSTR,        // [NUMELC*8] accumulated strains
    Real* __restrict__ d_EINT,        // [NUMELC*2] internal energy
    Real* __restrict__ d_EPSD_elem,   // [NUMELC]   element strain rate
    // ---- Per-integration-point state [NPT * NUMELC] each ----
    Real* __restrict__ d_SIGxx,
    Real* __restrict__ d_SIGyy,
    Real* __restrict__ d_SIGxy,
    Real* __restrict__ d_SIGyz,
    Real* __restrict__ d_SIGzx,
    Real* __restrict__ d_PLA,
    Real* __restrict__ d_EPSD_ip,
    Real* __restrict__ d_SIGBAKxx,
    Real* __restrict__ d_SIGBAKyy,
    Real* __restrict__ d_SIGBAKxy,
    Real* __restrict__ d_DPLA,
    Real* __restrict__ d_TEMPEL,      // [NPT*NUMELC] temperature
    // ---- Outputs: generalized forces/moments for Kernel 3 ----
    Real* __restrict__ d_FORxx,       // [NUMELC] FOR(1)= Nxx/thk
    Real* __restrict__ d_FORyy,       // [NUMELC] FOR(2)= Nyy/thk
    Real* __restrict__ d_FORxy,       // [NUMELC] FOR(3)= Nxy/thk
    Real* __restrict__ d_FORyz,       // [NUMELC] FOR(4)= Nyz/thk
    Real* __restrict__ d_FORzx,       // [NUMELC] FOR(5)= Nxz/thk
    Real* __restrict__ d_MOMxx,       // [NUMELC] MOM(1)= Mxx/thk^2
    Real* __restrict__ d_MOMyy,       // [NUMELC] MOM(2)= Myy/thk^2
    Real* __restrict__ d_MOMxy,       // [NUMELC] MOM(3)= Mxy/thk^2
    // Output SIGY (average yield)
    Real* __restrict__ d_SIGY,        // [NUMELC]
    // ---- Scalars & material ----
    JohnsonCookParams mat,
    Real dt,
    int    NUMELC,
    int    ITHK,                        // thickness update flag
    int    NCYCLE_DBG                    // cycle number for debug prints
)
{
    // ======================================================================
    //  Thread mapping
    // ======================================================================
    int I = blockIdx.x * blockDim.x + threadIdx.x;
    if (I >= NUMELC) return;

    // ======================================================================
    //  Load Kernel 1 outputs (all register-resident after this)
    // ======================================================================
    Real e1x = d_E1x[I]; Real e1y = d_E1y[I]; Real e1z = d_E1z[I];
    Real e2x = d_E2x[I]; Real e2y = d_E2y[I]; Real e2z = d_E2z[I];
    Real e3x = d_E3x[I]; Real e3y = d_E3y[I]; Real e3z = d_E3z[I];

    Real vl1x = d_VL1x[I]; Real vl1y = d_VL1y[I]; Real vl1z = d_VL1z[I];
    Real vl2x = d_VL2x[I]; Real vl2y = d_VL2y[I]; Real vl2z = d_VL2z[I];
    Real vl3x = d_VL3x[I]; Real vl3y = d_VL3y[I]; Real vl3z = d_VL3z[I];
    Real vl4x = d_VL4x[I]; Real vl4y = d_VL4y[I]; Real vl4z = d_VL4z[I];

    Real vrl1x = d_VRL1x[I]; Real vrl1y = d_VRL1y[I]; Real vrl1z = d_VRL1z[I];
    Real vrl2x = d_VRL2x[I]; Real vrl2y = d_VRL2y[I]; Real vrl2z = d_VRL2z[I];
    Real vrl3x = d_VRL3x[I]; Real vrl3y = d_VRL3y[I]; Real vrl3z = d_VRL3z[I];
    Real vrl4x = d_VRL4x[I]; Real vrl4y = d_VRL4y[I]; Real vrl4z = d_VRL4z[I];

    Real px1  = d_PX1[I];   Real px2  = d_PX2[I];
    Real py1  = d_PY1[I];   Real py2  = d_PY2[I];
    Real area = d_AREA[I];  Real a_i  = d_A_I[I];

    Real off_i  = d_OFF[I];
    Real thk_i  = d_THK[I];
    Real thk0_i = d_THK0[I];

    // ======================================================================
    //  1.  CCOEF3 — Material/geometry coefficients (simplified: uniform mat)
    // ======================================================================
    Real ym   = mat.E;
    Real nu_v = mat.nu;
    Real g_v  = mat.G;
    Real a11  = mat.A11;
    Real a12  = mat.A12;
    Real shf;

    Real thk02 = thk0_i * thk0_i;
    Real vol0  = thk0_i * area;

    // Shear factor: SHF logic from CCOEF3  (NPT==1 → shf=0, else from GEO)
    if (NPT == 1) {
        shf = 0.0;
    } else {
        shf = mat.SHF_COEF;
    }
    Real gs_val = g_v * shf;

    // ======================================================================
    //  2.  CDEFO3 — Membrane strain rates × area  (IHBE <= 1)
    // ======================================================================
    //  Project nodal velocities → local frame
    //  Staged FMA waves: 12 independent ops per wave to fill DP pipeline
    //  (6-cycle DP latency needs ≥6 independent insns; 12 per wave is ideal)

    //  Wave 1: initial products (12 independent DMULs)
    Real vx1 = e1x * vl1x;  Real vx2 = e1x * vl2x;
    Real vx3 = e1x * vl3x;  Real vx4 = e1x * vl4x;
    Real vy1 = e2x * vl1x;  Real vy2 = e2x * vl2x;
    Real vy3 = e2x * vl3x;  Real vy4 = e2x * vl4x;
    Real vz1 = e3x * vl1x;  Real vz2 = e3x * vl2x;
    Real vz3 = e3x * vl3x;  Real vz4 = e3x * vl4x;

    //  Wave 2: second component FMAs (12 independent, each depends on one Wave 1 result)
    vx1 = fma(e1y, vl1y, vx1);  vx2 = fma(e1y, vl2y, vx2);
    vx3 = fma(e1y, vl3y, vx3);  vx4 = fma(e1y, vl4y, vx4);
    vy1 = fma(e2y, vl1y, vy1);  vy2 = fma(e2y, vl2y, vy2);
    vy3 = fma(e2y, vl3y, vy3);  vy4 = fma(e2y, vl4y, vy4);
    vz1 = fma(e3y, vl1y, vz1);  vz2 = fma(e3y, vl2y, vz2);
    vz3 = fma(e3y, vl3y, vz3);  vz4 = fma(e3y, vl4y, vz4);

    //  Wave 3: third component FMAs (12 independent, each depends on one Wave 2 result)
    vx1 = fma(e1z, vl1z, vx1);  vx2 = fma(e1z, vl2z, vx2);
    vx3 = fma(e1z, vl3z, vx3);  vx4 = fma(e1z, vl4z, vx4);
    vy1 = fma(e2z, vl1z, vy1);  vy2 = fma(e2z, vl2z, vy2);
    vy3 = fma(e2z, vl3z, vy3);  vy4 = fma(e2z, vl4z, vy4);
    vz1 = fma(e3z, vl1z, vz1);  vz2 = fma(e3z, vl2z, vz2);
    vz3 = fma(e3z, vl3z, vz3);  vz4 = fma(e3z, vl4z, vz4);

    Real vz13 = vz1 - vz3;
    Real vz24 = vz2 - vz4;

    //  Transverse shear strain rates × area
    Real eyz = py1 * vz13 + py2 * vz24;
    Real exz = px1 * vz13 + px2 * vz24;

    //  IHBE <= 1: Z2 set to zero, membrane warping correction
    Real dt1v4 = 0.25 * dt;

    Real tmp2a = py2 + py1;
    Real tmp3a = copysign(fmax(fabs(tmp2a), 1.0e-20), tmp2a);
    Real tmp1a = dt1v4 * (vz13 - vz24) * (vz13 - vz24) / tmp3a;

    Real vx13 = vx1 - vx3 - tmp1a;
    Real vx24 = vx2 - vx4 + tmp1a;

    Real exx = px1 * vx13 + px2 * vx24;  // ε̇_xx × area
    Real exy = py1 * vx13 + py2 * vx24;  // partial γ̇_xy × area

    Real tmp1b = px2 - px1;
    Real tmp3b = copysign(fmax(fabs(tmp1b), 1.0e-20), tmp1b);
    Real tmp2b = dt1v4 * (vz13 + vz24) * (vz13 + vz24) / tmp3b;

    Real vy13 = vy1 - vy3 + tmp2b;
    Real vy24 = vy2 - vy4 + tmp2b;

    exy += px1 * vy13 + px2 * vy24;        // complete γ̇_xy × area
    Real eyy = py1 * vy13 + py2 * vy24;  // ε̇_yy × area

    // ======================================================================
    //  3.  CCURV3 — Curvature rates × area
    // ======================================================================
    //  Project rotational velocities → local frame (staged FMA waves, 8 per wave)
    //  Wave 1: initial products
    Real rx1 = e1x * vrl1x;  Real rx2 = e1x * vrl2x;
    Real rx3 = e1x * vrl3x;  Real rx4 = e1x * vrl4x;
    Real ry1 = e2x * vrl1x;  Real ry2 = e2x * vrl2x;
    Real ry3 = e2x * vrl3x;  Real ry4 = e2x * vrl4x;

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

    Real rx13 = rx1 - rx3;
    Real rx24 = rx2 - rx4;
    Real ry13 = ry1 - ry3;
    Real ry24 = ry2 - ry4;

    //  Curvature rates × area
    Real kyy = -(py1 * rx13 + py2 * rx24);
    Real kxx =   px1 * ry13 + px2 * ry24;
    Real kxy = (py1 * ry13 + py2 * ry24) - (px1 * rx13 + px2 * rx24);


    //  Update transverse shear from average rotation
    Real rxav = rx1 + rx2 + rx3 + rx4;
    Real ryav = ry1 + ry2 + ry3 + ry4;
    exz += ryav * 0.25 * area;
    eyz -= rxav * 0.25 * area;

    // ======================================================================
    //  4.  CSTRA3 — Strain increments (dt / area scaling) & accumulation
    // ======================================================================
    Real fac1 = dt / area;
    exx *= fac1;   // Δε_xx
    eyy *= fac1;   // Δε_yy
    exy *= fac1;   // Δγ_xy
    exz *= fac1;   // Δγ_xz
    eyz *= fac1;   // Δγ_yz
    kxx *= fac1;   // Δκ_xx
    kyy *= fac1;   // Δκ_yy
    kxy *= fac1;   // Δκ_xy

    //  GSTR accumulation (ISMSTR /= 11 path — standard incremental)
    if (ISMSTR != 11) {
        d_GSTR[0 * NUMELC + I] += exx;
        d_GSTR[1 * NUMELC + I] += eyy;
        d_GSTR[2 * NUMELC + I] += exy;
        d_GSTR[3 * NUMELC + I] += eyz;
        d_GSTR[4 * NUMELC + I] += exz;
        d_GSTR[5 * NUMELC + I] += kxx;
        d_GSTR[6 * NUMELC + I] += kyy;
        d_GSTR[7 * NUMELC + I] += kxy;
    } else {
        //  ISMSTR=11: membrane strains from displacement increments
        Real ux1_v = d_UX1[I]; Real uy1_v = d_UY1[I];
        Real ux2_v = d_UX2[I]; Real uy2_v = d_UY2[I];
        Real ux3_v = d_UX3[I]; Real uy3_v = d_UY3[I];
        Real ux4_v = d_UX4[I]; Real uy4_v = d_UY4[I];

        Real fac_a = a_i;  // 1/area
        Real ux13d = ux1_v - ux3_v;
        Real ux24d = ux2_v - ux4_v;
        Real uy13d = uy1_v - uy3_v;
        Real uy24d = uy2_v - uy4_v;

        d_GSTR[0 * NUMELC + I] = (px1 * ux13d + px2 * ux24d) * fac_a;
        d_GSTR[1 * NUMELC + I] = (py1 * uy13d + py2 * uy24d) * fac_a;
        d_GSTR[2 * NUMELC + I] = (py1 * ux13d + py2 * ux24d
                                 + px1 * uy13d + px2 * uy24d) * fac_a;
        d_GSTR[3 * NUMELC + I] += eyz;
        d_GSTR[4 * NUMELC + I] += exz;
        d_GSTR[5 * NUMELC + I] += kxx;
        d_GSTR[6 * NUMELC + I] += kyy;
        d_GSTR[7 * NUMELC + I] += kxy;
    }

    // ======================================================================
    //  4b. Stress-work internal energy — trapezoidal rule, part 1
    //      Read generalized forces from PREVIOUS timestep (still in d_FOR/MOM).
    //      degmb = FOR_old · Δε,  degfx = MOM_old · Δκ
    // ======================================================================
    Real for_old_xx = d_FORxx[I];
    Real for_old_yy = d_FORyy[I];
    Real for_old_xy = d_FORxy[I];
    Real for_old_yz = d_FORyz[I];
    Real for_old_zx = d_FORzx[I];
    Real mom_old_xx = d_MOMxx[I];
    Real mom_old_yy = d_MOMyy[I];
    Real mom_old_xy = d_MOMxy[I];

    Real degmb = for_old_xx * exx + for_old_yy * eyy + for_old_xy * exy
                 + for_old_yz * eyz + for_old_zx * exz;
    Real degfx = mom_old_xx * kxx + mom_old_yy * kyy + mom_old_xy * kxy;

    // ======================================================================
    //  5.  Equivalent strain rate for element (energy-equivalent)
    //      CPU formula (cforc3.F lines 564-568):
    //        eps_m2 = 4/3*(exx²+eyy²+exx*eyy+¼*exy²)
    //        eps_k2 = 1/9*thk²*(kxx²+kyy²+kxx*kyy+¼*kxy²)
    //        epsd_pg = sqrt(eps_m2 + eps_k2) / dt
    // ======================================================================
    Real eps_m2 = (4.0/3.0) * (exx*exx + eyy*eyy + exx*eyy + 0.25*exy*exy);
    Real eps_k2 = (1.0/9.0) * thk_i * thk_i
                  * (kxx*kxx + kyy*kyy + kxx*kyy + 0.25*kxy*kxy);
    Real epsd_eq = sqrt(eps_m2 + eps_k2) / fmax(dt, 1.0e-30);

    // ======================================================================
    //  6.  CMAIN3 / SIGEPS02C — Through-thickness material integration
    //      Gauss quadrature over NPT points.
    //      Accumulate generalized forces (Nij) and moments (Mij).
    // ======================================================================
    Real nxx = 0.0, nyy = 0.0, nxy = 0.0, nyz_f = 0.0, nxz_f = 0.0;
    Real mxx = 0.0, myy = 0.0, mxy = 0.0;
    Real sigy_acc = 0.0;

    Real off_old = fabs(off_i);  // store for rupture check
    Real thk_updated = thk_i;

    //  EPDR scaled by dt (as in SIGEPS02C)
    Real epdr_scaled = fmax(mat.EPDR * dt, 1.0e-20);

    //  Pre-compute thermal factor for J-C (IFORM=0)
    //  (Zerilli not implemented in this mockup for simplicity)

    for (int ipt = 0; ipt < NPT; ipt++) {
        // --- Gauss point z-coordinate in [-0.5, +0.5] of unit thickness ---
        //     For NPT points, Simpson-like spacing:
        //     zeta = (-0.5 + (ipt + 0.5) / NPT)  →  maps to [-0.5, +0.5]
        Real zeta = -0.5 + ((Real)ipt + 0.5) / (Real)NPT;
        Real z_pos = zeta * thk0_i;  // physical z-coordinate

        //  Integration weight  (trapezoidal / equal spacing):
        //  wt = thk0 / NPT   (equal weight for each point)
        Real wt = thk0_i / (Real)NPT;

        // --- Combined strain increment at this z-point ---
        Real deps_xx = exx + z_pos * kxx;
        Real deps_yy = eyy + z_pos * kyy;
        Real deps_xy = exy + z_pos * kxy;
        Real deps_yz = eyz;
        Real deps_zx = exz;

        // --- Read old stress & state for this integration point ---
        int idx = ipt * NUMELC + I;
        Real sigo_xx = d_SIGxx[idx];
        Real sigo_yy = d_SIGyy[idx];
        Real sigo_xy = d_SIGxy[idx];
        Real sigo_yz = d_SIGyz[idx];
        Real sigo_zx = d_SIGzx[idx];
        Real pla_ip  = d_PLA[idx];
        Real epsd_ip = d_EPSD_ip[idx];
        Real sbak_xx = d_SIGBAKxx[idx];
        Real sbak_yy = d_SIGBAKyy[idx];
        Real sbak_xy = d_SIGBAKxy[idx];
        Real temp_ip = d_TEMPEL[idx];

        // --- Strain rate for this integration point ---
        Real epsdot_ip;
        if (mat.VP == 1) {
            epsdot_ip = epsd_ip * dt;  // stored filtered plastic rate
        } else if (mat.VP == 2) {
            epsd_ip = mat.ASRATE * epsd_eq + (1.0 - mat.ASRATE) * epsd_ip;
            epsdot_ip = epsd_ip * dt;
        } else {
            epsdot_ip = epsd_ip * dt;  // VP=3 (simplified)
        }

        // --- Homologous temperature (J-C, IFORM=0) ---
        Real tstar = 0.0;
        if (mat.IFORM == 0 && mat.TMELT > mat.TREF) {
            tstar = fmax(0.0, (temp_ip - mat.TREF) / (mat.TMELT - mat.TREF));
        }

        // --- Effective yield parameters with thermal softening ---
        Real ca_eff = mat.CA;
        Real cb_eff = mat.CB;
        Real ymax_eff = mat.YMAX;
        if (mat.IFORM == 0 && tstar > 0.0) {
            Real tfac = 1.0 - pow(tstar, mat.M_EXP);
            tfac = fmax(tfac, 1.0e-20);
            ca_eff *= tfac;
            cb_eff *= tfac;
        }

        // --- Radial return (M2CPLR equivalent) ---
        Real sign_xx, sign_yy, sign_xy, sign_yz, sign_zx;
        Real dpla_ip, ezz_ip, yld_ip, hardm_ip, etse_ip;
        Real off_val = fmin(1.0, fabs(off_i));

        m2cplr_device<IPLA>(
            a11, a12, g_v, gs_val, nu_v, ym,
            ca_eff, cb_eff, mat.CN, ymax_eff,
            mat.CC, epdr_scaled, mat.FISOKIN,
            deps_xx, deps_yy, deps_xy, deps_yz, deps_zx,
            sigo_xx, sigo_yy, sigo_xy, sigo_yz, sigo_zx,
            sbak_xx, sbak_yy, sbak_xy,
            epsdot_ip,
            pla_ip, off_val,
            sign_xx, sign_yy, sign_xy, sign_yz, sign_zx,
            dpla_ip, ezz_ip, yld_ip, hardm_ip, etse_ip);

        // --- VP=1: post-update filtered plastic strain rate ---
        if (mat.VP == 1) {
            Real actual_rate = dpla_ip / fmax(dt, 1.0e-20);
            epsd_ip = mat.ASRATE * actual_rate + (1.0 - mat.ASRATE) * epsd_ip;
        }

        // --- Yield stress accumulation ---
        sigy_acc += yld_ip / (Real)NPT;

        // --- Ductile rupture check ---
        if (off_val == off_old && off_val > 0.0) {
            if (off_val == 1.0 && pla_ip >= mat.EPMX && mat.EPMX > 0.0) {
                off_val = 0.8;
            } else if (off_val < 1.0) {
                off_val *= 0.8;
            }
        }

        // --- Thickness update (physical layer thickness = thk0/NPT) ---
        // CPU MULAWGLC updates thickness unconditionally (ITHK not passed to mulawglc)
        // ITHK only controls THK0=THK snapshot in CCOEF3, not physical THK evolution
        Real thk_layer = thk0_i / (Real)NPT;
        Real ezz_ps = -(deps_xx + deps_yy) * nu_v - (1.0 - 2.0 * nu_v) * ezz_ip;
        ezz_ps /= (1.0 - nu_v);
        thk_updated += ezz_ps * thk_layer * off_val;

        // --- Temperature update (adiabatic) ---
        if (mat.RHOCP > 0.0) {
            temp_ip += yld_ip * dpla_ip / mat.RHOCP;
        }

        // --- Write updated state for this integration point ---
        d_SIGxx[idx]    = sign_xx;
        d_SIGyy[idx]    = sign_yy;
        d_SIGxy[idx]    = sign_xy;
        d_SIGyz[idx]    = sign_yz;
        d_SIGzx[idx]    = sign_zx;
        d_PLA[idx]      = pla_ip;
        d_EPSD_ip[idx]  = epsd_ip;
        d_SIGBAKxx[idx] = sbak_xx;
        d_SIGBAKyy[idx] = sbak_yy;
        d_SIGBAKxy[idx] = sbak_xy;
        d_DPLA[idx]     = dpla_ip;
        d_TEMPEL[idx]   = temp_ip;

        // --- Accumulate generalized forces & moments (numerical integration) ---
        nxx  += sign_xx * wt;
        nyy  += sign_yy * wt;
        nxy  += sign_xy * wt;
        nyz_f += sign_yz * wt;
        nxz_f += sign_zx * wt;
        mxx  += sign_xx * z_pos * wt;
        myy  += sign_yy * z_pos * wt;
        mxy  += sign_xy * z_pos * wt;

    } // end through-thickness loop

    // ======================================================================
    //  7.  Store generalized forces normalized by thickness
    //      FOR = N / thk,  MOM = M / thk^2
    // ======================================================================
    Real inv_thk  = 1.0 / fmax(thk0_i, 1.0e-20);
    Real inv_thk2 = inv_thk * inv_thk;  // MOM = M / thk0² (cf. CPU mulawc: mom = sum(wmc*sig))

    // ======================================================================
    //  7b. Stress-work internal energy — trapezoidal rule, part 2
    //      degmb += FOR_new · Δε,  degfx += MOM_new · Δκ
    //      then  eint(1) += degmb * ½ * vol0
    //            eint(2) += degfx * thk0 * ½ * vol0
    // ======================================================================
    {
        Real fnxx = nxx * inv_thk;
        Real fnyy = nyy * inv_thk;
        Real fnxy = nxy * inv_thk;
        Real fnyz = nyz_f * inv_thk;
        Real fnzx = nxz_f * inv_thk;
        Real mnxx = mxx * inv_thk2;
        Real mnyy = myy * inv_thk2;
        Real mnxy = mxy * inv_thk2;

        degmb += fnxx * exx + fnyy * eyy + fnxy * exy
               + fnyz * eyz + fnzx * exz;
        degfx += mnxx * kxx + mnyy * kyy + mnxy * kxy;

        Real half_vol0 = 0.5 * vol0;
        d_EINT[0 * NUMELC + I] += degmb * half_vol0;
        d_EINT[1 * NUMELC + I] += degfx * thk0_i * half_vol0;
    }

    d_FORxx[I] = nxx * inv_thk;
    d_FORyy[I] = nyy * inv_thk;
    d_FORxy[I] = nxy * inv_thk;
    d_FORyz[I] = nyz_f * inv_thk;
    d_FORzx[I] = nxz_f * inv_thk;
    d_MOMxx[I] = mxx * inv_thk2;
    d_MOMyy[I] = myy * inv_thk2;
    d_MOMxy[I] = mxy * inv_thk2;

    // ======================================================================
    //  8.  Update element-level state
    // ======================================================================
    d_THK[I]       = thk_updated;
    d_SIGY[I]      = sigy_acc;
    //  Element-level strain rate filtering (cf. cforc3.F line 570):
    //    gbuf%epsd = asrate * epsd_pg + (1-asrate) * gbuf%epsd
    //  For now asrate=1 (no filtering at element level), same as CPU default.
    d_EPSD_elem[I] = epsd_eq;
    // OFF is written back (may be modified by rupture at last ipt)
    // For simplicity, we use the last-layer off_val; a more rigorous
    // approach would track across all points.
    // d_OFF[I] is already current (rupture is per-element, tracked externally).
}

// ===========================================================================
//  Host-side launcher
// ===========================================================================
extern "C"
void launch_shell_strain_material_kernel(
    // Kernel 1 outputs (read-only)
    const Real* d_E1x, const Real* d_E1y, const Real* d_E1z,
    const Real* d_E2x, const Real* d_E2y, const Real* d_E2z,
    const Real* d_E3x, const Real* d_E3y, const Real* d_E3z,
    const Real* d_VL1x,  const Real* d_VL1y,  const Real* d_VL1z,
    const Real* d_VL2x,  const Real* d_VL2y,  const Real* d_VL2z,
    const Real* d_VL3x,  const Real* d_VL3y,  const Real* d_VL3z,
    const Real* d_VL4x,  const Real* d_VL4y,  const Real* d_VL4z,
    const Real* d_VRL1x, const Real* d_VRL1y, const Real* d_VRL1z,
    const Real* d_VRL2x, const Real* d_VRL2y, const Real* d_VRL2z,
    const Real* d_VRL3x, const Real* d_VRL3y, const Real* d_VRL3z,
    const Real* d_VRL4x, const Real* d_VRL4y, const Real* d_VRL4z,
    const Real* d_PX1,  const Real* d_PX2,
    const Real* d_PY1,  const Real* d_PY2,
    const Real* d_AREA, const Real* d_A_I,
    const Real* d_UX1,  const Real* d_UY1,
    const Real* d_UX2,  const Real* d_UY2,
    const Real* d_UX3,  const Real* d_UY3,
    const Real* d_UX4,  const Real* d_UY4,
    // Element state
    Real* d_OFF, Real* d_THK, const Real* d_THK0,
    Real* d_GSTR, Real* d_EINT, Real* d_EPSD_elem,
    // Per-integration-point state
    Real* d_SIGxx, Real* d_SIGyy, Real* d_SIGxy,
    Real* d_SIGyz, Real* d_SIGzx,
    Real* d_PLA, Real* d_EPSD_ip,
    Real* d_SIGBAKxx, Real* d_SIGBAKyy, Real* d_SIGBAKxy,
    Real* d_DPLA, Real* d_TEMPEL,
    // Outputs: generalized forces/moments
    Real* d_FORxx, Real* d_FORyy, Real* d_FORxy,
    Real* d_FORyz, Real* d_FORzx,
    Real* d_MOMxx, Real* d_MOMyy, Real* d_MOMxy,
    Real* d_SIGY,
    // Scalars
    JohnsonCookParams mat,
    Real dt, int NPT, int NUMELC, int ISMSTR, int ITHK,
    int NCYCLE_DBG,
    cudaStream_t stream)
{
    const int BLOCK_SIZE = 256;
    int grid_size = (NUMELC + BLOCK_SIZE - 1) / BLOCK_SIZE;

    // -----------------------------------------------------------------------
    //  Helper: launch kernel for a given (IPLA, NPT) pair
    //  Common args block to avoid massive duplication
    // -----------------------------------------------------------------------
    auto launch = [&](auto ipla_tag, auto npt_tag, auto ismstr_tag) {
        constexpr int IPLA_V   = decltype(ipla_tag)::value;
        constexpr int NPT_V    = decltype(npt_tag)::value;
        constexpr int ISMSTR_V = decltype(ismstr_tag)::value;
        shell_strain_material_kernel<IPLA_V, NPT_V, ISMSTR_V><<<grid_size, BLOCK_SIZE, 0, stream>>>(
            d_E1x, d_E1y, d_E1z, d_E2x, d_E2y, d_E2z, d_E3x, d_E3y, d_E3z,
            d_VL1x, d_VL1y, d_VL1z, d_VL2x, d_VL2y, d_VL2z,
            d_VL3x, d_VL3y, d_VL3z, d_VL4x, d_VL4y, d_VL4z,
            d_VRL1x, d_VRL1y, d_VRL1z, d_VRL2x, d_VRL2y, d_VRL2z,
            d_VRL3x, d_VRL3y, d_VRL3z, d_VRL4x, d_VRL4y, d_VRL4z,
            d_PX1, d_PX2, d_PY1, d_PY2, d_AREA, d_A_I,
            d_UX1, d_UY1, d_UX2, d_UY2, d_UX3, d_UY3, d_UX4, d_UY4,
            d_OFF, d_THK, d_THK0, d_GSTR, d_EINT, d_EPSD_elem,
            d_SIGxx, d_SIGyy, d_SIGxy, d_SIGyz, d_SIGzx,
            d_PLA, d_EPSD_ip,
            d_SIGBAKxx, d_SIGBAKyy, d_SIGBAKxy, d_DPLA, d_TEMPEL,
            d_FORxx, d_FORyy, d_FORxy, d_FORyz, d_FORzx,
            d_MOMxx, d_MOMyy, d_MOMxy, d_SIGY,
            mat, dt, NUMELC, ITHK, NCYCLE_DBG);
    };

    // Dispatch on (IPLA, NPT, ISMSTR)
    auto dispatch_ismstr = [&](auto ipla_tag, auto npt_tag) {
        switch (ISMSTR) {
            case 1:  launch(ipla_tag, npt_tag, std::integral_constant<int,1>{});  break;
            case 2:  launch(ipla_tag, npt_tag, std::integral_constant<int,2>{});  break;
            case 11: launch(ipla_tag, npt_tag, std::integral_constant<int,11>{}); break;
            default:
                fprintf(stderr, "Unsupported ISMSTR=%d in shell_strain_material_kernel\n", ISMSTR);
                exit(EXIT_FAILURE);
        }
    };

    auto dispatch_npt = [&](auto ipla_tag) {
        switch (NPT) {
            case 1:  dispatch_ismstr(ipla_tag, std::integral_constant<int,1>{}); break;
            case 3:  dispatch_ismstr(ipla_tag, std::integral_constant<int,3>{}); break;
            case 5:  dispatch_ismstr(ipla_tag, std::integral_constant<int,5>{}); break;
            default:
                fprintf(stderr, "Unsupported NPT=%d in shell_strain_material_kernel\n", NPT);
                exit(EXIT_FAILURE);
        }
    };

    switch (mat.IPLA) {
        case 1:  dispatch_npt(std::integral_constant<int,1>{}); break;
        case 2:  dispatch_npt(std::integral_constant<int,2>{}); break;
        default: dispatch_npt(std::integral_constant<int,0>{}); break;
    }

    // Check for kernel launch errors
    {
        cudaError_t err = cudaPeekAtLastError();
        if (err != cudaSuccess) {
            fprintf(stderr, "CUDA kernel launch error (strain+material): %s\n",
                    cudaGetErrorString(err));
            exit(EXIT_FAILURE);
        }
    }
}

#endif // SHELL_STRAIN_MATERIAL_KERNEL_CU
