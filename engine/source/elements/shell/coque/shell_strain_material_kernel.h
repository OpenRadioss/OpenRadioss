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
//  Host-side header for shell_strain_material_kernel
// ==============================================================================

#ifndef SHELL_STRAIN_MATERIAL_KERNEL_H
#define SHELL_STRAIN_MATERIAL_KERNEL_H

#include <cuda_runtime.h>
#include "real_type.h"

// ---------------------------------------------------------------------------
//  Johnson-Cook material parameter structure
// ---------------------------------------------------------------------------
struct JohnsonCookParams {
    Real E;           // Young's modulus
    Real nu;          // Poisson's ratio
    Real G;           // Shear modulus  E/(2(1+nu))
    Real A11;         // plane-stress stiffness  E/(1-nu^2)
    Real A12;         // coupling  nu * A11
    Real CA;          // J-C param A  (initial yield)
    Real CB;          // J-C param B  (hardening modulus)
    Real CN;          // J-C param n  (hardening exponent)
    Real CC;          // J-C param C  (strain-rate sensitivity)
    Real EPDR;        // reference strain rate (before dt scaling)
    Real EPMX;        // max plastic strain for ductile rupture
    Real YMAX;        // yield stress cap
    Real M_EXP;       // thermal exponent m
    Real FISOKIN;     // isotropic/kinematic ratio (1=iso, 0=kin)
    Real RHOCP;       // rho * Cp  (adiabatic heating)
    Real TREF;        // reference temperature
    Real TMELT;       // melting temperature
    Real ASRATE;      // strain-rate exponential filter coefficient
    Real RHO;         // density
    Real SSP;         // sound speed
    Real SHF_COEF;    // shear correction factor (e.g. 5/6)
    int    IPLA;        // plasticity algorithm (0, 1, or 2)
    int    VP;          // strain-rate type (1=plastic, 2=total, 3=deviatoric)
    int    IFORM;       // 0=Johnson-Cook, 1=Zerilli-Armstrong
    int    ICC;         // Cowper-Symonds flag
    Real Z3, Z4;      // Zerilli-Armstrong thermal params (0 for J-C)
};

#ifdef __cplusplus
extern "C" {
#endif

// ---------------------------------------------------------------------------
//  Launch the strain + material kernel  (CCOEF3 + CDEFO3 + CCURV3 + CSTRA3
//                                        + CMAIN3 / SIGEPS02C / M2CPLR)
//
//  All pointer arguments are device pointers.
//  mat         = material parameter struct (passed by value to kernel)
//  dt          = current time step
//  NPT         = number of through-thickness integration points
//  NUMELC      = total number of 4-node shell elements
//  ISMSTR      = small-strain flag (1, 2, or 11)
//  ITHK        = thickness-update flag (>0 enables)
//  stream      = CUDA stream (0 for default)
// ---------------------------------------------------------------------------
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
    // Per-integration-point state  [NPT * NUMELC]
    Real* d_SIGxx, Real* d_SIGyy, Real* d_SIGxy,
    Real* d_SIGyz, Real* d_SIGzx,
    Real* d_PLA, Real* d_EPSD_ip,
    Real* d_SIGBAKxx, Real* d_SIGBAKyy, Real* d_SIGBAKxy,
    Real* d_DPLA, Real* d_TEMPEL,
    // Outputs: generalized forces/moments  [NUMELC]
    Real* d_FORxx, Real* d_FORyy, Real* d_FORxy,
    Real* d_FORyz, Real* d_FORzx,
    Real* d_MOMxx, Real* d_MOMyy, Real* d_MOMxy,
    Real* d_SIGY,
    // Scalars
    JohnsonCookParams mat,
    Real dt, int NPT, int NUMELC, int ISMSTR, int ITHK,
    int NCYCLE_DBG,
    cudaStream_t stream);

#ifdef __cplusplus
}
#endif

#endif // SHELL_STRAIN_MATERIAL_KERNEL_H
