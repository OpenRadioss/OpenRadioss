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
//  Host-side header for shell_force_assembly_kernel
// ==============================================================================

#ifndef SHELL_FORCE_ASSEMBLY_KERNEL_H
#define SHELL_FORCE_ASSEMBLY_KERNEL_H

#include <cuda_runtime.h>
#include "real_type.h"

// ---------------------------------------------------------------------------
//  Hourglass parameter structure
// ---------------------------------------------------------------------------
struct HourglassParams {
    Real H1;          // membrane hourglass coefficient (from GEO(13,PID))
    Real H2;          // bending hourglass coefficient  (from GEO(14,PID))
    Real H3;          // rotational hourglass coefficient (from GEO(15,PID))
    Real SRH1;        // sqrt scale for H1              (from GEO(18,PID))
    Real SRH2;        // sqrt scale for H2              (from GEO(19,PID))
    Real SRH3;        // sqrt scale for H3              (from GEO(20,PID))
    Real HVISC;       // viscous hourglass scaling (from common /SCR06R/, typically 0.5)
    Real HELAS;       // elastic hourglass scaling (from common /SCR06R/, typically 0.5)
    Real HVLIN;       // linear  hourglass scaling (from common /SCR06R/, typically 0.0)
};

#ifdef __cplusplus
extern "C" {
#endif

// ---------------------------------------------------------------------------
//  Launch the force computation + atomic assembly kernel
//  (CHVIS3 + CFINT3 + CUPDT3)
//
//  All pointer arguments are device pointers.
//  d_Fx..d_STIFR are global nodal arrays — written with atomicAdd.
//  d_HOUR is per-element hourglass state [NUMELC * 5], read/write.
//  d_EINT is per-element internal energy  [NUMELC * 2], read/write.
//
//  The global nodal force/moment arrays MUST be zeroed before calling
//  this kernel (or use a Real-buffer scheme).
// ---------------------------------------------------------------------------
void launch_shell_force_assembly_kernel(
    // Connectivity  [NUMELC] each
    const int* d_N1, const int* d_N2, const int* d_N3, const int* d_N4,
    // Local frame from Kernel 1  [NUMELC] each
    const Real* d_E1x, const Real* d_E1y, const Real* d_E1z,
    const Real* d_E2x, const Real* d_E2y, const Real* d_E2z,
    const Real* d_E3x, const Real* d_E3y, const Real* d_E3z,
    // Shape-function derivatives & area  [NUMELC] each
    const Real* d_PX1, const Real* d_PX2,
    const Real* d_PY1, const Real* d_PY2,
    const Real* d_AREA,
    // Hourglass volume ratios [NUMELC] each (for non-uniform GAMA)
    const Real* d_VHX, const Real* d_VHY,
    // Gathered velocities from Kernel 1  [NUMELC] each
    const Real* d_VL1x,  const Real* d_VL1y,  const Real* d_VL1z,
    const Real* d_VL2x,  const Real* d_VL2y,  const Real* d_VL2z,
    const Real* d_VL3x,  const Real* d_VL3y,  const Real* d_VL3z,
    const Real* d_VL4x,  const Real* d_VL4y,  const Real* d_VL4z,
    const Real* d_VRL1x, const Real* d_VRL1y, const Real* d_VRL1z,
    const Real* d_VRL2x, const Real* d_VRL2y, const Real* d_VRL2z,
    const Real* d_VRL3x, const Real* d_VRL3y, const Real* d_VRL3z,
    const Real* d_VRL4x, const Real* d_VRL4y, const Real* d_VRL4z,
    // Element state  [NUMELC] each
    const Real* d_OFF,
    const Real* d_THK0,
    const Real* d_THK02,
    // Generalized forces/moments from Kernel 2  [NUMELC] each
    const Real* d_FORxx, const Real* d_FORyy, const Real* d_FORxy,
    const Real* d_FORyz, const Real* d_FORzx,
    const Real* d_MOMxx, const Real* d_MOMyy, const Real* d_MOMxy,
    // Hourglass state  [NUMELC * 5]
    Real* d_HOUR,
    // Stiffness outputs [NUMELC]
    Real* d_STI, Real* d_STIR,
    // Per-element material scalars [NUMELC] each
    const Real* d_SSP, const Real* d_RHO, const Real* d_YM,
    const Real* d_NU,  const Real* d_A11, const Real* d_G,
    const Real* d_SHF,
    // Internal energy  [NUMELC * 2]
    Real* d_EINT,
    // Global nodal arrays [NUMNOD] each — ATOMICALLY written
    Real* d_Fx, Real* d_Fy, Real* d_Fz,
    Real* d_Mx, Real* d_My, Real* d_Mz,
    Real* d_STIFN, Real* d_STIFR,
    // Scalars
    HourglassParams hg_params,
    Real dt, int NPT, int NUMELC,
    int compute_sti,
    int ISMSTR, int IHBE,
    cudaStream_t stream);

#ifdef __cplusplus
}
#endif

#endif // SHELL_FORCE_ASSEMBLY_KERNEL_H
