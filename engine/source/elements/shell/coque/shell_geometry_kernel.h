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
//  OpenRadioss GPU — Kernel 1 (Option C): Geometry
//  Host-side header for shell_geometry_kernel
// ==============================================================================

#ifndef SHELL_GEOMETRY_KERNEL_H
#define SHELL_GEOMETRY_KERNEL_H


#include <cuda_runtime.h>
#include "real_type.h"

#ifdef __cplusplus
extern "C" {
#endif

// ---------------------------------------------------------------------------
//  Launch the geometry kernel (CCOOR3 + CNVEC3 + CDERI3)
//
//  All pointer arguments are device pointers.
//  NUMELC = total number of 4-node shell elements.
//  ISMSTR = small-strain flag (1, 2, or 11).
//  stream = CUDA stream (0 for default stream).
// ---------------------------------------------------------------------------
void launch_shell_geometry_kernel(
    // Node arrays  [3,NUMNOD] each, {x1 y1 z1|x2 y2 z2|...}, contiguous in memory
    const Real* d_X,
    const Real* d_V,
    const Real* d_VR,
    // Connectivity  [NUMELC] each
    const int* d_N1, const int* d_N2, const int* d_N3, const int* d_N4,
    // Element state  (read/write)
    Real* d_OFF,          // [NUMELC]          activity flag
    Real* d_SMSTR,        // [NUMELC*6]        reference coords (ISMSTR)
    // Outputs: local frame  [NUMELC] each
    Real* d_E1x, Real* d_E1y, Real* d_E1z,
    Real* d_E2x, Real* d_E2y, Real* d_E2z,
    Real* d_E3x, Real* d_E3y, Real* d_E3z,
    // Outputs: gathered translational velocities  [NUMELC] each
    Real* d_VL1x,  Real* d_VL1y,  Real* d_VL1z,
    Real* d_VL2x,  Real* d_VL2y,  Real* d_VL2z,
    Real* d_VL3x,  Real* d_VL3y,  Real* d_VL3z,
    Real* d_VL4x,  Real* d_VL4y,  Real* d_VL4z,
    // Outputs: gathered rotational velocities  [NUMELC] each
    Real* d_VRL1x, Real* d_VRL1y, Real* d_VRL1z,
    Real* d_VRL2x, Real* d_VRL2y, Real* d_VRL2z,
    Real* d_VRL3x, Real* d_VRL3y, Real* d_VRL3z,
    Real* d_VRL4x, Real* d_VRL4y, Real* d_VRL4z,
    // Outputs: shape-function derivatives & geometry  [NUMELC] each
    Real* d_PX1,  Real* d_PX2,
    Real* d_PY1,  Real* d_PY2,
    Real* d_AREA, Real* d_A_I,
    Real* d_VHX,  Real* d_VHY,
    Real* d_Z2,
    // Outputs: displacement increments  [NUMELC] each
    Real* d_UX1,  Real* d_UY1,
    Real* d_UX2,  Real* d_UY2,
    Real* d_UX3,  Real* d_UY3,
    Real* d_UX4,  Real* d_UY4,
    // Outputs: stiffness (CPXPY3-style)
    Real* d_STI,  Real* d_STIR,
    // Per-element material scalars (for stiffness)
    const Real* d_YM,   const Real* d_THK0,
    const int* d_N3_stiff, const int* d_N4_stiff,
    // Scalars
    int NUMELC, int ISMSTR,
    Real H1, Real H2,
    cudaStream_t stream);

#ifdef __cplusplus
}
#endif

#endif // SHELL_GEOMETRY_KERNEL_H
