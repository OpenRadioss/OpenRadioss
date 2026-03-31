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
//  Covers: CCOOR3 (gather) → CNVEC3 (convected frame) → CDERI3 (shape funcs)
//
//  Scope: ISMSTR=1, IHBE<=1, ISHFRAM=0 (symmetric convected frame)
//         One thread per element, flat SoA layout.
// ==============================================================================

#ifndef SHELL_GEOMETRY_KERNEL_CU
#define SHELL_GEOMETRY_KERNEL_CU

#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cfloat>
#include <type_traits>
#include "real_type.h"

// ---------------------------------------------------------------------------
//  Small helpers
// ---------------------------------------------------------------------------
__device__ __forceinline__ Real safe_inv_norm(Real x, Real y, Real z)
{
    Real n2 = x * x + y * y + z * z;
    return (n2 > 1.0e-40) ? rsqrt(n2) : 0.0;
}

// ===========================================================================
//  Kernel 1 — shell_geometry_kernel
//
//  Per-element outputs written to global memory so that Kernel 2 (strains +
//  material) and Kernel 3 (forces + assembly) can consume them.
//
//  Thread mapping:  I = blockIdx.x * blockDim.x + threadIdx.x
//                   if (I >= NUMELC) return;
// ===========================================================================
template <int ISMSTR>
__global__ __launch_bounds__(256, 2) void shell_geometry_kernel(
    // ---- Global node arrays (read-only) ----
    const Real* __restrict__ d_X,    // [3*NUMNOD] node X-coordinates
    const Real* __restrict__ d_V,    // [3*NUMNOD] translational velocity X
    const Real* __restrict__ d_VR,   // [3*NUMNOD] rotational velocity X
    // ---- Connectivity (read-only, SoA) ----
    const int* __restrict__ d_N1,       // [NUMELC] node-1 index
    const int* __restrict__ d_N2,       // [NUMELC] node-2 index
    const int* __restrict__ d_N3,       // [NUMELC] node-3 index
    const int* __restrict__ d_N4,       // [NUMELC] node-4 index
    // ---- Element state (read/write) ----
    Real* __restrict__ d_OFF,         // [NUMELC] element activity flag
    Real* __restrict__ d_SMSTR,       // [NUMELC*6] reference local coords
    // ---- Per-element outputs for Kernel 2 & 3 (write) ----
    //  Local frame vectors (9 components)
    Real* __restrict__ d_E1x,         // [NUMELC]
    Real* __restrict__ d_E1y,
    Real* __restrict__ d_E1z,
    Real* __restrict__ d_E2x,
    Real* __restrict__ d_E2y,
    Real* __restrict__ d_E2z,
    Real* __restrict__ d_E3x,
    Real* __restrict__ d_E3y,
    Real* __restrict__ d_E3z,
    //  Gathered translational velocities  (4 nodes × 3 = 12)
    Real* __restrict__ d_VL1x,
    Real* __restrict__ d_VL1y,
    Real* __restrict__ d_VL1z,
    Real* __restrict__ d_VL2x,
    Real* __restrict__ d_VL2y,
    Real* __restrict__ d_VL2z,
    Real* __restrict__ d_VL3x,
    Real* __restrict__ d_VL3y,
    Real* __restrict__ d_VL3z,
    Real* __restrict__ d_VL4x,
    Real* __restrict__ d_VL4y,
    Real* __restrict__ d_VL4z,
    //  Gathered rotational velocities  (4 nodes × 3 = 12)
    Real* __restrict__ d_VRL1x,
    Real* __restrict__ d_VRL1y,
    Real* __restrict__ d_VRL1z,
    Real* __restrict__ d_VRL2x,
    Real* __restrict__ d_VRL2y,
    Real* __restrict__ d_VRL2z,
    Real* __restrict__ d_VRL3x,
    Real* __restrict__ d_VRL3y,
    Real* __restrict__ d_VRL3z,
    Real* __restrict__ d_VRL4x,
    Real* __restrict__ d_VRL4y,
    Real* __restrict__ d_VRL4z,
    //  Shape-function derivatives & geometry
    Real* __restrict__ d_PX1,         // [NUMELC]
    Real* __restrict__ d_PX2,
    Real* __restrict__ d_PY1,
    Real* __restrict__ d_PY2,
    Real* __restrict__ d_AREA,        // [NUMELC] element area
    Real* __restrict__ d_A_I,         // [NUMELC] 1/area
    Real* __restrict__ d_VHX,         // [NUMELC] hourglass vector X
    Real* __restrict__ d_VHY,         // [NUMELC] hourglass vector Y
    Real* __restrict__ d_Z2,          // [NUMELC] warping indicator
    //  Displacement increments (ISMSTR=11 only, zeroed for ISMSTR=1)
    Real* __restrict__ d_UX1,
    Real* __restrict__ d_UY1,
    Real* __restrict__ d_UX2,
    Real* __restrict__ d_UY2,
    Real* __restrict__ d_UX3,
    Real* __restrict__ d_UY3,
    Real* __restrict__ d_UX4,
    Real* __restrict__ d_UY4,
    //  Stiffness outputs (CPXPY3-style, written here because we have local coords)
    Real* __restrict__ d_STI,         // [NUMELC] translational stiffness
    Real* __restrict__ d_STIR,        // [NUMELC] rotational stiffness
    //  Per-element material scalars (read-only, for stiffness)
    const Real* __restrict__ d_YM,    // [NUMELC] Young's modulus
    const Real* __restrict__ d_THK0,  // [NUMELC] reference thickness
    const int* __restrict__ d_N3_conn,  // [NUMELC] node 3 connectivity (for triangle check)
    const int* __restrict__ d_N4_conn,  // [NUMELC] node 4 connectivity (for triangle check)
    //  Scalars
    int   NUMELC,
    Real H1,                          // membrane hourglass coefficient (GEO(13))
    Real H2                           // bending hourglass coefficient (GEO(14))
)
{
    // ======================================================================
    //  Thread mapping — one thread per element
    // ======================================================================
    int I = blockIdx.x * blockDim.x + threadIdx.x;
    if (I >= NUMELC) return;

    // ======================================================================
    //  1.  CCOOR3 — Gather nodal coordinates & velocities
    // ======================================================================
    int n1 = d_N1[I];
    int n2 = d_N2[I];
    int n3 = d_N3[I];
    int n4 = d_N4[I];

    // --- Global positions (XkG, YkG, ZkG) — AoS layout {x0,y0,z0,x1,y1,z1,...} ---
    Real x1g = __ldg(&d_X[3*n1]);    Real y1g = __ldg(&d_X[3*n1+1]);    Real z1g = __ldg(&d_X[3*n1+2]);
    Real x2g = __ldg(&d_X[3*n2]);    Real y2g = __ldg(&d_X[3*n2+1]);    Real z2g_nd = __ldg(&d_X[3*n2+2]);
    Real x3g = __ldg(&d_X[3*n3]);    Real y3g = __ldg(&d_X[3*n3+1]);    Real z3g = __ldg(&d_X[3*n3+2]);
    Real x4g = __ldg(&d_X[3*n4]);    Real y4g = __ldg(&d_X[3*n4+1]);    Real z4g = __ldg(&d_X[3*n4+2]);

    // --- Translational velocities — AoS layout ---
    Real vl1x = __ldg(&d_V[3*n1]);   Real vl1y = __ldg(&d_V[3*n1+1]);   Real vl1z = __ldg(&d_V[3*n1+2]);
    Real vl2x = __ldg(&d_V[3*n2]);   Real vl2y = __ldg(&d_V[3*n2+1]);   Real vl2z = __ldg(&d_V[3*n2+2]);
    Real vl3x = __ldg(&d_V[3*n3]);   Real vl3y = __ldg(&d_V[3*n3+1]);   Real vl3z = __ldg(&d_V[3*n3+2]);
    Real vl4x = __ldg(&d_V[3*n4]);   Real vl4y = __ldg(&d_V[3*n4+1]);   Real vl4z = __ldg(&d_V[3*n4+2]);

    // --- Rotational velocities — AoS layout ---
    Real vrl1x = __ldg(&d_VR[3*n1]);  Real vrl1y = __ldg(&d_VR[3*n1+1]);  Real vrl1z = __ldg(&d_VR[3*n1+2]);
    Real vrl2x = __ldg(&d_VR[3*n2]);  Real vrl2y = __ldg(&d_VR[3*n2+1]);  Real vrl2z = __ldg(&d_VR[3*n2+2]);
    Real vrl3x = __ldg(&d_VR[3*n3]);  Real vrl3y = __ldg(&d_VR[3*n3+1]);  Real vrl3z = __ldg(&d_VR[3*n3+2]);
    Real vrl4x = __ldg(&d_VR[3*n4]);  Real vrl4y = __ldg(&d_VR[3*n4+1]);  Real vrl4z = __ldg(&d_VR[3*n4+2]);

    // --- Element activity ---
    Real off_i = d_OFF[I];

    // Zero velocities if element is being deleted (OFFG < 0)
    if (off_i < 0.0) {
        vl1x = 0.0;  vl1y = 0.0;  vl1z = 0.0;
        vl2x = 0.0;  vl2y = 0.0;  vl2z = 0.0;
        vl3x = 0.0;  vl3y = 0.0;  vl3z = 0.0;
        vl4x = 0.0;  vl4y = 0.0;  vl4z = 0.0;
        vrl1x = 0.0;  vrl1y = 0.0;  vrl1z = 0.0;
        vrl2x = 0.0;  vrl2y = 0.0;  vrl2z = 0.0;
        vrl3x = 0.0;  vrl3y = 0.0;  vrl3z = 0.0;
        vrl4x = 0.0;  vrl4y = 0.0;  vrl4z = 0.0;
    }

    // ======================================================================
    //  2.  CNVEC3 — Convected orthonormal frame (ISHFRAM=0, default)
    // ======================================================================
    //  Edge vectors in global frame
    Real x21 = x2g - x1g;   Real y21 = y2g - y1g;   Real z21 = z2g_nd - z1g;
    Real x32 = x3g - x2g;   Real y32 = y3g - y2g;   Real z32 = z3g - z2g_nd;
    Real x34 = x3g - x4g;   Real y34 = y3g - y4g;   Real z34 = z3g - z4g;
    Real x41 = x4g - x1g;   Real y41 = y4g - y1g;   Real z41 = z4g - z1g;

    //  Initial E1 = (edge 2-1) + (edge 3-4)   — "diagonal" direction
    Real e1x = x21 + x34;
    Real e1y = y21 + y34;
    Real e1z = z21 + z34;

    //  Initial E2 = (edge 3-2) + (edge 4-1)   — other "diagonal"
    Real e2x = x32 + x41;
    Real e2y = y32 + y41;
    Real e2z = z32 + z41;

    //  E3 = E1 × E2  (shell normal, un-normalized)
    Real e3x = e1y * e2z - e1z * e2y;
    Real e3y = e1z * e2x - e1x * e2z;
    Real e3z = e1x * e2y - e1y * e2x;

    //  Normalize E3
    Real inv_n = safe_inv_norm(e3x, e3y, e3z);
    e3x *= inv_n;
    e3y *= inv_n;
    e3z *= inv_n;

    //  Symmetrize E1 (ISHFRAM=0):  ratio = |E1| / |E2|
    Real s1 = e1x * e1x + e1y * e1y + e1z * e1z;
    Real s2 = e2x * e2x + e2y * e2y + e2z * e2z;
    Real ratio = sqrt(s1 / fmax(s2, 1.0e-40));

    //  E1 += (E2 × E3) * ratio
    e1x += (e2y * e3z - e2z * e3y) * ratio;
    e1y += (e2z * e3x - e2x * e3z) * ratio;
    e1z += (e2x * e3y - e2y * e3x) * ratio;

    //  Normalize E1
    inv_n = safe_inv_norm(e1x, e1y, e1z);
    e1x *= inv_n;
    e1y *= inv_n;
    e1z *= inv_n;

    //  E2 = E3 × E1 (complete orthogonal triad)
    e2x = e3y * e1z - e3z * e1y;
    e2y = e3z * e1x - e3x * e1z;
    e2z = e3x * e1y - e3y * e1x;

    // ======================================================================
    //  3.  CDERI3 — Shape-function derivatives & local coordinates
    //              (ISMSTR = 1 or 2, co-rotational small strain)
    // ======================================================================

    // --- Step 1: Global relative vectors (nodes 2,3,4 w.r.t. node 1) ---
    Real x21g = x2g - x1g;   Real y21g = y2g - y1g;   Real z21g = z2g_nd - z1g;
    Real x31g = x3g - x1g;   Real y31g = y3g - y1g;   Real z31g = z3g - z1g;
    Real x41g = x4g - x1g;   Real y41g = y4g - y1g;   Real z41g = z4g - z1g;

    // --- Step 2: Project to local frame (E1, E2, E3) ---
    Real xl2 = e1x * x21g + e1y * y21g + e1z * z21g;
    Real yl2 = e2x * x21g + e2y * y21g + e2z * z21g;
    Real xl3 = e1x * x31g + e1y * y31g + e1z * z31g;
    Real yl3 = e2x * x31g + e2y * y31g + e2z * z31g;
    Real xl4 = e1x * x41g + e1y * y41g + e1z * z41g;
    Real yl4 = e2x * x41g + e2y * y41g + e2z * z41g;
    Real zl2 = e3x * x21g + e3y * y21g + e3z * z21g;   // warping indicator

    // --- Step 3: ISMSTR logic — reference geometry management ---
    //  Displacement increments (default zero for ISMSTR=1,2)
    Real ux1 = 0.0, uy1 = 0.0;
    Real ux2 = 0.0, uy2 = 0.0;
    Real ux3 = 0.0, uy3 = 0.0;
    Real ux4 = 0.0, uy4 = 0.0;

    //  SMSTR offsets:  component c for element I  →  d_SMSTR[c * NUMELC + I]
    //  c = 0..5  maps to  X2_ref, Y2_ref, X3_ref, Y3_ref, X4_ref, Y4_ref

    Real abs_off = fabs(off_i);

    if (ISMSTR == 11) {
        // ------------------------------------------------------------------
        //  ISMSTR = 11:  incremental small-strain formulation
        //  First cycle: |OFF|==1 → mark as 2, store reference, zero displ.
        //  Later:       |OFF|==2 → compute displacement increments from ref.
        // ------------------------------------------------------------------
        if (abs_off == 1.0) {
            // First cycle — store reference local coords, set flag to 2
            off_i = copysign(2.0, off_i);

            d_SMSTR[0 * NUMELC + I] = xl2;
            d_SMSTR[1 * NUMELC + I] = yl2;
            d_SMSTR[2 * NUMELC + I] = xl3;
            d_SMSTR[3 * NUMELC + I] = yl3;
            d_SMSTR[4 * NUMELC + I] = xl4;
            d_SMSTR[5 * NUMELC + I] = yl4;
            // ux*, uy* remain zero; xl*, yl* are the reference (no displacement)
        }
        else if (abs_off == 2.0) {
            // Subsequent cycles — compute displacement increments
            Real sm0 = d_SMSTR[0 * NUMELC + I];
            Real sm1 = d_SMSTR[1 * NUMELC + I];
            Real sm2 = d_SMSTR[2 * NUMELC + I];
            Real sm3 = d_SMSTR[3 * NUMELC + I];
            Real sm4 = d_SMSTR[4 * NUMELC + I];
            Real sm5 = d_SMSTR[5 * NUMELC + I];

            ux2 = xl2 - sm0;   uy2 = yl2 - sm1;
            ux3 = xl3 - sm2;   uy3 = yl3 - sm3;
            ux4 = xl4 - sm4;   uy4 = yl4 - sm5;
            // ux1, uy1 stay zero (node 1 is origin in local frame)

            // Use reference geometry for shape functions
            xl2 = sm0;  yl2 = sm1;
            xl3 = sm2;  yl3 = sm3;
            xl4 = sm4;  yl4 = sm5;
            zl2 = 0.0;
        }
    }
    else if (ISMSTR == 1 || ISMSTR == 2) {
        // ------------------------------------------------------------------
        //  ISMSTR = 1 or 2:  co-rotational, reference frozen after 1st cycle
        //  |OFF|==2  → read reference from SMSTR, replace local coords.
        //  otherwise → store current local coords into SMSTR.
        //  ISMSTR=1 additionally transitions OFF from 1 → 2 on first cycle.
        // ------------------------------------------------------------------
        if (abs_off == 2.0) {
            // Use stored reference geometry
            xl2 = d_SMSTR[0 * NUMELC + I];
            yl2 = d_SMSTR[1 * NUMELC + I];
            xl3 = d_SMSTR[2 * NUMELC + I];
            yl3 = d_SMSTR[3 * NUMELC + I];
            xl4 = d_SMSTR[4 * NUMELC + I];
            yl4 = d_SMSTR[5 * NUMELC + I];
            zl2 = 0.0;
        } else {
            // Store current geometry as reference
            d_SMSTR[0 * NUMELC + I] = xl2;
            d_SMSTR[1 * NUMELC + I] = yl2;
            d_SMSTR[2 * NUMELC + I] = xl3;
            d_SMSTR[3 * NUMELC + I] = yl3;
            d_SMSTR[4 * NUMELC + I] = xl4;
            d_SMSTR[5 * NUMELC + I] = yl4;
        }
        // ISMSTR=1: mark element as "initialized" on first cycle
        if (ISMSTR == 1 && off_i == 1.0) {
            off_i = 2.0;
        }
    }

    // --- Step 4: Shape-function derivatives (1-point quad at center) ---
    //     PX1 = ∂N/∂x diagonal 1-3,  PX2 = ∂N/∂x diagonal 2-4
    Real px1 = 0.5 * (yl2 - yl4);
    Real py1 = 0.5 * (xl4 - xl2);
    Real px2 = 0.5 * yl3;
    Real py2 = -0.5 * xl3;

    // --- Step 5: Element area ---
    Real area = 2.0 * (py2 * px1 - py1 * px2);
    area = fmax(area, 1.0e-20);
    Real a_i = 1.0 / area;

    // --- Step 6: Hourglass vectors ---
    Real vhx = (-xl2 + xl3 - xl4) * a_i;
    Real vhy = (-yl2 + yl3 - yl4) * a_i;

    // ======================================================================
    //  4.  Write all outputs to global memory
    // ======================================================================

    // Activity flag (may have been updated by ISMSTR logic)
    d_OFF[I] = off_i;

    // Local frame
    d_E1x[I] = e1x;   d_E1y[I] = e1y;   d_E1z[I] = e1z;
    d_E2x[I] = e2x;   d_E2y[I] = e2y;   d_E2z[I] = e2z;
    d_E3x[I] = e3x;   d_E3y[I] = e3y;   d_E3z[I] = e3z;

    // Gathered translational velocities
    d_VL1x[I] = vl1x;   d_VL1y[I] = vl1y;   d_VL1z[I] = vl1z;
    d_VL2x[I] = vl2x;   d_VL2y[I] = vl2y;   d_VL2z[I] = vl2z;
    d_VL3x[I] = vl3x;   d_VL3y[I] = vl3y;   d_VL3z[I] = vl3z;
    d_VL4x[I] = vl4x;   d_VL4y[I] = vl4y;   d_VL4z[I] = vl4z;

    // Gathered rotational velocities
    d_VRL1x[I] = vrl1x;   d_VRL1y[I] = vrl1y;   d_VRL1z[I] = vrl1z;
    d_VRL2x[I] = vrl2x;   d_VRL2y[I] = vrl2y;   d_VRL2z[I] = vrl2z;
    d_VRL3x[I] = vrl3x;   d_VRL3y[I] = vrl3y;   d_VRL3z[I] = vrl3z;
    d_VRL4x[I] = vrl4x;   d_VRL4y[I] = vrl4y;   d_VRL4z[I] = vrl4z;

    // Shape-function derivatives & geometry
    d_PX1[I]  = px1;
    d_PX2[I]  = px2;
    d_PY1[I]  = py1;
    d_PY2[I]  = py2;
    d_AREA[I] = area;
    d_A_I[I]  = a_i;
    d_VHX[I]  = vhx;
    d_VHY[I]  = vhy;
    d_Z2[I]   = zl2;

    // Displacement increments (non-zero only for ISMSTR=11)
    d_UX1[I] = ux1;   d_UY1[I] = uy1;
    d_UX2[I] = ux2;   d_UY2[I] = uy2;
    d_UX3[I] = ux3;   d_UY3[I] = uy3;
    d_UX4[I] = ux4;   d_UY4[I] = uy4;

    // ======================================================================
    //  5.  CDLEN3 — Characteristic length for time-step stiffness
    //      Compute ALDT² (squared characteristic length) from local coords
    //      and store in d_STI[I] as scratch for Kernel 3 (CDT3 STI formula).
    //      d_STIR[I] is zeroed.
    //
    //  CDLEN3 computes:
    //    AL1 = X2²+Y2²,  AL2 = (X3-X2)²+(Y3-Y2)²,  AL6 = X3²+Y3²
    //    AL3 = (X4-X3)²+(Y4-Y3)²,  AL4 = X4²+Y4²,  AL5 = (X4-X2)²+(Y4-Y2)²
    //    ALMIN = min(AL1,AL2,AL4[,AL3,AL5,AL6 if quad])
    //    DTDYN = AREA² / max(AL5,AL6)
    //    ALDT  = max(DTDYN, ALMIN)
    //    With hourglass correction: min(ALDT, 0.5*(ALMIN+ALDT)/max(H1,H2))
    //    Final: ALDT² is stored (before sqrt).
    // ======================================================================
    {
        // Use REFERENCE local coords for CDLEN3 (matching CPU CDERI3→CDLEN3 flow).
        // After ISMSTR logic, xl2/yl2/... contain frozen reference coords (ISMSTR=1,2)
        // and 'area' was computed from those reference coords.
        Real al1 = xl2*xl2 + yl2*yl2;
        Real al2 = (xl3-xl2)*(xl3-xl2) + (yl3-yl2)*(yl3-yl2);
        Real al6 = xl3*xl3 + yl3*yl3;
        Real al3 = (xl4-xl3)*(xl4-xl3) + (yl4-yl3)*(yl4-yl3);
        Real al4 = xl4*xl4 + yl4*yl4;
        Real al5 = (xl4-xl2)*(xl4-xl2) + (yl4-yl2)*(yl4-yl2);

        Real almin = fmin(al1, fmin(al2, al4));
        //  If node3 != node4 (quad), include al3, al5, al6
        if (al3 > 0.0) {
            Real alquad = fmin(al3, fmin(al5, al6));
            almin = fmin(almin, alquad);
        }

        // DTDYN uses reference AREA (already computed from reference coords above)
        Real dtdyn = (area * area) / fmax(fmax(al5, al6), 1.0e-20);
        Real aldt_sq = fmax(dtdyn, almin);

        //  Hourglass correction on dt: DTHOUR = 0.5*(ALMIN+ALDT)/max(H1,H2)
        //  CPU CDLEN3 uses SQUARED values: DTHOUR = 0.5*(almin_sq + aldt_sq) / hmax
        //  Then: if DTHOUR < ALDT_sq, ALDT_sq = DTHOUR
        Real hmax = fmax(H1, H2);
        if (hmax > 0.0) {
            Real dthour = 0.5 * (almin + aldt_sq) / hmax;
            if (dthour < aldt_sq) {
                aldt_sq = dthour;
            }
        }

        d_STI[I]  = aldt_sq;   // scratch: ALDT² for K3 CDT3 formula
        d_STIR[I] = 0.0;
    }
}

// ===========================================================================
//  Host-side launcher
// ===========================================================================
extern "C"
void launch_shell_geometry_kernel(
    // Node arrays (AoS: {x0,y0,z0,x1,y1,z1,...})
    const Real* d_X,
    const Real* d_V,
    const Real* d_VR,
    // Connectivity
    const int* d_N1, const int* d_N2, const int* d_N3, const int* d_N4,
    // Element state
    Real* d_OFF,   Real* d_SMSTR,
    // Outputs: local frame
    Real* d_E1x, Real* d_E1y, Real* d_E1z,
    Real* d_E2x, Real* d_E2y, Real* d_E2z,
    Real* d_E3x, Real* d_E3y, Real* d_E3z,
    // Outputs: gathered velocities
    Real* d_VL1x,  Real* d_VL1y,  Real* d_VL1z,
    Real* d_VL2x,  Real* d_VL2y,  Real* d_VL2z,
    Real* d_VL3x,  Real* d_VL3y,  Real* d_VL3z,
    Real* d_VL4x,  Real* d_VL4y,  Real* d_VL4z,
    Real* d_VRL1x, Real* d_VRL1y, Real* d_VRL1z,
    Real* d_VRL2x, Real* d_VRL2y, Real* d_VRL2z,
    Real* d_VRL3x, Real* d_VRL3y, Real* d_VRL3z,
    Real* d_VRL4x, Real* d_VRL4y, Real* d_VRL4z,
    // Outputs: shape functions & geometry
    Real* d_PX1,  Real* d_PX2,
    Real* d_PY1,  Real* d_PY2,
    Real* d_AREA, Real* d_A_I,
    Real* d_VHX,  Real* d_VHY,
    Real* d_Z2,
    // Outputs: displacement increments
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
    // CUDA launch config
    cudaStream_t stream)
{
    const int BLOCK_SIZE = 256;
    int grid_size = (NUMELC + BLOCK_SIZE - 1) / BLOCK_SIZE;

    auto launch = [&](auto ismstr_tag) {
        constexpr int ISMSTR_V = decltype(ismstr_tag)::value;
        shell_geometry_kernel<ISMSTR_V><<<grid_size, BLOCK_SIZE, 0, stream>>>(
            d_X,
            d_V,
            d_VR,
            d_N1, d_N2, d_N3, d_N4,
            d_OFF, d_SMSTR,
            d_E1x, d_E1y, d_E1z,
            d_E2x, d_E2y, d_E2z,
            d_E3x, d_E3y, d_E3z,
            d_VL1x, d_VL1y, d_VL1z,
            d_VL2x, d_VL2y, d_VL2z,
            d_VL3x, d_VL3y, d_VL3z,
            d_VL4x, d_VL4y, d_VL4z,
            d_VRL1x, d_VRL1y, d_VRL1z,
            d_VRL2x, d_VRL2y, d_VRL2z,
            d_VRL3x, d_VRL3y, d_VRL3z,
            d_VRL4x, d_VRL4y, d_VRL4z,
            d_PX1, d_PX2, d_PY1, d_PY2,
            d_AREA, d_A_I, d_VHX, d_VHY, d_Z2,
            d_UX1, d_UY1, d_UX2, d_UY2,
            d_UX3, d_UY3, d_UX4, d_UY4,
            d_STI, d_STIR, d_YM, d_THK0, d_N3_stiff, d_N4_stiff,
            NUMELC, H1, H2);
    };

    switch (ISMSTR) {
        case 1:  launch(std::integral_constant<int,1>{});  break;
        case 2:  launch(std::integral_constant<int,2>{});  break;
        case 11: launch(std::integral_constant<int,11>{}); break;
        default:
            fprintf(stderr, "Unsupported ISMSTR=%d in shell_geometry_kernel\n", ISMSTR);
            exit(EXIT_FAILURE);
    }

    // Check for kernel launch errors
    {
        cudaError_t err = cudaPeekAtLastError();
        if (err != cudaSuccess) {
            fprintf(stderr, "CUDA kernel launch error (geometry): %s\n",
                    cudaGetErrorString(err));
            exit(EXIT_FAILURE);
        }
    }
}

#endif // SHELL_GEOMETRY_KERNEL_CU
