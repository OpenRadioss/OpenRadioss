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
//  OpenRadioss GPU — Shell Element GPU Data Structures
//  Structure-of-Arrays (SoA) layout for all device arrays
// ==============================================================================

#ifndef SHELL_GPU_DATA_H
#define SHELL_GPU_DATA_H

#include <cuda_runtime.h>
#include "shell_strain_material_kernel.h"   // JohnsonCookParams
#include "shell_force_assembly_kernel.h"    // HourglassParams
#include "real_type.h"


// ---------------------------------------------------------------------------
//  Global (shared across all SUs) node/force arrays on the device.
//  Allocated once for the full mesh; each SU's kernels index into these
//  using global 0-based node IDs in their connectivity arrays.
//
//  Workflow per time step:
//    1. Host uploads NODES%X/V/VR → d_X/d_V/d_VR on global_stream
//    2. Zero d_raw_d2h on global_stream, record upload_done event
//    3. Each SU stream waits on upload_done, then runs 3 kernels
//       (K3 atomicAdd's into d_Fx..d_STIFR — safe across streams)
//    4. Each SU stream records kernels_done event
//    5. global_stream waits on all kernels_done, then D2H of d_raw_d2h
// ---------------------------------------------------------------------------
struct ShellGPUGlobal {
    int NUMNOD;             // global node count

    // --- Device node arrays (H2D each step) ---
    // d_raw_h2d [9*NUMNOD] = {x0,y0,z0, x1,y1,z1, ...} | V | VR  (AoS)
    Real* d_raw_h2d;
    Real* d_X;            // = d_raw_h2d + 0            [3*NUMNOD]
    Real* d_V;            // = d_raw_h2d + 3*NUMNOD     [3*NUMNOD]
    Real* d_VR;           // = d_raw_h2d + 6*NUMNOD     [3*NUMNOD]

    // --- Device force arrays (D2H after all kernels) ---
    // d_raw_d2h [8*NUMNOD] = Fx|Fy|Fz|Mx|My|Mz|STIFN|STIFR  (SoA)
    Real* d_raw_d2h;
    Real* d_Fx;   Real* d_Fy;   Real* d_Fz;
    Real* d_Mx;   Real* d_My;   Real* d_Mz;
    Real* d_STIFN;
    Real* d_STIFR;

    // --- Synchronization ---
    cudaStream_t stream;        // for H2D upload, zero, and D2H download
    cudaEvent_t  upload_done;   // recorded after H2D + zero; SU streams wait on this
};

// ---------------------------------------------------------------------------
//  Device memory pool for 4-node shell (COQUE) GPU computation.
//  All arrays use SoA layout for coalesced memory access.
//  Sizes: [NUMELC] per-element, [NPT*NUMELC] per-integration-point,
//         [NUMNOD] per-node (global).
// ---------------------------------------------------------------------------
struct ShellGPUData {

    // -----------------------------------------------------------------------
    //  Mesh dimensions
    // -----------------------------------------------------------------------
    int NUMELC;         // total number of 4-node shell elements
    int NUMNOD;         // total number of nodes
    int NPT;            // number of through-thickness integration points
    int ISMSTR;         // small-strain flag (1, 2, or 11)
    int ITHK;           // thickness update flag (>0 enables)
    int IHBE;           // hourglass formulation flag (>=1 needs non-uniform GAMA)
    int compute_sti;    // 1 = compute STI for STIFN (NODADT/=0 etc.), 0 = skip

    // -----------------------------------------------------------------------
    //  Material & hourglass parameters (passed by value to kernels)
    // -----------------------------------------------------------------------
    JohnsonCookParams mat;
    HourglassParams   hg;

    // -----------------------------------------------------------------------
    //  Global node arrays  [NUMNOD]
    //  Persistent on device, updated each time step by H2D transfer.
    //  d_Xx..d_VRz point into d_raw_h2d (contiguous 9*NUMNOD block).
    //  d_Fx..d_STIFR point into d_raw_d2h (contiguous 8*NUMNOD block).
    // -----------------------------------------------------------------------
    // Contiguous device buffers for single-memcpy transfers
    Real* d_raw_h2d;  // [9*NUMNOD] = Xx|Xy|Xz|Vx|Vy|Vz|VRx|VRy|VRz
    Real* d_raw_d2h;  // [8*NUMNOD] = Fx|Fy|Fz|Mx|My|Mz|STIFN|STIFR
    // Positions (pointers into d_raw_h2d)
    Real* d_X; // 3 x NUMNOD
    // Translational velocities (pointers into d_raw_h2d)
    Real* d_V;
    // Rotational velocities (pointers into d_raw_h2d)
    Real* d_VR;
    // Forces (pointers into d_raw_d2h, written by kernel, D2H each step)
    Real* d_Fx;   Real* d_Fy;   Real* d_Fz;
    // Moments (pointers into d_raw_d2h)
    Real* d_Mx;   Real* d_My;   Real* d_Mz;
    // Nodal stiffness (pointers into d_raw_d2h)
    Real* d_STIFN;
    Real* d_STIFR;

    // -----------------------------------------------------------------------
    //  Element connectivity  [NUMELC]  (read-only, uploaded once)
    // -----------------------------------------------------------------------
    int* d_N1;  int* d_N2;  int* d_N3;  int* d_N4;

    // -----------------------------------------------------------------------
    //  Element state  [NUMELC]  (persistent on device)
    // -----------------------------------------------------------------------
    Real* d_OFF;          // activity flag
    Real* d_THK;          // current thickness
    Real* d_THK0;         // initial thickness
    Real* d_THK02;        // initial thickness 2
    Real* d_SMSTR;        // reference coords [NUMELC*6]
    Real* d_GSTR;         // global strains   [NUMELC*8]
    Real* d_EINT;         // internal energy   [NUMELC*2]
    Real* d_EPSD_elem;    // element strain rate
    Real* d_HOUR;         // hourglass state   [NUMELC*5]

    // Per-element stiffness outputs
    Real* d_STI;
    Real* d_STIR;

    // Min-timestep reduction scalar (1 Real on device)
    Real* d_dt_min;

    // Per-element material scalars [NUMELC] (read-only, uploaded once)
    Real* d_SSP;
    Real* d_RHO;
    Real* d_YM;
    Real* d_NU;
    Real* d_A11;
    Real* d_G;
    Real* d_SHF;

    // -----------------------------------------------------------------------
    //  Per-integration-point state  [NPT * NUMELC]  (persistent)
    // -----------------------------------------------------------------------
    Real* d_SIGxx;     Real* d_SIGyy;     Real* d_SIGxy;
    Real* d_SIGyz;     Real* d_SIGzx;
    Real* d_PLA;
    Real* d_EPSD_ip;
    Real* d_SIGBAKxx;  Real* d_SIGBAKyy;  Real* d_SIGBAKxy;
    Real* d_DPLA;
    Real* d_TEMPEL;
    Real* d_SIGY;       // yield stress output [NUMELC]



    // -----------------------------------------------------------------------
    //  Intermediate arrays (Kernel 1 → Kernel 2 → Kernel 3)
    //  Allocated once, reused every time step.  [NUMELC] each.
    // -----------------------------------------------------------------------
    // Local frame (Kernel 1 output, Kernel 2 & 3 input)
    Real* d_E1x;  Real* d_E1y;  Real* d_E1z;
    Real* d_E2x;  Real* d_E2y;  Real* d_E2z;
    Real* d_E3x;  Real* d_E3y;  Real* d_E3z;

    // Gathered translational velocities (Kernel 1 → 2 & 3)
    Real* d_VL1x;  Real* d_VL1y;  Real* d_VL1z;
    Real* d_VL2x;  Real* d_VL2y;  Real* d_VL2z;
    Real* d_VL3x;  Real* d_VL3y;  Real* d_VL3z;
    Real* d_VL4x;  Real* d_VL4y;  Real* d_VL4z;

    // Gathered rotational velocities (Kernel 1 → 2 & 3)
    Real* d_VRL1x;  Real* d_VRL1y;  Real* d_VRL1z;
    Real* d_VRL2x;  Real* d_VRL2y;  Real* d_VRL2z;
    Real* d_VRL3x;  Real* d_VRL3y;  Real* d_VRL3z;
    Real* d_VRL4x;  Real* d_VRL4y;  Real* d_VRL4z;

    // Shape-function derivatives & geometry (Kernel 1 → 2 & 3)
    Real* d_PX1;   Real* d_PX2;
    Real* d_PY1;   Real* d_PY2;
    Real* d_AREA;  Real* d_A_I;
    Real* d_VHX;   Real* d_VHY;
    Real* d_Z2;

    // Displacement increments (Kernel 1 → 2)
    Real* d_UX1;  Real* d_UY1;
    Real* d_UX2;  Real* d_UY2;
    Real* d_UX3;  Real* d_UY3;
    Real* d_UX4;  Real* d_UY4;

    // Generalized forces / moments (Kernel 2 → 3)
    Real* d_FORxx;  Real* d_FORyy;  Real* d_FORxy;
    Real* d_FORyz;  Real* d_FORzx;
    Real* d_MOMxx;  Real* d_MOMyy;  Real* d_MOMxy;

    // -----------------------------------------------------------------------
    //  Per-SU cycle counter (for debug prints)
    // -----------------------------------------------------------------------
    int ncycle;

    // -----------------------------------------------------------------------
    //  CUDA stream for asynchronous execution
    // -----------------------------------------------------------------------
    cudaStream_t stream;

    // -----------------------------------------------------------------------
    //  Back-pointer to global (shared) node/force arrays
    //  Set by shell_gpu_set_global() after allocation.
    // -----------------------------------------------------------------------
    ShellGPUGlobal* global;

    // -----------------------------------------------------------------------
    //  Event recorded after this SU's K3 completes.
    //  The global stream waits on all SU events before D2H of forces.
    // -----------------------------------------------------------------------
    cudaEvent_t kernels_done;
};

#endif                    
