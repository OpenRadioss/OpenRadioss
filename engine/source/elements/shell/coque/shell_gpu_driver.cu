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
//  OpenRadioss GPU — Shell Element GPU Driver
//  Allocates device memory, manages host↔device transfers,
//  and launches the three Option C kernels in sequence.
// ==============================================================================

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>

#include <cuda_runtime.h>

#include "shell_gpu_data.h"
#include "shell_geometry_kernel.h"
#include "shell_strain_material_kernel.h"
#include "shell_force_assembly_kernel.h"
#include "real_type.h"


// ===========================================================================
//  CUDA error-checking helper
// ===========================================================================
#define CUDA_CHECK(call)                                                      \
    do {                                                                      \
        cudaError_t err = (call);                                             \
        if (err != cudaSuccess) {                                             \
            fprintf(stderr, "CUDA error at %s:%d — %s\n",                    \
                    __FILE__, __LINE__, cudaGetErrorString(err));             \
            exit(EXIT_FAILURE);                                               \
        }                                                                     \
    } while (0)

// ===========================================================================
//  Helper: allocate a Real array on device and zero it
// ===========================================================================
static inline Real* alloc_d(int n)
{
    Real* ptr = nullptr;
    CUDA_CHECK(cudaMalloc(&ptr, (size_t)n * sizeof(Real)));
    CUDA_CHECK(cudaMemset(ptr, 0, (size_t)n * sizeof(Real)));
    return ptr;
}

static inline int* alloc_i(int n)
{
    int* ptr = nullptr;
    CUDA_CHECK(cudaMalloc(&ptr, (size_t)n * sizeof(int)));
    CUDA_CHECK(cudaMemset(ptr, 0, (size_t)n * sizeof(int)));
    return ptr;
}

// ===========================================================================
//  Helper: safe free
// ===========================================================================
static inline void free_d(Real*& p) { if (p) { cudaFree(p); p = nullptr; } }
static inline void free_i(int*& p)    { if (p) { cudaFree(p); p = nullptr; } }

// ===========================================================================
//  0. GLOBAL NODE/FORCE HANDLE — shared across all SU streams
// ===========================================================================

// ---------------------------------------------------------------------------
//  Create and allocate the global handle for NUMNOD nodes.
//  Allocates d_raw_h2d [9*NUMNOD] and d_raw_d2h [8*NUMNOD] on device,
//  creates a dedicated CUDA stream and event.
// ---------------------------------------------------------------------------
extern "C"
ShellGPUGlobal* shell_gpu_global_create(int NUMNOD)
{
    ShellGPUGlobal* gh = (ShellGPUGlobal*)calloc(1, sizeof(ShellGPUGlobal));
    gh->NUMNOD = NUMNOD;
    const int NN = NUMNOD;

    // H2D buffer: 9*NN Reals = X(3*NN) | V(3*NN) | VR(3*NN)  (AoS)
    gh->d_raw_h2d = alloc_d(9 * NN);
    gh->d_X  = gh->d_raw_h2d;
    gh->d_V  = gh->d_raw_h2d + 3 * NN;
    gh->d_VR = gh->d_raw_h2d + 6 * NN;

    // D2H buffer: 8*NN Reals = Fx|Fy|Fz|Mx|My|Mz|STIFN|STIFR  (SoA)
    gh->d_raw_d2h = alloc_d(8 * NN);
    gh->d_Fx    = gh->d_raw_d2h + 0 * NN;
    gh->d_Fy    = gh->d_raw_d2h + 1 * NN;
    gh->d_Fz    = gh->d_raw_d2h + 2 * NN;
    gh->d_Mx    = gh->d_raw_d2h + 3 * NN;
    gh->d_My    = gh->d_raw_d2h + 4 * NN;
    gh->d_Mz    = gh->d_raw_d2h + 5 * NN;
    gh->d_STIFN = gh->d_raw_d2h + 6 * NN;
    gh->d_STIFR = gh->d_raw_d2h + 7 * NN;

    CUDA_CHECK(cudaStreamCreate(&gh->stream));
    CUDA_CHECK(cudaEventCreateWithFlags(&gh->upload_done, cudaEventDisableTiming));

    fprintf(stdout, " [GPU-GLOBAL] Created global handle: NUMNOD=%d, "
            "d_raw_h2d=%zu MB, d_raw_d2h=%zu MB\n",
            NN,
            (size_t)(9*NN)*sizeof(Real) / (1024*1024),
            (size_t)(8*NN)*sizeof(Real) / (1024*1024));

    return gh;
}

// ---------------------------------------------------------------------------
//  Destroy the global handle — free device arrays, stream, event.
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_global_destroy(ShellGPUGlobal* gh)
{
    if (!gh) return;
    free_d(gh->d_raw_h2d);
    free_d(gh->d_raw_d2h);
    if (gh->stream)      { cudaStreamDestroy(gh->stream);  gh->stream = nullptr; }
    if (gh->upload_done) { cudaEventDestroy(gh->upload_done); gh->upload_done = nullptr; }
    free(gh);
}

// ---------------------------------------------------------------------------
//  Upload global node positions/velocities (H2D) and zero force arrays.
//  Records upload_done event so that SU streams can wait on it.
//
//  X, V, VR are Fortran (3, NUMNOD) column-major = AoS in memory:
//    {x0,y0,z0, x1,y1,z1, ...}  — matches d_X[3*n + {0,1,2}] access
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_global_upload_nodes(ShellGPUGlobal* gh,
                                   const Real* X,
                                   const Real* V,
                                   const Real* VR)
{
    const int NN = gh->NUMNOD;
    const size_t s3n = (size_t)(3 * NN) * sizeof(Real);
    const size_t s8n = (size_t)(8 * NN) * sizeof(Real);
    cudaStream_t s = gh->stream;

    // H2D: X, V, VR → d_raw_h2d
    CUDA_CHECK(cudaMemcpyAsync(gh->d_X,  X,  s3n, cudaMemcpyHostToDevice, s));
    CUDA_CHECK(cudaMemcpyAsync(gh->d_V,  V,  s3n, cudaMemcpyHostToDevice, s));
    CUDA_CHECK(cudaMemcpyAsync(gh->d_VR, VR, s3n, cudaMemcpyHostToDevice, s));

    // Zero force accumulation arrays (Fx..STIFR) — contiguous block
    CUDA_CHECK(cudaMemsetAsync(gh->d_raw_d2h, 0, s8n, s));

    // Signal that upload + zero is complete — SU streams will wait on this
    CUDA_CHECK(cudaEventRecord(gh->upload_done, s));
}

// ---------------------------------------------------------------------------
//  Download global force arrays (D2H) into a pinned host buffer.
//  raw_gpu_to_cpu is [8*NUMNOD]: Fx|Fy|Fz|Mx|My|Mz|STIFN|STIFR
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_global_download_forces(ShellGPUGlobal* gh,
                                      Real* raw_gpu_to_cpu)
{
    const size_t s8n = (size_t)(8 * gh->NUMNOD) * sizeof(Real);
    CUDA_CHECK(cudaMemcpyAsync(raw_gpu_to_cpu, gh->d_raw_d2h, s8n,
                               cudaMemcpyDeviceToHost, gh->stream));
}

// ---------------------------------------------------------------------------
//  Block CPU until global stream (upload or download) completes.
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_global_synchronize(ShellGPUGlobal* gh)
{
    CUDA_CHECK(cudaStreamSynchronize(gh->stream));
}

// ---------------------------------------------------------------------------
//  Make an SU stream wait until the global upload is done.
//  Call this before launching SU kernels.
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_global_wait_upload(ShellGPUGlobal* gh, ShellGPUData* g)
{
    CUDA_CHECK(cudaStreamWaitEvent(g->stream, gh->upload_done, 0));
}

// ---------------------------------------------------------------------------
//  Make the global stream wait until an SU's kernels are done.
//  Call this after all SU kernel launches, before global D2H.
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_global_wait_su(ShellGPUGlobal* gh, ShellGPUData* g)
{
    CUDA_CHECK(cudaStreamWaitEvent(gh->stream, g->kernels_done, 0));
}

// ---------------------------------------------------------------------------
//  Pin Fortran NODES%X, %V, %VR for truly async H2D.
//  Also pin the D2H force result buffer.
//  Call once after Fortran arrays are allocated.
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_global_pin_host(const Real* X, const Real* V,
                               const Real* VR, Real* raw_gpu_to_cpu,
                               int NUMNOD)
{
    const size_t s3n = (size_t)(3 * NUMNOD) * sizeof(Real);
    const size_t s8n = (size_t)(8 * NUMNOD) * sizeof(Real);
    CUDA_CHECK(cudaHostRegister((void*)X,  s3n, cudaHostRegisterDefault));
    CUDA_CHECK(cudaHostRegister((void*)V,  s3n, cudaHostRegisterDefault));
    CUDA_CHECK(cudaHostRegister((void*)VR, s3n, cudaHostRegisterDefault));
    CUDA_CHECK(cudaHostRegister(raw_gpu_to_cpu, s8n, cudaHostRegisterDefault));
}

// ---------------------------------------------------------------------------
//  Set the back-pointer from a per-SU handle to the global handle.
//  Also aliases the node/force pointers for convenience.
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_set_global(ShellGPUData* g, ShellGPUGlobal* gh)
{
    g->global = gh;

    // Alias device node arrays (read-only for K1)
    g->d_X  = gh->d_X;
    g->d_V  = gh->d_V;
    g->d_VR = gh->d_VR;

    // Alias device force arrays (atomicAdd by K3)
    g->d_Fx    = gh->d_Fx;
    g->d_Fy    = gh->d_Fy;
    g->d_Fz    = gh->d_Fz;
    g->d_Mx    = gh->d_Mx;
    g->d_My    = gh->d_My;
    g->d_Mz    = gh->d_Mz;
    g->d_STIFN = gh->d_STIFN;
    g->d_STIFR = gh->d_STIFR;
}

// ===========================================================================
//  0a. CREATE / DESTROY the host-side ShellGPUData handle
//      These are needed because the Fortran side cannot sizeof() a C struct.
// ===========================================================================
extern "C"
ShellGPUData* shell_gpu_data_create()
{
    ShellGPUData* g = (ShellGPUData*)calloc(1, sizeof(ShellGPUData));
    return g;
}

extern "C"
void shell_gpu_data_destroy(ShellGPUData* g)
{
    if (g) free(g);
}

// ===========================================================================
//  0b. SET material (Johnson-Cook) and hourglass parameters on the handle
//      These are by-value structs inside ShellGPUData, passed to kernels 2&3.
//      They must be set after shell_gpu_allocate() and before the first
//      call to shell_gpu_run_kernels() / shell_gpu_full_step().
// ===========================================================================
extern "C"
void shell_gpu_set_mat_params(ShellGPUData* g,
    Real E,  Real nu, Real G,  Real A11, Real A12,
    Real CA, Real CB, Real CN, Real CC,  Real EPDR,
    Real EPMX, Real YMAX,  Real M_EXP,
    Real FISOKIN, Real RHOCP, Real TREF, Real TMELT, Real ASRATE,
    Real RHO, Real SSP, Real SHF_COEF,
    int IPLA, int VP, int IFORM, int ICC,
    Real Z3, Real Z4)
{
    g->mat.E        = E;
    g->mat.nu       = nu;
    g->mat.G        = G;
    g->mat.A11      = A11;
    g->mat.A12      = A12;
    g->mat.CA       = CA;
    g->mat.CB       = CB;
    g->mat.CN       = CN;
    g->mat.CC       = CC;
    g->mat.EPDR     = EPDR;
    g->mat.EPMX     = EPMX;
    g->mat.YMAX     = YMAX;
    g->mat.M_EXP    = M_EXP;
    g->mat.FISOKIN  = FISOKIN;
    g->mat.RHOCP    = RHOCP;
    g->mat.TREF     = TREF;
    g->mat.TMELT    = TMELT;
    g->mat.ASRATE   = ASRATE;
    g->mat.RHO      = RHO;
    g->mat.SSP      = SSP;
    g->mat.SHF_COEF = SHF_COEF;
    g->mat.IPLA     = IPLA;
    g->mat.VP       = VP;
    g->mat.IFORM    = IFORM;
    g->mat.ICC      = ICC;
    g->mat.Z3       = Z3;
    g->mat.Z4       = Z4;
}

extern "C"
void shell_gpu_set_hg_params(ShellGPUData* g,
    Real H1,  Real H2,  Real H3,
    Real SRH1, Real SRH2, Real SRH3,
    Real HVISC, Real HELAS, Real HVLIN)
{
    g->hg.H1   = H1;
    g->hg.H2   = H2;
    g->hg.H3   = H3;
    g->hg.SRH1 = SRH1;
    g->hg.SRH2 = SRH2;
    g->hg.SRH3 = SRH3;
    g->hg.HVISC = HVISC;
    g->hg.HELAS = HELAS;
    g->hg.HVLIN = HVLIN;
}

// ---------------------------------------------------------------------------
//  Set compute_sti flag: 1 = compute element stiffness for STIFN (NODADT/=0),
//                        0 = skip (NODADT==0 and IDT1SH!=1 and IDTMINS!=2)
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_set_compute_sti(ShellGPUData* g, int flag)
{
    g->compute_sti = flag;
}

extern "C"
void shell_gpu_set_ihbe(ShellGPUData* g, int ihbe)
{
    g->IHBE = ihbe;
}

// ===========================================================================
//  1. ALLOCATE all device arrays
// ===========================================================================
extern "C"
void shell_gpu_allocate(ShellGPUData* g,
                        int NUMELC, int NUMNOD, int NPT,
                        int ISMSTR, int ITHK)
{
    memset(g, 0, sizeof(ShellGPUData));

    g->NUMELC = NUMELC;
    g->NUMNOD = NUMNOD;
    g->NPT    = NPT;
    g->ISMSTR = ISMSTR;
    g->ITHK   = ITHK;

    CUDA_CHECK(cudaStreamCreate(&g->stream));
    CUDA_CHECK(cudaEventCreateWithFlags(&g->kernels_done, cudaEventDisableTiming));

    const int NE  = NUMELC;
    const int NN  = NUMNOD;
    const int NIP = NPT * NUMELC;

    // ----- Global node / force arrays are NOT allocated here -----
    // They live on ShellGPUGlobal and are aliased via shell_gpu_set_global().
    // d_raw_h2d, d_raw_d2h, d_X, d_V, d_VR, d_Fx..d_STIFR are set later.
    g->d_raw_h2d = nullptr;
    g->d_raw_d2h = nullptr;
    g->d_X = g->d_V = g->d_VR = nullptr;
    g->d_Fx = g->d_Fy = g->d_Fz = nullptr;
    g->d_Mx = g->d_My = g->d_Mz = nullptr;
    g->d_STIFN = g->d_STIFR = nullptr;
    g->global = nullptr;

    // ----- Connectivity [NUMELC] -----
    g->d_N1 = alloc_i(NE);  g->d_N2 = alloc_i(NE);
    g->d_N3 = alloc_i(NE);  g->d_N4 = alloc_i(NE);

    // ----- Element state [NUMELC] -----
    g->d_OFF       = alloc_d(NE);
    g->d_THK       = alloc_d(NE);
    g->d_THK0      = alloc_d(NE);
    g->d_THK02     = alloc_d(NE);
    g->d_SMSTR     = alloc_d(NE * 6);
    g->d_GSTR      = alloc_d(NE * 8);
    g->d_EINT      = alloc_d(NE * 2);
    g->d_EPSD_elem = alloc_d(NE);
    g->d_HOUR      = alloc_d(NE * 5);
    g->d_STI       = alloc_d(NE);
    g->d_STIR      = alloc_d(NE);
    g->d_dt_min    = alloc_d(1);

    // Per-element material scalars (read-only on device)
    g->d_SSP = alloc_d(NE);
    g->d_RHO = alloc_d(NE);
    g->d_YM  = alloc_d(NE);
    g->d_NU  = alloc_d(NE);
    g->d_A11 = alloc_d(NE);
    g->d_G   = alloc_d(NE);
    g->d_SHF = alloc_d(NE);

    // ----- Per-integration-point state [NPT * NUMELC] -----
    g->d_SIGxx    = alloc_d(NIP);  g->d_SIGyy    = alloc_d(NIP);
    g->d_SIGxy    = alloc_d(NIP);  g->d_SIGyz    = alloc_d(NIP);
    g->d_SIGzx    = alloc_d(NIP);
    g->d_PLA      = alloc_d(NIP);
    g->d_EPSD_ip  = alloc_d(NIP);
    g->d_SIGBAKxx = alloc_d(NIP);  g->d_SIGBAKyy = alloc_d(NIP);
    g->d_SIGBAKxy = alloc_d(NIP);
    g->d_DPLA     = alloc_d(NIP);
    g->d_TEMPEL   = alloc_d(NIP);
    g->d_SIGY     = alloc_d(NE);

    // ----- Intermediate arrays [NUMELC] -----
    // Local frame
    g->d_E1x = alloc_d(NE);  g->d_E1y = alloc_d(NE);  g->d_E1z = alloc_d(NE);
    g->d_E2x = alloc_d(NE);  g->d_E2y = alloc_d(NE);  g->d_E2z = alloc_d(NE);
    g->d_E3x = alloc_d(NE);  g->d_E3y = alloc_d(NE);  g->d_E3z = alloc_d(NE);

    // Gathered translational velocities (4 nodes × 3 components)
    g->d_VL1x = alloc_d(NE);  g->d_VL1y = alloc_d(NE);  g->d_VL1z = alloc_d(NE);
    g->d_VL2x = alloc_d(NE);  g->d_VL2y = alloc_d(NE);  g->d_VL2z = alloc_d(NE);
    g->d_VL3x = alloc_d(NE);  g->d_VL3y = alloc_d(NE);  g->d_VL3z = alloc_d(NE);
    g->d_VL4x = alloc_d(NE);  g->d_VL4y = alloc_d(NE);  g->d_VL4z = alloc_d(NE);

    // Gathered rotational velocities
    g->d_VRL1x = alloc_d(NE);  g->d_VRL1y = alloc_d(NE);  g->d_VRL1z = alloc_d(NE);
    g->d_VRL2x = alloc_d(NE);  g->d_VRL2y = alloc_d(NE);  g->d_VRL2z = alloc_d(NE);
    g->d_VRL3x = alloc_d(NE);  g->d_VRL3y = alloc_d(NE);  g->d_VRL3z = alloc_d(NE);
    g->d_VRL4x = alloc_d(NE);  g->d_VRL4y = alloc_d(NE);  g->d_VRL4z = alloc_d(NE);

    // Shape functions & geometry
    g->d_PX1  = alloc_d(NE);  g->d_PX2 = alloc_d(NE);
    g->d_PY1  = alloc_d(NE);  g->d_PY2 = alloc_d(NE);
    g->d_AREA = alloc_d(NE);  g->d_A_I = alloc_d(NE);
    g->d_VHX  = alloc_d(NE);  g->d_VHY = alloc_d(NE);
    g->d_Z2   = alloc_d(NE);

    // Displacement increments
    g->d_UX1 = alloc_d(NE);  g->d_UY1 = alloc_d(NE);
    g->d_UX2 = alloc_d(NE);  g->d_UY2 = alloc_d(NE);
    g->d_UX3 = alloc_d(NE);  g->d_UY3 = alloc_d(NE);
    g->d_UX4 = alloc_d(NE);  g->d_UY4 = alloc_d(NE);

    // Generalized forces / moments  (Kernel 2 → 3)
    g->d_FORxx = alloc_d(NE);  g->d_FORyy = alloc_d(NE);
    g->d_FORxy = alloc_d(NE);  g->d_FORyz = alloc_d(NE);
    g->d_FORzx = alloc_d(NE);
    g->d_MOMxx = alloc_d(NE);  g->d_MOMyy = alloc_d(NE);
    g->d_MOMxy = alloc_d(NE);
}

// ===========================================================================
//  2. DEALLOCATE all device arrays
// ===========================================================================
extern "C"
void shell_gpu_deallocate(ShellGPUData* g)
{
    // Global node/force arrays are NOT freed here — they belong to ShellGPUGlobal.
    // Just null out the aliased pointers.
    g->d_raw_h2d = nullptr;
    g->d_raw_d2h = nullptr;
    g->d_X = g->d_V = g->d_VR = nullptr;
    g->d_Fx = g->d_Fy = g->d_Fz = nullptr;
    g->d_Mx = g->d_My = g->d_Mz = nullptr;
    g->d_STIFN = g->d_STIFR = nullptr;
    g->global = nullptr;

    // Connectivity
    free_i(g->d_N1);  free_i(g->d_N2);  free_i(g->d_N3);  free_i(g->d_N4);

    // Element state
    free_d(g->d_OFF);   free_d(g->d_THK);
    free_d(g->d_THK0);  free_d(g->d_THK02);
    free_d(g->d_SMSTR); free_d(g->d_GSTR);
    free_d(g->d_EINT);  free_d(g->d_EPSD_elem);
    free_d(g->d_HOUR);
    free_d(g->d_STI);   free_d(g->d_STIR);
    free_d(g->d_dt_min);
    free_d(g->d_SSP);   free_d(g->d_RHO);  free_d(g->d_YM);
    free_d(g->d_NU);    free_d(g->d_A11);  free_d(g->d_G);
    free_d(g->d_SHF);

    // Per-integration-point state
    free_d(g->d_SIGxx);    free_d(g->d_SIGyy);    free_d(g->d_SIGxy);
    free_d(g->d_SIGyz);    free_d(g->d_SIGzx);
    free_d(g->d_PLA);      free_d(g->d_EPSD_ip);
    free_d(g->d_SIGBAKxx); free_d(g->d_SIGBAKyy); free_d(g->d_SIGBAKxy);
    free_d(g->d_DPLA);     free_d(g->d_TEMPEL);
    free_d(g->d_SIGY);

    // Intermediate arrays
    free_d(g->d_E1x);  free_d(g->d_E1y);  free_d(g->d_E1z);
    free_d(g->d_E2x);  free_d(g->d_E2y);  free_d(g->d_E2z);
    free_d(g->d_E3x);  free_d(g->d_E3y);  free_d(g->d_E3z);

    free_d(g->d_VL1x); free_d(g->d_VL1y); free_d(g->d_VL1z);
    free_d(g->d_VL2x); free_d(g->d_VL2y); free_d(g->d_VL2z);
    free_d(g->d_VL3x); free_d(g->d_VL3y); free_d(g->d_VL3z);
    free_d(g->d_VL4x); free_d(g->d_VL4y); free_d(g->d_VL4z);

    free_d(g->d_VRL1x); free_d(g->d_VRL1y); free_d(g->d_VRL1z);
    free_d(g->d_VRL2x); free_d(g->d_VRL2y); free_d(g->d_VRL2z);
    free_d(g->d_VRL3x); free_d(g->d_VRL3y); free_d(g->d_VRL3z);
    free_d(g->d_VRL4x); free_d(g->d_VRL4y); free_d(g->d_VRL4z);

    free_d(g->d_PX1);  free_d(g->d_PX2);
    free_d(g->d_PY1);  free_d(g->d_PY2);
    free_d(g->d_AREA); free_d(g->d_A_I);
    free_d(g->d_VHX);  free_d(g->d_VHY);
    free_d(g->d_Z2);

    free_d(g->d_UX1);  free_d(g->d_UY1);
    free_d(g->d_UX2);  free_d(g->d_UY2);
    free_d(g->d_UX3);  free_d(g->d_UY3);
    free_d(g->d_UX4);  free_d(g->d_UY4);

    free_d(g->d_FORxx); free_d(g->d_FORyy); free_d(g->d_FORxy);
    free_d(g->d_FORyz); free_d(g->d_FORzx);
    free_d(g->d_MOMxx); free_d(g->d_MOMyy); free_d(g->d_MOMxy);

    if (g->stream) {
        cudaStreamDestroy(g->stream);
        g->stream = nullptr;
    }
    if (g->kernels_done) {
        cudaEventDestroy(g->kernels_done);
        g->kernels_done = nullptr;
    }

    memset(g, 0, sizeof(ShellGPUData));
}

// ===========================================================================
//  3. HOST → DEVICE TRANSFERS
// ===========================================================================

// ---------------------------------------------------------------------------
//  Upload constant data (called once at initialization)
//  - Connectivity, initial thickness, material scalars, initial state
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_upload_constant(ShellGPUData*  g,
                               const int*     h_N1,   const int*     h_N2,
                               const int*     h_N3,   const int*     h_N4,
                               const Real*  h_THK0,
                               const Real*  h_OFF,
                               const Real*  h_SSP,  const Real*  h_RHO,
                               const Real*  h_YM,   const Real*  h_NU,
                               const Real*  h_A11,  const Real*  h_G,
                               const Real*  h_SHF)
{
    const int NE = g->NUMELC;
    const size_t se = (size_t)NE * sizeof(Real);
    const size_t si = (size_t)NE * sizeof(int);

    // Connectivity (never changes)
    CUDA_CHECK(cudaMemcpyAsync(g->d_N1, h_N1, si, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_N2, h_N2, si, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_N3, h_N3, si, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_N4, h_N4, si, cudaMemcpyHostToDevice, g->stream));

    // Initial thickness → THK0 and THK02 = THK0/2
    CUDA_CHECK(cudaMemcpyAsync(g->d_THK0, h_THK0, se, cudaMemcpyHostToDevice, g->stream));
    // THK = THK0 initially
    CUDA_CHECK(cudaMemcpyAsync(g->d_THK, h_THK0, se, cudaMemcpyHostToDevice, g->stream));

    // Compute THK02 = THK0^2 on host and upload
    //   (CPU ccoef3.F:  THK02(I) = THK0(I)*THK0(I))
    {
        Real* tmp = (Real*)malloc(se);
        for (int i = 0; i < NE; ++i) tmp[i] = h_THK0[i] * h_THK0[i];
        CUDA_CHECK(cudaMemcpyAsync(g->d_THK02, tmp, se, cudaMemcpyHostToDevice, g->stream));
        cudaStreamSynchronize(g->stream);
        free(tmp);
    }

    // Element activity
    CUDA_CHECK(cudaMemcpyAsync(g->d_OFF, h_OFF, se, cudaMemcpyHostToDevice, g->stream));

    // Material scalars per element
    CUDA_CHECK(cudaMemcpyAsync(g->d_SSP, h_SSP, se, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_RHO, h_RHO, se, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_YM,  h_YM,  se, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_NU,  h_NU,  se, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_A11, h_A11, se, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_G,   h_G,   se, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_SHF, h_SHF, se, cudaMemcpyHostToDevice, g->stream));

    CUDA_CHECK(cudaStreamSynchronize(g->stream));
}

// ---------------------------------------------------------------------------
//  Upload per-integration-point initial state (called once)
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_upload_ip_state(ShellGPUData*  g,
                               const Real*  h_SIGxx,    const Real*  h_SIGyy,
                               const Real*  h_SIGxy,    const Real*  h_SIGyz,
                               const Real*  h_SIGzx,
                               const Real*  h_PLA,      const Real*  h_EPSD_ip,
                               const Real*  h_SIGBAKxx, const Real*  h_SIGBAKyy,
                               const Real*  h_SIGBAKxy,
                               const Real*  h_TEMPEL)
{
    const size_t sip = (size_t)(g->NPT * g->NUMELC) * sizeof(Real);

    CUDA_CHECK(cudaMemcpyAsync(g->d_SIGxx,    h_SIGxx,    sip, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_SIGyy,    h_SIGyy,    sip, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_SIGxy,    h_SIGxy,    sip, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_SIGyz,    h_SIGyz,    sip, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_SIGzx,    h_SIGzx,    sip, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_PLA,      h_PLA,      sip, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_EPSD_ip,  h_EPSD_ip,  sip, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_SIGBAKxx, h_SIGBAKxx, sip, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_SIGBAKyy, h_SIGBAKyy, sip, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_SIGBAKxy, h_SIGBAKxy, sip, cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_TEMPEL,   h_TEMPEL,   sip, cudaMemcpyHostToDevice, g->stream));

    CUDA_CHECK(cudaStreamSynchronize(g->stream));
}

// ---------------------------------------------------------------------------
//  Upload node positions and velocities (called each time step before kernels)
// ---------------------------------------------------------------------------
//  PROFILING (nsys, 28440 steps):
//    1 H2D per step → 28,648 total H2D calls, 278 MB total.
//    GPU-side H2D: 17.5ms total (30.8% of GPU memops). ~0.6µs avg per call.
//    CPU-side cudaMemcpyAsync: 47µs avg — the high avg is because host memory
//    is NOT pinned, so cudaMemcpyAsync falls back to synchronous staging.
//
//  CRITICAL OPTIMIZATION — PIN HOST MEMORY:
//    cudaHostRegister(raw_cpu_to_gpu, 9*NUMNOD*8, cudaHostRegisterDefault)
//    once at init time.  This would:
//    a) Make cudaMemcpyAsync truly asynchronous (~2µs CPU time per call)
//    b) Allow the 3-pass SU pipeline to actually overlap CPU gather of
//       SU(k+1) with GPU execution of SU(k)
//    c) Reduce the 4.07s cudaMemcpyAsync total by ~3x
//    This is THE single most impactful remaining optimization.
//
//  Long-term: keep node data GPU-resident (GPU time integration).
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_upload_nodes(ShellGPUData*  g,
                            const Real*  X,
                            const Real*  V,
                            const Real*  VR)
{
    const size_t s3n = (size_t)(3 * g->NUMNOD) * sizeof(Real);

    // ONE H2D transfer: host raw_cpu_to_gpu[9*NUMNOD] → device d_raw_h2d[9*NUMNOD]
    // Both sides have identical SoA layout: [Xx|Xy|Xz|Vx|Vy|Vz|VRx|VRy|VRz]
    CUDA_CHECK(cudaMemcpyAsync(g->d_raw_h2d, X, s3n,
                               cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_raw_h2d + 3 * g->NUMNOD, V, s3n,
                               cudaMemcpyHostToDevice, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(g->d_raw_h2d + 6 * g->NUMNOD, VR, s3n,
                               cudaMemcpyHostToDevice, g->stream));

    // No cudaStreamSynchronize needed: run_kernels uses the same stream,
    // so the H2D will complete before any kernel reads the data.
}

// ===========================================================================
//  4. DEVICE → HOST TRANSFERS
// ===========================================================================

// ---------------------------------------------------------------------------
//  Download nodal forces, moments, stiffness (synchronous variant)
// ---------------------------------------------------------------------------
//  PROFILING (nsys, 28440 steps):
//    1 D2H per step → ~28K of the 57,712 D2H calls, ~10 KB avg.
//    GPU-side D2H: ~11ms total.  CPU-side: ~47µs avg (unpinned memory).
//    With pinned memory this would drop to ~2µs avg.
//
//  NOTE: The 3-pass SU pipeline (shell_gpu_full_step_async) avoids this
//  synchronous variant entirely. This function is kept for non-pipelined
//  callers (shell_gpu_full_step). Prefer the async path.
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_download_nodal_forces(const ShellGPUData* g,
                                     Real* raw_gpu_to_cpu)
{
    const size_t s8n = (size_t)(8 * g->NUMNOD) * sizeof(Real);

    // ONE D2H transfer: device d_raw_d2h[8*NUMNOD] → host raw_gpu_to_cpu[8*NUMNOD]
    CUDA_CHECK(cudaMemcpyAsync(raw_gpu_to_cpu, g->d_raw_d2h, s8n,
                               cudaMemcpyDeviceToHost, g->stream));

    // COST: ~5-10µs per call × 28K steps ≈ 140-280ms of CPU stall
    CUDA_CHECK(cudaStreamSynchronize(g->stream));
}

// ---------------------------------------------------------------------------
//  Download element energy (for output / energy balance checking)
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_download_energy(const ShellGPUData* g,
                               Real* h_EINT)
{
    const size_t se2 = (size_t)(g->NUMELC * 2) * sizeof(Real);
    CUDA_CHECK(cudaMemcpyAsync(h_EINT, g->d_EINT, se2, cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaStreamSynchronize(g->stream));
}

// ---------------------------------------------------------------------------
//  Download per-element ALDT² (synchronous variant)
//  Kernel 1 writes ALDT² into d_STI[]; Kernel 3 only reads it.
// ---------------------------------------------------------------------------
//  PROFILING (nsys, 28440 steps):
//    1 D2H per step → ~28K of the 57,712 D2H calls, ~1 KB avg.
//    With the 3-pass pipeline, prefer shell_gpu_download_aldt_sq_async
//    + deferred shell_gpu_synchronize to avoid per-SU sync.
//
//  OPTIMIZATION:  Replace with a GPU reduction kernel computing
//    min(sqrt(aldt_sq) * dtfac / ssp) and download a single scalar.
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_download_aldt_sq(const ShellGPUData* g,
                                Real* h_aldt_sq)
{
    const size_t se = (size_t)g->NUMELC * sizeof(Real);
    CUDA_CHECK(cudaMemcpyAsync(h_aldt_sq, g->d_STI, se, cudaMemcpyDeviceToHost, g->stream));

    // COST: blocks CPU until all prior GPU work completes.  ~5-10µs × 28K steps
    CUDA_CHECK(cudaStreamSynchronize(g->stream));
}

// ---------------------------------------------------------------------------
//  Download full element + IP state (for output / restart)
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_download_state(const ShellGPUData* g,
                              Real* h_OFF,   Real* h_THK,
                              Real* h_GSTR,  Real* h_EPSD_elem,
                              Real* h_SIGxx, Real* h_SIGyy,
                              Real* h_SIGxy, Real* h_SIGyz,
                              Real* h_SIGzx,
                              Real* h_PLA,   Real* h_EPSD_ip,
                              Real* h_SIGBAKxx, Real* h_SIGBAKyy,
                              Real* h_SIGBAKxy,
                              Real* h_TEMPEL)
{
    const int NE  = g->NUMELC;
    const int NIP = g->NPT * NE;
    const size_t se  = (size_t)NE  * sizeof(Real);
    const size_t sip = (size_t)NIP * sizeof(Real);

    CUDA_CHECK(cudaMemcpyAsync(h_OFF,       g->d_OFF,       se,  cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_THK,       g->d_THK,       se,  cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_GSTR,      g->d_GSTR,  (size_t)(NE*8)*sizeof(Real), cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_EPSD_elem, g->d_EPSD_elem, se,  cudaMemcpyDeviceToHost, g->stream));

    CUDA_CHECK(cudaMemcpyAsync(h_SIGxx,    g->d_SIGxx,    sip, cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_SIGyy,    g->d_SIGyy,    sip, cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_SIGxy,    g->d_SIGxy,    sip, cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_SIGyz,    g->d_SIGyz,    sip, cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_SIGzx,    g->d_SIGzx,    sip, cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_PLA,      g->d_PLA,      sip, cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_EPSD_ip,  g->d_EPSD_ip,  sip, cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_SIGBAKxx, g->d_SIGBAKxx, sip, cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_SIGBAKyy, g->d_SIGBAKyy, sip, cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_SIGBAKxy, g->d_SIGBAKxy, sip, cudaMemcpyDeviceToHost, g->stream));
    CUDA_CHECK(cudaMemcpyAsync(h_TEMPEL,   g->d_TEMPEL,   sip, cudaMemcpyDeviceToHost, g->stream));

    CUDA_CHECK(cudaStreamSynchronize(g->stream));
}

// ===========================================================================
//  5. ZERO NODAL ACCUMULATION ARRAYS
//     Must be called once per time step before running the kernels
//     (forces/moments/stiffness are accumulated via atomicAdd).
// ===========================================================================
//  PROFILING (nsys, 28440 steps):
//    1 memset per step → 28,440 calls (+ 760 from init = 29,200 total).
//    CPU-side: 139ms total (2.8% of CUDA API), avg 4.9µs. Negligible.
//    GPU-side: 16.4ms total (28.9% of GPU memops), avg 0.56µs.
//  Could be folded into Kernel 1 as first-touch writes to eliminate.
// ===========================================================================
extern "C"
void shell_gpu_zero_nodal_arrays(ShellGPUData* g)
{
    // Force arrays now live on ShellGPUGlobal and are zeroed there
    // (inside shell_gpu_global_upload_nodes).  This is a no-op.
    (void)g;
}

// ===========================================================================
//  6. RUN ALL THREE KERNELS FOR ONE TIME STEP
//     This is the main entry point called by the Fortran time-stepping loop.
//
//     Sequence:
//       a) Zero nodal F/M/STIF arrays
//       b) Kernel 1: Geometry  (CCOOR3 + CNVEC3 + CDERI3)
//       c) Kernel 2: Strains + Material  (CCOEF3 + CDEFO3 + CCURV3 +
//                                         CSTRA3 + CMAIN3/SIGEPS02C/M2CPLR)
//       d) Kernel 3: Forces + Assembly  (CHVIS3 + CFINT3 + CUPDT3)
//
//     The three kernel launches are enqueued into the same CUDA stream,
//     so they execute in order. No explicit synchronization between them
//     is needed; the stream guarantees ordering.
// ===========================================================================
extern "C"
void shell_gpu_run_kernels(ShellGPUData* g, Real dt)
{
    const int NE     = g->NUMELC;
    const int NPT    = g->NPT;
    const int ISMSTR = g->ISMSTR;
    const int ITHK   = g->ITHK;
    cudaStream_t s   = g->stream;

    // --- Per-SU cycle counter (stored in ShellGPUData, not static) ---
    g->ncycle++;
    const int ncycle_gpu = g->ncycle;

    // (a) Zero nodal accumulation arrays
    shell_gpu_zero_nodal_arrays(g);

    // ------------------------------------------------------------------
    //  (b) Kernel 1: Geometry
    //      Gathers node coords/velocities, builds convected frame,
    //      computes shape-function derivatives.
    //  PROFILING: 9.8% GPU compute, 176ms / 28K steps, avg 6.2µs.
    //             CPU launch cost: avg 5.5µs (471ms / 85K launches / 3).
    //             Memory-bound (many global reads via connectivity).
    // ------------------------------------------------------------------
    launch_shell_geometry_kernel(
        // Global node arrays
        g->d_X,  
        g->d_V,  
        g->d_VR, 
        // Connectivity
        g->d_N1, g->d_N2, g->d_N3, g->d_N4,
        // Element state (read/write)
        g->d_OFF,  g->d_SMSTR,
        // Outputs: local frame
        g->d_E1x, g->d_E1y, g->d_E1z,
        g->d_E2x, g->d_E2y, g->d_E2z,
        g->d_E3x, g->d_E3y, g->d_E3z,
        // Outputs: gathered translational velocities
        g->d_VL1x, g->d_VL1y, g->d_VL1z,
        g->d_VL2x, g->d_VL2y, g->d_VL2z,
        g->d_VL3x, g->d_VL3y, g->d_VL3z,
        g->d_VL4x, g->d_VL4y, g->d_VL4z,
        // Outputs: gathered rotational velocities
        g->d_VRL1x, g->d_VRL1y, g->d_VRL1z,
        g->d_VRL2x, g->d_VRL2y, g->d_VRL2z,
        g->d_VRL3x, g->d_VRL3y, g->d_VRL3z,
        g->d_VRL4x, g->d_VRL4y, g->d_VRL4z,
        // Outputs: shape functions & geometry
        g->d_PX1,  g->d_PX2,
        g->d_PY1,  g->d_PY2,
        g->d_AREA, g->d_A_I,
        g->d_VHX,  g->d_VHY,
        g->d_Z2,
        // Outputs: displacement increments
        g->d_UX1, g->d_UY1,
        g->d_UX2, g->d_UY2,
        g->d_UX3, g->d_UY3,
        g->d_UX4, g->d_UY4,
        // Outputs: stiffness (CPXPY3-style, computed here with local coords)
        g->d_STI, g->d_STIR,
        // Per-element material scalars (for stiffness)
        g->d_YM, g->d_THK0,
        g->d_N3, g->d_N4,
        // Scalars
        NE, ISMSTR,
        g->hg.H1, g->hg.H2,
        s);

    // ------------------------------------------------------------------
    //  (c) Kernel 2: Strains + Material
    //      Computes membrane/bending strains, runs Johnson-Cook material
    //      law with radial return, outputs generalized forces/moments.
    //  PROFILING: 71.2% GPU compute, 1.276s / 28K steps, avg 44.9µs.
    //             THE dominant GPU hotspot.
    //             High register pressure from radial-return limits occupancy.
    //             Optimization: reduce registers, increase block size,
    //             explore shared-memory caching of per-IP state.
    // ------------------------------------------------------------------

    // NOTE: d_FORxx..d_MOMxy are NOT zeroed here — Kernel 2 overwrites them
    // with direct assignment (=), not atomicAdd.  The old FOR/MOM values from
    // the previous time step must survive until Kernel 2 reads them for the
    // trapezoidal stress-work energy calculation (degmb/degfx).
    // d_SIGY and d_DPLA are also overwritten by direct assignment.

    // ------------------------------------------------------------------
    //  CCOEF3 equivalent:  THK0 = THK  (snapshot current thickness)
    //  When ITHK>0 and ISMSTR/=3, the reference thickness used for
    //  force/moment normalization, integration weights, and z-coordinates
    //  must be updated to the current physical thickness each cycle.
    //  Without this, forces drift as thickness evolves.
    // ------------------------------------------------------------------
    if (ITHK > 0 && ISMSTR != 3) {
        const size_t se = (size_t)NE * sizeof(Real);
        CUDA_CHECK(cudaMemcpyAsync(g->d_THK0, g->d_THK, se,
                                   cudaMemcpyDeviceToDevice, s));
    }

    launch_shell_strain_material_kernel(
        // Kernel 1 outputs (read-only)
        g->d_E1x, g->d_E1y, g->d_E1z,
        g->d_E2x, g->d_E2y, g->d_E2z,
        g->d_E3x, g->d_E3y, g->d_E3z,
        g->d_VL1x, g->d_VL1y, g->d_VL1z,
        g->d_VL2x, g->d_VL2y, g->d_VL2z,
        g->d_VL3x, g->d_VL3y, g->d_VL3z,
        g->d_VL4x, g->d_VL4y, g->d_VL4z,
        g->d_VRL1x, g->d_VRL1y, g->d_VRL1z,
        g->d_VRL2x, g->d_VRL2y, g->d_VRL2z,
        g->d_VRL3x, g->d_VRL3y, g->d_VRL3z,
        g->d_VRL4x, g->d_VRL4y, g->d_VRL4z,
        g->d_PX1,  g->d_PX2,
        g->d_PY1,  g->d_PY2,
        g->d_AREA, g->d_A_I,
        g->d_UX1,  g->d_UY1,
        g->d_UX2,  g->d_UY2,
        g->d_UX3,  g->d_UY3,
        g->d_UX4,  g->d_UY4,
        // Element state
        g->d_OFF, g->d_THK, g->d_THK0,
        g->d_GSTR, g->d_EINT, g->d_EPSD_elem,
        // Per-integration-point state
        g->d_SIGxx, g->d_SIGyy, g->d_SIGxy,
        g->d_SIGyz, g->d_SIGzx,
        g->d_PLA,   g->d_EPSD_ip,
        g->d_SIGBAKxx, g->d_SIGBAKyy, g->d_SIGBAKxy,
        g->d_DPLA,  g->d_TEMPEL,
        // Outputs: generalized forces/moments
        g->d_FORxx, g->d_FORyy, g->d_FORxy,
        g->d_FORyz, g->d_FORzx,
        g->d_MOMxx, g->d_MOMyy, g->d_MOMxy,
        g->d_SIGY,
        // Scalars
        g->mat, dt, NPT, NE, ISMSTR, ITHK, ncycle_gpu, s);

    // ------------------------------------------------------------------
    //  (d) Kernel 3: Forces + Assembly
    //      Computes hourglass forces, transforms to global, atomicAdd
    //      to nodal force/moment arrays.
    //  PROFILING: 19.0% GPU compute, 341ms / 28K steps, avg 12.0µs.
    //             atomicAdd contention on shared nodes.
    // ------------------------------------------------------------------
    launch_shell_force_assembly_kernel(
        // Connectivity
        g->d_N1, g->d_N2, g->d_N3, g->d_N4,
        // Local frame
        g->d_E1x, g->d_E1y, g->d_E1z,
        g->d_E2x, g->d_E2y, g->d_E2z,
        g->d_E3x, g->d_E3y, g->d_E3z,
        // Shape functions
        g->d_PX1, g->d_PX2,
        g->d_PY1, g->d_PY2,
        g->d_AREA,
        // Hourglass volume ratios (for non-uniform GAMA)
        g->d_VHX, g->d_VHY,
        // Gathered velocities
        g->d_VL1x, g->d_VL1y, g->d_VL1z,
        g->d_VL2x, g->d_VL2y, g->d_VL2z,
        g->d_VL3x, g->d_VL3y, g->d_VL3z,
        g->d_VL4x, g->d_VL4y, g->d_VL4z,
        g->d_VRL1x, g->d_VRL1y, g->d_VRL1z,
        g->d_VRL2x, g->d_VRL2y, g->d_VRL2z,
        g->d_VRL3x, g->d_VRL3y, g->d_VRL3z,
        g->d_VRL4x, g->d_VRL4y, g->d_VRL4z,
        // Element state
        g->d_OFF, g->d_THK0, g->d_THK02,
        // Generalized forces/moments
        g->d_FORxx, g->d_FORyy, g->d_FORxy,
        g->d_FORyz, g->d_FORzx,
        g->d_MOMxx, g->d_MOMyy, g->d_MOMxy,
        // Hourglass state
        g->d_HOUR,
        // Stiffness outputs
        g->d_STI, g->d_STIR,
        // Material
        g->d_SSP, g->d_RHO, g->d_YM, g->d_NU, g->d_A11, g->d_G, g->d_SHF,
        // Internal energy
        g->d_EINT,
        // Global nodal arrays (atomicAdd)
        g->d_Fx, g->d_Fy, g->d_Fz,
        g->d_Mx, g->d_My, g->d_Mz,
        g->d_STIFN, g->d_STIFR,
        // Scalars
        g->hg, dt, NPT, NE, g->compute_sti, ISMSTR, g->IHBE, s);

    // Record event: this SU's kernels are done.
    // The global stream will wait on this before D2H of forces.
    CUDA_CHECK(cudaEventRecord(g->kernels_done, s));
}

// ===========================================================================
//  7. SYNCHRONIZE
//     Block CPU until all GPU work on this stream has completed.
// ===========================================================================
extern "C"
void shell_gpu_synchronize(ShellGPUData* g)
{
    CUDA_CHECK(cudaStreamSynchronize(g->stream));
}

// ===========================================================================
//  7b. PIN / UNPIN HOST MEMORY
//      Page-lock the Fortran raw transfer buffers so that cudaMemcpyAsync
//      becomes truly asynchronous instead of falling back to synchronous
//      staging through an internal bounce buffer.
//
//      Must be called ONCE after the Fortran buffers are allocated and
//      ONCE before they are deallocated.  The pointers must remain valid
//      (not reallocated/moved) between pin and unpin.
//
//      Expected impact: cudaMemcpyAsync avg drops from ~47µs to ~2µs,
//      saving ~3s of CPU API time over 28K steps and enabling true
//      overlap of CPU gather with GPU execution in the 3-pass pipeline.
// ===========================================================================
extern "C"
void shell_gpu_pin_host_memory(const ShellGPUData* g,
                               Real* raw_cpu_to_gpu,
                               Real* raw_gpu_to_cpu)
{
    const size_t s_h2d = (size_t)(9 * g->NUMNOD) * sizeof(Real);
    const size_t s_d2h = (size_t)(8 * g->NUMNOD) * sizeof(Real);

    CUDA_CHECK(cudaHostRegister(raw_cpu_to_gpu, s_h2d, cudaHostRegisterDefault));
    CUDA_CHECK(cudaHostRegister(raw_gpu_to_cpu, s_d2h, cudaHostRegisterDefault));
}

extern "C"
void shell_gpu_unpin_host_memory(Real* raw_cpu_to_gpu,
                                 Real* raw_gpu_to_cpu)
{
    // cudaHostUnregister is safe even if the pointer was never registered
    // (it returns cudaErrorHostMemoryNotRegistered which we ignore).
    cudaHostUnregister(raw_cpu_to_gpu);
    cudaHostUnregister(raw_gpu_to_cpu);
}

// ===========================================================================
//  8. FULL TIME STEP — synchronous convenience wrapper
//     (Prefer shell_gpu_full_step_async + deferred sync for production.)
// ===========================================================================
//
//  PROFILING SUMMARY (nsys, 28440 steps, pipelined 3-pass SU loop):
//
//    Total CUDA API time: 4.89s  (was 14.1s before batching — 2.9x speedup)
//
//    cudaMemcpyAsync:    4.07s  83.3%  86,360 calls  avg 47µs
//    cudaLaunchKernel:   0.47s   9.6%  85,320 calls  avg 5.5µs
//    cudaMemsetAsync:    0.14s   2.8%  28,440 calls  avg 4.9µs
//    cudaStreamSync:     0.015s  0.3%  29,296 calls  avg 0.5µs  ← was 987ms!
//
//    GPU compute: 1.793s  (geometry 176ms + strain 1276ms + assembly 341ms)
//    GPU memops:  57ms    (H2D 17.5ms + D2H 23ms + memset 16.4ms)
//
//  PER-STEP CALL COUNT:  1 H2D + 1 memset + 3 launches + 1 D2D + 2 D2H
//                         + 1 sync = 9 CUDA calls  (was 28, then 8)
//
//  KEY INSIGHT — UNPINNED MEMORY DOMINATES:
//    cudaMemcpyAsync averages 47µs because host memory (Fortran pointer)
//    is NOT page-locked.  The CUDA runtime falls back to internal staging
//    through a pinned bounce buffer, making each "async" call synchronous.
//    Over 86K calls × 47µs = 4.07s of CPU stall in the memcpy driver.
//
//  REMAINING BOTTLENECKS (in priority order):
//
//    1. PINNED HOST MEMORY (highest impact, ~3x reduction in API time).
//       cudaHostRegister(raw_cpu_to_gpu) and cudaHostRegister(raw_gpu_to_cpu)
//       once at init.  Turns 47µs avg memcpy into ~2µs truly-async calls.
//       Enables real overlap of CPU gather(SU+1) with GPU compute(SU).
//       Expected savings: ~3s of CPU API time.
//
//    2. Kernel 2 optimization (71.2% of GPU compute, 1.276s).
//       Radial-return loop has high register pressure → low occupancy.
//       Profile with ncu --set full to identify register spills.
//
//    3. Fortran gather/scatter loops: scalar indirect-indexed O(NUMNOD)
//       CPU work between GPU launches.  Move to GPU or vectorize.
//
//    4. GPU reduction for min-timestep: eliminate the ALDT² D2H + CPU
//       reduction loop.  Download a single scalar instead.
//
// ===========================================================================
extern "C"
void shell_gpu_full_step(ShellGPUData* g, Real dt,
                         const Real* raw_cpu_to_gpu,
                         Real* raw_gpu_to_cpu)
{
    // 1. Upload current node positions/velocities
    //    Legacy path: raw_cpu_to_gpu is [9*NUMNOD] = X|V|VR contiguous
    shell_gpu_upload_nodes(g,
                           raw_cpu_to_gpu,
                           raw_cpu_to_gpu + 3 * g->NUMNOD,
                           raw_cpu_to_gpu + 6 * g->NUMNOD);

    // 2. Run the three kernels
    shell_gpu_run_kernels(g, dt);

    // 3. Download results (single contiguous buffer)
    shell_gpu_download_nodal_forces(g, raw_gpu_to_cpu);
}

// ===========================================================================
//  9. ASYNC VARIANTS — for pipelined SU iteration
//
//     The Fortran time-step loop is split into 3 passes over SU groups:
//
//       Pass 1: gather nodes (CPU) + upload + run_kernels + D2H (all async)
//       Pass 2: start async D2H of ALDT² per SU
//       Pass 3: synchronize each SU stream, scatter forces, reduce ALDT
//
//     PROFILING IMPACT:
//       cudaStreamSynchronize dropped from 987ms (3/step) to 15.5ms (1/step).
//       The one remaining sync per SU in pass 3 finds work already completed
//       in most cases → avg sync 0.5µs (was ~11µs pipeline-drain).
//
//     LIMITATION:
//       The SU pipelining benefit is currently limited because host memory
//       is not pinned — cudaMemcpyAsync blocks internally anyway.
//       With cudaHostRegister, pass 1 would truly overlap CPU/GPU work.
// ===========================================================================

// ---------------------------------------------------------------------------
//  Async full step: H2D + 3 kernels + D2H of forces, NO synchronization.
//  The caller must call shell_gpu_synchronize() later before reading results.
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_full_step_async(ShellGPUData* g, Real dt,
                               const Real* X,
                               const Real* V,
                               const Real* VR,
                               Real* raw_gpu_to_cpu)
{
    // With global node/force arrays:
    //   - H2D and D2H are handled by the global handle, NOT here.
    //   - This SU stream waits on the global upload_done event,
    //     runs the 3 kernels, and records kernels_done.
    //   - The caller (gpu_shell_launch_async) is responsible for
    //     calling shell_gpu_global_upload_nodes beforehand and
    //     shell_gpu_global_download_forces afterwards.

    // 1. Wait for global node upload + force zeroing to complete
    if (g->global) {
        CUDA_CHECK(cudaStreamWaitEvent(g->stream, g->global->upload_done, 0));
    } else {
        // Fallback: per-SU upload (legacy path, should not be reached)
        shell_gpu_upload_nodes(g, X, V, VR);
    }

    // 2. Run the three kernels (uses aliased global node/force pointers)
    shell_gpu_run_kernels(g, dt);

    // Note: shell_gpu_run_kernels already records g->kernels_done event.
    // No D2H here — the global handle does it after all SUs complete.
}

// ---------------------------------------------------------------------------
//  Async ALDT² download: enqueues D2H, NO synchronization.
//  The caller must call shell_gpu_synchronize() before reading h_aldt_sq.
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_download_aldt_sq_async(const ShellGPUData* g,
                                      Real* h_aldt_sq)
{
    const size_t se = (size_t)g->NUMELC * sizeof(Real);
    CUDA_CHECK(cudaMemcpyAsync(h_aldt_sq, g->d_STI, se,
                               cudaMemcpyDeviceToHost, g->stream));
}

// ===========================================================================
//  10. GPU MIN-DT REDUCTION
//      Replaces the host-side pattern of:
//        download_aldt_sq (NUMELC Reals D2H) + Fortran min-loop
//      with:
//        1 kernel launch (reduction) + 1 D2H of 8 bytes (single scalar)
//
//      d_STI[i] = ALDT² (from Kernel 1).
//      d_SSP[i] = sound speed (uploaded once at init).
//      d_OFF[i] = element activity flag.
//
//      Element timestep:  dt_elem = dtfac * sqrt(ALDT²) / SSP
//      Result:            min over all active elements.
//
//      PROFILING IMPACT (expected):
//        Eliminates 28K unpinned D2H calls (~1.2s cudaMemcpyAsync CPU time)
//        and 28K × NUMELC host-side iterations.
//        Replaces with 28K kernel launches (~5µs each) + 28K × 8-byte D2H.
// ===========================================================================

// ---------------------------------------------------------------------------
//  Device helper: atomicMin for Real (CUDA does not provide one natively).
//  Uses CAS loop on the underlying integer representation.
// ---------------------------------------------------------------------------
#ifdef MYREAL8
__device__ __forceinline__
Real atomicMin_Real(Real* addr, Real val)
{
    unsigned long long int* addr_ull = (unsigned long long int*)addr;
    unsigned long long int old = *addr_ull;
    unsigned long long int assumed;
    do {
        assumed = old;
        Real old_val = __longlong_as_double(assumed);
        if (val >= old_val) break;
        old = atomicCAS(addr_ull, assumed, __double_as_longlong(val));
    } while (assumed != old);
    return __longlong_as_double(old);
}
#else
__device__ __forceinline__
Real atomicMin_Real(Real* addr, Real val)
{
    unsigned int* addr_ui = (unsigned int*)addr;
    unsigned int old = *addr_ui;
    unsigned int assumed;
    do {
        assumed = old;
        Real old_val = __int_as_float(assumed);
        if (val >= old_val) break;
        old = atomicCAS(addr_ui, assumed, __float_as_int(val));
    } while (assumed != old);
    return __int_as_float(old);
}
#endif

// ---------------------------------------------------------------------------
//  Reduction kernel: each block reduces a tile, writes partial min
//  via atomicMin_Real to d_result (pre-initialized to +inf by the host).
// ---------------------------------------------------------------------------
__global__
void shell_min_dt_kernel(const Real* __restrict__ d_STI,
                         const Real* __restrict__ d_SSP,
                         const Real* __restrict__ d_OFF,
                         Real dtfac,
                         int    NE,
                         Real* __restrict__ d_result)
{
    __shared__ Real sdata[256];  // fixed size = BLOCK (avoids ptxas bounds warning)

    const int tid = threadIdx.x;
    const int gid = blockIdx.x * blockDim.x + tid;

    // Each thread computes its element's dt, or +inf if inactive / out of range
    Real my_dt = 1.0e30;
    if (gid < NE) {
        Real off = d_OFF[gid];
        if (off > 0.0) {
            Real aldt_sq = d_STI[gid];
            Real ssp     = d_SSP[gid];
            if (aldt_sq > 1.0e-20 && ssp > 1.0e-20) {
                my_dt = dtfac * sqrt(aldt_sq) / ssp;
            }
        }
    }

    sdata[tid] = my_dt;
    __syncthreads();

    // Tree reduction within block
    for (int s = blockDim.x / 2; s > 0; s >>= 1) {
        if (tid < s && sdata[tid + s] < sdata[tid]) {
            sdata[tid] = sdata[tid + s];
        }
        __syncthreads();
    }

    // Block leader: atomic min into global result
    if (tid == 0) {
        atomicMin_Real(d_result, sdata[0]);
    }
}

// ---------------------------------------------------------------------------
//  Host launcher: enqueues init + kernel + D2H onto the SU stream.
//  Caller must call shell_gpu_synchronize() before reading *h_dt_min.
// ---------------------------------------------------------------------------
extern "C"
void shell_gpu_min_dt(ShellGPUData* g,
                      Real        dtfac,
                      Real*       h_dt_min)
{
    const int NE    = g->NUMELC;
    const int BLOCK = 256;
    const int GRID  = (NE + BLOCK - 1) / BLOCK;

    // Initialize device result to +inf using memset (avoids unpinned H2D).
    // 0x7F repeated 8 times → 0x7F7F7F7F7F7F7F7F ≈ 1.7e306 (IEEE 754),
    // safely above any real dt_elem.  Uses cudaMemsetAsync (no host memory).
    CUDA_CHECK(cudaMemsetAsync(g->d_dt_min, 0x7F, sizeof(Real), g->stream));

    shell_min_dt_kernel<<<GRID, BLOCK, 0, g->stream>>>(g->d_STI, g->d_SSP, g->d_OFF, dtfac, NE, g->d_dt_min);

    {
        cudaError_t err = cudaPeekAtLastError();
        if (err != cudaSuccess) {
            fprintf(stderr, "CUDA kernel launch error (min_dt reduction): %s\n",
                    cudaGetErrorString(err));
            exit(EXIT_FAILURE);
        }
    }

    // Download single scalar (8 bytes)
    CUDA_CHECK(cudaMemcpyAsync(h_dt_min, g->d_dt_min, sizeof(Real),
                               cudaMemcpyDeviceToHost, g->stream));
}
