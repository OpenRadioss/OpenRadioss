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
//  C-linkage header for host-side orchestration functions
// ==============================================================================

#ifndef SHELL_GPU_DRIVER_H
#define SHELL_GPU_DRIVER_H

#include "shell_gpu_data.h"
#include "real_type.h"

#ifdef __cplusplus
extern "C" {
#endif

// ---------------------------------------------------------------------------
//  Global (shared) node/force handle — one per SHELLS_ instance
// ---------------------------------------------------------------------------

// Create global handle: allocate device node/force arrays for NUMNOD nodes.
ShellGPUGlobal* shell_gpu_global_create(int NUMNOD);

// Destroy global handle: free device arrays, stream, event.
void shell_gpu_global_destroy(ShellGPUGlobal* gh);

// Upload NODES%X/V/VR (H2D) + zero force arrays + record upload_done event.
void shell_gpu_global_upload_nodes(ShellGPUGlobal* gh,
                                   const Real* X,
                                   const Real* V,
                                   const Real* VR);

// Download force arrays (D2H) after all SU kernels complete.
void shell_gpu_global_download_forces(ShellGPUGlobal* gh,
                                      Real* raw_gpu_to_cpu);

// Block CPU until global stream completes.
void shell_gpu_global_synchronize(ShellGPUGlobal* gh);

// Make SU stream wait for global upload to finish.
void shell_gpu_global_wait_upload(ShellGPUGlobal* gh, ShellGPUData* g);

// Make global stream wait for SU kernels to finish.
void shell_gpu_global_wait_su(ShellGPUGlobal* gh, ShellGPUData* g);

// Pin Fortran NODES%X/V/VR and the D2H force buffer for async memcpy.
void shell_gpu_global_pin_host(const Real* X, const Real* V,
                               const Real* VR, Real* raw_gpu_to_cpu,
                               int NUMNOD);

// Set back-pointer from per-SU handle to global handle (aliases pointers).
void shell_gpu_set_global(ShellGPUData* g, ShellGPUGlobal* gh);

// ---------------------------------------------------------------------------
//  Per-SU Lifecycle
// ---------------------------------------------------------------------------

// Allocate all device arrays.  Call once during initialization.
void shell_gpu_allocate(ShellGPUData* g,
                        int NUMELC, int NUMNOD, int NPT,
                        int ISMSTR, int ITHK);

// Free all device arrays and destroy the CUDA stream.
void shell_gpu_deallocate(ShellGPUData* g);

// ---------------------------------------------------------------------------
//  One-time uploads (called once after allocation)
// ---------------------------------------------------------------------------

// Upload connectivity, initial thickness, activity flags, material scalars.
void shell_gpu_upload_constant(ShellGPUData*  g,
                               const int*     h_N1,   const int*     h_N2,
                               const int*     h_N3,   const int*     h_N4,
                               const Real*  h_THK0,
                               const Real*  h_OFF,
                               const Real*  h_SSP,  const Real*  h_RHO,
                               const Real*  h_YM,   const Real*  h_NU,
                               const Real*  h_A11,  const Real*  h_G,
                               const Real*  h_SHF);

// Upload per-integration-point initial state.
void shell_gpu_upload_ip_state(ShellGPUData*  g,
                               const Real*  h_SIGxx,    const Real*  h_SIGyy,
                               const Real*  h_SIGxy,    const Real*  h_SIGyz,
                               const Real*  h_SIGzx,
                               const Real*  h_PLA,      const Real*  h_EPSD_ip,
                               const Real*  h_SIGBAKxx, const Real*  h_SIGBAKyy,
                               const Real*  h_SIGBAKxy,
                               const Real*  h_TEMPEL);

// ---------------------------------------------------------------------------
//  Per-time-step transfers
// ---------------------------------------------------------------------------

// Upload node positions + velocities (H2D, before kernel launch).
// raw_cpu_to_gpu is a single contiguous buffer of 9*NUMNOD Reals:
//   [Xx|Xy|Xz|Vx|Vy|Vz|VRx|VRy|VRz], each sub-array of size NUMNOD.
void shell_gpu_upload_nodes(ShellGPUData*  g,
                            const Real*  raw_cpu_to_gpu);

// Download nodal forces, moments, stiffness (D2H, after kernel launch).
// raw_gpu_to_cpu is a single contiguous buffer of 8*NUMNOD Reals:
//   [Fx|Fy|Fz|Mx|My|Mz|STIFN|STIFR], each sub-array of size NUMNOD.
void shell_gpu_download_nodal_forces(const ShellGPUData* g,
                                     Real* raw_gpu_to_cpu);

// Download element internal energy (D2H).
void shell_gpu_download_energy(const ShellGPUData* g, Real* h_EINT);

// Download full element + IP state (D2H, for output / restart).
void shell_gpu_download_state(const ShellGPUData* g,
                              Real* h_OFF,   Real* h_THK,
                              Real* h_GSTR,  Real* h_EPSD_elem,
                              Real* h_SIGxx, Real* h_SIGyy,
                              Real* h_SIGxy, Real* h_SIGyz,
                              Real* h_SIGzx,
                              Real* h_PLA,   Real* h_EPSD_ip,
                              Real* h_SIGBAKxx, Real* h_SIGBAKyy,
                              Real* h_SIGBAKxy,
                              Real* h_TEMPEL);

// ---------------------------------------------------------------------------
//  Kernel execution
// ---------------------------------------------------------------------------

// Set the compute_sti mode (0=skip, 1=CPXPY3+CHSTI3, 2=CDT3).
void shell_gpu_set_compute_sti(ShellGPUData* g, int flag);

// Set the IHBE (hourglass formulation) flag.
void shell_gpu_set_ihbe(ShellGPUData* g, int ihbe);

// Zero nodal force/moment/stiffness arrays (must precede each kernel run).
void shell_gpu_zero_nodal_arrays(ShellGPUData* g);

// Launch all three kernels sequentially on the device stream.
// Does NOT upload nodes or download forces — call wrappers for that.
void shell_gpu_run_kernels(ShellGPUData* g, Real dt);

// Wait for all GPU work to complete.
void shell_gpu_synchronize(ShellGPUData* g);

// Pin (page-lock) host transfer buffers for truly async cudaMemcpyAsync.
// Call once after Fortran allocates raw_cpu_to_gpu / raw_gpu_to_cpu.
void shell_gpu_pin_host_memory(const ShellGPUData* g,
                               Real* raw_cpu_to_gpu,
                               Real* raw_gpu_to_cpu);

// Unpin host buffers. Call before deallocating the Fortran arrays.
void shell_gpu_unpin_host_memory(Real* raw_cpu_to_gpu,
                                 Real* raw_gpu_to_cpu);

// ---------------------------------------------------------------------------
//  Convenience: one-call full time step
//  (upload nodes → zero arrays → 3 kernels → download forces)
// ---------------------------------------------------------------------------
void shell_gpu_full_step(ShellGPUData* g, Real dt,
                         const Real* raw_cpu_to_gpu,
                         Real* raw_gpu_to_cpu);

// ---------------------------------------------------------------------------
//  Async variants — for pipelined SU iteration (no synchronization)
// ---------------------------------------------------------------------------

// Async full step: H2D + 3 kernels + D2H enqueued, caller must sync later.
void shell_gpu_full_step_async(ShellGPUData* g, Real dt,
                               const Real* X,
                               const Real* V,
                               const Real* VR,
                               Real* raw_gpu_to_cpu);

// Async ALDT² download: D2H enqueued, caller must sync before reading.
void shell_gpu_download_aldt_sq_async(const ShellGPUData* g,
                                      Real* h_aldt_sq);

// Download per-element ALDT² (synchronous — kept for backward compat).
void shell_gpu_download_aldt_sq(const ShellGPUData* g, Real* h_aldt_sq);

// Compute min element timestep on GPU via reduction kernel.
// Result = min over active elements of: dtfac * sqrt(d_STI[i]) / d_SSP[i].
// Downloads a single scalar (8 bytes) instead of NUMELC Reals.
// The result is available after shell_gpu_synchronize().
void shell_gpu_min_dt(ShellGPUData* g,
                      Real dtfac,
                      Real* h_dt_min);

#ifdef __cplusplus
}
#endif

#endif // SHELL_GPU_DRIVER_H
