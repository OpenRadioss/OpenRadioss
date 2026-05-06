!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      module shell_gpu_mod
! ======================================================================================================================
!! \brief  Fortran interfaces for the CUDA shell element GPU driver
!! \details
!!   This module provides ISO_C_BINDING interfaces to the C functions
!!   defined in shell_gpu_driver.cu.  The GPU handle (ShellGPUData) is
!!   treated as an opaque C pointer (type(c_ptr)).
!!
!!   LIFECYCLE — full calling sequence from Fortran:
!!
!!   1. INITIALISATION (once)
!!      gpu_handle = shell_gpu_data_create()          ! allocate C struct on host heap
!!      call shell_gpu_allocate(gpu_handle, ...)       ! allocate all device arrays
!!      call shell_gpu_set_mat_params(gpu_handle, ...) ! set Johnson-Cook material params
!!      call shell_gpu_set_hg_params(gpu_handle, ...)  ! set hourglass params
!!      call shell_gpu_upload_constant(gpu_handle, ..) ! H2D: connectivity, thickness, per-elem scalars
!!      call shell_gpu_upload_ip_state(gpu_handle, ..) ! H2D: initial stress / strain state
!!
!!   2. TIME STEPPING (each cycle)
!!      call shell_gpu_full_step(gpu_handle, dt, &
!!                               Xx, Xy, Xz, Vx, Vy, Vz, VRx, VRy, VRz, &
!!                               Fx, Fy, Fz, Mx, My, Mz, STIFN, STIFR)
!!
!!      Or equivalently, the three sub-steps:
!!        call shell_gpu_upload_nodes(gpu_handle, Xx, ...)
!!        call shell_gpu_run_kernels(gpu_handle, dt)
!!        call shell_gpu_download_nodal_forces(gpu_handle, Fx, ...)
!!
!!   3. OUTPUT / RESTART (as needed)
!!      call shell_gpu_download_energy(gpu_handle, EINT)
!!      call shell_gpu_download_state(gpu_handle,  ...)
!!
!!   4. CLEANUP (once)
!!      call shell_gpu_deallocate(gpu_handle)          ! free device arrays
!!      call shell_gpu_data_destroy(gpu_handle)        ! free the C struct itself
!!
!!   IMPORTANT NOTES:
!!   - gpu_handle is NOT populated by shell_gpu_full_step. It must be
!!     fully initialised (steps 1a–1f) before the first time-step call.
!!   - shell_gpu_full_step only uploads node positions / velocities,
!!     runs the 3 kernels, and downloads the resulting nodal forces.
!!   - All host arrays passed to these routines must be contiguous.
!!   - The GPU code always operates in double precision regardless of
!!     the build precision setting (WP).
! ======================================================================================================================

        use, intrinsic :: iso_c_binding, only : c_ptr , c_null_ptr ,  c_int
        use precision_mod, only : WP
        implicit none


        ! GPU_SHELL(COLOUR)%LAW2(GROUP) is a convenient Fortran-side mirror of the GPU data, used for
        type GPU_SHELL_LAW2
          ! sizes
          integer(c_int) :: numelc    !< Number of 4-node shell elements
          integer(c_int) :: numnod    !< Number of nodes
          integer(c_int) :: npt       !< Through-thickness integration points
          integer(c_int) :: ismstr    !< Small-strain flag (1, 2, or 11)
          integer(c_int) :: ithk      !< Thickness update flag (>0 enables)
          integer(c_int) :: ihbe      !< Hourglass formulation flag (>=1: non-uniform GAMA)
          ! mapping arrays
          integer(c_int), dimension(:), allocatable :: glob_2_gpu !< Maps global node ID to GPU node index
          integer(c_int), dimension(:), allocatable :: gpu_2_glob !< Maps GPU node index to global node ID
          type(c_ptr) :: handle = c_null_ptr

          integer(c_int), dimension(:), allocatable  :: n1     !< Connectivity node 1 [NUMELC]
          integer(c_int), dimension(:), allocatable  :: n2     !< Connectivity node 2 [NUMELC]
          integer(c_int), dimension(:), allocatable  :: n3     !< Connectivity node 3 [NUMELC]
          integer(c_int), dimension(:), allocatable  :: n4     !< Connectivity node 4 [NUMELC]
          integer(c_int), dimension(:), allocatable  :: ngl    !< Global element ID [NUMELC]
          integer(c_int), dimension(:), allocatable  :: part_id !< Part ID for each element [NUMELC]

          ! Host to Device: node data [NUMNOD]
          real(WP), dimension(:), pointer :: raw_cpu_to_gpu => null()
          ! These are pointers into a single contiguous buffer for optimized GPU transfers
          real(WP), dimension(:), pointer :: xx  => null() !< Node X coordinates
          real(WP), dimension(:), pointer :: xy  => null() !< Node Y coordinates
          real(WP), dimension(:), pointer :: xz  => null() !< Node Z coordinates
          real(WP), dimension(:), pointer :: vx  => null() !< Translational velocity X
          real(WP), dimension(:), pointer :: vy  => null() !< Translational velocity Y
          real(WP), dimension(:), pointer :: vz  => null() !< Translational velocity Z
          real(WP), dimension(:), pointer :: vrx => null() !< Rotational velocity X
          real(WP), dimension(:), pointer :: vry => null() !< Rotational velocity Y
          real(WP), dimension(:), pointer :: vrz => null() !< Rotational velocity Z
          ! Device to Host: nodal results [NUMNOD]
          real(WP), dimension(:), pointer :: raw_gpu_to_cpu => null()
          real(WP), dimension(:), pointer :: fx    => null() !< Nodal force X
          real(WP), dimension(:), pointer :: fy    => null() !< Nodal force Y
          real(WP), dimension(:), pointer :: fz    => null() !< Nodal force Z
          real(WP), dimension(:), pointer :: mx    => null() !< Nodal moment X
          real(WP), dimension(:), pointer :: my    => null() !< Nodal moment Y
          real(WP), dimension(:), pointer :: mz    => null() !< Nodal moment Z
          real(WP), dimension(:), pointer :: stifn => null() !< Nodal translational stiffness
          real(WP), dimension(:), pointer :: stifr => null() !< Nodal rotational stiffness
          ! Temporary per-element ALDT² buffer for async download (legacy)
          real(WP), dimension(:), allocatable :: aldt_sq
          ! Result scalar for GPU min-dt reduction (written by shell_gpu_min_dt)
          real(WP) :: dt_min_result = 0.0d0
          ! IP state arrays [NPT*NUMELC]
          real(WP), dimension(:), allocatable :: sigxx    !< Stress xx
          real(WP), dimension(:), allocatable :: sigyy    !< Stress yy
          real(WP), dimension(:), allocatable :: sigxy    !< Stress xy
          real(WP), dimension(:), allocatable :: sigyz    !< Stress yz
          real(WP), dimension(:), allocatable :: sigzx    !< Stress zx
          real(WP), dimension(:), allocatable :: pla      !< Plastic strain
          real(WP), dimension(:), allocatable :: epsd_ip  !< Strain rate
          real(WP), dimension(:), allocatable :: sigbakxx !< Back-stress xx
          real(WP), dimension(:), allocatable :: sigbakyy !< Back-stress yy
          real(WP), dimension(:), allocatable :: sigbakxy !< Back-stress xy
          real(WP), dimension(:), allocatable :: tempel    !< Temperature
          real(WP), dimension(:), allocatable :: eint      !< Internal energy

          ! Material parameters (stored by value in the handle)
          real(WP) :: e_young   !< Young's modulus
          real(WP) :: nu        !< Poisson's ratio
          real(WP) :: g_shear   !< Shear modulus
          real(WP) :: a11       !< Plane-stress stiffness E/(1-nu^2)
          real(WP) :: a12       !< Coupling  nu * A11
          real(WP) :: ca        !< J-C param A (initial yield)
          real(WP) :: cb        !< J-C param B (hardening modulus)
          real(WP) :: cn        !< J-C param n (hardening exponent)
          real(WP) :: cc        !< J-C param C (strain-rate sensitivity)
          real(WP) :: epdr      !< Reference strain rate
          real(WP) :: epmx      !< Max plastic strain for rupture
          real(WP) :: ymax      !< Yield stress cap
          real(WP) :: m_exp     !< Thermal exponent m
          real(WP) :: fisokin   !< Isotropic/kinematic ratio
          real(WP) :: rhocp     !< rho * Cp (adiabatic heating)
          real(WP) :: tref      !< Reference temperature
          real(WP) :: tmelt     !< Melting temperature
          real(WP) :: asrate    !< Strain-rate filter coefficient
          real(WP) :: rho       !< Density
          real(WP) :: ssp       !< Sound speed
          real(WP) :: shf_coef  !< Shear correction factor
          integer(c_int) :: ipla      !< Plasticity algorithm (0, 1, or 2)
          integer(c_int) :: vp        !< Strain-rate type
          integer(c_int) :: iform     !< 0=Johnson-Cook, 1=Zerilli-Armstrong
          integer(c_int) :: icc       !< Cowper-Symonds flag
          real(WP) :: z3        !< Zerilli-Armstrong param
          real(WP) :: z4        !< Zerilli-Armstrong param

          real(WP) :: h1    !< Membrane hourglass coeff
          real(WP) :: h2    !< Bending hourglass coeff
          real(WP) :: h3    !< Rotational hourglass coeff
          real(WP) :: srh1  !< sqrt scale for h1
          real(WP) :: srh2  !< sqrt scale for h2
          real(WP) :: srh3  !< sqrt scale for h3
          real(WP) :: dtfac !< Timestep scale factor DTFAC1(3)

          real(WP), dimension(:), allocatable   :: el_thk0   !< Initial thickness [NUMELC]
          real(WP), dimension(:), allocatable   :: el_off   !< Element activity [NUMELC]
          real(WP), dimension(:), allocatable   :: el_ssp    !< Sound speed [NUMELC]
          real(WP), dimension(:), allocatable   :: el_rho    !< Density [NUMELC]
          real(WP), dimension(:), allocatable   :: el_ym    !< Young's modulus [NUMELC]
          real(WP), dimension(:), allocatable   :: el_nu     !< Poisson's ratio [NUMELC]
          real(WP), dimension(:), allocatable   :: el_a11    !< Plane-stress stiffness [NUMELC]
          real(WP), dimension(:), allocatable   :: el_g_shear !< Shear modulus [NUMELC]
          real(WP), dimension(:), allocatable   :: el_shf    !< Shear correction factor [NUMELC]



        end type GPU_SHELL_LAW2

        ! SHELLS(COLOUR)%LAW2(SUPER_GROUP) is a convenient Fortran-side mirror of the GPU data, used for
        type SHELLS_
          !type(GPU_SHELL_LAW1), dimension(:), allocatable :: law1 !< Array of LAW structs, one per color
          type(GPU_SHELL_LAW2), dimension(:), allocatable :: law2 !
          !type(GPU_SHELL_LAW3), dimension(:), allocatable :: law3 !< Future expansion for additional material models
          !...
          !type(GPU_SHELL_LAW132), dimension(:), allocatable :: law132 !< Future expansion for additional material models
          ! GPU element dt should reduce DT2T only when the CPU CDT3 would
          ! do the same: NODADT=0 AND IDTMIN3/=0 (compute_sti==2).
          ! With NODADT/=0 the timestep is controlled by DTNODA from STIFN,
          ! not by element-level dt.
          logical :: reduce_elem_dt = .false.
          ! Global (shared) node/force handle — one per SHELLS_ instance
          type(c_ptr) :: global_handle = c_null_ptr
          ! Host-side D2H force buffer [8*NUMNOD_GLOBAL] — pinned memory
          real(WP), dimension(:), allocatable :: raw_gpu_to_cpu_global
          ! Global node count for the shared arrays
          integer(c_int) :: numnod_global = 0
        end type SHELLS_



        private
        ! Derived types
        public :: GPU_SHELL_LAW2
        public :: SHELLS_
        ! Opaque handle
        public :: c_ptr

        ! Lifecycle
        public :: shell_gpu_data_create
        public :: shell_gpu_data_destroy
        public :: shell_gpu_allocate
        public :: shell_gpu_deallocate

        ! Parameter setters
        public :: shell_gpu_set_mat_params
        public :: shell_gpu_set_hg_params
        public :: shell_gpu_set_compute_sti
        public :: shell_gpu_set_ihbe

        ! Data transfers
        public :: shell_gpu_upload_constant
        public :: shell_gpu_upload_ip_state
        public :: shell_gpu_upload_nodes
        public :: shell_gpu_download_nodal_forces
        public :: shell_gpu_download_energy
        public :: shell_gpu_download_aldt_sq
        public :: shell_gpu_download_state

        ! Kernel execution
        public :: shell_gpu_run_kernels
        public :: shell_gpu_zero_nodal_arrays
        public :: shell_gpu_synchronize
        public :: shell_gpu_pin_host_memory
        public :: shell_gpu_unpin_host_memory

        ! Convenience wrapper
        public :: shell_gpu_full_step

        ! Async variants for pipelined SU iteration
        public :: shell_gpu_full_step_async
        public :: shell_gpu_download_aldt_sq_async

        ! GPU min-dt reduction (replaces ALDT download + host loop)
        public :: shell_gpu_min_dt

        ! Global (shared) node/force handle
        public :: shell_gpu_global_create
        public :: shell_gpu_global_destroy
        public :: shell_gpu_global_upload_nodes
        public :: shell_gpu_global_download_forces
        public :: shell_gpu_global_synchronize
        public :: shell_gpu_global_wait_upload
        public :: shell_gpu_global_wait_su
        public :: shell_gpu_global_pin_host
        public :: shell_gpu_set_global

#ifdef WITH_CUDA
        ! ======================================================================================================================
        !                                                   INTERFACES
        ! ======================================================================================================================

        interface

          ! ----------------------------------------------------------------------------------------------------------------------
          !  0. Handle creation / destruction
          ! ----------------------------------------------------------------------------------------------------------------------

          !! \brief Allocate a ShellGPUData structure on the host heap.
          !! \return Opaque C pointer to the newly allocated handle (all fields zeroed).
          function shell_gpu_data_create() result(g) bind(c, name='shell_gpu_data_create')
            import :: c_ptr
            type(c_ptr) :: g
          end function shell_gpu_data_create

          !! \brief Free the host-side ShellGPUData structure.
          !!        Must be called AFTER shell_gpu_deallocate.
          subroutine shell_gpu_data_destroy(g) bind(c, name='shell_gpu_data_destroy')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: g
          end subroutine shell_gpu_data_destroy

          ! ----------------------------------------------------------------------------------------------------------------------
          !  1. Device memory allocation / deallocation
          ! ----------------------------------------------------------------------------------------------------------------------

          !! \brief Allocate all device arrays and create the CUDA stream.
          !! \details This sets mesh dimensions in the handle and allocates
          !!          device memory for nodes, elements, and integration points.
          subroutine shell_gpu_allocate(g, numelc, numnod, npt, ismstr, ithk) &
            bind(c, name='shell_gpu_allocate')
            import :: c_ptr , c_int
            type(c_ptr),       value, intent(in) :: g         !< GPU handle
            integer(c_int),    value, intent(in) :: numelc    !< Number of 4-node shell elements
            integer(c_int),    value, intent(in) :: numnod    !< Number of nodes
            integer(c_int),    value, intent(in) :: npt       !< Through-thickness integration points
            integer(c_int),    value, intent(in) :: ismstr    !< Small-strain flag (1, 2, or 11)
            integer(c_int),    value, intent(in) :: ithk      !< Thickness update flag (>0 enables)
          end subroutine shell_gpu_allocate

          !! \brief Free all device arrays and destroy the CUDA stream.
          subroutine shell_gpu_deallocate(g) bind(c, name='shell_gpu_deallocate')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: g
          end subroutine shell_gpu_deallocate

          ! ----------------------------------------------------------------------------------------------------------------------
          !  2. Material and hourglass parameter setters
          ! ----------------------------------------------------------------------------------------------------------------------

          !! \brief Set Johnson-Cook material parameters on the GPU handle.
          !! \details These are stored by value in the handle and passed to
          !!          kernel 2 (strain + material law) at each time step.
          subroutine shell_gpu_set_mat_params(g, &
            e_young, nu, g_shear, a11, a12, &
            ca, cb, cn, cc, epdr, epmx, ymax, m_exp, &
            fisokin, rhocp, tref, tmelt, asrate, &
            rho, ssp, shf_coef, &
            ipla, vp, iform, icc, &
            z3, z4) &
            bind(c, name='shell_gpu_set_mat_params')
            import :: c_ptr , c_int , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    value, intent(in) :: e_young   !< Young's modulus
            real(WP),    value, intent(in) :: nu        !< Poisson's ratio
            real(WP),    value, intent(in) :: g_shear   !< Shear modulus
            real(WP),    value, intent(in) :: a11       !< Plane-stress stiffness E/(1-nu^2)
            real(WP),    value, intent(in) :: a12       !< Coupling  nu * A11
            real(WP),    value, intent(in) :: ca        !< J-C param A (initial yield)
            real(WP),    value, intent(in) :: cb        !< J-C param B (hardening modulus)
            real(WP),    value, intent(in) :: cn        !< J-C param n (hardening exponent)
            real(WP),    value, intent(in) :: cc        !< J-C param C (strain-rate sensitivity)
            real(WP),    value, intent(in) :: epdr      !< Reference strain rate
            real(WP),    value, intent(in) :: epmx      !< Max plastic strain for rupture
            real(WP),    value, intent(in) :: ymax      !< Yield stress cap
            real(WP),    value, intent(in) :: m_exp     !< Thermal exponent m
            real(WP),    value, intent(in) :: fisokin   !< Isotropic/kinematic ratio
            real(WP),    value, intent(in) :: rhocp     !< rho * Cp (adiabatic heating)
            real(WP),    value, intent(in) :: tref      !< Reference temperature
            real(WP),    value, intent(in) :: tmelt     !< Melting temperature
            real(WP),    value, intent(in) :: asrate    !< Strain-rate filter coefficient
            real(WP),    value, intent(in) :: rho       !< Density
            real(WP),    value, intent(in) :: ssp       !< Sound speed
            real(WP),    value, intent(in) :: shf_coef  !< Shear correction factor
            integer(c_int),    value, intent(in) :: ipla      !< Plasticity algorithm (0, 1, or 2)
            integer(c_int),    value, intent(in) :: vp        !< Strain-rate type
            integer(c_int),    value, intent(in) :: iform     !< 0=Johnson-Cook, 1=Zerilli-Armstrong
            integer(c_int),    value, intent(in) :: icc       !< Cowper-Symonds flag
            real(WP),    value, intent(in) :: z3        !< Zerilli-Armstrong param
            real(WP),    value, intent(in) :: z4        !< Zerilli-Armstrong param
          end subroutine shell_gpu_set_mat_params

          !! \brief Set hourglass parameters on the GPU handle.
          !! \details These are stored by value and passed to kernel 3
          !!          (force + assembly) at each time step.
          subroutine shell_gpu_set_hg_params(g, h1, h2, h3, srh1, srh2, srh3, &
            hvisc_in, helas_in, hvlin_in) &
            bind(c, name='shell_gpu_set_hg_params')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    value, intent(in) :: h1    !< Membrane hourglass coeff
            real(WP),    value, intent(in) :: h2    !< Bending hourglass coeff
            real(WP),    value, intent(in) :: h3    !< Rotational hourglass coeff
            real(WP),    value, intent(in) :: srh1  !< sqrt scale for h1
            real(WP),    value, intent(in) :: srh2  !< sqrt scale for h2
            real(WP),    value, intent(in) :: srh3  !< sqrt scale for h3
            real(WP),    value, intent(in) :: hvisc_in !< viscous hg scaling (common SCR06R)
            real(WP),    value, intent(in) :: helas_in !< elastic hg scaling (common SCR06R)
            real(WP),    value, intent(in) :: hvlin_in !< linear  hg scaling (common SCR06R)
          end subroutine shell_gpu_set_hg_params

          !! \brief Set compute_sti flag: 1 = compute element stiffness for
          !!        STIFN (NODADT/=0), 0 = skip (matches CPU NODADT=0 behavior)
          subroutine shell_gpu_set_compute_sti(g, flag) &
            bind(c, name='shell_gpu_set_compute_sti')
            import :: c_ptr , c_int
            type(c_ptr),       value, intent(in) :: g
            integer(c_int),    value, intent(in) :: flag  !< 0=skip, 1=compute
          end subroutine shell_gpu_set_compute_sti

          !! \brief Set IHBE flag: hourglass formulation (>=1 needs non-uniform GAMA)
          subroutine shell_gpu_set_ihbe(g, ihbe) &
            bind(c, name='shell_gpu_set_ihbe')
            import :: c_ptr , c_int
            type(c_ptr),       value, intent(in) :: g
            integer(c_int),    value, intent(in) :: ihbe
          end subroutine shell_gpu_set_ihbe

          ! ----------------------------------------------------------------------------------------------------------------------
          !  3. Host to Device transfers
          ! ----------------------------------------------------------------------------------------------------------------------

          !! \brief Upload constant (one-time) data: connectivity, thickness,
          !!        element activity, per-element material scalars.
          subroutine shell_gpu_upload_constant(g, &
            h_n1, h_n2, h_n3, h_n4, &
            h_thk0, h_off, &
            h_ssp, h_rho, h_ym, h_nu, h_a11, h_g_shear, h_shf) &
            bind(c, name='shell_gpu_upload_constant')
            import :: c_ptr , c_int , WP
            type(c_ptr),       value, intent(in) :: g
            integer(c_int),    intent(in)        :: h_n1(*)     !< Connectivity node 1 [NUMELC]
            integer(c_int),    intent(in)        :: h_n2(*)     !< Connectivity node 2 [NUMELC]
            integer(c_int),    intent(in)        :: h_n3(*)     !< Connectivity node 3 [NUMELC]
            integer(c_int),    intent(in)        :: h_n4(*)     !< Connectivity node 4 [NUMELC]
            real(WP),    intent(in)        :: h_thk0(*)   !< Initial thickness [NUMELC]
            real(WP),    intent(in)        :: h_off(*)    !< Element activity [NUMELC]
            real(WP),    intent(in)        :: h_ssp(*)    !< Sound speed [NUMELC]
            real(WP),    intent(in)        :: h_rho(*)    !< Density [NUMELC]
            real(WP),    intent(in)        :: h_ym(*)     !< Young's modulus [NUMELC]
            real(WP),    intent(in)        :: h_nu(*)     !< Poisson's ratio [NUMELC]
            real(WP),    intent(in)        :: h_a11(*)    !< Plane-stress stiffness [NUMELC]
            real(WP),    intent(in)        :: h_g_shear(*) !< Shear modulus [NUMELC]
            real(WP),    intent(in)        :: h_shf(*)    !< Shear correction factor [NUMELC]
          end subroutine shell_gpu_upload_constant

          !! \brief Upload per-integration-point initial state (called once).
          subroutine shell_gpu_upload_ip_state(g, &
            h_sigxx, h_sigyy, h_sigxy, h_sigyz, h_sigzx, &
            h_pla, h_epsd_ip, &
            h_sigbakxx, h_sigbakyy, h_sigbakxy, &
            h_tempel) &
            bind(c, name='shell_gpu_upload_ip_state')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    intent(in)        :: h_sigxx(*)    !< Stress xx [NPT*NUMELC]
            real(WP),    intent(in)        :: h_sigyy(*)    !< Stress yy
            real(WP),    intent(in)        :: h_sigxy(*)    !< Stress xy
            real(WP),    intent(in)        :: h_sigyz(*)    !< Stress yz
            real(WP),    intent(in)        :: h_sigzx(*)    !< Stress zx
            real(WP),    intent(in)        :: h_pla(*)      !< Plastic strain
            real(WP),    intent(in)        :: h_epsd_ip(*)  !< Strain rate
            real(WP),    intent(in)        :: h_sigbakxx(*) !< Back-stress xx
            real(WP),    intent(in)        :: h_sigbakyy(*) !< Back-stress yy
            real(WP),    intent(in)        :: h_sigbakxy(*) !< Back-stress xy
            real(WP),    intent(in)        :: h_tempel(*)   !< Temperature
          end subroutine shell_gpu_upload_ip_state

          !! \brief Upload node positions and velocities (called each time step).
          subroutine shell_gpu_upload_nodes(g, raw_cpu_to_gpu) &
            bind(c, name='shell_gpu_upload_nodes')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    intent(in)        :: raw_cpu_to_gpu(*)  !< Node X coords [NUMNOD]
          end subroutine shell_gpu_upload_nodes

          ! ----------------------------------------------------------------------------------------------------------------------
          !  4. Device to Host transfers
          ! ----------------------------------------------------------------------------------------------------------------------

          !! \brief Download nodal forces, moments and stiffness (each time step).
          subroutine shell_gpu_download_nodal_forces(g, raw_gpu_to_cpu) &
            bind(c, name='shell_gpu_download_nodal_forces')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    intent(inout)     :: raw_gpu_to_cpu(*)    !< Nodal force X [NUMNOD]
          end subroutine shell_gpu_download_nodal_forces

          !! \brief Download element internal energy (for output / energy balance).
          subroutine shell_gpu_download_energy(g, h_eint) &
            bind(c, name='shell_gpu_download_energy')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    intent(inout)     :: h_eint(*) !< Internal energy [NUMELC*2]
          end subroutine shell_gpu_download_energy

          !! \brief Download per-element ALDT² (characteristic length squared).
          !! \details Kernel 1 stores ALDT² in d_STI[]; Kernel 3 only reads it.
          !!          Used by host-side element timestep so it uses frozen-reference
          !!          geometry (ISMSTR=1/2) instead of recomputing from current coords.
          subroutine shell_gpu_download_aldt_sq(g, h_aldt_sq) &
            bind(c, name='shell_gpu_download_aldt_sq')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    intent(inout)     :: h_aldt_sq(*) !< ALDT² per element [NUMELC]
          end subroutine shell_gpu_download_aldt_sq

          !! \brief Download full element + integration-point state (output/restart).
          subroutine shell_gpu_download_state(g, &
            h_off, h_thk, h_gstr, h_epsd_elem, &
            h_sigxx, h_sigyy, h_sigxy, h_sigyz, h_sigzx, &
            h_pla, h_epsd_ip, &
            h_sigbakxx, h_sigbakyy, h_sigbakxy, &
            h_tempel) &
            bind(c, name='shell_gpu_download_state')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    intent(inout)     :: h_off(*)       !< Element activity [NUMELC]
            real(WP),    intent(inout)     :: h_thk(*)       !< Current thickness [NUMELC]
            real(WP),    intent(inout)     :: h_gstr(*)      !< Global strains [NUMELC*8]
            real(WP),    intent(inout)     :: h_epsd_elem(*) !< Element strain rate [NUMELC]
            real(WP),    intent(inout)     :: h_sigxx(*)     !< Stress xx [NPT*NUMELC]
            real(WP),    intent(inout)     :: h_sigyy(*)
            real(WP),    intent(inout)     :: h_sigxy(*)
            real(WP),    intent(inout)     :: h_sigyz(*)
            real(WP),    intent(inout)     :: h_sigzx(*)
            real(WP),    intent(inout)     :: h_pla(*)
            real(WP),    intent(inout)     :: h_epsd_ip(*)
            real(WP),    intent(inout)     :: h_sigbakxx(*)
            real(WP),    intent(inout)     :: h_sigbakyy(*)
            real(WP),    intent(inout)     :: h_sigbakxy(*)
            real(WP),    intent(inout)     :: h_tempel(*)
          end subroutine shell_gpu_download_state

          ! ----------------------------------------------------------------------------------------------------------------------
          !  5. Kernel execution
          ! ----------------------------------------------------------------------------------------------------------------------

          !! \brief Zero the nodal force/moment/stiffness arrays on device.
          subroutine shell_gpu_zero_nodal_arrays(g) &
            bind(c, name='shell_gpu_zero_nodal_arrays')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: g
          end subroutine shell_gpu_zero_nodal_arrays

          !! \brief Run all three GPU kernels for one time step.
          !! \details Sequence: zero nodal arrays, geometry, strain+material,
          !!          force+assembly.  The handle g must be fully initialised.
          subroutine shell_gpu_run_kernels(g, dt) &
            bind(c, name='shell_gpu_run_kernels')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    value, intent(in) :: dt   !< Current time step
          end subroutine shell_gpu_run_kernels

          !! \brief Block the CPU until all GPU work on this handle's stream completes.
          subroutine shell_gpu_synchronize(g) &
            bind(c, name='shell_gpu_synchronize')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: g
          end subroutine shell_gpu_synchronize

          ! ----------------------------------------------------------------------------------------------------------------------
          !  6. Convenience wrapper — full time step in one call
          ! ----------------------------------------------------------------------------------------------------------------------

          !! \brief Perform a complete GPU shell time step:
          !!        upload nodes → run 3 kernels → download forces.
          !! \details The GPU handle (g) must have been fully initialised before the
          !!          first call:
          !!          - shell_gpu_data_create
          !!          - shell_gpu_allocate
          !!          - shell_gpu_set_mat_params / shell_gpu_set_hg_params
          !!          - shell_gpu_upload_constant / shell_gpu_upload_ip_state
          !!
          !!          This routine only transfers the per-step node data (positions
          !!          and velocities) to the device, executes the three kernels, and
          !!          copies the resulting nodal forces / moments / stiffness back.
          !!
          !!          raw_cpu_to_gpu is [9*NUMNOD]: Xx|Xy|Xz|Vx|Vy|Vz|VRx|VRy|VRz
          !!          raw_gpu_to_cpu is [8*NUMNOD]: Fx|Fy|Fz|Mx|My|Mz|STIFN|STIFR
          subroutine shell_gpu_full_step(g, dt, &
            raw_cpu_to_gpu, raw_gpu_to_cpu) &
            bind(c, name='shell_gpu_full_step')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    value, intent(in) :: dt             !< Current time step
            real(WP),    intent(in)        :: raw_cpu_to_gpu(*) !< H2D: 9*NUMNOD contiguous
            real(WP),    intent(inout)     :: raw_gpu_to_cpu(*) !< D2H: 8*NUMNOD contiguous
          end subroutine shell_gpu_full_step

          ! --------------------------------------------------------------------------------------------------------------------
          !  9. Async variants for pipelined SU iteration
          ! --------------------------------------------------------------------------------------------------------------------

          !! \brief Pin (page-lock) the Fortran host transfer buffers.
          !! \details Enables truly asynchronous cudaMemcpyAsync. Call once
          !!          after allocating raw_cpu_to_gpu and raw_gpu_to_cpu.
          subroutine shell_gpu_pin_host_memory(g, raw_cpu_to_gpu, raw_gpu_to_cpu) &
            bind(c, name='shell_gpu_pin_host_memory')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    intent(in)        :: raw_cpu_to_gpu(*)
            real(WP),    intent(in)        :: raw_gpu_to_cpu(*)
          end subroutine shell_gpu_pin_host_memory

          !! \brief Unpin host transfer buffers. Call before deallocation.
          subroutine shell_gpu_unpin_host_memory(raw_cpu_to_gpu, raw_gpu_to_cpu) &
            bind(c, name='shell_gpu_unpin_host_memory')
            import :: WP
            real(WP),    intent(in)        :: raw_cpu_to_gpu(*)
            real(WP),    intent(in)        :: raw_gpu_to_cpu(*)
          end subroutine shell_gpu_unpin_host_memory

          ! --------------------------------------------------------------------------------------------------------------------

          !! \brief Async full step: H2D + 3 kernels + D2H enqueued, NO sync.
          !! \details The caller must call shell_gpu_synchronize(handle) before
          !!          reading the force/moment/stiffness arrays.
          subroutine shell_gpu_full_step_async(g, dt, X, V, VR, raw_gpu_to_cpu) &
            bind(c, name='shell_gpu_full_step_async')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    value, intent(in) :: dt             !< Current time step
            real(WP),    intent(in)        :: X(3,*), V(3,*), VR(3,*) !< H2D: 9*NUMNOD contiguous
            real(WP),    intent(inout)     :: raw_gpu_to_cpu(*) !< D2H: 8*NUMNOD contiguous
          end subroutine shell_gpu_full_step_async

          !! \brief Async ALDT² download: D2H enqueued, NO sync.
          !! \details The caller must call shell_gpu_synchronize(handle) before
          !!          reading h_aldt_sq.
          subroutine shell_gpu_download_aldt_sq_async(g, h_aldt_sq) &
            bind(c, name='shell_gpu_download_aldt_sq_async')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in) :: g
            real(WP),    intent(inout)     :: h_aldt_sq(*) !< ALDT² per element [NUMELC]
          end subroutine shell_gpu_download_aldt_sq_async

          !! \brief Compute min element timestep via GPU reduction kernel.
          !! \details Replaces the pattern of downloading NUMELC doubles
          !!          (shell_gpu_download_aldt_sq_async) + Fortran reduction loop.
          !!          Downloads a single scalar (8 bytes) instead.
          !!          The caller must call shell_gpu_synchronize() before reading
          !!          h_dt_min.
          subroutine shell_gpu_min_dt(g, dtfac, h_dt_min) &
            bind(c, name='shell_gpu_min_dt')
            import :: c_ptr , WP
            type(c_ptr),       value, intent(in)    :: g
            real(WP),    value, intent(in)    :: dtfac     !< Timestep scale factor
            real(WP),           intent(inout) :: h_dt_min  !< Result: min element dt
          end subroutine shell_gpu_min_dt

          ! --------------------------------------------------------------------------------------------------------------------
          !  Global (shared) node/force handle
          ! --------------------------------------------------------------------------------------------------------------------

          !! \brief Create global handle: allocate device node/force arrays for NUMNOD nodes.
          function shell_gpu_global_create(numnod) result(gh) bind(c, name='shell_gpu_global_create')
            import :: c_ptr , c_int
            integer(c_int), value, intent(in) :: numnod
            type(c_ptr) :: gh
          end function shell_gpu_global_create

          !! \brief Destroy global handle: free device arrays, stream, event.
          subroutine shell_gpu_global_destroy(gh) bind(c, name='shell_gpu_global_destroy')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: gh
          end subroutine shell_gpu_global_destroy

          !! \brief Upload NODES%X/V/VR (H2D) + zero force arrays + record event.
          subroutine shell_gpu_global_upload_nodes(gh, X, V, VR) &
            bind(c, name='shell_gpu_global_upload_nodes')
            import :: c_ptr , WP
            type(c_ptr),    value, intent(in) :: gh
            real(WP), intent(in)        :: X(3,*), V(3,*), VR(3,*)
          end subroutine shell_gpu_global_upload_nodes

          !! \brief Download force arrays (D2H) after all SU kernels complete.
          subroutine shell_gpu_global_download_forces(gh, raw_gpu_to_cpu) &
            bind(c, name='shell_gpu_global_download_forces')
            import :: c_ptr , WP
            type(c_ptr),    value, intent(in) :: gh
            real(WP), intent(inout)     :: raw_gpu_to_cpu(*)
          end subroutine shell_gpu_global_download_forces

          !! \brief Block CPU until global stream completes.
          subroutine shell_gpu_global_synchronize(gh) bind(c, name='shell_gpu_global_synchronize')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: gh
          end subroutine shell_gpu_global_synchronize

          !! \brief Make SU stream wait for global upload to finish.
          subroutine shell_gpu_global_wait_upload(gh, g) &
            bind(c, name='shell_gpu_global_wait_upload')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: gh
            type(c_ptr), value, intent(in) :: g
          end subroutine shell_gpu_global_wait_upload

          !! \brief Make global stream wait for SU kernels to finish.
          subroutine shell_gpu_global_wait_su(gh, g) &
            bind(c, name='shell_gpu_global_wait_su')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: gh
            type(c_ptr), value, intent(in) :: g
          end subroutine shell_gpu_global_wait_su

          !! \brief Pin Fortran NODES%X/V/VR and force buffer for async memcpy.
          subroutine shell_gpu_global_pin_host(X, V, VR, raw_gpu_to_cpu, numnod) &
            bind(c, name='shell_gpu_global_pin_host')
            import :: WP , c_int
            real(WP), intent(in)    :: X(3,*), V(3,*), VR(3,*)
            real(WP), intent(inout) :: raw_gpu_to_cpu(*)
            integer(c_int), value, intent(in) :: numnod
          end subroutine shell_gpu_global_pin_host

          !! \brief Set back-pointer from per-SU handle to global handle.
          subroutine shell_gpu_set_global(g, gh) bind(c, name='shell_gpu_set_global')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: g
            type(c_ptr), value, intent(in) :: gh
          end subroutine shell_gpu_set_global

        end interface

#else
      contains

        ! ======================================================================================================================
        !  Stub implementations — used when building without CUDA (WITH_CUDA not defined).
        !  These empty procedures satisfy the linker; they are never called at runtime
        !  because the GPU code path is guarded by the gpu_shell_available flag.
        ! ======================================================================================================================

        ! --- Lifecycle ---

        function shell_gpu_data_create() result(g)
          type(c_ptr) :: g
          g = c_null_ptr
        end function shell_gpu_data_create

        subroutine shell_gpu_data_destroy(g)
          type(c_ptr), value, intent(in) :: g
        end subroutine shell_gpu_data_destroy

        subroutine shell_gpu_allocate(g, numelc, numnod, npt, ismstr, ithk)
          type(c_ptr),    value, intent(in) :: g
          integer(c_int), value, intent(in) :: numelc, numnod, npt, ismstr, ithk
        end subroutine shell_gpu_allocate

        subroutine shell_gpu_deallocate(g)
          type(c_ptr), value, intent(in) :: g
        end subroutine shell_gpu_deallocate

        ! --- Parameter setters ---

        subroutine shell_gpu_set_mat_params(g, &
          e_young, nu, g_shear, a11, a12, &
          ca, cb, cn, cc, epdr, epmx, ymax, m_exp, &
          fisokin, rhocp, tref, tmelt, asrate, &
          rho, ssp, shf_coef, &
          ipla, vp, iform, icc, z3, z4)
          type(c_ptr),    value, intent(in) :: g
          real(WP), value, intent(in) :: e_young, nu, g_shear, a11, a12
          real(WP), value, intent(in) :: ca, cb, cn, cc, epdr, epmx, ymax, m_exp
          real(WP), value, intent(in) :: fisokin, rhocp, tref, tmelt, asrate
          real(WP), value, intent(in) :: rho, ssp, shf_coef
          integer(c_int), value, intent(in) :: ipla, vp, iform, icc
          real(WP), value, intent(in) :: z3, z4
        end subroutine shell_gpu_set_mat_params

        subroutine shell_gpu_set_hg_params(g, h1, h2, h3, srh1, srh2, srh3, &
          hvisc_in, helas_in, hvlin_in)
          type(c_ptr),    value, intent(in) :: g
          real(WP), value, intent(in) :: h1, h2, h3, srh1, srh2, srh3
          real(WP), value, intent(in) :: hvisc_in, helas_in, hvlin_in
        end subroutine shell_gpu_set_hg_params

        subroutine shell_gpu_set_compute_sti(g, flag)
          type(c_ptr),    value, intent(in) :: g
          integer(c_int), value, intent(in) :: flag
        end subroutine shell_gpu_set_compute_sti

        subroutine shell_gpu_set_ihbe(g, ihbe)
          type(c_ptr),    value, intent(in) :: g
          integer(c_int), value, intent(in) :: ihbe
        end subroutine shell_gpu_set_ihbe

        ! --- Data transfers ---

        subroutine shell_gpu_upload_constant(g, &
          h_n1, h_n2, h_n3, h_n4, &
          h_thk0, h_off, h_ssp, h_rho, h_ym, h_nu, h_a11, h_g_shear, h_shf)
          type(c_ptr),    value, intent(in) :: g
          integer(c_int), intent(in) :: h_n1(*), h_n2(*), h_n3(*), h_n4(*)
          real(WP), intent(in) :: h_thk0(*), h_off(*)
          real(WP), intent(in) :: h_ssp(*), h_rho(*), h_ym(*), h_nu(*)
          real(WP), intent(in) :: h_a11(*), h_g_shear(*), h_shf(*)
        end subroutine shell_gpu_upload_constant

        subroutine shell_gpu_upload_ip_state(g, &
          h_sigxx, h_sigyy, h_sigxy, h_sigyz, h_sigzx, &
          h_pla, h_epsd_ip, h_sigbakxx, h_sigbakyy, h_sigbakxy, h_tempel)
          type(c_ptr),    value, intent(in) :: g
          real(WP), intent(in) :: h_sigxx(*), h_sigyy(*), h_sigxy(*)
          real(WP), intent(in) :: h_sigyz(*), h_sigzx(*)
          real(WP), intent(in) :: h_pla(*), h_epsd_ip(*)
          real(WP), intent(in) :: h_sigbakxx(*), h_sigbakyy(*), h_sigbakxy(*)
          real(WP), intent(in) :: h_tempel(*)
        end subroutine shell_gpu_upload_ip_state

        subroutine shell_gpu_upload_nodes(g, raw_cpu_to_gpu)
          type(c_ptr),    value, intent(in) :: g
          real(WP), intent(in) :: raw_cpu_to_gpu(*)
        end subroutine shell_gpu_upload_nodes

        subroutine shell_gpu_download_nodal_forces(g, raw_gpu_to_cpu)
          type(c_ptr),    value, intent(in) :: g
          real(WP), intent(inout) :: raw_gpu_to_cpu(*)
        end subroutine shell_gpu_download_nodal_forces

        subroutine shell_gpu_download_energy(g, h_eint)
          type(c_ptr),    value, intent(in) :: g
          real(WP), intent(inout) :: h_eint(*)
        end subroutine shell_gpu_download_energy

        subroutine shell_gpu_download_aldt_sq(g, h_aldt_sq)
          type(c_ptr),    value, intent(in) :: g
          real(WP), intent(inout) :: h_aldt_sq(*)
        end subroutine shell_gpu_download_aldt_sq

        subroutine shell_gpu_download_state(g, &
          h_off, h_thk, h_gstr, h_epsd_elem, &
          h_sigxx, h_sigyy, h_sigxy, h_sigyz, h_sigzx, &
          h_pla, h_epsd_ip, h_sigbakxx, h_sigbakyy, h_sigbakxy, h_tempel)
          type(c_ptr),    value, intent(in) :: g
          real(WP), intent(inout) :: h_off(*), h_thk(*), h_gstr(*), h_epsd_elem(*)
          real(WP), intent(inout) :: h_sigxx(*), h_sigyy(*), h_sigxy(*)
          real(WP), intent(inout) :: h_sigyz(*), h_sigzx(*)
          real(WP), intent(inout) :: h_pla(*), h_epsd_ip(*)
          real(WP), intent(inout) :: h_sigbakxx(*), h_sigbakyy(*), h_sigbakxy(*)
          real(WP), intent(inout) :: h_tempel(*)
        end subroutine shell_gpu_download_state

        ! --- Kernel execution ---

        subroutine shell_gpu_zero_nodal_arrays(g)
          type(c_ptr), value, intent(in) :: g
        end subroutine shell_gpu_zero_nodal_arrays

        subroutine shell_gpu_run_kernels(g, dt)
          type(c_ptr),    value, intent(in) :: g
          real(WP), value, intent(in) :: dt
        end subroutine shell_gpu_run_kernels

        subroutine shell_gpu_synchronize(g)
          type(c_ptr), value, intent(in) :: g
        end subroutine shell_gpu_synchronize

        ! --- Host pinning ---

        subroutine shell_gpu_pin_host_memory(g, raw_cpu_to_gpu, raw_gpu_to_cpu)
          type(c_ptr),    value, intent(in) :: g
          real(WP), intent(in) :: raw_cpu_to_gpu(*)
          real(WP), intent(in) :: raw_gpu_to_cpu(*)
        end subroutine shell_gpu_pin_host_memory

        subroutine shell_gpu_unpin_host_memory(raw_cpu_to_gpu, raw_gpu_to_cpu)
          real(WP), intent(in) :: raw_cpu_to_gpu(*)
          real(WP), intent(in) :: raw_gpu_to_cpu(*)
        end subroutine shell_gpu_unpin_host_memory

        ! --- Full step ---

        subroutine shell_gpu_full_step(g, dt, raw_cpu_to_gpu, raw_gpu_to_cpu)
          type(c_ptr),    value, intent(in) :: g
          real(WP), value, intent(in) :: dt
          real(WP), intent(in)    :: raw_cpu_to_gpu(*)
          real(WP), intent(inout) :: raw_gpu_to_cpu(*)
        end subroutine shell_gpu_full_step

        ! --- Async variants ---

        subroutine shell_gpu_full_step_async(g, dt, X, V, VR, raw_gpu_to_cpu)
          type(c_ptr),    value, intent(in) :: g
          real(WP), value, intent(in) :: dt
          real(WP), intent(in)    :: X(3,*), V(3,*), VR(3,*)
          real(WP), intent(inout) :: raw_gpu_to_cpu(*)
        end subroutine shell_gpu_full_step_async

        subroutine shell_gpu_download_aldt_sq_async(g, h_aldt_sq)
          type(c_ptr),    value, intent(in) :: g
          real(WP), intent(inout) :: h_aldt_sq(*)
        end subroutine shell_gpu_download_aldt_sq_async

        subroutine shell_gpu_min_dt(g, dtfac, h_dt_min)
          type(c_ptr),    value, intent(in)    :: g
          real(WP), value, intent(in)    :: dtfac
          real(WP),        intent(inout) :: h_dt_min
        end subroutine shell_gpu_min_dt

        ! --- Global (shared) node/force handle ---

        function shell_gpu_global_create(numnod) result(gh)
          integer(c_int), value, intent(in) :: numnod
          type(c_ptr) :: gh
          gh = c_null_ptr
        end function shell_gpu_global_create

        subroutine shell_gpu_global_destroy(gh)
          type(c_ptr), value, intent(in) :: gh
        end subroutine shell_gpu_global_destroy

        subroutine shell_gpu_global_upload_nodes(gh, X, V, VR)
          type(c_ptr),    value, intent(in) :: gh
          real(WP), intent(in) :: X(3,*), V(3,*), VR(3,*)
        end subroutine shell_gpu_global_upload_nodes

        subroutine shell_gpu_global_download_forces(gh, raw_gpu_to_cpu)
          type(c_ptr),    value, intent(in) :: gh
          real(WP), intent(inout) :: raw_gpu_to_cpu(*)
        end subroutine shell_gpu_global_download_forces

        subroutine shell_gpu_global_synchronize(gh)
          type(c_ptr), value, intent(in) :: gh
        end subroutine shell_gpu_global_synchronize

        subroutine shell_gpu_global_wait_upload(gh, g)
          type(c_ptr), value, intent(in) :: gh
          type(c_ptr), value, intent(in) :: g
        end subroutine shell_gpu_global_wait_upload

        subroutine shell_gpu_global_wait_su(gh, g)
          type(c_ptr), value, intent(in) :: gh
          type(c_ptr), value, intent(in) :: g
        end subroutine shell_gpu_global_wait_su

        subroutine shell_gpu_global_pin_host(X, V, VR, raw_gpu_to_cpu, numnod)
          real(WP), intent(in)    :: X(3,*), V(3,*), VR(3,*)
          real(WP), intent(inout) :: raw_gpu_to_cpu(*)
          integer(c_int), value, intent(in) :: numnod
        end subroutine shell_gpu_global_pin_host

        subroutine shell_gpu_set_global(g, gh)
          type(c_ptr), value, intent(in) :: g
          type(c_ptr), value, intent(in) :: gh
        end subroutine shell_gpu_set_global

#endif

      end module shell_gpu_mod

