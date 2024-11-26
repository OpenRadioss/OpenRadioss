!opyright>        OpenRadioss
!opyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!opyright>
!opyright>        This program is free software: you can redistribute it and/or modify
!opyright>        it under the terms of the GNU Affero General Public License as published by
!opyright>        the Free Software Foundation, either version 3 of the License, or
!opyright>        (at your option) any later version.
!opyright>
!opyright>        This program is distributed in the hope that it will be useful,
!opyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!opyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!opyright>        GNU Affero General Public License for more details.
!opyright>
!opyright>        You should have received a copy of the GNU Affero General Public License
!opyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!opyright>
!opyright>
!opyright>        Commercial Alternative: Altair Radioss Software
!opyright>
!opyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!opyright>        software under a commercial license.  Contact Altair to discuss further if the
!opyright>        commercial version may interest you: https://www.altair.com/radioss/.
module multi_fvm_mod
   use elbufdef_mod ,only: l_bufel_,buf_eos_
#include "my_real.inc"

   ! --------------------------------------------------
   ! /INT18 + LAW 151
   ! ----------------
   ! force accumulation (dt x fx) for remote nodes
   ! size : parith/on  : 3,6,NSN*NTHREADS
   !        parith/off : 0
   type remote_multi_fvm
      integer :: nodfi
      real(kind=8), dimension(:,:,:), allocatable :: r_force_int
   end type remote_multi_fvm
   ! --------------------------------------------------

   type face_data_struct
      my_real, dimension(:, :), allocatable :: surf
      my_real, dimension(:, :, :), allocatable :: normal, wfac
      my_real, dimension(:, :, :), allocatable :: centroid
   end type face_data_struct

   type elem_data_struct
      my_real, dimension(:, :), allocatable :: centroid
   end type elem_data_struct

   type fvm_inlet_data_struct
      integer :: formulation
      integer :: vector_velocity
      integer, dimension(3) :: func_vel
      integer, dimension(21) :: func_alpha,func_rho,func_pres
      my_real, dimension(3) :: val_vel
      my_real, dimension(21) :: val_alpha,val_rho,val_pres
   end type fvm_inlet_data_struct

   type fvm_connectivity_struct
      integer, dimension(:), allocatable :: kvois
   end type fvm_connectivity_struct

   type multi_fvm_struct
      type(face_data_struct) :: face_data
      type(elem_data_struct) :: elem_data
      integer :: iebcslgth
      integer :: nbmat
      logical :: is_used                  !< card /mat/law151 used in input file
      logical :: is_associated_to_a_part  !< card /mat/law151 can be used in input file but not necessarily associated to a given part
      logical :: are_all_parts_151        !< check if all part are associated with law151 (otherwise mixed scheme)
      logical :: is_restart
      integer :: muscl
!     compression coefficient for phase advection
      my_real :: beta
      my_real :: pres_shift
      integer :: nelem
      integer, dimension(:, :), allocatable :: n4_vois
      my_real, dimension(:, :, :), allocatable :: fluxes, subvol_fluxes, submass_fluxes, subener_fluxes
      my_real, dimension(:, :), allocatable :: vel
      my_real, dimension(:, :), allocatable :: acc
      my_real, dimension(:), allocatable :: sound_speed
      my_real, dimension(:), allocatable :: rho, eint, pres
      my_real, dimension(:), allocatable :: tburn, vol
      my_real, dimension(:, :), allocatable :: bfrac
      integer, pointer, dimension(:) :: pcnel, paddcnel, paddtmpl

      ! indicates whether we run in 3d (sym = 0), or 2d (sym = 1 planar case, sym = 2 cylindrical case)
      integer :: sym
      ! low mach options for water / air applications
      logical :: lowmach_opt
      ! muscl variables
      my_real, dimension(:, :), allocatable :: grad_rho, grad_u, grad_v, grad_w, grad_pres
      my_real, dimension(:, :), allocatable :: phase_alpha, phase_pres, phase_rho, phase_eint
      my_real, dimension(:, :, :), allocatable :: phase_grad_rho, phase_grad_alpha, phase_grad_pres
      ! ebcs
      type(fvm_connectivity_struct) :: fvm_connectivity
      ! --------------------------------------------------
      ! /int18 + law 151
      ! ----------------
      ! is_int18_law151 : boolean, true if /int18 + law 151
      ! number_int18 : integer, number of interface /int18
      ! int18_list : integer, dimension = number_int18, list of interface 18
      ! int18_global_list : boolean, dimension=ninter, true if /int18 + law151 for the nin interface
      !                     used for the mpi comm spmd_i7fcom_poff/pon
      ! s_append_array : integer, size of x_append,v_append,mass_append and kinet_append arrays
      logical :: is_int18_law151
      integer :: number_int18
      integer, dimension(:), allocatable :: int18_list
      logical, dimension(:), allocatable :: int18_global_list

      integer :: s_append_array ! size of x_append,v_append,mass_append and kinet_append arrays
      ! extended position/velocity/mass/kinet for /int18 + law 151
      ! size : numnod + numels
      my_real, dimension(:), allocatable :: x_append
      my_real, dimension(:), allocatable :: v_append
      my_real, dimension(:), allocatable :: mass_append
      integer, dimension(:), allocatable :: kinet_append
      ! force accumulation (dt x fx)
      ! size : parith/on  : 3,0
      !        parith/off : 3,numels*nthreads
      integer :: size_force_int_1   ! --> 1srt dimension = 3
      integer :: size_force_int_2   ! --> 2ns dimension  = numels*nthreads or 0
      my_real, dimension(:,:), allocatable :: force_int

      integer :: size_force_int_pon   ! --> 3rd dimension = numelsxnthreads (parith/on) or 0 (parith/off)
      real(kind=8), dimension(:,:,:), allocatable :: force_int_pon

      ! force accumulation (dt x fx) for remote nodes
      ! size : parith/on  : number of /type18+/law151
      !        parith/off : 0
      type(remote_multi_fvm), dimension(:), allocatable :: r_afi
      ! --------------------------------------------------
      ! navier-stokes diffusion activated with law 6
      logical :: ns_diff
   end type multi_fvm_struct

   type lbuf_ptr
      type(l_bufel_), pointer :: lbuf
   end type lbuf_ptr

   type ebuf_ptr
      type(buf_eos_), pointer :: ebuf
   end type ebuf_ptr

   type fvm_inivel_struct
      logical :: flag
      integer :: grbricid, grquadid, grsh3nid
      my_real :: vx, vy, vz
   end type fvm_inivel_struct



contains


end module multi_fvm_mod
