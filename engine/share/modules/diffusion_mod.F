Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
Chd|====================================================================
Chd|  DIFFUSION_MOD                 share/modules/diffusion_mod.F 
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|-- calls ---------------
Chd|        LINEAR_SOLVER_MOD             share/modules/linear_solver_mod.F
Chd|        MATRIX_MOD                    ../common_source/linearalgebra/matrix_mod.F
Chd|        VECTOR_MOD                    ../common_source/linearalgebra/vector_mod.F
Chd|====================================================================
      MODULE DIFFUSION_MOD
      USE LINEAR_SOLVER_MOD
      USE MATRIX_MOD
      USE VECTOR_MOD
      implicit none
#include "my_real.inc"
!     .___________________________.   !
!     |                           |   !
!     |   **********************  |   !
!     |   ** Type definitions **  |   !
!     |   **********************  |   !
!     |___________________________|   !

      type :: t_diffusion
      class(t_linear_solver), private, pointer :: linear_solver
      type(t_cfs_matrix) :: mat
      type(t_vector) :: rhs
      double precision, dimension(:), allocatable :: sol
      integer, dimension(:), allocatable :: flag_outlet
      my_real, dimension(:), allocatable :: nu
      integer :: nrhs
      logical :: outlet_flagged = .false.
      contains
      procedure, pass :: init_diffusion
      procedure, pass :: solve_diffusion
      procedure, pass :: terminate_diffusion
      procedure, pass :: get_solution
      end type t_diffusion

!     ._____________________________.   !
!     |                             |   !
!     |   ************************  |   !
!     |   ** Interface routines **  |   !
!     |   ************************  |   !
!     |_____________________________|   !

      contains
      subroutine init_diffusion(this, linsol, nrhs, iparg, ale_connect)
      use linear_solver_mod
      use ale_connectivity_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
!     NSPMD
#include      "com01_c.inc"
!     NPARG
#include      "param_c.inc"
!     NUMELS
#include      "com04_c.inc"
!     NSVOIS
#include      "spmd_c.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      class(t_diffusion) :: this
      class(t_linear_solver), intent(in), target :: linsol
      integer, dimension(nparg, ngroup), intent(in) :: iparg
      integer, intent(in) :: nrhs
      type(t_ale_connectivity), intent(in) :: ale_connect
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer :: mat_nz, ng, ii, nel, ity, matlaw, nft, i, jj, kk
      integer :: glob_dim, max_id, ierr, iad, lgth

      this%nrhs = nrhs
!     number of non zero values for the diffusion matrix elem <-> elem matrix
      mat_nz = 0
      do ng = 1, ngroup
         matlaw = iparg(1, ng)
         if (matlaw == 151) then
            nel = iparg(2, ng)
            nft = iparg(3, ng)
            ity = iparg(5, ng)
            do ii = 1, nel
               i = ii + nft
               mat_nz = mat_nz + 1
               iad = ale_connect%ee_connect%iad_connect(i)
               lgth = ale_connect%ee_connect%iad_connect(i+1)-ale_connect%ee_connect%iad_connect(i)
               do jj = 1, lgth
                  kk = ale_connect%ee_connect%connected(iad + jj - 1)
                  if (kk > 0) then
                     mat_nz = mat_nz + 1
                  endif
               enddo
            enddo
         endif
      enddo
!     create matrix
      call this%mat%matrix_create(mat_nz)
!     create rhs
      call this%rhs%create(nrhs * numels)
!     global system size
      glob_dim = numels
      max_id = maxval(ale_connect%idglob%id(1:numels + nsvois))
#ifdef MPI
      if (nspmd > 1) then
         call mpi_allreduce(max_id, glob_dim, 1, mpi_int, mpi_max, 
     .        mpi_comm_world, ierr)
      endif
#endif
!     solution
      allocate(this%sol(3 * glob_dim))
!     init solver
      this%linear_solver => linsol
      call this%linear_solver%init_solver(glob_dim)
!     set rhs and matrix
      call this%linear_solver%set_rhs(3, this%rhs)
      call this%linear_solver%set_matrix(this%mat)
!     diffusion coefficient, outlet flag
      this%outlet_flagged = .false.
      if (n2d == 0) then
         allocate(this%nu(numels + nsvois))
         this%nu(1:numels + nsvois) = zero
         allocate(this%flag_outlet(6 * numels))
         this%flag_outlet(1:6 * numels) = 0
      else
         allocate(this%nu(numelq + numeltg + nqvois + ntgvois))
         this%nu(1:numelq + numeltg + nqvois + ntgvois) = zero
         allocate(this%flag_outlet(4 * numelq + 3 * numeltg))
         this%flag_outlet(1:4 * numelq + 3 * numeltg) = 0
      endif

      end subroutine init_diffusion

      subroutine solve_diffusion(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
      class(t_diffusion) :: this
      integer :: glob_dim
      
      glob_dim = this%linear_solver%get_global_dim()
      call this%linear_solver%solve(this%sol, this%nrhs * glob_dim)
      
      end subroutine solve_diffusion

      subroutine get_solution(this, sol, glob_dim)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
      class(t_diffusion), intent(inout), target :: this
      integer, intent(out) :: glob_dim
      
      double precision, dimension(:), intent(inout), pointer :: sol
      sol => this%sol
      glob_dim = this%linear_solver%get_global_dim()
      end subroutine get_solution

      subroutine terminate_diffusion(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
      class(t_diffusion), intent(inout) :: this
      
    
      call this%linear_solver%terminate()
      call this%mat%matrix_destroy()
      call this%rhs%destroy()
      if (allocated(this%sol)) deallocate(this%sol)
      if (allocated(this%nu)) deallocate(this%nu)
      if (allocated(this%flag_outlet)) deallocate(this%flag_outlet)
      end subroutine terminate_diffusion
      end module diffusion_mod
