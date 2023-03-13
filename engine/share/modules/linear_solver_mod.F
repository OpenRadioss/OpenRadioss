Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  LINEAR_SOLVER_MOD             share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        DIFFUSION_MOD                 share/modules/diffusion_mod.F 
Chd|-- calls ---------------
Chd|        MATRIX_MOD                    ../common_source/linearalgebra/matrix_mod.F
Chd|        VECTOR_MOD                    ../common_source/linearalgebra/vector_mod.F
Chd|====================================================================
      MODULE LINEAR_SOLVER_MOD
      USE MATRIX_MOD
      USE VECTOR_MOD
      implicit none
#ifdef MUMPS5
#include "dmumps_struc.h"
#endif
#include "my_real.inc"

!     .___________________________.   !
!     |                           |   !
!     |   **********************  |   !
!     |   ** Type definitions **  |   !
!     |   **********************  |   !
!     |___________________________|   !

!     *********************    !
!     Generic Linear Solver    !
!     *********************    !

      type t_linear_solver
      integer, private :: global_dimension
      contains
      procedure, pass :: init_solver
      procedure, pass :: set_matrix
      procedure, pass :: set_rhs
      procedure, pass :: solve
      procedure, pass :: terminate
      procedure, nopass :: error
      procedure, pass :: get_global_dim
      end type t_linear_solver

!     ********************    !
!     MUMPS Linear Solver     !
!     ********************    !
#ifdef MUMPS5
      type, extends(t_linear_solver) :: t_mumps_solver
      type(dmumps_struc), private :: mumps_par
      logical :: job_1_done = .false.
      contains
      procedure, pass :: init_solver_mumps
      procedure, pass :: set_matrix_mumps
      procedure, pass :: set_rhs_mumps
      procedure, pass :: solve_mumps
      procedure, pass :: terminate_mumps
      end type t_mumps_solver
#endif

!     *************************    !
!     Conjugate gradient solver    !
!     *************************    !
      type, extends(t_linear_solver) :: t_cg_solver
      type(t_cfs_matrix), pointer :: mat
      type(t_vector), pointer :: rhs
      type(t_vector) :: sol_vec, r, rnew, temp, p
      my_real, dimension(:), allocatable :: diag
      integer :: nrhs
      contains
      procedure, pass :: init_solver_cg
      procedure, pass :: set_matrix_cg
      procedure, pass :: set_rhs_cg
      procedure, pass :: solve_cg
      procedure, pass :: terminate_cg
      end type t_cg_solver
      contains
      
!     ._____________________________.   !
!     |                             |   !
!     |   ************************  |   !
!     |   ** Interface routines **  |   !
!     |   ************************  |   !
!     |_____________________________|   !

!     ****************     !
!     Generic routines     !
!     ****************     !

!     Error
!     -----
      subroutine error()
      print*, "error"
      end subroutine error
      
      function get_global_dim(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_linear_solver), intent(in) :: this
      integer :: get_global_dim
      get_global_dim = this%global_dimension
      end function get_global_dim

!     Solver initialization
!     ---------------------
      subroutine init_solver(this, mat_dim)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_linear_solver), intent(inout) :: this
      integer, intent(in) :: mat_dim
      this%global_dimension = mat_dim
      select type(this)
#ifdef MUMPS5
      type is (t_mumps_solver)
      call this%init_solver_mumps(mat_dim)
#endif
      type is (t_cg_solver)
      call this%init_solver_cg(mat_dim)
      class default
      call this%error()
      end select
      end subroutine init_solver

!     Set matrix
!     ----------
      subroutine set_matrix(this, mat)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MATRIX_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_linear_solver), intent(inout) :: this
      type(t_cfs_matrix), intent(inout) :: mat
      select type(this)
#ifdef MUMPS5
      type is (t_mumps_solver)
      call this%set_matrix_mumps(mat)
#endif
      type is (t_cg_solver)
      call this%set_matrix_cg(mat)
      class default
      call this%error()
      end select
      end subroutine set_matrix

!     Set right hand side
!     -------------------
      subroutine set_rhs(this, nrhs, rhs)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE VECTOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_linear_solver), intent(inout) :: this
      integer, intent(in) :: nrhs
      type(t_vector), intent(inout) :: rhs
      select type(this)
#ifdef MUMPS5
      type is (t_mumps_solver)
      call this%set_rhs_mumps(nrhs, rhs)
#endif
      type is (t_cg_solver)
      call this%set_rhs_cg(nrhs, rhs)
      class default
      call this%error()
      end select
      end subroutine set_rhs

!     Solve the linear system
!     -----------------------
      subroutine solve(this, sol, dim)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_linear_solver), intent(inout) :: this
      integer, intent(in) :: dim
      double precision, dimension(dim), intent(out) :: sol
      select type(this)
#ifdef MUMPS5
      type is (t_mumps_solver)
      call this%solve_mumps(sol, dim)
#endif
      type is (t_cg_solver)
      call this%solve_cg(sol, dim)
      class default
      call this%error()
      end select
      end subroutine solve

!     End the solver instance
!     -----------------------
      subroutine terminate(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_linear_solver), intent(inout) :: this
      select type(this)
#ifdef MUMPS5
      type is (t_mumps_solver)
      call this%terminate_mumps()
#endif
      type is (t_cg_solver)
      call this%terminate_cg()
      class default
      call this%error()
      end select
      end subroutine terminate

!     **************     !
!     MUMPS routines     !
!     **************     !

!     Solver initialization
!     ---------------------
#ifdef MUMPS5
      subroutine init_solver_mumps(this, mat_dim)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
      class (t_mumps_solver), intent(inout) :: this
      integer, intent(in) :: mat_dim
      this%mumps_par%par = 1
#ifdef MPI      
      this%mumps_par%comm = mpi_comm_world
#else 
      this%mumps_par%comm = -1
#endif
      this%mumps_par%job = -1
      this%mumps_par%sym = 0
      call dmumps(this%mumps_par)

!     matrice globale
      this%mumps_par%icntl(5) = 0
!     matrice distribuee
      this%mumps_par%icntl(18) = 3
!     taille de la matrice
      this%mumps_par%n = mat_dim
!     matrice sym  trique d  finie positive
      this%mumps_par%sym = 1
!     distributed rhs
      this%mumps_par%icntl(20) = 10
!     distributed solution
      !mumps_par%icntl(21) = 1
!     un-distributed solution
      this%mumps_par%icntl(21) = 0
!     info on solution
      this%mumps_par%icntl(11) = 1
!     dump matrice
!      this%mumps_par%write_problem = "mat"
!     verbosity
      this%mumps_par%icntl(4) = 0
      
      end subroutine init_solver_mumps
      
!     Set matrix
!     ----------
      subroutine set_matrix_mumps(this, mat)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      use matrix_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_mumps_solver), intent(inout) :: this
      type(t_cfs_matrix), intent(inout) :: mat
      call mat%matrix_associate(this%mumps_par%irn_loc, this%mumps_par%jcn_loc, this%mumps_par%a_loc)
      this%mumps_par%nnz_loc = mat%get_dim()
      end subroutine set_matrix_mumps

!     Set right hand side
!     -------------------
      subroutine set_rhs_mumps(this, nrhs, rhs)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      use vector_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_mumps_solver), intent(inout) :: this
      integer, intent(in) :: nrhs
      type(t_vector), intent(inout) :: rhs
C-----------------------------------------------
C     D u m m y   a r g u m e n t s
C-----------------------------------------------
      integer :: dim

      call rhs%associate(this%mumps_par%irhs_loc, this%mumps_par%rhs_loc)
      dim = rhs%get_dim() / nrhs
      this%mumps_par%nrhs = nrhs
      this%mumps_par%nloc_rhs = dim
      this%mumps_par%lrhs_loc = dim
      end subroutine set_rhs_mumps

!     Solve the linear system
!     -----------------------
      subroutine solve_mumps(this, sol, dim)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
      class (t_mumps_solver), intent(inout) :: this
      integer, intent(in) :: dim
!      my_real, dimension(dim), intent(out), target :: sol
      double precision, dimension(dim), intent(out), target :: sol
C-----------------------------------------------     
C     L o c a l   V a r i a b l e s 
C----------------------------------------------- 
      integer :: ierr
      if (dim /= this%mumps_par%n * this%mumps_par%nrhs) then
         print*, "*** Dimension mismatch in solution vector"
         return
      else
         if (.not. this%job_1_done) then
! analysis
            this%mumps_par%job = 1
            call dmumps(this%mumps_par)
            this%job_1_done = .true.
         endif
!     factorization
         this%mumps_par%job = 2
         call dmumps(this%mumps_par)

!     solve
         this%mumps_par%rhs => sol
         this%mumps_par%lrhs = this%mumps_par%n
         this%mumps_par%job = 3
         call dmumps(this%mumps_par)

#ifdef MPI
!     Sent to all procs
         IF (NSPMD > 1) THEN
            CALL MPI_BCAST(SOL, DIM, REAL, 0, MPI_COMM_WORLD, IERR)
         ENDIF
#endif     
      endif
      end subroutine solve_mumps

!     End the solver instance
!     -----------------------
      subroutine terminate_mumps(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
      class (t_mumps_solver), intent(inout) :: this

      this%mumps_par%job = -2
      call dmumps(this%mumps_par)
 
      end subroutine terminate_mumps
#endif
C END OF MUMPS5 SPECIFIC CODE

!     ***********     !
!     CG routines     !
!     ***********     !
!     Solver initialization
!     ---------------------
      subroutine init_solver_cg(this, mat_dim)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_cg_solver), intent(inout) :: this
      integer, intent(in) :: mat_dim

      call this%sol_vec%create(mat_dim)
      call this%r%create(mat_dim)
      call this%rnew%create(mat_dim)
      call this%temp%create(mat_dim)
      call this%p%create(mat_dim)
      allocate(this%diag(mat_dim))

      end subroutine init_solver_cg
      
!     Set matrix
!     ----------
      subroutine set_matrix_cg(this, mat)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      use matrix_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_cg_solver), intent(inout) :: this
      type(t_cfs_matrix), intent(inout), target :: mat
      
      this%mat => mat
      
      end subroutine set_matrix_cg

!     Set right hand side
!     -------------------
      subroutine set_rhs_cg(this, nrhs, rhs)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      use vector_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_cg_solver), intent(inout) :: this
      integer, intent(in) :: nrhs
      type(t_vector), intent(inout), target :: rhs

      integer :: ii
      integer :: lgth
      
      this%rhs => rhs
      this%nrhs = nrhs
      
      
      end subroutine set_rhs_cg

!     Solve the linear system
!     -----------------------
      subroutine solve_cg(this, sol, dim)
      use vector_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
      class (t_cg_solver), intent(inout) :: this
      integer, intent(in) :: dim
      double precision, dimension(dim), intent(out), target :: sol
C-----------------------------------------------     
C     L o c a l   V a r i a b l e s 
C----------------------------------------------- 
      integer :: iter, system_size, irhs, ii, i, j, mat_nnz
      my_real :: error, norm_init
      my_real :: alpha, beta
      ! a sortir sous forme de parametre
      integer :: max_iter
      my_real :: tol

      tol = 1.d-8
      
      system_size = dim / this%nrhs
      max_iter = system_size

!     diaginal matrix made of inverse of square root of diagonal elements of the 
!     system matrix
      mat_nnz = this%mat%get_dim()
      do ii = 1, mat_nnz
         i = this%mat%irow(ii)
         j = this%mat%jcol(ii)
         if (i == j) then
            this%diag(i) = ONE / sqrt(this%mat%val(ii))
         endif
      enddo
      

      do irhs = 1, this%nrhs
         this%sol_vec%val(1:system_size) = zero     
!     initialisation du solver
         call prod_vec(this%mat, this%sol_vec, this%temp)
         this%r%val(1:system_size) = this%rhs%val(system_size * (irhs - 1) + 1 : system_size * (irhs - 1) + system_size) - 
     .        this%temp%val(1:system_size)
         this%p%val(1:system_size) = this%r%val(1:system_size)
         norm_init = this%r%norm()
         error = norm_init
         iter = 0
         do while (iter <= max_iter .and. error > tol)
            iter = iter + 1
            call prod_vec(this%mat, this%p, this%temp)
            alpha = dot_product(this%r%val(1:system_size),this% r%val(1:system_size)) /
     .           dot_product(this%temp%val(1:system_size), this%p%val(1:system_size))
            do ii = 1, system_size
               this%sol_vec%val(ii) = this%sol_vec%val(ii) + alpha * this%p%val(ii)
               this%rnew%val(ii) = this%r%val(ii) - alpha * this%temp%val(ii)
            enddo
            beta = dot_product(this%rnew%val(1:system_size), this%rnew%val(1:system_size)) / 
     .           dot_product(this%r%val(1:system_size), this%r%val(1:system_size))
            do ii = 1, system_size
               this%p%val(ii) = this%rnew%val(ii) + beta * this%p%val(ii)
               this%r%val(ii) =  this%rnew%val(ii)
            enddo
            error = this%r%norm() / norm_init
         enddo
         sol(system_size * (irhs - 1) + 1:system_size * (irhs - 1) + system_size) = 
     .        this%sol_vec%val(1:system_size)
      enddo

      
      end subroutine solve_cg

!     End the solver instance
!     -----------------------
      subroutine terminate_cg(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class (t_cg_solver), intent(inout) :: this
      call this%sol_vec%destroy()
      call this%r%destroy()
      call this%temp%destroy()
      call this%p%destroy()
      call this%rnew%destroy()
      deallocate(this%diag)
      end subroutine terminate_cg

      end module linear_solver_mod
