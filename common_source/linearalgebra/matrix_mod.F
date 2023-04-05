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
Chd|  MATRIX_MOD                    linearalgebra/matrix_mod.F    
Chd|-- called by -----------
Chd|        SET_MATRIX                    engine/share/modules/linear_solver_mod.F
Chd|        SET_MATRIX_CG                 engine/share/modules/linear_solver_mod.F
Chd|        SET_MATRIX_MUMPS              engine/share/modules/linear_solver_mod.F
Chd|        DIFFUSION_MOD                 engine/share/modules/diffusion_mod.F
Chd|        LINEAR_SOLVER_MOD             engine/share/modules/linear_solver_mod.F
Chd|-- calls ---------------
Chd|        VECTOR_MOD                    linearalgebra/vector_mod.F    
Chd|====================================================================
      MODULE MATRIX_MOD
      implicit none
#include      "my_real.inc"

!     *******************************     !
!     Coordinate format Sparse Matrix     !
!     *******************************     !

      type t_cfs_matrix
      integer, private :: dim
      integer, dimension(:), allocatable :: irow, jcol
      double precision, dimension(:), allocatable :: val
      contains
      procedure, pass :: matrix_create
      procedure, pass :: matrix_destroy
      procedure, pass :: matrix_associate
      procedure, pass :: get_dim
      end type t_cfs_matrix
      
      contains

!     ************     !
!     Mat vec prod     !
!     ************     !

      subroutine prod_vec(this, xvec, bvec)
      USE VECTOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class(t_cfs_matrix), intent(in) :: this
      type(t_vector), intent(in) :: xvec
      type(t_vector), intent(inout) :: bvec
      integer :: i, j
      
      integer :: ii
      bvec%val(1:bvec%get_dim()) = zero
      do ii = 1, this%dim
         i = this%irow(ii)
         j = this%jcol(ii)
         bvec%val(i) = bvec%val(i) + this%val(ii) * xvec%val(j)
      enddo
     
      end subroutine prod_vec

!     ********************     !
!     Get Matrix dimension     !
!     ********************     !
      
      function get_dim(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class(t_cfs_matrix), intent(in) :: this
      integer :: get_dim
      get_dim = this%dim
      end function get_dim

!     ***************     !
!     Allocate arrays     !
!     ***************     !

      subroutine matrix_create(this, nn)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class(t_cfs_matrix), intent(inout) :: this
      integer, intent(in) :: nn
      this%dim = nn
      allocate(this%irow(nn), this%jcol(nn), this%val(nn))
      end subroutine matrix_create

!     *********************     !
!     Free allocated memory     !
!     *********************     !

      subroutine matrix_destroy(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class(t_cfs_matrix), intent(inout) :: this
      if (allocated(this%irow)) deallocate(this%irow)
      if (allocated(this%jcol)) deallocate(this%jcol)
      if (allocated(this%val)) deallocate(this%val)
      end subroutine matrix_destroy

!     *******************     !
!     Pointer association     !
!     *******************     !

      subroutine matrix_associate(this, ptr_irow, ptr_jcol, ptr_val)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class(t_cfs_matrix), intent(inout), target :: this
      integer, dimension(:), pointer :: ptr_irow, ptr_jcol
      double precision, dimension(:), pointer :: ptr_val
      ptr_irow => this%irow 
      ptr_jcol => this%jcol
      ptr_val => this%val
      end subroutine matrix_associate

!     **********     !
!     End module     !
!     **********     !

      END MODULE MATRIX_MOD
