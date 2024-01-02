Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  VECTOR_MOD                    linearalgebra/vector_mod.F    
Chd|-- called by -----------
Chd|        MATRIX_MOD                    common_source/linearalgebra/matrix_mod.F
Chd|        SET_RHS                       engine/share/modules/linear_solver_mod.F
Chd|        SET_RHS_CG                    engine/share/modules/linear_solver_mod.F
Chd|        SET_RHS_MUMPS                 engine/share/modules/linear_solver_mod.F
Chd|        SOLVE_CG                      engine/share/modules/linear_solver_mod.F
Chd|        DIFFUSION_MOD                 engine/share/modules/diffusion_mod.F
Chd|        LINEAR_SOLVER_MOD             engine/share/modules/linear_solver_mod.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE VECTOR_MOD
      implicit none
#include      "my_real.inc"

!     ******     !
!     Vector     !
!     ******     !
      
      type :: t_vector
      integer, private :: dim
      integer, dimension(:), allocatable :: irow
      double precision, dimension(:), allocatable :: val
      contains
      procedure, pass :: create
      procedure, pass :: destroy
      procedure, pass :: associate
      procedure, pass :: get_dim
      procedure, pass :: set_dim
      procedure, pass :: norm
      end type t_vector

      contains
      
!     ********************     !
!     Get Vector dimension     !
!     ********************     !

      function get_dim(this)
      class(t_vector), intent(in) :: this
      integer :: get_dim
      get_dim = this%dim
      end function get_dim

!     ********************     !
!     Set Vector dimension     !
!     ********************     !

      subroutine set_dim(this, dim)
      class(t_vector), intent(inout) :: this
      integer :: dim
      this%dim = dim
      end subroutine set_dim

!     ***************     !
!     Allocate arrays     !
!     ***************     !

      subroutine create(this, nn)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class(t_vector), intent(inout) :: this
      integer, intent(in) :: nn
      this%dim = nn
      allocate(this%irow(nn), this%val(nn))
      end subroutine create

!     *********************     !
!     Free allocated memory     !
!     *********************     !

      subroutine destroy(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class(t_vector), intent(inout) :: this
      if (allocated(this%irow)) deallocate(this%irow)
      if (allocated(this%val)) deallocate(this%val)
      end subroutine destroy
      
!     *******************     !
!     Pointer association     !
!     *******************     !

      subroutine associate(this, ptr_irow, ptr_val)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class(t_vector), intent(inout), target :: this
      integer, dimension(:), pointer :: ptr_irow
      double precision, dimension(:), pointer :: ptr_val
      ptr_irow => this%irow 
      ptr_val => this%val
      end subroutine associate

!     *******************     !
!     Compute Vector norm     !
!     *******************     !

      function norm(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      class(t_vector), intent(in) :: this
      my_real :: norm
      integer :: ii
      norm = zero
      do ii = 1, this%dim
         norm = norm + abs(this%val(ii))
      enddo
      end function norm

!     **********     !
!     End module     !
!     **********     !

      END MODULE VECTOR_MOD
