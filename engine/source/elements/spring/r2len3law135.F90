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
!||====================================================================
!||    r2len3law135_mod   ../engine/source/elements/spring/r2len3law135.F90
!||--- called by ------------------------------------------------------
!||    r23law135          ../engine/source/elements/spring/r23law135.F90
!||====================================================================
      module r2len3law135_mod
      contains
!||====================================================================
!||    r2len3law135       ../engine/source/elements/spring/r2len3law135.F90
!||--- called by ------------------------------------------------------
!||    r23law135          ../engine/source/elements/spring/r23law135.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    mat_elem_mod       ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mvsiz_mod          ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod      ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine r2len3law135( &
          nel      ,sti      ,stir     ,      &
          xkm      ,xkr      )

!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod,    only : wp
          use constant_mod,     only : zero, one, em15
          use mvsiz_mod,        only : mvsiz
          use matparam_def_mod
          use mat_elem_mod

!-------------------------------------------------------------------------------
!   I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none

!-------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-------------------------------------------------------------------------------
          integer,                          intent(in)    :: nel       !< Number of elements
          real(kind=wp), dimension(3,nel),  intent(out)   :: sti       !< Translational stiffness
          real(kind=wp), dimension(3,nel),  intent(out)   :: stir      !< Rotational stiffness
          real(kind=wp), dimension(nel),    intent(inout) :: xkm       !< Max translational stiffness
          real(kind=wp), dimension(nel),    intent(inout) :: xkr       !< Max rotational stiffness
!-------------------------------------------------------------------------------
!   L o c a l   V a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i


!===============================================================================
!   B o d y
!===============================================================================

          ! Initial stiffness assignment for both nodes
          do i = 1, nel
            ! Node 1
            sti(1,i)  = xkm(i)  
            stir(1,i) = xkr(i)
            ! Node 2        
            sti(2,i)  = sti(1,i) 
            stir(2,i) = stir(1,i)
          end do

        end subroutine r2len3law135
      end module r2len3law135_mod