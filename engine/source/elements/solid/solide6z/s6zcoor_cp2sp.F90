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
!||    s6zcoor_cp2sp_mod   ../engine/source/elements/solid/solide6z/s6zcoor_cp2sp.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3            ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zcoor_cp2sp_mod
      contains
!||====================================================================
!||    s6zcoor_cp2sp   ../engine/source/elements/solid/solide6z/s6zcoor_cp2sp.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- uses       -----------------------------------------------------
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine s6zcoor_cp2sp(                       &
          x0       ,y0       ,z0       ,x1       ,      &
          x2       ,x3       ,x4       ,x5       ,      &
          x6       ,y1       ,y2       ,y3       ,      &
          y4       ,y5       ,y6       ,z1       ,      &
          z2       ,z3       ,z4       ,z5       ,      &
          z6       ,nel)

!-------------------------------------------------------------------------------
!   m o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod, only : wp
          use mvsiz_mod       ,only : mvsiz

!-------------------------------------------------------------------------------
!   i m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none

!-------------------------------------------------------------------------------
!   d u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
          integer,                          intent(in)    :: nel      !< number of elements

          ! double precision coordinates (input)
          !c     ensure double-precision (64-bit) floating-point calculations
          real(kind=8),  dimension(mvsiz,6), intent(in)    :: x0       !< original x coordinates (double precision)
          real(kind=8),  dimension(mvsiz,6), intent(in)    :: y0       !< original y coordinates (double precision)
          real(kind=8),  dimension(mvsiz,6), intent(in)    :: z0       !< original z coordinates (double precision)

          ! single/working precision coordinates (output)
          real(kind=wp), dimension(nel),     intent(out)   :: x1       !< x coordinate of node 1
          real(kind=wp), dimension(nel),     intent(out)   :: x2       !< x coordinate of node 2
          real(kind=wp), dimension(nel),     intent(out)   :: x3       !< x coordinate of node 3
          real(kind=wp), dimension(nel),     intent(out)   :: x4       !< x coordinate of node 4
          real(kind=wp), dimension(nel),     intent(out)   :: x5       !< x coordinate of node 5
          real(kind=wp), dimension(nel),     intent(out)   :: x6       !< x coordinate of node 6

          real(kind=wp), dimension(nel),     intent(out)   :: y1       !< y coordinate of node 1
          real(kind=wp), dimension(nel),     intent(out)   :: y2       !< y coordinate of node 2
          real(kind=wp), dimension(nel),     intent(out)   :: y3       !< y coordinate of node 3
          real(kind=wp), dimension(nel),     intent(out)   :: y4       !< y coordinate of node 4
          real(kind=wp), dimension(nel),     intent(out)   :: y5       !< y coordinate of node 5
          real(kind=wp), dimension(nel),     intent(out)   :: y6       !< y coordinate of node 6

          real(kind=wp), dimension(nel),     intent(out)   :: z1       !< z coordinate of node 1
          real(kind=wp), dimension(nel),     intent(out)   :: z2       !< z coordinate of node 2
          real(kind=wp), dimension(nel),     intent(out)   :: z3       !< z coordinate of node 3
          real(kind=wp), dimension(nel),     intent(out)   :: z4       !< z coordinate of node 4
          real(kind=wp), dimension(nel),     intent(out)   :: z5       !< z coordinate of node 5
          real(kind=wp), dimension(nel),     intent(out)   :: z6       !< z coordinate of node 6

!-------------------------------------------------------------------------------
!   l o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i

!===============================================================================
!   b o d y
!===============================================================================
          do i = 1, nel
            x1(i) = x0(i,1)
            y1(i) = y0(i,1)
            z1(i) = z0(i,1)

            x2(i) = x0(i,2)
            y2(i) = y0(i,2)
            z2(i) = z0(i,2)

            x3(i) = x0(i,3)
            y3(i) = y0(i,3)
            z3(i) = z0(i,3)

            x4(i) = x0(i,4)
            y4(i) = y0(i,4)
            z4(i) = z0(i,4)

            x5(i) = x0(i,5)
            y5(i) = y0(i,5)
            z5(i) = z0(i,5)

            x6(i) = x0(i,6)
            y6(i) = y0(i,6)
            z6(i) = z0(i,6)
          end do

        end subroutine s6zcoor_cp2sp

      end module s6zcoor_cp2sp_mod