!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    s6zrrota3_mod   ../engine/source/elements/solid/solide6z/s6zrrota3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zrrota3_mod
            contains
!||====================================================================
!||    s6zrrota3       ../engine/source/elements/solid/solide6z/s6zrrota3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- uses       -----------------------------------------------------
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
            subroutine s6zrrota3(r11, r12, r13, r21, r22, r23, r31, r32, r33, &
                               x1, x2, x3, x4, x5, x6, &
                               y1, y2, y3, y4, y5, y6, &
                               z1, z2, z3, z4, z5, z6, nel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
      use precision_mod, only : wp
      implicit none
!C-----------------------------------------------
!C   I m p l i c i t   T y p e s
!C-----------------------------------------------
!#include      "implicit_f.inc"
!C-----------------------------------------------
!C   D u m m y   A r g u m e n t s
!C-----------------------------------------------
      integer, intent(in) :: nel
      real(kind=WP), dimension(nel), intent(in) :: r11
      real(kind=WP), dimension(nel), intent(in) :: r12
      real(kind=WP), dimension(nel), intent(in) :: r13
      real(kind=WP), dimension(nel), intent(in) :: r21
      real(kind=WP), dimension(nel), intent(in) :: r22
      real(kind=WP), dimension(nel), intent(in) :: r23
      real(kind=WP), dimension(nel), intent(in) :: r31
      real(kind=WP), dimension(nel), intent(in) :: r32
      real(kind=WP), dimension(nel), intent(in) :: r33
      real(kind=WP), dimension(nel), intent(inout) :: x1
      real(kind=WP), dimension(nel), intent(inout) :: x2
      real(kind=WP), dimension(nel), intent(inout) :: x3
      real(kind=WP), dimension(nel), intent(inout) :: x4
      real(kind=WP), dimension(nel), intent(inout) :: x5
      real(kind=WP), dimension(nel), intent(inout) :: x6
      real(kind=WP), dimension(nel), intent(inout) :: y1
      real(kind=WP), dimension(nel), intent(inout) :: y2
      real(kind=WP), dimension(nel), intent(inout) :: y3
      real(kind=WP), dimension(nel), intent(inout) :: y4
      real(kind=WP), dimension(nel), intent(inout) :: y5
      real(kind=WP), dimension(nel), intent(inout) :: y6
      real(kind=WP), dimension(nel), intent(inout) :: z1
      real(kind=WP), dimension(nel), intent(inout) :: z2
      real(kind=WP), dimension(nel), intent(inout) :: z3
      real(kind=WP), dimension(nel), intent(inout) :: z4
      real(kind=WP), dimension(nel), intent(inout) :: z5
      real(kind=WP), dimension(nel), intent(inout) :: z6
!C-----------------------------------------------
!C   C o m m o n   B l o c k s
!C-----------------------------------------------
!C-----------------------------------------------
!C   L o c a l   V a r i a b l e s
! -----------------------------------------------
      integer :: i                     ! Loop counter
      real(kind=WP) :: x, y, z         ! Temporary variables for coordinate transformations
!C-----------------------------------------------
! Perform coordinate transformations for each element
      do i = 1, nel
            x = r11(i) * x1(i) + r21(i) * y1(i) + r31(i) * z1(i)
            y = r12(i) * x1(i) + r22(i) * y1(i) + r32(i) * z1(i)
            z = r13(i) * x1(i) + r23(i) * y1(i) + r33(i) * z1(i)
            x1(i) = x
            y1(i) = y
            z1(i) = z      

            x = r11(i) * x2(i) + r21(i) * y2(i) + r31(i) * z2(i)
            y = r12(i) * x2(i) + r22(i) * y2(i) + r32(i) * z2(i)
            z = r13(i) * x2(i) + r23(i) * y2(i) + r33(i) * z2(i)
            x2(i) = x
            y2(i) = y
            z2(i) = z      

            x = r11(i) * x3(i) + r21(i) * y3(i) + r31(i) * z3(i)
            y = r12(i) * x3(i) + r22(i) * y3(i) + r32(i) * z3(i)
            z = r13(i) * x3(i) + r23(i) * y3(i) + r33(i) * z3(i)
            x3(i) = x
            y3(i) = y
            z3(i) = z      

            x = r11(i) * x4(i) + r21(i) * y4(i) + r31(i) * z4(i)
            y = r12(i) * x4(i) + r22(i) * y4(i) + r32(i) * z4(i)
            z = r13(i) * x4(i) + r23(i) * y4(i) + r33(i) * z4(i)
            x4(i) = x
            y4(i) = y
            z4(i) = z      

            x = r11(i) * x5(i) + r21(i) * y5(i) + r31(i) * z5(i)
            y = r12(i) * x5(i) + r22(i) * y5(i) + r32(i) * z5(i)
            z = r13(i) * x5(i) + r23(i) * y5(i) + r33(i) * z5(i)
            x5(i) = x
            y5(i) = y
            z5(i) = z      

            x = r11(i) * x6(i) + r21(i) * y6(i) + r31(i) * z6(i)
            y = r12(i) * x6(i) + r22(i) * y6(i) + r32(i) * z6(i)
            z = r13(i) * x6(i) + r23(i) * y6(i) + r33(i) * z6(i)
            x6(i) = x
            y6(i) = y
            z6(i) = z
      end do      

      return
        end subroutine s6zrrota3
      end module s6zrrota3_mod