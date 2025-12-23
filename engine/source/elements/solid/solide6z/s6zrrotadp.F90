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
!||    s6zrrotadp_mod   ../engine/source/elements/solid/solide/s6zrrotadp.f90
!||--- called by ------------------------------------------------------
!||    s8eforc3      ../engine/source/elements/solid/solide8e/s8eforc3.f
!||    s8zforc3      ../engine/source/elements/solid/solide8z/s8zforc3.f
!||    sforc3        ../engine/source/elements/solid/solide/sforc3.f
!||    szforc3       ../engine/source/elements/solid/solidez/szforc3.f
!||====================================================================
      module s6zrrotadp_mod
      contains
!||====================================================================
!||
!||    s6zrrotadp
!||
!||--- called by ------------------------------------------------------
!||
!||    s8eforc3
!||    s8zforc3
!||    sforc3
!||    szforc3
!||
!||--- uses       -----------------------------------------------------
!||
!||    precision_mod   ../common_source/modules/precision_mod.f90
!||====================================================================
        subroutine s6zrrotadp( &
          r11      ,r12      ,r13      ,r21      ,      &
          r22      ,r23      ,r31      ,r32      ,      &
          r33      ,x1       ,x2       ,x3       ,      &
          x4       ,x5       ,x6       ,y1       ,      &
          y2       ,y3       ,y4       ,y5       ,      &
          y6       ,z1       ,z2       ,z3       ,      &
          z4       ,z5       ,z6       ,nel)

!-------------------------------------------------------------------------------
!   m o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod, only : wp

!-------------------------------------------------------------------------------
!   i m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none

!-------------------------------------------------------------------------------
!   d u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
          ! rotation matrix (input, working precision)
          real(kind=wp), dimension(nel),    intent(in)    :: r11       !< rotation matrix component 11
          real(kind=wp), dimension(nel),    intent(in)    :: r12       !< rotation matrix component 12
          real(kind=wp), dimension(nel),    intent(in)    :: r13       !< rotation matrix component 13
          real(kind=wp), dimension(nel),    intent(in)    :: r21       !< rotation matrix component 21
          real(kind=wp), dimension(nel),    intent(in)    :: r22       !< rotation matrix component 22
          real(kind=wp), dimension(nel),    intent(in)    :: r23       !< rotation matrix component 23
          real(kind=wp), dimension(nel),    intent(in)    :: r31       !< rotation matrix component 31
          real(kind=wp), dimension(nel),    intent(in)    :: r32       !< rotation matrix component 32
          real(kind=wp), dimension(nel),    intent(in)    :: r33       !< rotation matrix component 33

          ! coordinates (input/output, double precision)
          real(kind=8),  dimension(nel),    intent(inout) :: x1        !< x coordinate node 1
          real(kind=8),  dimension(nel),    intent(inout) :: x2        !< x coordinate node 2
          real(kind=8),  dimension(nel),    intent(inout) :: x3        !< x coordinate node 3
          real(kind=8),  dimension(nel),    intent(inout) :: x4        !< x coordinate node 4
          real(kind=8),  dimension(nel),    intent(inout) :: x5        !< x coordinate node 5
          real(kind=8),  dimension(nel),    intent(inout) :: x6        !< x coordinate node 6

          real(kind=8),  dimension(nel),    intent(inout) :: y1        !< y coordinate node 1
          real(kind=8),  dimension(nel),    intent(inout) :: y2        !< y coordinate node 2
          real(kind=8),  dimension(nel),    intent(inout) :: y3        !< y coordinate node 3
          real(kind=8),  dimension(nel),    intent(inout) :: y4        !< y coordinate node 4
          real(kind=8),  dimension(nel),    intent(inout) :: y5        !< y coordinate node 5
          real(kind=8),  dimension(nel),    intent(inout) :: y6        !< y coordinate node 6

          real(kind=8),  dimension(nel),    intent(inout) :: z1        !< z coordinate node 1
          real(kind=8),  dimension(nel),    intent(inout) :: z2        !< z coordinate node 2
          real(kind=8),  dimension(nel),    intent(inout) :: z3        !< z coordinate node 3
          real(kind=8),  dimension(nel),    intent(inout) :: z4        !< z coordinate node 4
          real(kind=8),  dimension(nel),    intent(inout) :: z5        !< z coordinate node 5
          real(kind=8),  dimension(nel),    intent(inout) :: z6        !< z coordinate node 6

          integer,                          intent(in)    :: nel       !< number of elements

!-------------------------------------------------------------------------------
!   l o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
          integer       :: i
          real(kind=wp) :: x, y, z

!===============================================================================
!   b o d y
!===============================================================================

          do i = 1, nel
            ! node 1
            x = r11(i)*x1(i) + r21(i)*y1(i) + r31(i)*z1(i)
            y = r12(i)*x1(i) + r22(i)*y1(i) + r32(i)*z1(i)
            z = r13(i)*x1(i) + r23(i)*y1(i) + r33(i)*z1(i)
            x1(i) = x
            y1(i) = y
            z1(i) = z

            ! node 2
            x = r11(i)*x2(i) + r21(i)*y2(i) + r31(i)*z2(i)
            y = r12(i)*x2(i) + r22(i)*y2(i) + r32(i)*z2(i)
            z = r13(i)*x2(i) + r23(i)*y2(i) + r33(i)*z2(i)
            x2(i) = x
            y2(i) = y
            z2(i) = z

            ! node 3
            x = r11(i)*x3(i) + r21(i)*y3(i) + r31(i)*z3(i)
            y = r12(i)*x3(i) + r22(i)*y3(i) + r32(i)*z3(i)
            z = r13(i)*x3(i) + r23(i)*y3(i) + r33(i)*z3(i)
            x3(i) = x
            y3(i) = y
            z3(i) = z

            ! node 4
            x = r11(i)*x4(i) + r21(i)*y4(i) + r31(i)*z4(i)
            y = r12(i)*x4(i) + r22(i)*y4(i) + r32(i)*z4(i)
            z = r13(i)*x4(i) + r23(i)*y4(i) + r33(i)*z4(i)
            x4(i) = x
            y4(i) = y
            z4(i) = z

            ! node 5
            x = r11(i)*x5(i) + r21(i)*y5(i) + r31(i)*z5(i)
            y = r12(i)*x5(i) + r22(i)*y5(i) + r32(i)*z5(i)
            z = r13(i)*x5(i) + r23(i)*y5(i) + r33(i)*z5(i)
            x5(i) = x
            y5(i) = y
            z5(i) = z

            ! node 6
            x = r11(i)*x6(i) + r21(i)*y6(i) + r31(i)*z6(i)
            y = r12(i)*x6(i) + r22(i)*y6(i) + r32(i)*z6(i)
            z = r13(i)*x6(i) + r23(i)*y6(i) + r33(i)*z6(i)
            x6(i) = x
            y6(i) = y
            z6(i) = z
          end do

        end subroutine s6zrrotadp

      end module s6zrrotadp_mod