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
!||    s6zrmalla11_mod   ../engine/source/elements/solid/solide6z/s6zrmalla11.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3          ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zrmalla11_mod
      contains
!||====================================================================
!||    s6zrmalla11     ../engine/source/elements/solid/solide6z/s6zrmalla11.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine s6zrmalla11( &
          sav      ,offg     ,wxx      ,wyy      ,      &
          wzz      ,r11      ,r12      ,r13      ,      &
          r21      ,r22      ,r23      ,r31      ,      &
          r32      ,r33      ,nel      ,ismstr)

!-------------------------------------------------------------------------------
!   m o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod, only : wp
          use constant_mod,  only : zero, one

!-------------------------------------------------------------------------------
!   i m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none

!-------------------------------------------------------------------------------
!   d u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
          integer,                          intent(in)    :: ismstr    !< small strain formulation flag
          integer,                          intent(in)    :: nel       !< number of elements

          real(kind=8),  dimension(nel,15), intent(inout) :: sav       !< saved variables (double precision)

          real(kind=wp), dimension(nel),    intent(in)    :: offg      !< global activation flag
          
          real(kind=wp), dimension(nel),    intent(inout) :: wxx       !< spin vector x
          real(kind=wp), dimension(nel),    intent(inout) :: wyy       !< spin vector y
          real(kind=wp), dimension(nel),    intent(inout) :: wzz       !< spin vector z

          real(kind=wp), dimension(nel),    intent(in)    :: r11       !< rotation matrix 11
          real(kind=wp), dimension(nel),    intent(in)    :: r12       !< rotation matrix 12
          real(kind=wp), dimension(nel),    intent(in)    :: r13       !< rotation matrix 13
          real(kind=wp), dimension(nel),    intent(in)    :: r21       !< rotation matrix 21
          real(kind=wp), dimension(nel),    intent(in)    :: r22       !< rotation matrix 22
          real(kind=wp), dimension(nel),    intent(in)    :: r23       !< rotation matrix 23
          real(kind=wp), dimension(nel),    intent(in)    :: r31       !< rotation matrix 31
          real(kind=wp), dimension(nel),    intent(in)    :: r32       !< rotation matrix 32
          real(kind=wp), dimension(nel),    intent(in)    :: r33       !< rotation matrix 33

!-------------------------------------------------------------------------------
!   l o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i
          real(kind=8) :: x, y, z
          real(kind=8) :: wxxg, wyyg, wzzg

!===============================================================================
!   b o d y
!===============================================================================
!-----------------------------
!      rotation rby coordinates(ref) for small strain
!-----------------------------

          if (ismstr == 11) then
            !------------wxx,y,z from local system to global 
            do i = 1, nel
              wxxg = r11(i)*wxx(i) + r12(i)*wyy(i) + r13(i)*wzz(i)
              wyyg = r21(i)*wxx(i) + r22(i)*wyy(i) + r23(i)*wzz(i)
              wzzg = r31(i)*wxx(i) + r32(i)*wyy(i) + r33(i)*wzz(i)
              
              wxx(i) = wxxg
              wyy(i) = wyyg
              wzz(i) = wzzg
            end do

            do i = 1, nel
              if (offg(i) == zero) cycle

              x = sav(i,1)
              y = sav(i,6)
              z = sav(i,11)
              sav(i,1)  = x - y*wzz(i) + z*wyy(i)
              sav(i,6)  = y - z*wxx(i) + x*wzz(i)
              sav(i,11) = z - x*wyy(i) + y*wxx(i) 
 
              x = sav(i,2)
              y = sav(i,7)
              z = sav(i,12)
              sav(i,2)  = x - y*wzz(i) + z*wyy(i)
              sav(i,7)  = y - z*wxx(i) + x*wzz(i)
              sav(i,12) = z - x*wyy(i) + y*wxx(i) 
 
              x = sav(i,3)
              y = sav(i,8)
              z = sav(i,13)
              sav(i,3)  = x - y*wzz(i) + z*wyy(i)
              sav(i,8)  = y - z*wxx(i) + x*wzz(i)
              sav(i,13) = z - x*wyy(i) + y*wxx(i) 
 
              x = sav(i,4)
              y = sav(i,9)
              z = sav(i,14)
              sav(i,4)  = x - y*wzz(i) + z*wyy(i)
              sav(i,9)  = y - z*wxx(i) + x*wzz(i)
              sav(i,14) = z - x*wyy(i) + y*wxx(i) 
 
              x = sav(i,5)
              y = sav(i,10)
              z = sav(i,15)
              sav(i,5)  = x - y*wzz(i) + z*wyy(i)
              sav(i,10) = y - z*wxx(i) + x*wzz(i)
              sav(i,15) = z - x*wyy(i) + y*wxx(i) 
            end do

          elseif (ismstr == 12) then
            !------------wxx,y,z from local system to global 
            do i = 1, nel
              if (offg(i) <= one) cycle
              
              wxxg = r11(i)*wxx(i) + r12(i)*wyy(i) + r13(i)*wzz(i)
              wyyg = r21(i)*wxx(i) + r22(i)*wyy(i) + r23(i)*wzz(i)
              wzzg = r31(i)*wxx(i) + r32(i)*wyy(i) + r33(i)*wzz(i)
              
              wxx(i) = wxxg
              wyy(i) = wyyg
              wzz(i) = wzzg
            end do

            do i = 1, nel
              if (offg(i) <= one) cycle

              x = sav(i,1)
              y = sav(i,2)
              z = sav(i,3)
              sav(i,1) = x - y*wzz(i) + z*wyy(i)
              sav(i,2) = y - z*wxx(i) + x*wzz(i)
              sav(i,3) = z - x*wyy(i) + y*wxx(i) 
 
              x = sav(i,4)
              y = sav(i,5)
              z = sav(i,6)
              sav(i,4) = x - y*wzz(i) + z*wyy(i)
              sav(i,5) = y - z*wxx(i) + x*wzz(i)
              sav(i,6) = z - x*wyy(i) + y*wxx(i) 
 
              x = sav(i,7)
              y = sav(i,8)
              z = sav(i,9)
              sav(i,7) = x - y*wzz(i) + z*wyy(i)
              sav(i,8) = y - z*wxx(i) + x*wzz(i)
              sav(i,9) = z - x*wyy(i) + y*wxx(i) 
 
              x = sav(i,10)
              y = sav(i,11)
              z = sav(i,12)
              sav(i,10) = x - y*wzz(i) + z*wyy(i)
              sav(i,11) = y - z*wxx(i) + x*wzz(i)
              sav(i,12) = z - x*wyy(i) + y*wxx(i) 
 
              x = sav(i,13)
              y = sav(i,14)
              z = sav(i,15)
              sav(i,13) = x - y*wzz(i) + z*wyy(i)
              sav(i,14) = y - z*wxx(i) + x*wzz(i)
              sav(i,15) = z - x*wyy(i) + y*wxx(i) 
            end do
          end if

        end subroutine s6zrmalla11

      end module s6zrmalla11_mod