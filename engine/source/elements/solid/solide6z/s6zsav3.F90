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
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    s6zsav3_mod   ../engine/source/elements/solid/solide6z/s6zsav3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3      ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zsav3_mod
      contains
!||====================================================================
!||    s6zsav3         ../engine/source/elements/solid/solide6z/s6zsav3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine s6zsav3(                    &
        offg,    sav,     xd1,     xd2,      &
        xd3,     xd4,     xd5,     xd6,      &
        yd1,     yd2,     yd3,     yd4,      &
        yd5,     yd6,     zd1,     zd2,      &
        zd3,     zd4,     zd5,     zd6,      &
        nel)
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
       use precision_mod, only : wp
       use constant_mod, only : one
!-------------------------------------------------------------------------------
!    I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,                      intent(in) :: nel          !< number of elements
      real(kind=wp), dimension(nel), intent(in):: offg         !< global element flag
!C     ENSURE DOUBLE-PRECISION (64-BIT) FLOATING-POINT CALCULATIONS, EVEN WHEN COMPILING IN SINGLE-PRECISION MODE.        
      real(kind=8), dimension(nel), intent(in) :: xd1          !< x coordinate of node 1
      real(kind=8), dimension(nel), intent(in) :: xd2          !< x coordinate of node 2
      real(kind=8), dimension(nel), intent(in) :: xd3          !< x coordinate of node 3
      real(kind=8), dimension(nel), intent(in) :: xd4          !< x coordinate of node 4
      real(kind=8), dimension(nel), intent(in) :: xd5          !< x coordinate of node 5
      real(kind=8), dimension(nel), intent(in) :: xd6          !< x coordinate of node 6
      real(kind=8), dimension(nel), intent(in) :: yd1          !< y coordinate of node 1
      real(kind=8), dimension(nel), intent(in) :: yd2          !< y coordinate of node 2
      real(kind=8), dimension(nel), intent(in) :: yd3          !< y coordinate of node 3
      real(kind=8), dimension(nel), intent(in) :: yd4          !< y coordinate of node 4
      real(kind=8), dimension(nel), intent(in) :: yd5          !< y coordinate of node 5
      real(kind=8), dimension(nel), intent(in) :: yd6          !< y coordinate of node 6
      real(kind=8), dimension(nel), intent(in) :: zd1          !< z coordinate of node 1
      real(kind=8), dimension(nel), intent(in) :: zd2          !< z coordinate of node 2
      real(kind=8), dimension(nel), intent(in) :: zd3          !< z coordinate of node 3
      real(kind=8), dimension(nel), intent(in) :: zd4          !< z coordinate of node 4
      real(kind=8), dimension(nel), intent(in) :: zd5          !< z coordinate of node 5
      real(kind=8), dimension(nel), intent(in) :: zd6          !< z coordinate of node 6
      real(kind=8),              intent(inout) :: sav(nel,15) 
   
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer i      
!=======================================================================
        do i=1,nel
          if(abs(offg(i)) <= one )then
           sav(i,1)=xd1(i)-xd6(i)
           sav(i,2)=yd1(i)-yd6(i)
           sav(i,3)=zd1(i)-zd6(i)
           sav(i,4)=xd2(i)-xd6(i)
           sav(i,5)=yd2(i)-yd6(i)
           sav(i,6)=zd2(i)-zd6(i)
           sav(i,7)=xd3(i)-xd6(i)
           sav(i,8)=yd3(i)-yd6(i)
           sav(i,9)=zd3(i)-zd6(i)
           sav(i,10)=xd4(i)-xd6(i)
           sav(i,11)=yd4(i)-yd6(i)
           sav(i,12)=zd4(i)-zd6(i)
           sav(i,13)=xd5(i)-xd6(i)
           sav(i,14)=yd5(i)-yd6(i)
           sav(i,15)=zd5(i)-zd6(i)
          endif
        enddo
!-----------------------------------------------------------------------

      end subroutine s6zsav3
      end module s6zsav3_mod