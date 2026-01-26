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
!||    s6zortho3_mod   ../starter/source/elements/solid/solide6z/s6zortho3.F90
!||--- called by ------------------------------------------------------
!||    s6zrcoor3       ../starter/source/elements/solid/solide6z/s6zrcoor3.F90
!||====================================================================
      module s6zortho3_mod
      contains

!||====================================================================
!||    s6zortho3       ../starter/source/elements/solid/solide6z/s6zortho3.F90
!||--- called by ------------------------------------------------------
!||    s6zrcoor3       ../starter/source/elements/solid/solide6z/s6zrcoor3.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
      subroutine s6zortho3(&
        x1   ,x2   ,x3   ,x4   ,x5   ,x6   ,&
        y1   ,y2   ,y3   ,y4   ,y5   ,y6   ,&
        z1   ,z2   ,z3   ,z4   ,z5   ,z6   ,&
        rx   ,ry   ,rz   ,sx   ,sy   ,sz   ,tx   ,ty   ,tz   ,      &
        e1x  ,e1y  ,e1z  ,e2x  ,e2y  ,e2z  ,e3x  ,e3y  ,e3z  ,nel)
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
      use precision_mod, only : wp
      use constant_mod

      implicit none
!c-----------------------------------------------
!c   d u m m y   a r g u m e n t s
!c-----------------------------------------------
      integer, intent(in) :: nel      
!c     real
      real(kind=8), dimension(nel),       intent(in)   :: x1       !< local x-coordinates node 1 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: x2       !< local x-coordinates node 2 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: x3       !< local x-coordinates node 3 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: x4       !< local x-coordinates node 4 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: x5       !< local x-coordinates node 5 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: x6       !< local x-coordinates node 6 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: y1       !< local y-coordinates node 1 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: y2       !< local y-coordinates node 2 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: y3       !< local y-coordinates node 3 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: y4       !< local y-coordinates node 4 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: y5       !< local y-coordinates node 5 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: y6       !< local y-coordinates node 6 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: z1       !< local z-coordinates node 1 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: z2       !< local z-coordinates node 2 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: z3       !< local z-coordinates node 3 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: z4       !< local z-coordinates node 4 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: z5       !< local z-coordinates node 5 in double precision
      real(kind=8), dimension(nel),       intent(in)   :: z6       !< local z-coordinates node 6 in double precision

       real(kind=wp), dimension(nel),       intent(inout)   :: rx
       real(kind=wp), dimension(nel),       intent(inout)   :: ry
       real(kind=wp), dimension(nel),       intent(inout)   :: rz

       real(kind=wp), dimension(nel),       intent(inout)   :: sx
       real(kind=wp), dimension(nel),       intent(inout)   :: sy
       real(kind=wp), dimension(nel),       intent(inout)   :: sz

       real(kind=wp), dimension(nel),       intent(inout)   :: tx
       real(kind=wp), dimension(nel),       intent(inout)   :: ty
       real(kind=wp), dimension(nel),       intent(inout)   :: tz
 
       real(kind=wp), dimension(nel),       intent(inout)   :: e1x
       real(kind=wp), dimension(nel),       intent(inout)   :: e1y
       real(kind=wp), dimension(nel),       intent(inout)   :: e1z       

       real(kind=wp), dimension(nel),       intent(inout)   :: e2x
       real(kind=wp), dimension(nel),       intent(inout)   :: e2y
       real(kind=wp), dimension(nel),       intent(inout)   :: e2z 
       
       real(kind=wp), dimension(nel),       intent(inout)   :: e3x
       real(kind=wp), dimension(nel),       intent(inout)   :: e3y
       real(kind=wp), dimension(nel),       intent(inout)   :: e3z       


!c-----------------------------------------------
!c   l o c a l   v a r i a b l e s
!c-----------------------------------------------
      integer :: i
!c     real
      real(kind=8), dimension(nel)  ::  x14,y14,z14
    
      real(kind=wp)::  det,c1,c2 
 
!c-----------------------------------------------
      do i=1,nel
       x14(i)=x1(i)+x4(i)
       y14(i)=y1(i)+y4(i)
       z14(i)=z1(i)+z4(i)
      enddo

      do i=1,nel
       tx(i)=x2(i)+x5(i)-x14(i)
       ty(i)=y2(i)+y5(i)-y14(i)
       tz(i)=z2(i)+z5(i)-z14(i)
      enddo

      do i=1,nel
       rx(i)=x3(i)+x6(i)-x14(i)
       ry(i)=y3(i)+y6(i)-y14(i)
       rz(i)=z3(i)+z6(i)-z14(i)
      enddo

      do i=1,nel
        sx(i)= (x4(i)+x5(i)+x6(i)-x1(i)-x2(i)-x3(i))*third
        sy(i)= (y4(i)+y5(i)+y6(i)-y1(i)-y2(i)-y3(i))*third
        sz(i)= (z4(i)+z5(i)+z6(i)-z1(i)-z2(i)-z3(i))*third
      enddo
!c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      do i=1,nel
!c
       e3x(i) = ty(i) * rz(i) - tz(i) * ry(i) 
       e3y(i) = tz(i) * rx(i) - tx(i) * rz(i) 
       e3z(i) = tx(i) * ry(i) - ty(i) * rx(i) 
!c
       det = sqrt(e3x(i)*e3x(i) + e3y(i)*e3y(i) + e3z(i)*e3z(i))
       
       if ( det/=zero) det = one / det
       
       e3x(i) = e3x(i) * det
       e3y(i) = e3y(i) * det
       e3z(i) = e3z(i) * det
!c
       c1=sqrt(tx(i)*tx(i)+ty(i)*ty(i)+tz(i)*tz(i))
       c2=sqrt(rx(i)*rx(i)+ry(i)*ry(i)+rz(i)*rz(i))
       e1x(i)=tx(i)*c2 +(ry(i) * e3z(i) - rz(i) * e3y(i))*c1  
       e1y(i)=ty(i)*c2 +(rz(i) * e3x(i) - rx(i) * e3z(i))*c1  
       e1z(i)=tz(i)*c2 +(rx(i) * e3y(i) - ry(i) * e3x(i))*c1
       det = sqrt(e1x(i)*e1x(i) + e1y(i)*e1y(i) + e1z(i)*e1z(i))

      
       if ( det/=zero) det = one / det
       
       e1x(i) = e1x(i)*det
       e1y(i) = e1y(i)*det
       e1z(i) = e1z(i)*det
!c
       e2x(i) = e3y(i) * e1z(i) - e3z(i) * e1y(i)
       e2y(i) = e3z(i) * e1x(i) - e3x(i) * e1z(i)
       e2z(i) = e3x(i) * e1y(i) - e3y(i) * e1x(i)

      enddo
!c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      end subroutine s6zortho3
      end module s6zortho3_mod
