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
!||    s6zjac_i_mod   ../engine/source/elements/solid/solide6z/s6zjac_i.F90
!||--- called by ------------------------------------------------------
!||    s6zupd11t12    ../engine/source/elements/solid/solide6z/s6zupd11t12.F90
!||====================================================================
      module s6zjac_i_mod
      contains

!||====================================================================
!||    s6zjac_i        ../engine/source/elements/solid/solide6z/s6zjac_i.F90
!||--- called by ------------------------------------------------------
!||    s6zupd11t12     ../engine/source/elements/solid/solide6z/s6zupd11t12.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    message_mod     ../engine/share/message_module/message_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine s6zjac_i(&
                   xd1  ,xd2  ,xd3  ,xd4  ,xd5  ,xd6 ,&
                   yd1  ,yd2  ,yd3  ,yd4  ,yd5  ,yd6 ,&
                   zd1  ,zd2  ,zd3  ,zd4  ,zd5  ,zd6 ,&
                   jac_i,nel)
!c-----------------------------------------------
!c   m o d u l e s
!c-----------------------------------------------
      use message_mod
      use precision_mod, only : wp
      use constant_mod , only :third, one_over_8


      implicit none
!c-----------------------------------------------
!c   d u m m y   a r g u m e n t s
!c-----------------------------------------------
      integer , intent(in) :: nel

      real(kind=wp),  dimension(10,nel),intent(out) :: jac_i     

     real(kind=8), dimension(nel),       intent(in)   :: xd1       !< local x-coordinates node 1 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: xd2       !< local x-coordinates node 2 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: xd3       !< local x-coordinates node 3 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: xd4       !< local x-coordinates node 4 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: xd5       !< local x-coordinates node 5 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: xd6       !< local x-coordinates node 6 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: yd1       !< local y-coordinates node 1 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: yd2       !< local y-coordinates node 2 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: yd3       !< local y-coordinates node 3 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: yd4       !< local y-coordinates node 4 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: yd5       !< local y-coordinates node 5 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: yd6       !< local y-coordinates node 6 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: zd1       !< local z-coordinates node 1 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: zd2       !< local z-coordinates node 2 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: zd3       !< local z-coordinates node 3 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: zd4       !< local z-coordinates node 4 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: zd5       !< local z-coordinates node 5 in double precision
     real(kind=8), dimension(nel),       intent(in)   :: zd6       !< local z-coordinates node 6 in double precision




!c-----------------------------------------------
!c   l o c a l   v a r i a b l e s
!c-----------------------------------------------
      integer :: i
!c
      real(kind=wp), dimension(nel) :: x21, x31, x41, x54, x64
      real(kind=wp), dimension(nel) :: y21, y31, y41, y54, y64
      real(kind=wp), dimension(nel) :: z21, z31, z41, z54, z64
      real(kind=wp), dimension(nel) :: jac1, jac2, jac3
      real(kind=wp), dimension(nel) :: jac4, jac5, jac6
      real(kind=wp), dimension(nel) :: jac7, jac8, jac9
      real(kind=wp), dimension(nel) :: jac_59_68, jac_67_49, jac_48_57
      real(kind=wp), dimension(nel) :: det, dett
!c
!c=======================================================================
  
      do i=1,nel
      x21(i)=xd2(i)-xd1(i)
      x31(i)=xd3(i)-xd1(i)
      x41(i)=xd4(i)-xd1(i)
      x54(i)=xd5(i)-xd4(i)
      x64(i)=xd6(i)-xd4(i)
!c
      y21(i)=yd2(i)-yd1(i)
      y31(i)=yd3(i)-yd1(i)
      y41(i)=yd4(i)-yd1(i)
      y54(i)=yd5(i)-yd4(i)
      y64(i)=yd6(i)-yd4(i)
!c
      z21(i)=zd2(i)-zd1(i)
      z31(i)=zd3(i)-zd1(i)
      z41(i)=zd4(i)-zd1(i)
      z54(i)=zd5(i)-zd4(i)
      z64(i)=zd6(i)-zd4(i)
      enddo
!c
!c jacobian matrix
      do i=1,nel
!c-------ri.xi---->ksi--------
       jac1(i)=x21(i)+x54(i)
       jac2(i)=y21(i)+y54(i)
       jac3(i)=z21(i)+z54(i)
!c-------si.xi--->eta--------
      jac4(i)=x31(i)+x64(i)
      jac5(i)=y31(i)+y64(i)
      jac6(i)=z31(i)+z64(i)
!c-------ti.xi----zeta-------
      jac7(i)=third*(x41(i)+xd5(i)-xd2(i)+xd6(i)-xd3(i))
      jac8(i)=third*(y41(i)+yd5(i)-yd2(i)+yd6(i)-yd3(i))
      jac9(i)=third*(z41(i)+zd5(i)-zd2(i)+zd6(i)-zd3(i))

      enddo

      do i=1,nel
      jac_59_68(i)=jac5(i)*jac9(i)-jac6(i)*jac8(i)
      jac_67_49(i)=jac6(i)*jac7(i)-jac4(i)*jac9(i)
      jac_48_57(i)=jac4(i)*jac8(i)-jac5(i)*jac7(i)
      enddo
!c
      do i=1,nel
       det(i) =one_over_8*(jac1(i)*jac_59_68(i)+jac2(i)*jac_67_49(i)+jac3(i)*jac_48_57(i))
       dett(i) = one_over_8 / det(i)
      enddo     
!c
!c jacobian matrix inverse
      do i=1,nel
      jac_i(1,i)=dett(i)*jac_59_68(i)
      jac_i(4,i)=dett(i)*jac_67_49(i)
      jac_i(7,i)=dett(i)*jac_48_57(i)
      jac_i(2,i)=dett(i)*(-jac2(i)*jac9(i)+jac3(i)*jac8(i))
      jac_i(5,i)=dett(i)*( jac1(i)*jac9(i)-jac3(i)*jac7(i))
      jac_i(8,i)=dett(i)*(-jac1(i)*jac8(i)+jac2(i)*jac7(i))
      jac_i(3,i)=dett(i)*( jac2(i)*jac6(i)-jac3(i)*jac5(i))
      jac_i(6,i)=dett(i)*(-jac1(i)*jac6(i)+jac3(i)*jac4(i))
      jac_i(9,i)=dett(i)*( jac1(i)*jac5(i)-jac2(i)*jac4(i))
     
      enddo
!c
      do i=1,nel
      jac_i(10,i) = det(i)
      enddo
!c
      
      end subroutine s6zjac_i
      end module s6zjac_i_mod
