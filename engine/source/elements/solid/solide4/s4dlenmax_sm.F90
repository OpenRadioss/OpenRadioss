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
      module s4dlenmax_sm_mod
      implicit none
      contains
! ======================================================================================================================
! \brief compute some geometric parameters of tetrahedron in case of small strain
! ======================================================================================================================
        subroutine s4dlenmax_sm(                          &
          nel      ,x        ,l_min      ,vol     ,       &
          nc1      ,nc2      ,nc3        ,nc4     ,       &
          numnod )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : one_over_6,six,em20
          use precision_mod, only : WP
          use mvsiz_mod , only : mvsiz
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                    :: nel             !< number of elements
          integer, intent(in)                                    :: numnod          !< number of nodes
          integer, dimension(mvsiz), intent(in   )               :: nc1             !< n1_id
          integer, dimension(mvsiz), intent(in   )               :: nc2             !< n2_id
          integer, dimension(mvsiz), intent(in   )               :: nc3             !< n3_id
          integer, dimension(mvsiz), intent(in   )               :: nc4             !< n4_id
          real(kind=WP), dimension(mvsiz), intent(inout)         :: vol             !< volume
          real(kind=WP), dimension(mvsiz), intent(inout)         :: l_min           !< charactistic length
          real(kind=WP), dimension(3,numnod), intent(in)         :: x               !< coordinates
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
          real(kind=WP), dimension(mvsiz) ::                  &
            x1,      x2,     x3,     x4,                      &
            y1,      y2,     y3,     y4,                      &
            z1,      z2,     z3,     z4,                      &
            rx,      ry,     rz,                              &
            sx,      sy,     sz,                              &
            tx,      ty,     tz                             
          real(kind=WP) :: a1x,a1y,a1z,a2x,a2y,a2z,a3x,a3y,a3z,a4x,a4y,a4z,a1,a2,a3,a4
          real(kind=WP) :: x43,y43,z43,x41,y41,z41,x42,y42,z42,b1,c1,d1
! ======================================================================================================================
      do i=1,nel
        x1(i) =x(1,nc1(i))
        y1(i) =x(2,nc1(i))
        z1(i) =x(3,nc1(i))
        x2(i) =x(1,nc2(i))
        y2(i) =x(2,nc2(i))
        z2(i) =x(3,nc2(i))
        x3(i) =x(1,nc3(i))
        y3(i) =x(2,nc3(i))
        z3(i) =x(3,nc3(i))
        x4(i) =x(1,nc4(i))
        y4(i) =x(2,nc4(i))
        z4(i) =x(3,nc4(i))
      end do
!
      do i=1,nel
       x43 = x4(i) - x3(i)
       y43 = y4(i) - y3(i)
       z43 = z4(i) - z3(i)
       x41 = x4(i) - x1(i)
       y41 = y4(i) - y1(i)
       z41 = z4(i) - z1(i)
       x42 = x4(i) - x2(i)
       y42 = y4(i) - y2(i)
       z42 = z4(i) - z2(i)
!
       rx(i) =  -x41
       ry(i) =  -y41
       rz(i) =  -z41
       sx(i) =  -x42
       sy(i) =  -y42
       sz(i) =  -z42
       tx(i) =  -x43
       ty(i) =  -y43
       tz(i) =  -z43
!
       b1  =  y43*z42 - y42*z43
       c1  =  z43*x42 - z42*x43
       d1  =  x43*y42 - x42*y43
       vol(i) = (x41*b1 + y41*c1 + z41*d1)*one_over_6
      end do
!      
      do i=1,nel
        a1x = ry(i)*sz(i)-rz(i)*sy(i)
        a1y = rz(i)*sx(i)-rx(i)*sz(i)
        a1z = rx(i)*sy(i)-ry(i)*sx(i)
        a1 = a1x*a1x+a1y*a1y+a1z*a1z
!      
        a2x = sy(i)*tz(i)-sz(i)*ty(i)
        a2y = sz(i)*tx(i)-sx(i)*tz(i)
        a2z = sx(i)*ty(i)-sy(i)*tx(i)
        a2 = a2x*a2x+a2y*a2y+a2z*a2z
!      
        a3x = ty(i)*rz(i)-tz(i)*ry(i)
        a3y = tz(i)*rx(i)-tx(i)*rz(i)
        a3z = tx(i)*ry(i)-ty(i)*rx(i)
        a3 = a3x*a3x+a3y*a3y+a3z*a3z
!      
        a4x = a1x+a2x+a3x
        a4y = a1y+a2y+a3y
        a4z = a1z+a2z+a3z
        a4 = a4x*a4x+a4y*a4y+a4z*a4z      
        l_min(i) = six*vol(i)/sqrt(max(a1,a2,a3,a4))
      end do
!
        end subroutine s4dlenmax_sm
! ----------------------------------------------------------------------------------------------------------------------
      end module s4dlenmax_sm_mod