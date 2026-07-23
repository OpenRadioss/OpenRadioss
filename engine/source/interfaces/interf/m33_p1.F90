!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
      module m33_p1_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine compute the first eiginvalue of a symmetry matrix 3x3
! ======================================================================================================================
        subroutine m33_p1(m11,m22,m33,m23,m13,m12,pin1)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only: WP
          use constant_mod , only : one,two,third,six,pi,em10,zero,half
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP), intent(in   ) :: m11,m22,m33,m23,m13,m12
          real(kind=WP), intent(  out) :: pin1
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP) :: s1,t1,t2,s(6),p1,detb,dets,r,alpha
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
! | S1 S4 S6 |
! | S4 S2 S5 |         
! | S6 S5 S3 |         
!
        s1 = m23*m23+m13*m13+m12*m12
        if (s1 > em10) then
          t1 = third*(m11+m22+m33)
          s(1) = m11 - t1
          s(2) = m22 - t1
          s(3) = m33 - t1
          s(4) = m12 
          s(5) = m23 
          s(6) = m13 
          t2 = s(1)*s(1)+s(2)*s(2)+s(3)*s(3) + two*s1
          p1 = sqrt(t2/six)
          dets = s(1)*(s(2)*s(3)-s(6)*s(6))-s(4)*(s(4)*s(3)-s(5)*s(6))+s(5)*(s(4)*s(6)-s(2)*s(5))
          detb = dets/p1
          r = half*detb
          if (r <= -one) then
            alpha = third*pi 
          elseif (r >= one) then
            alpha = zero
          else
            alpha = third*acos(r)
          end if
          pin1 =  t1 + two * p1* cos(alpha)
        else
          pin1 = max(m11,m22,m33)
        end if
          ! -------------------------
!
          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine m33_p1
      end module m33_p1_mod
