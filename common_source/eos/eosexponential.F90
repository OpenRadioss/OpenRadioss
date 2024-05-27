!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief This subroutine is solving Exponential EoS
!! \details P = P0 exp(alpha.time) - PSH
      subroutine eosexponential(iflag ,nel   ,pm   ,off  ,eint ,&
                                dvol  ,vnew  ,mat  ,psh  ,      &
                                pnew  ,dpdm  ,dpdE ,theta,time ,&
                                npropm,nummat,wfext)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod , only : three100, half, zero
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: npropm , nummat          !< size for pm array
      integer,intent(in) :: mat(nel)                 !< material identifiers for elems 1 to nel
      integer,intent(in) :: IFLAG                    !< flag mentionning what need to be computed
      integer,intent(in) :: NEL                      !< number of elems in group (current group size)
      my_real, intent(in) :: time                    !< simulation current time
      my_real, intent(in) :: pm(npropm,nummat)       !< parameters of all materials
      my_real, intent(in) :: vnew(nel)               !< current volume of elems
      my_real, intent(in) :: off(nel)                !< state of elems (0.0 if deleted)
      my_real, intent(in) :: dvol(nel)               !< volume change of elems
      my_real, intent(inout) :: pnew(nel)            !< pressure     
      my_real, intent(inout) :: dpdm(nel)            !< total derivative : mu = rho/rho0-1
      my_real, intent(inout) :: dpdE(nel)            !< partial derivative : E=rho0.e
      my_real, intent(inout) :: theta(nel)           !< temperature
      my_real, intent(inout) :: wfext                !< work of external forces                
      my_real, intent(inout) :: psh(nel)             !< pressure shift
      my_real, intent(inout) :: eint(nel)            !< internal energy
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i, mx
      my_real :: pp,p0,alpha,wfextt
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

!------------------------
      if(iflag == 0) then
       mx = mat(1)
       p0 = pm(104,mx) !starter:p0-psh
       alpha = pm( 32,mx)
       psh(1:nel) = pm( 88,mx)
       dpdm(1:nel) = zero
       dpdE(1:nel) = zero

!------------------------
      elseif(iflag == 1) then
       wfextt = zero
       mx = mat(1)
       p0 = pm(104,mx) !starter:p0-psh
       alpha = pm( 32,mx)
       psh(1:nel) = pm( 88,mx)
       do i=1,nel
         pnew(i) = p0*exp(alpha*time)
         pnew(i) = pnew(i)*off(i)
         eint(i) = eint(i) - half*dvol(i)*(pnew(i)+psh(i))
         wfextt  = wfextt-dvol(i)*psh(i)
       enddo
!$OMP ATOMIC
       wfext = wfext + wfextt
!$OMP END ATOMIC
       theta(1:nel) = three100
       
!------------------------
      elseif(iflag == 2) then
       mx  = mat(1)
       mx = mat(1)
       p0 = pm(104,mx) !starter:p0-psh
       alpha = pm( 32,mx)
       psh(1:nel) = pm( 88,mx)
         do i=1,nel
           if (vnew(i) > zero) then
             pnew(i) =  p0*exp(alpha*time)
             pnew(i) = pnew(i)*off(i)
             dpdm(i) = zero
             dpdE(i) = zero
           endif
         enddo
       endif

!------------------------
      RETURN
      END
