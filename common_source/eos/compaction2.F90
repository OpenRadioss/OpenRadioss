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
      !||    compaction2_mod   ../common_source/eos/compaction2.F90
      !||--- called by ------------------------------------------------------
      !||    eosmain           ../common_source/eos/eosmain.F
      !||====================================================================
      module compaction2_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief  This subroutine contains numerical solving of COMPACTION2 EOS
!----------------------------------------------------------------------------
!! \details STAGGERED SCHEME IS EXECUTED IN TWO PASSES IN EOSMAIN : IFLG=0 THEN IFLG=1
!! \details COLLOCATED SCHEME IS DOING A SINGLE PASS : IFLG=2
!! \details
!! \details  STAGGERED SCHEME
!! \details     EOSMAIN / IFLG = 0 : DERIVATIVE CALCULATION FOR SOUND SPEED ESTIMATION c[n+1] REQUIRED FOR PSEUDO-VISCOSITY (DPDE:partial derivative, DPDM:total derivative)
!! \details     MQVISCB            : PSEUDO-VISCOSITY Q[n+1]
!! \details     MEINT              : INTERNAL ENERGY INTEGRATION FOR E[n+1] : FIRST PART USING P[n], Q[n], and Q[n+1] CONTRIBUTIONS
!! \details     EOSMAIN / IFLG = 1 : UPDATE P[n+1], T[N+1]
!! \details                          INTERNAL ENERGY INTEGRATION FOR E[n+1] : LAST PART USING P[n+1] CONTRIBUTION
!! \details                            (second order integration dE = -P.dV where P = 0.5(P[n+1] + P[n]) )
!! \details  COLLOCATED SCHEME
!! \details     EOSMAIN / IFLG = 2 : SINGLE PASS FOR P[n+1] AND DERIVATIVES
!----------------------------------------------------------------------------
      !||====================================================================
      !||    compaction2     ../common_source/eos/compaction2.F90
      !||--- called by ------------------------------------------------------
      !||    eosmain         ../common_source/eos/eosmain.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod    ../common_source/modules/constant_mod.F
      !||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
      !||====================================================================
      subroutine compaction2(&
                            iflag, nel  , pm   , off   , eint  , mu    , &
                            dvol , mat  , psh  , &
                            pnew , dpdm , dpde , mu_bak,&
                            npf  , tf   , snpc , stf   , npropm, nummat,&
                            eos_param)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
       use constant_mod , only : zero, half, one, two, three, three100
       use eos_param_mod , only : eos_param_
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
      integer,intent(in) :: nel !< number of element in the currenbt group
      integer,intent(in) :: npropm, nummat !< array sizes
      integer,intent(in) :: mat(nel), iflag
      my_real,intent(inout) :: pm(npropm,nummat) !< material data (real parameters)
      my_real,intent(inout) :: off(nel),eint(nel),mu(nel),dvol(nel)
      my_real,intent(inout) :: pnew(nel),dpdm(nel),dpde(nel)
      integer,intent(in) :: snpc, stf !< array sizes
      integer,intent(in)::npf(snpc) !< data structure for /FUNCT
      my_real,intent(in)::tf(stf) !< data structure for /FUNCT
      type(eos_param_),intent(in) :: eos_param !< data structure for EoS parameters
      my_real,intent(inout) :: mu_bak(nel) !< backup of mu for unloading
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      integer i, mx, iform, p_func_id
      my_real :: p0,psh(nel),e0,sph, b(nel),pne1,pfrac
      my_real :: p(nel),p_
      my_real :: alpha
      my_real :: bmin, bmax, mumin, mumax,Fscale,Xscale
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External
! ----------------------------------------------------------------------------------------------------------------------
      my_real, external :: finter
! In order to get : y <- f(x)  and dydx <- f'(x) :
! Use :             y = FINTER(func_id,x,NPF,TF,dydx)
!                       where - NPF,TF are constant arrays automatically built by Starter (/FUNCT data).
!                               NPF contains the cursors and number of points for each function.
!                               TF contains the abscissa and ordinate values of the functions.
!                             - func_id is internal identifier (example : first function to be read /FUNCT/999 has user id #999 and internal id #1)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
       mx         = mat(1)
       e0         = pm(23,mx)
       psh(1:nel) = pm(88,mx)
       sph        = pm(69,mx)
       p0         = pm(31,mx)
       pfrac      = pm(37,mx)

       bmin  = eos_param%uparam(1)
       bmax  = eos_param%uparam(2)
       mumin = eos_param%uparam(3)
       mumax = eos_param%uparam(4)
       Fscale= eos_param%uparam(5)
       Xscale= eos_param%uparam(6)
       psh   = eos_param%uparam(7)
       iform = eos_param%iparam(1)
       p_func_id = eos_param%func(1)

      !----------------------------------------------------------------!
      !  COMPACTION EOS                                                !
      !----------------------------------------------------------------!
      !--- constant unload slope ---!
      if(iform == 1)then
        do i=1,nel
          p(i) = finter(p_func_id,xscale*mu(i),npf,tf,dpdm(i))
          p(i) = fscale * p(i)
          p_   = finter(p_func_id,xscale*mu_bak(i),npf,tf,dpdm(i))
          p_   = p_ * Fscale
          b(i) = bmax
          pne1 = p_-(mu_bak(i)-mu(i))*b(i)
          if(mu_bak(i) > mumin) p(i) = min(pne1, p(i))
          p(i) = max(p(i),pfrac)*off(i)
        enddo !next i
      !--- continuous unload slope (increases with compaction) ---!
      elseif(iform == 2)then
        do i=1,nel
          p(i) = finter(p_func_id,xscale*mu(i),npf,tf,dpdm(i))
          p(i) = fscale * p(i)
          p_   = finter(p_func_id,xscale*mu_bak(i),npf,tf,dpdm(i))
          p_   = p_ * Fscale
          !linear unload modulus
          alpha = one
          if(mumax > zero)then
            alpha=mu_bak(i)/mumax
          endif
          b(i) = alpha*bmax+(one-alpha)*bmin
          pne1 = p_-(mu_bak(i)-mu(i))*b(i)
          if(mu_bak(i) > mumin) p(i) = min(pne1, p(i))
          p(i) = max(p(i),pfrac)  *off(i)
        enddo !next i
      endif
      !----------------------------------------------------------------!
      !  SOUND SPEED                                                   !
      !----------------------------------------------------------------!
      do i=1,nel
        dpdm(i) = max(b(i),dpdm(i))
        dpde(i) = zero
      enddo !next i
      !----------------------------------------------------------------!
      !  OUTPUT                                                        !
      !----------------------------------------------------------------!
      do i=1,nel
        p(i)=max(pfrac,p(i))*off(i)
        pnew(i) = p(i)-psh(i)   ! P(mu[n+1],E[n+1])
      enddo !next i

      IF(iflag == 1) THEN
        !----------------------------------------------------------------!
        !  FRACTURE  - MU_BAK                                            !
        !----------------------------------------------------------------!
        do i=1,nel
          eint(i) = eint(i) - half*dvol(i)*(pnew(i)+psh(i) )
        enddo !next i
        !----------------------------------------------------------------!
        !  FRACTURE  - MU_BAK                                            !
        !----------------------------------------------------------------!
        do i=1,nel
          if(mu(i) > mu_bak(i)) mu_bak(i) = min(mumax,mu(i))
        enddo !next i
        !----------------------------------------------------------------!
        !  OUTPUT                                                        !
        !----------------------------------------------------------------!
        do i=1,nel
          p(i)=max(pfrac,p(i))*off(i)
          pnew(i) = p(i)-psh(i)
        enddo !next i
        !----------------------------------------------------------------!
        !  PARTIAL DERIVATIE                                             !
        !----------------------------------------------------------------!
        do i=1,nel
          dpde(i) = zero
        end do

      elseif(iflag == 2) then
        !----------------------------------------------------------------!
        !  FRACTURE  - MU_BAK                                            !
        !----------------------------------------------------------------!
        do i=1,nel
          if(mu(i) > mu_bak(i)) mu_bak(i) = min(mumax,mu(i))
        enddo !next i
        !----------------------------------------------------------------!
        !  OUTPUT                                                        !
        !----------------------------------------------------------------!
        do i=1,nel
          p(i)=max(pfrac,p(i))*off(i)
          pnew(i) = p(i)-psh(i)
        enddo !next i
        !----------------------------------------------------------------!
        !  PARTIAL DERIVATIE                                             !
        !----------------------------------------------------------------!
        do i=1,nel
          dpde(i) = zero
        end do
      endif

!------------------------
      return
      end subroutine compaction2
!------------------------
      end module compaction2_mod
