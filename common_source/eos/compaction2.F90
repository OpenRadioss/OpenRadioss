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
!||    eosmain51         ../engine/source/materials/mat/mat051/eosmain51.F90
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
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine compaction2(&
                             iflag  , nel   , pmin , off    , eint  , mu    , &
                             dvol   , psh  , &
                             pnew   , dpdm  , dpde , nvareos, vareos,&
                             npf    , tf    , snpc , stf   , &
                             eos_struct)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero, half, one, two, three, three100
          use eos_param_mod , only : eos_param_
          use precision_mod , only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: nel !< number of element in the currenbt group
      integer,intent(in) :: iflag
      real(kind=WP),intent(in) :: pmin !< minimum pressure
      real(kind=WP),intent(in) :: off(nel),mu(nel),dvol(nel)
      real(kind=WP),intent(inout) :: eint(nel)
      real(kind=WP),intent(inout) :: pnew(nel),dpdm(nel),dpde(nel)
      integer,intent(in) :: snpc, stf !< array sizes
      integer,intent(in)::npf(snpc) !< data structure for /FUNCT
      integer,intent(in)::nvareos !< number of variables for eos
      real(kind=WP),intent(inout)::vareos(nel,nvareos) !< ueos user variable (mu_bak
      real(kind=WP),intent(in)::tf(stf) !< data structure for /FUNCT
      type(eos_param_),intent(in) :: eos_struct !< data structure for EoS parameters
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, iform, p_func_id
          real(kind=WP) :: psh(nel), b(nel),pne1,pfrac
          real(kind=WP) :: p(nel),p_
          real(kind=WP) :: alpha
          real(kind=WP) :: bmin, bmax, mumin, mumax,Fscale,Xscale
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP), external :: finter
! In order to get : y <- f(x)  and dydx <- f'(x) :
! Use :             y = FINTER(func_id,x,NPF,TF,dydx)
!                       where - NPF,TF are constant arrays automatically built by Starter (/FUNCT data).
!                               NPF contains the cursors and number of points for each function.
!                               TF contains the abscissa and ordinate values of the functions.
!                             - func_id is internal identifier (example : first function to be read /FUNCT/999 has user id #999 and internal id #1)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
       psh(1:nel) = eos_struct%psh
       pfrac      = pmin

       bmin  = eos_struct%uparam(1)
       bmax  = eos_struct%uparam(2)
       mumin = eos_struct%uparam(3)
       mumax = eos_struct%uparam(4)
       Fscale= eos_struct%uparam(5)
       Xscale= eos_struct%uparam(6)

       iform = eos_struct%iparam(1)

       p_func_id = eos_struct%func(1)

       !vareos(:,1) is mu_bak for loading hsitory (load/unload/reload)

          !----------------------------------------------------------------!
          !  COMPACTION EOS                                                !
          !----------------------------------------------------------------!
          !--- constant unload slope ---!
          if(iform == 1)then
            do i=1,nel
              p(i) = finter(p_func_id,xscale*mu(i),npf,tf,dpdm(i))
              p(i) = fscale * p(i)
              p_   = finter(p_func_id,xscale*vareos(i,1),npf,tf,dpdm(i))
              p_   = p_ * Fscale
              b(i) = bmax
              pne1 = p_-(vareos(i,1)-mu(i))*b(i)
              if(vareos(i,1) > mumin) p(i) = min(pne1, p(i))
              p(i) = max(p(i),pfrac)*off(i)
            enddo !next i
            !--- continuous unload slope (increases with compaction) ---!
          elseif(iform == 2)then
            do i=1,nel
              p(i) = finter(p_func_id,xscale*mu(i),npf,tf,dpdm(i))
              p(i) = fscale * p(i)
              p_   = finter(p_func_id,xscale*vareos(i,1),npf,tf,dpdm(i))
              p_   = p_ * Fscale
              !linear unload modulus
              alpha = one
              if(mumax > zero)then
                alpha=vareos(i,1)/mumax
              endif
              b(i) = alpha*bmax+(one-alpha)*bmin
              pne1 = p_-(vareos(i,1)-mu(i))*b(i)
              if(vareos(i,1) > mumin) p(i) = min(pne1, p(i))
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
              if(mu(i) > vareos(i,1)) vareos(i,1) = min(mumax,mu(i))
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
              if(mu(i) > vareos(i,1)) vareos(i,1) = min(mumax,mu(i))
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
