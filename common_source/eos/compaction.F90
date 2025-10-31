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
!||    compaction_mod   ../common_source/eos/compaction.F90
!||--- called by ------------------------------------------------------
!||    eosmain          ../common_source/eos/eosmain.F
!||    eosmain51        ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
      module compaction_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief  This subroutine contains numerical solving of COMPACTION EOS
!! \details  Iform : formulation flag for unload behavior
!! \details           1 : constant unload modulus
!! \details           2 : unload modulus increases with compaction from C1 to BUNL in [MU_MIN,MU_MAX]
!! \details   MU_MIN : elastic behavior up to this limit
!! \details   MU_MAX : elastic behavior above this limit
!! \details   C0,C1,C2,C3 : EoS parameter
!! \details   BUNL : unload
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
!||    compaction      ../common_source/eos/compaction.F90
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine compaction( &
                            iflag , nel   , pfrac , off  , eint , mu   , mu2 , &
                            dvol  , psh   , &
                            pnew  , dpdm  , dpde  , mu_bak,&
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
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: nel !< number of element in the currenbt group
      integer,intent(in) :: iflag
      real(kind=WP),intent(in) :: pfrac, off(nel),mu(nel),mu2(nel),dvol(nel)
      real(kind=WP),intent(inout) :: pnew(nel),dpdm(nel),dpde(nel),mu_bak(nel),eint(nel)
      type(eos_param_),intent(in) :: eos_struct !< data structure for EoS parameters
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i, iform
      real(kind=WP) :: psh(nel),e0,b(nel),pne1
      real(kind=WP) :: c0,c1,c2,c3,bunl,p(nel),p_
      real(kind=WP) :: alpha,mu_min,mu_max
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
       mu_max = eos_struct%uparam(1)
       mu_min = eos_struct%uparam(2)
       bunl = eos_struct%uparam(3)

       c0 = eos_struct%uparam(4)
       c1 = eos_struct%uparam(5)
       c2 = eos_struct%uparam(6)
       c3 = eos_struct%uparam(7)
       e0 = eos_struct%e0

       !new format
       psh(1:nel) = eos_struct%psh
       mu_max = eos_struct%uparam(1)
       mu_min = eos_struct%uparam(2)
       bunl = eos_struct%uparam(3)
       iform = eos_struct%iparam(1)

          !----------------------------------------------------------------!
          !  COMPACTION EOS                                                !
          !----------------------------------------------------------------!
          !--- constant unload slope ---!
          if(iform == 1)then
            do i=1,nel
              p(i) = c0+c1*mu(i)+(c2+c3*mu(i))*mu2(i)
              p_   = c0+c1*mu_bak(i)+(c2+c3*mu_bak(i))*mu_bak(i)*mu_bak(i)
              b(i) = bunl
              pne1 = p_-(mu_bak(i)-mu(i))*b(i)
              if(mu_bak(i) > mu_min) p(i) = min(pne1, p(i))
              p(i) = max(p(i),pfrac)*off(i)
            enddo !next i
            !--- continuous unload slope (increases with compaction) ---!
          elseif(iform == 2)then
            do i=1,nel
              p(i) = c0+c1*mu(i)+(c2+c3*mu(i))*mu2(i)
              p_   = c0+c1*mu_bak(i)+(c2+c3*mu_bak(i))*mu_bak(i)*mu_bak(i)
              !linear unload modulus
              alpha = one
              if(mu_max > zero)then
                alpha=mu_bak(i)/mu_max
              endif
              b(i) = alpha*bunl+(one-alpha)*c1
              pne1 = p_-(mu_bak(i)-mu(i))*b(i)
              if(mu_bak(i) > mu_min) p(i) = min(pne1, p(i))
              p(i) = max(p(i),pfrac)  *off(i)
            enddo !next i
          endif
          !----------------------------------------------------------------!
          !  SOUND SPEED                                                   !
          !----------------------------------------------------------------!
          do i=1,nel
            dpdm(i) = c1 + max(zero,mu(i)) *( two*c2+three*c3*mu(i) )
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

          if(iflag == 1) then
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
              if(mu(i) > mu_bak(i)) mu_bak(i) = min(mu_max,mu(i))
            enddo !next i
            !----------------------------------------------------------------!
            !  OUTPUT                                                        !
            !----------------------------------------------------------------!
            do i=1,nel
              p(i)=max(pfrac,p(i))*off(i)
              pnew(i) = p(i)-psh(i)
            enddo !next i
            !----------------------------------------------------------------!
            !  PARTIAL DERIVATIVE                                            !
            !----------------------------------------------------------------!
            do i=1,nel
              dpde(i) = zero
            enddo

          elseif(iflag == 2) then
            !----------------------------------------------------------------!
            !  FRACTURE  - MU_BAK                                            !
            !----------------------------------------------------------------!
            do i=1,nel
              if(mu(i) > mu_bak(i)) mu_bak(i) = min(mu_max,mu(i))
            enddo !next i
            !----------------------------------------------------------------!
            !  OUTPUT                                                        !
            !----------------------------------------------------------------!
            do i=1,nel
              p(i)=max(pfrac,p(i))*off(i)
              pnew(i) = p(i)-psh(i)
            enddo !next i
          endif

!------------------------
          return
        end subroutine compaction
!------------------------
      end module compaction_mod
