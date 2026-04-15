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
!||    nice_solids_mod   ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||--- called by ------------------------------------------------------
!||    sigeps131         ../engine/source/materials/mat/mat131/sigeps131.F90
!||====================================================================
      module nice_solids_mod
! \brief NICE return mapping for solids in /MAT/LAW131
! \details Perform the NICE (Next Increment Corrects Error) explicit return
!          mapping algorithm for solid elements in /MAT/LAW131.
      contains
!||====================================================================
!||    nice_solids                              ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||--- called by ------------------------------------------------------
!||    sigeps131                                ../engine/source/materials/mat/mat131/sigeps131.F90
!||--- calls      -----------------------------------------------------
!||    elasto_plastic_eq_stress                 ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||    elasto_plastic_kinematic_hardening       ../engine/source/materials/mat/mat131/elasto_plastic_kinematic_hardening.F90
!||    elasto_plastic_trial_stress              ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||    elasto_plastic_yield_stress              ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||    mstrain_rate                             ../engine/source/materials/mat_share/mstrain_rate.F
!||--- uses       -----------------------------------------------------
!||    constant_mod                             ../common_source/modules/constant_mod.F
!||    elasto_plastic_eq_stress_mod             ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||    elasto_plastic_kinematic_hardening_mod   ../engine/source/materials/mat/mat131/elasto_plastic_kinematic_hardening.F90
!||    elasto_plastic_trial_stress_mod          ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||    elasto_plastic_yield_stress_mod          ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||    matparam_def_mod                         ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                            ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine nice_solids(                                                  &
        nel      ,matparam ,rho      ,nvartmp  ,vartmp   ,iresp    ,           &
        depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,           &
        epspxx   ,epspyy   ,epspzz   ,epspxy   ,epspyz   ,epspzx   ,           &     
        sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,           &
        signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,           &
        soundsp  ,off      ,pla      ,dpla     ,seq      ,et       ,           &
        sigy     ,timestep ,epsd     ,temp     ,israte   ,asrate   ,           &
        l_sigb   ,sigb     ,nuvar    ,uvar     ,ieos     ,dpdm     ,           &
        jthe     ,fheat    ,voln     ,vpflag   ,ikine    ,chard    ,           &
        inloc    ,dplanl   )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use precision_mod, only : WP
        use elasto_plastic_trial_stress_mod
        use elasto_plastic_eq_stress_mod
        use elasto_plastic_yield_stress_mod
        use elasto_plastic_kinematic_hardening_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        integer,                       intent(in)    :: nel       !< Number of elements in the group
        type(matparam_struct_),        intent(in)    :: matparam  !< Material parameters data
        real(kind=WP), dimension(nel), intent(in)    :: rho       !< Density at current time
        real(kind=WP), dimension(nel), intent(in)    :: depsxx    !< Strain increment xx
        real(kind=WP), dimension(nel), intent(in)    :: depsyy    !< Strain increment yy
        real(kind=WP), dimension(nel), intent(in)    :: depszz    !< Strain increment zz
        real(kind=WP), dimension(nel), intent(in)    :: depsxy    !< Strain increment xy
        real(kind=WP), dimension(nel), intent(in)    :: depsyz    !< Strain increment yz
        real(kind=WP), dimension(nel), intent(in)    :: depszx    !< Strain increment zx
        real(kind=WP), dimension(nel), intent(in)    :: epspxx    !< Strain rate component xx
        real(kind=WP), dimension(nel), intent(in)    :: epspyy    !< Strain rate component yy
        real(kind=WP), dimension(nel), intent(in)    :: epspzz    !< Strain rate component zz
        real(kind=WP), dimension(nel), intent(in)    :: epspxy    !< Strain rate component xy
        real(kind=WP), dimension(nel), intent(in)    :: epspyz    !< Strain rate component yz
        real(kind=WP), dimension(nel), intent(in)    :: epspzx    !< Strain rate component zx
        real(kind=WP), dimension(nel), intent(in)    :: sigoxx    !< Previous stress xx
        real(kind=WP), dimension(nel), intent(in)    :: sigoyy    !< Previous stress yy
        real(kind=WP), dimension(nel), intent(in)    :: sigozz    !< Previous stress zz
        real(kind=WP), dimension(nel), intent(in)    :: sigoxy    !< Previous stress xy
        real(kind=WP), dimension(nel), intent(in)    :: sigoyz    !< Previous stress yz
        real(kind=WP), dimension(nel), intent(in)    :: sigozx    !< Previous stress zx
        real(kind=WP), dimension(nel), intent(inout) :: signxx    !< Current stress xx
        real(kind=WP), dimension(nel), intent(inout) :: signyy    !< Current stress yy
        real(kind=WP), dimension(nel), intent(inout) :: signzz    !< Current stress zz
        real(kind=WP), dimension(nel), intent(inout) :: signxy    !< Current stress xy
        real(kind=WP), dimension(nel), intent(inout) :: signyz    !< Current stress yz
        real(kind=WP), dimension(nel), intent(inout) :: signzx    !< Current stress zx
        real(kind=WP), dimension(nel), intent(inout) :: soundsp   !< Current sound speed
        real(kind=WP), dimension(nel), intent(inout) :: off       !< Element failure flag
        real(kind=WP), dimension(nel), intent(inout) :: pla       !< Accumulated plastic strain
        real(kind=WP), dimension(nel), intent(inout) :: dpla      !< Plastic strain increment
        real(kind=WP), dimension(nel), intent(inout) :: seq       !< Equivalent stress
        real(kind=WP), dimension(nel), intent(inout) :: et        !< Hourglass stabilization variable
        real(kind=WP), dimension(nel), intent(inout) :: sigy      !< Current yield stress
        real(kind=WP), intent(in)                    :: timestep  !< Time step
        real(kind=WP), dimension(nel), intent(inout) :: epsd      !< Plastic strain rate
        integer,                       intent(in)    :: iresp     !< Precision flag
        integer,                       intent(in)    :: nvartmp   !< Number of variables used in tabulated variables
        integer, dimension(nel,nvartmp), intent(inout) :: vartmp  !< Temporary variables for tabulated hardening
        real(kind=WP), dimension(nel), intent(inout) :: temp      !< Temperature
        integer,                       intent(in)    :: israte    !< Strain rate filtering flag
        real(kind=WP),                 intent(in)    :: asrate    !< Strain rate filtering weighting factor
        integer,                       intent(in)    :: l_sigb    !< Size of backstress array
        real(kind=WP),dimension(nel,l_sigb),intent(inout) :: sigb !< Backstress components for kinematic hardening        
        integer,                       intent(in)    :: nuvar     !< Number of user variables
        real(kind=WP),dimension(nel,nuvar), intent(inout) :: uvar !< User variables
        integer,                       intent(in)    :: ieos      !< Equation of state flag
        real(kind=WP), dimension(nel), intent(inout) :: dpdm      !< Pressure derivative of the shear modulus for EOS coupling
        integer,                       intent(in)    :: jthe      !< /HEAT/MAT flag
        real(kind=WP), dimension(nel), intent(inout) :: fheat     !< Heat energy accumulated for /HEAT/MAT
        real(kind=WP), dimension(nel), intent(in)    :: voln      !< Current element volume
        integer,                       intent(in)    :: vpflag    !< Viscoplasticity flag
        integer,                       intent(in)    :: ikine     !< Kinematic hardening type
        real(kind=WP),                 intent(in)    :: chard     !< Isotropic/kinematic mixed hardening factor
        integer,                       intent(in)    :: inloc     !< Non-local regularization flag
        real(kind=WP), dimension(nel), intent(in)    :: dplanl    !< Non-local plastic strain increment
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i,j,ii,nindx,indx(nel),idev
        real(kind=WP), dimension(nel,6,6) :: cstf,N
        real(kind=WP), dimension(nel) :: pla0,normxx,normyy,normzz,            &
          normxy,normyz,normzx,phi,young,dsigy_dpla,dtemp_dpla,s13,s23,s43,    &
          shf,sigbxx,sigbyy,sigbzz,sigbxy,sigbyz,sigbzx,sigy0,dsigy0_dpla,     &
          dtemp0_dpla,zeros,dsigxx,dsigyy,dsigzz,dsigxy,dsigyz,dsigzx,phi0,    &
          sig0xx,sig0yy,sig0zz,sig0xy,sig0yz,sig0zx,seq0,norm0xx,norm0yy,      &
          norm0zz,norm0xy,norm0yz,norm0zx,sigm, dsigbxx_dlam,dsigbyy_dlam,     &
          dsigbzz_dlam,dsigbxy_dlam,dsigbyz_dlam,dsigbzx_dlam,dsigy_dlam,      &
          dpla_dlam,dseq_dlam,dlam,dsigxx_dlam,dsigyy_dlam,dsigzz_dlam,        &
          dsigxy_dlam,dsigyz_dlam,dsigzx_dlam,signxx_i,signyy_i,signzz_i,      &
          signxy_i,signyz_i,signzx_i,epsd_i,sigy_i,pla_i,seq0_i,dsigy_dpla_i,  &
          temp_i,seq_i,normxx_i,normyy_i,normzz_i,normxy_i,normyz_i,normzx_i,  &
          dphi_dseq,dphi_dsigy,dphi_dlam,sig_dseqdsig,dphi,dtemp_dpla_i
        real(kind=WP), dimension(nel,l_sigb) :: dsigb_dlam,sigb_i
        integer, dimension(nel,nvartmp) :: ipos0,vartmp_i
!
        integer, parameter :: eltype = 1               !< Element type (1 - Solids, 2 - Shells)
        logical, dimension(nel) :: active_elements_mask
        integer, dimension(nel) :: temp_all_indices
        zeros(1:nel) = zero
!===============================================================================
!
        !=======================================================================
        !< - Initialisation of computation on time step
        !=======================================================================
        !< Total or deviatoric strain rate for scaled yield stress formulation
        if (vpflag > 1 .and. vpflag <= 3) then
          idev = vpflag - 2
          call mstrain_rate(                                                   &
            nel      ,israte   ,asrate   ,epsd     ,idev     ,                 &
            epspxx   ,epspyy   ,epspzz   ,epspxy   ,epspyz   ,epspzx   )
        endif
        !< Initialisation of the hourglass control variable
        et(1:nel) = one
        !< Increment of cumulated plastic strain
        dpla(1:nel) = zero
        !< Derivative of temperature w.r.t. cumulated plastic strain
        dtemp_dpla(1:nel) = zero
        dtemp_dpla_i(1:nel) = zero
        !< Save the initial cumulated plastic strain value
        pla0(1:nel) = pla(1:nel)
        !< Save initial values of the stress tensor components for shell elements
        sig0xx(1:nel) = sigoxx(1:nel)
        sig0yy(1:nel) = sigoyy(1:nel)
        sig0zz(1:nel) = sigozz(1:nel)
        sig0xy(1:nel) = sigoxy(1:nel)
        sig0yz(1:nel) = sigoyz(1:nel)
        sig0zx(1:nel) = sigozx(1:nel)
        !< Recover previous value of the equivalent stress
        seq0(1:nel) = seq(1:nel)
        !< Update element status flag
        where (off(1:nel) < em01)
          off(1:nel) = zero
        end where
        where (off(1:nel) < one)
          off(1:nel) = off(1:nel)*four_over_5
        end where
!
        !=======================================================================
        !< - Computation of the elastic trial stress tensor
        !=======================================================================
        call elasto_plastic_trial_stress(                                      &
          matparam ,nel      ,soundsp  ,cstf     ,young    ,rho      ,         &
          depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,         &
          sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,         &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,         &
          eltype   ,shf      ,s13      ,s23      ,s43      ,ieos     ,         &
          dpdm     ,nvartmp  ,vartmp   ,epsd     ,nuvar    ,uvar     ,         &
          temp     )
!
        !=======================================================================
        !< - Computation of the initial yield stress
        !=======================================================================
        call elasto_plastic_yield_stress(                                      &
          matparam ,nel      ,sigy     ,pla      ,epsd     ,dsigy_dpla,        &
          nvartmp  ,vartmp   ,temp     ,dtemp_dpla,jthe    )
!
        !=======================================================================
        !< - Update non-local temperature if needed
        !=======================================================================        
        if (inloc > 0) then
          if (jthe /= 0) then
            where (off(1:nel) == one)
              fheat(1:nel) = fheat(1:nel) + sigy(1:nel)*dplanl(1:nel)*voln(1:nel)
            end where
          else
            where (off(1:nel) == one)
              temp(1:nel)  = temp(1:nel) + dtemp_dpla(1:nel)*dplanl(1:nel)
            end where
          endif
        endif
!
        !=======================================================================
        !< - Backstress tensor computation for kinematic hardening models
        !=======================================================================
        if (ikine > 0) then
          sigbxx(1:nel) = zero
          sigbyy(1:nel) = zero
          sigbzz(1:nel) = zero
          sigbxy(1:nel) = zero
          sigbyz(1:nel) = zero
          sigbzx(1:nel) = zero
          !< Compute the backstress tensor from all C-R kinematic hardenings
          do j = 1, l_sigb/6
            sigbxx(1:nel) = sigbxx(1:nel) + sigb(1:nel,6*(j-1) + 1)
            sigbyy(1:nel) = sigbyy(1:nel) + sigb(1:nel,6*(j-1) + 2)
            sigbzz(1:nel) = sigbzz(1:nel) + sigb(1:nel,6*(j-1) + 3)
            sigbxy(1:nel) = sigbxy(1:nel) + sigb(1:nel,6*(j-1) + 4)
            sigbyz(1:nel) = sigbyz(1:nel) + sigb(1:nel,6*(j-1) + 5)
            sigbzx(1:nel) = sigbzx(1:nel) + sigb(1:nel,6*(j-1) + 6)
          enddo
          !< Add the kinematic hardening contribution to stress tensors
          signxx(1:nel) = signxx(1:nel) - sigbxx(1:nel)
          signyy(1:nel) = signyy(1:nel) - sigbyy(1:nel)
          signzz(1:nel) = signzz(1:nel) - sigbzz(1:nel)
          signxy(1:nel) = signxy(1:nel) - sigbxy(1:nel)
          signyz(1:nel) = signyz(1:nel) - sigbyz(1:nel)
          signzx(1:nel) = signzx(1:nel) - sigbzx(1:nel)
          sig0xx(1:nel) = sig0xx(1:nel) - sigbxx(1:nel)
          sig0yy(1:nel) = sig0yy(1:nel) - sigbyy(1:nel)
          sig0zz(1:nel) = sig0zz(1:nel) - sigbzz(1:nel)
          sig0xy(1:nel) = sig0xy(1:nel) - sigbxy(1:nel)
          sig0yz(1:nel) = sig0yz(1:nel) - sigbyz(1:nel)
          sig0zx(1:nel) = sig0zx(1:nel) - sigbzx(1:nel)
          !< Initial yield stress computation for kinematic hardening models
          zeros(1:nel) = zero
          ipos0(1:nel,1:nvartmp) = 0
          call elasto_plastic_yield_stress(                                    &
            matparam ,nel      ,sigy0    ,zeros    ,epsd     ,dsigy0_dpla,     &
            nvartmp  ,ipos0    ,temp     ,dtemp0_dpla,jthe   )
          !< Update of the yield stress for kinematic hardening models
          sigy(1:nel) = (one - chard)*sigy(1:nel) + chard*sigy0(1:nel)
        endif
!
        !=======================================================================
        !< - Computation of the trial equivalent stress and its 1st derivative
        !=======================================================================
        call elasto_plastic_eq_stress(                                         &
          matparam ,nel      ,seq      ,iresp    ,eltype   ,                   &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,         &
          normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,         &
          N        ,.false.  )
!
        !=======================================================================
        !< - Computation of the trial yield function and count yielding elements
        !=======================================================================
        phi(1:nel) = (seq(1:nel) / sigy(1:nel))**2 - one
        active_elements_mask(1:nel) = (phi(1:nel) >= zero .and. off(1:nel) == one)
        where (.not. active_elements_mask(1:nel))
          phi(1:nel) = zero
        end where
        nindx = COUNT(active_elements_mask(1:nel))
        temp_all_indices(1:nel) = [(i, i=1,nel)]
!
        !=======================================================================
        !< - Return mapping 1 step using Next Increment Correct Error (N.I.C.E)
        !=======================================================================
        if (nindx > 0) then
!
          ! Note     : in this part, the purpose is to compute in one iteration
          ! a plastic multiplier allowing to update internal variables to satisfy
          ! the consistency condition. 
          ! Its expression is : dlam = - (phi0 + dphi)/dphi_dlam
          ! -> phi0 : old value of yield function (known)
          ! -> dphi : yield function prediction (to compute)
          ! -> dphi_dlambda : derivative of phi with respect to dlambda by taking
          !                   into account of internal variables kinetic : 
          !                plasticity, strain-rate ... (to be computed)
!
          !< Extract the indices of the yielding elements
          indx(1:nindx) = PACK(temp_all_indices(1:nel), active_elements_mask(1:nel))
!
          !< Loop over yielding elements
#include "vectorize.inc"
          do ii = 1,nindx
            i = indx(ii)
!
            !< 0 - Preliminary switch to local index of yielding elements
            !< -----------------------------------------------------------------
            seq0_i(ii)  = seq0(i)    !< Old value of equivalent stress
            phi0(ii)    = uvar(i,1)  !< Old value of yield function
            norm0xx(ii) = uvar(i,2)  !< Old value of the normal component xx
            norm0yy(ii) = uvar(i,3)  !< Old value of the normal component yy
            norm0zz(ii) = uvar(i,4)  !< Old value of the normal component zz
            norm0xy(ii) = uvar(i,5)  !< Old value of the normal component xy
            norm0yz(ii) = uvar(i,6)  !< Old value of the normal component yz
            norm0zx(ii) = uvar(i,7)  !< Old value of the normal component zx
            dsigy_dpla_i(ii) = dsigy_dpla(i) !< Derivative of yield stress w.r.t. plastic strain
            epsd_i(ii)   = epsd(i)   !< Current value of plastic strain rate
            dtemp_dpla_i(ii) = dtemp_dpla(i) !< Derivative of temperature w.r.t. plastic strain
          enddo
!
          if (nvartmp > 0) then 
#include "vectorize.inc"
            do ii = 1,nindx
              i = indx(ii)  
              !< Temporary variables for tabulated phenomena
              vartmp_i(ii,1:nvartmp) = vartmp(i,1:nvartmp) 
            enddo
          endif
!
            !< Loop over yielding elements
#include "vectorize.inc"
            do ii = 1,nindx
              i = indx(ii)
!
            !< 1 - Derivative of equivalent stress sigeq w.r.t lambda
            !< -----------------------------------------------------------------
!
            !<  a) Derivatives of stress tensor w.r.t lambda
            !<  ----------------------------------------------------------------
            dsigxx_dlam(ii) =                                                  &
                        -(cstf(i,1,1)*norm0xx(ii) + cstf(i,1,2)*norm0yy(ii) +  &
                          cstf(i,1,3)*norm0zz(ii) + cstf(i,1,4)*norm0xy(ii) +  &
                          cstf(i,1,5)*norm0yz(ii) + cstf(i,1,6)*norm0zx(ii))
            dsigyy_dlam(ii) =                                                  &
                        -(cstf(i,2,1)*norm0xx(ii) + cstf(i,2,2)*norm0yy(ii) +  &
                          cstf(i,2,3)*norm0zz(ii) + cstf(i,2,4)*norm0xy(ii) +  &
                          cstf(i,2,5)*norm0yz(ii) + cstf(i,2,6)*norm0zx(ii))
            dsigzz_dlam(ii) =                                                  &
                        -(cstf(i,3,1)*norm0xx(ii) + cstf(i,3,2)*norm0yy(ii) +  &
                          cstf(i,3,3)*norm0zz(ii) + cstf(i,3,4)*norm0xy(ii) +  &
                          cstf(i,3,5)*norm0yz(ii) + cstf(i,3,6)*norm0zx(ii))
            dsigxy_dlam(ii) =                                                  & 
                        -(cstf(i,4,1)*norm0xx(ii) + cstf(i,4,2)*norm0yy(ii) +  &
                          cstf(i,4,3)*norm0zz(ii) + cstf(i,4,4)*norm0xy(ii) +  &
                          cstf(i,4,5)*norm0yz(ii) + cstf(i,4,6)*norm0zx(ii))
            dsigyz_dlam(ii) =                                                  &
                        -(cstf(i,5,1)*norm0xx(ii) + cstf(i,5,2)*norm0yy(ii) +  &
                          cstf(i,5,3)*norm0zz(ii) + cstf(i,5,4)*norm0xy(ii) +  &
                          cstf(i,5,5)*norm0yz(ii) + cstf(i,5,6)*norm0zx(ii))
            dsigzx_dlam(ii) =                                                  &
                       -(cstf(i,6,1)*norm0xx(ii) + cstf(i,6,2)*norm0yy(ii) +   &
                         cstf(i,6,3)*norm0zz(ii) + cstf(i,6,4)*norm0xy(ii) +   &
                         cstf(i,6,5)*norm0yz(ii) + cstf(i,6,6)*norm0zx(ii))
!
            !<  b) Assembling derivative of eq. stress sigeq w.r.t lambda
            !<  ----------------------------------------------------------------
            dseq_dlam(ii) =                                                    &
              norm0xx(ii)*dsigxx_dlam(ii) + norm0yy(ii)*dsigyy_dlam(ii) +      &
              norm0zz(ii)*dsigzz_dlam(ii) + norm0xy(ii)*dsigxy_dlam(ii) +      &
              norm0yz(ii)*dsigyz_dlam(ii) + norm0zx(ii)*dsigzx_dlam(ii) 
!
            !< 2 - Derivative of yield stress ystrs w.r.t lambda
            !< -----------------------------------------------------------------
!
            !<  a) Derivative of eq. plastic strain w.r.t lambda
            !<  ----------------------------------------------------------------
            sig_dseqdsig(ii) = sig0xx(i)*norm0xx(ii) + sig0yy(i)*norm0yy(ii) + &
                               sig0zz(i)*norm0zz(ii) + sig0xy(i)*norm0xy(ii) + &
                               sig0yz(i)*norm0yz(ii) + sig0zx(i)*norm0zx(ii)
            dpla_dlam(ii) = sig_dseqdsig(ii)/max(sigy(i),em20)
!
            !<  b) Assembling derivative of ystrs w.r.t lambda
            !<  ----------------------------------------------------------------
            dsigy_dlam(ii) = (one - chard)*dsigy_dpla_i(ii)*dpla_dlam(ii)
          enddo
!
          !< 3 - Add kinematic hardening to the derivative of eq.stress 
          !   w.r.t lambda
          !<  ------------------------------------------------------------------              
          if (ikine > 0) then
#include "vectorize.inc"
            do ii = 1,nindx
              i = indx(ii)
              sigb_i(ii,1:l_sigb) = sigb(i,1:l_sigb) !< Backstress tensor
            enddo
            !< a - Derivative of backstress tensor w.r.t lambda
            !<  ----------------------------------------------------------------
            call elasto_plastic_kinematic_hardening(                           &
              matparam ,nindx    ,l_sigb   ,dsigb_dlam(1:nindx,1:l_sigb),      &
              dsigy_dpla_i,chard ,                                             &
              norm0xx  ,norm0yy  ,norm0zz  ,norm0xy   ,norm0yz   ,norm0zx  ,   &
              dpla_dlam,sigb_i(1:nindx,1:l_sigb),ikine   )
            !< b - Assembling the backstress contribution to the derivative  
            !  of eq. stress w.r.t lambda
            !<  ----------------------------------------------------------------
            dsigbxx_dlam(1:nindx) = zero
            dsigbyy_dlam(1:nindx) = zero
            dsigbzz_dlam(1:nindx) = zero
            dsigbxy_dlam(1:nindx) = zero
            dsigbyz_dlam(1:nindx) = zero
            dsigbzx_dlam(1:nindx) = zero
            do j = 1, l_sigb/6
#include "vectorize.inc"
              do ii = 1, nindx
                dsigbxx_dlam(ii) = dsigbxx_dlam(ii) + dsigb_dlam(ii,6*(j-1) + 1)
                dsigbyy_dlam(ii) = dsigbyy_dlam(ii) + dsigb_dlam(ii,6*(j-1) + 2)
                dsigbzz_dlam(ii) = dsigbzz_dlam(ii) + dsigb_dlam(ii,6*(j-1) + 3)
                dsigbxy_dlam(ii) = dsigbxy_dlam(ii) + dsigb_dlam(ii,6*(j-1) + 4)
                dsigbyz_dlam(ii) = dsigbyz_dlam(ii) + dsigb_dlam(ii,6*(j-1) + 5)
                dsigbzx_dlam(ii) = dsigbzx_dlam(ii) + dsigb_dlam(ii,6*(j-1) + 6)
              enddo
            enddo
#include "vectorize.inc"
            do ii = 1, nindx
              dseq_dlam(ii) = dseq_dlam(ii) - norm0xx(ii)*dsigbxx_dlam(ii) -   &
                                              norm0yy(ii)*dsigbyy_dlam(ii) -   &
                                              norm0zz(ii)*dsigbzz_dlam(ii) -   &
                                              norm0xy(ii)*dsigbxy_dlam(ii) -   &
                                              norm0yz(ii)*dsigbyz_dlam(ii) -   &
                                              norm0zx(ii)*dsigbzx_dlam(ii)
            enddo
          endif
!         
#include "vectorize.inc"   
          do ii = 1,nindx
            i = indx(ii)
            !< 4 - Assembling the derivative of phi w.r.t lambda
            !< -----------------------------------------------------------------
!
            !<  a) Derivative of phi w.r.t eq. stress sigeq
            !<  ----------------------------------------------------------------
            dphi_dseq(ii)  =  two*seq0(i)/(sigy(i)**2)
!
            !<  b) Derivative of phi w.r.t yield stress ystrs
            !<  ----------------------------------------------------------------
            dphi_dsigy(ii) = -two*(seq0(i)**2)/(sigy(i)**3)
!
            !<  c) Derivative of phi w.r.t lambda
            !<  ----------------------------------------------------------------
            dphi_dlam(ii) = dphi_dseq(ii)*dseq_dlam(ii) +                      &
                            dphi_dsigy(ii)*dsigy_dlam(ii)
            dphi_dlam(ii) = sign(max(abs(dphi_dlam(ii)),em20),dphi_dlam(ii))
!
            !< 5 - Computation of plastic multiplier and variables update
            !< -----------------------------------------------------------------
!
            !<  a) Computation of the plastic multiplier increment dlam
            !<  ----------------------------------------------------------------
            !< Computation of the trial stress increment
            dsigxx(ii) = signxx(i) - sig0xx(i)
            dsigyy(ii) = signyy(i) - sig0yy(i)
            dsigzz(ii) = signzz(i) - sig0zz(i)
            dsigxy(ii) = signxy(i) - sig0xy(i)
            dsigyz(ii) = signyz(i) - sig0yz(i)
            dsigzx(ii) = signzx(i) - sig0zx(i)
            !< Computation of yield surface trial increment dphi       
            dphi(ii) = dphi_dseq(ii) * (norm0xx(ii) * dsigxx(ii)               &
                                      + norm0yy(ii) * dsigyy(ii)               &
                                      + norm0zz(ii) * dsigzz(ii)               &
                                      + norm0xy(ii) * dsigxy(ii)               &
                                      + norm0yz(ii) * dsigyz(ii)               &
                                      + norm0zx(ii) * dsigzx(ii) )
            !< Assembling plastic multiplier
            dlam(ii) = -(phi0(ii) + dphi(ii))/dphi_dlam(ii)
!
            !<  b) Stress tensor update
            !<  ----------------------------------------------------------------
            signxx_i(ii) = signxx(i) + dsigxx_dlam(ii)*dlam(ii)
            signyy_i(ii) = signyy(i) + dsigyy_dlam(ii)*dlam(ii)
            signzz_i(ii) = signzz(i) + dsigzz_dlam(ii)*dlam(ii)
            signxy_i(ii) = signxy(i) + dsigxy_dlam(ii)*dlam(ii)
            signyz_i(ii) = signyz(i) + dsigyz_dlam(ii)*dlam(ii)
            signzx_i(ii) = signzx(i) + dsigzx_dlam(ii)*dlam(ii)
!
            !<  c) Update the plastic strain related variables
            !<  ----------------------------------------------------------------
            !< Equivalent plastic strain increment
            dpla(i) = max(dpla(i) + dpla_dlam(ii)*dlam(ii),zero)
            !< Equivalent plastic strain
            pla_i(ii) = pla0(i) + dpla(i)
          enddo
!
          !< Temperature or heating generation update for /HEAT/MAT
          if (inloc == 0) then 
            if (jthe /= 0) then
#include "vectorize.inc"
              do ii = 1,nindx
                i = indx(ii) 
                fheat(i) = fheat(i) + sigy(i)*dpla_dlam(ii)*dlam(ii)*voln(i)
                temp_i(ii) = temp(i)
              enddo
            else
#include "vectorize.inc"
              do ii = 1,nindx
                i = indx(ii) 
                temp_i(ii) = temp(i) + dtemp_dpla_i(ii)*dpla_dlam(ii)*dlam(ii)
              enddo
            endif
          else
#include "vectorize.inc"
            do ii = 1,nindx
              i = indx(ii)
              temp_i(ii) = temp(i)
            enddo
          endif
!
          !<  d) Yield stress update
          !<  ------------------------------------------------------------------
          call elasto_plastic_yield_stress(                                    &
            matparam ,nindx    ,sigy_i   ,pla_i    ,epsd_i   ,dsigy_dpla_i,    &
            nvartmp  ,vartmp_i(1:nindx,1:nvartmp)  ,temp_i   ,dtemp_dpla_i,    &
            jthe  )
!
          !<  e) Backstress tensor update
          !<  ------------------------------------------------------------------
          !< Update of the backstress tensor (if kinematic hardening)
          if (ikine > 0) then
#include "vectorize.inc"   
            do ii = 1, nindx
              i = indx(ii)
              ! -> Remove kinematic hardening contribution
              signxx_i(ii) = signxx_i(ii) + sigbxx(i)
              signyy_i(ii) = signyy_i(ii) + sigbyy(i)
              signzz_i(ii) = signzz_i(ii) + sigbzz(i)
              signxy_i(ii) = signxy_i(ii) + sigbxy(i)
              signyz_i(ii) = signyz_i(ii) + sigbyz(i)
              signzx_i(ii) = signzx_i(ii) + sigbzx(i)
            enddo
!DIR$ IVDEP
            do ii = 1, nindx
              i = indx(ii)
              ! -> Add the evolution of backstress tensor
              sigbxx(i) = sigbxx(i) + dsigbxx_dlam(ii)*dlam(ii)
              sigbyy(i) = sigbyy(i) + dsigbyy_dlam(ii)*dlam(ii)
              sigbzz(i) = sigbzz(i) + dsigbzz_dlam(ii)*dlam(ii)
              sigbxy(i) = sigbxy(i) + dsigbxy_dlam(ii)*dlam(ii)
              sigbyz(i) = sigbyz(i) + dsigbyz_dlam(ii)*dlam(ii)
              sigbzx(i) = sigbzx(i) + dsigbzx_dlam(ii)*dlam(ii)
            enddo
#include "vectorize.inc"   
            do ii = 1, nindx
              i = indx(ii)
              ! -> Add the kinematic hardening contribution
              signxx_i(ii) = signxx_i(ii) - sigbxx(i)
              signyy_i(ii) = signyy_i(ii) - sigbyy(i)
              signzz_i(ii) = signzz_i(ii) - sigbzz(i)
              signxy_i(ii) = signxy_i(ii) - sigbxy(i)
              signyz_i(ii) = signyz_i(ii) - sigbyz(i)
              signzx_i(ii) = signzx_i(ii) - sigbzx(i)
              enddo
#include "vectorize.inc"   
            do ii = 1, nindx
              i = indx(ii)
              !< Update of the yield stress for kinematic hardening models
              sigy_i(ii) = (one - chard)*sigy_i(ii) + chard*sigy0(i)
            enddo
            ! -> Update of the backstress components
            do j = 1, l_sigb/6
!DIR$ IVDEP
#include "vectorize.inc"
              do ii = 1, nindx
                i = indx(ii)
                sigb(i,6*(j-1) + 1) = sigb(i,6*(j-1) + 1) +                    &
                                     dsigb_dlam(ii,6*(j-1) + 1)*dlam(ii)
                sigb(i,6*(j-1) + 2) = sigb(i,6*(j-1) + 2) +                    &
                                     dsigb_dlam(ii,6*(j-1) + 2)*dlam(ii)
                sigb(i,6*(j-1) + 3) = sigb(i,6*(j-1) + 3) +                    &
                                     dsigb_dlam(ii,6*(j-1) + 3)*dlam(ii)
                sigb(i,6*(j-1) + 4) = sigb(i,6*(j-1) + 4) +                    &
                                     dsigb_dlam(ii,6*(j-1) + 4)*dlam(ii)
                sigb(i,6*(j-1) + 5) = sigb(i,6*(j-1) + 5) +                    &
                                     dsigb_dlam(ii,6*(j-1) + 5)*dlam(ii)
                sigb(i,6*(j-1) + 6) = sigb(i,6*(j-1) + 6) +                    &
                                     dsigb_dlam(ii,6*(j-1) + 6)*dlam(ii)
              enddo
            enddo
          endif
!
          !<  f) Equivalent stress update
          !<  ------------------------------------------------------------------
          call elasto_plastic_eq_stress(                                       &
            matparam ,nindx    ,seq_i   ,iresp    ,eltype   ,                  &
            signxx_i ,signyy_i ,signzz_i,signxy_i ,signyz_i ,signzx_i,         &
            normxx_i ,normyy_i ,normzz_i,normxy_i ,normyz_i ,normzx_i,         &
            N        ,.false.  )
!
#include "vectorize.inc" 
          do ii = 1, nindx
            i = indx(ii)
!
            !<  g) Recopy of local variables to global arrays
            !<  ----------------------------------------------------------------
            signxx(i) = signxx_i(ii) !< Current value of stress component xx
            signyy(i) = signyy_i(ii) !< Current value of stress component yy
            signzz(i) = signzz_i(ii) !< Current value of stress component zz
            signxy(i) = signxy_i(ii) !< Current value of stress component xy
            signyz(i) = signyz_i(ii) !< Current value of stress component yz
            signzx(i) = signzx_i(ii) !< Current value of stress component zx
            seq(i)    = seq_i(ii)    !< Equivalent stress
            sigy(i)   = sigy_i(ii)   !< Yield stress
            pla(i)    = pla_i(ii)    !< Equivalent plastic strain
            temp(i)   = temp_i(ii)   !< Temperature
            normxx(i) = normxx_i(ii) !< Normal component xx 
            normyy(i) = normyy_i(ii) !< Normal component yy 
            normzz(i) = normzz_i(ii) !< Normal component zz 
            normxy(i) = normxy_i(ii) !< Normal component xy 
            normyz(i) = normyz_i(ii) !< Normal component yz 
            normzx(i) = normzx_i(ii) !< Normal component zx
          enddo
!
          if (nvartmp > 0) then
#include "vectorize.inc" 
            do ii = 1, nindx
              i = indx(ii)
              !< Temporary variables for tabulated phenomena
              vartmp(i,1:nvartmp) = vartmp_i(ii,1:nvartmp) 
            enddo
          endif   
!
#include "vectorize.inc" 
          do ii = 1, nindx
            i = indx(ii)  
            !<  h) Yield function update
            !<  ----------------------------------------------------------------
            phi(i) = (seq(i)/sigy(i))**2 - one
!
            !< Update the hourglass stabilization variable
            et(i) = dsigy_dpla_i(ii) / (dsigy_dpla_i(ii) + young(i))
          enddo
!
        endif
!
        !=======================================================================
        !< - Update filtered plastic strain rate
        !=======================================================================
        if (vpflag == 1) then 
          epsd(1:nel) = asrate*dpla(1:nel)/max(timestep,em20) +                &
                                   (one - asrate)*epsd(1:nel)
        endif
!
        !=======================================================================
        !< - Remove backstress contribution from stress tensor
        !=======================================================================        
        if (ikine > 0) then
          signxx(1:nel) = signxx(1:nel) + sigbxx(1:nel)
          signyy(1:nel) = signyy(1:nel) + sigbyy(1:nel)
          signzz(1:nel) = signzz(1:nel) + sigbzz(1:nel)
          signxy(1:nel) = signxy(1:nel) + sigbxy(1:nel)
          signyz(1:nel) = signyz(1:nel) + sigbyz(1:nel)
          signzx(1:nel) = signzx(1:nel) + sigbzx(1:nel)
        endif
!        
        !=======================================================================
        !< - Save remaining error after return mapping
        !=======================================================================
        uvar(1:nel,1) = phi(1:nel)
        uvar(1:nel,2) = normxx(1:nel)
        uvar(1:nel,3) = normyy(1:nel)
        uvar(1:nel,4) = normzz(1:nel)
        uvar(1:nel,5) = normxy(1:nel)
        uvar(1:nel,6) = normyz(1:nel)
        uvar(1:nel,7) = normzx(1:nel)
!
        !=======================================================================
        !< - Equation of state coupling for solids
        !=======================================================================       
        if (ieos > 0) then
          sigm(1:nel) = (signxx(1:nel) + signyy(1:nel) + signzz(1:nel))/three
          signxx(1:nel) = signxx(1:nel) - sigm(1:nel)
          signyy(1:nel) = signyy(1:nel) - sigm(1:nel)
          signzz(1:nel) = signzz(1:nel) - sigm(1:nel)
        endif
!
       end subroutine nice_solids
       end module nice_solids_mod