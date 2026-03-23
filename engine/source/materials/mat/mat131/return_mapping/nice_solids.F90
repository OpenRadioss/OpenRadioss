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
        jthe     ,fheat    ,voln     )
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
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i,j,ii,nindx,indx(nel),vpflag,idev,ikine
        real(kind=WP), dimension(nel,6,6) :: cstf,N
        real(kind=WP) :: dlam,dsigxx_dlam,dsigyy_dlam,dsigzz_dlam,             &
          dsigxy_dlam,dsigyz_dlam,dsigzx_dlam,dseq_dlam,dpla_dlam,             &
          dphi_dseq,dphi_dsigy,dphi_dlam,sig_dseqdsig,dsigy_dlam,              &
          dsigbxx_dlam,dsigbyy_dlam,dsigbzz_dlam,dsigbxy_dlam,                 &
          dsigbyz_dlam,dsigbzx_dlam,chard,dphi
        real(kind=WP), dimension(nel) :: pla0,normxx,normyy,normzz,            &
          normxy,normyz,normzx,phi,young,dsigy_dpla,dtemp_dpla,s13,s23,s43,    &
          shf,sigbxx,sigbyy,sigbzz,sigbxy,sigbyz,sigbzx,sigy0,dsigy0_dpla,     &
          dtemp0_dpla,zeros,dsigxx,dsigyy,dsigzz,dsigxy,dsigyz,dsigzx,phi0,    &
          sig0xx,sig0yy,sig0zz,sig0xy,sig0yz,sig0zx,seq0,norm0xx,norm0yy,      &
          norm0zz,norm0xy,norm0yz,norm0zx,sigm
        real(kind=WP), dimension(nel,l_sigb) :: dsigb_dlam
        integer, dimension(nel,nvartmp) :: ipos0
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
        !< Viscoplastic formulation flag
        vpflag = matparam%iparam(10)
        !< Total or deviatoric strain rate for scaled yield stress formulation
        if (vpflag > 1 .and. vpflag < 3) then
          idev = vpflag - 2
          call mstrain_rate(                                                   &
            nel      ,israte   ,asrate   ,epsd     ,idev     ,                 &
            epspxx   ,epspyy   ,epspzz   ,epspxy   ,epspyz   ,epspzx   )
        endif
        !< Kinematic hardening flag
        ikine = matparam%iparam(22)
        !< Mixed kinematic/isotropic hardening parameter
        chard = matparam%uparam(matparam%iparam(20) + 1)
        !< Initialisation of the hourglass control variable
        et(1:nel) = one
        !< Increment of cumulated plastic strain
        dpla(1:nel) = zero
        !< Derivative of temperature w.r.t. cumulated plastic strain
        dtemp_dpla(1:nel) = zero
        !< Save the initial cumulated plastic strain value
        pla0(1:nel) = pla(1:nel)
        !< Save initial values of the stress tensor components for shell elements
        sig0xx(1:nel) = sigoxx(1:nel)
        sig0yy(1:nel) = sigoyy(1:nel)
        sig0zz(1:nel) = sigozz(1:nel)
        sig0xy(1:nel) = sigoxy(1:nel)
        sig0yz(1:nel) = sigoyz(1:nel)
        sig0zx(1:nel) = sigozx(1:nel)
        !< Recover previous value of the yield function
        phi0(1:nel) = uvar(1:nel,1)
        !< Recover previous value of the yield function
        seq0(1:nel) = seq(1:nel)
        phi0(1:nel) = uvar(1:nel,1)
        norm0xx(1:nel) = uvar(1:nel,2)
        norm0yy(1:nel) = uvar(1:nel,3)
        norm0zz(1:nel) = uvar(1:nel,4)
        norm0xy(1:nel) = uvar(1:nel,5)
        norm0yz(1:nel) = uvar(1:nel,6)
        norm0zx(1:nel) = uvar(1:nel,7)
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
          dpdm     )
!
        !=======================================================================
        !< - Computation of the initial yield stress
        !=======================================================================
        call elasto_plastic_yield_stress(                                      &
          matparam ,nel      ,sigy     ,pla      ,epsd     ,dsigy_dpla,        &
          nvartmp  ,vartmp   ,temp     ,dtemp_dpla,jthe    )
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
          indx(1:nindx) = PACK(temp_all_indices(1:nel), active_elements_mask(1:nel))
!
          !< Loop over yielding elements
          do ii = 1,nindx
            i = indx(ii)
!
            !< 1 - Derivative of equivalent stress sigeq w.r.t lambda
            !< -----------------------------------------------------------------
!
            !<  a) Derivatives of stress tensor w.r.t lambda
            !<  ----------------------------------------------------------------
            dsigxx_dlam = -(cstf(i,1,1)*norm0xx(i) + cstf(i,1,2)*norm0yy(i) +  &
                            cstf(i,1,3)*norm0zz(i) + cstf(i,1,4)*norm0xy(i) +  &
                            cstf(i,1,5)*norm0yz(i) + cstf(i,1,6)*norm0zx(i))
            dsigyy_dlam = -(cstf(i,2,1)*norm0xx(i) + cstf(i,2,2)*norm0yy(i) +  &
                            cstf(i,2,3)*norm0zz(i) + cstf(i,2,4)*norm0xy(i) +  &
                            cstf(i,2,5)*norm0yz(i) + cstf(i,2,6)*norm0zx(i))
            dsigzz_dlam = -(cstf(i,3,1)*norm0xx(i) + cstf(i,3,2)*norm0yy(i) +  &
                            cstf(i,3,3)*norm0zz(i) + cstf(i,3,4)*norm0xy(i) +  &
                            cstf(i,3,5)*norm0yz(i) + cstf(i,3,6)*norm0zx(i))
            dsigxy_dlam = -(cstf(i,4,1)*norm0xx(i) + cstf(i,4,2)*norm0yy(i) +  &
                            cstf(i,4,3)*norm0zz(i) + cstf(i,4,4)*norm0xy(i) +  &
                            cstf(i,4,5)*norm0yz(i) + cstf(i,4,6)*norm0zx(i))
            dsigyz_dlam = -(cstf(i,5,1)*norm0xx(i) + cstf(i,5,2)*norm0yy(i) +  &
                            cstf(i,5,3)*norm0zz(i) + cstf(i,5,4)*norm0xy(i) +  &
                            cstf(i,5,5)*norm0yz(i) + cstf(i,5,6)*norm0zx(i))
            dsigzx_dlam = -(cstf(i,6,1)*norm0xx(i) + cstf(i,6,2)*norm0yy(i) +  &
                            cstf(i,6,3)*norm0zz(i) + cstf(i,6,4)*norm0xy(i) +  &
                            cstf(i,6,5)*norm0yz(i) + cstf(i,6,6)*norm0zx(i))
!
            !<  b) Assembling derivative of eq. stress sigeq w.r.t lambda
            !<  ----------------------------------------------------------------
            dseq_dlam = norm0xx(i)*dsigxx_dlam + norm0yy(i)*dsigyy_dlam +      &
                        norm0zz(i)*dsigzz_dlam + norm0xy(i)*dsigxy_dlam +      &
                        norm0yz(i)*dsigyz_dlam + norm0zx(i)*dsigzx_dlam
!
            !< 2 - Derivative of yield stress ystrs w.r.t lambda
            !< -----------------------------------------------------------------
!
            !<  a) Derivative of eq. plastic strain w.r.t lambda
            !<  ----------------------------------------------------------------
            sig_dseqdsig = sig0xx(i)*norm0xx(i) + sig0yy(i)*norm0yy(i) +       &
                           sig0zz(i)*norm0zz(i) + sig0xy(i)*norm0xy(i) +       &
                           sig0yz(i)*norm0yz(i) + sig0zx(i)*norm0zx(i)
            dpla_dlam = sig_dseqdsig/max(sigy(i),em20)
!
            !<  b) Assembling derivative of ystrs w.r.t lambda
            !<  ----------------------------------------------------------------
            dsigy_dlam = (one - chard)*dsigy_dpla(i)*dpla_dlam
!
            !< 3 - Add kinematic hardening to the derivative of eq.stress 
            !   w.r.t lambda
            !<  ----------------------------------------------------------------              
            if (ikine > 0) then
              !< a - Derivative of backstress tensor w.r.t lambda
              !<  --------------------------------------------------------------
              call elasto_plastic_kinematic_hardening(                         &
                matparam ,1     ,l_sigb ,dsigb_dlam(i,1),dsigy_dpla(i),chard,  &
                norm0xx(i),norm0yy(i),norm0zz(i),norm0xy(i),norm0yz(i),        &
                norm0zx(i),dpla_dlam,sigb(i,1)) 
              !< b - Assembling the backstress contribution to the derivative  
              !  of eq. stress w.r.t lambda
              !<  --------------------------------------------------------------
              dsigbxx_dlam = zero
              dsigbyy_dlam = zero
              dsigbzz_dlam = zero
              dsigbxy_dlam = zero
              dsigbyz_dlam = zero
              dsigbzx_dlam = zero
              do j = 1, l_sigb/6
                dsigbxx_dlam = dsigbxx_dlam + dsigb_dlam(i,6*(j-1) + 1)
                dsigbyy_dlam = dsigbyy_dlam + dsigb_dlam(i,6*(j-1) + 2)
                dsigbzz_dlam = dsigbzz_dlam + dsigb_dlam(i,6*(j-1) + 3)
                dsigbxy_dlam = dsigbxy_dlam + dsigb_dlam(i,6*(j-1) + 4)
                dsigbyz_dlam = dsigbyz_dlam + dsigb_dlam(i,6*(j-1) + 5)
                dsigbzx_dlam = dsigbzx_dlam + dsigb_dlam(i,6*(j-1) + 6)
              enddo
              dseq_dlam = dseq_dlam - norm0xx(i)*dsigbxx_dlam -                &
                                      norm0yy(i)*dsigbyy_dlam -                &
                                      norm0zz(i)*dsigbzz_dlam -                &
                                      norm0xy(i)*dsigbxy_dlam -                &
                                      norm0yz(i)*dsigbyz_dlam -                &
                                      norm0zx(i)*dsigbzx_dlam
            endif
!            
            !< 4 - Assembling the derivative of phi w.r.t lambda
            !< -----------------------------------------------------------------
!
            !<  a) Derivative of phi w.r.t eq. stress sigeq
            !<  ----------------------------------------------------------------
            dphi_dseq  =  two*seq0(i)/(sigy(i)**2)
!
            !<  b) Derivative of phi w.r.t yield stress ystrs
            !<  ----------------------------------------------------------------
            dphi_dsigy = -two*(seq0(i)**2)/(sigy(i)**3)
!
            !<  c) Derivative of phi w.r.t lambda
            !<  ----------------------------------------------------------------
            dphi_dlam = dphi_dseq*dseq_dlam + dphi_dsigy*dsigy_dlam
            dphi_dlam = sign(max(abs(dphi_dlam),em20),dphi_dlam)
!
            !< 5 - Computation of plastic multiplier and variables update
            !< -----------------------------------------------------------------
!
            !<  a) Computation of the plastic multiplier increment dlam
            !<  ----------------------------------------------------------------
            !< Computation of the trial stress increment
            dsigxx(i) = signxx(i) - sig0xx(i)
            dsigyy(i) = signyy(i) - sig0yy(i)
            dsigzz(i) = signzz(i) - sig0zz(i)
            dsigxy(i) = signxy(i) - sig0xy(i)
            dsigyz(i) = signyz(i) - sig0yz(i)
            dsigzx(i) = signzx(i) - sig0zx(i)
            !< Computation of yield surface trial increment dphi       
            dphi = dphi_dseq * (norm0xx(i) * dsigxx(i)                         &
                              + norm0yy(i) * dsigyy(i)                         &
                              + norm0zz(i) * dsigzz(i)                         &
                              + norm0xy(i) * dsigxy(i)                         &
                              + norm0yz(i) * dsigyz(i)                         &
                              + norm0zx(i) * dsigzx(i) )
            !< Assembling plastic multiplier
            dlam = -(phi0(i) + dphi)/dphi_dlam
!
            !<  b) Stress tensor update
            !<  ----------------------------------------------------------------
            signxx(i) = signxx(i) + dsigxx_dlam*dlam
            signyy(i) = signyy(i) + dsigyy_dlam*dlam
            signzz(i) = signzz(i) + dsigzz_dlam*dlam
            signxy(i) = signxy(i) + dsigxy_dlam*dlam
            signyz(i) = signyz(i) + dsigyz_dlam*dlam
            signzx(i) = signzx(i) + dsigzx_dlam*dlam
!
            !<  c) Update the plastic strain related variables
            !<  ----------------------------------------------------------------
            !< Equivalent plastic strain increment
            dpla(i) = max(dpla(i) + dpla_dlam*dlam,zero)
            !< Equivalent plastic strain
            pla(i)  = pla0(i) + dpla(i)
            !< Temperature or heating generation update for /HEAT/MAT
            if (jthe /= 0) then
              fheat(i) = fheat(i) + sigy(i)*dpla_dlam*dlam*voln(i)
            else
              temp(i)  = temp(i)  + dtemp_dpla(i)*dpla_dlam*dlam
            endif
!
            !<  d) Yield stress update
            !<  ----------------------------------------------------------------
            call elasto_plastic_yield_stress(                                  &
              matparam ,1        ,sigy(i)  ,pla(i)   ,epsd(i) ,dsigy_dpla(i),  &
              nvartmp  ,vartmp(i,1:nvartmp),temp(i) ,dtemp_dpla(i),jthe     )
!
            !<  e) Backstress tensor update
            !<  ----------------------------------------------------------------
            !< Update of the backstress tensor (if kinematic hardening)
            if (ikine > 0) then
              ! -> Remove kinematic hardening contribution
              signxx(i) = signxx(i) + sigbxx(i)
              signyy(i) = signyy(i) + sigbyy(i)
              signzz(i) = signzz(i) + sigbzz(i)
              signxy(i) = signxy(i) + sigbxy(i)
              signyz(i) = signyz(i) + sigbyz(i)
              signzx(i) = signzx(i) + sigbzx(i)
              ! -> Add the evolution of backstress tensor
              sigbxx(i) = sigbxx(i) + dsigbxx_dlam*dlam
              sigbyy(i) = sigbyy(i) + dsigbyy_dlam*dlam
              sigbzz(i) = sigbzz(i) + dsigbzz_dlam*dlam
              sigbxy(i) = sigbxy(i) + dsigbxy_dlam*dlam
              sigbyz(i) = sigbyz(i) + dsigbyz_dlam*dlam
              sigbzx(i) = sigbzx(i) + dsigbzx_dlam*dlam
              ! -> Add the kinematic hardening contribution
              signxx(i) = signxx(i) - sigbxx(i)
              signyy(i) = signyy(i) - sigbyy(i)
              signzz(i) = signzz(i) - sigbzz(i)
              signxy(i) = signxy(i) - sigbxy(i)
              signyz(i) = signyz(i) - sigbyz(i)
              signzx(i) = signzx(i) - sigbzx(i)
              ! -> Update of the backstress components
              do j = 1, l_sigb/6
                sigb(i,6*(j-1) + 1) = sigb(i,6*(j-1) + 1) +                    &
                                     dsigb_dlam(i,6*(j-1) + 1)*dlam
                sigb(i,6*(j-1) + 2) = sigb(i,6*(j-1) + 2) +                    &
                                     dsigb_dlam(i,6*(j-1) + 2)*dlam
                sigb(i,6*(j-1) + 3) = sigb(i,6*(j-1) + 3) +                    &
                                     dsigb_dlam(i,6*(j-1) + 3)*dlam
                sigb(i,6*(j-1) + 4) = sigb(i,6*(j-1) + 4) +                    &
                                     dsigb_dlam(i,6*(j-1) + 4)*dlam
                sigb(i,6*(j-1) + 5) = sigb(i,6*(j-1) + 5) +                    &
                                     dsigb_dlam(i,6*(j-1) + 5)*dlam
                sigb(i,6*(j-1) + 6) = sigb(i,6*(j-1) + 6) +                    &
                                     dsigb_dlam(i,6*(j-1) + 6)*dlam
              enddo
              !< Update of the yield stress for kinematic hardening models
              sigy(i) = (one - chard)*sigy(i) + chard*sigy0(i)
            endif
!
            !<  f) Equivalent stress update
            !<  ----------------------------------------------------------------
            call elasto_plastic_eq_stress(                                     &
              matparam ,1        ,seq(i)   ,iresp    ,eltype   ,               &
              signxx(i),signyy(i),signzz(i),signxy(i),signyz(i),signzx(i),     &
              normxx(i),normyy(i),normzz(i),normxy(i),normyz(i),normzx(i),     &
              N(i,1,1) ,.false.  )
!
            !<  g) Yield function update
            !<  ----------------------------------------------------------------
            phi(i) = (seq(i)/sigy(i))**2 - one
!
            !< Update the hourglass stabilization variable
            et(i) = dsigy_dpla(i) / (dsigy_dpla(i) + young(i))
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