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
!||    cutting_plane_shells_mod   ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_shells.F90
!||--- called by ------------------------------------------------------
!||    sigeps131c                 ../engine/source/materials/mat/mat131/sigeps131c.F90
!||====================================================================
      module cutting_plane_shells_mod
      contains
!||====================================================================
!||    cutting_plane_shells                     ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_shells.F90
!||--- called by ------------------------------------------------------
!||    sigeps131c                               ../engine/source/materials/mat/mat131/sigeps131c.F90
!||--- calls      -----------------------------------------------------
!||    elasto_plastic_eq_stress                 ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||    elasto_plastic_kinematic_hardening       ../engine/source/materials/mat/mat131/elasto_plastic_kinematic_hardening.F90
!||    elasto_plastic_trial_stress              ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||    elasto_plastic_yield_stress              ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                             ../common_source/modules/constant_mod.F
!||    elasto_plastic_eq_stress_mod             ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||    elasto_plastic_kinematic_hardening_mod   ../engine/source/materials/mat/mat131/elasto_plastic_kinematic_hardening.F90
!||    elasto_plastic_trial_stress_mod          ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||    elasto_plastic_yield_stress_mod          ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||    matparam_def_mod                         ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                            ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine cutting_plane_shells(                                         &
        nel      ,matparam ,rho      ,nvartmp  ,vartmp   ,                     &
        depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,                     &
        sigoxx   ,sigoyy   ,sigoxy   ,sigoyz   ,sigozx   ,                     &
        signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,                     &
        soundsp  ,off      ,pla      ,dpla     ,seq      ,et       ,           &
        sigy     ,timestep ,epsd     ,temp     ,shf      ,thk      ,           &
        thkly    ,asrate   ,l_sigb   ,sigb     ,epsd_pg  ,                     &
        nuvar    ,uvar     ,inloc    ,dplanl   )
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
        integer,                       intent(in)    :: nvartmp   !< Number of variables used in tabulated variables
        integer, dimension(nel,nvartmp), intent(inout) :: vartmp  !< Temporary variables for tabulated hardening
        real(kind=WP), dimension(nel), intent(in)    :: depsxx    !< Strain increment xx
        real(kind=WP), dimension(nel), intent(in)    :: depsyy    !< Strain increment yy
        real(kind=WP), dimension(nel), intent(in)    :: depsxy    !< Strain increment xy
        real(kind=WP), dimension(nel), intent(in)    :: depsyz    !< Strain increment yz
        real(kind=WP), dimension(nel), intent(in)    :: depszx    !< Strain increment zx
        real(kind=WP), dimension(nel), intent(in)    :: sigoxx    !< Previous stress xx
        real(kind=WP), dimension(nel), intent(in)    :: sigoyy    !< Previous stress yy
        real(kind=WP), dimension(nel), intent(in)    :: sigoxy    !< Previous stress xy
        real(kind=WP), dimension(nel), intent(in)    :: sigoyz    !< Previous stress yz
        real(kind=WP), dimension(nel), intent(in)    :: sigozx    !< Previous stress zx
        real(kind=WP), dimension(nel), intent(inout) :: signxx    !< Current stress xx
        real(kind=WP), dimension(nel), intent(inout) :: signyy    !< Current stress yy
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
        real(kind=WP), dimension(nel), intent(inout) :: temp      !< Temperature
        real(kind=WP), dimension(nel), intent(in)    :: shf       !< Shear correction factor
        real(kind=WP), dimension(nel), intent(inout) :: thk       !< Current thickness
        real(kind=WP), dimension(nel), intent(in)    :: thkly     !< Integration point layer thickness
        real(kind=WP),                 intent(in)    :: asrate    !< Strain rate filtering weighting factor
        integer,                       intent(in)    :: l_sigb    !< Size of backstress array
        real(kind=WP),dimension(nel,l_sigb),intent(inout) :: sigb !< Backstress components for kinematic hardening
        real(kind=WP),dimension(nel),  intent(in)    :: epsd_pg   !< Global equivalent strain rate
        integer,                       intent(in)    :: nuvar     !< Number of user variables
        real(kind=WP),dimension(nel,nuvar), intent(inout) :: uvar  !< User variables
        integer,                       intent(in)    :: inloc     !< Non-local reguarization flag
        real(kind=WP), dimension(nel), intent(in)    :: dplanl    !< Non-local plastic strain increment
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i,j,ii,iter,nindx,indx(nel),vpflag,ikine
        real(kind=WP), dimension(nel,6,6) :: cstf,N
        real(kind=WP) :: dlam,dsigxx_dlam,dsigyy_dlam,dsigxy_dlam,dseq_dlam,   &
          dpla_dlam,dphi_dseq,dphi_dsigy,dphi_dlam,sig_dseqdsig,dsigy_dlam,    &
          dsigbxx_dlam,dsigbyy_dlam,dsigbzz_dlam,dsigbxy_dlam,chard,dlam_nl
        real(kind=WP), dimension(nel) :: pla0,normxx,normyy,normzz,normxy,     &
          normyz,normzx,phi,young,dsigy_dpla,dtemp_dpla,s13,s23,depzz,         &
          sigbxx,sigbyy,sigbzz,sigbxy,sigy0,dsigy0_dpla,dtemp0_dpla,zeros
        real(kind=WP), dimension(nel,l_sigb) :: dsigb_dlam
        real(kind=WP), dimension(nel) :: signzz,sigozz,depszz,dezz
        integer, dimension(nel,nvartmp) :: ipos0
!
        integer, parameter :: eltype = 2               !< Element type (1 - Solids, 2 - Shells)
        integer, parameter :: nitermax = 20            !< Maximum number of plastic iterations
        real(kind=WP), parameter :: tol = 1.0d-6       !< Tolerance for plasticity convergence
        integer, parameter :: iresp = 0                !< Response type (0 - standard)

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
        !< Total strain-rate computation
        if (vpflag > 1) then
          epsd(1:nel) = asrate*epsd_pg(1:nel) + (one-asrate)*epsd(1:nel)
        !< Plastic strain rate recovering
        else
          epsd(1:nel) = uvar(1:nel,1)
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
        !< Initialize out-of-plane plastic strain increment for shell elements
        depzz(1:nel) = zero
        !< Initialize dummy stress components for shell elements
        signzz(1:nel) = zero
        sigozz(1:nel) = zero
        depszz(1:nel) = zero
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
          eltype   ,shf      ,s13      ,s23      )
!
        !=======================================================================
        !< - Computation of the initial yield stress
        !=======================================================================
        call elasto_plastic_yield_stress(                                      &
          matparam ,nel      ,sigy     ,pla      ,epsd     ,dsigy_dpla,        &
          nvartmp  ,vartmp   ,temp     ,dtemp_dpla)
!
        !=======================================================================
        !< - Backstress tensor computation for kinematic hardening models
        !=======================================================================
        if (ikine > 0) then
          sigbxx(1:nel) = zero
          sigbyy(1:nel) = zero
          sigbzz(1:nel) = zero
          sigbxy(1:nel) = zero
            !< Compute the backstress tensor from all C-R kinematic hardenings
            do j = 1, l_sigb/6
            sigbxx(1:nel) = sigbxx(1:nel) + sigb(1:nel,6*(j-1) + 1)
            sigbyy(1:nel) = sigbyy(1:nel) + sigb(1:nel,6*(j-1) + 2)
            sigbzz(1:nel) = sigbzz(1:nel) + sigb(1:nel,6*(j-1) + 3)
            sigbxy(1:nel) = sigbxy(1:nel) + sigb(1:nel,6*(j-1) + 4)
          enddo
          !< Add the kinematic hardening contribution to stress tensors
          signxx(1:nel) = signxx(1:nel) - (sigbxx(1:nel) - sigbzz(1:nel))
          signyy(1:nel) = signyy(1:nel) - (sigbyy(1:nel) - sigbzz(1:nel))
          signxy(1:nel) = signxy(1:nel) -  sigbxy(1:nel)
          !< Initial yield stress computation for kinematic hardening models
          zeros(1:nel) = zero
          ipos0(1:nel,1:nvartmp) = 0
          call elasto_plastic_yield_stress(                                    &
            matparam ,nel      ,sigy0    ,zeros    ,epsd     ,dsigy0_dpla,     &
            nvartmp  ,ipos0    ,temp     ,dtemp0_dpla)
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
        ! nindx  = 0
        ! phi(1:nel) = (seq(1:nel)/sigy(1:nel))**2 - one
        ! do i=1,nel
        !   if (phi(i) >= zero .and. off(i) == one) then
        !     nindx = nindx + 1
        !     indx(nindx)  = i
        !   else
        !     phi(i) = zero
        !   endif
        ! enddo
        phi(1:nel) = (seq(1:nel) / sigy(1:nel))**2 - one
        active_elements_mask(1:nel) = (phi(1:nel) >= zero .and. off(1:nel) == one)
        nindx = COUNT(active_elements_mask(1:nel))
        temp_all_indices(1:nel) = [(i, i=1,nel)]
!
        !=======================================================================
        !< - Return mapping algorithm using Cutting Plane Method (C.P.M)
        !=======================================================================
        if (nindx > 0) then
!
          ! Note : in this part, the purpose is to compute for each iteration
          ! a plastic multiplier (lambda) allowing to update internal
          ! variables to satisfy the consistency condition using the cutting
          ! plane algorithm.
          ! Its expression at each iteration is: dlam = - phi/dphi_dlam
          ! -> phi       : current value of yield function (known)
          ! -> dphi_dlam : derivative of phi with respect to dlam by taking
          !                into account of internal variables kinetic:
          !                plasticity, strain-rate ... (to be computed)
          indx(1:nindx) = PACK(temp_all_indices(1:nel), active_elements_mask(1:nel))
!
          !< Loop over yielding elements
          do ii = 1,nindx
            i = indx(ii)
!
            !< Initialisation of the iteration counter
            iter = 0
!
            !< Iterative plastic correction procedure
            do while ((abs(phi(i))>=tol).and.(iter<=nitermax))
!
              !< 1 - Derivative of equivalent stress sigeq w.r.t lambda
              !< ---------------------------------------------------------------
!
              !<  a) Derivatives of stress tensor w.r.t lambda
              !<  --------------------------------------------------------------
              dsigxx_dlam = -(cstf(i,1,1)*normxx(i) + cstf(i,1,2)*normyy(i))
              dsigyy_dlam = -(cstf(i,1,2)*normxx(i) + cstf(i,2,2)*normyy(i))
              dsigxy_dlam = -(cstf(i,4,4)*normxy(i))
!             
              !<  b) Assembling derivative of eq. stress sigeq w.r.t lambda
              !<  --------------------------------------------------------------
              dseq_dlam = normxx(i)*dsigxx_dlam + normyy(i)*dsigyy_dlam +      &
                          normxy(i)*dsigxy_dlam
!
              !< 2 - Derivative of yield stress ystrs w.r.t lambda
              !< ---------------------------------------------------------------
!
              !<  a) Derivative of eq. plastic strain w.r.t lambda
              !<  --------------------------------------------------------------
              sig_dseqdsig = signxx(i)*normxx(i) +                             & 
                             signyy(i)*normyy(i) +                             &
                             signxy(i)*normxy(i)
              dpla_dlam = sig_dseqdsig/max(sigy(i),em20)
!
              !<  b) Assembling derivative of ystrs w.r.t lambda
              !<  --------------------------------------------------------------
              dsigy_dlam = (one - chard)*dsigy_dpla(i)*dpla_dlam
!
              !< 3 - Add kinematic hardening to the derivative of eq.stress 
              !   w.r.t lambda
              !<  --------------------------------------------------------------              
              if (ikine > 0) then
                !< a - Derivative of backstress tensor w.r.t lambda
                !<  ------------------------------------------------------------
                call elasto_plastic_kinematic_hardening(                       &
                  matparam ,1     ,l_sigb ,dsigb_dlam(i,1),dsigy_dpla(i),chard,&
                  normxx(i),normyy(i),normzz(i),normxy(i),normyz(i),normzx(i), &
                  dpla_dlam,sigb(i,1)) 
                !< b - Assembling the backstress contribution to the derivative  
                !  of eq. stress w.r.t lambda
                !<  ------------------------------------------------------------
                dsigbxx_dlam = zero
                dsigbyy_dlam = zero
                dsigbzz_dlam = zero
                dsigbxy_dlam = zero
                do j = 1, l_sigb/6
                  dsigbxx_dlam = dsigbxx_dlam + dsigb_dlam(i,6*(j-1) + 1)
                  dsigbyy_dlam = dsigbyy_dlam + dsigb_dlam(i,6*(j-1) + 2)
                  dsigbzz_dlam = dsigbzz_dlam + dsigb_dlam(i,6*(j-1) + 3)
                  dsigbxy_dlam = dsigbxy_dlam + dsigb_dlam(i,6*(j-1) + 4)
                enddo
                dseq_dlam = dseq_dlam - normxx(i)*dsigbxx_dlam -               &
                                        normyy(i)*dsigbyy_dlam +               &
                            (normxx(i)+normyy(i))*dsigbzz_dlam -               &
                                        normxy(i)*dsigbxy_dlam
              endif
!
              !< 4 - Assembling the derivative of phi w.r.t lambda
              !< ---------------------------------------------------------------
!
              !<  a) Derivative of phi w.r.t eq. stress sigeq
              !<  --------------------------------------------------------------
              dphi_dseq  =  two*seq(i)/(sigy(i)**2)
!
              !<  b) Derivative of phi w.r.t yield stress ystrs
              !<  --------------------------------------------------------------
              dphi_dsigy = -two*(seq(i)**2)/(sigy(i)**3)
!
              !<  c) Derivative of phi w.r.t lambda
              !<  --------------------------------------------------------------
              dphi_dlam = dphi_dseq*dseq_dlam + dphi_dsigy*dsigy_dlam
              dphi_dlam = sign(max(abs(dphi_dlam),em20),dphi_dlam)
!
              !< 5 - Computation of plastic multiplier and variables update
              !< ---------------------------------------------------------------
!
              !<  a) Computation of the plastic multiplier increment dlam
              !<  --------------------------------------------------------------
              dlam = -phi(i)/dphi_dlam
!
              !<  b) Stress tensor update
              !<  --------------------------------------------------------------
              signxx(i) = signxx(i) + dsigxx_dlam*dlam
              signyy(i) = signyy(i) + dsigyy_dlam*dlam
              signxy(i) = signxy(i) + dsigxy_dlam*dlam
!
              !<  c) Update the plastic strain related variables
              !<  --------------------------------------------------------------
              !< Equivalent plastic strain increment
              dpla(i) = max(dpla(i) + dpla_dlam*dlam,zero)
              !< Equivalent plastic strain
              pla(i)  = pla0(i) + dpla(i)
              !< Temperature
              temp(i) = temp(i) + dtemp_dpla(i)*dpla_dlam*dlam
              !< Out-of-plane plastic strain for shell elements
              depzz(i) = depzz(i) + dlam*normzz(i)
!
              !<  d) Yield stress update
              !<  --------------------------------------------------------------
              call elasto_plastic_yield_stress(                                &
                matparam ,1        ,sigy(i)  ,pla(i)   ,epsd(i) ,dsigy_dpla(i),&
                nvartmp  ,vartmp(i,1:nvartmp),temp(i) ,dtemp_dpla(i))
!
              !<  e) Backstress tensor update
              !<  --------------------------------------------------------------
              !< Update of the backstress tensor (if kinematic hardening)
              if (ikine > 0) then
                ! -> Remove kinematic hardening contribution
                signxx(i) = signxx(i) + (sigbxx(i) - sigbzz(i))
                signyy(i) = signyy(i) + (sigbyy(i) - sigbzz(i))
                signxy(i) = signxy(i) + sigbxy(i)
                ! -> Add the evolution of backstress tensor
                sigbxx(i) = sigbxx(i) + dsigbxx_dlam*dlam
                sigbyy(i) = sigbyy(i) + dsigbyy_dlam*dlam
                sigbzz(i) = sigbzz(i) + dsigbzz_dlam*dlam
                sigbxy(i) = sigbxy(i) + dsigbxy_dlam*dlam
                ! -> Add the kinematic hardening contribution
                signxx(i) = signxx(i) - (sigbxx(i) - sigbzz(i))
                signyy(i) = signyy(i) - (sigbyy(i) - sigbzz(i))
                signxy(i) = signxy(i) - sigbxy(i)
                ! -> Update of the backstress components
                do j = 1, l_sigb/6
                  sigb(i,6*(j-1) + 1) = sigb(i,6*(j-1) + 1) +                  &
                                       dsigb_dlam(i,6*(j-1) + 1)*dlam
                  sigb(i,6*(j-1) + 2) = sigb(i,6*(j-1) + 2) +                  &
                                       dsigb_dlam(i,6*(j-1) + 2)*dlam
                  sigb(i,6*(j-1) + 3) = sigb(i,6*(j-1) + 3) +                  &
                                       dsigb_dlam(i,6*(j-1) + 3)*dlam
                  sigb(i,6*(j-1) + 4) = sigb(i,6*(j-1) + 4) +                  &
                                       dsigb_dlam(i,6*(j-1) + 4)*dlam
                enddo
                !< Update of the yield stress for kinematic hardening models
                sigy(i) = (one - chard)*sigy(i) + chard*sigy0(i)
              endif
!
              !<  f) Equivalent stress update
              !<  --------------------------------------------------------------
              call elasto_plastic_eq_stress(                                   &
                matparam ,1        ,seq(i)   ,iresp    ,eltype   ,             &
                signxx(i),signyy(i),signzz(i),signxy(i),signyz(i),signzx(i),   &
                normxx(i),normyy(i),normzz(i),normxy(i),normyz(i),normzx(i),   &
                N(i,1,1) ,.false.  )
!
              !<  g) Yield function update
              !<  --------------------------------------------------------------
              phi(i) = (seq(i)/sigy(i))**2 - one
!
              !<  h) Update iterations number
              !<  --------------------------------------------------------------
              iter = iter + 1
            enddo
          enddo
!
          !< Update the hourglass stabilization variable
          where (active_elements_mask(1:nel))
            et(1:nel) = dsigy_dpla(1:nel) / (dsigy_dpla(1:nel) + young(1:nel))
          end where
!
        endif
!
        !=======================================================================
        !< - Update filtered plastic strain rate
        !=======================================================================
        if (vpflag == 1) then 
          epsd(1:nel) = asrate*dpla(1:nel)/max(timestep,em20) +                &
                                 (one - asrate)*uvar(1:nel,1)
          uvar(1:nel,1) = epsd(1:nel)
        endif
!
        !=======================================================================
        !< - Remove backstress contribution from stress tensor
        !=======================================================================        
        if (ikine > 0) then
          signxx(1:nel) = signxx(1:nel) + (sigbxx(1:nel) - sigbzz(1:nel))
          signyy(1:nel) = signyy(1:nel) + (sigbyy(1:nel) - sigbzz(1:nel))
          signxy(1:nel) = signxy(1:nel) +  sigbxy(1:nel)
        endif
!
        !=======================================================================
        !< Update thickness for shell elements
        !=======================================================================
        if (inloc == 0) then 
          dezz(1:nel) = s13(1:nel)*(signxx(1:nel) - sigoxx(1:nel)) +           &
                        s23(1:nel)*(signyy(1:nel) - sigoyy(1:nel)) +           &
                        depzz(1:nel)
          thk(1:nel)  = thk(1:nel) + dezz(1:nel)*thkly(1:nel)*off(1:nel) 
        else
          do i = 1, nel 
            if (off(i) == one) then 
              !< Computation of the non-local plastic multiplier for shell elements
              sig_dseqdsig = signxx(i)*normxx(i) +                             & 
                             signyy(i)*normyy(i) +                             &
                             signxy(i)*normxy(i)
              if (sig_dseqdsig > em01) then 
                dlam_nl = sigy(i)*dplanl(i)/sig_dseqdsig
              else
                dlam_nl = zero
              endif
              !< Update the thickness variation
              dezz(i) = s13(i)*(cstf(i,1,1)*(depsxx(i) - dlam_nl*normxx(i))  + &
                                cstf(i,1,2)*(depsyy(i) - dlam_nl*normyy(i))) + &
                        s23(i)*(cstf(i,2,1)*(depsxx(i) - dlam_nl*normxx(i))  + &
                                cstf(i,2,2)*(depsyy(i) - dlam_nl*normyy(i)))   &
                        + dlam_nl*normzz(i)
              thk(i)  = thk(i) + dezz(i)*thkly(i)*off(i)
            endif
          enddo
        endif
!
       end subroutine cutting_plane_shells
       end module cutting_plane_shells_mod