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
!||    cutting_plane_solids_mod   ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||--- called by ------------------------------------------------------
!||    sigeps131                  ../engine/source/materials/mat/mat131/sigeps131.F90
!||====================================================================
      module cutting_plane_solids_mod
! \brief Cutting plane return mapping for solids in /MAT/LAW131
! \details Perform the cutting plane semi-implicit return mapping algorithm
!          for solid elements in /MAT/LAW131.
      contains
!||====================================================================
!||    cutting_plane_solids                     ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
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
      subroutine cutting_plane_solids(                                         &
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
        integer :: i,j,ii,iter(nel),nindx,indx(nel),idev,nindx_1,indx_1(nel)
        real(kind=WP), dimension(nel,6,6) :: cstf,N
        real(kind=WP), dimension(nel) :: pla0,normxx,normyy,normzz,normxy,     &
          normyz,normzx,phi,young,dsigy_dpla,dtemp_dpla,s13,s23,s43,shf,       &
          sigbxx,sigbyy,sigbzz,sigbxy,sigbyz,sigbzx,sigy0,dsigy0_dpla,         &
          dtemp0_dpla,zeros,sigm,dlam,dsigxx_dlam,dsigyy_dlam,dsigzz_dlam,     &
          dsigxy_dlam,dsigyz_dlam,dsigzx_dlam,dseq_dlam,dpla_dlam,dsigy_dlam,  &
          dsigbxx_dlam,dsigbyy_dlam,dsigbzz_dlam,dsigbxy_dlam,dsigbyz_dlam,    &
          dsigbzx_dlam,signxx_i,signyy_i,signzz_i,signxy_i,signyz_i,signzx_i,  &
          epsd_i,sigy_i,pla_i,dsigy_dpla_i,temp_i,seq_i,normxx_i,normyy_i,     &
          normzz_i,normxy_i,normyz_i,normzx_i,dtemp_dpla_i,dphi_dseq,          &
          dphi_dsigy,dphi_dlam,sig_dseqdsig
        real(kind=WP), dimension(nel,l_sigb) :: dsigb_dlam,sigb_i
        integer, dimension(nel,nvartmp) :: ipos0,vartmp_i
        logical :: converged
!
        integer, parameter :: eltype = 1               !< Element type (1 - Solids, 2 - Shells)
        integer, parameter :: nitermax = 20            !< Maximum number of plastic iterations
        real(kind=WP), parameter :: tol = 1.0d-6       !< Tolerance for plasticity convergence
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
          !< Add the kinematic hardening contribution to stress tensor
          signxx(1:nel) = signxx(1:nel) - sigbxx(1:nel)
          signyy(1:nel) = signyy(1:nel) - sigbyy(1:nel)
          signzz(1:nel) = signzz(1:nel) - sigbzz(1:nel)
          signxy(1:nel) = signxy(1:nel) - sigbxy(1:nel)
          signyz(1:nel) = signyz(1:nel) - sigbyz(1:nel)
          signzx(1:nel) = signzx(1:nel) - sigbzx(1:nel)
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
!
          !< Extract the indices of the yielding elements
          indx(1:nindx) = PACK(temp_all_indices(1:nel), active_elements_mask(1:nel))
!
          !< Initialisation of the iteration counter and convergence flag
          iter(1:nel) = 0
          converged = .false.
!
          !< Loop over yielding elements
          do while (.not. converged) 
!
            !< Loop over yielding elements
#include "vectorize.inc"
            do ii = 1,nindx
              i = indx(ii)
              !< 0 - Preliminary switch to local index of yielding elements
              !< ---------------------------------------------------------------
              normxx_i(ii) = normxx(i) !< Normal xx component
              normyy_i(ii) = normyy(i) !< Normal yy component
              normzz_i(ii) = normzz(i) !< Normal zz component
              normxy_i(ii) = normxy(i) !< Normal xy component
              normyz_i(ii) = normyz(i) !< Normal yz component
              normzx_i(ii) = normzx(i) !< Normal zx component
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
              !< ---------------------------------------------------------------
!
              !<  a) Derivatives of stress tensor w.r.t lambda
              !<  --------------------------------------------------------------
              dsigxx_dlam(ii) =                                                &
                      -(cstf(i,1,1)*normxx_i(ii) + cstf(i,1,2)*normyy_i(ii) +  &
                        cstf(i,1,3)*normzz_i(ii) + cstf(i,1,4)*normxy_i(ii) +  &
                        cstf(i,1,5)*normyz_i(ii) + cstf(i,1,6)*normzx_i(ii))
              dsigyy_dlam(ii) =                                                &
                      -(cstf(i,2,1)*normxx_i(ii) + cstf(i,2,2)*normyy_i(ii) +  &
                        cstf(i,2,3)*normzz_i(ii) + cstf(i,2,4)*normxy_i(ii) +  &
                        cstf(i,2,5)*normyz_i(ii) + cstf(i,2,6)*normzx_i(ii))
              dsigzz_dlam(ii) =                                                &
                      -(cstf(i,3,1)*normxx_i(ii) + cstf(i,3,2)*normyy_i(ii) +  &
                        cstf(i,3,3)*normzz_i(ii) + cstf(i,3,4)*normxy_i(ii) +  &
                        cstf(i,3,5)*normyz_i(ii) + cstf(i,3,6)*normzx_i(ii))
              dsigxy_dlam(ii) =                                                &
                      -(cstf(i,4,1)*normxx_i(ii) + cstf(i,4,2)*normyy_i(ii) +  &
                        cstf(i,4,3)*normzz_i(ii) + cstf(i,4,4)*normxy_i(ii) +  &
                        cstf(i,4,5)*normyz_i(ii) + cstf(i,4,6)*normzx_i(ii))
              dsigyz_dlam(ii) =                                                &
                      -(cstf(i,5,1)*normxx_i(ii) + cstf(i,5,2)*normyy_i(ii) +  &
                        cstf(i,5,3)*normzz_i(ii) + cstf(i,5,4)*normxy_i(ii) +  &
                        cstf(i,5,5)*normyz_i(ii) + cstf(i,5,6)*normzx_i(ii))
              dsigzx_dlam(ii) =                                                &
                      -(cstf(i,6,1)*normxx_i(ii) + cstf(i,6,2)*normyy_i(ii) +  &
                        cstf(i,6,3)*normzz_i(ii) + cstf(i,6,4)*normxy_i(ii) +  &
                        cstf(i,6,5)*normyz_i(ii) + cstf(i,6,6)*normzx_i(ii))
!
              !<  b) Assembling derivative of eq. stress sigeq w.r.t lambda
              !<  --------------------------------------------------------------
              dseq_dlam(ii) =                                                  &
                 normxx_i(ii)*dsigxx_dlam(ii) + normyy_i(ii)*dsigyy_dlam(ii) + &
                 normzz_i(ii)*dsigzz_dlam(ii) + normxy_i(ii)*dsigxy_dlam(ii) + &
                 normyz_i(ii)*dsigyz_dlam(ii) + normzx_i(ii)*dsigzx_dlam(ii)
!
              !< 2 - Derivative of yield stress ystrs w.r.t lambda
              !< ---------------------------------------------------------------
!
              !<  a) Derivative of eq. plastic strain w.r.t lambda
              !<  --------------------------------------------------------------
              sig_dseqdsig(ii) =                                               &
                             signxx(i)*normxx_i(ii) + signyy(i)*normyy_i(ii) + &
                             signzz(i)*normzz_i(ii) + signxy(i)*normxy_i(ii) + &
                             signyz(i)*normyz_i(ii) + signzx(i)*normzx_i(ii)
              dpla_dlam(ii) = sig_dseqdsig(ii)/max(sigy(i),em20)
!
              !<  b) Assembling derivative of ystrs w.r.t lambda
              !<  --------------------------------------------------------------
              dsigy_dlam(ii) = (one - chard)*dsigy_dpla_i(ii)*dpla_dlam(ii)
            enddo
!
            !< 3 - Add kinematic hardening to the derivative of eq.stress 
            !   w.r.t lambda
            !<  ----------------------------------------------------------------              
            if (ikine > 0) then
#include "vectorize.inc"
              do ii = 1,nindx
                i = indx(ii)
                sigb_i(ii,1:l_sigb) = sigb(i,1:l_sigb) !< Backstress tensor
              enddo
              !< a - Derivative of backstress tensor w.r.t lambda
              !<  --------------------------------------------------------------
              call elasto_plastic_kinematic_hardening(                         &
                matparam ,nindx    ,l_sigb   ,dsigb_dlam(1:nindx,1:l_sigb),    &
                dsigy_dpla_i,chard ,                                           &
                normxx_i ,normyy_i ,normzz_i ,normxy_i  ,normyz_i    ,normzx_i,&
                dpla_dlam,sigb_i(1:nindx,1:l_sigb),ikine    )
              !< b - Assembling the backstress contribution to the derivative  
              !  of eq. stress w.r.t lambda
              !<  --------------------------------------------------------------
              dsigbxx_dlam(1:nindx) = zero
              dsigbyy_dlam(1:nindx) = zero
              dsigbzz_dlam(1:nindx) = zero
              dsigbxy_dlam(1:nindx) = zero
              dsigbyz_dlam(1:nindx) = zero
              dsigbzx_dlam(1:nindx) = zero
              do j = 1, l_sigb/6
#include "vectorize.inc"
                do ii = 1,nindx
                  dsigbxx_dlam(ii) = dsigbxx_dlam(ii) + dsigb_dlam(ii,6*(j-1)+1)
                  dsigbyy_dlam(ii) = dsigbyy_dlam(ii) + dsigb_dlam(ii,6*(j-1)+2)
                  dsigbzz_dlam(ii) = dsigbzz_dlam(ii) + dsigb_dlam(ii,6*(j-1)+3)
                  dsigbxy_dlam(ii) = dsigbxy_dlam(ii) + dsigb_dlam(ii,6*(j-1)+4)
                  dsigbyz_dlam(ii) = dsigbyz_dlam(ii) + dsigb_dlam(ii,6*(j-1)+5)
                  dsigbzx_dlam(ii) = dsigbzx_dlam(ii) + dsigb_dlam(ii,6*(j-1)+6)
                enddo
              enddo
#include "vectorize.inc"
              do ii = 1,nindx
                dseq_dlam(ii) = dseq_dlam(ii) - normxx_i(ii)*dsigbxx_dlam(ii)- &
                                                normyy_i(ii)*dsigbyy_dlam(ii)- &
                                                normzz_i(ii)*dsigbzz_dlam(ii)- &
                                                normxy_i(ii)*dsigbxy_dlam(ii)- &
                                                normyz_i(ii)*dsigbyz_dlam(ii)- &
                                                normzx_i(ii)*dsigbzx_dlam(ii)
              enddo
            endif
!
#include "vectorize.inc"
            do ii = 1,nindx
              i = indx(ii)
              !< 4 - Assembling the derivative of phi w.r.t lambda
              !< ---------------------------------------------------------------
!
              !<  a) Derivative of phi w.r.t eq. stress sigeq
              !<  --------------------------------------------------------------
              dphi_dseq(ii)  =  two*seq(i)/(sigy(i)**2)
!
              !<  b) Derivative of phi w.r.t yield stress ystrs
              !<  --------------------------------------------------------------
              dphi_dsigy(ii) = -two*(seq(i)**2)/(sigy(i)**3)
!
              !<  c) Derivative of phi w.r.t lambda
              !<  --------------------------------------------------------------
              dphi_dlam(ii) = dphi_dseq(ii)*dseq_dlam(ii) +                    &
                              dphi_dsigy(ii)*dsigy_dlam(ii)
              dphi_dlam(ii) = sign(max(abs(dphi_dlam(ii)),em20),dphi_dlam(ii))
!
              !< 5 - Computation of plastic multiplier and variables update
              !< -----------------------------------------------------------------
!
              !<  a) Computation of the plastic multiplier increment dlam
              !<  ----------------------------------------------------------------
              dlam(ii) = -phi(i)/dphi_dlam(ii)
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
                  temp_i(ii) = temp(i) + dtemp_dpla(i)*dpla_dlam(ii)*dlam(ii)
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
            !<  ----------------------------------------------------------------
            call elasto_plastic_yield_stress(                                  &
              matparam ,nindx    ,sigy_i   ,pla_i    ,epsd_i   ,dsigy_dpla_i,  &
              nvartmp  ,vartmp_i(1:nindx,1:nvartmp)  ,temp_i   ,dtemp_dpla_i,  &
              jthe     )
!
            !<  e) Backstress tensor update
            !<  ----------------------------------------------------------------
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
                  sigb(i,6*(j-1) + 1) = sigb(i,6*(j-1) + 1) +                  &
                                       dsigb_dlam(ii,6*(j-1) + 1)*dlam(ii)
                  sigb(i,6*(j-1) + 2) = sigb(i,6*(j-1) + 2) +                  &
                                       dsigb_dlam(ii,6*(j-1) + 2)*dlam(ii)
                  sigb(i,6*(j-1) + 3) = sigb(i,6*(j-1) + 3) +                  &
                                       dsigb_dlam(ii,6*(j-1) + 3)*dlam(ii)
                  sigb(i,6*(j-1) + 4) = sigb(i,6*(j-1) + 4) +                  &
                                       dsigb_dlam(ii,6*(j-1) + 4)*dlam(ii)
                  sigb(i,6*(j-1) + 5) = sigb(i,6*(j-1) + 5) +                  &
                                       dsigb_dlam(ii,6*(j-1) + 5)*dlam(ii)
                  sigb(i,6*(j-1) + 6) = sigb(i,6*(j-1) + 6) +                  &
                                       dsigb_dlam(ii,6*(j-1) + 6)*dlam(ii)
                enddo
              enddo
            endif
!
            !<  f) Equivalent stress update
            !<  ----------------------------------------------------------------
            call elasto_plastic_eq_stress(                                     &
              matparam ,nindx    ,seq_i   ,iresp    ,eltype   ,                &
              signxx_i ,signyy_i ,signzz_i,signxy_i ,signyz_i ,signzx_i,       &
              normxx_i ,normyy_i ,normzz_i,normxy_i ,normyz_i ,normzx_i,       &
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
              dsigy_dpla(i) = dsigy_dpla_i(ii) !< Derivative of yield stress w.r.t. plastic strain
              dtemp_dpla(i) = dtemp_dpla_i(ii) !< Derivative of temperature w.r.t. plastic strain
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
              !<  --------------------------------------------------------------
              phi(i) = (seq(i)/sigy(i))**2 - one
!
              !<  i) Update iterations number
              !<  --------------------------------------------------------------
              iter(i) = iter(i) + 1
            enddo           
!
            !<  j) Update the list of yielding elements 
            !<  --------------------------------------------------------------
            nindx_1 = 0
            do ii = 1, nindx
              i = indx(ii)  
              if ((abs(phi(i)) >= tol).and.(iter(i) < nitermax)) then 
                nindx_1 = nindx_1 + 1 
                indx_1(nindx_1) = i
              endif
            enddo
!
            !< Update the number of yielding elements for the next iteration 
            !  and check convergence
            nindx = nindx_1
            indx(1:nindx) = indx_1(1:nindx)
            if (nindx == 0) converged = .true.
!
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
        !< - Equation of state coupling for solids
        !=======================================================================       
        if (ieos > 0) then
          sigm(1:nel) = (signxx(1:nel) + signyy(1:nel) + signzz(1:nel))/three
          signxx(1:nel) = signxx(1:nel) - sigm(1:nel)
          signyy(1:nel) = signyy(1:nel) - sigm(1:nel)
          signzz(1:nel) = signzz(1:nel) - sigm(1:nel)
        endif
!
       end subroutine cutting_plane_solids
       end module cutting_plane_solids_mod