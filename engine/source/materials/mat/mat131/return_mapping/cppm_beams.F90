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
      module cppm_beams_mod
! \brief Closest Point Projection Method return mapping for beams in /MAT/LAW131
! \details Perform the CPPM (Closest Point Projection Method) implicit return
!          mapping algorithm for beam elements in /MAT/LAW131.
      contains
!||====================================================================
!||    cppm_beams                               ../engine/source/materials/mat/mat131/return_mapping/cppm_beams.F90
!||--- called by ------------------------------------------------------
!||    sigeps131pi                              ../engine/source/materials/mat/mat131/sigeps131pi.F90
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
        subroutine cppm_beams(                                                  &
          nel      ,matparam ,nvartmp  ,vartmp   ,off      ,timestep ,          &
          depsxx   ,depsxy   ,depszx   ,sigoxx   ,sigoxy   ,sigozx   ,          &
          signxx   ,signxy   ,signzx   ,pla      ,seq      ,et       ,          &
          sigy     ,epsd     ,temp     ,asrate   ,l_sigb   ,sigb     ,          &
          nuvar    ,uvar     ,jthe     ,vpflag   ,ikine    ,chard    )
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
          integer,                       intent(in)    :: nvartmp   !< Number of variables used in tabulated variables
          integer, dimension(nel,nvartmp), intent(inout) :: vartmp  !< Temporary variables for tabulated hardening
          real(kind=WP), dimension(nel), intent(inout) :: off       !< Integration point failure flag
          real(kind=WP), intent(in)                    :: timestep  !< Time step
          real(kind=WP), dimension(nel), intent(in)    :: depsxx    !< Strain increment xx
          real(kind=WP), dimension(nel), intent(in)    :: depsxy    !< Strain increment xy
          real(kind=WP), dimension(nel), intent(in)    :: depszx    !< Strain increment zx
          real(kind=WP), dimension(nel), intent(in)    :: sigoxx    !< Previous stress xx
          real(kind=WP), dimension(nel), intent(in)    :: sigoxy    !< Previous stress xy
          real(kind=WP), dimension(nel), intent(in)    :: sigozx    !< Previous stress zx
          real(kind=WP), dimension(nel), intent(inout) :: signxx    !< Current stress xx
          real(kind=WP), dimension(nel), intent(inout) :: signxy    !< Current stress xy
          real(kind=WP), dimension(nel), intent(inout) :: signzx    !< Current stress zx
          real(kind=WP), dimension(nel), intent(inout) :: pla       !< Accumulated plastic strain
          real(kind=WP), dimension(nel), intent(inout) :: seq       !< Equivalent stress
          real(kind=WP), dimension(nel), intent(inout) :: et        !< Hourglass stabilization variable
          real(kind=WP), dimension(nel), intent(inout) :: sigy      !< Current yield stress
          real(kind=WP), dimension(nel), intent(inout) :: epsd      !< Plastic strain rate
          real(kind=WP), dimension(nel), intent(inout) :: temp      !< Temperature
          real(kind=WP),                 intent(in)    :: asrate    !< Strain rate filtering weighting factor
          integer,                       intent(in)    :: l_sigb    !< Size of backstress array
          real(kind=WP),dimension(nel,l_sigb),intent(inout) :: sigb !< Backstress components for kinematic hardening
          integer,                       intent(in)    :: nuvar     !< Number of user variables
        real(kind=WP),dimension(nel,nuvar), intent(inout) :: uvar  !< User variables
          integer,                       intent(in)    :: jthe      !< /HEAT/MAT flag
          integer,                       intent(in)    :: vpflag    !< Viscoplasticity flag
          integer,                       intent(in)    :: ikine     !< Kinematic hardening type
          real(kind=WP),                 intent(in)    :: chard     !< Isotropic/kinematic mixed hardening factor
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: i,j,ii,iter(nel),nindx,indx(nel),nindx_1,indx_1(nel)
          real(kind=WP), dimension(:,:,:), allocatable :: cstf,N,N_i
          real(kind=WP) :: dlam_nl
          real(kind=WP), dimension(nel) :: pla0,normxx,normyy,normzz,normxy,     &
            normyz,normzx,phi,young,dsigy_dpla,dtemp_dpla,s13,s23,s43,           &
            sigbxx,sigbxy,sigbzx,sigy0,dsigy0_dpla,dtemp0_dpla,zeros,            &
            dpdm,dlam,dsigxx_dlam,dsigxy_dlam,dsigzx_dlam,dseq_dlam,dpla_dlam,   &
            dphi_dseq,dphi_dsigy,sig_dseqdsig,dsigbxx_dlam,                      &
            dsigbxy_dlam,dsigbzx_dlam,signxx_i,signyy_i,signzz_i,                &
            signxy_i,signyz_i,signzx_i,epsd_i,sigy_i,pla_i,dsigy_dpla_i,temp_i,  &
            seq_i,normxx_i,normyy_i,normzz_i,normxy_i,normyz_i,normzx_i,         &
            dtemp_dpla_i,dpla_dlam_i,strs_d2sds2dsdlam,epsdot,dav,deve1,deve2,   &
            deve3,deve4,dpla,rho,shf,soundsp
          real(kind=WP) :: dX_dRes(nel,2,2),detdRes_dX(nel),Res(nel,2),X(nel,2), &
            dRes_dX(nel,2,2),N_dsigdlam(nel,6)
          logical :: converged
          real(kind=WP), dimension(:,:), allocatable :: dsigb_dlam,sigb_i
          real(kind=WP), dimension(nel) :: signzz,sigozz,depszz,signyy,signyz,   &
          sigoyy,sigoyz,depsyy,depsyz
          integer, dimension(:,:), allocatable :: ipos0,vartmp_i
!
          integer, parameter :: eltype = 3               !< Element type (1 - Solids, 2 - Shells, 3 - Beams)
          integer, parameter :: nitermax = 500           !< Maximum number of plastic iterations
          real(kind=WP), parameter :: tol = 1.0d-6       !< Tolerance for plasticity convergence
          integer, parameter :: iresp = 0                !< Response type (0 - standard)
          integer, parameter :: ieos = 0                 !< Equation of state type (0 - standard)

          logical, dimension(nel) :: active_elements_mask
          integer, dimension(nel) :: temp_all_indices
          zeros(1:nel) = zero
!===============================================================================
!
          !< Allocate large arrays if not already allocated
          if (.not. allocated(cstf))       allocate(cstf(nel,6,6))
          if (.not. allocated(N))          allocate(N(nel,6,6))
          if (.not. allocated(N_i))        allocate(N_i(nel,6,6))
          if (.not. allocated(dsigb_dlam)) allocate(dsigb_dlam(nel,l_sigb))
          if (.not. allocated(sigb_i))     allocate(sigb_i(nel,l_sigb))
          if (.not. allocated(ipos0))      allocate(ipos0(nel,nvartmp))
          if (.not. allocated(vartmp_i))   allocate(vartmp_i(nel,nvartmp))
!
          !=======================================================================
          !< - Initialisation of computation on time step
          !=======================================================================
        !< Plastic strain rate
        if (vpflag == 1) epsd(1:nel) = uvar(1:nel,1)
          !< Initialisation of the hourglass control variable
          et(1:nel) = one
          !< Increment of cumulated plastic strain
          dpla(1:nel) = zero
          !< Derivative of temperature w.r.t. cumulated plastic strain
          dtemp_dpla(1:nel) = zero
          dtemp_dpla_i(1:nel) = zero
          !< Save the initial cumulated plastic strain value
          pla0(1:nel) = pla(1:nel)
          !< Initialize dummy stress components for shell elements
          signyy(1:nel) = zero
          signzz(1:nel) = zero
          signyz(1:nel) = zero
          sigoyy(1:nel) = zero
          sigozz(1:nel) = zero
          sigoyz(1:nel) = zero
          depsyy(1:nel) = zero
          depszz(1:nel) = zero
          depsyz(1:nel) = zero
          !< Initial density
          rho(1:nel) = matparam%rho0
          !< Shear correction factor for beam elements
          shf(1:nel) = five_over_6
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
          !< - Backstress tensor computation for kinematic hardening models
          !=======================================================================
          if (ikine > 0) then
            sigbxx(1:nel) = zero
            sigbxy(1:nel) = zero
            sigbzx(1:nel) = zero
            !< Compute the backstress tensor from all C-R kinematic hardenings
            do j = 1, l_sigb/6
              sigbxx(1:nel) = sigbxx(1:nel) + sigb(1:nel,6*(j-1) + 1)
              sigbxy(1:nel) = sigbxy(1:nel) + sigb(1:nel,6*(j-1) + 4)
            sigbzx(1:nel) = sigbzx(1:nel) + sigb(1:nel,6*(j-1) + 6)
            enddo
            !< Add the kinematic hardening contribution to stress tensors
            signxx(1:nel) = signxx(1:nel) - sigbxx(1:nel)
            signxy(1:nel) = signxy(1:nel) - sigbxy(1:nel)
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
            N        ,.true.   )
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
          !< - Iterative algorithm using Closest Point Projection Method (C.P.P.M)
          !=======================================================================
          if (nindx > 0) then
!
            !< Extract the indices of the yielding elements
            indx(1:nindx) = PACK(temp_all_indices(1:nel),active_elements_mask(1:nel))
!
            !< Initialisation of the iteration counter and convergence flag
            iter(1:nel) = 0
            converged = .false.
!
            !=====================================================================
            !< - Initialisation of residue vector
            !=====================================================================
            !< Loop over yielding elements
#include "vectorize.inc"
            do ii = 1,nindx
              i = indx(ii)
              !< Initialization design variables
              X(i,1) = zero !< Plastic multiplier dlambda
              X(i,2) = zero !< Equivalent plastic strain increment qepsInc
              Res(i,1) = (seq(i)/sigy(i))**2 - one
              !< 2nd residue: Energy equivalence ystrs*qepsInc = dlambda*sig_dsigeqdsig
              !< Derivative of the equivalent plastic strain w.r.t the plastic mult.
              sig_dseqdsig(i) = signxx(i)*normxx(i) + signxy(i)*normxy(i) +      &
                signzx(i)*normzx(i)
              dpla_dlam(i) = sig_dseqdsig(i)/max(sigy(i),em20)
              Res(i,2) = X(i,2) - X(i,1)*dpla_dlam(i)
            enddo
!
            !< Loop over yielding elements
            do while (.not. converged)
!
              !< Loop over yielding elements
#include "vectorize.inc"
              do ii = 1,nindx
                i = indx(ii)
!
                !< 0 - Preliminary computation:
                !-----------------------------------------------------------------
!
                !<  a) Switch to local index of yielding elements
                !< ---------------------------------------------------------------
                normxx_i(ii) = normxx(i) !< Normal xx component
                normxy_i(ii) = normxy(i) !< Normal xy component
                normzx_i(ii) = normzx(i) !< Normal zx component
                dsigy_dpla_i(ii) = dsigy_dpla(i) !< Derivative of yield stress w.r.t. plastic strain
                dpla_dlam_i(ii) = dpla_dlam(i) !< Derivative of plastic strain increment w.r.t. plastic multiplier
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
                !<  b) Derivative of yield function w.r.t equivalent stress
                !<  --------------------------------------------------------------
                dphi_dseq(ii)  =  two*seq(i)/(sigy(i)**2)
!
                !<  c) Derivative of yield function w.r.t yield stress
                !<  --------------------------------------------------------------
                dphi_dsigy(ii) = -two*(seq(i)**2)/(sigy(i)**3)
!
                !<  d) Derivative of the stress tensor w.r.t the plastic multiplier
                !<  --------------------------------------------------------------
                dsigxx_dlam(ii) =                                                &
                  -(cstf(i,1,1)*normxx_i(ii) + cstf(i,1,4)*normxy_i(ii) +        &
                    cstf(i,1,6)*normzx_i(ii))
                dsigxy_dlam(ii) =                                                &
                  -(cstf(i,4,1)*normxx_i(ii) + cstf(i,4,4)*normxy_i(ii) +        &
                    cstf(i,4,6)*normzx_i(ii))
                dsigzx_dlam(ii) =                                                &                                  
                  -(cstf(i,6,1)*normxx_i(ii) + cstf(i,6,4)*normxy_i(ii) +        &
                    cstf(i,6,6)*normzx_i(ii))
!
                !<  d) Product dstrs_dlam * dsigeq_dsig
                !<  --------------------------------------------------------------
                dseq_dlam(ii) =                                                  &
                  normxx_i(ii)*dsigxx_dlam(ii) + normxy_i(ii)*dsigxy_dlam(ii) +  &
                  normzx_i(ii)*dsigzx_dlam(ii)
!
                !<  e) Product d2sigeq_dsig2 * dstrs_dlam
                !<  --------------------------------------------------------------
                N_dsigdlam(ii,1) =                                               &
                  N(i,1,1)*dsigxx_dlam(ii) + N(i,1,4)*dsigxy_dlam(ii) +          &
                  N(i,1,6)*dsigzx_dlam(ii)
                N_dsigdlam(ii,4) =                                               &
                  N(i,4,1)*dsigxx_dlam(ii) + N(i,4,4)*dsigxy_dlam(ii) +          &
                  N(i,4,6)*dsigzx_dlam(ii)
                N_dsigdlam(ii,6) =                                               &
                  N(i,6,1)*dsigxx_dlam(ii) + N(i,6,4)*dsigxy_dlam(ii) +          &
                  N(i,6,6)*dsigzx_dlam(ii)
              enddo
!
              !<  f) Add the contribution of the backstress tensor to the derivative
              !   of the stress tensor w.r.t. plastic multiplier
              !<  ----------------------------------------------------------------
              if (ikine > 0) then
#include "vectorize.inc"
                do ii = 1,nindx
                  i = indx(ii)
                  sigb_i(ii,1:l_sigb) = sigb(i,1:l_sigb) !< Backstress tensor
                enddo
                !< f)i) - Derivative of backstress tensor w.r.t lambda
                !<  --------------------------------------------------------------
                call elasto_plastic_kinematic_hardening(                         &
                  matparam ,nindx    ,l_sigb   ,dsigb_dlam(1:nindx,1:l_sigb),    &
                  dsigy_dpla_i,chard ,                                           &
                  normxx_i ,normyy_i ,normzz_i ,normxy_i  ,normyz_i    ,normzx_i,&
                  dpla_dlam_i,sigb_i(1:nindx,1:l_sigb),ikine   )
                !< f)ii) Assembling the backstress contribution to the derivative
                !  of eq. stress w.r.t lambda
                !<  --------------------------------------------------------------
                dsigbxx_dlam(1:nindx) = zero
                dsigbxy_dlam(1:nindx) = zero
                dsigbzx_dlam(1:nindx) = zero
                do j = 1, l_sigb/6
#include "vectorize.inc"
                  do ii = 1,nindx
                    dsigbxx_dlam(ii) = dsigbxx_dlam(ii) + dsigb_dlam(ii,6*(j-1)+1)
                    dsigbxy_dlam(ii) = dsigbxy_dlam(ii) + dsigb_dlam(ii,6*(j-1)+4)
                    dsigbzx_dlam(ii) = dsigbzx_dlam(ii) + dsigb_dlam(ii,6*(j-1)+6)
                  enddo
                enddo
#include "vectorize.inc"
                do ii = 1,nindx
                  i = indx(ii)
                  dsigbxx_dlam(ii) = three_half*dsigbxx_dlam(ii)
                  dseq_dlam(ii) = dseq_dlam(ii) - normxx_i(ii)*dsigbxx_dlam(ii)- &
                                                  normxy_i(ii)*dsigbxy_dlam(ii)- &
                                                  normzx_i(ii)*dsigbzx_dlam(ii)
                  N_dsigdlam(ii,1) = N_dsigdlam(ii,1)                            &
                    - N(i,1,1)*dsigbxx_dlam(ii) - N(i,1,4)*dsigbxy_dlam(ii)      &
                    - N(i,1,6)*dsigbzx_dlam(ii)
                  N_dsigdlam(ii,4) = N_dsigdlam(ii,4)                            &
                    - N(i,4,1)*dsigbxx_dlam(ii) - N(i,4,4)*dsigbxy_dlam(ii)      &
                    - N(i,4,6)*dsigbzx_dlam(ii)
                  N_dsigdlam(ii,6) = N_dsigdlam(ii,6)                            &
                    - N(i,6,1)*dsigbxx_dlam(ii) - N(i,6,4)*dsigbxy_dlam(ii)      &
                    - N(i,6,6)*dsigbzx_dlam(ii)
                enddo
              endif
!
#include "vectorize.inc"
              do ii = 1,nindx
                i = indx(ii)
                !<   g) Product strs * (d2sigeq_dsig2 * dstrs_dlam)
                !<  --------------------------------------------------------------
                strs_d2sds2dsdlam(ii) = signxx(i)*N_dsigdlam(ii,1) +             &
                                        signxy(i)*N_dsigdlam(ii,4) +             &
                                        signzx(i)*N_dsigdlam(ii,6)
!
                !< 1 - Gradient of the residue vector
                !-----------------------------------------------------------------
!
                !<  a) Derivative of the yield function w.r.t the plastic multiplier
                !<  --------------------------------------------------------------
                dRes_dX(ii,1,1) = dphi_dseq(ii)*dseq_dlam(ii)
!
                !<  b) Derivative of the yield function w.r.t the eq. plastic strain
                !<  --------------------------------------------------------------
                dRes_dX(ii,1,2) = dphi_dsigy(ii)*dsigy_dpla_i(ii)
!
                !<  c) Derivative of energy equivalence residue w.r.t plastic multiplier
                !<  --------------------------------------------------------------
                dRes_dX(ii,2,1) = - dpla_dlam_i(ii) -                            &
                  (X(i,1)/sigy(i))*(dseq_dlam(ii) + strs_d2sds2dsdlam(ii))
!
                !<  d) Derivative of energy equivalence residue w.r.t eq. plastic strain
                !<  --------------------------------------------------------------
                dRes_dX(ii,2,2) = one +                                          &
                  ((sig_dseqdsig(i))/(sigy(i)**2))*dsigy_dpla_i(ii)*X(i,1)
!
                !< 2 - Update design variables
                !-----------------------------------------------------------------
!
                !<  a) Inverse of the gradient of residue vector
                !<  --------------------------------------------------------------
                dX_dRes(ii,1,1) =  dRes_dX(ii,2,2)
                dX_dRes(ii,1,2) = -dRes_dX(ii,1,2)
                dX_dRes(ii,2,1) = -dRes_dX(ii,2,1)
                dX_dRes(ii,2,2) =  dRes_dX(ii,1,1)
                detdRes_dX(ii) = dRes_dX(ii,1,1)*dRes_dX(ii,2,2) -               &
                  dRes_dX(ii,1,2)*dRes_dX(ii,2,1)
                detdRes_dX(ii) = sign(max(abs(detdRes_dX(ii)),em20),detdRes_dX(ii))
                dX_dRes(ii,1,1) = (one/detdRes_dX(ii))*dX_dRes(ii,1,1)
                dX_dRes(ii,1,2) = (one/detdRes_dX(ii))*dX_dRes(ii,1,2)
                dX_dRes(ii,2,1) = (one/detdRes_dX(ii))*dX_dRes(ii,2,1)
                dX_dRes(ii,2,2) = (one/detdRes_dX(ii))*dX_dRes(ii,2,2)
!
                !<  b) Update the design variables
                !<  --------------------------------------------------------------
                X(i,1) = X(i,1)-dX_dRes(ii,1,1)*Res(i,1)-dX_dRes(ii,1,2)*Res(i,2)
                X(i,2) = X(i,2)-dX_dRes(ii,2,1)*Res(i,1)-dX_dRes(ii,2,2)*Res(i,2)
!
                !< 3 - Computation of plastic multiplier and variables update
                !-----------------------------------------------------------------
!
                !<  a) Computation of the plastic multiplier increment dlam
                !<  --------------------------------------------------------------
                dlam(ii) = - dX_dRes(ii,1,1)*Res(i,1) - dX_dRes(ii,1,2)*Res(i,2)
!
                !<  b) Stress tensor update
                !<  --------------------------------------------------------------
                signxx_i(ii) = signxx(i) + dsigxx_dlam(ii)*dlam(ii)
                signxy_i(ii) = signxy(i) + dsigxy_dlam(ii)*dlam(ii)
                signzx_i(ii) = signzx(i) + dsigzx_dlam(ii)*dlam(ii)
!
                !<  c) Update the plastic strain related variables
                !<  --------------------------------------------------------------
                !< Equivalent plastic strain increment
                dpla(i) = max(X(i,2),zero)
                !< Equivalent plastic strain
                pla_i(ii) = pla0(i) + dpla(i)
              enddo
!
              !< Temperature or heating generation update for /HEAT/MAT
            if (jthe == 0) then
#include "vectorize.inc"
                do ii = 1,nindx
                  i = indx(ii)
                  temp_i(ii) = temp(i) + dtemp_dpla_i(ii)*dpla_dlam(ii)*dlam(ii)
                enddo
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
                matparam ,nindx    ,sigy_i   ,pla_i    ,epsd_i   ,dsigy_dpla_i,    &
                nvartmp  ,vartmp_i(1:nindx,1:nvartmp)  ,temp_i   ,dtemp_dpla_i,    &
                jthe  )
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
                  signxy_i(ii) = signxy_i(ii) + sigbxy(i)
                  signzx_i(ii) = signzx_i(ii) + sigbzx(i)
                enddo
!DIR$ IVDEP
                do ii = 1, nindx
                  i = indx(ii)
                  ! -> Add the evolution of backstress tensor
                  sigbxx(i) = sigbxx(i) + dsigbxx_dlam(ii)*dlam(ii)
                  sigbxy(i) = sigbxy(i) + dsigbxy_dlam(ii)*dlam(ii)
                  sigbzx(i) = sigbzx(i) + dsigbzx_dlam(ii)*dlam(ii)
                enddo
#include "vectorize.inc"
                do ii = 1, nindx
                  i = indx(ii)
                  ! -> Add the kinematic hardening contribution
                  signxx_i(ii) = signxx_i(ii) - sigbxx(i)
                  signxy_i(ii) = signxy_i(ii) - sigbxy(i)
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
                      three_half*dsigb_dlam(ii,6*(j-1) + 1)*dlam(ii)
                    sigb(i,6*(j-1) + 4) = sigb(i,6*(j-1) + 4) +                  &
                                 dsigb_dlam(ii,6*(j-1) + 4)*dlam(ii)
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
                N_i(1:nindx,1:6,1:6),.true. )
!
#include "vectorize.inc"
              do ii = 1, nindx
                i = indx(ii)
!
                !<  g) Recopy of local variables to global arrays
                !<  ----------------------------------------------------------------
                signxx(i) = signxx_i(ii) !< Current value of stress component xx
                signxy(i) = signxy_i(ii) !< Current value of stress component xy
                signzx(i) = signzx_i(ii) !< Current value of stress component zx
                seq(i)    = seq_i(ii)    !< Equivalent stress
                sigy(i)   = sigy_i(ii)   !< Yield stress
                pla(i)    = pla_i(ii)    !< Equivalent plastic strain
                temp(i)   = temp_i(ii)   !< Temperature
                normxx(i) = normxx_i(ii) !< Normal component xx
                normxy(i) = normxy_i(ii) !< Normal component xy
                normzx(i) = normzx_i(ii) !< Normal component zx
                dsigy_dpla(i) = dsigy_dpla_i(ii) !< Derivative of yield stress w.r.t. plastic strain
                dtemp_dpla(i) = dtemp_dpla_i(ii) !< Derivative of temperature w.r.t. plastic strain
              enddo
!
#include "vectorize.inc"
              do ii = 1, nindx
                i = indx(ii)
                N(i,1:6,1:6) = N_i(ii,1:6,1:6) !< Derivative of the normal w.r.t the stress tensor
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
                Res(i,1) = (seq(i)/sigy(i))**2 - one
!
                !<  i) Energy equivalence residue update
                !<  --------------------------------------------------------------
                sig_dseqdsig(i) = signxx_i(ii)*normxx_i(ii) +                    &
                                  signxy_i(ii)*normxy_i(ii) +                    &
                                  signzx_i(ii)*normzx_i(ii)
                dpla_dlam(i) = sig_dseqdsig(i)/max(sigy(i),em20)
                Res(i,2) = X(i,2) - X(i,1)*dpla_dlam(i)
!
                !<  j) Update iterations number
                !< ---------------------------------------------------------------
                iter(i) = iter(i) + 1
              enddo
!
              !<  k) Update the list of yielding elements
              !<  ----------------------------------------------------------------
              nindx_1 = 0
              indx_1 = 0
              do ii = 1, nindx
                i = indx(ii)
                if (((abs(Res(i,1)) >= tol).or.(abs(Res(i,2)) >= tol)) .and.     &
                  (iter(i) < nitermax)) then
                  nindx_1 = nindx_1 + 1
                  indx_1(nindx_1) = i
                endif
              enddo
!
              !< 4 - Update the number of yielding elements for the next iteration
              !       and check convergence
              !-------------------------------------------------------------------
              nindx = nindx_1
              indx = 0
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
              (one - asrate)*uvar(1:nel,1)
            uvar(1:nel,1) = epsd(1:nel)
          endif
!
          !=======================================================================
          !< - Remove backstress contribution from stress tensor
          !=======================================================================
          if (ikine > 0) then
          signxx(1:nel) = signxx(1:nel) + sigbxx(1:nel)
          signxy(1:nel) = signxy(1:nel) + sigbxy(1:nel)
          signzx(1:nel) = signzx(1:nel) + sigbzx(1:nel)
          endif
!
          !< Large array deallocation
          if (allocated(cstf))       deallocate(cstf)
          if (allocated(N))          deallocate(N)
          if (allocated(N_i))        deallocate(N_i)
          if (allocated(dsigb_dlam)) deallocate(dsigb_dlam)
          if (allocated(sigb_i))     deallocate(sigb_i)
          if (allocated(ipos0))      deallocate(ipos0)
          if (allocated(vartmp_i))   deallocate(vartmp_i)
!
        end subroutine cppm_beams
      end module cppm_beams_mod
