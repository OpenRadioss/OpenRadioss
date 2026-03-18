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
      module cppm_solids_mod
      contains
      subroutine cppm_solids(                                                  &
        nel      ,matparam ,rho      ,nvartmp  ,vartmp   ,iresp    ,           &
        depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,           &
        epspxx   ,epspyy   ,epspzz   ,epspxy   ,epspyz   ,epspzx   ,           &     
        sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,           &
        signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,           &
        soundsp  ,off      ,pla      ,dpla     ,seq      ,et       ,           &
        sigy     ,timestep ,epsd     ,temp     ,israte   ,asrate   ,           &
        l_sigb   ,sigb     )
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
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i,j,ii,iter,nindx,indx(nel),vpflag,idev,ikine
        real(kind=WP), dimension(nel,6,6) :: cstf,N
        real(kind=WP) :: dlam,dsigxx_dlam,dsigyy_dlam,dsigzz_dlam,             &
          dsigxy_dlam,dsigyz_dlam,dsigzx_dlam,dseq_dlam,dpla_dlam,             &
          dphi_dseq,dphi_dsigy,dphi_dlam,sig_dseqdsig,dsigy_dlam,              &
          dsigbxx_dlam,dsigbyy_dlam,dsigbzz_dlam,dsigbxy_dlam,                 &
          dsigbyz_dlam,dsigbzx_dlam,chard
        real(kind=WP), dimension(nel) :: pla0,normxx,normyy,normzz,            &
          normxy,normyz,normzx,phi,young,dsigy_dpla,dtemp_dpla,s13,s23,shf,    &
          sigbxx,sigbyy,sigbzz,sigbxy,sigbyz,sigbzx,sigy0,dsigy0_dpla,         &
          dtemp0_dpla,zeros
        real(kind=WP) :: dX_dRes(2,2),detdRes_dX,Res(2),X(2),strs_d2sds2dsdlam,&
          dRes_dX(2,2),N_dsigdlam(6)
        logical :: converged
        real(kind=WP), dimension(nel,l_sigb) :: dsigb_dlam
        integer, dimension(nel,nvartmp) :: ipos0
!
        integer, parameter :: eltype = 1               !< Element type (1 - Solids, 2 - Shells)
        integer, parameter :: nitermax = 500           !< Maximum number of plastic iterations
        real(kind=WP), parameter :: tol = 1.0d-8       !< Tolerance for plasticity convergence
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
          N        ,.true.   )
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
        !< - Iterative algorithm using Closest Point Projection Method (C.P.P.M)
        !=======================================================================
        if (nindx > 0) then
!
          indx(1:nindx) = PACK(temp_all_indices(1:nel), active_elements_mask(1:nel))
!
          !< Loop over yielding elements
          do ii = 1,nindx
            i = indx(ii)
!
            !===================================================================
            !< - Initialisation of residue vector
            !===================================================================
            !< Initialization design variables
            X(1) = zero !< Plastic multiplier dlambda
            X(2) = zero !< Equivalent plastic strain increment qepsInc
            Res(1) = (seq(i)/sigy(i))**2 - one
            !< 2nd residue: Energy equivalence ystrs*qepsInc = dlambda*sig_dsigeqdsig
            !< Derivative of the equivalent plastic strain w.r.t the plastic mult.
            sig_dseqdsig = signxx(i)*normxx(i) + signyy(i)*normyy(i) +         &
                           signzz(i)*normzz(i) + signxy(i)*normxy(i) +         &
                           signyz(i)*normyz(i) + signzx(i)*normzx(i)
            dpla_dlam = sig_dseqdsig/max(sigy(i),em20)
            Res(2) = X(2) - X(1)*dpla_dlam
!
            !< Initialization of the convergence flag
            converged = .false.
            !< Initialization return mapping number of interation
            iter = 0
!    
            !< Iterative plastic correction procedure
            do while ((.not.converged).and.(iter<nitermax))
!
              !< 0 - Preliminary computation: 
              !-----------------------------------------------------------------
!
              !<  a) Derivative of yield function w.r.t equivalent stress
              !<  --------------------------------------------------------------
              dphi_dseq  =  two*seq(i)/(sigy(i)**2)
!
              !<  b) Derivative of yield function w.r.t yield stress
              !<  --------------------------------------------------------------
              dphi_dsigy = -two*(seq(i)**2)/(sigy(i)**3)
!
              !<  c) Derivative of the stress tensor w.r.t the plastic multiplier
              !<  --------------------------------------------------------------
              dsigxx_dlam = -(cstf(i,1,1)*normxx(i) + cstf(i,1,2)*normyy(i) +  &
                              cstf(i,1,3)*normzz(i))
              dsigyy_dlam = -(cstf(i,1,2)*normxx(i) + cstf(i,2,2)*normyy(i) +  &
                              cstf(i,2,3)*normzz(i))
              dsigzz_dlam = -(cstf(i,1,3)*normxx(i) + cstf(i,2,3)*normyy(i) +  &
                              cstf(i,3,3)*normzz(i))
              dsigxy_dlam = -(cstf(i,4,4)*normxy(i))
              dsigyz_dlam = -(cstf(i,5,5)*normyz(i))
              dsigzx_dlam = -(cstf(i,6,6)*normzx(i))
!
              !<  d) Product dstrs_dlam * dsigeq_dsig
              !<  --------------------------------------------------------------
              dseq_dlam = normxx(i)*dsigxx_dlam + normyy(i)*dsigyy_dlam +      &
                          normzz(i)*dsigzz_dlam + normxy(i)*dsigxy_dlam +      &
                          normyz(i)*dsigyz_dlam + normzx(i)*dsigzx_dlam
!
              !<  e) Product d2sigeq_dsig2 * dstrs_dlam
              !<  --------------------------------------------------------------
              N_dsigdlam(1) = N(i,1,1)*dsigxx_dlam + N(i,1,2)*dsigyy_dlam      &
                            + N(i,1,3)*dsigzz_dlam + N(i,1,4)*dsigxy_dlam      &
                            + N(i,1,5)*dsigyz_dlam + N(i,1,6)*dsigzx_dlam
              N_dsigdlam(2) = N(i,2,1)*dsigxx_dlam + N(i,2,2)*dsigyy_dlam      &
                            + N(i,2,3)*dsigzz_dlam + N(i,2,4)*dsigxy_dlam      &
                            + N(i,2,5)*dsigyz_dlam + N(i,2,6)*dsigzx_dlam
              N_dsigdlam(3) = N(i,3,1)*dsigxx_dlam + N(i,3,2)*dsigyy_dlam      &
                            + N(i,3,3)*dsigzz_dlam + N(i,3,4)*dsigxy_dlam      &
                            + N(i,3,5)*dsigyz_dlam + N(i,3,6)*dsigzx_dlam
              N_dsigdlam(4) = N(i,4,1)*dsigxx_dlam + N(i,4,2)*dsigyy_dlam      &
                            + N(i,4,3)*dsigzz_dlam + N(i,4,4)*dsigxy_dlam      &
                            + N(i,4,5)*dsigyz_dlam + N(i,4,6)*dsigzx_dlam
              N_dsigdlam(5) = N(i,5,1)*dsigxx_dlam + N(i,5,2)*dsigyy_dlam      &
                            + N(i,5,3)*dsigzz_dlam + N(i,5,4)*dsigxy_dlam      &
                            + N(i,5,5)*dsigyz_dlam + N(i,5,6)*dsigzx_dlam
              N_dsigdlam(6) = N(i,6,1)*dsigxx_dlam + N(i,6,2)*dsigyy_dlam      &
                            + N(i,6,3)*dsigzz_dlam + N(i,6,4)*dsigxy_dlam      &
                            + N(i,6,5)*dsigyz_dlam + N(i,6,6)*dsigzx_dlam
!
              !<  f) Add the contribution of the backstress tensor to the derivative 
              !   of the stress tensor w.r.t. plastic multiplier   
              !<  --------------------------------------------------------------         
              if (ikine > 0) then
                !< f)i) - Derivative of backstress tensor w.r.t lambda
                !<  ------------------------------------------------------------
                call elasto_plastic_kinematic_hardening(                       &
                  matparam ,1     ,l_sigb ,dsigb_dlam(i,1),dsigy_dpla(i),chard,&
                  normxx(i),normyy(i),normzz(i),normxy(i),normyz(i),normzx(i), &
                  dpla_dlam,sigb(i,1)) 
                !< f)ii) Assembling the backstress contribution to the derivative  
                !  of eq. stress w.r.t lambda
                !<  ------------------------------------------------------------
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
                dseq_dlam = dseq_dlam - normxx(i)*dsigbxx_dlam -               &
                                        normyy(i)*dsigbyy_dlam -               &
                                        normzz(i)*dsigbzz_dlam -               &
                                        normxy(i)*dsigbxy_dlam -               &
                                        normyz(i)*dsigbyz_dlam -               &
                                        normzx(i)*dsigbzx_dlam
                N_dsigdlam(1) = N_dsigdlam(1)                                  &
                              - N(i,1,1)*dsigbxx_dlam - N(i,1,2)*dsigbyy_dlam  &
                              - N(i,1,3)*dsigbzz_dlam - N(i,1,4)*dsigbxy_dlam  &
                              - N(i,1,5)*dsigbyz_dlam - N(i,1,6)*dsigbzx_dlam
                N_dsigdlam(2) = N_dsigdlam(2)                                  &
                              - N(i,2,1)*dsigbxx_dlam - N(i,2,2)*dsigbyy_dlam  &
                              - N(i,2,3)*dsigbzz_dlam - N(i,2,4)*dsigbxy_dlam  &
                              - N(i,2,5)*dsigbyz_dlam - N(i,2,6)*dsigbzx_dlam
                N_dsigdlam(3) = N_dsigdlam(3)                                  &
                              - N(i,3,1)*dsigbxx_dlam - N(i,3,2)*dsigbyy_dlam  &
                              - N(i,3,3)*dsigbzz_dlam - N(i,3,4)*dsigbxy_dlam  &
                              - N(i,3,5)*dsigbyz_dlam - N(i,3,6)*dsigbzx_dlam
                N_dsigdlam(4) = N_dsigdlam(4)                                  &
                              - N(i,4,1)*dsigbxx_dlam - N(i,4,2)*dsigbyy_dlam  &
                              - N(i,4,3)*dsigbzz_dlam - N(i,4,4)*dsigbxy_dlam  &
                              - N(i,4,5)*dsigbyz_dlam - N(i,4,6)*dsigbzx_dlam
                N_dsigdlam(5) = N_dsigdlam(5)                                  &
                              - N(i,5,1)*dsigbxx_dlam - N(i,5,2)*dsigbyy_dlam  &
                              - N(i,5,3)*dsigbzz_dlam - N(i,5,4)*dsigbxy_dlam  &
                              - N(i,5,5)*dsigbyz_dlam - N(i,5,6)*dsigbzx_dlam
                N_dsigdlam(6) = N_dsigdlam(6)                                  &
                              - N(i,6,1)*dsigbxx_dlam - N(i,6,2)*dsigbyy_dlam  &
                              - N(i,6,3)*dsigbzz_dlam - N(i,6,4)*dsigbxy_dlam  &
                              - N(i,6,5)*dsigbyz_dlam - N(i,6,6)*dsigbzx_dlam
              endif
!
              !<   g) Product strs * (d2sigeq_dsig2 * dstrs_dlam)
              !<  --------------------------------------------------------------
              strs_d2sds2dsdlam = signxx(i)*N_dsigdlam(1) +                    &
                                  signyy(i)*N_dsigdlam(2) +                    &
                                  signzz(i)*N_dsigdlam(3) +                    &
                                  signxy(i)*N_dsigdlam(4) +                    &
                                  signyz(i)*N_dsigdlam(5) +                    &
                                  signzx(i)*N_dsigdlam(6)                      
! 
              !< 1 - Gradient of the residue vector
              !-----------------------------------------------------------------
!
              !<  a) Derivative of the yield function w.r.t the plastic multiplier
              !<  --------------------------------------------------------------
              dRes_dX(1,1) = dphi_dseq*dseq_dlam
!
              !<  b) Derivative of the yield function w.r.t the eq. plastic strain
              !<  --------------------------------------------------------------
              dRes_dX(1,2) = dphi_dsigy*dsigy_dpla(i)
!
              !<  c) Derivative of energy equivalence residue w.r.t plastic multiplier
              !<  --------------------------------------------------------------
              dRes_dX(2,1) = - dpla_dlam -                                     &
                            (X(1)/sigy(i))*(dseq_dlam + strs_d2sds2dsdlam)
!
              !<  d) Derivative of energy equivalence residue w.r.t eq. plastic strain
              !<  --------------------------------------------------------------
              dRes_dX(2,2) = one +                                             &
                            ((sig_dseqdsig)/(sigy(i)**2))*dsigy_dpla(i)*X(1)
!
              !< 2 - Update design variables
              !-----------------------------------------------------------------
!
              !<  a) Inverse of the gradient of residue vector 
              !<  --------------------------------------------------------------
              dX_dRes(1,1) =  dRes_dX(2,2)
              dX_dRes(1,2) = -dRes_dX(1,2)
              dX_dRes(2,1) = -dRes_dX(2,1)
              dX_dRes(2,2) =  dRes_dX(1,1)
              detdRes_dX = dRes_dX(1,1)*dRes_dX(2,2)-dRes_dX(1,2)*dRes_dX(2,1)
              detdRes_dX = sign(max(abs(detdRes_dX),em20),detdRes_dX)
              dX_dRes(1:2,1:2) = (one/detdRes_dX)*dX_dRes(1:2,1:2)
!
              !<  b) Update the design variables
              !<  --------------------------------------------------------------
              X(1) = X(1) - dX_dRes(1,1)*Res(1) - dX_dRes(1,2)*Res(2)
              X(2) = X(2) - dX_dRes(2,1)*Res(1) - dX_dRes(2,2)*Res(2)
!
              !< 3 - Computation of plastic multiplier and variables update
              !-----------------------------------------------------------------
!
              !<  a) Computation of the plastic multiplier increment dlam
              !<  --------------------------------------------------------------
              dlam = - dX_dRes(1,1)*Res(1) - dX_dRes(1,2)*Res(2)
!
              !<  b) Stress tensor update
              !<  --------------------------------------------------------------
              signxx(i) = signxx(i) + dsigxx_dlam*dlam 
              signyy(i) = signyy(i) + dsigyy_dlam*dlam
              signzz(i) = signzz(i) + dsigzz_dlam*dlam
              signxy(i) = signxy(i) + dsigxy_dlam*dlam
              signyz(i) = signyz(i) + dsigyz_dlam*dlam
              signzx(i) = signzx(i) + dsigzx_dlam*dlam
!
              !<  c) Update the plastic strain related variables
              !<  --------------------------------------------------------------
              !< Equivalent plastic strain increment
              dpla(i) = max(X(2),zero)
              !< Equivalent plastic strain                        
              pla(i)  = pla0(i) + dpla(i)
              !< Temperature
              temp(i) = temp(i) + dtemp_dpla(i)*dpla_dlam*dlam
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
                  sigb(i,6*(j-1) + 1) = sigb(i,6*(j-1) + 1) +                  &
                                       dsigb_dlam(i,6*(j-1) + 1)*dlam
                  sigb(i,6*(j-1) + 2) = sigb(i,6*(j-1) + 2) +                  &
                                       dsigb_dlam(i,6*(j-1) + 2)*dlam
                  sigb(i,6*(j-1) + 3) = sigb(i,6*(j-1) + 3) +                  &
                                       dsigb_dlam(i,6*(j-1) + 3)*dlam
                  sigb(i,6*(j-1) + 4) = sigb(i,6*(j-1) + 4) +                  &
                                       dsigb_dlam(i,6*(j-1) + 4)*dlam
                  sigb(i,6*(j-1) + 5) = sigb(i,6*(j-1) + 5) +                  &
                                       dsigb_dlam(i,6*(j-1) + 5)*dlam
                  sigb(i,6*(j-1) + 6) = sigb(i,6*(j-1) + 6) +                  &
                                       dsigb_dlam(i,6*(j-1) + 6)*dlam
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
                N(i,1,1) ,.true.   )
!
              !<  g) Yield function update
              !<  --------------------------------------------------------------
              Res(1) = (seq(i)/sigy(i))**2 - one
!
              !<  h) Energy equivalence residue update
              !<  --------------------------------------------------------------
              sig_dseqdsig = signxx(i)*normxx(i) + signyy(i)*normyy(i) +       &
                             signzz(i)*normzz(i) + signxy(i)*normxy(i) +       &
                             signyz(i)*normyz(i) + signzx(i)*normzx(i)
              dpla_dlam = sig_dseqdsig/max(sigy(i),em20)
              Res(2) = X(2) - X(1)*dpla_dlam
!
              !< 4 - Update convergence flag and iterations number
              !< --------------------------------------------------------------- 
!
              !<  a) Convergence flag
              !<  --------------------------------------------------------------
              converged = abs(Res(1))<tol .and. abs(Res(2))<tol  
!
              !<  b) Update iterations number
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
       end subroutine cppm_solids
       end module cppm_solids_mod