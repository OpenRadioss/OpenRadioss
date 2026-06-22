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
      module sigeps136g_mod
      contains
      subroutine sigeps136g(                                                   &
        nel    ,matparam,rho    ,dmg     ,thk0   ,                             &
        depsxx ,depsyy ,depsxy  ,depsyz  ,depszx ,                             &                             
        depbxx ,depbyy ,depbxy  ,                                              &
        sigoxx ,sigoyy ,sigoxy  ,sigoyz  ,sigozx ,                             &
        signxx ,signyy ,signxy  ,signyz  ,signzx ,                             &
        momnxx ,momnyy ,momnxy  ,                                              &
        ssp    ,et     ,gs      ,nuvar   ,uvar   ,                             &
        shf    ,pla    ,sigb    )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use precision_mod, only : WP
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        integer,                       intent(in)    :: nel      !< Number of elements in the group
        type(matparam_struct_),        intent(in)    :: matparam !< Material parameters data
        real(kind=WP), dimension(nel), intent(in)    :: rho      !< Density at current time
        real(kind=WP), dimension(nel,3),intent(inout):: dmg      !< Bending damage at current time
        real(kind=WP), dimension(nel), intent(in)    :: thk0     !< Initial thickness
        real(kind=WP), dimension(nel), intent(in)    :: depsxx   !< Membrane strain increment xx
        real(kind=WP), dimension(nel), intent(in)    :: depsyy   !< Membrane strain increment yy
        real(kind=WP), dimension(nel), intent(in)    :: depsxy   !< Membrane strain increment xy
        real(kind=WP), dimension(nel), intent(in)    :: depsyz   !< Membrane strain increment yz
        real(kind=WP), dimension(nel), intent(in)    :: depszx   !< Membrane strain increment zx
        real(kind=WP), dimension(nel), intent(in)    :: depbxx   !< Bending curvature increment xx
        real(kind=WP), dimension(nel), intent(in)    :: depbyy   !< Bending curvature increment yy
        real(kind=WP), dimension(nel), intent(in)    :: depbxy   !< Bending curvature increment xy
        real(kind=WP), dimension(nel), intent(in)    :: sigoxx   !< Membrane force xx at previous time step
        real(kind=WP), dimension(nel), intent(in)    :: sigoyy   !< Membrane force yy at previous time step
        real(kind=WP), dimension(nel), intent(in)    :: sigoxy   !< Membrane force xy at previous time step
        real(kind=WP), dimension(nel), intent(in)    :: sigoyz   !< Membrane force yz at previous time step
        real(kind=WP), dimension(nel), intent(in)    :: sigozx   !< Membrane force zx at previous time step
        real(kind=WP), dimension(nel), intent(inout) :: signxx   !< Membrane force xx at current time step
        real(kind=WP), dimension(nel), intent(inout) :: signyy   !< Membrane force yy at current time step
        real(kind=WP), dimension(nel), intent(inout) :: signxy   !< Membrane force xy at current time step
        real(kind=WP), dimension(nel), intent(inout) :: signyz   !< Membrane force yz at current time step
        real(kind=WP), dimension(nel), intent(inout) :: signzx   !< Membrane force zx at current time step
        real(kind=WP), dimension(nel), intent(inout) :: momnxx   !< Bending moment xx at current time step
        real(kind=WP), dimension(nel), intent(inout) :: momnyy   !< Bending moment yy at current time step
        real(kind=WP), dimension(nel), intent(inout) :: momnxy   !< Bending moment xy at current time
        real(kind=WP), dimension(nel), intent(inout) :: ssp      !< Current sound speed
        real(kind=WP), dimension(nel), intent(inout) :: et       !< Hourglass stabilization variable
        real(kind=WP), dimension(nel), intent(inout) :: gs       !< Correction factor for transverse shear
        integer,                       intent(in)    :: nuvar    !< Number of user variables
        real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< User variables at current time step
        real(kind=WP), dimension(nel), intent(in)    :: shf      !< Shear correction factor force coefficient
        real(kind=WP), dimension(nel), intent(inout) :: pla      !< Array of plastic strains for post-processing and output
        real(kind=WP), dimension(nel,5),intent(inout) :: sigb  !< Array of back stresses for post-processing and output
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i, ii, nindx1, nindx2, nindx3, indx1(nel), indx2(nel),      &
          indx3(nel), iter(nel), temp_all_indices(nel)
        real(kind=WP) :: young, nu, shear, lambda_m, mu_m, f_t, f_c, gamma,    &
          dmax1, dmax2, Dm11, Dm12, cm, sig_y(2), omega_x(2), omega_y(2),      &
          rho_x(2), rho_y(2), eeq, nueq
        real(kind=WP), dimension(nel) :: lambda_b, mu_b, k0, cb
        real(kind=WP), dimension(nel) :: xi_tr, epbxx, epbyy, epbxy, xi_kap1,  &
          xi_kap2, Db11, Db12, Db33, mfx_pos, mfx_neg, mfy_pos,mfy_neg,        &
          dmfx_pos,dmfx_neg, dmfy_pos, dmfy_neg, f1p, f2p,center, radius,      &
          kappa1, kappa2, tr_kb, Y1, Y2, phi_b_pos,phi_b_neg,d1_trial,d2_trial,&
          thetap, cos_thetap,sin_thetap, momn1, momn2,A11, A12, A21, A22, b1,  &
          b2, lam_I, lam_II, det2, dfI_dMx, dfI_dMy, dfI_dMxy, dfII_dMx,       &
          dfII_dMy,dfII_dMxy,depsp_x,depsp_y,dfI_dNx, dfI_dNy, dfII_dNx,       &
          dfII_dNy, dkp_x, dkp_y,dkp_xy, den
        integer, parameter :: nmax = 100
        real(kind=WP), parameter :: tol = 1d-8
        real(kind=WP), parameter :: xi_neg = -1.0d0
        real(kind=WP), parameter :: xi_pos =  1.0d0
        logical, dimension(nel) :: active_elements_mask1,active_elements_mask2,&
          active_elements_mask3
        logical :: converged
!
        !=======================================================================
        !< - Initialisation of computation on time step
        !=======================================================================
!
        !< Recovering real model parameters
        young      = matparam%young           !< Concrete Young's modulus
        nu         = matparam%nu              !< Concrete Poisson's ratio
        shear      = matparam%shear           !< Concrete shear modulus
        lambda_m   = matparam%uparam(1)       !< Membrane Lamé parameter
        mu_m       = matparam%uparam(2)       !< Membrane shear modulus
        f_t        = matparam%uparam(3)       !< Concrete Tensile strength
        f_c        = matparam%uparam(4)       !< Concrete Compressive strength
        gamma      = matparam%uparam(5)       !< Concrete damage decay parameter
        dmax1      = matparam%uparam(6)       !< Maximum damage for positive bending
        dmax2      = matparam%uparam(7)       !< Maximum damage for negative bending
        sig_y(1)   = matparam%uparam(8)       !< Yield stress of the reinforcement in lower layer
        omega_x(1) = matparam%uparam(9)       !< Reinforcement ratio in x direction for lower layer
        omega_y(1) = matparam%uparam(10)      !< Reinforcement ratio in y direction for lower layer
        rho_x(1)   = matparam%uparam(11)      !< Position in thickness of reinforcement in x direction for lower layer
        rho_y(1)   = matparam%uparam(12)      !< Position in thickness of reinforcement in y direction for lower layer
        sig_y(2)   = matparam%uparam(13)      !< Yield stress of the reinforcement in upper layer
        omega_x(2) = matparam%uparam(14)      !< Reinforcement ratio in x direction for upper layer
        omega_y(2) = matparam%uparam(15)      !< Reinforcement ratio in y direction for upper layer
        rho_x(2)   = matparam%uparam(16)      !< Position in thickness of reinforcement in x direction for upper layer
        rho_y(2)   = matparam%uparam(17)      !< Position in thickness of reinforcement in y direction for upper layer
        cm         = matparam%uparam(18)      !< Prager hardening parameter for membrane forces
        cb(1:nel)  = matparam%uparam(19)      !< Prager hardening parameter for bending moments
        lambda_b(1:nel) = matparam%uparam(20) !< Bending Lamé parameter
        mu_b(1:nel) = matparam%uparam(21)     !< Bending shear modulus
        eeq        = matparam%uparam(22)      !< Steel Young's modulus
        nueq       = matparam%uparam(23)      !< Steel Poisson's ratio
!
        !< Computation of some real parameters
        Dm11 = lambda_m + two*mu_m                         !< Membrane stiffness component D11 = lambda + 2*mu
        Dm12 = lambda_m                                    !< Membrane stiffness component D12 = lambda
        gs(1:nel) = shear*shf(1:nel)                       !< Correction factor for transverse shear 
        lambda_b(1:nel) = lambda_b(1:nel)*thk0(1:nel)*one_over_12 !< Bending Lamé parameter
        mu_b(1:nel) = mu_b(1:nel)*thk0(1:nel)*one_over_12  !< Bending shear modulus
        Db11(1:nel) = lambda_b(1:nel) + two*mu_b(1:nel)    !< Bending stiffness component Db11 = lambda_b + 2*mu_b
        Db12(1:nel) = lambda_b(1:nel)                      !< Bending stiffness component Db12 = lambda_b
        Db33(1:nel) = mu_b(1:nel)                          !< Bending stiffness component Db33 = mu_b
        cb(1:nel)   = cb(1:nel)*thk0(1:nel)*one_over_12    !< Prager hardening parameter for bending moments
        !< Bending damage threshold 
        k0(1:nel) = (one-gamma)*half*(lambda_b(1:nel) + two*mu_b(1:nel)) *     &
                    ((two*f_t*(one - nueq*nueq)) / (eeq*thk0(1:nel)))**2
        where(k0(1:nel) <= zero) k0(1:nel) = ep20
 !
        !< Recompute the bending curvature tensor at current time step
        ! -> Total bending curvature at current time step 
        uvar(1:nel,1) = uvar(1:nel,1) + depbxx(1:nel)
        uvar(1:nel,2) = uvar(1:nel,2) + depbyy(1:nel)
        uvar(1:nel,3) = uvar(1:nel,3) + depbxy(1:nel)
        !< Elastic bending curvature increment
        epbxx(1:nel)  = uvar(1:nel,1) - uvar(1:nel,6)
        epbyy(1:nel)  = uvar(1:nel,2) - uvar(1:nel,7)
        epbxy(1:nel)  = uvar(1:nel,3) - uvar(1:nel,8)
        !< Denormalize the bending damage variables
        dmg(1:nel,2)  = dmg(1:nel,2)*dmax1
        dmg(1:nel,3)  = dmg(1:nel,3)*dmax2
!
        !=======================================================================
        !< - COMPUTATION OF THE MEMBRANE TRIAL STRESS TENSOR
        !=======================================================================
        ! -> Membrane stresses
        signxx(1:nel) = sigoxx(1:nel) +  Dm11*depsxx(1:nel) + Dm12*depsyy(1:nel)                                      
        signyy(1:nel) = sigoyy(1:nel) +  Dm12*depsxx(1:nel) + Dm11*depsyy(1:nel)
        signxy(1:nel) = sigoxy(1:nel) +  mu_m*depsxy(1:nel)
        signyz(1:nel) = sigoyz(1:nel) + gs(1:nel)*depsyz(1:nel)
        signzx(1:nel) = sigozx(1:nel) + gs(1:nel)*depszx(1:nel)
        ! -> Hourglass stabilization variable
        et(1:nel) = one
        ! -> Sound speed
        ssp(1:nel) = sqrt((young/(one - nu*nu))/rho(1:nel))
!
        !=======================================================================
        !< - COMPUTATION OF THE TRIAL BENDING STRESSES
        !=======================================================================
        momnxx(1:nel) = (lambda_b(1:nel) + two*mu_b(1:nel))*epbxx(1:nel) +     &
                                            lambda_b(1:nel)*epbyy(1:nel)
        momnyy(1:nel) = (lambda_b(1:nel) + two*mu_b(1:nel))*epbyy(1:nel) +     &
                                            lambda_b(1:nel)*epbxx(1:nel)
        momnxy(1:nel) = mu_b(1:nel)*epbxy(1:nel)
!
        !=======================================================================
        !< - COMPUTATION OF THE LIMIT MOMENT FOR THE PLASTICITY CRITERIA
        !=======================================================================
        !< Add kinematic hardening contribution and nondimensionalize the forces 
        !  and moments by the tensile strength
        signxx(1:nel) = signxx(1:nel) - sigb(1:nel,1)
        signyy(1:nel) = signyy(1:nel) - sigb(1:nel,2)
        momnxx(1:nel) = momnxx(1:nel) - sigb(1:nel,3)
        momnyy(1:nel) = momnyy(1:nel) - sigb(1:nel,4)
        momnxy(1:nel) = momnxy(1:nel) - sigb(1:nel,5)
        do i = 1, nel
          !< Positive bending moment for the reinforcement in x direction
          call calc_M(signxx(i),f_c,sig_y,omega_x,rho_x,xi_pos,                &
            mfx_pos(i),dmfx_pos(i))
          !< Negative bending moment for the reinforcement in x direction
          call calc_M(signxx(i),f_c,sig_y,omega_x,rho_x,xi_neg,                &
            mfx_neg(i),dmfx_neg(i))
          !< Positive bending moment for the reinforcement in y direction
          call calc_M(signyy(i),f_c,sig_y,omega_y,rho_y,xi_pos,                &
            mfy_pos(i),dmfy_pos(i))
          !< Negative bending moment for the reinforcement in y direction
          call calc_M(signyy(i),f_c,sig_y,omega_y,rho_y,xi_neg,                &
            mfy_neg(i),dmfy_neg(i)) 
        enddo
!
        !=======================================================================
        !< - COMPUTATION OF THE MEMBRANE-BENDING PLASTICITY
        !=======================================================================
        !< Computation of the plasticity criteria
        f1p(1:nel) = -(momnxx(1:nel) - mfx_pos(1:nel))*(momnyy(1:nel) -        &
                                  mfy_pos(1:nel)) + momnxy(1:nel)*momnxy(1:nel)
        f2p(1:nel) = -(momnxx(1:nel) - mfx_neg(1:nel))*(momnyy(1:nel) -        &
                                  mfy_neg(1:nel)) + momnxy(1:nel)*momnxy(1:nel)
!
        !< Index of the yielding elements
        active_elements_mask1(1:nel) = (f1p(1:nel) >= tol .and. f2p(1:nel) >= tol)
        nindx1 = COUNT(active_elements_mask1(1:nel))
        active_elements_mask2(1:nel) = (f1p(1:nel) >= tol .and. f2p(1:nel) <  tol)
        nindx2 = COUNT(active_elements_mask2(1:nel))
        active_elements_mask3(1:nel) = (f1p(1:nel) <  tol .and. f2p(1:nel) >= tol)
        nindx3 = COUNT(active_elements_mask3(1:nel))
        temp_all_indices(1:nel) = [(i, i=1,nel)]
!
        !-----------------------------------------------------------------------
        !< Both criteria active : loop over the indices of the active criteria 
        !  to compute the plastic correction
        !-----------------------------------------------------------------------
        if (nindx1 > 0 .or. nindx2 > 0 .or. nindx3 > 0) then
!
          !< Extract the indices of the yielding elements
          indx1(1:nindx1) = PACK(temp_all_indices(1:nel),                      &
                            active_elements_mask1(1:nel))
          indx2(1:nindx2) = PACK(temp_all_indices(1:nel),                      &
                            active_elements_mask2(1:nel))
          indx3(1:nindx3) = PACK(temp_all_indices(1:nel),                      &
                            active_elements_mask3(1:nel))
!
          !< Initialisation of the iteration counter and convergence flag
          iter(1:nel) = 0
          converged = .false.
!
          !< Loop over yielding elements
          do while (.not. converged) 
!
            !<------------------------------------------------------------------
            !< Both criteria active
            !<------------------------------------------------------------------
            !< Loop over yielding elements
#include "vectorize.inc"
            do ii = 1,nindx1
              i = indx1(ii)
              !< Normal derivatives of the criteria f_I 
              dfI_dMx(ii)  = -(momnyy(i) - mfy_pos(i))
              dfI_dMy(ii)  = -(momnxx(i) - mfx_pos(i))
              dfI_dMxy(ii) =  two * momnxy(i)
              dfI_dNx(ii)  =  (momnyy(i) - mfy_pos(i)) * dmfx_pos(i)
              dfI_dNy(ii)  =  (momnxx(i) - mfx_pos(i)) * dmfy_pos(i)
              !< Normal derivatives of the criteria f_II
              dfII_dMx(ii)  = -(momnyy(i) - mfy_neg(i))
              dfII_dMy(ii)  = -(momnxx(i) - mfx_neg(i))
              dfII_dMxy(ii) =  two * momnxy(i)
              dfII_dNx(ii)  =  (momnyy(i) - mfy_neg(i)) * dmfx_neg(i)
              dfII_dNy(ii)  =  (momnxx(i) - mfx_neg(i)) * dmfy_neg(i)
              !< System 2x2 : [A]{lam} = {b}
              A11(ii) =                                                        &
                  dfI_dNx(ii)*(dfI_dNx(ii)*(Dm11 + cm) + dfI_dNy(ii)*Dm12) +   &
                  dfI_dNy(ii)*(dfI_dNx(ii)*Dm12 + dfI_dNy(ii)*(Dm11 + cm)) +   &
                  dfI_dMx(ii)*(dfI_dMx(ii)*(Db11(i) + cb(i)) +                 &
                           dfI_dMy(ii)*Db12(i))/thk0(i)   +                    &
                  dfI_dMy(ii)*(dfI_dMx(ii)*Db12(i) +                           &
                           dfI_dMy(ii)*(Db11(i) + cb(i)))/thk0(i) +            &
                  dfI_dMxy(ii)*(dfI_dMxy(ii)*(Db33(i) + cb(i)))/thk0(i)
              A12(ii) =                                                        &
                  dfI_dNx(ii)*(dfII_dNx(ii)*(Dm11 + cm) + dfII_dNy(ii)*Dm12)+  &
                  dfI_dNy(ii)*(dfII_dNx(ii)*Dm12 + dfII_dNy(ii)*(Dm11 + cm))+  &
                  dfI_dMx(ii)*(dfII_dMx(ii)*(Db11(i) + cb(i)) +                &
                           dfII_dMy(ii)*Db12(i))/thk0(i) +                     &
                  dfI_dMy(ii)*(dfII_dMx(ii)*Db12(i) +                          &
                           dfII_dMy(ii)*(Db11(i) + cb(i)))/thk0(i) +           &
                  dfI_dMxy(ii)*(dfII_dMxy(ii)*(Db33(i) + cb(i)))/thk0(i)
              A21(ii) =                                                        &
                  dfII_dNx(ii)*(dfI_dNx(ii)*(Dm11 + cm) + dfI_dNy(ii)*Dm12) +  &
                  dfII_dNy(ii)*(dfI_dNx(ii)*Dm12 + dfI_dNy(ii)*(Dm11 + cm)) +  &
                  dfII_dMx(ii)*(dfI_dMx(ii)*(Db11(i) + cb(i)) +                &
                            dfI_dMy(ii)*Db12(i))/thk0(i) +                     &
                  dfII_dMy(ii)*(dfI_dMx(ii)*Db12(i) +                          &
                            dfI_dMy(ii)*(Db11(i) + cb(i)))/thk0(i) +           &
                  dfII_dMxy(ii)*(dfI_dMxy(ii)*(Db33(i) + cb(i)))/thk0(i)
              A22(ii) =                                                        &
                  dfII_dNx(ii)*(dfII_dNx(ii)*(Dm11 + cm)+dfII_dNy(ii)*Dm12)+   &
                  dfII_dNy(ii)*(dfII_dNx(ii)*Dm12+dfII_dNy(ii)*(Dm11 + cm))+   &
                  dfII_dMx(ii)*(dfII_dMx(ii)*(Db11(i) + cb(i)) +               &
                            dfII_dMy(ii)*Db12(i))/thk0(i)  +                   &
                  dfII_dMy(ii)*(dfII_dMx(ii)*Db12(i) +                         &
                            dfII_dMy(ii)*(Db11(i) + cb(i)))/thk0(i) +          &
                  dfII_dMxy(ii)*(dfII_dMxy(ii)*(Db33(i) + cb(i)))/thk0(i)
              !< Right-hand side of the system
              b1(ii) = f1p(i) 
              b2(ii) = f2p(i)
              !< Compute inverse of A, and lambda_I, lambda_II
              det2(ii) = A11(ii)*A22(ii) - A12(ii)*A21(ii)
              if (abs(det2(ii)) < em10) then
                !< Matrix nearly singular: use single criterion
                if (f1p(i) > f2p(i)) then
                  lam_I(ii) = f1p(i) / max(abs(A11(ii)), em20)
                  lam_II(ii) = zero
                else
                  lam_I(ii) = zero
                  lam_II(ii) = f2p(i) / max(abs(A22(ii)), em20)
                endif
              else
                lam_I(ii)  = (b1(ii)*A22(ii) - b2(ii)*A12(ii)) / det2(ii)
                lam_II(ii) = (A11(ii)*b2(ii) - A21(ii)*b1(ii)) / det2(ii)
              endif
              !< Compute the iteration plastic strain increment 
              depsp_x(ii) = lam_I(ii)*dfI_dNx(ii)   + lam_II(ii)*dfII_dNx(ii)
              depsp_y(ii) = lam_I(ii)*dfI_dNy(ii)   + lam_II(ii)*dfII_dNy(ii)
              dkp_x(ii)   = (lam_I(ii)*dfI_dMx(ii)  + lam_II(ii)*dfII_dMx(ii)) &
                                                                     / thk0(i)
              dkp_y(ii)   = (lam_I(ii)*dfI_dMy(ii)  + lam_II(ii)*dfII_dMy(ii)) &
                                                                     / thk0(i)
              dkp_xy(ii)  = (lam_I(ii)*dfI_dMxy(ii) + lam_II(ii)*dfII_dMxy(ii))& 
                                                                     / thk0(i)
              !< Update integrated plastic strains for post-processing and output
              uvar(i,4) = uvar(i,4) + depsp_x(ii)
              uvar(i,5) = uvar(i,5) + depsp_y(ii)
              uvar(i,6) = uvar(i,6) + dkp_x(ii)
              uvar(i,7) = uvar(i,7) + dkp_y(ii)
              uvar(i,8) = uvar(i,8) + dkp_xy(ii)
              !< Update equivalent plastic strain for output and post-processing
              pla(i)    = sqrt(uvar(i,4)**2 + uvar(i,5)**2 +                   &
                          (thk0(i)*uvar(i,6))**2 + (thk0(i)*uvar(i,7))**2 +    &
                          half*(thk0(i)*uvar(i,8))**2)
              !< Update of the membrane stresses and bending moments
              signxx(i) = signxx(i) - Dm11*depsp_x(ii)  - Dm12*depsp_y(ii)
              signyy(i) = signyy(i) - Dm12*depsp_x(ii)  - Dm11*depsp_y(ii)
              momnxx(i) = momnxx(i) - Db11(i)*dkp_x(ii) - Db12(i)*dkp_y(ii)
              momnyy(i) = momnyy(i) - Db12(i)*dkp_x(ii) - Db11(i)*dkp_y(ii)
              momnxy(i) = momnxy(i) - Db33(i)*dkp_xy(ii)
              !< Update kinematic hardening variables
              ! -> Remove previous contribution of the back-stress to the trial stress
              ! -> Update of the back-stress with the new plastic strain increment
              sigb(i,1) = sigb(i,1) + cm*depsp_x(ii)
              sigb(i,2) = sigb(i,2) + cm*depsp_y(ii)
              sigb(i,3) = sigb(i,3) + cb(i)*dkp_x(ii)
              sigb(i,4) = sigb(i,4) + cb(i)*dkp_y(ii)
              sigb(i,5) = sigb(i,5) + cb(i)*dkp_xy(ii)
              !< Update the limit moments for the plasticity criteria
              !< Positive bending moment for the reinforcement in x direction
              call calc_M(signxx(i),f_c,sig_y,omega_x,rho_x,xi_pos,            &
                mfx_pos(i),dmfx_pos(i))
              !< Negative bending moment for the reinforcement in x direction
              call calc_M(signxx(i),f_c,sig_y,omega_x,rho_x,xi_neg,            &
                mfx_neg(i),dmfx_neg(i))
              !< Positive bending moment for the reinforcement in y direction
              call calc_M(signyy(i),f_c,sig_y,omega_y,rho_y,xi_pos,            &
                mfy_pos(i),dmfy_pos(i))
              !< Negative bending moment for the reinforcement in y direction
              call calc_M(signyy(i),f_c,sig_y,omega_y,rho_y,xi_neg,            &
                mfy_neg(i),dmfy_neg(i)) 
              !< Update the plasticity criteria
              f1p(i) = -(momnxx(i) - mfx_pos(i))*(momnyy(i) - mfy_pos(i)) +    &
                                                      momnxy(i)*momnxy(i)
              f2p(i) = -(momnxx(i) - mfx_neg(i))*(momnyy(i) - mfy_neg(i)) +    &
                                                      momnxy(i)*momnxy(i)
              !< Increment the iteration counter
              iter(i) = iter(i) + 1
            enddo
!
            !<------------------------------------------------------------------
            !< Only the first criterion active (positive bending)
            !<------------------------------------------------------------------
#include "vectorize.inc"
            do ii = 1,nindx2
              i = indx2(ii)
              !< Normal derivatives of the criteria f_I 
              dfI_dMx(ii)  = -(momnyy(i) - mfy_pos(i))
              dfI_dMy(ii)  = -(momnxx(i) - mfx_pos(i))
              dfI_dMxy(ii) =  two * momnxy(i)
              dfI_dNx(ii)  =  (momnyy(i) - mfy_pos(i)) * dmfx_pos(i)
              dfI_dNy(ii)  =  (momnxx(i) - mfx_pos(i)) * dmfy_pos(i)
              !< Denominator of lambda_I
              den(ii) =                                                        &
                  dfI_dNx(ii)*((Dm11 + cm)*dfI_dNx(ii) + Dm12*dfI_dNy(ii)) +   & 
                  dfI_dNy(ii)*(Dm12*dfI_dNx(ii) + (Dm11 + cm)*dfI_dNy(ii)) +   &
                  dfI_dMx(ii)*((Db11(i) + cb(i))*dfI_dMx(ii) +                 &
                                Db12(i)*dfI_dMy(ii))/thk0(i) +                 &
                  dfI_dMy(ii)*(Db12(i)*dfI_dMx(ii) +                           &
                              (Db11(i) + cb(i))*dfI_dMy(ii))/thk0(i) +         &
                  dfI_dMxy(ii)*(Db33(i) + cb(i))*dfI_dMxy(ii)/thk0(i)
              !< Plastic multiplier
              den(ii) = sign(max(abs(den(ii)),em20),den(ii))      
              lam_I(ii) = f1p(i) / den(ii)
              !< Computation of plastic strains
              depsp_x(ii) = lam_I(ii) * dfI_dNx(ii)
              depsp_y(ii) = lam_I(ii) * dfI_dNy(ii)
              dkp_x(ii)   = lam_I(ii) * dfI_dMx(ii) / thk0(i)
              dkp_y(ii)   = lam_I(ii) * dfI_dMy(ii) / thk0(i)
              dkp_xy(ii)  = lam_I(ii) * dfI_dMxy(ii) / thk0(i)
              !< Update integrated plastic strains for post-processing and output
              uvar(i,4) = uvar(i,4) + depsp_x(ii)
              uvar(i,5) = uvar(i,5) + depsp_y(ii)
              uvar(i,6) = uvar(i,6) + dkp_x(ii)
              uvar(i,7) = uvar(i,7) + dkp_y(ii)
              uvar(i,8) = uvar(i,8) + dkp_xy(ii)
              !< Update equivalent plastic strain for output and post-processing
              pla(i)    = sqrt(uvar(i,4)**2 + uvar(i,5)**2 +                   &
                          (thk0(i)*uvar(i,6))**2 + (thk0(i)*uvar(i,7))**2 +    &
                          half*(thk0(i)*uvar(i,8))**2)
              !< Update of the membrane stresses and bending moments
              signxx(i) = signxx(i) - Dm11*depsp_x(ii)  - Dm12*depsp_y(ii)
              signyy(i) = signyy(i) - Dm12*depsp_x(ii)  - Dm11*depsp_y(ii)
              momnxx(i) = momnxx(i) - Db11(i)*dkp_x(ii) - Db12(i)*dkp_y(ii)
              momnyy(i) = momnyy(i) - Db12(i)*dkp_x(ii) - Db11(i)*dkp_y(ii)
              momnxy(i) = momnxy(i) - Db33(i)*dkp_xy(ii)
              !< Update kinematic hardening variables
              ! -> Remove previous contribution of the back-stress to the trial stress
              ! -> Update of the back-stress with the new plastic strain increment
              sigb(i,1) = sigb(i,1) + cm*depsp_x(ii)
              sigb(i,2) = sigb(i,2) + cm*depsp_y(ii)
              sigb(i,3) = sigb(i,3) + cb(i)*dkp_x(ii)
              sigb(i,4) = sigb(i,4) + cb(i)*dkp_y(ii)
              sigb(i,5) = sigb(i,5) + cb(i)*dkp_xy(ii)
              !< Update the limit moments for the plasticity criteria
              !< Positive bending moment for the reinforcement in x direction
              call calc_M(signxx(i),f_c,sig_y,omega_x,rho_x,xi_pos,            &
                mfx_pos(i),dmfx_pos(i))
              !< Negative bending moment for the reinforcement in x direction
              call calc_M(signxx(i),f_c,sig_y,omega_x,rho_x,xi_neg,            &
                mfx_neg(i),dmfx_neg(i))
              !< Positive bending moment for the reinforcement in y direction
              call calc_M(signyy(i),f_c,sig_y,omega_y,rho_y,xi_pos,            &
                mfy_pos(i),dmfy_pos(i))
              !< Negative bending moment for the reinforcement in y direction
              call calc_M(signyy(i),f_c,sig_y,omega_y,rho_y,xi_neg,            &
                mfy_neg(i),dmfy_neg(i)) 
              !< Update the plasticity criteria
              f1p(i) = -(momnxx(i) - mfx_pos(i))*(momnyy(i) - mfy_pos(i)) +    &
                                                      momnxy(i)*momnxy(i)
              f2p(i) = -(momnxx(i) - mfx_neg(i))*(momnyy(i) - mfy_neg(i)) +    &
                                                      momnxy(i)*momnxy(i)
              !< Increment the iteration counter
              iter(i) = iter(i) + 1
            enddo
!
            !<------------------------------------------------------------------
            !< Only the second criterion active (negative bending)
            !<------------------------------------------------------------------
#include "vectorize.inc"
            do ii = 1,nindx3
              i = indx3(ii)
              !< Normal derivatives of the criteria f_II
              dfII_dMx(ii)  = -(momnyy(i) - mfy_neg(i))
              dfII_dMy(ii)  = -(momnxx(i) - mfx_neg(i))
              dfII_dMxy(ii) =  two * momnxy(i)
              dfII_dNx(ii)  =  (momnyy(i) - mfy_neg(i)) * dmfx_neg(i)
              dfII_dNy(ii)  =  (momnxx(i) - mfx_neg(i)) * dmfy_neg(i)
              !< Denominator of lambda_II
              den(ii) =                                                        &
                  dfII_dNx(ii)*((Dm11 + cm)*dfII_dNx(ii) + Dm12*dfII_dNy(ii))+ &
                  dfII_dNy(ii)*(Dm12*dfII_dNx(ii) +(Dm11 + cm)*dfII_dNy(ii)) + &
                  dfII_dMx(ii)*((Db11(i) + cb(i))*dfII_dMx(ii) +               &
                                 Db12(i)*dfII_dMy(ii))/thk0(i) +               &
                  dfII_dMy(ii)*(Db12(i)*dfII_dMx(ii) +                         &
                       (Db11(i) + cb(i))*dfII_dMy(ii))/thk0(i) +               &
                  dfII_dMxy(ii)*(Db33(i) + cb(i))*dfII_dMxy(ii)/thk0(i)
              !< Plastic multiplier
              den(ii) = sign(max(abs(den(ii)),em20),den(ii))      
              lam_II(ii) = f2p(i) / den(ii)      
              !< Computation of plastic strains
              depsp_x(ii) = lam_II(ii) * dfII_dNx(ii)
              depsp_y(ii) = lam_II(ii) * dfII_dNy(ii)
              dkp_x(ii)   = lam_II(ii) * dfII_dMx(ii) / thk0(i)
              dkp_y(ii)   = lam_II(ii) * dfII_dMy(ii) / thk0(i)
              dkp_xy(ii)  = lam_II(ii) * dfII_dMxy(ii) / thk0(i)
              !< Update integrated plastic strains for post-processing and output
              uvar(i,4) = uvar(i,4) + depsp_x(ii)
              uvar(i,5) = uvar(i,5) + depsp_y(ii)
              uvar(i,6) = uvar(i,6) + dkp_x(ii)
              uvar(i,7) = uvar(i,7) + dkp_y(ii)
              uvar(i,8) = uvar(i,8) + dkp_xy(ii)
              !< Update equivalent plastic strain for output and post-processing
              pla(i)    = sqrt(uvar(i,4)**2 + uvar(i,5)**2 +                   &
                          (thk0(i)*uvar(i,6))**2 + (thk0(i)*uvar(i,7))**2 +    &
                          half*(thk0(i)*uvar(i,8))**2)
              !< Update of the membrane stresses and bending moments
              signxx(i) = signxx(i) - Dm11*depsp_x(ii)  - Dm12*depsp_y(ii)
              signyy(i) = signyy(i) - Dm12*depsp_x(ii)  - Dm11*depsp_y(ii)
              momnxx(i) = momnxx(i) - Db11(i)*dkp_x(ii) - Db12(i)*dkp_y(ii)
              momnyy(i) = momnyy(i) - Db12(i)*dkp_x(ii) - Db11(i)*dkp_y(ii)
              momnxy(i) = momnxy(i) - Db33(i)*dkp_xy(ii)
              !< Update kinematic hardening variables
              ! -> Remove previous contribution of the back-stress to the trial stress
              ! -> Update of the back-stress with the new plastic strain increment
              sigb(i,1) = sigb(i,1) + cm*depsp_x(ii)
              sigb(i,2) = sigb(i,2) + cm*depsp_y(ii)
              sigb(i,3) = sigb(i,3) + cb(i)*dkp_x(ii)
              sigb(i,4) = sigb(i,4) + cb(i)*dkp_y(ii)
              sigb(i,5) = sigb(i,5) + cb(i)*dkp_xy(ii)
              !< Update the limit moments for the plasticity criteria
              !< Positive bending moment for the reinforcement in x direction
              call calc_M(signxx(i),f_c,sig_y,omega_x,rho_x,xi_pos,            &
                mfx_pos(i),dmfx_pos(i))
              !< Negative bending moment for the reinforcement in x direction
              call calc_M(signxx(i),f_c,sig_y,omega_x,rho_x,xi_neg,            &
                mfx_neg(i),dmfx_neg(i))
              !< Positive bending moment for the reinforcement in y direction
              call calc_M(signyy(i),f_c,sig_y,omega_y,rho_y,xi_pos,            &
                mfy_pos(i),dmfy_pos(i))
              !< Negative bending moment for the reinforcement in y direction
              call calc_M(signyy(i),f_c,sig_y,omega_y,rho_y,xi_neg,            &
                mfy_neg(i),dmfy_neg(i)) 
              !< Update the plasticity criteria
              f1p(i) = -(momnxx(i) - mfx_pos(i))*(momnyy(i) - mfy_pos(i)) +    &
                                                      momnxy(i)*momnxy(i)
              f2p(i) = -(momnxx(i) - mfx_neg(i))*(momnyy(i) - mfy_neg(i)) +    &
                                                        momnxy(i)*momnxy(i)
              !< Increment the iteration counter
              iter(i) = iter(i) + 1
            enddo
!
            !< Update the active elements mask and check convergence
            active_elements_mask1(1:nel) = (f1p(1:nel) >= tol .and.            &
                                            f2p(1:nel) >= tol .and.            &
                                            iter(1:nel) < nmax)
            nindx1 = COUNT(active_elements_mask1(1:nel))
            active_elements_mask2(1:nel) = (f1p(1:nel) >= tol .and.            &
                                            f2p(1:nel) <  tol .and.            &
                                            iter(1:nel) < nmax)
            nindx2 = COUNT(active_elements_mask2(1:nel))
            active_elements_mask3(1:nel) = (f1p(1:nel) <  tol .and.            &
                                            f2p(1:nel) >= tol .and.            &
                                            iter(1:nel) < nmax)
            nindx3 = COUNT(active_elements_mask3(1:nel))
            temp_all_indices(1:nel) = [(i, i=1,nel)]
            !< Extract the new indices of the yielding elements
            indx1(1:nindx1) = PACK(temp_all_indices(1:nel),                    &
                              active_elements_mask1(1:nel))
            indx2(1:nindx2) = PACK(temp_all_indices(1:nel),                    &
                              active_elements_mask2(1:nel))
            indx3(1:nindx3) = PACK(temp_all_indices(1:nel),                    &
                              active_elements_mask3(1:nel))

            converged = ((nindx1 == 0) .and. (nindx2 == 0) .and.               &
                         (nindx3 == 0)) .or. (MAXVAL(iter(1:nel)) >= nmax)
          enddo
        endif
! 
        !< Remove the contribution of the back-stress to the stresses and 
        !  unnormalize the stresses and moments by the tensile strength
        signxx(1:nel) = signxx(1:nel) + sigb(1:nel,1)
        signyy(1:nel) = signyy(1:nel) + sigb(1:nel,2)
        momnxx(1:nel) = momnxx(1:nel) + sigb(1:nel,3)
        momnyy(1:nel) = momnyy(1:nel) + sigb(1:nel,4)
        momnxy(1:nel) = momnxy(1:nel) + sigb(1:nel,5)
!
        !=======================================================================
        !< - COMPUTATION OF THE BENDING DAMAGE AND UPDATE OF THE BENDING MOMENTS
        !=======================================================================
        !< Update elastic curvatures for damage computation
        epbxx(1:nel) = uvar(1:nel,1) - uvar(1:nel,6)
        epbyy(1:nel) = uvar(1:nel,2) - uvar(1:nel,7)
        epbxy(1:nel) = uvar(1:nel,3) - uvar(1:nel,8)
        !< Trace of the bending curvature increment tensor
        tr_kb(1:nel) = epbxx(1:nel) + epbyy(1:nel)
        !< Eigenvalues of the bending curvature tensor
        center(1:nel) = (epbxx(1:nel) + epbyy(1:nel))/two
        radius(1:nel) = sqrt(((epbxx(1:nel)-epbyy(1:nel))*half)**2 +           &
                              (epbxy(1:nel)*half)**2)
        kappa1(1:nel) = center(1:nel) + radius(1:nel)
        kappa2(1:nel) = center(1:nel) - radius(1:nel)       
! 
        !< Computation of thermodynamic forces driving the bending damage
        !<----------------------------------------------------------------------
        !< Thermodynamic force Y1 (positive bending - tension inner face)
        Y1(1:nel) = half*lambda_b(1:nel) * max(tr_kb(1:nel), zero)**2          &
           + mu_b(1:nel) * (max(kappa1(1:nel), zero)**2 +                      &
                            max(kappa2(1:nel), zero)**2)
        phi_b_pos(1:nel) = Y1(1:nel)
        Y1(1:nel) = ((one - gamma)/(one + dmg(1:nel,2))**2) * Y1(1:nel)
        !< Thermodynamic force Y2 (negative bending - tension outer face)
        Y2(1:nel) = half*lambda_b(1:nel) * min(tr_kb(1:nel), zero)**2          &
           + mu_b(1:nel) * (min(kappa1(1:nel), zero)**2 +                      &
                            min(kappa2(1:nel), zero)**2)
        phi_b_neg(1:nel) = Y2(1:nel)
        Y2(1:nel) = ((one - gamma)/(one + dmg(1:nel,3))**2) * Y2(1:nel)
!
        !< Damage evolution and computation of the bending moments
        !< ---------------------------------------------------------------------
        !< Evolution law of the bending damage
        ! --> Positive bending, inner face
        where (Y1(1:nel) >= k0(1:nel))
          d1_trial(1:nel) = sqrt((phi_b_pos(1:nel)*(one-gamma))/k0(1:nel)) - one
          d1_trial(1:nel) = max(zero, min(d1_trial(1:nel), dmax1))
          dmg(1:nel,2)    = max(dmg(1:nel,2), d1_trial(1:nel) )
        endwhere
        ! --> Negative bending, outer face
        where (Y2(1:nel) >= k0(1:nel))
          d2_trial(1:nel) = sqrt((phi_b_neg(1:nel)*(one-gamma))/k0(1:nel)) - one
          d2_trial(1:nel) = max(zero, min(d2_trial(1:nel), dmax2))
          dmg(1:nel,3) = max(dmg(1:nel,3), d2_trial(1:nel))
        endwhere
        !< Softening factor for the moment Mxx and Myy
        xi_tr(1:nel) =                                                         &
         (one + gamma*merge(dmg(1:nel,2), dmg(1:nel,3), tr_kb(1:nel)  > zero))/&
         (one +       merge(dmg(1:nel,2), dmg(1:nel,3), tr_kb(1:nel)  > zero))
        xi_kap1(1:nel) =                                                       &
         (one + gamma*merge(dmg(1:nel,2), dmg(1:nel,3), kappa1(1:nel) > zero))/&
         (one +       merge(dmg(1:nel,2), dmg(1:nel,3), kappa1(1:nel) > zero))
        xi_kap2(1:nel) =                                                       &
         (one + gamma*merge(dmg(1:nel,2), dmg(1:nel,3), kappa2(1:nel) > zero))/&
         (one +       merge(dmg(1:nel,2), dmg(1:nel,3), kappa2(1:nel) > zero))
        !< Principal bending moments
        momn1(1:nel) = lambda_b(1:nel)*tr_kb(1:nel)*xi_tr(1:nel) +             &
                       two*mu_b(1:nel)*xi_kap1(1:nel)*kappa1(1:nel)
        momn2(1:nel) = lambda_b(1:nel)*tr_kb(1:nel)*xi_tr(1:nel) +             &
                       two*mu_b(1:nel)*xi_kap2(1:nel)*kappa2(1:nel)
        thetap(1:nel) = half * atan2(epbxy(1:nel), epbxx(1:nel) - epbyy(1:nel))
        cos_thetap(1:nel) = cos(thetap(1:nel))
        sin_thetap(1:nel) = sin(thetap(1:nel))
        !< Rotation of the principal bending moments to the global coordinate system
        momnxx(1:nel) = momn1(1:nel)*cos_thetap(1:nel)**2 +                    &
                        momn2(1:nel)*sin_thetap(1:nel)**2
        momnyy(1:nel) = momn1(1:nel)*sin_thetap(1:nel)**2 +                    &
                        momn2(1:nel)*cos_thetap(1:nel)**2
        momnxy(1:nel) = (momn1(1:nel) - momn2(1:nel))*                         &
                        sin_thetap(1:nel)*cos_thetap(1:nel)
! 
        !< Normalize the bending damage variables by the maximum damage 
        !  for output and post-processing
        dmg(1:nel,2) = dmg(1:nel,2)/dmax1
        dmg(1:nel,3) = dmg(1:nel,3)/dmax2
        !< Save maximum damage for output and post-processing
        dmg(1:nel,1) = max(dmg(1:nel,2), dmg(1:nel,3)) 
!
        !=======================================================================
        end subroutine sigeps136g
!
        !=======================================================================
        !< Computation of bending limit moment M_pos and its derivative w.r.t N
        !=======================================================================
        subroutine calc_M(                                                     &
          sigma    ,f_c      ,sig_y    ,omega    ,rho      ,xi      ,M_val    ,&
          dM_dN    )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use precision_mod, only: WP
          use constant_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          real(kind=WP), intent(in) :: sigma       !< Membrane stress in the direction of the bending moment
          real(kind=WP), intent(in) :: f_c         !< Concrete compressive strength
          real(kind=WP), intent(in) :: sig_y(2)    !< Yield stress of the reinforcement
          real(kind=WP), intent(in) :: omega(2)    !< Area of the reinforcement in x direction
          real(kind=WP), intent(in) :: rho(2)      !< Position of the reinforcement in x direction (normalized by the thickness)
          real(kind=WP), intent(in) :: xi          !< +1 for positive bending, -1 for negative bending
          real(kind=WP), intent(inout) :: M_val    !< Value of the bending moment for the given sigma
          real(kind=WP), intent(inout) :: dM_dN    !< Derivative of the bending moment w.r.t. N for the given sigma
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          real(kind=WP) :: N_lo, N_hi, N
          real(kind=WP) :: xi_inf, xi_sup
          real(kind=WP) :: gamma, rhoeq, denom, gamma1, gamma2
          real(kind=WP) :: a0, a1, a2
!
          !< Computation of the limits of N 
          gamma1 = (-omega(2)*sig_y(2))/(f_c)
          N_lo = xi*gamma1 - (one - xi*rho(1))*half
          gamma2 =  (omega(1)*sig_y(1))/(f_c)
          N_hi = xi*gamma2 - (one - xi*rho(2))*half
!
          !< Convert the normalized membrane stress to the equivalent normalized force N = sigma/f_c
          N = sigma/f_c
!
          !< Choose the corresponding xi_inf and xi_sup for the given N
          if (N <= N_lo) then
            xi_inf = -one
            xi_sup = -one
          elseif (N <= N_hi) then
            xi_inf = one
            xi_sup = -one
          else
            xi_inf = one
            xi_sup = one
          endif          
!
          !< Computation of the contribution of the reinforcement to the bending resistance
          gamma = (xi_sup*omega(2)*sig_y(2) + xi_inf*omega(1)*sig_y(1))/f_c
!
          !< Computation of the position of the equivalent reinforcement for the concrete contribution
          denom = xi_sup*omega(2) + xi_inf*omega(1)
          if (abs(denom) < em20) then
            rhoeq = zero
          else
            rhoeq = (xi_sup*omega(2)*rho(2) + xi_inf*omega(1)*rho(1)) / denom
          endif
!
          !< Computation of the bending moment coefficient for the given N
          a0 =  (one - xi*rhoeq)*gamma*half - xi*half*(gamma)**2
          a1 =  gamma - xi*half
          a2 = -xi*half
!
          !< Computation of the bending moment and its derivative for the given N
          M_val = a0 + a1*N + a2*N**2
          M_val = M_val * f_c
          dM_dN = a1 + 2*a2*N
!
        end subroutine calc_M
!
      end module sigeps136g_mod
