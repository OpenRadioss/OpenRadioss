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
!||    elasticity_bimod_isotropic_mod   ../engine/source/materials/mat/mat131/elasticity/elasticity_bimod_isotropic.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_trial_stress      ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||====================================================================
      module elasticity_bimod_isotropic_mod
! \brief Compute bimodular isotropic elastic stress for /MAT/LAW131
! \details Compute the elastic stress tensor using bimodular isotropic elasticity
!          (different moduli in tension and compression) for /MAT/LAW131.
      contains
!||====================================================================
!||    elasticity_bimod_isotropic    ../engine/source/materials/mat/mat131/elasticity/elasticity_bimod_isotropic.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_trial_stress   ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    matparam_def_mod              ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine elasticity_bimod_isotropic(                                   &
        matparam ,nel      ,eltype   ,ieos     ,rho      ,dpdm     ,           &
        depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,           &
        sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,           &
        signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,           &
        shf      ,cstf     ,soundsp  ,s13      ,s23      ,s43      ,           &
        young    )
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
        type(matparam_struct_),            intent(in)    :: matparam !< Material parameters data
        integer,                           intent(in)    :: nel      !< Number of elements in the group
        integer,                           intent(in)    :: eltype   !< Element type (1 for solids, 2 for shells)
        integer,                           intent(in)    :: ieos     !< Equation of state type
        real(kind=WP), dimension(nel),     intent(in)    :: rho      !< Density
        real(kind=WP), dimension(nel),     intent(in)    :: dpdm     !< Derivative of pressure with respect to volumetric strain
        real(kind=WP), dimension(nel),     intent(in)    :: depsxx   !< Strain increment component xx
        real(kind=WP), dimension(nel),     intent(in)    :: depsyy   !< Strain increment component yy
        real(kind=WP), dimension(nel),     intent(in)    :: depszz   !< Strain increment component zz
        real(kind=WP), dimension(nel),     intent(in)    :: depsxy   !< Strain increment component xy
        real(kind=WP), dimension(nel),     intent(in)    :: depsyz   !< Strain increment component yz
        real(kind=WP), dimension(nel),     intent(in)    :: depszx   !< Strain increment component zx
        real(kind=WP), dimension(nel),     intent(in)    :: sigoxx   !< Stress tensor component xx at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(in)    :: sigoyy   !< Stress tensor component yy at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(in)    :: sigozz   !< Stress tensor component zz at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(in)    :: sigoxy   !< Stress tensor component xy at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(in)    :: sigoyz   !< Stress tensor component yz at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(in)    :: sigozx   !< Stress tensor component zx at the beginning of the time step
        real(kind=WP), dimension(nel),     intent(inout) :: signxx   !< Stress tensor component xx at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(inout) :: signyy   !< Stress tensor component yy at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(inout) :: signzz   !< Stress tensor component zz at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(inout) :: signxy   !< Stress tensor component xy at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(inout) :: signyz   !< Stress tensor component yz at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(inout) :: signzx   !< Stress tensor component zx at the end of the time step (trial stress)
        real(kind=WP), dimension(nel),     intent(in)    :: shf      !< Shell thickness
        real(kind=WP), dimension(nel,6,6), intent(inout) :: cstf     !< Elastic stiffness matrix
        real(kind=WP), dimension(nel),     intent(inout) :: soundsp  !< Sound speed
        real(kind=WP), dimension(nel),     intent(inout) :: s13      !< Compliance matrix component for thickness update
        real(kind=WP), dimension(nel),     intent(inout) :: s23      !< Compliance matrix component for thickness update
        real(kind=WP), dimension(nel),     intent(inout) :: s43      !< Compliance matrix component for thickness update
        real(kind=WP), dimension(nel),     intent(inout) :: young    !< Young modulus
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i
        real(kind=WP) :: et,ec,nu,tt,tc
        real(kind=WP), dimension(nel) :: bulk,shear,lam,cii,cij,aii,aij,fac,p, &
          svm,triax
!===============================================================================
!
        !=======================================================================
        !< - Isotropic elasticity
        !=======================================================================
        !< Recover elastic parameter
        et = matparam%uparam(1)
        ec = matparam%uparam(2)
        nu = matparam%nu
        tt = matparam%uparam(3)
        tc = matparam%uparam(4)
        bulk(1:nel) = et/three/(one - two*nu)
        !< Young modulus
        young(1:nel) = et
        !< Solids
        if (eltype == 1) then
          !< Young modulus update for bimodular behavior     
          if (ec > zero) then
            p(1:nel) = -third*(sigoxx(1:nel) + sigoyy(1:nel) + sigozz(1:nel))
            svm(1:nel) =  half*(sigoyy(1:nel)-sigozz(1:nel))**2 +              &                 
                          half*(sigozz(1:nel)-sigoxx(1:nel))**2 +              & 
                          half*(sigoxx(1:nel)-sigoyy(1:nel))**2 +              &
                         three*(sigoxy(1:nel))**2               +              &
                         three*(sigoyz(1:nel))**2               +              &
                         three*(sigozx(1:nel))**2 
            svm(1:nel) = sqrt(svm(1:nel))
            triax(1:nel) = -p(1:nel)/max(svm(1:nel),em20)
            if (abs(tt - tc) < em20) then
              where (abs(triax(1:nel)) < em10)
                young(1:nel) = ec
              end where
            else
              fac(1:nel) = (triax(1:nel) - tc)/(tt - tc)
              fac(1:nel) = max(zero, min(one, fac(1:nel)))
              young(1:nel) = fac(1:nel)*et + (one - fac(1:nel))*ec
            endif
          endif
          !< Compute elastic stiffness matrix components
          bulk(1:nel) = young(1:nel)/three/(one - two*nu)
          shear(1:nel) = young(1:nel)/(two*(one + nu))
          lam(1:nel) = young(1:nel)*nu/(one + nu)/(one - two*nu)
          cii(1:nel) = lam(1:nel) + shear(1:nel)*two
          cij(1:nel) = lam(1:nel)
          !< Elastic stiffness matrix
          cstf(1:nel,1,1) =   cii(1:nel)
          cstf(1:nel,2,2) =   cii(1:nel)
          cstf(1:nel,3,3) =   cii(1:nel)
          cstf(1:nel,1,2) =   cij(1:nel)
          cstf(1:nel,1,3) =   cij(1:nel)
          cstf(1:nel,2,1) =   cij(1:nel)
          cstf(1:nel,2,3) =   cij(1:nel)
          cstf(1:nel,3,1) =   cij(1:nel)
          cstf(1:nel,3,2) =   cij(1:nel)
          cstf(1:nel,4,4) = shear(1:nel)
          cstf(1:nel,5,5) = shear(1:nel)
          cstf(1:nel,6,6) = shear(1:nel)
          !< Sound speed
          if (ieos > 0) then 
            soundsp(1:nel) = sqrt((dpdm(1:nel) +                               &
                                    four_over_3*shear(1:nel))/rho(1:nel))
          else
            soundsp(1:nel) = sqrt((bulk(1:nel)+                                &
                                    four_over_3*shear(1:nel))/rho(1:nel))
          endif
        !< Shells
        elseif (eltype == 2) then
          !< Young modulus update for bimodular behavior     
          if (ec > zero) then
            p(1:nel) = -third*(sigoxx(1:nel) + sigoyy(1:nel))
            svm(1:nel) = sigoxx(1:nel)**2 + sigoyy(1:nel)**2 -                 &
                         sigoxx(1:nel)*sigoyy(1:nel) + three*(sigoxy(1:nel)**2)
            svm(1:nel) = sqrt(svm(1:nel))
            triax(1:nel) = -p(1:nel)/max(svm(1:nel),em20)
            if (abs(tt - tc) < em20) then
              where (abs(triax(1:nel)) < em10)
                young(1:nel) = ec
              end where
            else
              fac(1:nel) = (triax(1:nel) - tc)/(tt - tc)
              fac(1:nel) = max(zero, min(one, fac(1:nel)))
              young(1:nel) = fac(1:nel)*et + (one - fac(1:nel))*ec
            endif
          endif
          !< Compute elastic stiffness matrix components
          aii(1:nel) = young(1:nel)/(one - nu*nu)
          aij(1:nel) = nu*aii(1:nel)
          shear(1:nel) = young(1:nel)/(two*(one + nu))
          !< Elastic stiffness matrix
          cstf(1:nel,1,1) = aii(1:nel)
          cstf(1:nel,2,2) = aii(1:nel)
          cstf(1:nel,1,2) = aij(1:nel)
          cstf(1:nel,2,1) = aij(1:nel)
          cstf(1:nel,4,4) = shear(1:nel)
          cstf(1:nel,5,5) = shear(1:nel)*shf(1:nel)
          cstf(1:nel,6,6) = shear(1:nel)*shf(1:nel)
          !< Compliance matrix components for thickness update
          s13(1:nel) = - matparam%nu / young(1:nel)
          s23(1:nel) = - matparam%nu / young(1:nel)
          s43(1:nel) = zero
          !< Sound speed
          soundsp(1:nel) = sqrt(cstf(1:nel,1,1)/rho(1:nel))
        endif 
!
        !=======================================================================
        !< Elastic trial stress computation
        !=======================================================================
#include "vectorize.inc"
        do i = 1,nel
          signxx(i) = sigoxx(i) + cstf(i,1,1)*depsxx(i) +                      &
                                  cstf(i,1,2)*depsyy(i) +                      &
                                  cstf(i,1,3)*depszz(i)
          signyy(i) = sigoyy(i) + cstf(i,2,1)*depsxx(i) +                      &
                                  cstf(i,2,2)*depsyy(i) +                      &
                                  cstf(i,2,3)*depszz(i)
          signzz(i) = sigozz(i) + cstf(i,3,1)*depsxx(i) +                      & 
                                  cstf(i,3,2)*depsyy(i) +                      & 
                                  cstf(i,3,3)*depszz(i)
          signxy(i) = sigoxy(i) + cstf(i,4,4)*depsxy(i)
          signyz(i) = sigoyz(i) + cstf(i,5,5)*depsyz(i)
          signzx(i) = sigozx(i) + cstf(i,6,6)*depszx(i)
        enddo
!
      end subroutine elasticity_bimod_isotropic
      end module elasticity_bimod_isotropic_mod
