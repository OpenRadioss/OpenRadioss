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
!||    elasticity_anisotropic_mod    ../engine/source/materials/mat/mat131/elasticity/elasticity_anisotropic.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_trial_stress   ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||====================================================================
      module elasticity_anisotropic_mod
! \brief Compute anisotropic elastic stress for /MAT/LAW131
! \details Compute the elastic stress tensor using anisotropic elasticity
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    elasticity_anisotropic        ../engine/source/materials/mat/mat131/elasticity/elasticity_anisotropic.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_trial_stress   ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    matparam_def_mod              ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine elasticity_anisotropic(                                       &
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
!===============================================================================
!
        !=======================================================================
        !< - Anisotropic elasticity
        !=======================================================================
        !< Young modulus
        young(1:nel) = matparam%young
        !< Solids
        if (eltype == 1) then     
          !< Elastic stiffness matrix
          cstf(1:nel,1,1) = matparam%uparam(1)
          cstf(1:nel,1,2) = matparam%uparam(2)
          cstf(1:nel,1,3) = matparam%uparam(3)
          cstf(1:nel,1,4) = matparam%uparam(4)
          cstf(1:nel,1,5) = matparam%uparam(5)
          cstf(1:nel,1,6) = matparam%uparam(6)
          cstf(1:nel,2,1) = matparam%uparam(2)
          cstf(1:nel,2,2) = matparam%uparam(7)
          cstf(1:nel,2,3) = matparam%uparam(8)
          cstf(1:nel,2,4) = matparam%uparam(9)
          cstf(1:nel,2,5) = matparam%uparam(10)
          cstf(1:nel,2,6) = matparam%uparam(11)
          cstf(1:nel,3,1) = matparam%uparam(3)
          cstf(1:nel,3,2) = matparam%uparam(8)
          cstf(1:nel,3,3) = matparam%uparam(12)
          cstf(1:nel,3,4) = matparam%uparam(13)
          cstf(1:nel,3,5) = matparam%uparam(14)
          cstf(1:nel,3,6) = matparam%uparam(15)
          cstf(1:nel,4,1) = matparam%uparam(4)
          cstf(1:nel,4,2) = matparam%uparam(9)
          cstf(1:nel,4,3) = matparam%uparam(13)
          cstf(1:nel,4,4) = matparam%uparam(16)
          cstf(1:nel,4,5) = matparam%uparam(17)
          cstf(1:nel,4,6) = matparam%uparam(18)
          cstf(1:nel,5,1) = matparam%uparam(5)
          cstf(1:nel,5,2) = matparam%uparam(10)
          cstf(1:nel,5,3) = matparam%uparam(14)
          cstf(1:nel,5,4) = matparam%uparam(17)
          cstf(1:nel,5,5) = matparam%uparam(19)
          cstf(1:nel,5,6) = matparam%uparam(20)
          cstf(1:nel,6,1) = matparam%uparam(6)
          cstf(1:nel,6,2) = matparam%uparam(11)
          cstf(1:nel,6,3) = matparam%uparam(15)
          cstf(1:nel,6,4) = matparam%uparam(18)
          cstf(1:nel,6,5) = matparam%uparam(20)
          cstf(1:nel,6,6) = matparam%uparam(21)
          !< Sound speed
          if (ieos > 0) then 
            soundsp(1:nel) = sqrt((dpdm(1:nel) +                               &
                                    four_over_3*matparam%shear)/rho(1:nel))
          else
            soundsp(1:nel) = sqrt(matparam%uparam(34)/rho(1:nel))
          endif
        !< Shells
        elseif (eltype == 2) then
          !< Elastic stiffness matrix
          cstf(1:nel,1,1) = matparam%uparam(22)
          cstf(1:nel,1,2) = matparam%uparam(23)
          cstf(1:nel,1,4) = matparam%uparam(24)
          cstf(1:nel,2,1) = matparam%uparam(23)
          cstf(1:nel,2,2) = matparam%uparam(25)
          cstf(1:nel,2,4) = matparam%uparam(26)
          cstf(1:nel,4,1) = matparam%uparam(24)
          cstf(1:nel,4,2) = matparam%uparam(26)
          cstf(1:nel,4,4) = matparam%uparam(27)
          cstf(1:nel,5,5) = matparam%uparam(28)*shf(1:nel)
          cstf(1:nel,5,6) = matparam%uparam(29)*shf(1:nel)
          cstf(1:nel,6,5) = matparam%uparam(29)*shf(1:nel)
          cstf(1:nel,6,6) = matparam%uparam(30)*shf(1:nel)
          !< Compliance matrix components for thickness update
          s13(1:nel) = matparam%uparam(31)
          s23(1:nel) = matparam%uparam(32)
          s43(1:nel) = matparam%uparam(33)
          !< Sound speed
          soundsp(1:nel) = sqrt(max(cstf(1:nel,1,1),cstf(1:nel,2,2),           &
                                        cstf(1:nel,4,4))/rho(1:nel))       
        endif
!
        !=======================================================================
        !< Elastic trial stress computation
        !=======================================================================
#include "vectorize.inc"
        do i = 1,nel
          signxx(i) = sigoxx(i) + cstf(i,1,1)*depsxx(i) +                      &
                                  cstf(i,1,2)*depsyy(i) +                      &
                                  cstf(i,1,3)*depszz(i) +                      &
                                  cstf(i,1,4)*depsxy(i) +                      &
                                  cstf(i,1,5)*depsyz(i) +                      &
                                  cstf(i,1,6)*depszx(i)
          signyy(i) = sigoyy(i) + cstf(i,2,1)*depsxx(i) +                      &
                                  cstf(i,2,2)*depsyy(i) +                      &
                                  cstf(i,2,3)*depszz(i) +                      &
                                  cstf(i,2,4)*depsxy(i) +                      &
                                  cstf(i,2,5)*depsyz(i) +                      &
                                  cstf(i,2,6)*depszx(i)
          signzz(i) = sigozz(i) + cstf(i,3,1)*depsxx(i) +                      & 
                                  cstf(i,3,2)*depsyy(i) +                      & 
                                  cstf(i,3,3)*depszz(i) +                      &
                                  cstf(i,3,4)*depsxy(i) +                      &
                                  cstf(i,3,5)*depsyz(i) +                      &
                                  cstf(i,3,6)*depszx(i)
          signxy(i) = sigoxy(i) + cstf(i,4,1)*depsxx(i) +                      &
                                  cstf(i,4,2)*depsyy(i) +                      &
                                  cstf(i,4,3)*depszz(i) +                      &
                                  cstf(i,4,4)*depsxy(i) +                      &
                                  cstf(i,4,5)*depsyz(i) +                      &
                                  cstf(i,4,6)*depszx(i)
          signyz(i) = sigoyz(i) + cstf(i,5,1)*depsxx(i) +                      &
                                  cstf(i,5,2)*depsyy(i) +                      &
                                  cstf(i,5,3)*depszz(i) +                      &
                                  cstf(i,5,4)*depsxy(i) +                      &
                                  cstf(i,5,5)*depsyz(i) +                      &
                                  cstf(i,5,6)*depszx(i)
          signzx(i) = sigozx(i) + cstf(i,6,1)*depsxx(i) +                      &
                                  cstf(i,6,2)*depsyy(i) +                      &
                                  cstf(i,6,3)*depszz(i) +                      &
                                  cstf(i,6,4)*depsxy(i) +                      &
                                  cstf(i,6,5)*depsyz(i) +                      &
                                  cstf(i,6,6)*depszx(i)
        enddo
!
      end subroutine elasticity_anisotropic
      end module elasticity_anisotropic_mod
