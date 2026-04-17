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
!||    elasto_plastic_trial_stress_mod   ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||--- called by ------------------------------------------------------
!||    cppm_shells                       ../engine/source/materials/mat/mat131/return_mapping/cppm_shells.F90
!||    cppm_solids                       ../engine/source/materials/mat/mat131/return_mapping/cppm_solids.F90
!||    cutting_plane_shells              ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_shells.F90
!||    cutting_plane_solids              ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||    nice_shells                       ../engine/source/materials/mat/mat131/return_mapping/nice_shells.F90
!||    nice_solids                       ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||====================================================================
      module elasto_plastic_trial_stress_mod
! \brief Compute elasto-plastic trial stress for /MAT/LAW131
! \details Compute the elastic trial stress tensor (predictor step)
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    elasto_plastic_trial_stress        ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||--- called by ------------------------------------------------------
!||    cppm_shells                        ../engine/source/materials/mat/mat131/return_mapping/cppm_shells.F90
!||    cppm_solids                        ../engine/source/materials/mat/mat131/return_mapping/cppm_solids.F90
!||    cutting_plane_shells               ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_shells.F90
!||    cutting_plane_solids               ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||    nice_shells                        ../engine/source/materials/mat/mat131/return_mapping/nice_shells.F90
!||    nice_solids                        ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||--- calls      -----------------------------------------------------
!||    elasticity_anisotropic             ../engine/source/materials/mat/mat131/elasticity/elasticity_anisotropic.F90
!||    elasticity_bimod_isotropic         ../engine/source/materials/mat/mat131/elasticity/elasticity_bimod_isotropic.F90
!||    elasticity_isotropic               ../engine/source/materials/mat/mat131/elasticity/elasticity_isotropic.F90
!||    elasticity_orthotropic             ../engine/source/materials/mat/mat131/elasticity/elasticity_orthotropic.F90
!||    elasticity_temp_isotropic          ../engine/source/materials/mat/mat131/elasticity/elasticity_temp_isotropic.F90
!||    elasticity_viscous_isotropic       ../engine/source/materials/mat/mat131/elasticity/elasticity_viscous_isotropic.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                       ../common_source/modules/constant_mod.F
!||    elasticity_anisotropic_mod         ../engine/source/materials/mat/mat131/elasticity/elasticity_anisotropic.F90
!||    elasticity_bimod_isotropic_mod     ../engine/source/materials/mat/mat131/elasticity/elasticity_bimod_isotropic.F90
!||    elasticity_isotropic_mod           ../engine/source/materials/mat/mat131/elasticity/elasticity_isotropic.F90
!||    elasticity_orthotropic_mod         ../engine/source/materials/mat/mat131/elasticity/elasticity_orthotropic.F90
!||    elasticity_temp_isotropic_mod      ../engine/source/materials/mat/mat131/elasticity/elasticity_temp_isotropic.F90
!||    elasticity_viscous_isotropic_mod   ../engine/source/materials/mat/mat131/elasticity/elasticity_viscous_isotropic.F90
!||    matparam_def_mod                   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                      ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine elasto_plastic_trial_stress(                                  &
        matparam ,nel      ,soundsp  ,cstf     ,young    ,rho      ,           &
        depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,           &
        sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,           &
        signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,           &
        eltype   ,shf      ,s13      ,s23      ,s43      ,ieos     ,           &
        dpdm     ,nvartmp  ,vartmp   ,epsd     ,nuvar    ,uvar     ,           &
        temp     )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use precision_mod, only : WP
        use elasticity_isotropic_mod
        use elasticity_orthotropic_mod
        use elasticity_anisotropic_mod
        use elasticity_viscous_isotropic_mod
        use elasticity_temp_isotropic_mod
        use elasticity_bimod_isotropic_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        integer,                       intent(in)    :: nel      !< Number of elements in the group
        type(matparam_struct_),        intent(in)    :: matparam !< Material parameters data
        real(kind=WP), dimension(nel), intent(inout) :: soundsp  !< Current sound speed
        real(kind=WP), dimension(nel,6,6),intent(inout) :: cstf  !< Elastic stiffness tensor
        real(kind=WP), dimension(nel), intent(inout) :: young    !< Young modulus
        real(kind=WP), dimension(nel), intent(in)    :: rho      !< Material density
        real(kind=WP), dimension(nel), intent(in)    :: depsxx   !< Strain increment xx
        real(kind=WP), dimension(nel), intent(in)    :: depsyy   !< Strain increment yy
        real(kind=WP), dimension(nel), intent(in)    :: depszz   !< Strain increment zz
        real(kind=WP), dimension(nel), intent(in)    :: depsxy   !< Strain increment xy
        real(kind=WP), dimension(nel), intent(in)    :: depsyz   !< Strain increment yz
        real(kind=WP), dimension(nel), intent(in)    :: depszx   !< Strain increment zx
        real(kind=WP), dimension(nel), intent(in)    :: sigoxx   !< Previous stress xx
        real(kind=WP), dimension(nel), intent(in)    :: sigoyy   !< Previous stress yy
        real(kind=WP), dimension(nel), intent(in)    :: sigozz   !< Previous stress zz
        real(kind=WP), dimension(nel), intent(in)    :: sigoxy   !< Previous stress xy
        real(kind=WP), dimension(nel), intent(in)    :: sigoyz   !< Previous stress yz
        real(kind=WP), dimension(nel), intent(in)    :: sigozx   !< Previous stress zx
        real(kind=WP), dimension(nel), intent(inout) :: signxx   !< Current stress xx
        real(kind=WP), dimension(nel), intent(inout) :: signyy   !< Current stress yy
        real(kind=WP), dimension(nel), intent(inout) :: signzz   !< Current stress zz
        real(kind=WP), dimension(nel), intent(inout) :: signxy   !< Current stress xy
        real(kind=WP), dimension(nel), intent(inout) :: signyz   !< Current stress yz
        real(kind=WP), dimension(nel), intent(inout) :: signzx   !< Current stress zx
        integer,                       intent(in)    :: eltype   !< Element type (1 - Solids, 2 - Shells)
        real(kind=WP), dimension(nel), intent(in)    :: shf      !< Shear factor for shells
        real(kind=WP), dimension(nel), intent(inout) :: s13      !< Compliance matrix component 13
        real(kind=WP), dimension(nel), intent(inout) :: s23      !< Compliance matrix component 23
        real(kind=WP), dimension(nel), intent(inout) :: s43      !< Compliance matrix component 43
        integer,                       intent(in)    :: ieos     !< Equation of state flag
        real(kind=WP), dimension(nel), intent(inout) :: dpdm     !< Pressure derivative of the shear modulus for EOS coupling
        integer,                       intent(in)    :: nvartmp  !< Number of temporary variables for table interpolation
        integer,dimension(nel,nvartmp),intent(inout) :: vartmp   !< Temporary variable array for table interpolation
        real(kind=WP), dimension(nel), intent(in)    :: epsd     !< Equivalent strain rate
        integer,                       intent(in)    :: nuvar    !< Number of internal variables
        real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< Internal variable array
        real(kind=WP), dimension(nel), intent(in)    :: temp     !< Temperature
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: ielas,i
!===============================================================================
!
        !< Initialize elastic stiffness matrix 
        cstf(1:nel,1:6,1:6) = zero
!
        !=======================================================================
        !< - Select elastic model
        !=======================================================================
        ielas = matparam%iparam(1)
        select case(ielas)
          !---------------------------------------------------------------------
          !< Isotropic elastic model
          !---------------------------------------------------------------------
          case(1)
            call elasticity_isotropic(                                         &
              matparam ,nel      ,eltype   ,ieos     ,rho      ,dpdm     ,     &
              depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,     &
              sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,     &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              shf      ,cstf     ,soundsp  ,s13      ,s23      ,s43      ,     &
              young    )
          !---------------------------------------------------------------------
          !< Orthotropic elastic model
          !---------------------------------------------------------------------
          case(2)
            call elasticity_orthotropic(                                       &
              matparam ,nel      ,eltype   ,ieos     ,rho      ,dpdm     ,     &
              depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,     &
              sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,     &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              shf      ,cstf     ,soundsp  ,s13      ,s23      ,s43      ,     &
              young    )
          !---------------------------------------------------------------------
          !< Anisotropic elastic model
          !---------------------------------------------------------------------
          case(3)
            call elasticity_anisotropic(                                       &
              matparam ,nel      ,eltype   ,ieos     ,rho      ,dpdm     ,     &
              depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,     &
              sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,     &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              shf      ,cstf     ,soundsp  ,s13      ,s23      ,s43      ,     &
              young    )      
          !---------------------------------------------------------------------
          !< Viscous isotropic elastic model
          !---------------------------------------------------------------------
          case(4)
            call elasticity_viscous_isotropic(                                 &
              matparam ,nel      ,eltype   ,ieos     ,rho      ,dpdm     ,     &
              depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,     &
              sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,     &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              shf      ,cstf     ,soundsp  ,s13      ,s23      ,s43      ,     &
              epsd     ,nvartmp  ,vartmp   ,young    )
          !---------------------------------------------------------------------
          !< Temperature-dependent isotropic elastic model
          !---------------------------------------------------------------------
          case(5)
            call elasticity_temp_isotropic(                                    &
              matparam ,nel      ,eltype   ,ieos     ,rho      ,dpdm     ,     &
              depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,     &
              sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,     &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              shf      ,cstf     ,soundsp  ,s13      ,s23      ,s43      ,     &
              temp     ,nvartmp  ,vartmp   ,young    ,nuvar    ,uvar     )
          !---------------------------------------------------------------------
          !< Bimodular isotropic elastic model
          !---------------------------------------------------------------------
          case(6)
            call elasticity_bimod_isotropic(                                   &
              matparam ,nel      ,eltype   ,ieos     ,rho      ,dpdm     ,     &
              depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,     &
              sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,     &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              shf      ,cstf     ,soundsp  ,s13      ,s23      ,s43      ,     &
              young    )
        end select
!
      end subroutine elasto_plastic_trial_stress
      end module elasto_plastic_trial_stress_mod
