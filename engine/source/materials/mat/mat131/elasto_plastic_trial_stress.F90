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
      contains
!||====================================================================
!||    elasto_plastic_trial_stress   ../engine/source/materials/mat/mat131/elasto_plastic_trial_stress.F90
!||--- called by ------------------------------------------------------
!||    cppm_shells                   ../engine/source/materials/mat/mat131/return_mapping/cppm_shells.F90
!||    cppm_solids                   ../engine/source/materials/mat/mat131/return_mapping/cppm_solids.F90
!||    cutting_plane_shells          ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_shells.F90
!||    cutting_plane_solids          ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||    nice_shells                   ../engine/source/materials/mat/mat131/return_mapping/nice_shells.F90
!||    nice_solids                   ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    matparam_def_mod              ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine elasto_plastic_trial_stress(                                  &
        matparam ,nel      ,soundsp  ,cstf     ,young    ,rho      ,           &
        depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,           &
        sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,           &
        signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,           &
        eltype   ,shf      ,s13      ,s23      )
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
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: ielas,i
!===============================================================================
!
        !< Initialize elastic stiffness matrix 
        cstf(1:nel,1:6,1:6) = zero
        !< Young modulus
        young(1:nel) = matparam%young
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
            !< Solids
            if (eltype == 1) then     
              !< Elastic stiffness matrix
              cstf(1:nel,1,1) = matparam%uparam(1)
              cstf(1:nel,2,2) = matparam%uparam(1)
              cstf(1:nel,3,3) = matparam%uparam(1)
              cstf(1:nel,1,2) = matparam%uparam(2)
              cstf(1:nel,1,3) = matparam%uparam(2)
              cstf(1:nel,2,1) = matparam%uparam(2)
              cstf(1:nel,2,3) = matparam%uparam(2)
              cstf(1:nel,3,1) = matparam%uparam(2)
              cstf(1:nel,3,2) = matparam%uparam(2)
              cstf(1:nel,4,4) = matparam%shear
              cstf(1:nel,5,5) = matparam%shear
              cstf(1:nel,6,6) = matparam%shear
              !< Sound speed
              soundsp(1:nel) = sqrt((matparam%bulk+four_over_3*matparam%shear)/&
                                                                    rho(1:nel))
            !< Shells
            elseif (eltype == 2) then
              !< Elastic stiffness matrix
              cstf(1:nel,1,1) = matparam%uparam(3)
              cstf(1:nel,2,2) = matparam%uparam(3)
              cstf(1:nel,1,2) = matparam%uparam(4)
              cstf(1:nel,2,1) = matparam%uparam(4)
              cstf(1:nel,4,4) = matparam%shear
              cstf(1:nel,5,5) = matparam%shear*shf(1:nel)
              cstf(1:nel,6,6) = matparam%shear*shf(1:nel)
              !< Compliance matrix components for thickness update
              s13(1:nel) = - matparam%nu / matparam%young
              s23(1:nel) = - matparam%nu / matparam%young
              !< Sound speed
              soundsp(1:nel) = sqrt(cstf(1:nel,1,1)/rho(1:nel))
            endif 
          !---------------------------------------------------------------------
          !< Orthotropic elastic model
          !---------------------------------------------------------------------
          case(2)
            !< Solids
            if (eltype == 1) then     
              !< Elastic stiffness matrix
              cstf(1:nel,1,1) = matparam%uparam(1)
              cstf(1:nel,2,2) = matparam%uparam(2)
              cstf(1:nel,3,3) = matparam%uparam(3)
              cstf(1:nel,1,2) = matparam%uparam(4)
              cstf(1:nel,1,3) = matparam%uparam(5)
              cstf(1:nel,2,1) = matparam%uparam(4)
              cstf(1:nel,2,3) = matparam%uparam(6)
              cstf(1:nel,3,1) = matparam%uparam(5)
              cstf(1:nel,3,2) = matparam%uparam(6)
              cstf(1:nel,4,4) = matparam%uparam(10)
              cstf(1:nel,5,5) = matparam%uparam(11)
              cstf(1:nel,6,6) = matparam%uparam(12)
              !< Sound speed
              soundsp(1:nel) = sqrt((matparam%bulk+four_over_3*matparam%shear)/&
                                                                rho(1:nel))
            !< Shells
            elseif (eltype == 2) then
              !< Elastic stiffness matrix
              cstf(1:nel,1,1) = matparam%uparam(7)
              cstf(1:nel,2,2) = matparam%uparam(8)
              cstf(1:nel,1,2) = matparam%uparam(9)
              cstf(1:nel,2,1) = matparam%uparam(9)
              cstf(1:nel,4,4) = matparam%uparam(10)
              cstf(1:nel,5,5) = matparam%uparam(11)*shf(1:nel)
              cstf(1:nel,6,6) = matparam%uparam(12)*shf(1:nel)  
              !< Compliance matrix components for thickness update
              s13(1:nel) = - matparam%uparam(15)/ matparam%uparam(13)
              s23(1:nel) = - matparam%uparam(16)/ matparam%uparam(14)
              !< Sound speed
              soundsp(1:nel) = sqrt(max(cstf(1:nel,1,1),cstf(1:nel,2,2))/      &
                                                              rho(1:nel))       
            endif
        end select
!
        !=======================================================================
        !< Elastic trial stress computation
        !=======================================================================
        signxx(1:nel) = sigoxx(1:nel) + cstf(1:nel,1,1)*depsxx(1:nel) +        &
                                        cstf(1:nel,1,2)*depsyy(1:nel) +        &
                                        cstf(1:nel,1,3)*depszz(1:nel)
        signyy(1:nel) = sigoyy(1:nel) + cstf(1:nel,2,1)*depsxx(1:nel) +        &
                                        cstf(1:nel,2,2)*depsyy(1:nel) +        &
                                        cstf(1:nel,2,3)*depszz(1:nel)
        signzz(1:nel) = sigozz(1:nel) + cstf(1:nel,3,1)*depsxx(1:nel) +        & 
                                        cstf(1:nel,3,2)*depsyy(1:nel) +        & 
                                        cstf(1:nel,3,3)*depszz(1:nel)
        signxy(1:nel) = sigoxy(1:nel) + cstf(1:nel,4,4)*depsxy(1:nel)
        signyz(1:nel) = sigoyz(1:nel) + cstf(1:nel,5,5)*depsyz(1:nel)
        signzx(1:nel) = sigozx(1:nel) + cstf(1:nel,6,6)*depszx(1:nel)
!
      end subroutine elasto_plastic_trial_stress
      end module elasto_plastic_trial_stress_mod
