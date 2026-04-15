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
!||    elasto_plastic_eq_stress_mod   ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||--- called by ------------------------------------------------------
!||    cppm_shells                    ../engine/source/materials/mat/mat131/return_mapping/cppm_shells.F90
!||    cppm_solids                    ../engine/source/materials/mat/mat131/return_mapping/cppm_solids.F90
!||    cutting_plane_shells           ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_shells.F90
!||    cutting_plane_solids           ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||    nice_shells                    ../engine/source/materials/mat/mat131/return_mapping/nice_shells.F90
!||    nice_solids                    ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||====================================================================
      module elasto_plastic_eq_stress_mod
! \brief Compute elasto-plastic equivalent stress for /MAT/LAW131
! \details Compute the equivalent stress from the deviatoric stress tensor
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    elasto_plastic_eq_stress                    ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||--- called by ------------------------------------------------------
!||    cppm_shells                                 ../engine/source/materials/mat/mat131/return_mapping/cppm_shells.F90
!||    cppm_solids                                 ../engine/source/materials/mat/mat131/return_mapping/cppm_solids.F90
!||    cutting_plane_shells                        ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_shells.F90
!||    cutting_plane_solids                        ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||    nice_shells                                 ../engine/source/materials/mat/mat131/return_mapping/nice_shells.F90
!||    nice_solids                                 ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||--- calls      -----------------------------------------------------
!||    elasto_plastic_second_order_numerical       ../engine/source/materials/mat/mat131/elasto_plastic_second_order_numerical.F90
!||    yield_criterion_barlat1989                  ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat1989.F90
!||    yield_criterion_barlat2000                  ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat2000.F90
!||    yield_criterion_hershey                     ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_hershey.F90
!||    yield_criterion_hill                        ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_hill.F90
!||    yield_criterion_vonmises                    ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_vonmises.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                                ../common_source/modules/constant_mod.F
!||    elasto_plastic_second_order_numerical_mod   ../engine/source/materials/mat/mat131/elasto_plastic_second_order_numerical.F90
!||    matparam_def_mod                            ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                               ../common_source/modules/precision_mod.F90
!||    yield_criterion_barlat1989_mod              ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat1989.F90
!||    yield_criterion_barlat2000_mod              ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat2000.F90
!||    yield_criterion_hershey_mod                 ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_hershey.F90
!||    yield_criterion_hill_mod                    ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_hill.F90
!||    yield_criterion_vonmises_mod                ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_vonmises.F90
!||====================================================================
      subroutine elasto_plastic_eq_stress(                                     &
          matparam ,nel      ,seq      ,iresp    ,eltype   ,                   &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,         &
          normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,         &
          N        ,second_order)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use precision_mod, only : WP
        use yield_criterion_vonmises_mod
        use yield_criterion_hershey_mod
        use yield_criterion_hill_mod
        use yield_criterion_barlat1989_mod
        use yield_criterion_barlat2000_mod
        use elasto_plastic_second_order_numerical_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        type(matparam_struct_),        intent(in)    :: matparam !< Material parameters data
        integer,                       intent(in)    :: nel      !< Number of elements in the group
        real(kind=WP), dimension(nel), intent(inout) :: seq      !< Equivalent stress
        integer,                       intent(in)    :: iresp    !< Precision flag
        real(kind=WP), dimension(nel), intent(in)    :: signxx   !< Current stress xx
        real(kind=WP), dimension(nel), intent(in)    :: signyy   !< Current stress yy
        real(kind=WP), dimension(nel), intent(in)    :: signzz   !< Current stress zz
        real(kind=WP), dimension(nel), intent(in)    :: signxy   !< Current stress xy
        real(kind=WP), dimension(nel), intent(in)    :: signyz   !< Current stress yz
        real(kind=WP), dimension(nel), intent(in)    :: signzx   !< Current stress zx
        real(kind=WP), dimension(nel), intent(inout) :: normxx   !< 1st derivative of equivalent stress wrt stress xx
        real(kind=WP), dimension(nel), intent(inout) :: normyy   !< 1st derivative of equivalent stress wrt stress yy
        real(kind=WP), dimension(nel), intent(inout) :: normzz   !< 1st derivative of equivalent stress wrt stress zz
        real(kind=WP), dimension(nel), intent(inout) :: normxy   !< 1st derivative of equivalent stress wrt stress xy
        real(kind=WP), dimension(nel), intent(inout) :: normyz   !< 1st derivative of equivalent stress wrt stress yz
        real(kind=WP), dimension(nel), intent(inout) :: normzx   !< 1st derivative of equivalent stress wrt stress zx
        integer,                       intent(in)    :: eltype   !< Element type
        real(kind=WP), dimension(nel,6,6), intent(inout) :: N    !< 2nd derivative of equivalent stress
        logical,                       intent(in)    :: second_order !< Flag for computing second order derivatives
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: icrit,offset
!===============================================================================
!
        !=======================================================================
        !< - Select yield criterion model
        !=======================================================================
        icrit  = matparam%iparam(6)
        offset = matparam%iparam(3)
        select case(icrit)
          !---------------------------------------------------------------------
          !< Von Mises yield criterion
          !---------------------------------------------------------------------
          case(1)
            call yield_criterion_vonmises(                                     &
              nel      ,seq      ,eltype   ,                                   &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,     &
              N        ,second_order)
          !---------------------------------------------------------------------
          !< Hershey yield criterion
          !---------------------------------------------------------------------
          case(2)
            call yield_criterion_hershey(                                      &          
              matparam ,nel      ,seq      ,iresp    ,eltype   ,               &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,     &
              N        ,second_order,offset)  
          !---------------------------------------------------------------------
          !< Hill yield criterion
          !---------------------------------------------------------------------
          case(3)
            call yield_criterion_hill(                                         &          
              matparam ,nel      ,seq      ,eltype   ,                         &
              signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,     &
              normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,     &
              N        ,second_order,offset)
          !---------------------------------------------------------------------
          !< Barlat 89 yield criterion
          !---------------------------------------------------------------------
          case(4)
            call yield_criterion_barlat1989(                                   &          
              matparam ,nel      ,seq      ,signxx   ,signyy   ,signxy   ,     &
              normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,     &
              offset   )
            if (second_order) then
              call elasto_plastic_second_order_numerical(                      &
                matparam ,nel      ,eltype   ,icrit    ,                       &
                signxx   , signyy  ,signzz   ,signxy   ,signyz   ,signzx   ,   &
                normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,   &
                N        ,offset   )
            else
              N(1:nel,1:6,1:6) = zero
            endif
          !---------------------------------------------------------------------
          !< Barlat 2000 yield criterion
          !---------------------------------------------------------------------
          case(5)
            call yield_criterion_barlat2000(                                   &          
              matparam ,nel      ,seq      ,signxx   ,signyy   ,signxy   ,     &
              normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,     &
              offset   )
            if (second_order) then
              call elasto_plastic_second_order_numerical(                      &
                matparam ,nel      ,eltype   ,icrit    ,                       &
                signxx   , signyy  ,signzz   ,signxy   ,signyz   ,signzx   ,   &
                normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,   &
                N        ,offset   )
            else
              N(1:nel,1:6,1:6) = zero
            endif
        end select
!
      end subroutine elasto_plastic_eq_stress
      end module elasto_plastic_eq_stress_mod
