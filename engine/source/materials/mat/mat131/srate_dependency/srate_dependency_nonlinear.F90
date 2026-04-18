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
!||    srate_dependency_nonlinear_mod   ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_nonlinear.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress      ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||====================================================================
      module srate_dependency_nonlinear_mod
! \brief Compute nonlinear strain rate dependency for /MAT/LAW131
! \details Compute the strain rate scaling factor using a nonlinear model
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    srate_dependency_nonlinear    ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_nonlinear.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    matparam_def_mod              ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine srate_dependency_nonlinear(                                   &
        matparam ,nel      ,sigy     ,epsd     ,dsigy_dpla,offset   )
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
        type(matparam_struct_),        intent(in)    :: matparam   !< Material parameters data
        integer,                       intent(in)    :: nel        !< Number of elements in the group
        real(kind=WP), dimension(nel), intent(inout) :: sigy       !< Equivalent stress
        real(kind=WP), dimension(nel), intent(in)    :: epsd       !< Strain rate
        real(kind=WP), dimension(nel), intent(inout) :: dsigy_dpla !< Derivative of eq. stress w.r.t. cumulated plastic strain
        integer,                       intent(in)    :: offset     !< Offset in the material parameters array for strain rate dependency parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        real(kind=WP) :: cs,dps
        real(kind=WP), dimension(nel) :: ratefac
!===============================================================================
!
        !=======================================================================
        !< - Non-linear strain rate dependency
        !=======================================================================
        !< Recover strain rate dependency parameters
        cs  = matparam%uparam(offset + 1) !< Non-linear strain rate dependency exponent
        dps = matparam%uparam(offset + 2) !< Reference strain rate
        !< Compute strain rate dependency factor
        ratefac(1:nel) = (one + (epsd(1:nel)/dps))**cs
        !< Apply strain rate dependency to sigy and dsigy_dpla
        sigy(1:nel) = sigy(1:nel)*ratefac(1:nel)
        dsigy_dpla(1:nel) = dsigy_dpla(1:nel)*ratefac(1:nel)
!
      end subroutine srate_dependency_nonlinear
      end module srate_dependency_nonlinear_mod
