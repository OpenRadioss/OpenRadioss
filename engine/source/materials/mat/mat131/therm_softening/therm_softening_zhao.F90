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
!||    therm_softening_zhao_mod      ../engine/source/materials/mat/mat131/therm_softening/therm_softening_zhao.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||====================================================================
      module therm_softening_zhao_mod
! \brief Compute Zhao thermal softening for /MAT/LAW131
! \details Compute the thermal softening factor using the Zhao model
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    therm_softening_zhao          ../engine/source/materials/mat/mat131/therm_softening/therm_softening_zhao.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    matparam_def_mod              ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine therm_softening_zhao(                                         &
        matparam ,nel      ,sigy     ,temp     ,dsigy_dpla,dtemp_dpla,offset   )
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
        real(kind=WP), dimension(nel), intent(inout) :: temp       !< Temperature
        real(kind=WP), dimension(nel), intent(inout) :: dsigy_dpla !< Derivative of yield stress w.r.t. cumulated plastic strain
        real(kind=WP), dimension(nel), intent(inout) :: dtemp_dpla !< Derivative of temperature w.r.t. cumulated plastic strain
        integer,                       intent(in)    :: offset     !< Offset in the material parameters array for thermal softening parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i
        real(kind=WP) :: tref,mu
        real(kind=WP), dimension(nel) :: thermfac
!===============================================================================
!
        !=======================================================================
        !< - Zhao thermal softening model
        !=======================================================================
        !< Recover thermal softening parameters
        mu    = matparam%uparam(offset + 1) !< Thermal softening slope
        tref  = matparam%therm%tref         !< Reference temperature
        !< Limit temperature to reference temperature + 1/mu to avoid negative yield stress
        temp(1:nel) = min(temp(1:nel), tref + one/mu)
        thermfac(1:nel) = one - mu*(temp(1:nel) - tref)
        !< Apply thermal softening to sigy and dsigy_dpla using chain rule
        dsigy_dpla(1:nel) = dsigy_dpla(1:nel)*thermfac(1:nel) - sigy(1:nel)*mu*dtemp_dpla(1:nel)
        sigy(1:nel) = sigy(1:nel)*thermfac(1:nel)
!
        end subroutine therm_softening_zhao
      end module therm_softening_zhao_mod
