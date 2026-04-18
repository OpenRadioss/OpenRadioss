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
!||    self_heating_taylor_mod       ../engine/source/materials/mat/mat131/self_heating/self_heating_taylor.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||====================================================================
      module self_heating_taylor_mod
! \brief Compute Taylor-Quinney self-heating for /MAT/LAW131
! \details Compute the temperature rise due to plastic work dissipation
!          using a constant Taylor-Quinney coefficient for /MAT/LAW131.
      contains
!||====================================================================
!||    self_heating_taylor           ../engine/source/materials/mat/mat131/self_heating/self_heating_taylor.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    matparam_def_mod              ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine self_heating_taylor(                                          &
        matparam ,nel      ,sigy     ,dtemp_dpla,epsd     ,offset   )
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
        real(kind=WP), dimension(nel), intent(inout) :: dtemp_dpla !< Derivative of temperature w.r.t. cumulated plastic strain
        real(kind=WP), dimension(nel), intent(in)    :: epsd       !< Equivalent strain rate
        integer,                       intent(in)    :: offset     !< Offset in the material parameters array for self heating parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i
        real(kind=WP) :: eta,deis,dead,rhocp
        real(kind=WP), dimension(nel) :: weight
!===============================================================================
!
        !=======================================================================
        !< - Taylor-Quinney (extended) self-heating model
        !=======================================================================
        !< Recover self heating parameters
        eta   = matparam%uparam(offset + 1) !< Taylor-Quinney coefficient
        deis  = matparam%uparam(offset + 2) !< Strain rates for the beginning of adiabatic transition
        dead  = matparam%uparam(offset + 3) !< Strain rates for the end of adiabatic transition
        rhocp = matparam%therm%rhocp        !< Material thermal inertia
        !< Strain rate weight factor computation
        weight(1:nel) = ((epsd(1:nel)-deis)**2) *                               &
             (three*dead - two*epsd(1:nel) - deis)/((dead-deis)**3)
        weight(1:nel) = max(zero, min(one, weight(1:nel)))
        where (epsd(1:nel) < deis) weight(1:nel) = zero
        where (epsd(1:nel) > dead) weight(1:nel) = one
        !< Update derivative of temperature w.r.t. cumulated plastic strain
        dtemp_dpla(1:nel) = (eta/rhocp)*sigy(1:nel)*weight(1:nel)
!
      end subroutine self_heating_taylor
      end module self_heating_taylor_mod
