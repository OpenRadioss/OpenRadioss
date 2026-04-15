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
!||    self_heating_tabulated_mod    ../engine/source/materials/mat/mat131/self_heating/self_heating_tabulated.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||====================================================================
      module self_heating_tabulated_mod
! \brief Compute tabulated self-heating for /MAT/LAW131
! \details Compute the temperature rise due to plastic work dissipation
!          using a tabulated Taylor-Quinney coefficient for /MAT/LAW131.
      contains
!||====================================================================
!||    self_heating_tabulated        ../engine/source/materials/mat/mat131/self_heating/self_heating_tabulated.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress   ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp             ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    matparam_def_mod              ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod         ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      subroutine self_heating_tabulated(                                       &
        matparam ,nel      ,sigy     ,dtemp_dpla,epsd     ,nvartmp  ,vartmp   ,&
        temp     ,pla      ,offset   )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use matparam_def_mod
          use constant_mod
          use table_mat_vinterp_mod
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
        integer,                       intent(in)    :: nvartmp    !< Number of temporary variables used in tabulated self heating
        integer,dimension(nel,nvartmp),intent(inout) :: vartmp     !< Temporary variables array
        real(kind=WP), dimension(nel), intent(in)    :: temp       !< Temperature
        real(kind=WP), dimension(nel), intent(in)    :: pla        !< Plastic strain
        integer,                       intent(in)    :: offset     !< Offset in the material parameters array for self heating parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: offset_tab,offset_var,i
        real(kind=WP) :: eta,rhocp
        real(kind=WP), dimension(nel) :: weight,dweight,xvec(nel,3)
!===============================================================================
!
        !=======================================================================
        !< - Tabulated self-heating model
        !=======================================================================
        offset_tab = matparam%iparam(21)
        offset_var = matparam%iparam(23)
        !< Recover self heating parameters
        eta   = matparam%uparam(offset + 1) !< Taylor-Quinney coefficient
        rhocp = matparam%therm%rhocp        !< Material thermal inertia
        !< Prepare input vectors for interpolation
        xvec(1:nel,1) = epsd(1:nel)
        xvec(1:nel,2) = temp(1:nel)
        xvec(1:nel,3) = pla(1:nel)
        !< Strain rate weight factor interpolation
        call table_mat_vinterp(matparam%table(offset_tab+1),nel,nel,           &
          vartmp(1:nel,offset_var+1),xvec,weight,dweight)
        !< Update derivative of temperature w.r.t. cumulated plastic strain
        dtemp_dpla(1:nel) = (eta/rhocp)*sigy(1:nel)*weight(1:nel)
!
        end subroutine self_heating_tabulated
      end module self_heating_tabulated_mod
