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
!||    srate_dependency_tabulated_mod   ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_tabulated.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_yield_stress      ../engine/source/materials/mat/mat131/elasto_plastic_yield_stress.F90
!||====================================================================
      module srate_dependency_tabulated_mod
! \brief Compute tabulated strain rate dependency for /MAT/LAW131
! \details Compute the strain rate scaling factor using tabulated data
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    srate_dependency_tabulated    ../engine/source/materials/mat/mat131/srate_dependency/srate_dependency_tabulated.F90
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
      subroutine srate_dependency_tabulated(                                   &
        matparam ,nel      ,sigy     ,epsd     ,dsigy_dpla,nvartmp  ,vartmp   )
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
        type(matparam_struct_),          intent(in)    :: matparam   !< Material parameters data
        integer,                         intent(in)    :: nel        !< Number of elements in the group
        real(kind=WP),   dimension(nel), intent(inout) :: sigy       !< Equivalent stress
        real(kind=WP),   dimension(nel), intent(in)    :: epsd       !< Strain rate
        real(kind=WP),   dimension(nel), intent(inout) :: dsigy_dpla !< Derivative of eq. stress w.r.t. cumulated plastic strain
        integer,                         intent(in)    :: nvartmp    !< Number of variables used in tabulated strain rate dependency
        integer, dimension(nel,nvartmp), intent(inout) :: vartmp     !< Temporary variables for tabulated strain rate dependency
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i,offset_tab,offset_var
        real(kind=WP) :: xvec(nel,1),dfact_depsd(nel),srate_fac(nel)
!===============================================================================
!
        !=======================================================================
        !< - Tabulated strain rate dependency model
        !=======================================================================
        !< Table offset
        offset_tab = matparam%iparam(10)
        offset_var = matparam%iparam(12)
        !< Prepare input vectors for interpolation
        xvec(1:nel,1) = epsd(1:nel)
        !< Interpolate to get srate_fac and dfact_depsd
        call table_mat_vinterp(matparam%table(offset_tab+1),nel,nel,           &
          vartmp(1:nel,offset_var+1),xvec,srate_fac,dfact_depsd)
        !< Update temporary variables
        sigy(1:nel) = sigy(1:nel)*srate_fac(1:nel)
        dsigy_dpla(1:nel) = dsigy_dpla(1:nel)*srate_fac(1:nel)
!
      end subroutine srate_dependency_tabulated
      end module srate_dependency_tabulated_mod
