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
!||    hm_read_srate_dependency_mod   ../starter/source/materials/mat/mat131/srate_dependency/hm_read_srate_dependency.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic         ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||====================================================================
      module hm_read_srate_dependency_mod
        implicit none
! \brief Read strain rate dependency input data for /MAT/LAW131
! \details Read and dispatch the strain rate dependency model input data
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    hm_read_srate_dependency                     ../starter/source/materials/mat/mat131/srate_dependency/hm_read_srate_dependency.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic                       ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||--- calls      -----------------------------------------------------
!||    hm_read_srate_dependency_cowpersymonds       ../starter/source/materials/mat/mat131/srate_dependency/hm_read_srate_dependency_cowpersymonds.F90
!||    hm_read_srate_dependency_johnsoncook         ../starter/source/materials/mat/mat131/srate_dependency/hm_read_srate_dependency_johnsoncook.F90
!||    hm_read_srate_dependency_nonlinear           ../starter/source/materials/mat/mat131/srate_dependency/hm_read_srate_dependency_nonlinear.F90
!||    hm_read_srate_dependency_tabulated           ../starter/source/materials/mat/mat131/srate_dependency/hm_read_srate_dependency_tabulated.F90
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod                           ../starter/share/modules1/hm_option_read_mod.F
!||    hm_read_srate_dependency_cowpersymonds_mod   ../starter/source/materials/mat/mat131/srate_dependency/hm_read_srate_dependency_cowpersymonds.F90
!||    hm_read_srate_dependency_johnsoncook_mod     ../starter/source/materials/mat/mat131/srate_dependency/hm_read_srate_dependency_johnsoncook.F90
!||    hm_read_srate_dependency_nonlinear_mod       ../starter/source/materials/mat/mat131/srate_dependency/hm_read_srate_dependency_nonlinear.F90
!||    hm_read_srate_dependency_tabulated_mod       ../starter/source/materials/mat/mat131/srate_dependency/hm_read_srate_dependency_tabulated.F90
!||    submodel_mod                                 ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_srate_dependency(                                   &
          ikey     ,type  ,iratedep ,nupar_ratedep,upar_ratedep,               &
          is_available,unitab,lsubmodel,iout     ,is_encrypted ,               &
          vpflag   ,israte ,parmat  ,ntab_srate  ,itab_srate   ,               &
          x2vect   ,x3vect ,x4vect  ,fscale      ,nvartmp      )   
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
          use hm_read_srate_dependency_johnsoncook_mod
          use hm_read_srate_dependency_cowpersymonds_mod
          use hm_read_srate_dependency_tabulated_mod
          use hm_read_srate_dependency_nonlinear_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          character(len=20),       intent(in)    :: type                  !< Keyword type
          integer,                 intent(inout) :: iratedep              !< Strain rate dependency type
          integer,                 intent(inout) :: nupar_ratedep         !< Number of rate dependency parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_ratedep      !< Strain rate dependency parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(inout) :: vpflag                !< Viscoplastic flag
          integer,                 intent(inout) :: israte                !< Strain rate filtering flag
          real(kind=WP),           intent(inout) :: parmat(100)           !< Material parameter global table 1
          integer,                 intent(inout) :: ntab_srate            !< Number of tabulated strain rate dependency functions/tables
          integer,       dimension(100), intent(inout) :: itab_srate      !< Identifiers of tabulated strain rate dependency functions/tables
          real(kind=WP), dimension(100), intent(inout) :: x2vect          !< x2 scale factor for tabulated strain rate dependency
          real(kind=WP), dimension(100), intent(inout) :: x3vect          !< x3 scale factor for tabulated strain rate dependency
          real(kind=WP), dimension(100), intent(inout) :: x4vect          !< x4 scale factor for tabulated strain rate dependency
          real(kind=WP), dimension(100), intent(inout) :: fscale          !< y  scale factor for tabulated strain rate dependency
          integer,                 intent(inout) :: nvartmp               !< number of variables used in tabulated strain rate dependency
!===============================================================================
!    
          !=====================================================================
          !< Johnson-Cook strain rate dependency parameters
          !=====================================================================
          if (type(1:11) == 'JOHNSONCOOK') then
            call hm_read_srate_dependency_johnsoncook(                         &
              ikey     ,iratedep ,nupar_ratedep,upar_ratedep,is_available,     &
              unitab   ,lsubmodel,iout         ,is_encrypted,vpflag      ,     &
              israte   ,parmat   )
          !=====================================================================
          !< Cowper-Symonds strain rate dependency parameters
          !=====================================================================
          elseif (type(1:6) == 'COWPER') then
            call hm_read_srate_dependency_cowpersymonds(                       &
              ikey     ,iratedep ,nupar_ratedep,upar_ratedep,is_available,     &
              unitab   ,lsubmodel,iout         ,is_encrypted,vpflag      ,     &
              israte   ,parmat   )
          !=====================================================================
          !< Tabulated strain rate dependency parameters
          !=====================================================================
          elseif (type(1:3) == 'TAB') then
            call hm_read_srate_dependency_tabulated(                           &
              ikey     ,iratedep ,ntab_srate,itab_srate  ,x2vect ,x3vect   ,   &
              x4vect   ,fscale   ,nvartmp   ,is_available,unitab ,lsubmodel,   &
              iout     ,is_encrypted,vpflag ,israte      ,parmat )
          !=====================================================================
          !< Non-linear strain rate dependency parameters
          !=====================================================================
          elseif (type(1:7) == 'NLINEAR') then
            call hm_read_srate_dependency_nonlinear(                           &
              ikey     ,iratedep ,nupar_ratedep,upar_ratedep,is_available,     &
              unitab   ,lsubmodel,iout         ,is_encrypted,vpflag      ,     &
              israte   ,parmat   )
          endif
! -------------------------------------------------------------------------------
        end subroutine hm_read_srate_dependency
      end module hm_read_srate_dependency_mod