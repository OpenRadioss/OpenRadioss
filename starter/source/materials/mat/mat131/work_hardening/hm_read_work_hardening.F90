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
!||    hm_read_work_hardening_mod   ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic       ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||====================================================================
      module hm_read_work_hardening_mod
        implicit none
! \brief Read work hardening input data for /MAT/LAW131
! \details Read and dispatch the work hardening model input data
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    hm_read_work_hardening                  ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic                  ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||--- calls      -----------------------------------------------------
!||    hm_read_work_hardening_linearvoce       ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_linearvoce.F90
!||    hm_read_work_hardening_powerlaw         ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_powerlaw.F90
!||    hm_read_work_hardening_tabulated        ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_tabulated.F90
!||    hm_read_work_hardening_voce             ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_voce.F90
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod                      ../starter/share/modules1/hm_option_read_mod.F
!||    hm_read_work_hardening_linearvoce_mod   ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_linearvoce.F90
!||    hm_read_work_hardening_powerlaw_mod     ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_powerlaw.F90
!||    hm_read_work_hardening_tabulated_mod    ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_tabulated.F90
!||    hm_read_work_hardening_voce_mod         ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_voce.F90
!||    submodel_mod                            ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_work_hardening(                                     &
          ikey     ,type     ,ihard    ,nupar_hard,upar_hard,ntab_hard,        &
          itab_hard,x2vect   ,x3vect   ,x4vect    ,fscale   ,nvartmp  ,        &
          is_available,unitab,lsubmodel,iout      ,is_encrypted,vpflag,        &
          israte   ,parmat   ,titr     ,mat_id    ,matparam    )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
          use hm_read_work_hardening_powerlaw_mod
          use hm_read_work_hardening_voce_mod
          use hm_read_work_hardening_tabulated_mod
          use hm_read_work_hardening_linearvoce_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          character(len=20),       intent(in)    :: type                  !< Keyword type
          integer,                 intent(inout) :: ihard                 !< Work hardening type
          integer,                 intent(inout) :: nupar_hard            !< Number of work hardening parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_hard         !< Work hardening parameters
          integer,                 intent(inout) :: ntab_hard             !< Number of tabulated hardening functions/tables
          integer, dimension(100), intent(inout) :: itab_hard             !< Identifiers of tabulated hardening functions/tables
          real(kind=WP),dimension(100),intent(inout) :: x2vect            !< X2 scale factor for tabulated hardening
          real(kind=WP),dimension(100),intent(inout) :: x3vect            !< X3 scale factor for tabulated hardening
          real(kind=WP),dimension(100),intent(inout) :: x4vect            !< X4 scale factor for tabulated hardening
          real(kind=WP),dimension(100),intent(inout) :: fscale            !< Y  scale factor for tabulated hardening
          integer,                 intent(inout) :: nvartmp               !< Number of variables used in tabulated hardening
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(inout) :: vpflag                !< Strain rate flag
          integer,                 intent(inout) :: israte                !< Strain rate filtering flag
          real(kind=WP),dimension(100),intent(inout) :: parmat            !< Material parameters
          character(len=nchartitle),intent(in)   :: titr                  !< Material law user title
          integer,                 intent(in)    :: mat_id                !< Material law user ID
          type(matparam_struct_),  intent(inout) :: matparam              !< Matparam data structure
!===============================================================================
!     
          !=====================================================================
          !< Power law hardening parameters
          !=====================================================================
          if (type(1:12) == 'POWERLAW_ENG') then
            call hm_read_work_hardening_powerlaw(                              &
              ikey     ,ihard    ,nupar_hard,upar_hard,is_available,           &
              unitab   ,lsubmodel,iout     ,is_encrypted,         1,           &
              titr     ,mat_id   ,matparam )          
          elseif (type(1:8) == 'POWERLAW') then
            call hm_read_work_hardening_powerlaw(                              &
              ikey     ,ihard    ,nupar_hard,upar_hard,is_available,           &
              unitab   ,lsubmodel,iout     ,is_encrypted,         2,           &
              titr     ,mat_id   ,matparam )
          !=====================================================================
          !< Voce hardening parameters
          !=====================================================================
          elseif (type(1:4) == 'VOCE') then
            call hm_read_work_hardening_voce(                                  &
              ikey     ,ihard    ,nupar_hard,upar_hard,is_available,           &
              unitab   ,lsubmodel,iout     ,is_encrypted)
          !=====================================================================
          !< Tabulated hardening parameters
          !=====================================================================
          elseif (type(1:3) == 'TAB') then
            call hm_read_work_hardening_tabulated(                             &
              ikey     ,ihard    ,ntab_hard ,itab_hard   ,x2vect ,x3vect   ,   &
              x4vect   ,fscale   ,nvartmp   ,is_available,unitab ,lsubmodel,   &
              iout     ,is_encrypted,vpflag ,israte      ,parmat ,nupar_hard,  &
              upar_hard)
          !=====================================================================
          !< Linear-Voce hardening parameters
          !=====================================================================
          elseif (type(1:7) == 'LINVOCE') then
            call hm_read_work_hardening_linearvoce(                            &
              ikey     ,ihard    ,nupar_hard,upar_hard,is_available,           &
              unitab   ,lsubmodel,iout     ,is_encrypted)
          endif
          !=====================================================================
!-------------------------------------------------------------------------------
        end subroutine hm_read_work_hardening
      end module hm_read_work_hardening_mod