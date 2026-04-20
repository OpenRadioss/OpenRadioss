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
!||    hm_read_therm_softening_mod   ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic        ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||====================================================================
      module hm_read_therm_softening_mod
        implicit none
! \brief Read thermal softening input data for /MAT/LAW131
! \details Read and dispatch the thermal softening model input data
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    hm_read_therm_softening                   ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic                    ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||--- calls      -----------------------------------------------------
!||    hm_read_therm_softening_johnsoncook       ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening_johnsoncook.F90
!||    hm_read_therm_softening_tabulated         ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening_tabulated.F90
!||    hm_read_therm_softening_zhao              ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening_zhao.F90
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                              ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod                        ../starter/share/modules1/hm_option_read_mod.F
!||    hm_read_therm_softening_johnsoncook_mod   ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening_johnsoncook.F90
!||    hm_read_therm_softening_tabulated_mod     ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening_tabulated.F90
!||    hm_read_therm_softening_zhao_mod          ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening_zhao.F90
!||    submodel_mod                              ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_therm_softening(                                    &
          ikey     ,type  ,itherm   ,nupar_therm ,upar_therm   ,               &
          is_available,unitab,lsubmodel,iout     ,is_encrypted ,               &
          ntab_therm  ,itab_therm   ,x2vect      ,x3vect       ,               &
          x4vect   ,fscale          ,nvartmp     ,mtag         ,               &
          matparam    )   
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use elbuftag_mod
          use precision_mod, only : WP
          use hm_read_therm_softening_johnsoncook_mod
          use hm_read_therm_softening_zhao_mod
          use hm_read_therm_softening_tabulated_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          character(len=20),       intent(in)    :: type                  !< Keyword type
          integer,                 intent(inout) :: itherm                !< Thermal softening type
          integer,                 intent(inout) :: nupar_therm           !< Number of thermal softening parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_therm        !< Thermal softening parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(inout) :: ntab_therm            !< Number of tabulated thermal softening functions/tables
          integer,       dimension(100), intent(inout) :: itab_therm      !< Identifiers of tabulated thermal softening functions/tables
          real(kind=WP), dimension(100), intent(inout) :: x2vect          !< x2 scale factor for tabulated thermal softening
          real(kind=WP), dimension(100), intent(inout) :: x3vect          !< x3 scale factor for tabulated thermal softening
          real(kind=WP), dimension(100), intent(inout) :: x4vect          !< x4 scale factor for tabulated thermal softening
          real(kind=WP), dimension(100), intent(inout) :: fscale          !< y  scale factor for tabulated thermal softening
          integer,                 intent(inout) :: nvartmp               !< Number of variables used in tabulated thermal softening
          type(mlaw_tag_),         intent(inout) :: mtag                  !< Material tag for internal variables in element buffer
          type(matparam_struct_),  intent(inout) :: matparam              !< Material parameter data structure
!===============================================================================
!    
          !=====================================================================
          !< Johnson-Cook thermal softening parameters
          !=====================================================================
          if (type(1:11) == 'JOHNSONCOOK') then
            call hm_read_therm_softening_johnsoncook(                          &
              ikey     ,itherm   ,nupar_therm  ,upar_therm  ,is_available,     &
              unitab   ,lsubmodel,iout         ,is_encrypted,matparam    )
          !=====================================================================
          !< Zhao thermal softening parameters
          !=====================================================================
          elseif (type(1:4) == 'ZHAO') then
            call hm_read_therm_softening_zhao(                                 &
              ikey     ,itherm   ,nupar_therm  ,upar_therm  ,is_available,     &
              unitab   ,lsubmodel,iout         ,is_encrypted,matparam    )
          !=====================================================================
          !< Tabulated thermal softening parameters
          !=====================================================================
          elseif (type(1:3) == 'TAB') then
            call hm_read_therm_softening_tabulated(                            &
              ikey     ,itherm   ,ntab_therm,itab_therm  ,x2vect ,x3vect   ,   &
              x4vect   ,fscale   ,nvartmp   ,is_available,unitab ,lsubmodel,   &
              iout     ,is_encrypted,matparam)
          endif
!
          !< Set temperature variable tag
          if (mtag%g_temp == 0) mtag%g_temp = 1
          if (mtag%l_temp == 0) mtag%l_temp = 1
!
          !< Activate heat source calculation in material
          if (matparam%heat_flag == 0) matparam%heat_flag = 1
!
! -------------------------------------------------------------------------------
        end subroutine hm_read_therm_softening
      end module hm_read_therm_softening_mod