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
!||    hm_read_self_heating_mod   ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic     ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||====================================================================
      module hm_read_self_heating_mod
        implicit none
! \brief Read self-heating input data for /MAT/LAW131
! \details Read and dispatch the self-heating model input data
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    hm_read_self_heating                 ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic               ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||--- calls      -----------------------------------------------------
!||    hm_read_self_heating_tabulated       ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating_tabulated.F90
!||    hm_read_self_heating_taylor          ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating_taylor.F90
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                         ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod                   ../starter/share/modules1/hm_option_read_mod.F
!||    hm_read_self_heating_tabulated_mod   ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating_tabulated.F90
!||    hm_read_self_heating_taylor_mod      ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating_taylor.F90
!||    submodel_mod                         ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_self_heating(                                       &
          ikey     ,type  ,iheat    ,nupar_heat  ,upar_heat    ,               &
          is_available,unitab,lsubmodel,iout     ,is_encrypted ,               &
          ntab_heat,itab_heat       ,x2vect      ,x3vect       ,               &
          x4vect   ,fscale          ,nvartmp     ,matparam     ,               &
          mtag     )   
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
          use hm_read_self_heating_taylor_mod
          use hm_read_self_heating_tabulated_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          character(len=20),       intent(in)    :: type                  !< Keyword type
          integer,                 intent(inout) :: iheat                 !< Self-heating type
          integer,                 intent(inout) :: nupar_heat            !< Number of self-heating parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_heat         !< Self-heating parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(inout) :: ntab_heat             !< Number of tabulated self-heating functions/tables
          integer,       dimension(100), intent(inout) :: itab_heat       !< Identifiers of tabulated self heating functions/tables
          real(kind=WP), dimension(100), intent(inout) :: x2vect          !< x2 scale factor for tabulated self heating
          real(kind=WP), dimension(100), intent(inout) :: x3vect          !< x3 scale factor for tabulated self heating
          real(kind=WP), dimension(100), intent(inout) :: x4vect          !< x4 scale factor for tabulated self heating
          real(kind=WP), dimension(100), intent(inout) :: fscale          !< y  scale factor for tabulated self heating
          integer,                 intent(inout) :: nvartmp               !< Number of variables used in tabulated self-heating
          type(matparam_struct_),  intent(inout) :: matparam              !< Material parameter data structure
          type(mlaw_tag_),         intent(inout) :: mtag                  !< Material tag for internal variables in element buffer
!===============================================================================
!    
          !=====================================================================
          !< Taylor-Quinney self heating parameters
          !=====================================================================
          if (type(1:6) == 'TAYLOR') then
            call hm_read_self_heating_taylor(                                  &
              ikey     ,iheat    ,nupar_heat   ,upar_heat   ,is_available,     &
              unitab   ,lsubmodel,iout         ,is_encrypted,matparam    )
          !=====================================================================
          !<  Tabulated self heating parameters
          !=====================================================================
          elseif (type(1:3) == 'TAB') then
            call hm_read_self_heating_tabulated(                               &
              ikey     ,iheat    ,ntab_heat,itab_heat   ,x2vect ,x3vect   ,    &
              x4vect   ,fscale   ,nvartmp  ,is_available,unitab ,lsubmodel,    &
              iout     ,is_encrypted       ,nupar_heat  ,upar_heat,matparam)
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
        end subroutine hm_read_self_heating
      end module hm_read_self_heating_mod