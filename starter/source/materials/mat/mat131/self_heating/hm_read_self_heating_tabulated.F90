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
!||    hm_read_self_heating_tabulated_mod   ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating_tabulated.F90
!||--- called by ------------------------------------------------------
!||    hm_read_self_heating                 ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating.F90
!||====================================================================
      module hm_read_self_heating_tabulated_mod
! \brief Read tabulated self-heating input data for /MAT/LAW131
! \details Read the tabulated Taylor-Quinney self-heating model parameters
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_self_heating_tabulated   ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating_tabulated.F90
!||--- called by ------------------------------------------------------
!||    hm_read_self_heating             ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_float_array_index         ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv_dim                ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_int_array_index           ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod               ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                     ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_self_heating_tabulated(                             &
          ikey     ,iheat    ,ntab_heat ,itab_heat   ,x2vect ,x3vect   ,       &
          x4vect   ,fscale   ,nvartmp   ,is_available,unitab ,lsubmodel,       &
          iout     ,is_encrypted,nupar_heat,upar_heat,matparam)

!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          integer,                 intent(inout) :: iheat                 !< Self heating type
          integer,                 intent(inout) :: ntab_heat             !< Number of tabulated self heating functions/tables
          integer,       dimension(100), intent(inout) :: itab_heat       !< Identifiers of tabulated self heating functions/tables
          real(kind=WP), dimension(100), intent(inout) :: x2vect          !< x2 scale factor for tabulated self heating
          real(kind=WP), dimension(100), intent(inout) :: x3vect          !< x3 scale factor for tabulated self heating
          real(kind=WP), dimension(100), intent(inout) :: x4vect          !< x4 scale factor for tabulated self heating
          real(kind=WP), dimension(100), intent(inout) :: fscale          !< y  scale factor for tabulated self heating
          integer,                 intent(inout) :: nvartmp               !< Number of variables used in tabulated self heating
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(inout) :: nupar_heat            !< Number of self-heating parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_heat         !< Self-heating parameters
          type(matparam_struct_),  intent(inout) :: matparam              !< Material parameter data structure
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: func_id
          real(kind=WP) :: cp,eta,t0,xscale,yscale
!===============================================================================
!       
          !===================================================================
          !< Tabulated self heating parameters
          !===================================================================
          call hm_get_float_array_index("HEAT_TAYLOR_T0" ,t0     ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HEAT_TAYLOR_ETA",eta    ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HEAT_TAYLOR_CP" ,cp     ,ikey,is_available,lsubmodel,unitab)
          call hm_get_int_array_index  ("HEAT_TAB_ID"    ,func_id,ikey,is_available,lsubmodel)
          call hm_get_float_array_index("HEAT_TAB_XSCALE",xscale ,ikey,is_available,lsubmodel,unitab)
          !< Self heating type
          iheat = 2
          !< Number of tabulated hardening functions/tables
          ntab_heat = 1
          !< Check default values
          if (xscale == zero) then 
            call hm_get_floatv_dim('HEAT_TAB_XSCALE',xscale,is_available,lsubmodel,unitab)
          endif
          !< Number of variables used in tabulated hardening
          nvartmp = 3
          !< Save table id
          itab_heat(1) = func_id
          !< Save scale factors
          x2vect(1) = xscale
          x3vect(1) = one
          x4vect(1) = one
          fscale(1) = one
          !< Number of parameters
          nupar_heat = 1
          !< Save self-heating parameters
          matparam%therm%tini  = t0
          matparam%therm%rhocp = matparam%rho*cp
          upar_heat(1) = eta
          !< Printing thermal softening parameters
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) t0,eta,cp,func_id,xscale
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"TABULATED SELF HEATING                                 ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"INITIAL TEMPERATURE (T0) . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"TAYLOR-QUINNEY COEFFICIENT (ETA) . . . . . . . . . . .=",1PG20.13/&
          5X,"THERMAL MASSIC CAPACITY (CP) . . . . . . . . . . . . .=",1PG20.13/&
          5X,"TABULATED FUNCTION ID. . . . . . . . . . . . . . . . .=",I10/&
          5X,"STRAIN RATE SCALE FACTOR (ETA_XSCALE). . . . . . . . .=",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_self_heating_tabulated
      end module hm_read_self_heating_tabulated_mod