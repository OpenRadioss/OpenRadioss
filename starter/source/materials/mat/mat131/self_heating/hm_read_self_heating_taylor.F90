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
!||    hm_read_self_heating_taylor_mod   ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating_taylor.F90
!||--- called by ------------------------------------------------------
!||    hm_read_self_heating              ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating.F90
!||====================================================================
      module hm_read_self_heating_taylor_mod
! \brief Read Taylor-Quinney self-heating input data for /MAT/LAW131
! \details Read the constant Taylor-Quinney self-heating model parameters
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_self_heating_taylor   ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating_taylor.F90
!||--- called by ------------------------------------------------------
!||    hm_read_self_heating          ../starter/source/materials/mat/mat131/self_heating/hm_read_self_heating.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_float_array_index      ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod            ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                  ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_self_heating_taylor(                                &
          ikey     ,iheat    ,nupar_heat   ,upar_heat   ,is_available,         &
          unitab   ,lsubmodel,iout         ,is_encrypted,matparam    )
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
          integer,                 intent(inout) :: iheat                 !< Thermal softening type
          integer,                 intent(inout) :: nupar_heat            !< Number of thermal softening parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_heat         !< Thermal softening parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          type(matparam_struct_),  intent(inout) :: matparam              !< Material parameter data structure
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          real(kind=WP) :: t0,eta,cp,deis,dead
!===============================================================================
!       
          !===================================================================
          !< Taylor-Quinney self heating parameters
          !===================================================================
          call hm_get_float_array_index("HEAT_TAYLOR_T0"   ,t0     ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HEAT_TAYLOR_ETA"  ,eta    ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HEAT_TAYLOR_CP"   ,cp     ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HEAT_TAYLOR_DEIS" ,deis   ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HEAT_TAYLOR_DEAD" ,dead   ,ikey,is_available,lsubmodel,unitab)
          !< Self-heating type
          iheat = 1
          !< Number of parameters
          nupar_heat = 3
          !< Check parameters
          if (deis == zero) deis = -two*infinity
          if (dead == zero) dead = -infinity
          eta = min(one, max(zero, eta))
          !< Save self-heating parameters
          matparam%therm%tini  = t0
          matparam%therm%rhocp = matparam%rho*cp
          upar_heat(1) = eta
          upar_heat(2) = deis
          upar_heat(3) = dead
          !< Printing self heating parameters
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) t0,eta,cp,deis,dead
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"TAYLOR-QUINNEY SELF HEATING                            ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"INITIAL TEMPERATURE (T0) . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"TAYLOR-QUINNEY COEFFICIENT (ETA) . . . . . . . . . . .=",1PG20.13/&
          5X,"THERMAL MASSIC CAPACITY (CP) . . . . . . . . . . . . .=",1PG20.13/&
          5X,"ISOTHERMAL STRAIN RATE (DEIS). . . . . . . . . . . . .=",1PG20.13/&
          5X,"ADIABATIC STRAIN RATE (DEAD) . . . . . . . . . . . . .=",1PG20.13)
! -------------------------------------------------------------------------------
        end subroutine hm_read_self_heating_taylor
      end module hm_read_self_heating_taylor_mod