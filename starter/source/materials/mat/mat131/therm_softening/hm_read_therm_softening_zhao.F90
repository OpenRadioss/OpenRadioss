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
!||    hm_read_therm_softening_zhao_mod   ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening_zhao.F90
!||--- called by ------------------------------------------------------
!||    hm_read_therm_softening            ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening.F90
!||====================================================================
      module hm_read_therm_softening_zhao_mod
! \brief Read Zhao thermal softening input data for /MAT/LAW131
! \details Read the Zhao thermal softening model parameters
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_therm_softening_zhao   ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening_zhao.F90
!||--- called by ------------------------------------------------------
!||    hm_read_therm_softening        ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_float_array_index       ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod             ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                   ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_therm_softening_zhao(                               &
          ikey     ,itherm   ,nupar_therm  ,upar_therm  ,is_available,         &
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
          integer,                 intent(inout) :: itherm                !< Thermal softening type
          integer,                 intent(inout) :: nupar_therm           !< Number of thermal softening parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_therm        !< Thermal softening parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          type(matparam_struct_),  intent(inout) :: matparam              !< Material parameter data structure
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          real(kind=WP) :: tref,mu
!===============================================================================
!       
          !===================================================================
          !< Johnson-Cook thermal softening parameters
          !===================================================================
          call hm_get_float_array_index("THERM_ZHAO_TREF" ,tref   ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("THERM_ZHAO_MU"   ,mu     ,ikey,is_available,lsubmodel,unitab)
          !< Thermal softening type
          itherm = 2
          !< Number of parameters
          nupar_therm = 1
          !< Save thermal softening parameters
          upar_therm(1) = mu
          matparam%therm%tref  = tref
          !< Printing thermal softening parameters
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) tref,mu
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"ZHAO THERMAL SOFTENING                                 ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"REFERENCE TEMPERATURE (TREF) . . . . . . . . . . . . .=",1PG20.13/&
          5X,'SOFTENING SLOPE (MU) . . . . . . . . . . . . . . . . .=',1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_therm_softening_zhao
      end module hm_read_therm_softening_zhao_mod