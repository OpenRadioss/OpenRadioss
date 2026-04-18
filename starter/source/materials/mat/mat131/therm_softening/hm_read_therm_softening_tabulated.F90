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
!||    hm_read_therm_softening_tabulated_mod   ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening_tabulated.F90
!||--- called by ------------------------------------------------------
!||    hm_read_therm_softening                 ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening.F90
!||====================================================================
      module hm_read_therm_softening_tabulated_mod
! \brief Read tabulated thermal softening input data for /MAT/LAW131
! \details Read the tabulated thermal softening model parameters
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_therm_softening_tabulated   ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening_tabulated.F90
!||--- called by ------------------------------------------------------
!||    hm_read_therm_softening             ../starter/source/materials/mat/mat131/therm_softening/hm_read_therm_softening.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_float_array_index            ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_int_array_index              ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod                  ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                        ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_therm_softening_tabulated(                          &
          ikey     ,itherm   ,ntab_therm,itab_therm  ,x2vect ,x3vect   ,       &
          x4vect   ,fscale   ,nvartmp   ,is_available,unitab ,lsubmodel,       &
          iout     ,is_encrypted,matparam)

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
          integer,                 intent(inout) :: ntab_therm            !< Number of tabulated thermal softening functions/tables
          integer,       dimension(100), intent(inout) :: itab_therm      !< Identifiers of tabulated thermal softening functions/tables
          real(kind=WP), dimension(100), intent(inout) :: x2vect          !< x2 scale factor for tabulated thermal softening
          real(kind=WP), dimension(100), intent(inout) :: x3vect          !< x3 scale factor for tabulated thermal softening
          real(kind=WP), dimension(100), intent(inout) :: x4vect          !< x4 scale factor for tabulated thermal softening
          real(kind=WP), dimension(100), intent(inout) :: fscale          !< y  scale factor for tabulated thermal softening
          integer,                 intent(inout) :: nvartmp               !< Number of variables used in tabulated thermal softening
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          type(matparam_struct_),  intent(inout) :: matparam              !< Material parameters data
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: tab_id
          real(kind=WP) :: tref
!===============================================================================
!       
          !===================================================================
          !< Tabulated thermal softening parameters
          !===================================================================
          call hm_get_int_array_index  ("THERM_TAB_ID"    ,tab_id ,ikey,is_available,lsubmodel)
          call hm_get_float_array_index("THERM_TAB_TREF"  ,tref   ,ikey,is_available,lsubmodel,unitab)
          !< Thermal softening type
          itherm = 3
          !< Number of tabulated thermal softening functions/tables
          ntab_therm = 1
          !< Number of variables used in tabulated hardening
          nvartmp = 4
          !< Save table id
          itab_therm(1) = tab_id
          !< Save reference temperature
          matparam%therm%tref = tref
          !< Save scale factors
          x2vect(1) = one
          x3vect(1) = one
          x4vect(1) = one
          fscale(1) = one
          !< Printing thermal softening parameters
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) tab_id,tref
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"TABULATED THERMAL SOFTENING                            ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"TABULATED SOFTENING FUNCTION ID (TAB_ID) . . . . . . .=",I10/    &
          5X,"REFERENCE TEMPERATURE (TREF) . . . . . . . . . . . . .=",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_therm_softening_tabulated
      end module hm_read_therm_softening_tabulated_mod