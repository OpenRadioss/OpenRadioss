!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    hm_preread_inivel_mod   ../starter/source/initial_conditions/general/inivel/hm_preread_inivel.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                  ../starter/source/starter/lectur.F
      !||====================================================================
      module hm_preread_inivel_mod
!        
       contains
  !! \brief subroutine to get number of /INIVEL using T_start or sensor
      !||====================================================================
      !||    hm_preread_inivel      ../starter/source/initial_conditions/general/inivel/hm_preread_inivel.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                 ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    submodel_mod           ../starter/share/modules1/submodel_mod.F
      !||====================================================================
        subroutine hm_preread_inivel(LSUBMODEL,unitab,hm_ninvel,ninivelt)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE UNITAB_MOD
          use SUBMODEL_MOD
          USE HM_OPTION_READ_MOD
          use MESSAGE_MOD
          use NAMES_AND_TITLES_MOD , only : ncharkey
          use constant_mod,          only : zero
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB
      TYPE(SUBMODEL_DATA)          :: LSUBMODEL(*)
      integer, intent(in   )       :: hm_ninvel
      integer, intent(inout)       :: ninivelt
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer  :: i,id,sens_id
      logical is_available
      character(len=ncharkey) :: key
      my_real  :: tstart
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      is_available = .false.
       

      call hm_option_start('/INIVEL')
      ninivelt = 0 

      do i =1,hm_ninvel
        !---set cursor on next inivel option
        call hm_option_read_key(lsubmodel,OPTION_ID = id,KEYWORD2 = key)
!
        if(key(1:4)=='NODE') cycle
        call hm_get_intv('sensor_id',sens_id,is_available,lsubmodel)
        call hm_get_floatv('tstart',tstart,is_available,lsubmodel,unitab)
        if (tstart>zero .or. sens_id>0) ninivelt = ninivelt + 1
      end do
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine hm_preread_inivel
      end module hm_preread_inivel_mod
