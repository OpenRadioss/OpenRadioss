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
      !||    check_sorting_criteria_mod   ../engine/source/interfaces/intsort/check_sorting_criteria.F90
      !||--- called by ------------------------------------------------------
      !||    i10main_tri                  ../engine/source/interfaces/intsort/i10main_tri.F
      !||    i11main_tri                  ../engine/source/interfaces/intsort/i11main_tri.F
      !||    i20main_tri                  ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i21main_tri                  ../engine/source/interfaces/intsort/i21main_tri.F
      !||    i22main_tri                  ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i23main_tri                  ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i24main_tri                  ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i25main_tri                  ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i7main_tri                   ../engine/source/interfaces/intsort/i7main_tri.F
      !||    inter_sort_07                ../engine/source/interfaces/int07/inter_sort_07.F
      !||====================================================================
      module check_sorting_criteria_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine checks if the current "interface_id" interface needs to be sorted
      !||====================================================================
      !||    check_sorting_criteria   ../engine/source/interfaces/intsort/check_sorting_criteria.F90
      !||--- called by ------------------------------------------------------
      !||    i10main_tri              ../engine/source/interfaces/intsort/i10main_tri.F
      !||    i11main_tri              ../engine/source/interfaces/intsort/i11main_tri.F
      !||    i20main_tri              ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i21main_tri              ../engine/source/interfaces/intsort/i21main_tri.F
      !||    i22main_tri              ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i23main_tri              ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i24main_tri              ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i25main_tri              ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i7main_tri               ../engine/source/interfaces/intsort/i7main_tri.F
      !||    inter_sort_07            ../engine/source/interfaces/int07/inter_sort_07.F
      !||--- calls      -----------------------------------------------------
      !||    i20xsinir                ../engine/source/interfaces/intsort/i20main_tri.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod             ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine check_sorting_criteria( need_computation,interface_id,nipari,nspmd,task_id,ipari,time,intbuf_tab )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use intbufdef_mod
          use constant_mod , only : zero
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "macro.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, intent(inout) :: need_computation !< boolean, true if the interface must be sorted
          integer, intent(in) :: interface_id !< id of the current interface
          integer, intent(in) :: nipari !< first dimension of ipari array
          integer, intent(in) :: nspmd !< number of processor
          integer, intent(in) :: task_id !< task id
          integer, dimension(nipari), intent(inout) :: ipari !< interface data
          my_real, intent(in) :: time !< current time
          type(intbuf_struct_), intent(inout) :: intbuf_tab !< interface data structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: my_bool,t_start_condition,t_stop_condition,distance_condition
          integer :: interface_type,sensor_id
          my_real :: t_start,t_stop,distance
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to mimimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! -------------------------
          my_bool = .false.
          need_computation = .false.
          t_start = intbuf_tab%variables( t_start_index ) ! get the start time 
          t_stop = intbuf_tab%variables( t_stop_index )   ! get the stop time
          distance = intbuf_tab%variables( distance_index ) ! get the distance criteria
    
          t_start_condition = (t_start>time)    ! if true, computation is not needed
          t_stop_condition = (time>t_stop)      ! if true, computation is not needed
          distance_condition = (distance>zero)  ! if true, computation is not needed

          interface_type = ipari(MACRO_NTY)
          ! -------------
          if(interface_type==7.or.interface_type==10.or.interface_type==11.or.  &
             interface_type==21.or.interface_type==23.or.interface_type==24) then

            ! check the 3 conditions : start / stop / distance
            my_bool = t_start_condition.or.t_stop_condition.or.distance_condition
            need_computation = .not.(my_bool)

          elseif(interface_type==20.or.interface_type==22) then

            ! check only 2 conditions : start / stop
            my_bool = t_start_condition.or.t_stop_condition
            need_computation = .not.(my_bool)
            ! ------
            ! only for interface type = 20
            if(need_computation.and.interface_type==20) then
              need_computation=need_computation.and.(.not.(distance_condition))
              if(distance_condition.and.nspmd>1) then
                 call i20xsinir( ipari(MACRO_NSNR),ipari(MACRO_NSNER),  &
                                 task_id,interface_id,intbuf_tab%stfac )
              endif
            endif
            ! ------

          elseif(interface_type==25) then

            ! check only 2 conditions : stop / distance
            sensor_id = ipari(MACRO_IDSENS)
            if(sensor_id==0) then
              my_bool = t_stop_condition
            endif
            need_computation = .not.(my_bool.or.distance_condition)

          endif
          ! -------------
          return
          ! -------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine check_sorting_criteria
      end module check_sorting_criteria_mod
