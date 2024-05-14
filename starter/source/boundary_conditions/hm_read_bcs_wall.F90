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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Reader subroutine for option /BCS/WALL
!! \details
      subroutine hm_read_bcs_wall(unitab, lsubmodel, igrnod, ngrnod, sensors, itabm1, numnod, multi_fvm)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use unitab_mod , only : unit_type_
        use submodel_mod , only : submodel_data, nsubmod
        use groupdef_mod , only : group_
        use sensor_mod , only : sensors_
        use message_mod , only : ancmsg, msgwarning, msgerror, aninfo
        use hm_option_read_mod , only : hm_option_read_key
        use multi_fvm_mod , only : multi_fvm_struct
        use bcs_mod , only : bcs
        use constant_mod , only : zero, em20, ep20
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
        implicit none
#include "my_real.inc"
#include "units_c.inc"
#include "nchar_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer, intent(in) :: ngrnod, numnod                            !< size for array definition
        integer, intent(in) :: itabm1(numnod)                            !< list of user node ids
        type(unit_type_), intent(in) :: unitab                           !< array used for Reader subroutines (optional units translation)
        type(submodel_data), dimension(nsubmod), intent(in) :: lsubmodel !< submodel data for Reader subroutines
        type (group_), dimension(ngrnod), target :: igrnod               !< data for group of nodes
        type (sensors_) ,intent(in) ,target  :: sensors                  !< data for sensors entities
        type(multi_fvm_struct), intent(inout) :: multi_fvm               !< data for collocated scheme (multifluid law 151)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        my_real :: tstart,tstop
        integer :: user_grnod_id, user_sensor_id
        integer :: internal_grnod_id, internal_sensor_id
        integer :: ii,jj
        integer :: id,uid,sub_id
        integer :: num_nodes_in_group
        integer :: ibufnodes(numnod)
        !logical :: l_tagnod(numnod)
        logical :: is_available_sensor,is_available_tstart,is_available_tstop,is_available_grnod
        logical :: is_found
        character*nchartitle :: titr
        character :: label*31, mess*40
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External Functions
! ----------------------------------------------------------------------------------------------------------------------
        integer,external :: nodgrnr5
! ----------------------------------------------------------------------------------------------------------------------
        data mess/'BOUNDARY CONDITIONS                     '/
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ----------------------------------------------------------------------------------------------------------------------
        ! if no option /BCS/WALL in input file then return
        if(bcs%num_wall == 0)return
        ! compatible only with collocated scheme (material law 151)
        if(.NOT. MULTI_FVM%IS_USED)then
          label = 'SLIDING WALL BOUNDARY CONDITION'
          call ancmsg(msgid=133,anmode=aninfo,msgtype=msgwarning,i1=id,c1=label,c2=label,c3=titr)
          return
        end if
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        !call hm_debug_print_option('/BCS/WALL')


        call hm_option_start('/BCS/WALL')
        allocate(bcs%wall(bcs%num_wall))
        write(iout, 2010)

        do ii = 1, bcs%num_wall

          !READER
          call hm_option_read_key(lsubmodel, option_id=id, unit_id=uid, submodel_id=sub_id, option_titr=titr)
          call hm_get_intv('set_ID', user_grnod_id, is_available_grnod, lsubmodel)
          call hm_get_intv('sensor_ID', user_sensor_id, is_available_sensor, lsubmodel)
          call hm_get_floatv('Tstart', tstart, is_available_tstart, lsubmodel, unitab)
          call hm_get_floatv('Tstop', tstop, is_available_tstop, lsubmodel, unitab)

          !PARAMETER CHECK
          if(is_available_tstart .or. is_available_tstop)then
            if(tstart /= zero .or. tstop /=zero)then
              bcs%wall(ii)%is_depending_on_time = .true.
            end if
          end if
          !DEFAULT
          if(tstop == zero)tstop = ep20
          if(tstop < tstart)then
            tstop = ep20
          end if

          !CHECK USER SET IDENTIFIER
          num_nodes_in_group = nodgrnr5(user_grnod_id,internal_grnod_id,ibufnodes,igrnod,itabm1,mess)

          !CHECK USER SENSOR IDENTIFIER
          internal_sensor_id = 0
          is_found = .false.
          if (user_sensor_id > 0) then
            do jj=1,sensors%nsensor
              if (user_sensor_id == sensors%sensor_tab(jj)%sens_id) then
                internal_sensor_id = jj
                is_found = .true.
                bcs%wall(ii)%is_depending_on_sensor = .true.
                if( bcs%wall(ii)%is_depending_on_time )then
                  bcs%wall(ii)%is_depending_on_time = .false.
                  label = 'SLIDING WALL BOUNDARY CONDITION'
                  !warn user that time parameters tstart tstop are ignored
                  call ancmsg(msgid=134,anmode=aninfo,msgtype=msgwarning,i1=id,c1=label,c2=label,c3=titr,i2=user_sensor_id)
                end if
                exit
              end if
            enddo
            if (.not. is_found) then
              label = 'SLIDING WALL BOUNDARY CONDITION'
              call ancmsg(msgid=1252,anmode=aninfo,msgtype=msgerror,i1=id,c1=label,c2=label,c3=titr,i2=user_sensor_id)
            endif
          endif

          if(.not.bcs%wall(ii)%is_depending_on_time .and. .not.bcs%wall(ii)%is_depending_on_sensor)then
            !when option is defined without any parameter then wall bcs is enabled from 0 to 1e20
            bcs%wall(ii)%is_depending_on_time = .true.
          end if

          !FILL BARRIER DATA STRUCTURE
          bcs%wall(ii)%is_enabled= .false.  !set by engine on cycle 0
          bcs%wall(ii)%user_id = id
          bcs%wall(ii)%grnod_id = internal_grnod_id
          bcs%wall(ii)%sensor_id = internal_sensor_id
          bcs%wall(ii)%tstart = tstart
          bcs%wall(ii)%tstop = tstop

          write(iout, 2001) id, trim(titr)
          write(iout, 2011) user_grnod_id !mandatory
          if (is_available_sensor) write(iout, 2012) user_sensor_id
          if (is_available_tstart) write(iout, 2013) tstart
          if (is_available_tstop)  write(iout, 2014) tstop
          write(iout, 2015)

        end do !next ii


        return
! ----------------------------------------------------------------------------------------------------------------------
2010    FORMAT(5X, &
          5X,/,'     SLIDING WALL BOUNDARY CONDITIONS  ', /,&
          5X, '--------------------------------  ', /)

2001    format(5X, 'id:', I0,',',5X,A)
2011    format(5X, 'set identifier . . . . . . . . . . . . =', I10)
2012    format(5X, 'sensor identifier  . . . . . . . . . . =', I10)
2013    format(5X, 'start time . . . . . . . . . . . . . . =', 1PG20.13)
2014    format(5X, 'stop time  . . . . . . . . . . . . . . =', 1PG20.13)
2015    format(5X)

! ----------------------------------------------------------------------------------------------------------------------
      end subroutine hm_read_bcs_wall
