!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    read_sensor_python_mod   ../starter/source/tools/sensor/hm_read_sensor_python.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_sensors          ../starter/source/tools/sensor/hm_read_sensors.F
      !||====================================================================
      module read_sensor_python_mod                
      contains
!! \details Read the python function defined by /FUNCT_PYTHON/
      !||====================================================================
      !||    read_sensor_python      ../starter/source/tools/sensor/hm_read_sensor_python.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_sensors         ../starter/source/tools/sensor/hm_read_sensors.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                  ../starter/source/output/message/message.F
      !||    hm_get_intv             ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_get_string_index     ../starter/source/devtools/hm_reader/hm_get_string_index.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod      ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod             ../starter/share/message_module/message_mod.F
      !||    submodel_mod            ../starter/share/modules1/submodel_mod.F
      !||====================================================================
        subroutine read_sensor_python(python,sensor_ptr ,sens_id  ,lsubmodel )

! ----------------------------------------------------------------------------------------------------------------------
!                                                      modules
! ----------------------------------------------------------------------------------------------------------------------
          use sensor_mod
          use names_and_titles_mod, only : ncharline, nchartitle
          use iso_c_binding , only : c_char, c_null_char
          use message_mod
          use submodel_mod
          use hm_option_read_mod
          use python_funct_mod
          use constant_mod
          use my_alloc_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          type (python_) ,intent(inout) :: python
          integer ,intent(in) :: sens_id
          type (sensor_str_) ,intent(inout) :: sensor_ptr
          type (submodel_data) ,dimension(nsubmod) ,intent(in) :: lsubmodel
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          character(len=ncharline) :: rline
          logical :: is_available
          integer :: nlines
          integer :: nb_funct
          integer :: i,j,l
          integer :: func_id
          integer :: position_in_code
          character(kind=c_char, len=:), allocatable :: code
          integer :: line_len
          integer :: error,error_old
          double precision :: argin(1), argout(1)
          character(len=nchartitle) :: titr !function name
! ----------------------------------------------------------------------------------------------------------------------
!                                                      body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(character(kind=c_char, len=max_code_length) :: code)
          sensor_ptr%type    = sensor_type_python 
          sensor_ptr%sens_id = sens_id
          sensor_ptr%status  = 0        ! status = deactivated
          sensor_ptr%tstart  = zero
          sensor_ptr%tcrit   = infinity 
          sensor_ptr%tmin    = zero         ! tmin global
          sensor_ptr%tdelay  = zero ! time delay before activation
          sensor_ptr%value   = zero ! stop time
          sensor_ptr%npari  = 0 
          sensor_ptr%nparr  = 0 
          sensor_ptr%nvar   = 0 
          call my_alloc(sensor_ptr%iparam,sensor_ptr%npari)
          call my_alloc(sensor_ptr%rparam,sensor_ptr%nparr)
          call my_alloc(sensor_ptr%var,sensor_ptr%nvar)
          python%nb_functs = python%nb_functs + 1
          i = python%nb_functs
          sensor_ptr%python_function_id = i
          func_id = sens_id
          code(1:max_code_length) = repeat(' ',max_code_length)
          !call hm_option_read_key(lsubmodel, option_id = func_id) sens_id
          call hm_get_intv('Number_of_datalines' ,nlines ,is_available, lsubmodel)
          python%functs(i)%num_lines = nlines
          python%functs(i)%user_id = func_id
!         write(6,*) "Python test: funct_id",func_id,"nlines",nlines
          position_in_code = 1
          if(nlines > 0) then
            ! create tempo file
            do j=1,nlines
              call hm_get_string_index('arraydatalines', rline, j, ncharline, is_available)
              line_len = len_trim(rline)
              code(position_in_code:position_in_code+line_len-1) = trim(rline)
              ! add c_null_char
              position_in_code = position_in_code + line_len
              code(position_in_code:position_in_code) = c_null_char
              position_in_code = position_in_code + 1
            enddo
            l = i + python%sensor_offset
            call python_funct_init(python%functs(i), code, position_in_code, nlines)
            call python_check_function(python%functs(i)%name,error)
            if(error > 0 ) then
              ! converts python%functs(i)%name of type  character(kind=c_char), dimension(:), allocatable :: name
              ! initialize titr with "/FUNCT_PYTHON"
              titr = repeat(' ',nchartitle)
              call ancmsg(MSGID=3038,&
              &MSGTYPE=MSGERROR,&
              &ANMODE=ANINFO_BLIND_2,&
              &I1=func_id)
            endif
            ! duplicate the python function into the sensor structure (no great solution for target and allocatable in Fortran)
            call copy_python_function(python%functs(i), sensor_ptr%python_function)

          else
              ! missing code for /SENSOR/PYTHON
          endif
           ! enddo
          deallocate(code)
          return
        end
! end the module
      end module read_sensor_python_mod
