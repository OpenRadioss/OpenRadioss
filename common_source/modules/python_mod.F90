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
      !||    python_funct_mod               ../common_source/modules/python_mod.F90
      !||--- called by ------------------------------------------------------
      !||    alefvm_grav_init               ../engine/source/ale/alefvm/alefvm_grav_init.F
      !||    alemain                        ../engine/source/ale/alemain.F
      !||    alewdx                         ../engine/source/ale/grid/alewdx.F
      !||    cfield_1                       ../engine/source/loads/general/load_centri/cfield.F
      !||    convec                         ../engine/source/constraints/thermic/convec.F
      !||    daasolv                        ../engine/source/fluid/daasolv.F
      !||    daasolvp                       ../engine/source/fluid/daasolvp.F
      !||    ddsplit                        ../starter/source/restart/ddsplit/ddsplit.F
      !||    ebcs11                         ../engine/source/boundary_conditions/ebcs/ebcs11.F90
      !||    ebcs_main                      ../engine/source/boundary_conditions/ebcs/ebcs_main.F
      !||    execargcheck                   ../engine/source/engine/execargcheck.F
      !||    fixflux                        ../engine/source/constraints/thermic/fixflux.F
      !||    fixvel                         ../engine/source/constraints/general/impvel/fixvel.F
      !||    flow0                          ../engine/source/fluid/flow0.F
      !||    force                          ../engine/source/loads/general/force.F90
      !||    forcefingeo                    ../engine/source/loads/general/forcefingeo.F
      !||    forcepinch                     ../engine/source/loads/general/forcepinch.F
      !||    forint                         ../engine/source/elements/forint.F
      !||    funct_python_update_elements   ../engine/source/tools/curve/funct_python_update_elements.F90
      !||    gravit                         ../engine/source/loads/general/grav/gravit.F
      !||    gravit_fvm_fem                 ../engine/source/loads/general/grav/gravit_fvm_fem.F
      !||    hm_read_funct_python           ../starter/source/tools/curve/hm_read_funct_python.F90
      !||    hm_read_sensors                ../starter/source/tools/sensor/hm_read_sensors.F
      !||    incpflow                       ../engine/source/fluid/incpflow.F
      !||    lag_fxv                        ../engine/source/tools/lagmul/lag_fxv.F
      !||    lag_fxvp                       ../engine/source/tools/lagmul/lag_fxv.F
      !||    lag_mult                       ../engine/source/tools/lagmul/lag_mult.F
      !||    lag_multp                      ../engine/source/tools/lagmul/lag_mult.F
      !||    lectur                         ../engine/source/input/lectur.F
      !||    load_pressure                  ../engine/source/loads/general/load_pressure/load_pressure.F
      !||    nbfunct                        ../starter/source/tools/curve/nbfunc.F
      !||    pfluid                         ../engine/source/loads/general/pfluid/pfluid.F
      !||    python_call_funct_cload_dp     ../engine/source/loads/general/python_call_funct_cload.F90
      !||    python_call_funct_cload_sp     ../engine/source/loads/general/python_call_funct_cload.F90
      !||    python_duplicate_nodes         ../starter/source/spmd/domain_decomposition/python_duplicate_nodes.F90
      !||    python_register                ../engine/source/tools/curve/python_register.F90
      !||    r1def3                         ../engine/source/elements/spring/r1def3.F
      !||    r23forc3                       ../engine/source/elements/spring/r23forc3.F
      !||    r23l108def3                    ../engine/source/elements/spring/r23l108def3.F
      !||    r23l113def3                    ../engine/source/elements/spring/r23l113def3.F
      !||    r23l114def3                    ../engine/source/elements/spring/r23l114def3.F
      !||    r23law108                      ../engine/source/elements/spring/r23law108.F
      !||    r23law113                      ../engine/source/elements/spring/r23law113.F
      !||    r23law114                      ../engine/source/elements/spring/r23law114.F
      !||    r26def3                        ../engine/source/elements/spring/r26def3.F
      !||    r26sig                         ../engine/source/elements/spring/r26sig.F
      !||    r27def3                        ../engine/source/elements/spring/r27def3.F
      !||    r2def3                         ../engine/source/elements/spring/r2def3.F
      !||    r3def3                         ../engine/source/elements/spring/r3def3.F
      !||    r4def3                         ../engine/source/elements/spring/r4def3.F
      !||    r6def3                         ../engine/source/elements/spring/r6def3.F
      !||    radiation                      ../engine/source/constraints/thermic/radiation.F
      !||    radioss2                       ../engine/source/engine/radioss2.F
      !||    rdresb                         ../engine/source/output/restart/rdresb.F
      !||    read_sensor_python             ../starter/source/tools/sensor/hm_read_sensor_python.F90
      !||    read_sensors                   ../engine/source/output/restart/read_sensors.F
      !||    redef3                         ../engine/source/elements/spring/redef3.F90
      !||    redef3_law113                  ../engine/source/elements/spring/redef3_law113.F
      !||    resol                          ../engine/source/engine/resol.F
      !||    resol_head                     ../engine/source/engine/resol_head.F
      !||    rforc3                         ../engine/source/elements/spring/rforc3.F
      !||    rgwal1                         ../engine/source/ale/grid/rgwal1.F
      !||    sensor_base                    ../engine/source/tools/sensor/sensor_base.F
      !||    sensor_init                    ../engine/source/tools/sensor/sensor_init.F
      !||    sensor_mod                     ../common_source/modules/sensor_mod.F90
      !||    sensor_python                  ../engine/source/tools/sensor/sensor_python.F90
      !||    timfun                         ../engine/source/tools/curve/timfun.F
      !||    vinter_mixed                   ../engine/source/tools/curve/vinter_mixed.F90
      !||    write_sensors                  ../engine/source/output/restart/write_sensors.F
      !||    wrrestp                        ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||    python_element_mod             ../common_source/modules/python_element_mod.F90
      !||====================================================================
      module python_funct_mod
        use iso_c_binding
        use python_element_mod
        integer, parameter :: max_line_length = 500 !< the maximum length of a line of code of python function
        integer, parameter :: max_num_lines = 1000 !< the maximum number of lines of python function
        integer, parameter :: max_code_length = max_line_length*max_num_lines
        integer, parameter :: max_variable_length = 100 !< the maximum length of a variable name
        integer, parameter :: funct_python_nsamples = 50 !< number of points to sample the python function
! global variable
        integer :: python_error !< true if the starter command line had the option "-python"
! use iso_c_binding to bind python_init to cpp_python_init and python_finalize to cpp_python_finalize
        interface
          subroutine python_initialize(ok) bind(c, name="cpp_python_initialize")
            use iso_c_binding
            integer(kind=c_int), intent(inout) :: ok  !< error code
          end subroutine python_initialize
          subroutine python_finalize() bind(c, name="cpp_python_finalize")
          end subroutine python_finalize

          ! run the python code that initializes the environment (if defined as /FUNCT_PYTHON/ with the name initialize_environment) 
          subroutine python_load_environment() bind(c, name="cpp_python_load_environment")
          end subroutine python_load_environment

          ! add a function to the python dictionary
          subroutine python_register_function(name, code, num_lines) bind(c, name="cpp_python_register_function")
            use iso_c_binding
            character(kind=c_char), dimension(*) :: name !< intent out: extracted from the code
            integer(kind=c_int), value :: num_lines
            character(kind=c_char), dimension(500,*) :: code
          end subroutine python_register_function
          ! call a function from the python dictionary
          subroutine python_call_function(name, num_args, args, num_return, return_values) &
            bind(c, name="cpp_python_call_function")
            use iso_c_binding
            character(kind=c_char), dimension(*), intent(in) :: name !< intent in
            integer(kind=c_int), value :: num_args
            integer(kind=c_int), value :: num_return
            real(kind = c_double), intent(in) :: args(num_args)
            real(kind = c_double), intent(out) :: return_values(num_return)
          end subroutine python_call_function
          subroutine python_sample_function(name, X, Y, N) bind(c, name="cpp_python_sample_function")
            use iso_c_binding
            character(kind=c_char), dimension(*), intent(in) :: name
            integer(kind=c_int), value, intent(in) :: N !< sample size
            real(kind=c_double), dimension(N), intent(inout) :: X
            real(kind=c_double), dimension(N), intent(inout) :: Y
          end subroutine python_sample_function

          ! a subroutine that checks if the function works, and returns a nonzero error code if it does not
          subroutine python_call_function_with_state(name, return_value) &
            bind(c, name="cpp_python_call_function_with_state") ! def: my_sensor(state_dictionary):
            use iso_c_binding
            character(kind=c_char), dimension(*), intent(in) :: name !< intent in
            real(kind = c_double), intent(out) :: return_value
          end subroutine python_call_function_with_state

          ! a subroutine that check if the function works, and return an nonzero error code if it does not
          subroutine python_check_function(name, error) bind(c, name="cpp_python_check_function")
            use iso_c_binding
            character(kind=c_char), dimension(*) :: name
            integer(kind=c_int), intent(out) :: error
          end subroutine python_check_function

          subroutine python_update_time(time,dt) bind(c, name="cpp_python_update_time")
            use iso_c_binding
#ifdef MYREAL8
            real(kind = c_double), value, intent(in) :: time
            real(kind = c_double), value, intent(in) :: dt
#else
            real(kind = c_float), value, intent(in) :: time
            real(kind = c_float), value, intent(in) :: dt
#endif
          end subroutine python_update_time

          !interface for    void cpp_python_update_nodal_entities(char *name, int len_name, my_real *values)
          subroutine python_set_node_values(numnod, name_len, name, val) bind(c, name="cpp_python_update_nodal_entity")
            use iso_c_binding
            integer(kind=c_int), value, intent(in) :: numnod
            integer(kind=c_int), value, intent(in) :: name_len
            character(kind=c_char), dimension(name_len), intent(in) :: name
#ifdef MYREAL8
            real(kind=c_double), dimension(3,numnod), intent(in) :: val
#else
            real(kind=c_float), dimension(3,numnod), intent(in) :: val
#endif
          end subroutine python_set_node_values
          subroutine python_update_active_node_values(name_len, name, val) bind(c, name="cpp_python_update_active_node")
            use iso_c_binding
            integer(kind=c_int), value, intent(in) :: name_len
            character(kind=c_char), dimension(name_len), intent(in) :: name
            real(kind=c_double), dimension(3), intent(in) :: val
          end subroutine python_update_active_node_values


          subroutine python_get_number_of_nodes(number_of_nodes) bind(c, name="cpp_python_get_number_of_nodes")
            use iso_c_binding
            integer(kind=c_int), intent(out) :: number_of_nodes
          end subroutine python_get_number_of_nodes

          subroutine python_get_nodes(nodes_global_ids) &
            bind(c, name="cpp_python_get_nodes")
            use iso_c_binding
            integer(kind=c_int), intent(inout) :: nodes_global_ids(*)
          end subroutine python_get_nodes

          subroutine python_update_sensors(types, uids, statuses, results, nsensor) &
            bind(c, name="cpp_python_update_sensors")
            use iso_c_binding
            integer(kind=c_int), intent(in) :: types(*)
            integer(kind=c_int), intent(in) :: uids(*)
            integer(kind=c_int), intent(in) :: statuses(*)
            real(kind=c_double), intent(in) :: results(*)
            integer(kind=c_int), intent(in) :: nsensor
          end subroutine python_update_sensors


          !interface for    void cpp_create_node_mapping(int * itab, int *num_nodes)
          subroutine python_create_node_mapping(itab, num_nodes) &
            bind(c, name="cpp_python_create_node_mapping")
            use iso_c_binding
            integer(kind=c_int), intent(in) :: num_nodes
            integer(kind=c_int), intent(in) :: itab(*)
          end subroutine python_create_node_mapping

        end interface
        interface python_call_funct1D
          module procedure python_call_funct1D_sp
          module procedure python_call_funct1D_dp
        end interface python_call_funct1D
        interface python_deriv_funct1D
          module procedure python_deriv_funct1D_sp
          module procedure python_deriv_funct1D_dp
        end interface python_deriv_funct1D


! ----------------------------------------------------------------------------------------------------------------------
!                                               Type definitions
! ----------------------------------------------------------------------------------------------------------------------
!! \brief the python function structure: it contains the python code in plain text
        type :: python_function
          character(kind=c_char), dimension(:), allocatable :: name !< the name of the python function
          character(kind=c_char), dimension(:), allocatable :: code !< the code of the python function
          integer :: len_name !< the length of the name
          integer :: len_code !< the length of the code
          integer :: num_lines !< the number of lines of the code
          integer :: num_args !< the number of arguments of the function (1 for 1D function)
          integer :: num_return !< the number of return values of the function (1 for 1D function)
          integer :: user_id !< the user id of the function
        end type python_function
! ----------------------------------------------------------------------------------------------------------------------
!! \brief the python structure: it contains the python functions
        type python_
          type(python_function), dimension(:), allocatable:: functs !< the python functions
          integer :: funct_offset !< the local id of the python function starts after the id of other kind of functions
          integer :: nb_functs !< the number of python functions
          integer :: sensor_offset !< the local id of the python sensor starts after the id of other kind of sensors
          integer :: nb_sensors !< the number of python sensors
          type(python_element) :: elements !< element quantities requested from Python code
        end type python_
! ----------------------------------------------------------------------------------------------------------------------

      contains
!! For performance reasons, this function must inlined, because it is called in a loop
!!      \brief return .TRUE. if the function id corresponds to a Python function
      !||====================================================================
      !||    python_funct_id   ../common_source/modules/python_mod.F90
      !||====================================================================
           integer function python_funct_id(nfunct, funct_id, npc) result(id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
!         use python_element_mod, only : element_is_python_funct_id
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nfunct
          integer, intent(in) :: funct_id !< the id of the function
          integer, intent(in) :: npc(3*nfunct+1) 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i 
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          i = 0
          id = 0
          if (funct_id> 0) i = npc(2*nfunct+funct_id+1)
          if(i < 0) id = -i
        end function python_funct_id 

      !! \brief copy a python function
      !||====================================================================
      !||    copy_python_function   ../common_source/modules/python_mod.F90
      !||--- called by ------------------------------------------------------
      !||    read_sensor_python     ../starter/source/tools/sensor/hm_read_sensor_python.F90
      !||    read_sensors           ../engine/source/output/restart/read_sensors.F
      !||====================================================================
      subroutine copy_python_function(src, dest)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_function), intent(in) :: src !< the source python function
          type(python_function), intent(out) :: dest !< the destination python function
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          ! Allocate and copy allocatable components
          allocate(dest%name(size(src%name)))
          allocate(dest%code(size(src%code)))
          dest%name = src%name
          dest%code = src%code
      
          ! Copy scalar components
          dest%len_name = src%len_name
          dest%len_code = src%len_code
          dest%num_lines = src%num_lines
          dest%num_args = src%num_args
          dest%num_return = src%num_return
          dest%user_id = src%user_id
      end subroutine copy_python_function



!! \brief serialize python_function into a buffer (for I/O)
      !||====================================================================
      !||    python_serialize    ../common_source/modules/python_mod.F90
      !||--- called by ------------------------------------------------------
      !||    ddsplit             ../starter/source/restart/ddsplit/ddsplit.F
      !||    python_funct_test   ../common_source/modules/python_mod.F90
      !||    wrrestp             ../engine/source/output/restart/wrrestp.F
      !||====================================================================
        subroutine python_serialize(python, buffer,buffer_size)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
!         use python_element_mod, only : element_serialize, element_get_size
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),                             intent(in) :: python!< the Fortran structure that holds the python functions
          integer, dimension(:),  allocatable,    intent(inout) :: buffer !< the buffer to serialize the python functions
          integer,                                intent(inout) :: buffer_size !< the size of the buffer
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,pos,len_name,len_code, ierr,elsize
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          buffer_size = 4 ! funct_offset, nb_functs, nb_sensors
! compute the size of the buffer
          do i = 1, python%nb_functs
            buffer_size = buffer_size + 6! 5 integers: len_name, len_code, num_lines, num_args, num_return
            buffer_size = buffer_size + python%functs(i)%len_name + python%functs(i)%len_code
          enddo
          elsize = 0
          !elsize = element_get_size(python%elements%global)
          buffer_size = buffer_size + elsize
! allocate the buffer

          if(allocated(buffer)) deallocate(buffer)
          allocate(buffer(buffer_size), stat = ierr)
          if(ierr /= 0) then
            write(6,*) "ERROR: python_serialize: allocation of buffer failed"
          else
            buffer(1) = buffer_size
            buffer(2) = python%funct_offset
            buffer(3) = python%nb_functs
            buffer(4) = python%nb_sensors
            pos = 5
            do i = 1,python%nb_functs
              buffer(pos)   = python%functs(i)%len_name
              buffer(pos+1) = python%functs(i)%len_code
              buffer(pos+2) = python%functs(i)%num_lines
              buffer(pos+3) = python%functs(i)%num_args
              buffer(pos+4) = python%functs(i)%num_return
              buffer(pos+5) = python%functs(i)%user_id
              pos = pos + 6
! transfer the name to the buffer, using the transfer function
              len_name=python%functs(i)%len_name
              len_code = python%functs(i)%len_code
              buffer(pos:pos+len_name-1) = transfer(python%functs(i)%name, buffer(pos:pos+len_name-1))
              pos = pos + python%functs(i)%len_name
              buffer(pos:pos+len_code-1) = transfer(python%functs(i)%code, buffer(pos:pos+len_code-1))
              pos = pos + python%functs(i)%len_code
            enddo
          endif
!         call element_serialize(python%elements%global,buffer(pos:pos+elsize-1),elsize)
        end subroutine python_serialize

!! \brief deserialize python_function (for I/O)
      !||====================================================================
      !||    python_deserialize   ../common_source/modules/python_mod.F90
      !||--- called by ------------------------------------------------------
      !||    python_funct_test    ../common_source/modules/python_mod.F90
      !||    rdresb               ../engine/source/output/restart/rdresb.F
      !||====================================================================
        subroutine python_deserialize(python, buffer)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
!         use python_element_mod, only : element_deserialize
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),                          intent(inout) :: python!< the Fortran structure that holds the python functions
          integer, dimension(:),  allocatable,    intent(inout) :: buffer !< the buffer to serialize the python functions
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,pos,ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          python_error = 0
          python%funct_offset = buffer(2)
          python%nb_functs = buffer(3)
          python%nb_sensors = buffer(4)
          allocate(python%functs(python%nb_functs),stat = ierr)
          if(ierr == 0 ) then
            pos = 5
            do i = 1,python%nb_functs
              python%functs(i)%len_name=buffer(pos)
              python%functs(i)%len_code = buffer(pos+1)
              python%functs(i)%num_lines = buffer(pos+2)
              python%functs(i)%num_args = buffer(pos+3)
              python%functs(i)%num_return = buffer(pos+4)
              python%functs(i)%user_id = buffer(pos+5)
              pos = pos + 6
              allocate(python%functs(i)%name(python%functs(i)%len_name),stat = ierr)
              if(ierr == 0) then
                python%functs(i)%name=transfer(buffer(pos:pos+python%functs(i)%len_name-1), python%functs(i)%name)
                pos = pos + python%functs(i)%len_name
              endif
              allocate(python%functs(i)%code(python%functs(i)%len_code),stat = ierr)
              if(ierr == 0) then
                python%functs(i)%code = transfer(buffer(pos:pos+python%functs(i)%len_code-1), python%functs(i)%code)
                pos = pos + python%functs(i)%len_code
              endif
            enddo
!           call element_deserialize(python%elements%global,buffer(pos:))
          endif

        end subroutine python_deserialize



!! \brief Initialize the python function
!! \details allocate funct%name and funct%code, and copy the name and code from the input file
      !||====================================================================
      !||    python_funct_init          ../common_source/modules/python_mod.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_funct_python       ../starter/source/tools/curve/hm_read_funct_python.F90
      !||    python_funct_test          ../common_source/modules/python_mod.F90
      !||    read_sensor_python         ../starter/source/tools/sensor/hm_read_sensor_python.F90
      !||--- calls      -----------------------------------------------------
      !||====================================================================
        subroutine python_funct_init(funct, code, len_code, num_lines)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_function),                     intent(out) :: funct !< the Fortran structure that holds the python function
          integer,                                    intent(in) :: len_code !< the length of the code
          integer,                                    intent(in) :: num_lines !< the number of lines of the code
          character(kind=c_char,len=len_code),        intent(in) :: code !< the code of the python function from the input file
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          character(len=max_line_length) :: name
          integer                        :: i
          integer                        :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          name=" "

          call python_register_function(name, code, num_lines)
          allocate(funct%name(len_trim(name)),stat = ierr)
          if(ierr == 0) then
            do i = 1, len_trim(name)
              funct%name(i) = name(i:i)
            end do
          endif
          allocate(funct%code(len_code))
          do i = 1, len_code
            funct%code(i) = code(i:i)
          end do
          funct%num_args = 1
          funct%num_return = 1
          funct%len_code = len_code
          funct%len_name=len_trim(name)
          funct%num_lines = num_lines
        end subroutine


!! \brief Evaluate the python function
!! \details the python function is called with one argument and one return value (double precision version)
      !||====================================================================
      !||    python_call_funct1d_dp   ../common_source/modules/python_mod.F90
      !||--- called by ------------------------------------------------------
      !||    python_solve             ../common_source/modules/python_mod.F90
      !||--- calls      -----------------------------------------------------
      !||====================================================================
        subroutine python_call_funct1D_dp(py, funct_id, x, y)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),                      intent(in) :: py !< the Fortran structure that holds the python function
          integer,                            intent(in) :: funct_id !< the id of the python function
          double precision,                   intent(in) :: x !< the input value
          double precision,                  intent(out) :: y !< the output value
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(1) :: argin, argout
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          argin(1) = x
!$OMP CRITICAL
          call python_call_function(py%functs(funct_id)%name, 1, argin, 1, argout)
!$OMP END CRITICAL
          y = argout(1)
        end subroutine

!! \brief Evaluate the python function
!! \details the python function is called with one argument and one return value (single precision version)
      !||====================================================================
      !||    python_call_funct1d_sp   ../common_source/modules/python_mod.F90
      !||--- calls      -----------------------------------------------------
      !||====================================================================
        subroutine python_call_funct1D_sp(py, funct_id, x, y)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),                      intent(in) :: py !< the Fortran structure that holds the python function
          integer,                            intent(in) :: funct_id !< the id of the python function
          real,                               intent(in) :: x !< the input value
          real,                               intent(out) :: y !< the output value
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(1) :: argin, argout
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          argin(1) = dble(x)
          ! the double precision function is called, then the result is converted to single
!$OMP CRITICAL
          call python_call_function(py%functs(funct_id)%name, 1, argin, 1, argout)
!$OMP END CRITICAL
          y = real(argout(1),kind(1.0))
        end subroutine

      !||====================================================================
      !||    python_set_active_node_values      ../common_source/modules/python_mod.F90
      !||--- called by ------------------------------------------------------
      !||    python_call_funct_cload_dp         ../engine/source/loads/general/python_call_funct_cload.F90
      !||    python_call_funct_cload_sp         ../engine/source/loads/general/python_call_funct_cload.F90
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine python_set_active_node_values(name_len, name, val)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
          use iso_c_binding
! --------------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                     intent(in) :: name_len !< the length of the name
          character(kind=c_char), dimension(name_len), intent(in) :: name      !< the name of the variable
          double precision, dimension(3),                intent(in) :: val !< the values
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          character(kind=c_char), dimension(name_len+1)        :: temp_name
          double precision :: valdb(3)
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          temp_name(1:name_len) = name
          temp_name(name_len+1:name_len+1) = c_null_char
          valdb(1:3) = val
          call python_update_active_node_values(name_len, temp_name, valdb)
        end subroutine







!! \brief Adaptive derivative of the python function (double precision version)
      !||====================================================================
      !||    python_deriv_funct1d_dp   ../common_source/modules/python_mod.F90
      !||--- called by ------------------------------------------------------
      !||    python_deriv_funct1d_sp   ../common_source/modules/python_mod.F90
      !||    python_solve              ../common_source/modules/python_mod.F90
      !||--- calls      -----------------------------------------------------
      !||====================================================================
        subroutine python_deriv_funct1D_dp(py, funct_id, x, y)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),                      intent(in) :: py !< the Fortran structure that holds the python function
          integer,                            intent(in) :: funct_id !< the id of the python function
          double precision,                   intent(in) :: x !< the input value
          double precision,                  intent(out) :: y !< the derivative at x
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(1) :: argin, argout
          double precision :: f1, f2
          double precision :: eps, h, prev_derivative, derivative
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          eps = epsilon(1.0D0)
          h = sqrt(eps) * max(1.0D0, abs(x))
          prev_derivative = 0.0D0
          derivative = 0.0D0
          i = 1

          do while (i <= 10 .and. h /= 0.0D0 .and. &
            (i == 1 .or. abs(derivative - prev_derivative) > eps * abs(derivative)))
            argin(1) = x + h
            call python_call_function(py%functs(funct_id)%name, 1, argin, 1, argout)
            f1 = argout(1)
            argin(1) = x - h
            call python_call_function(py%functs(funct_id)%name, 1, argin, 1, argout)
            f2 = argout(1)
            derivative = (f1 - f2) / (2.0D0 * h)
            prev_derivative = derivative
            h = h / 2.0D0
            i = i + 1
          end do

          y = derivative
        end subroutine

!! \brief adaptive derivative of the python function (single precision version)
      !||====================================================================
      !||    python_deriv_funct1d_sp   ../common_source/modules/python_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    python_deriv_funct1d_dp   ../common_source/modules/python_mod.F90
      !||====================================================================
        subroutine python_deriv_funct1D_sp(py, funct_id, x, y)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),                      intent(in) :: py !< the Fortran structure that holds the python function
          integer,                            intent(in) :: funct_id !< the id of the python function
          real,                   intent(in) :: x !< the input value
          real,                  intent(out) :: y !< the derivative at x
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision :: argin, argout
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          argin = dble(x)
          call python_deriv_funct1D_dp(py, funct_id, argin, argout)
          y = real(argout,kind(1.0))
        end subroutine

      !||====================================================================
      !||    python_solve              ../common_source/modules/python_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    python_call_funct1d_dp    ../common_source/modules/python_mod.F90
      !||    python_deriv_funct1d_dp   ../common_source/modules/python_mod.F90
      !||====================================================================
        subroutine python_solve(py, funct_id, root, rhs, tol_f, tol_x, max_iter)
          implicit none
# include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),               intent(in) :: py        !< The Fortran structure that holds the Python function
          integer,                     intent(in) :: funct_id  !< The ID of the Python function
          my_real, intent(inout) :: root                       !< Computed root
          my_real, intent(in) :: rhs                           !< Right-hand side of the equation f(x) = rhs
          my_real, intent(in), optional :: tol_f               !< Function value tolerance
          my_real, intent(in), optional :: tol_x               !< Solution tolerance
          integer, intent(in), optional :: max_iter            !< Maximum number of iterations
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: max_iter_val
          integer :: iter
          double precision :: x, fx, dfx, x_prev
          double precision :: tol_f_val, tol_x_val
          double precision, parameter :: epsilon = 1.0e-10  !< Small value to prevent NaN
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          ! Set default values for optional arguments
          tol_f_val = 1.0e-8
          tol_x_val = 1.0e-8
          max_iter_val = 10
          if (present(tol_f)) tol_f_val = tol_f
          if (present(tol_x)) tol_x_val = tol_x
          if (present(max_iter)) max_iter_val = max_iter
        
          ! Initialize x with the value of root
          x = root
          iter = 0
        
          do while (iter < max_iter_val)
            iter = iter + 1
!$OMP CRITICAL
            ! Evaluate the function value f(x)
             call python_call_funct1D_dp(py, funct_id, x, fx)
            ! Subtract the right-hand side to compute f(x) - rhs
            fx = fx - rhs
            ! Evaluate the derivative df(x)/dx
            call python_deriv_funct1D_dp(py, funct_id, x, dfx)
!$OMP END CRITICAL
        
            ! Check if the function value is sufficiently close to the target
            if (abs(fx) < tol_f_val) then
              root = x
              return
            end if
        
            ! Ensure the derivative is not too small
            if (abs(dfx) < epsilon) then
              return
            end if
        
            ! Perform the Newton's step
            x_prev = x
            if(abs(dfx) > epsilon) then
              x = x - fx / dfx
            else
              x = x - fx / epsilon
            endif
        
            ! Check if the solution converged
            if (abs(x - x_prev) < tol_x_val) then
              root = x
              return
            end if
          end do
        
          ! If the loop exits without convergence, return the last value of x
          root = x
          return
        end subroutine python_solve
  


!! \brief update variables known by python functions
      !||====================================================================
      !||    python_update_nodal_entity     ../common_source/modules/python_mod.F90
      !||--- called by ------------------------------------------------------
      !||    python_update_nodal_entities   ../common_source/modules/python_mod.F90
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine python_update_nodal_entity(numnod, name, name_len, val)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
          use iso_c_binding
! --------------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                     intent(in) :: numnod !< the number of nodes
          integer,                                     intent(in) :: name_len !< the length of the name
          character(kind=c_char), dimension(name_len), intent(in) :: name      !< the name of the variable
          my_real, dimension(3,numnod),                intent(in) :: val !< the values
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          character(kind=c_char), dimension(name_len+1)        :: temp_name
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          temp_name(1:name_len) = name
          temp_name(name_len+1:name_len+1) = c_null_char
          call python_set_node_values(numnod, name_len, temp_name, val)
        end subroutine

!! \brief update variables known by python functions
      !||====================================================================
      !||    python_update_nodal_entities   ../common_source/modules/python_mod.F90
      !||--- called by ------------------------------------------------------
      !||    resol                          ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    python_update_nodal_entity     ../common_source/modules/python_mod.F90
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine python_update_nodal_entities(numnod,X, A, D, DR, V, VR, AR)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
          use iso_c_binding, only : c_double
! --------------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                 intent(in) :: numnod !< the number of nodes
          my_real, optional,  dimension(3,numnod), intent(in) :: X !< the coordinates
          my_real, optional,  dimension(3,numnod), intent(in) :: A !< the acceleration
          my_real, optional,  dimension(3,numnod), intent(in) :: D !< the displacement
          my_real, optional,  dimension(3,numnod), intent(in) :: DR !< the rotational? relative? displacement
          my_real, optional,  dimension(3,numnod), intent(in) :: V !< the velocity
          my_real, optional,  dimension(3,numnod), intent(in) :: VR !< the rotational? relative? velocity
          my_real, optional,  dimension(3,numnod), intent(in) :: AR !< the acceleration
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          if(present(X))  call python_update_nodal_entity(numnod,"C",1, X)
          if(present(A))  call python_update_nodal_entity(numnod,"A",1, A)
          if(present(D))  call python_update_nodal_entity(numnod,"D",1, D)
          if(present(DR)) call python_update_nodal_entity(numnod,"DR",2, DR)
          if(present(V))  call python_update_nodal_entity(numnod,"V",1, V)
          if(present(VR)) call python_update_nodal_entity(numnod,"VR",2, VR)
          if(present(AR)) call python_update_nodal_entity(numnod,"AR",2, AR)

        end subroutine

        ! unit test
      !||====================================================================
      !||    python_funct_test      ../common_source/modules/python_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    python_deserialize     ../common_source/modules/python_mod.F90
      !||    python_funct_init      ../common_source/modules/python_mod.F90
      !||    python_serialize       ../common_source/modules/python_mod.F90
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine python_funct_test()
          use iso_c_binding , only: c_null_char,c_char
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision :: argin(2), argout(1)
          character(kind=c_char,len=max_code_length) :: code
          character(len=max_line_length) :: name
          integer :: buffer_size,ok
          integer, dimension(:), allocatable :: buffer
          integer :: ierr
          type(python_) :: py,py2
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
! Initialize python
          ok = 0
          call python_initialize(ok)
! write a code of the function multiply(x,y)
          code = " "
          code = "def test_function(x,y):" // c_null_char // "    return x*y"// c_null_char // "    "//c_null_char
! Register a function
          name=" "
          allocate(py%functs(1),stat = ierr)
          if( ierr /= 0) stop
          py%nb_functs = 1
          py%funct_offset = 0
          call python_funct_init(py%functs(1), code, len_trim(code),3)
          call python_serialize(py, buffer, buffer_size) ! write into buffer
          call python_deserialize(py2, buffer) ! read from buffer

! Call the function
          argin(1) = 2.0
          argin(2) = 3.0
          call python_call_function(py2%functs(1)%name, 2, argin, 1, argout)
! Check the result
          if (argout(1) /= 6.0) then
            !                write(*,*) "Error: python_funct_test failed"
            !top
          end if
! Finalize python
          call python_finalize()
        end subroutine python_funct_test

! python_init
      end module python_funct_mod
