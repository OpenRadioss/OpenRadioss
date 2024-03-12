!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
!hd|====================================================================
!hd|  PYTHON_FUNCT_MOD              modules/python_mod.F
!hd|-- called by -----------
!hd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
!hd|        EXECARGCHECK                  starter/source/starter/execargcheck.F
!hd|        LECTUR                        starter/source/starter/lectur.F
!hd|        DAASOLV                       engine/source/fluid/daasolv.F
!hd|        DAASOLVP                      engine/source/fluid/daasolvp.F
!hd|        FIXVEL                        engine/source/constraints/general/impvel/fixvel.F
!hd|        FLOW0                         engine/source/fluid/flow0.F
!hd|        FORCE                         engine/source/loads/general/force.F
!hd|        GRAVIT                        engine/source/loads/general/grav/gravit.F
!hd|        GRAVIT_FVM_FEM                engine/source/loads/general/grav/gravit_fvm_fem.F
!hd|        INCPFLOW                      engine/source/fluid/incpflow.F
!hd|        LAG_FXV                       engine/source/tools/lagmul/lag_fxv.F
!hd|        LAG_FXVP                      engine/source/tools/lagmul/lag_fxv.F
!hd|        LAG_MULT                      engine/source/tools/lagmul/lag_mult.F
!hd|        LAG_MULTP                     engine/source/tools/lagmul/lag_mult.F
!hd|        RADIOSS2                      engine/source/engine/radioss2.F
!hd|        RDRESB                        engine/source/output/restart/rdresb.F
!hd|        RESOL                         engine/source/engine/resol.F
!hd|        RESOL_HEAD                    engine/source/engine/resol_head.F
!hd|        WRRESTP                       engine/source/output/restart/wrrestp.F
!hd|-- calls ---------------
!hd|====================================================================
      module python_funct_mod
        use iso_c_binding
        integer, parameter :: max_line_length = 500 !< the maximum length of a line of code of python function
        integer, parameter :: max_num_lines = 1000 !< the maximum number of lines of python function
        integer, parameter :: max_code_length = max_line_length*max_num_lines
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
          ! a subroutine that check if the function works, and return an nonzero error code if it does not
          subroutine python_check_function(name, error) bind(c, name="cpp_python_check_function")
            use iso_c_binding
            character(kind=c_char), dimension(*) :: name
            integer(kind=c_int), intent(out) :: error
          end subroutine python_check_function
        end interface

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

!! \brief the python structure: it contains the python functions
        type python_
          type(python_function), dimension(:), allocatable :: functs !< the python functions
          integer :: funct_offset !< the local id of the python function starts after the id of other kind of functions
          integer :: nb_functs !< the number of python functions
        end type python_


        interface python_call_funct1D
          module procedure python_call_funct1D_sp
          module procedure python_call_funct1D_dp
        end interface python_call_funct1D

        interface python_deriv_funct1D
          module procedure python_deriv_funct1D_sp
          module procedure python_deriv_funct1D_dp
        end interface python_deriv_funct1D


      contains

!! \brief serialize python_function into a buffer (for I/O)
        subroutine python_serialize(python, buffer,buffer_size)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
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
          integer :: i,pos,len_name,len_code, ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          buffer_size = 3 ! funct_offset, nb_functs
! compute the size of the buffer
          do i = 1, python%nb_functs
            buffer_size = buffer_size + 6! 5 integers: len_name, len_code, num_lines, num_args, num_return
            buffer_size = buffer_size + python%functs(i)%len_name + python%functs(i)%len_code
          enddo
! allocate the buffer
          if(allocated(buffer)) deallocate(buffer)
          allocate(buffer(buffer_size), stat = ierr)
          if(ierr /= 0) then
            write(6,*) "ERROR: python_serialize: allocation of buffer failed"
          else
            buffer(1) = buffer_size
            buffer(2) = python%funct_offset
            buffer(3) = python%nb_functs
            pos = 4
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
        end subroutine python_serialize

!! \brief deserialize python_function (for I/O)
        subroutine python_deserialize(python, buffer)
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
          allocate(python%functs(python%nb_functs),stat = ierr)
          if(ierr == 0 ) then
            pos = 4
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
          endif

        end subroutine python_deserialize

!! \brief Initialize the python function
!! \details allocate funct%name and funct%code, and copy the name and code from the input file
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

!! \brief Register the python functions saved in the python structure into the python interpreter dictionary
        subroutine python_register(py)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),                     intent(in) :: py !< the Fortran structure that holds the python function
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          character(len=max_line_length) :: name
!         character(kind=c_char,len=max_code_length) :: code
          character(kind=c_char, len=:), allocatable :: code
          integer                        :: i,n,ierror
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(character(kind=c_char, len=max_code_length) :: code)

          ierror = 0 ! if python error = 1 => python_initialize will do nothing, because python is not avaiable
            ! i.e. starter without -python option
          if(py%nb_functs>0) call python_initialize(ierror)

          if(py%nb_functs > 0 .and. ierror == 1) then
            ! stops the program if python_initialize failed and there are python functions
            write(6,*) "ERROR: python_register: python_initialize failed"
            !stop
          endif
          do n = 1, py%nb_functs
            do i = 1, py%functs(n)%len_code
              code(i:i) = py%functs(n)%code(i)
            end do
            code(py%functs(n)%len_code+1:py%functs(n)%len_code+1) = c_null_char
            call python_register_function(name, code, py%functs(n)%num_lines)
          end do

          deallocate(code)
        end subroutine

!! \brief Evaluate the python function
!! \details the python function is called with one argument and one return value  (double precision version)
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
          call python_call_function(py%functs(funct_id)%name, 1, argin, 1, argout)
          y = argout(1)
        end subroutine

!! \brief Evaluate the python function
!! \details the python function is called with one argument and one return value  (single precision version)
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
          call python_call_function(py%functs(funct_id)%name, 1, argin, 1, argout)
          y = real(argout(1),kind(1.0))
        end subroutine

!! \brief Adaptive derivative of the python function (double precision version)
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


        ! unit test
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
          !            write(6,*) "Fortran name=", py%functs(1)%name
          !            write(6,*) "Fortran code = ", py%functs(1)%code(1:len_trim(code))
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
