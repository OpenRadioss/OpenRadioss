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
      !||    python_element_mod    ../common_source/modules/python_element_mod.F90
      !||--- called by ------------------------------------------------------
      !||    python_element_init   ../engine/source/mpi/python_spmd_mod.F90
      !||    python_element_sync   ../engine/source/mpi/python_spmd_mod.F90
      !||    python_funct_mod      ../common_source/modules/python_mod.F90
      !||    python_register       ../engine/source/tools/curve/python_register.F90
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      module python_element_mod
        use iso_c_binding
        integer, parameter :: NAME_LEN = 100
! ----------------------------------------------------------------------------------------------------------------------
!                                               Interface
! ----------------------------------------------------------------------------------------------------------------------
        interface
          subroutine python_update_elemental_entity(name,val,uid) bind(c,name="cpp_python_update_elemental_entity")
            use iso_c_binding
            integer(kind=c_int), value, intent(in) :: uid
            real(kind=c_double), value, intent(in) :: val
            character(kind=c_char), dimension(100), intent(in) :: name
          end subroutine
          subroutine python_get_number_elemental_entities(nb) bind(c,name="cpp_python_get_number_elemental_entities")
            use iso_c_binding, only : c_int
            integer(kind=c_int), intent(inout) :: nb
          end subroutine
!  void cpp_python_get_elemental_entity(int nb,  char *name, int *uid)
          subroutine python_get_elemental_entity(nb,name,uid) bind(c,name="cpp_python_get_elemental_entity")
            use iso_c_binding
            integer(kind=c_int), value, intent(in) :: nb
            integer(kind=c_int), intent(inout) :: uid !< returns the user id of the nth variable found in the python code
            character(kind=c_char), dimension(100), intent(inout) :: name !< variable name, as defined in H3D keyword
          end subroutine
        end interface

! ----------------------------------------------------------------------------------------------------------------------
!                                               Type definitions
! ----------------------------------------------------------------------------------------------------------------------
!! \brief Type to store python elementary values passed to the python interface
        type :: python_element_keyword
          character(len=NAME_LEN) :: h3d !< h3d keyword
          character(len=NAME_LEN) :: name !< python name (should be the same as h3d_keyword when possible)
        end type python_element_keyword
!! \brief Type to store python elementary values passed to the python interface (local to processor)
        type :: python_element_local
          integer :: n !< Number of values
          integer, dimension(:), allocatable :: user_ids !< User ids of the elements
          integer, dimension(:), allocatable :: group_id !< Group id of the elements
          integer, dimension(:), allocatable :: local_id !< id within the group
          integer, dimension(:), allocatable :: global_id !< global id of the elements
          type(python_element_keyword), dimension(:), allocatable :: keyword !< Keywords of the elements
          double precision, dimension(:), allocatable :: values !< scalar values of the elements
        end type python_element_local
!! \brief Type to store python elementary values passed to the python interface (global after MPI communication)
        type :: python_element_global
          integer :: n !< Number of values
          type(python_element_keyword), dimension(:), allocatable :: keyword !< Keywords of the elements
          double precision, dimension(:), allocatable :: values !< scalar values of the elements
          integer, dimension(:), allocatable :: user_ids !< User ids of the elements
          integer, dimension(:), allocatable :: processor !< Processor id of the elements
        end type python_element_global
!! \brief Type to store python elementary values passed to the python interface
        type :: python_element
          type(python_element_local) :: local !< Local values of the elements
          type(python_element_global) :: global !< Global values of the elements
        end type python_element

      contains
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Subroutines
! ----------------------------------------------------------------------------------------------------------------------
!! \brief get the size to serialize the python elemental variables found in the python function
      !||====================================================================
      !||    element_get_size   ../common_source/modules/python_element_mod.F90
      !||====================================================================
        integer function element_get_size(element) result(length)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_element_global),              intent(in) :: element!<  python element data to serialize
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          length = 1 ! n
          length = length + element%n * NAME_LEN ! keyword%h3d
          length = length + element%n * NAME_LEN ! keyword%name in python code
          length = length + element%n ! user ids
          return
        end function element_get_size
!! \brief serialize the python elemental variables found in the python function
      !||====================================================================
      !||    element_serialize   ../common_source/modules/python_element_mod.F90
      !||--- calls      -----------------------------------------------------
      !||====================================================================
        subroutine element_serialize(element, buffer,buffer_size)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_element_global),               intent(in) :: element!< the Fortran structure that holds the python functions
          integer,                                   intent(in) :: buffer_size !< the size of the buffer
          integer, dimension(buffer_size),        intent(inout) :: buffer !< the buffer to serialize the python functions
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: pos
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          pos = 1
          buffer(1) = element%n
          call flush(6)
          do i = 1, element%n
            buffer(pos:pos+NAME_LEN-1) = transfer(element%keyword(i)%h3d, buffer(pos:pos+NAME_LEN-1))
            pos = pos + NAME_LEN
            buffer(pos:pos+NAME_LEN-1) = transfer(element%keyword(i)%name, buffer(pos:pos+NAME_LEN-1))
            pos = pos + NAME_LEN
            buffer(pos:pos+1) = element%user_ids(i)
            pos = pos + 1
          enddo
        end subroutine element_serialize

!! \brief deserialize the python elemental variables found in the python function
      !||====================================================================
      !||    element_deserialize   ../common_source/modules/python_element_mod.F90
      !||====================================================================
        subroutine element_deserialize(element, buffer)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_element_global),            intent(inout) :: element!< the Fortran structure that holds the python functions
          integer, dimension(*),           intent(in) :: buffer !< the buffer to serialize the python functions
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: pos
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          element%n = buffer(1)
          pos = 1
          ! allocate the arrays
          if(allocated(element%keyword)) deallocate(element%keyword)
          if(allocated(element%user_ids)) deallocate(element%user_ids)
          allocate(element%keyword(element%n))
          allocate(element%user_ids(element%n))
          do i = 1, element%n
            element%keyword(i)%h3d = transfer(buffer(pos:pos+NAME_LEN-1), element%keyword(i)%h3d)
            pos = pos + NAME_LEN
            element%keyword(i)%name = transfer(buffer(pos:pos+NAME_LEN-1), element%keyword(i)%name)
            pos = pos + NAME_LEN
            element%user_ids(i) = buffer(pos)
            pos = pos + 1
          enddo

          ! allocate the other arrays
          if(allocated(element%values)) deallocate(element%values)
          allocate(element%values(element%n))
          element%values = 0.0d0
          if(allocated(element%processor)) deallocate(element%processor)
          allocate(element%processor(element%n))
          element%processor = 0
        end subroutine element_deserialize



      end module python_element_mod
