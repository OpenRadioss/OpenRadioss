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
      !||    python_spmd_mod                ../engine/source/mpi/python_spmd_mod.F90
      !||--- called by ------------------------------------------------------
      !||    funct_python_update_elements   ../engine/source/tools/curve/funct_python_update_elements.F90
      !||    python_register                ../engine/source/tools/curve/python_register.F90
      !||====================================================================
      module python_spmd_mod
      contains
!! \brief initialize the python elemental variables found in the python function
      !||====================================================================
      !||    python_element_init   ../engine/source/mpi/python_spmd_mod.F90
      !||--- called by ------------------------------------------------------
      !||    python_register       ../engine/source/tools/curve/python_register.F90
      !||--- calls      -----------------------------------------------------
      !||    c_delete_hash         ../common_source/tools/container/c_hash_table.cpp
      !||    c_hash_find           ../common_source/tools/container/c_hash_table.cpp
      !||    c_hash_insert         ../common_source/tools/container/c_hash_table.cpp
      !||    c_new_hash            ../common_source/tools/container/c_hash_table.cpp
      !||    spmd_comm_rank        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_comm_size        ../engine/source/mpi/spmd_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    python_element_mod    ../common_source/modules/python_element_mod.F90
      !||    spmd_mod              ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine python_element_init(element, n, group_id, local_id, user_ids)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
          use python_element_mod, only: python_element, NAME_LEN
          use spmd_mod, only : spmd_comm_size, spmd_comm_rank, spmd_allreduce, SPMD_MAX
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_element),                   intent(inout) :: element!< the Fortran structure that holds the python functions
          integer,                                intent(in) :: n !< total number of elements
          integer, dimension(n),                  intent(in) :: group_id !< group id of the elements
          integer, dimension(n),                  intent(in) :: user_ids !< user ids of the elements
          integer, dimension(n),                  intent(in) :: local_id !< id within the group
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: local_n
          integer :: i
          integer :: j
          integer :: k
          integer :: map_id
          integer :: rank
          integer :: spmd_size
          integer, dimension(:), allocatable :: processor
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          ! Find the local elements requested by python functions
          call c_new_hash(map_id, n)
          do i = 1, n
            call c_hash_insert(map_id, user_ids(i), i)
          enddo
          local_n = 0
          do i = 1, element%global%n
            j = -1
            call c_hash_find(map_id, element%global%user_ids(i), j)
            if(j > 0) then
              local_n = local_n + 1
            endif
          enddo
          element%local%n = local_n
          if(allocated(element%local%user_ids)) deallocate(element%local%user_ids)
          if(allocated(element%local%group_id)) deallocate(element%local%group_id)
          if(allocated(element%local%local_id)) deallocate(element%local%local_id)
          if(allocated(element%local%values)) deallocate(element%local%values)
          if(allocated(element%local%keyword)) deallocate(element%local%keyword)
          if(allocated(element%local%global_id)) deallocate(element%local%global_id)
          allocate(element%local%user_ids(local_n))
          allocate(element%local%group_id(local_n))
          allocate(element%local%local_id(local_n))
          allocate(element%local%values(local_n))
          allocate(element%local%keyword(local_n))
          allocate(element%local%global_id(local_n))
          allocate(processor(element%global%n))
          processor(:) = -1
          local_n = 0
          call spmd_comm_size(spmd_size)
          call spmd_comm_rank(rank)
          element%global%processor = -1
          do i = 1, element%global%n
            j = -1
            call c_hash_find(map_id, element%global%user_ids(i), j)
            if(j > 0) then
              local_n = local_n + 1
              element%local%user_ids(local_n) = element%global%user_ids(i)
              element%local%group_id(local_n) = group_id(j)
              element%local%local_id(local_n) = local_id(j)
              element%local%values(local_n) = element%global%values(i)
              do k = 1, NAME_LEN
                element%local%keyword(local_n)%h3d(k:k) = element%global%keyword(i)%h3d(k:k)
                element%local%keyword(local_n)%name(k:k) = element%global%keyword(i)%name(k:k)
                element%local%global_id(local_n) = i
              enddo
              processor(i) = rank
            endif
          enddo
          call c_delete_hash(map_id)

          call spmd_allreduce(processor, element%global%processor, element%global%n, SPMD_MAX)
          deallocate(processor)

!         if(rank == 0) then
!           write(6,*) "Python element initialization"
!           do i = 1, element%global%n
!             write(6,*) "Element ", element%global%user_ids(i), " is on processor ", element%global%processor(i)
!             write(6,*) "Keywords:", element%global%keyword(i)%h3d, element%global%keyword(i)%name
!           enddo
!         endif

!          write(6,*) "Processor ", rank, " has ", element%local%n, " elements"
!          do i = 1, element%global%n
!            if(element%global%processor(i) == rank) then
!              write(6,*) "Element ", element%global%user_ids(i), " is on processor ", rank
!              write(6,*) "Keywords:", element%global%keyword(i)%h3d, element%global%keyword(i)%name
!            endif
!          enddo
!         call flush(6)
        end subroutine python_element_init
!! \brief synchronize python elemental variables found in the python function
      !||====================================================================
      !||    python_element_sync              ../engine/source/mpi/python_spmd_mod.F90
      !||--- called by ------------------------------------------------------
      !||    funct_python_update_elements     ../engine/source/tools/curve/funct_python_update_elements.F90
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    python_element_mod               ../common_source/modules/python_element_mod.F90
      !||    spmd_mod                         ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine python_element_sync(element)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
          use python_element_mod !, only: python_element, python_update_elemental_entity
          use spmd_mod, only : spmd_allreduce, SPMD_MAX
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_element),                   intent(inout) :: element !< the Fortran structure that holds the python functions
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          double precision, dimension(:), allocatable :: values
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(values(element%global%n))
          values = -HUGE(0.0d0)
          do i = 1, element%local%n
            values(element%local%global_id(i)) = element%local%values(i)
          enddo
          call spmd_allreduce(values, element%global%values, element%global%n, SPMD_MAX)
          deallocate(values)
          do i = 1, element%global%n
            call python_update_elemental_entity(element%global%keyword(i)%h3d,element%global%values(i), element%global%user_ids(i))
          enddo
        end subroutine python_element_sync

      end module python_spmd_mod
