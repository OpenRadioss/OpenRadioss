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
      module coupling_adapter_mod
        use iso_c_binding
        implicit none

        ! Data type constants
        integer, parameter :: coupling_displacements = 1
        integer, parameter :: coupling_forces = 2
        integer, parameter :: coupling_positions = 3

        ! Operation modes
        integer, parameter :: coupling_replace = 1
        integer, parameter :: coupling_add = 2

        integer, parameter :: COUPLING_PRECICE = 1
        integer, parameter :: COUPLING_CWIPI = 2

        ! Generic coupling adapter type
        type :: coupling_type
          type(c_ptr) :: adapter_ptr = c_null_ptr
          logical :: active = .false.
          integer :: nb_coupling_nodes = 0
          double precision :: dt_limit = 0.0d0
          integer :: grnod_id = 0
          integer :: coupler
        end type coupling_type

        ! C interface declarations
        interface
          function coupling_adapter_create() bind(c, name='coupling_adapter_create')
            use iso_c_binding
            type(c_ptr) :: coupling_adapter_create
          end function coupling_adapter_create
          subroutine coupling_adapter_destroy(adapter) bind(c, name='coupling_adapter_destroy')
            use iso_c_binding
            type(c_ptr), value :: adapter
          end subroutine coupling_adapter_destroy
          function coupling_adapter_configure(adapter, filename) bind(c, name='coupling_adapter_configure')
            use iso_c_binding
            type(c_ptr), value :: adapter
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int) :: coupling_adapter_configure
          end function coupling_adapter_configure
          subroutine coupling_adapter_set_nodes(adapter, node_ids, num_nodes) bind(c, name='coupling_adapter_set_nodes')
            use iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int), intent(in) :: node_ids(*)
            integer(c_int), value :: num_nodes
          end subroutine coupling_adapter_set_nodes
          function coupling_adapter_initialize(adapter, coordinates, total_nodes, mpi_rank, mpi_size) &
            bind(c, name='coupling_adapter_initialize')
            use iso_c_binding
            type(c_ptr), value :: adapter
            real(c_double), intent(in) :: coordinates(*)
            integer(c_int), value :: total_nodes, mpi_rank, mpi_size
            integer(c_int) :: coupling_adapter_initialize
          end function coupling_adapter_initialize
          subroutine coupling_adapter_write_data(adapter, values, total_nodes, dt, data_type) &
            bind(c, name='coupling_adapter_write_data')
            use iso_c_binding
            type(c_ptr), value :: adapter
            real(c_double), intent(in) :: values(*)
            integer(c_int), value :: total_nodes, data_type
            real(c_double), value :: dt
          end subroutine coupling_adapter_write_data
          subroutine coupling_adapter_read_data(adapter, values, total_nodes, dt, data_type, mode) &
            bind(c, name='coupling_adapter_read_data')
            use iso_c_binding
            type(c_ptr), value :: adapter
            real(c_double), intent(inout) :: values(*)
            integer(c_int), value :: total_nodes, data_type, mode
            real(c_double), value :: dt
          end subroutine coupling_adapter_read_data

          subroutine coupling_adapter_advance(adapter, dt) bind(c, name='coupling_adapter_advance')
            use iso_c_binding
            type(c_ptr), value :: adapter
            real(c_double), intent(inout) :: dt
          end subroutine coupling_adapter_advance
          function coupling_adapter_is_coupling_ongoing(adapter) bind(c, name='coupling_adapter_is_coupling_ongoing')
            use iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_is_coupling_ongoing
          end function coupling_adapter_is_coupling_ongoing
          function coupling_adapter_requires_writing_checkpoint(adapter) &
            bind(c, name='coupling_adapter_requires_writing_checkpoint')
            use iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_requires_writing_checkpoint
          end function coupling_adapter_requires_writing_checkpoint
          function coupling_adapter_requires_reading_checkpoint(adapter) &
            bind(c, name='coupling_adapter_requires_reading_checkpoint')
            use iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_requires_reading_checkpoint
          end function coupling_adapter_requires_reading_checkpoint
          subroutine coupling_adapter_finalize(adapter) bind(c, name='coupling_adapter_finalize')
            use iso_c_binding
            type(c_ptr), value :: adapter
          end subroutine coupling_adapter_finalize
          function coupling_adapter_is_active(adapter) bind(c, name='coupling_adapter_is_active')
            use iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_is_active
          end function coupling_adapter_is_active
          function coupling_adapter_get_max_time_step_size(adapter) &
            bind(c, name='coupling_adapter_get_max_time_step_size')
            use iso_c_binding
            type(c_ptr), value :: adapter
            real(c_double) :: coupling_adapter_get_max_time_step_size
          end function coupling_adapter_get_max_time_step_size

          function coupling_adapter_get_num_coupling_nodes(adapter) &
            bind(c, name='coupling_adapter_get_num_coupling_nodes')
            use iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_get_num_coupling_nodes
          end function coupling_adapter_get_num_coupling_nodes
          !    int coupling_adapter_get_group_node_id(void* adapter);
          function coupling_adapter_get_group_node_id(adapter) &
            bind(c, name='coupling_adapter_get_group_node_id')
            use iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_get_group_node_id
          end function coupling_adapter_get_group_node_id

        end interface

      contains

        ! Initialize coupling adapter
        subroutine coupling_create(coupling)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Body
!-----------------------------------------------------------------------------------------------------------------------
          coupling%adapter_ptr = coupling_adapter_create()
          if (c_associated(coupling%adapter_ptr)) then
            coupling%active = .true.
          else
            coupling%active = .false.
          end if
        end subroutine coupling_create

        ! Read configuration file *.cpl
        subroutine coupling_configure(coupling, input_filename)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling
          character(*), intent(in) :: input_filename
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
!-----------------------------------------------------------------------------------------------------------------------
          character(kind=c_char) :: c_filename(len_trim(input_filename) + 1)
          integer :: i, result
          call coupling_create(coupling)
          if (.not. c_associated(coupling%adapter_ptr)) return

          ! Convert Fortran string to C string
          do i = 1, len_trim(input_filename)
            c_filename(i) = input_filename(i:i)
          end do
          c_filename(len_trim(input_filename) + 1) = c_null_char

          result = coupling_adapter_configure(coupling%adapter_ptr, c_filename)
          coupling%active = (result == 1)
        end subroutine coupling_configure

        ! Set coupling nodes
        subroutine coupling_set_nodes(coupling, igrnod, ngrnod)
          use GROUPDEF_MOD
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling
          integer, intent(in) :: ngrnod
          type(GROUP_), intent(in) :: igrnod(ngrnod)
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
!-----------------------------------------------------------------------------------------------------------------------
          integer :: i, j
!------------------------------------------------------------------------------------------------------------------------
!                                                   Body
!------------------------------------------------------------------------------------------------------------------------
          if (.not. c_associated(coupling%adapter_ptr)) return
          coupling%grnod_id = coupling_adapter_get_group_node_id(coupling%adapter_ptr)
          ! Find the group with matching ID
          j = 0
          do i = 1, ngrnod
            if (igrnod(i)%id == coupling%grnod_id) then
              j = i
              exit
            end if
          end do

          if (j == 0) then
            write(6,*) "ERROR: coupling_set_nodes:",coupling%grnod_id, " not found in igrnod"
            return
          end if

          coupling%nb_coupling_nodes = igrnod(j)%nentity

          call coupling_adapter_set_nodes(coupling%adapter_ptr, igrnod(j)%entity, coupling%nb_coupling_nodes)
        end subroutine coupling_set_nodes

        ! Initialize coupling
        subroutine coupling_initialize(coupling, X, nb_nodes, mpi_rank, mpi_commsize)
          use precision_mod, only: WP
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling
          integer, intent(in) :: nb_nodes, mpi_rank, mpi_commsize
          real(kind=WP), intent(in) :: X(3, nb_nodes)
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
!-----------------------------------------------------------------------------------------------------------------------
          integer :: result
          real(c_double) :: coordinates(3 * nb_nodes)
          integer :: i, j, k
!------------------------------------------------------------------------------------------------------------------------
!                                                   Body
!------------------------------------------------------------------------------------------------------------------------
          if (.not. c_associated(coupling%adapter_ptr)) return

          ! Convert coordinates to flat array
          k = 1
          do i = 1, nb_nodes
            do j = 1, 3
              coordinates(k) = real(X(j, i), c_double)
              k = k + 1
            end do
          end do

          result = coupling_adapter_initialize(coupling%adapter_ptr, coordinates, nb_nodes, mpi_rank, mpi_commsize)

          if (result == 1) then
            coupling%dt_limit = coupling_adapter_get_max_time_step_size(coupling%adapter_ptr)
            coupling%nb_coupling_nodes = coupling_adapter_get_num_coupling_nodes(coupling%adapter_ptr)
          else
            coupling%active = .false.
          end if
        end subroutine coupling_initialize

        ! Write data to coupling library
        subroutine coupling_write(coupling, dt, global_values, nb_nodes, name_id)
          use precision_mod, only: WP
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling
          integer, intent(in) :: nb_nodes, name_id
          real(kind=WP), intent(in) :: global_values(3, nb_nodes), dt
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
!-----------------------------------------------------------------------------------------------------------------------
          integer :: i, j, k
          real(c_double), dimension(:,:), allocatable :: values
!------------------------------------------------------------------------------------------------------------------------
!                                                   Body
!------------------------------------------------------------------------------------------------------------------------
          if (.not. c_associated(coupling%adapter_ptr)) return
          if (.not. coupling%active) return
#ifdef MYREAL8
          call coupling_adapter_write_data(coupling%adapter_ptr, global_values, nb_nodes, real(dt, c_double), name_id)
#else
          ! single precision, copy global_values into values
          allocate(values(3 , nb_nodes))
          values(1:3,1:nb_nodes) = real(global_values(1:3,1:nb_nodes), c_double)
          call coupling_adapter_write_data(coupling%  adapter_ptr, values, nb_nodes, real(dt, c_double), name_id)
          deallocate(values)
#endif
        end subroutine coupling_write

        ! Read data from coupling library
        subroutine coupling_read(coupling, dt, global_values, nb_nodes, mode, name_id)
          use precision_mod, only: WP
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling
          integer, intent(in) :: nb_nodes, mode, name_id
          real(kind=WP), intent(inout) :: global_values(3, nb_nodes)
          double precision, intent(in) :: dt
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
!-----------------------------------------------------------------------------------------------------------------------
          integer :: i, j, k
          real(c_double), dimension(:,:), allocatable :: values
!------------------------------------------------------------------------------------------------------------------------
!                                                   Body
!------------------------------------------------------------------------------------------------------------------------
          if (.not. c_associated(coupling%adapter_ptr)) return
          if (.not. coupling%active) return
#ifdef MYREAL8
          call coupling_adapter_read_data(coupling%adapter_ptr, global_values, nb_nodes, real(dt, c_double), name_id, mode)
#else
         ! single precision, copy global_values into values
          allocate(values(3 , nb_nodes))
          values(1:3,1:nb_nodes) = real(global_values(1:3,1:nb_nodes), c_double)
          call coupling_adapter_read_data(coupling%adapter_ptr, values, nb_nodes, real(dt, c_double), name_id, mode)
          ! Copy values back to global_values
          global_values(:,:) = real(values(:,:), WP)
          deallocate(values)
#endif

        end subroutine coupling_read


!! \brief main subroutine to create syncrhonization points from resol.F. It does both reading and writing
        subroutine coupling_sync(coupling, dt, nodes, name_id)
          use precision_mod, only: WP
          use nodal_arrays_mod, only : nodal_arrays_
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling
          integer, intent(in) ::  name_id !< the name of the data to synchronize
          real(kind=WP), intent(inout) :: dt
          type(nodal_arrays_), intent(inout) :: nodes
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
!-----------------------------------------------------------------------------------------------------------------------
          integer :: numnod
          real(c_double), dimension(:,:), allocatable :: values
!------------------------------------------------------------------------------------------------------------------------
!                                                   Body
!------------------------------------------------------------------------------------------------------------------------
          if (.not. c_associated(coupling%adapter_ptr)) return
          if (.not. coupling%active) return
          numnod = nodes%numnod
#ifdef MYREAL8
          if(name_id == coupling_displacements) then
            call coupling_adapter_write_data(coupling%adapter_ptr, nodes%D, numnod, &
              real(dt, c_double), coupling_displacements)
            ! Read positions
            call coupling_adapter_read_data(coupling%adapter_ptr, nodes%D, numnod, &
              real(dt, c_double), coupling_positions, coupling_replace)
          else if(name_id == coupling_forces) THEN
            ! Write forces
            NODES%FORCES(1:3,1:NUMNOD) = nodes%A(1:3,1:NUMNOD) - NODES%FORCES(1:3,1:NUMNOD)
            call coupling_adapter_write_data(coupling%adapter_ptr, nodes%FORCES, numnod, &
              real(dt, c_double), coupling_forces)
            ! Read forces
            call coupling_adapter_read_data(coupling%adapter_ptr, nodes%A, numnod, &
              real(dt, c_double), coupling_forces, coupling_add)
          else if(name_id == coupling_positions) then
            ! Write positions
            call coupling_adapter_write_data(coupling%adapter_ptr, nodes%X, numnod, &
              real(dt, c_double), coupling_positions)
            call coupling_adapter_read_data(coupling%adapter_ptr, nodes%X, numnod, &
              real(dt, c_double), coupling_positions, coupling_replace)
          end if
#else
          allocate(values(3, numnod))
          if(name_id == coupling_displacements) then
            ! Write displacements
            values(1:3,1:numnod) = real(nodes%D(1:3,1:numnod), c_double)
            call coupling_adapter_write_data(coupling%adapter_ptr, values, numnod, &
              real(dt, c_double), coupling_displacements)
            ! Read positions
            call coupling_adapter_read_data(coupling%adapter_ptr, values, numnod, &
              real(dt, c_double), coupling_positions, coupling_replace)
            nodes%D(1:3,1:numnod) = real(values(1:3,1:numnod), WP)
          else if(name_id == coupling_forces) THEN
            ! Write forces
            values(1:3,1:numnod) = real(nodes%A(1:3,1:numnod) - nodes%FORCES(1:3,1:numnod), c_double)
            call coupling_adapter_write_data(coupling%adapter_ptr, values, numnod, &
              real(dt, c_double), coupling_forces)
            ! Read forces
            call coupling_adapter_read_data(coupling%adapter_ptr, values, numnod, &
              real(dt, c_double), coupling_forces, coupling_add)
            nodes%A(1:3,1:numnod) = real(values(1:3,1:numnod), WP)
          else if(name_id == coupling_positions) then
            ! Write positions
            values(1:3,1:numnod) = real(nodes%X(1:3,1:numnod), c_double)
            call coupling_adapter_write_data(coupling%adapter_ptr, values, numnod, &
              real(dt, c_double), coupling_positions)
            call coupling_adapter_read_data(coupling%adapter_ptr, values, numnod, &
              real(dt, c_double), coupling_positions, coupling_replace)
            nodes%X(1:3,1:numnod) = real(values(1:3,1:numnod), WP)
          end if
          deallocate(values)
#endif

        end subroutine coupling_sync

        ! Advance coupling
        subroutine coupling_advance(coupling, dt)
          use precision_mod, only: WP
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling
          real(kind=WP), intent(inout) :: dt
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
!-----------------------------------------------------------------------------------------------------------------------
          real(c_double) :: c_dt
!------------------------------------------------------------------------------------------------------------------------
!                                                   Body
!------------------------------------------------------------------------------------------------------------------------
          if (.not. c_associated(coupling%adapter_ptr)) return
          if (.not. coupling%active) return

          c_dt = real(dt, c_double)
          call coupling_adapter_advance(coupling%adapter_ptr, c_dt)
          dt = real(c_dt, WP)

          coupling%dt_limit = coupling_adapter_get_max_time_step_size(coupling%adapter_ptr)
        end subroutine coupling_advance

        ! Check if coupling is ongoing
        subroutine coupling_ongoing(coupling, ongoing)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling
          logical, intent(out) :: ongoing
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
!-----------------------------------------------------------------------------------------------------------------------
          integer :: result
!------------------------------------------------------------------------------------------------------------------------
!                                                   Body
!------------------------------------------------------------------------------------------------------------------------
          ongoing = .false.
          if (.not. c_associated(coupling%adapter_ptr)) return
          if (.not. coupling%active) return

          result = coupling_adapter_is_coupling_ongoing(coupling%adapter_ptr)
          ongoing = (result == 1)
          coupling%active = ongoing
        end subroutine coupling_ongoing

        subroutine coupling_requires_writing_checkpoint(coupling, required)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(in) :: coupling
          logical, intent(out) :: required
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
!-----------------------------------------------------------------------------------------------------------------------
          integer :: result
!------------------------------------------------------------------------------------------------------------------------
!                                                   Body
!------------------------------------------------------------------------------------------------------------------------
          required = .false.
          if (.not. c_associated(coupling%adapter_ptr)) return
          if (.not. coupling%active) return

          result = coupling_adapter_requires_writing_checkpoint(coupling%adapter_ptr)
          required = (result == 1)
          if(required) then
            write(6,*) "Error: Coupling requires writing checkpoint"
            stop
          end if
        end subroutine coupling_requires_writing_checkpoint

        subroutine coupling_requires_reading_checkpoint(coupling, required)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(in) :: coupling
          logical, intent(out) :: required
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
!-----------------------------------------------------------------------------------------------------------------------
          integer :: result
!------------------------------------------------------------------------------------------------------------------------
!                                                   Body
!------------------------------------------------------------------------------------------------------------------------
          required = .false.
          if (.not. c_associated(coupling%adapter_ptr)) return
          if (.not. coupling%active) return

          result = coupling_adapter_requires_reading_checkpoint(coupling%adapter_ptr)
          required = (result == 1)
          if(required) then
            write(6,*) "Error: Coupling requires writing checkpoint"
            stop
          end if
        end subroutine coupling_requires_reading_checkpoint

        ! Finalize coupling
        subroutine coupling_finalize(coupling)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Body
!------------------------------------------------------------------------------------------------------------------------
          if (c_associated(coupling%adapter_ptr)) then
            call coupling_adapter_finalize(coupling%adapter_ptr)
            call coupling_adapter_destroy(coupling%adapter_ptr)
            coupling%adapter_ptr = c_null_ptr
          end if
          coupling%active = .false.
        end subroutine coupling_finalize

      end module coupling_adapter_mod
