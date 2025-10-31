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
!||    coupling_adapter_mod   ../engine/source/coupling/coupling_adapter.F90
!||--- called by ------------------------------------------------------
!||    inipar                 ../engine/source/mpi/init/inipar.F
!||    radioss2               ../engine/source/engine/radioss2.F
!||    resol                  ../engine/source/engine/resol.F
!||    resol_head             ../engine/source/engine/resol_head.F
!||====================================================================
      module coupling_adapter_mod
        ! This file contain the generic coupling adapter interface used by CWIPI and preCICE
        ! The workflow is as follows:
        ! 1. coupling_configure: called in inipar.F
        !    a) This creates the adapter pointer
        !    b) Reads the configuration file (e.g., modelName.cpl if the engine filename is modelName_0001.rad) see the configure method implementations for the format of the file
        ! 2. coupling_set_interface is called from resol.F to set the coupling nodes/mesh
        ! 3. coupling_initialize is called from resol.F to initialize the coupling adapter
        ! 4. coupling_sync is called from resol.F to synchronize the data
        !    a) this contains the calls to coupling_adapter_write_data and coupling_adapter_read_data that exchange data with the coupling library
        !    b) update the nodal arrays (nodes/acceleration) with the data read from the coupling library
        ! 5. advance called from resol.F is called to advance the coupling adapter
        !
        ! Implementation details:
        ! The implementation is done in the C++ layer for both preCICE and CWIPI.
        ! When compiling with -preCICE or -cwipi=/path/to/cwipi, the cwipi or the preCICE coupling adapter is built.
        ! Both adapters implement the virtual class in coupling.h
        ! - For preCICE see precice_coupling_adapter.cpp
        ! - For CWIPI see cwipi_coupling_adapter.cpp
        use, intrinsic :: iso_c_binding
        implicit none

        ! Data type constants
        integer, parameter :: coupling_displacements = 1
        integer, parameter :: coupling_forces = 2
        integer, parameter :: coupling_positions = 3
        ! In order to add a new data type, add a new integer here and in the C++ adapter
        ! see enum class DataType in coupling.h and the implementation of the configure method in both cwipi and preCICE adapters

        ! Operation modes
        integer, parameter :: coupling_replace = 1
        integer, parameter :: coupling_add = 2

        integer, parameter :: COUPLING_PRECICE = 1
        integer, parameter :: COUPLING_CWIPI = 2

        ! Generic coupling adapter type
        type :: coupling_type
          type(c_ptr) :: adapter_ptr = c_null_ptr ! Pointer to the C coupling adapter
          logical :: active = .false.
          integer :: nb_coupling_nodes = 0
          double precision :: dt_limit = 0.0d0
          integer :: grnod_id = 0
          integer :: surface_id = 0
          integer :: coupler
          character :: filnam*100
        end type coupling_type

        ! C interface declarations cooresponding to the file coupling_c_interface./cpp
        interface
          function coupling_adapter_create() bind(c, name="coupling_adapter_create")
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: coupling_adapter_create
          end function coupling_adapter_create
          subroutine coupling_adapter_destroy(adapter) bind(c, name="coupling_adapter_destroy")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
          end subroutine coupling_adapter_destroy
          function coupling_adapter_configure(adapter, filename) bind(c, name="coupling_adapter_configure")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int) :: coupling_adapter_configure
          end function coupling_adapter_configure
          subroutine coupling_adapter_set_nodes(adapter, node_ids, num_nodes) bind(c, name="coupling_adapter_set_nodes")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int), intent(in) :: node_ids(*)
            integer(c_int), value :: num_nodes
          end subroutine coupling_adapter_set_nodes
          function coupling_adapter_initialize(adapter, coordinates, total_nodes, mpi_rank, mpi_size) &
            bind(c, name="coupling_adapter_initialize")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            real(c_double), intent(in) :: coordinates(*)
            integer(c_int), value :: total_nodes, mpi_rank, mpi_size
            integer(c_int) :: coupling_adapter_initialize
          end function coupling_adapter_initialize
          subroutine coupling_adapter_write_data(adapter, values, total_nodes, dt, data_type) &
            bind(c, name="coupling_adapter_write_data")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            real(c_double), intent(in) :: values(*)
            integer(c_int), value :: total_nodes, data_type
            real(c_double), value :: dt
          end subroutine coupling_adapter_write_data
          subroutine coupling_adapter_read_data(adapter, values, total_nodes, dt, data_type, mode) &
            bind(c, name="coupling_adapter_read_data")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            real(c_double), intent(inout) :: values(*)
            integer(c_int), value :: total_nodes, data_type, mode
            real(c_double), value :: dt
          end subroutine coupling_adapter_read_data

          subroutine coupling_adapter_advance(adapter, dt) bind(c, name="coupling_adapter_advance")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            real(c_double), intent(inout) :: dt
          end subroutine coupling_adapter_advance
          function coupling_adapter_is_coupling_ongoing(adapter) bind(c, name="coupling_adapter_is_coupling_ongoing")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_is_coupling_ongoing
          end function coupling_adapter_is_coupling_ongoing
          function coupling_adapter_requires_writing_checkpoint(adapter) &
            bind(c, name="coupling_adapter_requires_writing_checkpoint")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_requires_writing_checkpoint
          end function coupling_adapter_requires_writing_checkpoint
          function coupling_adapter_requires_reading_checkpoint(adapter) &
            bind(c, name="coupling_adapter_requires_reading_checkpoint")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_requires_reading_checkpoint
          end function coupling_adapter_requires_reading_checkpoint
          subroutine coupling_adapter_finalize(adapter) bind(c, name="coupling_adapter_finalize")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
          end subroutine coupling_adapter_finalize
          function coupling_adapter_is_active(adapter) bind(c, name="coupling_adapter_is_active")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_is_active
          end function coupling_adapter_is_active
          function coupling_adapter_get_max_time_step_size(adapter) &
            bind(c, name="coupling_adapter_get_max_time_step_size")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            real(c_double) :: coupling_adapter_get_max_time_step_size
          end function coupling_adapter_get_max_time_step_size

          function coupling_adapter_get_num_coupling_nodes(adapter) &
            bind(c, name="coupling_adapter_get_num_coupling_nodes")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_get_num_coupling_nodes
          end function coupling_adapter_get_num_coupling_nodes
          !    int coupling_adapter_get_group_node_id(void* adapter);
          function coupling_adapter_get_group_node_id(adapter) &
            bind(c, name="coupling_adapter_get_group_node_id")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_get_group_node_id
          end function coupling_adapter_get_group_node_id

          !int coupling_adapter_get_surface_id(void* adapter) {
          function coupling_adapter_get_surface_id(adapter) &
            bind(c, name="coupling_adapter_get_surface_id")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_adapter_get_surface_id
          end function coupling_adapter_get_surface_id

          function coupling_get_communicator(adapter) &
            bind(c, name="coupling_adapter_get_communicator")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int) :: coupling_get_communicator
          end function coupling_get_communicator

          ! Not available in preCICE yet, only cwipi:
          subroutine coupling_adapter_set_mesh(adapter, elem_node_offsets, elem_node_indices, num_elements) &
            bind(c, name="coupling_adapter_set_mesh")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: adapter
            integer(c_int), intent(in) :: elem_node_offsets(*), elem_node_indices(*)
            integer(c_int), value :: num_elements
          end subroutine coupling_adapter_set_mesh

        end interface

      contains
        !!utility function to make unique values in an array of size 4
!||====================================================================
!||    make_unique         ../engine/source/coupling/coupling_adapter.F90
!||--- called by ------------------------------------------------------
!||    coupling_set_mesh   ../engine/source/coupling/coupling_adapter.F90
!||====================================================================
        function make_unique(arr) result(n_unique)
          implicit none
          integer, intent(inout) :: arr(4)
          integer :: n_unique
          integer :: temp(4)
          integer :: i, j
          logical :: is_new
          n_unique = 0
          ! Identify unique values and store them in temp
          do i = 1, 4
            if (arr(i) < 0) cycle
            is_new = .true.
            do j = 1, n_unique
              if (arr(i) == temp(j)) then
                is_new = .false.
                exit
              end if
            end do
            if (is_new) then
              n_unique = n_unique + 1
              temp(n_unique) = arr(i)
            end if
          end do
          ! Fill arr with the compacted unique values and -1
          do i = 1, n_unique
            arr(i) = temp(i)
          end do
          do i = n_unique + 1, 4
            arr(i) = -1
          end do
        end function make_unique

        ! Initialize coupling adapter
!||====================================================================
!||    coupling_create      ../engine/source/coupling/coupling_adapter.F90
!||--- called by ------------------------------------------------------
!||    coupling_configure   ../engine/source/coupling/coupling_adapter.F90
!||====================================================================
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
!||====================================================================
!||    coupling_configure   ../engine/source/coupling/coupling_adapter.F90
!||--- calls      -----------------------------------------------------
!||    coupling_create      ../engine/source/coupling/coupling_adapter.F90
!||====================================================================
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
          if(.not.coupling%active) then
            call coupling_create(coupling)
          end if
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
!||====================================================================
!||    coupling_set_nodes           ../engine/source/coupling/coupling_adapter.F90
!||--- called by ------------------------------------------------------
!||    coupling_set_interface       ../engine/source/coupling/coupling_adapter.F90
!||--- calls      -----------------------------------------------------
!||    coupling_adapter_set_nodes   ../engine/source/coupling/coupling_c_interface.cpp
!||--- uses       -----------------------------------------------------
!||    groupdef_mod                 ../common_source/modules/groupdef_mod.F
!||====================================================================
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
          if(coupling%grnod_id == 0) then
            return
          end if
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

!       Only for cwipi, not for precice yet
!||====================================================================
!||    coupling_set_mesh            ../engine/source/coupling/coupling_adapter.F90
!||--- called by ------------------------------------------------------
!||    coupling_set_interface       ../engine/source/coupling/coupling_adapter.F90
!||--- calls      -----------------------------------------------------
!||    coupling_adapter_set_mesh    ../engine/source/coupling/coupling_c_interface.cpp
!||    coupling_adapter_set_nodes   ../engine/source/coupling/coupling_c_interface.cpp
!||    make_unique                  ../engine/source/coupling/coupling_adapter.F90
!||--- uses       -----------------------------------------------------
!||    groupdef_mod                 ../common_source/modules/groupdef_mod.F
!||    nodal_arrays_mod             ../common_source/modules/nodal_arrays.F90
!||====================================================================
        subroutine coupling_set_mesh(coupling, surf,  nodes)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
!----------------------------------------------------------------------------------------------------------------------
          use GROUPDEF_MOD, only: surf_
          use nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling !< Coupling adapter
          type(surf_), intent(in) :: surf !< Array of surfaces
          type(nodal_arrays_), intent(in) :: nodes !< Nodal arrays
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,n,counter
          integer, dimension(:), allocatable :: index
          integer :: nb_unique_nodes
          integer :: next_node
          integer :: tmp(4)
          integer, dimension(:), allocatable :: connectIndex, connec, node_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(index(nodes%numnod))
          allocate(connectIndex(surf%NSEG+1))
          connectIndex = 0
          connectIndex(1) = 0
          allocate(connec(surf%NSEG*4))
          connec = 0
          allocate(node_id(surf%NSEG*4))
          node_id = 0
          write(6,*) "surf%nodes size:", size(surf%nodes)
          write(6,*) "surf%NSEG size:", surf%NSEG
          write(6,*) "surf%elem size", size(surf%elem)
          write(6,*) "surf%eltyp size:", size(surf%eltyp)
          write(6,*) "surf%type",surf%type
          call flush(6)
          index = 0
          counter = 0
          next_node = 0
          do i = 1, surf%nseg
            connectIndex(i+1) = connectIndex(i)
            ! check if it's a triangle = two surf%nodes(:,i) are the same
            tmp(1) = surf%nodes(i,1)
            tmp(2) = surf%nodes(i,2)
            tmp(3) = surf%nodes(i,3)
            tmp(4) = surf%nodes(i,4)
            !write(6,*) "surf%nodes", tmp(1), tmp(2), tmp(3), tmp(4)
            nb_unique_nodes = make_unique(tmp)
            connectIndex(i+1) = connectIndex(i+1) + nb_unique_nodes
            do j = 1, nb_unique_nodes
              if(tmp(j) < 0) then
                write(6,*) "Error in surf%nodes", tmp(j)
                cycle
              end if
              if(tmp(j) > nodes%numnod) then
                write(6,*) "Error in surf%nodes", tmp(j), nodes%numnod
                cycle
              end if
              n = tmp(j)
              if(index(n) == 0) then
                counter = counter + 1
                index(n) = counter
                node_id(counter) = n
              end if
              next_node = next_node + 1
              if(next_node /= connectIndex(i) + j ) then
                write(6,*) "Error in connectIndex?", connectIndex(i)+j, next_node
              end if
              connec(next_node) = index(n)
            end do
          end do
          call coupling_adapter_set_mesh(coupling%adapter_ptr, connectIndex, connec, surf%NSEG)
          coupling%nb_coupling_nodes = counter
          call coupling_adapter_set_nodes(coupling%adapter_ptr, node_id, counter)

        end subroutine coupling_set_mesh

!||====================================================================
!||    coupling_set_interface   ../engine/source/coupling/coupling_adapter.F90
!||--- called by ------------------------------------------------------
!||    resol                    ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    coupling_set_mesh        ../engine/source/coupling/coupling_adapter.F90
!||    coupling_set_nodes       ../engine/source/coupling/coupling_adapter.F90
!||--- uses       -----------------------------------------------------
!||    groupdef_mod             ../common_source/modules/groupdef_mod.F
!||    nodal_arrays_mod         ../common_source/modules/nodal_arrays.F90
!||====================================================================
        subroutine coupling_set_interface(coupling, igrnod, ngrnod, surf, nsurf,  nodes)
          use GROUPDEF_MOD
          use nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_type), intent(inout) :: coupling !< Coupling adapter
          integer, intent(in) :: ngrnod !< Number of groups
          integer, intent(in) :: nsurf !< Number of surfaces
          type(GROUP_), intent(in) :: igrnod(ngrnod) !< Array of groups
          type(surf_), intent(in) :: surf(nsurf) !< Array of surfaces
          type(nodal_arrays_), intent(in) :: nodes !< Nodal arrays
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
!-----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer :: surface_id
!------------------------------------------------------------------------------------------------------------------------
!                                                   Body
!------------------------------------------------------------------------------------------------------------------------
          surface_id = coupling_adapter_get_surface_id(coupling%adapter_ptr)
          ! convert global user surface ID to local surface ID
          coupling%surface_id = 0
          do i = 1,nsurf
            if(surface_id == surf(i)%id) then
              coupling%surface_id= i
              call coupling_set_mesh(coupling, surf(i), nodes)
            end if
          end do
          if(coupling%surface_id == 0) call coupling_set_nodes(coupling, igrnod, ngrnod)
        end subroutine coupling_set_interface


        ! Initialize coupling
!||====================================================================
!||    coupling_initialize   ../engine/source/coupling/coupling_adapter.F90
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||====================================================================
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
!||====================================================================
!||    coupling_write                ../engine/source/coupling/coupling_adapter.F90
!||--- calls      -----------------------------------------------------
!||    coupling_adapter_write_data   ../engine/source/coupling/coupling_c_interface.cpp
!||--- uses       -----------------------------------------------------
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
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
#ifndef MYREAL8
          real(c_double), dimension(:,:), allocatable :: values
#endif
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
!||====================================================================
!||    coupling_read                ../engine/source/coupling/coupling_adapter.F90
!||--- calls      -----------------------------------------------------
!||    coupling_adapter_read_data   ../engine/source/coupling/coupling_c_interface.cpp
!||--- uses       -----------------------------------------------------
!||    precision_mod                ../common_source/modules/precision_mod.F90
!||====================================================================
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
#ifndef MYREAL8
          real(c_double), dimension(:,:), allocatable :: values
#endif
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
!||====================================================================
!||    coupling_sync                 ../engine/source/coupling/coupling_adapter.F90
!||--- called by ------------------------------------------------------
!||    resol                         ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    coupling_adapter_read_data    ../engine/source/coupling/coupling_c_interface.cpp
!||    coupling_adapter_write_data   ../engine/source/coupling/coupling_c_interface.cpp
!||--- uses       -----------------------------------------------------
!||    nodal_arrays_mod              ../common_source/modules/nodal_arrays.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
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
#ifndef MYREAL8
          real(c_double), dimension(:,:), allocatable :: values
#endif
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
            ! Read forces into nodes%A
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
!||====================================================================
!||    coupling_advance           ../engine/source/coupling/coupling_adapter.F90
!||--- called by ------------------------------------------------------
!||    resol                      ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    coupling_adapter_advance   ../engine/source/coupling/coupling_c_interface.cpp
!||--- uses       -----------------------------------------------------
!||    precision_mod              ../common_source/modules/precision_mod.F90
!||====================================================================
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
!||====================================================================
!||    coupling_ongoing   ../engine/source/coupling/coupling_adapter.F90
!||--- called by ------------------------------------------------------
!||    resol              ../engine/source/engine/resol.F
!||====================================================================
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

!||====================================================================
!||    coupling_requires_writing_checkpoint   ../engine/source/coupling/coupling_adapter.F90
!||====================================================================
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

!||====================================================================
!||    coupling_requires_reading_checkpoint   ../engine/source/coupling/coupling_adapter.F90
!||====================================================================
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
!||====================================================================
!||    coupling_finalize           ../engine/source/coupling/coupling_adapter.F90
!||--- called by ------------------------------------------------------
!||    resol                       ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    coupling_adapter_destroy    ../engine/source/coupling/coupling_c_interface.cpp
!||    coupling_adapter_finalize   ../engine/source/coupling/coupling_c_interface.cpp
!||====================================================================
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
