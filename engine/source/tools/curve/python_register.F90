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

!! \brief Register the python functions saved in the python structure into the python interpreter dictionary
      !||====================================================================
      !||    python_register_mod   ../engine/source/tools/curve/python_register.F90
      !||--- called by ------------------------------------------------------
      !||    resol                 ../engine/source/engine/resol.F
      !||====================================================================
      module python_register_mod
      contains
      !||====================================================================
      !||    python_register                        ../engine/source/tools/curve/python_register.F90
      !||--- called by ------------------------------------------------------
      !||    resol                                  ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    element_user_id                        ../common_source/modules/element_user_id.F90
      !||    python_element_init                    ../engine/source/mpi/python_spmd_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    python_element_mod                     ../common_source/modules/python_element_mod.F90
      !||    python_funct_mod                       ../common_source/modules/python_mod.F90
      !||    python_spmd_mod                        ../engine/source/mpi/python_spmd_mod.F90
      !||    user_id_mod                            ../common_source/modules/element_user_id.F90
      !||====================================================================
        subroutine python_register(py, itab, numnod,&
        & ixs, nixs, numels, &
        & ixc, nixc, numelc, &
        & ixp, nixp, numelp, &
        & ixt, nixt, numelt, &
        & ixq, nixq, numelq, &
        & ixtg, nixtg, numeltg, &
        & ixr, nixr, numelr, &
        & iparg, ngroup, nparg, mvsiz)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
          use iso_c_binding, only : c_char, c_null_char
          use python_spmd_mod, only : python_element_init
          use python_element_mod, only : python_get_number_elemental_entities, python_get_elemental_entity
          use python_funct_mod, only : python_, max_code_length, max_line_length, NAME_LEN, python_create_node_mapping, &
          & python_register_function, python_initialize, python_load_environment
          use user_id_mod, only : element_user_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Implicit None
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),intent(inout) :: py          !< the Fortran structure that holds the python function
          integer,      intent(in) :: numnod         !< the global number of nodes
          integer,      intent(in) :: itab(numnod)   !< the global node ids
          integer, intent(in) :: nixs                !< number of integers in the solid data structure
          integer, intent(in) :: numels              !< number of solids
          integer, intent(in) :: ixs(nixs,numels)    !< solid data structure
          integer, intent(in) :: nixc                !< number of integers in the shell data structure
          integer, intent(in) :: numelc              !< number of shells
          integer, intent(in) :: ixc(nixc,numelc)    !< shell data structure
          integer, intent(in) :: nixp                !< number of integers in the beam data structure
          integer, intent(in) :: numelp              !< number of beams
          integer, intent(in) :: ixp(nixp,numelp)    !< beam data structure
          integer, intent(in) :: nixt                !< number of integers in the truss data structure
          integer, intent(in) :: numelt              !< number of trusses
          integer, intent(in) :: ixt(nixt,numelt)    !< truss data structure
          integer, intent(in) :: nixtg               !< number of integers in the triangle data structure
          integer, intent(in) :: numeltg             !< number of triangles
          integer, intent(in) :: ixtg(nixtg,numeltg) !< triangle data structure
          integer, intent(in) :: nixr                !< number of integers in the spring data structure
          integer, intent(in) :: numelr              !< number of springs
          integer, intent(in) :: ixr(nixr,numelr)    !< spring data structure
          integer, intent(in) :: nixq                !< number of integers in the quad data structure
          integer, intent(in) :: numelq              !< number of quads
          integer, intent(in) :: ixq(nixq,numelq)    !< quad data structure
          integer, intent(in) :: ngroup              !< number of groups
          integer, intent(in) :: nparg               !< number of integers in the group data structure
          integer, intent(in) :: iparg(nparg,ngroup) !< group data structure
          integer, intent(in) :: mvsiz               !< maximum size of a group
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          character(len=max_line_length)             :: name
          character(kind=c_char, len=:), allocatable :: code
          integer                                    :: i,j,n,ierror
          integer                                    :: nelem
          integer, dimension(:), allocatable         :: user_id
          integer, dimension(:), allocatable         :: local_id
          integer, dimension(:), allocatable         :: group_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          nelem = mvsiz * ngroup
          allocate(user_id(nelem))
          allocate(local_id(nelem))
          allocate(group_id(nelem))
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

          ! creates a mapping between the global node ids and the local node ids
          call python_create_node_mapping(itab, numnod)
          call element_user_id(user_id, group_id, local_id, nelem, &
            ixs, nixs, numels, &
            ixc, nixc, numelc, &
            ixp, nixp, numelp, &
            ixt, nixt, numelt, &
            ixq, nixq, numelq, &
            ixtg, nixtg, numeltg, &
            ixr, nixr, numelr, &
            iparg, ngroup, nparg)

          ! get the number of elemental entities called from Python
          call python_get_number_elemental_entities(n) !bind(c,name="cpp_python_get_number_elemental_entities")
          py%elements%global%n = n

          if(allocated(py%elements%global%keyword)) deallocate(py%elements%global%keyword)
          if(allocated(py%elements%global%user_ids)) deallocate(py%elements%global%user_ids)
          allocate(py%elements%global%keyword(py%elements%global%n))
          allocate(py%elements%global%user_ids(py%elements%global%n))
          ! allocate the other arrays
          if(allocated(py%elements%global%values)) deallocate(py%elements%global%values)
          allocate(py%elements%global%values(py%elements%global%n))
          py%elements%global%values = 0.0d0
          if(allocated(py%elements%global%processor)) deallocate(py%elements%global%processor)
          allocate(py%elements%global%processor(py%elements%global%n))
          py%elements%global%processor = 0

          do i = 1, n
            do j = 1, NAME_LEN
              py%elements%global%keyword(i)%h3d(j:j) = c_null_char
              py%elements%global%keyword(i)%name(j:j) = c_null_char
            enddo
            call python_get_elemental_entity(i,py%elements%global%keyword(i)%h3d,py%elements%global%user_ids(i)) !bind(c,name="cpp_python_get_elemental_entity")
          enddo

          call python_element_init(py%elements, n, group_id, local_id, user_id)
          if(py%nb_functs >0 )  call python_load_environment()
          deallocate(code)
          deallocate(user_id)
          deallocate(local_id)
          deallocate(group_id)
        end subroutine

      end module
