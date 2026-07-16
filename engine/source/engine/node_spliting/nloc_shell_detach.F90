!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    nloc_shell_detach_mod   ../engine/source/engine/node_spliting/nloc_shell_detach.F90
!||--- called by ------------------------------------------------------
!||    resol                   ../engine/source/engine/resol.F
!||====================================================================
      module nloc_shell_detach_mod
        implicit none

      contains

!! \brief Crack propagation by node splitting driven by the non-local damage field.
!||====================================================================
!||    nloc_shell_detach            ../engine/source/engine/node_spliting/nloc_shell_detach.F90
!||--- called by ------------------------------------------------------
!||    resol                        ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    apply_crack                  ../engine/source/engine/node_spliting/apply_crack.F90
!||    spmd_exchange_ghost_shells   ../engine/source/engine/node_spliting/ghost_shells.F90
!||--- uses       -----------------------------------------------------
!||    apply_crack_mod              ../engine/source/engine/node_spliting/apply_crack.F90
!||    check_pon_consistency_mod    ../engine/source/engine/node_spliting/check_pon_consistency.F90
!||    connectivity_mod             ../common_source/modules/connectivity.F90
!||    detach_node_mod              ../engine/source/engine/node_spliting/detach_node.F90
!||    ghost_shells_mod             ../engine/source/engine/node_spliting/ghost_shells.F90
!||    interfaces_mod               ../common_source/modules/interfaces/interfaces_mod.F90
!||    nlocal_reg_mod               ../common_source/modules/nlocal_reg_mod.F
!||    nodal_arrays_mod             ../common_source/modules/nodal_arrays.F90
!||    precision_mod                ../common_source/modules/precision_mod.F90
!||    spmd_mod                     ../engine/source/mpi/spmd_mod.F90
!||    umap_mod                     ../common_source/tools/container/umap_mod.F90
!||====================================================================
        subroutine nloc_shell_detach(nodes, element, interf, npari, ninter, ipari, numnod, &
          numnodg, numelc, ispmd, nspmd, new_crack, nloc_dmg, nthread)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use spmd_mod
          use ghost_shells_mod
          use precision_mod, only : wp
          use connectivity_mod
          use nodal_arrays_mod
          use interfaces_mod
          use detach_node_mod
          use nlocal_reg_mod
          use umap_mod
          use apply_crack_mod, only: node_split_info, apply_crack
          use check_pon_consistency_mod, only: check_pon_consistency
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(inout) :: nodes        !< nodal arrays
          type(connectivity_), intent(inout) :: element      !< element connectivity
          type(interfaces_),   intent(inout) :: interf       !< interface structure
          integer,             intent(in)    :: npari        !< number of interface parameters
          integer,             intent(in)    :: ninter       !< number of interfaces
          integer,             intent(inout) :: ipari(npari, ninter) !< interface parameters
          integer,             intent(inout) :: numnod       !< current number of local nodes
          integer,             intent(inout) :: numnodg      !< current number of global nodes
          integer,             intent(in)    :: numelc       !< number of quad-shell elements
          integer,             intent(in)    :: ispmd        !< local MPI rank (0-based)
          integer,             intent(in)    :: nspmd        !< number of MPI domains
          integer,             intent(out)   :: new_crack    !< number of new nodes created (sum over all domains)
          type(nlocal_str_),   intent(inout) :: nloc_dmg     !< non-local damage structure
          integer,             intent(in)    :: nthread      !< number of OpenMP threads
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=wp), allocatable :: detach_shell(:)  ! normalised damage per shell (0=intact, 1=failed)
          real(kind=wp), allocatable :: ghostshelldamage(:)
          double precision, allocatable :: nodal_damage(:)
          logical, allocatable :: parent_split_step(:)  ! Guard: one split per parent per cycle
          integer :: i, j, k, p
          integer :: nghostshells
          integer :: crack(20), nl_idx, nl_dof
          integer :: n_debug_shells
          integer :: diag_nl_idx, diag_old_pos, diag_dof  ! for diagnostic output
          double precision :: v(3), distance, current_max_dist, ecc_norm
          real(kind=wp) :: shell_peak_damage
          real(kind=wp), parameter :: split_frac = 0.5_wp  ! fraction of threshold to trigger split
          logical, parameter :: debug_detach_monitor = .false.  ! Disable verbose debug output
          integer, save :: diag_call_count = 0  ! sequential counter across calls
          integer :: new_owned_nodes, new_ghost_nodes
          type(node_split_info), dimension(:), allocatable :: crack_info_list

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          new_crack = 0
          ! touch the input arguments to avoid compiler warnings about unused variables
          if(ipari(1,1) .ne. ipari(1,1)) return
          if(nthread .ne. nthread) return
          if (interf%ninter .ne. interf%ninter) return
          if (nloc_dmg%imod == 0) return  ! non-local damage inactive
          if(nspmd.ne. nspmd) return

          allocate(detach_shell(0:numelc))
          detach_shell = 0.0_wp
          nghostshells = size(element%ghost_shell%uid)
          allocate(ghostshelldamage(nghostshells))
          ghostshelldamage = 0.0_wp

!          do i = 1, nodes%numnod
!            if (nodes%itab(i) == 13550) then
!              write(*,'(a,i0,a,i0,a,i0)') '[SPLIT][rank ', ispmd, '] DEBUG: node ', i, &
!                ' itab=', nodes%itab(i), ' weight=', nodes%WEIGHT(i)
!              flush(6)
!            end if
!          end do


          ! The values in detach_shell are local to each MPI domain.
          ! in output, the ghostshelldamage array will contain the maximum detach_shell value for each ghost shell across all MPI domains.
          call spmd_exchange_ghost_shells(element, ispmd, nspmd, 1, detach_shell, ghostshelldamage)

          ! Accumulate nodal damage: max detach_shell of all connected shells
          deallocate(ghostshelldamage)
          diag_call_count = diag_call_count + 1
          if(diag_call_count == 1) then
!         ! test  PUNCH_NLOCAL, fill the crack_info_list with a list of nodes and shells to detach, should be replaced by a physical criterion based on the non-local damage field
            ! ==================== TEST for PUNCH_NLOCAL ====================
            ! THIS IS THE CODE SNIPPET TO REPLACE WITH A PHYSICAL CRITERION BASED ON THE NON-LOCAL DAMAGE FIELD
            block  ! example on how to detach a list of nodes and shells, should be replaced by a physical criterion based on the non-local damage field
              !detach nodes :
              !Nodes 10682 10683 10684 .. 10722
              ! from list
              !Elements 12963 12966 12967 12987 13044 13047 13048 13053 13054 13055 13071 13074 13075 13089 13092 13093 12891 12894 12895 12984 12912 129113 12981 12985
              ! 13323 13326 13327 13341 13344 13345 13395 13398 13399 13413 13416 13417 13623 13626 13627 13644 13695 13698 13699 13716 13717 13722 13723
              integer  :: node_list(41), local_node_list(41)
              integer :: shell_list(49), local_shell_list(49)
              integer :: i,j, minuid, min_ghost_k,p
              integer :: nlocal_shell, nlocal_node, nghost_shell
              logical :: locally_owned_shell
              ! example of filling the crack_info_list based on the above lists, should be replaced by a physical criterion based on the non-local damage field

              node_list = [10682, 10683, 10684, 10685, 10686, 10687, 10688, 10689, 10690, 10691, 10692, 10693, &
                10694, 10695, 10696, 10697, 10698, 10699, 10700, 10701, 10702, 10703, 10704, 10705, 10706, 10707, &
                10708, 10709, 10710, 10711, 10712, 10713, 10714, 10715, 10716, 10717, 10718, 10719, 10720, 10721, 10722]

              ! REGRESSION TUNING for the /PARITH/ON latent band-order bug:
              ! shell 12010 is the 3rd fan shell of nodes 10711 & 10712 (their fans are
              ! {12010,12097,13326,13413} and {12010,12012,13326,13327}).  Adding it makes
              ! those two nodes migrate 3 shells with interleaved user-ids across the domain
              ! cut (12010 < 13326[rank5] < 13413[rank7]), so a mirror rank's role-based
              ! [local|recv] band is no longer uid-sorted -> non-associative sum diverges
              ! from the 1-rank run.  Each node still keeps >=1 shell on the parent
              ! (10711 keeps 12097, 10712 keeps 12012).  (At np=8; see doc/NODE_SPLIT_latent_band_order_fix.md.)
              shell_list = [12963, 12966, 12967, 12987, 13044, 13047, 13048, 13053, 13054, 13055, 13071, 13074, 13075, 12984, &
                13089, 13092, 13093, 12891, 12894, 12895, 12909, 12912, 12913, 12981, 12985,&
                13323, 13326, 13327, 13341, 13344, 13345, &
                13395, 13398, 13399, 13413, 13416, 13417, 13623, 13626,&
                13627, 13644, 13695, 13698, 13699, 13716, 13717, 13722, 13723, &
                12010]


              nlocal_shell = 0
              nlocal_node = 0

              ! for this demonstration, we set the same lists on all MPI domains, so we need to filter the lists to only keep the
              ! local nodes and shells on each MPI domain
              do i = 1, 41
                j = get_local_node_id(nodes, node_list(i))
                if (j > 0)  then
                  nlocal_node = nlocal_node + 1
                  local_node_list(nlocal_node) = j
                end if
              end do

              !fill crackinfo_list with only the local nodes and the shells to detach for each local node
              allocate(crack_info_list(nlocal_node))
              ! fill the crack_info_list with ids
              do i = 1, nlocal_node
                crack_info_list(i)%parent_id = local_node_list(i)
                crack_info_list(i)%parent_uid = nodes%itab(local_node_list(i))
              end do


              do i = 1, nlocal_node
                nlocal_shell = 0
                ! count the number of shells to detach for this node and fill the shell_uids list in the crack_info structure,
                !  should be replaced by a physical criterion based on the non-local damage field
                do j = 1, size(shell_list)
                  k = get_local_shell_id(element%shell, shell_list(j))
                  if(k == 0) then ! loop over ghost shells to find the local node
                    k = get_value_umap(element%ghost_shell%glob2loc, shell_list(j), 0)
                    if (k > 0) then
                      ! The shell is a ghost shell, check if the local node is connected to it
                      if (any(element%ghost_shell%nodes(:, k) == local_node_list(i))) then
                        ! The node is connected to the ghost shell, mark it for detachment
                        nlocal_shell = nlocal_shell + 1
                        local_shell_list(nlocal_shell) = -k ! mark ghost shells with negative index
                      end if
                    end if
                  elseif(k > 0) then ! search in element shell connectivity for the local node
                    if (any(element%shell%nodes(:, k) == local_node_list(i))) then
                      ! The node is connected to the local shell, mark it for detachment
                      nlocal_shell = nlocal_shell + 1
                      local_shell_list(nlocal_shell) = k
                    end if
                  else
                    ! This should not happen, but handle it just in case
                    !print *, "Warning: local_shell_list contains zero index for node ", local_node_list(i)
                  end if
                end do


                ! allocate and fill crack_info_list(i)%shell_uids with the shells to detach for this node
                if (nlocal_shell > 0) then
                  allocate(crack_info_list(i)%shell_uids(nlocal_shell))
                  crack_info_list(i)%shell_uids = local_shell_list(1:nlocal_shell)
                else
                  allocate(crack_info_list(i)%shell_uids(0))
                end if
              end do
            end block
            ! END OF THE CODE SNIPPET TO REPLACE WITH A PHYSICAL CRITERION BASED ON THE NON-LOCAL DAMAGE FIELD
            ! =================== END OF TEST for PUNCH_NLOCAL ====================
!

            ! Debug: print every crack candidate submitted to apply_crack so 1-rank vs 8-rank
            ! runs can be compared.  Format: [SPLIT][rank R] CANDIDATE: ...
!            do i = 1, size(crack_info_list)
!              write(*,'(a,i0,a,i0,a,f12.4,a)',advance='no') &
!                '[SPLIT][rank ', ispmd, '] CANDIDATE: parent_uid=', crack_info_list(i)%parent_uid, &
!                ' ms=', real(nodes%ms(crack_info_list(i)%parent_id)), ' shells='
!              if (allocated(crack_info_list(i)%shell_uids)) then
!                do j = 1, size(crack_info_list(i)%shell_uids)
!                  write(*,'(i0,a)',advance='no') crack_info_list(i)%shell_uids(j), ' '
!                end do
!              end if
!              write(*,*)
!              flush(6)
!            end do

            call apply_crack(nodes, element, interf, npari, ninter, ipari, numnod, numnodg, &
              ispmd, nspmd, nloc_dmg, nthread, new_crack, crack_info_list)

!            call check_pon_consistency(nodes, element, ispmd)
          endif

          if (allocated(detach_shell))           deallocate(detach_shell)
!         if (allocated(shell_list))             deallocate(shell_list)
          if (allocated(nodal_damage))           deallocate(nodal_damage)

        end subroutine nloc_shell_detach

      end module nloc_shell_detach_mod
