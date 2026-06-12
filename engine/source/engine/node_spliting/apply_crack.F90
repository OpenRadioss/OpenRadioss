!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    apply_crack_mod            ../engine/source/engine/node_spliting/apply_crack.F90
!||--- called by ------------------------------------------------------
!||    nloc_shell_detach          ../engine/source/engine/node_spliting/nloc_shell_detach.F90
!||--- calls      -----------------------------------------------------
!||    detach_node                ../engine/source/engine/node_spliting/detach_node.F90
!||    mirror_node_split          ../engine/source/engine/node_spliting/detach_node.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod           ../common_source/modules/connectivity.F90
!||    detach_node_mod            ../engine/source/engine/node_spliting/detach_node.F90
!||    interfaces_mod             ../common_source/modules/interfaces/interfaces_mod.F90
!||    nlocal_reg_mod             ../common_source/modules/nlocal_reg_mod.F
!||    nodal_arrays_mod           ../common_source/modules/nodal_arrays.F90
!||    precision_mod              ../common_source/modules/precision_mod.F90
!||    spmd_mod                   ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      module apply_crack_mod
        implicit none

!! \brief Information about a single node split request.
!! \details Built by the crack-detection loop; consumed by apply_crack.
        type :: node_split_info
          integer :: parent_uid  = -1  !< user id of the parent node to split
          integer :: parent_id   = -1  !< local id of the parent node to split
          integer :: node_uid    = -1  !< new user id of the new node  [auto-assigned in output]
          integer :: node_id     = -1  !< local id of the new node [auto-assigned in output]
          integer :: weight      = -1  !< 1 = this rank owns the new node, 0 = remote rank, -1 = not set
          integer :: owning_rank = -1  !< owning rank of the new node (0-based), set by apply_crack
          integer, dimension(:), allocatable :: shell_uids !< signed local ids of shells to detach
          !< (positive = local shell, negative = ghost shell local index)
        end type node_split_info

      contains

!! \brief Determine ownership and perform node splits for all entries in crack_info_list.
!! \details For each entry:
!!          1. Find the shell with the globally-smallest user id among the attached shells.
!!             If that shell is local, this rank owns the new node (weight=1).
!!             If it is a ghost shell, the rank that sent it (via ghost_shell%offset) owns it (weight=0).
!!             This determination is consistent across all ranks sharing the parent node because
!!             init_ghost_shells ensures every rank receives ghost copies of all shells that
!!             share a boundary node with it.
!!          2. If weight==1, call detach_node with LOCAL shells only (positive shell_uids).
!!             If weight==0 but this rank has local shells going to N', call mirror_node_split
!!             which creates a ghost copy of N' with MAIN_PROC=owning_rank.
!!          3. Phase 3: all ranks exchange (parent_uid, owning_rank) pairs via spmd_allgatherv.
!!          4. Phase 4: sort parent_uids, de-duplicate (same parent from multiple ranks = one
!!             unique split), assign one UID per unique split deterministically. All ranks that
!!             created a local N' for the same parent get the SAME uid assigned to their local slot.
!!          5. Phase 5: ranks that hold the parent locally but did NOT create N' halve ms/ms0.
!||====================================================================
!||    apply_crack                ../engine/source/engine/node_spliting/apply_crack.F90
!||--- called by ------------------------------------------------------
!||    nloc_shell_detach          ../engine/source/engine/node_spliting/nloc_shell_detach.F90
!||--- calls      -----------------------------------------------------
!||    detach_node                ../engine/source/engine/node_spliting/detach_node.F90
!||    mirror_node_split          ../engine/source/engine/node_spliting/detach_node.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod           ../common_source/modules/connectivity.F90
!||    detach_node_mod            ../engine/source/engine/node_spliting/detach_node.F90
!||    interfaces_mod             ../common_source/modules/interfaces/interfaces_mod.F90
!||    nlocal_reg_mod             ../common_source/modules/nlocal_reg_mod.F
!||    nodal_arrays_mod           ../common_source/modules/nodal_arrays.F90
!||    precision_mod              ../common_source/modules/precision_mod.F90
!||    spmd_mod                   ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine apply_crack(nodes, element, interf, npari, ninter, ipari, numnod, numnodg, &
          ispmd, nspmd, nloc_dmg, nthread, new_crack, crack_info_list)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use connectivity_mod
          use nodal_arrays_mod
          use interfaces_mod
          use nlocal_reg_mod
          use detach_node_mod
          use detach_node_nloc_mod
          use extend_array_mod
          use update_pon_mod, only : update_pon_shells
          use spmd_mod
          use precision_mod, only: wp
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_),   intent(inout) :: nodes        !< nodal arrays
          type(connectivity_),   intent(inout) :: element      !< element connectivity
          type(interfaces_),     intent(inout) :: interf       !< interface structure
          integer,               intent(in)    :: npari        !< number of interface parameters
          integer,               intent(in)    :: ninter       !< number of interfaces
          integer,               intent(inout) :: ipari(npari, ninter) !< interface parameters
          integer,               intent(inout) :: numnod       !< local node count (updated after splits)
          integer,               intent(inout) :: numnodg      !< global node count (updated after uid sync)
          integer,               intent(in)    :: ispmd        !< local MPI rank (0-based)
          integer,               intent(in)    :: nspmd        !< number of MPI domains
          type(nlocal_str_),     intent(inout) :: nloc_dmg     !< non-local damage structure
          integer,               intent(in)    :: nthread      !< number of OpenMP threads
          integer,               intent(out)   :: new_crack    !< total new nodes created (global, after uid sync)
          type(node_split_info), intent(inout), allocatable :: crack_info_list(:) !< split requests
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j, ii, k, p, minuid, min_ghost_k, n_owner_contrib
          integer, allocatable :: ghost_contrib_per_rank(:)
          logical :: locally_owned_shell, this_rank_created
          integer :: numnod0, numnodg0, old_max_uid
          integer :: local_new_count, total_new_nodes, local_n
          integer :: current_parent, current_owning_rank
          integer :: displ_arr(nspmd)
          integer :: empty_shells(0)
          integer :: empty_recv(0)
          integer, allocatable :: local_shells(:)
          integer, allocatable :: detached_nodes_local(:)
          integer, allocatable :: owning_ranks_local(:)
          integer, allocatable :: detached_nodes(:)
          integer, allocatable :: owning_ranks_global(:)
          integer, allocatable :: nb_detached_nodes(:), nb_detached_nodes_global(:)
          integer, allocatable :: permutation(:), processor(:), local_pos(:)
          logical, allocatable :: is_boundary_split(:)
          ! Phase 1.5: Parith/ON contributions_count gather
          integer :: n_pon_cracks_local, n_pon_cracks_total, kk, qr
          integer :: n_owner_contrib_pon   ! Parith/ON-specific recv slot count (may differ from NLOC)
          integer, allocatable :: pon_uid_local(:), pon_n_local(:)
          integer, allocatable :: pon_uid_global(:), pon_n_global(:), pon_rank_global(:)
          integer, allocatable :: pon_cnt(:), pon_cnt_global(:), pon_dsp(:)
          integer, allocatable :: pon_ghost_contrib_per_rank(:) ! Parith/ON ghost recv counts per rank
          integer, allocatable :: recv_pon_placeholder(:)       ! recv_procne for ghost placeholder Parith/ON slots
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          new_crack = 0
          if (.not. allocated(crack_info_list)) return

          numnod0 = numnod
          numnodg0 = numnodg
          local_new_count = 0
          allocate(detached_nodes_local(size(crack_info_list)))
          allocate(owning_ranks_local(size(crack_info_list)))
          detached_nodes_local = 0
          owning_ranks_local   = 0

          ! ---------------------------------------------------------------
          ! Phase 1: ownership determination (local, no MPI).
          ! All shell_uids entries are read (positive = local shell,
          ! negative = ghost shell local index). Ghost shells are only used
          ! for the min-uid race; they are NEVER passed to detach_node.
          ! ---------------------------------------------------------------
          do i = 1, size(crack_info_list)
            if (.not. allocated(crack_info_list(i)%shell_uids)) cycle
            if (size(crack_info_list(i)%shell_uids) == 0) cycle

            minuid = huge(0)
            min_ghost_k = 0
            locally_owned_shell = .false.

            do j = 1, size(crack_info_list(i)%shell_uids)
              if (crack_info_list(i)%shell_uids(j) < 0) then
                ! Ghost shell: encoded as negative local ghost-shell index
                if (element%ghost_shell%uid(-crack_info_list(i)%shell_uids(j)) < minuid) then
                  minuid = element%ghost_shell%uid(-crack_info_list(i)%shell_uids(j))
                  min_ghost_k = -crack_info_list(i)%shell_uids(j)
                  locally_owned_shell = .false.
                end if
              else if (crack_info_list(i)%shell_uids(j) > 0) then
                ! Local shell
                if (element%shell%user_id(crack_info_list(i)%shell_uids(j)) < minuid) then
                  minuid = element%shell%user_id(crack_info_list(i)%shell_uids(j))
                  locally_owned_shell = .true.
                end if
              end if
            end do

            if (locally_owned_shell) then
              crack_info_list(i)%weight = 1
              crack_info_list(i)%owning_rank = ispmd
            else
              crack_info_list(i)%weight = 0
              ! Find the sender rank: ghost shells from rank p-1 (0-based) are
              ! stored at ghost_shell%offset(p) .. ghost_shell%offset(p+1)-1
              crack_info_list(i)%owning_rank = -1
              do p = 1, nspmd
                if (min_ghost_k >= element%ghost_shell%offset(p) .and. &
                  min_ghost_k <  element%ghost_shell%offset(p+1)) then
                  crack_info_list(i)%owning_rank = p - 1
                  exit
                end if
              end do
            end if

          end do

          ! ---------------------------------------------------------------
          ! Phase 1.5: gather Parith/ON contributions_count from all ranks.
          !
          ! When a node N is split and a new node N' is created, the ADSKY
          ! (Parith/ON skyline) of N' on the OWNER rank gets one local FSKY
          ! row per shell corner at N' (contributions_count).  The GHOST
          ! rank that mirrors N' as N'' must create the same number of RECV
          ! FSKY rows so that REBUILD_PON_TABLES gives consistent FR_NBCC
          ! on both ranks.
          !
          ! Previously n_owner_contrib was estimated from ghost-copied
          ! shells (negative UIDs), which is ZERO when the owner's shells
          ! are not ghost-copied to the ghost rank (e.g. both ranks have
          ! independent local elements at the same boundary node).  This
          ! led to FR_NBCC(1,ghost+1) on owner > FR_NBCC(2,owner+1) on
          ! ghost → MPI buffer overflow in spmd_exch2_a_pon.
          !
          ! Fix: allgatherv actual corner counts so every rank knows the
          ! correct ADSKY recv-slot target before calling detach_node /
          ! mirror_node_split.
          ! ---------------------------------------------------------------
          n_pon_cracks_local = 0
          n_pon_cracks_total = 0
          if (nspmd > 1 .and. nodes%iparith > 0) then
            do i = 1, size(crack_info_list)
              if (.not. allocated(crack_info_list(i)%shell_uids)) cycle
              if (crack_info_list(i)%owning_rank == -1) cycle
              if (crack_info_list(i)%parent_id <= 0) cycle
              local_n = 0
              do j = 1, size(crack_info_list(i)%shell_uids)
                if (crack_info_list(i)%shell_uids(j) > 0) local_n = local_n + 1
              end do
              if (local_n > 0) n_pon_cracks_local = n_pon_cracks_local + 1
            end do

            allocate(pon_uid_local(max(1, n_pon_cracks_local)))
            allocate(pon_n_local(max(1, n_pon_cracks_local)))
            pon_uid_local = 0
            pon_n_local   = 0
            kk = 0
            do i = 1, size(crack_info_list)
              if (.not. allocated(crack_info_list(i)%shell_uids)) cycle
              if (crack_info_list(i)%owning_rank == -1) cycle
              if (crack_info_list(i)%parent_id <= 0) cycle
              local_n = 0
              do j = 1, size(crack_info_list(i)%shell_uids)
                if (crack_info_list(i)%shell_uids(j) > 0) local_n = local_n + 1
              end do
              if (local_n > 0) then
                kk = kk + 1
                pon_uid_local(kk) = crack_info_list(i)%parent_uid
                ! Count corners at parent_id in all local shells: mirrors the
                ! contributions_count that update_pon_shells will compute after
                ! detach_node_from_shells moves those corners to new_local_id.
                qr = 0
                do j = 1, size(crack_info_list(i)%shell_uids)
                  if (crack_info_list(i)%shell_uids(j) > 0) then
                    do k = 1, 4
                      if (element%shell%nodes(k, crack_info_list(i)%shell_uids(j)) == &
                        crack_info_list(i)%parent_id) qr = qr + 1
                    end do
                  end if
                end do
                pon_n_local(kk) = qr
                ! DEBUG: print Phase 1.5 contributions for key parents
!                if (crack_info_list(i)%parent_uid >= 10711 .and. &
!                  crack_info_list(i)%parent_uid <= 10711) then
!                  write(6,'(a,i0,a,i0,a,i0,a,i0)') &
!                    '[DBG_P15] r=', ispmd, &
!                    ' uid=', crack_info_list(i)%parent_uid, &
!                    ' local_n=', local_n, ' qr=', qr
!                  call flush(6)
!                end if
              end if
            end do

            ! Gather counts-per-rank via allreduce(SUM) of a rank-indexed array
            allocate(pon_cnt(nspmd))
            allocate(pon_cnt_global(nspmd))
            pon_cnt = 0
            pon_cnt(ispmd + 1) = n_pon_cracks_local
            call spmd_allreduce(pon_cnt, pon_cnt_global, nspmd, SPMD_SUM)
            deallocate(pon_cnt)

            allocate(pon_dsp(nspmd + 1))
            pon_dsp(1) = 0
            do p = 1, nspmd
              pon_dsp(p + 1) = pon_dsp(p) + pon_cnt_global(p)
            end do
            n_pon_cracks_total = pon_dsp(nspmd + 1)

            allocate(pon_uid_global(max(1, n_pon_cracks_total)))
            allocate(pon_n_global(max(1, n_pon_cracks_total)))
            allocate(pon_rank_global(max(1, n_pon_cracks_total)))
            pon_uid_global  = 0
            pon_n_global    = 0
            pon_rank_global = 0
            call spmd_allgatherv(pon_uid_local, n_pon_cracks_local, &
              pon_uid_global, pon_cnt_global, pon_dsp)
            call spmd_allgatherv(pon_n_local, n_pon_cracks_local, &
              pon_n_global, pon_cnt_global, pon_dsp)
            ! Annotate each gathered entry with the rank it came from (0-based)
            do p = 1, nspmd
              do kk = pon_dsp(p) + 1, pon_dsp(p + 1)
                pon_rank_global(kk) = p - 1
              end do
            end do
            deallocate(pon_uid_local, pon_n_local, pon_cnt_global, pon_dsp)
          end if

          ! ---------------------------------------------------------------
          ! Phase 2: local splits.
          ! weight=1 rank: full detach_node (owns new node N').
          ! weight=0 rank with local shells: mirror_node_split creates a
          !   ghost copy of N' with MAIN_PROC=owning_rank so boundary
          !   exchange can propagate N' data each time step.
          ! Only LOCAL (positive) shell_uids are forwarded — ghost shell
          ! indices must NEVER reach detach_node_from_shells.
          ! ---------------------------------------------------------------
          do i = 1, size(crack_info_list)
            if (.not. allocated(crack_info_list(i)%shell_uids)) cycle
            if (crack_info_list(i)%owning_rank == -1) cycle

            ! Count local (positive) shells going to N'
            local_n = 0
            do j = 1, size(crack_info_list(i)%shell_uids)
              if (crack_info_list(i)%shell_uids(j) > 0) local_n = local_n + 1
            end do

            if (local_n == 0) then
              ! No local shells on this rank for this node.
              ! If the parent is locally known and the owning rank is determined,
              ! create a ghost placeholder for N' so that after init_ghost_shells,
              ! ghost shells from the owning rank that reference the new UID can
              ! be resolved locally (positive local ID -> included in cnel for
              ! non-local damage). Without this placeholder, the new UID would be
              ! stored as a negative ID and excluded from the non-local neighbourhood,
              ! causing slowly-diverging physics in MPI vs 1-rank runs.
              if (crack_info_list(i)%parent_id > 0 .and. &
                crack_info_list(i)%owning_rank /= -1) then
                call extend_nodal_arrays(nodes, nodes%numnod + 1)
                call set_new_node_values(nodes, crack_info_list(i)%parent_id)
                nodes%MAIN_PROC(nodes%numnod + 1) = crack_info_list(i)%owning_rank+1
                nodes%WEIGHT(nodes%numnod + 1) = 0
                nodes%numnod = nodes%numnod + 1
                numnod = nodes%numnod
                ! Keep elements%pon%sadsky in sync with nodes%numnod so that
                ! subsequent update_pon_shells calls do not read adsky out of bounds.
                ! Ghost placeholder has 0 local shells but needs Parith/ON recv rows
                ! matching the owner's local contribution count so REBUILD_PON_TABLES
                ! gives consistent FR_NBCC (avoiding buffer overflow in spmd_exch2_a_pon).
                if (nodes%iparith > 0) then
                  n_owner_contrib_pon = 0
                  if (n_pon_cracks_total > 0) then
                    do kk = 1, n_pon_cracks_total
                      if (pon_uid_global(kk) == crack_info_list(i)%parent_uid .and. &
                        pon_rank_global(kk) == crack_info_list(i)%owning_rank) then
                        n_owner_contrib_pon = pon_n_global(kk)
                        exit
                      end if
                    end do
                  end if
                  if (n_owner_contrib_pon > 0) then
                    allocate(recv_pon_placeholder(n_owner_contrib_pon))
                    recv_pon_placeholder = crack_info_list(i)%owning_rank + 1
                    call update_pon_shells(nodes%itab(crack_info_list(i)%parent_id), element, 0, empty_shells, numnod, ispmd, &
                      n_owner_contrib_pon, recv_pon_placeholder)
                    deallocate(recv_pon_placeholder)
                  else
                    call update_pon_shells(nodes%itab(crack_info_list(i)%parent_id), element, 0, empty_shells, numnod, ispmd, 0, empty_recv)
                  end if
                end if
                ! Update the non-local damage structure for the ghost placeholder node.
                ! detach_node_nloc handles IDXI extension (Step 1) and — when the parent is
                ! a NLOC node — creates a new non-local DOF entry for N'' with the correct
                ! number of REMOTE recv rows (= n_owner_contrib_pon = owner's corner count).
                ! This makes SPMD_SUB_BOUNDARIES give a symmetric NLOC exchange size so that
                ! the owner's NLOC force contributions for N' are correctly received by rank R.
                if (nloc_dmg%imod > 0) then
                  call detach_node_nloc(nloc_dmg, crack_info_list(i)%parent_id, numnod, &
                    element, empty_shells, 0, numnod - 1, nthread, ispmd, nspmd, &
                    is_mirror=.true., n_owner_contrib=n_owner_contrib_pon)
                end if
                local_new_count = local_new_count + 1
                detached_nodes_local(local_new_count) = crack_info_list(i)%parent_uid
                owning_ranks_local(local_new_count)   = crack_info_list(i)%owning_rank
              end if
              cycle
            end if

            ! Build local-only shell list
            allocate(local_shells(local_n))
            local_n = 0
            do j = 1, size(crack_info_list(i)%shell_uids)
              if (crack_info_list(i)%shell_uids(j) > 0) then
                local_n = local_n + 1
                local_shells(local_n) = crack_info_list(i)%shell_uids(j)
              end if
            end do
            ! Sort local_shells by shell user_id (ascending) to ensure bitwise
            ! Parith/ON reproducibility between MONO and MPI.
            ! In MPI the owner rank's local shells are placed first (ADDCNE CC_0,
            ! CC_1...) and remote contributions follow as RECV rows (step 8f).
            ! The owner rank has the globally smallest user_id shell, so sorting
            ! by user_id puts the owner's shells first, matching the MPI layout.
            do j = 2, local_n
              do k = j, 2, -1
                if (element%shell%user_id(local_shells(k)) < &
                  element%shell%user_id(local_shells(k-1))) then
                  p = local_shells(k)
                  local_shells(k) = local_shells(k-1)
                  local_shells(k-1) = p
                else
                  exit
                end if
              end do
            end do

            if (crack_info_list(i)%weight == 1) then
              ! Count ghost shells per rank (negative UIDs) — kept for NLOC usage.
              n_owner_contrib = 0
              allocate(ghost_contrib_per_rank(0:nspmd-1))
              ghost_contrib_per_rank = 0
              do j = 1, size(crack_info_list(i)%shell_uids)
                if (crack_info_list(i)%shell_uids(j) < 0) then
                  n_owner_contrib = n_owner_contrib + 1
                  k = -crack_info_list(i)%shell_uids(j)
                  do p = 1, nspmd
                    if (k >= element%ghost_shell%offset(p) .and. &
                      k <  element%ghost_shell%offset(p+1)) then
                      ghost_contrib_per_rank(p-1) = ghost_contrib_per_rank(p-1) + 1
                      exit
                    end if
                  end do
                end if
              end do
              ! Build Parith/ON-specific ghost recv counts from communicated corner counts.
              ! ghost_contrib_per_rank is kept as-is for NLOC (detach_node_nloc); the new
              ! pon_ghost_contrib_per_rank is passed separately for the Parith/ON ADSKY slots
              ! so that REBUILD_PON_TABLES sees consistent FR_NBCC on owner and ghost.
              if (n_pon_cracks_total > 0) then
                allocate(pon_ghost_contrib_per_rank(0:nspmd-1))
                pon_ghost_contrib_per_rank = 0
                do kk = 1, n_pon_cracks_total
                  if (pon_uid_global(kk) == crack_info_list(i)%parent_uid .and. &
                    pon_rank_global(kk) /= ispmd) then
                    pon_ghost_contrib_per_rank(pon_rank_global(kk)) = pon_n_global(kk)
                  end if
                end do
                ! DEBUG: print pon_ghost_contrib for key parents to trace n_ghost=0 issue
!                if (crack_info_list(i)%parent_uid >= 10711 .and. &
!                  crack_info_list(i)%parent_uid <= 10711) then
!                  write(6,'(a,i0,a,i0,a,i0,a,8i3)') &
!                    '[DBG_PON1] r=', ispmd, &
!                    ' uid=', crack_info_list(i)%parent_uid, &
!                    ' ntot=', n_pon_cracks_total, &
!                    ' pgc=', pon_ghost_contrib_per_rank(0:min(7,nspmd-1))
!                  do kk = 1, n_pon_cracks_total
!                    if (pon_uid_global(kk) == crack_info_list(i)%parent_uid) then
!                      write(6,'(a,i0,a,i0,a,i0,a,i0,a,i0)') &
!                        '[DBG_PON1_KK] r=', ispmd, &
!                        ' uid=', crack_info_list(i)%parent_uid, &
!                        ' kk=', kk, &
!                        ' pon_rank=', pon_rank_global(kk), &
!                        ' pon_n=', pon_n_global(kk)
!                    end if
!                  end do
!                  call flush(6)
!                end if
                call detach_node(nodes, crack_info_list(i)%parent_id, element, &
                  local_shells, local_n, &
                  npari, ninter, ipari, interf, nloc_dmg, nthread, nspmd, ispmd, &
                  n_owner_contrib, &
                  ghost_contrib_per_rank, pon_ghost_contrib_per_rank)
                deallocate(pon_ghost_contrib_per_rank)
              else
                call detach_node(nodes, crack_info_list(i)%parent_id, element, &
                  local_shells, local_n, &
                  npari, ninter, ipari, interf, nloc_dmg, nthread, nspmd, ispmd, &
                  n_owner_contrib, &
                  ghost_contrib_per_rank)
              end if
              deallocate(ghost_contrib_per_rank)
            else
              ! Compute NLOC n_owner_contrib (count of ghost-copied owner shells).
              ! This is used for detach_node_nloc on the mirror (ghost) side.
              n_owner_contrib = 0
              do j = 1, size(crack_info_list(i)%shell_uids)
                if (crack_info_list(i)%shell_uids(j) < 0) n_owner_contrib = n_owner_contrib + 1
              end do
              ! Compute Parith/ON n_owner_contrib_pon from communicated corner count.
              ! Kept separate from the NLOC n_owner_contrib so detach_node_nloc keeps
              ! the old (ghost-copy based) count while only Parith/ON ADSKY gets the fix.
              n_owner_contrib_pon = n_owner_contrib  ! default: same as NLOC
              if (n_pon_cracks_total > 0) then
                do kk = 1, n_pon_cracks_total
                  if (pon_uid_global(kk) == crack_info_list(i)%parent_uid .and. &
                    pon_rank_global(kk) == crack_info_list(i)%owning_rank) then
                    n_owner_contrib_pon = pon_n_global(kk)
                    exit
                  end if
                end do
              end if
              call mirror_node_split(nodes, crack_info_list(i)%parent_id, element, &
                local_shells, local_n, &
                nloc_dmg, nthread, ispmd, nspmd, crack_info_list(i)%owning_rank, &
                n_owner_contrib, n_owner_contrib_pon)
            end if

            numnod = nodes%numnod
            local_new_count = local_new_count + 1
            detached_nodes_local(local_new_count) = crack_info_list(i)%parent_uid
            owning_ranks_local(local_new_count)   = crack_info_list(i)%owning_rank

            deallocate(local_shells)
          end do

          ! Clean up Phase 1.5 allocatables
          if (allocated(pon_uid_global))  deallocate(pon_uid_global)
          if (allocated(pon_n_global))    deallocate(pon_n_global)
          if (allocated(pon_rank_global)) deallocate(pon_rank_global)

          ! ---------------------------------------------------------------
          ! Phase 3: global gather of (parent_uid, owning_rank) pairs.
          ! Both creating ranks (weight=1 and weight=0 with local shells)
          ! contribute, so the same parent may appear from multiple ranks.
          ! ---------------------------------------------------------------
          allocate(nb_detached_nodes(nspmd))
          allocate(nb_detached_nodes_global(nspmd))
          nb_detached_nodes = 0
          nb_detached_nodes_global = 0
          nb_detached_nodes(ispmd + 1) = local_new_count
          if (nspmd > 1) then
            call spmd_allreduce(nb_detached_nodes, nb_detached_nodes_global, nspmd, SPMD_SUM)
          else
            nb_detached_nodes_global = nb_detached_nodes
          end if

          total_new_nodes = sum(nb_detached_nodes_global(1:nspmd))

          allocate(detached_nodes(max(total_new_nodes, 1)))
          allocate(owning_ranks_global(max(total_new_nodes, 1)))
          detached_nodes      = 0
          owning_ranks_global = 0
          displ_arr(1:nspmd) = 0
          do i = 2, nspmd
            displ_arr(i) = displ_arr(i-1) + nb_detached_nodes_global(i-1)
          end do

          if (nspmd > 1) then
            call spmd_allgatherv(detached_nodes_local, local_new_count, &
              detached_nodes, nb_detached_nodes_global, displ_arr)
            call spmd_allgatherv(owning_ranks_local, local_new_count, &
              owning_ranks_global, nb_detached_nodes_global, displ_arr)
          else
            if (total_new_nodes > 0) then
              detached_nodes(1:total_new_nodes)      = detached_nodes_local(1:total_new_nodes)
              owning_ranks_global(1:total_new_nodes) = owning_ranks_local(1:total_new_nodes)
            end if
          end if

          ! Consistent global base values (same on all ranks after allreduce)
          if (nspmd > 1) then
            call spmd_allreduce(numnodg0, p, 1, SPMD_MAX)
            numnodg0 = p
            call spmd_allreduce(nodes%max_uid, old_max_uid, 1, SPMD_MAX)
          else
            old_max_uid = nodes%max_uid
          end if

          ! ---------------------------------------------------------------
          ! Phase 4: de-duplicated UID assignment.
          ! stlsort_int_int sorts detached_nodes in place (ascending) and
          ! sets permutation(ii) = original index of the ii-th sorted entry.
          ! Entries with the same parent_uid form a group (one physical split
          ! across multiple ranks) and receive a single new uid.
          ! new_crack counts unique splits (groups), NOT total entries.
          ! ---------------------------------------------------------------
          if (total_new_nodes > 0) then
            k = total_new_nodes
            allocate(processor(k))
            allocate(local_pos(k))
            allocate(is_boundary_split(k))
            k = 0
            do p = 1, nspmd
              do i = 1, nb_detached_nodes_global(p)
                k = k + 1
                processor(k) = p
                if (ispmd + 1 == p) then
                  local_pos(k) = i
                else
                  local_pos(k) = 0
                end if
              end do
            end do

            k = total_new_nodes
            allocate(permutation(k))
            do i = 1, k
              permutation(i) = i
            end do
            call stlsort_int_int(k, detached_nodes, permutation)
            ! After this call:
            !   detached_nodes(ii)  = ii-th smallest parent_uid  (sorted in place)
            !   permutation(ii)     = original index of that entry
            !   owning_ranks_global not sorted; access as owning_ranks_global(permutation(ii))

            ! Pre-pass: mark entries that share a parent_uid with another rank.
            ! Adjacent equal values in the sorted array mean a boundary-node split.
            is_boundary_split = .false.
            do ii = 1, k - 1
              if (detached_nodes(ii) == detached_nodes(ii + 1)) then
                is_boundary_split(ii)     = .true.
                is_boundary_split(ii + 1) = .true.
              end if
            end do

            current_parent      = -1
            current_owning_rank = -1
            this_rank_created   = .false.

            do ii = 1, k
              i = permutation(ii)  ! original index

              if (detached_nodes(ii) /= current_parent) then
                ! Finalise the previous group: halve ms on non-creating ranks
                if (current_parent >= 0 .and. .not. this_rank_created) then
                  j = get_local_node_id(nodes, current_parent)
                  if (j > 0) then
                    nodes%ms(j)  = nodes%ms(j)  / 2.0_wp
                    nodes%ms0(j) = nodes%ms0(j) / 2.0_wp
                    nodes%nchilds(nodes%parent_node(j)) = &
                      nodes%nchilds(nodes%parent_node(j)) + 1

                  end if
                end if
                ! Open a new group
                old_max_uid         = old_max_uid + 1
                numnodg0            = numnodg0    + 1
                new_crack           = new_crack   + 1
                current_parent      = detached_nodes(ii)
                current_owning_rank = owning_ranks_global(i)
                this_rank_created   = .false.
              end if

              p = processor(i)
              if (p == ispmd + 1) then
!                write(6,*) 'New node created on rank ', ispmd, ' for parent_uid=', current_parent, &
!                  ' new_uid=', old_max_uid, ' owning_rank=', current_owning_rank
                ! This rank created a local N' for this parent
                this_rank_created = .true.
                j = local_pos(i)
                nodes%itab(numnod0 + j)         = old_max_uid
                nodes%itabm1(numnod0 + j)        = old_max_uid
                nodes%itabm1(2*(numnod0 + j))    = numnod0 + j
                ! nodglob is meaningful on every rank that holds N'
                ! (same global index regardless of ownership)
                nodes%nodglob(numnod0 + j) = numnodg0
                nodes%main_proc(numnod0 + j) = current_owning_rank+1

              end if
            end do

            ! Finalise the last group
            if (current_parent >= 0 .and. .not. this_rank_created) then
              j = get_local_node_id(nodes, current_parent)
              if (j > 0) then
                nodes%ms(j)  = nodes%ms(j)  / 2.0_wp
                nodes%ms0(j) = nodes%ms0(j) / 2.0_wp
                nodes%nchilds(nodes%parent_node(j)) = &
                  nodes%nchilds(nodes%parent_node(j)) + 1

              end if
            end if

            nodes%max_uid = old_max_uid
            numnodg = numnodg0

            if (allocated(permutation))       deallocate(permutation)
            if (allocated(processor))         deallocate(processor)
            if (allocated(local_pos))         deallocate(local_pos)
            if (allocated(is_boundary_split)) deallocate(is_boundary_split)
          end if

          ! Debug summary
!          write(*,'(a,i0,a,i0,a,i0,a,i0)') &
!            '[SPLIT][rank ', ispmd, '] DONE: new_crack=', new_crack, &
!            ' numnod=', numnod, ' numnodg=', numnodg
!          flush(6)

          if (allocated(nb_detached_nodes))        deallocate(nb_detached_nodes)
          if (allocated(nb_detached_nodes_global))  deallocate(nb_detached_nodes_global)
          if (allocated(detached_nodes_local))      deallocate(detached_nodes_local)
          if (allocated(owning_ranks_local))        deallocate(owning_ranks_local)
          if (allocated(detached_nodes))            deallocate(detached_nodes)
          if (allocated(owning_ranks_global))       deallocate(owning_ranks_global)

        end subroutine apply_crack

      end module apply_crack_mod
