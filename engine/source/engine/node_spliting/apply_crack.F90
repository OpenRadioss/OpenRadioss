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
!||    apply_crack_mod     ../engine/source/engine/node_spliting/apply_crack.F90
!||--- called by ------------------------------------------------------
!||    nloc_shell_detach   ../engine/source/engine/node_spliting/nloc_shell_detach.F90
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

!! \brief Area of a 4-node shell from the current nodal coordinates.
!! \details Sum of the two triangles (n1,n2,n3) and (n1,n3,n4).  Degenerated quads
!!          (triangles stored with node4 = node3) get a zero second triangle, so the
!!          formula is valid for both true quads and degenerated ones.
!||====================================================================
!||    shell_area4     ../engine/source/engine/node_spliting/apply_crack.F90
!||--- called by ------------------------------------------------------
!||    apply_crack     ../engine/source/engine/node_spliting/apply_crack.F90
!||--- uses       -----------------------------------------------------
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        function shell_area4(x, n1, n2, n3, n4) result(area)
          use precision_mod, only: wp
          implicit none
          real(kind=wp), intent(in) :: x(:,:)   !< nodal coordinates (3, numnod)
          integer,       intent(in) :: n1, n2, n3, n4 !< corner node local ids
          real(kind=wp) :: area
          real(kind=wp) :: u(3), v(3), c(3)
          u = x(1:3, n2) - x(1:3, n1)
          v = x(1:3, n3) - x(1:3, n1)
          c(1) = u(2)*v(3) - u(3)*v(2)
          c(2) = u(3)*v(1) - u(1)*v(3)
          c(3) = u(1)*v(2) - u(2)*v(1)
          area = 0.5_wp * sqrt(c(1)*c(1) + c(2)*c(2) + c(3)*c(3))
          u = x(1:3, n3) - x(1:3, n1)
          v = x(1:3, n4) - x(1:3, n1)
          c(1) = u(2)*v(3) - u(3)*v(2)
          c(2) = u(3)*v(1) - u(1)*v(3)
          c(3) = u(1)*v(2) - u(2)*v(1)
          area = area + 0.5_wp * sqrt(c(1)*c(1) + c(2)*c(2) + c(3)*c(3))
        end function shell_area4

!! \brief Area-weighted share of the parent nodal mass migrating to the new node.
!! \details Enumerates every shell attached to the parent — local shells by scanning
!!          the connectivity, remote shells through their ghost copies — and weights
!!          each by (number of corners at the parent) x (shell area), which is the
!!          lumped-mass contribution pattern of a 4-node shell (area/4 per corner, the
!!          common 1/4 cancels).  Returns migrating_area / total_area, clamped away
!!          from 0 and 1 so neither side of the crack is left massless.
!!
!!          Bitwise MPI/mono consistency: every shell area is computed once on the
!!          shell's home rank (ghost copies receive the home value through
!!          spmd_exchange_ghost_shells), and the sums run in ascending shell-user-id
!!          order, so all ranks holding the parent — and a 1-rank run — compute the
!!          exact same fraction.
!||====================================================================
!||    split_mass_fraction   ../engine/source/engine/node_spliting/apply_crack.F90
!||--- called by ------------------------------------------------------
!||    apply_crack           ../engine/source/engine/node_spliting/apply_crack.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod      ../common_source/modules/connectivity.F90
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||====================================================================
        function split_mass_fraction(element, parent_id, n_uids, shell_uids, &
          numelc, shell_area, nghost, ghost_area) result(w)
          use connectivity_mod
          use precision_mod, only: wp
          implicit none
          type(connectivity_), intent(in) :: element     !< element connectivity (pre-split state)
          integer,             intent(in) :: parent_id   !< local id of the node to split
          integer,             intent(in) :: n_uids      !< size of shell_uids
          integer,             intent(in) :: shell_uids(n_uids) !< signed shells migrating to N' (>0 local, <0 ghost index)
          integer,             intent(in) :: numelc      !< number of local shells
          real(kind=wp),       intent(in) :: shell_area(0:numelc) !< per-local-shell area
          integer,             intent(in) :: nghost      !< number of ghost shells
          real(kind=wp),       intent(in) :: ghost_area(*) !< per-ghost-shell area (from the home rank)
          real(kind=wp) :: w

          integer :: e, g, j, k, n_att, cnt, uid, tmp_uid
          integer, allocatable :: att_uid(:)
          real(kind=wp), allocatable :: att_area(:)
          logical, allocatable :: att_mig(:)
          real(kind=wp) :: tmp_area, a_tot, a_mig
          logical :: tmp_mig
          real(kind=wp), parameter :: w_min = 0.01_wp, w_max = 0.99_wp

          w = 0.5_wp

          ! Count the shells attached to the parent (local + ghost)
          n_att = 0
          do e = 1, numelc
            if (any(element%shell%nodes(1:4, e) == parent_id)) n_att = n_att + 1
          end do
          do g = 1, nghost
            if (any(element%ghost_shell%nodes(1:4, g) == parent_id)) n_att = n_att + 1
          end do
          if (n_att == 0) return

          allocate(att_uid(n_att), att_area(n_att), att_mig(n_att))
          k = 0
          do e = 1, numelc
            cnt = 0
            do j = 1, 4
              if (element%shell%nodes(j, e) == parent_id) cnt = cnt + 1
            end do
            if (cnt > 0) then
              k = k + 1
              att_uid(k)  = element%shell%user_id(e)
              att_area(k) = real(cnt, wp) * shell_area(e)
              att_mig(k)  = .false.
            end if
          end do
          do g = 1, nghost
            cnt = 0
            do j = 1, 4
              if (element%ghost_shell%nodes(j, g) == parent_id) cnt = cnt + 1
            end do
            if (cnt > 0) then
              k = k + 1
              att_uid(k)  = element%ghost_shell%uid(g)
              att_area(k) = real(cnt, wp) * ghost_area(g)
              att_mig(k)  = .false.
            end if
          end do

          ! Mark the migrating shells (positive = local shell id, negative = ghost index)
          do j = 1, n_uids
            if (shell_uids(j) > 0) then
              uid = element%shell%user_id(shell_uids(j))
            else if (shell_uids(j) < 0) then
              uid = element%ghost_shell%uid(-shell_uids(j))
            else
              cycle
            end if
            do k = 1, n_att
              if (att_uid(k) == uid) then
                att_mig(k) = .true.
                exit
              end if
            end do
          end do

          ! Insertion sort by user id: the summation order must not depend on the
          ! local/ghost storage layout of each rank
          do k = 2, n_att
            do j = k, 2, -1
              if (att_uid(j) < att_uid(j-1)) then
                tmp_uid = att_uid(j);   att_uid(j) = att_uid(j-1);   att_uid(j-1) = tmp_uid
                tmp_area = att_area(j); att_area(j) = att_area(j-1); att_area(j-1) = tmp_area
                tmp_mig = att_mig(j);   att_mig(j) = att_mig(j-1);   att_mig(j-1) = tmp_mig
              else
                exit
              end if
            end do
          end do

          a_tot = 0.0_wp
          a_mig = 0.0_wp
          do k = 1, n_att
            a_tot = a_tot + att_area(k)
            if (att_mig(k)) a_mig = a_mig + att_area(k)
          end do

          if (a_tot > 0.0_wp) w = min(max(a_mig / a_tot, w_min), w_max)
          deallocate(att_uid, att_area, att_mig)
        end function split_mass_fraction

!! \brief Canonical /PARITH/ON contribution order for a split node N'.
!! \details Each migrating shell contributes exactly one skyline (FSKY) row to N'.  For
!!          bitwise /PARITH/ON the rows of N''s band must be in a GLOBAL,
!!          decomposition-independent order.  shell_uids holds the full migrating set
!!          (identical on every rank holding the parent): local shells (>0) are owned by
!!          ispmd, ghost shells (<0) by the rank whose ghost_shell%offset range contains
!!          them.  Sorting the (uid, owner) pairs by user id yields the same band layout on
!!          every rank AND equal to the 1-rank (mono) order.  row_procne(k) is the 1-based
!!          owning rank of row k: a rank builds row k as a LOCAL row when
!!          row_procne(k)==ispmd+1, otherwise as a RECV row.  Used by BOTH the mechanical
!!          (update_pon_shells) and non-local (detach_node_nloc) skylines.
!||====================================================================
!||    build_contrib_order   ../engine/source/engine/node_spliting/apply_crack.F90
!||--- called by ------------------------------------------------------
!||    apply_crack           ../engine/source/engine/node_spliting/apply_crack.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod      ../common_source/modules/connectivity.F90
!||====================================================================
        subroutine build_contrib_order(element, n_uids, shell_uids, ispmd, nspmd, &
          m, row_uid, row_procne)
          use connectivity_mod
          implicit none
          type(connectivity_), intent(in)  :: element
          integer,             intent(in)  :: n_uids
          integer,             intent(in)  :: shell_uids(n_uids)
          integer,             intent(in)  :: ispmd
          integer,             intent(in)  :: nspmd
          integer,             intent(out) :: m
          integer, allocatable, intent(out) :: row_uid(:)
          integer, allocatable, intent(out) :: row_procne(:)
          integer :: a, g, p, j, tu, tp

          m = n_uids
          allocate(row_uid(max(1, m)), row_procne(max(1, m)))
          row_uid = 0
          row_procne = 0
          do a = 1, m
            if (shell_uids(a) > 0) then
              row_uid(a)    = element%shell%user_id(shell_uids(a))
              row_procne(a) = ispmd + 1
            else if (shell_uids(a) < 0) then
              g = -shell_uids(a)
              row_uid(a)    = element%ghost_shell%uid(g)
              row_procne(a) = ispmd + 1   ! defensive fallback (should be overwritten)
              do p = 1, nspmd
                if (g >= element%ghost_shell%offset(p) .and. &
                  g <  element%ghost_shell%offset(p+1)) then
                  row_procne(a) = p        ! ghost from rank p-1 -> procne = (p-1)+1 = p
                  exit
                end if
              end do
            end if
          end do

          ! Insertion sort by user id (ascending): decomposition-independent order.
          do a = 2, m
            do j = a, 2, -1
              if (row_uid(j) < row_uid(j-1)) then
                tu = row_uid(j);    row_uid(j)    = row_uid(j-1);    row_uid(j-1)    = tu
                tp = row_procne(j); row_procne(j) = row_procne(j-1); row_procne(j-1) = tp
              else
                exit
              end if
            end do
          end do
        end subroutine build_contrib_order

!! \brief Apply the parent side of a mass split on a rank that holds the parent
!!        node but did not create the new node.
!! \details Mirrors what set_new_node_values / detach_node_nloc do on the creating
!!          ranks: the parent keeps (1 - w) of its mass, rotational inertia,
!!          assembled force/moment and non-local mass.  Every rank holding the
!!          parent must apply the same factor so all MPI copies stay consistent.
!||====================================================================
!||    scale_parent_on_noncreating_rank   ../engine/source/engine/node_spliting/apply_crack.F90
!||--- called by ------------------------------------------------------
!||    apply_crack                        ../engine/source/engine/node_spliting/apply_crack.F90
!||--- uses       -----------------------------------------------------
!||    nlocal_reg_mod                     ../common_source/modules/nlocal_reg_mod.F
!||    nodal_arrays_mod                   ../common_source/modules/nodal_arrays.F90
!||    precision_mod                      ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine scale_parent_on_noncreating_rank(nodes, nloc_dmg, parent_uid, w)
          use nodal_arrays_mod
          use nlocal_reg_mod
          use precision_mod, only: wp
          implicit none
          type(nodal_arrays_), intent(inout) :: nodes      !< nodal arrays
          type(nlocal_str_),   intent(inout) :: nloc_dmg   !< non-local damage structure
          integer,             intent(in)    :: parent_uid !< user id of the split parent node
          real(kind=wp),       intent(in)    :: w          !< share taken by the (remote) new node
          integer :: j, nl_idx, nl_pos, nl_nddl

          j = get_local_node_id(nodes, parent_uid)
          if (j <= 0) return

          nodes%ms(j)  = nodes%ms(j)  * (1.0_wp - w)
          nodes%ms0(j) = nodes%ms0(j) * (1.0_wp - w)
          nodes%A(1:3,j)  = nodes%A(1:3,j)  * (1.0_wp - w)
          nodes%AR(1:3,j) = nodes%AR(1:3,j) * (1.0_wp - w)
          if (nodes%iroddl > 0) then
            nodes%IN(j)  = nodes%IN(j)  * (1.0_wp - w)
            nodes%IN0(j) = nodes%IN0(j) * (1.0_wp - w)
          end if
#ifdef MYREAL4
          if (nodes%iparith == 0) then
            nodes%ACC_DP(1:3,j) = nodes%ACC_DP(1:3,j) * (1.0_wp - w)
          end if
#endif
          if (nloc_dmg%imod > 0) then
            nl_idx = nloc_dmg%idxi(j)
            if (nl_idx > 0) then
              nl_pos  = nloc_dmg%posi(nl_idx)
              nl_nddl = nloc_dmg%posi(nl_idx + 1) - nl_pos
              nloc_dmg%mass (nl_pos:nl_pos+nl_nddl-1) = nloc_dmg%mass (nl_pos:nl_pos+nl_nddl-1) * (1.0_wp - w)
              nloc_dmg%mass0(nl_pos:nl_pos+nl_nddl-1) = nloc_dmg%mass0(nl_pos:nl_pos+nl_nddl-1) * (1.0_wp - w)
            end if
          end if

          nodes%nchilds(nodes%parent_node(j)) = nodes%nchilds(nodes%parent_node(j)) + 1
        end subroutine scale_parent_on_noncreating_rank

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
!||    apply_crack                        ../engine/source/engine/node_spliting/apply_crack.F90
!||--- called by ------------------------------------------------------
!||    nloc_shell_detach                  ../engine/source/engine/node_spliting/nloc_shell_detach.F90
!||--- calls      -----------------------------------------------------
!||    build_contrib_order                ../engine/source/engine/node_spliting/apply_crack.F90
!||    detach_node                        ../engine/source/engine/node_spliting/detach_node.F90
!||    detach_node_nloc                   ../engine/source/engine/node_spliting/detach_node_nloc.F90
!||    extend_nodal_arrays                ../common_source/modules/nodal_arrays.F90
!||    mirror_node_split                  ../engine/source/engine/node_spliting/detach_node.F90
!||    scale_parent_on_noncreating_rank   ../engine/source/engine/node_spliting/apply_crack.F90
!||    set_new_node_values                ../engine/source/engine/node_spliting/detach_node.F90
!||    shell_area4                        ../engine/source/engine/node_spliting/apply_crack.F90
!||    split_mass_fraction                ../engine/source/engine/node_spliting/apply_crack.F90
!||    spmd_exchange_ghost_shells         ../engine/source/engine/node_spliting/ghost_shells.F90
!||    stlsort_int_int                    ../common_source/tools/sort/cppsort.cpp
!||    update_pon_shells                  ../engine/source/engine/node_spliting/update_pon.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod                   ../common_source/modules/connectivity.F90
!||    detach_node_mod                    ../engine/source/engine/node_spliting/detach_node.F90
!||    detach_node_nloc_mod               ../engine/source/engine/node_spliting/detach_node_nloc.F90
!||    extend_array_mod                   ../common_source/tools/memory/extend_array.F90
!||    ghost_shells_mod                   ../engine/source/engine/node_spliting/ghost_shells.F90
!||    interfaces_mod                     ../common_source/modules/interfaces/interfaces_mod.F90
!||    nlocal_reg_mod                     ../common_source/modules/nlocal_reg_mod.F
!||    nodal_arrays_mod                   ../common_source/modules/nodal_arrays.F90
!||    precision_mod                      ../common_source/modules/precision_mod.F90
!||    spmd_mod                           ../engine/source/mpi/spmd_mod.F90
!||    update_pon_mod                     ../engine/source/engine/node_spliting/update_pon.F90
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
          use ghost_shells_mod, only : spmd_exchange_ghost_shells
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
          integer :: m_c                                   ! canonical N' contribution count (all migrating shells)
          integer, allocatable :: row_uid_c(:)             ! shell user id of each canonical row (uid-sorted)
          integer, allocatable :: row_procne_c(:)          ! 1-based owning rank of each canonical row
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
          ! Phase 1.6: area-weighted mass split fraction
          integer :: numelc_loc, nghost
          real(kind=wp), allocatable :: shell_area(:)      ! (0:numelc) per-local-shell area
          real(kind=wp), allocatable :: ghost_shell_area(:) ! per-ghost-shell area (home-rank value)
          real(kind=wp), allocatable :: w_split(:)         ! mass fraction per crack record
          real(kind=wp), allocatable :: w_local_arr(:), w_global_arr(:) ! Phase 3 gather of w
          real(kind=wp) :: current_w                       ! w of the Phase 4/5 group being processed
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
          allocate(w_local_arr(size(crack_info_list)))
          detached_nodes_local = 0
          owning_ranks_local   = 0
          w_local_arr          = 0.5_wp

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
          ! [FANDUMP] diagnostic — for every split record, print the full
          ! migrating fan with each contribution's global uid and owning rank,
          ! on every rank.  A boundary (cross-rank) split shows up as the same
          ! parent_uid on >=2 ranks (weight=1 on the owner, 0 on mirrors) with
          ! mixed owners in its fan.  The PARITH/ON latent bug needs a fan with
          ! >=3 contributions whose uids interleave across ranks (the owner does
          ! NOT hold a contiguous uid prefix).  Kept as a regression aid.
          ! ---------------------------------------------------------------
          block
            integer :: fj, fs, fuid, fowner, fp
            do i = 1, size(crack_info_list)
              if (.not. allocated(crack_info_list(i)%shell_uids)) cycle
              if (size(crack_info_list(i)%shell_uids) == 0) cycle
              write(6,'(a,i0,a,i0,a,i0,a,i0,a,i0,a)') &
                '[FANDUMP][rank ', ispmd, '] parent_uid=', crack_info_list(i)%parent_uid, &
                ' owner=', crack_info_list(i)%owning_rank, &
                ' weight=', crack_info_list(i)%weight, &
                ' nfan=', size(crack_info_list(i)%shell_uids), ' :'
              do fj = 1, size(crack_info_list(i)%shell_uids)
                fs = crack_info_list(i)%shell_uids(fj)
                if (fs > 0) then
                  write(6,'(a,i0,a,i0,a)') '    shell uid=', &
                    element%shell%user_id(fs), ' owner=', ispmd, ' (local)'
                else if (fs < 0) then
                  fuid   = element%ghost_shell%uid(-fs)
                  fowner = -1
                  do fp = 1, nspmd
                    if (-fs >= element%ghost_shell%offset(fp) .and. &
                      -fs <  element%ghost_shell%offset(fp+1)) then
                      fowner = fp - 1
                      exit
                    end if
                  end do
                  write(6,'(a,i0,a,i0,a)') '    shell uid=', &
                    fuid, ' owner=', fowner, ' (ghost)'
                end if
              end do
              flush(6)
            end do
          end block

          ! ---------------------------------------------------------------
          ! Phase 1.6: area-weighted mass split fraction per record.
          !
          ! w_split(i) = (area of the shells migrating to N') /
          !              (area of ALL shells attached to the parent),
          ! computed on the pre-split connectivity.  It replaces the previous
          ! fixed 50/50 split of the parent mass: the new node takes the share
          ! of the lumped nodal mass carried by the shell area that migrates
          ! with it, the parent keeps the complement.
          !
          ! Each local shell area is computed on its home rank and communicated
          ! to the ghost copies (same channel as the damage exchange), and the
          ! per-parent sums run in ascending shell-user-id order, so every rank
          ! and a 1-rank run obtain the bitwise-identical fraction.
          ! ---------------------------------------------------------------
          numelc_loc = size(element%shell%nodes, 2)
          nghost = 0
          if (allocated(element%ghost_shell%uid)) nghost = size(element%ghost_shell%uid)
          allocate(shell_area(0:numelc_loc))
          shell_area = 0.0_wp
          do i = 1, numelc_loc
            shell_area(i) = shell_area4(nodes%X, &
              element%shell%nodes(1, i), element%shell%nodes(2, i), &
              element%shell%nodes(3, i), element%shell%nodes(4, i))
          end do
          allocate(ghost_shell_area(max(1, nghost)))
          ghost_shell_area = 0.0_wp
          if (nspmd > 1) then
            call spmd_exchange_ghost_shells(element, ispmd, nspmd, 1, shell_area, ghost_shell_area)
          end if

          allocate(w_split(size(crack_info_list)))
          w_split = 0.5_wp
          do i = 1, size(crack_info_list)
            if (.not. allocated(crack_info_list(i)%shell_uids)) cycle
            if (size(crack_info_list(i)%shell_uids) == 0) cycle
            if (crack_info_list(i)%parent_id <= 0) cycle
            w_split(i) = split_mass_fraction(element, crack_info_list(i)%parent_id, &
              size(crack_info_list(i)%shell_uids), crack_info_list(i)%shell_uids, &
              numelc_loc, shell_area, nghost, ghost_shell_area)
          end do
          deallocate(shell_area)
          deallocate(ghost_shell_area)

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

            ! Canonical /PARITH/ON contribution order for N' (m rows in global-uid order,
            ! identical on every rank holding the parent, equal to the 1-rank layout).
            ! Used by the owner, mirror and placeholder paths and by BOTH skylines.
            call build_contrib_order(element, size(crack_info_list(i)%shell_uids), &
              crack_info_list(i)%shell_uids, ispmd, nspmd, m_c, row_uid_c, row_procne_c)

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
                call set_new_node_values(nodes, crack_info_list(i)%parent_id, w_split(i))
                nodes%MAIN_PROC(nodes%numnod + 1) = crack_info_list(i)%owning_rank+1
                nodes%WEIGHT(nodes%numnod + 1) = 0
                nodes%numnod = nodes%numnod + 1
                numnod = nodes%numnod
                ! Placeholder: 0 local shells, so its band is all RECV rows in the same
                ! canonical order (row_procne(k) = the owning rank of each contribution).
                if (nodes%iparith > 0) then
                  call update_pon_shells(nodes%itab(crack_info_list(i)%parent_id), element, 0, &
                    empty_shells, numnod, ispmd, m_c, row_uid_c, row_procne_c)
                end if
                if (nloc_dmg%imod > 0) then
                  call detach_node_nloc(nloc_dmg, crack_info_list(i)%parent_id, numnod, &
                    element, empty_shells, 0, numnod - 1, nthread, ispmd, nspmd, &
                    m_c, row_uid_c, row_procne_c, mass_fraction=w_split(i))
                end if
                local_new_count = local_new_count + 1
                detached_nodes_local(local_new_count) = crack_info_list(i)%parent_uid
                owning_ranks_local(local_new_count)   = crack_info_list(i)%owning_rank
                w_local_arr(local_new_count)          = w_split(i)
              end if
              deallocate(row_uid_c, row_procne_c)
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
              ! Owner of N': builds its own local rows plus RECV rows for every other
              ! contributing rank, all in the shared canonical order.
              call detach_node(nodes, crack_info_list(i)%parent_id, element, &
                local_shells, local_n, &
                npari, ninter, ipari, interf, nloc_dmg, nthread, nspmd, ispmd, &
                w_split(i), m_c, row_uid_c, row_procne_c)
            else
              ! Ghost copy of N': same canonical band; this rank's shells are LOCAL rows,
              ! all others (owner + any other rank) are RECV rows.
              call mirror_node_split(nodes, crack_info_list(i)%parent_id, element, &
                local_shells, local_n, &
                nloc_dmg, nthread, ispmd, nspmd, crack_info_list(i)%owning_rank, &
                w_split(i), m_c, row_uid_c, row_procne_c)
            end if

            numnod = nodes%numnod
            local_new_count = local_new_count + 1
            detached_nodes_local(local_new_count) = crack_info_list(i)%parent_uid
            owning_ranks_local(local_new_count)   = crack_info_list(i)%owning_rank
            w_local_arr(local_new_count)          = w_split(i)

            deallocate(local_shells)
            deallocate(row_uid_c, row_procne_c)
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

          allocate(w_global_arr(max(total_new_nodes, 1)))
          w_global_arr = 0.5_wp
          if (nspmd > 1) then
            call spmd_allgatherv(detached_nodes_local, local_new_count, &
              detached_nodes, nb_detached_nodes_global, displ_arr)
            call spmd_allgatherv(owning_ranks_local, local_new_count, &
              owning_ranks_global, nb_detached_nodes_global, displ_arr)
            ! Gather the mass-split fraction of each created node so that ranks
            ! holding the parent without creating N' (Phase 5) apply the same
            ! (1 - w) factor as the creating ranks.
            call spmd_allgatherv(w_local_arr, local_new_count, &
              w_global_arr, nb_detached_nodes_global, displ_arr)
          else
            if (total_new_nodes > 0) then
              detached_nodes(1:total_new_nodes)      = detached_nodes_local(1:total_new_nodes)
              owning_ranks_global(1:total_new_nodes) = owning_ranks_local(1:total_new_nodes)
              w_global_arr(1:total_new_nodes)        = w_local_arr(1:total_new_nodes)
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
            current_w           = 0.5_wp
            this_rank_created   = .false.

            do ii = 1, k
              i = permutation(ii)  ! original index

              if (detached_nodes(ii) /= current_parent) then
                ! Finalise the previous group: on non-creating ranks the parent
                ! keeps (1 - w) of its mass/inertia/force (same factor as on the
                ! creating ranks — see set_new_node_values / detach_node_nloc)
                if (current_parent >= 0 .and. .not. this_rank_created) then
                  call scale_parent_on_noncreating_rank(nodes, nloc_dmg, &
                    current_parent, current_w)
                end if
                ! Open a new group
                old_max_uid         = old_max_uid + 1
                numnodg0            = numnodg0    + 1
                new_crack           = new_crack   + 1
                current_parent      = detached_nodes(ii)
                current_owning_rank = owning_ranks_global(i)
                current_w           = w_global_arr(i)
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
              call scale_parent_on_noncreating_rank(nodes, nloc_dmg, &
                current_parent, current_w)
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
          if (allocated(w_split))                   deallocate(w_split)
          if (allocated(w_local_arr))               deallocate(w_local_arr)
          if (allocated(w_global_arr))              deallocate(w_global_arr)

        end subroutine apply_crack

      end module apply_crack_mod
