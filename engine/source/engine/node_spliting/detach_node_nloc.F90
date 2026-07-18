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
!||    detach_node_nloc_mod   ../engine/source/engine/node_spliting/detach_node_nloc.F90
!||--- called by ------------------------------------------------------
!||    apply_crack            ../engine/source/engine/node_spliting/apply_crack.F90
!||    detach_node            ../engine/source/engine/node_spliting/detach_node.F90
!||    mirror_node_split      ../engine/source/engine/node_spliting/detach_node.F90
!||====================================================================
      module detach_node_nloc_mod
        implicit none
      contains

!! \brief Update the non-local damage structure NLOC_DMG after a node split.
!!
!! \details When a node (old_local_id) is split into two, a new node
!!          (new_local_id = old numnod + 1) is created and a set of shell
!!          elements is re-connected to it.  This subroutine extends all
!!          non-local arrays in NLOC_DMG to account for the new node:
!!            - node-index tables (INDX, POSI, IDXI)
!!            - DOF-space state vectors (MASS, MASS0, VNL, VNL_OLD, DNL, UNL,
!!              FNL, STIFNL)
!!            - skyline connectivity (ADDCNE, PROCNE, FSKY, STSKY, IADC) for
!!              the PARITH/ON path
!!
!!          Must be called BEFORE nodes%numnod is incremented (i.e. from inside
!!          detach_node, after detach_node_from_shells).
!!          CNE is always allocated at size 0 and is never updated here.
!!
!! ADDCNE/PROCNE convention (1-based inclusive CSR):
!!   ADDCNE(i)   = first FSKY row for NLOC node i (inclusive)
!!   ADDCNE(i+1) = one past the last FSKY row       (exclusive)
!!   ADDCNE(nnod+1) = lcne_nl + 1   (sentinel = first free row)
!!   PROCNE(k) is 1-based: rank ispmd → procne = ispmd + 1
!!
!! Skyline invariant after split:
!!   Parent P's ADDCNE is ALWAYS kept intact.  Rows for shells that moved to N'
!!   are zeroed at split time (FORINT won't update them after IADC redirect).
!!   N' ADDCNE is always built fresh: n_contrib LOCAL rows + n_ghost_contrib
!!   REMOTE recv rows (owner only) or n_owner_contrib_local REMOTE recv rows +
!!   n_ghost_local_contrib LOCAL rows (ghost-node rank).
!!
!! Terminology:
!!   "ghost node"  = MPI mirror copy of a node owned by another rank; its local
!!                   shells write FSKY rows via IADC — this is what drives step 8.
!!   "ghost shell" = element%ghost_shell, used for the physical failure criteria
!!                   exchange in apply_crack.F90 Phase 1 only; unrelated to FSKY.
!||====================================================================
!||    detach_node_nloc    ../engine/source/engine/node_spliting/detach_node_nloc.F90
!||--- called by ------------------------------------------------------
!||    apply_crack         ../engine/source/engine/node_spliting/apply_crack.F90
!||    detach_node         ../engine/source/engine/node_spliting/detach_node.F90
!||    mirror_node_split   ../engine/source/engine/node_spliting/detach_node.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    connectivity_mod    ../common_source/modules/connectivity.F90
!||    extend_array_mod    ../common_source/tools/memory/extend_array.F90
!||    nlocal_reg_mod      ../common_source/modules/nlocal_reg_mod.F
!||    precision_mod       ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine detach_node_nloc(nloc_dmg, old_local_id, new_local_id, &
          elements, shell_list, list_size, old_numnod, nthread, ispmd, nspmd_in, &
          m, row_uid, row_procne, mass_fraction)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use nlocal_reg_mod
          use connectivity_mod
          use extend_array_mod
          use precision_mod, only : wp
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nlocal_str_),   intent(inout) :: nloc_dmg      !< non-local damage structure
          integer,             intent(in)    :: old_local_id  !< local id of the node being split
          integer,             intent(in)    :: new_local_id  !< local id of the new node (= old numnod + 1)
          type(connectivity_), intent(in)    :: elements      !< element connectivity (already updated for shell_list)
          integer,             intent(in)    :: list_size     !< number of shells being detached
          integer,             intent(in)    :: shell_list(list_size) !< local ids of detached shells
          integer,             intent(in)    :: old_numnod    !< total node count before the split
          integer,             intent(in)    :: nthread       !< number of threads (second dim of FNL/STIFNL)
          integer,             intent(in)    :: ispmd         !< local MPI rank (0-based); PROCNE uses 1-based ranks
          integer,             intent(in)    :: nspmd_in      !< number of MPI ranks
          integer,             intent(in)    :: m             !< total FSKY rows for N' (all migrating shells, canonical order)
          integer,             intent(in)    :: row_uid(m)    !< shell user id of each canonical row (uid-sorted)
          integer,             intent(in)    :: row_procne(m) !< 1-based owning rank of each canonical row
          real(kind=wp),       intent(in), optional :: mass_fraction !< area-weighted share of the parent non-local mass taken by the new node
          !< (computed by apply_crack, identical on every rank).  When present, the child gets
          !< mass_fraction and the parent keeps (1 - mass_fraction) so total mass is conserved.
          !< When absent, the legacy behaviour applies: child gets the corner-count f_detach and
          !< the parent mass is left unchanged.
          !integer,             intent(in), optional :: node_uid       !< global UID of the split node (for diagnostics)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nl_idx          ! non-local rank of the parent node
          integer :: old_pos         ! first DOF position of parent in non-local vectors
          integer :: nddl            ! DOF count of the parent node
          integer :: new_pos         ! first DOF position of the new node
          integer :: new_nnod        ! NNOD after the new node is added
          integer :: new_l_nloc      ! L_NLOC after extension
          integer :: n_contrib       ! number of FSKY row contributions for the new node
          integer :: n_old_contrib   ! FSKY row contributions remaining on the old node
          integer :: old_lcne        ! LCNE_NL before extension
          integer :: old_fsky_rows   ! number of FSKY rows before extension (= first new row index)
          integer :: new_fsky_rows   ! number of FSKY rows after extension  (= one past last new row)
          integer :: fsky_ncol, stsky_ncol
          integer :: fnl_ncol, stifnl_ncol
          integer :: i, j, k, r, i_el, a
          integer :: shell_id, su
          integer :: n_remain        ! corner count remaining on parent after split
          integer :: n_total         ! total corner count before split (n_remain + n_contrib)
          integer :: numelc          ! total number of shell elements
          integer :: cc              ! loop counter over ADDCNE/IADC entries
          integer :: parent_start    ! first FSKY row of parent node's ADDCNE range
          integer :: parent_end      ! last  FSKY row of parent node's ADDCNE range
          integer :: parent_total    ! total entries in parent's ADDCNE (local + remote)
          integer :: new_start       ! first FSKY row of new node's ADDCNE range (= old sentinel)
          real(kind=wp) :: f_retain  ! fraction of element corners retained by parent
          real(kind=wp) :: f_detach  ! fraction of element corners detached to child
          real(kind=wp) :: f_mass    ! mass split fraction actually applied to MASS/MASS0
          logical :: l_scale_parent  ! .true. when the parent mass keeps (1 - f_mass)
          real(kind=wp), parameter   :: ZERO = 0._wp
          real(kind=wp), parameter   :: ONE  = 1._wp
          integer :: n_contrib_global        ! m = all shells moving to N' (canonical total)
          integer :: n_total_global          ! parent_total from ADDCNE (all shells around parent)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          parent_start = 0
          parent_end   = 0
          if(ispmd < 0 .or. ispmd >= nspmd_in) return
!          write(6,*) "detach node user id",node_uid
          !Nodes 1682 - 1728
          ! Step 1 — Extend IDXI to cover new_local_id regardless of whether the parent
          !          has non-local DOFs.  This must happen before the early-exit check so
          !          that idxi is always sized to at least new_local_id; if we returned
          !          early without extending, the next split would call
          !          extend_array(idxi, numnod+1, numnod+2) with oldsize > size(idxi),
          !          causing a heap-buffer-overflow in the copy inside extend_array.
          call extend_array(nloc_dmg%idxi, old_numnod, new_local_id)
          ! The new node inherits no non-local DOFs by default (entry = 0).
          nloc_dmg%idxi(new_local_id) = 0

          nl_idx = nloc_dmg%idxi(old_local_id)
          if (nl_idx == 0) return

          ! Step 2 — DOF layout of the parent node
          old_pos = nloc_dmg%posi(nl_idx)
          nddl    = nloc_dmg%posi(nl_idx + 1) - old_pos
          new_pos = nloc_dmg%l_nloc + 1

          ! Step 3 — Set IDXI for the new node (extend_array already called in Step 1)
          nloc_dmg%idxi(new_local_id) = nloc_dmg%nnod + 1

          ! Step 4 — Extend INDX (size NNOD → NNOD+1)
          call extend_array(nloc_dmg%indx, nloc_dmg%nnod, nloc_dmg%nnod + 1)
          nloc_dmg%indx(nloc_dmg%nnod + 1) = new_local_id

          ! Step 5 — Extend POSI (size NNOD+1 → NNOD+2)
          !          POSI is 1-based; new node DOFs occupy [new_pos … new_pos+nddl-1]
          call extend_array(nloc_dmg%posi, nloc_dmg%nnod + 1, nloc_dmg%nnod + 2)
          nloc_dmg%posi(nloc_dmg%nnod + 2) = new_pos + nddl

          ! Step 6 — Extend DOF-space vectors by nddl entries and copy parent values
          new_l_nloc = nloc_dmg%l_nloc + nddl

          call extend_array(nloc_dmg%mass,    nloc_dmg%l_nloc, new_l_nloc)
          call extend_array(nloc_dmg%mass0,   nloc_dmg%l_nloc, new_l_nloc)
          call extend_array(nloc_dmg%vnl,     nloc_dmg%l_nloc, new_l_nloc)
          call extend_array(nloc_dmg%vnl_old, nloc_dmg%l_nloc, new_l_nloc)
          call extend_array(nloc_dmg%dnl,     nloc_dmg%l_nloc, new_l_nloc)
          call extend_array(nloc_dmg%unl,     nloc_dmg%l_nloc, new_l_nloc)

          ! Copy the accumulated damage state to the new node; set VNL/MASS proportional
          ! to the detached fraction.  See inline comments below for the physics rationale.

          ! --- Counting block -------------------------------------------------------
          ! Count element corners: detached (→ new_local_id) and retained (→ old_local_id).
          ! The connectivity was already updated by detach_node_from_shells before this
          ! call, so scanning elements%shell%nodes gives the post-split distribution.
          ! n_contrib also serves the PARITH/ON skyline update in Step 8.
          numelc    = size(elements%shell%nodes, 2)
          n_contrib = 0
          do i = 1, list_size
            shell_id = shell_list(i)
            do j = 1, 4
              if (elements%shell%nodes(j, shell_id) == new_local_id) n_contrib = n_contrib + 1
            end do
          end do
          n_remain = 0
          do i_el = 1, numelc
            do j = 1, 4
              if (elements%shell%nodes(j, i_el) == old_local_id) n_remain = n_remain + 1
            end do
          end do
          n_total = n_remain + n_contrib
          ! Safety: degenerate case — no corners on either node; fall back to equal share.
          if (n_total == 0) then
            f_retain = 0.5_wp
            f_detach = 0.5_wp
          else
            f_retain = real(n_remain,  wp) / real(n_total, wp)
            f_detach = real(n_contrib, wp) / real(n_total, wp)
          end if

          ! On MPI runs (PARITH/ON): correct f_detach/f_retain using the global corner count from
          ! ADDCNE so the VNL/mass scaling matches the 1-MPI result.  parent_total from ADDCNE
          ! counts ALL corners (local + remote).  The correction fires whenever remote entries
          ! exist (parent_total > n_contrib + n_remain), regardless of how many go to N'.
          ! With the canonical band, the number of shells migrating to N' (local + remote)
          ! is exactly m on every rank, and the parent's global corner count is its current
          ! ADDCNE band size (which already accounts for remote contributions via recv rows).
          n_contrib_global = m
          if (allocated(nloc_dmg%addcne)) then
            n_total_global = nloc_dmg%addcne(nl_idx + 1) - nloc_dmg%addcne(nl_idx)
            if (n_total_global > n_contrib + n_remain) then
              ! Remote entries exist — use the global total for correct f_detach/f_retain
              f_detach = real(n_contrib_global, wp) / real(n_total_global, wp)
              f_retain = real(n_total_global - n_contrib_global, wp) / real(n_total_global, wp)
            end if
          end if

          ! Mass split fraction: prefer the area-weighted fraction computed once per
          ! split by apply_crack (bitwise identical on every rank and in a 1-rank run);
          ! fall back to the corner-count f_detach when not provided (legacy behaviour,
          ! parent mass left unchanged).
          if (present(mass_fraction)) then
            f_mass = mass_fraction
            l_scale_parent = .true.
          else
            f_mass = f_detach
            l_scale_parent = .false.
          end if

          ! UNL: both sides of the crack start with the same value — copy it.
          nloc_dmg%unl    (new_pos:new_pos+nddl-1) = nloc_dmg%unl    (old_pos:old_pos+nddl-1)
          ! DNL is overwritten by NLOCAL_INCR (DNL := DT2*VNL) before it is next read.
          nloc_dmg%dnl    (new_pos:new_pos+nddl-1) = ZERO

          ! VNL/FNL for PARENT: leave unchanged.  MASS for PARENT: scaled by
          ! (1 - f_mass) below when the area-weighted mass_fraction is provided.
          !
          ! MPI consistency: in PARITH/ON the ghost copy of the parent node on remote
          ! ranks has the same VNL/MASS as the owner and receives the same FNL via the
          ! FSKY exchange.  NLOCAL_ACC and NLOCAL_VEL therefore compute identical VNL
          ! updates on all ranks — every modification applied here must use the same
          ! factor on every rank holding the parent (mass_fraction guarantees that).
          !
          ! Physical interpretation: the split takes effect for the NEXT cycle's FORINT
          ! (via IADC/ADDCNE updates below).  In the CURRENT cycle FNL was assembled from
          ! the pre-split connectivity and represents a valid force.  Using it unmodified
          ! lets VNL evolve continuously without an artificial discontinuity.
          !
          ! Child node N': inherit a proportional share of VNL/MASS so that the split
          ! preserves the amplitude of the non-local wave at the crack tip.
          nloc_dmg%vnl    (new_pos:new_pos+nddl-1) = nloc_dmg%vnl    (old_pos:old_pos+nddl-1) * f_detach
          nloc_dmg%vnl_old(new_pos:new_pos+nddl-1) = nloc_dmg%vnl_old(old_pos:old_pos+nddl-1) * f_detach

          ! Child node gets the migrating share of the non-local mass.  With the
          ! area-weighted mass_fraction the parent keeps the complement below, so the
          ! total non-local mass is exactly conserved; in the legacy (fallback) path
          ! the parent mass is left unchanged.
          nloc_dmg%mass (new_pos:new_pos+nddl-1) = nloc_dmg%mass (old_pos:old_pos+nddl-1) * f_mass
          nloc_dmg%mass0(new_pos:new_pos+nddl-1) = nloc_dmg%mass0(old_pos:old_pos+nddl-1) * f_mass
          if (l_scale_parent) then
            ! Every rank holding the parent applies the same factor (creating ranks
            ! here, non-creating ranks in apply_crack Phase 5), so all MPI copies of
            ! the parent stay bitwise consistent.
            nloc_dmg%mass (old_pos:old_pos+nddl-1) = nloc_dmg%mass (old_pos:old_pos+nddl-1) * (ONE - f_mass)
            nloc_dmg%mass0(old_pos:old_pos+nddl-1) = nloc_dmg%mass0(old_pos:old_pos+nddl-1) * (ONE - f_mass)
          end if

          ! DEBUG: print split state for tracing MPI vs MONO divergence.
!          write(6,'(a,i0,a,i0,a,l1,a,i0,a,i0,a,i0,a,i0,a,f8.5,a,f8.5,a,g13.6,a,g13.6,a,g13.6)') &
!            '[NLOC_SPLIT] ispmd=', ispmd, ' uid=', node_uid, &
!            ' mirror=', l_is_mirror, &
!            ' n_loc=', n_contrib, ' n_ghost=', n_ghost_contrib_local, &
!            ' n_owner=', n_owner_contrib_local, ' n_tot=', n_total_global, &
!            ' f_det=', f_detach, ' f_ret=', f_retain, &
!            ' vnl_par=', nloc_dmg%vnl(old_pos), &
!            ' vnl_chi=', nloc_dmg%vnl(new_pos), &
!            ' mass_chi=', nloc_dmg%mass(new_pos)
!          call flush(6)

          ! Extend multithreaded accumulators; use actual allocated column counts
          ! (can be 1 in PARITH/ON, nthread in PARITH/OFF).
          if (allocated(nloc_dmg%fnl)) then
            fnl_ncol = size(nloc_dmg%fnl, 2)
          else
            fnl_ncol = nthread
          end if
          if (allocated(nloc_dmg%stifnl)) then
            stifnl_ncol = size(nloc_dmg%stifnl, 2)
          else
            stifnl_ncol = nthread
          end if

          call extend_array(nloc_dmg%fnl,    nloc_dmg%l_nloc, fnl_ncol, new_l_nloc, fnl_ncol)
          call extend_array(nloc_dmg%stifnl, nloc_dmg%l_nloc, stifnl_ncol, new_l_nloc, stifnl_ncol)
          nloc_dmg%fnl   (new_pos:new_pos+nddl-1, 1:fnl_ncol) = ZERO
          nloc_dmg%stifnl(new_pos:new_pos+nddl-1, 1:stifnl_ncol) = ZERO

          ! Step 7 — Commit scalar counters
          nloc_dmg%nnod   = nloc_dmg%nnod + 1
          nloc_dmg%l_nloc = new_l_nloc
          new_nnod        = nloc_dmg%nnod

          ! Step 8 — Skyline update for PARITH/ON
          !          (only when FSKY/ADDCNE/PROCNE are allocated, i.e. PARITH/ON path)

          if (.not. allocated(nloc_dmg%addcne)) return

          ! Warn when solid or triangle-shell non-local elements are present: their
          ! IADS/IADTG back-pointers are NOT updated here.  Node splitting is currently
          ! restricted to quad-shell meshes; adding support for those types requires
          ! equivalent loops below for IADS (8 corners) and IADTG (3 corners).
          if (nloc_dmg%numels_nl > 0 .or. nloc_dmg%numeltg_nl > 0) then
            write(6,'(a)') &
              ' ** WARNING detach_node_nloc: non-local solid/triangle-shell elements detected.'
            write(6,'(a)') &
              '    IADS/IADTG back-pointers are NOT updated after node split.'
            write(6,'(a)') &
              '    Non-local assembly may be incorrect for those element types.'
          end if

          ! Steps 8b-8f: parent ADDCNE is always kept intact; N' ADDCNE is always built
          ! fresh; dead parent FSKY rows (shells moved to N') are zeroed before IADC
          ! redirect so they contribute zero to FNL(parent) in all subsequent cycles.

          parent_start = nloc_dmg%addcne(nl_idx)
          parent_end   = nloc_dmg%addcne(nl_idx + 1) - 1
          parent_total = parent_end - parent_start + 1

          ! 8b — Extend ADDCNE (CSR row offsets) by one entry for the new node.
          !      N' starts as a zero-entry phantom: addcne(new_nnod) = addcne(new_nnod+1)
          !      = old sentinel (= old_lcne + 1).  Subsequent steps append new rows.
          call extend_array(nloc_dmg%addcne, new_nnod, new_nnod + 1)
          new_start = nloc_dmg%addcne(new_nnod)   ! old phantom = N's first FSKY row index

          ! N' ADDCNE built fresh in the GLOBAL canonical (uid-sorted) order: m rows, one
          ! per migrating shell.  Row k gets PROCNE = row_procne(k) -- rows with
          ! row_procne == ispmd+1 are LOCAL (this rank's shells write them via IADC), the
          ! rest are REMOTE recv rows.  The same ordered band on every rank holding the
          ! parent makes SPMD_EXCH_SUB_PON symmetric (no MPI truncation) and the non-local
          ! assembly decomposition-independent (bitwise /PARITH/ON, equal to the 1-rank run).
          if (allocated(nloc_dmg%fsky)) then
            fsky_ncol = size(nloc_dmg%fsky, 2)
          else
            fsky_ncol = nloc_dmg%nddmax
          end if
          if (allocated(nloc_dmg%stsky)) then
            stsky_ncol = size(nloc_dmg%stsky, 2)
          else
            stsky_ncol = nloc_dmg%nddmax
          end if

          old_lcne = nloc_dmg%lcne_nl
          nloc_dmg%addcne(new_nnod + 1) = new_start + m
          if (m > 0) then
            call extend_array(nloc_dmg%procne, old_lcne, old_lcne + m)
            do k = 1, m
              nloc_dmg%procne(new_start + k - 1) = row_procne(k)
            end do
            ! FSKY/STSKY are sized to ADDCNE(NNOD+1) (= LCNE_NL+1, i.e. they carry the
            ! sentinel row), so grow the first dim to new_start+m = ADDCNE(new_nnod+1).
            ! new_start (= old sentinel = old_lcne+1) is the current first-dim size.
            call extend_array(nloc_dmg%fsky,  new_start, fsky_ncol,  new_start + m, fsky_ncol)
            call extend_array(nloc_dmg%stsky, new_start, stsky_ncol, new_start + m, stsky_ncol)
            nloc_dmg%fsky (new_start:new_start+m-1, 1:fsky_ncol)  = ZERO
            nloc_dmg%stsky(new_start:new_start+m-1, 1:stsky_ncol) = ZERO
            nloc_dmg%lcne_nl = old_lcne + m
          end if

          if (list_size > 0) then
            ! Zero the dead LOCAL rows in the parent's band (shells that moved to N')
            ! BEFORE redirecting IADC, so FNL(parent) receives zero for them next cycles.
            do i = 1, list_size
              shell_id = shell_list(i)
              do j = 1, 4
                if (elements%shell%nodes(j, shell_id) == new_local_id) then
                  cc = nloc_dmg%iadc(j, shell_id)
                  nloc_dmg%fsky (cc, 1:fsky_ncol)  = ZERO
                  nloc_dmg%stsky(cc, 1:stsky_ncol) = ZERO
                end if
              end do
            end do

            ! Redirect IADC of this rank's local migrating shells to their canonical row.
            do i = 1, list_size
              shell_id = shell_list(i)
              su = elements%shell%user_id(shell_id)
              do j = 1, 4
                if (elements%shell%nodes(j, shell_id) == new_local_id) then
                  do a = 1, m
                    if (row_uid(a) == su) then
                      nloc_dmg%iadc(j, shell_id) = new_start + a - 1
                      exit
                    end if
                  end do
                end if
              end do
            end do
          end if

        end subroutine detach_node_nloc

      end module detach_node_nloc_mod
