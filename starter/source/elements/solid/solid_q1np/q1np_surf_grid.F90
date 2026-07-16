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
!Chd|====================================================================
!Chd|  Q1NP_BUILD_SURF_GRID            source/elements/solid/solid_q1np/q1np_surf_grid.F90
!Chd|====================================================================
!=======================================================================
!   Build surface grid topology from quad segment connectivity.
!   Each segment is a quad with 4 nodes; segments share edges.
!   Uses BFS from segment 1 to assign (I,J) grid indices and to fill
!   GRID_NODE (node IDs at grid points) and GRID_TO_SEG (segment at (I,J)).
!   IERR: 0=ok, 3=corner ordering mismatch, 4=disconnected,
!         5=non-rectangular/inconsistent, 7=bounds.
!=======================================================================
      module q1np_surf_grid_mod
        use groupdef_mod
        implicit none
      contains
        integer function q1np_local_edge_from_pair(corner_a, corner_b)
          implicit none
          integer, intent(in) :: corner_a, corner_b
          integer :: cmin, cmax

          cmin = min(corner_a, corner_b)
          cmax = max(corner_a, corner_b)

          q1np_local_edge_from_pair = 0
          if (cmin .eq. 1 .and. cmax .eq. 4) then
            q1np_local_edge_from_pair = 1
          else if (cmin .eq. 2 .and. cmax .eq. 3) then
            q1np_local_edge_from_pair = 2
          else if (cmin .eq. 1 .and. cmax .eq. 2) then
            q1np_local_edge_from_pair = 3
          else if (cmin .eq. 3 .and. cmax .eq. 4) then
            q1np_local_edge_from_pair = 4
          end if
        end function q1np_local_edge_from_pair

        integer function q1np_grid_dir_to_local_edge(corner_order, grid_dir)
          implicit none
          integer, intent(in) :: corner_order(4), grid_dir
          integer :: corner_a, corner_b

          corner_a = 0
          corner_b = 0
          select case (grid_dir)
          case (1)
            corner_a = corner_order(4)
            corner_b = corner_order(1)
          case (2)
            corner_a = corner_order(2)
            corner_b = corner_order(3)
          case (3)
            corner_a = corner_order(1)
            corner_b = corner_order(2)
          case (4)
            corner_a = corner_order(3)
            corner_b = corner_order(4)
          end select

          q1np_grid_dir_to_local_edge = q1np_local_edge_from_pair(corner_a, corner_b)
        end function q1np_grid_dir_to_local_edge

        subroutine q1np_build_surf_grid(surf, nseg, &
     &                                  nx, ny, seg_i, seg_j, &
     &                                  grid_node, grid_to_seg, ierr)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use my_alloc_mod, only : my_alloc
          use my_dealloc_mod, only : my_dealloc
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(surf_), intent(in) :: surf
          integer, intent(in)  :: nseg
          integer, intent(out) :: nx, ny, ierr
          integer, intent(inout), dimension(nseg) :: seg_i, seg_j
          integer, intent(inout), dimension(nseg*4, nseg*4) :: grid_node
          integer, intent(inout), dimension(nseg, nseg) :: grid_to_seg
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer, parameter :: off = 2048
          integer, allocatable :: neighbor(:, :), seg_corner(:, :)
          integer, allocatable :: qseg(:), qi(:), qj(:)
          logical, allocatable :: assigned(:)
          integer :: nodes(4), n1, n2, na, nb, na2, nb2
          integer :: iseg, jseg, k, kk, dir, idir, d, dir_local
          integer :: ii, jj, i_min, i_max, j_min, j_max
          integer :: head, tail, nassign
          integer :: idx, itr, ii_new, jj_new, next(4)
          integer :: gi(4), gj(4), base(4), cand(4), exist(4)
          integer, allocatable :: grid_node_tmp(:, :)
          integer :: istat_tmp
!     Direction order for BFS: right(2), top(4), left(1), bottom(3)
          integer :: dir_list(4), delta_i(4), delta_j(4)
          integer :: gioff(4, 4), gjoff(4, 4)
          integer :: corner_xform(4, 8)
          integer :: jseg_found
          data next / 2, 3, 4, 1 /
          data dir_list / 2, 4, 1, 3 /
          data delta_i / -1, 1, 0, 0 /
          data delta_j / 0, 0, -1, 1 /
!     Grid offsets for neighbor quad corners (dir, corner): 1=left, 2=right, 3=bottom, 4=top
          data gioff / -1, 1, 0, 0,  0, 2, 1, 1,  0, 2, 1, 1,  -1, 1, 0, 0 /
          data gjoff / 0, 0, -1, 1,  0, 0, -1, 1,  1, 1, 0, 2,  1, 1, 0, 2 /
!     Try the four cyclic rotations first, then the four mirrored orderings.
          data corner_xform / &
     &      1, 2, 3, 4, &
     &      2, 3, 4, 1, &
     &      3, 4, 1, 2, &
     &      4, 1, 2, 3, &
     &      1, 4, 3, 2, &
     &      4, 3, 2, 1, &
     &      3, 2, 1, 4, &
     &      2, 1, 4, 3 /
!=======================================================================
          ierr = 0
          nx = 0
          ny = 0
          call my_alloc(neighbor, nseg, 4, "NEIGHBOR")
          call my_alloc(assigned, nseg, "ASSIGNED")
          call my_alloc(seg_corner, 4, nseg, "SEG_CORNER")
          call my_alloc(qseg, nseg, "QSEG")
          call my_alloc(qi, nseg, "QI")
          call my_alloc(qj, nseg, "QJ")
          allocate(grid_node_tmp(1-off:off*2+1, 1-off:off*2+1), stat=istat_tmp)
          if (istat_tmp /= 0) then
            call my_dealloc(neighbor)
            call my_dealloc(assigned)
            call my_dealloc(seg_corner)
            call my_dealloc(qseg)
            call my_dealloc(qi)
            call my_dealloc(qj)
            ierr = 7
            return
          end if

          seg_i(1:nseg) = 0
          seg_j(1:nseg) = 0
          neighbor(1:nseg, 1:4) = 0
          assigned(1:nseg) = .false.
          seg_corner(1:4, 1:nseg) = 0

!     --- Build neighbor list ---
!     For each segment and each edge (DIR): find segment sharing that edge.
!     DIR 1=left(edge 4-1), 2=right(2-3), 3=bottom(1-2), 4=top(3-4).
          do iseg = 1, nseg
            do k = 1, 4
              nodes(k) = surf%nodes(iseg, k)
            end do
            do dir = 1, 4
              if (dir .eq. 1) then
                na = nodes(4)
                nb = nodes(1)
              else if (dir .eq. 2) then
                na = nodes(2)
                nb = nodes(3)
              else if (dir .eq. 3) then
                na = nodes(1)
                nb = nodes(2)
              else
                na = nodes(3)
                nb = nodes(4)
              end if
              n1 = min(na, nb)
              n2 = max(na, nb)
              jseg_found = 0
              do jseg = 1, nseg
                if (jseg .eq. iseg) cycle
                do kk = 1, 4
                  na2 = surf%nodes(jseg, kk)
                  nb2 = surf%nodes(jseg, next(kk))
                  if (min(na2, nb2) .eq. n1 .and. max(na2, nb2) .eq. n2) then
                    neighbor(iseg, dir) = jseg
                    jseg_found = 1
                    exit
                  end if
                end do
                if (jseg_found .ne. 0) exit
              end do
            end do
          end do

!     Temporary grid for node IDs (offset indexing so (1,1) is valid)
          do ii = 1-off, off*2+1
            do jj = 1-off, off*2+1
              grid_node_tmp(ii, jj) = 0
            end do
          end do

!     --- BFS: assign (I,J) to each segment and fill GRID_NODE_TMP ---
!     Start with segment 1 at (1,1); quad nodes 1,2,3,4 at (1,1),(2,1),(2,2),(1,2).
          head = 1
          tail = 1
          qseg(1) = 1
          qi(1) = 1
          qj(1) = 1
          assigned(1) = .true.
          seg_i(1) = 1
          seg_j(1) = 1
          seg_corner(1:4, 1) = (/ 1, 2, 3, 4 /)
          do k = 1, 4
            nodes(k) = surf%nodes(1, k)
          end do
          grid_node_tmp(1, 1) = nodes(1)
          grid_node_tmp(2, 1) = nodes(2)
          grid_node_tmp(2, 2) = nodes(3)
          grid_node_tmp(1, 2) = nodes(4)
          nassign = 1

          do while (head .le. tail)
            iseg = qseg(head)
            ii = qi(head)
            jj = qj(head)
            head = head + 1

            do k = 1, 4
              nodes(k) = surf%nodes(iseg, k)
            end do

!       Process each direction: if unvisited neighbor exists, enqueue and assign node IDs
            do idir = 1, 4
              d = dir_list(idir)
              dir_local = q1np_grid_dir_to_local_edge(seg_corner(1:4, iseg), d)
              if (dir_local .le. 0) then
                ierr = 3
                call my_dealloc(grid_node_tmp)
                call my_dealloc(neighbor)
                call my_dealloc(assigned)
                call my_dealloc(seg_corner)
                call my_dealloc(qseg)
                call my_dealloc(qi)
                call my_dealloc(qj)
                return
              end if

              jseg = neighbor(iseg, dir_local)
              if (jseg .le. 0) cycle
              ii_new = ii + delta_i(d)
              jj_new = jj + delta_j(d)

              do k = 1, 4
                base(k) = surf%nodes(jseg, k)
              end do
              do k = 1, 4
                gi(k) = ii + gioff(d, k)
                gj(k) = jj + gjoff(d, k)
                exist(k) = grid_node_tmp(gi(k), gj(k))
              end do

!         Find a corner ordering for the neighbor.
              if (.not. assigned(jseg)) then
                idx = 0
                do itr = 1, 8
                  do k = 1, 4
                    cand(k) = base(corner_xform(k, itr))
                    if (exist(k) .gt. 0 .and. exist(k) .ne. cand(k)) exit
                  end do
                  if (k .gt. 4) then
                    idx = itr
                    exit
                  end if
                end do

                if (idx .le. 0) then
                  ierr = 3
                  call my_dealloc(grid_node_tmp)
                  call my_dealloc(neighbor)
                  call my_dealloc(assigned)
                  call my_dealloc(seg_corner)
                  call my_dealloc(qseg)
                  call my_dealloc(qi)
                  call my_dealloc(qj)
                  return
                end if

                assigned(jseg) = .true.
                seg_i(jseg) = ii_new
                seg_j(jseg) = jj_new
                seg_corner(1:4, jseg) = corner_xform(1:4, idx)
                nassign = nassign + 1
                tail = tail + 1
                if (tail .gt. nseg) then
                  ierr = 7
                  call my_dealloc(grid_node_tmp)
                  call my_dealloc(neighbor)
                  call my_dealloc(assigned)
                  call my_dealloc(seg_corner)
                  call my_dealloc(qseg)
                  call my_dealloc(qi)
                  call my_dealloc(qj)
                  return
                end if
                qseg(tail) = jseg
                qi(tail) = ii_new
                qj(tail) = jj_new
              else
                if (seg_i(jseg) .ne. ii_new .or. seg_j(jseg) .ne. jj_new) then
                  ierr = 5
                  call my_dealloc(grid_node_tmp)
                  call my_dealloc(neighbor)
                  call my_dealloc(assigned)
                  call my_dealloc(seg_corner)
                  call my_dealloc(qseg)
                  call my_dealloc(qi)
                  call my_dealloc(qj)
                  return
                end if
                do k = 1, 4
                  cand(k) = base(seg_corner(k, jseg))
                  if (exist(k) .gt. 0 .and. exist(k) .ne. cand(k)) then
                    ierr = 3
                    call my_dealloc(grid_node_tmp)
                    call my_dealloc(neighbor)
                    call my_dealloc(assigned)
                    call my_dealloc(seg_corner)
                    call my_dealloc(qseg)
                    call my_dealloc(qi)
                    call my_dealloc(qj)
                    return
                  end if
                end do
              end if

              do k = 1, 4
                grid_node_tmp(gi(k), gj(k)) = cand(k)
              end do
            end do
          end do

!     --- Sanity: all segments must have been assigned (connected surface) ---
          if (nassign .ne. nseg) then
            ierr = 4
            call my_dealloc(grid_node_tmp)
            call my_dealloc(neighbor)
            call my_dealloc(assigned)
            call my_dealloc(seg_corner)
            call my_dealloc(qseg)
            call my_dealloc(qi)
            call my_dealloc(qj)
            return
          end if

!     --- Normalize (I,J) to 1..NX, 1..NY and compute NX, NY ---
          i_min = seg_i(1)
          i_max = seg_i(1)
          j_min = seg_j(1)
          j_max = seg_j(1)
          do iseg = 2, nseg
            i_min = min(i_min, seg_i(iseg))
            i_max = max(i_max, seg_i(iseg))
            j_min = min(j_min, seg_j(iseg))
            j_max = max(j_max, seg_j(iseg))
          end do
          nx = i_max - i_min + 1
          ny = j_max - j_min + 1

          if (nx*ny .ne. nseg) then
            ierr = 5
            call my_dealloc(grid_node_tmp)
            call my_dealloc(neighbor)
            call my_dealloc(assigned)
            call my_dealloc(seg_corner)
            call my_dealloc(qseg)
            call my_dealloc(qi)
            call my_dealloc(qj)
            return
          end if

          do iseg = 1, nseg
            seg_i(iseg) = seg_i(iseg) - i_min + 1
            seg_j(iseg) = seg_j(iseg) - j_min + 1
          end do

!     --- Fill GRID_TO_SEG: segment index at each (I,J) ---
          grid_to_seg(1:nx, 1:ny) = 0
          do iseg = 1, nseg
            ii = seg_i(iseg)
            jj = seg_j(iseg)
            grid_to_seg(ii, jj) = iseg
          end do

!     --- Copy GRID_NODE_TMP into output GRID_NODE (1-based, size (NX+1)*(NY+1)) ---
          if (nx+1 .gt. nseg*4 .or. ny+1 .gt. nseg*4) then
            ierr = 7
            call my_dealloc(grid_node_tmp)
            call my_dealloc(neighbor)
            call my_dealloc(assigned)
            call my_dealloc(seg_corner)
            call my_dealloc(qseg)
            call my_dealloc(qi)
            call my_dealloc(qj)
            return
          end if

          do jj = j_min, j_max + 1
            do ii = i_min, i_max + 1
              grid_node(ii - i_min + 1, jj - j_min + 1) = grid_node_tmp(ii, jj)
            end do
          end do

          call my_dealloc(grid_node_tmp)
          call my_dealloc(neighbor)
          call my_dealloc(assigned)
          call my_dealloc(seg_corner)
          call my_dealloc(qseg)
          call my_dealloc(qi)
          call my_dealloc(qj)

          return
        end subroutine q1np_build_surf_grid
      end module q1np_surf_grid_mod
