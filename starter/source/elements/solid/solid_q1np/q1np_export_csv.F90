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
!Chd|  Q1NP_EXPORT_CSV                source/elements/solid/solid_q1np/q1np_export_csv.F90
!Chd|====================================================================
!=======================================================================
!   Export NURBS control points and element connectivity to CSV
!   All node IDs are global (control points use NUMNOD+local_CP_index).
!   Files written in current working directory:
!     - q1np_control_points.csv  (node_id,i_index,j_index,x,y,z)
!     - q1np_elements.csv       (cp_node_*, bulk_node_* = global node IDs)
!     - q1np_bulk_nodes.csv     (node_id,x,y,z)
!     - q1np_surface_nodes.csv  (node_id,grid_i,grid_j,x,y,z)
!     - hex8_elements.csv       (iel,elem_id,n1..n8 from IXS(2:9), IXS(11)=elem_id)
!     - hex8_nodes.csv          (node_id,x,y,z for nodes used by exported HEX8)
!=======================================================================
      module q1np_export_csv_mod
        use precision_mod, only : WP
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
        subroutine q1np_export_nurbs_csv(ncp_u, ncp_v, max_cp_u, max_cp_v, &
     &                                   q1np_cptab, cp_map, &
     &                                   numelq1np_out, kq1np_tab, &
     &                                   iq1np_tab, iq1np_bulk_tab, numnod, &
     &                                   surface_id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ncp_u, ncp_v, max_cp_u, max_cp_v
          integer, intent(in) :: numelq1np_out
          integer, intent(in) :: numnod
          integer, intent(in) :: surface_id
          real(kind=WP), intent(in) :: q1np_cptab(:, :)
          integer, intent(in) :: cp_map(max_cp_u, max_cp_v)
          integer, intent(in) :: kq1np_tab(:, :)
          integer, intent(in) :: iq1np_tab(:)
          integer, intent(in) :: iq1np_bulk_tab(:)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j, cp_id, unit_cp, unit_el, ios
          integer :: iel, nctrl, offset_ctrl, offset_bulk, idx
          integer :: node_id_cp
          integer :: max_nctrl
          character(len=2048) :: header
          character(len=32) :: tmp
          character(len=160) :: file_cp, file_el
!     Global node ID for control point: same convention as Q1NP_PROMOTE_CP_TO_NODES (NUMNOD + local CP index)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if (surface_id .le. 0) then
            file_cp = 'q1np_control_points.csv'
            file_el = 'q1np_elements.csv'
          else
            write(tmp, '(I0)') surface_id
            file_cp = 'q1np_control_points_'//trim(tmp)//'.csv'
            file_el = 'q1np_elements_'//trim(tmp)//'.csv'
          end if

!--- Open control point CSV (node_id = global node ID)
          unit_cp = 987
          open(unit=unit_cp, file=file_cp, &
     &         status='UNKNOWN', form='FORMATTED', iostat=ios)
          if (ios .ne. 0) return
          write(unit_cp, '(A)') 'node_id,i_index,j_index,x,y,z'

          do j = 1, ncp_v
            do i = 1, ncp_u
              if (i .gt. max_cp_u .or. j .gt. max_cp_v) cycle
              cp_id = cp_map(i, j)
              if (cp_id .le. 0) cycle
              node_id_cp = numnod + cp_id
              write(unit_cp, &
     &          '(I0,'','',I0,'','',I0,'','',ES22.14,'','',ES22.14,'','',ES22.14)') &
     &          node_id_cp, i, j, &
     &          q1np_cptab(1, cp_id), &
     &          q1np_cptab(2, cp_id), &
     &          q1np_cptab(3, cp_id)
            end do
          end do
          close(unit_cp)

!--- Open element CSV (all node IDs global: cp_node_* and bulk_node_*)
          unit_el = 988
          open(unit=unit_el, file=file_el, &
     &         status='UNKNOWN', form='FORMATTED', iostat=ios)
          if (ios .ne. 0) return

!     Determine maximum number of control points per element (for rectangular CSV)
          max_nctrl = 0
          do iel = 1, numelq1np_out
            if (kq1np_tab(3, iel) .gt. max_nctrl) max_nctrl = kq1np_tab(3, iel)
          end do

!     Build header with cp_node_1..cp_node_MAX_NCTRL
          header = 'iel,elem_id,elem_u,elem_v,nctrl'
          do idx = 1, max_nctrl
            write(tmp, '(A,I0)') ',cp_node_', idx
            header = trim(header) // trim(tmp)
          end do
          header = trim(header) // ',bulk_node_1,bulk_node_2,bulk_node_3,bulk_node_4'
          write(unit_el, '(A)') trim(header)

          do iel = 1, numelq1np_out
            nctrl       = kq1np_tab(3, iel)
            offset_ctrl = kq1np_tab(4, iel)
            offset_bulk = kq1np_tab(14, iel)

            write(unit_el, '(I0,'','',I0,'','',I0,'','',I0,'','',I0)', &
     &            advance='NO') &
     &            iel, &
     &            kq1np_tab(5, iel), &
     &            kq1np_tab(6, iel), &
     &            kq1np_tab(7, iel), &
     &            nctrl

            do idx = 0, max_nctrl - 1
              if (idx .le. nctrl - 1) then
                cp_id = iq1np_tab(offset_ctrl + idx)
                if (cp_id .le. 0) then
                  node_id_cp = 0
                else
                  node_id_cp = numnod + cp_id
                end if
              else
                node_id_cp = 0
              end if
              write(unit_el, '('','',I0)', advance='NO') node_id_cp
            end do

            do idx = 0, 3
              write(unit_el, '('','',I0)', advance='NO') &
     &          iq1np_bulk_tab(offset_bulk + idx)
            end do

            write(unit_el, *)
          end do

          close(unit_el)

          return
        end subroutine q1np_export_nurbs_csv

!=======================================================================
        subroutine q1np_export_bulk_nodes_csv(numelq1np_out, numnod, &
     &                                         kq1np_tab, iq1np_bulk_tab, x, &
     &                                         surface_id)
! ----------------------------------------------------------------------------------------------------------------------
          use my_alloc_mod, only : my_alloc
          use my_dealloc_mod, only : my_dealloc
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: numelq1np_out, numnod
          integer, intent(in) :: surface_id
          integer, intent(in) :: kq1np_tab(15, numelq1np_out)
          integer, intent(in) :: iq1np_bulk_tab(:)
          real(kind=WP), intent(in) :: x(3, numnod)
! ----------------------------------------------------------------------------------------------------------------------
          integer :: iel, offset_bulk, i, node_id
          integer :: unit_bn, ios
          integer, allocatable :: mark(:)
          character(len=32) :: tmp
          character(len=160) :: file_bn

          call my_alloc(mark, numnod, "MARK")
          mark = 0

!--- Mark all bulk nodes that are used
          do iel = 1, numelq1np_out
            offset_bulk = kq1np_tab(14, iel)
            do i = 0, 3
              node_id = iq1np_bulk_tab(offset_bulk + i)
              if (node_id .gt. 0 .and. node_id .le. numnod) mark(node_id) = 1
            end do
          end do

          if (surface_id .le. 0) then
            file_bn = 'q1np_bulk_nodes.csv'
          else
            write(tmp, '(I0)') surface_id
            file_bn = 'q1np_bulk_nodes_'//trim(tmp)//'.csv'
          end if

!--- Write CSV with node_id,x,y,z
          unit_bn = 989
          open(unit=unit_bn, file=file_bn, &
     &         status='UNKNOWN', form='FORMATTED', iostat=ios)
          if (ios .ne. 0) then
            call my_dealloc(mark)
            return
          end if

          write(unit_bn, '(A)') 'node_id,x,y,z'
          do node_id = 1, numnod
            if (mark(node_id) .eq. 0) cycle
            write(unit_bn, &
     &        '(I0,'','',ES22.14,'','',ES22.14,'','',ES22.14)') node_id, &
     &        x(1, node_id), x(2, node_id), x(3, node_id)
          end do
          close(unit_bn)

          call my_dealloc(mark)
          return
        end subroutine q1np_export_bulk_nodes_csv

!=======================================================================
!   Export original surface nodes before they are replaced by control points.
!   Caller passes GRID_NODE such that GRID_NODE(1:NX+1,1:NY+1) holds node IDs (0 if missing).
!=======================================================================
        subroutine q1np_export_surface_nodes_csv(nx, ny, x_grid, grid_node, &
     &                                           surface_id)
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nx, ny
          integer, intent(in) :: surface_id
          real(kind=WP), intent(in) :: x_grid(3, nx+1, ny+1)
          integer, intent(in) :: grid_node((nx+1)*(ny+1))
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j, k, node_id
          integer :: unit_sn, ios
          character(len=32) :: tmp
          character(len=160) :: file_sn

          if (surface_id .le. 0) then
            file_sn = 'q1np_surface_nodes.csv'
          else
            write(tmp, '(I0)') surface_id
            file_sn = 'q1np_surface_nodes_'//trim(tmp)//'.csv'
          end if

!--- Write CSV with node_id,grid_i,grid_j,x,y,z (one row per grid position)
          unit_sn = 990
          open(unit=unit_sn, file=file_sn, &
     &         status='UNKNOWN', form='FORMATTED', iostat=ios)
          if (ios .ne. 0) then
            return
          end if

          k = 1
          write(unit_sn, '(A)') 'node_id,grid_i,grid_j,x,y,z'
          do j = 1, ny + 1
            do i = 1, nx + 1
              node_id = grid_node(k)
              write(unit_sn, &
     &          '(I0,'','',I0,'','',I0,'','',ES22.14,'','',ES22.14,'','',ES22.14)') &
     &          node_id, i, j, &
     &          x_grid(1, i, j), x_grid(2, i, j), x_grid(3, i, j)
              k = k + 1
            end do
          end do
          close(unit_sn)

          return
        end subroutine q1np_export_surface_nodes_csv

!=======================================================================
!   FUNCTION TO CHECK IF AN ELEMENT IS A BRICK8
!   True if IXS(2:9,iel) are eight positive, pairwise different node IDs.
!=======================================================================
        logical function q1np_ixs_is_distinct_brick8(ixs, nixs, iel)
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nixs, iel
          integer, intent(in) :: ixs(nixs, *)
! ----------------------------------------------------------------------------------------------------------------------
          integer :: n8(8), ii, jj
! ----------------------------------------------------------------------------------------------------------------------
          q1np_ixs_is_distinct_brick8 = .false.
          if (iel < 1) return
          if (nixs < 9) return
          do ii = 1, 8
            n8(ii) = ixs(1 + ii, iel)
          end do
          do ii = 1, 8
            if (n8(ii) <= 0) return
          end do
          do ii = 1, 8
            do jj = ii + 1, 8
              if (n8(ii) == n8(jj)) return
            end do
          end do
          q1np_ixs_is_distinct_brick8 = .true.
        end function q1np_ixs_is_distinct_brick8

!=======================================================================
!   Export all 8-node brick solids (IXS(2:9)=nodes, IXS(11)=user element id).

!   HEX8 elements that are replaced by a Q1NP element (KQ1NP_TAB(10,:) = local
!   IXS index) are omitted so the CSV matches “remaining” classical bricks only.
!=======================================================================
        subroutine q1np_export_hex8_csv(numels, nixs, ixs, x, numnod, &
     &                                  numelq1np_in, kq1np_tab, nkq1np, &
     &                                  surface_id_opt)
! ----------------------------------------------------------------------------------------------------------------------
          use my_alloc_mod, only : my_alloc
          use my_dealloc_mod, only : my_dealloc
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: numels, nixs, numnod
          integer, intent(in) :: numelq1np_in, nkq1np
          integer, intent(in) :: ixs(nixs, *)
          integer, intent(in) :: kq1np_tab(nkq1np, *)
          integer, intent(in), optional :: surface_id_opt
          real(kind=WP), intent(in) :: x(3, *)
! ----------------------------------------------------------------------------------------------------------------------
          integer :: iel, i, node_id, unit_el, unit_nd, ios, maxnode
          integer :: iq, iel_hex
          integer, allocatable :: mark(:)
          logical, allocatable :: skip_q1np_under(:)
          character(len=64) :: file_el, file_nd
          character(len=32) :: suffix
! ----------------------------------------------------------------------------------------------------------------------
          call my_alloc(skip_q1np_under, numels, "SKIP_Q1NP_UNDER")
          skip_q1np_under = .false.
          if (numelq1np_in > 0) then
            do iq = 1, numelq1np_in
              iel_hex = kq1np_tab(10, iq)
              if (iel_hex >= 1 .and. iel_hex <= numels) then
                skip_q1np_under(iel_hex) = .true.
              end if
            end do
          end if

          maxnode = 0
          do iel = 1, numels
            if (skip_q1np_under(iel)) cycle
            if (.not. q1np_ixs_is_distinct_brick8(ixs, nixs, iel)) cycle
            do i = 2, 9
              node_id = ixs(i, iel)
              if (node_id > maxnode) maxnode = node_id
            end do
          end do

          if (present(surface_id_opt)) then
            write(suffix,'(I0)') surface_id_opt
            file_el = 'hex8_elements_'//trim(suffix)//'.csv'
          else
            file_el = 'hex8_elements.csv'
          end if

          unit_el = 991
          open(unit=unit_el, file=file_el, &
     &         status='UNKNOWN', form='FORMATTED', iostat=ios)
          if (ios .ne. 0) then
            call my_dealloc(skip_q1np_under)
            return
          end if

          write(unit_el, '(A)') &
     &    'iel,elem_id,n1,n2,n3,n4,n5,n6,n7,n8'

          do iel = 1, numels
            if (skip_q1np_under(iel)) cycle
            if (.not. q1np_ixs_is_distinct_brick8(ixs, nixs, iel)) cycle
            write(unit_el, &
     &        '(I0,'','',I0,8('','',I0))') &
     &        iel, &
     &        ixs(11, iel), &
     &        ixs(2, iel), ixs(3, iel), ixs(4, iel), ixs(5, iel), &
     &        ixs(6, iel), ixs(7, iel), ixs(8, iel), ixs(9, iel)
          end do
          close(unit_el)

          if (maxnode .le. 0) then
            if (present(surface_id_opt)) then
              write(suffix,'(I0)') surface_id_opt
              file_nd = 'hex8_nodes_'//trim(suffix)//'.csv'
            else
              file_nd = 'hex8_nodes.csv'
            end if

            unit_nd = 992
            open(unit=unit_nd, file=file_nd, &
     &           status='UNKNOWN', form='FORMATTED', iostat=ios)
            if (ios .eq. 0) then
              write(unit_nd, '(A)') 'node_id,x,y,z'
              close(unit_nd)
            end if
            call my_dealloc(skip_q1np_under)
            return
          end if

          call my_alloc(mark, maxnode, "MARK")
          mark = 0
          do iel = 1, numels
            if (skip_q1np_under(iel)) cycle
            if (.not. q1np_ixs_is_distinct_brick8(ixs, nixs, iel)) cycle
            do i = 2, 9
              node_id = ixs(i, iel)
              if (node_id .gt. 0 .and. node_id .le. maxnode) mark(node_id) = 1
            end do
          end do

          if (present(surface_id_opt)) then
            write(suffix,'(I0)') surface_id_opt
            file_nd = 'hex8_nodes_'//trim(suffix)//'.csv'
          else
            file_nd = 'hex8_nodes.csv'
          end if

          unit_nd = 992
          open(unit=unit_nd, file=file_nd, &
     &         status='UNKNOWN', form='FORMATTED', iostat=ios)
          if (ios .ne. 0) then
            call my_dealloc(mark)
            call my_dealloc(skip_q1np_under)
            return
          end if

          write(unit_nd, '(A)') 'node_id,x,y,z'
          do node_id = 1, maxnode
            if (mark(node_id) .eq. 0) cycle
            if (node_id .gt. numnod) cycle
            write(unit_nd, &
     &        '(I0,'','',ES22.14,'','',ES22.14,'','',ES22.14)') node_id, &
     &        x(1, node_id), x(2, node_id), x(3, node_id)
          end do
          close(unit_nd)

          call my_dealloc(mark)
          call my_dealloc(skip_q1np_under)
          return
        end subroutine q1np_export_hex8_csv

      end module q1np_export_csv_mod
