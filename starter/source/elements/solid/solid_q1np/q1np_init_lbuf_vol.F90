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
!Chd|====================================================================
!Chd|  Q1NP_INIT_LBUF_VOL            source/elements/solid/solid_q1np/q1np_init_lbuf_vol.F90
!Chd|====================================================================
!C=======================================================================
!C   Initialize per-Gauss-point reference volumes for Q1NP solid groups.
!C=======================================================================
      module q1np_init_lbuf_vol_mod
        use precision_mod, only : WP
        use q1np_restart_mod
        use q1np_geom_mod
        use elbufdef_mod
        implicit none
        public :: q1np_init_lbuf_gp_vol
      contains
!
      !  INITIALIZE PER-GAUSS-POINT REFERENCE VOLUMES FOR Q1NP SOLID GROUPS
        subroutine q1np_init_lbuf_gp_vol(iparg, elbuf_tab, x, numnod, numels, &
     &                                   kq1np_tab, iq1np_tab, &
     &                                   iq1np_bulk_tab)
!-----------------------------------------------------------------------
          use my_alloc_mod, only : my_alloc
          use my_dealloc_mod, only : my_dealloc
          integer, intent(in) :: numels
          integer, intent(in) :: numnod
          integer, intent(in) :: iparg(:,:)
          integer, intent(in) :: kq1np_tab(15,numelq1np_g)
          integer, intent(in) :: iq1np_tab(siq1np_g)
          integer, intent(in) :: iq1np_bulk_tab(sq1npbulk_g)
          real(kind=WP), intent(in) :: x(3,numnod)
          type(ELBUF_STRUCT_), target, intent(inout) :: elbuf_tab(:)
!-----------------------------------------------------------------------
!     Local variables
!-----------------------------------------------------------------------
          integer :: ng, nel, nft
          integer :: nptr, npts, nptt
          integer :: iel_local, iel_hex8, iel_q1np
          integer, allocatable :: hex8_to_q1np(:)
!=======================================================================
!   Early return if the global Q1NP context is not ready
!=======================================================================
          if (numels <= 0 .or. numelq1np_g <= 0) return
          if (.not. associated(q1np_ktab_g)) return
          if (q1np_nknot_sets_g <= 0) then
            if (q1np_nx_g <= 0 .or. q1np_ny_g <= 0) return
          end if
!
          call q1np_ensure_gauss_scheme(kq1np_tab)
          if (q1np_np_u_g <= 0 .or. q1np_np_v_g <= 0 .or. &
     &        q1np_np_t_g <= 0) then
            return
          end if
!=======================================================================
!   Build a direct HEX8 -> Q1NP lookup once for all groups
!=======================================================================
          call my_alloc(hex8_to_q1np, numels, "HEX8_TO_Q1NP")
          hex8_to_q1np = 0
!
          do iel_q1np = 1, numelq1np_g
            iel_hex8 = kq1np_tab(10, iel_q1np)
            if (iel_hex8 > 0 .and. iel_hex8 <= numels) then
              hex8_to_q1np(iel_hex8) = iel_q1np
            end if
          end do
!=======================================================================
!   Loop over solid groups and write mapped Q1NP entries into LBUF
!=======================================================================
          do ng = 1, size(elbuf_tab)
            if (iparg(5, ng) /= 1) cycle
!
            nel  = iparg(2, ng)
            nft  = iparg(3, ng)
            nptr = iparg(56, ng)
            npts = iparg(57, ng)
            nptt = iparg(58, ng)
!
            if (nel <= 0) cycle
            if (nptr /= q1np_np_u_g .or. npts /= q1np_np_v_g .or. &
     &          nptt /= q1np_np_t_g) then
              cycle
            end if
!
            do iel_local = 1, nel
              iel_hex8 = nft + iel_local
              if (iel_hex8 <= 0 .or. iel_hex8 > numels) cycle
!
              iel_q1np = hex8_to_q1np(iel_hex8)
              if (iel_q1np <= 0) cycle
!
              call q1np_fill_element_gp_volumes(iel_q1np, iel_local, &
     &             elbuf_tab(ng)%bufly(1)%lbuf, x, kq1np_tab, &
     &             iq1np_tab, iq1np_bulk_tab)
            end do
          end do
!
          call my_dealloc(hex8_to_q1np)
          return
        end subroutine q1np_init_lbuf_gp_vol
!
        !  ENSURE THE GAUSS SCHEME IS SET UP
        subroutine q1np_ensure_gauss_scheme(kq1np_tab)
!-----------------------------------------------------------------------
          integer, intent(in) :: kq1np_tab(:,:)
!=======================================================================
!   Starter-side calls may happen before the regular Q1NP Gauss setup.
!=======================================================================
          if (q1np_np_u_g > 0 .and. q1np_np_v_g > 0 .and. &
     &        q1np_np_t_g > 0) then
            return
          end if
!
          if (numelq1np_g <= 0) return
!
          call q1np_init_gauss_scheme_starter(kq1np_tab(8, 1) + 1, &
     &         kq1np_tab(9, 1) + 1, 2)
          return
        end subroutine q1np_ensure_gauss_scheme
!
        !  FILL ELEMENT GP VOLUMES
        subroutine q1np_fill_element_gp_volumes(iel_q1np, iel_local, &
     &      lbuf_arr, x, kq1np_tab, iq1np_tab, iq1np_bulk_tab)
!-----------------------------------------------------------------------
          use my_alloc_mod, only : my_alloc
          use my_dealloc_mod, only : my_dealloc
          integer, intent(in) :: iel_q1np, iel_local
          integer, intent(in) :: kq1np_tab(:,:)
          integer, intent(in) :: iq1np_tab(:)
          integer, intent(in) :: iq1np_bulk_tab(:)
          real(kind=WP), intent(in) :: x(:,:)
          type(L_BUFEL_), intent(inout) :: lbuf_arr(:,:,:)
!-----------------------------------------------------------------------
!     Local variables
!-----------------------------------------------------------------------
          integer :: p, q_deg, nctrl
          integer :: offset_ctrl, offset_bulk
          integer :: elem_u, elem_v
          integer :: n_top, n_total
          integer :: nknot_u, nknot_v
          integer :: knot_set_id, nx, ny, ktab_off
          integer :: i, iu, iv, it, k
          integer, allocatable :: node_ids(:)
          real(kind=WP) :: xi, eta, zeta, wg, detj, vol_gp
          real(kind=WP) :: jmat(3,3)
          real(kind=WP), allocatable :: nval(:), dn_local(:,:)
          real(kind=WP), allocatable :: xnode(:,:), u_knot(:), v_knot(:)
!=======================================================================
!   Collect element-local geometry data
!=======================================================================
          p           = kq1np_tab(8,  iel_q1np)
          q_deg       = kq1np_tab(9,  iel_q1np)
          nctrl       = kq1np_tab(3,  iel_q1np)
          offset_ctrl = kq1np_tab(4,  iel_q1np)
          elem_u      = kq1np_tab(6,  iel_q1np)
          elem_v      = kq1np_tab(7,  iel_q1np)
          offset_bulk = kq1np_tab(14, iel_q1np)
!
          n_top   = nctrl
          n_total = n_top + 4
!
          call my_alloc(node_ids, n_total, "NODE_IDS")
          call my_alloc(nval, n_total, "NVAL")
          call my_alloc(dn_local, n_total, 3, "DN_LOCAL")
          call my_alloc(xnode, 3, n_total, "XNODE")
!
          knot_set_id = kq1np_tab(15, iel_q1np)
          if (q1np_nknot_sets_g > 0 .and. knot_set_id > 0 .and. knot_set_id <= q1np_nknot_sets_g) then
            nx = q1np_nx_set_g(knot_set_id)
            ny = q1np_ny_set_g(knot_set_id)
            ktab_off = q1np_ktab_off_g(knot_set_id)
          else
            nx = q1np_nx_g
            ny = q1np_ny_g
            ktab_off = 1
          end if

          nknot_u = nx + 2 * p     + 1
          nknot_v = ny + 2 * q_deg + 1
          call my_alloc(u_knot, nknot_u, "U_KNOT")
          call my_alloc(v_knot, nknot_v, "V_KNOT")
!
          if (ktab_off > 0) then
            u_knot(:) = q1np_ktab_g(ktab_off : ktab_off + nknot_u - 1)
            v_knot(:) = q1np_ktab_g(ktab_off + nknot_u : ktab_off + nknot_u + nknot_v - 1)
          else
            call q1np_get_knot_vectors(nx, ny, p, q_deg, &
     &         q1np_ktab_g, u_knot, v_knot)
          end if
!
          do i = 1, nctrl
            node_ids(i) = iq1np_tab(offset_ctrl + i - 1)
          end do
          do i = 1, 4
            node_ids(n_top + i) = iq1np_bulk_tab(offset_bulk + i - 1)
          end do
!
          do k = 1, n_total
            xnode(1, k) = x(1, node_ids(k))
            xnode(2, k) = x(2, node_ids(k))
            xnode(3, k) = x(3, node_ids(k))
          end do
!=======================================================================
!   Compute and store one volume contribution per Gauss point
!=======================================================================
          do it = 1, size(lbuf_arr, 3)
            zeta = q1np_gp_t_g(it)
            do iu = 1, size(lbuf_arr, 1)
              xi = q1np_gp_u_g(iu)
              do iv = 1, size(lbuf_arr, 2)
                eta = q1np_gp_v_g(iv)
                wg  = q1np_gw_u_g(iu) * q1np_gw_v_g(iv) &
     &              * q1np_gw_t_g(it)
!
                call q1np_shape_functions(xi, eta, zeta, p, q_deg, &
     &               u_knot, v_knot, elem_u, elem_v, nval, dn_local)
!
                call q1np_jacobian(dn_local, xnode, n_total, jmat, detj)
!
                vol_gp = wg * detj
                if (associated(lbuf_arr(iu, iv, it)%vol)) then
                  lbuf_arr(iu, iv, it)%vol(iel_local) = vol_gp
                end if
                if (associated(lbuf_arr(iu, iv, it)%vol0dp)) then
                  lbuf_arr(iu, iv, it)%vol0dp(iel_local) = dble(vol_gp)
                end if
              end do
            end do
          end do
!
          call my_dealloc(u_knot)
          call my_dealloc(v_knot)
          call my_dealloc(node_ids)
          call my_dealloc(nval)
          call my_dealloc(dn_local)
          call my_dealloc(xnode)
          return
        end subroutine q1np_fill_element_gp_volumes
!
      end module q1np_init_lbuf_vol_mod
