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
!Chd|====================================================================
!Chd|  Q1NP_VOLUME                   source/elements/solid/solid_q1np/q1np_volume.F
!Chd|====================================================================
!C=======================================================================
!C   Volume computation for Q1NP enriched elements
!C
!C   This module implements 3D Gauss volume integration for Q1NP elements
!C   over (u, v, t) using enriched element shape functions (NURBS top + Bulk)
!||====================================================================
!||    q1np_volume_mod    ../starter/source/elements/solid/solid_q1np/q1np_volume.F90
!||--- called by ------------------------------------------------------
!||    genq1np_mod        ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- uses       -----------------------------------------------------
!||    message_mod        ../starter/share/message_module/message_mod.F
!||====================================================================
      module q1np_volume_mod
        use message_mod
        use q1np_restart_mod
        use precision_mod, only : WP
        use constant_mod, only : ZERO, ONE, HALF, FOURTH
        use q1np_geom_mod
        implicit none
        integer, parameter :: IDEBUG_Q1NP_VOL = 0
      contains
!C=======================================================================
!C   Main volume integration over element routine
!C=======================================================================
!||====================================================================
!||    q1np_compute_volume_element      ../starter/source/elements/solid/solid_q1np/q1np_volume.F90
!||--- called by ------------------------------------------------------
!||    genq1np                          ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine q1np_compute_volume_element(iel_q1np, &
     &                                         kq1np_tab, iq1np_tab, &
     &                                         iq1np_bulk_tab, q1np_ktab, &
     &                                         x, nx, ny, vol_el, &
     &                                         detj_min_out, &
     &                                         q1np_cptab_opt)
          use my_alloc_mod, only : my_alloc
          use my_dealloc_mod, only : my_dealloc
!C-----------------------------------------------
!C   D u m m y   A r g u m e n t s
!C-----------------------------------------------
          integer,      intent(in)  :: iel_q1np, nx, ny
          integer,      intent(in)  :: kq1np_tab(:,:), iq1np_tab(:)
          integer,      intent(in)  :: iq1np_bulk_tab(:)
          real(kind=WP),intent(in)  :: q1np_ktab(:), x(:,:)
          real(kind=WP),intent(inout) :: vol_el
          real(kind=WP),intent(inout),optional :: detj_min_out
          real(kind=WP),intent(in), optional :: q1np_cptab_opt(:,:)
!C-----------------------------------------------
!C   L o c a l   V a r i a b l e s
!C-----------------------------------------------
          integer :: p, q, nctrl, offset_ctrl, offset_bulk
          integer :: elem_u, elem_v
          integer :: nknot_u, nknot_v
          integer :: i, j, k, iu, iv, it
          integer :: n_top, n_total
          integer :: knot_set_id, nx_loc, ny_loc, ktab_off
          integer, allocatable :: node_ids(:)
          real(kind=WP) :: xi, eta, zeta, wg
          real(kind=WP), allocatable :: nval(:)
          real(kind=WP), allocatable :: dn_local(:,:)
          real(kind=WP), allocatable :: xnode(:,:)
          real(kind=WP), allocatable :: u(:), v(:)
          real(kind=WP) :: jmat(3,3), detj
          real(kind=WP) :: detj_min, detj_max
          real(kind=WP) :: sum_w
          real(kind=WP) :: sum_n, sum_n_min, sum_n_max
          real(kind=WP) :: detj2d, det2d_min, det2d_max, detj2d_gp
          real(kind=WP) :: area2d, sum_w2d
          integer :: ic, ib
          real(kind=WP) :: xi_dbg(4), eta_dbg(4), zeta_dbg
!C----------------------------------------------------------------------
!C   Get order (P,Q), control-point count, offsets, and element spans
!C----------------------------------------------------------------------
          p           = kq1np_tab(8, iel_q1np)
          q           = kq1np_tab(9, iel_q1np)
          nctrl       = kq1np_tab(3, iel_q1np)
          offset_ctrl = kq1np_tab(4, iel_q1np)
          offset_bulk = kq1np_tab(14,iel_q1np)
          elem_u      = kq1np_tab(6, iel_q1np)
          elem_v      = kq1np_tab(7, iel_q1np)

!C----------------------------------------------------------------------
!C   Allocate shape-function and node arrays (N_TOP control pts + 4 bulk)
!C----------------------------------------------------------------------
          n_top   = nctrl
          n_total = n_top + 4
          call my_alloc(node_ids, n_total, "NODE_IDS")
          call my_alloc(nval, n_total, "NVAL")
          call my_alloc(dn_local, n_total, 3, "DN_LOCAL")
          call my_alloc(xnode, 3, n_total, "XNODE")

!C----------------------------------------------------------------------
!C   Allocate and extract U,V knot vectors from global Q1NP_KTAB
!C----------------------------------------------------------------------
          knot_set_id = kq1np_tab(15, iel_q1np)
          if (q1np_nknot_sets_g > 0 .and. knot_set_id > 0 .and. knot_set_id <= q1np_nknot_sets_g) then
            nx_loc = q1np_nx_set_g(knot_set_id)
            ny_loc = q1np_ny_set_g(knot_set_id)
            ktab_off = q1np_ktab_off_g(knot_set_id)
          else
            nx_loc = nx
            ny_loc = ny
            ktab_off = 1
          end if

          nknot_u = nx_loc + 2*p + 1
          nknot_v = ny_loc + 2*q + 1
          call my_alloc(u, nknot_u, "U")
          call my_alloc(v, nknot_v, "V")
          u(:) = q1np_ktab(ktab_off : ktab_off + nknot_u - 1)
          v(:) = q1np_ktab(ktab_off + nknot_u : ktab_off + nknot_u + nknot_v - 1)

!C----------------------------------------------------------------------
!C   Starter-side Jacobian checks may run before the global Q1NP Gauss
!C   scheme is initialized in the regular startup sequence.
!C----------------------------------------------------------------------
          if (q1np_np_u_g <= 0 .or. q1np_np_v_g <= 0 .or. &
     &        q1np_np_t_g <= 0) then
            call q1np_init_gauss_scheme_starter(p + 1, q + 1, 2)
          end if

!C----------------------------------------------------------------------
!C   Build node list: first NCTRL control points, then 4 bulk nodes
!C----------------------------------------------------------------------
!C     Control points (top surface)
          do i = 1, nctrl
            node_ids(i) = iq1np_tab(offset_ctrl + i - 1)
          end do

!C     Bulk nodes (bottom face)
          do i = 1, 4
            node_ids(n_top + i) = iq1np_bulk_tab(offset_bulk + i - 1)
          end do

!C     Extract coordinates
          do k = 1, n_total
            if (k <= n_top .and. present(q1np_cptab_opt)) then
              xnode(1,k) = q1np_cptab_opt(1, node_ids(k))
              xnode(2,k) = q1np_cptab_opt(2, node_ids(k))
              xnode(3,k) = q1np_cptab_opt(3, node_ids(k))
            else
              xnode(1,k) = x(1, node_ids(k))
              xnode(2,k) = x(2, node_ids(k))
              xnode(3,k) = x(3, node_ids(k))
            end if
          end do

!C----------------------------------------------------------------------
!C   Optional: 2D surface area of TOP NURBS surface (ZETA=+1) for debug
!C----------------------------------------------------------------------
          if (idebug_q1np_vol > 0) then
            area2d    = ZERO
            sum_w2d   = ZERO
            det2d_min = huge(ZERO)
            det2d_max = -huge(ZERO)

            do iu = 1, q1np_np_u_g
              xi = q1np_gp_u_g(iu)
              do iv = 1, q1np_np_v_g
                eta  = q1np_gp_v_g(iv)
                zeta = ONE

                wg = q1np_gw_u_g(iu) * q1np_gw_v_g(iv)

                call q1np_shape_functions(xi, eta, zeta, p, q, u, v, &
     &                                  elem_u, elem_v, nval, dn_local)
                call q1np_jacobian(dn_local, xnode, n_total, jmat, detj)

                detj2d = jmat(1,1) * jmat(2,2) - jmat(1,2) * jmat(2,1)

                area2d    = area2d    + wg * detj2d
                sum_w2d   = sum_w2d   + wg
                det2d_min = min(det2d_min, detj2d)
                det2d_max = max(det2d_max, detj2d)
              end do
            end do

          end if

!C----------------------------------------------------------------------
!C   Volume via full 3D Gauss integration over (u, v, t)
!C----------------------------------------------------------------------
          vol_el     = ZERO
          detj_min   = huge(detj)
          detj_max   = -huge(detj)
          sum_w      = ZERO
          sum_n_min  = huge(sum_n)
          sum_n_max  = -huge(sum_n)

          do it = 1, q1np_np_t_g
            zeta = q1np_gp_t_g(it)
            do iu = 1, q1np_np_u_g
              xi = q1np_gp_u_g(iu)
              do iv = 1, q1np_np_v_g
                eta = q1np_gp_v_g(iv)
                wg  = q1np_gw_u_g(iu) * q1np_gw_v_g(iv) &
     &              * q1np_gw_t_g(it)

                call q1np_shape_functions(xi, eta, zeta, p, q, u, v, &
     &                                    elem_u, elem_v, nval, dn_local)

                sum_n = ZERO
                do k = 1, n_total
                  sum_n = sum_n + nval(k)
                end do
                sum_n_min = min(sum_n_min, sum_n)
                sum_n_max = max(sum_n_max, sum_n)

                call q1np_jacobian(dn_local, xnode, n_total, jmat, detj)

                vol_el   = vol_el + wg * detj
                sum_w    = sum_w  + wg
                detj_min = min(detj_min, detj)
                detj_max = max(detj_max, detj)
              end do
            end do
          end do

!C----------------------------------------------------------------------
!C   Optional debug output: 3D volume diagnostics
!C----------------------------------------------------------------------
          if (idebug_q1np_vol >= 2) then
            write(*,'(A,I6,6(A,1P,E12.5))') &
     & 'Q1NP VOL 3D DBG: IEL_Q1NP=', iel_q1np, &
     & ' VOL_EL=', vol_el, &
     & ' SUM_W3D=', sum_w, &
     & ' DETJ_MIN=', detj_min, &
     & ' DETJ_MAX=', detj_max, &
     & ' SUM_N_MIN=', sum_n_min, &
     & ' SUM_N_MAX=', sum_n_max
          end if

!C----------------------------------------------------------------------
!C   Return optional DETJ_MIN
!C----------------------------------------------------------------------
          if (present(detj_min_out)) detj_min_out = detj_min

!C----------------------------------------------------------------------
!C   Deallocate temporary arrays
!C----------------------------------------------------------------------
          call my_dealloc(u)
          call my_dealloc(v)
          call my_dealloc(node_ids)
          call my_dealloc(nval)
          call my_dealloc(dn_local)
          call my_dealloc(xnode)

          return ! Volume of element computed
        end subroutine q1np_compute_volume_element

!C=======================================================================
      end module q1np_volume_mod

