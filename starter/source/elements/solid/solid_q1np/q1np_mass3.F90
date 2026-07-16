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
!Chd|  Q1NP_MASS3                    source/elements/solid/solid_q1np/q1np_mass3.F90
!Chd|====================================================================
!=======================================================================
!   Lumped mass for Q1NP enriched elements.
!
!   m_K = rho * sum_gp( N_K(xi,eta,zeta) * det(J) * w_gp )
!=======================================================================
!||====================================================================
!||    q1np_mass3_mod     ../starter/source/elements/solid/solid_q1np/q1np_mass3.F90
!||--- called by ------------------------------------------------------
!||    q1np_init_mod      ../starter/source/elements/solid/solid_q1np/q1np_init.F90
!||--- uses       -----------------------------------------------------
!||    message_mod        ../starter/share/message_module/message_mod.F
!||====================================================================
      module q1np_mass3_mod
        use message_mod
        use q1np_restart_mod
        use q1np_geom_mod
        use elbufdef_mod
        use precision_mod, only : WP
        use constant_mod,  only : ZERO, ONE, HALF, FOURTH, SIX
        use element_mod   , only : NIXS
        implicit none
      contains
!
!||====================================================================
!||    q1np_mass3                       ../starter/source/elements/solid/solid_q1np/q1np_mass3.F90
!||--- called by ------------------------------------------------------
!||    q1np_init                        ../starter/source/elements/solid/solid_q1np/q1np_init.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                           ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine q1np_mass3( &
     &      rho, ms, mssa, partsav, x, v, &
     &      fill, iparg, elbuf_tab, kq1np_tab, iq1np_tab, iq1np_bulk_tab, &
     &      numelq1np_in, npropm, nummat, pm, &
     &      numels, numnod, npart, q1np_ktab_g,sfill,irest_mselt)
!-----------------------------------------------------------------------
          use my_alloc_mod,   only : my_alloc
          use my_dealloc_mod, only : my_dealloc
          implicit none
!-----------------------------------------------------------------------
!     Dummy arguments
!-----------------------------------------------------------------------
          integer, intent(in) :: numelq1np_in
          integer, intent(in) :: numels, numnod, npart
          integer, intent(in) :: npropm, nummat
          integer, intent(in) :: iparg(:,:)
          integer, intent(in) :: kq1np_tab(15, numelq1np_in)
          integer, intent(in) :: iq1np_tab(siq1np_g)
          integer, intent(in) :: iq1np_bulk_tab(sq1npbulk_g)
          integer, intent(in) :: sfill
          integer, intent(in) :: irest_mselt
          real(kind=WP), intent(in)    :: rho(:) 
          real(kind=WP), intent(in)    :: pm(npropm, nummat)
          real(kind=WP), intent(in)    :: q1np_ktab_g(:)
          type(ELBUF_STRUCT_), target, intent(in) :: elbuf_tab(:)

          real(kind=WP), intent(in)    :: fill(sfill)
          real(kind=WP), intent(inout) :: ms(numnod)
          real(kind=WP), intent(inout) :: mssa(numels)
          real(kind=WP), intent(inout) :: partsav(20,npart)
          real(kind=WP), intent(inout) :: x(3,numnod)
          real(kind=WP), intent(inout) :: v(3,numnod)

!-----------------------------------------------------------------------
!     Local variables
!-----------------------------------------------------------------------
          integer :: iel, iel_hex8, iel_local, ip, mid
          integer :: i, k, iu, iv, it, ng, igrp
          integer :: nel, nft
          integer :: p, q_deg, nctrl, offset_ctrl, offset_bulk
          integer :: elem_u, elem_v
          integer :: nknot_u, nknot_v
          integer :: n_top, n_total, nx, ny
          integer :: node_id
          real(kind=WP) :: rho_elem, rho_from_gbuf, rho_from_pm, fill_fac
          real(kind=WP) :: xi, eta, zeta, vol_gp
          real(kind=WP) :: xx, yy, zz, xy, yz, zx
          real(kind=WP) :: mass_node_k, mass_total_el
          real(kind=WP), allocatable :: nval(:), dn_local(:,:)
          real(kind=WP), allocatable :: mass_node(:)
          integer,       allocatable :: node_ids(:)
          real(kind=WP), allocatable :: u_knot(:), v_knot(:)
!=======================================================================
!   Early return if no Q1NP elements
!=======================================================================
          if (numelq1np_in == 0) return
!
          ! Knot vectors will be extracted per-knot-set for heterogeneous NX/NY.
!=======================================================================
!   Initialize Gauss scheme if not yet done
!=======================================================================
          p     = kq1np_tab(8, 1)
          q_deg = kq1np_tab(9, 1)
          if (q1np_np_u_g <= 0 .or. q1np_np_v_g <= 0 .or. &
     &        q1np_np_t_g <= 0) then
            call q1np_init_gauss_scheme_starter(p + 1, q_deg + 1, 2)
          end if
!=======================================================================
!   Loop over all Q1NP elements
!=======================================================================
          do iel = 1, numelq1np_in
            iel_hex8 = kq1np_tab(10, iel)
            if (iel_hex8 <= 0 .or. iel_hex8 > numels) cycle
!
            mid = kq1np_tab(1, iel)
            ip  = kq1np_tab(11, iel)
            if (ip <= 0) cycle
!
            p           = kq1np_tab(8, iel)
            q_deg       = kq1np_tab(9, iel)
            nctrl       = kq1np_tab(3, iel)
            offset_ctrl = kq1np_tab(4, iel)
            offset_bulk = kq1np_tab(14,iel)
            elem_u      = kq1np_tab(6, iel)
            elem_v      = kq1np_tab(7, iel)

            ! Extract knot vectors for this element's knot set.
            if (q1np_nknot_sets_g > 0) then
              igrp = kq1np_tab(15, iel)
              if (igrp <= 0 .or. igrp > q1np_nknot_sets_g) igrp = 1

              nx = q1np_nx_set_g(igrp)
              ny = q1np_ny_set_g(igrp)

              nknot_u = nx + 2*p + 1
              nknot_v = ny + 2*q_deg + 1

              call my_dealloc(u_knot)
              call my_dealloc(v_knot)
              call my_alloc(u_knot, nknot_u, "U_KNOT")
              call my_alloc(v_knot, nknot_v, "V_KNOT")

              ! Copy knots from the global concatenated knot-vector pool.
              u_knot(:) = q1np_ktab_g(q1np_ktab_off_g(igrp) : q1np_ktab_off_g(igrp)+nknot_u-1)
              v_knot(:) = q1np_ktab_g(q1np_ktab_off_g(igrp)+nknot_u : q1np_ktab_off_g(igrp)+nknot_u+nknot_v-1)
            else
              ! Legacy single-knot-set view.
              nx = q1np_nx_g
              ny = q1np_ny_g

              nknot_u = nx + 2*p + 1
              nknot_v = ny + 2*q_deg + 1

              call my_dealloc(u_knot)
              call my_dealloc(v_knot)
              call my_alloc(u_knot, nknot_u, "U_KNOT")
              call my_alloc(v_knot, nknot_v, "V_KNOT")

              call q1np_get_knot_vectors(nx, ny, p, q_deg, q1np_ktab_g, u_knot, v_knot)
            end if
!
            n_top   = nctrl
            n_total = n_top + 4
!-----------------------------------------------------------------------
!     Determine element density (rho * fill_factor)
!-----------------------------------------------------------------------
            rho_from_gbuf = ZERO
            rho_from_pm   = ZERO
            if (iel_hex8 > 0 .and. iel_hex8 <= size(rho)) then
              rho_from_gbuf = rho(iel_hex8)
            end if
            if (mid > 0 .and. mid <= nummat) then
              rho_from_pm = pm(1, mid)
            end if
            rho_elem = rho_from_gbuf
            if (rho_elem <= ZERO) rho_elem = rho_from_pm
            fill_fac = ONE
            if (sfill > 0) then
              if (fill(iel_hex8) > ZERO) then
                fill_fac = fill(iel_hex8)
              end if
            endif 
            rho_elem = fill_fac * rho_elem
!-----------------------------------------------------------------------
!     Build node list: NCTRL control points, then 4 bulk nodes
!-----------------------------------------------------------------------
            call my_alloc(node_ids, n_total, "NODE_IDS")
            call my_alloc(nval, n_total, "NVAL")
            call my_alloc(dn_local, n_total, 3, "DN_LOCAL")
            call my_alloc(mass_node, n_total, "MASS_NODE")
!
            do i = 1, nctrl
              node_ids(i) = iq1np_tab(offset_ctrl + i - 1)
            end do
            do i = 1, 4
              node_ids(n_top + i) = iq1np_bulk_tab(offset_bulk + i - 1)
            end do
!
            ng = 0
            do igrp = 1, size(elbuf_tab)
              if (iparg(5,igrp) /= 1) cycle
              nel = iparg(2,igrp)
              nft = iparg(3,igrp)
              if (iel_hex8 >= nft+1 .and. iel_hex8 <= nft+nel) then
                ng = igrp
                iel_local = iel_hex8 - nft
                exit
              end if
            end do
            if (ng == 0) then
              write(*,'(A,I8)') ' Q1NP ERROR: missing ELBUF group for Q1NP element ', iel
              call ancmsg(msgid=364, msgtype=msgerror, anmode=aninfo, &
     &          c1='Q1NP_MASS3 cannot map element to ELBUF group')
              call my_dealloc(node_ids)
              call my_dealloc(nval)
              call my_dealloc(dn_local)
              call my_dealloc(mass_node)
              call my_dealloc(u_knot)
              call my_dealloc(v_knot)
              return
            end if
!-----------------------------------------------------------------------
!     Gauss integration with precomputed GP volumes:
!       m_K = rho * sum_gp( N_K * vol_gp )
!-----------------------------------------------------------------------
            mass_node = ZERO
!
            do it = 1, q1np_np_t_g
              zeta = q1np_gp_t_g(it)
              do iu = 1, q1np_np_u_g
                xi = q1np_gp_u_g(iu)
                do iv = 1, q1np_np_v_g
                  eta = q1np_gp_v_g(iv)
!
                  call q1np_shape_functions(xi, eta, zeta, &
     &                 p, q_deg, u_knot, v_knot, &
     &                 elem_u, elem_v, nval, dn_local)
!
                  if (.not.associated(elbuf_tab(ng)%bufly(1)%lbuf(iu,iv,it)%vol)) then
                    write(*,'(A,4I8)') ' Q1NP ERROR: missing LBUF volume pointer ng/iu/iv/it=', &
     &                  ng, iu, iv, it
                    call ancmsg(msgid=364, msgtype=msgerror, anmode=aninfo, &
     &                c1='Q1NP_MASS3 missing LBUF volume pointer')
                    call my_dealloc(node_ids)
                    call my_dealloc(nval)
                    call my_dealloc(dn_local)
                    call my_dealloc(mass_node)
                    call my_dealloc(u_knot)
                    call my_dealloc(v_knot)
                    return
                  end if
                  if (iel_local <= 0 .or. iel_local > size(elbuf_tab(ng)%bufly(1)%lbuf(iu,iv,it)%vol)) then
                    write(*,'(A,2I8)') ' Q1NP ERROR: invalid LBUF volume index iel_local/ng=', &
     &                  iel_local, ng
                    call ancmsg(msgid=364, msgtype=msgerror, anmode=aninfo, &
     &                c1='Q1NP_MASS3 invalid LBUF volume index')
                    call my_dealloc(node_ids)
                    call my_dealloc(nval)
                    call my_dealloc(dn_local)
                    call my_dealloc(mass_node)
                    call my_dealloc(u_knot)
                    call my_dealloc(v_knot)
                    return
                  end if
                  vol_gp = elbuf_tab(ng)%bufly(1)%lbuf(iu,iv,it)%vol(iel_local)
!
                  do k = 1, n_total
                    mass_node(k) = mass_node(k) + nval(k) * vol_gp
                  end do
                end do
              end do
            end do
!
            do k = 1, n_total
              mass_node(k) = rho_elem * mass_node(k)
            end do
!-----------------------------------------------------------------------
!     Assemble nodal mass into global MS array
!-----------------------------------------------------------------------
            do k = 1, n_total
              node_id = node_ids(k)
              if (node_id > 0 .and. node_id <= numnod) then
                ms(node_id) = ms(node_id) + mass_node(k)
              end if
            end do
!-----------------------------------------------------------------------
!     PARTSAV statistics per-node masses
!-----------------------------------------------------------------------
            mass_total_el = ZERO
            do k = 1, n_total
              mass_total_el = mass_total_el + mass_node(k)
            end do
            if (irest_mselt /= 0) then
              if (iel_hex8 > 0 .and. iel_hex8 <= numels) then
                mssa(iel_hex8) = mass_total_el
              end if
            end if
            partsav(1,ip) = partsav(1,ip) + mass_total_el
!
            do k = 1, n_total
              node_id = node_ids(k)
              if (node_id > 0 .and. node_id <= numnod) then
                mass_node_k = mass_node(k)
                partsav(2,ip)  = partsav(2,ip)  + mass_node_k * x(1, node_id)
                partsav(3,ip)  = partsav(3,ip)  + mass_node_k * x(2, node_id)
                partsav(4,ip)  = partsav(4,ip)  + mass_node_k * x(3, node_id)
              end if
            end do
!
            xx = ZERO; yy = ZERO; zz = ZERO
            xy = ZERO; yz = ZERO; zx = ZERO
            do k = 1, n_total
              node_id = node_ids(k)
              if (node_id > 0 .and. node_id <= numnod) then
                mass_node_k = mass_node(k)
                xx = xx + mass_node_k * x(1, node_id) * x(1, node_id)
                yy = yy + mass_node_k * x(2, node_id) * x(2, node_id)
                zz = zz + mass_node_k * x(3, node_id) * x(3, node_id)
                xy = xy + mass_node_k * x(1, node_id) * x(2, node_id)
                yz = yz + mass_node_k * x(2, node_id) * x(3, node_id)
                zx = zx + mass_node_k * x(3, node_id) * x(1, node_id)
              end if
            end do
            partsav(5,ip)  = partsav(5,ip)  + (yy + zz)
            partsav(6,ip)  = partsav(6,ip)  + (zz + xx)
            partsav(7,ip)  = partsav(7,ip)  + (xx + yy)
            partsav(8,ip)  = partsav(8,ip)  - xy
            partsav(9,ip)  = partsav(9,ip)  - yz
            partsav(10,ip) = partsav(10,ip) - zx
!
            do k = 1, n_total
              node_id = node_ids(k)
              if (node_id > 0 .and. node_id <= numnod) then
                mass_node_k = mass_node(k)
                partsav(11,ip) = partsav(11,ip) + mass_node_k * v(1, node_id)
                partsav(12,ip) = partsav(12,ip) + mass_node_k * v(2, node_id)
                partsav(13,ip) = partsav(13,ip) + mass_node_k * v(3, node_id)
                partsav(14,ip) = partsav(14,ip) + HALF * mass_node_k * &
     &            ( v(1,node_id)*v(1,node_id) + &
     &              v(2,node_id)*v(2,node_id) + &
     &              v(3,node_id)*v(3,node_id) )
              end if
            end do
!
            call my_dealloc(node_ids)
            call my_dealloc(nval)
            call my_dealloc(dn_local)
            call my_dealloc(mass_node)
          end do
!
          call my_dealloc(u_knot)
          call my_dealloc(v_knot)
          return
        end subroutine q1np_mass3
!
      end module q1np_mass3_mod
