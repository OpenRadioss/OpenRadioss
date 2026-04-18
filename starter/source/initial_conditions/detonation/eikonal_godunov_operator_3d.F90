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
!||    eikonal_godunov_operator_3d_mod   ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_3d.F90
!||--- called by ------------------------------------------------------
!||    eikonal_compute_adjacent          ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
!||====================================================================
      module eikonal_godunov_operator_3d_mod
      implicit none
      contains

! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Godunov operator for fast marching method in 2D using gradient reconstruction (unstructured mesh)
!! \details Finite differences cannot be used since adjacent points are not necessarily aligned.
!||====================================================================
!||    eikonal_godunov_operator_3d   ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_3d.F90
!||--- called by ------------------------------------------------------
!||    eikonal_compute_adjacent      ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine eikonal_Godunov_Operator_3d(xel, tt, xel_adj, tt_adj, n_adj, Velocity, Velocity_adj)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero, one, ep21, two, four, em06
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: n_adj ! number of adjacent points
          real(kind=WP), intent(in) :: Xel(3) ! centroids coordinates
          real(kind=WP), intent(in) :: Xel_adj(3, n_adj) ! centroids coordinates of adjacent points
          real(kind=WP), intent(inout) :: tt ! arrival time
          real(kind=WP), intent(in) :: tt_adj(n_adj) ! arrival time of adjacent points
          real(kind=WP), intent(in) :: Velocity ! velocity on current point
          real(kind=WP), intent(in) :: Velocity_adj(n_adj) ! velocity on adjacent points
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: k,l,m
          real(kind=WP) :: dx, dy, dz, dist
          real(kind=WP) :: tt_candidate
          real(kind=WP) :: a, b, c
          real(kind=WP) :: s
          real(kind=WP) :: delta
          real(kind=WP) :: A1,A2,B1,B2,C1,C2,AA,BB,CC,DENOM
          real(kind=WP) :: max_abc
          real(kind=WP) :: D2
          real(kind=WP) :: u(3), norm_u, v(3), norm_v, k_proj, l_proj_u, l_proj_v

          real(kind=WP) :: x10,x20,x30, y10,y20,y30, z10,z20,z30
          real(kind=WP) :: invA(3,3)

          real(kind=WP) :: T1,T2,T3
          real(kind=WP) :: tt_1d(3)      ! 1D candidates for each direction
          integer :: dir_idx(3)          ! face indices for each direction (k,l,m)
          real(kind=WP) :: dir_tt(3)     ! upwind times for each direction (a,b,c)
          logical :: dir_avail(3)        ! is direction available ?
          integer :: p1                  ! loop index for 2D fallback pairs
          logical :: causality_ok
          integer :: pair(2,3)           ! 3 pairs of directions for 2D fallback
          real(kind=WP) :: tt_2d         ! 2D candidate
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ! Initialize gradients
          a = min(tt_adj(1),tt_adj(3))
          b = min(tt_adj(2),tt_adj(4))
          c = min(tt_adj(5),tt_adj(6))
          max_abc = max(max(a,b),c)
          if (a == ep21 .and. b == ep21 .and. c == ep21) return

          ! --- Identify best upwind neighbor index in each direction ---
          k=1
          if(a==tt_adj(3))k=3
          l=2
          if(b==tt_adj(4))l=4
          m=5
          if(c==tt_adj(6))m=6

          ! --- Store directions info ---
          dir_idx(1) = k ; dir_tt(1) = a ; dir_avail(1) = (a < ep21)
          dir_idx(2) = l ; dir_tt(2) = b ; dir_avail(2) = (b < ep21)
          dir_idx(3) = m ; dir_tt(3) = c ; dir_avail(3) = (c < ep21)

          ! --- STEP 1: Compute 1D candidates for each available direction ---
          tt_1d(1:3) = ep21
          do p1=1,3
            if(.not.dir_avail(p1))cycle
            dx = xel(1)-xel_adj(1,dir_idx(p1))
            dy = xel(2)-xel_adj(2,dir_idx(p1))
            dz = xel(3)-xel_adj(3,dir_idx(p1))
            dist = sqrt(dx*dx+dy*dy+dz*dz)
            s = max(Velocity, Velocity_adj(dir_idx(p1)))
            s = one / s
            tt_1d(p1) = dir_tt(p1) + s*dist
          end do

          ! --- Count available directions ---
          ! pairs for 2D fallback : (1,2), (1,3), (2,3)
          pair(1:2,1) = (/1,2/)
          pair(1:2,2) = (/1,3/)
          pair(1:2,3) = (/2,3/)

          ! --- STEP 2: Try 3D update if all 3 directions available ---
          if(dir_avail(1) .and. dir_avail(2) .and. dir_avail(3))then

            s = max(Velocity, Velocity_adj(k))
            s = max(s, Velocity_adj(l))
            s = max(s, Velocity_adj(m))
            s = one / s

            x10 = xel_adj(1,K)-xel(1)
            x20 = xel_adj(1,L)-xel(1)
            x30 = xel_adj(1,M)-xel(1)

            y10 = xel_adj(2,K)-xel(2)
            y20 = xel_adj(2,L)-xel(2)
            y30 = xel_adj(2,M)-xel(2)

            z10 = xel_adj(3,K)-xel(3)
            z20 = xel_adj(3,L)-xel(3)
            z30 = xel_adj(3,M)-xel(3)

            T1 = tt_adj(k)
            T2 = tt_adj(l)
            T3 = tt_adj(m)

            DENOM =  x10*y20*z30 - x10*z20*y30 - x20*y10*z30 + x20*z10*y30 + x30*y10*z20 - x30*z10*y20

            invA(1,1:3) = [ y20*z30-z20*y30, -y10*z30+z10*y30,  y10*z20-z10*y20]
            invA(2,1:3) = [-x20*z30+z20*x30,  x10*z30-x30*z10, -x10*z20+x20*z10]
            invA(3,1:3) = [ x20*y30-y20*x30, -x10*y30+x30*y10,  x10*y20-x20*y10]

            D2 = DENOM*DENOM

            AA = (-invA(1,1) - invA(1,2) - invA(1,3))**2 &
              + (-invA(2,1) - invA(2,2) - invA(2,3))**2 &
              + (-invA(3,1) - invA(3,2) - invA(3,3))**2

            BB = two*(invA(1,1)*T1 + invA(1,2)*T2 + invA(1,3)*T3) * (-invA(1,1)- invA(1,2) - invA(1,3)) &
              + two*(invA(2,1)*T1 + invA(2,2)*T2 + invA(2,3)*T3) * (-invA(2,1) -invA(2,2) - invA(2,3)) &
              + two*(invA(3,1)*T1 + invA(3,2)*T2 + invA(3,3)*T3) * (-invA(3,1)- invA(3,2) - invA(3,3))

            CC = (invA(1,1)*T1 + invA(1,2)*T2 + invA(1,3)*T3)**2 &
              + (invA(2,1)*T1 + invA(2,2)*T2 + invA(2,3)*T3)**2 &
              + (invA(3,1)*T1 + invA(3,2)*T2 + invA(3,3)*T3)**2 &
              - s*s*D2

            delta = BB*BB-FOUR*AA*CC

            causality_ok = .false.
            if(delta >= zero)then
              tt_candidate = (-BB + sqrt(delta)) / two / AA
              ! causality check : solution must be >= max of all upwind neighbors used
              if(tt_candidate >= max_abc) causality_ok = .true.
            end if

            if(causality_ok)then
              tt = min(tt, tt_candidate)
              return
            end if
            ! 3D update failed -> fallback to 2D pairs below

          end if

          ! --- STEP 3: Try 2D updates (3 pairs, or fewer if only 2 directions available) ---
          do p1=1,3
            if(.not.dir_avail(pair(1,p1)))cycle
            if(.not.dir_avail(pair(2,p1)))cycle

            k = dir_idx(pair(1,p1))
            l = dir_idx(pair(2,p1))

            s = max(Velocity, Velocity_adj(k))
            s = max(s, Velocity_adj(l))
            s = one / s

            ! Project 3D points onto 2D plane (PK, PL)
            ! u is PK vector
            u = xel_adj(:,k) - xel
            norm_u = sqrt(sum(u**2))
            if(norm_u < em06)cycle
            u = u / norm_u

            ! v is orthogonal to u in the plane (PK, PL)
            v = xel_adj(:,l) - xel
            v = v - dot_product(u, v) * u
            norm_v = sqrt(sum(v**2))
            if(norm_v < em06)cycle
            v = v / norm_v

            ! K-Projection on 2D plane
            k_proj = dot_product(u, xel_adj(:,k) - xel)
            ! L-Projection on 2D plane
            l_proj_u = dot_product(u, xel_adj(:,l) - xel)
            l_proj_v = dot_product(v, xel_adj(:,l) - xel)

            ! Calculate coefficients for quadratic equation
            A1 = (-l_proj_v)           ! (y_K - y_L) with y_K=0
            A2 = (l_proj_u - k_proj)   ! (x_L - x_K)
            B1 = two*( l_proj_v*tt_adj(k) - zero*tt_adj(l) )*A1    ! y_K=0
            B2 = two*( k_proj*tt_adj(l) - l_proj_u*tt_adj(k) )*A2
            C1 = ( l_proj_v*tt_adj(k) - zero*tt_adj(l) )
            C2 = ( k_proj*tt_adj(l) - l_proj_u*tt_adj(k) )

            DENOM = k_proj*l_proj_v - l_proj_u*zero  ! y_K=0 so second term vanishes
            DENOM = DENOM*DENOM

            if(DENOM < em06*em06)cycle

            A1 = A1*A1
            A2 = A2*A2
            C1 = C1*C1
            C2 = C2*C2

            AA = (A1+A2)/DENOM
            BB = (B1+B2)/DENOM
            CC = (C1+C2)/DENOM - s*s

            delta = BB*BB-FOUR*AA*CC

            causality_ok = .false.
            if(delta >= zero)then
              tt_2d = (-BB + sqrt(delta)) / two / AA
              ! causality check : solution must be >= max of upwind times used in this pair
              if(tt_2d >= max(dir_tt(pair(1,p1)), dir_tt(pair(2,p1)))) causality_ok = .true.
            end if

            if(causality_ok)then
              tt = min(tt, tt_2d)
            end if
          end do

          ! --- STEP 4: 1D fallback (always valid, take best) ---
          do p1=1,3
            if(dir_avail(p1))then
              tt = min(tt, tt_1d(p1))
            end if
          end do

        end subroutine eikonal_godunov_operator_3d
! ----------------------------------------------------------------------------------------------------------------------

      end module eikonal_godunov_operator_3d_mod
