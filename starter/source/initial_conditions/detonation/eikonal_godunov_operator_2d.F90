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
!||    eikonal_godunov_operator_2d_mod   ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_2d.F90
!||--- called by ------------------------------------------------------
!||    eikonal_compute_adjacent          ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
!||====================================================================
      module eikonal_godunov_operator_2d_mod
      implicit none
      contains

! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Godunov operator for fast marching method in 2D using gradient reconstruction (unstructured mesh)
!! \details Finite differences cannot be used since adjacent points are not necessarily aligned.
!||====================================================================
!||    eikonal_godunov_operator_2d   ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_2d.F90
!||--- called by ------------------------------------------------------
!||    eikonal_compute_adjacent      ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine eikonal_Godunov_Operator_2d(xel, tt, xel_adj, tt_adj, n_adj, Velocity, Velocity_adj)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero, one, ep21, two, four, em06, zep87
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
          real(kind=WP), intent(in) :: tt_adj(4) ! arrival time of adjacent points
          real(kind=WP), intent(in) :: Velocity ! velocity on current point
          real(kind=WP), intent(in) :: Velocity_adj(n_adj) ! velocity on adjacent points
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: k,l
          real(kind=WP) :: dy, dz, dist
          real(kind=WP) :: tt_candidate, tt_1d_k, tt_1d_l
          real(kind=WP) :: a, b
          real(kind=WP) :: s
          real(kind=WP) :: delta
          real(kind=WP) :: A1,A2,B1,B2,C1,C2,AA,BB,CC,DENOM
          real(kind=WP) :: fac
          logical :: causality_ok
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          !traingle only
          fac = one
          if(n_adj == 3)fac = ZEP87 !correction factor for 2d triangles

          ! Solve Eikonal Equation
          a = min(tt_adj(1),tt_adj(3))
          b = min(tt_adj(2),tt_adj(4))
          if (a == ep21 .and. b == ep21) return

          ! --- Identify best upwind neighbor index in each direction ---
          k=1
          if(a==tt_adj(3))k=3
          l=2
          if(b==tt_adj(4))l=4

          ! --- 1D fallback candidates (always computed when direction is available) ---
          tt_1d_k = ep21
          tt_1d_l = ep21

          if(a < ep21)then
            dy = (xel(2)-xel_adj(2,k))
            dz = (xel(3)-xel_adj(3,k))
            dist = sqrt(dy*dy + dz*dz)
            s = max(Velocity, Velocity_adj(k))
            s = s/fac
            s = one / s
            tt_1d_k = a + s*dist
          end if

          if(b < ep21)then
            dy = (xel(2)-xel_adj(2,l))
            dz = (xel(3)-xel_adj(3,l))
            dist = sqrt(dy*dy + dz*dz)
            s = max(Velocity, Velocity_adj(l))
            s = s/fac
            s = one / s
            tt_1d_l = b + s*dist
          end if

          if (a == ep21)then
            ! only direction b available -> 1D update
            tt = min(tt, tt_1d_l)
          else if (b == ep21) then
            ! only direction a available -> 1D update
            tt = min(tt, tt_1d_k)
          else
            ! both directions available -> try 2D update with causality check
            s = max(Velocity, Velocity_adj(k))
            s = max(s, Velocity_adj(l))
            s = s/fac
            s = one / s

            A1 = (xel_adj(3,k)-xel_adj(3,l))
            A2 = (xel_adj(2,l)-xel_adj(2,k))
            B1 = two*( (xel_adj(3,l)-xel(3))*tt_adj(k) - (xel_adj(3,k)-xel(3))*tt_adj(l) )*A1
            B2 = two*( (xel_adj(2,k)-xel(2))*tt_adj(l) - (xel_adj(2,l)-xel(2))*tt_adj(k) )*A2
            C1 = ( (xel_adj(3,l)-xel(3))*tt_adj(k) - (xel_adj(3,k)-xel(3))*tt_adj(l) )
            C2 = ( (xel_adj(2,k)-xel(2))*tt_adj(l) - (xel_adj(2,l)-xel(2))*tt_adj(k) )

            DENOM =  (xel_adj(2,k)-xel(2))*(xel_adj(3,l)-xel(3)) - (xel_adj(2,l)-xel(2))*(xel_adj(3,k)-xel(3))
            DENOM = DENOM*DENOM

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
              tt_candidate = (-BB + sqrt(delta)) / two / AA
              ! causality check : solution must be >= max of upwind neighbors used
              if(tt_candidate >= max(a,b)) causality_ok = .true.
            end if

            if(causality_ok)then
              ! 2D update is valid
              tt = min(tt, tt_candidate)
            else
              ! 2D update failed (delta<0 or causality violated) -> fallback to best 1D-path
              tt = min(tt, tt_1d_k)
              tt = min(tt, tt_1d_l)
            end if

          end if

        end subroutine eikonal_godunov_operator_2d
! ----------------------------------------------------------------------------------------------------------------------

      end module eikonal_godunov_operator_2d_mod
