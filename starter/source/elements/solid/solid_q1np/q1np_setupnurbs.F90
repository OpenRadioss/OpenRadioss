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
!Chd|  SETUPNURBSQ1NP                 source/elements/solid/solid_q1np/setupnurbsq1np.F90
!Chd|====================================================================
!C=======================================================================
!C   Set up NURBS knot vectors and weights for Q1Np elements
!C
!C   This routine generates open uniform knot vectors
!C
!C   Algorithm for open uniform knot vectors:
!C   - For ne elements and degree p, the knot vector has length: ne + 2*p + 1
!C   - Structure: (p+1) zeros, interior knots, (p+1) ones
!C   - Interior knots: evenly spaced from 0 to 1
!C   - Number of control points: ncp = ne + p
!C
!C   Example for ne=2, p=2:
!C     U = [0, 0, 0, 0.5, 1, 1, 1]
!C     (3 zeros, 1 interior knot at 0.5, 3 ones)
!C
!C   The routine generates:
!C   1. U knot vector (for u direction, length = NX + 2*P + 1)
!C   2. V knot vector (for v direction, length = NY + 2*Q + 1)
!C   3. Weights array (all 1.0 for non-rational B-splines)
!C
!C   Knot vectors are stored concatenated: U knots first, then V knots
!C=======================================================================
      module setupnurbsq1np_mod
        use precision_mod, only : WP
        use constant_mod, only : ZERO, ONE
        implicit none
      contains
        subroutine setupnurbsq1np(nx, ny, p, q, &
     &                            q1np_ktab, ncp_u, ncp_v, &
     &                            q1np_wtab, nweight_max)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
!C     NX,NY     - grid dimensions (number of elements in u,v directions)
!C     P,Q       - NURBS degrees (hardcoded: p=2, q=2 for testing)
!C     Q1NP_KTAB - knot vectors array (output: U knots then V knots)
!C     NCP_U     - number of control points in u direction (output)
!C     NCP_V     - number of control points in v direction (output)
!C     Q1NP_WTAB - weights array (output: all 1.0 for non-rational)
!C     NWEIGHT_MAX- maximum number of weights available in Q1NP_WTAB
          integer, intent(in) :: nx, ny, p, q
          integer, intent(out) :: ncp_u, ncp_v
          integer, intent(in) :: nweight_max
          real(kind=WP), intent(out) :: q1np_ktab(:), q1np_wtab(:)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer :: ne_u, ne_v          ! Number of elements in u,v directions
          integer :: nknot_u, nknot_v    ! Knot vector lengths
          integer :: iv_offset           ! Offset for V knot vector in Q1NP_KTAB
          real(kind=WP) :: delta_u, delta_v    ! Spacing for interior knots
!C=======================================================================
!C   Calculate NURBS parameters
!C=======================================================================
          ne_u = nx
          ne_v = ny
!C     Number of control points: ncp = ne + p
          ncp_u = ne_u + p
          ncp_v = ne_v + q
!C     Knot vector length: len(U) = ne + 2*p + 1
          nknot_u = ne_u + 2*p + 1
          nknot_v = ne_v + 2*q + 1

!C=======================================================================
!C   Generate U knot vector: (p+1) zeros, interior knots, (p+1) ones
!C=======================================================================
!C     Leading zeros: (p+1) zeros at start
          do i = 1, p + 1
            q1np_ktab(i) = ZERO
          end do

!C     Interior knots: evenly spaced from 0 to 1
!C     For ne elements, interior knots are at: 1/ne, 2/ne, ..., (ne-1)/ne
          if (ne_u > 1) then
            delta_u = ONE / real(ne_u, WP)
            do i = 1, ne_u - 1
              q1np_ktab(p + 1 + i) = real(i, WP) * delta_u
            end do
          end if

!C     Trailing ones: (p+1) ones at end
          do i = 1, p + 1
            q1np_ktab(p + 1 + ne_u + i - 1) = ONE
          end do

!C=======================================================================
!C   Generate V knot vector: (q+1) zeros, interior knots, (q+1) ones
!C=======================================================================
!C     V knot vector starts after U knot vector
          iv_offset = nknot_u

!C     Leading zeros: (q+1) zeros at start
          do i = 1, q + 1
            q1np_ktab(iv_offset + i) = ZERO
          end do

!C     Interior knots: evenly spaced from 0 to 1
          if (ne_v > 1) then
            delta_v = ONE / real(ne_v, WP)
            do i = 1, ne_v - 1
              q1np_ktab(iv_offset + q + 1 + i) = real(i, WP) * delta_v
            end do
          end if

!C     Trailing ones: (q+1) ones at end
          do i = 1, q + 1
            q1np_ktab(iv_offset + q + 1 + ne_v + i - 1) = ONE
          end do

!C=======================================================================
!C   Initialize weights to 1.0 (non-rational B-splines)
!C=======================================================================
!C     All weights are set to 1.0, making these non-rational B-splines
!C     rather than full NURBS. For rational surfaces, weights would vary.
          do i = 1, min(ncp_u * ncp_v, nweight_max)
            q1np_wtab(i) = ONE
          end do

          return
        end subroutine setupnurbsq1np
      end module setupnurbsq1np_mod
