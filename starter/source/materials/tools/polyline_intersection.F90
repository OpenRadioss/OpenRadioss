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
!||    polyline_intersection_mod   ../starter/source/materials/tools/polyline_intersection.F90
!||--- called by ------------------------------------------------------
!||    diffuse_necking_2d          ../starter/source/materials/fail/diffuse_necking_2d.F90
!||====================================================================
       module polyline_intersection_mod
         implicit none
         private
         public :: polyline_intersection

       contains
        !----------------------------------------------------------------------
        ! \brief  Finds one intersection between two tabulated functions treated as
        ! \brief  piecewise-linear curves y(x). Assumes x1 and x2 are strictly increasing.
        !
        ! \details Inputs:
        ! \details    x1(n1), y1(n1) : first function samples
        ! \details    x2(n2), y2(n2) : second function samples
        ! \details    n1, n2         : number of samples in each
        !
        ! \details  Outputs:
        ! \details    xint, yint : coordinates of an intersection if found
        ! \details    found      : .true. if an intersection is found, else .false.
        !
        ! \details   If multiple intersections exist, the first in increasing x is returned.
        ! \details   If segments are colinear over an overlap, the left overlap endpoint is returned.
        ! \details   Robust to near-parallel segments and endpoint touches via tolerances.
        !----------------------------------------------------------------------

!||====================================================================
!||    polyline_intersection   ../starter/source/materials/tools/polyline_intersection.F90
!||--- called by ------------------------------------------------------
!||    diffuse_necking_2d      ../starter/source/materials/fail/diffuse_necking_2d.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine polyline_intersection(n1, n2, x1, y1, x2, y2, xint, yint, found)
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use constant_mod  ,only : zero,half,one,hundred,ep03
          use precision_mod ,only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Global Arguments
! --------------------------------------------------------------------------------------------------
        integer, intent(in) :: n1, n2
        real(kind=WP) ,dimension(n1) ,intent(in)  :: x1,y1
        real(kind=WP) ,dimension(n2) ,intent(in)  :: x2,y2
        real(kind=WP) ,intent(out) :: xint, yint
        logical       ,intent(out) :: found
!-----------------------------------------------
!         L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: i, j
        real(kind=WP) :: dx1, dy1, dx2, dy2, m1, m2, b1, b2
        real(kind=WP) :: x1l, x1r, x2l, x2r, xL, xR
        real(kind=WP) :: y1L, y1R, y2L, y2R, dL, dR, denom, root
        real(kind=WP) :: epsx, epsy, slopes_eps, xrange, yrange
!===================================================================================================
        ! Initialize
        found = .false.
        xint  = zero
        yint  = zero

        if (n1 < 2 .or. n2 < 2) return

        ! Quick sanity: x must be strictly increasing for both series
        do i = 1, n1-1
           if (x1(i+1) <= x1(i)) return
        end do
        do j = 1, n2-1
           if (x2(j+1) <= x2(j)) return
        end do

        ! Dynamic tolerances scaled to data ranges
        xrange = max( abs(x1(n1) - x1(1)), abs(x2(n2) - x2(1)), one )
        yrange = max( maxval(abs(y1 - sum(y1)/dble(n1))), maxval(abs(y2 - sum(y2)/dble(n2))), one )
        epsx   = hundred  * epsilon(one) * xrange
        epsy   = hundred  * epsilon(one) * yrange
        slopes_eps = ep03 * epsilon(one)

        i = 1
        j = 1

        do while (i <= n1-1 .and. j <= n2-1)

           ! Segment 1: [x1(i), x1(i+1)]
           x1l = x1(i)
           x1r = x1(i+1)
           dx1 = x1r - x1l
           dy1 = y1(i+1) - y1(i)
           if (dx1 <= zero) then
              i = i + 1
              cycle
           end if
           m1 = dy1 / dx1
           b1 = y1(i) - m1 * x1l

           ! Segment 2: [x2(j), x2(j+1)]
           x2l = x2(j)
           x2r = x2(j+1)
           dx2 = x2r - x2l
           dy2 = y2(j+1) - y2(j)
           if (dx2 <= zero) then
              j = j + 1
              cycle
           end if
           m2 = dy2 / dx2
           b2 = y2(j) - m2 * x2l

           ! Overlap of x-intervals
           xL = max(x1l, x2l)
           xR = min(x1r, x2r)

           if (xR >= xL - epsx) then
              ! Compute y values at overlap endpoints via linear interpolation
              y1L = m1 * xL + b1
              y1R = m1 * xR + b1
              y2L = m2 * xL + b2
              y2R = m2 * xR + b2

              dL = y1L - y2L
              dR = y1R - y2R

              ! Check endpoint hits
              if (abs(dL) <= epsy) then
                 xint = xL
                 yint = half * (y1L + y2L)
                 found = .true.
                 return
              else if (abs(dR) <= epsy) then
                 xint = xR
                 yint = half * (y1R + y2R)
                 found = .true.
                 return
              end if

              ! General crossing in (xL, xR)
              if (dL * dR < zero) then
                 ! Linear root interpolation for robust bracketing
                 denom = dR - dL
                 if (abs(denom) > epsy) then
                    root = xL - dL * (xR - xL) / denom
                    ! Clamp root within [xL, xR] for safety
                    if (root < xL) root = xL
                    if (root > xR) root = xR
                    xint = root
                    yint = m1 * xint + b1
                    found = .true.
                    return
                 end if
              end if

              ! Parallel / nearly parallel segments
              if (abs(m1 - m2) <= slopes_eps) then
                 ! If colinear over the overlap, accept the left overlap point
                 if (abs((b1 - b2)) <= epsy) then
                    xint = xL
                    yint = half * (y1L + y2L)
                    found = .true.
                    return
                 end if
              end if

           end if

           ! Advance the segment with the smaller right endpoint
           if (x1r < x2r - epsx) then
              i = i + 1
           else if (x2r < x1r - epsx) then
              j = j + 1
           else
              ! Endpoints essentially equal; advance both to avoid stalling
              i = i + 1
              j = j + 1
           end if

        end do

        ! No intersection found
        found = .false.
!-----------------------------------------------
        end subroutine polyline_intersection
        end module polyline_intersection_mod
