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
!-----------------------------------------------
! Penetration computation
!-----------------------------------------------
!||====================================================================
!||    sts_penetr              ../engine/source/interfaces/ists/ists_penetr.F90
!||--- called by ------------------------------------------------------
!||    sts_contact_eval_pair   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
      subroutine sts_penetr(xupd, penetr, norm, a)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!     xupd: Element coordinates (3,8)
!     penetr: Output penetration distance
!     norm: Normal vector
!     a: Position matrix (3,24)
!-----------------------------------------------
      real*8, intent(in)    :: xupd(3,8)
      real*8, intent(inout) :: penetr
      real*8, intent(in)    :: norm(3)
      real*8, intent(in)    :: a(3,24)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER i, j, k
      real*8  xupd1d(24)
      
!-----------------------------------------------
!   Convert 2D coordinates to 1D for matrix operations
!-----------------------------------------------
      k = 0
      DO i=1,8
        DO j=1,3
          k = k + 1
          xupd1d(k) = xupd(j,i)
        ENDDO
      ENDDO
      
!-----------------------------------------------
!   Calculate penetration as dot product: penetr = x · a · norm
!-----------------------------------------------
      penetr = 0.d0
      DO i=1,3
        DO j=1,24
          penetr = penetr + xupd1d(j)*a(i,j)*norm(i)
        ENDDO
      ENDDO

      RETURN
      END