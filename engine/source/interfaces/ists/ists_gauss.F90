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
! Gaussian quadrature points and weights
!-----------------------------------------------
!||====================================================================
!||    sts_gausspt             ../engine/source/interfaces/ists/ists_gauss.F90
!||--- called by ------------------------------------------------------
!||    sts_contact_eval_pair   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
      subroutine sts_gausspt(norder, xtab, weight)
!-----------------------------------------------
!   Provides Gauss-Legendre quadrature points and weights
!   for integration on [-1,1]
!
!   Parameters:
!     norder: Number of integration points (1-10)
!     xtab: Output array of quadrature points
!     weight: Output array of quadrature weights
!-----------------------------------------------
      implicit none
      integer, intent(in)    :: norder
      integer :: i
      real*8, intent(inout)  :: xtab(norder), weight(norder)
      
      real*8, parameter :: ONE = 1.0d0
      real*8, parameter :: SQRT3 = 1.73205080756887729352d0
      
!-----------------------------------------------
!   Initialize arrays
!-----------------------------------------------
      DO i=1,norder
        xtab(i) = 0.0d0
        weight(i) = 0.0d0
      ENDDO
      
!-----------------------------------------------
!   Set quadrature points and weights based on order
!-----------------------------------------------
      IF (norder == 1) THEN
        xtab(1) = 0.0d0
        weight(1) = 2.0d0
        
      ELSE IF (norder == 2) THEN
        xtab(1) = -ONE/SQRT3
        xtab(2) = ONE/SQRT3
        weight(1) = ONE
        weight(2) = ONE
        
      ELSE IF (norder == 3) THEN
        xtab(1) = -0.774596669241483377d0
        xtab(2) = 0.0d0
        xtab(3) = 0.774596669241483377d0
        weight(1) = 5.0d0/9.0d0
        weight(2) = 8.0d0/9.0d0
        weight(3) = 5.0d0/9.0d0
        
      ELSE IF (norder == 4) THEN
        xtab(1) = -0.861136311594052575d0
        xtab(2) = -0.339981043584856264d0
        xtab(3) = 0.339981043584856264d0
        xtab(4) = 0.861136311594052575d0
        weight(1) = 0.347854845137453857d0
        weight(2) = 0.652145154862546142d0
        weight(3) = 0.652145154862546142d0
        weight(4) = 0.347854845137453857d0
        
      ELSE IF (norder == 5) THEN
        xtab(1) = -0.906179845938663992d0
        xtab(2) = -0.538469310105683091d0
        xtab(3) = 0.0d0
        xtab(4) = 0.538469310105683091d0
        xtab(5) = 0.906179845938663992d0
        weight(1) = 0.236926885056189087d0
        weight(2) = 0.478628670499366468d0
        weight(3) = 0.568888888888888888d0
        weight(4) = 0.478628670499366468d0
        weight(5) = 0.236926885056189087d0
        
      ELSE
        ! Unsupported order - fallback to 3-point rule
        IF (norder > 0 .AND. norder <= 10) THEN
          ! Use 3-point rule for unsupported orders
          xtab(1) = -0.774596669241483377d0
          xtab(2) = 0.0d0
          xtab(3) = 0.774596669241483377d0
          weight(1) = 5.0d0/9.0d0
          weight(2) = 8.0d0/9.0d0
          weight(3) = 5.0d0/9.0d0
        ELSE
          ! Invalid order - set to default
          xtab(1) = 0.0d0
          weight(1) = 2.0d0
        END IF
      ENDIF
      
      RETURN
      END