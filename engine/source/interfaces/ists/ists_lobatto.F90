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
!-----------------------------------------------
! Lobatto quadrature points and weights
!-----------------------------------------------
!||====================================================================
!||    sts_lobattopt           ../engine/source/interfaces/ists/ists_lobatto.F90
!||--- called by ------------------------------------------------------
!||    sts_contact_eval_pair   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
      subroutine sts_lobattopt(norder, xtab, weight)
!-----------------------------------------------
!   Provides Lobatto quadrature points and weights
!   for integration on [-1,1]
!
!   Parameters:
!     norder: Number of integration points (2-10)
!     xtab: Output array of quadrature points
!     weight: Output array of quadrature weights
!-----------------------------------------------
      implicit none
      integer, intent(in)    :: norder
      integer :: i
      real*8, intent(inout)  :: xtab(norder), weight(norder)

      real*8, parameter :: ONE = 1.0d0

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
      IF (norder == 2) THEN
        xtab(1) = -ONE
        xtab(2) = ONE
        weight(1) = ONE
        weight(2) = ONE
        
      ELSE IF (norder == 3) THEN
        xtab(1) = -ONE
        xtab(2) = 0.0d0
        xtab(3) = ONE
        weight(1) = ONE / 3.0d0
        weight(2) = 4.0d0 / 3.0d0
        weight(3) = ONE / 3.0d0
        
      ELSE IF (norder == 4) THEN
        xtab(1) = -ONE
        xtab(2) = -0.447213595499957939d0
        xtab(3) = 0.447213595499957939d0
        xtab(4) = ONE
        weight(1) = ONE / 6.0d0
        weight(2) = 5.0d0 / 6.0d0
        weight(3) = 5.0d0 / 6.0d0
        weight(4) = ONE / 6.0d0
        
      ELSE IF (norder == 5) THEN
        xtab(1) = -ONE
        xtab(2) = -0.654653670707977143d0
        xtab(3) = 0.0d0
        xtab(4) = 0.654653670707977143d0
        xtab(5) = ONE
        weight(1) = ONE / 10.0d0
        weight(2) = 49.0d0 / 90.0d0
        weight(3) = 32.0d0 / 45.0d0
        weight(4) = 49.0d0 / 90.0d0
        weight(5) = ONE / 10.0d0
        
      ELSE
        ! Unsupported order - fallback to 3-point rule
        IF (norder > 0 .AND. norder <= 10) THEN
          xtab(1) = -ONE
          xtab(2) = 0.0d0
          xtab(3) = ONE
          weight(1) = ONE / 3.0d0
          weight(2) = 4.0d0 / 3.0d0
          weight(3) = ONE / 3.0d0
        ELSE
          ! Invalid order - set to default 2-point rule
          xtab(1) = -ONE
          xtab(2) = ONE
          weight(1) = ONE
          weight(2) = ONE
        END IF
      ENDIF
      
      RETURN
      END