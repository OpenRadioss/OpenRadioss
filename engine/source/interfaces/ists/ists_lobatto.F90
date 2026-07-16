!||====================================================================
!||    sts_lobattopt  ../engine/source/interfaces/ists/ists_lobatto.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
!-----------------------------------------------
! Lobatto quadrature points and weights
!-----------------------------------------------
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
      integer norder, i
      real*8 xtab(norder), weight(norder)

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