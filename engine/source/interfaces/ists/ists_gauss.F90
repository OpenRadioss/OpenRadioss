!||====================================================================
!||    sts_gausspt  ../engine/source/interfaces/ists/ists_gauss.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
!-----------------------------------------------
! Gaussian quadrature points and weights
!-----------------------------------------------
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
      integer norder, i
      real*8 xtab(norder), weight(norder)
      
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