!||====================================================================
!||    sts_surfgeom  ../engine/source/interfaces/ists/ists_sufgeom.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
!-----------------------------------------------
! Surface geometry calculation
!-----------------------------------------------
      subroutine sts_surfgeom(xupd, daxi1, daxi2, daeta1, daeta2, &
     &                      norm, rhoxi1, rhoxi2, m_ij, detm, mij, detmPrimary)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!     xupd: Element coordinates
!     daxi*, daeta*: Derivative matrices
!     norm: Output normal vector
!     rhoxi*: Output surface tangent vectors
!     m_ij, detm, mij, detmPrimary: Output metric parameters
!-----------------------------------------------
      real*8  xupd(3,8)
      real*8  daxi1(3,24), daxi2(3,24), daeta1(3,24), daeta2(3,24)
      real*8  rhoxi1(3), rhoxi2(3), reta1(3), reta2(3)
      real*8  m_ij(2,2), detm, mij(2,2), mSecondary_ij(2,2), detmPrimary
      real*8  norm(3)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER i, j, k
      real*8  inv_sqrt_detm
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
!   Calculate Secondary surface tangent vectors
!-----------------------------------------------
      DO i=1,3
        reta1(i) = 0.d0
        reta2(i) = 0.d0
        DO j=1,24
          reta1(i) = reta1(i) - daeta1(i,j)*xupd1d(j)
          reta2(i) = reta2(i) - daeta2(i,j)*xupd1d(j)
        ENDDO
      ENDDO

!-----------------------------------------------
!   Calculate Secondary surface metric tensor
!-----------------------------------------------
      mSecondary_ij(1,1) = 0.d0
      mSecondary_ij(1,2) = 0.d0
      mSecondary_ij(2,1) = 0.d0
      mSecondary_ij(2,2) = 0.d0

      DO i=1,3
        mSecondary_ij(1,1) = mSecondary_ij(1,1) + reta1(i)**2
        mSecondary_ij(1,2) = mSecondary_ij(1,2) + reta1(i)*reta2(i)
        mSecondary_ij(2,1) = mSecondary_ij(2,1) + reta1(i)*reta2(i)
        mSecondary_ij(2,2) = mSecondary_ij(2,2) + reta2(i)**2
      ENDDO

      detm = mSecondary_ij(1,1)*mSecondary_ij(2,2) - &
     &       mSecondary_ij(2,1)*mSecondary_ij(1,2)

!-----------------------------------------------
!   Calculate Primary surface tangent vectors
!-----------------------------------------------
      DO i=1,3
        rhoxi1(i) = 0.d0
        rhoxi2(i) = 0.d0
        DO j=1,24
          rhoxi1(i) = rhoxi1(i) - daxi1(i,j)*xupd1d(j)
          rhoxi2(i) = rhoxi2(i) - daxi2(i,j)*xupd1d(j)
        ENDDO
      ENDDO

!-----------------------------------------------
!   Calculate Primary surface metric tensor
!-----------------------------------------------
      m_ij(1,1) = 0.d0
      m_ij(1,2) = 0.d0
      m_ij(2,1) = 0.d0
      m_ij(2,2) = 0.d0

      DO i=1,3
        m_ij(1,1) = m_ij(1,1) + rhoxi1(i)**2
        m_ij(1,2) = m_ij(1,2) + rhoxi1(i)*rhoxi2(i)
        m_ij(2,1) = m_ij(2,1) + rhoxi1(i)*rhoxi2(i)
        m_ij(2,2) = m_ij(2,2) + rhoxi2(i)**2
      ENDDO

      detmPrimary = m_ij(1,1)*m_ij(2,2) - m_ij(2,1)*m_ij(1,2)

!-----------------------------------------------
!   Calculate inverse metric tensor
!-----------------------------------------------
      ! Safety check for division by zero
      IF (dabs(detmPrimary) .LT. 1.0d-12) THEN
        ! Use identity matrix if determinant is too small
        mij(1,1) = 1.0d0
        mij(1,2) = 0.d0
        mij(2,1) = 0.d0
        mij(2,2) = 1.0d0
      ELSE
        mij(1,1) = m_ij(2,2) / detmPrimary
        mij(1,2) = -m_ij(1,2) / detmPrimary
        mij(2,1) = -m_ij(2,1) / detmPrimary
        mij(2,2) = m_ij(1,1) / detmPrimary
      ENDIF

!-----------------------------------------------
!   Calculate normal vector (cross product of tangents)
!-----------------------------------------------
      ! Safety check for zero determinant
      IF (dabs(detmPrimary) .LT. 1.0d-12) THEN
        ! Default normal if surface is degenerate
        norm(1) = 0.d0
        norm(2) = 0.d0
        norm(3) = 1.0d0
      ELSE
        inv_sqrt_detm = 1.0d0 / dsqrt(detmPrimary)
        
        ! Normal = (rhoxi1 × rhoxi2) / |rhoxi1 × rhoxi2|
        norm(1) = (rhoxi1(2)*rhoxi2(3) - rhoxi2(2)*rhoxi1(3)) * inv_sqrt_detm
        norm(2) = (rhoxi1(3)*rhoxi2(1) - rhoxi2(3)*rhoxi1(1)) * inv_sqrt_detm
        norm(3) = (rhoxi1(1)*rhoxi2(2) - rhoxi2(1)*rhoxi1(2)) * inv_sqrt_detm
      ENDIF

      RETURN
      END