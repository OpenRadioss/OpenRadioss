!||====================================================================
!||    sts_penetr  ../engine/source/interfaces/ists/ists_penetr.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
!-----------------------------------------------
! Penetration computation
!-----------------------------------------------
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
      real*8  xupd(3,8)
      real*8  penetr
      real*8  norm(3)
      real*8  a(3,24)
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