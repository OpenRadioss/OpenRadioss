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
! Project point to surface parameterization
!-----------------------------------------------
!||====================================================================
!||    sts_project             ../engine/source/interfaces/ists/ists_projection.F90
!||--- called by ------------------------------------------------------
!||    sts_contact_eval_pair   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||--- calls      -----------------------------------------------------
!||    sts_pos                 ../engine/source/interfaces/ists/ists_pos.F90
!||    sts_shape               ../engine/source/interfaces/ists/ists_shape_fct.F90
!||--- called by ----------------------------------------------------- 
!||    STS_CONTACT_EVAL_PAIR    ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
      subroutine sts_project(xupd, xi1, xi2, eta1, eta2, &
     &     xi1_guess, xi2_guess, use_guess)

!-----------------------------------------------
!   M o d u l e s   /   I m p l i c i t   T y p e s
!-----------------------------------------------
      use constant_mod
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!     xupd   : Coordinates of contact element (3,8)
!     xi1,xi2: Output parametric coordinates on Primary surface
!     eta1,eta2: Input parametric coordinates on Secondary surface
!     xi1_guess, xi2_guess: Warm-start coordinates when use_guess is true
!     use_guess: If true, start Newton from xi1_guess/xi2_guess
!-----------------------------------------------          
      real*8, intent(in)    :: xupd(3,8)
      real*8, intent(inout) :: xi1, xi2
      real*8, intent(in)    :: eta1, eta2
      real*8, intent(in)    :: xi1_guess, xi2_guess
      logical, intent(in)   :: use_guess
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER, PARAMETER :: STS_PROJ_MAX_ITER = 10
      real*8, PARAMETER :: STS_PROJ_TOL = 1.d-10

      INTEGER i, j, iter
      real*8  shape(3,4), N_eta(3,4)
      real*8  rho(3), rhoxi1(3), rhoxi2(3)
      real*8  xsl(3), dxi1, dxi2
      real*8  e, f(2)
      real*8  m_ij(2,2), detmPrimary
!-----------------------------------------------
!   Initialization - set initial guess
!-----------------------------------------------
      IF (use_guess) THEN
        xi1 = xi1_guess
        xi2 = xi2_guess
      ELSE
        xi1 = 0.d0
        xi2 = 0.d0
      ENDIF
      
!-----------------------------------------------
!   Get shape functions for Secondary surface
!-----------------------------------------------
      call sts_shape(eta1, eta2, N_eta)

!-----------------------------------------------
!   Compute Secondary point position in global coords
!-----------------------------------------------
      xsl(1) = 0.d0
      xsl(2) = 0.d0
      xsl(3) = 0.d0
      DO j=1,4
        xsl(1) = xsl(1) + N_eta(1,j)*xupd(1,j+4)
        xsl(2) = xsl(2) + N_eta(1,j)*xupd(2,j+4)
        xsl(3) = xsl(3) + N_eta(1,j)*xupd(3,j+4)
      ENDDO

!-----------------------------------------------
!   Newton's iteration for projection
!-----------------------------------------------
      DO iter=1, STS_PROJ_MAX_ITER
        call sts_shape(xi1, xi2, shape)

        DO i=1,3
          rho(i) = 0.d0
          rhoxi1(i) = 0.d0
          rhoxi2(i) = 0.d0
          DO j=1,4
            rho(i) = rho(i) + shape(1,j)*xupd(i,j)
            rhoxi1(i) = rhoxi1(i) + shape(2,j)*xupd(i,j)
            rhoxi2(i) = rhoxi2(i) + shape(3,j)*xupd(i,j)
          ENDDO
        ENDDO

        m_ij(1,1) = 0.d0
        m_ij(1,2) = 0.d0
        m_ij(2,1) = 0.d0
        m_ij(2,2) = 0.d0
        DO i=1,3
          m_ij(1,1) = m_ij(1,1) + rhoxi1(i)*rhoxi1(i)
          m_ij(1,2) = m_ij(1,2) + rhoxi1(i)*rhoxi2(i)
          m_ij(2,1) = m_ij(2,1) + rhoxi1(i)*rhoxi2(i)
          m_ij(2,2) = m_ij(2,2) + rhoxi2(i)*rhoxi2(i)
        ENDDO
        detmPrimary = m_ij(1,1)*m_ij(2,2) - m_ij(2,1)*m_ij(1,2)

        ! Projection residual and bilinear curvature correction
        e = 0.d0
        f(1) = 0.d0
        f(2) = 0.d0
        DO i=1,3
          e = e + (xupd(i,1)-xupd(i,2)+xupd(i,3)-xupd(i,4))* &
     &          (xsl(i)-rho(i))*0.25d0
          f(1) = f(1) + (xsl(i)-rho(i))*rhoxi1(i)
          f(2) = f(2) + (xsl(i)-rho(i))*rhoxi2(i)
        ENDDO
      
        detmPrimary = detmPrimary - e**2 + 2.d0*m_ij(1,2)*e
        
        IF (DABS(detmPrimary) .LT. EM30) EXIT
        dxi1 = (m_ij(2,2)*f(1) + (e-m_ij(1,2))*f(2))/detmPrimary
        dxi2 = (m_ij(1,1)*f(2) + (e-m_ij(2,1))*f(1))/detmPrimary
        
        xi1 = xi1 + dxi1
        xi2 = xi2 + dxi2

        IF (DABS(dxi1) + DABS(dxi2) .LT. STS_PROJ_TOL) EXIT
      ENDDO
      
      RETURN
      END
