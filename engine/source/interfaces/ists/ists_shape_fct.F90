!||====================================================================
!||    sts_shape  ../engine/source/interfaces/ists/ists_shape_fct.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
!-----------------------------------------------
! Shape functions for quad element
!-----------------------------------------------
      subroutine sts_shape(xi1, xi2, shape)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!     xi1,xi2: Parametric coordinates (-1 to 1)
!     shape: Output shape functions and derivatives
!            shape(1,i): Shape function values
!            shape(2,i): Derivatives w.r.t. xi1
!            shape(3,i): Derivatives w.r.t. xi2
!-----------------------------------------------
      real*8  shape(3,4)
      real*8  xi1, xi2
!-----------------------------------------------
!   Bilinear shape functions for quadrilateral element
!-----------------------------------------------
      ! Shape function values - standard bilinear quad
      shape(1,1) = 0.25d0*(1.d0-xi1)*(1.d0-xi2)  ! N1 at (-1,-1)
      shape(1,2) = 0.25d0*(1.d0+xi1)*(1.d0-xi2)  ! N2 at (1,-1)
      shape(1,3) = 0.25d0*(1.d0+xi1)*(1.d0+xi2)  ! N3 at (1,1)
      shape(1,4) = 0.25d0*(1.d0-xi1)*(1.d0+xi2)  ! N4 at (-1,1)

      ! Derivatives w.r.t. xi1
      shape(2,1) = -0.25d0*(1.d0-xi2)  ! dN1/dxi1
      shape(2,2) =  0.25d0*(1.d0-xi2)  ! dN2/dxi1
      shape(2,3) =  0.25d0*(1.d0+xi2)  ! dN3/dxi1
      shape(2,4) = -0.25d0*(1.d0+xi2)  ! dN4/dxi1

      ! Derivatives w.r.t. xi2
      shape(3,1) = -0.25d0*(1.d0-xi1)  ! dN1/dxi2
      shape(3,2) = -0.25d0*(1.d0+xi1)  ! dN2/dxi2
      shape(3,3) =  0.25d0*(1.d0+xi1)  ! dN3/dxi2
      shape(3,4) =  0.25d0*(1.d0-xi1)  ! dN4/dxi2

      RETURN
      END