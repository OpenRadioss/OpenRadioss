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
!||====================================================================
!||    sts_pos  ../engine/source/interfaces/ists/ists_pos.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||--- calls ---------------------------------------------------------
!||    sts_shape  ../engine/source/interfaces/ists/ists_shape_fct.F90
!||====================================================================
!-----------------------------------------------
! Position matrix assembly
!-----------------------------------------------
!||====================================================================
!||    sts_pos                 ../engine/source/interfaces/ists/ists_pos.F90
!||--- called by ------------------------------------------------------
!||    sts_contact_eval_pair   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||    sts_project             ../engine/source/interfaces/ists/ists_projection.F90
!||--- calls      -----------------------------------------------------
!||    sts_shape               ../engine/source/interfaces/ists/ists_shape_fct.F90
!||====================================================================
      subroutine sts_pos(a, daxi1, daxi2, daeta1, daeta2, xi1, xi2, eta1, eta2)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!     a, daxi1, daxi2, daeta1, daeta2: Output matrices for contact calculation
!     xi1, xi2: Parametric coordinates on Primary surface 
!     eta1, eta2: Parametric coordinates on Secondary surface
!-----------------------------------------------
      real*8, intent(inout) :: a(3,24), daxi1(3,24), daxi2(3,24), daeta1(3,24), daeta2(3,24)
      real*8, intent(in)    :: xi1, xi2, eta1, eta2
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      real*8  N_xi(3,4), N_eta(3,4)
      INTEGER i, j
!-----------------------------------------------
!   Get shape functions at parametric coordinates
!-----------------------------------------------
      call sts_shape(xi1, xi2, N_xi)
      call sts_shape(eta1, eta2, N_eta)
      
!-----------------------------------------------
!   Initialize all arrays to zero
!-----------------------------------------------
      DO i=1,3
        DO j=1,24
          a(i,j) = 0.d0
          daxi1(i,j) = 0.d0
          daxi2(i,j) = 0.d0
          daeta1(i,j) = 0.d0
          daeta2(i,j) = 0.d0
        ENDDO
      ENDDO
      
!-----------------------------------------------
!   Fill position and derivative matrices
!-----------------------------------------------
      ! Primary nodes (1-4): position matrix and derivatives
      DO i=1,4
        ! Position matrix (x, y, z components)
        a(1,(i-1)*3+1) = -N_xi(1,i)
        a(2,(i-1)*3+2) = -N_xi(1,i)
        a(3,(i-1)*3+3) = -N_xi(1,i)
        
        ! Derivatives w.r.t. xi1 and xi2
        daxi1(1,(i-1)*3+1) = -N_xi(2,i)
        daxi2(1,(i-1)*3+1) = -N_xi(3,i)
        daxi1(2,(i-1)*3+2) = -N_xi(2,i)
        daxi2(2,(i-1)*3+2) = -N_xi(3,i)
        daxi1(3,(i-1)*3+3) = -N_xi(2,i)
        daxi2(3,(i-1)*3+3) = -N_xi(3,i)
      ENDDO
      
      ! Secondary nodes (5-8): position matrix and derivatives
      DO i=1,4
        ! Position matrix (x, y, z components)
        a(1,12+(i-1)*3+1) = N_eta(1,i)
        a(2,12+(i-1)*3+2) = N_eta(1,i)
        a(3,12+(i-1)*3+3) = N_eta(1,i)
        
        ! Derivatives w.r.t. eta1 and eta2
        daeta1(1,12+(i-1)*3+1) = N_eta(2,i)
        daeta2(1,12+(i-1)*3+1) = N_eta(3,i)
        daeta1(2,12+(i-1)*3+2) = N_eta(2,i)
        daeta2(2,12+(i-1)*3+2) = N_eta(3,i)
        daeta1(3,12+(i-1)*3+3) = N_eta(2,i)
        daeta2(3,12+(i-1)*3+3) = N_eta(3,i)
      ENDDO

      RETURN
      END