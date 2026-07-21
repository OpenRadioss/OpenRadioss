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
! Shape functions for quad element
!-----------------------------------------------
!||====================================================================
!||    sts_shape               ../engine/source/interfaces/ists/ists_shape_fct.F90
!||--- called by ------------------------------------------------------
!||    sts_contact_eval_pair   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||    sts_pos                 ../engine/source/interfaces/ists/ists_pos.F90
!||    sts_project             ../engine/source/interfaces/ists/ists_projection.F90
!||====================================================================
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
      real*8, intent(inout) :: shape(3,4)
      real*8, intent(in)    :: xi1, xi2
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