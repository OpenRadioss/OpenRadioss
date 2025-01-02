!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    velrot_explicit_mod   ../engine/source/constraints/general/rbody/velrot_explicit.F90
      !||--- called by ------------------------------------------------------
      !||    rbe2v1                ../engine/source/constraints/general/rbe2/rbe2v.F
      !||    rbe2vl1               ../engine/source/constraints/general/rbe2/rbe2v.F
      !||    rgbodv                ../engine/source/constraints/general/rbody/rgbodv.F
      !||====================================================================
      module velrot_explicit_mod

      contains
! ======================================================================================================================
!                                                   PROCEDURES
!=======================================================================================================================
!!\brief This subroutine compute displacement due to finit rotation (no more precise w/ cross-product) 
!=======================================================================================================================
      !||====================================================================
      !||    velrot_explicit   ../engine/source/constraints/general/rbody/velrot_explicit.F90
      !||--- called by ------------------------------------------------------
      !||    rbe2v1            ../engine/source/constraints/general/rbe2/rbe2v.F
      !||    rbe2vl1           ../engine/source/constraints/general/rbe2/rbe2v.F
      !||    rgbodv            ../engine/source/constraints/general/rbody/rgbodv.F
      !||--- calls      -----------------------------------------------------
      !||    cross_product     ../engine/source/constraints/general/rbody/velrot_explicit.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod      ../common_source/modules/constant_mod.F
      !||====================================================================
      subroutine velrot_explicit(vr,lsm,vs,dt)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use constant_mod , only : one,zero,em06,em08,em20
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        my_real, intent(in   ) ,dimension(3)                  :: vr           !< rotational velocity
        my_real, intent(in   ) ,dimension(3)                  :: lsm          !< arm length
        my_real, intent(in   )                                :: dt           !< time step
        my_real, intent(inout) ,dimension(3)                  :: vs           !< nodal velocity 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer  i , j , k
        my_real ang2, rz(3,3), localz(3), l2, localx(3), lsmuni(3),trans(3,3),       &
                localy(3), lsmlocal(3),lsmltr(3),lsmgtr(3),norm,vs2,angelv,vrm(3)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
       call cross_product(vr,lsm,vs)
       vrm(1:3) = vr(1:3)*dt
       ang2 = vrm(1)*vrm(1)+vrm(2)*vrm(2)+vrm(3)*vrm(3)
       vs2 = (vs(1)*vs(1)+vs(2)*vs(2)+vs(3)*vs(3))*dt*dt  !ds
       if ( ang2 > em06 .and. vs2 > em08) then 
         angelv = sqrt(ang2)
         norm = one/max(em20,angelv)
         localz(1:3) = norm*vrm(1:3)
         norm = dt/sqrt(vs2)
         localx(1:3) = norm*vs(1:3)
         call cross_product(localz,localx,localy)
         trans(1,1:3) = localx(1:3)
         trans(2,1:3) = localy(1:3)
         trans(3,1:3) = localz(1:3)
         lsmlocal(1:3) = trans(1:3,1)*lsm(1)+trans(1:3,2)*lsm(2)+trans(1:3,3)*lsm(3)
         rz(1,1) = cos(angelv)
         rz(1,2) = sin(angelv)
         rz(2,1) = -rz(1,2)
         rz(2,2) = rz(1,1)
         rz(3,3) = one
         rz(1:2,3) = zero
         rz(3,1:2) = zero
         lsmltr(1:3) = rz(1,1:3)*lsmlocal(1)+rz(2,1:3)*lsmlocal(2)+rz(3,1:3)*lsmlocal(3)
         lsmgtr(1:3) = trans(1,1:3)*lsmltr(1)+trans(2,1:3)*lsmltr(2)+trans(3,1:3)*lsmltr(3)
         vs(1:3) = (lsmgtr(1:3) - lsm(1:3))/dt   ! velocity now
       end if

       end subroutine velrot_explicit
!=======================================================================================================================
!!\brief This subroutine compute cross-product Z = X (x) Y 
!=======================================================================================================================
      !||====================================================================
      !||    cross_product     ../engine/source/constraints/general/rbody/velrot_explicit.F90
      !||--- called by ------------------------------------------------------
      !||    velrot_explicit   ../engine/source/constraints/general/rbody/velrot_explicit.F90
      !||====================================================================
      subroutine cross_product(x,y,z)
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        my_real, intent(in   ) ,dimension(3)                  :: x            !< vector x
        my_real, intent(in   ) ,dimension(3)                  :: y            !< vector y
        my_real, intent(inout) ,dimension(3)                  :: z            !< vector z 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        z(1) =  x(2)*y(3) - y(2)*x(3)
        z(2) = -x(1)*y(3) + y(1)*x(3)
        z(3) =  x(1)*y(2) - y(1)*x(2)

      end subroutine cross_product
!
     end module velrot_explicit_mod
