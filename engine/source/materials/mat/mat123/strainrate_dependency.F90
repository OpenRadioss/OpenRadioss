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
!||    strainrate_dependency_mod   ../engine/source/materials/mat/mat123/strainrate_dependency.F90
!||--- called by ------------------------------------------------------
!||    sigeps123                   ../engine/source/materials/mat/mat123/sigeps123.F90
!||    sigeps123c                  ../engine/source/materials/mat/mat123/sigeps123c.F90
!||====================================================================
      module strainrate_dependency_mod
      contains
! ======================================================================================================================
!                                                   SUBROUTINE
! ======================================================================================================================    
!||====================================================================
!||    strainrate_dependency       ../engine/source/materials/mat/mat123/strainrate_dependency.F90
!||--- called by ------------------------------------------------------
!||    sigeps123                   ../engine/source/materials/mat/mat123/sigeps123.F90
!||    sigeps123c                  ../engine/source/materials/mat/mat123/sigeps123c.F90
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp           ../engine/source/materials/tools/table_mat_vinterp.F
!||    table_mat_vinterp_inv       ../engine/source/materials/tools/table_mat_vinterp_inv.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                ../common_source/modules/constant_mod.F
!||    matparam_def_mod            ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod               ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_inv_mod   ../engine/source/materials/tools/table_mat_vinterp_inv.F90
!||    table_mat_vinterp_mod       ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
        subroutine strainrate_dependency( nel, matparam, strain_rate, lc, vartmp , nvartmp,&
                                           xt    ,   xc,   yt,   yc,  sl,  &
                                           enkink,  ena,  enb,  ent,  enl , &
                                           mul, st, thetai)
         !
        use precision_mod, only : WP 
        use constant_mod 
        use matparam_def_mod 
        use table_mat_vinterp_inv_mod , only : table_mat_vinterp_inv
        use table_mat_vinterp_mod , only : table_mat_vinterp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
         implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ---------------------------------------------------------------------------------------------------------------------- 
          integer, intent(in) :: nel !< number of elements in the group
          integer, intent(in) :: nvartmp !< number of user variables
          integer, dimension(nel,nvartmp), intent(inout) :: vartmp !< user variables temporairy 
          real(kind=wp), dimension(nel), intent(in) :: strain_rate !< strain rate
          real(kind=wp), dimension(nel), intent(inout) :: xt !< tensile fiber
          real(kind=wp), dimension(nel), intent(inout) :: xc !< compressive  fiber
          real(kind=wp), dimension(nel), intent(inout) :: yt !< tensile matrix
          real(kind=wp), dimension(nel), intent(inout) :: yc !< compressive matrix
          real(kind=wp), dimension(nel), intent(inout) :: sl !< shear 
          real(kind=wp), dimension(nel), intent(inout) :: enkink !< enkink rate
          real(kind=wp), dimension(nel), intent(inout) :: ena !< ena rate
          real(kind=wp), dimension(nel), intent(inout) :: enb !< enb rate
          real(kind=wp), dimension(nel), intent(inout) :: ent !< ent rate
          real(kind=wp), dimension(nel), intent(inout) :: enl !< enl rate
          real(kind=wp), dimension(nel), intent(inout) :: mul !< updated mul
          real(kind=wp), dimension(nel), intent(inout) :: st !< updated st
          real(kind=wp), dimension(nel), intent(inout) :: thetai !< misalignment angle
          real(kind=wp), dimension(nel), intent(in)  :: lc 
          !
          type(matparam_struct_), intent(in) :: matparam !< material parameters data
! ----------------------------------------------------------------------------------------------------------------------
!                                                   L o c a l   V a r i a b l e s
! ----------------------------------------------------------------------------------------------------------------------   
         integer :: ipos(nel,1), tab,ntab,i,ipos1(nel,2),dimx
         real(kind=wp) :: xvec(nel,1),dydx(nel),yy(nel),xvec1(nel,2)
         real(kind=wp) :: r, ang0, aa, thetac, theta,mut,g12,bb,cc
! ----------------------------------------------------------------------------------------------------------------------
!                                                   coding 
! ----------------------------------------------------------------------------------------------------------------------     
        ntab =  matparam%ntable 
        xvec(1:nel,1) =strain_rate(1:nel) 
        tab =  matparam%table(7)%notable 
      ! xt rate computation
        if(tab /= 0) then
          ipos(:,1) = vartmp(:,13)
          call table_mat_vinterp(matparam%table(7),nel,nel,ipos,xvec,yy,dydx)
          xt(1:nel) = yy(1:nel)
          vartmp(:,13)=ipos(:,1)
        endif   !
       ! xc rate computaion
        tab= matparam%table(8)%notable
        if(tab /= 0) then
          ipos(:,1) = vartmp(:,14)
          call table_mat_vinterp(matparam%table(8),nel,nel,ipos,xvec,yy,dydx)
          xc(1:nel) = yy(1:nel)
          vartmp(:,14)=ipos(:,1)
        endif   
        tab= matparam%table(9)%notable
      !yt rate computation
        if(tab  /= 0) then
          ipos(:,1) = vartmp(:,15)
          call table_mat_vinterp(matparam%table(9),nel,nel,ipos,xvec,yy,dydx)
          yt(1:nel) = yy(1:nel)
          vartmp(:,15)=ipos(:,1)
        endif   !
       ! yc rate computation
        tab = matparam%table(10)%notable
        if(tab /= 0) then
          ipos(:,1) = vartmp(:,16)
          call table_mat_vinterp(matparam%table(10),nel,nel,ipos,xvec,yy,dydx)
          yc(1:nel) = yy(1:nel)
          vartmp(:,16)=ipos(:,1)
        endif  
        tab= matparam%table(11)%notable
        if(tab /= 0) then
          ipos(:,1) = vartmp(:,17)
          call table_mat_vinterp(matparam%table(11),nel,nel,ipos,xvec,yy,dydx)
          sl(1:nel) = yy(1:nel)
          vartmp(:,17)=ipos(:,1)
        endif  ! 
        tab = matparam%table(2)%notable ! enkink 
        xvec1(:,1)  = lc(:)
        xvec1(:,2)  = strain_rate(:)
        if(tab /= 0) then
          ipos1(:,1)  = vartmp(1:nel,3)
          ipos1(:,2)  = vartmp(1:nel,4)
          call table_mat_vinterp(matparam%table(2),nel,nel,ipos1,xvec1,yy,dydx)
          enkink(1:nel) = yy(1:nel)
          vartmp(:,3)=ipos1(:,1)
          vartmp(:,4)=ipos1(:,2)
        endif 
        tab = matparam%table(3)%notable! ena
        if(tab /= 0) then
          ipos1(:,1)  = vartmp(1:nel,5)
          ipos1(:,2)  = vartmp(1:nel,6)
          call table_mat_vinterp(matparam%table(3),nel,nel,ipos1,xvec1,yy,dydx)
          ena(1:nel) = yy(1:nel)
          vartmp(:,5)=ipos1(:,1)
          vartmp(:,6)=ipos1(:,2)
        endif  
        tab = matparam%table(4)%notable ! enb
        if(tab /= 0) then
          ipos1(:,1)  = vartmp(1:nel,7)
          ipos1(:,2)  = vartmp(1:nel,8)
          call table_mat_vinterp(matparam%table(4),nel,nel,ipos1,xvec1,yy,dydx)
          enb(1:nel) = yy(1:nel)
          vartmp(:,7)=ipos1(:,1)
          vartmp(:,8)=ipos1(:,2)
        endif 
        tab = matparam%table(5)%notable! ent
        if(tab /= 0) then
          ipos1(:,1)  = vartmp(1:nel,9)
          ipos1(:,2)  = vartmp(1:nel,10)
          call table_mat_vinterp(matparam%table(5),nel,nel,ipos1,xvec1,yy,dydx)
          ent(1:nel) = yy(1:nel)
          vartmp(:,9)=ipos1(:,1)
          vartmp(:,10)=ipos1(:,2)
        endif  
       tab = matparam%table(6)%notable ! enl
        if(tab /= 0) then
          ipos1(:,1)  = vartmp(1:nel,11)
          ipos1(:,2)  = vartmp(1:nel,12)
          call table_mat_vinterp(matparam%table(6),nel,nel,ipos1,xvec1,yy,dydx)
          enl(1:nel) = yy(1:nel)
          vartmp(:,11)=ipos1(:,1)
          vartmp(:,12)=ipos1(:,2)
        endif  
       ! computing material parameters for failure criteria
        if(matparam%table(8)%notable > 0 .or. matparam%table(10)%notable > 0   & 
                                         .or. matparam%table(11)%notable > 0 ) then 
         g12 = matparam%uparam(4)
         mut  = matparam%uparam(24)  
         ang0 = matparam%uparam(26)
         aa =  one/tan(ang0)  
         do i=1,nel
          st(i) = half*aa*yc(i) ! st = half*yc/tan(ang0)
          mul(i) = sl(i)*mut/st(i) ! mul = -sl/st/tan(2*ang0)
          bb = two*(sl(i)/xc(i) + mul(i)) 
          cc = one - two*bb*sl(i)/xc(i)
          if(cc >= zero )then
            thetac = (one - sqrt(cc)) / bb    
            thetac = atan(thetac)  
          else
             thetac = sl(i)/g12
          endif 
          !------------------------------------------
          ! computing the initial misalignment angle 
          !------------------------------------------
          tab = matparam%table(1)%notable
          if( tab > 0 ) then 
               dimx =  matparam%table(1)%ndim
               ipos1(1,1)= 1   
               ipos1(1,2) = 1
               theta = zero
               xvec1(1,1) = zero !  half*sin(two*theta)*xc
               xvec1(1,2) = zero
               call table_mat_vinterp_inv(matparam%table(1),dimx,1,ipos1(1,1),xvec1,yy,dydx)
               r = thetac  - yy(1) ! normally yy(1) = zero
              do while (abs(r) > 0.00001) 
                    dydx(1) = one + dydx(1)*xc(i)*cos(two*theta)
                    theta  = theta + r/dydx(1)
                    xvec(1,1) = half*sin(two*theta)*xc(i)
                   ! interpolation of inverse of shear function (theta = function of (half*sin(2*theta)*xt)
                    call table_mat_vinterp_inv(matparam%table(1),dimx,1,ipos(1,1),xvec,yy,dydx)
                    r = thetac - theta - yy(1)
               end do    
          else
               r = thetac 
               theta = zero 
               do while (abs(r) > 0.00001) 
                     !  dydx(1) = one/g12 ! linear fonction 
                     dydx(1) = one + xc(i)*cos(two*theta)/g12
                     theta  = theta + r/dydx(1)
                     xvec(1,1) = half*sin(two*theta)*xc(i)
                     yy(1) = xvec(1,1)/g12
                     r = thetac - theta - yy(1)
               end do  
          endif
          !< Update material parameters
          ! - misallignement angle
          thetai(i) = theta 
        enddo   
        !!stop
       endif ! need to update material parameters for failure criteria
       end subroutine strainrate_dependency
     end module strainrate_dependency_mod
