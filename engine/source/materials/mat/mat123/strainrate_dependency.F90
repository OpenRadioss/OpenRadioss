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
        subroutine strainrate_dependency( nel, matparam, strain_rate, vartmp , nvartmp,&
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
          !
          type(matparam_struct_), intent(in) :: matparam !< material parameters data
! ----------------------------------------------------------------------------------------------------------------------
!                                                   L o c a l   V a r i a b l e s
! ----------------------------------------------------------------------------------------------------------------------   
         integer :: ipos(nel,1), func,nfunc,i
         real(kind=wp) :: xvec(nel,1),dydx(nel),yy(nel)
         real(kind=wp) :: r, ang0, aa, thetac, theta,mut
! ----------------------------------------------------------------------------------------------------------------------
!                                                   coding 
! ----------------------------------------------------------------------------------------------------------------------     
        nfunc =  matparam%ntable 
        xvec(1:nel,1) =strain_rate(1:nel) 
        func = 0 
        if(nfunc >= 2) func =  matparam%table(2)%notable 
      ! xt rate computation
        if(func /= 0) then
          ipos(:,1) = vartmp(:,2)
          call table_mat_vinterp(matparam%table(2),nel,nel,ipos,xvec,yy,dydx)
          xt(1:nel) = yy(1:nel)
          vartmp(:,2)=ipos(:,1)
        endif   !
       ! xc rate computaion
        func = 0 
        if(nfunc >= 3)func= matparam%table(3)%notable
        if(func/= 0) then
          ipos(:,1) = vartmp(:,3)
          call table_mat_vinterp(matparam%table(3),nel,nel,ipos,xvec,yy,dydx)
          xc(1:nel) = yy(1:nel)
          vartmp(:,3)=ipos(:,1)
        endif   
        func = 0 
        if(nfunc >= 4)func= matparam%table(4)%notable
      !yt rate computation
        if(func /= 0) then
          ipos(:,1) = vartmp(:,4)
          call table_mat_vinterp(matparam%table(4),nel,nel,ipos,xvec,yy,dydx)
          yt(1:nel) = yy(1:nel)
          vartmp(:,4)=ipos(:,1)
        endif   !
       ! xc rate computation
        func = 0 
        if(nfunc >= 5)func= matparam%table(5)%notable
        if(func/= 0) then
          ipos(:,1) = vartmp(:,5)
          call table_mat_vinterp(matparam%table(5),nel,nel,ipos,xvec,yy,dydx)
          yc(1:nel) = yy(1:nel)
          vartmp(:,5)=ipos(:,1)
        endif  
        func = 0 
        if(nfunc >= 6)func= matparam%table(6)%notable
        if(func /= 0) then
          ipos(:,1) = vartmp(:,6)
          call table_mat_vinterp(matparam%table(6),nel,nel,ipos,xvec,yy,dydx)
          sl(1:nel) = yy(1:nel)
          vartmp(:,6)=ipos(:,1)
        endif  ! 
         func = 0 
        if(nfunc >= 7)func = matparam%table(7)%notable! enkink
        if(func /= 0) then
          ipos(:,1) = vartmp(:,7)
          call table_mat_vinterp(matparam%table(7),nel,nel,ipos,xvec,yy,dydx)
          enkink(1:nel) = yy(1:nel)
          vartmp(:,7)=ipos(:,1)
        endif 
        func = 0 
        if(nfunc >= 8)func = matparam%table(8)%notable! enkink
        if(func /= 0) then
          ipos(:,1) = vartmp(:,8)
          call table_mat_vinterp(matparam%table(8),nel,nel,ipos,xvec,yy,dydx)
          ena(1:nel) = yy(1:nel)
          vartmp(:,8)=ipos(:,1)
        endif  
        func = 0 
        if(nfunc >= 9)func = matparam%table(9)%notable ! enkink
        if(func /= 0) then
          ipos(:,1) = vartmp(:,9)
          call table_mat_vinterp(matparam%table(9),nel,nel,ipos,xvec,yy,dydx)
          enb(1:nel) = yy(1:nel)
          vartmp(:,9)=ipos(:,1)
        endif 
        func = 0 
        if(nfunc >= 10)func = matparam%table(10)%notable! enkink
        if(func /= 0) then
          ipos(:,1) = vartmp(:,10)
          call table_mat_vinterp(matparam%table(10),nel,nel,ipos,xvec,yy,dydx)
          ent(1:nel) = yy(1:nel)
          vartmp(:,10)=ipos(:,1)
        endif  
        func = 0 
        if(nfunc >= 11)func = matparam%table(11)%notable ! enkink
        if(func /= 0) then
          ipos(:,1) = vartmp(:,11)
          call table_mat_vinterp(matparam%table(11),nel,nel,ipos,xvec,yy,dydx)
          enl(1:nel) = yy(1:nel)
          vartmp(:,11)=ipos(:,1)
        endif  
       ! computing material parameters for failure criteria
         mut  = matparam%uparam(24)  
         ang0 = matparam%uparam(26)
         aa =  one/tan(ang0)  
       !! mut = -one/tan(two*ang0)
        do i=1,nel
          st(i) = half*aa*yc(i) ! st = half*yc/tan(ang0)
          mul(i) = sl(i)*mut/st(i) ! mul = -sl/st/tan(2*ang0)
          thetac = two*(sl(i)/xc(i) + mul(i)) 
          thetac = (one - sqrt(one - two*(thetac)*sl(i)/xc(i))) / thetac    
          thetac = atan(thetac)  
          ! computing the initial misalignment angle 
          ipos(1,1)= 1   
          theta = zero
          xvec(1,1) = zero !  half*sin(two*theta)*xc
          call table_mat_vinterp_inv(matparam%table(1),1,1,ipos(1,1),xvec,yy,dydx)
          r = thetac  - yy(1) ! normally yy(1) = zero
          do while (abs(r) > 0.00001) 
            dydx(1) = one + dydx(1)*cos(two*theta)
            theta  = theta + r/dydx(1)
            xvec(1,1) = half*sin(two*theta)*xc(i)
            ! interpolation of inverse of shear function (theta = function of (half*sin(2*theta)*xt)
            call table_mat_vinterp_inv(matparam%table(1),1,1,ipos(1,1),xvec,yy,dydx)
            r = thetac - theta - yy(1)
          end do    
          !< Update material parameters
          ! - misallignement angle
          thetai(i) = theta 
        enddo   
       end subroutine strainrate_dependency
     end module strainrate_dependency_mod
