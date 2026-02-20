!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    rate_dependency_parameters         ../engine/source/materials/mat/mat132/rate_dependency_parameters.F90
!||--- called by ------------------------------------------------------
!||    sigeps132c             ../engine/source/materials/mat/mat132/sigeps132c.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||====================================================================
      module rate_dependency_parameters_mod
      contains
! ======================================================================================================================
!                                                   SUBROUTINE
! ======================================================================================================================    
        subroutine rate_dependency_parameters( nel, matparam, strain_rate, vartmp , nvartmp,&
                                           xt    ,   xc,   yt,   yc,  sl,  &
                                           gxt,   gxc,  gyt,   gyc, gsl, &
                                           xt0,  xc0, gxt0, gxc0,       &
                                           eta_l , st , theta_c , g_ratio)
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
          real(kind=wp), dimension(nel), intent(inout) :: gxt !< tensile fiber fracture energy
          real(kind=wp), dimension(nel), intent(inout) :: gxc !< compressive fiber fracture energy
          real(kind=wp), dimension(nel), intent(inout) :: gyt !< tensile matrix fracture energy
          real(kind=wp), dimension(nel), intent(inout) :: gyc !< compressive matrix fracture energy
          real(kind=wp), dimension(nel), intent(inout) :: gsl !< shear fracture energy
          real(kind=wp), dimension(nel), intent(inout) :: xt0  !< tensile fiber fracture energy for bilinear softening
          real(kind=wp), dimension(nel), intent(inout) :: xc0 !< compressive fiber fracture energy for bilinear softening
          real(kind=wp), dimension(nel), intent(inout) :: gxt0 !< tensile fiber fracture energy for bilinear softening
          real(kind=wp), dimension(nel), intent(inout) :: gxc0 !< compressive fiber fracture energy for bilinear softening
          real(kind=wp), dimension(nel), intent(inout) :: eta_l !< length scale parameter
          real(kind=wp), dimension(nel), intent(inout) :: st    !< shear threshold for frictional sliding
          real(kind=wp), dimension(nel), intent(inout) :: theta_c !< compressive friction angle
          real(kind=wp), dimension(nel), intent(inout) :: g_ratio !< ratio of shear to normal fracture energy
          !
          type(matparam_struct_), intent(in) :: matparam !< material parameters data
! ----------------------------------------------------------------------------------------------------------------------
!                                                   L o c a l   V a r i a b l e s
! ----------------------------------------------------------------------------------------------------------------------   
         integer :: ipos(nel,1), func,nfunc,i
         real(kind=wp) :: xvec(nel,1),dydx(nel),yy(nel)
         real(kind=wp) :: r, ang0, aa, cos_2ang0, cos_ang0, sin_ang0, tan_2ang0,&
                          tmp,cos2_ang0
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
        if(nfunc >= 7)func = matparam%table(7)%notable! gxt
        if(func /= 0) then
          ipos(:,1) = vartmp(:,7)
          call table_mat_vinterp(matparam%table(7),nel,nel,ipos,xvec,yy,dydx)
          gxt(1:nel) = yy(1:nel)
          vartmp(:,7)=ipos(:,1)
        endif 
        func = 0 
        if(nfunc >= 8)func = matparam%table(8)%notable! gxc
        if(func /= 0) then
          ipos(:,1) = vartmp(:,8)
          call table_mat_vinterp(matparam%table(8),nel,nel,ipos,xvec,yy,dydx)
          gxc(1:nel) = yy(1:nel)
          vartmp(:,8)=ipos(:,1)
        endif  
        func = 0 
        if(nfunc >= 9)func = matparam%table(9)%notable ! gyt
        if(func /= 0) then
          ipos(:,1) = vartmp(:,9)
          call table_mat_vinterp(matparam%table(9),nel,nel,ipos,xvec,yy,dydx)
          gyt(1:nel) = yy(1:nel)
          vartmp(:,9)=ipos(:,1)
        endif 
        func = 0 
        if(nfunc >= 10)func = matparam%table(10)%notable! gyc
        if(func /= 0) then
          ipos(:,1) = vartmp(:,10)
          call table_mat_vinterp(matparam%table(10),nel,nel,ipos,xvec,yy,dydx)
          gyc(1:nel) = yy(1:nel)
          vartmp(:,10)=ipos(:,1)
        endif  
        func = 0 
        if(nfunc >= 11)func = matparam%table(11)%notable ! gsl
        if(func /= 0) then
          ipos(:,1) = vartmp(:,11)
          call table_mat_vinterp(matparam%table(11),nel,nel,ipos,xvec,yy,dydx)
          gsl(1:nel) = yy(1:nel)
          vartmp(:,11)=ipos(:,1)
        endif  

        if(nfunc >= 12) func =  matparam%table(12)%notable  ! xt0
      ! xt0 rate computation
        if(func /= 0) then
          ipos(:,1) = vartmp(:,12)
          call table_mat_vinterp(matparam%table(12),nel,nel,ipos,xvec,yy,dydx)
          xt0(1:nel) = yy(1:nel)
          vartmp(:,12)=ipos(:,1)
        endif   !
       ! xc0 rate computaion
        func = 0 
        if(nfunc >= 13)func= matparam%table(13)%notable
        if(func/= 0) then
          ipos(:,1) = vartmp(:,13)
          call table_mat_vinterp(matparam%table(13),nel,nel,ipos,xvec,yy,dydx)
          xc0(1:nel) = yy(1:nel)
          vartmp(:,13)=ipos(:,1)
        endif   
          func = 0 
        if(nfunc >= 14)func = matparam%table(14)%notable! gxt0
        if(func /= 0) then
          ipos(:,1) = vartmp(:,14)
          call table_mat_vinterp(matparam%table(14),nel,nel,ipos,xvec,yy,dydx)
          gxt0(1:nel) = yy(1:nel)
          vartmp(:,14)=ipos(:,1)
        endif 
        func = 0 
        if(nfunc >= 15)func = matparam%table(15)%notable! gxc
        if(func /= 0) then
          ipos(:,1) = vartmp(:,15)
          call table_mat_vinterp(matparam%table(15),nel,nel,ipos,xvec,yy,dydx)
          gxc0(1:nel) = yy(1:nel)
          vartmp(:,15)=ipos(:,1)
        endif  
       ! computing material parameters for failure criteria
        ang0 = matparam%uparam(27)
        cos_2ang0 = cos(two*ang0)
        cos_ang0 = cos(ang0)
        sin_ang0 = sin(ang0)
        tan_2ang0 = tan(two*ang0)
        cos2_ang0 = cos_ang0**2
        do i=1,nel
            eta_l(i)= -sl(i)*cos_2ang0/yc(i)/cos2_ang0 
            st(i)   = yc(i)*cos_ang0*(sin_ang0 + cos_ang0/tan_2ang0)
            tmp = sl(i)/xc(i)
            theta_c(i) = atan((one - sqrt(one - four*(tmp + eta_l(i))*tmp))  &
                                  / (two*(tmp + eta_l(i))))
           g_ratio(i) =  gyc(i)/gxc(i) ! gyt(i)/ gsl (i)
        end do 
       end subroutine rate_dependency_parameters
     end module rate_dependency_parameters_mod
