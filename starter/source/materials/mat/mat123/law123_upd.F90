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
!||    law123_upd_mod   ../starter/source/materials/mat/mat123/law123_upd.F90
!||--- called by ------------------------------------------------------
!||    updmat           ../starter/source/materials/updmat.F
!||====================================================================
      module law123_upd_mod
        implicit none
      contains
!! \brief update material law 123
!||====================================================================
!||    law123_upd                  ../starter/source/materials/mat/mat123/law123_upd.F90
!||--- called by ------------------------------------------------------
!||    updmat                      ../starter/source/materials/updmat.F
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp_inv       ../starter/source/materials/tools/table_mat_vinterp_inv.F90
!||--- uses       -----------------------------------------------------
!||    table_mat_vinterp_inv_mod   ../starter/source/materials/tools/table_mat_vinterp_inv.F90
!||====================================================================
        subroutine law123_upd(  matparam    )
! ----------------------------------------------------------------------------------------------------------------------
!   M o d u l e s
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use matparam_def_mod
          use table_mat_vinterp_inv_mod , only : table_mat_vinterp_inv
          use precision_mod, only : WP
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   D u m m y   a r g u m e n t s
!-----------------------------------------------
          type(matparam_struct_), target :: matparam
          !!integer, intent(in) :: npropm
          !!real(kind=WP), dimension(npropm), intent(inout) :: pm
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          real(kind=WP) :: nu,g,bulk,lam
          real(kind=WP) :: youngmin,youngini,youngmax,r,thetac,theta,xc
          real(kind=WP) :: xmax,xvec(1,1),yy(1),dydx(1)
          integer :: ipos(1,1),i
!--------------------------------------------------------------------------
!     copy global functions/tables to matparam data structure
!--------------------------------------------------------------------------
!
          !< Get material parameters
          xc = matparam%uparam(14)
          if( xc > zero ) then 
            thetac = matparam%uparam(27)
             ipos(1,1)= 1    
             theta = zero
             xvec(1,1) = zero ! half*sin(two*theta)*xc
             call table_mat_vinterp_inv(matparam%table(1),1,1,ipos(1,1),xvec,yy,dydx) ! yy(1) should normally be zero
             r = thetac  - yy(1) 
            do while (abs(r) > 0.00001) 
                dydx(1) = one + dydx(1)*cos(two*theta)
                theta  = theta + r/dydx(1)
                xvec(1,1) = half*sin(two*theta)*xc
               ! interpolation of inverse of shear function (theta = function of (half*sin(2*theta)*xt)
                call table_mat_vinterp_inv(matparam%table(1),1,1,ipos(1,1),xvec,yy,dydx)
                r = thetac - theta - yy(1)
             end do    
             !< Update material parameters
             ! - misalignment angle
              matparam%uparam(28) = theta 
           endif
          return
        end subroutine law123_upd
      end module law123_upd_mod

