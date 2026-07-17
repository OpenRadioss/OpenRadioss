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
!||    m36init_mod   ../starter/source/materials/mat/mat036/m36init.F90
!||--- called by ------------------------------------------------------
!||    matini        ../starter/source/materials/mat_share/matini.F
!||====================================================================
      module m36init_mod

      contains
!! \brief initialize state variables (UVAR) in material law36
!||====================================================================
!||    m36init         ../starter/source/materials/mat/mat036/m36init.F90
!||--- called by ------------------------------------------------------
!||    matini          ../starter/source/materials/mat_share/matini.F
!||--- calls      -----------------------------------------------------
!||    finter          ../starter/source/tools/curve/finter.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine m36init(mat_param,nel    ,nuvar  ,uvar   ,yldfac ) 
! ------------------------------------------------------------------------------
!           Modules
! ------------------------------------------------------------------------------
          use constant_mod , only : one
          use precision_mod, only : wp
          use matparam_def_mod
! ------------------------------------------------------------------------------
          implicit none
! ------------------------------------------------------------------------------
!          A r g u m e n t s
! ------------------------------------------------------------------------------
          integer       ,intent(in)    :: nel
          integer       ,intent(in)    :: nuvar
          real(kind=WP) ,intent(in)    :: yldfac(nel)
          real(kind=WP) ,intent(inout) :: uvar(nel,nuvar)
          type (matparam_struct_) ,intent(in) :: mat_param
! ------------------------------------------------------------------------------
!         Local variables
! ------------------------------------------------------------------------------
          integer :: vp
          real(kind=WP) :: yld,pfac
!===============================================================================
!         calculate initial yield and save in state variable if vp==1
!------------------------------------------
          vp = mat_param%iparam(3)
          if (vp == 1) then
            if (mat_param%table(2)%notable > 0) then                                    
              pfac = mat_param%table(2)%y1d(1)
            else                   
              pfac = one
            endif                                                 
            if (mat_param%table(1)%ndim == 1) then
              yld = mat_param%table(1)%y1d(1)
            else if (mat_param%table(1)%ndim == 2) then
              yld = mat_param%table(1)%y2d(1,1)
            end if
            uvar(1:nel,3) = pfac*yld*yldfac(1:nel)
          endif                                                   
!-------------
          return
        end subroutine m36init
      end module m36init_mod
