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
!||    m36init_mod   ../starter/source/materials/mat/mat132/m132init.F90
!||--- called by ------------------------------------------------------
!||    matini        ../starter/source/materials/mat_share/matini.F
!||====================================================================
      module m132init_mod

      contains
!! \brief initialize state variables (UVAR) in material law132
!||====================================================================
!||    m36init         ../starter/source/materials/mat/mat132/m132init.F90
!||--- called by ------------------------------------------------------
!||    matini          ../starter/source/materials/mat_share/matini.F
!||--- calls      -----------------------------------------------------
!||    finter          ../starter/source/tools/curve/finter.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      subroutine m132init(nel    ,nuvar  ,uvar   ) 
! ------------------------------------------------------------------------------
!           Modules
! ------------------------------------------------------------------------------
          use constant_mod , only : zero,one
          use precision_mod, only : wp
! ------------------------------------------------------------------------------
          implicit none
! ------------------------------------------------------------------------------
!          A r g u m e n t s
! ------------------------------------------------------------------------------
          integer       ,intent(in)    :: nel
          integer       ,intent(in)    :: nuvar
          real(kind=WP) ,intent(inout) :: uvar(nel,nuvar)
! ------------------------------------------------------------------------------
!         Local variables
! ------------------------------------------------------------------------------
!===============================================================================
          uvar(1:nel,1:nuvar ) = zero
!-------------
          return
        end subroutine m132init
      end module m132init_mod
