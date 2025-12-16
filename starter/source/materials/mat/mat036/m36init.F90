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
        subroutine m36init(nel    ,nuparam,nuvar  ,nfunc  ,ifunc  ,yldfac ,          &
          snpc   ,stf    ,npf    ,tf     ,uparam ,uvar   )
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
          integer       ,intent(in)    :: nuparam
          integer       ,intent(in)    :: nuvar
          integer       ,intent(in)    :: nfunc
          integer       ,intent(in)    :: snpc
          integer       ,intent(in)    :: stf
          integer       ,intent(in)    :: ifunc(nfunc)
          integer       ,intent(in)    :: npf(snpc)
          real(kind=WP) ,intent(in)    :: uparam(nuparam)
          real(kind=WP) ,intent(in)    :: yldfac(nel)
          real(kind=WP) ,intent(inout) :: uvar(nel,nuvar)
          real(kind=WP) ,intent(in)    :: tf(stf)
! ------------------------------------------------------------------------------
!         Local variables
! ------------------------------------------------------------------------------
          integer :: ipfun,pfun,nrate,vp
          real(kind=WP) :: dydx,yld,yfac,pfac
          real(kind=WP) ,external :: finter
!===============================================================================
          ipfun  = ifunc(nfunc-1)
          nrate  = nint(uparam(1))
          pfun   = nint(uparam(16+2*nrate))
          vp     = nint(uparam(26+2*nrate))
          yfac   = uparam(7+nrate)
!------------------------------------------
!         calculate initial yield and save in state variable if vp==1
!------------------------------------------
          if (vp == 1) then
            if (pfun > 0) then
              pfac = finter(ipfun ,zero,npf,tf,dydx)
            else
              pfac = one
            endif
            yld = yfac*finter(ifunc(1),zero,npf,tf,dydx)
            uvar(1:nel,3) = yld*pfac*yldfac(1:nel)
          endif
!-------------
          return
        end subroutine m36init
      end module m36init_mod
