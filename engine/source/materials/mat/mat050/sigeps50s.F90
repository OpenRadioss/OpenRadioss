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
!||    sigeps50s_mod   ../engine/source/materials/mat/mat050/sigeps50s.F90
!||--- called by ------------------------------------------------------
!||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8          ../engine/source/materials/mat_share/mulaw8.F90
!||====================================================================
      module sigeps50s_mod
      contains
! ======================================================================================================================
! \brief   Orthotropic honeycomb material with strain rate dependency /MAT/LAW50 (VISC_HONEY)
! \details
! ======================================================================================================================
!||====================================================================
!||    sigeps50s               ../engine/source/materials/mat/mat050/sigeps50s.F90
!||--- called by ------------------------------------------------------
!||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8                  ../engine/source/materials/mat_share/mulaw8.F90
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
        subroutine sigeps50s (mat_param,                                        &
          nel    ,timestep,nuvar  ,nvartmp,uvar   ,vartmp ,            &
          rho    ,soundsp,off     ,amu    ,eplas  ,epsd   ,            &
          epspxx ,epspyy ,epspzz  ,epspxy ,epspyz ,epspzx ,            &
          depsxx ,depsyy ,depszz  ,depsxy ,depsyz ,depszx ,            &
          epsxx  ,epsyy  ,epszz   ,epsxy  ,epsyz  ,epszx  ,            &
          sigoxx ,sigoyy ,sigozz  ,sigoxy ,sigoyz ,sigozx ,            &
          signxx ,signyy ,signzz  ,signxy ,signyz ,signzx )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use matparam_def_mod
          use table_mat_vinterp_mod
          use constant_mod ,only : zero,third,half,one,two,three,three_half,em20
          use precision_mod, only : WP
!-------------------------------------------------------------------------------
!   I m p l i c i t   T y p e s
!-------------------------------------------------------------------------------
          implicit none
!
!-------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-------------------------------------------------------------------------------
          integer                 ,intent(in)    :: nvartmp
          integer                 ,intent(in)    :: nel
          integer                 ,intent(in)    :: nuvar
          real(kind=WP)                 ,intent(in)    :: timestep
          real(kind=WP) ,dimension(nel) ,intent(in)    :: rho
          real(kind=WP) ,dimension(nel) ,intent(in)    :: amu
          real(kind=WP) ,dimension(nel) ,intent(in)    :: epspxx,epspyy,epspzz,epspxy,epspyz,epspzx
          real(kind=WP) ,dimension(nel) ,intent(in)    :: depsxx,depsyy,depszz,depsxy,depsyz,depszx
          real(kind=WP) ,dimension(nel) ,intent(in)    :: epsxx,epsyy,epszz,epsxy,epsyz,epszx
          real(kind=WP) ,dimension(nel) ,intent(in)    :: sigoxx,sigoyy,sigozz,sigoxy,sigoyz,sigozx
          real(kind=WP) ,dimension(nel) ,intent(out)   :: signxx,signyy,signzz,signxy,signyz,signzx
          real(kind=WP) ,dimension(nel) ,intent(out)   :: soundsp
          real(kind=WP) ,dimension(nel) ,intent(inout) :: eplas
          real(kind=WP) ,dimension(nel) ,intent(inout) :: off
          real(kind=WP) ,dimension(nel) ,intent(out)   :: epsd
          type(matparam_struct_)  ,intent(in)    :: mat_param
          real(kind=WP) ,intent(inout) :: uvar(nel,nuvar)
          integer ,intent(inout) :: vartmp(nel,nvartmp) ! last interpolation positions in function tables
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer :: i,ii,nindx,nindxc
          integer :: icomp,irate,iflag1,iflag2
          real(kind=WP) :: emx11,emx22,emx33,emx12,emx23,emx31
          real(kind=WP) :: muold,dmudt,asrate
          real(kind=WP) :: ecomp,gcomp,bulk,sigy,hcomp,vcomp
          real(kind=WP) :: depsv,pres,svm,rfact,yld
          real(kind=WP) :: stxx,styy,stzz,stxy,styz,stzx
          real(kind=WP) ,dimension(nel)   :: e11,e22,e33,g12,g23,g31
          real(kind=WP) ,dimension(nel)   :: dep1,dep2,dep3,dep4,dep5,dep6,dydx
          real(kind=WP) ,dimension(nel)   :: ep1,ep2,ep3,ep4,ep5,ep6
          real(kind=WP) ,dimension(nel)   :: sigyxx,sigyyy,sigyzz,sigyxy,sigyyz,sigyzx
          real(kind=WP) ,dimension(nel)   :: rvol,beta
          real(kind=WP), dimension(nel,1) :: xvec1
          real(kind=WP), dimension(nel,2) :: xvec2
          integer, dimension(nel,2) :: ipos2
          integer, dimension(nel)   :: indxc       ! compacted elements index table
          integer, dimension(nel)   :: indx        ! not fully compacted elements
!=======================================================================
          iflag1 = mat_param%iparam(1)
          iflag2 = mat_param%iparam(2)
          icomp  = mat_param%iparam(3)
          irate  = mat_param%iparam(4)
!
          emx11  = mat_param%uparam(7)
          emx22  = mat_param%uparam(8)
          emx33  = mat_param%uparam(9)
          emx12  = mat_param%uparam(10)
          emx23  = mat_param%uparam(11)
          emx31  = mat_param%uparam(12)
          asrate = min(one,mat_param%uparam(13)*timestep)
!
          nindx  = 0
          nindxc = 0
          if (icomp == 1) then        ! compact state option is active
            ! tag fully compacted and non compacted elements
            ecomp = mat_param%uparam(14)
            gcomp = mat_param%uparam(15)
            bulk  = mat_param%uparam(16)
            sigy  = mat_param%uparam(17)
            hcomp = mat_param%uparam(18)
            vcomp = mat_param%uparam(19)
            do i = 1,nel
              rvol(i) = one / (one + amu(i))
              beta(i) = max(min((one-rvol(i)) / (one-vcomp),one),zero)
              if (rvol(i) <= vcomp .and. vartmp(i,13) == 0) vartmp(i,13) = 1 ! element pass to compacted state
              if (vartmp(i,13) == 1) then
                nindxc = nindxc + 1
                indxc(nindxc) = i
              else
                nindx = nindx + 1
                indx(nindx) = i
              end if
            end do
            do i = 1,nel
              e11(i) = beta(i) * ecomp + (one - beta(i)) * mat_param%uparam(1)
              e22(i) = beta(i) * ecomp + (one - beta(i)) * mat_param%uparam(2)
              e33(i) = beta(i) * ecomp + (one - beta(i)) * mat_param%uparam(3)
              g12(i) = beta(i) * gcomp + (one - beta(i)) * mat_param%uparam(4)
              g23(i) = beta(i) * gcomp + (one - beta(i)) * mat_param%uparam(5)
              g31(i) = beta(i) * gcomp + (one - beta(i)) * mat_param%uparam(6)
            end do
          else        ! compact state option is not active
            e11(1:nel) = mat_param%uparam(1)
            e22(1:nel) = mat_param%uparam(2)
            e33(1:nel) = mat_param%uparam(3)
            g12(1:nel) = mat_param%uparam(4)
            g23(1:nel) = mat_param%uparam(5)
            g31(1:nel) = mat_param%uparam(6)
          end if
!
          do i=1,nel
            signxx(i) = sigoxx(i) + e11(i) * depsxx(i)
            signyy(i) = sigoyy(i) + e22(i) * depsyy(i)
            signzz(i) = sigozz(i) + e33(i) * depszz(i)
            signxy(i) = sigoxy(i) + g12(i) * depsxy(i)
            signyz(i) = sigoyz(i) + g23(i) * depsyz(i)
            signzx(i) = sigozx(i) + g31(i) * depszx(i)
            soundsp(i) = sqrt(max(e11(i),e22(i),e33(i),g12(i),g23(i),g31(i))/rho(i))
          enddo
!
          do i=1,nel
            if (epsxx(i) > emx11 .or. epsyy(i) > emx22 .or. epszz(i) > emx33 .or.    &
              abs(epsxy(i)*half) > emx12 .or.                                      &
              abs(epsyz(i)*half) > emx23 .or.                                      &
              abs(epszx(i)*half) > emx31) off(i) = zero
          enddo
!---------------------------------
          ! strain definition
!---------------------------------
          if (iflag1 == 1)then
            ep1(1:nel) = epsxx(1:nel)
            ep2(1:nel) = epsyy(1:nel)
            ep3(1:nel) = epszz(1:nel)
          elseif (iflag1 == -1)then
            ep1(1:nel) = -epsxx(1:nel)
            ep2(1:nel) = -epsyy(1:nel)
            ep3(1:nel) = -epszz(1:nel)
          else
            ep1(1:nel) = amu(1:nel)
            ep2(1:nel) = amu(1:nel)
            ep3(1:nel) = amu(1:nel)
          endif
          if (iflag2 == 1)then
            ep4(1:nel) = epsxy(1:nel)
            ep5(1:nel) = epsyz(1:nel)
            ep6(1:nel) = epszx(1:nel)
          elseif (iflag2 == -1)then
            ep4(1:nel) = -epsxy(1:nel)
            ep5(1:nel) = -epsyz(1:nel)
            ep6(1:nel) = -epszx(1:nel)
          else
            ep4(1:nel) = amu(1:nel)
            ep5(1:nel) = amu(1:nel)
            ep6(1:nel) = amu(1:nel)
          endif
!---------------------------------
          ! strain rate definition
!---------------------------------
!      if (iflag1 == 0 .and. iflag2 == 0) then   ! common volumetric strain rate
!                                                ! not used by d2rad, not tested
!        do i=1,nel
!          muold = uvar(i,1)
!          dmudt = abs((amu(i)-muold)) / max(timestep,em20)
!          epsd  = asrate*dmudt + (one-asrate)*uvar(i,2)
!          dep1(i)   = epsd
!          dep2(i)   = epsd
!          dep3(i)   = epsd
!          dep4(i)   = epsd
!          dep5(i)   = epsd
!          dep6(i)   = epsd
!          uvar(i,1) = amu(i)
!          uvar(i,2) = epsd
!        enddo
!---------------------------------
!
          if (irate == 2) then                  ! independent strain rate in each directions
            do i=1,nel
              uvar(i,1) = asrate*epspxx(i) + (one -asrate)*uvar(i,1)
              uvar(i,2) = asrate*epspyy(i) + (one -asrate)*uvar(i,2)
              uvar(i,3) = asrate*epspzz(i) + (one -asrate)*uvar(i,3)
              uvar(i,4) = asrate*epspxy(i) + (one -asrate)*uvar(i,4)
              uvar(i,5) = asrate*epspyz(i) + (one -asrate)*uvar(i,5)
              uvar(i,6) = asrate*epspzx(i) + (one -asrate)*uvar(i,6)
              dep1(i)   = abs(uvar(i,1))
              dep2(i)   = abs(uvar(i,2))
              dep3(i)   = abs(uvar(i,3))
              dep4(i)   = abs(uvar(i,4))
              dep5(i)   = abs(uvar(i,5))
              dep6(i)   = abs(uvar(i,6))
              epsd(i) = (dep1(i)**2 + dep2(i)**2 + dep3(i)**2)          &
                + (dep4(i)**2 + dep5(i)**2 + dep6(i)**2)*half          ! just for ouptput
            enddo
          else                     ! irate = 1 : equivalent strain rate common for all directions
            do i=1,nel
              epsd(i) = (epspxx(i)**2 + epspyy(i)**2 + epspzz(i)**2)          &
                + (epspxy(i)**2 + epspyz(i)**2 + epspzx(i)**2)*half
              epsd = asrate*sqrt(epsd) + (one -asrate)*uvar(i,1)
              dep1(i)   = epsd(i)
              dep2(i)   = epsd(i)
              dep3(i)   = epsd(i)
              dep4(i)   = epsd(i)
              dep5(i)   = epsd(i)
              dep6(i)   = epsd(i)
              uvar(i,1) = epsd(i)
            enddo
          end if
!-------------------------------------------------------------------------------
!     table interpolations in each direction
!-------------------------------------------------------------------------------
          !  direction xx : table(1)
          if (mat_param%table(1)%notable > 0) then
            if (mat_param%table(1)%ndim == 1) then
              xvec1(1:nel,1)   = ep1(1:nel)
              call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp(1,1),xvec1,sigyxx,dydx)
            else   ! ndim = 2
              xvec2(1:nel,1)  = ep1(1:nel)
              xvec2(1:nel,2)  = dep1(1:nel)
              ipos2(1:nel,1)  = vartmp(1:nel,1)
              ipos2(1:nel,2)  = vartmp(1:nel,2)
              call table_mat_vinterp(mat_param%table(1),nel,nel,ipos2,xvec2,sigyxx,dydx)
              vartmp(1:nel,1) = ipos2(1:nel,1)
              vartmp(1:nel,2) = ipos2(1:nel,2)
            end if
          endif
          !  direction yy : table(2)
          if (mat_param%table(2)%notable > 0) then
            if (mat_param%table(2)%ndim == 1) then
              xvec1(1:nel,1)   = ep2(1:nel)
              call table_mat_vinterp(mat_param%table(2),nel,nel,vartmp(1,3),xvec1,sigyyy,dydx)
            else   ! ndim = 2
              xvec2(1:nel,1)  = ep2(1:nel)
              xvec2(1:nel,2)  = dep2(1:nel)
              ipos2(1:nel,1)  = vartmp(1:nel,3)
              ipos2(1:nel,2)  = vartmp(1:nel,4)
              call table_mat_vinterp(mat_param%table(2),nel,nel,ipos2,xvec2,sigyyy,dydx)
              vartmp(1:nel,3) = ipos2(1:nel,1)
              vartmp(1:nel,4) = ipos2(1:nel,2)
            end if
          endif
          !  direction zz : table(3)
          if (mat_param%table(3)%notable > 0) then
            if (mat_param%table(3)%ndim == 1) then
              xvec1(1:nel,1)   = ep3(1:nel)
              call table_mat_vinterp(mat_param%table(3),nel,nel,vartmp(1,5),xvec1,sigyzz,dydx)
            else   ! ndim = 2
              xvec2(1:nel,1)  = ep3(1:nel)
              xvec2(1:nel,2)  = dep3(1:nel)
              ipos2(1:nel,1)  = vartmp(1:nel,5)
              ipos2(1:nel,2)  = vartmp(1:nel,6)
              call table_mat_vinterp(mat_param%table(3),nel,nel,ipos2,xvec2,sigyzz,dydx)
              vartmp(1:nel,5) = ipos2(1:nel,1)
              vartmp(1:nel,6) = ipos2(1:nel,2)
            end if
          endif
          !  direction xy : table(4)
          if (mat_param%table(4)%notable > 0) then
            if (mat_param%table(3)%ndim == 1) then
              xvec1(1:nel,1)   = ep4(1:nel)
              call table_mat_vinterp(mat_param%table(4),nel,nel,vartmp(1,7),xvec1,sigyxy,dydx)
            else   ! ndim = 2
              xvec2(1:nel,1)  = ep4(1:nel)
              xvec2(1:nel,2)  = dep4(1:nel)
              ipos2(1:nel,1)  = vartmp(1:nel,7)
              ipos2(1:nel,2)  = vartmp(1:nel,8)
              call table_mat_vinterp(mat_param%table(4),nel,nel,ipos2,xvec2,sigyxy,dydx)
              vartmp(1:nel,7) = ipos2(1:nel,1)
              vartmp(1:nel,8) = ipos2(1:nel,2)
            end if
          endif
          !  direction yz : table(5)
          if (mat_param%table(5)%notable > 0) then
            if (mat_param%table(5)%ndim == 1) then
              xvec1(1:nel,1)   = ep5(1:nel)
              call table_mat_vinterp(mat_param%table(5),nel,nel,vartmp(1,9),xvec1,sigyyz,dydx)
            else   ! ndim = 2
              xvec2(1:nel,1)  = ep5(1:nel)
              xvec2(1:nel,2)  = dep5(1:nel)
              ipos2(1:nel,1)  = vartmp(1:nel,9)
              ipos2(1:nel,2)  = vartmp(1:nel,10)
              call table_mat_vinterp(mat_param%table(5),nel,nel,ipos2,xvec2,sigyyz,dydx)
              vartmp(1:nel,9)  = ipos2(1:nel,1)
              vartmp(1:nel,10) = ipos2(1:nel,2)
            end if
          endif
          !  direction zz : table(6)
          if (mat_param%table(6)%notable > 0) then
            if (mat_param%table(6)%ndim == 1) then
              xvec1(1:nel,1)   = ep6(1:nel)
              call table_mat_vinterp(mat_param%table(6),nel,nel,vartmp(1,11),xvec1,sigyzx,dydx)
            else   ! ndim = 2
              xvec2(1:nel,1)  = ep6(1:nel)
              xvec2(1:nel,2)  = dep6(1:nel)
              ipos2(1:nel,1)  = vartmp(1:nel,11)
              ipos2(1:nel,2)  = vartmp(1:nel,12)
              call table_mat_vinterp(mat_param%table(6),nel,nel,ipos2,xvec2,sigyzx,dydx)
              vartmp(1:nel,11) = ipos2(1:nel,1)
              vartmp(1:nel,12) = ipos2(1:nel,2)
            end if
          end if
!-------------------------------------------------------------------------------
          if (icomp == 0) then
            signxx(1:nel) = sign(min(abs(signxx(1:nel)),sigyxx(1:nel)),signxx(1:nel))
            signyy(1:nel) = sign(min(abs(signyy(1:nel)),sigyyy(1:nel)),signyy(1:nel))
            signzz(1:nel) = sign(min(abs(signzz(1:nel)),sigyzz(1:nel)),signzz(1:nel))
            signxy(1:nel) = sign(min(abs(signxy(1:nel)),sigyxy(1:nel)),signxy(1:nel))
            signyz(1:nel) = sign(min(abs(signyz(1:nel)),sigyyz(1:nel)),signyz(1:nel))
            signzx(1:nel) = sign(min(abs(signzx(1:nel)),sigyzx(1:nel)),signzx(1:nel))
          else
            do ii = 1,nindx   ! loop only over not fully compacted elements
              i = indx(ii)
              signxx(i) = sign(min(abs(signxx(i)),sigyxx(i)),signxx(i))
              signyy(i) = sign(min(abs(signyy(i)),sigyyy(i)),signyy(i))
              signzz(i) = sign(min(abs(signzz(i)),sigyzz(i)),signzz(i))
              signxy(i) = sign(min(abs(signxy(i)),sigyxy(i)),signxy(i))
              signyz(i) = sign(min(abs(signyz(i)),sigyyz(i)),signyz(i))
              signzx(i) = sign(min(abs(signzx(i)),sigyzx(i)),signzx(i))
            end do
          end if
!-------------------------------------------------------------------------------
          ! plasticity treatment for fully compacted elements
          ! plasticity starts only after element compaction and all stress become coupled
!-------------------------------------------------------------------------------
          if (icomp == 1) then
            do ii = 1,nindxc
              i = indxc(ii)
              ! gcomp = 2G in compacted state
              depsv= (depsxx(i) + depsyy(i) + depszz(i)) * third
              pres = (sigoxx(i) + sigoyy(i) + sigozz(i)) * third
              stxx = sigoxx(i) + gcomp * (depsxx(i) - depsv) - pres
              styy = sigoyy(i) + gcomp * (depsyy(i) - depsv) - pres
              stzz = sigozz(i) + gcomp * (depszz(i) - depsv) - pres
              stxy = sigoxy(i) + gcomp * depsxy(i) * half
              styz = sigoyz(i) + gcomp * depsyz(i) * half
              stzx = sigozx(i) + gcomp * depszx(i) * half
              svm  = (stxx**2 + styy**2 + stzz**2) * half + stxy**2 + styz**2 + stzx**2
              svm  = sqrt(three * svm)
              yld  = sigy + hcomp * eplas(i)
              rfact = min(one,yld / svm)
              pres = pres + three*bulk*depsv
              signxx(i) = stxx * rfact + pres
              signyy(i) = styy * rfact + pres
              signzz(i) = stzz * rfact + pres
              signxy(i) = stxy * rfact
              signyz(i) = styz * rfact
              signzx(i) = stzx * rfact
              eplas(i)  = eplas(i) + (one-rfact) * svm / (gcomp*three_half + hcomp)
            end do
          end if
!-----------
          return
        end subroutine sigeps50s
!-----------
      end module sigeps50s_mod
