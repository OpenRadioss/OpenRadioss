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
!||    mat25_tsaiwu_c_mod   ../engine/source/materials/mat/mat025/mat25_tsaiwu_c.F90
!||--- called by ------------------------------------------------------
!||    sigeps25c            ../engine/source/materials/mat/mat025/sigeps25c.F
!||====================================================================
      module mat25_tsaiwu_c_mod
      contains

! ==================================================================================
! \brief nonlinear orthotropic material law25 with Tsai-Wu formulation
! \details calculates stress-strain relationship using Tsai-Wu plasticity criterion

! ==================================================================================
!||====================================================================
!||    mat25_tsaiwu_c     ../engine/source/materials/mat/mat025/mat25_tsaiwu_c.F90
!||--- called by ------------------------------------------------------
!||    sigeps25c          ../engine/source/materials/mat/mat025/sigeps25c.F
!||--- calls      -----------------------------------------------------
!||    rotov              ../engine/source/airbag/roto.F
!||    urotov             ../engine/source/airbag/uroto.F
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    message_mod        ../engine/share/message_module/message_mod.F
!||    precision_mod      ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine mat25_tsaiwu_c(mat_param   ,                                    &
          nel     ,off     ,sig     ,                         &
          wpla    ,dir     ,crak    ,                         &
          nfis1   ,nfis2   ,nfis3   ,ilayer  ,shf   ,         &
          ngl     ,eps     ,igtyp   ,wplar   ,strn1 ,         &
          strn2   ,strn3   ,strp1   ,strp2   ,sige  ,         &
          epsd_pg ,epsd    ,israte  ,asrate  ,offply  ,       &
          sigy    ,etse    ,ishplyxfem,ly_exx,ly_eyy  ,       &
          ly_exy  ,sigply  ,sigpe   ,ply_id  ,                &
          signxx  ,signyy  ,signxy  ,signyz  ,signzx,         &
          ipg     ,tsaiwu  ,iplyxfem,time    ,timestep,       &
          imconv  ,mvsiz   ,iout    ,dmg     ,l_dmg   )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use matparam_def_mod
          use message_mod
          use constant_mod ,only : zero,half,one,two,four,four_over_5
          use constant_mod ,only : em10,em15,em20,ep20
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------
          implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer ,intent(in) :: nel                       !< element group size
          integer ,intent(in) :: mvsiz                     !< max element group size
          integer ,intent(in) :: ilayer                    !< layer number
          integer ,intent(in) :: ply_id                    !< ply Id
          integer ,intent(in) :: ipg                       !< Gauss point number
          integer ,intent(in) :: imconv                    !< output flag
          integer ,intent(in) :: iout                      !< output file id
          integer ,intent(in) :: israte                    !< strain rate flag
          integer ,intent(in) :: igtyp                     !< property type
          integer ,intent(in) :: ishplyxfem                !< ply Xfem flag
          integer ,intent(in) :: iplyxfem                  !< ply Xfem flag
          integer ,intent(in) :: ngl(mvsiz)                !< element ID table
          real(kind=WP) ,intent(in) :: time                !< current time
          real(kind=WP) ,intent(in) :: timestep            !< current time step
          real(kind=WP) ,intent(in) :: asrate              !< strain rate filtering coefficient
          integer ,intent(in) :: l_dmg                     !< second dimension of damage table
          integer ,dimension(nel) ,intent(inout) :: nfis1  !< failure counter in 1st direction
          integer ,dimension(nel) ,intent(inout) :: nfis2  !< failure counter in 2nd direction
          integer ,dimension(nel) ,intent(inout) :: nfis3  !< failure counter in 3rd direction
          real(kind=WP) :: off(nel)                              !< element activation coefficient
          real(kind=WP) :: sig(nel,5)                            !< output stress tensor
          real(kind=WP) :: wpla(nel)                             !< plastic work
          real(kind=WP) :: dir(nel,2)                            !< orthotropy directions
          real(kind=WP) :: crak(nel,2)                           !< ply Xfem failure criterion
          real(kind=WP) :: shf(nel)                              !< transverse shear factor
          real(kind=WP) :: epsd_pg(nel)                          !< global strain rate in Gauss point
          real(kind=WP) :: epsd(nel)                             !< local strain rate (lbuf%epsd)
          real(kind=WP) :: eps(mvsiz,5)                          !< total strain tensor
          real(kind=WP) :: wplar(mvsiz)                          !< reference plastic work
          real(kind=WP) :: strp1(mvsiz)                          !< strain in 1st orthotropic direction
          real(kind=WP) :: strp2(mvsiz)                          !< strain in 2nd orthotropic direction
          real(kind=WP) :: strn1(mvsiz)                          !< strain in 1st direction
          real(kind=WP) :: strn2(mvsiz)                          !< strain in 2nd direction
          real(kind=WP) :: strn3(mvsiz)                          !< strain in 3rd direction
          real(kind=WP) :: sige(mvsiz,5)                         !< stress in local element coord system
          real(kind=WP) :: offply(nel)                           !< ply activation coefficient
          real(kind=WP) :: sigy(nel)                             !< yield stress
          real(kind=WP) :: etse(nel)                             !< tangent stiffness
          real(kind=WP) :: ly_exx(nel)                           !< layer sliding deformation xx
          real(kind=WP) :: ly_eyy(nel)                           !< layer sliding deformation yy
          real(kind=WP) :: ly_exy(nel)                           !< layer sliding deformation xy
          real(kind=WP) :: sigply(nel,3)                         !< layer stress
          real(kind=WP) :: sigpe(mvsiz,5)                        !< layer stress
          real(kind=WP) :: signxx(mvsiz)                         !< stress component
          real(kind=WP) :: signyy(mvsiz)                         !< stress component
          real(kind=WP) :: signxy(mvsiz)                         !< stress component
          real(kind=WP) :: signyz(mvsiz)                         !< stress component
          real(kind=WP) :: signzx(mvsiz)                         !< stress component
          real(kind=WP) :: tsaiwu(mvsiz)                         !< Tsai-Wu criterion
          real(kind=WP), intent(inout) :: dmg(nel,l_dmg)         !< damage related variables
          type (matparam_struct_) ,intent(in) :: mat_param !< material parameter structure
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer :: i,j,fail,ioff,icc,nindx,ifail0
          integer ,dimension(nel) :: index,icas,isoft
          integer ,dimension(nel) :: fail_old
          real(kind=WP) :: scale,cnn,scale1,scale2,dam1,dam2,dwpla,ht
          real(kind=WP) ,dimension(3)   :: soft
          real(kind=WP) ,dimension(nel) :: dp1,dp2,dp3,cb,cn,e11,e22,nu12,nu21
          real(kind=WP) ,dimension(nel) :: de1,de2,ds1,ds2,ds3,g12,g23,g31,fmax
          real(kind=WP) ,dimension(nel) :: wvec,t1,t2,t3,lamda,coef,a11,a12,a22
          real(kind=WP) ,dimension(nel) :: epst1,epst2,epsm1,epsm2,epsf1,epsf2
          real(kind=WP) ,dimension(nel) :: dmax,cc,epdr,fyld,wplaref,wplamx
          real(kind=WP) ,dimension(nel) :: f1,f2,f12,f11,f22,f33
          real(kind=WP) ,dimension(nel) :: so1,so2,so3,s1,s2,s3,s4,s5
          real(kind=WP) ,dimension(nel) :: epspfac
          real(kind=WP) ,dimension(mvsiz,5) :: epsply
          real(kind=WP) ,dimension(mvsiz,3) :: eply
!=======================================================================
          if (time == zero) then
            offply(1:nel) = one
          endif

          ioff   = mat_param%iparam(2)
          icc    = mat_param%iparam(3)
!-----------------------------------------------
!     constant input parameters
!-----------------------------------------------
          do i=1,nel
            e11(i)    = mat_param%uparam(1)      !  pm(33)
            e22(i)    = mat_param%uparam(2)      !  pm(34)
            nu12(i)   = mat_param%uparam(4)      !  pm(35)
            nu21(i)   = mat_param%uparam(5)      !  pm(36)
            g12(i)    = mat_param%uparam(6)      !  pm(37)
            g23(i)    = mat_param%uparam(7)      !  pm(38)
            g31(i)    = mat_param%uparam(8)      !  pm(39)
            wplaref(i)= mat_param%uparam(9)      !  pm(68)
            cc(i)     = mat_param%uparam(10)     !  pm(50)
            epdr(i)   = mat_param%uparam(11)     !  pm(51)

            epst1(i)  = mat_param%uparam(12)     ! pm(60)
            epst2(i)  = mat_param%uparam(13)     ! pm(61)
            epsm1(i)  = mat_param%uparam(14)     ! pm(62)
            epsm2(i)  = mat_param%uparam(15)     ! pm(63)
            epsf1(i)  = mat_param%uparam(16)     ! pm(98)
            epsf2(i)  = mat_param%uparam(17)     ! pm(99)
            dmax (i)  = mat_param%uparam(18)     ! pm(64)
          enddo

          wplamx(1:nel) = mat_param%uparam(20)   !  pm(41)
          f1(1:nel)     = mat_param%uparam(21)   !  pm(54)
          f2(1:nel)     = mat_param%uparam(22)   !  pm(55)
          f11(1:nel)    = mat_param%uparam(23)   !  pm(56)
          f22(1:nel)    = mat_param%uparam(24)   !  pm(57)
          f33(1:nel)    = mat_param%uparam(25)   !  pm(58)
          f12(1:nel)    = mat_param%uparam(26)   !  pm(59)

          cb(1:nel)     = mat_param%uparam(27)   !  pm(46)
          cn(1:nel)     = mat_param%uparam(28)   !  pm(47)
          fmax(1:nel)   = mat_param%uparam(29)   !  pm(49)
!-----------------------------------------------
!     element variable initialization
!-----------------------------------------------
          etse(1:nel)  = one
          isoft(1:nel) = 0
          sigy(1:nel)  = one/sqrt(max(em20,f33(1:nel)))
!-------------------------------------------------------------------
!     old failure
!-----------------------------
          do i=1,nel
            fail_old(i)=0
            if (dmg(i,2)>=dmax(i)) fail_old(i) = fail_old(i) + 1
            if (dmg(i,3)>=dmax(i)) fail_old(i) = fail_old(i) + 2
            if (wpla(i)< zero) then
!         wpla is negative in case of layer already reached failure-p :
              fail_old(i) = fail_old(i) + 4
              wpla(i)     = -wpla(i)
            end if
            if (crak(i,1)>=epsf1(i)-epst1(i)) then
              fail_old(i) = fail_old(i) + 8
            end if
            if (crak(i,2)>=epsf2(i)-epst2(i)) then
              fail_old(i) = fail_old(i) + 16
            end if
!       if (wpla(i)>=wplamx(i))fail_old(i) = fail_old(i) + 4
          enddo
!
          nindx=0
          do i=1,nel
            if (fail_old(i)<4 .or. (fail_old(i)<8.and.ioff<0)) then
              nindx=nindx+1
              index(nindx)=i
            end if
          enddo
!-------------------------------------------------------------------
!     stress reduction using wpla_old >= wplamx
!-------------------------------------------------------------------
          do i=1,nel
            if ((fail_old(i)>=4.and.ioff>=0).or. fail_old(i)>=8) then
              sig(i,1)=four_over_5*sig(i,1)
              sig(i,2)=four_over_5*sig(i,2)
              sig(i,3)=four_over_5*sig(i,3)
              sig(i,4)=four_over_5*sig(i,4)
              sig(i,5)=four_over_5*sig(i,5)
!
              if (abs(sig(i,1))<em20) sig(i,1)=zero
              if (abs(sig(i,2))<em20) sig(i,2)=zero
              if (abs(sig(i,3))<em20) sig(i,3)=zero
              if (abs(sig(i,4))<em20) sig(i,4)=zero
              if (abs(sig(i,5))<em20) sig(i,5)=zero
              eps(i,1)=zero
              eps(i,2)=zero
              eps(i,3)=zero
              eps(i,4)=zero
              eps(i,5)=zero
            endif
          enddo
!
!-------------------------------------------------------------------
          if (ishplyxfem /= 0 .and. iplyxfem==2) then
            do i=1,nel
              epsply(i,1)=ly_exx(i)
              epsply(i,2)=ly_eyy(i)
              epsply(i,3)=half*ly_exy(i)
              epsply(i,4)=zero
              epsply(i,5)=zero
            enddo
            call rotov(1,nel,epsply,dir,nel)
            do i=1,nel
              epsply(i,3)=two*epsply(i,3)
!         epsply(i,4)=two*epsply(i,4)
!         epsply(i,5)=two*epsply(i,5)
            enddo
            do i=1,nel
              if ((fail_old(i)>=4.and.ioff>=0).or. fail_old(i)>=8) then
                sigply(i,1)=four_over_5*sigply(i,1)
                sigply(i,2)=four_over_5*sigply(i,2)
                sigply(i,3)=four_over_5*sigply(i,3)
                if (abs(sigply(i,1))<em20) sigply(i,1)=zero
                if (abs(sigply(i,2))<em20) sigply(i,2)=zero
                if (abs(sigply(i,3))<em20) sigply(i,3)=zero
                epsply(i,1)=zero
                epsply(i,2)=zero
                epsply(i,3)=zero
              end if
            enddo
          end if
!-------------------------------------------------------------------
!     rotation into orthotropic directions
!-----------------------------
          do i=1,nel
            eps(i,3)=half*eps(i,3)
            eps(i,4)=half*eps(i,4)
            eps(i,5)=half*eps(i,5)
          enddo
          call rotov(1,nel,eps,dir,nel)
          do i=1,nel
            eps(i,3)=two*eps(i,3)
            eps(i,4)=two*eps(i,4)
            eps(i,5)=two*eps(i,5)
          enddo
!     strains in orthotropic directions
          do i=1,nel
            strp1(i) = dir(i,1)*dir(i,1)*strn1(i)         &
              + dir(i,2)*dir(i,2)*strn2(i)         &
              + two*dir(i,1)*dir(i,2)*strn3(i)
            strp2(i) = dir(i,2)*dir(i,2)*strn1(i)         &
              + dir(i,1)*dir(i,1)*strn2(i)         &
              - two*dir(i,2)*dir(i,1)*strn3(i)
          enddo
!-------------------------------------------------------------------
!     elastic deformations
!-----------------------------
          if (ishplyxfem /= 0 .and. iplyxfem==2) then
            do  i=1,nel
              de1(i) = one-max( zero , sign(dmg(i,2),sig(i,1)) )
              de2(i) = one-max( zero , sign(dmg(i,3),sig(i,2)) )
              scale  = (half +sign(half,de1(i)-one))*(half+sign(half,de2(i)-one))
              s1(i)  = sig(i,1)/de1(i)-nu12(i)*sig(i,2)*scale
              s2(i)  = sig(i,2)/de2(i)-nu21(i)*sig(i,1)*scale
              s1(i)  = s1(i)/e11(i)
              s2(i)  = s2(i)/e22(i)
              s3(i)  = sig(i,3)/de1(i)/de2(i)/g12(i)
              s4(i)  = sig(i,4)/max(de2(i)*g23(i)*shf(i),em20)
              s5(i)  = sig(i,5)/max(de1(i)*g31(i)*shf(i),em20)
              ! relatif displacement
              eply(i,1) = sigply(i,1)/de1(i)-nu12(i)*sigply(i,2)*scale
              eply(i,2) = sigply(i,2)/de2(i)-nu21(i)*sigply(i,1)*scale
              eply(i,1) = eply(i,1)/e11(i)
              eply(i,2) = eply(i,2)/e22(i)
              eply(i,3) = sigply(i,3)/de1(i)/de2(i)/g12(i)
            enddo
!
            do  i=1,nel
              s1(i)=s1(i)+eps(i,1)
              s2(i)=s2(i)+eps(i,2)
              s3(i)=s3(i)+eps(i,3)
              s4(i)=s4(i)+eps(i,4)
              s5(i)=s5(i)+eps(i,5)
              ! relatif displacement
              eply(i,1)=eply(i,1)+epsply(i,1)
              eply(i,2)=eply(i,2)+epsply(i,2)
              eply(i,3)=eply(i,3)+epsply(i,3)
            enddo
          else
            do i=1,nel
              de1(i) = one-max( zero , sign(dmg(i,2),sig(i,1)) )
              de2(i) = one-max( zero , sign(dmg(i,3),sig(i,2)) )
              scale  = (half +sign(half,de1(i)-one))*(half+sign(half,de2(i)-one))
              s1(i)  = sig(i,1)/de1(i)-nu12(i)*sig(i,2)*scale
              s2(i)  = sig(i,2)/de2(i)-nu21(i)*sig(i,1)*scale
              s1(i)  = s1(i)/e11(i)
              s2(i)  = s2(i)/e22(i)
              s3(i)  = sig(i,3)/de1(i)/de2(i)/g12(i)
              s4(i)  = sig(i,4)/max(de2(i)*g23(i)*shf(i),em20)
              s5(i)  = sig(i,5)/max(de1(i)*g31(i)*shf(i),em20)
            enddo
!
            do i=1,nel
              s1(i)=s1(i)+eps(i,1)
              s2(i)=s2(i)+eps(i,2)
              s3(i)=s3(i)+eps(i,3)
              s4(i)=s4(i)+eps(i,4)
              s5(i)=s5(i)+eps(i,5)
            enddo
!
          endif ! iplxfem
!----------------------------------------------------------------------
          if (ishplyxfem /= 0 .and. iplyxfem==2) then
#include "vectorize.inc"
            do j=1,nindx
              i=index(j)
              if (dmg(i,2)/=zero) then
                crak(i,1) = crak(i,1) + eps(i,1)+ epsply(i,1)
                dam1 = crak(i,1)/(epsm1(i)-epst1(i))
                dam2 = dam1*epsm1(i)/(crak(i,1)+epst1(i))
                dmg(i,2) = max(dmg(i,2),dam2)
                dmg(i,2) = min(dmg(i,2),dmax(i))
              endif
            enddo
!
#include "vectorize.inc"
            do j=1,nindx
              i=index(j)
!
              if (dmg(i,3)/=zero) then
                crak(i,2) = crak(i,2)+eps(i,2)+ epsply(i,2)
                dam1 = crak(i,2)/(epsm2(i)-epst2(i))
                dam2 = dam1*epsm2(i)/(crak(i,2)+epst2(i))
                dmg(i,3) = max(dmg(i,3),dam2)
                dmg(i,3) = min(dmg(i,3),dmax(i))
              endif
            enddo
          else
#include "vectorize.inc"
            do j=1,nindx
              i=index(j)
              if (dmg(i,2)/=zero) then
                crak(i,1) = crak(i,1) + eps(i,1)
                dam1 = crak(i,1)/(epsm1(i)-epst1(i))
                dam2 = dam1*epsm1(i)/(crak(i,1)+epst1(i))
                dmg(i,2) = max(dmg(i,2),dam2)
                dmg(i,2) = min(dmg(i,2),dmax(i))
              endif
            enddo
!
            do j=1,nindx
              i=index(j)
!
              if (dmg(i,3)/=zero) then
                crak(i,2) = crak(i,2)+eps(i,2)
                dam1 = crak(i,2)/(epsm2(i)-epst2(i))
                dam2 = dam1*epsm2(i)/(crak(i,2)+epst2(i))
                dmg(i,3) = max(dmg(i,3),dam2)
                dmg(i,3) = min(dmg(i,3),dmax(i))
              endif
            enddo
          endif
!
          do i=1,nel
            de1(i) = one- max( zero , sign(dmg(i,2),sig(i,1)) )
            de2(i) = one- max( zero , sign(dmg(i,3),sig(i,2)) )
            scale1 = (half + sign(half,de1(i)-one))*(half+sign(half,de2(i)-one))
            scale2 = one-nu12(i)*nu21(i)*scale1
            a11(i) = e11(i)*de1(i)/scale2
            a22(i) = e22(i)*de2(i)/scale2
            a12(i) = nu21(i)*a11(i)*scale1
          enddo
!-----------------------------
!     elastic stress
!-----------------------------
          do i=1,nel
            t1(i)    = a11(i)*s1(i)+a12(i)*s2(i)
            t2(i)    = a12(i)*s1(i)+a22(i)*s2(i)
            t3(i)    = de1(i)*de2(i)*g12(i)*s3(i)
            sig(i,4) = de2(i)*g23(i)*shf(i)*s4(i)
            sig(i,5) = de1(i)*g31(i)*shf(i)*s5(i)
          enddo
!
          if (ishplyxfem /= 0 .and. iplyxfem==2) then
            do i=1,nel
              sigply(i,1) = a11(i)*eply(i,1)+a12(i)*eply(i,2)
              sigply(i,2) = a12(i)*eply(i,1)+a22(i)*eply(i,2)
              sigply(i,3) = de1(i)*de2(i)*g12(i)*eply(i,3)
            end do
          end if
!
          do i=1,nel
            if (t1(i) > 0) then
              if (t2(i) > 0) icas(i) = 1
              if (t2(i) < 0) icas(i) = 4
            else
              if (t2(i) > 0) icas(i) = 2
              if (t2(i) < 0) icas(i) = 3
            endif
          enddo
!-------------------------------------------------------------------
!     strain rate
!-------------------------------------------------------------------
          do i=1,nel
            if (israte==0) then
              epsd(i) = max(                          &
              abs(eps(i,1)),abs(eps(i,2)),abs(eps(i,3)),           &
              abs(eps(i,4)),abs(eps(i,5))) / max(timestep,em20)
            else
              epsd(i) = asrate*epsd_pg(i) + (one-asrate)*epsd(i) 
            end if
            if (epsd(i) > epdr(i)) then
              epspfac(i) = log(epsd(i)/epdr(i))
            else
              epspfac(i)=zero
            endif
            coef(i)=zero
          enddo
!-------------------------------------------------------------------
          do i=1,nel
            epspfac(i)=one + cc(i) * epspfac(i)
            if (wpla(i)/=zero) then
              fyld(i)=(one+cb(i)*exp(cn(i)*log(wpla(i))))*epspfac(i)
            else
              fyld(i)=epspfac(i)
            endif
            if (icc==1.or.icc==3) then
              fmax(i) = fmax(i)*epspfac(i)
            endif
            if (icc==3.or.icc==4) then
              wplamx(i) = wplamx(i)*epspfac(i)
            endif
            fyld(i) = min(fmax(i),fyld(i))
          enddo
!-------------------------------------------------------------------
!     tsai wu plasticity criteria
!-------------------------------------------------------------------
          do i=1,nel
            wvec(i) = f1(i) *t1(i)       + f2(i) *t2(i)             &
              + f11(i)*t1(i)*t1(i) + f22(i)*t2(i)*t2(i)       &
              + f33(i)*t3(i)*t3(i) + two*f12(i)*t1(i)*t2(i)
            tsaiwu(i) = max(min(wvec(i)/fyld(i),one),tsaiwu(i))
          enddo
!
          do i=1,nel
            if (wvec(i)>fyld(i).and.off(i)==one) coef(i)=one
            cnn=cn(i)-one
            wvec(i)=zero
            if (wpla(i)>zero.and.fyld(i)<fmax(i))  wvec(i)=epspfac(i)*exp(cnn*log(wpla(i)))
          enddo
!
          do i=1,nel
            so1(i) = sig(i,1)
            so2(i) = sig(i,2)
            so3(i) = sig(i,3)
          enddo
!
          do i=1,nel
            dp1(i)=f1(i)+two*f11(i)*so1(i)+two*f12(i)*so2(i)
            dp2(i)=f2(i)+two*f22(i)*so2(i)+two*f12(i)*so1(i)
            dp3(i)=two*f33(i)*so3(i)
          enddo
!-----------------------------------------------------------------------
          do i=1,nel
            ds1(i)=t1(i)-so1(i)
            ds2(i)=t2(i)-so2(i)
            ds3(i)=t3(i)-so3(i)
          enddo
!
          do i=1,nel
            lamda(i)=(dp1(i)*ds1(i)+dp2(i)*ds2(i)+dp3(i)*ds3(i))*coef(i)
            if (lamda(i) /= zero) then
              lamda(i) = lamda(i)*coef(i)/                              &
                (dp1(i)*(a11(i)*dp1(i)+a12(i)*dp2(i))+                &
                dp2(i)*(a12(i)*dp1(i)+a22(i)*dp2(i))+                &
                two*dp3(i)*g12(i)*de1(i)*de2(i)*dp3(i)+               &
                (so1(i)*dp1(i)+so2(i)*dp2(i)+two*so3(i)*dp3(i))       &
                *cn(i)*cb(i)*wvec(i) )
            end if
          enddo
!
          do i=1,nel
            dp1(i)=lamda(i)*dp1(i)
            dp2(i)=lamda(i)*dp2(i)
            dp3(i)=lamda(i)*dp3(i)
          enddo
!
          do i=1,nel
            t1(i)=t1(i)-a11(i)*dp1(i)-a12(i)*dp2(i)
            t2(i)=t2(i)-a12(i)*dp1(i)-a22(i)*dp2(i)
            t3(i)=t3(i)-g12(i)*de1(i)*de2(i)*dp3(i)*two
          enddo
!
          do i=1,nel
            dwpla = half* (dp1(i)*(t1(i)+so1(i))+ dp2(i)*(t2(i)+so2(i))    &
              + two*dp3(i)*(t3(i)+so3(i)))
            dwpla   = max(dwpla ,zero) / wplaref(i)
            wpla(i) = wpla(i) + dwpla
            icas(i) = 0   ! flag for output
          enddo
!
          do i=1,nel
            ifail0  =  mod(fail_old(i),8)
            if (wplamx(i) < ep20) dmg(i,4) = min(wpla(i)/wplamx(i),one)
            if (wpla(i)>=wplamx(i) .or.ifail0 >= 4 )wplar(i)= wplar(i)+one
            if (wpla(i)>=wplamx(i).or.ifail0 >= 4 .or.                         &
              dmg(i,2)>=dmax(i).or.                                            &
              crak(i,1)>=epsf1(i)-epst1(i)) nfis1(i)=nfis1(i)+1
            if (wpla(i)>=wplamx(i).or.ifail0 >= 4 .or.                         &
              dmg(i,3)>=dmax(i).or.                                            &
              crak(i,2)>=epsf2(i)-epst2(i)) nfis2(i)=nfis2(i)+1
            if (wpla(i)>=wplamx(i).or.ifail0 >= 4 .or. dmg(i,2)>=dmax(i).or.   &
              crak(i,1)>=epsf1(i)-epst1(i) .or. dmg(i,3)>=dmax(i).or.        &
              crak(i,2)>=epsf2(i)-epst2(i)) nfis3(i)=nfis3(i)+1
          enddo
!-------------------------------------------------------------------
!     failure
!-------------------------------------------------------------------
          do i=1,nel
            fail = 0
            if (dmg(i,2) >= dmax(i))            fail = fail + 1
            if (dmg(i,3) >= dmax(i))            fail = fail + 2
            if (wpla(i) >= wplamx(i))           fail = fail + 4
            if (crak(i,1) >= epsf1(i)-epst1(i)) fail = fail + 8
            if (crak(i,2) >= epsf2(i)-epst2(i)) fail = fail + 16
            if (fail > 0 ) offply(i) = zero

            if (fail /= fail_old(i) .and. off(i) == one) then
              fail = fail - fail_old(i)
              if (imconv == 1) then
!$OMP CRITICAL
                if (igtyp == 17 .or. igtyp == 51 .or. igtyp == 52) then
                  if (fail==1.or.fail==3.or.fail==5) write(iout,1001) ngl(i),ilayer,ipg,ply_id,time
                  if (fail==2.or.fail==3.or.fail==6) write(iout,1002) ngl(i),ilayer,ipg,ply_id,time
                  if (fail==4.or.fail==5.or.fail==6) then
                    if (icas(i) == 0) then
                      write(iout, 2000) ngl(i),wplamx(i),ilayer,ipg,ply_id,time
                    else if (icas(i) == 1) then
                      write(iout, 2001) ngl(i),wplamx(i),ilayer,ipg,ply_id,time
                    else if (icas(i) == -1) then
                      write(iout, 2002) ngl(i),wplamx(i),ilayer,ipg,ply_id,time
                    else if (icas(i) == 2) then
                      write(iout, 2003) ngl(i),wplamx(i),ilayer,ipg,ply_id,time
                    else if (icas(i) == -2) then
                      write(iout, 2004) ngl(i),wplamx(i),ilayer,ipg,ply_id,time
                    else if (icas(i) == 3) then
                      write(iout, 2005) ngl(i),wplamx(i),ilayer,ipg,ply_id,time
                    end if    ! icas
                  endif
                  if (fail >= 16) then
                    write(iout,1003) ngl(i),ilayer,ipg,ply_id,time
                  else if (fail >= 8) then
                    write(iout,1004) ngl(i),ilayer,ipg,ply_id,time
                  end if
!
                else   ! igtyp 11
!
                  if (fail==1.or.fail==3.or.fail==5) write(iout,3001) ngl(i),ilayer,ipg,time
                  if (fail==2.or.fail==3.or.fail==6) write(iout,3002) ngl(i),ilayer,ipg,time
                  if (fail==4.or.fail==5.or.fail==6) then
                    if (icas(i) == 0) then
                      write(iout,4000) ngl(i),wplamx(i),ilayer,ipg,time
                    else if (icas(i) == 1) then
                      write(iout,4001) ngl(i),wplamx(i),ilayer,ipg,time
                    else if (icas(i) == -1) then
                      write(iout,4002) ngl(i),wplamx(i),ilayer,ipg,time
                    else if (icas(i) == 2) then
                      write(iout,4003) ngl(i),wplamx(i),ilayer,ipg,time
                    else if (icas(i) == -2) then
                      write(iout,4004) ngl(i),wplamx(i),ilayer,ipg,time
                    else if (icas(i) == 3) then
                      write(iout,4005) ngl(i),wplamx(i),ilayer,ipg,time
                    endif
                  endif
                  if (fail >= 16) then
                    write(iout,3003) ngl(i),ilayer,ipg,time
                  else if (fail >= 8) then
                    write(iout,3004) ngl(i),ilayer,ipg,time
                  end if
                endif   ! igtyp
!$OMP END CRITICAL
              endif     ! imconv == 1
            endif       ! new failure
!
!       Wpla negative for stresses reduction at next cycle in case of failure-p :
            if (wpla(i)>=wplamx(i).or.mod(fail_old(i),8)>=4) wpla(i)=-wpla(i)
          enddo
!-------------------------------------------------------------------
!     end failure
!-------------------------------------------------------------------
          do i=1,nel
            sig(i,1)=t1(i)
            sig(i,2)=t2(i)
            sig(i,3)=t3(i)
          enddo
!------for qeph
          do i=1,nel
            sigy(i) = fyld(i)*sigy(i)

!        if (abs(beta(i)) < one) then
!          if (wpla(i) > zero .and. fyld(i) < fmax(i)) then
!            ht = cn(i)*cb(i)*exp((cn(i)-one)*log(wpla(i)))
!          else
!            ht = em10
!          end if
!          etse(i) = ht/(ht+e11(i))
!        end if
          end do
!-------------------
!     plasticity end
!-------------------
          do i=1,nel
            signxx(i) = sig(i,1)
            signyy(i) = sig(i,2)
            signxy(i) = sig(i,3)
            signyz(i) = sig(i,4)
            signzx(i) = sig(i,5)
          enddo
!-------------------------------------------------------------------
!     retour dans le repere coque
!-----------------------------
          do i=1,nel
            sige(i,1)=sig(i,1)
            sige(i,2)=sig(i,2)
            sige(i,3)=sig(i,3)
            sige(i,4)=sig(i,4)
            sige(i,5)=sig(i,5)
          enddo
!
          call urotov(1,nel,sige,dir,nel)
!
!-------------------------------------------------------------------
!     Ply Xfem
!-------------------------------------------------------------------
          if (ishplyxfem /= 0 .and. iplyxfem == 2) then
            do  i=1,nel
              t1(i) = sigply(i,1)
              t2(i) = sigply(i,2)
              t3(i) = sigply(i,3)
              wvec(i)= f1(i) *t1(i) + f2(i) * t2(i)                    &
                + f11(i)*t1(i)*t1(i) + f22(i)*t2(i)*t2(i)         &
                + f33(i)*t3(i)*t3(i) + two*f12(i)*t1(i)*t2(i)
            enddo
!
            do i=1,nel
              coef(i) = zero
              if (wvec(i)>fyld(i).and.off(i)==one) coef(i)=one
              cnn=cn(i)-one
              wvec(i)=zero
              if (wpla(i)>zero.and.fyld(i)<fmax(i)) wvec(i)=epspfac(i)*exp(cnn*log(wpla(i)))
            enddo
!
            do i=1,nel
              so1(i) = t1(i)
              so2(i) = t2(i)
              so3(i) = t3(i)
            enddo
            do  i=1,nel
              dp1(i)=f1(i)+two*f11(i)*so1(i)+two*f12(i)*so2(i)
              dp2(i)=f2(i)+two*f22(i)*so2(i)+two*f12(i)*so1(i)
              dp3(i)=two*f33(i)*so3(i)
            enddo
            do  i=1,nel
              ds1(i)=t1(i)-so1(i)
              ds2(i)=t2(i)-so2(i)
              ds3(i)=t3(i)-so3(i)
            enddo
!
            do i=1,nel
              lamda(i)=(dp1(i)*ds1(i)+dp2(i)*ds2(i)+dp3(i)*ds3(i))*coef(i)
              if (lamda(i) /= zero) then
                lamda(i)=lamda(i)*coef(i)/                               &
                  (dp1(i)*(a11(i)*dp1(i)+a12(i)*dp2(i))+                 &
                  dp2(i)*(a12(i)*dp1(i)+a22(i)*dp2(i))+                 &
                  two*dp3(i)*g12(i)*de1(i)*de2(i)*dp3(i)+                &
                  (so1(i)*dp1(i)+so2(i)*dp2(i)+two*so3(i)*dp3(i))        &
                  *cn(i)*cb(i)*wvec(i) )
              endif
            enddo
!
            do  i=1,nel
              dp1(i)=lamda(i)*dp1(i)
              dp2(i)=lamda(i)*dp2(i)
              dp3(i)=lamda(i)*dp3(i)
            enddo
!
            do  i=1,nel
              sigply(i,1)=t1(i)-a11(i)*dp1(i)-a12(i)*dp2(i)
              sigply(i,2)=t2(i)-a12(i)*dp1(i)-a22(i)*dp2(i)
              sigply(i,3)=t3(i)-g12(i)*de1(i)*de2(i)*dp3(i)*two
            enddo
!
            do i=1,nel
              sigpe(i,1) = sigply(i,1)
              sigpe(i,2) = sigply(i,2)
              sigpe(i,3) = sigply(i,3)
              sigpe(i,4) = zero
              sigpe(i,5) = zero
              strp1(i) = strp1(i) + eply(i,1)
              strp2(i) = strp2(i) + eply(i,2)
            enddo
!
            call urotov(1,nel,sigpe,dir,nel)
!
          end if    ! Ply Xfem
!-----------
          return
!-----------------------------------------------------------------------
1001      format(' FAILURE-1 ELEMENT #',i10,', LAYER #',i3,                       &
            ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)
1002      format(' FAILURE-2 ELEMENT #',i10,', LAYER #',i3,                       &
            ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)
1003      format(' TOTAL FAILURE-2 ELEMENT #',i10,', LAYER #',i3,                 &
            ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)
1004      format(' TOTAL FAILURE-1 ELEMENT #',i10,', LAYER #',i3,                 &
            ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)
2000      format(' FAILURE-P-MAX ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,    &
            ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)
2001      format(' FAILURE-P-T1 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &
            ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)
2002      format(' FAILURE-P-C1 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &
            ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)
2003      format(' FAILURE-P-T2 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &
            ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)
2004      format(' FAILURE-P-C2 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &
            ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)
2005      format(' FAILURE-P-T12 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,    &
            ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)
3001      format(' FAILURE-1 ELEMENT #',i10,', LAYER #',i3,                       &
            ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)
3002      format(' FAILURE-1 ELEMENT #',i10,', LAYER #',i3,                       &
            ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)
3003      format(' TOTAL FAILURE-2 ELEMENT #',i10,', LAYER #',i3,                 &
            ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)
3004      format(' TOTAL FAILURE-1 ELEMENT #',i10,', LAYER #',i3,                 &
            ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)
4000      format(' FAILURE-P-MAX ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,    &
            ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)
4001      format(' FAILURE-P-T1 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &
            ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)
4002      format(' FAILURE-P-C1 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &
            ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)
4003      format(' FAILURE-P-T2 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &
            ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)
4004      format(' FAILURE-P-C2 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &
            ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)
4005      format(' FAILURE-P-T12 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,    &
            ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)
!-----------------------------------------------------------------------
        end subroutine mat25_tsaiwu_c
      end module mat25_tsaiwu_c_mod
