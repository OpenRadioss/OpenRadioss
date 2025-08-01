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
!||    mat25_crasurv_s_mod   ../engine/source/materials/mat/mat025/mat25_crasurv_s.F90
!||--- called by ------------------------------------------------------
!||    m25law                ../engine/source/materials/mat/mat025/m25law.F
!||====================================================================
      module mat25_crasurv_s_mod
      contains

! ======================================================================================================================
! \brief nonlinear orthotropic material law25 with Crasurv formulation for solids
! \details calculates stress-strain relationship using modified Tsai-Wu plasticity criterion

! ======================================================================================================================

!||====================================================================
!||    mat25_crasurv_s    ../engine/source/materials/mat/mat025/mat25_crasurv_s.F90
!||--- called by ------------------------------------------------------
!||    m25law             ../engine/source/materials/mat/mat025/m25law.F
!||--- calls      -----------------------------------------------------
!||    ancmsg             ../engine/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    message_mod        ../engine/share/message_module/message_mod.F
!||    precision_mod      ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine mat25_crasurv_s(mat_param   ,                     &
          nel  ,ngl ,off  ,flay,                            &
          s1  ,s2  ,s3, s4  ,s5  ,s6  ,                     &
          d1  ,d2  ,d3, d4  ,d5  ,d6  ,                     &
          epst,nfis1,nfis2,nfis3,                           &
          wplar,epsd , wpla, sigl, ilay,                    &
          ipg,tsaiwu,time,imconv  ,mvsiz   ,iout,           &
          dmg,l_dmg ,outv)
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
          use matparam_def_mod
          use message_mod
          use constant_mod ,only : zero,half,one,two,four,four_over_5
          use constant_mod ,only : em10,em15,em20,ep20
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
          integer ,intent(in) :: nel                       !< element group size
          integer ,intent(in) :: ilay                      !< layer number
          integer ,intent(in) :: ipg                       !< Gauss point number
          integer ,intent(in) :: imconv                    !< output flag
          integer ,intent(in) :: iout                      !< output file id
          integer ,intent(in) :: mvsiz                     !< max element group size
          integer ,intent(in) :: ngl(mvsiz)                !< element ID table
          integer ,intent(in) :: l_dmg                     !< second dimension of damage table
          integer ,dimension(nel) ,intent(inout) :: nfis1  !< failure counter in 1st direction
          integer ,dimension(nel) ,intent(inout) :: nfis2  !< failure counter in 2nd direction
          integer ,dimension(nel) ,intent(inout) :: nfis3  !< failure counter in 3rd direction
          integer ,dimension(nel) ,intent(inout) :: outv   !< outp counter
          real(kind=WP) ,intent(in)    :: time                   !< current time
          real(kind=WP) ,intent(inout) :: off(mvsiz)             !< element activation coefficient
          real(kind=WP) ,intent(inout) :: wpla(mvsiz)            !< plastic work
          real(kind=WP) ,intent(in)    :: epsd(mvsiz)            !< equivalent strain rate
          real(kind=WP) ,intent(inout) :: epst(nel,6)            !< total strain tensor
          real(kind=WP) ,intent(inout) :: wplar(mvsiz)           !< reference plastic work
          real(kind=WP) ,intent(inout) :: s1(mvsiz)              !< stress component
          real(kind=WP) ,intent(inout) :: s2(mvsiz)              !< stress component
          real(kind=WP) ,intent(inout) :: s3(mvsiz)              !< stress component
          real(kind=WP) ,intent(inout) :: s4(mvsiz)              !< stress component
          real(kind=WP) ,intent(inout) :: s5(mvsiz)              !< stress component
          real(kind=WP) ,intent(inout) :: s6(mvsiz)              !< stress component
          real(kind=WP) ,intent(inout) :: d1(mvsiz)              !< strain increment
          real(kind=WP) ,intent(inout) :: d2(mvsiz)              !< strain increment
          real(kind=WP) ,intent(inout) :: d3(mvsiz)              !< strain increment
          real(kind=WP) ,intent(inout) :: d4(mvsiz)              !< strain increment
          real(kind=WP) ,intent(inout) :: d5(mvsiz)              !< strain increment
          real(kind=WP) ,intent(inout) :: d6(mvsiz)              !< strain increment
          real(kind=WP) ,intent(inout) :: sigl(mvsiz,6)          !< output stress tensor
          real(kind=WP) ,intent(inout) :: flay(mvsiz)            !< layer failure coefficient
          real(kind=WP) ,intent(inout) :: tsaiwu(nel)            !< Tsai-Wu criterion
          real(kind=WP) ,intent(inout) :: dmg(nel,l_dmg)         !< damage related variables
          type (matparam_struct_) ,intent(in) :: mat_param !< material parameter structure
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer :: i,fail,iflag,imodwp,ioff,icc,id
          integer :: fail_old(mvsiz)
          integer :: nindx,index(mvsiz),j,icas(mvsiz)
          integer :: isoft(mvsiz)
          real(kind=WP) :: e11,e22,e33,nu12,nu21,g12,g23,g31,wplaref,cc,epdr
          real(kind=WP) :: e1,e2,e3,e4,e5,e6,alpha,strp12,coefa,coefb,delta,dwpla
          real(kind=WP) :: log_wpla,b1,b2,wpla1,wpla2,wpla3
          real(kind=WP) ::  scale,scale1,scale2,dam1,dam2
          real(kind=WP) :: sigyt1,sigyt2,sigyc1,sigyc2,sigyt12
          real(kind=WP) :: sigy0_t1,sigy0_t2,sigy0_c1,sigy0_c2,sigy0_t12
          real(kind=WP) :: sigmxt1,sigmxt2,sigmxc1,sigmxc2,sigmxt12
          real(kind=WP) :: cn_t1,cn_t2,cn_c1,cn_c2,cn_t12,cb_t1,cb_t2,cb_c1,cb_c2,cb_t12
          real(kind=WP) :: cc_t1,cc_t2,cc_c1,cc_c2,cc_t12
          real(kind=WP) ::                                                           &
            epst1(mvsiz),epst2(mvsiz),epsm1(mvsiz),epsm2(mvsiz),               &
            eps1t1(mvsiz), eps2t1(mvsiz), sigrst1(mvsiz),                      &
            eps1t2(mvsiz), eps2t2(mvsiz), sigrst2(mvsiz),                      &
            eps1c1(mvsiz), eps2c1(mvsiz), sigrsc1(mvsiz),                      &
            eps1c2(mvsiz), eps2c2(mvsiz), sigrsc2(mvsiz),                      &
            eps1t12(mvsiz), eps2t12(mvsiz), sigrst12(mvsiz),dmax(mvsiz)
          real(kind=WP) ::                                                           &
            dp1(mvsiz), dp2(mvsiz), dp3(mvsiz),cb(mvsiz),cn(mvsiz),            &
            ds1(mvsiz), ds2(mvsiz), ds3(mvsiz),                                &
            de1(mvsiz), de2(mvsiz), wvec(mvsiz), t1(mvsiz),                    &
            t2(mvsiz), t3(mvsiz),lamda(mvsiz), coef(mvsiz),                    &
            a11(mvsiz), a12(mvsiz), a22(mvsiz),a21(mvsiz),                     &
            so1(mvsiz), so2(mvsiz), so3(mvsiz),wplamx(mvsiz),                  &
            f1(mvsiz),f2(mvsiz),f12(mvsiz),f11(mvsiz),f22(mvsiz),f33(mvsiz),   &
            epsf1(mvsiz),epsf2(mvsiz),eps(mvsiz,6),soft(3),                    &
            wplamxt1(mvsiz),wplamxt2(mvsiz),wplamxc1(mvsiz),wplamxc2(mvsiz),   &
            wplamxt12(mvsiz),beta(mvsiz)
          real(kind=WP) :: frate(mvsiz)
!=======================================================================
          iflag  = mat_param%iparam(1)
          ioff   = mat_param%iparam(2)
          icc    = mat_param%iparam(3)
          imodwp = mat_param%iparam(4)
!
          e11    = mat_param%uparam(1)            !      pm(33)
          e22    = mat_param%uparam(2)            !      pm(34)
          e33    = mat_param%uparam(3)            !      pm(186)
          nu12   = mat_param%uparam(4)            !      pm(35)
          nu21   = mat_param%uparam(5)            !      pm(36)
          g12    = mat_param%uparam(6)            !      pm(37)
          g23    = mat_param%uparam(7)            !      pm(38)
          g31    = mat_param%uparam(8)            !      pm(39)
          wplaref= mat_param%uparam(9)            !      pm(68)
          cc     = mat_param%uparam(10)           !      pm(50)
          epdr   = mat_param%uparam(11)           !      pm(51)
!
#include "vectorize.inc"
          do  i=1,nel
            epst1(i)  = mat_param%uparam(12)      !  pm(60)
            epst2(i)  = mat_param%uparam(13)      !  pm(61)
            epsm1(i)  = mat_param%uparam(14)      !  pm(62)
            epsm2(i)  = mat_param%uparam(15)      !  pm(63)
            epsf1(i)  = mat_param%uparam(16)      !  pm(98)
            epsf2(i)  = mat_param%uparam(17)      !  pm(99)
            dmax (i)  = mat_param%uparam(18)      !  pm(64)
          enddo

          wplamx(1:nel) = mat_param%uparam(20)
          f1(1:nel)     = mat_param%uparam(21)
          f2(1:nel)     = mat_param%uparam(22)
          f11(1:nel)    = mat_param%uparam(23)
          f22(1:nel)    = mat_param%uparam(24)
          f33(1:nel)    = mat_param%uparam(25)
          f12(1:nel)    = mat_param%uparam(26)
!

#include "vectorize.inc"
          do  i=1,nel
            eps1t1(i)  = mat_param%uparam(27)
            eps1t2(i)  = mat_param%uparam(28)
            eps1c1(i)  = mat_param%uparam(29)
            eps1c2(i)  = mat_param%uparam(30)
            eps1t12(i) = mat_param%uparam(31)
            
            eps2t1(i)  = mat_param%uparam(32)
            eps2t2(i)  = mat_param%uparam(33)
            eps2c1(i)  = mat_param%uparam(34)
            eps2c2(i)  = mat_param%uparam(35)
            eps2t12(i) = mat_param%uparam(36)

            sigrst1(i) = mat_param%uparam(37)
            sigrst2(i) = mat_param%uparam(38)
            sigrsc1(i) = mat_param%uparam(39)
            sigrsc2(i) = mat_param%uparam(40)
            sigrst12(i)= mat_param%uparam(41)
          enddo

          sigy0_t1 = mat_param%uparam(42)
          sigy0_t2 = mat_param%uparam(43)
          sigy0_c1 = mat_param%uparam(44)
          sigy0_c2 = mat_param%uparam(45)
          sigy0_t12= mat_param%uparam(46)

          sigmxt1  = mat_param%uparam(47)
          sigmxt2  = mat_param%uparam(48)
          sigmxc1  = mat_param%uparam(49)
          sigmxc2  = mat_param%uparam(50)
          sigmxt12 = mat_param%uparam(51)

          cb_t1    = mat_param%uparam(52)
          cb_t2    = mat_param%uparam(53)
          cb_c1    = mat_param%uparam(54)
          cb_c2    = mat_param%uparam(55)
          cb_t12   = mat_param%uparam(56)

          cn_t1    = mat_param%uparam(57)
          cn_t2    = mat_param%uparam(58)
          cn_c1    = mat_param%uparam(59)
          cn_c2    = mat_param%uparam(60)
          cn_t12   = mat_param%uparam(61)

          cc_t1    = mat_param%uparam(62)           !  pm(145)
          cc_t2    = mat_param%uparam(63)           !  pm(150)
          cc_c1    = mat_param%uparam(64)           !  pm(155)
          cc_c2    = mat_param%uparam(65)           !  pm(160)
          cc_t12   = mat_param%uparam(66)           !  pm(165)

          wplamxt1(1:nel) = mat_param%uparam(67)
          wplamxt2(1:nel) = mat_param%uparam(68)
          wplamxc1(1:nel) = mat_param%uparam(69)
          wplamxc2(1:nel) = mat_param%uparam(70)
          wplamxt12(1:nel)= mat_param%uparam(71)
!-----------------------------------------------
!     element variable initialization
!--------------------------------
          isoft(1:nel) = 0

          do i=1,nel
            cb(i)   = zero  ! pm(46)
            cn(i)   = one   ! pm(47)
!        fmax(i) = one   ! pm(49)
!        fyld(i) = one
          enddo
!-------------------------------------------------------------------
!     old failure
!-----------------------------
#include   "nofusion.inc"
          do i=1,nel
            fail_old(i)= 0
            if(dmg(i,2) >= dmax(i))fail_old(i) = fail_old(i) + 1
            if(dmg(i,3) >= dmax(i))fail_old(i) = fail_old(i) + 2
!
            if(wpla(i) < zero )then
!         wpla is negative in case of layer already reached failure-p :
              fail_old(i) = fail_old(i) + 4
              wpla(i)     = -wpla(i)
            end if
!
            if(epst(i,1) >= epsf1(i) )then
              fail_old(i) = fail_old(i) + 8
            end if
            if(epst(i,2) >= epsf2(i) )then
              fail_old(i) = fail_old(i) + 16
            end if
          enddo
          nindx=0
#include   "nofusion.inc"
          do i=1,nel
            if( fail_old(i) < 4.or. (fail_old(i) < 8.and. ioff<0) )then
              nindx=nindx+1
              index(nindx)=i
            end if
          enddo
!-------------------------------------------------------------------
!     reduction de sig sur critere wpla_old >= wplamx
!-----------------------------
#include   "nofusion.inc"
          do i=1,nel
            if((fail_old(i) >= 4 .and. ioff >= 0) .or. fail_old(i) >= 8) then
              s1(i)=four_over_5*s1(i)
              s2(i)=four_over_5*s2(i)
              s3(i)=four_over_5*s3(i)
              s4(i)=four_over_5*s4(i)
              s5(i)=four_over_5*s5(i)
              s6(i)=four_over_5*s6(i)
!
              if(abs(s1(i)) < em20) s1(i)=zero
              if(abs(s2(i)) < em20) s2(i)=zero
              if(abs(s3(i)) < em20) s3(i)=zero
              if(abs(s4(i)) < em20) s4(i)=zero
              if(abs(s5(i)) < em20) s5(i)=zero
              if(abs(s6(i)) < em20) s6(i)=zero
!
              d1(i)=zero
              d2(i)=zero
              d3(i)=zero
              d4(i)=zero
              d5(i)=zero
              d6(i)=zero
            endif
          enddo
!-------------------------------------------------------------------
!     deformations elastiques
!-----------------------------
          de1(1:nel)  = one-max( zero , sign(dmg(1:nel,2),s1(1:nel)) )
          de2(1:nel)  = one-max( zero , sign(dmg(1:nel,3),s2(1:nel)) )
          do  i=1,nel
            scale = (half +sign(half,de1(i)-one))*(half+sign(half,de2(i)-one))
            e1 = s1(i)/de1(i)/e11  - nu21*s2(i)*scale/e22
            e2 = s2(i)/de2(i)/e22  - nu12*s1(i)*scale/e11
            e3 = s3(i)/ e33
            e4=s4(i)/de1(i)/de2(i)/g12
            e5=s5(i)/max(de2(i)*g23,em20)
            e6=s6(i)/max(de1(i)*g31,em20)
!
            eps(i,1)= e1 + d1(i)
            eps(i,2)= e2 + d2(i)
            eps(i,3)= e3 + d3(i)
            eps(i,4)= e4 + d4(i)
            eps(i,5)= e5 + d5(i)
            eps(i,6)= e6 + d6(i)
          enddo
!
          epst(1:nel,1) = epst(1:nel,1) + d1(1:nel)
          epst(1:nel,2) = epst(1:nel,2) + d2(1:nel)
          epst(1:nel,3) = epst(1:nel,3) + d3(1:nel)
          epst(1:nel,4) = epst(1:nel,4) + d4(1:nel)
          epst(1:nel,5) = epst(1:nel,5) + d5(1:nel)
          epst(1:nel,6) = epst(1:nel,6) + d6(1:nel)
!
#include "vectorize.inc"
          do j=1,nindx
            i=index(j)
            if (dmg(i,2) > zero) then
              dam1=(epst(i,1)-epst1(i))/(epsm1(i)-epst1(i))
              dam2= dam1*epsm1(i)/epst(i,1)
              dmg(i,2)= max(dmg(i,2),dam2)
              dmg(i,2)= min(dmg(i,2),dmax(i))
            endif
          enddo
!
#include "vectorize.inc"
          do j=1,nindx
            i=index(j)
            if (dmg(i,3) > zero) then
              dam1=(epst(i,2) - epst2(i))/(epsm2(i)-epst2(i))
              dam2= dam1*epsm2(i)/epst(i,2)
              dmg(i,3)= max(dmg(i,3),dam2)
              dmg(i,3)= min(dmg(i,3),dmax(i))
            endif
          enddo
!
          do i=1,nel
            de1(i)=one- max( zero , sign(dmg(i,2),s1(i)) )
            de2(i)=one- max( zero , sign(dmg(i,3),s2(i)) )
            scale1 =(half +sign(half,de1(i)-one))*(half+sign(half,de2(i)-one))
            scale2 =one-nu12*nu21*scale1
            a11(i)= e11*de1(i)/scale2
            a22(i)= e22*de2(i)/scale2
            a12(i)=nu21*a11(i)*scale1
            a21(i)=nu12*a22(i)*scale1
          enddo
!-----------------------------
!     elastic stress
!-----------------------------
          do  i=1,nel
            t1(i) =a11(i)*eps(i,1)+a12(i)*eps(i,2)
            t2(i) =a21(i)*eps(i,1)+a22(i)*eps(i,2)
            t3(i) =de1(i)*de2(i)*g12*eps(i,4)
            s3(i) =e33*eps(i,3)
            s5(i) =de2(i)*g23*eps(i,5)
            s6(i) =de1(i)*g31*eps(i,6)
          enddo
!
          do i=1,nel
            if(t1(i) > 0) then
              if(t2(i) > 0) icas(i) = 1
              if(t2(i) < 0) icas(i) = 4
            else
              if(t2(i) > 0) icas(i) = 2
              if(t2(i) < 0) icas(i) = 3
            endif
          enddo
!-------------------------------------------------------------------
!     strain rate
!-------------------------------------------------------------------
          do i=1,nel
            if(epsd(i) > epdr) then
              frate(i)=log(epsd(i)/epdr)
            else
              frate(i)= zero
            endif
            coef(i)=zero
          enddo
!-------------------------------------------------------------------
#include "vectorize.inc"
          do i=1,nel
            if (wpla(i) > zero) then
              log_wpla = log(wpla(i))
              sigyt1 = sigy0_t1  * (one + cb_t1 *exp(cn_t1 *log_wpla))
              sigyt2 = sigy0_t2  * (one + cb_t2 *exp(cn_t2 *log_wpla))
              sigyc1 = sigy0_c1  * (one + cb_c1 *exp(cn_c1 *log_wpla))
              sigyc2 = sigy0_c2  * (one + cb_c2 *exp(cn_c2 *log_wpla))
              sigyt12= sigy0_t12 * (one + cb_t12*exp(cn_t12*log_wpla))
            else
              sigyt1 = sigy0_t1
              sigyt2 = sigy0_t2
              sigyc1 = sigy0_c1
              sigyc2 = sigy0_c2
              sigyt12= sigy0_t12
            endif

            if (icc==1 .or. icc==3) then
              sigyt1 = min(sigyt1 ,sigmxt1)
              sigyt2 = min(sigyt2 ,sigmxt2)
              sigyc1 = min(sigyc1 ,sigmxc1)
              sigyc2 = min(sigyc2 ,sigmxc2)
              sigyt12= min(sigyt12,sigmxt12)
              sigyt1 = sigyt1*(one  + cc_t1 *frate(i))
              sigyt2 = sigyt2*(one  + cc_t2 *frate(i))
              sigyc1 = sigyc1*(one  + cc_c1 *frate(i))
              sigyc2 = sigyc2*(one  + cc_c2 *frate(i))
              sigyt12= sigyt12*(one + cc_t12*frate(i))
            else
              sigyt1 = sigyt1*(one  + cc_t1 *frate(i))
              sigyt2 = sigyt2*(one  + cc_t2 *frate(i))
              sigyc1 = sigyc1*(one  + cc_c1 *frate(i))
              sigyc2 = sigyc2*(one  + cc_c2 *frate(i))
              sigyt12= sigyt12*(one + cc_t12*frate(i))
              sigyt1 = min(sigyt1 ,sigmxt1 )
              sigyt2 = min(sigyt2 ,sigmxt2 )
              sigyc1 = min(sigyc1 ,sigmxc1 )
              sigyc2 = min(sigyc2 ,sigmxc2 )
              sigyt12= min(sigyt12,sigmxt12)
            endif
!-----------------------------------------------------------------
!       softening
!-----------------------------------------------------------------
            soft(1)=zero
            soft(2)=zero
            soft(3)=zero
            isoft(i) = 0
            ! direction 1
            ! -> failure index in compression
            if ((epst(i,1) < zero).and.(epst(i,1) > -eps1c1(i))) then
              dmg(i,5) = max(-abs(epst(i,1))/abs(eps1c1(i)),-one)
            elseif (epst(i,1) <= -eps1c1(i)) then
              soft(1)  = min(one,(epst(i,1)+eps1c1(i))/(eps1c1(i)-eps2c1(i)))
              dmg(i,5) = max(-one-soft(1),-1.95d0)
              ! -> failure index in tension
            elseif (epst(i,1) >= zero .and. epst(i,1) < eps1t1(i)) then
              dmg(i,5) = min(epst(i,1)/eps1t1(i),one)
            elseif (epst(i,1) >= eps1t1(i)) then
              soft(1)  = min(one,(epst(i,1)-eps1t1(i))/(eps2t1(i)-eps1t1(i)))
              dmg(i,5) = min(one + soft(1),1.95d0)
            endif
            sigyt1=min(sigyt1,(one -soft(1))*sigyt1+soft(1)*sigrst1(i))
            sigyc1=min(sigyc1,(one -soft(1))*sigyc1+soft(1)*sigrsc1(i))
            ! direction 2
            ! -> failure index in compression
            if ((epst(i,2) < zero).and.(epst(i,2) > -eps1c2(i))) then
              dmg(i,6) = max(-abs(epst(i,2))/abs(eps1c2(i)),-one)
            elseif (epst(i,2) <= -eps1c2(i)) then
              soft(2)  = min(one,(epst(i,2)+eps1c2(i))/(eps1c2(i)-eps2c2(i)))
              dmg(i,6) = max(- one - soft(2),-1.95d0)
              ! -> failure index in tension
            elseif (epst(i,2) >= zero .and. epst(i,2) < eps1t2(i)) then
              dmg(i,6) = min(epst(i,2)/eps1t2(i),one)
            elseif (epst(i,2) >= eps1t2(i)) then
              soft(2)  = min(one,(epst(i,2)-eps1t2(i))/(eps2t2(i)-eps1t2(i)))
              dmg(i,6) = min(one + soft(2),1.95d0)
            endif
            sigyt2=min(sigyt2,(one -soft(2))*sigyt2+soft(2)*sigrst2(i))
            sigyc2=min(sigyc2,(one -soft(2))*sigyc2+soft(2)*sigrsc2(i))
            ! direction 12
            strp12 = half*epst(i,4)
            ! -> failure index in shear
            if (strp12 <= - eps1t12(i)) then
              soft(3)  = min(one,(strp12 + eps1t12(i))/(eps1t12(i)-eps2t12(i)))
              dmg(i,7) = min(one + soft(3),1.95d0)
            elseif (strp12 >= eps1t12(i)) then
              soft(3)  = min(one,( strp12 - eps1t12(i))/(eps2t12(i)-eps1t12(i)))
              dmg(i,7) = min(one + soft(3),1.95d0)
            else
              dmg(i,7) = min(abs(strp12)/eps1t12(i),one)
            endif
            sigyt12=min(sigyt12,(one -soft(3))*sigyt12+soft(3)*sigrst12(i))
!
            if(soft(1) + soft(2) + soft(3) /= zero) isoft(i) =1
!
            f1(i)  = one/sigyt1-one/sigyc1
            f2(i)  = one/sigyt2-one/sigyc2
            f11(i) = one/(sigyt1*sigyc1)
            f22(i) = one/(sigyt2*sigyc2)
            f33(i) = one/(sigyt12*sigyt12)
            alpha=f12(i)
            f12(i) = -alpha/(two*sqrt(sigyt1*sigyc1*sigyt2*sigyc2))
!
            frate(i) = one + cc * frate(i)
            if(icc == 3.or.icc == 4)then
              wplamx(i) = wplamx(i)*frate(i)
            endif
          enddo
!-------------------------------------------------------------------
!     plasticity
!-------------------------------------------------------------------
          do  i=1,nel
            wvec(i) = f1(i) *t1(i)       + f2(i) *t2(i) +            &
              f11(i)*t1(i)*t1(i) + f22(i)*t2(i)*t2(i) +       &
              f33(i)*t3(i)*t3(i) + two*f12(i)*t1(i)*t2(i)
            tsaiwu(i) = max(min(wvec(i),one),tsaiwu(i))
          enddo
!
          do i=1,nel
            if (wvec(i) > one .and. off(i) == one) coef(i)=one
            wvec(i)=zero
          enddo
!
         nindx = 0
         do i=1,nel
            beta(i) = one
            if (coef(i) == one) then
              coefa = f11(i)*t1(i)*t1(i) + f22(i)*t2(i)*t2(i)       &
                + f33(i)*t3(i)*t3(i) + two*f12(i)*t1(i)*t2(i)
              coefb = f1(i)*t1(i) + f2(i)*t2(i)
              delta = coefb*coefb + four*coefa
              if (delta >= zero) then
                delta = sqrt(delta)

                if (abs(coefa) > em15 .or. abs(coefa) <= em15 .and. abs(coefb) > em15) then
                  if (abs(coefa) <= em15 .and. abs(coefb) > em15) then
                    beta(i) = one/coefb
                  endif
                  b1 = half * (coefb - delta) / coefa
                  b2 = half * (coefb + delta) / coefa
                  if (abs(one + b1) <= abs(one + b2)) then
                    beta(i) = -b1
                  else
                    beta(i) = -b2
                  endif
                  delta = (f1(i) * t1(i) + f2(i) * t2(i)            &
                    + f11(i) * beta(i)*t1(i)*t1(i)              &
                    + f22(i) * beta(i)*t2(i)*t2(i)              &
                    + f33(i) * beta(i)*t3(i)*t3(i)              &
                    + two*f12(i)*beta(i)*t1(i)*t2(i)) * beta(i)

                 else if (imconv==1 .and. outv(i) == 0) then  ! coefa<em15 & coefb<em15   
                   nindx = nindx + 1
                   index(nindx) = i              
                endif
              else if (imconv == 1 .and. outv (i) == 0) then   ! delta < 0
                   nindx = nindx + 1
                   index(nindx) = i
                  outv(i) = 1                 
              endif   ! delta > 0
            end if    ! coef == 1
          enddo
          if(nindx > 0) then
           do j=1,nindx       
              call ancmsg(msgid=244,anmode=aninfo, i1=ngl(index(j)))
           end do
          endif
!-----------------------------------------------------------------------
          do i=1,nel
            so1(i)=beta(i)*t1(i)
            so2(i)=beta(i)*t2(i)
            so3(i)=beta(i)*t3(i)
          enddo
!
          do  i=1,nel
            dp1(i)=f1(i)+two*f11(i)*so1(i)+two*f12(i)*so2(I)
            dp2(i)=f2(i)+two*f22(i)*so2(i)+two*f12(i)*so1(I)
            dp3(i)=two*f33(i)*so3(i)
          enddo
!
          do  i=1,nel
            ds1(i)=t1(i) - so1(i)
            ds2(i)=t2(i) - so2(i)
            ds3(i)=t3(i) - so3(i)
          enddo
!
          do  i=1,nel
            lamda(i)=(dp1(i)*ds1(i)+dp2(i)*ds2(i)+dp3(i)*DS3(I))*COEF(I)
            if(lamda(i) /= zero) then
              lamda(i)=lamda(i)*coef(i)/                             &
                (dp1(i)*(a11(i)*dp1(i)+a12(i)*dp2(i))+               &
                 dp2(i)*(a12(i)*dp1(i)+a22(i)*dp2(i))+               &
                 two*dp3(i)*g12*de1(i)*de2(i)*dp3(i)  )
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
            t1(i)=t1(i)-a11(i)*dp1(i)-a12(i)*dp2(i)
            t2(i)=t2(i)-a12(i)*dp1(i)-a22(i)*dp2(i)
            t3(i)=t3(i)-g12*de1(i)*de2(i)*dp3(i)*two
          enddo
!
          do  i=1,nel
            dwpla = half* (dp1(i)*(t1(i)+so1(i))+ dp2(i)*(t2(i)+so2(i))      &
              + two*dp3(i)*(t3(i)+so3(i)))
            dwpla = max(dwpla ,zero) / wplaref
            wpla(i)=wpla(i)+ dwpla
          enddo
!
          do i=1,nel
            sigyt1 = sigy0_t1
            sigyt2 = sigy0_t2
            sigyc1 = sigy0_c1
            sigyc2 = sigy0_c2
            sigyt12= sigy0_t12
!
            ! failure index for global maximum plastic work
            if (wplamx(i) < ep20) dmg(i,4) = min(wpla(i)/wplamx(i),one)
!
            if (imodwp == 0 .or. (imodwp> 0 .and. isoft(I) == 0)) then
!
              ! direction 1
              wpla1 = ep20
              if(t1(i) >= sigyt1 ) then
                wpla1 = wplamxt1(i)
                id = 1
              elseif(t1(i) <= -sigyc1) then
                wpla1 = wplamxc1(i)
                id =-1
              else
                wpla1 = ep20
                id = -HUGE(id)
              endif
!
              if(wpla1 < wplamx(i)) then
                wplamx(i) = wpla1
                icas(i) = id
              endif
!
              ! failure index in direction 1 update
              if (wpla(i)/wplamx(i) >= 0.95d0) then
                dmg(i,5) = sign(two,dmg(i,5))
              elseif (dmg(i,5) > one) then
                dmg(i,5) = max(dmg(i,5),one + wpla(i)/wplamx(i))
              elseif (dmg(i,5) < -one) then
                dmg(i,5) = min(dmg(i,5),-one-wpla(i)/wplamx(i))
              endif
!
              ! direction 2
              wpla2 = ep20
              if(t2(i) >= sigyt2  ) then
                wpla2 = wplamxt2(i)
                id =  2
              elseif(t2(i) <= -sigyc2)then
                wpla2 = wplamxc2(i)
                id = -2
              endif
              if(wpla2 < wplamx(i) ) then
                wplamx(i) = wpla2
                icas(i) = id
              endif
!
              ! failure index in direction 2 update
              if (wpla(i)/wplamx(i) >= 0.95d0) then
                dmg(i,6) = sign(two,dmg(i,6))
              elseif (dmg(i,6) > one) then
                dmg(i,6) = max(dmg(i,6),one + wpla(i)/wplamx(i))
              elseif (dmg(i,6) < -one) then
                dmg(i,6) = min(dmg(i,6),-one-wpla(i)/wplamx(i))
              endif
!
              ! shear
              wpla3 = ep20
              if(abs(t3(i)) >= sigyt12) then
                wpla3 = wplamxt12(i)
                id = 3
              endif
              if( wpla3 < wplamx(i) )then
                wplamx(i) = wpla3
                icas(i) = id
              endif
!
              ! failure index in shear plane 12 update
              if (wpla(i)/wplamx(i) >= 0.95d0) then
                dmg(i,7) = two
              elseif (dmg(i,7) > one) then
                dmg(i,7) = max(dmg(i,7),one + wpla(i)/wplamx(i))
              endif
            elseif (imodwp > 0 ) THEN
              id = 1
              if(abs(t2(i)) > abs(t1(i)))then
                id = 2
                if(abs(t3(i)) > abs(t2(i))) id = 3
              elseif( abs(t3(i)) > abs(t1(i))) then
                id = 3
              endif
              select case(id)
               case(1)
                icas(i) = sign(one,t1(i))
                if (icas(i) > 0 ) then
                  wplamx(i) = min(wplamx(i),wplamxt1(I))
                  ! update failure index in direction 1 (tension)
                  if (wpla(i)/wplamx(i) >= 0.95d0) then
                    dmg(i,5) = two
                  elseif (dmg(i,5) > one) then
                    dmg(i,5) = max(dmg(i,5),one + wpla(i)/wplamx(i))
                  endif
                else
                  wplamx(i)  = min(wplamx(i),wplamxC1(I))
                  ! update failure index in direction 1 (compression)
                  if (wpla(i)/wplamx(i) >= 0.95d0) then
                    dmg(i,5) = -two
                  elseif (dmg(i,5) < -one) then
                    dmg(i,5) = min(dmg(i,5),-one-wpla(i)/wplamx(i))
                  endif
                endif
               case(2)
                icas(i) = sign(two,t2(i))
                if(icas(i) > 0 ) then
                  wplamx(i) = min(wplamx(i),wplamxt2(I))
                  ! update failure index in direction 2 (tension)
                  if (wpla(i)/wplamx(i) >= 0.95d0) then
                    dmg(i,6) = two
                  elseif (dmg(i,6) > one) then
                    dmg(i,6) = max(dmg(i,6),one + wpla(i)/wplamx(i))
                  endif
                else
                  wplamx(i) = min(wplamx(i),wplamxc2(I))
                  ! update failure index in direction 2 (compression)
                  if (wpla(i)/wplamx(i) >= 0.95d0) then
                    dmg(i,6) = -two
                  elseif (dmg(i,6) < -one) then
                    dmg(i,6) = min(dmg(i,6),-one-wpla(i)/wplamx(i))
                  endif
                endif
               case(3)
                icas(i) = 3
                wplamx(i) = min(wplamx(i),wplamxt12(I))
                ! update failure index in plane 12 (shear)
                if (wpla(i)/wplamx(i) >= 0.95d0) then
                  dmg(i,7) = two
                elseif (dmg(i,7) > one) then
                  dmg(i,7) = max(dmg(i,7),one + wpla(i)/wplamx(i))
                endif
              end select
            endif ! iflag == 1 ...
          enddo
!
          do i=1,nel
            if(wpla(i) >= wplamx(i)) wplar(i) = wplar(i)+oNE
            if(wpla(i) >= wplamx(i) .or. dmg(i,2) >= dmax(I).or.          &
              epst(i,1) >= epsf1(i)) nfis1(i)=nfis1(i)+1
            if(wpla(i) >= wplamx(i) .or. dmg(i,3) >= dmax(I).or.           &
              epst(i,3) >= epsf2(i)) nfis2(i)=nfis2(i)+1
            if(wpla(i) >= wplamx(i) .or. dmg(i,2) >= dmax(I).or.           &
              epst(i,1) >= epsf1(i).or. dmg(i,3) >= dmax(I).or.           &
              epst(i,2) >= epsf2(i)) nfis3(i)=nfis3(i)+1
          enddo
!-------------------------------------------------------------------
!     failure
!-----------------------------
          do i=1,nel
            fail=0
            if(dmg(i,2) >= dmax(i))fail = fail + 1
            if(dmg(i,3) >= dmax(i))fail = fail + 2
            if(wpla(i) >= wplamx(i))fail = fail + 4
            if(epst(i,1) >= epsf1(i))fail=fail+8
            if(epst(i,2) >= epsf2(i))fail=fail+16
            if(fail /= fail_old(i).and.off(i) == one) then
              fail = fail - fail_old(i)
              if(imconv == 1)then
!$OMP CRITICAL
                if(fail == 1.or.fail == 3.or.fail == 5) then
                  write(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')                  &
                    ' FAILURE-1 ELEMENT #',NGL(I),                               &
                    ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,                &
                    ', TIME=',time
                  flay(i) =  one
                ENDIF
                if(fail == 2.or.fail == 3.or.fail == 6) then
                  write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')                 &
                    ' FAILURE-2 ELEMENT #',NGL(I),                               &
                    ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,                &
                    ', TIME=',time
                  flay(i) =  one
                endif
                if(fail == 4.or.fail == 5.or.fail == 6) then
                  flay(i) =  one
                  if(icas(i) == 0)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')                 &
                      ' FAILURE-P ELEMENT #',NGL(I),                               &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,                &
                      ', TIME=',time
                  elseif(icas(i) == 1)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')                &
                      ' FAILURE-P-T1 ELEMENT #',NGL(I),                            &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,                &
                      ', TIME=',time
                  elseif(icas(i) == -1)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')                &
                      ' FAILURE-P-C1 ELEMENT #',NGL(I),                            &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,                &
                      ', TIME=',time
                  elseif(icas(i) == 2)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')                 &
                      ' FAILURE-P-T2 ELEMENT #',NGL(I),                            &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,                &
                      ', TIME=',time
                  elseif(icas(i) == -2)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')               &
                      ' FAILURE-P-C2 ELEMENT #',NGL(I),                            &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,                &
                      ', TIME=',time
                  elseif(icas(i) == 3)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')                &
                      ' FAILURE-P-T12 ELEMENT #',NGL(I),                           &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,                &
                      ', TIME=',time
                  endif
                endif
                if(fail >= 16)then
                  write(iout, '(a,i10,a,i3,a,i3,a,1pe11.4)')                      &
                    ' TOTAL FAILURE-2 ELEMENT #',NGL(I),                          &
                    ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,                 &
                    ', TIME=',time
                  flay(i) =  one
                elseif(fail >= 8)then
                  write(iout, '(a,i10,a,i3,a,i3,a,1pe11.4)')                      &
                    ' TOTAL FAILURE-1 ELEMENT #',NGL(I),                          &
                    ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,                 &
                    ', TIME=',time
                  FLAY(I) =  ONE
                end if
!$OMP END CRITICAL
              endif
            endif
!
!       Wpla negative for stresses reduction at next cycle in case of failure-p :
!
            if(wpla(i) >= wplamx(i) .or. mod(fail_old(i),8) >= 4) wpla(I)=-wpla(I)
          enddo
!-------------------
          do  i=1,nel
            s1(i)=t1(i)
            s2(i)=t2(i)
            s4(i)=t3(i)
            sigl(i,1) = s1(i)
            sigl(i,2) = s2(i)
            sigl(i,3) = s3(i)
            sigl(i,3) = s4(i)
            sigl(i,5) = s5(i)
            sigl(i,6) = s6(i)
          enddo
!-------------------
!     plasticity end
!------------------------------------------------------------------------
3000      format(' ** WARNING : MATERIAL LAW 25 - ELEMENT ID =',i10)
3010      format('cannot project stresses on criteria')
!------------------------------------------------------------------------
          return
        end subroutine mat25_crasurv_s
!-------------------
      end module mat25_crasurv_s_mod
