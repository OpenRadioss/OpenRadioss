!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
!Chd|====================================================================
!Chd|  mat25_tsaiwu_s
!Chd|-- called by -----------
!Chd|-- calls ---------------
!Chd|====================================================================

      module mat25_tsaiwu_s_mod
      contains

! =================================================================================
! \brief nonlinear orthotropic material law25 with Tsai-Wu formulation for solids

! =================================================================================

        subroutine mat25_tsaiwu_s(mat_param,                          &
          nel   ,ngl   ,off   ,flay  ,                       &
          s1    ,s2    ,s3    ,s4    ,s5    ,s6    ,         &
          d1    ,d2    ,d3    ,d4    ,d5    ,d6    ,         &
          epst  ,nfis1 ,nfis2 ,nfis3 ,                       &
          wplar ,epsp  ,wpla  ,sigl  ,ilay  ,ipg   ,         &
          tsaiwu,time  ,imconv,mvsiz ,iout  ,dmg   ,         &
          l_dmg )
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
          use matparam_def_mod
          use constant_mod ,only : zero,half,one,two,four,four_over_5
          use constant_mod ,only : em10,em15,em20,ep20
! ---------------------------------------------------------------------------------
          implicit none
! ---------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------

#include "my_real.inc"

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
          my_real ,intent(in)    :: time                   !< current time
          my_real ,intent(inout) :: off(mvsiz)             !< element activation coefficient
          my_real ,intent(inout) :: wpla(mvsiz)            !< plastic work
          my_real ,intent(inout) :: epsp(mvsiz)            !< equivalent strain rate
          my_real ,intent(inout) :: epst(nel,6)            !< total strain tensor
          my_real ,intent(inout) :: wplar(mvsiz)           !< reference plastic work
          my_real ,intent(inout) :: s1(mvsiz)              !< stress component
          my_real ,intent(inout) :: s2(mvsiz)              !< stress component
          my_real ,intent(inout) :: s3(mvsiz)              !< stress component
          my_real ,intent(inout) :: s4(mvsiz)              !< stress component
          my_real ,intent(inout) :: s5(mvsiz)              !< stress component
          my_real ,intent(inout) :: s6(mvsiz)              !< stress component
          my_real ,intent(inout) :: d1(mvsiz)              !< strain increment
          my_real ,intent(inout) :: d2(mvsiz)              !< strain increment
          my_real ,intent(inout) :: d3(mvsiz)              !< strain increment
          my_real ,intent(inout) :: d4(mvsiz)              !< strain increment
          my_real ,intent(inout) :: d5(mvsiz)              !< strain increment
          my_real ,intent(inout) :: d6(mvsiz)              !< strain increment
          my_real ,intent(inout) :: sigl(mvsiz,6)          !< output stress tensor
          my_real ,intent(inout) :: flay(mvsiz)            !< layer failure coefficient
          my_real ,intent(inout) :: tsaiwu(nel)            !< Tsai-Wu criterion
          my_real ,intent(inout) :: dmg(nel,l_dmg)         !< damage related variables
          type (matparam_struct_) ,intent(in) :: mat_param !< material parameter structure
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer :: i,j,fail,ioff,icc,nindx
          integer :: fail_old(mvsiz),index(mvsiz),icas(mvsiz),isoft(mvsiz)
          my_real :: e11,e22,e33,nu12,nu21,g12,g23,g31,wplaref,cc,epdr
          my_real :: scale, cnn, scale1, scale2, dam1, dam2
          my_real :: strp12,coefa,coefb,delta,dwpla,e1,e2,e3,e4,e5,e6
          my_real                                                            &
            dp1(mvsiz), dp2(mvsiz), dp3(mvsiz),cb(mvsiz),cn(mvsiz),          &
            fmax(mvsiz),ds1(mvsiz), ds2(mvsiz), ds3(mvsiz),                  &
            de1(mvsiz), de2(mvsiz), wvec(mvsiz), t1(mvsiz),                  &
            t2(mvsiz), t3(mvsiz),lamda(mvsiz), coef(mvsiz),                  &
            a11(mvsiz), a12(mvsiz), a22(mvsiz),a21(mvsiz),                   &
            so1(mvsiz), so2(mvsiz), so3(mvsiz),wplamx(mvsiz),                &
            epst1(mvsiz),epst2(mvsiz),epsm1(mvsiz),epsm2(mvsiz),             &
            eps1t1(mvsiz), eps2t1(mvsiz), sigrst1(mvsiz),                    &
            eps1t2(mvsiz), eps2t2(mvsiz), sigrst2(mvsiz),                    &
            eps1c1(mvsiz), eps2c1(mvsiz), sigrsc1(mvsiz),                    &
            eps1c2(mvsiz), eps2c2(mvsiz), sigrsc2(mvsiz),                    &
            eps1t12(mvsiz), eps2t12(mvsiz), sigrst12(mvsiz),                 &
            dmax(mvsiz),fyld(mvsiz),                                         &
            f1(mvsiz), f2(mvsiz), f12(mvsiz), f11(mvsiz), f22(mvsiz),        &
            f33(mvsiz),epsf1(mvsiz),epsf2(mvsiz),eps(mvsiz,6)
          my_real :: soft(3)
!=======================================================================
          ioff   = mat_param%iparam(2)
          icc    = mat_param%iparam(3)

          e11    = mat_param%uparam(1)           !  pm(33)
          e22    = mat_param%uparam(2)           !  pm(34)
          e33    = mat_param%uparam(3)           !  pm(186)
          nu12   = mat_param%uparam(4)           !  pm(35)
          nu21   = mat_param%uparam(5)           !  pm(36)
          g12    = mat_param%uparam(6)           !  pm(37)
          g23    = mat_param%uparam(7)           !  pm(38)
          g31    = mat_param%uparam(8)           !  pm(39)
          wplaref= mat_param%uparam(9)           !  pm(68)
          cc     = mat_param%uparam(10)          !  pm(50)
          epdr   = mat_param%uparam(11)          !  pm(51)

          epst1(1:nel)  = mat_param%uparam(12)   !  pm(60)
          epst2(1:nel)  = mat_param%uparam(13)   !  pm(61)
          epsm1(1:nel)  = mat_param%uparam(14)   !  pm(62)
          epsm2(1:nel)  = mat_param%uparam(15)   !  pm(63)
          epsf1(1:nel)  = mat_param%uparam(16)   !  pm(98)
          epsf2(1:nel)  = mat_param%uparam(17)   !  pm(99)
          dmax (1:nel)  = mat_param%uparam(18)   !  pm(64)

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
          isoft(1:nel) = 0
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
            if (fail_old(i) < 4 .or. (fail_old(i) < 8.and. ioff<0) )then
              nindx=nindx+1
              index(nindx)=i
            end if
          enddo
!-------------------------------------------------------------------
!     reduction de sig sur critere wpla_old >= wplamx
!-----------------------------
#include   "nofusion.inc"
          do i=1,nel
            if ((fail_old(i) >= 4 .and. ioff >= 0) .or. fail_old(i) >= 8)then
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
!     elastic deformations
!-----------------------------
          de1(1:nel) = one-max( zero , sign(dmg(1:nel,2),s1(1:nel)) )
          de2(1:nel) = one-max( zero , sign(dmg(1:nel,3),s2(1:nel)) )
          do  i=1,nel
            scale = (half+sign(half,de1(i)-one))*(half+sign(half,de2(i)-one))
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
            scale1 =(half+sign(half,de1(i)-one))*(half+sign(half,de2(i)-one))
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
!      if (israte(i) == 0)epsp(i) = max(
!     .                   abs(eps(1,i)),abs(eps(2,i)),abs(eps(3,i)),
!     .                   abs(eps(4,i)),abs(eps(5,i))) / max(dt1,em20)
            if(epsp(i) > epdr) then
              epsp(i)=log(epsp(i)/epdr)
            else
              epsp(i)= zero
            endif
            coef(i)=zero
          enddo
!
          do i=1,nel
            epsp(i)=one + cc * epsp(i)
            if (wpla(i) /= zero) then
              fyld(i)=(one+cb(i)*exp(cn(i)*log(wpla(i))))*epsp(i)
            else
              fyld(i)=epsp(i)
            endif
            if(icc == 1.or.icc == 3)then
              fmax(i) = fmax(i)*epsp(i)
            endif
            if(icc == 3.or.icc == 4)then
              wplamx(i) = wplamx(i)*epsp(i)
            endif
            fyld(i)= min(fmax(i),fyld(i))
!
          enddo
!-------------------------------------------------------------------
!     plasticity
!-------------------------------------------------------------------
          do  i=1,nel
            wvec(i) = f1(i) *t1(i)       + f2(i) *t2(i) +          &
              f11(i)*t1(i)*t1(i) + f22(i)*t2(i)*t2(i) +     &
              f33(i)*t3(i)*t3(i) + two*f12(i)*t1(i)*t2(i)
            tsaiwu(i) = max(min(wvec(i)/fyld(i),one),tsaiwu(i))
          enddo
!
          do i=1,nel
            if (wvec(i) > fyld(i).and.off(i)  ==  one) coef(i)=one
            cnn=cn(i)-one
            wvec(i)=zero
            if(wpla(i) > zero.and.fyld(i) < fmax(i))                 &
              wvec(i)=epsp(i)*exp(cnn*log(wpla(i)))
          enddo
!
          do i=1,nel
            so1(i)= s1(i)
            so2(i)= s2(i)
            so3(i)= s4(i)
          enddo
!
          do  i=1,nel
            dp1(i)=f1(i)+two*f11(i)*so1(i)+two*f12(i)*so2(I)
            dp2(i)=f2(i)+two*f22(i)*so2(i)+two*f12(i)*so1(I)
            dp3(i)=two*f33(i)*so3(i)
          enddo
!
          do  i=1,nel
            ds1(i)=t1(i)-so1(i)
            ds2(i)=t2(i)-so2(i)
            ds3(i)=t3(i)-so3(i)
          enddo
!
          do  i=1,nel
            lamda(i)=(dp1(i)*ds1(i)+dp2(i)*ds2(i)+dp3(i)*DS3(I))*COEF(I)
            if (lamda(i) /= zero) then
              lamda(i)=lamda(i)*coef(i)/                                     &
                (dp1(i)*(a11(i)*dp1(i)+a12(i)*dp2(i))+                         &
                dp2(i)*(a12(i)*dp1(i)+a22(i)*dp2(i))+                          &
                two*dp3(i)*g12*de1(i)*de2(i)*dp3(i)+                        &
                (so1(i)*dp1(i)+so2(i)*dp2(i)+two*so3(i)*DP3(I))*cn(i)*cb(i)*wvec(i) )
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
            dwpla = half* (dp1(i)*(t1(i)+so1(i))                    &
              + dp2(i)*(t2(i)+so2(i))+ two*dp3(i)*(t3(i)+so3(i)))
            dwpla = max(dwpla ,zero) / wplaref
            wpla(i)=wpla(i)+ dwpla
          enddo
!
          do i=1,nel
            if (wplamx(i) < ep20) dmg(i,4) = min(wpla(i)/wplamx(i),one)
            if(wpla(i) >= wplamx(i)) wplar(i) = wplar(i)+oNE
            if(wpla(i) >= wplamx(i) .or. dmg(i,2) >= dmax(I).OR.         &
              epst(i,1) >= epsf1(i)) nfis1(i)=nfis1(i)+1
            if(wpla(i) >= wplamx(i) .or. dmg(i,3) >= dmax(I).OR.          &
              epst(i,3) >= epsf2(i)) nfis2(i)=nfis2(i)+1
            if(wpla(i) >= wplamx(i) .or. dmg(i,2) >= dmax(I).OR.          &
              epst(i,1) >= epsf1(i).or. dmg(i,3) >= dmax(I).OR.          &
              epst(i,2) >= epsf2(i)) nfis3(i)=nfis3(i)+1
          enddo
!
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
                  WRITE(IOUT, '(A,I10,A,I3,A,I3,A,1PE11.4)')              &
                    ' FAILURE-1 ELEMENT #',NGL(I),                           &
                    ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,            &
                    ', TIME=',time
                  flay(i) =  one
                ENDIF
                if(fail == 2.or.fail == 3.or.fail == 6) then
                  write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')             &
                    ' FAILURE-2 ELEMENT #',NGL(I),                           &
                    ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,            &
                    ', TIME=',time
                  flay(i) =  one
                endif
                if(fail == 4.or.fail == 5.or.fail == 6) then
                  flay(i) =  one
                  if(icas(i) == 0)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')             &
                      ' FAILURE-P ELEMENT #',NGL(I),                           &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,            &
                      ', TIME=',time
                  elseif(icas(i) == 1)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')            &
                      ' FAILURE-P-T1 ELEMENT #',NGL(I),                        &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,            &
                      ', TIME=',time
                  elseif(icas(i) == -1)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')            &
                      ' FAILURE-P-C1 ELEMENT #',NGL(I),                        &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,            &
                      ', TIME=',time
                  elseif(icas(i) == 2)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')             &
                      ' FAILURE-P-T2 ELEMENT #',NGL(I),                        &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,            &
                      ', TIME=',time
                  elseif(icas(i) == -2)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')            &
                      ' FAILURE-P-C2 ELEMENT #',NGL(I),                        &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,            &
                      ', TIME=',time
                  elseif(icas(i) == 3)then
                    write(iout, '(a,i10,a,i3,a,i3,x,a,1pe11.4)')            &
                      ' FAILURE-P-T12 ELEMENT #',NGL(I),                       &
                      ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,            &
                      ', TIME=',time
                  endif
                endif
                if(fail >= 16)then
                  write(iout, '(a,i10,a,i3,a,i3,a,1pe11.4)')                  &
                    ' TOTAL FAILURE-2 ELEMENT #',NGL(I),                      &
                    ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,             &
                    ', TIME=',time
                  flay(i) =  one
                elseif(fail >= 8)then
                  write(iout, '(a,i10,a,i3,a,i3,a,1pe11.4)')                  &
                    ' TOTAL FAILURE-1 ELEMENT #',NGL(I),                      &
                    ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,             &
                    ', TIME=',time
                  FLAY(I) =  ONE
                end if
!$OMP END CRITICAL
              endif
            endif
            ! Wpla negative for stresses reduction at next cycle in case of failure-p :
            if(wpla(i) >= wplamx(i) .OR. MOD(FAIL_OLD(I),8) >= 4) WPLA(I)=-WPLA(I)
          enddo
!
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
!-------------------
          return
        end subroutine mat25_tsaiwu_s
!-------------------
      end module mat25_tsaiwu_s_mod
