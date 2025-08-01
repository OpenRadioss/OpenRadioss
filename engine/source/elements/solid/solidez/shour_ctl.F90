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
!||    shour_ctl_mod   ../engine/source/elements/solid/solidez/shour_ctl.F90
!||--- called by ------------------------------------------------------
!||    s6chour_ctl     ../engine/source/elements/thickshell/solide6c/s6chour_ctl.F90
!||    scforc3         ../engine/source/elements/thickshell/solidec/scforc3.F
!||    szhour_ctl      ../engine/source/elements/solid/solidez/szhour_ctl.F
!||====================================================================
      module shour_ctl_mod
      implicit none
      contains
! ======================================================================================================================
! \brief new hexa hourglass formulation for distordtion control
! ======================================================================================================================
! ======================================================================================================================
!||====================================================================
!||    shour_ctl       ../engine/source/elements/solid/solidez/shour_ctl.F90
!||--- called by ------------------------------------------------------
!||    s6chour_ctl     ../engine/source/elements/thickshell/solide6c/s6chour_ctl.F90
!||    scforc3         ../engine/source/elements/thickshell/solidec/scforc3.F
!||    szhour_ctl      ../engine/source/elements/solid/solidez/szhour_ctl.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine  shour_ctl(                                  &
                             pm,      rho,     off,     vx1,    &
                             vx2,     vx3,     vx4,     vx5,    &
                             vx6,     vx7,     vx8,     vy1,    &
                             vy2,     vy3,     vy4,     vy5,    &
                             vy6,     vy7,     vy8,     vz1,    &
                             vz2,     vz3,     vz4,     vz5,    &
                             vz6,     vz7,     vz8,     f11,    &
                             f21,     f31,     f12,     f22,    &
                             f32,     f13,     f23,     f33,    &
                             f14,     f24,     f34,     f15,    &
                             f25,     f35,     f16,     f26,    &
                             f36,     f17,     f27,     f37,    &
                             f18,     f28,     f38,     px1h1,  &
                             px1h2,   px1h3,   px2h1,   px2h2,  &
                             px2h3,   px3h1,   px3h2,   px3h3,  &
                             px4h1,   px4h2,   px4h3,   vol,    &
                             fhour,   mtn,      dt1 ,    mat,   &  
                             cxx  ,   eint ,  npropm, nummat,   &
                             vol0 ,    dn  ,   sti  ,   nel )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod,   only : one,two,eight,ten,zero,zep5,half,third,fourth,zep00666666667,      &
                                 four_over_3,one_over_8,one_over_64,em01,two_third,four
      use precision_mod, only : WP
      use mvsiz_mod, only : mvsiz
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                           d u m m y   a r g u m e n t s
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                              :: nel              !< number of elements
          integer, intent(in)                              :: mtn              !< material type
          integer, intent(in)                              :: npropm           !< number of properties
          integer, intent(in)                              :: nummat           !< number of laws
          integer, dimension(mvsiz), intent(in   )         :: mat              !< material id
          real(kind=WP), dimension(npropm,nummat) ,intent(in)    :: pm               !< material data
          real(kind=WP), dimension(nel,3,4)       ,intent(inout) :: fhour            !< hourglass stress 
          real(kind=WP), dimension(nel),   intent(in   )         :: rho              !< density
          real(kind=WP), dimension(mvsiz), intent(in   )         :: cxx              !< speed sound 
          real(kind=WP), dimension(mvsiz), intent(in   )         :: off              !< off value 
          real(kind=WP), dimension(mvsiz), intent(in   )         :: vol              !< volume 
          real(kind=WP), dimension(mvsiz), intent(in   )         :: vol0             !< initial volume 
          real(kind=WP), dimension(mvsiz) ,intent(in   )         ::             &
                           vx1,vx2,vx3,vx4,vx5,vx6,vx7,vx8,               &     
                           vy1,vy2,vy3,vy4,vy5,vy6,vy7,vy8,               &     
                           vz1,vz2,vz3,vz4,vz5,vz6,vz7,vz8                     !< nodal velocity
          real(kind=WP), dimension(mvsiz) ,intent(in   )         ::             &
                           px1h1, px1h2, px1h3,                           &
                           px2h1, px2h2, px2h3,                           &
                           px3h1, px3h2, px3h3,                           &
                           px4h1, px4h2, px4h3                                 !< hourgalss parameters
         real(kind=WP), dimension(mvsiz)  ,intent(inout)         ::             &
                           f11,f21,f31,f12,f22,f32,                       &
                           f13,f23,f33,f14,f24,f34,                       &
                           f15,f25,f35,f16,f26,f36,                       &
                           f17,f27,f37,f18,f28,f38                             !< nodal internal force
         real(kind=WP), dimension(mvsiz)  ,intent(inout)         :: eint             !< internal energy (hourglass)
         real(kind=WP), dimension(mvsiz)  ,intent(inout)         :: sti              !< nodal stiffness
         real(kind=WP), intent(in   )                            :: dt1              !< time step
         real(kind=WP), intent(in   )                            :: dn               !< coef of viscous hourglass 
! ----------------------------------------------------------------------------------------------------------------------
!                                        l o c a l   v a r i a b l e s
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i, mx, j
!
      real(kind=WP)    :: caq(mvsiz), fcl(mvsiz), edt(mvsiz),                      &
                 hx1(mvsiz), hx2(mvsiz), hx3(mvsiz), hx4(mvsiz),          &
                 hy1(mvsiz), hy2(mvsiz), hy3(mvsiz), hy4(mvsiz),          &
                 hz1(mvsiz), hz2(mvsiz), hz3(mvsiz), hz4(mvsiz),          &
                 hgx1(mvsiz), hgx2(mvsiz), hgx3(mvsiz), hgx4(mvsiz),      &
                 hgy1(mvsiz), hgy2(mvsiz), hgy3(mvsiz), hgy4(mvsiz),      &
                 hgz1(mvsiz), hgz2(mvsiz), hgz3(mvsiz), hgz4(mvsiz),      &
                 g11(mvsiz),g21(mvsiz),g31(mvsiz),g41(mvsiz),             &
                 g51(mvsiz),g61(mvsiz),g71(mvsiz),g81(mvsiz),             &
                 g12(mvsiz),g22(mvsiz),g32(mvsiz),g42(mvsiz),             &
                 g52(mvsiz),g62(mvsiz),g72(mvsiz),g82(mvsiz),             &
                 g13(mvsiz),g23(mvsiz),g33(mvsiz),g43(mvsiz),             &
                 g53(mvsiz),g63(mvsiz),g73(mvsiz),g83(mvsiz),             &
                 e0,g0,c1,nu,qh,lamg ,stif ,ll ,fvl,lamgt(mvsiz),         &
                 f_et(mvsiz),f_sti(mvsiz),sfac,e_max,s_max,sfac1,f_gt(mvsiz)
! ----------------------------------------------------------------------------------------------------------------------
      mx = mat(1)
      nu=pm(21,mx)
      g0=pm(22,mx)  ! 2g for law 42,69,62,70
      c1=pm(32,mx)
      e0=pm(20,mx)
      qh = one
      lamgt(1:nel)=cxx(1:nel)*cxx(1:nel)*rho(1:nel)
      select case (mtn)
        case (70)
          e0=pm(24,mx)
          c1 = third*e0/(1-two*nu)
          g0 = half*e0/(1+nu)
        case (42,69)
          c1 = third*e0/(1-two*nu)
          g0 = half*g0
          f_gt(1:nel)=max(one,(lamgt(1:nel)-c1)/g0/four_over_3)
        case (62)
! ten for the moment
          g0 = half*g0
        case (90)
          qh = fourth*qh
      end select
      lamg = c1+four_over_3*g0
      f_et(1:nel)=max(one,lamgt(1:nel)/lamg)
      f_sti(1:nel)=one
      if (nu>0.48999) qh = zep5*qh
      stif = 0.3*qh*lamg     ! factor=8*1/8/3;  
!      fvl = fourth*dn*zep00666666667   
      fvl = fourth*dn*em01   ! 1/10 of isolid=5
! special case for stiffing      
      select case (mtn)
        case (42,69)
          do i=1,nel
            sfac = min(four,sqrt(f_gt(i)))
            f_et(i)=sfac*f_et(i)
            f_sti(i)=sfac*f_sti(i)
          enddo
        case (62)
          do i=1,nel
            sfac = min(ten,f_et(i))
            if (sfac>two) sfac=ten
            f_et(i)=sfac*f_et(i)
            f_sti(i)=sfac*f_sti(i)
          enddo
        case (90)
          f_et(1:nel)=max(one,qh*lamgt(1:nel)/lamg)
      end select
! law1,62 need high stiffness for distortion ctl but dt decreasing       
! increasing stif only after some hourglass strain for law1,62      
      if (mtn==1.or.mtn==62) then
        do i=1,nel
            s_max=zero
          do j=1,4
            s_max=max(s_max,abs(fhour(i,1,j)),abs(fhour(i,2,j)),abs(fhour(i,3,j)))
          end do
          sfac=f_et(i)*stif*vol(i)**two_third
! hourglass max_strain estimation      
          e_max=s_max/sfac
          sfac1 = min(ten,2500*e_max)
          sfac1 = max(one,sfac1)
          f_et(i)=sfac1*f_et(i)
          f_sti(i)=sfac1*f_sti(i)
        end do
      end if
!
      do i=1,nel
         ll = off(i)*vol(i)**third
         caq(i)=stif*dt1
         edt(i)=f_et(i)*caq(i)*ll
         fcl(i)=fvl*rho(i)*cxx(i)*ll*ll
         sti(i) = f_sti(i)*sti(i)
      enddo
!	  
       do i=1,nel
! 1 1 -1 -1 -1 -1 1 1 mode 1
        g11(i)= one_over_8-px1h1(i)
        g21(i)= one_over_8-px2h1(i)
        g31(i)=-one_over_8-px3h1(i)
        g41(i)=-one_over_8-px4h1(i)
        g51(i)=-one_over_8+px3h1(i)
        g61(i)=-one_over_8+px4h1(i)
        g71(i)= one_over_8+px1h1(i)
        g81(i)= one_over_8+px2h1(i)
        hgx1(i)=g11(i)*vx1(i)+g21(i)*vx2(i)+g31(i)*vx3(i)+g41(i)*vx4(i)    &
               +g51(i)*vx5(i)+g61(i)*vx6(i)+g71(i)*vx7(i)+g81(i)*vx8(i)
        hgy1(i)=g11(i)*vy1(i)+g21(i)*vy2(i)+g31(i)*vy3(i)+g41(i)*vy4(i)    &
               +g51(i)*vy5(i)+g61(i)*vy6(i)+g71(i)*vy7(i)+g81(i)*vy8(i)
        hgz1(i)=g11(i)*vz1(i)+g21(i)*vz2(i)+g31(i)*vz3(i)+g41(i)*vz4(i)    &
               +g51(i)*vz5(i)+g61(i)*vz6(i)+g71(i)*vz7(i)+g81(i)*vz8(i)
       enddo
!
       do i=1,nel
! 1 -1 -1 1 -1 1 1 -1 mode 2
        g12(i)= one_over_8-px1h2(i)
        g22(i)=-one_over_8-px2h2(i)
        g32(i)=-one_over_8-px3h2(i)
        g42(i)= one_over_8-px4h2(i)
        g52(i)=-one_over_8+px3h2(i)
        g62(i)= one_over_8+px4h2(i)
        g72(i)= one_over_8+px1h2(i)
        g82(i)=-one_over_8+px2h2(i)
        hgx2(i)=g12(i)*vx1(i)+g22(i)*vx2(i)+g32(i)*vx3(i)+g42(i)*vx4(i)    &
               +g52(i)*vx5(i)+g62(i)*vx6(i)+g72(i)*vx7(i)+g82(i)*vx8(i)
        hgy2(i)=g12(i)*vy1(i)+g22(i)*vy2(i)+g32(i)*vy3(i)+g42(i)*vy4(i)    &
               +g52(i)*vy5(i)+g62(i)*vy6(i)+g72(i)*vy7(i)+g82(i)*vy8(i)
        hgz2(i)=g12(i)*vz1(i)+g22(i)*vz2(i)+g32(i)*vz3(i)+g42(i)*vz4(i)    &
               +g52(i)*vz5(i)+g62(i)*vz6(i)+g72(i)*vz7(i)+g82(i)*vz8(i)
       enddo
       do i=1,nel
! 1 -1 1 -1 1 -1 1 -1 mode 3
        g13(i)= one_over_8-px1h3(i)
        g23(i)=-one_over_8-px2h3(i)
        g33(i)= one_over_8-px3h3(i)
        g43(i)=-one_over_8-px4h3(i)
        g53(i)= one_over_8+px3h3(i)
        g63(i)=-one_over_8+px4h3(i)
        g73(i)= one_over_8+px1h3(i)
        g83(i)=-one_over_8+px2h3(i)
        hgx3(i)=g13(i)*vx1(i)+g23(i)*vx2(i)+g33(i)*vx3(i)+g43(i)*vx4(i)    &
               +g53(i)*vx5(i)+g63(i)*vx6(i)+g73(i)*vx7(i)+g83(i)*vx8(i)
        hgy3(i)=g13(i)*vy1(i)+g23(i)*vy2(i)+g33(i)*vy3(i)+g43(i)*vy4(i)    &
               +g53(i)*vy5(i)+g63(i)*vy6(i)+g73(i)*vy7(i)+g83(i)*vy8(i)
        hgz3(i)=g13(i)*vz1(i)+g23(i)*vz2(i)+g33(i)*vz3(i)+g43(i)*vz4(i)    &
               +g53(i)*vz5(i)+g63(i)*vz6(i)+g73(i)*vz7(i)+g83(i)*vz8(i)
       enddo
!       
       do i=1,nel
! 1 -1 1 -1 -1 1 -1 1
         hgx4(i)= one_over_64*(                                            &
              vx1(i)-vx2(i)+vx3(i)-vx4(i)-vx5(i)+vx6(i)-vx7(i)+vx8(i))
         hgy4(i)= one_over_64*(                                            &
              vy1(i)-vy2(i)+vy3(i)-vy4(i)-vy5(i)+vy6(i)-vy7(i)+vy8(i))
         hgz4(i)= one_over_64*(                                            &
              vz1(i)-vz2(i)+vz3(i)-vz4(i)-vz5(i)+vz6(i)-vz7(i)+vz8(i))
       enddo
!
       do i=1,nel
         fhour(i,1:3,1:4) = fhour(i,1:3,1:4)*off(i) 
       enddo
       do i=1,nel
         fhour(i,1,1) = fhour(i,1,1) + edt(i)*hgx1(i)
         fhour(i,1,2) = fhour(i,1,2) + edt(i)*hgx2(i)
         fhour(i,1,3) = fhour(i,1,3) + edt(i)*hgx3(i)
         fhour(i,1,4) = fhour(i,1,4) + edt(i)*hgx4(i)
         fhour(i,2,1) = fhour(i,2,1) + edt(i)*hgy1(i)
         fhour(i,2,2) = fhour(i,2,2) + edt(i)*hgy2(i)
         fhour(i,2,3) = fhour(i,2,3) + edt(i)*hgy3(i)
         fhour(i,2,4) = fhour(i,2,4) + edt(i)*hgy4(i)
         fhour(i,3,1) = fhour(i,3,1) + edt(i)*hgz1(i)
         fhour(i,3,2) = fhour(i,3,2) + edt(i)*hgz2(i)
         fhour(i,3,3) = fhour(i,3,3) + edt(i)*hgz3(i)
         fhour(i,3,4) = fhour(i,3,4) + edt(i)*hgz4(i)
       enddo
       do i=1,nel
         hx1(i)=(fhour(i,1,1) +  fcl(i)*hgx1(i))*eight
         hx2(i)=(fhour(i,1,2) +  fcl(i)*hgx2(i))*eight
         hx3(i)=(fhour(i,1,3) +  fcl(i)*hgx3(i))*eight
         hx4(i)=(fhour(i,1,4) +  fcl(i)*hgx4(i))*eight
         hy1(i)=(fhour(i,2,1) +  fcl(i)*hgy1(i))*eight
         hy2(i)=(fhour(i,2,2) +  fcl(i)*hgy2(i))*eight
         hy3(i)=(fhour(i,2,3) +  fcl(i)*hgy3(i))*eight
         hy4(i)=(fhour(i,2,4) +  fcl(i)*hgy4(i))*eight
         hz1(i)=(fhour(i,3,1) +  fcl(i)*hgz1(i))*eight
         hz2(i)=(fhour(i,3,2) +  fcl(i)*hgz2(i))*eight
         hz3(i)=(fhour(i,3,3) +  fcl(i)*hgz3(i))*eight
         hz4(i)=(fhour(i,3,4) +  fcl(i)*hgz4(i))*eight
       enddo
!
       do i=1,nel
        f11(i)=f11(i)-g11(i)*hx1(i)-g12(i)*hx2(i)-g13(i)*hx3(i)-hx4(i)
        f12(i)=f12(i)-g21(i)*hx1(i)-g22(i)*hx2(i)-g23(i)*hx3(i)+hx4(i)
        f13(i)=f13(i)-g31(i)*hx1(i)-g32(i)*hx2(i)-g33(i)*hx3(i)-hx4(i)
        f14(i)=f14(i)-g41(i)*hx1(i)-g42(i)*hx2(i)-g43(i)*hx3(i)+hx4(i)
        f15(i)=f15(i)-g51(i)*hx1(i)-g52(i)*hx2(i)-g53(i)*hx3(i)+hx4(i)
        f16(i)=f16(i)-g61(i)*hx1(i)-g62(i)*hx2(i)-g63(i)*hx3(i)-hx4(i)
        f17(i)=f17(i)-g71(i)*hx1(i)-g72(i)*hx2(i)-g73(i)*hx3(i)+hx4(i)
        f18(i)=f18(i)-g81(i)*hx1(i)-g82(i)*hx2(i)-g83(i)*hx3(i)-hx4(i)
!
        f21(i)=f21(i)-g11(i)*hy1(i)-g12(i)*hy2(i)-g13(i)*hy3(i)-hy4(i)
        f22(i)=f22(i)-g21(i)*hy1(i)-g22(i)*hy2(i)-g23(i)*hy3(i)+hy4(i)
        f23(i)=f23(i)-g31(i)*hy1(i)-g32(i)*hy2(i)-g33(i)*hy3(i)-hy4(i)
        f24(i)=f24(i)-g41(i)*hy1(i)-g42(i)*hy2(i)-g43(i)*hy3(i)+hy4(i)
        f25(i)=f25(i)-g51(i)*hy1(i)-g52(i)*hy2(i)-g53(i)*hy3(i)+hy4(i)
        f26(i)=f26(i)-g61(i)*hy1(i)-g62(i)*hy2(i)-g63(i)*hy3(i)-hy4(i)
        f27(i)=f27(i)-g71(i)*hy1(i)-g72(i)*hy2(i)-g73(i)*hy3(i)+hy4(i)
        f28(i)=f28(i)-g81(i)*hy1(i)-g82(i)*hy2(i)-g83(i)*hy3(i)-hy4(i)
!
        f31(i)=f31(i)-g11(i)*hz1(i)-g12(i)*hz2(i)-g13(i)*hz3(i)-hz4(i)
        f32(i)=f32(i)-g21(i)*hz1(i)-g22(i)*hz2(i)-g23(i)*hz3(i)+hz4(i)
        f33(i)=f33(i)-g31(i)*hz1(i)-g32(i)*hz2(i)-g33(i)*hz3(i)-hz4(i)
        f34(i)=f34(i)-g41(i)*hz1(i)-g42(i)*hz2(i)-g43(i)*hz3(i)+hz4(i)
        f35(i)=f35(i)-g51(i)*hz1(i)-g52(i)*hz2(i)-g53(i)*hz3(i)+hz4(i)
        f36(i)=f36(i)-g61(i)*hz1(i)-g62(i)*hz2(i)-g63(i)*hz3(i)-hz4(i)
        f37(i)=f37(i)-g71(i)*hz1(i)-g72(i)*hz2(i)-g73(i)*hz3(i)+hz4(i)
        f38(i)=f38(i)-g81(i)*hz1(i)-g82(i)*hz2(i)-g83(i)*hz3(i)-hz4(i)
       enddo
! 
      do i=1,nel
        eint(i)= eint(i)+dt1*(                                            &
        hz1(i)*hgz1(i) + hz2(i)*hgz2(i) +                                 &
        hz3(i)*hgz3(i) + hz4(i)*hgz4(i) +                                 &
        hx1(i)*hgx1(i) + hx2(i)*hgx2(i) +                                 &
        hx3(i)*hgx3(i) + hx4(i)*hgx4(i) +                                 &
        hy1(i)*hgy1(i) + hy2(i)*hgy2(i) +                                 &
        hy3(i)*hgy3(i) + hy4(i)*hgy4(i) )/vol0(i) 
      enddo
!
      end subroutine shour_ctl
!-------------------
      end module shour_ctl_mod
