!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
!chd|====================================================================
!chd|  mat25_crasurv_c                 source/materials/mat/mat025/mat25_crasurv_c.F90
!chd|-- called by -----------
!chd|-- calls ---------------
!chd|====================================================================

      module mat25_crasurv_c_mod
      contains

! ======================================================================================================================
! \brief nonlinear orthotropic material law25 with Crasurv formulation
! \details calculates stress-strain relationship using modified Tsai-Wu plasticity criterion

! ======================================================================================================================
      subroutine mat25_crasurv_c(mat_param   ,                                &
                 nel     ,off     ,sig   ,                                    &             
                 wpla    ,dir     ,damt    ,crak  ,                           &            
                 nfis1   ,nfis2   ,nfis3   ,ilayer  ,shf   ,                  &            
                 ngl     ,eps     ,igtyp   ,wplar   ,strn1 ,                  &            
                 strn2   ,strn3   ,strp1   ,strp2   ,sige  ,                  &            
                 epsp    ,israte  ,offply  ,sigy    ,etse  ,                  &            
                 outv    ,ishplyxfem,ly_exx,ly_eyy,                           &            
                 ly_exy  ,sigply  ,sigpe   ,ply_id  ,                         &            
                 signxx  ,signyy  ,signxy  ,signyz  ,signzx,                  &            
                 ipg     ,tsaiwu  ,iplyxfem,time    ,timestep,                &            
                 imconv  ,mvsiz   ,iout    )                                      
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use matparam_def_mod
      use message_mod
      use constant_mod ,only : zero,half,one,two,four,four_over_5
      use constant_mod ,only : em10,em15,em20,ep20
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!     included files
! ----------------------------------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer ,intent(in) :: nel
      integer ,intent(in) :: ilayer
      integer ,intent(in) :: ipg
      integer ,intent(in) :: israte
      integer ,intent(in) :: igtyp
      integer ,intent(in) :: ishplyxfem
      integer ,intent(in) :: iplyxfem
      integer ,intent(in) :: ply_id
      integer ,intent(in) :: imconv
      integer ,intent(in) :: mvsiz
      integer ,intent(in) :: iout
      integer ,intent(in) :: ngl(mvsiz)
      my_real ,intent(in) :: time
      my_real ,intent(in) :: timestep
      integer ,dimension(nel) ,intent(inout) :: outv
      integer ,dimension(nel) ,intent(inout) :: nfis1
      integer ,dimension(nel) ,intent(inout) :: nfis2
      integer ,dimension(nel) ,intent(inout) :: nfis3
      my_real :: off(nel)
      my_real :: sig(nel,5)
      my_real :: wpla(nel)
      my_real :: dir(nel,2)
      my_real :: damt(nel,2)
      my_real :: crak(nel,2)
      my_real :: shf(nel)
      my_real :: epsp(nel)
      my_real :: eps(mvsiz,5)
      my_real :: wplar(mvsiz)
      my_real :: strp1(mvsiz)
      my_real :: strp2(mvsiz)
      my_real :: strn1(mvsiz)
      my_real :: strn2(mvsiz)
      my_real :: strn3(mvsiz)
      my_real :: sige(mvsiz,5)
      my_real :: offply(nel)
      my_real :: sigy(nel)
      my_real :: etse(nel)
      my_real :: ly_exx(nel)
      my_real :: ly_eyy(nel)
      my_real :: ly_exy(nel)
      my_real :: sigply(nel,3)
      my_real :: sigpe(mvsiz,5)
      my_real :: signxx(mvsiz)
      my_real :: signyy(mvsiz)
      my_real :: signxy(mvsiz)
      my_real :: signyz(mvsiz)
      my_real :: signzx(mvsiz)
      my_real :: tsaiwu(mvsiz)
      type (matparam_struct_) ,intent(in) :: mat_param
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,j,fail,ioff,icc,imodwp,nindx,id,ifail0
      integer ,dimension(nel) :: index,icas,isoft
      integer ,dimension(nel) :: fail_old
      my_real :: scale,scale1,scale2,dam1,dam2,sigyt1,sigyt2,sigyc1
      my_real :: sigyc2,sigyt12,alpha
      my_real :: strp12,coefa,coefb,delta,dwpla
      my_real :: wpla1,wpla2,wpla3,norm1,norm2,norm3,ht
      my_real :: e11,e22,g12,g23,g31,nu12,nu21
      my_real :: epst1,epst2,eps1t12,eps2t12,epsm1,epsm2
      my_real :: eps1t1,eps1t2,eps1c1,eps1c2,epsf1
      my_real :: eps2t1,eps2t2,eps2c1,eps2c2,epsf2
      my_real :: sigrst1,sigrst2,sigrsc1,sigrsc2,sigrst12
      my_real :: sigmxt1,sigmxt2,sigmxc1,sigmxc2,sigmxt12
      my_real :: sigy0_t1,sigy0_t2,sigy0_c1,sigy0_c2,sigy0_t12
      my_real :: wplaref,cc,epdr,dmax
      my_real :: cn_t1,cn_t2,cn_c1,cn_c2,cn_t12,cb_t1,cb_t2,cb_c1,cb_c2,cb_t12
      my_real :: cc_t1,cc_t2,cc_c1,cc_c2,cc_t12
      my_real :: log_wpla,b1,b2
      my_real ,dimension(3)       :: soft
      my_real ,dimension(nel)     :: dp1,dp2,dp3,de1,de2,ds1,ds2,ds3
      my_real ,dimension(nel)     :: wvec,t1,t2, t3,lamda,coef,a11,a12,a22
      my_real ,dimension(nel)     :: beta
      my_real ,dimension(nel)     :: f1,f2,f12,f11,f22,f33
      my_real ,dimension(nel)     :: so1,so2,so3,s1,s2,s3,s4,s5
      my_real ,dimension(nel)     :: wplamx,wplamxt1,wplamxt2
      my_real ,dimension(nel)     :: wplamxc1,wplamxc2,wplamxt12
      my_real ,dimension(mvsiz,5) :: epsply
      my_real ,dimension(mvsiz,3) :: eply
!=======================================================================
      if (time == zero) then
        offply(1:nel) = one
      endif      

      ioff   = mat_param%iparam(2)
      icc    = mat_param%iparam(3)
      imodwp = mat_param%iparam(4) 

!      ioff(i)   = nint(pm(42))
!      icc       = nint(pm(53))
!      imodwp(i) = nint(pm(189))

!-----------------------------------------------
!     constant input parameters
!---------------------------------   --------------
      e11      = mat_param%uparam(1)            !            pm(33) 
      e22      = mat_param%uparam(2)            !            pm(34) 
!     e33      = mat_param%uparam(3)            !            pm(186)
      nu12     = mat_param%uparam(4)            !            pm(35) 
      nu21     = mat_param%uparam(5)            !            pm(36) 
      g12      = mat_param%uparam(6)            !            pm(37) 
      g23      = mat_param%uparam(7)            !            pm(38) 
      g31      = mat_param%uparam(8)            !            pm(39) 
      wplaref  = mat_param%uparam(9)            !            pm(68) 
      cc       = mat_param%uparam(10)           !            pm(50) 
      epdr     = mat_param%uparam(11)           !            pm(51) 
                                                                    
      epst1    = mat_param%uparam(12)           !            pm(60) 
      epst2    = mat_param%uparam(13)           !            pm(61) 
      epsm1    = mat_param%uparam(14)           !            pm(62) 
      epsm2    = mat_param%uparam(15)           !            pm(63) 
      epsf1    = mat_param%uparam(16)           !            pm(98) 
      epsf2    = mat_param%uparam(17)           !            pm(99) 
      dmax     = mat_param%uparam(18)           !            pm(64) 
      ! ratio                                   !  mat_param%uparam(19)

      wplamx(1:nel) = mat_param%uparam(20)       !         pm(41)
      f1(1:nel)     = mat_param%uparam(21)       !         pm(54)
      f2(1:nel)     = mat_param%uparam(22)       !         pm(55)
      f11(1:nel)    = mat_param%uparam(23)       !         pm(56)
      f22(1:nel)    = mat_param%uparam(24)       !         pm(57)
      f33(1:nel)    = mat_param%uparam(25)       !         pm(58)
      f12(1:nel)    = mat_param%uparam(26)       !         pm(59)

      eps1t1   = mat_param%uparam(30)           !         pm(166) 
      eps1t2   = mat_param%uparam(31)           !         pm(170) 
      eps1c1   = mat_param%uparam(32)           !         pm(174) 
      eps1c2   = mat_param%uparam(33)           !         pm(178) 
      eps1t12  = mat_param%uparam(34)           !         pm(182) 

      eps2t1   = mat_param%uparam(35)           !         pm(167) 
      eps2t2   = mat_param%uparam(36)           !         pm(171) 
      eps2c1   = mat_param%uparam(37)           !         pm(175) 
      eps2c2   = mat_param%uparam(38)           !         pm(179) 
      eps2t12  = mat_param%uparam(39)           !         pm(183) 

      cb_t1    = mat_param%uparam(40)           !         pm(142) 
      cb_t2    = mat_param%uparam(41)           !         pm(147) 
      cb_c1    = mat_param%uparam(42)           !         pm(152) 
      cb_c2    = mat_param%uparam(43)           !         pm(157) 
      cb_t12   = mat_param%uparam(44)           !         pm(162) 

      cn_t1    = mat_param%uparam(45)           !         pm(143) 
      cn_t2    = mat_param%uparam(46)           !         pm(148) 
      cn_c1    = mat_param%uparam(47)           !         pm(153) 
      cn_c2    = mat_param%uparam(48)           !         pm(158) 
      cn_t12   = mat_param%uparam(49)           !         pm(163) 

      cc_t1    = mat_param%uparam(50)           !         pm(145) 
      cc_t2    = mat_param%uparam(51)           !         pm(150) 
      cc_c1    = mat_param%uparam(52)           !         pm(155) 
      cc_c2    = mat_param%uparam(53)           !         pm(160) 
      cc_t12   = mat_param%uparam(54)           !         pm(165) 
                                                                  
      sigrst1  = mat_param%uparam(55)           !         pm(168) 
      sigrst2  = mat_param%uparam(56)           !         pm(172) 
      sigrsc1  = mat_param%uparam(57)           !         pm(176) 
      sigrsc2  = mat_param%uparam(58)           !         pm(180) 
      sigrst12 = mat_param%uparam(59)           !         pm(184)                                   
      sigy0_t1 = mat_param%uparam(60)           !         pm(141) 
      sigy0_t2 = mat_param%uparam(61)           !         pm(146) 
      sigy0_c1 = mat_param%uparam(62)           !         pm(151) 
      sigy0_c2 = mat_param%uparam(63)           !         pm(156) 
      sigy0_t12= mat_param%uparam(64)           !         pm(161) 
      sigmxt1  = mat_param%uparam(65)           !         pm(144) 
      sigmxt2  = mat_param%uparam(66)           !         pm(149) 
      sigmxc1  = mat_param%uparam(67)           !         pm(154) 
      sigmxc2  = mat_param%uparam(68)           !         pm(159) 
      sigmxt12 = mat_param%uparam(69)           !         pm(164) 

      wplamxt1(1:nel) = mat_param%uparam(70)   !      pm(169) 
      wplamxt2(1:nel) = mat_param%uparam(71)   !      pm(173) 
      wplamxc1(1:nel) = mat_param%uparam(72)   !      pm(177) 
      wplamxc2(1:nel) = mat_param%uparam(73)   !      pm(181) 
      wplamxt12(1:nel)= mat_param%uparam(74)   !      pm(185) 

!      do i=1,nel
!        cb(i)   = zero  ! pm(46)
!        cn(i)   = one   ! pm(47)
!        fmax(i) = one   ! pm(49)
!        fyld(i) = one
!      enddo
!-----------------------------------------------
!     element variable initialization
!-----------------------------------------------

      etse(1:nel)  = one
      isoft(1:nel) = 0
!
!-------------------------------------------------------------------
!     old failure
!-------------------------------------------------------------------
      do i=1,nel
        fail_old(i)=0
        if (damt(i,1)>=dmax)fail_old(i) = fail_old(i) + 1
        if (damt(i,2)>=dmax)fail_old(i) = fail_old(i) + 2
        if (wpla(i)< zero) then
!         wpla is negative in case of layer already reached failure-p :
          fail_old(i) = fail_old(i) + 4
          wpla(i)     = -wpla(i)
        end if
        if (crak(i,1) >= epsf1-epst1) then
          fail_old(i) = fail_old(i) + 8
        end if
        if (crak(i,2) >= epsf2-epst2) then
          fail_old(i) = fail_old(i) + 16
        end if
!       if (wpla(i) >= wplamx(i)) fail_old(i) = fail_old(i) + 4
      enddo
!
      nindx = 0
      do i=1,nel
        if (fail_old(i)<4 .or. (fail_old(i)<8 .and. ioff<0)) then
          nindx=nindx+1
          index(nindx)=i
        end if
      enddo
!-------------------------------------------------------------------
!     stress reduction using wpla_old >= wplamx
!-----------------------------
      do i=1,nel
        if (fail_old(i) >= 4 .and. ioff >= 0 .or. fail_old(i) >= 8) then
         sig(i,1) = four_over_5*sig(i,1)
         sig(i,2) = four_over_5*sig(i,2)
         sig(i,3) = four_over_5*sig(i,3)
         sig(i,4) = four_over_5*sig(i,4)
         sig(i,5) = four_over_5*sig(i,5)
!
         if (abs(sig(i,1)) < em20) sig(i,1) = zero
         if (abs(sig(i,2)) < em20) sig(i,2) = zero
         if (abs(sig(i,3)) < em20) sig(i,3) = zero
         if (abs(sig(i,4)) < em20) sig(i,4) = zero
         if (abs(sig(i,5)) < em20) sig(i,5) = zero
         eps(i,1) = zero
         eps(i,2) = zero
         eps(i,3) = zero
         eps(i,4) = zero
         eps(i,5) = zero
!
        endif
      enddo
!-------------------------------------------------------------------
!     Ply Xfem
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
            if (abs(sigply(i,1)) < em20) sigply(i,1)=zero
            if (abs(sigply(i,2)) < em20) sigply(i,2)=zero
            if (abs(sigply(i,3)) < em20) sigply(i,3)=zero
            epsply(i,1)=zero
            epsply(i,2)=zero
            epsply(i,3)=zero
          end if
        enddo
      end if
!-------------------------------------------------------------------
!     rotation into orthotropic directions
!-------------------------------------------------------------------
      do i=1,nel
        eps(i,3) = half*eps(i,3)
        eps(i,4) = half*eps(i,4)
        eps(i,5) = half*eps(i,5)
      enddo
!
      call rotov(1,nel,eps,dir,nel)
!
      do i=1,nel
        eps(i,3) = two*eps(i,3)
        eps(i,4) = two*eps(i,4)
        eps(i,5) = two*eps(i,5)
      enddo
!     Total strains in orthotropic directions
      do i=1,nel
        strp1(i) = dir(i,1)*dir(i,1)*strn1(i)         &
                 + dir(i,2)*dir(i,2)*strn2(i)         &
                 + two*dir(i,1)*dir(i,2)*strn3(i)      
        strp2(i) = dir(i,2)*dir(i,2)*strn1(i)         &
                 + dir(i,1)*dir(i,1)*strn2(i)         &
                 - two*dir(i,2)*dir(i,1)*strn3(i)
      enddo
!-------------------------------------------------------------------
!     Elastic deformations
!-------------------------------------------------------------------
      if (ishplyxfem /= 0 .and. iplyxfem==2) then 
        do i=1,nel
          de1(i) =one-max(zero ,sign(damt(i,1),sig(i,1)) )
          de2(i) =one-max(zero ,sign(damt(i,2),sig(i,2)) )
          scale  =(half +sign(half,de1(i)-one))*(half+sign(half,de2(i)-one))
          s1(i)  = sig(i,1)/de1(i)-nu12*sig(i,2)*scale
          s2(i)  = sig(i,2)/de2(i)-nu21*sig(i,1)*scale
          s1(i)  = s1(i)/e11
          s2(i)  = s2(i)/e22
          s3(i)  = sig(i,3)/de1(i)/de2(i)/g12
          s4(i)  = sig(i,4)/max(de2(i)*g23*shf(i),em20)
          s5(i)  = sig(i,5)/max(de1(i)*g31*shf(i),em20)
          ! relatif displacement
          eply(i,1) = sigply(i,1)/de1(i)-nu12*sigply(i,2)*scale
          eply(i,2) = sigply(i,2)/de2(i)-nu21*sigply(i,1)*scale
          eply(i,1) = eply(i,1)/e11
          eply(i,2) = eply(i,2)/e22
          eply(i,3) = sigply(i,3)/de1(i)/de2(i)/g12         
        enddo      
!
        do i=1,nel
          s1(i)=s1(i)+eps(i,1)
          s2(i)=s2(i)+eps(i,2)
          s3(i)=s3(i)+eps(i,3)
          s4(i)=s4(i)+eps(i,4)
          s5(i)=s5(i)+eps(i,5)
          ! relatif displacement
          eply(i,1) = eply(i,1)+epsply(i,1)
          eply(i,2) = eply(i,2)+epsply(i,2)
          eply(i,3) = eply(i,3)+epsply(i,3)   
        enddo  
!
      else   ! without ply xfem
!
        do i=1,nel
          de1(i) = one-max( zero , sign(damt(i,1),sig(i,1)) )
          de2(i) = one-max( zero , sign(damt(i,2),sig(i,2)) )
          scale  = (half +sign(half,de1(i)-one))*(half+sign(half,de2(i)-one))
          s1(i) = sig(i,1)/de1(i)-nu12*sig(i,2)*scale
          s2(i) = sig(i,2)/de2(i)-nu21*sig(i,1)*scale
          s1(i) = s1(i)/e11
          s2(i) = s2(i)/e22
          s3(i) = sig(i,3)/de1(i)/de2(i)/g12
          s4(i) = sig(i,4)/max(de2(i)*g23*shf(i),em20)
          s5(i) = sig(i,5)/max(de1(i)*g31*shf(i),em20)    
        enddo
!
        do i=1,nel
          s1(i) = s1(i) + eps(i,1)
          s2(i) = s2(i) + eps(i,2)
          s3(i) = s3(i) + eps(i,3)
          s4(i) = s4(i) + eps(i,4)
          s5(i) = s5(i) + eps(i,5)
        enddo
!  
      endif ! iplxfem       
!   
      if (ishplyxfem /= 0 .and. iplyxfem==2) then 
#include "vectorize.inc"
        do j=1,nindx
          i=index(j)
          if (damt(i,1)/=zero) then
           crak(i,1)= crak(i,1) + eps(i,1)+ epsply(i,1)
           dam1 = crak(i,1)/(epsm1-epst1)
           dam2 = dam1*epsm1/(crak(i,1)+epst1)
           damt(i,1)= max(damt(i,1),dam2)
           damt(i,1)= min(damt(i,1),dmax)      
          endif
        enddo
!
#include "vectorize.inc"
        do j=1,nindx
          i=index(j)
          if (damt(i,2)/=zero) then
           crak(i,2) = crak(i,2) + eps(i,2) + epsply(i,2)
           dam1 = crak(i,2)/(epsm2-epst2)
           dam2 = dam1*epsm2/(crak(i,2)+epst2)
           damt(i,2)= max(damt(i,2),dam2)
           damt(i,2)= min(damt(i,2),dmax)
          endif
        enddo 
!
      else   ! without ply xfem
!
#include "vectorize.inc"
        do j=1,nindx
          i=index(j)
          if (damt(i,1) /= zero) then
           crak(i,1) = crak(i,1) + eps(i,1) 
           dam1 = crak(i,1)/(epsm1-epst1)
           dam2 = dam1*epsm1/(crak(i,1)+epst1)
           damt(i,1) = max(damt(i,1),dam2)
           damt(i,1) = min(damt(i,1),dmax)      
          endif
        enddo
!
        do j=1,nindx
          i=index(j)
          if (damt(i,2) > zero) then
           crak(i,2) = crak(i,2) + eps(i,2)
           dam1 = crak(i,2)/(epsm2-epst2)
           dam2 = dam1*epsm2/(crak(i,2)+epst2)
           damt(i,2) = max(damt(i,2),dam2)
           damt(i,2) = min(damt(i,2),dmax)
          endif
        enddo       
      endif  ! ply xfem
!
!-----------------------------
      do i=1,nel
        de1(i) = one - max(zero ,sign(damt(i,1),sig(i,1)) )
        de2(i) = one - max(zero ,sign(damt(i,2),sig(i,2)) )
        scale1 = (half + sign(half,de1(i)-one))*(half+sign(half,de2(i)-one))
        scale2 = one - nu12*nu21*scale1
        a11(i) = e11*de1(i)/scale2
        a22(i) = e22*de2(i)/scale2
        a12(i) = nu21*a11(i)*scale1
      enddo
!-----------------------------
!     elastic stress
!-----------------------------
      do i=1,nel
        t1(i)    = a11(i)*s1(i)+a12(i)*s2(i)
        t2(i)    = a12(i)*s1(i)+a22(i)*s2(i)
        t3(i)    = de1(i)*de2(i)*g12*s3(i)
        sig(i,4) = de2(i)*g23*shf(i)*s4(i)
        sig(i,5) = de1(i)*g31*shf(i)*s5(i)
      enddo
!
      if (ishplyxfem /= 0 .and. iplyxfem==2) then
       do i=1,nel
         sigply(i,1) = a11(i)*eply(i,1)+a12(i)*eply(i,2)
         sigply(i,2) = a12(i)*eply(i,1)+a22(i)*eply(i,2)
         sigply(i,3) = de1(i)*de2(i)*g12*eply(i,3)
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
      if (israte == 0) then 
        do i=1,nel
          epsp(i) = max(                                       &
          abs(eps(i,1)),abs(eps(i,2)),abs(eps(i,3)),           &
          abs(eps(i,4)),abs(eps(i,5))) / max(timestep,em20)
        enddo
      endif
      do i=1,nel
        if (epsp(i) > epdr) then
          epsp(i) = log(epsp(i)/epdr)
        else
          epsp(i) = zero
        endif
        coef(i)   = zero
      enddo
!-------------------------------------------------------------------
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
         sigyt1 = sigyt1*(one  + cc_t1 *epsp(i))
         sigyt2 = sigyt2*(one  + cc_t2 *epsp(i))
         sigyc1 = sigyc1*(one  + cc_c1 *epsp(i))
         sigyc2 = sigyc2*(one  + cc_c2 *epsp(i))
         sigyt12= sigyt12*(one + cc_t12*epsp(i))
       else
         sigyt1 = sigyt1*(one  + cc_t1 *epsp(i))
         sigyt2 = sigyt2*(one  + cc_t2 *epsp(i))
         sigyc1 = sigyc1*(one  + cc_c1 *epsp(i))
         sigyc2 = sigyc2*(one  + cc_c2 *epsp(i))
         sigyt12= sigyt12*(one + cc_t12*epsp(i))
         sigyt1 = min(sigyt1 ,sigmxt1 )
         sigyt2 = min(sigyt2 ,sigmxt2 )
         sigyc1 = min(sigyc1 ,sigmxc1 )
         sigyc2 = min(sigyc2 ,sigmxc2 )
         sigyt12= min(sigyt12,sigmxt12)
       endif
!-----------------------------------------------------------------
!      softening 
!-----------------------------------------------------------------
       soft(1)  = zero
       soft(2)  = zero
       soft(3)  = zero
       isoft(i) = 0 
       ! direction 1
       if (strp1(i) <= -eps1c1) then
         soft(1) = min(one,(strp1(i)+eps1c1)/(eps1c1-eps2c1))
       else if (strp1(i) >= eps1t1) then
         soft(1) = min(one,(strp1(i)-eps1t1)/(eps2t1-eps1t1))         
       endif
       sigyt1=min(sigyt1,(one -soft(1))*sigyt1+soft(1)*sigrst1)
       sigyc1=min(sigyc1,(one -soft(1))*sigyc1+soft(1)*sigrsc1)
       ! direction 2        
       if (strp2(i) <= -eps1c2) then
         soft(2) = min(one,(strp2(i)+eps1c2)/(eps1c2-eps2c2))
       else if (strp2(i) >= eps1t2) then
         soft(2) = min(one,(strp2(i)-eps1t2)/(eps2t2-eps1t2))
       endif
       sigyt2 = min(sigyt2,(one -soft(2))*sigyt2+soft(2)*sigrst2)
       sigyc2 = min(sigyc2,(one -soft(2))*sigyc2+soft(2)*sigrsc2)
       ! direction 12
       strp12=-dir(i,1)*dir(i,2)*strn1(i)                       &
             +dir(i,2)*dir(i,1)*strn2(i)                        &
             +(dir(i,1)*dir(i,1)-dir(i,2)*dir(i,2))*strn3(i)
       if (strp12 <= -eps1t12) then
         soft(3) = min(one,(strp12+eps1t12)/(eps1t12-eps2t12))
       else if (strp12 >= eps1t12) then
         soft(3) = min(one,(strp12-eps1t12)/(eps2t12-eps1t12))
       endif
       sigyt12 = min(sigyt12,(one - soft(3))*sigyt12 + soft(3)*sigrst12)
       if (soft(1) + soft(2) + soft(3) > zero) isoft(i) = 1
!
       f1(i)  = one/sigyt1-one/sigyc1
       f2(i)  = one/sigyt2-one/sigyc2
       f11(i) = one/(sigyt1*sigyc1)
       f22(i) = one/(sigyt2*sigyc2)
       f33(i) = one/(sigyt12*sigyt12)   
       alpha  = f12(i)
       f12(i) = -alpha/(two*sqrt(sigyt1*sigyc1*sigyt2*sigyc2))
       sigy(i)= min(sigyt1,sigyc1,sigyt2,sigyc2,sigyt12)
!
       epsp(i) = one + cc * epsp(i)
       if (icc==3 .or. icc==4) then
         wplamx(i)    = wplamx(i)   * epsp(i)
         wplamxt1(i)  = wplamxt1(i) * epsp(i)
         wplamxt2(i)  = wplamxt2(i) * epsp(i)
         wplamxc1(i)  = wplamxc1(i) * epsp(i)
         wplamxc2(i)  = wplamxc2(i) * epsp(i)
         wplamxt12(i) = wplamxt12(i)* epsp(i)
       endif
      enddo
!-------------------------------------------------------------------
!     plasticity
!-------------------------------------------------------------------
      do i=1,nel
        wvec(i) = f1(i) *t1(i)       + f2(i) *t2(i)             &
                + f11(i)*t1(i)*t1(i) + f22(i)*t2(i)*t2(i)       &
                + f33(i)*t3(i)*t3(i) + two*f12(i)*t1(i)*t2(i)
        tsaiwu(i) = max(min(wvec(i),one),tsaiwu(i))
      enddo
!
      do i=1,nel
        if (wvec(i) > one .and. off(i)==one) coef(i)=one    
        wvec(i)=zero
      enddo
!
      do i=1,nel                                                       
        beta(i) = one
        if (coef(i) == one) then
          coefa = f11(i)*sig(i,1)*sig(i,1) + f22(i)*sig(i,2)*sig(i,2)       &    
                + f33(i)*sig(i,3)*sig(i,3) + two*f12(i)*sig(i,1)*sig(i,2)                                     
          coefb = f1(i)*sig(i,1) + f2(i)*sig(i,2)                                  
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
              delta = (f1(i) * sig(i,1) + f2(i) * sig(i,2)            &     
                    + f11(i) * beta(i)*sig(i,1)*sig(i,1)              &    
                    + f22(i) * beta(i)*sig(i,2)*sig(i,2)              &    
                    + f33(i) * beta(i)*sig(i,3)*sig(i,3)              &    
                    + two*f12(i)*beta(i)*sig(i,1)*sig(i,2)) * beta(i)           

            else if (imconv==1 .and. outv(i) == 0) then  ! coefa<em15 & coefb<em15
!$OMP CRITICAL                                                        
              call ancmsg(msgid=244,anmode=aninfo, i1=ngl(i),i2=ilayer)
              outv(i) = 1
!$OMP END CRITICAL                                                       
            endif                                                           

          else if (imconv == 1 .and. outv(i) == 0) then   ! delta < 0
!$OMP CRITICAL                                                        
            call ancmsg(msgid=244,anmode=aninfo, i1=ngl(i),i2=ilayer)
            outv(i) = 1
!$OMP END CRITICAL                                                       
          endif   ! delta > 0
        end if    ! coef == 1
      enddo
!-----------------------------------------------------------------------      
      do i=1,nel
        so1(i) = beta(i)*sig(i,1)
        so2(i) = beta(i)*sig(i,2)
        so3(i) = beta(i)*sig(i,3)
      enddo
!
      do i=1,nel
        dp1(i) = f1(i)+two*f11(i)*so1(i)+two*f12(i)*so2(i)
        dp2(i) = f2(i)+two*f22(i)*so2(i)+two*f12(i)*so1(i)
        dp3(i) = two*f33(i)*so3(i)
      enddo
!-----------------------------------------------------------------------      
      do i=1,nel
        ds1(i) = t1(i) - so1(i)
        ds2(i) = t2(i) - so2(i)
        ds3(i) = t3(i) - so3(i)
      enddo
! 
      do i=1,nel
        lamda(i)=(dp1(i)*ds1(i)+dp2(i)*ds2(i)+dp3(i)*ds3(i))*coef(i)
        if (lamda(i) /= zero) then
          lamda(i) = lamda(i)*coef(i)                              &
                   / (dp1(i)*(a11(i)*dp1(i)+a12(i)*dp2(i))         &
                   + dp2(i)*(a12(i)*dp1(i)+a22(i)*dp2(i))          &
                   + two*dp3(i)*g12*de1(i)*de2(i)*dp3(i) )
        end if
      enddo
!
      do i=1,nel
        dp1(i) = lamda(i)*dp1(i)
        dp2(i) = lamda(i)*dp2(i)
        dp3(i) = lamda(i)*dp3(i)
      enddo
!               
      do i=1,nel  
        t1(i) = t1(i) - a11(i)*dp1(i) - a12(i)*dp2(i)
        t2(i) = t2(i) - a12(i)*dp1(i) - a22(i)*dp2(i)
        t3(i) = t3(i) - g12*de1(i)*de2(i)*dp3(i)*two
      enddo
!
      do i=1,nel                  
        dwpla = (dp1(i)*(t1(i)+so1(i)) + dp2(i)*(t2(i)+so2(i))   &
              + two*dp3(i)*(t3(i)+so3(i))) * half
        dwpla   = max(dwpla ,zero) / wplaref
        wpla(i) = wpla(i) + dwpla        
        icas(i) = 0   ! flag for output 
      enddo
!
      do i=1,nel  
        sigyt1 = sigy0_t1 
        sigyt2 = sigy0_t2 
        sigyc1 = sigy0_c1 
        sigyc2 = sigy0_c2 
        sigyt12= sigy0_t12
!                   
        if (imodwp == 0 .or. (imodwp > 0 .and. isoft(i) == 0)) then
          ! direction 1       
          wpla1 = ep20
          if (t1(i) >= sigyt1 ) then
            wpla1 = wplamxt1(i)
            id = 1
          else if (t1(i) <= -sigyc1) then
            wpla1 = wplamxc1(i)
            id =-1
          endif 
!                       
          if (wpla1 < wplamx(i)) then
            wplamx(i) = wpla1
            icas(i) = id
          endif
          ! direction 2             
          wpla2 = ep20
          if (t2(i) >= sigyt2  ) then
            wpla2 = wplamxt2(i)
            id =  2
          else if (t2(i) <= -sigyc2) then
            wpla2 = wplamxc2(i)
            id = -2
          endif
          if (wpla2 < wplamx(i) ) then
            wplamx(i) = wpla2
            icas(i) = id
          endif
          ! shear 
          wpla3 = ep20
          if (abs(t3(i)) >= sigyt12) then
            wpla3 = wplamxt12(i)
            id = 3
          endif          
          if (wpla3 < wplamx(i)) then               
            wplamx(i) = wpla3
            icas(i) = id
          endif
        else if (imodwp > 0) then  
          id = 1
          icas(i) = nint(sign(one,t1(i)))
          if (icas(i) > 0 ) then
            norm1 = ((t1(i))/sigyt1)
          else 
            norm1 = ((abs(t1(i))/sigyc1))
          endif
          icas(i) = nint(sign(one,t2(i)))
          if (icas(i) > 0 ) then
            norm2 = ((t2(i))/sigyt2)
          else 
            norm2 = ((abs(t2(i))/sigyc2))
          endif
          if (norm2 > norm1) then
            id = 2
          endif
          norm3 = ((abs(t3(i))/sigyt12))
            if (norm3 >= norm2) then
            id = 3
            else if (norm3 >= norm1) then
            id = 3
          endif
          select case(id)
            case(1)
             icas(i) = nint(sign(one,t1(i)) )
             if (icas(i) > 0 ) then
                wplamx(i) = min(wplamx(i),wplamxt1(i))
             else
                wplamx(i) = min(wplamx(i),wplamxc1(i))
             endif 
            case(2)
             icas(i) = nint(sign(two,t2(i)) )
             if (icas(i) > 0 ) then
               wplamx(i) = min(wplamx(i),wplamxt2(i))
             else
               wplamx(i) = min(wplamx(i),wplamxc2(i))
             endif 
            case(3)
              icas(i) = 3 
              wplamx(i) = min(wplamx(i),wplamxt12(i))
          end select 
        endif
      enddo 
!      
      do i=1,nel
        ifail0  =  mod(fail_old(i),8)
        if (wpla(i)>=wplamx(i) .or.ifail0 >= 4 )wplar(i)= wplar(i)+one
        if (wpla(i)>=wplamx(i).or.ifail0 >= 4 .or.                         &
          damt(i,1)>=dmax .or.                                             &
          crak(i,1)>=epsf1-epst1) nfis1(i)=nfis1(i)+1                
        if (wpla(i)>=wplamx(i).or.ifail0 >= 4 .or.                         &
          damt(i,2)>=dmax .or.                                             &
          crak(i,2)>=epsf2-epst2) nfis2(i)=nfis2(i)+1                
        if (wpla(i)>=wplamx(i).or.ifail0 >= 4 .or. damt(i,1)>=dmax .or.    &
            crak(i,1)>=epsf1-epst1 .or. damt(i,2)>=dmax .or.               &
            crak(i,2)>=epsf2-epst2) nfis3(i)=nfis3(i)+1 
      enddo
!
!-------------------------------------------------------------------
!     failure
!-------------------------------------------------------------------
      do i=1,nel
        fail = 0
        if (damt(i,1) >= dmax)        fail = fail + 1
        if (damt(i,2) >= dmax)        fail = fail + 2
        if (wpla(i)   >= wplamx(i))   fail = fail + 4
        if (crak(i,1) >= epsf1-epst1) fail = fail + 8
        if (crak(i,2) >= epsf2-epst2) fail = fail + 16
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
!     end of failure
!-------------------
      do i=1,nel
        sig(i,1)=t1(i)
        sig(i,2)=t2(i)
        sig(i,3)=t3(i)
      enddo      
      ! for qeph  
      do i=1,nel
        if (abs(beta(i)) < one) then
          ht = em10
          etse(i) = ht/(ht+e11)
        end if
      end do 
!-------------------------------------------------------------------
!     end of plasticity calculation
!-------------------------------------------------------------------
      do i=1,nel
        signxx(i) = sig(i,1)
        signyy(i) = sig(i,2)
        signxy(i) = sig(i,3)
        signyz(i) = sig(i,4)
        signzx(i) = sig(i,5)
      enddo
!-------------------------------------------------------------------
!     stress rotation - back to local element coordinate sysyem
!-----------------------------
      do i=1,nel
        sige(i,1) = sig(i,1)
        sige(i,2) = sig(i,2)
        sige(i,3) = sig(i,3)
        sige(i,4) = sig(i,4)
        sige(i,5) = sig(i,5)
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
          if (wvec(i) > one .and. off(i)==one) coef(i)=one    
          wvec(i)=zero
        enddo
!
        do 222 i=1,nel                                                       
          beta(i)= one                                                          
          if (coef(i)==zero) go to 222                       
           coefa=f11(i)*t1(i)*t1(i)+f22(i)*t2(i)*t2(i) +           & 
               f33(i)*t3(i)*t3(i) + two*f12(i)*t1(i)*t2(i)                                     
           coefb=f1(i)*t1(i)+f2(i)*t2(i)                                  
           delta=coefb*coefb + four*coefa                      
          if (delta<zero) then                                                
           if (imconv==1 .and. outv(i) == 0) then 
!$OMP CRITICAL                                                        
              call ancmsg(msgid=244,anmode=aninfo, i1=ngl(i),i2=ilayer)
              outv(i) = 1
!$OMP END CRITICAL                                                       
           endif                                                               
           go to 222                                                           
          else                                                                 
            delta=sqrt(delta)                                                  
          endif                                                                
          if (abs(coefa)<=em15) then                                          
            if (abs(coefb)<=em15) then                                        
             if (imconv==1 .and. outv(i) == 0) then  
!$OMP CRITICAL                                                        
              call ancmsg(msgid=244,anmode=aninfo, i1=ngl(i),i2=ilayer)
              outv(i) = 1
!$OMP END CRITICAL                                                       
              endif                                                             
               go to 222                                                       
              else                                                             
               beta(i) = one/coefb                                           
             endif                                                              
          endif                                                                
          if (abs(one+(coefb-delta)*half/coefa)<= abs(one+(coefb+delta)*half/coefa)) then                            
           beta(i)=(-coefb+delta)*half/coefa                                   
          else                                                                 
           beta(i)=(-coefb-delta)*half/coefa                                   
          endif                                                                               

 222    continue                                                               
!-----------------------------------------------
        do i=1,nel
          so1(i) = beta(i)*t1(i)
          so2(i) = beta(i)*t2(i)
          so3(i) = beta(i)*t3(i)
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
        do  i=1,nel
          lamda(i)=(dp1(i)*ds1(i)+dp2(i)*ds2(i)+dp3(i)*ds3(i))*coef(i)
          if (lamda(i) /= zero) then 
            lamda(i)=lamda(i)*coef(i)/                               &
              (dp1(i)*(a11(i)*dp1(i)+a12(i)*dp2(i))+                 &
               dp2(i)*(a12(i)*dp1(i)+a22(i)*dp2(i))+                 &
               two*dp3(i)*g12*de1(i)*de2(i)*dp3(i))
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
          sigply(i,3)=t3(i)-g12*de1(i)*de2(i)*dp3(i)*two
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
 1001 format(' FAILURE-1 ELEMENT #',i10,', LAYER #',i3,                       &
             ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)                
 1002 format(' FAILURE-2 ELEMENT #',i10,', LAYER #',i3,                       &
             ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)                
 1003 format(' TOTAL FAILURE-2 ELEMENT #',i10,', LAYER #',i3,                 &
             ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)          
 1004 format(' TOTAL FAILURE-1 ELEMENT #',i10,', LAYER #',i3,                 &
             ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)          
 2000 format(' FAILURE-P-MAX ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,    &                   
             ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)                
 2001 format(' FAILURE-P-T1 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &                   
             ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)                 
 2002 format(' FAILURE-P-C1 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &                   
             ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)                 
 2003 format(' FAILURE-P-T2 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &                   
             ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)                 
 2004 format(' FAILURE-P-C2 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &                   
             ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)                 
 2005 format(' FAILURE-P-T12 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,    &                   
             ', INTEGRATION POINT #',i3,', (PLY #',i10,'), TIME=',1pe11.4)          
 3001 format(' FAILURE-1 ELEMENT #',i10,', LAYER #',i3,                       &
             ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)                 
 3002 format(' FAILURE-1 ELEMENT #',i10,', LAYER #',i3,                       &
             ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)                 
 3003 format(' TOTAL FAILURE-2 ELEMENT #',i10,', LAYER #',i3,                 &
             ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)                 
 3004 format(' TOTAL FAILURE-1 ELEMENT #',i10,', LAYER #',i3,                 &
             ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)                 
 4000 format(' FAILURE-P-MAX ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,    &                   
             ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)                
 4001 format(' FAILURE-P-T1 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &                  
             ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)                
 4002 format(' FAILURE-P-C1 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &                  
             ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)                
 4003 format(' FAILURE-P-T2 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &                  
             ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)                
 4004 format(' FAILURE-P-C2 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,     &                  
             ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)                
 4005 format(' FAILURE-P-T12 ELEMENT #',i10,', WPLA ',f8.2,', LAYER #',i3,    &                   
             ', INTEGRATION POINT #',i3,', TIME=',1pe11.4)                
!-----------------------------------------------------------------------
      end subroutine mat25_crasurv_c
      end module mat25_crasurv_c_mod
