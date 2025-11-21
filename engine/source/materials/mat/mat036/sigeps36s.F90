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
!===================================================================================================

!||====================================================================
!||    sigeps36s_mod   ../engine/source/materials/mat/mat036/sigeps36s.F90
!||--- called by ------------------------------------------------------
!||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8          ../engine/source/materials/mat_share/mulaw8.F90
!||====================================================================
      module sigeps36s_mod
      contains

!||====================================================================
!||    sigeps36s               ../engine/source/materials/mat/mat036/sigeps36s.F90
!||--- called by ------------------------------------------------------
!||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8                  ../engine/source/materials/mat_share/mulaw8.F90
!||--- calls      -----------------------------------------------------
!||    m36iter_imp             ../engine/source/materials/mat/mat036/m36iter_imp.F
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    file_descriptor_mod     ../engine/source/modules/file_descriptor_mod.F90
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      subroutine sigeps36s(mat_param,                                           &
                 nel     ,nuvar   ,nvartmp ,timestep,time    ,                  &
                 depsxx  ,depsyy  ,depszz  ,depsxy  ,depsyz  ,depszx ,          &
                 epsxx   ,epsyy   ,epszz   ,epsxy   ,epsyz   ,epszx  ,          &
                 sigoxx  ,sigoyy  ,sigozz  ,sigoxy  ,sigoyz  ,sigozx ,          &
                 signxx  ,signyy  ,signzz  ,signxy  ,signyz  ,signzx ,          &
                 sigbxx  ,sigbyy  ,sigbzz  ,sigbxy  ,sigbyz  ,sigbzx ,          &
                 soundsp ,uvar    ,vartmp  ,off     ,ngl     ,impl_s ,          &
                 epsd    ,ipla    ,yld     ,pla     ,inloc   ,planl  ,          &
                 dpla1   ,etse    ,al_imp  ,signor  ,amu     ,dpdm   ,          &
                 yldfac  ,dmg     ,l_sigb  ,l_dmg   ,l_planl )
! ------------------------------------------------------------------------------
!       modules
! ------------------------------------------------------------------------------
          use matparam_def_mod
          use table_mat_vinterp_mod
          use precision_mod, only : wp
          use constant_mod
          use file_descriptor_mod
!-----------------------------------------------
!      i m p l i c i t   t y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!      g l o b a l   p a r a m e t e r s
!-----------------------------------------------
#include "mvsiz_p.inc"
!-----------------------------------------------
!       i n p u t   a r g u m e n t s
!-----------------------------------------------
        integer, intent(in) :: nel
        integer, intent(in) :: nuvar
        integer, intent(in) :: nvartmp
        integer, intent(in) :: ipla
        integer, intent(in) :: inloc
        integer, intent(in) :: impl_s
        integer ,intent(in) :: l_sigb
        integer ,intent(in) :: l_dmg
        integer ,intent(in) :: l_planl
        integer ,dimension(nel), intent(in) :: ngl
        integer ,dimension(nel,nvartmp)      ,intent(inout) :: vartmp
        real(kind=wp)                        ,intent(in)    :: timestep,time
        type(matparam_struct_)               ,intent(in)    :: mat_param
        real(kind=wp), dimension(mvsiz)      ,intent(in)    :: dpdm
        real(kind=wp) ,dimension(nel)        ,intent(in)    :: epsxx,epsyy,epszz,epsxy,epsyz,epszx
        real(kind=wp) ,dimension(nel)        ,intent(in)    :: depsxx,depsyy,depszz,depsxy,depsyz,depszx
        real(kind=wp) ,dimension(nel)        ,intent(inout) :: sigoxx,sigoyy,sigozz,sigoxy,sigoyz,sigozx
        real(kind=wp) ,dimension(nel)        ,intent(inout) :: epsd,etse,amu,yldfac
        real(kind=wp) ,dimension(nel)        ,intent(inout) :: signxx,signyy,signzz, signxy,signyz,signzx
        real(kind=wp), dimension(nel*l_sigb) ,intent(inout) :: sigbxx,sigbyy,sigbzz,sigbxy,sigbyz,sigbzx
        real(kind=wp) ,dimension(nel)        ,intent(inout) :: soundsp,dpla1,al_imp
        real(kind=wp) ,dimension(nel)        ,intent(inout) :: off,yld,pla
        real(kind=wp) ,dimension(mvsiz,6)    ,intent(inout) :: signor
        real(kind=wp) ,dimension(nel*l_dmg)  ,intent(inout) :: dmg
        real(kind=wp) ,dimension(nel*l_planl),intent(inout) :: planl
        real(kind=wp) ,dimension(nel,nuvar)  ,intent(inout) :: uvar
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
        integer :: i,ii,iter,vp,ndim,ifail
        integer :: ismooth,yldcheck,func_e,func_p
        integer :: nindx
        integer ,dimension(nel) :: indx
        real(kind=wp) :: nu,young,shear,kk,rho0,dav,vm,vm_1,fac,epst,p,epsp1,epsp2
        real(kind=wp) :: e1,e2,e3,e4,e5,e6,c,cc,d,y,yp,e42,e52,e62,epst2            
        real(kind=wp) :: epsr1,epsf,hkin,fisokin                                     
        real(kind=wp) :: dsxx,dsyy,dszz,dsxy,dsyz                                     
        real(kind=wp) :: dszx,sigpxx,sigpyy,sigpxy,sigpyz,sigpzx,sigpzz,alpha         
        real(kind=wp) :: epsr1dav,epsr1f,g3h,norm_1,epsmax,ssp,svm2                   
        real(kind=wp) :: einf,ce,epsr2,df,f                                           
        real(kind=wp) ,dimension(nel)   :: r,yfac
        real(kind=wp) ,dimension(mvsiz) :: bulk,g,g2,g3,y1,y2,h,dydx
        real(kind=wp) ,dimension(mvsiz) :: fail,e,p0,pfac,rfac,triax,pscale1,yld_ini,yld0
        real(kind=wp) ,dimension(mvsiz) :: coef,dfsr,epstt                
        real(kind=wp) ,dimension(mvsiz) :: sigexx,sigeyy,sigezz,sigexy,sigeyz,sigezx
        real(kind=wp) ,dimension(mvsiz) :: plaold,plap,svm,dpla_j,dpla,hi,escale
        real(kind=wp) ,dimension(nel,1) :: xvec1
        real(kind=wp) ,dimension(nel,2) :: xvec2
        integer, parameter :: niter=5
!=========================================================================================
        rho0     = mat_param%rho0
        young    = mat_param%young
        shear    = mat_param%shear
        kk       = mat_param%bulk
        nu       = mat_param%nu
!
        ismooth  = mat_param%iparam(1) ! strain rate interpolation flag (linear/log)
        ifail    = mat_param%iparam(2) ! flag for failure
        vp       = mat_param%iparam(3) ! flag for plastic strain dependency
        yldcheck = mat_param%iparam(4)
      
        epsmax   = mat_param%uparam(3)
        epsr1    = mat_param%uparam(4)
        epsr2    = mat_param%uparam(5)
        epsf     = mat_param%uparam(6)   
        fisokin  = mat_param%uparam(7) 
        ce       = mat_param%uparam(8) 
        einf     = mat_param%uparam(9)  
!        asrate   = mat_param%uparam(10) 
!        asrate   = min(one, asrate*timestep)

        ndim   = mat_param%table(1)%ndim
        func_e = mat_param%table(3)%notable
        func_p = mat_param%table(2)%notable
!
        ssp = sqrt((kk + four_over_3*shear) / rho0)
!------------------------------------------
!     elastic parameters
!------------------------------------------
        nindx=0
        do i=1,nel         
          if (pla(i) > zero .and. off(i) == one) then
            nindx=nindx+1
            indx(nindx)=i
          endif
        enddo
!
        if (func_e > 0) then           ! Young modulus function of plastic strain
          xvec1(1:nel,1) = pla(1:nel)
          call table_mat_vinterp(mat_param%table(3),nel,nel,vartmp(1,4),xvec1,escale,dydx)
          e(1:nel) = escale(1:nel)* young                                       
          g(1:nel) = half * e(1:nel)/(one+nu)                                   
        elseif (ce /= zero) then       ! variable Young modulus defined analytically                                               
          do ii=1,nindx
            i = indx(ii)
            e(i) = young - (young-einf)*(one-exp(-ce*pla(i)))
            g(i) = half * e(i)/(one+nu)                                   
          end do
        else                           ! constant Young modulus
          e(1:nel)  = young
          g(1:nel) = shear
        end if
        g2(1:nel) = g(1:nel) * two 
        g3(1:nel) = g(1:nel) * three
        bulk(1:nel) = e(1:nel) / (three*(one - two*nu))            
        soundsp(1:nel) = sqrt((bulk(1:nel) + four_over_3*g(1:nel))/rho0)
        fail(1:nel) = one
!------------------------------------------
!         kinematic hardening 
!------------------------------------------
        if (fisokin > zero) then
          !  subtract back stresses for kinmatic or mixed hardening from the old stresses
          !  => sigoxx is a shifted old stress
#include "vectorize.inc"
          do i=1,nel
            sigoxx(i) = sigoxx(i) - sigbxx(i)
            sigoyy(i) = sigoyy(i) - sigbyy(i)
            sigozz(i) = sigozz(i) - sigbzz(i)
            sigoxy(i) = sigoxy(i) - sigbxy(i)
            sigoyz(i) = sigoyz(i) - sigbyz(i)
            sigozx(i) = sigozx(i) - sigbzx(i)       
          enddo            
        endif 
!-------------------
!         damage factor based on principal strain (max 4 newton iterations)
!-------------------
        if (ifail > 1) then
          epsr1f = min(epsr1,epsf)
          do i=1,nel
            dav = (epsxx(i)+epsyy(i)+epszz(i)) * third
            e1 = epsxx(i) - dav
            e2 = epsyy(i) - dav
            e3 = epszz(i) - dav
            e4 = half*epsxy(i)
            e5 = half*epsyz(i)
            e6 = half*epszx(i)
!            -y = (e1-x)(e2-x)(e3-x) - e5^2(e1-x) - e6^2(e2-x) - e4^2(e3-x) + 2e4 e5 e6
!             e1 + e2 + e3 = 0 => terme en x^2 = 0
!             y = x^3 + c x + d
!             yp= 3 x^2 + c
            e42 = e4*e4  
            e52 = e5*e5
            e62 = e6*e6
            c = - half *(e1*e1 + e2*e2 + e3*e3) - e42 - e52 - e62
            epst = sqrt(-c*third)
!           2*epst is an upper bound of the solution
            epsr1dav = epsr1f - dav
            if (epst+epst < epsr1dav) cycle
            d = - e1*e2*e3 + e1*e52 + e2*e62 + e3*e42 - two*e4*e5*e6
            epst2 = epst * epst
            y = (epst2 + c)* epst + d
            if (abs(y) > em8) then
              epst = onep75 * epst
              epst2 = epst * epst
              y = (epst2 + c)* epst + d
              yp = three*epst2 + c
              epst = epst - y/yp
!             epst is an upper bound of the solution
              if (epst < epsr1dav) cycle
              epst2 = epst * epst
              y = (epst2 + c)* epst + d
              yp = three*epst2 + c
              epst = epst - y/yp
              if (epst < epsr1dav) cycle
              epst2 = epst * epst
              y = (epst2 + c)* epst + d
              yp = three*epst2 + c
              epst = epst - y/yp
              if (epst < epsr1dav) cycle
              epst2 = epst * epst
              y = (epst2 + c)* epst + d
              yp = three*epst2 + c
              epst = epst - y/yp
            endif
!
            epst = epst + dav
            epstt(i)= epst
            fail(i) = max(em20, min(one, (epsr2-epst)/(epsr2-epsr1) ))
            dmg(i)  = one - max(zero, min(one,(epsr2-epst)/(epsr2-epsr1)))
          enddo
        endif
!------------------------------------------
!       elastic trial stress tensor
!------------------------------------------
#include "vectorize.inc"
        do i=1,nel
          dav   =  (depsxx(i)+depsyy(i)+depszz(i))*third   ! strain increment axiator
          p0(i) = -(sigoxx(i)+sigoyy(i)+sigozz(i))*third
          signxx(i) = sigoxx(i) + p0(i) + g2(i)*(depsxx(i)-dav)
          signyy(i) = sigoyy(i) + p0(i) + g2(i)*(depsyy(i)-dav)
          signzz(i) = sigozz(i) + p0(i) + g2(i)*(depszz(i)-dav)
        enddo
!
        signxy(1:nel) = sigoxy(1:nel) + g(1:nel) * depsxy(1:nel)
        signyz(1:nel) = sigoyz(1:nel) + g(1:nel) * depsyz(1:nel)
        signzx(1:nel) = sigozx(1:nel) + g(1:nel) * depszx(1:nel)
        sigexx(1:nel) = signxx(1:nel)
        sigeyy(1:nel) = signyy(1:nel)
        sigezz(1:nel) = signzz(1:nel)               
        sigexy(1:nel) = signxy(1:nel)
        sigeyz(1:nel) = signyz(1:nel)
        sigezx(1:nel) = signzx(1:nel)
        dpla1(1:nel)  = zero
        epstt(1:nel)  = zero
        do i=1,nel
          svm2     = three*(half*(signxx(i)**2+signyy(i)**2+signzz(i)**2)     &
                   + signxy(i)**2+signyz(i)**2+signzx(i)**2)
          svm(i)   = sqrt(svm2)                
          triax(i) = -p0(i) / max(svm(i),em20)
        enddo
!-----------------------------------------------
!         pressure dependent yield function factor
!-----------------------------------------------
        if (func_p > 0) then 
          xvec1(1:nel,1) = triax(1:nel)
          call table_mat_vinterp(mat_param%table(2),nel,nel,vartmp(1,3),xvec1,pfac,dydx)
          pfac(1:nel) = max(zero, pfac(1:nel))
        else
          pfac(1:nel) = one                                
        endif
        yfac(1:nel) = fail(1:nel) * pfac(1:nel) * yldfac(1:nel)
!=======================================================================
!     split code to 2 independent parts :
!           vp = 1 dependent on plastic strain rate
!           vp = 0 dependent on total strain rate 
!=======================================================================
        if (vp == 0) then
!=======================================================================
            ! calculate yield stress
!-----------------------------------------------
          if (ndim == 1) then   ! only static curve => no strain rate interpolation
            xvec1(1:nel,1) = pla(1:nel)
            call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec1,y1,dydx)
            h(1:nel) = dydx(1:nel) * yfac(1:nel)
            if (fisokin == zero) then
              yld(1:nel) = y1(1:nel) * yfac(1:nel)
            else
              yld0(1:nel) = mat_param%table(1)%y1d(1)  ! initial static yield value
              yld(1:nel)  = ((one-fisokin)*y1(1:nel) + fisokin *yld0(1:nel))*yfac(1:nel)
            end if
!   
          else  ! ndim = 2 => yield depending on strain rate
!
            xvec2(1:nel,1) = pla(1:nel)
            if (ismooth == 2) then ! log interpolation
              xvec2(1:nel,2) = log(epsd(1:nel))
            else
              xvec2(1:nel,2) = epsd(1:nel)
            end if
!
            call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,y1,dydx)
!
            h(1:nel) = dydx(1:nel) * yfac(1:nel)
!
            if (fisokin == zero) then  ! isotropic hardening
              yld(1:nel) = y1(1:nel) * yfac(1:nel)
            else                       ! kinematic or mixed hardening
              xvec2(1:nel,1) = zero
              xvec2(1:nel,2) = epsd(1:nel)
              call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld0,dydx)
              do i=1,nel          
                yld(i) = ((one - fisokin) * yld(i) + fisokin * yld0(i)) * yfac(i)
              enddo
            end if
          end if
!
          if (yldcheck == 1) then
            yld(1:nel) = max(yld(1:nel), em20)
          end if
!-------------------
!         projection
!-------------------
          if (ipla == 0) then
            do i=1,nel
              if (svm(i) > yld(i)) then
                r(i) = yld(i)/ max(svm(i),em20)
                signxx(i)=signxx(i)*r(i)
                signyy(i)=signyy(i)*r(i)
                signzz(i)=signzz(i)*r(i)
                signxy(i)=signxy(i)*r(i)
                signyz(i)=signyz(i)*r(i)
                signzx(i)=signzx(i)*r(i)
                pla(i) = pla(i) + (one - r(i))*svm(i)/max(g3(i)+h(i),em20)
                dpla1(i) = (one - r(i))*svm(i)/max(g3(i)+h(i),em20)         
              endif
            enddo
!
          else if (ipla == 2) then
!
            do i=1,nel
             if (svm(i) > yld(i)) then
               r(i) = yld(i)/ max(svm(i),em20)
               signxx(i)=signxx(i)*r(i)
               signyy(i)=signyy(i)*r(i)
               signzz(i)=signzz(i)*r(i)
               signxy(i)=signxy(i)*r(i)
               signyz(i)=signyz(i)*r(i)
               signzx(i)=signzx(i)*r(i)
               pla(i)=pla(i) + (one-r(i))*svm(i)/max(g3(i),em20)
               dpla1(i) =  (one-r(i))*svm(i)/max(g3(i),em20)     
             endif 
           enddo
!
          else if (ipla == 1) then
!
            if (impl_s == 0) then    ! => explicit
              do i=1,nel
                if (svm(i) > yld(i)) then
                  r(i)    = yld(i) / max(svm(i),em20)
                  dpla(i) = (one - r(i)) * svm(i)/max(g3(i)+h(i),em20)       ! plastic strain increment
                  yld(i)  = max(yld(i) + (one - fisokin)*dpla(i)*h(i),zero)  ! updated yield stress
                  r(i)    = min(one,yld(i) / max(svm(i),em20))
!                     
                  ! here the updated stresses are shifted stresses for kinematic or mixed hardening
                  signxx(i)=signxx(i)*r(i)
                  signyy(i)=signyy(i)*r(i)
                  signzz(i)=signzz(i)*r(i)
                  signxy(i)=signxy(i)*r(i)
                  signyz(i)=signyz(i)*r(i)
                  signzx(i)=signzx(i)*r(i)
                  pla(i) = pla(i) + dpla(i)     
                  dpla1(i) = dpla(i)
                endif  
              enddo
!
            else ! -- impl_s > 0  => implicit 
!
! ---         nonlinear hardening requires iterations in radial return --
!            
              call m36iter_imp(                                                &
                   nel    ,g3   ,h     ,yld    ,yfac      ,pla  ,              &
                   mat_param%table(1),nvartmp,vartmp,dpla1,fisokin,            &
                   signxx,signyy,signzz,signxy,signyz,signzx,                  &
                   sigbxx,sigbyy,sigbzz,sigbxy,sigbyz,sigbzx)
            end if ! -- impl_s 
!
          end if ! -- ipla --      
!=======================================================================
        else  ! vp=1
!=======================================================================
          plaold(1:nel) = pla(1:nel)                                          
          plap(1:nel)   = uvar(1:nel,2)                              
          yld(1:nel)    = uvar(1:nel,3)    
!
          if (nindx > 0 ) then
!
            if (fisokin == zero) then  ! no kinematic hardening        
              ! initialize plastic increment using previous step
#include "vectorize.inc" 
              do ii=1,nindx                                             
                i = indx(ii) 
                dpla_j(i)  = uvar(i,1) + em09 
              enddo
              ! initial yield stress at the beginning of the step
              xvec2(1:nel,1) = pla(1:nel)    ! plastic strain 
              xvec2(1:nel,2) = zero          ! initial static solution
!              if (ismooth == 2) then
!                xvec2(1:nel,2) = log(plap(1:nel))
!              else
!                xvec2(1:nel,2) = plap(1:nel)
!              end if
!
              call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,y1,dydx)
!
              yld(1:nel) = y1(1:nel) * yfac(1:nel)
              yld_ini(1:nel) = yld(1:nel)            
              h(1:nel)   = dydx(1:nel) * yfac(1:nel)
              dfsr(1:nel)= max(h(1:nel), zero) 
!------------------------
              do iter=1,niter 
#include "vectorize.inc" 
                do ii=1,nindx                                              
                  i = indx(ii)                                        
                  dpla(i) = dpla_j(i)                                
                  pla(i)  = plaold(i) + dpla(i)                    
                  plap(i) = dpla(i) / timestep                     
                  r(i)    = yld(i) / (yld(i) + g3(i)*dpla(i))         
                  svm(i)  = r(i)* svm(i)                         
                  f  = svm(i) - yld(i) - g3(i) *dpla(i)                 
                  df = -g3(i) - dfsr(i)                                 
                  df = sign(max(abs(df),em20),df)                      
                  if (dpla(i) > zero) then                            
                    dpla_j(i)=max(em10 ,dpla(i)-f/df)               
                  else                                                 
                    dpla_j(i)=em10                                     
                  endif                                                
                  pla(i) = plaold(i) + dpla_j(i)                    
                  plap(i)= dpla_j(i) / timestep                     
                enddo                                                      
                ! update yield
                xvec2(1:nel,1) = pla(1:nel)    ! plastic strain 
                if (ismooth == 2) then
                  xvec2(1:nel,2) = log(plap(1:nel))
                else
                  xvec2(1:nel,2) = plap(1:nel)
                end if
                call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,y1,dydx)

                yld(1:nel) = y1(1:nel)    * yfac(1:nel)
                h(1:nel)   = dydx(1:nel) * yfac(1:nel)
#include "vectorize.inc" 
                do ii=1,nindx
                  i = indx(ii)
                  dfsr(i) = h(i) + yfac(i) * (yld(i)-yld_ini(i)) / (plap(i)*timestep)
                end do
              enddo  ! iter
! ------------------------  
#include "vectorize.inc" 
              do ii=1,nindx                                           
                i = indx(ii) 
                pla(i)    = plaold(i) + dpla(i)
                plap(i)   = dpla(i) / timestep    
                signxx(i) = signxx(i)*r(i)
                signyy(i) = signyy(i)*r(i)
                signzz(i) = signzz(i)*r(i)
                signxy(i) = signxy(i)*r(i)
                signyz(i) = signyz(i)*r(i)
                signzx(i) = signzx(i)*r(i)
                dpla1(i)  = dpla(i)      
                uvar(i,1) = dpla(i) 
                uvar(i,2) = plap(i)     
                uvar(i,3) = yld(i)     
              enddo  !nindx      
!------------------------------------------------  
            else  !  (fisokin > 0) : mixed iso-kine hardening
!------------------------------------------------  
#include "vectorize.inc" 
              do ii=1,nindx                                             
                i = indx(ii) 
                dpla_j(i)  =  uvar(i,1) + em09 
                ! dpla_j (i) = (svm-yld (i))/(g3 (i)+h (i))! converges less well with radial estimate
              enddo
              ! initial static yield stress
              xvec2(1:nel,1) = zero
              xvec2(1:nel,2) = zero
              call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld0,dydx)
              yld_ini(1:nel) = yld0(1:nel) 
!
              yld(1:nel) = yld(1:nel) * (one-fisokin) + fisokin * yld_ini(1:nel)
              yld(1:nel) = yld(1:nel) * yfac(1:nel)
              h(1:nel)   = dydx(1:nel) * yfac(1:nel)
              hi(i)      = h(i)*(one-fisokin)
              coef(i) = (yld(i)-yld_ini(i)) / (plap(i)*timestep)
              dfsr(1:nel)= h(1:nel) + coef(i)*(one-fisokin) * yfac(1:nel)
!
              !-----------------------  
              !   newton iterations :
              !-----------------------  
              do iter=1,niter
#include        "vectorize.inc"
                do ii=1,nindx                                             
                  i = indx(ii) 
                  dpla(i) = dpla_j(i)
                  pla(i)  = plaold(i) + dpla(i)
                  plap(i) = dpla(i) / timestep    
                  r(i)    = yld(i)/(yld(i) +(g3(i)+fisokin*h(i))*dpla(i))
                  svm(i)  = r(i) * svm(i)
                  f       = svm(i) -  yld(i) - (g3(i)+fisokin*h(i)) *dpla(i)
                  df      =-(g3(i)+fisokin*h(i)) - (dfsr(i) + coef(i))
                  df = sign(max(abs(df),em20),df)
                  if(dpla(i) > zero) then
                    dpla_j(i)=max( em10 ,dpla(i)-f/df)
                  else
                    dpla_j(i)=em10
                  endif 
                  pla(i)  = plaold(i) + dpla_j(i)
                  plap(i) = dpla_j(i) / timestep    
                enddo
!
                ! update yield and tangent modulus
!
                xvec2(1:nel,1) = zero  ! initial yld
                if (ismooth == 2) then
                  xvec2(1:nel,2) = log(uvar(1:nel,2))
                else
                  xvec2(1:nel,2) = uvar(1:nel,2)
                end if
                call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld_ini,dydx)
!
                xvec2(1:nel,1) = pla(1:nel)    ! plastic strain 
                call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld,dydx)
!
                h(1:nel) = dydx(1:nel) * yfac(1:nel)
                hi(i)    = h(i)*(one-fisokin)
                coef(i) = (yld(i)-yld_ini(i)) / (plap(i)*timestep)
                dfsr(1:nel)= h(1:nel) + coef(i)*(one-fisokin) * yfac(1:nel)

                yld(1:nel) = yld(1:nel)*(one-fisokin) + fisokin * yld_ini(1:nel)        
                yld(1:nel) = yld(1:nel) * yfac(1:nel)       


!                   hi(i)= h(i)*(one-fisokin)
!                  dfsr(i) = hi(i)+ max(zero,pfac(i))* (one-fisokin)*(y2(i)-y1(i))
!     .                     /(uparam(7+jj(i))-uparam(6+jj(i))) /timestep
!                   yld(i)= fail(i)*(y1(i) + fac*(y2(i)-y1(i)))
!                  yld(i) = (one-fisokin) * yld(i) + 
!     .                 fisokin * (fail(i)*(y1(i)    + fac*(y2(i)-y1(i))))
!                  yld(i) = max(yld(i),em20)
!                  yld(i) = yld(i)  *max(zero,pfac(i))

              enddo  ! iter  
!--------------------------  
!
#include "vectorize.inc"
              do ii=1,nindx                                           
                i = indx(ii) 
                pla(i)    = plaold(i) + dpla(i)
                plap(i)   = dpla(i) / timestep    
                signxx(i) = signxx(i)*r(i)
                signyy(i) = signyy(i)*r(i)
                signzz(i) = signzz(i)*r(i)
                signxy(i) = signxy(i)*r(i)
                signyz(i) = signyz(i)*r(i)
                signzx(i) = signzx(i)*r(i)
                dpla1(i)  = dpla(i)      
                uvar(i,1) = dpla(i) 
                uvar(i,2) = plap(i)     
                uvar(i,3) = yld(i)     
              enddo 
            endif  !   fisokin
          endif    !   nindx > 0  (plasticity)
!------------------------------------------
        endif ! vp flag
!=======================================================================
!
        if (ipla /= 1 .or. impl_s == 0) then
          if (fisokin > zero) then   ! kinematic or mixed hardening
#include   "vectorize.inc"
            do i=1,nel
               dsxx = sigexx(i) - signxx(i) 
               dsyy = sigeyy(i) - signyy(i)
               dszz = sigezz(i) - signzz(i)
               dsxy = sigexy(i) - signxy(i)
               dsyz = sigeyz(i) - signyz(i)
               dszx = sigezx(i) - signzx(i)
 
               hkin   = two_third*fisokin*h(i)
               alpha  = hkin/(g2(i)+hkin)  
               sigpxx = alpha*dsxx 
               sigpyy = alpha*dsyy
               sigpzz = alpha*dszz 
               sigpxy = alpha*dsxy
               sigpyz = alpha*dsyz
               sigpzx = alpha*dszx

!            ..updates back stresses
               sigbxx(i) = sigbxx(i)  + sigpxx 
               sigbyy(i) = sigbyy(i)  + sigpyy
               sigbzz(i) = sigbzz(i)  + sigpzz
               sigbxy(i) = sigbxy(i)  + sigpxy
               sigbyz(i) = sigbyz(i)  + sigpyz
               sigbzx(i) = sigbzx(i)  + sigpzx         

!            ..gets stresses from shifted stresses and back stresses
               signxx(i) = signxx(i) + sigbxx(i) 
               signyy(i) = signyy(i) + sigbyy(i) 
               signzz(i) = signzz(i) + sigbzz(i) 
               signxy(i) = signxy(i) + sigbxy(i)
               signyz(i) = signyz(i) + sigbyz(i) 
               signzx(i) = signzx(i) + sigbzx(i) 
            enddo
          endif !   fisokin 
        end if  !   ipla/=1.or.impl_s==0
!-------------------------------------------------------------------------------
        if (mat_param%ieos == 0) then   ! add pressure to the deviatoric stress
          do i=1,nel
            p = bulk(i) * amu(i)        ! bulk*(rho/rho0-1)
            signxx(i) = signxx(i) - p
            signyy(i) = signyy(i) - p
            signzz(i) = signzz(i) - p
          enddo
        else
          ! if eos is used, material law calculates only deviatoric stress tensor
          !                 sound speed depends on pressure derivative over volume change
          !                 calculated in eos
          do i = 1, nel
            soundsp(i) = sqrt((dpdm(i) + four*g(i)/three)/rho0)
          enddo       
        end if 
!-------------------------------------------------------------------------------
        if (impl_s > 0) then   ! implicit
          !      save  the shifted elastic predictors, plastic strain multiplier,
          !      the kirchhoff's modulus and the plastic hardening modulus
          !      for the sake of computation of elasto-plastic stiffness matrix
!
          do i=1,nel
            if(dpla1(i) > 0) etse(i)= h(i)/g2(i)
          enddo
          do i = 1,nel
            if (dpla1(i) > zero) then

              ! von mises stress at the elastic predictor (point b)
              ! sigexx, etc. are deviatoric stresses
              vm = half*(sigexx(i)**2+sigeyy(i)**2+sigezz(i)**2)     &
                 + sigexy(i)**2+sigeyz(i)**2+sigezx(i)**2
              vm_1 =one/sqrt(three*vm)
              g3h = g3(i)+h(i)
              r(i) = max(zero,one-g3h*dpla1(i)*vm_1)
              ! norm_1 normalizes deviatoric stresses, includes consistent
              ! stiffness matrix parameter beta, von mises at b, and two_pmi
              norm_1=g3(i)*vm_1*sqrt(r(i)/g3h)
              ! deviatoric stresses "normalized"
              signor(i,1)=sigexx(i)*norm_1
              signor(i,2)=sigeyy(i)*norm_1
              signor(i,3)=sigezz(i)*norm_1
              signor(i,4)=sigexy(i)*norm_1
              signor(i,5)=sigeyz(i)*norm_1
              signor(i,6)=sigezx(i)*norm_1
              al_imp(i) = one - g3(i)*dpla1(i)*vm_1  ! parameter alpha of consistent matrix
            else
              al_imp(i) = one
            endif
          enddo
        endif  ! impl_s > 0
!-----------------------------------------------------------------------------------------
        do i=1,nel
          if (off(i) < em01) off(i) = zero
          if (off(i) < one)  off(i) = off(i)*four_over_5
        enddo
!-----------------------------------------------------------------------------------------
        if (ifail > 0) then
          nindx = 0
          if (ifail == 2) then
            if (inloc > 0) then 
              do i=1,nel
                if (epsmax < ep20) dmg(i) = max(dmg(i),planl(i)/epsmax)
                if ((planl(i) > epsmax .or. epstt(i) > epsf) .and. off(i) == one) then
                  off(i)= four_over_5
                  nindx = nindx+1
                  indx(nindx) = i
                endif
              enddo
            else
              do i=1,nel
                if (epsmax < ep20) dmg(i) = max(dmg(i),pla(i)/epsmax)
                if ((pla(i) > epsmax .or. epstt(i) > epsf) .and. off(i) == one) then
                  off(i)= four_over_5
                  nindx = nindx+1
                  indx(nindx) = i
                endif
              enddo
            endif
          else
            if (inloc > 0) then 
              do i=1,nel
                if (epsmax < ep20) dmg(i) = planl(i)/epsmax
                if (planl(i) > epsmax .and. off(i) == one) then
                  off(i)= four_over_5
                  nindx = nindx+1
                  indx(nindx) = i
                endif
              enddo
            else
              do i=1,nel
                if (epsmax < ep20) dmg(i) = pla(i)/epsmax
                if (pla(i) > epsmax .and. off(i) == one) then
                  off(i)= four_over_5
                  nindx = nindx+1
                  indx(nindx) = i
                endif
              enddo
            endif
          end if
!
          !  element failure output
          if (nindx > 0) then
            do i=1,nindx
!$omp critical
              write(iout, 1000) ngl(indx(i))
              write(istdo,1100) ngl(indx(i)),time
!$omp end critical
            enddo
          endif
!
        endif  ! ifail
!--------------------------------------------------------------------------
 1000 format(1x,'rupture of solid element number ',i10)
 1100 format(1x,'rupture of solid element number ',i10, ' at time :',g11.4)
!--------------------------------------------------------------------------
        return
        end subroutine sigeps36s
!--------------------------------------------------------------------------
      end module sigeps36s_mod
