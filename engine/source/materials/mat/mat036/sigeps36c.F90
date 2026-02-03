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
! ==================================================================================================

!||====================================================================
!||    sigeps36c_mod   ../engine/source/materials/mat/mat036/sigeps36c.F90
!||--- called by ------------------------------------------------------
!||    mulawc          ../engine/source/materials/mat_share/mulawc.F90
!||====================================================================
      module sigeps36c_mod
      contains

!||====================================================================
!||    sigeps36c               ../engine/source/materials/mat/mat036/sigeps36c.F90
!||--- called by ------------------------------------------------------
!||    mulawc                  ../engine/source/materials/mat_share/mulawc.F90
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
          subroutine sigeps36c(mat_param,                             &
               nel     ,nuvar   ,nvartmp,iplas   ,timestep,           &
               thkly   ,epsd_pg ,epsd    ,                            &
               epspxx  ,epspyy  ,epspxy ,uvar    ,vartmp  ,           &
               depsxx  ,depsyy  ,depsxy ,depsyz  ,depszx  ,           &
               epsxx   ,epsyy   ,epsxy  ,                             &
               sigoxx  ,sigoyy  ,sigoxy ,sigoyz  ,sigozx  ,           &
               signxx  ,signyy  ,signxy ,signyz  ,signzx  ,           &
               soundsp ,thk     ,pla    ,impl_s  ,ikt     ,           &
               off     ,etse    ,gs     ,yld     ,l_dmg   ,           &
               dpla    ,gama_imp,signor ,shf     ,hardm   ,           &
               yldfac  ,inloc   ,dplanl ,dmg     ,planl   ,           &
               l_sigb  ,l_planl ,sigbxx  ,sigbyy ,sigbxy  ,           &
               loff    )
! -----------------------------------------------------------------------------------------------
!       modules
! -----------------------------------------------------------------------------------------------
          use matparam_def_mod
          use table_mat_vinterp_mod
          use precision_mod, only : WP
          use constant_mod , only : zero,one,two,three, five
          use constant_mod , only : third,half,two_third,three_half,fourth,three_over_4,four_over_5
          use constant_mod , only : em20, infinity
!-----------------------------------------------
!       I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!       G l o b a l   P a r a m e t e r s
!-----------------------------------------------
#include      "mvsiz_p.inc"
!-----------------------------------------------
!       I N P U T   A r g u m e n t s
!-----------------------------------------------
          integer ,intent(in) :: nel
          integer ,intent(in) :: nuvar
          integer ,intent(in) :: nvartmp
          integer ,intent(in) :: inloc
          integer ,intent(in) :: iplas
          integer ,intent(in) :: l_sigb
          integer ,intent(in) :: l_dmg
          integer ,intent(in) :: l_planl
          integer ,intent(in) :: impl_s
          integer ,intent(in) :: ikt
          integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp
          type(matparam_struct_)          ,intent(in)    :: mat_param
          real(kind=wp)                   ,intent(in)    :: timestep
          real(kind=wp) ,dimension(nel)   ,intent(in)    :: thkly
          real(kind=wp) ,dimension(nel)   ,intent(in)    :: yldfac ! yld scale factor per integration point          
          real(kind=wp) ,dimension(nel)   ,intent(in)    :: loff
          real(kind=wp) ,dimension(nel)   ,intent(in)    :: epsd_pg  ! global element strain rate
          real(kind=wp) ,dimension(nel)   ,intent(in)    :: epsxx ,epsyy,epsxy
          real(kind=wp) ,dimension(nel)   ,intent(in)    :: epspxx,epspyy,epspxy
          real(kind=wp) ,dimension(nel)   ,intent(in)    :: depsxx,depsyy,depsxy,depsyz,depszx
          real(kind=wp) ,dimension(nel)   ,intent(inout) :: sigoxx,sigoyy,sigoxy,sigoyz,sigozx
          real(kind=wp) ,dimension(nel)   ,intent(inout) :: gs,shf
          real(kind=wp) ,dimension(nel)   ,intent(inout) :: epsd     ! lbuf%epsd
          real(kind=wp) ,dimension(nel)   ,intent(inout) :: pla
          real(kind=wp) ,dimension(nel)   ,intent(inout) :: dplanl
          real(kind=wp), dimension(nel),   intent(inout) :: off,thk,yld,hardm
          real(kind=wp) ,dimension(nel)   ,intent(out)   :: soundsp,etse,dpla,gama_imp
          real(kind=wp) ,dimension(nel)   ,intent(out)   :: signxx,signyy,signxy,signyz,signzx
          real(kind=wp), dimension(nel*l_sigb)  ,intent(inout) :: sigbxx,sigbyy,sigbxy
          real(kind=wp) ,dimension(nel*l_dmg)   ,intent(inout) :: dmg
          real(kind=wp) ,dimension(nel*l_planl) ,intent(in)    :: planl
          real(kind=wp) ,dimension(mvsiz,5)     ,intent(out)   :: signor
          real(kind=wp) ,dimension(nel,nuvar)   ,intent(out)   :: uvar
!-----------------------------------------------
!       l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer :: i,ii,n,nindx,vp,ifail
          integer :: niter,func_p,func_e,yldcheck,ismooth,ndim
          integer, dimension(mvsiz) :: indx
          real(kind=wp) :: a,p2,r,dezz,svm,ssp
          real(kind=wp) :: hkin,dtinv,aaa,alpha,ce,einf,epsp1,epsp2
          real(kind=wp) :: f,df,q2,yld_i
          real(kind=wp) :: sigpxx,sigpyy,sigpxy
          real(kind=wp) :: young,a11,a21,shear,rho0,ccf
          real(kind=wp) :: nu,nu11,nu21,nu3,nu_mnu,u_mnu,t_pnu
          real(kind=wp) :: epsmax,epsr1,epsr2,epsf,fisokin,s1,s2,s3
          real(kind=wp) :: fcut,asrate
          real(kind=wp) ,dimension(nel)   :: yfac,yld0
          real(kind=wp) ,dimension(mvsiz) :: e,a1,a2,g,g3
          real(kind=wp) ,dimension(mvsiz) :: dydx,rfac,escale
          real(kind=wp) ,dimension(mvsiz) :: aa,bb,dpla_j
          real(kind=wp) ,dimension(mvsiz) :: pp,qq,fail,h,hk
          real(kind=wp) ,dimension(mvsiz) :: sigexx,sigeyy,sigexy
          real(kind=wp) ,dimension(mvsiz) :: y1,dr,svm2,yld2,hi,epst
          real(kind=wp) ,dimension(mvsiz) :: p0,pfac,pscale
          real(kind=wp) ,dimension(mvsiz) :: plap
          real(kind=wp) ,dimension(nel,1) :: xvec1
          real(kind=wp) ,dimension(nel,2) :: xvec2
!------------------
      data niter/3/
!=======================================================================
      rho0  = mat_param%rho0
      young = mat_param%young
      shear = mat_param%shear
      nu = mat_param%nu
!
      ismooth  = mat_param%iparam(1) ! strain rate interpolation flag (linear/log)
      ifail    = mat_param%iparam(2) ! flag for failure
      vp       = mat_param%iparam(3) ! flag for plastic strain dependency
      yldcheck = mat_param%iparam(4)
!
      a11      = mat_param%uparam(1)
      a21      = mat_param%uparam(2)
      epsmax   = mat_param%uparam(3)
      epsr1    = mat_param%uparam(4)
      epsr2    = mat_param%uparam(5)
      epsf     = mat_param%uparam(6)   
      fisokin  = mat_param%uparam(7) 
      ce       = mat_param%uparam(8) 
      einf     = mat_param%uparam(9)  
      fcut     = mat_param%uparam(10) 
      
      asrate   = min(one, fcut*timestep)

      ndim     = mat_param%table(1)%ndim
      func_p   = mat_param%table(2)%notable
      func_e   = mat_param%table(3)%notable

      nu_mnu = nu / (one - nu)
      t_pnu  = three  / (one + nu)
      u_mnu  = one  / (one - nu)
      nu3    = one-nu_mnu
      ccf    = (one - nu**2)*rho0
      ssp    = sqrt(young/ccf)
!-----------------------------
      etse(1:nel)   = one
      escale(1:nel) = one
!--------------------------------------------------------------------------------
      ! calculation of young modulus and elastic constants by element 
!------------------------------------------
      nindx=0
      do i=1,nel         
        if (pla(i) > zero .and. off(i) == one) then
          nindx = nindx+1
          indx(nindx) = i
        endif
      enddo
!
      if (func_e > 0) then           ! young modulus evolution function                                
        xvec1(1:nel,1) = pla(1:nel)                                                           
        call table_mat_vinterp(mat_param%table(3),nel,nel,vartmp(1,4),xvec1,escale,dydx)     
        e(1:nel) = escale(1:nel) * young                                                     
        g(1:nel) = half*e(1:nel) / (one+nu)                                                      
      elseif (ce /= zero) then       ! variable young modulus defined analytically                                                 
        do ii=1,nindx
          i = indx(ii)
          e(i) = young - (young-einf)*(one-exp(-ce*pla(i)))
          g(i) = half*e(i) / (one+nu)                                                      
        end do
      else                           ! constant Young modulus                                                                              
        e(1:nel) = young
        g(1:nel) = shear
      end if                                                                                  
      a1(1:nel) = e(1:nel)/(one - nu*nu)                                                      
      a2(1:nel) = nu*a1(1:nel)                                                                
      g3(1:nel) = g(1:nel) * three                                                            
      gs(1:nel) = g(1:nel) * shf(1:nel)                                                       
      soundsp(1:nel) = sqrt(e(1:nel)/ccf)                                                     
!-------------------
!     damage factor based on equivalent strain 
!-------------------
      if (ifail == 2) then
        epst(1:nel) = half*( epsxx(1:nel)+epsyy(1:nel)                                &
                    + sqrt( (epsxx(1:nel)-epsyy(1:nel))*(epsxx(1:nel)-epsyy(1:nel))   &
                    + epsxy(1:nel)*epsxy(1:nel) ) )
        fail(1:nel) = max(em20,min(one,(epsr2-epst(1:nel))/(epsr2-epsr1)))
        dmg(1:nel)  = one - fail(1:nel)
      else
        fail(1:nel) = one
      endif
!-----------------------------------------------------------------------------------------
        ! trial elastic stress
!-----------------------------------------------------------------------------------------
        sigoxx(1:nel) = sigoxx(1:nel) - sigbxx(1:nel)
        sigoyy(1:nel) = sigoyy(1:nel) - sigbyy(1:nel)
        sigoxy(1:nel) = sigoxy(1:nel) - sigbxy(1:nel)
!
        p0(1:nel) = -(sigoxx(1:nel) + sigoyy(1:nel))*third
        signxx(1:nel) = sigoxx(1:nel)+a1(1:nel)*depsxx(1:nel)+a2(1:nel)*depsyy(1:nel)
        signyy(1:nel) = sigoyy(1:nel)+a2(1:nel)*depsxx(1:nel)+a1(1:nel)*depsyy(1:nel)
        signxy(1:nel) = sigoxy(1:nel)+g(1:nel) *depsxy(1:nel)
        signyz(1:nel) = sigoyz(1:nel)+gs(1:nel) *depsyz(1:nel)
        signzx(1:nel) = sigozx(1:nel)+gs(1:nel) *depszx(1:nel)
        sigexx(1:nel) = signxx(1:nel)
        sigeyy(1:nel) = signyy(1:nel)
        sigexy(1:nel) = signxy(1:nel)
 !-----------------------------------------------
!       pressure dependent yield function factor
!-----------------------------------------------
        if (func_p > 0) then 
          xvec1(1:nel,1) = p0(1:nel)
          call table_mat_vinterp(mat_param%table(2),nel,nel,vartmp(1,3),xvec1,pfac,dydx)
          pfac(1:nel) = max(zero, pfac(1:nel))
        else
          pfac(1:nel) = one                                
        endif
        yfac(1:nel) = fail(1:nel) * pfac(1:nel) * yldfac(1:nel)
!=======================================================================
!     split code in 2 independent part :
!              vp = 1 dependent on plastic strain rate 
!              vp = 0 dependent on total strain rate 
!=======================================================================
      if (vp == 0) then
!------------------------------------------
        if (ndim == 1) then    ! static yield curve, no strain rate dependency
          do i=1,nel
            epsd(i) = half*( abs(epspxx(i)+epspyy(i))                       &
                    + sqrt( (epspxx(i)-epspyy(i))*(epspxx(i)-epspyy(i))     &
                    + epspxy(i)*epspxy(i) ) )
          enddo
        else                   ! strain rate is always filtered with multiple rate curves
          epsd(1:nel) = asrate*epsd_pg(1:nel) + (one-asrate)*epsd(1:nel)
        endif       
!-------------------
!       initial and actual yield stress
!-------------------
        if (fisokin > zero) then  ! initial yield value is needed
          if (ndim == 1) then
            yld0(1:nel) = mat_param%table(1)%y1d(1)
          else if (ndim == 2) then
            xvec2(1:nel,1) = zero
            xvec2(1:nel,2) = epsd(1:nel)
            call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld0,dydx)
          end if
        end if
!
        if (ndim == 1) then    ! only static curve => no strain rate interpolation
          xvec1(1:nel,1) = pla(1:nel)
          call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec1,y1,dydx)
        else if (ndim == 2) then
          xvec2(1:nel,1) = pla (1:nel)
          xvec2(1:nel,2) = epsd(1:nel)
          call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,y1,dydx)
        end if
!
        h(1:nel) = dydx(1:nel) * yfac(1:nel)
        if (fisokin == zero) then
          yld(1:nel) = y1(1:nel) * yfac(1:nel)
        else
          yld(1:nel) = ((one-fisokin)*y1(1:nel) + fisokin *yld0(1:nel))*yfac(1:nel)
        end if
!
        if (yldcheck == 1) then
          do i=1,nel
            yld(i) = max(yld(i), em20)
          end do
        end if
!------------------------------
!       projection
!-------------------
        dpla(1:nel) = zero
        if (iplas == 0) then  ! radial projection 
          do i=1,nel
            svm2(i)= signxx(i)**2 + signyy(i)**2 - signxx(i)*signyy(i) + three*signxy(i)*signxy(i)
          end do
          do i=1,nel
            if (svm2(i) > yld(i)**2) then
              svm = sqrt(svm2(i))  
              r   = yld(i)/svm
              signxx(i)=signxx(i)*r 
              signyy(i)=signyy(i)*r 
              signxy(i)=signxy(i)*r
              dpla(i) = off(i)*svm*(one-r)/(g3(i)+h(i))
              pla(i) = pla(i) + dpla(i)
              if (inloc == 0) then 
                if(yld(i) .ne. 0) then
                  !yld may be 0 when signxx+signyy == 0, that will cause floating point
                  !exception in debug mode
                  dezz = dpla(i) * half*(signxx(i)+signyy(i)) / yld(i)
                else
                  dezz = zero 
                endif
                dezz=-(depsxx(i)+depsyy(i))*nu_mnu-nu3*dezz
                thk(i) = thk(i) + dezz*thkly(i)*off(i)
              endif
              etse(i)= h(i)/(h(i)+e(i)) !  
            endif
          enddo
!
        elseif (iplas == 1) then    ! implicit scheme, 3 newton iterations
          do i=1,nel
            h(i) = max(zero,h(i))
            s1=signxx(i)+signyy(i)
            s2=signxx(i)-signyy(i)
            s3=signxy(i)
            aa(i)= fourth*s1*s1
            bb(i)= three_over_4*s2*s2 + three*s3*s3
            svm2(i)= aa(i)+bb(i)
          enddo
          if (inloc == 0) then 
            do i=1,nel  
              dezz = -(depsxx(i)+depsyy(i))*nu_mnu
              thk(i) = thk(i) + dezz*thkly(i)*off(i)
            enddo
          endif
          nindx=0
          do i=1,nel
            if (svm2(i) > yld(i)*yld(i) .and. off(i) == one) then
              nindx=nindx+1
              indx(nindx)=i
            endif
          enddo
!
          if (nindx > 0) then
#include "vectorize.inc"
            do ii=1,nindx
              i=indx(ii)
              svm=sqrt(svm2(i))  
              dpla_j(i)=(svm-yld(i))/(g3(i)+h(i))
              etse(i)= h(i)/(h(i)+e(i))  
              hi(i) = h(i)*(one-fisokin)
              hk(i) = two_third*h(i)*fisokin
            enddo
!
            do n=1,niter
#include "vectorize.inc"
              do ii=1,nindx
                i=indx(ii)
                dpla(i)=dpla_j(i)
                yld_i = yld(i) + hi(i)*dpla(i)
                dr(i) = half*e(i)*dpla(i)/yld_i
                aaa   = three*hk(i)/e(i) 
                nu11  = u_mnu+aaa
                nu21  = t_pnu+aaa
                pp(i) = one/(one+dr(i)*nu11)
                qq(i) = one/(one+dr(i)*nu21)     
                p2    = pp(i)*pp(i)
                q2    = qq(i)*qq(i)
                f     = aa(i)*p2+bb(i)*q2-yld_i*yld_i
                df    =-(aa(i)*nu11*p2*pp(i)+bb(i)*nu21*q2*qq(i))          &
                      * (e(i)-two*dr(i)*hi(i)) / yld_i - two*hi(i)*yld_i
                df = sign(max(abs(df),em20),df)
                if(dpla(i) > zero) then
                  dpla_j(i)=max(zero,dpla(i)-f/df)
                else
                  dpla_j(i)=zero
                endif        
              enddo
            enddo
!------------------------------------------
!           update plastic strain, stress and shell thickness
!------------------------------------------
#include "vectorize.inc"
            do ii=1,nindx
              i=indx(ii)
              pla(i) = pla(i) + dpla(i)
              s1=(signxx(i)+signyy(i))*pp(i)
              s2=(signxx(i)-signyy(i))*qq(i)
              signxx(i)=half*(s1+s2)
              signyy(i)=half*(s1-s2) 
              signxy(i)=signxy(i)*qq(i)
              if (inloc == 0) then 
                dezz = - nu3*dr(i)*s1/e(i) 
                thk(i) = thk(i) + dezz*thkly(i)*off(i)
              endif
              yld(i) = yld(i) + hi(i)*dpla(i) 
            enddo
          endif
!-------------------------------------------
        elseif (iplas == 2) then ! approximate normal projection + radial return
          do i=1,nel
            h(i) = max(zero,h(i))
            svm2(i) = signxx(i)**2 + signyy(i)**2 - signxx(i)*signyy(i) + three*signxy(i)*signxy(i)
            if (inloc == 0) then 
              dezz = -(depsxx(i)+depsyy(i))*nu_mnu
              thk(i) = thk(i) + dezz*thkly(i)*off(i)
            endif
          enddo
          nindx=0
          do i=1,nel
            yld2(i)=yld(i)*yld(i)
            if (svm2(i) > yld2(i) .and. off(i) == one) then
              nindx=nindx+1
              indx(nindx)=i
            endif
          enddo
!
          if (nindx > 0) then
#include "vectorize.inc"
            do ii=1,nindx
              i=indx(ii)
              a = (svm2(i)-yld2(i))                                                 &
                / (five*svm2(i)+three*(-signxx(i)*signyy(i)+signxy(i)*signxy(i)))
              s1= (one-two*a)*signxx(i)+ a*signyy(i)
              s2= a*signxx(i)+(one-two*a)*signyy(i)
              s3=(one-three*a)*signxy(i)
              signxx(i)=s1
              signyy(i)=s2
              signxy(i)=s3
              svm=sqrt(svm2(i))  
              dpla(i) = off(i)*(svm-yld(i))/(g3(i)+h(i))
!
              hk(i) = h(i)*(one-fisokin)
              yld(i) =yld(i)+hk(i)*dpla(i)
            end do
!
            do ii=1,nindx
              i=indx(ii)
              svm = sqrt(signxx(i)**2 +signyy(i)**2 - signxx(i)*signyy(i)     &
                  + three*signxy(i)*signxy(i))  
              r  = min(one,yld(i)/max(em20,svm))
              signxx(i)=signxx(i)*r
              signyy(i)=signyy(i)*r
              signxy(i)=signxy(i)*r
              pla(i) = pla(i) + dpla(i)
              if (inloc == 0) then
                dezz = dpla(i) * half*(signxx(i)+signyy(i)) / yld(i)
                dezz = -nu3*dezz
                thk(i) = thk(i) + dezz*thkly(i)*off(i)
              endif
              etse(i)= h(i)/(h(i)+e(i)) 
            end do
          end if
!=======================================================================
        endif !  iplas
!=======================================================================
      else    ! vp = 1  : plastic strain rate dependency
!=======================================================================          
          if (fisokin > zero) then  ! initial yield value is needed
            if (ndim == 1) then
              yld0(1:nel) = mat_param%table(1)%y1d(1)
            else if (ndim == 2) then
              xvec2(1:nel,1) = zero
              xvec2(1:nel,2) = uvar(1:nel,2)
              call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld0,dydx)
            end if
          end if
!
          xvec2(1:nel,1) = pla(1:nel)    ! plastic strain 
          if (ismooth == 2) then
            xvec2(1:nel,2) = log(uvar(1:nel,2))
          else
            xvec2(1:nel,2) = uvar(1:nel,2)
          end if
          call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,y1,dydx)
!
          h(1:nel) = dydx(1:nel) * yfac(1:nel)
          if (fisokin == zero) then  ! isotropic hardening
            yld(1:nel) = y1(1:nel) * yfac(1:nel)
          else                       ! kinematic or mixed hardening
            yld(1:nel) = ((one-fisokin)*y1(1:nel) + fisokin * yld0(1:nel))*yfac(1:nel)
            yld(1:nel) = max(yld(1:nel),em20)
          end if
!-------------------
!       yield criterion
!-------------------------
        do i=1,nel
          h(i) = max(zero,h(i))
          s1=signxx(i)+signyy(i)
          s2=signxx(i)-signyy(i)
          s3=signxy(i)
          aa(i)=fourth*s1*s1
          bb(i)=three_over_4*s2*s2+3.*s3*s3
          svm2(i)= aa(i)+bb(i)  
          if (inloc == 0) then 
            dezz = -(depsxx(i)+depsyy(i))*nu_mnu
            thk(i) = thk(i) + dezz*thkly(i)*off(i)
          endif
        enddo
!-------------------------
        nindx=0
        do i=1,nel
          if(svm2(i) > yld(i)*yld(i) .and. off(i) == one) then
            nindx=nindx+1
            indx(nindx)=i
          endif
        enddo
!
        if (nindx > 0) then
#include "vectorize.inc"
          do ii=1,nindx
           i=indx(ii)
           svm = sqrt(svm2(i))  
           dpla_j(i)=(svm-yld(i))/(g3(i)+h(i))
           etse(i)= h(i)/(h(i)+e(i))  
           hi(i) = h(i)*(one-fisokin)
           hk(i) = two_third*h(i)*fisokin
          enddo
!
          nu3 = one-nu_mnu
          do n=1,niter
#include "vectorize.inc"
            do ii=1,nindx
              i=indx(ii)
              dpla(i)=dpla_j(i)
              yld_i =yld(i)+hi(i)*dpla(i)
              dr(i) =half*e(i)*dpla(i)/yld_i
              aaa = three*hk(i)/e(i) 
              nu11  = u_mnu+aaa
              nu21  = t_pnu+aaa
              pp(i) = one/(one+dr(i)*nu11)
              qq(i) = one/(one+dr(i)*nu21)     
              p2    = pp(i)*pp(i)
              q2    = qq(i)*qq(i)
              f     = aa(i)*p2+bb(i)*q2-yld_i*yld_i
              df    =-(aa(i)*nu11*p2*pp(i)+bb(i)*nu21*q2*qq(i))        &
                    * (e(i)-two*dr(i)*hi(i))/yld_i  -two*hi(i)*yld_i
              df = sign(max(abs(df),em20),df)
              if(dpla(i) > zero) then
                dpla_j(i)=max(zero,dpla(i)-f/df)
              else
                dpla_j(i)=zero
              endif        
            enddo ! nindx
          enddo   ! niter
!------------------------------------------
!           update plastic strain, stress and shell thickness
!------------------------------------------
#include "vectorize.inc"
          do ii=1,nindx
             i=indx(ii)
             pla(i) = pla(i) + dpla(i)
             s1=(signxx(i)+signyy(i))*pp(i)
             s2=(signxx(i)-signyy(i))*qq(i)
             signxx(i)=half*(s1+s2)
             signyy(i)=half*(s1-s2) 
             signxy(i)=signxy(i)*qq(i)
             if (inloc == 0) then 
               dezz = - nu3*dr(i)*s1/e(i) 
               thk(i) = thk(i) + dezz*thkly(i)*off(i)
             endif
             !-----for kin hardening
             yld(i) =yld(i)+hi(i)*dpla(i) 
          enddo 
        endif 
!=======================================================================
      endif  ! vp flag
!=======================================================================
!       element failure test
!----------------------------------------------------------------------
        if (ifail == 1) then
          if (inloc > 0) then 
            do i=1,nel
              if (epsmax < infinity) dmg(i) = planl(i)/epsmax
              if (off(i) == one .and. planl(i) > epsmax) off(i) = four_over_5
            enddo
          else 
            do i=1,nel
              if (epsmax < infinity) dmg(i) = pla(i)/epsmax
              if (off(i) == one .and. pla(i) > epsmax)   off(i) = four_over_5
            enddo
          endif
        elseif (ifail == 2) then
          if (inloc > 0) then
            do i=1,nel 
              if (epsmax < infinity) dmg(i) = max(dmg(i),planl(i)/epsmax)
              if (off(i) == one .and. (planl(i) > epsmax .or. epst(i) > epsf)) off(i) = four_over_5
            enddo
          else
            do i=1,nel 
              if (epsmax < infinity) dmg(i) = max(dmg(i),pla(i)/epsmax)
              if (off(i) == one .and. (pla(i) > epsmax .or. epst(i) > epsf))   off(i) = four_over_5
            enddo
          endif
        endif     
!----------------------------------------------------------------------
        if (impl_s > 0 .and. ikt > 0 ) then
          do i = 1,nel                                                
            if (dpla(i) > zero) then                                 
! ...         parameter d(gama)                                        
              gama_imp(i)= three_half*dpla(i)/yld(i)                  
! ...         hk,hh---                                                 
              signor(i,4)=fisokin*h(i)                                 
              signor(i,5)=(one-fisokin)*h(i)                           
! ...         deviatoric stresses shifted by modified back stress ->ksi
              signor(i,1)=third*(two*signxx(i)-signyy(i))              
              signor(i,2)=third*(two*signyy(i)-signxx(i))              
              signor(i,3)=two*signxy(i)                                
            else                                                       
              gama_imp(i) = zero                                       
            end if                                                     
          end do                                                      
        end if
!------------------------------------------
!       plastic strain filtering
!------------------------------------------
        if (vp == 1) then
           dtinv = one/max(timestep,em20) 
           do i=1,nel    
              plap(i)   = asrate*dpla(i)*dtinv + (one - asrate)*uvar(i,2)
              uvar(i,2) = plap(i)
            enddo
        endif
!------------------------------------------
!       kinematic hardening
!------------------------------------------
        if (fisokin > zero) then
          do i=1,nel
            hkin  = fisokin*h(i)
            alpha = hkin*dpla(i)/yld(i)
            sigpxx = alpha*signxx(i) 
            sigpyy = alpha*signyy(i) 
            sigpxy = alpha*signxy(i)

            sigbxx(i) = sigbxx(i) + sigpxx
            sigbyy(i) = sigbyy(i) + sigpyy
            sigbxy(i) = sigbxy(i) + sigpxy
!
            signxx(i) = signxx(i) + sigbxx(i)
            signyy(i) = signyy(i) + sigbyy(i)
            signxy(i) = signxy(i) + sigbxy(i)
          enddo
        endif
!
        hardm(1:nel) = h(1:nel)  ! hardening modulus for output
!--------------------------------
!       non-local thickness variation
!--------------------------------
        if (inloc > 0) then
          do i = 1,nel 
            if (loff(i) == one) then 
              svm    = sqrt(signxx(i)*signxx(i) + signyy(i)**2 - signxx(i)*signyy(i)   &
                     + three*signxy(i)*signxy(i))
              dezz   = max(dplanl(i),zero)*half*(signxx(i)+signyy(i))/max(svm,em20)
              dezz   = -nu*((signxx(i)-sigoxx(i)+signyy(i)-sigoyy(i))/e(i)) - dezz
              thk(i) = thk(i) + dezz*thkly(i)*off(i)
            endif     
          enddo  
        endif
! ----------------------------------------------------------------------------------------
        return
        end subroutine sigeps36c
! ----------------------------------------------------------------------------------------
      end module sigeps36c_mod
