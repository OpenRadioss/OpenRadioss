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
!===============================================================================

      !||====================================================================
      !||    sigeps128s_mod   ../engine/source/materials/mat/mat128/sigeps128s.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw            ../engine/source/materials/mat_share/mulaw.F90
      !||====================================================================
      module sigeps128s_mod
      contains


      !||====================================================================
      !||    sigeps128s              ../engine/source/materials/mat/mat128/sigeps128s.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
      !||--- calls      -----------------------------------------------------
      !||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod            ../common_source/modules/constant_mod.F
      !||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    table4d_mod             ../common_source/modules/table4d_mod.F
      !||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
      !||====================================================================
       subroutine sigeps128s(mat_param  ,                                       &
           nel      ,nuvar    ,nvartmp  ,uvar     ,vartmp   ,timestep ,         &
           depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,         &
           sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,         &
           signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,         &
           sighl    ,yld      ,et       ,pla      ,dpla     ,epsd     ,         &
           soundsp  ,off      ,l_sigb   ,sigb     )
!
! =================================================================================
! \brief orthotropic hill material with plastic strain rate dependency for solids

! =================================================================================
!   m o d u l e s
!-----------------------------------------------
      use matparam_def_mod
      use constant_mod ,only : pi,zero,one,half,third,two_third,two,three,four
      use constant_mod ,only : four_over_3,four_over_5
      use constant_mod ,only : em01,em10,em15,em20,ep20
      use table4d_mod
      use table_mat_vinterp_mod
! ---------------------------------------------------------------------------------
          implicit none
! ---------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer ,intent(in) :: nel                           !< element group size
      integer ,intent(in) :: nuvar                         !< number of state variables
      integer ,intent(in) :: nvartmp                       !< number of temporary internal variables
      integer ,intent(in) :: l_sigb                        !< size of backstress tensor
      my_real ,intent(in) :: timestep                      !< time step
      my_real ,dimension(nel)     ,intent(in)    :: depsxx !< deformation increment component
      my_real ,dimension(nel)     ,intent(in)    :: depsyy !< deformation increment component
      my_real ,dimension(nel)     ,intent(in)    :: depszz !< deformation increment component
      my_real ,dimension(nel)     ,intent(in)    :: depsxy !< deformation increment component
      my_real ,dimension(nel)     ,intent(in)    :: depsyz !< deformation increment component
      my_real ,dimension(nel)     ,intent(in)    :: depszx !< deformation increment component
      my_real ,dimension(nel)     ,intent(in)    :: sigoxx !< input  stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigoyy !< input  stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigozz !< input  stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigoxy !< input  stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigoyz !< input  stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigozx !< input  stress component
      my_real ,dimension(nel)     ,intent(out)   :: signxx !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signyy !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signzz !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signxy !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signyz !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signzx !< output stress component
      my_real ,dimension(nel)     ,intent(inout) :: off    !< element activation coefficient
      my_real ,dimension(nel)     ,intent(inout) :: pla    !< plastic strain
      my_real ,dimension(nel)     ,intent(inout) :: yld    !< yield stress 
      my_real ,dimension(nel)     ,intent(out)   :: dpla   !< plastic strain increment
      my_real ,dimension(nel)     ,intent(out)   :: et     !< tangent module
      my_real ,dimension(nel)     ,intent(out)   :: sighl  !< Hill equivalent stress 
      my_real ,dimension(nel)     ,intent(out)   :: soundsp!< sound speed
      my_real ,dimension(nel)     ,intent(out)   :: epsd   !< plastic strain rate
      my_real ,dimension(nel,l_sigb)  ,intent(inout) :: sigb      !< backstress tensor
      my_real ,dimension(nel,nuvar)   ,intent(inout) :: uvar      !< state variables
      integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp    !< temporary internal variables
      type (matparam_struct_)         ,intent(in)    :: mat_param !< material parameter structure
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,ii,iter,niter,nindx,ifunc,ndim
      integer ,dimension(nel) :: indx
      my_real :: dlam,ddep,sig_dfdsig,dsig_dlam,dpdt,seq                                 
      my_real :: normxx,normyy,normzz,normxy,normyz,normzx
      my_real :: dsigbxx_dlam,dsigbyy_dlam,dsigbzz_dlam
      my_real :: dsigbxy_dlam,dsigbyz_dlam,dsigbzx_dlam
      my_real :: cc,cp,asrate,fisokin,beta,dtime       
      my_real :: qr1,qr2,qx1,qx2,cr1,cr2,cx1,cx2
      my_real :: ff,gg,hh,ll,mm,nn
      my_real :: sigy,young,shear,bulk,nu,rho0
      my_real :: cii,cij
      my_real ,dimension(nel)   :: cowp              !< Cowper-Symonds strain rate factor
      my_real ,dimension(nel)   :: h,h0,hk           !< hardening tangent stiffness
      my_real ,dimension(nel)   :: phi               !< plastic yield criterion
      my_real ,dimension(nel)   :: yld0              !< initial yield stress
      my_real ,dimension(nel)   :: dphi_dlam,dpla_dlam
      my_real ,dimension(nel)   :: dpxx,dpyy,dpzz,dpxy,dpyz,dpzx
      my_real ,dimension(nel,1) :: xvec1
      my_real ,dimension(nel,2) :: xvec2
!===============================================================================    
      niter  = 3   ! max number of newton iterations
      dtime  = max(timestep, em20)
      young  = mat_param%young
      shear  = mat_param%shear
      bulk   = mat_param%bulk
      nu     = mat_param%nu
      rho0   = mat_param%rho0
!
      sigy   = mat_param%uparam(1)  
      qr1    = mat_param%uparam(2)  
      cr1    = mat_param%uparam(3)  
      qr2    = mat_param%uparam(4)  
      cr2    = mat_param%uparam(5)  
      qx1    = mat_param%uparam(6)  
      cx1    = mat_param%uparam(7)  
      qx2    = mat_param%uparam(8)  
      cx2    = mat_param%uparam(9)  
      cc     = mat_param%uparam(10) 
      cp     = mat_param%uparam(11)    
      ff     = mat_param%uparam(12) 
      gg     = mat_param%uparam(13) 
      hh     = mat_param%uparam(14) 
      ll     = mat_param%uparam(15) 
      mm     = mat_param%uparam(16) 
      nn     = mat_param%uparam(17) 
      asrate = min(one,mat_param%uparam(18)*dtime)
      fisokin = mat_param%uparam(19)
      beta    = one - fisokin

      cij  = three*bulk*nu/(one+nu)
      cii  = cij + two*shear
      ndim = 0
      if (mat_param%ntable == 1) ndim = mat_param%table(1)%ndim
!
      et(1:nel)   = one
      dpla(1:nel) = zero
      epsd(1:nel) = uvar(1:nel,1)  ! filtered plastic strain rate from previous time step
      soundsp(1:nel) = sqrt((bulk + four_over_3*shear) / rho0)     ! sound-speed
!---------------------------------------------------------------------
      ! element deletion condition
      do i=1,nel
        if (off(i) < one)  off(i) = four_over_5*off(i)
        if (off(i) < em01) off(i) = zero
      enddo 
      !< Backstress tensor computation
      if (fisokin > zero) then 
        !< Add the kinematic hardening contribution to stress tensor
        signxx(1:nel) = signxx(1:nel) - sigb(1:nel,1)
        signyy(1:nel) = signyy(1:nel) - sigb(1:nel,2)
        signzz(1:nel) = signzz(1:nel) - sigb(1:nel,3)
        signxy(1:nel) = signxy(1:nel) - sigb(1:nel,4)
        signyz(1:nel) = signyz(1:nel) - sigb(1:nel,5)
        signzx(1:nel) = signzx(1:nel) - sigb(1:nel,6)
!
        if (mat_param%ntable == 0) then     ! analytical yield formulation
         yld0(1:nel) = sigy
         h0(1:nel)   = qr1*cr1 + qr2*cr2 + qx1*cx1 + qx2*cx2
        else 
          if (ndim == 1) then
            yld0(1:nel) = sigy
          else if (ndim == 2) then
            xvec2(1:nel,1) = zero
            xvec2(1:nel,2) = epsd(1:nel)
            call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld0,h0)
          end if       
        end if
      endif
!---------------------------------------------------------------------
      !< trial stress tensor 
      do i=1,nel      
        signxx(i) = sigoxx(i) + cii*depsxx(i) + cij*depsyy(i) + cij*depszz(i)
        signyy(i) = sigoyy(i) + cij*depsxx(i) + cii*depsyy(i) + cij*depszz(i)
        signzz(i) = sigozz(i) + cij*depsxx(i) + cij*depsyy(i) + cii*depszz(i)
        signxy(i) = sigoxy(i) + shear*depsxy(i)
        signyz(i) = sigoyz(i) + shear*depsyz(i)
        signzx(i) = sigozx(i) + shear*depszx(i)
!
        ! hill equivalent stress
        sighl(i) = ff*(signyy(i) - signzz(i))**2 + gg*(signzz(i) - signxx(i))**2   &
                 + hh*(signxx(i) - signyy(i))**2 + two*ll*signyz(i)**2             &
                 + two*mm*signzx(i)**2 + two*nn*signxy(i)**2
        sighl(i) = sqrt(max(zero,sighl(i)))
      enddo 
!---------------------------------------------------------------------
      ! computing yield stress
!
      if (mat_param%ntable == 0) then     ! analytical yield formulation
        if (cc > zero)  then
          cowp(1:nel) = (epsd(1:nel)/cc)**cp
        else
          cowp(1:nel) = zero
        end if
        do i = 1,nel
          yld(i) = sigy                                                   &
                 + qr1*(one - exp(-cr1*pla(i)))                           &
                 + qr2*(one - exp(-cr2*pla(i)))                           &
                 + qx1*(one - exp(-cx1*pla(i)))                           &
                 + qx2*(one - exp(-cx2*pla(i))) 
          h(i)   = qr1*cr1*exp(-cr1*pla(i)) + qr2*cr2*exp(-cr2*pla(i))    &
                 + qx1*cx1*exp(-cx1*pla(i)) + qx2*cx2*exp(-cx2*pla(i))
          hk(i)  = fisokin * h(i)  * (one + cowp(i))
          h(i)   = beta * h(i) * (one + cowp(i))
          yld(i) = beta * yld(i) + fisokin*sigy + sigy*cowp(i)
        enddo
      else                               ! tabulated yield function
        if (ndim == 1) then
          xvec1(1:nel,1) = pla(1:nel)
          call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec1,yld,h)
        else if (ndim == 2) then
          xvec2(1:nel,1) = pla (1:nel)
          xvec2(1:nel,2) = epsd(1:nel)
          call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld,h)
        end if       
        hk (1:nel) = fisokin * h(1:nel)
        h  (1:nel) = beta * h(1:nel)
        yld(1:nel) = beta * yld(1:nel) + fisokin*yld0(1:nel)
      endif 
!      
      !====================================================================
      ! check yield criterion for all elements
      !====================================================================
!
      phi(1:nel) = sighl(1:nel) - yld(1:nel)
!
      nindx   = 0
      indx(:) = 0
      do i=1,nel         
        if (phi(i) > zero .and. off(i) == one) then
          nindx = nindx+1
          indx(nindx) = i
        endif
      enddo
!             
      !====================================================================
      ! plastic correction with cutting plane algorithm (newton iterations)
      !====================================================================     
!
      if (nindx > 0) then 
        do iter = 1, niter
#include "vectorize.inc" 
          ! loop over yielding elements
          do ii=1,nindx 
            i = indx(ii)
!            
            ! note     : in this part, the purpose is to compute for each iteration
            ! a plastic multiplier allowing to update internal variables to satisfy
            ! the consistency condition using the backward euler implicit method
            ! with a newton-raphson iterative procedure
            ! its expression at each iteration is : dlambda = - phi/dphi_dlambda
              ! -> phi          : current value of yield function (known)
            ! -> dphi_dlambda : derivative of phi with respect to dlambda by taking
            !                   into account of internal variables kinetic : 
            !                   plasticity, temperature and damage (to compute)
!          
            !<  computation of dphi/dsig = normal to the yield surface
            !-------------------------------------------------------------
            seq = max(sighl(i),em20)
            normxx = (gg*(signxx(i)-signzz(i)) + hh*(signxx(i)-signyy(i))) / seq
            normyy = (ff*(signyy(i)-signzz(i)) + hh*(signyy(i)-signxx(i))) / seq
            normzz = (ff*(signzz(i)-signyy(i)) + gg*(signzz(i)-signxx(i))) / seq
            normxy = two*nn*signxy(i) / seq
            normyz = two*ll*signyz(i) / seq
            normzx = two*mm*signzx(i) / seq
!            
            !< computation of dphi/dlambda  for Newton iterations over lambda
            !---------------------------------------------------------
!          
            !< derivative of phi with respect to stress increments : 
            !< dsig/dlam = dphi/dsig : dsig/dlam = Normal : dsig/dlam
            !   --------------------------------------------------------
            dsig_dlam = normxx * (cii * normxx + cij * normyy + cij * normzz )      &
                      + normyy * (cij * normxx + cii * normyy + cij * normzz )      &
                      + normzz * (cij * normxx + cij * normyy + cii * normzz )      &
                      + normxy * normxy * shear                                     &
                      + normyz * normyz * shear                                     &
                      + normzx * normzx * shear      
!
            !<  derivative of dpla over lambda, dpla = lam * dphi/dsig
            !<  calculated from plastic work equivalence : yld*dpla = sig:depsd
            !<  dpla/dlam = (sig : dphi/dsig) / yld
            sig_dfdsig = signxx(i)*normxx + signyy(i)*normyy + signzz(i)*normzz   &
                       + signxy(i)*normxy + signyz(i)*normyz + signzx(i)*normzx
            dpla_dlam(i) = sig_dfdsig / max(yld(i),em20)
!
            ! derivative of phi with respect to dlam
            dphi_dlam(i) = - dsig_dlam - h(i)*dpla_dlam(i)
!            
            !< correction for kinematic hardening
            !< derivative of backstress increment over lambda
            if (fisokin > zero) then 
              dsigbxx_dlam = two_third*hk(i)*(normxx*two + normyy + normzz)
              dsigbyy_dlam = two_third*hk(i)*(normyy*two + normxx + normzz)
              dsigbzz_dlam = two_third*hk(i)*(normzz*two + normxx + normyy)
              dsigbxy_dlam = two_third*hk(i)*normxy
              dsigbyz_dlam = two_third*hk(i)*normyz
              dsigbzx_dlam = two_third*hk(i)*normzx
              dphi_dlam(i) = dphi_dlam(i)                                            &
                 - normxx*dsigbxx_dlam - normyy*dsigbyy_dlam - normzz*dsigbzz_dlam   &
                 - normxy*dsigbxy_dlam - normyz*dsigbyz_dlam - normzx*dsigbzx_dlam
            endif
            dphi_dlam(i) = sign(max(abs(dphi_dlam(i)),em20) ,dphi_dlam(i))
!            
            !< plastic multiplier increment and plastic strain increment
            dlam = -phi(i) / dphi_dlam(i)
            ddep    = dlam*dpla_dlam(i)
            dpla(i) = max(zero, dpla(i) + ddep)
            pla(i)  = pla(i) + ddep   
            !< plastic strain tensor
            dpxx(i) = dlam * normxx
            dpyy(i) = dlam * normyy
            dpzz(i) = dlam * normzz
            dpxy(i) = dlam * normxy
            dpyz(i) = dlam * normyz
            dpzx(i) = dlam * normzx  
!            
            !< Update of the backstress tensor (if kinematic hardening)
            if (fisokin > zero) then 
              !< Remove old backstress
              signxx(i) = signxx(i) + sigb(i,1)
              signyy(i) = signyy(i) + sigb(i,2)
              signzz(i) = signzz(i) + sigb(i,3)
              signxy(i) = signxy(i) + sigb(i,4)
              signyz(i) = signyz(i) + sigb(i,5)
              signzx(i) = signzx(i) + sigb(i,6)
            end if
            !< elasto-plastic stresses update   
            signxx(i) = signxx(i) - (cii*dpxx(i) + cij*dpyy(i) + cij*dpzz(i))
            signyy(i) = signyy(i) - (cij*dpxx(i) + cii*dpyy(i) + cij*dpzz(i))
            signzz(i) = signzz(i) - (cij*dpxx(i) + cij*dpyy(i) + cii*dpzz(i))
            signxy(i) = signxy(i) - dpxy(i)*shear
            signyz(i) = signyz(i) - dpyz(i)*shear
            signzx(i) = signzx(i) - dpzx(i)*shear
!
            if (fisokin > zero) then 
              !< backstress tensor evolution
              sigb(i,1) = sigb(i,1) + dsigbxx_dlam*dlam
              sigb(i,2) = sigb(i,2) + dsigbyy_dlam*dlam
              sigb(i,3) = sigb(i,3) + dsigbzz_dlam*dlam
              sigb(i,4) = sigb(i,4) + dsigbxy_dlam*dlam
              sigb(i,5) = sigb(i,5) + dsigbyz_dlam*dlam
              sigb(i,6) = sigb(i,6) + dsigbzx_dlam*dlam
              !< Add the new backstress contribution
              signxx(i) = signxx(i) - sigb(i,1)
              signyy(i) = signyy(i) - sigb(i,2)
              signzz(i) = signzz(i) - sigb(i,3)
              signxy(i) = signxy(i) - sigb(i,4)
              signyz(i) = signyz(i) - sigb(i,5)
              signzx(i) = signzx(i) - sigb(i,6)
            endif
!
            ! update hill equivalent stress          
            sighl(i) = ff*(signyy(i) - signzz(i))**2 + gg*(signzz(i) - signxx(i))**2   &
                     + hh*(signxx(i) - signyy(i))**2 + two*ll*signyz(i)**2             &
                     + two*mm*signzx(i)**2 + two*nn*signxy(i)**2                              
            sighl(i) = sqrt(max(sighl(i),zero)) 
!                                                                                            
            if (mat_param%ntable == 0) then    ! analytical yield formulation                                                         
              yld(i) = sigy                                                            &
                     + qr1*(one - exp(-cr1*pla(i))) + qr2*(one - exp(-cr2*pla(i)))     &
                     + qx1*(one - exp(-cx1*pla(i))) + qx2*(one - exp(-cx2*pla(i))) 
              h(i)   = qr1*cr1*exp(-cr1*pla(i)) + qr2*cr2*exp(-cr2*pla(i))    &
                     + qx1*cx1*exp(-cx1*pla(i)) + qx2*cx2*exp(-cx2*pla(i))
              hk(i)  = fisokin * h(i)  * (one + cowp(i))
              h(i)   = beta * h(i) * (one + cowp(i))
              yld(i) = beta * yld(i) + fisokin*sigy + sigy*cowp(i)
!
              ! update yield criterion
              phi(i) = sighl(i) - yld(i)
            endif
!
          enddo
          ! end of the loop over the yielding elements
!
          ! case of tabulated yield stress definition
          if (mat_param%ntable == 1) then
            if (ndim == 1) then
              xvec1(1:nel,1) = pla(1:nel)
              call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec1,yld,h)
            else if (ndim == 2) then
              xvec2(1:nel,1) = pla (1:nel)
              xvec2(1:nel,2) = epsd(1:nel)
              call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld,h)
            end if       
            hk(1:nel)  = fisokin * h(1:nel)
            h (1:nel)  = beta * h(1:nel)
            yld(1:nel) = beta * yld(1:nel) + fisokin*yld0(1:nel)
!
            phi(1:nel) = sighl(1:nel) - yld(1:nel)
          endif
!
        enddo   ! end of the loop over the iterations 
      endif     ! nindx
      !===================================================================
      ! - end of plastic correction with cutting plane algorithm
      !===================================================================       
!    
      ! tangent stiffness coefficient for hourglass
      do ii = 1,nindx
        i = indx(ii)
        et(i) = (h(i) + hk(i)) / (h(i) + hk(i) + young)
      end do
!
      ! plastic strain-rate filtering
      do i=1,nel        
        dpdt    = dpla(i) / dtime
        epsd(i) = asrate * dpdt + (one - asrate) * uvar(i,1)
        uvar(i,1) = max(cc, epsd(i))  ! strain rate effect below static limit is ignored
      enddo 
!-----------
      return
      end
!-----------
      end module sigeps128s_mod
