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
      !||    sigeps128c_mod   ../engine/source/materials/mat/mat128/sigeps128c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc           ../engine/source/materials/mat_share/mulawc.F90
      !||====================================================================
      module sigeps128c_mod
      contains

      !||====================================================================
      !||    sigeps128c              ../engine/source/materials/mat/mat128/sigeps128c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc                  ../engine/source/materials/mat_share/mulawc.F90
      !||--- calls      -----------------------------------------------------
      !||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod            ../common_source/modules/constant_mod.F
      !||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    table4d_mod             ../common_source/modules/table4d_mod.F
      !||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
      !||====================================================================
      subroutine sigeps128c(mat_param   ,                                           &
           nel      ,nuvar    ,nvartmp  ,uvar     ,vartmp   ,timestep ,             &
           depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,                       &
           sigoxx   ,sigoyy   ,sigoxy   ,sigoyz   ,sigozx   ,                       &
           signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,                       &
           soundsp  ,thk      ,pla      ,dpla     ,epsp     ,                       &
           off      ,et       ,thkly    ,shf      ,yld      ,                       &
           hardm    ,sighl    )
!
! =================================================================================
! \brief orthotropic hill material with plastic strain rate dependancy for shells

! =================================================================================
!   m o d u l e s
!-----------------------------------------------
      use matparam_def_mod
      use constant_mod ,only : pi,zero,one,half,third,two,three,four
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
      my_real ,intent(in) :: timestep                      !< time step
      my_real ,dimension(nel)     ,intent(in)    :: depsxx !< deformation increment component
      my_real ,dimension(nel)     ,intent(in)    :: depsyy !< deformation increment component
      my_real ,dimension(nel)     ,intent(in)    :: depsxy !< deformation increment component
      my_real ,dimension(nel)     ,intent(in)    :: depsyz !< deformation increment component
      my_real ,dimension(nel)     ,intent(in)    :: depszx !< deformation increment component
      my_real ,dimension(nel)     ,intent(in)    :: sigoxx !< input  stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigoyy !< input  stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigoxy !< input  stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigoyz !< input  stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigozx !< input  stress component
      my_real ,dimension(nel)     ,intent(in)    :: thkly  !< relative layer thickness
      my_real ,dimension(nel)     ,intent(in)    :: shf    !< transverse shear factor for shells
      my_real ,dimension(nel)     ,intent(inout) :: off    !< element activation coefficient
      my_real ,dimension(nel)     ,intent(inout) :: pla    !< plastic strain
      my_real ,dimension(nel)     ,intent(inout) :: thk    !< element thickness
      my_real ,dimension(nel)     ,intent(out)   :: signxx !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signyy !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signxy !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signyz !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signzx !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: dpla   !< plastic strain increment
      my_real ,dimension(nel)     ,intent(out)   :: sighl  !< hill equivalent stress 
      my_real ,dimension(nel)     ,intent(out)   :: yld    !< yield stress 
      my_real ,dimension(nel)     ,intent(out)   :: soundsp!< sound speed
      my_real ,dimension(nel)     ,intent(out)   :: hardm  !< tangent module
      my_real ,dimension(nel)     ,intent(out)   :: epsp   !< plastic strain rate
      my_real ,dimension(nel)     ,intent(out)   :: et     !< hourglass stiness factor
      my_real ,dimension(nel,nuvar)   ,intent(inout) :: uvar      !< state variables
      integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp    !< temporary internal variables
      type (matparam_struct_) ,intent(in) :: mat_param !< material parameter structure
      target :: mat_param
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,ii,iter,niter,nindx,ifunc,ndim
      integer ,dimension(nel) :: indx
      my_real :: sigy,young,shear,bulk,nu,a11,a12,rho0
      my_real :: dlam,ddep,sig_dfdsig,dfdsig2,dpdt,seq                                 
      my_real :: normxx,normyy,normzz,normxy,normyz,normzx
      my_real :: cc,cp,asrate,dtime       
      my_real :: qr1,qr2,qx1,qx2,cr1,cr2,cx1,cx2
      my_real :: ff,gg,hh,nn
      my_real :: dezz,deelzz
      my_real ,dimension(nel)   :: cowp              !< Cowper-Symonds strain rate factor
      my_real ,dimension(nel)   :: h                 !< hardening tangent stiffness
      my_real ,dimension(nel)   :: phi               !< plastic yield criterion
      my_real ,dimension(nel)   :: dphi_dlam,dpla_dlam
      my_real ,dimension(nel)   :: dpxx,dpyy,dpzz,dpxy,dpyz,dpzx
      my_real ,dimension(nel,1) :: xvec1
      my_real ,dimension(nel,2) :: xvec2
      type (table_4d_) ,pointer :: itable
!===============================================================================    
      niter  = 3   ! max number of newton iterations
      dtime  = max(timestep, em20)
      !< Elastic parameters
      young  = mat_param%young
      shear  = mat_param%shear
      bulk   = mat_param%bulk
      nu     = mat_param%nu
      rho0   = mat_param%rho0
      !< Hill yield function parameters
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
      nn     = mat_param%uparam(17) 
      asrate = min(one,mat_param%uparam(18)*dtime)
!
      a11 = young / (one - nu**2)
      a12 = nu*a11
!
      if (mat_param%ntable == 1) then
        itable => mat_param%table(1)
        ndim   = itable%ndim
      end if
!
      h(1:nel)      = zero
      dpla(1:nel)   = zero
      dpzz(1:nel)   = zero
      et(1:nel)     = one
      epsp(1:nel)   = uvar(1:nel,1)  ! filtered plastic strain rate from previous time step
      soundsp(1:nel)= sqrt(a11/rho0)
!---------------------------------------------------------------------
      ! element deletion condition
      do i=1,nel
        if (off(i) < one)  off(i) = four_over_5*off(i)
        if (off(i) < em01) off(i) = zero
      enddo 
!---------------------------------------------------------------------
      ! computing yield stress
      if (mat_param%ntable == 0) then     ! analytical yield formulation
        if (cc > zero)  then
          cowp(1:nel) = one + (epsp(1:nel)/cc)**cp
        else
          cowp(1:nel) = one
        end if
        do i = 1,nel
          yld(i) = sigy * cowp(i)                                         &
                 + qr1*(one - exp(-cr1*pla(i)))                           &
                 + qr2*(one - exp(-cr2*pla(i)))                           &
                 + qx1*(one - exp(-cx1*pla(i)))                           &
                 + qx2*(one - exp(-cx2*pla(i))) 
          h(i)   = qr1*cr1*exp(-cr1*pla(i)) + qr2*cr2*exp(-cr2*pla(i))    &
                 + qx1*cx1*exp(-cx1*pla(i)) + qx2*cx2*exp(-cx2*pla(i))
        enddo
      else                               ! tabulated yield function
        if (ndim == 1) then
          xvec1(1:nel,1) = pla(1:nel)
          call table_mat_vinterp(itable,nel,nel,vartmp,xvec1,yld,h)
        else if (ndim == 2) then
          xvec2(1:nel,1) = pla (1:nel)
          xvec2(1:nel,2) = epsp(1:nel)
          call table_mat_vinterp(itable,nel,nel,vartmp,xvec2,yld,h)
        end if       
      endif 
!      
      !========================================================================
      ! trial stress tensor 
      !========================================================================       
      do i=1,nel 
!    
        ! computation of the trial stress tensor  
        signxx(i) = sigoxx(i) + a11*depsxx(i) + a12*depsyy(i)
        signyy(i) = sigoyy(i) + a12*depsxx(i) + a11*depsyy(i)
        signxy(i) = sigoxy(i) + shear*depsxy(i)
        signyz(i) = sigoyz(i) + shf(i)*shear*depsyz(i)
        signzx(i) = sigozx(i) + shf(i)*shear*depszx(i)
!
        ! hill equivalent stress
        sighl(i) = (ff + hh)*signyy(i)**2 + (gg + hh)*signxx(i)**2        &
                 - two*hh*signxx(i)*signyy(i) + two*nn*signxy(i)**2        
        sighl(i) = sqrt(max(zero,sighl(i)))
      enddo 
!
      !====================================================================
      ! - check yield criterion for all elements
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
            ! 1 - computation of dphi_dsig the normal to the yield surface
          !------------------------------------------------------------- 
          seq = max(sighl(i),em20)
          normxx = (gg*signxx(i) + hh*(signxx(i)-signyy(i))) / seq
          normyy = (ff*signyy(i) + hh*(signyy(i)-signxx(i))) / seq
          normxy = two*nn*signxy(i) / seq
!          
          ! 2 - computation of dphi_dlambda
          !---------------------------------------------------------
!        
          !   a) derivative with respect stress increments tensor dsig
          !   --------------------------------------------------------
          dfdsig2 = normxx * (a11*normxx + a12*normyy)                          &
                  + normyy * (a12*normxx + a11*normyy)                          &
                  + normxy * normxy * shear         
!
          !   b) derivatives with respect to plastic strain p 
          !   ------------------------------------------------  
          !     ii) derivative of dpla with respect to dlam
          !     -------------------------------------------   
          sig_dfdsig   = signxx(i)*normxx + signyy(i)*normyy + signxy(i)*normxy          
          dpla_dlam(i) = sig_dfdsig / max(yld(i),em20)
!
          ! 3 - computation of plastic multiplier and variables update
          !----------------------------------------------------------
!          
          ! derivative of phi with respect to dlam
          dphi_dlam(i) = - dfdsig2 - h(i)*dpla_dlam(i)
          dphi_dlam(i) = sign(max(abs(dphi_dlam(i)),em20) ,dphi_dlam(i))
!          
          ! computation of the plastic multiplier
          dlam = -phi(i)/dphi_dlam(i)
!          
          ! plastic strains tensor update
          dpxx(i) = dlam * normxx
          dpyy(i) = dlam * normyy
          dpxy(i) = dlam * normxy
!          
          ! elasto-plastic stresses update   
          signxx(i) = signxx(i) - (a11*dpxx(i) + a12*dpyy(i))
          signyy(i) = signyy(i) - (a12*dpxx(i) + a11*dpyy(i))
          signxy(i) = signxy(i) - dpxy(i)*shear
!          
          ! cumulated plastic strain and strain rate update           
          ddep    = dlam*dpla_dlam(i)
          dpla(i) = max(zero, dpla(i) + ddep)
          pla(i)  = pla(i) + ddep   
!
          ! update hill equivalent stress          
          sighl(i) = (ff+hh)*signyy(i)**2 + (gg+hh)*signxx(i)**2            &
                   - two*hh*signxx(i)*signyy(i) + two*nn*signxy(i)**2    
          sighl(i) = sqrt(max(zero,sighl(i)))   
!      
          ! transverse strain update
          dpzz(i) = dpzz(i) - (dpxx(i)+dpyy(i)) 
!
          ! if the continuous hardening yield stress is chosen
          if (mat_param%ntable == 0) then
            ! update the hardening yield stress
            yld(i) = sigy * cowp(i)                                         &
                   + qr1*(one - exp(-cr1*pla(i)))                           &
                   + qr2*(one - exp(-cr2*pla(i)))                           &
                   + qx1*(one - exp(-cx1*pla(i)))                           &
                   + qx2*(one - exp(-cx2*pla(i))) 
            h(i)   = qr1*cr1*exp(-cr1*pla(i)) + qr2*cr2*exp(-cr2*pla(i))    &
                   + qx1*cx1*exp(-cx1*pla(i)) + qx2*cx2*exp(-cx2*pla(i))
!
            ! update yield criterion
            phi(i) = sighl(i) - yld(i)
          endif
        enddo     ! nel
        ! end of the loop over the yielding elements
!
        ! if the tabulated yield stress is chosen
        if (mat_param%ntable == 1) then
          if (ndim == 1) then
            xvec1(1:nel,1) = pla(1:nel)
            call table_mat_vinterp(itable,nel,nel,vartmp,xvec1,yld,h)
          else if (ndim == 2) then
            xvec2(1:nel,1) = pla (1:nel)
            xvec2(1:nel,2) = epsp(1:nel)
            call table_mat_vinterp(itable,nel,nel,vartmp,xvec2,yld,h)
          end if       
          phi(1:nel) = sighl(1:nel) - yld(1:nel)
        endif
!
      enddo   ! end of the loop over the iterations 
      !===================================================================
      ! - end of plastic correction with cutting plane algorithm
      !===================================================================                  
!    
      ! update and store new internal variables
      do i=1,nel
        ! coefficient for hourglass
        if (dpla(i)>zero) then 
          hardm(i) = h(i)
          et(i)    = h(i) / (h(i) + young)
        else
          hardm(i) = zero
          et(i)    = one
        endif
        ! computation of the thickness variation 
        deelzz = -(nu/young)*(signxx(i)-sigoxx(i)) -(nu/young)*(signyy(i)-sigoyy(i)) 
        dezz   = deelzz + dpzz(i)
        thk(i) = thk(i) + dezz*thkly(i)*off(i)  
        ! plastic strain-rate filtering
        dpdt    = dpla(i) / dtime
        epsp(i) = asrate * dpdt + (one - asrate) * uvar(i,1)
        uvar(i,1) = max(cc, epsp(i))  ! strain rate effect below static limit is ignored
      enddo 
!-----------
      return 
      end subroutine sigeps128c
!-----------
      end module sigeps128c_mod

