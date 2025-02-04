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
           sighl    ,yld      ,et       ,pla      ,dpla     ,epsp     ,         &
           soundsp  ,off      )
!
! =================================================================================
! \brief orthotropic hill material with plastic strain rate dependancy for solids

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
      my_real ,dimension(nel)     ,intent(out)   :: dpla   !< plastic strain increment
      my_real ,dimension(nel)     ,intent(out)   :: et     !< tangent module
      my_real ,dimension(nel)     ,intent(out)   :: sighl  !< Hill equivalent stress 
      my_real ,dimension(nel)     ,intent(out)   :: yld    !< yield stress 
      my_real ,dimension(nel)     ,intent(out)   :: soundsp!< sound speed
      my_real ,dimension(nel)     ,intent(out)   :: epsp   !< plastic strain rate
      my_real ,dimension(nel,nuvar)   ,intent(inout) :: uvar      !< state variables
      integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp    !< temporary internal variables
      type (matparam_struct_)         ,intent(in)    :: mat_param !< material parameter structure
      target :: mat_param
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,ii,iter,niter,nindx,ifunc,ndim
      integer ,dimension(nel) :: indx
      my_real :: dlam,ddep,sig_dfdsig,dfdsig2,dpdt,seq                                 
      my_real :: normxx,normyy,normzz,normxy,normyz,normzx
      my_real :: cc,cp,asrate,dtime       
      my_real :: qr1,qr2,qx1,qx2,cr1,cr2,cx1,cx2
      my_real :: ff,gg,hh,ll,mm,nn
      my_real :: sigy,young,shear,bulk,nu,rho0
      my_real :: cii,cij
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

      cij = three*bulk*nu/(one+nu)
      cii = cij + two*shear

      if (mat_param%ntable == 1) then
        itable => mat_param%table(1)
        ndim   = itable%ndim
      end if
!
      et(1:nel)   = one
      h(1:nel)    = zero
      dpla(1:nel) = zero
      epsp(1:nel) = uvar(1:nel,1)  ! filtered plastic strain rate from previous time step
      soundsp(1:nel) = sqrt((bulk + four_over_3*shear) / rho0)     ! sound-speed
!---------------------------------------------------------------------
      ! element deletion condition
      do i=1,nel
        if (off(i) < one)  off(i) = four_over_5*off(i)
        if (off(i) < em01) off(i) = zero
      enddo 
!---------------------------------------------------------------------
      ! computing yield stress
!
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
          normxx = (gg*(signxx(i)-signzz(i)) + hh*(signxx(i)-signyy(i))) / seq
          normyy = (ff*(signyy(i)-signzz(i)) + hh*(signyy(i)-signxx(i))) / seq
          normzz = (ff*(signzz(i)-signyy(i)) + gg*(signzz(i)-signxx(i))) / seq
          normxy = two*nn*signxy(i) / seq
          normyz = two*ll*signyz(i) / seq
          normzx = two*mm*signzx(i) / seq
!          
          ! 2 - computation of dphi_dlambda
          !---------------------------------------------------------
!        
          !   a) derivative with respect stress increments tensor dsig
          !   --------------------------------------------------------
          dfdsig2 = normxx * (cii * normxx + cij * normyy + cij * normzz )      &
                  + normyy * (cij * normxx + cii * normyy + cij * normzz )      &
                  + normzz * (cij * normxx + cij * normyy + cii * normzz )      &
                  + normxy * normxy * shear                                     &
                  + normyz * normyz * shear                                     &
                  + normzx * normzx * shear      
!
          !   b) derivatives with respect to plastic strain p 
          !   ------------------------------------------------  
!          
          !     i) derivative of the yield stress with respect to plastic strain dyld / dpla
          !     ----------------------------------------------------------------------------
          ! already computed 
!
          !     ii) derivative of dpla with respect to dlam
          !     -------------------------------------------   
          sig_dfdsig = signxx(i) * normxx                       &
                     + signyy(i) * normyy                       &
                     + signzz(i) * normzz                       &
                     + signxy(i) * normxy                       &
                     + signyz(i) * normyz                       &
                     + signzx(i) * normzx        
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
          dpzz(i) = dlam * normzz
          dpxy(i) = dlam * normxy
          dpyz(i) = dlam * normyz
          dpzx(i) = dlam * normzx  
!          
          ! elasto-plastic stresses update   
          signxx(i) = signxx(i) - (cii*dpxx(i) + cij*dpyy(i) + cij*dpzz(i))
          signyy(i) = signyy(i) - (cij*dpxx(i) + cii*dpyy(i) + cij*dpzz(i))
          signzz(i) = signzz(i) - (cij*dpxx(i) + cij*dpyy(i) + cii*dpzz(i))
          signxy(i) = signxy(i) - dpxy(i)*shear
          signyz(i) = signyz(i) - dpyz(i)*shear
          signzx(i) = signzx(i) - dpzx(i)*shear
!          
          ! cumulated plastic strain and strain rate update           
          ddep    = dlam*dpla_dlam(i)
          dpla(i) = max(zero, dpla(i) + ddep)
          pla(i)  = pla(i) + ddep   
!
          ! update hill equivalent stress          
          sighl(i) = ff*(signyy(i) - signzz(i))**2 + gg*(signzz(i) - signxx(i))**2   &
                   + hh*(signxx(i) - signyy(i))**2 + two*ll*signyz(i)**2             &
                   + two*mm*signzx(i)**2 + two*nn*signxy(i)**2                              
          sighl(i) = sqrt(max(sighl(i),zero)) 
!                                                                                          
          if (mat_param%ntable == 0) then    ! analytical yield formulation                                                           
            yld(i) = sigy * cowp(i)                                         &
                   + qr1*(one - exp(-cr1*pla(i)))                           &
                   + qr2*(one - exp(-cr2*pla(i)))                           &
                   + qx1*(one - exp(-cx1*pla(i)))                           &
                   + qx2*(one - exp(-cx2*pla(i))) 
            h(i)   = qr1*cr1*exp(-cr1*pla(i)) + qr2*cr2*exp(-cr2*pla(i))    &
                   + qx1*cx1*exp(-cx1*pla(i)) + qx2*cx2*exp(-cx2*pla(i))
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
      ! hourglass stiffness ratio
      do i=1,nel        
        if (dpla(i) > zero) then 
          et(i) = h(i) / (h(i) + young)
        else
          et(i) = one
        endif
      enddo 
      ! plastic strain-rate filtering
      do i=1,nel        
        dpdt    = dpla(i) / dtime
        epsp(i) = asrate * dpdt + (one - asrate) * uvar(i,1)
        uvar(i,1) = max(cc, epsp(i))  ! strain rate effect below static limit is ignored
      enddo 
!-----------
      return
      end
!-----------
      end module sigeps128s_mod
