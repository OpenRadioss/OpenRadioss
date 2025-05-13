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
      !||    sigeps134s_mod   ../engine/source/materials/mat/mat134/sigeps134s.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw            ../engine/source/materials/mat_share/mulaw.F90
      !||====================================================================
      module sigeps134s_mod
      contains


      !||====================================================================
      !||    sigeps134s         ../engine/source/materials/mat/mat134/sigeps134s.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw              ../engine/source/materials/mat_share/mulaw.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||====================================================================
       subroutine sigeps134s(mat_param  ,                                       &
           nel      ,nuvar     ,uvar     ,rho      ,timestep ,                  &
           depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,         &
           sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,         &
           signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,         &
           soundsp  ,off      )
!
! =================================================================================
! \brief orthotropic hill material with plastic strain rate dependancy for solids

! =================================================================================
!   m o d u l e s
!-----------------------------------------------
      use matparam_def_mod
      use constant_mod ,only : pi,zero,one,half,third,two,three,four,em20
      use constant_mod ,only : four_over_3,four_over_5,em10
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
      my_real ,intent(in) :: timestep                      !< time step
      ! my_real ,dimension(nel)     ,intent(in)    :: rho0  !< reference density
      my_real ,dimension(nel)     ,intent(in)    :: rho   !< density 
      my_real ,dimension(nel)     ,intent(in)    :: depsxx !<  strain increment component in direction xx 
      my_real ,dimension(nel)     ,intent(in)    :: depsyy !< strain increment component in direction yy
      my_real ,dimension(nel)     ,intent(in)    :: depszz !< strain increment component in direction zz
      my_real ,dimension(nel)     ,intent(in)    :: depsxy !< strain increment component  in xy direction
      my_real ,dimension(nel)     ,intent(in)    :: depsyz !<   
      my_real ,dimension(nel)     ,intent(in)    :: depszx !< strain rate component  in zx direction
      my_real ,dimension(nel)     ,intent(in)    :: sigoxx !< output stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigoyy !< output stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigozz !< output stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigoxy !< output stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigoyz !< output stress component
      my_real ,dimension(nel)     ,intent(in)    :: sigozx !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signxx !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signyy !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signzz !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signxy !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signyz !< output stress component
      my_real ,dimension(nel)     ,intent(out)   :: signzx !< output stress component
      !
      !
      my_real ,dimension(nel)     ,intent(inout) :: off    !< element activation coefficient
      my_real ,dimension(nel)     ,intent(out)   :: soundsp!< sound speed
      my_real ,dimension(nel,nuvar)   ,intent(inout) :: uvar      !< state variables
      type (matparam_struct_)         ,intent(in)    :: mat_param !< material parameter structure
      target :: mat_param
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i
      my_real ,dimension(nel) :: aa,bb,e1t,v2t
      my_real ,dimension(6) :: sigv_old,sigv,dsig,dsige
      my_real ::       &
        young, shear, bulk, nu, rho0, nu_shear, e1, n1, e2, v2, n2,   &
        r, dtime, nu_c  ,beta, cc      
!===============================================================================    
      dtime  = max(timestep, em20)
      young  = mat_param%young
      shear  = mat_param%shear
      bulk   = mat_param%bulk
      nu     = mat_param%nu
      rho0   = mat_param%rho0
      nu_c   = one/(one + nu)/(one - two*nu)
      nu_shear    = half/(one + nu) 
!
      e1    = mat_param%uparam(1)  
      n1    = mat_param%uparam(2)  
      e2    = mat_param%uparam(4)  
      v2    = mat_param%uparam(5)  
      n2    = mat_param%uparam(6) 
      do i=1,nel
        r = max(em20, rho0/rho(i))       ! relative volume v/v0
        e1t(i) = e1*exp(-n1*log(r))      ! e1t(i) = e1*(r**(-n1))  ! elastic modulus
        cc = abs(one - r)     ! relative volume change
        v2t(i) = zero
        if(cc >= em10) v2t(i) = two*v2*exp(n2*log(cc))         !v2*cc**n2 
        beta = e2/max(em20, v2t(i))   ! beta = e2/v2t(i)
        !
        aa(i) = exp(-beta*dtime)
        bb(i) = e2*exp(-beta*half*dtime)
      enddo 
      soundsp(1:nel) = sqrt((bulk + four_over_3*shear) / rho0)     ! sound-speed
!---------------------------------------------------------------------
      !  elastic and viscous stress
       do i=1,nel
        ! incremental formulation for elastic and viscouss stress 
        dsige(1) = e1t(i)*depsxx(i) 
        dsige(2) = e1t(i)*depsyy(i)    
        dsige(3) = e1t(i)*depszz(i)
        dsige(4) = e1t(i)*depsxy(i)
        dsige(5) = e1t(i)*depsyz(i)
        dsige(6) = e1t(i)*depszx(i)
       ! old viscous stress
        sigv_old(1) = uvar(i,1)
        sigv_old(2) = uvar(i,2)
        sigv_old(3) = uvar(i,3)
        sigv_old(4) = uvar(i,4)
        sigv_old(5) = uvar(i,5)
        sigv_old(6) = uvar(i,6)
        ! total viscous stress
        !  time integration using mid-point rule scheme for integral of convolution 
        ! sigv = aa*sigv_old + bb*deps *
        ! we resolve d(sigv)/dt + beta*sigv = E2*epsp
        sigv(1)  = aa(i)*sigv_old(1) + bb(i)*depsxx(i) 
        sigv(2)  = aa(i)*sigv_old(2) + bb(i)*depsyy(i)  
        sigv(3)  = aa(i)*sigv_old(3) + bb(i)*depszz(i)  
        sigv(4)  = aa(i)*sigv_old(4) + bb(i)*depsxy(i) 
        sigv(5)  = aa(i)*sigv_old(5) + bb(i)*depsyz(i)  
        sigv(6)  = aa(i)*sigv_old(6) + bb(i)*depszx(i)  
        ! incremenation of  stress
         dsig(1:6) = dsige(1:6) + sigv(1:6) - sigv_old(1:6)
        ! coupling stress
         signxx(i) = sigoxx(i) + nu_c*((one - nu)*dsig(1) + nu*(dsig(2) + dsig(3)))
         signyy(i) = sigoyy(i) + nu_c*((one - nu)*dsig(2) + nu*(dsig(1) + dsig(3)))
         signzz(i) = sigozz(i) + nu_c*((one - nu)*dsig(3) + nu*(dsig(1) + dsig(2)))
         signxy(i) = sigoxy(i) + dsig(4)*nu_shear
         signyz(i) = sigoyz(i) + dsig(5)*nu_shear
         signzx(i) = sigozx(i) + dsig(6)*nu_shear
        ! viscous variable
        uvar(i,1) = sigv(1) 
        uvar(i,2) = sigv(2)
        uvar(i,3) = sigv(3) 
        uvar(i,4) = sigv(4) 
        uvar(i,5) = sigv(5) 
        uvar(i,6) = sigv(6) 
      enddo 
      !
       do i=1,nel
        signxx(i) = signxx(i)*off(i)
        signyy(i) = signyy(i)*off(i)
        signzz(i) = signzz(i)*off(i)
        signxy(i) = signxy(i)*off(i)
        signyz(i) = signyz(i)*off(i)
        signzx(i) = signzx(i)*off(i)
      enddo  
!-----------
      return
      end
!-----------
      end module sigeps134s_mod
