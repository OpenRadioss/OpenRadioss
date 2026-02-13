!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!

!||====================================================================
!||    sigeps128c_mod   ../engine/source/materials/mat/mat128/sigeps128c.F90
!||--- called by ------------------------------------------------------
!||    mulawc           ../engine/source/materials/mat_share/mulawc.F90
!||====================================================================
! \brief orthotropic hill material with plastic strain rate dependency for shells

! ==================================================================================================
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
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table4d_mod             ../common_source/modules/table4d_mod.F
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
        subroutine sigeps128c(mat_param,                              &
          nel      ,nvartmp  ,vartmp   ,timestep ,                    &
          depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,          &
          sigoxx   ,sigoyy   ,sigoxy   ,sigoyz   ,sigozx   ,          &
          signxx   ,signyy   ,signxy   ,signyz   ,signzx   ,          &
          soundsp  ,thk      ,pla      ,dpla     ,epsd     ,          &
          off      ,et       ,thkly    ,shf      ,yld      ,          &
          hardm    ,sighl    ,l_sigb   ,sigb     )
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use matparam_def_mod
          use constant_mod ,only : pi,zero,one,half,third,two_third,two,three,four
          use constant_mod ,only : four_over_3,four_over_5
          use constant_mod ,only : em01,em10,em15,em20,ep20
          use table4d_mod
          use table_mat_vinterp_mod
          use precision_mod, only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Arguments
! --------------------------------------------------------------------------------------------------
          integer ,intent(in) :: nel                           !< element group size
          integer ,intent(in) :: nvartmp                       !< number of temporary internal variables
          integer ,intent(in) :: l_sigb                        !< size of backstress tensor
          real(kind=WP) ,intent(in) :: timestep                      !< time step
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: depsxx !< deformation increment component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: depsyy !< deformation increment component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: depsxy !< deformation increment component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: depsyz !< deformation increment component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: depszx !< deformation increment component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: thkly  !< relative layer thickness
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: shf    !< transverse shear factor for shells
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: sigoxx !< input  stress component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: sigoyy !< input  stress component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: sigoxy !< input  stress component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: sigoyz !< input  stress component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: sigozx !< input  stress component
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: off    !< element activation coefficient
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: pla    !< plastic strain
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: thk    !< element thickness
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: yld    !< yield stress
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: signxx !< output stress component
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: signyy !< output stress component
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: signxy !< output stress component
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: signyz !< output stress component
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: signzx !< output stress component
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: dpla   !< plastic strain increment
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: sighl  !< hill equivalent stress
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: soundsp!< sound speed
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: hardm  !< tangent module
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: epsd   !< local plastic strain rate
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: et     !< hourglass stiness factor
          real(kind=WP) ,dimension(nel,l_sigb)  ,intent(inout) :: sigb      !< backstress tensor
          integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp    !< temporary internal variables
          type (matparam_struct_) ,intent(in) :: mat_param !< material parameter structure
! ----------------------------------------------------------------------------------------------------------------------
!         Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,ii,iter,nindx,ndim
          integer, parameter :: niter = 3
          integer ,dimension(nel) :: indx
          real(kind=WP) :: sigy,young,shear,bulk,c11,c22,rho0
          real(kind=WP) :: nu,nu21
          real(kind=WP) :: ff,gg,hh,nn
          real(kind=WP) :: a01,a02,a03,a12
          real(kind=WP) :: ddep,seq,sig_dfdsig,dsig_dlam,dpdt
          real(kind=WP) :: dsigbxx_dlam,dsigbyy_dlam,dsigbxy_dlam,dsigb_dlam
          real(kind=WP) :: cc,cp,asrate,fisokin,dtime
          real(kind=WP) :: qr1,qr2,qx1,qx2,cr1,cr2,cx1,cx2
          real(kind=WP) :: dezz
          real(kind=WP) ,dimension(nel)   :: normxx,normyy,normxy
          real(kind=WP) ,dimension(nel)   :: cowp              !< Cowper-Symonds strain rate factor
          real(kind=WP) ,dimension(nel)   :: h,h0,hk           !< hardening tangent stiffness
          real(kind=WP) ,dimension(nel)   :: phi               !< plastic yield criterion
          real(kind=WP) ,dimension(nel)   :: yld0              !< initial yield stress
          real(kind=WP) ,dimension(nel)   :: dlam,dphi_dlam,dpla_dlam
          real(kind=WP) ,dimension(nel)   :: dpxx,dpyy,dpxy,dpzz
          real(kind=WP) ,dimension(nel,1) :: xvec1
          real(kind=WP) ,dimension(nel,2) :: xvec2
! ==================================================================================================
          dtime = max(timestep, em20)
          rho0  = mat_param%rho0
          young = mat_param%young
          shear = mat_param%shear
          bulk  = mat_param%bulk
          nu    = mat_param%nu
          c11   = young / (one - nu**2)
          c22   = nu*c11
          !< Hill yield function parameters
          sigy  = mat_param%uparam(1)
          qr1   = mat_param%uparam(2)
          cr1   = mat_param%uparam(3)
          qr2   = mat_param%uparam(4)
          cr2   = mat_param%uparam(5)
          qx1   = mat_param%uparam(6)
          cx1   = mat_param%uparam(7)
          qx2   = mat_param%uparam(8)
          cx2   = mat_param%uparam(9)
          cc    = mat_param%uparam(10)
          cp    = mat_param%uparam(11)
          ff    = mat_param%uparam(12)
          gg    = mat_param%uparam(13)
          hh    = mat_param%uparam(14)
          nn    = mat_param%uparam(17)
          asrate  = min(one,mat_param%uparam(18)*dtime)
          fisokin = mat_param%uparam(19)
!
          a01   = gg + hh
          a02   = ff + hh
          a03   = two*hh
          a12   = two*nn
          hardm(1:nel)  = zero
          dpla(1:nel)   = zero
          dpzz(1:nel)   = zero
          et(1:nel)     = one
          cowp(1:nel)   = one
          yld0(1:nel)   = sigy
          soundsp(1:nel)= sqrt(c11/rho0)
          if (mat_param%ntable == 1) then
            ndim = mat_param%table(1)%ndim
          else
            ndim = 0
          end if
! ----------------------------------------------------------------------------------------------------------------------
          !< element deletion condition
          do i=1,nel
            if (off(i) < one)  off(i) = four_over_5*off(i)
            if (off(i) < em01) off(i) = zero
          enddo
          if (mat_param%ntable == 0) then
            if (cc > zero) then         ! Cowper-Simonds strain rate factor
              cowp(1:nel) = one + (epsd(1:nel)/cc)**cp
            end if
          end if
! ----------------------------------------------------------------------------------------------------------------------
          if (fisokin > zero) then
            !< Save the initial yield stress in case of kinematic hardening
            if (mat_param%ntable == 0) then     ! analytical hardening equation
                yld0(1:nel) = sigy * cowp(1:nel)
            else if (ndim == 2) then            ! tabulated hardening curve with strain rate
              xvec2(1:nel,1) = zero
              xvec2(1:nel,2) = epsd(1:nel)
              call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld0,h0)
            end if
          endif
! --------------------------------------------------------------------------------------------------
          !< trial stress tensor
          do i = 1,nel
            signxx(i) = sigoxx(i) + c11 * depsxx(i) + c22 * depsyy(i)
            signyy(i) = sigoyy(i) + c22 * depsxx(i) + c11 * depsyy(i)
            signxy(i) = sigoxy(i) + shear * depsxy(i)
            signyz(i) = sigoyz(i) + shear * depsyz(i) * shf(i)
            signzx(i) = sigozx(i) + shear * depszx(i) * shf(i)
          end do
!
          if (fisokin > zero) then           !< Remove backstress from stress tensor
            signxx(1:nel) = signxx(1:nel) - sigb(1:nel,1)
            signyy(1:nel) = signyy(1:nel) - sigb(1:nel,2)
            signxy(1:nel) = signxy(1:nel) - sigb(1:nel,3)
          endif
! --------------------------------------------------------------------------------------------------
          ! computing yield stress
! ----------------------------------------------------------------------------------------------------------------------
          if (mat_param%ntable == 0) then    ! analytical yield formulation
            do i = 1,nel
              yld(i) = sigy + qr1*(one - exp(-cr1*pla(i)))                    &
                            + qr2*(one - exp(-cr2*pla(i)))                    &
                            + qx1*(one - exp(-cx1*pla(i)))                    &
                            + qx2*(one - exp(-cx2*pla(i)))                
              h(i)   = qr1*cr1*exp(-cr1*pla(i)) + qr2*cr2*exp(-cr2*pla(i))    &
                     + qx1*cx1*exp(-cx1*pla(i)) + qx2*cx2*exp(-cx2*pla(i))
              h(i)   = h(i)   * cowp(i)
              yld(i) = yld(i) * cowp(i)
            enddo
          else                                ! tabulated yield function
            if (ndim == 1) then
              xvec1(1:nel,1) = pla(1:nel)
              call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec1,yld,h)
            else if (ndim == 2) then
              xvec2(1:nel,1) = pla (1:nel)
              xvec2(1:nel,2) = epsd(1:nel)
              call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld,h)
            end if
          endif
!
          h(1:nel)  = max(zero, h(1:nel))
          hk(1:nel) = h(1:nel) * fisokin
          if (fisokin > zero) then
            yld(1:nel) = (one - fisokin)*yld(1:nel) + fisokin*yld0(1:nel)
          endif
! --------------------------------------------------------------------------------------------------
          ! - check yield criterion for all elements
! --------------------------------------------------------------------------------------------------
          ! equivalent Hill stress
          do i=1,nel
            sighl(i) = a01*signxx(i)**2 + a02*signyy(i)**2 - a03*signxx(i)*signyy(i)       &
                     + a12*signxy(i)**2
            sighl(i) = sqrt(max(zero,sighl(i)))
          enddo
!
          phi(1:nel) = sighl(1:nel) - yld(1:nel)
!
          nindx = 0
          do i = 1,nel
            if (phi(i) > zero .and. off(i) == one) then
              nindx = nindx + 1
              indx(nindx) = i
            end if
          end do
! --------------------------------------------------------------------------------------------------
          ! plastic correction
! --------------------------------------------------------------------------------------------------
          if (nindx > 0) then
            do iter = 1,niter
#include "vectorize.inc"
              do ii=1,nindx
                i = indx(ii)
                ! phi                         : current value of yield function (known)
                ! norm_ij = dphi/dsig         : normal to the yield surface
                ! dphi_dlambda                : derivative of phi with respect to dlambda
                ! dlambda = -phi/dphi_dlambda : plastic multiplier increment (Newton)
                !-------------------------------------------------------------
                seq = max(sighl(i),em20)
                normxx(i) = (a01*signxx(i) - half*a03*signyy(i)) / seq
                normyy(i) = (a02*signyy(i) - half*a03*signxx(i)) / seq
                normxy(i) =  a12*signxy(i) / seq
!
                !< dsig/dlam = (dphi/dsig : dsig/dlam) = (Normal : dsig/dlam)
                !   --------------------------------------------------------
                dsig_dlam = normxx(i) * (c11*normxx(i) + c22*normyy(i))                &
                          + normyy(i) * (c22*normxx(i) + c11*normyy(i))                &
                          + normxy(i) * normxy(i) * shear
!
                !< Derivative of backstress tensor wrt lambda
                dsigbxx_dlam = two_third*hk(i)*(two*normxx(i) + normyy(i))
                dsigbyy_dlam = two_third*hk(i)*(two*normyy(i) + normxx(i))
                dsigbxy_dlam = two_third*hk(i)*normxy(i)
                dsigb_dlam   = - normxx(i) * dsigbxx_dlam          &
                               - normyy(i) * dsigbyy_dlam          &
                               - normxy(i) * dsigbxy_dlam          

                !<  derivative of dpla over lambda, dpla = lam * dphi/dsig
                !<  dpla/dlam = (sig : dphi/dsig) / yld
                sig_dfdsig   = signxx(i)*normxx(i) + signyy(i)*normyy(i) + signxy(i)*normxy(i)
                dpla_dlam(i) = sig_dfdsig / max(yld(i),em20)
!
                ! derivative of phi with respect to dlam
                
                dphi_dlam(i) = -dsig_dlam - h(i)*(one-fisokin)*dpla_dlam(i) + dsigb_dlam
                dphi_dlam(i) = sign(max(abs(dphi_dlam(i)),em20) ,dphi_dlam(i))
!
                !< plastic multiplier and plastic strain increment
                dlam(i) = -phi(i) / dphi_dlam(i)
                ddep    = dlam(i)*dpla_dlam(i)
                dpla(i) = max(zero, dpla(i) + ddep)
                pla(i)  = pla(i) + ddep   !
              end do   ! ii = 1,nindx
!
              !< update yield stress with current plastic strain
              if (mat_param%ntable == 0) then       ! analytical hardening equation
                do ii=1,nindx
                  i = indx(ii)
                  yld(i) = sigy + qr1*(one - exp(-cr1*pla(i)))                    &
                                + qr2*(one - exp(-cr2*pla(i)))                    &
                                + qx1*(one - exp(-cx1*pla(i)))                    &
                                + qx2*(one - exp(-cx2*pla(i)))                    
                  h(i)   = qr1*cr1*exp(-cr1*pla(i)) + qr2*cr2*exp(-cr2*pla(i))    &
                         + qx1*cx1*exp(-cx1*pla(i)) + qx2*cx2*exp(-cx2*pla(i))
                  h(i)   = h(i)   * cowp(i)
                  yld(i) = yld(i) * cowp(i)
                end do
              else                               ! tabulated yield with strain rate
                if (ndim == 1) then
                  xvec1(1:nel,1) = pla(1:nel)
                  call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec1,yld,h)
                else if (ndim == 2) then
                  xvec2(1:nel,1) = pla (1:nel)
                  xvec2(1:nel,2) = epsd(1:nel)
                  call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld,h)
                end if
              endif
              if (fisokin > 0) then
                hk (1:nel) = fisokin * h(1:nel)
                yld(1:nel) = (one - fisokin)*yld(1:nel) + fisokin*yld0(1:nel)
              end if
!
              ! elasto-plastic stresses update
              do ii=1,nindx
                i = indx(ii)
                !< tensor of plastic strain increment
                dpxx(i) = dlam(i) * normxx(i)
                dpyy(i) = dlam(i) * normyy(i)
                dpxy(i) = dlam(i) * normxy(i)
                signxx(i) = signxx(i) - (c11*dpxx(i) + c22*dpyy(i))
                signyy(i) = signyy(i) - (c22*dpxx(i) + c11*dpyy(i))
                signxy(i) = signxy(i) - dpxy(i)*shear
              enddo
!
              if (fisokin > zero) then   !< incremental backstress update
                do ii=1,nindx
                  i = indx(ii)
                  signxx(i) = signxx(i) + sigb(i,1)
                  signyy(i) = signyy(i) + sigb(i,2)
                  signxy(i) = signxy(i) + sigb(i,3)
                  dsigbxx_dlam = two_third*hk(i)*(two*normxx(i) + normyy(i))
                  dsigbyy_dlam = two_third*hk(i)*(two*normyy(i) + normxx(i))
                  dsigbxy_dlam = two_third*hk(i)*normxy(i)
                  sigb(i,1) = sigb(i,1) + dsigbxx_dlam*dlam(i)
                  sigb(i,2) = sigb(i,2) + dsigbyy_dlam*dlam(i)
                  sigb(i,3) = sigb(i,3) + dsigbxy_dlam*dlam(i)
                  signxx(i) = signxx(i) - sigb(i,1)
                  signyy(i) = signyy(i) - sigb(i,2)
                  signxy(i) = signxy(i) - sigb(i,3)
                enddo
              endif
!
              ! update hill equivalent stress
              do ii=1,nindx
                i = indx(ii)
                sighl(i) = (gg+hh)*signxx(i)**2 + (ff+hh)*signyy(i)**2            &
                         - two*hh*signxx(i)*signyy(i) + two*nn*signxy(i)**2
                sighl(i) = sqrt(max(zero,sighl(i)))
!
                phi(i)  = sighl(i) - yld(i)
                dpzz(i) = dpzz(i) - (dpxx(i)+dpyy(i))  ! transverse strain update
                et(i)   = h(i) / (h(i) + young)        ! tangent stiffness coefficient for hourglass
              enddo
            enddo   ! end of the loop over the iterations
          end if    ! nindx > 0
!===================================================================
!         end of plastic correction with cutting plane algorithm
!===================================================================
          !< Add backstress to elasto-plastic stress after projection (if kinematic hardening)
          if (fisokin > zero) then
            signxx(1:nel) = signxx(1:nel) + sigb(1:nel,1)
            signyy(1:nel) = signyy(1:nel) + sigb(1:nel,2)
            signxy(1:nel) = signxy(1:nel) + sigb(1:nel,3)
          end if
!
          do i = 1, nel
            sighl(i) = a01*signxx(i)**2 + a02*signyy(i)**2 - a03*signxx(i)*signyy(i)       &
                     + a12*signxy(i)**2
            sighl(i) = sqrt(max(zero,sighl(i)))
          end do
          hardm(1:nel) = h(1:nel)
!
          do i=1,nel
            ! thickness variation
            nu21 = (one - two*nu) / (one - nu)
            dezz = -(depsxx(i)+depsyy(i)) * nu/(one-nu) + dpzz(i) * nu21
            thk(i) = thk(i) + dezz*thkly(i)*off(i)
            ! plastic strain-rate filtering
            dpdt    = dpla(i) / dtime
            epsd(i) = asrate * dpdt + (one - asrate) * epsd(i)
            epsd(i) = max(cc, epsd(i))  ! strain rate effect below static limit is ignored
          enddo
! --------------------------------------------------------------------------------------------------
          return
        end subroutine sigeps128c
! --------------------------------------------------------------------------------------------------
      end module sigeps128c_mod

