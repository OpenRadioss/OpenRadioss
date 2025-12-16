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
! ======================================================================================================================

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
!||    precision_mod           ../common_source/modules/precision_mod.F90
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
! ======================================================================================================================
! \brief orthotropic hill material with plastic strain rate dependency for solids

! ======================================================================================================================
!                 Modules
! ----------------------------------------------------------------------------------------------------------------------
          use matparam_def_mod
          use constant_mod ,only : pi,zero,one,half,third,two_third,two,three,four
          use constant_mod ,only : four_over_3,four_over_5
          use constant_mod ,only : em01,em10,em15,em20,ep20
          use table4d_mod
          use table_mat_vinterp_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                 Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer ,intent(in) :: nel                           !< element group size
          integer ,intent(in) :: nuvar                         !< number of state variables
          integer ,intent(in) :: nvartmp                       !< number of temporary internal variables
          integer ,intent(in) :: l_sigb                        !< size of backstress tensor
          real(kind=WP) ,intent(in) :: timestep                      !< time step
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: depsxx !< deformation increment component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: depsyy !< deformation increment component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: depszz !< deformation increment component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: depsxy !< deformation increment component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: depsyz !< deformation increment component
          real(kind=WP) ,dimension(nel)     ,intent(in)    :: depszx !< deformation increment component
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: sigoxx !< input  stress component
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: sigoyy !< input  stress component
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: sigozz !< input  stress component
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: sigoxy !< input  stress component
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: sigoyz !< input  stress component
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: sigozx !< input  stress component
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: signxx !< output stress component
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: signyy !< output stress component
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: signzz !< output stress component
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: signxy !< output stress component
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: signyz !< output stress component
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: signzx !< output stress component
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: off    !< element activation coefficient
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: pla    !< plastic strain
          real(kind=WP) ,dimension(nel)     ,intent(inout) :: yld    !< yield stress
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: dpla   !< plastic strain increment
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: et     !< tangent module
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: sighl  !< Hill equivalent stress
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: soundsp!< sound speed
          real(kind=WP) ,dimension(nel)     ,intent(out)   :: epsd   !< plastic strain rate
          real(kind=WP) ,dimension(nel,l_sigb)  ,intent(inout) :: sigb      !< backstress tensor
          real(kind=WP) ,dimension(nel,nuvar)   ,intent(inout) :: uvar      !< state variables
          integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp    !< temporary internal variables
          type (matparam_struct_)         ,intent(in)    :: mat_param !< material parameter structure
! ----------------------------------------------------------------------------------------------------------------------
!               Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,ii,iter,niter,nindx,ndim
          integer ,dimension(nel) :: indx
          real(kind=WP) :: dlam,ddep,sig_dfdsig,dsig_dlam,dpdt,seq
          real(kind=WP) :: cc,cp,asrate,fisokin,hkin,dtime
          real(kind=WP) :: qr1,qr2,qx1,qx2,cr1,cr2,cx1,cx2
          real(kind=WP) :: ff,gg,hh,ll,mm,nn
          real(kind=WP) :: sigy,young,shear,bulk,nu,rho0
          real(kind=WP) :: cii,cij
          real(kind=WP) ,dimension(nel)   :: cowp              !< Cowper-Symonds strain rate factor
          real(kind=WP) ,dimension(nel)   :: h,h0              !< hardening tangent stiffness
          real(kind=WP) ,dimension(nel)   :: phi               !< plastic yield criterion
          real(kind=WP) ,dimension(nel)   :: yld0              !< initial yield stress
          real(kind=WP) ,dimension(nel)   :: dphi_dlam,dpla_dlam
          real(kind=WP) ,dimension(nel)   :: dpxx,dpyy,dpzz,dpxy,dpyz,dpzx
          real(kind=WP) ,dimension(nel)   :: normxx,normyy,normzz,normxy,normyz,normzx
          real(kind=WP) ,dimension(nel,1) :: xvec1
          real(kind=WP) ,dimension(nel,2) :: xvec2
!==================================================================================================
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
!
          cij  = three*bulk*nu/(one+nu)
          cii  = cij + two*shear
          if (mat_param%ntable == 1) then
            ndim = mat_param%table(1)%ndim
          else
            ndim = 0
          end if
!
          et(1:nel)   = one
          dpla(1:nel) = zero
          epsd(1:nel) = uvar(1:nel,1)  ! filtered plastic strain rate from previous time step
          soundsp(1:nel) = sqrt((bulk + four_over_3*shear) / rho0)     ! sound-speed
! --------------------------------------------------------------------------------------------
          ! element deletion condition
          do i=1,nel
            if (off(i) < one)  off(i) = four_over_5*off(i)
            if (off(i) < em01) off(i) = zero
          enddo
!------------------------------------------
!         kinematic hardening
!------------------------------------------
          if (fisokin > zero) then
            !< Remove backstress from stress tensor
            sigoxx(1:nel) = sigoxx(1:nel) - sigb(1:nel,1)
            sigoyy(1:nel) = sigoyy(1:nel) - sigb(1:nel,2)
            sigozz(1:nel) = sigozz(1:nel) - sigb(1:nel,3)
            sigoxy(1:nel) = sigoxy(1:nel) - sigb(1:nel,4)
            sigoyz(1:nel) = sigoyz(1:nel) - sigb(1:nel,5)
            sigozx(1:nel) = sigozx(1:nel) - sigb(1:nel,6)
            ! compute initial yield stress for kinematic hardening
            if (mat_param%ntable == 0) then     ! analytical hardening equation
              yld0(1:nel) = sigy
            else                                ! tabulated hardening curve
              if (ndim == 1) then
                yld0(1:nel) = sigy
              else if (ndim == 2) then
                xvec2(1:nel,1) = zero
                xvec2(1:nel,2) = epsd(1:nel)
                call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld0,h0)
              end if
            end if
          end if
! --------------------------------------------------------------------------------------------
          !< elastic trial stress tensor
          do i=1,nel
            signxx(i) = sigoxx(i) + cii*depsxx(i) + cij*depsyy(i) + cij*depszz(i)
            signyy(i) = sigoyy(i) + cij*depsxx(i) + cii*depsyy(i) + cij*depszz(i)
            signzz(i) = sigozz(i) + cij*depsxx(i) + cij*depsyy(i) + cii*depszz(i)
            signxy(i) = sigoxy(i) + shear*depsxy(i)
            signyz(i) = sigoyz(i) + shear*depsyz(i)
            signzx(i) = sigozx(i) + shear*depszx(i)
          enddo
          ! equivalent Hill stress
          do i=1,nel
            sighl(i) = ff*(signyy(i) - signzz(i))**2                                &
              + gg*(signzz(i) - signxx(i))**2                                &
              + hh*(signxx(i) - signyy(i))**2                                &
              + two*(ll*signyz(i)**2 + mm*signzx(i)**2 + nn*signxy(i)**2)
            sighl(i) = sqrt(max(zero,sighl(i)))
          enddo
! -------------------------------------------------------------------------------
          ! compute current yield stress
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
              h(i)   = h(i)   * (one + cowp(i))
              yld(i) = yld(i) * (one + cowp(i))
            enddo
          else                     ! tabulated yield function, includes strain rate
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
          if (fisokin > zero) then
            yld(1:nel) = (one - fisokin)*yld(1:nel) + fisokin*yld0(1:nel)
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
                ! phi                         : current value of yield function (known)
                ! norm_ij = dphi/dsig         : normal to the yield surface
                ! dphi_dlambda                : derivative of phi with respect to dlambda
                ! dlambda = -phi/dphi_dlambda : plastic multiplier increment (Newton)
                !-------------------------------------------------------------
                seq = max(sighl(i),em20)
                normxx(i) = (gg*(signxx(i)-signzz(i)) + hh*(signxx(i)-signyy(i))) / seq
                normyy(i) = (hh*(signyy(i)-signxx(i)) + ff*(signyy(i)-signzz(i))) / seq
                normzz(i) = (ff*(signzz(i)-signyy(i)) + gg*(signzz(i)-signxx(i))) / seq
                normxy(i) = two*nn*signxy(i) / seq
                normyz(i) = two*ll*signyz(i) / seq
                normzx(i) = two*mm*signzx(i) / seq
!
                !< dsig/dlam = (dphi/dsig : dsig/dlam) = (Normal : dsig/dlam)
                !   --------------------------------------------------------
                dsig_dlam = normxx(i) * (cii*normxx(i) + cij*normyy(i) + cij*normzz(i))    &
                  + normyy(i) * (cij*normxx(i) + cii*normyy(i) + cij*normzz(i))    &
                  + normzz(i) * (cij*normxx(i) + cij*normyy(i) + cii*normzz(i))    &
                  + normxy(i) * normxy(i) * shear                                  &
                  + normyz(i) * normyz(i) * shear                                  &
                  + normzx(i) * normzx(i) * shear
!
                !<  derivative of dpla over lambda, dpla = lam * dphi/dsig
                !<  dpla/dlam = (sig : dphi/dsig) / yld
                sig_dfdsig   = signxx(i)*normxx(i) + signyy(i)*normyy(i) + signzz(i)*normzz(i) &
                  + signxy(i)*normxy(i) + signyz(i)*normyz(i) + signzx(i)*normzx(i)
                dpla_dlam(i) = sig_dfdsig / max(yld(i),em20)
!
                ! derivative of phi with respect to dlam
                dphi_dlam(i) = -dsig_dlam - h(i)*dpla_dlam(i)
                dphi_dlam(i) = sign(max(abs(dphi_dlam(i)),em20) ,dphi_dlam(i))
!
                !< plastic multiplier and plastic strain increment
                dlam    = -phi(i) / dphi_dlam(i)
                ddep    = dlam*dpla_dlam(i)
                dpla(i) = max(zero, dpla(i) + ddep)
                pla(i)  = pla(i) + ddep
              end do   ! ii = 1,nindx
!
              !< update yield stress with current plastic strain
!
              if (mat_param%ntable == 0) then    ! analytical hardening equation
                do ii=1,nindx
                  i = indx(ii)
                  yld(i) = sigy                                                            &
                    + qr1*(one - exp(-cr1*pla(i))) + qr2*(one - exp(-cr2*pla(i)))     &
                    + qx1*(one - exp(-cx1*pla(i))) + qx2*(one - exp(-cx2*pla(i)))
                  h(i)   = qr1*cr1*exp(-cr1*pla(i)) + qr2*cr2*exp(-cr2*pla(i))             &
                    + qx1*cx1*exp(-cx1*pla(i)) + qx2*cx2*exp(-cx2*pla(i))
                  h(i)   = h(i)   * (one + cowp(i))
                  yld(i) = yld(i) * (one + cowp(i))
                end do   ! ii = 1,nindx
              else                               ! tabulated yield with strain rate
                if (ndim == 1) then
                  xvec1(1:nel,1) = pla(1:nel)
                  call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec1,yld,h)
                else if (ndim == 2) then
                  xvec2(1:nel,1) = pla (1:nel)
                  xvec2(1:nel,2) = epsd(1:nel)
                  call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld,h)
                end if
              end if
              if (fisokin > 0) then
                yld(1:nel) = (one - fisokin)*yld(1:nel) + fisokin*yld0(1:nel)
              end if
!
              ! update stresses and yield criterion
!
              do ii=1,nindx
                i = indx(ii)
                !< tensor of plastic strain increment
                dpxx(i) = dlam * normxx(i)
                dpyy(i) = dlam * normyy(i)
                dpzz(i) = dlam * normzz(i)
                dpxy(i) = dlam * normxy(i)
                dpyz(i) = dlam * normyz(i)
                dpzx(i) = dlam * normzx(i)
                !< elasto-plastic stresses update
                signxx(i) = signxx(i) - (cii*dpxx(i) + cij*dpyy(i) + cij*dpzz(i))
                signyy(i) = signyy(i) - (cij*dpxx(i) + cii*dpyy(i) + cij*dpzz(i))
                signzz(i) = signzz(i) - (cij*dpxx(i) + cij*dpyy(i) + cii*dpzz(i))
                signxy(i) = signxy(i) - dpxy(i)*shear
                signyz(i) = signyz(i) - dpyz(i)*shear
                signzx(i) = signzx(i) - dpzx(i)*shear
!
                if (fisokin > 0) then   !< incremental backstress update
                  hkin = h(i) * fisokin
                  sigb(i,1) = sigb(i,1) + dpxx(i) * hkin
                  sigb(i,2) = sigb(i,2) + dpyy(i) * hkin
                  sigb(i,3) = sigb(i,3) + dpzz(i) * hkin
                  sigb(i,4) = sigb(i,4) + dpxy(i) * hkin
                  sigb(i,5) = sigb(i,5) + dpyz(i) * hkin
                  sigb(i,6) = sigb(i,6) + dpzx(i) * hkin
                end if
!
                ! update hill equivalent stress
                sighl(i) = ff*(signyy(i) - signzz(i))**2                                &
                  + gg*(signzz(i) - signxx(i))**2                                &
                  + hh*(signxx(i) - signyy(i))**2                                &
                  + two*(ll*signyz(i)**2 + mm*signzx(i)**2 + nn*signxy(i)**2)
                sighl(i) = sqrt(max(sighl(i),zero))
!
                ! update yield criterion
                phi(i) = sighl(i) - yld(i)
                et(i)  = h(i) / (h(i) + young) ! tangent stiffness coefficient for hourglass
!
              enddo  ! end of the loop over the yielding elements
            enddo    ! end of the loop over the iterations
          endif      ! nindx > 0
!===================================================================
!         end of plastic correction with cutting plane algorithm
!===================================================================
!
          !< Add backstress to elasto-plastic stress after projection (if kinematic hardening)
          if (fisokin > zero) then
            signxx(1:nel) = signxx(1:nel) + sigb(1:nel,1)
            signyy(1:nel) = signyy(1:nel) + sigb(1:nel,2)
            signzz(1:nel) = signzz(1:nel) + sigb(1:nel,3)
            signxy(1:nel) = signxy(1:nel) + sigb(1:nel,4)
            signyz(1:nel) = signyz(1:nel) + sigb(1:nel,5)
            signzx(1:nel) = signzx(1:nel) + sigb(1:nel,6)
          end if
          !
          ! plastic strain-rate filtering
          do i=1,nel
            dpdt    = dpla(i) / dtime
            epsd(i) = asrate * dpdt + (one - asrate) * uvar(i,1)
            uvar(i,1) = max(cc, epsd(i))  ! strain rate effect below static limit is ignored
          enddo
! ---------------------------------------------------------------------------------------------------
          return
        end subroutine sigeps128s
! ---------------------------------------------------------------------------------------------------
      end module sigeps128s_mod
