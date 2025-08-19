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
!||    sigeps129s_mod   ../engine/source/materials/mat/mat129/sigeps129s.F90
!||--- called by ------------------------------------------------------
!||    mulaw            ../engine/source/materials/mat_share/mulaw.F90
!||====================================================================
      module sigeps129s_mod
      contains


!||====================================================================
!||    sigeps129s              ../engine/source/materials/mat/mat129/sigeps129s.F90
!||--- called by ------------------------------------------------------
!||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    sensor_mod              ../common_source/modules/sensor_mod.F90
!||    table4d_mod             ../common_source/modules/table4d_mod.F
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
        subroutine sigeps129s(mat_param,                                       &
          nel      ,nuvar    ,nvartmp  ,uvar     ,vartmp   ,timestep ,         &
          depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,         &
          sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,         &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,         &
          yld      ,et       ,pla      ,dpla     ,epsp     ,soundsp  ,         &
          temp0    ,temp     ,off      ,time     ,iexpan   ,amu      ,         &
          sensors  )
!
! ======================================================================================================================
! \brief thermo-elasto-viscoplastic material with creep, for solid elements

! ======================================================================================================================
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use matparam_def_mod
          use constant_mod ,only : zero,one,two,three,half,third,two_third,four_over_3,four_over_5
          use constant_mod ,only : em01,em20,infinity
          use sensor_mod
          use table4d_mod
          use table_mat_vinterp_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------

#include "my_real.inc"

! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer ,intent(in) :: nel                           !< element group size
          integer ,intent(in) :: nuvar                         !< number of state variables
          integer ,intent(in) :: nvartmp                       !< number of temporary internal variables
          integer ,intent(in) :: iexpan                        !< /heat/therm_stress flag
          my_real ,intent(in) :: time                          !< current time
          my_real ,intent(in) :: timestep                      !< time step
          my_real ,dimension(nel)     ,intent(in)    :: temp0  !< previous temperature
          my_real ,dimension(nel)     ,intent(in)    :: temp   !< current  temperature
          my_real ,dimension(nel)     ,intent(in)    :: amu    !< mu
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
          my_real ,dimension(nel)     ,intent(out)   :: yld    !< yield stress
          my_real ,dimension(nel)     ,intent(out)   :: soundsp!< sound speed
          my_real ,dimension(nel)     ,intent(out)   :: epsp   !< plastic strain rate
          my_real ,dimension(nel,nuvar)   ,intent(inout) :: uvar      !< state variables
          integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp    !< temporary internal variables
          type (matparam_struct_)         ,intent(in)    :: mat_param !< material parameter structure
          type (sensors_)                 ,intent(in)    :: sensors   !< sensor structure
          target :: mat_param,temp,pla
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,ii,iter,niter,nindx,crp_law,isens,ndim
          integer ,dimension(nel) :: indx
          my_real :: dpdt
          my_real :: epsp0,lame,ldav,epsc,deps,dcreep,p,rfact
          my_real :: facp
          my_real :: asrate,dtime,tstart
          my_real :: cr1,cr2,cx1,cx2
          my_real :: j2,g2,g3,rho0
          my_real :: sig_crp,time_crp,tref
          my_real :: dphi_dlam
          my_real :: ca,n,m,cq
          my_real :: fsig,ftime
          my_real :: alpha0,crpa0
          my_real ,dimension(nel)   :: pla0,dlam
          my_real ,dimension(nel)   :: sxx,syy,szz,sxy,syz,szx !< deviatoric stress components
          my_real ,dimension(nel)   :: crpa,crpm,crpn,crpq     !< creep parameters
          my_real ,dimension(nel)   :: cc,cp,cowp              !< strain rate parameters
          my_real ,dimension(nel)   :: young,shear,bulk,nu     !< elastic moduli
          my_real ,dimension(nel)   :: h                       !< hardening tangent stiffness
          my_real ,dimension(nel)   :: phi                     !< plastic yield criterion
          my_real ,dimension(nel)   :: sigy                    !< yield stress
          my_real ,dimension(nel)   :: svm                     !< Von Mises stress
          my_real ,dimension(nel)   :: sigm                    !< pressure
          my_real ,dimension(nel)   :: fscale                  !< scale factor
          my_real ,dimension(nel)   :: alpha                   ! thermal expansion coeff
          my_real ,dimension(nel)   :: depsth,depsc
          my_real ,dimension(nel)   :: qr1,qr2,qx1,qx2
          my_real ,dimension(:,:) ,pointer :: xvec1
          my_real ,dimension(:,:) ,pointer :: xvec2
          type (table_4d_)        ,pointer :: itable
! ----------------------------------------------------------------------------------------------------------------------
!   state  v a r i a b l e s (uvar)
! ----------------------------------------------------------------------------------------------------------------------
!     uvar(1) = plastic strain rate, saved for filtering
!     uvar(2) = accumulated creep strain
! ======================================================================================================================
          niter = 3   ! max number of newton iterations
          dtime = max(timestep, em20)
          rho0  = mat_param%rho0
!
          young(1:nel) = mat_param%young
          shear(1:nel) = mat_param%shear
          bulk(1:nel)  = mat_param%bulk
          nu(1:nel)    = mat_param%nu
!
          crp_law      = mat_param%iparam(1)
          isens        = mat_param%iparam(2)
!
          sigy(1:nel)  = mat_param%uparam(1)
          qr1(1:nel)   = mat_param%uparam(2)
          qr2(1:nel)   = mat_param%uparam(3)
          qx1(1:nel)   = mat_param%uparam(4)
          qx2(1:nel)   = mat_param%uparam(5)
          cr1          = mat_param%uparam(6)
          cr2          = mat_param%uparam(7)
          cx1          = mat_param%uparam(8)
          cx2          = mat_param%uparam(9)
          epsp0        = mat_param%uparam(10)
          cc(1:nel)    = epsp0
          cp(1:nel)    = mat_param%uparam(11)
          alpha0       = mat_param%uparam(12)
          tref         = mat_param%uparam(13)
          crpa0        = mat_param%uparam(14)
          crpn(1:nel)  = mat_param%uparam(15)
          crpm(1:nel)  = mat_param%uparam(16)
          crpq(1:nel)  = mat_param%uparam(17)
          sig_crp      = mat_param%uparam(18)
          time_crp     = mat_param%uparam(19)
          asrate = min(one,mat_param%uparam(20)*dtime)
!
          alpha(1:nel) = alpha0
          crpa(1:nel)  = crpa0
! ----------------------------------------------------------------------------------------------------------------------
          ! check material parameters dependency on temperature and apply scale factors
! ----------------------------------------------------------------------------------------------------------------------
          xvec1(1:nel,1:1) => temp(1:nel)
!
          if (mat_param%table(2)%notable > 0) then   ! young modulus evolution
            call table_mat_vinterp(mat_param%table(2),nel,nel,vartmp(1,3),xvec1,young,h)
          end if
!
          if (mat_param%table(3)%notable > 0) then  ! Poisson coefficient evolution
            call table_mat_vinterp(mat_param%table(3),nel,nel,vartmp(1,4),xvec1,nu,h)
            shear(1:nel) = half * young(1:nel) / (one + nu(1:nel))
          end if
!
          if (mat_param%table(4)%notable > 0) then  ! Yield stress evolution
            call table_mat_vinterp(mat_param%table(4),nel,nel,vartmp(1:nel,5),xvec1,sigy,h)
          end if
!
          if (mat_param%table(5)%notable > 0) then  ! Qr1 and Qr2 parameters evolution
            call table_mat_vinterp(mat_param%table(5),nel,nel,vartmp(1:nel,6),xvec1,fscale,h)
            qr1(1:nel) = qr1(1:nel) * fscale(1:nel)
            qr2(1:nel) = qr2(1:nel) * fscale(1:nel)
          end if
!
          if (mat_param%table(6)%notable > 0) then  ! Qx1 and Qx2 parameters evolution
            call table_mat_vinterp(mat_param%table(6),nel,nel,vartmp(1,7),xvec1,fscale,h)
            qx1(1:nel) = qx1(1:nel) * fscale(1:nel)
            qx2(1:nel) = qx2(1:nel) * fscale(1:nel)
          end if
!
          if (mat_param%uparam(10) > zero) then  ! Cowper-Symonds strain rate
            cc(1:nel) = mat_param%uparam(10)
            cp(1:nel) = mat_param%uparam(11)
!
            if (mat_param%table(7)%notable > 0) then  ! cc parameters evolution
              call table_mat_vinterp(mat_param%table(7),nel,nel,vartmp(1,8),xvec1,cc,h)
            end if
!
            if (mat_param%table(8)%notable > 0) then  ! cp parameters evolution
              call table_mat_vinterp(mat_param%table(8),nel,nel,vartmp(1,9),xvec1,cp,h)
            end if
          end if
!
          if (mat_param%table(9)%notable > 0) then  ! creep A parameters evolution
            call table_mat_vinterp(mat_param%table(9),nel,nel,vartmp(1,10),xvec1,crpa,h)
          end if
!
          if (mat_param%table(10)%notable > 0) then  ! creep B parameters evolution
            call table_mat_vinterp(mat_param%table(10),nel,nel,vartmp(1,11),xvec1,crpn,h)
          end if
!
          if (mat_param%table(11)%notable > 0) then  ! creep Q parameters evolution
            call table_mat_vinterp(mat_param%table(11),nel,nel,vartmp(1,12),xvec1,crpm,h)
          end if
!
          if (mat_param%table(12)%notable > 0) then  ! creep M parameters evolution
            call table_mat_vinterp(mat_param%table(12),nel,nel,vartmp(1,13),xvec1,crpq,h)
          end if
!
          if (mat_param%table(13)%notable > 0) then  ! alpha parameters evolution
            call table_mat_vinterp(mat_param%table(13),nel,nel,vartmp(1,14),xvec1,alpha,h)
          end if
! ----------------------------------------------------------------------------------------------------------------------
          if (mat_param%table(1)%notable > 0) then  ! tabulated input of equivalent stress
            itable => mat_param%table(1)
            ndim   = itable%ndim
            if (ndim == 1) then
              xvec1(1:nel,1:1) => pla(1:nel)
              call table_mat_vinterp(itable,nel,nel,vartmp,xvec1,yld,h)
            else if (ndim == 2) then
              xvec2(1:nel,1:1) => pla (1:nel)
              xvec2(1:nel,2:2) => temp(1:nel)
              call table_mat_vinterp(itable,nel,nel,vartmp,xvec2,yld,h)
            end if
          else                   ! use analytic Voce hardening formula
            do i = 1,nel
              yld(i) = sigy(i)                                     &
                     + qr1(i)*(one - exp(-cr1*pla(i)))             &
                     + qr2(i)*(one - exp(-cr2*pla(i)))             &
                     + qx1(i)*(one - exp(-cx1*pla(i)))             &
                     + qx2(i)*(one - exp(-cx2*pla(i)))
              h(i)   = qr1(i)*cr1*exp(-cr1*pla(i))                 &
                     + qr2(i)*cr2*exp(-cr2*pla(i))                 &
                     + qx1(i)*cx1*exp(-cx1*pla(i))                 &
                     + qx2(i)*cx2*exp(-cx2*pla(i))
            enddo
          end if
! ----------------------------------------------------------------------------------------------------------------------
          epsp(1:nel) = uvar(1:nel,1)  ! filtered plastic strain rate from previous time step
!
          ! apply strain rate scaling factor on yield stress
          if (epsp0 > zero)  then
            do i = 1,nel
              cowp(i) = one + (epsp(i)/cc(i))**cp(i)
              yld(i)  = yld(i) * cowp(i)
              h(i)    = h(i)   * cowp(i)
            enddo
          end if
! ----------------------------------------------------------------------------------------------------------------------
          ! element deletion condition
          do i=1,nel
            if (off(i) < one)  off(i) = four_over_5*off(i)
            if (off(i) < em01) off(i) = zero
          enddo
! ----------------------------------------------------------------------------------------------------------------------
          ! elastic trial stress tensor
! ----------------------------------------------------------------------------------------------------------------------
          do i=1,nel
            g2 = shear(i) * two
            lame = g2 * nu(i) / (one - two*nu(i))
            ldav = (depsxx(i) + depsyy(i) + depszz(i)) * lame
            signxx(i) = sigoxx(i) + depsxx(i)*g2 + ldav
            signyy(i) = sigoyy(i) + depsyy(i)*g2 + ldav
            signzz(i) = sigozz(i) + depszz(i)*g2 + ldav
            signxy(i) = sigoxy(i) + depsxy(i)*shear(i)
            signyz(i) = sigoyz(i) + depsyz(i)*shear(i)
            signzx(i) = sigozx(i) + depszx(i)*shear(i)
          enddo
! ----------------------------------------------------------------------------------------------------------------------
          ! thermal strain increment and thermal stress correction
          if (iexpan == 0 .and. alpha0 > zero) then
            depsth(1:nel) = alpha(1:nel) * (temp(1:nel) - temp0(1:nel))
            do i=1,nel
              g2 = shear(i) * two
              signxx(i) = signxx(i) - depsth(i) * g2
              signyy(i) = signyy(i) - depsth(i) * g2
              signzz(i) = signzz(i) - depsth(i) * g2
            end do
          end if
          ! initial deviatoric stress
          do i=1,nel
            sigm(i) = (signxx(i) + signyy(i) + signzz(i)) * third
            sxx(i)  = signxx(i) - sigm(i)
            syy(i)  = signyy(i) - sigm(i)
            szz(i)  = signzz(i) - sigm(i)
            sxy(i)  = signxy(i)
            syz(i)  = signyz(i)
            szx(i)  = signzx(i)
            ! Von Mises stress = sqrt(3*J2)
            j2 = (sxx(i)**2 + syy(i)**2 + szz(i)**2)*half + sxy(i)**2 + syz(i)**2 + szx(i)**2
            svm(i) = sqrt(three*j2)
          enddo
! ----------------------------------------------------------------------------------------------------------------------
          et(1:nel)    = one
          dpla(1:nel)  = zero
          pla0(1:nel)  = pla(1:nel)
          depsc(1:nel) = zero
! ----------------------------------------------------------------------------------------------------------------------
          ! check yield condition and calculate plastic strain
! ----------------------------------------------------------------------------------------------------------------------
          phi(1:nel) = svm(1:nel) - yld(1:nel)
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
          !< plastic projection - Newton iterations  
          do iter = 1,niter 
#include "vectorize.inc" 
            do ii=1,nindx
              i  = indx(ii)
              dphi_dlam  = -(three*shear(i) + h(i))
              dphi_dlam  = sign(max(abs(dphi_dlam), em20), dphi_dlam)
              dlam(i) = -phi(i) / dphi_dlam
              dpla(i) = max(dpla(i) + dlam(i), em20)
              pla(i)  = pla0(i) + dpla(i)
            end do   ! yielding elements
!
            ! update yield stress and hardening module
!
            if (mat_param%table(1)%notable > 0) then  ! tabulated input of equivalent stress
              itable => mat_param%table(1)
              ndim   = itable%ndim
              if (ndim == 1) then
                xvec1(1:nel,1:1) => pla(1:nel)
                call table_mat_vinterp(itable,nel,nel,vartmp,xvec1,yld,h)
              else if (ndim == 2) then
                xvec2(1:nel,1:1) => pla (1:nel)
                xvec2(1:nel,2:2) => temp(1:nel)
                call table_mat_vinterp(itable,nel,nel,vartmp,xvec2,yld,h)
              end if
            else                   ! use analytic Voce hardening formula
#include "vectorize.inc" 
              do i = 1,nel
                yld(i) = sigy(i)                                     &
                       + qr1(i)*(one - exp(-cr1*pla(i)))             &
                       + qr2(i)*(one - exp(-cr2*pla(i)))             &
                       + qx1(i)*(one - exp(-cx1*pla(i)))             &
                       + qx2(i)*(one - exp(-cx2*pla(i)))
                h(i)   = qr1(i)*cr1*exp(-cr1*pla(i))                 &
                       + qr2(i)*cr2*exp(-cr2*pla(i))                 &
                       + qx1(i)*cx1*exp(-cx1*pla(i))                 &
                       + qx2(i)*cx2*exp(-cx2*pla(i))
              enddo
            end if
            !< Update the stress tensor
#include "vectorize.inc" 
            do ii=1,nindx
              i  = indx(ii)
              g3 = shear(i) * three
              rfact  = yld(i) / (g3*dlam(i) + yld(i))
              sxx(i) = sxx(i) * rfact 
              syy(i) = syy(i) * rfact
              szz(i) = szz(i) * rfact
              sxy(i) = sxy(i) * rfact
              syz(i) = syz(i) * rfact
              szx(i) = szx(i) * rfact
              ! Von Mises stress = sqrt(3*J2)
              j2 = (sxx(i)**2 + syy(i)**2 + szz(i)**2)*half + sxy(i)**2 + syz(i)**2 + szx(i)**2
              svm(i) = sqrt(three*j2)
!
              phi(i) = svm(i) - yld(i)
              et(i) = h(i) / (h(i) + young(i))
            enddo
          end do     ! end of the Newton iterations
! ----------------------------------------------------------------------------------------------------------------------
          ! calculation of creep strain increment and total creep strain
          ! stop creep evolution if sensor is activated
! ----------------------------------------------------------------------------------------------------------------------
          tstart = infinity
          if (crpa0 > zero) then
            if (isens > zero) then
              tstart = sensors%sensor_tab(isens)%tstart
            end if
            if (time > zero .and. time < tstart) then
              if (crp_law == 1) then           ! use transient Norton power law
                do i=1,nel
                  ca = crpa(i)
                  n  = crpn(i)
                  m  = crpm(i)
                  fsig  = (svm(i)/sig_crp)**n
                  ftime = (time/time_crp)**m
                  depsc(i) = ca * fsig * ftime * dtime
                end do
              else if (crp_law == 2) then      ! use Garfallo steady state law
                do i=1,nel
                  ca = crpa(i)
                  n  = crpn(i)
                  m  = crpm(i)
                  cq = crpq(i)
                  fsig  = (sinh(svm(i)/sig_crp))**n
                  ftime = exp(-cq/max(temp(i),em20))
                  depsc(i) = ca * fsig * ftime * dtime
                end do
              else if (crp_law == 3) then      ! use strain hardening Norton formulation
                do i=1,nel
                  ca = crpa(i)
                  n  = crpn(i)
                  m  = crpm(i)
                  depsc(i) = m*ca**(one/m) * (svm(i)/sig_crp)**(n/m)   &
                    * uvar(i,2)**((m-one)/m) * dtime/time_crp
                end do
              end if
              do i=1,nel
                deps = (depsxx(i)**2 + depsyy(i)**2 + depszz(i)**2) * half     &
                     +  depsxy(i)**2 + depsyz(i)**2 + depszx(i)**2
                depsc(i) = min(depsc(i) ,sqrt(two_third*deps))   ! creep strain should be lower than elastic !
                epsc = uvar(i,2) + depsc(i)
                uvar(i,2) = epsc
              end do
            end if
          end if
! ----------------------------------------------------------------------------------------------------------------------
          ! deviatoric stress correction due to creep
! ----------------------------------------------------------------------------------------------------------------------
          do i=1,nel
            dcreep = max(depsc(i), zero)
            g3     = shear(i) * three
            rfact  = yld(i) / (g3*dcreep + yld(i))
            sxx(i) = sxx(i) * rfact 
            syy(i) = syy(i) * rfact
            szz(i) = szz(i) * rfact
            sxy(i) = sxy(i) * rfact
            syz(i) = syz(i) * rfact
            szx(i) = szx(i) * rfact
          end do
! ----------------------------------------------------------------------------------------------------------------------
          ! pressure correction with thermal expansion
          if (iexpan > 0) then
            do i=1,nel
              p = bulk(i) * amu(i)
              signxx(i) = signxx(i) - p
              signyy(i) = signyy(i) - p
              signzz(i) = signzz(i) - p
            end do
          else
            do i=1,nel
              signxx(i) = sxx(i) + sigm(i)
              signyy(i) = syy(i) + sigm(i)
              signzz(i) = szz(i) + sigm(i)
              signxy(i) = sxy(i)
              signyz(i) = syz(i)
              signzx(i) = szx(i)
            end do
          end if
! ----------------------------------------------------------------------------------------------------------------------
          ! plastic strain-rate filtering
          if (epsp0 > zero)  then
            do i=1,nel
              dpdt    = dpla(i) / dtime
              epsp(i) = asrate * dpdt + (one - asrate) * uvar(i,1)
              uvar(i,1) = max(cc(i), epsp(i))  ! strain rate effect below static limit is ignored
            enddo
          end if
          soundsp(1:nel) = sqrt((bulk(1:nel) + four_over_3*shear(1:nel)) / rho0)
          return
        end
! ----------------------------------------------------------------------------------------------------------------------
      end module sigeps129s_mod
