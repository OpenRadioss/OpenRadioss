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
!||====================================================================
!||    rbe3f_pen_mod   ../engine/source/constraints/general/rbe3/rbe3f_pen.F90
!||--- called by ------------------------------------------------------
!||    rbe3f           ../engine/source/constraints/general/rbe3/rbe3f.F
!||====================================================================
      module rbe3f_pen_mod
      implicit none
      contains
! ======================================================================================================================
! \brief rbe3 penalty force, stiffness update
! ======================================================================================================================
!||====================================================================
!||    rbe3f_pen       ../engine/source/constraints/general/rbe3/rbe3f_pen.F90
!||--- called by ------------------------------------------------------
!||    rbe3f           ../engine/source/constraints/general/rbe3/rbe3f.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine rbe3f_pen(                                           &
          ns      ,nmt0        ,numnod     ,nmt         ,         &
          nml     ,iml         ,ilsk       ,iadmp       ,         &
          a       ,ar          ,am         ,arm         ,         &
          stifn   ,stifr       ,stifnm      ,stifrm     ,         &
          v       ,vr          ,frbe3       ,x          ,         &
          lskew   ,numskw      ,skew        ,rrbe3pen_f ,         &
          rrbe3pen_stf,rrbe3pen_fac,rrbe3pen_vi ,rrbe3pen_m ,         &
          dt1     ,in          ,iroddl      )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod,          only : half,one,two,zero,zep05,em20,third,fourth,ten,em6,em12
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                :: ns              !< reference node id
          integer, intent(in)                                :: numnod          !< number of nodes
          integer, intent(in)                                :: nmt             !< dimension of master AM,ARM
          integer, intent(in)                                :: nmt0            !< dimension of total master nodes
          integer, intent(in)                                :: nml             !< number of independent nodes
          integer, intent(in)                                :: lskew           !< 1er dimension of skew
          integer, intent(in)                                :: numskw          !< number of skew
          integer, intent(in)                                :: iroddl          !< rotational dof flag
          integer, dimension(nml),     intent(in   )         :: iml             !< independent node list
          integer, dimension(nml),     intent(in   )         :: ilsk            !< local skew id of independent node
          integer, dimension(nmt0),    intent(in   )         :: iadmp           !< maping to local ind nodes
          real(kind=WP), dimension(3,numnod),intent(in   )         :: x               !< coordinates
          real(kind=WP), dimension(3,numnod),intent(in   )         :: v               !< velocity
          real(kind=WP), dimension(3,numnod),intent(in   )         :: vr              !< rotational velocity
          real(kind=WP), dimension(6,nml),   intent(in)            :: frbe3           !< normalized weight
          real(kind=WP), dimension(3),       intent(inout)         :: rrbe3pen_f      !< force
          real(kind=WP), dimension(2),       intent(in   )         :: rrbe3pen_stf    !< stiffness
          real(kind=WP),                     intent(in   )         :: dt1             !< time step
          real(kind=WP),                     intent(in   )         :: rrbe3pen_fac    !< stiffness factor
          real(kind=WP),                     intent(in   )         :: rrbe3pen_vi     !< damping coefficient
          real(kind=WP), dimension(3),       intent(inout)         :: rrbe3pen_m      !< moment
          real(kind=WP), dimension(3,numnod),intent(inout)         :: a               !< nodal internal force
          real(kind=WP), dimension(3,numnod),intent(inout)         :: ar              !< rotational internal force
          real(kind=WP), dimension(3,nmt),   intent(inout)         :: am              !< local nodal internal force of ind
          real(kind=WP), dimension(3,nmt),   intent(inout)         :: arm             !< local internal moment of ind
          real(kind=WP), dimension(numnod),  intent(inout)         :: stifn           !< nodal stifness
          real(kind=WP), dimension(numnod),  intent(inout)         :: stifr           !< nodal rotational stifness
          real(kind=WP), dimension(nmt),     intent(inout)         :: stifnm          !< local stifness of ind
          real(kind=WP), dimension(nmt),     intent(inout)         :: stifrm          !< local rotational stifness of ind
          real(kind=WP), dimension(lskew,numskw),intent(in)        :: skew            !< local skew
          real(kind=WP), dimension(numnod),  intent(in   )         :: in              !< nodal inertia
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,k,m,icoline,iel,el(3,3,nml)
          real(kind=WP), dimension(3) :: xbar,vts,wrv,dwrv,omgRb,tmrn,vit,          &
            for,mom,drot,rR,rn,vl,vrl,gminvmR,                    &
            frefb,mrefb,mrefr,mrefbr,mref0,fn,vrg,vrt
          real(kind=WP) :: wi(nml),rndotrn,det,gamma(9),gminv(9),gamma_max,jgamma,wmax
          real(kind=WP) :: wri(3,nml),stfn,stfr,facn,facr,fac_vi,fac_ref
          real(kind=WP) :: srR(3,3),srRT(3,3),srn(3,3),omgsrn(3,3),aa(3,3),aar(3,3)
          double precision :: disdp(3)
! ======================================================================================================================
!! specifications for penalty method :
!! 1.  only transtional dof of indepenent nodes are used for calculate rotation excepting for colinear case
!! 2.  partial transtional dof of indepenent nodes is forbenden and error out : to do
!! 3.  local skew is used for partial rotational dof (co-linear case)
!! 4.  rrbe3pen_* will be in restart file : 10*nrbe3_lp, init only with TT=zero
          gminv = zero
          mrefbr(1:3) = -HUGE(mrefbr(1))
          wi(1:nml) = frbe3(1,1:nml)
          xbar(1:3) = zero
          vts(1:3) = zero
          do i=1,nml
            m = iml(i)
            xbar(1:3) = xbar(1:3)+wi(i)*x(1:3,m)
            vts(1:3)  = vts(1:3)+wi(i)*v(1:3,m)
          end do
!
          disdp(1:3)= x(1:3,ns) - xbar(1:3)
          rR(1:3)   = disdp(1:3)
          srR(1,1) =  zero
          srR(1,2) = -rR(3)
          srR(1,3) =  rR(2)
          srR(2,1) =  rR(3)
          srR(2,2) =  zero
          srR(2,3) = -rR(1)
          srR(3,1) = -rR(2)
          srR(3,2) =  rR(1)
          srR(3,3) =  zero
!
!     calculate T
!
          gamma(1:9) = zero
          wrv(1:3) = zero
          do i=1,nml
            m = iml(i)
            rn(1:3) = x(1:3,m)-xbar(1:3)
            rndotrn = rn(1)*rn(1)+rn(2)*rn(2)+rn(3)*rn(3)
            vl(1:3) = v(1:3,m)
!
            gamma(1) = gamma(1)+wi(i)*(rndotrn-rn(1)*rn(1))
            gamma(2) = gamma(2)+wi(i)*(       -rn(2)*rn(1))
            gamma(3) = gamma(3)+wi(i)*(       -rn(3)*rn(1))
            gamma(4) = gamma(4)+wi(i)*(       -rn(1)*rn(2))
            gamma(5) = gamma(5)+wi(i)*(rndotrn-rn(2)*rn(2))
            gamma(6) = gamma(6)+wi(i)*(       -rn(3)*rn(2))
            gamma(7) = gamma(7)+wi(i)*(       -rn(1)*rn(3))
            gamma(8) = gamma(8)+wi(i)*(       -rn(2)*rn(3))
            gamma(9) = gamma(9)+wi(i)*(rndotrn-rn(3)*rn(3))
!
            dwrv(1) = wi(i)*(rn(2)*vl(3)-rn(3)*vl(2))
            dwrv(2) = wi(i)*(rn(3)*vl(1)-rn(1)*vl(3))
            dwrv(3) = wi(i)*(rn(1)*vl(2)-rn(2)*vl(1))
            wrv(1:3) = wrv(1:3)+dwrv(1:3)
!
          end do
          icoline = 0
          det =                                                          &
            (gamma(1)*(gamma(5)*gamma(9)-gamma(6)*gamma(8))-       &
            gamma(2)*(gamma(4)*gamma(9)-gamma(6)*gamma(7))+       &
            gamma(3)*(gamma(4)*gamma(8)-gamma(5)*gamma(7)))
!
!         maximum diagonal gamma value, to normalize det
!
          gamma_max = max(em20,gamma(1),gamma(5),gamma(9))
          if(abs(det/(gamma_max*gamma_max*gamma_max)) < em6) then!elements with colinear master nodes
!
!         if (irot==0) write(*,*)'error :RBE3: colinear master nodes' ! will check in ini
            icoline = 1
!         omgRb(1:3)  = vrs(1:3)
!
          else
!!
            jgamma = one/det

            gminv(1) = jgamma * (gamma(5)*gamma(9)-gamma(6)*gamma(8))
            gminv(2) = jgamma * (gamma(3)*gamma(8)-gamma(2)*gamma(9))
            gminv(3) = jgamma * (gamma(2)*gamma(6)-gamma(3)*gamma(5))
            gminv(4) = jgamma * (gamma(6)*gamma(7)-gamma(4)*gamma(9))
            gminv(5) = jgamma * (gamma(1)*gamma(9)-gamma(3)*gamma(7))
            gminv(6) = jgamma * (gamma(3)*gamma(4)-gamma(1)*gamma(6))
            gminv(7) = jgamma * (gamma(4)*gamma(8)-gamma(5)*gamma(7))
            gminv(8) = jgamma * (gamma(2)*gamma(7)-gamma(1)*gamma(8))
            gminv(9) = jgamma * (gamma(1)*gamma(5)-gamma(2)*gamma(4))
            omgRb(1) = gminv(1)*wrv(1)+gminv(4)*wrv(2)+gminv(7)*wrv(3)
            omgRb(2) = gminv(2)*wrv(1)+gminv(5)*wrv(2)+gminv(8)*wrv(3)
            omgRb(3) = gminv(3)*wrv(1)+gminv(6)*wrv(2)+gminv(9)*wrv(3)
!   calculate S(rR) multi T for stifn,stifr update
            srRT (1:3,1) = srR(1:3,1)*gminv(1)+srR(1:3,2)*gminv(2)+srR(1:3,3)*gminv(3)
            srRT (1:3,2) = srR(1:3,1)*gminv(4)+srR(1:3,2)*gminv(5)+srR(1:3,3)*gminv(6)
            srRT (1:3,3) = srR(1:3,1)*gminv(7)+srR(1:3,2)*gminv(8)+srR(1:3,3)*gminv(9)
          end if
          if (icoline>0) then
            wri(1:3,1:nml) = frbe3(4:6,1:nml)
            omgRb(1:3) = zero
            do k=1,nml
              m = iml(k)
              iel = ilsk(k)
              el = zero
              if (iel > 0) then
                do i = 1, 3
                  el(i,1,k) = skew(i,iel)
                  el(i,2,k) = skew(i+3,iel)
                  el(i,3,k) = skew(i+6,iel)
                end do
              else
                do i = 1, 3
                  el(i,i,k) = one
                end do
              end if
              vrl(1:3) = el(1:3,1,k)*vr(1,m)+el(1:3,2,k)*vr(2,m)+el(1:3,3,k)*vr(3,m)
              vrl(1:3) = wri(1:3,k)*vrl(1:3)
              vrg(1:3) = el(1,1:3,k)*vrl(1)+el(2,1:3,k)*vrl(2)+el(3,1:3,k)*vrl(3)
              omgRb(1:3)  = omgRb(1:3)+vrg(1:3)
            end do
          end if
! incre. force,moment
!          disp(1:3) = disdp(1:3) - rrbe3pen_d(1:3)
!          lms2=disdp(1)*disdp(1)+disdp(2)*disdp(2)+disdp(3)*disdp(3)
          vrt(1) = rR(2)*omgRb(3)-rR(3)*omgRb(2)
          vrt(2) = rR(3)*omgRb(1)-rR(1)*omgRb(3)
          vrt(3) = rR(1)*omgRb(2)-rR(2)*omgRb(1)
          vit(1:3) = v(1:3,ns)-vts(1:3) + vrt(1:3) !remove rotational part : -omgRb x rR
          for(1:3) = rrbe3pen_f(1:3) + rrbe3pen_stf(1)*vit(1:3)*dt1
          rrbe3pen_f(1:3) = for(1:3)
          for(1:3) = for(1:3)+rrbe3pen_vi*vit(1:3) ! add damping
          mom(1:3) = zero
!-------forces distributions & stifn,stifr update
          fac_vi = one + two*zep05
          fac_ref = fac_vi*rrbe3pen_fac
          stifn(ns) = stifn(ns)+fac_ref*rrbe3pen_stf(1)
          if (iroddl>0) then
            if (in(ns)>zero.and.rrbe3pen_stf(2)>em20) then
              drot(1:3) = (vr(1:3,ns)-omgRb(1:3))*dt1
              mom(1:3) = rrbe3pen_m(1:3)+ rrbe3pen_stf(2)*drot(1:3)
              rrbe3pen_m(1:3) = mom(1:3)
              facr = max(one,half*rrbe3pen_fac)
              stifr(ns) = stifr(ns)+facr*rrbe3pen_stf(2)
              ar(1:3,ns) = ar(1:3,ns) - mom(1:3) ! a(1:3,ns) will be updated at the end
            end if
          end if
          frefb(1:3) = for(1:3)  ! will be distributed to ind nodes
          mrefb(1:3) = mom(1:3)  ! will be distributed to ind nodes
! forces distributed to ind nodes and get balance forces for ref node
          for(1:3) = zero
! for ind nodes :
          stfn = fac_vi*rrbe3pen_stf(1)
          stfr = rrbe3pen_stf(2)     ! only stifnr will be condensed to idep for time step concern
          if (icoline>0) then
            do i=1,nml
              k = iadmp(i)
              wmax=max(wri(1,i),wri(2,i),wri(3,i))
              facn = wi(i)*wi(i)
              facr  = wmax*wmax
              stifnm(k) = stifnm(k) + stfn*facn
              stifrm(k) = stifrm(k) + stfr*facr
!
              fn(1:3) = wi(i)*frefb(1:3)
              for(1:3) = for(1:3) + fn(1:3)
              am(1:3,k) = am(1:3,k) + fn(1:3)
!
              mrefr(1:3) = mrefb(1)*el(1,1:3,i)+mrefb(2)*el(2,1:3,i)+mrefb(3)*el(3,1:3,i)
              mrefr(1:3) = wri(1:3,i)*mrefr(1:3)
              mref0(1:3) = mrefr(1)*el(1:3,1,i)+mrefr(2)*el(1:3,2,i)+mrefr(3)*el(1:3,3,i)
              arm(1:3,k) = arm(1:3,k) + mref0(1:3)
            end do
          else
!
            mrefbr(1) = mrefb(1)+rR(2)*frefb(3)-rR(3)*frefb(2)
            mrefbr(2) = mrefb(2)+rR(3)*frefb(1)-rR(1)*frefb(3)
            mrefbr(3) = mrefb(3)+rR(1)*frefb(2)-rR(2)*frefb(1)
            gminvmR(1) = gminv(1)*mrefbr(1)+gminv(4)*mrefbr(2)+ gminv(7)*mrefbr(3)
            gminvmR(2) = gminv(2)*mrefbr(1)+gminv(5)*mrefbr(2)+ gminv(8)*mrefbr(3)
            gminvmR(3) = gminv(3)*mrefbr(1)+gminv(6)*mrefbr(2)+ gminv(9)*mrefbr(3)
            do i=1,nml
              m = iml(i)
              k = iadmp(i)
              disdp(1:3)= x(1:3,m) - xbar(1:3)
              rn(1:3) = disdp(1:3)
!   matrix S(rn) for each master node
              srn(1,1) =  zero
              srn(1,2) = -rn(3)
              srn(1,3) =  rn(2)
              srn(2,1) =  rn(3)
              srn(2,2) =  zero
              srn(2,3) = -rn(1)
              srn(3,1) = -rn(2)
              srn(3,2) =  rn(1)
              srn(3,3) =  zero
!        weight matrix X S(r
              omgsrn(1:3,1:3) = srn(1:3,1:3)*wi(i)
              aa(1,1:3)=-srRT(1,1)*omgsrn(1,1:3)-srRT(1,2)*omgsrn(2,1:3)-srRT(1,3)*omgsrn(3,1:3)
              aa(2,1:3)=-srRT(2,1)*omgsrn(1,1:3)-srRT(2,2)*omgsrn(2,1:3)-srRT(2,3)*omgsrn(3,1:3)
              aa(3,1:3)=-srRT(3,1)*omgsrn(1,1:3)-srRT(3,2)*omgsrn(2,1:3)-srRT(3,3)*omgsrn(3,1:3)
              aa(1,1) = aa(1,1) +wi(i)
              aa(2,2) = aa(2,2) +wi(i)
              aa(3,3) = aa(3,3) +wi(i)
              aar(1,1:3)= gminv(1)*omgsrn(1,1:3)+gminv(4)*omgsrn(2,1:3)+gminv(7)*omgsrn(3,1:3)
              aar(2,1:3)= gminv(2)*omgsrn(1,1:3)+gminv(5)*omgsrn(2,1:3)+gminv(8)*omgsrn(3,1:3)
              aar(3,1:3)= gminv(3)*omgsrn(1,1:3)+gminv(6)*omgsrn(2,1:3)+gminv(9)*omgsrn(3,1:3)
              facn =       max(                                                  &
                aa(1,1)*aa(1,1)+aa(2,1)*aa(2,1)+aa(3,1)*aa(3,1),       &
                aa(1,2)*aa(1,2)+aa(2,2)*aa(2,2)+aa(3,2)*aa(3,2),       &
                aa(1,3)*aa(1,3)+aa(2,3)*aa(2,3)+aa(3,3)*aa(3,3))
              facr =       max(                                                  &
                aar(1,1)*aar(1,1)+aar(2,1)*aar(2,1)+aar(3,1)*aar(3,1),       &
                aar(1,2)*aar(1,2)+aar(2,2)*aar(2,2)+aar(3,2)*aar(3,2),       &
                aar(1,3)*aar(1,3)+aar(2,3)*aar(2,3)+aar(3,3)*aar(3,3))
              stifnm(k) = stifnm(k) + stfn*facn+stfr*facr
!
              tmrn(1) = gminvmR(2)*rn(3)-gminvmR(3)*rn(2)
              tmrn(2) = gminvmR(3)*rn(1)-gminvmR(1)*rn(3)
              tmrn(3) = gminvmR(1)*rn(2)-gminvmR(2)*rn(1)
              fn(1:3) = wi(i)*(frefb(1:3)+tmrn(1:3))
!
              for(1:3) = for(1:3) + fn(1:3)
              am(1:3,k) = am(1:3,k) + fn(1:3)
            end do
!
          end if !if (icoline>0) then
!
          a(1:3,ns) = a(1:3,ns) - for(1:3)
!
!
        end subroutine rbe3f_pen
      end module rbe3f_pen_mod
