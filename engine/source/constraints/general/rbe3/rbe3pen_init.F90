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
!||    rbe3pen_init_mod   ../engine/source/constraints/general/rbe3/rbe3pen_init.F90
!||--- called by ------------------------------------------------------
!||    rbe3t1             ../engine/source/constraints/general/rbe3/rbe3f.F
!||====================================================================
      module rbe3pen_init_mod
      implicit none
      contains
! ======================================================================================================================
! \brief rbe3 penalty formulation initialization
! ======================================================================================================================
!||====================================================================
!||    rbe3pen_init     ../engine/source/constraints/general/rbe3/rbe3pen_init.F90
!||--- called by ------------------------------------------------------
!||    rbe3t1           ../engine/source/constraints/general/rbe3/rbe3f.F
!||--- calls      -----------------------------------------------------
!||    rbe3fpen_ininp   ../engine/source/constraints/general/rbe3/rbe3pen_init.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod     ../common_source/modules/constant_mod.F
!||    precision_mod    ../common_source/modules/precision_mod.F90
!||    rbe3_mod         ../common_source/modules/constraints/rbe3_mod.F90
!||====================================================================
        subroutine rbe3pen_init(                                        &
          x       ,ms          ,in         ,stifn       ,         &
          stifr   ,numnod      ,rbe3       ,time        ,         &
          impl_s  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use rbe3_mod
          use precision_mod, only : WP
          use constant_mod,          only : zero
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                :: numnod          !< number of nodes
          real(kind=WP), dimension(numnod),  intent(inout)         :: ms              !< nodal mass
          real(kind=WP), dimension(numnod),  intent(inout)         :: in              !< nodal inertia
          real(kind=WP), dimension(numnod),  intent(in   )         :: stifn           !< nodal stifness
          real(kind=WP), dimension(numnod),  intent(in   )         :: stifr           !< nodal rotational stifness
          real(kind=WP), dimension(3,numnod),intent(in   )         :: x               !< coordinates
          integer, intent(in)                                      :: impl_s          !< if implicit solution
          real(kind=WP), intent(in)                                :: time            !< time
          type (rbe3_)                ,intent(inout)         :: rbe3            !< rbe3 data structure
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: n,iad,ipen,nml,ns,irot,n_p
! ======================================================================================================================
!! 1.  rrbe3pen_d, rrbe3pen_stf, rrbe3pen_fac, rrbe3pen_vi, rrbe3pen_m, init only with TT=zero
          if (time==zero) then
            n_p = 0
            do n = 1,rbe3%nrbe3
!
              iad = rbe3%irbe3(1,n)
              ns  = rbe3%irbe3(3,n)
              nml = rbe3%irbe3(5,n)
              irot =rbe3%irbe3(6,n)
              ipen =rbe3%irbe3(9,n)
              if (ns==0.or.ipen==0) cycle
              n_p = n_p + 1
!
              call rbe3fpen_ininp(                                                       &
                ns               ,irot          ,numnod       ,nml       ,         &
                ms               ,in            ,stifn        ,stifr     ,         &
                rbe3%lrbe3(iad+1)     ,rbe3%frbe3(6*iad+1)    ,x         ,         &
                rbe3%pen%rrbe3pen_f(1,n_p), rbe3%pen%rrbe3pen_stf(1,n_p)     ,         &
                rbe3%pen%rrbe3pen_fac(n_p), rbe3%pen%rrbe3pen_vi(n_p )       ,         &
                rbe3%pen%rrbe3pen_m(1,n_p))
!
            end do
          end if !(time==zero) then
          if (impl_s>0) then ! not use penalty for implicit
            do n = 1,rbe3%nrbe3
              ipen =rbe3%irbe3(9,n)
              if (ipen >0) rbe3%irbe3(9,n) =-ipen
            end do
          else
            do n = 1,rbe3%nrbe3
              ipen =rbe3%irbe3(9,n)
              if (ipen <0) rbe3%irbe3(9,n) =-ipen
            end do
          end if
!
        end subroutine rbe3pen_init
! ----------------------------------------------------------------------------------------------------------------------
! ======================================================================================================================
! \brief rbe3 penalty force, stiffness update
! ======================================================================================================================
!||====================================================================
!||    rbe3fpen_ininp   ../engine/source/constraints/general/rbe3/rbe3pen_init.F90
!||--- called by ------------------------------------------------------
!||    rbe3pen_init     ../engine/source/constraints/general/rbe3/rbe3pen_init.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod     ../common_source/modules/constant_mod.F
!||    precision_mod    ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine rbe3fpen_ininp(                                      &
          ns      ,irot        ,numnod     ,nml         ,         &
          ms      ,in          ,stifn      ,stifr       ,         &
          iml     ,frbe3       ,x          ,rrbe3pen_f  ,         &
          rrbe3pen_stf,rrbe3pen_fac,rrbe3pen_vi,rrbe3pen_m  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod,          only : one,two,zero,zep05,em6,em20,third,fourth,four,ten,em10,em03
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                :: ns              !< reference node id
          integer, intent(in)                                :: irot            !< if rotational dof of ind nodes used
          integer, intent(in)                                :: numnod          !< number of nodes
          integer, intent(in)                                :: nml             !< number of independent nodes
          integer, dimension(nml),     intent(in   )         :: iml             !< independent node list
          real(kind=WP), dimension(6,nml),   intent(in   )         :: frbe3           !< normalized weight
          real(kind=WP), dimension(3,numnod),intent(in   )         :: x               !< coordinates
          real(kind=WP), dimension(numnod),  intent(in   )         :: ms              !< nodal mass
          real(kind=WP), dimension(numnod),  intent(in   )         :: in              !< nodal inertia
          real(kind=WP), dimension(numnod),  intent(in   )         :: stifn           !< nodal stifness
          real(kind=WP), dimension(numnod),  intent(in   )         :: stifr           !< nodal rotational stifness
          real(kind=WP), dimension(3),       intent(inout)         :: rrbe3pen_f      !< force
          real(kind=WP), dimension(2),       intent(inout)         :: rrbe3pen_stf    !< stiffness
          real(kind=WP),                     intent(inout)         :: rrbe3pen_fac    !< stiffness factor
          real(kind=WP),                     intent(inout)         :: rrbe3pen_vi     !< damping coefficient
          real(kind=WP), dimension(3),       intent(inout)         :: rrbe3pen_m      !< moment
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,m,icoline
          real(kind=WP), dimension(3) :: xbar,wri,rR,rn
          real(kind=WP) :: wi(nml),rndotrn,facn,facr,det,gamma(9),gminv(9),gamma_max,jgamma,wmax
          real(kind=WP) :: msbar,dk_m,rdummy,stfnm,stfrm,stif,damp,lsm2
          real(kind=WP) ::  srR(3,3),srRT(3,3),srn(3,3),omgsrn(3,3),A(3,3),Ar(3,3)
          double precision :: disdp(3)
! ======================================================================================================================
          xbar(1:3) = zero
          msbar = zero
          stfnm = zero
          stfrm = zero
          gminv(1:9) = -HUGE(gminv(1))
          do i=1,nml
            m = iml(i)
            wi(i) = frbe3(1,i)
            xbar(1:3) = xbar(1:3)+wi(i)*x(1:3,m)
            msbar = msbar+wi(i)*ms(m)
            stfnm = stfnm+wi(i)*stifn(m)
            stif = stifr(m)
            if (stif <=em20) then
              rn(1:3) = x(1:3,m)-xbar(1:3)
              rndotrn = rn(1)*rn(1)+rn(2)*rn(2)+rn(3)*rn(3)
              stif = stifn(m)*rndotrn
            end if
            stfrm  = stfrm + wi(i)*stif
          end do
!      print *,'stf_m,stf_s',stfrm,stifn(ns)
!
          disdp(1:3)= x(1:3,ns) - xbar(1:3)
          rR(1:3)   = disdp(1:3)
          rrbe3pen_f(1:3) = zero
          rrbe3pen_m(1:3) = zero
          lsm2 = rR(1)*rR(1)+rR(2)*rR(2)+rR(3)*rR(3)
!-------set up rrbe3pen_stf
          if (stifn(ns)<=em20) then ! free node w/ spc
            rrbe3pen_stf(1) = two*stfnm
            rrbe3pen_stf(2) = four*stfrm
          elseif (stifn(ns)<=em10) then ! small stif
            rrbe3pen_stf(1) = em03*stfnm*min(one,ms(ns)/msbar)
            rrbe3pen_stf(2) = em20 ! only have translation stif
          else
            rrbe3pen_stf(1) = stfnm*stifn(ns)/(stfnm+stifn(ns))
!         stfnr_p is fixed to rrbe3pen_stf(1,n_p)*rndotrn (Lsm2) excepting for spc case
            rrbe3pen_stf(2) = rrbe3pen_stf(1)*lsm2
          end if
!      print *,'pen_stf',rrbe3pen_stf(1:2)
!
!-------set up rrbe3pen_fac
!
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
          do i=1,nml
            m = iml(i)
            rn(1:3) = x(1:3,m)-xbar(1:3)
            rndotrn = rn(1)*rn(1)+rn(2)*rn(2)+rn(3)*rn(3)
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
          end do
          icoline = 0
          det =                                                          &
            (gamma(1)*(gamma(5)*gamma(9)-gamma(6)*gamma(8))-       &
            gamma(2)*(gamma(4)*gamma(9)-gamma(6)*gamma(7))+       &
            gamma(3)*(gamma(4)*gamma(8)-gamma(5)*gamma(7)))
!
          gamma_max = max(em20,gamma(1),gamma(5),gamma(9))
          if(abs(det/(gamma_max*gamma_max*gamma_max)) < em6) then!elements with colinear master nodes
!
!         if (irot==0) write(*,*)'error :RBE3: colinear master nodes' ! error out, add check in Starter if possible
            icoline = 1
!
          else
!
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
          end if
          facn = zero
          facr = zero
          if (icoline>0.and.irot>0) then
            do i=1,nml
              wri(1:3) = frbe3(4:6,i)
              wmax=max(wri(1),wri(2),wri(3))
!            facn = facn + wi(i)*wi(i)
              facr = facr + wmax*wmax
            end do
          else
!   S(rR) multi T
            srRT (1:3,1) = srR(1:3,1)*gminv(1)+srR(1:3,2)*gminv(2)+srR(1:3,3)*gminv(3)
            srRT (1:3,2) = srR(1:3,1)*gminv(4)+srR(1:3,2)*gminv(5)+srR(1:3,3)*gminv(6)
            srRT (1:3,3) = srR(1:3,1)*gminv(7)+srR(1:3,2)*gminv(8)+srR(1:3,3)*gminv(9)
            do i=1,nml
              m = iml(i)
              rn(1:3) = x(1:3,m)-xbar(1:3)
!         matrix S(rn) for each master node
              srn(1,1) =  zero
              srn(1,2) = -rn(3)
              srn(1,3) =  rn(2)
              srn(2,1) =  rn(3)
              srn(2,2) =  zero
              srn(2,3) = -rn(1)
              srn(3,1) = -rn(2)
              srn(3,2) =  rn(1)
              srn(3,3) =  zero
!         weight matrix X S(rn)
              omgsrn(1:3,1:3) = srn(1:3,1:3)*wi(i)
              A(1,1:3)=-srRT(1,1)*omgsrn(1,1:3)-srRT(1,2)*omgsrn(2,1:3)-srRT(1,3)*omgsrn(3,1:3)
              A(2,1:3)=-srRT(2,1)*omgsrn(1,1:3)-srRT(2,2)*omgsrn(2,1:3)-srRT(2,3)*omgsrn(3,1:3)
              A(3,1:3)=-srRT(3,1)*omgsrn(1,1:3)-srRT(3,2)*omgsrn(2,1:3)-srRT(3,3)*omgsrn(3,1:3)
              A(1,1) = A(1,1) +wi(i)
              A(2,2) = A(2,2) +wi(i)
              A(3,3) = A(3,3) +wi(i)
              Ar(1,1:3)= gminv(1)*omgsrn(1,1:3)+gminv(4)*omgsrn(2,1:3)+gminv(7)*omgsrn(3,1:3)
              Ar(2,1:3)= gminv(2)*omgsrn(1,1:3)+gminv(5)*omgsrn(2,1:3)+gminv(8)*omgsrn(3,1:3)
              Ar(3,1:3)= gminv(3)*omgsrn(1,1:3)+gminv(6)*omgsrn(2,1:3)+gminv(9)*omgsrn(3,1:3)
              facn =   facn +    max(                                      &
                A(1,1)*A(1,1)+A(2,1)*A(2,1)+A(3,1)*A(3,1),       &
                A(1,2)*A(1,2)+A(2,2)*A(2,2)+A(3,2)*A(3,2),       &
                A(1,3)*A(1,3)+A(2,3)*A(2,3)+A(3,3)*A(3,3))
              facr =   facr +    lsm2*max(                                 &
                Ar(1,1)*Ar(1,1)+Ar(2,1)*Ar(2,1)+Ar(3,1)*Ar(3,1),       &
                Ar(1,2)*Ar(1,2)+Ar(2,2)*Ar(2,2)+Ar(3,2)*Ar(3,2),       &
                Ar(1,3)*Ar(1,3)+Ar(2,3)*Ar(2,3)+Ar(3,3)*Ar(3,3))
            end do
!
          end if !if (icoline>0) then
          if (in(ns)>zero) then
            rrbe3pen_fac = max(one,(facn+facr))  ! solid/solid(shell)
            rrbe3pen_stf(2) = rrbe3pen_stf(2)/rrbe3pen_fac !instability concern
          else
            rrbe3pen_fac = max(one,facn)         ! shell/solid(shell)
          end if
!-------set up rrbe3pen_vi
          damp= zep05
          rdummy = one/ms(ns)+one/msbar
          dk_m = rrbe3pen_stf(1)/rdummy
          rrbe3pen_vi = two*damp*sqrt(dk_m)
!      print *,'rrbe3pen_d',rrbe3pen_d(1:3)

!
        end subroutine rbe3fpen_ininp
      end module rbe3pen_init_mod
