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
!||    granular51_mod   ../engine/source/materials/mat/mat051/granular51.F90
!||--- called by ------------------------------------------------------
!||    sigeps51         ../engine/source/materials/mat/mat051/sigeps51.F90
!||====================================================================
      module granular51_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!||====================================================================
!||    granular51              ../engine/source/materials/mat/mat051/granular51.F90
!||--- called by ------------------------------------------------------
!||    sigeps51                ../engine/source/materials/mat/mat051/sigeps51.F90
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
        subroutine granular51 &
          (nel     ,sigd     ,vol      ,epseq  , &
          deps    ,uparam   ,volume   ,eint   , plas    , &
          uvar    ,nuvar    ,kk       ,rho0   , &
          pfrac   ,pp       ,gg1      , &
          pext    ,timestep ,de       , nummat , matparam , &
          nvartmp ,vartmp   ,vfrac)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use constant_mod , only : zero, em20, em15, em14, em02, third, half, one, onep333, two, three, hundred
          use matparam_def_mod , only : matparam_struct_
          use table_mat_vinterp_mod , only : table_mat_vinterp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: nel
          integer,intent(in) :: nummat
          real(kind=wp), intent(in) :: rho0
          real(kind=wp), intent(in) :: pfrac !< fracture pressure (pfrac <= 0)
          real(kind=wp), intent(in) :: vfrac(nel) !< volume fractions
          real(kind=wp), intent(in) :: deps(6,nel)
          real(kind=wp), intent(in) :: vol(nel),uparam(*) ,volume(nel)
          real(kind=wp), intent(in) :: timestep, pext , de(nel)
          real(kind=wp), intent(inout) :: plas(nel)                !< accumulated plastic strain
          real(kind=wp),intent(inout) :: sigd(6,nel),pp(nel),eint(nel)
          real(kind=wp),intent(inout) :: gg1(nel) !< shear modulus (variable with granular constitutive model)
          type(matparam_struct_),intent(in),dimension(nummat) :: matparam
          integer ,intent(in) :: nvartmp                       !< number of temporary internal variables
          integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp    !< temporary internal variables
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,kk,nuvar
          real(kind=wp) :: fact,fac2
          real(kind=wp) :: uvar(nel,nuvar)
          real(kind=wp) :: mas, ptot
          real(kind=wp) :: young, nu
          real(kind=wp) :: pold(nel)
          real(kind=wp) :: t1(nel), t2(nel), t3(nel), t4(nel), t5(nel), t6(nel)
          real(kind=wp) :: p(nel), r
          real(kind=wp) :: g(nel), g43(nel), g0(nel), g2(nel)
          real(kind=wp) :: j2(nel),yield2(nel), epseq(nel)
          real(kind=wp) :: rho_new(nel), rho_old(nel),vnew(nel),sigdo(6,nel)
          real(kind=wp) :: einc,vol_avg,vm,sig_y,dpla
          real(kind=WP), dimension(nel,1) :: xvec1 !<temporary array for table interpolation
          real(kind=WP) :: slope(nel,1) !<required for table interpolation
          integer :: mid
          logical, parameter :: opt_extrapolate = .false.
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          young = uparam(02)
          mid   = int(uparam(14))
          nu    = uparam(22)

          ! if(timestep==zero)return  !law3 is treeting this also on NC=0   (Einc must be added )
          do i=1,nel
            pold(i)    = uvar(i,18+kk)
            rho_old(i) = uvar(i,12+kk)
            vnew(i)    = uvar(i,1+kk)*volume(i) -timestep*uvar(i,13+kk)
            vnew(i)    = min(max(zero,vnew(i)),volume(i))
            if(vnew(i) > em15)then
              rho_new(i) = uvar(i,9+kk) / vnew(i)  !mass/volume
            else
              rho_new(i)=rho_old(i)
            end if
          end do

          !========================================================================
          !< Recovering Yield surface value Y = Y(P)
          !========================================================================
          !
          xvec1(1:nel,1) = Pold(1:nel)
          ! matparam%table(1) : G(rho) function
          ! ivartmp(1,1) : backup index poisition to optimize search during next cycle
          ! xvec1 : abscissa for table interpolation (in)
          ! g : ordinate for table interpolation (out)
          ! slope : slope for table interpolation (out)
          call table_mat_vinterp(MATPARAM(MID)%table(2),nel,nel,vartmp(1,1),xvec1,g0,slope,opt_extrapolate)


          !========================================================================
          !< Recovering shear modulus for each element : G = G(rho)
          !========================================================================
          !
          xvec1(1:nel,1) = rho_new(1:nel)
          ! matparam%table(1) : G(rho) function
          ! ivartmp(1,1) : backup index poisition to optimize search during next cycle
          ! xvec1 : abscissa for table interpolation (in)
          ! shear : ordinate for table interpolation (out)
          ! slope : slope for table interpolation (out)
          call table_mat_vinterp(MATPARAM(MID)%table(1),nel,nel,vartmp(1,1),xvec1,g,slope,opt_extrapolate)
          g2(1:nel) = two*g(1:nel)
          g43(1:nel)  = onep333*g(1:nel)
          gg1(1:nel) = g2(1:nel)

          do i=1,nel
            mas = uvar(i,9+kk)
            if(mas < em20)then
              rho_new(i) = rho0
            end if

            t1(i)=sigd(1,i)
            t2(i)=sigd(2,i)
            t3(i)=sigd(3,i)
            t4(i)=sigd(4,i)
            t5(i)=sigd(5,i)
            t6(i)=sigd(6,i)

            fac2 = one
            if(pold(i) <= pfrac)then
              p(i)  = pfrac
              fac2 = zero
            end if
            pp(i) = p(i)

            sigdo(1:6,i) = sigd(1:6,i)
            fact = vfrac(i)

            if(vfrac(i) > two*em02)then
              t1(i) = t1(i) + g2(i)* (deps(1,i)-de(i))*fact
              t2(i) = t2(i) + g2(i)* (deps(2,i)-de(i))*fact
              t3(i) = t3(i) + g2(i)* (deps(3,i)-de(i))*fact
              t4(i) = t4(i) + g(i) * deps(4,i)*fact
              t5(i) = t5(i) + g(i) * deps(5,i)*fact
              t6(i) = t6(i) + g(i) * deps(6,i)*fact
            end if
            j2(i)=half*(t1(i)**2+t2(i)**2+t3(i)**2)+t4(i)**2+t5(i)**2+t6(i)**2
            vm=sqrt(three*j2(i))
            ptot = pold(i)+pext
            g0(i)= max(zero,g0(i))
            sig_y=g0(i)
            g0(i)=third*g0(i)*g0(i)
            if(pold(i) <= pfrac) g0(i) = zero
            yield2(i)=j2(i)-g0(i)

            if(vfrac(i) > two*em02) then

              r = zero
              if (yield2(i) > zero)then
                if(yield2(i) >= zero)then
                  r = sig_y/(vm+ EM14)
                  dpla = (one - r)*vm  /max(three*g(i),em15)
                  plas(i)  = plas(i)  + dpla
                  epseq(i) = epseq(i) + dpla
                end if
              else
                r = one-em02 ! 1-epsilon
              end if

              ! deviatoric stress (projection)
              sigd(1,i) = t1(i) * r
              sigd(2,i) = t2(i) * r
              sigd(3,i) = t3(i) * r
              sigd(4,i) = t4(i) * r
              sigd(5,i) = t5(i) * r
              sigd(6,i) = t6(i) * r

              !plastic work
              vol_avg = half*(vfrac(i)*volume(i)+vol(i))
              einc    = half*vol_avg* &
                ( (sigdo(1,i)+sigd(1,i)) * deps(1,i) &
                + (sigdo(2,i)+sigd(2,i)) * deps(2,i) &
                + (sigdo(3,i)+sigd(3,i)) * deps(3,i) &
                + (sigdo(4,i)+sigd(4,i)) * deps(4,i) &
                + (sigdo(5,i)+sigd(5,i)) * deps(5,i) &
                + (sigdo(6,i)+sigd(6,i)) * deps(6,i))
              eint(i) = eint(i) + einc

            else if(vfrac(i) < em02)then
              plas(i) = zero
              sigd(1,i) = zero
              sigd(2,i) = zero
              sigd(3,i) = zero
              sigd(4,i) = zero
              sigd(5,i) = zero
              sigd(6,i) = zero
            else
              !smooth transition vfrac \in [0.01 0.02]
              r = (vfrac(i)-em02)*hundred
              plas(i) = r * plas(i)
              if(yield2(i) >= zero)then
                r = r * sig_y/(vm+em14)
              end if
              if(g0(i)  ==zero) r = zero
              !--------------
              ! projection
              !--------------
              sigd(1,i) = sigd(1,i) * r
              sigd(2,i) = sigd(2,i) * r
              sigd(3,i) = sigd(3,i) * r
              sigd(4,i) = sigd(4,i) * r
              sigd(5,i) = sigd(5,i) * r
              sigd(6,i) = sigd(6,i) * r
            end if

          end do !next i

          return
        end subroutine granular51
      end module granular51_mod


