!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    fail_tab2_b_mod   ../engine/source/materials/fail/tabulated/fail_tab2_b.F90
!||--- called by ------------------------------------------------------
!||    fail_beam3        ../engine/source/elements/beam/fail_beam3.F
!||====================================================================
      module fail_tab2_b_mod
        implicit none
      contains
! ======================================================================================================================
! \brief   tab2 failure criteria for type3 beam elements
! \details multiple failure models with different combinations with strain rate, thermal or mesh size dependency.
! ======================================================================================================================

!||====================================================================
!||    fail_tab2_b             ../engine/source/materials/fail/tabulated/fail_tab2_b.F90
!||--- called by ------------------------------------------------------
!||    fail_beam3              ../engine/source/elements/beam/fail_beam3.F
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    elbufdef_mod            ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    fail_param_mod          ../common_source/modules/mat_elem/fail_param_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
        subroutine fail_tab2_b (fail,                               &
          nel     ,nuvar   ,nvartmp ,uvar    ,vartmp  ,             &
          time    ,ngl     ,aldt    ,dpla    ,epsp    ,             &
          f1      ,area    ,temp    ,off     ,dfmax   ,             &
          tdele   ,dmgscl  )
!c-----------------------------------------------
!                                                    modules
!c-----------------------------------------------

          use elbufdef_mod
          use constant_mod
          use precision_mod, only : WP
          use fail_param_mod
          use table_mat_vinterp_mod
!c-----------------------------------------------
!                                               c i m p l i c i t t y p e
!c-----------------------------------------------
          implicit none
#include      "units_c.inc"

!c-----------------------------------------------
!                                             c i n p u t a r g u m e n t s
!c-----------------------------------------------
          integer                     ,intent(in)     :: nel      ! size of element group
          integer                     ,intent(in)     :: nuvar    ! size of user variable array
          integer                     ,intent(in)     :: nvartmp  !
          integer, dimension(nel)     ,intent(in)     :: ngl      ! element identifiers
          integer, dimension(nel,nvartmp)   ,intent(inout)  :: vartmp

          real(kind=WP)                     ,intent(in)     :: time     ! current time
          real(kind=WP), dimension(nel)     ,intent(in)     :: aldt     ! time increment
          real(kind=WP), dimension(nel)     ,intent(in)     :: dpla     ! plastic strain
          real(kind=WP), dimension(nel)     ,intent(in)     :: epsp     ! strain rate
          real(kind=WP), dimension(nel)     ,intent(in)     :: temp     ! temperature

          real(kind=WP) ,dimension(nel)     ,intent(inout)  :: f1       ! force in local x direction
          real(kind=WP)                     ,intent(in)     :: area     !< cross section area
          real(kind=WP), dimension(nel, nuvar), intent(inout) :: uvar   ! user variables
          real(kind=WP), dimension(nel)     ,intent(inout)  :: dfmax    ! maximum damage
          real(kind=WP), dimension(nel)     ,intent(inout)  :: dmgscl
          real(kind=WP), dimension(nel)     ,intent(inout)  :: tdele    ! element deletion time
          real(kind=WP), dimension(nel)     ,intent(inout)  :: off      ! offset
          type (fail_param_)                ,intent(in)     :: fail     !< failure model data structure
!c-----------------------------------------------
!                                                  local variables
!c-----------------------------------------------
          integer :: i, j, nindx, itab_epsf, &
            itab_inst, itab_size, ireg, ndim, &
            log_scale1, log_scale2
          integer, dimension(nel) :: indx, iad, ilen
          real(kind=WP) :: fcrit, dn, dcrit, ecrit, exp_ref, expo, el_ref, &
            sr_ref1, fscale_el, shrf, biaxf, sr_ref2, &
            fscale_sr, cjc, fscale_dlim, temp_ref, fscale_temp,rgtr1,rgtr2
          real(kind=WP) :: dpl_def, cos3theta, det, p, svm, &
            sxx, syy, szz, reta
          real(kind=WP), dimension(nel) :: inst, dc, l0, triax, xi, epsf, epsl, &
            depsf, depsl, sizefac, ratefac, dsize, &
            softexp, dlim, tempfac, tempfac2, dft, var
          real(kind=WP), dimension(nel) :: lambda,dydx
          real(kind=WP), dimension(nel, 3) :: xvec

!c=======================================================================
!c=======================================================================
          !c user variables
          !c! user variable # 1,      instability damage
          !c! user variable # 2,      necking critical damage
          !c! user variable # 3,       element size
!c===============================================================================================
          !step1: recovering failure criterion parameters and initiation
          !=======================================================================
          ! - initialisation of computation on time step
          !=======================================================================
          ! recovering failure criterion parameters
          fcrit         = fail%uparam(1)                !> scale factor for failure plastic strain table
          dn            = fail%uparam(4)                !> damage accumulation exponent. default = 1.0 (real)
          dcrit         = fail%uparam(5)                !> critical damage for stress softening triggering. default = 0.0 (real)
          ecrit         = fail%uparam(6)                !> scale factor for necking plastic strain table identifier. (real)
          exp_ref       = fail%uparam(7)                !> reference element size for stress softening exponent function. default = 1.0 (real)
          expo          = fail%uparam(8)                !> scale factor for stress softening exponent function.default = 1.0 (real)
          ireg          = nint(fail%uparam(9))          !> regularization flag for element size. default = 1 (integer)
          el_ref        = fail%uparam(10)               !> reference element size for element size scaling table. default = 1.0 (real)
          sr_ref1       = fail%uparam(11)               !> reference strain rate for size scaling table. default = 1.0 (real)
          fscale_el     = fail%uparam(12)               !> scale factor for element size scaling function. default = 1.0 (real)
          shrf          = fail%uparam(13)               !> lower stress triaxiality boundary for element size scaling. default = -1.0 (real)
          biaxf         = fail%uparam(14)               !> upper stress triaxiality boundary for element size scaling. default = 1.0 (real)
          sr_ref2       = fail%uparam(15)               !> reference strain rate for strain rate dependency function. default = 1.0 (real)
          fscale_sr     = fail%uparam(16)               !> scale factor for strain rate dependency function. default = 1.0 (real)
          cjc           = fail%uparam(17)               !> johnson-cook strain rate dependency factor.
          fscale_dlim   = fail%uparam(18)               !> damage limit function scale factor. default = 1.0 (real)
          temp_ref      = fail%uparam(19)               !> reference temperature for temperature dependency function. default = 0.0 (integer)
          fscale_temp   = fail%uparam(20)               !> scale factor for temperature scaling function.
          log_scale1    = nint(fail%uparam(21))
          log_scale2    = nint(fail%uparam(22))
          rgtr1         = fail%uparam(24)
          rgtr1         = max(rgtr1,em06)
          rgtr2         = fail%uparam(25)
          rgtr2         = min(rgtr2,two_third - em06)
!c
          ! checking element failure and recovering user variable
          do i=1,nel
            ! if necking control is activated
            if (fail%table4d(6)%notable > 0.or. ecrit > zero) then
              if (uvar(i,2) == zero) uvar(i,2) = one
            else
              if (uvar(i,2) == zero) uvar(i,2) = dcrit
            endif
            ! instability damage
            inst(i) = uvar(i,1)
            ! necking critical damage
            dc(i)   = uvar(i,2)
          end do
!c
          !c
          !====================================================================
          ! - loop over the element to compute the stress state quantities
          !====================================================================
          do i=1,nel
!c
            ! computation of hydrostatic stress, von mises stress, and stress triaxiality
            p   = third*(f1(i)/area)
            sxx = f1(i)/area - p
            syy = 0. - p
            szz = 0. - p
            svm = half*(sxx**2 + syy**2 + szz**2)
            svm = sqrt(max(three*svm,zero))
            triax(i) = p/max(em20,svm)
            if (triax(i) < -one) triax(i) = -one
            if (triax(i) >  one) triax(i) = one
!c
            ! computation of lode parameter
            det  = sxx*syy*szz
            cos3theta = half*twenty7*det/max(em20,svm**3)
            if (cos3theta < -one) cos3theta = -one
            if (cos3theta > one)  cos3theta = one
            xi(i) = one - two*acos(cos3theta)/pi
!c
          end do
!c
          !====================================================================
          ! - compute factors for element size, strain rate and temperature
          !====================================================================
          ! at initial time, save the element size
          if (uvar(1,3) == zero) uvar(1:nel,3) = aldt(1:nel)
          l0(1:nel) = uvar(1:nel,3)
!c
          ! compute the softening exponent
          if (fail%table4d(1)%notable > 0) then
            lambda(1:nel) = l0(1:nel)/exp_ref
            call table_mat_vinterp(fail%table4d(1),nel,nel,vartmp(1:nel,1:1),lambda,softexp,dydx)
          else
            softexp(1:nel) = expo
          end if
!c
          ! compute the temperature dependency factor
          if (fail%table4d(4)%notable > 0) then
            lambda(1:nel) = temp(1:nel) / temp_ref
            call table_mat_vinterp(fail%table4d(4),nel,nel,vartmp(1:nel,4:4),lambda,tempfac,dydx)
          else
            tempfac(1:nel) = one
          endif
          tempfac2(1:nel) = tempfac(1:nel)
!c
          ! compute the element size regularization factor
          if (fail%table4d(7)%notable > 0) then
            ndim = fail%table4d(7)%ndim
            if (ireg == 1) then
              xvec(1:nel,2:3) = zero
              select case (ndim)
               case(1)            ! scale factor vs element size
                xvec(1:nel,1)   = l0(1:nel)/el_ref
               case(2)            ! scale factor vs element size vs strain rate
                xvec(1:nel,1)   = l0(1:nel)/el_ref
                if (log_scale1 > 0) then
                  do i = 1,nel
                    xvec(i,2) = log(max(epsp(i),em20)/sr_ref1)
                  enddo
                else
                  xvec(1:nel,2) = epsp(1:nel)/sr_ref1
                endif
              end select
            else if (ireg == 2) then
              xvec(1:nel,1)   = l0(1:nel)/el_ref
              xvec(1:nel,2)   = triax(1:nel)
              xvec(1:nel,3)   = xi(1:nel)
            end if  ! ireg
!
            call table_mat_vinterp(fail%table4d(7),nel,nel,vartmp(1:nel,11),xvec,sizefac,dydx)
!
            if (ireg == 1) then
              do i = 1,nel
                if (triax(i) < third) then
                  reta =  shrf*(one - min(max(triax(i),zero),rgtr1)/rgtr1)
                else !if (triax(i) >= third) then
                  reta = (three*biaxf/(three*rgtr2 - two))*                    &
                    (rgtr2 - min(max(triax(i),rgtr2),two_third))
                endif
                reta = max(zero, min(one, reta))
                sizefac(i) = sizefac(i) + reta*(one - sizefac(i))
              end do
            end if
          else
            sizefac(1:nel) = one
          end if
!c
          ! compute the strain rate dependency factor
          if (fail%table4d(2)%notable > 0) then
            if (log_scale2 > 0) then
              do i = 1,nel
                lambda(i) = log(max(epsp(i),em20)/sr_ref2)
              enddo
            else
              lambda(1:nel) = epsp(1:nel)/sr_ref2
            endif
            call table_mat_vinterp(fail%table4d(2),nel,nel,vartmp(1:nel,2:2),lambda,ratefac,dydx)
          else if (cjc > zero) then
            do i=1,nel
              if (epsp(i) > sr_ref2) then
                ratefac(i) = one + cjc*log(epsp(i)/sr_ref2)
              else
                ratefac(i) = one
              endif
            enddo
          else
            ratefac(1:nel) = one
          end if
          ! Compute the damage limit value
          if (fail%table4d(3)%notable > 0) then
            call table_mat_vinterp(fail%table4d(3),nel,nel,vartmp(1:nel,3:3),triax,dlim,dydx)
            do i = 1,nel
              dlim(i) = min(dlim(i),one)
              dlim(i) = max(dlim(i),zero)
            enddo
          else
            dlim(1:nel) = one
          endif
!c
          !====================================================================
          ! - computation of plastic strain at failure
          !====================================================================
          if (fail%table4d(5)%notable > 0) then
            xvec(1:nel,1)   = triax(1:nel)
            xvec(1:nel,2)   = xi(1:nel)
            xvec(1:nel,3)   = temp(1:nel)/temp_ref
            if (fail%table4d(5)%ndim == 3) tempfac(1:nel)  = one
            call table_mat_vinterp(fail%table4d(5),nel,nel,vartmp(1:nel,5),xvec,epsf,dydx)
          else
            epsf(1:nel) = fcrit
          end if
!c
          !====================================================================
          ! - computation of plastic strain at necking
          !====================================================================
          if (fail%table4d(6)%notable > 0) then     ! Instability plastic strain vs triaxiality vs Lode vs temperature
            xvec(1:nel,1)   = triax(1:nel)
            xvec(1:nel,2)   = xi(1:nel)
            xvec(1:nel,3)   = temp(1:nel)/temp_ref
            call table_mat_vinterp(fail%table4d(6),nel,nel,vartmp(1:nel,8),xvec,epsl,dydx)
            if (fail%table4d(6)%ndim == 3) tempfac2(1:nel) = one
          else
            epsl(1:nel) = ecrit
          end if
!c
          !====================================================================
          ! - computation of the damage variable evolution
          !====================================================================
          ! initialization of element failure index
          nindx = 0
          indx(1:nel) = 0
!c
          ! loop over the elements
          do i=1,nel
!c
            ! if the element is not broken
            if (off(i) ==  one .and. dpla(i) > zero) then
!c
              ! needs to initialize damage at a very small value the first time
              if (dfmax(i) == zero) dfmax(i) = em20
              if (inst(i)  == zero) inst(i)  = em20
!c
              ! compute failure strain damage variable
              dpl_def  = dpla(i)/max(epsf(i)*ratefac(i)*sizefac(i)*tempfac(i),em20)
              dfmax(i) = dfmax(i) + dpl_def*dn*(dfmax(i)**(one-(one/dn)))
              dfmax(i) = min(dfmax(i),dlim(i))
              if (dfmax(i) >= one) then
                nindx       = nindx + 1
                indx(nindx) = i
                off(i)    = zero
                tdele(i)  = time

              end if
!c
              ! compute the control necking instability damage
              if ((ecrit > zero)) then
                dpl_def = dpla(i)/max(epsl(i)*ratefac(i)*sizefac(i)*tempfac2(i),em20)
                inst(i) = inst(i) + dpl_def*dn*(inst(i)**(one-(one/dn)))
                inst(i) = min(inst(i),one)
                if ((inst(i) >= one).and.(dc(i) == one)) then
                  dc(i) = dfmax(i)
                end if
              end if
!c
            end if
          end do
!c
          !====================================================================
          ! - update uvar and the stress tensor
          !====================================================================
          do i = 1,nel
            !< Save necking instability damage
            uvar(i,1) = inst(i)
            !< Damage criterion for stress softening
            if (dfmax(i) >= dc(i)) then
              !< Damage softening scale computation
              if (dc(i) < one) then
                dmgscl(i) = one - ((dfmax(i)-dc(i))/max(one-dc(i),em20))**softexp(i)
              else
                dmgscl(i) = zero
              end if
            else
              dmgscl(i) = one
            endif
            !< Update necking critical damage
            uvar(i,2) = dc(i)
          end do
!c
          !====================================================================
          ! - printout data about failed elements
          !====================================================================
          if (nindx > 0) then
            do j=1,nindx
              i = indx(j)
              if (off(i) == zero) then
                write(iout, 2000) ngl(i),time
                write(istdo,2000) ngl(i),time
              end if
            end do
          end if
!c-----------------------------------------------------------------------
2000      format(1x,"-- RUPTURE OF BEAM ELEMENT :",i10,   &
            " AT TIME :",1pe12.4)
        end subroutine fail_tab2_b
      end module fail_tab2_b_mod
