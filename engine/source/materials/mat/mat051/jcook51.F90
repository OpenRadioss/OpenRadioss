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
!||    jcook51_mod   ../engine/source/materials/mat/mat051/jcook51.F90
!||--- called by ------------------------------------------------------
!||    sigeps51      ../engine/source/materials/mat/mat051/sigeps51.F90
!||====================================================================
      module jcook51_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!||====================================================================
!||    jcook51         ../engine/source/materials/mat/mat051/jcook51.F90
!||--- called by ------------------------------------------------------
!||    sigeps51        ../engine/source/materials/mat/mat051/sigeps51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine jcook51(nel   ,sigd     ,plas   ,temp   ,vol  , &
          deps  ,epd      ,uparam ,volume ,eint , &
          de    , &
          vfrac )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use constant_mod, only : em02, em06, em15, em20, zero, half, one, two, three, three_half, hundred
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Include Files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: NEL
          real(kind=wp), intent(in) :: DEPS(6,NEL)
          real(kind=wp), intent(in) :: TEMP(NEL)
          real(kind=wp), intent(in) :: EPD(NEL)
          real(kind=wp), intent(in) :: VOL(NEL)
          real(kind=wp), intent(in) :: UPARAM(*)
          real(kind=wp), intent(in) :: VOLUME(NEL)
          real(kind=wp), intent(inout) :: SIGD(6,NEL)
          real(kind=wp), intent(inout) :: EINT(NEL)
          real(kind=wp), intent(inout) :: PLAS(NEL)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: I
          real(kind=wp) :: G2,G3,VM,DPLA,R,DY,CMX,TSTAR
          real(kind=wp) :: G,Y0,B,N,CC,EPDR,CM,TMELT,THETL,PLAMX,SIGMX
          real(kind=wp) :: VM2X,T0,EPS0,BEPS0PNM1,CTE,FACT,EINC,BETA
          real(kind=wp) :: Y(NEL),H(NEL),VM2(NEL),DE(NEL),SIGDO(6,NEL),VFRAC(NEL),VOL_AVG
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Source Lines
! ----------------------------------------------------------------------------------------------------------------------
          g         = uparam(1)
          y0        = uparam(2)
          b         = uparam(3)
          n         = uparam(4)
          cc        = uparam(5)
          epdr      = uparam(6)
          cm        = uparam(7)
          tmelt     = uparam(8)
          thetl     = uparam(9)
          plamx     = uparam(10)
          sigmx     = uparam(11)
          t0        = uparam(13)
          g2        = two*g
          g3        = three*g
          vm2x      = zero
          eps0      = em06
          beps0pnm1 = b * eps0**n / eps0
          beta      = zero

          do i = 1,nel
            plas(i) = max(plas(i),zero)
            if(plas(i) >= plamx) then
              y(i)    = zero
              h(i)    = zero
            else if(temp(i) >= tmelt) then
              !plas(i) = zero
              y(i)    = zero
              h(i)    = zero
            else if(vfrac(i) <= em02) then
              !plas(i) = zero
              y(i)    = zero
              h(i)    = zero
            else
              cte = one
              !temperature
              if(cm /= zero.and.temp(i) > t0) then
                tstar = (temp(i)-t0)/(tmelt-t0)
                cmx   = cm
                if(temp(i) > thetl) cmx = one
                cte   = one - tstar**cmx
              end if
              !strain rate depedency
              if(cc /= zero .and. epd(i) > epdr) then
                cte   = cte * (one + cc * log(epd(i)/epdr))
              end if
              if(plas(i) <= em20)then
                !h(i)  = cte * beps0pnm1
                !y(i)  = cte * y0  + h(i) * plas(i)
                dy    = zero
                y(i)  = cte * (y0 + dy)
                h(i)  = zero
              else
                dy    = b * plas(i)**n
                y(i)  = cte * (y0 + dy)
                h(i)  = cte * n * dy / plas(i)
              end if
              if(y(i) > sigmx)then
                y(i)  = sigmx
                h(i)  = zero
              end if
            end if
          end do

          do i = 1,nel
            fact         = vfrac(i)
            sigdo(1:6,i) = sigd(1:6,i)
            !--------------
            ! elastic increment
            !--------------
            if(vfrac(i) > two*em02)then
              sigd(1,i)    = sigd(1,i) + g2 * (deps(1,i)-de(i))*fact
              sigd(2,i)    = sigd(2,i) + g2 * (deps(2,i)-de(i))*fact
              sigd(3,i)    = sigd(3,i) + g2 * (deps(3,i)-de(i))*fact
              sigd(4,i)    = sigd(4,i) + g  * deps(4,i)*fact
              sigd(5,i)    = sigd(5,i) + g  * deps(5,i)*fact
              sigd(6,i)    = sigd(6,i) + g  * deps(6,i)*fact
            end if
            !--------------
            ! von mises
            !--------------
            !s_vm = sqrt(3.J2),  J2 = 1/2 (s1**2 + s2**2 + s3**2) + (s4**2 + s5**2 + s6**2)   where s is deviator
            vm2(i) = three_half * ( sigd(1,i)*sigd(1,i) + sigd(2,i)*sigd(2,i) + sigd(3,i)*sigd(3,i) ) &
              +three * ( sigd(4,i)*sigd(4,i) + sigd(5,i)*sigd(5,i) + sigd(6,i)*sigd(6,i) )
            vm = sqrt(vm2(i))
          END DO
          do i = 1,nel
            if(temp(i) > tmelt)then
              sigd(1,i) = zero
              sigd(2,i) = zero
              sigd(3,i) = zero
              sigd(4,i) = zero
              sigd(5,i) = zero
              sigd(6,i) = zero
              plas(i) = zero
            else if(VFRAC(I) > two*em02) then
              !--------------
              ! criterion
              !--------------
              r = one-em02 ! 1-epsilon
              if(vm2(i) > y(i)*y(i) .and. y(i) /= zero)then
                VM      = SQRT(VM2(I))
                DPLA    = (VM-Y(I)) / (G3+H(I))
                PLAS(I) = PLAS(I) + DPLA
                R       = (Y(I) + H(I) * DPLA) / MAX(VM,EM15)
              end if
              if(y(i) == zero) r = zero
              !--------------
              ! projection
              !--------------
              sigd(1,i) = sigd(1,i) * r
              sigd(2,i) = sigd(2,i) * r
              sigd(3,i) = sigd(3,i) * r
              sigd(4,i) = sigd(4,i) * r
              sigd(5,i) = sigd(5,i) * r
              sigd(6,i) = sigd(6,i) * r
              !--------------
              ! plastic energy
              !--------------
              vol_avg = half*(vfrac(i)*volume(i)+vol(i))
              einc    = half*vol_avg* &
                ( (sigdo(1,i)+sigd(1,i)) * deps(1,i) &
                + (sigdo(2,i)+sigd(2,i)) * deps(2,i) &
                + (sigdo(3,i)+sigd(3,i)) * deps(3,i) &
                + (sigdo(4,i)+sigd(4,i)) * deps(4,i) &
                + (sigdo(5,i)+sigd(5,i)) * deps(5,i) &
                + (sigdo(6,i)+sigd(6,i)) * deps(6,i) )
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
              r = (vfrac(i)-em02)*hundred
              plas(i) = r * plas(i)
              if(vm2(i) > y(i)*y(i))then
                !smooth transition vfrac \in [0.01 0.02]
                VM      = SQRT(VM2(I))
                DPLA    = (VM-Y(I)) / (G3+H(I))
                R       = r*(Y(I) + H(I) * DPLA) / MAX(VM,EM15)
              end if
              if(y(i) ==zero)r = zero
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
          end do

          return
        end subroutine jcook51
      end module jcook51_mod


