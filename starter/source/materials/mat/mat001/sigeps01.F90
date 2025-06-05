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
      !||    sigeps01_mod   ../starter/source/materials/mat/mat001/sigeps01.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw          ../starter/source/materials/mat_share/mulaw.F
      !||====================================================================
      module sigeps01_mod
        implicit none
      contains
! ===============================================================================================================
!  \brief elastic material in starter with both incremental&total formulation
! ===============================================================================================================
      !||====================================================================
      !||    sigeps01        ../starter/source/materials/mat/mat001/sigeps01.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw           ../starter/source/materials/mat_share/mulaw.F
      !||--- calls      -----------------------------------------------------
      !||    valpvecdp       ../starter/source/materials/tools/matrix.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine sigeps01(                                          &
          nel     ,pm      , npropm  , rho   , rho0  , ismstr,        &
          depsxx  , depsyy , depszz  , depsxy, depsyz, depszx,        &
          epsxx   , epsyy  , epszz   , epsxy , epsyz , epszx ,        &
          sigoxx  , sigoyy , sigozz  , sigoxy, sigoyz, sigozx,        &
          signxx  , signyy , signzz  , signxy, signyz, signzx,        &
          soundsp , viscmax)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : one , zero,two,half,third,onep333
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                  :: nel             !< number of elements in the group
          integer, intent(in)                  :: ismstr          !< strain formulation flag
          integer, intent(in)                  :: npropm          !< 1er dimension of pm
!
          real(kind=WP), dimension(npropm),intent(in):: pm              !< material data
          real(kind=WP), dimension(nel), intent(in)  :: rho0            !< initial material density
          real(kind=WP), dimension(nel), intent(in)  :: rho             !< current material density
          real(kind=WP), dimension(nel), intent(in)  :: depsxx          !< strain increment xx
          real(kind=WP), dimension(nel), intent(in)  :: depsyy          !< strain increment yy
          real(kind=WP), dimension(nel), intent(in)  :: depszz          !< strain increment zz
          real(kind=WP), dimension(nel), intent(in)  :: depsxy          !< strain increment xy
          real(kind=WP), dimension(nel), intent(in)  :: depsyz          !< strain increment yz
          real(kind=WP), dimension(nel), intent(in)  :: depszx          !< strain increment zx
          real(kind=WP), dimension(nel), intent(in)  :: epsxx           !< total strain xx
          real(kind=WP), dimension(nel), intent(in)  :: epsyy           !< total strain yy
          real(kind=WP), dimension(nel), intent(in)  :: epszz           !< total strain zz
          real(kind=WP), dimension(nel), intent(in)  :: epsxy           !< total strain xy
          real(kind=WP), dimension(nel), intent(in)  :: epsyz           !< total strain yz
          real(kind=WP), dimension(nel), intent(in)  :: epszx           !< total strain zx
          real(kind=WP), dimension(nel), intent(in)  :: sigoxx          !< old stress xx
          real(kind=WP), dimension(nel), intent(in)  :: sigoyy          !< old stress yy
          real(kind=WP), dimension(nel), intent(in)  :: sigozz          !< old stress zz
          real(kind=WP), dimension(nel), intent(in)  :: sigoxy          !< old stress xy
          real(kind=WP), dimension(nel), intent(in)  :: sigoyz          !< old stress yz
          real(kind=WP), dimension(nel), intent(in)  :: sigozx          !< old stress zx
          real(kind=WP), dimension(nel), intent(out) :: signxx          !< new stress xx
          real(kind=WP), dimension(nel), intent(out) :: signyy          !< new stress yy
          real(kind=WP), dimension(nel), intent(out) :: signzz          !< new stress zz
          real(kind=WP), dimension(nel), intent(out) :: signxy          !< new stress xy
          real(kind=WP), dimension(nel), intent(out) :: signyz          !< new stress yz
          real(kind=WP), dimension(nel), intent(out) :: signzx          !< new stress zx
          real(kind=WP), dimension(nel),intent(inout):: soundsp         !< sound speed
          real(kind=WP), dimension(nel),intent(inout):: viscmax         !< maximum viscosity
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          real(kind=WP) :: c1,g,g2,ekk,p,dav,pold,pnew
          real(kind=WP), dimension(3,mvsiz) :: sigprv
          real(kind=WP), dimension(mvsiz,3) :: ev,evv
          real(kind=WP), dimension(6,mvsiz) :: av
          real(kind=WP), dimension(3,3,mvsiz) :: dirprv
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          c1   =  pm(32)
          g    =  pm(22)
          g2   =  two*g

          select case (ismstr)
           case (10,12)
            do i = 1,nel
              av(1,i) = epsxx(i)
              av(2,i) = epsyy(i)
              av(3,i) = epszz(i)
              av(4,i) = epsxy(i)*half
              av(5,i) = epsyz(i)*half
              av(6,i) = epszx(i)*half
            enddo
!
            call valpvecdp(av,evv,dirprv,nel)
            do i=1,nel
              ev(i,1:3)=half*log(evv(i,1:3)+ one)
              ekk=ev(i,1)+ev(i,2)+ev(i,3)
              dav=-third*ekk
              p = c1*ekk
              sigprv(1:3,i)=p +g2*(ev(i,1:3)+dav)
            enddo
            do i=1,nel
              ! transform principal cauchy stresses to global directions
              signxx(i) = dirprv(1,1,i)*dirprv(1,1,i)*sigprv(1,i)        &
                + dirprv(1,2,i)*dirprv(1,2,i)*sigprv(2,i)        &
                + dirprv(1,3,i)*dirprv(1,3,i)*sigprv(3,i)
!
              signyy(i) = dirprv(2,2,i)*dirprv(2,2,i)*sigprv(2,i)        &
                + dirprv(2,3,i)*dirprv(2,3,i)*sigprv(3,i)        &
                + dirprv(2,1,i)*dirprv(2,1,i)*sigprv(1,i)
!
              signzz(i) = dirprv(3,3,i)*dirprv(3,3,i)*sigprv(3,i)        &
                + dirprv(3,1,i)*dirprv(3,1,i)*sigprv(1,i)        &
                + dirprv(3,2,i)*dirprv(3,2,i)*sigprv(2,i)
!
              signxy(i) = dirprv(1,1,i)*dirprv(2,1,i)*sigprv(1,i)        &
                + dirprv(1,2,i)*dirprv(2,2,i)*sigprv(2,i)        &
                + dirprv(1,3,i)*dirprv(2,3,i)*sigprv(3,i)
!
              signyz(i) = dirprv(2,2,i)*dirprv(3,2,i)*sigprv(2,i)        &
                + dirprv(2,3,i)*dirprv(3,3,i)*sigprv(3,i)        &
                + dirprv(2,1,i)*dirprv(3,1,i)*sigprv(1,i)
!
              signzx(i) = dirprv(3,3,i)*dirprv(1,3,i)*sigprv(3,i)        &
                + dirprv(3,1,i)*dirprv(1,1,i)*sigprv(1,i)        &
                + dirprv(3,2,i)*dirprv(1,2,i)*sigprv(2,i)
            enddo
            soundsp(1:nel)  =sqrt((onep333*g+c1)/rho(1:nel))

           case (11)
            do i=1,nel
              ekk=epsxx(i)+epsyy(i)+epszz(i)
              dav=-third*ekk
              p = c1*ekk
              signxx(i) = p+g2*(epsxx(i)+dav)
              signyy(i) = p+g2*(epsyy(i)+dav)
              signzz(i) = p+g2*(epszz(i)+dav)
              signxy(i) = g*epsxy(i)
              signyz(i) = g*epsyz(i)
              signzx(i) = g*epszx(i)
            enddo
            soundsp(1:nel)  =sqrt((onep333*g+c1)/rho0(1:nel))
           case default         !incremental
            do i=1,nel
              pold = third*(sigoxx(i)+sigoyy(i)+sigozz(i))
              pnew = -c1*(rho(i)/rho0(i)-one)
              p = pnew - pold
              dav=-third*(depsxx(i)+depsyy(i)+depszz(i))
              signxx(i) = sigoxx(i)+ p +g2*(depsxx(i)+dav)
              signyy(i) = sigoyy(i)+ p +g2*(depsyy(i)+dav)
              signzz(i) = sigozz(i)+ p +g2*(depszz(i)+dav)
              signxy(i) = sigoxy(i)+ g*depsxy(i)
              signyz(i) = sigoyz(i)+ g*depsyz(i)
              signzx(i) = sigozx(i)+ g*depszx(i)
            enddo
            soundsp(1:nel)  =sqrt((onep333*g+c1)/rho0(1:nel))
          end select
          viscmax(1:nel) = zero
!-----------------------------------------------------------------
        end subroutine sigeps01
      end module sigeps01_mod
