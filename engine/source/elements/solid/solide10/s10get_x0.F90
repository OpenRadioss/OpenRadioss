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
!||    s10get_x0_mod   ../engine/source/elements/solid/solide10/s10get_x0.F90
!||--- called by ------------------------------------------------------
!||    s10forc3        ../engine/source/elements/solid/solide10/s10forc3.F
!||====================================================================
      module s10get_x0_mod
      implicit none
      contains
! ======================================================================================================================
! \brief get initial configuration by displacement
! ======================================================================================================================
!||====================================================================
!||    s10get_x0       ../engine/source/elements/solid/solide10/s10get_x0.F90
!||--- called by ------------------------------------------------------
!||    s10forc3        ../engine/source/elements/solid/solide10/s10forc3.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine s10get_x0(                                   &
          nel      ,numnod   ,x          ,xdp     ,       &
          d        ,xx0      ,yy0        ,zz0     ,       &
          nc       )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : half
          use precision_mod, only : WP
          use mvsiz_mod , only : mvsiz
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                    :: nel             !< number of elements
          integer, intent(in)                                    :: numnod          !< number of nodes
          integer, dimension(mvsiz,10), intent(in)               :: nc              !< element connectivity
          real(kind=WP), dimension(3,numnod), intent(in)         :: x               !< nodal coordinates
          real(kind=WP), dimension(3,numnod), intent(in)         :: d               !< nodal displacements
          double precision, dimension(3,numnod), intent(in)      :: xdp             !< nodal coordinates dp
          double precision, dimension(mvsiz,10), intent(inout)   :: xx0, yy0, zz0   !< nodal coordinates in initial configuration
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,iperm1(10),iperm2(10),n,n1,n2,nn
          data iperm1/0,0,0,0,1,2,3,1,2,3/
          data iperm2/0,0,0,0,2,3,1,4,4,4/
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
!
          if (WP==4)then  !sp
            do n=1,4
              do i=1,nel
                nn = nc(i,n)
                xx0(i,n) = xdp(1,nn) - d(1,nn)
                yy0(i,n) = xdp(2,nn) - d(2,nn)
                zz0(i,n) = xdp(3,nn) - d(3,nn)
              end do
            end do
!
            do n=5,10
              do i=1,nel
                nn = nc(i,n)
                if(nn/=0)then
                  xx0(i,n) = xdp(1,nn) - d(1,nn)
                  yy0(i,n) = xdp(2,nn) - d(2,nn)
                  zz0(i,n) = xdp(3,nn) - d(3,nn)
                else
                  n1=iperm1(n)
                  n2=iperm2(n)
                  xx0(i,n) = half*(xx0(i,n1)+xx0(i,n2))
                  yy0(i,n) = half*(yy0(i,n1)+yy0(i,n2))
                  zz0(i,n) = half*(zz0(i,n1)+zz0(i,n2))
                end if
              end do
            end do
          else !dp
            do n=1,4
              do i=1,nel
                nn = nc(i,n)
                xx0(i,n) = x(1,nn) - d(1,nn)
                yy0(i,n) = x(2,nn) - d(2,nn)
                zz0(i,n) = x(3,nn) - d(3,nn)
              end do
            end do
            do n=5,10
              do i=1,nel
                nn = nc(i,n)
                if(nn/=0)then
                  xx0(i,n) = x(1,nn) - d(1,nn)
                  yy0(i,n) = x(2,nn) - d(2,nn)
                  zz0(i,n) = x(3,nn) - d(3,nn)
                else
                  n1=iperm1(n)
                  n2=iperm2(n)
                  xx0(i,n) = half*(xx0(i,n1)+xx0(i,n2))
                  yy0(i,n) = half*(yy0(i,n1)+yy0(i,n2))
                  zz0(i,n) = half*(zz0(i,n1)+zz0(i,n2))
                end if
              end do
            end do
          end if ! (WP==4)then
!----------------------------
        end subroutine s10get_x0
!-------------------
      end module s10get_x0_mod
