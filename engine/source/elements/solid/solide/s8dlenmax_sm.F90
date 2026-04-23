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
!||====================================================================
      module s8dlenmax_sm_mod
      implicit none
      contains
! ======================================================================================================================
! \brief compute some geometric parameters of hexahedron in case of small strain
! ======================================================================================================================
        subroutine s8dlenmax_sm(                          &
          nel      ,rho0     ,vol0       ,x       ,       &
          nc1      ,nc2      ,nc3        ,nc4     ,       &
          nc5      ,nc6      ,nc7        ,nc8     ,       &
          l_max    ,rho      ,vol        ,l_min   ,       &
          numnod )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : em20
          use precision_mod, only : WP
          use mvsiz_mod , only : mvsiz
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                    :: nel             !< number of elements
          integer, intent(in)                                    :: numnod          !< number of nodes
          integer, dimension(mvsiz), intent(in   )               :: nc1             !< n1_id
          integer, dimension(mvsiz), intent(in   )               :: nc2             !< n2_id
          integer, dimension(mvsiz), intent(in   )               :: nc3             !< n3_id
          integer, dimension(mvsiz), intent(in   )               :: nc4             !< n4_id
          integer, dimension(mvsiz), intent(in   )               :: nc5             !< n5_id
          integer, dimension(mvsiz), intent(in   )               :: nc6             !< n6_id
          integer, dimension(mvsiz), intent(in   )               :: nc7             !< n7_id
          integer, dimension(mvsiz), intent(in   )               :: nc8             !< n8_id
          real(kind=WP), dimension(nel),   intent(in   )         :: rho             !< density
          real(kind=WP), dimension(nel),   intent(in   )         :: vol0            !< initial volume
          real(kind=WP), dimension(mvsiz), intent(inout)         :: vol             !< volume
          real(kind=WP), dimension(mvsiz), intent(inout)         :: l_max           !< maximum length
          real(kind=WP), dimension(mvsiz), intent(inout)         :: l_min           !< minimumlength
          real(kind=WP), dimension(3,numnod), intent(in)         :: x               !< coordinates
          real(kind=WP), intent(in   )                           :: rho0            !< initial density
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
          real(kind=WP), dimension(mvsiz) ::                  &
            x1,      x2,     x3,     x4,                      &
            y1,      y2,     y3,     y4,                      &
            z1,      z2,     z3,     z4,                      &
            x5,      x6,     x7,     x8,                      &
            y5,      y6,     y7,     y8,                      &
            z5,      z6,     z7,     z8
! ======================================================================================================================
            do i=1,nel
              x1(i) =x(1,nc1(i))
              y1(i) =x(2,nc1(i))
              z1(i) =x(3,nc1(i))
              x2(i) =x(1,nc2(i))
              y2(i) =x(2,nc2(i))
              z2(i) =x(3,nc2(i))
              x3(i) =x(1,nc3(i))
              y3(i) =x(2,nc3(i))
              z3(i) =x(3,nc3(i))
              x4(i) =x(1,nc4(i))
              y4(i) =x(2,nc4(i))
              z4(i) =x(3,nc4(i))
              x5(i) =x(1,nc5(i))
              y5(i) =x(2,nc5(i))
              z5(i) =x(3,nc5(i))
              x6(i) =x(1,nc6(i))
              y6(i) =x(2,nc6(i))
              z6(i) =x(3,nc6(i))
              x7(i) =x(1,nc7(i))
              y7(i) =x(2,nc7(i))
              z7(i) =x(3,nc7(i))
              x8(i) =x(1,nc8(i))
              y8(i) =x(2,nc8(i))
              z8(i) =x(3,nc8(i))
            enddo
!
           call sdlenmax(l_max,                                    &
                         x1,      x2,      x3,      x4,            &
                         x5,      x6,      x7,      x8,            &
                         y1,      y2,      y3,      y4,            &
                         y5,      y6,      y7,      y8,            &
                         z1,      z2,      z3,      z4,            &
                         z5,      z6,      z7,      z8,            &
                         nel)
            do i=1,nel
              vol(i) = vol0(i)*rho0/max(em20,rho(i))
              l_min(i) = vol(i)/l_max(i)**2
            end do  
!
        end subroutine s8dlenmax_sm
! ----------------------------------------------------------------------------------------------------------------------
      end module s8dlenmax_sm_mod
