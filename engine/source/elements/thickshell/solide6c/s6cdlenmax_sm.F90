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
!||    s6cdlenmax_sm_mod   ../engine/source/elements/thickshell/solide6c/s6cdlenmax_sm.F90
!||--- called by ------------------------------------------------------
!||    s6cforc3            ../engine/source/elements/thickshell/solide6c/s6cforc3.F
!||====================================================================
      module s6cdlenmax_sm_mod
      implicit none
      contains
! ======================================================================================================================
! \brief compute some geometric parameters of penta6 thick shell in case of small strain
! ======================================================================================================================
!||====================================================================
!||    s6cdlenmax_sm   ../engine/source/elements/thickshell/solide6c/s6cdlenmax_sm.F90
!||--- called by ------------------------------------------------------
!||    s6cforc3        ../engine/source/elements/thickshell/solide6c/s6cforc3.F
!||--- calls      -----------------------------------------------------
!||    sdlensh3n2      ../engine/source/elements/thickshell/solide6c/sdlensh3n2.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine s6cdlenmax_sm(                         &
          nel      ,x        ,numnod     ,vol     ,       &
          nc1      ,nc2      ,nc3        ,nc4     ,       &
          nc5      ,nc6      ,llsh       ,area   )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : third,one_over_8
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
          real(kind=WP), dimension(mvsiz), intent(inout)         :: vol             !< volume
          real(kind=WP), dimension(mvsiz), intent(inout)         :: llsh            !< maximum length of thick shell
          real(kind=WP), dimension(mvsiz), intent(inout)         :: area            !< area of thick shell
          real(kind=WP), dimension(3,numnod), intent(in)         :: x               !< coordinates
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
          real(kind=WP), dimension(nel) ::                    &
            x1,      x2,     x3,     x4,                      &
            y1,      y2,     y3,     y4,                      &
            z1,      z2,     z3,     z4,                      &
            x5,      x6,     y5,     y6,                      &
            z5,      z6
          real(kind=WP) :: x21,x31,x41,x54,x64,y21,y31,y41,y54,y64,z21,z31,z41,z54,z64,    &
            jac1,jac2,jac3,jac4,jac5,jac6,jac7,jac8,jac9, jac_59_68,jac_67_49,jac_48_57
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
            enddo
!
            call sdlensh3n2(vol,llsh ,area ,                        &
                          x1, x2, x3, x4, x5, x6,                   &
                          y1, y2, y3, y4, y5, y6,                   &
                          z1, z2, z3, z4, z5, z6,nel)
          do i=1,nel   ! recompute volume as rho is no more precise due to type22
            x21=x2(i)-x1(i)
            x31=x3(i)-x1(i)
            x41=x4(i)-x1(i)
            x54=x5(i)-x4(i)
            x64=x6(i)-x4(i)
            y21=y2(i)-y1(i)
            y31=y3(i)-y1(i)
            y41=y4(i)-y1(i)
            y54=y5(i)-y4(i)
            y64=y6(i)-y4(i)
            z21=z2(i)-z1(i)
            z31=z3(i)-z1(i)
            z41=z4(i)-z1(i)
            z54=z5(i)-z4(i)
            z64=z6(i)-z4(i)
            jac1=x21+x54
            jac2=y21+y54
            jac3=z21+z54
            jac4=x31+x64
            jac5=y31+y64
            jac6=z31+z64
            jac7=third*(x41+x5(i)-x2(i)+x6(i)-x3(i))
            jac8=third*(y41+y5(i)-y2(i)+y6(i)-y3(i))
            jac9=third*(z41+z5(i)-z2(i)+z6(i)-z3(i))
            jac_59_68=jac5*jac9-jac6*jac8
            jac_67_49=jac6*jac7-jac4*jac9
            jac_48_57=jac4*jac8-jac5*jac7
            vol(i) = one_over_8*(jac1*jac_59_68+jac2*jac_67_49+jac3*jac_48_57)
          end do
!
        end subroutine s6cdlenmax_sm
! ----------------------------------------------------------------------------------------------------------------------
      end module s6cdlenmax_sm_mod