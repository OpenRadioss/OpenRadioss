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
!||    sc8dlenmax_sm_mod   ../engine/source/elements/thickshell/solidec/sc8dlenmax_sm.F90
!||--- called by ------------------------------------------------------
!||    s8cforc3            ../engine/source/elements/thickshell/solide8c/s8cforc3.F
!||    scforc3             ../engine/source/elements/thickshell/solidec/scforc3.F
!||====================================================================
      module sc8dlenmax_sm_mod
      implicit none
      contains
! ======================================================================================================================
! \brief compute some geometric parameters of hexahedron thick shell in case of small strain
! ======================================================================================================================
!||====================================================================
!||    sc8dlenmax_sm   ../engine/source/elements/thickshell/solidec/sc8dlenmax_sm.F90
!||--- called by ------------------------------------------------------
!||    s8cforc3        ../engine/source/elements/thickshell/solide8c/s8cforc3.F
!||    scforc3         ../engine/source/elements/thickshell/solidec/scforc3.F
!||--- calls      -----------------------------------------------------
!||    sdlensh14       ../engine/source/elements/thickshell/solide8c/sdlensh14.F
!||    sdlensh2        ../engine/source/elements/thickshell/solidec/sdlensh2.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine sc8dlenmax_sm(                         &
          nel      ,x        ,numnod     ,vol     ,       &
          nc1      ,nc2      ,nc3        ,nc4     ,       &
          nc5      ,nc6      ,nc7        ,nc8     ,       &
          llsh     ,area     ,icsig      )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : one_over_64
          use precision_mod, only : WP
          use mvsiz_mod , only : mvsiz
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                    :: nel             !< number of elements
          integer, intent(in)                                    :: icsig           !< thickness direction flag
          integer, intent(in)                                    :: numnod          !< number of nodes
          integer, dimension(mvsiz), intent(in   )               :: nc1             !< n1_id
          integer, dimension(mvsiz), intent(in   )               :: nc2             !< n2_id
          integer, dimension(mvsiz), intent(in   )               :: nc3             !< n3_id
          integer, dimension(mvsiz), intent(in   )               :: nc4             !< n4_id
          integer, dimension(mvsiz), intent(in   )               :: nc5             !< n5_id
          integer, dimension(mvsiz), intent(in   )               :: nc6             !< n6_id
          integer, dimension(mvsiz), intent(in   )               :: nc7             !< n7_id
          integer, dimension(mvsiz), intent(in   )               :: nc8             !< n8_id
          real(kind=WP), dimension(mvsiz), intent(inout)         :: vol             !< volume
          real(kind=WP), dimension(mvsiz), intent(inout)         :: llsh            !< maximum length of thick shell
          real(kind=WP), dimension(mvsiz), intent(inout)         :: area            !< area of thick shell
          real(kind=WP), dimension(3,numnod), intent(in)         :: x               !< coordinates
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          real(kind=WP), dimension(nel) ::                    &
            x1,      x2,     x3,     x4,                      &
            y1,      y2,     y3,     y4,                      &
            z1,      z2,     z3,     z4,                      &
            x5,      x6,     x7,     x8,                      &
            y5,      y6,     y7,     y8,                      &
            z5,      z6,     z7,     z8
          real(kind=WP) :: x17, x28, x35, x46, y17, y28, y35, y46, z17, z28, z35, z46, &
                           jac1,jac2,jac3,jac4,jac5,jac6,jac7,jac8,jac9,               &
                           a17,a28,b17,b28,c17,c28,jac_59_68, jac_67_49, jac_48_57
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
          if (icsig==0) then
            call sdlensh2(vol,llsh ,area ,                        &
                        x1, x2, x3, x4, x5, x6, x7, x8,           &
                        y1, y2, y3, y4, y5, y6, y7, y8,           &
                        z1, z2, z3, z4, z5, z6, z7, z8,nel)
          else 
            call sdlensh14(vol,llsh ,area ,                       &
                        x1, x2, x3, x4, x5, x6, x7, x8,           &
                        y1, y2, y3, y4, y5, y6, y7, y8,           &
                        z1, z2, z3, z4, z5, z6, z7, z8,icsig,nel)
          end if
          do i=1,nel   ! recompute volume as rho is no more precise due to type22
            x17=x7(i)-x1(i)
            x28=x8(i)-x2(i)
            x35=x5(i)-x3(i)
            x46=x6(i)-x4(i)
            y17=y7(i)-y1(i)
            y28=y8(i)-y2(i)
            y35=y5(i)-y3(i)
            y46=y6(i)-y4(i)
            z17=z7(i)-z1(i)
            z28=z8(i)-z2(i)
            z35=z5(i)-z3(i)
            z46=z6(i)-z4(i)
            jac1=x17+x28-x35-x46
            jac2=y17+y28-y35-y46
            jac3=z17+z28-z35-z46
            a17=x17+x46
            a28=x28+x35
            b17=y17+y46
            b28=y28+y35
            c17=z17+z46
            c28=z28+z35
            jac4=a17+a28
            jac5=b17+b28
            jac6=c17+c28
            jac7=a17-a28
            jac8=b17-b28
            jac9=c17-c28
            jac_59_68=jac5*jac9-jac6*jac8
            jac_67_49=jac6*jac7-jac4*jac9
            jac_48_57=jac4*jac8-jac5*jac7
            vol(i)=one_over_64*(jac1*jac_59_68+jac2*jac_67_49+jac3*jac_48_57)
          end do  
!
        end subroutine sc8dlenmax_sm
! ----------------------------------------------------------------------------------------------------------------------
      end module sc8dlenmax_sm_mod