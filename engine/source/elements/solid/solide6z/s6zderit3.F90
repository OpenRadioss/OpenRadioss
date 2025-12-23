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
!||    s6zderit3_mod   ../engine/source/elements/solid/solide6z/s6zderit3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3       ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zderit3_mod
      contains
        ! ======================================================================================================================
        ! \brief Compute derivatives and Jacobian matrix for 6-node solid element
        ! \details This routine calculates the Jacobian matrix, its inverse, and related
        !          shape function derivatives for 6-node solid elements. It also handles
        !          negative volume detection and correction.
        ! ======================================================================================================================
!||====================================================================
!||    s6zderit3        ../engine/source/elements/solid/solide6z/s6zderit3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg          ../engine/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    message_mod     ../engine/share/message_module/message_mod.F
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine s6zderit3(                                                    &
          OFF      ,DET      ,NGL      ,NEL      ,                                                             &
          xd1      ,xd2      ,xd3      ,xd4      ,xd5      ,xd6      ,           &
          yd1      ,yd2      ,yd3      ,yd4      ,yd5      ,yd6      ,           &
          zd1      ,zd2      ,zd3      ,zd4      ,zd5      ,zd6      ,           &
          px1      ,px2      ,px3      ,px4      ,px5      ,px6      ,           &
          py1      ,py2      ,py3      ,py4      ,py5      ,py6      ,           &
          pz1      ,pz2      ,pz3      ,pz4      ,pz5      ,pz6      ,           &
          jacob1   ,jacob2   ,jacob3   ,jacob4   ,jacob5   ,jacob6   ,           &
          jacob9   ,voldp)
!
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use message_mod
          use mvsiz_mod, only : mvsiz
          use precision_mod, only : wp
          use constant_mod, only : zero, one, two, third, fourth, one_over_8, one_over_12
!-------------------------------------------------------------------------------
!    I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none
#include      "units_c.inc"
!-------------------------------------------------------------------------------
!    D u m m y   a r g u m e n t s
!------------------------------------------------------------------------------
        integer,                       intent(in)    :: nel         !< Number of elements
        real(kind=wp), dimension(nel), intent(out)   :: off         !< Jacobian determinant
        real(kind=wp), dimension(nel), intent(INout)   :: det         !< Jacobian determinant
        integer, dimension(nel), intent(out)   :: ngl
        real(kind=wp), dimension(nel), intent(out)   :: px1         !< Shape function derivative dN1/dxi
        real(kind=wp), dimension(nel), intent(out)   :: px2         !< Shape function derivative dN2/dxi
        real(kind=wp), dimension(nel), intent(out)   :: px3         !< Shape function derivative dN3/dxi
        real(kind=wp), dimension(nel), intent(out)   :: px4         !< Shape function derivative dN4/dxi
        real(kind=wp), dimension(nel), intent(out)   :: px5         !< Shape function derivative dN5/dxi
        real(kind=wp), dimension(nel), intent(out)   :: px6         !< Shape function derivative dN6/dxi
        real(kind=wp), dimension(nel), intent(out)   :: py1         !< Shape function derivative dN1/deta
        real(kind=wp), dimension(nel), intent(out)   :: py2         !< Shape function derivative dN2/deta
        real(kind=wp), dimension(nel), intent(out)   :: py3         !< Shape function derivative dN3/deta
        real(kind=wp), dimension(nel), intent(out)   :: py4         !< Shape function derivative dN4/deta
        real(kind=wp), dimension(nel), intent(out)   :: py5         !< Shape function derivative dN5/deta
        real(kind=wp), dimension(nel), intent(out)   :: py6         !< Shape function derivative dN6/deta
        real(kind=wp), dimension(nel), intent(out)   :: pz1         !< Shape function derivative dN1/dzeta
        real(kind=wp), dimension(nel), intent(out)   :: pz2         !< Shape function derivative dN2/dzeta
        real(kind=wp), dimension(nel), intent(out)   :: pz3         !< Shape function derivative dN3/dzeta
        real(kind=wp), dimension(nel), intent(out)   :: pz4         !< Shape function derivative dN4/dzeta
        real(kind=wp), dimension(nel), intent(out)   :: pz5         !< Shape function derivative dN5/dzeta
        real(kind=wp), dimension(nel), intent(out)   :: pz6         !< Shape function derivative dN6/dzeta
        real(kind=wp), dimension(nel), intent(out)   :: jacob1      !< Jacobian matrix component J12
        real(kind=wp), dimension(nel), intent(out)   :: jacob2      !< Jacobian matrix component J22
        real(kind=wp), dimension(nel), intent(out)   :: jacob3      !< Jacobian matrix component J32
        real(kind=wp), dimension(nel), intent(out)   :: jacob4      !< Jacobian matrix component J13
        real(kind=wp), dimension(nel), intent(out)   :: jacob5      !< Jacobian matrix component J23
        real(kind=wp), dimension(nel), intent(out)   :: jacob6      !< Jacobian matrix component J33
        real(kind=wp), dimension(nel), intent(out)   :: jacob9      !< Jacobian matrix component J33
!C     ENSURE DOUBLE-PRECISION (64-BIT) FLOATING-POINT CALCULATIONS, EVEN WHEN COMPILING IN SINGLE-PRECISION MODE.
        real(kind=8), dimension(nel), intent(inout) :: xd1          !< X coordinate of node 1
        real(kind=8), dimension(nel), intent(inout) :: xd2          !< X coordinate of node 2
        real(kind=8), dimension(nel), intent(inout) :: xd3          !< X coordinate of node 3
        real(kind=8), dimension(nel), intent(inout) :: xd4          !< X coordinate of node 4
        real(kind=8), dimension(nel), intent(inout) :: xd5          !< X coordinate of node 5
        real(kind=8), dimension(nel), intent(inout) :: xd6          !< X coordinate of node 6
        real(kind=8), dimension(nel), intent(inout) :: yd1          !< Y coordinate of node 1
        real(kind=8), dimension(nel), intent(inout) :: yd2          !< Y coordinate of node 2
        real(kind=8), dimension(nel), intent(inout) :: yd3          !< Y coordinate of node 3
        real(kind=8), dimension(nel), intent(inout) :: yd4          !< Y coordinate of node 4
        real(kind=8), dimension(nel), intent(inout) :: yd5          !< Y coordinate of node 5
        real(kind=8), dimension(nel), intent(inout) :: yd6          !< Y coordinate of node 6
        real(kind=8), dimension(nel), intent(inout) :: zd1          !< Z coordinate of node 1
        real(kind=8), dimension(nel), intent(inout) :: zd2          !< Z coordinate of node 2
        real(kind=8), dimension(nel), intent(inout) :: zd3          !< Z coordinate of node 3
        real(kind=8), dimension(nel), intent(inout) :: zd4          !< Z coordinate of node 4
        real(kind=8), dimension(nel), intent(inout) :: zd5          !< Z coordinate of node 5
        real(kind=8), dimension(nel), intent(inout) :: zd6          !< Z coordinate of node 6
        real(kind=8), dimension(nel),  intent(out)   :: voldp        !< Global element volume
!-------------------------------------------------------------------------------
!    L o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i, j, icor, nnega                            !< Loop counters and flags
          integer :: index(nel)                                   !< Index array for negative volume elements
!C     ENSURE DOUBLE-PRECISION (64-BIT) FLOATING-POINT CALCULATIONS, EVEN WHEN COMPILING IN SINGLE-PRECISION MODE.
          real(kind=8) :: dett(nel)                              !< Inverse determinant
          real(kind=8) :: jac1(nel), jac2(nel), jac3(nel)        !< Jacobian matrix components
          real(kind=8) :: jac4(nel), jac5(nel), jac6(nel)        !< Jacobian matrix components
          real(kind=8) :: jac7(nel), jac8(nel), jac9(nel)        !< Jacobian matrix components
          real(kind=8) :: jaci1, jaci2, jaci3                    !< Jacobian inverse components
          real(kind=8) :: jaci4, jaci5, jaci6                    !< Jacobian inverse components
          real(kind=8) :: jaci7, jaci8, jaci9                    !< Jacobian inverse components
          real(kind=8) :: jaci12, jaci45, jaci78                 !< Combined inverse components
          real(kind=8) :: x21(nel), x31(nel), x54(nel), x64(nel) !< Coordinate differences
          real(kind=8) :: y21(nel), y31(nel), y54(nel), y64(nel) !< Coordinate differences
          real(kind=8) :: z21(nel), z31(nel), z54(nel), z64(nel) !< Coordinate differences
          real(kind=8) :: x41(nel), y41(nel), z41(nel)           !< Coordinate differences
          real(kind=8) :: jac_59_68(nel), jac_67_49(nel), jac_48_57(nel) !< Cross products
!-------------------------------------------------------------------------------
!
          !< Coordinate differences      
          do i = 1, nel
            x21(i) = xd2(i) - xd1(i)
            x31(i) = xd3(i) - xd1(i)
            x41(i) = xd4(i) - xd1(i)
            x54(i) = xd5(i) - xd4(i)
            x64(i) = xd6(i) - xd4(i)
!
            y21(i) = yd2(i) - yd1(i)
            y31(i) = yd3(i) - yd1(i)
            y41(i) = yd4(i) - yd1(i)
            y54(i) = yd5(i) - yd4(i)
            y64(i) = yd6(i) - yd4(i)
!
            z21(i) = zd2(i) - zd1(i)
            z31(i) = zd3(i) - zd1(i)
            z41(i) = zd4(i) - zd1(i)
            z54(i) = zd5(i) - zd4(i)
            z64(i) = zd6(i) - zd4(i)
          enddo
!
          !< Jacobian matrix components
          do i=1,nel
!  -------ri.xi---->ksi--------
            jac1(i) = x21(i) + x54(i)
            jac2(i) = y21(i) + y54(i)
            jac3(i) = z21(i) + z54(i)
!  -------si.xi--->eta--------
            jac4(i) = x31(i) + x64(i)
            jac5(i) = y31(i) + y64(i)
            jac6(i) = z31(i) + z64(i)
!  -------ti.xi----zeta-------
            jac7(i) = third*(x41(i) + xd5(i) - xd2(i) + xd6(i) - xd3(i))
            jac8(i) = third*(y41(i) + yd5(i) - yd2(i) + yd6(i) - yd3(i))
            jac9(i) = third*(z41(i) + zd5(i) - zd2(i) + zd6(i) - zd3(i))
          enddo
!
          !< Store Jacobian components for output
          do i=1,nel
            jacob1(i) = jac1(i)
            jacob2(i) = jac2(i)
            jacob3(i) = jac3(i)            
            jacob4(i) = jac4(i)
            jacob5(i) = jac5(i)
            jacob6(i) = jac6(i)
            jacob9(i) = jac9(i)
          enddo
!
          !< Compute determinant using scalar triple product
          do i=1,nel
            jac_59_68(i) = jac5(i)*jac9(i) - jac6(i)*jac8(i)
            jac_67_49(i) = jac6(i)*jac7(i) - jac4(i)*jac9(i)
            jac_48_57(i) = jac4(i)*jac8(i) - jac5(i)*jac7(i)
          end do
!
          !< Determinant
          do i = 1, nel
              voldp(i) = one_over_8*(jac1(i) * jac_59_68(i) + jac2(i) * jac_67_49(i) + jac3(i) * jac_48_57(i))
              det(i) = voldp(i)
          end do

!C
      CALL SCHKJAB3(                &
         OFF,     DET,     NGL,     NEL)
!C

! 
          !< Jacobian inverse matrix
          do i = 1, nel
            dett(i) = one_over_8 / det(i)
!
            jaci1 = dett(i) * jac_59_68(i)
            jaci4 = dett(i) * jac_67_49(i)
            jaci7 = dett(i) * jac_48_57(i)
            jaci2 = dett(i) * (-jac2(i) * jac9(i) + jac3(i) * jac8(i))
            jaci5 = dett(i) * ( jac1(i) * jac9(i) - jac3(i) * jac7(i))
            jaci8 = dett(i) * (-jac1(i) * jac8(i) + jac2(i) * jac7(i))
            jaci3 = dett(i) * ( jac2(i) * jac6(i) - jac3(i) * jac5(i))
            jaci6 = dett(i) * (-jac1(i) * jac6(i) + jac3(i) * jac4(i))
            jaci9 = dett(i) * ( jac1(i) * jac5(i) - jac2(i) * jac4(i))
!
            jaci12 = jaci1 + jaci2
            jaci45 = jaci4 + jaci5
            jaci78 = jaci7 + jaci8
!
            px1(i) = -jaci12 - third * jaci3
            py1(i) = -jaci45 - third * jaci6
            pz1(i) = -jaci78 - third * jaci9
!
            px4(i) = -jaci12 + third * jaci3
            py4(i) = -jaci45 + third * jaci6
            pz4(i) = -jaci78 + third * jaci9
!
            px2(i) = jaci1 - third * jaci3
            py2(i) = jaci4 - third * jaci6
            pz2(i) = jaci7 - third * jaci9
!
            px5(i) = jaci1 + third * jaci3
            py5(i) = jaci4 + third * jaci6
            pz5(i) = jaci7 + third * jaci9
!
            px3(i) = jaci2 - third * jaci3
            py3(i) = jaci5 - third * jaci6
            pz3(i) = jaci8 - third * jaci9
!
            px6(i) = jaci2 + third * jaci3
            py6(i) = jaci5 + third * jaci6
            pz6(i) = jaci8 + third * jaci9
          enddo
!
        end subroutine s6zderit3
      end module s6zderit3_mod
