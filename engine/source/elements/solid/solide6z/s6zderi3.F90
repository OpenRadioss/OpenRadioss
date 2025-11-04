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
!||    s6zderi3_mod   ../engine/source/elements/solid/solide6z/s6zderi3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3       ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zderi3_mod
      contains
      ! ======================================================================================================================
      ! \brief Compute derivatives and Jacobian matrix for 6-node solid element
      ! \details This routine calculates the Jacobian matrix, its inverse, and related
      !          shape function derivatives for 6-node solid elements. It also handles
      !          negative volume detection and correction.
      ! ======================================================================================================================
!||====================================================================
!||    s6zderi3        ../engine/source/elements/solid/solide6z/s6zderi3.F90
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
      subroutine s6zderi3(                                                     &
        off      ,det      ,ngl      ,                                         &
        x1       ,x2       ,x3       ,x4       ,x5       ,x6       ,           &
        y1       ,y2       ,y3       ,y4       ,y5       ,y6       ,           &
        z1       ,z2       ,z3       ,z4       ,z5       ,z6       ,           &
        px1      ,px2      ,px3      ,px4      ,px5      ,px6      ,           &
        py1      ,py2      ,py3      ,py4      ,py5      ,py6      ,           &
        pz1      ,pz2      ,pz3      ,pz4      ,pz5      ,pz6      ,           &
        jacob5   ,jacob6   ,jacob4   ,jacob8   ,jacob9   ,jacob7   ,           &
        vzl      ,volg     ,sav      ,offg     ,nel      ,ismstr   ,           &
        idel7nok ,ineg_v   ,mstop    ,volmin   ,idtmin   )  
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
!-------------------------------------------------------------------------------
      integer,                       intent(in)    :: ismstr      !< Material strain option flag
      integer,                       intent(in)    :: nel         !< Number of elements
      integer,       dimension(nel), intent(in)    :: ngl         !< Global element numbers
      real(kind=wp), dimension(nel), intent(inout) :: off         !< Element deactivation flag
      real(kind=wp), dimension(nel), intent(out)   :: det         !< Jacobian determinant
      real(kind=wp), dimension(nel), intent(inout) :: x1          !< X coordinate of node 1
      real(kind=wp), dimension(nel), intent(inout) :: x2          !< X coordinate of node 2
      real(kind=wp), dimension(nel), intent(inout) :: x3          !< X coordinate of node 3
      real(kind=wp), dimension(nel), intent(inout) :: x4          !< X coordinate of node 4
      real(kind=wp), dimension(nel), intent(inout) :: x5          !< X coordinate of node 5
      real(kind=wp), dimension(nel), intent(inout) :: x6          !< X coordinate of node 6
      real(kind=wp), dimension(nel), intent(inout) :: y1          !< Y coordinate of node 1
      real(kind=wp), dimension(nel), intent(inout) :: y2          !< Y coordinate of node 2
      real(kind=wp), dimension(nel), intent(inout) :: y3          !< Y coordinate of node 3
      real(kind=wp), dimension(nel), intent(inout) :: y4          !< Y coordinate of node 4
      real(kind=wp), dimension(nel), intent(inout) :: y5          !< Y coordinate of node 5
      real(kind=wp), dimension(nel), intent(inout) :: y6          !< Y coordinate of node 6
      real(kind=wp), dimension(nel), intent(inout) :: z1          !< Z coordinate of node 1
      real(kind=wp), dimension(nel), intent(inout) :: z2          !< Z coordinate of node 2
      real(kind=wp), dimension(nel), intent(inout) :: z3          !< Z coordinate of node 3
      real(kind=wp), dimension(nel), intent(inout) :: z4          !< Z coordinate of node 4
      real(kind=wp), dimension(nel), intent(inout) :: z5          !< Z coordinate of node 5
      real(kind=wp), dimension(nel), intent(inout) :: z6          !< Z coordinate of node 6
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
      real(kind=wp), dimension(nel), intent(out)   :: jacob4      !< Jacobian matrix component J12
      real(kind=wp), dimension(nel), intent(out)   :: jacob5      !< Jacobian matrix component J22
      real(kind=wp), dimension(nel), intent(out)   :: jacob6      !< Jacobian matrix component J32
      real(kind=wp), dimension(nel), intent(out)   :: jacob7      !< Jacobian matrix component J13
      real(kind=wp), dimension(nel), intent(out)   :: jacob8      !< Jacobian matrix component J23
      real(kind=wp), dimension(nel), intent(out)   :: jacob9      !< Jacobian matrix component J33  
      real(kind=wp), dimension(nel), intent(out)   :: vzl         !< Local volume change rate
      real(kind=wp), dimension(nel), intent(out)   :: volg        !< Global element volume
      real(kind=wp), dimension(nel), intent(inout) :: offg        !< Global element deactivation flag
      real(kind=8), intent(in)                     :: sav(nel,15) !< Saved nodal coordinates for negative volume recovery sav must be in double precision, so kind = 8
      integer, intent(inout)                       :: idel7nok  
      integer, intent(inout)                       :: ineg_v
      integer, intent(inout)                       :: mstop
      real(kind=wp), intent(in)                    :: volmin
      integer,dimension(102)                       :: idtmin
!-------------------------------------------------------------------------------
!    L o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
      integer :: i, j, icor, nnega                            !< Loop counters and flags
      integer :: index(nel)                                   !< Index array for negative volume elements  
      real(kind=wp) :: dett(nel)                              !< Inverse determinant
      real(kind=wp) :: jac1(nel), jac2(nel), jac3(nel)        !< Jacobian matrix components
      real(kind=wp) :: jac4(nel), jac5(nel), jac6(nel)        !< Jacobian matrix components
      real(kind=wp) :: jac7(nel), jac8(nel), jac9(nel)        !< Jacobian matrix components    
      real(kind=wp) :: jaci1, jaci2, jaci3                    !< Jacobian inverse components
      real(kind=wp) :: jaci4, jaci5, jaci6                    !< Jacobian inverse components
      real(kind=wp) :: jaci7, jaci8, jaci9                    !< Jacobian inverse components
      real(kind=wp) :: jaci12, jaci45, jaci78                 !< Combined inverse components 
      real(kind=wp) :: x21(nel), x31(nel), x54(nel), x64(nel) !< Coordinate differences
      real(kind=wp) :: y21(nel), y31(nel), y54(nel), y64(nel) !< Coordinate differences
      real(kind=wp) :: z21(nel), z31(nel), z54(nel), z64(nel) !< Coordinate differences
      real(kind=wp) :: x41(nel), y41(nel), z41(nel)           !< Coordinate differences
      real(kind=wp) :: jac_59_68(nel), jac_67_49(nel), jac_48_57(nel) !< Cross products
!-------------------------------------------------------------------------------
!
      !< Coordinate differences
      do i = 1, nel
        x21(i) = x2(i) - x1(i)
        x31(i) = x3(i) - x1(i)
        x41(i) = x4(i) - x1(i)
        x54(i) = x5(i) - x4(i)
        x64(i) = x6(i) - x4(i)
!
        y21(i) = y2(i) - y1(i)
        y31(i) = y3(i) - y1(i)
        y41(i) = y4(i) - y1(i)
        y54(i) = y5(i) - y4(i)
        y64(i) = y6(i) - y4(i)
!  
        z21(i) = z2(i) - z1(i)
        z31(i) = z3(i) - z1(i)
        z41(i) = z4(i) - z1(i)
        z54(i) = z5(i) - z4(i)
        z64(i) = z6(i) - z4(i)
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
        jac7(i) = third*(x41(i) + x5(i) - x2(i) + x6(i) - x3(i))
        jac8(i) = third*(y41(i) + y5(i) - y2(i) + y6(i) - y3(i))
        jac9(i) = third*(z41(i) + z5(i) - z2(i) + z6(i) - z3(i))
      enddo
!
      !< Store Jacobian components for output
      do i=1,nel
        jacob4(i) = jac4(i)
        jacob5(i) = jac5(i)
        jacob6(i) = jac6(i)
        jacob7(i) = jac7(i)
        jacob8(i) = jac8(i)
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
        det(i) = one_over_8*(jac1(i) * jac_59_68(i) + jac2(i) * jac_67_49(i) + jac3(i) * jac_48_57(i))
      end do
!
      !< Check for minimum volume conditions and handle negative volumes
      nnega = 0
      if (idtmin(1) == 1) then
        icor = 0
        do i = 1, nel
          if (off(i) == zero) then
            det(i) = one
          elseif ((det(i) <= volmin) .or. (det(i) <= zero)) then
            icor = 1
          endif
        enddo
        if (icor > 0) then
          do i = 1, nel
            if (off(i) /= zero) then
              if (det(i) <= volmin) then
                det(i) = one
                off(i) = zero
                write(istdo, 2000) ngl(i)
                write(iout , 2000) ngl(i)
              elseif (det(i) <= zero) then
                call ancmsg(msgid=166, anmode=aninfo, i1=ngl(i))
                mstop = 1
              endif
            endif
          enddo
        endif
      elseif (idtmin(1) == 2) then
        icor = 0
        do i = 1, nel
          if (off(i) == zero) then
            det(i) = one
          elseif ((det(i) <= volmin) .or. (det(i) <= zero)) then
            icor = 1
          endif
        enddo
        if (icor > 0) then
          do i = 1, nel
            if ((off(i) /= zero) .and. (det(i) <= volmin .or. det(i) <= zero)) then
              det(i) = one
              off(i) = zero
              write(istdo, 2000) ngl(i)
              write(iout, 2000) ngl(i)
              idel7nok = 1
            endif
          enddo
        endif
      elseif (ismstr /= 4) then
        icor = 0
        do i = 1, nel
          if (off(i) == zero) then
            det(i) = one
          elseif ((det(i) <= volmin) .or. (det(i) <= zero)) then
            icor = 1
          endif
        enddo
        if (icor > 0) then
          do i = 1, nel
            if (off(i) == zero) then
              det(i) = one
            elseif (offg(i) > one) then
              !< Element already flagged - skip processing
            elseif ((det(i) <= volmin) .or. (det(i) <= zero)) then
              nnega = nnega + 1
              index(nnega) = i
              write(istdo, 3000) ngl(i)
              write(iout, 3000) ngl(i)
            endif
          enddo
          if (ineg_v == 0) then
            call ancmsg(msgid=280, anmode=aninfo)
            mstop = 1
          endif
        end if
      else
        !< Handle case where ismstr == 4
        icor = 0
        do i = 1, nel
          if (off(i) == zero) then
            det(i) = one
          elseif (det(i) <= zero) then
            icor = 1
          endif
        enddo
        if (icor > 0) then
          do i = 1, nel
            if (off(i) /= zero) then
              if (det(i) <= zero) then
                call ancmsg(msgid=166, anmode=aninfo, i1=ngl(i))
                mstop = 1
              endif
            endif
          enddo
        endif
      endif
!
      !< Process elements with negative volumes for coordinate recovery
      if (nnega > 0) then
        do j = 1, nnega
          i = index(j)
          x1(i) = sav(i,1)
          y1(i) = sav(i,2)
          z1(i) = sav(i,3)
          x2(i) = sav(i,4)
          y2(i) = sav(i,5)
          z2(i) = sav(i,6)
          x3(i) = sav(i,7)
          y3(i) = sav(i,8)
          z3(i) = sav(i,9)
          x4(i) = sav(i,10)
          y4(i) = sav(i,11)
          z4(i) = sav(i,12)
          x5(i) = sav(i,13)
          y5(i) = sav(i,14)
          z5(i) = sav(i,15)
          x6(i) = zero
          y6(i) = zero
          z6(i) = zero
!
          x21(i) = x2(i) - x1(i)
          x31(i) = x3(i) - x1(i)
          x41(i) = x4(i) - x1(i)
          x54(i) = x5(i) - x4(i)
          x64(i) = x6(i) - x4(i)
          y21(i) = y2(i) - y1(i)
          y31(i) = y3(i) - y1(i)
          y41(i) = y4(i) - y1(i)
          y54(i) = y5(i) - y4(i)
          y64(i) = y6(i) - y4(i)
          z21(i) = z2(i) - z1(i)
          z31(i) = z3(i) - z1(i)
          z41(i) = z4(i) - z1(i)
          z54(i) = z5(i) - z4(i)
          z64(i) = z6(i) - z4(i)
!
          jac1(i) = x21(i) + x54(i)
          jac2(i) = y21(i) + y54(i)
          jac3(i) = z21(i) + z54(i)
!
          jac4(i) = x31(i) + x64(i)
          jac5(i) = y31(i) + y64(i)
          jac6(i) = z31(i) + z64(i)
          jac7(i) = third * (x41(i) + x5(i) - x2(i) + x6(i) - x3(i))
          jac8(i) = third * (y41(i) + y5(i) - y2(i) + y6(i) - y3(i))
          jac9(i) = third * (z41(i) + z5(i) - z2(i) + z6(i) - z3(i))
!
          jacob4(i) = jac4(i)
          jacob5(i) = jac5(i)
          jacob6(i) = jac6(i)
          jacob7(i) = jac7(i)
          jacob8(i) = jac8(i)
          jacob9(i) = jac9(i)
!
          jac_59_68(i) = jac5(i) * jac9(i) - jac6(i) * jac8(i)
          jac_67_49(i) = jac6(i) * jac7(i) - jac4(i) * jac9(i)
          jac_48_57(i) = jac4(i) * jac8(i) - jac5(i) * jac7(i)
!
          det(i) = one_over_8 * (jac1(i) * jac_59_68(i) + jac2(i) * jac_67_49(i) + jac3(i) * jac_48_57(i))
          offg(i) = two
        enddo
      endif
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
      do i = 1, nel
        vzl(i) = fourth * (jacob9(i) * ( &
          x54(i) * y64(i) - x21(i) * y31(i) - x64(i) * y54(i) + x31(i) * y21(i)) &
          - jacob8(i) * ( &
          x54(i) * z64(i) + x31(i) * z21(i) - x21(i) * z31(i) - x64(i) * z54(i)) &
          + jacob7(i) * ( &
          y54(i) * z64(i) + y31(i) * z21(i) - y21(i) * z31(i) - y64(i) * z54(i))) 
        volg(i) = det(i)
      enddo
!
2000 format(/' zero or negative volume : delete 3d-element nb',i10/)
3000 format(/' zero or negative volume : 3d-element nb:', i10, /, &
            'solid-shell element is switched to small strain option'/)
    end subroutine s6zderi3
  end module s6zderi3_mod