!copyright>        openradioss
!copyright>        copyright (c) 1986-2025 altair engineering inc.
!copyright>
!copyright>        this program is free software: you can redistribute it and/or modify
!copyright>        it under the terms of the gnu affero general public license as published by
!copyright>        the free software foundation, either version 3 of the license, or
!copyright>        (at your option) any later version.
!copyright>
!copyright>        this program is distributed in the hope that it will be useful,
!copyright>        but without any warranty; without even the implied warranty of
!copyright>        merchantability or fitness for a particular purpose.  see the
!copyright>        gnu affero general public license for more details.
!copyright>
!copyright>        you should have received a copy of the gnu affero general public license
!copyright>        along with this program.  if not, see <https://www.gnu.org/licenses/>.
!copyright>
!copyright>
!copyright>        commercial alternative: altair radioss software
!copyright>
!copyright>        as an alternative to this open-source version, altair also offers altair radioss
!copyright>        software under a commercial license.  contact altair to discuss further if the
!copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    s6zderi3_mod   ../engine/source/elements/solid/solide6z/s6zderi3.f90
!||--- called by ------------------------------------------------------
!||    s6zforc3       ../engine/source/elements/solid/solide6z/s6zforc3.f90
!||====================================================================
      module s6zderito3_mod
      contains
!||====================================================================
!||    s6zderi3        ../engine/source/elements/solid/solide6z/s6zderi3.f90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.f90
!||--- calls      -----------------------------------------------------
!||    ancmsg          ../engine/source/output/message/message.f
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.f
!||    message_mod     ../engine/share/message_module/message_mod.f
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.f90
!||    precision_mod   ../common_source/modules/precision_mod.f90
!||====================================================================
      subroutine s6zderito3(                                                   &
        det      ,                                         &
        px1      ,px2      ,px3      ,px4      ,px5      ,px6      ,           &
        py1      ,py2      ,py3      ,py4      ,py5      ,py6      ,           &
        pz1      ,pz2      ,pz3      ,pz4      ,pz5      ,pz6      ,           &
        nel      ,jac_i )  
!
!-------------------------------------------------------------------------------
!   m o d u l e s
!-------------------------------------------------------------------------------
      use message_mod
      use mvsiz_mod, only : mvsiz
      use precision_mod, only : wp
      use constant_mod, only : zero, one, two, third, fourth, one_over_8, one_over_12
!-------------------------------------------------------------------------------
!    i m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
      implicit none
#include      "units_c.inc"
!-------------------------------------------------------------------------------
!    d u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
      integer,                       intent(in)    :: nel         !< number of elements
      real(kind=wp), dimension(nel), intent(out)   :: det         !< jacobian determinant
      real(kind=wp), dimension(nel), intent(out)   :: px1         !< shape function derivative dn1/dxi
      real(kind=wp), dimension(nel), intent(out)   :: px2         !< shape function derivative dn2/dxi
      real(kind=wp), dimension(nel), intent(out)   :: px3         !< shape function derivative dn3/dxi
      real(kind=wp), dimension(nel), intent(out)   :: px4         !< shape function derivative dn4/dxi
      real(kind=wp), dimension(nel), intent(out)   :: px5         !< shape function derivative dn5/dxi
      real(kind=wp), dimension(nel), intent(out)   :: px6         !< shape function derivative dn6/dxi
      real(kind=wp), dimension(nel), intent(out)   :: py1         !< shape function derivative dn1/deta
      real(kind=wp), dimension(nel), intent(out)   :: py2         !< shape function derivative dn2/deta
      real(kind=wp), dimension(nel), intent(out)   :: py3         !< shape function derivative dn3/deta
      real(kind=wp), dimension(nel), intent(out)   :: py4         !< shape function derivative dn4/deta
      real(kind=wp), dimension(nel), intent(out)   :: py5         !< shape function derivative dn5/deta
      real(kind=wp), dimension(nel), intent(out)   :: py6         !< shape function derivative dn6/deta
      real(kind=wp), dimension(nel), intent(out)   :: pz1         !< shape function derivative dn1/dzeta
      real(kind=wp), dimension(nel), intent(out)   :: pz2         !< shape function derivative dn2/dzeta
      real(kind=wp), dimension(nel), intent(out)   :: pz3         !< shape function derivative dn3/dzeta
      real(kind=wp), dimension(nel), intent(out)   :: pz4         !< shape function derivative dn4/dzeta
      real(kind=wp), dimension(nel), intent(out)   :: pz5         !< shape function derivative dn5/dzeta
      real(kind=wp), dimension(nel), intent(out)   :: pz6         !< shape function derivative dn6/dzeta
      real(kind=wp), dimension(10,nel), intent(out) :: jac_i     !< jacobian matrix component j12
  
!-------------------------------------------------------------------------------
!    l o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
      integer :: i, j, icor, nnega                            !< loop counters and flags
      integer :: index(nel)                                   !< index array for negative volume elements  
      real(kind=wp) :: jac1(nel), jac2(nel), jac3(nel)        !< jacobian matrix components
      real(kind=wp) :: jac4(nel), jac5(nel), jac6(nel)        !< jacobian matrix components
      real(kind=wp) :: jac7(nel), jac8(nel), jac9(nel)        !< jacobian matrix components    
      real(kind=wp) :: jaci1, jaci2, jaci3                    !< jacobian inverse components
      real(kind=wp) :: jaci4, jaci5, jaci6                    !< jacobian inverse components
      real(kind=wp) :: jaci7, jaci8, jaci9                    !< jacobian inverse components
      real(kind=wp) :: jaci12, jaci45, jaci78                 !< combined inverse components 
      real(kind=wp) :: x21(nel), x31(nel), x54(nel), x64(nel) !< coordinate differences
      real(kind=wp) :: y21(nel), y31(nel), y54(nel), y64(nel) !< coordinate differences
      real(kind=wp) :: z21(nel), z31(nel), z54(nel), z64(nel) !< coordinate differences
      real(kind=wp) :: x41(nel), y41(nel), z41(nel)           !< coordinate differences
      real(kind=wp) :: jac_59_68(nel), jac_67_49(nel), jac_48_57(nel) !< cross products
!-------------------------------------------------------------------------------
!

!
      !< jacobian matrix components
      do i=1,nel
      jaci1 = jac_i(1,i)
      jaci4 = jac_i(4,i)
      jaci7 = jac_i(7,i)
      jaci2 = jac_i(2,i)
      jaci5 = jac_i(5,i)
      jaci8 = jac_i(8,i)
      jaci3 = jac_i(3,i)
      jaci6 = jac_i(6,i)
      jaci9 = jac_i(9,i)
      det(i) = jac_i(10,i)

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
    
    end subroutine s6zderito3
  end module s6zderito3_mod