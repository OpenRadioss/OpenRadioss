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
!! \brief Internal force computation for 6-node solid elements
!! \details Computes internal forces for solid elements using stress and strain data
!!          Includes multi-layer averaging and various material properties

!||====================================================================
!||    s6zfint3_mod   ../engine/source/elements/solid/solide6z/s6zfint3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3       ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zfint3_mod
      contains
      ! ======================================================================================================================
      ! \brief Internal force computation for 6-node solid elements
      ! \details Computes internal forces for solid elements using stress and strain data
      !          Includes multi-layer averaging and various material properties
      ! ======================================================================================================================
!||====================================================================
!||    s6zfint3        ../engine/source/elements/solid/solide6z/s6zfint3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine s6zfint3(                                                     &
        sig      ,px1      ,px2      ,px3      ,px4      ,px5      ,           &
        px6      ,py1      ,py2      ,py3      ,py4      ,py5      ,           &
        py6      ,pz1      ,pz2      ,pz3      ,pz4      ,pz5      ,           &
        pz6      ,f11      ,f21      ,f31      ,f12      ,f22      ,           &
        f32      ,f13      ,f23      ,f33      ,f14      ,f24      ,           &
        f34      ,f15      ,f25      ,f35      ,f16      ,f26      ,           &
        f36      ,vol      ,qvis     ,eint     ,rho      ,q        ,           &
        epla     ,epsd     ,epsdm    ,sigm     ,eintm    ,rhom     ,           &
        qm       ,eplasm   ,volg     ,off      ,vol0     ,vol0g    ,           &
        g_pla    ,g_epsd   ,nel      ,svis     ,nlay     )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
      use precision_mod, only : wp
      use mvsiz_mod, only : mvsiz
      use constant_mod, only : one, half, three
!-------------------------------------------------------------------------------
!    I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------------------
!    D u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
      integer,                           intent(in)    :: g_pla           !< Plastic flag
      integer,                           intent(in)    :: g_epsd          !< Strain rate flag
      integer,                           intent(in)    :: nel             !< Number of elements
      integer,                           intent(in)    :: nlay            !< Number of layers
      real(kind=wp), dimension(nel,6),   intent(in)    :: sig             !< Stress tensor
      real(kind=wp), dimension(nel),     intent(in)    :: px1, px2, px3, px4, px5, px6  !< X coordinates
      real(kind=wp), dimension(nel),     intent(in)    :: py1, py2, py3, py4, py5, py6  !< Y coordinates
      real(kind=wp), dimension(nel),     intent(in)    :: pz1, pz2, pz3, pz4, pz5, pz6  !< Z coordinates
      real(kind=wp), dimension(mvsiz),   intent(in)    :: vol              !< Element volume
      real(kind=wp), dimension(nel),     intent(in)    :: qvis             !< Viscous pressure
      real(kind=wp), dimension(nel),     intent(in)    :: eint             !< Internal energy
      real(kind=wp), dimension(nel),     intent(in)    :: rho              !< Density
      real(kind=wp), dimension(nel),     intent(in)    :: q                !< Artificial viscosity
      real(kind=wp), dimension(nel),     intent(in)    :: epla             !< Plastic strain
      real(kind=wp), dimension(nel),     intent(in)    :: epsd             !< Strain rate
      real(kind=wp), dimension(nel),     intent(in)    :: volg             !< Global volume
      real(kind=wp), dimension(nel),     intent(in)    :: off              !< Element off flag            !< Poisson's ratio
      real(kind=wp), dimension(nel),     intent(in)    :: vol0             !< Initial volume
      real(kind=wp), dimension(nel),     intent(in)    :: vol0g            !< Initial global volume
      real(kind=wp), dimension(mvsiz,6), intent(inout) :: svis             !< Viscous stress
      real(kind=wp), dimension(nel),     intent(inout) :: f11, f21, f31    !< Forces on node 1
      real(kind=wp), dimension(nel),     intent(inout) :: f12, f22, f32    !< Forces on node 2
      real(kind=wp), dimension(nel),     intent(inout) :: f13, f23, f33    !< Forces on node 3
      real(kind=wp), dimension(nel),     intent(inout) :: f14, f24, f34    !< Forces on node 4
      real(kind=wp), dimension(nel),     intent(inout) :: f15, f25, f35    !< Forces on node 5
      real(kind=wp), dimension(nel),     intent(inout) :: f16, f26, f36    !< Forces on node 6
      real(kind=wp), dimension(nel),     intent(inout) :: eplasm           !< Mean plastic strain
      real(kind=wp), dimension(nel,6),   intent(inout) :: sigm             !< Mean stress
      real(kind=wp), dimension(nel),     intent(inout) :: eintm            !< Mean internal energy
      real(kind=wp), dimension(nel),     intent(inout) :: rhom             !< Mean density
      real(kind=wp), dimension(nel),     intent(inout) :: qm               !< Mean artificial viscosity
      real(kind=wp), dimension(nel),     intent(inout) :: epsdm            !< Mean strain rate
!
!-------------------------------------------------------------------------------
!    L o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
      integer :: i                                                         !< Loop counter
      real(kind=wp), dimension(nel) :: s1, s2, s3, s4, s5, s6              !< Stress components
      real(kind=wp), dimension(nel) :: fac                                 !< Volume factor
 
 
 
!
!===============================================================================
!     S o u r c e  l i n e s
!===============================================================================
!
      !<  
      do i=1,nel
        s1(i)=(sig(i,1)+svis(i,1)-qvis(i))*vol(i)
        s2(i)=(sig(i,2)+svis(i,2)-qvis(i))*vol(i)
        s3(i)=(sig(i,3)+svis(i,3)-qvis(i))*vol(i)
        s4(i)=(sig(i,4)+svis(i,4))*vol(i)
        s5(i)=(sig(i,5)+svis(i,5))*vol(i)
        s6(i)=(sig(i,6)+svis(i,6))*vol(i)
      end do

      !< Internal forces computation
      do i = 1, nel
        f11(i) = f11(i) - (s1(i) * px1(i) + s4(i) * py1(i) + s6(i) * pz1(i))
        f21(i) = f21(i) - (s2(i) * py1(i) + s5(i) * pz1(i) + s4(i) * px1(i))
        f31(i) = f31(i) - (s3(i) * pz1(i) + s6(i) * px1(i) + s5(i) * py1(i))

        f12(i) = f12(i) - (s1(i) * px2(i) + s4(i) * py2(i) + s6(i) * pz2(i))
        f22(i) = f22(i) - (s2(i) * py2(i) + s5(i) * pz2(i) + s4(i) * px2(i))
        f32(i) = f32(i) - (s3(i) * pz2(i) + s6(i) * px2(i) + s5(i) * py2(i))

        f13(i) = f13(i) - (s1(i) * px3(i) + s4(i) * py3(i) + s6(i) * pz3(i))
        f23(i) = f23(i) - (s2(i) * py3(i) + s5(i) * pz3(i) + s4(i) * px3(i))
        f33(i) = f33(i) - (s3(i) * pz3(i) + s6(i) * px3(i) + s5(i) * py3(i))

        f14(i) = f14(i) - (s1(i) * px4(i) + s4(i) * py4(i) + s6(i) * pz4(i))
        f24(i) = f24(i) - (s2(i) * py4(i) + s5(i) * pz4(i) + s4(i) * px4(i))
        f34(i) = f34(i) - (s3(i) * pz4(i) + s6(i) * px4(i) + s5(i) * py4(i))

        f15(i) = f15(i) - (s1(i) * px5(i) + s4(i) * py5(i) + s6(i) * pz5(i))
        f25(i) = f25(i) - (s2(i) * py5(i) + s5(i) * pz5(i) + s4(i) * px5(i))
        f35(i) = f35(i) - (s3(i) * pz5(i) + s6(i) * px5(i) + s5(i) * py5(i))

        f16(i) = f16(i) - (s1(i) * px6(i) + s4(i) * py6(i) + s6(i) * pz6(i))
        f26(i) = f26(i) - (s2(i) * py6(i) + s5(i) * pz6(i) + s4(i) * px6(i))
        f36(i) = f36(i) - (s3(i) * pz6(i) + s6(i) * px6(i) + s5(i) * py6(i))
      end do

      ! Post-processing: mean value in the sense a' = (integral a dv) / v
      if (nlay > 1) then
        do i = 1, nel
          fac(i)     = off(i) * vol(i) / volg(i)
          sigm(i,1) = sigm(i,1) + fac(i) * sig(i,1)
          sigm(i,2) = sigm(i,2) + fac(i) * sig(i,2)
          sigm(i,3) = sigm(i,3) + fac(i) * sig(i,3)
          sigm(i,4) = sigm(i,4) + fac(i) * sig(i,4)
          sigm(i,5) = sigm(i,5) + fac(i) * sig(i,5)
          sigm(i,6) = sigm(i,6) + fac(i) * sig(i,6)
          rhom(i)   = rhom(i)   + fac(i) * rho(i)
          eintm(i)  = eintm(i)  + eint(i) * vol0(i) / vol0g(i)
          qm(i)     = qm(i)     + fac(i) * q(i)
        end do
        if (g_pla > 0) then
          do i = 1, nel
         eplasm(i) = eplasm(i) + fac(i) * epla(i)
          end do
        end if
        if (g_epsd > 0) then
          do i = 1, nel
         epsdm(i) = epsdm(i) + fac(i) * epsd(i)
          end do
        end if
      end if
      end subroutine s6zfint3
      end module s6zfint3_mod