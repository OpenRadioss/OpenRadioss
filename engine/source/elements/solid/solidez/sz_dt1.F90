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
!||    sz_dt1_mod   ../engine/source/elements/solid/solidez/sz_dt1.F90
!||--- called by ------------------------------------------------------
!||    s8eforc3     ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    s8zforc3     ../engine/source/elements/solid/solide8z/s8zforc3.F
!||    szforc3      ../engine/source/elements/solid/solidez/szforc3.F
!||====================================================================
      module sz_dt1_mod
      implicit none
      contains
! ======================================================================================================================
! \brief new hexa hourglass formulation for distordtion control
! ======================================================================================================================
!||====================================================================
!||    sz_dt1          ../engine/source/elements/solid/solidez/sz_dt1.F90
!||--- called by ------------------------------------------------------
!||    s8eforc3        ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    s8zforc3        ../engine/source/elements/solid/solide8z/s8zforc3.F
!||    szforc3         ../engine/source/elements/solid/solidez/szforc3.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine  sz_dt1(                                     &
          px1,     px2,     px3,     px4,    &
          py1,     py2,     py3,     py4,    &
          pz1,     pz2,     pz3,     pz4,    &
          gfac,    deltax1, nel )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod,   only : zero,one,two,four,third,two_third
          use precision_mod,  only : WP
          use mvsiz_mod,      only : mvsiz
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                    :: nel              !< number of elements
          real(kind=WP), dimension(mvsiz) ,intent(in   )         ::      &
            px1,     px2,     px3,     px4,             &
            py1,     py2,     py3,     py4,             &
            pz1,     pz2,     pz3,     pz4                          !< B matrix components
          real(kind=WP), intent(in   )                            :: gfac             !< factor G/BULK=(1-2nu)/(1-nu)
          real(kind=WP), dimension(mvsiz)  ,intent(inout)         :: deltax1          !< characteristic length of dt1
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
!
          real(kind=WP)    :: pxx,pyy,pzz,pxy,pxz,pyz,aa,bb,p,d
! ----------------------------------------------------------------------------------------------------------------------
          if (gfac<one) then
            do i=1,nel
              pxx=two*(px1(i)*px1(i)+px2(i)*px2(i)+px3(i)*px3(i)+px4(i)*px4(i))
              pyy=two*(py1(i)*py1(i)+py2(i)*py2(i)+py3(i)*py3(i)+py4(i)*py4(i))
              pzz=two*(pz1(i)*pz1(i)+pz2(i)*pz2(i)+pz3(i)*pz3(i)+pz4(i)*pz4(i))
              pxy=two*(px1(i)*py1(i)+px2(i)*py2(i)+px3(i)*py3(i)+px4(i)*py4(i))
              pxz=two*(px1(i)*pz1(i)+px2(i)*pz2(i)+px3(i)*pz3(i)+px4(i)*pz4(i))
              pyz=two*(py1(i)*pz1(i)+py2(i)*pz2(i)+py3(i)*pz3(i)+py4(i)*pz4(i))
!
              aa = -(pxx+pyy+pzz)
              bb =  gfac*(pxx*pyy+pxx*pzz+pyy*pzz-pxy**2-pxz**2-pyz**2)
              p  = bb-third*aa*aa
              d  = four*sqrt(third*max(-p,zero))-two_third*aa  ! due mas_j=mas_e/8
!
              deltax1(i) =one/sqrt(d)
            end do
          else
            deltax1(1:nel) =  zero
          end if
!
        end subroutine sz_dt1
! ----------------------------------------------------------------------------------------------------------------------
      end module sz_dt1_mod
