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
!||    s6zupd11t12_mod   ../engine/source/elements/solid/solide6z/s6zupd11t12.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3          ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zupd11t12_mod
      contains
!||====================================================================
!||    s6zupd11t12     ../engine/source/elements/solid/solide6z/s6zupd11t12.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- calls      -----------------------------------------------------
!||    s6zjac_i        ../engine/source/elements/solid/solide6z/s6zjac_i.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||    s6zjac_i_mod    ../engine/source/elements/solid/solide6z/s6zjac_i.F90
!||====================================================================
        subroutine s6zupd11t12( &
          offg     ,offg0    ,x        ,xdp      ,      &
          nc1      ,nc2      ,nc3      ,nc4      ,      &
          nc5      ,nc6      ,jac_1    ,sig      ,      &
          sigl     ,nel      ,numnod   ,iresp )

!-------------------------------------------------------------------------------
!   m o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod, only : wp
          use constant_mod,  only : one
          use mvsiz_mod,     only : mvsiz
          use s6zjac_i_mod,  only : s6zjac_i

!-------------------------------------------------------------------------------
!   i m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none

!-------------------------------------------------------------------------------
!   d u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
          real(kind=wp), dimension(nel),      intent(in)    :: offg      !< global element activation flag
          real(kind=wp), dimension(nel),      intent(in)    :: offg0     !< previous global element activation flag
          integer,                            intent(in)    :: numnod 
          real(kind=wp), dimension(3,numnod), intent(in)    :: x         !< nodal coordinates (working precision)
          real(kind=8),  dimension(3,numnod), intent(in)    :: xdp       !< nodal coordinates (double precision)

          integer,       dimension(nel),      intent(in)    :: nc1       !< node connectivity index 1
          integer,       dimension(nel),      intent(in)    :: nc2       !< node connectivity index 2
          integer,       dimension(nel),      intent(in)    :: nc3       !< node connectivity index 3
          integer,       dimension(nel),      intent(in)    :: nc4       !< node connectivity index 4
          integer,       dimension(nel),      intent(in)    :: nc5       !< node connectivity index 5
          integer,       dimension(nel),      intent(in)    :: nc6       !< node connectivity index 6

          real(kind=wp), dimension(10,nel), intent(inout) :: jac_1     !< jacobian inverse terms
          real(kind=wp), dimension(nel,6),  intent(in)    :: sig       !< stress tensor
          real(kind=wp), dimension(nel,6),  intent(inout) :: sigl      !< local stress tensor

          integer,                          intent(in)    :: nel   
          integer,                          intent(in)    :: iresp     !< number of elements

!-------------------------------------------------------------------------------
!   l o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i, j, nch
          integer, dimension(nel) :: index
          real(kind=wp), dimension(10,nel) :: jac_i
          ! local double precision arrays for vectorization
          real(kind=8), dimension(nel) :: xd1, xd2, xd3, xd4, xd5, xd6
          real(kind=8), dimension(nel) :: yd1, yd2, yd3, yd4, yd5, yd6
          real(kind=8), dimension(nel) :: zd1, zd2, zd3, zd4, zd5, zd6

!===============================================================================
!   b o d y
!===============================================================================

          nch = 0
          do i = 1, nel
             if (offg(i) /= offg0(i) .and. abs(offg(i)) > one) then
               nch = nch + 1
               index(nch) = i
             end if
          end do
          
          if (nch == 0) return


          if (iresp == 1) then 
            !#include "vectorize.inc"
            do j = 1, nch
              i = index(j)
              xd1(j) = xdp(1,nc1(i))  
              yd1(j) = xdp(2,nc1(i))  
              zd1(j) = xdp(3,nc1(i))  
              
              xd2(j) = xdp(1,nc2(i))  
              yd2(j) = xdp(2,nc2(i))  
              zd2(j) = xdp(3,nc2(i))  
              
              xd3(j) = xdp(1,nc3(i))  
              yd3(j) = xdp(2,nc3(i))  
              zd3(j) = xdp(3,nc3(i))  
              
              xd4(j) = xdp(1,nc4(i))  
              yd4(j) = xdp(2,nc4(i))  
              zd4(j) = xdp(3,nc4(i))  
              
              xd5(j) = xdp(1,nc5(i))  
              yd5(j) = xdp(2,nc5(i))  
              zd5(j) = xdp(3,nc5(i))  
              
              xd6(j) = xdp(1,nc6(i))  
              yd6(j) = xdp(2,nc6(i))  
              zd6(j) = xdp(3,nc6(i))            
            end do
          else
            !#include "vectorize.inc"
            do j = 1, nch
              i = index(j)
              xd1(j) = x(1,nc1(i))  
              yd1(j) = x(2,nc1(i))  
              zd1(j) = x(3,nc1(i))  
              
              xd2(j) = x(1,nc2(i))  
              yd2(j) = x(2,nc2(i))  
              zd2(j) = x(3,nc2(i))  
              
              xd3(j) = x(1,nc3(i))  
              yd3(j) = x(2,nc3(i))  
              zd3(j) = x(3,nc3(i))  
              
              xd4(j) = x(1,nc4(i))  
              yd4(j) = x(2,nc4(i))  
              zd4(j) = x(3,nc4(i))  
              
              xd5(j) = x(1,nc5(i))  
              yd5(j) = x(2,nc5(i))  
              zd5(j) = x(3,nc5(i))  
              
              xd6(j) = x(1,nc6(i))  
              yd6(j) = x(2,nc6(i))  
              zd6(j) = x(3,nc6(i))  
            end do      
          end if

          call s6zjac_i( &
            xd1  ,xd2  ,xd3  ,xd4  ,xd5  ,xd6 , &
            yd1  ,yd2  ,yd3  ,yd4  ,yd5  ,yd6 , &
            zd1  ,zd2  ,zd3  ,zd4  ,zd5  ,zd6 , &
            jac_i,nch)

          !#include "vectorize.inc"
          do j = 1, nch
            i = index(j)
            jac_1(1:10,i) = jac_i(1:10,j)
            sigl(i,1:6)   = sig(i,1:6)
          end do      

        end subroutine s6zupd11t12

      end module s6zupd11t12_mod