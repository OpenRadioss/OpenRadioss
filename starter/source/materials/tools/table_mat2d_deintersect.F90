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
!||    table_mat2d_deintersect   ../starter/source/materials/mat/tools/table_mat2d_deintersect.F90
!||--- called by ------------------------------------------------------
!||====================================================================
      module table_mat2d_deintersect_mod
      contains

!! \brief  Check intersection between two curves in a 2D material table
!!         New values are calculated in case of intersection warning message is written 

!||====================================================================
!||    table_mat2d_deintersect   ../starter/source/materials/mat/tools/table_mat2d_deintersect.F90
!||--- called by ------------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
      subroutine table_mat2d_deintersect(table  ,mat_title ,mat_id )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------        
          use table4d_mod
          use constant_mod , only : zero, one, ten, em02  
          use precision_mod, only : WP
          use names_and_titles_mod
          use message_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer ,intent(in) :: mat_id
          character(len=nchartitle) ,intent(in) :: mat_title
          type(table_4d_) ,intent(inout) :: table
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer :: i,j,i1,i2,i3,j1,j2,j3
          integer :: ipt,npt,ifunc,nfunc
          real(kind=WP) :: yl1,yl2,yl3,yh1,yh2,yh3,dy1,dy2,dy3
          real(kind=WP) :: x1,x2,x3,xint,yint
          real(kind=WP) :: dm,alpha
!=========================================================================================      
        npt = size(table%x(1)%values)
!
        if (table%ndim == 2 .and. npt > 2) then
          nfunc   = size(table%x(2)%values)
          do ifunc = 2,nfunc
            j1   = ifunc - 1
            j2   = ifunc
            xint = zero
            yint = zero
            do ipt = 2,npt-1
              i1  = ipt-1
              i2  = ipt
              i3  = ipt+1
              yl1 = table%y2d(i1,j1)
              yh1 = table%y2d(i1,j2)
              yl2 = table%y2d(i2,j1)
              yh2 = table%y2d(i2,j2)
              yl3 = table%y2d(i3,j1)
              yh3 = table%y2d(i3,j2)
              dy1 = yh1 - yl1
              dy2 = yh2 - yl2
              dy3 = yh3 - yl3
              if (dy2 == zero .and. dy1*dy3 < zero) then
                xint = table%x(1)%values(i2)
                yint = yl2
              else if (dy1*dy2 < zero) then
                dm = (yh2 - yh1) - (yl2 - yl1)
                alpha = (yl1 - yh1) / dm
                alpha = min(one, max(zero, alpha))
                x1   = table%x(1)%values(i1)
                x2   = table%x(1)%values(i2)
                xint = x1  + alpha*(x2 - x1)
                yint = yl1 + alpha*(yl2 - yl1)
              else if (dy2*dy3 < zero) then
                dm = (yh3 - yh2) - (yl3 - yl2)
                alpha = (yl2 - yh2) / dm
                alpha = min(one, max(zero, alpha))
                x2   = table%x(1)%values(i2)
                x3   = table%x(1)%values(i3)
                xint = x2  + alpha*(x3 - x2)
                yint = yl2 + alpha*(yl3 - yl2)
              end if 
!
              if (xint > zero .and. yint > zero) then
                call ancmsg(MSGID=3010, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,  &
                     i1 = mat_id,                                                 &
                     i2 = table%notable,                                          &       
                     c1 = mat_title,                                              &
                     r1 = table%x(2)%values(j1),                                  &                   
                     r2 = table%x(2)%values(j2),                                  &                   
                     r3 = xint,                                                   &
                     r4 = yint)
                exit
              end if
            end do
          end do
        end if
!-----------------------------------------
        return        
        end subroutine table_mat2d_deintersect
      end module table_mat2d_deintersect_mod
