!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      module same_shellori_mod

      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief if the same orientation of two shell connected by i1,i2 (i2>i1)
!=======================================================================================================================
        subroutine same_shellori(i1,i2,ixm,ixn,isame)
! ------------------------------------------------------------------------------
!
! ------------------------------------------------------------------------------
!
          implicit none
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,  intent(in   )               :: i1 , i2        !< commun seg i1,i2
          integer,  intent(in   ),dimension(4)  :: ixm, ixn       !< shell connectivity
          integer,  intent(inout)               :: isame          !< same orientation flag
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer i,k,nextk(4),im1,im2,in1,in2,ivm,ivn
          data nextk/2,3,4,1/
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ivm = 0
          ivn = 0
          do i = 1,4
            im1=ixm(i)
            im2=ixm(nextk(i))
            if(min(im1,im2)==i1.and.max(im1,im2)==i2) then
              if(im2 > im1)then
                ivm = 1
              else
                ivm = -1
              endif
            endif
          end do
          do i=1,4
            im1=ixn(i)
            im2=ixn(nextk(i))
            if(min(im1,im2)==i1.and.max(im1,im2)==i2) then
              if(im2 > im1)then
                ivn = 1
              else
                ivn = -1
              endif
            endif
          end do
          isame = -ivm*ivn
!
        end subroutine same_shellori
      end module same_shellori_mod

