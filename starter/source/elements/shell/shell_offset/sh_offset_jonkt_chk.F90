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
      module sh_offset_jonct_chk_mod

        contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine check the joinction shell and tag to remove some in prjection
!=======================================================================================================================
      subroutine sh_offset_jonct_chk(nshel    ,irect   ,xyz     ,numnod ,          &
                                     ichange  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,one
          use same_shellori_mod, only:same_shellori
!
      implicit none
!
#include "my_real.inc"       
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,  intent(in   )                           :: nshel        !< number of segment
      integer,  intent(in   )                           :: numnod        !< number of node 
      integer,  intent(in   ),dimension(4,nshel)        :: irect        !< connectivity of segment 
      integer,  intent(inout),dimension(nshel)          :: ichange      !< if tagged   
      my_real,  intent(in   ),dimension(3,numnod)       :: xyz          !< coordinate of node
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer i,j,k,l,stat,iep,ll,i1,i2,i1m,i2m,nl,is,li,ie,ied,iem
      integer nextk(4),iwork(70000),ix(4),i1n,i2n,ien,n0,nn,nl_max,mode
      integer isame(3),is_im,is_in,is_mn
      integer, dimension(:,:), allocatable :: lineix
      integer, dimension(:), allocatable   :: index
!
      my_real  n_m(3),n_n(3),n_i(3),a_mn,a_mi,a_ni,a_max,a_min,area

      data nextk/2,3,4,1/
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------  
        allocate(index(8*nshel))
        allocate(lineix(2,4*nshel))
        ll = 0
        do j=1,nshel
          do k=1,4
            i1=irect(k,j)
            i2=irect(nextk(k),j)
            if (i1==i2) cycle
            ll = ll+1
            if(i2 > i1)then
              lineix(1,ll) = i1
              lineix(2,ll) = i2
            else
              lineix(1,ll) = i2
              lineix(2,ll) = i1
            endif
          enddo
        enddo
!
        mode = 0
        CALL MY_ORDERS(mode,iwork,lineix,index,ll,2)
!   now index(ll+1:nshel) used for elem_id       
        nl = 0
        do j=1,nshel
          do k=1,4
            i1=irect(k,j)
            i2=irect(nextk(k),j)
            if (i1==i2) cycle
            nl = nl+1
            index(ll+nl)=j
          enddo
        enddo
!---------------------------------------
!       check max-jonction number
!---------------------------------------
        nl_max = 0
        l = 1
        li  = index(l)
        i1m = lineix(1,li)
        i2m = lineix(2,li)
        iem = index(ll+li)
!---------
        nl = 1
        do l=2,ll
          li = index(l)
          i1 = lineix(1,li)   
          i2 = lineix(2,li)
          ie = index(ll+li)
          if (i1 == i1m .and. i2 == i2m ) then 
            nl = nl+1
          else 
            nl = 1
          end if
          nl_max = max(nl_max,nl)
          i1m = i1
          i2m = i2
          iem = ie
        end do
        if (nl_max>=3) then
!---------------------------------------
!       check T-jonction
!---------------------------------------
          nl = 0
          l = 1
          li  = index(l)
          i1m = lineix(1,li)
          i2m = lineix(2,li)
          iem = index(ll+li)
          ix(1:4) = irect(1:4,iem)
          call norma4n(n_m(1),n_m(2),n_m(3),area,ix ,xyz )
!---------
          do l=2,ll
            li = index(l)
            i1 = lineix(1,li)   
            i2 = lineix(2,li)
            ie = index(ll+li)
            ix(1:4) = irect(1:4,ie)
            call norma4n(n_i(1),n_i(2),n_i(3),area,ix ,xyz )
            if (i1 == i1m .and. i2 == i2m  .and.l<ll                             &
                .and.ichange(iem)>0.and.ichange(ie)>0) then ! double commun line
              li  = index(l+1)
              i1n = lineix(1,li)
              i2n = lineix(2,li)
              ien = index(ll+li)
              if (i1 == i1n .and. i2 == i2n .and. ichange(ien)>0) then ! triple commun line
! criteria to choose the leaving one (m i n)
! 1: already taged before 
! 2: the two others have the same orientation            
! 3: a_max is the others          
                call same_shellori(i1,i2,irect(1,ie),irect(1,iem),is_im)
                call same_shellori(i1,i2,irect(1,ie),irect(1,ien),is_in)
                call same_shellori(i1,i2,irect(1,iem),irect(1,ien),is_mn)
                ix(1:4) = irect(1:4,ien)
                call norma4n(n_n(1),n_n(2),n_n(3),area,ix ,xyz )
                if (is_mn/=1) then 
                  a_mn = -one
                else
                  a_mn = n_m(1)*n_n(1)+n_m(2)*n_n(2)+n_m(3)*n_n(3)
                end if
                if (is_im/=1) then 
                  a_mi = -one
                else
                  a_mi = n_m(1)*n_i(1)+n_m(2)*n_i(2)+n_m(3)*n_i(3)
                end if
                if (is_in/=1) then 
                  a_ni = -one
                else
                  a_ni = n_i(1)*n_n(1)+n_i(2)*n_n(2)+n_i(3)*n_n(3)
                end if
                a_max= max(a_mn,a_mi,a_ni)
                a_min= min(a_mn,a_mi,a_ni)
!   check the convexibility
                  if (a_mn==a_max) then  
                      ichange(ie)  = -ichange(ie) 
                  elseif(a_mi==a_max) then 
                      ichange(ien) = -ichange(ien) 
                  elseif(a_ni==a_max) then 
                      ichange(iem) = -ichange(iem)
                  end if
                  nl = nl + 1
!!   jonction more than 3, elem will be tagged                
                if (nl_max>3) then 
                  do j=l+2,l+nl_max-2
                    li  = index(l)
                    i1n = lineix(1,li)
                    i2n = lineix(2,li)
                    ien = index(ll+li)
                    if (i1 == i1n .and. i2 == i2n .and. ichange(ien)>0) then
                      ichange(ien) = -ichange(ien) 
                    end if 
                  end do
                end if
              endif !i1 == i1n .and. i2 == i2n
!         
            endif !i1 == i1m .and. i2 == i2m
            i1m = i1
            i2m = i2
            iem = ie
            n_m(1:3) = n_i(1:3)
          enddo
!          print *,'triple lines nl = ',nl
        end if !(nl_max>=3) then
        deallocate(index)
        deallocate(lineix)
!        
        end subroutine sh_offset_jonct_chk
    end module sh_offset_jonct_chk_mod