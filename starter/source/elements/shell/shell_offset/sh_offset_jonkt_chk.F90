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
!||    sh_offset_jonct_chk_mod   ../starter/source/elements/shell/shell_offset/sh_offset_jonkt_chk.F90
!||--- called by ------------------------------------------------------
!||    shell_offsetp             ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
!||====================================================================
      module sh_offset_jonct_chk_mod

        implicit none

      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine check the junction shell and tag to remove some in projection
!=======================================================================================================================
!||====================================================================
!||    sh_offset_jonct_chk   ../starter/source/elements/shell/shell_offset/sh_offset_jonkt_chk.F90
!||--- called by ------------------------------------------------------
!||    shell_offsetp         ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
!||--- calls      -----------------------------------------------------
!||    norma4n               ../starter/source/interfaces/inter3d1/norma1.F
!||    same_shellori         ../starter/source/elements/shell/shell_offset/same_shellori.F90
!||--- uses       -----------------------------------------------------
!||    same_shellori_mod     ../starter/source/elements/shell/shell_offset/same_shellori.F90
!||====================================================================
        subroutine sh_offset_jonct_chk(nshel    ,irect   ,xyz     ,numnod ,          &
          ichange   ,sh_offset ,thk_g )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,one,fourth
          use same_shellori_mod, only:same_shellori
          use precision_mod, only : WP
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,  intent(in   )                           :: nshel        !< number of segment
          integer,  intent(in   )                           :: numnod        !< number of node
          integer,  intent(in   ),dimension(4,nshel)        :: irect        !< connectivity of segment
          integer,  intent(inout),dimension(nshel)          :: ichange      !< if tagged
          real(kind=WP),  intent(in   ),dimension(3,numnod)       :: xyz          !< coordinate of node
          real(kind=WP),  intent(in   ),dimension(nshel)          :: sh_offset    !< offset value
          real(kind=WP),  intent(in   ),dimension(nshel)          :: thk_g        !< thickness
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: j,k,l,ll,i1,i2,i1m,i2m,nl,li,ie,iem,lj
          integer :: nextk(4),iwork(70000),ix(4),i1n,i2n,ien,nl_max,mode
          integer :: is_im,is_in,is_mn
          integer, dimension(:,:), allocatable :: lineix
          integer, dimension(:), allocatable   :: index,icmore
!
          real(kind=WP)  :: n_m(3),n_n(3),n_i(3),a_mn,a_mi,a_ni,a_max,a_min,area,l2_e,l2_m,ds(3)

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
              end if
            end do
          end do
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
            end do
          end do
!---------------------------------------
!       check max-junction number
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
!       check T-junction
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
                  if (is_mn/=1.or.sh_offset(iem)/=sh_offset(ien)) then
                    a_mn = -one
                  else
                    a_mn = n_m(1)*n_n(1)+n_m(2)*n_n(2)+n_m(3)*n_n(3)
                  end if
                  if (is_im/=1.or.sh_offset(iem)/=sh_offset(ie)) then
                    a_mi = -one
                  else
                    a_mi = n_m(1)*n_i(1)+n_m(2)*n_i(2)+n_m(3)*n_i(3)
                  end if
                  if (is_in/=1.or.sh_offset(ie)/=sh_offset(ien)) then
                    a_ni = -one
                  else
                    a_ni = n_i(1)*n_n(1)+n_i(2)*n_n(2)+n_i(3)*n_n(3)
                  end if
                  a_max= max(a_mn,a_mi,a_ni)
                  a_min= min(a_mn,a_mi,a_ni)
!   check the convexibility
                  if (a_mn==a_max) then
                    ichange(ie)  = -ichange(ie)
                  else if(a_mi==a_max) then
                    ichange(ien) = -ichange(ien)
                  else if(a_ni==a_max) then
                    ichange(iem) = -ichange(iem)
                  end if
                  nl = nl + 1
!!   junction more than 3, elem will be tagged
                  if (nl_max>3) then
                    do j=l+2,l+nl_max-2
                      li  = index(j)
                      i1n = lineix(1,li)
                      i2n = lineix(2,li)
                      ien = index(ll+li)
                      if (i1 == i1n .and. i2 == i2n .and. ichange(ien)>0) then
                        ichange(ien) = -ichange(ien)
                      end if
                    end do
                  end if
                end if !i1 == i1n .and. i2 == i2n
!
              end if !i1 == i1m .and. i2 == i2m
              i1m = i1
              i2m = i2
              iem = ie
              n_m(1:3) = n_i(1:3)
            end do
!---------------------------------------
!       remove one more element line for high thikness case
!---------------------------------------
            allocate(icmore(nshel))
            icmore = 0
            l = 1
            li  = index(l)
            i1m = lineix(1,li)
            i2m = lineix(2,li)
            iem = index(ll+li)
            ix(1:4) = irect(1:4,iem)
            ds(1:3) = xyz(1:3,ix(2))-xyz(1:3,ix(1))
            l2_m = ds(1)*ds(1)+ds(2)*ds(2)+ds(3)*ds(3)
!---------
            do while (l<ll) !l=2,ll
              l = l + 1
              li = index(l)
              i1 = lineix(1,li)
              i2 = lineix(2,li)
              ie = index(ll+li)
              ix(1:4) = irect(1:4,ie)
              ds(1:3) = xyz(1:3,ix(2))-xyz(1:3,ix(1))
              l2_e = ds(1)*ds(1)+ds(2)*ds(2)+ds(3)*ds(3)
              if (i1 == i1m .and. i2 == i2m .and.l<ll ) then ! double commun line
                li  = index(l+1)
                i1n = lineix(1,li)
                i2n = lineix(2,li)
                if (i1 == i1n .and. i2 == i2n ) then  ! triple commun line
                  l = l + 1
                  if (nl_max>3) then
                    lj = l
                    do j=lj+1,lj+nl_max-2
                      li  = index(j)
                      i1n = lineix(1,li)
                      i2n = lineix(2,li)
                      if (i1 == i1n .and. i2 == i2n ) l = l +1
                    end do
                  end if
                elseif (ichange(ie)*ichange(iem)<0) then ! commun edge between T-junction seg. and the neighbor
                  if (ichange(ie)>0.and.icmore(ie)==0) then ! the one possible to remove
                    if (thk_g(ie)*thk_g(ie)>fourth*l2_e) icmore(ie) = 1 ! if thickness >0.5*l
                  elseif (ichange(iem)>0.and.icmore(iem)==0) then
                    if (thk_g(iem)*thk_g(iem)>fourth*l2_m) icmore(iem) = 1
                  end if
                end if
              end if !i1 == i1m .and. i2 == i2m
              i1m = i1
              i2m = i2
              iem = ie
              l2_m = l2_e
            end do !while (l<ll)
            do j=1,nshel
              if (icmore(j)==1.and.ichange(j)>0) ichange(j) = -ichange(j)
            end do
            deallocate(icmore)
          end if !(nl_max>=3) then
          deallocate(index)
          deallocate(lineix)
!
        end subroutine sh_offset_jonct_chk
      end module sh_offset_jonct_chk_mod
