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
!||    ifrbody_off_mod   ../starter/source/constraints/general/rbody/ifrbody_off.F90
!||--- called by ------------------------------------------------------
!||    checkrby          ../starter/source/constraints/general/rbody/checkrby.F
!||====================================================================
      module ifrbody_off_mod
      contains
! ======================================================================================================================
! \brief look at if RBODY are deactivated by /RBODY/OFF of Radioss engine input decks
! ======================================================================================================================
!||====================================================================
!||    ifrbody_off            ../starter/source/constraints/general/rbody/ifrbody_off.F90
!||--- called by ------------------------------------------------------
!||    checkrby               ../starter/source/constraints/general/rbody/checkrby.F
!||--- calls      -----------------------------------------------------
!||    read_engine_ilist      ../starter/source/constraints/general/rbody/ifrbody_off.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine ifrbody_off(nm,im_id,rootname,rootlen,nl_max,ifound)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
      use inoutfile_mod
      use names_and_titles_mod , only : ncharline
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                      :: nm              !< number of main nodes
          integer, dimension(nm), intent(in)                       :: im_id           !< main node list
          integer, intent(in)                                      :: nl_max          !< dimension max of /RBODY/OFF 
          integer, intent(in)                                      :: rootlen         !< dimension of rootname
          integer, intent(inout)                                   :: ifound          !< =1 if found, 0 else
          character(len=rootlen),               intent(in)         :: rootname        !< rootname for input file
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer i,j,n,io_err, len_tmp_name,iout,n_id,key_len,nfound
      integer, dimension(nl_max) :: ilist
      character(len=109)        :: filname
      character(len=1)          :: n2c
      character(len=4096)       :: tmp_name
      character(len=20)         :: keyword
! ======================================================================================================================
      ifound = 0
      iout = 71
      key_len = 10
      keyword(1:key_len) = '/RBODY/OFF'
      do n=1,9        ! limited to 9 engine restart files
        write(n2c,'(i1)') n
        filname=rootname(1:rootlen)//'_000'//n2c//'.rad'
        tmp_name=infile_name(1:infile_name_len)//filname(1:rootlen+9)
        len_tmp_name = infile_name_len+rootlen+9
        io_err = 0
        open(unit=iout,file=tmp_name(1:len_tmp_name),access='sequential',status='old',iostat=io_err)
        if (io_err == 0) then
           call read_engine_ilist(iout,keyword,key_len,nl_max,ilist,n_id)
           nfound = 0
           do i = 1,n_id
             do j = 1, nm
               if (im_id(j) == ilist(i)) nfound = nfound + 1
             end do 
           end do
           if (nfound>=nm-1) ifound=1  ! at least one of the two (if nm=2)
        else
           nfound = 0
        end if
        close(iout)
        if (ifound == 0) exit  ! should be found in any restart file
      end do ! n=1,9        
        
!      
       end subroutine ifrbody_off
! ======================================================================================================================
! \brief read Engine input file to get a list of id after the keyword 
! ======================================================================================================================
!||====================================================================
!||    read_engine_ilist      ../starter/source/constraints/general/rbody/ifrbody_off.F90
!||--- called by ------------------------------------------------------
!||    ifrbody_off            ../starter/source/constraints/general/rbody/ifrbody_off.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
      subroutine read_engine_ilist(iout,keyword,key_len,nlist_max,ilist,n_id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
      use names_and_titles_mod , only : ncharline
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                      :: iout            !< file unit number
          integer, intent(in)                                      :: key_len         !< dimension of keyword
          character(len=key_len),               intent(in)         :: keyword         !< keyword to search
          integer, intent(in)                                      :: nlist_max       !< dimension of ilist
          integer, intent(inout)                                   :: n_id            !< number of id read
          integer, dimension(nlist_max) ,intent(inout)             :: ilist           !< list of id read
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer i, j, k, n ,len_c,ic,ifw,id
      character(len=80)         :: keya
      character(len=ncharline)  :: line
! ----------------------------------------------------------------------------------------------------------------------
      n_id = 0
      ifw = 1
      do while (ifw==1) ! always read to end as the keyword could be defined several times
!
        read(iout,fmt='(a)',end=20) keya
        if (keya(1:key_len) == keyword ) then
           ic = 1
           do while(ic == 1)           
             read(iout,fmt='(a)',err=20,end=20) line
             if(line(1:1) == '#' .or. line(1:1) == '$') cycle
             ic = 0
             len_c = len_trim(line)
             if(line(len_c:len_c)==char(13)) len_c = len_c - 1
             if (line(1:1) == '/' .or. len_c==0) then
             else
               j=1
               do while(line(1:1) /= '/' .and. line(1:1) /= '#' .and.          &
                        line(1:1) /= '$' .and. len_c /= 0) 
                 do while (j <= len_c)
                  if(line(j:j) /= ' ') then
                    k=j
                    do while(line(k:k) /= ' ' .and. k < len_c)
                      k=k+1
                    enddo
                    if (n_id < nlist_max) then
                      n_id = n_id + 1
                      read(line(j:k),'(i10)') id
                      ilist(n_id) = id 
                    else ! avoid oob of ilist
                      read(line(j:k),'(i10)') id
                    end if
                    j = k
                  endif
                  j = j +1
                 end do
                 read(iout,fmt='(a)',err=20,end=20) line
                 len_c = len_trim(line)
                 if(line(len_c:len_c)==char(13)) len_c = len_c - 1
               end do
             end if
           end do !(ic == 1) remove comments
        end if !(keya(1:key_len) == keyword ) then
      end do !while (ifw==1)
!
 20     continue

      end subroutine read_engine_ilist
      end module ifrbody_off_mod
