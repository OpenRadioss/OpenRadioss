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
!||    hm_read_th_checksum_mod   ../starter/source/output/th/hm_read_thchecksum.F90
!||--- called by ------------------------------------------------------
!||    hm_read_thgrou            ../starter/source/output/th/hm_read_thgrou.F
!||====================================================================
      module hm_read_th_checksum_mod

      implicit none

      contains
!=======================================================================================================================
!!\brief /CHECKSUM/START option : prepares structutures to write Checksum fingerprints in /TH
!=======================================================================================================================
!||====================================================================
!||    hm_read_thchecksum            ../starter/source/output/th/hm_read_thchecksum.F90
!||--- called by ------------------------------------------------------
!||    hm_read_thgrou                ../starter/source/output/th/hm_read_thgrou.F
!||--- calls      -----------------------------------------------------
!||    deck_checksum_read            ../starter/source/output/checksum/checksum_model.cpp
!||    fretitl                       ../starter/source/starter/freform.F
!||    fretitl2                      ../starter/source/starter/freform.F
!||    r2r_exist                     ../starter/source/coupling/rad2rad/routines_r2r.F
!||--- uses       -----------------------------------------------------
!||    checksum_starter_option_mod   ../starter/source/output/checksum/checksum_option.F90
!||    file_descriptor_mod           ../starter/source/modules/file_descriptor_mod.F90
!||    format_mod                    ../starter/share/modules1/format_mod.F90
!||    hm_option_read_mod            ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                   ../starter/share/message_module/message_mod.F
!||    submodel_mod                  ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_thchecksum(                                                 &
        &                              ityp     ,key     ,iad    ,ifi    ,nsne,         &
        &                              ithgrp   ,ithbuf  ,nv     ,vare   ,lithbufmx,    &
        &                              checksum ,nsubdom ,nithgr ,ltitr  ,ipri  )
!
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Modules
          ! ----------------------------------------------------------------------------------------------------------------------
          use message_mod
          use submodel_mod
          use hm_option_read_mod
          use names_and_titles_mod , only : nchartitle
          use format_mod , only : fmw_i_a
          use checksum_output_option_mod
          use checksum_starter_option_mod
          use file_descriptor_mod
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Implicit none
          ! ----------------------------------------------------------------------------------------------------------------------
          implicit none
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Arguments
          ! ----------------------------------------------------------------------------------------------------------------------
          ! input variables
          integer, intent(in) :: nithgr
          integer, intent(in) :: ltitr
          integer, intent(in) :: nsubdom
          integer, intent(in) :: ipri
          integer, intent(in) :: ityp
          integer, intent(in) :: nv
          integer, intent(in) :: lithbufmx
          type(checksum_option_), intent(in) :: checksum
          character(len=10),intent(in) :: vare(nv)
          character(len=10),intent(in) :: key

          ! output variables
          integer, intent(inout) :: ifi
          integer, intent(inout) :: iad
          integer, intent(inout) :: nsne
          integer, intent(inout) :: ithgrp(nithgr)
          integer, intent(inout) :: ithbuf(lithbufmx)
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   local variables
          ! ----------------------------------------------------------------------------------------------------------------------
          integer :: j
          integer :: i
          integer :: id
          integer :: nne
          integer :: k
          integer :: iad0
          integer :: ifitmp
          integer :: nvar
          integer :: n
          integer :: iad1
          integer :: iad2
          integer :: idsmax
          integer :: ids
          integer :: ids_obj1
          integer :: hm_nthchecksum
          character(len=nchartitle)::titr
          logical :: is_available
          integer,target :: len_title
          integer,target :: len_checksum
          character(len=ncharline),target:: checksum_title
          character(len=64),target:: deck_checksum
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   external functions
          ! ----------------------------------------------------------------------------------------------------------------------
          integer,external :: r2r_exist
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Body
          ! ----------------------------------------------------------------------------------------------------------------------
          is_available = .false.
          nsne = 0
          ! id of the /th
          id = ithgrp(1)
          ! title of the /th
          call fretitl2(titr,ithgrp(nithgr-ltitr+1),ltitr)
          ! type of the /th
          ithgrp(2)=ityp
          ithgrp(3)=0
          ifitmp=ifi+1000
          !
          ! number of variables indicated by the user
          hm_nthchecksum = checksum%checksum_count
          nvar = 1
          ithbuf(iad) = 1
          !
          ! number of objects ids
          idsmax = hm_nthchecksum
          ids_obj1 = 1
          !
          ! filling tables
          ithgrp(6) = nvar
          ithgrp(7) = iad
          iad       = iad+nvar
          ifi       = ifi+nvar
          nne       = idsmax
          ithgrp(4) = nne
          ithgrp(5) = iad
          iad2      = iad+3*nne
          ithgrp(8) = iad2
          nne = 0
!
          ! loop over objects ids
          do k = 1,idsmax
            ids = 1
            if (nsubdom > 0) then
              if (r2r_exist(ityp,ids) == 0) cycle
            end if
            n = 1
            nne  = nne + 1
            nsne = nsne + 1
            ithbuf(iad) = n
            iad = iad+1
          end do
!
          ithgrp(4) = nne
          iad2      = ithgrp(5)+3*nne
          ithgrp(8) = iad2
          ifi       = ifi+3*nne+40*nne
          iad       = ithgrp(5)
!
          do i = 1,nne
            n = ithbuf(iad)

            checksum_title = " "
            deck_checksum = " "
            call deck_checksum_read(checksum%checksum_list,i,c_loc(checksum_title),&
            &c_loc(len_title),c_loc( deck_checksum),c_loc(len_checksum))
            titr = checksum_title(1:len_title)//"_"// deck_checksum(1:len_checksum)

            ithbuf(iad+2*nne) = i
            call fretitl(titr,ithbuf(iad2),ltitr)
            iad=iad+1
            iad2=iad2+40

          end do
!
          iad = iad2

          ! output in .out file
          if(ipri<1) return
          !
          n = ithgrp(4)
          iad1 = ithgrp(5)
          nvar=ithgrp(6)
          iad0=ithgrp(7)
          iad2=ithgrp(8)
          write(iout,"(//)")
          call fretitl2(titr,ithgrp(nithgr-ltitr+1),ltitr)
          write(iout,"(A,I10,3A,I3,A,I5,2A)")" TH GROUP:",ithgrp(1),",",trim(titr),",",nvar," VAR",n, key,":"
          write(iout,"(A)")" -------------------"
          write(iout,"(10A10)")(vare(ithbuf(j)),j=1,nvar)
          write(iout,"(3A)")"    ",key,"        NAME "
          do k=iad1,iad1+n-1
            call fretitl2(titr,ithbuf(iad2),40)
            iad2=iad2+40
            write(iout,"(I10,X,A)")k-iad1+1, titr(1:len_trim(titr))
          end do
          !
          return
        end subroutine hm_read_thchecksum
      end module hm_read_th_checksum_mod
