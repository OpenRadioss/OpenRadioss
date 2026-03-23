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
!||    checksum_starter_option_mod   ../starter/source/output/checksum/checksum_option.F90
!||--- called by ------------------------------------------------------
!||    ddsplit                       ../starter/source/restart/ddsplit/ddsplit.F
!||    f_anend                       ../starter/source/output/analyse/analyse_arret.F
!||    hm_read_prethgrou             ../starter/source/output/th/hm_read_prethgrou.F
!||    hm_read_thchecksum            ../starter/source/output/th/hm_read_thchecksum.F90
!||    hm_read_thgrou                ../starter/source/output/th/hm_read_thgrou.F
!||    starter0                      ../starter/source/starter/starter0.F
!||====================================================================
      module checksum_starter_option_mod
        implicit none

        interface
          function deck_checksum_creation( len_filename, filename) bind (C, name="deck_checksum_creation")
            use, intrinsic :: iso_c_binding
            integer(C_INT), value :: len_filename
            type(C_PTR),value  :: filename
            type(C_PTR) :: deck_checksum_creation
          end function deck_checksum_creation

          function deck_checksum_count(checksum_list) bind (C, name="deck_checksum_count")
            use, intrinsic :: iso_c_binding
            type(C_PTR),value :: checksum_list
            integer(C_INT) :: deck_checksum_count
          end function deck_checksum_count

          subroutine deck_checksum_read(checksum_list, i,         &
          &                                checksum_title,len_title, &
          &                                checksum,len_checksum)    &
          &                                bind (C, name="deck_checksum_read")
            use, intrinsic :: iso_c_binding
            type(C_PTR),value :: checksum_list
            integer(C_INT),value :: i
            type(C_PTR),value :: checksum_title
            type(C_PTR),value :: len_title
            type(C_PTR),value :: checksum
            type(C_PTR),value :: len_checksum
          end subroutine deck_checksum_read

        end interface

      contains

!! \brief Compute MD5 checksum of input file sections.
!! \details Section starts with /CHECKSUM/START and ends with /CHECKSUM/END.
!! \details The result is a list of MD5 checksums for each sections.
!! \details print the list in Starter output file.
!||====================================================================
!||    hm_read_checksum       ../starter/source/output/checksum/checksum_option.F90
!||--- called by ------------------------------------------------------
!||    starter0               ../starter/source/starter/starter0.F
!||--- calls      -----------------------------------------------------
!||    deck_checksum_read     ../starter/source/output/checksum/checksum_model.cpp
!||    hm_option_count        ../starter/source/devtools/hm_reader/hm_option_count.F
!||--- uses       -----------------------------------------------------
!||    file_descriptor_mod    ../starter/source/modules/file_descriptor_mod.F90
!||====================================================================
        subroutine hm_read_checksum(leni,input,lenp,path,output)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use file_descriptor_mod
          use names_and_titles_mod
          use output_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: leni
          integer, intent(in) :: lenp
          character(len=leni), intent(in) :: input
          character(len=lenp), intent(in) :: path
          type(output_), intent(inout) :: output
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer :: checksum_option_count
          integer :: checksum_digest_count

          integer,target :: len_title
          integer,target :: len_checksum
          character(len=ncharline),target:: checksum_title
          character(len=64),target:: checksum
          character(kind=C_CHAR,len=leni+lenp+1),target :: filename
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call hm_option_count("/CHECKSUM/START", checksum_option_count)

          output%checksum%checksum_count = checksum_option_count
          if (checksum_option_count > 0) then

            filename = path(1:lenp)//input(1:leni)//c_null_char
            output%checksum%files_checksum = new_file_checksum_list()
            output%checksum%checksum_list=deck_checksum_creation(LENI+LENP+1,c_loc(filename))   ! Creates the checksum list / Commputes the MD5 digests
            checksum_digest_count=deck_checksum_count(output%checksum%checksum_list)  ! Count real number of checksums il list

            ! Print the checksum list in the output file
            ! -------------------------------------------
            write(iout,"(a)") " "
            write(iout,"(a)") " "
            write(iout,"(a)") "    CHECKSUM DIGESTS"
            write(iout,"(a)") "    ----------------"
            write(iout,"(a)") " "
            do i=1,checksum_digest_count
              call deck_checksum_read(output%checksum%checksum_list, i,       &
              &                          c_loc(checksum_title),c_loc(len_title), &
              &                          c_loc(checksum),c_loc(len_checksum))
              write(iout,"(a,a,a,a)") "    CHECKSUM : ",checksum_title(1:len_title), "_",checksum(1:len_checksum)
            end do
          end if
        end subroutine hm_read_checksum

!! \brief Print Starter checksum section in the output file
!! \details Write the checksum section into the starter .out file (unit iout)
!! \details with the MD5 checksums of the section between /CHECKSUM/START and /CHECKSUM/END
!! \details and the checksum information related to the output.
!||====================================================================
!||    st_checksum_file_print   ../starter/source/output/checksum/checksum_option.F90
!||--- called by ------------------------------------------------------
!||    f_anend                  ../starter/source/output/analyse/analyse_arret.F
!||--- calls      -----------------------------------------------------
!||    deck_checksum_read       ../starter/source/output/checksum/checksum_model.cpp
!||--- uses       -----------------------------------------------------
!||    file_descriptor_mod      ../starter/source/modules/file_descriptor_mod.F90
!||====================================================================
        subroutine st_checksum_file_print(output)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use names_and_titles_mod
          use file_descriptor_mod
          use output_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(output_), intent(inout) :: output
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer :: checksum_digest_count
          integer,target :: len_title
          integer,target :: len_checksum
          character(len=ncharline),target:: checksum_title
          character(len=64),target:: checksum
          character(len=ncharline):: assembled_checksum
          integer :: assembled_checksum_length
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          checksum_digest_count=deck_checksum_count(output%checksum%checksum_list)  ! Count real number of checksums in list
          allocate(output%checksum%checksums(checksum_digest_count))                ! Create the array of checksums to keep the checksum digests for the output file section. Mandatory to use common 
                                                                                    !        routine to write .checksum files for Starter & Engine

          if (checksum_digest_count > 0) then                                   ! There are checksum in the list

            write(iout,"(a)") " "
            write(iout,"(a)") " "
            write(iout,"(a)") " CHECKSUM OPTION: DECK FINGERPRINTS"
            write(iout,"(a)") " ----------------------------------"

            do i=1,checksum_digest_count

              call deck_checksum_read(output%checksum%checksum_list,i,         &
              &                          c_loc(checksum_title),c_loc(len_title),  &
              &                          c_loc(checksum),c_loc(len_checksum))
              assembled_checksum(1:ncharline)=""
              assembled_checksum=checksum_title(1:len_title)//"_"//checksum(1:len_checksum)
              assembled_checksum_length=len_title+len_checksum+1
              write(iout,"(a,a)") "    CHECKSUM: ",trim(assembled_checksum)
              output%checksum%checksums(i)=assembled_checksum(1:assembled_checksum_length)
            end do
          end if

        end subroutine st_checksum_file_print


!! \brief Writes the MD5 checksum in Restart files
!||====================================================================
!||    checksum_write_starter_restart   ../starter/source/output/checksum/checksum_option.F90
!||--- called by ------------------------------------------------------
!||    ddsplit                          ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||    deck_checksum_read               ../starter/source/output/checksum/checksum_model.cpp
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine checksum_write_starter_restart(output)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use output_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(output_), intent(in) :: output
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
          integer :: checksum_digest_count

          integer,target :: len_title
          integer,target :: len_checksum
          character(len=ncharline),target:: checksum_title
          character(len=64),target:: checksum

          character(len=ncharline):: assembled_checksum
          integer,dimension(ncharline):: i_assembled_checksum
          integer :: assembled_checksum_length
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          checksum_digest_count = output%checksum%checksum_count
          call  write_i_c(checksum_digest_count,1)
          if (checksum_digest_count > 0) then

            do i=1,checksum_digest_count

              call deck_checksum_read(output%checksum%checksum_list,i,        &
              &                            c_loc(checksum_title),c_loc(len_title), &
              &                            c_loc(checksum),c_loc(len_checksum))

              assembled_checksum(1:ncharline)=""
              assembled_checksum=checksum_title(1:len_title)//"_"//checksum(1:len_checksum)
              assembled_checksum_length=len_title+len_checksum+1
              ! Transform assembled_checksum_length to integer array
              do j=1,assembled_checksum_length
                i_assembled_checksum(j)=ichar(assembled_checksum(j:j))
              end do

              call write_i_c(assembled_checksum_length,1)
              call write_i_array_c(i_assembled_checksum,assembled_checksum_length)
            end do

          end if
        end subroutine checksum_write_starter_restart
!!
      end module checksum_starter_option_mod
