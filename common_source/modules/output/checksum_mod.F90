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
      module checksum_output_option_mod

        use iso_c_binding

!! \brief Engine checksum type
!! \details Hosts the checksums from input file.
!! \details item is to print them in output files : *.out, animation, H3D, TimeHistory
        use names_and_titles_mod , only : ncharline


        type checksum_option_
          integer :: checksum_count
          integer :: st_checksum_read
          character(len=ncharline) :: rootname      ! Stores rootname of -checksum option
          character(len=ncharline),dimension(:),allocatable :: checksums
          character(len=8) :: date
          character(len=10) :: time
          type(C_PTR) :: checksum_list
          type(C_PTR) :: files_checksum
        end type checksum_option_

        interface

          function new_file_checksum_list()  bind (C, name='new_file_checksum_list')
            use iso_c_binding
            type(C_PTR) :: new_file_checksum_list
          end function new_file_checksum_list

          subroutine compute_binary_checksum(checksum_list,file, len , izip)  bind (C, name='compute_binary_checksum')
            use iso_c_binding
            type(C_PTR),value :: checksum_list
            type(C_PTR),value :: file
            integer(C_INT),value :: len
            integer(C_INT),value :: izip

          end subroutine compute_binary_checksum

          subroutine print_checksum_list(checksum_list,unit) bind (C, name='print_checksum_list')
            use iso_c_binding
            type(C_PTR),value :: checksum_list
            integer(C_INT),value :: unit
          end subroutine print_checksum_list
        end interface

      contains
!! \brief reads checksum from restart file
!||====================================================================
!||    checksum_restart_read   ../common_source/modules/output/checksum_mod.F90
!||--- called by ------------------------------------------------------
!||    rdresb                  ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    read_i_c                ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    file_descriptor_mod     ../engine/source/modules/file_descriptor_mod.F90
!||    names_and_titles_mod    ../common_source/modules/names_and_titles_mod.F
!||====================================================================
        subroutine checksum_restart_read(checksum)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use names_and_titles_mod
          use file_descriptor_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(checksum_option_), intent(inout) :: checksum
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: checksum_option_count
          integer :: i
          integer :: j
          integer :: checksum_digest_length
          integer,dimension(ncharline) :: checksum_digest
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call read_i_c(checksum_option_count, 1)                !! Read number of checksums
          checksum%checksum_count = checksum_option_count
          if (checksum_option_count > 0) then
            checksum%files_checksum = new_file_checksum_list()
          end if

          allocate(checksum%checksums(checksum_option_count))
          checksum%checksums(1:checksum_option_count)(:)=''

          do i=1,checksum_option_count
            call read_i_c(checksum_digest_length, 1)
            call read_i_array_c(checksum_digest, checksum_digest_length)

            do j=1,checksum_digest_length
              checksum%checksums(i)(j:j) = char(checksum_digest(j))
            end do
          end do

          !  do i=1,checksum_option_count
          !      print *, 'checksum%checksums(',i,') = ', trim(checksum%checksums(i))
          ! end do
        end subroutine checksum_restart_read

        !! \brief reads checksum from result file
!||====================================================================
!||    checksum_restart_write   ../common_source/modules/output/checksum_mod.F90
!||--- called by ------------------------------------------------------
!||    wrrestp                  ../engine/source/output/restart/wrrestp.F
!||--- calls      -----------------------------------------------------
!||    write_i_c                ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    file_descriptor_mod      ../engine/source/modules/file_descriptor_mod.F90
!||    names_and_titles_mod     ../common_source/modules/names_and_titles_mod.F
!||====================================================================
        subroutine checksum_restart_write(checksum)
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Modules
          ! ----------------------------------------------------------------------------------------------------------------------
          use names_and_titles_mod
          use file_descriptor_mod
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Implicit none
          ! ----------------------------------------------------------------------------------------------------------------------
          implicit none
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Arguments
          ! ----------------------------------------------------------------------------------------------------------------------
          type(checksum_option_), intent(in) :: checksum
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Local variables
          ! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer :: j
          integer :: checksum_digest_length
          integer,dimension(ncharline) :: checksum_digest
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Body
          ! ----------------------------------------------------------------------------------------------------------------------
          call write_i_c(checksum%checksum_count, 1)                !! write number of checksums

          do i=1,checksum%checksum_count

            checksum_digest_length = len_trim(checksum%checksums(i))
            do j=1,checksum_digest_length
              checksum_digest(j)=ichar(checksum%checksums(i)(j:j))
            end do
            call write_i_c(checksum_digest_length, 1)
            call write_i_array_c(checksum_digest, checksum_digest_length)

          end do
        end subroutine checksum_restart_write

        !! \brief writes checksum in .out file
!||====================================================================
!||    checksum_option_outfile   ../common_source/modules/output/checksum_mod.F90
!||--- called by ------------------------------------------------------
!||    radioss2                  ../engine/source/engine/radioss2.F
!||--- uses       -----------------------------------------------------
!||    file_descriptor_mod       ../engine/source/modules/file_descriptor_mod.F90
!||====================================================================
        subroutine checksum_option_outfile(checksum)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use file_descriptor_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(checksum_option_), intent(in) :: checksum   ! checksum structure with the deck fingerprints
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          write(iout,'(a)') ' '
          write(iout,'(a)') ' CHECKSUM DIGESTS'
          write(iout,'(a)') ' ----------------'
          write(iout,'(a)') ' '
          do i=1,checksum%checksum_count
            write(iout,'(a,a)') '    CHECKSUM : ',trim( checksum%checksums(i))
          end do
          write(iout,'(a)') ' '
        end subroutine checksum_option_outfile

        !! \brief writes checksum in .out file
!||====================================================================
!||    checksum_option_checksum_file   ../common_source/modules/output/checksum_mod.F90
!||--- called by ------------------------------------------------------
!||    arret                           ../engine/source/system/arret.F
!||--- calls      -----------------------------------------------------
!||    print_checksum_list             ../common_source/output/checksum/checksum.cpp
!||--- uses       -----------------------------------------------------
!||    file_descriptor_mod             ../engine/source/modules/file_descriptor_mod.F90
!||    names_and_titles_mod            ../common_source/modules/names_and_titles_mod.F
!||====================================================================
        subroutine checksum_option_checksum_file(checksum,rootname,rootlen,chrun)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use names_and_titles_mod
          use file_descriptor_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer :: rootlen                               ! length of the rootname
          character(len=4) :: chrun                        ! run number
          character(len=rootlen) :: rootname               ! rootname of the input file
          type(checksum_option_), intent(in) :: checksum   ! checksum structure with the deck fingerprints
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          character(len=2048) :: checksum_file      ! Checksum output file
          character(len=40) :: formated_date_time        ! date in same format as in the output file
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          checksum_file  = rootname(1:rootlen)//'_'//chrun//'.checksum'
          open(unit=fchecksum,file=trim(checksum_file),access='sequential',form='formatted',status='unknown')
          formated_date_time = checksum%date(1:4)//'/'//checksum%date(5:6)//'/'//checksum%date(7:8)//'  '// &
          &                                  checksum%time(1:2)//':'//checksum%time(3:4)//':'//checksum%time(5:6)
          write(fchecksum,'(a)') ' ************************************************************************'
          write(fchecksum,'(a)') ' **                                                                    **'
          write(fchecksum,'(a)') ' **                                                                    **'
          write(fchecksum,'(a)') ' **                           Checksum Digest                          **'
          write(fchecksum,'(a)') ' **                                                                    **'
          write(fchecksum,'(a)') ' **                                                                    **'
          write(fchecksum,'(a)') ' ************************************************************************'
          write(fchecksum,'(a)') ' ** OpenRadioss Software                                               **'
          write(fchecksum,'(a)') ' ** COPYRIGHT (C) 1986-2025 Altair Engineering, Inc.                   **'
          write(fchecksum,'(a)') ' ** Licensed under GNU Affero General Public License.                  **'
          write(fchecksum,'(a)') ' ** See License file.                                                  **'
          write(fchecksum,'(a)') ' ************************************************************************'
          write(fchecksum,'(a,a)') ' DECK ROOTNAME .............................:      ',rootname(1:rootlen)
          write(fchecksum,'(a,a)') ' EXECUTION COMPLETED .......................:      ',trim(formated_date_time)
          write(fchecksum,'(a)') ' '
          write(fchecksum,'(a)') ' DECK FINGERPRINTS'
          write(fchecksum,'(a)') ' -----------------'

          do i=1,checksum%checksum_count
            write(fchecksum,'(a,a)') '    CHECKSUM : ',trim( checksum%checksums(i))
          end do

          write(fchecksum,'(a)') ' '
          write(fchecksum,'(a)') ' OUTPUT FILES CHECKSUM DIGESTS'
          write(fchecksum,'(a)') ' -----------------------------'
          call print_checksum_list(checksum%files_checksum,fchecksum )
          write(fchecksum,'(a)') ' '


          close(unit=fchecksum)

        end subroutine checksum_option_checksum_file

      end module checksum_output_option_mod

