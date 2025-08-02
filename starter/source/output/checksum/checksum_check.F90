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
!||    checksum_check_mod   ../starter/source/output/checksum/checksum_check.F90
!||--- called by ------------------------------------------------------
!||    starter0             ../starter/source/starter/starter0.F
!||====================================================================
      module checksum_check_mod

      contains
!! \brief Grab all checksums from execution directory & write them in output file.
!! \details All checksums from .out, Animation & TimeHistory are grabbed.
!||====================================================================
!||    checksum_check        ../starter/source/output/checksum/checksum_check.F90
!||--- called by ------------------------------------------------------
!||    starter0              ../starter/source/starter/starter0.F
!||--- calls      -----------------------------------------------------
!||    grab_checksums        ../starter/source/output/checksum/checksum_list.cpp
!||    radioss_title         ../starter/source/starter/radioss_title.F
!||--- uses       -----------------------------------------------------
!||    file_descriptor_mod   ../starter/source/modules/file_descriptor_mod.F90
!||====================================================================
        subroutine checksum_check(rootname,PATH,cpunam,archtitle,iresp)
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
          character(len=80), intent(in)   :: rootname
          character(len=2048), intent(in) :: path
          integer, intent(in)             :: iresp    ! single or double precision
          character(len=20),intent(in)    :: cpunam
          character(len=66),intent(in)    :: archtitle
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          ! Variables for get_file_name_info
          integer :: lenp    ! path length
          integer :: lenr    ! length deck rootname
          ! Checksum output file
          character(len=2048) :: checksum_file
          character(len=2028) :: titl_filename
          character(len=4) :: runn    ! run nummber
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          titl_filename=''
          checksum_file=''
          runn='0000'
          lenr=len_trim(rootname)

          call  radioss_title(istdo,cpunam,archtitle,rootname,len_trim(rootname),runn,iresp,1)
          write(istdo,'(A)')' .. CHECKSUMS FROM INPUT DECK AND RESULT FILES'

          ! Open checksum file
          checksum_file  = trim(rootname)//'.report'
          open(unit=fchecksum,file=trim(checksum_file),access='sequential',form='formatted',status='unknown')

          ! Write checksum file header
          call  radioss_title(fchecksum,cpunam,archtitle,rootname,len_trim(rootname),runn,iresp,1)

          write(fchecksum,'(A)')' '
          write(fchecksum,'(A)')'    DECK AND OUTPUT CHECKSUM REPORT'
          write(fchecksum,'(A)')'    -------------------------------'
          write(fchecksum,'(A)')'    ROOTNAME. . . . . . . . . . . . .     '//trim(rootname)
          write(fchecksum,'(A)')'    DIRECTORY . . . . . . . . . . . .     '//trim(path)
          write(fchecksum,'(A)')' '

          lenp=len_trim(path)
          call grab_checksums(fchecksum,rootname,lenr,path,lenp);

          close(unit=fchecksum)

          ! Write output file footer
          write(istdo,'(A)')' .. DONE'
          write(istdo,'(A)')'------------------------------------------------------------------------'
          write(istdo,'(A)')' '
          write(istdo,'(A)')'    CHECKSUMS ARE WRITTEN IN '//trim(checksum_file)
          write(istdo,'(A)')' '
        end subroutine checksum_check
      end module checksum_check_mod
