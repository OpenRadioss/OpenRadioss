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

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Detect encoding of greek letter mu and replace it with ASCII 'mu' (two letters)
!! \details treatment is done for each argument key1,...,key6
!! \details  {key1,key2,key3} is input unit system
!! \details  {key3,key4,key5} is working unit system
!! \details      (ISO 8859, Latin, Windows) : greek letter \mu has decimal code 181
!! \details      (UTF-8)                    : greek letter \mu has double octet (0xC2):194 (0XB5):181
!! \details  conditional test is then : ( iachar(key(1:1))==181  .or. (iachar(key(1:1))==194 .AND. iachar(key(2:2))==181) )
      !||====================================================================
      !||    ascii_encoding_mu_letter   ../starter/source/starter/ascii_encoding_mu_letter.F90
      !||--- called by ------------------------------------------------------
      !||    contrl                     ../starter/source/starter/contrl.F
      !||    hm_read_unit               ../starter/source/general_controls/computation/hm_read_unit.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine ascii_encoding_mu_letter(key1,key2,key3,key4,key5,key6)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
!  [ the module names in use must be in uppercase for now, it will change latter]
!  [ ONLY is mandatory, note the space before the ,]
          use names_and_titles_mod , only : ncharkey
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          character(len=ncharkey),intent(inout) :: key1, key2, key3 !< INPUT unit system
          character(len=ncharkey),intent(inout) :: key4, key5, key6 !< WORKING unit system
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          character(len=ncharkey), pointer :: pkey                 !< pointer on list_key (key1 -> key6)
          character(len=ncharkey),dimension(6),target :: list_key  !< list of all keys : key1,key2,...key6
          character(len=1),dimension(6) :: list_MLT                !< MLT codes : (m,g,s)
          integer ii
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
       
        !init.
        list_key(1:6) = (/ key1,key2,key3,key4,key5,key6 /)
        list_MLT(1)='m' ; list_MLT(2)='g' ; list_MLT(3)='s'
        list_MLT(4)='m' ; list_MLT(5)='g' ; list_MLT(6)='s'

        ! (ISO 8859, Latin, Windows) : greek letter \mu has decimal code 181
        ! (UTF-8)                    : greek letter \mu has double octet (0xC2):194 (0XB5):181
         do ii=1,6
          pkey => list_key(ii)
          if( iachar(pkey(1:1))==181  .or. (iachar(pkey(1:1))==194 .AND. iachar(pkey(2:2))==181) )then
            ! greek letter \mu detected
            pkey(1:ncharkey)='' ! reset
            pkey(1:2)='mu' ! use ASCII representation instead (two letters)
            pkey(3:3)=list_MLT(ii) ! add MLT code : 'm', 'g' or 's'
          endif
        enddo
        
        key1=list_key(1); key2=list_key(2); key3=list_key(3)   !INPUT system
        key4=list_key(4); key5=list_key(5); key6=list_key(6)   !WORKING system

! ----------------------------------------------------------------------------------------------------------------------
      end subroutine ascii_encoding_mu_letter

