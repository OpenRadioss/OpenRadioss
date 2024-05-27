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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Reader for option /EOS/EXPONENTIAL
!! \details  PM(31) = P(MU0,E0) -> will be used to initialize pressure from stress tensor SIG(1:3,*)
      subroutine hm_read_eos_exponential(iout,pm,unitab,lsubmodel,npropm)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use unitab_mod
      use submodel_mod
      use constant_mod , only : three100, em20, zero
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: npropm                                     !< size for pm array
      integer,intent(in) :: iout                                       !< file unit of starter listing
      type (unit_type_),intent(in) :: unitab                           !< data structure for unit systems required by reader subroutines
      my_real, intent(inout) :: pm(npropm)                             !< material parameters
      type(submodel_data), dimension(nsubmod), intent(in) :: lsubmodel !< submodel data structure required for reader subroutines
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      my_real :: p0, alpha, psh,ssp0
      logical :: is_encrypted, is_available
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      is_encrypted = .false.
      is_available = .false.

      call hm_option_is_encrypted(is_encrypted)

      call hm_get_floatv('MAT_C0', p0, is_available,lsubmodel,unitab)
      call hm_get_floatv('Alpha', alpha, is_available,lsubmodel,unitab)
      call hm_get_floatv('MAT_PSH', psh, is_available,lsubmodel,unitab)

      pm(104)=p0-psh
      pm( 31)=p0-psh
      pm( 32)=alpha
      pm( 88)=psh
      if(pm(79)==zero)pm(79)=three100
      ssp0 = em20
      pm(27)=ssp0

      write(iout,1000)
      if(is_encrypted)then
        write(iout,'(5X,A,//)')'CONFIDENTIAL DATA'
      else
        write(iout,1500)p0,alpha,psh
      endif

      return
 1000 format(&
      5X,'  EXPONENTIAL EOS',/,&
      5X,'  ---------------')
 1500 format(&
      5X,'INITIAL PRESSURE (P0) . . . . . . . . . .=',1PG20.13/,&
      5X,'DECAY PARAMETER (ALPHA) . . . . . . . . .=',1PG20.13/,&
      5X,'PRESSURE SHIFT (PSH)  . . . . . . . . . .=',1PG20.13/)

      return
      end subroutine hm_read_eos_exponential

