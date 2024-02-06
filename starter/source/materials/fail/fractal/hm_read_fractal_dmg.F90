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
!hd|====================================================================
!hd|  hm_read_fractal_dmg
!hd|-- called by -----------
!hd|-- calls ---------------
!hd|====================================================================
      module hm_read_fractal_dmg_mod
      contains
! ========================================================================================
! \brief read config file for fractal damage model initialization
!! \details 
! ========================================================================================

      subroutine hm_read_fractal_dmg(fail ,                                  &
                 mat_id   ,fail_id  ,irupt    ,lsubmodel,unitab   ,iout  )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use fail_param_mod
      use unitab_mod
      use message_mod
      use submodel_mod
      use hm_option_read_mod
      use constant_mod ,only : zero,one
! ---------------------------------------------------------------------------------------------
      implicit none
! ---------------------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!   D u m m y   a r g u m e n t s
!-----------------------------------------------
      integer             ,intent(in) :: fail_id             !< failure model ID
      integer             ,intent(in) :: mat_id              !< material law ID
      integer             ,intent(in) :: irupt               !< failure model number
      integer             ,intent(in) :: iout                !< output file ID
      type (unit_type_)   ,intent(in) :: unitab              !< table of input units
      type (submodel_data),intent(in) :: lsubmodel(nsubmod)  !< submodel table 
      type (fail_param_)  ,intent(inout) :: fail             !< failure model data structure
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,set1,set2,n_rwalk,seed,debug
      integer :: grsh4n_1,grsh3n_1,grsh4n_2,grsh3n_2
      my_real :: dmg,probability
      logical :: is_available,is_encrypted
!=======================================================================
      is_encrypted = .false.
      is_available = .false.
!--------------------------------------------------
!     check encryption
!
      call hm_option_is_encrypted(is_encrypted)
!--------------------------------------------------
!     Read config file
!--------------------------------------------------
      call hm_get_intv   ('grsh4n_1'       ,grsh4n_1    ,is_available,lsubmodel)
      call hm_get_intv   ('grsh3n_1'       ,grsh3n_1    ,is_available,lsubmodel)
      call hm_get_intv   ('grsh4n_2'       ,grsh4n_2    ,is_available,lsubmodel)
      call hm_get_intv   ('grsh3n_2'       ,grsh3n_2    ,is_available,lsubmodel)
      call hm_get_floatv ('Damage'         ,dmg         ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Probability'    ,probability ,is_available,lsubmodel,unitab)
      call hm_get_intv   ('Seed'           ,seed        ,is_available,lsubmodel)
      call hm_get_intv   ('Num_walk'       ,n_rwalk     ,is_available,lsubmodel)
      call hm_get_intv   ('Printout'       ,debug       ,is_available,lsubmodel)
!------------------------------------------------------------------------------------
      dmg = max(zero, min(dmg,one))
!------------------------------------------------------------------------------------
      fail%keyword = 'Fractal damage'
      fail%irupt   = irupt
      fail%fail_id = fail_id
      fail%nuparam = 2
      fail%niparam = 7
      fail%nfunc   = 0
      fail%ntable  = 0
!      
      allocate(fail%uparam(fail%nuparam))
      allocate(fail%iparam(fail%niparam))
      allocate(fail%ifunc(fail%nfunc))
      allocate(fail%table(fail%ntable))
!      
      fail%iparam(1) = n_rwalk
      fail%iparam(2) = seed
      fail%iparam(3) = grsh4n_1
      fail%iparam(4) = grsh3n_1
      fail%iparam(5) = grsh4n_2
      fail%iparam(6) = grsh3n_2
      fail%iparam(7) = debug
!      
      fail%uparam(1) = dmg
      fail%uparam(2) = probability
!---------------------------------------------------------------------
      if (is_encrypted) then
        write(iout,1000)
      else 
        write(iout, 2000) mat_id,fail_id,dmg,probability,seed,n_rwalk,        &
                          grsh4n_1,grsh3n_1,grsh4n_2,grsh3n_2
      endif
!---------------------------------------------------------------------
 1000 format(                                                                  &
      5X,'    CONFIDENTIAL DATA                               '/,              &
      5X,'    -----------------                               '/)               
 2000 format(                                                                  &
      5X,'  FRACTAL DAMAGE INITIALIZATION                     ',/,             &
      5X,'  -----------------------------                     ',/              &
      5X,'MATERIAL ID . . . . . . . . . . . . . . . . . . . .=',I10/           &
      5X,'FAILURE MODEL ID. . . . . . . . . . . . . . . . . .=',I10/           &
      5X,'INITIAL DAMAGE VALUE. . . . . . . . . . . . . . . .=',E12.4/         &
      5X,'DAMAGE PROBABILITY. . . . . . . . . . . . . . . . .=',E12.4/         &
      5X,'RANDOM SEED . . . . . . . . . . . . . . . . . . . .=',I10/           &
      5X,'NUMBER OF WALKERS . . . . . . . . . . . . . . . . .=',I10/           &
      5X,'STARTING SH4N ELEMENT GROUP . . . . . . . . . . . .=',I10/           &
      5X,'STARTING SH3N ELEMENT GROUP . . . . . . . . . . . .=',I10/           &
      5X,'DAMAGED SH4N ELEMENT GROUP. . . . . . . . . . . . .=',I10/           &
      5X,'DAMAGED SH3N ELEMENT GROUP. . . . . . . . . . . . .=',I10/)          
!-----------
      return
      end
!-----------
      end module

