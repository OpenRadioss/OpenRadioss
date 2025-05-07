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
      module hm_read_fail_lemaitre_mod
      contains
      subroutine hm_read_fail_lemaitre(                                       &
                  fail     ,fail_id  ,irupt    ,lsubmodel,unitab   ,iout     ) 
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use fail_param_mod
      use unitab_mod
      use message_mod 
      use submodel_mod
      use hm_option_read_mod 
      use elbuftag_mod
      use constant_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
      implicit none 
#include  "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer             ,INTENT(IN)    :: fail_id      !< failure model ID
      integer             ,INTENT(IN)    :: irupt        !< failure model number
      type (unit_type_)   ,INTENT(IN)    :: unitab       !< table of input units
      type (submodel_data),INTENT(IN)    :: lsubmodel(*) !< submodel table
      type (fail_param_)  ,INTENT(INOUT) :: fail         !< failure model data structure
      integer             ,INTENT(IN)    :: iout         !< output unit number
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: failip
      my_real :: epsd,s,dc,pthk
      logical :: is_available,is_encrypted
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
      is_encrypted = .false.
      is_available = .false.
!----------------------------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
!----------------------------------------------------------------
      !< Lemaitre damage model parameters
      call hm_get_floatv('FAIL_EPSD'  ,epsd   ,is_available,lsubmodel,unitab)
      call hm_get_floatv('FAIL_S'     ,s      ,is_available,lsubmodel,unitab)
      call hm_get_floatv('FAIL_DC'    ,dc     ,is_available,lsubmodel,unitab)
      call hm_get_intv  ('FAILIP'     ,failip ,is_available,lsubmodel)
      call hm_get_floatv('P_thickfail',pthk   ,is_available,lsubmodel,unitab)
!--------------------------
      !< Check values and set default values
      if (epsd == zero) epsd = infinity
      dc = max(dc,zero)
      dc = min(dc,one)
      failip = max(failip,1)
      pthk = max(pthk,zero)
      pthk = min(pthk,one)
      if (pthk == zero) pthk = one
!--------------------------
!     Filling buffer tables
!--------------------------   
      !< Failure tables size
      fail%keyword = 'LEMAITRE' 
      fail%irupt   = irupt 
      fail%fail_id = fail_id 
      fail%niparam = 1
      fail%nuparam = 3
      fail%nuvar   = 0
      fail%nfunc   = 0
      fail%ntable  = 0
      fail%nmod    = 0
      fail%pthk    = pthk
!      
      !< Allocation of failure parameters tables      
      allocate (fail%iparam(fail%niparam))
      allocate (fail%uparam(fail%nuparam))
!
      !< Integer material parameter
      fail%iparam(1) = failip
!      
      !< Real material parameters
      fail%uparam(1) = epsd
      fail%uparam(2) = s
      fail%uparam(3) = dc
!
!--------------------------
!     Parameters printout
!--------------------------
      if (is_encrypted)then
        write(iout,'(5X,A,//)')'CONFIDENTIAL DATA'
      else       
        write(iout, 1000) epsd,s,dc
        write(iout, 1100) pthk,failip 
      endif
!---------------------------
 1000 format(/                                                                 &
     & 5X,'---------------------------------------------------------',/,       &
     & 5X,'     LEMAITRE CONTINUUM DAMAGE MECHANICS (CDM) MODEL     ',/,       &
     & 5X,'---------------------------------------------------------',/,       & 
     & 5X,'                                                         ',/,       &
     & 5X,'DAMAGE SOFTENING PARAMETERS:                             ',/,       &
     & 5X,'----------------------------                             ',/,       &
     & 5X,'EFFECTIVE PLASTIC STRAIN THRESHOLD (EPSD). . . . . . . .=',1pg20.13/&
     & 5X,'SOFTENING PARAMETER (S). . . . . . . . . . . . . . . . .=',1pg20.13/&
     & 5X,'CRITICAL DAMAGE AT FAILURE (DC). . . . . . . . . . . . .=',1pg20.13/)
 1100 format(/                                                                 &
     & 5X,'ELEMENT DELETION:                                        ',/,       &
     & 5X,'-----------------                                        ',/,       &
     & 5X,'SHELL ELEMENT DELETION PARAMETER PTHICKFAIL. . . . . . .=',1pg20.13,/&
     & 5X,'   > 0.0: FRACTION OF FAILED THICKNESS                   ',/,       &
     & 5X,'   < 0.0: FRACTION OF FAILED INTG. POINTS                ',/,       &
     & 5X,'NUMBER OF FAILED INTG. POINTS PRIOR TO ELEM DELETION . .=',i10/)    
!    
      end subroutine hm_read_fail_lemaitre
      end module hm_read_fail_lemaitre_mod
