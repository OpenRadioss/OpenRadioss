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
      !||    hm_read_fail_spalling_mod   ../starter/source/materials/fail/spalling/hm_read_fail_spalling.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_fail                ../starter/source/materials/fail/hm_read_fail.F
      !||====================================================================
      module hm_read_fail_spalling_mod
      contains
      !||====================================================================
      !||    hm_read_fail_spalling    ../starter/source/materials/fail/spalling/hm_read_fail_spalling.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_fail_spalling(                                        &
                 fail     ,mat_id   ,fail_id  ,irupt    ,lsubmodel,unitab   ,  &
                 fail_tag ,iout     )
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
      type (fail_param_)  ,intent(inout) :: fail
      integer             ,intent(in)    :: mat_id        
      integer             ,intent(in)    :: fail_id       
      integer             ,intent(in)    :: irupt         
      type (submodel_data),intent(in)    :: lsubmodel(*) 
      type (unit_type_)   ,intent(in)    :: unitab        
      type (fail_tag_)    ,intent(inout) :: fail_tag
      integer             ,intent(in)    :: iout
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ishell,isolid
      my_real :: d1,d2,d3,d4,d5,epsp0,pmin
      logical :: is_available,is_encrypted
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
      is_encrypted = .false.
      is_available = .false.
!----------------------------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
!----------------------------------------------------------------
      !< Johnson-Cook failure criterion parameters
      call hm_get_floatv('D1'           ,d1     ,is_available,lsubmodel,unitab)
      call hm_get_floatv('D2'           ,d2     ,is_available,lsubmodel,unitab)
      call hm_get_floatv('D3'           ,d3     ,is_available,lsubmodel,unitab)
      call hm_get_floatv('D4'           ,d4     ,is_available,lsubmodel,unitab)
      call hm_get_floatv('D5'           ,d5     ,is_available,lsubmodel,unitab)
      !< Spalling failure criterion parameters
      call hm_get_floatv('Epsilon_Dot_0',epsp0  ,is_available,lsubmodel,unitab)
      call hm_get_floatv('P_min'        ,pmin   ,is_available,lsubmodel,unitab)
      call hm_get_intv  ('Ifail_so'     ,isolid ,is_available,lsubmodel)
!---------------------------
      if (epsp0 == zero) epsp0 =  em20
      pmin = -abs(pmin)
      if (pmin == zero) pmin = -ep20
      isolid = max(1,isolid)
      isolid = min(6,isolid)
!---------------------------
      fail%keyword = 'SPALLING' 
      fail%irupt   = irupt 
      fail%fail_id = fail_id 
      fail%nuparam = 7
      fail%niparam = 1
      fail%nuvar   = 0
      fail%nfunc   = 0
      fail%ntable  = 0
      fail%nmod    = 2
!--------------------------
!     Filling buffer tables
!--------------------------   
!      
      !< Allocation of failure parameters tables      
      allocate (fail%uparam(fail%nuparam))
      allocate (fail%iparam(fail%niparam))
      allocate (fail%ifunc (fail%nfunc))
      allocate (fail%table (fail%ntable))
      allocate (fail%mode  (fail%nmod))
!
      ! Modes of failure
      fail_tag%lf_dammx = 1 + fail%nmod
      fail%mode(1)  = "Johnson-Cook criterion"
      fail%mode(2)  = "Pmin criterion"
!
      !< Integer material parameter
      fail%iparam(1) = isolid
!      
      !< Real material parameters
      fail%uparam(1) = d1
      fail%uparam(2) = d2
      fail%uparam(3) = d3
      fail%uparam(4) = d4
      fail%uparam(5) = d5
      fail%uparam(6) = epsp0
      fail%uparam(7) = pmin
!
!--------------------------
!     Parameters printout
!--------------------------
      if (is_encrypted)then
        write(iout,'(5X,A,//)')'CONFIDENTIAL DATA'
      else       
        write(iout, 1000)d1,d2,d3,d4,d5,epsp0,pmin         
        if (isolid == 1) then
          write(iout, 1100)
        elseif (isolid == 2) then
          write(iout, 1200)
        elseif (isolid == 3) then
          write(iout, 1300)
        elseif (isolid == 4) then
          write(iout, 1400)  
        elseif (isolid == 5) then
          write(iout, 1500)
        elseif (isolid == 6) then
          write(iout, 1600)
        endif
        if (d3 > zero) then
          call ancmsg(msgid = 831,                                             &
                      msgtype = msgwarning,                                    &
                      anmode = aninfo,                                         &
                      i1 = mat_id)     
       endif
!
      endif
!---------------------------
 1000 format(/                                                                 &
     & 5X,' --------------------------------------------------------',/,       &
     & 5X,'      JOHNSON-COOK AND SPALLING FAILURE CRITERION        ',/,       &
     & 5X,' --------------------------------------------------------',/,       & 
     & 5X,'                                                         ',/,       &
     & 5X,'JOHNSON-COOK CRITERION PARAMETERS:                       ',/,       &
     & 5X,'----------------------------------                       ',/,       &
     & 5X,'FIRST  FAILURE PARAMETER (D1). . . . . . . . . . . . . .=',1PG20.13/&
     & 5X,'SECOND FAILURE PARAMETER (D2). . . . . . . . . . . . . .=',1PG20.13/&
     & 5X,'THIRD  FAILURE PARAMETER (D3). . . . . . . . . . . . . .=',1PG20.13/&
     & 5X,'FORTH  FAILURE PARAMETER (D4). . . . . . . . . . . . . .=',1PG20.13/&
     & 5X,'FIFTH  FAILURE PARAMETER (D5). . . . . . . . . . . . . .=',1PG20.13/&
     & 5X,'REFERENCE STRAIN RATE (EPSP0). . . . . . . . . . . . . .=',1PG20.13/&
     & 5X,'                                                         ',/,       &
     & 5X,'SPALLING CRITERION PARAMETER:                            ',/,       &
     & 5X,'-----------------------------                            ',/,       &
     & 5X,'LIMIT PRESSURE (PMIN). . . . . . . . . . . . . . . . . .=',1PG20.13/)
 1100 format(/                                                                 &
     & 5X,'NO JOHNSON-COOK CRITERION, ONLY SPALLING MINIMUM PRESSURE',/,       &
     & 5X,'IS USED. DEVIATORIC STRESS TENSOR WILL BE NULLYFIED AND  ',/,       &
     & 5X,'HYDROSTATIC PRESSURE WILL BE LIMITED TO COMPRESSION.     ',/,       &
     & 5X,'UNDER TENSILE LOADING THE STRESS TENSOR WILL VANISH.     ',/)    
 1200 format(/                                                                 &
     & 5X,'JOHNSON-COOK CRITERION IS USED TO TRIGGER ELEMENT FAILURE',/,       &
     & 5X,'WHEREAS SPALLING MINIMUM PRESSURE IS USED TO NULLIFY THE ',/,       &
     & 5X,'DEVIATORIC STRESS TENSOR AND LIMIT HYDROSTATIC PRESSURE  ',/,       &
     & 5X,'TO COMPRESSION.                                          ',/,       &
     & 5X,'UNDER TENSILE LOADING THE STRESS TENSOR WILL VANISH.     ',/)
 1300 format(/                                                                 &
     & 5X,'JOHNSON-COOK CRITERION IS USED TO NULLIFY THE DEVIATORIC ',/,       &
     & 5X,'STRESS TENSOR. SPALLING MINIMUM PRESSURE IS USED TO LIMIT',/,       &
     & 5X,'HYDROSTATIC PRESSURE TO COMPRESSION.                     ',/,       &
     & 5X,'UNDER TENSILE LOADING THE STRESS TENSOR WILL VANISH.     ',/) 
 1400 format(/                                                                 &
     & 5X,'BOTH JOHNSON-COOK AND SPALLING MINIMUM PRESSURE CRITERION',/,       &
     & 5X,'ARE USED TO TRIGGER ELEMENT DELETION                     ',/)   
 1500 format(/                                                                 &
     & 5X,'HYDROSTATIC PRESSURE IS LIMITED BY THE SPALLING MINIMUM  ',/,       &
     & 5X,'PRESSURE. DEVIATORIC STRESS TENSOR IS UNTOUCHED          ',/)
 1600 format(/                                                                 &
     & 5X,'IF MAXIMUM PRINCIPAL STRESS OVERTAKE THE SPALLING MINIMUM',/,       &
     & 5X,'PRESSURE, THE DEVIATORIC TENSOR IS NULLYFIED AND         ',/,       &
     & 5X,'HYDROSTATIC PRESSURE IS LIMITED TO COMPRESSION.          ',/,       &
     & 5X,'UNDER TENSILE LOADING THE STRESS TENSOR WILL VANISH.     ',/)

      end subroutine hm_read_fail_spalling
      end module hm_read_fail_spalling_mod