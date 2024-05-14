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
#include "my_real.inc"
!-----------------------------------------------------------------------
!hd|====================================================================
!hd|  PLOAD_CYL_MOD                 modules/loads/pload_cyl_mod.F
!hd|-- called by -----------
!hd|        LOADS_MOD                     common_source/modules/loads/loads_mod.F
!hd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
!hd|        WRITE_PCYL                    starter/source/loads/general/load_pcyl/write_pcyl.F
!hd|        PRESS_SEG3                    engine/source/loads/general/load_pcyl/press_seg3.F
!hd|        RDCOMI                        engine/source/output/restart/rdcomm.F
!hd|        RDRESB                        engine/source/output/restart/rdresb.F
!hd|-- calls ---------------
!hd|====================================================================
      MODULE PLOAD_CYL_MOD
!-----------------------------------------------------------------------
        TYPE PRESS_CYL_
          INTEGER   :: ID                !  option ID
          INTEGER   :: IFRAME            !  moving frame
          INTEGER   :: ISENS             !  sensor number
          INTEGER   :: ITABLE            !  table number
          INTEGER   :: NSEG              !  number of surface segments
          INTEGER ,DIMENSION(:,:) ,ALLOCATABLE :: SEGNOD ! segment nodes(NSEG,4)
          my_real   :: XSCALE_R
          my_real   :: XSCALE_T
          my_real   :: YSCALE
          ! ---------
          ! starter only
          INTEGER ,DIMENSION(:) ,ALLOCATABLE :: NUMBER_SEG_PER_PROC ! number of segment defined on a proc
          INTEGER ,DIMENSION(:) ,ALLOCATABLE :: PROC_ID ! processor id where the segment is defined
          ! ---------

          ! ---------
          ! parith/on option : starter and engine
          INTEGER :: S_SEGMENT_ADRESS ! second dimension of SEGMENT_ADRESS
          INTEGER, DIMENSION(:,:), ALLOCATABLE :: SEGMENT_ADRESS ! adress to point to the adsky array, dim =(4,S_SEGMENT_ADRESS) (parith/on option)
          ! ---------
        END TYPE PRESS_CYL_
!-----------------------------------------------------------------------
      END MODULE PLOAD_CYL_MOD
