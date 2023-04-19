Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  INTERFACES_MOD                modules/interfaces/interfaces_mod.F
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        HM_READ_INTERFACES            starter/source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTER_STRUCT          starter/source/interfaces/reader/hm_read_inter_struct.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        I24MAINF                      engine/source/interfaces/int24/i24main.F
Chd|        I25MAINF                      engine/source/interfaces/int25/i25mainf.F
Chd|        INTFOP2                       engine/source/interfaces/interf/intfop2.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDRESA                        engine/source/output/restart/rdresa.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        PARAMETERS_MOD                modules/interfaces/parameters_mod.F
Chd|====================================================================
        MODULE INTERFACES_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE PARAMETERS_MOD
       implicit  none
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"

!   -----------------------------------------------
!   D e r i v e d   T y p e   D e f i n i t i o n s
!   -----------------------------------------------

!   -------------------------

C----------------------------------------------
C   Global Interfaces stucture
C---------------------------------------------

          TYPE INTERFACES_
               TYPE (PARAMETERS_) PARAMETERS
          END TYPE INTERFACES_

C
        END MODULE INTERFACES_MOD
