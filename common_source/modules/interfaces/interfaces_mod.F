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
Chd|  STATE_MOD                     modules/interfaces_mod.F           
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|        ECRIT                         engine/source/output/ecrit.F  
Chd|        FREDYNAIN                     engine/source/input/fredynain.F
Chd|        FREFORM                       engine/source/input/freform.F 
Chd|        GENDYNAIN                     engine/source/output/dynain/gendynain.F
Chd|        LECINP                        engine/source/input/lecinp.F  
Chd|        LECTUR                        engine/source/input/lectur.F  
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RDCOMR                        engine/source/output/restart/rdcomm.F
Chd|        RDRESA                        engine/source/output/restart/rdresa.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        WRCOMR                        engine/source/output/restart/wrcomm.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
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
