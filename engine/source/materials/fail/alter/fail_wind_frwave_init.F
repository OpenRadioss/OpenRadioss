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
Chd|  FAIL_WIND_FRWAVE_INIT         source/materials/fail/alter/fail_wind_frwave_init.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        RUPTURE_MOD                   share/modules/rupture_mod.F   
Chd|====================================================================
        SUBROUTINE FAIL_WIND_FRWAVE_INIT(NGROUP)
        USE RUPTURE_MOD
!$COMMENT
!       FAIL_WIND_FRWAVE_INIT description :
!              FAIL_WIND_FRWAVE_INIT allocates and initializes the
!              INDEX_OVER_50_CYCLES array
!       FAIL_WIND_FRWAVE_INIT organization :
!               - INDEX_OVER_50_CYCLES is allocated & initialized
!$ENDCOMMENT

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "comlock.inc"

C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
        INTEGER, INTENT(in) :: NGROUP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        ALLOCATE( INDEX_OVER_50_CYCLES(NGROUP) )
        INDEX_OVER_50_CYCLES(1:NGROUP) = 50      
        RETURN
        END SUBROUTINE FAIL_WIND_FRWAVE_INIT
