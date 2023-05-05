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
Chd|  LECINP                        source/input/lecinp.F         
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        FREFORM                       source/input/freform.F        
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|====================================================================
      SUBROUTINE LECINP(IRUN,IRFL,IRFE,H3D_DATA,FLAG_CST_AMS,DYNAIN_DATA,
     .                  SENSORS )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE STATE_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRUN, IRFL, IRFE,FLAG_CST_AMS
      TYPE(H3D_DATABASE), INTENT(INOUT) :: H3D_DATA
      TYPE(DYNAIN_DATABASE), INTENT(INOUT) :: DYNAIN_DATA
      TYPE(SENSORS_), INTENT(INOUT) :: SENSORS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr02_c.inc"
#include      "scr07_c.inc"
#include      "scr18_c.inc"
#include      "scrnoi_c.inc"
#include      "scrcut_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      CHARACTER CART*ncharline
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NNOISE=0
      NCUTS=0
C
      DO I=1,52
        DTFAC1(I) = ZERO
        DTMIN1(I) = ZERO
        IDTMIN(I) = 0
      ENDDO
      NODADT = 0
C
      IFREEF=1
      CALL FREFORM(IRUN,IRFL,IRFE,H3D_DATA,FLAG_CST_AMS,DYNAIN_DATA,
     .             SENSORS)
C
      REWIND IIN
C
      RETURN
      END
