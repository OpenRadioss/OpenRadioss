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
Chd|  CBASTRA3PINCH                 source/elements/shell/coqueba/cbastra3pinch.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE CBASTRA3PINCH(
     1                         JFT   ,JLT   ,NPLAT    ,IPLAT    ,
     2                         VDEFPINCH    ,EPINCHXZ ,
     3                         EPINCHYZ     ,EZZ   ,
     3                         DT1C  ,NG    ,EZZPG ,
     4                         EPGPINCHXZ,
     5                         EPGPINCHYZ,
     6                         EPGPINCHZZ)       
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PINCHTYPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,JFT,NPLAT,IPLAT(*),NG
      my_real
     .   VDEFPINCH(MVSIZ,3),DT1C(*),
     .   EPINCHXZ(*),EPINCHYZ(*),EZZ(MVSIZ),EZZPG(MVSIZ,4),
     .   EPGPINCHXZ(MVSIZ), EPGPINCHYZ(MVSIZ), EPGPINCHZZ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,EP
C-----------------------------------------------
#include "vectorize.inc"
      DO I=JFT,JLT 
        EP=IPLAT(I)
        EPINCHXZ(EP) = VDEFPINCH(EP,1) * DT1C(EP)
        EPINCHYZ(EP) = VDEFPINCH(EP,2) * DT1C(EP)
        EZZ(EP)      = VDEFPINCH(EP,3) * DT1C(EP)
        EZZPG(EP,NG) = EZZ(EP)
C       for outputs in TH, strains are accumulated strains
        EPGPINCHXZ(EP) = EPGPINCHXZ(EP) + EPINCHXZ(EP)
        EPGPINCHYZ(EP) = EPGPINCHYZ(EP) + EPINCHYZ(EP)
        EPGPINCHZZ(EP) = EPGPINCHZZ(EP) + EZZPG(EP,NG)
      ENDDO
      RETURN
      END
