Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  VITESSEPINCH                  source/assembly/vitessepinch.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE VITESSEPINCH(
     1                        APINCH  ,  VPINCH,
     2                        NODFT   ,  NODLT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PINCHTYPE_MOD
C----6------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT
      my_real
     .   APINCH(3,NPINCH),VPINCH(3,NPINCH)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------    
#include      "vectorize.inc"
      DO N=NODFT,NODLT
        VPINCH(1,N)=VPINCH(1,N)+DT12*APINCH(1,N)
        VPINCH(2,N)=VPINCH(2,N)+DT12*APINCH(2,N)
        VPINCH(3,N)=VPINCH(3,N)+DT12*APINCH(3,N)
        APINCH(1,N)=ZERO
        APINCH(2,N)=ZERO
        APINCH(3,N)=ZERO     
      ENDDO
      RETURN
      END
