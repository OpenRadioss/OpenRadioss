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
Chd|  ACCELEPINCH                   source/assembly/accelepinch.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE ACCELEPINCH(
     1                         APINCH,      MS,  MSPINCH,
     2                      STIFPINCH,   NODFT,    NODLT,
     3                          DTNOD,   DTFAC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PINCHTYPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT  
      my_real APINCH(3,*),MS(*),MSPINCH(*),STIFPINCH(*)
      my_real DTNOD,DTFAC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N
      my_real RTMP,MTEMPPINCH,FACTOR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------    
#include      "vectorize.inc"
      DO N=NODFT,NODLT
        IF(MS(N)>ZERO) THEN
          FACTOR = (DTNOD**2) * HALF / (DTFAC**2)
          MSPINCH(N) = MAX(MSPINCH(N), STIFPINCH(N)*FACTOR) 
          RTMP = ONE / MSPINCH(N)
C       
          APINCH(1,N) = APINCH(1,N) * RTMP
          APINCH(2,N) = APINCH(2,N) * RTMP
          APINCH(3,N) = APINCH(3,N) * RTMP
        ELSE
          APINCH(1,N) = ZERO
          APINCH(2,N) = ZERO
          APINCH(3,N) = ZERO
        ENDIF
        STIFPINCH(N) = ZERO          
      ENDDO
      RETURN
      END
