Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  ASSPART                       source/assembly/asspart.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE ASSPART(
     2   PARTFT,PARTLT,PARTSAV,GREFT,GRELT,GRESAV)
C----6---------------------------------------------------------------7---------8
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PARTSAV(*),GRESAV(*)
      INTEGER PARTFT,PARTLT,GREFT,GRELT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,KM,NUM7
C-----------------------------------------------
      NUM7 = NPSAV*NPART
C
      KM = 0
      DO 950 K=1,NTHREAD-1
          KM = KM + NUM7
#include "vectorize.inc"
          DO 940 I=PARTFT,PARTLT
           PARTSAV(I)    = PARTSAV(I) + PARTSAV(I+KM)
           PARTSAV(I+KM) = ZERO
 940      CONTINUE
 950  CONTINUE
C
      CALL MY_BARRIER
C
      NUM7 = NPSAV*NGPE
C
      KM = 0
      IF (NTHPART > 0) THEN
        DO 970 K=1,NTHREAD-1
          KM = KM + NUM7
#include "vectorize.inc"
          DO 960 I=GREFT,GRELT
           GRESAV(I)    = GRESAV(I) + GRESAV(I+KM)
           GRESAV(I+KM) = ZERO
 960      CONTINUE
 970    CONTINUE
      ENDIF
      CALL MY_BARRIER
C
      RETURN
      END
