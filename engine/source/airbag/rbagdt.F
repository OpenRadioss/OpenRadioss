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
Chd|  RBAGDT                        source/airbag/rbagdt.F        
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBAGDT(GEO,IGEO)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*)
C     REAL
      my_real
     .   GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG, ITYP
C     REAL
      my_real
     .   DTX, DTY, GAMA, P, V, S, ROT, VMIN, AL      
C-----------------------------------------------
         VMIN = EM8
      DTX=ONE
      DTY=ONE
      DO100 IG=1,NUMGEO
      ITYP=IGEO(11,IG)
      IF(ITYP/=7) GOTO100
      GAMA=GEO(13,IG)
      P=GEO(27,IG)
      V=GEO(25,IG)
      S=GEO(35,IG)
      ROT=GEO(36,IG)
      IF(V>VMIN)
     *   DTX=MAX(SQRT(ONE+DT2**2*GAMA*P*S/V/ROT),DTX)
      AL=GEO(14,IG)
      DTY=MIN(SQRT(ONE+AL)-AL,DTY)
 100  CONTINUE
C
      DT2=DT2*DTY/DTX
      RETURN
      END
