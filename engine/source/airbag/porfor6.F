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
Chd|  PORFOR6                       source/airbag/porfor6.F       
Chd|-- called by -----------
Chd|        AIRBAGB1                      source/airbag/airbagb1.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PORFOR6(SVTFAC,IM,IPM,PM,STRAIN,P,PEXT,IEL,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPM(NPROPMI,*),NG,IM,IEL,NEL
      my_real
     .   SVTFAC,PM(NPROPM,*),STRAIN(*),P,PEXT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   X0,X1,X2,X3,EPSXX,EPSYY,RS,RP
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C ANAGONYE WANG EFFECTIVE LEAKAGE AREA  FORMULATION
C--------------------------------------------------
      EPSXX = STRAIN(IEL)
      EPSYY = STRAIN(IEL+NEL)
      RS=MAX(ONE+EPSXX+EPSYY+EPSXX*EPSYY,ONE)
      RP=MIN(PEXT/P,ONE)
C
      X0 = PM(164,IM)
      X1 = PM(165,IM)
      X2 = PM(166,IM)
      X3 = PM(167,IM)
      SVTFAC= (X0+X2*RP)/RS+X1+X3*RP
C
      RETURN
      END
Chd|====================================================================
Chd|  PORFORM6                      source/airbag/porfor6.F       
Chd|-- called by -----------
Chd|        FVVENT0                       source/airbag/fvvent0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PORFORM6(SVTFAC,IM,PM,AREA,AREA0,P,PEXT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IM
      my_real
     .   SVTFAC,PM(NPROPM,*),AREA,AREA0,P,PEXT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   X0,X1,X2,X3,RS,RP
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C ANAGONYE WANG EFFECTIVE LEAKAGE AREA  FORMULATION
C--------------------------------------------------
      RS=MAX(AREA/AREA0,ONE)
      RP=MIN(PEXT/P,ONE)
C
      X0 = PM(164,IM)
      X1 = PM(165,IM)
      X2 = PM(166,IM)
      X3 = PM(167,IM)
      SVTFAC= (X0+X2*RP)/RS+X1+X3*RP
C
      RETURN
      END
