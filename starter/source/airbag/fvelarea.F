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
Chd|  FVELAREA                      source/airbag/fvelarea.F      
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- calls ---------------
Chd|        FVNORMAL                      source/airbag/fvmbag1.F       
Chd|====================================================================
      SUBROUTINE FVELAREA(IBUF,   ELEM,      X,    NEL,
     .                    ELAREA )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C#include      "param_c.inc"
C#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUF(*), ELEM(3,*)
      INTEGER NEL
      my_real
     .        X(3,NUMNOD), ELAREA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEL, N1, N2, N3, NN1, NN2, NN3
      my_real
     .        NRX, NRY, NRZ, AREA2

C--------------------------------------------------
      DO IEL=1,NEL
         N1=ELEM(1,IEL)
         N2=ELEM(2,IEL)
         N3=ELEM(3,IEL)
         NN1=IBUF(N1)
         NN2=IBUF(N2)
         NN3=IBUF(N3)
         CALL FVNORMAL(X,NN1,NN2,NN3,0,NRX,NRY,NRZ)
         AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
         ELAREA(IEL)=HALF*AREA2
      ENDDO
C     
      RETURN
      END
