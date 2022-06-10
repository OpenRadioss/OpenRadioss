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
Chd|  TGRAD3                        source/ale/ale3d/tgrad3.F     
Chd|-- called by -----------
Chd|        AGRAD0                        source/ale/agrad0.F           
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TGRAD3(GRD,GRAD,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NEL
C     REAL
      my_real
     .   GRD(NEL,6), GRAD(6,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO 70 I=LFT,LLT
      GRAD(1,I)= GRD(I,1)
      GRAD(2,I)= GRD(I,2)
      GRAD(3,I)= GRD(I,3)
      GRAD(4,I)= GRD(I,4)
      GRAD(5,I)= GRD(I,5)
      GRAD(6,I)= GRD(I,6)
   70 CONTINUE
C
      RETURN
      END
