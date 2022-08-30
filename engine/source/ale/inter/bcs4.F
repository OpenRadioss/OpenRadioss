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
Chd|  BCS4                          source/ale/inter/bcs4.F       
Chd|-- called by -----------
Chd|        I9AVEL                        source/interfaces/int09/i9avel.F
Chd|        IQELA2                        source/ale/inter/iqela2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCS4(J,K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER K
      INTEGER J(3)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(K == 0) RETURN
      
      J(1)=0
      J(2)=0
      J(3)=0

      GO TO (10,20,30,40,50,60,70),K
   10 J(3)=1
      GO TO 200
   20 J(2)=1
      GO TO 200
   30 J(2)=1
      J(3)=1
      GO TO 200
   40 J(1)=1
      GO TO 200
   50 J(1)=1
      J(3)=1
      GO TO 200
   60 J(1)=1
      J(2)=1
      GO TO 200
   70 J(1)=1
      J(2)=1
      J(3)=1
  200 CONTINUE
      RETURN
      END
