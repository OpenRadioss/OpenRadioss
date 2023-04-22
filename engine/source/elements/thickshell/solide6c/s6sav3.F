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
Chd|  S6SAV3                        source/elements/thickshell/solide6c/s6sav3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6SAV3(
     1   OFFG,    SAV,     XD1,     XD2,
     2   XD3,     XD4,     XD5,     XD6,
     3   YD1,     YD2,     YD3,     YD4,
     4   YD5,     YD6,     ZD1,     ZD2,
     5   ZD3,     ZD4,     ZD5,     ZD6,
     6   NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   OFFG(*), SAV(NEL,15)
      
      my_real 
     .   XD1(*), XD2(*), XD3(*), XD4(*), XD5(*), XD6(*),  
     .   YD1(*), YD2(*), YD3(*), YD4(*), YD5(*), YD6(*),  
     .   ZD1(*), ZD2(*), ZD3(*), ZD4(*), ZD5(*), ZD6(*)      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I      
C=======================================================================
        DO I=1,NEL
          IF(ABS(OFFG(I)) <= ONE )THEN
           SAV(I,1)=XD1(I)-XD6(I)
           SAV(I,2)=YD1(I)-YD6(I)
           SAV(I,3)=ZD1(I)-ZD6(I)
           SAV(I,4)=XD2(I)-XD6(I)
           SAV(I,5)=YD2(I)-YD6(I)
           SAV(I,6)=ZD2(I)-ZD6(I)
           SAV(I,7)=XD3(I)-XD6(I)
           SAV(I,8)=YD3(I)-YD6(I)
           SAV(I,9)=ZD3(I)-ZD6(I)
           SAV(I,10)=XD4(I)-XD6(I)
           SAV(I,11)=YD4(I)-YD6(I)
           SAV(I,12)=ZD4(I)-ZD6(I)
           SAV(I,13)=XD5(I)-XD6(I)
           SAV(I,14)=YD5(I)-YD6(I)
           SAV(I,15)=ZD5(I)-ZD6(I)
          ENDIF
        ENDDO
C-----------
      RETURN
      END
