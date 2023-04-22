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
Chd|  S8SAVSR3                      source/elements/solid/solide/s8savsr3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SAVSR3(
     1   OFFG,    SAV,     XD,      YD,
     2   ZD,      NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   XD(MVSIZ,8),YD(MVSIZ,8),ZD(MVSIZ,8), SAV(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I      
C=======================================================================
        DO I=1,NEL
          IF(ABS(OFFG(I)) <= ONE )THEN
           SAV(I,1)=XD(I,1)-XD(I,8)
           SAV(I,2)=YD(I,1)-YD(I,8)
           SAV(I,3)=ZD(I,1)-ZD(I,8)
           SAV(I,4)=XD(I,2)-XD(I,8)
           SAV(I,5)=YD(I,2)-YD(I,8)
           SAV(I,6)=ZD(I,2)-ZD(I,8)
           SAV(I,7)=XD(I,3)-XD(I,8)
           SAV(I,8)=YD(I,3)-YD(I,8)
           SAV(I,9)=ZD(I,3)-ZD(I,8)
           SAV(I,10)=XD(I,4)-XD(I,8)
           SAV(I,11)=YD(I,4)-YD(I,8)
           SAV(I,12)=ZD(I,4)-ZD(I,8)
           SAV(I,13)=XD(I,5)-XD(I,8)
           SAV(I,14)=YD(I,5)-YD(I,8)
           SAV(I,15)=ZD(I,5)-ZD(I,8)
           SAV(I,16)=XD(I,6)-XD(I,8)
           SAV(I,17)=YD(I,6)-YD(I,8)
           SAV(I,18)=ZD(I,6)-ZD(I,8)
           SAV(I,19)=XD(I,7)-XD(I,8)
           SAV(I,20)=YD(I,7)-YD(I,8)
           SAV(I,21)=ZD(I,7)-ZD(I,8)
          ENDIF
        ENDDO
C-----------
      RETURN
      END
