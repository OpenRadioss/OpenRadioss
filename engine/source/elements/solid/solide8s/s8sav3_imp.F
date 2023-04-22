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
Chd|  S8SAV3_IMP                    source/elements/solid/solide8s/s8sav3_imp.F
Chd|-- called by -----------
Chd|        SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SAV3_IMP(
     1   OFFG,    SAV,     XD1,     XD2,
     2   XD3,     XD4,     XD5,     XD6,
     3   XD7,     XD8,     YD1,     YD2,
     4   YD3,     YD4,     YD5,     YD6,
     5   YD7,     YD8,     ZD1,     ZD2,
     6   ZD3,     ZD4,     ZD5,     ZD6,
     7   ZD7,     ZD8,     NEL)
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
     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   XD1(*), XD2(*), XD3(*), XD4(*), XD5(*), XD6(*), XD7(*), XD8(*), 
     .   YD1(*), YD2(*), YD3(*), YD4(*), YD5(*), YD6(*), YD7(*), YD8(*), 
     .   ZD1(*), ZD2(*), ZD3(*), ZD4(*), ZD5(*), ZD6(*), ZD7(*), ZD8(*),      
     .   SAV(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I      
C=======================================================================
        !print*,'OFFG(10) ', offg(10)
        DO I=1,NEL
          IF(ABS(OFFG(I)) <= ONE )THEN
           SAV(I,1)=XD2(I)-XD1(I)
           SAV(I,2)=YD2(I)-YD1(I)
           SAV(I,3)=ZD2(I)-ZD1(I)
           SAV(I,4)=XD3(I)-XD1(I)
           SAV(I,5)=YD3(I)-YD1(I)
           SAV(I,6)=ZD3(I)-ZD1(I)
           SAV(I,7)=XD4(I)-XD1(I)
           SAV(I,8)=YD4(I)-YD1(I)
           SAV(I,9)=ZD4(I)-ZD1(I)
           SAV(I,10)=XD5(I)-XD1(I)
           SAV(I,11)=YD5(I)-YD1(I)
           SAV(I,12)=ZD5(I)-ZD1(I)
           SAV(I,13)=XD6(I)-XD1(I)
           SAV(I,14)=YD6(I)-YD1(I)
           SAV(I,15)=ZD6(I)-ZD1(I)
           SAV(I,16)=XD7(I)-XD1(I)
           SAV(I,17)=YD7(I)-YD1(I)
           SAV(I,18)=ZD7(I)-ZD1(I)
           SAV(I,19)=XD8(I)-XD1(I)
           SAV(I,20)=YD8(I)-YD1(I)
           SAV(I,21)=ZD8(I)-ZD1(I)
          ENDIF
        ENDDO
C-----------
      RETURN
      END
