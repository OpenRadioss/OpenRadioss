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
Chd|  QMASS2                        source/elements/solid_2d/quad/qmass2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        IDP_FREE                      source/system/machine.F       
Chd|        IDP_LOCK                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE QMASS2(
     1   OFF,     RHO,     MS,      AREA,
     2   NC1,     NC2,     NC3,     NC4,
     3   NEL)
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
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   OFF(*), RHO(*), MS(*),AREA(*)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   MASS(MVSIZ)
      INTEGER I
C-----------------------------------------------
      DO I=1,NEL
       MASS(I)=RHO(I)*AREA(I)*OFF(I)*FOURTH
      ENDDO
C
      CALL IDP_LOCK(1)
      DO I=1,NEL
C
       MS(NC1(I))=MS(NC1(I)) + MASS(I)
       MS(NC2(I))=MS(NC2(I)) + MASS(I)
       MS(NC3(I))=MS(NC3(I)) + MASS(I)
       MS(NC4(I))=MS(NC4(I)) + MASS(I)
      ENDDO
      CALL IDP_FREE(1)
C
      RETURN
      END
