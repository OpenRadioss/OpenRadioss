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
Chd|  SDERITO3                      source/elements/solid/solide/sderi3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDERITO3(OFF,DET,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   JAC_I, NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL
      my_real
     .   OFF(*),DET(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   JAC_I(10,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   JAC12, JAC45, JAC78
C-----------------------------------------------
      DO I=1,NEL
        JACI1=JAC_I(1,I)
        JACI4=JAC_I(4,I)
        JACI7=JAC_I(7,I)
        JACI2=JAC_I(2,I)
        JACI5=JAC_I(5,I)
        JACI8=JAC_I(8,I)
        JACI3=JAC_I(3,I)
        JACI6=JAC_I(6,I)
        JACI9=JAC_I(9,I)
        DET(I)=JAC_I(10,I)
C      
        JAC12=JACI1+JACI2
        JAC45=JACI4+JACI5
        JAC78=JACI7+JACI8

        PX1(I)=-JAC12-JACI3
        PY1(I)=-JAC45-JACI6
        PZ1(I)=-JAC78-JACI9
C
        PX2(I)=-JAC12+JACI3
        PY2(I)=-JAC45+JACI6
        PZ2(I)=-JAC78+JACI9
C
        JAC12=JACI1-JACI2
        JAC45=JACI4-JACI5
        JAC78=JACI7-JACI8

        PX3(I)= JAC12+JACI3
        PY3(I)= JAC45+JACI6
        PZ3(I)= JAC78+JACI9
C
        PX4(I)= JAC12-JACI3
        PY4(I)= JAC45-JACI6
        PZ4(I)= JAC78-JACI9


      ENDDO
      RETURN
C
      END
