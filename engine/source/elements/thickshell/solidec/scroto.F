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
Chd|  SCROTO                        source/elements/thickshell/solidec/scroto.F
Chd|-- called by -----------
Chd|        SCORDEF3                      source/elements/thickshell/solidec/scordef3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCROTO(NEL,SIG,SIGN,DIR)
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
C
      my_real
     .   SIG(MVSIZ,6), SIGN(MVSIZ,6), DIR(MVSIZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      my_real
     .   X2,Y2,XY,XYS2
C-----------------------------------------------
      DO I=1,NEL
         X2=DIR(I,1)*DIR(I,1)
         Y2=DIR(I,2)*DIR(I,2)
         XY=DIR(I,1)*DIR(I,2)
         XYS2=TWO*XY*SIG(I,4)
         SIGN(I,1) = X2*SIG(I,1)+Y2*SIG(I,2)-XYS2
         SIGN(I,2) = Y2*SIG(I,1)+X2*SIG(I,2)+XYS2
         SIGN(I,3) = SIG(I,3)
         SIGN(I,4) =(SIG(I,1)-SIG(I,2))*XY+(X2-Y2)*SIG(I,4)
         SIGN(I,5) = DIR(I,2)*SIG(I,6)+DIR(I,1)*SIG(I,5)
         SIGN(I,6) = DIR(I,1)*SIG(I,6)-DIR(I,2)*SIG(I,5)
      ENDDO
C-----------
      RETURN
      END
