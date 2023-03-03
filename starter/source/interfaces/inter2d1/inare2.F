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
C  _clean      SUBROUTINE INARE2(AREA)
Chd|====================================================================
Chd|  INARE2                        source/interfaces/inter2d1/inare2.F
Chd|-- called by -----------
Chd|        I3STI2                        source/interfaces/inter2d1/i3sti2.F
Chd|        I9STI2                        source/interfaces/int09/i9sti2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INARE2(AREA,YE ,ZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   AREA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   FS2, FS3, FT2, FT3, E, F, G,YE(4) ,ZE(4)
C
      FS2=-YE(1)+YE(2)+YE(3)-YE(4)
      FS3=-ZE(1)+ZE(2)+ZE(3)-ZE(4)
      FT2=-YE(1)-YE(2)+YE(3)+YE(4)
      FT3=-ZE(1)-ZE(2)+ZE(3)+ZE(4)
      E=FS2*FS2+FS3*FS3
      F=FS2*FT2+FS3*FT3
      G=FT2*FT2+FT3*FT3
      AREA=SQRT((E*G-F*F)/SIXTEEN)
      RETURN
      END
