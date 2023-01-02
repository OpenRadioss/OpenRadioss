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
C
Chd|====================================================================
Chd|  CCOLI3                        source/elements/shell/coque/ccoli3.F
Chd|-- called by -----------
Chd|        C3VEOK3                       source/elements/sh3n/coque3n/c3veok3.F
Chd|        CVEOK3                        source/elements/shell/coque/cveok3.F
Chd|        SVEOK3                        source/elements/solid/solide/sveok3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CCOLI3(IXI,NVCI,LFT,LLT)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NVCI, LFT, LLT
      INTEGER IXI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I2, I, J1, II, J, IJ
      I2=LLT-1
      NVCI=0
      DO 20 I=LFT,I2
      IF(NVCI/=0) GO TO 20
      J1=I+1
      II=IXI(I)
      DO 10 J=J1,LLT
      IJ=IXI(J)
      IF(IJ/=II) GO TO 10
      NVCI=1
   10 CONTINUE
   20 CONTINUE
C
      RETURN
      END
