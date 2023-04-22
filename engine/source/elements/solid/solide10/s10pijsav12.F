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
Chd|  S10PIJSAV12                   source/elements/solid/solide10/s10pijsav12.F
Chd|-- called by -----------
Chd|        S10UPD11T12                   source/elements/solid/solide10/s10upd11t12.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10PIJSAV12(PX,PY,PZ, PIJ,SIG,SIGL,INDEX,NB,NEL)
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
      INTEGER NEL,NB,INDEX(*)
      my_real
     .   PX(MVSIZ,10),PY(MVSIZ,10),PZ(MVSIZ,10),
     .   PIJ(NEL,3,10),SIG(NEL,6),SIGL(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C-----------------------------------------------
#include "vectorize.inc"
      DO J=1,NB
        I = INDEX(J)
        PIJ(I,1,1:10) = PX(J,1:10) 
        PIJ(I,2,1:10) = PY(J,1:10) 
        PIJ(I,3,1:10) = PZ(J,1:10) 
        SIGL(I,1:6) = SIG(I,1:6)
      END DO
C-----------
      RETURN
      END
