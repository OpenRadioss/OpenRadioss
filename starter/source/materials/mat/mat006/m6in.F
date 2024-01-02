Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  M6IN                          source/materials/mat/mat006/m6in.F
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M6IN(PM,MAT,SIG,RHO,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*),NEL
      my_real
     .   PM(NPROPM,*),RHO(*),SIG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MA
      my_real
     .   C0,C1,C4,C5,E0,P,AMU
C=======================================================================
      DO I=LFT,LLT
        MA=MAT(I)
        C0=PM(31,MA)
        C1=PM(32,MA)
        C4=PM(35,MA)
        C5=PM(36,MA)
        E0=PM(23,MA)
        P=-(SIG(I,1)+SIG(I,2)+SIG(I,3))*THIRD
        AMU=(P-C0-C4*E0)/(C1+C5*E0)
        RHO(I)=RHO(I)*(ONE + AMU)
      ENDDO
C-----------
      RETURN
      END
