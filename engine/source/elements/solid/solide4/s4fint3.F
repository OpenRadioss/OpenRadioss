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
Chd|  S4FINT3                       source/elements/solid/solide4/s4fint3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4FINT3(SIG,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   VOL,QVIS,NEL)
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
      INTEGER NEL
C     REAL
      my_real
     .   SIG(NEL,6),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   VOL(*),QVIS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "nsvis_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   S1, S2, S3,
     .   S4, S5, S6,
     .   FINT
C-----------------------------------------------
      DO I=LFT,LLT
        S1=(SIG(I,1)+SVIS(I,1)-QVIS(I))*VOL(I)
        S2=(SIG(I,2)+SVIS(I,2)-QVIS(I))*VOL(I)
        S3=(SIG(I,3)+SVIS(I,3)-QVIS(I))*VOL(I)
        S4=(SIG(I,4)+SVIS(I,4))*VOL(I)
        S5=(SIG(I,5)+SVIS(I,5))*VOL(I)
        S6=(SIG(I,6)+SVIS(I,6))*VOL(I)
C
        F11(I)=F11(I)-(S1*PX1(I)+S4*PY1(I)+S6*PZ1(I))
        F21(I)=F21(I)-(S2*PY1(I)+S4*PX1(I)+S5*PZ1(I))
        F31(I)=F31(I)-(S3*PZ1(I)+S6*PX1(I)+S5*PY1(I))
        F12(I)=F12(I)-(S1*PX2(I)+S4*PY2(I)+S6*PZ2(I))
        F22(I)=F22(I)-(S2*PY2(I)+S4*PX2(I)+S5*PZ2(I))
        F32(I)=F32(I)-(S3*PZ2(I)+S6*PX2(I)+S5*PY2(I))
        F13(I)=F13(I)-(S1*PX3(I)+S4*PY3(I)+S6*PZ3(I))
        F23(I)=F23(I)-(S2*PY3(I)+S4*PX3(I)+S5*PZ3(I))
        F33(I)=F33(I)-(S3*PZ3(I)+S6*PX3(I)+S5*PY3(I))
        F14(I)=F14(I)-(S1*PX4(I)+S4*PY4(I)+S6*PZ4(I))
        F24(I)=F24(I)-(S2*PY4(I)+S4*PX4(I)+S5*PZ4(I))
        F34(I)=F34(I)-(S3*PZ4(I)+S6*PX4(I)+S5*PY4(I))
      ENDDO
C
      RETURN
      END
