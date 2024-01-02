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
Chd|  IN12R                         source/interfaces/interf/in12r.F
Chd|-- called by -----------
Chd|        INT12W                        source/ale/inter/int12w.F     
Chd|        INTTI12F                      source/interfaces/interf/intti12.F
Chd|        INTTI12V                      source/interfaces/interf/intti12.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IN12R(A,FRIGAP,NSV,NSN,FLAG,TAGKINE )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,FLAG,TAGKINE(*)
      INTEGER NSV(*)
      my_real
     .   A(3,*), FRIGAP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I
      my_real
     .  XX,YY,ZZ,
     .  S11,S21,S31,S12,S22,S32,S13,S23,S33
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C TRANSFORMATION TEMPORAIRE DANS LE REPERE MAIN
        S11=FRIGAP(5)
        S21=FRIGAP(6)
        S31=FRIGAP(7)
        S12=FRIGAP(8)
        S22=FRIGAP(9)
        S32=FRIGAP(10)
        S13=FRIGAP(12)
        S23=FRIGAP(13)
        S33=FRIGAP(14)
       IF(FLAG==1)THEN
        DO II=1,NSN
         IF(TAGKINE(II)>0)THEN
           I=NSV(II)
           XX=A(1,I)
           YY=A(2,I)
           ZZ=A(3,I)
           A(1,I)=S11*XX+S12*YY+S13*ZZ
           A(2,I)=S21*XX+S22*YY+S23*ZZ
           A(3,I)=S31*XX+S32*YY+S33*ZZ
         ENDIF
        ENDDO
       ELSE
C RETOUR REPERE SECOND
        DO II=1,NSN
         IF(TAGKINE(II)>0)THEN
           I=NSV(II)
           XX=A(1,I)
           YY=A(2,I)
           ZZ=A(3,I)
           A(1,I)=S11*XX+S21*YY+S31*ZZ
           A(2,I)=S12*XX+S22*YY+S32*ZZ
           A(3,I)=S13*XX+S23*YY+S33*ZZ
         ENDIF
        ENDDO
       ENDIF

      RETURN
      END
