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
Chd|  BCS1V                         source/constraints/general/bcs/bcs1.F
Chd|-- called by -----------
Chd|        BCS10                         source/constraints/general/bcs/bcs10.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCS1V(NINDX,INDX,ISKEW,ICODT,A,
     .                 SKEW ,V                 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDX, INDX(NINDX), ISKEW(SISKEW), ICODT(SICODT)
      my_real A(3,NUMNOD), V(3,NUMNOD), SKEW(LSKEW,SSKEW/LSKEW)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, K, L, ISK, LCOD
      my_real AA, VV
C-----------------------------------------------
#include "vectorize.inc"
      DO K = 1, NINDX
        L = INDX(K)
        ISK =ISKEW(L)
        LCOD=ICODT(L)
        IF(ISK==1) THEN
C------------------
C     GLOBAL SYSTEM
C------------------
          IF(LCOD==1)THEN
            V(3,L)=ZERO
            A(3,L)=ZERO
          ELSEIF(LCOD==2)THEN
            V(2,L)=ZERO
            A(2,L)=ZERO
          ELSEIF(LCOD==3)THEN
            V(2,L)=ZERO
            V(3,L)=ZERO
            A(2,L)=ZERO
            A(3,L)=ZERO
          ELSEIF(LCOD==4)THEN
            V(1,L)=ZERO
            A(1,L)=ZERO
          ELSEIF(LCOD==5)THEN
            V(1,L)=ZERO
            V(3,L)=ZERO
            A(1,L)=ZERO
            A(3,L)=ZERO
          ELSEIF(LCOD==6)THEN
            V(1,L)=ZERO
            V(2,L)=ZERO
            A(1,L)=ZERO
            A(2,L)=ZERO
          ELSEIF(LCOD==7)THEN
            V(1,L)=ZERO
            V(2,L)=ZERO
            V(3,L)=ZERO
            A(1,L)=ZERO
            A(2,L)=ZERO
            A(3,L)=ZERO
          ENDIF
C
        ELSE
C-------------------
C     USER SYSTEM
C-------------------
          IF(LCOD==1)THEN
            AA =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
            VV =SKEW(7,ISK)*V(1,L)+SKEW(8,ISK)*V(2,L)+SKEW(9,ISK)*V(3,L)
            A(1,L)=A(1,L)-SKEW(7,ISK)*AA
            A(2,L)=A(2,L)-SKEW(8,ISK)*AA
            A(3,L)=A(3,L)-SKEW(9,ISK)*AA
            V(1,L)=V(1,L)-SKEW(7,ISK)*VV
            V(2,L)=V(2,L)-SKEW(8,ISK)*VV
            V(3,L)=V(3,L)-SKEW(9,ISK)*VV
          ELSEIF(LCOD==2)THEN
            AA =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
            VV =SKEW(4,ISK)*V(1,L)+SKEW(5,ISK)*V(2,L)+SKEW(6,ISK)*V(3,L)
            A(1,L)=A(1,L)-SKEW(4,ISK)*AA
            A(2,L)=A(2,L)-SKEW(5,ISK)*AA
            A(3,L)=A(3,L)-SKEW(6,ISK)*AA
            V(1,L)=V(1,L)-SKEW(4,ISK)*VV
            V(2,L)=V(2,L)-SKEW(5,ISK)*VV
            V(3,L)=V(3,L)-SKEW(6,ISK)*VV
          ELSEIF(LCOD==3)THEN
            AA =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
            VV =SKEW(7,ISK)*V(1,L)+SKEW(8,ISK)*V(2,L)+SKEW(9,ISK)*V(3,L)
            A(1,L)=A(1,L)-SKEW(7,ISK)*AA
            A(2,L)=A(2,L)-SKEW(8,ISK)*AA
            A(3,L)=A(3,L)-SKEW(9,ISK)*AA
            V(1,L)=V(1,L)-SKEW(7,ISK)*VV
            V(2,L)=V(2,L)-SKEW(8,ISK)*VV
            V(3,L)=V(3,L)-SKEW(9,ISK)*VV
            AA =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
            VV =SKEW(4,ISK)*V(1,L)+SKEW(5,ISK)*V(2,L)+SKEW(6,ISK)*V(3,L)
            A(1,L)=A(1,L)-SKEW(4,ISK)*AA
            A(2,L)=A(2,L)-SKEW(5,ISK)*AA
            A(3,L)=A(3,L)-SKEW(6,ISK)*AA
            V(1,L)=V(1,L)-SKEW(4,ISK)*VV
            V(2,L)=V(2,L)-SKEW(5,ISK)*VV
            V(3,L)=V(3,L)-SKEW(6,ISK)*VV
          ELSEIF(LCOD==4)THEN
            AA =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
            VV =SKEW(1,ISK)*V(1,L)+SKEW(2,ISK)*V(2,L)+SKEW(3,ISK)*V(3,L)
            A(1,L)=A(1,L)-SKEW(1,ISK)*AA
            A(2,L)=A(2,L)-SKEW(2,ISK)*AA
            A(3,L)=A(3,L)-SKEW(3,ISK)*AA
            V(1,L)=V(1,L)-SKEW(1,ISK)*VV
            V(2,L)=V(2,L)-SKEW(2,ISK)*VV
            V(3,L)=V(3,L)-SKEW(3,ISK)*VV
          ELSEIF(LCOD==5)THEN
            AA =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
            VV =SKEW(7,ISK)*V(1,L)+SKEW(8,ISK)*V(2,L)+SKEW(9,ISK)*V(3,L)
            A(1,L)=A(1,L)-SKEW(7,ISK)*AA
            A(2,L)=A(2,L)-SKEW(8,ISK)*AA
            A(3,L)=A(3,L)-SKEW(9,ISK)*AA
            V(1,L)=V(1,L)-SKEW(7,ISK)*VV
            V(2,L)=V(2,L)-SKEW(8,ISK)*VV
            V(3,L)=V(3,L)-SKEW(9,ISK)*VV
            AA =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
            VV =SKEW(1,ISK)*V(1,L)+SKEW(2,ISK)*V(2,L)+SKEW(3,ISK)*V(3,L)
            A(1,L)=A(1,L)-SKEW(1,ISK)*AA
            A(2,L)=A(2,L)-SKEW(2,ISK)*AA
            A(3,L)=A(3,L)-SKEW(3,ISK)*AA
            V(1,L)=V(1,L)-SKEW(1,ISK)*VV
            V(2,L)=V(2,L)-SKEW(2,ISK)*VV
            V(3,L)=V(3,L)-SKEW(3,ISK)*VV
          ELSEIF(LCOD==6)THEN
            AA =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
            VV =SKEW(1,ISK)*V(1,L)+SKEW(2,ISK)*V(2,L)+SKEW(3,ISK)*V(3,L)
            A(1,L)=A(1,L)-SKEW(1,ISK)*AA
            A(2,L)=A(2,L)-SKEW(2,ISK)*AA
            A(3,L)=A(3,L)-SKEW(3,ISK)*AA
            V(1,L)=V(1,L)-SKEW(1,ISK)*VV
            V(2,L)=V(2,L)-SKEW(2,ISK)*VV
            V(3,L)=V(3,L)-SKEW(3,ISK)*VV
            AA =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
            VV =SKEW(4,ISK)*V(1,L)+SKEW(5,ISK)*V(2,L)+SKEW(6,ISK)*V(3,L)
            A(1,L)=A(1,L)-SKEW(4,ISK)*AA
            A(2,L)=A(2,L)-SKEW(5,ISK)*AA
            A(3,L)=A(3,L)-SKEW(6,ISK)*AA
            V(1,L)=V(1,L)-SKEW(4,ISK)*VV
            V(2,L)=V(2,L)-SKEW(5,ISK)*VV
            V(3,L)=V(3,L)-SKEW(6,ISK)*VV
          ELSEIF(LCOD==7)THEN
            A(1,L)=ZERO
            A(2,L)=ZERO
            A(3,L)=ZERO
            V(1,L)=ZERO
            V(2,L)=ZERO
            V(3,L)=ZERO
          ENDIF
C
        END IF
C
      ENDDO
C
      RETURN
      END
