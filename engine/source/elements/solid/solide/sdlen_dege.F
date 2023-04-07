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
Chd|  SDLEN_DEGE                    source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SDLEN8                        source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        DEGENES8                      source/elements/solid/solide/sdlen3.F
Chd|        DEGES4V                       source/elements/solid/solide/sdlen3.F
Chd|        IDEGE8                        source/elements/solid/solide/sdlen3.F
Chd|====================================================================
      SUBROUTINE SDLEN_DEGE(
     1   VOLG,    LAT,     X1,      X2,
     2   X3,      X4,      X5,      X6,
     3   X7,      X8,      Y1,      Y2,
     4   Y3,      Y4,      Y5,      Y6,
     5   Y7,      Y8,      Z1,      Z2,
     6   Z3,      Z4,      Z5,      Z6,
     7   Z7,      Z8,      IXS,     IDEGE,
     8   NEL)
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
      INTEGER IXS(NIXS,*), IDEGE(*)
      my_real
     .   VOLG(*),LAT(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----------DELTAX crrection for degenerated element
      INTEGER I,IT
      my_real
     .   AREAM(MVSIZ), ATAX,FAC,V_G
C-----------------------------------------------
      CALL DEGENES8(
     1   IXS,     IDEGE,   NEL)
C
      DO I=1,NEL
C-------due to the fact that AREA_Max*L is far from V for Dege---
       IF(IDEGE(I) > 0) THEN
        AREAM(I) =EM20
C----tetra 4  ,pyrami     
        IF (IDEGE(I)>2) THEN
         FAC=ONE_OVER_9
        ELSEIF (IDEGE(I)>1) THEN
         FAC=FOURTH
        ELSE
         FAC=ONE
        END IF
		IT = 0
        CALL IDEGE8(X1(I),X2(I),X3(I),X4(I),Y1(I),Y2(I),Y3(I),Y4(I),
     .             Z1(I),Z2(I),Z3(I),Z4(I),AREAM(I),FAC,IT)
        CALL IDEGE8(X5(I),X6(I),X7(I),X8(I),Y5(I),Y6(I),Y7(I),Y8(I),
     .             Z5(I),Z6(I),Z7(I),Z8(I),AREAM(I),FAC,IT)
        CALL IDEGE8(X1(I),X2(I),X6(I),X5(I),Y1(I),Y2(I),Y6(I),Y5(I),
     .             Z1(I),Z2(I),Z6(I),Z5(I),AREAM(I),FAC,IT)
        CALL IDEGE8(X2(I),X3(I),X7(I),X6(I),Y2(I),Y3(I),Y7(I),Y6(I),
     .             Z2(I),Z3(I),Z7(I),Z6(I),AREAM(I),FAC,IT)
        CALL IDEGE8(X3(I),X4(I),X8(I),X7(I),Y3(I),Y4(I),Y8(I),Y7(I),
     .             Z3(I),Z4(I),Z8(I),Z7(I),AREAM(I),FAC,IT)
        CALL IDEGE8(X4(I),X1(I),X5(I),X8(I),Y4(I),Y1(I),Y5(I),Y8(I),
     .             Z4(I),Z1(I),Z5(I),Z8(I),AREAM(I),FAC,IT)
C----tetra 4       
        IF (IT ==0  ) AREAM(I)=FAC*AREAM(I)
        IF (IDEGE(I)>3 ) THEN
		 AREAM(I)=AREAM(I)*FAC
         CALL DEGES4V(V_G,
     .   X1(I), X2(I), X3(I), X4(I), X5(I), X6(I), X7(I), X8(I),
     .   Y1(I), Y2(I), Y3(I), Y4(I), Y5(I), Y6(I), Y7(I), Y8(I),
     .   Z1(I), Z2(I), Z3(I), Z4(I), Z5(I), Z6(I), Z7(I), Z8(I))
		ELSE
		 V_G=VOLG(I)
		END IF
        LAT(I) = FOUR*V_G/SQRT(AREAM(I))

       ENDIF
      ENDDO
C      
      RETURN
      END
