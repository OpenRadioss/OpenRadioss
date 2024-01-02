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
Chd|  SLDEGE                        source/elements/solid/solide/sldege.F
Chd|-- called by -----------
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|-- calls ---------------
Chd|        DEGES4V                       source/elements/solid/solide/deges4v.F
Chd|        IDEGE                         source/elements/solid/solide/idege.F
Chd|====================================================================
      SUBROUTINE SLDEGE(
     1   X1,      X2,      X3,      X4,
     2   X5,      X6,      X7,      X8,
     3   Y1,      Y2,      Y3,      Y4,
     4   Y5,      Y6,      Y7,      Y8,
     5   Z1,      Z2,      Z3,      Z4,
     6   Z5,      Z6,      Z7,      Z8,
     7   AREA,    AREAM,   VOLG,    NEL)
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
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   AREA(MVSIZ,6),AREAM(*),VOLG(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDEG(MVSIZ),J,IDET4(MVSIZ),IT(MVSIZ)
      INTEGER :: N_INDX
      INTEGER, DIMENSION(MVSIZ) :: INDX
      my_real FAC(MVSIZ),V_G
C-----------------------------------------------
      IDEG(1:MVSIZ)=0
      DO J=1,6
       DO I=1,NEL
        IF(AREA(I,J)<EM30) IDEG(I)=IDEG(I)+1
       ENDDO
      ENDDO
C
      N_INDX = 0
      DO I=1,NEL
C-------due to the fact that AREA_Max*L is far from V for Dege---
       IF(IDEG(I) > 0) THEN
                AREAM(I) =EM20
C----tetra 4  ,pyrami    
                IF (IDEG(I)>=2) THEN
                 FAC(I)=ONE_OVER_9
                ELSE
                 FAC(I)=FOURTH
                END IF
                N_INDX = N_INDX + 1
                INDX(N_INDX) = I
        ENDIF
      ENDDO
      IDET4(1:MVSIZ) = 1
      IT(1:MVSIZ) = 0
      IF(N_INDX>0) THEN
        CALL IDEGE(X1,X2,X3,X4,Y1,Y2,Y3,Y4,
     .             Z1,Z2,Z3,Z4,AREA(1,1),AREAM,FAC,IDET4,IT,INDX,N_INDX)
        CALL IDEGE(X5,X6,X7,X8,Y5,Y6,Y7,Y8,
     .             Z5,Z6,Z7,Z8,AREA(1,2),AREAM,FAC,IDET4,IT,INDX,N_INDX)
        CALL IDEGE(X1,X2,X6,X5,Y1,Y2,Y6,Y5,
     .             Z1,Z2,Z6,Z5,AREA(1,3),AREAM,FAC,IDET4,IT,INDX,N_INDX)
        CALL IDEGE(X2,X3,X7,X6,Y2,Y3,Y7,Y6,
     .             Z2,Z3,Z7,Z6,AREA(1,4),AREAM,FAC,IDET4,IT,INDX,N_INDX)
        CALL IDEGE(X3,X4,X8,X7,Y3,Y4,Y8,Y7,
     .             Z3,Z4,Z8,Z7,AREA(1,5),AREAM,FAC,IDET4,IT,INDX,N_INDX)
        CALL IDEGE(X4,X1,X5,X8,Y4,Y1,Y5,Y8,
     .             Z4,Z1,Z5,Z8,AREA(1,6),AREAM,FAC,IDET4,IT,INDX,N_INDX)
        
#include      "vectorize.inc"
          DO J=1,N_INDX
                I = INDX(J)
C--------suposse here V=0.5*A_max*L for penta  =0.333A_max*L for Pyram 
                IF (IT(I) ==0  ) AREAM(I)=FAC(I)*AREAM(I)
C--------add special treat for tetra4, as V is not right values
                IF (IDET4(I) ==1 ) THEN
                        CALL DEGES4V(V_G,
     .                  X1(I), X2(I), X3(I), X4(I), X5(I), X6(I), X7(I), X8(I),
     .                  Y1(I), Y2(I), Y3(I), Y4(I), Y5(I), Y6(I), Y7(I), Y8(I),
     .                  Z1(I), Z2(I), Z3(I), Z4(I), Z5(I), Z6(I), Z7(I), Z8(I))
                          FAC(I)=THIRD*VOLG(I)/V_G
                        AREAM(I)=FAC(I)*FAC(I)*AREAM(I)
                  END IF
        ENDDO
      ENDIF
C
      RETURN
      END
