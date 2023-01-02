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
Chd|  Q4KEROT2                      source/elements/solid_2d/quad4/q4kerot2.F
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|-- calls ---------------
Chd|        SYSTRAN2                      source/elements/solid_2d/quad4/q4kerot2.F
Chd|====================================================================
      SUBROUTINE Q4KEROT2 (
     1                R22,R32,R23,R33,
     2                K11,K12,K13,K14,K22,
     3                K23,K24,K33,K34,K44)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    R22(*), R23(*), R32(*), R33(*), 
     .    K11(2,2,*), K12(2,2,*), K13(2,2,*), K14(2,2,*), K22(2,2,*), 
     .    K23(2,2,*), K24(2,2,*), K33(2,2,*), K34(2,2,*), K44(2,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ISYM
      my_real
     .   Q(2,2,MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        Q(1,1,I)=R22(I)
        Q(1,2,I)=R23(I)
        Q(2,1,I)=R32(I)
        Q(2,2,I)=R33(I)
      ENDDO
C
      ISYM = 1
      CALL SYSTRAN2(LFT, LLT, Q, K11, ISYM)
      CALL SYSTRAN2(LFT, LLT, Q, K22, ISYM)
      CALL SYSTRAN2(LFT, LLT, Q, K33, ISYM)
      CALL SYSTRAN2(LFT, LLT, Q, K44, ISYM)
      ISYM = 0
      CALL SYSTRAN2(LFT, LLT, Q, K12, ISYM)
      CALL SYSTRAN2(LFT, LLT, Q, K13, ISYM)
      CALL SYSTRAN2(LFT, LLT, Q, K14, ISYM)
      CALL SYSTRAN2(LFT, LLT, Q, K23, ISYM)
      CALL SYSTRAN2(LFT, LLT, Q, K24, ISYM)
      CALL SYSTRAN2(LFT, LLT, Q, K34, ISYM)
C
      RETURN
      END
Chd|====================================================================
Chd|  SYSTRAN2                      source/elements/solid_2d/quad4/q4kerot2.F
Chd|-- called by -----------
Chd|        Q4KEROT2                      source/elements/solid_2d/quad4/q4kerot2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SYSTRAN2(JFT, JLT, VQ, KK, ISYM)
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
      INTEGER JFT, JLT, ISYM
      my_real 
     .   VQ(2,2,*), KK(2,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, EP
      my_real
     .    K(2,2,MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C     IF (ISYM==1) THEN
C       DO EP=JFT,JLT
C         DO I=1,2
C         DO J=I,2
C           K(I,J,EP)=
C    1      VQ(I,1,EP)*(KK(1,1,EP)*VQ(J,1,EP)+KK(1,2,EP)*VQ(J,2,EP)) + 
C    2      VQ(I,2,EP)*(KK(1,2,EP)*VQ(J,1,EP)+KK(2,2,EP)*VQ(J,2,EP))
C         ENDDO
C         ENDDO
C       ENDDO
C       DO EP=JFT,JLT
C         DO I=1,2
C         DO J=I,2
C           KK(I,J,EP) = K(I,J,EP)
C         ENDDO
C         ENDDO
C       ENDDO
C       DO EP=JFT,JLT
C         KK(2,1,EP) = K(1,2,EP)
C       ENDDO
C     ELSE
        DO EP=JFT,JLT 
          DO I=1,2
          DO J=1,2
            K(I,J,EP) = 
     1      VQ(I,1,EP)*(KK(1,1,EP)*VQ(J,1,EP)+KK(1,2,EP)*VQ(J,2,EP)) + 
     2      VQ(I,2,EP)*(KK(2,1,EP)*VQ(J,1,EP)+KK(2,2,EP)*VQ(J,2,EP))                 
          ENDDO
          ENDDO
        ENDDO
        DO EP=JFT,JLT
          DO I=1,2
          DO J=1,2
            KK(I,J,EP) = K(I,J,EP)
          ENDDO
          ENDDO
        ENDDO
C     ENDIF
C
      RETURN
      END
