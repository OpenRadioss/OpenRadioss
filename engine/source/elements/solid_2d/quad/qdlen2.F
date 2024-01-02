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
Chd|  QDLEN2                        source/elements/solid_2d/quad/qdlen2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE QDLEN2(Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,AIRE,DELTAX,IPARG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_MOD           
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
#include      "com01_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(63:63)
      my_real
     .   Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .   AIRE(*),DELTAX(*)
C-----------------------------------------------
      INTEGER I, NFAC, ISFLUID
      my_real
     .   AL1(MVSIZ), AL2(MVSIZ), AL3(MVSIZ), AL4(MVSIZ), ALMX(MVSIZ),
     .   XOFF(MVSIZ), ATEST(MVSIZ)
C-----------------------------------------------
C
      DO I=LFT,LLT
        XOFF(I)=ONE
      ENDDO
C
      ISFLUID=IPARG(63)

      IF(ALE%GLOBAL%ICAA==1 .AND. N2D==2 .AND. ISFLUID==1 .AND. JEUL+JALE/=0)THEN
        DO  I=LFT,LLT
          AL1(I)=(Z3(I)-Z1(I))*(Z3(I)-Z1(I))+(Y3(I)-Y1(I))*(Y3(I)-Y1(I))
          AL2(I)=(Z4(I)-Z2(I))*(Z4(I)-Z2(I))+(Y4(I)-Y2(I))*(Y4(I)-Y2(I))
          ALMX(I)=FOURTH*(AL1(I)+AL2(I))
        ENDDO
      ELSE
      DO I=LFT,LLT
        AL1(I)=(Z2(I)-Z1(I))*(Z2(I)-Z1(I))+(Y2(I)-Y1(I))*(Y2(I)-Y1(I))
        AL2(I)=(Z3(I)-Z2(I))*(Z3(I)-Z2(I))+(Y3(I)-Y2(I))*(Y3(I)-Y2(I))
        AL3(I)=(Z4(I)-Z3(I))*(Z4(I)-Z3(I))+(Y4(I)-Y3(I))*(Y4(I)-Y3(I))
        AL4(I)=(Z4(I)-Z1(I))*(Z4(I)-Z1(I))+(Y4(I)-Y1(I))*(Y4(I)-Y1(I))
      ENDDO
C
      DO I=LFT,LLT
        ALMX(I) = MAX(AL1(I),AL2(I),AL3(I),AL4(I))
      ENDDO
C
      DO I=LFT,LLT
        AL1(I)=(Z3(I)-Z1(I))*(Z3(I)-Z1(I))+(Y3(I)-Y1(I))*(Y3(I)-Y1(I))
        AL2(I)=(Z4(I)-Z2(I))*(Z4(I)-Z2(I))+(Y4(I)-Y2(I))*(Y4(I)-Y2(I))
      ENDDO
C
      DO I=LFT,LLT
        ALMX(I) = MAX(AL1(I),AL2(I),ALMX(I))
      ENDDO
C
      IF(ALE%GLOBAL%ICAA==0 .OR. JEUL+JALE==0)THEN
        DO I=LFT,LLT
          ATEST(I)=EM4*ALMX(I)
        ENDDO
        DO I=LFT,LLT
          NFAC=0
          IF(AL1(I)<ATEST(I)) NFAC=NFAC+1
          IF(AL2(I)<ATEST(I)) NFAC=NFAC+1
          IF(AL3(I)<ATEST(I)) NFAC=NFAC+1
          IF(AL4(I)<ATEST(I)) NFAC=NFAC+1
          IF(NFAC>=2) XOFF(I)=EP03
        ENDDO
      ENDIF
C
      ENDIF
      DO I=LFT,LLT
        DELTAX(I)=AIRE(I)*XOFF(I)/SQRT(ALMX(I))
      ENDDO
C
      RETURN
      END
