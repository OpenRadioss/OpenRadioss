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
Chd|  QDLEN2                        source/elements/solid_2d/quad/qdlen2.F
Chd|-- called by -----------
Chd|        BINIT2                        source/ale/bimat/binit2.F
Chd|        MULTIFLUID_INIT2              source/multifluid/multifluid_init2.F
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|-- calls ---------------
Chd|        LOI_FLUID                     source/fluid/loi_fluid.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE QDLEN2(IPARG,
     .     AIRE, DELTAX,
     .     Y1, Y2, Y3, Y4,
     .     Z1, Z2, Z3, Z4)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE  ALE_MOD
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
#include      "com01_c.inc"
C-----------------------------------------------
C   E x t e r n a l
C-----------------------------------------------
        LOGICAL,EXTERNAL :: LOI_FLUID
C-----------------------------------------------
C   D u m m y    A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARG(63:63)
        my_real
     .       AIRE(*), DELTAX(*),
     .       Y1(*), Y2(*), Y3(*), Y4(*),
     .       Z1(*), Z2(*), Z3(*), Z4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, NFAC, ISFLUID
        my_real
     .     AL1(MVSIZ), AL2(MVSIZ), AL3(MVSIZ), AL4(MVSIZ),
     .     ALMX(MVSIZ),
     .     XOFF(MVSIZ), ATEST(MVSIZ)
C--------------------------------------------------------------------
C
        DO I=LFT,LLT
          XOFF(I)=ONE
        END DO

        ISFLUID = 0
        IF(LOI_FLUID(MTN))ISFLUID=1

        IF(ALE%GLOBAL%ICAA==1 .AND. N2D==2 .AND. ISFLUID==1 .AND. MTN/=5)THEN
          DO I=LFT,LLT
            AL1(I)=(Z3(I)-Z1(I))*(Z3(I)-Z1(I))+(Y3(I)-Y1(I))*(Y3(I)-Y1(I))
            AL2(I)=(Z4(I)-Z2(I))*(Z4(I)-Z2(I))+(Y4(I)-Y2(I))*(Y4(I)-Y2(I))
            ALMX(I)=FOURTH*(AL1(I)+AL2(I))
          ENDDO
        ELSE
          DO I=LFT,LLT
            AL1(I)=(Z2(I)-Z1(I))*(Z2(I)-Z1(I))+
     .             (Y2(I)-Y1(I))*(Y2(I)-Y1(I))
            AL2(I)=(Z3(I)-Z2(I))*(Z3(I)-Z2(I))+
     .             (Y3(I)-Y2(I))*(Y3(I)-Y2(I))
            AL3(I)=(Z4(I)-Z3(I))*(Z4(I)-Z3(I))+
     .             (Y4(I)-Y3(I))*(Y4(I)-Y3(I))
            AL4(I)=(Z4(I)-Z1(I))*(Z4(I)-Z1(I))+
     .             (Y4(I)-Y1(I))*(Y4(I)-Y1(I))

            ALMX(I)= MAX(AL1(I),AL2(I),AL3(I),AL4(I))

            AL1(I)=(Z3(I)-Z1(I))*(Z3(I)-Z1(I))+
     .             (Y3(I)-Y1(I))*(Y3(I)-Y1(I))
            AL2(I)=(Z4(I)-Z2(I))*(Z4(I)-Z2(I))+
     .             (Y4(I)-Y2(I))*(Y4(I)-Y2(I))

            ALMX(I)= MAX(AL1(I),AL2(I),ALMX(I))
          END DO
          IF(ALE%GLOBAL%ICAA==0)THEN
            DO I=LFT,LLT
              ATEST(I)=EM4*ALMX(I)
              NFAC=0
              IF(AL1(I)<ATEST(I)) NFAC=NFAC+1
              IF(AL2(I)<ATEST(I)) NFAC=NFAC+1
              IF(AL3(I)<ATEST(I)) NFAC=NFAC+1
              IF(AL4(I)<ATEST(I)) NFAC=NFAC+1
              IF(NFAC>=2) XOFF(I)=EP03
            END DO
          ENDIF
        ENDIF

        DO I=LFT,LLT
          DELTAX(I)=AIRE(I)*XOFF(I)/SQRT(ALMX(I))
        END DO

        RETURN

      END
