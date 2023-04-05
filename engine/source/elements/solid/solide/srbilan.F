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
Chd|  SRBILAN                       source/elements/solid/solide/srbilan.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|====================================================================
      SUBROUTINE SRBILAN(PARTSAV,EINT,RHO,RK,VOL,
     .   VXA, VYA, VZA, VA2, VNEW,IPARTS ,
     .   GRESAV,GRTH,IGRTH,OFF,IEXPAN,EINTTH,
     .   FILL  ,XX, YY, ZZ,
     .   XX2,YY2,ZZ2,XY,YZ,ZX,ITASK,IPARG)
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
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .  PARTSAV(NPSAV,*), EINT(*), RHO(*), RK(*), VOL(*),
     .  VXA(*), VYA(*), VZA(*), VA2(*),
     .  VNEW(*), GRESAV(*), OFF(MVSIZ), EINTTH(*), FILL(*),
     .  XX(MVSIZ), YY(MVSIZ), ZZ(MVSIZ),
     .  XX2(MVSIZ), YY2(MVSIZ), ZZ2(MVSIZ),
     .  XY(MVSIZ), YZ(MVSIZ), ZX(MVSIZ)
      INTEGER IEXPAN,ITASK,
     .  IPARTS(*),GRTH(*),IGRTH(*),IPARG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, M, FLAG
C     REAL
      my_real
     .   XMAS(MVSIZ), 
     .   EI(MVSIZ) , EK(MVSIZ),
     .   XM(MVSIZ) , YM(MVSIZ) , ZM(MVSIZ),
     .   XCG(MVSIZ), YCG(MVSIZ), ZCG(MVSIZ),
     .   XXM(MVSIZ), YYM(MVSIZ), ZZM(MVSIZ),
     .   IXX(MVSIZ), IYY(MVSIZ), IZZ(MVSIZ),
     .   IXY(MVSIZ), IYZ(MVSIZ), IZX(MVSIZ),
     .   REI(MVSIZ), REK(MVSIZ), OFF_L(MVSIZ)
C-----------------------------------------------
      FLAG = IPARG(80) ! Flag pour sorties additionnelles
C-----------------------------------------------
C     CALCUL BILAN SORTIES DEF 
C-----------------------------------------------
      DO I=LFT,LLT
        VXA(I) = VXA(I)*ONE_OVER_8
        VYA(I) = VYA(I)*ONE_OVER_8
        VZA(I) = VZA(I)*ONE_OVER_8
        VA2(I) = VA2(I)*ONE_OVER_8
      ENDDO
C
      DO I=LFT,LLT
        XMAS(I)=FILL(I)*RHO(I)*VNEW(I)
        EI(I)  = FILL(I)*EINT(I)*VOL(I)
        EK(I)  = XMAS(I)*VA2(I)*HALF
        XM(I)  = XMAS(I)*VXA(I)
        YM(I)  = XMAS(I)*VYA(I)
        ZM(I)  = XMAS(I)*VZA(I)
      ENDDO
C
      IF(IPARTSPH==0)THEN
        DO I=LFT,LLT
          M=IPARTS(I)
          PARTSAV(1,M)=PARTSAV(1,M) + EI(I)
          PARTSAV(2,M)=PARTSAV(2,M) + EK(I)
          PARTSAV(3,M)=PARTSAV(3,M) + XM(I)
          PARTSAV(4,M)=PARTSAV(4,M) + YM(I)
          PARTSAV(5,M)=PARTSAV(5,M) + ZM(I)
          IF(OFF(I) >= ONE) PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
        END DO
      ELSE
        DO I=LFT,LLT
          IF(OFF(I) < ONE) CYCLE
          M=IPARTS(I)
          PARTSAV(1,M)=PARTSAV(1,M) + EI(I)
          PARTSAV(2,M)=PARTSAV(2,M) + EK(I)
          PARTSAV(3,M)=PARTSAV(3,M) + XM(I)
          PARTSAV(4,M)=PARTSAV(4,M) + YM(I)
          PARTSAV(5,M)=PARTSAV(5,M) + ZM(I)
          PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
        END DO
      END IF
C
C-----------------------------------------------
C     CALCUL BILAN SORTIES ADDITIONNELLES 
C-----------------------------------------------
      IF(FLAG==1) THEN
        DO I=LFT,LLT
          XX(I) = XX(I)*ONE_OVER_8
          YY(I) = YY(I)*ONE_OVER_8
          ZZ(I) = ZZ(I)*ONE_OVER_8
          XY(I) = XY(I)*ONE_OVER_8
          YZ(I) = YZ(I)*ONE_OVER_8
          ZX(I) = ZX(I)*ONE_OVER_8
          XX2(I)= XX2(I)*ONE_OVER_8
          YY2(I)= YY2(I)*ONE_OVER_8
          ZZ2(I)= ZZ2(I)*ONE_OVER_8
        ENDDO
C
        DO I=LFT,LLT
          XCG(I)= XMAS(I)*XX(I)
          YCG(I)= XMAS(I)*YY(I)
          ZCG(I)= XMAS(I)*ZZ(I)
          IXY(I)= -XMAS(I)*XY(I)
          IYZ(I)= -XMAS(I)*YZ(I)
          IZX(I)= -XMAS(I)*ZX(I)
          IXX(I)= XMAS(I)*(YY2(I) + ZZ2(I))
          IYY(I)= XMAS(I)*(ZZ2(I) + XX2(I))
          IZZ(I)= XMAS(I)*(XX2(I) + YY2(I))
          XXM(I)= VZA(I)*YCG(I)-VYA(I)*ZCG(I)
          YYM(I)= VXA(I)*ZCG(I)-VZA(I)*XCG(I)
          ZZM(I)= VYA(I)*XCG(I)-VXA(I)*YCG(I)
          REI(I)= ZERO
          REK(I)= ZERO
        ENDDO
C
        IF(IPARTSPH==0)THEN
          DO I=LFT,LLT
            M=IPARTS(I)
            PARTSAV(9,M) =PARTSAV(9,M)  + XCG(I)
            PARTSAV(10,M)=PARTSAV(10,M) + YCG(I)
            PARTSAV(11,M)=PARTSAV(11,M) + ZCG(I)
            PARTSAV(12,M)=PARTSAV(12,M) + XXM(I)
            PARTSAV(13,M)=PARTSAV(13,M) + YYM(I)
            PARTSAV(14,M)=PARTSAV(14,M) + ZZM(I)
            PARTSAV(15,M)=PARTSAV(15,M) + IXX(I)
            PARTSAV(16,M)=PARTSAV(16,M) + IYY(I)
            PARTSAV(17,M)=PARTSAV(17,M) + IZZ(I)
            PARTSAV(18,M)=PARTSAV(18,M) + IXY(I)
            PARTSAV(19,M)=PARTSAV(19,M) + IYZ(I)
            PARTSAV(20,M)=PARTSAV(20,M) + IZX(I)
            PARTSAV(21,M)=PARTSAV(21,M) + REI(I)
            PARTSAV(22,M)=PARTSAV(22,M) + REK(I) 
C           PARTSAV(8,M)=PARTSAV(8,M) + EHOU(I)
          END DO
        ELSE
          DO I=LFT,LLT
            IF(OFF(I) < ONE) CYCLE
            M=IPARTS(I)
            PARTSAV(9,M) =PARTSAV(9,M)  + XCG(I)
            PARTSAV(10,M)=PARTSAV(10,M) + YCG(I)
            PARTSAV(11,M)=PARTSAV(11,M) + ZCG(I)
            PARTSAV(12,M)=PARTSAV(12,M) + XXM(I)
            PARTSAV(13,M)=PARTSAV(13,M) + YYM(I)
            PARTSAV(14,M)=PARTSAV(14,M) + ZZM(I)
            PARTSAV(15,M)=PARTSAV(15,M) + IXX(I)
            PARTSAV(16,M)=PARTSAV(16,M) + IYY(I)
            PARTSAV(17,M)=PARTSAV(17,M) + IZZ(I)
            PARTSAV(18,M)=PARTSAV(18,M) + IXY(I)
            PARTSAV(19,M)=PARTSAV(19,M) + IYZ(I)
            PARTSAV(20,M)=PARTSAV(20,M) + IZX(I)
            PARTSAV(21,M)=PARTSAV(21,M) + REI(I)
            PARTSAV(22,M)=PARTSAV(22,M) + REK(I) 
C           PARTSAV(8,M)=PARTSAV(8,M) + EHOU(I)
          END DO
        END IF
      ENDIF
C-----------------------------------------------
      IF (IGRE /= 0) THEN
          CALL GRELEM_SAV(LFT   ,LLT   ,GRESAV,IGRTH ,GRTH  ,
     2                    OFF   ,EI    ,EK    ,XM    ,YM    ,
     3                    ZM    ,XMAS  ,XCG   ,YCG   ,ZCG   ,
     4                    XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                    IZZ   ,IXY   ,IYZ   ,IZX   ,REI   ,
     6                    REK   ,FLAG  )
      ENDIF
C-----------------------------------------------
C
      IF(JTUR/=0)THEN
       DO 50 I=LFT,LLT
       M=IPARTS(I)
       PARTSAV(7,M)=PARTSAV(7,M) + RK(I)*VOL(I)
   50  CONTINUE
      ENDIF
C
      IF(IEXPAN/=0)THEN
        DO I=LFT,LLT
         M=IPARTS(I)
          PARTSAV(27,M)=PARTSAV(27,M) + EINTTH(I)*VOL(I)
        ENDDO
      ENDIF
      DO I=LFT,LLT
          IF(OFF(I) < ONE) THEN
            OFF_L(I) = ZERO
          ELSE
            OFF_L(I) = ONE
          ENDIF
      ENDDO
C
      DO I = LFT,LLT
        M=IPARTS(I)
        IF (OFF(I) < ONE) THEN 
          PARTSAV(25,M) = PARTSAV(25,M) + ONE
        ENDIF
      ENDDO
C
      CALL SENSOR_ENERGY_BILAN(LFT,LLT,EI,EK,OFF_L,IPARTS,ITASK)    
C
      RETURN
      END
