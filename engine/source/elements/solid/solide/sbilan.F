Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  SBILAN                        source/elements/solid/solide/sbilan.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|====================================================================
      SUBROUTINE SBILAN(PARTSAV,EINT,RHO,RK,VOL,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   VNEW,IPARTS,GRESAV,GRTH,IGRTH, OFF,
     .   IEXPAN,EINTTH,FILL,MOM,NEL,IFVM22,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   ITASK,IPARG)
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
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "parit_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(IN) :: MOM(NEL,3)
      my_real
     .  PARTSAV(NPSAV,*), EINT(NEL), RHO(NEL), RK(NEL), VOL(NEL),
     .  VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .  VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .  VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .  VNEW(NEL), GRESAV(*), OFF(NEL), EINTTH(NEL), FILL(NEL),
     .  X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .  Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .  Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
      INTEGER IEXPAN,ITASK,
     .  IPARTS(*),GRTH(*),IGRTH(*),IPARG(*),NEL
      INTEGER,INTENT(IN) :: IFVM22
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, M, FLAG
C     REAL ou REAL*8
      my_real
     .   VXA(MVSIZ), VYA(MVSIZ) , VZA(MVSIZ), 
     .   VA2(MVSIZ), XMAS(MVSIZ), 
     .   EI(MVSIZ) , EK(MVSIZ),
     .   XM(MVSIZ) , YM(MVSIZ) , ZM(MVSIZ),
     .   XX(MVSIZ) , YY(MVSIZ) , ZZ(MVSIZ),
     .   XY(MVSIZ) , YZ(MVSIZ) , ZX(MVSIZ),
     .   XX2(MVSIZ), YY2(MVSIZ), ZZ2(MVSIZ),
     .   XCG(MVSIZ), YCG(MVSIZ), ZCG(MVSIZ),
     .   XXM(MVSIZ), YYM(MVSIZ), ZZM(MVSIZ),
     .   IXX(MVSIZ), IYY(MVSIZ), IZZ(MVSIZ),
     .   IXY(MVSIZ), IYZ(MVSIZ), IZX(MVSIZ),
     .   REI(MVSIZ), REK(MVSIZ), OFF_L(MVSIZ)
      INTEGER IC, II, J, JST(MVSIZ+1)
C-----------------------------------------------
      FLAG = IPARG(80) ! Flag pour sorties additionnelles
C-----------------------------------------------
C     CALCUL BILAN SORTIES DEF 
C-----------------------------------------------
      !ELEM VELOCITIES
      IF(INT22==0 .OR. IFVM22==0)THEN
        !FEM : from nodes
        DO I=LFT,LLT
         VXA(I)=VX1(I)+VX2(I)+VX3(I)+VX4(I)+VX5(I)+VX6(I)+VX7(I)+VX8(I)
         VYA(I)=VY1(I)+VY2(I)+VY3(I)+VY4(I)+VY5(I)+VY6(I)+VY7(I)+VY8(I)
         VZA(I)=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)+VZ5(I)+VZ6(I)+VZ7(I)+VZ8(I)
         VA2(I)=VX1(I)**2+VX2(I)**2+VX3(I)**2+VX4(I)**2
     1         +VX5(I)**2+VX6(I)**2+VX7(I)**2+VX8(I)**2
     2         +VY1(I)**2+VY2(I)**2+VY3(I)**2+VY4(I)**2
     3         +VY5(I)**2+VY6(I)**2+VY7(I)**2+VY8(I)**2
     4         +VZ1(I)**2+VZ2(I)**2+VZ3(I)**2+VZ4(I)**2
     5         +VZ5(I)**2+VZ6(I)**2+VZ7(I)**2+VZ8(I)**2
        ENDDO
        DO I=LFT,LLT
          VXA(I)=VXA(I)*ONE_OVER_8
          VYA(I)=VYA(I)*ONE_OVER_8
          VZA(I)=VZA(I)*ONE_OVER_8
          VA2(I)=VA2(I)*ONE_OVER_8
        ENDDO
      ELSE
        !FVM - from centroid
        DO I=LFT,LLT
          VXA(I)  = MOM(I,1)/RHO(I)/VOL(I)
          VYA(I)  = MOM(I,2)/RHO(I)/VOL(I)
          VZA(I)  = MOM(I,3)/RHO(I)/VOL(I)
          VA2(I)  = (VXA(I)**2 + VYA(I)**2 + VZA(I)**2)
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        XMAS(I)= FILL(I)*RHO(I)*VNEW(I)
        EI(I)  = FILL(I)*EINT(I)*VOL(I)
        EK(I)  = XMAS(I)*VA2(I)*HALF     
        XM(I)  = XMAS(I)*VXA(I)
        YM(I)  = XMAS(I)*VYA(I)
        ZM(I)  = XMAS(I)*VZA(I)
      ENDDO
C
      M=IPARTS(LFT)
      IF(IPARTSPH==0)THEN
        DO I=LFT,LLT
          PARTSAV(1,M)=PARTSAV(1,M) + EI(I)
          PARTSAV(2,M)=PARTSAV(2,M) + EK(I)
          PARTSAV(3,M)=PARTSAV(3,M) + XM(I)
          PARTSAV(4,M)=PARTSAV(4,M) + YM(I)
          PARTSAV(5,M)=PARTSAV(5,M) + ZM(I)
          PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          IF(OFF(I) < ONE)CYCLE
            PARTSAV(1,M)=PARTSAV(1,M) + EI(I)
            PARTSAV(2,M)=PARTSAV(2,M) + EK(I)
            PARTSAV(3,M)=PARTSAV(3,M) + XM(I)
            PARTSAV(4,M)=PARTSAV(4,M) + YM(I)
            PARTSAV(5,M)=PARTSAV(5,M) + ZM(I)
            PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
        ENDDO
      END IF
C
C-----------------------------------------------
C     CALCUL BILAN SORTIES ADDITIONNELLES 
C-----------------------------------------------
      IF(FLAG.EQ.1) THEN
        IF(INT22==0 .OR. IFVM22==0)THEN
          !FEM : from nodes
          DO I=LFT,LLT
           XX(I)=X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I)+X7(I)+X8(I)
           YY(I)=Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I)+Y7(I)+Y8(I)
           ZZ(I)=Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I)+Z7(I)+Z8(I)
           XX2(I)=X1(I)**2+X2(I)**2+X3(I)**2+X4(I)**2
     .           +X5(I)**2+X6(I)**2+X7(I)**2+X8(I)**2
           YY2(I)=Y1(I)**2+Y2(I)**2+Y3(I)**2+Y4(I)**2
     .           +Y5(I)**2+Y6(I)**2+Y7(I)**2+Y8(I)**2
           ZZ2(I)=Z1(I)**2+Z2(I)**2+Z3(I)**2+Z4(I)**2
     .           +Z5(I)**2+Z6(I)**2+Z7(I)**2+Z8(I)**2
           XY(I)=X1(I)*Y1(I)+X2(I)*Y2(I)+X3(I)*Y3(I)+X4(I)*Y4(I)
     .          +X5(I)*Y5(I)+X6(I)*Y6(I)+X7(I)*Y7(I)+X8(I)*Y8(I)
           YZ(I)=Y1(I)*Z1(I)+Y2(I)*Z2(I)+Y3(I)*Z3(I)+Y4(I)*Z4(I)
     .          +Y5(I)*Z5(I)+Y6(I)*Z6(I)+Y7(I)*Z7(I)+Y8(I)*Z8(I)
           ZX(I)=Z1(I)*X1(I)+Z2(I)*X2(I)+Z3(I)*X3(I)+Z4(I)*X4(I)
     .          +Z5(I)*X5(I)+Z6(I)*X6(I)+Z7(I)*X7(I)+Z8(I)*X8(I)
          ENDDO
          DO I=LFT,LLT
            XX(I)=XX(I)*ONE_OVER_8
            YY(I)=YY(I)*ONE_OVER_8
            ZZ(I)=ZZ(I)*ONE_OVER_8
            XY(I)=XY(I)*ONE_OVER_8
            YZ(I)=YZ(I)*ONE_OVER_8
            ZX(I)=ZX(I)*ONE_OVER_8
            XX2(I)=XX2(I)*ONE_OVER_8
            YY2(I)=YY2(I)*ONE_OVER_8
            ZZ2(I)=ZZ2(I)*ONE_OVER_8
          ENDDO
        ELSE
          !FVM - from centroid
          DO I=LFT,LLT
            XX(I)   = X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I)+X7(I)+X8(I)
            YY(I)   = Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I)+Y7(I)+Y8(I)
            ZZ(I)   = Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I)+Z7(I)+Z8(I)
            XX2(I)=X1(I)**2+X2(I)**2+X3(I)**2+X4(I)**2
     .            +X5(I)**2+X6(I)**2+X7(I)**2+X8(I)**2
            YY2(I)=Y1(I)**2+Y2(I)**2+Y3(I)**2+Y4(I)**2
     .            +Y5(I)**2+Y6(I)**2+Y7(I)**2+Y8(I)**2
            ZZ2(I)=Z1(I)**2+Z2(I)**2+Z3(I)**2+Z4(I)**2
     .            +Z5(I)**2+Z6(I)**2+Z7(I)**2+Z8(I)**2
            XY(I)=X1(I)*Y1(I)+X2(I)*Y2(I)+X3(I)*Y3(I)+X4(I)*Y4(I)
     .           +X5(I)*Y5(I)+X6(I)*Y6(I)+X7(I)*Y7(I)+X8(I)*Y8(I)
            YZ(I)=Y1(I)*Z1(I)+Y2(I)*Z2(I)+Y3(I)*Z3(I)+Y4(I)*Z4(I)
     .           +Y5(I)*Z5(I)+Y6(I)*Z6(I)+Y7(I)*Z7(I)+Y8(I)*Z8(I)
            ZX(I)=Z1(I)*X1(I)+Z2(I)*X2(I)+Z3(I)*X3(I)+Z4(I)*X4(I)
     .           +Z5(I)*X5(I)+Z6(I)*X6(I)+Z7(I)*X7(I)+Z8(I)*X8(I)
          ENDDO
          DO I=LFT,LLT
            XX(I)=XX(I)*ONE_OVER_8
            YY(I)=YY(I)*ONE_OVER_8
            ZZ(I)=ZZ(I)*ONE_OVER_8
            XY(I)=XY(I)*ONE_OVER_8
            YZ(I)=YZ(I)*ONE_OVER_8
            ZX(I)=ZX(I)*ONE_OVER_8
            XX2(I)=XX2(I)*ONE_OVER_8
            YY2(I)=YY2(I)*ONE_OVER_8
            ZZ2(I)=ZZ2(I)*ONE_OVER_8
          ENDDO
        ENDIF
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
        M=IPARTS(LFT)
        IF(IPARTSPH==0)THEN
          DO I=LFT,LLT
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
          ENDDO
        ELSE
          DO I=LFT,LLT
            IF(OFF(I) < ONE)CYCLE
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
          ENDDO
        END IF
      ENDIF
C
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
      IF (JTUR /= 0) THEN
        DO I=LFT,LLT
          M=IPARTS(I)
          PARTSAV(7,M)=PARTSAV(7,M) + RK(I)*VOL(I)
        ENDDO
      ENDIF
C
      IF(IEXPAN.NE.0)THEN
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

      CALL SENSOR_ENERGY_BILAN(LFT,LLT,EI,EK,OFF_L,IPARTS,ITASK)      
C
C-----------
      RETURN
      END
