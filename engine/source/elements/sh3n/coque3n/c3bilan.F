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
Chd|  C3BILAN                       source/elements/sh3n/coque3n/c3bilan.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|        C3COOR3_CRK2                  source/elements/xfem/c3coor3_crk.F
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|====================================================================
      SUBROUTINE C3BILAN(JFT    ,JLT    ,PM   ,V    ,THK    ,EINT     ,
     2                   PARTSAV,IPARTTG,RHO  ,VOL00,IXTG   ,X        ,
     3                   VR     ,THK02  ,AREA ,GRESAV,GRTH  ,
     4                   IGRTH  ,OFF    ,IXFEM,ILEV ,IEL_CRK,IADTG_CRK,
     5                   NFT1   ,IEXPAN ,EINTTH ,ITASK,MAT  ,GVOL     ,
     6                   ACTIFXFEM)
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
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, IEXPAN,
     .   IPARTTG(*),IXTG(NIXTG,*),GRTH(*),IGRTH(*),IXFEM,
     .   ILEV,IEL_CRK(*),IADTG_CRK(3,*),NFT1,ITASK
      INTEGER, INTENT(IN) :: MAT(MVSIZ),ACTIFXFEM
C     REAL
      my_real
     .   PM(NPROPM,*), V(3,*), THK(*), EINT(JLT,2),PARTSAV(NPSAV,*)
      my_real
     .   RHO(MVSIZ), VOL00(MVSIZ),X(3,*),VR(3,*) ,THK02(*),AREA(*),
     .   GRESAV(*), OFF(*),EINTTH(*)
      my_real, INTENT(IN) :: GVOL(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, II, J, IC, JST(MVSIZ+1),FLAG
C     REAL
      my_real
     .   IN25,XX,YY,ZZ,XY,YZ,ZX, COEF, INEL, 
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), 
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ),
     .   EI(MVSIZ),XMAS(MVSIZ),REI(MVSIZ),REK(MVSIZ),
     .   EK(MVSIZ), XM(MVSIZ), YM(MVSIZ), ZM(MVSIZ),
     .   VXA(MVSIZ), VYA(MVSIZ), VZA(MVSIZ) , VA2, XMAS25(MVSIZ),
     .   XXM(MVSIZ), YYM(MVSIZ), ZZM(MVSIZ), 
     .   XCG(MVSIZ), YCG(MVSIZ), ZCG(MVSIZ),
     .   IXX(MVSIZ), IYY(MVSIZ), IZZ(MVSIZ),
     .   IXY(MVSIZ), IYZ(MVSIZ), IZX(MVSIZ),
     .   VRX1(MVSIZ), VRX2(MVSIZ), VRX3(MVSIZ),
     .   VRY1(MVSIZ), VRY2(MVSIZ), VRY3(MVSIZ), 
     .   VRZ1(MVSIZ), VRZ2(MVSIZ), VRZ3(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), 
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3)
C-----------------------------------------------
      IF(IXFEM == 0)THEN
        DO I=JFT,JLT
          VX1(I)=V(1,IXTG(2,I))
          VY1(I)=V(2,IXTG(2,I))
          VZ1(I)=V(3,IXTG(2,I))
          VX2(I)=V(1,IXTG(3,I))
          VY2(I)=V(2,IXTG(3,I))
          VZ2(I)=V(3,IXTG(3,I))
          VX3(I)=V(1,IXTG(4,I))
          VY3(I)=V(2,IXTG(4,I))
          VZ3(I)=V(3,IXTG(4,I))
        ENDDO
      ELSEIF(IXFEM==1)THEN
        CALL C3COOR3_CRK2(
     1              JFT  ,JLT  ,NFT1  ,ILEV     ,IEL_CRK,
     2              X    ,V    ,VR   ,VL1      ,VL2    ,
     3              VL3  ,VRL1 ,VRL2 ,VRL3     ,X1     ,
     4              X2   ,X3   ,Y1   ,Y2       ,Y3     ,
     5              Z1   ,Z2   ,Z3   ,IADTG_CRK)
        DO I=JFT,JLT
          VX1(I)=VL1(I,1)
          VY1(I)=VL1(I,2)
          VZ1(I)=VL1(I,3)
          VX2(I)=VL2(I,1)
          VY2(I)=VL2(I,2)
          VZ2(I)=VL2(I,3)
          VX3(I)=VL3(I,1)
          VY3(I)=VL3(I,2)
          VZ3(I)=VL3(I,3)
          VRX1(I)=VRL1(I,1)
          VRY1(I)=VRL1(I,2)
          VRZ1(I)=VRL1(I,3)
          VRX2(I)=VRL2(I,1)
          VRY2(I)=VRL2(I,2)
          VRZ2(I)=VRL2(I,3)
          VRX3(I)=VRL3(I,1)
          VRY3(I)=VRL3(I,2)
          VRZ3(I)=VRL3(I,3)
        ENDDO
      ENDIF
C
      MX = MAT(JFT)
      DO I=JFT,JLT
       VXA(I)=VX1(I)+VX2(I)+VX3(I)
       VYA(I)=VY1(I)+VY2(I)+VY3(I)
       VZA(I)=VZ1(I)+VZ2(I)+VZ3(I)
       XMAS(I)=PM(1,MX)*GVOL(I)
       VA2   =VX1(I)*VX1(I)+VX2(I)*VX2(I)+VX3(I)*VX3(I)
     1       +VY1(I)*VY1(I)+VY2(I)*VY2(I)+VY3(I)*VY3(I)
     2       +VZ1(I)*VZ1(I)+VZ2(I)*VZ2(I)+VZ3(I)*VZ3(I)
       EI(I) = EINT(I,1) + EINT(I,2)
       EK(I) = XMAS(I)*VA2*ONE_OVER_6
       XMAS25(I)= XMAS(I)*THIRD
       XM(I) = XMAS25(I)*VXA(I)
       YM(I) = XMAS25(I)*VYA(I)
       ZM(I) = XMAS25(I)*VZA(I)
      ENDDO
C
C     !!!!!!!!!! VOL0,VOL00,THK0,THK02,X,VR ne sont transmis que par
C                CFORC3 et pas par CSUBC3 et CZFORC3 (IFLA=0) !!!!!!!
C
      IF(NPSAV>=21)THEN
       IF(IXFEM == 0)THEN
        DO I=JFT,JLT
          XX= X(1,IXTG(2,I))+X(1,IXTG(3,I))+X(1,IXTG(4,I))
          YY= X(2,IXTG(2,I))+X(2,IXTG(3,I))+X(2,IXTG(4,I))
          ZZ= X(3,IXTG(2,I))+X(3,IXTG(3,I))+X(3,IXTG(4,I))
          XCG(I)= XMAS25(I)*XX
          YCG(I)= XMAS25(I)*YY
          ZCG(I)= XMAS25(I)*ZZ
          IN25 = XMAS25(I)*(THK02(I)+AREA(I))*ONE_OVER_12
          INEL = THREE*IN25
          XX   = THIRD*XX
          YY   = THIRD*YY
          ZZ   = THIRD*ZZ
          IXY(I) = -XCG(I)*YY
          IYZ(I) = -YCG(I)*ZZ
          IZX(I) = -ZCG(I)*XX
          XX = XCG(I)*XX
          YY = YCG(I)*YY
          ZZ = ZCG(I)*ZZ
          IXX(I)= INEL + YY + ZZ
          IYY(I)= INEL + ZZ + XX
          IZZ(I)= INEL + XX + YY
          VXA(I)=THIRD*VXA(I)
          VYA(I)=THIRD*VYA(I)
          VZA(I)=THIRD*VZA(I)
          XXM(I)= VZA(I)*YCG(I)-VYA(I)*ZCG(I)
     .           +IN25*
     .           (VR(1,IXTG(2,I))+VR(1,IXTG(3,I))+VR(1,IXTG(4,I)))
          YYM(I)= VXA(I)*ZCG(I)-VZA(I)*XCG(I)
     .           +IN25*
     .           (VR(2,IXTG(2,I))+VR(2,IXTG(3,I))+VR(2,IXTG(4,I)))
          ZZM(I)= VYA(I)*XCG(I)-VXA(I)*YCG(I)
     .           +IN25*
     .           (VR(3,IXTG(2,I))+VR(3,IXTG(3,I))+VR(3,IXTG(4,I)))
          VA2 =  
     .   VR(1,IXTG(2,I))*VR(1,IXTG(2,I))+VR(1,IXTG(3,I))*VR(1,IXTG(3,I))
     .  +VR(1,IXTG(4,I))*VR(1,IXTG(4,I))
     .  +VR(2,IXTG(2,I))*VR(2,IXTG(2,I))+VR(2,IXTG(3,I))*VR(2,IXTG(3,I))
     .  +VR(2,IXTG(4,I))*VR(2,IXTG(4,I))
     .  +VR(3,IXTG(2,I))*VR(3,IXTG(2,I))+VR(3,IXTG(3,I))*VR(3,IXTG(3,I))
     .  +VR(3,IXTG(4,I))*VR(3,IXTG(4,I))
          REI(I)= EINT(I,2)
          REK(I)= IN25*VA2*HALF
        ENDDO
       ELSEIF(IXFEM==1)THEN
        DO I=JFT,JLT
          XX= X1(I)+X2(I)+X3(I)
          YY= Y1(I)+Y2(I)+Y3(I)
          ZZ= Z1(I)+Z2(I)+Z3(I)
          XCG(I)= XMAS25(I)*XX
          YCG(I)= XMAS25(I)*YY
          ZCG(I)= XMAS25(I)*ZZ
          IN25 = XMAS25(I)*(THK02(I)+AREA(I))*ONE_OVER_12
          INEL = THREE*IN25
          XX   = THIRD*XX
          YY   = THIRD*YY
          ZZ   = THIRD*ZZ
          IXY(I) = -XCG(I)*YY
          IYZ(I) = -YCG(I)*ZZ
          IZX(I) = -ZCG(I)*XX
          XX = XCG(I)*XX
          YY = YCG(I)*YY
          ZZ = ZCG(I)*ZZ
          IXX(I)= INEL + YY + ZZ
          IYY(I)= INEL + ZZ + XX
          IZZ(I)= INEL + XX + YY
          VXA(I)=THIRD*VXA(I)
          VYA(I)=THIRD*VYA(I)
          VZA(I)=THIRD*VZA(I)
          XXM(I)= VZA(I)*YCG(I)-VYA(I)*ZCG(I)
     .           +IN25*(VRX1(I)+VRX2(I)+VRX3(I))
          YYM(I)= VXA(I)*ZCG(I)-VZA(I)*XCG(I)
     .           +IN25*(VRY1(I)+VRY2(I)+VRY3(I))
          ZZM(I)= VYA(I)*XCG(I)-VXA(I)*YCG(I)
     .           +IN25*(VRZ1(I)+VRZ2(I)+VRZ3(I))
          VA2 =  VRX1(I)*VRX1(I)+VRX2(I)*VRX2(I)+VRX3(I)*VRX3(I)
     .          +VRY1(I)*VRY1(I)+VRY2(I)*VRY2(I)+VRY3(I)*VRY3(I)
     .          +VRZ1(I)*VRZ1(I)+VRZ2(I)*VRZ2(I)+VRZ3(I)*VRZ3(I)
          REI(I)= EINT(I,2)
          REK(I)= IN25*VA2*HALF
        ENDDO
       ENDIF
C
        FLAG = 1
C
        IF (IGRE /= 0) THEN
          CALL GRELEM_SAV(JFT   ,JLT   ,GRESAV,IGRTH ,GRTH  ,
     2                    OFF   ,EI    ,EK    ,XM    ,YM    ,
     3                    ZM    ,XMAS  ,XCG   ,YCG   ,ZCG   ,
     4                    XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                    IZZ   ,IXY   ,IYZ   ,IZX   ,REI   ,
     6                    REK   ,FLAG)
        ENDIF
C
        IC=1
        JST(IC)=JFT
        DO J=JFT+1,JLT
          IF (IPARTTG(J)/=IPARTTG(J-1)) THEN
            IC=IC+1
            JST(IC)=J
          ENDIF
        ENDDO
        JST(IC+1)=JLT+1                 
C
        IF (IC==1) THEN
          MX = IPARTTG(JFT)
          DO I=JFT,JLT
            IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
              IF(OFF(I)/=ZERO)THEN
                PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
                PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
                PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
                PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
                PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
              ENDIF
            ELSE 
              PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
              PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
              PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
              PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
              PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
            ENDIF
            IF(OFF(I)/=ZERO)THEN
              PARTSAV(6,MX)=PARTSAV(6,MX) + XMAS(I)
            ENDIF
            PARTSAV(9,MX)=PARTSAV(9,MX) + XCG(I)
            PARTSAV(10,MX)=PARTSAV(10,MX) + YCG(I)
            PARTSAV(11,MX)=PARTSAV(11,MX) + ZCG(I)
            PARTSAV(12,MX)=PARTSAV(12,MX) + XXM(I)
            PARTSAV(13,MX)=PARTSAV(13,MX) + YYM(I)
            PARTSAV(14,MX)=PARTSAV(14,MX) + ZZM(I)
            PARTSAV(15,MX)=PARTSAV(15,MX) + IXX(I)
            PARTSAV(16,MX)=PARTSAV(16,MX) + IYY(I)
            PARTSAV(17,MX)=PARTSAV(17,MX) + IZZ(I)
            PARTSAV(18,MX)=PARTSAV(18,MX) + IXY(I)
            PARTSAV(19,MX)=PARTSAV(19,MX) + IYZ(I)
            PARTSAV(20,MX)=PARTSAV(20,MX) + IZX(I)
            PARTSAV(21,MX)=PARTSAV(21,MX) + REI(I)
            PARTSAV(22,MX)=PARTSAV(22,MX) + REK(I)
          ENDDO
        ELSE
          DO II=1,IC
            MX=IPARTTG(JST(II))
            IF (JST(II+1)-JST(II)>15) THEN
              DO I=JST(II),JST(II+1)-1
                IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
                  IF(OFF(I)/=ZERO)THEN
                    PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
                    PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
                    PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
                    PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
                    PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
                  ENDIF
                ELSE 
                  PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
                  PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
                  PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
                  PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
                  PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
                ENDIF
                IF(OFF(I)/=ZERO)THEN
                  PARTSAV(6,MX)=PARTSAV(6,MX) + XMAS(I)
                ENDIF
                PARTSAV(9,MX)=PARTSAV(9,MX) + XCG(I)
                PARTSAV(10,MX)=PARTSAV(10,MX) + YCG(I)
                PARTSAV(11,MX)=PARTSAV(11,MX) + ZCG(I)
                PARTSAV(12,MX)=PARTSAV(12,MX) + XXM(I)
                PARTSAV(13,MX)=PARTSAV(13,MX) + YYM(I)
                PARTSAV(14,MX)=PARTSAV(14,MX) + ZZM(I)
                PARTSAV(15,MX)=PARTSAV(15,MX) + IXX(I)
                PARTSAV(16,MX)=PARTSAV(16,MX) + IYY(I)
                PARTSAV(17,MX)=PARTSAV(17,MX) + IZZ(I)
                PARTSAV(18,MX)=PARTSAV(18,MX) + IXY(I)
                PARTSAV(19,MX)=PARTSAV(19,MX) + IYZ(I)
                PARTSAV(20,MX)=PARTSAV(20,MX) + IZX(I)
                PARTSAV(21,MX)=PARTSAV(21,MX) + REI(I)
                PARTSAV(22,MX)=PARTSAV(22,MX) + REK(I)
              ENDDO
            ELSE
#include "vectorize.inc"
              DO I=JST(II),JST(II+1)-1
                IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
                  IF(OFF(I)/=ZERO)THEN
                    PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
                    PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
                    PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
                    PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
                    PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
                  ENDIF
                ELSE 
                  PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
                  PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
                  PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
                  PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
                  PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
                ENDIF
                IF(OFF(I)/=ZERO)THEN
                  PARTSAV(6,MX)=PARTSAV(6,MX) + XMAS(I)
                ENDIF
                PARTSAV(9,MX)=PARTSAV(9,MX) + XCG(I)
                PARTSAV(10,MX)=PARTSAV(10,MX) + YCG(I)
                PARTSAV(11,MX)=PARTSAV(11,MX) + ZCG(I)
                PARTSAV(12,MX)=PARTSAV(12,MX) + XXM(I)
                PARTSAV(13,MX)=PARTSAV(13,MX) + YYM(I)
                PARTSAV(14,MX)=PARTSAV(14,MX) + ZZM(I)
                PARTSAV(15,MX)=PARTSAV(15,MX) + IXX(I)
                PARTSAV(16,MX)=PARTSAV(16,MX) + IYY(I)
                PARTSAV(17,MX)=PARTSAV(17,MX) + IZZ(I)
                PARTSAV(18,MX)=PARTSAV(18,MX) + IXY(I)
                PARTSAV(19,MX)=PARTSAV(19,MX) + IYZ(I)
                PARTSAV(20,MX)=PARTSAV(20,MX) + IZX(I)
                PARTSAV(21,MX)=PARTSAV(21,MX) + REI(I)
                PARTSAV(22,MX)=PARTSAV(22,MX) + REK(I)
              ENDDO
            ENDIF ! IF (JST(II+1)-JST(II)>15)
          ENDDO
        ENDIF ! IF (IC==1)
      ELSE ! IF(NPSAV>=21)
C
        IC=1
        JST(IC)=JFT
        DO J=JFT+1,JLT
          IF (IPARTTG(J)/=IPARTTG(J-1)) THEN
            IC=IC+1
            JST(IC)=J
          ENDIF
        ENDDO
        JST(IC+1)=JLT+1                 
C
        IF (IC==1) THEN
          MX = IPARTTG(JFT)
          DO I=JFT,JLT
            IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
              IF(OFF(I)/=ZERO)THEN
                PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
                PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
                PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
                PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
                PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
              ENDIF
            ELSE 
              PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
              PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
              PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
              PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
              PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
            ENDIF
            IF(OFF(I)/=ZERO)THEN
              PARTSAV(6,MX)=PARTSAV(6,MX) + XMAS(I)
            ENDIF
          ENDDO
        ELSE
          DO II=1,IC
            MX=IPARTTG(JST(II))
            IF (JST(II+1)-JST(II)>15) THEN
              DO I=JST(II),JST(II+1)-1
                IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
                  IF(OFF(I)/=ZERO)THEN
                    PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
                    PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
                    PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
                    PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
                    PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
                  ENDIF
                ELSE 
                  PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
                  PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
                  PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
                  PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
                  PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
                ENDIF
                IF(OFF(I)/=ZERO)THEN
                  PARTSAV(6,MX)=PARTSAV(6,MX) + XMAS(I)
                ENDIF
              ENDDO
            ELSE
#include "vectorize.inc"
              DO I=JST(II),JST(II+1)-1
                IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
                  IF(OFF(I)/=ZERO)THEN
                    PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
                    PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
                    PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
                    PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
                    PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
                  ENDIF
                ELSE 
                  PARTSAV(1,MX)=PARTSAV(1,MX) + EI(I)
                  PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
                  PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
                  PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
                  PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
                ENDIF
                IF(OFF(I)/=ZERO)THEN
                  PARTSAV(6,MX)=PARTSAV(6,MX) + XMAS(I)
                ENDIF
              ENDDO
            ENDIF ! IF (JST(II+1)-JST(II)>15)
          ENDDO
        ENDIF ! IF (IC==1)
      ENDIF ! IF(NPSAV>=21)
C
      IF(IEXPAN > 0) THEN
         DO I=JFT,JLT
            MX = IPARTTG(I)
            IF(OFF(I)/=0.0)THEN
               PARTSAV(27,MX)=PARTSAV(27,MX) + EINTTH(I)
            ENDIF
         ENDDO
       ENDIF
C
      DO I = JFT,JLT
        MX = IPARTTG(I)
        IF (OFF(I)==ZERO) THEN 
          PARTSAV(25,MX) = PARTSAV(25,MX) + ONE
        ENDIF
      ENDDO
C 
      CALL SENSOR_ENERGY_BILAN(JFT,JLT,EI,EK,OFF,IPARTTG,ITASK) 
C---
      RETURN
      END
