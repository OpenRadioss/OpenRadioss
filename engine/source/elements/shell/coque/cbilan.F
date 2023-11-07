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
Chd|  CBILAN                        source/elements/shell/coque/cbilan.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE CBILAN(
     1   JFT,      JLT,      PM,       V,
     2   IXC,      THK,      EINT,     PARTSAV,
     3   AREA,     MAT,      IPARTC,   X,
     4   VR,       VOL0,     VOL00,    THK0,
     5   THK02,    IFLA,     OFF,      NFT1,
     6   GRESAV,   GRTH,     IGRTH,    VL1,
     7   VL2,      VL3,      VL4,      VRL1,
     8   VRL2,     VRL3,     VRL4,     X1G,
     9   X2G,      X3G,      X4G,      Y1G,
     A   Y2G,      Y3G,      Y4G,      Z1G,
     B   Z2G,      Z3G,      Z4G,      IXFEM,
     C   IEXPAN,   EINTTH,   ITASK,    GVOL,
     D   ACTIFXFEM,IGRE)
      USE SENSOR_MOD
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
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IGRE
      INTEGER IXC(NIXC,*),MAT(MVSIZ), JFT, JLT,IFLA,IEXPAN,
     .   IPARTC(*),NFT1,GRTH(*),IGRTH(*),IXFEM,ITASK
      INTEGER, INTENT(IN) :: ACTIFXFEM
C     REAL
      my_real
     .   PM(NPROPM,*), V(3,*), THK(*), EINT(JLT,2),
     .   PARTSAV(NPSAV,*), AREA(*),X(3,*),VR(3,*) ,VOL0(*),VOL00(*),
     .   THK0(*), THK02(*),OFF(*),
     .   GRESAV(*),EINTTH(*)
      my_real
     .   X1G(MVSIZ), X2G(MVSIZ), X3G(MVSIZ), X4G(MVSIZ),
     .   Y1G(MVSIZ), Y2G(MVSIZ), Y3G(MVSIZ), Y4G(MVSIZ),
     .   Z1G(MVSIZ), Z2G(MVSIZ), Z3G(MVSIZ), Z4G(MVSIZ),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),VRL4(MVSIZ,3)
      my_real, INTENT(IN) :: GVOL(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, II, J, IC, JST(MVSIZ+1),FLAG,
     .        IXCTMP2,IXCTMP3,IXCTMP4,IXCTMP5,NEL,IJK
C     REAL
      my_real
     .   IN25,XX,YY,ZZ,XY,YZ,ZX,VA2, INEL,
     .   VXA(MVSIZ), VYA(MVSIZ), VZA(MVSIZ), 
     .   XMAS(MVSIZ), RHO, EI(MVSIZ),REI(MVSIZ),REK(MVSIZ),
     .   EK(MVSIZ), XM(MVSIZ), YM(MVSIZ), ZM(MVSIZ), XMAS25(MVSIZ),
     .   XXM(MVSIZ), YYM(MVSIZ), ZZM(MVSIZ), 
     .   XCG(MVSIZ), YCG(MVSIZ), ZCG(MVSIZ),
     .   IXX(MVSIZ), IYY(MVSIZ), IZZ(MVSIZ),
     .   IXY(MVSIZ), IYZ(MVSIZ), IZX(MVSIZ),
     .   RBIDON(1)
      INTEGER :: K,IPART,LOCAL_INDEX
      my_real, DIMENSION(MVSIZ,2) :: FSTPARIT   
C-----------------------------------------------
      FLAG = 0
      RBIDON = ZERO
      
      MX = MAT(JFT)
      RHO=PM(1,MX)
C-----add available to output XCG,... with other shells than belytschko
      IF(IFLA>1)THEN
       DO I=JFT,JLT      
        IXCTMP2=IXC(2,I)
        IXCTMP3=IXC(3,I)
        IXCTMP4=IXC(4,I)
        IXCTMP5=IXC(5,I)
C----------------------------
        X1G(I)=X(1,IXCTMP2)
        Y1G(I)=X(2,IXCTMP2)
        Z1G(I)=X(3,IXCTMP2)
        X2G(I)=X(1,IXCTMP3)
        Y2G(I)=X(2,IXCTMP3)
        Z2G(I)=X(3,IXCTMP3)
        X3G(I)=X(1,IXCTMP4)
        Y3G(I)=X(2,IXCTMP4)
        Z3G(I)=X(3,IXCTMP4)
        X4G(I)=X(1,IXCTMP5)
        Y4G(I)=X(2,IXCTMP5)
        Z4G(I)=X(3,IXCTMP5)
        VL1(I,1)=V(1,IXCTMP2)
        VL1(I,2)=V(2,IXCTMP2)
        VL1(I,3)=V(3,IXCTMP2)
        VL2(I,1)=V(1,IXCTMP3)
        VL2(I,2)=V(2,IXCTMP3)
        VL2(I,3)=V(3,IXCTMP3)
        VL3(I,1)=V(1,IXCTMP4)
        VL3(I,2)=V(2,IXCTMP4)
        VL3(I,3)=V(3,IXCTMP4)
        VL4(I,1)=V(1,IXCTMP5)
        VL4(I,2)=V(2,IXCTMP5)
        VL4(I,3)=V(3,IXCTMP5)
        VRL1(I,1)=VR(1,IXCTMP2)
        VRL1(I,2)=VR(2,IXCTMP2)
        VRL1(I,3)=VR(3,IXCTMP2)
        VRL2(I,1)=VR(1,IXCTMP3)
        VRL2(I,2)=VR(2,IXCTMP3)
        VRL2(I,3)=VR(3,IXCTMP3)
        VRL3(I,1)=VR(1,IXCTMP4)
        VRL3(I,2)=VR(2,IXCTMP4)
        VRL3(I,3)=VR(3,IXCTMP4)
        VRL4(I,1)=VR(1,IXCTMP5)
        VRL4(I,2)=VR(2,IXCTMP5)
        VRL4(I,3)=VR(3,IXCTMP5)
       END DO
      END IF
C
      DO I=JFT,JLT
        VXA(I)=VL1(I,1)+VL2(I,1)+VL3(I,1)+VL4(I,1)
        VYA(I)=VL1(I,2)+VL2(I,2)+VL3(I,2)+VL4(I,2)
        VZA(I)=VL1(I,3)+VL2(I,3)+VL3(I,3)+VL4(I,3)
      ENDDO

C
      DO I=JFT,JLT
        XMAS(I)=RHO*GVOL(I)
      ENDDO
C
      DO I=JFT,JLT
        VA2 =  VL1(I,1)*VL1(I,1)+VL2(I,1)*VL2(I,1)
     2      +VL3(I,1)*VL3(I,1)+VL4(I,1)*VL4(I,1)
     3      +VL1(I,2)*VL1(I,2)+VL2(I,2)*VL2(I,2)
     4      +VL3(I,2)*VL3(I,2)+VL4(I,2)*VL4(I,2)
     5      +VL1(I,3)*VL1(I,3)+VL2(I,3)*VL2(I,3)
     6      +VL3(I,3)*VL3(I,3)+VL4(I,3)*VL4(I,3)
        EI(I)= EINT(I,1) + EINT(I,2)
        EK(I)= XMAS(I)*VA2*ONE_OVER_8
        XMAS25(I)= XMAS(I)*FOURTH
        XM(I)= XMAS25(I)*VXA(I)
        YM(I)= XMAS25(I)*VYA(I)
        ZM(I)= XMAS25(I)*VZA(I)
      ENDDO
C
C     !!!!!!!!!! VOL0,VOL00,THK0,THK02,X,VR ne sont transmis que par
C                CFORC3 et pas par CSUBC3 et CZFORC3 (IFLA=0) !!!!!!!
      IF(IFLA/=0.AND.NPSAV>=21)THEN
        DO I=JFT,JLT
         XX= X1G(I)+X2G(I)+X3G(I)+X4G(I)
         YY= Y1G(I)+Y2G(I)+Y3G(I)+Y4G(I)
         ZZ= Z1G(I)+Z2G(I)+Z3G(I)+Z4G(I)
         XCG(I)= XMAS25(I)*XX
         YCG(I)= XMAS25(I)*YY
         ZCG(I)= XMAS25(I)*ZZ
C
         IN25 = XMAS25(I)*(THK02(I)+AREA(I))*ONE_OVER_12
         INEL = FOUR*IN25
         XX = FOURTH*XX
         YY = FOURTH*YY
         ZZ = FOURTH*ZZ
         IXY(I) = -XCG(I)*YY
         IYZ(I) = -YCG(I)*ZZ
         IZX(I) = -ZCG(I)*XX
         XX = XCG(I)*XX
         YY = YCG(I)*YY
         ZZ = ZCG(I)*ZZ
         IXX(I)= INEL + YY + ZZ
         IYY(I)= INEL + ZZ + XX
         IZZ(I)= INEL + XX + YY
         VXA(I)=FOURTH*VXA(I)
         VYA(I)=FOURTH*VYA(I)
         VZA(I)=FOURTH*VZA(I)
         XXM(I)= VZA(I)*YCG(I)-VYA(I)*ZCG(I)
     .           +IN25*
     .     (VRL1(I,1)+VRL2(I,1)+VRL3(I,1)+VRL4(I,1))
          YYM(I)= VXA(I)*ZCG(I)-VZA(I)*XCG(I)
     .           +IN25*
     .     (VRL1(I,2)+VRL2(I,2)+VRL3(I,2)+VRL4(I,2))
         ZZM(I)= VYA(I)*XCG(I)-VXA(I)*YCG(I)
     .          + IN25*
     .          (VRL1(I,3)+VRL2(I,3)+VRL3(I,3)+VRL4(I,3))
         VA2 = VRL1(I,1)*VRL1(I,1)+VRL2(I,1)*VRL2(I,1)
     2        + VRL3(I,1)*VRL3(I,1)+VRL4(I,1)*VRL4(I,1)
     3        + VRL1(I,2)*VRL1(I,2)+VRL2(I,2)*VRL2(I,2)
     4        + VRL3(I,2)*VRL3(I,2)+VRL4(I,2)*VRL4(I,2)
     5        + VRL1(I,3)*VRL1(I,3)+VRL2(I,3)*VRL2(I,3)
     6        + VRL3(I,3)*VRL3(I,3)+VRL4(I,3)*VRL4(I,3)
         REI(I)= EINT(I,2)
         REK(I)= IN25*VA2*HALF
        ENDDO
C
        IF (IGRE /= 0) THEN
          FLAG = 1
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
	  IF (IPARTC(J)/=IPARTC(J-1)) THEN
	    IC=IC+1
	    JST(IC)=J
	  ENDIF
	ENDDO
	JST(IC+1)=JLT+1
        IF (IC==1) THEN
         MX = IPARTC(JFT)
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
C           PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
          ENDIF
          PARTSAV(9,MX) =PARTSAV(9,MX)  + XCG(I)
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
C                 
          DO II=1,IC
            MX=IPARTC(JST(II))
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
C                PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
               ENDIF
               PARTSAV(9,MX) =PARTSAV(9,MX)  + XCG(I)
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
              DO I=JST(II),JST(II+1)-1
               IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
                  IF (OFF(I)/=ZERO) THEN
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
C                PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
               ENDIF
               PARTSAV(9,MX) =PARTSAV(9,MX)  + XCG(I)
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
            ENDIF
          ENDDO
        ENDIF
      ELSE
        IF (IGRE /= 0) THEN
          FLAG = 0
          CALL GRELEM_SAV(JFT   ,JLT   ,GRESAV,IGRTH ,GRTH  ,
     2                    OFF   ,EI    ,EK    ,XM    ,YM    ,
     3                    ZM    ,XMAS  ,RBIDON,RBIDON,RBIDON,
     4                    RBIDON,RBIDON,RBIDON,RBIDON,RBIDON,
     5                    RBIDON,RBIDON,RBIDON,RBIDON,RBIDON,
     6                    RBIDON,FLAG)
        ENDIF	   
	IC=1
	JST(IC)=JFT
	DO J=JFT+1,JLT
	  IF (IPARTC(J)/=IPARTC(J-1)) THEN
	    IC=IC+1
	    JST(IC)=J
	  ENDIF
	ENDDO
	JST(IC+1)=JLT+1		   
        IF (IC==1) THEN
          MX = IPARTC(JFT)
          DO I=JFT,JLT
            IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
              IF (OFF(I)/=ZERO)THEN
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
C             PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
            ENDIF
          ENDDO
        ELSE
           DO II=1,IC
             MX=IPARTC(JST(II))
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
C                  PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
                 ENDIF
               ENDDO
             ELSE
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
C                  PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
                 ENDIF
               ENDDO
             ENDIF
           ENDDO
        ENDIF
      ENDIF
C
      IF(IEXPAN > 0) THEN
         DO I=JFT,JLT
            MX = IPARTC(I)
            IF(OFF(I)/=ZERO)THEN
               PARTSAV(27,MX)=PARTSAV(27,MX) + EINTTH(I)
            ENDIF
         ENDDO
      ENDIF
C
      DO I = JFT,JLT
        MX = IPARTC(I)
        IF (OFF(I)==ZERO) THEN 
          PARTSAV(25,MX) = PARTSAV(25,MX) + ONE
        ENDIF
      ENDDO
C
      CALL SENSOR_ENERGY_BILAN(JFT,JLT,EI,EK,OFF,IPARTC,ITASK)      
C---
      RETURN
      END
