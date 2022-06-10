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
Chd|  S4BILAN                       source/elements/solid/solide4/s4bilan.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|====================================================================
      SUBROUTINE S4BILAN(PARTSAV,EINT,RHO,RK,VOL,
     .   VX1, VX2, VX3, VX4, VY1, VY2, VY3, VY4, 
     .   VZ1, VZ2, VZ3, VZ4, VNEW,IPARTS,GRESAV,
     .   GRTH,IGRTH,IEXPAN,EINTTH,FILL ,
     .   X1, X2, X3, X4, Y1, Y2, Y3, Y4, 
     .   Z1, Z2, Z3, Z4,ITASK,IPARG)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .  PARTSAV(NPSAV,*), 
     .  EINT(*), RHO(*), RK(*), VOL(*),
     .  VX1(*), VX2(*), VX3(*), VX4(*), 
     .  VY1(*), VY2(*), VY3(*), VY4(*), 
     .  VZ1(*), VZ2(*), VZ3(*), VZ4(*), 
     .  VNEW(*),GRESAV(*),EINTTH(*), FILL(*),
     .  X1(*), X2(*), X3(*), X4(*), 
     .  Y1(*), Y2(*), Y3(*), Y4(*), 
     .  Z1(*), Z2(*), Z3(*), Z4(*)
      INTEGER IEXPAN,ITASK,
     .  IPARTS(*),GRTH(*),IGRTH(*),IPARG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, M,FLAG,NXS1,NXS2,NXS3,NXS4
C     REAL
      my_real
     .   VXA(MVSIZ), VYA(MVSIZ) , VZA(MVSIZ), 
     .   VA2(MVSIZ), XMAS(MVSIZ), 
     .   OFF(MVSIZ), EI(MVSIZ) , EK(MVSIZ),
     .   XM(MVSIZ) , YM(MVSIZ) , ZM(MVSIZ),
     .   XX(MVSIZ) , YY(MVSIZ) , ZZ(MVSIZ),
     .   XY(MVSIZ) , YZ(MVSIZ) , ZX(MVSIZ),
     .   XX2(MVSIZ), YY2(MVSIZ), ZZ2(MVSIZ),
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
        VXA(I)=VX1(I)+VX2(I)+VX3(I)+VX4(I)
        VYA(I)=VY1(I)+VY2(I)+VY3(I)+VY4(I)
        VZA(I)=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)
        VA2(I)=VX1(I)**2+VX2(I)**2+VX3(I)**2+VX4(I)**2
     +        +VY1(I)**2+VY2(I)**2+VY3(I)**2+VY4(I)**2
     +        +VZ1(I)**2+VZ2(I)**2+VZ3(I)**2+VZ4(I)**2
      ENDDO
C
      DO I=LFT,LLT
        VXA(I) = VXA(I)*FOURTH
        VYA(I) = VYA(I)*FOURTH
        VZA(I) = VZA(I)*FOURTH
        VA2(I) = VA2(I)*FOURTH
      ENDDO
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
      DO I=LFT,LLT
        M=IPARTS(I)
        PARTSAV(1,M)=PARTSAV(1,M) + EI(I)
        PARTSAV(2,M)=PARTSAV(2,M) + EK(I)
        PARTSAV(3,M)=PARTSAV(3,M) + XM(I)
        PARTSAV(4,M)=PARTSAV(4,M) + YM(I)
        PARTSAV(5,M)=PARTSAV(5,M) + ZM(I)
        PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
      ENDDO 
C-----------------------------------------------
C     CALCUL BILAN SORTIES ADDITIONNELLES 
C-----------------------------------------------
      IF(FLAG.EQ.1) THEN
        DO I=LFT,LLT      
          XX(I)= X1(I)+X2(I)+X3(I)+X4(I)
          YY(I)= Y1(I)+Y2(I)+Y3(I)+Y4(I)
          ZZ(I)= Z1(I)+Z2(I)+Z3(I)+Z4(I)
          XX2(I)= X1(I)**2+X2(I)**2+X3(I)**2+X4(I)**2
          YY2(I)= Y1(I)**2+Y2(I)**2+Y3(I)**2+Y4(I)**2
          ZZ2(I)= Z1(I)**2+Z2(I)**2+Z3(I)**2+Z4(I)**2
          XY(I)= X1(I)*Y1(I)+X2(I)*Y2(I)+X3(I)*Y3(I)+X4(I)*Y4(I)
          YZ(I)= Y1(I)*Z1(I)+Y2(I)*Z2(I)+Y3(I)*Z3(I)+Y4(I)*Z4(I)
          ZX(I)= Z1(I)*X1(I)+Z2(I)*X2(I)+Z3(I)*X3(I)+Z4(I)*X4(I)
        ENDDO
C
        DO I=LFT,LLT
          XX(I) = XX(I)*FOURTH
          YY(I) = YY(I)*FOURTH
          ZZ(I) = ZZ(I)*FOURTH
          XY(I) = XY(I)*FOURTH
          YZ(I) = YZ(I)*FOURTH
          ZX(I) = ZX(I)*FOURTH
          XX2(I)= XX2(I)*FOURTH
          YY2(I)= YY2(I)*FOURTH
          ZZ2(I)= ZZ2(I)*FOURTH
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
        ENDDO 
      ENDIF
C
C-----------------------------------------------
      IF (IGRE /= 0) THEN
        DO I=LFT,LLT
          OFF(I) = ONE
        ENDDO
        CALL GRELEM_SAV(LFT   ,LLT   ,GRESAV,IGRTH ,GRTH  ,
     2                  OFF   ,EI    ,EK    ,XM    ,YM    ,
     3                  ZM    ,XMAS  ,XCG   ,YCG   ,ZCG   ,
     4                  XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                  IZZ   ,IXY   ,IYZ   ,IZX   ,REI   ,
     6                  REK   ,FLAG)
      ENDIF
C-----------------------------------------------
C
      IF(JTUR.NE.0)THEN
       DO 50 I=LFT,LLT
       M=IPARTS(I)
       PARTSAV(7,M)=PARTSAV(7,M) + RK(I)*VOL(I)
   50  CONTINUE
      ENDIF
C
      IF(IEXPAN.NE.0)THEN
        DO I=LFT,LLT
         M=IPARTS(I)
        PARTSAV(27,M)=PARTSAV(27,M) + EINTTH(I)*VOL(I)
        ENDDO
      ENDIF

      OFF_L(LFT:LLT) = ONE
      CALL SENSOR_ENERGY_BILAN(LFT,LLT,EI,EK,OFF_L,IPARTS,ITASK) 
C
      RETURN
      END
