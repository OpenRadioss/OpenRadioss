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
Chd|  S10BILAN                      source/elements/solid/solide10/s10bilan.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|====================================================================
      SUBROUTINE S10BILAN(PARTSAV,EINT,RHO,RK,VOL,
     .   VX, VY, VZ, NX, VNEW,IPARTS,
     .   GRESAV,GRTH,IGRTH,IEXPAN,EINTTH,
     .   FILL  ,X, Y, Z,ITASK,IPARG)
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
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .  PARTSAV(NPSAV,*), EINT(*), RHO(*), RK(*), VOL(*),
     .  VX(MVSIZ,10),VY(MVSIZ,10),VZ(MVSIZ,10),
     .  NX(MVSIZ,10),VNEW(*),GRESAV(*),EINTTH(*),
     .  FILL(*),
     .  X(MVSIZ,10),Y(MVSIZ,10),Z(MVSIZ,10)
      INTEGER IEXPAN,ITASK,
     .  IPARTS(*),GRTH(*),IGRTH(*),IPARG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, M,N, FLAG
C     REAL ou REAL*8
      my_real
     .   X1,Y1,Z1,VXM,VYM,VZM,
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
     .   REI(MVSIZ), REK(MVSIZ), FAC, AM, BM, CM,
     .   OFF_L(MVSIZ)
      INTEGER IPERM1(10),IPERM2(10),N1,N2
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
C-----------------------------------------------
      FLAG = IPARG(80) ! Flag pour sorties additionnelles
C-----------------------------------------------
C     CALCUL BILAN SORTIES DEF 
C-----------------------------------------------
      FAC=TQUATORZE
C
      IF(ISROT == 0)THEN
        AM = FAC/(QUATRE*FAC + SIX)
        BM = UN/(QUATRE*FAC + SIX)
        CM = FAC
      ELSEIF(ISROT == 1)THEN
        AM = UN/QUATRE
        BM = ZERO
      ELSEIF(ISROT == 2)THEN
        AM = UN/QUATRE
        BM = ZERO
      ENDIF
C
      N = 1
      DO I=LFT,LLT
        VXA(I)=VX(I,N)
        VYA(I)=VY(I,N)
        VZA(I)=VZ(I,N)
        VA2(I)=(VX(I,N)**2+VY(I,N)**2+VZ(I,N)**2)
      ENDDO
C
      DO N=2,4
        DO I=LFT,LLT
          VXA(I)=VXA(I)+VX(I,N)
          VYA(I)=VYA(I)+VY(I,N)
          VZA(I)=VZA(I)+VZ(I,N)
          VA2(I)=VA2(I)+(VX(I,N)**2+VY(I,N)**2+VZ(I,N)**2)
        ENDDO
      ENDDO
C
      IF(ISROT == 0) THEN
        DO I=LFT,LLT
          VXA(I)=VXA(I)*CM
          VYA(I)=VYA(I)*CM
          VZA(I)=VZA(I)*CM
          VA2(I)=VA2(I)*CM
        ENDDO
        DO N=5,10
          DO I=LFT,LLT
            VXA(I)=VXA(I)+VX(I,N)
            VYA(I)=VYA(I)+VY(I,N)
            VZA(I)=VZA(I)+VZ(I,N)
            VA2(I)=VA2(I)+(VX(I,N)**2+VY(I,N)**2+VZ(I,N)**2)
          ENDDO
        ENDDO
        DO I=LFT,LLT
          VXA(I)=VXA(I)*BM
          VYA(I)=VYA(I)*BM
          VZA(I)=VZA(I)*BM
          VA2(I)=VA2(I)*BM
        ENDDO
      ELSE
        DO I=LFT,LLT
          VXA(I)=VXA(I)*AM
          VYA(I)=VYA(I)*AM
          VZA(I)=VZA(I)*AM
          VA2(I)=VA2(I)*AM
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        XMAS(I)=FILL(I)*RHO(I)*VNEW(I)
        EI(I)  = FILL(I)*EINT(I)*VOL(I)
        EK(I)  = XMAS(I)*VA2(I)*UNDEMI
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
C
C-----------------------------------------------
C     CALCUL BILAN SORTIES ADDITIONNELLES 
C-----------------------------------------------
      IF(FLAG.EQ.1) THEN
        N = 1
        DO I=LFT,LLT
          XX(I) =X(I,N)
          YY(I) =Y(I,N)
          ZZ(I) =Z(I,N)
          XY(I) =X(I,N)*Y(I,N)
          YZ(I) =Y(I,N)*Z(I,N)
          ZX(I) =Z(I,N)*X(I,N)
          XX2(I)=(X(I,N)**2)
          YY2(I)=(Y(I,N)**2)
          ZZ2(I)=(Z(I,N)**2)
        ENDDO
C
        DO N=2,4
          DO I=LFT,LLT
            XX(I) =XX(I)+X(I,N)
            YY(I) =YY(I)+Y(I,N)
            ZZ(I) =ZZ(I)+Z(I,N)
            XY(I) =XY(I)+X(I,N)*Y(I,N)
            YZ(I) =YZ(I)+Y(I,N)*Z(I,N)
            ZX(I) =ZX(I)+Z(I,N)*X(I,N)
            XX2(I)=XX2(I)+(X(I,N)**2)
            YY2(I)=YY2(I)+(Y(I,N)**2)
            ZZ2(I)=ZZ2(I)+(Z(I,N)**2)
          ENDDO
        ENDDO
C
        IF(ISROT == 0) THEN
          DO I=LFT,LLT
            XX(I) =XX(I)*CM
            YY(I) =YY(I)*CM
            ZZ(I) =ZZ(I)*CM
            XY(I) =XY(I)*CM
            YZ(I) =YZ(I)*CM
            ZX(I) =ZX(I)*CM
            XX2(I)=XX2(I)*CM
            YY2(I)=YY2(I)*CM
            ZZ2(I)=ZZ2(I)*CM
          ENDDO
          DO N=5,10
            DO I=LFT,LLT
              XX(I) =XX(I)+X(I,N)
              YY(I) =YY(I)+Y(I,N)
              ZZ(I) =ZZ(I)+Z(I,N)
              XY(I) =XY(I)+X(I,N)*Y(I,N)
              YZ(I) =YZ(I)+Y(I,N)*Z(I,N)
              ZX(I) =ZX(I)+Z(I,N)*X(I,N)
              XX2(I)=XX2(I)+(X(I,N)**2)
              YY2(I)=YY2(I)+(Y(I,N)**2)
              ZZ2(I)=ZZ2(I)+(Z(I,N)**2)
            ENDDO
          ENDDO
          DO I=LFT,LLT
            XX(I) =XX(I)*BM
            YY(I) =YY(I)*BM
            ZZ(I) =ZZ(I)*BM
            XY(I) =XY(I)*BM
            YZ(I) =YZ(I)*BM
            ZX(I) =ZX(I)*BM
            XX2(I)=XX2(I)*BM
            YY2(I)=YY2(I)*BM
            ZZ2(I)=ZZ2(I)*BM
          ENDDO
        ELSE
          DO I=LFT,LLT
            XX(I) =XX(I)*AM
            YY(I) =YY(I)*AM
            ZZ(I) =ZZ(I)*AM
            XY(I) =XY(I)*AM
            YZ(I) =YZ(I)*AM
            ZX(I) =ZX(I)*AM
            XX2(I)=XX2(I)*AM
            YY2(I)=YY2(I)*AM
            ZZ2(I)=ZZ2(I)*AM
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
C-----------------------------------------------
      IF (IGRE /= 0) THEN
          DO I=LFT,LLT
            OFF(I)     = UN
          ENDDO
          CALL GRELEM_SAV(LFT   ,LLT   ,GRESAV,IGRTH ,GRTH  ,
     2                    OFF   ,EI    ,EK    ,XM    ,YM    ,
     3                    ZM    ,XMAS  ,XCG   ,YCG   ,ZCG   ,
     4                    XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                    IZZ   ,IXY   ,IYZ   ,IZX   ,REI   ,
     6                    REK   ,FLAG  )
      ENDIF
C-----------------------------------------------
C
      IF(JTUR.NE.0)THEN
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

      OFF_L(LFT:LLT) = UN
      CALL SENSOR_ENERGY_BILAN(LFT,LLT,EI,EK,OFF_L,IPARTS,ITASK)  
C
      RETURN
      END
