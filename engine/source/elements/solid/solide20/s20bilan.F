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
Chd|  S20BILAN                      source/elements/solid/solide20/s20bilan.F
Chd|-- called by -----------
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|====================================================================
      SUBROUTINE S20BILAN(PARTSAV,EINTG,RHOG, VOLG,
     .   VX, VY, VZ,IPARTS, VOL0G,
     .   GRESAV,GRTH,IGRTH,IEXPAN,EINTTH,
     .   FILL ,X, Y, Z,ITASK,IPARG,OFFG)
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
      INTEGER IEXPAN,
     .  IPARTS(*),GRTH(*),IGRTH(*),IPARG(*),ITASK
C     REAL
      my_real
     .  PARTSAV(NPSAV,*), EINTG(*), RHOG(*), 
     .  VX(MVSIZ,*),VY(MVSIZ,*),VZ(MVSIZ,*),
     .  VOLG(*),VOL0G(*),GRESAV(*),EINTTH(*),
     .  FILL(*),
     .  X(MVSIZ,*),Y(MVSIZ,*),Z(MVSIZ,*)
      my_real, INTENT(IN) :: OFFG(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, M,N, FLAG
C     REAL
      my_real
     .   X1, X2, X3, V1, V2, V3, FAC,
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
      N=1
      DO I=LFT,LLT
        V1=VX(I,N)
        V2=VY(I,N)
        V3=VZ(I,N)
        VXA(I)=V1
        VYA(I)=V2
        VZA(I)=V3
        VA2(I)=(V1**2+V2**2+V3**2)	  
      ENDDO
C
      DO N=2,8
        DO I=LFT,LLT
          V1=VX(I,N)
          V2=VY(I,N)
          V3=VZ(I,N)
          VXA(I)=VXA(I)+V1
          VYA(I)=VYA(I)+V2
          VZA(I)=VZA(I)+V3
          VA2(I)=VA2(I)+(V1**2+V2**2+V3**2)	  
        ENDDO
      ENDDO
C
      FAC = THREE/FOURTEEN
      DO I=LFT,LLT
        VXA(I)=VXA(I)*FAC
        VYA(I)=VYA(I)*FAC
        VZA(I)=VZA(I)*FAC
        VA2(I)=VA2(I)*FAC  
      ENDDO
C
      DO N=9,20
        DO I=LFT,LLT
          V1=VX(I,N)
          V2=VY(I,N)
          V3=VZ(I,N)
          VXA(I)=VXA(I)+V1
          VYA(I)=VYA(I)+V2
          VZA(I)=VZA(I)+V3
          VA2(I)=VA2(I)+(V1**2+V2**2+V3**2)
        ENDDO
      ENDDO
C
      FAC = SEVEN/EIGHTY16
      DO I=LFT,LLT
        VXA(I)=VXA(I)*FAC
        VYA(I)=VYA(I)*FAC
        VZA(I)=VZA(I)*FAC
        VA2(I)=VA2(I)*FAC  
      ENDDO
C
      DO I=LFT,LLT
        XMAS(I)= FILL(I)*RHOG(I)*VOLG(I)
        EI(I)  = FILL(I)*EINTG(I)*VOL0G(I)
        EK(I)  = XMAS(I)*VA2(I)*HALF
        XM(I)  = XMAS(I)*VXA(I)
        YM(I)  = XMAS(I)*VYA(I)
        ZM(I)  = XMAS(I)*VZA(I)
      ENDDO
C
      DO I=LFT,LLT
        M=IPARTS(I)
        PARTSAV(1,M)=PARTSAV(1,M) + FILL(I)*EINTG(I)*VOL0G(I)
        PARTSAV(2,M)=PARTSAV(2,M) + XMAS(I)*VA2(I)*HALF
        PARTSAV(3,M)=PARTSAV(3,M) + XMAS(I)*VXA(I)
        PARTSAV(4,M)=PARTSAV(4,M) + XMAS(I)*VYA(I)
        PARTSAV(5,M)=PARTSAV(5,M) + XMAS(I)*VZA(I)
        IF (OFFG(I) >= ONE) PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
      ENDDO
C
      IF(IEXPAN/=0)THEN
        DO I=LFT,LLT
         M=IPARTS(I)
        PARTSAV(27,M)=PARTSAV(27,M) + EINTTH(I)*VOL0G(I)
        ENDDO
      ENDIF
C
C-----------------------------------------------
C     CALCUL BILAN SORTIES ADDITIONNELLES 
C-----------------------------------------------
      IF(FLAG==1) THEN
        N=1
        DO I=LFT,LLT
          X1=X(I,N)
          X2=Y(I,N)
          X3=Z(I,N)
          XX(I)=X1
          YY(I)=X2
          ZZ(I)=X3
          XX2(I)=(X1**2)
          YY2(I)=(X2**2)
          ZZ2(I)=(X3**2)
          XY(I)=(X1*X2)
          YZ(I)=(X2*X3)
          ZX(I)=(X3*X1)
        ENDDO
        DO N=2,8
          DO I=LFT,LLT
            X1=X(I,N)
            X2=Y(I,N)
            X3=Z(I,N)
            XX(I)=XX(I)+X1
            YY(I)=YY(I)+X2
            ZZ(I)=ZZ(I)+X3
            XX2(I)=XX2(I)+(X1**2)
            YY2(I)=YY2(I)+(X2**2)
            ZZ2(I)=ZZ2(I)+(X3**2)
            XY(I)=XY(I)+(X1*X2)
            YZ(I)=YZ(I)+(X2*X3)
            ZX(I)=ZX(I)+(X3*X1)
          ENDDO
        ENDDO
C
        FAC = THREE/FOURTEEN
        DO I=LFT,LLT
          XX(I)=XX(I)*FAC
          YY(I)=YY(I)*FAC
          ZZ(I)=ZZ(I)*FAC
          XX2(I)=XX2(I)*FAC
          YY2(I)=YY2(I)*FAC
          ZZ2(I)=ZZ2(I)*FAC
          XY(I)=XY(I)*FAC
          YZ(I)=YZ(I)*FAC
          ZX(I)=ZX(I)*FAC
        ENDDO
C
        DO N=9,20
          DO I=LFT,LLT
            X1=X(I,N)
            X2=Y(I,N)
            X3=Z(I,N)
            XX(I)=XX(I)+X1
            YY(I)=YY(I)+X2
            ZZ(I)=ZZ(I)+X3
            XX2(I)=XX2(I)+(X1**2)
            YY2(I)=YY2(I)+(X2**2)
            ZZ2(I)=ZZ2(I)+(X3**2)
            XY(I)=XY(I)+(X1*X2)
            YZ(I)=YZ(I)+(X2*X3)
            ZX(I)=ZX(I)+(X3*X1)
          ENDDO
        ENDDO
C
        FAC = SEVEN/EIGHTY16
        DO I=LFT,LLT
          XX(I)=XX(I)*FAC
          YY(I)=YY(I)*FAC
          ZZ(I)=ZZ(I)*FAC
          XX2(I)=XX2(I)*FAC
          YY2(I)=YY2(I)*FAC
          ZZ2(I)=ZZ2(I)*FAC
          XY(I)=XY(I)*FAC
          YZ(I)=YZ(I)*FAC
          ZX(I)=ZX(I)*FAC
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
            OFF(I)     = ONE
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
      OFF_L(LFT:LLT) = ONE
C
      DO I = LFT,LLT
        M=IPARTS(I)
        IF (OFFG(I) < ONE) THEN 
          PARTSAV(25,M) = PARTSAV(25,M) + ONE
        ENDIF
      ENDDO
C
      CALL SENSOR_ENERGY_BILAN(LFT,LLT,EI,EK,OFF_L,IPARTS,ITASK) 
C
      RETURN
      END
