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
Chd|  R5BILAN                       source/elements/spring/r5bilan.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|====================================================================
      SUBROUTINE R5BILAN(
     1   EINT,    PARTSAV, IXR,     UMAS,
     2   V,       IPARTR,  UINER,   X,
     3   VR,      GRESAV,  GRTH,    IGRTH,
     4   NC1,     NC2,     ITASK,   IAD,
     5   IGRE,    NFT,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: IAD
      INTEGER, INTENT(IN) :: IGRE
      INTEGER, INTENT(IN) :: NFT
      INTEGER IPARTR(*),IXR(NIXR,*),GRTH(*),IGRTH(*),
     .        NC1(*),NC2(*),ITASK
C     REAL
      my_real
     .   UMAS(*),EINT(*),PARTSAV(NPSAV,*),V(3,*),VR(3,*),X(3,*),
     .   UINER(MVSIZ),GRESAV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX, FLAG
      my_real
     .   REINTT 
      my_real
     .   X1(MVSIZ), Y1(MVSIZ), Z1(MVSIZ),
     .   X2(MVSIZ), Y2(MVSIZ), Z2(MVSIZ),
     .   V1(MVSIZ,3),V2(MVSIZ,3),
     .   VR1(MVSIZ,3),VR2(MVSIZ,3),
     .   IN2,EI(MVSIZ),REK(MVSIZ),
     .   EK(MVSIZ), XM(MVSIZ), YM(MVSIZ), ZM(MVSIZ), XMAS2(MVSIZ),
     .   XXM(MVSIZ), YYM(MVSIZ), ZZM(MVSIZ), 
     .   XCG(MVSIZ), YCG(MVSIZ), ZCG(MVSIZ),
     .   IXX(MVSIZ), IYY(MVSIZ), IZZ(MVSIZ),
     .   IXY(MVSIZ), IYZ(MVSIZ), IZX(MVSIZ),RBIDON(1),OFF(MVSIZ),
     .   XX(MVSIZ) , YY(MVSIZ) , ZZ(MVSIZ) ,
     .   XY(MVSIZ) , YZ(MVSIZ) , ZX(MVSIZ) ,
     .   XX2(MVSIZ), YY2(MVSIZ), ZZ2(MVSIZ),
     .   VXA(MVSIZ), VYA(MVSIZ), VZA(MVSIZ),
     .   VA2(MVSIZ), VRA2(MVSIZ)
C-----------------------------------------------
      FLAG = 0
      RBIDON = ZERO
      OFF(1:MVSIZ) = ZERO
      REINTT = ZERO
C
      DO I=1,NEL
        REINTT=REINTT + EINT(I)
      ENDDO
C
#include "atomic.inc"
        REINT = REINT + REINTT
#include "atomend.inc"
C
      DO I=1,NEL
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        V1(I,1)=V(1,NC1(I))
        V1(I,2)=V(2,NC1(I))
        V1(I,3)=V(3,NC1(I))
        V2(I,1)=V(1,NC2(I))
        V2(I,2)=V(2,NC2(I))
        V2(I,3)=V(3,NC2(I))
        VR1(I,1)=VR(1,NC1(I))
        VR1(I,2)=VR(2,NC1(I))
        VR1(I,3)=VR(3,NC1(I))
        VR2(I,1)=VR(1,NC2(I))
        VR2(I,2)=VR(2,NC2(I))
        VR2(I,3)=VR(3,NC2(I))
      ENDDO

      DO I=1,NEL
        XX(I)= X1(I)+X2(I)
        YY(I)= Y1(I)+Y2(I)
        ZZ(I)= Z1(I)+Z2(I)
        XX2(I) = X1(I)*X1(I)+X2(I)*X2(I)
        YY2(I) = Y1(I)*Y1(I)+Y2(I)*Y2(I)
        ZZ2(I) = Z1(I)*Z1(I)+Z2(I)*Z2(I)
        XY(I)  = X1(I)*Y1(I)+X2(I)*Y2(I)
        YZ(I)  = Y1(I)*Z1(I)+Y2(I)*Z2(I)
        ZX(I)  = Z1(I)*X1(I)+Z2(I)*X2(I)
        VXA(I)=V1(I,1)+V2(I,1)
        VYA(I)=V1(I,2)+V2(I,2)
        VZA(I)=V1(I,3)+V2(I,3)
        VA2(I)=V1(I,1)*V1(I,1)+V2(I,1)*V2(I,1)
     .        +V1(I,2)*V1(I,2)+V2(I,2)*V2(I,2)
     .        +V1(I,3)*V1(I,3)+V2(I,3)*V2(I,3)
        VRA2(I) = VR1(I,1)*VR1(I,1)+VR2(I,1)*VR2(I,1)
     .           +VR1(I,2)*VR1(I,2)+VR2(I,2)*VR2(I,2)
     .           +VR1(I,3)*VR1(I,3)+VR2(I,3)*VR2(I,3)
      ENDDO
C
      DO I=1,NEL
        XMAS2(I)=UMAS(I)*HALF
        XM(I)= XMAS2(I)*VXA(I)
        YM(I)= XMAS2(I)*VYA(I)
        ZM(I)= XMAS2(I)*VZA(I)
        EK(I)= XMAS2(I)*VA2(I)*HALF
      ENDDO
C
      IF (NPSAV >= 21) THEN
        DO I=1,NEL
          XCG(I)= XMAS2(I)*XX(I)
          YCG(I)= XMAS2(I)*YY(I)
          ZCG(I)= XMAS2(I)*ZZ(I)
          IN2 = UINER(I)*HALF
          IXY(I) = -XMAS2(I)*XY(I)
          IYZ(I) = -XMAS2(I)*YZ(I)
          IZX(I) = -XMAS2(I)*ZX(I)
          IXX(I) = UINER(I) + XMAS2(I)*(YY2(I) + ZZ2(I))
          IYY(I) = UINER(I) + XMAS2(I)*(ZZ2(I) + XX2(I))
          IZZ(I) = UINER(I) + XMAS2(I)*(XX2(I) + YY2(I))
C
          XXM(I)= XMAS2(I)*
     .      (V1(I,3)*Y1(I)-V1(I,2)*Z1(I)
     .      +V2(I,3)*Y2(I)-V2(I,2)*Z2(I))
     .           +IN2*(VR1(I,1)+VR2(I,1))
          YYM(I)= XMAS2(I)*
     .      (V1(I,1)*Z1(I)-V1(I,3)*X1(I)
     .      +V2(I,1)*Z2(I)-V2(I,3)*X2(I))
     .           +IN2*(VR1(I,2)+VR2(I,2))
          ZZM(I)= XMAS2(I)*
     .      (V1(I,2)*X1(I)-V1(I,1)*Y1(I)
     .      +V2(I,2)*X2(I)-V2(I,1)*Y2(I))
     .           +IN2*(VR1(I,3)+VR2(I,3))
          REK(I)= IN2*VRA2(I)*HALF
        ENDDO
C
      IF (IGRE /= 0) THEN
          DO I=1,NEL
            OFF(I)     = ONE
          ENDDO
          CALL GRELEM_SAV(1   ,NEL   ,GRESAV,IGRTH ,GRTH  ,
     2                    OFF   ,EINT  ,EK    ,XM    ,YM    ,
     3                    ZM    ,UMAS  ,XCG   ,YCG   ,ZCG   ,
     4                    XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                    IZZ   ,IXY   ,IYZ   ,IZX   ,RBIDON,
     6                    REK   ,FLAG  )
      ENDIF
C
        DO I=1,NEL
          MX = IPARTR(I)
          PARTSAV(1,MX)=PARTSAV(1,MX) + EINT(I)
          PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
          PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
          PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
          PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
          PARTSAV(6,MX)=PARTSAV(6,MX) + UMAS(I)
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
C COQUE          PARTSAV(21,MX)=PARTSAV(21,MX) + REI(I)
          PARTSAV(22,MX)=PARTSAV(22,MX) + REK(I)
        ENDDO
      ENDIF
      OFF(1:MVSIZ) = ONE
      CALL SENSOR_ENERGY_BILAN(1,NEL,EINT,EK,OFF,IPARTR,ITASK)  
C---
      RETURN
      END
