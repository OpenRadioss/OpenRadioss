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
Chd|  RBILAN                        source/elements/spring/rbilan.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|====================================================================
      SUBROUTINE RBILAN(EINT   ,PARTSAV,IXR   ,GEO ,V    ,
     2                  IPARTR ,AL0    ,GRESAV,GRTH,IGRTH,
     3                  OFF_DUM,NC1    ,NC2   ,X   ,VR   ,ITASK)
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
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARTR(*),IXR(NIXR,*),GRTH(*),IGRTH(*),
     .        NC1(*),NC2(*),ITASK
C     REAL
      my_real
     .   GEO(NPROPG,*),EINT(*),PARTSAV(NPSAV,*),V(3,*), AL0(*),
     .   GRESAV(*),OFF_DUM(*),X(3,*),VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J, ILENG, FLAG
      my_real
     .   X1(MVSIZ), Y1(MVSIZ), Z1(MVSIZ),
     .   X2(MVSIZ), Y2(MVSIZ), Z2(MVSIZ),
     .   V1(MVSIZ,3),V2(MVSIZ,3),
     .   VR1(MVSIZ,3),VR2(MVSIZ,3),
     .   XMAS2(MVSIZ), REINTT,
     .   XMAS(MVSIZ), RHO(MVSIZ), INEL,
     .   EK(MVSIZ), XM(MVSIZ), YM(MVSIZ), ZM(MVSIZ),
     .   VA1,XM1PHI,XM2PHI,RBIDON(1),OFF(MVSIZ),
     .   IN2, EI(MVSIZ),REK(MVSIZ),
     .   XXM(MVSIZ), YYM(MVSIZ), ZZM(MVSIZ), 
     .   XCG(MVSIZ), YCG(MVSIZ), ZCG(MVSIZ),
     .   IXX(MVSIZ), IYY(MVSIZ), IZZ(MVSIZ),
     .   IXY(MVSIZ), IYZ(MVSIZ), IZX(MVSIZ),
     .   XX(MVSIZ) , YY(MVSIZ) , ZZ(MVSIZ) ,
     .   XY(MVSIZ) , YZ(MVSIZ) , ZX(MVSIZ) ,
     .   XX2(MVSIZ), YY2(MVSIZ), ZZ2(MVSIZ),
     .   VXA(MVSIZ), VYA(MVSIZ), VZA(MVSIZ),
     .   VA2(MVSIZ), VRA2(MVSIZ)
C-----------------------------------------------
      REINTT = ZERO
      FLAG = 0
      RBIDON = ZERO
      OFF    = ZERO
      DO I=LFT,LLT
        REINTT=REINTT + EINT(I)
      ENDDO
C
#include "atomic.inc"
        REINT = REINT + REINTT
#include "atomend.inc"
C
      DO I=LFT,LLT
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
c
      DO I=LFT,LLT
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
      DO I=LFT,LLT
        ILENG=NINT(GEO(93,IXR(1,I)))
        IF (ILENG /= 0) THEN
          XMAS(I)=GEO(1,IXR(1,I))*AL0(I)
        ELSE
          XMAS(I)=GEO(1,IXR(1,I))
        ENDIF
        XMAS2(I)=XMAS(I)*HALF
        XM(I)= XMAS2(I)*VXA(I)
        YM(I)= XMAS2(I)*VYA(I)
        ZM(I)= XMAS2(I)*VZA(I)
        EK(I)= XMAS2(I)*VA2(I)*HALF
      ENDDO
C
      DO I=LFT,LLT
        XCG(I)= XMAS2(I)*XX(I)
        YCG(I)= XMAS2(I)*YY(I)
        ZCG(I)= XMAS2(I)*ZZ(I)
        ILENG=NINT(GEO(93,IXR(1,I)))
        IF (ILENG /= 0) THEN
          INEL=GEO(9,IXR(1,I))*AL0(I)
        ELSE
          INEL=GEO(9,IXR(1,I))
        ENDIF
        IN2 = INEL*HALF
        IXY(I) = -XMAS2(I)*XY(I)
        IYZ(I) = -XMAS2(I)*YZ(I)
        IZX(I) = -XMAS2(I)*ZX(I)
        IXX(I)= INEL + XMAS2(I)*(YY2(I) + ZZ2(I))
        IYY(I)= INEL + XMAS2(I)*(ZZ2(I) + XX2(I))
        IZZ(I)= INEL + XMAS2(I)*(XX2(I) + YY2(I))
C
        XXM(I)= XMAS2(I)*
     .    (V1(I,3)*Y1(I)-V1(I,2)*Z1(I)
     .    +V2(I,3)*Y2(I)-V2(I,2)*Z2(I))
     .          +IN2*(VR1(I,1)+VR2(I,1))
        YYM(I)= XMAS2(I)*
     .    (V1(I,1)*Z1(I)-V1(I,3)*X1(I)
     .    +V2(I,1)*Z2(I)-V2(I,3)*X2(I))
     .         +IN2*(VR1(I,2)+VR2(I,2))
        ZZM(I)= XMAS2(I)*
     .    (V1(I,2)*X1(I)-V1(I,1)*Y1(I)
     .    +V2(I,2)*X2(I)-V2(I,1)*Y2(I))
     .         +IN2*(VR1(I,3)+VR2(I,3))
        REK(I)= IN2*VRA2(I)*HALF
      ENDDO
C
      IF (IGRE /= 0) THEN
          DO I=LFT,LLT
            OFF(I) = ONE
          ENDDO
          CALL GRELEM_SAV(LFT   ,LLT   ,GRESAV,IGRTH ,GRTH  ,
     2                    OFF   ,EINT  ,EK    ,XM    ,YM    ,
     3                    ZM    ,XMAS  ,XCG   ,YCG   ,ZCG   ,
     4                    XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                    IZZ   ,IXY   ,IYZ   ,IZX   ,RBIDON,
     6                    REK   ,FLAG  )
      ENDIF
C
      DO I=LFT,LLT
         MX = IPARTR(I)
         PARTSAV(1,MX)=PARTSAV(1,MX) + EINT(I)
         PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
         PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
         PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
         PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
         IF (OFF_DUM(I) > ZERO) PARTSAV(6,MX)=PARTSAV(6,MX) + XMAS(I)
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
         PARTSAV(22,MX)=PARTSAV(22,MX) + REK(I)
      ENDDO

      OFF(1:MVSIZ) = ONE
      CALL SENSOR_ENERGY_BILAN(LFT,LLT,EINT,EK,OFF,IPARTR,ITASK) 
C
      RETURN
      END
