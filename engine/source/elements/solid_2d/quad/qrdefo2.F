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
Chd|  QRDEFO2                       source/elements/solid_2d/quad/qrdefo2.F
Chd|-- called by -----------
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QRDEFO2(V,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  WYZ,DYZ,DZY,EYY,EZZ,ETT,EYZ,EYT,EZT,
     .                  RX,RY,RZ,SX,SY,SZ,TX,TY,TZ,
     .                  VOLN,AIRE,AIREM,
     .                  NC1,NC2,NC3,NC4,
     .                  R22 ,R23 ,R32 ,R33 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   V(3,*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VZ1(*),VZ2(*),VZ3(*),VZ4(*),
     .   PY1(*), PY2(*), PZ1(*), PZ2(*),
     .   WYZ(*), DYZ(*), DZY(*),
     .   EYY(*),EZZ(*), ETT(*), EYZ(*), EYT(*), EZT(*),
     .   RX(*),RY(*), RZ(*), SX(*), SY(*), SZ(*), TX(*), TY(*), TZ(*),
     .   VOLN(*),AIRE(*),AIREM(*),
     .   R22(*),R23(*),R32(*),R33(*)
C
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   VY13(MVSIZ), VY24(MVSIZ), VZ13(MVSIZ),VZ24(MVSIZ),
     .   YM1(MVSIZ), YM2(MVSIZ), YM3(MVSIZ), YM4(MVSIZ),
     .   ZM1(MVSIZ), ZM2(MVSIZ), ZM3(MVSIZ), ZM4(MVSIZ),
     .   YAVG(MVSIZ),A1(MVSIZ) , A2(MVSIZ), ZAVG(MVSIZ),
     .   VYG(MVSIZ),VY,VZ,Y,Z
C-----------------------------------------------
C
C     JCVT.NE.0 Lagrangian case only
      DO I=LFT,LLT
        VY1(I)=V(2,NC1(I))
        VZ1(I)=V(3,NC1(I))
        VY2(I)=V(2,NC2(I))
        VZ2(I)=V(3,NC2(I))
        VY3(I)=V(2,NC3(I))
        VZ3(I)=V(3,NC3(I))
        VY4(I)=V(2,NC4(I))
        VZ4(I)=V(3,NC4(I))
      END DO
C
      DO I=LFT,LLT
        VY=R22(I)*VY1(I)+R32(I)*VZ1(I)
        VZ=R23(I)*VY1(I)+R33(I)*VZ1(I)
        VY1(I)=VY
        VZ1(I)=VZ
        VY=R22(I)*VY2(I)+R32(I)*VZ2(I)
        VZ=R23(I)*VY2(I)+R33(I)*VZ2(I)
        VY2(I)=VY
        VZ2(I)=VZ
        VY=R22(I)*VY3(I)+R32(I)*VZ3(I)
        VZ=R23(I)*VY3(I)+R33(I)*VZ3(I)
        VY3(I)=VY
        VZ3(I)=VZ
        VY=R22(I)*VY4(I)+R32(I)*VZ4(I)
        VZ=R23(I)*VY4(I)+R33(I)*VZ4(I)
        VY4(I)=VY
        VZ4(I)=VZ
      END DO
C
      DO I=LFT,LLT
        PY1(I)=HALF*(Z2(I)-Z4(I))
        PY2(I)=HALF*(Z3(I)-Z1(I))
        PZ1(I)=HALF*(Y4(I)-Y2(I))
        PZ2(I)=HALF*(Y1(I)-Y3(I))	 
      END DO
C
      DO I=LFT,LLT
        A1(I) =Y2(I)*(Z3(I)-Z4(I))+Y3(I)*(Z4(I)-Z2(I))+
     &       Y4(I)*(Z2(I)-Z3(I))
        A2(I) =Y2(I)*(Z4(I)-Z1(I))+Y4(I)*(Z1(I)-Z2(I))+
     &       Y1(I)*(Z2(I)-Z4(I))
        AIREM(I)=(A1(I)+A2(I))*HALF
C AUTRE FORMULE AIREM(I)=2.*(PZ2(I)*PY1(I)-PZ1(I)*PY2(I))
      END DO
C
      IF(N2D.EQ.1)THEN
        DO I=LFT,LLT
          Y =Y1(I)+Y2(I)+Y3(I)+Y4(I)
          Z =Z1(I)+Z2(I)+Z3(I)+Z4(I)
          YAVG(I) =R22(I)*Y+R23(I)*Z
        END DO
      END IF
C
      DO I=LFT,LLT
        VY13(I)=VY1(I)-VY3(I)
        VY24(I)=VY2(I)-VY4(I)
        VZ13(I)=VZ1(I)-VZ3(I)
        VZ24(I)=VZ2(I)-VZ4(I)
      END DO
C
      DO I=LFT,LLT
        IF(AIREM(I).GT.ZERO) THEN
          EYY(I)=(PY1(I)*VY13(I)+PY2(I)*VY24(I))/AIREM(I)
          EZZ(I)=(PZ1(I)*VZ13(I)+PZ2(I)*VZ24(I))/AIREM(I)
          ETT(I)=ZERO
          DZY(I)=(PY1(I)*VZ13(I)+PY2(I)*VZ24(I))/AIREM(I)
          DYZ(I)=(PZ1(I)*VY13(I)+PZ2(I)*VY24(I))/AIREM(I)
          EYT(I)=ZERO
          EZT(I)=ZERO	
        ELSE
          EYY(I)=ZERO
          EZZ(I)=ZERO
          ETT(I)=ZERO
          DZY(I)=ZERO
          DYZ(I)=ZERO
          EYT(I)=ZERO
          EZT(I)=ZERO
        END IF
      END DO
C
      IF(N2D.EQ.1) THEN
        DO I=LFT,LLT
          IF (YAVG(I).GT.ZERO)THEN
            VY=VY1(I)+VY2(I)+VY3(I)+VY4(I)
            VZ=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)
            VYG(I) =R22(I)*VY+R23(I)*VZ
            ETT(I)=VYG(I)/YAVG(I)
          END IF
        END DO
      ENDIF
C
C     correction due to rotation of the skew:
      DO I=LFT,LLT
        EYZ(I)  = DYZ(I)+DZY(I)
     .  	 -DT1*(EYY(I)*DYZ(I)+DZY(I)*EZZ(I))
        EYY(I)  = EYY(I)
     .  	 -0.5*DT1*(EYY(I)*EYY(I)+DZY(I)*DZY(I))
        EZZ(I)  = EZZ(I)
     .  	 -0.5*DT1*(EZZ(I)*EZZ(I)+DYZ(I)*DYZ(I))
        ETT(I)  = ETT(I)
C   pas de rotation => pas de terme    .  	 -0.5*DT1*ETT(I)*ETT(I)
        WYZ(I)= ZERO
      ENDDO
C---------------------------------------
C     REPERE LOCAL POUR MATERIAUX NON ISOTROPES
C---------------------------------------
      DO 90 I=LFT,LLT
      RX(I)=ONE
      RY(I)=ZERO
      RZ(I)=ZERO
      SX(I)=ZERO    
      SY(I)=-PZ1(I)-PZ2(I)
      SZ(I)=+PY1(I)+PY2(I)
      TX(I)=ZERO
      TY(I)=+PZ1(I)-PZ2(I)
      TZ(I)=-PY1(I)+PY2(I)
   90 CONTINUE
C
      RETURN
      END
