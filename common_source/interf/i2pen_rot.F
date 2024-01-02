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
Chd|  I2PEN_ROT                     interf/i2pen_rot.F            
Chd|-- called by -----------
Chd|        I2_DTN_25                     starter/source/interfaces/inter3d1/i2_dtn.F
Chd|        I2FOR25                       engine/source/interfaces/interf/i2for25.F
Chd|        I2FOR25P                      engine/source/interfaces/interf/i2for25p.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2PEN_ROT(SKEW    ,TT      ,DT1  ,STIF ,
     .         RS   ,RM   ,V1   ,V2   ,V3   ,
     .         RX   ,RY   ,RZ   ,VA   ,VB   ,
     .         VC   ,VD   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   TT,DT1,STIF,V1,V2,V3
      my_real
     .   SKEW(9),RS(3),RM(3),RX(4),RY(4),RZ(4),
     .   VA(3), VB(3), VC(3), VD(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IREP1,J
C     REAL
      my_real
     .   CS,SN,KSI
      my_real 
     .   R(3),DWDU,
     .   X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .   XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .   A1,A2,A3,B1,B2,B3,C1,C2,C3,MR,MRX,MRY,MRZ,DET,
     .   BB1,BB2,BB3,CC1,CC2,CC3,
     .   VRX,VRY,VRZ,MGX,MGY,MGZ
C=======================================================================
C
      IF (TT == ZERO) THEN
C       rayon vecteur HS a TT=0, en coord locales 
        SKEW(1) = RS(1)-RM(1)                                                         
        SKEW(2) = RS(2)-RM(2)                                                           
        SKEW(3) = RS(3)-RM(3)                                                          
      ENDIF
C--------------------   
C
      X12=RX(1)*RX(1)
      X22=RX(2)*RX(2)
      X32=RX(3)*RX(3)
      X42=RX(4)*RX(4)
      Y12=RY(1)*RY(1)
      Y22=RY(2)*RY(2)
      Y32=RY(3)*RY(3)
      Y42=RY(4)*RY(4) 
      Z12=RZ(1)*RZ(1)           
      Z22=RZ(2)*RZ(2)
      Z32=RZ(3)*RZ(3)           
      Z42=RZ(4)*RZ(4)           
      XX=X12 + X22 + X32 + X42 
      YY=Y12 + Y22 + Y32 + Y42 
      ZZ=Z12 + Z22 + Z32 + Z42 
      XY=RX(1)*RY(1) + RX(2)*RY(2) + RX(3)*RY(3) + RX(4)*RY(4) 
      YZ=RY(1)*RZ(1) + RY(2)*RZ(2) + RY(3)*RZ(3) + RY(4)*RZ(4) 
      ZX=RZ(1)*RX(1) + RZ(2)*RX(2) + RZ(3)*RX(3) + RZ(4)*RX(4)
      ZZZ=XX+YY
      XXX=YY+ZZ
      YYY=ZZ+XX 
      XY2=XY*XY
      YZ2=YZ*YZ
      ZX2=ZX*ZX
      DET= XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 
     .                                     - TWO*XY*YZ*ZX
      DET=ONE/DET
      B1=ZZZ*YYY-YZ2
      B2=XXX*ZZZ-ZX2
      B3=YYY*XXX-XY2
      C3=ZZZ*XY+YZ*ZX
      C1=XXX*YZ+ZX*XY
      C2=YYY*ZX+XY*YZ
C--------------------   
      MGX = RY(1)*VA(3) + RY(2)*VB(3) + RY(3)*VC(3) + RY(4)*VD(3) 
     .    - RZ(1)*VA(2) - RZ(2)*VB(2) - RZ(3)*VC(2) - RZ(4)*VD(2)
      MGY = RZ(1)*VA(1) + RZ(2)*VB(1) + RZ(3)*VC(1) + RZ(4)*VD(1) 
     .    - RX(1)*VA(3) - RX(2)*VB(3) - RX(3)*VC(3) - RX(4)*VD(3)
      MGZ = RX(1)*VA(2) + RX(2)*VB(2) + RX(3)*VC(2) + RX(4)*VD(2) 
     .    - RY(1)*VA(1) - RY(2)*VB(1) - RY(3)*VC(1) - RY(4)*VD(1)
      VRX=DET*(MGX*B1+MGY*C3+MGZ*C2)
      VRY=DET*(MGY*B2+MGZ*C1+MGX*C3)
      VRZ=DET*(MGZ*B3+MGX*C2+MGY*C1)
C
C     rayon vecteur a TT=0 
      R(1)=SKEW(1)
      R(2)=SKEW(2)
      R(3)=SKEW(3)
C
      V1 = V1 - (VRY*R(3)-VRZ*R(2))
      V2 = V2 - (VRZ*R(1)-VRX*R(3))
      V3 = V3 - (VRX*R(2)-VRY*R(1))
C
      BB1=B1*B1
      BB2=B2*B2
      BB3=B3*B3
      CC1=C1*C1
      CC2=C2*C2
      CC3=C3*C3
      DWDU=DET*SQRT(MAX(BB1*(YY+ZZ)+CC3*(ZZ+XX)+CC2*(XX+YY),
     .                  BB2*(ZZ+XX)+CC1*(XX+YY)+CC3*(YY+ZZ),
     .                  BB3*(XX+YY)+CC2*(YY+ZZ)+CC1*(ZZ+XX)))
C
      STIF=SQRT((R(1)*R(1)+R(2)*R(2)+R(3)*R(3)))*DWDU
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  I2PEN_ROT26                   interf/i2pen_rot.F            
Chd|-- called by -----------
Chd|        I2FOR26                       engine/source/interfaces/interf/i2for26.F
Chd|        I2FOR26P                      engine/source/interfaces/interf/i2for26p.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2PEN_ROT26(TT ,DT1  ,DWDU ,WLX   ,WLY   ,
     .         WLZ   ,RX   ,RY   ,RZ   ,VA   ,
     .         VB   ,VC   ,VD   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   TT,DT1,STIF,WLX,WLY,WLZ
      my_real
     .   RX(4),RY(4),RZ(4),
     .   VA(3), VB(3), VC(3), VD(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IREP1,J
C     REAL
      my_real
     .   CS,SN,KSI
      my_real 
     .   R(3),DWDU,
     .   X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .   XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .   A1,A2,A3,B1,B2,B3,C1,C2,C3,MR,MRX,MRY,MRZ,DET,
     .   BB1,BB2,BB3,CC1,CC2,CC3,
     .   MGX,MGY,MGZ
C======================================================================= 
C
C     Calcul vitesse rotation facette WX,WY,WZ dans repère local
C
      X12=RX(1)*RX(1)
      X22=RX(2)*RX(2)
      X32=RX(3)*RX(3)
      X42=RX(4)*RX(4)
      Y12=RY(1)*RY(1)
      Y22=RY(2)*RY(2)
      Y32=RY(3)*RY(3)
      Y42=RY(4)*RY(4) 
      Z12=RZ(1)*RZ(1)           
      Z22=RZ(2)*RZ(2)
      Z32=RZ(3)*RZ(3)           
      Z42=RZ(4)*RZ(4)           
      XX=X12 + X22 + X32 + X42 
      YY=Y12 + Y22 + Y32 + Y42 
      ZZ=Z12 + Z22 + Z32 + Z42 
      XY=RX(1)*RY(1) + RX(2)*RY(2) + RX(3)*RY(3) + RX(4)*RY(4) 
      YZ=RY(1)*RZ(1) + RY(2)*RZ(2) + RY(3)*RZ(3) + RY(4)*RZ(4) 
      ZX=RZ(1)*RX(1) + RZ(2)*RX(2) + RZ(3)*RX(3) + RZ(4)*RX(4)
      ZZZ=XX+YY
      XXX=YY+ZZ
      YYY=ZZ+XX 
      XY2=XY*XY
      YZ2=YZ*YZ
      ZX2=ZX*ZX
      DET= XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 
     .                                     - TWO*XY*YZ*ZX
      DET=ONE/DET
      B1=ZZZ*YYY-YZ2
      B2=XXX*ZZZ-ZX2
      B3=YYY*XXX-XY2
      C3=ZZZ*XY+YZ*ZX
      C1=XXX*YZ+ZX*XY
      C2=YYY*ZX+XY*YZ
C--------------------   
C
C rappel : rotation vector expressed wrt du = v*dt
C     MGX = Y1*V(3,J1) + Y2*V(3,J2) + Y3*V(3,J3) + Y4*V(3,J4) 
C     .   - Z1*V(2,J1) - Z2*V(2,J2) - Z3*V(2,J3) - Z4*V(2,J4)
C     MGY = Z1*V(1,J1) + Z2*V(1,J2) + Z3*V(1,J3) + Z4*V(1,J4) 
C     .   - X1*V(3,J1) - X2*V(3,J2) - X3*V(3,J3) - X4*V(3,J4)
C     MGZ = X1*V(2,J1) + X2*V(2,J2) + X3*V(2,J3) + X4*V(2,J4) 
C     .   - Y1*V(1,J1) - Y2*V(1,J2) - Y3*V(1,J3) - Y4*V(1,J4)
C     VRX=DET*(MGX*B1+MGY*C3+MGZ*C2)
C     VRY=DET*(MGY*B2+MGZ*C1+MGX*C3)
C     VRZ=DET*(MGZ*B3+MGX*C2+MGY*C1)
C
c     MRX = (ABS(B1)+ABS(C3)+ABS(C2))
c     MRY = (ABS(B2)+ABS(C1)+ABS(C3))
c     MRZ = (ABS(B3)+ABS(C2)+ABS(C1))
C
C--------------------   
      MGX = RY(1)*VA(3) + RY(2)*VB(3) + RY(3)*VC(3) + RY(4)*VD(3) 
     .    - RZ(1)*VA(2) - RZ(2)*VB(2) - RZ(3)*VC(2) - RZ(4)*VD(2)
      MGY = RZ(1)*VA(1) + RZ(2)*VB(1) + RZ(3)*VC(1) + RZ(4)*VD(1) 
     .    - RX(1)*VA(3) - RX(2)*VB(3) - RX(3)*VC(3) - RX(4)*VD(3)
      MGZ = RX(1)*VA(2) + RX(2)*VB(2) + RX(3)*VC(2) + RX(4)*VD(2) 
     .    - RY(1)*VA(1) - RY(2)*VB(1) - RY(3)*VC(1) - RY(4)*VD(1)
C
      WLX=DET*(MGX*B1+MGY*C3+MGZ*C2)
      WLY=DET*(MGY*B2+MGZ*C1+MGX*C3)
      WLZ=DET*(MGZ*B3+MGX*C2+MGY*C1)
C
      BB1=B1*B1
      BB2=B2*B2
      BB3=B3*B3
      CC1=C1*C1
      CC2=C2*C2
      CC3=C3*C3
      DWDU=DET*SQRT(MAX(BB1*(YY+ZZ)+CC3*(ZZ+XX)+CC2*(XX+YY),
     .                  BB2*(ZZ+XX)+CC1*(XX+YY)+CC3*(YY+ZZ),
     .                  BB3*(XX+YY)+CC2*(YY+ZZ)+CC1*(ZZ+XX)))
C
C      STIF=SQRT((R(1)*R(1)+R(2)*R(2)+R(3)*R(3)))*DWDU
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I2PEN_ROT27                   interf/i2pen_rot.F            
Chd|-- called by -----------
Chd|        I2_DTN_27_PEN                 starter/source/interfaces/inter3d1/i2_dtn_27.F
Chd|        I2FOR27P_PEN                  engine/source/interfaces/interf/i2for27p_pen.F
Chd|        I2FOR27_PEN                   engine/source/interfaces/interf/i2for27_pen.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2PEN_ROT27(SKEW    ,TT      ,DT1  ,STIF ,
     .         RS   ,RM   ,VX   ,VY   ,VZ   ,
     .         RX   ,RY   ,RZ   ,VA   ,VB   ,
     .         VC   ,VD   ,VRM  ,VRS  ,DET  ,
     .         B1   ,B2   ,B3   ,C1   ,C2   ,
     .         C3   ,IN_SECND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   TT,DT1,STIF,VX,VY,VZ,B1,B2,B3,C1,C2,C3,DET
      my_real
     .   SKEW(9),RS(3),RM(3),RX(4),RY(4),RZ(4),
     .   VA(3), VB(3), VC(3), VD(3),VRS(3),VRM(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IREP1,J
C     REAL
      my_real
     .   CS,SN,KSI
      my_real 
     .   R(3),DWDU,
     .   X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .   XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .   A1,A2,A3,MR,MRX,MRY,MRZ,
     .   BB1,BB2,BB3,CC1,CC2,CC3,
     .   MGX,MGY,MGZ,WX,WY,WZ,IN_SECND
C=======================================================================
C
      IF (TT == ZERO) THEN
C       rayon vecteur HS a TT=0, en coord locales 
        SKEW(1) = RS(1)-RM(1)                                                         
        SKEW(2) = RS(2)-RM(2)                                                           
        SKEW(3) = RS(3)-RM(3)                                                          
      ENDIF
C--------------------   
C
      X12=RX(1)*RX(1)
      X22=RX(2)*RX(2)
      X32=RX(3)*RX(3)
      X42=RX(4)*RX(4)
      Y12=RY(1)*RY(1)
      Y22=RY(2)*RY(2)
      Y32=RY(3)*RY(3)
      Y42=RY(4)*RY(4) 
      Z12=RZ(1)*RZ(1)           
      Z22=RZ(2)*RZ(2)
      Z32=RZ(3)*RZ(3)           
      Z42=RZ(4)*RZ(4)           
      XX=X12 + X22 + X32 + X42 
      YY=Y12 + Y22 + Y32 + Y42 
      ZZ=Z12 + Z22 + Z32 + Z42 
      XY=RX(1)*RY(1) + RX(2)*RY(2) + RX(3)*RY(3) + RX(4)*RY(4) 
      YZ=RY(1)*RZ(1) + RY(2)*RZ(2) + RY(3)*RZ(3) + RY(4)*RZ(4) 
      ZX=RZ(1)*RX(1) + RZ(2)*RX(2) + RZ(3)*RX(3) + RZ(4)*RX(4)
      ZZZ=XX+YY
      XXX=YY+ZZ
      YYY=ZZ+XX 
      XY2=XY*XY
      YZ2=YZ*YZ
      ZX2=ZX*ZX
      DET= XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 
     .                                     - TWO*XY*YZ*ZX
      DET=ONE/DET
      B1=ZZZ*YYY-YZ2
      B2=XXX*ZZZ-ZX2
      B3=YYY*XXX-XY2
      C3=ZZZ*XY+YZ*ZX
      C1=XXX*YZ+ZX*XY
      C2=YYY*ZX+XY*YZ
C
      IF (IRODDL == 0 .OR. IN_SECND == ZERO) THEN
C--    For solid secnd node VRM=VRS=Velocity rotation main segment 
        MGX = RY(1)*VA(3) + RY(2)*VB(3) + RY(3)*VC(3) + RY(4)*VD(3) 
     .      - RZ(1)*VA(2) - RZ(2)*VB(2) - RZ(3)*VC(2) - RZ(4)*VD(2)
        MGY = RZ(1)*VA(1) + RZ(2)*VB(1) + RZ(3)*VC(1) + RZ(4)*VD(1) 
     .      - RX(1)*VA(3) - RX(2)*VB(3) - RX(3)*VC(3) - RX(4)*VD(3)
        MGZ = RX(1)*VA(2) + RX(2)*VB(2) + RX(3)*VC(2) + RX(4)*VD(2) 
     .      - RY(1)*VA(1) - RY(2)*VB(1) - RY(3)*VC(1) - RY(4)*VD(1)
C
        VRM(1)=DET*(MGX*B1+MGY*C3+MGZ*C2)
        VRM(2)=DET*(MGY*B2+MGZ*C1+MGX*C3)
        VRM(3)=DET*(MGZ*B3+MGX*C2+MGY*C1)
C     
        WX = VRM(1)
        WY = VRM(2)
        WZ = VRM(3)
      ELSE
        WX = (VRM(1)+VRS(1))*HALF
        WY = (VRM(2)+VRS(2))*HALF
        WZ = (VRM(3)+VRS(3))*HALF
      ENDIF
C
C     rayon vecteur a TT=0 
      R(1)=SKEW(1)
      R(2)=SKEW(2)
      R(3)=SKEW(3)

C     Vitesse entrainemnet
      VX = VX - (WY*R(3)-WZ*R(2))
      VY = VY - (WZ*R(1)-WX*R(3))
      VZ = VZ - (WX*R(2)-WY*R(1))
C
      BB1=B1*B1
      BB2=B2*B2
      BB3=B3*B3
      CC1=C1*C1
      CC2=C2*C2
      CC3=C3*C3
      DWDU=DET*SQRT(MAX(BB1*(YY+ZZ)+CC3*(ZZ+XX)+CC2*(XX+YY),
     .                  BB2*(ZZ+XX)+CC1*(XX+YY)+CC3*(YY+ZZ),
     .                  BB3*(XX+YY)+CC2*(YY+ZZ)+CC1*(ZZ+XX)))
C
      STIF=SQRT((R(1)*R(1)+R(2)*R(2)+R(3)*R(3)))*DWDU
C
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  I2PEN_ROT28                   interf/i2pen_rot.F            
Chd|-- called by -----------
Chd|        I2_DTN_28_PEN                 starter/source/interfaces/inter3d1/i2_dtn_28.F
Chd|        I2FOR28P_PEN                  engine/source/interfaces/interf/i2for28p_pen.F
Chd|        I2FOR28_PEN                   engine/source/interfaces/interf/i2for28_pen.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2PEN_ROT28(SKEW    ,TT      ,DT1  ,STIF ,
     .         RS   ,RM   ,VX   ,VY   ,VZ   ,
     .         RX   ,RY   ,RZ   ,VA   ,VB   ,
     .         VC   ,VD   ,VRM  ,VRS  ,DET  ,
     .         B1   ,B2   ,B3   ,C1   ,C2   ,
     .         C3   ,IN_SECND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   TT,DT1,STIF,VX,VY,VZ,B1,B2,B3,C1,C2,C3,DET
      my_real
     .   SKEW(9),RS(3),RM(3),RX(4),RY(4),RZ(4),
     .   VA(3), VB(3), VC(3), VD(3),VRS(3),VRM(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IREP1,J
C     REAL
      my_real
     .   CS,SN,KSI
      my_real 
     .   R(3),DWDU,
     .   X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .   XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .   A1,A2,A3,MR,MRX,MRY,MRZ,
     .   BB1,BB2,BB3,CC1,CC2,CC3,
     .   MGX,MGY,MGZ,WX,WY,WZ,IN_SECND
C=======================================================================
C
      IF (TT == ZERO) THEN
C       rayon vecteur HS a TT=0, en coord locales 
        SKEW(1) = RS(1)-RM(1)                                                         
        SKEW(2) = RS(2)-RM(2)                                                           
        SKEW(3) = RS(3)-RM(3)                                                          
      ENDIF
C--------------------   
C
      X12=RX(1)*RX(1)
      X22=RX(2)*RX(2)
      X32=RX(3)*RX(3)
      X42=RX(4)*RX(4)
      Y12=RY(1)*RY(1)
      Y22=RY(2)*RY(2)
      Y32=RY(3)*RY(3)
      Y42=RY(4)*RY(4) 
      Z12=RZ(1)*RZ(1)           
      Z22=RZ(2)*RZ(2)
      Z32=RZ(3)*RZ(3)           
      Z42=RZ(4)*RZ(4)           
      XX=X12 + X22 + X32 + X42 
      YY=Y12 + Y22 + Y32 + Y42 
      ZZ=Z12 + Z22 + Z32 + Z42 
      XY=RX(1)*RY(1) + RX(2)*RY(2) + RX(3)*RY(3) + RX(4)*RY(4) 
      YZ=RY(1)*RZ(1) + RY(2)*RZ(2) + RY(3)*RZ(3) + RY(4)*RZ(4) 
      ZX=RZ(1)*RX(1) + RZ(2)*RX(2) + RZ(3)*RX(3) + RZ(4)*RX(4)
      ZZZ=XX+YY
      XXX=YY+ZZ
      YYY=ZZ+XX 
      XY2=XY*XY
      YZ2=YZ*YZ
      ZX2=ZX*ZX
      DET= XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 
     .                                     - TWO*XY*YZ*ZX
      DET=ONE/DET
      B1=ZZZ*YYY-YZ2
      B2=XXX*ZZZ-ZX2
      B3=YYY*XXX-XY2
      C3=ZZZ*XY+YZ*ZX
      C1=XXX*YZ+ZX*XY
      C2=YYY*ZX+XY*YZ
C  
      MGX = RY(1)*VA(3) + RY(2)*VB(3) + RY(3)*VC(3) + RY(4)*VD(3) 
     .    - RZ(1)*VA(2) - RZ(2)*VB(2) - RZ(3)*VC(2) - RZ(4)*VD(2)
      MGY = RZ(1)*VA(1) + RZ(2)*VB(1) + RZ(3)*VC(1) + RZ(4)*VD(1) 
     .    - RX(1)*VA(3) - RX(2)*VB(3) - RX(3)*VC(3) - RX(4)*VD(3)
      MGZ = RX(1)*VA(2) + RX(2)*VB(2) + RX(3)*VC(2) + RX(4)*VD(2) 
     .    - RY(1)*VA(1) - RY(2)*VB(1) - RY(3)*VC(1) - RY(4)*VD(1)
C
      VRM(1)=DET*(MGX*B1+MGY*C3+MGZ*C2)
      VRM(2)=DET*(MGY*B2+MGZ*C1+MGX*C3)
      VRM(3)=DET*(MGZ*B3+MGX*C2+MGY*C1)
C
      IF (IRODDL == 0 .OR. IN_SECND == ZERO) THEN
C--    For solid secnd node VRM=VRS=velocity main segment
        WX = VRM(1)
        WY = VRM(2)
        WZ = VRM(3)
      ELSE
        WX = (VRM(1)+VRS(1))*HALF
        WY = (VRM(2)+VRS(2))*HALF
        WZ = (VRM(3)+VRS(3))*HALF
      ENDIF
C
C     rayon vecteur a TT=0 
      R(1)=SKEW(1)
      R(2)=SKEW(2)
      R(3)=SKEW(3)

C     Vitesse entrainemnet
      VX = VX - (WY*R(3)-WZ*R(2)) -(VRM(2)*RM(3)-VRM(3)*RM(2))
      VY = VY - (WZ*R(1)-WX*R(3)) -(VRM(3)*RM(1)-VRM(1)*RM(3))
      VZ = VZ - (WX*R(2)-WY*R(1)) -(VRM(1)*RM(2)-VRM(2)*RM(1))
C
      BB1=B1*B1
      BB2=B2*B2
      BB3=B3*B3
      CC1=C1*C1
      CC2=C2*C2
      CC3=C3*C3
      DWDU=DET*SQRT(MAX(BB1*(YY+ZZ)+CC3*(ZZ+XX)+CC2*(XX+YY),
     .                  BB2*(ZZ+XX)+CC1*(XX+YY)+CC3*(YY+ZZ),
     .                  BB3*(XX+YY)+CC2*(YY+ZZ)+CC1*(ZZ+XX)))
C
      STIF=SQRT((R(1)*R(1)+R(2)*R(2)+R(3)*R(3)))*DWDU
C      STIF=ZERO
C-----------
      RETURN
      END
