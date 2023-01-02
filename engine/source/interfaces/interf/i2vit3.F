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
Chd|  I2VIT3N                       source/interfaces/interf/i2vit3.F
Chd|-- called by -----------
Chd|        INTTI2V                       source/interfaces/interf/intti2v.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2VIT3N(
     1 NSN   , NMN   , A   , IRECT, CRST,
     2 MSR   , NSV   , IRTL, V    , MS  ,
     3 WEIGHT, MMASS                    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),WEIGHT(*)
C     REAL
      my_real
     .   A(*), CRST(2,*), V(*),MS(*), MMASS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, I3, J3, I2, J2, I1, J1, ISK, ICOD, II, L, JJ,
     .   NN
C     REAL
      my_real
     .   H(4), SS, TT, AMX, AMY, AMZ, VMX, VMY, VMZ,SP,SM,TP,TM,MAS
C-----------------------------------------------
      NIR=2
      IF(N2D==0)NIR=4
C
      DO II=1,NSN
        I=NSV(II)
        IF(I>0)THEN
          L=IRTL(II)
C
          SS=CRST(1,II)
          TT=CRST(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=TP*SP
          H(4)=TP*SM
          I3=3*I
          I2=I3-1
          I1=I2-1
          AMX=ZERO
          AMY=ZERO
          AMZ=ZERO
          VMX=ZERO
          VMY=ZERO
          VMZ=ZERO
C
          DO JJ=1,NIR
            J=IRECT(JJ,L)
            J3=3*J
            J2=J3-1
            J1=J2-1
            AMX=AMX+A(J1)*H(JJ)
            AMY=AMY+A(J2)*H(JJ)
            AMZ=AMZ+A(J3)*H(JJ)
            VMX=VMX+V(J1)*H(JJ)
            VMY=VMY+V(J2)*H(JJ)
            VMZ=VMZ+V(J3)*H(JJ)
          ENDDO
          A(I1)=AMX
          A(I2)=AMY
          A(I3)=AMZ
          V(I1)=VMX
          V(I2)=VMY
          V(I3)=VMZ
        ENDIF
C
      ENDDO
C-----------
      RETURN
      END
Cel|         INTTI12V                        /interf/intti12.F
Chd|====================================================================
Chd|  I2VIT3                        source/interfaces/interf/i2vit3.F
Chd|-- called by -----------
Chd|        INTTI2V                       source/interfaces/interf/intti2v.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2VIT3(
     1 NSN   , NMN   , A   , IRECT, CRST,
     2 MSR   , NSV   , IRTL, V    , MS  ,
     3 WEIGHT, MMASS                    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),WEIGHT(*)
C     REAL
      my_real
     .   A(*), CRST(2,*), V(*),MS(*), MMASS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, I3, J3, I2, J2, I1, J1, ISK, ICOD, II, L, JJ,
     .   NN
C     REAL
      my_real
     .   H(4), SS, TT, AMX, AMY, AMZ, VMX, VMY, VMZ,SP,SM,TP,TM,MAS
C-----------------------------------------------
      NIR=2
      IF(N2D==0)NIR=4
C
      DO 70 II=1,NSN
       I=NSV(II)
       IF(I>0)THEN
        L=IRTL(II)
C
        SS=CRST(1,II)
        TT=CRST(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + TT)
        TM=FOURTH*(ONE - TT)
        H(1)=TM*SM
        H(2)=TM*SP
        H(3)=TP*SP
        H(4)=TP*SM
        I3=3*I
        I2=I3-1
        I1=I2-1
        AMX=ZERO
        AMY=ZERO
        AMZ=ZERO
        VMX=ZERO
        VMY=ZERO
        VMZ=ZERO    
Cel   XMSI=MS(I)*WEIGHT(I)
C
        DO JJ=1,NIR
C          J3=3*MSR(IRECT(JJ,L))
          J=IRECT(JJ,L)
          J3=3*J
          J2=J3-1
          J1=J2-1
          AMX=AMX+A(J1)*H(JJ)
          AMY=AMY+A(J2)*H(JJ)
          AMZ=AMZ+A(J3)*H(JJ)
          VMX=VMX+V(J1)*H(JJ)
          VMY=VMY+V(J2)*H(JJ)
          VMZ=VMZ+V(J3)*H(JJ)
CFP (RESET MASSE MAIN)
Cel     MS(J)=MS(J)-XMSI*H(JJ)
        ENDDO
        A(I1)=AMX
        A(I2)=AMY
        A(I3)=AMZ
        V(I1)=VMX
        V(I2)=VMY
        V(I3)=VMZ
       ENDIF
C
 70   CONTINUE
C
Cel restitution de la masse initiale sur noeuds main
      DO II=1,NMN
        J=MSR(II)
        MS(J)=MMASS(II)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I2ROT3                        source/interfaces/interf/i2vit3.F
Chd|-- called by -----------
Chd|        INTTI2V                       source/interfaces/interf/intti2v.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2ROT3(NSN,NMN,AR  ,IRECT,CRST,MSR ,
     2                  NSV,IRTL,VR  ,IN  ,A   ,V    ,X   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*)
C     REAL
      my_real
     .   AR(3,*), CRST(2,*), VR(3,*),
     .   IN(*), A(3,*),  V(3,*), X(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, I3, J3, I2, J2, I1, J1, ISK, ICOD, II, L, JJ,
     .   NN
      my_real
     .   H(4), SS, TT, AMX, AMY, AMZ, VMX, VMY, VMZ,INM,
     .   X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,Z0,Z1,Z2,Z3,Z4,AA,
     .   NX,NY,NZ,X13,X24,Y13,Y24,Z13,Z24,XC0,YC0,ZC0,SP,SM,TP,TM,
     .   XC,YC,ZC,VMXX,VMYY,VMZZ
C-----------------------------------------------
C
      DO 70 II=1,NSN
       I=NSV(II)
       IF(I>0)THEN
        L=IRTL(II)
C
        SS=CRST(1,II)
        TT=CRST(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + TT)
        TM=FOURTH*(ONE - TT)
        H(1)=TM*SM
        H(2)=TM*SP
        H(3)=TP*SP
        H(4)=TP*SM
C  
        XC0 = X(1,I)
        YC0 = X(2,I)
        ZC0 = X(3,I)
C  
        AMX=ZERO	
        AMY=ZERO
        AMZ=ZERO
        VMX=ZERO
        VMY=ZERO
        VMZ=ZERO     
C
        DO JJ=1,4
C          J=MSR(IRECT(JJ,L))
          J=IRECT(JJ,L)
          AMX=AMX+AR(1,J)*H(JJ)
          AMY=AMY+AR(2,J)*H(JJ)
          AMZ=AMZ+AR(3,J)*H(JJ)
          VMX=VMX+VR(1,J)*H(JJ)
          VMY=VMY+VR(2,J)*H(JJ)
          VMZ=VMZ+VR(3,J)*H(JJ)
          XC0=XC0 - X(1,J) * H(JJ)
          YC0=YC0 - X(2,J) * H(JJ)
          ZC0=ZC0 - X(3,J) * H(JJ)
        ENDDO
C
        AR(1,I)=AMX
        AR(2,I)=AMY
        AR(3,I)=AMZ
        VR(1,I)=VMX
        VR(2,I)=VMY
        VR(3,I)=VMZ
C
        VMXX = VMY*ZC0 - VMZ*YC0
        VMYY = VMZ*XC0 - VMX*ZC0
        VMZZ = VMX*YC0 - VMY*XC0
C
        A(1,I)= A(1,I) + AMY*ZC0 -AMZ*YC0 +HALF*(VMY*VMZZ-VMZ*VMYY)
        A(2,I)= A(2,I) + AMZ*XC0 -AMX*ZC0 +HALF*(VMZ*VMXX-VMX*VMZZ)
        A(3,I)= A(3,I) + AMX*YC0 -AMY*XC0 +HALF*(VMX*VMYY-VMY*VMXX)
        V(1,I)= V(1,I) + VMXX
        V(2,I)= V(2,I) + VMYY
        V(3,I)= V(3,I) + VMZZ
C
       ENDIF
C
 70   CONTINUE
      RETURN
      END
C=======================================================================
Chd|====================================================================
Chd|  I2VIROT3                      source/interfaces/interf/i2vit3.F
Chd|-- called by -----------
Chd|        I2VIT28                       source/interfaces/interf/i2vit28.F
Chd|        INTTI2V                       source/interfaces/interf/intti2v.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2VIROT3(NSN,NMN,A   ,IRECT,DPARA,MSR ,
     2                  NSV,IRTL,V   ,MS    ,AR  ,VR   ,
     3                  X  ,WEIGHT )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),WEIGHT(*)
C     REAL
      my_real
     .    A(3,*), DPARA(7,*), V(3,*),
     .    MS(*),VR(3,*),AR(3,*),X(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, J1, J2, J3, J4, ISK, ICOD, II, L, JJ,
     .   NN,I1
C     REAL
      my_real
     .   H(4), SS, TT, AMX, AMY, AMZ, VMX, VMY, VMZ,SP,SM,TP,TM,MAS,
     .   MRX,MRY,MRZ,MGX,MGY,MGZ,DET,ARX,ARY,ARZ,
     .   X0,X1,X2,X3,X4,XS,Y0,Y1,Y2,Y3,Y4,YS,Z0,Z1,Z2,Z3,Z4,ZS,
     .   X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .   XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .   VRX,VRY,VRZ,B1,B2,B3,C1,C2,C3,
     .   XMSI     
C-----------------------------------------------
      NIR=2
      IF(N2D==0)NIR=4
CFP (RESET MASSE MAIN)
C      DO II=1,NSN
C        I=NSV(II)
C        L=IRTL(II)
C        SS=CRST(1,II)
C        TT=CRST(2,II)
C        SP=1.0+SS
C        SM=1.0-SS
C        TP=.25*(1.0+TT)
C        TM=.25*(1.0-TT)
C        H(1)=TM*SM
C        H(2)=TM*SP
C        H(3)=TP*SP
C        H(4)=TP*SM
C        I1=3*I-2
C        XMSI=MS(I)*WEIGHT(I)
C        DO JJ=1,NIR
C         J=IRECT(JJ,L)
C         MS(J)=MS(J)-XMSI*H(JJ)
C        ENDDO
C      ENDDO
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
      DO II=1,NSN
       I=NSV(II)
       IF(I>0)THEN
        L=IRTL(II)
C          J1=MSR(IRECT(1,L))
C          J2=MSR(IRECT(2,L))
C          J3=MSR(IRECT(3,L))
C          J4=MSR(IRECT(4,L))
        J1=IRECT(1,L)
        J2=IRECT(2,L)
        J3=IRECT(3,L)
        J4=IRECT(4,L)
C----------------------------------------
C       VITESSE MOYENNE DU SEGMENT MAIN
C----------------------------------------
        VMX=FOURTH*(V(1,J1)+V(1,J2)+V(1,J3)+V(1,J4))
        VMY=FOURTH*(V(2,J1)+V(2,J2)+V(2,J3)+V(2,J4))
        VMZ=FOURTH*(V(3,J1)+V(3,J2)+V(3,J3)+V(3,J4))
        AMX=FOURTH*(A(1,J1)+A(1,J2)+A(1,J3)+A(1,J4))
        AMY=FOURTH*(A(2,J1)+A(2,J2)+A(2,J3)+A(2,J4))
        AMZ=FOURTH*(A(3,J1)+A(3,J2)+A(3,J3)+A(3,J4))	
C----------------------------------------------------
C       VITESSE DE ROTATION MOYENNE DU SEGMENT MAIN
C----------------------------------------------------
        X1=X(1,J1)
        Y1=X(2,J1)
        Z1=X(3,J1)
        X2=X(1,J2)
        Y2=X(2,J2)
        Z2=X(3,J2)
        X3=X(1,J3)
        Y3=X(2,J3)
        Z3=X(3,J3)
        X4=X(1,J4)
        Y4=X(2,J4)
        Z4=X(3,J4)
        X0=FOURTH*(X1+X2+X3+X4)
        Y0=FOURTH*(Y1+Y2+Y3+Y4)
        Z0=FOURTH*(Z1+Z2+Z3+Z4)	
        X1=X1-X0
        Y1=Y1-Y0
        Z1=Z1-Z0
        X2=X2-X0
        Y2=Y2-Y0
        Z2=Z2-Z0
        X3=X3-X0
        Y3=Y3-Y0
        Z3=Z3-Z0
        X4=X4-X0
        Y4=Y4-Y0
        Z4=Z4-Z0
        XS=X(1,I)-X0
        YS=X(2,I)-Y0
        ZS=X(3,I)-Z0
C
C        X12=X1*X1
C        X22=X2*X2
C        X32=X3*X3
C        X42=X4*X4 
C        Y12=Y1*Y1
C        Y22=Y2*Y2
C        Y32=Y3*Y3
C        Y42=Y4*Y4 
C        Z12=Z1*Z1 
C        Z22=Z2*Z2
C        Z32=Z3*Z3 
C        Z42=Z4*Z4 
C        XX=X12 + X22 + X32 + X42 
C        YY=Y12 + Y22 + Y32 + Y42 
C        ZZ=Z12 + Z22 + Z32 + Z42 
C        XY=X1*Y1 + X2*Y2 + X3*Y3 + X4*Y4 
C        YZ=Y1*Z1 + Y2*Z2 + Y3*Z3 + Y4*Z4 
C        ZX=Z1*X1 + Z2*X2 + Z3*X3 + Z4*X4
C        ZZZ=XX+YY
C        XXX=YY+ZZ
C        YYY=ZZ+XX 
C        XY2=XY*XY
C        YZ2=YZ*YZ
C        ZX2=ZX*ZX
C        DET= XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 - 2.*XY*YZ*ZX
C        DET=1./DET
C        B1=ZZZ*YYY-YZ2
C        B2=XXX*ZZZ-ZX2
C        B3=YYY*XXX-XY2
C        C3=ZZZ*XY+YZ*ZX
C        C1=XXX*YZ+ZX*XY
C        C2=YYY*ZX+XY*YZ
C
C
        DET= DPARA(1,II)
        B1=DPARA(2,II)
        B2=DPARA(3,II)
        B3=DPARA(4,II)
        C1=DPARA(5,II)
        C2=DPARA(6,II)
        C3=DPARA(7,II)
C
        MGX = Y1*V(3,J1) + Y2*V(3,J2) + Y3*V(3,J3) + Y4*V(3,J4) 
     .      - Z1*V(2,J1) - Z2*V(2,J2) - Z3*V(2,J3) - Z4*V(2,J4)
        MGY = Z1*V(1,J1) + Z2*V(1,J2) + Z3*V(1,J3) + Z4*V(1,J4) 
     .      - X1*V(3,J1) - X2*V(3,J2) - X3*V(3,J3) - X4*V(3,J4)
        MGZ = X1*V(2,J1) + X2*V(2,J2) + X3*V(2,J3) + X4*V(2,J4) 
     .      - Y1*V(1,J1) - Y2*V(1,J2) - Y3*V(1,J3) - Y4*V(1,J4)
C
        MRX = Y1*A(3,J1) + Y2*A(3,J2) + Y3*A(3,J3) + Y4*A(3,J4) 
     .      - Z1*A(2,J1) - Z2*A(2,J2) - Z3*A(2,J3) - Z4*A(2,J4)
        MRY = Z1*A(1,J1) + Z2*A(1,J2) + Z3*A(1,J3) + Z4*A(1,J4) 
     .      - X1*A(3,J1) - X2*A(3,J2) - X3*A(3,J3) - X4*A(3,J4)
        MRZ = X1*A(2,J1) + X2*A(2,J2) + X3*A(2,J3) + X4*A(2,J4) 
     .      - Y1*A(1,J1) - Y2*A(1,J2) - Y3*A(1,J3) - Y4*A(1,J4)
C

        VRX=DET*(MGX*B1+MGY*C3+MGZ*C2)
        VRY=DET*(MGY*B2+MGZ*C1+MGX*C3)
        VRZ=DET*(MGZ*B3+MGX*C2+MGY*C1)
        ARX=DET*(MRX*B1+MRY*C3+MRZ*C2)
        ARY=DET*(MRY*B2+MRZ*C1+MRX*C3)
        ARZ=DET*(MRZ*B3+MRX*C2+MRY*C1)
C
C----------------------------------------------------
C       VITESSE DE ROTATION DU NOEUD SECOND
C----------------------------------------------------
        IF (IRODDL == 1) THEN
          VR(1,I)=VRX
          VR(2,I)=VRY
          VR(3,I)=VRZ
          AR(1,I)=ARX
          AR(2,I)=ARY
          AR(3,I)=ARZ
        ENDIF
C----------------------------------------------------
C       VITESSE DU NOEUD SECOND
C----------------------------------------------------
        V(1,I)=VMX + VRY*ZS - VRZ*YS
        V(2,I)=VMY + VRZ*XS - VRX*ZS
        V(3,I)=VMZ + VRX*YS - VRY*XS
        A(1,I)=AMX + ARY*ZS - ARZ*YS
        A(2,I)=AMY + ARZ*XS - ARX*ZS
        A(3,I)=AMZ + ARX*YS - ARY*XS
       ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I2ROT3_27                     source/interfaces/interf/i2vit3.F
Chd|-- called by -----------
Chd|        I2VIT27                       source/interfaces/interf/i2vit27.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2ROT3_27(NSN,NMN,AR  ,IRECT,CRST,MSR ,
     2                  NSV,IRTL,VR  ,IN  ,A   ,V    ,X,
     3                  SINER,DPARA,MSEGTYP2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),MSEGTYP2(*)
C     REAL
      my_real
     .   AR(3,*), CRST(2,*), VR(3,*),
     .   IN(*), A(3,*),  V(3,*), X(3,*), SINER(*),DPARA(7,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, I3, J3, I2, J2, I1, J1, ISK, ICOD, II, L, JJ,
     .   NN,J4
      my_real
     .   H(4), SS, TT, AMX, AMY, AMZ, VMX, VMY, VMZ,INM,
     .   X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,Z0,Z1,Z2,Z3,Z4,AA,
     .   NX,NY,NZ,X13,X24,Y13,Y24,Z13,Z24,XC0,YC0,ZC0,SP,SM,TP,TM,
     .   XC,YC,ZC,MGX,MGY,MGZ,MRX,MRY,MRZ,VMXX,VMYY,VMZZ,
     .   DET,C1,C2,C3,B1,B2,B3  
C
C-----------------------------------------------
C
C
      DO 70 II=1,NSN
       I=NSV(II)
C
       IF(I>0)THEN
        L=IRTL(II)
C
        SS=CRST(1,II)
        TT=CRST(2,II)

        IF (IRECT(3,L) == IRECT(4,L)) THEN
C--      Shape functions of triangles
          H(1) = SS
          H(2) = TT
          H(3) = ONE-SS-TT
          H(4) = ZERO
        ELSE
C--       Shape functions of quadrangles         
          SP = ONE + SS           
          SM = ONE - SS           
          TP = FOURTH*(ONE + TT)    
          TM = FOURTH*(ONE - TT)
C       
          H(1)=TM*SM           
          H(2)=TM*SP           
          H(3)=TP*SP           
          H(4)=TP*SM 
        ENDIF
C
        XC0 = X(1,I)
        YC0 = X(2,I)
        ZC0 = X(3,I)
C
        DO JJ=1,4
          J=IRECT(JJ,L)
          XC0=XC0 - X(1,J) * H(JJ)
          YC0=YC0 - X(2,J) * H(JJ)
          ZC0=ZC0 - X(3,J) * H(JJ)
        ENDDO
C
        J1=IRECT(1,L)
        J2=IRECT(2,L)
        J3=IRECT(3,L)
        J4=IRECT(4,L)
C
        IF (MSEGTYP2(L)==0) THEN
C
C--------------------------------------------C
C--- solid / solid connection ---------------C
C--------------------------------------------C     
C        
          X1=X(1,J1)
          Y1=X(2,J1)
          Z1=X(3,J1)
          X2=X(1,J2)
          Y2=X(2,J2)
          Z2=X(3,J2)
          X3=X(1,J3)
          Y3=X(2,J3)
          Z3=X(3,J3)
          X4=X(1,J4)
          Y4=X(2,J4)
          Z4=X(3,J4)
C
          IF (J3 == J4) THEN
            X0=THIRD*(X1+X2+X3)
            Y0=THIRD*(Y1+Y2+Y3)
            Z0=THIRD*(Z1+Z2+Z3)
          ELSE
            X0=FOURTH*(X1+X2+X3+X4)
            Y0=FOURTH*(Y1+Y2+Y3+Y4)
            Z0=FOURTH*(Z1+Z2+Z3+Z4)
          ENDIF
C	
          X1=X1-X0
          Y1=Y1-Y0
          Z1=Z1-Z0
          X2=X2-X0
          Y2=Y2-Y0
          Z2=Z2-Z0
          X3=X3-X0
          Y3=Y3-Y0
          Z3=Z3-Z0
          X4=X4-X0
          Y4=Y4-Y0
          Z4=Z4-Z0
C
          IF (J3 == J4) THEN
            X4 = ZERO
            Y4 = ZERO
            Z4 = ZERO
          ENDIF
C       
          DET=DPARA(1,II)
          B1=DPARA(2,II)
          B2=DPARA(3,II)
          B3=DPARA(4,II)
          C1=DPARA(5,II)
          C2=DPARA(6,II)
          C3=DPARA(7,II)
C
          MGX = Y1*V(3,J1) + Y2*V(3,J2) + Y3*V(3,J3) + Y4*V(3,J4) 
     .        - Z1*V(2,J1) - Z2*V(2,J2) - Z3*V(2,J3) - Z4*V(2,J4)
          MGY = Z1*V(1,J1) + Z2*V(1,J2) + Z3*V(1,J3) + Z4*V(1,J4) 
     .        - X1*V(3,J1) - X2*V(3,J2) - X3*V(3,J3) - X4*V(3,J4)
          MGZ = X1*V(2,J1) + X2*V(2,J2) + X3*V(2,J3) + X4*V(2,J4) 
     .        - Y1*V(1,J1) - Y2*V(1,J2) - Y3*V(1,J3) - Y4*V(1,J4)
C
          MRX = Y1*A(3,J1) + Y2*A(3,J2) + Y3*A(3,J3) + Y4*A(3,J4) 
     .        - Z1*A(2,J1) - Z2*A(2,J2) - Z3*A(2,J3) - Z4*A(2,J4)
          MRY = Z1*A(1,J1) + Z2*A(1,J2) + Z3*A(1,J3) + Z4*A(1,J4) 
     .        - X1*A(3,J1) - X2*A(3,J2) - X3*A(3,J3) - X4*A(3,J4)
          MRZ = X1*A(2,J1) + X2*A(2,J2) + X3*A(2,J3) + X4*A(2,J4) 
     .        - Y1*A(1,J1) - Y2*A(1,J2) - Y3*A(1,J3) - Y4*A(1,J4)
C
          VMX=DET*(MGX*B1+MGY*C3+MGZ*C2)
          VMY=DET*(MGY*B2+MGZ*C1+MGX*C3)
          VMZ=DET*(MGZ*B3+MGX*C2+MGY*C1)
          AMX=DET*(MRX*B1+MRY*C3+MRZ*C2)
          AMY=DET*(MRY*B2+MRZ*C1+MRX*C3)
          AMZ=DET*(MRZ*B3+MRX*C2+MRY*C1)
C
        ELSE
C--------------------------------------------------C
C--- shell / shell - shell / solide  --------------C
C--------------------------------------------------C
C
          AMX=ZERO	
          AMY=ZERO
          AMZ=ZERO
          VMX=ZERO
          VMY=ZERO
          VMZ=ZERO
C
          DO JJ=1,4
            J=IRECT(JJ,L)
            AMX=AMX+AR(1,J)*H(JJ)
            AMY=AMY+AR(2,J)*H(JJ)
            AMZ=AMZ+AR(3,J)*H(JJ)
            VMX=VMX+VR(1,J)*H(JJ)
            VMY=VMY+VR(2,J)*H(JJ)
            VMZ=VMZ+VR(3,J)*H(JJ)
          ENDDO
C
        ENDIF
C
        IF (IRODDL==1) THEN
          AR(1,I)=AMX
          AR(2,I)=AMY
          AR(3,I)=AMZ
          VR(1,I)=VMX
          VR(2,I)=VMY
          VR(3,I)=VMZ
        ENDIF
C
        VMXX = VMY*ZC0 - VMZ*YC0
        VMYY = VMZ*XC0 - VMX*ZC0
        VMZZ = VMX*YC0 - VMY*XC0
C
        A(1,I)= A(1,I) + AMY*ZC0 -AMZ*YC0 +HALF*(VMY*VMZZ-VMZ*VMYY)
        A(2,I)= A(2,I) + AMZ*XC0 -AMX*ZC0 +HALF*(VMZ*VMXX-VMX*VMZZ)
        A(3,I)= A(3,I) + AMX*YC0 -AMY*XC0 +HALF*(VMX*VMYY-VMY*VMXX)
        V(1,I)= V(1,I) + VMXX
        V(2,I)= V(2,I) + VMYY
        V(3,I)= V(3,I) + VMZZ
C
       ENDIF
C
 70   CONTINUE
      RETURN
      END
Chd|====================================================================
Chd|  I2VIT3_27                     source/interfaces/interf/i2vit3.F
Chd|-- called by -----------
Chd|        I2VIT27                       source/interfaces/interf/i2vit27.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2VIT3_27(
     1 NSN   , NMN   , A   , IRECT, CRST,
     2 MSR   , NSV   , IRTL, V    , MS  ,
     3 WEIGHT, MMASS                    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),WEIGHT(*)
C     REAL
      my_real
     .   A(*), CRST(2,*), V(*),MS(*), MMASS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, I3, J3, I2, J2, I1, J1, ISK, ICOD, II, L, JJ,
     .   NN
C     REAL
      my_real
     .   H(4), SS, TT, AMX, AMY, AMZ, VMX, VMY, VMZ,SP,SM,TP,TM,MAS
C-----------------------------------------------
      NIR=2
      IF(N2D==0)NIR=4
C
      DO II=1,NSN
        I=NSV(II)
        IF(I>0)THEN
          L=IRTL(II)
C
          SS=CRST(1,II)
          TT=CRST(2,II)

          IF (IRECT(3,L) == IRECT(4,L)) THEN
C--        Shape functions of triangles
            H(1) = SS
            H(2) = TT
            H(3) = ONE-SS-TT
            H(4) = ZERO
          ELSE
C--         Shape functions of quadrangles         
            SP = ONE + SS           
            SM = ONE - SS           
            TP = FOURTH*(ONE + TT)    
            TM = FOURTH*(ONE - TT)
C       
            H(1)=TM*SM           
            H(2)=TM*SP           
            H(3)=TP*SP           
            H(4)=TP*SM 
          ENDIF
C
          I3=3*I
          I2=I3-1
          I1=I2-1
          AMX=ZERO
          AMY=ZERO
          AMZ=ZERO
          VMX=ZERO
          VMY=ZERO
          VMZ=ZERO
C
          DO JJ=1,NIR
            J=IRECT(JJ,L)
            J3=3*J
            J2=J3-1
            J1=J2-1
            AMX=AMX+A(J1)*H(JJ)
            AMY=AMY+A(J2)*H(JJ)
            AMZ=AMZ+A(J3)*H(JJ)
            VMX=VMX+V(J1)*H(JJ)
            VMY=VMY+V(J2)*H(JJ)
            VMZ=VMZ+V(J3)*H(JJ)
          ENDDO
          A(I1)=AMX
          A(I2)=AMY
          A(I3)=AMZ
          V(I1)=VMX
          V(I2)=VMY
          V(I3)=VMZ
        ENDIF
C
      ENDDO
C-----------
      RETURN
      END