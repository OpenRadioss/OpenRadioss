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
Chd|  I2FOR28P_CIN                  source/interfaces/interf/i2for28p_cin.F
Chd|-- called by -----------
Chd|        I2FOR28P                      source/interfaces/interf/i2for28p.F
Chd|-- calls ---------------
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FOR28P_CIN(
     1             NSN      ,NMN     ,A      ,IRECT   ,DPARA   ,
     2             MSR      ,NSV     ,IRTL   ,MS      ,WEIGHT  ,
     3             AR       ,IN      ,X      ,STIFN   ,STIFR   ,
     4             FSKYI2   ,IADI2   ,FSAV   ,DMAST   ,ADM     ,
     5             MMASS    ,I0      ,NIR    ,I2SIZE  ,IDEL2   ,
     6             SMASS    ,SINER   ,V      ,CRST    ,FNCONT  ,
     7             INDXC    ,IADX    ,H3D_DATA,FNCONTP,FTCONTP )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN, I0, NIR, I2SIZE, IDEL2,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*),
     .   IADI2(NIR,*),INDXC(*),IADX(*)
C     REAL
      my_real
     .   A(3,*),AR(3,*), X(3,*),V(*), FSKYI2(I2SIZE,*),MMASS(*),
     .   DPARA(7,*), MS(*), IN(*),STIFN(*),STIFR(*),DMAST,ADM(*),
     .   SMASS(*), SINER(*),FSAV(*), CRST(2,*),FNCONT(3,*),
     .   FNCONTP(3,*) ,FTCONTP(3,*) 
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K,J1,J2,J3,J4, II, L, JJ, NN,NISKY2,I0BASE
C     REAL
      my_real
     .   H(4),
     .   S,T,SS, ST, XMSI, FXS, FYS, FZS,SP,SM,TP,TM,
     .   MX,MY,MZ,DET,FX0,FY0,FZ0,INS,
     .   X0,X1,X2,X3,X4,XS,Y0,Y1,Y2,Y3,Y4,YS,Z0,Z1,Z2,Z3,Z4,ZS,
     .   X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .   XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .   A1,A2,A3,B1,B2,B3,C1,C2,C3,MR,MRX,MRY,MRZ,INX,INY,INZ,STF,FACT
C=======================================================================
C
C------------------------------
C     DUPLICATED FROM I2FOMO3P - SPOTFLAG1 FORMULATION
C------------------------------
C
      I0BASE = I0
C
C Traitement specifique pour DMAS
C
C MMASS(II) initialise a MS(J) a t=0 dans starter
      IF(ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS  >0) THEN
        DO II=1,NMN
          J=MSR(II)
          ADM(J) = ADM(J)*MMASS(II)
        ENDDO
      ENDIF
C
C------------------------------
C     FORCES ET MOMENTS DES NOEUDS SECONDS 
C     TRANSMIS AUX NOEUDS MAINS SOUS 
C     FORME DE FORCES 
C
C     MASSES ET INERTIES DES NOEUDS SECONDS 
C     TRANSMISES AUX NOEUDS MAINS SOUS 
C     FORME DE MASSES 
C------------------------------
C
#include "vectorize.inc"
      DO II=1,NSN
       K = INDXC(II)                                      
       IF (K == 0) CYCLE                                  
       I = NSV(K)
       IF(I>0)THEN
        L=IRTL(II)
C
        S = CRST(1,II)    
        T = CRST(2,II)    
        SP=ONE + S          
        SM=ONE - S          
        TP=FOURTH*(ONE + T)  
        TM=FOURTH*(ONE - T)
C    
        H(1)=ONE/NIR                                         
        H(2)=ONE/NIR                                          
        H(3)=ONE/NIR                                          
        H(4)=ONE/NIR                                          
C
        J1=IRECT(1,L)
        J2=IRECT(2,L)
        J3=IRECT(3,L)
        J4=IRECT(4,L)
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
        X12=X1*X1
        X22=X2*X2
        X32=X3*X3
        X42=X4*X4 
        Y12=Y1*Y1
        Y22=Y2*Y2
        Y32=Y3*Y3
        Y42=Y4*Y4 
        Z12=Z1*Z1 
        Z22=Z2*Z2
        Z32=Z3*Z3 
        Z42=Z4*Z4 
        XX=X12 + X22 + X32 + X42 
        YY=Y12 + Y22 + Y32 + Y42 
        ZZ=Z12 + Z22 + Z32 + Z42 
        XY=X1*Y1 + X2*Y2 + X3*Y3 + X4*Y4 
        YZ=Y1*Z1 + Y2*Z2 + Y3*Z3 + Y4*Z4 
        ZX=Z1*X1 + Z2*X2 + Z3*X3 + Z4*X4
        ZZZ=XX+YY
        XXX=YY+ZZ
        YYY=ZZ+XX 
        XY2=XY*XY
        YZ2=YZ*YZ
        ZX2=ZX*ZX
        DET= XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 
     .                                       - TWO*XY*YZ*ZX
        DET=ONE/DET
        B1=ZZZ*YYY-YZ2
        B2=XXX*ZZZ-ZX2
        B3=YYY*XXX-XY2
        C3=ZZZ*XY+YZ*ZX
        C1=XXX*YZ+ZX*XY
        C2=YYY*ZX+XY*YZ
C
        DPARA(1,II)=DET
        DPARA(2,II)=B1
        DPARA(3,II)=B2
        DPARA(4,II)=B3
        DPARA(5,II)=C1
        DPARA(6,II)=C2
        DPARA(7,II)=C3
C
        IF (WEIGHT(I)==1) THEN
         XMSI=MS(I)
         FXS=A(1,I)
         FYS=A(2,I)
         FZS=A(3,I) 
         IF (IRODDL==1) THEN
           INS=IN(I)
           MX=AR(1,I) + YS*FZS - ZS*FYS
           MY=AR(2,I) + ZS*FXS - XS*FZS
           MZ=AR(3,I) + XS*FYS - YS*FXS
         ELSE
           INS=ZERO
           MX=YS*FZS - ZS*FYS
           MY=ZS*FXS - XS*FZS
           MZ=XS*FYS - YS*FXS
         ENDIF
C
         A1=DET*(MX*B1+MY*C3+MZ*C2)
         A2=DET*(MY*B2+MZ*C1+MX*C3)
         A3=DET*(MZ*B3+MX*C2+MY*C1)
C
         FX0=FXS*FOURTH
         FY0=FYS*FOURTH
         FZ0=FZS*FOURTH
C
C------------------------------------------------------
C     INERTIES => MASSES
C------------------------------------------------------
C
         INX=INS + XMSI*(XS*XS+YS*YS+ZS*ZS)
         MRX = (B1+C3+C2)
         MRY = (B2+C1+C3)
         MRZ = (B3+C2+C1)
         MR=DET*INX*MAX(MRX,MRY,MRZ)
C
C------------------------------------------------------
C     MASSES & FORCES TRANSMISES AUX NOEUDS MAINS
C------------------------------------------------------
C
         FACT = ONE
         IF (IRODDL==1) THEN
         IF ((IN(J1)>ZERO.AND.IN(J2)>ZERO.AND.IN(J3)>ZERO.AND.IN(J4)>ZERO)) THEN
C--        Inertie transmise sous forme d'inertie
           FACT = ZERO
         ENDIF
         ENDIF
C
         XMSI=FOURTH*XMSI+MR*FACT
C
         IF (IRODDL == 1) THEN
           STF = FOURTH*STIFN(I)+ DET*MAX(MRX,MRY,MRZ)*(STIFR(I)+STIFN(I)*(XS*XS+YS*YS+ZS*ZS))
         ELSE
           STF = FOURTH*STIFN(I)+ DET*MAX(MRX,MRY,MRZ)*(STIFN(I)*(XS*XS+YS*YS+ZS*ZS))
         ENDIF
C
         I0 = I0BASE + IADX(K)
         NN = IADI2(1,I0)
         FSKYI2(1,NN) = FX0 + A2*Z1 - A3*Y1
         FSKYI2(2,NN) = FY0 + A3*X1 - A1*Z1
         FSKYI2(3,NN) = FZ0 + A1*Y1 - A2*X1
         FSKYI2(4,NN) = XMSI
         FSKYI2(5,NN) = STF
         IF (IRODDL == 1) THEN
           FSKYI2(6,NN) = ZERO
           FSKYI2(7,NN) = ZERO
           FSKYI2(8,NN) = ZERO
           FSKYI2(9,NN) = INX*FOURTH*(ONE-FACT)
           FSKYI2(10,NN)= ZERO
         ENDIF
C
         NN = IADI2(2,I0)
         FSKYI2(1,NN) = FX0 + A2*Z2 - A3*Y2
         FSKYI2(2,NN) = FY0 + A3*X2 - A1*Z2
         FSKYI2(3,NN) = FZ0 + A1*Y2 - A2*X2
         FSKYI2(4,NN) = XMSI
         FSKYI2(5,NN) = STF
         IF (IRODDL == 1) THEN
           FSKYI2(6,NN) = ZERO
           FSKYI2(7,NN) = ZERO
           FSKYI2(8,NN) = ZERO
           FSKYI2(9,NN) = INX*FOURTH*(ONE-FACT)
           FSKYI2(10,NN)= ZERO
         ENDIF
C
         NN = IADI2(3,I0)
         FSKYI2(1,NN) = FX0 + A2*Z3 - A3*Y3
         FSKYI2(2,NN) = FY0 + A3*X3 - A1*Z3
         FSKYI2(3,NN) = FZ0 + A1*Y3 - A2*X3
         FSKYI2(4,NN) = XMSI
         FSKYI2(5,NN) = STF
         IF (IRODDL == 1) THEN
           FSKYI2(6,NN) = ZERO
           FSKYI2(7,NN) = ZERO
           FSKYI2(8,NN) = ZERO
           FSKYI2(9,NN) = INX*FOURTH*(ONE-FACT)
           FSKYI2(10,NN)= ZERO
         ENDIF  
C
         NN = IADI2(4,I0)
         FSKYI2(1,NN) =  FX0 + A2*Z4 - A3*Y4
         FSKYI2(2,NN) =  FY0 + A3*X4 - A1*Z4
         FSKYI2(3,NN) =  FZ0 + A1*Y4 - A2*X4
         FSKYI2(4,NN) = XMSI
         FSKYI2(5,NN) = STF
         IF (IRODDL == 1) THEN
           FSKYI2(6,NN) = ZERO
           FSKYI2(7,NN) = ZERO
           FSKYI2(8,NN) = ZERO
           FSKYI2(9,NN) = INX*FOURTH*(ONE-FACT)
           FSKYI2(10,NN)= ZERO
         ENDIF  
C
         DMAST = DMAST + 4.*XMSI - MS(I)
C
         IF (ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS >0) THEN
          ADM(J1) = ADM(J1) + XMSI - FOURTH*MS(I)
          ADM(J2) = ADM(J2) + XMSI - FOURTH*MS(I)
          ADM(J3) = ADM(J3) + XMSI - FOURTH*MS(I)
          ADM(J4) = ADM(J4) + XMSI - FOURTH*MS(I)
         ENDIF
        ENDIF
C
        IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
        MS(I)=ZERO
        STIFN(I)=EM20
C
        IF (IRODDL==1) THEN
          IF(IDEL2/=0.AND.IN(I)/=0.)SINER(II)=IN(I)
          IN(I)=ZERO
          STIFR(I)=EM20
        ENDIF
C
C---    output of tied contact forces
          CALL I2FORCES(X      ,V  ,A   ,MS    ,I    ,
     .                IRECT(1,L),H  ,NIR     ,FSAV    ,FNCONT ,
     .                FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)   
C----
C stokage ZERO pour noeuds delete par idel2
       ELSEIF(WEIGHT(-I)==1) THEN
         I0 = I0BASE + IADX(K)
         NN = IADI2(1,I0)
         FSKYI2(1,NN) = ZERO
         FSKYI2(2,NN) = ZERO
         FSKYI2(3,NN) = ZERO
         FSKYI2(4,NN) = ZERO
         FSKYI2(5,NN) = ZERO
         IF (IRODDL == 1) THEN
           FSKYI2(6,NN) = ZERO
           FSKYI2(7,NN) = ZERO
           FSKYI2(8,NN) = ZERO
           FSKYI2(9,NN) = ZERO
           FSKYI2(10,NN)= ZERO
         ENDIF
         NN = IADI2(2,I0)
         FSKYI2(1,NN) = ZERO
         FSKYI2(2,NN) = ZERO
         FSKYI2(3,NN) = ZERO
         FSKYI2(4,NN) = ZERO
         FSKYI2(5,NN) = ZERO
         IF (IRODDL == 1) THEN
           FSKYI2(6,NN) = ZERO
           FSKYI2(7,NN) = ZERO
           FSKYI2(8,NN) = ZERO
           FSKYI2(9,NN) = ZERO
           FSKYI2(10,NN)= ZERO
         ENDIF
         FSKYI2(10,NN)= ZERO
         NN = IADI2(3,I0)
         FSKYI2(1,NN) = ZERO
         FSKYI2(2,NN) = ZERO
         FSKYI2(3,NN) = ZERO
         FSKYI2(4,NN) = ZERO
         FSKYI2(5,NN) = ZERO
         IF (IRODDL == 1) THEN
           FSKYI2(6,NN) = ZERO
           FSKYI2(7,NN) = ZERO
           FSKYI2(8,NN) = ZERO
           FSKYI2(9,NN) = ZERO
           FSKYI2(10,NN)= ZERO
         ENDIF  
         NN = IADI2(4,I0)
         FSKYI2(1,NN) = ZERO
         FSKYI2(2,NN) = ZERO
         FSKYI2(3,NN) = ZERO
         FSKYI2(4,NN) = ZERO
         FSKYI2(5,NN) = ZERO
         IF (IRODDL == 1) THEN
           FSKYI2(6,NN) = ZERO
           FSKYI2(7,NN) = ZERO
           FSKYI2(8,NN) = ZERO
           FSKYI2(9,NN) = ZERO
           FSKYI2(10,NN)= ZERO
         ENDIF
C
       ENDIF
      ENDDO
C
C Traitement specifique pour ADM
C
      IF(ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS  >0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          J=MSR(II)
          ADM(J) = ADM(J)/MMASS(II)
        ENDDO
      ENDIF
C
      RETURN
      END
