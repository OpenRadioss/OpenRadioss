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
Chd|  I2FOR27_CIN                   source/interfaces/interf/i2for27_cin.F
Chd|-- called by -----------
Chd|        I2FOR27                       source/interfaces/interf/i2for27.F
Chd|-- calls ---------------
Chd|        I2CIN_ROT27                   ../common_source/interf/i2cin_rot27.F
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        I2LOCEQ_27                    ../common_source/interf/i2loceq.F
Chd|        I2REP                         ../common_source/interf/i2rep.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FOR27_CIN(NSN    ,NMN    ,A      ,IRECT  ,CRST   ,
     2                   MSR    ,NSV    ,IRTL   ,MS     ,WEIGHT ,
     3                   STIFN  ,MMASS  ,IDEL2  ,SMASS  ,X      ,
     4                   V      ,FSAV   ,FNCONT ,INDXC  ,H3D_DATA,
     5                   IN     ,SINER  ,DPARA  ,MSEGTYP2,AR     ,
     6                   STIFR  ,CSTS_BIS,T2FAC_SMS,FNCONTP,FTCONTP)
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
      INTEGER NSN, NMN, IDEL2,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*),INDXC(NSN),MSEGTYP2(*)
C     REAL
      my_real
     .    X(3,*),V(3,*),A(3,*),MS(*),CRST(2,*),STIFN(*),MMASS(*),SMASS(*),
     .    FSAV(*),FNCONT(3,*),IN(*),SINER(*),DPARA(7,*),AR(3,*),STIFR(*),CSTS_BIS(2,*),
     .    T2FAC_SMS(*),FNCONTP(3,*)   ,FTCONTP(3,*) 
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, K, II, L, JJ,
     .        IX1,IX2,IX3,IX4
C     REAL
      my_real
     .   H(4),XMSJ, SS, ST, XMSI, FXI, FYI, FZI,SP,SM,TP,TM,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .   X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,X0,Y0,Z0,XS(3),XM(3),
     .   STIFM,FMX(4),FMY(4),FMZ(4),FX(4),FY(4),FZ(4),
     .   RX(4),RY(4),RZ(4),RS(3),FLX,FLY,FLZ,FAC_TRIANG,STBRK,
     .   MXS,MYS,MZS,STIFMR,DWDU,RM(3),BETAX,BETAY,H2(4),MXI,MYI,MZI
C=======================================================================
      NIR=2
      IF(N2D.EQ.0)NIR=4
C
      IF(IMPL_S.GT.0) THEN
      DO II=1,NSN
       K = INDXC(II)
       IF (K == 0) CYCLE
       I = NSV(K)
       IF(I.GT.0)THEN
        L=IRTL(II)
C
        XMSI=MS(I)*WEIGHT(I)
        FXI=A(1,I)*WEIGHT(I)
        FYI=A(2,I)*WEIGHT(I)
        FZI=A(3,I)*WEIGHT(I)
C
        IF (IRODDL == 1) THEN
          MXI=AR(1,I)*WEIGHT(I)
          MYI=AR(2,I)*WEIGHT(I)
          MZI=AR(3,I)*WEIGHT(I)
        ENDIF
C
        IX1 = IRECT(1,L)                                       
        IX2 = IRECT(2,L)                                       
        IX3 = IRECT(3,L)                                       
        IX4 = IRECT(4,L)
C
        IF (IX3 == IX4) THEN
C--       Shape functions of triangles
          NIR = 3
          H(1) = CRST(1,II)
          H(2) = CRST(2,II)
          H(3) = ONE-CRST(1,II)-CRST(2,II)
          H(4) = ZERO
          H2(1) = CSTS_BIS(1,II)
          H2(2) = CSTS_BIS(2,II)
          H2(3) = ONE-CSTS_BIS(1,II)-CSTS_BIS(2,II)
          H2(4) = ZERO
        ELSE
C--       Shape functions of quadrangles
          NIR = 4
          SS=CRST(1,II)                                    
          ST=CRST(2,II)
          SP=ONE + SS                                       
          SM=ONE - SS                                       
          TP=FOURTH*(ONE + ST)                               
          TM=FOURTH*(ONE - ST)                               
          H(1)=TM*SM                                       
          H(2)=TM*SP                                       
          H(3)=TP*SP                                       
          H(4)=TP*SM

C         Additional form functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
          SS=CSTS_BIS(1,II)                                    
          ST=CSTS_BIS(2,II)
          SP=ONE + SS                                       
          SM=ONE - SS                                       
          TP=FOURTH*(ONE + ST)                               
          TM=FOURTH*(ONE - ST)                               
          H2(1)=TM*SM                                       
          H2(2)=TM*SP                                       
          H2(3)=TP*SP                                       
          H2(4)=TP*SM  
        ENDIF
C
        IF (MSEGTYP2(L)==0) THEN
C
C--------------------------------------------------------------C
C--- solid main segment -- moment equilibrium----------------C
C--------------------------------------------------------------C
C               
C---- rep local facette main
C  
            X1  = X(1,IX1)                                       
            Y1  = X(2,IX1)                                          
            Z1  = X(3,IX1)                                          
            X2  = X(1,IX2)					     
            Y2  = X(2,IX2)					     
            Z2  = X(3,IX2)					     
            X3  = X(1,IX3)					     
            Y3  = X(2,IX3)					     
            Z3  = X(3,IX3)					     
            X4  = X(1,IX4)					     
            Y4  = X(2,IX4)					     
            Z4  = X(3,IX4)					     
            XS(1)  = X(1,I)                                          
            XS(2)  = X(2,I)                                         
            XS(3)  = X(3,I)                                           
C								
            CALL I2REP(X1     ,X2     ,X3     ,X4     ,
     .               Y1     ,Y2     ,Y3     ,Y4     ,
     .               Z1     ,Z2     ,Z3     ,Z4     ,
     .               E1X    ,E1Y    ,E1Z    ,
     .               E2X    ,E2Y    ,E2Z    ,
     .               E3X    ,E3Y    ,E3Z    ,NIR)

C                 
            IF (NIR == 4) THEN
              FAC_TRIANG = ONE
              X0  = FOURTH*(X1 + X2 + X3 + X4)
              Y0  = FOURTH*(Y1 + Y2 + Y3 + Y4)
              Z0  = FOURTH*(Z1 + Z2 + Z3 + Z4)
            ELSE
              FAC_TRIANG = ZERO                                       
              X0  = THIRD*(X1 + X2 + X3)
              Y0  = THIRD*(Y1 + Y2 + Y3)
              Z0  = THIRD*(Z1 + Z2 + Z3)
            ENDIF
C
            XS(1) = XS(1) - X0
            XS(2) = XS(2) - Y0
            XS(3) = XS(3) - Z0
C                                      
            X1 = X1 - X0                                              
            Y1 = Y1 - Y0                                              
            Z1 = Z1 - Z0                                              
            X2 = X2 - X0                                              
            Y2 = Y2 - Y0                                              
            Z2 = Z2 - Z0                                              
            X3 = X3 - X0                                              
            Y3 = Y3 - Y0                                              
            Z3 = Z3 - Z0                                              
            X4 = X4 - X0                                              
            Y4 = Y4 - Y0                                              
            Z4 = Z4 - Z0
            IF (NIR==3) THEN
              X4 = ZERO                                             
              Y4 = ZERO                                               
              Z4 = ZERO 
            END IF
C
            XM(1) = X1*H(1) + X2*H(2) + X3*H(3) + X4*H(4)
            XM(2) = Y1*H(1) + Y2*H(2) + Y3*H(3) + Y4*H(4)
            XM(3) = Z1*H(1) + Z2*H(2) + Z3*H(3) + Z4*H(4)
                                                
C---- computation of local coordinates
C
            RS(1) = XS(1)*E1X + XS(2)*E1Y + XS(3)*E1Z
            RS(2) = XS(1)*E2X + XS(2)*E2Y + XS(3)*E2Z
            RS(3) = XS(1)*E3X + XS(2)*E3Y + XS(3)*E3Z
C
            RM(1) = XM(1)*E1X + XM(2)*E1Y + XM(3)*E1Z
            RM(2) = XM(1)*E2X + XM(2)*E2Y + XM(3)*E2Z
            RM(3) = XM(1)*E3X + XM(2)*E3Y + XM(3)*E3Z
c
            RX(1) = E1X*X1 + E1Y*Y1 + E1Z*Z1        
            RY(1) = E2X*X1 + E2Y*Y1 + E2Z*Z1         
            RZ(1) = E3X*X1 + E3Y*Y1 + E3Z*Z1         
            RX(2) = E1X*X2 + E1Y*Y2 + E1Z*Z2         
            RY(2) = E2X*X2 + E2Y*Y2 + E2Z*Z2         
            RZ(2) = E3X*X2 + E3Y*Y2 + E3Z*Z2         
            RX(3) = E1X*X3 + E1Y*Y3 + E1Z*Z3         
            RY(3) = E2X*X3 + E2Y*Y3 + E2Z*Z3         
            RZ(3) = E3X*X3 + E3Y*Y3 + E3Z*Z3         
            RX(4) = E1X*X4 + E1Y*Y4 + E1Z*Z4         
            RY(4) = E2X*X4 + E2Y*Y4 + E2Z*Z4         
            RZ(4) = E3X*X4 + E3Y*Y4 + E3Z*Z4
C
C---- computation of kinematic parameters and stbrk - local coordinates
            CALL I2CIN_ROT27(STBRK,RS,RM,RX(1),RY(1),RZ(1),RX(2),RY(2),RZ(2),RX(3),RY(3),RZ(3),
     .                       RX(4),RY(4),RZ(4),DPARA(1,II),DWDU,E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .                       NIR,BETAX,BETAY)                                
C               
C---- computation of force in local skew
C
            FLX = FXI*E1X + FYI*E1Y + FZI*E1Z
            FLY = FXI*E2X + FYI*E2Y + FZI*E2Z
            FLZ = FXI*E3X + FYI*E3Y + FZI*E3Z
C
            DO J=1,4
              FMX(J) = H(J)*FLX
              FMY(J) = H(J)*FLY
              FMZ(J) = H(J)*FLZ
            ENDDO
C
C---- update main forces (moment balance) - local coordinates RX
            IF (IRODDL==1) THEN
              MXS = MXI*E1X + MYI*E1Y + MZI*E1Z
              MYS = MXI*E2X + MYI*E2Y + MZI*E2Z
              MZS = MXI*E3X + MYI*E3Y + MZI*E3Z
              
C--           moment balance + moment transfer
              CALL I2LOCEQ_27(NIR    ,RS     ,RX     ,RY     ,RZ      ,    
     .                        FMX    ,FMY    ,FMZ    ,H      ,STIFM   ,
     .                        MXS    ,MYS    ,MZS    ,STIFMR ,BETAX   ,
     .                        BETAY)
            ELSE
              MXS = ZERO
              MYS = ZERO
              MZS = ZERO

C--           moment balance
              CALL I2LOCEQ_27(NIR    ,RS     ,RX     ,RY     ,RZ      ,    
     .                        FMX    ,FMY    ,FMZ    ,H      ,STIFM   ,
     .                        MXS    ,MYS    ,MZS    ,STIFMR ,BETAX   ,
     .                        BETAY)
              STIFMR = ZERO
            ENDIF
C               
C---- computation of force in global skew
C
            DO J=1,4
              FX(J) = E1X*FMX(J) + E2X*FMY(J) + E3X*FMZ(J) 
              FY(J) = E1Y*FMX(J) + E2Y*FMY(J) + E3Y*FMZ(J) 
              FZ(J) = E1Z*FMX(J) + E2Z*FMY(J) + E3Z*FMZ(J) 
            ENDDO
C
        ELSE
C----------------------------------------------------C
C--- shell / shell or shell / solide connection  ----C
C----------------------------------------------------C
C
            STIFM=ZERO
            STBRK=ZERO
            STIFMR=ZERO
            DWDU=ZERO 
C
            DO J=1,4
              FX(J) = FXI*H(J)
              FY(J) = FYI*H(J)
              FZ(J) = FZI*H(J)
            ENDDO
C
        ENDIF
C
        IF(IRODDL/=0)THEN
          DO JJ=1,NIR
            J=IRECT(JJ,L)
            A(1,J)=A(1,J)+FX(JJ)
            A(2,J)=A(2,J)+FY(JJ)
            A(3,J)=A(3,J)+FZ(JJ)
            MS(J)=MS(J)+XMSI*H2(JJ)
            STIFN(J)=STIFN(J)+WEIGHT(I)*(STIFN(I)*(ONE+STBRK)*(ABS(H(JJ))+STIFM)+STIFR(I)*STIFMR*DWDU)
          ENDDO
        ELSE
          DO JJ=1,NIR
            J=IRECT(JJ,L)
            A(1,J)=A(1,J)+FX(JJ)
            A(2,J)=A(2,J)+FY(JJ)
            A(3,J)=A(3,J)+FZ(JJ)
            MS(J)=MS(J)+XMSI*H2(JJ)
            STIFN(J)=STIFN(J)+WEIGHT(I)*(STIFN(I)*(ONE+STBRK)*(ABS(H(JJ))+STIFM))
          ENDDO
        END IF
C 
C---    output of tied contact forces
 	CALL I2FORCES(X      ,V	,A	 ,MS	  ,I	  ,
     .	              IRECT(1,L),H	,NIR     ,FSAV    ,FNCONT ,
     .                FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C      
        IF(IRODDL.EQ.0)THEN
          IF(IDEL2.NE.0.AND.MS(I).NE.0.)SMASS(II)=MS(I)
          MS(I)=ZERO
          STIFN(I)=EM20
          A(1,I)=ZERO
          A(2,I)=ZERO
          A(3,I)=ZERO
        ENDIF
C            
       ENDIF
C----
      ENDDO
c
c
      ELSE
c
      DO II=1,NSN
       K = INDXC(II)
       IF (K == 0) CYCLE
       I = NSV(K)
       IF(I.GT.0)THEN
        L=IRTL(II)
C
        XMSI=MS(I)*WEIGHT(I)
        FXI=A(1,I)*WEIGHT(I)
        FYI=A(2,I)*WEIGHT(I)
        FZI=A(3,I)*WEIGHT(I)
C
        IF (IRODDL == 1) THEN
          MXI=AR(1,I)*WEIGHT(I)
          MYI=AR(2,I)*WEIGHT(I)
          MZI=AR(3,I)*WEIGHT(I)
        ENDIF
C
        IX1 = IRECT(1,L)                                       
        IX2 = IRECT(2,L)                                       
        IX3 = IRECT(3,L)                                       
        IX4 = IRECT(4,L)
C
        IF (IX3 == IX4) THEN
C--       Shape functions of triangles
          NIR = 3
          H(1) = CRST(1,II)
          H(2) = CRST(2,II)
          H(3) = ONE-CRST(1,II)-CRST(2,II)
          H(4) = ZERO
          H2(1) = CSTS_BIS(1,II)
          H2(2) = CSTS_BIS(2,II)
          H2(3) = ONE-CSTS_BIS(1,II)-CSTS_BIS(2,II)
          H2(4) = ZERO
        ELSE
C--       Shape functions of quadrangles
          NIR = 4
          SS=CRST(1,II)                                    
          ST=CRST(2,II)
          SP=ONE + SS                                       
          SM=ONE - SS                                       
          TP=FOURTH*(ONE + ST)                               
          TM=FOURTH*(ONE - ST)                               
          H(1)=TM*SM                                       
          H(2)=TM*SP                                       
          H(3)=TP*SP                                       
          H(4)=TP*SM

C         Additional form functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
          SS=CSTS_BIS(1,II)                                    
          ST=CSTS_BIS(2,II)
          SP=ONE + SS                                       
          SM=ONE - SS                                       
          TP=FOURTH*(ONE + ST)                               
          TM=FOURTH*(ONE - ST)                               
          H2(1)=TM*SM                                       
          H2(2)=TM*SP                                       
          H2(3)=TP*SP                                       
          H2(4)=TP*SM  
        ENDIF
C
        IF (MSEGTYP2(L)==0) THEN
C
C--------------------------------------------------------------C
C--- solid main segment -- moment equilibrium----------------C
C--------------------------------------------------------------C
C          
C---- rep local facette main
C  
            X1  = X(1,IX1)                                       
            Y1  = X(2,IX1)                                          
            Z1  = X(3,IX1)                                          
            X2  = X(1,IX2)					     
            Y2  = X(2,IX2)					     
            Z2  = X(3,IX2)					     
            X3  = X(1,IX3)					     
            Y3  = X(2,IX3)					     
            Z3  = X(3,IX3)					     
            X4  = X(1,IX4)					     
            Y4  = X(2,IX4)					     
            Z4  = X(3,IX4)					     
            XS(1)  = X(1,I)                                          
            XS(2)  = X(2,I)                                         
            XS(3)  = X(3,I)                                           
C								
            CALL I2REP(X1     ,X2     ,X3     ,X4     ,
     .               Y1     ,Y2     ,Y3     ,Y4     ,
     .               Z1     ,Z2     ,Z3     ,Z4     ,
     .               E1X    ,E1Y    ,E1Z    ,
     .               E2X    ,E2Y    ,E2Z    ,
     .               E3X    ,E3Y    ,E3Z    ,NIR)

C                 
            IF (NIR == 4) THEN
              FAC_TRIANG = ONE
              X0  = FOURTH*(X1 + X2 + X3 + X4)
              Y0  = FOURTH*(Y1 + Y2 + Y3 + Y4)
              Z0  = FOURTH*(Z1 + Z2 + Z3 + Z4)
            ELSE
              FAC_TRIANG = ZERO                                       
              X0  = THIRD*(X1 + X2 + X3)
              Y0  = THIRD*(Y1 + Y2 + Y3)
              Z0  = THIRD*(Z1 + Z2 + Z3)
            ENDIF
C
            XS(1) = XS(1) - X0
            XS(2) = XS(2) - Y0
            XS(3) = XS(3) - Z0
C                                      
            X1 = X1 - X0                                              
            Y1 = Y1 - Y0                                              
            Z1 = Z1 - Z0                                              
            X2 = X2 - X0                                              
            Y2 = Y2 - Y0                                              
            Z2 = Z2 - Z0                                              
            X3 = X3 - X0                                              
            Y3 = Y3 - Y0                                              
            Z3 = Z3 - Z0                                              
            X4 = X4 - X0                                              
            Y4 = Y4 - Y0                                              
            Z4 = Z4 - Z0
            IF (NIR==3) THEN
              X4 = ZERO                                             
              Y4 = ZERO                                               
              Z4 = ZERO 
            END IF                                              
c
            XM(1) = X1*H(1) + X2*H(2) + X3*H(3) + X4*H(4)
            XM(2) = Y1*H(1) + Y2*H(2) + Y3*H(3) + Y4*H(4)
            XM(3) = Z1*H(1) + Z2*H(2) + Z3*H(3) + Z4*H(4)
                                               
C---- computation of local coordinates
C
            RS(1) = XS(1)*E1X + XS(2)*E1Y + XS(3)*E1Z
            RS(2) = XS(1)*E2X + XS(2)*E2Y + XS(3)*E2Z
            RS(3) = XS(1)*E3X + XS(2)*E3Y + XS(3)*E3Z
C
            RM(1) = XM(1)*E1X + XM(2)*E1Y + XM(3)*E1Z
            RM(2) = XM(1)*E2X + XM(2)*E2Y + XM(3)*E2Z
            RM(3) = XM(1)*E3X + XM(2)*E3Y + XM(3)*E3Z
c
            RX(1) = E1X*X1 + E1Y*Y1 + E1Z*Z1        
            RY(1) = E2X*X1 + E2Y*Y1 + E2Z*Z1         
            RZ(1) = E3X*X1 + E3Y*Y1 + E3Z*Z1         
            RX(2) = E1X*X2 + E1Y*Y2 + E1Z*Z2         
            RY(2) = E2X*X2 + E2Y*Y2 + E2Z*Z2         
            RZ(2) = E3X*X2 + E3Y*Y2 + E3Z*Z2         
            RX(3) = E1X*X3 + E1Y*Y3 + E1Z*Z3         
            RY(3) = E2X*X3 + E2Y*Y3 + E2Z*Z3         
            RZ(3) = E3X*X3 + E3Y*Y3 + E3Z*Z3         
            RX(4) = E1X*X4 + E1Y*Y4 + E1Z*Z4         
            RY(4) = E2X*X4 + E2Y*Y4 + E2Z*Z4         
            RZ(4) = E3X*X4 + E3Y*Y4 + E3Z*Z4
C
C---- computation of kinematic parameters and stbrk - local coordinates
            CALL I2CIN_ROT27(STBRK,RS,RM,RX(1),RY(1),RZ(1),RX(2),RY(2),RZ(2),RX(3),RY(3),RZ(3),
     .                       RX(4),RY(4),RZ(4),DPARA(1,II),DWDU,E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .                       NIR,BETAX,BETAY)                                       
C               
C---- computation of force in local skew
C
            FLX = FXI*E1X + FYI*E1Y + FZI*E1Z
            FLY = FXI*E2X + FYI*E2Y + FZI*E2Z
            FLZ = FXI*E3X + FYI*E3Y + FZI*E3Z
C
            DO J=1,4
              FMX(J) = H(J)*FLX
              FMY(J) = H(J)*FLY
              FMZ(J) = H(J)*FLZ
            ENDDO
C
C---- update main forces (moment balance)
            IF (IRODDL==1) THEN
              MXS = MXI*E1X + MYI*E1Y + MZI*E1Z
              MYS = MXI*E2X + MYI*E2Y + MZI*E2Z
              MZS = MXI*E3X + MYI*E3Y + MZI*E3Z
              
C--           moment balance + moment transfer
              CALL I2LOCEQ_27(NIR    ,RS     ,RX     ,RY     ,RZ      ,    
     .                        FMX    ,FMY    ,FMZ    ,H      ,STIFM   ,
     .                        MXS    ,MYS    ,MZS    ,STIFMR ,BETAX   ,
     .                        BETAY)
            ELSE
              MXS = ZERO
              MYS = ZERO
              MZS = ZERO

C--           moment balance
              CALL I2LOCEQ_27(NIR    ,RS     ,RX     ,RY     ,RZ      ,    
     .                        FMX    ,FMY    ,FMZ    ,H      ,STIFM   ,
     .                        MXS    ,MYS    ,MZS    ,STIFMR ,BETAX   ,
     .                        BETAY)
              STIFMR = ZERO
            ENDIF
C               
C---- computation of force in global skew
C
            DO J=1,4
              FX(J) = E1X*FMX(J) + E2X*FMY(J) + E3X*FMZ(J) 
              FY(J) = E1Y*FMX(J) + E2Y*FMY(J) + E3Y*FMZ(J) 
              FZ(J) = E1Z*FMX(J) + E2Z*FMY(J) + E3Z*FMZ(J) 
            ENDDO
C
        ELSE
C----------------------------------------------------C
C--- shell / shell or shell / solide connection  ----C
C----------------------------------------------------C
C
            STIFM=ZERO
            STBRK=ZERO
            STIFMR=ZERO
            DWDU=ZERO  
C
            DO J=1,4
              FX(J) = FXI*H(J)
              FY(J) = FYI*H(J)
              FZ(J) = FZI*H(J)
            ENDDO
C
        ENDIF
C
        IF(IRODDL/=0)THEN
          DO JJ=1,NIR
            J=IRECT(JJ,L)
            A(1,J)=A(1,J)+FX(JJ)
            A(2,J)=A(2,J)+FY(JJ)
            A(3,J)=A(3,J)+FZ(JJ)
            MS(J)=MS(J)+XMSI*H2(JJ)
            STIFN(J)=STIFN(J)+WEIGHT(I)*(STIFN(I)*(ONE+STBRK)*(ABS(H(JJ))+STIFM)+STIFR(I)*STIFMR*DWDU)
          ENDDO
        ELSE
          DO JJ=1,NIR
            J=IRECT(JJ,L)
            A(1,J)=A(1,J)+FX(JJ)
            A(2,J)=A(2,J)+FY(JJ)
            A(3,J)=A(3,J)+FZ(JJ)
            MS(J)=MS(J)+XMSI*H2(JJ)
            STIFN(J)=STIFN(J)+WEIGHT(I)*(STIFN(I)*(ONE+STBRK)*(ABS(H(JJ))+STIFM))
          ENDDO
        END IF
C
        IF(IDTMINS==2.OR.IDTMINS_INT/=0) THEN
C----   For AMS scaling factor on stiffness is stored - only used for solid main surface
          T2FAC_SMS(I) = (ONE+STBRK)*(ONE+STIFM)
        ENDIF
C
C---    output of tied contact forces
 	CALL I2FORCES(X      ,V	,A	 ,MS	  ,I	  ,
     .	              IRECT(1,L),H	,NIR     ,FSAV    ,FNCONT ,
     .                FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C
        IF(IRODDL.EQ.0)THEN
          IF(IDEL2.NE.0.AND.MS(I).NE.0.) SMASS(II)=MS(I)
          MS(I)=ZERO
          STIFN(I)=EM20
          A(1,I)=ZERO
          A(2,I)=ZERO
          A(3,I)=ZERO
        ENDIF                        
C----
       ENDIF
C
      ENDDO
      ENDIF
C
      RETURN
      END
