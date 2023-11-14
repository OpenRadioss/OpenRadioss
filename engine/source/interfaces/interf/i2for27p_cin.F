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
Chd|  I2FOR27P_CIN                  source/interfaces/interf/i2for27p_cin.F
Chd|-- called by -----------
Chd|        I2FOR27P                      source/interfaces/interf/i2for27p.F
Chd|-- calls ---------------
Chd|        I2CIN_ROT27                   ../common_source/interf/i2cin_rot27.F
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        I2LOCEQ_27                    ../common_source/interf/i2loceq.F
Chd|        I2REP                         ../common_source/interf/i2rep.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FOR27P_CIN(
     1           NSN    ,NMN    ,A      ,CRST   ,NSV    ,         
     2           MS     ,WEIGHT ,STIFN  ,MMASS  ,FSKYI2 ,         
     3           IADI2  ,I0     ,NIR    ,I2SIZE ,IDEL2  ,         
     4           SMASS  ,IRECT  ,X      ,V      ,FSAV   ,         
     5           FNCONT  ,IRTL  ,INDXC  ,IADX   ,H3D_DATA,
     6           IN      ,SINER ,DPARA  ,MSEGTYP2,AR    ,
     7           STIFR   ,CSTS_BIS,T2FAC_SMS,FNCONTP,FTCONTP) 
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
     .   IRECT(4,*),IADI2(NIR,*), NSV(*),  WEIGHT(*), IRTL(*),
     .   INDXC(NSN),IADX(NSN),MSEGTYP2(*)
C     REAL
      my_real
     .   X(3,*),V(3,*),A(3,*), CRST(2,*), MS(*), STIFN(*), MMASS(*),
     .   FSKYI2(I2SIZE,*), SMASS(*),FSAV(*),FNCONT(3,*),IN(*),SINER(*),
     .   DPARA(7,*),AR(3,*),STIFR(*),CSTS_BIS(2,*),T2FAC_SMS(*),
     .   FNCONTP(3,*) ,FTCONTP(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2, I3, II, NN, L, J,K, JJ, I0BASE,
     .        IX1,IX2,IX3,IX4,NIRL
C     REAL
      my_real
     .   SS, ST, XMSI, FXI, FYI, FZI,SP,SM,TP,TM,
     .   H(4),E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .   X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,X0,Y0,Z0,XS(3),XM(3),
     .   STIFM,FMX(4),FMY(4),FMZ(4),FX(4),FY(4),FZ(4),
     .   RX(4),RY(4),RZ(4),RS(3),FLX,FLY,FLZ,FAC_TRIANG,DWDU,STBRK,
     .   MXS,MYS,MZS,STIFMR,RM(3),BETAX,BETAY,H2(4)
C=======================================================================
      I0BASE = I0
#include "vectorize.inc"
      DO II=1,NSN                                         
        K = INDXC(II)                                      
        IF (K == 0) CYCLE                                  
        I = NSV(K)
C                                         
        IF (I > 0) THEN                                    
         L=IRTL(II)                                        
         I3=3*I                                            
         I2=I3-1                                           
         I1=I2-1                                                                    
C
         IX1 = IRECT(1,L)                                       
         IX2 = IRECT(2,L)                                       
         IX3 = IRECT(3,L)                                       
         IX4 = IRECT(4,L)
C
         IF (IX3 == IX4) THEN
C--        Shape functions of triangles
           NIRL = 3
           H(1) = CRST(1,II)
           H(2) = CRST(2,II)
           H(3) = ONE-CRST(1,II)-CRST(2,II)
           H(4) = ZERO
           H2(1) = CSTS_BIS(1,II)
           H2(2) = CSTS_BIS(2,II)
           H2(3) = ONE-CSTS_BIS(1,II)-CSTS_BIS(2,II)
           H2(4) = ZERO
         ELSE
C--        Shape functions of quadrangles
           NIRL = 4
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

C          Additional form functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
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
C--------------------------------------------------C
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
     .               E3X    ,E3Y    ,E3Z    ,NIRL)
C                 
            IF (NIRL == 4) THEN
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
            IF (NIRL==3) THEN
              X4 = ZERO                                             
              Y4 = ZERO                                               
              Z4 = ZERO 
            END IF
C  
            XM(1) = X1*H(1) + X2*H(2) + X3*H(3) + X4*H(4)
            XM(2) = Y1*H(1) + Y2*H(2) + Y3*H(3) + Y4*H(4)
            XM(3) = Z1*H(1) + Z2*H(2) + Z3*H(3) + Z4*H(4)
C
C---- computation of local coordinates                                          
C
            RS(1) = XS(1)*E1X + XS(2)*E1Y + XS(3)*E1Z
            RS(2) = XS(1)*E2X + XS(2)*E2Y + XS(3)*E2Z
            RS(3) = XS(1)*E3X + XS(2)*E3Y + XS(3)*E3Z
C
            RM(1) = XM(1)*E1X + XM(2)*E1Y + XM(3)*E1Z
            RM(2) = XM(1)*E2X + XM(2)*E2Y + XM(3)*E2Z
            RM(3) = XM(1)*E3X + XM(2)*E3Y + XM(3)*E3Z
C
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
     .                       NIRL,BETAX,BETAY)
C
            IF (WEIGHT(I) == 1) THEN                                                             
C               
C---- computation of force in local skew
C
              FLX = A(1,I)*E1X + A(2,I)*E1Y + A(3,I)*E1Z
              FLY = A(1,I)*E2X + A(2,I)*E2Y + A(3,I)*E2Z
              FLZ = A(1,I)*E3X + A(2,I)*E3Y + A(3,I)*E3Z
C
              DO J=1,4
                FMX(J) = H(J)*FLX
                FMY(J) = H(J)*FLY
                FMZ(J) = H(J)*FLZ
              ENDDO
C
C---- update main forces (moment balance) - local coordinates RX
              IF (IRODDL==1) THEN
                MXS = AR(1,I)*E1X + AR(2,I)*E1Y + AR(3,I)*E1Z
                MYS = AR(1,I)*E2X + AR(2,I)*E2Y + AR(3,I)*E2Z
                MZS = AR(1,I)*E3X + AR(2,I)*E3Y + AR(3,I)*E3Z
              
C--           moment balance + moment transfer
                CALL I2LOCEQ_27(NIRL    ,RS     ,RX     ,RY     ,RZ      ,    
     .                          FMX     ,FMY    ,FMZ    ,H      ,STIFM   ,
     .                          MXS     ,MYS    ,MZS    ,STIFMR ,BETAX   ,
     .                          BETAY)
              ELSE
                MXS = ZERO
                MYS = ZERO
                MZS = ZERO

C--           moment balance
                CALL I2LOCEQ_27(NIRL    ,RS     ,RX     ,RY     ,RZ      ,    
     .                          FMX     ,FMY    ,FMZ    ,H      ,STIFM   ,
     .                          MXS     ,MYS    ,MZS    ,STIFMR ,BETAX   ,
     .                          BETAY)
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
            ENDIF
C
          ELSEIF (WEIGHT(I) == 1) THEN  
C----------------------------------------------------C
C--- shell / shell or shell / solide connection  ----C
C----------------------------------------------------C
C
            FAC_TRIANG=ONE
            STIFM=ZERO
            STIFMR = ZERO
            STBRK=ZERO
            DWDU=ZERO                                     
            FXI=A(1,I)                                        
            FYI=A(2,I)                                        
            FZI=A(3,I)
C
            DO J=1,4
              FX(J) = FXI*H(J)
              FY(J) = FYI*H(J)
              FZ(J) = FZI*H(J)
            ENDDO
C
          ENDIF
C                                                                            
C--------------------------------------------------C
C
         IF (WEIGHT(I) == 1) THEN
C
           XMSI=MS(I)
C
           I0 = I0BASE + IADX(K)                                
           NN = IADI2(1,I0)                               
           FSKYI2(1,NN) = FX(1)                          
           FSKYI2(2,NN) = FY(1)                          
           FSKYI2(3,NN) = FZ(1)                         
           FSKYI2(4,NN) = XMSI*H2(1)                         
           FSKYI2(5,NN) = STIFN(I)*(ONE+STBRK)*(ABS(H(1))+STIFM)+STIFR(I)*STIFMR*DWDU               
C
           NN = IADI2(2,I0)                                 
           FSKYI2(1,NN) = FX(2)                          
           FSKYI2(2,NN) = FY(2)                          
           FSKYI2(3,NN) = FZ(2)                         
           FSKYI2(4,NN) = XMSI*H2(2)                         
           FSKYI2(5,NN) = STIFN(I)*(ONE+STBRK)*(ABS(H(2))+STIFM)+STIFR(I)*STIFMR*DWDU                     
C
           NN = IADI2(3,I0)                                 
           FSKYI2(1,NN) = FX(3)                         
           FSKYI2(2,NN) = FY(3)                         
           FSKYI2(3,NN) = FZ(3)                         
           FSKYI2(4,NN) = XMSI*H2(3)                         
           FSKYI2(5,NN) = STIFN(I)*(ONE+STBRK)*(ABS(H(3))+STIFM)+STIFR(I)*STIFMR*DWDU
C
           NN = IADI2(4,I0)                                 
           FSKYI2(1,NN) = FX(4)                          
           FSKYI2(2,NN) = FY(4)                          
           FSKYI2(3,NN) = FZ(4)                          
           FSKYI2(4,NN) = XMSI*H2(4)                         
           FSKYI2(5,NN) = STIFN(I)*(ONE+STBRK)*(ABS(H(4))+STIFM*FAC_TRIANG)+STIFR(I)*STIFMR*DWDU*FAC_TRIANG
C
           IF(IDTMINS==2.OR.IDTMINS_INT/=0) THEN
C---- For AMS scaling factor on stiffness is stored - only used for solid main surface
             T2FAC_SMS(I) = (ONE+STBRK)*(ONE+STIFM)
           ENDIF     
C                   
         ENDIF
C                                             
C---     output of tied contact forces
           CALL I2FORCES(X      ,V  ,A   ,MS    ,I    ,
     .                 IRECT(1,L),H  ,NIR     ,FSAV    ,FNCONT ,
     .                 FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C                      
         IF (IRODDL == 0) THEN                               
           STIFN(I)=EM20                                    
           IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)    
           MS(I)=ZERO                                       
           A(1,I)=ZERO                                       
           A(2,I)=ZERO                                       
           A(3,I)=ZERO                                       
         ENDIF                                          
C                                            
C        stokage ZERO pour noeuds delete par idel2         
        ELSEIF (WEIGHT(-I) == 1) THEN                       
          I0 = I0BASE + IADX(K)                                
          NN = IADI2(1,I0)                                 
          FSKYI2(1,NN) = ZERO                              
          FSKYI2(2,NN) = ZERO                              
          FSKYI2(3,NN) = ZERO                              
          FSKYI2(4,NN) = ZERO                              
          FSKYI2(5,NN) = ZERO                              
          NN = IADI2(2,I0)                                 
          FSKYI2(1,NN) = ZERO                              
          FSKYI2(2,NN) = ZERO                              
          FSKYI2(3,NN) = ZERO                              
          FSKYI2(4,NN) = ZERO                              
          FSKYI2(5,NN) = ZERO                              
          NN = IADI2(3,I0)                                 
          FSKYI2(1,NN) = ZERO                              
          FSKYI2(2,NN) = ZERO                              
          FSKYI2(3,NN) = ZERO                              
          FSKYI2(4,NN) = ZERO                              
          FSKYI2(5,NN) = ZERO                              
          NN = IADI2(4,I0)                                 
          FSKYI2(1,NN) = ZERO                              
          FSKYI2(2,NN) = ZERO                              
          FSKYI2(3,NN) = ZERO                              
          FSKYI2(4,NN) = ZERO                              
          FSKYI2(5,NN) = ZERO                              
        ENDIF                                              
C----
      ENDDO
c-----------
      RETURN
      END
