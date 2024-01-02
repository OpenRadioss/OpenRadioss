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
Chd|  I2_DTN_28                     source/interfaces/inter3d1/i2_dtn_28.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        I2_DTN_28_CIN                 source/interfaces/inter3d1/i2_dtn_28.F
Chd|        I2_DTN_28_PEN                 source/interfaces/inter3d1/i2_dtn_28.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I2_DTN_28(X,INTBUF_TAB,IPARI,STIFN,MS,IN,N,NSN)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD
C=======================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NSN,N
      my_real
     .   X(3,*),STIFN(*),MS(*),IN(*)
C
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NI,NINDXC,NINDXP
C=======================================================================
      NINDXC = 0     
      NINDXP = 0     
C----------------          
      DO I=1,NSN
        IF (INTBUF_TAB(N)%IRUPT(I) == 0) THEN     
          NINDXC = NINDXC + 1             
        ELSE                        
          NINDXP = NINDXP + 1          
        ENDIF                       
      ENDDO
c-----------
      IF (NINDXC > 0) THEN
        CALL I2_DTN_28_CIN(X,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%MSR,INTBUF_TAB(N)%NSV,INTBUF_TAB(N)%IRTLM,
     .                     IPARI(1,N),STIFN, STIFN(NUMNOD+1),MS,IN,INTBUF_TAB(N)%IRUPT)
      ENDIF
c-----------
      IF (NINDXP > 0) THEN
        CALL I2_DTN_28_PEN(X,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS,INTBUF_TAB(N)%NSV,INTBUF_TAB(N)%IRTLM,
     .                     IPARI(1,N),INTBUF_TAB(N)%MSEGTYP2,STIFN,STIFN(NUMNOD+1),INTBUF_TAB(N)%SPENALTY,
     .                     INTBUF_TAB(N)%STFR_PENALTY,INTBUF_TAB(N)%VARIABLES(14),IN,INTBUF_TAB(N)%IRUPT)
      ENDIF      
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I2_DTN_28_CIN                 source/interfaces/inter3d1/i2_dtn_28.F
Chd|-- called by -----------
Chd|        I2_DTN_28                     source/interfaces/inter3d1/i2_dtn_28.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2_DTN_28_CIN(X,IRECT , MSR   ,NSV  ,IRTL  ,
     2                         IPARI, STIFN, STIFR, MS, IN,IRUPT)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), MSR(*), NSV(*),IRTL(*),
     .        IPARI(*),IRUPT(*)
C     REAL
      my_real
     .   X(3,*),STIFN(*),STIFR(*), MS(*),IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,I,J,JJ,L,J1,J2,J3,J4,NIR,NRTM,NSN,NMN
      my_real
     .   X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,XS,YS,ZS,X0,Y0,Z0,
     .   XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .   X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .   A1,A2,A3,B1,B2,B3,C1,C2,C3,MR,MRX,MRY,MRZ,INX,INY,INZ,STF,FACT,
     .   DET,XMSI
C=======================================================================
      NRTM   = IPARI(4)
      NSN    = IPARI(5)
      NMN    = IPARI(6)
C
      DO II=1,NSN
        IF (IRUPT(II) /= 0) CYCLE
        I = NSV(II)
        L = IRTL(II)
C
        J1=IRECT(1,L)
        J2=IRECT(2,L)
        J3=IRECT(3,L)
        J4=IRECT(4,L)   
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
        DET= XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 - TWO*XY*YZ*ZX
        DET=ONE/MAX(DET,EM20)
        B1=ZZZ*YYY-YZ2
        B2=XXX*ZZZ-ZX2
        B3=YYY*XXX-XY2
        C3=ZZZ*XY+YZ*ZX
        C1=XXX*YZ+ZX*XY
        C2=YYY*ZX+XY*YZ
C
        IF (IRODDL == 1) THEN
          INX= IN(I) + MS(I)*(XS*XS+YS*YS+ZS*ZS)
        ELSE
          INX= MS(I)*(XS*XS+YS*YS+ZS*ZS)
        ENDIF

        MRX = (B1+C3+C2)
        MRY = (B2+C1+C3)
        MRZ = (B3+C2+C1)
        MR=DET*INX*MAX(MRX,MRY,MRZ)
C
        FACT = ONE
        IF (IRODDL==1) THEN
        IF (IN(J1)>ZERO.AND.IN(J2)>ZERO.AND.IN(J3)>ZERO.AND.IN(J4)>ZERO) THEN
C--       Inertie transmise sous forme d'inertie
          FACT = ZERO
        ENDIF
        ENDIF
C
        XMSI=FOURTH*MS(I)+MR*FACT
C
        MS(J1)=MS(J1)+XMSI
        MS(J2)=MS(J2)+XMSI
        MS(J3)=MS(J3)+XMSI
        MS(J4)=MS(J4)+XMSI
C
        IF (IRODDL == 1) THEN
          STF = FOURTH*STIFN(I)
     .         + DET*MAX(MRX,MRY,MRZ)*(STIFR(I)+STIFN(I)*(XS*XS+YS*YS+ZS*ZS))
        ELSE
          STF = FOURTH*STIFN(I)
     .         + DET*MAX(MRX,MRY,MRZ)*(STIFN(I)*(XS*XS+YS*YS+ZS*ZS))
        ENDIF
C
        STIFN(J1)=STIFN(J1) + STF
        STIFN(J2)=STIFN(J2) + STF
        STIFN(J3)=STIFN(J3) + STF
        STIFN(J4)=STIFN(J4) + STF
C
        IF (IRODDL==1) THEN
          IN(J1)=IN(J1)+INX*FOURTH*(ONE-FACT)
          IN(J2)=IN(J2)+INX*FOURTH*(ONE-FACT)
          IN(J3)=IN(J3)+INX*FOURTH*(ONE-FACT)
          IN(J4)=IN(J4)+INX*FOURTH*(ONE-FACT)
        ENDIF
C
        MS(I)=ZERO
        STIFN(I)=EM20
C  
        IF (IRODDL==1) THEN
          IN(I)=ZERO
          STIFR(I)=EM20
        ENDIF
C
      ENDDO
C      
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  I2_DTN_28_PEN                 source/interfaces/inter3d1/i2_dtn_28.F
Chd|-- called by -----------
Chd|        I2_DTN_28                     source/interfaces/inter3d1/i2_dtn_28.F
Chd|-- calls ---------------
Chd|        I2PEN_ROT28                   ../common_source/interf/i2pen_rot.F
Chd|        I2REP                         ../common_source/interf/i2rep.F
Chd|====================================================================
      SUBROUTINE I2_DTN_28_PEN(X,IRECT ,CRST    ,NSV  ,IRTL,
     2                         IPARI,MSEGTYP2,STIFN, STIFR,STFN,
     3                         STFR,VISC,IN,IRUPT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IRECT(4,*),NSV(*),IRTL(*),MSEGTYP2(*),IPARI(*),IRUPT(*)
      my_real
     .   X(3,*),IN(*),STIFN(*),STIFR(*),STFN(*),STFR(*),CRST(2,*),VISC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR,I,J,II,JJ,L,W,NN,KK,
     .        IX1, IX2, IX3, IX4,NSVG,NSN
C     REAL
      my_real
     .   S,T,SP,SM,TP,TM,E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .   XSM,YSM,ZSM,XM,YM,ZM,X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,X0,Y0,Z0,XS,YS,ZS,STF_MOM,
     .   STF,STR,STBRK
      my_real
     .   H(4),H2(4),RX(4),RY(4),RZ(4),RM(3),RS(3),STIF,VIS
      my_real
     .   LEN2,FAC_TRIANG,IROT,SKEW(9),TT,DERX,DERY,DERZ,DET,B1,B2,B3,C1,C2,C3,BID,BID3(3)  
C=======================================================================
      NSN    = IPARI(5)
C
      BID = ZERO
      BID3(1:3)=ZERO
      TT = ZERO
C
      DO II=1,NSN
        IF (IRUPT(II) == 0) CYCLE
        I = NSV(II)
        L = IRTL(II)
C
        IX1 = IRECT(1,L)                                       
        IX2 = IRECT(2,L)                                       
        IX3 = IRECT(3,L)                                       
        IX4 = IRECT(4,L)
C
        IF (I > 0) THEN
          S = CRST(1,II)
          T = CRST(2,II)
          L = IRTL(II)
C
          IX1 = IRECT(1,L)                                       
          IX2 = IRECT(2,L)                                       
          IX3 = IRECT(3,L)                                       
          IX4 = IRECT(4,L)
C
          NIR= 4                
          SP = ONE + S           
          SM = ONE - S           
          TP = FOURTH*(ONE + T)    
          TM = FOURTH*(ONE - T)      
C    
          H(1)=FOURTH          
          H(2)=FOURTH           
          H(3)=FOURTH           
          H(4)=FOURTH
C    
          H2(1)=TM*SM           
          H2(2)=TM*SP           
          H2(3)=TP*SP           
          H2(4)=TP*SM             
C
          IF (IX3 == IX4) THEN
            NIR = 3
            H(1)=THIRD          
            H(2)=THIRD           
            H(3)=THIRD           
            H(4) = ZERO            
          ENDIF
C------------------------------------------------                  
C         rep local facette main
C------------------------------------------------   
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
          XS  = X(1,I)                                          
          YS  = X(2,I)                                         
          ZS  = X(3,I)                                           

C---------------------
          CALL I2REP(X1     ,X2     ,X3     ,X4     ,
     .               Y1     ,Y2     ,Y3     ,Y4     ,
     .               Z1     ,Z2     ,Z3     ,Z4     ,
     .               E1X    ,E1Y    ,E1Z    ,
     .               E2X    ,E2Y    ,E2Z    ,
     .               E3X    ,E3Y    ,E3Z    ,NIR    )
C------------------------------------------------                  
        IF (NIR == 4) THEN
          FAC_TRIANG = ONE
C
          XM = X1*H2(1) + X2*H2(2) + X3*H2(3) + X4*H2(4)
          YM = Y1*H2(1) + Y2*H2(2) + Y3*H2(3) + Y4*H2(4)
          ZM = Z1*H2(1) + Z2*H2(2) + Z3*H2(3) + Z4*H2(4)
          X0  = (X1 + X2 + X3 + X4)/NIR
          Y0  = (Y1 + Y2 + Y3 + Y4)/NIR
          Z0  = (Z1 + Z2 + Z3 + Z4)/NIR

          XM = XM - X0
          YM = YM - Y0
          ZM = ZM - Z0
          XS = XS - X0
          YS = YS - Y0
          ZS = ZS - Z0
          XSM = XS - XM
          YSM = YS - YM
          ZSM = ZS - ZM
C
        ELSE                                      
          X0  = (X1 + X2 + X3)/NIR
          Y0  = (Y1 + Y2 + Y3)/NIR
          Z0  = (Z1 + Z2 + Z3)/NIR

          XM = X1*H(1) + X2*H(2) + X3*H(3)
          YM = Y1*H(1) + Y2*H(2) + Y3*H(3)
          ZM = Z1*H(1) + Z2*H(2) + Z3*H(3)

          XM = XM - X0
          YM = YM - Y0
          ZM = ZM - Z0
          XS = XS - X0
          YS = YS - Y0
          ZS = ZS - Z0
          XSM = XS - XM
          YSM = YS - YM
          ZSM = ZS - ZM
        ENDIF  
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
C
c       global -> local
c
        RS(1) = XS*E1X + YS*E1Y + ZS*E1Z
        RS(2) = XS*E2X + YS*E2Y + ZS*E2Z
        RS(3) = XS*E3X + YS*E3Y + ZS*E3Z
        RM(1) = XM*E1X + YM*E1Y + ZM*E1Z
        RM(2) = XM*E2X + YM*E2Y + ZM*E2Z
        RM(3) = XM*E3X + YM*E3Y + ZM*E3Z
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
        IF (NIR==3) THEN
            RX(4)=ZERO
            RY(4)=ZERO
            RZ(4)=ZERO
        END IF
C
C---------
          CALL I2PEN_ROT28(
     .    SKEW ,TT   ,BID  ,STBRK,
     .    RS   ,RM   ,BID3 ,BID3 ,BID3 ,                        
     .    RX   ,RY   ,RZ   ,BID3 ,BID3 ,
     .    BID3   ,BID3   ,BID3 ,BID3  ,DET  ,
     .    B1   ,B2   ,B3   ,C1   ,C2   ,
     .    C3   ,IN(I))
C
C------------------------------------------------
C
          STF     = STFN(II)*(VISC + SQRT(VISC**2 + (ONE+STBRK)))**2
C
          LEN2 = XSM**2+YSM**2+ZSM**2  
          STR     = (STFR(II)+STFN(II)*LEN2)*(VISC + SQRT(VISC**2 + ONE))**2
C
          DERX = (B1+C3+C2)
          DERY = (B2+C1+C3)
          DERZ = (B3+C2+C1)
C
          STF_MOM = DET*MAX(DERX,DERY,DERZ)*(STR+STF*(XM*XM+YM*YM+ZM*ZM))  
C----------------------------------------------------
C
          STIFN(IX1) = STIFN(IX1)+ABS(STF*H(1))+STF_MOM
          STIFN(IX2) = STIFN(IX2)+ABS(STF*H(2))+STF_MOM
          STIFN(IX3) = STIFN(IX3)+ABS(STF*H(3))+STF_MOM
          STIFN(IX4) = STIFN(IX4)+ABS(STF*H(4))+STF_MOM
C
        END IF  
      ENDDO
C
C-----------
      RETURN
      END
