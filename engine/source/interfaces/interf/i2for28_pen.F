Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i2for28_pen   ../engine/source/interfaces/interf/i2for28_pen.F
!||--- called by ------------------------------------------------------
!||    i2for28       ../engine/source/interfaces/interf/i2for28.F
!||--- calls      -----------------------------------------------------
!||    i2forces      ../engine/source/interfaces/interf/i2forces.F
!||    i2pen_rot28   ../common_source/interf/i2pen_rot.F
!||    i2rep         ../common_source/interf/i2rep.F
!||    i2sms28       ../engine/source/interfaces/interf/i2sms28.F
!||--- uses       -----------------------------------------------------
!||    h3d_mod       ../engine/share/modules/h3d_mod.F
!||====================================================================
      SUBROUTINE I2FOR28_PEN(X       ,V       ,VR      ,A       ,AR      ,
     .                   MS_PENA ,IN      ,STIFN   ,STIFR   ,WEIGHT  ,
     .                   NSV     ,IRTL    ,CRST    ,SKEW    ,DX      ,
     .                   DR      ,FINI    ,FSAV    ,FNCONT  ,NSN     ,
     .                   STFN    ,STFR    ,VISC    ,PENFLAG ,IROT    ,
     .                   NOINT   ,NODNX_SMS,DMINT2 ,SAV_FOR_PENA,IRECT,
     .                   DT2T    ,NELTST  ,ITYPTST ,INDXP   ,SAV_INER_POFF,
     .                   H3D_DATA,FNCONTP ,FTCONTP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NSN,PENFLAG,IROT, NOINT,NELTST,ITYPTST
      INTEGER  IRECT(4,*),NSV(*),IRTL(*),WEIGHT(*),INDXP(NSN),
     .         NODNX_SMS(*)
C     REAL
      my_real
     .   VISC,DT2T
      my_real
     .   X(3,*),VR(3,*),V(3,*),A(3,*),AR(3,*),DR(3,*),SKEW(9,*),
     .   DX(3,*),FINI(6,*),MS_PENA(*),IN(*),STIFN(*),STIFR(*),STFN(*),STFR(*),
     .   CRST(2,*),FSAV(*),FNCONT(3,*),
     .   DMINT2(4,*),SAV_FOR_PENA(8,*),FNCONTP(3,*)   ,FTCONTP(3,*) 
      my_real
     .   SAV_INER_POFF(*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR,I,J,II,JJ,L,W,NN,KK,K,LLT,
     .        IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ)
C     REAL
      my_real
     .   S,T,SP,SM,TP,TM,ECONTT,ECONVT,E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .   FNORM,FLX,FLY,FLZ,FS(3),XSM,YSM,ZSM,XM,YM,ZM,
     .   X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,X0,Y0,Z0,XS,YS,ZS,STF_MOM(MVSIZ),
     .   VX1,VX2,VX3,VX4,VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,DLX,DLY,DLZ,
     .   VX0,VY0,VZ0,VSX,VSY,VSZ,VMX,VMY,VMZ,VX,VY,VZ,DTINV,STF,
     .   FXV,FYV,FZV,DRX,DRY,DRZ,STBRK,DTI,MHARM,DKM,DET,B1,B2,B3,C1,C2,C3,
     .   A1,A2,A3,MTTX,MTTY,MTTZ,DERX,DERY,DERZ, DXT
      my_real
     .   H(4,MVSIZ),FN(3),FT(3),FX(4),FY(4),FZ(4),FMX(4),FMY(4),FMZ(4),
     .   RX(4),RY(4),RZ(4),RM(3),RS(3),V0(3),VS(3),VM(3),
     .   STIF(MVSIZ), VIS(MVSIZ), VA(3),VB(3),VC(3),VD(3),H2(4,MVSIZ)
      my_real
     .   VRM(3),VRS(3),
     .   VRX0,VRX1,VRX2,VRX3,VRX4,VRY0,VRY1,VRY2,VRY3,VRY4,VRZ0,VRZ1,VRZ2,VRZ3,VRZ4,
     .   VRSX,VRSY,VRSZ,VRX,VRY,VRZ,MLX,MLY,MLZ,MX(4),MY(4),MZ(4),MRX,MRY,MRZ,
     .   MGX,MGY,MGZ,MSX,MSY,MSZ,MVX,MVY,MVZ,STR,VISR(MVSIZ),DKI,INHARM,LEN2,
     .   HL(4)
C=======================================================================
      I7KGLO = 1
      ECONTT = ZERO
      ECONVT = ZERO
      NSVG(1:MVSIZ) = 0
C----------------    
      DO KK=1,NSN,MVSIZ
C
       LLT=MIN(NSN-KK+1,MVSIZ)
       DO K=1,LLT
C
        II = INDXP(KK+K-1)
        IF (II == 0) CYCLE
        I = NSV(II)
C
        IF (I > 0) THEN
          NSVG(K) = I
          W = WEIGHT(I)
          S = CRST(1,II)
          T = CRST(2,II)
          L = IRTL(II)
C
          IX1(K) = IRECT(1,L)                                       
          IX2(K) = IRECT(2,L)                                       
          IX3(K) = IRECT(3,L)                                       
          IX4(K) = IRECT(4,L)  
C
          NIR= 4                
          SP = ONE + S           
          SM = ONE - S           
          TP = FOURTH*(ONE + T)    
          TM = FOURTH*(ONE - T)
C    
          H(1,K)=FOURTH          
          H(2,K)=FOURTH           
          H(3,K)=FOURTH           
          H(4,K)=FOURTH
C    
          H2(1,K)=TM*SM           
          H2(2,K)=TM*SP           
          H2(3,K)=TP*SP           
          H2(4,K)=TP*SM             
C
          IF (IX3(K) == IX4(K)) THEN
            NIR = 3
            H(1,K)=THIRD          
            H(2,K)=THIRD           
            H(3,K)=THIRD           
            H(4,K) = ZERO            
          ENDIF
C------------------------------------------------                  
C         rep local facette main
C------------------------------------------------   
          X1  = X(1,IX1(K))                                       
          Y1  = X(2,IX1(K))                                          
          Z1  = X(3,IX1(K))                                          
          X2  = X(1,IX2(K))               
          Y2  = X(2,IX2(K))               
          Z2  = X(3,IX2(K))               
          X3  = X(1,IX3(K))               
          Y3  = X(2,IX3(K))               
          Z3  = X(3,IX3(K))               
          X4  = X(1,IX4(K))               
          Y4  = X(2,IX4(K))               
          Z4  = X(3,IX4(K))               
          XS  = X(1,I)                                          
          YS  = X(2,I)                                         
          ZS  = X(3,I)                                           
          VSX = V(1,I)                                                   
          VSY = V(2,I)                                                   
          VSZ = V(3,I)                                                  
          VX1 = V(1,IX1(K))                  
          VY1 = V(2,IX1(K))                  
          VZ1 = V(3,IX1(K))                  
          VX2 = V(1,IX2(K))                  
          VY2 = V(2,IX2(K))                  
          VZ2 = V(3,IX2(K))                  
          VX3 = V(1,IX3(K))                  
          VY3 = V(2,IX3(K))                  
          VZ3 = V(3,IX3(K))                  
          VX4 = V(1,IX4(K))                  
          VY4 = V(2,IX4(K))                  
          VZ4 = V(3,IX4(K))
          IF (IRODDL == 1 .AND. SAV_INER_POFF(I) > ZERO) THEN
            VRSX = VR(1,I)
            VRSY = VR(2,I)
            VRSZ = VR(3,I) 
            VRX1 = VR(1,IX1(K))                  
            VRY1 = VR(2,IX1(K))                  
            VRZ1 = VR(3,IX1(K))                  
            VRX2 = VR(1,IX2(K))                  
            VRY2 = VR(2,IX2(K))                  
            VRZ2 = VR(3,IX2(K))                  
            VRX3 = VR(1,IX3(K))                  
            VRY3 = VR(2,IX3(K))                  
            VRZ3 = VR(3,IX3(K))                  
            VRX4 = VR(1,IX4(K))                  
            VRY4 = VR(2,IX4(K))                  
            VRZ4 = VR(3,IX4(K))
          ENDIF
C---------------------
          CALL I2REP(X1     ,X2     ,X3     ,X4     ,
     .               Y1     ,Y2     ,Y3     ,Y4     ,
     .               Z1     ,Z2     ,Z3     ,Z4     ,
     .               E1X    ,E1Y    ,E1Z    ,
     .               E2X    ,E2Y    ,E2Z    ,
     .               E3X    ,E3Y    ,E3Z    ,NIR    )
C------------------------------------------------                  
        IF (NIR == 4) THEN
          XM = X1*H2(1,K) + X2*H2(2,K) + X3*H2(3,K) + X4*H2(4,K)
          YM = Y1*H2(1,K) + Y2*H2(2,K) + Y3*H2(3,K) + Y4*H2(4,K)
          ZM = Z1*H2(1,K) + Z2*H2(2,K) + Z3*H2(3,K) + Z4*H2(4,K)
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
c
          VX0 = (VX1 + VX2 + VX3 + VX4)/NIR
          VY0 = (VY1 + VY2 + VY3 + VY4)/NIR  
          VZ0 = (VZ1 + VZ2 + VZ3 + VZ4)/NIR
          VMX = VX1*H(1,K) + VX2*H(2,K) + VX3*H(3,K) + VX4*H(4,K) - VX0 
          VMY = VY1*H(1,K) + VY2*H(2,K) + VY3*H(3,K) + VY4*H(4,K) - VY0 
          VMZ = VZ1*H(1,K) + VZ2*H(2,K) + VZ3*H(3,K) + VZ4*H(4,K) - VZ0
C
        ELSE                                       
          X0  = (X1 + X2 + X3)/NIR
          Y0  = (Y1 + Y2 + Y3)/NIR
          Z0  = (Z1 + Z2 + Z3)/NIR

          XM = X1*H(1,K) + X2*H(2,K) + X3*H(3,K)
          YM = Y1*H(1,K) + Y2*H(2,K) + Y3*H(3,K)
          ZM = Z1*H(1,K) + Z2*H(2,K) + Z3*H(3,K)

          XM = XM - X0
          YM = YM - Y0
          ZM = ZM - Z0
          XS = XS - X0
          YS = YS - Y0
          ZS = ZS - Z0
          XSM = XS - XM
          YSM = YS - YM
          ZSM = ZS - ZM

          VX0 = (VX1 + VX2 + VX3)/NIR
          VY0 = (VY1 + VY2 + VY3)/NIR  
          VZ0 = (VZ1 + VZ2 + VZ3)/NIR
          VMX = VX1*H(1,K) + VX2*H(2,K) + VX3*H(3,K) - VX0
          VMY = VY1*H(1,K) + VY2*H(2,K) + VY3*H(3,K) - VY0
          VMZ = VZ1*H(1,K) + VZ2*H(2,K) + VZ3*H(3,K) - VZ0
        ENDIF                                      
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
        VSX = VSX  - VX0
        VSY = VSY  - VY0
        VSZ = VSZ  - VZ0
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
        VS(1) = VSX*E1X + VSY*E1Y + VSZ*E1Z
        VS(2) = VSX*E2X + VSY*E2Y + VSZ*E2Z
        VS(3) = VSX*E3X + VSY*E3Y + VSZ*E3Z
        IF (IRODDL == 1 .AND. SAV_INER_POFF(I) > ZERO) THEN
          VRS(1) = VRSX*E1X + VRSY*E1Y + VRSZ*E1Z
          VRS(2) = VRSX*E2X + VRSY*E2Y + VRSZ*E2Z
          VRS(3) = VRSX*E3X + VRSY*E3Y + VRSZ*E3Z
        ENDIF

        VM(1) = VMX*E1X + VMY*E1Y + VMZ*E1Z
        VM(2) = VMX*E2X + VMY*E2Y + VMZ*E2Z
        VM(3) = VMX*E3X + VMY*E3Y + VMZ*E3Z
 
        VA(1) = VX1*E1X + VY1*E1Y + VZ1*E1Z
        VA(2) = VX1*E2X + VY1*E2Y + VZ1*E2Z
        VA(3) = VX1*E3X + VY1*E3Y + VZ1*E3Z
 
        VB(1) = VX2*E1X + VY2*E1Y + VZ2*E1Z
        VB(2) = VX2*E2X + VY2*E2Y + VZ2*E2Z
        VB(3) = VX2*E3X + VY2*E3Y + VZ2*E3Z
 
        VC(1) = VX3*E1X + VY3*E1Y + VZ3*E1Z
        VC(2) = VX3*E2X + VY3*E2Y + VZ3*E2Z
        VC(3) = VX3*E3X + VY3*E3Y + VZ3*E3Z
 
        VD(1) = VX4*E1X + VY4*E1Y + VZ4*E1Z
        VD(2) = VX4*E2X + VY4*E2Y + VZ4*E2Z
        VD(3) = VX4*E3X + VY4*E3Y + VZ4*E3Z
C
C---------   Local displacement
        IF (TT == ZERO) THEN
          DX(1,II) = ZERO
          DX(2,II) = ZERO
          DX(3,II) = ZERO
          FINI(1,II) = ZERO
          FINI(2,II) = ZERO
          FINI(3,II) = ZERO
          DR(1,II) = ZERO
          DR(2,II) = ZERO
          DR(3,II) = ZERO
          FINI(4,II) = ZERO
          FINI(5,II) = ZERO
          FINI(6,II) = ZERO
        ENDIF
C
          VX  = VS(1) - VM(1)
          VY  = VS(2) - VM(2)
          VZ  = VS(3) - VM(3)

C---------  Vi = Vi -VR ^ MS 
          CALL I2PEN_ROT28(
     .    SKEW(1,II) ,TT   ,DT1  ,STBRK,
     .    RS   ,RM   ,VX   ,VY   ,VZ   ,                        
     .    RX   ,RY   ,RZ   ,VA   ,VB   ,
     .    VC   ,VD   ,VRM  ,VRS  ,DET  ,
     .    B1   ,B2   ,B3   ,C1   ,C2   ,
     .    C3   ,SAV_INER_POFF(I))
C
          VRX =  VRS(1) - VRM(1)
          VRY =  VRS(2) - VRM(2)
          VRZ =  VRS(3) - VRM(3)

C-------------  vers increm en vitesses
          DLX = VX*DT1
          DLY = VY*DT1
          DLZ = VZ*DT1
          DRX = VRX*DT1
          DRY = VRY*DT1
          DRZ = VRZ*DT1
C-------------  DX == deplacement relatif
          DX(1,II) = DX(1,II) + DLX
          DX(2,II) = DX(2,II) + DLY
          DX(3,II) = DX(3,II) + DLZ
          DR(1,II) = DR(1,II) + DRX
          DR(2,II) = DR(2,II) + DRY
          DR(3,II) = DR(3,II) + DRZ
C
C------------------------------------------------                  
C         Calcul de la force
C------------------------------------------------
C                         
          FLX = DX(1,II) * STFN(II)
          FLY = DX(2,II) * STFN(II)
          FLZ = DX(3,II) * STFN(II)
C
          IF(MS_PENA(I)==ZERO.OR.MS_PENA(IX1(K))==ZERO.OR.
     .                           MS_PENA(IX2(K))==ZERO.OR.
     .                           MS_PENA(IX3(K))==ZERO.OR.
     .                           MS_PENA(IX4(K))==ZERO)THEN
            MHARM = ZERO
          ELSEIF(NIR==4)THEN
            MHARM = ONE/MS_PENA(I) + 
     .              ONE/MS_PENA(IX1(K)) + ONE/MS_PENA(IX2(K)) + ONE/MS_PENA(IX3(K)) + ONE/MS_PENA(IX4(K))
            MHARM = ONE/MHARM
          ELSE
            MHARM = ONE/MS_PENA(I) + 
     .              ONE/MS_PENA(IX1(K)) + ONE/MS_PENA(IX2(K)) + ONE/MS_PENA(IX3(K))
            MHARM = ONE/MHARM
          END IF
          DKM    = TWO*STFN(II)*MHARM
          VIS(K) = VISC*SQRT(DKM)
C
          FXV = VIS(K) * VX
          FYV = VIS(K) * VY
          FZV = VIS(K) * VZ
c
          DXT = DX(1,II)**2 + DX(2,II)**2 + DX(3,II)**2
          ECONTT = ECONTT + HALF*STFN(II)*DXT*W

          ECONVT = ECONVT + (FXV*VX + FYV*VY + FZV*VZ)*DT1*W
c
          FLX = FLX + FXV
          FLY = FLY + FYV
          FLZ = FLZ + FZV
C 
          FS(1) = E1X*FLX + E2X*FLY + E3X*FLZ
          FS(2) = E1Y*FLX + E2Y*FLY + E3Y*FLZ
          FS(3) = E1Z*FLX + E2Z*FLY + E3Z*FLZ
C
C------------------------------------------------                  
C         Calcul du Moment
C------------------------------------------------
C
          IF (IRODDL == 1 .AND. SAV_INER_POFF(I) > ZERO) THEN
C
C--   Secnd node shell of  spring 
C
          IF(SAV_INER_POFF(I)==ZERO.OR.SAV_INER_POFF(IX1(K))==ZERO.OR.
     .                      SAV_INER_POFF(IX2(K))==ZERO.OR.
     .                      SAV_INER_POFF(IX3(K))==ZERO.OR.
     .                      SAV_INER_POFF(IX4(K))==ZERO)THEN
            INHARM = ZERO
          ELSEIF(NIR==4)THEN
            INHARM = ONE/SAV_INER_POFF(I) + 
     .               ONE/SAV_INER_POFF(IX1(K)) + ONE/SAV_INER_POFF(IX2(K)) +
     .               ONE/SAV_INER_POFF(IX3(K)) + ONE/SAV_INER_POFF(IX4(K))
            INHARM = ONE/INHARM
          ELSE
            INHARM = ONE/SAV_INER_POFF(I) + 
     .               ONE/SAV_INER_POFF(IX1(K)) + ONE/SAV_INER_POFF(IX2(K)) + ONE/SAV_INER_POFF(IX3(K))
            INHARM = ONE/INHARM
          END IF
C
          DKI    = TWO*STFR(II)*INHARM
          VISR(K) = VISC*SQRT(DKI)
C
          MLX = DR(1,II) * STFR(II)
          MLY = DR(2,II) * STFR(II)
          MLZ = DR(3,II) * STFR(II)
C
          MVX = VISR(K) * VRX
          MVY = VISR(K) * VRY
          MVZ = VISR(K) * VRZ
C
          DXT = DR(1,II)**2  + DR(2,II)**2 + DR(3,II)**2
          ECONTT = ECONTT + HALF*STFR(II)*DXT*W

          ECONVT = ECONVT    + (MVX*VRX 
     .                       +  MVY*VRY 
     .                       +  MVZ*VRZ)*DT1*W
C
          MLX = MLX + MVX
          MLY = MLY + MVY
          MLZ = MLZ + MVZ
C
          MGX = E1X*MLX + E2X*MLY + E3X*MLZ 
          MGY = E1Y*MLX + E2Y*MLY + E3Y*MLZ 
          MGZ = E1Z*MLX + E2Z*MLY + E3Z*MLZ
C
          MRX = HALF*(YSM*FS(3) - ZSM*FS(2))
          MRY = HALF*(ZSM*FS(1) - XSM*FS(3))
          MRZ = HALF*(XSM*FS(2) - YSM*FS(1))
C
          ELSE
C
C--   Secnd node of solids
C
          MGX = ZERO
          MGY = ZERO
          MGZ = ZERO
C
          MRX = YSM*FS(3) - ZSM*FS(2)
          MRY = ZSM*FS(1) - XSM*FS(3)
          MRZ = XSM*FS(2) - YSM*FS(1)
C
          ENDIF

C------------------------------------------------                  
C         Computation of stiffness for nodal time step
C------------------------------------------------
C
          STF     = STFN(II)*(VISC + SQRT(VISC**2 + (ONE+STBRK)))**2
C
          LEN2 = XSM**2+YSM**2+ZSM**2  
          STR     = (STFR(II)+STFN(II)*LEN2)*(VISC + SQRT(VISC**2 + ONE))**2
C
C----------------------------------------------------
C         Secnd forces/moments -> global coordinates 
C----------------------------------------------------
C
          SAV_FOR_PENA(1,I) = SAV_FOR_PENA(1,I) - FS(1)
          SAV_FOR_PENA(2,I) = SAV_FOR_PENA(2,I) - FS(2)   
          SAV_FOR_PENA(3,I) = SAV_FOR_PENA(3,I) - FS(3) 
          SAV_FOR_PENA(4,I) = SAV_FOR_PENA(4,I) + STF
C
C         for SMS ::
          STIF(K) = (ONE+STBRK)*STFN(II)
C
          IF (IRODDL == 1) THEN
            IF (SAV_INER_POFF(I)>ZERO) THEN 
              SAV_FOR_PENA(5,I) = SAV_FOR_PENA(5,I) - MGX + MRX
              SAV_FOR_PENA(6,I) = SAV_FOR_PENA(6,I) - MGY + MRY  
              SAV_FOR_PENA(7,I) = SAV_FOR_PENA(7,I) - MGZ + MRZ
              SAV_FOR_PENA(8,I) = SAV_FOR_PENA(8,I) + STR
            ENDIF
          ENDIF 
C
C----------------------------------------------------
C         Main forces/moments 
C----------------------------------------------------
C
C----     Transfer or moments in forces 
C
          MTTX=E1X*(MGX+MRX) + E1Y*(MGY+MRY) + E1Z*(MGZ+MRZ) + RM(2)*FLZ - RM(3)*FLY
          MTTY=E2X*(MGX+MRX) + E2Y*(MGY+MRY) + E2Z*(MGZ+MRZ) + RM(3)*FLX - RM(1)*FLZ
          MTTZ=E3X*(MGX+MRX) + E3Y*(MGY+MRY) + E3Z*(MGZ+MRZ) + RM(1)*FLY - RM(2)*FLX
C
          A1=DET*(MTTX*B1+MTTY*C3+MTTZ*C2)
          A2=DET*(MTTY*B2+MTTZ*C1+MTTX*C3)
          A3=DET*(MTTZ*B3+MTTX*C2+MTTY*C1)
C
          DERX = (B1+C3+C2)
          DERY = (B2+C1+C3)
          DERZ = (B3+C2+C1)
C         for SMS :: 
          STF_MOM(K) = DET*MAX(DERX,DERY,DERZ)*(STR+STF*(XM*XM+YM*YM+ZM*ZM))    
C
          DO J=1,4
            FMX(J) = H(J,K)*FLX + A2*RZ(J) - A3*RY(J)
            FMY(J) = H(J,K)*FLY + A3*RX(J) - A1*RZ(J)
            FMZ(J) = H(J,K)*FLZ + A1*RY(J) - A2*RX(J)          
          ENDDO     
C
          DO J=1,4
            FX(J) = E1X*FMX(J) + E2X*FMY(J) + E3X*FMZ(J) 
            FY(J) = E1Y*FMX(J) + E2Y*FMY(J) + E3Y*FMZ(J) 
            FZ(J) = E1Z*FMX(J) + E2Z*FMY(J) + E3Z*FMZ(J) 
          ENDDO
C
          IF (W == 1) THEN
C
            A(1,IX1(K)) = A(1,IX1(K)) + FX(1)
            A(2,IX1(K)) = A(2,IX1(K)) + FY(1)
            A(3,IX1(K)) = A(3,IX1(K)) + FZ(1)
            STIFN(IX1(K)) = STIFN(IX1(K))+ABS(STF*H(1,K))+STF_MOM(K)
c
            A(1,IX2(K)) = A(1,IX2(K)) + FX(2)
            A(2,IX2(K)) = A(2,IX2(K)) + FY(2)
            A(3,IX2(K)) = A(3,IX2(K)) + FZ(2)
            STIFN(IX2(K)) = STIFN(IX2(K))+ABS(STF*H(2,K))+STF_MOM(K)
c
            A(1,IX3(K)) = A(1,IX3(K)) + FX(3)
            A(2,IX3(K)) = A(2,IX3(K)) + FY(3)
            A(3,IX3(K)) = A(3,IX3(K)) + FZ(3)
            STIFN(IX3(K)) = STIFN(IX3(K))+ABS(STF*H(3,K))+STF_MOM(K)
c
            IF (NIR==4) THEN
              A(1,IX4(K)) = A(1,IX4(K)) + FX(4)
              A(2,IX4(K)) = A(2,IX4(K)) + FY(4)
              A(3,IX4(K)) = A(3,IX4(K)) + FZ(4)
              STIFN(IX4(K)) = STIFN(IX4(K))+ABS(STF*H(4,K))+STF_MOM(K)
            ENDIF                                                         
          ENDIF   
C

C------------------------------------------------                  
          FINI(1,II) = FLX
          FINI(2,II) = FLY
          FINI(3,II) = FLZ
          IF (IRODDL == 1 .AND. SAV_INER_POFF(I) > ZERO) THEN
            FINI(4,II) = MLX
            FINI(5,II) = MLY
            FINI(6,II) = MLZ
          ENDIF
C------------------------------------------------                  
C         composantes N/T de la forces nodale -> output
C------------------------------------------------           
          HL(1:4) = H(1:4,K)       
          CALL I2FORCES(X       ,FS     ,FX     ,FY      ,FZ     ,
     .                  IRECT(1,L),NIR  ,FSAV   ,FNCONT  ,FNCONTP,
     .                  FTCONTP ,WEIGHT ,H3D_DATA,I      ,HL)
C----------
        ELSE  ! desactivated secnd node
          NSVG(K)= -I
          L = IRTL(II)
C
          IX1(K) = IRECT(1,L)                                       
          IX2(K) = IRECT(2,L)                                       
          IX3(K) = IRECT(3,L)                                       
          IX4(K) = IRECT(4,L)  
          STIF(K)= ZERO
          VIS(K) = ZERO
        ENDIF
       ENDDO
c
       IF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
         DTI=DT2T
         CALL I2SMS28(LLT   ,IX1   ,IX2  ,IX3  ,IX4   ,
     2                NSVG  ,H      ,STIF ,NOINT ,
     3                DMINT2(1,KK),NODNX_SMS ,VIS   ,
     4                STF_MOM ,DTI   )
         IF(DTI<DT2T)THEN
           DT2T    = DTI
           NELTST  = NOINT
           ITYPTST = 10
         ENDIF
       END IF  
      ENDDO
C----------
#include "lockon.inc"
      ECONT  = ECONT  + ECONTT ! Elastic energy 
      ECONTD = ECONTD + ECONVT ! Damping Elastic energy 
      FSAV(26) = FSAV(26) + ECONTT
      FSAV(28) = FSAV(28) + ECONVT
#include "lockoff.inc"
C-----------
      RETURN
      END SUBROUTINE I2FOR28_PEN
