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
Chd|  I2FOR26                       source/interfaces/interf/i2for26.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        I2LOCEQ                       ../common_source/interf/i2loceq.F
Chd|        I2PEN_ROT26                   ../common_source/interf/i2pen_rot.F
Chd|        I2REP                         ../common_source/interf/i2rep.F
Chd|        I2SMS26                       source/interfaces/interf/i2sms26.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FOR26(
     .           X       ,V       ,VR      ,A        ,AR       ,
     .           MS      ,STIFN   ,STIFR   ,WEIGHT   ,IRECT    ,
     .           NSV     ,IRTL    ,DR      ,DL       ,FINI     ,
     .           FSAV    ,FNCONT  ,NSN     ,STFN     ,STFR     ,
     .           VISC    ,NOINT   ,NODNX_SMS,DMINT2 ,SAV_FOR_PENA,
     .           MS_PENA ,IN      ,DT2T    ,NELTST  ,ITYPTST   ,
     .           MINER   ,H3D_DATA,FNCONTP   ,FTCONTP)
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
      INTEGER  NSN, NOINT, NELTST,ITYPTST
      INTEGER  IRECT(4,*),NSV(*),IRTL(*),WEIGHT(*),NODNX_SMS(*)
C     REAL
      my_real
     .   VISC,DT2T
      my_real
     .   X(3,*),V(3,*),A(3,*),VR(3,*),AR(3,*),MS(*),IN(*),FINI(6,4,*),
     .   DL(3,4,*),DR(3,4,*),STIFN(*),STIFR(*),STFN(*),STFR(*),
     .   FSAV(*),FNCONT(3,*),DMINT2(4,*),SAV_FOR_PENA(8,*),MS_PENA(*),
     .   MINER(*),FNCONTP(3,*)   ,FTCONTP(3,*)
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
      INTEGER NIR,I,J,IR,II,JJ,L,W,KK,K,LLT,NM,
     .   IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),NSVG(MVSIZ)
C     REAL
      my_real
     .   ECONTT,ECONVT,E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,XSM,YSM,ZSM,
     .   X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,MLX,MLY,MLZ,DTI,STIFMS,
     .   DRX,DRY,DRZ,VRX,VRY,VRZ,DLX,DLY,DLZ,LEN2,DKM,DIN,
     .   DVX,DVY,DVZ,VXX,VYY,VZZ,VLX,VLY,VLZ,WX,WY,WZ,DWX,DWY,DWZ, DXT
      my_real
     .   STIF(MVSIZ),VIS(4,MVSIZ),VISR(4,MVSIZ),STF(4,MVSIZ),STR(4,MVSIZ),
     .   FX(4),FY(4),FZ(4),MX(4),MY(4),MZ(4),MRX(4),MRY(4),MRZ(4),
     .   FLOCX(4),FLOCY(4),FLOCZ(4),FLOCXV(4),FLOCYV(4),FLOCZV(4),
     .   MLOCX(4),MLOCY(4),MLOCZ(4),MLOCXV(4),MLOCYV(4),MLOCZV(4),MS_HARM,
     .   FNORM,FN(3),FT(3),STBRK,VA(3),VB(3),VC(3),VD(3),RX(4),RY(4),RZ(4),
     .   VX1,VY1,VZ1,VX2,VY2,VZ2,VX3,VY3,VZ3,VX4,VY4,VZ4,RS(3),
     .   X0,Y0,Z0,XS,YS,ZS,DWDU,H(4),STIFM(MVSIZ),STMAX,WLX,WLY,WLZ,IN_HARM
C=======================================================================
      I7KGLO = 1
      ECONTT = ZERO
      ECONVT = ZERO
C----------------
      DO KK=1,NSN,MVSIZ
C
       LLT = MIN(NSN-KK+1,MVSIZ)
c
       DO K=1,LLT
        II = KK + K - 1
        I  = NSV(II)
C
        IF (I > 0) THEN
          NSVG(K) = I
          W = WEIGHT(I)
          L = IRTL(II)
C
          IX1(K) = IRECT(1,L)                                       
          IX2(K) = IRECT(2,L)                                       
          IX3(K) = IRECT(3,L)                                       
          IX4(K) = IRECT(4,L)  
          IF (IX3(K) == IX4(K)) THEN
            NIR = 3
            STF(4,K) = ZERO
            H(1) = ONE
            H(2) = ONE
            H(3) = ONE
            H(4) = ZERO
          ELSE
            NIR= 4
            H(1) = ONE
            H(2) = ONE
            H(3) = ONE
            H(4) = ONE
C
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
C
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
C               
C---------------------
          CALL I2REP(X1     ,X2     ,X3     ,X4     ,
     .               Y1     ,Y2     ,Y3     ,Y4     ,
     .               Z1     ,Z2     ,Z3     ,Z4     ,
     .               E1X    ,E1Y    ,E1Z    ,
     .               E2X    ,E2Y    ,E2Z    ,
     .               E3X    ,E3Y    ,E3Z    ,NIR    )
C
          IF (NIR == 4) THEN
            X0  = (X1 + X2 + X3 + X4)/NIR
            Y0  = (Y1 + Y2 + Y3 + Y4)/NIR
            Z0  = (Z1 + Z2 + Z3 + Z4)/NIR
          ELSE                                       
            X0  = (X1 + X2 + X3)/NIR
            Y0  = (Y1 + Y2 + Y3)/NIR
            Z0  = (Z1 + Z2 + Z3)/NIR
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
          XS  = X(1,I) - X0                                          
          YS  = X(2,I) - Y0                                         
          ZS  = X(3,I) - Z0                                              
C
c       global -> local
c
          RS(1) = XS*E1X + YS*E1Y + ZS*E1Z
          RS(2) = XS*E2X + YS*E2Y + ZS*E2Z
          RS(3) = XS*E3X + YS*E3Y + ZS*E3Z
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
          IF (NIR==3) THEN 
            VD(1) = ZERO
            VD(2) = ZERO
            VD(3) = ZERO          
          ENDIF
C
          IF (IRODDL == 0 .OR. (MINER(I) <= EM20)) THEN
C--------- Connection solide : calcul vitesse entrainement facette main Vi = Vi -VR ^ MS
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
            CALL I2PEN_ROT26(TT   ,DT1  ,DWDU,
     .         WLX   ,WLY   ,WLZ   ,                        
     .         RX   ,RY   ,RZ   ,VA   ,VB   ,
     .         VC   ,VD)
C
          ENDIF
C
c----------------------------------------------------------
         DO IR = 1,NIR
           NM = IRECT(IR,L)
           
c          velocities in global coords

           IF (IRODDL == 1 .and. MINER(I) > EM20) THEN
             WX  = (VR(1,I) + VR(1,NM)) * HALF
             WY  = (VR(2,I) + VR(2,NM)) * HALF
             WZ  = (VR(3,I) + VR(3,NM)) * HALF
             WLX = WX*E1X + WY*E1Y + WZ*E1Z 
             WLY = WX*E2X + WY*E2Y + WZ*E2Z
             WLZ = WX*E3X + WY*E3Y + WZ*E3Z
             DWX =  VR(1,I) - VR(1,NM)
             DWY =  VR(2,I) - VR(2,NM)
             DWZ =  VR(3,I) - VR(3,NM)
             STBRK = ZERO
           ELSE
             DWX = ZERO
             DWY = ZERO
             DWZ = ZERO
             STBRK = SQRT(XSM*XSM+YSM*YSM+ZSM*ZSM)*DWDU
           ENDIF

           DVX  = V(1,I) - V(1,NM)
           DVY  = V(2,I) - V(2,NM)
           DVZ  = V(3,I) - V(3,NM)
C
           XSM  = RS(1) - RX(IR)
           YSM  = RS(2) - RY(IR)
           ZSM  = RS(3) - RZ(IR)
           LEN2 = XSM*XSM + YSM*YSM + ZSM*ZSM

           VXX  = DVX
           VYY  = DVY
           VZZ  = DVZ
           
c          displacements & rotations in local coord

           VLX = VXX*E1X + VYY*E1Y + VZZ*E1Z + YSM*WLZ - ZSM*WLY 
           VLY = VXX*E2X + VYY*E2Y + VZZ*E2Z + ZSM*WLX - XSM*WLZ
           VLZ = VXX*E3X + VYY*E3Y + VZZ*E3Z + XSM*WLY - YSM*WLX
c
           VRX = DWX*E1X + DWY*E1Y + DWZ*E1Z 
           VRY = DWX*E2X + DWY*E2Y + DWZ*E2Z
           VRZ = DWX*E3X + DWY*E3Y + DWZ*E3Z
c
           DLX = VLX * DT1 
           DLY = VLY * DT1 
           DLZ = VLZ * DT1 
c
           DRX = VRX * DT1 
           DRY = VRY * DT1 
           DRZ = VRZ * DT1 
c
           DL(1,IR,II) = DL(1,IR,II) + DLX 
           DL(2,IR,II) = DL(2,IR,II) + DLY
           DL(3,IR,II) = DL(3,IR,II) + DLZ
c           
           DR(1,IR,II) = DR(1,IR,II) + DRX 
           DR(2,IR,II) = DR(2,IR,II) + DRY 
           DR(3,IR,II) = DR(3,IR,II) + DRZ 
C----------------------------------------------------
c          Stiffness
C----------------------------------------------------
          IF (VISC /= ZERO) THEN
            MS_HARM = (MS_PENA(I)*MS_PENA(NM))/(MS_PENA(I)+MS_PENA(NM))
            DKM    = TWO*STFN(II)*MS_HARM
            VIS(IR,K) = VISC*SQRT(DKM)
            STF(IR,K) = (VIS(IR,K) + SQRT(VIS(IR,K)**2 + (ONE+STBRK)*DKM))**2/(TWO*MS_HARM)
          ELSE
            STF(IR,K) = (ONE+STBRK)*STFN(II)
          ENDIF
          IF (IRODDL == 1 .and. MINER(I) > EM20) THEN
            IN_HARM   = (IN(I)*IN(NM))/(IN(I)+IN(NM))
            STFR(II)  = STFN(II)*LEN2
            DKM       = TWO*STFR(II)*IN_HARM
            VISR(IR,K)= VISC*SQRT(DKM)
            STR(IR,K) = (VISR(IR,K) + SQRT(VISR(IR,K)**2 + DKM))**2/(TWO*IN_HARM)
          ELSE
            VISR(IR,K)  = ZERO
            STFR(II)   = ZERO
            STR(IR,K)  = ZERO
          ENDIF
C----------------------------------------------------
c          forces & moments in local coord      
C----------------------------------------------------
           FLOCX(IR)  = STFN(II) * DL(1,IR,II)
           FLOCY(IR)  = STFN(II) * DL(2,IR,II)
           FLOCZ(IR)  = STFN(II) * DL(3,IR,II)
c
           FLOCXV(IR) = VIS(IR,K) * VLX
           FLOCYV(IR) = VIS(IR,K) * VLY
           FLOCZV(IR) = VIS(IR,K) * VLZ
c---
           DXT = DL(1,IR,II)**2 + DL(2,IR,II)**2+ DL(3,IR,II)**2
           ECONTT = ECONTT + HALF*STFN(II)*DXT

           ECONVT = ECONVT + (FLOCXV(IR)*VLX 
     .                     +  FLOCYV(IR)*VLY 
     .                     +  FLOCZV(IR)*VLZ)*DT1
c---           
           FLOCX(IR) = FLOCX(IR) + FLOCXV(IR)
           FLOCY(IR) = FLOCY(IR) + FLOCYV(IR)
           FLOCZ(IR) = FLOCZ(IR) + FLOCZV(IR)
c
           MLOCX(IR)  = STFR(II) * DR(1,IR,II)
           MLOCY(IR)  = STFR(II) * DR(2,IR,II)
           MLOCZ(IR)  = STFR(II) * DR(3,IR,II)
c
           MLOCXV(IR) = VISR(IR,K) * VRX
           MLOCYV(IR) = VISR(IR,K) * VRY
           MLOCZV(IR) = VISR(IR,K) * VRZ
c
           IF (IRODDL == 1 .and. MINER(I) > EM20) THEN 
    
             DXT = DR(1,IR,II)**2  + DR(2,IR,II)**2 + DR(3,IR,II)**2
             ECONTT = ECONTT + HALF*STFR(II)*DXT

             ECONVT = ECONVT + (MLOCXV(IR)*VRX 
     .                       +  MLOCYV(IR)*VRY 
     .                       +  MLOCZV(IR)*VRZ)*DT1
           ENDIF
c
           MLOCX(IR) = MLOCX(IR) + MLOCXV(IR)
           MLOCY(IR) = MLOCY(IR) + MLOCYV(IR)
           MLOCZ(IR) = MLOCZ(IR) + MLOCZV(IR)
c
         ENDDO ! IR = 1,NIR
C
         STMAX = MAX(STF(1,K),STF(2,K),STF(3,K),STF(4,K))
         IF (IRODDL == 1 .and. MINER(I) > EM20) THEN 
           STIFM(K) = ZERO
         ELSE
C----------------------------------------------------
c         update main forces (moment balance) 
C
           CALL I2LOCEQ( NIR    ,RS     ,RX     ,RY     ,RZ      ,    
     .                  FLOCX    ,FLOCY    ,FLOCZ    ,H ,STIFM(K))  
         ENDIF        
C
         DO IR = 1,NIR
           NM = IRECT(IR,L)
C
           XSM  = X(1,I) - X(1,NM)
           YSM  = X(2,I) - X(2,NM)
           ZSM  = X(3,I) - X(3,NM)
C----------------------------------------------------
C          forces/moments -> global coordinates
C----------------------------------------------------
           FX(IR) = E1X*FLOCX(IR) + E2X*FLOCY(IR) + E3X*FLOCZ(IR)
           FY(IR) = E1Y*FLOCX(IR) + E2Y*FLOCY(IR) + E3Y*FLOCZ(IR)
           FZ(IR) = E1Z*FLOCX(IR) + E2Z*FLOCY(IR) + E3Z*FLOCZ(IR)

           MX(IR) = E1X*MLOCX(IR) + E2X*MLOCY(IR) + E3X*MLOCZ(IR)
           MY(IR) = E1Y*MLOCX(IR) + E2Y*MLOCY(IR) + E3Y*MLOCZ(IR)
           MZ(IR) = E1Z*MLOCX(IR) + E2Z*MLOCY(IR) + E3Z*MLOCZ(IR)
           
           MRX(IR) = HALF*(YSM*FZ(IR) - ZSM*FY(IR))
           MRY(IR) = HALF*(ZSM*FX(IR) - XSM*FZ(IR))
           MRZ(IR) = HALF*(XSM*FY(IR) - YSM*FX(IR)) 
c
c          secnd node
c
           SAV_FOR_PENA(1,I) = SAV_FOR_PENA(1,I) - FX(IR) 
           SAV_FOR_PENA(2,I) = SAV_FOR_PENA(2,I) - FY(IR) 
           SAV_FOR_PENA(3,I) = SAV_FOR_PENA(3,I) - FZ(IR) 
           SAV_FOR_PENA(4,I) = SAV_FOR_PENA(4,I) + STF(IR,K)
c
           IF (IRODDL == 1 .and. MINER(I) > EM20) THEN
             SAV_FOR_PENA(5,I) = SAV_FOR_PENA(5,I) - MX(IR) + MRX(IR) 
             SAV_FOR_PENA(6,I) = SAV_FOR_PENA(6,I) - MY(IR) + MRY(IR) 
             SAV_FOR_PENA(7,I) = SAV_FOR_PENA(7,I) - MZ(IR) + MRZ(IR) 
             SAV_FOR_PENA(8,I) = SAV_FOR_PENA(8,I) + STR(IR,K)
           ENDIF
c--------------------------------------------
c           MLX = (MRX(IR)*E1X + MRY(IR)*E1Y + MRZ(IR)*E1Z)*TWO
c           MLY = (MRX(IR)*E2X + MRY(IR)*E2Y + MRZ(IR)*E2Z)*TWO
c           MLZ = (MRX(IR)*E3X + MRY(IR)*E3Y + MRZ(IR)*E3Z)*TWO
c--------------------------------------------

           FINI(1,IR,II) = FLOCX(IR)
           FINI(2,IR,II) = FLOCY(IR)
           FINI(3,IR,II) = FLOCZ(IR)
           IF (IRODDL == 1 .and. MINER(I) > EM20) THEN     
             FINI(4,IR,II) = MLOCX(IR)
             FINI(5,IR,II) = MLOCY(IR)
             FINI(6,IR,II) = MLOCZ(IR)
           ENDIF
C
C------------------------------------------------                  
C         composantes N/T de la forces nodale -> output
C------------------------------------------------                  
           FNORM = E3X*FLOCX(IR) + E3Y*FLOCY(IR) + E3Z*FLOCZ(IR)
           FN(1) = E3X*FNORM
           FN(2) = E3Y*FNORM
           FN(3) = E3Z*FNORM
C
           FT(1) = FLOCX(IR) - FN(1)
           FT(2) = FLOCY(IR) - FN(2)
           FT(3) = FLOCZ(IR) - FN(3)
C
           FSAV(1) = FSAV(1) + FN(1)*DT1*W
           FSAV(2) = FSAV(2) + FN(2)*DT1*W
           FSAV(3) = FSAV(3) + FN(3)*DT1*W
           FSAV(4) = FSAV(4) + FT(1)*DT1*W
           FSAV(5) = FSAV(5) + FT(2)*DT1*W
           FSAV(6) = FSAV(6) + FT(3)*DT1*W
C
           IF (ANIM_V(13)+H3D_DATA%N_VECT_CONT2 > 0) THEN
             FNCONT(1,I)  = FNCONT(1,I)  - FX(IR) * W
             FNCONT(2,I)  = FNCONT(2,I)  - FY(IR) * W
             FNCONT(3,I)  = FNCONT(3,I)  - FZ(IR) * W
             FNCONT(1,IRECT(IR,L)) = FNCONT(1,IRECT(IR,L)) + FX(IR)*W
             FNCONT(2,IRECT(IR,L)) = FNCONT(2,IRECT(IR,L)) + FY(IR)*W
             FNCONT(3,IRECT(IR,L)) = FNCONT(3,IRECT(IR,L)) + FZ(IR)*W
           ENDIF

          IF(ANIM_V(27)+H3D_DATA%N_VECT_PCONT2>0) THEN ! Normal/Tangential forces output
            FNCONTP(1,I) = FNCONTP(1,I)  - FN(1) * W
            FNCONTP(2,I) = FNCONTP(2,I)  - FN(2) * W
            FNCONTP(3,I) = FNCONTP(3,I)  - FN(3) * W

            FNCONTP(1,IRECT(IR,L)) = FNCONTP(1,IRECT(IR,L)) + FN(1)*W
            FNCONTP(2,IRECT(IR,L)) = FNCONTP(2,IRECT(IR,L)) + FN(2)*W
            FNCONTP(3,IRECT(IR,L)) = FNCONTP(3,IRECT(IR,L)) + FN(3)*W

            FTCONTP(1,I) = FTCONTP(1,I)  - FT(1) * W
            FTCONTP(2,I) = FTCONTP(2,I)  - FT(2) * W
            FTCONTP(3,I) = FTCONTP(3,I)  - FT(3) * W

            FTCONTP(1,IRECT(IR,L)) = FTCONTP(1,IRECT(IR,L)) + FT(1)*W
            FTCONTP(2,IRECT(IR,L)) = FTCONTP(2,IRECT(IR,L)) + FT(2)*W
            FTCONTP(3,IRECT(IR,L)) = FTCONTP(3,IRECT(IR,L)) + FT(3)*W
         ENDIF
C
C
         ENDDO ! IR = 1,NIR
c--------------------------------------------
c        main node
c--------------------------------------------
           IF (W == 1) THEN
             DO IR = 1,NIR
               NM = IRECT(IR,L)
               A(1,NM) = A(1,NM) + FX(IR)
               A(2,NM) = A(2,NM) + FY(IR)
               A(3,NM) = A(3,NM) + FZ(IR)
               STIFN(NM) = STIFN(NM) + STF(IR,K) + STIFM(K)*STMAX
c           
               IF (IRODDL == 1 .and. MINER(I) > EM20) THEN
                 AR(1,NM) = AR(1,NM) + MX(IR) + MRX(IR)
                 AR(2,NM) = AR(2,NM) + MY(IR) + MRY(IR)
                 AR(3,NM) = AR(3,NM) + MZ(IR) + MRZ(IR)
                 STIFR(NM) = STIFR(NM) + STR(IR,K)
               ENDIF
             ENDDO ! loop over penalty springs / main nodes 
           ENDIF
C------------------------------------------------
         ELSE
           NSVG(K)= -I
           L = IRTL(II)
C
           IX1(K) = IRECT(1,L)                                      
           IX2(K) = IRECT(2,L)                                      
           IX3(K) = IRECT(3,L)                                      
           IX4(K) = IRECT(4,L)  
           STIF(K)= ZERO
           VIS(IR,K) = ZERO
         ENDIF !  I > 0
C---
        ENDDO  ! K=1,LLT
C------------------------------------------------
        IF (IDTMINS==2 .or. IDTMINS_INT/=0) THEN                    
          DTI = DT2T
          CALL I2SMS26(LLT   ,IX1   ,IX2  ,IX3  ,IX4   ,        
     .                 NSVG  ,STF   ,NOINT,DMINT2(1,KK),  
     .                 NODNX_SMS,VIS,DTI  )                
          IF (DTI < DT2T) THEN
            DT2T    = DTI
            NELTST  = NOINT
            ITYPTST = 10
          ENDIF
        END IF                                                  
c
      ENDDO    !  KK=1,NSN,MVSIZ
C----------
#include "lockon.inc"
      ECONT  = ECONT  + ECONTT ! Elastic energy 
      ECONTD = ECONTD + ECONVT ! Damping Elastic energy 
      FSAV(26) = FSAV(26) + ECONTT
      FSAV(28) = FSAV(28) + ECONVT
#include "lockoff.inc"
C-----------
      RETURN
      END SUBROUTINE I2FOR26
