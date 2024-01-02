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
Chd|  I2CURVF                       source/interfaces/interf/i2curvf.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        I2CURV_REP                    source/interfaces/interf/i2curv_rep.F
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        I2_FFORM                      source/interfaces/interf/i2_fform.F
Chd|        INV3                          source/elements/joint/rskew33.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2CURVF(
     .           MS      ,IN      ,X       ,V       ,VR      ,
     .           A       ,AR      ,STIFN   ,STIFR   ,WEIGHT  ,
     .           NSV     ,MSR     ,IRTL    ,IRECT   ,CRST    ,
     .           MMASS   ,SMASS   ,SINER   ,FSAV    ,NSN     ,
     .           NMN     ,IDEL2   ,FNCONT  ,H3D_DATA,FNCONTP ,
     .           FTCONTP)
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
      INTEGER NSN,NMN,IDEL2
      INTEGER IRECT(4,*),NSV(*),MSR(*),IRTL(*),WEIGHT(*)
C     REAL
      my_real
     .  MS(*),IN(*),MMASS(*),SMASS(*),SINER(*),X(3,*),V(3,*),
     .  VR(3,*),A(3,*),AR(3,*),STIFN(*),STIFR(*),FSAV(*),
     .  CRST(2,*),FNCONT(3,*),FNCONTP(3,*),FTCONTP(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I1,I2,I3,I4,II,IS,IM,JJ,L,NIR            
      INTEGER INOD(4)
      my_real
     .   XMS,INS,STFN,STFR,FXI,FYI,FZI,MXI,MYI,MZI,LS1,LS2,LT1,LT2,
     .   LS,LT,LEN,S,T,W,XC,YC,ZC,DM1,DM2,DIN1,DIN2,DIN3,
     .   SUMHRS,SUMHRT,SUMHPRS,SUMHPRT
      my_real
     .   FSLOC(6),FMLOC(4,6),ROT(9),ROTI(9),DM(4),DIN(4),DSTFN(4),
     .   DSTFR(4),H(4),HH(4),HRS(4),HRT(4),HPS(4),HPT(4),HPRS(4),
     .   HPRT(4),HXS(4),HXT(4)
C======================================================================|
      NIR = 4
C----------------------
      DO II=1,NSN
        IS = NSV(II)
        IF (IS > 0) THEN
          W = WEIGHT(IS)
          L = IRTL(II)
          DO JJ=1,NIR
            INOD(JJ) = IRECT(JJ,L)
          ENDDO
C--- 
          CALL I2CURV_REP(INOD    , X      ,V       ,LS1     ,LS2     ,
     .                    LT1     ,LT2     ,ROT(1)  ,ROT(4)  ,ROT(7)  ,  
     .                    ROT(2)  ,ROT(5)  ,ROT(8)  ,ROT(3)  ,ROT(6)  ,  
     .                    ROT(9)  )                                      
          CALL INV3(ROT,ROTI)
C---
          S=CRST(1,II)
          T=CRST(2,II)
          CALL I2_FFORM(
     .         NIR,S,T,H,HH,HRS,HRT,HPS,HPT,HPRS,HPRT,
     .         HXS,HXT,LS1,LS2,LT1,LT2,LS,LT)
C
          SUMHRS  = ABS(HRS(1) +HRS(2) +HRS(3) +HRS(4))
          SUMHRT  = ABS(HRT(1) +HRT(2) +HRT(3) +HRT(4))
          SUMHPRS = ABS(HPRS(1)+HPRS(2)+HPRS(3)+HPRS(4))
          SUMHPRT = ABS(HPRT(1)+HPRT(2)+HPRT(3)+HPRT(4))
          I1 = INOD(1)                                                
          I2 = INOD(2)                                                
          I3 = INOD(3)                                                
          I4 = INOD(4)                                                
          XC = X(1,I1)*H(1)+X(1,I2)*H(2)+X(1,I3)*H(3)+X(1,I4)*H(4)    
          YC = X(2,I2)*H(1)+X(2,I2)*H(2)+X(2,I3)*H(3)+X(2,I4)*H(4)    
          ZC = X(3,I3)*H(1)+X(3,I2)*H(2)+X(3,I3)*H(3)+X(3,I4)*H(4)    
C
          FXI =  A(1,IS)*W
          FYI =  A(2,IS)*W
          FZI =  A(3,IS)*W
          MXI = AR(1,IS)*W                                            
          MYI = AR(2,IS)*W                                            
          MZI = AR(3,IS)*W                                            
          XMS =   MS(IS)*W
          INS =   IN(IS)*W                                              
          STFN=STIFN(IS)*W                
          STFR=STIFR(IS)*W                        
C
C---      secnd forces -> rep local
C
          FSLOC(1) = ROT(1)*FXI + ROT(4)*FYI + ROT(7)*FZI
          FSLOC(2) = ROT(2)*FXI + ROT(5)*FYI + ROT(8)*FZI
          FSLOC(3) = ROT(3)*FXI + ROT(6)*FYI + ROT(9)*FZI
          FSLOC(4) = ROT(1)*MXI + ROT(4)*MYI + ROT(7)*MZI  
          FSLOC(5) = ROT(2)*MXI + ROT(5)*MYI + ROT(8)*MZI  
          FSLOC(6) = ROT(3)*MXI + ROT(6)*MYI + ROT(9)*MZI
C
C-------  transfer secnd forces to main
          DO JJ=1,NIR                                               
            FMLOC(JJ,1) = H(JJ)*FSLOC(1)             
            FMLOC(JJ,2) = H(JJ)*FSLOC(2)             
            FMLOC(JJ,3) = HH(JJ)*FSLOC(3)             
     .                  + HPS(JJ)*FSLOC(4)  + HPT(JJ)*FSLOC(5)       
            FMLOC(JJ,4) = HRS(JJ)*FSLOC(3)            
     .                  + HPRS(JJ)*FSLOC(4) + HXT(JJ)*FSLOC(5)      
            FMLOC(JJ,5) = HRT(JJ)*FSLOC(3)            
     .                  + HXS(JJ)*FSLOC(4)  + HPRT(JJ)*FSLOC(5)     
            FMLOC(JJ,6) = H(JJ)*FSLOC(6)             
          ENDDO                                                     
C-------  transfer secnd mass + inertia + stiffness to main
          DO JJ=1,NIR                                               
            IM  = INOD(JJ)
            LEN = SQRT((X(1,IM)-XC)**2+(X(2,IM)-YC)**2+(X(3,IM)-ZC)**2)                       
C
            DM1 = H(JJ) *XMS                                        
            DM2 = HH(JJ)*XMS 
            IF (LEN > ZERO) THEN
              DM2 = DM2 + HH(JJ)*(SUMHRS +SUMHRT)*XMS/LEN
     .          +(ABS(HPS(JJ))*SUMHPRS + ABS(HPT(JJ))*SUMHPRT)*INS/LEN
            ENDIF
            IF (DM1 > DM2) THEN
              DM(JJ) = DM1                                
              DSTFN(JJ) = H(JJ)*STFN                      
            ELSEIF (LEN > ZERO) THEN
              DM(JJ) = DM2                                
              DSTFN(JJ) = HH(JJ)*STFN                      
     .          + HH(JJ)*(SUMHRS +SUMHRT)*STFN/LEN
     .          +(ABS(HPS(JJ))*SUMHPRS + ABS(HPT(JJ))*SUMHPRT)*STFR/LEN
            ELSE
              DM(JJ) = DM2                                
              DSTFN(JJ) = HH(JJ)*STFN                      
            ENDIF
C
            DIN1 = ABS(HRS(JJ))*(SUMHRS+SUMHRT)*XMS
     .           +(ABS(HPRS(JJ))*SUMHPRS + ABS(HXT(JJ))*SUMHPRT)*INS
     .           + ABS(HRS(JJ))*XMS*LEN
            DIN2 = ABS(HRT(JJ))*(SUMHRS+SUMHRT)*XMS                          
     .           +(ABS(HPRT(JJ))*SUMHPRT + ABS(HXS(JJ))*SUMHPRS)*INS    
     .           + ABS(HRT(JJ))*XMS*LEN
            DIN3 = H(JJ)*INS
            IF (DIN1 >= DIN2 .AND. DIN1 >= DIN3) THEN
              DIN(JJ) = DIN1                               
              DSTFR(JJ) = ABS(HRS(JJ))*(SUMHRS+SUMHRT)*STFN                   
     .             +(ABS(HPRS(JJ))*SUMHPRS + ABS(HXT(JJ))*SUMHPRT)*STFR
     .             + ABS(HRS(JJ))*STFN*LEN
            ELSEIF (DIN2 >= DIN1 .AND. DIN2 >= DIN3) THEN
              DIN(JJ) = DIN2                               
              DSTFR(JJ) = ABS(HRT(JJ))*(SUMHRS+SUMHRT)*STFN               
     .           +(ABS(HPRT(JJ))*SUMHPRT + ABS(HXS(JJ))*SUMHPRS)*STFR    
     .           + ABS(HRT(JJ))*STFN*LEN
            ELSEIF (DIN3 >= DIN1 .AND. DIN3 >= DIN2) THEN
              DIN(JJ) = DIN3                              
              DSTFR(JJ) = H(JJ)*STFR               
            ENDIF
          ENDDO                                                     
C
C---      update main forces in global frame
C
          DO JJ=1,NIR
            IM  = INOD(JJ)
            A(1,IM) = A(1,IM) +
     .      ROTI(1)*FMLOC(JJ,1)+ROTI(4)*FMLOC(JJ,2)+ROTI(7)*FMLOC(JJ,3)  
            A(2,IM) = A(2,IM) +
     .      ROTI(2)*FMLOC(JJ,1)+ROTI(5)*FMLOC(JJ,2)+ROTI(8)*FMLOC(JJ,3)  
            A(3,IM) = A(3,IM) +
     .      ROTI(3)*FMLOC(JJ,1)+ROTI(6)*FMLOC(JJ,2)+ROTI(9)*FMLOC(JJ,3)
            AR(1,IM) = AR(1,IM) +                                                  
     .      ROTI(1)*FMLOC(JJ,4)+ROTI(4)*FMLOC(JJ,5)+ROTI(7)*FMLOC(JJ,6)  
            AR(2,IM) = AR(2,IM) +                                                      
     .      ROTI(2)*FMLOC(JJ,4)+ROTI(5)*FMLOC(JJ,5)+ROTI(8)*FMLOC(JJ,6)  
            AR(3,IM) = AR(3,IM) +                                                     
     .      ROTI(3)*FMLOC(JJ,4)+ROTI(6)*FMLOC(JJ,5)+ROTI(9)*FMLOC(JJ,6) 
            MS(IM) =  MS(IM) + DM(JJ)
            IN(IM) =  IN(IM) + DIN(JJ)
            STIFN(IM) = STIFN(IM) + DSTFN(JJ)
            STIFR(IM) = STIFR(IM) + DSTFR(JJ)
          ENDDO
C
C---      output of tied contact forces
            CALL I2FORCES(X      ,V  ,A   ,MS    ,IS    ,
     .                  INOD(1),H  ,NIR     ,FSAV    ,FNCONT ,
     .                  FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)                         
C---
          IF (IDEL2/=0.AND.MS(IS)/=ZERO) SMASS(II)=MS(IS)
          IF (IDEL2/=0.AND.MS(IS)/=ZERO) SINER(II)=IN(IS)
          STIFR(IS)=EM20                                            
          STIFN(IS)=EM20                                            
          IN(IS)   =ZERO                                            
          MS(IS)   =ZERO                                            
          A(1,IS)  =ZERO                                            
          A(2,IS)  =ZERO                                            
          A(3,IS)  =ZERO                                            
          AR(1,IS) =ZERO                                            
          AR(2,IS) =ZERO                                            
          AR(3,IS) =ZERO                                            
C---
        ENDIF
      ENDDO
C-----------
      RETURN
      END
