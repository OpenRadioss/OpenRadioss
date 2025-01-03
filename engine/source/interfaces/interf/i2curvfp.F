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
      !||    i2curvfp     ../engine/source/interfaces/interf/i2curvfp.F
      !||--- called by ------------------------------------------------------
      !||    intti2f      ../engine/source/interfaces/interf/intti2f.F
      !||--- calls      -----------------------------------------------------
      !||    i2_fform     ../engine/source/interfaces/interf/i2_fform.F
      !||    i2curv_rep   ../engine/source/interfaces/interf/i2curv_rep.F
      !||    i2forces     ../engine/source/interfaces/interf/i2forces.F
      !||    inv3         ../engine/source/elements/joint/rskew33.F
      !||--- uses       -----------------------------------------------------
      !||    h3d_mod      ../engine/share/modules/h3d_mod.F
      !||====================================================================
      SUBROUTINE I2CURVFP(
     .           MS      ,IN      ,X       ,V       ,VR      ,
     .           A       ,AR      ,STIFN   ,STIFR   ,WEIGHT  ,
     .           NSV     ,MSR     ,IRTL    ,IRECT   ,CRST    ,
     .           NSN     ,NMN     ,IDEL2   ,I0      ,I2SIZE  ,
     .           IADI2   ,MMASS   ,SMASS   ,SINER   ,FSKYI2  ,
     .           FSAV    ,FNCONT  ,H3D_DATA,FNCONTP ,FTCONTP)
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
      INTEGER NSN, NMN, I0,I2SIZE,IDEL2
      INTEGER IRECT(4,*),NSV(*),MSR(*),IRTL(*),WEIGHT(*),IADI2(4,*)
C     REAL
      my_real
     .  MS(*),IN(*),MMASS(*),SMASS(*),SINER(*),X(3,*),V(3,*),VR(3,*),
     .  A(3,*),AR(3,*),STIFN(*),STIFR(*),FSAV(*),CRST(2,*),
     .  FSKYI2(I2SIZE,*),FNCONT(3,*),FNCONTP(3,*),FTCONTP(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I1,I2,I3,I4,II,IS,IM,JJ,L,NN,NIR            
      INTEGER INOD(4)
      my_real
     .   XMS,INS,STFN,STFR,FS(3),MOMS(3),LS1,LS2,LT1,LT2,
     .   LS,LT,LEN,S,T,XC,YC,ZC,DM1,DM2,DIN1,DIN2,DIN3,
     .   SUMHRS,SUMHRT,SUMHPRS,SUMHPRT
      my_real
     .   FSLOC(6),FMLOC(4,6),ROT(9),ROTI(9),DM(4),DIN(4),DSTFN(4),
     .   DSTFR(4),H(4),HH(4),HRS(4),HRT(4),HPS(4),HPT(4),HPRS(4),
     .   HPRT(4),HXS(4),HXT(4),FX(4),FY(4),FZ(4)
C======================================================================|
      NIR = 4
C----------------------
#include "vectorize.inc"
      DO II=1,NMN        
        IM=MSR(II)        
        MMASS(II)=MS(IM)  
        IN(IM)=MAX(EM20,IN(IM))
      ENDDO              
C----------------------
      DO II=1,NSN
        IS = NSV(II)
        IF (IS > 0) THEN
         IF (WEIGHT(IS) == 1) THEN
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
          FS(1) =  A(1,IS)
          FS(2) =  A(2,IS)
          FS(3) =  A(3,IS)
          MOMS(1) = AR(1,IS)                                          
          MOMS(2) = AR(2,IS)                                          
          MOMS(3) = AR(3,IS)                                          
          XMS =   MS(IS)
          INS =   IN(IS)                                            
          STFN=STIFN(IS)              
          STFR=STIFR(IS)                      
C
C---      secnd forces -> rep local
C
          FSLOC(1) = ROT(1)*FS(1) + ROT(4)*FS(2) + ROT(7)*FS(3)
          FSLOC(2) = ROT(2)*FS(1) + ROT(5)*FS(2) + ROT(8)*FS(3)
          FSLOC(3) = ROT(3)*FS(1) + ROT(6)*FS(2) + ROT(9)*FS(3)
          FSLOC(4) = ROT(1)*MOMS(1) + ROT(4)*MOMS(2) + ROT(7)*MOMS(3)  
          FSLOC(5) = ROT(2)*MOMS(1) + ROT(5)*MOMS(2) + ROT(8)*MOMS(3)  
          FSLOC(6) = ROT(3)*MOMS(1) + ROT(6)*MOMS(2) + ROT(9)*MOMS(3)  
C
C-------  transfer secnd forces to main
          DO JJ=1,NIR                                               
            FMLOC(JJ,1) = H(JJ)*FSLOC(1)             
            FMLOC(JJ,2) = H(JJ)*FSLOC(2)             
            FMLOC(JJ,3) = HH(JJ)*FSLOC(3)             
     .                  + HPS(JJ)*FSLOC(4) + HPT(JJ)*FSLOC(5)       
            FMLOC(JJ,4) = HRS(JJ)*FSLOC(3)           
     .                  + HPRS(JJ)*FSLOC(4)+ HXT(JJ)*FSLOC(5)      
            FMLOC(JJ,5) = HRT(JJ)*FSLOC(3)           
     .                  + HXS(JJ)*FSLOC(4) + HPRT(JJ)*FSLOC(5)     
            FMLOC(JJ,6) = H(JJ)*FSLOC(6)             
          ENDDO                                                     
C-------  transfer secnd mass + inertia + stiffness to main
          DO JJ=1,NIR                                               
            IM  = INOD(JJ)
            LEN = SQRT((X(1,IM)-XC)**2+(X(2,IM)-YC)**2+(X(3,IM)-ZC)**2)                       
C
            DM1 = H(JJ) *XMS                                        
            DM2 = HH(JJ)*XMS 
     .          + HH(JJ)*(SUMHRS +SUMHRT)*XMS/LEN
     .          +(ABS(HPS(JJ))*SUMHPRS + ABS(HPT(JJ))*SUMHPRT)*INS/LEN
            IF (DM1 > DM2) THEN
              DM(JJ) = DM1                                
              DSTFN(JJ) = H(JJ)*STFN                      
            ELSE
              DM(JJ) = DM2                                
              DSTFN(JJ) = HH(JJ)*STFN                      
     .          + HH(JJ)*(SUMHRS +SUMHRT)*STFN/LEN
     .          +(ABS(HPS(JJ))*SUMHPRS + ABS(HPT(JJ))*SUMHPRT)*STFR/LEN
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
          DO JJ=1,4
            FX(JJ) = ROTI(1)*FMLOC(JJ,1)+ROTI(4)*FMLOC(JJ,2)+ROTI(7)*FMLOC(JJ,3)    
            FY(JJ) = ROTI(2)*FMLOC(JJ,1)+ROTI(5)*FMLOC(JJ,2)+ROTI(8)*FMLOC(JJ,3)    
            FZ(JJ) = ROTI(3)*FMLOC(JJ,1)+ROTI(6)*FMLOC(JJ,2)+ROTI(9)*FMLOC(JJ,3)
          ENDDO
C
          I0 = I0 + 1                                                     
          JJ = 1                                                          
          NN = IADI2(JJ,I0)                                               

          FSKYI2(1,NN) = FX(JJ)   
          FSKYI2(2,NN) = FY(JJ)    
          FSKYI2(3,NN) = FZ(JJ)       
          FSKYI2(4,NN) = DM(JJ)                                           
          FSKYI2(5,NN) = DSTFN(JJ)                                        
          FSKYI2(6,NN) =                                                  
     .     ROTI(1)*FMLOC(JJ,4)+ROTI(4)*FMLOC(JJ,5)+ROTI(7)*FMLOC(JJ,6)    
          FSKYI2(7,NN) =                                                  
     .     ROTI(2)*FMLOC(JJ,4)+ROTI(5)*FMLOC(JJ,5)+ROTI(8)*FMLOC(JJ,6)    
          FSKYI2(8,NN) =                                                  
     .     ROTI(3)*FMLOC(JJ,4)+ROTI(6)*FMLOC(JJ,5)+ROTI(9)*FMLOC(JJ,6)    
          FSKYI2(9,NN) = DIN(JJ)                                          
          FSKYI2(10,NN)= DSTFR(JJ)                                        
C
          JJ = 2                                                          
          NN = IADI2(JJ,I0)                                    
          FSKYI2(1,NN) = FX(JJ)  
          FSKYI2(2,NN) = FY(JJ)  
          FSKYI2(3,NN) = FZ(JJ) 
          FSKYI2(4,NN) = DM(JJ)                                           
          FSKYI2(5,NN) = DSTFN(JJ)                                        
          FSKYI2(6,NN) =                                                  
     .     ROTI(1)*FMLOC(JJ,4)+ROTI(4)*FMLOC(JJ,5)+ROTI(7)*FMLOC(JJ,6)    
          FSKYI2(7,NN) =                                                  
     .     ROTI(2)*FMLOC(JJ,4)+ROTI(5)*FMLOC(JJ,5)+ROTI(8)*FMLOC(JJ,6)    
          FSKYI2(8,NN) =                                                  
     .     ROTI(3)*FMLOC(JJ,4)+ROTI(6)*FMLOC(JJ,5)+ROTI(9)*FMLOC(JJ,6)    
          FSKYI2(9,NN) = DIN(JJ)                                          
          FSKYI2(10,NN)= DSTFR(JJ)                                        
C
          JJ = 3                                                          
          NN = IADI2(JJ,I0)                                   
          FSKYI2(1,NN) = FX(JJ)
          FSKYI2(2,NN) = FY(JJ)   
          FSKYI2(3,NN) = FZ(JJ)   
          FSKYI2(4,NN) = DM(JJ)                                           
          FSKYI2(5,NN) = DSTFN(JJ)                                        
          FSKYI2(6,NN) =                                                  
     .     ROTI(1)*FMLOC(JJ,4)+ROTI(4)*FMLOC(JJ,5)+ROTI(7)*FMLOC(JJ,6)    
          FSKYI2(7,NN) =                                                  
     .     ROTI(2)*FMLOC(JJ,4)+ROTI(5)*FMLOC(JJ,5)+ROTI(8)*FMLOC(JJ,6)    
          FSKYI2(8,NN) =                                                  
     .     ROTI(3)*FMLOC(JJ,4)+ROTI(6)*FMLOC(JJ,5)+ROTI(9)*FMLOC(JJ,6)    
          FSKYI2(9,NN) = DIN(JJ)                                          
          FSKYI2(10,NN)= DSTFR(JJ)                                        
C
          JJ = 4                                                          
          NN = IADI2(JJ,I0)                                               
          FSKYI2(1,NN) = FX(JJ) 
          FSKYI2(2,NN) = FY(JJ)  
          FSKYI2(3,NN) = FZ(JJ)
          FSKYI2(4,NN) = DM(JJ)                                           
          FSKYI2(5,NN) = DSTFN(JJ)                                        
          FSKYI2(6,NN) =                                                  
     .     ROTI(1)*FMLOC(JJ,4)+ROTI(4)*FMLOC(JJ,5)+ROTI(7)*FMLOC(JJ,6)    
          FSKYI2(7,NN) =                                                  
     .     ROTI(2)*FMLOC(JJ,4)+ROTI(5)*FMLOC(JJ,5)+ROTI(8)*FMLOC(JJ,6)    
          FSKYI2(8,NN) =                                                  
     .     ROTI(3)*FMLOC(JJ,4)+ROTI(6)*FMLOC(JJ,5)+ROTI(9)*FMLOC(JJ,6)    
          FSKYI2(9,NN) = DIN(JJ)                                          
          FSKYI2(10,NN)= DSTFR(JJ)                                        
C
C---      output of tied contact forces
          CALL I2FORCES(X       ,FS     ,FX     ,FY      ,FZ     ,
     .                  INOD(1) ,NIR    ,FSAV   ,FNCONT  ,FNCONTP,
     .                  FTCONTP ,WEIGHT ,H3D_DATA,IS     ,H)
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
       ELSEIF(WEIGHT(-IS) == 1) THEN  
C        stokage ZERO pour noeuds delete par idel2
         I0 = I0 + 1                 
         NN = IADI2(1,I0)            
         FSKYI2(1,NN) = ZERO         
         FSKYI2(2,NN) = ZERO         
         FSKYI2(3,NN) = ZERO         
         FSKYI2(4,NN) = ZERO         
         FSKYI2(5,NN) = ZERO         
         FSKYI2(6,NN) = ZERO         
         FSKYI2(7,NN) = ZERO         
         FSKYI2(8,NN) = ZERO         
         FSKYI2(9,NN) = ZERO         
         FSKYI2(10,NN)= ZERO         
         NN = IADI2(2,I0)            
         FSKYI2(1,NN) = ZERO         
         FSKYI2(2,NN) = ZERO         
         FSKYI2(3,NN) = ZERO         
         FSKYI2(4,NN) = ZERO         
         FSKYI2(5,NN) = ZERO         
         FSKYI2(6,NN) = ZERO         
         FSKYI2(7,NN) = ZERO         
         FSKYI2(8,NN) = ZERO         
         FSKYI2(9,NN) = ZERO         
         FSKYI2(10,NN)= ZERO         
         NN = IADI2(3,I0)            
         FSKYI2(1,NN) = ZERO         
         FSKYI2(2,NN) = ZERO         
         FSKYI2(3,NN) = ZERO         
         FSKYI2(4,NN) = ZERO         
         FSKYI2(5,NN) = ZERO         
         FSKYI2(6,NN) = ZERO         
         FSKYI2(7,NN) = ZERO         
         FSKYI2(8,NN) = ZERO         
         FSKYI2(9,NN) = ZERO         
         FSKYI2(10,NN)= ZERO         
         NN = IADI2(4,I0)            
         FSKYI2(1,NN) = ZERO         
         FSKYI2(2,NN) = ZERO         
         FSKYI2(3,NN) = ZERO         
         FSKYI2(4,NN) = ZERO         
         FSKYI2(5,NN) = ZERO         
         FSKYI2(6,NN) = ZERO         
         FSKYI2(7,NN) = ZERO         
         FSKYI2(8,NN) = ZERO         
         FSKYI2(9,NN) = ZERO         
         FSKYI2(10,NN)= ZERO         
        ENDIF
      ENDDO
C-----------
      RETURN
      END
