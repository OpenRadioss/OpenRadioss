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
C----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
Chd|====================================================================
Chd|  REDEF3                        source/elements/spring/redef3.F
Chd|-- called by -----------
Chd|        R1DEF3                        source/elements/spring/r1def3.F
Chd|        R23L108DEF3                   source/elements/spring/r23l108def3.F
Chd|        R23L113DEF3                   source/elements/spring/r23l113def3.F
Chd|        R23L114DEF3                   source/elements/spring/r23l114def3.F
Chd|        R2DEF3                        source/elements/spring/r2def3.F
Chd|        R3DEF3                        source/elements/spring/r3def3.F
Chd|        R4DEF3                        source/elements/spring/r4def3.F
Chd|        R6DEF3                        source/elements/spring/r6def3.F
Chd|-- calls ---------------
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        VINTER2DP                     source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE REDEF3(FX      ,XK     ,DX     ,FXEP       ,DXOLD ,
     .                  DPX     ,TF     ,NPF    ,XC         ,OFF   ,
     .                  E       ,DPX2   ,ANIM   ,IANI       ,POS   ,
     .                  FR_WAVE ,XL0    ,DMN    ,DMX        ,DVX   ,
     .                  FF      ,LSCALE ,EE     ,GF3        ,IFUNC3,
     .                  YIELD   ,ALDP   ,AK     ,B          ,D     ,
     .                  IECROU  ,IFUNC  ,IFV    ,IFUNC2     ,EPLA  ,
     .                  XX_OLD  ,FX_MAX ,E_OFFSET,XKC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPF(*),IANI,IECROU(MVSIZ),IFUNC(MVSIZ),IFV(MVSIZ),
     .    IFUNC2(MVSIZ)
C     REAL
      my_real
     .   FX(*), XK(*), DX(*), FXEP(*), DXOLD(*), DPX(*), TF(*), XC(*),
     .   OFF(*), E(*), DPX2(*),ANIM(*),POS(5,*),FR_WAVE(*),
     .   XL0(*),DMN(*),DMX(*),DVX(*),
     .   FF(*),LSCALE(*),EE(*),YIELD(*),
     .   AK(MVSIZ),B(MVSIZ),D(MVSIZ),EPLA(MVSIZ),XX_OLD(*),
     .   FX_MAX(*),E_OFFSET(*),XKC(*)
      DOUBLE PRECISION ALDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     . JPOS(MVSIZ), JLEN(MVSIZ),JAD(MVSIZ),
     . JPOS2(MVSIZ), JLEN2(MVSIZ),JPOS3(MVSIZ), JLEN3(MVSIZ), 
     . JAD2(MVSIZ),JFUNC,JFUNC2,JDMP,JECROU(-1:11),J2DMP,K1,NP1,NP2,
     . I, J, II, INTERP, K, IC1(MVSIZ), JLEN4(MVSIZ),FUNC,FUND,J1,
     . IC2(MVSIZ),IC0(MVSIZ),TMP1(MVSIZ),TMP2(MVSIZ), IFUNC3(MVSIZ),
     . J2POS(MVSIZ), J2LEN(MVSIZ),J2AD(MVSIZ),J2FUNC,JPOS4(MVSIZ)
c     REAL ou REAL*8
      my_real 
     .     FINTER 
      EXTERNAL FINTER
      my_real
     . DDX(MVSIZ),
     . FOLD(MVSIZ), GX(MVSIZ), DXELA(MVSIZ), 
     . DYDX(MVSIZ), XX(MVSIZ), XX2(MVSIZ), XX3(MVSIZ), YY(MVSIZ),
     . YY2(MVSIZ), YY3(MVSIZ), DYDX2(MVSIZ), DYDX3(MVSIZ), 
     . DYDXV(MVSIZ), DPERM(MVSIZ), XKI, X, XBID, DVV, DFAC, DT11, 
     . DAMP, DAMM,FMAX(MVSIZ),DVXS(MVSIZ),GF3(MVSIZ),DYDXV2(MVSIZ),
     . FMAX2,FMIN(MVSIZ),YP0,YP1,YP2,B1,GX2(MVSIZ),FY0(MVSIZ),
     . XFY0(MVSIZ),YCUT(MVSIZ),XCUT(MVSIZ),XN3FY0(MVSIZ),
     . UL(MVSIZ),FYIELD(MVSIZ),DPL(MVSIZ),YY22(MVSIZ),XFYN1(MVSIZ),
     . C1,D1,A2,TMP,T2B1,T2B1PHI,T1M,T2M,T1PHIM,T2PHIM,B2,C2,D2,
     . T1,T2,T1PHI,T2PHI,FMAXPHI,A1PHI,B1PHI,T1B1,T1B1PHI,DPERMPHI,
     . ftmp,t1t2,t1t2phi,t1t2b1,t1t2b1phi,XI1,XI2,YI1,YI2,SI1,SI2,TI1,
     . TI2,S1,S2,T1C,T2C,X1,X2,Y1,Y2,DYDXC,DTDS,SX,TY,DDX_T,DDX_C,
     . AY0,AN3Y0(MVSIZ),DDPX(MVSIZ),AY1,AYN3,
     . XFYN33(MVSIZ),XFYN11(MVSIZ),DPX3(MVSIZ),DPX22(MVSIZ),XX22(MVSIZ),
     . EDECAL2(MVSIZ),EDECAL3(MVSIZ),ECROU(MVSIZ),DDXT,DDXC
C=======================================================================
      DT11 = DT1
      IF(DT11==ZERO)DT11 = EP30
      DO I=LFT,LLT
        DX(I)=DX(I)/XL0(I)
        DXOLD(I)=DXOLD(I)/XL0(I)
        DPX(I)=DPX(I)/XL0(I)
        DPX2(I)=DPX2(I)/XL0(I)
        E(I)=E(I)/XL0(I)
      ENDDO
C
      DO 80  I=LFT,LLT
      FOLD(I)=FX(I)
      DDX(I)= (DX(I)-DXOLD(I))
      DVX(I)= DDX(I)/ DT11
      DVXS(I)= DVX(I)*FF(I)
   80 CONTINUE
C
C
      IF(IANI/=0)THEN
        DO I=LFT,LLT
          II=I+NFT
          DAMP=DX(I)/MAX(DMX(I),EM15)
          DAMM=DX(I)/MIN(DMN(I),-EM15)
          ANIM(II)=MAX(ANIM(II),DAMP,DAMM)
          ANIM(II)=MIN(ANIM(II),ONE)
        ENDDO
      ENDIF
C-------------------------------------
C        VECTOR INTERPOLATION (ADRESS)
C-------------------------------------
      JECROU(-1)  = 0
      JECROU(0)  = 0
      JECROU(1)  = 0
      JECROU(2)  = 0
      JECROU(3)  = 0
      JECROU(4)  = 0
      JECROU(5)  = 0
      JECROU(6)  = 0
      JECROU(7)  = 0
      JECROU(8)  = 0
      JECROU(9)  = 0
      JECROU(10) = 0
      JECROU(11) = 0
      INTERP = 0
      JDMP = 0
      J2DMP = 0
C
      DO I=LFT,LLT
       IF(IECROU(I) == 9)THEN
         JECROU(9) = JECROU(9) + 1
       ELSEIF(IECROU(I) == 11)THEN
         JECROU(11) = JECROU(11) + 1
       ELSEIF(IFUNC(I)==0)THEN  ! ifunc =IGEO(101)-FCT_id1
         JECROU(-1) = JECROU(-1) + 1
c modif pour vectorisation
       ELSEIF(IECROU(I)==0)THEN
         JECROU(0) = JECROU(0) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==1)THEN
         JECROU(1) = JECROU(1) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==2)THEN
         JECROU(2) = JECROU(2) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==3)THEN
         JECROU(3) = JECROU(3) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==4)THEN
         JECROU(4) = JECROU(4) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==5)THEN
         JECROU(5) = JECROU(5) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==6)THEN
         JECROU(6) = JECROU(6) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==7)THEN
         JECROU(7) = JECROU(7) + 1
         INTERP = 1
       ELSEIF(IECROU(I) == 8)THEN
         JECROU(8) = JECROU(8) + 1
         INTERP = 1
       ELSEIF(IECROU(I) == 10)THEN
         JECROU(10) = JECROU(10) + 1
         INTERP = 1
       ENDIF
       IF(IFV(I)/=0) JDMP = JDMP + 1
       IF(IFUNC3(I)/=0) J2DMP = J2DMP + 1
      ENDDO
C
      IF(INTERP>0)THEN
        DO I=LFT,LLT
          JPOS(I)  = NINT(POS(1,I))
          JPOS2(I) = NINT(POS(2,I))
          JPOS3(I) = NINT(POS(3,I))
          
          JFUNC =MAX(1,IFUNC(I))
          JFUNC2=MAX(1,IFUNC2(I))
          JAD(I)   = NPF(JFUNC) / 2  + 1
          JAD2(I)  = NPF(JFUNC2) / 2 + 1
          JLEN(I)  = NPF(JFUNC+1) / 2  - JAD(I)  - JPOS(I)
          JLEN2(I) = NPF(JFUNC2+1) / 2 - JAD2(I) - JPOS2(I)
          JLEN3(I) = NPF(JFUNC+1) / 2  - JAD(I)  - JPOS3(I)
          XX(I) =ZERO
          XX2(I)=ZERO
          XX3(I)=ZERO  
        ENDDO
      ENDIF            
C-------------------------------------
C        NON LINEAIRE ELASTIQUE, F=f(total length)
C-------------------------------------
      IF (JECROU(8) == LLT) THEN
        DO I=LFT,LLT
          XX(I) = ALDP(I)
        ENDDO
      ELSEIF (JECROU(8) > 0) THEN
        DO I=LFT,LLT
          IF (IECROU(I) == 8) THEN
            XX(I) = ALDP(I)
          ELSE
            XX(I) = DX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        NON LINEAIRE ELASTIQUE
C        NL ELASTO PLASTIQUE (TENSION COMPRESSION DECOUPLEE)
C-------------------------------------
      IF(JECROU(0)+JECROU(2)==LLT)THEN
        DO I=LFT,LLT
          XX(I)=DX(I)
        ENDDO
      ELSEIF(JECROU(0)+JECROU(2)>0)THEN
        DO I=LFT,LLT
          IF(IFUNC(I)/=0.AND.(IECROU(I)==0.OR.IECROU(I)==2))THEN
            XX(I)=DX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (ISOTROPE)
C        ELASTO PLASTIQUE (TENSION COMPRESSION COUPLEE 6 DOF COUPLES)
C-------------------------------------
      IF(JECROU(1)+JECROU(3)==LLT)THEN
        DO I=LFT,LLT
           FX(I)=FXEP(I)+XK(I)*DDX(I)
           IF(FX(I)>=0.)THEN
             XX(I)=DPX(I)+FX(I)/XK(I)
           ELSE
             XX(I)=-DPX(I)+FX(I)/XK(I)
           ENDIF
        ENDDO
      ELSEIF(JECROU(1)+JECROU(3)>0)THEN
        DO I=LFT,LLT
         IF(IFUNC(I)/=0.AND.(IECROU(I)==1.OR.IECROU(I)==3))THEN
           FX(I)=FXEP(I)+XK(I)*DDX(I)
           IF(FX(I)>=ZERO)THEN
             XX(I)=DPX(I)+FX(I)/XK(I)
           ELSE
             XX(I)=-DPX(I)+FX(I)/XK(I)
           ENDIF
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (ECOUISSAGE CINEMATIQUE)
C-------------------------------------
      IF(JECROU(4)==LLT)THEN
        DO I=LFT,LLT
           INTERP = MAX(2,INTERP)
           XX(I) =DX(I)
           XX2(I)=DX(I)
        ENDDO
      ELSEIF(JECROU(4)>0)THEN
        DO I=LFT,LLT
         IF(IFUNC(I)/=0.AND.IECROU(I)==4)THEN
           INTERP = MAX(2,INTERP)
           XX(I) =DX(I)
           XX2(I)=DX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (TENSION COMPRESSION DECOUPLEE)
C         (D/R)ECHARGEMENT NON LINEAIRE
C         DPX = DEPLACEMENT MAXIMUM (ET NON PLASTIQUE)
C-------------------------------------
      IF(JECROU(5)==LLT)THEN
        DO I=LFT,LLT
           XX(I)=DX(I)
           IF(DX(I)>ZERO)THEN
             INTERP = MAX(3,INTERP)
             XX2(I)=DPX(I)
             XX3(I)=DPX(I)
           ELSEIF(DX(I)<ZERO)THEN
             INTERP = MAX(3,INTERP)
             XX2(I)=DPX2(I)
             XX3(I)=DPX2(I)
           ELSE
             INTERP = MAX(1,INTERP)
           ENDIF
        ENDDO
      ELSEIF(JECROU(5)>0)THEN
        DO I=LFT,LLT
         IF(IFUNC(I)/=0.AND.IECROU(I)==5)THEN
           XX(I)=DX(I)
           IF(DX(I)>ZERO)THEN
             INTERP = MAX(3,INTERP)
             XX2(I)=DPX(I)
             XX3(I)=DPX(I)
           ELSEIF(DX(I)<ZERO)THEN
             INTERP = MAX(3,INTERP)
             XX2(I)=DPX2(I)
             XX3(I)=DPX2(I)
           ELSE
             INTERP = MAX(1,INTERP)
           ENDIF
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (ECOUISSAGE ISOTROPE)
C-------------------------------------
      IF(JECROU(6)==LLT)THEN
        DO I=LFT,LLT                                          
          FUND = IFUNC2(I)     ! courbe N3 de unload                                                             
          NP2  = (NPF(FUND+1)-NPF(FUND))*HALF                       
          AN3Y0(I)= ZERO 
          DO  K=2,NP2                                                       
             K1=2*(K-2)                                                
             X1=TF(NPF(FUND)+K1)                                       
             X2=TF(NPF(FUND)+K1+2)                                     
             Y1=TF(NPF(FUND)+K1+1)                                     
             Y2=TF(NPF(FUND)+K1+3) 
             IF((FXEP(I)< Y2.AND.FXEP(I)>=Y1))THEN  
                AN3Y0(I)=(Y2-Y1)/ (X2-X1)                     
                XN3FY0(I)=(FXEP(I)-Y1)/AN3Y0(I) + X1   !ABS DE N3  
                EXIT     
             ENDIF 
           ENDDO 
           IF (AN3Y0(I)== ZERO)THEN ! extrapolation (exterieur aux points de l input)
             X1=TF(NPF(FUND)+(NP2-2)*2)                                       
             X2=TF(NPF(FUND)+(NP2-2)*2+2)                                     
             Y1=TF(NPF(FUND)+(NP2-2)*2+1)                                     
             Y2=TF(NPF(FUND)+(NP2-2)*2+3) 
C
             XI1=TF(NPF(FUND))                                       
             XI2=TF(NPF(FUND)+2)                                     
             YI1=TF(NPF(FUND)+1)                                     
             YI2=TF(NPF(FUND)+3) 
             IF(FXEP(I)>Y2)AN3Y0(I)=(Y2-Y1)/ (X2-X1) 
             IF(FXEP(I)<YI1)AN3Y0(I)=(YI2-YI1)/ (XI2-XI1)
           ENDIF    
           FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
           XX(I)=SIGN(ABS(XX_OLD(I)),FX(I))
           XX(I)=XX(I)+DDX(I)
           INTERP=0
        ENDDO
      ELSEIF(JECROU(6)>0)THEN
        DO I=LFT,LLT
         IF(IFUNC(I)/=0.AND.IECROU(I)== 6)THEN
          FUND = IFUNC2(I)                                                                  
          NP2  = (NPF(FUND+1)-NPF(FUND))*HALF                       
          AN3Y0(I)= ZERO 
          DO  K=2,NP2                                                       
             K1=2*(K-2)                                                
             X1=TF(NPF(FUND)+K1)                                       
             X2=TF(NPF(FUND)+K1+2)                                     
             Y1=TF(NPF(FUND)+K1+1)                                     
             Y2=TF(NPF(FUND)+K1+3) 
             IF((FXEP(I)< Y2.AND.FXEP(I)>=Y1))THEN              
                AN3Y0(I)=(Y2-Y1)/ (X2-X1)                     
                XN3FY0(I)=(FXEP(I)-Y1)/AN3Y0(I) + X1   !ABS DE N3  
                EXIT     
             ENDIF 
           ENDDO 
           IF (AN3Y0(I)== ZERO)THEN
             X1=TF(NPF(FUND)+(NP2-2)*2)                                       
             X2=TF(NPF(FUND)+(NP2-2)*2+2)                                     
             Y1=TF(NPF(FUND)+(NP2-2)*2+1)                                     
             Y2=TF(NPF(FUND)+(NP2-2)*2+3) 
C
             XI1=TF(NPF(FUND))                                       
             XI2=TF(NPF(FUND)+2)                                     
             YI1=TF(NPF(FUND)+1)                                     
             YI2=TF(NPF(FUND)+3) 
             IF(FXEP(I)>Y2)AN3Y0(I)=(Y2-Y1)/ (X2-X1) 
             IF(FXEP(I)<YI1)AN3Y0(I)=(YI2-YI1)/ (XI2-XI1)
           ENDIF    
           FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
           XX(I)=SIGN(ABS(XX_OLD(I)),FX(I))
           XX(I)=XX(I)+DDX(I)
          ENDIF
        ENDDO
      ENDIF
c-------------------------------------
c      ELASTO PLASTIC TWO CURVES FOR LOAD AND UNLOAD
c-------------------------------------
      IF(JECROU(7)==LLT)THEN
        DO I=LFT,LLT
           INTERP = MAX(2,INTERP)
           XX(I) =DX(I)
           XX2(I)=DX(I)
        ENDDO
      ELSEIF(JECROU(7)>0)THEN
        DO I=LFT,LLT
         IF(IFUNC(I)/=0.AND.IECROU(I)==7)THEN
           INTERP = MAX(2,INTERP)
           XX(I) =DX(I)
           XX2(I)=DX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PERFECLTY PLASTIQUE (ECOUISSAGE ISOTROPE) - input by values
C-------------------------------------
      IF(JECROU(9)==LLT)THEN
        DO I=LFT,LLT                                                              
          AN3Y0(I)= ZERO
          IF (ABS(FXEP(I)) > FX_MAX(I)) THEN
            AN3Y0(I)= ZERO
          ELSE
             AN3Y0(I)= XK(I)
          ENDIF
C                                                          
          FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
          XX(I)=SIGN(ABS(XX_OLD(I)),FX(I))
          XX(I)=XX(I)+DDX(I)
        ENDDO
      ELSEIF(JECROU(9)>0)THEN
        DO I=LFT,LLT
          IF(IECROU(I)== 9) THEN                      
            AN3Y0(I)= ZERO 
            IF (ABS(FXEP(I)) > FX_MAX(I)) THEN
              AN3Y0(I)= ZERO
            ELSE
              AN3Y0(I)= XK(I)
            ENDIF
C    
            FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
            XX(I)=SIGN(ABS(XX_OLD(I)),FX(I))
            XX(I)=XX(I)+DDX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (ECOUISSAGE ISOTROPE) - PERFECLTY PLASTIC IN COMPRESSION
C-------------------------------------
      IF(JECROU(10)==LLT)THEN
        DO I=LFT,LLT                                          
          FUND = IFUNC2(I)     ! courbe N3 de unload                                                             
          NP2  = (NPF(FUND+1)-NPF(FUND))*HALF                       
          AN3Y0(I)= ZERO
          DXELA(I)=DX(I)-DPX(I)
          IF (((DXELA(I) >= ZERO).OR.(FXEP(I) >= ZERO)).AND.(FUND > 0)) THEN
C--- Tension - load curve is used
            DO  K=2,NP2                                                       
              K1=2*(K-2)                                                
              X1=TF(NPF(FUND)+K1)                                       
              X2=TF(NPF(FUND)+K1+2)                                     
              Y1=TF(NPF(FUND)+K1+1)                                     
              Y2=TF(NPF(FUND)+K1+3) 
              IF((FXEP(I)< Y2.AND.FXEP(I)>=Y1))THEN  
                AN3Y0(I)=(Y2-Y1)/ (X2-X1)                     
                XN3FY0(I)=(FXEP(I)-Y1)/AN3Y0(I) + X1   !ABS DE N3  
                EXIT     
              ENDIF 
            ENDDO
C            IF ((I==1).AND.(JECROU(6)>0)) print *,"LL2_A",FXEP(I),AN3Y0(I) 
            IF (AN3Y0(I)== ZERO)THEN ! extrapolation (exterieur aux points de l input)
              X1=TF(NPF(FUND)+(NP2-2)*2)                                       
              X2=TF(NPF(FUND)+(NP2-2)*2+2)                                     
              Y1=TF(NPF(FUND)+(NP2-2)*2+1)                                     
              Y2=TF(NPF(FUND)+(NP2-2)*2+3) 
C
              XI1=TF(NPF(FUND))                                       
              XI2=TF(NPF(FUND)+2)                                     
              YI1=TF(NPF(FUND)+1)                                     
              YI2=TF(NPF(FUND)+3) 
              IF(FXEP(I)>Y2)AN3Y0(I)=(Y2-Y1)/ (X2-X1) 
              IF(FXEP(I)<YI1)AN3Y0(I)=(YI2-YI1)/ (XI2-XI1)
            ENDIF
C----       Crossing of compression/tension line - mix stiffness computed
            IF ((DXELA(I) < ZERO).AND.(ABS(DDX(I)) > ZERO)) THEN
              DDXT = -FXEP(I)/AN3Y0(I)
              DDXC = DDX(I) - DDXT
              AN3Y0(I) = (DDXT/DDX(I))*AN3Y0(I) + (DDXC/DDX(I))*XKC(I)
            ENDIF
C
            IF (DXELA(I) >= ZERO) XX(I)=XX_OLD(I)+DDX(I)
          ELSE
C--- Compression - perfectly plastic behavior
            AN3Y0(I)= XKC(I)
          ENDIF
          FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
          INTERP=0
        ENDDO
      ELSEIF(JECROU(10)>0)THEN
        DO I=LFT,LLT
         IF(IFUNC(I)/=0.AND.IECROU(I)== 10)THEN
          FUND = IFUNC2(I)     ! courbe N3 de unload                                                             
          NP2  = (NPF(FUND+1)-NPF(FUND))*HALF                       
          AN3Y0(I)= ZERO
          DXELA(I)=DX(I)-DPX(I)
          IF (((DXELA(I) >= ZERO).OR.(FXEP(I) >= ZERO)).AND.(FUND > 0)) THEN
C--- Tension - load curve is used
            DO  K=2,NP2                                                       
              K1=2*(K-2)                                                
              X1=TF(NPF(FUND)+K1)                                       
              X2=TF(NPF(FUND)+K1+2)                                     
              Y1=TF(NPF(FUND)+K1+1)                                     
              Y2=TF(NPF(FUND)+K1+3) 
              IF((FXEP(I)< Y2.AND.FXEP(I)>=Y1))THEN  
                AN3Y0(I)=(Y2-Y1)/ (X2-X1)                     
                XN3FY0(I)=(FXEP(I)-Y1)/AN3Y0(I) + X1   !ABS DE N3  
                EXIT     
              ENDIF 
            ENDDO
C            IF ((I==1).AND.(JECROU(6)>0)) print *,"LL2_A",FXEP(I),AN3Y0(I) 
            IF (AN3Y0(I)== ZERO)THEN ! extrapolation (exterieur aux points de l input)
              X1=TF(NPF(FUND)+(NP2-2)*2)                                       
              X2=TF(NPF(FUND)+(NP2-2)*2+2)                                     
              Y1=TF(NPF(FUND)+(NP2-2)*2+1)                                     
              Y2=TF(NPF(FUND)+(NP2-2)*2+3) 
C
              XI1=TF(NPF(FUND))                                       
              XI2=TF(NPF(FUND)+2)                                     
              YI1=TF(NPF(FUND)+1)                                     
              YI2=TF(NPF(FUND)+3) 
              IF(FXEP(I)>Y2)AN3Y0(I)=(Y2-Y1)/ (X2-X1) 
              IF(FXEP(I)<YI1)AN3Y0(I)=(YI2-YI1)/ (XI2-XI1)
            ENDIF
C----       Crossing of compression/tension line - mix stiffness computed
            IF ((DXELA(I) < ZERO).AND.(ABS(DDX(I)) > ZERO)) THEN
              DDXT = -FXEP(I)/AN3Y0(I)
              DDXC = DDX(I) - DDXT
              AN3Y0(I) = (DDXT/DDX(I))*AN3Y0(I) + (DDXC/DDX(I))*XKC(I)
            ENDIF
C
            IF (DXELA(I) >= ZERO) XX(I)=XX_OLD(I)+DDX(I)
          ELSE
C--- Compression - perfectly plastic behavior
            AN3Y0(I)= XKC(I)
          ENDIF
          FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        LINEAR ELASTIC in tension - perfleclty palstic in compression (same as 10 without curve)
C-------------------------------------
      IF(JECROU(11)==LLT)THEN
        DO I=LFT,LLT                                                              
          AN3Y0(I)= ZERO
          DXELA(I)=DX(I)-DPX(I)
          AN3Y0(I)= XK(I)
          IF ((DXELA(I) >= ZERO).OR.(FXEP(I) >= ZERO)) THEN
C----       Crossing of compression/tension line - mix stiffness computed
            IF ((DXELA(I) < ZERO).AND.(ABS(DDX(I)) > ZERO)) THEN
              DDXT = -FXEP(I)/AN3Y0(I)
              DDXC = DDX(I) - DDXT
              AN3Y0(I) = (DDXT/DDX(I))*AN3Y0(I) + (DDXC/DDX(I))*XKC(I)
            ENDIF
          ELSE
            AN3Y0(I)= XKC(I)
          ENDIF                                                          
          FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
        ENDDO
      ELSEIF(JECROU(11)>0)THEN
        DO I=LFT,LLT
          IF(IECROU(I)== 11) THEN                      
            AN3Y0(I)= ZERO
            DXELA(I)=DX(I)-DPX(I)
            IF ((DXELA(I) >= ZERO).OR.(FXEP(I) >= ZERO)) THEN
              AN3Y0(I)= XK(I)
C----       Crossing of compression/tension line - mix stiffness computed
              IF ((DXELA(I) < ZERO).AND.(ABS(DDX(I)) > ZERO)) THEN
                DDXT = -FXEP(I)/AN3Y0(I)
                DDXC = DDX(I) - DDXT
                AN3Y0(I) = (DDXT/DDX(I))*AN3Y0(I) + (DDXC/DDX(I))*XKC(I)
              ENDIF
            ELSE
              AN3Y0(I)= XKC(I)
            ENDIF                                                          
            FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
c     VECTOR INTERPOLATION
C-------------------------------------
      DO I=LFT,LLT
        XX(I)  = XX(I) *LSCALE(I)                   
        XX2(I) = XX2(I)*LSCALE(I)                   
        XX3(I) = XX3(I)*LSCALE(I)                   
      ENDDO                          
C----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (IRESP==1) THEN
        IF(INTERP>=1)
     .      CALL VINTER2DP(TF,JAD ,JPOS ,JLEN ,LLT,XX ,DYDX ,YY)
        IF(INTERP>=2)
     .      CALL VINTER2DP(TF,JAD2,JPOS2,JLEN2,LLT,XX2,DYDX2,YY2)
        IF(INTERP>=3)
     .      CALL VINTER2DP(TF,JAD ,JPOS3,JLEN3,LLT,XX3,DYDX3,YY3)
      ELSE
        IF(INTERP>=1)
     .      CALL VINTER2(TF,JAD ,JPOS ,JLEN ,LLT,XX ,DYDX ,YY )
        IF(INTERP>=2)
     .      CALL VINTER2(TF,JAD2,JPOS2,JLEN2,LLT,XX2,DYDX2,YY2)
        IF(INTERP>=3)
     .      CALL VINTER2(TF,JAD ,JPOS3,JLEN3,LLT,XX3,DYDX3,YY3)
      ENDIF
      IF(INTERP>0)THEN
        DO I=LFT,LLT
          POS(1,I) = JPOS(I)
          POS(2,I) = JPOS2(I)
          POS(3,I) = JPOS3(I)
        ENDDO
      ENDIF
C-------------------------------------
C        LINEAIRE ELASTIQUE
C-------------------------------------
      IF(JECROU(-1)==LLT)THEN
        DO I=LFT,LLT
           FX(I)=XK(I)*DX(I)
        ENDDO
      ELSEIF(JECROU(-1)>0)THEN
        DO I=LFT,LLT
          IF(IFUNC(I)==0)THEN
            FX(I)=XK(I)*DX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        NON LINEAIRE ELASTIQUE, F = f(total length)
C-------------------------------------
      IF (JECROU(8) == LLT)THEN
        DO I=LFT,LLT
          FX(I)=YY(I)
        ENDDO
      ELSEIF (JECROU(8) > 0)THEN
        DO I=LFT,LLT
          IF (IECROU(I) == 8) FX(I)=YY(I)
        ENDDO
      ENDIF
C-------------------------------------
C        NON LINEAIRE ELASTIQUE
C-------------------------------------
      IF(JECROU(0)==LLT)THEN
        DO I=LFT,LLT
           FX(I)=YY(I)
        ENDDO
      ELSEIF(JECROU(0)>0)THEN
        DO I=LFT,LLT
           IF(IFUNC(I)/=0.AND.IECROU(I)==0) FX(I)=YY(I)
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (ISOTROPE)
C-------------------------------------
      IF(JECROU(1)==LLT)THEN
        DO I=LFT,LLT
           IF(FX(I)>=ZERO.AND.FX(I)>YY(I))THEN
             DPX(I)=DPX(I)+(FX(I)-YY(I))/XK(I)
             FX(I)=YY(I)
           ELSEIF(FX(I)<ZERO.AND.FX(I)<YY(I))THEN
             DPX(I)=DPX(I)+(YY(I)-FX(I))/XK(I)
             FX(I)=YY(I)
           ENDIF
           FXEP(I)=FX(I)
        ENDDO
      ELSEIF(JECROU(1)>0)THEN
        DO I=LFT,LLT
         IF(IFUNC(I)/=0.AND.IECROU(I)==1)THEN
           IF(FX(I)>=ZERO.AND.FX(I)>YY(I))THEN
             DPX(I)=DPX(I)+(FX(I)-YY(I))/XK(I)
             FX(I)=YY(I)
           ELSEIF(FX(I)<ZERO.AND.FX(I)<YY(I))THEN
             DPX(I)=DPX(I)+(YY(I)-FX(I))/XK(I)
             FX(I)=YY(I)
           ENDIF
           FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (TENSION COMPRESSION DECOUPLEE)
C-------------------------------------
      IF(JECROU(2)==LLT)THEN
        DO I=LFT,LLT
           IF(DX(I)>DPX(I))THEN
             FX(I)  = XK(I) * (DX(I)-DPX(I))
             FXEP(I)= YY(I)
             FX(I)  = MIN(FX(I),FXEP(I))
             DPX(I) = DX(I) - FX(I) / XK(I) 
           ELSEIF(DX(I)<DPX2(I))THEN
             FX(I)   = XK(I) * (DX(I)-DPX2(I))
             FXEP(I) = YY(I)
             FX(I)   = MAX(FX(I),FXEP(I))
             DPX2(I) = DX(I) - FX(I) / XK(I) 
           ELSE
             FX(I)   = ZERO
           ENDIF
        ENDDO
      ELSEIF(JECROU(2)>0)THEN
        DO I=LFT,LLT
         IF(IFUNC(I)/=0.AND.IECROU(I)==2)THEN
           IF(DX(I)>DPX(I))THEN
             FX(I)  = XK(I) * (DX(I)-DPX(I))
             FXEP(I)= YY(I)
             FX(I)  = MIN(FX(I),FXEP(I))
             DPX(I) = DX(I) - FX(I) / XK(I) 
           ELSEIF(DX(I)<DPX2(I))THEN
             FX(I)   = XK(I) * (DX(I)-DPX2(I))
             FXEP(I) = YY(I)
             FX(I)   = MAX(FX(I),FXEP(I))
             DPX2(I) = DX(I) - FX(I) / XK(I) 
           ELSE
             FX(I)   = ZERO
           ENDIF
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (TENSION COMPRESSION COUPLEE 6 DOF COUPLES)
C-------------------------------------
      IF(JECROU(3)==LLT)THEN
        DO I=LFT,LLT
           IF(FX(I)>=ZERO.AND.FX(I)>YY(I))THEN
               EPLA(I)=EPLA(I)+ABS(YY(I)*(FX(I)-YY(I))/XK(I))
               FX(I)=YY(I)
           ELSEIF(FX(I)<ZERO.AND.FX(I)<YY(I))THEN
               EPLA(I)=EPLA(I)+ABS(YY(I)*(YY(I)-FX(I))/XK(I))
               FX(I)=YY(I)
           ENDIF
           FXEP(I)=FX(I)
        ENDDO
      ELSEIF(JECROU(3)>0)THEN
        DO I=LFT,LLT
         IF(IFUNC(I)/=0.AND.IECROU(I)==3)THEN
           IF(FX(I)>=ZERO.AND.FX(I)>YY(I))THEN
               EPLA(I)=EPLA(I)+ABS(YY(I)*(FX(I)-YY(I))/XK(I))
               FX(I)=YY(I)
           ELSEIF(FX(I)<ZERO.AND.FX(I)<YY(I))THEN
               EPLA(I)=EPLA(I)+ABS(YY(I)*(YY(I)-FX(I))/XK(I))
               FX(I)=YY(I)
           ENDIF
           FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (ECOUISSAGE CINEMATIQUE)
C-------------------------------------
      IF(JECROU(4)==LLT)THEN
        DO I=LFT,LLT
          FX(I) = FXEP(I) + XK(I)*DDX(I)
          IC2(I)= 0
          IF(FX(I)>YY(I))THEN
            IC2(I)=1
            FX(I) = YY(I)
          ENDIF
          IF(FX(I)<YY2(I))THEN
            IC2(I)=2
            FX(I) = YY2(I)
          ENDIF
        ENDDO
        DO I=LFT,LLT
          FXEP(I)=FX(I)
          DPX(I) = DX(I) - FX(I) / XK(I) 
        ENDDO
      ELSEIF(JECROU(4)>0)THEN
        DO I=LFT,LLT
          IF(IFUNC(I)/=0.AND.IECROU(I)==4)THEN
            FX(I) = FXEP(I) + XK(I)*DDX(I)
            IC2(I)= 0
            IF(FX(I)>YY(I))THEN
              IC2(I)=1
              FX(I) = YY(I)
            ENDIF
            IF(FX(I)<YY2(I))THEN
              IC2(I)=2
              FX(I) = YY2(I)
            ENDIF
          ENDIF
        ENDDO
        DO I=LFT,LLT
          IF(IFUNC(I)/=0.AND.IECROU(I)==4)THEN
            FXEP(I)=FX(I)
            DPX(I) = DX(I) - FX(I) / XK(I) 
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (TENSION COMPRESSION DECOUPLEE)
C         (D/R)ECHARGEMENT NON LINEAIRE
C         DPX = DEPLACEMENT MAXIMUM (ET NON PLASTIQUE)
C-------------------------------------
      IF(JECROU(5)==LLT)THEN
        DO I=LFT,LLT
           IF(DX(I)>DPX(I))THEN
             FX(I)=YY(I)
             DPX(I) = DX(I)
           ELSEIF(DX(I)>ZERO)THEN
             DPERM(I)=MAX(YY2(I),ZERO)
             TMP1(I) = DPERM(I)
             IF(DX(I)>DPERM(I).AND.YY3(I)/=ZERO)THEN
               FMAX(I)=YY3(I)/LSCALE(I)
               TMP2(I) = FMAX(I)
               DPERM(I)=MIN(DPERM(I),DPX(I)- FMAX(I) / XK(I))
               B1 = (DPX(I)-DPERM(I))*XK(I)/FMAX(I)
               FMIN(I)=FMAX(I)*
     .          ( (DX(I)-DPERM(I))/(DPX(I)-DPERM(I)) )**B1
               FMAX(I) = FMAX(I)*(DX(I)-DPERM(I))/(DPX(I)-DPERM(I))
               FX(I)=FXEP(I)+XK(I)*DDX(I)
               FX(I)=MAX(FX(I),FMIN(I),ZERO)
               FX(I)=MIN(FX(I),FMAX(I),YY(I))
             ELSE
               FX(I) = ZERO
             ENDIF
           ELSEIF(DX(I)<DPX2(I))THEN
             FX(I)=YY(I)
             DPX2(I) = DX(I)
           ELSEIF(DX(I)<ZERO)THEN
             DPERM(I)=MIN(YY2(I),ZERO)
             TMP1(I) = DPERM(I)
             IF(DX(I)<DPERM(I).AND.YY3(I)/=ZERO)THEN
               FMAX(I)=YY3(I)/LSCALE(I)
               TMP2(I) = FMAX(I)
               DPERM(I)=MAX(DPERM(I),DPX2(I)- FMAX(I) / XK(I))
               B1 = (DPX2(I)-DPERM(I))*XK(I)/FMAX(I)
               FMIN(I)= FMAX(I)*
     .             ( (-DX(I)+DPERM(I)) / (-DPX2(I)+DPERM(I)) )**B1
               FMAX(I) = FMAX(I)*(DX(I)-DPERM(I))/(DPX2(I)-DPERM(I))
               FX(I)=FXEP(I)+XK(I)*DDX(I)
               FX(I)=MIN(FX(I),FMIN(I),ZERO)
               FX(I)=MAX(FX(I),FMAX(I),YY(I))
             ELSE
               FX(I) = ZERO
             ENDIF
           ENDIF
           FXEP(I)=FX(I)
        ENDDO
      ELSEIF(JECROU(5)>0)THEN
        DO I=LFT,LLT
         IF(IFUNC(I)/=0.AND.IECROU(I)==5)THEN
           IF(DX(I)>DPX(I))THEN
             FX(I)=YY(I)
             DPX(I) = DX(I)
           ELSEIF(DX(I)>ZERO)THEN
             DPERM(I)=MAX(YY2(I),ZERO)
             IF(DX(I)>DPERM(I).AND.YY3(I)/=ZERO)THEN
               FMAX(I)=YY3(I)/LSCALE(I)
               DPERM(I)=MIN(DPERM(I),DPX(I)- FMAX(I) / XK(I))
C              y = a (x-x1)^b
               B1 = (DPX(I)-DPERM(I))*XK(I)/FMAX(I)
               FMIN(I) = FMAX(I) *
     .            ( (DX(I)-DPERM(I))/(DPX(I)-DPERM(I)) )**B1
               FMAX(I) = FMAX(I)*(DX(I)-DPERM(I))/(DPX(I)-DPERM(I))
               FX(I)=FXEP(I)+XK(I)*DDX(I)
               FX(I)=MAX(FX(I),FMIN(I),ZERO)
               FX(I)=MIN(FX(I),FMAX(I),YY(I))
             ELSE
               FX(I) = ZERO
             ENDIF
           ELSEIF(DX(I)<DPX2(I))THEN
             FX(I)=YY(I)
             DPX2(I) = DX(I)
           ELSEIF(DX(I)<ZERO)THEN
             DPERM(I)=YY2(I)
             DPERM(I)=MIN(DPERM(I),ZERO)
             IF(DX(I)<DPERM(I).AND.YY3(I)/=ZERO)THEN
               FMAX(I)=YY3(I)/LSCALE(I)
               DPERM(I)=MAX(DPERM(I),DPX2(I)- FMAX(I) / XK(I))
C              y = a (x-x1)^b
               B1 = (DPX2(I)-DPERM(I))*XK(I)/FMAX(I)
               FMIN(I) = FMAX(I)*
     .          ( (-DX(I)+DPERM(I))/(-DPX2(I)+DPERM(I)) )**B1
               FMAX(I) = FMAX(I)*(DX(I)-DPERM(I))/(DPX2(I)-DPERM(I))
               FX(I)=FXEP(I)+XK(I)*DDX(I)
               FX(I)=MIN(FX(I),FMIN(I),ZERO)
               FX(I)=MAX(FX(I),FMAX(I),YY(I))
             ELSE
               FX(I) = ZERO
             ENDIF
           ENDIF
           FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (ECOUISSAGE ISOTROPE)
C-------------------------------------
      IF(JECROU(6)==LLT)THEN
        CALL VINTER2(TF,JAD ,JPOS ,JLEN ,LLT,XX ,DYDX ,YY )
        DO I=LFT,LLT
           IF(FX(I)>= ZERO.AND.FX(I)>YIELD(I))THEN
               POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)    
               DPX(I)=DPX(I)+(FX(I)-YY(I))/AN3Y0(I)
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I) !H1
               YIELD(I)=FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
C---FX< O 
           ELSEIF(FX(I)< ZERO.AND.FX(I)< -YIELD(I))THEN
               POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)  
               DPX(I)=DPX(I)+(YY(I)-FX(I))/AN3Y0(I)  
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I)
               YIELD(I)=-FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
           ENDIF
           FXEP(I)=FX(I)
        ENDDO
      ELSEIF(JECROU(6)>0)THEN
        CALL VINTER2(TF,JAD ,JPOS ,JLEN ,LLT,XX ,DYDX ,YY )
        DO I=LFT,LLT
         IF(IFUNC(I)/= 0.AND.IECROU(I)== 6)THEN
           IF(FX(I)>= ZERO.AND.FX(I)>YIELD(I))THEN
               POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)    
               DPX(I)=DPX(I)+(FX(I)-YY(I))/AN3Y0(I)
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I) !H1
               YIELD(I)=FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
C---FX< O 
           ELSEIF(FX(I)< ZERO.AND.FX(I)< -YIELD(I))THEN
               POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)  
               DPX(I)=DPX(I)+(YY(I)-FX(I))/AN3Y0(I)  
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I)
               YIELD(I)=-FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
           ENDIF
           FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C
C-------------------------------------
C        ELASTO PLASTIQUE (!!)
C-------------------------------------
      IF (JECROU(7)==LLT) THEN
        DO I=LFT,LLT
          FX(I) = FXEP(I) + XK(I)*DDX(I)
          IF (DX(I)>= DXOLD(I).AND.DX(I)>=0)THEN
            IF (FX(I)>YY(I)) FX(I) = YY(I) 
          ELSEIF(DX(I)< DXOLD(I).AND.DX(I)>=0)THEN
            IF (FX(I)<YY2(I))FX(I) = YY2(I) 
          ELSEIF(DX(I)>= DXOLD(I).AND.DX(I)<0)THEN
            IF(FX(I)> YY2(I))FX(I) = YY2(I)            
          ELSEIF(DX(I)< DXOLD(I).AND.DX(I)<0)THEN
            IF(FX(I)< YY(I))FX(I) = YY(I)
          ENDIF
        ENDDO
        DO I=LFT,LLT
          FXEP(I)= FX(I)
          DPX(I) = DX(I) - FX(I) / XK(I) 
        ENDDO
      ELSEIF (JECROU(7) > 0) THEN
        DO I=LFT,LLT
          IF (IFUNC(I)/=0 .AND. IECROU(I)==7) THEN
            FX(I) = FXEP(I) + XK(I)*DDX(I)
            IF (DX(I)>= DXOLD(I) .AND. DX(I)>=0) THEN
              IF (FX(I)>YY(I)) FX(I) = YY(I)                     
            ELSEIF (DX(I)< DXOLD(I) .AND. DX(I)>= 0) THEN
              IF  (FX(I) < YY2(I)) FX(I) = YY2(I)                    
            ELSEIF (DX(I)>= DXOLD(I) .AND. DX(I)<0) THEN
              IF (FX(I)> YY2(I)) FX(I) = YY2(I)                  
            ELSEIF (DX(I)< DXOLD(I) .AND. DX(I)<0) THEN
              IF (FX(I)< YY(I))  FX(I) = YY(I)
            ENDIF
            FXEP(I) = FX(I)                      
            DPX(I)  = DX(I) - FX(I) / XK(I)      
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C     SEATBELT - ELASTO PERFECLTY PLASTIQUE (ECOUISSAGE ISOTROPE) - input by values
C-------------------------------------
      IF(JECROU(9)==LLT)THEN
        DO I=LFT,LLT
C
           IF (ABS(XK(I)*XX(I)) > FX_MAX(I)) THEN
             YY(I) =SIGN(FX_MAX(I),XX(I))
             DYDX = 0
           ELSE
             YY(I) = XK(I)*XX(I)
             DYDX = XK(I) 
           ENDIF
C
           IF(FX(I)>= ZERO.AND.FX(I)>YIELD(I))THEN
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)    
               DPX(I)=DPX(I)+(FX(I)-YY(I))/AN3Y0(I)
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I) !H1
               YIELD(I)=FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
C---FX< O 
           ELSEIF(FX(I)< ZERO.AND.FX(I)< -YIELD(I))THEN

C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)  
               DPX(I)=DPX(I)+(YY(I)-FX(I))/AN3Y0(I)  
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I)
               YIELD(I)=-FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
           ENDIF
           FXEP(I)=FX(I)
        ENDDO
      ELSEIF(JECROU(9)>0)THEN
        DO I=LFT,LLT
         IF(IECROU(I)== 9)THEN
C
           IF (ABS(XK(I)*XX(I)) > FX_MAX(I)) THEN
             YY(I) =SIGN(FX_MAX(I),XX(I))
             DYDX = 0
           ELSE
             YY(I) = XK(I)*XX(I)
             DYDX = XK(I) 
           ENDIF
C
           IF(FX(I)>= ZERO.AND.FX(I)>YIELD(I))THEN
               POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)    
               DPX(I)=DPX(I)+(FX(I)-YY(I))/AN3Y0(I)
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I) !H1
               YIELD(I)=FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
C---FX< O 
           ELSEIF(FX(I)< ZERO.AND.FX(I)< -YIELD(I))THEN
               POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)  
               DPX(I)=DPX(I)+(YY(I)-FX(I))/AN3Y0(I)  
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I)
               YIELD(I)=-FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
           ENDIF
           FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C     SEATBELT - ELASTO PLASTIQUE (ECOUISSAGE ISOTROPE) in tension - perfleclty plastic in compression
C-------------------------------------
      IF(JECROU(10)==LLT)THEN
        CALL VINTER2(TF,JAD ,JPOS ,JLEN ,LLT,XX ,DYDX ,YY )
        DO I=LFT,LLT
           IF(FX(I)> ZERO.AND.FX(I)>YIELD(I))THEN
               POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)    
               DPX(I)=DPX(I)+(FX(I)-YY(I))/AN3Y0(I)
               FX(I)=YY(I)
               YIELD(I)=FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
          ELSEIF(FX(I)<= -FX_MAX(I))THEN
               YY(I) = -FX_MAX(I)
C-- COMPUTE PLASTIC DEFORMATION (TOTAL)  
               DPX(I)=DPX(I)+(-YY(I)+FX(I))/AN3Y0(I)
               FX(I)=YY(I)
          ENDIF
          FXEP(I)=FX(I)
        ENDDO
      ELSEIF(JECROU(10)>0)THEN
        CALL VINTER2(TF,JAD ,JPOS ,JLEN ,LLT,XX ,DYDX ,YY )
        DO I=LFT,LLT
         IF(IFUNC(I)/= 0.AND.IECROU(I)== 10)THEN
           IF(FX(I)> ZERO.AND.FX(I)>YIELD(I))THEN
               POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)    
               DPX(I)=DPX(I)+(FX(I)-YY(I))/AN3Y0(I)
               FX(I)=YY(I)
               YIELD(I)=FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
          ELSEIF(FX(I)<= -FX_MAX(I))THEN
               YY(I) = -FX_MAX(I)
C-- COMPUTE PLASTIC DEFORMATION (TOTAL)  
               DPX(I)=DPX(I)+(-YY(I)+FX(I))/AN3Y0(I)
               FX(I)=YY(I)
          ENDIF
          FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C     SEATBELT - LINEAR ELASTIC in tension - perfleclty plastic in compression
C-------------------------------------
      IF(JECROU(11)==LLT)THEN
        DO I=LFT,LLT
          IF(FX(I)<= -FX_MAX(I))THEN
            YY(I) = -FX_MAX(I)
C-- COMPUTE PLASTIC DEFORMATION (TOTAL)  
            DPX(I)=DPX(I)+(-YY(I)+FX(I))/AN3Y0(I)
            FX(I)=YY(I)
          ENDIF
          FXEP(I)=FX(I)
        ENDDO
      ELSEIF(JECROU(11)>0)THEN
        DO I=LFT,LLT
          IF(IECROU(I)== 11)THEN
            IF(FX(I)<= -FX_MAX(I))THEN
              YY(I) = -FX_MAX(I)
C-- COMPUTE PLASTIC DEFORMATION (TOTAL)  
              DPX(I)=DPX(I)+(-YY(I)+FX(I))/AN3Y0(I)
              FX(I)=YY(I)
            ENDIF
            FXEP(I)=FX(I)
          ENDIF
        ENDDO
      ENDIF 
C--------------------------------------------------------------------
C     NON LINEAR DAMPING
C--------------------------------------------------------------------
      IF(IMPL_S==0.OR.IDYNA>0) THEN
        IF(JDMP>0)THEN
          DO I=LFT,LLT
           JPOS(I) = NINT(POS(4,I))
           JFUNC=MAX(IFV(I),1)
           JAD(I)  = NPF(JFUNC) / 2 + 1
           JLEN(I) = NPF(JFUNC+1) / 2 - JAD(I) - JPOS(I)
          ENDDO
C
          CALL VINTER2(TF,JAD,JPOS,JLEN,LLT,DVXS,DYDXV,GX)
C
          DO I=LFT,LLT
            POS(4,I) = JPOS(I)
          ENDDO
        ENDIF
c---------------------------G * funct_id_4
        IF(J2DMP>0)THEN
          DO I=LFT,LLT
           J2POS(I) = NINT(POS(5,I))
           J2FUNC=MAX(IFUNC3(I),1)
           J2AD(I)  = NPF(J2FUNC) / 2 + 1
           J2LEN(I) = NPF(J2FUNC+1) / 2 - J2AD(I) - J2POS(I)
          ENDDO
           CALL VINTER2(TF,J2AD,J2POS,J2LEN,LLT,DVXS,DYDXV2,GX2)

         DO I=LFT,LLT
           POS(5,I) = J2POS(I)
         ENDDO
        ENDIF
c-------------------------        
        IF(JDMP/=LLT)THEN
          DO I=LFT,LLT
           IF(IFV(I)==0) GX(I)=ZERO
          ENDDO
        ENDIF
        IF(J2DMP/=LLT)THEN 
          DO I=LFT,LLT            
           IF(IFUNC3(I)==0) GX2(I)=ZERO
          ENDDO
        ENDIF

        DO I=LFT,LLT
          DVV  = MAX(ONE,ABS(DVX(I)/D(I)))
          DFAC = AK(I) + B(I) * LOG(DVV) + EE(I)*GX(I)
          FX(I)= ( DFAC*FX(I) + XC(I)*DVX(I) + GF3(I)*GX2(I) ) *OFF(I)
          E(I) = E(I) + (DX(I)-DXOLD(I)) * (FX(I)+FOLD(I)) * HALF
        ENDDO
      ELSE
        DO I=LFT,LLT
         FX(I)= FX(I)  *AK(I)* OFF(I)
         E(I) = E(I) + (DX(I)-DXOLD(I)) * (FX(I)+FOLD(I)) * HALF
        ENDDO
      ENDIF
      DO I=LFT,LLT
        DX(I)=DX(I)*XL0(I)
        DXOLD(I)=DXOLD(I)*XL0(I)
        DPX(I)=DPX(I)*XL0(I)
        DPX2(I)=DPX2(I)*XL0(I)
        E(I)=E(I)*XL0(I)
      ENDDO
C
C----
      RETURN
      END
