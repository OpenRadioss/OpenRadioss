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
Chd|  REDEF_SEATBELT                source/tools/seatbelts/redef_seatbelt.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE REDEF_SEATBELT(FX      ,XK     ,DX     ,FXEP   ,DXOLD ,
     .                          DPX     ,TF     ,NPF    ,XC     ,OFF   ,
     .                          E       ,ANIM   ,IANI   ,POS    ,XL0   ,
     .                          DMN     ,DMX    ,LSCALE ,YIELD  ,AK    ,  
     .                          IECROU  ,IFUNC  ,IFUNC2 ,XX_OLD ,FX_MAX,
     .                          XKC     ,LLT_LOC,INDEXA ,FLAG,XK_TAN)
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
      INTEGER NPF(*),IECROU(MVSIZ),IFUNC(MVSIZ),IFUNC2(MVSIZ),IANI,
     .        INDEXA(*),LLT_LOC,FLAG
C     REAL
      my_real
     .   FX(*), XK(*), DX(*), FXEP(*), DXOLD(*), DPX(*), TF(*),
     .   OFF(*),E(*),XL0(*),LSCALE(*),YIELD(*),AK(MVSIZ),XX_OLD(*),
     .   FX_MAX(*),XKC(*),ANIM(*),DMN(*),DMX(*),POS(*),XC(*),XK_TAN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     . I, K, NP2, FUND, K1, II, JECROU(11), INTERP, JPOS(MVSIZ), JFUNC,
     . JLEN(MVSIZ),JAD(MVSIZ),J
      my_real
     . X1,X2,Y1,Y2,DT11,AN3Y0(MVSIZ),XN3FY0(MVSIZ),DXELA(MVSIZ),XI1,XI2,YI1,YI2,DDXT,DDXC,
     . XX(MVSIZ),YY(MVSIZ),GET_U_FUNC,DYDX(MVSIZ),DDX(MVSIZ),DVX(MVSIZ),FOLD(MVSIZ),
     . DAMP,DAMM
C=======================================================================
C     Derived from REDEF3
C     Can be launched on a reduced number of elements (defined by INDEXA)
C     --> FLAG = 1 : loop on all elements I = J
C     --> FLAG = 2 : loop only on 2nd strand of elements in slipring - I /= J 
C        (ALL LOCAL ARRAYS USE J)
C-----------------------------------------------------------------------
C
      DT11 = DT1
      IF(DT11.EQ.ZERO)DT11 = EP30
C
      DO J=LFT,LLT_LOC
        I = INDEXA(J)
        DX(I)=DX(I)/XL0(I)
        DXOLD(I)=DXOLD(I)/XL0(I)
        DPX(I)=DPX(I)/XL0(I)
        E(I)=E(I)/XL0(I)
C
        FOLD(J)=FX(I)
        DDX(J)= (DX(I)-DXOLD(I))
        DVX(J)= DDX(J)/ DT11
      ENDDO
C
      IF ((IANI.NE.0).AND.(FLAG==1))THEN
        DO I=LFT,LLT_LOC
          II=I+NFT
          DAMP=DX(I)/MAX(DMX(I),EM15)
          DAMM=DX(I)/MIN(DMN(I),-EM15)
          ANIM(II)=MAX(ANIM(II),DAMP,DAMM)
          ANIM(II)=MIN(ANIM(II),ONE)
        ENDDO
      ENDIF
C
C-------------------------------------
C        VECTOR INTERPOLATION (ADRESS)
C-------------------------------------
C
      JECROU(1:11) = 0
      INTERP = 0
C
      DO J=LFT,LLT_LOC
       I = INDEXA(J)
       IF(IECROU(I) == 11)THEN
         JECROU(11) = JECROU(11) + 1
       ELSEIF(IECROU(I) == 10)THEN
         JECROU(10) = JECROU(10) + 1
         INTERP = 1
       ENDIF
      ENDDO
C
      IF(INTERP.GT.0)THEN
        DO J=LFT,LLT_LOC
          I = INDEXA(J)
          JPOS(J)  = NINT(POS(I))          
          JFUNC =MAX(1,IFUNC(I))
          JAD(J)   = NPF(JFUNC) / 2  + 1
          JLEN(J)  = NPF(JFUNC+1) / 2  - JAD(J)  - JPOS(J)
          XX(J) =ZERO
        ENDDO
      ENDIF            
C
C-------------------------------------
C        ELASTO PLASTIQUE (ECOUISSAGE ISOTROPE) - PERFECLTY PLASTIC IN COMPRESSION
C-------------------------------------
      IF(JECROU(10)==LLT_LOC)THEN
        DO J=LFT,LLT_LOC
          I = INDEXA(J)                                          
          FUND = IFUNC2(I)     ! courbe N3 de unload                                                             
          NP2  = (NPF(FUND+1)-NPF(FUND))*HALF                       
          AN3Y0(J)= ZERO
          DXELA(J)=DX(I)-DPX(I)
          IF (((DXELA(J) >= ZERO).OR.(FXEP(I) >= ZERO)).AND.(FUND > 0)) THEN
C--- Tension - load curve is used
            DO  K=2,NP2                                                       
              K1=2*(K-2)                                                
              X1=TF(NPF(FUND)+K1)                                       
              X2=TF(NPF(FUND)+K1+2)                                     
              Y1=TF(NPF(FUND)+K1+1)                                     
              Y2=TF(NPF(FUND)+K1+3) 
              IF((FXEP(I)< Y2.AND.FXEP(I)>=Y1))THEN  
                AN3Y0(J)=(Y2-Y1)/ (X2-X1)                     
                XN3FY0(I)=(FXEP(I)-Y1)/AN3Y0(J) + X1   !ABS DE N3  
                EXIT     
              ENDIF 
            ENDDO
C
            IF (AN3Y0(J)== ZERO)THEN ! extrapolation (exterieur aux points de l input)
              X1=TF(NPF(FUND)+(NP2-2)*2)                                       
              X2=TF(NPF(FUND)+(NP2-2)*2+2)                                     
              Y1=TF(NPF(FUND)+(NP2-2)*2+1)                                     
              Y2=TF(NPF(FUND)+(NP2-2)*2+3) 
C
              XI1=TF(NPF(FUND))                                       
              XI2=TF(NPF(FUND)+2)                                     
              YI1=TF(NPF(FUND)+1)                                     
              YI2=TF(NPF(FUND)+3) 
              IF(FXEP(I)>Y2)AN3Y0(J)=(Y2-Y1)/ (X2-X1) 
              IF(FXEP(I)<YI1)AN3Y0(J)=(YI2-YI1)/ (XI2-XI1)
            ENDIF
C----       Crossing of compresion/tension line - mix stiffness computed
            IF ((DXELA(J) < ZERO).AND.(ABS(DDX(J)) > ZERO)) THEN
              DDXT = -FXEP(I)/AN3Y0(J)
              DDXC = DDX(J) - DDXT
              AN3Y0(J) = (DDXT/DDX(J))*AN3Y0(J) + (DDXC/DDX(J))*XKC(I)
            ENDIF
C
            IF (DXELA(J) >= ZERO) XX(J)=XX_OLD(I)+DDX(J)
          ELSE
C--- Compression - perfectly plastic behavior
            AN3Y0(J)= XKC(I)
          ENDIF
          FX(I)=FXEP(I)+AN3Y0(J)*DDX(J)
        ENDDO
      ELSEIF(JECROU(10)>0)THEN
        DO J=LFT,LLT_LOC
         I = INDEXA(J)
         IF(IFUNC(I).NE.0.AND.IECROU(I)== 10)THEN
          FUND = IFUNC2(I)     ! courbe N3 de unload                                                             
          NP2  = (NPF(FUND+1)-NPF(FUND))*HALF                       
          AN3Y0(J)= ZERO
          DXELA(J)=DX(I)-DPX(I)
          IF (((DXELA(J) >= ZERO).OR.(FXEP(I) >= ZERO)).AND.(FUND > 0)) THEN
C--- Tension - load curve is used
            DO  K=2,NP2                                                       
              K1=2*(K-2)                                                
              X1=TF(NPF(FUND)+K1)                                       
              X2=TF(NPF(FUND)+K1+2)                                     
              Y1=TF(NPF(FUND)+K1+1)                                     
              Y2=TF(NPF(FUND)+K1+3) 
              IF((FXEP(I)< Y2.AND.FXEP(I)>=Y1))THEN  
                AN3Y0(J)=(Y2-Y1)/ (X2-X1)                     
                XN3FY0(I)=(FXEP(I)-Y1)/AN3Y0(J) + X1   !ABS DE N3  
                EXIT     
              ENDIF 
            ENDDO
C
            IF (AN3Y0(J)== ZERO)THEN ! extrapolation (exterieur aux points de l input)
              X1=TF(NPF(FUND)+(NP2-2)*2)                                       
              X2=TF(NPF(FUND)+(NP2-2)*2+2)                                     
              Y1=TF(NPF(FUND)+(NP2-2)*2+1)                                     
              Y2=TF(NPF(FUND)+(NP2-2)*2+3) 
C
              XI1=TF(NPF(FUND))                                       
              XI2=TF(NPF(FUND)+2)                                     
              YI1=TF(NPF(FUND)+1)                                     
              YI2=TF(NPF(FUND)+3) 
              IF(FXEP(I)>Y2)AN3Y0(J)=(Y2-Y1)/ (X2-X1) 
              IF(FXEP(I)<YI1)AN3Y0(J)=(YI2-YI1)/ (XI2-XI1)
            ENDIF
C----       Crossing of compresion/tension line - mix stiffness computed
            IF ((DXELA(J) < ZERO).AND.(ABS(DDX(J)) > ZERO)) THEN
              DDXT = -FXEP(I)/AN3Y0(J)
              DDXC = DDX(J) - DDXT
              AN3Y0(J) = (DDXT/DDX(J))*AN3Y0(J) + (DDXC/DDX(J))*XKC(I)
            ENDIF
C
            IF (DXELA(J) >= ZERO) XX(J)=XX_OLD(I)+DDX(J)
          ELSE
C--- Compression - perfectly plastic behavior
            AN3Y0(J)= XKC(I)
          ENDIF
          FX(I)=FXEP(I)+AN3Y0(J)*DDX(J)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        LINEAR ELASTIC in tension - perfleclty palstic in compression (same as 10 without curve)
C-------------------------------------
      IF(JECROU(11)==LLT_LOC)THEN
        DO J=LFT,LLT_LOC  
          I = INDEXA(J)                                                            
          AN3Y0(J)= ZERO
          DXELA(J)=DX(I)-DPX(I)
          AN3Y0(J)= XK(I)
          IF ((DXELA(J) >= ZERO).OR.(FXEP(I) >= ZERO)) THEN
C----       Crossing of compresion/tension line - mix stiffness computed
            IF ((DXELA(J) < ZERO).AND.(ABS(DDX(J)) > ZERO)) THEN
              DDXT = -FXEP(I)/AN3Y0(J)
              DDXC = DDX(J) - DDXT
              AN3Y0(J) = (DDXT/DDX(J))*AN3Y0(J) + (DDXC/DDX(J))*XKC(I)
            ENDIF
          ELSE
            AN3Y0(J)= XKC(I)
          ENDIF                                                          
          FX(I)=FXEP(I)+AN3Y0(J)*DDX(J)
        ENDDO
      ELSEIF(JECROU(11)>0)THEN
        DO J=LFT,LLT_LOC
          I = INDEXA(J)
          IF(IECROU(I)== 11) THEN                      
            AN3Y0(J)= ZERO
            DXELA(J)=DX(I)-DPX(I)
            IF ((DXELA(J) >= ZERO).OR.(FXEP(I) >= ZERO)) THEN
              AN3Y0(J)= XK(I)
C----       Crossing of compresion/tension line - mix stiffness computed
              IF ((DXELA(J) < ZERO).AND.(ABS(DDX(J)) > ZERO)) THEN
                DDXT = -FXEP(I)/AN3Y0(J)
                DDXC = DDX(J) - DDXT
                AN3Y0(J) = (DDXT/DDX(J))*AN3Y0(J) + (DDXC/DDX(J))*XKC(I)
              ENDIF
            ELSE
              AN3Y0(J)= XKC(I)
            ENDIF                                                          
            FX(I)=FXEP(I)+AN3Y0(J)*DDX(J)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
c     VECTOR INTERPOLATION
C-------------------------------------
      DO J=LFT,LLT_LOC
        I = INDEXA(J)
        XX(J)  = XX(J) *LSCALE(I)        
      ENDDO                          
C-------------------------------------
C     SEATBELT - ELASTO PLASTIQUE (ECOUISSAGE ISOTROPE) in tension - perfleclty plastic in compression
C-------------------------------------
      IF(JECROU(10).EQ.LLT_LOC)THEN
        CALL VINTER2(TF,JAD ,JPOS ,JLEN ,LLT_LOC,XX ,DYDX ,YY )
        DO J=LFT,LLT_LOC
           I = INDEXA(J)
           XK_TAN(I) = DYDX(J)
           IF(FX(I)> ZERO.AND.FX(I)>YIELD(I))THEN
               POS(I) = JPOS(J)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)    
               DPX(I)=DPX(I)+(FX(I)-YY(J))/AN3Y0(J)
               FX(I)=YY(J)
               YIELD(I)=FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(J))
          ELSEIF(FX(I)<= -FX_MAX(I))THEN
               YY(J) = -FX_MAX(I)
C-- COMPUTE PLASTIC DEFORMATION (TOTAL)  
               DPX(I)=DPX(I)+(-YY(J)+FX(I))/AN3Y0(J)
               FX(I)=YY(J)
          ENDIF
          FXEP(I)=FX(I)
        ENDDO
      ELSEIF(JECROU(10).GT.0)THEN
        CALL VINTER2(TF,JAD ,JPOS ,JLEN ,LLT_LOC,XX ,DYDX ,YY )
        DO J=LFT,LLT_LOC
         I = INDEXA(J)
         IF(IFUNC(I)/= 0.AND.IECROU(I)== 10)THEN
           XK_TAN(I) = DYDX(J)
           IF(FX(I)> ZERO.AND.FX(I)>YIELD(I))THEN
               POS(I) = JPOS(J)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)    
               DPX(I)=DPX(I)+(FX(I)-YY(J))/AN3Y0(J)
               FX(I)=YY(J)
               YIELD(I)=FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(J))
          ELSEIF(FX(I)<= -FX_MAX(I))THEN
               YY(J) = -FX_MAX(I)
C-- COMPUTE PLASTIC DEFORMATION (TOTAL)  
               DPX(I)=DPX(I)+(-YY(J)+FX(I))/AN3Y0(J)
               FX(I)=YY(J)
          ENDIF
          FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C     SEATBELT - LINEAR ELASTIC in tension - perfleclty plastic in compression
C-------------------------------------
      IF(JECROU(11).EQ.LLT_LOC)THEN
        DO J=LFT,LLT_LOC
          I = INDEXA(J)
           XK_TAN(I) = DYDX(J)
          IF(FX(I)<= -FX_MAX(I))THEN
            YY(J) = -FX_MAX(I)
C-- COMPUTE PLASTIC DEFORMATION (TOTAL)  
            DPX(I)=DPX(I)+(-YY(J)+FX(I))/AN3Y0(J)
            FX(I)=YY(J)
          ENDIF
          FXEP(I)=FX(I)
        ENDDO
      ELSEIF(JECROU(11).GT.0)THEN
        DO J=LFT,LLT_LOC
          I = INDEXA(J)
          IF(IECROU(I)== 11)THEN
            XK_TAN(I) = DYDX(J)
            IF(FX(I)<= -FX_MAX(I))THEN
              YY(J) = -FX_MAX(I)
C-- COMPUTE PLASTIC DEFORMATION (TOTAL)  
              DPX(I)=DPX(I)+(-YY(J)+FX(I))/AN3Y0(J)
              FX(I)=YY(J)
            ENDIF
            FXEP(I)=FX(I)
          ENDIF
        ENDDO
      ENDIF
C--------------------------------------------------------------------
C     NON LINEAR DAMPING
C--------------------------------------------------------------------
      IF(IMPL_S.EQ.0.OR.IDYNA.GT.0) THEN
        DO J=LFT,LLT_LOC
          I = INDEXA(J)
          FX(I)= (AK(I)*FX(I) + XC(I)*DVX(J)) *OFF(I)
          E(I) = E(I) + (DX(I)-DXOLD(I)) * (FX(I)+FOLD(J)) * HALF
        ENDDO
      ELSE
        DO J=LFT,LLT_LOC
          I = INDEXA(J)
          FX(I)= FX(I)  *AK(I)* OFF(I)
          E(I) = E(I) + (DX(I)-DXOLD(I)) * (FX(I)+FOLD(J)) * HALF
        ENDDO
      ENDIF
c-------------------------  
      DO J=LFT,LLT_LOC
        I = INDEXA(J)
        DX(I)=DX(I)*XL0(I)
        DXOLD(I)=DXOLD(I)*XL0(I)
        DPX(I)=DPX(I)*XL0(I)
        E(I)=E(I)*XL0(I)
      ENDDO
C
C----
      RETURN
      END
