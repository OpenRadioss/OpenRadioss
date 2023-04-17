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
C----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
Chd|====================================================================
Chd|  REDEF3_LAW113                      source/elements/spring/redef3_law113.F
Chd|-- called by -----------        
Chd|        R23L113DEF3                   source/elements/spring/r23l113def3.F
Chd|-- calls ---------------
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        VINTER2DP                     source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE REDEF3_LAW113(NEL ,NFT,
     .                  FX      ,XK     ,DX     ,FXEP       ,DXOLD ,
     .                  DPX     ,TF     ,NPF    ,XC         ,OFF   ,
     .                  E       ,DPX2   ,ANIM   ,IANI       ,POS   ,
     .                  XL0     ,DMN    ,DMX    ,DVX        ,
     .                  FF      ,LSCALE ,EE     ,GF3        ,IFUNC3,
     .                  YIELD    ,AK     ,B     ,D          ,
     .                  IECROU  ,IFUNC  ,IFV    ,IFUNC2     ,IFUNC4 ,
     .                  EPLA    ,XX_OLD   )
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
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t 
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPF(SNPC),IANI,NEL, NFT
      INTEGER, DIMENSION(MVSIZ),INTENT(IN) :: IECROU,IFUNC,IFV,
     .                                        IFUNC2,IFUNC3,IFUNC4
C     REAL
      my_real, DIMENSION(MVSIZ), INTENT(IN):: AK,B,XL0,XK,FF,LSCALE,DMN,
     .                                        DMX,OFF,XC,EE,D
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: EPLA,DXOLD
      my_real, DIMENSION(MVSIZ), INTENT(OUT)   ::   DVX
      my_real, DIMENSION(NEL)  , INTENT(INOUT)   ::  FX,DX,DPX,FXEP,
     .                                               E,DPX2, YIELD,XX_OLD
      my_real, INTENT(INOUT) :: POS(6,NEL), ANIM(NUMELR*IANI)
      my_real, INTENT(IN) :: TF(STF)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(MVSIZ) :: JPOS , JLEN,JAD,JPOS2, 
     .                             JLEN2,JPOS3, JLEN3,JAD2,
     .                             IC1, JLEN4,IC2,IC0,
     .                             J2POS,J2LEN,
     .                             J2AD,JPOS4, J3POS, J3LEN,J3AD
     
       INTEGER 
     . JFUNC,JFUNC2,JDMP,JECROU(-1:11),J2DMP,K1,NP1,NP2,
     . I, J, II, INTERP, K, FUNC,FUND,J1,J2FUNC,J3FUNC,J2K
c     REAL ou REAL*8
      my_real 
     .     FINTER 
      EXTERNAL FINTER
      my_real, DIMENSION(MVSIZ) ::   DDX,FOLD, GX, DXELA, DYD,
     .                               XX, XX2, XX3, YY, YY2, YY3,DYDX,
     .                               DYDX2, DYDX3, DYDXV, DPERM,FMAX,
     .                               DVXS,GF3,DYDXV2,FMIN,GX2,FY0,
     .                               XFY0,YCUT,XCUT,XN3FY0,
     .                               UL,FYIELD,DPL,YY22,XFYN1,AN3Y0,DDPX,
     .                               XFYN33,XFYN11,DPX3,DPX22,XX22,
     .                               EDECAL2,EDECAL3,ECROU,KX2,DKDX2,
     .                               TMP1, TMP2
      my_real 
     . DVV, DFAC, DT11, DAMP, DAMM,B1,XI1,XI2,YI1,YI2 ,
     . S1,S2,T1C,T2C,X1,X2,Y1,Y2
C=======================================================================
      DT11 = DT1
      IF(DT11==ZERO)DT11 = EP30
      DO I=1,NEL
        DX(I)=DX(I)/XL0(I)
        DXOLD(I)=DXOLD(I)/XL0(I)
        DPX(I)=DPX(I)/XL0(I)
        DPX2(I)=DPX2(I)/XL0(I)
        E(I)=E(I)/XL0(I)
      ENDDO
C
      DO 80  I=1,NEL
      FOLD(I)=FX(I)
      DDX(I)= (DX(I)-DXOLD(I))
      DVX(I)= DDX(I)/ DT11
      DVXS(I)= DVX(I)*FF(I)
   80 CONTINUE
C
C
      IF(IANI/=0)THEN
        DO I=1,NEL
          II=I + NFT
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
      J2K = 0
C
      DO I=1,NEL
       IF(IFUNC(I)==0)THEN  ! ifunc =IGEO(101)-FCT_id1
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
       ENDIF
       IF(IFV(I)/=0) JDMP = JDMP + 1
       IF(IFUNC3(I)/=0) J2DMP = J2DMP + 1
       IF(IFUNC4(I)/=0) J2K   = J2K + 1
      ENDDO
C
      IF(INTERP > 0)THEN
        DO I=1,NEL
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
C        NON LINEAR ELASTIC
C        NL ELASTO PLASTIC (UNCOUPLED TENSION COMPRESSION )
C-------------------------------------
      IF(JECROU(0)+JECROU(2)== NEL)THEN
        DO I=1,NEL
          XX(I)=DX(I)
        ENDDO
      ELSEIF(JECROU(0)+JECROU(2)>0)THEN
        DO I=1,NEL
          IF(IFUNC(I)/=0.AND.(IECROU(I)==0.OR.IECROU(I)==2))THEN
            XX(I)=DX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO-PLASTIC (ISOTROP)
C        ELASTO PLASTIC (6DOF COUPLED TENSION COMPRESSION )
C-------------------------------------
      IF(JECROU(1)+JECROU(3)== NEL )THEN
        DO I=1,NEL
           FX(I)=FXEP(I)+XK(I)*DDX(I)
           IF(FX(I)>=0.)THEN
             XX(I)=DPX(I)+FX(I)/XK(I)
           ELSE
             XX(I)=-DPX(I)+FX(I)/XK(I)
           ENDIF
        ENDDO
      ELSEIF(JECROU(1)+JECROU(3)>0)THEN
        DO I=1,NEL
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
C        ELASTO PLASTIC (KINEMATIC HARDENING )
C-------------------------------------
      IF(JECROU(4)== NEL )THEN
        DO I=1,NEL
           INTERP = MAX(2,INTERP)
           XX(I) =DX(I)
           XX2(I)=DX(I)
        ENDDO
      ELSEIF(JECROU(4)>0)THEN
        DO I=1,NEL
         IF(IFUNC(I)/=0.AND.IECROU(I)==4)THEN
           INTERP = MAX(2,INTERP)
           XX(I) =DX(I)
           XX2(I)=DX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIC (UNCOUPLED TENSION COMPRESSION )
C         (D/R) NON LINEAR RELOADING 
C         DPX = MAXIMAL DISPLACEMENT 
C-------------------------------------
      IF(JECROU(5)== NEL )THEN
        DO I=1,NEL
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
        DO I=1,NEL
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
C       ELASTO PLASTIC (ISTROPIC HARDENING  )
C-------------------------------------
      IF(JECROU(6)== NEL)THEN
        DO I=1,NEL                                          
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
        DO I=1,NEL
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
      IF(JECROU(7)== NEL)THEN
        DO I=1,NEL
           INTERP = MAX(2,INTERP)
           XX(I) =DX(I)
           XX2(I)=DX(I)
        ENDDO
      ELSEIF(JECROU(7)>0)THEN
        DO I=1,NEL
         IF(IFUNC(I)/=0.AND.IECROU(I)==7)THEN
           INTERP = MAX(2,INTERP)
           XX(I) =DX(I)
           XX2(I)=DX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
c     VECTOR INTERPOLATION
C-------------------------------------
      DO I=1,NEL
        XX(I)  = XX(I) *LSCALE(I)                   
        XX2(I) = XX2(I)*LSCALE(I)                   
        XX3(I) = XX3(I)*LSCALE(I)                   
      ENDDO                          
C----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (IRESP==1) THEN
        IF(INTERP>=1)
     .      CALL VINTER2DP(TF,JAD ,JPOS ,JLEN ,NEL,XX ,DYDX ,YY)
        IF(INTERP>=2)
     .      CALL VINTER2DP(TF,JAD2,JPOS2,JLEN2,NEL,XX2,DYDX2,YY2)
        IF(INTERP>=3)
     .      CALL VINTER2DP(TF,JAD ,JPOS3,JLEN3,NEL,XX3,DYDX3,YY3)
      ELSE
        IF(INTERP>=1)
     .      CALL VINTER2(TF,JAD ,JPOS ,JLEN ,NEL,XX ,DYDX ,YY )
        IF(INTERP>=2)
     .      CALL VINTER2(TF,JAD2,JPOS2,JLEN2,NEL,XX2,DYDX2,YY2)
        IF(INTERP>=3)
     .      CALL VINTER2(TF,JAD ,JPOS3,JLEN3,NEL,XX3,DYDX3,YY3)
      ENDIF
      IF(INTERP>0)THEN
        DO I=1,NEL
          POS(1,I) = JPOS(I)
          POS(2,I) = JPOS2(I)
          POS(3,I) = JPOS3(I)
        ENDDO
      ENDIF
C-------------------------------------
C       LINEAR ELASTIC 
C-------------------------------------
      IF(JECROU(-1) == NEL)THEN
        DO I=1,NEL
           FX(I)=XK(I)*DX(I)
        ENDDO
      ELSEIF(JECROU(-1)>0)THEN
        DO I=1,NEL
          IF(IFUNC(I)==0)THEN
            FX(I)=XK(I)*DX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTIC F = f(total length)
C-------------------------------------
      IF (JECROU(8) == NEL )THEN
        DO I=1,NEL
          FX(I)=YY(I)
        ENDDO
      ELSEIF (JECROU(8) > 0)THEN
        DO I=1,NEL
          IF (IECROU(I) == 8) FX(I)=YY(I)
        ENDDO
      ENDIF
C-------------------------------------
C        NON LINEAR ELASTIC
C-------------------------------------
      IF(JECROU(0)== NEL)THEN
        DO I=1,NEL
           FX(I)=YY(I)
        ENDDO
      ELSEIF(JECROU(0)>0)THEN
        DO I=1,NEL
           IF(IFUNC(I)/=0.AND.IECROU(I)==0) FX(I)=YY(I)
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIC (ISOTROP)
C-------------------------------------
      IF(JECROU(1)== NEL )THEN
        DO I=1,NEL
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
        DO I=1,NEL
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
C       ELASTO PLASTIC (UNCOUPLED TENSION COMPRESSION )
C-------------------------------------
      IF(JECROU(2)== NEL )THEN
        DO I=1,NEL
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
        DO I=1,NEL
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
C        ELASTO PLASTIC (6DOF COUPLED TENSION COMPRESSION )
C-------------------------------------
      IF(JECROU(3)== NEL )THEN
        DO I=1,NEL
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
        DO I=1,NEL
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
C       ELASTO-PLASTIC  (KINEMATIC HARDENING)
C-------------------------------------
      IF(JECROU(4)== NEL )THEN
        DO I=1,NEL
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
        DO I=1,NEL
          FXEP(I)=FX(I)
          DPX(I) = DX(I) - FX(I) / XK(I) 
        ENDDO
      ELSEIF(JECROU(4)>0)THEN
        DO I=1,NEL
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
        DO I=1,NEL
          IF(IFUNC(I)/=0.AND.IECROU(I)==4)THEN
            FXEP(I)=FX(I)
            DPX(I) = DX(I) - FX(I) / XK(I) 
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO-PLASTIC (TENSION COMPRESSION UNCOUPLED)
C         NONLINEAR LOADING/UNLOADING
C         DPX =  MAXIMAL DISPLACEMENT (ELASTIC)
C-------------------------------------
      IF(JECROU(5)== NEL )THEN
        DO I=1,NEL
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
        DO I=1,NEL
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
C        ELASTO PLASTIC (ISOTROPIC HARDENING )
C-------------------------------------
      IF(JECROU(6)== NEL )THEN
        CALL VINTER2(TF,JAD ,JPOS ,JLEN ,NEL ,XX ,DYDX ,YY )
        DO I=1,NEL
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
        CALL VINTER2(TF,JAD ,JPOS ,JLEN ,NEL ,XX ,DYDX ,YY )
        DO I=1,NEL
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
C       ELSTO-PLASTIC
C-------------------------------------
      IF (JECROU(7)== NEL ) THEN
        DO I=1,NEL
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
        DO I=1,NEL
          FXEP(I)= FX(I)
          DPX(I) = DX(I) - FX(I) / XK(I) 
        ENDDO
      ELSEIF (JECROU(7) > 0) THEN
        DO I=1,NEL
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
C--------------------------------------------------------------------
C     NON LINEAR DAMPING
C--------------------------------------------------------------------
      IF(IMPL_S==0.OR.IDYNA>0) THEN
        IF(JDMP>0)THEN
          DO I=1,NEL
           JPOS(I) = NINT(POS(4,I))
           JFUNC=MAX(IFV(I),1)
           JAD(I)  = NPF(JFUNC) / 2 + 1
           JLEN(I) = NPF(JFUNC+1) / 2 - JAD(I) - JPOS(I)
          ENDDO
C
          CALL VINTER2(TF,JAD,JPOS,JLEN,NEL ,DVXS,DYDXV,GX)
C
          DO I=1,NEL
            POS(4,I) = JPOS(I)
          ENDDO
        ENDIF
c---------------------------G * funct_id_4
        IF(J2DMP>0)THEN
          DO I=1,NEL
           J2POS(I) = NINT(POS(5,I))
           J2FUNC=MAX(IFUNC3(I),1)
           J2AD(I)  = NPF(J2FUNC) / 2 + 1
           J2LEN(I) = NPF(J2FUNC+1) / 2 - J2AD(I) - J2POS(I)
          ENDDO
           CALL VINTER2(TF,J2AD,J2POS,J2LEN,NEL,DVXS,DYDXV2,GX2)

         DO I=1,NEL
           POS(5,I) = J2POS(I)
         ENDDO
        ENDIF
c---------------------------K * funct_id_5
        IF(J2K > 0)THEN
          DO I=1,NEL
           J3POS(I) = NINT(POS(6,I))
           J3FUNC   = MAX(IFUNC4(I),1)
           J3AD(I)  = NPF(J3FUNC) / 2 + 1
           J3LEN(I) = NPF(J3FUNC+1) / 2 - J3AD(I) - J3POS(I)
          ENDDO
           CALL VINTER2(TF,J3AD,J3POS,J3LEN,NEL,XX,DKDX2,KX2)

         DO I=1,NEL
           POS(6,I) = J3POS(I)
         ENDDO
        ENDIF
c------------------------- 
        IF(J2K /= NEL)THEN
          DO I=1,NEL            
           IF(IFUNC4(I) == 0) KX2(I) = ONE
          ENDDO
        ENDIF       
        IF(JDMP/= NEL)THEN
          DO I=1,NEL
           IF(IFV(I)==0) GX(I)=ZERO
          ENDDO
        ENDIF
        IF(J2DMP/= NEL)THEN 
          DO I=1,NEL            
           IF(IFUNC3(I)==0) GX2(I)=ZERO
           GX2(I)=GX2(I)*KX2(I)
          ENDDO
        ELSE
          DO I=1,NEL   
           GX2(I)=GX2(I)*KX2(I)
          ENDDO 
        ENDIF
      
        DO I=1,NEL
           
          DVV  = MAX(ONE,ABS(DVX(I)/D(I)))
          DFAC = AK(I) + B(I) * LOG(DVV) + EE(I)*GX(I)
          FX(I)= ( DFAC*FX(I) + XC(I)*DVX(I) + GF3(I)*GX2(I) ) *OFF(I)
          E(I) = E(I) + (DX(I)-DXOLD(I)) * (FX(I)+FOLD(I)) * HALF
        ENDDO
      ELSE
        DO I=1,NEL
         FX(I)= FX(I)  *AK(I)* OFF(I)
         E(I) = E(I) + (DX(I)-DXOLD(I)) * (FX(I)+FOLD(I)) * HALF
        ENDDO
      ENDIF
      DO I=1,NEL
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
