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
C PRF : parallel rheological framework
Chd|====================================================================
Chd|  SIGEPS100                     source/materials/mat/mat100/sigeps100.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        CALCMATB                      source/materials/mat/mat100/calcmatb.F
Chd|        KMATINV3                      source/materials/tools/kmatinv.F
Chd|        NEO_HOOK_T                    source/materials/mat/mat100/neo_hook_t.F
Chd|        POLYSTRESS2                   source/materials/mat/mat100/sigpoly.F
Chd|        PRODAAT                       source/materials/tools/prodAAT.F
Chd|        PRODMAT                       source/materials/tools/prodmat.F
Chd|        ROTTOGLOB                     source/materials/mat/mat095/sigeps95.F
Chd|        ROTTOLOC                      source/materials/mat/mat095/sigeps95.F
Chd|        SIGABOYCE                     source/materials/mat/mat100/sigaboyce.F
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        VISCBB                        source/materials/mat/mat100/viscbb.F
Chd|        VISCPOWER                     source/materials/mat/mat100/viscpower.F
Chd|        VISCSINH                      source/materials/mat/mat100/viscsinh.F
Chd|====================================================================
       SUBROUTINE SIGEPS100(
     1      NEL    , NUPARAM, NUVAR   , NFUNC  , IFUNC , 
     2      NPF    ,TF      , TIME    , TIMESTEP, UPARAM, 
     3      RHO0  , RHO     ,VOLUME   , EINT   , NGL     ,
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      MFXX   ,MFXY    ,MFXZ,MFYX, MFYY  , MFYZ  ,  
     B      MFZX   ,MFZY    ,MFZZ     , TEMPEL,                 
     C      SOUNDSP, VISCMAX, UVAR    , OFF   , ISMSTR, ET  ,
     D      IHET   ,OFFG    , EPSTH   , IEXPAN    ,
     E      UPARAMF,UVARF   ,NVARF    ,JCVT   ,GAMA_R)
C
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "impl1_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL,  JCVT, NVARF,   NUPARAM, NUVAR,ISMSTR,NGL(*),IHET,IEXPAN
      my_real
     .      TIME       , TIMESTEP   , UPARAM(NUPARAM),UPARAMF(NUPARAM),
     .      RHO   (NEL), RHO0  (NEL), VOLUME(NEL), EINT(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),OFFG(NEL),
     .      MFXX(NEL)  ,MFXY(NEL)  ,MFXZ(NEL)  ,MFYX(NEL) ,
     .      MFYY(NEL)   ,MFYZ(NEL)   ,MFZX(NEL)  ,MFZY(NEL)  ,MFZZ(NEL), 
     .      EPSTH(NEL) ,TEMPEL(NEL), GAMA_R(NEL,6)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), SIGVZZ(NEL),
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SOUNDSP(NEL), VISCMAX(NEL), ET(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      UVAR(NEL,NUVAR), OFF(NEL) ,UVARF(NEL,NVARF)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I,J,KK,LL,N,FLAGBB,DIRECT,ITER,NITER,TAB,TABN,SHIFT,NINDX 
      INTEGER  N_NETWORK, FLAG_HE, FLAG_MUL, FLAG_T,NHYPER,NPLAS,IFORM,
     .        FLAG_PL,NET,EXPPL, NVISC(10),
     .        FLAG_VISC(10),
     .   IPOS1(NEL),ILEN1(NEL),IAD1(NEL),
     .   IPOS2(NEL),ILEN2(NEL),IAD2(NEL)
 
      my_real
     .   ET1,ET2,ET3,G,RBULK,AA,BB,CC,SB, FACTOR,
     .   MAXL,STIFF0,DSIG,DEPS,COEF1,COEF2,COEF3,COEF4,COEF5,COEF6,
     .   C10,C01,C20,C11,C02,C30,C21,C12,C03,D1,D2,D3,TAUY0,FF, EPSHAT,
     .   TEMP1,FACPL,HH,R3R3,
C
     .   BI1(NEL),BI2(NEL),JDET(NEL),I1(NEL),IP1(NEL),GAMMAOLD(NEL),STIFF(NEL), 
     .   TA1(NEL), TA2(NEL),TA3(NEL),T1(NEL), T2(NEL),T3(NEL),LPCHAIN(NEL), 
     .   TB1(NEL), TB2(NEL),TB3(NEL),TRACE(NEL),TRACEB(NEL),ETA(NEL),ETB(NEL),
     .   SB1(NEL), SB2(NEL),SB3(NEL),SB4(NEL), SB5(NEL),SB6(NEL),WW(NEL),
     .   SA1(NEL), SA2(NEL),SA3(NEL),SA4(NEL), SA5(NEL),SA6(NEL),TANORM(NEL),
     .   TBNORM(NEL),DGAMMA(NEL),PLA(NEL),DYDX(NEL),YLD(NEL), TAUY(NEL),
     .   TRACEA(NEL),PLAP(NEL),DPLA(NEL),MUNH(NEL),DNH(NEL),DYDX1(NEL),DYDX2(NEL),
     .   R1X(NEL),R1Y(NEL),R1Z(NEL),R2X(NEL),R2Y(NEL),R2Z(NEL),R3X(NEL),R3Y(NEL),R3Z(NEL),
C
     .   NB(NEL,3),SNINV(NEL,3,3),
     .   F(NEL,3,3),FT(NEL,3,3),FE(NEL,3,3),FET(NEL,3,3),FP(NEL,3,3),
     .   FFT(NEL,3,3),INVFPO(NEL,3,3),MATB(NEL,3,3),FPO(NEL,3,3),
     .   SIG(NEL,3,3),SIGB(NEL,3,3),SIGA(NEL,3,3),SN(NEL,3,3), 
     .   FTH(NEL,3,3), FTOT(NEL,3,3),FMEC(NEL,3,3),INVFTH(NEL,3,3),
     .   FFTN(NEL,3,3),FPEQ(NEL,3,3),FPEQO(NEL,3,3),S(NEL,3,3),FEDP(NEL,3,3),
     .   DFP(NEL,3,3),LB(NEL,3,3),DFP2(NEL,3,3),FPDOT(NEL,3,3),INVFE(NEL,3,3)
      my_real
     .   A1(10),EXPC(10),EXPM(10),KSI(10),A10(10),STIFFN(10),
     .   B0(10),EXPN(10),TAUREF(10) 
      my_real
     .  C1,C2,C3,C4,C5,MU,LM,D,BETA,SCALE1,SCALE2,CMAX
      my_real
     .    COEFR,BETAF ,COEFM
C----------------------------------------------------------------
C----------------------------------------------------------------
      IFORM = 1
      COEFR = ONE
      BETAF = ZERO
      COEFM = ONE
      N_NETWORK  =  UPARAM(1)  
      FLAG_HE    =  UPARAM(2)  
      IF ( FLAG_HE == 3 .OR.FLAG_HE == 4 .OR.FLAG_HE == 5 )FLAG_HE = 1

      FLAG_MUL   =  UPARAM(3)  !CALCULATED IN UPDMAT = 1 IF IRUP==33

      IF (FLAG_MUL > ZERO)THEN
       COEFR = UPARAMF(1)    
       BETAF = UPARAMF(2)    
       COEFM = UPARAMF(3)    
      ENDIF

      FLAG_PL    =  UPARAM(5) 
      NPLAS      =  0 
      TAB  = 8
      TABN = 0

      !hyperelastic parameters
      IF (FLAG_HE == 1) THEN 
         C10    = UPARAM(TAB + 1)  
         C01    = UPARAM(TAB + 2)  
         C20    = UPARAM(TAB + 3)  
         C11    = UPARAM(TAB + 4)  
         C02    = UPARAM(TAB + 5)  
         C30    = UPARAM(TAB + 6)  
         C21    = UPARAM(TAB + 7)  
         C12    = UPARAM(TAB + 8)  
         C03    = UPARAM(TAB + 9)  
         D1     = UPARAM(TAB + 10) 
         D2     = UPARAM(TAB + 11) 
         D3     = UPARAM(TAB + 12) 
         TAB = TAB + 12 
      ELSEIF (FLAG_HE == 2) THEN 
         C1   =    UPARAM(TAB + 1)
         C2   =    UPARAM(TAB + 2)
         C3   =    UPARAM(TAB + 3)
         C4   =    UPARAM(TAB + 4)
         C5   =    UPARAM(TAB + 5)
         MU   =    UPARAM(TAB + 6)
         D    =    UPARAM(TAB + 7) !=1/D
         BETA =    UPARAM(TAB + 8)
         TAB = TAB + 10  
      ELSEIF (FLAG_HE == 13) THEN 

         SCALE1   =    UPARAM(TAB + 1)
         SCALE2   =    UPARAM(TAB + 2)
      
         G        =    UPARAM(TAB + 4)!CMAX COMPUTED IN LAW100_UPD
         RBULK    =    UPARAM(TAB + 5)
         TAB = TAB + 5  
         DO I=1,NEL  
            IPOS1(I) = NINT(UVAR(I, TABN +1))
            IAD1(I)  = NPF(IFUNC(1)) / 2 + 1
            ILEN1(I) = NPF(IFUNC(1)+1) / 2 - IAD1(I) - IPOS1(I)

            IPOS2(I) = NINT(UVAR(I, TABN +2))
            IAD2(I)  = NPF(IFUNC(2)) / 2 + 1
            ILEN2(I) = NPF(IFUNC(2)+1) / 2 - IAD2(I) - IPOS2(I)
         ENDDO
         CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,TEMPEL,DYDX1,MUNH)
         CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,TEMPEL,DYDX2,DNH)

         DO I=1,NEL           
           UVAR(I, TABN +1) = IPOS1(I)
           UVAR(I, TABN +2) = IPOS2(I)
           MUNH(I) = MUNH(I) * SCALE1
           DNH(I)  = DNH(I) * SCALE2
         ENDDO
         TABN = TABN + 2
      ENDIF

      IF (FLAG_PL == 1) THEN
        NPLAS = 5 
        FF  = UPARAM(TAB + 1)    
        EPSHAT  = UPARAM(TAB + 2)    
        TAUY0   = UPARAM(TAB + 3)    
        EXPPL   = NINT(UPARAM(TAB + 4) ) 
        FACPL   = UPARAM(TAB + 5)
      ENDIF


      !viscous parameters
      TAB = TAB   + NPLAS 
      DO N = 1, N_NETWORK
        STIFFN(N)    = UPARAM(TAB + 1)
        FLAG_VISC(N) = NINT(UPARAM(TAB + 2))
        NVISC(N)     = UPARAM(TAB + 3)  
 
        IF (FLAG_VISC(N) == 1)THEN  
          A10(N)    = UPARAM(TAB + 4)
          A1(N)     = A10(N)*TIMESTEP
          EXPC(N)   = UPARAM(TAB + 5)
          EXPM(N)   = UPARAM(TAB + 6)
          KSI(N)    = UPARAM(TAB + 7)
          TAUREF(N) = UPARAM(TAB + 8)
          TAB = TAB + 3 + NVISC(N)
        ELSEIF (FLAG_VISC(N) == 2)THEN
          A10(N)    = UPARAM(TAB + 4)
          A1(N)     = A10(N)*TIMESTEP
          B0(N)     = UPARAM(TAB + 5)
          EXPN(N)   = UPARAM(TAB + 6)
          TAB = TAB + 3 + NVISC(N) 
        ELSEIF (FLAG_VISC(N) == 3)THEN
          A10(N)    = UPARAM(TAB + 4)
          A1(N)     = A10(N)*TIMESTEP
          EXPN(N)   = UPARAM(TAB + 5)
          EXPM(N)   = UPARAM(TAB + 6)
          TAB = TAB + 3 + NVISC(N)
        ENDIF
      ENDDO
      IF (FLAG_HE /= 13) THEN 
        G    =  UPARAM(TAB + 1 ) !idem starter stockage at the end
        RBULK=  UPARAM(TAB + 2 ) 
      ENDIF
      STIFF0 = FOUR_OVER_3*G + RBULK

      IF(TIME == ZERO)THEN
       IF (FLAG_PL == 1) THEN
        DO  I = 1, NEL   
         UVAR(I,TABN+1) = TAUY0
         UVAR(I,TABN+5) = ONE  
         UVAR(I,TABN+6) = ONE  
         UVAR(I,TABN+7) = ONE  
        ENDDO      
        TABN = TABN+ 4 + 9    
       ENDIF      
       DO N = 1, N_NETWORK

       !------------------------       
        IF (FLAG_VISC(N) == 1 ) THEN !BB
         DO  I = 1, NEL
         UVAR(I,TABN+1) = ONE
         UVAR(I,TABN+2) = ONE
         UVAR(I,TABN+3) = ONE
         ENDDO
         SHIFT = 9 +1  +1
         ! 9 = nombre de termes dans matrices FP
         !+1 = dgamma
         !+1 = TBNORM
        ELSEIF (FLAG_VISC(N) == 2 ) THEN !SINH
         DO  I = 1, NEL
         UVAR(I,TABN+1) = ONE
         UVAR(I,TABN+2) = ONE
         UVAR(I,TABN+3) = ONE
         ENDDO
         SHIFT = 9 +1  +1 
         ! 9 = nombre de termes dans matrices FP
         !+1 = dgamma
         !+1 = TBNORM
        ELSEIF (FLAG_VISC(N) == 3 ) THEN !POWER
         DO  I = 1, NEL
         UVAR(I,TABN+1) = ONE
         UVAR(I,TABN+2) = ONE
         UVAR(I,TABN+3) = ONE
         UVAR(I,TABN+10)= EM20 !EQUIVALENT STRAIN OLD
         ENDDO
         SHIFT = 10 +1 +1
         ! 9 = nombre de termes dans matrices FP
         !+1 = gamma
         !+1 = dgamma
         !+1 = TBNORM
        ENDIF
        TABN = TABN + SHIFT
       ENDDO
      ENDIF   


      TABN = 0
      IF (FLAG_HE == 13)TABN = TABN + 2

      IF (JCVT >0 ) THEN ! corotational => need to rotate Fp into the LOCAL frame
        R1X (1:NEL) =  GAMA_R(1:NEL,1)
        R1Y (1:NEL) =  GAMA_R(1:NEL,2)
        R1Z (1:NEL) =  GAMA_R(1:NEL,3)
        R2X (1:NEL) =  GAMA_R(1:NEL,4)
        R2Y (1:NEL) =  GAMA_R(1:NEL,5)
        R2Z (1:NEL) =  GAMA_R(1:NEL,6)
        
        R3X (1:NEL) = R1Y (1:NEL)*  R2Z (1:NEL) - R1Z (1:NEL) * R2Y (1:NEL)
        R3Y (1:NEL) = R1Z (1:NEL)*  R2X (1:NEL) - R1X (1:NEL) * R2Z (1:NEL)
        R3Z (1:NEL) = R1X (1:NEL)*  R2Y (1:NEL) - R1Y (1:NEL) * R2X (1:NEL)
        DO I=1,NEL
         R3R3 = SQRT(R3X(I)*R3X(I) + R3Y(I)*R3Y(I) + R3Z(I)*R3Z(I))
         IF (R3R3 /= ZERO) THEN
          R3X (I) = R3X(I)/R3R3
          R3Y (I) = R3Y(I)/R3R3
          R3Z (I) = R3Z(I)/R3R3
         ENDIF
        ENDDO
      ENDIF



    
      !REMOVE THERMAL STRAIN FROM TOTAL STRAIN
      !FTH = I + ALPHA DT
      IF(IEXPAN > 0)THEN
       DO I=1,NEL       
         FTH(I,1,1)  = ONE + EPSTH(I) 
         FTH(I,2,2)  = ONE + EPSTH(I)
         FTH(I,3,3)  = ONE + EPSTH(I) 
         FTH(I,1,2)  = ZERO 
         FTH(I,2,3)  = ZERO 
         FTH(I,3,1)  = ZERO   
         FTH(I,2,1)  = ZERO 
         FTH(I,3,2)  = ZERO 
         FTH(I,1,3)  = ZERO 
         FTOT(I,1,1)  = ONE+MFXX(I)
         FTOT(I,2,2)  = ONE+MFYY(I)
         FTOT(I,3,3)  = ONE+MFZZ(I)
         FTOT(I,1,2)  = MFXY(I)
         FTOT(I,2,3)  = MFYZ(I)
         FTOT(I,3,1)  = MFZX(I)      
         FTOT(I,2,1)  = MFYX(I)
         FTOT(I,3,2)  = MFZY(I)
         FTOT(I,1,3)  = MFXZ(I)     
       ENDDO 
       CALL KMATINV3 (FTH , INVFTH, NEL) ! INVERSE (FTH)  
       CALL PRODMAT  (FTOT , INVFTH, FMEC, NEL) ! FMEC = FTOT * INVFTH 
        
       DO I=1,NEL       
         F(I,1,1)  = FMEC(I,1,1)
         F(I,2,2)  = FMEC(I,2,2)
         F(I,3,3)  = FMEC(I,3,3)
         F(I,1,2)  = FMEC(I,1,2)
         F(I,2,3)  = FMEC(I,2,3)
         F(I,3,1)  = FMEC(I,3,1)
         F(I,2,1)  = FMEC(I,2,1)
         F(I,3,2)  = FMEC(I,3,2)
         F(I,1,3)  = FMEC(I,1,3)
       ENDDO 
      ELSE    
       DO I=1,NEL       
         F(I,1,1)  = ONE+MFXX(I) !FMEC
         F(I,2,2)  = ONE+MFYY(I)
         F(I,3,3)  = ONE+MFZZ(I)
         F(I,1,2)  = MFXY(I)
         F(I,2,3)  = MFYZ(I)
         F(I,3,1)  = MFZX(I)      
         F(I,2,1)  = MFYX(I)
         F(I,3,2)  = MFZY(I)
         F(I,1,3)  = MFXZ(I)     
       ENDDO 

      ENDIF
C
C

      IF (FLAG_PL == 1) THEN 
        DO I=1,NEL              
          TAUY(I)       = UVAR(I,TABN+1)     
          FPEQO(I,1,1)  = UVAR(I,TABN+5) 
          FPEQO(I,2,2)  = UVAR(I,TABN+6) 
          FPEQO(I,3,3)  = UVAR(I,TABN+7) 
          FPEQO(I,1,2)  = UVAR(I,TABN+8) 
          FPEQO(I,2,3)  = UVAR(I,TABN+9) 
          FPEQO(I,3,1)  = UVAR(I,TABN+10)        
          FPEQO(I,2,1)  = UVAR(I,TABN+11) 
          FPEQO(I,3,2)  = UVAR(I,TABN+12) 
          FPEQO(I,1,3)  = UVAR(I,TABN+13)   
        ENDDO 
        IF (JCVT >0 ) THEN ! corotational => need to rotate NEW Fp TO the GLOBAL frame
          CALL ROTTOLOC (NEL,FPEQO,
     .                     R1X, R1Y, R1Z, R2X, R2Y, R2Z, R3X, R3Y, R3Z)
        ENDIF
        ! F^e = F * inv FPEQO then F^eF^e^T = B trial when having plasiticity in equilibrium network
        CALL CALCMATB (NEL, F, FPEQO, FFT)! 
      ELSE
        !  F^eF^e^T = B trial considering trial F = F^e
        CALL PRODAAT(F ,  FFT, NEL) ! b = F * FT
      ENDIF



      !===========================
      !network 0 : compute stress  
      !===========================
      IF (FLAG_HE == 1) THEN 
        CALL POLYSTRESS2(
     1                NEL , FFT , C10, C01, C20, 
     2                C11 ,C02, C30, C21, C12,
     3                C03 ,D1 ,D2  ,  D3, SIGA ,
     4                BI1,BI2,JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF,RBULK,IFORM)
      ELSEIF (FLAG_HE == 2) THEN 
        CALL SIGABOYCE(
     1                NEL , FFT ,C1,C2  ,C3,
     3                C4  ,C5   ,MU,BETA,D ,
     4                SIGA ,BI1  , JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF  )
      ELSEIF (FLAG_HE == 13) THEN 
        CALL NEO_HOOK_T(
     1                NEL , FFT , SIGA ,
     4                BI1,JDET ,FLAG_MUL,MUNH,DNH,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF)

      ENDIF ! FLAG_HE  

      !========================================
      !Equilibrium networks: compute plasticity   
      !========================================
      IF (FLAG_PL == 1) THEN 
        DO I = 1,NEL
          TRACEA(I) = THIRD*(SIGA(I,1,1) +SIGA(I,2,2) + SIGA(I,3,3))
          !DEVIATOR OF STRESS A
          SA1(I) =  SIGA(I,1,1)  - TRACEA(I)
          SA2(I) =  SIGA(I,2,2)  - TRACEA(I)
          SA3(I) =  SIGA(I,3,3)  - TRACEA(I)
          SA4(I) =  SIGA(I,1,2) 
          SA5(I) =  SIGA(I,2,3) 
          SA6(I) =  SIGA(I,3,1) 
          TANORM(I)   = SQRT( (MAX(EM20,SA1(I)**2+SA2(I)**2+SA3(I)**2  
     .                 +     TWO*(SA4(I)**2+SA5(I)**2+SA6(I)**2 )) ) ) ! NORM!
        ENDDO
C
        DO I=1,NEL
         PLA (I)   = UVAR(I,TABN+4)
         S(I,1,1) = ONE         
         S(I,2,2) = ONE         
         S(I,3,3) = ONE         
         S(I,1,2) = ZERO       
         S(I,2,3) = ZERO          
         S(I,3,1) = ZERO                
         S(I,2,1) = ZERO                                   
         S(I,3,2) = ZERO                                    
         S(I,1,3) = ZERO                                     
        END DO
        DO  I=1,NEL  
          TEMP1   =   TANORM(I) / TAUY(I) 
          PLAP(I) =   FACPL*TEMP1**EXPPL
          !PLAP(I)= FACPL* EXP(TEMP1 )
          DPLA(I) = PLAP(I)*TIMESTEP
          PLA(I)  =    UVAR(I,TABN+4) + DPLA(I)
          FACTOR    = PLAP(I)*TIMESTEP/TANORM(I)
          S(I,1,1) = ONE+FACTOR *SA1(I) !gamma_dot*N  
          S(I,2,2) = ONE+FACTOR *SA2(I)               
          S(I,3,3) = ONE+FACTOR *SA3(I)               
          S(I,1,2) = FACTOR *SA4(I)  !gamma_dot*N     
          S(I,2,3) = FACTOR *SA5(I)                   
          S(I,3,1) = FACTOR *SA6(I)                   
          S(I,2,1) = S(I,1,2)                                            
          S(I,3,2) = S(I,2,3)                                            
          S(I,1,3) = S(I,3,1)                                              
        ENDDO ! J=1,NINDX
        CALL PRODMAT(S  ,FPEQO,  FPEQ, NEL) ! F_N+1 = (i + DT *DP)* F_N
        CALL CALCMATB (NEL, F, FPEQ, FFTN) ! b = F * FT   
        !Update stress
        IF (FLAG_HE == 1) THEN 
          CALL POLYSTRESS2(
     1                   NEL , FFTN , C10, C01, C20, 
     2                   C11 ,C02, C30, C21, C12,
     3                   C03 ,D1 ,D2  ,  D3, SIGA ,
     4                   BI1,BI2,JDET ,FLAG_MUL,
     5                   NVARF,COEFR, BETAF,COEFM  ,UVARF,RBULK,IFORM)      
        ELSEIF (FLAG_HE == 2) THEN 
          CALL SIGABOYCE(
     1                   NEL , FFTN ,C1,C2  ,C3,
     3                   C4  ,C5   ,MU,BETA,D ,
     4                   SIGA ,BI1  , JDET ,FLAG_MUL,
     5                   NVARF,COEFR, BETAF,COEFM  ,UVARF  )
        ELSEIF (FLAG_HE == 13) THEN 
         CALL NEO_HOOK_T(
     1                NEL , FFT , SIGA ,
     4                BI1,JDET ,FLAG_MUL,MUNH,DNH,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF)

        ENDIF ! FLAG_HE  
        IF (JCVT >0 ) THEN ! corotational => need to rotate NEW Fp TO the GLOBAL frame
           CALL ROTTOGLOB (NEL,FPEQ,
     .                     R1X, R1Y, R1Z, R2X, R2Y, R2Z, R3X, R3Y, R3Z)

        ENDIF
        DO I=1,NEL
          TAUY(I)  = TAUY0 * (FF +(ONE - FF)*EXP(-PLA(I)/EPSHAT))
          !TAUY(I)  = HH *(ONE - UVAR(I,1)/TAUY0)*PLAP(I)*TIMESTEP + UVAR(I,1)
          UVAR(I,TABN +1) = TAUY(I) 
          UVAR(I,TABN +4) = PLA(I)           
          UVAR(I,TABN +5)    =   FPEQ(I,1,1) ! stored in global frame always
          UVAR(I,TABN +6)    =   FPEQ(I,2,2) 
          UVAR(I,TABN +7)    =   FPEQ(I,3,3) 
          UVAR(I,TABN +8)    =   FPEQ(I,1,2) 
          UVAR(I,TABN +9)    =   FPEQ(I,2,3) 
          UVAR(I,TABN +10)   =   FPEQ(I,3,1)      
          UVAR(I,TABN +11)   =   FPEQ(I,2,1) 
          UVAR(I,TABN +12)   =   FPEQ(I,3,2) 
          UVAR(I,TABN +13)   =   FPEQ(I,1,3)  
        ENDDO          
        TABN = TABN + 13
      ENDIF! IF (FLAG_PL == 1) 

      !====================================
      !Secondary networks: compute stresses   
      !====================================
      FLAG_MUL   =  0
      DO I=1,NEL       
       !print*, ' JDET', JDET(I)
       SIGNXX(I) = SIGA(I,1,1) 
       SIGNYY(I) = SIGA(I,2,2) 
       SIGNZZ(I) = SIGA(I,3,3) 
       SIGNXY(I) = SIGA(I,1,2) 
       SIGNYZ(I) = SIGA(I,2,3) 
       SIGNZX(I) = SIGA(I,3,1) 
      ENDDO 
      !------------------------
      !start loop over networks
      !------------------------
      !***************************************************************
      DO N = 1, N_NETWORK
      !***************************************************************
       DO I=1,NEL  
         SIGB(I,1,1) = ZERO  
         SIGB(I,2,2) = ZERO 
         SIGB(I,3,3) = ZERO 
         SIGB(I,1,2) = ZERO  
         SIGB(I,2,3) = ZERO  
         SIGB(I,3,1) = ZERO  
         FPO(I,1,1)  = UVAR(I,TABN+1) ! global frame must be modified to local if jcvt >0
         FPO(I,2,2)  = UVAR(I,TABN+2) 
         FPO(I,3,3)  = UVAR(I,TABN+3) 
         FPO(I,1,2)  = UVAR(I,TABN+4) 
         FPO(I,2,3)  = UVAR(I,TABN+5) 
         FPO(I,3,1)  = UVAR(I,TABN+6)         
         FPO(I,2,1)  = UVAR(I,TABN+7) 
         FPO(I,3,2)  = UVAR(I,TABN+8) 
         FPO(I,1,3)  = UVAR(I,TABN+9)         
c
       ENDDO
       IF (JCVT >0 ) THEN ! corotational => need to rotate NEW Fp TO the GLOBAL frame
          CALL ROTTOLOC (NEL,FPO,
     .                     R1X, R1Y, R1Z, R2X, R2Y, R2Z, R3X, R3Y, R3Z)
       ENDIF
       !FE ={F}{Fp_old}^(-1) then  MATB = FE FE^(T) (elastic part)
       !CALL CALCMATB (NEL, F, FPO, MATB)    
       CALL KMATINV3 (FPO , INVFPO, NEL)      !INVFPO = INVERSE (FP)  
       CALL PRODMAT  (F   , INVFPO, FE, NEL)  ! FE = F * INVFPO      
       CALL PRODAAT  (FE  , MATB  , NEL)      ! MATB = FE FE^(T)   
        
       !--------------------------------------------
       !ESTIMATE TRIAL CAUCHY STRESS
       !--------------------------------------------
       ! secondary network : compute trial stress : 
       !--------------------------------------------
       IF (FLAG_HE == 1) THEN !polynomial
       CALL POLYSTRESS2(
     1                NEL , MATB , C10, C01, C20, 
     2                C11 ,C02, C30, C21, C12,
     3                C03 ,D1 ,D2  ,  D3, SIGB ,
     4                BI1,BI2,JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF,RBULK,IFORM)
     
       ELSEIF (FLAG_HE == 2) THEN !arruda boyce
       CALL SIGABOYCE(
     1                NEL , MATB ,C1,C2  ,C3,
     3                C4  ,C5   ,MU,BETA,D ,
     4                SIGB,BI1  ,JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF)
       ELSEIF (FLAG_HE == 13) THEN !thermal neo hook
        CALL NEO_HOOK_T(
     1                NEL , FFT , SIGB ,
     4                BI1,JDET ,FLAG_MUL,MUNH,DNH,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF)
     
     
       ENDIF ! FLAG_HE  
     
       DO I=1,NEL  !     scale trial cauchy stress in chain B
         SIGB(I,1,1) = STIFFN(N) *  SIGB(I,1,1)
         SIGB(I,2,2) = STIFFN(N) *  SIGB(I,2,2)
         SIGB(I,3,3) = STIFFN(N) *  SIGB(I,3,3) 
         SIGB(I,1,2) = STIFFN(N) *  SIGB(I,1,2)
         SIGB(I,2,3) = STIFFN(N) *  SIGB(I,2,3)
         SIGB(I,3,1) = STIFFN(N) *  SIGB(I,3,1) 
       ENDDO
      
        !COMPUTE EEFFECTIVE CREEP STRAIN RATE
        !------------------------------------   
        DO I=1,NEL 
          TRACEB(I) = THIRD*(SIGB(I,1,1) +SIGB(I,2,2) + SIGB(I,3,3))
          !DEVIATOR OF secondary network STRESS  
          SB1(I) =  SIGB(I,1,1)  - TRACEB(I)
          SB2(I) =  SIGB(I,2,2)  - TRACEB(I)
          SB3(I) =  SIGB(I,3,3)  - TRACEB(I)
          SB4(I) =  SIGB(I,1,2) 
          SB5(I) =  SIGB(I,2,3) 
          SB6(I) =  SIGB(I,3,1) 
          !Nomr of stress secondary network N
          TBNORM(I)   = SQRT (MAX(EM20,SB1(I)**2+SB2(I)**2+SB3(I)**2  
     .                 +     TWO*(SB4(I)**2+SB5(I)**2+SB6(I)**2 ))  ) ! NORM!
        ENDDO

        !------------------------------------        
        !COMPUTE EEFFECTIVE CREEP STRAIN RATE
        !------------------------------------        
        IF (FLAG_VISC(N) == 1 ) THEN!BERGSTROM BOYCE MODEL          
          CALL VISCBB ( NEL, FPO, TBNORM, A1(N) ,EXPC(N),
     .                  EXPM(N) , KSI(N), TAUREF(N), DGAMMA ) 
          DO I=1,NEL 
            UVAR(I,TABN+10) =   DGAMMA(I)               
            UVAR(I,TABN+11) =   TBNORM(I)
          ENDDO
          SHIFT = 9  +1 +1

        ELSEIF (FLAG_VISC(N) == 2 )THEN !Hyperbolic Sine          
          CALL VISCSINH ( NEL, TBNORM,  A1(N),B0(N),
     .                  EXPN(N) , DGAMMA ) 
          DO I=1,NEL 
            UVAR(I,TABN+10) =   DGAMMA(I)               
            UVAR(I,TABN+11) =   TBNORM(I)
          ENDDO
          SHIFT = 9  + 1 +1
        ELSEIF (FLAG_VISC(N) == 3 )THEN ! Power law          
          DO I=1,NEL 
            GAMMAOLD(I) =  UVAR(I,TABN+10)               
          ENDDO
          CALL VISCPOWER ( NEL, TBNORM,  A1(N),EXPM(N) ,EXPN(N),GAMMAOLD, DGAMMA ) 
          DO I=1,NEL 
            UVAR(I,TABN+10) =   GAMMAOLD(I) +  DGAMMA(I)   
            UVAR(I,TABN+11) =   DGAMMA(I) 
            UVAR(I,TABN+12) =   TBNORM(I)              
          ENDDO
          SHIFT = 10  +1 +1
        ENDIF
        !------------------------------------        

        DO I=1,NEL 
          FACTOR = DGAMMA(I)/TBNORM(I)                                     
          LB(I,1,1) = FACTOR *SB1(I) !gamma_dot*N  
          LB(I,2,2) = FACTOR *SB2(I)               
          LB(I,3,3) = FACTOR *SB3(I)               
          LB(I,1,2) = FACTOR *SB4(I) !gamma_dot*N     
          LB(I,2,3) = FACTOR *SB5(I)                  
          LB(I,3,1) = FACTOR *SB6(I)                  
          LB(I,2,1) = LB(I,1,2)                                            
          LB(I,3,2) = LB(I,2,3)                                            
          LB(I,1,3) = LB(I,3,1)                                                                        
        ENDDO
        !------------------------------------   
        !Solve F_n+1 viscous :   
        !------------------------------------  
        CALL KMATINV3(FE , INVFE, NEL)       !  INVFE  = INVERSE (FE)   
        CALL PRODMAT (LB ,    FE, FEDP, NEL) !  FEDP   = Lb * FE
        CALL PRODMAT(INVFE ,FEDP, DFP , NEL) !  DFP    = INVFE  * Lb * FE

        !CALL PRODMAT(DFP ,DFP, DFP2 , NEL) ! if order 2
        DO I=1,NEL               
         ! Fp_n+1 = exp(DFP) * Fp_old = (I+DFP) * Fp_old
          SN(I,1,1) = ONE + DFP(I,1,1) !+ HALF * DFP2(I,1,1)
          SN(I,2,2) = ONE + DFP(I,2,2) !+ HALF * DFP2(I,2,2)             
          SN(I,3,3) = ONE + DFP(I,3,3) !+ HALF * DFP2(I,3,3)               
          SN(I,1,2) = DFP(I,1,2)!+ HALF * DFP2(I,1,2)    
          SN(I,2,3) = DFP(I,2,3)!+ HALF * DFP2(I,2,3)                 
          SN(I,3,1) = DFP(I,3,1)!+ HALF * DFP2(I,3,1)                 
          SN(I,2,1) = DFP(I,2,1)!+ HALF * DFP2(I,2,1)                                          
          SN(I,3,2) = DFP(I,3,2)!+ HALF * DFP2(I,3,2)                                           
          SN(I,1,3) = DFP(I,1,3)!+ HALF * DFP2(I,1,3)     
        ENDDO

        CALL PRODMAT(SN ,FPO,  FP, NEL)  !Fp_N+1 = (I + dT *DFP)* Fp_N 
        CALL CALCMATB (NEL, F, FP, MATB) !FE ={F}{Fp}^(-1) then  MATB = FE FE^(T) 

        IF (FLAG_HE == 1) THEN 
         CALL POLYSTRESS2(
     1                NEL , MATB , C10, C01, C20, 
     2                C11 ,C02, C30, C21, C12,
     3                C03 ,D1 ,D2  ,  D3, SIGB ,
     4                BI1,BI2,JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF,RBULK,IFORM)
        ELSEIF (FLAG_HE == 2) THEN 
         CALL SIGABOYCE(
     1                NEL , MATB ,C1,C2  ,C3,
     3                C4  ,C5   ,MU,BETA,D ,
     4                SIGB,BI1  ,JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF)
      
        ELSEIF (FLAG_HE == 13) THEN 
         CALL NEO_HOOK_T(
     1                NEL , FFT , SIGB ,
     4                BI1,JDET ,FLAG_MUL,MUNH,DNH,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF)
     
     
        ENDIF ! FLAG_HE  

        IF (JCVT >0 ) THEN ! corotational => need to rotate NEW Fp TO the GLOBAL frame
         CALL ROTTOGLOB (NEL,FP,
     .                     R1X, R1Y, R1Z, R2X, R2Y, R2Z, R3X, R3Y, R3Z)

        ENDIF

        DO I=1,NEL  
         SIGB(I,1,1) = STIFFN(N) *  SIGB(I,1,1)
         SIGB(I,2,2) = STIFFN(N) *  SIGB(I,2,2)
         SIGB(I,3,3) = STIFFN(N) *  SIGB(I,3,3) 
         SIGB(I,1,2) = STIFFN(N) *  SIGB(I,1,2)
         SIGB(I,2,3) = STIFFN(N) *  SIGB(I,2,3)
         SIGB(I,3,1) = STIFFN(N) *  SIGB(I,3,1) 
         UVAR(I,TABN+1)   =   FP(I,1,1)
         UVAR(I,TABN+2)   =   FP(I,2,2)
         UVAR(I,TABN+3)   =   FP(I,3,3)
         UVAR(I,TABN+4)   =   FP(I,1,2)
         UVAR(I,TABN+5)   =   FP(I,2,3)
         UVAR(I,TABN+6)   =   FP(I,3,1)      
         UVAR(I,TABN+7)   =   FP(I,2,1)
         UVAR(I,TABN+8)   =   FP(I,3,2)
         UVAR(I,TABN+9)   =   FP(I,1,3)   
        ENDDO
         
        !--------------------
        !UPDATE TOTAL STRESS
        !--------------------
        DO I=1,NEL  
         SIGNXX(I) = SIGNXX(I) + SIGB(I,1,1)
         SIGNYY(I) = SIGNYY(I) + SIGB(I,2,2)
         SIGNZZ(I) = SIGNZZ(I) + SIGB(I,3,3)
         SIGNXY(I) = SIGNXY(I) + SIGB(I,1,2)
         SIGNYZ(I) = SIGNYZ(I) + SIGB(I,2,3)
         SIGNZX(I) = SIGNZX(I) + SIGB(I,3,1)       
        ENDDO
        TABN = TABN + SHIFT

      ENDDO! N = 1, N_NETWORK
      !***************************************************************
      !***************************************************************

     
C-------------------------------
C cauchy to glabale
C-------------------------------
      DO I=1,NEL
        DSIG = SQRT((SIGNXX(I)-SIGOXX(I))**2+(SIGNYY(I)-SIGOYY(I))**2+(SIGNZZ(I)-SIGOZZ(I))**2
     .         + TWO*((SIGNXY(I)-SIGOXY(I))**2+(SIGNYZ(I)-SIGOYZ(I))**2+(SIGNZX(I)-SIGOZX(I))**2))
        DEPS = SQRT   (DEPSXX(I)**2 + DEPSYY(I)**2 + DEPSZZ(I)**2
     .         + TWO*(DEPSXY(I)**2 + DEPSYZ(I)**2 + DEPSZX(I)**2))
        IF(DEPS == ZERO)THEN
          STIFF(I)= STIFF0
        ELSE
          STIFF(I)= MAX(STIFF0 ,DSIG /MAX(EM20,DEPS))
        ENDIF
C         
        SOUNDSP(I)=SQRT(STIFF(I)/RHO(I))

        VISCMAX(I) = ZERO 
       ENDDO
      IF (IMPL_S > 0 .OR. IHET > 1) THEN
       DO I=1,NEL
         ET(I)= MAX(ONE,STIFF(I))
       ENDDO
      ENDIF
C   
      RETURN
      END

