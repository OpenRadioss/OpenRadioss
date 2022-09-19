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
C BERGSTROM BOYCE
Chd|====================================================================
Chd|  SIGEPS95                      source/materials/mat/mat095/sigeps95.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        CALCMATB                      source/materials/mat/mat100/calcmatb.F
Chd|        KMATINV3                      source/materials/tools/kmatinv.F
Chd|        POLYSTRESS2                   source/materials/mat/mat100/sigpoly.F
Chd|        POLYSTREST2                   source/materials/mat/mat100/sigpoly.F
Chd|        PRODAAT                       source/materials/tools/prodAAT.F
Chd|        PRODMAT                       source/materials/tools/prodmat.F
Chd|        ROTTOGLOB                     source/materials/mat/mat095/sigeps95.F
Chd|        ROTTOLOC                      source/materials/mat/mat095/sigeps95.F
Chd|        VISCBB                        source/materials/mat/mat100/viscbb.F
Chd|====================================================================
       SUBROUTINE SIGEPS95(
     1      NEL    , NUPARAM, NUVAR   , NFUNC  , IFUNC ,
     2      NPF    ,TF      , TIME    , TIMESTEP, UPARAM,
     3      RHO0  , RHO     ,VOLUME   , EINT   , NGL     ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX,
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      MFXX   ,MFXY    ,MFXZ,MFYX, MFYY  , MFYZ  ,
     B      MFZX   ,MFZY    ,MFZZ     ,
     C      SOUNDSP, VISCMAX, UVAR    , OFF   , ISMSTR, ET  ,
     D      IHET   ,OFFG    , EPSTH3  , IEXPAN,
     E      UPARAMF,UVARF   ,NVARF    ,JCVT   ,GAMA_R)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include "scr17_c.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "scr05_c.inc"
#include "impl1_c.inc"
#include "units_c.inc"
#include "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL, JCVT,    NUPARAM, NUVAR,ISMSTR,NGL(*),IHET,IEXPAN,NVARF
      my_real
     .      TIME       , TIMESTEP   , UPARAM(NUPARAM),UPARAMF(NUPARAM),
     .      RHO   (NEL), RHO0  (NEL), VOLUME(NEL), EINT(NEL),
     .      EPSPXX(NEL), EPSPYY(NEL), EPSPZZ(NEL),
     .      EPSPXY(NEL), EPSPYZ(NEL), EPSPZX(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),OFFG(NEL),
     .      MFXX(NEL)  , MFXY(NEL)  ,MFXZ(NEL)  ,MFYX(NEL) ,
     .      MFYY(NEL)  , MFYZ(NEL)  ,MFZX(NEL)  ,MFZY(NEL)  ,MFZZ(NEL),
     .      EPSTH3(NEL), GAMA_R(NEL,6)
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
      INTEGER   I,J,K,KK,LL,FLAGBB,DIRECT,ITER,NITER,FLAG_MUL,IAVIS

      my_real
     .   ET1,ET2,ET3,A1,A10,KSI,MU,D,LAM,G,RBULK,AA,BB,CC,SB, TEMP,
     .   TEMP2,MAXL,STIFF0,DSIG,DEPS,COEF1,COEF2,COEF3,COEF4,COEF5,COEF6,
     .   AA1,B1,C1,DD1,AA2,B2,C2,DD2,AA3,B3,C3,DD3,INV12,INV22,INV32,
     .   EVB12,EVB22,EVB32,JDET2,JM1,ETVOL,FACTOR,TAUREF,R3R3,R2R2,R1R1,
     .   C10,C01,C20,C11,C02,C30,C21,C12,C03,D1,D2,D3,EXPC,EXPM,R(3),JCST(3),
     .   BI1(NEL),BI2(NEL),JDET(NEL),STIFF(NEL),
     .   TA1(NEL), TA2(NEL),TA3(NEL),T1(NEL), T2(NEL),T3(NEL),LPCHAIN(NEL),
     .   TB1(NEL), TB2(NEL),TB3(NEL),TRACE(NEL),TRACEB(NEL),ETA(NEL),ETB(NEL),
     .   SB1(NEL), SB2(NEL),SB3(NEL),SB4(NEL), SB5(NEL),SB6(NEL),
     .   TBNORM(NEL),DGAMMA(NEL),
     .   DEVB1(NEL),DEVB2(NEL),DEVB3(NEL),DEVB4(NEL),DEVB5(NEL),DEVB6(NEL),
     .   R1X(NEL),R1Y(NEL),R1Z(NEL),R2X(NEL),R2Y(NEL),R2Z(NEL),R3X(NEL),R3Y(NEL),R3Z(NEL),
     .   NB(NEL,3),
     .   F(NEL,3,3),FT(NEL,3,3),FE(NEL,3,3),INVFE(NEL,3,3),FET(NEL,3,3),FP(NEL,3,3),
     .   FFT(NEL,3,3),INVFPO(NEL,3,3),MATB(NEL,3,3),FPO(NEL,3,3),BPO(NEL,3,3),INVSN(NEL,3,3),
     .   SIG(NEL,3,3),SIGB(NEL,3,3),SIGA(NEL,3,3),SN(NEL,3,3),FEDP(NEL,3,3),
     .   DFP(NEL,3,3),LB(NEL,3,3),DFP2(NEL,3,3),FPDOT(NEL,3,3),
     .   CAII(NEL,3),CBII(NEL,3),C_MAX(NEL),CII(3)
      my_real
     .    COEFR,BETAF ,COEFM
C----------------------------------------------------------------
      COEFR = ONE
      BETAF = ZERO
      COEFM = ONE

c    material parameters
      C10  =  UPARAM(1)
      C01  =  UPARAM(2)
      C20  =  UPARAM(3)
      C11  =  UPARAM(4)
      C02  =  UPARAM(5)
      C30  =  UPARAM(6)
      C21  =  UPARAM(7)
      C12  =  UPARAM(8)
      C03  =  UPARAM(9)
      D1   =  UPARAM(11)  !ONE/D1
      D2   =  UPARAM(12)  !ONE/D2
      D3   =  UPARAM(13)  !ONE/D3
      SB   =  UPARAM(14)
      A10  =  UPARAM(15)
      A1   =  A10*TIMESTEP
      EXPC =  UPARAM(16)
      EXPM =  UPARAM(17)
      KSI  =  UPARAM(18)
      G    =  UPARAM(19)
      RBULK=  UPARAM(20)
      TAUREF= UPARAM(22)
      !
      STIFF0 = FOUR_OVER_3*G + RBULK
      MAXL = ONE
	  IAVIS =1
	  IF (A10*SB==ZERO) IAVIS=0

      FLAG_MUL   =  UPARAM(21)  !CALCULATED IN UPDMAT = 1 IF IRUP==33
      IF (FLAG_MUL > ZERO)THEN
       COEFR = UPARAMF(1)
       BETAF = UPARAMF(2)
       COEFM = UPARAMF(3)
      ENDIF

      IF(TIME == ZERO)THEN
        DO  I = 1, NEL
         UVAR(I,1) = ONE
         UVAR(I,2) = ONE
         UVAR(I,3) = ONE
         UVAR(I,4) = ZERO
         UVAR(I,5) = ZERO
         UVAR(I,6) = ZERO
         UVAR(I,7) = ZERO
         UVAR(I,8) = ZERO
         UVAR(I,9) = ZERO
        ENDDO
      ENDIF
C	  
      IF (IAVIS>0) THEN	
      DO  I = 1, NEL
         !retreive previous viscous deformation gradient
         FPO(I,1,1)  = UVAR(I,1)
         FPO(I,2,2)  = UVAR(I,2)
         FPO(I,3,3)  = UVAR(I,3)
         FPO(I,1,2)  = UVAR(I,4)
         FPO(I,2,3)  = UVAR(I,5)
         FPO(I,3,1)  = UVAR(I,6)
         FPO(I,2,1)  = UVAR(I,7)
         FPO(I,3,2)  = UVAR(I,8)
         FPO(I,1,3)  = UVAR(I,9)
      ENDDO

      IF (JCVT >0 ) THEN ! corotational => need to rotate Fp in the LOCAL frame
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
        CALL ROTTOLOC (NEL,FPO,
     .                     R1X, R1Y, R1Z, R2X, R2Y, R2Z, R3X, R3Y, R3Z)

      ENDIF
      ENDIF !(IAVIS>0)

      DO I=1,NEL
       F(I,1,1)  = ONE+MFXX(I)
       F(I,2,2)  = ONE+MFYY(I)
       F(I,3,3)  = ONE+MFZZ(I)
       F(I,1,2)  = MFXY(I)
       F(I,2,3)  = MFYZ(I)
       F(I,3,1)  = MFZX(I)
       F(I,2,1)  = MFYX(I)
       F(I,3,2)  = MFZY(I)
       F(I,1,3)  = MFXZ(I)
      ENDDO


      CALL PRODAAT(F , FFT, NEL) ! b = F * FT


      !-------------------------
      !chain A: compute stress :      cauchy stress in chain A
      !-------------------------
      !entry B=F*FT matrix --  output hyperelastic stress
       CALL POLYSTREST2(
     1                NEL , FFT , C10, C01, C20,
     2                C11 ,C02, C30, C21, C12,
     3                C03 ,D1 ,D2  ,  D3, SIGA ,
     4                BI1,BI2,JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF,CAII)


      IF (IAVIS>0) THEN	
      !-------------------------
      !chain B: compute stress :
      !-------------------------
      DO I=1,NEL
         SIGB(I,1,1) = ZERO
         SIGB(I,2,2) = ZERO
         SIGB(I,3,3) = ZERO
         SIGB(I,1,2) = ZERO
         SIGB(I,2,3) = ZERO
         SIGB(I,3,1) = ZERO
      ENDDO

      !CALL CALCMATB (NEL, F, FPO, MATB)
      !FE ={F}{Fp_old}^(-1) then  MATB = FE FE^(T) (elastic part)
      CALL KMATINV3 (FPO , INVFPO, NEL)      !INVFPO = INVERSE (FP)
      CALL PRODMAT  (F   , INVFPO, FE, NEL)  ! FE = F * INVFPO
      CALL PRODAAT  (FE  , MATB  , NEL)      ! MATB = FE FE^(T)

      !---------------------------------
      !ESTIMATE TRIAL CAUCHY STRESS : cauchy stress in chain B
      !---------------------------------
      !entry B matrix --  output hyperelastic stress
       CALL POLYSTRESS2(
     1                NEL , MATB , C10, C01, C20,
     2                C11 ,C02, C30, C21, C12,
     3                C03 ,D1 ,D2  ,  D3, SIGB ,
     4                BI1,BI2,JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF)


        DO I=1,NEL  !     scale trial cauchy stress in chain B
         SIGB(I,1,1) = SB *  SIGB(I,1,1)
         SIGB(I,2,2) = SB *  SIGB(I,2,2)
         SIGB(I,3,3) = SB *  SIGB(I,3,3)
         SIGB(I,1,2) = SB *  SIGB(I,1,2)
         SIGB(I,2,3) = SB *  SIGB(I,2,3)
         SIGB(I,3,1) = SB *  SIGB(I,3,1)
        ENDDO


        DO I=1,NEL
          TRACEB(I) = THIRD*(SIGB(I,1,1) +SIGB(I,2,2) + SIGB(I,3,3))
          !DEVIATOR OF STRESS B
          SB1(I) =  SIGB(I,1,1)  - TRACEB(I)
          SB2(I) =  SIGB(I,2,2)  - TRACEB(I)
          SB3(I) =  SIGB(I,3,3)  - TRACEB(I)
          SB4(I) =  SIGB(I,1,2)
          SB5(I) =  SIGB(I,2,3)
          SB6(I) =  SIGB(I,3,1)
          !Nomr of stress B
          TBNORM(I)   = SQRT (MAX(EM20,SB1(I)**2+SB2(I)**2+SB3(I)**2
     .                 +     TWO*(SB4(I)**2+SB5(I)**2+SB6(I)**2 ))  ) ! NORM!
        ENDDO

        !COMPUTE EEFFECTIVE CREEP STRAIN RATE
        !------------------------------------
        !DGAMMA(I) = A1* ((LPCHAIN(I) - ONE +KSI)**C) * (TBNORM(I)/TAUREF)**EXPM
        CALL VISCBB ( NEL , FPO, TBNORM  , A1     , EXPC,
     .                EXPM, KSI, TAUREF  , DGAMMA )


        DO I=1,NEL ! calcul lde Lb
          UVAR(I,10)= DGAMMA(I)
          FACTOR    = DGAMMA(I)/TBNORM(I)
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
        !PRODMAT(A, B, C, NEL) computes C(NEL,3,3) wich is the product [C] = [A][B]
        CALL KMATINV3(FE , INVFE, NEL)       !  INVFE  = INVERSE (FE)
        CALL PRODMAT (LB ,    FE, FEDP, NEL) !  FEDP   = Lb * FE
        CALL PRODMAT(INVFE ,FEDP, DFP , NEL) !  DFP    = INVFE  * Lb * FE
        !not needed CALL PRODMAT(DFP ,FPO, FPDOT , NEL) !FPDOT = DFP * FPO
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
        CALL PRODMAT(SN ,FPO,  FP, NEL) ! Fp_N+1 = (I + dT *DFP)* Fp_N
        CALL CALCMATB (NEL, F, FP, MATB) !FE ={F}{Fp}^(-1) then  MATB = FE FE^(T)

        !-------------------------
        !chain B: compute stress :      cauchy stress in chain B
        !-------------------------
        CALL POLYSTREST2(
     1                NEL , MATB , C10, C01, C20,
     2                C11 ,C02, C30, C21, C12,
     3                C03 ,D1 ,D2  ,  D3, SIGB ,
     4                BI1,BI2,JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF,CBII)

        DO I=1,NEL  !     scale  cauchy stress in chain B
         SIGB(I,1,1) = SB *  SIGB(I,1,1)
         SIGB(I,2,2) = SB *  SIGB(I,2,2)
         SIGB(I,3,3) = SB *  SIGB(I,3,3)
         SIGB(I,1,2) = SB *  SIGB(I,1,2)
         SIGB(I,2,3) = SB *  SIGB(I,2,3)
         SIGB(I,3,1) = SB *  SIGB(I,3,1)
        ENDDO
       IF (JCVT >0 ) THEN ! corotational => need to rotate NEW Fp TO the GLOBAL frame
        CALL ROTTOGLOB (NEL,FP,
     .                     R1X, R1Y, R1Z, R2X, R2Y, R2Z, R3X, R3Y, R3Z)

       ENDIF
       DO I=1,NEL
         UVAR(I,1)   =   FP(I,1,1)!store visous gradient of deformation
         UVAR(I,2)   =   FP(I,2,2)
         UVAR(I,3)   =   FP(I,3,3)
         UVAR(I,4)   =   FP(I,1,2)
         UVAR(I,5)   =   FP(I,2,3)
         UVAR(I,6)   =   FP(I,3,1)
         UVAR(I,7)   =   FP(I,2,1)
         UVAR(I,8)   =   FP(I,3,2)
         UVAR(I,9)   =   FP(I,1,3)
       ENDDO
C----- A10*SB=0 ->sigB=sigA		
	  ELSE	
        DO I=1,NEL  !     scale  cauchy stress in chain B
         SIGB(I,1:3,1:3) = SB *  SIGA(I,1:3,1:3)
		 CBII(I,1:3) = CAII(I,1:3)
        ENDDO
      END IF !(IAVIS>0) THEN	

      !--------------------
      !UPDATE TOTAL STRESS : sum of the stresses of each network
      !--------------------
        DO I=1,NEL
         SIGNXX(I) = SIGA(I,1,1) + SIGB(I,1,1)
         SIGNYY(I) = SIGA(I,2,2) + SIGB(I,2,2)
         SIGNZZ(I) = SIGA(I,3,3) + SIGB(I,3,3)
         SIGNXY(I) = SIGA(I,1,2) + SIGB(I,1,2)
         SIGNYZ(I) = SIGA(I,2,3) + SIGB(I,2,3)
         SIGNZX(I) = SIGA(I,3,1) + SIGB(I,3,1)
        ENDDO


       DO I=1,NEL
         CII(1:3)=CAII(I,1:3)+SB*CBII(I,1:3)
		 C_MAX(I)=MAX(STIFF0,CII(1),CII(2),CII(3))
       ENDDO

       DO I=1,NEL
c        DSIG = SQRT((SIGNXX(I)-SIGOXX(I))**2+(SIGNYY(I)-SIGOYY(I))**2
c     .            +(SIGNZZ(I)-SIGOZZ(I))**2
c     .         + TWO*((SIGNXY(I)-SIGOXY(I))**2
c     .                +(SIGNYZ(I)-SIGOYZ(I))**2+(SIGNZX(I)-SIGOZX(I))**2))
c
c        DEPS = SQRT   (DEPSXX(I)**2 + DEPSYY(I)**2 + DEPSZZ(I)**2
c     .         + TWO*(DEPSXY(I)**2 + DEPSYZ(I)**2 + DEPSZX(I)**2))
c
c        IF(DEPS == ZERO)THEN
c          STIFF(I)= STIFF0
c        ELSE
c          STIFF(I)= MAX(STIFF0,DSIG /MAX(EM20,DEPS))
c        ENDIF
C         
        SOUNDSP(I)=SQRT(C_MAX(I)/RHO(I))
        VISCMAX(I) = ZERO
       ENDDO
      IF (IMPL_S > 0 .OR. IHET > 1) THEN
       DO I=1,NEL
         ET(I)= ONE
       ENDDO
      ENDIF

C   
      RETURN
      END
Chd|====================================================================
Chd|  ROTTOLOC                      source/materials/mat/mat095/sigeps95.F
Chd|-- called by -----------
Chd|        SIGEPS100                     source/materials/mat/mat100/sigeps100.F
Chd|        SIGEPS95                      source/materials/mat/mat095/sigeps95.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTTOLOC(NEL,FP,
     .                         R1X, R1Y, R1Z,R2X,R2Y,R2Z,R3X,R3Y,R3Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,  INTENT(IN) ::  NEL

      my_real, DIMENSION(NEL), INTENT(IN) :: R1X(NEL),R1Y(NEL),R1Z(NEL),
     .   R2X(NEL), R2Y(NEL),R2Z(NEL),R3X(NEL),R3Y(NEL),R3Z(NEL)

      my_real, DIMENSION(NEL), INTENT(INOUT) :: FP(NEL,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   SX,SY,SZ,FXX,FXY,FXZ,FYX,FYY,FYZ,FZX,FZY,FZZ
C-----------------------------------------------
      DO I=1,NEL
         SX = FP(I,1,1)*R1X(I)+FP(I,2,1)*R1Y(I)+FP(I,3,1)*R1Z(I)
         SY = FP(I,1,2)*R1X(I)+FP(I,2,2)*R1Y(I)+FP(I,3,2)*R1Z(I)
         SZ = FP(I,1,3)*R1X(I)+FP(I,2,3)*R1Y(I)+FP(I,3,3)*R1Z(I)
         FXX = SX*R1X(I)+SY*R1Y(I)+SZ*R1Z(I)
         FXY = SX*R2X(I)+SY*R2Y(I)+SZ*R2Z(I)
         FXZ = SX*R3X(I)+SY*R3Y(I)+SZ*R3Z(I)
         SX = FP(I,1,1)*R2X(I)+FP(I,2,1)*R2Y(I)+FP(I,3,1)*R2Z(I)
         SY = FP(I,1,2)*R2X(I)+FP(I,2,2)*R2Y(I)+FP(I,3,2)*R2Z(I)
         SZ = FP(I,1,3)*R2X(I)+FP(I,2,3)*R2Y(I)+FP(I,3,3)*R2Z(I)
         FYX = SX*R1X(I)+SY*R1Y(I)+SZ*R1Z(I)
         FYY = SX*R2X(I)+SY*R2Y(I)+SZ*R2Z(I)
         FYZ = SX*R3X(I)+SY*R3Y(I)+SZ*R3Z(I)
         SX = FP(I,1,1)*R3X(I)+FP(I,2,1)*R3Y(I)+FP(I,3,1)*R3Z(I)
         SY = FP(I,1,2)*R3X(I)+FP(I,2,2)*R3Y(I)+FP(I,3,2)*R3Z(I)
         SZ = FP(I,1,3)*R3X(I)+FP(I,2,3)*R3Y(I)+FP(I,3,3)*R3Z(I)
         FZX = SX*R1X(I)+SY*R1Y(I)+SZ*R1Z(I)
         FZY = SX*R2X(I)+SY*R2Y(I)+SZ*R2Z(I)
         FZZ = SX*R3X(I)+SY*R3Y(I)+SZ*R3Z(I)
         FP(I,1,1)=FXX
         FP(I,1,2)=FXY
         FP(I,1,3)=FXZ
         FP(I,2,1)=FYX
         FP(I,2,2)=FYY
         FP(I,2,3)=FYZ
         FP(I,3,1)=FZX
         FP(I,3,2)=FZY
         FP(I,3,3)=FZZ
      ENDDO
C-----------
      RETURN
      END SUBROUTINE ROTTOLOC
Chd|====================================================================
Chd|  ROTTOGLOB                     source/materials/mat/mat095/sigeps95.F
Chd|-- called by -----------
Chd|        SIGEPS100                     source/materials/mat/mat100/sigeps100.F
Chd|        SIGEPS95                      source/materials/mat/mat095/sigeps95.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTTOGLOB(NEL,FP,
     .                         R1X, R1Y, R1Z,R2X,R2Y,R2Z,R3X,R3Y,R3Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,  INTENT(IN) ::  NEL

      my_real, DIMENSION(NEL), INTENT(IN) :: R1X(NEL),R1Y(NEL),R1Z(NEL),
     .   R2X(NEL), R2Y(NEL),R2Z(NEL),R3X(NEL),R3Y(NEL),R3Z(NEL)

      my_real, DIMENSION(NEL), INTENT(INOUT) :: FP(NEL,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   SX,SY,SZ,FXX,FXY,FXZ,FYX,FYY,FYZ,FZX,FZY,FZZ
C-----------------------------------------------
      DO I=1,NEL
         SX = FP(I,1,1)*R1X(I)+FP(I,2,1)*R2X(I)+FP(I,3,1)*R3X(I)
         SY = FP(I,1,2)*R1X(I)+FP(I,2,2)*R2X(I)+FP(I,3,2)*R3X(I)
         SZ = FP(I,1,3)*R1X(I)+FP(I,2,3)*R2X(I)+FP(I,3,3)*R3X(I)
         FXX = SX*R1X(I)+SY*R2X(I)+SZ*R3X(I)
         FXY = SX*R1Y(I)+SY*R2Y(I)+SZ*R3Y(I)
         FXZ = SX*R1Z(I)+SY*R2Z(I)+SZ*R3Z(I)
         SX = FP(I,1,1)*R1Y(I)+FP(I,2,1)*R2Y(I)+FP(I,3,1)*R3Y(I)
         SY = FP(I,1,2)*R1Y(I)+FP(I,2,2)*R2Y(I)+FP(I,3,2)*R3Y(I)
         SZ = FP(I,1,3)*R1Y(I)+FP(I,2,3)*R2Y(I)+FP(I,3,3)*R3Y(I)
         FYX = SX*R1X(I)+SY*R2X(I)+SZ*R3X(I)
         FYY = SX*R1Y(I)+SY*R2Y(I)+SZ*R3Y(I)
         FYZ = SX*R1Z(I)+SY*R2Z(I)+SZ*R3Z(I)
         SX = FP(I,1,1)*R1Z(I)+FP(I,2,1)*R2Z(I)+FP(I,3,1)*R3Z(I)
         SY = FP(I,1,2)*R1Z(I)+FP(I,2,2)*R2Z(I)+FP(I,3,2)*R3Z(I)
         SZ = FP(I,1,3)*R1Z(I)+FP(I,2,3)*R2Z(I)+FP(I,3,3)*R3Z(I)
         FZX = SX*R1X(I)+SY*R2X(I)+SZ*R3X(I)
         FZY = SX*R1Y(I)+SY*R2Y(I)+SZ*R3Y(I)
         FZZ = SX*R1Z(I)+SY*R2Z(I)+SZ*R3Z(I)

         FP(I,1,1)=FXX
         FP(I,1,2)=FXY
         FP(I,1,3)=FXZ
         FP(I,2,1)=FYX
         FP(I,2,2)=FYY
         FP(I,2,3)=FYZ
         FP(I,3,1)=FZX
         FP(I,3,2)=FZY
         FP(I,3,3)=FZZ
      ENDDO
C-----------
      RETURN
      END SUBROUTINE ROTTOGLOB

