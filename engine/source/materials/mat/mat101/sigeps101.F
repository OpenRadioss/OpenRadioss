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
C polypropylene - Bouvard law 
Chd|====================================================================
Chd|  SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        ATXB_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        AT_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|        AXBT_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        AXB_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        DET_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        DEV_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        EIGSRT                        source/materials/mat/mat101/sigeps101.F
Chd|        EMOD_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        ERR_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        INV_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        MAT3X3TOVEC6X1                source/materials/mat/mat101/sigeps101.F
Chd|        QAQT_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        QTAQ_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        SKW_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        SP_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|        SST_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        ST_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|        SYM_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        TR_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|        VALPROP_TPU                   source/materials/mat/mat101/sigeps101.F
Chd|        VEC6X1TOMAT3X3                source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
       SUBROUTINE SIGEPS101(
     1      NEL    , NUPARAM, NUVAR   , NFUNC  , IFUNC , 
     2      NPF    ,TF      , TIME    , TIMESTEP, UPARAM, 
     3      RHO0  , RHO     ,VOLUME   , EINT   , NGL     ,
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     C      SOUNDSP, VISCMAX, UVAR    , OFF   , ISMSTR, ET  ,
     D      IHET   ,OFFG    , EPSTH, IEXPAN   ,TEMPEL,
     1      FPSXX    , FPSXY  , FPSXZ  , FPSYX  ,    
     2      FPSYY  ,FPSYZ    , FPSZX  , FPSZY  , FPSZZ  , 
     3      UPSXX  ,UPSYY    , UPSZZ  , UPSXY  , UPSYZ  ,
     4      UPSXZ  )   
C
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
#include "param_c.inc"
#include "scr05_c.inc"
#include "impl1_c.inc"
#include "units_c.inc"
#include "com01_c.inc"
#include "scr18_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL,  NVARF,   NUPARAM, NUVAR,ISMSTR,NGL(*),IHET,IEXPAN
      my_real
     .      TIME       , TIMESTEP   , UPARAM(NUPARAM),UPARAMF(NUPARAM),
     .      RHO   (NEL), RHO0  (NEL), VOLUME(NEL), EINT(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),OFFG(NEL),
     .      EPSTH(NEL) ,TEMPEL(NEL),
     .   FPSXX(NEL),FPSYY(NEL),FPSZZ(NEL),
     .   FPSXY(NEL),FPSYX(NEL),FPSXZ(NEL),
     .   FPSZX(NEL),FPSYZ(NEL),FPSZY(NEL),
     .   UPSXX(NEL),UPSYY(NEL),UPSZZ(NEL),
     .   UPSXY(NEL),UPSYZ(NEL),UPSXZ(NEL)
C----------------------------------------------- 
     
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
     .      UVAR(NEL,NUVAR), OFF(NEL) 
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I,J,KK,LL,N
      INTEGER IE,NROT,IER,NDI,NSHR,NTENS,SIGN
 
      my_real
     .   ET1,ET2,ET3,G,RBULK,AA,CC,SB, 
C
     .   BI1(NEL),BI2(NEL),JDET(NEL),I1(NEL),IP1(NEL),GAMMAOLD(NEL),STIFF(NEL), 
     .   TEMP(NEL),
C
     .   NB(NEL,3),SNINV(NEL,3,3)
C----------------------------------------------------------------

      LOGICAL DEBUG, DEBUG1, DEBUG2, DEBUG3, DEBUG4, DEBUG5
      
      my_real
     . TIMEA(2),DTIME,F(3,3), F0(3,3), U0(3,3), F1(3,3), U1(3,3), 
     . TENSOR1(3,3), TENSOR2(3,3), TENSOR3(3,3), TENSOR4(3,3), 
     . TENSOR5(3,3),
     . TENS0(3,3), TENS1(3,3),
     . RDFGRD(3,3), VGRAD(3,3),
     . D_UMAT(3,3), W_UMAT(3,3),
     . ROTE0(3,3), ROTE1(3,3), ROT0(3,3), ROT1(3,3),
     . BETAM(3,3), BETAM0(3,3), ALPHAM(3,3),
     . DEV_ALPHAM(3,3), DEV_xM(3,3),
     . TAUM(3,3), SIGMA_Q(3,3),
     . FVM(3,3), FVM_INV(3,3), BP(3,3), 
     . FTHETA(3,3), FTHETA_INV(3,3),
     . FE0(3,3), FI_INV(3,3), CE0(3,3), UE0(3,3),
     . UE0_INV(3,3), EE0(3,3), XM(3,3), SIGMAM(3,3),
     . DEV_MEFF(3,3), DV(3,3), TEMP1(3,3), XMEFF(3,3),
     . TEMP1_INV(3,3), TEMP2(3,3), TEMP2_INV(3,3),
     . EXP_DVDT(3,3), FVM_ITER(3,3),
     . FE1(3,3), CE1(3,3), UE1(3,3),
     . UE1_INV(3,3), EE1(3,3),
     . VCE0(3,3), DCE0(3),
     . VCE1(3,3), DCE1(3),
     . A(3,3),   
c
     . FV(6), SIGMA(6),
     . BETA(6),
     . DEV_MEFF_VEC(6), NV_VEC(6),
     . DV_VEC(6), TAU(6),
     . FV_ITER(6), XM_VEC(6), D_UMAT_VEC(6),
c      
     . J1, T1, T2, T3,
     . DEREF, DE0, DE, DPOISS,
     . DEDOT_REF, DVE1, DVE2,
     . GAMV_REF, DQ, DV1, DC3, DC4, CALPHAK1,
     . CALPHAK2, DC5, DC6, DC7, DC8, DC9, DC10,
     . DC11, DC12, DC13, DC14, DC1, DC2,
     . THETA0, BETA0, BTHETA, FACTOR, CV, RHO_P,
     . THETA, THETADOT, DTHETA_ITER, THETADOTITER,
     . THETAITER, THETAG,
     . D_THETA_OPT, THETAI,
     . DMU, DK, ALPHAP, DM, DY0,
     . CKAPPA1, CKAPPA2 ,
     . H0, DES1_0, DESTAR_0, DESTAR_S, G0,
     . H1, DES2_0, DES2_S, TR_BP,
     . DES10, DES20, DESTAR0, DLAMBDAP_BAR,
     . DMU_R, DLAMBDA_L, DRS1,
     . TWOMU0, BULKA0,
     . DES1, DESTAR, DES2, DKAPPA1, DKAPPA2,
     . GAMV, GAMVEQ, DKAPPA, TR_ALPHA,
     . DET_F0, DET_F1, DET, DET_FE,
     . DMU_B, KSI_B, DET_BETA,
     . EQSTRESS, PRESSURE, NORM_DEVMEFF, TR_M,
     . DGAMV_ITER, DET_VCE0,
     . GAMV_ITER, DET_VCE1, D1, D2, D3,
     . DE1, DE2, DE3,
     . TR_EE1, TR_EE0, COEFF1_A, COEFF1_B,
     . COEFF1, COEFF2, COEFF3, EXCHANGE, GAMV0, ROT, ps
     
      my_real
     .  ONE, TWO, THREE, FOUR, FIVE,  SEVEN,
     . FIFTEEN, TSEVEN, HALF, SMALL, BIG, TWOTHIRD, DS32, DS3, DS13, DKB, 
     . DR

      my_real
     .   PIDENT(6), FACTOTWO(6), FACTOHALF(6)
      my_real
     .   PMIDENT(3,3)
C
      DATA    PMIDENT   /1.D0,  0.D0,  0.D0, 0.D0,  1.D0,  0.D0,
     &                   0.D0,  0.D0,  1.D0/
      DATA    PIDENT    /1.D0, 1.D0, 1.D0, 0.D0,  0.D0,  0.D0/
      DATA    FACTOTWO  /1.D0, 1.D0, 1.D0, 2.D0,  2.D0,  2.D0/
      DATA    FACTOHALF /1.D0, 1.D0, 1.D0, 0.5D0, 0.5D0, 0.5D0/
C-----------------------------------------------	  
C
      	NDI= 3
C----------------------------------------------- 
C   TIMESTEP
 

	     DTIME = TIMESTEP
      ONE     = UN
	     TWO     = DEUX
      THREE   = TROIS
      FOUR    = QUATRE
      FIVE    = CINQ

      SEVEN   = SEPT
      FIFTEEN = DEUX*SEPT+UN
      TSEVEN  = DEUX*DIX+SEPT
      HALF    = UNDEMI
      SMALL   = EM6
      BIG     = EP16
      TWOTHIRD= TWO/THREE

      DS32    = SQRT(THREE/TWO)
      DS3     = SQRT(THREE)
      DS13    = SQRT(ONE/THREE)
C---- MATERIAL CONSTANTS
C
      DKB     = 1.3806*EM30 ! boltzmann constant
      !DR      = 8.31*EM3
C
C---- DEBUG
C
      DEBUG = .FALSE.  !STEP0
      DEBUG1= .FALSE.  !STEP1 DATA  
      DEBUG2= .FALSE.  !STEP1 BRANCHA_0
      DEBUG3= .FALSE.  !STEP1 VISCOPLASTICITY
      DEBUG4= .FALSE.  !STEP1 BRANCHB_1
      DEBUG5= .FALSE.  !STEP1 BRANCHA_1 & STRESS
      DEREF      = UPARAM(1)
      DE0        = UPARAM(2)
      DPOISS     = UPARAM(3)
      DVE1       = UPARAM(4)
      DVE2       = UPARAM(5)
      DEDOT_REF  = UPARAM(6)
      GAMV_REF   = UPARAM(7) 
      ALPHAP     = UPARAM(8) 
      DQ         = UPARAM(9)
      DV1        = UPARAM(10)
      DM         = UPARAM(11)
      DC3        = UPARAM(12)
      DC4        = UPARAM(13)
      CALPHAK1   = UPARAM(14)
      CALPHAK2   = UPARAM(15)
      H0         = UPARAM(16)
      DES1_0     = UPARAM(17)
      DC5        = UPARAM(18)
      DC6        = UPARAM(19)
      DC7        = UPARAM(20)
      DC8        = UPARAM(21)
      DC9        = UPARAM(22)
      DC10       = UPARAM(23)
      H1         = UPARAM(24)
      DES2_0     = UPARAM(25)
      DC11       = UPARAM(26)
      DC12       = UPARAM(27)
      DC13       = UPARAM(28)
      DC14       = UPARAM(29)
      DC1        = UPARAM(30)
      DC2        = UPARAM(31)
      DLAMBDA_L  = UPARAM(32)
      RHO_P        = UPARAM(33)
      CV         = UPARAM(34)
      THETA0     = UPARAM(35)
      BETA0      = UPARAM(36)
      !BTHETA     = UPARAM(37)
      FACTOR     = UPARAM(38)
      D_THETA_OPT = UPARAM(39)
      THETAI     = UPARAM(40)
      THETAG     = UPARAM(41)  
      DR         = UPARAM(42)  
 
      
c------------------------------------------------------

C----------------------------------------------- 
C     RECUPERER CHAMP TEMPERATURE 
C-----------------------------------------------       
      TEMP(1:NEL)   = TEMPEL(1:NEL)
C----------------------------------------------- 
C     USER VARIABLES INITIALIZATION 
C----------------------------------------------- 
 

      IF ( (TIME .EQ. ZERO) ) THEN
 
C
C     LOOP THROUGH ALL BLOCKS
C
      DO IE = 1, NEL
 
C
C------ TEMPERATURE DEPENDENCE 1
C        
        DESTAR_0 = DC5*(THETAI-THETA0)+ DC6
        DMU_R    = DC1*(THETAI-THETA0)+ DC2
C
C-------- COMPUTE DMU_B
C
        DMU_B = DMU_R
C
C-------- INITIALIZE STATE VARIABLES
C
        UVAR(IE, 1) = UN !FV11
        UVAR(IE, 2) = UN !FV22
        UVAR(IE, 3) = UN !FV33
        UVAR(IE, 4) = ZERO !FV12
        UVAR(IE, 5) = ZERO !FV23
        UVAR(IE, 6) = ZERO !FV13
        UVAR(IE, 7) = DES1_0 !DES1
        UVAR(IE, 8) = DESTAR_0 !DESTAR   
        UVAR(IE, 9) = DES2_0 !DES2
        UVAR(IE,10) = UN  !BETA11
        UVAR(IE,11) = UN  !BETA22
        UVAR(IE,12) = UN  !BETA33
        UVAR(IE,13) = ZERO  !BETA12
        UVAR(IE,14) = ZERO  !BETA23
        UVAR(IE,15) = ZERO  !BETA13
        UVAR(IE,16) = DMU_B  !DMU_B
        UVAR(IE,17) = ZERO  !GAMV
        UVAR(IE,18) = ZERO  !DGAMV
        UVAR(IE,19) = ZERO  !GAMVEQ
        UVAR(IE,20) = THETAI
        IF (ABS(D_THETA_OPT-UN) .LT. EM06) THEN
            UVAR(IE,20)  = TEMP(IE)
        ENDIF
        UVAR(IE,21) = ZERO !THETADOT (ADIABATIC)
		
        UVAR(IE,22) = UN !F011
        UVAR(IE,23) = UN !F022
        UVAR(IE,24) = UN !F033
        UVAR(IE,25) = ZERO !F012
        UVAR(IE,26) = ZERO !F021
        UVAR(IE,27) = ZERO !F013
        UVAR(IE,28) = ZERO !F031
        UVAR(IE,29) = ZERO !F023
        UVAR(IE,30) = ZERO !F032
		
        UVAR(IE,31) = UN !U011
        UVAR(IE,32) = UN !U022
        UVAR(IE,33) = UN !U033
        UVAR(IE,34) = ZERO !U012
        UVAR(IE,35) = ZERO !U021
        UVAR(IE,36) = ZERO !U013
        UVAR(IE,37) = ZERO !U031
        UVAR(IE,38) = ZERO !U023
        UVAR(IE,39) = ZERO !U032		
	       UVAR(IE,40) = ZERO !E133 true strain xx
        UVAR(IE,41) = ZERO !E133 true strain yy
        UVAR(IE,42) = ZERO !E133 true strain zz

C
C------ INTERNAL STATE VARIABLES AT BEGINNING OF STEP
       
        FV(1)  = UVAR(IE, 1)
        FV(2)  = UVAR(IE, 2)
        FV(3)  = UVAR(IE, 3)
        FV(4)  = UVAR(IE, 4)
        FV(5)  = UVAR(IE, 5)
        FV(6)  = UVAR(IE, 6)
        THETA  = UVAR(IE, 20)
C
C----- setup F1; U1   
C        
        F1(1,1) = FPSXX(IE)
        F1(2,2) = FPSYY(IE)
        F1(3,3) = FPSZZ(IE)
        F1(1,2) = FPSXY(IE)
        F1(2,1) = FPSYX(IE)
        F1(1,3) = FPSXZ(IE)
        F1(3,1) = FPSZX(IE)
        F1(2,3) = FPSYZ(IE)
        F1(3,2) = FPSZY(IE)
	  
        U1(1,1) = UPSXX(IE)
        U1(2,2) = UPSYY(IE)
        U1(3,3) = UPSZZ(IE)
        U1(1,2) = UPSXY(IE)
        U1(2,1) = UPSXY(IE)
        U1(1,3) = UPSXZ(IE)
        U1(3,1) = UPSXZ(IE)
        U1(2,3) = UPSYZ(IE)
        U1(3,2) = UPSYZ(IE) 
C----   COMPUTE DROT_ABA
C
C-----  --- ROTATION TENSORS FROM F=RU, I.E., R=F*U^(-1)
C
        CALL ST_TPU(TENSOR1, ZERO, 3*3)
        CALL INV_TPU(U1, TENSOR1, DET)
        CALL AXB_TPU(F1, TENSOR1, ROT1, 3)
      
       !--------------------------------------------------------------
       !-------- COMPUTE ELASTIC STRESSES
       !--------------------------------------------------------------
       !-------- TEMPERATURE DEPENDENCE OF PARAMETERS
       !---(STRAIN RATE DEPENDENCE IS NEGLECTED FOR TIME=0)
       !

        CALL EMOD_TPU(DEREF,DE0,THETA,THETA0,DVE1,DVE2,
     &                EM04,DEDOT_REF,DE)

        DMU  = DE / (DEUX+DEUX*DPOISS)
        DK   = DEUX*DMU*(UN+DPOISS)/(TROIS*(UN-DEUX*DPOISS))

        !CALCULATE SOME CONSTANTS

        TWOMU0    = DEUX*DMU
        BULKA0    = DK - (DEUX/3.D0)*DMU

        !COMPUTE: FE->CE->UE->RE->EE
        CALL ST_TPU(FVM,    ZERO, 3*3)
        CALL ST_TPU(FVM_INV,ZERO, 3*3)
        CALL ST_TPU(TENS0,  ZERO, 3*3)
        CALL ST_TPU(FE1,    ZERO, 3*3)
        CALL ST_TPU(CE1,    ZERO, 3*3)
        CALL ST_TPU(UE1,    ZERO, 3*3)
        CALL ST_TPU(UE1_INV,ZERO, 3*3)
        CALL ST_TPU(ROTE1,  ZERO, 3*3)
        CALL ST_TPU(EE1,    ZERO, 3*3)


        !DETERMINATION OF FE AND CE
        CALL VEC6X1TOMAT3X3(FV, FVM,3)
        CALL INV_TPU(FVM, FVM_INV, DET)
        CALL AXB_TPU(F1, FVM_INV, FE1, 3)
        CALL ATXB_TPU(FE1, FE1, CE1, 3)
C
        DO I=1,NDI
            DO J=1,NDI
              A(I,J)= CE1(I,J)
            ENDDO
        ENDDO 
C
        CALL VALPROP_TPU(A, 3, 3, DCE1, VCE1, NROT, IER)
C
        CALL EIGSRT(DCE1, VCE1, 3, 3) 
C
        IF (IER.EQ.1) THEN
        CALL ERR_TPU('ERROR IN EIG VAL')
        ENDIF
       !EIGENVALUES (AND ASSOC EIGENVECTORS) ARE ORDERED FROM 
       !LARGER TO SMALLER.
       !REDEFINE AXIS(2) TO BE THE LARGEST IN ORDER TO IMPROVE 
       !ACCURACY IN THE
       !CALCULATION OF THE ESHELBY TENSOR.
       !IF DET(B)<0 MEANS THAT THE SYSTEM IS LEFT HANDED. IT IS MADE
       !RIGHT
       !HANDED BY EXCHANGING 1 AND 2.
        SIGN=-UN
        CALL DET_TPU(VCE1, DET_VCE1)
        IF(DET_VCE1 .LE. ZERO) SIGN=UN
           DO I=1,3
              EXCHANGE=VCE1(I,1)
              VCE1(I,1)=VCE1(I,2)
              VCE1(I,2)=EXCHANGE*SIGN
            ENDDO
            EXCHANGE=DCE1(1)
            DCE1(1)=DCE1(2)
            DCE1(2)=EXCHANGE
           !COMPUTE UE1
            D1=SQRT(DCE1(1))
            D2=SQRT(DCE1(2))
            D3=SQRT(DCE1(3))
            DE1=LOG(SQRT(DCE1(1)))
            DE2=LOG(SQRT(DCE1(2)))
            DE3=LOG(SQRT(DCE1(3)))
            DO I=1,3
               DO J=1,3
               UE1(I,J)=D1*VCE1(I,1)*VCE1(J,1)
     &         +D2*VCE1(I,2)*VCE1(J,2)+D3*VCE1(I,3)*VCE1(J,3)
               END DO
            END DO
            DO I=1,3
               DO J=1,3
               EE1(I,J)=DE1*VCE1(I,1)*VCE1(J,1)
     &         +DE2*VCE1(I,2)*VCE1(J,2)+DE3*VCE1(I,3)*VCE1(J,3)
               END DO
            END DO
C
	       	   CALL INV_TPU(UE1, UE1_INV, DET)
            CALL AXB_TPU(FE1, UE1_INV, ROTE1, 3)
            CALL DET_TPU(FE1, DET_FE)
C
            !DETERMINATION OF TAU
C
            CALL ST_TPU(XM,      ZERO, 3*3)
            CALL ST_TPU(TAUM,    ZERO, 3*3)
C
            CALL TR_TPU(EE1, TR_EE1, 3)
            COEFF1_A       = TWOMU0
            COEFF1_B       = BULKA0*TR_EE1
            CALL AT_TPU(COEFF1_A, EE1, COEFF1_B, PMIDENT, XM, 9)
            CALL QAQT_TPU(ROTE1, XM, TAUM, 3)
        
C
            !DETERMINATION OF SIGMA
C
            CALL ST_TPU(SIGMAM,     ZERO, 3*3)
            CALL ST_TPU(SIGMA_Q,    ZERO, 3*3)
C
            CALL SST_TPU(UN/DET_FE, TAUM, SIGMAM, 3*3)
C
C-------    DETERMINATION OF SIGMA  (IN THE ROTATED CONFIGURATION)
            CALL QTAQ_TPU(ROT1, SIGMAM, SIGMA_Q, 3)
C
C-------    SIGMA_Q(I,J)
            UVAR(IE,22) = F1(1,1) !F011
            UVAR(IE,23) = F1(2,2) !F022
            UVAR(IE,24) = F1(3,3) !F033
            UVAR(IE,25) = F1(1,2) !F012
            UVAR(IE,26) = F1(2,1) !F021
            UVAR(IE,27) = F1(1,3) !F013
	       UVAR(IE,28) = F1(3,1) !F031
            UVAR(IE,29) = F1(2,3) !F023
            UVAR(IE,30) = F1(3,2) !F032
            UVAR(IE,31) = U1(1,1) !U011
            UVAR(IE,32) = U1(2,2) !U022
            UVAR(IE,33) = U1(3,3) !U033
            UVAR(IE,34) = U1(1,2) !U012
            UVAR(IE,35) = U1(2,1) !U021
            UVAR(IE,36) = U1(1,3) !U013
            UVAR(IE,37) = U1(3,1) !U031
            UVAR(IE,38) = U1(2,3) !U023
            UVAR(IE,39) = U1(3,2) !U032
            UVAR(IE,40) = LOG(U1(1,1)) !E111
            UVAR(IE,41) = LOG(U1(2,2)) !E122
            UVAR(IE,42) = LOG(U1(3,3)) !E133
            SIGNXX(IE) = SIGOXX(IE)
            SIGNYY(IE) = SIGOYY(IE)  
            SIGNZZ(IE) = SIGOZZ(IE)    
            SIGNXY(IE) = SIGOXY(IE)  
            SIGNYZ(IE) = SIGOYZ(IE)   
            SIGNZX(IE) = SIGOZX(IE)     
 
            SOUNDSP(IE) = SQRT((DK+4*DMU/3)/RHO0(IE)) 
            VISCMAX(IE) = ZERO     

      ENDDO
C
      RETURN
C
      ENDIF ! ( (TIME .EQ. ZERO) )
C
 
C
C---------------------------------------------
C---- EVOLVE MATERIAL STATE - COMPUTE STRESSES
C---------------------------------------------
C
C---- LOOP OVER ELEMENTS/IPS TO UPDATE STRESS
C
 
      DO 20 IE = 1, NEL
C
       !INTERNAL STATE VARIABLES AT BEGINNING OF STEP
        FV(1)   = UVAR(IE, 1)
        FV(2)   = UVAR(IE, 2)
        FV(3)   = UVAR(IE, 3)
        FV(4)   = UVAR(IE, 4)
        FV(5)   = UVAR(IE, 5)
        FV(6)   = UVAR(IE, 6)
        DES10   = UVAR(IE, 7) 
        DESTAR0 = UVAR(IE, 8)
        DES20   = UVAR(IE, 9) 
        BETA(1) = UVAR(IE,10)
        BETA(2) = UVAR(IE,11)
        BETA(3) = UVAR(IE,12)
        BETA(4) = UVAR(IE,13)
        BETA(5) = UVAR(IE,14)
        BETA(6) = UVAR(IE,15)
        DMU_B    = UVAR(IE,16)
        GAMV     = UVAR(IE, 17)
        GAMVEQ   = UVAR(IE, 19)
        THETA    = UVAR(IE, 20)
        THETADOT = UVAR(IE, 21)
        IF (ABS(D_THETA_OPT-ONE) .LT. SMALL) THETA=TEMP(IE)
C
C-----  SETUP F0; U0; F1; U1; 
        CALL ST_TPU(F0, ZERO, 3*3)  
        CALL ST_TPU(U0, ZERO, 3*3)  
        CALL ST_TPU(F1, ZERO, 3*3)  
        CALL ST_TPU(U1, ZERO, 3*3)  
C

C
C-----  setup F0; U0, F1, U1
C        
        F0(1,1) = UVAR(IE,22)
        F0(2,2) = UVAR(IE,23)
        F0(3,3) = UVAR(IE,24)
        F0(1,2) = UVAR(IE,25)
        F0(2,1) = UVAR(IE,26)
        F0(1,3) = UVAR(IE,27)
        F0(3,1) = UVAR(IE,28)
        F0(2,3) = UVAR(IE,29)
        F0(3,2) = UVAR(IE,30)
      
        U0(1,1) = UVAR(IE,31)
        U0(2,2) = UVAR(IE,32)
        U0(3,3) = UVAR(IE,33)
        U0(1,2) = UVAR(IE,34)
        U0(2,1) = UVAR(IE,35)
        U0(1,3) = UVAR(IE,36)
        U0(3,1) = UVAR(IE,37)
        U0(2,3) = UVAR(IE,38)
        U0(3,2) = UVAR(IE,39)

        F1(1,1) = FPSXX(IE)
        F1(2,2) = FPSYY(IE)
        F1(3,3) = FPSZZ(IE)
        F1(1,2) = FPSXY(IE)
        F1(2,1) = FPSYX(IE)
        F1(1,3) = FPSXZ(IE)
        F1(3,1) = FPSZX(IE)
        F1(2,3) = FPSYZ(IE)
        F1(3,2) = FPSZY(IE)
      
        U1(1,1) = UPSXX(IE)
        U1(2,2) = UPSYY(IE)
        U1(3,3) = UPSZZ(IE)
        U1(1,2) = UPSXY(IE)
        U1(2,1) = UPSXY(IE)
        U1(1,3) = UPSXZ(IE)
        U1(3,1) = UPSXZ(IE)
        U1(2,3) = UPSYZ(IE)
        U1(3,2) = UPSYZ(IE)
! C
C------ !COMPUTE L,D,W,DROT FROM F_N & F_(N+1)
C
C-------!RELATIVE DEFORMATION GRADIENT: F=F_(N+1)*F_N^{-1}
C
        CALL ST_TPU(TENSOR1,  ZERO, 3*3)                             
        CALL ST_TPU(RDFGRD,   ZERO, 3*3)                             
        CALL ST_TPU(TENSOR2,  ZERO, 3*3)                             
        CALL ST_TPU(TENSOR3,  ZERO, 3*3)                             
        CALL ST_TPU(VGRAD,    ZERO, 3*3)                             
C
        CALL INV_TPU(F0, TENSOR1, DET)                               
        CALL AXB_TPU(F1, TENSOR1, RDFGRD, 3)                         
C
C------- APPROXIMATION TO VELOCITY GRADIENT: L=2/DT*(F-I)*(F+I)^{-1}-
C
        CALL AT_TPU(ONE, RDFGRD, -ONE, PMIDENT, TENSOR2, 3*3)        
        CALL AT_TPU(ONE, RDFGRD, ONE, PMIDENT, TENSOR1, 3*3)         
        CALL INV_TPU(TENSOR1, TENSOR3, DET)                          
        CALL AXB_TPU(TENSOR2, TENSOR3, VGRAD, 3)                     
        CALL SST_TPU(DEUX/DTIME, VGRAD, VGRAD, 3*3)                  
C
C------- RATE OF DEFORMATION AND SPIN: D_UMAT=SYM(L), W_UMAT=SKW(L) -
C
        CALL ST_TPU(D_UMAT, ZERO, 3*3)                               
        CALL ST_TPU(W_UMAT, ZERO, 3*3)                               
C
        CALL SYM_TPU(VGRAD, D_UMAT, 3)                               
        CALL SKW_TPU(VGRAD, W_UMAT, 3)                               
C
C------- COMPUTE EDOT=SQRT(2/3*D:D)                                 -
C
        CALL ST_TPU(D_UMAT_VEC, ZERO, 6)                             
C
        CALL MAT3X3TOVEC6X1(D_UMAT,D_UMAT_VEC,3)                     
        CALL SP_TPU(D_UMAT_VEC,D_UMAT_VEC,COEFF3,6)                  
        COEFF2 = SQRT((DEUX/TROIS)*COEFF3)                           
C
C-------- ROTATION TENSORS FROM F=RU, I.E., R=F*U^(-1)
C
        CALL ST_TPU(ROT1, ZERO, 3*3)                                
        CALL ST_TPU(TENSOR1, ZERO, 3*3)                             
C
        CALL INV_TPU(U1,  TENSOR1, DET)                             
        CALL AXB_TPU(F1,  TENSOR1, ROT1, 3)                         
C
C------- TEMPERATURE DEPENDENCE OF PARAMETERS                      -
        IF (COEFF2 .LT. SMALL) THEN                                 
            COEFF2 = EM04                                           
        ENDIF                                                       
C
        CALL EMOD_TPU(DEREF,DE0,THETA,THETA0,DVE1,DVE2,             
     &                COEFF2,DEDOT_REF,DE)                          
        DMU  = DE / (DEUX+DEUX*DPOISS)                              
        DK   = DEUX*DMU*(UN+DPOISS)/(TROIS*(UN-DEUX*DPOISS))        
        DY0  = DC3*(THETA-THETA0)+DC4                               
        CKAPPA1 = CALPHAK1*DMU                                      
        DESTAR_0 = DC5*(THETA-THETA0)+DC6                           
        DESTAR_S = DC7*(THETA-THETA0)+DC8                           
        G0       = DC9*(THETA-THETA0)+DC10                          
        CKAPPA2 = CALPHAK2*DMU                                      
        DES2_S  = DC11*(THETA-THETA0)+DC12                          
        DMU_R = DC1*(THETA-THETA0)+DC2                              
        DRS1  = DC13*(THETA-THETA0)+ DC14                           
C
        !-----------------------------------------------------
        !CHECK PARAM
        !-----------------------------------------------------
        TWOMU0 = TWO*DMU
        BULKA0 = DK - TWOTHIRD*DMU
        !-------------------------------------------------------------72
        !- AT THE BEGINNING OF THE STEP
        !-------------------------------------------------------------72
C
        CALL ST_TPU(FVM,     ZERO, 3*3)
        CALL ST_TPU(FVM_INV, ZERO, 3*3)
        CALL ST_TPU(FTHETA, ZERO, 3*3)
        CALL ST_TPU(FTHETA_INV, ZERO, 3*3)
        CALL ST_TPU(TENS0,  ZERO, 3*3)
        CALL ST_TPU(TENS1,  ZERO, 3*3)
        CALL ST_TPU(FE0,     ZERO, 3*3)
        CALL ST_TPU(CE0,     ZERO, 3*3)
        CALL ST_TPU(UE0,     ZERO, 3*3)
        CALL ST_TPU(UE0_INV, ZERO, 3*3)
        CALL ST_TPU(ROTE0,   ZERO, 3*3)
        CALL ST_TPU(EE0,     ZERO, 3*3)
        CALL ST_TPU(XM,      ZERO, 3*3)
C
C       DETERMINATION OF BP
C
        CALL VEC6X1TOMAT3X3(FV,FVM,3)
        CALL AXBT_TPU(FVM, FVM, BP, 3)
        CALL TR_TPU(BP, TR_BP, 3)
C
C       DETERMINATION OF FE AND CE
C
        CALL SST_TPU(ONE+BETA0*(THETA-THETAI), PMIDENT, FTHETA, 3*3)
        CALL INV_TPU(FTHETA, FTHETA_INV, DET)
        CALL INV_TPU(FVM, FVM_INV, DET)
        CALL AXB_TPU(F0, FTHETA_INV, TENS0, 3)
        CALL AXB_TPU(TENS0, FVM_INV, FE0, 3)
        CALL ATXB_TPU(FE0, FE0, CE0, 3)
C
        DO I=1,NDI
            DO J=1,NDI
              A(I,J)= CE0(I,J)
            ENDDO
        ENDDO
        CALL VALPROP_TPU(A, 3, 3, DCE0, VCE0, NROT, IER)
        CALL EIGSRT(DCE0, VCE0, 3, 3)
        IF (IER.EQ.1) THEN
          CALL ERR_TPU('ERROR IN EIG VAL')
        ENDIF
       !EIGENVALUES (AND ASSOC EIGENVECTORS) ARE ORDERED FROM LARGER 
       !TO SMALLER.
       !REDEFINE AXIS(2) TO BE THE LARGEST IN ORDER TO IMPROVE ACCURACY
       !IN THE CALCULATION OF THE ESHELBY TENSOR.
       !IF DET(B)<0 MEANS THAT THE SYSTEM IS LEFT HANDED. IT IS MADE 
       !RIGHT HANDED BY EXCHANGING 1 AND 2.
        SIGN=-ONE
        CALL DET_TPU(VCE0, DET_VCE0)
        IF(DET_VCE0 .LE. ZERO) SIGN=ONE
        DO I=1,3                                               
          EXCHANGE=VCE0(I,1)                                   
          VCE0(I,1)=VCE0(I,2)                                  
          VCE0(I,2)=EXCHANGE*SIGN                              
        ENDDO                                                  
        EXCHANGE=DCE0(1)                                       
        DCE0(1)=DCE0(2)                                        
        DCE0(2)=EXCHANGE                                       
C
        !COMPUTE UE0                                           
        D1=SQRT(DCE0(1))                                       
        D2=SQRT(DCE0(2))                                       
        D3=SQRT(DCE0(3))                                       
        DE1=LOG(SQRT(DCE0(1)))                                 
        DE2=LOG(SQRT(DCE0(2)))                                 
        DE3=LOG(SQRT(DCE0(3)))                                 
        DO I=1,3                                               
           DO J=1,3                                            
           UE0(I,J)=D1*VCE0(I,1)*VCE0(J,1)                     
     &     +D2*VCE0(I,2)*VCE0(J,2)+D3*VCE0(I,3)*VCE0(J,3)      
           END DO                                              
        END DO                                                 
        DO I=1,3                                               
           DO J=1,3                                            
           EE0(I,J)=DE1*VCE0(I,1)*VCE0(J,1)                    
     &     +DE2*VCE0(I,2)*VCE0(J,2)+DE3*VCE0(I,3)*VCE0(J,3)    
           END DO                                              
        END DO                                                 
        CALL INV_TPU(UE0, UE0_INV, DET)                                    
        CALL AXB_TPU(FE0, UE0_INV, ROTE0, 3)                               
        CALL DET_TPU(FE0, DET_FE)                                          
C
C====   DETERMINATION OF MANDEL STRESS M                                ===
C
        CALL TR_TPU(EE0, TR_EE0, 3)                                        
        COEFF1_A       = TWOMU0                                            
        COEFF1_B       = BULKA0*TR_EE0                                     
C
        CALL AT_TPU(COEFF1_A, EE0, COEFF1_B, PMIDENT, XM, 9)               
C
       !-------------------------------------------------------------72    
       !- CHECK 2                                                          
       !- ELASTIC STRESSES AT THE BEGINNING OF THE STEP                    
       !-!------------------------------------------------------------72   
       !-------------------------------------------------------------72    
       !DETERMINATION OF FLOW RULE FOR TIME-DEPENDENT DASHPOT              
       !-------------------------------------------------------------72    
       !DETERMINATION OF EQSTRESS, PRESSURE, DIRECTION OF PLASTIC FLOW
        CALL ST_TPU(DEV_ALPHAM,   ZERO, 3*3)                             
        CALL ST_TPU(BETAM0,       ZERO, 3*3)                             
        CALL ST_TPU(DEV_XM,       ZERO, 3*3)                             
        CALL ST_TPU(ALPHAM,       ZERO, 3*3)                             
        CALL ST_TPU(XMEFF,        ZERO, 3*3)                             
        CALL ST_TPU(DEV_MEFF,     ZERO, 3*3)                             
        CALL ST_TPU(TEMP1,        ZERO, 3*3)                             
        CALL ST_TPU(TEMP1_INV,    ZERO, 3*3)                             
        CALL ST_TPU(TEMP2,        ZERO, 3*3)                             
        CALL ST_TPU(TEMP2_INV,    ZERO, 3*3)                             
        CALL ST_TPU(EXP_DVDT,     ZERO, 3*3)                             
        CALL ST_TPU(FVM_ITER,     ZERO, 3*3)                             
        CALL ST_TPU(DEV_MEFF_VEC, ZERO, 6)                               
        DO I = 1, 6!NTENS                                         
          NV_VEC(I)=PIDENT(I)                                     
        ENDDO                                                     
C
        CALL VEC6X1TOMAT3X3(BETA, BETAM0, 3)                      
        CALL SST_TPU(DMU_B, BETAM0, ALPHAM, 3*3)                  
        CALL DEV_TPU(ALPHAM, DEV_ALPHAM, 3)                       
        CALL DEV_TPU(XM, DEV_XM, 3)                               
        CALL AT_TPU(ONE, DEV_XM, -ONE, DEV_ALPHAM, DEV_MEFF, 9)   
        CALL MAT3X3TOVEC6X1(DEV_MEFF,DEV_MEFF_VEC,3)              
        CALL SP_TPU(DEV_MEFF_VEC,DEV_MEFF_VEC, ps, 6)             
        NORM_DEVMEFF = SQRT(ps)                                   
C
        DKAPPA1 = CKAPPA1*DES10                                   
        DKAPPA2 = CKAPPA2*DES20                                   
        DKAPPA  = DKAPPA1 + DKAPPA2                               
        CALL TR_TPU(XM, TR_M, 3)                                  
        PRESSURE    = -THIRD*TR_M                                 
        EQSTRESS = (NORM_DEVMEFF/SQRT(TWO))
     &              -(DY0+DKAPPA+ALPHAP*PRESSURE)
C
           !DETERMINATION OF GAMVDOT
C
        IF (EQSTRESS .LT. SMALL) THEN                                 
            DGAMV_ITER = ZERO                                         
C           WRITE(0 ,*) '!!!NO VISCOSITY!!!! ', DGAMV_ITER            
        ELSE                                                          
C        WRITE(0 ,*) '!!!VISCOSITY!!!! ', DGAMV_ITER                  
         CALL  SST_TPU(ONE/NORM_DEVMEFF, DEV_MEFF_VEC, NV_VEC,6)      
C
            GAMV0      = GAMV_REF*EXP(-DQ/(DR*THETA))                 
            DGAMV_ITER = GAMV0*DTIME*                                 
     &                   (SINH(EQSTRESS*DV1/(TWO*DKB*THETA)))**DM     
            IF (DGAMV_ITER .GT. EP06) THEN                            
                 CALL ERR_TPU('PLASTICITY .GT. 1.0D6!!!')             
             ENDIF                                                    
C
        ENDIF                                                         
C
C.......DETERMINATION OF INTERNAL STRAIN                   
C
        DESTAR     = DESTAR0                               
     &                + (DESTAR_S-G0*DESTAR0)*DGAMV_ITER   
C
        DES1       = DES10 + H0*                           
     &		       (ONE-DES10/DESTAR)*DGAMV_ITER               
C
        DLAMBDAP_BAR = ONE/DS3*SQRT(TR_BP)                 
        DES2         = DES20 + H1*(DLAMBDAP_BAR-ONE)*      
     &                 (ONE-DES20/DES2_S)*DGAMV_ITER       
C
C.......DETERMINATION OF BETA                              
C
        CALL ST_TPU(DV_VEC,   ZERO, 6)                                  
        CALL ST_TPU(DV,       ZERO, 3*3)                                
        CALL ST_TPU(TENSOR1,  ZERO, 3*3)                                
        CALL ST_TPU(TENSOR2,  ZERO, 3*3)                                
        CALL ST_TPU(TENSOR3,  ZERO, 3*3)                                
        CALL ST_TPU(TENSOR4,  ZERO, 3*3)                                
        CALL ST_TPU(TENSOR5,  ZERO, 3*3)                                
        CALL ST_TPU(BETAM,    ZERO, 3*3)                                
C
        CALL SST_TPU(DGAMV_ITER/(DTIME*SQRT(TWO)), NV_VEC, DV_VEC, 6)   
        CALL VEC6X1TOMAT3X3(DV_VEC, DV,3)                               
C
C       COMPUTE BETA_DOT                                                
C
        CALL AXB_TPU(DV, BETAM0, TENSOR1, 3)                            
        CALL AXB_TPU(BETAM0, DV, TENSOR2, 3)                            
        CALL AT_TPU(ONE, TENSOR1, ONE, TENSOR2, TENSOR3, 9)             
C
        CALL SST_TPU(DRS1,                                   
     &               TENSOR3, TENSOR4, 3*3)                  
        CALL AT_TPU(ONE, BETAM0, DTIME, TENSOR4, BETAM, 9)   
C
C       EIGEN VALUES AND VECTOR OF BETA                      
C
        DO I=1,NDI                                                             
            DO J=1,NDI                                                         
              A(I,J)= BETAM(I,J)                                               
            ENDDO                                                              
        ENDDO                                                                  
        CALL VALPROP_TPU(A, 3, 3, DCE1, VCE1, NROT, IER)                       
        CALL EIGSRT(DCE1, VCE1, 3, 3)                                          
        IF (IER.EQ.1) THEN                                                     
          CALL ERR_TPU('ERROR IN EIG VAL')                                     
        ENDIF                                                                  
        SIGN=-UN                                                               
        CALL DET_TPU(VCE1, DET_VCE1)                                           
        IF(DET_VCE1.LE.0.) SIGN=UN                                             
        DO I=1,3                                                                   
           EXCHANGE=VCE1(I,1)                                                      
           VCE1(I,1)=VCE1(I,2)                                                     
           VCE1(I,2)=EXCHANGE*SIGN                                                 
        ENDDO                                                                  
        EXCHANGE=DCE1(1)                                                           
        DCE1(1)=DCE1(2)                                                            
        DCE1(2)=EXCHANGE                                                           
C
        DO I=1,3                                                                   
           DO J=1,3                                                                
           BETAM0(I,J)=DCE1(1)*VCE1(I,1)*VCE1(J,1)                                 
     &     +DCE1(2)*VCE1(I,2)*VCE1(J,2)+DCE1(3)*VCE1(I,3)*VCE1(J,3)                
           END DO                                                                  
        END DO                                                                     
C
C...... DETERMINATION OF MATERIAL FUNCTION  MU_B                                  
        DMU_B = DMU_R*(ONE-(DCE1(1)+DCE1(2)+DCE1(3)-TROIS)/DLAMBDA_L)**(-1.0)     
C...... DETERMINATION OF ALPHA                                                    
C
C
        CALL AT_TPU(DTIME, DV, ONE, PMIDENT, TEMP1, 9)                             
      	 CALL AXB_TPU(TEMP1, FVM, FVM_ITER, 3)                                      
        CALL MAT3X3TOVEC6X1(FVM_ITER,FV_ITER,3)                                    
C--------------------------------------------------------------------72
C-------- ELASTIC STRESSES AT THE END OF THE STEP
C--------------------------------------------------------------------72
C
C----   COMPUTE: FE->CE->UE->RE->EE                                       
        CALL ST_TPU(FVM_INV, ZERO, 3*3)                                   
        CALL ST_TPU(FTHETA,  ZERO, 3*3)                                   
        CALL ST_TPU(FTHETA_INV, ZERO, 3*3)                                
        CALL ST_TPU(TENS0, ZERO, 3*3)                                     
        CALL ST_TPU(TENS1, ZERO, 3*3)                                     
        CALL ST_TPU(FE1,     ZERO, 3*3)                                   
        CALL ST_TPU(CE1,     ZERO, 3*3)                                   
        CALL ST_TPU(UE1,     ZERO, 3*3)                                   
        CALL ST_TPU(UE1_INV, ZERO, 3*3)                                   
        CALL ST_TPU(ROTE1,   ZERO, 3*3)                                   
        CALL ST_TPU(EE1,     ZERO, 3*3)                                   
C
C       DETERMINATION OF FE AND CE                                        
C
        CALL SST_TPU(ONE+BETA0*(THETA-THETAI), PMIDENT, FTHETA, 3*3)      
        CALL INV_TPU(FTHETA, FTHETA_INV, DET)                             
        CALL INV_TPU(FVM_ITER, FVM_INV, DET)                              
        CALL AXB_TPU(F1, FTHETA_INV, TENS0, 3)                            
        CALL AXB_TPU(TENS0, FVM_INV, FE1, 3)                              
        CALL ATXB_TPU(FE1, FE1, CE1, 3)                                   
        DO I=1,NDI                                                        
            DO J=1,NDI                                                    
              A(I,J)= CE1(I,J)                                            
            ENDDO                                                         
        ENDDO                                                             
        CALL VALPROP_TPU(A, 3, 3, DCE1, VCE1, NROT, IER)                                
        CALL EIGSRT(DCE1, VCE1, 3, 3)                                                   
        IF (IER.EQ.1) THEN                                                              
          CALL ERR_TPU('ERROR IN EIG VAL')                                              
        ENDIF                                                                           
        !*** EIGENVALUES (AND ASSOC EIGENVECTORS) ARE ORDERED FROM LARGER TO SMALLER.  !
        !*** REDEFINE AXIS(2) TO BE THE LARGEST IN ORDER TO IMPROVE ACCURACY IN THE    !
        !   CALCULATION OF THE ESHELBY TENSOR.                                        !
        !*** IF DET(B)<0 MEANS THAT THE SYSTEM IS LEFT HANDED. IT IS MADE RIGHT        !
        !   HANDED BY EXCHANGING 1 AND 2.                                             !
        SIGN=-ONE                                                                       
        CALL DET_TPU(VCE1, DET_VCE1)                                                    
        IF(DET_VCE1.LE.ZERO) SIGN=ONE                                                   
        DO I=1,3                                                                        
           EXCHANGE=VCE1(I,1)                                                           
           VCE1(I,1)=VCE1(I,2)                                                          
           VCE1(I,2)=EXCHANGE*SIGN                                                      
        ENDDO                                                                           
        EXCHANGE=DCE1(1)                                                                
        DCE1(1)=DCE1(2)                                                                 
        DCE1(2)=EXCHANGE                                                                
C
C------ COMPUTE UE1                                                                     
C
        D1=SQRT(DCE1(1))                                                                
        D2=SQRT(DCE1(2))                                                                
        D3=SQRT(DCE1(3))                                                                
        DE1=LOG(SQRT(DCE1(1)))                                                          
        DE2=LOG(SQRT(DCE1(2)))                                                          
        DE3=LOG(SQRT(DCE1(3)))                                                          
C
        DO I=1,3                                                
           DO J=1,3                                             
             UE1(I,J)=D1*VCE1(I,1)*VCE1(J,1)                    
     &     +D2*VCE1(I,2)*VCE1(J,2)+D3*VCE1(I,3)*VCE1(J,3)       
           END DO                                               
        END DO                                                  
C
        DO I=1,3                                                
           DO J=1,3                                             
             EE1(I,J)=DE1*VCE1(I,1)*VCE1(J,1)                     
     &     +DE2*VCE1(I,2)*VCE1(J,2)+DE3*VCE1(I,3)*VCE1(J,3)     
           END DO                                               
        END DO                                                  
C
        CALL INV_TPU(UE1, UE1_INV, DET)                         
        CALL AXB_TPU(FE1, UE1_INV, ROTE1, 3)                    
        CALL DET_TPU(FE1, DET_FE)                               
C
C=======DETERMINATION OF TAU
C
        CALL ST_TPU(XM,        ZERO, 3*3)                                  
        CALL ST_TPU(TAUM,      ZERO, 3*3)                                  
C
        CALL TR_TPU(EE1, TR_EE1, 3)                                        
        COEFF1_A       = TWOMU0                                            
        COEFF1_B       = BULKA0*TR_EE1                                     
        CALL AT_TPU(COEFF1_A,EE1, COEFF1_B, PMIDENT, XM, 9)                
        CALL QAQT_TPU(ROTE1, XM, TAUM, 3)                                  
C
C-------DETERMINATION OF SIGMA AND INTERNAL STATE VARIABLES                
C
        CALL ST_TPU(SIGMAM,      ZERO, 3*3)                                
        CALL ST_TPU(SIGMA_Q,     ZERO, 3*3)                                
C
        CALL SST_TPU(ONE/DET_FE, TAUM, SIGMAM, 3*3)                        
C
C-------DETERMINATION OF SIGMA  (IN THE ROTATED CONFIGURATION)    
C
        CALL QTAQ_TPU(ROT1, SIGMAM, SIGMA_Q, 3)                            
C
C====================================================================72
C============ UPDATE VARIABLES
C====================================================================72
C
		      CALL SP_TPU(DV_VEC, DV_VEC,ps,6)                          
        GAMV_ITER    = GAMV + DGAMV_ITER                          
        GAMVEQ       = GAMVEQ +SQRT((DEUX/TROIS)*ps)*DTIME        
C
C-------TEMPERATURE, ASSUMING ADIABATIC CONDITIONS            ----
C
        IF (ABS(D_THETA_OPT-ZERO) .LT. SMALL .OR.                 
     &      ABS(D_THETA_OPT-ONE)  .LT. SMALL) FACTOR = ZERO       
C
        CALL ST_TPU(XM_VEC,  ZERO, 6)                             
C
        RHO_P = RHO_P*(1.42D0*THETAG+44.7D0)                      
     &                / (1.42D0*THETAG+0.15D0*THETA)              
        CV = CV*(0.106D0+TROIS*EM03*THETA)                        
C
        CALL MAT3X3TOVEC6X1(XM, XM_VEC, 3)                        
				    CALL SP_TPU(XM_VEC,DV_VEC,ps,6)                           
        DTHETA_ITER  = (DTIME*FACTOR*ps) /(CV*RHO_P)              
        THETAITER    = THETA + DTHETA_ITER                        
        THETADOTITER = DTHETA_ITER/DTIME                          
C=======DETERMINATION OF SIGMA AND INTERNAL STATE VARIABLES       
C
C-------SIGMA_Q(I,J)                   
C
        SIGNXX(IE) = SIGMA_Q(1,1)      
        SIGNYY(IE) = SIGMA_Q(2,2)      
        SIGNZZ(IE) = SIGMA_Q(3,3)      
        SIGNXY(IE) = SIGMA_Q(1,2)      
        SIGNYZ(IE) = SIGMA_Q(2,3)      
        SIGNZX(IE) = SIGMA_Q(1,3)      
        
C
        UVAR(IE, 1)  = FV_ITER(1)      
        UVAR(IE, 2)  = FV_ITER(2)      
        UVAR(IE, 3)  = FV_ITER(3)      
        UVAR(IE, 4)  = FV_ITER(4)      
        UVAR(IE, 5)  = FV_ITER(5)      
        UVAR(IE, 6)  = FV_ITER(6)      
        UVAR(IE, 7)  = DES1            
        UVAR(IE, 8)  = DESTAR          
        UVAR(IE, 9)  = DES2            
        UVAR(IE, 10) = BETAM0(1,1)     
        UVAR(IE, 11) = BETAM0(2,2)     
        UVAR(IE, 12) = BETAM0(3,3)     
        UVAR(IE, 13) = BETAM0(1,2)     
        UVAR(IE, 14) = BETAM0(2,3)     
        UVAR(IE, 15) = BETAM0(3,1)     
        UVAR(IE, 16) = DMU_B           
        UVAR(IE, 17) = GAMV_ITER       
        UVAR(IE, 18) = DGAMV_ITER      
        UVAR(IE, 19) = GAMVEQ          
        UVAR(IE, 20) = THETAITER       
        UVAR(IE, 21) = THETADOTITER    
        UVAR(IE,22) = F1(1,1) !F011    
        UVAR(IE,23) = F1(2,2) !F022         
        UVAR(IE,24) = F1(3,3) !F033         
        UVAR(IE,25) = F1(1,2) !F012         
        UVAR(IE,26) = F1(2,1) !F021         
        UVAR(IE,27) = F1(1,3) !F013         
	   UVAR(IE,28) = F1(3,1) !F031         
        UVAR(IE,29) = F1(2,3) !F023         
        UVAR(IE,30) = F1(3,2) !F032         
        UVAR(IE,31) = U1(1,1) !U011         
        UVAR(IE,32) = U1(2,2) !U022         
        UVAR(IE,33) = U1(3,3) !U033         
        UVAR(IE,34) = U1(1,2) !U012         
        UVAR(IE,35) = U1(2,1) !U021         
        UVAR(IE,36) = U1(1,3) !U013         
	   UVAR(IE,37) = U1(3,1) !U031         
        UVAR(IE,38) = U1(2,3) !U023         
        UVAR(IE,39) = U1(3,2) !U032         
        UVAR(IE,40) = LOG(U1(1,1)) !E111    
        UVAR(IE,41) = LOG(U1(2,2)) !E122    
        UVAR(IE,42) = LOG(U1(3,3)) !E133    
		
C

C sound velocity  
            SOUNDSP(IE) = SQRT((DK+4*DMU/3)/RHO0(IE)) 
            VISCMAX(IE) = ZERO     
	 
C................................... MODEL IMPLEMENTATION CHOICE
C
20    ENDDO 
C
C................................... END ELEMENT LOOP
     
         
C Temperature         
         TEMP(1:NEL)   = TEMPEL(1:NEL)
         
         
      RETURN
      END
C

      
      
C=====================================================================72
C=====================================================================72
C---- SUPPORTING ROUTINES FOR TPU MATERIAL ROUTINE
C=====================================================================72
C=====================================================================72
C
Chd|====================================================================
Chd|  ST_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        ATXB_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        AXBT_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        AXB_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        COMPEE_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|        SKW_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ST_TPU(A, VALUE, N)
#include "implicit_f.inc"

      INTEGER N, I
      my_real  VALUE
      my_real  A(N)
C
C---------------------------------------------------------------------72
C
      DO I = 1, N
        A(I) = VALUE
      ENDDO

      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  AXB_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        COMPEE_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|        QAQT_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        QTAQ_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|        ST_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE AXB_TPU(A, B, C, N)
#include "implicit_f.inc"

      INTEGER N, I, J, K
      my_real  A(N, N), B(N, N), C(N, N)
C
C---------------------------------------------------------------------72
C
      CALL ST_TPU(C,ZERO, N*N)
C
      DO I = 1, N
         DO J = 1, N
            DO K = 1, N
               C(I,J) = C(I,J) + A(I,K) * B(K,J)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  ATXB_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        COMPEE_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|        QTAQMT_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|        QTAQ_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|        ST_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE ATXB_TPU(A, B, C, N)
#include "implicit_f.inc"

      INTEGER N, I, J, K
      my_real   A(N, N), B(N, N), C(N, N)
C
C---------------------------------------------------------------------72
C
      CALL ST_TPU(C,ZERO, N*N)
C
      DO I = 1, N
         DO J = 1, N
            DO K = 1, N
               C(I,J) = C(I,J) + A(K,I) * B(K,J)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  SST_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SST_TPU(COEFA, A, B, N)
#include "implicit_f.inc"

      INTEGER N, I
      my_real  COEFA
      my_real  A(N), B(N)
C
C---------------------------------------------------------------------72
C
      DO I = 1, N
        B(I) = COEFA*A(I)
      ENDDO
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  AXBT_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        QAQT_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        QTAQMT_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|        ST_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE AXBT_TPU(A, B, C, N)
#include "implicit_f.inc"

      INTEGER N, I, J, K
      my_real  A(N, N), B(N, N), C(N, N)
C
C---------------------------------------------------------------------72
C
      CALL ST_TPU(C,ZERO, N*N)
C
      DO I = 1, N
         DO J = 1, N
            DO K = 1, N
               C(I,J) = C(I,J) + A(I,K) * B(J,K)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  TR_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        COMPEE_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|        DEV_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|        ERR_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE TR_TPU(TENSOR, TR_TENSOR, N)
#include "implicit_f.inc"

      INTEGER N
      my_real  TENSOR(N*N)
      my_real  TR_TENSOR
C
C---------------------------------------------------------------------72
C
      IF (N .NE. 3)
     &   CALL ERR_TPU('TRACEOFTENSOR: N =! 3')
C
      TR_TENSOR = TENSOR(1) + TENSOR(5) + TENSOR(9)
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  DET_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        COMPEE_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|        INV_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DET_TPU(TENSOR, TENSOR_DET)
#include "implicit_f.inc"

      my_real  TENSOR(3, 3)
      my_real   DET11, DET12, DET13, DET21, DET22, DET23
      my_real   TENSOR_DET
C
C---------------------------------------------------------------------72
C
C  DETERMINANT OF A SECOND ORDER TENSOR (3X3 MATRIX)
C
      DET11 = TENSOR(1, 1) * TENSOR(2, 2) * TENSOR(3, 3)
      DET12 = TENSOR(1, 2) * TENSOR(2, 3) * TENSOR(3, 1)
      DET13 = TENSOR(1, 3) * TENSOR(2, 1) * TENSOR(3, 2)
      DET21 = TENSOR(1, 1) * TENSOR(2, 3) * TENSOR(3, 2)
      DET22 = TENSOR(1, 2) * TENSOR(2, 1) * TENSOR(3, 3)
      DET23 = TENSOR(1, 3) * TENSOR(2, 2) * TENSOR(3, 1)
C
      TENSOR_DET = DET11 + DET12 + DET13 - DET21 - DET22 - DET23
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  INV_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        COMPEE_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|        QTAQMT_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|        DET_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE INV_TPU(TENSOR, TENSOR_INV, TENSOR_DET)
#include "implicit_f.inc"

      my_real  TENSOR_DET
      my_real  TENSOR(3, 3), TENSOR_INV(3, 3)
      my_real  TINV11, TINV12, TINV13
      my_real  TINV21, TINV22, TINV23
      my_real  TINV31, TINV32, TINV33
C
C---------------------------------------------------------------------72
C
      CALL DET_TPU(TENSOR, TENSOR_DET)
C
      TINV11 = TENSOR(2, 2) * TENSOR(3, 3) - TENSOR(2, 3) * TENSOR(3, 2)
      TINV12 = TENSOR(1, 3) * TENSOR(3, 2) - TENSOR(1, 2) * TENSOR(3, 3)
      TINV13 = TENSOR(1, 2) * TENSOR(2, 3) - TENSOR(1, 3) * TENSOR(2, 2)
      TINV21 = TENSOR(2, 3) * TENSOR(3, 1) - TENSOR(2, 1) * TENSOR(3, 3)
      TINV22 = TENSOR(1, 1) * TENSOR(3, 3) - TENSOR(1, 3) * TENSOR(3, 1)
      TINV23 = TENSOR(1, 3) * TENSOR(2, 1) - TENSOR(1, 1) * TENSOR(2, 3)
      TINV31 = TENSOR(2, 1) * TENSOR(3, 2) - TENSOR(2, 2) * TENSOR(3, 1)
      TINV32 = TENSOR(3, 1) * TENSOR(1, 2) - TENSOR(1, 1) * TENSOR(3, 2)
      TINV33 = TENSOR(1, 1) * TENSOR(2, 2) - TENSOR(1, 2) * TENSOR(2, 1)
C
      IF (TENSOR_DET /= ZERO) THEN
      TENSOR_INV(1, 1) = TINV11 / TENSOR_DET
      TENSOR_INV(1, 2) = TINV12 / TENSOR_DET
      TENSOR_INV(1, 3) = TINV13 / TENSOR_DET
      TENSOR_INV(2, 1) = TINV21 / TENSOR_DET
      TENSOR_INV(2, 2) = TINV22 / TENSOR_DET
      TENSOR_INV(2, 3) = TINV23 / TENSOR_DET
      TENSOR_INV(3, 1) = TINV31 / TENSOR_DET
      TENSOR_INV(3, 2) = TINV32 / TENSOR_DET
      TENSOR_INV(3, 3) = TINV33 / TENSOR_DET
      ENDIF
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  AT_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        COMPEE_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE AT_TPU(COEFA, A, COEFB, B, C, N)
#include "implicit_f.inc"

      INTEGER N, I 
      my_real  COEFA, COEFB
      my_real  A(N), B(N), C(N)
C
C---------------------------------------------------------------------72
C
      DO I = 1, N
        C(I) = COEFA*A(I) + COEFB*B(I)
C        WRITE(0 , *) 'CHECK AT_TPU '
C        WRITE(0 , *) 'COEFA ',COEFA,' ','COEFB ',COEFB
C    	 WRITE(0 , *) 'A ',I,' ',A(I)
C        WRITE(0 , *) 'B ',I,' ',B(I) 
C        WRITE(0 , *) 'C ',I,' ',C(I)
      ENDDO
C
      RETURN
      END
C
C=====================================================================72
C
C=====================================================================72
C
Chd|====================================================================
Chd|  DEV_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|        ERR_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        TR_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE DEV_TPU(TENSOR, TENSORDEV, N)
#include "implicit_f.inc"

      INTEGER N
      my_real  TENSKK, TR_TENSOR
      my_real  TENSOR(N*N), TENSORDEV(N*N)
C
C---------------------------------------------------------------------72
C
      IF (N .NE. 3)
     &   CALL ERR_TPU('DEVIATORICTENSOR: N =! 3')
C
      CALL TR_TPU(TENSOR, TR_TENSOR, N)
      TENSKK = TR_TENSOR / TROIS
C
      TENSORDEV(1) = TENSOR(1) - TENSKK
      TENSORDEV(5) = TENSOR(5) - TENSKK
      TENSORDEV(9) = TENSOR(9) - TENSKK
C
      TENSORDEV(2) = TENSOR(2)
      TENSORDEV(3) = TENSOR(3)
      TENSORDEV(6) = TENSOR(6)
C
      TENSORDEV(4) = TENSOR(4)
      TENSORDEV(7) = TENSOR(7)
      TENSORDEV(8) = TENSOR(8)
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  QAQT_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|        AXBT_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        AXB_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE QAQT_TPU(Q, A, B, N)
#include "implicit_f.inc"

      INTEGER N
      my_real  Q(N,N), A(N,N), B(N,N)
      my_real  TMP(N,N)
C
C---------------------------------------------------------------------72
C
      CALL AXB_TPU(Q, A, TMP, N)       ! TMP = Q*A
      CALL AXBT_TPU(TMP, Q, B, N)      ! B = TMP*QT
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  QTAQ_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|        ATXB_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        AXB_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE QTAQ_TPU(Q, A, B, N)
#include "implicit_f.inc"

      INTEGER N
      my_real  Q(N,N), A(N,N), B(N,N)
      my_real  TMP(N,N)
C
C---------------------------------------------------------------------72
C
      CALL ATXB_TPU(Q, A, TMP, N)      ! TMP = QT*A
      CALL AXB_TPU(TMP, Q, B, N)       ! B = TMP*Q
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  SYM_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|        ERR_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE SYM_TPU(TENSOR, SYMMTENSOR, N)
#include "implicit_f.inc"

      INTEGER  N
      my_real  TENSOR(N*N), SYMMTENSOR(N*N)
C
C---------------------------------------------------------------------72
C
      IF ((N .NE. 2) .AND. (N .NE. 3))
     &   CALL ERR_TPU('SYM_TPU: N =! 2 OR 3')
C
      IF (N .EQ. 2) THEN
         SYMMTENSOR(1) = TENSOR(1)
         SYMMTENSOR(4) = TENSOR(4)
         SYMMTENSOR(3) = UNDEMI*(TENSOR(3) + TENSOR(2))
         SYMMTENSOR(2) = SYMMTENSOR(3)
      ELSE   ! N = 3
         SYMMTENSOR(1) = TENSOR(1)                     ! 11
         SYMMTENSOR(5) = TENSOR(5)                     ! 22
         SYMMTENSOR(9) = TENSOR(9)                     ! 33
         SYMMTENSOR(4) = UNDEMI*(TENSOR(4) + TENSOR(2))   ! 12
         SYMMTENSOR(7) = UNDEMI*(TENSOR(7) + TENSOR(3))   ! 13
         SYMMTENSOR(8) = UNDEMI*(TENSOR(8) + TENSOR(6))   ! 23
         SYMMTENSOR(2) = SYMMTENSOR(4)                 ! 21
         SYMMTENSOR(3) = SYMMTENSOR(7)                 ! 31
         SYMMTENSOR(6) = SYMMTENSOR(8)                 ! 32
      ENDIF
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  SKW_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|        ERR_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        ST_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE SKW_TPU(TENSOR, SKEWTENSOR, N)
#include "implicit_f.inc"

      INTEGER  N
      my_real  TENSOR(N*N), SKEWTENSOR(N*N)
C
C---------------------------------------------------------------------72
C
      IF ((N .NE. 2) .AND. (N .NE. 3))
     &   CALL ERR_TPU('SKW_TPU: N =! 2 OR 3')
C
      CALL ST_TPU(SKEWTENSOR,ZERO, N*N)
C
      IF (N .EQ. 2) THEN
         SKEWTENSOR(3) = UNDEMI*(TENSOR(3) - TENSOR(2))
         SKEWTENSOR(2) = - SKEWTENSOR(3)
      ELSE   ! N = 3
         SKEWTENSOR(4) = UNDEMI*(TENSOR(4) - TENSOR(2))
         SKEWTENSOR(7) = UNDEMI*(TENSOR(7) - TENSOR(3))
         SKEWTENSOR(8) = UNDEMI*(TENSOR(8) - TENSOR(6))
         SKEWTENSOR(2) = - SKEWTENSOR(4)
         SKEWTENSOR(3) = - SKEWTENSOR(7)
         SKEWTENSOR(6) = - SKEWTENSOR(8)
      ENDIF
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  MAT3X3TOVEC6X1                source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAT3X3TOVEC6X1(
     &   MATRIX, VECTOR, N
     &   )
C
#include "implicit_f.inc"

C
      INTEGER  N
      my_real  MATRIX(N*N), VECTOR(6)
C
C---------------------------------------------------------------------72
C
      VECTOR(1) = MATRIX(1)
      VECTOR(2) = MATRIX(5)
      VECTOR(3) = MATRIX(9)
      VECTOR(4) = MATRIX(2)
      VECTOR(5) = MATRIX(3)
      VECTOR(6) = MATRIX(6)
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  VEC6X1TOMAT3X3                source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        COMPEE_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VEC6X1TOMAT3X3(
     &   VECTOR, MATRIX, N
     &   )
C
#include "implicit_f.inc"

C
      INTEGER  N
      my_real  MATRIX(N*N), VECTOR(6)
C
C---------------------------------------------------------------------72
C
      MATRIX(1) = VECTOR(1)
      MATRIX(5) = VECTOR(2)
      MATRIX(9) = VECTOR(3)
      MATRIX(2) = VECTOR(4)
      MATRIX(3) = VECTOR(5)
      MATRIX(6) = VECTOR(6)
      MATRIX(4) = VECTOR(4)
      MATRIX(7) = VECTOR(5)
      MATRIX(8) = VECTOR(6)
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
      ! my_real FUNCTION SP_TPU(A, B, N)
      ! IMPLICIT NONE
      ! INTEGER N, I
      ! my_real A(N), B(N)
! C
! C---------------------------------------------------------------------72
! C
      ! SP_TPU =ZERO
      ! DO I = 1, 3
        ! SP_TPU = SP_TPU + A(I)*B(I)        
      ! ENDDO   

      ! DO I = 4, N
        ! SP_TPU = SP_TPU + DEUX*A(I)*B(I)        
      ! ENDDO   

      ! RETURN
      ! END
	  
	  
Chd|====================================================================
Chd|  SP_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SP_TPU(A, B, c, N)
#include "implicit_f.inc"

      INTEGER N, I
      my_real A(N), B(N), c
C
C---------------------------------------------------------------------72
C
      c =ZERO
      DO I = 1, 3
        c = c + A(I)*B(I)        
      ENDDO   

      DO I = 4, N
        c = c + DEUX*A(I)*B(I)        
      ENDDO   

      RETURN
      END	  
	  
	  
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  QTAQMT_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ATXB_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        AXBT_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        INV_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE QTAQMT_TPU(Q, A, B, N)
#include "implicit_f.inc"

      INTEGER N
      my_real  Q(N,N), A(N,N), B(N,N)
      my_real  TMP(N,N), QM1(N,N)
      my_real  DET
C
C---------------------------------------------------------------------72
C
      CALL ATXB_TPU(Q, A, TMP, N)      ! TMP = QT*A
      CALL INV_TPU(Q, QM1, DET)        
      CALL AXBT_TPU(TMP, QM1, B, N)    ! B = TMP*QMT
C                                                                               
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
C
Chd|====================================================================
Chd|  EMOD_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EMOD_TPU(AREF,A0  ,C   ,C0    ,DE1  ,DE2, D1,DREF,RES)
!        CALL EMOD_TPU  (DEREF,DE0,THETA,THETA0,DVE1,DVE2,
!     &                                                  EM04,DEDOT_REF,DE)

#include "implicit_f.inc"

      my_real AREF,A0,C,C0,DE1,DE2,D1,DREF,RES
C---------------------------------------------------------------------72

C
      RES   = (AREF + A0*(C-C0))*(UN+DE1/
     &          (UN+ EXP(-(LOG10(MAX(EM20,D1))-LOG10(MAX(EM20, DREF)) ) /MAX(EM20,DE2))))
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
C
Chd|====================================================================
Chd|  VALPROP_TPU                   source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VALPROP_TPU(A,N,NP,D,V,NROT,IER)
#include "implicit_f.inc"

      INTEGER N,NP,NROT,NMAX
      my_real A(NP,NP),D(NP),V(NP,NP)
      PARAMETER (NMAX=500)
      INTEGER I,IP,IQ,J,IER
      my_real C,G,H,S,SM,T,TAU,THETA,TRESH,B(NMAX),Z(NMAX)
      DO 12 IP=1,N
        DO 11 IQ=1,N
          V(IP,IQ)=0.
11      CONTINUE
        V(IP,IP)=UN
12    CONTINUE
      DO 13 IP=1,N
        B(IP)=A(IP,IP)
        D(IP)=B(IP)
        Z(IP)=ZERO
13    CONTINUE
      NROT=0
      DO 24 I=1,50
        SM=ZERO
        DO 15 IP=1,N-1
          DO 14 IQ=IP+1,N
            SM=SM+ABS(A(IP,IQ))

14        CONTINUE
15      CONTINUE
C
        IF(SM.EQ.0.)THEN
        IER=0
        RETURN
        ENDIF
C
        IF(I.LT.4)THEN
          TRESH=0.2D0*SM/N**2
        ELSE
          TRESH=ZERO
        ENDIF
        DO 22 IP=1,N-1
          DO 21 IQ=IP+1,N
            G=CENT*ABS(A(IP,IQ))
            IF((I.GT.4).AND.(ABS(D(IP))+
     *G.EQ.ABS(D(IP))).AND.(ABS(D(IQ))+G.EQ.ABS(D(IQ))))THEN
              A(IP,IQ)=0.
            ELSE IF(ABS(A(IP,IQ)).GT.TRESH)THEN
              H=D(IQ)-D(IP)
              IF(ABS(H)+G.EQ.ABS(H))THEN
                T=A(IP,IQ)/H
C
              ELSE
                THETA=UNDEMI*H/A(IP,IQ)
                T=UN/(ABS(THETA)+SQRT(UN+THETA**2))
                IF(THETA.LT.ZERO)T=-T
              ENDIF
              C=UN/SQRT(UN+T**2)
              S=T*C
              TAU=S/(UN+C)
              H=T*A(IP,IQ)
              Z(IP)=Z(IP)-H
              Z(IQ)=Z(IQ)+H
              D(IP)=D(IP)-H
              D(IQ)=D(IQ)+H
              A(IP,IQ)=ZERO
              DO 16 J=1,IP-1
                G=A(J,IP)
                H=A(J,IQ)
C
                A(J,IP)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
16            CONTINUE
              DO 17 J=IP+1,IQ-1
                G=A(IP,J)
                H=A(J,IQ)
                A(IP,J)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
17            CONTINUE
              DO 18 J=IQ+1,N
                G=A(IP,J)
                H=A(IQ,J)
                A(IP,J)=G-S*(H+G*TAU)
                A(IQ,J)=H+S*(G-H*TAU)
18            CONTINUE
              DO 19 J=1,N
                G=V(J,IP)
                H=V(J,IQ)
C
                V(J,IP)=G-S*(H+G*TAU)
                V(J,IQ)=H+S*(G-H*TAU)
19            CONTINUE
              NROT=NROT+1
            ENDIF
21        CONTINUE
22      CONTINUE
        DO 23 IP=1,N
          B(IP)=B(IP)+Z(IP)
          D(IP)=B(IP)
          Z(IP)=ZERO
23      CONTINUE
24    CONTINUE
C      PAUSE 'TOO MANY ITERATIONS IN JACOBI'
C
      IER=1
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  EIGSRT                        source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EIGSRT(D,V,N,NP)
#include "implicit_f.inc"

      INTEGER N,NP
      my_real D(NP),V(NP,NP)
      INTEGER I,J,K
      my_real P
      DO 13 I=1,N-1
        K=I
        P=D(I)
        DO 11 J=I+1,N
          IF(D(J).GE.P)THEN
            K=J
            P=D(J)
          ENDIF
11      CONTINUE
        IF(K.NE.I)THEN
          D(K)=D(I)
          D(I)=P
          DO 12 J=1,N
            P=V(J,I)
            V(J,I)=V(J,K)
            V(J,K)=P
12        CONTINUE
        ENDIF
13    CONTINUE
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  COMPEE_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ATXB_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        AT_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|        AXB_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        DET_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        INV_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        ST_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|        TR_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|        VEC6X1TOMAT3X3                source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE COMPEE_TPU(F, FV, ROT, EE, NDI)

#include "implicit_f.inc"
      INTEGER I, J, NDI
      my_real FVM(3,3), FVM_INV(3,3)
      my_real FE(3,3), CE(3,3), UE(3,3), F(3,3)
      my_real UE_INV(3,3), EE(3,3), XMA(3,3), ROT(3,3)
      my_real CE2(3,3), PMIDENT(3,3), TEMP(3,3)
      my_real FV(6)
      my_real INV1, INV2, INV3, B, C, M, N, T
      my_real L1, L2, L3, P1, P2, P3, D1,  SMALL
      my_real ONE, TWO, THREE, X1, X2, X3,  DET
      my_real COEFF1, COEFF2, TR_CE2
C
!      DATA PI/3.141592653589793238462643D0/
      DATA    PMIDENT   /1.D0,  0.D0,  0.D0, 0.D0,  1.D0,  0.D0,
     &                   0.D0,  0.D0,  1.D0/
C
C*********************************************************************72
C
C---- NUMERICAL CONSTANTS
C

      ONE     = UN
      TWO     = DEUX
      THREE   = TROIS
      SMALL   = EM06
C
C--------------------------------------------------------------------72
C-------- COMPONENTS USEFUL FOR THE BRANCH A
C--------------------------------------------------------------------72
C
C=======DETERMINATION OF DIFFERENT TENSORS: FE->CE->UE->RE->EE
C
        CALL ST_TPU(FVM, ZERO, 3*3)
        CALL ST_TPU(FVM_INV, ZERO, 3*3)
        CALL ST_TPU(FE, ZERO, 3*3)
        CALL ST_TPU(CE, ZERO, 3*3)
        CALL ST_TPU(CE2, ZERO, 3*3)
        CALL ST_TPU(UE, ZERO, 3*3)
        CALL ST_TPU(UE_INV, ZERO, 3*3)
        CALL ST_TPU(ROT, ZERO, 3*3)
C
C       DETERMINATION OF FE AND CE
C
        CALL VEC6X1TOMAT3X3(FV,FVM,3)
        CALL INV_TPU(FVM, FVM_INV, DET)
        CALL AXB_TPU(F, FVM_INV, FE, 3)
        CALL ATXB_TPU(FE, FE, CE, 3)
        CALL AXB_TPU(CE, CE, CE2, 3)
C
C        WRITE(0 , *) 'FE IN SUBROUTINE'
C        WRITE(0 , *) FE(1,1),' ',FE(1,2),' ',FE(1,3)
C        WRITE(0 , *) FE(2,1),' ',FE(2,2),' ',FE(2,3)
C        WRITE(0 , *) FE(3,1),' ',FE(3,2),' ',FE(3,3)
C
C        WRITE(0 , *) 'CE IN SUBROUTINE'
C        WRITE(0 , *) CE(1,1),' ',CE(1,2),' ',CE(1,3)
C        WRITE(0 , *) CE(2,1),' ',CE(2,2),' ',CE(2,3)
C        WRITE(0 , *) CE(3,1),' ',CE(3,2),' ',CE(3,3)
C
C       DETERMINATION OF UE USING FRANCA ALGORITHM 
C       (SIMO & HUGHES P243)
C
        CALL TR_TPU(CE, INV1, 3)
        CALL TR_TPU(CE2, TR_CE2, 3)
        INV2 = (ONE/TWO)*(INV1**TWO - TR_CE2)
        CALL DET_TPU(CE, INV3)
C
        B    = INV2 - (INV1**TWO)/THREE
        C    = -(DEUX/27.D0)*(INV1**THREE) + INV1*INV2/THREE - INV3
C
        IF ( ABS(B) .LT. EM04) THEN
                X1 = -ABS(C)**(THIRD)
                X2 = X1
                X3 = X1
        ELSE
                M  = TWO*SQRT(-B/THREE)
                N  = THREE*C/(M*B)
C
                IF (N .LT. -UN) THEN
                        N = -UN
                ELSE IF (N .GT. UN) THEN
                        N = UN
                ENDIF
C

                T  = ATAN2(SQRT(ONE-(N*N)),N)/THREE
                X1 = M * COS(T)
                X2 = M * COS(T+DEUX*PI/THREE)
                X3 = M * COS(T+quatre*PI/THREE)
        ENDIF
C
        L1   = SQRT(X1 + INV1/THREE)
        L2   = SQRT(X2 + INV1/THREE)
        L3   = SQRT(X3 + INV1/THREE)
C
C        WRITE(0, *) 'EIG OF CE ', L1,' ',L2,' ',L3     
C
        P1   = L1 + L2 + L3
        P2   = L1*L2 + L1*L3 + L2*L3
        P3   = L1*L2*L3
C
        D1   = P1*P2 - P3
        COEFF1 = P1*P1-P2
        COEFF2 = P1*P3
C
        CALL AT_TPU(COEFF1, CE, COEFF2, PMIDENT, TEMP, 9)
        CALL AT_TPU(-ONE/D1, CE2, ONE/D1, TEMP, UE, 9)
        CALL AT_TPU(-P1, UE, P2, PMIDENT, TEMP, 9)
        CALL AT_TPU(ONE/P3, CE, ONE/P3, TEMP, UE_INV, 9)
        CALL AXB_TPU(FE, UE_INV, ROT, 3)
C
C        WRITE(0 , *) 'ROT IN SUBR'
C        WRITE(0 , *) ROT(1,1),' ',ROT(1,2),' ',ROT(1,3)
C        WRITE(0 , *) ROT(2,1),' ',ROT(2,2),' ',ROT(2,3)
C        WRITE(0 , *) ROT(3,1),' ',ROT(3,2),' ',ROT(3,3)
C
C        WRITE(0 , *) 'UE IN SUBR'
C        WRITE(0 , *) UE(1,1),' ',UE(1,2),' ',UE(1,3)
C        WRITE(0 , *) UE(2,1),' ',UE(2,2),' ',UE(2,3)
C        WRITE(0 , *) UE(3,1),' ',UE(3,2),' ',UE(3,3)
C
        CALL ST_TPU(EE, ZERO, 3*3)
        EE(1,1)     = LOG(UE(1,1))
        EE(2,2)     = LOG(UE(2,2))
	EE(3,3)     = LOG(UE(3,3))
C
C        WRITE(0 , *) 'EE IN SUBR'
C        WRITE(0 , *) EE(1,1),' ',EE(1,2),' ',EE(1,3)
C        WRITE(0 , *) EE(2,1),' ',EE(2,2),' ',EE(2,3)
C        WRITE(0 , *) EE(3,1),' ',EE(3,2),' ',EE(3,3)
C        WRITE(0,*) 'SUB5'
C
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  INVL_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ERR_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        SIGNUM_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|====================================================================
      SUBROUTINE INVL_TPU(A, B, C)
#include "implicit_f.inc"

      my_real A, B, C
      my_real ARG, SARG
C
C---------------------------------------------------------------------72
C
      ARG = A / B
      IF ( ABS(ARG) .LT. 0.84136D0) THEN
        C = 1.31446D0 * TAN(1.58986D0*ARG) + 0.91209D0 * ARG
      ELSE IF (ABS(ARG) .GE. 0.84136D0 .AND. ABS(ARG) .LT. UN) THEN
        CALL SIGNUM_TPU(ARG, SARG)
        C = UN / (SARG - ARG)
      ELSE
        WRITE(0 ,*) 'CHECK ERROR LANGEVIN'
        WRITE(0 ,*) 'LAMBDA_BAR ', A, ' COEFF ', B
        WRITE(0 ,*) 'ARG ', ARG         
        CALL ERR_TPU('ERROR IN INVERSE LANGEVIN ARGUMENT')
      ENDIF
C
      RETURN
      END

C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  SIGNUM_TPU                    source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        INVL_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGNUM_TPU(A, B )
#include "implicit_f.inc"
c
      my_real A, B
C
C---------------------------------------------------------------------72
C
       IF (A .GT. 0.0) THEN
         B = UN
       ELSE IF (A .LT.ZERO) THEN
         B = -UN
       ELSE
         B =ZERO
       ENDIF
C                                                                                
      RETURN
      END
C
C=====================================================================72
C
C
C=====================================================================72
C
Chd|====================================================================
Chd|  ERR_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|-- called by -----------
Chd|        DEV_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        INVL_TPU                      source/materials/mat/mat101/sigeps101.F
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|        SKW_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        SYM_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        TR_TPU                        source/materials/mat/mat101/sigeps101.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
      SUBROUTINE ERR_TPU(MESSAGE)
c
#include "implicit_f.inc"
      CHARACTER*(*) MESSAGE
C
C---------------------------------------------------------------------72
C
      WRITE(*, 1000) MESSAGE
      CALL ARRET(2)
C                                                                                
1000  FORMAT(/,'***ERROR MESSAGE: '/, 3X, A)
C                                                                                
      END


