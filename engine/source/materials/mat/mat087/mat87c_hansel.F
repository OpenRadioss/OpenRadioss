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
Chd|  MAT87C_HANSEL                 source/materials/mat/mat087/mat87c_hansel.F
Chd|-- called by -----------
Chd|        SIGEPS87C                     source/materials/mat/mat087/sigeps87c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAT87C_HANSEL(
     1     NEL    ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP ,
     3     UVAR   ,RHO0    ,THKLY   ,THK     ,OFF      ,
     4     EPSPXX ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     5     DEPSXX ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,
     6     SIGOXX ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     8     SIGNXX ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     9     SOUNDSP,PLA     ,DPLA    ,EPSP    ,YLD      ,
     B     ETSE   ,GS      ,ISRATE  ,FCUT    ,UPARAM   ,
     C     TEMPEL ,SIGB    ,INLOC   ,DPLANL  ,SEQ      )
C=======================================================================
C   BARLAT YLD2000 material model
C=======================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL F
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C NPF     |  *      | I | R | FUNCTION ARRAY   
C IFLAG   |  *      | I | R | GEOMETRICAL FLAGS   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL    | F | R | INITIAL DENSITY
C THKLY   | NEL    | F | R | LAYER THICKNESS
C EPSPXX  | NEL    | F | R | STRAIN RATE XX
C EPSPYY  | NEL    | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL    | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL    | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C SIGOXX  | NEL    | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL    | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL    | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL    | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL    | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL    | F |R/W| THICKNESS
C PLA     | NEL    | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,ISRATE,INLOC
      my_real 
     .   TIME,TIMESTEP,FCUT,UPARAM(NUPARAM),
     .   RHO0(NEL),GS(NEL),
     .   THKLY(NEL),PLA(NEL),EPSPXX(NEL),EPSPYY(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DPLANL(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL), 
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),ETSE(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real ,DIMENSION(NEL,12) ,INTENT(INOUT) :: SIGB
      my_real 
     .   UVAR(NEL,NUVAR),OFF(NEL),THK(NEL),SEQ(NEL),
     .   YLD(NEL),DPLA(NEL),EPSP(NEL),TEMPEL(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,K,NRATE,NITER,JJ(NEL),J1,J2,N,NINDX,NFALG3,
     .        IFLAG,IFLAGSR,EXPA,EXPAM2,
     .        INDX(NEL), IPOS1(NEL),ILEN1(NEL),IAD1(NEL),NFLAG4,
     .        IPOS2(NEL),ILEN2(NEL),IAD2(NEL)
      my_real
     .        Y1(NEL),Y2(NEL),DYDX(NEL),DYDX1(NEL),DYDX2(NEL),
     .        XPXX(NEL),XPYY(NEL),XPXY(NEL),
     .        XPPXX(NEL),XPPYY(NEL),XPPXY(NEL),PHIP(NEL),PHIPP(NEL),
     .        DEPLXX(NEL),DEPLYY(NEL),DEPLXY(NEL),DEPLZZ(NEL),
     .        DEELZZ(NEL),DEPSZZ(NEL),XP1(NEL),XP2(NEL),XPP1(NEL),
     .        XPP2(NEL),SIG(NEL),H(NEL), DAEXP(NEL)
      my_real
     .      E,NU,BULK,A1,A2,G,UNSA,DU,R,ASRATE,DDEP,
     .      AL1, AL2 , AL3 , AL4 ,DSIGPL1,DSPL1,
     .      AL5, AL6 , AL7 , AL8 ,DSIGPL2,DSPL2,
     .      AL9, AL10, AL11, AL12,DSIGPL3,DSPL3,
     .      YSCALE,CEPS,EPS0,EXPN,TERM1,
     .      LPP11,LPP12,LPP21,LPP22,LPP66,
     .      FCTP,BPP,CPP,DPP,FP1,FP2,FP3,NORMXX,NORMYY,NORMXY,
     .      KINT,FPP1,FPP2,FPP3,DFP1,DFP2,DFP3,
     .      DFPP1,DFPP2,DFPP3,DP,AP,DF1,DF2,DF3,
     .      DSDEPL1,DSDEPL2,DSDEPL3,NORMEF,EPSPI,
     .      ASWIFT ,EPSO,QVOCE,BETA,KO,ALPHA,NEXP,
     .      EXPV,KSWIFT,KVOCE,UNSP,UNSC,PLA_I,YLD_I,NORMSIG,
     .      DSWIFT,DVOCE,DFEPSP,YLDP, p2,UNSPT,UNSCT,DFSR,TREF,DVM,
     .      K1,K2,AHS,BHS,MHS,EPS0HS,NHS,HMART,TEMP0,ETA,CP, AM    ,
     .      BM    , CM    , DM    , PPM    , QM    , E0MART, VM0 
      my_real
     .        HK(NEL),EPSF(NEL),PLAP(NEL),FRATE(NEL),
     .        SVM2(NEL),YLD2(NEL),HI(NEL),G3(NEL),
     .        HKIN,dezz,svm,DPLA_I(NEL),dr(NEL),
     .        AA(NEL),BB(NEL),DPLA_J(NEL),   
     .        PP(NEL),QQ(NEL),FAIL(NEL),SVMO(NEL),
     .        HI2(NEL), ES2(NEL), ATEMP(NEL),AEXP(NEL),
     .        VM(NEL),DTEMP(NEL),WP(NEL),TEMP(NEL),DPLA_DLAM(NEL)
      my_real
     .        YFAC(NEL,2),PUI_1,PUI_2
      my_real NORM_SP,FCT,DF,DLAM,YLD_NORM(NEL)


      my_real
     .        DSIGBXX(NEL),DSIGBYY(NEL),DSIGBXY(NEL),
     .        SIGBXX(NEL) ,SIGBYY(NEL) ,SIGBXY(NEL),
     .        DSIGBOXX(NEL),DSIGBOYY(NEL),DSIGBOXY(NEL),
     .        YLDP0(NEL),SIG_DFDSIG,
     .        DADEPL1,DADEPL2,DADEPL3,DAEXP0,FISOKIN,
     .        AKCK,CKH(4) ,AKH(4)  ,AEXP0 ,YLD0 (NEL)
C=======================================================================
c      Swift/Voce
c            k=alpha*k1(eps,p)+(1-alpha)*k2(eps,p)  
c            k1=A*(eps,p+e0)**n
c            k2=Q*(1-exp(-b*eps,p))+sig0
c-----------------------------------------------
c     USER VARIABLES
c-----------------------------------------------
!       UVAR1  = PLAP  
!       UVAR2  = YLD
!       UVAR3  = H
!       UVAR4  = DPLA
C=======================================================================
      NITER = 3
      FCT = zero
c-----------------------------------------------
      ! Elastic parameters
      E     =  UPARAM(1)  
      NU    =  UPARAM(2)  
      A1    =  E*UPARAM(21)
      A2    =  E*UPARAM(22)
      G     =  UPARAM(23)
      NORM_SP = EP03 / E
c
      ! Linear projection parameter
      AL1   =  UPARAM(3)  
      AL2   =  UPARAM(4)  
      AL3   =  UPARAM(5)  
      AL4   =  UPARAM(6)  
      AL5   =  UPARAM(7)  
      AL6   =  UPARAM(8)  
      AL7   =  UPARAM(9)  
      AL8   =  UPARAM(10) 
c
      ! A exponent parameter
      EXPA  =  NINT(UPARAM(15) )
      UNSA  =  ONE/EXPA
      EXPAM2=  EXPA - 2 
c
c
      IFLAG   = NINT(UPARAM(24))            

      NFALG3 = 34 
      K1      = UPARAM(NFALG3+1)
      K2      = UPARAM(NFALG3+2)
      AHS     = UPARAM(NFALG3+3)
      BHS     = UPARAM(NFALG3+4)
      MHS     = UPARAM(NFALG3+5)
      EPS0HS  = UPARAM(NFALG3+6)
      NHS     = UPARAM(NFALG3+7)
      HMART   = UPARAM(NFALG3+8)
      TEMP0   = UPARAM(NFALG3+9)
      ETA     = UPARAM(NFALG3+10)
      CP      = UPARAM(NFALG3+11)  
      TREF    = UPARAM(NFALG3+12)
      AM      = UPARAM(NFALG3+13)
      BM      = UPARAM(NFALG3+14)
      CM      = UPARAM(NFALG3+15)
      DM      = UPARAM(NFALG3+16)
      QM      = UPARAM(NFALG3+17)   
      PPM     = UPARAM(NFALG3+18)     
      E0MART  = UPARAM(NFALG3+19)     
      VM0     = UPARAM(NFALG3+20)     

      NFLAG4 = NFALG3 + 21
      FISOKIN= UPARAM(NFLAG4+1)
      IF (FISOKIN >ZERO) THEN                  
         CKH(1) = UPARAM(NFLAG4+2) 
         AKH(1) = UPARAM(NFLAG4+3) 
         CKH(2) = UPARAM(NFLAG4+4) 
         AKH(2) = UPARAM(NFLAG4+5) 
         CKH(3) = UPARAM(NFLAG4+6) 
         AKH(3) = UPARAM(NFLAG4+7) 
         CKH(4) = UPARAM(NFLAG4+8) 
         AKH(4) = UPARAM(NFLAG4+9) 
      ENDIF

      AKCK = AKH(1)*CKH(1) + AKH(2)* CKH(2) + AKH(3)*CKH(3)  + AKH(4) * CKH(4)
 
 
      ! IFLAG=1 => Swift-Voce Yld, IFLAG=0 => Tabulated Yld
c
      ! Barlat criterion parameter
      LPP11=(-TWO* AL3+  TWO*AL4 +  EIGHT*AL5 -   TWO*AL6)/NINE
      LPP12=(       AL3-FOUR*AL4- FOUR*AL5 + FOUR*AL6)/NINE
      LPP21=(FOUR*AL3-FOUR*AL4- FOUR*AL5 +        AL6)/NINE
      LPP22=(-TWO* AL3+  EIGHT*AL4+   TWO*AL5 -   TWO*AL6)/NINE
      LPP66= AL8
c
        ! Initial value
        ! Initial value
      IF (ISIGI == 0 .and. TIME == ZERO) THEN
         DO I=1,NEL
          ! Initial MARTENSITE VALUE
          UVAR(I, 9)  = VM0 
          ! Initial TEMPERATURE VALUE
          UVAR(I,10) = TEMP0
         ENDDO
      ENDIF   
c       Temperature calculation when adiabatic
c-----------------------------------------------
      IF (JTHE > 0 ) THEN        
        DO I=1,NEL     
          TEMP(I) = TEMPEL(I)
        ENDDO
      ELSE
        DO I=1,NEL     
          TEMP(I) = UVAR(I,10)
        ENDDO
      ENDIF


c-----------------------------------------------
c     COMPUTATION OF THE TRIAL STRESS TENSOR
c-----------------------------------------------
       ! Computation of the trial stress tensor
       IF (FISOKIN > ZERO) THEN 
         DO I=1,NEL
          ! CHABOCHE - ROUSSELIER BACKSTRESS CALCULATION
          SIGBXX(I) = SIGB(I,1) + SIGB(I,4) + SIGB(I,7) + SIGB(I,10)
          SIGBYY(I) = SIGB(I,2) + SIGB(I,5) + SIGB(I,8) + SIGB(I,11)
          SIGBXY(I) = SIGB(I,3) + SIGB(I,6) + SIGB(I,9) + SIGB(I,12)

          SIGNXX(I) = SIGOXX(I) - SIGBXX(I) + A1*DEPSXX(I) + A2*DEPSYY(I)
          SIGNYY(I) = SIGOYY(I) - SIGBYY(I) + A2*DEPSXX(I) + A1*DEPSYY(I)
          SIGNXY(I) = SIGOXY(I) - SIGBXY(I) + G*DEPSXY(I)  
        ENDDO
      ELSE
        DO I=1,NEL
          SIGBXX(I) = ZERO 
          SIGBYY(I) = ZERO 
          SIGBXY(I) = ZERO
          SIGNXX(I)  = SIGOXX(I) + A1*DEPSXX(I) + A2*DEPSYY(I)
          SIGNYY(I)  = SIGOYY(I) + A2*DEPSXX(I) + A1*DEPSYY(I)
          SIGNXY(I)  = SIGOXY(I) + G *DEPSXY(I)
        ENDDO
      ENDIF


      DO I=1,NEL
        SIGNYZ(I)  = SIGOYZ(I) + GS(I)*DEPSYZ(I)
        SIGNZX(I)  = SIGOZX(I) + GS(I)*DEPSZX(I)
        ! Sound speed and tangent modulus
        SOUNDSP(I) = SQRT(A1/RHO0(I))
        ETSE(I)    = ONE
        DPLA(I)    = ZERO
        DEPSZZ(I)  = ZERO
        DEPLZZ(I)  = ZERO
      ENDDO
c-----------------------------------------------
       ! hansel hardening
C-------------------------------------------------------------------------
C-------------------------------------------------------------------------

        PUI_1 = EXP(NHS*LOG(EPS0HS))
        AEXP0  = (BHS-AHS)* EXP(-MHS*PUI_1)    

        IF (FISOKIN == ZERO) THEN
           !PURE ISOTROPIC HARDENING                                 
           DO I=1,NEL
             VM(I) = UVAR(I,9)
             ATEMP(I)   = (K1 + K2* TEMP(I))
             IF((PLA(I)+EPS0HS)>ZERO) THEN
               PUI_1 = EXP(NHS*LOG((PLA(I)+EPS0HS)))
             ELSE
               PUI_1 = ZERO
             ENDIF                                                                            
             AEXP(I)  = (BHS - AHS)* EXP(-MHS*PUI_1)                                  
             YLD(I)   = (BHS - AEXP(I)) * ATEMP(I)+ HMART*VM(I)
           ENDDO    
        ELSEIF (FISOKIN == ONE) THEN     
           !PURE KINEMATIC HARDENING                                 
           DO I=1,NEL
             VM(I)      = UVAR(I,9)
             ATEMP(I)   = (K1 + K2* TEMP(I))
             YLD0(I)    = (BHS - AEXP0) * ATEMP(I)+ HMART*VM(I) !EXP(NHS*LOG(EPS0HS))
             YLD(I)     = YLD0(I) 
           ENDDO                                
        ELSE                              
           !COMBINED ISOTROPIC KINEMATIC HARDENING WEIGHTENED WITH FISOKIN                              
           DO I=1,NEL
             VM(I)      = UVAR(I,9)
             ATEMP(I)   = (K1 + K2* TEMP(I))
             YLD0(I)    = (BHS - AEXP0) * ATEMP(I)+ HMART*VM(I)
             IF((PLA(I)+EPS0HS)>ZERO) THEN
               PUI_1 = EXP(NHS*LOG((PLA(I)+EPS0HS)))
             ELSE
               PUI_1 = ZERO
             ENDIF                                                                              
             AEXP(I)  = (BHS-AHS)* EXP(-MHS*PUI_1)                                    
             YLD(I)   = (BHS - AEXP(I)) * ATEMP(I)+ HMART*VM(I)
             YLD(I)   = (ONE - FISOKIN) * YLD(I) + FISOKIN * YLD0(I)             
           ENDDO                                
        ENDIF                            
C
        ! Computation of the Barlat equivalent stress
        DO I=1,NEL
c
          YLD_NORM(I) = YLD(I)*NORM_SP      
          !BARLAT EFFECTIVE STRESS IS COMPUTED WITH (SIGMA - ALPHA) IF ALPHA NOT ZERO (KINEMATIC HARDENING)
          XPXX(I) = (TWO*AL1*SIGNXX(I)-AL1*SIGNYY(I))/THREE
          XPYY(I) =(-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I))/THREE
          XPXY(I) = AL7*SIGNXY(I)
c       
          XPPXX(I) = (LPP11*SIGNXX(I) + LPP12*SIGNYY(I))
          XPPYY(I) = (LPP21*SIGNXX(I) + LPP22*SIGNYY(I))
          XPPXY(I) = LPP66*SIGNXY(I)
c       
          XP1(I) = (XPXX(I)+XPYY(I)+SQRT(MAX(ZERO,
     .             (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .            + FOUR*XPXY(I)*XPXY(I))))*HALF
          XP2(I) = (XPXX(I)+XPYY(I)-SQRT(MAX(ZERO,
     .             (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .            + FOUR*XPXY(I)*XPXY(I))))*HALF
c       
          XPP1(I) = (XPPXX(I)+XPPYY(I)+SQRT(MAX(ZERO,
     .              (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .             + FOUR*XPPXY(I)*XPPXY(I) ) ))*HALF
          XPP2(I) = (XPPXX(I)+XPPYY(I)-SQRT(MAX(ZERO,
     .              (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .             + FOUR*XPPXY(I)*XPPXY(I))))*HALF
c
          PHIP(I) = (ABS( XP1(I)-XP2(I)      ) *NORM_SP)**EXPA
          PHIPP(I)= (ABS( TWO*XPP2(I)+XPP1(I)) *NORM_SP)**EXPA
     .             +(ABS( TWO*XPP1(I)+XPP2(I)) *NORM_SP)**EXPA
c       
          IF ((HALF*(PHIP(I)+PHIPP(I)))>ZERO) THEN
            SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))))
          ELSE
            SIG(I) = ZERO
          ENDIF
        ENDDO    
c-----------------------------------------------
c       CHECKING THE YIELD CONDITION
c-----------------------------------------------
        NINDX  = 0
        DO I=1,NEL
          IF (SIG(I) > YLD_NORM(I) .AND. OFF(I) == ONE) THEN ! Plastic Loading
            NINDX = NINDX + 1
            INDX(NINDX)  = I 
          ENDIF
        ENDDO
c-----------------------------------------------
c       COMPUTING THE RETURN MAPPING
c-----------------------------------------------
        IF ( FISOKIN > ZERO .AND. NINDX > 0) THEN
          DAEXP0 = -NHS * MHS * EXP((NHS-ONE)*LOG(EPS0HS))
          DO II=1,NINDX                                              
            I = INDX(II)                                      
            YLDP0(I)   = - ATEMP(I)* AEXP0 *  DAEXP0       
            YLDP0(I)   =   ZERO ! FISOKIN*YLDP0(I)*NORM_SP
          ENDDO
        ELSE
          DO II=1,NINDX                                              
            I = INDX(II)                                      
            YLDP0(I) = ZERO
          ENDDO
        ENDIF
        DO II=1,NINDX                                              
          I = INDX(II)  
          ! Initialization of the plastic strain                                           
          DEPLXX(I)=ZERO
          DEPLYY(I)=ZERO
          DEPLXY(I)=ZERO     
          DSIGBXX(I) = ZERO
          DSIGBYY(I) = ZERO
          DSIGBXY(I) = ZERO
          IF(FISOKIN > ZERO) THEN
              DSIGBOXX(I) = CKH(1)*SIGB(I,1) + CKH(2)*SIGB(I,4) + CKH(3)*SIGB(I,7) + CKH(4)*SIGB(I,10)
              DSIGBOYY(I) = CKH(1)*SIGB(I,2) + CKH(2)*SIGB(I,5) + CKH(3)*SIGB(I,8) + CKH(4)*SIGB(I,11)
              DSIGBOXY(I) = CKH(1)*SIGB(I,3) + CKH(2)*SIGB(I,6) + CKH(3)*SIGB(I,9) + CKH(4)*SIGB(I,12)
          ENDIF !(FISOKIN > ZERO) 
          ! yield criterion  
          FCT = SIG(I)/NORM_SP - YLD(I)
          ! Iteration loop
          DO K = 1,NITER
            !algo newton
            !calcul de dsigma/ddpla=dphi/dsigma*dsigma/ddpla
            !phi = phip + phipp
            !dphip/dsigma = dphip/dxp*dxp/dsigma  where  dxp/dsigma = Lp
C
            ! Derivative of the HANSEL yield stress
            IF((PLA(I)+EPS0HS)>ZERO) THEN                                   
              PUI_1 = EXP(NHS*LOG((PLA(I)+EPS0HS)))
            ELSE
              PUI_1 = ZERO
            ENDIF

            AEXP(I)  = (BHS-AHS)* EXP(-MHS*PUI_1)                                         
            DAEXP(I) = -NHS * PUI_1 * MHS /(PLA(I)+EPS0HS)
            YLDP     = - ATEMP(I)* AEXP(I) *  DAEXP(I)        
            YLDP     = (ONE-FISOKIN)*YLDP 
c
            ! Computation of the normal to the yield surface
            NORMSIG = SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I) 
     .              + TWO*SIGNXY(I)*SIGNXY(I)
            IF (NORMSIG > ZERO) THEN
              NORMSIG = SQRT(NORMSIG)
            ELSE
              NORMSIG = ONE
            ENDIF
c         
            XPXX(I) = (TWO*AL1*SIGNXX(I)-AL1*SIGNYY(I)) /NORMSIG/THREE
            XPYY(I) =(-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I)) /NORMSIG/THREE
            XPXY(I) = AL7*(SIGNXY(I)/NORMSIG)
c       
            XPPXX(I) = (LPP11*SIGNXX(I) + LPP12*SIGNYY(I))/NORMSIG
            XPPYY(I) = (LPP21*SIGNXX(I) + LPP22*SIGNYY(I))/NORMSIG
            XPPXY(I) = LPP66*SIGNXY(I)/NORMSIG
c       
            XP1(I) =(XPXX(I)+XPYY(I)+SQRT(MAX(ZERO,
     .            (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .            +FOUR*XPXY(I)*XPXY(I))))*HALF
            XP2(I) =(XPXX(I)+XPYY(I)-SQRT(MAX(ZERO,
     .            (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .            +FOUR*XPXY(I)*XPXY(I))))*HALF
c       
            XPP1(I)=(XPPXX(I)+XPPYY(I)+SQRT(MAX(ZERO,
     .            (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .            +FOUR*XPPXY(I)*XPPXY(I) ) ))*HALF
            XPP2(I)=(XPPXX(I)+XPPYY(I)-SQRT(MAX(ZERO,
     .            (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .            +FOUR*XPPXY(I)*XPPXY(I))))*HALF
c     
            PHIP(I) = ABS(XP1(I)-XP2(I))**EXPA
            PHIPP(I)= ABS(TWO*XPP2(I)+XPP1(I))**EXPA
     .              + ABS(TWO*XPP1(I)+XPP2(I))**EXPA
c       
            IF ((HALF*(PHIP(I)+PHIPP(I))) > ZERO) THEN
              SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))))
            ELSE
              SIG(I) = ZERO
            ENDIF            
c            
            AP = EXPA*(XP1(I)-XP2(I))*ABS(XP1(I)-XP2(I))**EXPAM2
            DP = (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))+FOUR*XPXY(I)*XPXY(I)
c         
            BPP = EXPA*(TWO*XPP2(I)+XPP1(I))*ABS(TWO*XPP2(I)+XPP1(I))**EXPAM2
            CPP = EXPA*(TWO*XPP1(I)+XPP2(I))*ABS(TWO*XPP1(I)+XPP2(I))**EXPAM2
            DPP = (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))+FOUR*XPPXY(I)*XPPXY(I)
            !phip
            FP1 = AP*(XPXX(I)-XPYY(I))/SQRT(MAX(EM20,DP))
            FP2 = -FP1
            FP3 = AP*FOUR*XPXY(I)/SQRT(MAX(EM20,DP))
c        
            KINT = HALF*(XPPXX(I)-XPPYY(I))/SQRT(MAX(EM20,DPP))
c        
            FPP1 = BPP*(THREE_HALF-KINT)+CPP*(THREE_HALF+KINT) 
            FPP2 = BPP*(THREE_HALF+KINT)+CPP*(THREE_HALF-KINT)
            FPP3 = TWO*(CPP-BPP)*XPPXY(I)/SQRT(MAX(EM20,DPP))
c        
            !dphip/dsigma = dphip/dxp*dxp/dsigma  where  dxp/dsigma = Lp
            DFP1 = THIRD*(TWO*AL1*FP1-AL2*FP2)
            DFP2 = THIRD*(TWO*AL2*FP2-AL1*FP1)
            DFP3 = AL7*FP3
c        
            !dphipP/dsigma = dphipP/dxpP*dxpP/dsigma  where  dxpP/dsigma = LpP
            DFPP1 = FPP1*LPP11+FPP2*LPP21
            DFPP2 = FPP1*LPP12+FPP2*LPP22
            DFPP3 = FPP3*LPP66
c
            PUI_1 = SIG(I)**(EXPA-1)
            DU    = ONE/(TWO*EXPA*PUI_1)
            ! df/dsigma = dphip/dsigma+dphipP/dsigma
            DF1   = DFP1+DFPP1
            DF2   = DFP2+DFPP2
            DF3   = DFP3+DFPP3 ! df/dsigma = dphip/dsigma+dphipP/dsigma
c
c          ! DF/DSIG = normal
            NORMXX  = DF1 *DU  
            NORMYY  = DF2 *DU
            NORMXY  = DF3 *DU
            !ddsig/d dlam                                    
            DSDEPL1 = - (A1*NORMXX + A2*NORMYY)              
            DSDEPL2 = - (A1*NORMYY + A2*NORMXX)              
            DSDEPL3 = -  G *NORMXY                           
            !Derivative of dPLA with respect to DLAM         
            !-------------------------------------------     
            SIG_DFDSIG = SIGNXX(I) * NORMXX                  
     .                  + SIGNYY(I) * NORMYY                 
     .                  + SIGNXY(I) * NORMXY                 
            DPLA_DLAM(I) = SIG_DFDSIG/YLD(I)                 
C                                                           
            DADEPL1 = ZERO                                                       
            DADEPL2 = ZERO                                                       
            DADEPL3 = ZERO                                                       
            IF(FISOKIN > ZERO) THEN                                              
              !Dalpha/d dlam derivative of backstress                            
              DADEPL1 =  FISOKIN * (DSIGBOXX(I) * DPLA_DLAM(I) - AKCK * NORMXX)  
              DADEPL2 =  FISOKIN * (DSIGBOYY(I) * DPLA_DLAM(I) - AKCK * NORMYY)  
              DADEPL3 =  FISOKIN * (DSIGBOXY(I) * DPLA_DLAM(I) - AKCK * NORMXY)  
            ENDIF                                                                

            DF = NORMXX * (DSDEPL1 + DADEPL1)                                    
     .       + NORMYY * (DSDEPL2 + DADEPL2)                                      
     .       + NORMXY * (DSDEPL3 + DADEPL3)                                      
            DF = DF  - YLDP

            DF = SIGN(MAX(ABS(DF),EM20) ,DF)   
c
            ! Plastic multiplier               
            DLAM = -FCT/DF                     
            ! Plastic strain tensor increment  
            DEPLXX(I) = DLAM * NORMXX          
            DEPLYY(I) = DLAM * NORMYY          
            DEPLXY(I) = DLAM * NORMXY          
            DEPLZZ(I) = DEPLZZ(I) - (DEPLXX(I)+DEPLYY(I))

c
            ! Updating the plastic strain and the plastic strain rate         
            DDEP    = DLAM * DPLA_DLAM(I)          
            DPLA(I) = MAX(ZERO, DPLA(I) + DDEP)    
            PLA(I)  = PLA(I)  + DDEP               
            IF (ISRATE == 0) PLAP(I) = DPLA(I)/MAX(TIMESTEP,EM20)  
c
            !Updating the backstress                              
            IF(FISOKIN >ZERO) THEN                                
              DSIGBXX(I) = AKCK * DEPLXX(I) - DSIGBOXX(I) * DDEP  
              DSIGBYY(I) = AKCK * DEPLYY(I) - DSIGBOYY(I) * DDEP  
              DSIGBXY(I) = AKCK * DEPLXY(I) - DSIGBOXY(I) * DDEP  
            ENDIF

            !Updating the stress tensor
            SIGNXX(I) = SIGNXX(I) - A1*DEPLXX(I) - A2*DEPLYY(I) - FISOKIN*DSIGBXX(I)             
            SIGNYY(I) = SIGNYY(I) - A2*DEPLXX(I) - A1*DEPLYY(I) - FISOKIN*DSIGBYY(I)                
            SIGNXY(I) = SIGNXY(I) - G*DEPLXY(I)  - FISOKIN*DSIGBXY(I)
            IF (FISOKIN > ZERO ) THEN                                                                         
             !BACKSTRESS 1                                                                                    
             SIGB(I,1) = SIGB(I,1) + FISOKIN*(AKH(1)*CKH(1) * DEPLXX(I) - CKH(1) * SIGB(I,1) * DDEP)          
             SIGB(I,2) = SIGB(I,2) + FISOKIN*(AKH(1)*CKH(1) * DEPLYY(I) - CKH(1) * SIGB(I,2) * DDEP)          
             SIGB(I,3) = SIGB(I,3) + FISOKIN*(AKH(1)*CKH(1) * DEPLXY(I) - CKH(1) * SIGB(I,3) * DDEP)          
             !BACKSTRESS 2                                                                                    
             SIGB(I,4) = SIGB(I,4) + FISOKIN*(AKH(2)*CKH(2) * DEPLXX(I) - CKH(2) * SIGB(I,4) * DDEP)          
             SIGB(I,5) = SIGB(I,5) + FISOKIN*(AKH(2)*CKH(2) * DEPLYY(I) - CKH(2) * SIGB(I,5) * DDEP)          
             SIGB(I,6) = SIGB(I,6) + FISOKIN*(AKH(2)*CKH(2) * DEPLXY(I) - CKH(2) * SIGB(I,6) * DDEP)          
             !BACKSTRESS 3                                                                                    
             SIGB(I,7) = SIGB(I,7) + FISOKIN*(AKH(3)*CKH(3) * DEPLXX(I) - CKH(3) * SIGB(I,7) * DDEP)          
             SIGB(I,8) = SIGB(I,8) + FISOKIN*(AKH(3)*CKH(3) * DEPLYY(I) - CKH(3) * SIGB(I,8) * DDEP)          
             SIGB(I,9) = SIGB(I,9) + FISOKIN*(AKH(3)*CKH(3) * DEPLXY(I) - CKH(3) * SIGB(I,9) * DDEP)          
             !BACKSTRESS 4                                                                                    
             SIGB(I,10) =SIGB(I,10)+ FISOKIN*(AKH(4)*CKH(4) * DEPLXX(I) - CKH(4) *SIGB(I,10) * DDEP)          
             SIGB(I,11) =SIGB(I,11)+ FISOKIN*(AKH(4)*CKH(4) * DEPLYY(I) - CKH(4) *SIGB(I,11) * DDEP)          
             SIGB(I,12) =SIGB(I,12)+ FISOKIN*(AKH(4)*CKH(4) * DEPLXY(I) - CKH(4) *SIGB(I,12) * DDEP)          
            ENDIF                                                                                             
c
            ! Updating the Barlat equivalent stress
            XPXX(I) = (TWO*AL1*SIGNXX(I)- AL1*SIGNYY(I))*NORM_SP/THREE
            XPYY(I) = (-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I))*NORM_SP/THREE
            XPXY(I) = AL7*SIGNXY(I)*NORM_SP
c       
            XPPXX(I) = (LPP11*SIGNXX(I) + LPP12*SIGNYY(I))*NORM_SP
            XPPYY(I) = (LPP21*SIGNXX(I) + LPP22*SIGNYY(I))*NORM_SP
            XPPXY(I) = LPP66*SIGNXY(I)*NORM_SP
c       
            XP1(I) = (XPXX(I)+XPYY(I)+SQRT(MAX(ZERO,
     .             (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .             + FOUR*XPXY(I)*XPXY(I))))*HALF
            XP2(I) = (XPXX(I)+XPYY(I)-SQRT(MAX(ZERO,
     .             (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .             + FOUR*XPXY(I)*XPXY(I))))*HALF
c       
            XPP1(I) = (XPPXX(I)+XPPYY(I)+SQRT(MAX(ZERO,
     .             (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .             + FOUR*XPPXY(I)*XPPXY(I))))*HALF
            XPP2(I) = (XPPXX(I)+XPPYY(I)-SQRT(MAX(ZERO,
     .             (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .             + FOUR*XPPXY(I)*XPPXY(I))))*HALF
            PHIP(I) = ABS(XP1(I)-XP2(I))**EXPA
            PHIPP(I) = ABS(TWO*XPP2(I)+XPP1(I))**EXPA
     .               + ABS(TWO*XPP1(I)+XPP2(I))**EXPA
            IF ((PHIP(I)+PHIPP(I)) > ZERO) THEN
              SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))))
            ELSE
              SIG(I) = ZERO
            ENDIF 
c
            ! Updating the yield stress
            IF((PLA(I)+EPS0HS)>ZERO) THEN
              PUI_1 = EXP(NHS*LOG((PLA(I)+EPS0HS)))
            ELSE
              PUI_1 = ZERO
            ENDIF                                                                               
            AEXP(I)  = (BHS-AHS)* EXP(-MHS*PUI_1)                                     
            YLD(I)   = (BHS - AEXP(I)) * ATEMP(I)+ HMART*VM(I)
            IF (FISOKIN > ZERO)YLD(I)   = (ONE - FISOKIN) * YLD(I) + FISOKIN * YLD0(I) 
  
c
            ! Updating the yield function value
            FCT = SIG(I)/NORM_SP - YLD(I)
c
          ENDDO   ! NEWTON ITERATIONS
        ENDDO     ! NINDX
C-------------------------------------------------------------------------
C-------------------------------------------------------------------------
      IF (FISOKIN > ZERO ) THEN
        DO I=1,NEL
C
          SIGBXX(I) = SIGB(I,1) + SIGB(I,4) + SIGB(I,7) + SIGB(I,10)
          SIGBYY(I) = SIGB(I,2) + SIGB(I,5) + SIGB(I,8) + SIGB(I,11)
          SIGBXY(I) = SIGB(I,3) + SIGB(I,6) + SIGB(I,9) + SIGB(I,12)
          SIGNXX(I) = SIGNXX(I) + SIGBXX(I) !+ DSIGBXX(I)
          SIGNYY(I) = SIGNYY(I) + SIGBYY(I) !+ DSIGBYY(I)
          SIGNXY(I) = SIGNXY(I) + SIGBXY(I) !+ DSIGBXY(I) 
       ENDDO
      ENDIF
C=======================================================================
c     Temperature if adiabatic (jthe=0)
C=======================================================================
      IF (JTHE == 0 ) THEN        
        DO I=1,NEL 
        
c          IF (PLAP(I) <= DEPS0)THEN
c            OMEGA = ZERO
c          ELSEIF (PLAP(I) > DEPSAD) THEN
c            OMEGA = ONE
c          ELSE
c            OMEGA = ((PLAP(I)-DEPS0)**2 )*
c     .              (THREE*DEPSAD - TWO*PLAP(I) - DEPS0)
c     .             /((DEPSAD-DEPS0)**3)
c          ENDIF
          WP(I)   = YLD(I)*DPLA(I)
          TEMP(I) = UVAR(I,10) + WP(I)*ETA/(RHO0(I)*CP)
        ENDDO
      ENDIF
C=======================================================================
C     COMPUTE MARTENSITE FRACTION FOR HANSEL HARDENING   
C=======================================================================
C=======================================================================
C     COMPUTE MARTENSITE FRACTION WHEN HANSEL HARDENING IS USED  
      DO I=1,NEL 
         VM(I)= UVAR(I,9) 
         DVM  = ZERO
         IF (DPLA(I) /= ZERO .AND. PLA(I) >= E0MART. AND . TEMP(I) >ZERO) THEN
           TERM1    = (BM * HALF*(ONE - TANH(CM + DM*TEMP(I)) )* EXP(QM/TEMP(I)))/AM
           IF(VM(I)>ZERO.AND.VM(I) < ONE) THEN
              PUI_1 = EXP(PPM*LOG(VM(I)))
              PUI_2 = EXP( ((BM+ONE)/BM)* LOG( (ONE-VM(I))/VM(I) )   )
           ELSE
              PUI_1 = ZERO
              PUI_2 = ZERO
           ENDIF                                                                               
           DVM   = TERM1 * PUI_1 * PUI_2
           VM(I) = VM(I) + MAX(DVM*DPLA(I),ZERO)
           VM(I) = MAX(MIN(VM(I), ONE),ZERO)
           UVAR(I,9) = VM(I)
        ENDIF
      ENDDO ! I=1,NEL 
c
      DO I=1,NEL
        UVAR(I,10) = TEMP(I)
        UVAR(I,2)  = YLD(I)
        SEQ(I)     = SIG(I)/NORM_SP
        DEELZZ(I ) = -NU*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E
        IF (INLOC > 0) THEN
          NORMSIG = SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I) 
     .            + TWO*SIGNXY(I)*SIGNXY(I)
          IF (NORMSIG > ZERO) THEN
            NORMSIG = MAX(SQRT(NORMSIG*NORM_SP),EM20)
          ELSE
            NORMSIG = ONE
          ENDIF
          XPXX(I)  = (TWO*AL1*SIGNXX(I)-AL1*SIGNYY(I)) *NORM_SP/NORMSIG/THREE
          XPYY(I)  = (-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I)) *NORM_SP/NORMSIG/THREE
          XPXY(I)  = AL7*(SIGNXY(I)*NORM_SP/NORMSIG)
          XPPXX(I) = (LPP11*SIGNXX(I) + LPP12*SIGNYY(I))*NORM_SP/NORMSIG
          XPPYY(I) = (LPP21*SIGNXX(I) + LPP22*SIGNYY(I))*NORM_SP/NORMSIG
          XPPXY(I) = LPP66*SIGNXY(I)*NORM_SP/NORMSIG
          XP1(I)   = (XPXX(I)+XPYY(I)+SQRT(MAX(ZERO,
     .               (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .               + FOUR*XPXY(I)*XPXY(I))))*HALF
          XP2(I)   = (XPXX(I)+XPYY(I)-SQRT(MAX(ZERO,
     .               (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .               + FOUR*XPXY(I)*XPXY(I))))*HALF
          XPP1(I)  = (XPPXX(I)+XPPYY(I)+SQRT(MAX(ZERO,
     .               (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .               + FOUR*XPPXY(I)*XPPXY(I) ) ))*HALF
          XPP2(I)  = (XPPXX(I)+XPPYY(I)-SQRT(MAX(ZERO,
     .               (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .               + FOUR*XPPXY(I)*XPPXY(I))))*HALF
          PHIP(I)  = ABS(XP1(I)-XP2(I))**EXPA
          PHIPP(I) = ABS(TWO*XPP2(I)+XPP1(I))**EXPA
     .             + ABS(TWO*XPP1(I)+XPP2(I))**EXPA
          IF ((HALF*(PHIP(I)+PHIPP(I))) > ZERO) THEN
            SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))))
          ELSE
            SIG(I) = ZERO
          ENDIF               
          AP    = EXPA*(XP1(I)-XP2(I))*ABS(XP1(I)-XP2(I))**EXPAM2
          DP    = (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))+FOUR*XPXY(I)*XPXY(I)
          BPP   = EXPA*(TWO*XPP2(I)+XPP1(I))*ABS(TWO*XPP2(I)+XPP1(I))**EXPAM2
          CPP   = EXPA*(TWO*XPP1(I)+XPP2(I))*ABS(TWO*XPP1(I)+XPP2(I))**EXPAM2
          DPP   = (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))+FOUR*XPPXY(I)*XPPXY(I)
          !phip
          FP1   = AP*(XPXX(I)-XPYY(I))/SQRT(MAX(EM20,DP))
          FP2   = -FP1
          FP3   = AP*FOUR*XPXY(I)/SQRT(MAX(EM20,DP))
          KINT  = HALF*(XPPXX(I)-XPPYY(I))/SQRT(MAX(EM20,DPP))
          FPP1  = BPP*(THREE_HALF-KINT)+CPP*(THREE_HALF+KINT) 
          FPP2  = BPP*(THREE_HALF+KINT)+CPP*(THREE_HALF-KINT)
          FPP3  = TWO*(CPP-BPP)*XPPXY(I)/SQRT(MAX(EM20,DPP))  
          DFP1  = THIRD*(TWO*AL1*FP1-AL2*FP2)
          DFP2  = THIRD*(TWO*AL2*FP2-AL1*FP1)
          DFP3  = AL7*FP3
          DFPP1 = FPP1*LPP11+FPP2*LPP21
          DFPP2 = FPP1*LPP12+FPP2*LPP22
          DFPP3 = FPP3*LPP66
          DF1   = DFP1+DFPP1
          DF2   = DFP2+DFPP2
          DF3   = DFP3+DFPP3
          SIG_DFDSIG = SIGNXX(I)*DF1 + SIGNYY(I)*DF2 + SIGNXY(I)*DF3
          IF (SIG_DFDSIG /= ZERO) THEN 
            DEPLZZ(I) = -MAX(DPLANL(I),ZERO)*(YLD(I)/SIG_DFDSIG)*(DF1+DF2)
          ELSE
            DEPLZZ(I) = ZERO
          ENDIF
        ENDIF
        DEPSZZ(I)  = DEELZZ(I) + DEPLZZ(I)
        THK(I)     = THK(I) + DEPSZZ(I)*THKLY(I)*OFF(I)
      ENDDO
c-----------      
      RETURN
      END