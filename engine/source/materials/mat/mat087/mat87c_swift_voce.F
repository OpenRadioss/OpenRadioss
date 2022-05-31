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
Chd|====================================================================
Chd|  MAT87C_SWIFT_VOCE             source/materials/mat/mat087/mat87c_swift_voce.F
Chd|-- called by -----------
Chd|        SIGEPS87C                     source/materials/mat/mat087/sigeps87c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAT87C_SWIFT_VOCE(
     1     NEL    ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP ,
     3     UPARAM ,UVAR    ,RHO0    ,THKLY   ,THK      ,
     4     EPSPXX ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     5     DEPSXX ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,
     6     SIGOXX ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     8     SIGNXX ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     9     SOUNDSP,PLA     ,DPLA    ,EPSP    ,YLD      ,
     B     ETSE   ,GS      ,ISRATE  ,ASRATE  ,OFF      ,
     C     SIGB   ,INLOC   ,DPLANL  )
C=======================================================================
C   BARLAT YLD2000 material model iflag=1 swift voce yield
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
      INTEGER NEL, NUPARAM, NUVAR,ISRATE,INLOC
      my_real 
     .   TIME,TIMESTEP,ASRATE,UPARAM(NUPARAM),
     .   RHO0(NEL),GS(NEL),
     .   THKLY(NEL),PLA(NEL),EPSPXX(NEL),EPSPYY(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DPLANL(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),FACYLDI(NEL),
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
     .   UVAR(NEL,NUVAR),OFF(NEL),THK(NEL),
     .   YLD(NEL),DPLA(NEL),EPSP(NEL),TEMPEL(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,K,NRATE,NITER,JJ(NEL),J1,J2,N,NINDX,NFALG3,
     .        IFLAG,IFLAGSR,EXPA,EXPAM2,NFLAG4,
     .        INDX(NEL), IPOS1(NEL),ILEN1(NEL),IAD1(NEL),
     .        IPOS2(NEL),ILEN2(NEL),IAD2(NEL)
      my_real
     .        Y1(NEL),Y2(NEL),DYDX(NEL),DYDX1(NEL),DYDX2(NEL),
     .        XPXX(NEL),XPYY(NEL),XPXY(NEL),
     .        XPPXX(NEL),XPPYY(NEL),XPPXY(NEL),PHIP(NEL),PHIPP(NEL),
     .        DEPLXX(NEL),DEPLYY(NEL),DEPLXY(NEL),DEPLZZ(NEL),
     .        DEELZZ(NEL),DEPSZZ(NEL),XP1(NEL),XP2(NEL),XPP1(NEL),
     .        XPP2(NEL),SIG(NEL),H(NEL),SIGTRXX(NEL),SIGTRYY(NEL),
     .        SIGTRXY(NEL),DAEXP(NEL)
      my_real
     .      E,NU,BULK,A1,A2,G,UNSA,DU,R,
     .      AL1, AL2 , AL3 , AL4 ,DSIGPL1,DSPL1,
     .      AL5, AL6 , AL7 , AL8 ,DSIGPL2,DSPL2,
     .      AL9, AL10, AL11, AL12,DSIGPL3,DSPL3,
     .      YSCALE,CEPS,EPS0,EXPN,TERM1,
     .      LPP11,LPP12,LPP21,LPP22,LPP66,
     .      FCTP,BPP,CPP,DPP,FP1,FP2,FP3,
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
     .        VM(NEL)
      my_real
     .        YFAC(NEL,2),PUI_1,PUI_2
      my_real NORM_SP,FCT,DF,DLAM,YLD_NORM(NEL)
      my_real
     .        DSIGBXX(NEL),DSIGBYY(NEL),DSIGBXY(NEL),
     .        SIGBXX(NEL) ,SIGBYY(NEL) ,SIGBXY(NEL),
     .        DSIGBNXX(NEL),DSIGBNYY(NEL),DSIGBNXY(NEL),
     .        YLDP0(NEL),KSWIFT0,SIG_DFDSIG,
     .        DADEPL1,DADEPL2,DADEPL3,FISOKIN,
     .        AKCK,CKH(4) ,AKH(4)  ,AEXP0 ,YLD0(NEL)
c=======================================================================
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
c=======================================================================
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
      ! Yield stress parameters
      ALPHA   =  UPARAM(16) 
      NEXP    =  UPARAM(17)
      EPSO    =  UPARAM(18) 
      ASWIFT  =  UPARAM(19) 
      QVOCE = UPARAM(25+1)
      BETA  = UPARAM(25+2)
      KO    = UPARAM(25+3)
      UNSP  = UPARAM(25+4)
      UNSC  = UPARAM(25+5)
      UNSPT  = UPARAM(25+6)
      UNSCT  = UPARAM(25+7)
c
      IFLAG   = NINT(UPARAM(24))            
      IFLAGSR = NINT(UPARAM(25+8)) 

 
      NFALG3 = 34 
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
      ! IFLAGSR=0 => total strain rate,  IFLAGSR=1 => plastic strain rate 
c
      ! Barlat criterion parameter
      LPP11=(-TWO* AL3+  TWO*AL4 +  EIGHT*AL5 -   TWO*AL6)/NINE
      LPP12=(       AL3-FOUR*AL4- FOUR*AL5 + FOUR*AL6)/NINE
      LPP21=(FOUR*AL3-FOUR*AL4- FOUR*AL5 +        AL6)/NINE
      LPP22=(-TWO* AL3+  EIGHT*AL4+   TWO*AL5 -   TWO*AL6)/NINE
      LPP66= AL8
c


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
c     COMPUTATION OF STRAIN-RATE
c-----------------------------------------------
      IF (ISRATE == 0 .and. IFLAGSR == 0) THEN
        DO I=1,NEL
           EPSP(I) = HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .             + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .             + EPSPXY(I)*EPSPXY(I) ) )
        ENDDO
      ELSE IF (IFLAGSR == 1) THEN
        PLAP(:NEL) = UVAR(:NEL,1)
      ENDIF
c-----------------------------------------------
c     FORMULATION FLAG
c-----------------------------------------------
c       ! Swift-Voce (analytical yield formulation) + Cowper-Symonds strain rate

        ! Computation of the yield stress                                                  
      IF (FISOKIN == ZERO) THEN
        DO I=1,NEL
          EXPV   = EXP(-BETA*PLA(I))
          IF((PLA(I) + EPSO)>ZERO) THEN
            PUI_1 = EXP(NEXP*LOG((PLA(I) + EPSO)))
          ELSE
            PUI_1 = ZERO
          ENDIF                                                                               
          KSWIFT   = ASWIFT*PUI_1
          KVOCE    = KO + QVOCE*(ONE - EXPV)                                       
          YLD(I)   = ALPHA*KSWIFT + (ONE-ALPHA)*KVOCE 
          FRATE(I) = ONE 
        ENDDO    
      ELSEIF (FISOKIN == ONE) THEN     
        !PURE KINEMATIC HARDENING                                 
        PUI_1     = EXP(NEXP*LOG( EPSO))
        KSWIFT0   = ASWIFT*PUI_1
        DO I=1,NEL                                      
          YLD0(I)   = ALPHA*KSWIFT0 + (ONE-ALPHA)*KO 
          YLD(I)    = YLD0(I)
          FRATE(I) = ONE 
        ENDDO    
      ELSE                              
        !COMBINED ISOTROPIC KINEMATIC HARDENING WEIGHTENED WITH FISOKIN                              
        PUI_1     = EXP(NEXP*LOG( EPSO))
        KSWIFT0   = ASWIFT*PUI_1
        DO I=1,NEL
          YLD0(I)   = ALPHA*KSWIFT0 + (ONE-ALPHA)*KO 
          EXPV   = EXP(-BETA*PLA(I))
          IF((PLA(I) + EPSO)>ZERO) THEN
            PUI_1 = EXP(NEXP*LOG((PLA(I) + EPSO)))
          ELSE
            PUI_1 = ZERO
          ENDIF                                                                               
          KSWIFT   = ASWIFT*PUI_1
          KVOCE    = KO + QVOCE*(ONE - EXPV)                                       
          YLD(I)   = ALPHA*KSWIFT + (ONE-ALPHA)*KVOCE 
          FRATE(I) = ONE 
          YLD(I)   = (ONE - FISOKIN) * YLD(I) + FISOKIN * YLD0(I)
        ENDDO    
      ENDIF                            

        ! Computation of the strain-rate factor
      IF (UNSPT /= ZERO )THEN 
          DO I=1,NEL
            IF (EPSP(I)/=ZERO) THEN 
              FRATE(I) = ONE + EXP(UNSPT * LOG(UNSCT*EPSP(I)))
              YLD(I)   = YLD(I)*FRATE(I) 
            ENDIF    
          ENDDO    
        ! Compute Cowper-Symonds plastic strain rate         
        ELSE IF (UNSP /= ZERO) THEN
          DO I=1,NEL
            IF (PLAP(I) > ZERO) THEN
              FRATE(I) = ONE + EXP(UNSP * LOG(UNSC*PLAP(I)))
              YLD(I)   = YLD(I)*FRATE(I) 
            ENDIF 
          ENDDO    
      ENDIF
C
        ! Computation of the Barlat equivalent stress
      DO I=1,NEL
c
          YLD_NORM(I) = YLD(I)*NORM_SP      
  
          XPXX(I) = (TWO*AL1*SIGNXX(I)-AL1*SIGNYY(I))*NORM_SP/THREE
          XPYY(I) =(-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I))*NORM_SP/THREE
          XPXY(I) = AL7*SIGNXY(I)*NORM_SP
c       
          XPPXX(I) = (LPP11*SIGNXX(I) + LPP12*SIGNYY(I))*NORM_SP
          XPPYY(I) = (LPP21*SIGNXX(I) + LPP22*SIGNYY(I))*NORM_SP
          XPPXY(I) = LPP66*SIGNXY(I)*NORM_SP
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
          PHIP(I) = ABS(XP1(I)-XP2(I))**EXPA
          PHIPP(I)= ABS(TWO*XPP2(I)+XPP1(I))**EXPA
     .           +ABS(TWO*XPP1(I)+XPP2(I))**EXPA
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
        DO II=1,NINDX                                              
          I = INDX(II)  
          ! Initialization of the plastic strain                                           
          DEPLXX(I)=ZERO
          DEPLYY(I)=ZERO
          DEPLXY(I)=ZERO     
          DSIGBXX(I) = ZERO
          DSIGBYY(I) = ZERO
          DSIGBXY(I) = ZERO
          ! yield criterion  
          FCT = SIG(I) - YLD_NORM(I)
          ! Iteration loop
          DO K = 1,NITER
            !algo newton
            !calcul de dsigma/ddpla=dphi/dsigma*dsigma/ddpla
            !phi = phip + phipp
            !dphip/dsigma = dphip/dxp*dxp/dsigma  where  dxp/dsigma = Lp
C
            ! Derivative of the yield stress
            EXPV = EXP(-BETA*PLA(I))
            IF ((PLA(I) + EPSO) > ZERO) THEN                                           
              PUI_1 = EXP(NEXP*LOG(PLA(I) + EPSO))
            ELSE
              PUI_1 = ZERO
            ENDIF
            KSWIFT = ASWIFT*PUI_1                                       
            KVOCE  = KO + QVOCE*(ONE - EXPV)
            DSWIFT = NEXP*KSWIFT/(PLA(I) + EPSO)
            DVOCE  = QVOCE*BETA*EXPV
            YLDP   = ALPHA*DSWIFT + (ONE-ALPHA)*DVOCE                
            YLDP   = (ONE-FISOKIN)*YLDP*FRATE(I)*NORM_SP
c
            ! Computation of the normal to the yield surface
            NORMSIG = SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I) 
     .              + TWO*SIGNXY(I)*SIGNXY(I)
            IF (NORMSIG > ZERO) THEN
              NORMSIG = SQRT(NORMSIG*NORM_SP)
            ELSE
              NORMSIG = ONE
            ENDIF
c         
            XPXX(I) = (TWO*AL1*SIGNXX(I)-AL1*SIGNYY(I)) *NORM_SP/NORMSIG/THREE
            XPYY(I) =(-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I)) *NORM_SP/NORMSIG/THREE
            XPXY(I) = AL7*(SIGNXY(I)*NORM_SP/NORMSIG)
c       
            XPPXX(I) = (LPP11*SIGNXX(I) + LPP12*SIGNYY(I))*NORM_SP/NORMSIG
            XPPYY(I) = (LPP21*SIGNXX(I) + LPP22*SIGNYY(I))*NORM_SP/NORMSIG
            XPPXY(I) = LPP66*SIGNXY(I)*NORM_SP/NORMSIG
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
            DF1   = DFP1+DFPP1
            DF2   = DFP2+DFPP2
            DF3   = DFP3+DFPP3
c
            !dsig/d dlam
            DSDEPL1 = -(A1*DF1+A2*DF2)*DU*NORM_SP
            DSDEPL2 = -(A2*DF1+A1*DF2)*DU*NORM_SP
            DSDEPL3 = - G*DF3*DU*NORM_SP
C            
            IF(FISOKIN > ZERO) THEN
              !Dalpha/d dlam derivative of backstress
              DSIGBNXX(I) = CKH(1)*SIGB(I,1) + CKH(2)*SIGB(I,4) + CKH(3)*SIGB(I,7) + CKH(4)*SIGB(I,10)
              DSIGBNYY(I) = CKH(1)*SIGB(I,2) + CKH(2)*SIGB(I,5) + CKH(3)*SIGB(I,8) + CKH(4)*SIGB(I,11)
              DSIGBNXY(I) = CKH(1)*SIGB(I,3) + CKH(2)*SIGB(I,6) + CKH(3)*SIGB(I,9) + CKH(4)*SIGB(I,12)
              DADEPL1 = - DSIGBNXX(I) + AKCK * DF1*DU
              DADEPL2 = - DSIGBNYY(I) + AKCK * DF2*DU
              DADEPL3 = - DSIGBNXY(I) + AKCK * DF3*DU
              DSDEPL1 = DSDEPL1 - FISOKIN*DADEPL1
              DSDEPL2 = DSDEPL2 - FISOKIN*DADEPL2
              DSDEPL3 = DSDEPL3 - FISOKIN*DADEPL3
            ENDIF
c
            DF = DF1*DSDEPL1+DF2*DSDEPL2+DSDEPL3*DF3
            DF = DF * DU - YLDP
c
            ! Plastic multiplier
            DLAM = -FCT/SIGN(MAX(ABS(DF),EM20) ,DF) 
c
            ! Updating the plastic strain and the plastic strain rate         
            DPLA(I) = DPLA(I) + DLAM
            PLA(I)  = PLA(I)  + DLAM
            !IF (ISRATE == 0) PLAP(I) = DPLA(I)/MAX(TIMESTEP,EM20)  
c
            ! Plastic strain tensor increment
            DEPLXX(I) = DLAM*DF1*DU
            DEPLYY(I) = DLAM*DF2*DU
            DEPLXY(I) = DLAM*DF3*DU
            DEPLZZ(I) = DEPLZZ(I) - (DEPLXX(I)+DEPLYY(I))

           !Updating the backstress
           IF(FISOKIN >ZERO) THEN
              DSIGBXX(I) = AKCK * DEPLXX(I) - DSIGBNXX(I) * DLAM
              DSIGBYY(I) = AKCK * DEPLYY(I) - DSIGBNYY(I) * DLAM
              DSIGBXY(I) = AKCK * DEPLXY(I) - DSIGBNXY(I) * DLAM
           ENDIF
c
            ! Updating the stress tensor
            SIGNXX(I) = SIGNXX(I) - A1*DEPLXX(I) - A2*DEPLYY(I) - FISOKIN*DSIGBXX(I)                   
            SIGNYY(I) = SIGNYY(I) - A2*DEPLXX(I) - A1*DEPLYY(I) - FISOKIN*DSIGBYY(I)                   
            SIGNXY(I) = SIGNXY(I) - G*DEPLXY(I) - FISOKIN*DSIGBXY(I)


           IF (FISOKIN > ZERO ) THEN
            !BACKSTRESS 1
            SIGB(I,1) = SIGB(I,1) + FISOKIN*(AKH(1)*CKH(1) * DEPLXX(I) - CKH(1) * SIGB(I,1) * DLAM)
            SIGB(I,2) = SIGB(I,2) + FISOKIN*(AKH(1)*CKH(1) * DEPLYY(I) - CKH(1) * SIGB(I,2) * DLAM)
            SIGB(I,3) = SIGB(I,3) + FISOKIN*(AKH(1)*CKH(1) * DEPLXY(I) - CKH(1) * SIGB(I,3) * DLAM)
            !BACKSTRESS 2          
            SIGB(I,4) = SIGB(I,4) + FISOKIN*(AKH(2)*CKH(2) * DEPLXX(I) -CKH(2) *  SIGB(I,4) * DLAM)
            SIGB(I,5) = SIGB(I,5) + FISOKIN*(AKH(2)*CKH(2) * DEPLYY(I) -CKH(2) *  SIGB(I,5) * DLAM) 
            SIGB(I,6) = SIGB(I,6) + FISOKIN*(AKH(2)*CKH(2) * DEPLXY(I) -CKH(2) *  SIGB(I,6) * DLAM) 
            !BACKSTRESS 3          
            SIGB(I,7) = SIGB(I,7) + FISOKIN*(AKH(3)*CKH(3) * DEPLXX(I) -CKH(3) *  SIGB(I,7) * DLAM)
            SIGB(I,8) = SIGB(I,8) + FISOKIN*(AKH(3)*CKH(3) * DEPLYY(I) -CKH(3) *  SIGB(I,8) * DLAM)
            SIGB(I,9) = SIGB(I,9) + FISOKIN*(AKH(3)*CKH(3) * DEPLXY(I) -CKH(3) *  SIGB(I,9) * DLAM)        
            !BACKSTRESS 4          
            SIGB(I,10) =SIGB(I,10)+ FISOKIN*(AKH(4)*CKH(4) * DEPLXX(I) - CKH(4) *SIGB(I,10) * DLAM) 
            SIGB(I,11) =SIGB(I,11)+ FISOKIN*(AKH(4)*CKH(4) * DEPLYY(I) - CKH(4) *SIGB(I,11) * DLAM)  
            SIGB(I,12) =SIGB(I,12)+ FISOKIN*(AKH(4)*CKH(4) * DEPLXY(I) - CKH(4) *SIGB(I,12) * DLAM)  
C
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
            EXPV = EXP(-BETA*PLA(I))
            IF ((PLA(I) + EPSO) > ZERO) THEN                                           
              PUI_1 = EXP(NEXP*LOG(PLA(I) + EPSO))
            ELSE
              PUI_1 = ZERO
            ENDIF
            KSWIFT = ASWIFT*PUI_1                                       
            KVOCE  = KO + QVOCE*(ONE - EXPV)                                       
            YLD(I) = ALPHA*KSWIFT + (ONE-ALPHA)*KVOCE 
            IF (FISOKIN > ZERO)YLD(I)   = (ONE - FISOKIN) * YLD(I) + FISOKIN * YLD0(I) 
            YLD(I) = YLD(I) * FRATE(I)
c
            ! Updating the yield function value
            FCT = SIG(I) - YLD(I)*NORM_SP
c
          ENDDO   ! NEWTON ITERATIONS

        ENDDO     ! NINDX



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
C============================================================
C============================================================
      IF (IFLAGSR == 1) THEN
          ASRATE = MIN(ONE, ASRATE*TIMESTEP)
          DO I=1,NEL
            PLAP(I)   = DPLA(I) / MAX(TIMESTEP,EM20)         
            PLAP(I)   = ASRATE*PLAP(I) + (ONE - ASRATE)*UVAR(I,1)
            UVAR(I,1) = PLAP(I)
          ENDDO
      ELSE
        UVAR(:NEL,1) = EPSP(:NEL)      
      ENDIF

c
      DO I=1,NEL
        UVAR(I,2)  = YLD(I)
        DEELZZ(I ) = -NU*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E
        IF (INLOC > 0) THEN
          NORMSIG = SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I) 
     .            + TWO*SIGNXY(I)*SIGNXY(I)
          IF (NORMSIG > ZERO) THEN
            NORMSIG = SQRT(NORMSIG*NORM_SP)
          ELSE
            NORMSIG = ONE
          ENDIF
          XPXX(I) = (TWO*AL1*SIGNXX(I)-AL1*SIGNYY(I)) *NORM_SP/NORMSIG/THREE
          XPYY(I) =(-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I)) *NORM_SP/NORMSIG/THREE
          XPXY(I) = AL7*(SIGNXY(I)*NORM_SP/NORMSIG)
          XPPXX(I) = (LPP11*SIGNXX(I) + LPP12*SIGNYY(I))*NORM_SP/NORMSIG
          XPPYY(I) = (LPP21*SIGNXX(I) + LPP22*SIGNYY(I))*NORM_SP/NORMSIG
          XPPXY(I) = LPP66*SIGNXY(I)*NORM_SP/NORMSIG 
          XP1(I) =(XPXX(I)+XPYY(I)+SQRT(MAX(ZERO,
     .          (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .          +FOUR*XPXY(I)*XPXY(I))))*HALF
          XP2(I) =(XPXX(I)+XPYY(I)-SQRT(MAX(ZERO,
     .          (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .          +FOUR*XPXY(I)*XPXY(I))))*HALF
          XPP1(I)=(XPPXX(I)+XPPYY(I)+SQRT(MAX(ZERO,
     .          (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .          +FOUR*XPPXY(I)*XPPXY(I) ) ))*HALF
          XPP2(I)=(XPPXX(I)+XPPYY(I)-SQRT(MAX(ZERO,
     .          (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .          +FOUR*XPPXY(I)*XPPXY(I))))*HALF
          PHIP(I) = ABS(XP1(I)-XP2(I))**EXPA
          PHIPP(I)= ABS(TWO*XPP2(I)+XPP1(I))**EXPA
     .            + ABS(TWO*XPP1(I)+XPP2(I))**EXPA
          IF ((HALF*(PHIP(I)+PHIPP(I))) > ZERO) THEN
            SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))))
          ELSE
            SIG(I) = ZERO
          ENDIF              
          AP = EXPA*(XP1(I)-XP2(I))*ABS(XP1(I)-XP2(I))**EXPAM2
          DP = (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))+FOUR*XPXY(I)*XPXY(I) 
          BPP = EXPA*(TWO*XPP2(I)+XPP1(I))*ABS(TWO*XPP2(I)+XPP1(I))**EXPAM2
          CPP = EXPA*(TWO*XPP1(I)+XPP2(I))*ABS(TWO*XPP1(I)+XPP2(I))**EXPAM2
          DPP = (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))+FOUR*XPPXY(I)*XPPXY(I)
          FP1 = AP*(XPXX(I)-XPYY(I))/SQRT(MAX(EM20,DP))
          FP2 = -FP1
          FP3 = AP*FOUR*XPXY(I)/SQRT(MAX(EM20,DP))
          KINT = HALF*(XPPXX(I)-XPPYY(I))/SQRT(MAX(EM20,DPP))
          FPP1 = BPP*(THREE_HALF-KINT)+CPP*(THREE_HALF+KINT) 
          FPP2 = BPP*(THREE_HALF+KINT)+CPP*(THREE_HALF-KINT)
          FPP3 = TWO*(CPP-BPP)*XPPXY(I)/SQRT(MAX(EM20,DPP))
          DFP1 = THIRD*(TWO*AL1*FP1-AL2*FP2)
          DFP2 = THIRD*(TWO*AL2*FP2-AL1*FP1)
          DFP3 = AL7*FP3
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
