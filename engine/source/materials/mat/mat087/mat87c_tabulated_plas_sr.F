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
Chd|  MAT87C_TABULATED_PLAS_SR      source/materials/mat/mat087/mat87c_tabulated_plas_sr.F
Chd|-- called by -----------
Chd|        SIGEPS87C                     source/materials/mat/mat087/sigeps87c.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE MAT87C_TABULATED_PLAS_SR(
     1     NEL    ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC    ,
     2     NPF    ,TF      ,TIME    ,TIMESTEP,UPARAM   ,
     3     UVAR   ,RHO0    ,THKLY   ,THK     ,OFF      ,
     4     EPSPXX ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     5     DEPSXX ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,
     6     SIGOXX ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     8     SIGNXX ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     9     SOUNDSP,PLA     ,DPLA    ,EPSP    ,YLD      ,
     B     ETSE   ,GS      ,ISRATE  ,ASRATE  ,FACYLDI  ,
     C     SIGB   ,INLOC   ,DPLANL  ,SEQ     )
C=======================================================================
C   BARLAT YLD2000 material model
C=======================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
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
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM, NUVAR,ISRATE,INLOC
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
     .   UVAR(NEL,NUVAR),OFF(NEL),THK(NEL),SEQ(NEL),
     .   YLD(NEL),DPLA(NEL),EPSP(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real 
     .     FINTER ,TF(*)
      EXTERNAL FINTER
c     Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
c     Y       : y = f(x)
c     X       : x
c     DYDX    : f'(x) = dy/dx
c     IFUNC(J): FUNCTION INDEX
c     J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
c     NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,K,NRATE,NITER,JJ(NEL),J1,J2,N,NINDX,
     .        IFLAG,IFLAGSR,EXPA,EXPAM2,NFLAG4,NFLAG3,
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
     .      E,NU,BULK,A1,A2,G,UNSA,DU,R,DDEP,
     .      AL1, AL2 , AL3 , AL4 ,DSIGPL1,DSPL1,
     .      AL5, AL6 , AL7 , AL8 ,DSIGPL2,DSPL2,
     .      AL9, AL10, AL11, AL12,DSIGPL3,DSPL3,
     .      YSCALE,CEPS,EPS0,EXPN,TERM1,NORMXX,NORMYY,NORMXY,
     .      LPP11,LPP12,LPP21,LPP22,LPP66,
     .      FCTP,BPP,CPP,DPP,FP1,FP2,FP3,
     .      KINT,FPP1,FPP2,FPP3,DFP1,DFP2,DFP3,
     .      DFPP1,DFPP2,DFPP3,DP,AP,DF1,DF2,DF3,
     .      DSDEPL1,DSDEPL2,DSDEPL3,NORMEF,EPSPI,
     .      ASWIFT ,EPSO,QVOCE,BETA,KO,ALPHA,NEXP,
     .      EXPV,KSWIFT,KVOCE,UNSP,UNSC,PLA_I,YLD_I,NORMSIG,
     .      DSWIFT,DVOCE,DFEPSP,YLDP, p2,UNSPT,UNSCT,DFSR,TREF,DVM,
     .      K1,K2,AHS,BHS,MHS,EPS0HS,NHS,HMART,TEMP0,ETA,CP, AM    ,
     .          BM    , CM    , DM    , PPM    , QM    , E0MART, VM0 
      my_real
     .        HK(NEL),EPSF(NEL),PLAP(NEL),FRATE(NEL),
     .        SVM2(NEL),YLD2(NEL),HI(NEL),G3(NEL),
     .        HKIN,dezz,svm,DPLA_I(NEL),dr(NEL),
     .        AA(NEL),BB(NEL),DPLA_J(NEL),   
     .        PP(NEL),QQ(NEL),FAIL(NEL),SVMO(NEL),
     .        HI2(NEL), ES2(NEL), ATEMP(NEL),AEXP(NEL),
     .        VM(NEL),DPLA_DLAM(NEL)
      my_real
     .        YFAC(NEL,2),PUI_1,PUI_2
      my_real NORM_SP,FCT,DF,DLAM,YLD_NORM(NEL)
      my_real
     .        DSIGBXX(NEL),DSIGBYY(NEL),DSIGBXY(NEL),
     .        SIGBXX(NEL) ,SIGBYY(NEL) ,SIGBXY(NEL),
     .        DSIGBOXX(NEL),DSIGBOYY(NEL),DSIGBOXY(NEL),
     .        YLDP0(NEL),KSWIFT0,SIG_DFDSIG,
     .        DADEPL1,DADEPL2,DADEPL3,FISOKIN,
     .        AKCK,CKH(4) ,AKH(4)  ,AEXP0 ,YLD0(NEL)
C=======================================================================
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
      ! Yield stress parameters
      NRATE   = NINT(UPARAM(25))
c
      IFLAG   = NINT(UPARAM(24))            
      IFLAGSR = NINT(UPARAM(25+2*NRATE+8)) 

 
      ! IFLAG=1 => Swift-Voce Yld, IFLAG=0 => Tabulated Yld
      ! IFLAGSR=0 => total strain rate,  IFLAGSR=1 => plastic strain rate 
c-----------------------------------------------
      NFLAG3 = 34 + 2*NRATE
      NFLAG4 = NFLAG3 + 21
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
c
      ! Barlat criterion parameter
      LPP11=(-TWO* AL3+  TWO*AL4 +  EIGHT*AL5 -   TWO*AL6)/NINE
      LPP12=(       AL3-FOUR*AL4- FOUR*AL5 + FOUR*AL6)/NINE
      LPP21=(FOUR*AL3-FOUR*AL4- FOUR*AL5 +        AL6)/NINE
      LPP22=(-TWO* AL3+  EIGHT*AL4+   TWO*AL5 -   TWO*AL6)/NINE
      LPP66= AL8
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
      PLAP(:NEL) = UVAR(:NEL,1)
c-----------------------------------------------
c     FORMULATION FLAG
c-----------------------------------------------
c------------------------------
       IF (ISIGI == 0) THEN
            IF (TIME == ZERO) THEN
              DO I=1,NEL
                YFAC(I,1) = UPARAM(25+NRATE+1)*FACYLDI(I)
                YLD(I)    = YFAC(I,1)*FINTER(IFUNC(1),ZERO,NPF,TF,DYDX(I)) 
                UVAR(I,2) = YLD(I)     
                UVAR(I,3) = DYDX(I)     
              ENDDO
            ENDIF
      ENDIF
      DO I=1,NEL
            YLD(I)  = UVAR(I,2)
            DYDX(I) = UVAR(I,3)                                                                        
      ENDDO       
C------------------------------
c------------------------------
        ! Computation of the Barlat equivalent stress      
        DO I=1,NEL

          YLD_NORM(I) = YLD(I)*NORM_SP        
c
          XPXX(I) = (TWO*AL1*SIGNXX(I) -AL1*SIGNYY(I)) /THREE
          XPYY(I) = (-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I)) /THREE
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
     .            (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .            + FOUR*XPXY(I)*XPXY(I))))*HALF
c       
          XPP1(I) = (XPPXX(I)+XPPYY(I)+SQRT(MAX(ZERO,
     .             (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .            + FOUR*XPPXY(I)*XPPXY(I) ) ))*HALF
          XPP2(I) = (XPPXX(I)+XPPYY(I)-SQRT(MAX(ZERO,
     .          (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .          + FOUR*XPPXY(I)*XPPXY(I))))*HALF
c     
          PHIP(I) = (ABS( XP1(I)-XP2(I)      ) *NORM_SP)**EXPA
          PHIPP(I)= (ABS( TWO*XPP2(I)+XPP1(I)) *NORM_SP)**EXPA
     .             +(ABS( TWO*XPP1(I)+XPP2(I)) *NORM_SP)**EXPA
c       
          IF((HALF*(PHIP(I)+PHIPP(I)))>ZERO) THEN
            SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I)))) !normalized
          ELSE
            SIG(I) = ZERO
          ENDIF
        UVAR(I, 8+NRATE +3 ) = SIG(I)/NORM_SP
        ENDDO
c-----------------------------------------------
c       CHECKING THE YIELD CONDITION
c-----------------------------------------------
        NINDX  = 0
        DO I=1,NEL
          IF (SIG(I)>YLD_NORM(I).AND.OFF(I) == ONE)THEN ! Plastic Loading
            NINDX = NINDX + 1
            INDX(NINDX)  = I 
          ENDIF
        ENDDO
c-----------------------------------------------
c         COMPUTING THE RETURN MAPPING
c-----------------------------------------------
        DO II=1,NINDX                                              
            I = INDX(II)                                             
C
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
C            
            JJ(I) = 1                                              
            DO J=2,NRATE-1                                        
              IF (PLAP(I) >= UPARAM(25+J)) JJ(I) = J               
            ENDDO                                                  
            J1 = JJ(I)
            J2 = J1+1
            FRATE(I)   = (PLAP(I) - UPARAM(25+J1))/(UPARAM(26+J1) - UPARAM(25+J1))
            YFAC(I,1) = UPARAM(25+NRATE+J1)*FACYLDI(I)
            YFAC(I,2) = UPARAM(25+NRATE+J2)*FACYLDI(I)
            !Y1 and Y2 for Pl SR dependency
c
            IF (FISOKIN == ZERO) THEN
              Y1(I)  = YFAC(I,1)*FINTER(IFUNC(J1),PLA(I),NPF,TF,DYDX1(I)) 
              Y2(I)  = YFAC(I,2)*FINTER(IFUNC(J2),PLA(I),NPF,TF,DYDX2(I)) 
c
              YLD(I) = Y1(I) + FRATE(I)*(Y2(I)-Y1(I))
              YLD(I) = MAX(YLD(I),EM20) 
              DYDX1(I) = DYDX1(I)*YFAC(I,1)
              DYDX2(I) = DYDX2(I)*YFAC(I,2)
            ELSEIF (FISOKIN == ONE) THEN
              Y1(I) = TF(NPF(IFUNC(J1))+1)
              Y2(I) = TF(NPF(IFUNC(J2))+1)
              Y1(I) = Y1(I)*YFAC(I,1)
              Y2(I) = Y2(I)*YFAC(I,2)
              YLD(I) = (Y1(I)    + FRATE(I)*( Y2(I)-Y1(I) )  )
              YLD0(I) = YLD(I)
            ELSE  ! Mixed hardening
              Y1(I)  = YFAC(I,1)*FINTER(IFUNC(J1),PLA(I),NPF,TF,DYDX1(I)) 
              Y2(I)  = YFAC(I,2)*FINTER(IFUNC(J2),PLA(I),NPF,TF,DYDX2(I)) 
              YLD(I) = Y1(I) + FRATE(I)*(Y2(I)-Y1(I))
              YLD(I) = MAX(YLD(I),EM20) 
              DYDX1(I) = DYDX1(I)*YFAC(I,1)
              DYDX2(I) = DYDX2(I)*YFAC(I,2)
              Y1(I) = TF(NPF(IFUNC(J1))+1)
              Y2(I) = TF(NPF(IFUNC(J2))+1)
              Y1(I) = Y1(I)*YFAC(I,1)
              Y2(I) = Y2(I)*YFAC(I,2)
              YLD0(I) = (Y1(I)    + FRATE(I)*( Y2(I)-Y1(I) )  )
              YLD(I)  =(ONE - FISOKIN) * YLD(I) + FISOKIN * YLD0(I)
            ENDIF  ! FISOKIN
c
            FCT = SIG(I)/NORM_SP - YLD(I)
c
            ! Loop over iterations
            DO K = 1,NITER
              !algo newton
              !calcul de dsigma/ddpla=dphi/dsigma*dsigma/ddpla
              !phi = phip + phipp
              !dphip/dsigma = dphip/dxp*dxp/dsigma  where  dxp/dsigma = Lp
c
              ! Computing the yield stress derivative
              !--------------------------------------- 
              DYDX(I) = (ONE - FISOKIN) *(DYDX1(I) + FRATE(I)*(DYDX2(I)-DYDX1(I)))
              YLDP    = ( DYDX(I)+ (Y2(I)-Y1(I))
     .             /(UPARAM(26+J1)-UPARAM(25+J1))  /TIMESTEP)
              !--------------------------------------- 
C              
              ! Computation of the normal to the yield surface
              NORMSIG = SQRT( (SIGNXX(I)*SIGNXX(I)
     .                + SIGNYY(I)*SIGNYY(I)
     .                + TWO*SIGNXY(I)*SIGNXY(I)) )
              IF (NORMSIG == ZERO) NORMSIG = ONE
           
              XPXX(I) = (TWO*AL1*SIGNXX(I) -AL1*SIGNYY(I)) /NORMSIG/THREE
              XPYY(I) = (-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I)) /NORMSIG/THREE
              XPXY(I) = AL7*SIGNXY(I)/NORMSIG
c       
              XPPXX(I) = (LPP11*SIGNXX(I) + LPP12*SIGNYY(I))/NORMSIG
              XPPYY(I) = (LPP21*SIGNXX(I) + LPP22*SIGNYY(I))/NORMSIG
              XPPXY(I) = LPP66*SIGNXY(I)/NORMSIG
       
              XP1(I) =(XPXX(I)+XPYY(I)+SQRT(MAX(ZERO,
     .              (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .              +FOUR*XPXY(I)*XPXY(I))))*HALF
              XP2(I) =(XPXX(I)+XPYY(I)-SQRT(MAX(ZERO,
     .              (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .              +FOUR*XPXY(I)*XPXY(I))))*HALF
       
              XPP1(I)=(XPPXX(I)+XPPYY(I)+SQRT(MAX(ZERO,
     .              (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .              +FOUR*XPPXY(I)*XPPXY(I) ) ))*HALF
              XPP2(I)=(XPPXX(I)+XPPYY(I)-SQRT(MAX(ZERO,
     .              (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .              +FOUR*XPPXY(I)*XPPXY(I))))*HALF
     
              PHIP(I) = ABS(XP1(I)-XP2(I))**EXPA
              PHIPP(I)= ABS(TWO*XPP2(I)+XPP1(I))**EXPA
     .               +ABS(TWO*XPP1(I)+XPP2(I))**EXPA
       
              IF ((HALF*(PHIP(I)+PHIPP(I))) > ZERO) THEN
                SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))))
              ELSE
                SIG(I) = ZERO
              ENDIF

              AP  = EXPA*(XP1(I)-XP2(I))*ABS(XP1(I)-XP2(I))**EXPAM2
              DP  = (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))+FOUR*XPXY(I)*XPXY(I)
c           
              BPP = EXPA*(TWO*XPP2(I)+XPP1(I))*ABS(TWO*XPP2(I)+XPP1(I))**EXPAM2
              CPP = EXPA*(TWO*XPP1(I)+XPP2(I))*ABS(TWO*XPP1(I)+XPP2(I))**EXPAM2
              DPP = (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))+FOUR*XPPXY(I)*XPPXY(I)
              !phip
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


              PUI_1=SIG(I)**(EXPA-1)
              DU = ONE/(TWO*EXPA*PUI_1)

              !derivative to sigma (dphip/dsig+dphipp/dig)
              DF1 = DFP1+DFPP1
              DF2 = DFP2+DFPP2
              DF3 = DFP3+DFPP3
c             ! DF/DSIG = normal
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
     .                    + SIGNYY(I) * NORMYY
     .                    + SIGNXY(I) * NORMXY 
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
     .         + NORMYY * (DSDEPL2 + DADEPL2)
     .         + NORMXY * (DSDEPL3 + DADEPL3)

              DF = DF - YLDP 
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
              !Updating the backstress
              IF(FISOKIN >ZERO) THEN
                DSIGBXX(I) = AKCK * DEPLXX(I) - DSIGBOXX(I) * DDEP
                DSIGBYY(I) = AKCK * DEPLYY(I) - DSIGBOYY(I) * DDEP
                DSIGBXY(I) = AKCK * DEPLXY(I) - DSIGBOXY(I) * DDEP
              ENDIF
              ! Updating the stress tensor
              SIGNXX(I) = SIGNXX(I) - A1*DEPLXX(I) - A2*DEPLYY(I) - FISOKIN*DSIGBXX(I)                   
              SIGNYY(I) = SIGNYY(I) - A2*DEPLXX(I) - A1*DEPLYY(I) - FISOKIN*DSIGBYY(I)                   
              SIGNXY(I) = SIGNXY(I) - G*DEPLXY(I) - FISOKIN*DSIGBXY(I)
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
C
              ENDIF

              ! Updating the Barlat equivalent stress
              XPXX(I) = (TWO*AL1*SIGNXX(I)- AL1*SIGNYY(I))* NORM_SP/THREE
              XPYY(I) = (-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I))* NORM_SP/THREE
              XPXY(I) = AL7*SIGNXY(I)* NORM_SP
       
              XPPXX(I) = (LPP11*SIGNXX(I) + LPP12*SIGNYY(I))* NORM_SP
              XPPYY(I) = (LPP21*SIGNXX(I) + LPP22*SIGNYY(I))* NORM_SP
              XPPXY(I) = LPP66*SIGNXY(I)* NORM_SP
       
              XP1(I) = (XPXX(I)+XPYY(I)+SQRT(MAX(ZERO,
     .              (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .             + FOUR*XPXY(I)*XPXY(I))))*HALF
              XP2(I) = (XPXX(I)+XPYY(I)-SQRT(MAX(ZERO,
     .              (XPXX(I)-XPYY(I))*(XPXX(I)-XPYY(I))
     .             + FOUR*XPXY(I)*XPXY(I))))*HALF
       
              XPP1(I) = (XPPXX(I)+XPPYY(I)+SQRT(MAX(ZERO,
     .             (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .             + FOUR*XPPXY(I)*XPPXY(I))))*HALF
              XPP2(I) = (XPPXX(I)+XPPYY(I)-SQRT(MAX(ZERO,
     .             (XPPXX(I)-XPPYY(I))*(XPPXX(I)-XPPYY(I))
     .             + FOUR*XPPXY(I)*XPPXY(I))))*HALF
     
              PHIP(I)  = ABS(XP1(I)-XP2(I))**EXPA
              PHIPP(I) = ABS(TWO*XPP2(I)+XPP1(I))**EXPA
     .              + ABS(TWO*XPP1(I)+XPP2(I))**EXPA
              IF ((PHIP(I)+PHIPP(I)) > ZERO) THEN
                SIG(I) = EXP( UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))) )
              ELSE
                SIG(I) = ZERO
              ENDIF
              ! Updating the yield stress
              JJ(I) = 1                                              
              DO J=2,NRATE-1                                        
                IF(PLAP(I) >= UPARAM(25+J)) JJ(I) = J               
              ENDDO                                                  
              J1 = JJ(I)
              J2 = J1+1
              FRATE(I)   = (PLAP(I) - UPARAM(25+J1))/(UPARAM(26+J1) - UPARAM(25+J1))
              YFAC(I,1) = UPARAM(25+NRATE+J1)*FACYLDI(I)
              YFAC(I,2) = UPARAM(25+NRATE+J2)*FACYLDI(I)
              !Y1 and Y2 for Pl SR dependency
              Y1(I)  = YFAC(I,1)*FINTER(IFUNC(J1),PLA(I),NPF,TF,DYDX1(I)) 
              Y2(I)  = YFAC(I,2)*FINTER(IFUNC(J2),PLA(I),NPF,TF,DYDX2(I)) 
c
              YLD(I) = Y1(I)    + FRATE(I)*(Y2(I)-Y1(I))
              YLD(I) = MAX(YLD(I),EM20)
              IF (FISOKIN > ZERO)THEN
                 Y1(I) = TF(NPF(IFUNC(J1))+1)
                 Y2(I) = TF(NPF(IFUNC(J2))+1)
                 Y1(I) = Y1(I)*YFAC(I,1)
                 Y2(I) = Y2(I)*YFAC(I,2)
                 YLD0(I) = (Y1(I)    + FRATE(I)*( Y2(I)-Y1(I) )  )
                 YLD(I)   = (ONE - FISOKIN) * YLD(I) + FISOKIN * YLD0(I)
              ENDIF
              ! Updating the yield function value
              FCT = SIG(I)/NORM_SP- YLD(I)
            ENDDO ! iter k                                    
            H(I) = MAX(EM10,DYDX(I))
            UVAR(I,3) = H(I)
            ETSE(I)   = H(I)/(H(I)+E)
c
      ENDDO !ndx
C=======================================================================
      ASRATE = MIN(ONE, ASRATE*TIMESTEP)
      DO I=1,NEL
            PLAP(I)   = DPLA(I) / MAX(TIMESTEP,EM20)         
            PLAP(I)   = ASRATE*PLAP(I) + (ONE - ASRATE)*UVAR(I,1)
            UVAR(I,1) = PLAP(I)
      ENDDO
C-------------------------------------------------------------------------
      IF (FISOKIN > ZERO ) THEN
        DO I=1,NEL
          SIGBXX(I) = SIGB(I,1) + SIGB(I,4) + SIGB(I,7) + SIGB(I,10)
          SIGBYY(I) = SIGB(I,2) + SIGB(I,5) + SIGB(I,8) + SIGB(I,11)
          SIGBXY(I) = SIGB(I,3) + SIGB(I,6) + SIGB(I,9) + SIGB(I,12)
          SIGNXX(I) = SIGNXX(I) + SIGBXX(I) 
          SIGNYY(I) = SIGNYY(I) + SIGBYY(I) 
          SIGNXY(I) = SIGNXY(I) + SIGBXY(I) 
       ENDDO
      ENDIF
c
      DO I=1,NEL
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
