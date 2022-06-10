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
Chd|  SIGEPS187                     source/materials/mat/mat187/sigeps187.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS187(
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC    ,
     2     NPF    ,TF     ,TIME    ,TIMESTEP,UPARAM   ,
     3     RHO0   ,RHO    ,VOLUME  ,EINT    ,TEMPEL,NGL , 
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX   , 
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX , 
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,YLD     ,PLA    ,
     B     DPLA    ,ETSE   ,IPM     ,MAT     ,ISRATE ,
     C     FACYLDI ,EPSP     )  
C=======================================================================
C   BARLAT YLD2000 material model
C=======================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX
C NPF     |  *      | I | R | FUNCTION ARRAY
C TF      |  *      | F | R | FUNCTION ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UVAR    | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM, NUVAR,
     .        IPM(NPROPMI,*),ISRATE(NEL),MAT(NEL), NGL(NEL)
      my_real 
     .   TIME,TIMESTEP,UPARAM(NUPARAM),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   TEMPEL(NEL),FACYLDI(NEL),EPSP(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .   SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .   SOUNDSP(NEL),VISCMAX(NEL),YLD(NEL) ,
     .   PLA(NEL),DPLA(NEL),ETSE(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .   UVAR(NEL,NUVAR), OFF(NEL)
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
      INTEGER I,II,J,K,NRATE,JJ(NEL),J1,J2,N,NINDX,IFLAG,IFLAGSR,EXPA,EXPAM2,
     .        NMAX,INDX(NEL), IPOS1(NEL),ILEN1(NEL),IAD1(NEL),
     .       IPOS2(NEL),ILEN2(NEL),IAD2(NEL)
      my_real
     .        Y1(NEL),Y2(NEL),DYDX(NEL),DYDX1(NEL),DYDX2(NEL),
     .        XPXX(NEL),XPYY(NEL),XPXY(NEL),XPYZ(NEL),XPZX(NEL),
     .        XPPXX(NEL),XPPYY(NEL),XPPXY(NEL),XPPYZ(NEL),XPPZX(NEL),
     .        PHIP(NEL),PHIPP(NEL),
     .        DELTAP(NEL), DELTAPP1(NEL), DELTAPP2(NEL), DELTAPP(NEL),
     .        DEPLXX(NEL),DEPLYY(NEL),DEPLXY(NEL),DEPLZZ(NEL),
     .        DEELZZ(NEL),XP1(NEL),XP2(NEL),XPP1(NEL),
     .        XPP2(NEL),SIG(NEL),H(NEL),SIGTRXX(NEL),SIGTRYY(NEL),
     .        SIGTRXY(NEL),RATE(NEL),SIGTRYZ(NEL),SIGTRZZ(NEL),
     .        SIGTRZX(NEL),DEPLYZ(NEL),DEPLZX(NEL),DDEPLYZ(NEL),DDEPLZX(NEL),
     .        DDEPLXX(NEL),DDEPLYY(NEL),DDEPLXY(NEL),DDEPLZZ(NEL)
      my_real
     .      E,NU,BULK,A1,A2,G,UNSA,DU,R,UMR,G2,LAMDA,
     .      AL1, AL2 , AL3 , AL4 ,DSIGPL1,DSPL1,
     .      AL5, AL6 , AL7 , AL8 ,DSIGPL2,DSPL2,
     .      AL9, AL10, AL11, AL12,DSIGPL3,DSPL3,
     .      YSCALE,CEPS,EPS0,EXPN,DAV,
     .      LPP11,LPP12,LP13,LPP21,LPP22,LPP23,
     .      LPP34,LPP45,LPP56,LPP13,LPP66,
     .      FCT,FCTP,BPP,CPP,DPP,FP1,FP2,FP3,FP4,FP5,
     .      KINT,FPP1,FPP2,FPP3,DFP1,DFP2,DFP3,FPP4,FPP5,
     .      DFPP1,DFPP2,DFPP3,DP,AP,DF1,DF2,DF3,DF,DFP4,DFP5,DFP6,
     .      DSDEPL1,DSDEPL2,DSDEPL3,NORMEF,EPSPI,DFPP4,DFPP5,DFPP6,
     .      ASWIFT ,EPSO,QVOCE,BETA,KO,ALPHA,NEXP,DF4,DF5,DF6,
     .      EXPV,KSWIFT,KVOCE,UNSP,UNSC,PLA_I,YLD_I,
     .      DSWIFT,DVOCE,DFEPSP,YLDP, p2,UNSPT,UNSCT,DFSR,
     .      TPP1, TPP2,TPP3,TP1,TP2,fiptest,fipptest

      my_real
     .        HK(NEL),NU_1MNU(NEL),EPSF(NEL),PLAP(NEL),F_EPSP(NEL),
     .        SVM2(NEL),YLD2(NEL),HI(NEL),G3(NEL),T_1PNU(NEL),
     .        HKIN,NU110,NU210,dezz,svm,DPLA_I(NEL),dr(NEL),
     .        AA(NEL),BB(NEL),DPLA_J(NEL),U_1MNU(NEL),     
     .        PP(NEL),QQ(NEL),FAIL(NEL),SVMO(NEL),F_EPSPT(NEL),
     .        NU11V(NEL),NU21V3(NEL),AANU11V(NEL),BBNU21V3(NEL),
     .        HI2(NEL), ES2(NEL)
       my_real
     .        YFAC(NEL,2),PUI_1,PUI_2
       DATA NMAX/5/
c-----------------------------------------------
c     USER VARIABLES INITIALIZATION
c-----------------------------------------------
c      Swift/Voce
c            k=alpha*k1(eps,p)+(1-alpha)*k2(eps,p)  
c            k1=A*(eps,p+e0)**n
c            k2=Q*(1-exp(-b*eps,p))+sig0
c-----------------------------------------------
c    UVAR1  =PLA
c    UVAR2  =YLD
c    UVAR3  =DPLA
c    UVAR4  =ETSE
c    UVAR5  =IPOS
c    UVAR6  =H
c-----------------------------------------------
C
      E     =  UPARAM(1)  
      NU    =  UPARAM(2)  
      G     = UPARAM(23)
      G2    = TWO *G 
      BULK  = E/THREE/(ONE-TWO*NU)

      AL1   =  UPARAM(3)  
      AL2   =  UPARAM(4)  
      AL3   =  UPARAM(5)  
      AL4   =  UPARAM(6)  
      AL5   =  UPARAM(7)  
      AL6   =  UPARAM(8)  
      AL7   =  UPARAM(9)  
      AL8   =  UPARAM(10) 
      AL9   =  UPARAM(11)  
      AL10   = UPARAM(12)  
      AL11   = UPARAM(13)  
      AL12   = UPARAM(14)  


c
      EXPA  =  NINT(UPARAM(15) )
      UNSA  =  ONE/EXPA
      EXPAM2=  EXPA - 2 
c
      ALPHA   =  UPARAM(16) 
      NEXP    =  UPARAM(17)
      EPSO    =  UPARAM(18) 
      ASWIFT  =  UPARAM(19) 

      NRATE   = NINT(UPARAM(25))
      LAMDA   = UPARAM(25+2*NFUNC+9) 


      QVOCE = UPARAM(25+2*NRATE+1)
      BETA  = UPARAM(25+2*NRATE+2)
      KO    = UPARAM(25+2*NRATE+3)

      UNSP  = UPARAM(25+2*NRATE+4)
      UNSC  = UPARAM(25+2*NRATE+5)


      UNSPT  = UPARAM(25+2*NRATE+6)
      UNSCT  = UPARAM(25+2*NRATE+7)

      IFLAGSR  = NINT(UPARAM(25+2*NRATE+8))
      !DEPLACER AU STARTER
      LPP11=(-TWO* AL3  +TWO*AL4  +  EIGHT*AL5-  TWO*AL6)/NINE
      LPP12=(-FOUR*AL4 +FOUR*AL6+      AL3 -  FOUR*AL5)/NINE
      LPP13=(AL3         +TWO*AL4  -FOUR*AL5-  TWO*AL6   )/NINE
      LPP21=(FOUR*AL3  -FOUR*AL4-FOUR*AL5+       AL6)/NINE
      LPP22=(-TWO* AL3+  EIGHT*AL4  +  TWO*AL5-  TWO*AL6)/NINE
      LPP23=(-TWO* AL3 -FOUR*AL4 +  TWO*AL5+       AL6)/NINE
      LPP34=AL8
      LPP45=AL11
      LPP56=AL12
      

      IFLAG    =  INT(UPARAM(24)) 

c ---
      DO I=1,NEL
        DAV = DEPSXX(I)+DEPSYY(I)+DEPSZZ(I)
        SIGNXX(I)  = SIGOXX(I) + DEPSXX(I)*G2 + LAMDA*DAV
        SIGNYY(I)  = SIGOYY(I) + DEPSYY(I)*G2 + LAMDA*DAV
        SIGNZZ(I)  = SIGOZZ(I) + DEPSZZ(I)*G2 + LAMDA*DAV
        SIGNXY(I)  = SIGOXY(I) + DEPSXY(I)*G
        SIGNYZ(I)  = SIGOYZ(I) + DEPSYZ(I)*G
        SIGNZX(I)  = SIGOZX(I) + DEPSZX(I)*G 

C------------------
C SIGMA TRIAL
C------------------ 
        SIGTRXX(I)=SIGNXX(I)
        SIGTRYY(I)=SIGNYY(I)
        SIGTRZZ(I)=SIGNZZ(I)
        SIGTRXY(I)=SIGNXY(I)
        SIGTRYZ(I)=SIGNYZ(I)
        SIGTRZX(I)=SIGNZX(I)
c-------------------
c       SOUND SPEED, TANGENT MODULUS
c-------------------
        SOUNDSP(I) = SQRT((BULK+FOUR_OVER_3*G)/RHO0(I))
        VISCMAX(I) = ZERO
        ETSE(I) = ONE
        DPLA(I) =  ZERO
      ENDDO
C-------------------
c flag formulation ************************************************
C-------------------

      IF (IFLAG == 1) THEN ! ANALYTIC SWIFT-VOCE YIELD
c     -----    Yield ---------------                                                   
       DO I=1,NEL
           EXPV   = EXP(-BETA*PLA(I))
           IF((PLA(I) + EPSO)>ZERO) THEN
            PUI_1 = EXP(NEXP*LOG((PLA(I) + EPSO)))
           ELSE
            PUI_1 = ZERO
           ENDIF                                           
!           KSWIFT = ASWIFT*(PLA(I) + EPSO)**NEXP                                      
           KSWIFT = ASWIFT*PUI_1
           KVOCE  = KO + QVOCE*(ONE - EXPV)                                      
           YLD(I)  = ALPHA*KSWIFT + (ONE-ALPHA)*KVOCE 
           F_EPSPT(I) = ONE 
           F_EPSP(I)  = ONE 
           !!print*, 'yld ', YLD(I)
       ENDDO    
       IF (UNSPT /= ZERO )THEN 
        DO I=1,NEL
          IF(EPSP(I)/=ZERO)
     .    F_EPSPT(I) =ONE + EXP(UNSPT * LOG(UNSCT*EPSP(I)))   
        ENDDO    
       ENDIF  
       DO I=1,NEL
         !--------------------
         !BARLAT CRITERION 
         !--------------------
         !XP(5) = LP(5*6) * SIGMA(6) 
         XPXX(I) = (TWO*AL1*SIGNXX(I)-AL1*SIGNYY(I)-AL1*SIGNZZ(I))/THREE
         XPYY(I) =(-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I)-AL2*SIGNZZ(I))/THREE
         XPXY(I) = AL7*SIGNXY(I)
         XPYZ(I) = AL9*SIGNYZ(I)
         XPZX(I) = AL10*SIGNZX(I) 
      
         !XPP(5) = LPP(5*6) * SIGMA(6) 
         XPPXX(I) = LPP11*SIGNXX(I) + LPP12*SIGNYY(I) +LPP13*SIGNZZ(I)
         XPPYY(I) = LPP21*SIGNXX(I) + LPP22*SIGNYY(I) +LPP23*SIGNZZ(I)
         XPPXY(I) = LPP34*SIGNXY(I)
         XPPYZ(I) = LPP45*SIGNYZ(I)
         XPPZX(I) = LPP56*SIGNZX(I)

         DELTAP(I)   = (XPXX(I)-XPYY(I))**2 + FOUR * (XPXY(I)**2 + XPYZ(I)**2 + XPZX(I)**2 )    
         PHIP(I)  = SQRT(MAX(ZERO, DELTAP(I)))**EXPA


       
         DELTAPP(I)  = (XPPXX(I)-XPPYY(I))**2 + FOUR * (XPPXY(I)**2 + XPPYZ(I)**2 + XPPZX(I)**2 )
         DELTAPP(I)  = SQRT(MAX(ZERO, DELTAPP(I)))
         DELTAPP1(I) = THREE_HALF*(XPPXX(I)-XPPYY(I))+HALF*DELTAPP(I)
         DELTAPP2(I) = THREE_HALF*(XPPXX(I)-XPPYY(I))-HALF*DELTAPP(I)

         PHIPP(I)= DELTAPP1(I)**EXPA + DELTAPP2(I)**EXPA

         !SIG(I) = (HALF*(PHIP(I)+PHIPP(I)))**UNSA
         IF(((PHIP(I)+PHIPP(I)))>ZERO) THEN
          SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))))
         ELSE
          SIG(I) = ZERO
         ENDIF

       ENDDO   
 
C      Check Yield Condition  
C      ----------------------------------------------------
       NINDX  = 0
       DO I=1,NEL
        IF (SIG(I)>YLD(I).AND.OFF(I) == ONE)THEN ! Plastic Loading
         NINDX = NINDX + 1
         INDX(NINDX)  = I 
        ENDIF
       ENDDO
       !--------------------
       !PLASTIC FLOW
       !--------------------
       DO II=1,NINDX                                              
        I = INDX(II)                                             
        !IF (SIG(I)>YLD(I).AND.OFF(I) == ONE)THEN
	   DPLA(I)= UVAR(I,3)+ EM09

        !DPLA(I)= (SIG(I)-YLD(I))/(THREE*G+DYDX(I))
        DEPLXX(I)=ZERO
        DEPLYY(I)=ZERO
        DEPLZZ(I)=ZERO
        DEPLXY(I)=ZERO
        DEPLYZ(I)=ZERO
        DEPLZX(I)=ZERO

        !YLD_I=YLD(I)
        PLA_I=PLA(I)

        DO K=1,4
         !algo newton
         !calcul de dsigma/ddpla=dphi/dsigma*dsigma/ddpla
         !phi = phip + phipp
         !dphip/dsigma = dphip/dxp*dxp/dsigma  where  dxp/dsigma = Lp
         PLA(I) = PLA_I + DPLA(I)
         PLAP(I)= DPLA(I)/TIMESTEP
C
         EXPV   = EXP(-BETA*PLA(I))
!         KSWIFT = ASWIFT*(PLA(I) + EPSO)**NEXP   
         IF((PLA(I) + EPSO)>ZERO) THEN                                           
          PUI_1 = EXP(NEXP*LOG(PLA(I) + EPSO))
         ELSE
          PUI_1 = ZERO
         ENDIF
         KSWIFT = ASWIFT*PUI_1                                       
         KVOCE  = KO + QVOCE*(ONE - EXPV)                                       
         YLD(I) = ALPHA*KSWIFT + (ONE-ALPHA)*KVOCE 

         !===========ADD PL SR DEPENDENCY THROUGH COWPER SEYMONDS
         !Compute plastic strain rate Cowper seymonds          
         IF( UNSP/=ZERO)THEN
           IF(PLAP(I)>ZERO)THEN
             F_EPSP(I) = ONE+EXP(UNSP * LOG(UNSC*PLAP(I)))   
           ELSE
             F_EPSP(I) = ONE
           ENDIF 
         ELSE
            F_EPSP(I) =  ONE 
         ENDIF
         !DERIVATIVE OF YLD
         DSWIFT = NEXP*KSWIFT/(PLA(I) + EPSO)
         DVOCE  = QVOCE*BETA*EXPV
         !DFEPSP = UNSP *F_EPSP(I)/ (DPLA(I)+TIMESTEP/MAX(EM20,UNSC))
         DFEPSP = UNSP *(F_EPSP(I)-ONE)/ MAX(EM20,DPLA(I))
         YLDP   = ALPHA*DSWIFT + (ONE-ALPHA)*DVOCE                
         YLDP   = YLDP*F_EPSP(I) + YLD(I)*DFEPSP
         YLD(I) =  YLD(I) * F_EPSP(I)  *F_EPSPT(I)
         !===========END PL SR DEPENDENCY  
         FCT = SIG(I)- YLD(I)  ! residu

         TP1 = EXPA*(SQRT(MAX(ZERO,DELTAP(I))))**(EXPA-2)
         !D_phip/D_XP
         FP1 = TP1 * (XPXX(I) - XPYY(I))
         FP2 = -FP1  
         FP3 = THREE * TP1 * XPXY(I)
         FP4 = THREE * TP1 * XPYZ(I)
         FP5 = THREE * TP1 * XPZX(I) 


         TPP1 = EXPA * DELTAPP1(I) ** (EXPA - 1)
         TPP2 = EXPA * DELTAPP2(I) ** (EXPA - 1)
         TPP3 = TWO *(TPP1  -  TPP2) /DELTAPP(I)

         !D_phipp/D_XPP
         FPP1 = TPP1 *(THREE_HALF+HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
     .         + TPP2 *(THREE_HALF-HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
         FPP2 = TPP1 *(-THREE_HALF-HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
     .         + TPP2 *(-THREE_HALF+HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
         FPP3 = TPP3 * XPPXY(I)
         FPP4 = TPP3 * XPPYZ(I) 
         FPP5 = TPP3 * XPPZX(I)
        
         DFP1=THIRD*(TWO*AL1*FP1-AL2*FP2)
         DFP2=THIRD*(TWO*AL2*FP2-AL1*FP1)
         DFP3=THIRD*(-AL1*FP1-AL2*FP2)
         DFP4=AL7*FP3
         DFP5=AL9*FP4
         DFP6=AL10*FP5 
       
         DFPP1=FPP1*LPP11+FPP2*LPP21
         DFPP2=FPP1*LPP12+FPP2*LPP22
         DFPP3=FPP1*LPP13+FPP2*LPP23
         DFPP4=FPP3*LPP34
         DFPP5=FPP4*LPP45
         DFPP6=FPP5*LPP56
          
         DU =  UNSA * SIG(I) / (MAX(EM20, PHIP(I)+PHIPP(I)) ) 
 

         DF1 = DFP1+DFPP1
         DF2 = DFP2+DFPP2
         DF3 = DFP3+DFPP3
         DF4 = DFP4+DFPP4
         DF5 = DFP5+DFPP5
         DF6 = DFP6+DFPP6

         !DF = DF1*DSDEPL1+DF2*DSDEPL2+DSDEPL3*DF3
         dDEPLXX(I)=DF1*DU
         dDEPLYY(I)=DF2*DU
         dDEPLZZ(I)=DF3*DU
         dDEPLXY(I)=DF4*DU
         dDEPLYZ(I)=DF5*DU
         dDEPLZX(I)=DF6*DU

         DF = - G * DU**2
     .        * (TWO*(DF1*DF1 + DF2*DF2 + DF3*DF3 )
     .            + DF4*DF4 + DF5*DF5 + DF6*DF6 )

c         DF = -TWO*G * (DU * DF1) **2 
c     .        -TWO*G * (DU * DF2) **2 
c     .        -TWO*G * (DU * DF3) **2 
c     .        -G * (DU * DF4) **2 
c     .        -G * (DU * DF5) **2 
c     .        -G * (DU * DF6) **2 

         DF = DF  - YLDP
         !CALCUL PAS PLASTIQUE
         DPLA(I)=DPLA(I)-FCT/DF
         DPLA(I)=MAX(ZERO,DPLA(I))

         !TENSEUR PLASTIQUE
         DEPLXX(I)=DPLA(I)*DF1*DU
         DEPLYY(I)=DPLA(I)*DF2*DU
         DEPLZZ(I)=DPLA(I)*DF3*DU
         DEPLXY(I)=DPLA(I)*DF4*DU
         DEPLYZ(I)=DPLA(I)*DF5*DU
         DEPLZX(I)=DPLA(I)*DF6*DU

         !ACTUALISATION DES CONTRAINTES
         SIGNXX(I)=SIGTRXX(I)- G2*DEPLXX(I)            
         SIGNYY(I)=SIGTRYY(I)- G2*DEPLYY(I) 
         SIGNZZ(I)=SIGTRZZ(I)- G2*DEPLZZ(I)            
         SIGNXY(I)=SIGTRXY(I)- G*DEPLXY(I)
         SIGNYZ(I)=SIGTRYZ(I)- G*DEPLYZ(I)
         SIGNZX(I)=SIGTRZX(I)- G*DEPLZX(I)
         !CALCUL NOUVEAU SIGMA EFFECTIF
         !--------------------
         !BARLAT CRITERION 
         !--------------------
         XPXX(I) = (TWO*AL1*SIGNXX(I)-AL1*SIGNYY(I)-AL1*SIGNZZ(I))/THREE
         XPYY(I) =(-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I)-AL2*SIGNZZ(I))/THREE
         XPXY(I) = AL7*SIGNXY(I)
         XPYZ(I) = AL9*SIGNYZ(I)
         XPZX(I) = AL10*SIGNZX(I) 
      
         XPPXX(I) = LPP11*SIGNXX(I) + LPP12*SIGNYY(I) +LPP13*SIGNZZ(I)
         XPPYY(I) = LPP21*SIGNXX(I) + LPP22*SIGNYY(I) +LPP23*SIGNZZ(I)
         XPPXY(I) = LPP34*SIGNXY(I)
         XPPYZ(I) = LPP45*SIGNYZ(I)
         XPPZX(I) = LPP56*SIGNZX(I)

         DELTAP(I)   = (XPXX(I)-XPYY(I))**2 + FOUR * (XPXY(I)**2 + XPYZ(I)**2 + XPZX(I)**2 )  

         PHIP(I)  = SQRT(MAX(ZERO, DELTAP(I)))**EXPA

       
         DELTAPP(I)  = (XPPXX(I)-XPPYY(I))**2 + FOUR * (XPPXY(I)**2 + XPPYZ(I)**2 + XPPZX(I)**2 )
         DELTAPP(I)  = SQRT(MAX(EM20, DELTAPP(I)))

         DELTAPP1(I) = THREE_HALF*(XPPXX(I)-XPPYY(I))+HALF*DELTAPP(I)
         DELTAPP2(I) = THREE_HALF*(XPPXX(I)-XPPYY(I))-HALF*DELTAPP(I)

         PHIPP(I)= DELTAPP1(I)**EXPA + DELTAPP2(I)**EXPA
      
!        SIG(I) = (HALF*(PHIP(I)+PHIPP(I)))**UNSA
         IF(((PHIP(I)+PHIPP(I)))>ZERO) THEN
          SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))))
         ELSE
          SIG(I) = ZERO
         ENDIF
  
        ENDDO  ! iter  
	   UVAR(I,3)=DPLA(I)       
       ENDDO ! NINDX
C-------------------
C-------------------
      ELSE ! IFLAG *TABULATED YIELD               
C-------------------
c yield tabulated in below
C-------------------
       IF(IFLAGSR == 0)THEN
C-------------------
C-------------------
        DO I=1,NEL                                   
          JJ(I) = 1      
          DO J=2,NRATE-1                      
            IF (EPSP(I) > UPARAM(25+J)) JJ(I) = J 
          ENDDO 
        ENDDO
        DO I=1,NEL 
          EPSPI  = UPARAM(25+JJ(I)) !
          RATE(I)=(EPSP(I) - EPSPI)/(UPARAM(26+JJ(I)) - EPSPI)
          YFAC(I,1)=UPARAM(25+NRATE+JJ(I))*FACYLDI(I)
          YFAC(I,2)=UPARAM(25+NRATE+JJ(I)+1)*FACYLDI(I)
        ENDDO
        DO I=1,NEL
           J1 = JJ(I)
           J2 = J1+1
           IPOS1(I) = NINT(UVAR(I,J1+5))
           IAD1(I)  = NPF(IPM(10+J1,MAT(1))) / 2  + 1
           ILEN1(I) = NPF(IPM(10+J1,MAT(1))+1) / 2 - IAD1(I)-IPOS1(I)
           IPOS2(I) = NINT(UVAR(I,J2+5))
           IAD2(I)  = NPF(IPM(10+J2,MAT(1))) / 2 + 1
           ILEN2(I) = NPF(IPM(10+J2,MAT(1))+1) / 2 - IAD2(I)-IPOS2(I)
        END DO
        CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
        CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
        DO I=1,NEL
           J1 = JJ(I)
           J2 = J1+1
           Y1(I)=Y1(I)*YFAC(I,1)
           Y2(I)=Y2(I)*YFAC(I,2)
           YLD(I) = (Y1(I)    + RATE(I)*(Y2(I)-Y1(I)))
           YLD(I) = MAX(YLD(I),EM20)
           DYDX1(I)=DYDX1(I)*YFAC(I,1)
           DYDX2(I)=DYDX2(I)*YFAC(I,2)
           DYDX(I)   = (DYDX1(I) + RATE(I)*(DYDX2(I)-DYDX1(I)))
           UVAR(I,J1+5) = IPOS1(I)
           UVAR(I,J2+5) = IPOS2(I)
        ENDDO
c------------------------------
       ELSE !flagSR
c------------------------------
        IF (ISIGI == 0) THEN
         IF(TIME == ZERO)THEN
          DO I=1,NEL
           YFAC(I,1)= UPARAM(25+NRATE+1)*FACYLDI(I)
           YLD(I)   = YFAC(I,1)*FINTER(IPM(11,MAT(1))  ,ZERO,NPF,TF,DYDX(I)) 
           UVAR(I,2)= YLD(I)     
          ENDDO
         ENDIF
        ENDIF
        DO I=1,NEL
         PLA(I)  = UVAR(I,1)                                        
         YLD(I)  = UVAR(I,2)
         DYDX(I) = UVAR(I,5)                                                                       
         PLAP(I) = UVAR(I,6)                                   
         JJ(I) = 1                                                  
        ENDDO       
      ENDIF !flagSR
C-------------------
C-------------------
c------------------------------     
      DO I=1,NEL
       !--------------------
       !BARLAT CRITERION 
       !--------------------
       XPXX(I) = (TWO*AL1*SIGNXX(I)-AL1*SIGNYY(I)-AL1*SIGNZZ(I))/THREE
       XPYY(I) =(-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I)-AL2*SIGNZZ(I))/THREE
       XPXY(I) = AL7*SIGNXY(I)
       XPYZ(I) = AL9*SIGNYZ(I)
       XPZX(I) = AL10*SIGNZX(I) 
      
       XPPXX(I) = LPP11*SIGNXX(I) + LPP12*SIGNYY(I) +LPP13*SIGNZZ(I)
       XPPYY(I) = LPP21*SIGNXX(I) + LPP22*SIGNYY(I) +LPP23*SIGNZZ(I)
       XPPXY(I) = LPP34*SIGNXY(I)
       XPPYZ(I) = LPP45*SIGNYZ(I)
       XPPZX(I) = LPP56*SIGNZX(I)

       DELTAP(I)   = (XPXX(I)-XPYY(I))**2 + FOUR * (XPXY(I)**2 + XPYZ(I)**2 + XPZX(I)**2 )    
       PHIP(I)  = SQRT(MAX(ZERO, DELTAP(I)))**EXPA

       
       DELTAPP(I)  = (XPPXX(I)-XPPYY(I))**2 + FOUR * (XPPXY(I)**2 + XPPYZ(I)**2 + XPPZX(I)**2 )
       DELTAPP(I)  = SQRT(MAX(EM20, DELTAPP(I)))
       DELTAPP1(I) = THREE_HALF*(XPPXX(I)-XPPYY(I))+HALF*DELTAPP(I)
       DELTAPP2(I) = THREE_HALF*(XPPXX(I)-XPPYY(I))-HALF*DELTAPP(I)

       PHIPP(I)= DELTAPP1(I)**EXPA + DELTAPP2(I)**EXPA
       
!      SIG(I) = (HALF*(PHIP(I)+PHIPP(I)))**UNSA
       IF((PHIP(I)+PHIPP(I))>ZERO) THEN
        SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))))
       ELSE
        SIG(I) = ZERO
       ENDIF
      ENDDO
C     ----------------------------------------------------
C     Check Yield Condition  
C     ----------------------------------------------------
      NINDX  = 0
      DO I=1,NEL
       IF (SIG(I)>YLD(I).AND.OFF(I) == ONE)THEN ! Plastic Loading
        NINDX = NINDX + 1
        INDX(NINDX)  = I 
       ENDIF
      ENDDO
C     ----------------------------------------------------

C     ----------------------------------------------------
c     flag strain rate option *****************************************
C     ----------------------------------------------------
      IF(IFLAGSR == 0)THEN
       DO II=1,NINDX                                              
         I = INDX(II)                                             
        !--------------------
        !PLASTIC FLOW
        !--------------------
	       !DPLA(I)= UVAR(I,3)
         !DPLA(I)= (SIG(I)-YLD(I))/(THREE*G+DYDX(I))
	    DPLA(I)= UVAR(I,3)+ EM09
         DEPLXX(I)=ZERO
         DEPLYY(I)=ZERO
         DEPLZZ(I)=ZERO
         DEPLXY(I)=ZERO
         DEPLYZ(I)=ZERO
         DEPLZX(I)=ZERO

         YLD_I=YLD(I)
         DO K=1,5
          !algo newton
          !calcul de dsigma/ddpla=dphi/dsigma*dsigma/ddpla
          !phi = phip + phipp
          !dphip/dsigma = dphip/dxp*dxp/dsigma  where  dxp/dsigma = Lp
          YLD(I) =YLD_I+DYDX(I)*DPLA(I)
          FCT = SIG(I)- YLD(I)  ! residu

          TP1 = EXPA*(SQRT(MAX(ZERO,DELTAP(I))))**(EXPA-2)

          !D_phip/D_XP
          FP1 = TP1 * (XPXX(I) - XPYY(I))
          FP2 = -FP1  
          FP3 = THREE * TP1 * XPXY(I)
          FP4 = THREE * TP1 * XPYZ(I)
          FP5 = THREE * TP1 * XPZX(I) 
         

          TPP1 = EXPA * DELTAPP1(I) ** (EXPA - 1)
          TPP2 = EXPA * DELTAPP2(I) ** (EXPA - 1)
          TPP3 = TWO *(TPP1  -  TPP2) /DELTAPP(I)
         
          !D_phipp/D_XPP
          FPP1 = TPP1 *(THREE_HALF+HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
     .         +TPP2 *(THREE_HALF-HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
          FPP2 = TPP1 *(-THREE_HALF-HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
     .         +TPP2 *(-THREE_HALF+HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
          FPP3 = TPP3 * XPPXY(I)
          FPP4 = TPP3 * XPPYZ(I) 
          FPP5 = TPP3 * XPPZX(I)

          DFP1=THIRD*(TWO*AL1*FP1-AL2*FP2)
          DFP2=THIRD*(TWO*AL2*FP2-AL1*FP1)
          DFP3=THIRD*(-AL1*FP1-AL2*FP2)
          DFP4=AL7*FP3
          DFP5=AL9*FP4
          DFP6=AL10*FP5 
         

          DFPP1=FPP1*LPP11+FPP2*LPP21
          DFPP2=FPP1*LPP12+FPP2*LPP22
          DFPP3=FPP1*LPP13+FPP2*LPP23
          DFPP4=FPP3*LPP34
          DFPP5=FPP4*LPP45
          DFPP6=FPP5*LPP56
          
          DU =  UNSA * SIG(I) / (MAX(EM20, PHIP(I)+PHIPP(I)) ) 

          DF1 = DFP1+DFPP1
          DF2 = DFP2+DFPP2
          DF3 = DFP3+DFPP3
          DF4 = DFP4+DFPP4
          DF5 = DFP5+DFPP5
          DF6 = DFP6+DFPP6

          DF = -TWO*G * (DU * DF1) **2 
     .         -TWO*G * (DU * DF2) **2 
     .         -TWO*G * (DU * DF3) **2 
     .         -G * (DU * DF4) **2 
     .         -G * (DU * DF5) **2 
     .         -G * (DU * DF6) **2 

          DF = DF  - YLDP

          !CALCUL PAS PLASTIQUE
          DPLA(I)=DPLA(I)-FCT/DF
          DPLA(I)=MAX(ZERO,DPLA(I))
          !TENSEUR PLASTIQUE
          DEPLXX(I)=DPLA(I)*DF1*DU
          DEPLYY(I)=DPLA(I)*DF2*DU
          DEPLZZ(I)=DPLA(I)*DF3*DU
          DEPLXY(I)=DPLA(I)*DF4*DU
          DEPLYZ(I)=DPLA(I)*DF5*DU
          DEPLZX(I)=DPLA(I)*DF6*DU

          !ACTUALISATION DES CONTRAINTES
          SIGNXX(I)=SIGTRXX(I)- G2*DEPLXX(I)           
          SIGNYY(I)=SIGTRYY(I)- G2*DEPLYY(I) 
          SIGNZZ(I)=SIGTRZZ(I)- G2*DEPLZZ(I)           
          SIGNXY(I)=SIGTRXY(I)- G*DEPLXY(I)
          SIGNYZ(I)=SIGTRYZ(I)- G*DEPLYZ(I)
          SIGNZX(I)=SIGTRZX(I)- G*DEPLZX(I)

          !CALCUL NOUVEAU SIGMA EFFECTIF
          !--------------------
          !BARLAT CRITERION 
          !--------------------
          XPXX(I) = (TWO*AL1*SIGNXX(I)-AL1*SIGNYY(I)-AL1*SIGNZZ(I))/THREE
          XPYY(I) =(-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I)-AL2*SIGNZZ(I))/THREE
          XPXY(I) = AL7*SIGNXY(I)
          XPYZ(I) = AL9*SIGNYZ(I)
          XPZX(I) = AL10*SIGNZX(I) 
      
          XPPXX(I) = LPP11*SIGNXX(I) + LPP12*SIGNYY(I) +LPP13*SIGNZZ(I)
          XPPYY(I) = LPP21*SIGNXX(I) + LPP22*SIGNYY(I) +LPP23*SIGNZZ(I)
          XPPXY(I) = LPP34*SIGNXY(I)
          XPPYZ(I) = LPP45*SIGNYZ(I)
          XPPZX(I) = LPP56*SIGNZX(I)

          DELTAP(I)   = (XPXX(I)-XPYY(I))**2 + FOUR * (XPXY(I)**2 + XPYZ(I)**2 + XPZX(I)**2 )    
          PHIP(I)  = SQRT(MAX(ZERO, DELTAP(I)))**EXPA

       
          DELTAPP(I)  = (XPPXX(I)-XPPYY(I))**2 + FOUR * (XPPXY(I)**2 + XPPYZ(I)**2 + XPPZX(I)**2 )
          DELTAPP(I)  = SQRT(MAX(EM20, DELTAPP(I)))
          DELTAPP1(I) = THREE_HALF*(XPPXX(I)-XPPYY(I))+HALF*DELTAPP(I)
          DELTAPP2(I) = THREE_HALF*(XPPXX(I)-XPPYY(I))-HALF*DELTAPP(I)

          PHIPP(I)= DELTAPP1(I)**EXPA + DELTAPP2(I)**EXPA
       
!         !SIG(I) = (HALF*(PHIP(I)+PHIPP(I)))**UNSA
          IF((HALF*(PHIP(I)+PHIPP(I)))>ZERO) THEN
           SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))))
          ELSE
           SIG(I) = ZERO
          ENDIF

         ENDDO !iter   
	        UVAR(I,3)=DPLA(I)       
       ENDDO ! NINDX
C=======================================================================
      ELSE !IFLAGSR PLASTIC STRAIN RATE ******************************
C=======================================================================
       DO II=1,NINDX                                              
        I = INDX(II)                                             
        !--------------------
        !PLASTIC FLOW
        !--------------------
        DPLA(I) =  UVAR(I,3) + EM09 
        PLAP(I) = DPLA(I)/TIMESTEP
        !-------------- 
        !update yield :
        !-------------- 
        JJ(I) = 1                                              
        DO J=2,NRATE-1                                        
          IF(PLAP(I) >= UPARAM(25+J)) JJ(I) = J               
        ENDDO                                                  
        J1 = JJ(I)
        J2 = J1+1
        RATE(I)=(PLAP(I) - UPARAM(25+J1))/(UPARAM(26+J1) - UPARAM(25+J1))
        YFAC(I,1)=UPARAM(25+NRATE+J1)*FACYLDI(I)
        YFAC(I,2)=UPARAM(25+NRATE+J2)*FACYLDI(I)
        !Y1 and Y2 for Pl SR dependency
        Y1(I)   = YFAC(I,1)*FINTER(IPM(10+J1,MAT(1)),PLA(I),NPF,TF,DYDX1(I)) 
        Y2(I)   = YFAC(I,2)*FINTER(IPM(10+J2,MAT(1)),PLA(I),NPF,TF,DYDX2(I))        
        YLD(I)  = Y1(I)    + RATE(I)*(Y2(I)-Y1(I))
        YLD(I)  = MAX(YLD(I),EM20)
        DYDX1(I)= DYDX1(I)*YFAC(I,1)
        DYDX2(I)= DYDX2(I)*YFAC(I,2)
        DYDX(I) = DYDX1(I) + RATE(I)*(DYDX2(I)-DYDX1(I))
        YLDP    = DYDX(I)  + (Y2(I)-Y1(I))/
     .           (UPARAM(26+J1)-UPARAM(25+J1)) /TIMESTEP


       
	       !DPLA(I)= UVAR(I,3)
        !DPLA(I)= (SIG(I)-YLD(I))/(THREE*G+DYDX(I))
         DEPLXX(I)=ZERO
         DEPLYY(I)=ZERO
         DEPLZZ(I)=ZERO
         DEPLXY(I)=ZERO
         DEPLYZ(I)=ZERO
         DEPLZX(I)=ZERO


        DO K=1,5
         !algo newton
         !calcul de dsigma/ddpla=dphi/dsigma*dsigma/ddpla
         !phi = phip + phipp
         !dphip/dsigma = dphip/dxp*dxp/dsigma  where  dxp/dsigma = Lp
         !---------------------------------------
         !update yield :
         !---------------------------------------
         PLA(I)    = UVAR(I,1) + DPLA(I)
         PLAP(I)   = DPLA(I) / TIMESTEP    
         JJ(I) = 1                                              
         DO J=2,NRATE-1                                        
          IF(PLAP(I) >= UPARAM(25+J)) JJ(I) = J               
         ENDDO                                                  
         J1 = JJ(I)
         J2 = J1+1
         RATE(I)=(PLAP(I) - UPARAM(25+J1))/(UPARAM(26+J1) - UPARAM(25+J1))
         YFAC(I,1)=UPARAM(25+NRATE+J1)*FACYLDI(I)
         YFAC(I,2)=UPARAM(25+NRATE+J2)*FACYLDI(I)
         !Y1 and Y2 for Pl SR dependency
         Y1(I)   = YFAC(I,1)*FINTER(IPM(10+J1,MAT(1)),PLA(I),NPF,TF,DYDX1(I)) 
         Y2(I)   = YFAC(I,2)*FINTER(IPM(10+J2,MAT(1)),PLA(I),NPF,TF,DYDX2(I)) 
         YLD(I)  = Y1(I)    + RATE(I)*(Y2(I)-Y1(I))
         YLD(I)  = MAX(YLD(I),EM20)
         DYDX1(I)=DYDX1(I)*YFAC(I,1)
         DYDX2(I)=DYDX2(I)*YFAC(I,2)
         DYDX(I)   = (DYDX1(I) + RATE(I)*(DYDX2(I)-DYDX1(I)))
         !--------------------------------------- 
         YLDP   = DYDX(I)+ (Y2(I)-Y1(I))
     .            /(UPARAM(26+J1)-UPARAM(25+J1))  /TIMESTEP

         !--------------------------------------- 

         !YLD(I) =YLD_I+DYDX(I)*DPLA(I)

         FCT = SIG(I)- YLD(I)

          TP1 = EXPA*(SQRT(MAX(ZERO,DELTAP(I))))**(EXPA-2)

          !D_phip/D_XP
          FP1 = TP1 * (XPXX(I) - XPYY(I))
          FP2 = -FP1  
          FP3 = THREE * TP1 * XPXY(I)
          FP4 = THREE * TP1 * XPYZ(I)
          FP5 = THREE * TP1 * XPZX(I) 
         

          TPP1 = EXPA * DELTAPP1(I) ** (EXPA - 1)
          TPP2 = EXPA * DELTAPP2(I) ** (EXPA - 1)
          TPP3 = TWO *(TPP1  -  TPP2) /DELTAPP(I)
         
          !D_phipp/D_XPP
          FPP1 = TPP1 *(THREE_HALF+HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
     .         +TPP2 *(THREE_HALF-HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
          FPP2 = TPP1 *(-THREE_HALF-HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
     .         +TPP2 *(-THREE_HALF+HALF* ( XPPXX(I) - XPPYY(I) )/DELTAPP(I))
          FPP3 = TPP3 * XPPXY(I)
          FPP4 = TPP3 * XPPYZ(I) 
          FPP5 = TPP3 * XPPZX(I)

          DFP1=THIRD*(TWO*AL1*FP1-AL2*FP2)
          DFP2=THIRD*(TWO*AL2*FP2-AL1*FP1)
          DFP3=THIRD*(-AL1*FP1-AL2*FP2)
          DFP4=AL7*FP3
          DFP5=AL9*FP4
          DFP6=AL10*FP5 
         

          DFPP1=FPP1*LPP11+FPP2*LPP21
          DFPP2=FPP1*LPP12+FPP2*LPP22
          DFPP3=FPP1*LPP13+FPP2*LPP23
          DFPP4=FPP3*LPP34
          DFPP5=FPP4*LPP45
          DFPP6=FPP5*LPP56
          
          DU =  UNSA * SIG(I) / (MAX(EM20, PHIP(I)+PHIPP(I)) ) 

          DF1 = DFP1+DFPP1
          DF2 = DFP2+DFPP2
          DF3 = DFP3+DFPP3
          DF4 = DFP4+DFPP4
          DF5 = DFP5+DFPP5
          DF6 = DFP6+DFPP6

          DF = -TWO*G * (DU * DF1) **2 
     .         -TWO*G * (DU * DF2) **2 
     .         -TWO*G * (DU * DF3) **2 
     .         -G * (DU * DF4) **2 
     .         -G * (DU * DF5) **2 
     .         -G * (DU * DF6) **2 

          DF = DF  - YLDP

          !CALCUL PAS PLASTIQUE
          DPLA(I)=DPLA(I)-FCT/DF
          DPLA(I)=MAX(ZERO,DPLA(I))
          !TENSEUR PLASTIQUE
          DEPLXX(I)=DPLA(I)*DF1*DU
          DEPLYY(I)=DPLA(I)*DF2*DU
          DEPLZZ(I)=DPLA(I)*DF3*DU
          DEPLXY(I)=DPLA(I)*DF4*DU
          DEPLYZ(I)=DPLA(I)*DF5*DU
          DEPLZX(I)=DPLA(I)*DF6*DU

          !ACTUALISATION DES CONTRAINTES
          SIGNXX(I)=SIGTRXX(I)- G2*DEPLXX(I)           
          SIGNYY(I)=SIGTRYY(I)- G2*DEPLYY(I) 
          SIGNZZ(I)=SIGTRZZ(I)- G2*DEPLZZ(I)           
          SIGNXY(I)=SIGTRXY(I)- G*DEPLXY(I)
          SIGNYZ(I)=SIGTRYZ(I)- G*DEPLYZ(I)
          SIGNZX(I)=SIGTRZX(I)- G*DEPLZX(I)

          !CALCUL NOUVEAU SIGMA EFFECTIF
          !--------------------
          !BARLAT CRITERION 
          !--------------------
          XPXX(I) = (TWO*AL1*SIGNXX(I)-AL1*SIGNYY(I)-AL1*SIGNZZ(I))/THREE
          XPYY(I) =(-AL2*SIGNXX(I)+TWO*AL2*SIGNYY(I)-AL2*SIGNZZ(I))/THREE
          XPXY(I) = AL7*SIGNXY(I)
          XPYZ(I) = AL9*SIGNYZ(I)
          XPZX(I) = AL10*SIGNZX(I) 
      
          XPPXX(I) = LPP11*SIGNXX(I) + LPP12*SIGNYY(I) +LPP13*SIGNZZ(I)
          XPPYY(I) = LPP21*SIGNXX(I) + LPP22*SIGNYY(I) +LPP23*SIGNZZ(I)
          XPPXY(I) = LPP34*SIGNXY(I)
          XPPYZ(I) = LPP45*SIGNYZ(I)
          XPPZX(I) = LPP56*SIGNZX(I)

          DELTAP(I)   = (XPXX(I)-XPYY(I))**2 + FOUR * (XPXY(I)**2 + XPYZ(I)**2 + XPZX(I)**2 )    
          PHIP(I)  = SQRT(MAX(ZERO, DELTAP(I)))**EXPA

       
          DELTAPP(I)  = (XPPXX(I)-XPPYY(I))**2 + FOUR * (XPPXY(I)**2 + XPPYZ(I)**2 + XPPZX(I)**2 )
          DELTAPP(I)  = SQRT(MAX(EM20, DELTAPP(I)))
          DELTAPP1(I) = THREE_HALF*(XPPXX(I)-XPPYY(I))+HALF*DELTAPP(I)
          DELTAPP2(I) = THREE_HALF*(XPPXX(I)-XPPYY(I))-HALF*DELTAPP(I)

          PHIPP(I)= DELTAPP1(I)**EXPA + DELTAPP2(I)**EXPA
       
!         !SIG(I) = (HALF*(PHIP(I)+PHIPP(I)))**UNSA
          IF((HALF*(PHIP(I)+PHIPP(I)))>ZERO) THEN
           SIG(I) = EXP(UNSA*LOG(HALF*(PHIP(I)+PHIPP(I))))
          ELSE
           SIG(I) = ZERO
          ENDIF
        ENDDO ! iter k   
        UVAR(I,3) =DPLA(I)                                                                  
       ENDDO !ndx
c----------------------------
       ENDIF   !flag formulation 
      ENDIF
C=======================================================================
      DO I=1,NEL
C 
       IF( DPLA(I) > ZERO)THEN
        H(I)=(YLD(I)-UVAR(I,2))/DPLA(I)
        H(I)=MAX(EM10,H(I))
        UVAR(I,2)=YLD(I)
        UVAR(I,4)=H(I)/G2!(H(I)+E)
        UVAR(I,5)=H(I)
       ENDIF
       PLA(I)=UVAR(I,1) + DPLA(I)
       UVAR(I,1) = PLA(I)
       ETSE(I)= UVAR(I,4)
       YLD(I) = UVAR(I,2)
       H(I)   = UVAR(I,5)
      ENDDO     
      RETURN
      END
