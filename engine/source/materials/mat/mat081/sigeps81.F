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
Chd|  SIGEPS81                      source/materials/mat/mat081/sigeps81.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        FCRIT                         source/materials/mat/mat081/sigeps81.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS81 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,NGL    , 
     2     NPF    ,TF     ,TIME    ,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,AMU    ,DEFP    ,SOUNDSP ,VISCMAX ,UVAR   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include      "param_c.inc" 
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "vect01_c.inc"
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
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VOLUME  | NEL     | F | R | VOLUME
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
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR
      INTEGER NGL(NEL)
      my_real TIME
      my_real UPARAM(NUPARAM)
      my_real,DIMENSION(NEL), INTENT(IN) :: RHO,RHO0,VOLUME,AMU,
     .   EPSPXX,EPSPYY,EPSPZZ,EPSPXY,EPSPYZ,EPSPZX,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   EPSXX ,EPSYY ,EPSZZ ,EPSXY ,EPSYZ ,EPSZX ,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
      TARGET :: DEFP
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
       my_real ,DIMENSION(NEL), INTENT(OUT) :: SOUNDSP,VISCMAX,
     .    SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX,SIGVXX,SIGVYY,SIGVZZ
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
       my_real UVAR(NEL,NUVAR),DEFP(NEL,2)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
       my_real FCRIT, FINTER ,TF(*)
      EXTERNAL FINTER, FCRIT
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NIT,SOFT_FLAG
      INTEGER, PARAMETER :: NITMAX=3
      my_real ALPHA,TGB,TGP,MAX_DILAT,KINI,GINI,YLDINI,CAPINI,DELTA,
     .  DRCDP,DFDP,DFDPB,DFDC,DGDP,HH,DLAMBDA,DWPX2,QX2,DEPSPV,DEPSLV,
     .  DFDS1,DFDS2,DFDS3,DFDS4,DFDS5,DFDS6,DEPSP1,DEPSP2,DEPSP3,DEPSP4,
     .  DEPSP5,DEPSP6,DEPSL1,DEPSL2,DEPSL3,DEPSL4,DEPSL5,DEPSL6,
     .  SY1,SY2,SY3,SY4,SY5,SY6,QY,FY,PY,UY,DERI,FAC,
     .  EPSPVOL0,RHOW0,KWATER,POR0,VISCFAC,SAT0,U0,UNMSPOR0,TOLMU,PU,
     .  DRCDPB,AA,RC,RP,PMPA,F1,F2,X,X1,X2,DX    
      my_real, DIMENSION(NEL) :: K,G,G2,C,PB,DEPSVOL,DAV,D1,D2,D3,
     .  DS1,DS2,DS3,DS4,DS5,DS6,DP,S1,S2,S3,S4,S5,S6,P,
     .  DCDEPSP,DPBDEPSP,PA,PBMPA,PBMPA2,S1N,S2N,S3N,S4N,S5N,S6N,
     .  P0,PN,QN,Q2,Q,F,DE1,DE2,DE3,DE4,DE5,DE6,POR,MUW,U,DU,DUDMU,
     .  PO,UO,SO1,SO2,SO3,SO4,SO5,SO6,FO
      my_real SMALL,BIG,TOL 
      my_real ,DIMENSION(:), POINTER :: EPSPD,EPSPV
C=======================================================================
C         UVAR(1) COHESION
C         UVAR(2) CAP PRESSURE PB
C         UVAR(3) U PORE PRESSURE
C         UVAR(4) POR POROSITY
C         UVAR(5) SAT SATURATION
C         UVAR(6) U=CAP SHIFT (porosity)
c--------------------------------------
C         UPARAM(1) K
C         UPARAM(2) G
C         UPARAM(3) Tangente beta (critere)
C         UPARAM(4) Tangente Psi  (plastic flow)
C         UPARAM(5) Alpha Pa/Pb
C         UPARAM(6) Max Dilatancy   
C         UPARAM(7) Flag to deactivate cap softening
C
C         UPARAM(8)  Initial Epsvp
C         UPARAM(9)  KWATER 
C         UPARAM(10) POR0 Initial porosity
C         UPARAM(11) SAT0 Initial "saturation" (1+mu0)
C         UPARAM(12) U0 Initial pore pressure
C         UPARAM(13) TOLMU transition for criterion shift
C         UPARAM(14) Viscosity factor
C=======================================================================
      SMALL = EM10
      BIG   = EP20
      TOL   = EM20
c---------------------     
      EPSPD => DEFP(1:NEL,1)   ! Von Mises Equivalent Plastic Strain  
      EPSPV => DEFP(1:NEL,2)   ! Volumetric Plastic Strain
c---------------------     
      KINI = UPARAM(1)
      GINI = UPARAM(2)
      TGB  = UPARAM(3)
      TGP  = UPARAM(4)
      ALPHA= UPARAM(5)
      MAX_DILAT = UPARAM(6) 
      SOFT_FLAG = INT(UPARAM(7)) 
C
      EPSPVOL0 = UPARAM(8)
      KWATER   = UPARAM(9)
      POR0     = UPARAM(10)
      SAT0     = UPARAM(11)  ! sat0=1+mu0 = la saturation initiale quand <1
      U0       = UPARAM(12)
      TOLMU    = UPARAM(13)
      VISCFAC  = UPARAM(14)
      YLDINI   = UPARAM(15)
      CAPINI   = UPARAM(16)
c
      UNMSPOR0 = ONE-POR0      
c------------------------------------------------
c     ATTENTION INITIALISATIONS PROVISOIREMENT ICI
c     A rendre compatible : IF(EPSPV(I) == ZERO) EPSPV(I)=UPARAM(8)! EPSVINI
      IF (ISIGI == 0) THEN  
        IF (TIME == ZERO) THEN
          DO I=1,NEL
            IF(EPSPV(I) == ZERO) EPSPV(I)=UPARAM(8)
            ! -------------------------------------------------------------------------
            ! Modification (26/05/2020) - V.DAVAZE - for missing scale factors in manual
            IF (IFUNC(3) > 0) THEN 
              C(I) = YLDINI*FINTER(IFUNC(3),EPSPD(I),NPF,TF,DCDEPSP(I))
            ELSE
              C(I) = YLDINI
            ENDIF
            C(I) = MAX(C(I), ZERO)
            IF (IFUNC(4) > 0) THEN
              PB(I) = CAPINI*FINTER(IFUNC(4),EPSPV(I),NPF,TF,DPBDEPSP(I))
            ELSE
              PB(I) = CAPINI
            ENDIF
            ! -------------------------------------------------------------------------
            UVAR(I,1)=C(I)
            UVAR(I,2)=PB(I)
            UVAR(I,3)=U0
            UVAR(I,4)=POR0
            UVAR(I,5)=SAT0
            MUW(I)=SAT0-ONE
            IF(MUW(I) >= TOLMU)THEN
               UVAR(I,6)=KWATER*MUW(I)
            ELSEIF(MUW(I) > -TOLMU)THEN
               UVAR(I,6)=KWATER/FOUR/TOLMU*(MUW(I)+TOLMU)**2
            ELSE
               UVAR(I,6)=ZERO
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C-----------------------------------------------
C     ELASTIC SOLUTION
C------------------------------------------
C for practical reason (ALE) Note the followings :
C - SIGO and SIGN are the skeleton stress tensors (effective stress)
C - Pore presure U goes into SIGV and is stored into AUX UVAR (I,5)
C
      IF (IFUNC(1) > 0)THEN
        DO I=1,NEL 
          K(I) = KINI*FINTER(IFUNC(1),EPSPV(I),NPF,TF,DERI)
        ENDDO
      ELSE
        K(1:NEL) = KINI
      ENDIF
      IF (IFUNC(2) > 0)THEN
        DO I=1,NEL 
          G(I) = GINI*FINTER(IFUNC(2),EPSPV(I),NPF,TF,DERI)
        ENDDO
      ELSE
        G(1:NEL) = GINI
      ENDIF
c---------------      
      DO I=1,NEL
        ! -------------------------------------------------------------------------
        ! Modification (26/05/2020) - V.DAVAZE - for missing scale factors in manual
        IF (IFUNC(3) > 0) THEN 
          C(I) = YLDINI*FINTER(IFUNC(3),EPSPD(I),NPF,TF,DCDEPSP(I))
        ELSE
          C(I) = YLDINI
        ENDIF
        C(I) = MAX(C(I), ZERO)
        IF (IFUNC(4) > 0) THEN
          PB(I) = CAPINI*FINTER(IFUNC(4),EPSPV(I),NPF,TF,DPBDEPSP(I))
        ELSE
          PB(I) = CAPINI
        ENDIF
        ! -------------------------------------------------------------------------
        PA(I) = ALPHA*PB(I)
        PBMPA(I) = (PB(I)-PA(I))
        PBMPA2(I)= PBMPA(I)**2
        DELTA = (PA(I)*TGB + C(I))**2 + EIGHT*PBMPA2(I)*TGB**2
        IF (TGB > SMALL) THEN
          P0(I)=PA(I)+(-(PA(I)*TGB+C(I))+SQRT(DELTA))/FOUR/TGB
        ELSE
          P0(I)=PA(I)
        ENDIF
        G2(I) = TWO*G(I)
C
        DEPSVOL(I) = (DEPSXX(I) +DEPSYY(I) +DEPSZZ(I))
        DAV(I) = DEPSVOL(I)*THIRD
        D1(I)  = DEPSXX(I)-DAV(I)
        D2(I)  = DEPSYY(I)-DAV(I)
        D3(I)  = DEPSZZ(I)-DAV(I)
c       Increments elastiques pression et deviateur        
        DP(I) =-K(I)* DEPSVOL(I)
        DS1(I)= G2(I)* D1(I) 
        DS2(I)= G2(I)* D2(I)
        DS3(I)= G2(I)* D3(I)
        DS4(I)= G(I) * DEPSXY(I)
        DS5(I)= G(I) * DEPSYZ(I)
        DS6(I)= G(I) * DEPSZX(I)
c       Valeurs elastiques pression et deviateur   
        PO(I)  =-(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD
        SO1(I) = SIGOXX(I) + PO(I)
        SO2(I) = SIGOYY(I) + PO(I)
        SO3(I) = SIGOZZ(I) + PO(I)
        SO4(I) = SIGOXY(I)
        SO5(I) = SIGOYZ(I)
        SO6(I) = SIGOZX(I)

        S1(I) = SO1(I) + DS1(I)
        S2(I) = SO2(I) + DS2(I)
        S3(I) = SO3(I) + DS3(I)
        S4(I) = SO4(I) + DS4(I)
        S5(I) = SO5(I) + DS5(I)
        S6(I) = SO6(I) + DS6(I)
        P(I)  = PO(I)  + DP(I)
c       Contraintes elastiques
        SIGNXX(I) = S1(I)-P(I)
        SIGNYY(I) = S2(I)-P(I)
        SIGNZZ(I) = S3(I)-P(I)
        SIGNXY(I) = S4(I)
        SIGNYZ(I) = S5(I)
        SIGNZX(I) = S6(I)
C
      ENDDO
c------------------------------------------
c     old criterion
      DO I=1,NEL          
        Q2(I) = THREE_HALF*(SO1(I)**2+SO2(I)**2+SO3(I)**2)
     .        + THREE*(SO4(I)**2+SO5(I)**2+SO6(I)**2)
        Q(I)  = SQRT(Q2(I))
        UO(I) = UVAR(I,6)
        FO(I) = FCRIT(PO(I),UO(I),TGB,C(I),PA(I),P0(I),PBMPA2(I),Q(I))
      ENDDO
c------------------------------------------
cfp_poro +18 Pore pressure is calculated at the end
cfp_poro in the first part U is the non linear cap shift
      IF (SAT0 > ZERO) THEN
        DO I=1,NEL
          POR(I)=ONE-UNMSPOR0*EXP(EPSPV(I)-EPSPVOL0)
          FAC=MAX(EM03,POR(I)/POR0)
          MUW(I)=SAT0/FAC*AMU(I)
          IF (MUW(I) >= TOLMU) THEN
            U(I)=KWATER*MUW(I)
          ELSEIF (MUW(I) > -TOLMU) THEN
            U(I)=KWATER/FOUR/TOLMU*(MUW(I)+TOLMU)**2
          ELSE
            U(I)=ZERO
          ENDIF
          DU(I) = U(I) - UVAR(I,6)
          DUDMU(I)=ZERO
          IF (MUW(I) >= -TOLMU) DUDMU(I)=KWATER*SAT0/FAC
        ENDDO
      ELSE
        DO I=1,NEL
          MUW(I)=-ONE
          U(I)  = ZERO
          DU(I) = ZERO
          DUDMU(I)=ZERO
        ENDDO
      ENDIF  
c------------------------------------------
c     EVALUATION FONCTION CRITERE DE PLASTICITE
c------------------------------------------
      DO I=1,NEL          
        Q2(I) = THREE_HALF*(S1(I)**2+S2(I)**2+S3(I)**2)
     .        + THREE*(S4(I)**2+S5(I)**2+S6(I)**2)
        Q(I)= SQRT(Q2(I))
        F(I)= FCRIT(P(I),U(I),TGB,C(I),PA(I),P0(I),PBMPA2(I),Q(I))
      ENDDO
c------------------------------------------
c     ITERATION POUR TROUVER LE PT D'INTERSECTION AVEC LE CRITERE
c------------------------------------------
      DO I=1,NEL
        IF (F(I) > TOL) THEN
          IF (FO(I) < ZERO) THEN
            X1 = ONE
            X2 = ZERO
            F1 = FO(I)
            F2 = F(I)
            DO NIT=1,NITMAX
              X   = (X1*F2 - X2*F1) / (F2 - F1)  ! regula falsi
              SY1 = S1(I) - X * DS1(I)
              SY2 = S2(I) - X * DS2(I)    
              SY3 = S3(I) - X * DS3(I)
              SY4 = S4(I) - X * DS4(I)
              SY5 = S5(I) - X * DS5(I)
              SY6 = S6(I) - X * DS6(I)
              PY  = P(I)  - X * DP(I) 
              UY  = U(I)  - X * DU(I)
              QY  = THREE_HALF*(SY1**2+SY2**2+SY3**2) + THREE*(SY4**2+SY5**2+SY6**2)
              QY  = SQRT(QY)
              FY  = FCRIT(PY,UY,TGB,C(I),PA(I),P0(I),PBMPA2(I),QY)
c
              IF (FY*F1 > 0) THEN
                X1 = X
                F1 = FY
              ELSEIF (FY*F2 > 0) THEN
                X2 = X
                F2 = FY
              ELSE
                EXIT
              ENDIF
c              IF (NIT == NITMAX) THEN
c                print*, DX,FY,F(I)
c              ENDIF
            ENDDO
c
c           constraints
            DS1(I) = X*DS1(I)
            DS2(I) = X*DS2(I)
            DS3(I) = X*DS3(I)
            DS4(I) = X*DS4(I)
            DS5(I) = X*DS5(I)
            DS6(I) = X*DS6(I)
            DP(I)  = X*DP(I)
            DU(I)  = X*DU(I)
c
            S1(I) = SY1
            S2(I) = SY2
            S3(I) = SY3
            S4(I) = SY4
            S5(I) = SY5
            S6(I) = SY6
            P(I)  = PY
            U(I)  = UY
            Q(I)  = QY
c           deformations
            DE1(I) = X*D1(I)
            DE2(I) = X*D2(I)
            DE3(I) = X*D3(I)
            DE4(I) = X*DEPSXY(I)
            DE5(I) = X*DEPSYZ(I)
            DE6(I) = X*DEPSZX(I)
            DEPSVOL(I)=X*DEPSVOL(I)
          ELSE
            DE1(I) = D1(I)
            DE2(I) = D2(I)
            DE3(I) = D3(I)
            DE4(I) = DEPSXY(I)
            DE5(I) = DEPSYZ(I)
            DE6(I) = DEPSZX(I)
          ENDIF
        ENDIF
      ENDDO
c---------------------------------
c     CALCUL DE L'ECOULEMENT PLASTIQUE
c---------------------------------
      DO I=1,NEL
        IF (F(I) > ZERO) THEN
          IF (P(I) <= PA(I)) THEN
            RC = ONE     
            DFDP =-TGB  
            DGDP =-TGP  
            DFDPB= ZERO 
          ELSE
            IF (P(I) < P0(I)) THEN                                 
              PU = P(I)                                           
            ELSE IF (P(I) <= P0(I)+U(I)) THEN                      
              PU = P0(I)                                          
            ELSE                                                   
              PU = P(I)-U(I)                                      
            ENDIF                                                  
            PMPA = PU - PA(I)                                      
            RP = PMPA / PBMPA(I)                                   
            RC = ONE - MIN(ONE, PMPA**2/PBMPA2(I))                   
            RC = SQRT(RC)                                          
            AA = TGB*PU + C(I)                                     
            DRCDP = -PMPA/PBMPA2(I)/MAX(SMALL,RC)                  
            DFDP  = -(TGB*RC + DRCDP*AA)                           
            DRCDPB = PMPA**2/PBMPA2(I)/PBMPA(I)                    
            DFDPB = -PU*PMPA*AA/PB(I)/PBMPA2(I)                     
            DFDPB = DFDPB/MAX(SMALL,RC)                           
c
            IF (P(I) > P0(I)) THEN                                 
              DGDP = DFDP   ! associe sur le cap                   
            ELSE                                                   
              DGDP = -TGP*(P0(I)-P(I))/(P0(I)-PA(I))              
              IF (SOFT_FLAG == 1) DFDPB = ZERO                     
              DU(I) = ZERO                                         
            ENDIF                                                  
            IF (MUW(I) >= ZERO) DGDP = ZERO
          ENDIF
c
          DFDC = -RC
c         MAX_DILAT est toujours negatif.
          IF (RHO(I) <= (ONE+MAX_DILAT)*RHO0(I)) THEN
            DGDP=MAX(ZERO,DGDP)
            DFDP=MAX(ZERO,DFDP)
          ENDIF

          IF (Q(I) > EM20) THEN
            FAC   = THREE_HALF/Q(I)
            DFDS1 = S1(I)*FAC
            DFDS2 = S2(I)*FAC
            DFDS3 = S3(I)*FAC
            DFDS4 = S4(I)*FAC
            DFDS5 = S5(I)*FAC
            DFDS6 = S6(I)*FAC
          ELSE
            DFDS1 =ZERO
            DFDS2 =ZERO
            DFDS3 =ZERO
            DFDS4 =ZERO
            DFDS5 =ZERO
            DFDS6 =ZERO        
          ENDIF
c
          IF (RC > EM05) THEN
c           DENOMINATEUR         
            HH = THREE*G(I) - DFDC*DCDEPSP(I) + K(I)*DFDP*DGDP 
     .         - DGDP*DFDPB*DPBDEPSP(I)
c
c           INCREMENT DEFORMATIONS PASTIQUES
c           il y a deux fois dfds4 c'est df/ds12 et non (df/ds12 + df/ds21)
c
            DLAMBDA = DFDS1*DS1(I) + DFDS2*DS2(I) + DFDS3*DS3(I)+
     .              +(DFDS4*DS4(I)+ DFDS5*DS5(I)+ DFDS6*DS6(I))*TWO
     .              + DFDP*(DP(I)-DU(I))
C
            DLAMBDA = DLAMBDA/MAX(HH,EM20)
C
            DEPSP1 =   DLAMBDA*DFDS1 
            DEPSP2 =   DLAMBDA*DFDS2 
            DEPSP3 =   DLAMBDA*DFDS3 
c           le facteur 2 est du au fait que c'est des gamma
            DEPSP4 = TWO*DLAMBDA*DFDS4 
            DEPSP5 = TWO*DLAMBDA*DFDS5 
            DEPSP6 = TWO*DLAMBDA*DFDS6
            DEPSPV = DLAMBDA*DGDP
          ELSE
            DEPSP1 = DE1(I)
            DEPSP2 = DE2(I)
            DEPSP3 = DE3(I)
            DEPSP4 = DE4(I)
            DEPSP5 = DE5(I)
            DEPSP6 = DE6(I)
            DEPSPV = (DP(I)-DU(I))/(K(I)+DPBDEPSP(I))
            IF (MUW(I) >= ZERO) DEPSPV=ZERO
          ENDIF
C
c         INCREMENT DEFORMATIONS ELASTIQUES DEVIATORIQUE
C
          DEPSL1 = DE1(I) - DEPSP1
          DEPSL2 = DE2(I) - DEPSP2
          DEPSL3 = DE3(I) - DEPSP3
          DEPSL4 = DE4(I) - DEPSP4
          DEPSL5 = DE5(I) - DEPSP5
          DEPSL6 = DE6(I) - DEPSP6
          DEPSLV = DEPSVOL(I) + DEPSPV
C
c         INCREMENT CONTRAINTES !! A PARTIR DU POINT SUR LE CRITERE !!
C
          DS1(I)= G2(I)*DEPSL1
          DS2(I)= G2(I)*DEPSL2
          DS3(I)= G2(I)*DEPSL3
          DS4(I)= G(I) *DEPSL4
          DS5(I)= G(I) *DEPSL5
          DS6(I)= G(I) *DEPSL6
          DP(I) =-K(I) *DEPSLV
C
c         NOUVELLES CONTRAINTES 
C
          S1N(I) = S1(I)+DS1(I)
          S2N(I) = S2(I)+DS2(I)
          S3N(I) = S3(I)+DS3(I)
          S4N(I) = S4(I)+DS4(I)
          S5N(I) = S5(I)+DS5(I)
          S6N(I) = S6(I)+DS6(I)
          PN(I)  = P(I) +DP(I)
C
c         NOUVEAUX PARAMETRES ECROUISSAGE
C         
          DWPX2 = (S1N(I)+S1(I))*DEPSP1
     .          + (S2N(I)+S2(I))*DEPSP2    
     .          + (S3N(I)+S3(I))*DEPSP3    
     .          + ((S4N(I)+S4(I))*DEPSP4  
     .          +  (S5N(I)+S5(I))*DEPSP5
     .          +  (S6N(I)+S6(I))*DEPSP6 )*HALF
C
          Q2(I) = THREE_HALF*(S1N(I)**2+S2N(I)**2+S3N(I)**2)
     .          + THREE*(S4N(I)**2+S5N(I)**2+S6N(I)**2)
          QN(I) = SQRT(Q2(I))
          QX2   = Q(I) + QN(I)
C
          IF (QX2 > SMALL) EPSPD(I) = EPSPD(I) + DWPX2/QX2
C          
          ! -------------------------------------------------------------------------
          ! Modification (26/05/2020) - V.DAVAZE - for missing scale factors in manual
          IF (IFUNC(3) > 0) THEN 
            C(I) = YLDINI*FINTER(IFUNC(3),EPSPD(I),NPF,TF,DCDEPSP(I))
          ELSE
            C(I) = YLDINI
          ENDIF
          C(I) = MAX(C(I), ZERO)
          ! -------------------------------------------------------------------------          
C
          IF (SOFT_FLAG == 1) THEN
             EPSPV(I) = EPSPV(I) + MAX(DEPSPV,ZERO)
          ELSE
             EPSPV(I) = EPSPV(I) + DEPSPV
          ENDIF
C     
          ! -------------------------------------------------------------------------
          ! Modification (26/05/2020) - V.DAVAZE - for missing scale factors in manual
          IF (IFUNC(4) > 0) THEN
            PB(I) = CAPINI*FINTER(IFUNC(4),EPSPV(I),NPF,TF,DPBDEPSP(I))
          ELSE
            PB(I) = CAPINI
          ENDIF
          ! -------------------------------------------------------------------------
          IF (IFUNC(1) > 0) K(I) = KINI*FINTER(IFUNC(1),EPSPV(I),NPF,TF,DERI)
          IF (IFUNC(2) > 0) G(I) = GINI*FINTER(IFUNC(2),EPSPV(I),NPF,TF,DERI)
C
          UVAR(I,1) = C(I)
          UVAR(I,2) = PB(I)
          PA(I) = ALPHA*PB(I)
          PBMPA2(I) = (PB(I)-PA(I))**2
c         fp_poro +6
          DELTA = (PA(I)*TGB+C(I))**2+EIGHT*PBMPA2(I)*TGB**2
          IF (TGB > SMALL) THEN
            P0(I)=PA(I)+(-(PA(I)*TGB+C(I))+SQRT(DELTA))/FOUR/TGB
          ELSE
            P0(I)=PA(I)
          ENDIF
C
c         UPDATE CAP SHIFT for NEW EPSSO (porosity)
C
          IF (SAT0 > ZERO) THEN
            POR(I) = ONE - UNMSPOR0*EXP(EPSPV(I)-EPSPVOL0)
            FAC = MAX(EM03,POR(I)/POR0)
            MUW(I) = SAT0/FAC*AMU(I)
            IF (MUW(I) >= TOLMU) THEN
              U(I) = KWATER*MUW(I)
            ELSEIF (MUW(I) > -TOLMU) THEN
              U(I)=KWATER/FOUR/TOLMU*(MUW(I)+TOLMU)**2
            ELSE
              U(I)=ZERO
            ENDIF
            IF (MUW(I) >= -TOLMU) DUDMU(I)=MAX(DUDMU(I),KWATER*SAT0/FAC)
          ENDIF
C
c         REPROJECTION  CRIT
C
          FY = FCRIT(PN(I),U(I),TGB,C(I),PA(I),P0(I),PBMPA2(I),QN(I))
c
          IF (FY > SMALL) THEN
            IF (TGB*PN(I) + C(I) <= ZERO) THEN
              PN(I)  =-C(I)/TGB
              S1N(I) = ZERO
              S2N(I) = ZERO
              S3N(I) = ZERO
              S4N(I) = ZERO
              S5N(I) = ZERO
              S6N(I) = ZERO
C             WRITE(6,*)'TRI-TRACTION FAILURE'
            ELSE 
c******
c               fp_poro This section has been simplified w.r.t. DPCAP
c******
                IF (PN(I) < P0(I)) THEN
                   PU = PN(I)
                ELSE IF (PN(I) <= P0(I)+U(I)) THEN
                   PU = P0(I)
                ELSE
                   PU = PN(I) - U(I)
                ENDIF
                IF (PN(I) > PA(I)) THEN
                  RC = SQRT(MAX(ZERO,ONE - ((PU-PA(I))**2/PBMPA2(I))))
                ELSE
                  RC = ONE
                ENDIF
                IF (RC > TOL ) THEN        ! changed by Marian at 2017.12.06
                  IF (QN(I) > SMALL) THEN
                    X = RC*(PU*TGB+C(I))/QN(I)
C                   IF (X < ONE-EM02 .OR. X > ONE) THEN
C                      WRITE(7,*)'REPROJ Q',X,FY,PN(I),PU,QN(I)
C                      WRITE(6,*)'REPROJ Q',X,FY,PN(I),PU,QN(I)
C                   ENDIF
                    S1N(I) = X*S1N(I)
                    S2N(I) = X*S2N(I)
                    S3N(I) = X*S3N(I)
                    S4N(I) = X*S4N(I)
                    S5N(I) = X*S5N(I)
                    S6N(I) = X*S6N(I)
                  ENDIF
                ELSE
                  S1N(I) = ZERO
                  S2N(I) = ZERO 
                  S3N(I) = ZERO
                  S4N(I) = ZERO 
                  S5N(I) = ZERO 
                  S6N(I) = ZERO
                  PN(I)  = PB(I)+U(I)                    
               ENDIF               
            ENDIF
          ENDIF
          SIGNXX(I) = S1N(I)-PN(I)
          SIGNYY(I) = S2N(I)-PN(I)
          SIGNZZ(I) = S3N(I)-PN(I)
          SIGNXY(I) = S4N(I)
          SIGNYZ(I) = S5N(I)
          SIGNZX(I) = S6N(I)
C         
        ENDIF   ! F > 0       
      ENDDO     ! I=1,NEL
c----------------------------------------------
c     END OF PLASTICITY PROJECTION LOOP
c----------------------------------------------
c     update porosity
c----------------------------------------------
      IF (SAT0 > ZERO) THEN
        DO I=1,NEL
          UVAR(I,6) = U(I)   ! cap shift
          UVAR(I,4) = POR(I)
          UVAR(I,5) = MUW(I) + ONE
c         Pore pressure is calculated here
          IF (MUW(I) >  ZERO) THEN
            U(I) = KWATER*MUW(I)
          ELSE
            U(I) = ZERO
          ENDIF
        ENDDO
C
        DO I=1,NEL
          VISCMAX(I) = ZERO
c         fp_poro adding viscosity close to saturation
          IF (MUW(I) > -TOLMU) THEN 
            VISCMAX(I) = VISCFAC*(SQRT(KWATER*RHO(I))*VOLUME(I)**THIRD)
            U(I) = U(I) - VISCMAX(I)*(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))
          ENDIF 
c         fp_poro the pore pressure is stored in the viscous stress
c         for practical reasons including compatibility with ALE
          SIGVXX(I) = -U(I)
          SIGVYY(I) = -U(I)
          SIGVZZ(I) = -U(I)
          UVAR(I,3) = U(I)
        ENDDO
      ENDIF
c----------------------------------------------
      DO I=1,NEL
        SOUNDSP(I) = SQRT((K(I) + FOUR_OVER_3*G(I) + DUDMU(I))/RHO(I))
      ENDDO
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  FCRIT                         source/materials/mat/mat081/sigeps81.F
Chd|-- called by -----------
Chd|        FAIL_INIEVO_C                 source/materials/fail/inievo/fail_inievo_c.F
Chd|        FAIL_INIEVO_S                 source/materials/fail/inievo/fail_inievo_s.F
Chd|        SIGEPS81                      source/materials/mat/mat081/sigeps81.F
Chd|-- calls ---------------
Chd|====================================================================
      my_real FUNCTION FCRIT(P,U,TGB,COH,PA,P0,PBMPA2,Q)
#include      "implicit_f.inc"
      my_real P,P0,U,TGB,COH,PA,PBMPA2,Q
      my_real A,RC,PU,DP
C=======================================================================
      IF (P < P0) THEN
        PU = P
      ELSE IF (P <= P0+U) THEN
        PU = P0
      ELSE
        PU = P - U
      ENDIF
      DP = PU - PA
      IF (DP > ZERO) THEN
        RC = ONE - MIN(ONE, DP**2/PBMPA2)
        RC = SQRT(RC)
      ELSE
        RC = ONE
      ENDIF
c
      A = MAX(ZERO, PU*TGB + COH)
      FCRIT = Q - RC*A
c-----------
      RETURN
      END
