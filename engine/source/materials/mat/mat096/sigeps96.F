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
Chd|  SIGEPS96                      source/materials/mat/mat096/sigeps96.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS96 (
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   , 
     2     NPF     ,TF      ,UPARAM  ,UVAR    ,JTHE    ,
     3     RHO     ,TEMP    ,DEFP    ,SOUNDSP ,OFF     ,EPSD    ,
     4     EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     5     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     6     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  )
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
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C RHO     | NEL     | F | R | DENSITY
C TEMP    | NEL     | F | R | ELEMENT TEMPERATURE (TSTAR)
C DEFP    | NEL     | F | R | PLASTIC STRAIN
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,NFUNC,JTHE
      INTEGER NPF(*),NGL(NEL),IFUNC(NFUNC)
      my_real TF(*),UPARAM(NUPARAM)
      my_real,DIMENSION(NEL), INTENT(IN) :: RHO,TEMP,OFF,
     .   EPSPXX,EPSPYY,EPSPZZ,EPSPXY,EPSPYZ,EPSPZX,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
       my_real ,DIMENSION(NEL), INTENT(OUT) :: SOUNDSP,
     .    SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
       my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT), TARGET :: UVAR
       my_real ,DIMENSION(NEL,2), INTENT(INOUT), TARGET :: DEFP
       my_real ,DIMENSION(NEL), INTENT(INOUT) :: EPSD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITER,NITER,IVARF,IFUNC_E,IFUNC_YLD
      INTEGER, DIMENSION(NEL) :: IPOS1,ILEN1,IAD1,IPOS2,ILEN2,IAD2
c
      my_real TANB,TANP,EINI,KINI,GINI,NU,YXI,KEP,H,HO,HM,RES,DRES,
     .  SIGY,SIGA,SIGB,SIGR,RA1,RA2,RB,RC,NA,FAC,Q2,PP1,PP2,PLA2,PLA,DPLA,
     .  DFDP,DFDC,DGDP,DENOM,LDOT,DWPX2,QX2,DEPSLV,DEPSPV,DEPSPD,
     .  E1,E2,E3,E4,E5,E6,TR,SY1,SY2,SY3,SY4,SY5,SY6,
     .  DFDS1,DFDS2,DFDS3,DFDS4,DFDS5,DFDS6,DEPSP1,DEPSP2,DEPSP3,DEPSP4,
     .  DEPSP5,DEPSP6,DEPSL1,DEPSL2,DEPSL3,DEPSL4,DEPSL5,DEPSL6,EPSP,
     .  QY,FY,PY,F1,F2,X,X1,X2,DX,ALPHA
      my_real, DIMENSION(NEL) :: K,G,G2,RA,DEPSV,DAV,D1,D2,D3,
     .  DS1,DS2,DS3,DS4,DS5,DS6,DP,DE1,DE2,DE3,DE4,DE5,DE6,DYDX,
     .  FO,PO,SO1,SO2,SO3,SO4,SO5,SO6,S1,S2,S3,S4,S5,S6,P,YLD,
     .  S1N,S2N,S3N,S4N,S5N,S6N,PN,QN,Q,F,FAC_E,FRATE   
      my_real SMALL,BIG,TOL 
      my_real ,DIMENSION(:), POINTER :: EPSPD,EPSPV
C=======================================================================
c      UPARAM(1)   E    : Young modulus
c      UPARAM(2)   G    : Shear modulus
c      UPARAM(3)   K    : Bulk modulus
c      UPARAM(4)   NU   : Poisson coefficient
c      UPARAM(5)   TANB : Tangent beta (yield friction angle)
c      UPARAM(6)   TANP : Tangent Psi  (plastic flow angle)
c      UPARAM(7)   YXI  : sqrt(1 + TANB^2)
c      UPARAM(8)   SIGY : initial yield value in pure shear (p=0)
c      UPARAM(9)   SIGA : Max yield gain during early hardening peak
c      UPARAM(10)  SIGB : Max Yield drop during softening phase
c      UPARAM(11)  SIGR : Rubbery modulus (hardening modulus in rubbery state)
c      UPARAM(12)  RA1  : 1st exponent coefficient for SIGA evolution
c      UPARAM(13)  RA2  : 2nd exponent coefficient for SIGA evolution
c      UPARAM(14)  NA   : exponent coefficient for temperature dependency of SIGA
c      UPARAM(15)  RB   : exponent coefficient for SIGB evolution
c      UPARAM(16)  RC   : coefficient for SIGR evolution
c      UPARAM(20)  NUP  : Plastic deformation Poisson coefficient
c---------------------     
c      UVAR(1)     YIELD
c      UVAR(2)     EPSD
C=======================================================================
      NITER = 3
      SMALL = EM10
      BIG   = EP20
      TOL   = EM20
c---------------------     
      EPSPD => DEFP(1:NEL,1)   ! Von Mises Equivalent Plastic Strain  
      EPSPV => DEFP(1:NEL,2)   ! Volumetric Plastic Strain
c---------------------     
      EINI = UPARAM(1)
      GINI = UPARAM(2)
      KINI = UPARAM(3)
      NU   = UPARAM(4)
      TANB = UPARAM(5)
      TANP = UPARAM(6)
      YXI  = UPARAM(7)
      SIGY = UPARAM(8) 
      SIGA = UPARAM(9) 
      SIGB = UPARAM(10)
      SIGR = UPARAM(11)
      RA1  = UPARAM(12)
      RA2  = UPARAM(13)
      NA   = UPARAM(14)
      RB   = UPARAM(15)
      RC   = UPARAM(16)
      KEP  = UPARAM(17)    ! 1 / (1 + nup^2)   : d_epsp = kep*ldot
      ALPHA= UPARAM(19)    ! strain rate smoothing coefficient
c
      IFUNC_E   = IFUNC(1)
      IFUNC_YLD = IFUNC(2)
c-------------------------------------------
c     TEMPERATURE AND STRAIN RATE DEPENDENCY (YOUNG MODULUS, YIELD)
c-------------------------------------------
      IVARF = 2
      IF (JTHE == 1. and. IFUNC_E > 0) THEN
        DO I=1,NEL
          IPOS1(I) = NINT(UVAR(I, IVARF + 1))
          IAD1(I)  = NPF(IFUNC_E) / 2 + 1
          ILEN1(I) = NPF(IFUNC_E + 1) / 2 - IAD1(I) - IPOS1(I)
        ENDDO
c
        CALL VINTER2(TF,IAD1,IPOS1,ILEN1,NEL,TEMP,DYDX,FAC_E)
        UVAR(1:NEL,IVARF + 1) = IPOS1(1:NEL)
c
        K(1:NEL)  = KINI*FAC_E(1:NEL)
        G(1:NEL)  = GINI*FAC_E(1:NEL)
        G2(1:NEL) = G(1:NEL)*TWO
        IVARF = IVARF + 1
      ELSE
        K(1:NEL)  = KINI
        G(1:NEL)  = GINI
        G2(1:NEL) = GINI*TWO
      ENDIF
c---
      IF (IFUNC_YLD > 0) THEN
        DO I=1,NEL
          TR = (EPSPXX(I) + EPSPYY(I) + EPSPZZ(I))*THIRD
          E1 = EPSPXX(I) - TR
          E2 = EPSPYY(I) - TR
          E3 = EPSPZZ(I) - TR
          E4 = HALF*EPSPXY(I)
          E5 = HALF*EPSPYZ(I)
          E6 = HALF*EPSPZX(I)
          EPSP = HALF*(E1**2 + E2**2 + E3**2) + E4**2 + E5**2 + E6**2
          EPSP = SQRT(THREE*EPSP)/THREE_HALF
          EPSP = ALPHA*EPSP + (ONE -ALPHA)*EPSD(I)
          EPSD(I) = EPSP
        ENDDO
c
        DO I=1,NEL
          IPOS2(I) = NINT(UVAR(I, IVARF + 1))
          IAD2(I)  = NPF(IFUNC_YLD) / 2 + 1
          ILEN2(I) = NPF(IFUNC_YLD + 1) / 2 - IAD2(I) - IPOS2(I)
        ENDDO
c
        CALL VINTER2(TF,IAD2,IPOS2,ILEN2,NEL,EPSD,DYDX,FRATE)
        UVAR(1:NEL,IVARF + 1) = IPOS2(1:NEL)
      ELSE
         FRATE(1:NEL) = ONE
      ENDIF
      UVAR(1:NEL,2) = EPSD(1:NEL)
c---
      IF (JTHE == 1) THEN
        RA(1:NEL) = RA1*TEMP(1:NEL)**NA + RA2
      ELSE
        RA(1:NEL) = RA1 + RA2
      ENDIF
c------------------------------------------
c     ELASTIC TRIAL INCREMENT
c------------------------------------------
      DO I=1,NEL
        PLA  = EPSPD(I)
        PLA2 = PLA**2
        PP1  = ONE - RC*PLA2
        PP2  = PP1 + TWO
        YLD(I) = SIGY*FRATE(I) + SIGA*(ONE - EXP(-RA(I)*PLA)) 
     .         - SIGB *(ONE - EXP(-RB*PLA))
     .         + SIGR*PLA*THIRD*PP2 / PP1
c
        DEPSV(I) = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I))
        DAV(I) = DEPSV(I)*THIRD
        D1(I)  = DEPSXX(I)-DAV(I)
        D2(I)  = DEPSYY(I)-DAV(I)
        D3(I)  = DEPSZZ(I)-DAV(I)
c       Increments elastiques pression et deviateur        
        DP(I) =-K(I)* DEPSV(I)
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
        SIGNXX(I) = S1(I) - P(I)
        SIGNYY(I) = S2(I) - P(I)
        SIGNZZ(I) = S3(I) - P(I)
        SIGNXY(I) = S4(I)
        SIGNYZ(I) = S5(I)
        SIGNZX(I) = S6(I)
      ENDDO
c------------------------------------------
c     EVALUATION FONCTION CRITERE DE PLASTICITE
c------------------------------------------
c     old criterion
      DO I=1,NEL          
        Q2   = THREE_HALF*(SO1(I)**2+SO2(I)**2+SO3(I)**2)
     .       + THREE*(SO4(I)**2+SO5(I)**2+SO6(I)**2)
        Q(I) = SQRT(Q2)
        FO(I)= Q(I) - MAX(ZERO, PO(I)*TANB + YLD(I))
      ENDDO
c------------------------------------------
      DO I=1,NEL          
        Q2  = THREE_HALF*(S1(I)**2+S2(I)**2+S3(I)**2)
     .      + THREE*(S4(I)**2+S5(I)**2+S6(I)**2)
        Q(I)= SQRT(Q2)
        F(I)= Q(I) - MAX(ZERO, P(I)*TANB + YLD(I))
      ENDDO
c------------------------------------------
c     ITERATION POUR TROUVER LE PT D'INTERSECTION AVEC LE CRITERE
c------------------------------------------
      DO I=1,NEL
        IF (F(I) > TOL) THEN
c
          IF (FO(I) < ZERO) THEN
            X1 = ONE
            X2 = ZERO
            F1 = FO(I)
            F2 = F(I)
            DO ITER=1,NITER
              X   = (X1*F2 - X2*F1) / (F2 - F1)  ! regula falsi
              SY1 = S1(I) - X * DS1(I)
              SY2 = S2(I) - X * DS2(I)    
              SY3 = S3(I) - X * DS3(I)
              SY4 = S4(I) - X * DS4(I)
              SY5 = S5(I) - X * DS5(I)
              SY6 = S6(I) - X * DS6(I)
              PY  = P(I)  - X * DP(I) 
              QY  = THREE_HALF*(SY1**2+SY2**2+SY3**2) + THREE*(SY4**2+SY5**2+SY6**2)
              QY  = SQRT(QY)
              FY  = QY - MAX(ZERO, PY*TANB + YLD(I))
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
c
            S1(I) = SY1
            S2(I) = SY2
            S3(I) = SY3
            S4(I) = SY4
            S5(I) = SY5
            S6(I) = SY6
            P(I)  = PY
            Q(I)  = QY
c           deformations
            DE1(I) = X*D1(I)
            DE2(I) = X*D2(I)
            DE3(I) = X*D3(I)
            DE4(I) = X*DEPSXY(I)
            DE5(I) = X*DEPSYZ(I)
            DE6(I) = X*DEPSZX(I)
            DEPSV(I)=X*DEPSV(I)
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
c     PLASTIC FLOW  : FULL NEWTON ITERATIONS
c---------------------------------
      DO I=1,NEL
        IF (F(I) > ZERO) THEN
          DFDP =-TANB  
          DGDP =-TANP  
          DFDC =-ONE
c
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
          HO = SIGA*RA(I)*EXP(-RA(I)*EPSPD(I)) - SIGB*RB*EXP(-RB*EPSPD(I))
     .       + SIGR*(ONE + TWO_THIRD*RC*PLA2 * PP2 / PP1**2)
          DENOM = THREE*G(I) + K(I)*DFDP*DGDP - DFDC*HO
          IF (DENOM /= ZERO) THEN
            LDOT = F(I) / DENOM
          ELSE
            LDOT = ZERO
          ENDIF
c-----------------------------------------------------------------------
          NITER = 4
          DO ITER=1,NITER
c          
            DEPSPD = LDOT
            DEPSPV = LDOT*DGDP*THIRD
c
            DEPSP1 = LDOT*DFDS1 
            DEPSP2 = LDOT*DFDS2 
            DEPSP3 = LDOT*DFDS3 
            DEPSP4 = LDOT*DFDS4 * TWO
            DEPSP5 = LDOT*DFDS5 * TWO
            DEPSP6 = LDOT*DFDS6 * TWO
c
c           Deviatoric elastic deformations
c
            DEPSL1 = DE1(I) - DEPSP1
            DEPSL2 = DE2(I) - DEPSP2
            DEPSL3 = DE3(I) - DEPSP3
            DEPSL4 = DE4(I) - DEPSP4
            DEPSL5 = DE5(I) - DEPSP5
            DEPSL6 = DE6(I) - DEPSP6
            DEPSLV = DEPSV(I) + DEPSPV
c
c           Stress increment starting from yield surface
c
            DS1(I)= G2(I)*DEPSL1
            DS2(I)= G2(I)*DEPSL2
            DS3(I)= G2(I)*DEPSL3
            DS4(I)= G(I) *DEPSL4
            DS5(I)= G(I) *DEPSL5
            DS6(I)= G(I) *DEPSL6
            DP(I) =-K(I) *DEPSLV
c
c           New stress
c
            S1N(I) = S1(I) + DS1(I)
            S2N(I) = S2(I) + DS2(I)
            S3N(I) = S3(I) + DS3(I)
            S4N(I) = S4(I) + DS4(I)
            S5N(I) = S5(I) + DS5(I)
            S6N(I) = S6(I) + DS6(I)
            PN(I)  = P(I)  + DP(I)
c
c           Update yield and plastic increment
c           
            Q2   = THREE_HALF*(S1N(I)**2 + S2N(I)**2 + S3N(I)**2)
     .           + THREE*(S4N(I)**2 + S5N(I)**2 + S6N(I)**2)
            QN(I)= SQRT(Q2)
c
            DPLA = LDOT*KEP
            PLA  = EPSPD(I) + DPLA
            PLA2 = PLA**2
            PP1  = ONE - RC*PLA2
            PP2  = PP1 + TWO
            YLD(I) = SIGY*FRATE(I) + SIGA*(ONE - EXP(-RA(I)*PLA))
     .             - SIGB *(ONE - EXP(-RB*PLA))
     .             + SIGR*PLA*THIRD*PP2 / PP1
            YLD(I) = MAX(YLD(I), ZERO)

c           H = SIGA*RA(I)*EXP(-RA(I)*EPSPD(I)) - SIGB*RB*EXP(-RB*EPSPD(I))
c    .        + SIGR*(ONE + TWO_THIRD*RC*PLA2 * PP2 / PP1**2)
c            HM = (HO + H) * HALF
            HM = HO

            RES  = QN(I) - MAX(ZERO, PN(I)*TANB + YLD(I))
            DRES = -(THREE*G(I) + K(I)*DFDP*DGDP - DFDC*HM)
            
            IF (DRES /= ZERO) THEN
              LDOT = LDOT - RES / DRES
            ENDIF
c
          ENDDO !  ITER=1,NITER
c-----------------------------------------------------------------------
c         CHECK REPROJECTION
c
          FY = QN(I) - MAX(ZERO, PN(I)*TANB + YLD(I))
c
          IF (FY > SMALL) THEN
            IF (TANB*PN(I) + YLD(I) <= ZERO) THEN
              PN(I)  =-YLD(I)/TANB
              S1N(I) = ZERO
              S2N(I) = ZERO
              S3N(I) = ZERO
              S4N(I) = ZERO
              S5N(I) = ZERO
              S6N(I) = ZERO
c              WRITE(6,*)'TRI-TRACTION FAILURE'
            ELSE 
              IF (QN(I) > SMALL) THEN
                X = (PN(I)*TANB + YLD(I)) / QN(I)
                IF (X < ONE-EM02 .OR. X > ONE) THEN
                  WRITE(7,*)'REPROJ Q',X,FY,PN(I),QN(I)
                  WRITE(6,*)'REPROJ Q',X,FY,PN(I),QN(I)
                ENDIF
                S1N(I) = X*S1N(I)
                S2N(I) = X*S2N(I)
                S3N(I) = X*S3N(I)
                S4N(I) = X*S4N(I)
                S5N(I) = X*S5N(I)
                S6N(I) = X*S6N(I)
              ENDIF
            ENDIF
          ENDIF
          EPSPD(I) = EPSPD(I) + LDOT*KEP
          EPSPV(I) = EPSPV(I) + LDOT*DGDP
c
c         Final stress update
c
          SIGNXX(I) = S1N(I) - PN(I)
          SIGNYY(I) = S2N(I) - PN(I)
          SIGNZZ(I) = S3N(I) - PN(I)
          SIGNXY(I) = S4N(I)
          SIGNYZ(I) = S5N(I)
          SIGNZX(I) = S6N(I)
c
        ENDIF   ! F > 0       
c
        UVAR(I,1)= YLD(I)
c
      ENDDO     ! I=1,NEL
c----------------------------------------------
c     END OF PLASTICITY PROJECTION LOOP
c----------------------------------------------
      DO I=1,NEL
        SOUNDSP(I) = SQRT((K(I) + FOUR_OVER_3*G(I))/RHO(I))
      ENDDO
c-----------
      RETURN
      END
