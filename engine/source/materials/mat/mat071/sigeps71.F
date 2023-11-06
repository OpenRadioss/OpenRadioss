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
Chd|  SIGEPS71                      source/materials/mat/mat071/sigeps71.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        VALPVECDP_V                   source/materials/mat/mat033/sigeps33.F
Chd|        VALPVEC_V                     source/materials/mat/mat033/sigeps33.F
Chd|====================================================================
      SUBROUTINE SIGEPS71(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NGL     ,IPM    ,
     B     MAT    ,JTHE   ,TEMPEL  ,ISMSTR  ,ETSE)
C-----------------------------------------------
c  Law for SMA (Shape memory alloy - NiTinol)
c  based on Auricchio 1997
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY
C TF      |  *      | F | R | FUNCTION ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
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
C SIG0XX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX
C SIG0YY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
C
        INTEGER NEL, NUPARAM, NUVAR,IPT,JTHE,
     .     NGL(NEL),MAT(NEL),IPLA,IPM(NPROPMI,*)
        my_real
     .     TIME,TIMESTEP,UPARAM(*),
     .     RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .     EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .     EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .     DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .     DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .     EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .     EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .     SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .     SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL), TEMPEL(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
        my_real
     .      SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .      SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .      SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .      SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .      SOUNDSP(NEL),ETSE(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
        my_real
     .          UVAR(NEL,NUVAR), OFF(NEL),  PLA(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION
C-----------------------------------------------
        INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
        my_real
     .     FINTER2, TF(*),FINTER
        EXTERNAL FINTER2,FINTER
C   EXTERNAL FINTER, FINTER2
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
        INTEGER I,J,J1,J2,I1,I2,KK,IADBUF,EFLAG,ISMSTR,IFUNC(100)
        my_real
     .      E,EMART,NU,G,G2,WAVE,SQDT,A,B,C,FCT,FCTP, DFTR,UNMXN,DB,
     .      ALPHA,EPSL,AA1,FM,DFMSA,DFMAS,UXX,NN,BETA,GM,KM,
     .      CB,CC,CAAS,CBAS,POLD,DGT,DKT,CP,TINI,INVE, n1,n2,n3,
     .      K,P,SXX,CAS,CSA,TSAS,TFAS, TSSA,TFSA,RV_PUI,DFS,
     .      SYY,SZZ,SXY,SYZ,SZX,FASS,FSAS,FASF,FSAF,RSAS,RFAS,
     .      SV,FS,FS0,YLD_ASS,YLD_ASF,YLD_SAS,YLD_SAF,RSSA,RFSA,
     .      DFM, FSS,DSXX,DSYY,DSXY,DSYZ,DSZX,DSZZ,VAR,H,ET,
     .      PM,DELTA,X1,X2,test,test2,ftest,gnew,knew,BETAn,
     .      NX2,NY2 ,NZ2,NXY2,NYZ2,NZX2,NE,DNX,DNY,DNZ,DNXY,DNYZ,DNZX,
     .      NXX(NEL),NYY(NEL),NZZ(NEL),NXY(NEL),NYZ(NEL),NZX(NEL),
     .      E1(NEL),E2(NEL),E3(NEL),E4(NEL),E5(NEL),E6(NEL),TRDE(NEL),
     .      DE1(NEL),DE2(NEL),DE3(NEL),GT(NEL),KT(NEL),TEMP(NEL),
     .      EE1(NEL),EE2(NEL),EE3(NEL),PP(NEL),NNE(NEL),DET(NEL),
     .      SIGXX(NEL), SIGYY(NEL), SIGZZ(NEL)
        my_real
     .              EV(MVSIZ,3)
        my_real
     .              AV(MVSIZ,6),EVV(MVSIZ,3),DIRPRV(MVSIZ,3,3)

C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------

        E         = UPARAM(1)
        NU        = UPARAM(2)
        G         = UPARAM(3)
        K         = UPARAM(4)
        AA1       = UPARAM(5)
        YLD_ASS   = UPARAM(6)
        YLD_ASF   = UPARAM(7)
        YLD_SAS   = UPARAM(8)
        YLD_SAF   = UPARAM(9)
        ALPHA     = UPARAM(10)
        EPSL      = UPARAM(11)/(SQRT(TWO_THIRD)+ALPHA)
        EMART     = UPARAM(14)
        EFLAG     = INT(UPARAM(15))
        GM        = UPARAM(16)
        KM        = UPARAM(17)
        G2        = TWO*G
        BETA      = EPSL*(G2+NINE*K*ALPHA*ALPHA)
        SQDT      = SQRT(TWO/THREE)
        CAS       = UPARAM(18)
        CSA       = UPARAM(19)
        TSAS      = UPARAM(20)
        TFAS      = UPARAM(21)
        TSSA      = UPARAM(22)
        TFSA      = UPARAM(23)
        CP        = UPARAM(24)
        TINI      = UPARAM(25)
c
C
        DO I=1,NEL
          AV(I,1)=EPSXX(I)
          AV(I,2)=EPSYY(I)
          AV(I,3)=EPSZZ(I)
          AV(I,4)=EPSXY(I) * HALF
          AV(I,5)=EPSYZ(I) * HALF
          AV(I,6)=EPSZX(I) * HALF
        ENDDO
C     Eigenvalues needed to be calculated in double precision
C     for a simple precision executing
        IF (IRESP==1) THEN
          CALL VALPVECDP_V(AV,EVV,DIRPRV,NEL)
        ELSE
          CALL VALPVEC_V(AV,EVV,DIRPRV,NEL)
        ENDIF

        IF(ISMSTR== 0.OR. ISMSTR==2.OR. ISMSTR==4) THEN
          DO I=1,NEL
C ---- (STRAIN IS LOGARITHMIC)
            EV(I,1)=EXP(EVV(I,1))
            EV(I,2)=EXP(EVV(I,2))
            EV(I,3)=EXP(EVV(I,3))
          ENDDO
        ELSEIF(ISMSTR==10.OR.ISMSTR==12) THEN
          DO I =1,NEL
            EV(I,1)=SQRT(EVV(I,1)+ ONE)! = lambda
            EV(I,2)=SQRT(EVV(I,2)+ ONE)
            EV(I,3)=SQRT(EVV(I,3)+ ONE)
          ENDDO
        ELSE
C ----  STRAIN IS ENGINEERING)
          DO I=1,NEL
            EV(I,1)=EVV(I,1)+ ONE
            EV(I,2)=EVV(I,2)+ ONE
            EV(I,3)=EVV(I,3)+ ONE
          ENDDO
        ENDIF
        DO I =1,NEL
          !DET A (A = GRADIENT OF DEFORMATION)
          DET(I) =EV(I,1)*EV(I,2)*EV(I,3)
          IF(DET(I)/=ZERO) THEN
            TRDE(I) = LOG(DET(I)) !TRACE OF DEFORMATION
            RV_PUI = EXP((-THIRD)*TRDE(I))
          ELSE
            RV_PUI = ZERO
            TRDE(I)= ZERO
          ENDIF
          EE1(I)  = LOG(EV(I,1)*RV_PUI) !DEVIATOR OF DEFORMATION
          EE2(I)  = LOG(EV(I,2)*RV_PUI)
          EE3(I)  = LOG(EV(I,3)*RV_PUI)
        ENDDO
C-----------------------------------------------
C  Calcul de la tempurature. (conduction ou adiabatique)
C--------------------
        IF(JTHE < 0 ) THEN
          DO I=1,NEL
            TEMP(I) = TEMPEL(I)
          ENDDO
        ELSE
          DO I=1,NEL
            TEMP(I) = TINI
     .              + EINT(I)/ RHO0(I)/CP/MAX(EM15,VOLUME(I))
          ENDDO
        ENDIF
C-----------------------------------------------

        IF (EFLAG > ZERO)THEN
C=======================================================================
          RSAS = YLD_ASS *(SQDT+ALPHA)-CAS*TSAS
          RFAS = YLD_ASF *(SQDT+ALPHA)-CAS*TFAS
          RSSA = YLD_SAS *(SQDT+ALPHA)-CSA*TSSA
          RFSA = YLD_SAF *(SQDT+ALPHA)-CSA*TFSA
          DO I = 1,NEL
C
            FM = UVAR(I,1)   ! fraction of Martensite
            GT(I) = G + FM * (GM - G) !G_n
            KT(I) = K + FM * (KM - K) !K_n
            !pressure
            P = KT(I) * (TRDE(I) - THREE*ALPHA*EPSL*FM)
            ! n= e/ norm(e)
            NE = SQRT( EE1(I)**2 + EE2(I)**2 + EE3(I)**2)
            NXX(I) =EE1(I)/MAX(NE,EM20)
            NYY(I) =EE2(I)/MAX(NE,EM20)
            NZZ(I) =EE3(I)/MAX(NE,EM20)
!       Estimation dev(sigma_n+1)
            SXX= TWO*GT(I)*(EE1(I) -EPSL*FM*NXX(I))
            SYY= TWO*GT(I)*(EE2(I) -EPSL*FM*NYY(I))
            SZZ= TWO*GT(I)*(EE3(I) -EPSL*FM*NZZ(I))

            SV =  SQRT( SXX*SXX + SYY*SYY + SZZ*SZZ  )
c       sound velocity
            SOUNDSP(I) = SQRT(AA1/RHO0(I))

            VISCMAX(I) = ZERO
C-------------------
C transformation
C-------------------
            DFMSA = ZERO
            DFMAS = ZERO
c       loading function
            FS = SV + THREE*ALPHA*P  - CAS*TEMP(I)
C----------------------
C   Check Austenite -----> martensite
            FASS = FS -  RSAS
            FASF = FS -  RFAS
            FS0 = UVAR(I,2)
            BETA      = EPSL*(TWO*GT(I)+NINE*KT(I)*ALPHA*ALPHA)
            IF((FS - FS0) > ZERO .AND. FASS > ZERO.AND. FM <= ONE ) THEN
              ! DFMAS = MIN(ONE, -(FS-FS0)*(ONE-FM)/(FASF-BETA*(ONE-FM) )  ) ! (should be positive)
              DB    = (TWO * (GM-G) +NINE*ALPHA*ALPHA*(KM-K)) *EPSL
              UNMXN = ONE - FM
              DFTR  = TWO*NE*(GM-G) + THREE*ALPHA*TRDE(I)*(KM-K)
              DFMAS = MIN(ONE, -(FS-FS0)*(ONE-FM)/(FASF-BETA*(ONE-FM) )  )
              A = UNMXN *DB
              B = (RFAS-FS+UNMXN*(BETA-DFTR))
              C =  UNMXN*(FS0 - FS)
              DO KK = 1,3
                FCT  = DFMAS*DFMAS *A+ DFMAS*   B  +C
                FCTP = TWO*DFMAS *A+ B
                DFMAS = DFMAS - FCT / FCTP
              ENDDO
              DFMAS = MIN(ONE,DFMAS  ) ! (should be positive)
            ENDIF
C----------------------
C   Check marteniste -----> austenite
c      Unloading function
            FS = SV + THREE*ALPHA*P  - CSA*TEMP(I)
            FSAS = FS -RSSA
            FSAF = FS -RFSA
            FS0 = UVAR(I,3)

            IF((FS - FS0) < ZERO .AND. FSAS < ZERO .AND. FM >ZERO)THEN
              !DFMSA =  MAX(-ONE , FM * (FS - FS0)/ (FSAF+BETA*FM) )
              DB    = (TWO * (GM-G) +NINE*ALPHA*ALPHA*(KM-K))*EPSL
              DFTR  = TWO * (GM-G)*NE+ THREE*ALPHA*(KM-K)*TRDE(I)
              DFMSA = ZERO
              A = FM *DB
              B = -(RFSA-FS+FM*(DFTR-BETA))
              C =  -FM*(FS - FS0)
              DO KK = 1,3
                FCT  = DFMSA*DFMSA *A+ DFMSA*   B  +C
                FCTP = TWO*DFMSA *A+ B
                DFMSA = DFMSA - FCT / FCTP
              ENDDO
              DFMSA =  MAX(-ONE , DFMSA )
            ENDIF
C--------------------------------
C      new martensite fraction
            DFM =  DFMAS + DFMSA
            IF(DFM < ZERO .AND. FM == ZERO) DFM = ZERO
C----------------------
            !UPDATE
C----------------------

            DGT = DFM * (GM - G)
            DKT = DFM * (KM - K)

            SXX =  SXX -TWO*GT(I)      *   EPSL*NXX(I)*DFM
     .                + TWO*DGT* (EE1(I)-EPSL*NXX(I)*DFM)
            SYY =  SYY -TWO*GT(I)* EPSL*DFM*NYY(I)
     .                + TWO*DGT* (EE2(I)-EPSL*NYY(I)*DFM)
            SZZ =  SZZ -TWO*GT(I)* EPSL*DFM*NZZ(I)
     .                + TWO*DGT* (EE3(I)-EPSL*NZZ(I)*DFM)

            P = P - KT(I)*EPSL*THREE*ALPHA*DFM
     .            + DKT *(TRDE(I) -EPSL*THREE*ALPHA*DFM)
            !KIRCHHOFF STRESS
            SIGXX(I)= SXX + P
            SIGYY(I)= SYY + P
            SIGZZ(I)= SZZ + P
            SV =  SQRT( SXX*SXX + SYY*SYY + SZZ*SZZ )
            FS = SV + THREE*ALPHA*P
            !CAUCHY STRESS
            IF(DET(I)/=zero)THEN
              INVE = ONE/DET(I)
            ELSE
              INVE = zero
            ENDIF
            SIGXX(I)= SIGXX(I) *INVE
            SIGYY(I)= SIGYY(I) *INVE
            SIGZZ(I)= SIGZZ(I) *INVE
C      TRANSFORM PRINCIPAL  CAUCHY STRESSES TO GLOBAL DIRECTIONS
            SIGNXX(I) =  DIRPRV(I,1,1)*DIRPRV(I,1,1)*SIGXX(I)
     .                + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SIGYY(I)
     .                + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SIGZZ(I)
            SIGNYY(I) =  DIRPRV(I,2,2)*DIRPRV(I,2,2)*SIGYY(I)
     .               + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SIGZZ(I)
     .               + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SIGXX(I)
            SIGNZZ(I) =  DIRPRV(I,3,3)*DIRPRV(I,3,3)*SIGZZ(I)
     .               + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SIGXX(I)
     .               + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SIGYY(I)
            SIGNXY(I) =  DIRPRV(I,1,1)*DIRPRV(I,2,1)*SIGXX(I)
     .               + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SIGYY(I)
     .               + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SIGZZ(I)
            SIGNYZ(I) =  DIRPRV(I,2,2)*DIRPRV(I,3,2)*SIGYY(I)
     .               + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SIGZZ(I)
     .               + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SIGXX(I)
            SIGNZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*SIGZZ(I)
     .               + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SIGXX(I)
     .               + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SIGYY(I)
c
            UVAR(I,1) = UVAR(I,1) + DFM
            UVAR(I,1) = MAX(ZERO, UVAR(I,1))
            UVAR(I,1) = MIN(ONE, UVAR(I,1))
            UVAR(I,2) = FS- CAS*TEMP(I)
            UVAR(I,3) = FS- CSA*TEMP(I)
            IF (DFMAS /= ZERO) DFS     = ABS(UVAR(I,2) - FS0)
            IF (DFMSA /= ZERO) DFS     = ABS(UVAR(I,3) - FS0)
            IF (DFS   /= ZERO .AND. EPSL /= ZERO .AND. DFM/= ZERO) THEN
              H       = DFS/EPSL/DFM
              ETSE(I) = H *(ONE+NU) /( (E + UVAR(I,1)*(EMART-E))  + H)
            ELSE
              ETSE(I) = ONE
            ENDIF
            UVAR(I,10) = EPSXX(I)
            UVAR(I,11) = temp(I)
          ENDDO
C=======================================================================
        ELSE !EFLAG = 0
          RSAS = YLD_ASS *(SQDT+ALPHA)-CAS*TSAS
          RFAS = YLD_ASF *(SQDT+ALPHA)-CAS*TFAS
          RSSA = YLD_SAS *(SQDT+ALPHA)-CSA*TSSA
          RFSA = YLD_SAF *(SQDT+ALPHA)-CSA*TFSA
          DO I = 1,NEL
C
            FM = UVAR(I,1)   ! fraction of Martensite
            !pressure
            P = K * (TRDE(I) - THREE*ALPHA*EPSL*FM)
            ! n= e/ norm(e)
            NE = SQRT( EE1(I)**2 + EE2(I)**2 + EE3(I)**2)
            NXX(I) =EE1(I)/MAX(NE,EM20)
            NYY(I) =EE2(I)/MAX(NE,EM20)
            NZZ(I) =EE3(I)/MAX(NE,EM20)
!       Estimation dev(sigma_n+1)
            SXX= G2*(EE1(I) -EPSL*FM*NXX(I))
            SYY= G2*(EE2(I) -EPSL*FM*NYY(I))
            SZZ= G2*(EE3(I) -EPSL*FM*NZZ(I))

            SV =  SQRT( SXX*SXX + SYY*SYY + SZZ*SZZ  )
c       sound velocity
            SOUNDSP(I) = SQRT(AA1/RHO0(I))
            VISCMAX(I) = ZERO
C-------------------
C transformation
C-------------------
            DFMSA = ZERO
            DFMAS = ZERO

c loading function
            FS = SV + THREE*ALPHA*P - CAS*TEMP(I) ! loading function A--> M
            !FSS = FS - YLD_ASS
C----------------------
C   Check Austenite -----> martensite
            FASS = FS -RSAS
            FASF = FS -RFAS
            FS0 = UVAR(I,2)
            IF((FS - FS0) > ZERO .AND. FASS > ZERO.AND. FM < ONE ) THEN!
              DFMAS = MIN(ONE, -(FS-FS0)*(ONE-FM)/(FASF-BETA*(ONE-FM) )  ) ! (should be positive)
            ENDIF
C----------------------
!   Check marteniste -----> austenite
            FS = SV + THREE*ALPHA*P - CSA*TEMP(I) ! Unloading function M--> A
            FSAS = FS - RSSA
            FSAF = FS - RFSA
            FS0 = UVAR(I,3)
            IF((FS - FS0) < ZERO .AND. FSAS < ZERO  .AND. FM > ZERO) THEN !
              DFMSA =  MAX(-ONE , FM * (FS - FS0)/ (FSAF+BETA*FM) )  !compute marteniste fraction
            ENDIF
C----------------------
!      new martensite fraction
            DFM =  DFMAS + DFMSA
            IF(DFM < ZERO .AND. FM == ZERO) DFM = ZERO
C----------------------
            !UPDATE
            SXX =  SXX -G2* EPSL*DFM*NXX(I)
            SYY =  SYY -G2* EPSL*DFM*NYY(I)
            SZZ =  SZZ -G2* EPSL*DFM*NZZ(I)
            P = P - K*EPSL*THREE*ALPHA*DFM
            !KIRCHHOFF STRESS
            SIGXX(I)= SXX + P
            SIGYY(I)= SYY + P
            SIGZZ(I)= SZZ + P
            SV =  SQRT( SXX*SXX + SYY*SYY + SZZ*SZZ )
            FS = SV + THREE*ALPHA*P
            !CAUCHY STRESS
            IF(DET(I)/=zero)THEN
              INVE = ONE/DET(I)
            ELSE
              INVE = zero
            ENDIF
            SIGXX(I)= SIGXX(I) *INVE
            SIGYY(I)= SIGYY(I) *INVE
            SIGZZ(I)= SIGZZ(I) *INVE
C      TRANSFORM PRINCIPAL  CAUCHY STRESSES TO GLOBAL DIRECTIONS
            SIGNXX(I) =  DIRPRV(I,1,1)*DIRPRV(I,1,1)*SIGXX(I)
     .                + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SIGYY(I)
     .                + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SIGZZ(I)
            SIGNYY(I) =  DIRPRV(I,2,2)*DIRPRV(I,2,2)*SIGYY(I)
     .               + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SIGZZ(I)
     .               + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SIGXX(I)
            SIGNZZ(I) =  DIRPRV(I,3,3)*DIRPRV(I,3,3)*SIGZZ(I)
     .               + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SIGXX(I)
     .               + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SIGYY(I)
            SIGNXY(I) =  DIRPRV(I,1,1)*DIRPRV(I,2,1)*SIGXX(I)
     .               + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SIGYY(I)
     .               + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SIGZZ(I)
            SIGNYZ(I) =  DIRPRV(I,2,2)*DIRPRV(I,3,2)*SIGYY(I)
     .               + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SIGZZ(I)
     .               + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SIGXX(I)
            SIGNZX(I) =  DIRPRV(I,3,3)*DIRPRV(I,1,3)*SIGZZ(I)
     .               + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SIGXX(I)
     .               + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SIGYY(I)
            UVAR(I,1) = UVAR(I,1) + DFM
            UVAR(I,1) = MAX(ZERO, UVAR(I,1))
            UVAR(I,1) = MIN(ONE, UVAR(I,1))
            UVAR(I,2) = FS- CAS*TEMP(I)
            UVAR(I,3) = FS- CSA*TEMP(I)
            DFS = ZERO
            IF (DFMAS /= ZERO) DFS     = ABS(UVAR(I,2) - FS0)
            IF (DFMSA /= ZERO) DFS     = ABS(UVAR(I,3) - FS0)
            IF (DFS /= ZERO .AND. EPSL /= ZERO.AND.DFM/= ZERO) THEN
              H       = DFS/EPSL/DFM
              ETSE(I) = H * (ONE+NU)/(E + H)
            ELSE
              ETSE(I) = ONE
            ENDIF
            UVAR(I,4) = EPSL*DFM ! transformation strain
            UVAR(I,7) = UVAR(I,7)+EPSL*DFM
            UVAR(I,10) = EPSXX(I)
            UVAR(I,11) = TEMP(I)
          ENDDO
C
        ENDIF ! EFLAG
        RETURN
      END
C
