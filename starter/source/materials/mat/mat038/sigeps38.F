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
Chd|  SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        CHECKAXES                     source/materials/mat/mat038/sigeps38.F
Chd|        DREH                          source/materials/tools/matrix.F
Chd|        JACOBIEW                      source/materials/tools/matrix.F
Chd|        VALPVEC                       source/materials/tools/matrix.F
Chd|        VALPVECDP                     source/materials/tools/matrix.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS38(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VOLUME , EINT   ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX, 
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF   ,
     B      UVARBID, ISMSTR , MFXX    , MFXY  , MFXZ  , MFYX ,    
     C      MFYY   , MFYZ   , MFZX    , MFZY  , MFZZ  )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "scr05_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL, NUPARAM, NUVAR,ISMSTR
      my_real
     .      UVARBID
      my_real
     .      TIME       , TIMESTEP   , UPARAM(NUPARAM),
     .      RHO   (NEL), RHO0  (NEL), VOLUME(NEL), EINT(NEL),
     .      EPSPXX(NEL), EPSPYY(NEL), EPSPZZ(NEL),
     .      EPSPXY(NEL), EPSPYZ(NEL), EPSPZX(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),
     .      MFXX(NEL)  ,   MFXY(NEL),   MFXZ(NEL),
     .      MFYX(NEL)  ,   MFYY(NEL),   MFYZ(NEL),
     .      MFZX(NEL)  ,   MFZY(NEL),   MFZZ(NEL)    
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), SIGVZZ(NEL),
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SOUNDSP(NEL), VISCMAX(NEL), 
     .      DEN1,DEN2,DEN3,DEN1D,DEN2D,
     .      DEN3D,OLD1,OLD2,OLD3,OLD4,OLD5,OLD6
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     . UVAR(NEL,NUVAR), OFF(NEL) 
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real
     .   FINTER,TF(*)
      EXTERNAL FINTER
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER          MFUNC,IUNLOAD
      INTEGER          NUPARAM0
      INTEGER          I,J,L,N,M,MDIR,NROT
      INTEGER          K(MVSIZ,3),K1(MVSIZ,3),KF(MVSIZ,3)
      INTEGER          KF1(MVSIZ,3),KUN(MVSIZ,3)
      INTEGER          IFLAG,ITOTAL,IMSTA
      INTEGER          NFUNC1,NFUNCUL,NFUNCP,NPCURVE,KCOMPAIR
      INTEGER          KRECOVER,KDECAY
      LOGICAL          TOTAL,UNCHANGED,JACOBI,UNLOADING
      INTEGER          IND1,LD
      INTEGER          II,JJ,KEN
C     REAL ou REAL*8
      my_real
     .   STRAIN(MVSIZ,3),RATE(MVSIZ,3),RATEM(MVSIZ),
     .   VISC(MVSIZ,3),
     .   DSIGMA(MVSIZ,3),DECAY,TENSIONCUT,
     .   AMIN,AMAX,TOLERANCE,LAMDA,EFINAL,EPSFIN,EFACTOR,
     .   A(3,3),SN(3,3),EARJ(3),
     .   PSC(MVSIZ,3),EAR(MVSIZ,3),
     .   EBR(MVSIZ,3),ECR(MVSIZ,3),
     .   PSN(MVSIZ,3),EAN(MVSIZ,3),
     .   EBN(MVSIZ,3),ECN(MVSIZ,3),
     .   EL(MVSIZ,3),
     .   DPRA(3,3),DPRAO(3,3),DPRAD(3,3),
     .   AV(6,MVSIZ),EARV(3,MVSIZ),DIRPRV(3,3,MVSIZ),
     .   SIGPRV(3,MVSIZ),EI(MVSIZ),
     .   E0,VT,VC,RV,KKK,GGG,
     .   BETA,HYSTER,RATEDAMP,THETA,
     .   P0,RELAXP,MAXPRES,PHI,GAMMA,
     .   PAIR(MVSIZ),VOLUMER(MVSIZ),
     .   DF,DF1,DF2,
     .   VISCOSITY,
     .   ALPHA(5),EDOT(5),
     .   PSN1(MVSIZ,3),PSN2(MVSIZ,3),
     .   EDOT0(MVSIZ,3),EDOTS(MVSIZ,3),
     .   EDOTL(MVSIZ,3),EDOTU(MVSIZ,3),
     .   EXPONAS,EXPONBS,FUNLOAD,RUNLOAD,
     .   E12(MVSIZ),E23(MVSIZ),E31(MVSIZ),
     .   A11(MVSIZ),A12(MVSIZ),A13(MVSIZ),A22(MVSIZ),
     .   A23(MVSIZ),A33(MVSIZ),DETC(MVSIZ),
     .   V12(MVSIZ),V23(MVSIZ),V31(MVSIZ),
     .   D11(MVSIZ),D12(MVSIZ),D44(MVSIZ),
     .   EMAX(MVSIZ),EYN(MVSIZ,3),
     .   HUGE,SMALL,TINY,SHIFT,PSCALE   
c     .  ESECANT(MVSIZ,3),ETANGENT(MVSIZ,3),EELASTIC(MVSIZ,3),
      my_real 
     .   AR1(6),DAR1(6),AR2(6),DAR2(6),DAR3(3),EARJD(3),
     .   TMP0,TMP1,TMP2,TMP3,TMP4,TMP5,TMP6,AD(3,3),
     .   AA(MVSIZ),SIGMAX(MVSIZ),ESECANT(MVSIZ,3),
     .   TMP0P,TMP1P,TMP2P,TMP3P,TMP4P,TMP5P,TMP6P, DTI
C=======================================================================
      HUGE = EP30
      SMALL= EM3
      TINY = EM30   
C......................................................................
C SET INITIAL MATERIAL CONSTANTS

      NUPARAM0= UPARAM(1)

      E0        = UPARAM(2)
      VT        = UPARAM(3)
      VC        = UPARAM(4)
      RV        = UPARAM(5)
      IFLAG     = UPARAM(6)
      ITOTAL    = UPARAM(7)

      BETA      = UPARAM(8)
      HYSTER    = UPARAM(9)
      RATEDAMP  = UPARAM(10)
      KRECOVER  = UPARAM(11)
      KDECAY    = UPARAM(12)
      THETA     = UPARAM(13)
c      THETA     = RATEDAMP

      KCOMPAIR  = UPARAM(14)
      P0        = UPARAM(15)
      GAMMA     = UPARAM(16)
      RELAXP    = UPARAM(17)
      MAXPRES   = UPARAM(18)
      PHI       = UPARAM(19)

      IUNLOAD   = UPARAM(20)
      FUNLOAD   = UPARAM(21)
      RUNLOAD   = UPARAM(22)
      EXPONAS   = UPARAM(23)
      EXPONBS   = UPARAM(24)

      MFUNC     = UPARAM(25)
      IMSTA     = UPARAM(26)
      TENSIONCUT= UPARAM(27)

      EFINAL    = UPARAM(28)
      EPSFIN    = UPARAM(29)
      LAMDA     = UPARAM(30)
      VISCOSITY = UPARAM(31)
      TOLERANCE = UPARAM(32)
      PSCALE    = UPARAM(33)
      NFUNC1=(NFUNC-2)/2
* unloading function number
      NFUNCUL=NFUNC-1
* function number for enclosed air pressure
      NFUNCP=NFUNC

* set strain rates and scale factors for curves
      DO I=1,NFUNC1
        EDOT(I) =UPARAM(NUPARAM-NFUNC1+I)
        ALPHA(I)=UPARAM(NUPARAM-NFUNC1*2+I)
      ENDDO

* initialize e-module
      TOTAL=.TRUE.
C ITOTAL=0 ; TOTAL - LINEAR IN TENSION
C ITOTAL=1 ; TOTAL - NONLINEAR IN TENSION
C ITOTAL=2 ; INCREMENTAL - LINEAR IN TENSION
C ITOTAL=3 ; INCREMENTAL - NONLINEAR IN TENSION
      IF(ITOTAL.EQ.2.OR.ITOTAL.EQ.3) TOTAL=.FALSE.
C +++ ([F]=[M_F]+[1]) -----
      IF(ISMSTR>=10) THEN
C--------- [B]=[F][F]^t strain-----
c      DO I=1,NEL
c       EPSXX(I)=MFXX(I)*(TWO+MFXX(I))+
c     .          MFXY(I)*MFXY(I)+MFXZ(I)*MFXZ(I)
c       EPSYY(I)=MFYY(I)*(TWO+MFYY(I))+
c     .          MFYX(I)*MFYX(I)+MFYZ(I)*MFYZ(I)
c       EPSZZ(I)=MFZZ(I)*(TWO+MFZZ(I))+
c     .          MFZX(I)*MFZX(I)+MFZY(I)*MFZY(I)
c       EPSXY(I)=TWO*(MFXY(I)+MFYX(I)+MFXX(I)*MFYX(I)+
c     .               MFXY(I)*MFYY(I)+MFXZ(I)*MFYZ(I))
c       EPSZX(I)=TWO*(MFXZ(I)+MFZX(I)+MFXX(I)*MFZX(I)+
c     .               MFXY(I)*MFZY(I)+MFXZ(I)*MFZZ(I))
c       EPSYZ(I)=TWO*(MFZY(I)+MFYZ(I)+MFZX(I)*MFYX(I)+
c     .               MFZY(I)*MFYY(I)+MFZZ(I)*MFYZ(I))
c      ENDDO
      TOTAL=.TRUE.
      ENDIF
       
C......................................................................
C INITIALIZE
      DO I=1,NEL
        EI(I)=E0
        DO J=1,3
         VISC(I,J)=ZERO
        ENDDO
C        IF(TIME.EQ.ZERO) THEN
C          EYN(I,1) = E0
C          EYN(I,2) = E0
C          EYN(I,3) = E0
C STRAIN(1-3),STRESS(4-6),STRAIN RATE(7-9)
C          DO J=1,9
C            UVAR(I,J)=ZERO
C          ENDDO
C MODULE (EYN INITIALIZED TO E0)
C          DO J=10,12
C            UVAR(I,J)=E0
C          ENDDO
C POISSON'S RATIO/MODULE
C          DO J=13,15
C            UVAR(I,J)=VT/E0
C          ENDDO
C PRESSURE
C          UVAR(I,16)=ZERO
C PRINCIPAL DIRECTIONS
C          DO J=17,25
C            UVAR(I,J)=ZERO
C          ENDDO
C          UVAR(I,17)=ONE
C          UVAR(I,21)=ONE
C          UVAR(I,25)=ONE
C LAW STORAGE FOR DECAY AND HYSTERESIS
C          DO J=26,31
C            UVAR(I,J)=ZERO
C          ENDDO
C        ENDIF
      ENDDO

      IF(IFLAG.EQ.2)THEN
       JACOBI=.TRUE.
       IFLAG=0
      ELSE
       JACOBI=.FALSE.
      ENDIF
C......................................................................

C DEFINE STRETCH TENSOR


      IF(IFLAG.EQ.0)THEN
C PRINCIPAL STRAINS FORMULATION
        MDIR=3
        DO I=1,NEL
          IF(TOTAL) THEN
            AV(1,I)=EPSXX(I)
            AV(2,I)=EPSYY(I)
            AV(3,I)=EPSZZ(I)
            AV(4,I)=EPSXY(I)*HALF
            AV(5,I)=EPSYZ(I)*HALF
            AV(6,I)=EPSZX(I)*HALF    
          ELSE
            AV(1,I)=DEPSXX(I)
            AV(2,I)=DEPSYY(I)
            AV(3,I)=DEPSZZ(I)
            AV(4,I)=DEPSXY(I)*HALF
            AV(5,I)=DEPSYZ(I)*HALF
            AV(6,I)=DEPSZX(I)*HALF        
          ENDIF
        ENDDO
        IF(JACOBI) THEN
          DO I=1,NEL
            A(1,1)=AV(1,I)
            A(2,2)=AV(2,I)
            A(3,3)=AV(3,I)
            A(2,1)=AV(4,I)
            A(3,2)=AV(5,I)
            A(3,1)=AV(6,I)
            CALL JACOBIEW(A,3,EARJ,DPRA,NROT)
            EARV(1,I)=EARJ(1)
            EARV(2,I)=EARJ(2)
            EARV(3,I)=EARJ(3)
            DO N=1,3
              DO J=1,3
                DIRPRV(N,J,I)=DPRA(N,J)
              ENDDO
            ENDDO
          ENDDO
        ELSE
C           Eigenvalues needed to be calcultated in double precision
C           for a simple precision executing
          IF (IRESP.EQ.1) THEN
            CALL VALPVECDP(AV,EARV,DIRPRV,NEL)
          ELSE
            CALL VALPVEC(AV,EARV,DIRPRV,NEL)
          ENDIF
        ENDIF
C SET OLD PRINCIPAL DIRECTIONS
        DO I=1,NEL
          M=0
          DO N=1,3
            DO L=1,3
              M=M+1
              DPRAO(L,N)=UVAR(I,16+M)
            ENDDO
          ENDDO
          M=0
          DO N=1,3
            DO J=1,3
              M=M+1
              DPRA(N,J)=DIRPRV(N,J,I)
            ENDDO
          ENDDO

C COMPARE NEW AND OLD PRINCIPAL DIRECTIONS AND TRANSFORM STORED 
C VALUES FROM OLD INTO NEW PRINCIPAL DIRECTIONS IF TOLERANCE EXCEEDED

          CALL CHECKAXES(DPRAO,DPRA,AMIN,AMAX,UNCHANGED,TOLERANCE)
          IF(.NOT.UNCHANGED) THEN
            DO M=1,4
              DO  N=1,3
                DO  L=1,3
                  SN(N,L)=ZERO
                  IF(N.EQ.L) SN(N,L)= UVAR(I,N+3*(M-1))
                ENDDO
              ENDDO
C ROTATE STORED VALUES IN OLD PRINCIPAL DIRECTIONS INTO GLOBAL DIRECTIONS
              CALL DREH(SN,DPRAO,1,1,1)
C ROTATE STORED VALUES IN GLOBAL DIRECTIONS INTO NEW PRINCIPAL DIRECTIONS 
              CALL DREH(SN,DPRA ,1,1,0)
              DO N=1,3
                UVAR(I,N+3*(M-1)) = SN(N,N)
              ENDDO
            ENDDO
C           pour la partie decay et hysteresis
            IF (BETA.GT.TINY) THEN
            DO M=1,2
              DO  N=1,3
                DO  L=1,3
                  SN(N,L)=ZERO
                  IF(N.EQ.L) SN(N,L)= UVAR(I,N+3*(M-1)+25)
                ENDDO
              ENDDO
              CALL DREH(SN,DPRAO ,1,1,1)
              CALL DREH(SN,DPRA ,1,1,0)
              DO N=1,3
                UVAR(I,N+3*(M-1)+25) = SN(N,N)
              ENDDO
            ENDDO
            ENDIF 
            UVAR(I,17)=DPRA(1,1)
            UVAR(I,18)=DPRA(2,1)
            UVAR(I,19)=DPRA(3,1)
            UVAR(I,20)=DPRA(1,2)
            UVAR(I,21)=DPRA(2,2)
            UVAR(I,22)=DPRA(3,2)
            UVAR(I,23)=DPRA(1,3)
            UVAR(I,24)=DPRA(2,3)
            UVAR(I,25)=DPRA(3,3)
          ENDIF

          IF(TOTAL) THEN
            EAR(I,1)=EARV(1,I)
            EAR(I,2)=EARV(2,I)
            EAR(I,3)=EARV(3,I)
          ELSE
            ECR(I,1)=EARV(1,I)
            ECR(I,2)=EARV(2,I)
            ECR(I,3)=EARV(3,I)
          ENDIF
        ENDDO
      ELSE

C AVERAGE STRAIN FORMULATION

        MDIR=1
        DO I=1,NEL
          IF(TOTAL) THEN
            EAR(I,1)= SIGN(ONE,EPSXX(I)+EPSYY(I)+EPSZZ(I))*SQRT(
     &      (EPSXX(I)-EPSYY(I))**TWO+
     &      (EPSYY(I)-EPSZZ(I))**TWO+
     &      (EPSZZ(I)-EPSXX(I))**TWO+
     &      TWO*(EPSXY(I)**TWO+EPSYZ(I)**TWO+EPSZX(I)**TWO))/SQRT(TWO)
          ELSE 
            ECR(I,1)= SIGN(ONE,DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))*SQRT(
     &      (DEPSXX(I)-DEPSYY(I))**TWO+
     &      (DEPSYY(I)-DEPSZZ(I))**TWO+
     &      (DEPSZZ(I)-DEPSXX(I))**TWO+
     &      ((DEPSXY(I))**TWO+(DEPSYZ(I))**TWO+(DEPSZX(I))**TWO))
     &      /SQRT(TWO)             
          ENDIF
        ENDDO

      ENDIF

C......................................................................
          IF(TIMESTEP.LE.ZERO) THEN
           DTI=ZERO
          ELSE
           DTI=ONE/TIMESTEP
          ENDIF

      DO J=1,MDIR
        DO I=1,NEL
          IF(     TOTAL) ECR(I,J)=EAR(I,J)-UVAR(I,J)
          IF(.NOT.TOTAL) EAR(I,J)=ECR(I,J)+UVAR(I,J)
          EBR(I,J)=ECR(I,J)*DTI
        ENDDO
C COMPUTE NOMINAL VALUES
        DO I=1,NEL
C COMPUTE PRINCIPAL STRETCHES
C LAMDA=E_NOMINAL+1=EXP(E_RADIOSS)
        IF(ISMSTR.EQ.0.OR.ISMSTR.EQ.2.OR.ISMSTR.EQ.4) THEN
          EL(I,J)=EXP(EAR(I,J))
C COMPUTE PRINCIPAL NOMINAL STRAIN(J) RATES FROM PRINCIPAL STRAIN(J) RATES
C E_DOT_NOMINAL=EDOT_RADIOSS*EXP(E_RADIOSS)=EDOT_RADIOSS*LAMDA
          EBN(I,J)=EBR(I,J)*EL(I,J)
          ECN(I,J)=EL(I,J)*(ONE-EXP(-ECR(I,J)))
        ELSEIF(ISMSTR.EQ.10.OR.ISMSTR.EQ.12) THEN
          EL(I,J)=SQRT(EAR(I,J)+ONE)
          EBN(I,J)=EBR(I,J)
          ECN(I,J)=ECR(I,J)
        ELSE
          EL(I,J)=EAR(I,J)+ONE
          EBN(I,J)=EBR(I,J)
          ECN(I,J)=ECR(I,J)
        ENDIF
C COMPUTE PRINCIPAL NOMINAL STRAINS FROM PRINCIPAL STRAINS
          EAN(I,J)=EL(I,J)-ONE
C STRAIN RATE FILTERING
          EBN(I,J)=UVAR(I,J+6)+(EBN(I,J)-UVAR(I,J+6))*RATEDAMP
       ENDDO
      ENDDO

C......................................................................

C RELATIVE VOLUME 
        DO I=1,NEL
          PAIR(I)=ZERO
          VOLUMER(I)=EL(I,1)*EL(I,2)*EL(I,3)
        ENDDO

C CONFINED AIR PRESSURE
        DO I=1,NEL
          IF (KCOMPAIR.EQ.1.AND.VOLUMER(I).LT.ONE) THEN
            NPCURVE=IFUNC(NFUNCP)
            IF (VOLUMER(I)-PHI.GT.SMALL) THEN
              IF(NPCURVE.NE.0) THEN
                 PAIR(I)=-FINTER(NPCURVE,VOLUMER(I),NPF,TF,DF)
                 PAIR(I)=PAIR(I)*PSCALE
              ELSE
                PAIR(I)=P0*(VOLUMER(I)-ONE)/(VOLUMER(I)-PHI)
              ENDIF
            ELSE
              PAIR(I)=UVAR(I,16)
            ENDIF
C RELAXATION ON AIR PRESSURE
            PAIR(I)=EXP(-RELAXP*TIME)*MAX(PAIR(I),-MAXPRES)
            UVAR(I,16)=PAIR(I)
          ELSEIF (KCOMPAIR.EQ.2.AND.VOLUMER(I).LT.ONE) THEN
            NPCURVE=IFUNC(NFUNCP)
            PAIR(I)=-FINTER(NPCURVE,VOLUMER(I),NPF,TF,DF)
          ENDIF
        ENDDO

C......................................................................

C UPDATE TENSILE MODULE (INCREASES WITH COMPRESSION)

C COMPUTE STRESS COMPONENTS VIA INTERPOLATION

C LOOP ON PRINCIPAL DIRECTIONS
      DO J=1,MDIR
        DO I=1,NEL
          PSN(I,J)=ZERO
C COMPRESSION IS POSITIVE!
          STRAIN(I,J)=-EAN(I,J)
          RATE(I,J)=ABS(EBN(I,J))
          SHIFT=ZERO
          UNLOADING=.FALSE.
          IF((EAN(I,J).LT.ZERO.AND.EBN(I,J).GT.ZERO).OR.
     &    (EAN(I,J).GT.ZERO.AND.EBN(I,J).LT.ZERO))UNLOADING=.TRUE.                        
C COMPUTE ELASTIC MODULE FROM STATIC CURVE
            PSN1(I,J)=-ALPHA(1)*FINTER(IFUNC(1),STRAIN(I,J),NPF,TF,DF)
C CURVE INTERPOLATION
C EDOT0 = ACTUAL RATE
C EDOTU = UNLOADING RATE
C EDOTL = UPPER BOUND RATE
C EDOTS = LOWERBOUND RATE
          EDOT0(I,J)=RATE(I,J)
          IF(EDOT0(I,J).LT.EDOT(1)) EDOT0(I,J)=EDOT(1)
          IF(UNLOADING.AND.IUNLOAD.NE.0) THEN
C CASE OF IUNLOAD.NE.0 (UNLOAD BETWEEN THE IUNLOAD CURVE (UPPER LIMIT)
C AND THE FIRST CURVE (LOWER LIMIT)- STATIC- )
           IF (ABS(RUNLOAD-EDOT(1)).LT.EM20) THEN
            PSN(I,J)=-FUNLOAD*
     &      (FINTER(IFUNC(NFUNCUL),STRAIN(I,J),NPF,TF,DF1))
            EYN(I,J)=FUNLOAD*DF1
            VISC(I,J)=ZERO
           ELSE
            EDOTU(I,J)=EDOT0(I,J)
            EDOTS(I,J)=EDOT(1)
            EDOTL(I,J)=MAX(RUNLOAD,EDOTS(I,J))

            IF(EDOTU(I,J).LT.EDOTS(I,J))EDOTU(I,J)=EDOTS(I,J)
            IF(EDOTU(I,J).GT.EDOTL(I,J))EDOTU(I,J)=EDOTL(I,J)

            PSN1(I,J)=-ALPHA(1)*
     &      FINTER(IFUNC(1),STRAIN(I,J),NPF,TF,DF1)
            EYN(I,J)=ALPHA(1)*DF1
            PSN2(I,J)=-FUNLOAD*
     &      (FINTER(IFUNC(NFUNCUL),STRAIN(I,J),NPF,TF,DF2))

             PSN(I,J)=PSN2(I,J)+(PSN1(I,J)-PSN2(I,J))*
     &      (ONE-((MIN(ONE,(EDOTU(I,J)-EDOTS(I,J))/
     &      (MAX(TINY,EDOTL(I,J)-EDOTS(I,J)))))**EXPONAS))**EXPONBS

            IF(ABS(EDOTU(I,J)-EDOTS(I,J)).GT.TINY.AND.
     &      ABS(PSN(I,J)-PSN1(I,J)).GT.TINY)VISC(I,J)=
     &      ABS((PSN(I,J)-PSN1(I,J))/(EDOTU(I,J)-EDOTS(I,J)))

            VISC(I,J)=MIN(VISC(I,J),VISCOSITY)
            PSN(I,J)=PSN1(I,J)-VISC(I,J)*(EDOTU(I,J)-EDOTS(I,J))
           ENDIF 

          ELSE
            KUN(I,J)=0
            IF(UNLOADING) KUN(I,J)=NFUNC1
C            IF(UNLOADING) SHIFT=TWO*(SIGN(1,STRAIN(I,J))-STRAIN(I,J))
C CASE OF ONLY ONE CURVE INPUT I.E. NO STRAIN(J) RATE EFFECT LOADING
C AND UNLOADING FOLLOW THE SAME PATH
            IF (NFUNC1.EQ.1) THEN
              PSN(I,J)=-ALPHA(1)*
     &        FINTER(IFUNC(KUN(I,J)+1),STRAIN(I,J)+SHIFT,NPF,TF,DF)
              EYN(I,J)=ALPHA(1)*DF
            ELSE
C CASE OF IUNLOAD.EQ.0 ( UNLOAD BETWEEN THE IUNLOAD CURVE (UPPER LIMIT)
C AND THE FIRST CURVE (LOWER LIMIT )- STATIC- 
C IF UNLOADING CURVES ARE SPECIFIED THEY WILL BE USED FOR INTERPOLATION
C OTHERWISE STARTER INITILIZES THEM TO THE DEFAULT CURVE NUMBER I.E. THE
C FIRST LOADING (STATIC) CURVE
               DO L=1,NFUNC1
                IF(EDOT0(I,J).LE.EDOT(L)) GOTO 10
              ENDDO
              L=NFUNC1
              EDOT0(I,J)=EDOT(L)
 10           CONTINUE
              K(I,J)=L
              KF(I,J)=L+KUN(I,J)

              PSN2(I,J)=-ALPHA(K(I,J))*
     &        FINTER(IFUNC(KF(I,J)),STRAIN(I,J)+SHIFT,NPF,TF,DF2)
              K1(I,J)=MAX(1,L-1)
              KF1(I,J)=MAX(1,L-1)+KUN(I,J)
              EDOTL(I,J)=EDOT(K(I,J))
              EDOTS(I,J)=EDOT(K1(I,J))
              PSN1(I,J)=-ALPHA(K1(I,J))*
     &        FINTER(IFUNC(KF1(I,J)),STRAIN(I,J)+SHIFT,NPF,TF,DF1)
              IF(L.EQ.NFUNC1) THEN
               EYN(I,J)=ALPHA(L)*DF2
              ELSE
               EYN(I,J)=ALPHA(K1(I,J))*DF1
              ENDIF

              PSN(I,J)=PSN2(I,J)+(PSN1(I,J)-PSN2(I,J))*
     &        (ONE-((MIN(ONE,(EDOT0(I,J)-EDOTS(I,J))/
     &        (MAX(TINY,EDOTL(I,J)-EDOTS(I,J)))))**EXPONAS))
     &        **EXPONBS

              IF(ABS(EDOT0(I,J)-EDOTS(I,J)).GT.TINY.AND.
     &        ABS(PSN(I,J)-PSN1(I,J)).GT.TINY)VISC(I,J)=
     &        ABS((PSN(I,J)-PSN1(I,J))/(EDOT0(I,J)-EDOTS(I,J)))

              VISC(I,J)=MIN(VISC(I,J),VISCOSITY)
              PSN(I,J)=PSN1(I,J)-VISC(I,J)*(EDOT0(I,J)-EDOTS(I,J))
            ENDIF
          ENDIF

        ENDDO
        
C......................................................................

C UNLOADING

        DO I=1,NEL
          DECAY = UVAR(I,J+28)
C HYSTERESIS AND DECAY OCCURS ON COMPRESSION ONLY BUT REMAINS PERMANENT
C IN TENSION
          IF(EAN(I,J).LT.ZERO) THEN
C NO RECOVERY ON UNLOADING - ACCUMULATE STRAIN
            IF(KRECOVER.EQ.0.AND.ECN(I,J).LT.ZERO)
     &      UVAR(I,J+25)=UVAR(I,J+25)+ABS(ECN(I,J))
C RECOVERY ON UNLOADING - DOWN INCREMENT STRAIN
            IF(KRECOVER.EQ.1)UVAR(I,J+25)=UVAR(I,J+25)-ECN(I,J)
            IF(EBN(I,J).LT.ZERO) THEN
              DECAY = MIN(ONE,HYSTER*(ONE-EXP(-BETA*UVAR(I,J+25))))    
            ENDIF          
          ENDIF  
C DECAY ON LOADING AND UNLOADING
          IF(KDECAY.EQ.0)
     &    PSN(I,J)=PSN(I,J)*(ONE-DECAY)
C DECAY ON LOADING ONLY
          IF(KDECAY.EQ.1.AND.ECN(I,J).LT.ZERO)
     &    PSN(I,J)=PSN(I,J)*(ONE-DECAY)
C DECAY ON UNLOADING ONLY
          IF(KDECAY.EQ.2.AND.ECN(I,J).GT.ZERO)
     &    PSN(I,J)=PSN(I,J)*(ONE-DECAY)
          UVAR(I,J+28)=DECAY
C......................................................................
          DSIGMA(I,J)=PSN(I,J)-UVAR(I,J+3)
          IF(TOTAL) THEN
C SECANT MODULE
            TMP1 = EYN(I,J)
          ELSE
           IF(ABS(ECN(I,J)).GT.TINY)THEN
              EYN(I,J)=ABS(DSIGMA(I,J)/ECN(I,J))
            ELSE
              EYN(I,J)=UVAR(I,J+9)
            ENDIF
          ENDIF
        ENDDO

        IF(.NOT.TOTAL) THEN
        DO I=1,NEL
          IF(SIGN(ONE,(EBN(I,J)/(UVAR(I,J+6)+TINY))).NE.ONE) 
     &    EYN(I,J)=UVAR(I,J+9)
        ENDDO

        DO I=1,NEL
          EYN(I,J)=EYN(I,J)*THETA+UVAR(I,J+9)*(ONE-THETA)
        ENDDO
        ENDIF


        IF(ITOTAL.EQ.0.OR.ITOTAL.EQ.2) THEN
C LINEAR IN TENSION
          DO I=1,NEL
            IF(EAN(I,J).GE.ZERO) THEN
              UVAR(I,J+28)=ZERO
              TMP1=exp(-lamda*(VOLUMER(I)-1.+EPSFIN))
              EI(I)=EFINAL+(E0-EFINAL)*(1-TMP1)
              TMP2=LAMDA*(EFINAL-E0)*TMP1
              EYN(I,J)=MAX(EI(I),TMP2)
              IF(TOTAL) THEN
                PSN(I,J)=EI(I)*EAN(I,J)
              ELSE
                PSN(I,J)=UVAR(I,J+3)+EI(I)*ECN(I,J)
              ENDIF
              UVAR(I,J+12)=VT/EI(I)
            ENDIF
          ENDDO
Cqz option cachee +++
        ELSEIF(ITOTAL.EQ.-1) THEN
C MODULE MAXMAL IN TENSION ONE CYCLE LATER (TOTAL)
          DO I=1,NEL
            IF(EAN(I,J).GT.0.) THEN
              UVAR(I,J+28)=ZERO
              EI(I)=MAX(UVAR(I,10),UVAR(I,11),UVAR(I,12))
              EYN(I,J)=EI(I)
              PSN(I,J)=EI(I)*EAN(I,J)
              UVAR(I,J+12)=VT/EI(I)
            ENDIF
          ENDDO
        ELSEIF(ITOTAL.EQ.-2) THEN
C MODULE MAXMAL IN TENSION ONE CYCLE LATER (INCR)
          DO I=1,NEL
            IF(EAN(I,J).GT.ZERO) THEN
              UVAR(I,J+28)=ZERO
              EI(I)=MAX(UVAR(I,10),UVAR(I,11),UVAR(I,12))
              EYN(I,J)=EI(I)
              PSN(I,J)=UVAR(I,J+3)+EI(I)*ECN(I,J)
              UVAR(I,J+12)=VT/EI(I)
            ENDIF
          ENDDO
Cqz option cachee ---
        ENDIF

C END OF LOOP ON MDIR
      ENDDO
C BEGIN OF LOOP ON MDIR
      IF (IMSTA.GE.1) THEN
       DO I=1,NEL
         SIGMAX(I)=-(MIN(PSN(I,1),PSN(I,2),PSN(I,3))-
     .          MAX(PSN(I,1),PSN(I,2),PSN(I,3)))
       ENDDO
       DO J=1,MDIR
        DO I=1,NEL
         ESECANT(I,J)=0.4*ABS(PSN(I,J))/MAX(TINY,ABS(STRAIN(I,J)))
         IF (ESECANT(I,J).LE.SIGMAX(I)) THEN
          TMP1=0.2*(SIGMAX(I)-ESECANT(I,J))
C calcul de PSN pose pb en P/ON sous Solaris => compilation en 03
          PSN(I,J)=PSN(I,J)+TMP1*EAN(I,J)
          EYN(I,J)=MAX(EYN(I,J),(ONE+TMP1)*ESECANT(I,J))
         ENDIF 
        ENDDO
       ENDDO
      ENDIF 
      DO J=1,MDIR
        DO I=1,NEL
          TMP0=MAX(EYN(I,J),UVAR(I,J+9))
          UVAR(I,J  )=EAR(I,J)
          UVAR(I,J+3)=PSN(I,J)
          UVAR(I,J+6)=EBN(I,J)
          UVAR(I,J+9)=EYN(I,J)
          EYN(I,J)=TMP0
        ENDDO
C END OF LOOP ON MDIR
      ENDDO

      IF(IFLAG.EQ.1) THEN
C OCTAHEDRAL STRAIN (AVERAGE) FORMULATION
        DO I=1,NEL
           EMAX(I)=EI(I)
          D11(I)=EMAX(I)*(ONE-VT)/(ONE+VT)/(ONE-TWO*VT)
          D12(I)=EMAX(I)*VT/(ONE+VT)/(ONE-TWO*VT)
          D44(I)=EMAX(I)/TWO/(ONE+VT)
        ENDDO
        DO I=1,NEL
          IF(TOTAL) THEN
            SIGNXX(I)=D11(I)*EPSXX(I)+D12(I)*EPSYY(I)+D12(I)*EPSZZ(I)
            SIGNYY(I)=D12(I)*EPSXX(I)+D11(I)*EPSYY(I)+D12(I)*EPSZZ(I)
            SIGNZZ(I)=D12(I)*EPSXX(I)+D12(I)*EPSYY(I)+D11(I)*EPSZZ(I)
            SIGNXY(I)=D44(I)*EPSXY(I)
            SIGNYZ(I)=D44(I)*EPSYZ(I)
            SIGNZX(I)=D44(I)*EPSZX(I)
            SOUNDSP(I) = SQRT(D11(I)/RHO0(I))
          ELSE
            SIGNXX(I)=SIGOXX(I)+
     &      D11(I)*DEPSXX(I)+D12(I)*DEPSYY(I)+D12(I)*DEPSZZ(I)
            SIGNYY(I)=SIGOYY(I)+
     &      D12(I)*DEPSXX(I)+D11(I)*DEPSYY(I)+D12(I)*DEPSZZ(I)
            SIGNZZ(I)=SIGOZZ(I)+
     &      D12(I)*DEPSXX(I)+D12(I)*DEPSYY(I)+D11(I)*DEPSZZ(I)
            SIGNXY(I)=SIGOXY(I)+D44(I)*DEPSXY(I)
            SIGNYZ(I)=SIGOYZ(I)+D44(I)*DEPSYZ(I)
            SIGNZX(I)=SIGOZX(I)+D44(I)*DEPSZX(I)
            SOUNDSP(I) = SQRT(D11(I)/RHO0(I))
          ENDIF
          VISCMAX(I)=VISC(I,1)

        ENDDO
        RETURN
      ENDIF

      DO I=1,NEL
        IF(VT+VC.LE.TWO*TINY) THEN
          IF(TOTAL) THEN
            PSC(I,1)=PSN(I,1)
            PSC(I,2)=PSN(I,2)
            PSC(I,3)=PSN(I,3)
          ELSE
            PSC(I,1)=EYN(I,1)*ECN(I,1)
            PSC(I,2)=EYN(I,2)*ECN(I,2)
            PSC(I,3)=EYN(I,3)*ECN(I,3)
          ENDIF
        ELSE
          E12(I)=(EAN(I,1)+EAN(I,2))/TWO
          E23(I)=(EAN(I,2)+EAN(I,3))/TWO
          E31(I)=(EAN(I,3)+EAN(I,1))/TWO    
          V12(I)=VC+(VT-VC)*(ONE-EXP(-RV*ABS(E12(I))))*
     &    (SIGN(ONE,E12(I))+ONE)/TWO
          V23(I)=VC+(VT-VC)*(ONE-EXP(-RV*ABS(E23(I))))*
     &    (SIGN(ONE,E23(I))+ONE)/TWO
          V31(I)=VC+(VT-VC)*(ONE-EXP(-RV*ABS(E31(I))))*
     &    (SIGN(ONE,E31(I))+ONE)/TWO
          IF(TOTAL) THEN
            DETC(I)=ONE-V23(I)*V23(I)-V31(I)*V31(I)-V12(I)*V12(I)-
     &      TWO*V12(I)*V31(I)*V23(I)
            A11(I)=(ONE-V23(I)*V23(I))
            A12(I)=(V12(I)+V23(I)*V31(I))
            A13(I)=(V31(I)+V23(I)*V12(I))
            A22(I)=(ONE-V31(I)*V31(I))
            A23(I)=(V23(I)+V31(I)*V12(I))
            A33(I)=(ONE-V12(I)*V12(I))
            PSC(I,1)=(A11(I)*PSN(I,1)+A12(I)*PSN(I,2)+A13(I)*PSN(I,3))/
     &      DETC(I)
            PSC(I,2)=(A12(I)*PSN(I,1)+A22(I)*PSN(I,2)+A23(I)*PSN(I,3))/
     &      DETC(I)
            PSC(I,3)=(A13(I)*PSN(I,1)+A23(I)*PSN(I,2)+A33(I)*PSN(I,3))/
     &      DETC(I)
          ELSE
            UVAR(I,13)=THETA*V23(I)/EI(I)+(ONE-THETA)*UVAR(I,13)
            UVAR(I,14)=THETA*V31(I)/EI(I)+(ONE-THETA)*UVAR(I,14)
            UVAR(I,15)=THETA*V12(I)/EI(I)+(ONE-THETA)*UVAR(I,15)
            DETC(I)=ONE/(EYN(I,1)*EYN(I,2)*EYN(I,3))-
     &      UVAR(I,13)*UVAR(I,13)/EYN(I,1)-
     &      UVAR(I,14)*UVAR(I,14)/EYN(I,2)-
     &      UVAR(I,15)*UVAR(I,15)/EYN(I,3)-
     &      2*UVAR(I,13)*UVAR(I,14)*UVAR(I,15)
            A11(I)=ONE/(EYN(I,2)*EYN(I,3))-UVAR(I,13)*UVAR(I,13)
            A12(I)=UVAR(I,15)/EYN(I,3)+UVAR(I,13)*UVAR(I,14)
            A13(I)=UVAR(I,14)/EYN(I,2)+UVAR(I,13)*UVAR(I,15)
            A22(I)=ONE/(EYN(I,1)*EYN(I,3))-UVAR(I,14)*UVAR(I,14)
            A23(I)=UVAR(I,13)/EYN(I,1)+UVAR(I,14)*UVAR(I,15)
            A33(I)=ONE/(EYN(I,1)*EYN(I,2))-UVAR(I,15)*UVAR(I,15)
C COMPUTE STRESS INCREMENT IN PRINCIPAL DIRECTIONS
            PSC(I,1)=((A11(I)*ECN(I,1)+A12(I)*ECN(I,2)+A13(I)*ECN(I,3))
     &      /DETC(I))
            PSC(I,2)=((A12(I)*ECN(I,1)+A22(I)*ECN(I,2)+A23(I)*ECN(I,3))
     &      /DETC(I))
            PSC(I,3)=((A13(I)*ECN(I,1)+A23(I)*ECN(I,2)+A33(I)*ECN(I,3))
     &      /DETC(I))
          ENDIF
        ENDIF

        DO J=1,3
          IF(OFF(I).EQ.ZERO.OR.PSN(I,J).GT.ABS(TENSIONCUT))THEN
            PSC(I,1)=ZERO
            PSC(I,2)=ZERO
            PSC(I,3)=ZERO
            OFF(I)=ZERO
          ENDIF
        ENDDO
C COMPUTE CAUCHY STRESS INCREMENT IN PRINCIPAL DIRECTIONS
CSIGMA_CAUCHY(I) = LAMDA(I) * SIGMA_NOMINAL(I) / RELATIVE VOLUME
        IF(ISMSTR.EQ.0.OR.ISMSTR.EQ.2.OR.ISMSTR.EQ.4) THEN
C CAUCHY MODULE FOR TIME STEP
          DEN1=EL(I,2)*EL(I,3)
          DEN2=EL(I,3)*EL(I,1)
          DEN3=EL(I,1)*EL(I,2)
          PSC(I,1)=PSC(I,1)/DEN1
          PSC(I,2)=PSC(I,2)/DEN2
          PSC(I,3)=PSC(I,3)/DEN3
          EYN(I,1)=EYN(I,1)/DEN1
          EYN(I,2)=EYN(I,2)/DEN2
          EYN(I,3)=EYN(I,3)/DEN3
        ELSEIF(ISMSTR.EQ.10.OR.ISMSTR.EQ.12) THEN
C------------directly to Chauchy-------
          DEN1=EL(I,1)/VOLUMER(I)
          DEN2=EL(I,2)/VOLUMER(I)
          DEN3=EL(I,3)/VOLUMER(I)
          PSC(I,1)=PSC(I,1)*DEN1
          PSC(I,2)=PSC(I,2)*DEN2
          PSC(I,3)=PSC(I,3)*DEN3
          EYN(I,1)=EYN(I,1)*DEN1
          EYN(I,2)=EYN(I,2)*DEN2
          EYN(I,3)=EYN(I,3)*DEN3
        ENDIF
      ENDDO
       IF (KCOMPAIR.EQ.2) THEN
        DO I=1,NEL
          TMP0=VOLUMER(I)
          TMP3=MIN(EL(I,1),EL(I,2),EL(I,3))
          IF (TMP0.LT.ONE.AND.TMP3.LT.ONE
     &     .AND.TMP3.GT.TMP0.AND.ABS(TMP0-TMP3).GT.EM6) THEN
            TMP2=VOLUMER(I)**(1./3.)-VOLUMER(I)
            TMP4=(TMP3-TMP0)/TMP2
            AA(I)=MIN(ONE,TMP4)
          ELSE
            AA(I)=ZERO
          ENDIF 
        ENDDO
        DO J=1,3
         DO I=1,NEL
          SIGPRV(J,I)=PSC(I,J)+AA(I)*(PAIR(I)-PSC(I,J))
         ENDDO 
        ENDDO
       ELSE
        DO J=1,3
         DO I=1,NEL
          SIGPRV(J,I)=PSC(I,J)+PAIR(I)
         ENDDO
        ENDDO
       ENDIF 
C TRANSFORM FROM PRINCIPAL TO GLOBAL DIRECTIONS
      DO I=1,NEL
        SIGNXX(I) = DIRPRV(1,1,I)*DIRPRV(1,1,I)*SIGPRV(1,I)
     &            + DIRPRV(1,2,I)*DIRPRV(1,2,I)*SIGPRV(2,I)
     &            + DIRPRV(1,3,I)*DIRPRV(1,3,I)*SIGPRV(3,I)
        SIGNYY(I) = DIRPRV(2,2,I)*DIRPRV(2,2,I)*SIGPRV(2,I)
     &            + DIRPRV(2,3,I)*DIRPRV(2,3,I)*SIGPRV(3,I)
     &            + DIRPRV(2,1,I)*DIRPRV(2,1,I)*SIGPRV(1,I)
        SIGNZZ(I) = DIRPRV(3,3,I)*DIRPRV(3,3,I)*SIGPRV(3,I)
     &            + DIRPRV(3,1,I)*DIRPRV(3,1,I)*SIGPRV(1,I)
     &            + DIRPRV(3,2,I)*DIRPRV(3,2,I)*SIGPRV(2,I)
        SIGNXY(I) = DIRPRV(1,1,I)*DIRPRV(2,1,I)*SIGPRV(1,I)
     &            + DIRPRV(1,2,I)*DIRPRV(2,2,I)*SIGPRV(2,I)
     &            + DIRPRV(1,3,I)*DIRPRV(2,3,I)*SIGPRV(3,I)
        SIGNYZ(I) = DIRPRV(2,2,I)*DIRPRV(3,2,I)*SIGPRV(2,I)
     &            + DIRPRV(2,3,I)*DIRPRV(3,3,I)*SIGPRV(3,I)
     &            + DIRPRV(2,1,I)*DIRPRV(3,1,I)*SIGPRV(1,I)
        SIGNZX(I) = DIRPRV(3,3,I)*DIRPRV(1,3,I)*SIGPRV(3,I)
     &            + DIRPRV(3,1,I)*DIRPRV(1,1,I)*SIGPRV(1,I)
     &            + DIRPRV(3,2,I)*DIRPRV(1,2,I)*SIGPRV(2,I)
      ENDDO

      IF(.NOT.TOTAL) THEN
        DO I=1,NEL
C ADD INCREMENT OF STRESS
          SIGNXX(I)=SIGNXX(I)+SIGOXX(I)
          SIGNYY(I)=SIGNYY(I)+SIGOYY(I)
          SIGNZZ(I)=SIGNZZ(I)+SIGOZZ(I)
          SIGNXY(I)=SIGNXY(I)+SIGOXY(I)
          SIGNYZ(I)=SIGNYZ(I)+SIGOYZ(I)
          SIGNZX(I)=SIGNZX(I)+SIGOZX(I)
        ENDDO
      ENDIF
C SOUNDSPEED
      IF(IFLAG.EQ.0) THEN
       DO I=1,NEL
        EMAX(I)=MAX(EI(I),EYN(I,1),EYN(I,2),EYN(I,3))
       ENDDO
      ENDIF

Cqz option cachee +++ 
      IF (IMSTA.EQ.2) THEN
       DO I=1,NEL
        EPSXY(I) = DIRPRV(1,1,I)*DIRPRV(2,1,I)*EAN(I,1)
     &            + DIRPRV(1,2,I)*DIRPRV(2,2,I)*EAN(I,2)
     &            + DIRPRV(1,3,I)*DIRPRV(2,3,I)*EAN(I,3)
        EPSYZ(I) = DIRPRV(2,2,I)*DIRPRV(3,2,I)*EAN(I,2)
     &            + DIRPRV(2,3,I)*DIRPRV(3,3,I)*EAN(I,3)
     &            + DIRPRV(2,1,I)*DIRPRV(3,1,I)*EAN(I,1)
        EPSZX(I) = DIRPRV(3,3,I)*DIRPRV(1,3,I)*EAN(I,3)
     &            + DIRPRV(3,1,I)*DIRPRV(1,1,I)*EAN(I,1)
     &            + DIRPRV(3,2,I)*DIRPRV(1,2,I)*EAN(I,2)
       ENDDO
       DO I=1,NEL
        ESECANT(I,1)=0.5*ABS(SIGNXY(I))/MAX(TINY,ABS(EPSXY(I)))
        ESECANT(I,2)=0.5*ABS(SIGNYZ(I))/MAX(TINY,ABS(EPSYZ(I)))
        ESECANT(I,3)=0.5*ABS(SIGNZX(I))/MAX(TINY,ABS(EPSZX(I)))
        SIGMAX(I)=MAX(0.5*EI(I),SIGMAX(I))
         IF (ESECANT(I,1).LE.SIGMAX(I)) THEN
          TMP1=0.1*(SIGMAX(I)-ESECANT(I,1))
          SIGNXY(I)=SIGNXY(I)+TMP1*EPSXY(I)
         ENDIF 
         IF (ESECANT(I,2).LE.SIGMAX(I)) THEN
          TMP1=0.1*(SIGMAX(I)-ESECANT(I,2))
          SIGNYZ(I)=SIGNYZ(I)+TMP1*EPSYZ(I)
         ENDIF 
         IF (ESECANT(I,3).LE.SIGMAX(I)) THEN
          TMP1=0.1*(SIGMAX(I)-ESECANT(I,3))
          SIGNZX(I)=SIGNZX(I)+TMP1*EPSZX(I)
         ENDIF 
       ENDDO
      ENDIF 
Cqz option cachee ---
      DO I=1,NEL
        KKK=EMAX(I)/(THREE*(ONE-TWO*MAX(VC,VT)))
        GGG=EMAX(I)/(TWO*(ONE+MAX(VC,VT)))
        SOUNDSP(I)=SQRT((KKK+FOUR_OVER_3*GGG)/RHO0(I))
        VISCMAX(I)=MAX(VISC(I,1),VISC(I,2),VISC(I,3))
      ENDDO

      RETURN

      END

Chd|====================================================================
Chd|  CHECKAXES                     source/materials/mat/mat038/sigeps38.F
Chd|-- called by -----------
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHECKAXES(TRAN1,TRAN2,AMIN,AMAX,UNCHANGED,TOL)
C----------------------------------------------------------------
      IMPLICIT NONE
#include      "constant.inc"   
      my_real
     . TRAN1(3,3),TRAN2(3,3),ANG(3),AMAX,TOL,AMIN
      INTEGER I,J,K
      LOGICAL UNCHANGED
C----------------------------------------------------------------
      AMAX=ZERO
      UNCHANGED=.FALSE.
      DO J=1,3
        ANG(J)=ZERO
        DO K=1,3
          ANG(J)=ANG(J)+TRAN1(K,J)*TRAN2(K,J)
        ENDDO
        ANG(J)=ABS(ABS(ANG(J))-ONE)
        IF (ANG(J).GT.AMAX) AMAX=ANG(J)
      ENDDO
      IF (AMAX.LT.TOL) UNCHANGED=.TRUE.
      AMIN=ANG(1)
      RETURN
      END
