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
Chd|  SIGEPS57C                     source/materials/mat/mat057/sigeps57c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        DCPPM_UPC                     source/materials/mat/mat057/sigeps57c.F
Chd|        VINTER                        source/tools/curve/vinter.F
Chd|        FINTER                        source/tools/curve/finter.F
Chd|====================================================================
      SUBROUTINE SIGEPS57C(
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC  ,
     2     NPF    ,NPT    ,IPT     ,IFLAG   ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0   ,
     3     AREA   ,EINT   ,THKLY   ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,THK     ,PLA     ,UVAR   ,
     B     OFF    ,NGL    ,SHF     ,ETSE    ,SIGY   ,
     C     HARDM  ,SEQ_OUTPUT,ISRATE,EPSP   ,INLOC  ,
     D     DPLANL )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "com01_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
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
C NPT     |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER
C IFLAG   |  *      | I | R | GEOMETRICAL FLAGS
C TF      |  *      | F | R | FUNCTION ARRAY
C NGL     | NEL     | I | R | ELEMENT NUMBER
C SHF     | NEL     | F | R | SHEAR FACTOR
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C AREA    | NEL     | F | R | AREA
C EINT    | 2*NEL   | F | R | INTERNAL ENERGY(MEMBRANE,BENDING)
C THKLY   | NEL     | F | R | LAYER THICKNESS
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
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL     | F |R/W| THICKNESS
C PLA     | NEL     | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
        INTEGER NEL, NUVAR, NPT, IPT,IFLAG(1),
     .     NGL(NEL), NUPARAM,ISRATE,INLOC
        my_real TIME,TIMESTEP
        my_real UPARAM(NUPARAM),
     .     AREA(NEL),RHO0(NEL),EINT(NEL,2),
     .     THKLY(NEL),PLA(NEL),SHF(NEL),
     .     EPSPXX(NEL),EPSPYY(NEL),
     .     EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .     DEPSXX(NEL),DEPSYY(NEL),
     .     DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .     EPSXX(NEL) ,EPSYY(NEL) ,
     .     EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .     SIGOXX(NEL),SIGOYY(NEL),
     .     SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .     SEQ_OUTPUT(NEL),EPSP(NEL),DPLANL(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
        my_real
     .      SIGNXX(NEL),SIGNYY(NEL),
     .      SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .      SIGVXX(NEL),SIGVYY(NEL),
     .      SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .      SOUNDSP(NEL),VISCMAX(NEL),ETSE(NEL),SIGY(NEL),HARDM(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
        my_real
     .      UVAR(NEL,NUVAR), OFF(NEL),THK(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION
C-----------------------------------------------
        INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
        my_real FINTER ,TF(*)
        EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I,J,NRATE,J1,J2,N,NINDX,NMAX,
     .          IAD1(NEL),IPOS1(NEL),ILEN1(NEL),NS,
     .          IAD2(NEL),IPOS2(NEL),ILEN2(NEL),OPTE,
     .          JJ(NEL),INDEX(NEL),NBMAX,NPARAM,IDFUNCE,NINC
        PARAMETER (NPARAM=5)
        my_real A,FAC, DEZZ, EPST,NU31,DR0
        my_real DG(NEL),SIGY_O(NEL),SIGY_K(NEL),
     .          E(NEL), A1(NEL),A2(NEL),G(NEL), G3(NEL),
     .          MC(NEL,4),MD(NEL),A_XY(NEL),PARAM(NEL,NPARAM),
     .          A01, A02, A03, A12,
     .          DYDX1(NEL),DYDX2(NEL),RATE(NEL,2),SVM(NEL),
     .          YLD(NEL), Y1(NEL), Y2(NEL),
     .          YFAC(NEL,2), NNU1(NEL), NU(NEL),
     .          SXX(NEL),SYY(NEL),SXY(NEL),
     .          XXX(NEL),XYY(NEL),XXY(NEL),ALPHA(NEL),
     .          FAIL(NEL), EPSMAX,
     .          EPSR1 ,EPSR2 ,S11(NEL), S22(NEL),
     .          B_1(NEL) ,B_2(NEL) ,B_3(NEL) ,Q12(NEL),
     .          H(NEL),GS(NEL),FISOKIN ,HK(NEL) ,FHK ,M,
     .          CE,EINF, ESCALE(NEL),C1(NEL),DYDXE(NEL),YLDN(NEL),NORM,NORM1
        my_real K1,K2,SEFF,DK1_DSIGXX,DK1_DSIGYY,DK2_DSIGXX,DK2_DSIGYY,DF_DK1,
     .          DF_DK2,NORMXX,NORMYY,DK2_DSIGXY,NORMXY,SIG_DFDSIG
C
        DATA NMAX/4/,NS/10/,NBMAX/3/
C=======================================================================
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
        A01 = UPARAM(7)
C------- A02 = C*2^M  now, Scalar is sufficient
        A02 = UPARAM(8)
        A03 = UPARAM(9)
        A12 = UPARAM(10)
        NRATE = NINT(UPARAM(1))
        M   = UPARAM(NS+2*NRATE+4)
        EPSMAX=UPARAM(NS+2*NRATE+1)

        DO I=1,NEL
          E(I)   = UPARAM(2)
          A1(I)  = UPARAM(3)
          A2(I)  = UPARAM(4)
          G(I)   = UPARAM(5)
          GS(I)  = G(I)*SHF(I)
          NU(I)  = UPARAM(6)
C        A02(I) = UPARAM(8)
          G3(I)  = UPARAM(NS+2*NRATE+5)
C
          NNU1(I)   = NU(I) / (ONE - NU(I))
        ENDDO
        IF(EPSMAX==ZERO)THEN
          IF(TF(NPF(IFUNC(1)+1)-1)==ZERO)THEN
            EPSMAX=TF(NPF(IFUNC(1)+1)-2)
          ELSE
            EPSMAX= EP30
          ENDIF
        ENDIF
c------------------------------------------------------
        EPSR1  = UPARAM(NS+2*NRATE+2)
        EPSR2  = UPARAM(NS+2*NRATE+3)
        FISOKIN= UPARAM(NS+2*NRATE+8)
        OPTE = UPARAM(NS+2*NRATE+10)
        EINF = UPARAM(NS+2*NRATE+11)
        CE   = UPARAM(NS+2*NRATE+12)

        IF (OPTE == 1)THEN
          IDFUNCE = UPARAM(NS+2*NRATE+9)
          DO I=1,NEL
            IF(PLA(I) > ZERO)THEN
              ESCALE(I) = FINTER(IFUNC(IDFUNCE),PLA(I),NPF,TF,DYDXE(I))
            ENDIF
          ENDDO

          DO I=1,NEL
            IF(PLA(I) > ZERO)THEN
              E(I) =  ESCALE(I)* E(I)
              A1(I) = E(I)/(ONE - NU(I)*NU(I))
              A2(I) = NU(I)*A1(I)
              G(I) =  HALF*E(I)/(ONE+NU(I))
              GS(I)   =  G(I)*SHF(I)
              G3(I) = THREE*G(I)
            ENDIF
          ENDDO
        ELSEIF ( CE /= ZERO) THEN

          DO I=1,NEL
            IF(PLA(I) > ZERO)THEN
              E(I) = E(I)-(E(I)-EINF)*(ONE-EXP(-CE*PLA(I)))
              A1(I) = E(I)/(ONE - NU(I)*NU(I))
              A2(I) = NU(I)*A1(I)
              G(I) =  HALF*E(I)/(ONE+NU(I))
              GS(I)   =  G(I)*SHF(I)
              G3(I) = THREE*G(I)
            ENDIF
          ENDDO
c--------------------------------------------------------------
        ENDIF
C
        IF (ISIGI==0) THEN
          IF(TIME==ZERO)THEN
            DO I=1,NEL
              UVAR(I,1)=ZERO
              UVAR(I,2)=ZERO
              UVAR(I,3)=ZERO
              UVAR(I,4)=ZERO
            ENDDO
            DO J=1,NRATE
              DO I=1,NEL
                UVAR(I,J+4)=ZERO
              ENDDO
            ENDDO
          ENDIF
        ENDIF
C
C  ------ecroui cine---
#include "vectorize.inc"
        DO I=1,NEL
!           SIGOXX(I) = SIGOXX(I) - UVAR(I,2)
!           SIGOYY(I) = SIGOYY(I) - UVAR(I,3)
!           SIGOXY(I) = SIGOXY(I) - UVAR(I,4)
        ENDDO
C   ---
#include "vectorize.inc"
        DO I=1,NEL
          SIGNXX(I)=SIGOXX(I) - UVAR(I,2) +A1(I)*DEPSXX(I)+A2(I)*DEPSYY(I)
          SIGNYY(I)=SIGOYY(I) - UVAR(I,3) +A2(I)*DEPSXX(I)+A1(I)*DEPSYY(I)
          SIGNXY(I)=SIGOXY(I) - UVAR(I,4) +G(I) *DEPSXY(I)
          SIGNYZ(I)=SIGOYZ(I)+GS(I)*DEPSYZ(I)
          SIGNZX(I)=SIGOZX(I)+GS(I)*DEPSZX(I)
C
          SOUNDSP(I) = SQRT(A1(I)/RHO0(I))
          VISCMAX(I) = ZERO
          ETSE(I) = ONE
C-------------------
C     STRAIN RATE
C-------------------
          IF (ISRATE == 0) THEN
            EPSP(I) = HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .       + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                     + EPSPXY(I)*EPSPXY(I) ) )
          ENDIF
C-------------------
C     STRAIN
C-------------------
          EPST = HALF*( EPSXX(I)+EPSYY(I)
     .     + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                   + EPSXY(I)*EPSXY(I) ) )
          FAIL(I) = MAX(ZERO,MIN(ONE,(EPSR2-EPST)/(EPSR2-EPSR1)))
C
        ENDDO
C-------------------
C     HARDENING LAW
C-------------------
        DO I=1,NEL
          JJ(I) = 1
        ENDDO
        DO J=2,NRATE-1
          DO I=1,NEL
            IF(EPSP(I)>=UPARAM(NS+J)) JJ(I) = J
          ENDDO
        ENDDO
#include "vectorize.inc"
        DO I=1,NEL
          RATE(I,1)=UPARAM(NS+JJ(I))
          RATE(I,2)=UPARAM(NS+JJ(I)+1)
          YFAC(I,1)=UPARAM(NS+NRATE+JJ(I))
          YFAC(I,2)=UPARAM(NS+NRATE+JJ(I)+1)
        ENDDO
#include "vectorize.inc"
        DO I=1,NEL
          J1 = JJ(I)
          J2 = J1+1
          IPOS1(I) = NINT(UVAR(I,J1+4))
          IAD1(I)  = NPF(IFUNC(J1)) / 2 + 1
          ILEN1(I) = NPF(IFUNC(J1)+1) / 2 - IAD1(I) - IPOS1(I)
          IPOS2(I) = NINT(UVAR(I,J2+4))
          IAD2(I)  = NPF(IFUNC(J2)) / 2 + 1
          ILEN2(I) = NPF(IFUNC(J2)+1) / 2 - IAD2(I) - IPOS2(I)
        ENDDO
C
        CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
        CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
C
        DO I=1,NEL
          J1 = JJ(I)
          J2 = J1+1
          UVAR(I,J1+4) = IPOS1(I)
          UVAR(I,J2+4) = IPOS2(I)
        ENDDO
#include "vectorize.inc"
        DO I=1,NEL
          J1 = JJ(I)
          J2 = J1+1
          Y1(I)=Y1(I)*YFAC(I,1)
          Y2(I)=Y2(I)*YFAC(I,2)
          FAC   = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
          YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
          YLD(I) = MAX(YLD(I),EM20)
          SIGY(I) = YLD(I)
          H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
C       ECROUISSAGE CINEMATIQUE
          Y1(I)=TF(NPF(IFUNC(J1))+1)
          Y2(I)=TF(NPF(IFUNC(J2))+1)
          YLD(I) = (1.-FISOKIN) * YLD(I) +
     .         FISOKIN * (FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I))))
        ENDDO
C-------------------------
C     BARLAT 3-PARAMETER CRITERION
C     A01->a;A02->c*2^m;A03->h;A12->p
C-------------------------
        H(1:NEL) = MAX(ZERO,H(1:NEL))
        B_1(1:NEL)=HALF*(SIGNXX(1:NEL)-A03*SIGNYY(1:NEL))
        B_2(1:NEL)=A12*SIGNXY(1:NEL)
        B_3(1:NEL)=SQRT(B_1(1:NEL)*B_1(1:NEL)+B_2(1:NEL)*B_2(1:NEL))

        DO  I=1,NEL
          A =HALF*(SIGNXX(I)+A03*SIGNYY(I))
          B_1(I)=A-B_3(I)
          B_2(I)=A+B_3(I)
        ENDDO
C----normilized by E*EM03 to avoid numeric failure in SP
        NORM =EP03/UPARAM(2)
        DO  I=1,NEL
          B_1(I)=B_1(I)*NORM
          B_2(I)=B_2(I)*NORM
          B_3(I)=B_3(I)*NORM
          YLDN(I) = YLD(I)*NORM
        ENDDO
C       A02(1:NEL)=A02(1:NEL)*TWO**M
        SVM(1:NEL)=A01*(ABS(B_1(1:NEL)**M)+ABS(B_2(1:NEL)**M))
     .            +A02*B_3(1:NEL)**M
        SVM(1:NEL)=(HALF*SVM(1:NEL))**(ONE/M)

        DO  I=1,NEL
          DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU1(I)
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
        ENDDO
        HARDM(1:NEL) = H(1:NEL)
!      ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
        NINDX=0

        DO I=1,NEL
          IF(SVM(I)>YLDN(I).AND.OFF(I)==ONE) THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          ENDIF
        ENDDO
        IF(NINDX>0) THEN
          NINC = NMAX
          IF (IMPL_S>0 .OR. IDTMINS>0) NINC = NMAX+1
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
#include "vectorize.inc"
          DO  J=1,NINDX
            I=INDEX(J)
            ETSE(I)= H(I)/(H(I)+E(I))
            Q12(I) =A12*A12
            HK(I) = H(I)*FISOKIN
C       H(I) = H(I)-HK(I)
            H(I) = MAX(ZERO,H(I))
C--------Ecroui. cinematique
c       FHK = FOUR_OVER_3*HK(I)/A1(I)
c       NU2(I) = ONE+FHK
c       NU3(I) = NU(I)+FHK*HALF
c       NU4(I) = HALF*(ONE-NU(I))+THREE_OVER_4*FHK
          ENDDO
C
C
#include "vectorize.inc"
          DO  J=1,NINDX
            I=INDEX(J)
            PARAM(J,1)=A01
            PARAM(J,2)=A02
            PARAM(J,3)=A03
            PARAM(J,4)=A12
            PARAM(J,5)=M
            S11(J) =SIGNXX(I)*NORM
            S22(J) =SIGNYY(I)*NORM
            A_XY(J)=SIGNXY(I)*NORM
            SXX(J) =S11(J)
            SYY(J) =S22(J)
            SXY(J) =A_XY(J)
            SIGY_O(J) =YLDN(I)
            SIGY_K(J) =YLDN(I)
C------Matrix C : S=C:E-------
            MC(J,1)=A1(I)*NORM
            MC(J,2)=A1(I)*NORM
            MC(J,3)=G(I)*NORM
            MC(J,4)=A2(I)*NORM
            MD(J)=H(I)*NORM
            DG(J)=ZERO
C------Xij: increment of plastic strains----
            XXX(J)=ZERO
            XYY(J)=ZERO
            XXY(J)=ZERO
            ALPHA(J)=ZERO
          ENDDO
C------NBMAX=3----
          DO N=1,NBMAX
            CALL DCPPM_UPC(
     1          NINDX  ,NEL    ,NPARAM ,PARAM  ,
     2          XXX    ,XYY    ,XXY    ,ALPHA  ,
     3          SXX    ,SYY    ,SXY    ,SIGY_K   ,
     4          S11    ,S22    ,A_XY   ,SIGY_O  ,
     5          MC     ,MD     ,DG     ,NINC    )
          ENDDO
C------------------------------------------
C     PLASTIC RETURN MAPPING
C------------------------------------------
          NORM1 =EM03*UPARAM(2)
C----to optimize with HK=0--
#include "vectorize.inc"
          DO J=1,NINDX
            I=INDEX(J)
            SIGNXX(I) =SXX(J)*NORM1
            SIGNYY(I) =SYY(J)*NORM1
            SIGNXY(I) =SXY(J)*NORM1
            PLA(I) = PLA(I) + ALPHA(J)
c       X_ij are inverse signs
            NU31=ONE-NU(I)/(ONE-NU(I))
            IF (INLOC == 0) THEN
              DEZZ = NU31*(XXX(J)+XYY(J))
              THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
            ENDIF
            DR0 = TWO_THIRD*HK(I)
c         DR0 = FOUR_OVER_3*HK(I)
            UVAR(I,2) = UVAR(I,2) - (TWO*XXX(J)+XYY(J))*DR0
            UVAR(I,3) = UVAR(I,3) - (TWO*XYY(J)+XXX(J))*DR0
            UVAR(I,4) = UVAR(I,4) -  XXY(J)*DR0
          ENDDO
C
C
          DO I=1,NEL
            IF(PLA(I)>EPSMAX.AND.OFF(I)==ONE)OFF(I)=ZEP80
          ENDDO
        ENDIF
#include "vectorize.inc"
        DO I=1,NEL
          SIGNXX(I) = SIGNXX(I) + UVAR(I,2)
          SIGNYY(I) = SIGNYY(I) + UVAR(I,3)
          SIGNXY(I) = SIGNXY(I) + UVAR(I,4)
        ENDDO
C
!-------------------------------------------------------------
!     NON-LOCAL THICKNESS VARIATION
!-------------------------------------------------------------
        IF (INLOC > 0) THEN
          DO I = 1,NEL
            ! Computation of the normal to the yield surface
            K1      = (SIGNXX(I) + A03*SIGNYY(I))*NORM/TWO
            K2      = SQRT(((SIGNXX(I) - A03*SIGNYY(I))/TWO)**TWO + (A12*SIGNXY(I))**TWO)*NORM
            SEFF    = A01*ABS(K1+K2)**M + A01*ABS(K1-K2)**M + A02*ABS(TWO*K2)**M
            SEFF    = (HALF*MAX(SEFF,EM20))**(ONE/M)
            DF_DK1  = (SEFF**(1-M))*(A01/TWO)*(
     .                +  SIGN(ONE,K1+K2)*(ABS(K1+K2)**(M-1))
     .                +  SIGN(ONE,K1-K2)*(ABS(K1-K2)**(M-1)))
            DF_DK2  = (SEFF**(1-M))*((A01/TWO)*(
     .                +  SIGN(ONE,K1+K2)*(ABS(K1+K2)**(M-1))
     .                -  SIGN(ONE,K1-K2)*(ABS(K1-K2)**(M-1)))
     .                +  A02*(ABS(TWO*K2)**(M-1)))
            DK1_DSIGXX = HALF
            DK1_DSIGYY = A03/TWO
            DK2_DSIGXX = (SIGNXX(I)-A03*SIGNYY(I))*NORM/(MAX(FOUR*K2,EM20))
            DK2_DSIGYY = -A03*(SIGNXX(I)-A03*SIGNYY(I))*NORM/(MAX(FOUR*K2,EM20))
            DK2_DSIGXY = (A12**TWO)*SIGNXY(I)*NORM/MAX(K2,EM20)
            NORMXX     = DF_DK1*DK1_DSIGXX + DF_DK2*DK2_DSIGXX
            NORMYY     = DF_DK1*DK1_DSIGYY + DF_DK2*DK2_DSIGYY
            NORMXY     = DF_DK2*DK2_DSIGXY
            SIG_DFDSIG = NORMXX*SIGNXX(I) + NORMYY*SIGNYY(I) + NORMXY*SIGNXY(I)
            ! Updating the thickness
            DEZZ       = -NU(I)*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E(I))
            IF (SIG_DFDSIG /= ZERO) THEN
              DEZZ     = DEZZ - MAX(DPLANL(I),ZERO)*(SEFF/SIG_DFDSIG)*(NORMXX + NORMYY)
            ENDIF
            THK(I)     = THK(I) + DEZZ*THKLY(I)*OFF(I)
          ENDDO
        ENDIF
!-------------------------
!     EQUIVALENT STRESS FOR OUTPUT - BARLAT -
!-------------------------
        DO I = 1,NEL
          K1 = (SIGNXX(I) + A03*SIGNYY(I))/TWO
          K2 = SQRT(((SIGNXX(I) - A03*SIGNYY(I))/TWO)**TWO + (A12*SIGNXY(I))**TWO)
          SEQ_OUTPUT(I) = A01*ABS(K1+K2)**M + A01*ABS(K1-K2)**M + A02*ABS(TWO*K2)**M
          IF (SEQ_OUTPUT(I) > ZERO) THEN
            SEQ_OUTPUT(I) = (HALF*SEQ_OUTPUT(I))**(ONE/M)
          ELSE
            SEQ_OUTPUT(I) = ZERO
          ENDIF
        ENDDO
!---
        RETURN
      END
Chd|====================================================================
Chd|  FONC_F                        source/materials/mat/mat057/sigeps57c.F
Chd|-- called by -----------
Chd|        DCPPM_UPC                     source/materials/mat/mat057/sigeps57c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FONC_F(
     1     NEL    ,F      ,NPARAM ,PARAM  ,
     2     B_1     ,B_2     ,B_3  ,SIGYM  ,NEL0)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
        INTEGER NEL, NEL0, NPARAM
        my_real
     .     F(NEL)   ,PARAM(NEL0,NPARAM),
     .     B_1(NEL), B_2(NEL), B_3(NEL)  ,SIGYM(NEL)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I,K,NINC
        my_real
     .    A01, A02, M, A, SVM
C
C----PARAM:1:A01,2:A02,3:A03,4:A12,5:M
C
#include "vectorize.inc"
        DO  I=1,NEL
          A01 = PARAM(I,1)
          A02 = PARAM(I,2)
C        A03 = PARAM(I,3)
C        A12 = PARAM(I,4)
          M = PARAM(I,5)
          SVM=A01*(ABS(B_1(I)**M)+ABS(B_2(I)**M))+A02*B_3(I)**M
          F(I)=SVM/SIGYM(I)-TWO
C        F(I)=SVM-TWO*SIGY(I)**M
        ENDDO
        RETURN
      END
Chd|====================================================================
Chd|  DERIVE_F                      source/materials/mat/mat057/sigeps57c.F
Chd|-- called by -----------
Chd|        DCPPM_LOWC                    source/materials/mat/mat057/sigeps57c.F
Chd|        DCPPM_UPC                     source/materials/mat/mat057/sigeps57c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DERIVE_F(
     1     NEL    ,NPARAM ,PARAM , SIGXX  ,SIGYY  ,
     2    SIGXY   ,SIGY   ,B_1   ,B_2     ,B_3    ,
     3       MF   ,GM     ,GM44  ,NEL0)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-------------------------------------------------------------------------------------------
C               INPUT ARGUMENT
C-------------------------------------------------------------------------------------------
        INTEGER NEL, NEL0, NPARAM
        my_real
     .     PARAM(NEL0,NPARAM), SIGXX(NEL), SIGYY(NEL), SIGXY(NEL),
     .     SIGY(NEL)
C-------------------------------------------------------------------------------------------
C               OUTPUT ARGUMENT MF:derivee premiere,GM:derivee seconde
C-------------------------------------------------------------------------------------------
        my_real
     .       MF(NEL,4),GM(NEL,6),GM44(NEL),B_1(NEL),B_2(NEL),B_3(NEL)
C-------------------------------------------------------------------------------------------
C               LOCALS VARIABLES
C-------------------------------------------------------------------------------------------
        INTEGER I
        my_real
     .      A01, A02, A03, A12, M,
     .      C_1,C_2,B3_1,A,B_1M2,B_2M2,B_1M1,B_2M1,B_3M2,B_3M1,
     .      DB_3,AP,AN,DB_312,Q12,H2,P2,DB_32,DB_3_3,DB_3_2,
     .      D_11(NEL),D_22(NEL),D_33(NEL),D_12(NEL),D_13(NEL),D_23(NEL)
C
#include "vectorize.inc"
        DO  I=1,NEL
          M = PARAM(I,5)
          A01 = M*PARAM(I,1)
          A02 = M*PARAM(I,2)
          A03 = PARAM(I,3)
          A12 = PARAM(I,4)
C-------------------------------------------------------------------------------------------
C                Derive de f
C-------------------------------------------------------------------------------------------
          C_1=HALF*(SIGXX(I)-A03*SIGYY(I))
          C_2=A12*SIGXY(I)
          B_3(I)=SQRT(C_1*C_1+C_2*C_2)
          B3_1=ONE/MAX(EM20,B_3(I))
          A =HALF*(SIGXX(I)+A03*SIGYY(I))
          B_1(I)=A-B_3(I)
          B_2(I)=A+B_3(I)
          B_1M2=A01*ABS(B_1(I)**(M-TWO))
          B_2M2=A01*ABS(B_2(I)**(M-TWO))
          B_1M1=B_1(I)*B_1M2
          B_2M1=B_2(I)*B_2M2
          B_3M2=A02*B_3(I)**(M-TWO)
          B_3M1=B_3(I)*B_3M2
C-------------------------------------------------------------------------------------------
C                Dr/Ds11
C-------------------------------------------------------------------------------------------
          DB_3 = FOURTH*(SIGXX(I)-A03*SIGYY(I))*B3_1
          AP=HALF*(B_1M1+B_2M1)
          AN=DB_3*(B_1M1-B_2M1-B_3M1)
          DB_312 = A12*C_2*B3_1
C-------------------------------------------------------------------------------------------
C                Derive premier
C-------------------------------------------------------------------------------------------
          MF(I,1)=AP-AN
          MF(I,2)=A03*(AP+AN)
          MF(I,3)=DB_312*(-B_1M1+B_2M1+B_3M1)
          MF(I,4)=-TWO*M*SIGY(I)**(M-ONE)
C-------------------------------------------------------------------------------------------
C                Derive seconde de f
C-------------------------------------------------------------------------------------------
          Q12 = C_2*C_2
          H2 = A03*A03
          P2 = A12*A12
          DB_32 =DB_3*DB_3
          DB_3_3 =Q12*B3_1*B3_1
C-------------------------------------------------------------------------------------------
C                D2r/D2s11
C-------------------------------------------------------------------------------------------
          DB_3_2 =FOURTH*DB_3_3*B3_1
C-------------------------------------------------------------------------------------------
C                Derive seconde
C-------------------------------------------------------------------------------------------
          A = (M-ONE)*DB_3*(B_1M2-B_2M2)
          D_11(I)=(M-ONE)*((FOURTH+DB_32)*(B_1M2+B_2M2)+DB_32*B_3M2)
     .            +DB_3_2*(-B_1M1+B_2M1+B_3M1)
          D_22(I)=(D_11(I)+A)*H2
          D_11(I)= D_11(I)-A
C---------------------------------------------------------------------------------------------
C                    d2f/d2S12
C---------------------------------------------------------------------------------------------
          D_33(I)=P2*((-B_1M1+B_2M1+B_3M1)*(B3_1-DB_3_2)
     .                +(M-ONE)*DB_3_3*(-B_1M2+B_2M2+B_3M2))
C--------------------------------------------------------------------------------------------
C                    d2f/dS11S22
C---------------------------------------------------------------------------------------------
          A = A03*DB_3_2
          D_12(I)=A*((M-ONE)*B_3(I)*(B_1M2+B_2M2)+(B_1M1-B_2M1-B_3M1))
     .            -A03*(M-ONE)*DB_32*B_3M2
C--------------------------------------------------------------------------------------------
C                    d2f/dS11S12
C---------------------------------------------------------------------------------------------
          A = DB_312*(M-ONE)*HALF*(-B_1M2+B_2M2)
          D_13(I)=DB_312*DB_3*((M-ONE)*(B_1M2+B_2M2+B_3M2)
     .                          +B3_1*(B_1M1-B_2M1-B_3M1))
          D_23(I)=A03*(A-D_13(I))
          D_13(I)=D_13(I)+A
          GM44(I)  = -TWO*M*(M-ONE)*SIGY(I)**(M-TWO)
        ENDDO
C---------------------------------------------------------------------------------------------
        DO  I=1,NEL
          GM(I,1)=D_11(I)
          GM(I,2)=D_22(I)
          GM(I,3)=D_33(I)
          GM(I,4)=D_12(I)
          GM(I,5)=D_13(I)
          GM(I,6)=D_23(I)
        ENDDO
        RETURN
      END
Chd|====================================================================
Chd|  INCREAM                       source/materials/mat/mat057/sigeps57c.F
Chd|-- called by -----------
Chd|        DCPPM_LOWC                    source/materials/mat/mat057/sigeps57c.F
Chd|        DCPPM_UPC                     source/materials/mat/mat057/sigeps57c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INCREAM(NEL ,G  , G44, GM, GM44, DG, R, D ,NEL0)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   DUMMY   A r g u m e n t s
C-----------------------------------------------
        INTEGER NEL, NPARAM, NEL0
        my_real
     .     G(NEL0,4)  ,G44(NEL), DG(NEL),
     .     GM(NEL,6) ,GM44(NEL), R(NEL,4),D(NEL,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I, J, K, N
        my_real
     .     B(NEL,3,3) ,B44(NEL) ,DETA_1(NEL) ,DETA_2(NEL) ,
     .     DETA_3(NEL) ,DETA(NEL) ,INVA(NEL,3,3)
C----------------------------------------------------------------------------
C                          CALCUL DE B =dg* GRAD(M)*G+I
C-------------------------------------------------------------------------------
C                            JACOBIAN
C------------------------------------------------------------------------------
C
        DO I=1,NEL
          B(I,1,1) = ONE+DG(I)*(GM(I,1)*G(I,1)+GM(I,4)*G(I,4))
          B(I,1,2) =    DG(I)*(GM(I,1)*G(I,4)+GM(I,4)*G(I,2))
          B(I,1,3) =    DG(I)*GM(I,5)*G(I,3)
          B(I,2,2) = ONE+DG(I)*(GM(I,4)*G(I,4)+GM(I,2)*G(I,2))
          B(I,2,3) =    DG(I)*GM(I,6)*G(I,3)
          B(I,3,3) = ONE+DG(I)*GM(I,3)*G(I,3)
        ENDDO
C
        DO I=1,NEL
          B44(I) =  ONE+DG(I)*GM44(I)*G44(I)
        ENDDO
C                   |B11 B12 B13  0 |   |          0 |
C                   |B21 B22 B23  0 |   |   A      0 |
C               B = |B31 B32 B33  0 | = |          0 |
C                   | 0   0   0  B44|   |0  0  0  B44|
C-----------------------------------------------------------------------------
C                          DETERMINANT DE A
C----------------------------------------------------------------------------
        DO I=1,NEL
          DETA_1(I) = B(I,2,2)*B(I,3,3)-B(I,2,3)*B(I,2,3)
          DETA_2(I) = -B(I,1,2)*B(I,3,3)+B(I,2,3)*B(I,1,3)
          DETA_3(I) = B(I,1,2)*B(I,2,3)-B(I,2,2)*B(I,1,3)
          DETA(I) = B(I,1,1)*DETA_1(I)+B(I,1,2)*DETA_2(I)+
     .                 B(I,1,3)*DETA_3(I)
          IF(ABS(DETA(I))<=EM20) THEN
            DETA(I) = EM20
          ELSE
            DETA(I) = ONE/DETA(I)
          ENDIF
        ENDDO
C------------------------------------------------------------------------------
C                          INVERSE DE A = INVA
C----------------------------------------------------------------------------
        DO I=1,NEL
          INVA(I,1,1) = DETA_1(I)*DETA(I)
          INVA(I,1,2) = DETA_2(I)*DETA(I)
          INVA(I,1,3) = DETA_3(I)*DETA(I)
          INVA(I,2,2) = (B(I,1,1)*B(I,3,3)-B(I,1,3)*B(I,1,3))*DETA(I)
          INVA(I,2,3) = -(B(I,1,1)*B(I,2,3)-B(I,1,3)*B(I,1,2))*DETA(I)
          INVA(I,3,3) = (B(I,1,1)*B(I,2,2)-B(I,1,2)*B(I,1,2))*DETA(I)
        ENDDO
C-----------------------------------------------------------------------------
C                        D = -J^(-1)*R
C----------------------------------------------------------------------------
C
        DO I=1,NEL
          D(I,1) = -(INVA(I,1,1)*R(I,1)+INVA(I,1,2)*R(I,2)+
     .                 INVA(I,1,3)*R(I,3))
          D(I,2) = -(INVA(I,1,2)*R(I,1)+INVA(I,2,2)*R(I,2)+
     .                 INVA(I,2,3)*R(I,3))
          D(I,3) = -(INVA(I,1,3)*R(I,1)+INVA(I,2,3)*R(I,2)+
     .                 INVA(I,3,3)*R(I,3))
          D(I,4) = -R(I,4)/MAX(EM20,B44(I))
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  DCPPM_LOWC                    source/materials/mat/mat057/sigeps57c.F
Chd|-- called by -----------
Chd|        DCPPM_UPC                     source/materials/mat/mat057/sigeps57c.F
Chd|-- calls ---------------
Chd|        DERIVE_F                      source/materials/mat/mat057/sigeps57c.F
Chd|        INCREAM                       source/materials/mat/mat057/sigeps57c.F
Chd|====================================================================
      SUBROUTINE DCPPM_LOWC(
     1    NEL    ,NPARAM ,PARAM  ,
     2    XXX    ,XYY    ,XXY    ,ALPHA  ,DG ,
     3    SIGXX  ,SIGYY  ,SIGXY  ,SIGY   ,
     4    SIGTXX ,SIGTYY ,SIGTXY ,SIGY_OLD  ,
     5    G      ,    G44,NEL0)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
        INTEGER NEL, NEL0, NPARAM
        my_real
     .     SIGTXX(NEL) ,SIGTYY(NEL) ,SIGTXY(NEL) ,SIGY_OLD(NEL) ,
     .     G(NEL0,4)  ,G44(NEL),
     .     DG(NEL), PARAM(NEL0,NPARAM)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
        my_real
     .     XXX(NEL)   ,XYY(NEL)   ,XXY(NEL)   ,ALPHA(NEL),
     .     SIGXX(NEL) ,SIGYY(NEL) ,SIGXY(NEL) ,SIGY(NEL)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I
        my_real
     .     GM(NEL,6) ,GM44(NEL), MF(NEL,4)   ,
     .     R(NEL,4) ,D(NEL,4)  ,
     .     B_1(NEL) ,B_2(NEL) ,B_3(NEL)
C-------------------------------------------------------------------------------
C                            JACOBIAN
C------------------------------------------------------------------------------
C
        CALL DERIVE_F(
     1    NEL    ,NPARAM ,PARAM , SIGXX  ,SIGYY  ,
     2   SIGXY   ,SIGY_OLD,B_1   ,B_2     ,B_3    ,
     3      MF   ,GM     ,GM44   ,NEL0)
C-----------------------------------------------------------------------------
C                                   VECTEUR R
C-----------------------------------------------------------------------------
        DO I=1,NEL
          R(I,1) = XXX(I)+DG(I)*MF(I,1)
          R(I,2) = XYY(I)+DG(I)*MF(I,2)
          R(I,3) = XXY(I)+DG(I)*MF(I,3)
          R(I,4) = ALPHA(I)+DG(I)*MF(I,4)
        ENDDO
        CALL INCREAM(NEL ,G  , G44, GM, GM44, DG, R, D ,NEL0)
C------------------------------------------------------------------------------
C                       LINE SEARCH
C------------------------------------------------------------------------------
        ALPHA(1:NEL) = MAX(ZERO,(ALPHA(1:NEL)+D(1:NEL,4)))
        DO I=1,NEL
          XXX(I) = XXX(I)+D(I,1)
          XYY(I) = XYY(I)+D(I,2)
          XXY(I) = XXY(I)+D(I,3)
          SIGXX(I) = SIGTXX(I)+G(I,1)*XXX(I)+G(I,4)*XYY(I)
          SIGYY(I) = SIGTYY(I)+G(I,4)*XXX(I)+G(I,2)*XYY(I)
          SIGXY(I) = SIGTXY(I)+G(I,3)*XXY(I)
          SIGY(I)  = SIGY_OLD(I)+G44(I)*ALPHA(I)
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  DCPPM_UPC                     source/materials/mat/mat057/sigeps57c.F
Chd|-- called by -----------
Chd|        SIGEPS57C                     source/materials/mat/mat057/sigeps57c.F
Chd|-- calls ---------------
Chd|        DCPPM_LOWC                    source/materials/mat/mat057/sigeps57c.F
Chd|        DERIVE_F                      source/materials/mat/mat057/sigeps57c.F
Chd|        FONC_F                        source/materials/mat/mat057/sigeps57c.F
Chd|        INCREAM                       source/materials/mat/mat057/sigeps57c.F
Chd|====================================================================
      SUBROUTINE DCPPM_UPC(
     1     NEL    ,NEL0   ,NPARAM ,PARAM  ,
     2     XXX    ,XYY    ,XXY    ,ALPHA  ,
     3     SIGXX  ,SIGYY  ,SIGXY  ,SIGY   ,
     4     SIGTXX ,SIGTYY ,SIGTXY ,SIGY_OLD  ,
     6     G      ,G44    ,DG     ,NINC   )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
        INTEGER NEL, NEL0, NPARAM, NINC
        my_real
     .     PARAM(NEL0,NPARAM)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
        my_real
     .     G(NEL0,4)    ,G44(NEL), DG(NEL),
     .     XXX(NEL)   ,XYY(NEL)   ,XXY(NEL)   ,ALPHA(NEL),
     .     SIGXX(NEL) ,SIGYY(NEL) ,SIGXY(NEL) ,SIGY(NEL) ,
     .     SIGTXX(NEL) ,SIGTYY(NEL) ,SIGTXY(NEL) ,SIGY_OLD(NEL)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I, J, N
        my_real
     .     JJ(NEL,3),JJ4(NEL),P(NEL),GM(NEL,6) ,
     .     B_1(NEL) ,B_2(NEL) ,B_3(NEL),SIGYM(NEL),
     .     GM44(NEL)   ,F(NEL), MF(NEL,4),D(NEL,4),dDG
C      DATA  NINC/4/
C-----------normalize F By SIGY**M-----------------------------------------------------------------
        DO I=1,NEL
          SIGYM(I) = SIGY(I)**PARAM(I,5)
        ENDDO
C
        CALL DERIVE_F(
     1    NEL    ,NPARAM ,PARAM , SIGXX  ,SIGYY  ,
     2   SIGXY   ,SIGY   ,B_1   ,B_2     ,B_3    ,
     3      MF   ,GM     ,GM44  ,NEL0)
C                        D = -J^(-1)*MF
        CALL INCREAM(NEL ,G  , G44, GM, GM44, DG, MF, D ,NEL0)
C-----------------------------------------------------------------------------
C                          J = -G * D
C-----------------------------------------------------------------------------
C
        DO I=1,NEL
          JJ(I,1) = -(G(I,1)*D(I,1)+G(I,4)*D(I,2))
          JJ(I,2) = -(G(I,4)*D(I,1)+G(I,2)*D(I,2))
          JJ(I,3) = -G(I,3)*D(I,3)
          JJ4(I) = -G44(I)*D(I,4)
        ENDDO
C-----------------------------------------------------------------------------
C                         PRODUIT SCALAIRE DE GRAD(F)*J
C-----------------------------------------------------------------------------
        DO I=1,NEL
          P(I) = (JJ(I,1)*MF(I,1)+JJ(I,2)*MF(I,2)+JJ(I,3)*MF(I,3)+
     .    JJ4(I)*MF(I,4))/SIGYM(I)
        ENDDO
C-----------------------------------------------------------------------------
C                         CALCUL DE F
C------------------------------------------------------------------------------
        CALL FONC_F(
     1     NEL    ,F      ,NPARAM ,PARAM  ,
     2     B_1  ,B_2  ,B_3  ,SIGYM ,NEL0)
C
C------------------------------------------------------------------------------
C                         CALCUL DE DG
C-------------------------------------------------------------------------------
        DO I=1,NEL
          dDG = F(I)/P(I)
          DG(I) = MAX(ZERO,(DG(I)+dDG))
        ENDDO

        DO N=1,NINC
          CALL DCPPM_LOWC(
     1        NEL    ,NPARAM ,PARAM,
     4        XXX    ,XYY    ,XXY  ,ALPHA  ,DG ,
     7        SIGXX  ,SIGYY  ,SIGXY  ,SIGY   ,
     8        SIGTXX ,SIGTYY ,SIGTXY ,SIGY_OLD,
     9        G      ,G44    ,NEL0)
        ENDDO
C
        RETURN
      END
