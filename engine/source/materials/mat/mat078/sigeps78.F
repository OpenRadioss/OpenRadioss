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
C=======================================================================
C      Yoshida-Uemori law - two surface plasticity
C=======================================================================
Chd|====================================================================
Chd|  SIGEPS78                      source/materials/mat/mat078/sigeps78.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F
Chd|====================================================================
      SUBROUTINE SIGEPS78 (
     1     NEL      ,NUPARAM  ,NUVAR    ,NFUNC    ,IFUNC    ,NPF      ,
     2     TF       ,TIME     ,TIMESTEP ,UPARAM   ,RHO0     ,RHO      ,
     3     SIGA     ,SIGB     ,SIGC     ,UVAR     ,PLA      ,DEP      ,
     4     DEPSXX   ,DEPSYY   ,DEPSZZ   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     5     SIGOXX   ,SIGOYY   ,SIGOZZ   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     6     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     7     SOUNDSP  ,OFF      ,YLD      ,ETSE     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
        my_real ,DIMENSION(6,NEL), INTENT(INOUT) :: SIGA,SIGB,SIGC
        INTEGER NEL, NUPARAM, NUVAR
        my_real
     .     TIME,TIMESTEP,UPARAM(NUPARAM),
     .     RHO(NEL),RHO0(NEL),
     .     DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .     DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .     SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .     SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
        my_real
     .      SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .      SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .      SOUNDSP(NEL),YLD(NEL) ,
     .      PLA(NEL),DEP(NEL),ETSE(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
        my_real
     .      UVAR(NEL,NUVAR), OFF(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION
C-----------------------------------------------
        INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
        my_real
     .         FINTER ,TF(*)
        EXTERNAL FINTER
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
        INTEGER I,J,K,NITER,OPTE
        my_real EINI,NU,BSAT,HYU,CYU,RSAT,
     .          YIELD,BYU,MYU,C1_KH,C2_KH,
     .          EINF,COE,FCTP,FCT,CUN,CDEUX,CTROIS,SIGY,DMU,PRDEN,
     .          THA,THB,TOLR,SEFF,DG,SEFFP,CASTA,THETAQ,DTCA,DTBM,
     .          STHABXX,STHABYY,STHABZZ,STHABXY,STHABYZ,STHABZX,
     .          SAXX,SAYY,SAZZ,SAXY,SAYZ,SAZX,GAMA,HDGSIG,H,BQDB,EQBQ
        my_real :: YOUNG(NEL),
     .          ALXX(NEL),ALYY(NEL),ALZZ(NEL),RBULK(NEL),SHEAR(NEL),LAMDA(NEL),
     .          ALXY(NEL),ALYZ(NEL),ALZX(NEL),
     .          AYU(NEL),SIGM(NEL),SIGEQ(NEL),DYDXE(NEL),WAVE(NEL),R(NEL),
     .          STRIALXX(NEL),STRIALYY(NEL),STRIALZZ(NEL),
     .          STRIALXY(NEL),STRIALYZ(NEL),STRIALZX(NEL),
     .          G2(NEL),ASTA(NEL),SCALEE(NEL),RNIH(NEL),DRNIH(NEL),EPBAR(NEL),
     .          MAX_ASTA(NEL)
        my_real :: DB(6,NEL),EP(6,NEL),AST(6,NEL),BETA(6,NEL),
     .          DR(NEL),BQ(6,NEL)
C=======================================================================
C     SIGA         = CENTER OF YIELD SURFACE ALPHA
C     SIGB         = CENTER OF BOUNDING SURFACE
C     SIGC         = q : CENTER OF NON-IH SURFACE G_SIGMA
c
C     UVAR(I,1)    = R : ISOTROPIC HARDENING OF BOUNDING SURFACE
C     UVAR(I,2)    = r : RADIUS OF G_SIGMA (RNIH)
C     UVAR(I,3)    = a = B + R - YIELD     (AYU)
C     UVAR(I,4)    = YLD STRESS
C     UVAR(I,5)    = EFFECTIVE PLASTIC STRAIN INCREMENT
C=======================================================================
        EINI  = UPARAM(1)
        NU    = UPARAM(2)
        YIELD = UPARAM(3)
        BYU   = UPARAM(4)
        C2_KH = UPARAM(5)
        HYU   = UPARAM(6)
        BSAT  = UPARAM(7)
        MYU   = UPARAM(8)
        RSAT  = UPARAM(9)
        EINF  = UPARAM(10)
        COE   = UPARAM(11)
        OPTE  = UPARAM(12)
        C1_KH = UPARAM(22)
c
        NITER = 3
        CUN = THIRD/(ONE-TWO*NU)
        CDEUX = HALF/(ONE+NU)
        CTROIS = NU/(ONE+NU)/(ONE-TWO*NU)
        IF (ISIGI == 0) THEN
          IF(TIME == ZERO) THEN
            DO I=1,NEL
              UVAR(I,3) = BSAT - YIELD
              UVAR(I,4) = YIELD
            ENDDO
          ENDIF
        ENDIF
C       EINI = initial young modulus
C       EINF = saturated young modulus
C       PLA=equivalent plastic strain (eps bar)
C     READING STORED STATE_VARIABLES
        DO J=1,6
          DO I=1,NEL
            AST(J,I) =SIGA(J,I)
            BETA(J,I)=SIGB(J,I)
          ENDDO
        ENDDO
        IF (OPTE == 1)THEN
          DO I=1,NEL
            YOUNG(I)=EINI
            IF(PLA(I) > ZERO)THEN
              SCALEE(I) = FINTER(IFUNC(1),PLA(I),NPF,TF,DYDXE(I))
              YOUNG(I) = SCALEE(I)*EINI
            ENDIF
          ENDDO
        ELSE
          DO I=1,NEL
            YOUNG(I)=EINI
            IF(PLA(I) > ZERO)THEN
              YOUNG(I)=EINI-(EINI-EINF)*(ONE-EXP(-COE*PLA(I)))
            ENDIF
          ENDDO
        ENDIF
        DO I=1,NEL
          RBULK(I) = YOUNG(I)*CUN
          SHEAR(I) = YOUNG(I)*CDEUX
          LAMDA(I) = YOUNG(I)*CTROIS
          G2(I) = TWO*SHEAR(I)
C       Module d'onde de compression
          WAVE(I)=G2(I)+LAMDA(I)
          ASTA(I)=ZERO
          MAX_ASTA(I) = UVAR(I,6)
        ENDDO

        DO I=1,NEL
          ALXX(I)=AST(1,I)+BETA(1,I)
          ALYY(I)=AST(2,I)+BETA(2,I)
          ALZZ(I)=AST(3,I)+BETA(3,I)
          ALXY(I)=AST(4,I)+BETA(4,I)
          ALYZ(I)=AST(5,I)+BETA(5,I)
          ALZX(I)=AST(6,I)+BETA(6,I)
          ASTA(I)=ASTA(I)+THREE_HALF*(
     .         AST(1,I)*AST(1,I)+AST(2,I)*AST(2,I)+AST(3,I)*AST(3,I)
     .        +TWO*(AST(4,I)*AST(4,I)+AST(5,I)*AST(5,I)
     .        +AST(6,I)*AST(6,I)))
          ASTA(I)=SQRT(MAX(EM20,ASTA(I)))
          MAX_ASTA(I) = MAX(MAX_ASTA(I),ASTA(I))
        ENDDO
C    =====================================================
C    DEVIATORIC STRESS AT CURRENT STEP
C    =====================================================
        DO I=1,NEL
C     Estimation sigma_n+1
          SIGNXX(I)=SIGOXX(I)+WAVE(I)*DEPSXX(I)
     .                       +LAMDA(I)*DEPSYY(I)
     .                       +LAMDA(I)*DEPSZZ(I)
          SIGNYY(I)=SIGOYY(I)+WAVE(I)*DEPSYY(I)
     .                       +LAMDA(I)*DEPSXX(I)
     .                       +LAMDA(I)*DEPSZZ(I)
          SIGNZZ(I)=SIGOZZ(I)+WAVE(I)*DEPSZZ(I)
     .                       +LAMDA(I)*DEPSXX(I)
     .                       +LAMDA(I)*DEPSYY(I)
          SIGM(I)=-(SIGNXX(I)+SIGNYY(I)+SIGNZZ(I)) * THIRD
C       STRIAL_ij deviateur de sigma_n+1=sigm_n+ELa DEPS
          STRIALXX(I)=SIGNXX(I)+SIGM(I)
          STRIALYY(I)=SIGNYY(I)+SIGM(I)
          STRIALZZ(I)=SIGNZZ(I)+SIGM(I)
          STRIALXY(I)=SIGOXY(I)+G2(I)*DEPSXY(I)
          STRIALYZ(I)=SIGOYZ(I)+G2(I)*DEPSYZ(I)
          STRIALZX(I)=SIGOZX(I)+G2(I)*DEPSZX(I)
        ENDDO
C    =====================================================
C    CURRENT TRIAL EQUIVALENT STRESS CALCULATION
C    =====================================================
        DO I=1,NEL
C     f=3/2(s-alpha):(s-alpha)-Y.Y
          SEFF=SQRT(THREE_HALF*(
     .           (STRIALXX(I)-ALXX(I))*(STRIALXX(I)-ALXX(I))
     .            +(STRIALYY(I)-ALYY(I))*(STRIALYY(I)-ALYY(I))
     .            +(STRIALZZ(I)-ALZZ(I))*(STRIALZZ(I)-ALZZ(I))
     .            +TWO*((STRIALXY(I)-ALXY(I))*(STRIALXY(I)-ALXY(I))
     .            +(STRIALYZ(I)-ALYZ(I))*(STRIALYZ(I)-ALYZ(I))
     .            +(STRIALZX(I)-ALZX(I))*(STRIALZX(I)-ALZX(I)))))
          DEP(I)=ZERO
          AYU(I)=UVAR(I,3)
          IF (SEFF <= YIELD) THEN
            DO J=1,6
              EP(J,I)=ZERO
            ENDDO
          ELSE
C=======================================================================
C------------------------------------------------------------
C   PROJECTION
C------------------------------------------------------------
C=======================================================================
C       Compute the effective strain increment with Newton algorithm
            DEP(I)=UVAR(I,5)
C=======================================================================
C                                  Newton
C=======================================================================
            IF (MAX_ASTA(I) < BSAT - YIELD) THEN
              CYU = C1_KH
            ELSE
              CYU = C2_KH
            ENDIF
            PRDEN=THREE*SHEAR(I)+CYU*AYU(I)+MYU*BYU
            CASTA=CYU*SQRT(AYU(I)/ASTA(I))
            DO K=1,NITER
c       THA et THB pour semi implicite---------------------------------
              THA=ONE-CASTA*DEP(I)
              THB=ONE-MYU*DEP(I)
C       sum of Tetaa alpha* + Tetab Beta=STHAB
              STHABXX=THA*AST(1,I)+THB*BETA(1,I)
              STHABYY=THA*AST(2,I)+THB*BETA(2,I)
              STHABZZ=THA*AST(3,I)+THB*BETA(3,I)
              STHABXY=THA*AST(4,I)+THB*BETA(4,I)
              STHABYZ=THA*AST(5,I)+THB*BETA(5,I)
              STHABZX=THA*AST(6,I)+THB*BETA(6,I)
              SEFF=SQRT(THREE_HALF*(
     .                 (STRIALXX(I)-STHABXX)*(STRIALXX(I)-STHABXX)
     .                +(STRIALYY(I)-STHABYY)*(STRIALYY(I)-STHABYY)
     .                +(STRIALZZ(I)-STHABZZ)*(STRIALZZ(I)-STHABZZ)
     .          +TWO*((STRIALXY(I)-STHABXY)*(STRIALXY(I)-STHABXY)
     .                +(STRIALYZ(I)-STHABYZ)*(STRIALYZ(I)-STHABYZ)
     .                +(STRIALZX(I)-STHABZX)*(STRIALZX(I)-STHABZX))))
              SEFF=MAX(EM20,SEFF)
              FCT=SEFF-YIELD-PRDEN*DEP(I)
              SEFFP=THREE*((STRIALXX(I)-STHABXX)
     .                    *(CASTA*AST(1,I)+MYU*BETA(1,I))
     .                 +(STRIALYY(I)-STHABYY)
     .                    *(CASTA*AST(2,I)+MYU*BETA(2,I))
     .                 +(STRIALZZ(I)-STHABZZ)
     .                    *(CASTA*AST(3,I)+MYU*BETA(3,I))
     .           +TWO*((STRIALXY(I)-STHABXY)
     .                    *(CASTA*AST(4,I)+MYU*BETA(4,I))
     .                 +(STRIALYZ(I)-STHABYZ)
     .                    *(CASTA*AST(5,I)+MYU*BETA(5,I))
     .                 +(STRIALZX(I)-STHABZX)
     .                   *(CASTA*AST(6,I)+MYU*BETA(6,I))))
c
              FCTP=HALF*SEFFP/SEFF-PRDEN
              DEP(I)=MAX(ZERO,DEP(I)-FCT/FCTP)
            ENDDO
C=======================================================================
C                    END of Newton iteration
C=======================================================================
            THA=ONE-CASTA*DEP(I)
            THB=ONE-MYU*DEP(I)
C       somme de Tetaa alpha* + Tetab Beta=STHAB
            STHABXX=THA*AST(1,I)+THB*BETA(1,I)
            STHABYY=THA*AST(2,I)+THB*BETA(2,I)
            STHABZZ=THA*AST(3,I)+THB*BETA(3,I)
            STHABXY=THA*AST(4,I)+THB*BETA(4,I)
            STHABYZ=THA*AST(5,I)+THB*BETA(5,I)
            STHABZX=THA*AST(6,I)+THB*BETA(6,I)
c
c        SEFF=SQRT(THREE*HALF*(
c     .           (STRIALXX(I)-STHABXX)*(STRIALXX(I)-STHABXX)
c     .          +(STRIALYY(I)-STHABYY)*(STRIALYY(I)-STHABYY)
c     .          +(STRIALZZ(I)-STHABZZ)*(STRIALZZ(I)-STHABZZ)
c     .    +TWO*((STRIALXY(I)-STHABXY)*(STRIALXY(I)-STHABXY)
c     .          +(STRIALYZ(I)-STHABYZ)*(STRIALYZ(I)-STHABYZ)
c     .          +(STRIALZX(I)-STHABZX)*(STRIALZX(I)-STHABZX))))
c        SEFF=MAX(EM20,SEFF)
c ========================================================================
C        Calcul de s_n+1 - alpha_n+1
            SAXX=YIELD*(STRIALXX(I)-STHABXX)/(YIELD+DEP(I)*PRDEN)
            SAYY=YIELD*(STRIALYY(I)-STHABYY)/(YIELD+DEP(I)*PRDEN)
            SAZZ=YIELD*(STRIALZZ(I)-STHABZZ)/(YIELD+DEP(I)*PRDEN)
            SAXY=YIELD*(STRIALXY(I)-STHABXY)/(YIELD+DEP(I)*PRDEN)
            SAYZ=YIELD*(STRIALYZ(I)-STHABYZ)/(YIELD+DEP(I)*PRDEN)
            SAZX=YIELD*(STRIALZX(I)-STHABZX)/(YIELD+DEP(I)*PRDEN)
C============= EP= delta epsilon_n+1^p=======================
            EP(1,I)=THREE_HALF*DEP(I)*SAXX/YIELD
            EP(2,I)=THREE_HALF*DEP(I)*SAYY/YIELD
            EP(3,I)=THREE_HALF*DEP(I)*SAZZ/YIELD
            EP(4,I)=THREE_HALF*DEP(I)*SAXY/YIELD
            EP(5,I)=THREE_HALF*DEP(I)*SAYZ/YIELD
            EP(6,I)=THREE_HALF*DEP(I)*SAZX/YIELD
c          EPBAR(I)=SQRT(2./3.*(EP(1,I)*EP(1,I)+EP(2,I)*EP(2,I)
c     .      +EP(3,I)*EP(3,I)+TWO*(EP(4,I)*EP(4,I)+EP(5,I)*EP(5,I)
c     .      +EP(6,I)*EP(6,I))))
C=======================================================================
            DTCA=TWO*THIRD*CYU*AYU(I)
            DTBM=TWO*THIRD*BYU*MYU
            DO J=1,6
C         Actualisation de alpha* et beta avec Epsilon_n+1^p
              AST(J,I)=THA*AST(J,I)+DTCA*EP(J,I)
              BETA(J,I)=THB*BETA(J,I)+DTBM*EP(J,I)
            ENDDO
            DO J=1,6
C         Compute betadot
              DB(J,I)=BETA(J,I)-SIGB(J,I)
              SIGA(J,I)=AST(J,I)
              SIGB(J,I)=BETA(J,I)
            ENDDO
C=======================================================================
C    WORKHARDENING STAGNATION
C=======================================================================
            R(I)=UVAR(I,1)
            BQDB=ZERO
            EQBQ=ZERO
            RNIH(I)=UVAR(I,2)
            DO J=1,6
C         BQ= beta_n+1-q_n
              BQ(J,I)=BETA(J,I)-SIGC(J,I)
C         Compute g_sigma the non-IH surface
              EQBQ=EQBQ+THREE_HALF*BQ(J,I)*BQ(J,I)
              BQDB=BQDB+BQ(J,I)*DB(J,I)
            ENDDO
            TOLR=EQBQ-RNIH(I)*RNIH(I)
            IF (TOLR >= ZERO .AND. BQDB > ZERO) THEN
              R(I)=THB*UVAR(I,1)+MYU*RSAT*DEP(I)  ! Rdot=m(R_sat-R).pdot
              DR(I)=R(I)-UVAR(I,1)
            ELSE
              DR(I)=ZERO
            ENDIF
            GAMA=ZERO
            DMU=ZERO
            IF ( HYU> ZERO )THEN
              HDGSIG=THREE*HYU*BQDB
              IF (RNIH(I) == ZERO)THEN
                DMU = EQBQ/HDGSIG-ONE
              ELSE
                DMU =((THREE*HYU*BQDB+SQRT(HDGSIG*HDGSIG
     .            +FOUR*RNIH(I)*RNIH(I)*EQBQ))
     .           /(TWO*RNIH(I)*RNIH(I)))-ONE
              ENDIF
              BQDB=ZERO
              DO J=1,6
                SIGC(J,I)=BETA(J,I)-BQ(J,I)/(ONE+DMU) !update of q_n+1
              ENDDO
              DO J=1,6
                BQ(J,I)=BETA(J,I)-SIGC(J,I)
              ENDDO
              DO J=1,6
                BQDB=BQDB+BQ(J,I)*DB(J,I)
              ENDDO
              IF (DR(I) > ZERO) THEN
                IF ( RNIH(I) == ZERO) THEN
                  RNIH(I)=THREE*BQDB*HYU
                ELSE
                  GAMA=BQDB*THREE_HALF/RNIH(I)
                  RNIH(I)= UVAR(I,2)+ HYU*GAMA
                ENDIF
              ENDIF
            ENDIF
            UVAR(I,1)=R(I)
            UVAR(I,2)=RNIH(I)
            AYU(I)=BSAT+R(I)-YIELD
            UVAR(I,3)=AYU(I)
C      END WORKHARDENING STAGNATION
          ENDIF
        ENDDO
C=======================================================================
C    UPDATE CURRENT STRESSES
C============================================================
        DO I=1,NEL
          SIGNXX(I)=SIGOXX(I)+WAVE(I)*(DEPSXX(I)-EP(1,I))
     .                       +LAMDA(I)*(DEPSYY(I)-EP(2,I))
     .                       +LAMDA(I)*(DEPSZZ(I)-EP(3,I))
          SIGNYY(I)=SIGOYY(I)+WAVE(I)*(DEPSYY(I)-EP(2,I))
     .                       +LAMDA(I)*(DEPSXX(I)-EP(1,I))
     .                       +LAMDA(I)*(DEPSZZ(I)-EP(3,I))
          SIGNZZ(I)=SIGOZZ(I)+WAVE(I)*(DEPSZZ(I)-EP(3,I))
     .                       +LAMDA(I)*(DEPSXX(I)-EP(1,I))
     .                       +LAMDA(I)*(DEPSYY(I)-EP(2,I))
          SIGNXY(I)=SIGOXY(I)+G2(I)*(DEPSXY(I)-EP(4,I))
          SIGNYZ(I)=SIGOYZ(I)+G2(I)*(DEPSYZ(I)-EP(5,I))
          SIGNZX(I)=SIGOZX(I)+G2(I)*(DEPSZX(I)-EP(6,I))
c==========Module tangent=====================================

          IF( DEP(I) > ZERO)THEN
            SIGM(I)=-(SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))*THIRD
            SIGY=SQRT(THREE_HALF*(
     .       (SIGNXX(I)+SIGM(I))*(SIGNXX(I)+SIGM(I))
     .      +(SIGNYY(I)+SIGM(I))*(SIGNYY(I)+SIGM(I))
     .      +(SIGNZZ(I)+SIGM(I))*(SIGNZZ(I)+SIGM(I))
     .      +TWO*(SIGNXY(I)*SIGNXY(I)+SIGNYZ(I)*SIGNYZ(I)
     .      +SIGNZX(I)*SIGNZX(I))))
            H = (SIGY-UVAR(I,4))/DEP(I)
            H = MAX(EM10, H)
            UVAR(I,4) = SIGY
            UVAR(I,5) = DEP(I)
            UVAR(I,6) = MAX_ASTA(I)
            ETSE(I) = H/G2(I)
            PLA(I)  = PLA(I)+DEP(I)
          ELSE
            ETSE(I) = ONE
          ENDIF
C=======================================================================
          SOUNDSP(I) = SQRT((RBULK(I)+FOUR_OVER_3*SHEAR(I))/RHO0(I))
          YLD(I) = UVAR(I,4)
        ENDDO
c-----------
        RETURN
      END

