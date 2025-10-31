Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    tillotson_mod   ../common_source/eos/tillotson.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
      MODULE TILLOTSON_MOD
      IMPLICIT NONE
      CONTAINS
!||====================================================================
!||    tillotson       ../common_source/eos/tillotson.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      SUBROUTINE TILLOTSON(
     1                     IFLAG,NEL  ,PMIN ,OFF   ,EINT    ,MU    , MU2,
     2                     ESPE ,DVOL ,DF   ,VNEW  ,PSH     ,
     3                     PNEW ,DPDM ,DPDE ,VAREOS, NVAREOS, EOS_STRUCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE CONSTANT_MOD , ONLY : ZERO, EM15, HALF, ONE, TWO
        USE EOS_PARAM_MOD , ONLY : eos_param_
        use precision_mod , only : WP
        IMPLICIT NONE
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
!----------------------------------------------------------------------------
!! \details STAGGERED SCHEME IS EXECUTED IN TWO PASSES IN EOSMAIN : IFLG=0 THEN IFLG=1
!! \details COLLOCATED SCHEME IS DOING A SINGLE PASS : IFLG=2
!! \details
!! \details  STAGGERED SCHEME
!! \details     EOSMAIN / IFLG = 0 : DERIVATIVE CALCULATION FOR SOUND SPEED ESTIMATION c[n+1] REQUIRED FOR PSEUDO-VISCOSITY (DPDE:partial derivative, DPDM:total derivative)
!! \details     MQVISCB            : PSEUDO-VISCOSITY Q[n+1]
!! \details     MEINT              : INTERNAL ENERGY INTEGRATION FOR E[n+1] : FIRST PART USING P[n], Q[n], and Q[n+1] CONTRIBUTIONS
!! \details     EOSMAIN / IFLG = 1 : UPDATE P[n+1], T[N+1]
!! \details                          INTERNAL ENERGY INTEGRATION FOR E[n+1] : LAST PART USING P[n+1] CONTRIBUTION
!! \details                            (second order integration dE = -P.dV where P = 0.5(P[n+1] + P[n]) )
!! \details  COLLOCATED SCHEME
!! \details     EOSMAIN / IFLG = 2 : SINGLE PASS FOR P[n+1] AND DERIVATIVES
!----------------------------------------------------------------------------
C
C Solver for Tillotson Equation of State.
C This equation of state depends of material state. It has several region in (P,v) diagram :
C        REGION=1 / (I)  : compression
C        REGION=2 / (II) : cold expansion
C        REGION=3 / (III): transition (currently empty but can be implemented)
C        REGION=4 / (IV) : hot expansion
C
C  in following source code regions are identified with following criteria :
C         !IF(MU(I) => ZERO)THEN
C         !  REGION = 1
C         !ELSEIF(MU(I) < ZERO)THEN
C         !  REGION = 2
C         !  IF( V>VSUBL_(I) .OR.  (V<VSUBL .AND. ESPE(I)>=ESUBL_(I)) )THEN
C         !    REGION=4
C         !  ENDIF
C         !ENDIF
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) ::  IFLAG, NEL, NVAREOS
      real(kind=WP) :: PMIN,
     .           OFF(NEL) , EINT(NEL), MU(NEL)  ,
     .           MU2(NEL) , ESPE(NEL), DVOL(NEL), DF(NEL),
     .           VNEW(NEL), PNEW(NEL), DPDM(NEL), DPDE(NEL)
      real(kind=WP),INTENT(INOUT) :: VAREOS(NEL,NVAREOS)
      real(kind=WP),INTENT(INOUT) :: PSH(NEL)
      TYPE(EOS_PARAM_),INTENT(IN) :: EOS_STRUCT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
      real(kind=WP) :: AA, BB, DVV, ETA, ENEW, OMEGA, XX, EXPA, EXPB,
     .                 PP, FACC1, FACC2, FACPB,
     .                 C1,C2,PTIA,PTIB,EZERO,
     .                 ALPHA,BETA,ESUBL,VSUBL,
     .                 REGION
C--------------------------------------------------------------------
      C1      = EOS_STRUCT%UPARAM(1)
      C2      = EOS_STRUCT%UPARAM(2)
      PTIA    = EOS_STRUCT%UPARAM(3)
      PTIB    = EOS_STRUCT%UPARAM(4)
      EZERO   = EOS_STRUCT%UPARAM(5)
      ESUBL   = EOS_STRUCT%UPARAM(6)
      VSUBL   = EOS_STRUCT%UPARAM(7)
      ALPHA   = EOS_STRUCT%UPARAM(8)
      BETA    = EOS_STRUCT%UPARAM(9)
      PSH(:)  = EOS_STRUCT%PSH

      IF(IFLAG == 0) THEN
        DO I=1,NEL
          FACC1=ONE
          FACC2=ONE
          FACPB=ONE
          REGION=1
          IF(MU(I) < ZERO) THEN
            REGION=2
            FACC2=ZERO
            IF(DF(I) > VSUBL .OR. (DF(I) <= VSUBL .AND. ESPE(I) >= ESUBL)) THEN
             REGION=4
             XX=MU(I)/(ONE+MU(I))
             EXPA=EXP(-ALPHA*XX*XX)
             EXPB=EXP(BETA*XX)
             FACC1=EXPA*EXPB
             FACPB=EXPA
            ENDIF
          ENDIF
          ETA=ONE+MU(I)
          OMEGA= ONE+ESPE(I)/(EZERO*ETA**2)
          AA=FACC1*C1*MU(I)+FACC2*C2*MU2(I)
          BB=PTIA+FACPB*PTIB/OMEGA
          PP=MAX(AA+BB*ETA*ESPE(I),PMIN)*OFF(I)
          DPDM(I)=FACC1*C1+TWO*FACC2*C2*MU(I)+BB*ETA*PP*DF(I)*DF(I)
     .           +ESPE(I)*( BB+(TWO*ESPE(I)/ETA-PP*DF(I)*DF(I))
     .           *PTIB*FACPB/(EZERO*ETA*OMEGA**2) )
          DPDE(I)=BB*ETA
          VAREOS(I,1)=REGION
          PNEW(I) = MAX(PP,PMIN)*OFF(I)! P(mu[n+1],E[n])
        ENDDO

      ELSEIF(IFLAG == 1) THEN

        DO I=1,NEL
          DVV=HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
          ETA=ONE+MU(I)
          OMEGA= ONE+ESPE(I)/(EZERO*ETA**2)
          FACC1=ONE
          FACC2=ONE
          FACPB=ONE
          REGION=1
          IF(MU(I) < ZERO) THEN
            REGION=2
            FACC2=ZERO
            IF(DF(I) > VSUBL .OR. (DF(I) <= VSUBL .AND. ESPE(I) >= ESUBL)) THEN
              REGION=4
              XX=MU(I)/(ONE+MU(I))
              EXPA=EXP(-ALPHA*XX*XX)
              EXPB=EXP(BETA*XX)
              FACC1=EXPA*EXPB
              FACPB=EXPA
            ENDIF
          ENDIF
          AA=FACC1*C1*MU(I)+FACC2*C2*MU2(I)
          BB=(PTIA+FACPB*PTIB/OMEGA)*ETA
          PNEW(I)=(AA +BB*ESPE(I))/(ONE+ BB*DVV)
          ENEW=ESPE(I) - PNEW(I)*DVV
          !one iteration
          OMEGA= ONE+ENEW/(EZERO*ETA**2)
          BB=(PTIA+FACPB*PTIB/OMEGA)*ETA
          PNEW(I)=(AA +BB*ESPE(I))/(ONE+ BB*DVV)
          PNEW(I)= MAX(PNEW(I),PMIN)*OFF(I)! P(mu[n+1],E[n+1])
          EINT(I)= EINT(I) - HALF*DVOL(I)*PNEW(I)
          DPDE(I) = BB
          VAREOS(I,1)=REGION
        ENDDO

      ELSEIF(IFLAG == 2) THEN

        DO I=1, NEL
          REGION=1
          IF (VNEW(I) > ZERO) THEN
            FACC1=ONE
            FACC2=ONE
            FACPB=ONE
            REGION=1
            IF(MU(I)<ZERO) THEN
               REGION=2
               FACC2=ZERO
               IF(DF(I) > VSUBL .OR. (DF(I) <= VSUBL .AND. ESPE(I) >= ESUBL)) THEN
                  REGION=4
                  XX  = MU(I)/(ONE+MU(I))
                  EXPA= EXP(-ALPHA*XX*XX)
                  EXPB= EXP(BETA*XX)
                  FACC1=EXPA*EXPB
                  FACPB=EXPA
               ENDIF
            ENDIF
            ETA=ONE+MU(I)
            OMEGA= ONE+ESPE(I)/(EZERO*ETA**2)
            AA=FACC1*C1*MU(I)+FACC2*C2*MU2(I)
            BB=PTIA+FACPB*PTIB/OMEGA
            PP=MAX(AA+BB*ETA*ESPE(I),PMIN)*OFF(I)
            DPDM(I)=FACC1*C1+TWO*FACC2*C2*MU(I)+BB*ETA*PP*DF(I)*DF(I)
     .             +ESPE(I)*( BB+(TWO*ESPE(I)/ETA-PP*DF(I)*DF(I))
     .             *PTIB*FACPB/(EZERO*ETA*OMEGA**2) )
            DPDE(I)=BB*ETA
            PNEW(I) = PP
          ENDIF
          VAREOS(I,1)=REGION
        ENDDO
      ENDIF
      RETURN
      END SUBROUTINE TILLOTSON
      END MODULE TILLOTSON_MOD
