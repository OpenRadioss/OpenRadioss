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
!||    puff_mod    ../common_source/eos/puff.F
!||--- called by ------------------------------------------------------
!||    eosmain     ../common_source/eos/eosmain.F
!||    eosmain51   ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
      MODULE PUFF_MOD
      IMPLICIT NONE
      CONTAINS
!||====================================================================
!||    puff            ../common_source/eos/puff.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||====================================================================
      SUBROUTINE PUFF(
     1                IFLAG ,NEL   ,PMIN ,OFF  ,EINT ,MU   ,MU2,
     2                ESPE  ,DVOL  ,DF   ,VNEW ,
     3                PNEW  ,DPDM  ,DPDE ,EOS_STRUCT)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of PUFF EOS
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
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CONSTANT_MOD , ONLY : ZERO, EM15, HALF, ONE, THREE_HALF, TWO, THREE, THREE100
      USE EOS_PARAM_MOD , ONLY : EOS_PARAM_
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
#include      "my_real.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) ::  IFLAG, NEL
      my_real,INTENT(IN) :: PMIN, OFF(NEL), MU(NEL),DVOL(NEL),MU2(NEL),DF(NEL),VNEW(NEL),ESPE(NEL)
      my_real,INTENT(INOUT) ::  DPDM(NEL), DPDE(NEL), PNEW(NEL)
      my_real,INTENT(INOUT) ::  EINT(NEL)
      TYPE(EOS_PARAM_) , INTENT(IN) :: EOS_STRUCT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real AA, BB, DVV, ETA, XX, GX, PRES, CC, EXPA, EE
      my_real C1,C2,C3,T1,T2, G0,ESUBL,HH,PSH(NEL)
C--------------------------------------------------------------------

      C1    = EOS_STRUCT%UPARAM(1)
      C2    = EOS_STRUCT%UPARAM(2)
      C3    = EOS_STRUCT%UPARAM(3)
      T1    = EOS_STRUCT%UPARAM(4)
      T2    = EOS_STRUCT%UPARAM(5)
      ESUBL = EOS_STRUCT%UPARAM(6)
      G0    = EOS_STRUCT%UPARAM(7)
      HH    = EOS_STRUCT%UPARAM(8)

      PSH(1:NEL)  = EOS_STRUCT%PSH


      IF(IFLAG == 0) THEN
        DO I=1,NEL
          XX =MU(I)/(ONE+MU(I))
          IF(MU(I) >= ZERO) THEN
            AA=(C1+C3*MU2(I))*MU(I)+C2*MU2(I)
            GX=ONE-G0*HALF*XX
            BB=G0
            PRES=MAX(AA*GX+BB*ESPE(I),PMIN)*OFF(I)
            DPDM(I)=(C1+TWO*C2*MU(I)+THREE*C3*MU2(I))*GX+G0*DF(I)*DF(I)*(PRES-HALF*AA)
          ELSEIF(ESPE(I) < ESUBL)THEN
            AA=(T1+T2*MU(I))*MU(I)
            GX=ONE-G0*HALF*XX
            BB=G0
            PRES=MAX(AA*GX+BB*ESPE(I),PMIN)*OFF(I)
            DPDM(I)=(T1+TWO*T2*MU(I))*GX+G0*DF(I)*DF(I)*(PRES-HALF*AA)
          ELSE
            ETA=ONE+MU(I)
            EE=SQRT(ETA)
            BB=(HH+(G0-HH)*EE)*ETA
            CC= C1/(G0*ESUBL)
            EXPA=EXP(CC*XX)
            AA= BB*ESUBL*(EXPA-ONE)
            PRES=MAX(AA+BB*ESPE(I),PMIN)*OFF(I)
            DPDM(I)=BB*DF(I)*DF(I)*(PRES+ESUBL*EXPA*CC) +(ESPE(I)+ESUBL*(EXPA-ONE))*(HH+THREE_HALF*EE*(G0-HH))
          ENDIF
          DPDE(I)=BB
          PNEW(I) = MAX(PRES,PMIN)*OFF(I)! P(mu[n+1],E[n])
        ENDDO

      ELSEIF(IFLAG == 1) THEN
        DO I=1,NEL
          DVV=HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
          XX =MU(I)/(ONE+MU(I))
          IF(MU(I) >= ZERO) THEN
            AA=(C1+C3*MU2(I))*MU(I)+C2*MU2(I)
            AA=AA*(ONE-G0*HALF*XX)
            BB=G0
          ELSEIF(ESPE(I) < ESUBL)THEN
            AA=(T1+T2*MU(I))*MU(I)
            AA=AA*(ONE-G0*HALF*XX)
            BB=G0
          ELSE
            ETA=ONE+MU(I)
            BB=(HH+(G0-HH)*SQRT(ETA))*ETA
            CC=C1/(G0*ESUBL)
            EXPA=EXP(CC*XX)
            AA=BB*ESUBL*(EXPA-ONE)
          ENDIF
          DPDE(I)=BB
          PNEW(I)=(AA +BB*ESPE(I))/(ONE+ BB*DVV)
          PNEW(I)=MAX(PNEW(I),PMIN)*OFF(I)! P(mu[n+1],E[n+1])
          EINT(I)=EINT(I)-HALF*DVOL(I)*PNEW(I)
        ENDDO

      ELSEIF(IFLAG == 2) THEN
        DO I=1, NEL
          IF (VNEW(I) > ZERO) THEN
            XX =MU(I)/(ONE+MU(I))
            IF(MU(I) >= ZERO) THEN
               AA=(C1+C3*MU2(I))*MU(I)+C2*MU2(I)
               GX=ONE-G0*HALF*XX
               BB=G0
               PRES=MAX(AA*GX+BB*ESPE(I),PMIN)*OFF(I)
               DPDM(I)=(C1+TWO*C2*MU(I)+THREE*C3*MU2(I))*GX+G0*DF(I)*DF(I)*(PRES-HALF*AA)
            ELSEIF(ESPE(I)<ESUBL)THEN
               AA=(T1+T2*MU(I))*MU(I)
               GX=ONE-G0*HALF*XX
               BB=G0
               PRES=MAX(AA*GX+BB*ESPE(I),PMIN)*OFF(I)
               DPDM(I)=(T1+TWO*T2*MU(I))*GX+G0*DF(I)*DF(I)*(PRES-HALF*AA)
            ELSE
               ETA=ONE+MU(I)
               EE=SQRT(ETA)
               BB=(HH+(G0-HH)*EE)*ETA
               CC= C1/(G0*ESUBL)
               EXPA=EXP(CC*XX)
               AA= BB*ESUBL*(EXPA-ONE)
               PRES=MAX(AA+BB*ESPE(I),PMIN)*OFF(I)
               DPDM(I)=BB*DF(I)*DF(I)*(PRES+ESUBL*EXPA*CC) + (ESPE(I)+ESUBL*(EXPA-ONE))*(HH+THREE_HALF*EE*(G0-HH))
            ENDIF
            DPDE(I)=BB
            PNEW(I) = PRES
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
        RETURN
      END SUBROUTINE PUFF
      END MODULE PUFF_MOD
