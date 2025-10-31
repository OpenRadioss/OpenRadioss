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
!||    gruneisen_mod   ../common_source/eos/gruneisen.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
      MODULE GRUNEISEN_MOD
      IMPLICIT NONE
      CONTAINS
!||====================================================================
!||    gruneisen       ../common_source/eos/gruneisen.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      SUBROUTINE GRUNEISEN(
     1                     IFLAG , NEL    ,PMIN ,OFF   ,EINT  ,MU   ,MU2 ,
     2                     ESPE  , DVOL   ,DF   ,VNEW  ,RHO0,
     3                     PNEW  , DPDM   ,DPDE ,PSH   ,
     4                     EOS_STRUCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE EOS_PARAM_MOD , ONLY : EOS_PARAM_
      USE PRECISION_MOD, ONLY : WP
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IFLAG               !< flag for the operation (staggered scheme IFLAG=0,1 ; collocated scheme IFLAG=2)
      INTEGER,INTENT(IN) :: NEL                 !< number of elements in the current group
      my_real, INTENT(IN) :: PMIN               !< minimum pressure allowed
      my_real,INTENT(IN) :: OFF(NEL)            !< OFF=1 if element is active, OFF=0 if element is inactive
      my_real,INTENT(IN) :: MU(NEL)             !< volumetric strain : rho/rho0-1
      my_real,INTENT(IN) :: MU2(NEL)            !< MU**2 IF MU > 0 , 0 otherwise
      my_real,INTENT(IN) :: ESPE(NEL)           !< specific internal energy
      my_real,INTENT(IN)  :: DVOL(NEL)          !< volume change
      my_real,INTENT(IN) :: VNEW(NEL)           !< current volume
      my_real,INTENT(IN) :: RHO0(NEL)           !< reference density
      my_real,INTENT(IN) :: DF(NEL)             !< relative volume (v/v0 = rho0/rho)
      my_real,INTENT(INOUT) :: EINT(NEL)        !< internal energy
      my_real,INTENT(INOUT) :: PSH(NEL)         !< pressure shift (for relative pressure modeling)
      my_real,INTENT(INOUT) :: PNEW(NEL)        !< current pressure
      my_real,INTENT(INOUT) :: DPDE(NEL)        !< partial derivative dP/dE where E=Eint/V0
      my_real,INTENT(INOUT) :: DPDM(NEL)        !< total derivative : DP/Dmu = dPdmu + dPdE* P/(1+mu)**2
      TYPE(EOS_PARAM_),INTENT(IN) :: EOS_STRUCT !< data structure for EoS parameters
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      real(kind=WP) :: AA, BB, DVV, FF, FG, FAC, XX, DFF, DFG, FAC1, PP
      real(kind=WP) :: CC,S1,S2,S3,G0,GA
C-----------------------------------------------
C   B o d y
C-----------------------------------------------
      CC     = EOS_STRUCT%UPARAM(1)
      S1     = EOS_STRUCT%UPARAM(2)
      S2     = EOS_STRUCT%UPARAM(3)
      S3     = EOS_STRUCT%UPARAM(4)
      G0     = EOS_STRUCT%UPARAM(5)
      GA     = EOS_STRUCT%UPARAM(6)
      PSH(:) = EOS_STRUCT%PSH

      IF(IFLAG == 0) THEN !--- SOUND SPEED DERIVATIVE (DPDE is partial derivative, DPDM is total derivative)
        ! Derivative
        DO I=1,NEL
         FAC=ONE
         FAC1=ONE
         IF(MU(I) > ZERO) THEN
          XX= MU(I)/(ONE+MU(I))
          FF=ONE+(ONE-HALF*G0)*MU(I)-HALF*GA*MU2(I)
          FG=ONE-(S1-ONE+S2*XX+S3*XX*XX)*MU(I)
          FAC=FF/(FG*FG)
          DFF=ONE-HALF*G0-GA*MU(I)
          DFG=ONE-S1+XX*(-TWO*S2+XX*(S2-THREE*S3)+TWO*S3*XX*XX)
          FAC1=FAC*(ONE+MU(I)*(DFF/FF-TWO*DFG/FG))
         ENDIF
         AA=FAC*RHO0(I)*CC*CC*MU(I)
         BB=G0+GA*MU(I)
         PP=MAX(AA+BB*ESPE(I),PMIN)*OFF(I) ! total pressure need for sound speed calculation (=> no pressure shift)
         DPDM(I)=FAC1*RHO0(I)*CC*CC+PP*DF(I)*DF(I)*BB+GA*ESPE(I)
         DPDE(I)=BB
         PNEW(I) = MAX(PP,PMIN)*OFF(I)! P(mu[n+1],E[n])
         PNEW(I) = PNEW(I) - PSH(I)
        ENDDO

      ELSEIF(IFLAG == 1) THEN  !--- P[n+1] and EINT[n+1]
        ! Pressure P[n+1]
        ! Internal Energy Eint[n+1] (2nd order integration requires P[n+1])
        DO I=1,NEL
         FAC=ONE
         IF(MU(I) > ZERO) THEN
          XX= MU(I)/(ONE+MU(I))
          FF=ONE+(ONE-HALF*G0)*MU(I)-HALF*GA*MU2(I)
          FG=ONE-(S1-ONE+S2*XX+S3*XX*XX)*MU(I)
          FAC=FF/(FG*FG)
         ENDIF
         AA=FAC*RHO0(I)*CC*CC*MU(I)
         BB=G0+GA*MU(I)
         DPDE(I) = BB
         DVV=HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
         PNEW(I)=(AA+BB*ESPE(I))/(ONE+BB*DVV)
         PNEW(I)= MAX(PNEW(I),PMIN)*OFF(I) ! P(mu[n+1],E[n+1])
         EINT(I)=EINT(I) - HALF*DVOL(I)*PNEW(I) !total pressure for energy integration
         PNEW(I) = PNEW(I) - PSH(I)
        ENDDO

      ELSEIF(IFLAG == 2) THEN  !--- collocated scheme (law151)
        ! pressure and sound speed derivatives
        DO I=1,NEL
          IF (VNEW(I) > ZERO) THEN
            FAC=ONE
            FAC1=ONE
            IF(MU(I) > ZERO) THEN
               XX= MU(I)/(ONE+MU(I))
               FF=ONE+(ONE-HALF*G0)*MU(I)-HALF*GA*MU2(I)
               FG=ONE-(S1-ONE+S2*XX+S3*XX*XX)*MU(I)
               FAC=FF/(FG*FG)
               DFF=ONE-HALF*G0-GA*MU(I)
               DFG=ONE-S1+XX*(-TWO*S2+XX*(S2-THREE*S3)+TWO*S3*XX*XX)
               FAC1=FAC*(ONE+MU(I)*(DFF/FF-TWO*DFG/FG))
            ENDIF
            AA=FAC*RHO0(I)*CC*CC*MU(I)
            BB=G0+GA*MU(I)
            PNEW(I)=MAX(AA+BB*ESPE(I),PMIN)*OFF(I)
            DPDM(I)=FAC1*RHO0(I)*CC*CC+PNEW(I)*DF(I)*DF(I)*BB+GA*ESPE(I)
            DPDE(I)=BB
            PNEW(I)=PNEW(I)-PSH(I)
          ENDIF
        ENDDO

        ENDIF

        RETURN
      END SUBROUTINE GRUNEISEN
      END MODULE GRUNEISEN_MOD
