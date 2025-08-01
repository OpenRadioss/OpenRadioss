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
!||    osborne   ../common_source/eos/osborne.F
!||--- called by ------------------------------------------------------
!||    eosmain   ../common_source/eos/eosmain.F
!||====================================================================
      SUBROUTINE OSBORNE  (IFLAG,NEL  ,PM   ,OFF  ,EINT ,MU   ,
     2                     ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH  ,
     3                     PNEW ,DPDM ,DPDE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of OSBORNE EOS
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
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(NEL), IFLAG, NEL
      my_real PM(NPROPM,NUMMAT), 
     .        OFF(NEL)  ,EINT(NEL) ,MU(NEL)   , 
     .        ESPE(NEL) ,DVOL(NEL) ,DF(NEL)  ,
     .        VNEW(NEL) ,PNEW(NEL) ,DPDM(NEL), PSH(NEL) ,
     .        DPDE(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real :: P0,E0,DVV,PP,dPdMU
      my_real :: A1,A2,B0,B1,B2,C0,C1,D0, A2_,RHO0
      my_real :: DENOM
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(IFLAG == 0) THEN
        MX         = MAT(1)
        RHO0       = PM(01,MX)
        E0         = PM(23,MX)
        A1         = PM(164,MX)
        A2         = PM(32,MX)
        B0         = PM(33,MX)
        B1         = PM(35,MX)
        B2         = PM(36,MX)
        C0         = PM(160,MX)
        C1         = PM(161,MX)
        D0         = PM(162,MX)
        P0         = PM(163,MX)
        PSH(1:NEL) = PM(88,MX)
        DO I=1,NEL
          A2_=A2
          IF(MU(I)<ZERO)A2_=-A2
          DENOM   = (ESPE(I)+D0)
          PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))/DENOM
          dPdMU   = (A1+2*A2_*MU(I)+(TWO*B2*MU(I)+B1)*ESPE(I)+C1*ESPE(I)*ESPE(I))/DENOM
          dPdE(I) = (((B2*MU(I)+B1)*MU(I)+B0)+(TWO*(C1*MU(I)+C0))*ESPE(I) - PP/DENOM)/DENOM
          DPDM(I) = dPdmu + dPdE(I)*DF(I)*DF(I)*(PP)   !total derivative
          PNEW(I) = PP * OFF(I) ! P(mu[n+1],E[n])
          PNEW(I) = PNEW(I) - PSH(I)
        ENDDO

      ELSEIF(IFLAG == 1) THEN
        MX         = MAT(1)
        RHO0       = PM(01,MX)
        E0         = PM(23,MX)
        A1         = PM(164,MX)
        A2         = PM(32,MX)
        B0         = PM(33,MX)
        B1         = PM(35,MX)
        B2         = PM(36,MX)
        C0         = PM(160,MX)
        C1         = PM(161,MX)
        D0         = PM(162,MX)
        P0         = PM(163,MX)
        PSH(1:NEL) = PM(88,MX)
        DO I=1,NEL
          A2_=A2
          IF(MU(I)<ZERO)A2_=-A2
          DVV      = DVOL(I)*DF(I) / MAX(EM15,VNEW(I))   ! DVOL/V0  car  ESPE =EINT/V0
          DVV      = HALF*DVV ! car 2 iterations
          DENOM   = (ESPE(I)+D0)
          PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))/DENOM
          ESPE(I) = ESPE(I) - (PP)*DVV
          DENOM   = (ESPE(I)+D0)
          PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))/DENOM
          ESPE(I) = ESPE(I) - (PP)*DVV
          PNEW(I) = PP * OFF(I) ! P(mu[n+1],E[n+1])
          EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I))
          PNEW(I) = PNEW(I) - PSH(I)
          dPdE(I) = (((B2*MU(I)+B1)*MU(I)+B0)+(TWO*(C1*MU(I)+C0))*ESPE(I) - PP/DENOM)/DENOM
        ENDDO

      ELSEIF(IFLAG == 2) THEN
        MX           = MAT(1)
        RHO0         = PM(01,MX)
        E0           = PM(23,MX)
        A1           = PM(164,MX)
        A2           = PM(32,MX)
        B0           = PM(33,MX)
        B1           = PM(35,MX)
        B2           = PM(36,MX)
        C0           = PM(160,MX)
        C1           = PM(161,MX)
        D0           = PM(162,MX)
        P0           = PM(163,MX)
        PSH(1:NEL) = PM(88,MX)
        DO I=1, NEL
          IF (VNEW(I) > ZERO) THEN
            A2_=A2
            IF(MU(I)<ZERO)A2_=-A2
            DENOM   = (ESPE(I)+D0)
            PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))/DENOM
            dPdMU   = (A1+2*A2_*MU(I)+(TWO*B2*MU(I)+B1)*ESPE(I)+C1*ESPE(I)*ESPE(I))/DENOM
            dPdE(I) = (((B2*MU(I)+B1)*MU(I)+B0)+(TWO*(C1*MU(I)+C0))*ESPE(I) - PP/DENOM)/DENOM
            DPDM(I) = dPdmu + dPdE(I)*DF(I)*DF(I)*(PP) !total derivative
            PNEW(I) = PP*OFF(I)
            PNEW(I) = PNEW(I) - PSH(I)
          ENDIF
        ENDDO

      ENDIF

C-----------------------------------------------
      RETURN
      END
