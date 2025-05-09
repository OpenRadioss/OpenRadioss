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
      !||    stiffgas   ../common_source/eos/stiffgas.F
      !||--- called by ------------------------------------------------------
      !||    eosmain    ../common_source/eos/eosmain.F
      !||====================================================================
      SUBROUTINE STIFFGAS
     1                    (IFLAG,NEL, PM    ,OFF  ,EINT ,MU   ,MU2 ,
     2                     ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                     PNEW ,DPDM ,DPDE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of STIFFENED-GAS EOS
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
     .        MU2(NEL)  ,ESPE(NEL) ,DVOL(NEL) ,DF(NEL)  , 
     .        VNEW(NEL) ,PNEW(NEL) ,DPDM(NEL),
     .        DPDE(NEL)
      my_real, INTENT(INOUT) :: PSH(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real :: P0,GAMMA,T0,E0,SPH,AA, BB, DVV, PP, PSTAR, PC
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(IFLAG == 0) THEN
        MX         = MAT(1)
        E0         = PM(23,MX)
        GAMMA      = PM(34,MX)
        P0         = PM(32,MX)
        PSH(1:NEL) = PM(88,MX)
        PSTAR      = PM(35,MX)
        SPH        = PM(69,MX)
        PC         = PM(37,MX)
        DO I=1,NEL
          PP      = -GAMMA*PSTAR-PSH(I) + (GAMMA-ONE)*(ONE+MU(I))*ESPE(I)
          DPDM(I) =  (GAMMA-ONE) *ESPE(I)+(GAMMA-ONE)*(ONE+MU(I))*DF(I)*DF(I)*(PP+PSH(I) )
          DPDE(I) = (GAMMA-ONE)*(ONE+MU(I))
          PNEW(I) = MAX(PP,PC-PSH(I))*OFF(I) ! P(mu[n+1],E[n])
        ENDDO

      ELSEIF(IFLAG == 1) THEN
        MX         = MAT(1)
        E0         = PM(23,MX)
        GAMMA      = PM(34,MX)
        P0         = PM(32,MX)
        PSH(1:NEL) = PM(88,MX)
        PSTAR      = PM(35,MX)
        SPH        = PM(69,MX)
        PC         = PM(37,MX)
        DO I=1,NEL
          AA      = -GAMMA*PSTAR-PSH(I)
          BB      = (GAMMA-ONE)*(ONE+MU(I))
          DPDE(I) = (GAMMA-ONE)*(ONE+MU(I))
          DVV     = HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
          PNEW(I) = (AA+BB*(ESPE(I)-PSH(I) *DVV))/(ONE+BB*DVV)
          PNEW(I) = MAX(PNEW(I),PC-PSH(I) )*OFF(I)  ! P(mu[n+1],E[n+1])
          EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I)+PSH(I) )
        ENDDO

      ELSEIF(IFLAG == 2) THEN
        MX           = MAT(1)
        E0           = PM(23,MX)
        GAMMA        = PM(34,MX)
        P0           = PM(32,MX)
        PSH(1:NEL) = PM(88,MX)
        PSTAR        = PM(35,MX)
        SPH          = PM(69,MX)
        PC = PM(37, MX)
        DO I=1, NEL
          IF (VNEW(I) > ZERO) THEN
            PNEW(I) = (GAMMA-ONE)*(ONE+MU(I))*ESPE(I) - GAMMA*PSTAR
            PNEW(I) = MAX(PNEW(I),PC)*OFF(I)
            !EINT(I) = (PNEW(I) + PSH(I) + GAMMA * PSTAR) / (GAMMA - ONE)
            DPDM(I) = (GAMMA-ONE)*(ESPE(I)+PNEW(I)*DF(I)) !total derivative
            DPDE(I) = (GAMMA-ONE)*(ONE+MU(I)) !partial derivative
            PNEW(I) = PNEW(I)-PSH(I)
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
