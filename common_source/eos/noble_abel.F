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
!||    noble_abel   ../common_source/eos/noble_abel.F
!||--- called by ------------------------------------------------------
!||    eosmain      ../common_source/eos/eosmain.F
!||====================================================================
      SUBROUTINE NOBLE_ABEL(IFLAG,NEL, PM    ,OFF  ,EINT ,MU   ,
     2                      ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                      PNEW ,DPDM ,DPDE )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of NOBLE-ABEL EOS
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
C
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
     .        VNEW(NEL) ,PSH(NEL)  ,PNEW(NEL) ,DPDM(NEL),
     .        DPDE(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real GM1(NEL), DVV, PP,BB_(NEL),RHO0(NEL)
      my_real DENOM,BB
C-----------------------------------------------
      IF(IFLAG == 0) THEN
        DO I=1,NEL
          MX      = MAT(I)
          BB_(I)  = PM(32,MX)
          GM1(I)  = PM(33,MX)
          RHO0(I) = PM(34,MX)
          PSH(I)  = PM(88,MX)
        ENDDO
        DO I=1,NEL
          DENOM   = ONE-BB_(I)*RHO0(I)*(ONE+MU(I))
          PP      = ESPE(I)*GM1(I)*(MU(I)+ONE)/DENOM
          DPDE(I) = GM1(I)*(ONE+MU(I))/DENOM
          DPDM(I) = GM1(I)*ESPE(I)/DENOM + PP/DENOM*BB_(I)*RHO0(I) + PP*DF(I)*DF(I)*DPDE(I)
          PNEW(I) = MAX(PP,ZERO)*OFF(I)! P(mu[n+1],E[n])
          PNEW(I) = PNEW(I)-PSH(I)
        ENDDO

      ELSEIF(IFLAG == 1) THEN
        DO I=1,NEL
          MX      = MAT(I)
          BB_(I)  = PM(32,MX)
          GM1(I)  = PM(33,MX)
          RHO0(I) = PM(34,MX)
          PSH(I)  = PM(88,MX)
        ENDDO
        DO I=1,NEL
          DENOM   = ONE-BB_(I)*RHO0(I)*(ONE+MU(I))
          BB      = GM1(I)*(ONE+MU(I))/DENOM
          DPDE(I) = BB
          DVV     = HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
          PNEW(I) = (BB*ESPE(I))/(ONE+BB*DVV)
          PNEW(I) = PNEW(I)*OFF(I)! P(mu[n+1],E[n])
          EINT(I) = EINT(I) - HALF*DVOL(I)*PNEW(I)
          PNEW(I) = PNEW(I)-PSH(I)
        ENDDO

      ELSEIF(IFLAG == 2) THEN
        DO I=1, NEL
          MX      = MAT(I)
          BB_(I)  = PM(32,MX)
          GM1(I)  = PM(33,MX)
          RHO0(I) = PM(34,MX)
          PSH(I)  = PM(88,MX)
        ENDDO
        DO I=1, NEL
          IF (VNEW(I) > ZERO) THEN
            DENOM   = ONE-BB_(I)*RHO0(I)*(ONE+MU(I))
            PP      = ESPE(I)*GM1(I)*(MU(I)+ONE)/DENOM
            DPDE(I) = GM1(I)*(ONE+MU(I))/DENOM
            DPDM(I) = GM1(I)*ESPE(I)/DENOM + PP/DENOM*BB_(I)*RHO0(I) + PP*DF(I)*DF(I)*DPDE(I)
            PNEW(I) = PP-PSH(I)
          ENDIF
        ENDDO
      ENDIF
      RETURN
      END
