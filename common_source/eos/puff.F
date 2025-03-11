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
      !||    puff           ../common_source/eos/puff.F
      !||--- called by ------------------------------------------------------
      !||    eosmain        ../common_source/eos/eosmain.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod   ../common_source/modules/constant_mod.F
      !||====================================================================
      SUBROUTINE PUFF(IFLAG,NEL  ,PM   ,OFF  ,EINT ,MU   ,MU2, 
     2                ESPE ,DVOL ,DF   ,VNEW ,MAT  ,
     3                PNEW ,DPDM ,DPDE )
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
       IMPLICIT NONE
#include      "my_real.inc"
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
     .        OFF(NEL) , EINT(NEL), MU(NEL)  , 
     .        MU2(NEL) , ESPE(NEL), DVOL(NEL), DF(NEL)   , 
     .        VNEW(NEL), PNEW(NEL), DPDM(NEL), DPDE(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real AA, BB, DVV, ETA, XX, GX, PRES, CC, EXPA, EE
      my_real C1(NEL),C2(NEL),C3(NEL),T1(NEL),T2(NEL), G0(NEL),ESUBL(NEL),HH(NEL),PC(NEL),PSH(NEL)
C--------------------------------------------------------------------    
      IF(IFLAG == 0) THEN
        DO I=1,NEL
          MX      = MAT(I)
          C1(I)   = PM(32,MX)
          C2(I)   = PM(33,MX)
          C3(I)   = PM(34,MX)
          T1(I)   = PM(35,MX)
          T2(I)   = PM(36,MX)
          PC(I)   = PM(37,MX)
          ESUBL(I)= PM(160,MX)
          G0(I)   = PM(161,MX)
          HH(I)   = PM(162,MX)
          PSH(I)  = PM(88,MX)
        ENDDO
        DO I=1,NEL
          XX =MU(I)/(ONE+MU(I))
          IF(MU(I) >= ZERO) THEN
            AA=(C1(I)+C3(I)*MU2(I))*MU(I)+C2(I)*MU2(I)
            GX=ONE-G0(I)*HALF*XX
            BB=G0(I)
            PRES=MAX(AA*GX+BB*ESPE(I),PC(I))*OFF(I)
            DPDM(I)=(C1(I)+TWO*C2(I)*MU(I)+THREE*C3(I)*MU2(I))*GX+G0(I)*DF(I)*DF(I)*(PRES-HALF*AA)
          ELSEIF(ESPE(I) < ESUBL(I))THEN
            AA=(T1(I)+T2(I)*MU(I))*MU(I)
            GX=ONE-G0(I)*HALF*XX
            BB=G0(I)
            PRES=MAX(AA*GX+BB*ESPE(I),PC(I))*OFF(I)
            DPDM(I)=(T1(I)+TWO*T2(I)*MU(I))*GX+G0(I)*DF(I)*DF(I)*(PRES-HALF*AA)
          ELSE
            ETA=ONE+MU(I)
            EE=SQRT(ETA)
            BB=(HH(I)+(G0(I)-HH(I))*EE)*ETA
            CC= C1(I)/(G0(I)*ESUBL(I))
            EXPA=EXP(CC*XX)
            AA= BB*ESUBL(I)*(EXPA-ONE)
            PRES=MAX(AA+BB*ESPE(I),PC(I))*OFF(I)
            DPDM(I)=BB*DF(I)*DF(I)*(PRES+ESUBL(I)*EXPA*CC) +(ESPE(I)+ESUBL(I)*(EXPA-ONE))*(HH(I)+THREE_HALF*EE*(G0(I)-HH(I)))
          ENDIF
          DPDE(I)=BB
        ENDDO

      ELSEIF(IFLAG == 1) THEN
        DO I=1,NEL
          MX      = MAT(I)
          C1(I)   = PM(32,MX)
          C2(I)   = PM(33,MX)
          C3(I)   = PM(34,MX)
          T1(I)   = PM(35,MX)
          T2(I)   = PM(36,MX)
          PC(I)   = PM(37,MX)
          ESUBL(I)= PM(160,MX)
          G0(I)   = PM(161,MX)
          HH(I)   = PM(162,MX)
          PSH(I)  = PM( 88,MX)
        ENDDO
        DO I=1,NEL
          DVV=HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
          XX =MU(I)/(ONE+MU(I))
          IF(MU(I) >= ZERO) THEN
            AA=(C1(I)+C3(I)*MU2(I))*MU(I)+C2(I)*MU2(I)
            AA=AA*(ONE-G0(I)*HALF*XX)
            BB=G0(I)
          ELSEIF(ESPE(I) < ESUBL(I))THEN
            AA=(T1(I)+T2(I)*MU(I))*MU(I)
            AA=AA*(ONE-G0(I)*HALF*XX)
            BB=G0(I)
          ELSE
            ETA=ONE+MU(I)
            BB=(HH(I)+(G0(I)-HH(I))*SQRT(ETA))*ETA
            CC=C1(I)/(G0(I)*ESUBL(I))
            EXPA=EXP(CC*XX)
            AA=BB*ESUBL(I)*(EXPA-ONE)
          ENDIF
          DPDE(I)=BB
          PNEW(I)=(AA +BB*ESPE(I))/(ONE+ BB*DVV)
          PNEW(I)=MAX(PNEW(I),PC(I))*OFF(I)
          EINT(I)=EINT(I)-HALF*DVOL(I)*PNEW(I)
        ENDDO

      ELSEIF(IFLAG == 2) THEN
        DO I=1, NEL
          MX      = MAT(I)
          C1(I)   = PM(32,MX)
          C2(I)   = PM(33,MX)
          C3(I)   = PM(34,MX)
          T1(I)   = PM(35,MX)
          T2(I)   = PM(36,MX)
          PC(I)   = PM(37,MX)
          ESUBL(I)= PM(160,MX)
          G0(I)   = PM(161,MX)
          HH(I)   = PM(162,MX)
        ENDDO
        DO I=1, NEL
          IF (VNEW(I) > ZERO) THEN
            XX =MU(I)/(ONE+MU(I))
            IF(MU(I) >= ZERO) THEN
               AA=(C1(I)+C3(I)*MU2(I))*MU(I)+C2(I)*MU2(I)
               GX=ONE-G0(I)*HALF*XX
               BB=G0(I)
               PRES=MAX(AA*GX+BB*ESPE(I),PC(I))*OFF(I)
               DPDM(I)=(C1(I)+TWO*C2(I)*MU(I)+THREE*C3(I)*MU2(I))*GX+G0(I)*DF(I)*DF(I)*(PRES-HALF*AA)
            ELSEIF(ESPE(I)<ESUBL(I))THEN
               AA=(T1(I)+T2(I)*MU(I))*MU(I)
               GX=ONE-G0(I)*HALF*XX
               BB=G0(I)
               PRES=MAX(AA*GX+BB*ESPE(I),PC(I))*OFF(I)
               DPDM(I)=(T1(I)+TWO*T2(I)*MU(I))*GX+G0(I)*DF(I)*DF(I)*(PRES-HALF*AA)
            ELSE
               ETA=ONE+MU(I)
               EE=SQRT(ETA)
               BB=(HH(I)+(G0(I)-HH(I))*EE)*ETA
               CC= C1(I)/(G0(I)*ESUBL(I))
               EXPA=EXP(CC*XX)
               AA= BB*ESUBL(I)*(EXPA-ONE)
               PRES=MAX(AA+BB*ESPE(I),PC(I))*OFF(I)
               DPDM(I)=BB*DF(I)*DF(I)*(PRES+ESUBL(I)*EXPA*CC) +
     .              (ESPE(I)+ESUBL(I)*(EXPA-ONE))*(HH(I)+THREE_HALF*EE*(G0(I)-HH(I)))
            ENDIF
            DPDE(I)=BB
            PNEW(I) = PRES
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
