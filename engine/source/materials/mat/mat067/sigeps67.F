Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SIGEPS67                      source/materials/mat/mat067/sigeps67.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SIGEPS67(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VOLUME , EINT   ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX, 
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF   , DVOL  , PM    )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "param_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL,     NUPARAM, NUVAR

      my_real
     .      TIME         , TIMESTEP   , UPARAM(NUPARAM),
     .      RHO   (NEL), RHO0  (NEL), VOLUME(NEL), EINT(NEL),
     .      EPSPXX(NEL), EPSPYY(NEL), EPSPZZ(NEL),
     .      EPSPXY(NEL), EPSPYZ(NEL), EPSPZX(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),
     .      DVOL(NEL), PM(NPROPM)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), SIGVZZ(NEL),
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SOUNDSP(NEL), VISCMAX(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      UVAR(NEL,NUVAR), OFF(NEL) 
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I, N, NIT, CAS
      my_real
     .    AMU,PEXT,T0,CPA,CPB,CPC,RMW,
     .    T1,T2,T3, E0,EE, H0,
     .    DELTA,T01,T02, VOLINV, RHOCV, POLD
      my_real
     .  P(MVSIZ), TNEW(MVSIZ),
     .  TOLD(MVSIZ), VOLD(MVSIZ),
     .  AA(MVSIZ), BB(MVSIZ), CC(MVSIZ), FF(MVSIZ), DD(MVSIZ),
     .  ENTHA(MVSIZ)
      DATA NIT/3/
C----------------------------------------------------------------

C SET INITIAL MATERIAL CONSTANTS
      PEXT = UPARAM(1)
      IF(TIME==ZERO)THEN
        T0  = UPARAM(2)
        CPA  = PM(186)
        CPB  = PM(187)
        CPC  = PM(188)
        RMW  = PM(189)
        T2=T0*T0
        T3=T0*T2
        H0=CPA*T0+HALF*CPB*T2+THIRD*CPC*T3
        E0=H0-RMW*T0
        DO I=1,NEL
C
C         UVAR(...,1) non utilis 
C
C  Told pour amorce recherche iterative (est rezonne)
          UVAR(I,2)=T0
C
C  pour rezonne ale
          UVAR(I,3)=RHO(I)*CPA
          UVAR(I,4)=RHO(I)*CPB
          UVAR(I,5)=RHO(I)*CPC
          UVAR(I,6)=RHO(I)*RMW
          EINT(I)=RHO(I)*VOLUME(I)*E0
        END DO
      ELSE
        DO I=1,NEL
          VOLINV=ONE/MAX(EM30,VOLUME(I))
          UVAR(I,3)=UVAR(I,3)*VOLINV
          UVAR(I,4)=UVAR(I,4)*VOLINV
          UVAR(I,5)=UVAR(I,5)*VOLINV
          UVAR(I,6)=UVAR(I,6)*VOLINV
        END DO
      END IF
C
C      IF(CPA-RMW<=ZERO)THEN
C      ELSEIF(CPC>ZERO)THEN
C        IF(CPB>ZERO)THEN
C         0 <= T1 <= T2 <= infinite
C          CAS=0
C        ELSE
C         DELTA=CPB*CPB-FOUR*(CPA-RMW)*CPC
C         IF(DELTA>ZERO)THEN
C           DELTA=SQRT(DELTA)
C           T01  =(-CPB-DELTA)/(TWO*CPC)
C           T02  =(-CPB+DELTA)/(TWO*CPC)
C           CAS=2
C           0 <= T1 <= T2 < T01
C           or T02 < T1 <= T2 <= infinite
C         ELSE
C          0 <= T1 <= T2 <= infinite
C           CAS=0
C         END IF
C        END IF
C      ELSEIF(CPC<ZERO)THEN
C        DELTA=SQRT(CPB*CPB-FOUR*(CPA-RMW)*CPC)
C       T01  =(-CPB-DELTA)/(TWO*CPC) is negative
C       T02  =(-CPB+DELTA)/(TWO*CPC) is positive
C        CAS=1
C        T01  =(-CPB+DELTA)/(TWO*CPC)
C       0 <= T1 <= T2 < T01
C      ELSE
C        IF(CPB>ZERO)THEN
C         0 <= T1 <= T2 <= infinite
C          CAS=0
C        ELSEIF(CPB<ZERO)THEN
C          CAS=1
C          T01=-(CPA-RMW)/CPB
C         0 <= T1 <= T2 < T01
C        ELSE
C        END IF
C      END IF
C
C
      DO I=1,NEL
        VOLD(I)= VOLUME(I)-DVOL(I)
        TOLD(I)= UVAR(I,2)
      END DO
C
      DO I=1,NEL
        AA(I)=VOLUME(I)* (UVAR(I,3)-UVAR(I,6))
        BB(I)=VOLUME(I)* UVAR(I,4)
        CC(I)=VOLUME(I)* UVAR(I,5)
        DD(I)=VOLUME(I)* UVAR(I,6)
      END DO
C
C     Temperature de la qte de matiere MM a l'energie Eint
c      DO I=1,NEL
c        POLD=-(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD+PEXT
c        ENTHA(I)=EINT(I)+POLD*VOLD(I)
c      END DO
      DO N=1,NIT
       DO I=1,NEL
         T1=TOLD(I)
         T2=T1*T1
         T3=T1*T2
         TOLD(I)= MAX(ZERO,T1 +
     .    (EINT(I)-(THIRD*CC(I)*T3+HALF*BB(I)*T2+AA(I)*T1))
     .                 /MAX(EM30,CC(I)*T2+BB(I)*T1+AA(I))      )
       END DO
      END DO
C
C     Temperature de la qte de matiere MM a l'energie Eint-PdV
      DO I=1,NEL
        TNEW(I)=TOLD(I)
C
C       Pold dV = MM R/Mw Told dV / Vold 
C               = rhoR/MW Told dV (1+dV/Vold)
        POLD=-(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD+PEXT
        FF(I)  =EINT(I)
     .     -HALF*POLD*DVOL(I)
C    .     -HALF*UVAR(I,6)*TOLD(I)*DVOL(I)*(ONE+DVOL(I)/VOLD(I))
      END DO
      DO N=1,NIT
       DO I=1,NEL
        T1=TNEW(I)
        T2=T1*T1
        T3=T1*T2
C
C       rho V (cva T + 1/2 cpb T2 + 1/3 cpc T3) + 1/2 P dV
C       = Eold - 1/2 Pold dV
C       avec P = rho R/Mw T
        TNEW(I)= MAX(ZERO,T1+
     .   (FF(I)-(THIRD*CC(I)*T3+HALF*BB(I)*T2+AA(I)*T1
     .                            +HALF*UVAR(I,6)*T1*DVOL(I)))
     .      /MAX(EM30,CC(I)*T2+BB(I)*T1+AA(I)+HALF*UVAR(I,6)*DVOL(I)))
C                                                                     12
       END DO
      END DO
C
      DO I=1,NEL
        UVAR(I,2)=TNEW(I)
C Pressure
        P(I)     = UVAR(I,6)*UVAR(I,2)-PEXT
        SIGNXX(I)=-P(I)
        SIGNYY(I)=-P(I)
        SIGNZZ(I)=-P(I)
        SIGNXY(I)=ZERO
        SIGNYZ(I)=ZERO
        SIGNZX(I)=ZERO
C Energy
        T1=TNEW(I)
        T2=T1*T1
        T3=T1*T2
        EINT(I)=THIRD*CC(I)*T3+HALF*BB(I)*T2+AA(I)*T1
* SET SOUND SPEED
C       RHOCV=(UVAR(I,3)-UVAR(I,6))+UVAR(I,4)*T1+UVAR(I,5)*T2
        RHOCV=UVAR(I,3)-UVAR(I,6)
        SOUNDSP(I)=SQRT((ONE+UVAR(I,6)/RHOCV)*(P(I)+PEXT)/RHO(I))
* SET MAXIMUM VISCOSITY
        VISCMAX(I)=ZERO
      END DO

      RETURN
      END

