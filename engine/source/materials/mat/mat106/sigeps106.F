Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  SIGEPS106                     source/materials/mat/mat106/sigeps106.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        JC                            source/materials/mat/mat106/sigeps106.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
       SUBROUTINE SIGEPS106(
     A      NEL    , NUPARAM, NUVAR  , NFUNC   , IFUNC   ,
     B      NPF    , TF     , TIME   , TIMESTEP, UPARAM  ,
     C      RHO0   , RHO    , VOLUME , EINT    ,
     D      DEPSXX , DEPSYY , DEPSZZ , DEPSXY  , DEPSYZ  , DEPSZX,
     E      EPSXX  , EPSYY  , EPSZZ  , EPSXY   , EPSYZ   , EPSZX ,
     F      SIGOXX , SIGOYY , SIGOZZ , SIGOXY  , SIGOYZ  , SIGOZX,
     G      SIGNXX , SIGNYY , SIGNZZ , SIGNXY  , SIGNYZ  , SIGNZX,
     H      SIGVXX , SIGVYY , SIGVZZ , SIGVXY  , SIGVYZ  , SIGVZX,
     I      SOUNDSP, VISCMAX, UVAR   , OFF     , PLA     , DPLA  ,
     J      EPSD   , TEMP   , JTHE   , NFT     )
C------------------------------------------------------------------------
C     Radial return for Johnson-Cook without viscous effect
C     R = (A+B*p^n)*(1-Tr^m)    
C--------------------------------------------------
C     Johnson-Cook without viscous effect
C     sigma = (A+B*p^n)*(1-Tr^m) with Tr=(T-Tref)/(Tm-Tref)
C     p      - cumulated plastic strain
C     epsm   - numerical set max value for plastic strain
C     sigmam - numerical set max value for stress
C--------------------------------------------------
C     Be careful, 
C     
C     EPSXX ... are updates strain values = eps^{n+1}
C     with eps_xx^{n+1} = eps_xx^{n} + deps_xx ...

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n 
C-----------------------------------------------
C
C----------------------------------------------------------------
C  I n p u t   A r g u m e n t s
C----------------------------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR, JTHE, NFT
      my_real
     .   TIME       , TIMESTEP   , UPARAM(NUPARAM),
     .   RHO(NEL)   , RHO0(NEL)  , VOLUME(NEL), EINT(NEL),
     .   DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL), DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .   EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL), EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .   SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL), SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),
     .   OFF(NEL)   , PLA(NEL)   , DPLA(NEL)  , EPSD(NEL)  , TEMP(NEL)
C----------------------------------------------------------------
C  O u t p u t   A r g u m e n t s
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), SIGVZZ(NEL),
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SOUNDSP(NEL), VISCMAX(NEL)
C----------------------------------------------------------------
C  I n p u t  O u t p u t   A r g u m e n t s
C----------------------------------------------------------------
      my_real :: UVAR(NEL,NUVAR)
C----------------------------------------------------------------------
C  V a r i a b l e s  f o r  f u n c t i o n  i n t e r p o l a t i o n 
C----------------------------------------------------------------------
      INTEGER :: NPF(*), NFUNC, IFUNC(NFUNC)
      my_real :: TF(*)
C----------------------------------------------------------------
C  L o c a l  V a r i a b l e s
C----------------------------------------------------------------
      INTEGER I,J,NMAX,N, ITER, ITER_MAX
      my_real
     .     E,NU,G,A,B,CM,CN,TM,TREF,EPSM,SIGMAM,
     .     MU,MU0,LAMBDA0,MU_RATIO,BULK,LAMBDA,
     .     F,TOL,INTERVAL,E0, NU0,
     .     SXX,SYY,SZZ,SXY,SYZ,SZX,EQVM,TRC,RATIO,
     .     SIGXX,SIGYY,SIGZZ,SIGXY,SIGYZ,SIGZX,
     .     EXX,EYY,EZZ,EXY,EYZ,EZX,TRACE_DEPS,TRACE_SIG, PRESSURE, PRESSURE0,
     .     V0,V1,FUN,DFUN, SCALE, CS, ERROR, JC_VAL, DJC_VAL, INCR, ALPHA,
     .     XX, DXX, NORM_FUN0, ERROR1, ERROR2, NORM_V0,
     .     DEPSP_XX, DEPSP_YY, DEPSP_ZZ, DEPSP_XY, DEPSP_YZ, DEPSP_ZX, TRCEPS
      my_real
     .     FINTER,DYDX
      LOGICAL :: CONVERGED
      EXTERNAL FINTER
C-----------------------------------------------
C     PARAMETERS READING
C-----------------------------------------------
         E  = UPARAM(1)
         NU = UPARAM(2)
         A  = UPARAM(3)
         B  = UPARAM(4)
         CM = UPARAM(5)
         CN = UPARAM(6)
         TM = UPARAM(7)
         TREF = UPARAM(8)
         EPSM   = UPARAM(11)
         SIGMAM = UPARAM(12)
         NMAX   = UPARAM(13)
         TOL    = UPARAM(14)
         CS     = UPARAM(15)
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
         IF(TIMESTEP == ZERO)THEN
            DO I=1,NEL
              UVAR(I,1)=ZERO  ! accumulated plastic strain
              UVAR(I,2)=ZERO  ! JC yield stress
              UVAR(I,3)=ZERO  ! Tangent of yield stress
              UVAR(I,4)=ZERO  ! Plastic strain XX
              UVAR(I,5)=ZERO  ! Plastic strain YY
              UVAR(I,6)=ZERO  ! Plastic strain ZZ
              UVAR(I,7)=ZERO  ! Plastic strain XY
              UVAR(I,8)=ZERO  ! Plastic strain YZ
              UVAR(I,9)=ZERO  ! Plastic strain ZX
C         Temperature dependent material properties
              IF (IFUNC(1) > 0) THEN
                  E =  UPARAM(1)*FINTER(IFUNC(1),TEMP(I),NPF,TF,DYDX)
              ENDIF
              IF (IFUNC(3) > 0) THEN
                  NU = UPARAM(2)*FINTER(IFUNC(3),TEMP(I),NPF,TF,DYDX)
              ENDIF
              UVAR(I,10)=E   ! Previous Young's modulus
              UVAR(I,11)=NU  ! Previous Poisson's ratio
              UVAR(I,12)=TEMP(I)  ! Previous temperature
              PLA(I) = ZERO
            ENDDO
         ENDIF

         INTERVAL=ONE
         DO I=1,NEL
C         Temperature dependent material properties
            IF (IFUNC(1) > 0) THEN
                IF (IFUNC(2) <= 0 .OR. TEMP(I) > UVAR(I,12)) THEN
                    E = UPARAM(1)*FINTER(IFUNC(1),TEMP(I),NPF,TF,DYDX)
                ELSE
                    E = UPARAM(1)*FINTER(IFUNC(2),TEMP(I),NPF,TF,DYDX)
                ENDIF
            ENDIF
            IF (IFUNC(3) > 0) THEN
              NU = UPARAM(2)*FINTER(IFUNC(3),TEMP(I),NPF,TF,DYDX)
            ENDIF
C         Important, values newly added element should be initialized
            IF (UVAR(I,10) == ZERO) THEN
               UVAR(I,10) = E
            ENDIF
            IF (UVAR(I,11) == ZERO) THEN
               UVAR(I,11) = NU
            ENDIF
            IF (UVAR(I,12) == ZERO) THEN
               IF(JTHE >=0) THEN
                  UVAR(I,12) = TREF
               ELSE
                  UVAR(I,12) = TEMP(I)
               ENDIF
            ENDIF

C     E and NU at previous time 
            NU0 = UVAR(I, 11)
            E0 = UVAR(I, 10)

            MU = HALF * E / (ONE + NU)
            MU0 = HALF * E0 / (ONE + NU0)
            MU_RATIO = MU / MU0
            LAMBDA = E * NU / (ONE + NU) / (ONE - TWO * NU)
            LAMBDA0 = E0 * NU0 / (ONE + NU0) / (ONE - TWO * NU0)
            BULK = THIRD * E / (ONE - TWO * NU)

C       Sound speed = sqrt((lambda+2*mu)/density)
            SOUNDSP(I) = SQRT((TWO*MU+LAMBDA)/RHO(I))
            VISCMAX(I) = ZERO

C       Deviatoric stress at previous time step
            TRC = SIGOXX(I) + SIGOYY(I) + SIGOZZ(I)
            PRESSURE0 = - THIRD * TRC
            SXX = SIGOXX(I) + PRESSURE0
            SYY = SIGOYY(I) + PRESSURE0
            SZZ = SIGOZZ(I) + PRESSURE0
            SXY = SIGOXY(I)
            SYZ = SIGOYZ(I)
            SZX = SIGOZX(I)

C       Deviatoric strain increment
            TRCEPS = DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)
            EXX = DEPSXX(I) - THIRD * TRCEPS
            EYY = DEPSYY(I) - THIRD * TRCEPS
            EZZ = DEPSZZ(I) - THIRD * TRCEPS
            EXY = DEPSXY(I)
            EYZ = DEPSYZ(I)
            EZX = DEPSZX(I)

C       Deviatoric elastic prediction at current time step
            SXX = (SXX * MU_RATIO + TWO * MU * EXX)
            SYY = (SYY * MU_RATIO + TWO * MU * EYY)
            SZZ = (SZZ * MU_RATIO + TWO * MU * EZZ)
            SXY = (SXY * MU_RATIO + MU * EXY)
            SYZ = (SYZ * MU_RATIO + MU * EYZ)
            SZX = (SZX * MU_RATIO + MU * EZX)
C     Trace of the updated strain tensor
            TRC = EPSXX(I) + EPSYY(I) + EPSZZ(I)
C     Pressure at time n+1 does not depend on plastic strain ...
C     a direct computation leads to: 
            PRESSURE = - BULK * TRC
C=======================================================================
C     II - JC yield stress
C=======================================================================
            CALL JC(UVAR(I,1),TEMP(I),A,B,CM,CN,TREF,TM,EPSM,SIGMAM,UVAR(I,2),UVAR(I,3))
            EQVM = SQRT(THREE_HALF*(SXX**2+SYY**2+SZZ**2+TWO*(SXY**2+SYZ**2+SZX**2)))
            F = EQVM - UVAR(I,2)

            UVAR(I, 13) = ZERO
            UVAR(I, 14) = ZERO

            IF (F <= ZERO) THEN
C=======================================================================
C     Case 1: pure elastic
C=======================================================================
              DPLA(I) = ZERO
              RATIO = ONE
            ELSEIF(TEMP(I) >= TM) THEN
C=======================================================================
C     Case 2: pure hydrodynamic
C=======================================================================
              DPLA(I) = ZERO
              RATIO = ZERO
            ELSE
C=======================================================================
C     Case 3: elastoplastic
C       Newton-Raphson to solve:
C       eqVM - 3*mu*dp - R(p0+dp)=0
C       to get dp
C=======================================================================
C     Initial condition for Newton method
               V0 = EQVM / (THREE * MU)
               CALL JC(UVAR(I,1)+V0,TEMP(I),A,B,CM,CN,TREF,TM,EPSM,SIGMAM,JC_VAL,DJC_VAL)
               FUN = EQVM - THREE * MU * V0 - JC_VAL
               NORM_FUN0 = ABS(FUN)
               NORM_V0 = ABS(V0)
               CONVERGED = .FALSE.
               ITER = 0
               ITER_MAX = NMAX
               DO WHILE (.NOT. CONVERGED .AND. ITER <= ITER_MAX)
                  CALL JC(UVAR(I,1)+V0,TEMP(I),A,B,CM,CN,TREF,TM,EPSM,SIGMAM,JC_VAL,DJC_VAL)
                  FUN = EQVM - THREE * MU * V0 - JC_VAL
                  ERROR1 = ABS(FUN)
                  CONVERGED = ERROR1 < TOL * NORM_FUN0
                  IF (.NOT. CONVERGED) THEN
                     DFUN = -THREE * MU - DJC_VAL
                     INCR = - FUN / DFUN
                     ALPHA = ONE
                     IF (INCR < ZERO) THEN
                        ALPHA =MIN (ONE , - NINE / TEN * V0 / INCR)
                     ENDIF
                     V0 = V0 + ALPHA * INCR
                     ERROR2 = ABS(ALPHA * INCR)
                     CONVERGED = ERROR2 < TOL * ABS(V0)
                  ENDIF
                  ITER = ITER + 1
               ENDDO

               UVAR(I, 13) = ITER
               UVAR(I, 14) = ERROR2

              DPLA(I) = V0
              UVAR(I,1) = UVAR(I,1) + V0
              CALL JC(UVAR(I,1),TEMP(I),A,B,CM,CN,TREF,TM,EPSM,SIGMAM,UVAR(I,2),UVAR(I,3))
              SCALE=THREE_HALF*V0/EQVM
              UVAR(I,4) = UVAR(I,4) + SCALE * SXX
              UVAR(I,5) = UVAR(I,5) + SCALE * SYY
              UVAR(I,6) = UVAR(I,6) + SCALE * SZZ
              UVAR(I,7) = UVAR(I,7) + SCALE * SXY
              UVAR(I,8) = UVAR(I,8) + SCALE * SYZ
              UVAR(I,9) = UVAR(I,9) + SCALE * SZX

              UVAR(I,2)=UVAR(I,2)
              UVAR(I,3)=UVAR(I,3)
              RATIO = (ONE - THREE * MU * V0 / EQVM)

              IF(JTHE >=0 ) TEMP(I)=UVAR(I,12)+UVAR(I,2)*DPLA(I)/CS			
            ENDIF

            UVAR(I,10) = E  ! register as previous Young's modulus
            UVAR(I,11) = NU  ! register as previous Poisson's ratio
            UVAR(I,12) = TEMP(I)  ! register as previous temperature
            SIGNXX(I) = SXX * RATIO - PRESSURE
            SIGNYY(I) = SYY * RATIO - PRESSURE
            SIGNZZ(I) = SZZ * RATIO - PRESSURE
            SIGNXY(I) = SXY * RATIO
            SIGNYZ(I) = SYZ * RATIO
            SIGNZX(I) = SZX * RATIO
         ENDDO
C
         PLA(1:NEL) = UVAR(1:NEL,1)

         DO I=1,NEL
           VISCMAX(I)= ZERO
           SIGVXX(I) = ZERO
           SIGVYY(I) = ZERO
           SIGVZZ(I) = ZERO
           SIGVXY(I) = ZERO
           SIGVYZ(I) = ZERO
           SIGVZX(I) = ZERO
         ENDDO

      RETURN
      END

Chd|====================================================================
Chd|  JC                            source/materials/mat/mat106/sigeps106.F
Chd|-- called by -----------
Chd|        SIGEPS106                     source/materials/mat/mat106/sigeps106.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE JC(P,T,A,B,CM,CN,TREF,TM,EPSM,SIGMAM,JC_YIELD,TAN_JC)
C--------------------------------------------------
C     Johnson-Cook and tangent without viscous effect
C--------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          my_real, INTENT(IN) :: P,T,A,B,CM,CN
          my_real, INTENT(IN) :: TREF,TM,EPSM,SIGMAM
          my_real, INTENT(OUT) :: JC_YIELD, TAN_JC
          my_real :: TR
C---------------------------------------------------
          TR=MAX(T,TREF)
          TR=MIN(TR,TM)
          TR=(TR-TREF)/(TM-TREF)
          IF (TM == ZERO) THEN
              TR = ZERO
          ENDIF
          IF(P<=ZERO) THEN
              JC_YIELD=A*(ONE-TR**CM)
              TAN_JC=ZERO
          ELSEIF(P>EPSM) THEN
              JC_YIELD=(A+B*EPSM**CN)*(ONE-TR**CM)
              TAN_JC=ZERO
          ELSE
              JC_YIELD=(A+B*P**CN)*(ONE-TR**CM)
              TAN_JC=B*CN*P**(CN-ONE)*(ONE-TR**CM)
          ENDIF
          JC_YIELD=MIN(SIGMAM,JC_YIELD)
          RETURN
      END SUBROUTINE JC
