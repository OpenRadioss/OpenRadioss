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
Chd|====================================================================
Chd|  SIGEPS102                     source/materials/mat/mat102/sigeps102.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SIGEPS102(
     1      NEL    , NUPARAM, NUVAR  , UPARAM  , RHO0    , RHO    , 
     2      DEPSXX , DEPSYY , DEPSZZ , DEPSXY  , DEPSYZ  , DEPSZX ,
     3      SIGOXX , SIGOYY , SIGOZZ , SIGOXY  , SIGOYZ  , SIGOZX ,
     4      SIGNXX , SIGNYY , SIGNZZ , SIGNXY  , SIGNYZ  , SIGNZX ,
     5      SOUNDSP, UVAR   , OFF    , ET      ,
     6      PSH    , PNEW   , DPDM   , SSP     , PLA     )   
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER NEL,NVARF,NUPARAM,NUVAR
      my_real :: TIME,TIMESTEP   
      my_real :: UPARAM(NUPARAM)
      my_real ,DIMENSION(NEL) :: RHO, RHO0,
     .   DEPSXX, DEPSYY, DEPSZZ, DEPSXY, DEPSYZ, DEPSZX,
     .   SIGOXX, SIGOYY, SIGOZZ, SIGOXY, SIGOYZ, SIGOZX,
     .   SSP   , DPDM  , PNEW, PSH, PLA
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SOUNDSP(NEL), ET(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      UVAR(NEL,NUVAR), OFF(NEL) ,MU(NEL),MU2(NEL)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      my_real :: A0,A1,A2,AMAX
      my_real :: DAV,POLD(NEL)
      my_real :: T1(NEL),T2(NEL),T3(NEL),T4(NEL),T5(NEL),T6(NEL)
      my_real :: PTOT,G0(NEL),RATIO(NEL),YIELD2(NEL)
      my_real :: PSTAR,G,GG,SCRT(NEL),AJ2(NEL),DPLA(NEL)
      my_real :: I3(NEL),COS3T(NEL),SQRT_J2,THETA,C,PHI,K
      integer :: I,IFORM
C----------------------------------------------------------------
C  S o u r c e   L i n e s
C----------------------------------------------------------------
      C         = UPARAM(1)
      PHI       = UPARAM(2)
      PSTAR     = UPARAM(3)
      A0        = UPARAM(4)
      A1        = UPARAM(5)
      A2        = UPARAM(6)
      AMAX      = UPARAM(7) 
      G         = UPARAM(8)
      GG        = TWO*G
      IFORM     = NINT(UPARAM(9))
      !----------------------------------------------------------------!
      !  STATE INIT.                                                   !
      !----------------------------------------------------------------!        
      DO I=1,NEL
        POLD(I) = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD 
        SCRT(I) =  (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))*THIRD 
        MU(I)   = RHO(I)/RHO0(I) - ONE
        MU2(I)  = MU(I) * MAX(ZERO,MU(I))
      ENDDO !next I    
      !----------------------------------------------------------------!
      !  TEMPORARY DEVIATORIC STRESS TENSOR : T(1:6)                   !
      !----------------------------------------------------------------!  
      DO I=1,NEL
        T1(I)=SIGOXX(I)+POLD(I)+GG*(DEPSXX(I)-SCRT(I))
        T2(I)=SIGOYY(I)+POLD(I)+GG*(DEPSYY(I)-SCRT(I))
        T3(I)=SIGOZZ(I)+POLD(I)+GG*(DEPSZZ(I)-SCRT(I))
        T4(I)=SIGOXY(I)        + G*DEPSXY(I)
        T5(I)=SIGOYZ(I)        + G*DEPSYZ(I)
        T6(I)=SIGOZX(I)        + G*DEPSZX(I)
      ENDDO !next I  
      !----------------------------------------------------------------!
      !  SOUND SPEED                                                   !
      !----------------------------------------------------------------!      
      DO I=1,NEL
        DPDM(I) = DPDM(I) + ONEP333*G
        SSP(I)  = SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO !next I      
      !----------------------------------------------------------------!
      !  YIELD SURFACE                                                 !
      !----------------------------------------------------------------!     
      DO I=1,NEL
        AJ2(I)= HALF*(T1(I)**2+T2(I)**2+T3(I)**2)+T4(I)**2+T5(I)**2+T6(I)**2
      ENDDO
      !----SUBCASE --- ORIGINAL MOHR COULOMB
      IF(IFORM==4)THEN
        K = ONE/SQRT(THREE)
        DO I=1,NEL
          I3(I)    = T2(I)*T3(I)*T1(I)-T2(I)*T6(I)*T6(I)-T3(I)*T4(I)*T4(I)-T5(I)*T5(I)*T1(I)+2*T5(I)*T4(I)*T6(I)
          SQRT_J2  = SQRT(AJ2(I))
          COS3T(I) = NINE*I3(I)/TWO/SQRT(THREE)/SQRT_J2/SQRT_J2/SQRT_J2
          THETA    = ACOS(MAX(ZERO,MIN(ONE,COS3T(I))))
          PTOT     = PNEW(I)+PSH(I)
          G0(I)    = -PTOT*SIN(PHI)+SQRT_J2*(COS(THETA)-K*SIN(THETA)*SIN(PHI))-C*COS(PHI)
          G0(I)    = MAX(ZERO,G0(I))
          YIELD2(I)= AJ2(I)-G0(I)
        ENDDO
      !----SUBCASE --- FITTED DRUCKER PRAGER FROM MOHR COULOMB PARAMETERS (A0,A1,A2 CALCULATED DURING STARTER)
      ELSE
        DO I=1,NEL
          PTOT     = PNEW(I)+PSH(I)
          G0(I)    = A0 +A1 *PTOT+A2 *PTOT*PTOT
          G0(I)    = MIN(AMAX,G0(I))
          G0(I)    = MAX(ZERO,G0(I))
          IF(PTOT <= PSTAR)G0(I)=ZERO
          YIELD2(I)=AJ2(I)-G0(I)
        ENDDO !next I           
      ENDIF 

      !----------------------------------------------------------------!
      !  PROJECTION FACTOR ON YIELD SURFACE                            !
      !----------------------------------------------------------------!      
      DO  I=1,NEL
        RATIO(I)=ZERO
        IF(YIELD2(I)<=ZERO .AND. G0(I)>ZERO)THEN
          RATIO(I)=ONE
        ELSE
          RATIO(I)=SQRT(G0(I)/(AJ2(I)+ EM14))
        ENDIF
      ENDDO !next I 
      !----------------------------------------------------------------!
      !  UPDATE DEVIATORIC STRESS TENSOR IN SIG(:,:)                   !
      !----------------------------------------------------------------!      
      DO I=1,NEL
        SIGNXX(I)=RATIO(I)*T1(I)*OFF(I)
        SIGNYY(I)=RATIO(I)*T2(I)*OFF(I)
        SIGNZZ(I)=RATIO(I)*T3(I)*OFF(I)
        SIGNXY(I)=RATIO(I)*T4(I)*OFF(I)
        SIGNYZ(I)=RATIO(I)*T5(I)*OFF(I)
        SIGNZX(I)=RATIO(I)*T6(I)*OFF(I)
        DPLA(I)  =(ONE -RATIO(I))*SQRT(AJ2(I)) / MAX(EM20,THREE*G)
      ENDDO !next I     
      PLA(1:NEL) = PLA(1:NEL) + DPLA(1:NEL)
c-----------         
      RETURN
      END
