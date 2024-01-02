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
Chd|  SIGEPS79                      source/materials/mat/mat079/sigeps79.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SIGEPS79(
     1      NEL      ,NUPARAM  ,NUVAR    ,TIME     ,TIMESTEP ,UPARAM   ,                                                                                                          
     2      RHO0     ,RHO      ,NGL      ,SIGY     ,DPLA     ,DEFP     ,
     3      DEPSXX   ,DEPSYY   ,DEPSZZ   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     4      SIGOXX   ,SIGOYY   ,SIGOZZ   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     5      SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     6      EPSD     ,DMG      ,SOUNDSP  ,UVAR     ,OFF      ,AMU      ,
     7      IDEL7NOK )      
C
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "param_c.inc"
#include "comlock.inc"
#include "units_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER,INTENT(IN) :: 
     .      NEL,NUPARAM,NUVAR,NGL(NEL)
      INTEGER, INTENT(INOUT) :: 
     .      IDEL7NOK 
      my_real,INTENT(IN) :: 
     .      TIME,TIMESTEP,UPARAM(NUPARAM),RHO0(NEL),RHO(NEL),
     .      DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .      DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .      SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .      SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .      EPSD(NEL),AMU(NEL)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real,INTENT(OUT) ::
     .      SIGY(NEL),DPLA(NEL),SOUNDSP(NEL),
     .      SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .      SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real,INTENT(INOUT) :: 
     .      UVAR(NEL,NUVAR),OFF(NEL),DMG(NEL),
     .      DEFP(NEL)
C----------------------------------------------------------------
C  L O C A L  V A R I A B L E S
C----------------------------------------------------------------
      INTEGER I,J,NINDX,INDX(NEL),IDEL
      my_real
     .   G     , G2   , AA    , BB     , MM   ,
     .   NN    , CC   , EPS0  , SIGFMAX,
     .   TSTAR , PHEL , SHEL  , BETA   ,
     .   D1    , D2   , EPSMAX, K1     , K2     , K3
      my_real
     .   MU(NEL),MU2(NEL),POLD(NEL),VM(NEL),
     .   DELTAP(NEL),PNEW(NEL),PSTAR(NEL),SCALE(NEL), 
     .   SIGYI(NEL),SIGYF(NEL),SIGYOLD(NEL),DMG_OLD(NEL)
      my_real
     .   DAV, CE, SIGSTAR, EPFAIL, P1, YIELD, DELTAU, DPDMU,
     .   PMIN, RATIO, J2
C
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      G       = UPARAM(1)
      G2      = UPARAM(2)
      AA      = UPARAM(3)
      BB      = UPARAM(4)
      MM      = UPARAM(5)
      NN      = UPARAM(6)
      CC      = UPARAM(7)
      EPS0    = UPARAM(8)
      SIGFMAX = UPARAM(9)
      TSTAR   = UPARAM(10)
      PHEL    = UPARAM(11)
      SHEL    = UPARAM(12)
      D1      = UPARAM(13)
      D2      = UPARAM(14)
      K1      = UPARAM(15)
      K2      = UPARAM(16)
      K3      = UPARAM(17)
      BETA    = UPARAM(18)
      IDEL    = NINT(UPARAM(19))
      EPSMAX  = UPARAM(20)
c
      ! Recovering internal variables
      DO I=1,NEL
        IF (OFF(I) < EM01) OFF(I) = ZERO
        IF (OFF(I) <  ONE) OFF(I) = OFF(I)*FOUR_OVER_5
        DELTAP(I)  = UVAR(I,1)
        SIGYOLD(I) = UVAR(I,2)/SHEL
        DMG_OLD(I) = DMG(I)
        MU(I)      = AMU(I)
        MU2(I)     = MU(I)*MU(I)
      ENDDO
c
      !========================================================================
      ! - COMPUTATION OF ELASTIC DEVIATORIC STRESSES AND EQUIVALENT STRESS
      !========================================================================
      DO I=1,NEL
        DAV       = (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))*THIRD
        POLD(I)   = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD
        SIGNXX(I) = SIGOXX(I)+POLD(I)+G2*(DEPSXX(I)-DAV)
        SIGNYY(I) = SIGOYY(I)+POLD(I)+G2*(DEPSYY(I)-DAV)
        SIGNZZ(I) = SIGOZZ(I)+POLD(I)+G2*(DEPSZZ(I)-DAV)
        SIGNXY(I) = SIGOXY(I)+G*DEPSXY(I)
        SIGNYZ(I) = SIGOYZ(I)+G*DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I)+G*DEPSZX(I)
        J2        = HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     .               +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
        VM(I)     = SQRT(THREE*J2)
      ENDDO
c
      !========================================================================
      ! - COMPUTATION OF THE PRESSURE
      !========================================================================
      DO I=1,NEL
        PNEW(I) = K1*MU(I) + DELTAP(I)
        IF (MU(I) > ZERO) THEN
          PNEW(I) = PNEW(I) + K2*MU2(I) + K3*MU2(I)*MU(I)
        ELSEIF (IDEL /= 1) THEN
          PMIN=-TSTAR*PHEL*(ONE-DMG(I))
          PNEW(I)=MAX(PNEW(I),PMIN)
        ENDIF
        PSTAR(I) = PNEW(I)/PHEL
      ENDDO
c
      !========================================================================
      ! - COMPUTATION OF THE YIELD STRESS
      !========================================================================
      DO I=1,NEL
        IF (NN == ZERO) THEN
          SIGYI(I) = AA
        ELSEIF ((PSTAR(I)+TSTAR) > ZERO) THEN
          SIGYI(I) = AA*(PSTAR(I)+TSTAR)**NN
        ELSE
          SIGYI(I) = ZERO
        ENDIF
C
        IF (MM == ZERO) THEN
          SIGYF(I) = BB
        ELSEIF (PSTAR(I) > ZERO) THEN
          SIGYF(I) = BB*(PSTAR(I))**MM
        ELSE
          SIGYF(I) = ZERO
        ENDIF
C
        IF (EPSD(I)<=EPS0) THEN
          CE = ONE
        ELSE
          CE = ONE + CC*LOG(EPSD(I)/EPS0)
        ENDIF
C
         SIGYI(I) = CE*SIGYI(I)
         SIGYF(I) = CE*SIGYF(I)
         SIGYF(I) = MIN(SIGYF(I),SIGFMAX)
         SIGY(I)  = (ONE-DMG(I))*SIGYI(I)+DMG(I)*SIGYF(I)
      ENDDO
c
      !========================================================================
      ! - RADIAL RETURN
      !========================================================================
      DO I=1,NEL
        IF (OFF(I) == ONE) THEN 
          SIGSTAR = VM(I)/SHEL
          IF (SIGSTAR < SIGY(I)) THEN
            SCALE(I) = ONE
          ELSEIF (VM(I) > ZERO) THEN
            SCALE(I) = SIGY(I)/SIGSTAR
          ELSE
            SCALE(I) = ZERO
          ENDIF
          SIGNXX(I) = SCALE(I)*SIGNXX(I)
          SIGNYY(I) = SCALE(I)*SIGNYY(I)
          SIGNZZ(I) = SCALE(I)*SIGNZZ(I)
          SIGNXY(I) = SCALE(I)*SIGNXY(I)
          SIGNYZ(I) = SCALE(I)*SIGNYZ(I)
          SIGNZX(I) = SCALE(I)*SIGNZX(I)
        ENDIF
      ENDDO
c
      !========================================================================
      ! - UPDATE PLASTIC STRAIN AND DAMAGE
      !========================================================================
      NINDX = 0
      INDX(1:NEL) = 0
      DO I=1,NEL
        IF (OFF(I) == ONE) THEN 
c
          ! Compute plastic strain at failure
          IF (D2 == ZERO) THEN
            EPFAIL = D1
          ELSEIF ((PSTAR(I)+TSTAR) >= ZERO) THEN
            EPFAIL = D1*(PSTAR(I)+TSTAR)**D2
          ELSE
            EPFAIL = ZERO
          ENDIF
c
          ! Update plastic strain and damage
          ! -> When damage parameters are defined, progressive failure + plastic strain
          IF (EPFAIL > ZERO) THEN 
            DPLA(I) = (ONE - SCALE(I))*VM(I)/(THREE*SQRT(THREE)*G)
            DEFP(I) = DEFP(I) + DPLA(I)  
            DMG(I)  = DMG(I)  + DPLA(I)/EPFAIL
            DMG(I)  = MIN(DMG(I),ONE)
          ! -> When damage parameters are not defined, no plastic strain and instant failure
          ELSEIF (SCALE(I)<ONE) THEN 
            DMG(I)  = ONE
          ENDIF
c 
          ! Check element deletion
          IF (IDEL == 1) THEN 
            IF ((PSTAR(I)+TSTAR) < ZERO) THEN 
              OFF(I) = FOUR_OVER_5
              NINDX  = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF 
          ELSEIF (IDEL == 2) THEN 
            IF (DEFP(I) > EPSMAX) THEN 
              OFF(I) = FOUR_OVER_5
              NINDX  = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF
          ELSEIF (IDEL == 3) THEN 
            IF (DMG(I) == ONE) THEN 
              OFF(I) = FOUR_OVER_5
              NINDX  = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF            
          ENDIF
        ENDIF
      ENDDO    
c  
      !========================================================================
      ! - COMPUTE PRESSURE INCREMENT
      !========================================================================
      DO I=1,NEL
        IF ((DMG(I) > DMG_OLD(I)).AND.(MU(I) > ZERO).AND.(OFF(I) == ONE)) THEN     
          P1     = K1*MU(I)
          YIELD  = (ONE-DMG(I))*SIGYI(I)+DMG(I)*SIGYF(I)
          DELTAU = (SIGYOLD(I)*SIGYOLD(I)-YIELD*YIELD)/(SIX*G)
          IF (DELTAU > ZERO) THEN
            DELTAU = DELTAU*SHEL*SHEL
            DELTAP(I) = -P1+
     .               SQRT((DELTAP(I)+P1)**2+TWO*BETA*K1*DELTAU)
          ENDIF
        ENDIF
      ENDDO
C
      !========================================================================
      ! - UPDATE STRESS TENSOR AND SOUND SPEED
      !========================================================================
      DO I=1,NEL
        UVAR(I,1)  = DELTAP(I)
        UVAR(I,2)  = SIGY(I)*SHEL
        SIGNXX(I)  = SIGNXX(I)-PNEW(I)
        SIGNYY(I)  = SIGNYY(I)-PNEW(I)
        SIGNZZ(I)  = SIGNZZ(I)-PNEW(I)
        IF (MU(I) > ZERO) THEN 
          DPDMU    = K1+TWO*K2*MU(I)+THREE*K3*MU2(I)
        ELSE 
          DPDMU    = K1
        ENDIF
        SOUNDSP(I) = SQRT((DPDMU+FOUR_OVER_3*G)/RHO0(I))
      ENDDO
C
      !========================================================================
      ! - PRINT OUT ELEMENT DELETION DATA
      !========================================================================
      IF (NINDX>0)THEN
        DO J=1,NINDX
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDX(J)),TIME
          WRITE(ISTDO,1000) NGL(INDX(J)),TIME
#include "lockoff.inc"
        ENDDO
      ENDIF
C
 1000 FORMAT(1X,'-- RUPTURE (J-HOLMQUIST) OF SOLID ELEMENT :',I10,' AT TIME :',1PE12.4)     
C
      END
