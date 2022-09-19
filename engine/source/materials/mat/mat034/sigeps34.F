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
Chd|  SIGEPS34                      source/materials/mat/mat034/sigeps34.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS34(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VOLUME , EINT   ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX,
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF                    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR
      my_real
     .      TIME       , TIMESTEP   , UPARAM(NUPARAM),
     .      RHO   (NEL), RHO0  (NEL), VOLUME(NEL), EINT(NEL),
     .      EPSPXX(NEL), EPSPYY(NEL), EPSPZZ(NEL),
     .      EPSPXY(NEL), EPSPYZ(NEL), EPSPZX(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL)
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
      my_real UVAR(NEL,NUVAR), OFF(NEL)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real TF(*)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER :: I
      my_real :: BULK,BULK3,G_INS,G_INF,GE,GE2,GV,GV2,C1,C2,BETA,P0,PHI,
     .   GAMA0,DP_DRHO
      my_real ,DIMENSION(NEL) :: GAMA,DPDGAMA,DP,
     .   DEPSM,EPSPM,EM,DEPSVXX,DEPSVYY,DEPSVZZ,DEPSVXY,DEPSVYZ,DEPSVZX,
     .   DEXX,DEYY,DEZZ,DEXY,DEYZ,DEZX,DEPSDXX,DEPSDYY,DEPSDZZ,
     .   DEPSDXY,DEPSDYZ,DEPSDZX,DDEXX,DDEYY,DDEZZ,DDEXY,DDEYZ,DDEZX
C=======================================================================
      BULK  = UPARAM(1)
      G_INS = UPARAM(2)        ! instant shear modulus
      G_INF = UPARAM(3)        ! long term shear modulus
      BETA  = UPARAM(4)
      P0    = UPARAM(5)
      PHI   = UPARAM(6)
      GAMA0 = UPARAM(7)
c
      GE  = G_INF              ! elastic part of shear modulus
      GV  = G_INS-G_INF        ! viscous part of shear modulus
      GE2 = GE * TWO
      GV2 = GV * TWO
      C1  = ONE - EXP(-BETA*TIMESTEP)
      C2  =-C1 / BETA
      BULK3 = BULK*THREE
c
      DO I=1,NEL
        GAMA(I) = RHO0(I)/RHO(I) - ONE + GAMA0
        DPDGAMA(I)=-P0*(ONE-PHI) / (ONE+GAMA(I)-PHI)
        DEPSM(I) = THIRD *(DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))
        EPSPM(I) = THIRD *(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))
        EM(I)    = THIRD *(EPSXX(I) +EPSYY(I) +EPSZZ(I) )
      END DO
c
c     Deviators of strain, strain increment and strain rate
c
      DO I=1,NEL
        DEXX(I) = EPSXX(I)-EM(I)
        DEYY(I) = EPSYY(I)-EM(I)
        DEZZ(I) = EPSZZ(I)-EM(I)
        DEXY(I) = EPSXY(I)
        DEYZ(I) = EPSYZ(I)
        DEZX(I) = EPSZX(I)
c
        DDEXX(I) = DEPSXX(I)-DEPSM(I)
        DDEYY(I) = DEPSYY(I)-DEPSM(I)
        DDEZZ(I) = DEPSZZ(I)-DEPSM(I)
        DDEXY(I) = DEPSXY(I)
        DDEYZ(I) = DEPSYZ(I)
        DDEZX(I) = DEPSZX(I)
c
        DEPSDXX(I) = EPSPXX(I)-EPSPM(I)
        DEPSDYY(I) = EPSPYY(I)-EPSPM(I)
        DEPSDZZ(I) = EPSPZZ(I)-EPSPM(I)
        DEPSDXY(I) = EPSPXY(I)
        DEPSDYZ(I) = EPSPYZ(I)
        DEPSDZX(I) = EPSPZX(I)
      END DO

      DO I=1,NEL
        DEPSVXX(I) = C1*(DEXX(I)-UVAR(I,1)) + C2*DEPSDXX(I)
        DEPSVYY(I) = C1*(DEYY(I)-UVAR(I,2)) + C2*DEPSDYY(I)
        DEPSVZZ(I) = C1*(DEZZ(I)-UVAR(I,3)) + C2*DEPSDZZ(I)
        DEPSVXY(I) = C1*(DEXY(I)-UVAR(I,4)) + C2*DEPSDXY(I)
        DEPSVYZ(I) = C1*(DEYZ(I)-UVAR(I,5)) + C2*DEPSDYZ(I)
        DEPSVZX(I) = C1*(DEZX(I)-UVAR(I,6)) + C2*DEPSDZX(I)
      END DO

      DP(1:NEL) = (BULK3 - DPDGAMA(1:NEL)) * DEPSM(1:NEL)

      DO I=1,NEL
        SIGNXX(I) = SIGOXX(I) + GE2*DDEXX(I) - GV2*DEPSVXX(I) + DP(I)
        SIGNYY(I) = SIGOYY(I) + GE2*DDEYY(I) - GV2*DEPSVYY(I) + DP(I)
        SIGNZZ(I) = SIGOZZ(I) + GE2*DDEZZ(I) - GV2*DEPSVZZ(I) + DP(I)
        SIGNXY(I) = SIGOXY(I) + GE *DDEXY(I) - GV *DEPSVXY(I)
        SIGNYZ(I) = SIGOYZ(I) + GE *DDEYZ(I) - GV *DEPSVYZ(I)
        SIGNZX(I) = SIGOZX(I) + GE *DDEZX(I) - GV *DEPSVZX(I)
      END DO
c
      DP_DRHO = FOUR_OVER_3*G_INS + BULK
      SOUNDSP(1:NEL) = SQRT(DP_DRHO / RHO0(1:NEL))
c
      DO I=1,NEL
        UVAR(I,1) = UVAR(I,1) + DEPSVXX(I) + DDEXX(I)
        UVAR(I,2) = UVAR(I,2) + DEPSVYY(I) + DDEYY(I)
        UVAR(I,3) = UVAR(I,3) + DEPSVZZ(I) + DDEZZ(I)
        UVAR(I,4) = UVAR(I,4) + DEPSVXY(I) + DDEXY(I)
        UVAR(I,5) = UVAR(I,5) + DEPSVYZ(I) + DDEYZ(I)
        UVAR(I,6) = UVAR(I,6) + DEPSVZX(I) + DDEZX(I)
      END DO
c-----------
      RETURN
      END

