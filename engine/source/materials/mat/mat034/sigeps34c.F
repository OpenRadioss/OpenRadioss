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
Chd|  SIGEPS34C                     source/materials/mat/mat034/sigeps34c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS34C(
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC  ,
     2     NPF    , TF     ,TIME   ,TIMESTEP,UPARAM  ,
     3     RHO0   ,THKLY   , GS     ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     A     SOUNDSP,THK     ,UVAR    ,OFF        )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER  :: NEL,NUPARAM, NUVAR
      my_real :: TIME,TIMESTEP
      my_real :: UPARAM(NUPARAM)
      my_real ,DIMENSION(NEL) ::
     .      RHO0  , 
     .      EPSPXX, EPSPYY,EPSPXY, EPSPYZ, EPSPZX,
     .      DEPSXX, DEPSYY, DEPSXY, DEPSYZ, DEPSZX,
     .      EPSXX , EPSYY , EPSXY , EPSYZ , EPSZX,
     .      SIGOXX, SIGOYY,SIGOXY, SIGOYZ, SIGOZX,
     .      GS    ,THKLY ,    THK
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real ,DIMENSION(NEL) ,INTENT(OUT) :: SOUNDSP,
     .    SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real :: UVAR(NEL,NUVAR), OFF(NEL) 
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real
     .        FINTER,TF(*)
      EXTERNAL FINTER
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
       INTEGER :: I, IFORM
       my_real :: BULK,BULK3,G_INS,G_INF,GE,GE2,GV,GV2,C1,C2,BETA,
     .   AA, BB, DAV, P, TRACE, H0(6),H(6),ALPHA,DP_DRHO,CC2
        my_real ,DIMENSION(NEL) :: 
     .   DEPSM,DEPSZZ, SIGNZZ, SIGOZZ, DEPSDXX,DEPSDYY,DEPSDZZ, DP,
     .   DEXX,DEYY,DEZZ,DEXY,DEYZ,DEZX,DDEXX,DDEYY,DDEZZ,DDEXY,DDEYZ,
     .   DDEZX,DEPSDXY,DEPSDYZ,DEPSDZX,EPSPM,
     .   DEPSVXX,DEPSVYY,DEPSVZZ,DEPSVXY,DEPSVYZ,DEPSVZX,EM,EPSM ,EPSZZ,
     .   EPSPZZ 
C=======================================================================
          BULK  = UPARAM(1)     
          G_INS = UPARAM(2)        ! instant shear modulus
          G_INF = UPARAM(3)        ! long term shear modulus
          BETA  = UPARAM(4)
c            
            GE  = G_INF              ! elastic part of shear modulus
            GV  = G_INS-G_INF        ! viscous part of shear modulus
            GE2 = GE * TWO
            GV2 = GV * TWO
            C1  = ONE - EXP(-BETA*TIMESTEP)
            C2  =-C1 / BETA
            BULK3 = BULK*THREE
            CC2 = GV2*(C1 + C2/MAX(EM20,TIMESTEP))
c
            DO I=1,NEL
              !
              ! Computing depzz assuming signzz= 0
              !
               DEZZ(I) = UVAR(I,7)
               H(3)   = UVAR(I,3)
               AA = GV2*C1*(DEZZ(I) - H(3)) + THIRD*(GE2 - CC2 - BULK3)*(DEPSXX(I) + DEPSYY(I)) 
               BB = TWO_THIRD*GE2 + BULK - TWO_THIRD*CC2
               DEPSZZ(I) = AA/BB  
               DDEZZ(I) = TWO_THIRD*DEPSZZ(I) - THIRD*(DEPSXX(I) + DEPSYY(I))
               DEZZ(I)  = UVAR(I,7) + DDEZZ(I)
               EPSZZ(I) = THREE_HALF*DEZZ(I) + HALF*(EPSXX(I)+ EPSYY(I))
               EPSPZZ(I) = DEPSZZ(I)/MAX(EM20,TIMESTEP)
               !!
               DEPSM(I) = THIRD *(DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))
               EPSPM(I) = THIRD *(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))
               EM(I)    = THIRD *(EPSXX(I) +EPSYY(I) +EPSZZ(I) )
               UVAR(I,7) = DEZZ(I)
              ! thickness 
               THK(I) = THK(I) + DEPSZZ(I)*THKLY(I)*OFF(I)
            END DO
c
c           Deviators of strain, strain increment and strain rate       
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

            DP(1:NEL) = BULK3  * DEPSM(1:NEL)

            DO I=1,NEL
              SIGNXX(I) = SIGOXX(I) + GE2*DDEXX(I) - GV2*DEPSVXX(I) + DP(I)
              SIGNYY(I) = SIGOYY(I) + GE2*DDEYY(I) - GV2*DEPSVYY(I) + DP(I)
              !! SIGNZZ(I) =   GE2*DDEZZ(I) - GV2*DEPSVZZ(I) + DP(I) ! must be zero ! only for checking if is needed
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
C=======================================================================
 
c-----------        
      RETURN
      END

