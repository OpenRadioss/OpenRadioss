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
Chd|  SIGEPS34PI                    source/materials/mat/mat034/sigeps34pi.F
Chd|-- called by -----------
Chd|        MULAWP                        source/elements/beam/mulawp.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS34PI(
     1               NEL     ,NUPARAM ,UPARAM  ,IPM     ,MAT     ,
     2               NUVAR   ,UVAR    ,DEPSXX  ,DEPSXY  ,DEPSXZ  ,
     3               EPSXX   ,SIGOXX  ,SIGOXY  ,SIGOXZ  , 
     5               SIGNXX  ,SIGNXY  ,SIGNXZ  ,TIMESTEP,
     6               EPSXY   ,EPSXZ    )      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN) :: IPM
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: MAT
      my_real :: TIMESTEP
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: 
     .   EPSXX,DEPSXX,DEPSXY,DEPSXZ,SIGOXX,SIGOXY,SIGOXZ,
     .   EPSXY,EPSXZ
      my_real ,DIMENSION(*) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: SIGNXX,SIGNXY,SIGNXZ
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT):: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IADBUF
      my_real :: DDEXX,DDEXY,DDEXZ,DEPSDXX,DEXX,DEPSVXX,DEPSVXY,DEPSVXZ,DP
      my_real, DIMENSION(NEL) :: BULK,G_INS,G_INF,GE,GE2,GV,GV2,BETA,C1,C2,
     .      EPSPXX,EPSPXY,EPSPXZ            
C=======================================================================
!!      SHFACT = FIVE_OVER_6
c
      DO I=1,NEL
        IADBUF   = IPM(7,MAT(I))-1
        BULK(I)  = UPARAM(IADBUF+1)
        G_INS(I) = UPARAM(IADBUF+2)
        G_INF(I) = UPARAM(IADBUF+3)
        BETA(I)  = UPARAM(IADBUF+4)
      ENDDO
c        
      GE(1:NEL)  = G_INF(1:NEL)                ! elastic part of shear modulus
      GV(1:NEL)  = G_INS(1:NEL) - G_INF(1:NEL)   ! viscous part of shear modulus
      GE2(1:NEL) = GE(1:NEL) * TWO
      GV2(1:NEL) = GV(1:NEL) * TWO
      C1(1:NEL)  = ONE - EXP(-BETA(1:NEL)*TIMESTEP)
      C2(1:NEL)  =-C1(1:NEL) / BETA(1:NEL)
      EPSPXX(1:NEL) = DEPSXX(1:NEL)/MAX(EM20,TIMESTEP)
      EPSPXY(1:NEL) = DEPSXY(1:NEL)/MAX(EM20,TIMESTEP)
      EPSPXZ(1:NEL) = DEPSXZ(1:NEL)/MAX(EM20,TIMESTEP)
c      
      DO I=1,NEL
        DDEXX = TWO_THIRD*DEPSXX(I)
        DDEXY = DEPSXY(I)
        DDEXZ = DEPSXZ(I)
        
        DEPSDXX = TWO_THIRD*EPSPXX(I)
        DEXX = TWO_THIRD*EPSXX(I)
        !
        DEPSVXX = C1(I)*(DEXX     - UVAR(I,1)) + C2(I)*DEPSDXX
        DEPSVXY = C1(I)*(EPSXY(I) - UVAR(I,2)) + C2(I)*EPSPXY(I)
        DEPSVXZ = C1(I)*(EPSXZ(I) - UVAR(I,3)) + C2(I)*EPSPXZ(I)
        DP  = BULK(I)*DEPSXX(I)
        !!        
        SIGNXX(I) = SIGOXX(I) + GE2(I)*DDEXX - GV2(I)*DEPSVXX + DP
        SIGNXY(I) = SIGOXY(I) + GE(I) *DDEXY - GV(I) *DEPSVXY
        SIGNXZ(I) = SIGOXZ(I) + GE(I) *DDEXZ - GV(I) *DEPSVXZ
        
        UVAR(I,1) = UVAR(I,1) + DEPSVXX + DDEXX
        UVAR(I,2) = UVAR(I,2) + DEPSVXY + DDEXY
        UVAR(I,3) = UVAR(I,3) + DEPSVXZ + DDEXZ
c
      END DO
c-----------
      RETURN
      END
