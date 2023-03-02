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
Chd|  SIGEPS120_CONNECT_MAIN        source/materials/mat/mat120/sigeps120_connect_main.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|        SIGEPS120_CONNECT_DP          source/materials/mat/mat120/sigeps120_connect_dp.F
Chd|        SIGEPS120_CONNECT_TAB_DP      source/materials/mat/mat120/sigeps120_connect_tab_dp.F
Chd|        SIGEPS120_CONNECT_TAB_VM      source/materials/mat/mat120/sigeps120_connect_tab_vm.F
Chd|        SIGEPS120_CONNECT_VM          source/materials/mat/mat120/sigeps120_connect_vm.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS120_CONNECT_MAIN(
     1     NEL     ,NGL     ,TIME    ,TIMESTEP,UPARAM  ,OFF     ,
     2     EPSD    ,STIFM   ,JTHE    ,
     3     AREA    ,DEPSZZ  ,DEPSYZ  ,DEPSZX  ,EPSZZ   ,NUPARAM ,
     4     SIGOZZ  ,SIGOYZ  ,SIGOZX  ,SIGNZZ  ,SIGNYZ  ,SIGNZX  ,
     5     PLA     ,JSMS    ,DMELS   ,UVAR    ,NUVAR   ,
     6     NUMTABL ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP  ,TEMP    )   
c====================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc" 
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "sms_c.inc"
C----------------------------------------------------------------
C  D u m m y   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER :: NEL,NUPARAM,NUVAR,JSMS,NUMTABL,NVARTMP,JTHE
      my_real :: TIME,TIMESTEP
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN) :: NGL
      INTEGER ,DIMENSION(NUMTABL) ,INTENT(IN) :: ITABLE
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
      !my_real ,DIMENSION(NEL)     ,INTENT(OUT):: DMG
      my_real ,DIMENSION(NEL)     ,INTENT(IN) :: AREA,TEMP,EPSZZ,
     .         DEPSZZ,DEPSYZ,DEPSZX,
     .         SIGOZZ,SIGOYZ,SIGOZX
      my_real, DIMENSION(NEL) ,INTENT(INOUT) :: EPSD,
     .         SIGNZZ,SIGNYZ,SIGNZX,STIFM,DMELS
      my_real ,DIMENSION(NEL)         ,INTENT(INOUT) :: PLA,OFF
      my_real ,DIMENSION(NEL,NUVAR)   ,INTENT(INOUT) :: UVAR
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      TYPE(TTABLE) ,DIMENSION(NTABLE) ,INTENT(IN)    :: TABLE
c----------------------------------------------------------------
c  L o c a l  v a r ia b l e s
c----------------------------------------------------------------
      INTEGER  :: IFORM,ITABL,I
      my_real :: THICK0
      my_real ,DIMENSION(NEL)    ::    THICK
c=======================================================================
c------------------------------------------------------------------
c--------------------------------
c     Material parameters
c--------------------------------
c     UPARAM(1)  = Young modulus E
c     UPARAM(2)  = Poisson's ratio nu
c     UPARAM(3)  = shear modulus   G
c     UPARAM(4)  = bulk modulus    K
c     UPARAM(5)  = Yld0: instant yield stress 
c     UPARAM(6)  = Q : expontial term coefficient in the hardening law
c     UPARAM(7)  = BETA : exponent of the exponential term 
c     UPARAM(8)  = P : multiplier of the linear term in the hardening law
c     UPARAM(9)  = A1F : parameter of the yield function
c     UPARAM(10) = A2F : parameter of the yield function
c     UPARAM(11) = A1H : distortionnal yield hardening coefficiant
c     UPARAM(12) = A2H : distortionnal yield hardening coefficiant
c     UPARAM(13) = AS : parameter of the potential function
c     UPARAM(14) = D1C: first  damage strain parameter in initial damage threshold
c     UPARAM(15) = D2C: second damage strain parameter in initial damage threshold 
c     UPARAM(16) = D1F: first  damage strain parameter in final damage threshold
c     UPARAM(17) = D2F: second damage strain parameter in final damage threshold
c     UPARAM(18) = D_TRX : triaxiality factor in damage formula
c     UPARAM(19) = D_JC: JC strain rate coefficient in damage formula
c     UPARAM(20) = EXPN exponent in damage evolution
c     UPARAM(21) = CC : Johnson-Cook strain rate-dependency coefficient
c     UPARAM(22) = EPSDREF quasi-static reference strain rate
c     UPARAM(23) = EPSDMAX maximal reference strain rate
c     UPARAM(24) = FCUT : cut frequency for strain rate filtering
c     UPARAM(25) = IFORM = 1: Yield formulation flag  => Drucker-Prager in tension
c                  IFORM = 2: Yield formulation flag  => Von Mises in tension
c     UPARAM(26) = ITRX = 1 : pressure dependent for all T values
c                  ITRX = 2 : no pressure dependency when T < 0
c     UPARAM(27) = IDAM = 1 : damage model without turning point
c                  IDAM = 2 : damage model with turning point
c     UPARAM(28) = SSP  : sound speed
c     UPARAM(29) = Table Id
c     UPARAM(30) = Xscale for yld function
c     UPARAM(31) = Yscale for yld function
C-----------
      IFORM = NINT(UPARAM(25))
      ITABL = NINT(UPARAM(29))
C-----------
      THICK0   = UPARAM(34)     
      DO I=1,NEL 
        THICK(I) =  THICK0 + EPSZZ(I)
      ENDDO
C-----------
      IF (ITABL > 0) THEN
        IF (IFORM == 1) THEN     ! tabulated Drucker-Prager formulation
          CALL SIGEPS120_CONNECT_TAB_DP(
     1     NEL     ,NGL     ,TIME    ,TIMESTEP,UPARAM  ,OFF     ,
     2     EPSD    ,STIFM   ,THICK   ,JTHE    ,
     3     AREA    ,DEPSZZ  ,DEPSYZ  ,DEPSZX  ,NUPARAM ,
     4     SIGOZZ  ,SIGOYZ  ,SIGOZX  ,SIGNZZ  ,SIGNYZ  ,SIGNZX  ,
     5     PLA     ,JSMS    ,DMELS   ,UVAR    ,NUVAR   ,
     6     NUMTABL ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP  ,TEMP    )

        ELSE                     ! tabulated Von-Mises formulation
          CALL SIGEPS120_CONNECT_TAB_VM(
     1     NEL     ,NGL     ,TIME    ,TIMESTEP,UPARAM  ,OFF     ,
     2     EPSD    ,STIFM   ,THICK   ,JTHE    ,
     3     AREA    ,DEPSZZ  ,DEPSYZ  ,DEPSZX  ,NUPARAM ,
     4     SIGOZZ  ,SIGOYZ  ,SIGOZX  ,SIGNZZ  ,SIGNYZ  ,SIGNZX  ,
     5     PLA     ,JSMS    ,DMELS   ,UVAR    ,NUVAR   ,
     6     NUMTABL ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP  ,TEMP    )
        END IF


      ELSE
c
        IF (IFORM == 1) THEN    ! analytical Drucker-Prager formulation
          CALL SIGEPS120_CONNECT_DP(
     1     NEL     ,NGL     ,TIME    ,TIMESTEP,UPARAM  ,OFF     ,
     2     EPSD    ,STIFM   ,THICK   ,
     3     AREA    ,DEPSZZ  ,DEPSYZ  ,DEPSZX  ,NUPARAM ,
     4     SIGOZZ  ,SIGOYZ  ,SIGOZX  ,SIGNZZ  ,SIGNYZ  ,SIGNZX  ,
     5     PLA     ,JSMS    ,DMELS   ,UVAR    ,NUVAR   )

        ELSE                     ! analytical Von-Mises formulation
         CALL SIGEPS120_CONNECT_VM(
     1     NEL     ,NGL     ,TIME    ,TIMESTEP,UPARAM  ,OFF     ,
     2     EPSD    ,STIFM   ,THICK,
     3     AREA    ,DEPSZZ  ,DEPSYZ  ,DEPSZX  ,NUPARAM ,
     4     SIGOZZ  ,SIGOYZ  ,SIGOZX  ,SIGNZZ  ,SIGNYZ  ,SIGNZX  ,
     5     PLA     ,JSMS    ,DMELS   ,UVAR    ,NUVAR   )
        END IF
      END IF


C-----------
      RETURN
      END
