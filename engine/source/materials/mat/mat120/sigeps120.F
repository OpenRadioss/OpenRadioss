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
Chd|  SIGEPS120                     source/materials/mat/mat120/sigeps120.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        SIGEPS120_DP                  source/materials/mat/mat120/sigeps120_dp.F
Chd|        SIGEPS120_TAB_DP              source/materials/mat/mat120/sigeps120_tab_dp.F
Chd|        SIGEPS120_TAB_VM              source/materials/mat/mat120/sigeps120_tab_vm.F
Chd|        SIGEPS120_VM                  source/materials/mat/mat120/sigeps120_vm.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS120(
     1           NEL     ,NUPARAM ,NUVAR   ,NVARTMP ,NUMTABL ,ITABLE  ,
     2           TABLE   ,TIME    ,TIMESTEP,SOUNDSP ,UVAR    ,VARTMP  ,
     3           UPARAM  ,NGL     ,OFF     ,PLA     ,EPSD    ,TEMP    ,
     4           EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     5           DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     6           SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7           SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8           JTHE    ,INLOC   ,DPLANL  )
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
C----------------------------------------------------------------
C  D u m m y   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER :: NEL,NUPARAM,NUVAR,NVARTMP,NUMTABL,JTHE,INLOC
      my_real :: TIME,TIMESTEP
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN) :: NGL
      INTEGER ,DIMENSION(NUMTABL) ,INTENT(IN) :: ITABLE
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL)     ,INTENT(IN) :: TEMP,
     .         EPSPXX,EPSPYY,EPSPZZ,EPSPXY,EPSPYZ,EPSPZX,
     .         DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .         SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
      my_real, DIMENSION(NEL) ,INTENT(INOUT) :: EPSD,SOUNDSP,
     .         SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL)         ,INTENT(INOUT) :: PLA,DPLANL,OFF
      my_real ,DIMENSION(NEL,NUVAR)   ,INTENT(INOUT) :: UVAR
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      TYPE(TTABLE) ,DIMENSION(NTABLE) ,INTENT(IN)    :: TABLE
c----------------------------------------------------------------
c  L o c a l  v a r ia b l e s
c----------------------------------------------------------------
      INTEGER  :: IFORM,ITABL
C=======================================================================
      IFORM = NINT(UPARAM(25))
      ITABL = NINT(UPARAM(29))
c
c------------------------------------------------------------------
      IF (ITABL > 0) THEN
        IF (IFORM == 1) THEN     ! tabulated Drucker-Prager formulation
          CALL SIGEPS120_TAB_DP(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2         UPARAM  ,UVAR    ,OFF     ,PLA     ,EPSD    ,SOUNDSP ,
     3         EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         NUMTABL ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP  ,TEMP    ,
     8         JTHE    ,INLOC   ,DPLANL  )
        ELSE                     ! tabulated Von-Mises formulation
          CALL SIGEPS120_TAB_VM(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2         UPARAM  ,UVAR    ,OFF     ,PLA     ,EPSD    ,SOUNDSP ,
     3         EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         NTABLE  ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP  ,TEMP    ,
     8         JTHE    ,INLOC   ,DPLANL  )
        END IF
c
      ELSE
c
        IF (IFORM == 1) THEN    ! analytical Drucker-Prager formulation
          CALL SIGEPS120_DP(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2         UPARAM  ,UVAR    ,OFF     ,PLA     ,EPSD    ,SOUNDSP ,
     3         EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         INLOC   ,DPLANL  )
        ELSE                     ! analytical Von-Mises formulation
          CALL SIGEPS120_VM(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2         UPARAM  ,UVAR    ,OFF     ,PLA     ,EPSD    ,SOUNDSP ,
     3         EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         INLOC   ,DPLANL  )
        END IF
      END IF
c-----------------------------------------------------------------
      RETURN
      END
