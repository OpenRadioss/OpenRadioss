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
!||    sigeps112             ../engine/source/materials/mat/mat112/sigeps112.F
!||--- called by ------------------------------------------------------
!||    mulaw                 ../engine/source/materials/mat_share/mulaw.F90
!||--- calls      -----------------------------------------------------
!||    mat112_xia_newton     ../engine/source/materials/mat/mat112/mat112_xia_newton.F
!||    mat112_xia_nice       ../engine/source/materials/mat/mat112/mat112_xia_nice.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    table_mod             ../engine/share/modules/table_mod.F
!||====================================================================
      SUBROUTINE SIGEPS112(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2     UPARAM  ,UVAR    ,JTHE    ,OFF     ,RHO0    ,RHO     ,
     3     PLA     ,DPLA    ,EPSD    ,SOUNDSP ,EPSZZ   ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SIGY    ,ET      ,
     8     NVARTMP ,NUMTABL ,VARTMP  ,ITABLE  ,TABLE   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE ELBUFDEF_MOD     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include      "com04_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,JTHE,NUMTABL,NVARTMP,
     .        ITABLE(NUMTABL)
      INTEGER ,DIMENSION(NEL), INTENT(IN) :: NGL
      my_real 
     .   TIME,TIMESTEP
      INTEGER :: VARTMP(NEL,NVARTMP)
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO0,RHO,EPSZZ,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   DPLA,OFF
      my_real ,DIMENSION(NEL,4), INTENT(INOUT)       :: 
     .   PLA,EPSD
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
c
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IRES
C=======================================================================
      IRES  = NINT(UPARAM(13)) ! Plastic projection method
                               !  = 1 => Nice method
                               !  = 2 => Newton-iteration method
C=======================================================================
        ! Choice of return mapping algorithm
        SELECT CASE (IRES)
          ! Nice explicit return mapping      
          CASE(1)        
            CALL MAT112_XIA_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2         UPARAM  ,UVAR    ,JTHE    ,OFF     ,RHO0    ,RHO     ,
     3         PLA     ,DPLA    ,EPSD    ,SOUNDSP ,EPSZZ   ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,
     8         NVARTMP ,NUMTABL ,VARTMP  ,ITABLE  ,TABLE   )     
          ! Newton cutting plane return mapping
          CASE(2)
            CALL MAT112_XIA_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2         UPARAM  ,UVAR    ,JTHE    ,OFF     ,RHO0    ,RHO     ,
     3         PLA     ,DPLA    ,EPSD    ,SOUNDSP ,EPSZZ   ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,
     8         NVARTMP ,NUMTABL ,VARTMP  ,ITABLE  ,TABLE   )
        END SELECT        
c-----------
      END
