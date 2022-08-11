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
Chd|  SIGEPS107C                    source/materials/mat/mat107/sigeps107c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        MAT107C_NEWTON                source/materials/mat/mat107/mat107c_newton.F
Chd|        MAT107C_NICE                  source/materials/mat/mat107/mat107c_nice.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS107C(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2     TF      ,TIMESTEP,TIME    ,UPARAM  ,UVAR    ,JTHE    ,RHO     ,
     3     PLA     ,DPLA    ,SOUNDSP ,EPSD    ,OFF     ,
     4     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,SHF     ,
     5     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,SIGY    ,     
     6     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,ET      ,NUMTABL ,
     7     ITABLE  ,TABLE   ,NVARTMP ,VARTMP  )
C     
      !=======================================================================
      !      Modules
      !=======================================================================
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
#include      "param_c.inc" 
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "mvsiz_p.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,NFUNC,JTHE,NUMTABL,NVARTMP
      INTEGER NPF(*),NGL(NEL),IFUNC(NFUNC),ITABLE(NUMTABL)
      my_real 
     .   TIMESTEP,TIME,TF(*),UPARAM(NUPARAM)
      INTEGER :: VARTMP(NEL,NVARTMP)
      my_real,DIMENSION(NEL), INTENT(IN) :: 
     .   RHO,DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,
     .   SHF
c
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
c
      my_real ,DIMENSION(NEL) :: 
     .   SIGY,ET
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: 
     .   OFF,DPLA
      my_real ,DIMENSION(NEL,6),INTENT(INOUT) :: 
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
c=======================================================================           
        ! Choice of return mapping algorithm
        SELECT CASE (IRES)
          ! Nice explicit return mapping      
          CASE(1)        
            CALL MAT107C_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2         UPARAM  ,UVAR    ,JTHE    ,OFF     ,RHO     ,
     3         PLA     ,DPLA    ,EPSD    ,SOUNDSP ,SHF     ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,
     8         NUMTABL ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP  )        
          ! Newton cutting plane return mapping
          CASE(2)
            CALL MAT107C_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2         UPARAM  ,UVAR    ,JTHE    ,OFF     ,RHO     ,
     3         PLA     ,DPLA    ,EPSD    ,SOUNDSP ,SHF     ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,
     8         NUMTABL ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP  )
        END SELECT
c-----------
      END
