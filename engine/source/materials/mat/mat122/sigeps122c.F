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
      SUBROUTINE SIGEPS122C(
     1     NEL     ,NUPARAM ,NUVAR   ,UPARAM  ,UVAR    ,  
     2     EPSXX   ,EPSYY   ,RHO     ,PLA     ,DPLA    ,
     3     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     4     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     5     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     6     THK     ,THKLY   ,OFF     ,SIGY    ,ETSE    ,
     7     DMG     ,SEQ     ,SHF     ,SOUNDSP ,
     8     EPSPL   ,NFUNC   ,IFUNC   ,NPF     ,TF      ,
     9     NVARTMP ,VARTMP  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,
     .   NFUNC,IFUNC(NFUNC),NPF(SNPC),NVARTMP
      my_real, INTENT(IN) ::
     .   UPARAM(NUPARAM),TF(STF)
      my_real,DIMENSION(NEL), INTENT(IN) :: 
     .   RHO,EPSXX,EPSYY,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,
     .   THKLY,SHF
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,
     .   SIGY,ETSE
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: 
     .   PLA,THK,OFF,DMG,SEQ,DPLA,EPSPL
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      INTEGER ,DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IRES
C=======================================================================
c
      IRES = NINT(UPARAM(17)) ! Plastic projection method
                              !  = 1 => Nice method
                              !  = 2 => Cutting plane
c--------------------------                        
      SELECT CASE (IRES)
c      
        CASE(1)
c     
          CALL MAT122C_NICE(
     1         NEL     ,NUPARAM ,NUVAR   ,UPARAM  ,UVAR    ,
     2         EPSXX   ,EPSYY   ,RHO     ,PLA     ,DPLA    ,
     3         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     4         SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     5         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     6         THK     ,THKLY   ,OFF     ,SIGY    ,ETSE    ,
     7         DMG     ,SEQ     ,EPSPL   ,SHF     ,SOUNDSP ,
     8         NFUNC   ,IFUNC   ,NPF     ,TF      ,NVARTMP ,
     9         VARTMP  )
c      
        CASE(2)
c
          CALL MAT122C_NEWTON(
     1         NEL     ,NUPARAM ,NUVAR   ,UPARAM  ,UVAR    ,        
     2         EPSXX   ,EPSYY   ,RHO     ,PLA     ,DPLA    ,
     3         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     4         SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     5         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     6         THK     ,THKLY   ,OFF     ,SIGY    ,ETSE    ,
     7         DMG     ,SEQ     ,EPSPL   ,SHF     ,SOUNDSP ,
     8         NFUNC   ,IFUNC   ,NPF     ,TF      ,NVARTMP ,
     9         VARTMP  )
    
      END SELECT        
c  
c-----------
      END
