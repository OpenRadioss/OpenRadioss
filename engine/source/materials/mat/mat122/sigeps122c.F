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
!||    sigeps122c       ../engine/source/materials/mat/mat122/sigeps122c.F
!||--- called by ------------------------------------------------------
!||    mulawc           ../engine/source/materials/mat_share/mulawc.F90
!||--- calls      -----------------------------------------------------
!||    mat122c_newton   ../engine/source/materials/mat/mat122/mat122c_newton.F
!||    mat122c_nice     ../engine/source/materials/mat/mat122/mat122c_nice.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod     ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE SIGEPS122C(
     1     NEL     ,NUPARAM ,NUVAR   ,UPARAM  ,UVAR    ,  
     2     EPSXX   ,EPSYY   ,RHO     ,PLA     ,DPLA    ,
     3     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     4     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     5     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     6     THK     ,THKLY   ,OFF     ,SIGY    ,ETSE    ,
     7     DMG     ,SEQ     ,SHF     ,SOUNDSP ,ASRATE  ,
     8     EPSD_PG ,NFUNC   ,IFUNC   ,NPF     ,TF      ,
     9     NVARTMP ,VARTMP  ,IOFF_DUCT)
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
      my_real, INTENT(IN) :: ASRATE
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
      my_real ,DIMENSION(NEL), INTENT(IN)    :: EPSD_PG
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: PLA,THK,OFF,SEQ,DPLA
      my_real ,DIMENSION(NEL,6), INTENT(INOUT) :: DMG
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      INTEGER ,DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: IOFF_DUCT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IRES,I
      my_real ,DIMENSION(NEL) :: EPSD
C=======================================================================
c
      IRES = NINT(UPARAM(17)) ! Plastic projection method
                              !  = 1 => Nice method
                              !  = 2 => Cutting plane
                              
      ! strain rate filtering, using global element strain rate
      do i=1,nel
        epsd(i) = asrate*epsd_pg(i) + (one-asrate)*uvar(i,18)
        uvar(i,18) = epsd(i)
      end do
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
     7         DMG     ,SEQ     ,EPSD    ,SHF     ,SOUNDSP ,
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
     7         DMG     ,SEQ     ,EPSD    ,SHF     ,SOUNDSP ,
     8         NFUNC   ,IFUNC   ,NPF     ,TF      ,NVARTMP ,
     9         VARTMP  )
    
      END SELECT     
c
      ! Ductile failure activation
      IOFF_DUCT(1:NEL) = 1  
c  
c-----------
      END
