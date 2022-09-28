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
Chd|  SIGEPS119C                    source/materials/mat/mat119/sigeps119c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        LAW119_MEMBRANE               source/materials/mat/mat119/law119_membrane.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS119C(
     .            NEL     ,IPT     ,NPT     ,NUPARAM ,NUVAR   ,
     .            NUMTABL ,ITABLE  ,TABLE   ,UVAR    ,UPARAM  ,
     .            THK     ,THKLYL  ,SHF     ,SSP     ,OFF     ,
     .            IQEPH   ,ZCFAC   ,DEPSXX  ,DEPSYY  ,DEPSXY  ,
     .            EPSXX   ,EPSYY   ,EPSXY   ,EPSYZ   ,EPSZX   ,
     .            SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     .            SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  )
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,IPT,NPT,NUPARAM,NUVAR,NUMTABL
      INTEGER ,DIMENSION(NUMTABL),INTENT(IN)   :: ITABLE
      LOGICAL ,INTENT(IN) :: IQEPH
      my_real ,DIMENSION(MVSIZ,2) ,INTENT(INOUT) :: ZCFAC
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN)  :: UPARAM
      my_real ,DIMENSION(NEL) ,INTENT(IN) ::  SHF,THKLYL,OFF,
     .   DEPSXX,DEPSYY,DEPSXY,EPSXX,EPSYY,EPSXY,EPSYZ,EPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX
      my_real ,DIMENSION(NEL) ,INTENT(OUT) ::
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,SSP
      my_real, DIMENSION(NEL), INTENT(INOUT) :: THK
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, ICOAT
      my_real :: NU,G12,GS,GC,A1C,A2C,TCOAT,DEZZ
      my_real ,DIMENSION(NEL) :: ET,THFAC
C=======================================================================
      NU      = UPARAM(5)
      G12     = UPARAM(6)
      A1C     = UPARAM(14)
      A2C     = UPARAM(15)
      TCOAT   = UPARAM(16)
      SSP     = UPARAM(17)
      GC      = UPARAM(20)
      GS      = G12*SHF(1)
      ICOAT   = 0
      IF (A1C > ZERO .and. NPT == 3 .and. (IPT == 1 .or.  IPT == NPT)) ICOAT = 1
c-------------------------------------------------------------------------
      IF (ICOAT == 1) THEN ! external coating
        IF (TCOAT == ZERO) THEN
          THFAC(1:NEL) = ONE
        ELSE
          THFAC(1:NEL) = TCOAT / THKLYL(1:NEL)
        END IF
        DO I=1,NEL
          SIGNXX(I) = (A1C*EPSXX(I) + A2C*EPSYY(I))*THFAC(I)
          SIGNYY(I) = (A2C*EPSXX(I) + A1C*EPSYY(I))*THFAC(I)
          SIGNXY(I) = GC*EPSXY(I)*THFAC(I)
          SIGNYZ(I) = GC*EPSYZ(I)*THFAC(I)*SHF(1)
          SIGNZX(I) = GC*EPSZX(I)*THFAC(I)*SHF(1)
        END DO
      ELSE
        CALL LAW119_MEMBRANE(
     .       NEL    ,NUPARAM,NUVAR  ,UPARAM ,UVAR   ,
     .       GS     ,ET     ,DEPSXX ,DEPSYY ,DEPSXY ,
     .       EPSXX  ,EPSYY  ,EPSXY  ,EPSYZ  ,EPSZX  ,
     .       SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX ,
     .       SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX ,
     .       NUMTABL,ITABLE ,TABLE  )
      END IF
c------------------------------------            
c       thickness update:
c------------------------------------            
      DO I=1,NEL
        DEZZ = -(DEPSXX(I)+DEPSYY(I))*NU/(ONE-NU)
        THK(I) = THK(I) + DEZZ * THKLYL(I)*OFF(I)
      ENDDO
C
C------for QEPH-------       
      IF (IQEPH) THEN
        DO I=1,NEL
          ZCFAC(I,1) = ZCFAC(I,1) + ET(I) / NPT
          ZCFAC(I,2) = MIN(ET(I),ZCFAC(I,2))
        ENDDO
      ENDIF
c-----------
      RETURN
      END
