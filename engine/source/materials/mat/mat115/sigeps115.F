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
Chd|  SIGEPS115                     source/materials/mat/mat115/sigeps115.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        MAT115_NEWTON                 source/materials/mat/mat115/mat115_newton.F
Chd|        MAT115_NICE                   source/materials/mat/mat115/mat115_nice.F
Chd|====================================================================
      SUBROUTINE SIGEPS115(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,GRHO    ,
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,OFF     ,SIGY    ,
     3     RHO0    ,PLA     ,DPLA    ,SOUNDSP ,ET      ,SEQ     ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  )      
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
      INTEGER NEL,NUPARAM,NUVAR
      INTEGER ,DIMENSION(NEL), INTENT(IN) :: NGL
      my_real 
     .   TIME,TIMESTEP
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(2*NEL), INTENT(IN)     :: 
     .   GRHO
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO0,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   PLA,DPLA,OFF,SEQ
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IRES,NINDX,I,J,INDX(NEL)
      my_real
     .   CFAIL,PFAIL
c=======================================================================
c
      IRES  = NINT(UPARAM(11)) ! Plastic projection method
                               !  = 1 => Nice method
                               !  = 2 => Newton
      CFAIL = UPARAM(14)       ! Tensile volumetric strain at failure
      PFAIL = UPARAM(15)       ! Maximum principal stress at failure
c
c--------------------------                              
      SELECT CASE (IRES)
c      
        CASE(1) 
          CALL MAT115_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,GRHO    , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,OFF     ,SIGY    ,
     3         RHO0    ,PLA     ,DPLA    ,SOUNDSP ,ET      ,SEQ     ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  )
        CASE(2) 
          CALL MAT115_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,GRHO    , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,OFF     ,SIGY    ,
     3         RHO0    ,PLA     ,DPLA    ,SOUNDSP ,ET      ,SEQ     ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  )
c
      END SELECT         
c  
c-----------
C
      ! Counting broken elements
      IF ((CFAIL > ZERO).OR.(PFAIL > ZERO)) THEN
        NINDX = 0
        DO I = 1,NEL
          IF (OFF(I) == QCINQ) THEN
            IDEL7NOK    = 1
            NINDX       = NINDX+1
            INDX(NINDX) = I
          ENDIF
        ENDDO
        ! Printout element deletion
        IF (NINDX > 0) THEN
          DO J=1,NINDX
#include "lockon.inc"
            WRITE(IOUT, 1000) NGL(INDX(J))
            WRITE(ISTDO,1100) NGL(INDX(J)),TT
#include "lockoff.inc"
          ENDDO
        ENDIF
      ENDIF
!---
 1000 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',G11.4)                          
!---
      END
