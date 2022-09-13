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
Chd|  SIGEPS104                     source/materials/mat/mat104/sigeps104.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        MAT104_LDAM_NEWTON            source/materials/mat/mat104/mat104_ldam_newton.F
Chd|        MAT104_LDAM_NICE              source/materials/mat/mat104/mat104_ldam_nice.F
Chd|        MAT104_NLDAM_NEWTON           source/materials/mat/mat104/mat104_nldam_newton.F
Chd|        MAT104_NLDAM_NICE             source/materials/mat/mat104/mat104_nldam_nice.F
Chd|        MAT104_NODAM_NEWTON           source/materials/mat/mat104/mat104_nodam_newton.F
Chd|        MAT104_NODAM_NICE             source/materials/mat/mat104/mat104_nodam_nice.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS104(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,LOFF    ,
     3     RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SIGY    ,ET      ,TEMPEL  ,VARNL   ,OFF     ,IPG     ,
     8     DMG     ,TEMP    ,SEQ     ,NPG     ,ELBUF_TAB)
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
      INTEGER NEL,NUPARAM,NUVAR,JTHE,IPG,NPG
      INTEGER ,DIMENSION(NEL), INTENT(IN) :: NGL
      my_real 
     .   TIME,TIMESTEP
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO0,RHO,TEMPEL,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   PLA,DPLA,EPSD,VARNL,LOFF,OFF,DMG,TEMP,SEQ
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IGURSON,NICE,NINDX,I,J,INDX(NEL),IR,IS,IT
C=======================================================================
c
      NICE    = NINT(UPARAM(11)) ! Plastic projection method
                                 !  = 1 => Nice method
                                 !  = 2 => Newton
                                 !  = 3 => Nice method with elastic update
c
      IGURSON = NINT(UPARAM(30)) ! Gurson switch flag: 
                                 !  = 0 => Drucker material law with no damage
                                 !  = 1 => local Gurson damage model
                                 !  = 2 => non local (Forest - micromorphic) damage model
                                 !  = 3 => non local (Peerlings) damage model
c--------------------------                              
      SELECT CASE (IGURSON)
c      
        CASE(0)   ! Drucker material law with no damage
c
c                        
          IF ((NICE == 1).OR.(NICE == 3)) THEN
            CALL MAT104_NODAM_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF    ,
     3         RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,TEMPEL  ,TEMP    ,SEQ     )
          ELSE  ! Newton
            CALL MAT104_NODAM_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF    ,
     3         RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,TEMPEL  ,TEMP    ,SEQ     )
          ENDIF
c
        CASE(1,2)   ! Drucker material law with local Gurson damage
c 
          IF ((NICE == 1).OR.(NICE == 3)) THEN
            CALL MAT104_LDAM_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,LOFF    ,
     3         RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,TEMPEL  ,VARNL   ,DMG     ,TEMP    ,
     8         SEQ     )
          ELSE  ! Newton
            CALL MAT104_LDAM_NEWTON(
     1           NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2           TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,LOFF    ,
     3           RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4           DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5           SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6           SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7           SIGY    ,ET      ,TEMPEL  ,VARNL   ,DMG     ,TEMP    ,
     8           SEQ     )
          ENDIF
c
        CASE(3)   ! Drucker material law with non local (Peerling) damage model
c
          IF ((NICE == 1).OR.(NICE == 3)) THEN
            CALL MAT104_NLDAM_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,LOFF    ,
     3         RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,TEMPEL  ,VARNL   ,DMG     ,TEMP    ,
     8         SEQ     )
          ELSE  ! Newton
            CALL MAT104_NLDAM_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,LOFF    ,
     3         RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,TEMPEL  ,VARNL   ,DMG     ,TEMP    ,
     8         SEQ     )
          ENDIF
c
      END SELECT        
c--------------------------
      ! Solid elements deletion
      IF (IGURSON>0) THEN 
        NINDX = 0
        ! Fully integrated solid elements
        IF (NPG > 1) THEN
          DO I=1,NEL
            !Integration point failure
            IF (LOFF(I) == FOUR_OVER_5) THEN
              IDEL7NOK   = 1
              NINDX      = NINDX+1
              INDX(NINDX)= I
            ENDIF
          ENDDO
          ! Checking full failure of the element
          IF (IPG == NPG) THEN 
            DO I = 1,NEL
              ! Initialization of OFFG
              OFF(I) = ZERO
              ! Loop over integration points
              DO IR = 1, ELBUF_TAB%NPTR
                DO IS = 1, ELBUF_TAB%NPTS
                  DO IT = 1, ELBUF_TAB%NPTT
                    !If one integration points is not fully broken, the brick remains
                    IF (ELBUF_TAB%BUFLY(1)%LBUF(IR,IS,IT)%OFF(I)>ZERO) OFF(I) = ONE
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
          ENDIF
        ! Under-integrated solid element
        ELSE
          DO I=1,NEL
            !Initialization for checking complete failure of the shell (all integration points)
            IF (IPG == 1) THEN
              OFF(I) = ZERO
            ENDIF
            !If one integration points is not fully broken, the brick remains
            IF (LOFF(I)>ZERO) OFF(I) = ONE
            !Integration point failure
            IF (LOFF(I) == FOUR_OVER_5) THEN
              IDEL7NOK   = 1
              NINDX      = NINDX+1
              INDX(NINDX)= I
            ENDIF
          ENDDO
        ENDIF
        IF((NINDX>0).AND.(IMCONV==1))THEN
          DO J=1,NINDX
#include "lockon.inc"
            WRITE(IOUT, 1000) NGL(INDX(J)),IPG
            WRITE(ISTDO,1100) NGL(INDX(J)),IPG,TT
#include "lockoff.inc"
          ENDDO
        ENDIF
      ENDIF
c
 1000 FORMAT(1X,'RUPTURE (GURSON) IN SOLID ELEMENT NUMBER ',I10,1X,',GAUSS PT',I2,1X)
 1100 FORMAT(1X,'RUPTURE (GURSON) IN SOLID ELEMENT NUMBER ',I10,1X,',GAUSS PT',I2,1X,
     .          ' AT TIME :',G11.4)  
c  
c-----------
      RETURN
      END
