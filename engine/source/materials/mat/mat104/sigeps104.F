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
!||    sigeps104             ../engine/source/materials/mat/mat104/sigeps104.F
!||--- called by ------------------------------------------------------
!||    mulaw                 ../engine/source/materials/mat_share/mulaw.F90
!||--- calls      -----------------------------------------------------
!||    mat104_ldam_newton    ../engine/source/materials/mat/mat104/mat104_ldam_newton.F
!||    mat104_ldam_nice      ../engine/source/materials/mat/mat104/mat104_ldam_nice.F
!||    mat104_nldam_newton   ../engine/source/materials/mat/mat104/mat104_nldam_newton.F
!||    mat104_nldam_nice     ../engine/source/materials/mat/mat104/mat104_nldam_nice.F
!||    mat104_nodam_newton   ../engine/source/materials/mat/mat104/mat104_nodam_newton.F
!||    mat104_nodam_nice     ../engine/source/materials/mat/mat104/mat104_nodam_nice.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE SIGEPS104(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,NPG     ,UELR    , 
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,LOFF    ,
     3     RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SIGY    ,ET      ,TEMP    ,VARNL   ,OFF     ,IPG     ,
     8     DMG     ,L_DMG   ,PLA_NL  ,L_PLANL ,PLAP_NL ,L_EPSDNL,
     9     SEQ     ,INLOC   ,JLAG    ,FHEAT   ,VOLUME  )
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
#include      "scr17_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,JTHE,IPG,NPG,INLOC
      INTEGER ,DIMENSION(NEL), INTENT(IN) :: NGL
      INTEGER ,INTENT(IN) :: L_DMG,L_PLANL,L_EPSDNL 
      INTEGER ,INTENT(IN) :: JLAG
      my_real 
     .   TIME,TIMESTEP
      my_real, DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real, DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO0,RHO,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
      my_real, DIMENSION(NEL*L_PLANL), INTENT(IN) :: 
     .   PLA_NL
      my_real, DIMENSION(NEL*L_EPSDNL), INTENT(IN) :: 
     .   PLAP_NL
      my_real ,DIMENSION(NEL), INTENT(IN) :: VOLUME
      my_real, DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      my_real, DIMENSION(NEL), INTENT(INOUT)       :: 
     .   PLA,DPLA,EPSD,VARNL,LOFF,OFF,TEMP,SEQ,UELR
      my_real, DIMENSION(NEL*L_DMG), INTENT(INOUT) :: 
     .   DMG
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
      my_real ,DIMENSION(NEL)      , INTENT(INOUT) :: FHEAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IGURSON,NICE,NINDX,I,J,INDX(NEL),IR,IS,IT
C=======================================================================
c
      NICE    = NINT(UPARAM(11)) ! Plastic projection method
                                 !  = 1 => Nice method
                                 !  = 2 => Newton
c
      IGURSON = NINT(UPARAM(30)) ! Gurson switch flag: 
                                 !  = 0 => Drucker material law with no damage
                                 !  = 1 => Local Gurson damage model
                                 !  = 2 => Non local (Forest - micromorphic) damage model
                                 !  = 3 => Non local (Peerlings) damage model
c--------------------------                              
      SELECT CASE (IGURSON)
c      
        CASE(0)   ! Drucker material law with no damage
c                   
          IF ((NICE == 1).OR.(NICE == 3)) THEN
            CALL MAT104_NODAM_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,VOLUME  ,FHEAT   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3         RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,TEMP    ,SEQ     ,INLOC   ,VARNL   ,
     8         JLAG    )
          ELSE  ! Newton
            CALL MAT104_NODAM_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,VOLUME  ,FHEAT   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3         RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,TEMP    ,SEQ     ,INLOC   ,VARNL   ,
     8         JLAG    )
          ENDIF
c
        CASE(1,2)   ! Drucker material law with local Gurson damage
c 
          IF ((NICE == 1).OR.(NICE == 3)) THEN
            CALL MAT104_LDAM_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,VOLUME  ,FHEAT   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,LOFF    ,
     3         RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,VARNL   ,DMG     ,TEMP    ,JLAG    ,
     8         SEQ     ,PLA_NL  ,L_PLANL ,PLAP_NL ,L_EPSDNL)
          ELSE  ! Newton
            CALL MAT104_LDAM_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,VOLUME  , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,LOFF    ,
     3         RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,VARNL   ,DMG     ,TEMP    ,FHEAT   ,
     8         SEQ     ,PLA_NL  ,L_PLANL ,PLAP_NL ,L_EPSDNL,JLAG    )
          ENDIF
c
        CASE(3)   ! Drucker material law with non local (Peerling) damage model
c
          IF ((NICE == 1).OR.(NICE == 3)) THEN
            CALL MAT104_NLDAM_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,VOLUME  ,FHEAT   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,LOFF    ,
     3         RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,VARNL   ,DMG     ,TEMP    ,SEQ     ,
     8         PLA_NL  ,PLAP_NL ,JLAG    )
          ELSE  ! Newton
            CALL MAT104_NLDAM_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,VOLUME  ,FHEAT   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,LOFF    ,
     3         RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         SIGY    ,ET      ,VARNL   ,DMG     ,TEMP    ,SEQ     ,
     8         PLA_NL  ,PLAP_NL ,JLAG    )
          ENDIF
c
      END SELECT        
c--------------------------
      ! Solid elements deletion
      IF (IGURSON>0) THEN 
        NINDX = 0
        INDX(1:NEL) = 0
        DO I=1,NEL
          !Integration point failure
          IF (LOFF(I) == FOUR_OVER_5) THEN
            UELR(I) = UELR(I) + ONE
            ! If all integration points have failed, element is deleted
            IF (NINT(UELR(I)) == NPG) THEN 
              NINDX       = NINDX+1
              INDX(NINDX) = I
              OFF(I)      = ZERO
            ENDIF
          ENDIF
        ENDDO
        IF((NINDX>0).AND.(IMCONV==1))THEN
          DO J=1,NINDX
#include "lockon.inc"
            WRITE(IOUT, 1000) NGL(INDX(J)),TT
            WRITE(ISTDO,1000) NGL(INDX(J)),TT
#include "lockoff.inc"
          ENDDO
        ENDIF
      ENDIF
c
 1000 FORMAT(1X,'-- RUPTURE (GURSON) OF SOLID ELEMENT :',I10,' AT TIME :',1PE12.4)  
c  
c-----------
      RETURN
      END
