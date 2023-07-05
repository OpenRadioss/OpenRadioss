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
Chd|  SIGEPS104C                    source/materials/mat/mat104/sigeps104c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        MAT104C_LDAM_NEWTON           source/materials/mat/mat104/mat104c_ldam_newton.F
Chd|        MAT104C_LDAM_NICE             source/materials/mat/mat104/mat104c_ldam_nice.F
Chd|        MAT104C_NLDAM_NEWTON          source/materials/mat/mat104/mat104c_nldam_newton.F
Chd|        MAT104C_NLDAM_NICE            source/materials/mat/mat104/mat104c_nldam_nice.F
Chd|        MAT104C_NODAM_NEWTON          source/materials/mat/mat104/mat104c_nodam_newton.F
Chd|        MAT104C_NODAM_NICE            source/materials/mat/mat104/mat104c_nodam_nice.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS104C(
     1     NEL     ,NGL     ,IPG     ,ILAY    ,IPT     ,NUPARAM ,NUVAR   ,
     2     NFUNC   ,IFUNC   ,NPF     ,TF      ,TIMESTEP,TIME    ,UPARAM  ,
     3     UVAR    ,JTHE    ,RHO     ,TEMPEL  ,PLA     ,DPLA    ,SOUNDSP ,
     4     OFFL    ,EPSD    ,GS      ,DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,
     5     DEPSZX  ,THKLY   ,OFF     ,SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,
     6     SIGOZX  ,THK     ,SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     VISCMAX ,SIGY    ,ET      ,VARNL   ,DMG     ,TEMP    ,SEQ     ,
     8     NPTR    ,NPTS    ,NPTT    ,BUFLY   ,INLOC   ,EPSPL   )
     
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,NFUNC,JTHE,IPG,ILAY,IPT,INLOC
      INTEGER NPF(*),NGL(NEL),IFUNC(NFUNC),NPTR,NPTS,NPTT
      my_real 
     .   TIMESTEP,TIME,TF(*),UPARAM(NUPARAM)
      my_real,DIMENSION(NEL), INTENT(IN) :: 
     .   RHO, 
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,
     .   TEMPEL, THKLY,GS
c
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
c
      my_real ,DIMENSION(NEL) :: 
     .   SIGY,ET,VISCMAX
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: 
     .   PLA,EPSD,OFFL,THK,OFF,VARNL,DMG,TEMP,SEQ,DPLA,EPSPL
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      TYPE(BUF_LAY_) :: BUFLY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,II,IGURSON,NSP,ITER,NITER,NINDX,NICE,INDEX(NEL),INDX(NEL),
     .        IR,IS,IT
C=======================================================================
c      UVAR(1)     
c      UVAR(2)     
C      DEPIJ = PLASTIC STRAIN TENSOR COMPONENT
C      DEPSIJ = TOTAL STRAIN  TENSOR COMPONENT (EL+PL)
C=======================================================================
c  
       NICE    = NINT(UPARAM(11))! Plastic projection method
                                 !  = 1 => Nice method
                                 !  = 2 => Newton-iteration method
                                 !  = 3 => Nice method with elastic update
       IGURSON = NINT(UPARAM(30)) ! Gurson switch flag: 
                                 !  = 0 => Drucker material law with no damage model
                                 !  = 1 => local Gurson damage model
                                 !  = 2 => non local (Forest - micromorphic) damage model
                                 !  = 3 => non local (Peerlings) damage model
c--------------------------                        
      SELECT CASE (IGURSON)
c      
        CASE(0)
c     
c
c
          IF ((NICE == 1).OR.(NICE == 3)) THEN 
            CALL MAT104C_NODAM_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3         GS      ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,THKLY   ,
     7         THK     ,SIGY    ,ET      ,TEMPEL  ,VARNL   ,TEMP    ,
     8         SEQ     ,INLOC   )
          ELSE  ! Newton
            CALL MAT104C_NODAM_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3         GS      ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,THKLY   ,
     7         THK     ,SIGY    ,ET      ,TEMPEL  ,VARNL   ,TEMP    ,
     8         SEQ     ,INLOC   )
          ENDIF
c      
        CASE(1,2)
c
          IF ((NICE == 1).OR.(NICE == 3)) THEN 
            CALL MAT104C_LDAM_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFFL    ,
     3         GS      ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,THKLY   ,
     7         THK     ,SIGY    ,ET      ,TEMPEL  ,VARNL   ,DMG     ,
     8         TEMP    ,SEQ     )
          ELSE  ! Newton
            CALL MAT104C_LDAM_NEWTON(
     1           NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2           TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFFL    ,
     3           GS      ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4           DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5           SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6           SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,THKLY   ,
     7           THK     ,SIGY    ,ET      ,TEMPEL  ,VARNL   ,DMG     ,
     8           TEMP    ,SEQ     )
          ENDIF      
        CASE(3)
c
          IF ((NICE == 1).OR.(NICE == 3)) THEN 
            CALL MAT104C_NLDAM_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2         TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFFL    ,
     3         GS      ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,THKLY   ,
     7         THK     ,SIGY    ,ET      ,TEMPEL  ,VARNL   ,DMG     ,
     8         TEMP    ,SEQ     )
          ELSE  ! Newton
            CALL MAT104C_NLDAM_NEWTON(
     1           NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2           TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFFL    ,
     3           GS      ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4           DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5           SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6           SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,THKLY   ,
     7           THK     ,SIGY    ,ET      ,TEMPEL  ,VARNL   ,DMG     ,
     8           TEMP    ,SEQ     )
          ENDIF      
      END SELECT        
c    
      ! Save the strain rate value for failure criterion
      EPSPL(1:NEL) = EPSD(1:NEL)
c--------------------------
      ! Integration point failure
      IF (IGURSON>0) THEN 
        NINDX = 0
        DO I=1,NEL
          ! Case of under-integrated shells
          IF ((NPTR == 1).AND.(NPTS == 1)) THEN
            !Initialization for checking complete failure of the shell (all integration points)
            IF (IPT == 1) THEN
              OFF(I) = ZERO
            ENDIF
            !If one integration points is not fully broken, the shell remains
            IF (OFFL(I)>ZERO) OFF(I) = ONE
          ! Case of fully integrated shells
          ELSE
            IF ((IPG == 1).AND.(IPT == 1)) THEN 
              !Initialization for checking complete failure of the shell (all integration points)
              OFF(I) = ZERO
              ! Loop over all Gauss points (thickness + surface)
              DO IR = 1,NPTR
                DO IS = 1,NPTS
                  DO IT = 1,NPTT
                    !If one integration points is not fully broken, the shell remains
                    IF (BUFLY%LBUF(IR,IS,IT)%OFF(I)>ZERO) OFF(I) = ONE
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
          ENDIF
          !Integration point failure
          IF (OFFL(I) == FOUR_OVER_5) THEN
            NINDX      = NINDX+1
            INDX(NINDX)= I
          ENDIF
        ENDDO
        IF (NINDX>0) THEN
          DO J=1,NINDX
#include "lockon.inc"
            WRITE(IOUT, 2000) NGL(INDX(J)),IPG,IPT
            WRITE(ISTDO,2100) NGL(INDX(J)),IPG,IPT,TT
#include "lockoff.inc"
          ENDDO
        ENDIF
      ENDIF
c
 2000 FORMAT(1X,'FAILURE (GURSON) IN SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',THICKNESS INTG. PT',I3)
 2100 FORMAT(1X,'FAILURE (GURSON) IN SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',THICKNESS INTG. PT',I3,
     .       1X,'AT TIME :',1PE12.4)
c  
c-----------
      RETURN
      END
