Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  FAIL_MAXSTRAIN_C              source/materials/fail/max_strain/fail_maxstrain_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_MAXSTRAIN_C(
     1     NEL       ,NUPARAM   ,NUVAR     ,UPARAM    ,UVAR      , 
     2     TIME      ,NGL       ,IPG       ,ILAY      ,IPT       ,                          
     3     EPSXX     ,EPSYY     ,EPSXY     ,EPSYZ     ,EPSZX     ,  
     4     OFF       ,FOFF      ,DMG_FLAG  ,DMG_SCALE ,
     5     DFMAX     ,TDEL      ,TIMESTEP  )               
C-----------------------------------------------
C    modified Puck model ------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "units_c.inc"
#include  "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME,TIMESTEP
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: OFF,
     .   EPSXX,EPSYY,EPSXY,EPSYZ,EPSZX
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(OUT) :: DMG_FLAG
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: FOFF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL,DMG_SCALE
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX,IFAIL_SH
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real
     .   EPS1_MAX,EPS2_MAX,GAM12_MAX,TMAX,FCUT
      my_real
     .   EXX(NEL),EYY(NEL),EXY(NEL),FINDEX,RFACTR,ASRATE
C!----------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      EPS1_MAX  = UPARAM(1)
      EPS2_MAX  = UPARAM(2)
      GAM12_MAX = UPARAM(3)
      TMAX      = UPARAM(4)
      FCUT      = UPARAM(5)
      IFAIL_SH  = INT(UPARAM(6))
c
      ! Stress softening activation
      DMG_FLAG = 1    
c
      ! Stress tensor filtering
      IF (FCUT > ZERO) THEN
        ASRATE = TWO*PI*FCUT*TIMESTEP
        ASRATE = ASRATE/(ONE+ASRATE)
        DO I = 1,NEL
          EXX(I)    = ASRATE*EPSXX(I) + (ONE - ASRATE)*UVAR(I,2)
          EYY(I)    = ASRATE*EPSYY(I) + (ONE - ASRATE)*UVAR(I,3)
          EXY(I)    = ASRATE*EPSXY(I) + (ONE - ASRATE)*UVAR(I,4)
          UVAR(I,2) = EXX(I)
          UVAR(I,3) = EYY(I)
          UVAR(I,4) = EXY(I)
        ENDDO
      ELSE
        DO I = 1,NEL
          EXX(I) = EPSXX(I)
          EYY(I) = EPSYY(I)
          EXY(I) = EPSXY(I)
        ENDDO
      ENDIF
c
      !====================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !==================================================================== 
      ! Initialization of element failure index           
      NINDX = 0
      INDX(1:NEL) = 0  
c
      ! Loop over the elements
      DO I=1,NEL
c
        ! If damage has not been reached yet
        IF (DFMAX(I)<ONE) THEN
c
          ! Compute failure index and reserve factor
          FINDEX = MAX(ABS(EXX(I))/EPS1_MAX,
     .                 ABS(EYY(I))/EPS2_MAX,
     .                 ABS(EXY(I))/GAM12_MAX)
          IF (FINDEX > ZERO) THEN 
            RFACTR = ONE/SQRT(FINDEX)
          ELSE 
            RFACTR = ZERO
          ENDIF
c
          ! Damage variable update
          FINDEX   = MAX(ZERO,FINDEX)
          DFMAX(I) = MIN(ONE ,MAX(FINDEX,DFMAX(I)))
          IF (DFMAX(I) >= ONE) THEN
            NINDX = NINDX+1                                    
            INDX(NINDX) = I                                    
            IF (IFAIL_SH > 0) THEN 
              UVAR(I,1) = TIME
            ENDIF                                
          ENDIF
        ENDIF
c
        ! Stress relaxation in case of damage reached
        IF ((UVAR(I,1) > ZERO).AND.(FOFF(I) /= 0).AND.(IFAIL_SH > 0)) THEN 
          DMG_SCALE(I) = EXP(-(TIME - UVAR(I,1))/TMAX)   
          IF (DMG_SCALE(I) < EM02) THEN
            FOFF(I) = 0
            TDEL(I) = TIME
            DMG_SCALE(I) = ZERO
          ENDIF
        ENDIF  
      ENDDO       
c              
      !====================================================================
      ! - PRINTOUT DATA ABOUT FAILED ELEMENTS
      !====================================================================
      IF (NINDX > 0) THEN  
        DO J=1,NINDX       
          I = INDX(J)      
#include  "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,ILAY,IPT
          WRITE(ISTDO,2100) NGL(I),IPG,ILAY,IPT,TIME
#include  "lockoff.inc"
        END DO
      END IF              
c------------------------
 2000 FORMAT(1X,'FAILURE (MAXSTRAIN) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (MAXSTRAIN) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------------
      END
