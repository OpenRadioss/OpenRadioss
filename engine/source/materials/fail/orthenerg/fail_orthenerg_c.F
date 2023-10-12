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
      SUBROUTINE FAIL_ORTHENERG_C(
     1     NEL      ,NUPARAM  ,NUVAR    ,UPARAM   ,UVAR     ,
     2     NGL      ,TIME     ,IPG      ,ILAY     ,IPT      ,   
     3     DEPSXX   ,DEPSYY   ,DEPSXY   ,DMG_FLAG ,DMG_SCALE,
     4     ALDT     ,PTHKF    ,FOFF     ,DFMAX    ,TDEL     ,
     5     SIGNXX   ,SIGNYY   ,SIGNXY   ,IGTYP    ,PLY_ID   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT,
     .   IGTYP,PLY_ID
      INTEGER, DIMENSION(NEL), INTENT(IN) :: NGL
      my_real, INTENT(IN) :: TIME
      my_real, DIMENSION(NEL), INTENT(IN) :: DEPSXX,DEPSYY,
     .   DEPSXY,SIGNXX,SIGNYY,SIGNXY,ALDT
      my_real, DIMENSION(NUPARAM), INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER, INTENT(OUT) :: DMG_FLAG
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: FOFF
      my_real, INTENT(OUT) :: PTHKF
      my_real, DIMENSION(NEL), INTENT(INOUT) :: DFMAX,TDEL
      my_real, DIMENSION(NEL,5), INTENT(OUT) :: DMG_SCALE
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDX,FAILMOD,MODE,ISHAP11T,ISHAP11C,ISHAP22T,
     .  ISHAP22C,ISHAP12T,ISHAP12C,NMOD
      INTEGER ,DIMENSION(NEL) :: INDX
      INTEGER ,DIMENSION(NEL,6) :: FMODE
      my_real DAM(NEL,6),ENER(NEL,6),LE(NEL)
      my_real SIGMA_11T,SIGMA_11C,SIGMA_22T,SIGMA_22C,
     .  SIGMA_12T,SIGMA_12C,G_11T,G_11C,G_22T,G_22C,G_12T,G_12C
C
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      PTHKF      = UPARAM(1)  ! -> Percentage of failed thickness/ratio
      SIGMA_11T  = UPARAM(3)  ! -> Critical stress for tension in direction 11
      SIGMA_11C  = UPARAM(4)  ! -> Critical stress for compression in direction 11
      SIGMA_22T  = UPARAM(5)  ! -> Critical stress for tension in direction 22
      SIGMA_22C  = UPARAM(6)  ! -> Critical stress for compression in direction 22
      SIGMA_12T  = UPARAM(9)  ! -> Critical stress for positive shear in plane 12
      SIGMA_12C  = UPARAM(10) ! -> Critical stress for negative shear in plane 12
      G_11T      = UPARAM(15) ! -> Fracture energy for tension in direction 11
      G_11C      = UPARAM(16) ! -> Fracture energy for compression in direction 11
      G_22T      = UPARAM(17) ! -> Fracture energy for tension in direction 22
      G_22C      = UPARAM(18) ! -> Fracture energy for compression in direction 22
      G_12T      = UPARAM(21) ! -> Fracture energy for positive shear in plane 12
      G_12C      = UPARAM(22) ! -> Fracture energy for negative shear in plane 12
      ISHAP11T   = NINT(UPARAM(27)) ! -> Softening shape flag for tension in direction 11
      ISHAP11C   = NINT(UPARAM(28)) ! -> Softening shape flag for compression in direction 11
      ISHAP22T   = NINT(UPARAM(29)) ! -> Softening shape flag for tension in direction 22
      ISHAP22C   = NINT(UPARAM(30)) ! -> Softening shape flag for compression in direction 22
      ISHAP12T   = NINT(UPARAM(33)) ! -> Softening shape flag for positive shear in plane 12
      ISHAP12C   = NINT(UPARAM(34)) ! -> Softening shape flag for negative shear in plane 12
      NMOD       = NINT(UPARAM(39)) ! -> Number of failed modes prior to integration point failure
      NMOD       = MIN(NMOD,6)
C
      ! Set DMG_FLAG to Orthotropic
      DMG_FLAG = 3
C 
      ! Save initial element length
      IF (UVAR(1,13) == ZERO) THEN
        UVAR(1:NEL,13) = ALDT(1:NEL)
      ENDIF
      LE(1:NEL) = UVAR(1:NEL,13)
c
      ! Recover user variable value
      DO J = 1,6
        DO I = 1,NEL 
          ! Damage variable per mode
          DAM(I,J)  = UVAR(I,J)
          ! Dissipated energy per mode
          ENER(I,J) = UVAR(I,J+6) 
        ENDDO
      ENDDO
c
      !====================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !==================================================================== 
      ! Initialization of element failure index    
      NINDX = 0  
c
      ! Loop over the elements
      DO I=1, NEL
c
        IF (FOFF(I) == 1) THEN
c
          ! Initialization of failed modes index
          FMODE(I,1:6) = 0
          FAILMOD = 0
c
          ! Mode 1 failure - Tension XX 
          MODE = 1
          IF (SIGNXX(I) > SIGMA_11T .AND. SIGNXX(I) > ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP11T == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*DEPSXX(I)*SIGMA_11T/(TWO*G_11T)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP11T == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + SIGNXX(I)*LE(I)*DEPSXX(I)
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+6))
              DAM(I,MODE)  = (ONE - EXP(-ENER(I,MODE)/G_11T))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 1 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! Mode 2 failure - Tension YY
          MODE = 2 
          IF (SIGNYY(I) > SIGMA_22T .AND. SIGNYY(I) > ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP22T == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*DEPSYY(I)*SIGMA_22T/(TWO*G_22T)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP22T == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + SIGNYY(I)*LE(I)*DEPSYY(I)
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+6))
              DAM(I,MODE)  = (ONE - EXP(-ENER(I,MODE)/G_22T))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 2 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! Mode 3 failure - Positive shear in plane XY
          MODE = 3
          IF (SIGNXY(I) > SIGMA_12T .AND. SIGNXY(I) > ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP12T == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*DEPSXY(I)*SIGMA_12T/(FOUR*G_12T)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP12T == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + SIGNXY(I)*LE(I)*HALF*DEPSXY(I)
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+6))
              DAM(I,MODE)  = (ONE - EXP(-ENER(I,MODE)/G_12T))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 3 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! Mode 4 failure - Compression XX
          MODE  = 4
          IF (-SIGNXX(I) > SIGMA_11C .AND. SIGNXX(I) < ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP11C == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*ABS(DEPSXX(I))*SIGMA_11C/(TWO*G_11C)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP11C == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + ABS(SIGNXX(I))*LE(I)*ABS(DEPSXX(I))
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+6))
              DAM(I,MODE)  = (ONE - EXP(-ENER(I,MODE)/G_11C))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 4 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! Mode 5 failure - Compression YY
          MODE = 5
          IF (-SIGNYY(I) > SIGMA_22C .AND. SIGNYY(I) < ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP22C == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*ABS(DEPSYY(I))*SIGMA_22C/(TWO*G_22C)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP22C == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + ABS(SIGNYY(I))*LE(I)*ABS(DEPSYY(I))
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+6))
              DAM(I,MODE)  = (ONE - EXP(-ENER(I,MODE)/G_22C))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 5 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! Mode 6 failure - Negative shear in plane XY
          MODE = 6
          IF (-SIGNXY(I) > SIGMA_12C .AND. SIGNXY(I) < ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP12C == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*ABS(DEPSXY(I))*SIGMA_12C/(FOUR*G_12C)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP12C == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + ABS(SIGNXY(I))*LE(I)*HALF*ABS(DEPSXY(I))
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+6))
              DAM(I,MODE) = (ONE - EXP(-ENER(I,MODE)/G_12C))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 6 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! If at least one mode has failed 
          IF (FAILMOD >= NMOD) THEN
            FOFF(I) = 0
            TDEL(I) = TIME 
            NINDX   = NINDX + 1  
            INDX(NINDX) = I
          ENDIF
        ENDIF
      ENDDO 
c  
      !====================================================================
      ! - COMPUTE STRESS SOFTENING SCALE FACTORS
      !====================================================================
      DO I=1, NEL
        IF (FOFF(I) == 1) THEN
          ! Direction 1
          IF (SIGNXX(I) >= ZERO) THEN  
            DMG_SCALE(I,1) = ONE - DAM(I,1)
          ELSE 
            DMG_SCALE(I,1) = ONE - DAM(I,4)
          ENDIF 
          ! Direction 2 
          IF (SIGNYY(I) >= ZERO) THEN 
            DMG_SCALE(I,2) = ONE - DAM(I,2)
          ELSE 
            DMG_SCALE(I,2) = ONE - DAM(I,5)
          ENDIF
          ! Plane 12
          IF (SIGNXY(I) >= ZERO) THEN 
            DMG_SCALE(I,3) = ONE - DAM(I,3)
          ELSE 
            DMG_SCALE(I,3) = ONE - DAM(I,6)
          ENDIF
        ENDIF 
      ENDDO
c  
      !====================================================================
      ! - UPDATE USER VARIABLES
      !====================================================================
      DO J = 1,6
        DO I = 1,NEL
          ! Damage variable per mode
          UVAR(I,J)   = DAM(I,J)
          ! Dissipated energy per mode
          UVAR(I,J+6) = ENER(I,J)
        ENDDO
      ENDDO   
c
      !====================================================================
      ! - PRINTOUT DATA ABOUT FAILED MODES
      !====================================================================
      IF (NINDX > 0) THEN 
        DO J=1,NINDX    
          I = INDX(J)
#include  "lockon.inc"
          IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN 
            WRITE(IOUT, 1200) NGL(I),IPG,PLY_ID,IPT
            WRITE(ISTDO,1200) NGL(I),IPG,PLY_ID,IPT
          ELSEIF (IGTYP == 1 .OR. IGTYP == 9) THEN 
            WRITE(IOUT, 1000) NGL(I),IPG,IPT
            WRITE(ISTDO,1000) NGL(I),IPG,IPT
          ELSE 
            WRITE(IOUT, 1100) NGL(I),IPG,ILAY,IPT
            WRITE(ISTDO,1100) NGL(I),IPG,ILAY,IPT
          ENDIF
          IF (FMODE(I,1)==1) WRITE(IOUT, 2000) '1 - TRACTION XX'
          IF (FMODE(I,2)==1) WRITE(IOUT, 2000) '2 - TRACTION YY'
          IF (FMODE(I,3)==1) WRITE(IOUT, 2000) '3 - POSITIVE SHEAR XY'
          IF (FMODE(I,4)==1) WRITE(IOUT, 2000) '4 - COMPRESSION XX'
          IF (FMODE(I,5)==1) WRITE(IOUT, 2000) '5 - COMPRESSION YY'
          IF (FMODE(I,6)==1) WRITE(IOUT, 2000) '6 - NEGATIVE SHEAR XY'
#include  "lockoff.inc"
        ENDDO
      ENDIF
c-----------------------------------------------------------------------
 1000 FORMAT(1X,'FAILURE (ORTHENERG) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',INTEGRATION PT',I3)
 1100 FORMAT(1X,'FAILURE (ORTHENERG) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 1200 FORMAT(1X,'FAILURE (ORTHENERG) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',PLY ID',I10,1X,',INTEGRATION PT',I3)
 2000 FORMAT(1X,'MODE',1X,A)
c-----------------------------------------------------------------------
      END
