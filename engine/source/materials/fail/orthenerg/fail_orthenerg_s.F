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
Chd|  FAIL_ORTHENERG_S              source/materials/fail/orthenerg/fail_orthenerg_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_ORTHENERG_S(
     1     NEL      ,NUPARAM  ,NUVAR    ,UPARAM   ,UVAR     ,NGL      , 
     2     NPG      ,IPG      ,ILAY     ,OFF      ,LOFF     ,NOFF     ,
     3     DEPSXX   ,DEPSYY   ,DEPSZZ   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     4     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5     TIME     ,TDEL     ,DFMAX    ,ALDT     ,DMG_SCALE,IDEL7NOK )
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
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,NPG,ILAY
      INTEGER, DIMENSION(NEL), INTENT(IN) :: NGL
      my_real, INTENT(IN) :: TIME
      my_real, DIMENSION(NEL), INTENT(IN) :: DEPSXX,DEPSYY,
     .   DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,SIGNXX,SIGNYY,SIGNZZ,
     .   SIGNXY,SIGNYZ,SIGNZX,ALDT
      my_real, DIMENSION(NUPARAM), INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: IDEL7NOK
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: NOFF
      my_real, DIMENSION(NEL), INTENT(INOUT) :: DFMAX,TDEL,OFF,LOFF
      my_real, DIMENSION(NEL,6), INTENT(INOUT) :: DMG_SCALE
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDX,NINDX2,FAILMOD,MODE,ISHAP11T,ISHAP11C,
     .  ISHAP22T,ISHAP22C,ISHAP33T,ISHAP33C,ISHAP12T,ISHAP12C,
     .  ISHAP23T,ISHAP23C,ISHAP31T,ISHAP31C,FAILIP,NMOD
      INTEGER ,DIMENSION(NEL) :: INDX, INDX2
      INTEGER ,DIMENSION(NEL,12) :: FMODE
      my_real DAM(NEL,12),ENER(NEL,12),LE(NEL)
      my_real SIGMA_11T,SIGMA_11C,SIGMA_22T,SIGMA_22C,SIGMA_33T,
     .  SIGMA_33C,SIGMA_12T,SIGMA_12C,SIGMA_23T,SIGMA_23C,
     .  SIGMA_31T,SIGMA_31C,G_11T,G_11C,G_22T,G_22C,G_33T,G_33C,
     .  G_12T,G_12C,G_23T,G_23C,G_31T,G_31C
C
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      FAILIP     = NINT(UPARAM(2)) ! -> Percentage of failed thickness/ratio
      FAILIP     = MIN(FAILIP,NPG)
      SIGMA_11T  = UPARAM(3)  ! -> Critical stress for tension in direction 11
      SIGMA_11C  = UPARAM(4)  ! -> Critical stress for compression in direction 11
      SIGMA_22T  = UPARAM(5)  ! -> Critical stress for tension in direction 22
      SIGMA_22C  = UPARAM(6)  ! -> Critical stress for compression in direction 22
      SIGMA_33T  = UPARAM(7)  ! -> Critical stress for tension in direction 33
      SIGMA_33C  = UPARAM(8)  ! -> Critical stress for compression in direction 33
      SIGMA_12T  = UPARAM(9)  ! -> Critical stress for positive shear in plane 12
      SIGMA_12C  = UPARAM(10) ! -> Critical stress for negative shear in plane 12
      SIGMA_23T  = UPARAM(11) ! -> Critical stress for positive shear in plane 23
      SIGMA_23C  = UPARAM(12) ! -> Critical stress for negative shear in plane 23
      SIGMA_31T  = UPARAM(13) ! -> Critical stress for positive shear in plane 31
      SIGMA_31C  = UPARAM(14) ! -> Critical stress for negative shear in plane 31
      G_11T      = UPARAM(15) ! -> Fracture energy for tension in direction 11
      G_11C      = UPARAM(16) ! -> Fracture energy for compression in direction 11
      G_22T      = UPARAM(17) ! -> Fracture energy for tension in direction 22
      G_22C      = UPARAM(18) ! -> Fracture energy for compression in direction 22
      G_33T      = UPARAM(19) ! -> Fracture energy for tension in direction 33
      G_33C      = UPARAM(20) ! -> Fracture energy for compression in direction 33
      G_12T      = UPARAM(21) ! -> Fracture energy for positive shear in plane 12
      G_12C      = UPARAM(22) ! -> Fracture energy for negative shear in plane 12
      G_23T      = UPARAM(23) ! -> Fracture energy for positive shear in plane 23
      G_23C      = UPARAM(24) ! -> Fracture energy for negative shear in plane 23
      G_31T      = UPARAM(25) ! -> Fracture energy for positive shear in plane 31
      G_31C      = UPARAM(26) ! -> Fracture energy for negative shear in plane 31
      ISHAP11T   = NINT(UPARAM(27)) ! -> Softening shape flag for tension in direction 11
      ISHAP11C   = NINT(UPARAM(28)) ! -> Softening shape flag for compression in direction 11
      ISHAP22T   = NINT(UPARAM(29)) ! -> Softening shape flag for tension in direction 22
      ISHAP22C   = NINT(UPARAM(30)) ! -> Softening shape flag for compression in direction 22
      ISHAP33T   = NINT(UPARAM(31)) ! -> Softening shape flag for tension in direction 33
      ISHAP33C   = NINT(UPARAM(32)) ! -> Softening shape flag for compression in direction 33
      ISHAP12T   = NINT(UPARAM(33)) ! -> Softening shape flag for positive shear in plane 12
      ISHAP12C   = NINT(UPARAM(34)) ! -> Softening shape flag for negative shear in plane 12
      ISHAP23T   = NINT(UPARAM(35)) ! -> Softening shape flag for positive shear in plane 23
      ISHAP23C   = NINT(UPARAM(36)) ! -> Softening shape flag for negative shear in plane 23
      ISHAP31T   = NINT(UPARAM(37)) ! -> Softening shape flag for positive shear in plane 31
      ISHAP31C   = NINT(UPARAM(38)) ! -> Softening shape flag for negative shear in plane 31
      NMOD       = NINT(UPARAM(39)) ! -> Number of failed modes prior to integration point failure
C 
      ! Save initial element length
      IF (UVAR(1,25) == ZERO) THEN
        UVAR(1:NEL,25) = ALDT(1:NEL)
      ENDIF
      LE(1:NEL) = UVAR(1:NEL,25)
c
      ! Recover user variable value
      DO J = 1,12
        DO I = 1,NEL 
          ! Damage variable per mode
          DAM(I,J)  = UVAR(I,J)
          ! Dissipated energy per mode
          ENER(I,J) = UVAR(I,J+12) 
        ENDDO
      ENDDO
c
      !====================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !==================================================================== 
      ! Initialization of element failure index    
      NINDX = 0  
      NINDX2 = 0
      INDX(1:NEL) = 0
      INDX2(1:NEL) = 0
c
      ! Loop over the elements
      DO I = 1, NEL
c
        IF ((LOFF(I) == ONE).AND.(OFF(I) == ONE)) THEN
c
          ! Initialization of failed modes index
          FMODE(I,1:12) = 0
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
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
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
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
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
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
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
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
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
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
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
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
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
          ! Mode 7 failure - Tension ZZ
          MODE = 7
          IF (SIGNZZ(I) > SIGMA_33T .AND. SIGNZZ(I) > ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP33T == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*DEPSZZ(I)*SIGMA_33T/(TWO*G_33T)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP33T == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + SIGNZZ(I)*LE(I)*DEPSZZ(I)
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
              DAM(I,MODE)  = (ONE - EXP(-ENER(I,MODE)/G_33T))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 7 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! Mode 8 failure - Compression ZZ
          MODE = 8
          IF (-SIGNZZ(I) > SIGMA_33C .AND. SIGNZZ(I) < ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP33C == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*ABS(DEPSZZ(I))*SIGMA_33C/(TWO*G_33C)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP33C == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + ABS(SIGNZZ(I))*LE(I)*ABS(DEPSZZ(I))
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
              DAM(I,MODE)  = (ONE - EXP(-ENER(I,MODE)/G_33C))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 8 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! Mode 9 failure - Positive shear in plane YZ
          MODE = 9
          IF (SIGNYZ(I) > SIGMA_23T .AND. SIGNYZ(I) > ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP23T == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*DEPSYZ(I)*SIGMA_23T/(FOUR*G_23T)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP23T == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + SIGNYZ(I)*LE(I)*HALF*DEPSYZ(I)
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
              DAM(I,MODE)  = (ONE - EXP(-ENER(I,MODE)/G_23T))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 9 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! Mode 10 failure - Negative shear in plane YZ
          MODE = 10
          IF (-SIGNYZ(I) > SIGMA_23C .AND. SIGNYZ(I) < ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP23C == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*ABS(DEPSYZ(I))*SIGMA_23C/(FOUR*G_23C)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP23C == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + ABS(SIGNYZ(I))*LE(I)*HALF*ABS(DEPSYZ(I))
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
              DAM(I,MODE) = (ONE - EXP(-ENER(I,MODE)/G_23C))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 10 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! Mode 11 failure - Positive shear in plane ZX
          MODE = 11
          IF (SIGNZX(I) > SIGMA_31T .AND. SIGNZX(I) > ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP31T == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*DEPSZX(I)*SIGMA_31T/(FOUR*G_31T)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP31T == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + SIGNZX(I)*LE(I)*HALF*DEPSZX(I)
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
              DAM(I,MODE)  = (ONE - EXP(-ENER(I,MODE)/G_31T))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 11 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! Mode 12 failure - Negative shear in plane YZ
          MODE = 12
          IF (-SIGNZX(I) > SIGMA_31C .AND. SIGNZX(I) < ZERO .AND. DAM(I,MODE) < ONE) THEN
            ! -> Linear stress softening
            IF (ISHAP31C == 1) THEN 
              DAM(I,MODE) = DAM(I,MODE) + LE(I)*ABS(DEPSZX(I))*SIGMA_31C/(FOUR*G_31C)
              DAM(I,MODE) = MAX(DAM(I,MODE),UVAR(I,MODE))
            ! -> Exponential stress softening
            ELSEIF (ISHAP31C == 2) THEN 
              ENER(I,MODE) = ENER(I,MODE) + ABS(SIGNZX(I))*LE(I)*HALF*ABS(DEPSZX(I))
              ENER(I,MODE) = MAX(ENER(I,MODE),UVAR(I,MODE+12))
              DAM(I,MODE) = (ONE - EXP(-ENER(I,MODE)/G_31C))
              IF (DAM(I,MODE) > 0.999D0) DAM(I,MODE) = ONE
            ENDIF
            DAM(I,MODE) = MIN(DAM(I,MODE),ONE)
            ! Output damage variable
            DFMAX(I) = MAX(DFMAX(I),DAM(I,MODE))
          ENDIF
          ! -> Check mode 12 failure
          IF (DAM(I,MODE) >= ONE) THEN
            FAILMOD = FAILMOD + 1
            FMODE(I,MODE) = 1
          ENDIF
c
          ! If at least one mode has failed 
          IF (FAILMOD >= NMOD) THEN
            LOFF(I) = ZERO 
            NINDX   = NINDX + 1  
            INDX(NINDX) = I
            NOFF(I) = NOFF(I) + 1
            IF (NOFF(I) >= FAILIP) THEN 
              OFF(I)  = ZERO 
              TDEL(I) = TIME 
              NINDX2  = NINDX2 + 1
              INDX2(NINDX2) = I
              IDEL7NOK = 1
            ENDIF
          ENDIF
        ENDIF
      ENDDO 
c  
      !====================================================================
      ! - COMPUTE STRESS SOFTENING SCALE FACTORS
      !====================================================================
      DO I = 1, NEL 
        IF ((LOFF(I) == ONE) .AND. (OFF(I) == ONE)) THEN 
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
          ! Direction 3 
          IF (SIGNZZ(I) >= ZERO) THEN 
            DMG_SCALE(I,3) = ONE - DAM(I,7)
          ELSE 
            DMG_SCALE(I,3) = ONE - DAM(I,8)
          ENDIF
          ! Plane 12
          IF (SIGNXY(I) >= ZERO) THEN 
            DMG_SCALE(I,4) = ONE - DAM(I,3)
          ELSE 
            DMG_SCALE(I,4) = ONE - DAM(I,6)
          ENDIF
          ! Plane 23
          IF (SIGNYZ(I) >= ZERO) THEN 
            DMG_SCALE(I,5) = ONE - DAM(I,9)
          ELSE 
            DMG_SCALE(I,5) = ONE - DAM(I,10)
          ENDIF
          ! Plane 31
          IF (SIGNZX(I) >= ZERO) THEN 
            DMG_SCALE(I,6) = ONE - DAM(I,11)
          ELSE
            DMG_SCALE(I,6) = ONE - DAM(I,12)
          ENDIF
        ENDIF
      ENDDO 
c
      !====================================================================
      ! - UPDATE USER VARIABLES
      !====================================================================
      DO J = 1,12
        DO I = 1,NEL
          ! Damage variable per mode
          UVAR(I,J) = DAM(I,J)
          ! Dissipated energy per mode
          UVAR(I,J+12) = ENER(I,J)
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
          WRITE(IOUT, 1000) NGL(I),IPG,ILAY
          WRITE(ISTDO,1000) NGL(I),IPG,ILAY
          IF (FMODE(I,1)  == 1) WRITE(IOUT,2000) '1  - TRACTION XX'
          IF (FMODE(I,2)  == 1) WRITE(IOUT,2000) '2  - TRACTION YY'
          IF (FMODE(I,3)  == 1) WRITE(IOUT,2000) '3  - POSITIVE SHEAR XY'
          IF (FMODE(I,4)  == 1) WRITE(IOUT,2000) '4  - COMPRESSION XX'
          IF (FMODE(I,5)  == 1) WRITE(IOUT,2000) '5  - COMPRESSION YY'
          IF (FMODE(I,6)  == 1) WRITE(IOUT,2000) '6  - NEGATIVE SHEAR XY'
          IF (FMODE(I,7)  == 1) WRITE(IOUT,2000) '7  - TRACTION ZZ'
          IF (FMODE(I,8)  == 1) WRITE(IOUT,2000) '8  - COMPRESSION ZZ'
          IF (FMODE(I,9)  == 1) WRITE(IOUT,2000) '9  - POSITIVE SHEAR YZ'
          IF (FMODE(I,10) == 1) WRITE(IOUT,2000) '10 - NEGATIVE SHEAR YZ'
          IF (FMODE(I,11) == 1) WRITE(IOUT,2000) '11 - POSITIVE SHEAR ZX'
          IF (FMODE(I,12) == 1) WRITE(IOUT,2000) '12 - NEGATIVE SHEAR ZX'
#include  "lockoff.inc"
        ENDDO
      ENDIF
c
      IF (NINDX2 > 0) THEN 
        DO J=1,NINDX2    
          I = INDX2(J)
#include  "lockon.inc"
          WRITE(IOUT, 3000) NGL(I),TIME
          WRITE(ISTDO,3000) NGL(I),TIME
#include  "lockoff.inc"
        ENDDO
      ENDIF
c-----------------------------------------------------------------------
 1000 FORMAT(1X,'FAILURE (ORTHENERG) OF SOLID ELEMENT ',I10,1X,',GAUSS PT',
     .       I5,1X,',LAYER',I3)
 2000 FORMAT(1X,'MODE',1X,A)
 3000 FORMAT(1X,'-- RUPTURE OF SOLID ELEMENT : ',I10,1X,'AT TIME : ',1PE12.4)
c-----------------------------------------------------------------------
      END
