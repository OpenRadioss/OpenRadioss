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
!||    hm_read_ale_grid     ../starter/source/general_controls/ale_grid/hm_read_ale_grid.F
!||--- called by ------------------------------------------------------
!||    contrl               ../starter/source/starter/contrl.F
!||--- calls      -----------------------------------------------------
!||    ancmsg               ../starter/source/output/message/message.F
!||    hm_get_floatv        ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv          ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_count      ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key   ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start      ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod   ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod          ../starter/share/message_module/message_mod.F
!||    submodel_mod         ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_ALE_GRID(DT_INPUT, ALPHA, GAMMA, VGX, VGY, VGZ, VOLMIN, LSUBMODEL, UNITAB)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Input Reader for options /ALE/GRID/...
C  NWALE       FORMULATION
C  0           'DONEA'
C  1           'DISP'
C  2           'SPRING'
C  3           'ZERO'
C  4           'STANDARD'
C  5           'LAPLACIAN'
C  6           'VOLUME'
C  7           'FLOW-TRACKING'
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "com01_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(INOUT) :: DT_INPUT, ALPHA, GAMMA, VGX, VGY, VGZ, VOLMIN
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NALE_GRID, II, NITER,NTETRA4
      CHARACTER :: KEY*32
      LOGICAL :: IS_AVAILABLE
      LOGICAL :: IS_DEFINED_ALE_GRID
      INTEGER :: IS_DEF, IS_ROT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
!     Initializations
      IS_DEFINED_ALE_GRID = .FALSE.
      ALE%GRID%NWALE = 0
      ALPHA = ZERO
      GAMMA = ZERO
      VGX = ZERO
      VGY = ZERO
      VGZ = ZERO
      VOLMIN = ZERO
!     Counting
      CALL HM_OPTION_COUNT('/ALE/GRID', NALE_GRID)

      CALL HM_OPTION_START('/ALE/GRID')
      DO II = 1, NALE_GRID
         CALL HM_OPTION_READ_KEY(LSUBMODEL, KEYWORD3 = KEY)
         IF (KEY(1:5) == 'DONEA') THEN
            IS_DEFINED_ALE_GRID = .TRUE.
            IF(NUMTETRA4 > 0) ALE%GLOBAL%NALENOVS=12
            CALL HM_GET_FLOATV('Alpha', ALPHA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('GAMMA', GAMMA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('VEL_X', VGX, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('VEL_Y', VGY, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('VEL_Z', VGZ, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('MINI', VOLMIN, IS_AVAILABLE, LSUBMODEL, UNITAB)
            IF (GAMMA == ZERO) GAMMA = HUNDRED
            IF (VGX == ZERO) VGX = ONE
            IF (VGY == ZERO) VGY = ONE
            IF (VGZ == ZERO) VGZ = ONE
         ELSEIF (KEY(1:4) == 'DISP') THEN
            IS_DEFINED_ALE_GRID = .TRUE.
            ALE%GRID%NWALE = 1
            CALL HM_GET_FLOATV('MAT_EPS', ALPHA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('MAT_PC', VOLMIN, IS_AVAILABLE, LSUBMODEL, UNITAB)
            IF (ALPHA == ZERO) ALPHA = INFINITY
            VGX = ONE
            VGY = ONE
            VGZ = ONE
         ELSEIF (KEY(1:6) == 'SPRING') THEN
            IS_DEFINED_ALE_GRID = .TRUE.
            IF (N2D /= 0) THEN
               CALL ANCMSG(MSGID = 787, MSGTYPE=MSGERROR, ANMODE=ANINFO,C1='SPRING')
            ENDIF
            ALE%GRID%NWALE = 2
            VGZ = ZERO
            CALL HM_GET_FLOATV('deltaT', ALPHA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('MAT_GAMA0', GAMMA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('DAMP1', VGX, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Nu_l', VGY, IS_AVAILABLE, LSUBMODEL, UNITAB)
            IF (VGX == ZERO) VGX = HALF
            IF (VGY == ZERO) VGY = ONE
            DT_INPUT = ALPHA
            ALPHA = ALPHA / (-VGX + SQRT(VGX * VGX + ONE))
         ELSEIF (KEY(1:8) == 'STANDARD') THEN
            IS_DEFINED_ALE_GRID = .TRUE.
            ALE%GRID%NWALE = 4
            CALL HM_GET_FLOATV('deltaT_min', ALPHA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('MAT_GAMAi', GAMMA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('DAMP4', VGX, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Nu_g', VGY, IS_AVAILABLE, LSUBMODEL, UNITAB)
            IF (ALPHA == ZERO) ALPHA = ZEP9
            IF (GAMMA == ZERO) GAMMA = EM02
            IF (VGX == ZERO) VGX = EM02
            IF (VGY == ZERO) THEN
               CALL ANCMSG(MSGID = 1119, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .                     C1='** CRITICAL ELEMENT SIZE MUST BE DEFINED (4TH FIELD)')
            ENDIF
            IF (GAMMA < ZERO .OR. GAMMA > ONE) THEN
               CALL ANCMSG(MSGID = 1119, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .                     C1='** NON-LINEAR FACTOR MUST BE BETWEEN 0 AND 1 (2ND FIELD)')
            ENDIF
         ELSEIF (KEY(1:4) == 'ZERO') THEN
            IS_DEFINED_ALE_GRID = .TRUE.
            ALE%GRID%NWALE = 3
         ELSEIF (KEY(1:9) == 'LAPLACIAN') THEN
            IS_DEFINED_ALE_GRID = .TRUE.
            ALE%GRID%NWALE = 5
            CALL HM_GET_FLOATV('LAMBDA', ALPHA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INTV('Niter', NITER, IS_AVAILABLE, LSUBMODEL)
            GAMMA = ZERO
            VGX = NITER
            ALPHA = MAX(ZERO, MIN(ALPHA, EP06))
            IF (ALPHA == ZERO) ALPHA = ONE
            IF (GAMMA == ZERO) GAMMA = ONE
            IF (GAMMA /= ONE .AND. GAMMA /= TWO) GAMMA = ONE
            IF (VGX <= ZERO) VGX = ONE
         ELSEIF (KEY(1:6) == 'VOLUME') THEN
            IS_DEFINED_ALE_GRID = .TRUE.
            ALE%GRID%NWALE = 6
         ELSEIF (KEY(1:13) == 'FLOW-TRACKING') THEN
            IS_DEFINED_ALE_GRID = .TRUE.
            ALE%GRID%NWALE = 7
            CALL HM_GET_INTV('IS_DEF', IS_DEF, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('IS_ROT', IS_ROT, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOATV('SCALE_DEF', ALPHA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('SCALE_ROT', GAMMA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            IF(IS_DEF == 0)IS_DEF = 1
            IF(IS_DEF /= 1 .AND. IS_DEF /= -1)IS_DEF = 1 !value different than expected values leads to default value
            IF(IS_ROT == 0)IS_ROT = 1
            IF(IS_ROT /= 1 .AND. IS_ROT /= -1)IS_ROT = 1 !value different than expected values leads to default value
            IF (ALPHA == ZERO) ALPHA = ONE
            IF (GAMMA == ZERO) GAMMA = ONE
            VGX = IS_DEF
            VGY = IS_ROT
            VGZ = ZERO
         ELSEIF (KEY(1:8) == 'LAGRANGE') THEN
            IS_DEFINED_ALE_GRID = .TRUE.
            ALE%GRID%NWALE = 8
         ELSE
           !invalid keyword : error message managed by Reader itself
         ENDIF

      ENDDO

       ! NO /ALE/GRID formulation => DEFAULT:'DISP' & its default values are set here
       IF(.NOT. IS_DEFINED_ALE_GRID .OR. NALE_GRID == 0)THEN
         ALE%GRID%NWALE = 1
         ALPHA = INFINITY
         VGX = ONE
         VGY = ONE
         VGZ = ONE
       ENDIF

       IF(NALE_GRID > 1)THEN
         ! multiple definition of /ALE/GRID : error message eliminates any ambiguity
          CALL ANCMSG(MSGID = 1118, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .    C1='ONLY A SINGLE DEFINITION OF GRID SMOOTHING ALGORITHM IS ALLOWED')
       ENDIF

      END SUBROUTINE HM_READ_ALE_GRID
