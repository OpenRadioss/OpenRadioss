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
Chd|  HM_READ_ALE_GRID              source/general_controls/ale_grid/hm_read_ale_grid.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_ALE_GRID(DT_INPUT, ALPHA, GAMMA, VGX, VGY, VGZ, VOLMIN, LSUBMODEL, UNITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "submod_c.inc"
#include "nchar_c.inc"
#include "param_c.inc"
#include "com01_c.inc"
#include "com04_c.inc"
#include "ale_param.inc"
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
      CHARACTER :: KEY*ncharkey
      LOGICAL :: IS_AVAILABLE

!     Initializations
      NWALE = 0
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
            IF(NUMTETRA4 > 0) NALENOVS=12
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
            NWALE = 1
            CALL HM_GET_FLOATV('MAT_EPS', ALPHA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('VEL_X', VGX, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('VEL_Y', VGY, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('VEL_Z', VGZ, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('MAT_PC', VOLMIN, IS_AVAILABLE, LSUBMODEL, UNITAB)
            IF (ALPHA == ZERO) ALPHA = INFINITY
            IF (VGX == ZERO) VGX = ONE
            IF (VGY == ZERO) VGY = ONE
            IF (VGZ == ZERO) VGZ = ONE
         ELSEIF (KEY(1:6) == 'SPRING') THEN
            IF (N2D /= 0) THEN
               CALL ANCMSG(MSGID = 787, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .              C1='SPRING')
            ENDIF 
            NWALE = 2
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
            NWALE = 4
            CALL HM_GET_FLOATV('deltaT_min', ALPHA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('MAT_GAMAi', GAMMA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('DAMP4', VGX, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Nu_g', VGY, IS_AVAILABLE, LSUBMODEL, UNITAB)
            IF (ALPHA == ZERO) ALPHA = ZEP9
            IF (GAMMA == ZERO) GAMMA = EM02
            IF (VGX == ZERO) VGX = EM02
            IF (VGY == ZERO) THEN
               CALL ANCMSG(MSGID = 1119, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .              C1='** CRITICAL ELEMENT SIZE MUST BE DEFINED (4TH FIELD)')
            ENDIF
            IF (GAMMA < ZERO .OR. GAMMA > ONE) THEN
               CALL ANCMSG(MSGID = 1119, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .              C1='** NON-LINEAR FACTOR MUST BE BETWEEN 0 AND 1 (2ND FIELD)')
            ENDIF
         ELSEIF (KEY(1:4) == 'ZERO') THEN
            NWALE = 3
         ELSEIF (KEY(1:9) == 'LAPLACIAN') THEN
            NWALE = 5
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
            NWALE = 6
         ENDIF
        
      ENDDO
      
       ! NO /ALE/GRID formulation => DEFAULT = DONEA & its default values are set here
       IF(NWALE==0 .OR. NWALE==-1)THEN
         NWALE=0
         CALL HM_OPTION_COUNT('/TETRA4', NTETRA4)
         IF(NTETRA4 > 0) NALENOVS=12              
         IF(GAMMA == ZERO) GAMMA=HUNDRED
         IF(VGX == ZERO)   VGX=ONE
         IF(VGY == ZERO)   VGY=ONE
         IF(VGZ == ZERO)   VGZ=ONE
       ENDIF      

      END SUBROUTINE HM_READ_ALE_GRID
