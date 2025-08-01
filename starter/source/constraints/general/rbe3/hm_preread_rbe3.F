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
!||    hm_preread_rbe3          ../starter/source/constraints/general/rbe3/hm_preread_rbe3.F
!||--- called by ------------------------------------------------------
!||    lectur                   ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    hm_get_int_array_index   ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start          ../starter/source/devtools/hm_reader/hm_option_start.F
!||    hm_sz_r2r                ../starter/source/coupling/rad2rad/routines_r2r.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    r2r_mod                  ../starter/share/modules1/r2r_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_PREREAD_RBE3(LNUM ,LREAL,  IGRNOD, GRNOD_UID,LSUBMODEL)
C-------------------------------------
C     LECTURE STRUCTURE RIGIDES 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: LNUM,LREAL
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      INTEGER :: GRNOD_UID
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, NSLT,NM,INGU,IGS,J,NN,NRB,UID,ID
      LOGICAL :: IS_AVAILABLE
      CHARACTER(LEN=NCHARTITLE) :: TITR
C=======================================================================
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/RBE3')
      LNUM  = 0
      LREAL  = 0
      NRB = 0
      IF (NRBE3 == 0) RETURN
      DO I=1,NRBE3
        NRB=NRB+1
C----------Multidomaines --> on ignore les rbe3 non tages---------
        IF(NSUBDOM > 0)THEN
          IF(TAGRB3(NRB) == 0)CALL HM_SZ_R2R(TAGRB3,NRB,LSUBMODEL)
        END IF
C--------------------------------------------------
C EXTRACT DATAS OF /RBE3/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID = ID,UNIT_ID = UID,OPTION_TITR = TITR)
        CALL HM_GET_INTV('nset',NSLT,IS_AVAILABLE,LSUBMODEL)
        NM = 0
        DO J=1,NSLT
         CALL HM_GET_INT_ARRAY_INDEX('independentnodesets',INGU,J,IS_AVAILABLE,LSUBMODEL)
         IF(INGU == 0) THEN 
           NN = 0
           IGS = 0
         ELSE
           CALL C_HASH_FIND(GRNOD_UID,INGU,IGS)
           IF(IGS == 0) THEN 
             NN = 0 
           ELSE
             NN = IGRNOD(IGS)%NENTITY 
           ENDIF
         ENDIF
         NM = NM + NN
        ENDDO
       LREAL  = LREAL + NM
       LNUM  = LNUM +NRBE3L
      ENDDO
      RETURN
      END SUBROUTINE HM_PREREAD_RBE3
