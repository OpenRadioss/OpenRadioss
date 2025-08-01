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
!||    hm_preread_load_centri   ../starter/source/loads/general/load_centri/hm_preread_load_centri.F
!||--- called by ------------------------------------------------------
!||    lectur                   ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start          ../starter/source/devtools/hm_reader/hm_option_start.F
!||    ngr2usr                  ../starter/source/system/nintrr.F
!||    ngr2usrn                 ../starter/source/system/nintrr.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_PREREAD_LOAD_CENTRI(NUMCENTRI, IGRNOD ,IGRSURF,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT) :: NUMCENTRI
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET, DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER               :: I, ID, IGU, IGS, NN, UID, FLAG_FMT
      INTEGER               :: FLAG_FMT_TMP, IFIX_TMP, ISU, IS, J
      CHARACTER(LEN=NCHARTITLE) :: TITR
      LOGICAL IS_AVAILABLE
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USRN, NGR2USR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C START BROWSING MODEL CLOAD
C--------------------------------------------------
      CALL HM_OPTION_START('/LOAD/CENTRI')
C-----------------------------------------------
      NUMCENTRI  = 0
      DO I=1,NLOADC

        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /LOAD/CENTRI... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV  ('entityid',IGU,IS_AVAILABLE,LSUBMODEL)
C-------
        IGS = NGR2USRN(IGU,IGRNOD,NGRNOD,NN)
        NUMCENTRI = NUMCENTRI + NN
      ENDDO
C-----------
      RETURN

C-----------------------------------------------
      END SUBROUTINE HM_PREREAD_LOAD_CENTRI
