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
!||    hm_preread_impacc    ../starter/source/constraints/general/impvel/hm_preread_impacc.F
!||--- called by ------------------------------------------------------
!||    lectur               ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    hm_get_intv          ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_count      ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key   ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start      ../starter/source/devtools/hm_reader/hm_option_start.F
!||    ngr2usrn             ../starter/source/system/nintrr.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod   ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod         ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_PREREAD_IMPACC(NUM    ,IGRNOD,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
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
      INTEGER ,INTENT(INOUT)  :: NUM
      TYPE (GROUP_)      ,DIMENSION(NGRNOD)  ,INTENT(IN) :: IGRNOD
      TYPE(SUBMODEL_DATA),DIMENSION(*)       ,INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID,IGU,IGS,NN,NIMPACC
      CHARACTER(nchartitle)  :: TITR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USRN
C=======================================================================
      IS_AVAILABLE = .FALSE.
      NUM = 0
c--------------------------------------------------
c     COUNT /IMPACC Options
c--------------------------------------------------
      CALL HM_OPTION_COUNT('/IMPACC',NIMPACC)
C--------------------------------------------------
C START BROWSING MODEL IMPACC
C--------------------------------------------------
      CALL HM_OPTION_START('/IMPACC')
C--------------------------------------------------
C BROWSING MODEL IMPACC 1-> NIMPACC
C--------------------------------------------------
      DO I=1,NIMPACC
        TITR = ''
C--------------------------------------------------
C EXTRACT DATA OF /IMPACC/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID = ID, OPTION_TITR = TITR)
C--------------------------------------------------
C EXTRACT DATA (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('entityid',IGU,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
        IGS = NGR2USRN(IGU,IGRNOD,NGRNOD,NN)
        NUM = NUM + NN
C--------------------------------------------------
      ENDDO  
C-----------
      RETURN
      END SUBROUTINE HM_PREREAD_IMPACC
