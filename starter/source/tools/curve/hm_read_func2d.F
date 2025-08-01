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
!||    hm_read_func2d             ../starter/source/tools/curve/hm_read_func2d.F
!||--- called by ------------------------------------------------------
!||    lectur                     ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key         ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start            ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    func2d_mod                 ../starter/share/modules1/func2d_mod.F
!||    hm_option_read_mod         ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    submodel_mod               ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FUNC2D(FUNC2D, LSUBMODEL, UNITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE FUNC2D_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARLINE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C NFUNCT
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(FUNC2D_STRUCT), DIMENSION(NFUNC2D), INTENT(INOUT) :: FUNC2D
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------.
      INTEGER :: KK, ID, UID, NPT, IPT, DIM, REC_START
      CHARACTER(LEN=NCHARLINE) :: TITR
      LOGICAL :: IS_AVAILABLE

      CALL HM_OPTION_START('/FUNC_2D')
      
      DO KK = 1, NFUNC2D
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID)
         CALL HM_GET_INTV('dim', DIM, IS_AVAILABLE, LSUBMODEL)
         

         FUNC2D(KK)%ID = ID
         NPT = 0
         FUNC2D(KK)%DIM = DIM

         CALL HM_GET_INTV('FUNC_size', NPT, IS_AVAILABLE, LSUBMODEL)
         FUNC2D(KK)%NPT = NPT
         ALLOCATE(FUNC2D(KK)%XVAL(2, NPT), FUNC2D(KK)%FVAL(DIM, NPT))

         SELECT CASE (DIM)
         CASE(1)
            DO IPT = 1, NPT
               CALL HM_GET_FLOAT_ARRAY_INDEX('X', FUNC2D(KK)%XVAL(1, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Y', FUNC2D(KK)%XVAL(2, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Z1', FUNC2D(KK)%FVAL(1, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
            ENDDO
         CASE (2)
            DO IPT = 1, NPT
               CALL HM_GET_FLOAT_ARRAY_INDEX('X', FUNC2D(KK)%XVAL(1, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Y', FUNC2D(KK)%XVAL(2, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Z1', FUNC2D(KK)%FVAL(1, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Z2', FUNC2D(KK)%FVAL(2, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
            ENDDO
         CASE (3)
            DO IPT = 1, NPT
               CALL HM_GET_FLOAT_ARRAY_INDEX('X', FUNC2D(KK)%XVAL(1, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Y', FUNC2D(KK)%XVAL(2, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Z1', FUNC2D(KK)%FVAL(1, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Z2', FUNC2D(KK)%FVAL(2, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Z3', FUNC2D(KK)%FVAL(3, IPT), IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
            ENDDO
         CASE DEFAULT
C     TODO(DC) : Throw error here
         END SELECT
      ENDDO
      RETURN
      END SUBROUTINE HM_READ_FUNC2D
