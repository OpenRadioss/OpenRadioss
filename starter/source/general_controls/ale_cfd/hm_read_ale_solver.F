Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    hm_read_ale_solver   ../starter/source/general_controls/ale_cfd/hm_read_ale_solver.F
!||--- called by ------------------------------------------------------
!||    contrl               ../starter/source/starter/contrl.F
!||--- calls      -----------------------------------------------------
!||    ancmsg               ../starter/source/output/message/message.F
!||    hm_get_intv          ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_count      ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_next       ../starter/source/devtools/hm_reader/hm_option_next.F
!||    hm_option_start      ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod   ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod          ../starter/share/message_module/message_mod.F
!||    submodel_mod         ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_ALE_SOLVER(LSUBMODEL, UNITAB, ICAA, ISFINT, IS_ALE_SOLVER_MULTIMAT)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is reading /ALE/SOLVER/FINT option in user input file
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARLINE
      USE ALE_MOD , ONLY : ALE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
      INTEGER, INTENT(IN) :: ICAA
      INTEGER, INTENT(INOUT) :: ISFINT
      INTEGER,INTENT(INOUT) :: IS_ALE_SOLVER_MULTIMAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NALESOLVER_FINT
      INTEGER :: NALESOLVER_MULTIMAT
      INTEGER :: IAD, NFIX, NB, OPT_ID
      INTEGER :: IFORM, SURF_ID, SURF_ID2, SURF_ID3, SURF_ID4, SURF_ID5
      INTEGER :: II
      LOGICAL :: IS_AVAILABLE

      !---------------------------------------------------------------------------!
      !  /ALE/SOLVER/FINT - Integration formulation for div.sigma (momentum eqn)  !
      !---------------------------------------------------------------------------!
      CALL HM_OPTION_COUNT('/ALE/SOLVER/FINT', NALESOLVER_FINT)
      IF (NALESOLVER_FINT > 0) THEN
         CALL HM_OPTION_START('/ALE/SOLVER/FINT')
         CALL HM_OPTION_NEXT()
         CALL HM_GET_INTV('Iform', ISFINT, IS_AVAILABLE, LSUBMODEL)
         IF (ISFINT < 0 .OR. ISFINT > 3)THEN
           CALL ANCMSG(MSGID=1138, MSGTYPE=MSGERROR, ANMODE=ANINFO, C1='ISFINT', I1=ISFINT)
         ENDIF
         IF (ISFINT == 2 .AND. ICAA == 1)THEN
             CALL ANCMSG(MSGID=1138, MSGTYPE=MSGERROR, ANMODE=ANINFO, C1='ISFINT=2 NOT COMPATIBLE  WITH CAA', I1=ICAA)
         ENDIF
         IF (ISFINT == 0) ISFINT = 3
      ENDIF

      !---------------------------------------------------------------------------!
      !  /ALE/SOLVER/FINT - Integration formulation for div.sigma (momentum eqn)  !
      !---------------------------------------------------------------------------!
      CALL HM_OPTION_COUNT('/ALE/SOLVER/MULTIMAT', NALESOLVER_MULTIMAT)
      IS_ALE_SOLVER_MULTIMAT = 0
      ALE%SOLVER%MULTIMAT%NB = NALESOLVER_MULTIMAT
      ALE%SOLVER%MULTIMAT%is_defined_mmale1 = 0
      ALE%SOLVER%MULTIMAT%is_defined_mmale2 = 0
      ALE%SOLVER%MULTIMAT%is_defined_mmale3 = 0
      IF (NALESOLVER_MULTIMAT > 0) THEN
         IS_ALE_SOLVER_MULTIMAT = 1
          CALL ALE%SOLVER%MULTIMAT%CONSTRUCT()
          do ii=1,NALESOLVER_MULTIMAT
            call hm_option_read_key(lsubmodel, option_id=opt_id)
            CALL HM_OPTION_START('/ALE/SOLVER/MULTIMAT')
            CALL HM_OPTION_NEXT()
            CALL HM_GET_INTV('ALE_SOLVER_MMALE', IFORM  , IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('ALE_SURF_SETID'  , SURF_ID, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('ALE_SURF_SETID2' , SURF_ID2, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('ALE_SURF_SETID3' , SURF_ID3, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('ALE_SURF_SETID4' , SURF_ID4, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('ALE_SURF_SETID5' , SURF_ID5, IS_AVAILABLE, LSUBMODEL)

            IF (IFORM < 0 .OR. IFORM > 4)THEN
              CALL ANCMSG(MSGID=1138, MSGTYPE=MSGERROR, ANMODE=ANINFO, C1='ALE/SOLVER/MULTIMAT,IFORM=', I1=IFORM)
              IFORM = 0
            ENDIF
            IF (SURF_ID <= 0)THEN
              CALL ANCMSG(MSGID=1138, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .        C1='ALE/SOLVER/MULTIMAT,POLYGON NOT PROVIDED : polyg_id=', I1=IFORM)
              IFORM = 0
            ENDIF
            IF(IFORM == 1)ALE%SOLVER%MULTIMAT%is_defined_mmale1 = 1
            IF(IFORM == 2)ALE%SOLVER%MULTIMAT%is_defined_mmale2 = 1
            IF(IFORM == 3)ALE%SOLVER%MULTIMAT%is_defined_mmale3 = 1
            IF(SURF_ID  < 0)SURF_ID  = 0
            IF(SURF_ID2 < 0)SURF_ID2 = 0
            IF(SURF_ID3 < 0)SURF_ID3 = 0
            IF(SURF_ID4 < 0)SURF_ID4 = 0
            IF(SURF_ID5 < 0)SURF_ID5 = 0
            ALE%SOLVER%MULTIMAT%LIST(II)%mmale = IFORM
            ALE%SOLVER%MULTIMAT%LIST(II)%surf_id  = SURF_ID
            ALE%SOLVER%MULTIMAT%LIST(II)%surf_id2 = SURF_ID2
            ALE%SOLVER%MULTIMAT%LIST(II)%surf_id3 = SURF_ID3
            ALE%SOLVER%MULTIMAT%LIST(II)%surf_id4 = SURF_ID4
            ALE%SOLVER%MULTIMAT%LIST(II)%surf_id5 = SURF_ID5
            write(iout,100) iform,SURF_ID,SURF_ID2,SURF_ID3,SURF_ID4,SURF_ID5
          ENDDO

      ENDIF

      RETURN

100   FORMAT(/"ALE BI-MATERIAL CUT-CELL ",/,
     .       5X,/,
     .       5X,"FORMULATION FLAG . . . . . . . . . . . . . .=",I10/,
     .       5X,"POLYGON ID_1 . . . . . . . . . . . . . . . .=",I10/,
     .       5X,"POLYGON ID_2 (OPTIONAL)  . . . . . . . . . .=",I10/,
     .       5X,"POLYGON ID_3 (OPTIONAL)  . . . . . . . . . .=",I10/,
     .       5X,"POLYGON ID_4 (OPTIONAL)  . . . . . . . . . .=",I10/,
     .       5X,"POLYGON ID_5 (OPTIONAL)  . . . . . . . . . .=",I10/)

      END SUBROUTINE HM_READ_ALE_SOLVER