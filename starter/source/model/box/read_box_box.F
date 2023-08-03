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
Chd|====================================================================
Chd|  READ_BOX_BOX                  source/model/box/read_box_box.F
Chd|-- called by -----------
Chd|        HM_READ_BOX                   source/model/box/hm_read_box.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_BOX_BOX(IBOX     ,IAD     ,NBOX     ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE OPTIONDEF_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IAD,NBOX
      TYPE (BOX_)  ,DIMENSION(NBBOX)  :: IBOX
      TYPE(SUBMODEL_DATA),DIMENSION(*) ,INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,KK,BOXID,SUB_ID,IDNEG,NLIST,NBOX_POS,NBOX_NEG
      CHARACTER(ncharkey)    :: KEY
      CHARACTER(nchartitle)  :: TITR
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C     IBOX(I)%ID       : BOX IDENTIFIER
C     IBOX(I)%TITLE    : BOX title
C     IBOX(I)%NBOXBOX  : NUMBER OF SUB BOXES (BOXES OF BOXES)
C     IBOX(I)%ISKBOX   : BOX SKEW_ID (RECTA)
C     IBOX(I)%NOD1     : FIRST NODE for box limit definition  - N1 -
C     IBOX(I)%NOD2     : SECOND NODE for box limit definition - N2 -
C     IBOX(I)%TYPE     : BOX SHAPE (1='RECTA',2='CYLIN' ,3='SPHER')
C     IBOX(I)%NBLEVELS : TEMPORARY LEVEL NB OF BOXES
C     IBOX(I)%LEVEL    : FLAG "SUBLEVEL DONE" FOR BOX OF BOXES
C     IBOX(I)%ACTIBOX  : FLAG FOR ACTIVATED BOX FOR (GRNOD,GRSHEL,LINE,SURF...)
C     IBOX(I)%NENTITY  : NUMBER OF BOX ENTITIES (NODES,ELEMS,LINES,SURF)
C                        WITHIN ACTIVATED BOX
C     IBOX(I)%SURFIAD  : temporary address for solid external surface (in box)
C     IBOX(I)%BOXIAD   : temporary address
C     IBOX(I)%DIAM     : BOX diameter (CYLIN + SPHER)
C     IBOX(I)%X1       : coord.X for N1
C     IBOX(I)%Y1       : coord.Y for N1
C     IBOX(I)%Z1       : coord.Z for N1
C     IBOX(I)%X2       : coord.X for N2
C     IBOX(I)%Y2       : coord.Y for N2
C     IBOX(I)%Z2       : coord.Z for N2
C     IBOX(I)%IBOXBOX(NBOXBOX)  : LIST OF BOXES (in /box/box)
C     IBOX(I)%ENTITY(NENTITY)   : LIST OF ENTITIES (NODES,ELEMS,LINES,SURF)
C=======================================================================
c
      CALL HM_OPTION_START('/BOX/BOX')
c
c--------------------------------------------------
      KK = 0
      DO I = 1,NBOX
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,  OPTION_ID   = BOXID ,
     .                                      SUBMODEL_ID = SUB_ID,
     .                                      OPTION_TITR = TITR  ,
     .                                      KEYWORD2    = KEY   )
c-----------------------
        CALL HM_GET_INTV  ('Nbox'     ,NBOX_POS ,IS_AVAILABLE, LSUBMODEL)        
        CALL HM_GET_INTV  ('Nboxneg'  ,NBOX_NEG ,IS_AVAILABLE, LSUBMODEL)
c
        NLIST = NBOX_POS + NBOX_NEG   
c
        IAD = IAD + 1
        IBOX(IAD)%NBOXBOX = NLIST
        CALL MY_ALLOC(IBOX(IAD)%IBOXBOX  ,NLIST)
c
        II = 0
        IF (NBOX_POS > 0) THEN
          DO J=1,NBOX_POS
            II = II + 1
            CALL HM_GET_INT_ARRAY_INDEX('box_ID',IBOX(IAD)%IBOXBOX(II),J,IS_AVAILABLE,LSUBMODEL)
          END DO
        END IF
c
        IF (NBOX_NEG > 0) THEN
          DO J=1,NBOX_NEG    
            II = II + 1
            CALL HM_GET_INT_ARRAY_INDEX('box_IDneg',IDNEG,J,IS_AVAILABLE,LSUBMODEL)
            IBOX(IAD)%IBOXBOX(II) = -IDNEG
          END DO
        END IF
c
        IF (NLIST == 0) THEN
          CALL ANCMSG(MSGID=801, MSGTYPE= MSGERROR,
     .                           ANMODE = ANINFO  ,
     .                           I1     = BOXID   ,
     .                           C1     = TITR    )
        END IF
c
        IBOX(IAD)%TITLE   = TRIM(TITR)
        IBOX(IAD)%ID      = BOXID
        IBOX(IAD)%ISKBOX  = 0
        IBOX(IAD)%NBLEVELS=-1
        IBOX(IAD)%LEVEL   = 0
        IBOX(IAD)%TYPE    = 0
        IBOX(IAD)%ACTIBOX = 0
        IBOX(IAD)%NOD1    = 0
        IBOX(IAD)%NOD2    = 0
        IBOX(IAD)%DIAM    = ZERO
        IBOX(IAD)%X1      = ZERO
        IBOX(IAD)%Y1      = ZERO
        IBOX(IAD)%Z1      = ZERO
        IBOX(IAD)%X2      = ZERO
        IBOX(IAD)%Y2      = ZERO
        IBOX(IAD)%Z2      = ZERO
c
      ENDDO
c-----------
      RETURN
      END
