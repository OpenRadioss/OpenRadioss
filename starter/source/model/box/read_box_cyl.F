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
Chd|  READ_BOX_CYL                  source/model/box/read_box_cyl.F
Chd|-- called by -----------
Chd|        HM_READ_BOX                   source/model/box/hm_read_box.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        SUBROTPOINT                   source/model/submodel/subrot.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_BOX_CYL(
     .           IBOX     ,IAD      ,NBOX     ,ITABM1   ,X        ,
     .           RTRANS   ,UNITAB   ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)    :: NBOX
      INTEGER ,INTENT(INOUT) :: IAD
      INTEGER ,DIMENSION(NUMNOD)  ,INTENT(IN)  :: ITABM1
      my_real            ,DIMENSION(3,NUMNOD)  ,INTENT(IN) :: X
      my_real            ,DIMENSION(NTRANSF,*) ,INTENT(IN) :: RTRANS
      TYPE (UNIT_TYPE_)                        ,INTENT(IN) :: UNITAB 
      TYPE(SUBMODEL_DATA),DIMENSION(*)         ,INTENT(IN) :: LSUBMODEL
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N1,N2,UID,BOXID,SUB_ID,IUNIT,FLAGUNIT
      my_real :: FAC_L,XP1,YP1,ZP1,XP2,YP2,ZP2,DIAM
      CHARACTER(ncharkey)    :: KEY
      CHARACTER(nchartitle)  :: TITR,MESS
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'MULTI-BOX DEFINITION                    '/
C-----------------------------------------------
C     IBOX(I)%ID       : BOX IDENTIFIER
C     IBOX(I)%TITLE    : BOX title
C     IBOX(I)%NBOXBOX  : NUMBER OF SUB BOXES (BOXES OF BOXES)
C     IBOX(I)%ISKBOX   : BOX SKEW_ID (RECTA + CYLIN)
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
C=======================================================================
c
      CALL HM_OPTION_START('/BOX/CYL')
c
c--------------------------------------------------
      DO I = 1,NBOX
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID   = BOXID,
     .                                     UNIT_ID     = UID,
     .                                     SUBMODEL_ID = SUB_ID,
     .                                     OPTION_TITR = TITR,
     .                                     KEYWORD2    = KEY)
c-----------------------
        IF (UID > 0) THEN
          FLAGUNIT = 0
          DO IUNIT=1,NUNITS
            IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN
              FLAGUNIT = 1
              EXIT
            ENDIF
          ENDDO
          IF (UID > 0 .AND. FLAGUNIT == 0) THEN
            CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I2= UID  ,I1=BOXID,
     .                  C1='BOX' ,
     .                  C2='BOX' ,
     .                  C3='TITR')                              
          ENDIF
        ENDIF
c-----------------------
c
        CALL HM_GET_INTV  ('cylinder_base_node'      ,N1    ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV  ('cylinder_direction_node' ,N2    ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOATV('cylinder_diameter'       ,DIAM  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
        CALL HM_GET_FLOATV('cylinder_base_x'         ,XP1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('cylinder_base_y'         ,YP1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('cylinder_base_z'         ,ZP1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
        CALL HM_GET_FLOATV('cylinder_direction_x'    ,XP2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('cylinder_direction_y'    ,YP2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('cylinder_direction_z'    ,ZP2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c-----------------------
c
        IF (N1 > 0 .and. N2 > 0) THEN
c
          CALL HM_GET_FLOATV_DIM('cylinder_base_x',FAC_L,IS_AVAILABLE,LSUBMODEL,UNITAB)
          XP1 = X(1,USR2SYS(N1,ITABM1,MESS,BOXID)) * FAC_L
          YP1 = X(2,USR2SYS(N1,ITABM1,MESS,BOXID)) * FAC_L
          ZP1 = X(3,USR2SYS(N1,ITABM1,MESS,BOXID)) * FAC_L
          XP2 = X(1,USR2SYS(N2,ITABM1,MESS,BOXID)) * FAC_L
          YP2 = X(2,USR2SYS(N2,ITABM1,MESS,BOXID)) * FAC_L
          ZP2 = X(3,USR2SYS(N2,ITABM1,MESS,BOXID)) * FAC_L
c       
        ELSE
c         Submodel rotation 
          IF (SUB_ID > 0) CALL SUBROTPOINT(XP1,YP1,ZP1,RTRANS,SUB_ID,LSUBMODEL)
          IF (SUB_ID > 0) CALL SUBROTPOINT(XP2,YP2,ZP2,RTRANS,SUB_ID,LSUBMODEL)
c       
        ENDIF
c
        IF ((XP1 == ZERO .and. YP1 == ZERO .and. ZP1 == ZERO) .and.
     .      (XP2 == ZERO .and. YP2 == ZERO .and. ZP2 == ZERO)) THEN
          CALL ANCMSG(MSGID=752, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                C1 = 'BOX',
     .                I1 = BOXID,
     .                C2 = TITR ,
     .                C3 = TITR ,
     .                C4 = ' '  )
        END IF
c-----------------------
c
        IAD = IAD + 1
        IBOX(IAD)%TITLE   = TRIM(TITR)
        IBOX(IAD)%ID      = BOXID
        IBOX(IAD)%ISKBOX  = 0
        IBOX(IAD)%NBLEVELS= 0
        IBOX(IAD)%LEVEL   = 1
        IBOX(IAD)%TYPE    = 2
        IBOX(IAD)%ACTIBOX = 0
        IBOX(IAD)%NBOXBOX = 0
        IBOX(IAD)%NOD1    = N1
        IBOX(IAD)%NOD2    = N2
        IBOX(IAD)%DIAM    = DIAM
        IBOX(IAD)%X1      = XP1
        IBOX(IAD)%Y1      = YP1
        IBOX(IAD)%Z1      = ZP1
        IBOX(IAD)%X2      = XP2
        IBOX(IAD)%Y2      = YP2
        IBOX(IAD)%Z2      = ZP2
c
      ENDDO

c-----------
      RETURN
      END
