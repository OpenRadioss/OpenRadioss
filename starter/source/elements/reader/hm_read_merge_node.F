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
Chd|  HM_READ_MERGE_NODE            source/elements/reader/hm_read_merge_node.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MERGE_NODE(X,LSUBMODEL,UNITAB,IGRNOD,MERGE_NODE_TAB,
     .                              MERGE_NODE_TOL,NMERGE_NODE_CAND,NMERGE_NODE_DEST)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MERGE_NODE_TAB(4,*),NMERGE_NODE_CAND,NMERGE_NODE_DEST
      my_real
     .   X(3,*),MERGE_NODE_TOL(*)
      TYPE(UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,UID,IDN,GR_ID,GR_IDS,MERGE_TYPE,FLAG_FULL_MERGE
      CHARACTER TITR*nchartitle
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGNOD1,TAGNOD2
      LOGICAL   IS_AVAILABLE
      my_real
     .   TOL,XN,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,DX,DY,DZ,TOL_DEF
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C=====================================================================
C
      WRITE(IOUT,1000)
C
      TOL_DEF = ZERO
      FLAG_FULL_MERGE = 0
      ALLOCATE(TAGNOD1(NUMNOD),TAGNOD2(NUMNOD))
      TAGNOD1 = 0
      TAGNOD2 = 0
C
C--------------------------------------------------
C START BROWSING MODEL MERGE
C--------------------------------------------------
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/MERGE/NODE')
C--------------------------------------------------
      DO I=1,NB_MERGE_NODE
C--------------------------------------------------
C EXTRACT DATAS OF /RBODY/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       OPTION_TITR = TITR)

C--------------------------------------------------
C EXTRACT DATAS
C--------------------------------------------------
        CALL HM_GET_FLOATV('tol'      ,TOL               ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
        CALL HM_GET_INTV  ('Type'     ,MERGE_TYPE        ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV  ('grnod_id' ,GR_ID             ,IS_AVAILABLE, LSUBMODEL)
C
        GR_IDS = 0
        IF (GR_ID > 0) THEN
C--       Grnod 
          DO J=1,NGRNOD
            IF (IGRNOD(J)%ID == GR_ID) GR_IDS = J
          ENDDO
          IF (GR_IDS == 0) THEN
            CALL ANCMSG(MSGID=53,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1='IN /MERGE/NODE DEFINITION',
     .                  I1=GR_ID)
          ENDIF
        ENDIF
C
        IF ((TOL_DEF == ZERO).AND.(TOL == ZERO)) THEN
C--     computation of default tolerance (only one time)
          XN=NUMNOD
          XMIN =  EP20
          XMAX = -EP20
          YMIN =  EP20
          YMAX = -EP20
          ZMIN =  EP20
          ZMAX = -EP20
          DO J = 1,NUMNOD
            XMIN = MIN(XMIN,X(1,J))       
            XMAX = MAX(XMAX,X(1,J))       
            YMIN = MIN(YMIN,X(2,J))       
            YMAX = MAX(YMAX,X(2,J))       
            ZMIN = MIN(ZMIN,X(3,J))       
            ZMAX = MAX(ZMAX,X(3,J))     
          END DO
          DX  = XMAX-XMIN
          DY  = YMAX-YMIN
          DZ  = ZMAX-ZMIN
          TOL_DEF = EM05*(DX+DY+DZ)/(THREE*EXP(THIRD*LOG(XN)))
        ENDIF        
C
        IF (GR_IDS > 0) THEN
C--       Grnod id is defined
          IF (MERGE_TYPE == 0) MERGE_TYPE = 1
          DO J=1,IGRNOD(GR_IDS)%NENTITY
            TAGNOD1(IGRNOD(GR_IDS)%ENTITY(J)) = 1
            TAGNOD2(IGRNOD(GR_IDS)%ENTITY(J)) = 1
          ENDDO
          IF (MERGE_TYPE == 2) TAGNOD2(1:NUMNOD) = 1
        ELSE
C--       all nodes taken into account
          IF (FLAG_FULL_MERGE == 0) THEN
            FLAG_FULL_MERGE = ID
          ELSE
            CALL ANCMSG(MSGID=2036,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,I2=FLAG_FULL_MERGE)
          ENDIF
          GR_IDS=0
          TAGNOD1(1:NUMNOD) = 1
          TAGNOD2(1:NUMNOD) = 1
          MERGE_TYPE = 1
        ENDIF
C
        IF (TOL == ZERO) TOL = TOL_DEF
        NMERGE_NODE_CAND = 0
        NMERGE_NODE_DEST = 0
        DO J=1,NUMNOD
          IF (TAGNOD1(J) == 1) NMERGE_NODE_CAND = NMERGE_NODE_CAND + 1
          IF (TAGNOD2(J) == 1) NMERGE_NODE_DEST = NMERGE_NODE_DEST + 1
        ENDDO
        MERGE_NODE_TAB(1,I) = MERGE_TYPE
        MERGE_NODE_TAB(2,I) = GR_IDS
        MERGE_NODE_TAB(3,I) = GR_ID
        MERGE_NODE_TAB(4,I) = ID
        MERGE_NODE_TOL(I) = TOL
C
        WRITE(IOUT,1100) ID,TRIM(TITR),TOL,MERGE_TYPE,GR_ID    
C
      ENDDO
C
C-----------
      RETURN
C
1000  FORMAT(/
     . '      MERGE/NODE DEFINITIONS '/
     . '      ---------------------- ')
1100  FORMAT(/5X,'MERGE NODE ID ',I10,1X,A
     .       /5X,'TOLERANCE. . . . . . . . . . . . . . . . .',1PG20.4
     .       /5X,'MERGING TYPE. . . . . . . . . . . . . . . ',I10
     .       /5X,'GROUP OF NODES. . . . . . . . . . . . . . ',I10)
      END 
C
