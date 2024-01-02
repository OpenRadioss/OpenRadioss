Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  HM_READ_SUBMODEL              source/model/assembling/hm_read_submodel.F
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SUBMODEL(LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ //SUBMODELs USING HM_READER
C   BUILD //SUBMODEL DATA STRUCTURE
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C    
C     LSUBMODEL    SUBMODEL STRUCTURE       
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE SUBMODEL_MOD   
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C OUTPUT ARGUMENTS
      TYPE(SUBMODEL_DATA),INTENT(OUT)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,CUR_SUBMOD,ISUB_HIERARCHY,ISUBSKEW,STAT
      INTEGER, DIMENSION(:), ALLOCATABLE :: NOSUBMOD,IFATHER,LEVEL,UID_SUB
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: OFFSETS
      INTEGER  NB_BEGIN,SCHAR,SUB_INDEX,NBUNIT_SUB
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C--------------------------------------------------------
      IF(NSUBMOD > 0)THEN
        ALLOCATE (NOSUBMOD(NSUBMOD),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='NOSUBMOD')
        ALLOCATE (IFATHER(NSUBMOD),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IFATHER')
        ALLOCATE (LEVEL(NSUBMOD),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='LEVEL')
        ALLOCATE (OFFSETS(7,NSUBMOD),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='OFFSETS')
        ALLOCATE (UID_SUB(NSUBMOD),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='UID_SUB')
        NOSUBMOD = 0
        IFATHER = 0
        LEVEL = 1
        LSUBMODEL(1:NSUBMOD)%NOSUBMOD=0
        LSUBMODEL(1:NSUBMOD)%NBTRANS=0
        LSUBMODEL(1:NSUBMOD)%LEVEL=0
        LSUBMODEL(1:NSUBMOD)%IFATHER=0
        LSUBMODEL(1:NSUBMOD)%UID=0
        UID_SUB(1:NSUBMOD)=0
        OFFSETS(1:7,1:NSUBMOD)=0
      ELSE
        ALLOCATE (NOSUBMOD(0),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='NOSUBMOD')
        ALLOCATE (IFATHER(0),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IFATHER')
        ALLOCATE (LEVEL(0),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='LEVEL')
        ALLOCATE (OFFSETS(0,0),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='OFFSETS')
        ALLOCATE (UID_SUB(0),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='UID_SUB')
      ENDIF
C
      IF(NSUBMOD > 0) CALL CPP_SUBMODEL_BUILD(IFATHER,NOSUBMOD,LEVEL,OFFSETS,UID_SUB)
C
      DO I=1,NSUBMOD
        LSUBMODEL(I)%IFATHER = IFATHER(I)
        LSUBMODEL(I)%NOSUBMOD = NOSUBMOD(I) 
        LSUBMODEL(I)%LEVEL = LEVEL(I)  
        LSUBMODEL(I)%UID = UID_SUB(I)
      ENDDO
c
      ISUBSKEW = 1000000000
      DO I=1,NSUBMOD
        ISUBSKEW = ISUBSKEW + 1
        IF (LSUBMODEL(I)%SKEW == 0) LSUBMODEL(I)%SKEW = ISUBSKEW
      ENDDO
      IF (NSUBMOD /= 0) THEN
        DO I=1,NSUBMOD
          CUR_SUBMOD = I
          DO WHILE (LSUBMODEL(CUR_SUBMOD)%IFATHER /= 0)
            CUR_SUBMOD = LSUBMODEL(CUR_SUBMOD)%IFATHER
            LSUBMODEL(I)%LEVEL = LSUBMODEL(I)%LEVEL + 1
          ENDDO
        ENDDO
      ENDIF
C
      NBUNIT_SUB = 0
      IF(NSUBMOD > 0)THEN
        CALL HM_OPTION_COUNT('/BEGIN',NB_BEGIN)
        SCHAR = 20
        IF (NB_BEGIN /= 0) THEN
          CALL HM_OPTION_START('/BEGIN')
          DO I=1,NB_BEGIN
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              SUBMODEL_INDEX = SUB_INDEX)
            IF (SUB_INDEX /= 0) THEN
              NBUNIT_SUB = NBUNIT_SUB + 1
              LSUBMODEL(SUB_INDEX)%UID = ID_UNIT_AUTO + NBUNIT_SUB
            ENDIF
          ENDDO
c
          
        ENDIF
      ENDIF
C
c      print *,'INTERNAL_ID,USER_ID,FATHER,OFF_DEF,UID'
c      DO I=1,NSUBMOD    
c        print *,I,LSUBMODEL(I)%NOSUBMOD,LSUBMODEL(I)%IFATHER,LSUBMODEL(I)%OFF_DEF,
c     .                                            LSUBMODEL(I)%UID
c      ENDDO
c
      IF (ALLOCATED(NOSUBMOD)) DEALLOCATE(NOSUBMOD)
      IF (ALLOCATED(IFATHER)) DEALLOCATE(IFATHER)
      IF (ALLOCATED(LEVEL)) DEALLOCATE(LEVEL)
      IF (ALLOCATED(OFFSETS)) DEALLOCATE(OFFSETS)
C-----------
      RETURN
      END SUBROUTINE HM_READ_SUBMODEL
