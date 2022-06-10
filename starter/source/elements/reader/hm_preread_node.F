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
C
Chd|====================================================================
Chd|  HM_PREREAD_NODE               source/elements/reader/hm_preread_node.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        AUTO_NODE_MERGE               source/elements/nodes/auto_node_merge.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_NODE(UNITAB,LSUBMODEL,NUMNUSR,IS_DYNA)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE UNITAB_MOD
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
#include      "hash_id.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "com_xfem1.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr06_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "titr_c.inc"
#include      "sphcom.inc"
#include      "remesh_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)  :: UNITAB 
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
      INTEGER,INTENT(IN)   :: NUMNUSR
      INTEGER,INTENT(IN)   :: IS_DYNA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I,J,IERROR, STAT
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,IFLAGUNIT, UID, ID
      INTEGER OFF_NOD(NSUBMOD), OFF_DEF(NSUBMOD)
C     REAL
      my_real
     .    FAC_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_NOD,UID_NOD
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAB
      my_real, DIMENSION(:,:), ALLOCATABLE ::
     .                                       X
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: HM_X
      REAL*8, DIMENSION(:), ALLOCATABLE :: DMERGE
      CHARACTER KEY*ncharfield
      LOGICAL IS_AVAILABLE
C-----------------------------------------------      
      FAC_L = ONE
C-----------------------------------------------
C      HM OFFSETS IDs
C--------------------------------------------------
      DO I=1,NSUBMOD
        OFF_NOD(I) = LSUBMODEL(I)%OFF_NOD
        OFF_DEF(I) = LSUBMODEL(I)%OFF_DEF
      ENDDO  
C--------------------------------------------------
C      ALLOCS & INITS
C--------------------------------------------------
      ALLOCATE (ITAB(NUMNUSR+NUMCNOD)       ,STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ITAB')
      ALLOCATE (X(3,NUMNUSR+NUMCNOD)       ,STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='X')
      ALLOCATE (SUB_NOD(NUMNUSR+NUMCNOD),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_NOD') 
      ALLOCATE (UID_NOD(NUMNUSR+NUMCNOD),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='UID_NOD')  
      ALLOCATE (HM_X(3,NUMNUSR+NUMCNOD),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='HM_X') 
      ALLOCATE (DMERGE(NUMCNOD),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='DMERGE') 
      SUB_NOD(1:NUMNUSR+NUMCNOD) = 0
      UID_NOD(1:NUMNUSR+NUMCNOD) = 0
      DMERGE(1:NUMCNOD) = ZERO
C--------------------------------------------------
C      PRE-LECTURE DES NODES INPUTS DANS HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_NODES_READ(ITAB,HM_X,DMERGE,SUB_NOD,UID_NOD)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      UID = -1 
      N=0                                              
      DO I=1,NUMNUSR+NUMCNOD                                        
        N=N+1  
        X(1,N) = HM_X(1,N)
        X(2,N) = HM_X(2,N)
        X(3,N) = HM_X(3,N)
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUB_NOD(N) .NE. 0)THEN
          IF(ITAB(N) .NE. 0) ITAB(N) = ITAB(N) + OFF_NOD(SUB_NOD(N))
          IF(UID_NOD(N) .NE. 0) UID_NOD(N) = UID_NOD(N) + OFF_DEF(SUB_NOD(N))
          IF(UID_NOD(N) == 0 .AND. LSUBMODEL(SUB_NOD(N))%UID .NE. 0) 
     .        UID_NOD(N) = LSUBMODEL(SUB_NOD(N))%UID
        ENDIF
        IF ( ITAB(N) > ID_LIMIT )THEN
          CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ITAB(N),C1=LINE,C2='/NODE')
        ENDIF                    
C--------------------------------------------------
C      UNITS
C--------------------------------------------------
        IF(UID_NOD(N) .NE. UID )THEN
          UID = UID_NOD(N)
          IFLAGUNIT = 0          
          DO J=1,NUNITS                   
            IF (UNITAB%UNIT_ID(J) == UID) THEN  
              FAC_L = UNITAB%FAC_L(J)                 
              IFLAGUNIT = 1                 
              EXIT                              
            ENDIF                               
          ENDDO                    
          IF (UID.NE.0 .AND. IFLAGUNIT.EQ.0 .AND. I .LE. NUMNUSR)THEN
            CALL ANCMSG(MSGID=643,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=UID,C1='/NODE')
          ELSEIF (UID.NE.0 .AND. IFLAGUNIT.EQ.0 .AND. I .GT. NUMNUSR)THEN
            CALL ANCMSG(MSGID=643,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=UID,C1='/CNODE')
          ENDIF 
        ENDIF
        X(1,N) = X(1,N)*FAC_L  
        X(2,N) = X(2,N)*FAC_L  
        X(3,N) = X(3,N)*FAC_L 
      ENDDO
      IF(ALLOCATED(SUB_NOD)) DEALLOCATE(SUB_NOD)
      IF(ALLOCATED(UID_NOD)) DEALLOCATE(UID_NOD)
      IF(ALLOCATED(HM_X)) DEALLOCATE(HM_X)
      IF(ALLOCATED(DMERGE)) DEALLOCATE(DMERGE)                             
C--------------------------------------------------
C     Check nodes within some tolerance (possibly merge nodes)
C--------------------------------------------------
      CALL AUTO_NODE_MERGE(IS_DYNA,NUMNUSR,NUMCNOD,NUMNOD,ITAB,X)
C--------------------------------
      DEALLOCATE(ITAB,X)
      RETURN
C--------------------------------
 999  CALL FREERR(1)
      RETURN
      END
