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
C
!||====================================================================
!||    hm_preread_node        ../starter/source/elements/reader/hm_preread_node.F
!||--- called by ------------------------------------------------------
!||    contrl                 ../starter/source/starter/contrl.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    auto_node_merge        ../starter/source/elements/nodes/auto_node_merge.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_PREREAD_NODE(UNITAB,LSUBMODEL,NUMNUSR,IS_DYNA)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD , ONLY : NSUBMOD, SUBMODEL_DATA
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARFIELD
      USE READER_OLD_MOD , ONLY : LINE
      USE USER_ID_MOD , ONLY : ID_LIMIT
      use my_alloc_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)  :: UNITAB 
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(NSUBMOD)
      INTEGER,INTENT(IN)   :: NUMNUSR
      INTEGER,INTENT(IN)   :: IS_DYNA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: N,I,J,STAT
      INTEGER :: IFLAGUNIT, UID
      my_real :: FAC_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_NOD,UID_NOD
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAB
      my_real, DIMENSION(:,:), ALLOCATABLE :: X
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: HM_X
      REAL*8, DIMENSION(:), ALLOCATABLE :: DMERGE
C-----------------------------------------------      
      FAC_L = ONE
C--------------------------------------------------
C      ALLOCS & INITS
C--------------------------------------------------
      call my_alloc(ITAB, NUMNUSR+NUMCNOD, "ITAB", stat=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='ITAB')
      call my_alloc(X, 3, NUMNUSR+NUMCNOD, "X", stat=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='X')
      call my_alloc(SUB_NOD, NUMNUSR+NUMCNOD, "SUB_NOD", stat=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='SUB_NOD')
      call my_alloc(UID_NOD, NUMNUSR+NUMCNOD, "UID_NOD", stat=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='UID_NOD')
      call my_alloc(HM_X, 3, NUMNUSR+NUMCNOD, "HM_X", stat=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='HM_X')
      call my_alloc(DMERGE, NUMCNOD, "DMERGE", stat=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='DMERGE')
      SUB_NOD(1:NUMNUSR+NUMCNOD) = 0
      UID_NOD(1:NUMNUSR+NUMCNOD) = 0
      DMERGE(1:NUMCNOD) = ZERO
C--------------------------------------------------
C      PRE-READING OF INPUT NODES IN HM STRUCTURE
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
        IF(SUB_NOD(N) /= 0)THEN
          IF(UID_NOD(N) == 0 .AND. LSUBMODEL(SUB_NOD(N))%UID /= 0) UID_NOD(N) = LSUBMODEL(SUB_NOD(N))%UID
        ENDIF
        IF ( ITAB(N) > ID_LIMIT%GLOBAL )THEN
          CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=ITAB(N),C1=LINE,C2='/NODE')
        ENDIF                    
C--------------------------------------------------
C      UNITS
C--------------------------------------------------
        IF(UID_NOD(N) /= UID )THEN
          UID = UID_NOD(N)
          IFLAGUNIT = 0          
          DO J=1,UNITAB%NUNITS                   
            IF (UNITAB%UNIT_ID(J) == UID) THEN  
              FAC_L = UNITAB%FAC_L(J)
              IFLAGUNIT = 1                 
              EXIT                              
            ENDIF                               
          ENDDO                    
          IF (UID/=0 .AND. IFLAGUNIT==0 .AND. I <= NUMNUSR)THEN
            CALL ANCMSG(MSGID=643,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=UID,C1='/NODE')
          ELSEIF (UID/=0 .AND. IFLAGUNIT==0 .AND. I > NUMNUSR)THEN
            CALL ANCMSG(MSGID=643,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=UID,C1='/CNODE')
          ENDIF 
        ENDIF
        X(1,N) = X(1,N)*FAC_L  
        X(2,N) = X(2,N)*FAC_L  
        X(3,N) = X(3,N)*FAC_L 
      ENDDO
      IF(ALLOCATED(SUB_NOD)) CALL MY_DEALLOC(SUB_NOD)
      IF(ALLOCATED(UID_NOD)) CALL MY_DEALLOC(UID_NOD)
      IF(ALLOCATED(HM_X)) CALL MY_DEALLOC(HM_X)
      IF(ALLOCATED(DMERGE)) CALL MY_DEALLOC(DMERGE)
C--------------------------------------------------
C     Check nodes within some tolerance (possibly merge nodes)
C--------------------------------------------------
      CALL AUTO_NODE_MERGE(IS_DYNA,NUMNUSR,NUMCNOD,NUMNOD,ITAB,X)
C--------------------------------
      CALL MY_DEALLOC(ITAB)
      CALL MY_DEALLOC(X)
      RETURN
C--------------------------------
      END
