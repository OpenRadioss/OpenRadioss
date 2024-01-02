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
Chd|  HM_SUBMODGRN                  source/groups/hm_submodgr.F   
Chd|-- called by -----------
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_SUBMODGRN(
     .           ITAB   ,ITABM1    ,ISUBMOD  ,SID     ,
     .           NNOD   ,MESS      ,FLAG     ,TITR    ,
     .           TITR1  ,LSUBMODEL ,IGRNOD   ,NN      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE GROUPDEF_MOD
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
      INTEGER  ITAB(*),ITABM1(*),ISUBMOD(*)
      INTEGER  SID,NNOD,FLAG,NN
      CHARACTER MESS(*)
      CHARACTER*nchartitle,
     .   TITR,TITR1
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_),TARGET  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ISU,IDNOD,INUM,JREC,NTAG,CTAG,CUR_SUBMOD,ISELECT,IOK,
     .        ID,IDU,NBNODES,NENTITY,KK,JJ,NBCNODES
      INTEGER  TAGS(0:NSUBMOD),J10(10)
      CHARACTER KEY*ncharkey,CART*ncharline,VERS_IN*ncharfield
      INTEGER,DIMENSION(:),ALLOCATABLE :: LIST_ENTITY
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGNODSUB
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  USR2SYS
      INTERFACE
       SUBROUTINE GROUPS_GET_ELEM_LIST(arg1,arg2,arg3)
        USE SUBMODEL_MOD
        INTEGER,DIMENSION(:),ALLOCATABLE       :: arg1
        INTEGER,INTENT(INOUT)                  :: arg2
        TYPE(SUBMODEL_DATA)                    :: arg3(NSUBMOD)
       END SUBROUTINE  
      END INTERFACE 
C=======================================================================
C     Tag submodels
C-------------------------
      TAGS   = 0
      IS_AVAILABLE = .FALSE.
      CALL GROUPS_GET_ELEM_LIST(LIST_ENTITY, NENTITY, LSUBMODEL)
      DO KK=1,NENTITY                                                                         
        JJ = LIST_ENTITY(KK)            
        IF(JJ /= 0)THEN
          ISU=0  
          ISELECT = 0 
          IOK = 0 
          DO K=1,NSUBMOD 
            CUR_SUBMOD = K
            ISELECT = 0
            DO WHILE (CUR_SUBMOD /= 0 .AND. ISELECT == 0)
              IF (LSUBMODEL(CUR_SUBMOD)%NOSUBMOD == JJ) ISELECT = 1
              IF (LSUBMODEL(CUR_SUBMOD)%NOSUBMOD == -JJ) ISELECT = -1
              CUR_SUBMOD = LSUBMODEL(CUR_SUBMOD)%IFATHER
            ENDDO
            
            IF (ISELECT == 1) THEN
              ISU=K        
              TAGS(K) = TAGS(K) + 1
              IOK = 1
            ELSEIF (ISELECT == -1) THEN
              ISU=K
              TAGS(K) = TAGS(K) - 1
              IOK = 1
            ENDIF
          ENDDO                                              
          IF (IOK == 0) CALL ANCMSG(MSGID=194,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO,
     .                  C1=TITR1,
     .                  I1=SID,
     .                  C2=TITR1,
     .                  C3=TITR,
     .                  C4='SUBMODEL',
     .                  I2=J10(J))                                              
        ENDIF                                                
      ENDDO 
      IF(ALLOCATED(LIST_ENTITY))DEALLOCATE (LIST_ENTITY)
C--- 
C-------------------------------------
C Tag submodel nodes
C-------------------------------------
      CALL CPP_NODES_COUNT(NBNODES,NBCNODES) 
      ALLOCATE(TAGNODSUB(NBNODES+NBCNODES))
      TAGNODSUB = 0
      CALL CPP_NODE_SUB_TAG(TAGNODSUB)                             
      IF (FLAG == 0) THEN 
        DO I=1,NSUBMOD
          IF(TAGS(I) >= 1)THEN
            DO J=1,NBNODES+NBCNODES
              IF(TAGNODSUB(J) == I) NNOD=NNOD+1
            ENDDO 
          ENDIF
        ENDDO   
      ELSE
        DO I=1,NSUBMOD
          IF(TAGS(I) >= 1)THEN   
            DO J=1,NBNODES +NBCNODES
              IF(TAGNODSUB(J) == I) THEN                           
                NN = NN + 1
                IGRNOD%ENTITY(NN)=J
              ENDIF
            ENDDO 
          ENDIF
        ENDDO 
      ENDIF 
      IF (ALLOCATED(TAGNODSUB)) DEALLOCATE(TAGNODSUB)
  
C-------------------------
      RETURN
      END
Chd|====================================================================
Chd|  HM_SUBMODGRE                  source/groups/hm_submodgr.F   
Chd|-- called by -----------
Chd|        HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_SUBMODGRE(
     .           ISUBMOD  ,IX      ,NIX      ,SID      ,
     .           NEL      ,NUMEL   ,IELT     ,MESS     ,
     .           FLAG     ,TITR    ,TITR1    ,LSUBMODEL,IGRELEM   ,
     .           NN       ,IGS     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NIX,SID,NEL,NUMEL,IELT,FLAG,NN,IGS
      INTEGER  ISUBMOD(*),IX(NIX,*)
      CHARACTER MESS(*)
      CHARACTER*nchartitle,
     .   TITR,TITR1
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
      TYPE (GROUP_) IGRELEM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ISU,IDEL,INUM,TAG,CUR_SUBMOD,ISELECT,IOK, ID,IDU,ELEMTYPE,KK,JJ
      INTEGER  TAGS(0:NSUBMOD)
      CHARACTER KEY*ncharkey,CART*ncharline,VERS_IN*ncharfield
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGELEMSUB
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
      INTEGER,DIMENSION(:),ALLOCATABLE :: LIST_ELEM
      INTEGER :: NENTITY
C-----------------------------------------------
      INTERFACE
       SUBROUTINE GROUPS_GET_ELEM_LIST(arg1,arg2,arg3)
        USE SUBMODEL_MOD
        INTEGER,DIMENSION(:),ALLOCATABLE       :: arg1
        INTEGER,INTENT(INOUT)                  :: arg2
        TYPE(SUBMODEL_DATA)                    :: arg3(NSUBMOD)
       END SUBROUTINE  
      END INTERFACE     
C-----------------------------------------------
C     IELT = ELEMENT TYPE (0 POUR NOEUDS, 1 BRIC, 2 QUAD, 3 SHELL, 
C                          4 TRUSS, 5 BEAM, 6 SPRINGS,7 SHELL_3N)
C=======================================================================
C     Tag submodels
C-------------------------
      TAGS   = 0

      CALL GROUPS_GET_ELEM_LIST(LIST_ELEM, NENTITY, LSUBMODEL)                            
      DO KK=1,NENTITY                                                                         
          JJ=LIST_ELEM(KK)           
          IF(JJ /= 0)THEN                                                                     
            ISU=0                                                                             
            ISELECT = 0                                                                       
            IOK = 0                                                                           
            DO K=1,NSUBMOD                                                                    
              CUR_SUBMOD = K                                                                  
              ISELECT = 0                                                                     
              DO WHILE (CUR_SUBMOD /= 0 .AND. ISELECT == 0)                               
                IF (LSUBMODEL(CUR_SUBMOD)%NOSUBMOD == JJ) ISELECT = 1                         
                IF (LSUBMODEL(CUR_SUBMOD)%NOSUBMOD == -JJ) ISELECT = -1                       
                CUR_SUBMOD = LSUBMODEL(CUR_SUBMOD)%IFATHER                                    
              ENDDO                                                                           
              IF (ISELECT == 1) THEN                                                          
                ISU=K                                                                         
                TAGS(K) = TAGS(K) + 1                                                         
                IOK = 1                                                                       
              ELSEIF (ISELECT == -1) THEN                                                     
                ISU=K                                                                         
                TAGS(K) = TAGS(K) - 1                                                         
                IOK = 1                                                                       
              ENDIF                                                                           
            ENDDO                                                                             
            IF (IOK == 0) CALL ANCMSG(MSGID=194,                                              
     .                  MSGTYPE=MSGWARNING,                                                   
     .                  ANMODE=ANINFO,                                                        
     .                  C1=TITR1,                                                             
     .                  I1=SID,                                                               
     .                  C2=TITR1,                                                             
     .                  C3=TITR,                                                              
     .                  C4='SUBMODEL',                                                        
     .                  I2=JJ)                                                                
          ENDIF                                                                               
      ENDDO! NEXT KK  
      IF(ALLOCATED(LIST_ELEM))DEALLOCATE (LIST_ELEM)                                                                        
                  
      
C--- -            
      DO K=1,NSUBMOD 
        IF (TAGS(K) > 1) TAGS(K) = 1
        IF (TAGS(K) < -1) TAGS(K) = -1
      ENDDO
c
      ISU = 0
      TAG = 0
      CUR_SUBMOD = 0
      REWIND IUSBM

C-------------------------------------
C Tag submodel ELEMS
C-------------------------------------
C     IELT = ELEMENT TYPE (0 POUR NOEUDS, 1 BRIC, 2 QUAD, 3 SHELL, 
C                          4 TRUSS, 5 BEAM, 6 SPRINGS,7 SHELL_3N)
      IF (IELT == 1) THEN
        ELEMTYPE = 208
      ELSEIF (IELT == 2) THEN
        ELEMTYPE = 104
      ELSEIF (IELT == 3) THEN
        ELEMTYPE = 104
      ELSEIF (IELT == 4) THEN
        ELEMTYPE = 61
      ELSEIF (IELT == 5) THEN
        ELEMTYPE = 60
      ELSEIF (IELT == 6) THEN
        ELEMTYPE = 21
      ELSEIF (IELT == 7) THEN
        ELEMTYPE = 103
      ENDIF

      ALLOCATE(TAGELEMSUB(NUMEL))
      TAGELEMSUB = 0
      CALL CPP_ELEM_SUB_TAG(ELEMTYPE,TAGELEMSUB)
      IF (FLAG == 0) THEN 
        DO I=1,NSUBMOD
          IF(TAGS(I) >= 1)THEN
            DO J=1,NUMEL
              IF(TAGELEMSUB(J) == I) NEL=NEL+1
            ENDDO                                  
          ENDIF
        ENDDO
      ELSEIF (FLAG == 1) THEN 
        DO I=1,NSUBMOD
          IF(TAGS(I) >= 1)THEN  
            DO J=1,NUMEL 
              IF(TAGELEMSUB(J) == I) THEN                      
                NN = NN + 1
                IGRELEM(IGS)%ENTITY(NN) = J
              ENDIF 
            ENDDO                                   
          ENDIF
        ENDDO
      ENDIF 
      IF (ALLOCATED(TAGELEMSUB)) DEALLOCATE(TAGELEMSUB)
C-------------------------
      RETURN
      END
