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
Chd|  HM_GROGRONOD                  source/groups/hm_grogronod.F  
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_GROGRONOD(IGRNOD  ,ICOUNT  ,FLAG    ,ITER  ,ELKEY,   
     .                        LSUBMODEL)
!---
!   group des group des elements + parts
!---
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
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
#include      "scr17_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  FLAG,ICOUNT,ITER
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,ID,NEL,IGS,IGRS,JREC,ISU,NONTRI,JJ,KK,
     .        FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,SKIPFLAG,UID,NN,NENTITY
      INTEGER J10(10),BUFTMP(NUMNOD*2)
      INTEGER,DIMENSION(:),ALLOCATABLE :: LIST_ENTITY
      CHARACTER TITR*nchartitle,KEY*ncharkey,ELKEY*4,KEY2*ncharkey
      LOGICAL IS_AVAILABLE   
      INTERFACE
       SUBROUTINE GROUPS_GET_ELEM_LIST(arg1,arg2,arg3)
        USE SUBMODEL_MOD
        INTEGER,DIMENSION(:),ALLOCATABLE       :: arg1
        INTEGER,INTENT(INOUT)                  :: arg2
        TYPE(SUBMODEL_DATA)                    :: arg3(NSUBMOD)
       END SUBROUTINE  
      END INTERFACE 
C=======================================================================
      IS_AVAILABLE = .FALSE.
      IF (FLAG == 0) ICOUNT=0
      IGS =0
      CALL HM_OPTION_START('/GRNOD')
C     boucle sur les groupes
      DO I=1,NGRNOD     
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2 )
                                                               
        IGS=IGS+1
        IF (IGRNOD(IGS)%GRPGRP == 2) THEN
          BUFTMP = 0 
          NEL=0                                                                    
          NN = 0
          NONTRI = IGRNOD(IGS)%SORTED
C-----------                                             
          IF (FLAG == 0 .AND. IGRNOD(IGS)%NENTITY == -1) THEN
c          
            SKIPFLAG = 0  
            IF (SKIPFLAG == 0) THEN  
              CALL GROUPS_GET_ELEM_LIST(LIST_ENTITY, NENTITY, LSUBMODEL)
              DO KK = 1,NENTITY
                JJ = LIST_ENTITY(KK)   
                IF (JJ /= 0) THEN 
                  IGRS=0
                  DO K=1,NGRNOD
                    IF (IABS(JJ) == IGRNOD(K)%ID) THEN
                      IGRS=K
                      EXIT
                    ENDIF
                  ENDDO
                  IF (IGRS == 0) THEN
                    CALL ANCMSG(MSGID=174,
     .                          MSGTYPE=MSGWARNING,
     .                          ANMODE=ANINFO,
     .                          I1=IGRNOD(IGS)%ID,C1=TITR,
     .                          I2=IABS(JJ))
                  ELSEIF (IGRNOD(IGRS)%LEVEL == 0) THEN
C                       reference a un groupe non initialise
                    IF (ITER > NGRNOD) GOTO 900
                    IGRNOD(IGS)%NENTITY=-1
                    IGRNOD(IGS)%LEVEL=0
                    ICOUNT=1
                    SKIPFLAG = 1
                    NEL = 0
                    CYCLE
                  ELSE
C                       reference a un groupe initialise
                    IF (NONTRI == 0) THEN
C                         sorted group, tag les noeuds
                      DO L=1,IGRNOD(IGRS)%NENTITY
                        IF (JJ < 0) THEN
C                             retire les noeuds du group si j10 < 0
                          BUFTMP(IGRNOD(IGRS)%ENTITY(L))=-1
                        ELSEIF (BUFTMP(IGRNOD(IGRS)%ENTITY(L)) == 0) THEN
C                              ajoute les noeuds au group si j10 > 0 et noeud non rire
                          BUFTMP(IGRNOD(IGRS)%ENTITY(L))=1
                        ENDIF
                      ENDDO
                    ELSE
c                         non sorted group
                      NEL = NEL + IGRNOD(IGRS)%NENTITY
                    ENDIF
                  ENDIF ! IF (IGRS == 0)
                ENDIF ! IF (J10(J) /= 0)
              ENDDO ! DO J=1,10
              IF(ALLOCATED(LIST_ENTITY))DEALLOCATE (LIST_ENTITY)   
            ENDIF ! IF (SKIPFLAG == 0)
C-----
            IF (SKIPFLAG == 0) THEN               
              IF (NONTRI == 0 ) THEN              
C                  sorted group                     
                DO J=1,NUMNOD                      
                  IF (BUFTMP(J) > 0) NEL=NEL+1    
                ENDDO                             
              ENDIF                               
C
              IGRNOD(IGS)%NENTITY = NEL
              CALL MY_ALLOC(IGRNOD(IGS)%ENTITY,NEL)
              IGRNOD(IGS)%ENTITY = 0
            ENDIF ! IF (SKIPFLAG == 0)
C-----------                                             
          ELSEIF (FLAG == 1 .AND. IGRNOD(IGS)%LEVEL == 0 .AND. 
     .                            IGRNOD(IGS)%NENTITY > -1) THEN
c
            CALL GROUPS_GET_ELEM_LIST(LIST_ENTITY, NENTITY, LSUBMODEL)
            DO KK=1,NENTITY
              JJ = LIST_ENTITY(KK)
              IF (JJ /= 0) THEN
                IGRS=0                                                           
                DO K=1,NGRNOD                                                   
                  IF (IABS(JJ) == IGRNOD(K)%ID) THEN
                    IGRS=K                                                      
                    EXIT                                                        
                  ENDIF                                                         
                ENDDO                                                           
                IF (IGRS == 0) THEN
                ELSEIF (IGRNOD(IGRS)%NENTITY == -1) THEN
                  CYCLE                                            
                ELSE                                                 
                  IF (NONTRI == 0) THEN                                                       
C                       tag les noeuds                                                            
                    DO L=1,IGRNOD(IGRS)%NENTITY
                      IF (JJ < 0) THEN                                                    
C                           retire les noeuds du group si j10 < 0                                 
                        BUFTMP(IGRNOD(IGRS)%ENTITY(L))=-1
                      ELSEIF (BUFTMP(IGRNOD(IGRS)%ENTITY(L)) == 0) THEN
C                           ajoute les noeuds au group si j10 > 0 et noeud non retire            
                        BUFTMP(IGRNOD(IGRS)%ENTITY(L))=1
                      ENDIF                                                                   
                    ENDDO                                                                     
                  ELSE                                                                        
                    DO L=1,IGRNOD(IGRS)%NENTITY
                      NN = NN + 1
                      IGRNOD(IGS)%ENTITY(NN) = IGRNOD(IGRS)%ENTITY(L)
                    ENDDO                                                                     
                  ENDIF ! IF (NONTRI == 0)                                                                     
                ENDIF ! IF (IGRS == 0)
              ENDIF ! IF (J10(J) /= 0)
            ENDDO ! DO J=1,10       
            IF(ALLOCATED(LIST_ENTITY))DEALLOCATE (LIST_ENTITY)   
C-----                                                                          
            IF (NONTRI == 0) THEN                    
c               sorted group                                                                
              DO J=1,NUMNOD                                                       
                IF (BUFTMP(J) > 0) THEN                                            
                  NN = NN + 1
                  IGRNOD(IGS)%ENTITY(NN)=J
                ENDIF                                                             
              ENDDO                                                              
            ENDIF ! IF (NONTRI == 0)   
            IGRNOD(IGS)%LEVEL = 1
C-----------  end_ flag = 1                                           
          ENDIF ! IF (FLAG == 0 .AND. IGRNOD(IGS)%NENTITY == -1)
        ENDIF ! IF (IGRNOD(IGS)%GRPGRP == 2)
      ENDDO ! DO I=1,NLINE(KCUR)                                                    
C-----------
      RETURN
 900  CALL ANCMSG(MSGID=176,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            C1=TITR,
     .            I2=IGRNOD(IGS)%ID,
     .            C2=ELKEY)
C-----------
      RETURN
      END
