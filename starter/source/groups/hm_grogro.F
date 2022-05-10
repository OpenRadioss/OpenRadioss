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
Chd|  HM_GROGRO                     source/groups/hm_grogro.F     
Chd|-- called by -----------
Chd|        LECGGROUP                     source/groups/lecggroup.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_GROGRO(NUMEN   ,NGRELN  ,IGRELEM ,ICOUNT,
     .                  FLAG    ,ITER    ,ELKEY  ,LSUBMODEL )
!---
!   group des group des elements + parts
!---
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "my_allocate.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMEN,NGRELN,FLAG,ICOUNT,ITER
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRELN) :: IGRELEM
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,ID,NEL,IGS,IGRS,JREC,ISU,NONTRI,JJ,KK,
     .        FLAG_FMT,GROUP_ID,IFIX_TMP,SKIPFLAG,UID,NN
      INTEGER BUFTMP(NUMEN*2)
      CHARACTER TITR*nchartitle,KEY*ncharkey,ELKEY*4,KEY2*ncharkey
      LOGICAL IS_ENCRYPTED, IS_AVAILABLE
      INTEGER,DIMENSION(:),ALLOCATABLE :: LIST_ELEM
      INTEGER :: NENTITY
C-----------------------------------------------
      INTERFACE
       SUBROUTINE GROUPS_GET_ELEM_LIST(arg1,arg2,arg3)
        USE SUBMODEL_MOD
#include      "submod_c.inc"
        INTEGER,DIMENSION(:),ALLOCATABLE       :: arg1
        INTEGER,INTENT(INOUT)                  :: arg2
        TYPE(SUBMODEL_DATA)                    :: arg3(NSUBMOD)
       END SUBROUTINE  
      END INTERFACE     
C=======================================================================
      IF (FLAG == 0) ICOUNT=0
      IGS = 0
C     boucle sur les groupes
      DO I=1,NGRELN
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = GROUP_ID,
     .                          OPTION_TITR = TITR  ,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)
            IGS=IGS+1
            IF (IGRELEM(IGS)%GRPGRP == 2) THEN
            BUFTMP = 0 
            NEL=0                                                                    
            NN = 0
            NONTRI = IGRELEM(IGS)%SORTED
C-----------                                             
            IF (FLAG == 0 .AND. IGRELEM(IGS)%NENTITY == -1) THEN
            
              SKIPFLAG = 0 
              IF (SKIPFLAG == 0) THEN  
              
               CALL GROUPS_GET_ELEM_LIST(LIST_ELEM, NENTITY, LSUBMODEL)                     
               DO KK=1,NENTITY                                                                  
                   JJ=LIST_ELEM(KK)    
                   IF(JJ /= 0)THEN                                                              
                     IGRS=0                                                                                                     
                     DO K=1,NGRELN                                                                                              
                       IF (IABS(JJ) == IGRELEM(K)%ID) THEN
                         IGRS=K
                         EXIT
                       ENDIF
                     ENDDO
                     IF (IGRS == 0) THEN
                       CALL ANCMSG(MSGID=174,
     .                             MSGTYPE=MSGWARNING,
     .                             ANMODE=ANINFO,
     .                             I1=IGRELEM(IGS)%ID,C1=TITR,
     .                             I2=IABS(JJ))
                     ELSEIF (IGRELEM(IGRS)%LEVEL == 0) THEN
                       !reference a un groupe non initialise
                       IF (ITER > NGRELN) GOTO 900
                       IGRELEM(IGS)%NENTITY=-1
                       IGRELEM(IGS)%LEVEL=0
                       ICOUNT=1
                       SKIPFLAG = 1
                       NEL = 0
                       CYCLE
                     ELSE
                       !reference a un groupe initialise
                       IF (NONTRI == 0) THEN
                         !sorted group, tag les noeuds
                         DO L=1,IGRELEM(IGRS)%NENTITY
                           IF (JJ < 0) THEN
                             !retire les noeuds du group si JJ < 0
                              BUFTMP(IGRELEM(IGRS)%ENTITY(L))=-1
                           ELSEIF (BUFTMP(IGRELEM(IGRS)%ENTITY(L)) == 0) THEN
                              !ajoute les noeuds au group si JJ > 0 et noeud non rire
                             BUFTMP(IGRELEM(IGRS)%ENTITY(L))=1
                           ENDIF
                         ENDDO
                       ELSE
                          !non sorted group
                         NEL = NEL + IGRELEM(IGRS)%NENTITY
                       ENDIF
                     ENDIF
                   ENDIF                                                                        
               ENDDO! NEXT KK      
               IF(ALLOCATED(LIST_ELEM))DEALLOCATE (LIST_ELEM)                                                             
              ENDIF      
              
C-----
              IF (SKIPFLAG == 0) THEN               
                IF (NONTRI == 0 ) THEN              
C                  sorted group                     
                  DO J=1,NUMEN                      
                    IF (BUFTMP(J) > 0) NEL=NEL+1    
                  ENDDO                             
                ENDIF                               
C
                IGRELEM(IGS)%NENTITY = NEL
                MY_ALLOCATE(IGRELEM(IGS)%ENTITY,NEL)
                IGRELEM(IGS)%ENTITY = 0
              ENDIF                                 
C-----------                                             
            ELSEIF (FLAG == 1 .AND. IGRELEM(IGS)%LEVEL == 0 .AND. 
     .                              IGRELEM(IGS)%NENTITY > -1) THEN



              CALL GROUPS_GET_ELEM_LIST(LIST_ELEM, NENTITY, LSUBMODEL)
              DO KK=1,NENTITY
                  JJ=LIST_ELEM(KK)
                  IF(JJ /= 0)THEN
                  IGRS=0                                                          
                    DO K=1,NGRELN                                                   
                      IF (IABS(JJ) == IGRELEM(k)%ID) THEN
                        IGRS=K                                                      
                        EXIT                                                        
                      ENDIF                                                         
                    ENDDO                                                           
                    IF (IGRS == 0) THEN
                    ELSEIF (IGRELEM(IGRS)%NENTITY == -1) THEN
                      CYCLE                                            
                    ELSE                                                 
                      IF (NONTRI == 0) THEN                                                       
C                       tag les noeuds                                                            
                        DO L=1,IGRELEM(IGRS)%NENTITY
                          IF (JJ < 0) THEN                                                    
C                           retire les noeuds du group si JJ < 0                                 
                            BUFTMP(IGRELEM(IGRS)%ENTITY(L))=-1
                          ELSEIF (BUFTMP(IGRELEM(IGRS)%ENTITY(L)) == 0) THEN
C                           ajoute les noeuds au group si JJ > 0 et noeud non retire            
                            BUFTMP(IGRELEM(IGRS)%ENTITY(L))=1
                          ENDIF                                                                   
                        ENDDO                                                                     
                      ELSE                                                                        
                        DO L=1,IGRELEM(IGRS)%NENTITY
                           NN = NN + 1
                           IGRELEM(IGS)%ENTITY(NN) = IGRELEM(IGRS)%ENTITY(L)
                        ENDDO                                                                     
                      ENDIF                                                                       
                    ENDIF 
                  ENDIF
              ENDDO! NEXT KK
              IF(ALLOCATED(LIST_ELEM))DEALLOCATE (LIST_ELEM)
                  
                                                                          

C-----                                                                          
              IF (NONTRI == 0) THEN                    
c               sorted group                                                                
                DO J=1,NUMEN                                                       
                 IF (BUFTMP(J) > 0) THEN                                            
                   NN = NN + 1
                   IGRELEM(IGS)%ENTITY(NN)=J
                 ENDIF                                                             
                ENDDO                                                              
              ENDIF                                                             
               IGRELEM(IGS)%LEVEL = 1
C-----------  end_ flag = 1                                           
            ENDIF 
          ENDIF                                                                   
                                                                  
      ENDDO                                                                        

                                  
C-----------
      RETURN
 900  CALL ANCMSG(MSGID=176,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            C1=TITR,
     .            I2=IGRELEM(IGS)%ID,
     .            C2=ELKEY)
C-----------
      RETURN
      END
