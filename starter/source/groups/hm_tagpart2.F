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
Chd|  HM_TAGPART2                   source/groups/hm_tagpart2.F   
Chd|-- called by -----------
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        SET_USRTOS                    source/model/sets/ipartm1.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_TAGPART2(BUFFTMP,IPART  ,KEY     ,
     .                    ID     ,TITR   ,TITR1   ,INDX     ,NINDX  ,
     .                    FLAG   ,SUBSET ,LSUBMODEL, MAP)
C optimized routine w/ index of BUFFTMP
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "r2r_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER BUFFTMP(*),IPART(LIPART1,*),
     .        INDX(*), NINDX, ID, FLAG
      CHARACTER KEY*(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      INTEGER, DIMENSION(NPART,2), INTENT(in) :: MAP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,NSEG,JREC,IADV,ISU,K,L,IPP,NUMA,JJ,KK,NENTITY_POS, NENTITY_NEG,NENTITY
      INTEGER,DIMENSION(:),ALLOCATABLE :: TAG_ENTITY_POS, TAG_ENTITY_NEG,LIST_ENTITY
      INTEGER :: ID_LOCAL
      INTEGER, DIMENSION(:), ALLOCATABLE :: ENTITY_POS,ENTITY_NEG
      CHARACTER MOT*4
      CHARACTER*nchartitle,
     .   TITR,TITR1
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
      INTEGER SET_USRTOS
      EXTERNAL SET_USRTOS
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
C-----------------------------------------------
      IF (KEY(1:6) == 'SUBSET') THEN
C-------------------------
C groupes de SUBSETS
C-------------------------

           CALL GROUPS_GET_ELEM_LIST(LIST_ENTITY, NENTITY, LSUBMODEL)                  
           DO KK=1,NENTITY                                                              
            JJ=LIST_ENTITY(KK)       
            IF (JJ /= 0) THEN 
              ISU=0                                      
              DO K=1,NSUBS
                IF (JJ == SUBSET(K)%ID) THEN
                  ISU=K
                  DO L=1,SUBSET(ISU)%NTPART
C                   tag les parts
                    IF(BUFFTMP(SUBSET(ISU)%TPART(L))==0)THEN
                      BUFFTMP(SUBSET(ISU)%TPART(L))=1
                      NINDX=NINDX+1
                      INDX(NINDX)=SUBSET(ISU)%TPART(L)
                    END IF
                  ENDDO                                    
                  EXIT
                ELSEIF (JJ == -SUBSET(K)%ID) THEN
                  ISU=K
                  DO L=1,SUBSET(ISU)%NTPART
C                   tag les parts 
                    IF(BUFFTMP(SUBSET(ISU)%TPART(L))==0)THEN
                      BUFFTMP(SUBSET(ISU)%TPART(L))=-1
                      NINDX=NINDX+1
                      INDX(NINDX)=SUBSET(ISU)%TPART(L)
                    END IF                    
                  ENDDO                                        
                  EXIT
                ENDIF
              ENDDO                                      
              IF (ISU == 0 .AND. FLAG == 0) THEN                           
                CALL ANCMSG(MSGID=194,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1=TITR1,
     .                      C2=TITR,
     .                      C3='SUBSET',
     .                      I2=JJ)
              ENDIF                                            
            ENDIF                                              
           ENDDO 
           IF(ALLOCATED(LIST_ENTITY))DEALLOCATE (LIST_ENTITY)   
                                                                                                       
      ELSEIF (KEY(1:4) == 'PART' .OR. KEY(1:3) == 'MAT' .OR.
     .        KEY(1:4) == 'PROP') THEN
C-------------------------
C groupes de PART,MAT,PROP
C-------------------------
              IF(KEY(1:4) == 'PART')THEN
                MOT='PART'
                IPP=4
              ELSEIF(KEY(1:3) == 'MAT')THEN  
                MOT='MAT'
                IPP=5
              ELSEIF(KEY(1:4) == 'PROP')THEN
                MOT='PROP'
                IPP=6
              ENDIF
              
              CALL HM_GET_INTV  ('idsmax' ,NENTITY_POS,IS_AVAILABLE,LSUBMODEL) 
              CALL HM_GET_INTV  ('negativeIdsmax' ,NENTITY_NEG,IS_AVAILABLE,LSUBMODEL)                 
              ALLOCATE(TAG_ENTITY_POS(NENTITY_POS))  
              ALLOCATE(TAG_ENTITY_NEG(NENTITY_NEG))  
              TAG_ENTITY_POS(1:NENTITY_POS)=0
              TAG_ENTITY_NEG(1:NENTITY_NEG)=0

              ALLOCATE(ENTITY_POS(NENTITY_POS)) 
              ALLOCATE(ENTITY_NEG(NENTITY_NEG))  

              DO KK=1,NENTITY_POS
                CALL HM_GET_INT_ARRAY_INDEX ('ids' ,JJ ,KK,IS_AVAILABLE,LSUBMODEL)  
                ENTITY_POS(KK) = JJ  
              ENDDO

              DO KK=1,NENTITY_NEG                                            
                CALL HM_GET_INT_ARRAY_INDEX ('negativeIds' ,JJ ,KK,IS_AVAILABLE,LSUBMODEL)  
                ENTITY_NEG(KK) = JJ  
              ENDDO


              IF(IPP==4) THEN
                DO KK=1,NENTITY_POS 
                    JJ = ENTITY_POS(KK)                  
                    ID_LOCAL = SET_USRTOS(JJ,MAP,NPART)
                    IF(ID_LOCAL == 0) THEN
                         ! part not found
                         CYCLE
                    ENDIF
                    ISU=MAP(ID_LOCAL,2)
                    TAG_ENTITY_POS(KK)=1  
                    !tag les parts                         
                    IF(BUFFTMP(ISU)==0)THEN                
                        BUFFTMP(ISU)=1                       
                        NINDX=NINDX+1                        
                        INDX(NINDX)=ISU                      
                    END IF                    
                ENDDO

                DO KK=1,NENTITY_NEG 
                    JJ = ENTITY_NEG(KK)                  
                    ID_LOCAL = SET_USRTOS(JJ,MAP,NPART)
                    IF(ID_LOCAL == 0) THEN
                         ! Part not found
                         CYCLE
                    ENDIF

                    ISU=MAP(ID_LOCAL,2)
                    TAG_ENTITY_NEG(KK)=1  
                    !tag les parts                         
                    IF(BUFFTMP(ISU)==0)THEN                
                        BUFFTMP(ISU)=-1          
                        NINDX=NINDX+1            
                        INDX(NINDX)=ISU                     
                    END IF                    
                ENDDO
              ELSE
                DO KK=1,NENTITY_POS 
                    JJ = ENTITY_POS(KK)
                    DO K=1,NPART
                              NUMA = IPART(IPP,K)
                
                        IF (NSUBDOM>0) THEN
                                  IF (IPP==5) NUMA = IPART_R2R(2,K)
                              ENDIF
                        ISU = 0                
                        IF(JJ == NUMA)THEN                       
                            ISU=K 
                            TAG_ENTITY_POS(KK)=1  
                            !tag les parts                         
                            IF(BUFFTMP(ISU)==0)THEN                
                                BUFFTMP(ISU)=1                       
                                NINDX=NINDX+1                        
                                INDX(NINDX)=ISU                      
                            END IF                               
                        ENDIF
                    ENDDO
                ENDDO

                DO KK=1,NENTITY_NEG 
                    JJ = ENTITY_NEG(KK)
                    DO K=1,NPART
                              NUMA = IPART(IPP,K)
                
                        IF (NSUBDOM>0) THEN
                                  IF (IPP==5) NUMA = IPART_R2R(2,K)
                              ENDIF
                        ISU = 0
                        IF(JJ == NUMA)THEN      
                            ISU=K 
                            TAG_ENTITY_NEG(KK)=1  
                            !tag les parts              
                            IF(BUFFTMP(ISU)==0)THEN   
                                BUFFTMP(ISU)=-1          
                                NINDX=NINDX+1            
                                INDX(NINDX)=ISU          
                            END IF                   
                        ENDIF 
                    ENDDO
                ENDDO
              ENDIF
             
              !If positive USER_ID is not relevant
              IF(FLAG == 0)THEN
                DO KK=1,NENTITY_POS
                  IF(TAG_ENTITY_POS(KK)==0)THEN
                    CALL HM_GET_INT_ARRAY_INDEX ('ids' ,JJ ,KK,IS_AVAILABLE,LSUBMODEL)
                    CALL ANCMSG(MSGID=194, MSGTYPE=MSGWARNING,ANMODE=ANINFO,I1=ID,C1=TITR1,C2=TITR,C3=MOT,I2=JJ)
                  ENDIF
                ENDDO               
              ENDIF

              !If negative USER_ID is not relevant
              IF(FLAG == 0)THEN
                DO KK=1,NENTITY_NEG
                  IF(TAG_ENTITY_NEG(KK)==0)THEN
                    CALL HM_GET_INT_ARRAY_INDEX ('negativeIdsmax' ,JJ ,KK,IS_AVAILABLE,LSUBMODEL)
                    CALL ANCMSG(MSGID=194, MSGTYPE=MSGWARNING,ANMODE=ANINFO,I1=ID,C1=TITR1,C2=TITR,C3=MOT,I2=JJ)
                  ENDIF
                ENDDO                
              ENDIF

              DEALLOCATE(TAG_ENTITY_POS)
              DEALLOCATE(TAG_ENTITY_NEG)   
              DEALLOCATE(ENTITY_POS)  
              DEALLOCATE(ENTITY_NEG)    
              
C-------------------------
      ENDIF
C-------------------------
      RETURN
      END
