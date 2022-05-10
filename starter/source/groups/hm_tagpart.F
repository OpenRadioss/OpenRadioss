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
Chd|  HM_TAGPART                    source/groups/hm_tagpart.F    
Chd|-- called by -----------
Chd|        HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|        HM_READ_GRPART                source/groups/hm_read_grpart.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_TAGPART(BUFFTMP,IPART  ,KEY   ,
     .                   ID     ,TITR   ,TITR1  ,FLAG   ,SUBSET,LSUBMODEL)
C old routine w/o index of BUFFTMP
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD     
      USE MESSAGE_MOD
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
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER BUFFTMP(*),IPART(LIPART1,*),ID,
     .        FLAG
      CHARACTER KEY*(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
      INTEGER J,NSEG,J10(10),IGS,JREC,IADV,ISU,K,L,IPP,NUMA,KK,JJ,NENTITY
      CHARACTER MOT*4
      CHARACTER*nchartitle,TITR,TITR1
      INTEGER,DIMENSION(:),ALLOCATABLE :: LIST_ENTITY
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
      NENTITY = 0
      IF (KEY(1:6) == 'SUBSET') THEN
C-------------------------
C groupes de SUBSETS
C-------------------------
           CALL GROUPS_GET_ELEM_LIST(LIST_ENTITY, NENTITY, LSUBMODEL)                  
           DO KK=1,NENTITY                                                              
               JJ=LIST_ENTITY(KK)       
               IF(JJ /= 0)THEN                                                          
                 ISU=0                                                                  
                 DO K=1,NSUBS                                                           
                   IF (JJ == SUBSET(K)%ID) THEN                                         
                     ISU=K                                                              
                     DO L=1,SUBSET(ISU)%NTPART                                          
                       !tag les parts                                                    
                       BUFFTMP(SUBSET(ISU)%TPART(L)) = 1                                
                     ENDDO                                                              
                     EXIT                                                               
                   ELSEIF (JJ == -SUBSET(K)%ID) THEN                                    
                     ISU=K                                                              
                     DO L=1,SUBSET(ISU)%NTPART                                          
                       !tag les parts                                                    
                       BUFFTMP(SUBSET(ISU)%TPART(L)) = -1                               
                     ENDDO                                                              
                     EXIT                                                               
                   ENDIF                                                                
                 ENDDO                                                                  
                 IF (ISU == 0 .AND. FLAG == 0) THEN                                     
                   CALL ANCMSG(MSGID=194,                                               
     .                         MSGTYPE=MSGWARNING,                                      
     .                         ANMODE=ANINFO,                                           
     .                         I1=ID,                                                   
     .                         C1=TITR1,                                                
     .                         C2=TITR,                                                 
     .                         C3='SUBSET',                                             
     .                         I2=JJ)                                                   
                 ENDIF                                                                  
               ENDIF                                                                    
           ENDDO! NEXT KK                                                               
           IF(ALLOCATED(LIST_ENTITY))DEALLOCATE (LIST_ENTITY)           
                                              
      ELSEIF (KEY(1:4) == 'PART' .OR. KEY(1:3) == 'MAT' .OR. KEY(1:4) == 'PROP') THEN
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
              
                  CALL GROUPS_GET_ELEM_LIST(LIST_ENTITY, NENTITY, LSUBMODEL)                  
                  DO KK=1,NENTITY
                      JJ=LIST_ENTITY(KK)
                      IF(JJ /= 0)THEN
                          ISU=0           
                          DO K=1,NPART
		            NUMA = IPART(IPP,K)
                            IF (NSUBDOM.GT.0) THEN
		               IF (IPP.EQ.5) NUMA = IPART_R2R(2,K)
		            ENDIF
C-------------------------------------------------------------------------------------------------------------		    
                            IF(JJ == NUMA)THEN
                              ISU=K
C                             !tag les parts
                              BUFFTMP(ISU)=1
                            ELSEIF(-JJ == NUMA)THEN
                              ISU=K
C                             !tag les parts 
                              BUFFTMP(ISU)=-1
                            ENDIF
                          ENDDO
                          IF(ISU == 0 .AND. FLAG == 0)THEN
                            IF(MOT(1:4)/='PART')THEN
                              CALL ANCMSG(MSGID=195,
     .                             MSGTYPE=MSGWARNING,
     .                             ANMODE=ANINFO,
     .                             I1=ID,
     .                             C1=TITR1,
     .                             C2=TITR1,
     .                             C3=TITR,
     .                             C4=MOT,
     .                             I2=JJ)
                            ELSE
                               CALL ANCMSG(MSGID=194,                                               
     .                         MSGTYPE=MSGWARNING,                                      
     .                         ANMODE=ANINFO,                                           
     .                         I1=ID,                                                   
     .                         C1=TITR1,                                                
     .                         C2=TITR1,                                                 
     .                         C3=TITR,   
     .                         C4='PART',                                             
     .                         I2=JJ)                               
                            ENDIF
                          ENDIF
                      ENDIF
                  ENDDO! NEXT KK
                  IF(ALLOCATED(LIST_ENTITY))DEALLOCATE (LIST_ENTITY) 
                  
C-------------------------
      ENDIF
C-------------------------
      RETURN
      END
