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
Chd|  HM_SUBMODPART                 source/groups/hm_submodpart.F 
Chd|-- called by -----------
Chd|        HM_READ_GRPART                source/groups/hm_read_grpart.F
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_SUBMODPART(
     .           ISUBMOD  ,TAGBUF  ,IPART   ,ID      ,FLAG     ,
     .           MESS     ,TITR    ,TITR1   ,INDX    ,NINDX    ,
     .           LSUBMODEL)
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
#include      "com04_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER   TAGBUF(*),ISUBMOD(*),IPART(LIPART1,*), INDX(*)
      INTEGER   ID,FLAG,NINDX
      CHARACTER MESS(*)
      CHARACTER*nchartitle,
     .   TITR,TITR1
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ISU,JREC,PARTID,CUR_SUBMOD,ISELECT,IOK,
     .        ID_SUB,IDU,ID1,UID1,SUB_INDEX,KK,JJ
      INTEGER  TAGS(NSUBMOD)
      CHARACTER KEY*ncharkey,CART*ncharline,VERS_IN*ncharfield
      CHARACTER TITR2*ncharline
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
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
C     Tag submodels
C-------------------------
      TAGS(1:NSUBMOD)   = 0
      
      CALL GROUPS_GET_ELEM_LIST(LIST_ELEM, NENTITY, LSUBMODEL)                                                               
      DO KK=1,NENTITY                                                                                                            
        JJ=LIST_ELEM(KK)                                              
        IF(JJ > 0)THEN                                                                                                          
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
     .                   MSGTYPE=MSGWARNING,                                                                                     
     .                   ANMODE=ANINFO,                                                                                          
     .                   I1=ID,                                                                                                  
     .                   C1=TITR1,                                                                                               
     .                   C2=TITR,                                                                                                
     .                   C3='SUBMODEL',                                                                                          
     .                   I2=JJ)                                                                                                  
        ENDIF                                                                                                                    
      ENDDO      
      IF(ALLOCATED(LIST_ELEM))DEALLOCATE (LIST_ELEM) 

C-------------------------
C     Tag Parts
C-------------------------   
      DO K=1,NSUBMOD 
        IF (TAGS(K) > 1) TAGS(K) = 1
        IF (TAGS(K) < -1) TAGS(K) = -1
      ENDDO 

C--------------------------------------------------
C BROWSING MODEL PARTS 1->NPART
C--------------------------------------------------
      DO I=1,NPART
        SUB_INDEX = IPART(9,I)
        CUR_SUBMOD = SUB_INDEX
        IF(TAGBUF(I)==0 .AND. CUR_SUBMOD > 0)THEN
          IF(TAGS(CUR_SUBMOD) /=0 )THEN
            TAGBUF(I)=TAGS(CUR_SUBMOD)
            NINDX=NINDX+1
            INDX(NINDX)=I
          END IF
        END IF                
      ENDDO
C-------------------------
      RETURN
      END
