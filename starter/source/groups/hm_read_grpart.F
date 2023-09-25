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
Chd|  HM_READ_GRPART                source/groups/hm_read_grpart.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SUBMODPART                 source/groups/hm_submodpart.F 
Chd|        HM_TAGPART                    source/groups/hm_tagpart.F    
Chd|        UDOUBLE_IGR                   source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_GRPART(
     1           IGRPART   ,IPART   ,ISUBMOD ,FLAG    ,NGRPRT  ,
     2           LSUBMODEL ,SUBSET  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
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
      INTEGER FLAG,NGRPRT
      INTEGER IPART(LIPART1,*),ISUBMOD(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS)  :: SUBSET
      TYPE (GROUP_)  , DIMENSION(NGRPRT) :: IGRPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,ID,NEL,IGS,JREC,
     .        IT0,IT1,IT2,IT3,IT4,IT5,
     .        FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,IBID,N1,N2,OK,NINDX,
     .        NN,IDMIN,IDMAX,OFFSET,NENTITY
      INTEGER J10(10),BUFTMP(NSUBS+NPART),INDX(NSUBS+NPART),
     .        LIST_IGR(NGRPRT),UID,KK
      my_real
     . BID
      CHARACTER TITR*nchartitle,KEY*ncharkey,KEY2*ncharkey,MES*40
      CHARACTER*nchartitle,TITR1
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
      DATA MES/' PART GROUP'/
C-----------------------------------------------
C    IGRPART(IGS)%ID : GROUP identifier
C    IGRPART(IGS)%TITLE : GROUP title
C    IGRPART(IGS)%NENTITY : Entities (parts) number of the GROUP
C    IGRPART(IGS)%GRTYPE : TYPE ( 0-NOEUDS, 1-BRIC, 2-QUAD,   3-SHELL_4N,
!                                4-TRUSS,  5-BEAM, 6-SPRINGS,7-SHELL_3N,
C                               -1 PARTS)
!!                   GRTYPE --- > OBSOLETE
C    IGRPART(IGS)%SORTED  : FLAG for sorted/unsorted parts
!                    = 0 -> sorted
!                    = 1 -> unsorted
C    IGRPART(IGS)%GRPGRP : TYPE of part GROUP
!                                 = 1 ELEM
!                                 = 2 GRELEM
!                                 = 3 SUBSET,PART,MAT,PROP
!                                 = 4 BOX,GENE
!                                 = 5 SUBMOD
C    IGRPART(IGS)%LEVEL  : Hierarchy level
!                             (FLAG 'SUBLEVEL DONE' FOR GROUPS OF GROUPS)
!                            = 0 ---> not yet initialized
!                            = 1 ---> done
C-----------------------------------------------
      IF(NGRPART<=0)RETURN
C-----------------------------------------------
      IBID=0
      IT0=0
      IT1=0
      IT2=0
      IT3=0
      IT4=0
      IT5=0
      TITR1='PART GROUP'
C-------------------------
C-------------------------
C groupes d' elements + init IGRN(1,IGS)
C-------------------------

      CALL HM_OPTION_START('/GRPART')

      IGS=0
      
      
      DO IGS=1,NGRPART
      
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)
      
 !
! initialized variables:
            IF (FLAG == 0) THEN
              IGRPART(IGS)%ID = 0
              IGRPART(IGS)%NENTITY = 0
              IGRPART(IGS)%GRTYPE = 0
              IGRPART(IGS)%SORTED = 0
              IGRPART(IGS)%GRPGRP = 0
              IGRPART(IGS)%LEVEL = 0
              IGRPART(IGS)%R2R_ALL = 0
              IGRPART(IGS)%R2R_SHARE = 0
            ENDIF
!
            IGRPART(IGS)%ID=ID
            IGRPART(IGS)%GRTYPE=-1
            IGRPART(IGS)%TITLE=TITR
C
            IF(KEY(1:6) == 'GRPART')THEN
C             groupes de groupes
              IGRPART(IGS)%NENTITY=-1
              IGRPART(IGS)%GRPGRP=2
              IT0=IT0+1
            ELSEIF(KEY(1:4) == 'PART' .OR. KEY(1:6) == 'SUBSET' .OR. KEY(1:3) == 'MAT' .OR. KEY(1:4) == 'PROP') THEN
C             groupe de SUBSET PART MAT OU PROP
              IT2=IT2+1
              IF (FLAG == 0) THEN
                IGRPART(IGS)%NENTITY=0
                IGRPART(IGS)%GRPGRP=3
              ENDIF
            ELSEIF(KEY(1:4) == 'GENE' )THEN
C            groupe d'elements dans un box
             IT3=IT3+1
             IF (FLAG == 0) THEN
               IGRPART(IGS)%NENTITY=0
               IGRPART(IGS)%GRPGRP=4
             ENDIF
            ELSEIF(KEY(1:6) == 'SUBMOD')THEN
C             groupe de parts dans un submodel
              IT4=IT4+1
              IF (FLAG  == 0) THEN
                IGRPART(IGS)%NENTITY=0
                IGRPART(IGS)%GRPGRP=5
              ENDIF
            ELSEIF(KEY(1:8) == 'GEN_INCR' )THEN
C            groupe d'elements gen_inc first last offset
              IT5=IT5+1
              IF (FLAG == 0) THEN
                IGRPART(IGS)%NENTITY=0
                IGRPART(IGS)%GRPGRP=4
              ENDIF
            ENDIF
C
      ENDDO

C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      IF (FLAG == 0) THEN
        DO IGS=1,NGRPRT
          LIST_IGR(IGS) = IGRPART(IGS)%ID
        ENDDO
        CALL UDOUBLE_IGR(LIST_IGR,NGRPRT,MES,0,BID)
      ENDIF
C-------------------------
C groupes de SUBSETS,PART,MAT,PROP
C-------------------------                                                      
      IF (IT2 > 0) THEN
        CALL HM_OPTION_START('/GRPART')
        DO IGS=1,NGRPART
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR  ,
     .                            UNIT_ID     = UID,
     .                            KEYWORD2    = KEY   ,
     .                            KEYWORD3    = KEY2)      
          IF (KEY(1:6)=='SUBSET'.OR.KEY(1:4)=='PART'.OR. KEY(1:3)=='MAT'   .OR.KEY(1:4)=='PROP') THEN            
            NN = 0                                                                                                 
            NEL = 0                 
            BUFTMP = 0                                                                                           
            CALL HM_TAGPART(BUFTMP     ,IPART  ,KEY   ,IGRPART(IGS)%ID,TITR   ,TITR1 ,FLAG   ,SUBSET ,LSUBMODEL)               
            IF (FLAG == 0) THEN                                                                                  
              DO J=1,NPART                                                                                       
                IF (BUFTMP(J) == 1) NEL=NEL+1                                                                    
              ENDDO                                                                                              
              IGRPART(IGS)%NENTITY=NEL                                                                           
              CALL MY_ALLOC(IGRPART(IGS)%ENTITY,NEL)                                                               
              IGRPART(IGS)%ENTITY = 0                                                                            
            ELSEIF (FLAG == 1) THEN                                                                              
              DO J=1,NPART                                                                                       
                IF (BUFTMP(J) == 1) THEN                                                                         
                  NN = NN + 1                                                                                    
                  IGRPART(IGS)%ENTITY(NN)=J                                                                      
                ENDIF                                                                                            
              ENDDO                                                                                              
              IGRPART(IGS)%LEVEL=1                                                                               
            ENDIF                                                                                                
          ENDIF    
        ENDDO                                                                                              
      ENDIF                                                           


C-------------------------
C GENERATION
C-------------------------
      IF (IT3 /= 0) THEN
        CALL HM_OPTION_START('/GRPART')      
        DO IGS=1,NGRPART
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR  ,
     .                            UNIT_ID     = UID,
     .                            KEYWORD2    = KEY   ,
     .                            KEYWORD3    = KEY2)      
          IF(KEY(1:4) == 'GENE')THEN                                                               
            NN = 0                                                                                   
            NEL=0                                                                                    
            BUFTMP = 0                                                                             
            CALL HM_GET_INTV  ('grnodGenArrCnt' ,NENTITY,IS_AVAILABLE,LSUBMODEL)                   
            DO KK=1,NENTITY                                                                        
              CALL HM_GET_INT_ARRAY_INDEX ('Ifirst'    ,N1    ,KK,IS_AVAILABLE,LSUBMODEL)          
              CALL HM_GET_INT_ARRAY_INDEX ('Ilast'     ,N2    ,KK,IS_AVAILABLE,LSUBMODEL)          
              DO K=1,NPART                                                                         
                IF (IPART(4,K) >= N1.AND.IPART(4,K) <= N2)BUFTMP(K)=1                              
              ENDDO                                                                                
            ENDDO                                                                                  
            NEL=0                                                                                  
            IF (FLAG == 0) THEN                                                                    
              DO J=1,NPART                                                                         
                IF (BUFTMP(J) == 1) NEL=NEL+1                                                      
              ENDDO                                                                                
              IGRPART(IGS)%NENTITY=NEL                                                             
              CALL MY_ALLOC(IGRPART(IGS)%ENTITY,NEL)                                                 
              IGRPART(IGS)%ENTITY = 0                                                              
            ELSEIF (FLAG == 1) THEN                                                                
              DO J=1,NPART                                                                         
                IF (BUFTMP(J) == 1) THEN                                                           
                  NN = NN + 1                                                                      
                  IGRPART(IGS)%ENTITY(NN)=J                                                        
                ENDIF                                                                              
              ENDDO                                                                                
            ENDIF                                                                                  
          ENDIF                                                                                    
        ENDDO
      ENDIF
      
C-------------------------
C groupes de SUBMODELS
C-------------------------
      IF (IT4 > 0) THEN   
        CALL HM_OPTION_START('/GRPART')                                               
        DO IGS=1,NGRPART
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR  ,
     .                            UNIT_ID     = UID,
     .                            KEYWORD2    = KEY   ,
     .                            KEYWORD3    = KEY2)      
          IF (KEY(1:6) == 'SUBMOD') THEN                                                                          
            NN=0                                                                                                  
            NEL=0                                                                                                 
            BUFTMP = 0                                                                                            
            NINDX = 0                                                                                             
            CALL HM_SUBMODPART(ISUBMOD,BUFTMP ,IPART  ,ID     ,FLAG     ,                                            
     .                      MES    ,TITR,TITR1,INDX,NINDX ,                                                       
     .                      LSUBMODEL)                                                                            
            IF (FLAG == 0) THEN                                                                                   
              DO J=1,NPART                                                                                        
                IF (BUFTMP(J) == 1) NEL=NEL+1                                                                     
              ENDDO                                                                                               
              IGRPART(IGS)%NENTITY=NEL                                                                            
              CALL MY_ALLOC(IGRPART(IGS)%ENTITY,NEL)                                                                
              IGRPART(IGS)%ENTITY = 0                                                                             
            ELSEIF (FLAG == 1) THEN                                                                               
              DO J=1,NPART                                                                                        
                IF (BUFTMP(J) == 1) THEN                                                                          
                  NN = NN + 1                                                                                     
                  IGRPART(IGS)%ENTITY(NN)=J                                                                       
                ENDIF                                                                                             
              ENDDO                                                                                               
              IGRPART(IGS)%LEVEL=1                                                                                
            ENDIF                                                                                                 
          ENDIF                                                                                                   
        ENDDO                                                      
      ENDIF        

C-------------------------
C GENERATION MIN MAX OFFSET
C-------------------------
      IF (IT5 /= 0) THEN
        CALL HM_OPTION_START('/GRPART')      
        DO IGS=1,NGRPART
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR  ,
     .                            UNIT_ID     = UID,
     .                            KEYWORD2    = KEY   ,
     .                            KEYWORD3    = KEY2)      


          IF(KEY(1:8) == 'GEN_INCR')THEN                                                                
            NN = 0                                                                                      
            NEL=0                                                                                       
            BUFTMP = 0                                                                                  
            CALL HM_GET_INTV  ('grnodGenArrCnt' ,NENTITY,IS_AVAILABLE,LSUBMODEL)                        
            DO KK=1,NENTITY                                                                             
              CALL HM_GET_INT_ARRAY_INDEX ('Ifirst'    ,IDMIN    ,KK,IS_AVAILABLE,LSUBMODEL)            
              CALL HM_GET_INT_ARRAY_INDEX ('Ilast'     ,IDMAX    ,KK,IS_AVAILABLE,LSUBMODEL)            
              CALL HM_GET_INT_ARRAY_INDEX ('Iincr'     ,OFFSET   ,KK,IS_AVAILABLE,LSUBMODEL)               
              !optimized loop
              DO K=1,NPART
                ID=IPART(4,K)
                IF(ID<IDMIN .OR. ID>IDMAX)CYCLE
                IF(MOD(ID-IDMIN,OFFSET)==0)BUFTMP(K)=1
              ENDDO
              !previous loop                  
              !DO J=MAX(1,IDMIN),IDMAX,OFFSET                                                                   
              !  DO K=1,NPART                                                                          
              !    IF (IPART(4,K) == J) THEN
              !      BUFTMP(K)=1    !tag des parts 
              !      EXIT
              !    ENDIF                 
              !  ENDDO                                                                                          
              !ENDDO                                                                                                   
            ENDDO                                                                                       
            NEL=0                                                                                       
            IF (FLAG == 0) THEN                                                                         
              DO J=1,NPART                                                                              
                IF (BUFTMP(J) == 1) NEL=NEL+1                                                           
              ENDDO                                                                                     
              IGRPART(IGS)%NENTITY=NEL                                                                  
              CALL MY_ALLOC(IGRPART(IGS)%ENTITY,NEL)                                                      
              IGRPART(IGS)%ENTITY = 0                                                                   
            ELSEIF (FLAG == 1) THEN                                                                     
              DO J=1,NPART                                                                              
                IF (BUFTMP(J) == 1) THEN                                                                
                  NN = NN + 1                                                                           
                  IGRPART(IGS)%ENTITY(NN)=J                                                             
                ENDIF                                                                                   
              ENDDO                                                                                     
            ENDIF                                                                                       
          ENDIF                                                                                         
        ENDDO
      ENDIF   
C------------------------------ 
      RETURN
      END

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
