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
Chd|  HM_LINES_OF_LINES             source/groups/hm_lines_of_lines.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_LINES_OF_LINES(IGRSLIN ,INSEG  ,FLAG  ,ICOUNT ,ITER ,NSETS, LSUBMODEL)
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
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INSEG,FLAG,ICOUNT,ITER
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,K,L,J,ID,IGS,IGRS,JREC,IAD0,IADV,NSEG,NSEGV,
     .        FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,SKIPFLAG,UID,NSEG_TOT,
     .        NSETS,NENTITY,KK,JJ
      CHARACTER TITR*nchartitle,KEY*ncharkey,KEY2*ncharkey
!
      TYPE (SURF_)   , DIMENSION(NSLIN+NSETS)   :: IGRSLIN
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
!   IGRSLIN(IGS)%ID   :: LINE identifier
!   IGRSLIN(IGS)%TITLE   :: LINE title
!   IGRSLIN(IGS)%NSEG   :: Number of lines
!   IGRSLIN(IGS)%LEVEL   :: FLAG "SUBLEVEL DONE" FOR LINES OF LINES
!                                 = 0 ! initialized line
!                                 = 1 ! uninitialized line
!   IGRSLIN(IGS)%ELEM(J) :: element attached to the line segment
!   IGRSLIN(IGS)%NODES(J,2) :: 2 nodes of the line segment
!   IGRSLIN(IGS)%PROC(J) :: field to store the processor ID  (/LINE only)
!
!   1. Set processor only when no element is set in lines
!   2. Split lines accordingly
!
C-----------------------------------------------
C     LIGNES DES LIGNES ...
C=======================================================================
      IF (FLAG == 0) ICOUNT=0

C     boucle sur les lignes  
      CALL HM_OPTION_START('/LINE')
      DO IGS=1,NSLIN
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)
        SKIPFLAG = 0                    
          NSEG=0                                            
          IF (KEY(1:4) == 'LINE') THEN                       
            IF (FLAG == 0 .AND. IGRSLIN(IGS)%NSEG == -1) THEN
              CALL HM_GET_INTV  ('idsmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)                 
              IF (SKIPFLAG == 0) THEN                                        
                DO KK=1,NENTITY                                                              
                  CALL HM_GET_INT_ARRAY_INDEX ('ids' ,JJ ,KK,IS_AVAILABLE,LSUBMODEL)       
                    IF (JJ /= 0) THEN                        
                      IGRS=0                                   
                      DO K=1,NSLIN
                        IF (JJ == IGRSLIN(K)%ID) THEN
                          IGRS=K
                          EXIT
                        ENDIF      
                      ENDDO                                    
                      IF (IGRS == 0)THEN                                   
                        CALL ANCMSG(MSGID=180,
     .                              MSGTYPE=MSGERROR,
     .                              ANMODE=ANINFO,
     .                              I1=ID,
     .                              C1=TITR,
     .                              I2=JJ)
                      !! comme les /surf/surf                      ELSEIF (IGRSLIN(IGRS)%NSEG == -1) THEN
                      ELSEIF (IGRSLIN(IGRS)%LEVEL == 0)THEN
C                       reference a une ligne non initialise     
                        IF (ITER > NSLIN) GOTO 900               
                        IGRSLIN(IGS)%NSEG=-1
                        IGRSLIN(IGS)%LEVEL=0
                        ICOUNT=1  
                        NSEG = 0                               
                        SKIPFLAG = 1
                        CYCLE                                 
                      ELSE                                       
C                       reference a une ligne initialise         
                        NSEGV=IGRSLIN(IGRS)%NSEG
                        NSEG=NSEG+NSEGV                          
                      ENDIF                                    
                    ENDIF                                                                            
                ENDDO 
              ENDIF                                        
              IF (SKIPFLAG == 0) THEN               
                INSEG=INSEG+6*NSEG                                
                IGRSLIN(IGS)%NSEG= NSEG
                CALL MY_ALLOC(IGRSLIN(IGS)%NODES,NSEG,2)
                IGRSLIN(IGS)%NODES(1:NSEG,1:2) = 0
                CALL MY_ALLOC(IGRSLIN(IGS)%ELTYP,NSEG)
                IGRSLIN(IGS)%ELTYP(1:NSEG) = 0
                CALL MY_ALLOC(IGRSLIN(IGS)%ELEM,NSEG)
                IGRSLIN(IGS)%ELEM(1:NSEG) = 0
                CALL MY_ALLOC(IGRSLIN(IGS)%PROC,NSEG)
                IGRSLIN(IGS)%PROC(1:NSEG) = 0
              ENDIF                                 
C-----------                                             
            ELSEIF (FLAG == 1 .AND. IGRSLIN(IGS)%LEVEL == 0 .AND. IGRSLIN(IGS)%NSEG > -1) THEN
              NSEG_TOT = 0
              CALL HM_GET_INTV  ('idsmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)                 
                DO KK=1,NENTITY                                                              
                  CALL HM_GET_INT_ARRAY_INDEX ('ids' ,JJ ,KK,IS_AVAILABLE,LSUBMODEL)       
                  IF (JJ /= 0) THEN                        
                    IGRS=0                                   
                    DO K=1,NSLIN
                      IF (JJ == IGRSLIN(K)%ID) THEN
                        IGRS=K
                        EXIT
                      ENDIF      
                    ENDDO                                    
                    IF (IGRSLIN(IGRS)%NSEG == -1) THEN
                      CYCLE                               
                    ELSE                                     
                      NSEGV=IGRSLIN(IGRS)%NSEG
                      DO L=1,NSEGV                       
                        NSEG_TOT = NSEG_TOT + 1
                        IGRSLIN(IGS)%NODES(NSEG_TOT,1) = IGRSLIN(IGRS)%NODES(L,1)
                        IGRSLIN(IGS)%NODES(NSEG_TOT,2) = IGRSLIN(IGRS)%NODES(L,2)
                        IGRSLIN(IGS)%ELTYP(NSEG_TOT)   = IGRSLIN(IGRS)%ELTYP(L)
                        IGRSLIN(IGS)%ELEM(NSEG_TOT)    = IGRSLIN(IGRS)%ELEM(L)
                      ENDDO
                    ENDIF                                    
                  ENDIF                                    
                ENDDO                                        
                IGRSLIN(IGS)%LEVEL=1                                         
            ENDIF   
        ENDIF                                           
      ENDDO
C-----------
      RETURN
 900  CONTINUE
      CALL ANCMSG(MSGID=189,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            C1='LINE',
     .            C2='LINE',
     .            I1=ID,
     .            C3='LINE',
     .            C4=TITR,
     .            C5='LINE',
     .            I2=IGRSLIN(IGS)%ID)
C-----------
      RETURN
      END
