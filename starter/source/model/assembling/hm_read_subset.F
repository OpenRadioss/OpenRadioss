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
Chd|  HM_READ_SUBSET                source/model/assembling/hm_read_subset.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ECRSUB2                       source/model/assembling/hm_read_subset.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        UDOUBLE_IGR                   source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SUBSET(SUBSET,IPART,NSUBS,NPART,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /SUBSET USING HM_READER
C   BUILD SUBSET HIERARCHY
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     SUBSET          SUBSET STRUCTURE
C     IPART           PART ARRAY
C     NSUBS           SUBSET NUMBER ( INCLUDING GLOBAL SUBSET )
C     NPART           PART NUMBER
C     LSUBMODEL       SUBMODEL STRUCTURE    
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      INTEGER,INTENT(IN)::NSUBS
      INTEGER,INTENT(IN)::NPART
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
c      TYPE (SUBSET_),DIMENSION(NSUBS),INTENT(OUT)::SUBSET
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C MODIFIED ARGUMENTS
      INTEGER,INTENT(INOUT)::IPART(LIPART1,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ID,IDV,IAD,IP,IS,ISU,NSU,NL,NC,NP,NTP,NS,
     .   CONT,NIVEAU,NIVMAX,LIST_IGR(NSUBS),UID,SUB_INDEX
      INTEGER IFIX_TMP,TITLEN,ICHILD
      INTEGER J10(10)
      INTEGER, DIMENSION(NPART+NSUBS) :: BUFTMP
      my_real BID
      CHARACTER TITR*nchartitle,MESS*40
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  LISTCNT,SUBLVL
c-----------------------------------------------
      DATA MESS/' SUBSET DEFINITION                      '/
c-----------------------------------------------
C  SUBSET(ISU)%ID  ::SUBSET identifier
C  SUBSET(ISU)%TITLE  : SUBSET title
C  SUBSET(ISU)%TH_FLAG : TIME HISTORY flag
C  SUBSET(ISU)%PARENT : SUBSET PARENT
C  SUBSET(ISU)%NCHILD : SUBSETS number of childs
C  SUBSET(ISU)%NPART : SUBSET number of part (within one subset)
C  SUBSET(ISU)%NTPART : Total number of parts (within parent subset)
C  SUBSET(ISU)%THIAD : IAD for global ITHBUF storage variables
C  SUBSET(ISU)%NVARTH : nb of subset TH/ATH variable (10)
!     /iTH , i=A,,B, ... I 
!     (  /iTH --> 9 additional time history files + 1 for /TH )
C=======================================================================
      IS_AVAILABLE = .FALSE.
      SUB_INDEX = 0
      UID = 0
      ISU = 0
      BUFTMP(:) = 0 
C--------------------------------------------------
C      START READING SUBSETS
C-------------------------------------------------- 
      CALL HM_OPTION_START('SUBSET')  
C--------------------------------------------------
C BROWSING SUBSETS 1->NSUBS-1 ( NSUBS = NUMBER OF /SUBSET + GLOBAL_SUBSET )
C--------------------------------------------------
      DO I=1,NSUBS-1
        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /SUBSET/... LINE
C--------------------------------------------------
c        CALL HM_OPTION_READ(ID,UID,SUB_INDEX,TITR,LSUBMODEL)
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       OPTION_TITR = TITR)
      IF(LEN(TITR)==0 .OR. LEN(TRIM(TITR))==0 )TITR(1:6)='noname'
C--------------------------------------------------
C EXTRACT DATAS NUMBER OF CHILDREN(INTEGER VALUE)
C--------------------------------------------------
        CALL HM_GET_INTV('numberofassemblies',NSU,IS_AVAILABLE,LSUBMODEL)
        ISU = ISU+1
        SUBSET(ISU)%ID = ID
        SUBSET(ISU)%LEVEL  = 0
        SUBSET(ISU)%PARENT = 0
        SUBSET(ISU)%NPART  = 0
        SUBSET(ISU)%NCHILD = NSU
        SUBSET(ISU)%TH_FLAG = 0
        CALL MY_ALLOC(SUBSET(ISU)%NVARTH,10) 
c        /iTH , i=A,,B, ... I 
c     (  /iTH --> 9 additional time history files + 1 for /TH )
        SUBSET(ISU)%NVARTH(1:10) = 0
        SUBSET(ISU)%THIAD = 0
        CALL MY_ALLOC(SUBSET(ISU)%CHILD,NSU)
        DO K=1,NSU
          SUBSET(ISU)%CHILD(K) = 0
        ENDDO
        SUBSET(ISU)%TITLE = TITR
C--------------------------------------------------
C EXTRACT CHILDREN(INTEGER VALUES)
C--------------------------------------------------
        DO NS=1,NSU
          CALL HM_GET_INT_ARRAY_INDEX('assemblies',ICHILD,NS,IS_AVAILABLE,LSUBMODEL)
          SUBSET(ISU)%CHILD(NS) = ICHILD
        ENDDO
      ENDDO
C-------------------------------------
c SEARCH FOR DUPLICATED IDs
C-------------------------------------
      LIST_IGR(1:NSUBS) = 0
      DO ISU=1,NSUBS-1
        LIST_IGR(ISU) = SUBSET(ISU)%ID
      ENDDO
      CALL UDOUBLE_IGR(LIST_IGR,NSUBS,MESS,0,BID)
C-------------------------------------
C REPLACE USER IDs WITH SYSTEM IDs
C-------------------------------------
      DO ISU=1,NSUBS-1
        NSU = SUBSET(ISU)%NCHILD
        DO I=1,NSU
          ID = SUBSET(ISU)%CHILD(I)
          SUBSET(ISU)%CHILD(I) = 0
          DO IS=1,NSUBS
            IDV = SUBSET(IS)%ID
            IF (ID == IDV) THEN
              SUBSET(ISU)%CHILD(I) = IS
              SUBSET(IS)%PARENT  = ISU
            ENDIF
          ENDDO
          IF (SUBSET(ISU)%CHILD(I) == 0) THEN
             CALL ANCMSG(MSGID=182,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO,
     .                   I1=SUBSET(ISU)%ID,
     .                   C1=SUBSET(ISU)%TITLE,
     .                   I2=ID,
     .                   I3=ID)
          ENDIF
        ENDDO
      ENDDO
C-------------------------------------
c     COMPACTION (INEXISTING SUBSET <=> MSGID=182)
C-------------------------------------
      DO ISU=1,NSUBS-1
        NS  = SUBSET(ISU)%NCHILD
        NSU = 0
        DO I=1,NS
          ID = SUBSET(ISU)%CHILD(I)
          IF (ID /= 0) THEN
            NSU = NSU + 1
            SUBSET(ISU)%CHILD(NSU) = ID
          ENDIF
        ENDDO
        SUBSET(ISU)%NCHILD = NSU
      ENDDO
C-------------------------------------
c     CREATE GLOBAL SUBSET
C-------------------------------------
      BUFTMP(:) = 0 
      TITR = 'GLOBAL MODEL'
      SUBSET(NSUBS)%TITLE = TITR
c
      SUBSET(NSUBS)%ID     = 0
      SUBSET(NSUBS)%LEVEL  = 0
      SUBSET(NSUBS)%PARENT = 0
      SUBSET(NSUBS)%NCHILD = 0
      SUBSET(NSUBS)%NPART  = 0
      SUBSET(NSUBS)%TH_FLAG = 0
      CALL MY_ALLOC(SUBSET(NSUBS)%NVARTH,10) !  /iTH , i=A,,B, ... I 
!     (  /iTH --> 9 additional time history files + 1 for /TH )
      SUBSET(NSUBS)%NVARTH(1:10) = 0
      SUBSET(NSUBS)%THIAD = 0
!
      NSU = 0
      DO ISU=1,NSUBS-1
        IF (SUBSET(ISU)%PARENT == 0) THEN
          SUBSET(ISU)%PARENT = NSUBS
          NSU = NSU+1
          BUFTMP(NSU) = ISU
        ENDIF
      ENDDO
!==================================================
      SUBSET(NSUBS)%NCHILD = NSU
      CALL MY_ALLOC(SUBSET(NSUBS)%CHILD,NSU)
      DO I=1,NSU
        SUBSET(NSUBS)%CHILD(I) = BUFTMP(I)
      ENDDO
!==================================================
C-------------------------------------
c     SEARCH CHILDREN OF EACH SUBSET
C-------------------------------------
      DO ISU=1,NSUBS
        ID = SUBSET(ISU)%ID
        BUFTMP(:) = 0
        NP = 0
        DO K=1,NPART
          IF (ID == IPART(7,K)) THEN
            IPART(3,K) = ISU
            NP = NP+1
            BUFTMP(NP) = K
          ENDIF
        ENDDO
        SUBSET(ISU)%NPART = NP   
        CALL MY_ALLOC(SUBSET(ISU)%PART,NP)
        DO K=1,NP
          SUBSET(ISU)%PART(K) = BUFTMP(K) 
        ENDDO
      ENDDO
C-------------------------------------
c     CHECK SUBSETs REFERENCED BY PARTs
C-------------------------------------
      DO K=1,NPART
        IF (IPART(3,K) == 0) THEN
           CALL FRETITL2(TITR,IPART(LIPART1-LTITR+1,K),LTITR)
           CALL ANCMSG(MSGID=183,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO,
     .                 I1=IPART(4,K),
     .                 C1=TITR,
     .                 I2=IPART(7,K))
        ENDIF
      ENDDO
C-------------------------------------
c     SORTING SUBSET BY LEVEL NSUBS*LOG(NSUBS)
C-------------------------------------
      NIVMAX = 0
      CONT = 1
      DO WHILE (CONT == 1)
        CONT = 0
        DO ISU=1,NSUBS
          ID = SUBSET(ISU)%PARENT
          IF (ID > 0) THEN
            NIVEAU = SUBSET(ID)%LEVEL + 1
            IF (SUBSET(ISU)%LEVEL /= NIVEAU) THEN
              SUBSET(ISU)%LEVEL = NIVEAU
              NIVMAX = MAX(NIVMAX,NIVEAU)
              CONT = 1
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C-------------------------------------
c     SEARCH PARTS IN DESCENDANCE (recursive) 
C-------------------------------------
      DO ISU = 1,NSUBS
        BUFTMP(:) = 0
        NTP = 0
        NC  = SUBSET(ISU)%NCHILD
        IF (NC == 0) NC = SUBSET(ISU)%NPART
        DO WHILE (NC > 0)
          NC = SUBLVL(SUBSET,NSUBS,ISU,NTP,BUFTMP)               
        ENDDO
        SUBSET(ISU)%NTPART = NTP
        CALL MY_ALLOC(SUBSET(ISU)%TPART,NTP)
        DO I=1,NTP
          SUBSET(ISU)%TPART(I) = BUFTMP(I)
        ENDDO
      ENDDO
C-------------------------------------
c     WRITING SUBSETS  (TREE WRITING)
C-------------------------------------
      WRITE(IOUT,'(//A)')'       HIERARCHICAL SUBSET ORGANIZATION' 
      WRITE(IOUT,'(A//)')'       --------------------------------' 
      IAD = 1
      BUFTMP(:) = 0
      DO ISU=1,NSUBS
        IF (SUBSET(ISU)%LEVEL == 0) THEN
          BUFTMP(IAD) = ISU
          DO WHILE (IAD > 0)
            I = BUFTMP(IAD)
            NSU = SUBSET(I)%NCHILD
            IAD = IAD - 1                             
            CALL ECRSUB2(SUBSET,NSUBS,I,IPART,NIVMAX)  
            IF (NSU > 0) THEN                          
              DO K = NSU,1,-1                          
                IAD = IAD+1                            
                BUFTMP(IAD) = SUBSET(I)%CHILD(K)       
              ENDDO                                    
            ENDIF                                      
          ENDDO
        ENDIF
      ENDDO
C=======================================================================
      RETURN
      END
      
       RECURSIVE INTEGER FUNCTION SUBLVL(SUBSET,NSUBS,ISU,NP,BUFTMP)
     . RESULT(NS)
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISU,NP,NSUBS,BUFTMP(*)
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NC,IS,IP,NK
c=======================================================================      
      NS = SUBSET(ISU)%NCHILD
      DO J=1,SUBSET(ISU)%NPART                  
        NP = NP + 1                         
        BUFTMP(NP) = SUBSET(ISU)%PART(J)     
      ENDDO                                     
      IF (NS > 0) THEN
        NC = 0
        DO I = 1,NS
          IS = SUBSET(ISU)%CHILD(I)
          NK = SUBLVL(SUBSET,NSUBS,IS,NP,BUFTMP)
          NC = NC + NK
        ENDDO
        IF (NC == 0) NS = 0
      ENDIF
c-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  ECRSUB2                       source/model/assembling/hm_read_subset.F
Chd|-- called by -----------
Chd|        HM_READ_SUBSET                source/model/assembling/hm_read_subset.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ECRSUB2(SUBSET,NSUBS,ISU,IPART,NIVMAX)
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSUBS,ISU,IPART(LIPART1,*)
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,K2,L,LL,NP,IP,ID,NIVEAU,NIVMAX,TITLEN
      INTEGER CNT(0:33)
      CHARACTER LIGNE*132,LIGN2*132,BAR(33)*10,BLI*21
      CHARACTER TITR*nchartitle
      DATA BAR /33*'|         '/
      DATA BLI /'|____________________'/
      DATA CNT /34*0/
C=======================================================================

      LIGNE=' '
      ID = SUBSET(ISU)%ID
      NIVEAU = SUBSET(ISU)%LEVEL
      IF (NIVEAU >= 33) GOTO 999
      CNT(NIVEAU) = CNT(NIVEAU)+1
      BAR(NIVEAU+1)='|         '
      CNT(NIVEAU+1)=0

      L = MIN(10,MAX(2,22/(NIVMAX+1)))
      
c     lignes vides avant subset 
      K = 3
      DO I=1,NIVEAU
        LIGNE(K:K+L) = BAR(I)(1:L)
        K = K+L
        IF (K > 132) GOTO 999
      ENDDO
      K = K-1
      WRITE(IOUT,'(A)')LIGNE(1:K)
      WRITE(IOUT,'(A)')LIGNE(1:K)
c--------------------------------
      IF (NIVEAU == 0) THEN
      ELSEIF (CNT(NIVEAU) == SUBSET(SUBSET(ISU)%PARENT)%NCHILD) THEN
        BAR(NIVEAU) = '          '
      ENDIF
      
c     ligne subset
      LIGNE=' '
      IF (NIVEAU == 0) THEN
        K = 2
      ELSE
        K = 3
      ENDIF
C  +++
!      if (ID == 50000005)then
!        write(iout,*)'MIRC_STOP_SUBSETS',ID,ISU,NIVEAU,L
!        write(iout,*)'MIRC_STOP_SUBSETS',LEN(SUBSET(ISU)%TITLE),
!     . SUBSET(ISU)%TITLE
!        stop
!      endif
C  ---
      DO I=1,NIVEAU-1
        LIGNE(K:K+L-1)=BAR(I)(1:L)
        K=K+L
        IF (K > 132) GOTO 999
      ENDDO
      IF (NIVEAU /= 0) THEN
        LIGNE(K:K+L-2)=BLI(1:L-1)
        K = K+L-1
        IF (K > 132-16) GOTO 999
      ENDIF
      WRITE(LIGNE(K:K+17),FMT='(A7,I10,A1)')'SUBSET:',ID,','
      K = K+17
      I = 0
      TITLEN = LEN(SUBSET(ISU)%TITLE)
      DO WHILE (I < TITLEN .AND. K < 132)
        K=K+1
        I=I+1
        LIGNE(K:K) = SUBSET(ISU)%TITLE(I:I)
      ENDDO
      WRITE(IOUT,'(A)')LIGNE(1:K)
      
c    subset souligne + ligne vide apres subset
      K=3
      LIGNE=' '
      DO I=1,NIVEAU
        LIGNE(K:K+L-1)=BAR(I)(1:L)
        K=K+L
        IF(K > 132)GOTO 999
      ENDDO
      K2 = K
      K  = K-1
      WRITE(LIGNE(K:K+5),FMT='(A6)')'~~~~~~' 
      WRITE(IOUT,'(A)')LIGNE(1:K+5)
c     part
      NP = SUBSET(ISU)%NPART
      IF (NP == 0) RETURN

      K = K2
      LIGNE(K-1:K)=' |'
      WRITE(IOUT,'(A)')LIGNE(1:K)
      DO LL=1,NP
        IP = SUBSET(ISU)%PART(LL)

        LIGNE=' '
        K=3
        DO I=1,NIVEAU
          LIGNE(K:K+L-1)=BAR(I)(1:L)
          K = K+L
          IF (K > 132-20) GOTO 999
        ENDDO
c
        IF (LL == 1) THEN
          LIGNE(K:K+L-2)=BLI(1:L-1)
          WRITE(LIGNE(K+L-1:K+L+17),FMT='(A8,I10,A1)')
     .         'Part(s):',IPART(4,IP),','
        ELSEIF (SUBSET(ISU)%NCHILD == 0) THEN
          WRITE(LIGNE(K+L-1:K+L+17),FMT='(A8,I10,A1)')
     .         '        ',IPART(4,IP),','
        ELSE
          LIGNE(K:K)='|'
          WRITE(LIGNE(K+L-1:K+L+17),FMT='(A8,I10,A1)')
     .         '        ',IPART(4,IP),','
        ENDIF
        K = K + L + 17
        I = 0
        CALL FRETITL2(TITR,IPART(LIPART1-LTITR+1,IP),LTITR)
        DO WHILE (I < 40 .AND. K < 132)
          K = K + 1
          I = I + 1
          LIGNE(K:K) = TITR(I:I)
        ENDDO
        WRITE(IOUT,'(A)')LIGNE(1:K)
C              
      ENDDO
c-----------
      RETURN
c-----------
 999  CALL ANCMSG(MSGID=170,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            C1=LIGNE(1:132))
c-----------
      RETURN
      END

      
