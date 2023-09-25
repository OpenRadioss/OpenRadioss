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
Chd|  HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|-- called by -----------
Chd|        LECGROUP                      source/groups/lecgroup.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        GROUPS_GET_NENTITY            source/groups/groups_get_nentity.F
Chd|        HM_ADMLCNT                    source/groups/hm_admlistcnt.F 
Chd|        HM_ADMLIST                    source/groups/hm_admlist.F    
Chd|        HM_BIGBOX2                    source/model/box/hm_bigbox2.F 
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SUBMODGRE                  source/groups/hm_submodgr.F   
Chd|        HM_TAGPART                    source/groups/hm_tagpart.F    
Chd|        UDOUBLE_IGR                   source/system/sysfus.F        
Chd|        NINTLSTN                      source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_LECGRE(
     1           IGRELEM ,IX       ,
     2           NIX1    ,NIX      ,NUMEL   ,NGRELE  ,IELT     ,
     3           IPART   ,IPARTE   ,X       ,ELKEY   ,
     4           ISUBMOD ,FLAG     ,KELTREE ,ELTREE  ,KSONTREE ,
     5           NSONTREE,KLEVTREE ,SKEW    ,ISKN    ,UNITAB   ,
     6           ITABM1  ,IBOX     ,RTRANS  ,LSUBMODEL,
     7           IXX_S   ,IXX_S_IND,IADBOXMAX,SUBSET , STARTKEY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE OPTIONDEF_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr17_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NIX1  ,NIX    ,NUMEL ,NGRELE,IELT,FLAG
      INTEGER IPARTE(*),IPART(LIPART1,*),IX(NIX,*),ISUBMOD(*),
     .        KELTREE, ELTREE(KELTREE,*),
     .        KSONTREE, NSONTREE, KLEVTREE,ISKN(LISKN,*),ITABM1(*),
     .        IXX_S(*), IXX_S_IND(*)
      MY_REAL
     .        X(3,*),SKEW(LSKEW,*),RTRANS(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      CHARACTER ELKEY*4,STARTKEY*7
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS)  :: SUBSET
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRELE) :: IGRELEM
      TYPE (BOX_)    , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,II,JJ,KK,ISU,ID,NEL,N1,N2,IGS,JREC,
     .        IAD0,IADC,IADFIN,IT0,IT1,IT2,IT3,IT4,IT5,
     .        KAD,UID,IFLAGUNIT,
     .        ISK,BOXTYPE,J2(2),IT6,SUB_ID,IADBOX,NN,IADBOXMAX,
     .        IT7, IDB
      INTEGER NLIST,STAT,LIST_IGR(NGRELE)
      INTEGER IP, NLEV, MY_LEV,IDMIN,IDMAX,OFFSET,NLINES
      MY_REAL
     .        XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,BID,FAC_L,
     .        DIAM,XP1,YP1,ZP1,XP2,YP2,ZP2
      CHARACTER TITR*nchartitle,KEY*ncharkey,MES*40,KEY2*ncharkey,
     .          TYPE*nchartitle
      INTEGER, DIMENSION(:),ALLOCATABLE :: BUFTMP
      INTEGER, DIMENSION(:),ALLOCATABLE :: BUFBOX
      CHARACTER*nchartitle,
     .   TITR1
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
      INTEGER, DIMENSION(:), POINTER :: ELEM
      INTEGER,DIMENSION(:),ALLOCATABLE :: LIST_ENTITY
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
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTLST,NINTLSTN,LISTCNT
C-----------------------------------------------
C    IGRELEM(IGS)%ID : GROUP identifier
C    IGRELEM(IGS)%TITLE : GROUP title
C    IGRELEM(IGS)%NENTITY : Entities (elements) number of the GROUP
C    IGRELEM(IGS)%GRTYPE : TYPE ( 0-NOEUDS, 1-BRIC, 2-QUAD,   3-SHELL_4N,
!                                 4-TRUSS,  5-BEAM, 6-SPRINGS,7-SHELL_3N)
!!                   GRTYPE --- > OBSOLETE
C    IGRELEM(IGS)%SORTED  : FLAG for sorted/unsorted elements
!                    = 0 -> sorted
!                    = 1 -> unsorted
C    IGRELEM(IGS)%GRPGRP : TYPE of element GROUP
!                                 = 1 ELEM
!                                 = 2 GRELEM
!                                 = 3 SUBSET,PART,MAT,PROP
!                                 = 4 BOX,GENE
!                                 = 5 SUBMOD
C    IGRELEM(IGS)%LEVEL  : Hierarchy level
!                             (FLAG 'SUBLEVEL DONE' FOR GROUPS OF GROUPS)
!                            = 0 ---> not yet initialized
!                            = 1 ---> done
C=======================================================================
      MES(01:04) = ELKEY
      MES(05:18) = ' ELEMENT GROUP'
      MES(19:40) = '                      '
      IT0 = 0
      IT1 = 0
      IT2 = 0
      IT3 = 0
      IT4 = 0
      IT5 = 0
      IT6 = 0
      IT7 = 0
      TITR1='ELEMENT GROUP'
      ALLOCATE(BUFTMP(MAX(NUMEL*5,NPART)),STAT=stat)
        IF (STAT /= 0) THEN
          CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='BUFTMP')
        ENDIF
C-------------------------
C groupes d' elements + init IGRN(1,IGS)
C-------------------------
      IGS=0
      CALL HM_OPTION_START(TRIM(STARTKEY))      
      DO I=1,NGRELE  !NGRBRIC, NGRQUAD, etc...

        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)

            IGS=IGS+1
!
! initialized variables:
            IF (FLAG == 0) THEN
              IGRELEM(IGS)%ID = 0
              IGRELEM(IGS)%NENTITY = 0
              IGRELEM(IGS)%GRTYPE = 0
              IGRELEM(IGS)%SORTED = 0
              IGRELEM(IGS)%GRPGRP = 0
              IGRELEM(IGS)%LEVEL = 0
              IGRELEM(IGS)%R2R_ALL = 0
              IGRELEM(IGS)%R2R_SHARE = 0
            ENDIF
!
            IGRELEM(IGS)%ID=ID
            IGRELEM(IGS)%GRTYPE=IELT
            IGRELEM(IGS)%LEVEL=1
            IGRELEM(IGS)%TITLE=TITR
C
            IF(KEY(1:6) == STARTKEY(2:7))THEN
C             groupes de groupes
              IGRELEM(IGS)%NENTITY=-1
              IGRELEM(IGS)%GRPGRP=2
              IGRELEM(IGS)%LEVEL=0
              IT0=IT0+1
            ELSEIF(KEY(1:4) == ELKEY)THEN
C             groupe d'elements
              IT1=IT1+1
              IF (FLAG == 0) THEN
                IGRELEM(IGS)%NENTITY=0
                IGRELEM(IGS)%GRPGRP=1
              ENDIF
            ELSEIF(KEY(1:4) == 'PART' .OR. KEY(1:6) == 'SUBSET' .OR. KEY(1:3) == 'MAT' .OR. KEY(1:4) == 'PROP') THEN
C             groupe de SUBSET PART MAT OU PROP
              IT2=IT2+1
              IF (FLAG == 0) THEN
                IGRELEM(IGS)%NENTITY=0
                IGRELEM(IGS)%GRPGRP=3
              ENDIF
            ELSEIF((KEY(1:3) == 'BOX' .AND. NBBOX == 0 .AND.(KEY2(1:5) /= 'RECTA'.AND.
     .              KEY2(1:5) /= 'CYLIN'.AND.KEY2(1:5) /= 'SPHER')).OR.KEY(1:4) == 'GENE')THEN
C             groupe d'elements dans un box (old/obsolete/not supported with new reader) OR  GENE
              IT3=IT3+1
              IF (FLAG == 0) THEN
                IGRELEM(IGS)%NENTITY=0
                IGRELEM(IGS)%GRPGRP=4
              ENDIF
            ELSEIF(KEY(1:6) == 'SUBMOD')THEN
C             groupe d'elements dans un submodel
              IT4=IT4+1
              IF (FLAG  == 0) THEN
                IGRELEM(IGS)%NENTITY=0
                IGRELEM(IGS)%GRPGRP=5
              ENDIF
            ELSEIF(KEY(1:3) == 'BOX'.AND.(KEY2(1:5) == 'RECTA'.OR.
     .        KEY2(1:5) == 'CYLIN'.OR.KEY2(1:5) == 'SPHER'))THEN
C             old /grnod/box (not /BOX/BOX)
C             groupe de noeuds dans un box (clasical box, parallelepiped,
C                              cylindrical, spherical)
              IT5=IT5+1
              IF (FLAG == 0) THEN
                IGRELEM(IGS)%NENTITY=0
                IGRELEM(IGS)%GRPGRP=6
              ENDIF
            ELSEIF(KEY(1:3) == 'BOX' .AND. NBBOX > 0)THEN
C             multi box (box de box)
              IT6=IT6+1
            ELSEIF(KEY(1:8) == 'GEN_INCR')THEN
C             groupe d'elements gen_inc first last offset
C       
              IT7=IT7+1
              IF (FLAG == 0) THEN
                IGRELEM(IGS)%NENTITY=0
                IGRELEM(IGS)%GRPGRP=4
              ENDIF
            ENDIF
C
      ENDDO !next I

C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      IF (FLAG == 0) THEN
        DO IGS=1,NGRELE
          LIST_IGR(IGS) = IGRELEM(IGS)%ID
        ENDDO!next IGS
        CALL UDOUBLE_IGR(LIST_IGR,NGRELE,MES,0,BID)
      ENDIF

C-------------------------------------
C Remplacement des n0 d'elements user par systeme
C-------------------------------------

      IGS=0
      KK=0

      IF (IT1 /= 0) THEN

        BUFTMP = 0
        IF(NADMESH==0.OR.(ELKEY(1:4)/='SHEL'.AND.ELKEY(1:4)/='SH3N'))THEN
          CALL HM_OPTION_START(TRIM(STARTKEY))
          DO I=1,NGRELE !NGRBRIC, NGRQUAD, etc...
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITR  ,
     .                              UNIT_ID     = UID,
     .                              KEYWORD2    = KEY   ,
     .                              KEYWORD3    = KEY2)
              NEL=0
              NN = 0
              IGS=IGS+1
              IF(KEY(1:4) == ELKEY)THEN
                IF(FLAG == 0)THEN
                  CALL GROUPS_GET_NENTITY(NEL,LSUBMODEL)
                  IGRELEM(IGS)%NENTITY=NEL
                  CALL MY_ALLOC(IGRELEM(IGS)%ENTITY,NEL)
                  IGRELEM(IGS)%ENTITY = 0
                ELSEIF (FLAG == 1) THEN
                  
                  CALL GROUPS_GET_ELEM_LIST(LIST_ENTITY, NENTITY, LSUBMODEL)
                  DO KK=1,NENTITY
                      JJ=LIST_ENTITY(KK)
                      IF(JJ /= 0)THEN
                        NN = NN + 1
                        IGRELEM(IGS)%ENTITY(NN) = JJ
                      ENDIF
                  ENDDO! NEXT KK
                  IF(ALLOCATED(LIST_ENTITY))DEALLOCATE (LIST_ENTITY)
                  
                  IF ( NUMEL == 0 ) IGRELEM(IGS)%NENTITY = 0
                  NEL = IGRELEM(IGS)%NENTITY
                  IF (NEL > 0) THEN
                    TITR = IGRELEM(IGS)%TITLE
                    ELEM => IGRELEM(IGS)%ENTITY
                    NEL=NINTLSTN(
     .                 ELEM,NEL     ,IXX_S   ,NIX     ,NUMEL   ,
     .                 MES,IXX_S_IND,BUFTMP(1+2*NUMEL),ELKEY,
     .                 IGRELEM(IGS)%ID,TITR)
                    IGRELEM(IGS)%NENTITY=NEL
                  ENDIF
                ENDIF

              ENDIF
          ENDDO!next I
        ELSE
          CALL HM_OPTION_START(TRIM(STARTKEY))        
          DO I=1,NGRELE !NGRBRIC, NGRQUAD, etc ...
              NEL=0
              IGS=IGS+1
              CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                                OPTION_ID   = ID,
     .                                OPTION_TITR = TITR  ,
     .                                UNIT_ID     = UID,
     .                                KEYWORD2    = KEY   ,
     .                                KEYWORD3    = KEY2)
              IF(KEY(1:4) == ELKEY)THEN
                IF(FLAG == 0)THEN
                  CALL GROUPS_GET_NENTITY(NLIST,LSUBMODEL)
                  TITR=IGRELEM(IGS)%TITLE
                  CALL HM_ADMLCNT(NIX ,IX      ,NUMEL   ,IPARTE  ,IPART   ,
     .                         KELTREE  ,ELTREE  ,KSONTREE,NSONTREE,KLEVTREE,
     .                         NLIST    ,MES     ,BUFTMP ,BUFTMP(1+NUMEL),BUFTMP(1+2*NUMEL),
     .                         KK       ,NEL     ,ELKEY  ,IGRELEM(IGS)%ID,TITR,LSUBMODEL)
                  KK=1
                  IGRELEM(IGS)%NENTITY=NEL
                  CALL MY_ALLOC(IGRELEM(IGS)%ENTITY,NEL)
                  IGRELEM(IGS)%ENTITY = 0
                ELSEIF (FLAG == 1) THEN
                  NEL = IGRELEM(IGS)%NENTITY
                  IF (NEL > 0) THEN
                   CALL GROUPS_GET_NENTITY(NLIST,LSUBMODEL)
                   TITR=IGRELEM(IGS)%TITLE
                   ELEM => IGRELEM(IGS)%ENTITY
                   CALL HM_ADMLIST(NIX ,IX      ,NUMEL   ,IPARTE  ,IPART  ,
     .                          KELTREE ,ELTREE  ,KSONTREE,NSONTREE,KLEVTREE,
     .                          NLIST   ,MES     ,BUFTMP ,BUFTMP(1+NUMEL),BUFTMP(1+2*NUMEL),
     .                          KK      ,NEL     ,ELEM,ELKEY,IGRELEM(IGS)%ID,TITR,LSUBMODEL)
                   KK=1
                  ENDIF
                ENDIF
              ENDIF
          ENDDO! next I
        ENDIF
      ENDIF
C-------------------------
C BOX, GENERATION
C-------------------------
      IGS=0
      IF (IT3 /= 0) THEN
        CALL HM_OPTION_START(TRIM(STARTKEY))
        DO I=1,NGRELE

          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR  ,
     .                            UNIT_ID     = UID,
     .                            KEYWORD2    = KEY   ,
     .                            KEYWORD3    = KEY2)
            NN = 0
            NEL=0
            IGS=IGS+1
C-------------------------
C BOX (OLD)
C-------------------------
            IF(KEY(1:3) == 'BOX'.AND.(KEY2(1:5) /= 'RECTA'.AND.KEY2(1:5) /= 'CYLIN'.AND.KEY2(1:5) /= 'SPHER').AND.NBBOX == 0)THEN
              !no longer supported
                       !ERROR MESSAGE
C-------------------------
C GENERATION
C-------------------------
            ELSEIF(KEY(1:4) == 'GENE')THEN
              IADC = 0
              BUFTMP = 0
              CALL HM_GET_INTV  ('grnodGenArrCnt' ,NENTITY,IS_AVAILABLE,LSUBMODEL)                           
              DO KK=1,NENTITY                                                                        
                  CALL HM_GET_INT_ARRAY_INDEX        ('Ifirst'    ,N1    ,KK,IS_AVAILABLE,LSUBMODEL)    
                  CALL HM_GET_INT_ARRAY_INDEX        ('Ilast'     ,N2    ,KK,IS_AVAILABLE,LSUBMODEL)    
                  IF(N2>N1)THEN                                         
                   !tag des elements                                                                 
                   DO K=1,NUMEL                                                                      
                     IF (IX(NIX,K) >= N1 .AND. IX(NIX,K) <= N2) BUFTMP(K)=1                            
                   ENDDO!next K                                                                           
                 ENDIF                                                                               
              ENDDO! NEXT KK                                                                         
              
              IF(NADMESH==0)THEN
                NEL=0
                IF (FLAG == 0) THEN
                  DO J=1,NUMEL
                    IF (BUFTMP(J) == 1) NEL=NEL+1
                  ENDDO!next J
                  IGRELEM(IGS)%NENTITY=NEL
                  CALL MY_ALLOC(IGRELEM(IGS)%ENTITY,NEL)
                  IGRELEM(IGS)%ENTITY = 0
                ELSEIF (FLAG == 1) THEN
                  DO J=1,NUMEL
                    IF (BUFTMP(J) == 1) THEN
                      NN = NN + 1
                      IGRELEM(IGS)%ENTITY(NN)=J
                    ENDIF
                  ENDDO!next J
                ENDIF
              ELSE
                NEL=0
                IF (FLAG == 0) THEN
                  DO J=1,NUMEL
                    IF (BUFTMP(J) == 1) THEN
                      IP=IPARTE(J)
                      NLEV  =IPART(10,IP)
                      MY_LEV=ELTREE(KLEVTREE,J)
                      IF(MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                      IF(MY_LEV==NLEV)NEL=NEL+1
                    END IF
                  ENDDO!next J
                  IGRELEM(IGS)%NENTITY=NEL
                  CALL MY_ALLOC(IGRELEM(IGS)%ENTITY,NEL)
                  IGRELEM(IGS)%ENTITY = 0
                ELSEIF (FLAG == 1) THEN
                  DO J=1,NUMEL
                    IF (BUFTMP(J) == 1) THEN
                      IP=IPARTE(J)
                      NLEV  =IPART(10,IP)
                      MY_LEV=ELTREE(KLEVTREE,J)
                      IF(MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                      IF(MY_LEV==NLEV)THEN
                        NN = NN + 1
                        IGRELEM(IGS)%ENTITY(NN)=J
                      ENDIF
                    ENDIF
                  ENDDO!next J
                ENDIF
              ENDIF
          ENDIF
        ENDDO! next I
      ENDIF
C-------------------------
C BOX (parallelepiped, cylindrical, spherical) - old one (10SA1)
C-------------------------

      IGS=0
      IF (IT5 /= 0) THEN
         !obsolete
         
         !ERROR MESSAGE
 
      ENDIF
C-------------------------
C NEW BOX OPTION (MULTI BOX COMBINATION)
C-------------------------
      IGS=0
      IF(IT6 /= 0)THEN
C---
C
        IF (FLAG == 0) THEN
          ALLOCATE(BUFBOX(1))
          BUFBOX = 0
        ELSEIF (FLAG == 1) THEN
          ALLOCATE(BUFBOX(IADBOXMAX))
          BUFBOX(1:IADBOXMAX) = 0
        ENDIF
!
        CALL HM_OPTION_START(TRIM(STARTKEY))
        DO I=1,NGRELE
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITR  ,
     .                              UNIT_ID     = UID,
     .                              KEYWORD2    = KEY   ,
     .                              KEYWORD3    = KEY2)
            NN = 0
            NEL=0
            IGS=IGS+1
            IF (KEY(1:3) == 'BOX'.AND. NBBOX > 0)THEN
              BUFTMP = 0
              IADBOX = 1
              IFLAGUNIT = 0
              ID = IGRELEM(IGS)%ID
              DO J=1,NUNITS
                IF (UNITAB%UNIT_ID(J) == UID) THEN
                  FAC_L = UNITAB%FAC_L(J)
                  IFLAGUNIT = 1
                  EXIT
                ENDIF
              ENDDO
              IF (UID/=0.AND.IFLAGUNIT==0) THEN
                CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                      I2=UID,I1=ID,C1='ELEMENT GROUP',
     .                      C2='ELEMENT GROUP',
     .                      C3=TITR)
              ENDIF
C---
              CALL HM_GET_INT_ARRAY_INDEX        ('ids'   ,IDB    ,1,IS_AVAILABLE,LSUBMODEL)                
              CALL HM_BIGBOX2(X ,FLAG   ,NEL     ,
     .                SKEW   ,IGS    ,ISKN    ,ITABM1,IBOX   ,
     .                ID     ,NADMESH,NIX     ,IX    ,NIX1   ,NUMEL ,
     .                IPARTE ,IPART  ,KLEVTREE,ELTREE,KELTREE,BUFTMP,
     .                KEY     ,TITR  ,MES    ,IGRELEM,NGRELE ,NN    ,
     .                IADBOX ,IADBOXMAX,BUFBOX,IDB)
C---
             IADBOXMAX = MAX(IADBOX,IADBOXMAX)
              IF(FLAG == 0)THEN
                IGRELEM(IGS)%NENTITY=NEL
                CALL MY_ALLOC(IGRELEM(IGS)%ENTITY,NEL)
                IGRELEM(IGS)%ENTITY = 0
              ELSE IF(FLAG == 1)THEN
                IGRELEM(IGS)%NENTITY=NEL
              ENDIF ! IF(FLAG == 0)
            ENDIF


        ENDDO
        IF(ALLOCATED(BUFBOX))DEALLOCATE(BUFBOX)
      ENDIF ! IF(IT6 /= 0)
C-------------------------
C groupes de SUBSETS,PART,MAT,PROP
C-------------------------

      IGS=0
      IF (IT2 > 0) THEN
        CALL HM_OPTION_START(TRIM(STARTKEY))
        DO I=1,NGRELE
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITR    ,
     .                              UNIT_ID     = UID     ,
     .                              KEYWORD2    = KEY     ,
     .                              KEYWORD3    = KEY2)
            NN = 0
            NEL=0
            IGS=IGS+1
            IF (KEY(1:6)=='SUBSET' .OR. KEY(1:4)=='PART' .OR. KEY(1:3)=='MAT' .OR. KEY(1:4)=='PROP') THEN
C---
              BUFTMP = 0
              CALL HM_TAGPART(BUFTMP, IPART, KEY, IGRELEM(IGS)%ID, TITR, TITR1, FLAG, SUBSET, LSUBMODEL)
C---
              IF(NADMESH==0)THEN
                IF (FLAG == 0) THEN
                  DO J=1,NUMEL
                    IF (BUFTMP(IPARTE(J)) == 1) NEL=NEL+1
                  ENDDO
                  IGRELEM(IGS)%NENTITY=NEL
                  CALL MY_ALLOC(IGRELEM(IGS)%ENTITY,NEL)
                  IGRELEM(IGS)%ENTITY = 0
                ELSEIF (FLAG == 1) THEN
                  DO J=1,NUMEL
                    IF (BUFTMP(IPARTE(J)) == 1) THEN
                      NN = NN + 1
                      IGRELEM(IGS)%ENTITY(NN)=J
                    ENDIF
                  ENDDO
                ENDIF
              ELSE
                IF (FLAG == 0) THEN
                  DO J=1,NUMEL
                    IP=IPARTE(J)
                    IF(BUFTMP(IP) == 1) THEN
                      NLEV  =IPART(10,IP)
                      MY_LEV=ELTREE(KLEVTREE,J)
                      IF(MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                      IF(MY_LEV==NLEV)NEL=NEL+1
                    END IF
                  ENDDO
                  IGRELEM(IGS)%NENTITY=NEL
                  CALL MY_ALLOC(IGRELEM(IGS)%ENTITY,NEL)
                  IGRELEM(IGS)%ENTITY = 0
                ELSEIF (FLAG == 1) THEN
                  DO J=1,NUMEL
                    IP=IPARTE(J)
                    IF (BUFTMP(IP) == 1) THEN
                      NLEV  =IPART(10,IP)
                      MY_LEV=ELTREE(KLEVTREE,J)
                      IF(MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                      IF(MY_LEV==NLEV)THEN
                        NN = NN + 1
                        IGRELEM(IGS)%ENTITY(NN)=J
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ENDIF

        ENDDO
      ENDIF
C-------------------------
C groupes de SUBMODELS
C-------------------------

      IGS=0
      IF (IT4 > 0) THEN
        CALL HM_OPTION_START(TRIM(STARTKEY))
        DO I=1,NGRELE
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITR    ,
     .                              UNIT_ID     = UID     ,
     .                              KEYWORD2    = KEY     ,
     .                              KEYWORD3    = KEY2)
            NN = 0
            NEL=0
            IGS=IGS+1
            IF (KEY(1:6) == 'SUBMOD') THEN
              CALL HM_SUBMODGRE(
     .             ISUBMOD ,IX      ,NIX     ,ID       ,
     .             NEL     ,NUMEL   ,IELT    ,MES      ,
     .             FLAG    ,TITR    ,TITR1   ,LSUBMODEL,IGRELEM ,
     .             NN      ,IGS     )
              IF (FLAG == 0) THEN
                IGRELEM(IGS)%NENTITY=NEL
                CALL MY_ALLOC(IGRELEM(IGS)%ENTITY,NEL)
                IGRELEM(IGS)%ENTITY = 0
              ENDIF
            ENDIF

        ENDDO
      ENDIF
C-------------------------
C GENERATION MIN MAX OFFSET
C-------------------------
      IGS=0
      IF (IT7 /= 0) THEN
        CALL HM_OPTION_START(TRIM(STARTKEY))
        DO I=1,NGRELE
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITR    ,
     .                              UNIT_ID     = UID     ,
     .                              KEYWORD2    = KEY     ,
     .                              KEYWORD3    = KEY2)
            NN = 0
            NEL=0
            IGS=IGS+1
            IF(KEY(1:8) == 'GEN_INCR')THEN
              IADC = 0
              BUFTMP = 0
              CALL HM_GET_INTV  ('grnodGenArrCnt' ,NLINES,IS_AVAILABLE,LSUBMODEL)  
              DO KK=1,NLINES              
                CALL HM_GET_INT_ARRAY_INDEX        ('Ifirst'   ,IDMIN    ,KK,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX        ('Ilast'    ,IDMAX    ,KK,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX        ('Iincr'    ,OFFSET   ,KK,IS_AVAILABLE,LSUBMODEL)                                
                !optimized loop
                DO K=1,NUMEL
                  ID=IX(NIX,K)
                  IF(ID<IDMIN .OR. ID>IDMAX)CYCLE
                  IF(MOD(ID-IDMIN,OFFSET)==0)BUFTMP(K)=1
                ENDDO
              ENDDO
              
              IF(NADMESH==0)THEN
                NEL=0
                IF (FLAG == 0) THEN
                  DO J=1,NUMEL
                    IF (BUFTMP(J) == 1) NEL=NEL+1
                  ENDDO
                  IGRELEM(IGS)%NENTITY=NEL
                  CALL MY_ALLOC(IGRELEM(IGS)%ENTITY,NEL)
                  IGRELEM(IGS)%ENTITY = 0
                ELSEIF (FLAG == 1) THEN
                  DO J=1,NUMEL
                    IF (BUFTMP(J) == 1) THEN
                      NN = NN + 1
                      IGRELEM(IGS)%ENTITY(NN)=J
                    ENDIF
                  ENDDO
                ENDIF
              ELSE
                NEL=0
                IF (FLAG == 0) THEN
                  DO J=1,NUMEL
                    IF (BUFTMP(J) == 1) THEN
                      IP=IPARTE(J)
                      NLEV  =IPART(10,IP)
                      MY_LEV=ELTREE(KLEVTREE,J)
                      IF(MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                      IF(MY_LEV==NLEV)NEL=NEL+1
                    END IF
                  ENDDO
                  IGRELEM(IGS)%NENTITY=NEL
                  CALL MY_ALLOC(IGRELEM(IGS)%ENTITY,NEL)
                  IGRELEM(IGS)%ENTITY = 0
                ELSEIF (FLAG == 1) THEN
                  DO J=1,NUMEL
                    IF (BUFTMP(J) == 1) THEN
                      IP=IPARTE(J)
                      NLEV  =IPART(10,IP)
                      MY_LEV=ELTREE(KLEVTREE,J)
                      IF(MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                      IF(MY_LEV==NLEV)THEN
                        NN = NN + 1
                        IGRELEM(IGS)%ENTITY(NN)=J
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ENDIF

        ENDDO!next I
      ENDIF
C------------------------------ 
      IF (ALLOCATED(BUFTMP)) DEALLOCATE(BUFTMP)
C------------------------------
      RETURN
      END
