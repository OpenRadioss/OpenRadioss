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
Chd|  HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_BIGBOX                     source/model/box/hm_bigbox.F  
Chd|        HM_ELNGR                      source/groups/hm_elngr.F      
Chd|        HM_ELNGRR                     source/groups/hm_elngrr.F     
Chd|        HM_ELNGRS                     source/groups/hm_elngr.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_LINENGR                    source/groups/hm_linengr.F    
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SUBMODGRN                  source/groups/hm_submodgr.F   
Chd|        HM_SURFNOD                    source/groups/hm_surfnod.F    
Chd|        HM_TAGPART                    source/groups/hm_tagpart.F    
Chd|        TAGNOD                        source/groups/tagnod.F        
Chd|        TAGNODR                       source/groups/tagnodr.F       
Chd|        TAGNODS                       source/groups/tagnod.F        
Chd|        TAGNODX                       source/groups/tagnodx.F       
Chd|        UDOUBLE_IGR                   source/system/sysfus.F        
Chd|        ULIST2S                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_LECGRN(
     1           ITAB    ,ITABM1  ,IGRNOD  ,
     2           ISUBMOD ,X       ,GEO     ,IXS     ,
     3           IXQ     ,IXC     ,IXT     ,IXP     ,IXR     ,
     4           IXTG                               ,IPART   ,
     5           IPARTS  ,IPARTQ  ,IPARTC  ,IPARTT  ,IPARTP  ,
     6           IPARTR  ,IPARTG  ,IPARTSP ,KXSP    ,
     7           FLAG    ,MAXNNOD ,SKEW    ,ISKN    ,
     8           UNITAB  ,IBOX    ,IXS10   ,IXS20   ,
     9           IXS16   ,RTRANS  ,LSUBMODEL,IXX    ,
     A           KXX     ,IPARTX  ,IADBOXMAX,IGRSLIN,SUBSET  ,
     B           IGRBRIC ,IGRQUAD ,IGRSH4N ,IGRSH3N ,IGRTRUSS,
     C           IGRBEAM ,IGRSPRING,IGRSURF,NSETS   )
C !!! WARNING due to issues on ibm plateform keep IBUFTMP at the last position
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "sysunit.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ITABM1(*),
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXT(NIXT,*),
     .        IXP(NIXP,*),IXR(NIXR,*),IXTG(NIXTG,*),IPARTS(*),
     .        IPARTQ(*),IPARTC(*),IPARTT(*),IPARTP(*),IPARTR(*),
     .        IPARTG(*),IPART(LIPART1,*),ITAB(*),
     .        IXS10(6,*) ,IXS20(12,*) ,IXS16(8,*),
     .        KXSP(NISP,*),IPARTSP(*),ISUBMOD(*),ISKN(LISKN,*),
     .        IXX(*),KXX(*),IPARTX(*),IADBOXMAX,NSETS
      INTEGER FLAG,MAXNNOD
      MY_REAL
     .        X(3,*),GEO(NPROPG,*),SKEW(LSKEW,*),RTRANS(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS)   :: SUBSET
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (SURF_)   , TARGET, DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , TARGET, DIMENSION(NSLIN)   :: IGRSLIN
      TYPE (BOX_)    , DIMENSION(NBBOX) :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J10(10),BUFTMP(NUMNOD*2+NPART),ID_SUB
      INTEGER I,J,K,II,KK,N1,N2,ISU,ID,JREC,NNOD,NL,NTRI,IGS,IGRS,
     .        OK,IT0,IT1,IT2,IT3,IT4,IT5,IT6,
     .        FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,STAT,IT7,UID,IFLAGUNIT,
     .        IT8,SUB_ID,IADBOX,NN,LIST_IGR(NGRNOD),IDMIN,IDMAX,OFFSET,
     .        IT9,IDB,NENTITY,NLINES,JJ
      MY_REAL
     .        XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,BID,FAC_L
      INTEGER, DIMENSION(:),ALLOCATABLE :: BUFTMP2
      CHARACTER TITR*nchartitle,KEY*ncharkey,MESS*40,KEY2*ncharkey
      INTEGER, DIMENSION(:),ALLOCATABLE :: BUFBOX
      CHARACTER*nchartitle,
     .   TITR1
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,ULIST2S,LISTCNT
C                1234567890123456789012345678901234567890
      DATA MESS/'NODE GROUP DEFINITION                   '/
C-----------------------------------------------
C    IGRNOD(IGS)%ID : GROUP identifier
C    IGRNOD(IGS)%TITLE : GROUP title
C    IGRNOD(IGS)%NENTITY : Entities (nodes) number of the GROUP
C    IGRNOD(IGS)%GRTYPE : TYPE ( 0-NOEUDS, 1-BRIC, 2-QUAD,   3-SHELL_4N,
!                                4-TRUSS,  5-BEAM, 6-SPRINGS,7-SHELL_3N )
!!                   GRTYPE --- > OBSOLETE
C    IGRNOD(IGS)%SORTED  : FLAG for sorted/unsorted nodes
!                    = 0 -> sorted
!                    = 1 -> unsorted
C    IGRNOD(IGS)%GRPGRP : TYPE of nodal GROUP
!                                 = 1 FOR /GRNOD/NOD
!                                 = 2 FOR /GRNOD/GNROD
!                                 = 0 ALL THE REST
C    IGRNOD(IGS)%LEVEL  : Hierarchy level
!                             (FLAG 'SUBLEVEL DONE' FOR GROUPS OF GROUPS)
!                            = 0 ---> not yet initialized
!                            = 1 ---> done
!  - R2R -
!                              ! R2R_ALL <--- IGROUP2(8,I) = IGROUP2(2,I) -- before splitting
!                              ! R2R_SHARE <--- IGROUP2(9,I)   (shared on boundary subdomain)
C    IGRNOD(IGS)%R2R_ALL   ! Multidomaines -> number of node(elems, parts) before split
C    IGRNOD(IGS)%R2R_SHARE ! shared on boundary subdomain
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
      IT0=0
      IT1=0
      IT2=0
      IT3=0
      IT4=0
      IT5=0
      IT6=0
      IT7=0
      IT8=0
      IT9=0
C=======================================================================
C Start reading groups of nodes (type NODE) + init IGRN(1,IGS)
C=======================================================================
      IGS=0
      TITR1='NODE GROUP'
C
      CALL HM_OPTION_START('/GRNOD')
C      
      ! Loop over all GRNOD
      DO I=1,NGRNOD
C
        ! Read keys and uid
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)   
C      
        ! Increasing the counter
        IGS=IGS+1
C
        ! Initialization of data structure
        IF (FLAG == 0) THEN
          IGRNOD(IGS)%ID = 0
          IGRNOD(IGS)%NENTITY = 0
          IGRNOD(IGS)%GRTYPE = 0
          IGRNOD(IGS)%SORTED = 0
          IGRNOD(IGS)%GRPGRP = 0
          IGRNOD(IGS)%LEVEL = 0
          IGRNOD(IGS)%R2R_ALL = 0
          IGRNOD(IGS)%R2R_SHARE = 0
        ENDIF
        NN = 0
        NNOD = 0
        IGRNOD(IGS)%ID=ID
        IGRNOD(IGS)%GRTYPE=0
        IGRNOD(IGS)%SORTED=0
        IGRNOD(IGS)%LEVEL=1
        IGRNOD(IGS)%TITLE = TITR
C
        ! Filling data structure following type of GRNOD
        !-----------------------------------------------------------------
        ! GRNOD from GRNODNS
        IF(KEY(1:7) == 'GRNODNS')THEN
C          tag des groupes de groupes
           IGRNOD(IGS)%NENTITY=-1
c          non sorted nodes
             IGRNOD(IGS)%SORTED=1
             IGRNOD(IGS)%GRPGRP=2 !!! /GROUP/GROUP
             IGRNOD(IGS)%LEVEL=0
        !-----------------------------------------------------------------
        ! GRNOD from GRNOD
        ELSEIF(KEY(1:5) == 'GRNOD')THEN
          IGRNOD(IGS)%NENTITY=-1
          IGRNOD(IGS)%GRPGRP=2
          IGRNOD(IGS)%LEVEL=0
          IT0=IT0+1
        !-----------------------------------------------------------------
        ! GRNOD from NODENS
        ELSEIF(KEY(1:6) == 'NODENS') THEN
          IT1=IT1+1
          IF (FLAG == 0) THEN
            CALL HM_GET_INTV('idsmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)
            DO KK = 1,NENTITY
              CALL HM_GET_INT_ARRAY_INDEX ('ids',JJ  ,KK,IS_AVAILABLE,LSUBMODEL)
              IF (JJ /= 0) THEN
                NNOD = NNOD + 1
              ENDIF
            ENDDO
            IGRNOD(IGS)%NENTITY=NNOD
! may be allocated already (prelecgrns.F) if NUMELX > 0
            IF (.NOT. ALLOCATED(IGRNOD(IGS)%ENTITY)) 
     .                MY_ALLOCATE(IGRNOD(IGS)%ENTITY,NNOD)
            IGRNOD(IGS)%GRPGRP=1
            MAXNNOD = MAX(NNOD,MAXNNOD)
          ELSE 
            IGRNOD(IGS)%SORTED=1
            CALL HM_GET_INTV('idsmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)
            DO KK = 1,NENTITY
              CALL HM_GET_INT_ARRAY_INDEX ('ids',JJ  ,KK,IS_AVAILABLE,LSUBMODEL)
              IF (JJ /= 0) THEN
                NN = NN+1
                IGRNOD(IGS)%ENTITY(NN) = USR2SYS(JJ,ITABM1,MESS,ID)
              ENDIF
            ENDDO          
          ENDIF
        !-----------------------------------------------------------------
        ! GRNOD from NODE
        ELSEIF(KEY(1:4) == 'NODE' .OR. KEY(1:5) == 'CNODE') THEN
C         groupe de noeuds
          IT1=IT1+1
          IF (FLAG == 0) THEN
            CALL HM_GET_INTV('idsmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)
           IF(IS_AVAILABLE)NNOD = NNOD + NENTITY  
            IGRNOD(IGS)%NENTITY=NNOD
! may be allocated already (prelecgrns.F) if NUMELX > 0
            IF( .NOT. ALLOCATED(IGRNOD(IGS)%ENTITY)) 
     .                MY_ALLOCATE(IGRNOD(IGS)%ENTITY,NNOD)
            IGRNOD(IGS)%GRPGRP=1
            MAXNNOD = MAX(NNOD,MAXNNOD)
          ELSE
c           sorted nodes
            CALL HM_GET_INTV('idsmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)
            DO KK = 1,NENTITY
              CALL HM_GET_INT_ARRAY_INDEX ('ids',JJ  ,KK,IS_AVAILABLE,LSUBMODEL)
              IF (JJ /= 0) THEN
                NN = NN+1
                IGRNOD(IGS)%ENTITY(NN) = JJ
              ENDIF
            ENDDO        
          ENDIF
        !-----------------------------------------------------------------
        ! GRNOD from PART, SUBSET, MAT and PROP          
        ELSEIF(KEY(1:4) == 'PART'.OR.KEY(1:6) == 'SUBSET'.OR.
     .         KEY(1:3) == 'MAT' .OR.KEY(1:4) == 'PROP') THEN
          IT2=IT2+1
          IF (FLAG == 0) IGRNOD(IGS)%NENTITY=0
          IGRNOD(IGS)%GRPGRP=0
        !-----------------------------------------------------------------
        ! GRNOD from /BOX/RECTA, /BOX/CYLIN, /BOX/SPHERE, GENE, BOXA
        ELSEIF((KEY(1:3) == 'BOX' .AND. NBBOX == 0 .AND.
     .         (KEY2(1:5) /= 'RECTA'.AND.
     .          KEY2(1:5) /= 'CYLIN'.AND.KEY2(1:5) /= 'SPHER')).OR.
     .          KEY(1:4) == 'GENE'.OR.KEY(1:4) == 'BOXA')THEN
          IT3=IT3+1
          IF (FLAG == 0) IGRNOD(IGS)%NENTITY=0
          IGRNOD(IGS)%GRPGRP=0
        !-----------------------------------------------------------------
        ! GRNOD from GR, SURF, LINE
        ELSEIF(KEY(1:2) == 'GR'.OR.KEY(1:4) == 'SURF'.OR.KEY(1:4) == 'LINE')THEN
          IT4=IT4+1
          IF (FLAG == 0) IGRNOD(IGS)%NENTITY=0
          IGRNOD(IGS)%GRPGRP=0
        !-----------------------------------------------------------------
        ! GRNOD from SUBMODEL
        ELSEIF(KEY(1:6) == 'SUBMOD')THEN
          IT5=IT5+1
          IF (FLAG == 0) IGRNOD(IGS)%NENTITY=0
          IGRNOD(IGS)%GRPGRP=0
        !-----------------------------------------------------------------
        ! GRNOD from /BOX
        ELSEIF(KEY(1:3) == 'BOX'.AND.(KEY2(1:5) == 'RECTA'.OR.
     .         KEY2(1:5) == 'CYLIN'.OR.KEY2(1:5) == 'SPHER'))THEN
C             old /grnod/box (not /BOX/BOX)
C             groupe de noeuds dans un box (classical box, parallelepiped,
C                         cylindrical, spherical)
          IT7=IT7+1
          IF (FLAG == 0) IGRNOD(IGS)%NENTITY=0
          IGRNOD(IGS)%GRPGRP=0
        !-----------------------------------------------------------------
        ! GRNOD from BOX            
        ELSEIF(KEY(1:3) == 'BOX' .AND. NBBOX > 0)THEN
C         multi box (box de box)
          IT8=IT8+1
        !-----------------------------------------------------------------
        ! GRNOD from GEN_INCR
        ELSEIF(KEY(1:8) == 'GEN_INCR')THEN
          IT9=IT9+1
          IF (FLAG == 0) IGRNOD(IGS)%NENTITY=0
          IGRNOD(IGS)%GRPGRP=0
        ENDIF
C---
      ENDDO
C-------------------------------------
C Looking for double IDs
C-------------------------------------
      IF (FLAG == 0) THEN
        DO IGS = 1,NGRNOD
          LIST_IGR(IGS) = IGRNOD(IGS)%ID
        ENDDO
        CALL UDOUBLE_IGR(LIST_IGR,NGRNOD,MESS,0,BID)
      ENDIF
C=======================================================================
C Remplacement des n0 de noeuds user par systeme (type NODE)
C=======================================================================
      IF (IT1 /= 0 .AND. FLAG == 1)THEN
!
        ALLOCATE(BUFTMP2(MAXNNOD*2),STAT=stat)
        IF (STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='BUFTMP2')
        ENDIF
!
        DO I=1,NGRNOD
          IF (IGRNOD(I)%GRPGRP == 1) THEN
            ID=IGRNOD(I)%ID
            NNOD=IGRNOD(I)%NENTITY
            NTRI=IGRNOD(I)%SORTED
            IF (NNOD > 0 .AND. NTRI == 0)THEN
              IF(NNOD == 1) THEN
C pas de tri necessaire et dichotomie
                NN = IGRNOD(I)%ENTITY(NNOD)
                IGRNOD(I)%ENTITY(NNOD)=USR2SYS(NN,ITABM1,MESS,ID)
                IGRNOD(I)%NENTITY=1
C            ELSEIF(NNOD < 10 ou 100 ...)
C algo avec tri simplifie et dichotomie, nnod limite a definir
              ELSE
C algo optimise pour groupe de taille non negligeable devant numnod
                BUFTMP2(1:2*NNOD) = 0
                NNOD=ULIST2S(IGRNOD(I)%ENTITY,NNOD,ITABM1,MESS,BUFTMP2,ID)
                IGRNOD(I)%NENTITY=NNOD
              END IF
            ENDIF
          ENDIF
        ENDDO
        DEALLOCATE (BUFTMP2)
      ENDIF ! IF (IT1 /= 0 .AND. FLAG == 1)THEN
C=======================================================================
C BOX, GENERATION
C=======================================================================
      IGS=0
      IF (IT3 /= 0) THEN
        CALL HM_OPTION_START('/GRNOD')
        DO I=1,NGRNOD
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR  ,
     .                            UNIT_ID     = UID,
     .                            KEYWORD2    = KEY   ,
     .                            KEYWORD3    = KEY2)        
C        
          NNOD = 0
          NN   = 0
          IGS  = IGS+1
C-------------------------
C BOX (old)
C-------------------------
          IF((KEY(1:3) == 'BOX'.AND.(KEY2(1:5) /= 'RECTA'.AND.
     .       KEY2(1:5) /= 'CYLIN'.AND.KEY2(1:5) /= 'SPHER').AND.
     .       NBBOX == 0) .OR. (KEY(1:4) == 'BOXA'))THEN
              ! No longer supported, ERROR MESSAGE
C-------------------------
C GENERATION
C-------------------------
          ELSEIF (KEY(1:4) == 'GENE') THEN
            BUFTMP = 0
C              
            CALL HM_GET_INTV('grnodGenArrCnt' ,NENTITY,IS_AVAILABLE,LSUBMODEL)  
            DO KK = 1,NENTITY
              CALL HM_GET_INT_ARRAY_INDEX('Ifirst',N1 ,KK  ,IS_AVAILABLE,LSUBMODEL)    
              CALL HM_GET_INT_ARRAY_INDEX('Ilast' ,N2 ,KK  ,IS_AVAILABLE,LSUBMODEL)  
              IF (N2 >= N1) THEN
                DO K=1,NUMNOD
                  IF (ITAB(K) >= N1 .AND. ITAB(K) <= N2) BUFTMP(K)=1
                ENDDO
              ENDIF
            ENDDO
C---
            NNOD=0
            IF (FLAG == 0) THEN
              DO J=1,NUMNOD
                IF (BUFTMP(J) == 1) NNOD = NNOD+1
              ENDDO
              IGRNOD(IGS)%NENTITY=NNOD
              MY_ALLOCATE(IGRNOD(IGS)%ENTITY,NNOD)
              IGRNOD(IGS)%ENTITY=0
            ELSE
              DO J=1,NUMNOD
                IF (BUFTMP(J) == 1)THEN
                  NN = NN + 1
                  IGRNOD(IGS)%ENTITY(NN)=J
                ENDIF
              ENDDO
            ENDIF
C---
          ENDIF
        ENDDO
      ENDIF
C-------------------------
C BOX (parallelepiped (oriented), cylindrical, spherical) - old one (10SA1)
C-------------------------
      IGS=0
      IF(IT7 /= 0)THEN
        ! Error message, obsolete
      ENDIF
C-------------------------
C NEW BOX OPTION (MULTI BOX COMBINATION)
C-------------------------
      IGS=0
      IF(IT8 /= 0)THEN
C
        IF (FLAG == 0) THEN
          ALLOCATE(BUFBOX(1))
          BUFBOX = 0
        ELSEIF (FLAG == 1) THEN
          ALLOCATE(BUFBOX(IADBOXMAX))
          BUFBOX(1:IADBOXMAX) = 0
        ENDIF
C
        CALL HM_OPTION_START('/GRNOD')
        DO I = 1,NGRNOD
c
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR  ,
     .                            UNIT_ID     = UID,
     .                            KEYWORD2    = KEY   ,
     .                            KEYWORD3    = KEY2)          
c        
          NN   = 0
          NNOD = 0
          IGS  = IGS+1
c          
          IF (KEY(1:3) == 'BOX' .AND. NBBOX > 0) THEN
            IADBOX = 1
            IFLAGUNIT = 0
            DO J=1,NUNITS
              IF (UNITAB%UNIT_ID(J) == UID) THEN
                FAC_L = UNITAB%FAC_L(J)
                IFLAGUNIT = 1
                EXIT
              ENDIF
            ENDDO
            IF (UID/=0.AND.IFLAGUNIT==0) THEN
              CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I2=UID,I1=ID,C1='NODE GROUP',
     .                    C2='NODE GROUP',
     .                    C3=TITR)
            ENDIF
C---
            CALL HM_GET_INT_ARRAY_INDEX('ids' ,IDB ,1,IS_AVAILABLE,LSUBMODEL)
            CALL HM_BIGBOX(X   ,FLAG,NNOD  ,
     .                  SKEW,IGS   ,ISKN  ,ITABM1,IBOX   ,
     .                  ID  ,BUFBOX,IADBOX,TITR,KEY,NN,
     .                  IADBOXMAX,IGRNOD,IDB)
C---
            IADBOXMAX = MAX(IADBOX,IADBOXMAX)
            IF (FLAG == 0) THEN
              IGRNOD(IGS)%NENTITY=NNOD
              MY_ALLOCATE(IGRNOD(IGS)%ENTITY,NNOD)
              IGRNOD(IGS)%ENTITY = 0
            ELSEIF (FLAG == 1) THEN
              IGRNOD(IGS)%NENTITY=NNOD
            ENDIF ! IF (FLAG == 0)
          ENDIF
        ENDDO
C---
        IF(ALLOCATED(BUFBOX))DEALLOCATE(BUFBOX)
      ENDIF ! IF(IT8 /= 0)
C=======================================================================
C groupes de SUBSETS,PART,MAT,PROP
C=======================================================================
      IGS=0
      IF (IT2 /= 0)THEN
        CALL HM_OPTION_START('/GRNOD')
        DO I = 1,NGRNOD
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR    ,
     .                            UNIT_ID     = UID     ,
     .                            KEYWORD2    = KEY     ,
     .                            KEYWORD3    = KEY2)        
C             
          IGS=IGS+1
          NN = 0
          IF (KEY(1:4) == 'PART'.OR.KEY(1:6) == 'SUBSET'.OR.KEY(1:3) == 'MAT' .OR.KEY(1:4) == 'PROP') THEN
C             tag les PARTs
            BUFTMP = 0
            CALL HM_TAGPART(BUFTMP ,IPART ,KEY  ,IGRNOD(IGS)%ID,TITR  ,TITR1 ,FLAG  ,SUBSET, LSUBMODEL)
C-------------------------
C             tag les noeuds
            CALL TAGNODS(IXS,IXS10,IXS20,IXS16,IPARTS,BUFTMP,IGRNOD(IGS)%ID,TITR)
            CALL TAGNOD(IXQ,NIXQ,2,5,NUMELQ,IPARTQ,BUFTMP,NPART)
            CALL TAGNOD(IXC,NIXC,2,5,NUMELC,IPARTC,BUFTMP,NPART)
            CALL TAGNOD(IXTG,NIXTG,2,4,NUMELTG,IPARTG,BUFTMP,NPART)
            CALL TAGNOD(IXT,NIXT,2,3,NUMELT,IPARTT,BUFTMP,NPART)
            CALL TAGNOD(IXP,NIXP,2,3,NUMELP,IPARTP,BUFTMP,NPART)
            CALL TAGNODR(IXR,GEO,NUMELR,IPARTR,BUFTMP,NPART)
            CALL TAGNOD(KXSP,NISP,3,3,NUMSPH,IPARTSP,BUFTMP,NPART)
            CALL TAGNODX(IXX,KXX,NUMELX,IPARTX,BUFTMP,NPART)
C---
            NNOD=0
            IF (FLAG == 0) THEN
              DO J=1,NUMNOD
                IF (BUFTMP(J+NPART) /= 0) NNOD=NNOD+1
              ENDDO
              IGRNOD(IGS)%NENTITY=NNOD
              MY_ALLOCATE(IGRNOD(IGS)%ENTITY,NNOD)
              IGRNOD(IGS)%ENTITY=0
            ELSE
              DO J=1,NUMNOD
                IF (BUFTMP(J+NPART) /= 0)THEN
                  NN = NN + 1
                  IGRNOD(IGS)%ENTITY(NN) = J
                ENDIF
              ENDDO
            ENDIF
C---
          ENDIF
        ENDDO
      ENDIF
C=======================================================================
C groupes de SUBMODELS
C=======================================================================
      IGS=0
      IF (IT5 > 0)THEN
        CALL HM_OPTION_START('/GRNOD')
        DO I = 1,NGRNOD
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR    ,
     .                            UNIT_ID     = UID     ,
     .                            KEYWORD2    = KEY     ,
     .                            KEYWORD3    = KEY2)        
c        
          NNOD = 0
          IGS  = IGS+1
          NN   = 0
          IF(KEY(1:6) == 'SUBMOD')THEN
            CALL HM_SUBMODGRN(ITAB    ,ITABM1     ,ISUBMOD    ,ID     ,
     .                        NNOD    ,MESS       ,FLAG       ,TITR   ,
     .                        TITR1   ,LSUBMODEL  ,IGRNOD(IGS),NN     )
            IF (FLAG == 0) THEN
              IGRNOD(IGS)%NENTITY=NNOD
              MY_ALLOCATE(IGRNOD(IGS)%ENTITY,NNOD)
              IGRNOD(IGS)%ENTITY = 0
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C=======================================================================
C groupes de groupes d'elements + surfaces
C=======================================================================
      IGS=0
      IF (IT4 /= 0)THEN
        CALL HM_OPTION_START('/GRNOD')
        DO I=1,NGRNOD
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR    ,
     .                            UNIT_ID     = UID     ,
     .                            KEYWORD2    = KEY     ,
     .                            KEYWORD3    = KEY2)   
c
          IGS=IGS+1
          NN = 0
C---
          IF(KEY(1:5) == 'GRNOD')THEN
            CYCLE
          ELSEIF(KEY(1:2) == 'GR' .OR. KEY(1:4) == 'SURF' .OR. KEY(1:4) == 'LINE')THEN
            BUFTMP = 0
            ID = IGRNOD(IGS)%ID
            IF(KEY(1:6) == 'GRBRIC')THEN
              CALL HM_ELNGRS(IXS,IXS10,IXS20,IXS16,NGRBRIC,KEY(1:6),
     .                       ID ,IGRBRIC,BUFTMP,TITR,
     .                       FLAG,LSUBMODEL)
            ELSEIF(KEY(1:6) == 'GRQUAD')THEN
              CALL HM_ELNGR(IXQ,NIXQ,2,5,NGRQUAD,KEY(1:6),
     .                      ID,IGRQUAD,BUFTMP,TITR,
     .                      FLAG,LSUBMODEL)
            ELSEIF(KEY(1:6) == 'GRSHEL')THEN
              CALL HM_ELNGR(IXC,NIXC,2,5,NGRSHEL,KEY(1:6),
     .                      ID,IGRSH4N,BUFTMP,TITR,
     .                      FLAG,LSUBMODEL)
            ELSEIF(KEY(1:6) == 'GRTRUS')THEN
              CALL HM_ELNGR(IXT,NIXT,2,3,NGRTRUS,KEY(1:6),
     .                      ID,IGRTRUSS,BUFTMP,TITR,
     .                      FLAG,LSUBMODEL)
            ELSEIF(KEY(1:6) == 'GRBEAM')THEN
              CALL HM_ELNGR(IXP,NIXP,2,3,NGRBEAM,KEY(1:6),
     .                      ID,IGRBEAM,BUFTMP,TITR,
     .                      FLAG,LSUBMODEL)
            ELSEIF(KEY(1:6) == 'GRSPRI')THEN
              CALL HM_ELNGRR(IXR,GEO,NGRSPRI,ID,
     .                       IGRSPRING,BUFTMP,TITR,
     .                       FLAG,LSUBMODEL)
            ELSEIF(KEY(1:6) == 'GRSH3N' .OR. KEY(1:6) == 'GRTRIA')THEN
              CALL HM_ELNGR(IXTG,NIXTG,2,4,NGRSH3N,KEY(1:6),
     .                      ID,IGRSH3N,BUFTMP,TITR,
     .                      FLAG,LSUBMODEL)
            ELSEIF(KEY(1:4) == 'SURF')THEN
              CALL HM_SURFNOD(ID,IGRSURF,BUFTMP,TITR,NSETS,LSUBMODEL)
            ELSEIF(KEY(1:4) == 'LINE')THEN
              CALL HM_LINENGR(ID,IGRSLIN,BUFTMP,TITR,NSETS,LSUBMODEL)
            ENDIF
C---
            NNOD=0
            IF (FLAG == 0) THEN
              DO J=1,NUMNOD
                IF (BUFTMP(J) /= 0) NNOD=NNOD+1
              ENDDO
              IGRNOD(IGS)%NENTITY=NNOD
              MY_ALLOCATE(IGRNOD(IGS)%ENTITY,NNOD)
              IGRNOD(IGS)%ENTITY = 0
            ELSE
              DO J=1,NUMNOD
                IF (BUFTMP(J) /= 0)THEN
                  NN = NN + 1
                  IGRNOD(IGS)%ENTITY(NN)=J
                ENDIF
              ENDDO
            ENDIF
C---
          ENDIF
        ENDDO
      ENDIF
C=======================================================================
C BOX, GENERATION
C=======================================================================
      IGS=0
      IF(IT9 /= 0)THEN
        CALL HM_OPTION_START('/GRNOD')
        DO I=1,NGRNOD
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR    ,
     .                            UNIT_ID     = UID     ,
     .                            KEYWORD2    = KEY     ,
     .                            KEYWORD3    = KEY2    )   
c
          NNOD=0
          NN = 0
          IGS=IGS+1
C-------------------------
C GENERATION MIN MAX OFFSET
C-------------------------
          IF (KEY(1:8) == 'GEN_INCR') THEN
            BUFTMP = 0
            CALL HM_GET_INTV  ('grnodGenArrCnt' ,NLINES,IS_AVAILABLE,LSUBMODEL)
            DO KK=1,NLINES              
              CALL HM_GET_INT_ARRAY_INDEX('Ifirst',IDMIN  ,KK,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('Ilast' ,IDMAX  ,KK,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('Iincr' ,OFFSET ,KK,IS_AVAILABLE,LSUBMODEL)               
              DO J=IDMIN, IDMAX , OFFSET
                IF (J > 0) THEN
                  DO K=1,NUMNOD
                    ID = ITAB(K)
                    IF(ID<IDMIN .OR. ID>IDMAX) CYCLE
                    IF(MOD(ID-IDMIN,OFFSET)==0) BUFTMP(K) = 1
                  ENDDO
                ENDIF
              ENDDO            
            ENDDO
C---
            NNOD=0
            IF (FLAG == 0) THEN
              DO J=1,NUMNOD
                IF (BUFTMP(J) == 1) NNOD=NNOD+1
              ENDDO
              IGRNOD(IGS)%NENTITY=NNOD
              MY_ALLOCATE(IGRNOD(IGS)%ENTITY,NNOD)
              IGRNOD(IGS)%ENTITY=0
            ELSE
              DO J=1,NUMNOD
                IF (BUFTMP(J) == 1)THEN
                  NN = NN + 1
                  IGRNOD(IGS)%ENTITY(NN)=J
                ENDIF
              ENDDO
            ENDIF
C---
          ENDIF
        ENDDO
      ENDIF
C-----
      RETURN
      END
