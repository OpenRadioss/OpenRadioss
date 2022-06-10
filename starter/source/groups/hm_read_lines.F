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
Chd|  HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DEALLOCATE_SURF_ELM           source/groups/init_surf_elm.F 
Chd|        HM_BIGSBOX                    source/groups/hm_bigsbox.F    
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SUBMODPART                 source/groups/hm_submodpart.F 
Chd|        HM_SURFGR2                    source/groups/hm_surfgr2.F    
Chd|        HM_TAGPART2                   source/groups/hm_tagpart2.F   
Chd|        INIT_SURF_ELM                 source/groups/init_surf_elm.F 
Chd|        LINEDGE                       source/groups/linedge.F       
Chd|        SURFTAG                       source/groups/surftag.F       
Chd|        SURFTAGE                      source/groups/surftage.F      
Chd|        SURFTAGX                      source/groups/surftag.F       
Chd|        UDOUBLE_IGR                   source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MAPPING_OPTION_MOD            share/modules1/dichotomy_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        SURF_MOD                      share/modules1/surf_mod.F     
Chd|====================================================================
      SUBROUTINE HM_READ_LINES(
     1           ITAB   ,ITABM1 ,
     2           ISUBMOD,IGRSLIN,IGRSURF,X      ,IXS    ,
     3           IXQ    ,IXC    ,IXT    ,IXP    ,IXR    ,
     4           IXTG   ,IPART  ,IPARTS ,IPARTQ ,IPARTC ,
     5           IPARTT ,IPARTP ,IPARTR ,IPARTTG,
     6           NSEGS  ,        FLAG   ,SKEW   ,ISKN   ,
     7           UNITAB ,IBOX   ,RTRANS ,LSUBMODEL,
     8           IPARTX ,KXX    ,IXX    ,IADBOXMAX,SUBSET,
     9           IGRTRUSS,IGRBEAM,IGRSPRING,NSETS,MAP_TABLES)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE OPTIONDEF_MOD
      USE SURF_MOD
      USE HM_OPTION_READ_MOD
      USE MAPPING_OPTION_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "scr23_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "sysunit.inc"
#include      "my_allocate.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ITABM1(*),
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXT(NIXT,*),
     .        IXP(NIXP,*),IXR(NIXR,*),IXTG(NIXTG,*),IPARTS(*),
     .        IPARTQ(*),IPARTC(*),IPARTT(*),IPARTP(*),IPARTR(*),
     .        IPARTTG(*),IPART(LIPART1,*),
     .        ITAB(*),ISUBMOD(*),FLAG,NSEGS,ISKN(LISKN,*),
     .        IPARTX(*),KXX(NIXX,*),
     .        IXX(*),IADBOXMAX,NSETS
      my_real
     .        X(3,*),SKEW(LSKEW,*),RTRANS(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      TYPE(MAPPING_STRUCT_), INTENT(IN) :: MAP_TABLES
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS)   :: SUBSET
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (SURF_)   , DIMENSION(NSURF+NSETS)   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN+NSETS)   :: IGRSLIN
      TYPE (BOX_)    , DIMENSION(NBBOX)   :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,II,K,L,J,JC,JJ,KK,ISU,ID,NSEG,NSEG0,NSEGV,N1,N2,NUMEL,
     .        OK,IGS,IGRS,JREC,IAD0,NE,ITYP,
     .        IT0,IT1,IT2,IT3,IT4,IT5,IT6,IT7,SBUFBOX,UID,IFLAGUNIT,
     .        ISK,BOXTYPE,J2(2),IT8,SUB_ID,IBUFSIZ,NINDX,STAT,
     .        INTMAX,IADBOX,LIST_LINE(NSLIN),ISEG,IBID,NSEG_TOT,NN,LINE_NSEG0,SUB_INDEX
      my_real
     .        XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,BID,FAC_L,
     .        DIAM,XP1,YP1,ZP1,XP2,YP2,ZP2
      CHARACTER TITR*nchartitle,KEY*ncharkey,MESS*40,KEY2*ncharkey
      INTEGER, DIMENSION(:),ALLOCATABLE :: BUFBOX, BUFTMP, INDX,TAGT ,TAGP
      CHARACTER*nchartitle,
     .   TITR1,STRING
      LOGICAL :: FLAG_GRBRIC
      INTEGER :: MODE, NENTITY
      TYPE(PART_TYPE), DIMENSION(:), ALLOCATABLE :: SURF_ELM
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED, lERROR
C-----------------------------------------------
C   D e s c r i p t i o n 
C-----------------------------------------------
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       
!       FLAG_GRBRIC : flag to initialize the INDX_SOL(10) arraies
!                     and optimize an old and expensive treatment in SSURFTAG 
!                     NOT USED IN THIS ROUTINE !!!!!!!!!!!!     
!       MODE : integer
!              switch to initialize solid/shell/shell3n or truss/beam/spring
!       SURF_ELM : PART_TYPE structur 
!                  %Nxxx : number of element per part
!                  %xxx_PART : ID of the element 
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
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
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C                1234567890123456789012345678901234567890
      DATA MESS/'LINE DEFINITION                        '/
      DATA INTMAX /2147483647/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IT0=0
      IT1=0
      IT2=0
      IT3=0
      IT4=0
      IT5=0
      IT6=0
      IT7=0
      IT8=0
      OK =0
      FLAG_GRBRIC = .FALSE.
      IBUFSIZ=6*NSEGS
      ALLOCATE(BUFTMP(IBUFSIZ),INDX(IBUFSIZ),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='BUFTMP')
      NINDX=0
      BUFTMP(1:IBUFSIZ)=0    ! init to 0 only one time
C-----------------------------------------------
      CALL HM_OPTION_START('/LINE')
      TITR1='LINE'
      DO IGS=1,NSLIN
        lERROR=.FALSE.
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)
        ISEG = 0                                                           
        IF (FLAG == 0) THEN                                                
          IGRSLIN(IGS)%ID = 0                                              
          IGRSLIN(IGS)%NSEG = 0                                            
          IGRSLIN(IGS)%TYPE = 0                                            
          IGRSLIN(IGS)%LEVEL = 0                                           
          IGRSLIN(IGS)%NSEG_R2R_ALL = 0                                    
          IGRSLIN(IGS)%NSEG_R2R_SHARE = 0                                  
        ENDIF                                                              
        IGRSLIN(IGS)%ID = ID                                               
        IGRSLIN(IGS)%LEVEL = 1                                             
        IGRSLIN(IGS)%TITLE = TITR                                          
        IF(KEY(1:4) == 'LINE') THEN !tag des lines of lines                
          IGRSLIN(IGS)%NSEG = -1                                           
          IGRSLIN(IGS)%LEVEL = 0                                           
          IT0=IT0+1                                                        
        ELSEIF(KEY(1:3) == 'SEG') THEN                                     
          IT1=IT1+1                                                        
          IF (FLAG == 0) THEN                                              
            CALL HM_GET_INTV('segmax',NSEG,IS_AVAILABLE,LSUBMODEL)                                             
            IGRSLIN(IGS)%NSEG = NSEG                                       
            MY_ALLOCATE2D(IGRSLIN(IGS)%NODES,NSEG,2)                       
            IGRSLIN(IGS)%NODES(1:NSEG,1:2) = 0                             
            MY_ALLOCATE(IGRSLIN(IGS)%ELTYP,NSEG)                           
            IGRSLIN(IGS)%ELTYP(1:NSEG) = 0                                 
            MY_ALLOCATE(IGRSLIN(IGS)%ELEM,NSEG)                            
            IGRSLIN(IGS)%ELEM(1:NSEG) = 0                                  
            MY_ALLOCATE(IGRSLIN(IGS)%PROC,NSEG)                            
            IGRSLIN(IGS)%PROC(1:NSEG) = 0                                  
          ENDIF  
          IF(FLAG == 1) THEN                    
            CALL HM_GET_INTV  ('segmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)                 
            DO KK=1,NENTITY                                                              
              CALL HM_GET_INT_ARRAY_INDEX ('N1' ,N1 ,KK,IS_AVAILABLE,LSUBMODEL)       
              CALL HM_GET_INT_ARRAY_INDEX ('N2' ,N2 ,KK,IS_AVAILABLE,LSUBMODEL)       
              IGRSLIN(IGS)%NODES(KK,1) = USR2SYS(N1,ITABM1,MESS,ID)      
              IGRSLIN(IGS)%NODES(KK,2) = USR2SYS(N2,ITABM1,MESS,ID)      
              IGRSLIN(IGS)%ELTYP(KK) = 0                                     
              IGRSLIN(IGS)%ELEM(KK)  = 0 
            ENDDO   
          ENDIF                                                                                   
        ELSEIF(KEY(1:4) == 'PART'.OR.KEY(1:6) == 'SUBSET'.OR. KEY(1:3) == 'MAT' .OR.KEY(1:4) == 'PROP') THEN              
C         line de SUBSET PART MAT OU PROP                                  
          IT2=IT2+1                                                        
          IF (FLAG == 0) IGRSLIN(IGS)%NSEG = 0                             
        ELSEIF(KEY(1:3) == 'BOX'.AND.NBBOX == 0 .AND.(KEY2(1:5) /= 'RECTA'.AND.KEY2(1:5) /= 'CYLIN'.AND.KEY2(1:5) /= 'SPHER'))THEN             
C         line dans un box (old)                                           
          lERROR=.TRUE.                           
        ELSEIF(KEY(1:2) == 'GR'.OR.KEY(1:4) == 'WIRE') THEN                
C         line d'ele 2N dans un group d'elements                           
          IT4=IT4+1                                                        
          IF (FLAG == 0) IGRSLIN(IGS)%NSEG = 0                             
        ELSEIF(KEY(1:4) == 'SURF'.OR.KEY(1:4) == 'EDGE') THEN              
C         line d'ele 2N dans une surface ou son bord                       
          IT5=IT5+1                                                        
          IF (FLAG == 0) IGRSLIN(IGS)%NSEG = 0                             
        ELSEIF(KEY(1:6) == 'SUBMOD') THEN                                  
C         line de submodel                                                 
          IT6=IT6+1                                                        
          IF (FLAG == 0) IGRSLIN(IGS)%NSEG = 0                             
        ELSEIF(KEY(1:3) == 'BOX'.AND.(KEY2(1:5) == 'RECTA'.OR. KEY2(1:5) == 'CYLIN'.OR.KEY2(1:5) == 'SPHER'))THEN               
          !old /grnod/box (not /BOX/BOX)                                    
          !line dans un box (classical box, parallelepiped (oriented),      
          !          cylindrical, spherical)                                
          lERROR=.TRUE.                             
        ELSEIF(KEY(1:3) == 'BOX' .AND. NBBOX > 0)THEN                      
          !multi box (box de box)                                           
          IT8=IT8+1 
        ELSE
          lERROR=.TRUE.                                                                         
        ENDIF                                                              
        
        IF(lERROR)THEN
          !INVALID KEYWORD
          STRING=' '
          STRING = "/LINE/"//KEY(1:LEN_TRIM(KEY)-1)
          IF(LEN_TRIM(KEY2)>1)STRING = STRING//KEY2(1:LEN_TRIM(KEY2)-1)
          CALL ANCMSG(MSGID=688,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=ID, C1=TITR, C2=STRING)         
        ENDIF
                    
      ENDDO

C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      IF (FLAG == 0) THEN
        DO IGS=1,NSLIN
          LIST_LINE(IGS) = IGRSLIN(IGS)%ID
        ENDDO
        CALL UDOUBLE_IGR(LIST_LINE,NSLIN,MESS,0,BID)
      ENDIF
C=======================================================================
C BOX (old)
C=======================================================================
      IF(IT3 /= 0) THEN
        !NO LONGER SUPPORTED
      ENDIF
C=======================================================================
C BOX (parallelepiped, cylindrical, spherical) - old one (10SA1)
C=======================================================================
      IF(IT7 /= 0) THEN
        !NO LONGER SUPPORTED
      ENDIF
C-------------------------
C NEW BOX OPTION (MULTI BOX COMBINATION)
C-------------------------
      IF (IT8 /= 0) THEN
        ALLOCATE(TAGT(NUMELT),STAT=stat)
        ALLOCATE(TAGP(NUMELP),STAT=stat)
        TAGT(1:NUMELT) = 0
        TAGP(1:NUMELP) = 0
        IF (FLAG == 0) THEN
          ALLOCATE(BUFBOX(1))
          BUFBOX = 0
        ELSEIF (FLAG == 1) THEN
          ALLOCATE(BUFBOX(IADBOXMAX))
          BUFBOX(1:IADBOXMAX) = 0
        ENDIF
        SBUFBOX = INT(INTMAX)
        CALL HM_OPTION_START('/LINE')
        TITR1='LINE'
        DO IGS=1,NSLIN
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)
            NN = 0
            NSEG=0
            KLINE=LINE
            IF(KEY(1:3) == 'BOX'.AND. NBBOX > 0)THEN
              NSEG=0
              IADBOX = 1
              IFLAGUNIT = 0
              DO J=1,NUNITS
                IF (UNITAB%UNIT_ID(J) == UID) THEN
                  FAC_L = UNITAB%FAC_L(J)
                  IFLAGUNIT = 1
                  EXIT
                ENDIF
              ENDDO
              IF (UID.NE.0.AND.IFLAGUNIT.EQ.0) THEN
                CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,I2=UID,I1=ID,C1='LINE',C2='LINE',C3=TITR)
              ENDIF

              IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
                NSEG0 = IGRSLIN(IGS)%NSEG
                MY_ALLOCATE2D(IGRSLIN(IGS)%NODES,NSEG0,2)
                IGRSLIN(IGS)%NODES(1:NSEG0,1:2) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%ELTYP,NSEG0)
                IGRSLIN(IGS)%ELTYP(1:NSEG0) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%ELEM,NSEG0)
                IGRSLIN(IGS)%ELEM(1:NSEG0) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%PROC,NSEG0)
                IGRSLIN(IGS)%PROC(1:NSEG0) = 0
              ENDIF

              IF(NUMELT > 0)
     .        CALL HM_BIGSBOX(NUMELT ,IXT      ,NIXT        ,2    ,3     ,4   ,
     .                        X      ,NSEG     ,FLAG        ,SKEW  ,
     .                        ISKN   ,0        ,ITABM1      ,IBOX  ,
     .                        ID     ,BUFBOX   ,IGRSLIN(IGS),IADBOX, KEY ,
     .                        SBUFBOX,TITR     ,MESS        ,TAGT,
     .                        NN     ,LSUBMODEL)
              IADBOXMAX = MAX(IADBOX,IADBOXMAX)
             IF (IADBOX>SBUFBOX .OR. IADBOX<0) 
     .        CALL ANCMSG(MSGID=1007,MSGTYPE=MSGERROR,ANMODE=ANSTOP)
              IF(NUMELP > 0)
     .        CALL HM_BIGSBOX(NUMELP ,IXP    ,NIXP        ,2    ,3     ,5   ,
     .                        X      ,NSEG   ,FLAG        ,SKEW,
     .                        ISKN   ,0      ,ITABM1      ,IBOX  ,
     .                        ID     ,BUFBOX ,IGRSLIN(IGS),IADBOX, KEY ,
     .                        SBUFBOX,TITR   ,MESS        ,TAGP ,
     .                        NN    ,LSUBMODEL)
             IF (IADBOX>SBUFBOX .OR. IADBOX<0) 
     .        CALL ANCMSG(MSGID=1007,MSGTYPE=MSGERROR,ANMODE=ANSTOP)
              IADBOXMAX = MAX(IADBOX,IADBOXMAX)
              IF (FLAG == 0) THEN
                IGRSLIN(IGS)%NSEG = NSEG
              ELSEIF (FLAG == 1) THEN
                IGRSLIN(IGS)%NSEG = NSEG
              ENDIF
            ENDIF
        ENDDO
        DEALLOCATE(TAGT,TAGP)
        IF(ALLOCATED(BUFBOX))DEALLOCATE(BUFBOX)
      ENDIF ! IF(IT8 /= 0)
C---
C=======================================================================
C ligne de SUBSETS,PART,MAT,PROP
C=======================================================================
      IF(IT2/=0.OR.IT6/=0) THEN
        ALLOCATE( SURF_ELM(NPART) )        
        MODE = 2
        CALL INIT_SURF_ELM(IBID    ,IBID   ,IBID    ,IBID   ,IBID    ,
     1                     NUMELT  ,NUMELP ,NUMELR  ,NPART  ,IBID    ,
     2                     IBID   ,IBID    ,IPARTT  ,IPARTP ,IPARTR  ,
     3                     SURF_ELM,MODE )
      ENDIF

      IF(IT2 /= 0) THEN
        CALL HM_OPTION_START('/LINE')
        TITR1='LINE'
        DO IGS=1,NSLIN
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)
            NSEG=0
            KLINE=LINE
            IF (KEY(1:4) == 'PART'.OR.KEY(1:6) == 'SUBSET'.OR. KEY(1:3) == 'MAT' .OR.KEY(1:4) == 'PROP') THEN
              IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
                NSEG0 = IGRSLIN(IGS)%NSEG
                MY_ALLOCATE2D(IGRSLIN(IGS)%NODES,NSEG0,2)
                IGRSLIN(IGS)%NODES(1:NSEG0,1:2) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%ELTYP,NSEG0)
                IGRSLIN(IGS)%ELTYP(1:NSEG0) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%ELEM,NSEG0)
                IGRSLIN(IGS)%ELEM(1:NSEG0) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%PROC,NSEG0)
                IGRSLIN(IGS)%PROC(1:NSEG0) = 0
              ENDIF
              CALL HM_TAGPART2(BUFTMP,IPART ,KEY   ,
     .                      IGRSLIN(IGS)%ID,TITR,TITR1,INDX,NINDX ,
     .                      FLAG,SUBSET,LSUBMODEL,MAP_TABLES%IPARTM)
              CALL SURFTAG(NUMELT,IXT,NIXT,2,3,4,IPARTT,
     .                     BUFTMP,IGRSLIN(IGS),NSEG,FLAG,NINDX,
     .                     INDX,SURF_ELM)
              CALL SURFTAG(NUMELP,IXP,NIXP,2,3,5,IPARTP,
     .                     BUFTMP,IGRSLIN(IGS),NSEG,FLAG,NINDX,
     .                     INDX,SURF_ELM)
              IF (KEY(1:3) /= 'MAT')
     .           CALL SURFTAG(NUMELR,IXR,NIXR,2,3,6,IPARTR,
     .                        BUFTMP,IGRSLIN(IGS),NSEG,FLAG,NINDX,
     .                        INDX,SURF_ELM)
              IF (KEY(1:4) == 'PART')
     .           CALL SURFTAGX(NUMELX,IXX,KXX,NIXX,8,IPARTX,
     .                        BUFTMP,IGRSLIN(IGS),NSEG,FLAG)
              IF (FLAG == 0) IGRSLIN(IGS)%NSEG = NSEG
            ENDIF
            ! reset BUFTMP to 0 (only where it was set to 1/-1)
            DO II=1,NINDX
              BUFTMP(INDX(II))=0
            END DO
            NINDX=0
        ENDDO
      ENDIF

C=======================================================================
C ligne de SUBMODELS
C=======================================================================
      IGS=0
      IF (IT6 > 0)THEN
        CALL HM_OPTION_START('/LINE')
        TITR1='LINE'
        DO IGS=1,NSLIN
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)
            NSEG=0
            IF (KEY(1:6)=='SUBMOD') THEN
              IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
                NSEG0 = IGRSLIN(IGS)%NSEG
                MY_ALLOCATE2D(IGRSLIN(IGS)%NODES,NSEG0,2)
                IGRSLIN(IGS)%NODES(1:NSEG0,1:2) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%ELTYP,NSEG0)
                IGRSLIN(IGS)%ELTYP(1:NSEG0) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%ELEM,NSEG0)
                IGRSLIN(IGS)%ELEM(1:NSEG0) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%PROC,NSEG0)
                IGRSLIN(IGS)%PROC(1:NSEG0) = 0
              ENDIF
!
              CALL HM_SUBMODPART(ISUBMOD,BUFTMP,IPART,ID  ,FLAG ,
     .                        MESS   ,TITR  ,TITR1,INDX,NINDX,
     .                        LSUBMODEL)
              CALL SURFTAG(NUMELT,IXT,NIXT,2,3,4,IPARTT,
     .                     BUFTMP,IGRSLIN(IGS),NSEG,FLAG,NINDX,
     .                     INDX,SURF_ELM)
              CALL SURFTAG(NUMELP,IXP,NIXP,2,3,5,IPARTP,
     .                     BUFTMP,IGRSLIN(IGS),NSEG,FLAG,NINDX,
     .                     INDX,SURF_ELM)
              CALL SURFTAG(NUMELR,IXR,NIXR,2,3,6,IPARTR,
     .                     BUFTMP,IGRSLIN(IGS),NSEG,FLAG,NINDX,
     .                     INDX,SURF_ELM)
              IF (FLAG == 0) IGRSLIN(IGS)%NSEG = NSEG
            ENDIF
C reset BUFTMP to 0 (only where it was set to 1/-1)
            DO II=1,NINDX
              BUFTMP(INDX(II))=0
            END DO
            NINDX=0
        ENDDO
      ENDIF
C=======================================================================
C ligne de bord de surface
C=======================================================================
      IF (IT5 /= 0) THEN
        CALL HM_OPTION_START('/LINE')
        TITR1='LINE'
        DO IGS=1,NSLIN
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)
            IF (KEY(1:4) == 'SURF' .OR. KEY(1:4) == 'EDGE') THEN
              IAD0 =1
              NSEG =0
              NSEG0=0
              CALL HM_GET_INTV  ('idsmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)                 
              DO KK=1,NENTITY                                                              
                CALL HM_GET_INT_ARRAY_INDEX ('ids' ,JJ ,KK,IS_AVAILABLE,LSUBMODEL)         
                IF (JJ /= 0) THEN                                                          
                  IGRS=0                                                                   
                  DO K=1,NSURF                                                             
                    IF (JJ == IGRSURF(K)%ID) THEN                                          
                      IGRS=K                                                               
                      EXIT                                                                 
                    ENDIF                                                                  
                  ENDDO                                                                    
                  IF (IGRS /= 0) THEN                                                      
                    NSEG0=NSEG0+IGRSURF(IGRS)%NSEG                                         
                    DO K=0,IGRSURF(IGRS)%NSEG-1                                            
                      BUFTMP(IAD0+6*K)   = IGRSURF(IGRS)%NODES(K+1,1)                      
                      BUFTMP(IAD0+6*K+1) = IGRSURF(IGRS)%NODES(K+1,2)                      
                      BUFTMP(IAD0+6*K+2) = IGRSURF(IGRS)%NODES(K+1,3)                      
                      BUFTMP(IAD0+6*K+3) = IGRSURF(IGRS)%NODES(K+1,4)                      
                      BUFTMP(IAD0+6*K+4) = IGRSURF(IGRS)%ELTYP(K+1)                        
                      BUFTMP(IAD0+6*K+5) = IGRSURF(IGRS)%ELEM(K+1)                         
                      DO JJ=1,6                                                            
                        NINDX=NINDX+1                                                      
                        INDX(NINDX)=IAD0+6*K+JJ-1                                          
                      ENDDO                                                                
                    ENDDO                                                                  
                    IAD0=IAD0+6*IGRSURF(IGRS)%NSEG                                         
                  ENDIF                                                                    
                ENDIF                                                                      
              ENDDO
              IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
                MY_ALLOCATE2D(IGRSLIN(IGS)%NODES, IGRSLIN(IGS)%NSEG,2)
                IGRSLIN(IGS)%NODES(1:IGRSLIN(IGS)%NSEG,1:2) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%ELTYP,IGRSLIN(IGS)%NSEG)
                IGRSLIN(IGS)%ELTYP(1:IGRSLIN(IGS)%NSEG) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%ELEM,IGRSLIN(IGS)%NSEG)
                IGRSLIN(IGS)%ELEM(1:IGRSLIN(IGS)%NSEG) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%PROC,IGRSLIN(IGS)%NSEG)
                IGRSLIN(IGS)%PROC(1:IGRSLIN(IGS)%NSEG) = 0
              ENDIF
              IF (NSEG0 > 0) THEN
                LINE_NSEG0 = 1
                IF (FLAG == 1) LINE_NSEG0 = IGRSLIN(IGS)%NSEG
                CALL LINEDGE(NSEG0 ,NSEG ,BUFTMP,IGRSLIN(IGS)%NODES ,KEY,
     .                       FLAG  ,IGRSLIN(IGS)%ELTYP,IGRSLIN(IGS)%ELEM,
     .                       LINE_NSEG0)
              ENDIF
              IF (FLAG == 0) IGRSLIN(IGS)%NSEG = NSEG
            ENDIF
            !reset BUFTMP to 0 (only where it was set to 1/-1)
            DO II=1,NINDX
              BUFTMP(INDX(II))=0
            END DO
            NINDX=0
        ENDDO
      ENDIF
C=======================================================================
C LIGNE FORMEE DE GROUPE DE TRUSS POUTRE RESSORT
C=======================================================================
      IF (IT4 /= 0) THEN
        CALL HM_OPTION_START('/LINE')
        TITR1='LINE'
        DO IGS=1,NSLIN
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)
            NSEG=0
            NSEG_TOT=0
            IF (KEY(1:2) == 'GR'.OR.KEY(1:4) == 'WIRE') THEN
              IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
                NSEG0 = IGRSLIN(IGS)%NSEG
                MY_ALLOCATE2D(IGRSLIN(IGS)%NODES,NSEG0,2)
                IGRSLIN(IGS)%NODES(1:NSEG0,1:2) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%ELTYP,IGRSLIN(IGS)%NSEG)
                IGRSLIN(IGS)%ELTYP(1:IGRSLIN(IGS)%NSEG) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%ELEM,IGRSLIN(IGS)%NSEG)
                IGRSLIN(IGS)%ELEM(1:IGRSLIN(IGS)%NSEG) = 0
                MY_ALLOCATE(IGRSLIN(IGS)%PROC,NSEG0)
                IGRSLIN(IGS)%PROC(1:NSEG0) = 0
              ENDIF
              IF(KEY(1:6) == 'GRSPRI') THEN
                NUMEL=NUMELR
                CALL HM_SURFGR2(NGRSPRI,KEY(1:6),NUMEL,IGRSLIN(IGS)%ID,
     .                      IGRSPRING,BUFTMP,TITR,TITR1,
     .                      INDX,NINDX,FLAG,IBID,IBID,
     .                      IBID,IBID,FLAG_GRBRIC,LSUBMODEL)
                CALL SURFTAGE(NUMELR,IXR,NIXR,2,3,6,
     .                        BUFTMP,IGRSLIN(IGS),NSEG,FLAG,
     .                        INDX,NINDX,NSEG_TOT)
              ELSEIF(KEY(1:6) == 'GRTRUS') THEN
                NUMEL=NUMELT
                CALL HM_SURFGR2(NGRTRUS,KEY(1:6),NUMEL,IGRSLIN(IGS)%ID,
     .                      IGRTRUSS,BUFTMP,TITR,TITR1,
     .                      INDX,NINDX,FLAG,IBID,IBID,
     .                      IBID,IBID,FLAG_GRBRIC,LSUBMODEL)
              CALL SURFTAGE(NUMELT,IXT,NIXT,2,3,4,
     .                      BUFTMP,IGRSLIN(IGS),NSEG,FLAG,
     .                      INDX,NINDX,NSEG_TOT)
              ELSEIF(KEY(1:6) == 'GRBEAM') THEN
                NUMEL=NUMELP
                CALL HM_SURFGR2(NGRBEAM,KEY(1:6),NUMEL,IGRSLIN(IGS)%ID,
     .                      IGRBEAM,BUFTMP,TITR,TITR1,
     .                      INDX,NINDX,FLAG,IBID,IBID,
     .                      IBID,IBID,FLAG_GRBRIC,LSUBMODEL)
              CALL SURFTAGE(NUMELP,IXP,NIXP,2,3,5,
     .                      BUFTMP,IGRSLIN(IGS),NSEG,FLAG,
     .                      INDX,NINDX,NSEG_TOT)
              ENDIF
              IF (FLAG == 0) IGRSLIN(IGS)%NSEG = NSEG
            ENDIF
            ! reset BUFTMP to 0 (only where it was set to 1/-1)
            DO II=1,NINDX
              BUFTMP(INDX(II))=0
            END DO
            NINDX=0            
C-----------
        ENDDO
      ENDIF
C=======================================================================
      DEALLOCATE(BUFTMP,INDX)
      IF(IT2/=0.OR.IT6/=0) THEN
!       deallocation of SURT_ELM structur
        MODE = 2
        CALL DEALLOCATE_SURF_ELM(NPART,SURF_ELM,MODE)
        DEALLOCATE( SURF_ELM ) 
      ENDIF
      RETURN
 900  CONTINUE
c  Il n y a pas de label 900 ...
      CALL ANCMSG(MSGID=189,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=IGRSLIN(IGS)%ID)
      RETURN
      END
