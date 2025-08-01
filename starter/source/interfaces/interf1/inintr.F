Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    inintr                       ../starter/source/interfaces/interf1/inintr.F
!||--- called by ------------------------------------------------------
!||    lectur                       ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    flush_remnode_array          ../starter/source/interfaces/inter3d1/flush_remnode_array.F
!||    fretitl2                     ../starter/source/starter/freform.F
!||    inint2                       ../starter/source/interfaces/inter2d1/inint2.F
!||    inint3                       ../starter/source/interfaces/inter3d1/inint3.F
!||    iwcontdd_new                 ../starter/source/spmd/domain_decomposition/grid2mat.F
!||    remn_i2_edgop                ../starter/source/interfaces/inter3d1/i7remnode.F
!||    remn_i2op                    ../starter/source/interfaces/inter3d1/i7remnode.F
!||    remn_self24                  ../starter/source/interfaces/inter3d1/remn_self24.F
!||    reset_gap                    ../starter/source/interfaces/interf1/reset_gap.F
!||    ri2_int24p_ini               ../starter/source/interfaces/inter3d1/i7remnode.F
!||    upgrade_remnode              ../starter/source/interfaces/interf1/upgrade_remnode.F
!||    upgrade_remnode_edg          ../starter/source/interfaces/interf1/upgrade_remnode.F
!||--- uses       -----------------------------------------------------
!||    front_mod                    ../starter/share/modules1/front_mod.F
!||    intbufmod                    ../starter/share/modules1/restart_mod.F
!||    intbufscratch_mod            ../starter/source/interfaces/interf1/intbufscratch_mod.F
!||    inter_cand_mod               ../starter/share/modules1/inter_cand_mod.F
!||    message_mod                  ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE ININTR(IPARI     ,INSCR     ,X                 ,V               ,IXS         ,IXQ   ,
     2                  IXC       ,PM        ,GEO               ,ITAB            ,MS          ,
     3                  MWA       ,RWA       ,IXTG              ,IKINE           ,IXT         ,
     4                  IXP       ,IXR       ,ALE_CONNECTIVITY  ,NELEMINT        ,IDDLEVEL    ,
     5                  IFIEND    ,IGRBRIC   ,IWCONT            ,IWCIN2          ,KNOD2ELS    ,
     6                  KNOD2ELC  ,KNOD2ELTG ,NOD2ELS           ,NOD2ELC         ,NOD2ELTG    ,
     8                  IGRSURF   ,IELEM21   ,SH4TREE           ,SH3TREE         ,IPART       ,
     9                  IPARTC    ,IPARTTG   ,THK               ,THK_PART        ,NOD2EL1D    ,
     A                  KNOD2EL1D ,IXS10     ,INTER_CAND        ,FRIGAP          ,IXS16       ,
     B                  IXS20     ,IPM       ,NOM_OPT           ,IPARTS          ,SISKWN      ,
     C                  KXX       ,IXX       ,IGEO              ,INTERCEP        ,LELX        ,
     D                  INTBUF_TAB,FILLSOL   ,PM_STACK          ,IWORKSH         ,NSNT        ,
     E                  NMNT      ,KXIG3D    ,IXIG3D            ,KNOD2ELQ        ,NOD2ELQ     ,
     F                  SEGQUADFR,TAGPRT_FRIC,INTBUF_FRIC_TAB   ,IPARTT          ,
     G                  IPARTP    ,IPARTX    ,IPARTR            ,NSN_MULTI_CONNEC,T2_NB_CONNEC,
     H                  SICODE    ,ICODE     ,ISKEW             ,MULTI_FVM       ,S_NOD2ELS   ,
     I                  SITAB     ,SITABM1   ,FLAG_ELEM_INTER25 ,LIST_NIN25      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE INTBUFMOD
      USE FRONT_MOD
      USE INTBUFDEF_MOD
      USE INTBUFSCRATCH_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
      USE INOUTFILE_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MULTI_FVM_MOD
      USE INTER_CAND_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE INT18_LAW151_NSV_SHIFT_MOD , ONLY : INT18_LAW151_NSV_SHIFT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr12_c.inc"
#include      "scr15_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: SITAB, SICODE, SITABM1, SISKWN !< array size
      INTEGER :: S_NOD2ELS !< size of NOD2ELS
      INTEGER IPARI(NPARI,NINTER), IXS(*), IXQ(*),
     .   IXC(*), ITAB(SITAB), MWA(*), IXTG(*), IKINE(*),
     .   IWCONT(5,*),IWCIN2(2,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), 
     .   NOD2ELS(S_NOD2ELS), NOD2ELC(*), NOD2ELTG(*),
     .   IXT(*), IXP(*), IXR(*), NELEMINT,  IDDLEVEL,IFIEND,
     .   IELEM21(*),IPM(NPROPMI,NUMMAT),
     .   SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*), IPART(*),
     .   IPARTC(*), IPARTTG(*),NOD2EL1D(*),KNOD2EL1D(*), IXS10(*),I_MEM,
     .   RESORT  , IXS16(8,*), IXS20(12,*),IPARTS(*),IGEO(*),
     .   IWORKSH(*),NSNT, NMNT,KXIG3D(NIXIG3D,*),IXIG3D(*),
     .   KNOD2ELQ(*),NOD2ELQ(*),SEGQUADFR(2,*),TAGPRT_FRIC(*),IPARTT(*),
     .   IPARTP(*),IPARTX(*),IPARTR(*),NSN_MULTI_CONNEC,T2_NB_CONNEC(*),
     .   ICODE(*), ISKEW(SISKWN)
      my_real X(3,*),V(3,*), PM(*), GEO(*), MS(*), RWA(6,*),
     .        THK(*),THK_PART(*),FRIGAP(NPARIR,NINTER),
     .        LELX(*), FILLSOL(*),PM_STACK(*)
      INTEGER NOM_OPT(LNOPT1,*),KXX(*),IXX(*)
      INTEGER, INTENT(IN) :: LIST_NIN25(NINTER)
      INTEGER, INTENT(IN) :: FLAG_ELEM_INTER25(NINTER25,NUMELS)   
      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(SCRATCH_STRUCT_) INSCR(*)
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC)  :: IGRBRIC
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(INTER_CAND_), INTENT(inout) :: INTER_CAND
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER,DIMENSION(:), ALLOCATABLE :: ITABM1
      INTEGER N, JINSCR, NIN,IWRN, I
      INTEGER NTY, NSN2T, NMN2T,ID,
     .        NSNET  ,NMNET  ,MULTIMP, IREMNODE, NREMNODE,
     .        NREMN(NINTER),ST2_CONNEC,
     .        REMNODE_SIZE,LEN_FILNAM,REMNODE_SIZE_EDG,IREMNODE_EDG,NIN25
      CHARACTER*(2148) FILNAM
      CHARACTER(LEN=NCHARTITLE) :: TITR
      INTEGER, DIMENSION(:),ALLOCATABLE :: T2_ADD_CONNEC,T2_CONNEC,IKINE1
      
      INTEGER :: NS
      INTEGER :: NSN,NMN
      LOGICAL :: CONDITION(NINTER)
      my_real :: v1(3),v2(3)
      INTEGER :: f1,f2
      my_real :: displacement,displacement_max
      INTEGER :: NRTM
      INTEGER :: MAIN_INTERFACE_SIZE  
      INTEGER :: ID_MAIN_INTERFACE
      INTEGER :: CPT,NODE_ID,J
      INTEGER :: IPARI_14,INACTI
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG
      DOUBLE PRECISION :: avg_stiff(NINTER)
      DOUBLE PRECISION :: main_stiff
      DOUBLE PRECISION :: min_stiff
      LOGICAL :: IS_INTER18_AND_LAW151
      INTEGER :: FLAG_OUTPUT !< flag for output
      LOGICAL :: FLAG_REMOVED_NODE !< flag to remove some S node from the list of candidates
      INTEGER :: IJK
      INTEGER :: KIND_INTERFACE
      INTEGER, DIMENSION(3) :: NEXT_INTER ! number of interface (3 different kinds of interface : type2,type24 and other)
      INTEGER, DIMENSION(3) :: ADDRESS_INTER ! address in the INTERFACE_INDEX array
      INTEGER, DIMENSION(NINTER) :: INTERFACE_TYPE2,INTERFACE_TYPE24,INTERFACE_OTHER ! interfaces, savec according to theirs types
      INTEGER, DIMENSION(NINTER) :: INTERFACE_INDEX ! index of interface : other : 1-->next_inter(1), type2 : next_inter(1)+1--> next_inter(2), type24 : next_inter(1)+(2)--> next_inter(1)+(2)+(3)
      INTEGER :: INTER_TYPE2_NUMBER !<number of interface type 2
      INTEGER(KIND=8) :: NREMNODE_KIND8
      INTEGER :: IEDGE !< check if edge to edge is used by the interface
      INTEGER :: SKIP_TYPE25_EDGE_2_EDGE !< flag to activate only the computation of interface type 25 with edge to edge
      INTEGER, DIMENSION(:), ALLOCATABLE :: ELEM_LINKED_TO_SEGMENT
C-----------------------------------------------
      CALL MY_ALLOC(ITABM1,SITABM1)
C
      I_MEM = 0
      RESORT = 0
      NREMNODE = 0
      ALLOCATE(IKINE1(3*NUMNOD))
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
C-----------------------------------------------
      IF(((IDDLEVEL == 0)) .AND. (DECTYP >= 3 .AND. DECTYP <= 6) .AND. N2D == 0)THEN
        NSNT = 0
        NMNT = 0
        NSN2T = 0
        NMN2T = 0
        NSNET = 0
        NMNET = 0
        DO I = 1, NUMNOD
          IWCONT(1,I) = 0
          IWCONT(2,I) = 0
          IWCONT(3,I) = 0
          IWCONT(4,I) = 0
          IWCIN2(1,I) = 0
          IWCIN2(2,I) = 0
        END DO
      END IF

C----------------------Treatment for TYPE2 spt27/28 compatibility  check - computation of size and allocation of array
       IF (NSN_MULTI_CONNEC > 0) THEN
         ALLOCATE (T2_ADD_CONNEC(NUMNOD))
         T2_ADD_CONNEC(1:NUMNOD) = 0
         ST2_CONNEC = 0
         T2_ADD_CONNEC(1) = 1
         IF (T2_NB_CONNEC(1)>1) ST2_CONNEC = 1 + 5*T2_NB_CONNEC(1)
         DO I=2,NUMNOD
C--        only potential multiple connections are counted - nodes with only one connections are not counted -> nb of connections set to 0
           IF (T2_NB_CONNEC(I) == 1) T2_NB_CONNEC(I) = 0
C--
           ST2_CONNEC = ST2_CONNEC + 1 + 5*T2_NB_CONNEC(I)
           T2_ADD_CONNEC(I) = T2_ADD_CONNEC(I-1) + 1 + 5*T2_NB_CONNEC(I-1)
         ENDDO
         ALLOCATE (T2_CONNEC(ST2_CONNEC))
         T2_CONNEC(1:ST2_CONNEC) = 0
       ELSE
         ST2_CONNEC = 0
         ALLOCATE (T2_ADD_CONNEC(0),T2_CONNEC(0))
       ENDIF

C-----------------------------------------------     
        ALLOCATE( ELEM_LINKED_TO_SEGMENT(NUMELS) )
        ! ----------------
        ! loop over the interfaces
        ! interface type 24 & 25 must be treated at the end
        ! interface type 2 must be treated before interface 24
        ! other interfaces are treated at the beginning
        SKIP_TYPE25_EDGE_2_EDGE = 0
        NEXT_INTER(1:3) = 0
        DO N=1,NINTER
          NTY=IPARI(7,N)
          IF(NTY==2) THEN
            NEXT_INTER(2) = NEXT_INTER(2) + 1
            INTERFACE_TYPE2(NEXT_INTER(2)) = N
          ELSEIF(NTY == 24 .OR. NTY == 25) THEN
            NEXT_INTER(3) = NEXT_INTER(3) + 1
            INTERFACE_TYPE24(NEXT_INTER(3)) = N
            IF(NTY == 25) THEN
              ! special case : interface type 25 with edge to edge 
              ! LEDGE array is initialized during the sorting                    
              ! i7remnode algo for interface type 25 with edge to edge must be done after the %LEDGE initialization
              IEDGE = IPARI(58,N)
              IF(IEDGE/=0) SKIP_TYPE25_EDGE_2_EDGE = 1
            ENDIF
          ELSE
            NEXT_INTER(1) = NEXT_INTER(1) + 1
            INTERFACE_OTHER(NEXT_INTER(1)) = N
          ENDIF
        ENDDO

        ADDRESS_INTER(1) = 0
        ADDRESS_INTER(2) = NEXT_INTER(1)
        ADDRESS_INTER(3) = NEXT_INTER(1) + NEXT_INTER(2)
        INTERFACE_INDEX(1:NEXT_INTER(1)) = INTERFACE_OTHER(1:NEXT_INTER(1))
        INTERFACE_INDEX(ADDRESS_INTER(2)+1:ADDRESS_INTER(2)+NEXT_INTER(2)) = INTERFACE_TYPE2(1:NEXT_INTER(2))
        INTERFACE_INDEX(ADDRESS_INTER(3)+1:ADDRESS_INTER(3)+NEXT_INTER(3)) = INTERFACE_TYPE24(1:NEXT_INTER(3))
        ! ----------------
        IWRN = 0
        INTER_TYPE2_NUMBER = 0
        NREMN(1:NINTER) = 0     
        ! ----------------
        ! loop over the interfaces
        ! 1) interface type 1,3:6,8:23
        !    forbidden S nodes (defined in remnode structure) will BE take into account in the domain decomposition
        ! 2) interface type 2
        !    at the end of the interface type 2 treatment, update :
        !    a) remnode algo for interface type 2 
        !    b) remnode algo for interface type 24
        ! 3) interface type 24 / 25
        !    forbidden S nodes (defined in remnode structure) will not be take into account in the domain decomposition
        DO KIND_INTERFACE=1,3
          
          ! ----------------
          DO IJK=1,NEXT_INTER(KIND_INTERFACE)
            N = INTERFACE_INDEX(ADDRESS_INTER(KIND_INTERFACE)+IJK)
            IREMNODE = 0
            IREMNODE_EDG = 0
            NREMNODE = 0
            NTY=IPARI(7,N)
            IEDGE = IPARI(58,N)
            FLAG_REMOVED_NODE = .FALSE.
            IF ((NTY == 7 .OR. NTY == 25) .AND. IPARI(63,N) == 2 .AND. IDDLEVEL == 1)THEN
C---     Initial dimension of REMNODE arrays
              IREMNODE = 1
              NREMNODE_KIND8 = 16*IPARI(4,N)
              IF(NREMNODE_KIND8 > HUGE(NREMNODE)) THEN
                NREMNODE = HUGE(NREMNODE)/2  ! decrease initial value of NREMNODE to fit into integer storage
              ELSE
                NREMNODE = NREMNODE_KIND8
              END IF
              CALL UPGRADE_REMNODE(IPARI(1,N),NREMNODE,INTBUF_TAB(N),NTY)
              FLAG_REMOVED_NODE = .TRUE.
            ENDIF
            IF (NTY == 11.AND. IPARI(63,N) == 2 .AND. IDDLEVEL == 1)THEN
C---      Initial dimension of REMNODE arrays
              IREMNODE = 1
              REMNODE_SIZE = 5*IPARI(4,N)
              CALL UPGRADE_REMNODE(IPARI(1,N),REMNODE_SIZE,INTBUF_TAB(N),NTY)
            ENDIF
            IF (NTY == 25.AND.IPARI(58,N) >0 .AND. IPARI(63,N) == 2 .AND. IDDLEVEL == 1)THEN
C---     Initial dimension of REMNODE arrays
              IREMNODE_EDG = 1
              REMNODE_SIZE_EDG = 5*IPARI(68,N)
              CALL UPGRADE_REMNODE_EDG(IPARI(1,N),REMNODE_SIZE_EDG,INTBUF_TAB(N))
            ENDIF
            IF((NTY==24.OR.NTY==25).AND.IDDLEVEL==0.AND.IPARI(63,N)>0) THEN
              IF(INTBUF_TAB(N)%S_KREMNODE>0) FLAG_REMOVED_NODE = .TRUE.
              IF(NTY==25.AND.IEDGE/=0) FLAG_REMOVED_NODE = .TRUE.
              IF(NTY==25.AND.NEXT_INTER(2)==0) FLAG_REMOVED_NODE = .FALSE.
            ENDIF

            IF (NTY == 2)  INTER_TYPE2_NUMBER=INTER_TYPE2_NUMBER+1
            RESORT = 0
            IF (NTY == 14.OR.NTY == 15.OR.NTY == 16.OR.NTY == 18.OR.NTY==0) CYCLE
 200        CONTINUE
    
            IF (I_MEM == 2)THEN
              IF(NTY == 11) THEN
                MULTIMP = MAX(IPARI(23,N)+8,NINT(IPARI(23,N)*1.75))
                MULTIMP = MAX(MULTIMP,IPARI(23,N)+2500000/MAX(1,IPARI(18,N)))
                MULTIMP = MAX(MULTIMP,INTBUF_TAB(N)%S_CAND_MAX / MAX(1,IPARI(18,N))) 
                INTBUF_TAB(N)%S_CAND_MAX = MAX(MULTIMP*IPARI(18,N),INTBUF_TAB(N)%S_CAND_MAX)
              ELSE
                MULTIMP = MAX(IPARI(23,N)+8,NINT(IPARI(23,N)*1.5))
              ENDIF

              CALL RESET_GAP(N,IPARI,INTBUF_TAB(N),FRIGAP)
              CALL UPGRADE_MULTIMP(N,MULTIMP,INTBUF_TAB(N))
              I_MEM = 0
              RESORT = 1
            ENDIF

            JINSCR=IPARI(10,N)
            NIN=N
            ID=NOM_OPT(1,NIN)
            CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,NIN),LTITR)
            NIN25 = LIST_NIN25(NIN)

            IF(N2D == 0)THEN
              IF( MULTI_FVM%IS_INT18_LAW151 ) THEN
                CALL INT18_LAW151_NSV_SHIFT('+',0,1,MULTI_FVM,IPARI,INTBUF_TAB,npari,ninter,numnod,OPT_INT_ID=N)
              ENDIF
              CALL ININT3(
     .               INSCR(N)%WA             , X                     , IXS                   , IXC            , PM               ,
     1               GEO                     , IPARI                 , NIN                   , ITAB           , MS               ,
     2               MWA                     , RWA                   , IXTG                  , IWRN           , IKINE            ,
     3               IXT                     , IXP                   , IXR                   , NELEMINT       , IDDLEVEL         ,
     4               IFIEND                  , ALE_CONNECTIVITY      , NSNET                 , NMNET          , IGRBRIC          ,
     5               IWCONT                  , NSNT                  , NMNT                  , NSN2T          , NMN2T            ,
     6               IWCIN2                  , KNOD2ELS              , KNOD2ELC              , KNOD2ELTG      , NOD2ELS          ,
     7               NOD2ELC                 , NOD2ELTG              , IGRSURF               , IKINE1         , IELEM21          ,
     8               SH4TREE                 , SH3TREE               , IPART                 , IPARTC         , IPARTTG          ,
     9               THK                     , THK_PART              , NOD2EL1D              , KNOD2EL1D      , IXS10            ,
     A               I_MEM                   , RESORT                , INTER_CAND            , IXS16          , IXS20            ,
     B               ID                      , TITR                  , IREMNODE              , NREMNODE       , IPARTS           ,
     C               KXX                     , IXX                   , IGEO                  , INTERCEP       , LELX             ,
     D               INTBUF_TAB              , FILLSOL               , PM_STACK              , IWORKSH        , KXIG3D           ,
     E               IXIG3D                  , TAGPRT_FRIC           , INTBUF_FRIC_TAB       , IPARTT         , IPARTP           ,
     F               IPARTX                  , IPARTR                , NSN_MULTI_CONNEC      , T2_ADD_CONNEC  , T2_NB_CONNEC     ,
     G               T2_CONNEC               , NOM_OPT               , ICODE                 , ISKEW          , IREMNODE_EDG     ,
     H               MULTI_FVM%S_APPEND_ARRAY, MULTI_FVM%X_APPEND    , MULTI_FVM%MASS_APPEND , N2D            , FLAG_REMOVED_NODE,
     I               NSPMD                   ,INTER_TYPE2_NUMBER     , ELEM_LINKED_TO_SEGMENT, INSCR(N)%SINSCR, SICODE           ,
     J               SITAB                   ,NIN25                  , FLAG_ELEM_INTER25     , MULTI_FVM )
              IF( MULTI_FVM%IS_INT18_LAW151 ) THEN
                CALL INT18_LAW151_NSV_SHIFT('-',0,1,MULTI_FVM,IPARI,INTBUF_TAB,npari,ninter,numnod,OPT_INT_ID=N)
              ENDIF

              IF (I_MEM /= 0) GOTO 200
            ELSE
              CALL ININT2(
     1    INTBUF_TAB(N),INSCR(N)%WA  ,X         ,IXQ       ,INSCR(N)%SINSCR,
     2    PM           ,GEO          ,IPARI(1,N),NIN       ,ITAB           ,
     3    ITABM1       ,NUMNOD       ,IKINE     ,MWA       ,IPM            ,
     4    ID           ,TITR         ,KNOD2ELQ  ,NOD2ELQ   ,SEGQUADFR      ,
     5    NUMMAT       ,NINTER       ,SITAB     ,SITABM1   ,SICODE         ,
     6    ICODE)
            ENDIF
          ENDDO
          ! ----------------

          ! ----------------
          ! update the forbidden nodes for interface type 2 and 24
          ! thank to remnode algo (remnode for removed node ???)
          IF (N2D==0.AND.KIND_INTERFACE==2.AND.NSPMD>1) THEN
            IF(IDDLEVEL==0) THEN   
              FLAG_OUTPUT = 0
              IF (INTER_TYPE2_NUMBER >0) THEN
                CALL REMN_I2OP(1,NINTER,IPARI,INTBUF_TAB,ITAB,NOM_OPT,NREMN,FLAG_OUTPUT,SKIP_TYPE25_EDGE_2_EDGE)
              ENDIF
              CALL REMN_SELF24(X   ,IXS   ,IXS10 ,IXS16,IXS20   ,
     .                       KNOD2ELS,NOD2ELS,IPARI ,INTBUF_TAB ,
     .                       ITAB , NOM_OPT,NREMN, S_NOD2ELS,FLAG_OUTPUT)
              CALL REMN_I2_EDGOP(IPARI,INTBUF_TAB,ITAB,NREMN)
              CALL RI2_INT24P_INI(IPARI,INTBUF_TAB,ITAB,NOM_OPT,NREMN )
            ELSE
              CALL FLUSH_REMNODE_ARRAY(NINTER,NPARI,IPARI,INTBUF_TAB)
            ENDIF
          END IF
          ! ----------------

        ENDDO
        ! ----------------
        DEALLOCATE(ELEM_LINKED_TO_SEGMENT)
C      
C--- IREM_I2 treatment has been removed at end of ININTR2 to take into account 
C---- the compaction of type2 w/ Itetra10=2  
C
      IF(IWRN/=0) THEN
        LEN_FILNAM = OUTFILE_NAME_LEN + ROOTLEN + 6
        FILNAM = OUTFILE_NAME(1:OUTFILE_NAME_LEN)//ROOTNAM(1:ROOTLEN)//'.coord'
        OPEN(UNIT=IOU2,FILE=FILNAM(1:LEN_FILNAM),STATUS='UNKNOWN',
     .       FORM='FORMATTED')
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        WRITE(IOU2,'(A)')'# NEW NODES COORDINATES'
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        WRITE(IOU2,'(I10,1P3G20.13)')
     .               (ITAB(I),X(1,I),X(2,I),X(3,I),I=1,NUMNOD)
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        WRITE(IOU2,'(A)')'# END OF NEW NODES COORDINATES'
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        CLOSE(UNIT=IOU2)
      ENDIF

C =============================================================
C DETECT INTERFACES WITH HIGH CPU COST 
C - auto-impacting interface on solid 
C - with low stiffness
C - included in the *main interface*
C
C *main interface* : interface that concern a lot of nodes with
C significantly different velocities between secnd and main
      displacement_max = 0.0
      NS = 0
      MAIN_INTERFACE_SIZE = -1 
      ID_MAIN_INTERFACE = -1
      IF(N2D==0)THEN
      IF((IDDLEVEL==0).AND.(DECTYP>=3.AND.DECTYP<=6))THEN

        CONDITION(1:NINTER) = .FALSE.
        avg_stiff(1:NINTER) = HUGE(1.0D0)
        ALLOCATE(TAG(NUMNOD))
        TAG(1:NUMNOD) = 0
        DO N=1,NINTER
          NTY=IPARI(7,N)
          IF( NTY == 7 ) THEN
            NMN      = IPARI(6,N)
            NSN      = IPARI(5,N)
            NRTM     = IPARI(4,N)
            INACTI   = IPARI(22,N)
            IPARI_14 = IPARI(14,N)
            IS_INTER18_AND_LAW151 = .FALSE.
            IF(INACTI == 7)THEN
              IF(IPARI_14 == 151)IS_INTER18_AND_LAW151 = .TRUE.
            ENDIF
            IF(IS_INTER18_AND_LAW151)CYCLE
            NS = 0
C           CALL COUNT_SOLID_NODES(NOD2EL1D,KNOD2EL1D,INTBUF_TAB(N),NMN,NSN,NS)
C ----------- Count solid nodes
            DO I = 1,NMN
              NODE_ID =  INTBUF_TAB(N)%MSR(I)
              DO J = KNOD2ELS(NODE_ID)+1,KNOD2ELS(NODE_ID+1) 
                 NS = NS +1 
                 EXIT
              ENDDO
            ENDDO
C
            IF (NS > 9*(NMN) / 10) THEN
            ! Interface concerns mainly solids
              CPT = 0
              DO I = 1,NSN
                TAG(INTBUF_TAB(N)%NSV(I)) = 1
              ENDDO
              DO I = 1,NMN
                IF(TAG(INTBUF_TAB(N)%MSR(I)) == 1) CPT = CPT + 1
              ENDDO
              DO I = 1,NSN
                TAG(INTBUF_TAB(N)%NSV(I)) = 0
              ENDDO
              IF( abs(NSN - NMN) < NSN / 50 .AND. abs(NMN - CPT) < NMN/50) THEN
! Heuristic to find auto-impacting interface 
                 CONDITION(N) = .TRUE.
                 avg_stiff(N) = 0.0D0
                 DO I = 1,NRTM 
                   avg_stiff(N) = avg_stiff(N) + INTBUF_TAB(N)%STFM(I)/dble(NRTM)
                 ENDDO
                 IF(avg_stiff(N) == 0) THEN
                   DO I = 1,NSN 
                     avg_stiff(N) = avg_stiff(N) + INTBUF_TAB(N)%STFNS(I)/dble(NSN)
                   ENDDO
                 ENDIF
              ENDIF
            ENDIF
            INACTI = IPARI(22,N)
            IPARI_14 = IPARI(14,N)
            IS_INTER18_AND_LAW151 = .FALSE.
            IF(INACTI == 7 .AND. IPARI_14 == 151) IS_INTER18_AND_LAW151 = .TRUE.
            IF(.NOT. IS_INTER18_AND_LAW151)THEN
             CALL C_COMPUTE_VELOCITY(V, NUMNOD, INTBUF_TAB(N)%NSV, NSN, v1, f1)
             CALL C_COMPUTE_VELOCITY(V, NUMNOD, INTBUF_TAB(N)%MSR, NMN, v2, f2)
             displacement = (v1(1) - v2(1))**2 + (v1(2) - v2(2))**2 + (v1(3) - v2(3))**2
             IF(f1 > NSN / 2 .AND. f2 > NMN / 2) THEN
               IF(displacement > displacement_max / 10.0 .AND. NMN + NSN > MAIN_INTERFACE_SIZE) THEN
                 IF( NMN + NSN > NUMNOD / 100 ) THEN
                   ! main interface = interface that has the maximum displacement velocity
                   ! between main and secnd, and that contains at least 1% of the nodes
                   MAIN_INTERFACE_SIZE = NMN + NSN
                   ID_MAIN_INTERFACE = N
C                  CONDITION(N) = .FALSE.
                   displacement_max = displacement
                 ENDIF ! NMN+ NSN
               ENDIF ! Displacement
             ENDIF ! f1 & f2
            ENDIF
          ENDIF ! NTY
        ENDDO ! N

        TAG(1:NUMNOD) = 0
        IF(ID_MAIN_INTERFACE > 0) THEN
          NSN      = IPARI(5,ID_MAIN_INTERFACE)
          NRTM     = IPARI(4,ID_MAIN_INTERFACE)
          NMN      = IPARI(6,ID_MAIN_INTERFACE)
          main_stiff = 0.0D0
          DO I = 1,NRTM
            main_stiff = main_stiff + INTBUF_TAB(ID_MAIN_INTERFACE)%STFM(I) / dble(NRTM)
          ENDDO
          IF(main_stiff == 0) THEN
            DO I = 1,NSN 
              main_stiff = main_stiff + INTBUF_TAB(ID_MAIN_INTERFACE)%STFNS(I) / dble(NSN)
            ENDDO
          ENDIF
c         WRITE(,*) "main stiff=",main_stiff
          DO I = 1,NSN
            TAG(INTBUF_TAB(ID_MAIN_INTERFACE)%NSV(I)) = 1
          ENDDO
          DO I = 1,NMN
            TAG(INTBUF_TAB(ID_MAIN_INTERFACE)%MSR(I)) = 1
          ENDDO
          min_stiff = HUGE(0.0D0)
          DO N=1,NINTER
            IPARI(69,N) = 0
            IF(CONDITION(N) .AND. N /= ID_MAIN_INTERFACE) THEN
!            auto-impacting interface mainly made of solids
              CPT = 0
              NMN      = IPARI(6,N)
              NSN      = IPARI(5,N)
              DO I = 1,NSN
                IF(TAG(INTBUF_TAB(N)%NSV(I)) == 1) CPT = CPT +1
              ENDDO
              IF( CPT > (NSN)/3 ) THEN  
              ! the nodes of this interface are included in the main
              ! interface
                 min_stiff = MIN(min_stiff,avg_stiff(N)) 
                 IPARI(69,N) = 1
C Appel   routine poids noeuds interfaces
              ENDIF !CPT
            ENDIF ! CONDITION
          ENDDO ! NINTER
          DO N=1,NINTER
            IF(IPARI(69,N) ==  1) THEN
              NMN      = IPARI(6,N)
              NSN      = IPARI(5,N)
              IF(avg_stiff(N) < main_stiff / 10.0) THEN
                I =  0
                IF(avg_stiff(N) <= 3.0*min_stiff .AND. avg_stiff(N) < main_stiff / 200.0) I = 1
                IF(avg_stiff(N) <= 2.0*min_stiff .AND. avg_stiff(N) < main_stiff / 500.0) I = 4
c               WRITE(6,*) "Interface",IPARI(15,N),"weight=",I 
c               WRITE(6,*) "Stiff=",min_stiff,avg_stiff(N),main_stiff
                IF(I > 0) THEN 
                  WRITE(IOUT,*)"INFO: WEIGHT OF INTERFACE",IPARI(15,N), "INCREASED"
                  CALL IWCONTDD_NEW(INTBUF_TAB(N)%NSV,INTBUF_TAB(N)%MSR,NSN,NMN,IWCONT,I)
                ENDIF
              ENDIF
            ENDIF ! CONDITION
          ENDDO ! NINTER

        ENDIF ! main interface

        DEALLOCATE(TAG)
      ENDIF
      ENDIF ! N2D

C
      DEALLOCATE(T2_ADD_CONNEC,T2_CONNEC,IKINE1)
      DEALLOCATE(ITABM1)
C
      RETURN
      END
