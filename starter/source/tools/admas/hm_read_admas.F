Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    hm_read_admas              ../starter/source/tools/admas/hm_read_admas.F
!||--- called by ------------------------------------------------------
!||    lectur                     ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                     ../starter/source/output/message/message.F
!||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv              ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key         ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start            ../starter/source/devtools/hm_reader/hm_option_start.F
!||    surfmas                    ../starter/source/tools/admas/surfmas.F
!||    usr2sys                    ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod         ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    r2r_mod                    ../starter/share/modules1/r2r_mod.F
!||    submodel_mod               ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_ADMAS(
     .                   MS       ,ITABM1   ,IGRNOD   ,UNITAB  ,IGRSURF   ,
     .                   IPART    ,IPMAS    ,TOTADDMAS,FLAG    ,IGRPART   ,
     .                   X        ,LSUBMODEL,IDDLEVEL ,IN      ,TOTADDINER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE OPTIONDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE PRECISION_MOD, ONLY: WP
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER    ,INTENT(IN) :: ITABM1(*),IPART(LIPART1,*),FLAG,IDDLEVEL
      my_real    ,INTENT(IN) :: X(3,*)
      my_real    ,INTENT(INOUT) :: MS(*),TOTADDMAS,TOTADDINER
      my_real    ,INTENT(INOUT) :: IN(NUMNOD)                !< Nodal inertia array to be updated with added inertia
      TYPE(SUBMODEL_DATA),DIMENSION(*)         ,INTENT(IN)    :: LSUBMODEL
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB 
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  ,INTENT(IN)   :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRPART) ,INTENT(IN)   :: IGRPART
      TYPE (SURF_)   , DIMENSION(NSURF)   ,INTENT(IN)   :: IGRSURF
      TYPE (ADMAS_)  , DIMENSION(NODMAS)  ,INTENT(INOUT):: IPMAS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ITYPE,ID,UID,IGR,IGRS,NOSYS,ISU,NNOD,
     .        ISS,NN,IBUFN(4),CAPT,ITY,IPA,IP,IGRPA,IDP,
     .        NEL,IFLAG,JCURR,CPT_LAST,IMS,ENTITYMAX
      my_real
     .   AMAS,COEFF_R2R
      LOGICAL LOOP_2
!
      CHARACTER(nchartitle)  :: TITR,MESS
      LOGICAL :: IS_AVAILABLE
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ENTITY_MULTI,IFLAG_MULTI
      my_real, ALLOCATABLE, DIMENSION(:) :: AMAS_MULTI
      REAL(KIND=WP) :: Ixx, Ixy, Ixz, Iyy, Iyz, Izz    !< local inertia components for type 0
      REAL(KIND=WP), DIMENSION(:), ALLOCATABLE ::      !< local inertia arrays for type 5
     .                                Ixx_multi, Ixy_multi, Ixz_multi,
     .                                Iyy_multi, Iyz_multi, Izz_multi
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'ADDED MASS DEFINITION                   '/
C=======================================================================
!     IPMAS(IGM)%ID    : ADMAS IDENTIFIER
!     IPMAS(IGM)%NPART : NUMBER of parts to get ADMAS
!     IPMAS(IGM)%TYPE  : ! ADMAS type
!                    = 0 ! Mass is added to each node of node group
!                    = 1 ! Mass/N is added to each node of node group.
!                          N being the total number of nodes in the node group
!                    = 2 ! Mass/Area - additional surface mass applied on the shell area
!                    = 3 ! Additional mass distributed on the part-group
!                    = 4 ! Final mass distributed on the part-group
!                    = 5 ! Mass is added to each single node
!                    = 6 ! Additional mass distributed on each single part
!                    = 7 ! Final mass distributed on each single part
!                    = 10 ! Inertia is added to each node of node group
!                    = 15 ! Inertia is added to each single node
!     IPMAS(IGM)%TITLE  : ADMAS title
!     IPMAS(IGM)%WEIGHT_FLAG : Flag to switch between area weighted distribution and volume 
!                                   weighted distribution of the added mass to parts)
!                    = 0 ! Volume weighted distribution (shell and solid parts) (default)
!                    = 1 ! Area weighted distribution (shell parts only)
!     IPMAS(IGM)%PARTID(J) : PART_ID to get ADMAS
!     IPMAS(IGM)%PART(J)%RPMAS : ADMAS added to PARTS (or NODES)
!
!     IPMAS(IGM)%NODEID(INOD) : NODE_ID to add INERTIA
!     IPMAS(IGM)%NODE(INOD)%INERTIA : INERTIA added to NODES
!     IPMAS(IGM)%NODE(INOD)%MASS : MASS added to NODES
!     IPMAS(IGM)%NODE(INOD)%IXX : IXX component of inertia tensor added to NODES
!     IPMAS(IGM)%NODE(INOD)%IYY : IYY component of inertia tensor added to NODES
!     IPMAS(IGM)%NODE(INOD)%IZZ : IZZ component of inertia tensor added to NODES
!     IPMAS(IGM)%NODE(INOD)%IXY : IXY component of inertia tensor added to NODES
!     IPMAS(IGM)%NODE(INOD)%IXZ : IXZ component of inertia tensor added to NODES
!     IPMAS(IGM)%NODE(INOD)%IYZ : IYZ component of inertia tensor added to NODES
C=======================================================================
!
      IS_AVAILABLE = .FALSE.
!
C--------------------------------------------------
C START BROWSING MODEL /ADMAS
C--------------------------------------------------
      CALL HM_OPTION_START('/ADMAS')
C--------------------------------------------------
C BROWSING MODEL ADMAS 1-> NODMAS
C--------------------------------------------------
      IMASADD = 0
c for optimisation 1 (last group is memorised)
      JCURR = 1
!
      DO I=1,NODMAS
        TITR = ''
        IMS = 0
C--------------------------------------------------
C EXTRACT DATAS OF /ADMAS/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          UNIT_ID     = UID,
     .                          OPTION_TITR = TITR)
!
        CALL HM_GET_INTV('type'   ,ITYPE  ,IS_AVAILABLE,LSUBMODEL)

!---
        IPMAS(I)%TITLE = TITR
        IPMAS(I)%ID = ID
        IPMAS(I)%TYPE = ITYPE
!------
        IF (ITYPE == 0 .or. ITYPE == 1) THEN
!------
!---
! added mass to nodes of grnod
!---
          IF (FLAG == 0) THEN
            CALL HM_GET_FLOATV('masses' ,AMAS ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
            CALL HM_GET_INTV('grnd_ID'    ,IGR  ,IS_AVAILABLE ,LSUBMODEL)
!
            IF(AMAS < ZERO)THEN
              CALL ANCMSG(MSGID=476,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR)
            ENDIF
!!          AMAS = AMAS * FAC_M
            IF(IGR == 0)THEN
              CALL ANCMSG(MSGID=668,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1='/ADMAS',
     .                    C2='/ADMAS',
     .                    C3=TITR,
     .                    I1=ID)
            ENDIF
!
            IGRS=0

c original
c    DO J=1,NGRNOD
c      IF(IGR == IGROU(1,J))THEN
c        IGRS=J
c        GOTO 100
c      ENDIF
c    ENDDO
c end original
ccccccccccccccccccccccccc
c OPTIMISATION1
ccccccccccccccccccccccccc
c optimisation to avoid quadratic loop
c if group is found, next search start from this group
c optimal in case of sorted list of GRNOD in ADMAS file
c in case on non sorted file, a dichotomic search is more appropriate

            CPT_LAST = NGRNOD
            LOOP_2 = .FALSE.
110      CONTINUE
              DO J=JCURR,CPT_LAST
                IF (IGR == IGRNOD(J)%ID) THEN
                  IGRS = J
                  JCURR = J
c          group found
                  GOTO 100
                ENDIF
                IF (J == NGRNOD) THEN 
                IF(LOOP_2)THEN
c second passage IGRS has not been found we output in error 
                  GOTO 100
                ELSE
c first passage in loop, we will start a 2nd passage from 1 to jcurr
                  LOOP_2 = .TRUE.
                ENDIF
                CPT_LAST = JCURR
                  JCURR = 1
c begin again loop 1
                  GOTO 110
                ENDIF
              ENDDO ! DO J=JCURR,CPT_LAST
ccccccccccccccccccccccccc
c end OPTIMISATION1
ccccccccccccccccccccccccc
100         CONTINUE
C---
            IF (ITYPE == 1) THEN 
                COEFF_R2R = 1
              NNOD = IGRNOD(IGRS)%NENTITY
C-----------Multidomains: the total mass is corrected with the new number of nodes--------------
              IF (NSUBDOM > 0) THEN
                   IF (IPID==0) NNOD = NNOD-IGRNOD(IGRS)%R2R_SHARE
                   COEFF_R2R=(1.00*NNOD)/(1.00*MAX(1,IGRNOD(IGRS)%R2R_ALL))
              ENDIF
              AMAS = COEFF_R2R*AMAS/MAX(1,NNOD)
            ENDIF !  ! IF (ITYPE == 1)
C
            IF (IGRS /= 0) THEN
              DO J=1,IGRNOD(IGRS)%NENTITY
                NOSYS=IGRNOD(IGRS)%ENTITY(J)
C-----------Multidomains: common nodes are only processed on 1 domain--------------
                IF ((NSUBDOM > 0).AND.(IPID == 0)) THEN
                  IF (TAGNO(NPART+NOSYS) > 1) GOTO 150
                ENDIF
                MS(NOSYS) = MS(NOSYS) + AMAS
                TOTADDMAS = TOTADDMAS + AMAS
 150      CONTINUE
              ENDDO
              NNOD = IGRNOD(IGRS)%NENTITY
            ELSE
              CALL ANCMSG(MSGID=53,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1='IN /ADMAS OPTION',
     .                    I1=IGR)
            ENDIF ! IF (IGRS /= 0)
          ENDIF ! IF (FLAG == 0)
!------
        ELSEIF (ITYPE == 2) THEN
!------
!---
!  added mass per unit area for surfaces
!---
          IF (FLAG == 0) THEN
            ISU = 0
            CALL HM_GET_FLOATV('masses' ,AMAS ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
            CALL HM_GET_INTV('surf_ID'    ,ISU  ,IS_AVAILABLE ,LSUBMODEL)
!
            IF (AMAS < ZERO) THEN
              CALL ANCMSG(MSGID=875,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    R1=AMAS)
            ENDIF
!!            AMAS = AMAS * FAC_M
            IF (ISU == 0) THEN
              CALL ANCMSG(MSGID=872,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR)
            ENDIF
            ISS=0
            NN =0
            DO J=1,NSURF
              IF (ISU == IGRSURF(J)%ID) THEN
                ISS=J
                  NN = IGRSURF(ISS)%NSEG
                EXIT
              ENDIF
            ENDDO
C-----------Multidomains -> we count the common segments, we only count them once---
            IF (NSUBDOM > 0) THEN
                IF (IDDOM > 0) NN = NN-ISURF_R2R(1,ISS)
            ENDIF
C-----------
            IF (ISS /= 0) THEN
              DO J=1,NN
                  IF (IDDOM > 0) THEN
C-----------Multidomains -> we eliminate the common segments, we only process them once---
                    CAPT=0
                  DO K=1,4
                      CAPT=CAPT+TAGNO(NPART+IGRSURF(ISS)%NODES(J,K))
                  ENDDO
                    IF (CAPT == 8) GOTO 160
                  ENDIF
C 
                ITY=IGRSURF(ISS)%ELTYP(J)
C
                IBUFN(1)=IGRSURF(ISS)%NODES(J,1)
                IBUFN(2)=IGRSURF(ISS)%NODES(J,2)
                IBUFN(3)=IGRSURF(ISS)%NODES(J,3)
                IF (IGRSURF(ISS)%NODES(J,3) == 
     .              IGRSURF(ISS)%NODES(J,4)) ITY = 7
                IF (ITY == 7) THEN
C          true triangles (not segments built from 3 nodes) or degenerated
                 IBUFN(4)=0
                ELSE
                 IBUFN(4)=IGRSURF(ISS)%NODES(J,4)
                ENDIF
C
                CALL SURFMAS(MS,IBUFN,ITY,AMAS,X,IGRSURF(ISS)%ID,TOTADDMAS,ID,TITR)
C
 160            CONTINUE
              ENDDO ! DO J=1,NN
            ELSE
              CALL ANCMSG(MSGID=873,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=ISU)
            ENDIF ! IF(ISS /= 0)
          ENDIF ! IF (FLAG == 0)
!------
        ELSEIF (ITYPE == 3 .or. ITYPE == 4) THEN
!------
! added mass to a group of parts
!---
          CALL HM_GET_FLOATV('masses' ,AMAS   ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
          CALL HM_GET_INTV('grpart_ID'    ,IGRPA  ,IS_AVAILABLE ,LSUBMODEL)
          CALL HM_GET_INTV('iflags'   ,IFLAG  ,IS_AVAILABLE ,LSUBMODEL)
!
          IF (AMAS < ZERO .and. FLAG == 0) THEN
            CALL ANCMSG(MSGID=875,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  R1=AMAS)
          ENDIF
!!          AMAS = AMAS * FAC_M
          IF (IGRPA == 0 .and. FLAG == 0) THEN 
              CALL ANCMSG(MSGID=878,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR)
          ENDIF
          IF (IFLAG /= 0 .and. IFLAG /= 1) IFLAG = 0
          IPMAS(I)%WEIGHT_FLAG = IFLAG
          IGRS = 0
C
          DO J=1,NGRPART
            IF (IGRPA == IGRPART(J)%ID) THEN
              IGRS=J
              EXIT
            ENDIF
          ENDDO
C---
          IF (FLAG == 0) THEN
            IF (IGRS /= 0) THEN
              NEL = IGRPART(IGRS)%NENTITY
              IPMAS(I)%NPART = NEL
!    allocate only one time because of "IDDLEVEL"
              if (.not.allocated(IPMAS(I)%PART))  ALLOCATE(IPMAS(I)%PART(NEL))
              if (.not.allocated(IPMAS(I)%PARTID))ALLOCATE(IPMAS(I)%PARTID(NEL))
            ELSE
              CALL ANCMSG(MSGID=879,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IGRPA)
            ENDIF ! IF (IGRS /= 0)
          ELSEIF(FLAG == 1)THEN
            IF (IGRS /= 0) THEN
              IMASADD = IMASADD + 1
C
              NEL = IGRPART(IGRS)%NENTITY
C-----------Multidomains: we cannot split the mass in this case--------------
              IF ((NSUBDOM > 0) .AND.(NEL /= IGRPART(IGRS)%R2R_ALL).AND.(NEL > 0)) THEN
                CALL ANCMSG(MSGID=893,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID)
              ENDIF        
              DO J=1,NEL
                IDP=IGRPART(IGRS)%ENTITY(J)
                IPMAS(I)%PARTID(J) = IDP
                IPMAS(I)%PART(J)%RPMAS = AMAS
              ENDDO
            ENDIF ! IF (IGRS /= 0)
          ENDIF ! IF (FLAG == 0)
!------
        ELSEIF (ITYPE == 5) THEN
!------
! added mass to nodes
!---
          IF (FLAG == 0) THEN
            CALL HM_GET_INTV('entityidsmax'   ,ENTITYMAX ,IS_AVAILABLE ,LSUBMODEL)
!
            ALLOCATE(AMAS_MULTI(ENTITYMAX))
            AMAS_MULTI(1:ENTITYMAX) = ZERO
            ALLOCATE(ENTITY_MULTI(ENTITYMAX))
            ENTITY_MULTI(1:ENTITYMAX) = 0
            DO J=1,ENTITYMAX
              CALL HM_GET_FLOAT_ARRAY_INDEX('masses'  ,AMAS_MULTI(J)   ,J ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              CALL HM_GET_INT_ARRAY_INDEX('node_ID' ,ENTITY_MULTI(J) ,J ,IS_AVAILABLE, LSUBMODEL)
!
              IF (AMAS_MULTI(J) < ZERO) THEN
                CALL ANCMSG(MSGID=875,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,
     .                      C1=TITR,
     .                      R1=AMAS_MULTI(J))
              ENDIF
!!              AMAS = AMAS * FAC_M
              IF (ENTITY_MULTI(J) <=  0)THEN
                CALL ANCMSG(MSGID=871,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ENTITY_MULTI(J))
              ENDIF
              NOSYS = USR2SYS(ENTITY_MULTI(J),ITABM1,MESS,ID)
C-----------multidomains: common nodes are only processed in one domain--------------
              IF ((NSUBDOM > 0) .AND. (IPID == 0)) THEN
                   IF (TAGNO(NPART+NOSYS) > 1) GOTO 170
                ENDIF
              MS(NOSYS) = MS(NOSYS) + AMAS_MULTI(J)
              TOTADDMAS = TOTADDMAS + AMAS_MULTI(J)
 170          CONTINUE
            ENDDO ! DO J=1,ENTITYMAX
            IF (ALLOCATED(AMAS_MULTI)) DEALLOCATE(AMAS_MULTI)
            IF (ALLOCATED(ENTITY_MULTI)) DEALLOCATE(ENTITY_MULTI)
          ENDIF ! IF (FLAG == 0)
!------
        ELSEIF (ITYPE == 6 .or. ITYPE == 7) THEN
!------
! added mass by part
!---
          CALL HM_GET_INTV('entityidsmax'   ,ENTITYMAX ,IS_AVAILABLE ,LSUBMODEL)
!
          ALLOCATE(AMAS_MULTI(ENTITYMAX))
          AMAS_MULTI(1:ENTITYMAX) = ZERO
          ALLOCATE(ENTITY_MULTI(ENTITYMAX))
          ENTITY_MULTI(1:ENTITYMAX) = 0
          ALLOCATE(IFLAG_MULTI(ENTITYMAX))
          IFLAG_MULTI(1:ENTITYMAX) = 0
!
          IF (FLAG == 0) THEN
            IPMAS(I)%NPART = ENTITYMAX
!    allocate only one time because of "IDDLEVEL"
            if (.not.allocated(IPMAS(I)%PART))  ALLOCATE(IPMAS(I)%PART(ENTITYMAX))
            if (.not.allocated(IPMAS(I)%PARTID))ALLOCATE(IPMAS(I)%PARTID(ENTITYMAX))
          ENDIF ! IF (FLAG == 0)
!
          IPA = 0
          DO J=1,ENTITYMAX
            CALL HM_GET_FLOAT_ARRAY_INDEX('masses'  ,AMAS_MULTI(J)   ,J ,IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('part_ID' ,ENTITY_MULTI(J) ,J ,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('iflags'    ,IFLAG_MULTI(J)  ,J ,IS_AVAILABLE, LSUBMODEL)
!
            IF (AMAS_MULTI(J) < ZERO .and. FLAG == 0) THEN
              CALL ANCMSG(MSGID=875,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    R1=AMAS_MULTI(J))
            ENDIF
!!          AMAS = AMAS * FAC_M
            IF (ENTITY_MULTI(J) == 0 .and. FLAG == 0) THEN
              CALL ANCMSG(MSGID=874,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR)
            ENDIF
            IF (IFLAG_MULTI(J) /= 0 .and. IFLAG_MULTI(J) /= 1) IFLAG_MULTI(J) = 0
            IPMAS(I)%WEIGHT_FLAG = IFLAG_MULTI(J)
!
            IP = 0
            IF (FLAG == 1) THEN
              DO K=1,NPART
                IF (ENTITY_MULTI(J) == IPART(4,K)) THEN
                  IP = K
                  EXIT
                ENDIF
              ENDDO
!
C-----------multidomains: parts that do not belong to the domain are ignored--------------
              IF (NSUBDOM > 0) THEN
                  IF (TAG_PART(IP) == 0) THEN
                  IPMAS(I)%NPART = IPMAS(I)%NPART -1
            GOTO 180
          ENDIF
                ENDIF

              IF (IP > 0) THEN
                IMASADD = IMASADD + 1
                IMS  = IMS + 1
                IPMAS(I)%PARTID(IMS) = IP
                IPMAS(I)%PART(IMS)%RPMAS = AMAS_MULTI(J)
              ELSE
                CALL ANCMSG(MSGID=876,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ENTITY_MULTI(J))
              ENDIF
180           CONTINUE
              ENDIF ! IF (FLAG == 1)

          ENDDO ! DO J=1,ENTITYMAX
          IF (ALLOCATED(AMAS_MULTI))   DEALLOCATE(AMAS_MULTI)
          IF (ALLOCATED(ENTITY_MULTI)) DEALLOCATE(ENTITY_MULTI)
          IF (ALLOCATED(IFLAG_MULTI))  DEALLOCATE(IFLAG_MULTI)
!------
        ELSE IF (ITYPE == 10) THEN
          !------
          ! added mass to nodes of grnod
          !------
          IF (FLAG == 0) THEN
            CALL HM_GET_FLOATV('IXX' ,Ixx ,IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('IXY' ,Ixy ,IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('IXZ' ,Ixz ,IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('IYY' ,Iyy ,IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('IYZ' ,Iyz ,IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('IZZ' ,Izz ,IS_AVAILABLE, LSUBMODEL, UNITAB)

            CALL HM_GET_FLOATV('masses'  ,AMAS ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
            CALL HM_GET_INTV('grnd_ID' ,IGR  ,IS_AVAILABLE ,LSUBMODEL)
!
            ! For deformable bodies, the only one value of Inertia tensor is needed, IXX. 
            ! For rigid bodies (not yet implemented), the 6 components of inertia tensor are added to each node of the group.
!
            IF (AMAS < 0.0_WP) THEN
              CALL ANCMSG(MSGID=3151,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    R1=AMAS)
            ENDIF
!
            IF (Ixx < 0.0_WP) THEN
              CALL ANCMSG(MSGID=3152,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    R1=Ixx)
            ENDIF
!
            IF (IGR == 0) THEN
              CALL ANCMSG(MSGID=668,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1='/ADMAS',
     .                    C2='/ADMAS',
     .                    C3=TITR,
     .                    I1=ID)
            ENDIF
!
            IGRS=0

!             original
!             do j=1,ngrnod
!               if (igr == igrou(1,j)) then
!                 IGRS=j
!                 goto 200
!               endif
!             enddo
!             end original
!-----------------------
!             OPTIMISATION1
!-----------------------
!             optimisation to avoid quadratic loop
!             if group is found, next search start from this group
!             optimal in case of sorted list of GRNOD in ADMAS file
!             in case on non sorted file, a dichotomic search is more appropriate

            CPT_LAST = NGRNOD
            LOOP_2 = .FALSE.
210       CONTINUE
            DO J = JCURR,CPT_LAST
              IF (IGR == IGRNOD(J)%ID) THEN
                IGRS = J
                JCURR = J
!           group found
                GOTO 200
              ENDIF
              IF (J == NGRNOD) THEN 
                IF(LOOP_2)THEN
!           second passage IGRS has not been found we output in error 
                  GOTO 200
                ELSE
!           first passage in loop, we will start a 2nd passage from 1 to jcurr
                  LOOP_2 = .TRUE.
                ENDIF
                CPT_LAST = JCURR
                JCURR = 1
!           begin again loop 1
                GOTO 210
              ENDIF
            ENDDO ! DO J = JCURR,CPT_LAST
!-----------------------
!           end OPTIMISATION1
!-----------------------
200       CONTINUE
!
            IF (IGRS /= 0) THEN
              NNOD = IGRNOD(IGRS)%NENTITY
              ! For optimization purposes, allocation of "node" arrays 
              ! is only done at the moment of group processing, and only if not already allocated
              ! (in case the same group is used in several /ADMAS definitions)
              !
              ! for type 0, the number of nodes is given by the group definition, so allocation of "node" arrays is done
              ! here with size "1"  to be able to store "node" data for at least one node, and then reallocation 
              ! is done if needed in case of several /ADMAS definitions with the same group
              IF (.NOT.ALLOCATED(IPMAS(I)%NODEID) .AND. IDDLEVEL==0) ALLOCATE(IPMAS(I)%NODEID(NNOD))
              IF (.NOT.ALLOCATED(IPMAS(I)%NODE)  .AND. IDDLEVEL==0) ALLOCATE(IPMAS(I)%NODE(1))
              !
              IPMAS(I)%GRNODID = IGR
              IPMAS(I)%NBNOD = NNOD
              !
              IPMAS(I)%NODE(1)%MASS = AMAS
              IPMAS(I)%NODE(1)%IXX = Ixx
              IPMAS(I)%NODE(1)%IYY = Iyy
              IPMAS(I)%NODE(1)%IZZ = Izz
              IPMAS(I)%NODE(1)%IXY = Ixy
              IPMAS(I)%NODE(1)%IXZ = Ixz
              IPMAS(I)%NODE(1)%IYZ = Iyz
              !
              DO J=1,NNOD
                NOSYS=IGRNOD(IGRS)%ENTITY(J)
                ! inertia/mass storage
                IPMAS(I)%NODEID(J) = NOSYS
!-----------Multidomains: common nodes are only processed on 1 domain--------------
                IF ((NSUBDOM > 0).AND.(IPID == 0)) THEN
                  IF (TAGNO(NPART+NOSYS) > 1) GOTO 250
                ENDIF
                !
                IF (AMAS > ZERO) THEN
                  MS(NOSYS) = MS(NOSYS) + AMAS
                  TOTADDMAS = TOTADDMAS + AMAS
                ENDIF
                !! compute of inertia for each node of the group (only Ixx is kept for the moment)
                IF (Ixx > ZERO) THEN
                  IN(NOSYS) = IN(NOSYS) + Ixx
                  TOTADDINER = TOTADDINER + Ixx
                ENDIF
 250      CONTINUE
              ENDDO ! J=1,NNOD
            ELSE
              CALL ANCMSG (MSGID=53,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     C1='IN /ADMAS OPTION',
     .                     I1=IGR)
            ENDIF ! if (igr /= 0)
!------
          ENDIF ! IF (FLAG == 0)
        ELSE IF (ITYPE == 15) THEN
          !------
          ! added mass to nodes
          !------
          IF (FLAG == 0) THEN
            CALL HM_GET_INTV('entityidsmax'   ,ENTITYMAX ,IS_AVAILABLE ,LSUBMODEL)
!
            ALLOCATE(Ixx_multi(ENTITYMAX))
            Ixx_multi(1:ENTITYMAX) = 0.0_WP
            ALLOCATE(Ixy_multi(ENTITYMAX))
            Ixy_multi(1:ENTITYMAX) = 0.0_WP
            ALLOCATE(Ixz_multi(ENTITYMAX))
            Ixz_multi(1:ENTITYMAX) = 0.0_WP
            ALLOCATE(Iyy_multi(ENTITYMAX))
            Iyy_multi(1:ENTITYMAX) = 0.0_WP
            ALLOCATE(Iyz_multi(ENTITYMAX))
            Iyz_multi(1:ENTITYMAX) = 0.0_WP
            ALLOCATE(Izz_multi(ENTITYMAX))
            Izz_multi(1:ENTITYMAX) = 0.0_WP
            ALLOCATE(AMAS_MULTI(ENTITYMAX))
            AMAS_MULTI(1:ENTITYMAX) = 0.0_WP
            ALLOCATE(ENTITY_MULTI(ENTITYMAX))
            ENTITY_MULTI(1:ENTITYMAX) = 0
!
            IF (.NOT.ALLOCATED(IPMAS(I)%NODEID) .AND. IDDLEVEL==0)  ALLOCATE(IPMAS(I)%NODEID(ENTITYMAX))
            IF (.NOT.ALLOCATED(IPMAS(I)%NODE)   .AND. IDDLEVEL==0)  ALLOCATE(IPMAS(I)%NODE(ENTITYMAX))
!
            IPMAS(I)%NBNOD = ENTITYMAX
            DO J=1,ENTITYMAX
              CALL HM_GET_FLOAT_ARRAY_INDEX('IXX'  ,Ixx_multi(J)     ,J ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              CALL HM_GET_FLOAT_ARRAY_INDEX('IXY'  ,Ixy_multi(J)     ,J ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              CALL HM_GET_FLOAT_ARRAY_INDEX('IXZ'  ,Ixz_multi(J)     ,J ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              CALL HM_GET_FLOAT_ARRAY_INDEX('IYY'  ,Iyy_multi(J)     ,J ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              CALL HM_GET_FLOAT_ARRAY_INDEX('IYZ'  ,Iyz_multi(J)     ,J ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              CALL HM_GET_FLOAT_ARRAY_INDEX('IZZ'  ,Izz_multi(J)     ,J ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              CALL HM_GET_FLOAT_ARRAY_INDEX('masses'  ,AMAS_MULTI(J) ,J ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              CALL HM_GET_INT_ARRAY_INDEX('node_ID' ,ENTITY_MULTI(J) ,J ,IS_AVAILABLE, LSUBMODEL)
!
                IF (AMAS_MULTI(J) < 0.0_WP) THEN
                  CALL ANCMSG(MSGID=3151,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=ID,
     .                        C1=TITR,
     .                        R1=AMAS_MULTI(J))
                ENDIF
!
                IF (IXX_MULTI(J) < 0.0_WP) THEN
                  CALL ANCMSG(MSGID=3152,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=ID,
     .                        C1=TITR,
     .                        R1=IXX_MULTI(J))
                ENDIF
!
                IF (ENTITY_MULTI(J) <=  0) THEN
                  CALL ANCMSG(MSGID=871 ,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1=TITR,
     .                        I2=ENTITY_MULTI(J))
                ENDIF
                NOSYS = USR2SYS(ENTITY_MULTI(J),ITABM1,MESS,ID)
                ! inertia/mass storage
                IPMAS(I)%NODEID(J) = NOSYS
                IPMAS(I)%NODE(J)%MASS = AMAS_MULTI(J)
                IPMAS(I)%NODE(J)%IXX = IXX_MULTI(J)
                IPMAS(I)%NODE(J)%IYY = IYY_MULTI(J)
                IPMAS(I)%NODE(J)%IZZ = IZZ_MULTI(J)
                IPMAS(I)%NODE(J)%IXY = IXY_MULTI(J)
                IPMAS(I)%NODE(J)%IXZ = IXZ_MULTI(J)
                IPMAS(I)%NODE(J)%IYZ = IYZ_MULTI(J)
!-----------multidomains: common nodes are only processed in one domain--------------
                IF ((NSUBDOM > 0) .AND. (IPID == 0)) THEN
                  IF (TAGNO(NPART+NOSYS) > 1) GOTO 270
                ENDIF
                MS(NOSYS) = MS(NOSYS) + AMAS_MULTI(J)
                TOTADDMAS = TOTADDMAS + AMAS_MULTI(J)
                !! compute of inertia for each node of the group (only ixx is kept for the moment)
                IN(NOSYS) = IN(NOSYS) + IXX_MULTI(J)
                TOTADDINER = TOTADDINER + IXX_MULTI(J)
 270      CONTINUE
              ENDDO ! DO J=1,ENTITYMAX
!
              IF (ALLOCATED(ENTITY_MULTI)) DEALLOCATE(ENTITY_MULTI)
              IF (ALLOCATED(AMAS_MULTI)) DEALLOCATE(AMAS_MULTI)
              IF (ALLOCATED(IXX_MULTI)) DEALLOCATE(IXX_MULTI)
              IF (ALLOCATED(IXY_MULTI)) DEALLOCATE(IXY_MULTI)
              IF (ALLOCATED(IXZ_MULTI)) DEALLOCATE(IXZ_MULTI)
              IF (ALLOCATED(IYY_MULTI)) DEALLOCATE(IYY_MULTI)
              IF (ALLOCATED(IYZ_MULTI)) DEALLOCATE(IYZ_MULTI)
              IF (ALLOCATED(IZZ_MULTI)) DEALLOCATE(IZZ_MULTI)
          ENDIF ! IF (FLAG == 0)
        ENDIF ! IF (ITYPE == 0 .or. ITYPE == 1 ... )
!------
      ENDDO ! DO I=1,NODMAS
C---
      RETURN
      END
